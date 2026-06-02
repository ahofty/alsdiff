#!/usr/bin/env python3
"""
als_reorder.py — reordena blocos de música (cenas) na Session View de um Live Set.

Premissas (ver docs/plano-reordenar-cenas.md):
  - Um "bloco" de música começa numa cena cujo NOME tem um prefixo fixo (default ">>").
    Ex.: ">> VAGALUME". O bloco vai dessa cena até imediatamente antes da próxima cena
    com prefixo (ou o fim da lista). Tudo no meio (STOP, cenas em branco, clips) viaja
    junto com o bloco — inclusive o separador final.
  - Cenas ANTES do primeiro bloco nomeado são "cabeçalho" e ficam fixas no topo.

Correção (o que a ferramenta garante):
  - Reordena os <Scene> dentro de <Scenes> E, em lockstep, os <ClipSlot> de TODA
    <ClipSlotList> cujo nº de filhos diretos == nº de cenas (todas as tracks).
  - Remapeia os "jump to scene" das follow actions de CLIP (FollowActionA/B == 8):
    o índice absoluto de destino passa a apontar para a nova posição da cena.
  - NÃO re-serializa a árvore XML (que corromperia aspas/entidades). Faz cirurgia de
    texto: move os spans exatos e só reescreve os JumpIndex necessários.
  - Sempre grava um arquivo NOVO; nunca sobrescreve o original.

Uso:
  als_reorder.py inspect  ENTRADA.als
  als_reorder.py reorder  ENTRADA.als --order "B,A,E,D,C" [--output SAIDA.als] [--prefix ">>"]
  als_reorder.py selftest
"""

import argparse
import gzip
import re
import sys

# FollowActionA/B value que significa "Jump to scene" (destino = índice ABSOLUTO de cena).
# Confirmado empiricamente no set real (Live 12.3.1): dentre os clips com JumpIndex>1,
# o FollowAction é SEMPRE 9 (126 casos, todos enabled). Outros valores (4=play again,
# 8=next, etc.) não usam JumpIndex absoluto. Ver docs/plano-reordenar-cenas.md §Fase 0.
JUMP_ACTION = "9"
DEFAULT_PREFIX = ">>"


# --------------------------------------------------------------------------------------
# Scanner XML depth-aware (texto bruto, preserva tudo)
# --------------------------------------------------------------------------------------

_NAME = r"[A-Za-z0-9_]+"


def _open_re(tag):
    # casa <Tag ...> mas NÃO <TagOutro (exige delimitador após o nome)
    return re.compile(r"<" + re.escape(tag) + r"(?=[\s/>])")


def _tag_end(s, lt):
    """Dado o índice `lt` de um '<', retorna o índice do '>' que FECHA essa tag,
    pulando regiões entre aspas (atributos podem conter '>' literal — ex.: o prefixo
    '>>' do usuário em Value=">>A"). Lida com aspas simples e duplas."""
    i = lt + 1
    n = len(s)
    quote = None
    while i < n:
        c = s[i]
        if quote:
            if c == quote:
                quote = None
        elif c == '"' or c == "'":
            quote = c
        elif c == ">":
            return i
        i += 1
    raise ValueError("tag não fechada a partir de %d" % lt)


def child_elements(s, tag=None):
    """Itera elementos FILHOS DIRETOS dentro da string `s` (conteúdo entre as tags do
    pai). Yields (tag, start, end) com offsets em `s`; [start:end] é o elemento inteiro,
    incluindo tags de abertura/fechamento. Lida com self-closing e aninhamento do mesmo
    nome. Se `tag` for dado, filtra só aquele nome."""
    i, n = 0, len(s)
    while i < n:
        lt = s.find("<", i)
        if lt == -1:
            break
        m = re.match(r"<(" + _NAME + r")", s[lt:])
        if not m:
            i = lt + 1
            continue
        name = m.group(1)
        gt = _tag_end(s, lt)
        if s[gt - 1] == "/":  # self-closing
            span = (name, lt, gt + 1)
            i = gt + 1
            if tag is None or name == tag:
                yield span
            continue
        # elemento com filhos: achar fechamento correspondente (depth-aware)
        open_re = _open_re(name)
        close = "</%s>" % name
        depth = 1
        pos = gt + 1
        while depth > 0:
            mo = open_re.search(s, pos)
            nc = s.find(close, pos)
            if nc == -1:
                raise ValueError("XML desbalanceado para <%s>" % name)
            if mo and mo.start() < nc:
                g2 = _tag_end(s, mo.start())
                if s[g2 - 1] != "/":
                    depth += 1
                pos = g2 + 1
            else:
                depth -= 1
                pos = nc + len(close)
        span = (name, lt, pos)
        i = pos
        if tag is None or name == tag:
            yield span


def find_element(s, tag, start=0):
    """Acha o primeiro elemento <tag>...</tag> (ou self-closing) a partir de `start`.
    Retorna (open_start, inner_start, inner_end, end) ou None.
    inner_* delimita o conteúdo entre as tags (vazio para self-closing)."""
    m = _open_re(tag).search(s, start)
    if not m:
        return None
    lt = m.start()
    gt = _tag_end(s, lt)
    if s[gt - 1] == "/":
        return (lt, gt + 1, gt + 1, gt + 1)
    open_re = _open_re(tag)
    close = "</%s>" % tag
    depth = 1
    pos = gt + 1
    inner_start = gt + 1
    while depth > 0:
        mo = open_re.search(s, pos)
        nc = s.find(close, pos)
        if nc == -1:
            raise ValueError("XML desbalanceado para <%s>" % tag)
        if mo and mo.start() < nc:
            g2 = _tag_end(s, mo.start())
            if s[g2 - 1] != "/":
                depth += 1
            pos = g2 + 1
        else:
            depth -= 1
            if depth == 0:
                inner_end = nc
            pos = nc + len(close)
    return (lt, inner_start, inner_end, pos)


# --------------------------------------------------------------------------------------
# Modelo do Live Set (sobre texto)
# --------------------------------------------------------------------------------------


class LiveSet:
    def __init__(self, text):
        self.text = text
        # Localiza <LiveSet> ... <Scenes>
        ls = find_element(text, "LiveSet")
        if not ls:
            raise ValueError("LiveSet não encontrado")
        ls_open, ls_inner_start, ls_inner_end, ls_end = ls
        sc = find_element(text, "Scenes", ls_inner_start)
        if not sc:
            raise ValueError("Scenes não encontrado")
        self.scenes_open, self.scenes_inner_start, self.scenes_inner_end, self.scenes_end = sc
        # Spans absolutos das cenas (filhos diretos de <Scenes>)
        inner = text[self.scenes_inner_start:self.scenes_inner_end]
        self.scene_spans = []  # (abs_start, abs_end)
        for name, a, b in child_elements(inner, "Scene"):
            self.scene_spans.append((self.scenes_inner_start + a, self.scenes_inner_start + b))
        self.num_scenes = len(self.scene_spans)

    def scene_name(self, idx):
        a, b = self.scene_spans[idx]
        span = self.text[a:b]
        nm = find_element(span, "Name")
        if not nm:
            return ""
        tag_open = span[nm[0]:_tag_end(span, nm[0]) + 1]
        m = re.search(r'Value="([^"]*)"', tag_open)
        return _xml_unescape(m.group(1)) if m else ""

    def clip_slot_lists(self):
        """Retorna lista de (inner_start, inner_end, [child_spans]) para CADA ClipSlotList
        cujo nº de filhos diretos ClipSlot == num_scenes."""
        result = []
        pos = 0
        text = self.text
        while True:
            el = find_element(text, "ClipSlotList", pos)
            if not el:
                break
            _, ins, ine, end = el
            pos = end
            inner = text[ins:ine]
            spans = [(ins + a, ins + b) for (_, a, b) in child_elements(inner, "ClipSlot")]
            if len(spans) == self.num_scenes:
                result.append((ins, ine, spans))
        return result


def _xml_unescape(s):
    return (s.replace("&lt;", "<").replace("&gt;", ">")
             .replace("&quot;", '"').replace("&apos;", "'")
             .replace("&amp;", "&"))


# --------------------------------------------------------------------------------------
# Detecção de blocos
# --------------------------------------------------------------------------------------


def detect_blocks(ls, prefix):
    """Retorna (head_count, blocks) onde blocks = [(label, start_idx, end_idx_excl)].
    head_count = nº de cenas antes do 1º bloco (ficam fixas no topo)."""
    starts = []
    for i in range(ls.num_scenes):
        nm = ls.scene_name(i).strip()
        if nm.startswith(prefix):
            label = nm[len(prefix):].strip()
            starts.append((i, label))
    head_count = starts[0][0] if starts else ls.num_scenes
    blocks = []
    for k, (idx, label) in enumerate(starts):
        end = starts[k + 1][0] if k + 1 < len(starts) else ls.num_scenes
        blocks.append((label, idx, end))
    return head_count, blocks


# --------------------------------------------------------------------------------------
# inspect
# --------------------------------------------------------------------------------------


def scene_clip_count(ls, idx, csls):
    """Quantos slots dessa cena têm clip de fato (heurística p/ inspeção)."""
    cnt = 0
    for _, _, spans in csls:
        a, b = spans[idx]
        slot = ls.text[a:b]
        # clip presente se houver <MidiClip ou <AudioClip dentro
        if "<MidiClip" in slot or "<AudioClip" in slot:
            cnt += 1
    return cnt


def cmd_inspect(path, prefix):
    text = read_als(path)
    ls = LiveSet(text)
    csls = ls.clip_slot_lists()
    print("Arquivo:", path)
    print("Cenas:", ls.num_scenes)
    print("ClipSlotList com %d slots (reordenáveis em lockstep): %d" % (ls.num_scenes, len(csls)))
    head, blocks = detect_blocks(ls, prefix)
    print("Prefixo de bloco: %r" % prefix)
    print("Cenas de cabeçalho (fixas, antes do 1º bloco): %d" % head)
    print("Blocos detectados: %d" % len(blocks))
    if not blocks:
        print("\n(Nenhuma cena com o prefixo. Nomeie a 1ª cena de cada bloco como '%s NOME'.)" % prefix)
    for label, s, e in blocks:
        names = [ls.scene_name(i).strip() for i in range(s, e)]
        clips = sum(1 for i in range(s, e) if scene_clip_count(ls, i, csls) > 0)
        print("  [%3d..%3d] (%2d cenas, %2d c/ clips)  %r" % (s, e - 1, e - s, clips, label))
    # checagem de jumps que cruzam blocos
    print("\nJumps de clip ativos (FollowActionA/B==%s): use 'reorder' para remapear." % JUMP_ACTION)


# --------------------------------------------------------------------------------------
# reorder
# --------------------------------------------------------------------------------------


def build_permutation(ls, prefix, order_labels):
    """Retorna new_order: lista de índices ANTIGOS na nova ordem (tamanho num_scenes)."""
    head, blocks = detect_blocks(ls, prefix)
    by_label = {}
    for b in blocks:
        if b[0] in by_label:
            raise SystemExit("Erro: rótulo de bloco duplicado: %r" % b[0])
        by_label[b[0]] = b
    want = [s.strip() for s in order_labels if s.strip()]
    if set(want) != set(by_label):
        missing = set(by_label) - set(want)
        extra = set(want) - set(by_label)
        msg = "Erro: a ordem deve ser uma permutação dos blocos.\n"
        if missing:
            msg += "  Faltando: %s\n" % sorted(missing)
        if extra:
            msg += "  Desconhecidos: %s\n" % sorted(extra)
        msg += "  Blocos no set: %s" % list(by_label)
        raise SystemExit(msg)
    new_order = list(range(head))  # cabeçalho fixo
    for label in want:
        _, s, e = by_label[label]
        new_order.extend(range(s, e))
    assert len(new_order) == ls.num_scenes, (len(new_order), ls.num_scenes)
    assert sorted(new_order) == list(range(ls.num_scenes)), "permutação inválida"
    return new_order


def remap_jumps_in_clipspan(span, inv):
    """Reescreve JumpIndexA/B dentro de um <ClipSlot> span quando a FollowAction
    correspondente é jump (FollowActionA/B == JUMP_ACTION). `inv[old]=new` mapeia
    índice antigo de cena -> novo índice. Retorna o span possivelmente modificado."""
    # Processa cada bloco <FollowAction>...</FollowAction>
    out = []
    pos = 0
    while True:
        fa = find_element(span, "FollowAction", pos)
        if not fa:
            out.append(span[pos:])
            break
        fa_open, fa_in_s, fa_in_e, fa_end = fa
        out.append(span[pos:fa_in_s])
        inner = span[fa_in_s:fa_in_e]
        inner = _remap_one_followaction(inner, inv)
        out.append(inner)
        pos = fa_in_e
    return "".join(out)


def _get_value(inner, tag):
    el = find_element(inner, tag)
    if not el:
        return None, None
    open_tag = inner[el[0]:_tag_end(inner, el[0]) + 1]
    m = re.search(r'Value="([^"]*)"', open_tag)
    return (m.group(1) if m else None), el


def _set_jump(inner, tag, inv):
    el = find_element(inner, tag)
    if not el:
        return inner
    o_start = el[0]
    o_gt = _tag_end(inner, o_start)
    open_tag = inner[o_start:o_gt + 1]
    m = re.search(r'(Value=")(\d+)(")', open_tag)
    if not m:
        return inner
    old = int(m.group(2))
    if 0 <= old < len(inv):
        new = inv[old]
        new_open = open_tag[:m.start(2)] + str(new) + open_tag[m.end(2):]
        return inner[:o_start] + new_open + inner[o_gt + 1:]
    return inner


def _remap_one_followaction(inner, inv):
    aval, _ = _get_value(inner, "FollowActionA")
    bval, _ = _get_value(inner, "FollowActionB")
    if aval == JUMP_ACTION:
        inner = _set_jump(inner, "JumpIndexA", inv)
    if bval == JUMP_ACTION:
        inner = _set_jump(inner, "JumpIndexB", inv)
    return inner


def reorder_text(ls, new_order):
    """Aplica a permutação ao texto, devolvendo o novo texto completo."""
    inv = [0] * ls.num_scenes  # inv[old_idx] = new_idx
    for new_idx, old_idx in enumerate(new_order):
        inv[old_idx] = new_idx
    text = ls.text

    # Coleta todos os segmentos a reescrever: o bloco <Scenes> e cada ClipSlotList.
    # Cada um é uma sequência de spans (children) que serão permutados.
    csls = ls.clip_slot_lists()

    edits = []  # (region_start, region_end, new_region_text)

    # --- Scenes ---
    scene_strs = [text[a:b] for (a, b) in ls.scene_spans]
    # whitespace entre cenas: capturamos o "gap" antes de cada cena para preservar layout
    gaps = _gaps(ls.scenes_inner_start, ls.scene_spans, ls.scenes_inner_end, text)
    new_scenes_inner = _rebuild(scene_strs, gaps, new_order, jump_inv=None)
    edits.append((ls.scenes_inner_start, ls.scenes_inner_end, new_scenes_inner))

    # --- cada ClipSlotList ---
    for ins, ine, spans in csls:
        slot_strs = [text[a:b] for (a, b) in spans]
        gaps = _gaps(ins, spans, ine, text)
        new_inner = _rebuild(slot_strs, gaps, new_order, jump_inv=inv)
        edits.append((ins, ine, new_inner))

    # Aplica edits da direita pra esquerda (offsets não deslocam)
    edits.sort(key=lambda e: e[0], reverse=True)
    for start, end, repl in edits:
        text = text[:start] + repl + text[end:]
    return text


def _gaps(inner_start, spans, inner_end, text):
    """Retorna (leading, [gap_after_i...]) — whitespace inicial e entre/após elementos.
    Estratégia: o gap que PRECEDE cada elemento fica associado a ele, exceto o gap
    inicial. Assim, ao mover um elemento, ele carrega seu whitespace de indentação."""
    leading = text[inner_start:spans[0][0]]
    pre = []  # pre[i] = whitespace imediatamente antes do elemento i (i>=1)
    for i in range(1, len(spans)):
        pre.append(text[spans[i - 1][1]:spans[i][0]])
    trailing = text[spans[-1][1]:inner_end]
    return (leading, pre, trailing)


def _rebuild(elem_strs, gaps, new_order, jump_inv):
    """Reconstrói o conteúdo interno permutado. Cada elemento carrega o gap (indentação)
    que vinha ANTES dele, de modo que a indentação fique consistente na nova ordem."""
    leading, pre, trailing = gaps
    n = len(elem_strs)
    # gap[i] = whitespace que separa elementos; usamos um separador uniforme = pre[0]
    # se existir, senão leading. Mantemos o leading original no começo e trailing no fim.
    sep = pre[0] if pre else ""
    out = [leading]
    for k, old_idx in enumerate(new_order):
        s = elem_strs[old_idx]
        if jump_inv is not None:
            s = remap_jumps_in_clipspan(s, jump_inv)
        if k > 0:
            out.append(sep)
        out.append(s)
    out.append(trailing)
    return "".join(out)


def cmd_reorder(path, order_csv, output, prefix):
    text = read_als(path)
    ls = LiveSet(text)
    new_order = build_permutation(ls, prefix, order_csv.split(","))
    new_text = reorder_text(ls, new_order)
    if output is None:
        if path.endswith(".als"):
            output = path[:-4] + ".reordered.als"
        else:
            output = path + ".reordered.als"
    write_als(output, new_text)
    moved = sum(1 for i, o in enumerate(new_order) if i != o)
    print("OK: %d cenas, %d reposicionadas." % (ls.num_scenes, moved))
    print("Saída:", output)


# --------------------------------------------------------------------------------------
# IO .als (gzip)
# --------------------------------------------------------------------------------------


def read_als(path):
    with open(path, "rb") as f:
        head = f.read(2)
    if head == b"\x1f\x8b":
        return gzip.open(path).read().decode("utf-8")
    # talvez já seja XML cru
    return open(path, "r", encoding="utf-8").read()


def write_als(path, text):
    raw = text.encode("utf-8")
    with gzip.open(path, "wb") as f:
        f.write(raw)


# --------------------------------------------------------------------------------------
# selftest (fixture sintética)
# --------------------------------------------------------------------------------------

SYN = """<?xml version="1.0" encoding="UTF-8"?>
<Ableton>
\t<LiveSet>
\t\t<Tracks>
\t\t\t<MidiTrack Id="1">
\t\t\t\t<ClipSlotList>
{T1_SLOTS}
\t\t\t\t</ClipSlotList>
\t\t\t</MidiTrack>
\t\t</Tracks>
\t\t<Scenes>
{SCENES}
\t\t</Scenes>
\t</LiveSet>
</Ableton>
"""


def _syn_scene(idx, name):
    return (
        '\t\t\t<Scene Id="%d">\n'
        '\t\t\t\t<FollowAction>\n'
        '\t\t\t\t\t<FollowActionA Value="4" />\n'
        '\t\t\t\t\t<JumpIndexA Value="1" />\n'
        '\t\t\t\t</FollowAction>\n'
        '\t\t\t\t<Name Value="%s" />\n'
        '\t\t\t</Scene>' % (idx, name)
    )


def _syn_slot(idx, jumpA=None):
    # se jumpA != None, cria um clip com FollowActionA=JUMP_ACTION saltando p/ cena jumpA
    if jumpA is None:
        clip = ""
    else:
        clip = (
            '\n\t\t\t\t\t\t<Value>\n'
            '\t\t\t\t\t\t\t<MidiClip Id="%d">\n'
            '\t\t\t\t\t\t\t\t<Name Value="clip%d" />\n'
            '\t\t\t\t\t\t\t\t<FollowAction>\n'
            '\t\t\t\t\t\t\t\t\t<FollowActionA Value="%s" />\n'
            '\t\t\t\t\t\t\t\t\t<FollowActionB Value="0" />\n'
            '\t\t\t\t\t\t\t\t\t<JumpIndexA Value="%d" />\n'
            '\t\t\t\t\t\t\t\t\t<JumpIndexB Value="0" />\n'
            '\t\t\t\t\t\t\t\t</FollowAction>\n'
            '\t\t\t\t\t\t\t</MidiClip>\n'
            '\t\t\t\t\t\t</Value>' % (idx, idx, JUMP_ACTION, jumpA)
        )
    return (
        '\t\t\t\t\t<ClipSlot Id="%d">%s\n'
        '\t\t\t\t\t</ClipSlot>' % (idx, clip)
    )


def _build_syn():
    # 6 cenas: head(1) + bloco A (>>A em 1, cenas 1-2) + bloco B (>>B em 3, cenas 3-5)
    names = ["", ">>A", "", ">>B", "", ""]
    scenes = "\n".join(_syn_scene(i, names[i]) for i in range(6))
    # clip na cena 1 que salta para cena 4 (dentro do bloco B)
    jumps = {1: 4}
    slots = "\n".join(_syn_slot(i, jumps.get(i)) for i in range(6))
    return SYN.format(T1_SLOTS=slots, SCENES=scenes)


def cmd_selftest():
    text = _build_syn()
    ls = LiveSet(text)
    assert ls.num_scenes == 6, ls.num_scenes
    csls = ls.clip_slot_lists()
    assert len(csls) == 1, "esperava 1 ClipSlotList com 6 slots, achei %d" % len(csls)
    head, blocks = detect_blocks(ls, ">>")
    assert head == 1, head
    assert [b[0] for b in blocks] == ["A", "B"], blocks
    assert blocks[0][1:] == (1, 3) and blocks[1][1:] == (3, 6), blocks
    print("selftest: detecção de blocos OK (head=1, A=[1,3), B=[3,6))")

    # Reordena para B, A
    new_order = build_permutation(ls, ">>", ["B", "A"])
    # esperado: head(0) + B(3,4,5) + A(1,2) => [0,3,4,5,1,2]
    assert new_order == [0, 3, 4, 5, 1, 2], new_order
    print("selftest: permutação B,A => %s OK" % new_order)

    new_text = reorder_text(ls, new_order)
    ls2 = LiveSet(new_text)
    # nomes na nova ordem
    names2 = [ls2.scene_name(i).strip() for i in range(6)]
    assert names2 == ["", ">>B", "", "", ">>A", ""], names2
    print("selftest: ordem das cenas após reorder OK -> %s" % names2)

    # O clip que estava na cena 1 (jump->4) agora deve estar na nova posição de 1.
    # nova posição de 1 = inv[1]; e o jump 4 -> inv[4].
    inv = [0] * 6
    for ni, oi in enumerate(new_order):
        inv[oi] = ni
    # posição nova do clip:
    clip_new_pos = inv[1]  # = 4
    csls2 = ls2.clip_slot_lists()
    a, b = csls2[0][2][clip_new_pos]
    slot = new_text[a:b]
    m = re.search(r'JumpIndexA Value="(\d+)"', slot)
    assert m, "clip não encontrado na nova posição"
    got = int(m.group(1))
    assert got == inv[4], "jump deveria virar %d (inv[4]), virou %s" % (inv[4], got)
    print("selftest: remap de jump OK (clip movido p/ cena %d, jump 4 -> %d)" % (clip_new_pos, got))

    # nenhum outro slot deve ter clip
    others = 0
    for i in range(6):
        if i == clip_new_pos:
            continue
        aa, bb = csls2[0][2][i]
        if "<MidiClip" in new_text[aa:bb]:
            others += 1
    assert others == 0, "clip vazou para outras cenas: %d" % others
    print("selftest: lockstep OK (clip viajou só com sua cena)")
    print("\nTODOS OS TESTES PASSARAM ✅")


# --------------------------------------------------------------------------------------
# CLI
# --------------------------------------------------------------------------------------


def main(argv=None):
    ap = argparse.ArgumentParser(description="Reordena blocos de música (cenas) num .als")
    sub = ap.add_subparsers(dest="cmd", required=True)

    pi = sub.add_parser("inspect", help="lista blocos detectados")
    pi.add_argument("input")
    pi.add_argument("--prefix", default=DEFAULT_PREFIX)

    pr = sub.add_parser("reorder", help="gera um novo .als com os blocos reordenados")
    pr.add_argument("input")
    pr.add_argument("--order", required=True, help='nova ordem dos rótulos, ex.: "B,A,E"')
    pr.add_argument("--output", default=None)
    pr.add_argument("--prefix", default=DEFAULT_PREFIX)

    sub.add_parser("selftest", help="roda testes internos numa fixture sintética")

    args = ap.parse_args(argv)
    if args.cmd == "inspect":
        cmd_inspect(args.input, args.prefix)
    elif args.cmd == "reorder":
        cmd_reorder(args.input, args.order, args.output, args.prefix)
    elif args.cmd == "selftest":
        cmd_selftest()


if __name__ == "__main__":
    main()
