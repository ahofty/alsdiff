#!/usr/bin/env python3
"""
als_transplant.py — utilitários para transplantar blocos de música ENTRE projetos .als.

Fase B do roadmap (ver docs/roadmap-subset-transplant-merge.md). Por ora implementa só o
VALIDADOR (pré-condição de segurança): comparar dois projetos A e B e dizer se são
"compatíveis para transplante" — mesmo roster de tracks e mesmas cadeias de device — para
que o remap posicional de PointeeId (automação interna dos clips) seja seguro. Se algo
divergir, aponta exatamente o quê e NÃO deixa seguir.

Por que isso importa: ao mover um clip de A para a track casada de B, os `<ClipEnvelope>`
do clip apontam para alvos de parâmetro (ids) da cadeia de device de A. O remap A→B só é
seguro se as cadeias forem estruturalmente idênticas (mesmos devices, mesma ordem, mesma
contagem de alvos). A SDK da Ableton NÃO ajuda aqui (não expõe automação de clip).

Uso:
  als_transplant.py compare A.als B.als
"""

import argparse
import bisect
import re
import sys
import os

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import als_reorder as A  # noqa: E402  (scanner XML quote/depth-aware já testado)

TRACK_TYPES = ("MidiTrack", "AudioTrack", "GroupTrack", "ReturnTrack")
_TRACK_OPEN_RE = re.compile(r"<(" + "|".join(TRACK_TYPES) + r")(?=[\s/>])")


def _first_value(span, tag):
    """Primeiro Value="..." de <tag ...> dentro de span (ou None)."""
    el = A.find_element(span, tag)
    if not el:
        return None
    open_tag = span[el[0]:A._tag_end(span, el[0]) + 1]
    m = re.search(r'Value="([^"]*)"', open_tag)
    return m.group(1) if m else None


def track_effective_name(span):
    return _first_value(span, "EffectiveName") or ""


def device_signature(track_span):
    """Assinatura ordenada da cadeia de devices da track: para CADA container <Devices>
    (em qualquer profundidade, na ordem do documento), a lista de (tag, UserName) dos seus
    filhos DIRETOS. Captura racks aninhados (cada device é filho direto de exatamente um
    <Devices>). Retorna lista de tuplas achatadas: [(device_tag, user_name), ...]."""
    sig = []
    for m in re.finditer(r"<Devices(?=[\s/>])", track_span):
        lt = m.start()
        gt = A._tag_end(track_span, lt)
        if track_span[gt - 1] == "/":
            continue  # <Devices /> vazio
        # inner deste container
        el = A.find_element(track_span, "Devices", lt)
        if not el:
            continue
        _, ins, ine, _end = el
        inner = track_span[ins:ine]
        for (name, a, b) in A.child_elements(inner):
            uname = _first_value(inner[a:b], "UserName")
            sig.append((name, uname if uname is not None else ""))
    return sig


def automation_target_count(track_span):
    """Quantos alvos de automação a cadeia define (proxy p/ identidade de parâmetros).
    Conta AutomationTarget e os ControllerTargets.N (alvos típicos de PointeeId)."""
    at = len(re.findall(r"<AutomationTarget Id=", track_span))
    ct = len(re.findall(r"<ControllerTargets\.\d+ Id=", track_span))
    return at, ct


def collect_tracks(text):
    """Lista as tracks na ordem do documento: [(type, effname, span_start, span_end)]."""
    out = []
    for m in _TRACK_OPEN_RE.finditer(text):
        tag = m.group(1)
        el = A.find_element(text, tag, m.start())
        if not el or el[0] != m.start():
            continue
        span = text[el[0]:el[3]]
        out.append((tag, track_effective_name(span), el[0], el[3]))
    return out


def track_profile(text, t):
    tag, name, s, e = t
    span = text[s:e]
    at, ct = automation_target_count(span)
    return {
        "type": tag,
        "name": name,
        "devices": device_signature(span),
        "auto_targets": at,
        "ctrl_targets": ct,
    }


def _is_subsequence(small, big):
    """True se `small` é subsequência de `big` (big = small com inserções, ordem preservada).
    Casamento guloso — alvos idênticos repetidos podem casar de forma não-única, mas isso não
    afeta a verdade da relação de contenção."""
    it = iter(big)
    return all(x in it for x in small)


def _direction_report(src_name, dst_name, src_tracks, dst_profiles, src_profiles):
    """Avalia se é seguro mover clips de SRC para DST: todas as tracks de SRC existem em DST e,
    por track, a cadeia de device de SRC está CONTIDA na de DST (subsequência). Retorna
    (safe, lines)."""
    lines = []
    safe = True
    dst_keys = set(dst_profiles)
    # roster: toda track de SRC tem que existir em DST (DST pode ter extras)
    missing = [k for k in src_tracks if k not in dst_keys]
    if missing:
        safe = False
        lines.append("    ✗ tracks de %s ausentes em %s: %s"
                     % (src_name, dst_name, [n for _, n in missing]))
    # devices: por track casada, SRC ⊆ DST
    bad = 0
    for k in src_tracks:
        if k not in dst_keys:
            continue
        s, d = src_profiles[k]["devices"], dst_profiles[k]["devices"]
        if not _is_subsequence(s, d):
            safe = False
            bad += 1
            from collections import Counter
            extra = Counter(map(_dev_label, s)) - Counter(map(_dev_label, d))
            lines.append("    ✗ [%s] %s: %s tem devices que %s não tem: %s"
                         % (k[0], k[1], src_name, dst_name,
                            _top(extra, 8) if extra else "(ordem incompatível)"))
    if safe:
        lines.append("    ✓ todas as tracks de %s existem em %s e suas cadeias estão contidas"
                     % (src_name, dst_name))
    return safe, lines


def compare(a_text, b_text, a_name="A", b_name="B"):
    """Compara A e B de forma DIRECIONAL p/ transplante. Retorna (compatible_any, lines).
    compatible_any = True se ao menos uma direção (A→B ou B→A) é segura."""
    ta = collect_tracks(a_text)
    tb = collect_tracks(b_text)

    def key(t):
        return (t[0], t[1])  # (type, name)

    pa = {key(t): track_profile(a_text, t) for t in ta}
    pb = {key(t): track_profile(b_text, t) for t in tb}
    ka = [key(t) for t in ta]
    kb = [key(t) for t in tb]

    lines = ["Tracks em %s: %d | em %s: %d" % (a_name, len(ta), b_name, len(tb))]

    identical = (ka == kb) and all(pa[k]["devices"] == pb[k]["devices"] for k in ka)
    if identical:
        lines.append("  ✓ Projetos idênticos em tracks e cadeias de device — seguro nos 2 sentidos.")
        return True, lines

    safe_ab, lines_ab = _direction_report(a_name, b_name, ka, pb, pa)
    safe_ba, lines_ba = _direction_report(b_name, a_name, kb, pa, pb)

    lines.append("")
    lines.append("Direção %s→%s (mover clips de %s para %s): %s"
                 % (a_name, b_name, a_name, b_name, "SEGURO ✓" if safe_ab else "INSEGURO ✗"))
    lines += lines_ab
    lines.append("")
    lines.append("Direção %s→%s (mover clips de %s para %s): %s"
                 % (b_name, a_name, b_name, a_name, "SEGURO ✓" if safe_ba else "INSEGURO ✗"))
    lines += lines_ba

    lines.append("")
    if safe_ab and safe_ba:
        verdict = "COMPATÍVEL nos dois sentidos ✓"
    elif safe_ab:
        verdict = "Só %s→%s é seguro" % (a_name, b_name)
    elif safe_ba:
        verdict = "Só %s→%s é seguro" % (b_name, a_name)
    else:
        verdict = "INCOMPATÍVEL nos dois sentidos ✗"
    lines.append("RESULTADO: " + verdict)
    return (safe_ab or safe_ba), lines


def block_report(a_text, b_text, prefix=A.DEFAULT_PREFIX, a_name="A", b_name="B"):
    """Compara as MÚSICAS (blocos, pelo prefixo de nome) de A e B, só por NOME.
    Retorna lista de linhas: comuns, só em A, só em B."""
    la = A.detect_blocks(A.LiveSet(a_text), prefix)[1]
    lb = A.detect_blocks(A.LiveSet(b_text), prefix)[1]
    names_a = [b[0] for b in la]
    names_b = [b[0] for b in lb]
    sa, sb = set(names_a), set(names_b)
    lines = ["Músicas (blocos '%s') — %s: %d | %s: %d" % (prefix, a_name, len(names_a),
                                                          b_name, len(names_b))]
    for nm, names in ((a_name, names_a), (b_name, names_b)):
        dups = sorted({x for x in names if names.count(x) > 1})
        if dups:
            lines.append("  ⚠ nomes repetidos em %s: %s" % (nm, dups))
    common = [n for n in names_a if n in sb]
    only_a = [n for n in names_a if n not in sb]
    only_b = [n for n in names_b if n not in sa]
    lines.append("  Em comum (%d): %s" % (len(common), ", ".join(common) if common else "—"))
    lines.append("  Só em %s (%d): %s" % (a_name, len(only_a),
                                          ", ".join(only_a) if only_a else "—"))
    lines.append("  Só em %s (%d): %s" % (b_name, len(only_b),
                                          ", ".join(only_b) if only_b else "—"))
    return lines


# --------------------------------------------------------------------------------------
# Remap de PointeeId (alvos de automação) entre projetos — núcleo da correção do transplante
# --------------------------------------------------------------------------------------

TARGET_TAGS = ("AutomationTarget", "ModulationTarget", "Pointee")
_TARGET_RE = re.compile(r'<(?:AutomationTarget|ModulationTarget|Pointee) [^>]*\bId="(\d+)"')


def _target_ids_ordered(span):
    """Ids de alvo (AutomationTarget/ModulationTarget/Pointee) em ordem de documento."""
    return [(int(m.group(1)), m.start()) for m in _TARGET_RE.finditer(span)]


def _param_context(text, tid, n=5):
    """Assinatura do parâmetro que tem Id=tid: as últimas n tags abertas antes dele.
    Serve para CONFIRMAR que um remap candidato aponta para o mesmo tipo de parâmetro."""
    m = re.search(r'<[A-Za-z0-9_.]+[^>]*\bId="%d"' % tid, text)
    if not m:
        return None
    before = text[max(0, m.start() - 300):m.start()]
    return tuple(re.findall(r'<([A-Za-z0-9_.]+)[ >]', before)[-n:])


def flatten_devices_spans(track_span):
    """Como device_signature, mas devolve (tag, uname, start, end) — span completo de cada
    device, em ordem de documento (filhos diretos de cada container <Devices>, aninhados incl.)."""
    devs = []
    for m in re.finditer(r"<Devices(?=[\s/>])", track_span):
        lt = m.start()
        gt = A._tag_end(track_span, lt)
        if track_span[gt - 1] == "/":
            continue
        el = A.find_element(track_span, "Devices", lt)
        if not el:
            continue
        _, ins, ine, _ = el
        inner = track_span[ins:ine]
        for (name, a, b) in A.child_elements(inner):
            uname = _first_value(inner[a:b], "UserName") or ""
            devs.append((name, uname, ins + a, ins + b))
    return devs


def _per_device_targets(track_span):
    """Devolve (devs, dev_targets): devs=[(tag,uname,start,end)] e dev_targets[i]=lista de ids
    de alvo cujo device INNERMOST é devs[i] (em ordem de documento)."""
    devs = flatten_devices_spans(track_span)
    targets = _target_ids_ordered(track_span)
    dev_targets = [[] for _ in devs]
    order = sorted(range(len(devs)), key=lambda i: devs[i][2])  # por start asc
    for tid, pos in targets:
        best, bests = -1, -1
        for i in order:
            s, e = devs[i][2], devs[i][3]
            if s > pos:
                break
            if s <= pos < e and s > bests:
                best, bests = i, s
        if best >= 0:
            dev_targets[best].append(tid)
    return devs, dev_targets


def build_track_pointee_map(a_span, b_span):
    """Mapa id_de_alvo_A -> id_de_alvo_B para uma track casada. Cadeia idêntica => zip
    posicional; divergente => alinha devices (subsequência A⊆B) e faz zip por device casado.
    Lança SystemExit se não der p/ casar com segurança."""
    sig_a, sig_b = device_signature(a_span), device_signature(b_span)
    ta = [t for t, _ in _target_ids_ordered(a_span)]
    tb = [t for t, _ in _target_ids_ordered(b_span)]
    if sig_a == sig_b:
        if len(ta) != len(tb):
            raise SystemExit("Cadeias idênticas mas nº de alvos difere (%d vs %d)" % (len(ta), len(tb)))
        return dict(zip(ta, tb))
    # divergente: alinhamento por device. LENIENTE: devices casados com nº de alvos
    # diferente (ex.: rack onde se adicionou um device interno → muda alvos do mixer da
    # chain) ficam AMBÍGUOS e seus alvos NÃO entram no mapa. O transplante só aborta se um
    # pointee REALMENTE USADO cair num device ambíguo (verificação no remap).
    devs_a, dta = _per_device_targets(a_span)
    devs_b, dtb = _per_device_targets(b_span)

    def key(d):
        return (d[0], d[1])

    mapping, ambiguous = {}, 0
    j = 0
    for i, da in enumerate(devs_a):
        jj = j
        while jj < len(devs_b) and key(devs_b[jj]) != key(da):
            jj += 1
        if jj >= len(devs_b):
            ambiguous += 1  # não achou (não deveria, com A⊆B); deixa alvos sem mapa
            continue
        j = jj
        la, lb = dta[i], dtb[j]
        if len(la) == len(lb):
            mapping.update(zip(la, lb))
        else:
            # device casado mas com nº de alvos diferente (ex.: rack com chain adicionada →
            # mixers de chain extras no FIM da lista de alvos do rack; macros/params ficam no
            # início). Mapeia o PREFIXO comum (estáveis) e deixa o excedente sem mapa.
            ambiguous += 1
            mapping.update(zip(la, lb))  # zip já trunca no menor → mapeia o prefixo comum
        j += 1
    return mapping


def _dev_label(d):
    tag, uname = d
    return tag + (("/" + uname) if uname else "")


def _describe_device_diff(a, b, max_items=8):
    """Diff CONCISO entre duas cadeias de device (listas de (tag, uname)): tamanhos,
    1º ponto de divergência e multiset adicionado/removido (limitado)."""
    from collections import Counter
    parts = ["devices diferem (A=%d, B=%d)" % (len(a), len(b))]
    # 1ª divergência posicional
    first = next((i for i in range(min(len(a), len(b))) if a[i] != b[i]), min(len(a), len(b)))
    if first < max(len(a), len(b)):
        av = _dev_label(a[first]) if first < len(a) else "—"
        bv = _dev_label(b[first]) if first < len(b) else "—"
        parts.append("1ª divergência na posição %d: A=%s | B=%s" % (first, av, bv))
    added = Counter(map(_dev_label, b)) - Counter(map(_dev_label, a))
    removed = Counter(map(_dev_label, a)) - Counter(map(_dev_label, b))
    if added:
        parts.append("só em B (+%d): %s" % (sum(added.values()), _top(added, max_items)))
    if removed:
        parts.append("só em A (+%d): %s" % (sum(removed.values()), _top(removed, max_items)))
    return "; ".join(parts)


def _top(counter, n):
    items = ["%s×%d" % (k, v) if v > 1 else k
             for k, v in sorted(counter.items(), key=lambda kv: -kv[1])[:n]]
    extra = len(counter) - n
    return ", ".join(items) + (" …(+%d tipos)" % extra if extra > 0 else "")


# --------------------------------------------------------------------------------------
# transplant (mutação): mover blocos de SRC para DST, antes de um bloco-âncora
# --------------------------------------------------------------------------------------


def lockstep_csls(text, ls):
    """{(track_type, track_name, occ): (ins, ine, [slot_spans])} para TODAS as ClipSlotLists
    com nº de filhos == num_scenes. Usa o scan global confiável (als_reorder) e associa cada
    CSL à track imediatamente anterior no documento (sua dona), contando a ordem por track."""
    opens = [(m.start(), m.group(1)) for m in _TRACK_OPEN_RE.finditer(text)]
    starts = [o[0] for o in opens]
    out, occ_counter = {}, {}
    for ins, ine, spans in ls.clip_slot_lists():
        i = bisect.bisect_right(starts, ins) - 1
        if i < 0:
            continue
        tstart, ttype = opens[i]
        en = re.search(r'<EffectiveName Value="([^"]*)"', text[tstart:tstart + 4000])
        name = en.group(1) if en else "?"
        occ = occ_counter.get((ttype, name), 0)
        occ_counter[(ttype, name)] = occ + 1
        out[(ttype, name, occ)] = (ins, ine, spans)
    return out


def transplant_text(src, dst, move_labels, before_label, prefix=A.DEFAULT_PREFIX):
    """Move os blocos `move_labels` (nessa ordem) de SRC para DST, inseridos antes do bloco
    `before_label` em DST. Retorna (novo_dst_text, report dict). Aborta (SystemExit) se inseguro."""
    lss, lsd = A.LiveSet(src), A.LiveSet(dst)

    # blocos da origem -> índices de cena movidos (na ordem pedida)
    sby = {b[0]: b for b in A.detect_blocks(lss, prefix)[1]}
    moved = []
    for lab in move_labels:
        if lab not in sby:
            raise SystemExit("bloco %r não existe na origem" % lab)
        _, s, e = sby[lab]
        moved += list(range(s, e))
    nmoved = len(moved)

    # âncora no destino
    dby = {b[0]: b for b in A.detect_blocks(lsd, prefix)[1]}
    if before_label not in dby:
        raise SystemExit("bloco-âncora %r não existe no destino" % before_label)
    insertion = dby[before_label][1]

    # casar tracks por (tipo, nome) e construir mapa global de PointeeId
    tracks_s = {(t[0], t[1]): t for t in collect_tracks(src)}
    tracks_d = {(t[0], t[1]): t for t in collect_tracks(dst)}
    pmap = {}
    for k, ts in tracks_s.items():
        if k not in tracks_d:
            raise SystemExit("track %s da origem não existe no destino — direção insegura" % (k,))
        td = tracks_d[k]
        pmap.update(build_track_pointee_map(src[ts[2]:ts[3]], dst[td[2]:td[3]]))

    # nova posição (índice de cena em DST) de cada cena movida; inv p/ remap de jumps dos clips movidos
    inv_moved = [None] * lss.num_scenes
    for i, old in enumerate(moved):
        inv_moved[old] = insertion + i

    # pré-checagem: clips movidos com "jump to scene" (FA==9) p/ FORA dos blocos movidos.
    # Esses pulos ficariam pendurados no destino → abortar com relatório (como no subset).
    moved_set = set(moved)
    s_lock_pre = lockstep_csls(src, lss)
    dangling = []
    for k, (ins, ine, spans) in s_lock_pre.items():
        for old in moved:
            x, y = spans[old]
            for letter, tgt in A._scan_jumps_in_span(src[x:y]):
                if tgt not in moved_set:
                    dangling.append((old, tgt))
    if dangling:
        sby_lbl = {}
        for lab, s, e in A.detect_blocks(lss, prefix)[1]:
            for idx in range(s, e):
                sby_lbl[idx] = lab
        msg = ["%d clip(s) movido(s) têm 'jump to scene' para FORA dos blocos movidos"
               " (pulo ficaria pendurado). Abortei sem gravar:" % len(dangling)]
        for old, tgt in sorted(set(dangling)):
            msg.append("  cena %d (%s) -> cena %d (%s)"
                       % (old, sby_lbl.get(old, "?"), tgt, sby_lbl.get(tgt, "?")))
        msg.append("Corrija esse follow action na origem (ou peça --disable-dangling, a implementar).")
        raise SystemExit("\n".join(msg))

    # ids novos (namespace pequeno) p/ cenas e colunas de clipslot importadas
    dmax = max([int(x) for x in re.findall(r'<Scene Id="(\d+)"', dst)]
               + [int(x) for x in re.findall(r'<ClipSlot Id="(\d+)"', dst)])
    scene_new_id = [dmax + 1 + i for i in range(nmoved)]
    col_new_id = [dmax + 1 + nmoved + i for i in range(nmoved)]

    s_lock, d_lock = lockstep_csls(src, lss), lockstep_csls(dst, lsd)
    for k in s_lock:
        if k not in d_lock:
            raise SystemExit("ClipSlotList %s da origem não casada no destino" % (k,))

    # Pointees realmente usados pelos clips movidos → resolver mapa FINAL verificado por
    # contexto. Preferimos IDENTIDADE (mesmo id, mesmo parâmetro em B — comum quando os
    # projetos compartilham linhagem) e só caímos no candidato device-aligned se o contexto
    # bater. Abortamos se algum pointee usado não resolver com segurança.
    used = set()
    for k, (ins, ine, spans) in s_lock.items():
        for old in moved:
            x, y = spans[old]
            used.update(int(p) for p in re.findall(r'<PointeeId Value="(\d+)"', src[x:y]))
    final, unresolved = {}, []
    for p in used:
        ca = _param_context(src, p)
        if ca is not None and _param_context(dst, p) == ca:
            final[p] = p  # identidade (contexto confere)
        elif p in pmap and _param_context(dst, pmap[p]) == ca:
            final[p] = pmap[p]  # device-aligned (contexto confere)
        else:
            unresolved.append(p)
    if unresolved:
        msg = ["Não consegui mapear com SEGURANÇA %d PointeeId usados por clips movidos"
               " (parâmetro que mudou no destino). Abortei sem gravar:" % len(unresolved)]
        for p in sorted(unresolved):
            msg.append("  %d  contexto-origem=%s  (mesmo-id-no-destino=%s)"
                       % (p, _param_context(src, p), _param_context(dst, p)))
        raise SystemExit("\n".join(msg))

    def remap_pointees(span):
        def _sub(m):
            return '<PointeeId Value="%d"' % final[int(m.group(1))]
        return re.sub(r'<PointeeId Value="(\d+)"', _sub, span)

    # textos das cenas movidas
    moved_scene_texts = []
    for i, old in enumerate(moved):
        a, b = lss.scene_spans[old]
        sc = src[a:b]
        sc = re.sub(r'<Scene Id="\d+"', '<Scene Id="%d"' % scene_new_id[i], sc, count=1)
        sc = A.remap_jumps_in_clipspan(sc, inv_moved)  # FA==9 de cena (raro); aborta se cruzar
        moved_scene_texts.append(sc)

    # textos dos slots movidos, por CSL casada
    moved_slots = {}
    for k, (ins, ine, spans) in s_lock.items():
        lst = []
        for i, old in enumerate(moved):
            x, y = spans[old]
            sl = src[x:y]
            sl = re.sub(r'<ClipSlot Id="\d+"', '<ClipSlot Id="%d"' % col_new_id[i], sl, count=1)
            sl = remap_pointees(sl)
            sl = A.remap_jumps_in_clipspan(sl, inv_moved)
            lst.append(sl)
        moved_slots[k] = lst

    # FASE 1: deslocar +nmoved os jumps de clip e SavedPlayingSlot do DST que apontam p/ cena >= insertion
    inv_dst = [o if o < insertion else o + nmoved for o in range(lsd.num_scenes)]
    edits1 = []
    for k, (ins, ine, spans) in d_lock.items():
        for x, y in spans:
            sl = dst[x:y]
            if any(t >= insertion for _, t in A._scan_jumps_in_span(sl)):
                ns = A.remap_jumps_in_clipspan(sl, inv_dst)
                if ns != sl:
                    edits1.append((x, y, ns))
    for m in re.finditer(r'(<SavedPlayingSlot Value=")(-?\d+)("\s*/>)', dst):
        v = int(m.group(2))
        if v >= insertion:
            edits1.append((m.start(), m.end(), m.group(1) + str(v + nmoved) + m.group(3)))
    edits1.sort(key=lambda e: e[0], reverse=True)
    d2 = dst
    for s, e, r in edits1:
        d2 = d2[:s] + r + d2[e:]

    # FASE 2: recomputar offsets e inserir cenas + slots antes da âncora
    lsd2 = A.LiveSet(d2)
    d2_lock = lockstep_csls(d2, lsd2)

    def gap_before(prev_end, cur_start, region_start):
        return d2[prev_end:cur_start] if insertion > 0 else d2[region_start:cur_start]

    edits2 = []
    # cenas
    P = lsd2.scene_spans[insertion][0]
    prev = lsd2.scene_spans[insertion - 1][1] if insertion > 0 else lsd2.scenes_inner_start
    gap = d2[prev:P]
    edits2.append((P, P, "".join(t + gap for t in moved_scene_texts)))
    # cada CSL casada
    for k, (ins, ine, spans) in d2_lock.items():
        if k not in moved_slots:
            continue
        Pp = spans[insertion][0]
        prevp = spans[insertion - 1][1] if insertion > 0 else ins
        gp = d2[prevp:Pp]
        edits2.append((Pp, Pp, "".join(t + gp for t in moved_slots[k])))
    edits2.sort(key=lambda e: e[0], reverse=True)
    for s, e, r in edits2:
        d2 = d2[:s] + r + d2[e:]

    report = {
        "moved_blocks": list(move_labels),
        "moved_scenes": nmoved,
        "insertion_scene_index": insertion,
        "csls": len(moved_slots),
        "pointees_used": sorted(final),
        "pointee_remaps": {p: final[p] for p in sorted(final)},
        "pointee_changed": {p: final[p] for p in sorted(final) if final[p] != p},
        "scene_ids": (scene_new_id[0], scene_new_id[-1]) if nmoved else None,
    }
    return d2, report


def cmd_transplant(src_path, dst_path, move_csv, before, output, prefix):
    src = A.read_als(src_path)
    dst = A.read_als(dst_path)
    move_labels = [x.strip() for x in move_csv.split(",") if x.strip()]
    new_dst, rep = transplant_text(src, dst, move_labels, before, prefix)
    if output is None:
        base = dst_path[:-4] if dst_path.endswith(".als") else dst_path
        output = base + ".transplanted.als"
    A.write_als(output, new_dst)
    print("OK: movidos %d blocos (%d cenas) de SRC p/ DST, antes de %r."
          % (len(rep["moved_blocks"]), rep["moved_scenes"], before))
    print("Blocos:", ", ".join(rep["moved_blocks"]))
    print("ClipSlotLists casadas:", rep["csls"])
    print("Scene Ids novos:", rep["scene_ids"])
    print("PointeeId usados pelos clips movidos: %d (identidade: %d | remapeados: %d)"
          % (len(rep["pointee_remaps"]),
             len(rep["pointee_remaps"]) - len(rep["pointee_changed"]),
             len(rep["pointee_changed"])))
    for old, new in rep["pointee_changed"].items():
        print("    remap %d -> %d" % (old, new))
    print("Saída:", output)
    return 0


def cmd_compare(a_path, b_path, prefix=A.DEFAULT_PREFIX):
    a_text = A.read_als(a_path)
    b_text = A.read_als(b_path)
    a_name, b_name = "A", "B"
    print("A:", os.path.basename(a_path))
    print("B:", os.path.basename(b_path))
    ok, lines = compare(a_text, b_text, a_name, b_name)
    print("\n".join(lines))
    print("")
    print("\n".join(block_report(a_text, b_text, prefix, a_name, b_name)))
    return 0 if ok else 1


def _syn(tracks):
    """tracks = [(type, name, [(devtag, uname), ...]), ...] -> XML mínimo de um LiveSet."""
    parts = ['<?xml version="1.0"?>', "<Ableton>", "<LiveSet>", "<Tracks>"]
    for typ, name, devs in tracks:
        parts.append('<%s Id="0">' % typ)
        parts.append('<Name><EffectiveName Value="%s" /></Name>' % name)
        parts.append("<DeviceChain><DeviceChain><Devices>")
        for tag, uname in devs:
            parts.append('<%s><UserName Value="%s" /></%s>' % (tag, uname, tag))
        parts.append("</Devices></DeviceChain></DeviceChain>")
        parts.append("</%s>" % typ)
    parts += ["</Tracks>", "</LiveSet>", "</Ableton>"]
    return "\n".join(parts)


def cmd_selftest():
    base = [
        ("MidiTrack", "DRUMS", [("Eq8", ""), ("Compressor2", "Squash")]),
        ("AudioTrack", "VOX", [("Reverb", "")]),
        ("ReturnTrack", "A-DELAY", [("Delay", "")]),
    ]
    a = _syn(base)
    # idênticos -> seguro nos dois sentidos
    ok, lines = compare(a, _syn(base))
    assert ok and any("idênticos" in l for l in lines), lines
    print("selftest: A vs A idêntico -> seguro nos 2 sentidos ✓")

    # B = A + device adicionado no FIM da chain de DRUMS -> A→B seguro, B→A inseguro
    bigger = [t if t[1] != "DRUMS" else ("MidiTrack", "DRUMS",
              t[2] + [("Saturator", "")]) for t in base]
    ok2, lines2 = compare(a, _syn(bigger), "A", "B")
    txt = "\n".join(lines2)
    assert ok2, "esperava ao menos uma direção segura"
    assert "Direção A→B (mover clips de A para B): SEGURO ✓" in txt, txt
    assert "Direção B→A (mover clips de B para A): INSEGURO ✗" in txt, txt
    assert "Saturator" in txt, txt
    print("selftest: device extra em B -> só A→B seguro (direcional) ✓")

    # track removida em B -> A→B inseguro (track de A ausente em B)
    fewer = base[1:]
    ok3, lines3 = compare(a, _syn(fewer), "A", "B")
    assert any("ausentes em B" in l for l in lines3), lines3
    print("selftest: track de A ausente em B detectada (direcional) ✓")

    # ordem trocada (mesmas tracks/devices) -> ORDEM NÃO importa p/ transplante: seguro
    swp = [base[1], base[0], base[2]]
    ok4, lines4 = compare(a, _syn(swp))
    assert ok4 and any("dois sentidos" in l for l in lines4), lines4
    print("selftest: ordem de tracks diferente NÃO reprova (casa por nome) ✓")

    # ---- build_track_pointee_map ----
    def trk(devs):
        # devs = [(tag, [target_ids])]
        s = '<MidiTrack Id="0"><Name><EffectiveName Value="T" /></Name>'
        s += "<DeviceChain><DeviceChain><Devices>"
        for tag, tids in devs:
            s += "<%s>" % tag
            for t in tids:
                s += '<P><AutomationTarget Id="%d" /></P>' % t
            s += "</%s>" % tag
        s += "</Devices></DeviceChain></DeviceChain></MidiTrack>"
        return s
    # cadeia idêntica (ids diferentes entre projetos) -> zip posicional
    a1 = trk([("Eq8", [10, 11]), ("Compressor2", [12])])
    b1 = trk([("Eq8", [20, 21]), ("Compressor2", [22])])
    m1 = build_track_pointee_map(a1, b1)
    assert m1 == {10: 20, 11: 21, 12: 22}, m1
    print("selftest: pointee map cadeia idêntica (zip) OK %s" % m1)
    # cadeia divergente: B tem device EXTRA no fim -> alvos do extra são ignorados
    b2 = trk([("Eq8", [20, 21]), ("Compressor2", [22]), ("Roar", [23, 24])])
    m2 = build_track_pointee_map(a1, b2)
    assert m2 == {10: 20, 11: 21, 12: 22}, m2
    print("selftest: pointee map device EXTRA no destino (alinhado) OK %s" % m2)
    # divergente: device extra NO MEIO -> alinhamento por device ainda acerta
    b3 = trk([("Eq8", [20, 21]), ("Saturator", [99]), ("Compressor2", [22])])
    m3 = build_track_pointee_map(a1, b3)
    assert m3 == {10: 20, 11: 21, 12: 22}, m3
    print("selftest: pointee map device extra NO MEIO (alinhado) OK %s" % m3)
    print("\nTODOS OS TESTES PASSARAM ✅")


def main(argv=None):
    ap = argparse.ArgumentParser(description="Utilitários de transplante entre projetos .als")
    sub = ap.add_subparsers(dest="cmd", required=True)
    pc = sub.add_parser("compare", help="compara tracks/devices de A e B (pré-condição) + músicas")
    pc.add_argument("a")
    pc.add_argument("b")
    pc.add_argument("--prefix", default=A.DEFAULT_PREFIX, help="prefixo de nome de bloco (def: >>)")

    pt = sub.add_parser("transplant", help="move blocos de SRC para DST antes de um bloco-âncora")
    pt.add_argument("src")
    pt.add_argument("dst")
    pt.add_argument("--move", required=True, help='blocos a mover, na ordem: "caught up,heaven"')
    pt.add_argument("--before", required=True, help="nome do bloco-âncora no destino")
    pt.add_argument("--output", default=None)
    pt.add_argument("--prefix", default=A.DEFAULT_PREFIX)

    sub.add_parser("selftest", help="testes internos em fixtures sintéticas")
    args = ap.parse_args(argv)
    if args.cmd == "compare":
        sys.exit(cmd_compare(args.a, args.b, args.prefix))
    elif args.cmd == "transplant":
        sys.exit(cmd_transplant(args.src, args.dst, args.move, args.before, args.output, args.prefix))
    elif args.cmd == "selftest":
        cmd_selftest()


if __name__ == "__main__":
    main()
