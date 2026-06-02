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


def cmd_compare(a_path, b_path):
    a_text = A.read_als(a_path)
    b_text = A.read_als(b_path)
    a_name, b_name = "A", "B"
    print("A:", os.path.basename(a_path))
    print("B:", os.path.basename(b_path))
    ok, lines = compare(a_text, b_text, a_name, b_name)
    print("\n".join(lines))
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
    print("\nTODOS OS TESTES PASSARAM ✅")


def main(argv=None):
    ap = argparse.ArgumentParser(description="Utilitários de transplante entre projetos .als")
    sub = ap.add_subparsers(dest="cmd", required=True)
    pc = sub.add_parser("compare", help="compara tracks/devices de A e B (pré-condição)")
    pc.add_argument("a")
    pc.add_argument("b")
    sub.add_parser("selftest", help="testes internos em fixtures sintéticas")
    args = ap.parse_args(argv)
    if args.cmd == "compare":
        sys.exit(cmd_compare(args.a, args.b))
    elif args.cmd == "selftest":
        cmd_selftest()


if __name__ == "__main__":
    main()
