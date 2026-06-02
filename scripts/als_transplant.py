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


def compare(a_text, b_text):
    """Compara A e B. Retorna (compatible: bool, lines: [str])."""
    ta = collect_tracks(a_text)
    tb = collect_tracks(b_text)
    lines = []
    ok = True

    def key(t):
        return (t[0], t[1])  # (type, name)

    ka = [key(t) for t in ta]
    kb = [key(t) for t in tb]

    lines.append("Tracks em A: %d | em B: %d" % (len(ta), len(tb)))

    # 1) Roster: mesmo conjunto e mesma ordem?
    set_a, set_b = set(ka), set(kb)
    only_a = [k for k in ka if k not in set_b]
    only_b = [k for k in kb if k not in set_a]
    if only_a:
        ok = False
        lines.append("  ✗ Só em A (faltam em B): %s" % only_a)
    if only_b:
        ok = False
        lines.append("  ✗ Só em B (extra, não existem em A): %s" % only_b)
    if not only_a and not only_b and ka != kb:
        ok = False
        lines.append("  ✗ Mesmo conjunto de tracks, mas ORDEM diferente:")
        lines.append("      A: %s" % [n for _, n in ka])
        lines.append("      B: %s" % [n for _, n in kb])

    # 2) Por track casada (mesmo type+name): cadeia de device idêntica?
    pa = {key(t): track_profile(a_text, t) for t in ta}
    pb = {key(t): track_profile(b_text, t) for t in tb}
    matched = [k for k in ka if k in set_b]
    dev_diffs = 0
    for k in matched:
        a, b = pa[k], pb[k]
        msgs = []
        if a["devices"] != b["devices"]:
            msgs.append(_describe_device_diff(a["devices"], b["devices"]))
        if (a["auto_targets"], a["ctrl_targets"]) != (b["auto_targets"], b["ctrl_targets"]):
            msgs.append("nº de alvos de automação difere (A=%d auto/%d ctrl | B=%d auto/%d ctrl)"
                        % (a["auto_targets"], a["ctrl_targets"],
                           b["auto_targets"], b["ctrl_targets"]))
        if msgs:
            ok = False
            dev_diffs += 1
            lines.append("  ✗ [%s] %s:" % (k[0], k[1]))
            for m in msgs:
                lines.append("      - " + m)
    if matched and dev_diffs == 0:
        lines.append("  ✓ %d tracks casadas: cadeias de device e contagem de alvos idênticas"
                     % len(matched))

    lines.append("")
    lines.append("RESULTADO: %s" % ("COMPATÍVEL para transplante ✓" if ok
                                     else "INCOMPATÍVEL — não prosseguir ✗"))
    return ok, lines


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
    print("A:", os.path.basename(a_path))
    print("B:", os.path.basename(b_path))
    ok, lines = compare(a_text, b_text)
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
    # idênticos -> compatível
    ok, _ = compare(a, _syn(base))
    assert ok, "esperava COMPATÍVEL p/ projetos idênticos"
    print("selftest: A vs A idêntico -> COMPATÍVEL ✓")

    # device a mais numa track -> incompatível, apontando a track
    mod = [t if t[1] != "DRUMS" else ("MidiTrack", "DRUMS",
           t[2] + [("Saturator", "")]) for t in base]
    ok2, lines2 = compare(a, _syn(mod))
    assert not ok2, "esperava INCOMPATÍVEL com device a mais"
    assert any("DRUMS" in l for l in lines2) and any("devices diferem" in l for l in lines2), lines2
    print("selftest: device extra em DRUMS -> INCOMPATÍVEL apontando a track ✓")

    # track renomeada/removida -> roster diff
    ren = [("MidiTrack", "BEATS", base[0][2])] + base[1:]
    ok3, lines3 = compare(a, _syn(ren))
    assert not ok3 and any("Só em A" in l for l in lines3), lines3
    print("selftest: roster diff (DRUMS->BEATS) detectado ✓")

    # ordem trocada -> detecta ordem diferente
    swp = [base[1], base[0], base[2]]
    ok4, lines4 = compare(a, _syn(swp))
    assert not ok4 and any("ORDEM diferente" in l for l in lines4), lines4
    print("selftest: ordem de tracks diferente detectada ✓")
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
