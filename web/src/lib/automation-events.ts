import type {
  FieldView,
  ItemView,
  ViewNode,
  AutomationEvent,
  AutomationRange,
  CurveControls,
} from "../types";

function clamp01(v: number): number {
  return Math.min(1, Math.max(0, v));
}

// --- Old format: parse from name string ---

const RE_ADDED_REMOVED = /Time=([\d.-]+),\s*Value=([\d.]+)/;
const RE_MODIFIED_TIME = /Time:\s*([\d.-]+)->([\d.]+)/;
const RE_MODIFIED_VALUE = /Value:\s*([\d.]+)->([\d.]+)/;
const RE_CURVE = /Curve1=\(([\d.]+),([\d.]+)\)\s*Curve2=\(([\d.]+),([\d.]+)\)/;

function parseCurve(name: string): CurveControls | undefined {
  const cm = name.match(RE_CURVE);
  if (!cm) return undefined;
  return {
    curve1X: clamp01(parseFloat(cm[1])),
    curve1Y: clamp01(parseFloat(cm[2])),
    curve2X: clamp01(parseFloat(cm[3])),
    curve2Y: clamp01(parseFloat(cm[4])),
  };
}

function parseOldFormat(child: ItemView): AutomationEvent | undefined {
  const name = child.name;

  if (child.change === "Added" || child.change === "Removed") {
    const m = name.match(RE_ADDED_REMOVED);
    if (m) {
      return {
        time: parseFloat(m[1]),
        value: parseFloat(m[2]),
        change: child.change,
        curve: parseCurve(name),
      };
    }
  } else if (child.change === "Modified") {
    const tm = name.match(RE_MODIFIED_TIME);
    const vm = name.match(RE_MODIFIED_VALUE);
    if (tm || vm) {
      let time: number;
      if (tm) {
        time = parseFloat(tm[2]);
      } else {
        const fallback = name.match(/Time=([\d.]+)/);
        time = fallback ? parseFloat(fallback[1]) : 0;
      }
      return {
        time,
        value: vm ? parseFloat(vm[2]) : 0,
        change: "Modified",
        oldTime: tm ? parseFloat(tm[1]) : undefined,
        oldValue: vm ? parseFloat(vm[1]) : undefined,
        curve: parseCurve(name),
      };
    }
  }

  return undefined;
}

// --- New format: parse from structured children ---

function getField(
  children: ViewNode[],
  name: string,
): { old: number | undefined; new: number | undefined } {
  const f = children.find(
    (c) => c.type === "field" && c.name === name,
  );
  if (!f) return { old: undefined, new: undefined };
  const field = f as FieldView;
  return {
    old: typeof field.old_value === "number" ? field.old_value : undefined,
    new: typeof field.new_value === "number" ? field.new_value : undefined,
  };
}

function getCurveControls(
  children: ViewNode[],
  useOld: boolean,
): CurveControls | undefined {
  const curveItem = children.find(
    (c) => c.type === "item" && c.name === "Curve",
  );
  if (!curveItem || !("children" in curveItem) || !curveItem.children)
    return undefined;
  const cc = curveItem.children;
  const c1x = getField(cc, "Curve1 X");
  const c1y = getField(cc, "Curve1 Y");
  const c2x = getField(cc, "Curve2 X");
  const c2y = getField(cc, "Curve2 Y");
  const val = useOld ? c1x.old : (c1x.new ?? c1x.old);
  if (val === undefined) return undefined;
  const y1 = useOld ? (c1y.old ?? 0) : (c1y.new ?? c1y.old ?? 0);
  const x2 = useOld ? (c2x.old ?? 0) : (c2x.new ?? c2x.old ?? 0);
  const y2 = useOld ? (c2y.old ?? 0) : (c2y.new ?? c2y.old ?? 0);
  return {
    curve1X: clamp01(val),
    curve1Y: clamp01(y1),
    curve2X: clamp01(x2),
    curve2Y: clamp01(y2),
  };
}

function parseNewFormat(child: ItemView): AutomationEvent | undefined {
  const fields = child.children ?? [];
  if (fields.length === 0) return undefined;

  const timeF = getField(fields, "Time");
  const valueF = getField(fields, "Value");

  if (child.change === "Added" || child.change === "Removed") {
    const t = timeF.new;
    if (t === undefined) return undefined;
    return {
      time: t,
      value: valueF.new ?? 0,
      change: child.change,
      curve: getCurveControls(fields, false),
    };
  } else if (child.change === "Modified") {
    const t = timeF.new ?? timeF.old;
    if (t === undefined) return undefined;
    return {
      time: t,
      value: valueF.new ?? valueF.old ?? 0,
      change: "Modified",
      oldTime: timeF.old,
      oldValue: valueF.old,
      curve: getCurveControls(fields, false),
      oldCurve: getCurveControls(fields, true),
    };
  }

  return undefined;
}

// --- Main parser ---

export function parseAutomationEvents(
  automationItem: ItemView,
): AutomationEvent[] {
  const events: AutomationEvent[] = [];
  const children = automationItem.children ?? [];
  if (children.length === 0) return events;

  for (const child of children) {
    if (child.type !== "item" || child.domain_type !== "Event") continue;

    // Try new structured format first (has field/item children with Time/Value)
    const hasStructuredFields = (child.children ?? []).some(
      (c) => c.type === "field" && (c.name === "Time" || c.name === "Value"),
    );

    const event = hasStructuredFields
      ? parseNewFormat(child)
      : parseOldFormat(child);

    if (event) events.push(event);
  }

  return events;
}

export function computeAutomationRange(
  events: AutomationEvent[],
): AutomationRange {
  if (events.length === 0) {
    return { minValue: 0, maxValue: 1, minTime: 0, maxTime: 4 };
  }

  let minValue = Infinity;
  let maxValue = -Infinity;
  let minTime = Infinity;
  let maxTime = -Infinity;

  for (const e of events) {
    const v = e.oldValue ?? e.value;
    if (v < minValue) minValue = v;
    if (v > maxValue) maxValue = v;
    const t = e.oldTime ?? e.time;
    if (t < minTime) minTime = t;
    if (t > maxTime) maxTime = t;
  }

  const valuePad = Math.max(0.1, (maxValue - minValue) * 0.15);
  const timePad = Math.max(1, (maxTime - minTime) * 0.1);

  return {
    minValue: minValue - valuePad,
    maxValue: maxValue + valuePad,
    minTime: Math.max(0, minTime - timePad),
    maxTime: maxTime + timePad,
  };
}
