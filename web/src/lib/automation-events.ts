import type {
  ItemView,
  AutomationEvent,
  AutomationRange,
  CurveControls,
} from "../types";

const RE_ADDED_REMOVED = /Time=([\d.]+),\s*Value=([\d.]+)/;
const RE_MODIFIED_TIME = /Time:\s*([\d.]+)->([\d.]+)/;
const RE_MODIFIED_VALUE = /Value:\s*([\d.]+)->([\d.]+)/;
const RE_CURVE = /Curve1=\(([\d.]+),([\d.]+)\)\s*Curve2=\(([\d.]+),([\d.]+)\)/;

function clamp01(v: number): number {
  return Math.min(1, Math.max(0, v));
}

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

export function parseAutomationEvents(
  automationItem: ItemView,
): AutomationEvent[] {
  const events: AutomationEvent[] = [];
  const children = automationItem.children ?? [];
  if (children.length === 0) return events;

  for (const child of children) {
    if (child.type !== "item" || child.domain_type !== "Event") continue;
    const name = child.name;

    if (child.change === "Added" || child.change === "Removed") {
      const m = name.match(RE_ADDED_REMOVED);
      if (m) {
        events.push({
          time: parseFloat(m[1]),
          value: parseFloat(m[2]),
          change: child.change,
          curve: parseCurve(name),
        });
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
        events.push({
          time,
          value: vm ? parseFloat(vm[2]) : 0,
          change: "Modified",
          oldTime: tm ? parseFloat(tm[1]) : undefined,
          oldValue: vm ? parseFloat(vm[1]) : undefined,
          curve: parseCurve(name),
        });
      }
    }
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
