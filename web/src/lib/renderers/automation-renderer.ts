import type { AutomationEvent, AutomationRange, CurveControls } from "../../types";
import type { TimeSignature } from "../time-format";
import {
  getCSSColor,
  getChangeColor,
  computeGridInterval,
  drawRuler,
  type RulerMarker,
} from "../canvas-utils";
import { buildCircleIndex, type HitCircle } from "../hit-testing";
import {
  quarterNoteToPosition,
  formatPosition,
} from "../time-format";

const GRID_HEIGHT = 200;
const RULER_HEIGHT = 24;
const VALUE_TICKS = 6;
const GRID_INTERVALS = [0.25, 0.5, 1, 2, 4];
const MIN_GRID_PX = 25;
const MARKER_RADIUS = 5;
const HIT_RADIUS = 8;

export interface AutomationRenderParams {
  events: AutomationEvent[];
  range: AutomationRange;
  ppb: number;
  gridWidth: number;
  timeSignature: TimeSignature;
}

function valueToY(value: number, r: AutomationRange): number {
  const fraction = (value - r.minValue) / (r.maxValue - r.minValue);
  return (1 - fraction) * GRID_HEIGHT;
}

export function renderAutomation(
  ctx: CanvasRenderingContext2D,
  params: AutomationRenderParams,
  vp: { scrollLeft: number; scrollTop: number; visibleWidth: number; visibleHeight: number },
): HitCircle[] {
  const { events, range, ppb, gridWidth, timeSignature } = params;
  const totalHeight = RULER_HEIGHT + GRID_HEIGHT;

  ctx.clearRect(0, 0, gridWidth, totalHeight);

  const hitCircles: HitCircle[] = [];
  const gridLineColor = getCSSColor("--piano-grid-line");
  const gridBeatColor = getCSSColor("--piano-grid-beat");

  // Ruler
  const minor = computeGridInterval(ppb, GRID_INTERVALS, MIN_GRID_PX);
  const qnPerBar = (timeSignature.numer * 4) / timeSignature.denom;
  const gridStart = Math.floor(range.minTime / minor) * minor;
  const gridEnd = Math.ceil(range.maxTime / minor) * minor;

  const topMarkers: RulerMarker[] = [];
  for (let b = gridStart; b <= gridEnd + minor * 0.5; b += minor) {
    const isBar = Math.abs(((b % qnPerBar) + qnPerBar) % qnPerBar) < 1e-6;
    let label = "";
    if (isBar) {
      const p = quarterNoteToPosition(b, timeSignature);
      label = formatPosition(p.bar, p.beat, p.sixteenth);
    }
    topMarkers.push({ pos: (b - range.minTime) * ppb, label, isMajor: isBar });
  }
  drawRuler(ctx, topMarkers, gridWidth, RULER_HEIGHT, false);

  // Grid area
  const gridTop = RULER_HEIGHT;

  // Horizontal value grid lines
  const step = (range.maxValue - range.minValue) / VALUE_TICKS;
  for (let i = 0; i <= VALUE_TICKS; i++) {
    const v = range.minValue + step * i;
    const y = gridTop + valueToY(v, range);
    ctx.strokeStyle = gridLineColor;
    ctx.lineWidth = 0.5;
    ctx.beginPath();
    ctx.moveTo(0, y + 0.5);
    ctx.lineTo(gridWidth, y + 0.5);
    ctx.stroke();
  }

  // Vertical beat lines
  const firstVisibleBeat = range.minTime + vp.scrollLeft / ppb;
  const lastVisibleBeat = range.minTime + (vp.scrollLeft + vp.visibleWidth) / ppb;

  for (let b = gridStart; b <= gridEnd + minor * 0.5; b += minor) {
    if (b < firstVisibleBeat - minor || b > lastVisibleBeat + minor) continue;
    const x = Math.round((b - range.minTime) * ppb) + 0.5;
    const isBar = Math.abs(((b % qnPerBar) + qnPerBar) % qnPerBar) < 1e-6;
    ctx.strokeStyle = isBar ? gridBeatColor : gridLineColor;
    ctx.lineWidth = isBar ? 1.5 : 0.5;
    ctx.beginPath();
    ctx.moveTo(x, gridTop);
    ctx.lineTo(x, gridTop + GRID_HEIGHT);
    ctx.stroke();
  }

  // Ghost path (old values for modified events)
  const ghosts: { time: number; value: number }[] = [];
  for (const e of events) {
    if (e.change === "Removed") {
      ghosts.push({ time: e.time, value: e.value });
    } else if (e.oldTime !== undefined || e.oldValue !== undefined) {
      ghosts.push({
        time: e.oldTime ?? e.time,
        value: e.oldValue ?? e.value,
      });
    }
  }

  if (ghosts.length >= 2) {
    ghosts.sort((a, b) => a.time - b.time);
    ctx.strokeStyle = getCSSColor("--color-removed");
    ctx.lineWidth = 1.5;
    ctx.globalAlpha = 0.5;
    ctx.setLineDash([6, 3]);
    ctx.beginPath();
    ctx.moveTo(
      (ghosts[0].time - range.minTime) * ppb,
      gridTop + valueToY(ghosts[0].value, range),
    );
    for (let i = 1; i < ghosts.length; i++) {
      ctx.lineTo(
        (ghosts[i].time - range.minTime) * ppb,
        gridTop + valueToY(ghosts[i].value, range),
      );
    }
    ctx.stroke();
    ctx.setLineDash([]);
    ctx.globalAlpha = 1;
  }

  // Envelope segments
  const sorted = [...events].sort((a, b) => a.time - b.time);

  function drawSegment(
    x0: number, y0: number, curve: CurveControls | undefined,
    x1: number, y1: number, color: string,
  ) {
    ctx.strokeStyle = color;
    ctx.lineWidth = 2;
    ctx.beginPath();
    ctx.moveTo(x0, y0);
    if (curve) {
      const dt = x1 - x0;
      const dv = y1 - y0;
      ctx.bezierCurveTo(
        x0 + curve.curve1X * dt,
        y0 + curve.curve1Y * dv,
        x1 + curve.curve2X * dt,
        y1 + curve.curve2Y * dv,
        x1,
        y1,
      );
    } else {
      ctx.lineTo(x1, y1);
    }
    ctx.stroke();
  }

  for (let i = 0; i < sorted.length - 1; i++) {
    const cur = sorted[i];
    const nxt = sorted[i + 1];
    const x0 = (cur.time - range.minTime) * ppb;
    const y0 = gridTop + valueToY(cur.value, range);
    const x1 = (nxt.time - range.minTime) * ppb;
    const y1 = gridTop + valueToY(nxt.value, range);
    const color = getChangeColor(cur.change);

    ctx.globalAlpha = cur.change === "Unchanged" ? 0.5 : 1;
    drawSegment(x0, y0, cur.curve, x1, y1, color);
    ctx.globalAlpha = 1;
  }

  // Event markers
  for (const event of events) {
    const x = (event.time - range.minTime) * ppb;
    const y = gridTop + valueToY(event.value, range);
    const color = getChangeColor(event.change);

    // Ghost marker for modified events
    if (event.oldTime !== undefined || event.oldValue !== undefined) {
      const gx = ((event.oldTime ?? event.time) - range.minTime) * ppb;
      const gy = gridTop + valueToY(event.oldValue ?? event.value, range);
      ctx.globalAlpha = 0.6;
      ctx.strokeStyle = getCSSColor("--color-modified");
      ctx.lineWidth = 1;
      ctx.setLineDash([3, 2]);
      ctx.beginPath();
      ctx.arc(gx, gy, MARKER_RADIUS, 0, Math.PI * 2);
      ctx.stroke();
      ctx.setLineDash([]);
      ctx.globalAlpha = 1;
    }

    ctx.globalAlpha = event.change === "Unchanged" ? 0.5 : 1;
    ctx.fillStyle = color;
    ctx.strokeStyle = color;
    ctx.lineWidth = 1;
    ctx.beginPath();
    ctx.arc(x, y, MARKER_RADIUS, 0, Math.PI * 2);
    ctx.fill();
    ctx.stroke();
    ctx.globalAlpha = 1;

    hitCircles.push({
      cx: x,
      cy: y,
      r: HIT_RADIUS,
      data: { event },
    });
  }

  return hitCircles;
}

export function getEventTooltip(
  hitCircles: HitCircle[],
  worldX: number,
  worldY: number,
): string | null {
  const hit = buildCircleIndex(hitCircles).test(worldX, worldY);
  if (hit?.data && typeof hit.data === "object") {
    const d = hit.data as { event: AutomationEvent };
    const e = d.event;
    let text = `t=${e.time.toFixed(2)} v=${e.value.toFixed(3)}`;
    if (e.oldTime !== undefined || e.oldValue !== undefined) {
      text += ` (was t=${(e.oldTime ?? e.time).toFixed(2)} v=${(e.oldValue ?? e.value).toFixed(3)})`;
    }
    return text;
  }
  return null;
}

export { GRID_HEIGHT, RULER_HEIGHT };
