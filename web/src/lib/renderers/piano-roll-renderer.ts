import type { MidiNoteData, NoteRange } from "../../types";
import type { TimeSignature } from "../time-format";
import {
  getCSSColor,
  getChangeColor,
  computeGridInterval,
  drawRuler,
  type RulerMarker,
} from "../canvas-utils";
import { buildRectIndex, type HitRect } from "../hit-testing";
import { getNoteName, isBlackKey } from "../midi-notes";
import {
  quarterNoteToPosition,
  formatPosition,
} from "../time-format";

const ROW_HEIGHT = 14;
const RULER_HEIGHT = 24;
const VELOCITY_LANE_HEIGHT = 56;
const GRID_INTERVALS = [0.25, 0.5, 1, 2, 4];
const MIN_GRID_PX = 25;

export interface PianoRollRenderParams {
  notes: MidiNoteData[];
  noteRange: NoteRange;
  ppb: number;
  gridWidth: number;
  gridHeight: number;
  timeSignature: TimeSignature;
}

export function renderPianoRoll(
  ctx: CanvasRenderingContext2D,
  params: PianoRollRenderParams,
  vp: { scrollLeft: number; scrollTop: number; visibleWidth: number; visibleHeight: number },
): HitRect[] {
  const { notes, noteRange, ppb, gridWidth, gridHeight, timeSignature } = params;
  const totalHeight = RULER_HEIGHT + gridHeight + VELOCITY_LANE_HEIGHT;

  ctx.clearRect(0, 0, gridWidth, totalHeight);

  const hitRects: HitRect[] = [];

  // Determine visible pitch rows
  const firstVisibleRow = Math.max(0, Math.floor((vp.scrollTop - RULER_HEIGHT) / ROW_HEIGHT));
  const lastVisibleRow = Math.min(
    noteRange.maxPitch - noteRange.minPitch,
    Math.ceil((vp.scrollTop - RULER_HEIGHT + vp.visibleHeight) / ROW_HEIGHT),
  );

  // Compute grid interval
  const minor = computeGridInterval(ppb, GRID_INTERVALS, MIN_GRID_PX);
  const qnPerBar = (timeSignature.numer * 4) / timeSignature.denom;
  const gridStart = Math.floor(noteRange.minTime / minor) * minor;
  const gridEnd = Math.ceil(noteRange.maxTime / minor) * minor;

  const bgColor = getCSSColor("--bg-secondary");
  const borderColor = getCSSColor("--border");
  const gridLineColor = getCSSColor("--piano-grid-line");
  const gridBeatColor = getCSSColor("--piano-grid-beat");
  const whiteKeyColor = getCSSColor("--piano-white-key");
  const blackKeyColor = getCSSColor("--piano-black-key");

  // Draw grid rows (ruler area first)
  // Ruler
  const topMarkers: RulerMarker[] = [];
  for (let b = gridStart; b <= gridEnd + minor * 0.5; b += minor) {
    const isBar = Math.abs(((b % qnPerBar) + qnPerBar) % qnPerBar) < 1e-6;
    let label = "";
    if (isBar) {
      const p = quarterNoteToPosition(b, timeSignature);
      label = formatPosition(p.bar, p.beat, p.sixteenth);
    }
    topMarkers.push({ pos: (b - noteRange.minTime) * ppb, label, isMajor: isBar });
  }
  drawRuler(ctx, topMarkers, gridWidth, RULER_HEIGHT, false);

  // Grid rows
  const rowCount = noteRange.maxPitch - noteRange.minPitch + 1;

  for (let i = firstVisibleRow; i <= lastVisibleRow && i < rowCount; i++) {
    const pitch = noteRange.maxPitch - i;
    const y = RULER_HEIGHT + i * ROW_HEIGHT;

    // Row background
    ctx.fillStyle = isBlackKey(pitch) ? blackKeyColor : whiteKeyColor;
    ctx.fillRect(0, y, gridWidth, ROW_HEIGHT);

    // Row border
    ctx.strokeStyle = gridLineColor;
    ctx.lineWidth = 0.5;
    ctx.beginPath();
    ctx.moveTo(0, y + ROW_HEIGHT - 0.5);
    ctx.lineTo(gridWidth, y + ROW_HEIGHT - 0.5);
    ctx.stroke();

    // Beat lines within row
    const firstVisibleBeat = noteRange.minTime + vp.scrollLeft / ppb;
    const lastVisibleBeat = noteRange.minTime + (vp.scrollLeft + vp.visibleWidth) / ppb;

    for (let b = gridStart; b <= gridEnd + minor * 0.5; b += minor) {
      if (b < firstVisibleBeat - minor || b > lastVisibleBeat + minor) continue;
      const x = Math.round((b - noteRange.minTime) * ppb) + 0.5;
      const isBar = Math.abs(((b % qnPerBar) + qnPerBar) % qnPerBar) < 1e-6;
      ctx.strokeStyle = isBar ? gridBeatColor : gridLineColor;
      ctx.lineWidth = isBar ? 1.5 : 0.5;
      ctx.beginPath();
      ctx.moveTo(x, y);
      ctx.lineTo(x, y + ROW_HEIGHT);
      ctx.stroke();
    }
  }

  // Draw notes
  const firstVisibleBeat = noteRange.minTime + vp.scrollLeft / ppb;
  const lastVisibleBeat = noteRange.minTime + (vp.scrollLeft + vp.visibleWidth) / ppb;

  for (const note of notes) {
    const noteStart = note.time;
    const noteEnd = note.time + note.duration;
    if (noteEnd < firstVisibleBeat || noteStart > lastVisibleBeat) continue;

    const x = (note.time - noteRange.minTime) * ppb;
    const w = Math.max(2, note.duration * ppb);
    const pitchIdx = noteRange.maxPitch - note.pitch;
    if (pitchIdx < 0 || pitchIdx >= rowCount) continue;
    const y = RULER_HEIGHT + pitchIdx * ROW_HEIGHT;

    const color = getChangeColor(note.change);
    ctx.globalAlpha = note.change === "Unchanged" ? 0.5 : 1;

    ctx.fillStyle = color;
    ctx.beginPath();
    ctx.roundRect(x, y, w, ROW_HEIGHT - 1, 2);
    ctx.fill();

    ctx.strokeStyle = color;
    ctx.lineWidth = 1;
    ctx.beginPath();
    ctx.roundRect(x, y, w, ROW_HEIGHT - 1, 2);
    ctx.stroke();

    ctx.globalAlpha = 1;

    hitRects.push({
      x,
      y,
      w,
      h: ROW_HEIGHT - 1,
      data: { note },
    });
  }

  // Velocity lane
  const velY = RULER_HEIGHT + gridHeight;
  ctx.fillStyle = bgColor;
  ctx.fillRect(0, velY, gridWidth, VELOCITY_LANE_HEIGHT);

  // Velocity lane border
  ctx.strokeStyle = borderColor;
  ctx.lineWidth = 1;
  ctx.beginPath();
  ctx.moveTo(0, velY + 0.5);
  ctx.lineTo(gridWidth, velY + 0.5);
  ctx.stroke();

  for (const note of notes) {
    const x = (note.time - noteRange.minTime) * ppb;
    if (x + Math.max(3, note.duration * ppb) < vp.scrollLeft || x > vp.scrollLeft + vp.visibleWidth) continue;

    const barW = Math.max(3, note.duration * ppb);
    const barH = (note.velocity / 127) * (VELOCITY_LANE_HEIGHT - 4);
    const color = getChangeColor(note.change);

    ctx.globalAlpha = note.change === "Unchanged" ? 0.5 : 1;
    ctx.fillStyle = color;
    ctx.fillRect(x, velY + VELOCITY_LANE_HEIGHT - 2 - barH, barW, barH);
    ctx.globalAlpha = 1;
  }

  return hitRects;
}

export function getNoteTooltip(hitRects: HitRect[], worldX: number, worldY: number): string | null {
  const hit = buildRectIndex(hitRects).test(worldX, worldY);
  if (hit?.data && typeof hit.data === "object") {
    const d = hit.data as { note: MidiNoteData };
    const name = getNoteName(d.note.pitch);
    let text = `${name} v=${d.note.velocity}`;
    if (d.note.oldPitch !== undefined) {
      text += ` (was ${getNoteName(d.note.oldPitch)}`;
      if (d.note.oldVelocity !== undefined) text += ` v=${d.note.oldVelocity}`;
      text += ")";
    }
    return text;
  }
  return null;
}
