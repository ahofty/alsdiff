import type { TrackData, ClipData, TimelineRange } from "../../types";
import {
  getCSSColor,
  getChangeColor,
  computeGridInterval,
} from "../canvas-utils";
import { buildRectIndex, type HitRect } from "../hit-testing";

const TRACK_HEIGHT = 48;
const GRID_INTERVALS = [0.5, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512];
const MIN_GRID_PX = 30;

export interface ArrangementRenderParams {
  tracks: TrackData[];
  range: TimelineRange;
  ppb: number;
  selectedTrackIdx: number | null;
  selectedClipName: string | null;
  totalWidth: number;
  extractClips: (track: TrackData) => ClipData[];
}

export function renderArrangement(
  ctx: CanvasRenderingContext2D,
  params: ArrangementRenderParams,
  vp: { scrollLeft: number; visibleWidth: number },
): HitRect[] {
  const { tracks, range, ppb, selectedTrackIdx, selectedClipName, totalWidth, extractClips } = params;
  const trackCount = tracks.length;
  const tracksHeight = trackCount * TRACK_HEIGHT;

  ctx.clearRect(0, 0, totalWidth, tracksHeight);

  const hitRects: HitRect[] = [];

  for (let i = 0; i < trackCount; i++) {
    const y = i * TRACK_HEIGHT;
    const isSelected = selectedTrackIdx === i;

    ctx.fillStyle = isSelected
      ? getCSSColor("--bg-selected")
      : i % 2 === 0
        ? getCSSColor("--bg-secondary")
        : getCSSColor("--bg-primary");
    ctx.fillRect(0, y, totalWidth, TRACK_HEIGHT);

    ctx.strokeStyle = getCSSColor("--border");
    ctx.lineWidth = 1;
    ctx.beginPath();
    ctx.moveTo(0, y + TRACK_HEIGHT - 0.5);
    ctx.lineTo(totalWidth, y + TRACK_HEIGHT - 0.5);
    ctx.stroke();

    const clips = extractClips(tracks[i]);
    const firstVisibleBeat = range.minStart + vp.scrollLeft / ppb;
    const lastVisibleBeat = range.minStart + (vp.scrollLeft + vp.visibleWidth) / ppb;

    for (const clip of clips) {
      if (clip.endTime < firstVisibleBeat || clip.startTime > lastVisibleBeat) continue;

      const clipX = (clip.startTime - range.minStart) * ppb;
      const clipW = Math.max(4, (clip.endTime - clip.startTime) * ppb);
      const clipY = y + 4;
      const clipH = TRACK_HEIGHT - 8;

      const color = getChangeColor(clip.change);
      const clipName = clip.name.match(/:\s*(.+)/)?.[1] ?? clip.name;
      const isClipSelected = selectedTrackIdx === i && selectedClipName === clip.name;

      if (clip.change === "Removed") {
        ctx.strokeStyle = color;
        ctx.lineWidth = 2;
        ctx.setLineDash([4, 3]);
        ctx.strokeRect(clipX, clipY, clipW, clipH);
        ctx.setLineDash([]);
      } else if (clip.change === "Modified") {
        const halfW = clipW / 2;
        ctx.fillStyle = getCSSColor("--color-removed");
        ctx.fillRect(clipX, clipY, halfW, clipH);
        ctx.fillStyle = getCSSColor("--color-added");
        ctx.fillRect(clipX + halfW, clipY, halfW, clipH);
      } else {
        ctx.fillStyle = color;
        ctx.globalAlpha = clip.change === "Unchanged" ? 0.6 : 1;
        ctx.beginPath();
        ctx.roundRect(clipX, clipY, clipW, clipH, 3);
        ctx.fill();
        ctx.globalAlpha = 1;
      }

      if (clip.clipType === "midi") {
        ctx.fillStyle = getCSSColor("--color-clip-midi");
      } else {
        ctx.fillStyle = getCSSColor("--color-clip-audio");
      }
      ctx.fillRect(clipX, clipY, 3, clipH);

      if (clipW > 40) {
        ctx.fillStyle = clip.change === "Removed"
          ? getCSSColor("--color-removed")
          : getCSSColor("--text-primary");
        ctx.font = "11px -apple-system, BlinkMacSystemFont, sans-serif";
        ctx.textBaseline = "middle";
        const maxTextW = clipW - 12;
        ctx.save();
        ctx.beginPath();
        ctx.rect(clipX + 6, clipY, maxTextW, clipH);
        ctx.clip();
        ctx.fillText(clipName, clipX + 6, clipY + clipH / 2);
        ctx.restore();
      }

      if (isClipSelected) {
        ctx.strokeStyle = getCSSColor("--text-primary");
        ctx.lineWidth = 1.5;
        ctx.strokeRect(clipX, clipY, clipW, clipH);
      }

      hitRects.push({
        x: clipX,
        y: clipY,
        w: clipW,
        h: clipH,
        data: { trackIdx: i, clipName: clip.name, clip },
      });
    }
  }

  // Grid lines (vertical)
  const minor = computeGridInterval(ppb, GRID_INTERVALS, MIN_GRID_PX);
  const major = minor * 2;
  const gridStart = Math.floor(range.minStart / minor) * minor;
  const gridEnd = Math.ceil(range.maxEnd / minor) * minor;

  for (let b = gridStart; b <= gridEnd + minor * 0.5; b += minor) {
    const x = Math.round((b - range.minStart) * ppb) + 0.5;
    if (x < vp.scrollLeft - 1 || x > vp.scrollLeft + vp.visibleWidth + 1) continue;
    const isMajor = Math.abs(((b % major) + major) % major) < 1e-6;
    ctx.strokeStyle = isMajor ? getCSSColor("--border-light") : getCSSColor("--border");
    ctx.lineWidth = isMajor ? 1 : 0.5;
    ctx.beginPath();
    ctx.moveTo(x, 0);
    ctx.lineTo(x, tracksHeight);
    ctx.stroke();
  }

  return hitRects;
}

export function hitTestTracks(
  hitRects: HitRect[],
  worldX: number,
  worldY: number,
): { trackIdx: number; clipName: string } | null {
  const idx = buildRectIndex(hitRects).test(worldX, worldY);
  if (idx?.data && typeof idx.data === "object") {
    const d = idx.data as { trackIdx: number; clipName: string };
    return { trackIdx: d.trackIdx, clipName: d.clipName };
  }
  return null;
}

export function getTrackIndexFromY(worldY: number): number | null {
  const trackIdx = Math.floor(worldY / TRACK_HEIGHT);
  return trackIdx >= 0 ? trackIdx : null;
}
