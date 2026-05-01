import { createSignal } from "solid-js";
import type { DiffResult, TrackData } from "../types";
import { computeTimelineRange } from "../lib/diff-parser";

export const [diffResult, setDiffResult] = createSignal<DiffResult | null>(
  null,
);
export const [rawJson, setRawJson] = createSignal<string>("");
export const [tracks, setTracks] = createSignal<TrackData[]>([]);
export const [isLoading, setIsLoading] = createSignal(false);
export const [error, setError] = createSignal<string | null>(null);
export const [selectedTrackIdx, setSelectedTrackIdx] = createSignal<
  number | null
>(null);
export const [selectedClipName, setSelectedClipName] = createSignal<
  string | null
>(null);

export const [zoomFactor, setZoomFactor] = createSignal(1.0);
export const [timelineWidth, setTimelineWidth] = createSignal(800);

export const pixelsPerBeat = (): number => {
  const range = computeTimelineRange(tracks());
  if (range.totalBeats <= 0) return 1;
  return (timelineWidth() / range.totalBeats) * zoomFactor();
};

export const [detailHeight, setDetailHeight] = createSignal(300);
export const [detailCollapsed, setDetailCollapsed] = createSignal(false);
export const [detailTab, setDetailTab] = createSignal<"devices" | "clip">(
  "devices",
);

export function resetSelection(): void {
  setSelectedTrackIdx(null);
  setSelectedClipName(null);
  setDetailTab("devices");
  setDetailHeight(300);
  setDetailCollapsed(false);
}
