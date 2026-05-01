import { createSignal } from "solid-js";
import type { DiffResult, TrackData } from "../types";

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
export const [pixelsPerBeat, setPixelsPerBeat] = createSignal(30);
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
