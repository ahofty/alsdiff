import { Show, For } from "solid-js";
import {
  tracks,
  selectedTrackIdx,
  selectedClipName,
  detailTab,
  setDetailTab,
  detailHeight,
} from "../stores/diff-store";
import {
  extractDevices,
  extractClips,
} from "../lib/diff-parser";
import DeviceChain from "./DeviceChain";
import ClipDetail from "./ClipDetail";
import type { ClipData } from "../types";

export default function DetailView() {
  const selectedTrack = () => {
    const idx = selectedTrackIdx();
    if (idx == null) return null;
    return tracks()[idx] ?? null;
  };

  const selectedClip = (): ClipData | null => {
    const track = selectedTrack();
    if (!track) return null;
    const name = selectedClipName();
    if (!name) return null;
    return extractClips(track).find((c) => c.name === name) ?? null;
  };

  const devices = () => {
    const track = selectedTrack();
    return track ? extractDevices(track) : [];
  };

  const hasDevices = () => devices().length > 0;
  const hasClip = () => selectedClip() !== null;

  return (
    <div class="detail-pane" style={{ height: `${detailHeight()}px` }}>
      <Show
        when={selectedTrack()}
        fallback={
          <div
            style={{
              display: "flex",
              "align-items": "center",
              "justify-content": "center",
              height: "100%",
              color: "var(--text-dim)",
            }}
          >
            Select a track to view details
          </div>
        }
      >
        <div class="detail-tabs">
          <Show when={hasDevices()}>
            <div
              class={`detail-tab ${detailTab() === "devices" ? "active" : ""}`}
              onClick={() => setDetailTab("devices")}
            >
              Devices ({devices().length})
            </div>
          </Show>
          <Show when={hasClip()}>
            <div
              class={`detail-tab ${detailTab() === "clip" ? "active" : ""}`}
              onClick={() => setDetailTab("clip")}
            >
              Clip: {selectedClip()?.name}
            </div>
          </Show>
        </div>
        <div class="detail-content">
          <Show when={detailTab() === "devices" && hasDevices()}>
            <DeviceChain devices={devices()} />
          </Show>
          <Show when={detailTab() === "clip" && hasClip()}>
            <ClipDetail clipChildren={selectedClip()?.children ?? []} />
          </Show>
        </div>
      </Show>
    </div>
  );
}
