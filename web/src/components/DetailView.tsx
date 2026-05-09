import { Show } from "solid-js";
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
  extractAutomations,
} from "../lib/diff-parser";
import { extractMidiNotes } from "../lib/midi-notes";
import DeviceChain from "./DeviceChain";
import ClipDetail from "./ClipDetail";
import PianoRollView from "./PianoRollView";
import AutomationView from "./AutomationView";
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
  const hasNotes = () => {
    const clip = selectedClip();
    if (!clip || clip.clipType !== "midi") return false;
    return extractMidiNotes(clip.children).length > 0;
  };

  const automations = () => {
    const track = selectedTrack();
    return track ? extractAutomations(track) : [];
  };
  const hasAutomations = () => automations().length > 0;

  return (
    <div class="detail-pane" data-testid="detail-pane" style={{ height: `${detailHeight()}px` }}>
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
              data-testid="detail-tab-devices"
              onClick={() => setDetailTab("devices")}
            >
              Devices ({devices().length})
            </div>
          </Show>
          <Show when={hasClip()}>
            <div
              class={`detail-tab ${detailTab() === "clip" ? "active" : ""}`}
              data-testid="detail-tab-clip"
              onClick={() => setDetailTab("clip")}
            >
              Clip: {selectedClip()?.name}
            </div>
          </Show>
          <Show when={hasNotes()}>
            <div
              class={`detail-tab ${detailTab() === "pianoRoll" ? "active" : ""}`}
              data-testid="detail-tab-pianoRoll"
              onClick={() => setDetailTab("pianoRoll")}
            >
              Piano Roll
            </div>
          </Show>
          <Show when={hasAutomations()}>
            <div
              class={`detail-tab ${detailTab() === "automation" ? "active" : ""}`}
              data-testid="detail-tab-automation"
              onClick={() => setDetailTab("automation")}
            >
              Automation ({automations().length})
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
          <Show when={detailTab() === "pianoRoll" && hasNotes()}>
            <PianoRollView clipChildren={selectedClip()?.children ?? []} />
          </Show>
          <Show when={detailTab() === "automation" && hasAutomations()}>
            <AutomationView automationItems={automations()} />
          </Show>
        </div>
      </Show>
    </div>
  );
}
