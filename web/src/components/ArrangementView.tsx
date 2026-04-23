import { For } from "solid-js";
import { Show } from "solid-js";
import {
  tracks,
  pixelsPerBeat,
  setPixelsPerBeat,
  selectedTrackIdx,
  setSelectedTrackIdx,
  setSelectedClipName,
  setDetailTab,
} from "../stores/diff-store";
import { computeTimelineRange } from "../lib/diff-parser";
import TrackHeader from "./TrackHeader";
import TrackLane from "./TrackLane";

export default function ArrangementView() {
  const range = () => computeTimelineRange(tracks());
  const totalWidth = () => range().totalBeats * pixelsPerBeat() + 100;

  const selectTrack = (idx: number) => {
    setSelectedTrackIdx(idx);
    setSelectedClipName(null);
    setDetailTab("devices");
  };

  const selectClip = (trackIdx: number, clipName: string) => {
    setSelectedTrackIdx(trackIdx);
    setSelectedClipName(clipName);
    setDetailTab("clip");
  };

  const beatMarkers = () => {
    const markers: { pos: number; label: string; isBar: boolean }[] = [];
    const r = range();
    const startBeat = Math.floor(r.minStart);
    const endBeat = Math.ceil(r.maxEnd);
    for (let b = startBeat; b <= endBeat; b++) {
      markers.push({
        pos: (b - r.minStart) * pixelsPerBeat(),
        label: b % 4 === 0 ? String(b) : "",
        isBar: b % 4 === 0,
      });
    }
    return markers;
  };

  return (
    <div class="arrangement-view">
      <Show
        when={tracks().length > 0}
        fallback={
          <div
            style={{
              flex: 1,
              display: "flex",
              "align-items": "center",
              "justify-content": "center",
              color: "var(--text-dim)",
              padding: "32px",
            }}
          >
            No track changes detected between files.
          </div>
        }
      >
        <div
          style={{
            display: "flex",
            "align-items": "center",
            gap: "8px",
            padding: "4px 16px",
            background: "var(--bg-header)",
            "border-bottom": "1px solid var(--border)",
            "flex-shrink": 0,
          }}
        >
          <span style={{ color: "var(--text-secondary)", "font-size": "12px" }}>
            Zoom
          </span>
          <input
            type="range"
            min="3"
            max="300"
            value={pixelsPerBeat()}
            onInput={(e) => setPixelsPerBeat(Number(e.currentTarget.value))}
            style={{ width: "120px" }}
          />
          <span style={{ color: "var(--text-dim)", "font-size": "11px" }}>
            {pixelsPerBeat()}px/beat
          </span>
        </div>

        <div class="arrangement-content">
          <div class="track-headers">
            <div style={{ height: `${28}px` }} />
            <For each={tracks()}>
              {(track, idx) => (
                <TrackHeader
                  track={track}
                  index={idx()}
                  onSelect={() => selectTrack(idx())}
                />
              )}
            </For>
          </div>

          <div class="timeline-area">
            <div class="timeline-ruler" style={{ width: `${totalWidth()}px` }}>
              <For each={beatMarkers()}>
                {(m) => (
                  <div
                    style={{
                      position: "absolute",
                      left: `${m.pos}px`,
                      top: "0",
                      height: "100%",
                      "border-left": `1px solid ${m.isBar ? "var(--border-light)" : "var(--border)"}`,
                      display: "flex",
                      "align-items": "flex-end",
                      "padding-left": "3px",
                      "font-size": "10px",
                      color: "var(--text-dim)",
                    }}
                  >
                    {m.label}
                  </div>
                )}
              </For>
            </div>

            <div class="track-lanes" style={{ width: `${totalWidth()}px` }}>
              <For each={tracks()}>
                {(track, idx) => (
                  <TrackLane
                    track={track}
                    index={idx()}
                    offset={range().minStart}
                    totalWidth={totalWidth()}
                    onClipSelect={selectClip}
                  />
                )}
              </For>
            </div>
          </div>
        </div>
      </Show>
    </div>
  );
}
