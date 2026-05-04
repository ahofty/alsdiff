import { For, Show, createEffect, onMount, onCleanup } from "solid-js";
import {
  tracks,
  pixelsPerBeat,
  zoomFactor,
  setZoomFactor,
  setTimelineWidth,
  setSelectedTrackIdx,
  setSelectedClipName,
  setDetailTab,
  tempo,
  timeSignature,
} from "../stores/diff-store";
import { computeTimelineRange } from "../lib/diff-parser";
import {
  quarterNoteToPosition,
  formatPosition,
  quarterNoteToRealtime,
  formatRealtime,
} from "../lib/time-format";
import TrackHeader from "./TrackHeader";
import TrackLane from "./TrackLane";
import { extractClips, extractAutomations } from "../lib/diff-parser";
import { extractMidiNotes } from "../lib/midi-notes";
import { zoomToSlider, sliderToZoom, handleWheelZoom } from "../lib/zoom";

const ZOOM_MIN = 0.1;
const ZOOM_MAX = 20;

export default function ArrangementView() {
  let headersRef: HTMLDivElement | undefined;
  let timelineRef: HTMLDivElement | undefined;

  const range = () => computeTimelineRange(tracks());
  const totalWidth = () => {
    const ppb = pixelsPerBeat();
    return range().totalBeats * ppb;
  };

  const measureWidth = () => {
    const el = document.querySelector(".timeline-area");
    if (el) setTimelineWidth(el.clientWidth);
  };

  createEffect(() => {
    tracks();
    requestAnimationFrame(measureWidth);
  });

  createEffect(() => {
    if (tracks().length > 0) setZoomFactor(1.0);
  });

  createEffect(() => {
    tracks(); // reactive dependency — re-run when tracks load and refs become available
    const timeline = timelineRef;
    const headers = headersRef;
    if (!timeline || !headers) return;
    const handler = () => { headers.scrollTop = timeline.scrollTop; };
    timeline.addEventListener("scroll", handler);
    onCleanup(() => timeline.removeEventListener("scroll", handler));
  });

  onMount(() => {
    window.addEventListener("resize", measureWidth);
  });

  onCleanup(() => {
    window.removeEventListener("resize", measureWidth);
  });

  const selectTrack = (idx: number) => {
    setSelectedTrackIdx(idx);
    setSelectedClipName(null);
    setDetailTab("devices");
  };

  const selectClip = (trackIdx: number, clipName: string) => {
    setSelectedTrackIdx(trackIdx);
    setSelectedClipName(clipName);
    const track = tracks()[trackIdx];
    if (track) {
      const clip = extractClips(track).find((c) => c.name === clipName);
      if (clip && clip.clipType === "midi" && extractMidiNotes(clip.children).length > 0) {
        setDetailTab("pianoRoll");
        return;
      }
      if (extractAutomations(track).length > 0) {
        setDetailTab("automation");
        return;
      }
    }
    setDetailTab("clip");
  };

  // Ordered finest-to-coarsest; walk until interval * ppb >= threshold
  const GRID_INTERVALS = [0.5, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512];
  const MIN_GRID_PX = 30;

  const beatMarkers = () => {
    const markers: { pos: number; label: string; isMajor: boolean }[] = [];
    const r = range();
    const ppb = pixelsPerBeat();
    const ts = timeSignature();

    let minor = GRID_INTERVALS[GRID_INTERVALS.length - 1];
    for (const iv of GRID_INTERVALS) {
      if (iv * ppb >= MIN_GRID_PX) { minor = iv; break; }
    }
    const major = minor * 2;

    const start = Math.floor(r.minStart / minor) * minor;
    const end = Math.ceil(r.maxEnd / minor) * minor;

    for (let b = start; b <= end + minor * 0.5; b += minor) {
      const isMajor = Math.abs(((b % major) + major) % major) < 1e-6;
      let label = "";
      if (isMajor) {
        const p = quarterNoteToPosition(b, ts);
        label = formatPosition(p.bar, p.beat, p.sixteenth);
      }
      markers.push({ pos: (b - r.minStart) * ppb, label, isMajor });
    }
    return markers;
  };

  const realtimeMarkers = () => {
    const markers: { pos: number; label: string; isMajor: boolean }[] = [];
    const r = range();
    const ppb = pixelsPerBeat();
    const bpm = tempo();

    let minor = GRID_INTERVALS[GRID_INTERVALS.length - 1];
    for (const iv of GRID_INTERVALS) {
      if (iv * ppb >= MIN_GRID_PX) { minor = iv; break; }
    }
    const major = minor * 2;

    const start = Math.floor(r.minStart / minor) * minor;
    const end = Math.ceil(r.maxEnd / minor) * minor;

    for (let b = start; b <= end + minor * 0.5; b += minor) {
      const isMajor = Math.abs(((b % major) + major) % major) < 1e-6;
      let label = "";
      if (isMajor) {
        const rt = quarterNoteToRealtime(b, bpm);
        label = formatRealtime(rt.min, rt.sec, rt.ms);
      }
      markers.push({ pos: (b - r.minStart) * ppb, label, isMajor });
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
            min="0"
            max="100"
            value={zoomToSlider(zoomFactor(), ZOOM_MIN, ZOOM_MAX)}
            onInput={(e) => setZoomFactor(sliderToZoom(Number(e.currentTarget.value), ZOOM_MIN, ZOOM_MAX))}
            style={{ width: "120px" }}
          />
          <span style={{ color: "var(--text-dim)", "font-size": "11px" }}>
            {zoomFactor().toFixed(1)}x
          </span>
        </div>

        <div class="arrangement-content">
          <div class="track-headers" ref={headersRef}>
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

          <div
            class="timeline-area"
            ref={timelineRef}
            onWheel={(e) =>
              handleWheelZoom(e, zoomFactor(), setZoomFactor, ZOOM_MIN, ZOOM_MAX)
            }
          >
            <div class="timeline-ruler" style={{ width: `${totalWidth()}px` }}>
              <For each={beatMarkers()}>
                {(m) => (
                  <div
                    style={{
                      position: "absolute",
                      left: `${m.pos}px`,
                      top: "0",
                      height: "100%",
                      "border-left": `${m.isMajor ? "2px" : "1px"} solid ${m.isMajor ? "var(--border-light)" : "var(--border)"}`,
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

            <div class="timeline-ruler-bottom" style={{ width: `${totalWidth()}px` }}>
              <For each={realtimeMarkers()}>
                {(m) => (
                  <div
                    style={{
                      position: "absolute",
                      left: `${m.pos}px`,
                      top: "0",
                      height: "100%",
                      "border-left": `${m.isMajor ? "2px" : "1px"} solid ${m.isMajor ? "var(--border-light)" : "var(--border)"}`,
                      display: "flex",
                      "align-items": "flex-start",
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
          </div>
        </div>
      </Show>
    </div>
  );
}
