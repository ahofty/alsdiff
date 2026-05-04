import { For, createEffect, onCleanup } from "solid-js";
import {
  pianoRollZoomFactor,
  setPianoRollZoomFactor,
  timeSignature,
} from "../stores/diff-store";
import {
  extractMidiNotes,
  computeNoteRange,
  getNoteName,
  isBlackKey,
} from "../lib/midi-notes";
import { quarterNoteToPosition, formatPosition } from "../lib/time-format";
import { zoomToSlider, sliderToZoom, handleWheelZoom } from "../lib/zoom";
import type { ViewNode, MidiNoteData, NoteRange } from "../types";

const ROW_HEIGHT = 14;
const RULER_HEIGHT = 24;
const VELOCITY_LANE_HEIGHT = 56;

const ZOOM_MIN = 0.2;
const ZOOM_MAX = 10;

const GRID_INTERVALS = [0.25, 0.5, 1, 2, 4];
const MIN_GRID_PX = 25;

interface Props {
  clipChildren: ViewNode[];
}

export default function PianoRollView(props: Props) {
  let keyboardRef: HTMLDivElement | undefined;
  let scrollRef: HTMLDivElement | undefined;

  const notes = () => extractMidiNotes(props.clipChildren);
  const noteRange = () => computeNoteRange(notes());

  const pixelsPerBeat = () => {
    const base = 40;
    return base * pianoRollZoomFactor();
  };

  const totalBeats = () => noteRange().maxTime - noteRange().minTime;
  const gridWidth = () => totalBeats() * pixelsPerBeat();
  const rowCount = () => noteRange().maxPitch - noteRange().minPitch + 1;
  const gridHeight = () => rowCount() * ROW_HEIGHT;

  const pitches = () => {
    const r = noteRange();
    const result: number[] = [];
    for (let p = r.maxPitch; p >= r.minPitch; p--) {
      result.push(p);
    }
    return result;
  };

  // Scroll sync: keyboard <-> grid vertical
  createEffect(() => {
    notes(); // reactive dep
    const scroll = scrollRef;
    const kb = keyboardRef;
    if (!scroll || !kb) return;
    const handler = () => {
      kb.scrollTop = scroll.scrollTop;
    };
    scroll.addEventListener("scroll", handler);
    onCleanup(() => scroll.removeEventListener("scroll", handler));
  });

  const beatMarkers = () => {
    const markers: { pos: number; label: string; isMajor: boolean }[] = [];
    const r = noteRange();
    const ppb = pixelsPerBeat();
    const ts = timeSignature();

    let minor = GRID_INTERVALS[GRID_INTERVALS.length - 1];
    for (const iv of GRID_INTERVALS) {
      if (iv * ppb >= MIN_GRID_PX) {
        minor = iv;
        break;
      }
    }
    const qnPerBar = (ts.numer * 4) / ts.denom;

    const start = Math.floor(r.minTime / minor) * minor;
    const end = Math.ceil(r.maxTime / minor) * minor;

    for (let b = start; b <= end + minor * 0.5; b += minor) {
      const isMajor =
        Math.abs(((b % qnPerBar) + qnPerBar) % qnPerBar) < 1e-6;
      let label = "";
      if (isMajor) {
        const p = quarterNoteToPosition(b, ts);
        label = formatPosition(p.bar, p.beat, p.sixteenth);
      }
      markers.push({
        pos: (b - r.minTime) * ppb,
        label,
        isMajor,
      });
    }
    return markers;
  };

  const gridLines = () => {
    const lines: { pos: number; isBar: boolean }[] = [];
    const r = noteRange();
    const ppb = pixelsPerBeat();
    const ts = timeSignature();
    const qnPerBar = (ts.numer * 4) / ts.denom;

    let minor = GRID_INTERVALS[GRID_INTERVALS.length - 1];
    for (const iv of GRID_INTERVALS) {
      if (iv * ppb >= MIN_GRID_PX) {
        minor = iv;
        break;
      }
    }

    const start = Math.floor(r.minTime / minor) * minor;
    const end = Math.ceil(r.maxTime / minor) * minor;

    for (let b = start; b <= end + minor * 0.5; b += minor) {
      const isBar =
        Math.abs(((b % qnPerBar) + qnPerBar) % qnPerBar) < 1e-6;
      lines.push({ pos: (b - r.minTime) * ppb, isBar });
    }
    return lines;
  };

  const noteStyle = (
    note: MidiNoteData,
    range: NoteRange,
  ): Record<string, string> => {
    const ppb = pixelsPerBeat();
    const left = (note.time - range.minTime) * ppb;
    const width = Math.max(2, note.duration * ppb);
    const top = (range.maxPitch - note.pitch) * ROW_HEIGHT;
    return {
      position: "absolute",
      left: `${left}px`,
      top: `${top}px`,
      width: `${width}px`,
      height: `${ROW_HEIGHT - 1}px`,
    };
  };

  const velocityStyle = (
    note: MidiNoteData,
    range: NoteRange,
  ): Record<string, string> => {
    const ppb = pixelsPerBeat();
    const left = (note.time - range.minTime) * ppb;
    const height = (note.velocity / 127) * (VELOCITY_LANE_HEIGHT - 4);
    return {
      position: "absolute",
      left: `${left}px`,
      bottom: "2px",
      width: `${Math.max(3, note.duration * ppb)}px`,
      height: `${height}px`,
    };
  };

  const changeClass = (change: string): string => {
    switch (change) {
      case "Added":
        return "added";
      case "Removed":
        return "removed";
      case "Modified":
        return "modified";
      default:
        return "unchanged";
    }
  };

  const tooltipText = (note: MidiNoteData): string => {
    const name = getNoteName(note.pitch);
    let text = `${name} v=${note.velocity}`;
    if (note.oldPitch !== undefined) {
      text += ` (was ${getNoteName(note.oldPitch)}`;
      if (note.oldVelocity !== undefined) text += ` v=${note.oldVelocity}`;
      text += ")";
    }
    return text;
  };

  return (
    <div class="piano-roll">
      <div class="piano-roll-controls">
        <span
          style={{ color: "var(--text-secondary)", "font-size": "12px" }}
        >
          Zoom
        </span>
        <input
          type="range"
          min="0"
          max="100"
          value={zoomToSlider(pianoRollZoomFactor(), ZOOM_MIN, ZOOM_MAX)}
          onInput={(e) =>
            setPianoRollZoomFactor(sliderToZoom(Number(e.currentTarget.value), ZOOM_MIN, ZOOM_MAX))
          }
          style={{ width: "100px" }}
        />
        <span style={{ color: "var(--text-dim)", "font-size": "11px" }}>
          {notes().length} notes
        </span>
      </div>

      <div class="piano-roll-body">
        <div class="piano-keyboard" ref={keyboardRef}>
          <div
            class="piano-keyboard-inner"
            style={{
              height: `${RULER_HEIGHT + gridHeight() + VELOCITY_LANE_HEIGHT}px`,
              "padding-top": `${RULER_HEIGHT}px`,
            }}
          >
            <For each={pitches()}>
              {(pitch) => (
                <div
                  class={`piano-key-row ${isBlackKey(pitch) ? "black" : "white"}`}
                >
                  {getNoteName(pitch)}
                </div>
              )}
            </For>
          </div>
        </div>

        <div
          class="piano-scroll-area"
          ref={scrollRef}
          onWheel={(e) =>
            handleWheelZoom(e, pianoRollZoomFactor(), setPianoRollZoomFactor, ZOOM_MIN, ZOOM_MAX)
          }
        >
          <div style={{ width: `${gridWidth()}px` }}>
            <div class="piano-ruler" style={{ width: `${gridWidth()}px` }}>
              <For each={beatMarkers()}>
                {(m) => (
                  <div
                    style={{
                      position: "absolute",
                      left: `${m.pos}px`,
                      top: "0",
                      height: "100%",
                      "border-left": `${m.isMajor ? "1.5px" : "1px"} solid ${m.isMajor ? "var(--border-light)" : "var(--border)"}`,
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

            <div
              class="piano-grid-container"
              style={{
                width: `${gridWidth()}px`,
                height: `${gridHeight()}px`,
                position: "relative",
              }}
            >
              <For each={pitches()}>
                {(pitch) => (
                  <div
                    class={`piano-grid-row ${isBlackKey(pitch) ? "black" : "white"}`}
                  >
                    <For each={gridLines()}>
                      {(line) => (
                        <div
                          class={`beat-line ${line.isBar ? "bar" : ""}`}
                          style={{ left: `${line.pos}px` }}
                        />
                      )}
                    </For>
                  </div>
                )}
              </For>

              <For each={notes()}>
                {(note) => (
                  <div
                    class={`piano-note ${changeClass(note.change)}`}
                    style={noteStyle(note, noteRange())}
                  >
                    <span class="note-tooltip">{tooltipText(note)}</span>
                  </div>
                )}
              </For>
            </div>

            <div
              class="piano-velocity-lane"
              style={{
                width: `${gridWidth()}px`,
                position: "sticky",
                bottom: "0",
              }}
            >
              <For each={notes()}>
                {(note) => (
                  <div
                    class={`piano-velocity-bar ${changeClass(note.change)}`}
                    style={velocityStyle(note, noteRange())}
                  />
                )}
              </For>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
