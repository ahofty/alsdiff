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
import { zoomToSlider, sliderToZoom, handleWheelZoom } from "../lib/zoom";
import type { ViewNode } from "../types";
import CanvasSurface from "./CanvasSurface";
import type { Viewport } from "./CanvasSurface";
import {
  renderPianoRoll,
  getNoteTooltip,
  type PianoRollRenderParams,
} from "../lib/renderers/piano-roll-renderer";
import type { HitRect } from "../lib/hit-testing";

const ROW_HEIGHT = 14;
const RULER_HEIGHT = 24;
const VELOCITY_LANE_HEIGHT = 56;

const ZOOM_MIN = 0.2;
const ZOOM_MAX = 10;

interface Props {
  clipChildren: ViewNode[];
}

export default function PianoRollView(props: Props) {
  let keyboardRef: HTMLDivElement | undefined;
  let surfaceRef: HTMLDivElement | undefined;
  let hitRects: HitRect[] = [];

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
  const totalHeight = () => RULER_HEIGHT + gridHeight() + VELOCITY_LANE_HEIGHT;

  const pitches = () => {
    const r = noteRange();
    const result: number[] = [];
    for (let p = r.maxPitch; p >= r.minPitch; p--) {
      result.push(p);
    }
    return result;
  };

  createEffect(() => {
    notes();
    const surface = surfaceRef;
    const kb = keyboardRef;
    if (!surface || !kb) return;
    const handler = () => {
      kb.scrollTop = surface.scrollTop;
    };
    surface.addEventListener("scroll", handler);
    onCleanup(() => surface.removeEventListener("scroll", handler));
  });

  const render = (ctx: CanvasRenderingContext2D, vp: Viewport) => {
    const params: PianoRollRenderParams = {
      notes: notes(),
      noteRange: noteRange(),
      ppb: pixelsPerBeat(),
      gridWidth: gridWidth(),
      gridHeight: gridHeight(),
      timeSignature: timeSignature(),
    };
    hitRects = renderPianoRoll(ctx, params, vp);
  };

  const handleHover = (worldX: number, worldY: number, tooltip: (text: string) => void) => {
    const text = getNoteTooltip(hitRects, worldX, worldY);
    tooltip(text ?? "");
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
              height: `${totalHeight()}px`,
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
          ref={surfaceRef}
          onWheel={(e) =>
            handleWheelZoom(e, pianoRollZoomFactor(), setPianoRollZoomFactor, ZOOM_MIN, ZOOM_MAX)
          }
        >
          <CanvasSurface
            contentWidth={gridWidth}
            contentHeight={totalHeight}
            render={render}
            onHover={handleHover}
            testId="piano-roll-canvas"
          />
        </div>
      </div>
    </div>
  );
}
