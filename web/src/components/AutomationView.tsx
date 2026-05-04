import { For, Show, createEffect, onCleanup } from "solid-js";
import {
  automationZoomFactor,
  setAutomationZoomFactor,
  selectedAutomationIdx,
  setSelectedAutomationIdx,
  timeSignature,
} from "../stores/diff-store";
import {
  parseAutomationEvents,
  computeAutomationRange,
} from "../lib/automation-events";
import { quarterNoteToPosition, formatPosition } from "../lib/time-format";
import { zoomToSlider, sliderToZoom, handleWheelZoom } from "../lib/zoom";
import type { ItemView, AutomationEvent, AutomationRange, CurveControls } from "../types";

const GRID_HEIGHT = 200;
const RULER_HEIGHT = 24;
const VALUE_TICKS = 6;

const ZOOM_MIN = 0.2;
const ZOOM_MAX = 10;

const GRID_INTERVALS = [0.25, 0.5, 1, 2, 4];
const MIN_GRID_PX = 25;

interface Props {
  automationItems: ItemView[];
}

export default function AutomationView(props: Props) {
  let yAxisRef: HTMLDivElement | undefined;
  let scrollRef: HTMLDivElement | undefined;

  const currentAutomation = () =>
    props.automationItems[selectedAutomationIdx()] ?? null;

  const events = () => {
    const auto = currentAutomation();
    if (!auto) return [];
    return parseAutomationEvents(auto);
  };

  const range = () => computeAutomationRange(events());

  const pixelsPerBeat = () => {
    const base = 40;
    return base * automationZoomFactor();
  };

  const totalBeats = () => range().maxTime - range().minTime;
  const gridWidth = () => totalBeats() * pixelsPerBeat();

  const valueToY = (value: number, r: AutomationRange): number => {
    const fraction = (value - r.minValue) / (r.maxValue - r.minValue);
    return (1 - fraction) * GRID_HEIGHT;
  };

  const yTickValues = () => {
    const r = range();
    const step = (r.maxValue - r.minValue) / VALUE_TICKS;
    const ticks: { value: number; label: string; y: number }[] = [];
    for (let i = 0; i <= VALUE_TICKS; i++) {
      const v = r.minValue + step * i;
      ticks.push({
        value: v,
        label: v.toFixed(2),
        y: valueToY(v, r),
      });
    }
    return ticks;
  };

  // Scroll sync: y-axis <-> grid vertical
  createEffect(() => {
    events();
    const scroll = scrollRef;
    const yax = yAxisRef;
    if (!scroll || !yax) return;
    const handler = () => {
      yax.scrollTop = scroll.scrollTop;
    };
    scroll.addEventListener("scroll", handler);
    onCleanup(() => scroll.removeEventListener("scroll", handler));
  });

  const beatMarkers = () => {
    const markers: { pos: number; label: string; isMajor: boolean }[] = [];
    const r = range();
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

  const beatLines = () => {
    const lines: { pos: number; isBar: boolean }[] = [];
    const r = range();
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

  const eventStyle = (
    event: AutomationEvent,
    r: AutomationRange,
  ): Record<string, string> => {
    const ppb = pixelsPerBeat();
    const x = (event.time - r.minTime) * ppb;
    const y = valueToY(event.value, r);
    return {
      position: "absolute",
      left: `${x}px`,
      top: `${y}px`,
    };
  };

  const ghostStyle = (
    event: AutomationEvent,
    r: AutomationRange,
  ): Record<string, string> | null => {
    if (event.oldTime === undefined && event.oldValue === undefined) return null;
    const ppb = pixelsPerBeat();
    const x = ((event.oldTime ?? event.time) - r.minTime) * ppb;
    const y = valueToY(event.oldValue ?? event.value, r);
    return {
      position: "absolute",
      left: `${x}px`,
      top: `${y}px`,
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

  const tooltipText = (event: AutomationEvent): string => {
    let text = `t=${event.time.toFixed(2)} v=${event.value.toFixed(3)}`;
    if (event.oldTime !== undefined || event.oldValue !== undefined) {
      text += ` (was t=${(event.oldTime ?? event.time).toFixed(2)} v=${(event.oldValue ?? event.value).toFixed(3)})`;
    }
    return text;
  };

  function segmentPath(
    x0: number, y0: number, curve: CurveControls | undefined,
    x1: number, y1: number,
  ): string {
    if (curve) {
      const dt = x1 - x0;
      const dv = y1 - y0;
      const cp1x = x0 + curve.curve1X * dt;
      const cp1y = y0 + curve.curve1Y * dv;
      const cp2x = x1 + curve.curve2X * dt;
      const cp2y = y1 + curve.curve2Y * dv;
      return `C ${cp1x},${cp1y} ${cp2x},${cp2y} ${x1},${y1}`;
    }
    return `L ${x1},${y1}`;
  }

  function buildSegmentPaths(
    evts: AutomationEvent[], r: AutomationRange, ppb: number,
  ): { d: string; change: string }[] {
    const sorted = [...evts].sort((a, b) => a.time - b.time);
    if (sorted.length < 2) return [];
    const paths: { d: string; change: string }[] = [];
    for (let i = 0; i < sorted.length - 1; i++) {
      const cur = sorted[i];
      const nxt = sorted[i + 1];
      const x0 = (cur.time - r.minTime) * ppb;
      const y0 = valueToY(cur.value, r);
      const x1 = (nxt.time - r.minTime) * ppb;
      const y1 = valueToY(nxt.value, r);
      const seg = segmentPath(x0, y0, cur.curve, x1, y1);
      const change = changeClass(cur.change);
      paths.push({ d: `M ${x0},${y0} ${seg}`, change });
    }
    return paths;
  }

  function buildGhostPath(
    evts: AutomationEvent[], r: AutomationRange, ppb: number,
  ): string | null {
    const ghosts: { time: number; value: number }[] = [];
    for (const e of evts) {
      if (e.change === "Removed") {
        ghosts.push({ time: e.time, value: e.value });
      } else if (e.oldTime !== undefined || e.oldValue !== undefined) {
        ghosts.push({
          time: e.oldTime ?? e.time,
          value: e.oldValue ?? e.value,
        });
      }
    }
    if (ghosts.length < 2) return null;
    ghosts.sort((a, b) => a.time - b.time);
    const parts: string[] = [];
    const x0 = (ghosts[0].time - r.minTime) * ppb;
    const y0 = valueToY(ghosts[0].value, r);
    parts.push(`M ${x0},${y0}`);
    for (let i = 1; i < ghosts.length; i++) {
      const x1 = (ghosts[i].time - r.minTime) * ppb;
      const y1 = valueToY(ghosts[i].value, r);
      parts.push(`L ${x1},${y1}`);
    }
    return parts.join(" ");
  }

  return (
    <div class="automation-view">
      <div class="automation-controls">
        <span
          style={{ color: "var(--text-secondary)", "font-size": "12px" }}
        >
          Zoom
        </span>
        <input
          type="range"
          min="0"
          max="100"
          value={zoomToSlider(automationZoomFactor(), ZOOM_MIN, ZOOM_MAX)}
          onInput={(e) =>
            setAutomationZoomFactor(
              sliderToZoom(Number(e.currentTarget.value), ZOOM_MIN, ZOOM_MAX),
            )
          }
          style={{ width: "100px" }}
        />
        <select
          class="automation-selector"
          value={selectedAutomationIdx()}
          onChange={(e) =>
            setSelectedAutomationIdx(Number(e.currentTarget.value))
          }
        >
          <For each={props.automationItems}>
            {(item, idx) => <option value={idx()}>{item.name}</option>}
          </For>
        </select>
        <span style={{ color: "var(--text-dim)", "font-size": "11px" }}>
          {events().length} events
        </span>
      </div>

      <div class="automation-body">
        <div class="automation-y-axis" ref={yAxisRef}>
          <div
            style={{
              height: `${RULER_HEIGHT + GRID_HEIGHT}px`,
              position: "relative",
              "padding-top": `${RULER_HEIGHT}px`,
            }}
          >
            <For each={yTickValues()}>
              {(tick) => (
                <div
                  class="automation-y-tick"
                  style={{ top: `${tick.y}px` }}
                >
                  {tick.label}
                </div>
              )}
            </For>
          </div>
        </div>

        <div
          class="automation-scroll-area"
          ref={scrollRef}
          onWheel={(e) =>
            handleWheelZoom(e, automationZoomFactor(), setAutomationZoomFactor, ZOOM_MIN, ZOOM_MAX)
          }
        >
          <div style={{ width: `${gridWidth()}px` }}>
            <div
              class="automation-ruler"
              style={{ width: `${gridWidth()}px` }}
            >
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
              class="automation-grid-container"
              style={{
                width: `${gridWidth()}px`,
                height: `${GRID_HEIGHT}px`,
                position: "relative",
              }}
            >
              {/* Horizontal value grid lines */}
              <For each={yTickValues()}>
                {(tick) => (
                  <div
                    class="automation-grid-line"
                    style={{ top: `${tick.y}px` }}
                  />
                )}
              </For>

              {/* Vertical beat lines */}
              <For each={beatLines()}>
                {(line) => (
                  <div
                    class={`automation-beat-line ${line.isBar ? "bar" : ""}`}
                    style={{ left: `${line.pos}px` }}
                  />
                )}
              </For>

              {/* Envelope paths */}
              <svg
                class="automation-envelope-svg"
                viewBox={`0 0 ${gridWidth()} ${GRID_HEIGHT}`}
                preserveAspectRatio="none"
              >
                <Show when={buildGhostPath(events(), range(), pixelsPerBeat())}>
                  {(d) => (
                    <path class="automation-ghost-path" d={d()} />
                  )}
                </Show>
                <For each={buildSegmentPaths(events(), range(), pixelsPerBeat())}>
                  {(seg) => (
                    <path
                      class={`automation-envelope-path ${seg.change}`}
                      d={seg.d}
                    />
                  )}
                </For>
              </svg>

              {/* Event markers */}
              <For each={events()}>
                {(event) => (
                  <div
                    class={`automation-event-marker ${changeClass(event.change)}`}
                    style={eventStyle(event, range())}
                  >
                    {/* Ghost marker for modified events */}
                    <Show when={ghostStyle(event, range())}>
                      {(gs) => (
                        <div
                          class="automation-event-marker ghost"
                          style={gs()}
                        />
                      )}
                    </Show>
                    <span class="event-tooltip">{tooltipText(event)}</span>
                  </div>
                )}
              </For>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
