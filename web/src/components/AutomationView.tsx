import { For, createEffect, onCleanup } from "solid-js";
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
import { zoomToSlider, sliderToZoom, handleWheelZoom } from "../lib/zoom";
import type { ItemView } from "../types";
import CanvasSurface from "./CanvasSurface";
import type { Viewport } from "./CanvasSurface";
import {
  renderAutomation,
  getEventTooltip,
  GRID_HEIGHT,
  RULER_HEIGHT,
  type AutomationRenderParams,
} from "../lib/renderers/automation-renderer";
import type { HitCircle } from "../lib/hit-testing";

const VALUE_TICKS = 6;

const ZOOM_MIN = 0.2;
const ZOOM_MAX = 10;

interface Props {
  automationItems: ItemView[];
}

export default function AutomationView(props: Props) {
  let yAxisRef: HTMLDivElement | undefined;
  let surfaceRef: HTMLDivElement | undefined;
  let hitCircles: HitCircle[] = [];

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
  const totalHeight = () => RULER_HEIGHT + GRID_HEIGHT;

  const yTickValues = () => {
    const r = range();
    const step = (r.maxValue - r.minValue) / VALUE_TICKS;
    const ticks: { value: number; label: string; y: number }[] = [];
    for (let i = 0; i <= VALUE_TICKS; i++) {
      const v = r.minValue + step * i;
      const fraction = (v - r.minValue) / (r.maxValue - r.minValue);
      ticks.push({
        value: v,
        label: v.toFixed(2),
        y: (1 - fraction) * GRID_HEIGHT,
      });
    }
    return ticks;
  };

  createEffect(() => {
    events();
    const surface = surfaceRef;
    const yax = yAxisRef;
    if (!surface || !yax) return;
    const handler = () => {
      yax.scrollTop = surface.scrollTop;
    };
    surface.addEventListener("scroll", handler);
    onCleanup(() => surface.removeEventListener("scroll", handler));
  });

  const render = (ctx: CanvasRenderingContext2D, vp: Viewport) => {
    const params: AutomationRenderParams = {
      events: events(),
      range: range(),
      ppb: pixelsPerBeat(),
      gridWidth: gridWidth(),
      timeSignature: timeSignature(),
    };
    hitCircles = renderAutomation(ctx, params, vp);
  };

  const handleHover = (worldX: number, worldY: number, tooltip: (text: string) => void) => {
    const text = getEventTooltip(hitCircles, worldX, worldY);
    tooltip(text ?? "");
  };

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
              height: `${totalHeight()}px`,
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
          ref={surfaceRef}
          onWheel={(e) =>
            handleWheelZoom(e, automationZoomFactor(), setAutomationZoomFactor, ZOOM_MIN, ZOOM_MAX)
          }
        >
          <CanvasSurface
            contentWidth={gridWidth}
            contentHeight={totalHeight}
            render={render}
            onHover={handleHover}
          />
        </div>
      </div>
    </div>
  );
}
