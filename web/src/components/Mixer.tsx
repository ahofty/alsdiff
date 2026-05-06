import { Show } from "solid-js";
import type { ItemView, ViewNode, ChangeType } from "../types";

interface MixerProps {
  mixer: ItemView;
}

function isItem(node: ViewNode): node is ItemView {
  return node.type === "item";
}

function isField(node: ViewNode) {
  return node.type === "field";
}

function findMixerParam(children: ViewNode[], paramName: string): ItemView | undefined {
  return children.find(
    (c): c is ItemView => isItem(c) && c.name === paramName,
  );
}

function getParamValue(children: ViewNode[]): { oldVal?: number; newVal?: number; change: ChangeType } | null {
  const valueField = children.find(c => isField(c) && c.name === "Value");
  if (!valueField || valueField.type !== "field") return null;
  return {
    oldVal: valueField.old_value as number | undefined,
    newVal: valueField.new_value as number | undefined,
    change: valueField.change,
  };
}

// --- Formatting helpers ---

function toDb(v: number): number {
  if (v <= 0) return -60;
  return Math.max(-60, 20 * Math.log10(v));
}

function fmtDb(v: number): string {
  return `${toDb(v).toFixed(1)}dB`;
}

function fmtPan(v: number): string {
  if (v === 0) return "C";
  const n = Math.round(Math.abs(v) * 50);
  return v < 0 ? `${n}L` : `${n}R`;
}

function volumeDiffText(oldVal: number | undefined, newVal: number | undefined, change: ChangeType): string {
  if (change === "Added" && newVal !== undefined) return fmtDb(newVal);
  if (change === "Removed" && oldVal !== undefined) return fmtDb(oldVal);
  if (oldVal !== undefined && newVal !== undefined && oldVal !== newVal)
    return `${fmtDb(oldVal)}→${fmtDb(newVal)}`;
  const v = newVal ?? oldVal;
  return v !== undefined ? fmtDb(v) : "";
}

function panDiffText(oldVal: number | undefined, newVal: number | undefined, change: ChangeType): string {
  if (change === "Added" && newVal !== undefined) return fmtPan(newVal);
  if (change === "Removed" && oldVal !== undefined) return fmtPan(oldVal);
  if (oldVal !== undefined && newVal !== undefined && oldVal !== newVal)
    return `${fmtPan(oldVal)}→${fmtPan(newVal)}`;
  const v = newVal ?? oldVal;
  return v !== undefined ? fmtPan(v) : "";
}

// --- Horizontal bar slider ---

interface SliderProps {
  value: NonNullable<ReturnType<typeof getParamValue>>;
  formatLabel: (old: number | undefined, new_: number | undefined, change: ChangeType) => string;
  valueToPercent: (v: number) => number;
}

function Slider(props: SliderProps) {
  const { oldVal, newVal, change } = props.value;
  const width = 90;

  const oldPct = () => oldVal !== undefined ? props.valueToPercent(oldVal) * width : undefined;
  const newPct = () => newVal !== undefined ? props.valueToPercent(newVal) * width : undefined;
  const label = () => props.formatLabel(oldVal, newVal, change);

  const fillColor = () => {
    if (change === "Added") return "rgba(76, 175, 80, 0.35)";
    return "rgba(255, 179, 0, 0.25)";
  };

  return (
    <div class="mixer-bar" style={{ width: `${width}px` }}>
      <Show when={change === "Removed" && oldPct() !== undefined}>
        <div
          class="mixer-bar-fill ghost"
          style={{
            width: `${oldPct()}px`,
            "background-color": "var(--color-removed)",
            border: "1px dashed var(--color-removed)",
          }}
        />
      </Show>
      <Show when={change === "Added" && newPct() !== undefined}>
        <div
          class="mixer-bar-fill"
          style={{ width: `${newPct()}px`, "background-color": fillColor() }}
        />
      </Show>
      <Show when={change === "Modified"}>
        <Show when={oldPct() !== undefined}>
          <div
            class="mixer-bar-fill ghost"
            style={{ width: `${oldPct()}px`, "background-color": "var(--color-removed)" }}
          />
        </Show>
        <Show when={newPct() !== undefined}>
          <div
            class="mixer-bar-fill"
            style={{ width: `${newPct()}px`, "background-color": fillColor() }}
          />
        </Show>
      </Show>
      <Show when={change === "Unchanged" && (newPct() ?? oldPct()) !== undefined}>
        <div
          class="mixer-bar-fill"
          style={{
            width: `${(newPct() ?? oldPct())!}px`,
            "background-color": "rgba(255, 255, 255, 0.15)",
          }}
        />
      </Show>
      <div class="mixer-bar-label">{label()}</div>
    </div>
  );
}

function MixerToggle(props: { label: string; value: ReturnType<typeof getParamValue> }) {
  if (!props.value) return null;
  const { oldVal, newVal, change } = props.value;

  const isOn = () => (newVal ?? oldVal) !== 0;
  const glowColor = () => {
    if (change === "Added") return "var(--color-added)";
    if (change === "Removed") return "var(--color-removed)";
    return "var(--color-modified)";
  };

  return (
    <div
      class={`mixer-toggle ${isOn() ? "active" : ""} ${change !== "Unchanged" ? "changed" : ""}`}
      style={change !== "Unchanged" ? { "box-shadow": `0 0 4px ${glowColor()}` } : {}}
    >
      {props.label}
    </div>
  );
}

export default function MixerStrip(props: MixerProps) {
  const children = () => props.mixer.children ?? [];
  const hasChanges = () => children().length > 0;

  const volume = () => {
    const vol = findMixerParam(children(), "Volume");
    return vol?.children ? getParamValue(vol.children) : null;
  };

  const pan = () => {
    const p = findMixerParam(children(), "Pan");
    return p?.children ? getParamValue(p.children) : null;
  };

  const mute = () => {
    const m = findMixerParam(children(), "Mute");
    return m?.children ? getParamValue(m.children) : null;
  };

  const solo = () => {
    const s = findMixerParam(children(), "Solo");
    return s?.children ? getParamValue(s.children) : null;
  };

  return (
    <Show when={hasChanges()}>
      <div class="mixer-strip">
        <div class="mixer-toggles">
          <Show when={mute()}>
            {(m) => <MixerToggle label="M" value={m()} />}
          </Show>
          <Show when={solo()}>
            {(s) => <MixerToggle label="S" value={s()} />}
          </Show>
        </div>
        <div class="mixer-sliders">
          <Show when={pan()}>
            {(p) => <Slider value={p()!} formatLabel={panDiffText} valueToPercent={v => Math.max(0, Math.min(1, (v + 1) / 2))} />}
          </Show>
          <Show when={volume()}>
            {(v) => <Slider value={v()!} formatLabel={volumeDiffText} valueToPercent={v => Math.max(0, Math.min(1, v))} />}
          </Show>
        </div>
      </div>
    </Show>
  );
}
