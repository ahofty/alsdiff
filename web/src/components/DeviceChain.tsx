import type { ItemView } from "../types";
import DeviceCard from "./DeviceCard";
import { For } from "solid-js";

interface Props {
  devices: ItemView[];
}

export default function DeviceChain(props: Props) {
  return (
    <div class="device-chain" data-testid="device-chain">
      <For each={props.devices}>
        {(device) => <DeviceCard device={device} />}
      </For>
      <Show when={props.devices.length === 0}>
        <div style={{ color: "var(--text-dim)", padding: "8px" }}>
          No device changes
        </div>
      </Show>
    </div>
  );
}

import { Show } from "solid-js";
