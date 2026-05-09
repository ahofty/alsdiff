import type { ViewNode, ItemView, FieldView } from "../types";
import { For, Show } from "solid-js";
import FieldChange from "./FieldChange";
import CollectionList from "./CollectionList";

interface Props {
  clipChildren: ViewNode[];
}

function getFields(children: ViewNode[]): FieldView[] {
  return children.filter((c): c is FieldView => c.type === "field");
}

function findChild(children: ViewNode[], name: string): ItemView | undefined {
  return children.find(
    (c): c is ItemView => c.type === "item" && c.name === name,
  );
}

function findCollection(
  children: ViewNode[],
  name: string,
): ViewNode | undefined {
  return children.find(
    (c) => c.type === "collection" && c.name === name,
  );
}

export default function ClipDetail(props: Props) {
  const fields = () => getFields(props.clipChildren);
  const loop = () => findChild(props.clipChildren, "Loop");
  const sig = () => findChild(props.clipChildren, "TimeSignature");
  const sampleRef = () => findChild(props.clipChildren, "SampleRef");
  const fade = () => findChild(props.clipChildren, "Fade");
  const notes = () => findCollection(props.clipChildren, "Notes");

  return (
    <div class="clip-detail" data-testid="clip-detail">
      <div class="clip-detail-section">
        <h4>Properties</h4>
        <For each={fields()}>
          {(f) => <FieldChange field={f} />}
        </For>
      </div>

      <Show when={loop()}>
        {(l) => (
          <div class="clip-detail-section">
            <h4>Loop</h4>
            <For each={getFields(l().children ?? [])}>
              {(f) => <FieldChange field={f} />}
            </For>
          </div>
        )}
      </Show>

      <Show when={sig()}>
        {(s) => (
          <div class="clip-detail-section">
            <h4>Time Signature</h4>
            <For each={getFields(s().children ?? [])}>
              {(f) => <FieldChange field={f} />}
            </For>
          </div>
        )}
      </Show>

      <Show when={sampleRef()}>
        {(sr) => (
          <div class="clip-detail-section">
            <h4>Sample Reference</h4>
            <For each={getFields(sr().children ?? [])}>
              {(f) => <FieldChange field={f} />}
            </For>
          </div>
        )}
      </Show>

      <Show when={fade()}>
        {(f) => (
          <div class="clip-detail-section">
            <h4>Fade</h4>
            <For each={getFields(f().children ?? [])}>
              {(field) => <FieldChange field={field} />}
            </For>
          </div>
        )}
      </Show>

      <Show when={notes()}>
        {(n) =>
          n().type === "collection" ? (
            <div class="clip-detail-section" style={{ "grid-column": "1 / -1" }}>
              <CollectionList collection={n() as any} />
            </div>
          ) : null
        }
      </Show>
    </div>
  );
}
