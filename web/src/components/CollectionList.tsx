import type { CollectionView, ViewNode } from "../types";
import { For } from "solid-js";

interface Props {
  collection: CollectionView;
}

export function ViewNodeRow(props: { node: ViewNode; depth?: number }) {
  const depth = () => props.depth ?? 0;

  return (
    <div style={{ "padding-left": `${depth() * 12}px` }}>
      {props.node.type === "field" && (
        <div class="collection-item" style={{ "border-left-color": "var(--border)" }}>
          <span style={{ color: "var(--text-secondary)" }}>
            {props.node.name}:
          </span>{" "}
          {props.node.old_value != null && (
            <span style={{ color: "var(--color-removed)", "text-decoration": "line-through" }}>
              {String(props.node.old_value)}
            </span>
          )}
          {props.node.old_value != null && props.node.new_value != null && " → "}
          {props.node.new_value != null && (
            <span style={{ color: "var(--color-added)" }}>
              {String(props.node.new_value)}
            </span>
          )}
        </div>
      )}
      {props.node.type === "item" && (
        <div
          class={`collection-item ${props.node.change.toLowerCase()}`}
          style={{ "margin-bottom": "2px" }}
        >
          <strong>{props.node.name}</strong>
          <Show when={props.node.children && props.node.children.length > 0}>
            <div>
              <For each={props.node.children ?? []}>
                {(child) => <ViewNodeRow node={child} depth={depth() + 1} />}
              </For>
            </div>
          </Show>
        </div>
      )}
      {props.node.type === "collection" && (
        <div>
          <div class="collection-header">{props.node.name}</div>
          <For each={props.node.items}>
            {(item) => <ViewNodeRow node={item} depth={depth() + 1} />}
          </For>
        </div>
      )}
    </div>
  );
}

import { Show } from "solid-js";

export default function CollectionList(props: Props) {
  return (
    <div class="collection-list">
      <div class="collection-header">
        {props.collection.name}
        <Show when={props.collection.total != null}>
          {" "}
          ({props.collection.displayed}/{props.collection.total})
        </Show>
      </div>
      <For each={props.collection.items}>
        {(item) => <ViewNodeRow node={item} />}
      </For>
    </div>
  );
}
