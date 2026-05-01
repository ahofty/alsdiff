import type { TrackData } from "../types";
import ClipBlock from "./ClipBlock";
import { selectedTrackIdx, selectedClipName } from "../stores/diff-store";
import { extractClips } from "../lib/diff-parser";
import { For } from "solid-js";

interface Props {
  track: TrackData;
  index: number;
  offset: number;
  totalWidth: number;
  onClipSelect: (trackIdx: number, clipName: string) => void;
}

export default function TrackLane(props: Props) {
  const clips = () => extractClips(props.track);
  const isSelected = () => selectedTrackIdx() === props.index;

  return (
    <div class={`track-lane ${isSelected() ? "selected" : ""}`}>
      <div style={{ width: `${props.totalWidth}px`, height: "100%", position: "relative" }}>
        <For each={clips()}>
          {(clip) => (
            <ClipBlock
              clip={clip}
              offset={props.offset}
              selected={selectedClipName() === clip.name}
              onSelect={() => props.onClipSelect(props.index, clip.name)}
            />
          )}
        </For>
      </div>
    </div>
  );
}
