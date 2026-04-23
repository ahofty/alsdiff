import type { TrackData } from "../types";
import DiffIndicator from "./DiffIndicator";
import { selectedTrackIdx, selectedClipName } from "../stores/diff-store";

interface Props {
  track: TrackData;
  index: number;
  onSelect: () => void;
}

export default function TrackHeader(props: Props) {
  const isSelected = () => selectedTrackIdx() === props.index;
  const trackLabel = () => {
    const match = props.track.name.match(/:\s*(.+)/);
    return match ? match[1] : props.track.name;
  };
  const trackType = () => {
    if (props.track.name.startsWith("MidiTrack")) return "MIDI";
    if (props.track.name.startsWith("AudioTrack")) return "Audio";
    if (props.track.name.startsWith("Group")) return "Group";
    if (props.track.name.startsWith("MainTrack")) return "Master";
    return "";
  };

  return (
    <div
      class={`track-header ${isSelected() ? "selected" : ""}`}
      onClick={() => props.onSelect()}
    >
      <div
        class="change-indicator"
        style={{
          "background-color":
            props.track.change === "Added"
              ? "var(--color-added)"
              : props.track.change === "Removed"
                ? "var(--color-removed)"
                : props.track.change === "Modified"
                  ? "var(--color-modified)"
                  : "var(--color-unchanged)",
        }}
      />
      <span
        style={{
          color: "var(--text-dim)",
          "font-size": "10px",
          "min-width": "32px",
        }}
      >
        {trackType()}
      </span>
      <span class="track-name">{trackLabel()}</span>
      {props.track.change !== "Unchanged" && (
        <DiffIndicator change={props.track.change} showLabel={false} />
      )}
    </div>
  );
}
