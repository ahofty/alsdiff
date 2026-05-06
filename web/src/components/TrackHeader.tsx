import type { TrackData } from "../types";
import DiffIndicator from "./DiffIndicator";
import MixerStrip from "./Mixer";
import { extractMixer } from "../lib/diff-parser";
import { selectedTrackIdx, collapsedGroups, setCollapsedGroups } from "../stores/diff-store";

interface Props {
  track: TrackData;
  index: number;
  depth: number;
  isGroup: boolean;
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
  const expanded = () => !collapsedGroups().has(props.track.trackId);
  const mixer = () => extractMixer(props.track);

  const toggleGroup = (e: MouseEvent) => {
    e.stopPropagation();
    setCollapsedGroups((prev) => {
      const next = new Set(prev);
      if (next.has(props.track.trackId)) {
        next.delete(props.track.trackId);
      } else {
        next.add(props.track.trackId);
      }
      return next;
    });
  };

  return (
    <div
      class={`track-header ${isSelected() ? "selected" : ""}`}
      onClick={() => props.onSelect()}
      style={{ "padding-left": `${10 + props.depth * 20}px` }}
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
      {props.isGroup && (
        <span
          style={{
            cursor: "pointer",
            "font-size": "10px",
            "min-width": "14px",
            "user-select": "none",
            color: "var(--text-dim)",
          }}
          onClick={toggleGroup}
        >
          {expanded() ? "▼" : "▶"}
        </span>
      )}
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
      {mixer() && <MixerStrip mixer={mixer()!} />}
      {props.track.change !== "Unchanged" && (
        <DiffIndicator change={props.track.change} showLabel={false} />
      )}
    </div>
  );
}
