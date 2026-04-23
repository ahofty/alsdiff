import type { ClipData } from "../types";
import { pixelsPerBeat } from "../stores/diff-store";

interface Props {
  clip: ClipData;
  offset: number;
  onSelect: () => void;
  selected: boolean;
}

export default function ClipBlock(props: Props) {
  const left = () => (props.clip.startTime - props.offset) * pixelsPerBeat();
  const width = () =>
    Math.max(4, (props.clip.endTime - props.clip.startTime) * pixelsPerBeat());

  const clipName = () => {
    const match = props.clip.name.match(/:\s*(.+)/);
    return match ? match[1] : props.clip.name;
  };

  const changeClass = () => props.clip.change.toLowerCase();

  return (
    <div
      class={`clip-block ${changeClass()} ${props.clip.clipType} ${props.selected ? "selected" : ""}`}
      style={{
        left: `${left()}px`,
        width: `${width()}px`,
      }}
      onClick={(e) => {
        e.stopPropagation();
        props.onSelect();
      }}
      title={props.clip.name}
    >
      {width() > 40 && <span class="clip-name">{clipName()}</span>}
    </div>
  );
}
