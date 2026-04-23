import type { ChangeType } from "../types";

interface Props {
  change: ChangeType;
  showLabel?: boolean;
}

export default function DiffIndicator(props: Props) {
  const label = () => props.change;

  return (
    <span class={`diff-indicator ${props.change.toLowerCase()}`}>
      <span class="dot" />
      {props.showLabel !== false && <span>{label()}</span>}
    </span>
  );
}
