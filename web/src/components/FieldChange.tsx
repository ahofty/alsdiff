import type { FieldView } from "../types";

interface Props {
  field: FieldView;
}

export default function FieldChange(props: Props) {
  const f = () => props.field;

  return (
    <div class="field-change">
      <span class="field-name">{f().name}</span>
      {f().old_value != null && (
        <span class="old-val">{String(f().old_value)}</span>
      )}
      {f().old_value != null && f().new_value != null && (
        <span class="arrow">&rarr;</span>
      )}
      {f().new_value != null && (
        <span class="new-val">{String(f().new_value)}</span>
      )}
    </div>
  );
}
