import {
  detailHeight,
  setDetailHeight,
  detailCollapsed,
  setDetailCollapsed,
} from "../stores/diff-store";

export default function ResizeHandle() {
  const MIN_HEIGHT = 120;
  let savedHeight = 300;
  let dragging = false;
  let startY = 0;
  let startHeight = 0;

  const getMaxHeight = () => window.innerHeight - 200;

  const clamp = (v: number, min: number, max: number) =>
    Math.max(min, Math.min(max, v));

  const onPointerDown = (e: PointerEvent) => {
    e.preventDefault();
    dragging = true;
    startY = e.clientY;
    startHeight = detailHeight();
    (e.currentTarget as HTMLElement).setPointerCapture(e.pointerId);
    (e.currentTarget as HTMLElement).classList.add("active");
  };

  const onPointerMove = (e: PointerEvent) => {
    if (!dragging) return;
    const delta = startY - e.clientY;
    const newH = clamp(startHeight + delta, MIN_HEIGHT, getMaxHeight());
    setDetailHeight(newH);
    setDetailCollapsed(false);
  };

  const onPointerUp = (e: PointerEvent) => {
    if (!dragging) return;
    dragging = false;
    (e.currentTarget as HTMLElement).classList.remove("active");
  };

  const onDoubleClick = () => {
    if (detailCollapsed()) {
      setDetailHeight(savedHeight);
      setDetailCollapsed(false);
    } else {
      savedHeight = detailHeight();
      setDetailHeight(MIN_HEIGHT);
      setDetailCollapsed(true);
    }
  };

  return (
    <div
      class="resize-handle"
      onPointerDown={onPointerDown}
      onPointerMove={onPointerMove}
      onPointerUp={onPointerUp}
      onDblClick={onDoubleClick}
    />
  );
}
