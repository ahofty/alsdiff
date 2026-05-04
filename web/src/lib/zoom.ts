export function zoomToSlider(
  zoom: number,
  zoomMin: number,
  zoomMax: number,
): number {
  const logRatio = zoomMax / zoomMin;
  return Math.round(
    (Math.log(zoom / zoomMin) / Math.log(logRatio)) * 100,
  );
}

export function sliderToZoom(
  val: number,
  zoomMin: number,
  zoomMax: number,
): number {
  const logRatio = zoomMax / zoomMin;
  const t = val / 100;
  return zoomMin * Math.pow(logRatio, t);
}

export function handleWheelZoom(
  e: WheelEvent,
  currentZoom: number,
  setZoom: (z: number) => void,
  zoomMin: number,
  zoomMax: number,
): boolean {
  if (!e.ctrlKey && !e.metaKey) return false;
  e.preventDefault();
  const factor = Math.pow(1.15, -e.deltaY * 0.01);
  const next = Math.min(zoomMax, Math.max(zoomMin, currentZoom * factor));
  setZoom(next);
  return true;
}
