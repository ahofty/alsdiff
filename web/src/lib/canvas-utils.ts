const dpr = window.devicePixelRatio || 1;

export function setupCanvas(
  canvas: HTMLCanvasElement,
  width: number,
  height: number,
): CanvasRenderingContext2D {
  canvas.width = width * dpr;
  canvas.height = height * dpr;
  canvas.style.width = `${width}px`;
  canvas.style.height = `${height}px`;
  const ctx = canvas.getContext("2d");
  if (!ctx) throw new Error("Failed to get 2D rendering context");
  ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
  return ctx;
}

const cssColorCache = new Map<string, string>();

export function getCSSColor(varName: string): string {
  const cached = cssColorCache.get(varName);
  if (cached) return cached;
  const value = getComputedStyle(document.documentElement)
    .getPropertyValue(varName)
    .trim();
  cssColorCache.set(varName, value);
  return value;
}

export function clearCSSColorCache(): void {
  cssColorCache.clear();
}

export function computeGridInterval(
  ppb: number,
  intervals: number[],
  minPx: number,
): number {
  let minor = intervals[intervals.length - 1];
  for (const iv of intervals) {
    if (iv * ppb >= minPx) {
      minor = iv;
      break;
    }
  }
  return minor;
}

export interface RulerMarker {
  pos: number;
  label: string;
  isMajor: boolean;
}

export function drawRuler(
  ctx: CanvasRenderingContext2D,
  markers: RulerMarker[],
  width: number,
  height: number,
  alignBottom: boolean,
): void {
  ctx.fillStyle = getCSSColor("--bg-header");
  ctx.fillRect(0, 0, width, height);

  const borderColor = getCSSColor("--border");
  ctx.strokeStyle = borderColor;
  ctx.lineWidth = 1;
  if (alignBottom) {
    ctx.beginPath();
    ctx.moveTo(0, 0.5);
    ctx.lineTo(width, 0.5);
    ctx.stroke();
  } else {
    ctx.beginPath();
    ctx.moveTo(0, height - 0.5);
    ctx.lineTo(width, height - 0.5);
    ctx.stroke();
  }

  const textDim = getCSSColor("--text-dim");
  const borderLight = getCSSColor("--border-light");

  for (const m of markers) {
    const x = Math.round(m.pos) + 0.5;
    ctx.strokeStyle = m.isMajor ? borderLight : borderColor;
    ctx.lineWidth = m.isMajor ? 1.5 : 1;
    ctx.beginPath();
    ctx.moveTo(x, 0);
    ctx.lineTo(x, height);
    ctx.stroke();

    if (m.label) {
      ctx.fillStyle = textDim;
      ctx.font = "10px -apple-system, BlinkMacSystemFont, sans-serif";
      ctx.textBaseline = alignBottom ? "top" : "bottom";
      ctx.fillText(m.label, m.pos + 3, alignBottom ? 2 : height - 2);
    }
  }
}

export function getChangeColor(change: string): string {
  switch (change) {
    case "Added":
      return getCSSColor("--color-added");
    case "Removed":
      return getCSSColor("--color-removed");
    case "Modified":
      return getCSSColor("--color-modified");
    default:
      return getCSSColor("--color-unchanged");
  }
}
