export interface HitRect {
  x: number;
  y: number;
  w: number;
  h: number;
  data: unknown;
}

export interface HitCircle {
  cx: number;
  cy: number;
  r: number;
  data: unknown;
}

export function buildRectIndex(items: HitRect[]): {
  test: (x: number, y: number) => HitRect | null;
} {
  return {
    test(x: number, y: number): HitRect | null {
      for (let i = items.length - 1; i >= 0; i--) {
        const r = items[i];
        if (x >= r.x && x <= r.x + r.w && y >= r.y && y <= r.y + r.h) {
          return r;
        }
      }
      return null;
    },
  };
}

export function buildCircleIndex(items: HitCircle[]): {
  test: (x: number, y: number) => HitCircle | null;
} {
  return {
    test(x: number, y: number): HitCircle | null {
      for (let i = items.length - 1; i >= 0; i--) {
        const c = items[i];
        const dx = x - c.cx;
        const dy = y - c.cy;
        if (dx * dx + dy * dy <= c.r * c.r) {
          return c;
        }
      }
      return null;
    },
  };
}
