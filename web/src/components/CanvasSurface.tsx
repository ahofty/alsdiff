import {
  createEffect,
  onCleanup,
  onMount,
  Show,
} from "solid-js";
import { setupCanvas, clearCSSColorCache } from "../lib/canvas-utils";

export interface Viewport {
  scrollLeft: number;
  scrollTop: number;
  visibleWidth: number;
  visibleHeight: number;
}

interface CanvasSurfaceProps {
  contentWidth: () => number;
  contentHeight: () => number;
  render: (ctx: CanvasRenderingContext2D, vp: Viewport) => void;
  onClick?: (worldX: number, worldY: number) => void;
  onHover?: (worldX: number, worldY: number, tooltip: (text: string) => void) => void;
  class?: string;
  testId?: string;
}

export default function CanvasSurface(props: CanvasSurfaceProps) {
  let canvasRef: HTMLCanvasElement | undefined;
  let scrollRef: HTMLDivElement | undefined;
  let tooltipRef: HTMLDivElement | undefined;
  let ctx: CanvasRenderingContext2D | undefined;
  let rafId = 0;

  const draw = () => {
    const canvas = canvasRef;
    const scroll = scrollRef;
    if (!canvas || !scroll) return;

    const w = props.contentWidth();
    const h = props.contentHeight();
    if (w <= 0 || h <= 0) return;

    ctx = setupCanvas(canvas, w, h);

    const vp: Viewport = {
      scrollLeft: scroll.scrollLeft,
      scrollTop: scroll.scrollTop,
      visibleWidth: scroll.clientWidth,
      visibleHeight: scroll.clientHeight,
    };

    props.render(ctx, vp);
  };

  const scheduleDraw = () => {
    cancelAnimationFrame(rafId);
    rafId = requestAnimationFrame(draw);
  };

  createEffect(() => {
    props.contentWidth();
    props.contentHeight();
    clearCSSColorCache();
    scheduleDraw();
  });

  onMount(() => {
    const scroll = scrollRef;
    if (!scroll) return;
    scroll.addEventListener("scroll", scheduleDraw, { passive: true });
    onCleanup(() => scroll.removeEventListener("scroll", scheduleDraw));
  });

  onMount(() => {
    const handleResize = () => scheduleDraw();
    window.addEventListener("resize", handleResize);
    onCleanup(() => window.removeEventListener("resize", handleResize));
  });

  onCleanup(() => cancelAnimationFrame(rafId));

  const handleClick = (e: MouseEvent) => {
    if (!props.onClick || !scrollRef || !canvasRef) return;
    const rect = canvasRef.getBoundingClientRect();
    const x = e.clientX - rect.left;
    const y = e.clientY - rect.top;
    props.onClick(x, y);
  };

  const handleMouseMove = (e: MouseEvent) => {
    if (!props.onHover || !scrollRef || !canvasRef) return;
    const rect = canvasRef.getBoundingClientRect();
    const x = e.clientX - rect.left;
    const y = e.clientY - rect.top;
    props.onHover(x, y, (text: string) => {
      if (!tooltipRef) return;
      tooltipRef.textContent = text;
      tooltipRef.style.display = text ? "block" : "none";
      tooltipRef.style.left = `${e.clientX - (scrollRef?.getBoundingClientRect().left ?? 0) + 12}px`;
      tooltipRef.style.top = `${e.clientY - (scrollRef?.getBoundingClientRect().top ?? 0) - 20}px`;
    });
  };

  const handleMouseLeave = () => {
    if (tooltipRef) tooltipRef.style.display = "none";
  };

  return (
    <div
      class={props.class ?? "canvas-surface-scroll"}
      ref={scrollRef}
      onClick={handleClick}
      onMouseMove={handleMouseMove}
      onMouseLeave={handleMouseLeave}
      style={{ position: "relative", overflow: "auto", flex: "1" }}
    >
      <canvas
        ref={canvasRef}
        data-testid={props.testId}
        style={{ display: "block" }}
      />
      <Show when={props.onHover}>
        <div
          ref={tooltipRef}
          style={{
            display: "none",
            position: "absolute",
            "background-color": "var(--bg-header)",
            border: "1px solid var(--border)",
            padding: "3px 6px",
            "font-size": "10px",
            "white-space": "nowrap",
            "border-radius": "3px",
            color: "var(--text-primary)",
            "pointer-events": "none",
            "z-index": "10",
          }}
        />
      </Show>
    </div>
  );
}
