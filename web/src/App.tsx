import { Show, createSignal } from "solid-js";
import { diffResult, rawJson, isLoading } from "./stores/diff-store";
import FileUpload from "./components/FileUpload";
import ArrangementView from "./components/ArrangementView";
import ResizeHandle from "./components/ResizeHandle";
import DetailView from "./components/DetailView";

export default function App() {
  const [showRawJson, setShowRawJson] = createSignal(false);

  return (
    <div
      style={{
        display: "flex",
        "flex-direction": "column",
        height: "100%",
      }}
    >
      <FileUpload />
      <Show when={isLoading()}>
        <div class="loading-container">
          <div class="loading-spinner" />
          Comparing files...
        </div>
      </Show>
      <Show
        when={diffResult()}
        fallback={
          <Show when={!isLoading()}>
            <div
              style={{
                flex: 1,
                display: "flex",
                "align-items": "center",
                "justify-content": "center",
                color: "var(--text-dim)",
              }}
            >
              Upload two .als files to compare
            </div>
          </Show>
        }
      >
        <ArrangementView />
        <ResizeHandle />
        <DetailView />
        <Show when={rawJson()}>
          <div
            style={{
              "border-top": "1px solid var(--border)",
              "flex-shrink": 0,
            }}
          >
            <button
              onClick={() => setShowRawJson(!showRawJson())}
              style={{
                background: "var(--bg-secondary)",
                color: "var(--text-secondary)",
                border: "none",
                padding: "4px 12px",
                "font-size": "11px",
                cursor: "pointer",
                width: "100%",
                "text-align": "left",
              }}
            >
              Raw JSON {showRawJson() ? "▼" : "▶"}
            </button>
            <Show when={showRawJson()}>
              <pre
                style={{
                  "max-height": "300px",
                  overflow: "auto",
                  "font-size": "10px",
                  padding: "8px",
                  margin: "0",
                  background: "var(--bg-secondary)",
                  color: "var(--text-primary)",
                }}
              >
                {rawJson()}
              </pre>
            </Show>
          </div>
        </Show>
      </Show>
    </div>
  );
}
