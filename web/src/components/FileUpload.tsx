import { Show, For } from "solid-js";
import { setIsLoading, setError, setDiffResult, setTracks, setRawJson, setTempo, setTimeSignature, isLoading, error } from "../stores/diff-store";
import { diffFilesJson } from "../lib/alsdiff-api";
import { extractTracks, extractTempo, extractTimeSignature } from "../lib/diff-parser";
import {
  preset, setPreset,
  customConfigName,
  PRESET_OPTIONS,
  type PresetName,
  buildDiffOptions,
  handleConfigUpload,
  clearCustomConfig,
} from "../stores/settings-store";
import type { ViewNode } from "../types";

export default function FileUpload() {
  let file1: File | null = null;
  let file2: File | null = null;
  let configInput!: HTMLInputElement;

  const handleFile = (
    e: Event & { currentTarget: HTMLInputElement },
    which: 1 | 2,
  ) => {
    const files = e.currentTarget.files;
    if (files && files.length > 0) {
      if (which === 1) file1 = files[0];
      else file2 = files[0];
      setError(null);
    }
  };

  const handleConfigChange = async (
    e: Event & { currentTarget: HTMLInputElement },
  ) => {
    const files = e.currentTarget.files;
    if (files && files.length > 0) {
      try {
        await handleConfigUpload(files[0]);
        setError(null);
      } catch (err) {
        setError(err instanceof Error ? err.message : String(err));
      }
    }
    e.currentTarget.value = "";
  };

  const handleCompare = async () => {
    if (!file1 || !file2) {
      setError("Please select two .als files");
      return;
    }

    setIsLoading(true);
    setError(null);

    try {
      const options = buildDiffOptions();
      const result = await diffFilesJson(file1, file2, options);
      setRawJson(JSON.stringify(result, null, 2));

      if (!result.diff || result.diff.length === 0) {
        setError("No differences found between files.");
        setDiffResult(null);
        return;
      }

      setDiffResult(result);
      const liveset = result.diff[0] as ViewNode | undefined;
      const children =
        liveset && liveset.type === "item" ? liveset.children ?? [] : [];
      const extractedTracks = extractTracks(children);
      setTracks(extractedTracks);
      setTempo(extractTempo(children));
      setTimeSignature(extractTimeSignature(children));
    } catch (e: unknown) {
      setError(e instanceof Error ? e.message : String(e));
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <div class="file-upload-bar">
      <div class="upload-group">
        <label class="upload-label">File A</label>
        <input
          type="file"
          accept=".als"
          onChange={(e) => handleFile(e, 1)}
          disabled={isLoading()}
        />
      </div>
      <div class="upload-group">
        <label class="upload-label">File B</label>
        <input
          type="file"
          accept=".als"
          onChange={(e) => handleFile(e, 2)}
          disabled={isLoading()}
        />
      </div>

      <div class="upload-group">
        <label class="upload-label">Preset</label>
        <select
          class="preset-select"
          value={preset()}
          onChange={(e) => setPreset(e.currentTarget.value as PresetName)}
          disabled={isLoading()}
        >
          <For each={PRESET_OPTIONS}>
            {(opt) => <option value={opt.value}>{opt.label}</option>}
          </For>
        </select>
      </div>

      <div class="upload-group">
        <input
          ref={configInput}
          type="file"
          accept=".json"
          onChange={handleConfigChange}
          style={{ display: "none" }}
        />
        <button
          class="config-btn"
          onClick={() => configInput.click()}
          disabled={isLoading()}
          title="Upload custom config JSON"
        >
          Config
        </button>
        <Show when={customConfigName()}>
          <span class="config-badge">
            {customConfigName()}
            <button class="config-clear" onClick={clearCustomConfig}>
              ×
            </button>
          </span>
        </Show>
      </div>

      <button
        class="compare-btn"
        onClick={handleCompare}
        disabled={isLoading()}
      >
        <Show when={isLoading()} fallback="Compare">
          Comparing...
        </Show>
      </button>
      <Show when={error()}>
        <span class="error-msg">{error()}</span>
      </Show>

      <style>{`
        .file-upload-bar {
          display: flex;
          align-items: center;
          gap: 16px;
          padding: 10px 16px;
          background: var(--bg-header);
          border-bottom: 1px solid var(--border);
          flex-shrink: 0;
        }
        .upload-group {
          display: flex;
          align-items: center;
          gap: 8px;
        }
        .upload-label {
          color: var(--text-secondary);
          font-size: 12px;
          font-weight: 600;
        }
        .upload-group input[type="file"] {
          color: var(--text-primary);
          font-size: 12px;
          max-width: 200px;
        }
        .upload-group input[type="file"]::file-selector-button {
          background: var(--bg-secondary);
          color: var(--text-primary);
          border: 1px solid var(--border);
          padding: 4px 10px;
          border-radius: 3px;
          cursor: pointer;
          font-size: 12px;
        }
        .upload-group input[type="file"]::file-selector-button:hover {
          background: var(--bg-hover);
        }
        .preset-select {
          background: var(--bg-secondary);
          color: var(--text-primary);
          border: 1px solid var(--border);
          padding: 4px 8px;
          border-radius: 3px;
          font-size: 12px;
          cursor: pointer;
        }
        .preset-select:hover {
          background: var(--bg-hover);
        }
        .config-btn {
          background: var(--bg-secondary);
          color: var(--text-primary);
          border: 1px solid var(--border);
          padding: 4px 10px;
          border-radius: 3px;
          cursor: pointer;
          font-size: 12px;
        }
        .config-btn:hover:not(:disabled) {
          background: var(--bg-hover);
        }
        .config-btn:disabled {
          opacity: 0.5;
          cursor: not-allowed;
        }
        .config-badge {
          display: inline-flex;
          align-items: center;
          gap: 4px;
          background: var(--bg-secondary);
          border: 1px solid var(--border);
          border-radius: 3px;
          padding: 2px 6px;
          font-size: 11px;
          color: var(--text-secondary);
          max-width: 160px;
          overflow: hidden;
          text-overflow: ellipsis;
          white-space: nowrap;
        }
        .config-clear {
          background: none;
          border: none;
          color: var(--text-secondary);
          cursor: pointer;
          font-size: 14px;
          line-height: 1;
          padding: 0 2px;
        }
        .config-clear:hover {
          color: var(--color-removed);
        }
        .compare-btn {
          background: var(--color-modified);
          color: #000;
          border: none;
          padding: 6px 20px;
          border-radius: 4px;
          font-weight: 600;
          font-size: 13px;
          cursor: pointer;
        }
        .compare-btn:hover:not(:disabled) {
          opacity: 0.9;
        }
        .compare-btn:disabled {
          opacity: 0.5;
          cursor: not-allowed;
        }
        .error-msg {
          color: var(--color-removed);
          font-size: 12px;
        }
      `}</style>
    </div>
  );
}
