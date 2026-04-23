import { type JSX, Show } from "solid-js";
import { setIsLoading, setError, setDiffResult, setTracks, setRawJson, isLoading, error } from "../stores/diff-store";
import { diffFilesJson } from "../lib/alsdiff-api";
import { extractTracks } from "../lib/diff-parser";
import type { DiffResult, ViewNode } from "../types";

export default function FileUpload() {
  let file1: File | null = null;
  let file2: File | null = null;

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

  const handleCompare = async () => {
    if (!file1 || !file2) {
      setError("Please select two .als files");
      return;
    }

    setIsLoading(true);
    setError(null);

    try {
      const result = await diffFilesJson(file1, file2);
      setRawJson(JSON.stringify(result, null, 2));
      console.log("[alsdiff] diff result:", result);

      if (!result.diff || result.diff.length === 0) {
        setError("No differences found between files.");
        setDiffResult(null);
        return;
      }

      setDiffResult(result);
      const liveset = result.diff[0] as ViewNode | undefined;
      const children =
        liveset && liveset.type === "item" ? liveset.children ?? [] : [];
      console.log("[alsdiff] liveset children:", children.length);
      const extractedTracks = extractTracks(children);
      console.log("[alsdiff] extracted tracks:", extractedTracks.length);
      setTracks(extractedTracks);
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
