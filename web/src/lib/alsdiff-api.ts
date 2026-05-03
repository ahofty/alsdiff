import type { DiffResult } from "../types";

let worker: Worker | null = null;
let requestIdCounter = 0;
const pendingRequests = new Map<
  number,
  { resolve: (result: DiffResult) => void; reject: (error: Error) => void }
>();

function getWorker(): Worker {
  if (!worker) {
    worker = new Worker("/alsdiff-worker.js", { type: "classic" });
    worker.onmessage = (e: MessageEvent) => {
      const msg = e.data;
      const entry = pendingRequests.get(msg.requestId);
      if (!entry) return;
      pendingRequests.delete(msg.requestId);
      if (msg.type === "result") {
        try {
          const result: DiffResult = JSON.parse(msg.result);
          console.log("[alsdiff] parsed diff:", result.diff.length, "top-level nodes");
          entry.resolve(result);
        } catch (err) {
          entry.reject(new Error(`Failed to parse diff result: ${err}`));
        }
      } else if (msg.type === "error") {
        entry.reject(new Error(msg.error));
      }
    };
    worker.onerror = (e: ErrorEvent) => {
      console.error("[alsdiff] worker error:", e.message);
      for (const [id, entry] of pendingRequests) {
        entry.reject(new Error(`Worker error: ${e.message}`));
        pendingRequests.delete(id);
      }
      worker = null;
    };
  }
  return worker;
}

export function diffFilesJson(
  file1: File,
  file2: File,
): Promise<DiffResult> {
  const w = getWorker();
  const requestId = requestIdCounter++;

  return new Promise<DiffResult>((resolve, reject) => {
    pendingRequests.set(requestId, { resolve, reject });
    w.postMessage({
      type: "diff",
      requestId,
      file1,
      file2,
      options: { mode: "json", preset: "verbose" },
    });
  });
}
