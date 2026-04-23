import type { DiffResult } from "../types";

declare global {
  interface Window {
    alsdiff: {
      diffFilesById: (
        id1: number,
        id2: number,
        name1: string,
        name2: string,
        options: { mode: string; preset: string },
      ) => Promise<string>;
      setDebug: (enabled: boolean) => void;
    };
    __alsdiff_files: Record<number, File>;
  }
}

let fileIdCounter = 0;
const fileStore: Record<number, File> = {};

function storeFile(file: File): number {
  const id = fileIdCounter++;
  fileStore[id] = file;
  window.__alsdiff_files = { ...window.__alsdiff_files, ...fileStore };
  return id;
}

export function diffFilesJson(
  file1: File,
  file2: File,
): Promise<DiffResult> {
  const id1 = storeFile(file1);
  const id2 = storeFile(file2);

  return window.alsdiff
    .diffFilesById(id1, id2, file1.name, file2.name, {
      mode: "json",
      preset: "verbose",
    })
    .then((jsonStr: string) => {
      console.log("[alsdiff] raw JSON response:", jsonStr);
      const result: DiffResult = JSON.parse(jsonStr);
      console.log("[alsdiff] parsed diff:", result.diff.length, "top-level nodes");
      return result;
    });
}
