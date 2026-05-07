import { createSignal, createEffect } from "solid-js";

export type PresetName =
  | "compact"
  | "composer"
  | "config"
  | "full"
  | "inline"
  | "mixing"
  | "quiet"
  | "verbose";

export const PRESET_OPTIONS: { value: PresetName; label: string }[] = [
  { value: "verbose", label: "Verbose" },
  { value: "full", label: "Full" },
  { value: "compact", label: "Compact" },
  { value: "inline", label: "Inline" },
  { value: "quiet", label: "Quiet" },
  { value: "mixing", label: "Mixing" },
  { value: "composer", label: "Composer" },
  { value: "config", label: "config.json" },
];

const STORAGE_KEY = "alsdiff-settings";

interface SettingsState {
  preset: PresetName;
  customConfig: string | null;
  customConfigName: string | null;
}

function loadSettings(): SettingsState {
  try {
    const raw = localStorage.getItem(STORAGE_KEY);
    if (raw) {
      const parsed = JSON.parse(raw) as Partial<SettingsState>;
      return {
        preset: parsed.preset ?? "verbose",
        customConfig: parsed.customConfig ?? null,
        customConfigName: parsed.customConfigName ?? null,
      };
    }
  } catch {
    // ignore corrupt data
  }
  return { preset: "verbose", customConfig: null, customConfigName: null };
}

const saved = loadSettings();

export const [preset, setPreset] = createSignal<PresetName>(saved.preset);
export const [customConfig, setCustomConfig] = createSignal<string | null>(
  saved.customConfig,
);
export const [customConfigName, setCustomConfigName] = createSignal<
  string | null
>(saved.customConfigName);

createEffect(() => {
  const state: SettingsState = {
    preset: preset(),
    customConfig: customConfig(),
    customConfigName: customConfigName(),
  };
  localStorage.setItem(STORAGE_KEY, JSON.stringify(state));
});

export function handleConfigUpload(file: File): Promise<void> {
  return new Promise((resolve, reject) => {
    const reader = new FileReader();
    reader.onload = () => {
      try {
        const text = reader.result as string;
        JSON.parse(text);
        setCustomConfig(text);
        setCustomConfigName(file.name);
        resolve();
      } catch {
        reject(new Error("Invalid JSON in config file"));
      }
    };
    reader.onerror = () => reject(new Error("Failed to read config file"));
    reader.readAsText(file);
  });
}

export function clearCustomConfig(): void {
  setCustomConfig(null);
  setCustomConfigName(null);
}

export function buildDiffOptions(): {
  mode: string;
  preset?: string;
  config?: string;
} {
  if (preset() === "config") {
    const cfg = customConfig();
    if (cfg) {
      return { mode: "json", config: cfg };
    }
    return { mode: "json", preset: "verbose" };
  }
  return { mode: "json", preset: preset() };
}
