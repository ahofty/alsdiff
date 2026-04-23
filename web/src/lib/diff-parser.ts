import type {
  ViewNode,
  ItemView,
  CollectionView,
  TrackData,
  ClipData,
  TimelineRange,
} from "../types";

function isItem(node: ViewNode): node is ItemView {
  return node.type === "item";
}

function isCollection(node: ViewNode): node is CollectionView {
  return node.type === "collection";
}

function findChildItems(children: ViewNode[], domainType: string): ItemView[] {
  return children.filter(
    (c): c is ItemView => isItem(c) && c.domain_type === domainType,
  );
}

function findCollection(
  children: ViewNode[],
  name: string,
): CollectionView | undefined {
  return children.find(
    (c): c is CollectionView => isCollection(c) && c.name === name,
  );
}

function getNumericField(
  children: ViewNode[],
  fieldName: string,
): number | undefined {
  const field = children.find(
    (c) => c.type === "field" && c.name === fieldName,
  );
  if (field && field.type === "field") {
    return (field.new_value ?? field.old_value) as number | undefined;
  }
  return undefined;
}

export function extractTracks(livesetChildren: ViewNode[]): TrackData[] {
  const tracks: TrackData[] = [];

  for (const child of livesetChildren) {
    if (isItem(child) && child.domain_type === "Track") {
      tracks.push({
        name: child.name,
        change: child.change,
        domainType: child.domain_type,
        children: child.children ?? [],
      });
    }
  }

  return tracks;
}

export function extractClips(track: TrackData): ClipData[] {
  const clips: ClipData[] = [];

  const clipsCollection = findCollection(track.children, "Clips");
  if (!clipsCollection) return clips;

  for (const node of clipsCollection.items) {
    if (!isItem(node) || node.domain_type !== "Clip") continue;

    const children = node.children ?? [];
    const startTime = getNumericField(children, "Start Time") ?? 0;
    const endTime = getNumericField(children, "End Time") ?? startTime + 4;

    const clipType = node.name.startsWith("AudioClip") ? "audio" : "midi";

    clips.push({
      name: node.name,
      change: node.change,
      startTime,
      endTime,
      children,
      clipType,
    });
  }

  return clips;
}

export function extractDevices(track: TrackData): ItemView[] {
  const devicesCollection = findCollection(track.children, "Devices");
  if (!devicesCollection) return [];
  return devicesCollection.items.filter(isItem);
}

export function extractMixer(track: TrackData): ItemView | undefined {
  return track.children.find(
    (c): c is ItemView => isItem(c) && c.name === "Mixer",
  );
}

export function extractAutomations(track: TrackData): ItemView[] {
  const autoCollection = findCollection(track.children, "Automations");
  if (!autoCollection) return [];
  return autoCollection.items.filter(isItem);
}

export function extractRoutings(track: TrackData): ItemView | undefined {
  return track.children.find(
    (c): c is ItemView => isItem(c) && c.name === "Routings",
  );
}

export function computeTimelineRange(tracks: TrackData[]): TimelineRange {
  let minStart = Infinity;
  let maxEnd = -Infinity;

  for (const track of tracks) {
    const clips = extractClips(track);
    for (const clip of clips) {
      if (clip.startTime < minStart) minStart = clip.startTime;
      if (clip.endTime > maxEnd) maxEnd = clip.endTime;
    }
  }

  if (minStart === Infinity) {
    minStart = 0;
    maxEnd = 32;
  }

  return {
    minStart,
    maxEnd,
    totalBeats: maxEnd - minStart,
  };
}

export function getChangeColor(change: string): string {
  switch (change) {
    case "Added":
      return "var(--color-added)";
    case "Removed":
      return "var(--color-removed)";
    case "Modified":
      return "var(--color-modified)";
    default:
      return "var(--color-unchanged)";
  }
}
