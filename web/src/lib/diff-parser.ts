import type {
  ViewNode,
  ItemView,
  CollectionView,
  TrackData,
  TrackNode,
  ClipData,
  TimelineRange,
} from "../types";
import type { TimeSignature } from "./time-format";

function isItem(node: ViewNode): node is ItemView {
  return node.type === "item";
}

function isCollection(node: ViewNode): node is CollectionView {
  return node.type === "collection";
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

function getTrackIntField(children: ViewNode[], fieldName: string, defaultVal: number): number {
  const field = children.find(c => c.type === "field" && c.name === fieldName);
  if (field && field.type === "field") return ((field.new_value ?? field.old_value) as number) ?? defaultVal;
  return defaultVal;
}

/** Extract track ID from item name like "AudioTrack (#17): Bell" → 17 */
function extractTrackIdFromName(name: string): number {
  const m = name.match(/\(#(\d+)\)/);
  return m ? parseInt(m[1], 10) : 0;
}

export function extractTracks(livesetChildren: ViewNode[]): TrackData[] {
  const tracks: TrackData[] = [];

  for (const child of livesetChildren) {
    if (isItem(child) && child.domain_type === "Track") {
      const tc = child.children ?? [];
      const fieldTrackId = getTrackIntField(tc, "TrackId", 0);
      tracks.push({
        name: child.name,
        change: child.change,
        domainType: child.domain_type,
        trackId: fieldTrackId || extractTrackIdFromName(child.name),
        groupId: getTrackIntField(tc, "GroupId", -1),
        children: tc,
      });
    }
  }

  return tracks;
}

export function buildTrackHierarchy(tracks: TrackData[]): TrackNode[] {
  const nodes: TrackNode[] = tracks.map((track, i) => ({
    track,
    trackIndex: i,
    depth: 0,
    children: [],
  }));

  // Build index from trackId to node (first occurrence wins for lookups)
  const idToNode = new Map<number, TrackNode>();
  for (const node of nodes) {
    if (!idToNode.has(node.track.trackId)) {
      idToNode.set(node.track.trackId, node);
    }
  }

  const topNodes: TrackNode[] = [];
  for (const node of nodes) {
    if (node.track.groupId !== -1 && idToNode.has(node.track.groupId)) {
      idToNode.get(node.track.groupId)!.children.push(node);
    } else {
      topNodes.push(node);
    }
  }

  const setDepth = (ns: TrackNode[], depth: number) => {
    for (const n of ns) {
      n.depth = depth;
      setDepth(n.children, depth + 1);
    }
  };
  setDepth(topNodes, 0);

  return topNodes;
}

export function flattenVisibleTracks(
  rootNodes: TrackNode[],
  collapsedGroups: Set<number>,
): TrackNode[] {
  const result: TrackNode[] = [];
  const walk = (nodes: TrackNode[]) => {
    for (const node of nodes) {
      result.push(node);
      const isGroup = node.children.length > 0;
      if (isGroup && !collapsedGroups.has(node.track.trackId)) {
        walk(node.children);
      }
    }
  };
  walk(rootNodes);
  return result;
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

export function extractTempo(diffChildren: ViewNode[]): number {
  for (const child of diffChildren) {
    if (isItem(child) && child.domain_type === "Track") {
      const mixer = child.children?.find(
        (c): c is ItemView => isItem(c) && c.name === "Main Mixer",
      );
      if (!mixer?.children) continue;
      const tempo = mixer.children.find(
        (c): c is ItemView => isItem(c) && c.name === "Tempo",
      );
      if (!tempo?.children) continue;
      const value = getNumericField(tempo.children, "Value");
      if (value !== undefined) return value;
    }
  }
  return 120;
}

export function extractTimeSignature(
  diffChildren: ViewNode[],
): TimeSignature {
  for (const child of diffChildren) {
    if (isItem(child) && child.domain_type === "Track") {
      const mixer = child.children?.find(
        (c): c is ItemView => isItem(c) && c.name === "Main Mixer",
      );
      if (!mixer?.children) continue;
      const ts = mixer.children.find(
        (c): c is ItemView => isItem(c) && c.name === "Time Signature",
      );
      if (!ts?.children) continue;
      const numer = getNumericField(ts.children, "Numerator");
      const denom = getNumericField(ts.children, "Denominator");
      if (numer !== undefined && denom !== undefined)
        return { numer, denom };
    }
  }
  return { numer: 4, denom: 4 };
}

export function computeTimelineRange(tracks: TrackData[]): TimelineRange {
  let minStart = Infinity;
  let maxEnd = -Infinity;

  const allClips: ClipData[] = [];
  for (const track of tracks) {
    allClips.push(...extractClips(track));
  }

  const changedClips = allClips.filter((c) => c.change !== "Unchanged");
  const source = changedClips.length > 0 ? changedClips : allClips;

  for (const clip of source) {
    if (clip.startTime < minStart) minStart = clip.startTime;
    if (clip.endTime > maxEnd) maxEnd = clip.endTime;
  }

  if (minStart === Infinity) {
    minStart = 0;
    maxEnd = 32;
  }

  const range = maxEnd - minStart;
  const padding = Math.max(4, range * 0.1);

  return {
    minStart: Math.max(0, minStart - padding),
    maxEnd: maxEnd + padding,
    totalBeats: Math.max(1, (maxEnd + padding) - Math.max(0, minStart - padding)),
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
