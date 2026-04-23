export type ChangeType = "Unchanged" | "Added" | "Removed" | "Modified";

export type DomainType =
  | "Liveset"
  | "Track"
  | "Device"
  | "Clip"
  | "Automation"
  | "Mixer"
  | "Routing"
  | "Locator"
  | "Param"
  | "Note"
  | "Event"
  | "Send"
  | "Preset"
  | "Macro"
  | "Snapshot"
  | "Loop"
  | "Signature"
  | "SampleRef"
  | "Version"
  | "Other";

export type FieldValue = number | string | boolean;

export interface FieldView {
  type: "field";
  name: string;
  change: ChangeType;
  domain_type: DomainType;
  old_value?: FieldValue;
  new_value?: FieldValue;
}

export interface ItemView {
  type: "item";
  name: string;
  change: ChangeType;
  domain_type: DomainType;
  children?: ViewNode[];
}

export interface CollectionView {
  type: "collection";
  name: string;
  change: ChangeType;
  domain_type: DomainType;
  items: ViewNode[];
  total?: number;
  displayed?: number;
  truncated?: { added: number; removed: number; modified: number };
}

export type ViewNode = FieldView | ItemView | CollectionView;

export interface DiffResult {
  diff: ViewNode[];
}

export interface TrackData {
  name: string;
  change: ChangeType;
  domainType: DomainType;
  children: ViewNode[];
}

export interface ClipData {
  name: string;
  change: ChangeType;
  startTime: number;
  endTime: number;
  children: ViewNode[];
  clipType: "midi" | "audio";
}

export interface TimelineRange {
  minStart: number;
  maxEnd: number;
  totalBeats: number;
}
