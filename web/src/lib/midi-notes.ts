import type {
  ViewNode,
  ItemView,
  CollectionView,
  MidiNoteData,
  NoteRange,
} from "../types";

const NOTE_NAMES = [
  "C",
  "C#",
  "D",
  "D#",
  "E",
  "F",
  "F#",
  "G",
  "G#",
  "A",
  "A#",
  "B",
];

export function getNoteName(pitch: number): string {
  const octave = Math.floor(pitch / 12) - 1;
  const note = NOTE_NAMES[pitch % 12];
  return `${note}${octave}`;
}

export function isBlackKey(pitch: number): boolean {
  const n = pitch % 12;
  return n === 1 || n === 3 || n === 6 || n === 8 || n === 10;
}

function isCollection(node: ViewNode): node is CollectionView {
  return node.type === "collection";
}

function isItem(node: ViewNode): node is ItemView {
  return node.type === "item";
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

export function extractMidiNotes(clipChildren: ViewNode[]): MidiNoteData[] {
  const notesCollection = clipChildren.find(
    (c): c is CollectionView =>
      isCollection(c) && c.name === "Notes",
  );
  if (!notesCollection) return [];

  const notes: MidiNoteData[] = [];

  for (const node of notesCollection.items) {
    if (!isItem(node) || node.domain_type !== "Note") continue;

    const children = node.children ?? [];
    const pitch = getNumericField(children, "Note") ?? 60;
    const time = getNumericField(children, "Time") ?? 0;
    const duration = getNumericField(children, "Duration") ?? 1;
    const velocity = getNumericField(children, "Velocity") ?? 100;
    const offVelocity = getNumericField(children, "Off Velocity") ?? 0;

    // For modified notes, extract old values from fields with old_value
    const pitchField = children.find(
      (c) => c.type === "field" && c.name === "Note",
    );
    const timeField = children.find(
      (c) => c.type === "field" && c.name === "Time",
    );
    const durationField = children.find(
      (c) => c.type === "field" && c.name === "Duration",
    );
    const velocityField = children.find(
      (c) => c.type === "field" && c.name === "Velocity",
    );

    const note: MidiNoteData = {
      pitch,
      time,
      duration,
      velocity,
      offVelocity,
      change: node.change,
    };

    if (
      pitchField?.type === "field" &&
      pitchField.old_value !== undefined
    ) {
      note.oldPitch = pitchField.old_value as number;
    }
    if (
      timeField?.type === "field" &&
      timeField.old_value !== undefined
    ) {
      note.oldTime = timeField.old_value as number;
    }
    if (
      durationField?.type === "field" &&
      durationField.old_value !== undefined
    ) {
      note.oldDuration = durationField.old_value as number;
    }
    if (
      velocityField?.type === "field" &&
      velocityField.old_value !== undefined
    ) {
      note.oldVelocity = velocityField.old_value as number;
    }

    notes.push(note);
  }

  return notes;
}

export function computeNoteRange(notes: MidiNoteData[]): NoteRange {
  if (notes.length === 0) {
    return { minPitch: 48, maxPitch: 72, minTime: 0, maxTime: 4 };
  }

  let minPitch = 127;
  let maxPitch = 0;
  let minTime = Infinity;
  let maxTime = -Infinity;

  for (const note of notes) {
    const p = note.oldPitch ?? note.pitch;
    if (p < minPitch) minPitch = p;
    if (p > maxPitch) maxPitch = p;

    const t = note.oldTime ?? note.time;
    if (t < minTime) minTime = t;

    const endTime = (note.oldTime ?? note.time) + (note.oldDuration ?? note.duration);
    if (endTime > maxTime) maxTime = endTime;
  }

  const pitchPad = Math.max(2, Math.ceil((maxPitch - minPitch) * 0.15));
  const timePad = Math.max(1, (maxTime - minTime) * 0.1);

  return {
    minPitch: Math.max(0, minPitch - pitchPad),
    maxPitch: Math.min(127, maxPitch + pitchPad),
    minTime: Math.max(0, minTime - timePad),
    maxTime: maxTime + timePad,
  };
}
