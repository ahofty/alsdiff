export interface TimeSignature {
  numer: number;
  denom: number;
}

export function quarterNoteToPosition(
  qn: number,
  ts: TimeSignature,
): { bar: number; beat: number; sixteenth: number } {
  if (qn <= 0) return { bar: 1, beat: 1, sixteenth: 1 };
  if (ts.denom === 0) return { bar: 1, beat: 1, sixteenth: 1 };
  const qnPerBar = (ts.numer * 4) / ts.denom;
  const qnPerBeat = 4 / ts.denom;
  const barCount = Math.floor(qn / qnPerBar);
  const bar = barCount + 1;
  const remBar = qn - barCount * qnPerBar;
  const beatCount = Math.floor(remBar / qnPerBeat);
  const beat = beatCount + 1;
  const remBeat = remBar - beatCount * qnPerBeat;
  const sixteenth = Math.floor(remBeat * 4) + 1;
  return { bar, beat, sixteenth };
}

export function formatPosition(
  bar: number,
  beat: number,
  sixteenth: number,
): string {
  if (beat === 1 && sixteenth === 1) return String(bar);
  if (sixteenth === 1) return `${bar}.${beat}`;
  return `${bar}.${beat}.${sixteenth}`;
}

export function quarterNoteToRealtime(
  qn: number,
  bpm: number,
): { min: number; sec: number; ms: number } {
  const totalSeconds = (qn * 60) / bpm;
  const min = Math.floor(totalSeconds / 60);
  const rem = totalSeconds - min * 60;
  const sec = Math.floor(rem);
  const ms = Math.round((rem - sec) * 1000);
  return { min, sec, ms };
}

export function formatRealtime(
  min: number,
  sec: number,
  ms: number,
): string {
  const ss = String(sec).padStart(2, "0");
  if (ms > 0) {
    const tenths = Math.round(ms / 100);
    return tenths > 0 ? `${min}:${ss}.${tenths}` : `${min}:${ss}`;
  }
  return `${min}:${ss}`;
}
