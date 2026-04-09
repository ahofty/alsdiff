#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const zlib = require('zlib');
const { TextDecoder } = require('util');

function fail(message) {
  console.error(`[js-regression] FAIL: ${message}`);
  process.exit(1);
}

function ensure(condition, message) {
  if (!condition) fail(message);
}

async function withTimeout(promise, label, timeoutMs = 30000) {
  let timer = null;
  const timeout = new Promise((_, reject) => {
    timer = setTimeout(() => reject(new Error(`${label} timed out after ${timeoutMs}ms`)), timeoutMs);
  });
  try {
    return await Promise.race([promise, timeout]);
  } finally {
    clearTimeout(timer);
  }
}

function bufferToArrayBuffer(buffer) {
  return buffer.buffer.slice(buffer.byteOffset, buffer.byteOffset + buffer.byteLength);
}

function makeFileLike(name, compressedBuffer) {
  return {
    name,
    async arrayBuffer() {
      return bufferToArrayBuffer(compressedBuffer);
    },
  };
}

function setGlobalBrowserShims() {
  global.window = global;
  global.self = global;
  global.TextDecoder = TextDecoder;
  global.performance = global.performance || { now: () => Date.now() };
  global.__alsdiff_files = {};
  global.pako = {
    ungzip(bytes) {
      return zlib.gunzipSync(Buffer.from(bytes));
    },
  };
}

function loadBrowserBundle(bundlePath) {
  const resolved = path.resolve(bundlePath);
  delete require.cache[resolved];
  require(resolved);
  ensure(global.alsdiff, 'window.alsdiff is missing after loading browser bundle');
  ensure(typeof global.alsdiff.diffFilesById === 'function', 'diffFilesById is missing');
  ensure(typeof global.alsdiff.diffFiles === 'function', 'diffFiles is missing');
  ensure(typeof global.alsdiff.diffBlobs === 'function', 'diffBlobs is missing');
  ensure(typeof global.alsdiff.setDebug === 'function', 'setDebug is missing');
}

function storeFiles(file1, file2) {
  if (!global.__alsdiff_files) {
    global.__alsdiff_files = {};
  }
  const id1 = Object.keys(global.__alsdiff_files).length;
  const id2 = id1 + 1;
  global.__alsdiff_files[id1] = file1;
  global.__alsdiff_files[id2] = file2;
  return [id1, id2];
}

async function captureConsoleLogs(asyncFn) {
  const originalLog = console.log;
  const logs = [];
  console.log = (...args) => {
    logs.push(args.map((arg) => String(arg)).join(' '));
    originalLog(...args);
  };
  try {
    const result = await asyncFn();
    return { logs, result };
  } finally {
    console.log = originalLog;
  }
}

async function runBrowserApiTests(xmlPath) {
  const xml = fs.readFileSync(xmlPath, 'utf8');
  const xmlChanged = xml.replace('<Ableton', '<Ableton TestAttr="1"');

  const gz1 = zlib.gzipSync(Buffer.from(xml, 'utf8'));
  const gz2 = zlib.gzipSync(Buffer.from(xmlChanged, 'utf8'));

  const file1 = makeFileLike('a.als', gz1);
  const file2 = makeFileLike('b.als', gz2);
  const file3 = makeFileLike('c.als', gz1);
  const file4 = makeFileLike('d.als', gz2);

  const [id1, id2] = storeFiles(file1, file2);
  const singleResult = await withTimeout(
    Promise.resolve(global.alsdiff.diffFilesById(id1, id2, file1.name, file2.name)),
    'diffFilesById single run'
  );
  ensure(typeof singleResult === 'string', 'diffFilesById should resolve to string');
  ensure(singleResult.includes('LiveSet'), 'diffFilesById output missing LiveSet section');

  const [id3, id4] = storeFiles(file3, file4);
  const [concurrent1, concurrent2] = await withTimeout(
    Promise.all([
      Promise.resolve(global.alsdiff.diffFilesById(id1, id2, file1.name, file2.name)),
      Promise.resolve(global.alsdiff.diffFilesById(id3, id4, file3.name, file4.name)),
    ]),
    'diffFilesById concurrent run'
  );
  ensure(typeof concurrent1 === 'string' && typeof concurrent2 === 'string',
    'concurrent diffFilesById runs should both resolve to strings');

  const legacyFilesResult = await withTimeout(
    Promise.resolve(global.alsdiff.diffFiles(file1, file2)),
    'diffFiles legacy run'
  );
  ensure(typeof legacyFilesResult === 'string', 'diffFiles should resolve to string');

  const legacyBlobsResult = await withTimeout(
    Promise.resolve(global.alsdiff.diffBlobs(file1, file1.name, file2, file2.name)),
    'diffBlobs legacy run'
  );
  ensure(typeof legacyBlobsResult === 'string', 'diffBlobs should resolve to string');

  const optionsStatsResult = await withTimeout(
    Promise.resolve(global.alsdiff.diffFilesById(id1, id2, file1.name, file2.name, {
      mode: 'stats',
      preset: 'quiet',
    })),
    'diffFilesById options stats run'
  );
  ensure(typeof optionsStatsResult === 'string', 'options stats run should resolve to string');
  ensure(optionsStatsResult.length > 0, 'options stats run should produce non-empty output');

  const optionsTreeResult = await withTimeout(
    Promise.resolve(global.alsdiff.diffFilesById(id1, id2, file1.name, file2.name, {
      mode: 'tree',
      prefixAdded: '[+]',
      prefixRemoved: '[-]',
      prefixModified: '[*]',
      prefixUnchanged: '[=]',
      noteNameStyle: 'Flat',
      maxCollectionItems: 25,
    })),
    'diffFilesById options tree run'
  );
  ensure(typeof optionsTreeResult === 'string', 'options tree run should resolve to string');
  ensure(optionsTreeResult.includes('[*]'),
    'options tree run should include modified prefix override');

  try {
    await withTimeout(
      Promise.resolve(global.alsdiff.diffFilesById(id1, id2, file1.name, file2.name, {
        mode: 'stats',
        prefixAdded: 'ADD',
      })),
      'stats incompatible options run'
    );
    fail('stats mode should reject prefix overrides');
  } catch (err) {
    const message = err && err.message ? err.message : String(err);
    ensure(message.includes('--mode stats is incompatible'),
      `unexpected stats incompatibility error: ${message}`);
  }

  global.alsdiff.setDebug(true);
  const deprecatedOptionsResult = await captureConsoleLogs(async () => withTimeout(
    Promise.resolve(global.alsdiff.diffFilesById(id1, id2, file1.name, file2.name, {
      dumpPreset: 'full',
      validateConfig: '{"added":"Summary"}',
      dumpSchema: true,
    })),
    'deprecated options compatibility run'
  ));
  ensure(typeof deprecatedOptionsResult.result === 'string',
    'deprecated options should still resolve normally');
  const deprecatedWarningFound = deprecatedOptionsResult.logs.some((line) =>
    line.includes('Ignored unsupported browser options')
      && line.includes('dumpPreset')
      && line.includes('validateConfig')
      && line.includes('dumpSchema'));
  ensure(deprecatedWarningFound,
    'debug logs should include warning for ignored deprecated browser options');
  global.alsdiff.setDebug(false);

  const savedPako = global.pako;
  global.pako = undefined;
  try {
    const [idErr1, idErr2] = storeFiles(file1, file2);
    await withTimeout(
      Promise.resolve(global.alsdiff.diffFilesById(idErr1, idErr2, file1.name, file2.name)),
      'missing pako error run'
    );
    fail('diffFilesById should reject when pako is missing');
  } catch (err) {
    const message = err && err.message ? err.message : String(err);
    ensure(message.includes('pako.js library not loaded'),
      `unexpected missing pako error: ${message}`);
  } finally {
    global.pako = savedPako;
  }

  global.alsdiff.setDebug(false);
}

async function main() {
  const [bundlePath, xmlPath] = process.argv.slice(2);
  if (!bundlePath || !xmlPath) {
    fail('usage: node js_regression_runner.js <browser_bundle.js> <t4.xml>');
  }

  setGlobalBrowserShims();
  loadBrowserBundle(bundlePath);
  global.alsdiff.setDebug(false);

  await runBrowserApiTests(xmlPath);

  console.log('[js-regression] PASS');
}

main().catch((err) => {
  const message = err && err.stack ? err.stack : String(err);
  fail(message);
});
