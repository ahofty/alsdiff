# alsdiff Browser Test

Simple browser-based test for the alsdiff JavaScript library.

## Syncing Dist Assets

`test/browser/index.html` is the source of truth. Regenerate this `dist/` folder with:

```bash
./scripts/sync_browser_dist.sh
```

## Running

Using Python's built-in HTTP server:

```bash
# Python 3
python3 -m http.server 8000

# Or Python 2
python -m SimpleHTTPServer 8000
```

Then open your browser to: http://localhost:8000

## Browser Compatibility

Requires `pako` to be loaded before `alsdiff.js` (already included in `index.html`).

## Usage

1. Select two Ableton Live Set (.als) files
2. Click "Compare Files"
3. The diff output will be displayed in the output area

## JavaScript API

The library exports four functions to the global `alsdiff` object:

```javascript
// Preferred API: compare files by pre-stored IDs from window.__alsdiff_files
await window.alsdiff.diffFilesById(id1, id2, name1, name2, options);

// Compare JavaScript File objects (from <input type="file">)
await window.alsdiff.diffFiles(file1, file2, options);

// Compare Blob objects with specified names
await window.alsdiff.diffBlobs(blob1, name1, blob2, name2, options);

// Enable/disable verbose debug logs
window.alsdiff.setDebug(true);  // or false
```

All diff functions return Promises that resolve to the diff output string.

Supported `options` fields:
- `mode`
- `config` (JSON content string or JSON object)
- `preset`
- `prefixAdded`, `prefixRemoved`, `prefixModified`, `prefixUnchanged`
- `noteNameStyle`
- `maxCollectionItems`
- `gitMode` and `positionalArgs` (accepted for parity; ignored for browser exit semantics)

Not supported in browser options:
- `dumpPreset`
- `validateConfig`
- `dumpSchema`

If unsupported fields are passed, they are ignored. With `setDebug(true)`, a warning is logged.
