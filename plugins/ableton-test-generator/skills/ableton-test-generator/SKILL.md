---
name: ableton-test-generator
description: Generate Ableton Live project files (.als) for testing. Use this skill when the user wants to create test .als files with specific configurations (tempo, tracks, clips, devices, etc.). The skill parses user prompts, applies changes via Ableton MCP tools, saves the project using ableton-save skill, and returns the saved file path.
---

# Ableton Test Generator

## Overview

Generate Ableton Live project files (.als) for testing by parsing user prompts, applying changes via Ableton MCP tools, and saving the project.

## Workflow

### 1. Parse User Request

Extract from the user's prompt:

**File path (required)** - Look for patterns like:
- "save to <path>"
- "save as <path>"
- "output to <path>"
- Relative or absolute paths ending in `.als`

**Changes to apply (optional)** - Identify:
- Tempo changes (e.g., "145 BPM", "tempo 120")
- Track creation (e.g., "create 3 MIDI tracks", "add audio track")
- Clip creation (e.g., "add clip", "create 4-bar clip")
- Device parameters (e.g., "set filter to 500 Hz")
- Automation (e.g., "automate volume from -inf to 0 dB")
- Other Live set modifications

### 2. Apply Changes Using Ableton MCP Tools

Map the parsed changes to the appropriate MCP tools:

| Change Type | MCP Tool |
|------------|----------|
| Tempo | `mcp__AbletonMCP__set_tempo` |
| Time signature | `mcp__AbletonMCP__set_signature_numerator`, `mcp__AbletonMCP__set_signature_denominator` |
| Create MIDI track | `mcp__AbletonMCP__create_midi_track` |
| Create audio track | `mcp__AbletonMCP__create_audio_track` |
| Set track name | `mcp__AbletonMCP__set_track_name` |
| Create clip | `mcp__AbletonMCP__create_clip` |
| Add MIDI notes | `mcp__AbletonMCP__add_notes_to_clip` |
| Set device parameter | `mcp__AbletonMCP__set_device_parameter` |
| Write automation | `mcp__AbletonMCP__write_automation` |

For comprehensive list of all 76 available MCP tools, see `references/ableton-mcp-tools.md`.

### 3. Save the Live Set

Use the Skill tool to invoke `ableton-save:save-live-set` with the extracted file path:

```
skill: "ableton-save", args: "<file_path>"
```

### 4. Return Result

Display the saved file path returned by the `ableton-save` skill:

```
Saved test .als file to: <returned_path>
```

## Error Handling

- **Missing file path**: Ask the user to provide the output file path
- **Ableton Live not running**: Inform the user that Ableton Live must be running
- **Change application failed**: Describe what went wrong and suggest alternatives
- **Save failed**: Check the ableton-save skill output for specific error messages

## Examples

**Example 1: Simple tempo change**
```
User: "Create a test file with tempo 145 BPM, save to test_als/test.als"

Actions:
1. Parse: tempo=145, path="test_als/test.als"
2. Call: mcp__AbletonMCP__set_tempo(tempo=145.0)
3. Call: skill: "ableton-save", args: "test_als/test.als"
4. Return: "Saved test .als file to: /path/to/test.als"
```

**Example 2: Multiple tracks with clips**
```
User: "Create 2 MIDI tracks with clips, tempo 120, save to /tmp/test_set.als"

Actions:
1. Parse: tracks=2, clips=yes, tempo=120, path="/tmp/test_set.als"
2. Call: mcp__AbletonMCP__set_tempo(tempo=120.0)
3. Call: mcp__AbletonMCP__create_midi_track(index=-1)
4. Call: mcp__AbletonMCP__create_clip(track_index=0, clip_index=0, length=4.0)
5. Call: mcp__AbletonMCP__create_midi_track(index=-1)
6. Call: mcp__AbletonMCP__create_clip(track_index=1, clip_index=0, length=4.0)
7. Call: skill: "ableton-save", args: "/tmp/test_set.als"
8. Return: "Saved test .als file to: /tmp/test_set.als"
```

## Resources

### references/ableton-mcp-tools.md

Complete reference of all 76 Ableton MCP tools organized by category. Read this file when you need to find specific tools for advanced Live set manipulation.

### scripts/

No scripts required for this skill - all operations are performed through MCP tools.

### assets/

No assets required for this skill.
