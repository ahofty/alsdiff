# Ableton MCP Tools Reference

Complete reference of all 76 Ableton Live MCP (Model Context Protocol) tools for use with the ableton-test-generator skill.

## Table of Contents

- [Core Session Information](#core-session-information)
- [Application View Control](#application-view-control)
- [Transport & Playback Control](#transport--playback-control)
- [Track Management](#track-management)
- [Clip Management](#clip-management)
- [Scene Management](#scene-management)
- [Device Control](#device-control)
- [Tempo & Time Signature](#tempo--time-signature)
- [Automation](#automation)
- [Recording & Looping](#recording--looping)
- [Arrangement Control](#arrangement-control)
- [Mixer & Routing](#mixer--routing)
- [System & UI](#system--ui)

---

## Core Session Information

### `get_session_info`
Get detailed information about the current Ableton session including tracks, scenes, and devices.

### `get_application_info`
Get information about the Live Application (LOM Application).

### `get_application_document`
Get a brief summary of the current Live Set via Application.get_document().

### `get_application_version`
Get version details from the Live Application (major/minor/bugfix/version_string).

### `get_application_process_usage`
Get average and peak process usage from the Live Application.

### `get_application_view_state`
Get Application.View properties: browse_mode and focused_document_view.

### `list_control_surfaces`
List control surfaces configured in Live's preferences.

---

## Application View Control

### `application_view_available_main_views`
Return list of available main view names ('Browser', 'Arranger', 'Session', etc.).

### `application_view_focus_view`
Shows named view and focuses on it. Empty string refers to the main window view.

### `application_view_hide_view`
Hides the named view. Empty string refers to the main window view.

### `application_view_is_view_visible`
Returns whether the specified view is currently visible.

### `application_view_scroll_view`
Scroll the specified view.
- `direction`: 0=up, 1=down, 2=left, 3=right
- `modifier_pressed`: Affects Arranger behavior

### `application_view_show_view`
Shows the named view.

### `application_view_toggle_browse`
Displays device chain and browser and toggles Hot-Swap Mode for selected device.

### `application_view_zoom_view`
Zoom the specified view. Only Arrangement and Session can be zoomed.

---

## Transport & Playback Control

### `start_playback`
Start playing the Ableton session.

### `stop_playback`
Stop playing the Ableton session.

### `continue_playing`
Continue playback.

### `play_selection`
Play selection.

### `stop_all_clips`
Stop all Session clips.
- `quantized`: 1 = quantized stop, 0 = stop immediately

### `jump_by`
Jump by specified beats.

### `jump_to_cue`
Jump to specific cue point by index.

### `jump_to_next_cue`
Jump to next cue point.

### `jump_to_prev_cue`
Jump to previous cue point.

### `toggle_cue_at_current`
Toggle cue point at current position.

### `set_song_position`
Set the song's current playback time in the arrangement (in beats).

### `set_current_song_time_beats`
Set Song.current_song_time exactly in beats.

### `get_current_song_time_beats`
Read current song time in beats and formatted bars.beats.sixteenths.ticks.

---

## Track Management

### `get_track_info`
Get detailed information about a specific track by index.

### `create_midi_track`
Create a new MIDI track in the Ableton session.
- `index`: Position to insert track (-1 = end of list)

### `create_audio_track`
Create a new audio track in the Ableton session.
- `index`: Position to insert track (-1 = end of list)

### `set_track_name`
Set the name of a track by index.

---

## Clip Management

### `create_clip`
Create a new MIDI clip in the specified track and clip slot.
- `track_index`: Track index
- `clip_index`: Clip slot index
- `length`: Clip length in beats (default: 4.0)

### `get_clip_info`
Get detailed information about a specific clip.

### `set_clip_name`
Set the name of a clip.

### `add_notes_to_clip`
Add MIDI notes to a clip.
- `notes`: Array of note objects with pitch, start_time, duration, velocity, mute

### `fire_clip`
Start playing a clip.

### `stop_clip`
Stop playing a clip.

### `duplicate_track_clip_to_arrangement`
Duplicate a Session clip to Arrangement at a given beat position.
- `track_index`: Track index
- `clip_index`: Clip slot index
- `start_beats`: Start position in beats
- `length_beats`: Length in beats
- `loop`: Enable looping

---

## Scene Management

### `list_scenes`
Get a list of all scenes in the Ableton session.

### `create_scene`
Create a new scene in the Ableton session.
- `scene_index`: Position to create scene (-1 = end of list)

### `fire_scene`
Fire a scene in the Ableton session.

### `rename_scene`
Rename a scene by index.

---

## Device Control

### `get_device_parameters`
Get a list of parameters for a specific device on a track.

### `get_device_details`
Get detailed information about a specific device on a track.

### `find_device_by_name`
Find the index of a device on a track by its name.

### `set_device_parameter`
Set the value of a device parameter.
- `track_index`: Track index
- `device_index`: Device index on track
- `value`: New parameter value (0.0 to 1.0)
- `parameter_index`: Parameter index (optional)
- `parameter_name`: Parameter name (optional)

### `delete_device`
Delete a device from a track.

### `load_instrument_or_effect`
Load an instrument, effect, or audio file from the browser onto a track using its URI.

### `get_browser_items_at_path`
Get browser items at a specific path in Ableton's browser.

### `get_browser_tree`
Get hierarchical tree of browser categories from Ableton.
- `category_type`: 'all', 'instruments', 'sounds', 'drums', 'audio_effects', 'midi_effects', 'plugins'
- `max_depth`: How many levels of subfolders to explore (default: 2)

### `load_drum_kit`
Load a drum rack and then load a specific drum kit into it.

### `modify_m4l_device_default`
Create a new Max for Live device file with a modified default value for a parameter.

---

## Tempo & Time Signature

### `set_tempo`
Set the tempo of the Ableton session in BPM.

### `set_signature_numerator`
Set the time signature numerator of the song.

### `set_signature_denominator`
Set the time signature denominator of the song.

---

## Automation

### `write_automation`
Write automation points for a device parameter within a clip.
- `track_index`: Track index
- `clip_index`: Clip slot index
- `device_index`: Device index on track
- `points`: Array of automation points with "time" and "value" keys
- `parameter_index`: Parameter index (optional)
- `parameter_name`: Parameter name (optional)

### `re_enable_automation`
Re-enable automation after it was disabled.

---

## Recording & Looping

### `set_record_mode`
Set record mode on/off.

### `set_session_automation_record`
Set session automation recording on/off.

### `trigger_session_record`
Trigger session recording.
- `record_length`: Optional record length in beats

### `set_loop`
Set loop on/off.

### `set_loop_region`
Set loop region.
- `start`: Start position in beats
- `length`: Length in beats

### `set_clip_trigger_quantization`
Set clip trigger quantization.

### `set_arrangement_overdub`
Set arrangement overdub mode on/off.

---

## Arrangement Control

### `create_locator`
Create a locator (cue point) at a specific time in the arrangement.
- `time`: Time in beats

### `list_locators`
Get a list of all locators (cue points) in the Ableton session.

### `rename_cue_point`
Rename a cue point by index.

### `clear_arrangement`
Delete all arrangement clips on specified tracks or all tracks if None.

### `set_back_to_arranger`
Set back to arranger mode on/off.

### `set_start_time`
Set start time in beats.

---

## Mixer & Routing

### `list_return_tracks`
Get a list of all return tracks in the Ableton session.

### `set_send_level`
Set the send level for a track.
- `track_index`: Track index
- `send_index`: Send index (corresponds to return track index)
- `level`: Send level (0.0 to 1.0)

---

## System & UI

### `press_current_dialog_button`
Press a button in the current Live dialog box by index.

### `show_message`
Display a message in Ableton's status bar.

### `set_metronome`
Set metronome on/off.
