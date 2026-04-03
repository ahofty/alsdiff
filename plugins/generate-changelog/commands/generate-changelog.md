---
description: Generate a changelog from git commit history between two refs
allowed-tools: Bash
argument-hint: "[<from>...<to>]"
---

# Generate Changelog Command

Generate a structured changelog from git commit history following the Keep a Changelog format.

## Usage

```
/generate-changelog:generate-changelog v1.0.0...v2.0.0
/generate-changelog:generate-changelog v1.0.0...HEAD
/generate-changelog:generate-changelog
```

## Arguments

The argument follows the pattern `<from>...<to>` (same as git log range):
- **from**: Starting git ref (tag, commit SHA, or branch). Default: previous tag.
- **to**: Ending git ref. Default: HEAD.

If no argument is provided, generates a changelog from the previous tag to HEAD.

## Implementation

Use the `generate-changelog` skill to process the arguments and generate the changelog. Parse the argument string to extract `from` and `to` refs:

1. Split the argument on `...` to get `from` and `to` values
2. If no argument provided, resolve the previous tag for `from` and use HEAD for `to`
3. Invoke the generate-changelog skill with these parameters
