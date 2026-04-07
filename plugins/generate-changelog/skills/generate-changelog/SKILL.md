---
name: gen-changelog
description: Generate changelogs from git commit history between two tags or commits following the Keep a Changelog format from https://keepachangelog.com/en/1.1.0/. This skill should be used when users need to create structured changelogs for releases based on git commit messages.
---

# Gen-Changelog Skill

Automatically generate changelogs from git commit history following the Keep a Changelog format.

## Overview

This skill reads git commit messages between two specified points (tags or commits) and generates a structured changelog in markdown format. It uses Conventional Commits parsing to categorize changes into standard sections.

## Quick Start

Basic usage:
```
Generate a changelog between v1.0.0 and v2.0.0
```

Or with more specific parameters:
```
Create a changelog from tag v0.3.0 to v0.4.0 and save to CHANGELOG.md
```

## Agent Guidelines

### DO
- Focus on **user-facing changes** - what users care about
- Group related commits under single bullet points
- Preserve original commit message wording where appropriate
- Use Conventional Commits types for categorization
- Include release dates from git tags when available
- Handle non-conventional commits gracefully (put in "Other" or infer from context)
- Link to issues/PRs when referenced in commits
- Preserve breaking change notices from commit bodies
- Use ISO 8601 dates (YYYY-MM-DD)
- Order versions in descending order (newest first)

### DON'T
- Dump raw commit messages without processing
- Include internal refactoring or code cleanup in user-facing changelog
- Create categories for empty sections
- Change commit message wording unless necessary for clarity
- Include merge commits or revert commits in the output
- Use code commits as changelog entries (write user-facing descriptions)
- Add unreleased sections unless explicitly requested

## Generation Process

1. **Extract Parameters**: Parse user input for `--from` and `--to` git refs
2. **Validate Repository**: Verify we're in a git repository
3. **Resolve Refs**: Convert tags/commits to SHA hashes
4. **Fetch Commits**: Get all commits between the refs (exclusive from, inclusive to)
5. **Parse Commits**: Extract hash, subject, body from each commit
6. **Categorize**: Classify commits by type (Added/Changed/Fixed/etc.)
7. **Generate Markdown**: Format output following Keep a Changelog spec
8. **Output**: Write to file or stdout

## Parameter Extraction

Parse the following patterns from user input:

| Pattern | Example | Extracted Values |
|---------|---------|------------------|
| `between X and Y` | "between v1.0.0 and v2.0.0" | from=v1.0.0, to=v2.0.0 |
| `from X to Y` | "from v0.3.0 to v0.4.0" | from=v0.3.0, to=v0.4.0 |
| `X...Y` | "v1.0.0...v2.0.0" | from=v1.0.0, to=v2.0.0 |
| `save to FILE` | "save to CHANGELOG.md" | output=CHANGELOG.md |

Default behavior when `--from` is omitted: use the previous tag
Default behavior when `--to` is omitted: use HEAD

## Categorization Logic

Map Conventional Commits types to Keep a Changelog sections:

| Commit Type | Changelog Section |
|-------------|-------------------|
| `feat:` | Added |
| `fix:` | Fixed |
| `change:` | Changed |
| `deprecate:` | Deprecated |
| `remove:` | Removed |
| `security:` | Security |
| `perf:` | Changed (performance) |
| `refactor:` | Omit (internal only) |
| `test:` | Omit (internal only) |
| `docs:` | Omit unless user-facing |
| `style:` | Omit (internal only) |
| `ci:` | Omit (internal only) |
| `build:` | Omit (internal only) |
| `chore:` | Omit (internal only) |

**Non-conventional commits**: Infer category from message content or place under "Other"

## Error Handling

### Git Repository Errors
- **Not a git repository**: "Error: Not in a git repository. Please run from a valid git directory."
- **Invalid ref**: "Error: Could not resolve git reference '{ref}'"
- **Empty range**: "Error: No commits found between {from} and {to}"

### Tag/Commit Resolution
- **Tag not found**: "Error: Tag '{tag}' does not exist"
- **Ambiguous ref**: "Error: '{ref}' is ambiguous, please use full SHA or branch name"
- **No tags**: Suggest using commit SHAs instead

### Output Errors
- **Cannot write file**: "Error: Cannot write to '{path}': {reason}"
- **Directory not found**: "Error: Directory for '{path}' does not exist"

## Usage Examples

### Example 1: Basic Version Changelog
```
Generate a changelog between v1.0.0 and v1.1.0
```

Output:
```markdown
## [1.1.0] - 2026-04-01

### Added
- New user authentication system
- Export to PDF functionality

### Fixed
- Memory leak in audio processing
- Crash on startup with older configuration files
```

### Example 2: With Output File
```
Create a changelog from v0.3.0 to v0.4.0 and save to RELEASE_NOTES.md
```

### Example 3: Using Commit SHAs
```
Generate changelog from abc123 to def456
```

### Example 4: Current Release
```
Create a changelog from the last tag to HEAD
```

### Example 5: Specific Range
```
What changed between v2.0.0 and v2.1.0?
```

## Output Format

Generated changelogs follow this structure:

```markdown
## [VERSION] - YYYY-MM-DD

### Added
- Feature description

### Changed
- Modification description

### Deprecated
- Deprecation notice

### Removed
- Removal notice

### Fixed
- Bug fix description

### Security
- Security fix description
```

Empty sections are omitted.

## Resources

### Scripts
- `scripts/gen_changelog.py` - Core implementation script

### References
- `references/keep_a_changelog.md` - Keep a Changelog specification
- Conventional Commits: https://www.conventionalcommits.org/
- Git Log Documentation: https://git-scm.com/docs/git-log

## Advanced Usage

### Filtering by Commit Type
```
Generate a changelog for v1.0.0 to v2.0.0, only include features and fixes
```

### Including Internal Changes
```
Create a detailed changelog from v0.1.0 to v0.2.0 including all commits
```

### Custom Output Path
```
Generate changelog between tags and save to docs/releases/v1.0.0.md
```
