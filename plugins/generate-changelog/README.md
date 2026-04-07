# Gen-Changelog Plugin

A Claude Code skill for automatically generating changelogs from git commit history following the [Keep a Changelog](https://keepachangelog.com/) format.

## Features

- **Automatic Categorization**: Uses Conventional Commits parsing to categorize changes
- **Keep a Changelog Format**: Follows industry-standard changelog format
- **Flexible Input**: Accepts git tags, commit SHAs, or branch names
- **Smart Filtering**: Automatically excludes internal commits (refactor, test, chore, etc.)
- **Breaking Change Detection**: Identifies and highlights breaking changes
- **Multiple Output Options**: Write to file or print to stdout

## Installation

This plugin is installed as part of the Claude Code skills system. Place it in your `plugins/` directory:

```
plugins/
└── gen-changelog/
    └── .claude-plugin/
        └── plugin.json
    └── skills/
        └── gen-changelog/
            └── SKILL.md
```

## Usage

### Via Claude Code

The skill integrates with Claude Code's natural language interface:

```
Generate a changelog between v1.0.0 and v2.0.0
```

```
Create a changelog from v0.3.0 to v0.4.0 and save to CHANGELOG.md
```

```
What changed between abc123 and def456?
```

### Direct Script Usage

You can also use the Python script directly:

```bash
# Basic usage
python scripts/gen_changelog.py v1.0.0 v2.0.0

# Save to file
python scripts/gen_changelog.py v1.0.0 v2.0.0 --output CHANGELOG.md

# Specify repository path
python scripts/gen_changelog.py v1.0.0 v2.0.0 --repo /path/to/repo
```

## Conventional Commits Support

The skill recognizes the following commit types and maps them to changelog sections:

| Commit Type | Changelog Section |
|-------------|-------------------|
| `feat:` | Added |
| `fix:` | Fixed |
| `change:` | Changed |
| `deprecate:` | Deprecated |
| `remove:` | Removed |
| `security:` | Security |

Internal commit types are automatically excluded:
- `refactor:` - Code refactoring
- `test:` - Test changes
- `docs:` - Documentation changes
- `style:` - Code style changes
- `ci:` - CI configuration
- `build:` - Build system changes
- `chore:` - Maintenance tasks

## Breaking Changes

Breaking changes are automatically detected and formatted:

```markdown
### Changed
- **BREAKING:** The `API.connect()` method now requires authentication
```

Breaking changes are identified by:
- `!` after the type/scope: `feat(api)!: remove legacy endpoint`
- `BREAKING CHANGE:` footer in commit body

## Output Format

Generated changelogs follow Keep a Changelog format:

```markdown
## [1.2.3] - 2026-04-01

### Added
- New user authentication system
- Export to PDF functionality

### Changed
- Improved performance of audio processing

### Fixed
- Memory leak in audio processing
- Crash on startup with older configuration files
```

## Parameters

| Parameter | Description | Required | Default |
|-----------|-------------|----------|---------|
| `from_ref` | Starting git ref (exclusive) | Yes | - |
| `to_ref` | Ending git ref (inclusive) | Yes | - |
| `--output` / `-o` | Output file path | No | stdout |
| `--repo` / `-r` | Repository path | No | Current directory |

## Examples

### Example 1: Version Release

```
Generate a changelog between v1.0.0 and v1.1.0
```

Output:
```markdown
## [1.1.0] - 2026-04-01

### Added
- user authentication system
- export to PDF functionality

### Fixed
- memory leak in audio processing
- crash on startup with older configuration files
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

## Error Handling

The skill provides clear error messages for common issues:

- **Not a git repository**: "Error: Not in a git repository. Please run from a valid git directory."
- **Invalid ref**: "Error: Could not resolve git reference '{ref}'"
- **Empty range**: "Error: No commits found between {from} and {to}"
- **Cannot write file**: "Error: Cannot write to '{path}': {reason}"

## Requirements

- Python 3.8+
- Git installed and accessible in PATH
- Git repository with conventional commit messages (recommended)

## References

- [Keep a Changelog](https://keepachangelog.com/en/1.1.0/)
- [Conventional Commits](https://www.conventionalcommits.org/)
- [Semantic Versioning](https://semver.org/)

## License

This plugin is part of the alsdiff project and follows the same license terms.
