#!/usr/bin/env python3
"""
Generate changelogs from git commit history following Keep a Changelog format.

This script reads git commits between two refs and generates a structured
changelog in markdown format based on the Keep a Changelog specification.
"""

import argparse
import re
import subprocess
import sys
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Tuple


class Commit:
    """Represents a git commit with parsed metadata."""

    def __init__(self, sha: str, subject: str, body: str = ""):
        self.sha = sha
        self.subject = subject.strip()
        self.body = body.strip()
        self.type, self.scope, self.breaking = self._parse_conventional()

    def _parse_conventional(self) -> Tuple[Optional[str], Optional[str], bool]:
        """
        Parse Conventional Commits format.

        Returns: (type, scope, is_breaking)
        """
        # Match conventional commit format: type(scope)!: subject
        pattern = r'^(\w+)(?:\(([^)]+)\))?(!)?:\s*(.+)$'
        match = re.match(pattern, self.subject)

        if match:
            commit_type, scope, breaking, _ = match.groups()
            is_breaking = breaking is not None or self._has_breaking_change()
            return commit_type.lower(), scope, is_breaking

        # Check for breaking change in body
        if self._has_breaking_change():
            return None, None, True

        return None, None, False

    def _has_breaking_change(self) -> bool:
        """Check if commit body contains BREAKING CHANGE footer."""
        return bool(re.search(r'^BREAKING CHANGE:\s*', self.body, re.MULTILINE))

    def __repr__(self):
        return f"Commit(sha={self.sha[:7]}, type={self.type}, subject='{self.subject[:30]}...')"


class ChangelogGenerator:
    """Generates changelogs from git commit history."""

    # Conventional commit types to Keep a Changelog sections
    TYPE_MAPPING = {
        'feat': 'Added',
        'fix': 'Fixed',
        'change': 'Changed',
        'deprecate': 'Deprecated',
        'remove': 'Removed',
        'security': 'Security',
        'perf': 'Changed',  # Performance improvements go under Changed
    }

    # Internal types to exclude from changelog
    INTERNAL_TYPES = {'refactor', 'test', 'docs', 'style', 'ci', 'build', 'chore'}

    def __init__(self, repo_path: str = "."):
        self.repo_path = Path(repo_path).resolve()

    def run_git(self, args: List[str]) -> str:
        """Run a git command and return stdout."""
        cmd = ['git'] + args
        try:
            result = subprocess.run(
                cmd,
                cwd=self.repo_path,
                capture_output=True,
                text=True,
                check=True
            )
            return result.stdout.strip()
        except subprocess.CalledProcessError as e:
            raise RuntimeError(f"Git command failed: {e.stderr}") from e
        except FileNotFoundError:
            raise RuntimeError("Git is not installed or not in PATH") from None

    def validate_repo(self) -> None:
        """Verify we're in a git repository."""
        try:
            self.run_git(['rev-parse', '--git-dir'])
        except RuntimeError:
            raise RuntimeError("Not a git repository") from None

    def resolve_ref(self, ref: str) -> str:
        """Resolve a tag or commit ref to a SHA hash."""
        try:
            return self.run_git(['rev-parse', ref])
        except RuntimeError:
            raise ValueError(f"Could not resolve git reference '{ref}'") from None

    def get_previous_tag(self, from_ref: str) -> Optional[str]:
        """Get the tag immediately preceding the given ref."""
        try:
            # Get all tags, sorted by version, find the one before from_ref
            tags = self.run_git([
                'tag', '--sort=-v:refname'
            ]).split('\n')

            if not tags:
                return None

            # Get the commit date of from_ref
            from_date = self.run_git(['log', '-1', '--format=%ci', from_ref])

            # Find the most recent tag before from_ref
            for tag in reversed(tags):
                try:
                    tag_date = self.run_git(['log', '-1', '--format=%ci', tag])
                    if tag_date < from_date:
                        return tag
                except RuntimeError:
                    continue

            return None
        except RuntimeError:
            return None

    def fetch_commits(self, from_ref: str, to_ref: str) -> List[Commit]:
        """
        Fetch commits between two refs.

        Args:
            from_ref: Starting ref (exclusive)
            to_ref: Ending ref (inclusive)

        Returns:
            List of Commit objects
        """
        # Use git log with format: SHA|subject|body
        format_str = "%H|%s|%b"
        try:
            output = self.run_git([
                'log',
                f'{from_ref}..{to_ref}',
                f'--format={format_str}',
                '--no-merges',
                '--reverse'
            ])
        except RuntimeError as e:
            if "bad revision" in str(e):
                raise ValueError(f"Invalid commit range: {from_ref}..{to_ref}") from e
            raise

        if not output:
            return []

        commits = []
        for line in output.split('\n'):
            if not line:
                continue
            parts = line.split('|', 2)
            if len(parts) == 3:
                sha, subject, body = parts
                commits.append(Commit(sha, subject, body))

        return commits

    def extract_version(self, ref: str) -> Optional[str]:
        """Extract semantic version from a tag ref."""
        # Handle various tag formats: v1.2.3, 1.2.3, release-1.2.3
        patterns = [
            r'^v?(\d+\.\d+\.\d+)$',  # v1.2.3 or 1.2.3
            r'^release-?(\d+\.\d+\.\d+)$',  # release-1.2.3
            r'^v?(\d+\.\d+\.\d+-.+)$',  # v1.2.3-beta.1
        ]

        for pattern in patterns:
            match = re.match(pattern, ref)
            if match:
                return match.group(1)

        return None

    def get_release_date(self, ref: str) -> Optional[str]:
        """Get the date of a tag/ref in ISO 8601 format."""
        try:
            date_str = self.run_git(['log', '-1', '--format=%ci', ref])
            # Convert from "2026-04-01 12:34:56 +0000" to "2026-04-01"
            if date_str:
                dt = datetime.fromisoformat(date_str.replace(' ', 'T'))
                return dt.strftime('%Y-%m-%d')
        except RuntimeError:
            pass
        return None

    def categorize_commits(self, commits: List[Commit]) -> Dict[str, List[str]]:
        """
        Categorize commits into Keep a Changelog sections.

        Returns:
            Dict mapping section names to lists of descriptions
        """
        sections: Dict[str, List[str]] = {
            'Added': [],
            'Changed': [],
            'Deprecated': [],
            'Removed': [],
            'Fixed': [],
            'Security': [],
        }

        for commit in commits:
            # Skip revert commits
            if commit.subject.lower().startswith('revert:'):
                continue

            # Categorize conventional commits
            if commit.type:
                if commit.type in self.INTERNAL_TYPES:
                    continue

                section = self.TYPE_MAPPING.get(commit.type)
                if section:
                    description = self._format_commit_description(commit)
                    sections[section].append(description)

                # Handle breaking changes separately
                if commit.breaking:
                    description = self._format_breaking_change(commit)
                    if 'Changed' not in sections:
                        sections['Changed'] = []
                    sections['Changed'].append(description)
            else:
                # Non-conventional commit - try to infer or skip
                description = self._infer_category(commit)
                if description:
                    sections['Added'].append(description)  # Default to Added

        # Remove empty sections
        return {k: v for k, v in sections.items() if v}

    def _format_commit_description(self, commit: Commit) -> str:
        """Format a commit for the changelog."""
        # Remove conventional commit prefix
        pattern = r'^\w+(\([^)]+\))?!?:\s*'
        description = re.sub(pattern, '', commit.subject)

        # Lowercase first letter for consistency
        if description:
            description = description[0].lower() + description[1:]

        # Add scope if present
        if commit.scope:
            return f"**{commit.scope}:** {description}"
        return description

    def _format_breaking_change(self, commit: Commit) -> str:
        """Format a breaking change notice."""
        # Extract breaking change description from body
        match = re.search(r'^BREAKING CHANGE:\s*(.+)$', commit.body, re.MULTILINE)
        if match:
            return f"**BREAKING:** {match.group(1)}"
        return f"**BREAKING:** {commit.subject}"

    def _infer_category(self, commit: Commit) -> Optional[str]:
        """
        Infer category from non-conventional commit message.

        Returns None if commit should be omitted.
        """
        subject_lower = commit.subject.lower()

        # Skip internal changes
        internal_keywords = [
            'refactor', 'cleanup', 'format', 'lint', 'typo',
            'internal', 'wip', 'work in progress', 'tmp', 'temp'
        ]
        if any(kw in subject_lower for kw in internal_keywords):
            return None

        # Try to infer category from keywords
        if any(kw in subject_lower for kw in ['add', 'new', 'implement', 'introduce']):
            return commit.subject
        if any(kw in subject_lower for kw in ['fix', 'bug', 'issue', 'correct']):
            return commit.subject
        if any(kw in subject_lower for kw in ['change', 'update', 'modify', 'improve']):
            return commit.subject
        if any(kw in subject_lower for kw in ['remove', 'delete', 'deprecate']):
            return commit.subject

        # Default: include as-is for review
        return commit.subject

    def generate(self, from_ref: str, to_ref: str) -> str:
        """
        Generate a changelog between two refs.

        Args:
            from_ref: Starting ref (exclusive)
            to_ref: Ending ref (inclusive)

        Returns:
            Changelog in markdown format
        """
        self.validate_repo()

        # Resolve refs to SHAs
        from_sha = self.resolve_ref(from_ref)
        to_sha = self.resolve_ref(to_ref)

        # Fetch commits
        commits = self.fetch_commits(from_sha, to_sha)

        if not commits:
            return f"# No Changes\n\nNo commits found between {from_ref} and {to_ref}."

        # Categorize commits
        sections = self.categorize_commits(commits)

        # Get version and date
        version = self.extract_version(to_ref) or to_ref[:7]
        date = self.get_release_date(to_ref) or datetime.now().strftime('%Y-%m-%d')

        # Generate markdown
        lines = [f"## [{version}] - {date}", ""]

        for section_name, items in sections.items():
            lines.append(f"### {section_name}")
            for item in items:
                lines.append(f"- {item}")
            lines.append("")

        return '\n'.join(lines)

    def generate_to_file(self, from_ref: str, to_ref: str, output_path: str) -> None:
        """Generate changelog and write to file."""
        changelog = self.generate(from_ref, to_ref)

        output_file = Path(output_path)
        output_file.parent.mkdir(parents=True, exist_ok=True)

        output_file.write_text(changelog)
        print(f"Changelog written to {output_file}")


def main():
    """CLI entry point."""
    parser = argparse.ArgumentParser(
        description="Generate changelogs from git commit history",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s v1.0.0 v2.0.0
  %(prog)s v1.0.0 HEAD --output CHANGELOG.md
  %(prog)s abc123 def456 --repo /path/to/repo
        """
    )
    parser.add_argument(
        'from_ref',
        help='Starting git ref (tag or commit, exclusive)'
    )
    parser.add_argument(
        'to_ref',
        help='Ending git ref (tag or commit, inclusive)'
    )
    parser.add_argument(
        '-o', '--output',
        help='Output file path (default: stdout)'
    )
    parser.add_argument(
        '-r', '--repo',
        default='.',
        help='Repository path (default: current directory)'
    )

    args = parser.parse_args()

    try:
        generator = ChangelogGenerator(args.repo)

        if args.output:
            generator.generate_to_file(args.from_ref, args.to_ref, args.output)
        else:
            changelog = generator.generate(args.from_ref, args.to_ref)
            print(changelog)

    except (RuntimeError, ValueError) as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)
    except KeyboardInterrupt:
        sys.exit(130)


if __name__ == '__main__':
    main()
