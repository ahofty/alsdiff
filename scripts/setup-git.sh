#!/bin/bash
# One-line setup for alsdiff with git

set -e

echo "🔧 Setting up alsdiff with Git..."
echo ""

# Check if alsdiff is in PATH
if ! command -v alsdiff &> /dev/null; then
    echo "❌ alsdiff not found in PATH"
    echo ""
    echo "Please install alsdiff first. See the Installation section in README.md"
    echo ""
    echo "Quick install for macOS:"
    echo "  1. Download from https://github.com/krfantasy/alsdiff/releases"
    echo "  2. Run: chmod +x alsdiff-macos-*"
    echo "  3. Run: mkdir -p ~/bin && mv alsdiff-macos-* ~/bin/alsdiff"
    echo "  4. Add to PATH: echo 'export PATH=\"\$HOME/bin:\$PATH\"' >> ~/.zshrc"
    echo "  5. Reload: source ~/.zshrc"
    exit 1
fi

# Check if we're in a git repository
if [ ! -d ".git" ]; then
    echo "⚠️  No git repository found in current directory"
    echo ""
    echo "To use this script:"
    echo "  1. Navigate to your music project folder"
    echo "  2. Make sure it has a .git folder (run 'git init' if needed)"
    echo "  3. Run this script again from that folder"
    exit 1
fi

# Setup .gitattributes
if [ -f ".gitattributes" ]; then
    if grep -q "*.als diff=alsdiff" .gitattributes; then
        echo "✅ .gitattributes already configured"
    else
        echo "*.als diff=alsdiff" >> .gitattributes
        echo "✅ Added to .gitattributes"
    fi
else
    echo "*.als diff=alsdiff" > .gitattributes
    echo "✅ Created .gitattributes"
fi

# Ask user for extra command line arguments
echo ""
echo "🔧 Optional: Add extra command line arguments for alsdiff?"
echo ""
echo "Common options:"
echo "  --preset PRESET        Output preset (compact, composer, full, inline, mixing, quiet, verbose)"
echo "  --config FILE          Load configuration from JSON file"
echo "  --prefix-added PREFIX  Custom prefix for added items (default: '+')"
echo "  --prefix-removed PREFIX Custom prefix for removed items (default: '-')"
echo "  --note-name-style STYLE Note name style (Sharp, Flat)"
echo "  --max-collection-items N Max items to show in collections"
echo ""
read -p "Enter extra arguments (or press Enter to skip): " extra_args

# Build the command with arguments
if [ -z "$extra_args" ]; then
    ALSDIFF_CMD="alsdiff --git"
else
    ALSDIFF_CMD="alsdiff $extra_args --git"
fi

# Configure git diff driver
git config --global diff.alsdiff.command "$ALSDIFF_CMD"
echo "✅ Configured git to use: $ALSDIFF_CMD"

# --- prepare-commit-msg hook ---
echo ""
echo "🔧 Optional: Install a prepare-commit-msg hook?"
echo ""
echo "This hook auto-generates commit message summaries from .als file changes"
echo "using 'alsdiff --mode stats'. When you commit staged .als files, the"
echo "commit message will be pre-filled with a change summary like:"
echo ""
echo "  MyProject.als:"
echo "    Tracks: 1 Added, 3 Modified"
echo "    Devices: 2 Added, 5 Removed"
echo ""
read -p "Install prepare-commit-msg hook? (y/N): " install_hook

if [[ "$install_hook" =~ ^[Yy]$ ]]; then
    HOOK_FILE=".git/hooks/prepare-commit-msg"
    MARKER_BEGIN="# alsdiff:begin"
    MARKER_END="# alsdiff:end"

    # Check if our hook block is already installed
    if [ -f "$HOOK_FILE" ] && grep -q "$MARKER_BEGIN" "$HOOK_FILE"; then
        echo "✅ prepare-commit-msg hook already contains alsdiff block"
    else
        # Define the hook block
        HOOK_BLOCK='
'"$MARKER_BEGIN"'
# Auto-generate commit message from .als file changes using alsdiff --mode stats
# Installed by: scripts/setup-git.sh
alsdiff_prepare_commit_msg() {
    COMMIT_MSG_FILE="$1"
    COMMIT_SOURCE="${2:-}"

    # Skip for merges, squashes, amends, and -m messages
    if [ -n "$COMMIT_SOURCE" ]; then
        return
    fi

    # Check if alsdiff is available
    if ! command -v alsdiff &> /dev/null; then
        return
    fi

    # Find staged .als files
    ALS_STATUS=$(git diff --cached --name-status -- "*.als" 2>/dev/null)
    if [ -z "$ALS_STATUS" ]; then
        return
    fi

    # Create temp directory for blob extraction
    TMPDIR_ALS=$(mktemp -d)
    trap "rm -rf \"$TMPDIR_ALS\"" RETURN

    STATS_OUTPUT=""

    while IFS=$'"'"'\t'"'"' read -r status filepath; do
        filename=$(basename "$filepath")
        case "$status" in
            M)
                # Modified: diff HEAD vs staged
                OLD_FILE="$TMPDIR_ALS/old_${filename}"
                NEW_FILE="$TMPDIR_ALS/new_${filename}"
                git show "HEAD:${filepath}" > "$OLD_FILE" 2>/dev/null || continue
                git show ":${filepath}" > "$NEW_FILE" 2>/dev/null || continue
                DIFF_STATS=$(alsdiff --mode stats "$OLD_FILE" "$NEW_FILE" 2>/dev/null) || true
                if [ -n "$DIFF_STATS" ] && [ "$DIFF_STATS" != "No changes." ]; then
                    # Indent each line of stats output
                    INDENTED=$(echo "$DIFF_STATS" | sed "s/^/  /")
                    STATS_OUTPUT="${STATS_OUTPUT}${filepath}:\n${INDENTED}\n"
                fi
                ;;
            A)
                STATS_OUTPUT="${STATS_OUTPUT}${filepath}: New file\n"
                ;;
            D)
                STATS_OUTPUT="${STATS_OUTPUT}${filepath}: Deleted\n"
                ;;
        esac
    done <<< "$ALS_STATUS"

    if [ -n "$STATS_OUTPUT" ]; then
        # Prepend stats to commit message (before any # comment lines)
        TMPFILE="$TMPDIR_ALS/commit_msg"
        printf "%b\n" "$STATS_OUTPUT" > "$TMPFILE"
        cat "$COMMIT_MSG_FILE" >> "$TMPFILE"
        mv "$TMPFILE" "$COMMIT_MSG_FILE"
    fi
}
alsdiff_prepare_commit_msg "$@"
'"$MARKER_END"

        if [ -f "$HOOK_FILE" ]; then
            # Append to existing hook
            echo "$HOOK_BLOCK" >> "$HOOK_FILE"
            echo "✅ Appended alsdiff block to existing prepare-commit-msg hook"
        else
            # Create new hook
            echo '#!/bin/bash' > "$HOOK_FILE"
            echo "$HOOK_BLOCK" >> "$HOOK_FILE"
            chmod +x "$HOOK_FILE"
            echo "✅ Created prepare-commit-msg hook"
        fi
    fi
else
    echo "⏭️  Skipping prepare-commit-msg hook"
fi

echo ""
echo "🎉 Done! Git configured with:"
echo "   Diff driver: $ALSDIFF_CMD"
if [[ "$install_hook" =~ ^[Yy]$ ]]; then
    echo "   Commit hook: .git/hooks/prepare-commit-msg (alsdiff --mode stats)"
fi
