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
if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    echo "⚠️  No git repository found in current directory"
    echo ""
    echo "To use this script:"
    echo "  1. Navigate to your music project folder"
    echo "  2. Make sure it is a git repo (run 'git init' if needed)"
    echo "  3. Run this script again from that folder"
    exit 1
fi

REPO_ROOT=$(git rev-parse --show-toplevel 2>/dev/null)
CURRENT_DIR=$(pwd -P)
if [ "$CURRENT_DIR" != "$REPO_ROOT" ]; then
    echo "⚠️  Please run this script from the repository root"
    echo ""
    echo "Detected repository root:"
    echo "  $REPO_ROOT"
    echo ""
    echo "Run:"
    echo "  cd \"$REPO_ROOT\""
    echo "  bash \"$0\""
    exit 1
fi

GITATTR_FILE="$REPO_ROOT/.gitattributes"

# Setup .gitattributes
if [ -f "$GITATTR_FILE" ]; then
    if grep -Fqx "*.als diff=alsdiff" "$GITATTR_FILE"; then
        echo "✅ .gitattributes already configured"
    else
        echo "*.als diff=alsdiff" >> "$GITATTR_FILE"
        echo "✅ Added to .gitattributes"
    fi
else
    echo "*.als diff=alsdiff" > "$GITATTR_FILE"
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
git config diff.alsdiff.command "$ALSDIFF_CMD"
echo "✅ Configured git to use: $ALSDIFF_CMD"

git config diff.alsdiff.trustExitCode true
echo "✅ Enabled trustExitCode for alsdiff"

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
    HOOK_FILE=$(git rev-parse --git-path hooks/prepare-commit-msg)
    MARKER_BEGIN="# alsdiff:begin"
    MARKER_END="# alsdiff:end"
    INCLUDE_MESSAGE=0
    INCLUDE_COMMIT=0

    echo ""
    read -p "Include stats for explicit message commits (-m/-F)? (y/N): " include_message
    if [[ "$include_message" =~ ^[Yy]$ ]]; then
        INCLUDE_MESSAGE=1
    fi

    read -p "Include stats for amend/reuse commits (--amend/-c/-C)? (y/N): " include_commit
    if [[ "$include_commit" =~ ^[Yy]$ ]]; then
        INCLUDE_COMMIT=1
    fi

    # Define the hook block
    HOOK_BLOCK='
'"$MARKER_BEGIN"'
# Auto-generate commit message from .als file changes using alsdiff --mode stats
# Installed by: scripts/setup-git.sh
alsdiff_prepare_commit_msg() {
    COMMIT_MSG_FILE="$1"
    COMMIT_SOURCE="${2:-}"
    INCLUDE_MESSAGE='"$INCLUDE_MESSAGE"'
    INCLUDE_COMMIT='"$INCLUDE_COMMIT"'

    # Run only for normal editor/template flows.
    # Skip explicit message commits (-m/-F), merges, squashes, and amend/reuse flows.
    case "$COMMIT_SOURCE" in
        ""|template)
            ;;
        message)
            if [ "$INCLUDE_MESSAGE" -ne 1 ]; then
                return
            fi
            ;;
        commit)
            if [ "$INCLUDE_COMMIT" -ne 1 ]; then
                return
            fi
            ;;
        merge|squash)
            return
            ;;
        *)
            return
            ;;
    esac

    # Create temp directory for blob extraction
    TMPDIR_ALS=$(mktemp -d)
    cleanup_alsdiff_tmpdir() {
        rm -rf "$TMPDIR_ALS"
    }
    trap cleanup_alsdiff_tmpdir 0 HUP INT TERM

    COMMENT_CHAR=$(git config --get core.commentChar 2>/dev/null || printf "")
    if [ -z "$COMMENT_CHAR" ] || [ "$COMMENT_CHAR" = "auto" ]; then
        COMMENT_CHAR="#"
    fi
    STATS_MARKER_BEGIN="${COMMENT_CHAR} alsdiff-stats:begin"
    STATS_MARKER_END="${COMMENT_CHAR} alsdiff-stats:end"

    CLEAN_MSG_FILE="$TMPDIR_ALS/commit_msg_clean"
    awk -v begin="$STATS_MARKER_BEGIN" -v end="$STATS_MARKER_END" "
        \$0 == begin { inblock=1; next }
        \$0 == end { inblock=0; next }
        inblock == 0 { print }
    " "$COMMIT_MSG_FILE" > "$CLEAN_MSG_FILE"

    # Check if alsdiff is available
    if ! command -v alsdiff >/dev/null 2>&1; then
        mv "$CLEAN_MSG_FILE" "$COMMIT_MSG_FILE"
        return
    fi

    # Find staged .als files
    ALS_STATUS=$(git diff --cached --name-status -- "*.als" 2>/dev/null)
    if [ -z "$ALS_STATUS" ]; then
        mv "$CLEAN_MSG_FILE" "$COMMIT_MSG_FILE"
        return
    fi

    STATS_OUTPUT=""
    TAB=$(printf "\t")

    while IFS="$TAB" read -r file_status oldpath newpath; do
        if [ -z "$file_status" ]; then
            continue
        fi
        filename=$(basename "$oldpath")
        case "$file_status" in
            M)
                # Modified: diff HEAD vs staged
                OLD_FILE="$TMPDIR_ALS/old_${filename}"
                NEW_FILE="$TMPDIR_ALS/new_${filename}"
                git show "HEAD:${oldpath}" > "$OLD_FILE" 2>/dev/null || continue
                git show ":${oldpath}" > "$NEW_FILE" 2>/dev/null || continue
                if ! DIFF_STATS=$(alsdiff --mode stats "$OLD_FILE" "$NEW_FILE" 2>&1); then
                    echo "Warning: alsdiff failed for ${oldpath}" >&2
                    if [ -n "$DIFF_STATS" ]; then
                        echo "$DIFF_STATS" | sed "s/^/  /" >&2
                    else
                        echo "  (no error output from alsdiff)" >&2
                    fi
                    continue
                fi
                if [ -n "$DIFF_STATS" ] && [ "$DIFF_STATS" != "No changes." ]; then
                    # Indent each line of stats output
                    INDENTED=$(echo "$DIFF_STATS" | sed "s/^/  /")
                    STATS_OUTPUT="${STATS_OUTPUT}${oldpath}:\n${INDENTED}\n"
                fi
                ;;
            A)
                STATS_OUTPUT="${STATS_OUTPUT}${oldpath}: New file\n"
                ;;
            D)
                STATS_OUTPUT="${STATS_OUTPUT}${oldpath}: Deleted\n"
                ;;
            R*)
                # Renamed: diff HEAD old_path vs staged new_path
                OLD_FILE="$TMPDIR_ALS/old_$(basename "$oldpath")"
                NEW_FILE="$TMPDIR_ALS/new_$(basename "$newpath")"
                git show "HEAD:${oldpath}" > "$OLD_FILE" 2>/dev/null || continue
                git show ":${newpath}" > "$NEW_FILE" 2>/dev/null || continue
                if ! DIFF_STATS=$(alsdiff --mode stats "$OLD_FILE" "$NEW_FILE" 2>&1); then
                    echo "Warning: alsdiff failed for ${oldpath} -> ${newpath}" >&2
                    if [ -n "$DIFF_STATS" ]; then
                        echo "$DIFF_STATS" | sed "s/^/  /" >&2
                    else
                        echo "  (no error output from alsdiff)" >&2
                    fi
                    continue
                fi
                if [ -n "$DIFF_STATS" ] && [ "$DIFF_STATS" != "No changes." ]; then
                    INDENTED=$(echo "$DIFF_STATS" | sed "s/^/  /")
                    STATS_OUTPUT="${STATS_OUTPUT}${oldpath} -> ${newpath}:\n${INDENTED}\n"
                fi
                ;;
            C*)
                # Copied: diff HEAD old_path vs staged new_path
                OLD_FILE="$TMPDIR_ALS/old_$(basename "$oldpath")"
                NEW_FILE="$TMPDIR_ALS/new_$(basename "$newpath")"
                git show "HEAD:${oldpath}" > "$OLD_FILE" 2>/dev/null || continue
                git show ":${newpath}" > "$NEW_FILE" 2>/dev/null || continue
                if ! DIFF_STATS=$(alsdiff --mode stats "$OLD_FILE" "$NEW_FILE" 2>&1); then
                    echo "Warning: alsdiff failed for ${oldpath} -> ${newpath}" >&2
                    if [ -n "$DIFF_STATS" ]; then
                        echo "$DIFF_STATS" | sed "s/^/  /" >&2
                    else
                        echo "  (no error output from alsdiff)" >&2
                    fi
                    continue
                fi
                if [ -n "$DIFF_STATS" ] && [ "$DIFF_STATS" != "No changes." ]; then
                    INDENTED=$(echo "$DIFF_STATS" | sed "s/^/  /")
                    STATS_OUTPUT="${STATS_OUTPUT}${oldpath} -> ${newpath} (copy):\n${INDENTED}\n"
                fi
                ;;
        esac
    done <<EOF
$ALS_STATUS
EOF

    if [ -n "$STATS_OUTPUT" ]; then
        # Prepend generated stats block to commit message.
        TMPFILE="$TMPDIR_ALS/commit_msg"
        {
            printf "%s\n" "$STATS_MARKER_BEGIN"
            printf "%b\n" "$STATS_OUTPUT"
            printf "%s\n\n" "$STATS_MARKER_END"
            cat "$CLEAN_MSG_FILE"
        } > "$TMPFILE"
        mv "$TMPFILE" "$COMMIT_MSG_FILE"
    else
        mv "$CLEAN_MSG_FILE" "$COMMIT_MSG_FILE"
    fi
}
alsdiff_prepare_commit_msg "$@"
'"$MARKER_END"

    if [ -f "$HOOK_FILE" ]; then
        FIRST_LINE=$(head -n 1 "$HOOK_FILE" 2>/dev/null || true)
        if [[ "$FIRST_LINE" == "#!"* ]]; then
            INTERPRETER=$(printf "%s\n" "$FIRST_LINE" | sed -E "s/^#![[:space:]]*//; s/[[:space:]].*$//")
            if [ "$INTERPRETER" = "/usr/bin/env" ]; then
                ENV_TARGET=$(printf "%s\n" "$FIRST_LINE" | sed -E "s/^#![[:space:]]*\/usr\/bin\/env[[:space:]]+//; s/[[:space:]].*$//")
                case "$ENV_TARGET" in
                    sh|bash)
                        ;;
                    *)
                        echo "❌ Existing hook uses unsupported interpreter: $FIRST_LINE"
                        echo "   Only sh/bash hooks are supported for safe auto-update."
                        exit 1
                        ;;
                esac
            else
                case "$INTERPRETER" in
                    */sh|*/bash|sh|bash)
                        ;;
                    *)
                        echo "❌ Existing hook uses unsupported interpreter: $FIRST_LINE"
                        echo "   Only sh/bash hooks are supported for safe auto-update."
                        exit 1
                        ;;
                esac
            fi
        fi
    fi

    if [ -f "$HOOK_FILE" ] && grep -q "$MARKER_BEGIN" "$HOOK_FILE"; then
        TMP_HOOK=$(mktemp)
        awk -v begin="$MARKER_BEGIN" -v end="$MARKER_END" '
            $0 == begin { inblock=1; next }
            $0 == end { inblock=0; next }
            inblock == 0 { print }
        ' "$HOOK_FILE" > "$TMP_HOOK"
        printf "%s\n" "$HOOK_BLOCK" >> "$TMP_HOOK"
        mv "$TMP_HOOK" "$HOOK_FILE"
        echo "✅ Updated prepare-commit-msg hook alsdiff block"
    else
        if [ -f "$HOOK_FILE" ]; then
            # Append to existing hook
            echo "$HOOK_BLOCK" >> "$HOOK_FILE"
            echo "✅ Appended alsdiff block to existing prepare-commit-msg hook"
        else
            # Create new hook
            echo '#!/bin/bash' > "$HOOK_FILE"
            echo "$HOOK_BLOCK" >> "$HOOK_FILE"
            echo "✅ Created prepare-commit-msg hook"
        fi
    fi

    if ! chmod +x "$HOOK_FILE" 2>/dev/null; then
        echo "Error: Failed to make hook executable: $HOOK_FILE" >&2
        exit 1
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
