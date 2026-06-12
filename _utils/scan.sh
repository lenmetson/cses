#!/usr/bin/env bash
set -euo pipefail

LOG_FILE="ignored_files.log"

# Check inside git repo
if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  echo "Error: Not a git repo." >&2
  exit 1
fi

echo "Logging ignored files..."
git ls-files --ignored --exclude-standard --others > "$LOG_FILE"

if [[ -s "$LOG_FILE" ]]; then
  echo "Ignored files logged to $LOG_FILE"
  git add -f "$LOG_FILE"
else
  echo "No ignored files found."
  rm -f "$LOG_FILE"
  exit 0
fi

echo "Adding .gitkeep to folders containing ignored files..."

# Get unique directories from ignored files
dirs=$(awk '{
  # If file has no /, directory is "."
  if (index($0, "/") == 0) {
    print "."
  } else {
    sub("/[^/]*$", "", $0)
    print $0
  }
}' "$LOG_FILE" | sort -u)

for dir in $dirs; do
  gitkeep_path="$dir/.gitkeep"
  if [[ ! -f "$gitkeep_path" ]]; then
    echo "Adding .gitkeep to $dir"
    touch "$gitkeep_path"
    git add "$gitkeep_path"
  fi
done

echo "Done. Commit and push the changes."
