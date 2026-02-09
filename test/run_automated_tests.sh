#!/usr/bin/env bash
set -euo pipefail

INPUT_DIR="/workspace/test/automated-tests/input"
OUT_DIR="/workspace/test/automated-tests/output"

RUNNER="/workspace/InCollege"

LIVE_INPUT="/workspace/input/InCollege-Input.txt"
LIVE_OUTPUT="/workspace/output/Incollege-Output.txt"

# Sanity checks (no creation)
[ -d "$INPUT_DIR" ] || { echo "Missing $INPUT_DIR"; exit 1; }
[ -d "$OUT_DIR" ]   || { echo "Missing $OUT_DIR"; exit 1; }
[ -f "$LIVE_INPUT" ] || { echo "Missing $LIVE_INPUT"; exit 1; }
[ -x "$RUNNER" ]     || { echo "Runner not executable: $RUNNER"; exit 1; }

# Backup live input
BACKUP="$(mktemp)"
cp -f "$LIVE_INPUT" "$BACKUP"

# Delete old output
find "$OUT_DIR" -mindepth 1 -type f -delete

# Delete old database
: > /workspace/users.dat
: > /workspace/profiles.dat
: > /workspace/profiles.tmp

cleanup() {
  cp -f "$BACKUP" "$LIVE_INPUT"
  rm -f "$BACKUP"
}
trap cleanup EXIT

# Run tests
while IFS= read -r testfile; do
    base="$(basename "$testfile")"
    name="${base%.txt}"

    echo "=== Running $base ==="

    cp -f "$testfile" "$LIVE_INPUT"

    # Ensure trailing newline
    [[ "$(tail -c 1 "$LIVE_INPUT")" != $'\n' ]] && printf '\n' >> "$LIVE_INPUT"

    ( cd /workspace && "$RUNNER" )


    cp -f "$LIVE_OUTPUT" "$OUT_DIR/${name}-output.txt"
    echo
done < <(
    ls "$INPUT_DIR"/*.txt | sort -t'-' -k1,1n
)

echo "All automated tests completed."
