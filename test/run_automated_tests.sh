#!/usr/bin/env bash
set -euo pipefail

INPUT_DIR="/workspace/test/automated-tests/input"
OUT_DIR="/workspace/test/automated-tests/output"

RUNNER="/workspace/InCollege"

LIVE_INPUT="/workspace/input/InCollege-Input.txt"
LIVE_OUTPUT="/workspace/output/Incollege-Output.txt"

# Sanity checks
[ -d "$INPUT_DIR" ]    || { echo "Missing $INPUT_DIR"; exit 1; }
[ -d "$OUT_DIR" ]      || { echo "Missing $OUT_DIR"; exit 1; }
[ -f "$LIVE_INPUT" ]   || { echo "Missing $LIVE_INPUT"; exit 1; }

# Backup live input and output before resetting
BACKUP="$(mktemp)"
BACKUP_OUT="$(mktemp)"
cp -f "$LIVE_INPUT" "$BACKUP"
cp -f "$LIVE_OUTPUT" "$BACKUP_OUT"

cleanup() {
  cp -f "$BACKUP" "$LIVE_INPUT"
  cp -f "$BACKUP_OUT" "$LIVE_OUTPUT"
  rm -f "$BACKUP" "$BACKUP_OUT"
}
trap cleanup EXIT

# Reset databases once at the start of each script run

for f in /workspace/users.dat /workspace/profiles.dat \
          /workspace/connections.dat /workspace/connections_temp.dat "$LIVE_OUTPUT"; do
    : > "$f"
done

# Delete old test outputs
rm -f "$OUT_DIR"/*.txt

# Build once
echo "=== Building ==="
( cd /workspace && cobc -x -free -I./src src/InCollege.cob -o InCollege )
[ -x "$RUNNER" ] || { echo "Build failed: $RUNNER not found or not executable"; exit 1; }

# Run tests
while IFS= read -r testfile; do
    base="$(basename "$testfile")"
    name="${base%.txt}"

    echo "=== Running $base ==="

    cp -f "$testfile" "$LIVE_INPUT"

    # Ensure trailing newline
    [[ "$(tail -c 1 "$LIVE_INPUT")" != $'\n' ]] && printf '\n' >> "$LIVE_INPUT"

    : > "$LIVE_OUTPUT"

    ( cd /workspace && "$RUNNER" ) || true

    cp -f "$LIVE_OUTPUT" "$OUT_DIR/${name}-output.txt"
    echo
done < <(
    ls "$INPUT_DIR"/*.txt | sort -t'-' -k1,1n
)

echo "All automated tests completed."