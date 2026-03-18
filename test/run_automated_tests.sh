#!/usr/bin/env bash
set -euo pipefail

# Change to the directory where this script is located
cd "$(dirname "${BASH_SOURCE[0]}")"

INPUT_DIR="./automated-tests/input"
OUT_DIR="./automated-tests/output"

LIVE_INPUT="../input/InCollege-Input.txt"
LIVE_OUTPUT="../output/InCollege-Output.txt"

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

for f in ../users.dat ../profiles.dat \
          ../connections.dat ../connections_temp.dat ../jobs.dat ../applications.dat "$LIVE_OUTPUT"; do
    : > "$f"
done

# Delete old test outputs
rm -f "$OUT_DIR"/*.txt

# Build once
echo "=== Building ==="
( cobc -x -free -I../src ../src/InCollege.cob -o ../InCollege )

# Run tests
while IFS= read -r testfile; do
    base="$(basename "$testfile")"
    name="${base%.txt}"

    echo "=== Running $base ==="

    cp -f "$testfile" "$LIVE_INPUT"

    # Ensure trailing newline
    [[ "$(tail -c 1 "$LIVE_INPUT")" != $'\n' ]] && printf '\n' >> "$LIVE_INPUT"

    : > "$LIVE_OUTPUT"

    ( cd .. && ./InCollege ) || true

    cp -f "$LIVE_OUTPUT" "$OUT_DIR/${name}-output.txt"
    echo
done < <(
    ls "$INPUT_DIR"/*.txt | sort -t'-' -k1,1n
)

echo "All automated tests completed."