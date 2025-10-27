#!/usr/bin/env bash
#
# catpublic.sh : print only the PUBLIC section of one or more files
# For each file, emit everything from the start of the file up to (but not
# including) the first line containing the marker:
#   //// END OF PUBLIC
#
# Output is wrapped with:
#   === BEGIN FILE: <path> ===
#   <public content>
#   === END FILE: <path> ===
#
# If a file contains no marker, the entire file is printed.
# Nonexistent inputs are warned about on stderr.

# Usage
if [ "$#" -eq 0 ]; then
  echo "Usage: $0 file1 [file2 ...]"
  exit 1
fi

for file in "$@"; do
  if [ -f "$file" ]; then
    echo "=== BEGIN PUBLIC SECTION OF FILE: $file ==="
    # Print lines until the marker appears; exclude the marker line itself.
    # Using awk index() avoids regex escaping of the slashes.
    awk 'index($0,"//// END_OF_PUBLIC"){exit} {print}' "$file"
    echo "=== END PUBLIC SECTION FOR FILE: $file ==="
    echo
  else
    echo "Warning: '$file' is not a valid file" >&2
  fi
done