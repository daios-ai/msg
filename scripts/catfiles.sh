#!/bin/bash

# catfiles.sh : concatenate one or more files
# For each file, emit everything from the start to the end of the file.
#
# Output is wrapped with:
#   === BEGIN FILE: <path> ===
#   <public content>
#   === END FILE: <path> ===
#

# Check if at least one filename is provided
if [ "$#" -eq 0 ]; then
    echo "Usage: $0 file1 [file2 ...]"
    exit 1
fi

# Iterate over each provided filename.
for file in "$@"; do
    if [ -f "$file" ]; then
        echo "=== BEGIN FILE: $file ==="
        cat "$file"
        echo
        echo "=== END FILE: $file ==="
        echo # blank line for readability
    else
        echo "Warning: '$file' is not a valid file" >&2
    fi
done
