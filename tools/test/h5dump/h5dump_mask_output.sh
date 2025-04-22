#!/bin/bash

# The purpose of this script is to mask certain numeric values in h5dump output files,
# to allow tests to ignore values that cannot be guaranteed to be consistent across
# multiple generations of the same file (e.g. offset values within the file layout)

# Usage: h5dump_filter <filename>

h5dump_filter() {
    local result_file="$1"
    local mask_string="XXX"

    if [ ! -f "$result_file" ]; then
        echo "Error: File '$result_file' not found."
        return 1
    fi

    local temp_file=$(mktemp)
    
    # Replace numbers after OFFSET with XXX
    # (only when OFFSET is preceded by whitespace from line start)
    sed -E "s/(^)([[:space:]]*OFFSET[[:space:]]+)[0-9]+/\1\2$mask_string/g" "$result_file" > "$temp_file"

    # Use the temp file as input for the next operation
    mv "$temp_file" "$result_file"

    # Replace numbers in DATATYPE references with XXX
    # (only when DATATYPE is preceded by whitespace from line start)
    sed -E "s/(^)([[:space:]]*DATATYPE[[:space:]]+\"#)[0-9]+(\".*)/\1\2$mask_string\3/g" "$result_file" > "$temp_file"

    mv "$temp_file" "$result_file"
    
    echo "File '$result_file' has been processed."
}

# Parse command line arguments
if [ $# -eq 0 ]; then
    echo "Usage: $0 <filename>"
    exit 1
fi

h5dump_filter "$1"
