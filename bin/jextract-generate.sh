#!/bin/bash
# jextract-generate.sh
# Cross-platform jextract wrapper for generating HDF5 FFM bindings
#
# Usage: jextract-generate.sh <hdf5_install_dir> <output_dir> <jextract_home>
#
# Arguments:
#   hdf5_install_dir - Directory containing HDF5 headers and libraries
#   output_dir       - Directory where generated Java sources will be placed
#   jextract_home    - Directory containing jextract installation

set -e

if [ $# -ne 3 ]; then
    echo "Usage: $0 <hdf5_install_dir> <output_dir> <jextract_home>"
    exit 1
fi

HDF5_DIR="$1"
OUTPUT_DIR="$2"
JEXTRACT_HOME="$3"

# Validate inputs
if [ ! -d "$HDF5_DIR" ]; then
    echo "Error: HDF5 directory not found: $HDF5_DIR"
    exit 1
fi

if [ ! -f "$HDF5_DIR/include/hdf5.h" ]; then
    echo "Error: hdf5.h not found in $HDF5_DIR/include/"
    exit 1
fi

if [ ! -x "$JEXTRACT_HOME/bin/jextract" ]; then
    echo "Error: jextract not found or not executable: $JEXTRACT_HOME/bin/jextract"
    exit 1
fi

# Create output directory if it doesn't exist
mkdir -p "$OUTPUT_DIR"

echo "========================================="
echo "Generating FFM bindings with jextract"
echo "========================================="
echo "HDF5 directory: $HDF5_DIR"
echo "Output directory: $OUTPUT_DIR"
echo "Jextract: $JEXTRACT_HOME/bin/jextract"
echo ""

# Run jextract
"$JEXTRACT_HOME/bin/jextract" \
  --include-dir "$HDF5_DIR/include" \
  --output "$OUTPUT_DIR" \
  --target-package org.hdfgroup.javahdf5 \
  --library hdf5 \
  "$HDF5_DIR/include/hdf5.h"

echo ""
echo "Jextract completed successfully!"
echo "Generated files in: $OUTPUT_DIR"
