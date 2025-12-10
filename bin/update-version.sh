#!/bin/sh
#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the LICENSE file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#

# A script to regenerate all version-templated files from VERSION.txt
#
# This script reads VERSION.txt and regenerates all files that contain
# version information (H5public.h, README.md, CHANGELOG.md, Java files, etc.)
#
# Usage:
#   1. Edit VERSION.txt to set the new version
#   2. Run this script: ./bin/update-version.sh
#   3. Review changes: git diff
#   4. Commit if correct: git add VERSION.txt <changed files> && git commit

set -e

# Determine script and repository root directories
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
HDF5_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "=========================================="
echo "HDF5 Version File Regeneration Script"
echo "=========================================="
echo ""

# Change to repository root
cd "$HDF5_ROOT"

# Check that VERSION.txt exists
if [ ! -f "VERSION.txt" ]; then
    echo "ERROR: VERSION.txt not found in $HDF5_ROOT"
    exit 1
fi

# Read and display current version
echo "Reading version from VERSION.txt..."
MAJOR=$(grep "^MAJOR=" VERSION.txt | cut -d= -f2)
MINOR=$(grep "^MINOR=" VERSION.txt | cut -d= -f2)
RELEASE=$(grep "^RELEASE=" VERSION.txt | cut -d= -f2)
SUBRELEASE=$(grep "^SUBRELEASE=" VERSION.txt | cut -d= -f2)

if [ -z "$SUBRELEASE" ]; then
    VERSION_STR="${MAJOR}.${MINOR}.${RELEASE}"
else
    VERSION_STR="${MAJOR}.${MINOR}.${RELEASE}-${SUBRELEASE}"
fi

echo "  Version: ${VERSION_STR}"
echo "  Components: MAJOR=${MAJOR}, MINOR=${MINOR}, RELEASE=${RELEASE}, SUBRELEASE=${SUBRELEASE}"
echo ""

# Create a temporary build directory
BUILD_DIR=$(mktemp -d "${TMPDIR:-/tmp}/hdf5-version-update.XXXXXX")
echo "Creating temporary build directory: $BUILD_DIR"

# Ensure cleanup on exit
cleanup() {
    if [ -n "$BUILD_DIR" ] && [ -d "$BUILD_DIR" ]; then
        echo "Cleaning up temporary directory..."
        rm -rf "$BUILD_DIR"
    fi
}
trap cleanup EXIT INT TERM

# Run CMake to regenerate version files
echo ""
echo "Running CMake to regenerate version-templated files..."
echo "(This may take a moment...)"
echo ""

# Run CMake with version regeneration enabled
# We only need to configure, not build
if cmake -S "$HDF5_ROOT" -B "$BUILD_DIR" \
    -DHDF5_REGENERATE_VERSION_FILES=ON \
    -DHDF5_BUILD_JAVA=ON \
    > "$BUILD_DIR/cmake_output.log" 2>&1; then
    echo "✓ CMake configuration successful"
else
    echo "✗ CMake configuration failed. Check log:"
    cat "$BUILD_DIR/cmake_output.log"
    exit 1
fi

# List files that were changed
echo ""
echo "=========================================="
echo "Version files have been regenerated"
echo "=========================================="
echo ""
echo "The following files have been updated:"
echo ""

# Use git to show which files changed (if in a git repo)
if git rev-parse --git-dir > /dev/null 2>&1; then
    # Show changed files with statistics
    if git diff --stat VERSION.txt \
        src/H5public.h \
        README.md \
        release_docs/CHANGELOG.md \
        config/cmake/scripts/HDF5config.cmake \
        config/examples/HDF5AsSubdirMacros.cmake \
        java/hdf/hdf5lib/H5.java \
        java/test/TestH5.java \
        java/src-jni/hdf/hdf5lib/H5.java \
        java/src-jni/test/TestH5.java 2>/dev/null; then
        echo ""
    else
        echo "  (No changes detected - version may already be up to date)"
        echo ""
    fi

    echo "Review changes with:"
    echo "  git diff"
    echo ""
    echo "If changes look correct, commit them with:"
    echo "  git add VERSION.txt src/H5public.h README.md release_docs/CHANGELOG.md \\"
    echo "          config/cmake/scripts/HDF5config.cmake \\"
    echo "          config/examples/HDF5AsSubdirMacros.cmake \\"
    echo "          java/hdf/hdf5lib/H5.java java/test/TestH5.java \\"
    echo "          java/src-jni/hdf/hdf5lib/H5.java java/src-jni/test/TestH5.java"
    echo "  git commit -m \"Update version to ${VERSION_STR}\""
else
    echo "  src/H5public.h"
    echo "  README.md"
    echo "  release_docs/CHANGELOG.md"
    echo "  config/cmake/scripts/HDF5config.cmake"
    echo "  config/examples/HDF5AsSubdirMacros.cmake"
    echo "  java/hdf/hdf5lib/H5.java"
    echo "  java/test/TestH5.java"
    echo "  java/src-jni/hdf/hdf5lib/H5.java"
    echo "  java/src-jni/test/TestH5.java"
    echo ""
    echo "Review and commit these files as needed."
fi

echo ""
echo "Done!"
