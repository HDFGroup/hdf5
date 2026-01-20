#!/bin/bash
#
# HDF5 Version Extraction Utility
#
# This script provides a reusable function to extract version information
# from src/H5public.h by parsing individual version component defines.
#
# Usage:
#   source bin/get_version.sh
#   version=$(get_full_version)
#   echo $version  # e.g., "2.1.0" or "2.1.0-snap0"
#

# Function to extract the full version string from H5public.h
# Parses individual component defines rather than relying on H5_VERS_STR macro
get_full_version() {
    local h5public="${1:-src/H5public.h}"

    # Validate that H5public.h exists
    if [ ! -f "$h5public" ]; then
        echo "ERROR: $h5public not found" >&2
        return 1
    fi

    # Extract numeric components
    local major=$(grep '^#define H5_VERS_MAJOR' "$h5public" | awk '{print $3}')
    local minor=$(grep '^#define H5_VERS_MINOR' "$h5public" | awk '{print $3}')
    local release=$(grep '^#define H5_VERS_RELEASE' "$h5public" | awk '{print $3}')

    # Extract string literal content for subrelease (strips quotes)
    local subrelease=$(grep '^#define H5_VERS_SUBRELEASE' "$h5public" | cut -d'"' -f2)

    # Validate that all required components were extracted
    if [ -z "$major" ] || [ -z "$minor" ] || [ -z "$release" ]; then
        echo "ERROR: Failed to extract version components from $h5public" >&2
        echo "  major=$major, minor=$minor, release=$release" >&2
        return 1
    fi

    # Construct and return the full version string
    echo "${major}.${minor}.${release}${subrelease}"
}

# Function to extract individual version components
# Usage: get_version_component MAJOR|MINOR|RELEASE|SUBRELEASE [path/to/H5public.h]
get_version_component() {
    local component="$1"
    local h5public="${2:-src/H5public.h}"

    if [ ! -f "$h5public" ]; then
        echo "ERROR: $h5public not found" >&2
        return 1
    fi

    case "$component" in
        MAJOR)
            grep '^#define H5_VERS_MAJOR' "$h5public" | awk '{print $3}'
            ;;
        MINOR)
            grep '^#define H5_VERS_MINOR' "$h5public" | awk '{print $3}'
            ;;
        RELEASE)
            grep '^#define H5_VERS_RELEASE' "$h5public" | awk '{print $3}'
            ;;
        SUBRELEASE)
            grep '^#define H5_VERS_SUBRELEASE' "$h5public" | cut -d'"' -f2
            ;;
        *)
            echo "ERROR: Invalid component '$component'. Must be MAJOR, MINOR, RELEASE, or SUBRELEASE" >&2
            return 1
            ;;
    esac
}

# If script is executed directly (not sourced), print version
if [ "${BASH_SOURCE[0]}" = "${0}" ]; then
    get_full_version "$@"
fi
