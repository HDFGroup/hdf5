#!/bin/bash

# Check that HDF5 builds reproducibly: build and install it twice from
# different source and build directories, with a different environment
# (time zone, locale, umask, kernel release, parallelism) and with tests built
# in only one of them, and compare the installed trees: file contents,
# permissions and symlinks.
#
# Usage: ./check-reproducible-build.sh <hdf5-source-dir> [work-dir]
#
# Requires cmake, ninja, gcc, g++, gfortran and setarch.

set -euo pipefail

SRC_DIR=$(cd "${1:?usage: $0 <hdf5-source-dir> [work-dir]}" && pwd)
WORK_DIR="${2:-$(mktemp -d)}"
mkdir -p "${WORK_DIR}"
WORK_DIR=$(cd "${WORK_DIR}" && pwd)
# The sources are copied into the work directory, so it cannot be inside them
if [[ "${WORK_DIR}/" == "${SRC_DIR}/"* ]]; then
    echo "ERROR: the work directory must be outside the source directory" >&2
    exit 1
fi

# The same fixed timestamp for both builds, as distribution build tools do
export SOURCE_DATE_EPOCH="${SOURCE_DATE_EPOCH:-$(git -C "${SRC_DIR}" log -1 --format=%ct 2>/dev/null || echo 1700000000)}"

echo "=== HDF5 reproducible build check ==="
echo "Source: ${SRC_DIR}"
echo "Work directory: ${WORK_DIR}"
echo "SOURCE_DATE_EPOCH: ${SOURCE_DATE_EPOCH}"

# Run a command with its output in a log file, and show the end of the log
# if it fails (Ninja prints compiler errors to stdout)
logged () {
    local log="$1"
    shift
    if ! "$@" > "${log}" 2>&1; then
        echo "FAILED: last lines of ${log}:"
        tail -n 100 "${log}"
        exit 1
    fi
}

build () {
    local n="$1"
    local src="${WORK_DIR}/src-${n}/hdf5"
    local bld="${WORK_DIR}/build-${n}-tree"
    local dest="${WORK_DIR}/install-${n}"
    rm -rf "${WORK_DIR}/src-${n}" "${bld}" "${dest}"
    mkdir -p "${src}" "${bld}"
    tar -C "${SRC_DIR}" --exclude=.git -cf - . | tar -C "${src}" -xf -

    # What distributions pass to keep build paths and the build host out of binaries
    local map="-ffile-prefix-map=${src}=/usr/src/hdf5 -ffile-prefix-map=${bld}=/usr/src/hdf5/build"

    local -a vary
    local jobs
    local testing
    if [ "${n}" = 1 ]; then
        vary=(env TZ=UTC LC_ALL=C)
        umask 022
        jobs=$(nproc)
        # Distributions often skip the tests in one of their builds (Debian's
        # reproducibility check sets nocheck in its second build), so build
        # them here only. This is the build with the most parallel jobs.
        testing=ON
    else
        # Different time zone, locale, kernel release and parallelism
        vary=(env TZ=Asia/Tokyo LC_ALL=C.UTF-8 setarch "$(uname -m)" --uname-2.6)
        umask 002
        jobs=2
        testing=OFF
    fi

    echo ""
    echo "=== Build ${n}: ${src} -> ${bld} ==="
    # Static archives are created in deterministic mode (ar D), as distribution
    # toolchains do by default, so the result does not depend on the host's ar
    logged "${WORK_DIR}/configure-${n}.log" "${vary[@]}" cmake -G Ninja -S "${src}" -B "${bld}" \
        -DCMAKE_BUILD_TYPE=RelWithDebInfo \
        -DCMAKE_INSTALL_PREFIX=/usr \
        "-DCMAKE_C_FLAGS=${map}" \
        "-DCMAKE_CXX_FLAGS=${map}" \
        "-DCMAKE_Fortran_FLAGS=${map}" \
        "-DCMAKE_C_ARCHIVE_CREATE=<CMAKE_AR> qcD <TARGET> <LINK_FLAGS> <OBJECTS>" \
        "-DCMAKE_C_ARCHIVE_APPEND=<CMAKE_AR> qD <TARGET> <LINK_FLAGS> <OBJECTS>" \
        "-DCMAKE_C_ARCHIVE_FINISH=<CMAKE_RANLIB> -D <TARGET>" \
        "-DCMAKE_CXX_ARCHIVE_CREATE=<CMAKE_AR> qcD <TARGET> <LINK_FLAGS> <OBJECTS>" \
        "-DCMAKE_CXX_ARCHIVE_APPEND=<CMAKE_AR> qD <TARGET> <LINK_FLAGS> <OBJECTS>" \
        "-DCMAKE_CXX_ARCHIVE_FINISH=<CMAKE_RANLIB> -D <TARGET>" \
        "-DCMAKE_Fortran_ARCHIVE_CREATE=<CMAKE_AR> qcD <TARGET> <LINK_FLAGS> <OBJECTS>" \
        "-DCMAKE_Fortran_ARCHIVE_APPEND=<CMAKE_AR> qD <TARGET> <LINK_FLAGS> <OBJECTS>" \
        "-DCMAKE_Fortran_ARCHIVE_FINISH=<CMAKE_RANLIB> -D <TARGET>" \
        -DHDF5_BUILD_HOST_SYSTEM=Linux \
        -DBUILD_SHARED_LIBS=ON \
        -DBUILD_STATIC_LIBS=ON \
        -DHDF5_BUILD_HL_LIB=ON \
        -DHDF5_BUILD_CPP_LIB=ON \
        -DHDF5_BUILD_FORTRAN=ON \
        -DHDF5_BUILD_TOOLS=ON \
        -DHDF5_ENABLE_ZLIB_SUPPORT=ON \
        -DBUILD_TESTING="${testing}" \
        -DHDF5_BUILD_EXAMPLES=OFF
    logged "${WORK_DIR}/build-${n}.log" "${vary[@]}" cmake --build "${bld}" --parallel "${jobs}"
    # DESTDIR goes first: vary ends with setarch, which runs its first operand
    logged "${WORK_DIR}/install-${n}.log" env DESTDIR="${dest}" "${vary[@]}" cmake --install "${bld}"
    umask 022
}

build 1
build 2

echo ""
echo "=== Comparing installed files ==="
status=0
nfiles=0
ndiff=0

# Every installed entry (files, directories and symlinks) with its type,
# permissions and symlink target, so that missing entries, permission
# differences (the builds use different umasks) and differing symlinks are found
manifest () {
    (cd "$1" && find . -mindepth 1 -printf '%P\t%y %m %l\n' | LC_ALL=C sort)
}
manifest "${WORK_DIR}/install-1" > "${WORK_DIR}/install-1.manifest"
manifest "${WORK_DIR}/install-2" > "${WORK_DIR}/install-2.manifest"
if ! diff "${WORK_DIR}/install-1.manifest" "${WORK_DIR}/install-2.manifest" > "${WORK_DIR}/manifest.diff"; then
    echo "Entries that are missing or differ in type, permissions or symlink target"
    echo "(< build 1, > build 2; columns: path, type, mode, symlink target):"
    grep '^[<>]' "${WORK_DIR}/manifest.diff" | head -50
    ndiff=$((ndiff + $(sed -n 's/^[<>] //p' "${WORK_DIR}/manifest.diff" | cut -f1 | sort -u | wc -l)))
fi

# Contents of the regular files installed by both builds
while IFS= read -r -d '' rel; do
    nfiles=$((nfiles + 1))
    f="${WORK_DIR}/install-1/${rel}"
    other="${WORK_DIR}/install-2/${rel}"
    if [ -f "${other}" ] && ! cmp -s "${f}" "${other}"; then
        echo "DIFFERS: ${rel} ($(cmp -l "${f}" "${other}" | wc -l) bytes)"
        diff <(strings -n 6 "${f}") <(strings -n 6 "${other}") | head -6 || true
        ndiff=$((ndiff + 1))
    fi
done < <(cd "${WORK_DIR}/install-1" && find . -type f -printf '%P\0')

echo "Installed files: ${nfiles}, differing entries: ${ndiff}"
if [ "${nfiles}" -eq 0 ]; then
    status=1
    echo "FAILED: build 1 installed no files"
elif [ "${ndiff}" -ne 0 ]; then
    status=1
    echo "FAILED: the two builds are not identical"
else
    echo "PASSED: the two builds are identical"
fi
exit "${status}"
