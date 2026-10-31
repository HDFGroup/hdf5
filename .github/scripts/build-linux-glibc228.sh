#!/bin/bash

# Build HDF5 inside a glibc 2.28 container, so that the Linux tarball we
# publish runs on every distribution with glibc 2.28 or newer. Run by the
# "AlmaLinux 8 gcc (glibc 2.28)" job in .github/workflows/ctest.yml, which mounts the
# workspace at /work.
#
# Usage: build-linux-glibc228.sh <source-dir> <preset> <uid> <gid>

set -euo pipefail

SRC_DIR="${1:?usage: $0 <source-dir> <preset> <uid> <gid>}"
PRESET="${2:?usage: $0 <source-dir> <preset> <uid> <gid>}"
OWNER_UID="${3:?usage: $0 <source-dir> <preset> <uid> <gid>}"
OWNER_GID="${4:?usage: $0 <source-dir> <preset> <uid> <gid>}"

# The image already carries gcc-toolset-14 (including gfortran) and CMake.
# AlmaLinux 8's ninja is 1.8.2, which CMake refuses to use for Fortran, so
# take a current one from PyPI, pinned so the release toolchain is
# reproducible. There is no JDK in the image at all.
dnf install -y java-21-openjdk-devel
pipx install ninja==1.13.2
export PATH="${HOME}/.local/bin:${PATH}"

echo "=== Toolchain ==="
cat /etc/os-release | head -2
ldd --version | head -1
gcc --version | head -1
gfortran --version | head -1
cmake --version | head -1
ninja --version

cd "/work/${SRC_DIR}"
cmake --workflow --preset="${PRESET}" --fresh

# The container runs as root, so hand the tree back to the runner's user
chown -R "${OWNER_UID}:${OWNER_GID}" /work
