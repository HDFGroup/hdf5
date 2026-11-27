#!/usr/bin/env bash
# Verify vendored JARs under java/lib/ against committed SHA-256 checksums (offline).
# See java/lib/NOTICES.txt for details.
#
# Manifest: java/lib/vendored-jars.sha256 (GNU sha256sum format, paths relative to repo root).
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
ROOT="${1:-$PROJECT_ROOT}"

MANIFEST_REL="java/lib/vendored-jars.sha256"
MANIFEST="${ROOT}/${MANIFEST_REL}"

if [[ ! -f "$MANIFEST" ]]; then
  echo "ERROR: checksum manifest missing: ${MANIFEST_REL}" >&2
  exit 1
fi

cd "$ROOT"
if command -v sha256sum >/dev/null 2>&1; then
  sha256sum -c "$MANIFEST_REL"
elif command -v shasum >/dev/null 2>&1; then
  shasum -a 256 -c "$MANIFEST_REL"
else
  echo "ERROR: need sha256sum or shasum for offline verification" >&2
  exit 1
fi

echo "All vendored java/lib JARs match committed SHA-256 checksums."
