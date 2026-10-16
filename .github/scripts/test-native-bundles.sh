#!/usr/bin/env bash
# Run NativeBundleSmokeTest against the Maven artifacts in a directory, with only JARs on the
# classpath and no HDF5 library path set, so libhdf5 and its dependencies must come from the
# hdf5-*-native JARs.
#
# Usage: test-native-bundles.sh <maven-artifacts-dir>
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
ARTIFACTS_DIR="${1:?usage: $0 <maven-artifacts-dir>}"

# Java on Windows needs native paths and ';' as the classpath separator
SEP=":"
to_java_path() { printf '%s' "$1"; }
if command -v cygpath >/dev/null 2>&1; then
  SEP=";"
  to_java_path() { cygpath -m "$1"; }
fi

find_one() {
  find "$ARTIFACTS_DIR" -maxdepth 1 -name "$1" ! -name "*sources*" ! -name "*javadoc*" | head -1
}

HDF5_JAR="$(find_one 'jarhdf5-*.jar')"
NATIVE_JAR="$(find_one 'hdf5-native-*.jar')"
ZLIB_JAR="$(find_one 'hdf5-zlib-native-*.jar')"
SZIP_JAR="$(find_one 'hdf5-szip-native-*.jar')"
JNI_JAR="$(find_one 'hdf5-jni-native-*.jar')"

for required in HDF5_JAR NATIVE_JAR; do
  if [[ -z "${!required}" ]]; then
    echo "ERROR: ${required} not found in ${ARTIFACTS_DIR}" >&2
    ls -la "$ARTIFACTS_DIR" >&2
    exit 1
  fi
done

CP=""
for jar in "$HDF5_JAR" "$NATIVE_JAR" "$ZLIB_JAR" "$SZIP_JAR" "$JNI_JAR" \
           "$REPO_ROOT/java/lib/native-lib-loader-2.5.0.jar" \
           "$REPO_ROOT/java/lib/slf4j-api-2.0.16.jar" \
           "$REPO_ROOT/java/lib/ext/slf4j-simple-2.0.16.jar"; do
  [[ -n "$jar" ]] && CP="${CP:+${CP}${SEP}}$(to_java_path "$jar")"
done

JAVA_OPTS=()
# grep reads the whole listing (no -q) so jar does not die of SIGPIPE under pipefail
if jar tf "$(to_java_path "$HDF5_JAR")" | grep '^org/hdfgroup/javahdf5/' >/dev/null; then
  JAVA_OPTS+=(--enable-native-access=ALL-UNNAMED)
fi
JAVA_OPTS+=(
  "-Dexpect.zlib=$([[ -n "$ZLIB_JAR" ]] && echo true || echo false)"
  "-Dexpect.szip=$([[ -n "$SZIP_JAR" ]] && echo true || echo false)"
  "-Dexpect.jni=$([[ -n "$JNI_JAR" ]] && echo true || echo false)"
)

echo "Java options: ${JAVA_OPTS[*]}"
echo "Classpath:"
tr "$SEP" '\n' <<< "$CP" | sed 's/^/  /'

# Empty working directory so no stray native library is found next to the test
WORK_DIR="$(mktemp -d)"
trap 'rm -rf "$WORK_DIR"' EXIT
cp "$SCRIPT_DIR/NativeBundleSmokeTest.java" "$WORK_DIR/"
cd "$WORK_DIR"

javac -cp "$CP" -d . NativeBundleSmokeTest.java
env -u LD_LIBRARY_PATH -u DYLD_LIBRARY_PATH \
  java "${JAVA_OPTS[@]}" -cp ".${SEP}${CP}" NativeBundleSmokeTest
