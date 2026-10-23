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
JNI_JAR="$(find_one 'hdf5-jni-native-*.jar')"

for required in HDF5_JAR NATIVE_JAR; do
  if [[ -z "${!required}" ]]; then
    echo "ERROR: ${required} not found in ${ARTIFACTS_DIR}" >&2
    ls -la "$ARTIFACTS_DIR" >&2
    exit 1
  fi
done

# Empty working directory so no stray native library is found next to the test
WORK_DIR="$(mktemp -d)"
trap 'rm -rf "$WORK_DIR"' EXIT

RPATH_DIR="$WORK_DIR/rpath-check"
mkdir -p "$RPATH_DIR"
for jar in "$NATIVE_JAR" "$JNI_JAR"; do
  [[ -n "$jar" ]] || continue
  (cd "$RPATH_DIR" && jar xf "$(to_java_path "$jar")")
done

# hdf5-native carries zlib / libaec + libsz when libhdf5 links them as shared libraries
has_bundled() {
  find "$RPATH_DIR/natives" -type f -name "$1" | grep . >/dev/null
}
EXPECT_ZLIB=false
EXPECT_SZIP=false
if has_bundled 'libz.so' || has_bundled 'libz.dylib' || has_bundled 'z.dll' || has_bundled 'zlib*.dll'; then
  EXPECT_ZLIB=true
fi
if has_bundled 'libsz.so' || has_bundled 'libsz.dylib' || has_bundled 'sz.dll' || has_bundled 'szip.dll'; then
  EXPECT_SZIP=true
fi

# Bundled libraries must not keep runtime search paths into the build tree
while IFS= read -r lib; do
  case "$(uname -s)" in
    Linux)
      if command -v readelf >/dev/null 2>&1 && readelf -d "$lib" | grep -E '\((RPATH|RUNPATH)\)'; then
        echo "ERROR: $(basename "$lib") still has RPATH/RUNPATH" >&2
        exit 1
      fi
      ;;
    Darwin)
      if otool -l "$lib" | grep -A2 'cmd LC_RPATH'; then
        echo "ERROR: $(basename "$lib") still has LC_RPATH" >&2
        exit 1
      fi
      ;;
  esac
done < <(find "$RPATH_DIR/natives" -type f \( -name '*.so' -o -name '*.dylib' \))
rm -rf "$RPATH_DIR"

CP=""
for jar in "$HDF5_JAR" "$NATIVE_JAR" "$JNI_JAR" \
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
  "-Dexpect.zlib=${EXPECT_ZLIB}"
  "-Dexpect.szip=${EXPECT_SZIP}"
  "-Dexpect.jni=$([[ -n "$JNI_JAR" ]] && echo true || echo false)"
)

echo "Java options: ${JAVA_OPTS[*]}"
echo "Classpath:"
tr "$SEP" '\n' <<< "$CP" | sed 's/^/  /'

cp "$SCRIPT_DIR/NativeBundleSmokeTest.java" "$WORK_DIR/"
cd "$WORK_DIR"

javac -cp "$CP" -d . NativeBundleSmokeTest.java
env -u LD_LIBRARY_PATH -u DYLD_LIBRARY_PATH \
  java "${JAVA_OPTS[@]}" -cp ".${SEP}${CP}" NativeBundleSmokeTest
