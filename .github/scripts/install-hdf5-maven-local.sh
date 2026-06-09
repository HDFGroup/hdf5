#!/usr/bin/env bash
# Install HDF5 Java/Maven build outputs into the local repository (~/.m2).
# Mirrors coordinate rules from .github/workflows/maven-deploy.yml.
#
# Usage:
#   ./install-hdf5-maven-local.sh [--jni-dir DIR] [--ffm-dir DIR] [--ffm-jsrc DIR]
#
# Defaults (repo root = parent of .github):
#   JNI:   build-maven-jni/java/src-jni/hdf/hdf5lib
#   FFM:   build-maven-ffm/java/hdf/hdf5lib
#   jsrc:  build-maven-ffm/java/jsrc
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

JNI_LIB_DIR="${REPO_ROOT}/build-maven-jni/java/src-jni/hdf/hdf5lib"
FFM_LIB_DIR="${REPO_ROOT}/build-maven-ffm/java/hdf/hdf5lib"
FFM_JSRC_DIR="${REPO_ROOT}/build-maven-ffm/java/jsrc"
G="org.hdfgroup"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --jni-dir) JNI_LIB_DIR="$2"; shift 2 ;;
    --ffm-dir) FFM_LIB_DIR="$2"; shift 2 ;;
    --ffm-jsrc) FFM_JSRC_DIR="$2"; shift 2 ;;
    -h|--help)
      sed -n '1,20p' "$0"
      exit 0
      ;;
    *) echo "Unknown option: $1" >&2; exit 2 ;;
  esac
done

pom_version() {
  local pom="$1"
  if [[ ! -f "$pom" ]]; then
    echo "ERROR: missing POM: $pom" >&2
    exit 1
  fi
  grep -o '<version>[^<]*</version>' "$pom" | head -1 | sed 's/<[^>]*>//g'
}

classifier_from_native_jar() {
  local ver="$1"
  local jar="$2"
  # hdf5-native-${ver}-linux-x86_64.jar -> linux-x86_64
  local base
  base=$(basename "$jar" .jar)
  local prefix="hdf5-native-${ver}-"
  if [[ "$base" != "$prefix"* ]]; then
    echo "ERROR: unexpected native JAR name: $jar" >&2
    exit 1
  fi
  echo "${base#"$prefix"}"
}

install_file() {
  echo "[install] $*"
  mvn -q install:install-file "$@"
}

VER=""
CLASS=""

if [[ -f "${JNI_LIB_DIR}/pom.xml" ]]; then
  VER="$(pom_version "${JNI_LIB_DIR}/pom.xml")"
elif [[ -f "${FFM_LIB_DIR}/pom.xml" ]]; then
  VER="$(pom_version "${FFM_LIB_DIR}/pom.xml")"
else
  echo "ERROR: need at least one of:" >&2
  echo "  ${JNI_LIB_DIR}/pom.xml" >&2
  echo "  ${FFM_LIB_DIR}/pom.xml" >&2
  exit 1
fi

# Discover platform classifier from an existing native bundle JAR
NATIVE_JAR=""
if ls "${JNI_LIB_DIR}"/hdf5-native-"${VER}"-*.jar >/dev/null 2>&1; then
  NATIVE_JAR="$(ls "${JNI_LIB_DIR}"/hdf5-native-"${VER}"-*.jar | head -1)"
elif ls "${FFM_LIB_DIR}"/hdf5-native-"${VER}"-*.jar >/dev/null 2>&1; then
  NATIVE_JAR="$(ls "${FFM_LIB_DIR}"/hdf5-native-"${VER}"-*.jar | head -1)"
else
  echo "ERROR: no hdf5-native-${VER}-*.jar under JNI or FFM lib dirs" >&2
  exit 1
fi
CLASS="$(classifier_from_native_jar "$VER" "$NATIVE_JAR")"
echo "HDF5 Maven local install: version=${VER} classifier=${CLASS}"

# 1) hdf5-native (JNI tree, then FFM if present)
if [[ -f "${JNI_LIB_DIR}/pom-hdf5-native.xml" && -f "${JNI_LIB_DIR}/hdf5-native-${VER}-${CLASS}.jar" ]]; then
  install_file \
    -DgroupId="$G" -DartifactId=hdf5-native -Dversion="$VER" \
    -Dpackaging=jar -Dclassifier="$CLASS" \
    -Dfile="${JNI_LIB_DIR}/hdf5-native-${VER}-${CLASS}.jar" \
    -DpomFile="${JNI_LIB_DIR}/pom-hdf5-native.xml"
fi
if [[ -f "${FFM_LIB_DIR}/hdf5-native-${VER}-${CLASS}.jar" && -f "${FFM_LIB_DIR}/pom-hdf5-native.xml" ]]; then
  install_file \
    -DgroupId="$G" -DartifactId=hdf5-native -Dversion="$VER" \
    -Dpackaging=jar -Dclassifier="$CLASS" \
    -Dfile="${FFM_LIB_DIR}/hdf5-native-${VER}-${CLASS}.jar" \
    -DpomFile="${FFM_LIB_DIR}/pom-hdf5-native.xml"
fi

# 2) hdf5-zlib-native (JNI tree, then FFM if present)
if [[ -f "${JNI_LIB_DIR}/pom-hdf5-zlib-native.xml" && -f "${JNI_LIB_DIR}/hdf5-zlib-native-${VER}-${CLASS}.jar" ]]; then
  install_file \
    -DgroupId="$G" -DartifactId=hdf5-zlib-native -Dversion="$VER" \
    -Dpackaging=jar -Dclassifier="$CLASS" \
    -Dfile="${JNI_LIB_DIR}/hdf5-zlib-native-${VER}-${CLASS}.jar" \
    -DpomFile="${JNI_LIB_DIR}/pom-hdf5-zlib-native.xml"
fi
if [[ -f "${FFM_LIB_DIR}/hdf5-zlib-native-${VER}-${CLASS}.jar" && -f "${FFM_LIB_DIR}/pom-hdf5-zlib-native.xml" ]]; then
  install_file \
    -DgroupId="$G" -DartifactId=hdf5-zlib-native -Dversion="$VER" \
    -Dpackaging=jar -Dclassifier="$CLASS" \
    -Dfile="${FFM_LIB_DIR}/hdf5-zlib-native-${VER}-${CLASS}.jar" \
    -DpomFile="${FFM_LIB_DIR}/pom-hdf5-zlib-native.xml"
fi

# 3) hdf5-szip-native (JNI tree, then FFM if present)
if [[ -f "${JNI_LIB_DIR}/pom-hdf5-szip-native.xml" && -f "${JNI_LIB_DIR}/hdf5-szip-native-${VER}-${CLASS}.jar" ]]; then
  install_file \
    -DgroupId="$G" -DartifactId=hdf5-szip-native -Dversion="$VER" \
    -Dpackaging=jar -Dclassifier="$CLASS" \
    -Dfile="${JNI_LIB_DIR}/hdf5-szip-native-${VER}-${CLASS}.jar" \
    -DpomFile="${JNI_LIB_DIR}/pom-hdf5-szip-native.xml"
fi
if [[ -f "${FFM_LIB_DIR}/hdf5-szip-native-${VER}-${CLASS}.jar" && -f "${FFM_LIB_DIR}/pom-hdf5-szip-native.xml" ]]; then
  install_file \
    -DgroupId="$G" -DartifactId=hdf5-szip-native -Dversion="$VER" \
    -Dpackaging=jar -Dclassifier="$CLASS" \
    -Dfile="${FFM_LIB_DIR}/hdf5-szip-native-${VER}-${CLASS}.jar" \
    -DpomFile="${FFM_LIB_DIR}/pom-hdf5-szip-native.xml"
fi

# 4) hdf5-jni-native (JNI only)
if [[ -f "${JNI_LIB_DIR}/pom-hdf5-jni-native.xml" && -f "${JNI_LIB_DIR}/hdf5-jni-native-${VER}-${CLASS}.jar" ]]; then
  install_file \
    -DgroupId="$G" -DartifactId=hdf5-jni-native -Dversion="$VER" \
    -Dpackaging=jar -Dclassifier="$CLASS" \
    -Dfile="${JNI_LIB_DIR}/hdf5-jni-native-${VER}-${CLASS}.jar" \
    -DpomFile="${JNI_LIB_DIR}/pom-hdf5-jni-native.xml"
fi

# 5) javahdf5 (FFM — generated POM)
if [[ -f "${FFM_JSRC_DIR}/javahdf5-${VER}.jar" ]]; then
  install_file \
    -DgroupId="$G" -DartifactId=javahdf5 -Dversion="$VER" \
    -Dpackaging=jar -DgeneratePom=true \
    -Dfile="${FFM_JSRC_DIR}/javahdf5-${VER}.jar"
else
  echo "[skip] FFM bindings JAR not found: ${FFM_JSRC_DIR}/javahdf5-${VER}.jar"
fi

# 6) Main Java API JARs (Maven artifactId differs from file name jarhdf5)
if [[ -f "${JNI_LIB_DIR}/pom.xml" && -f "${JNI_LIB_DIR}/jarhdf5-${VER}-${CLASS}.jar" ]]; then
  install_file \
    -DgroupId="$G" -DartifactId=hdf5-java-jni -Dversion="$VER" \
    -Dpackaging=jar -Dclassifier="$CLASS" \
    -Dfile="${JNI_LIB_DIR}/jarhdf5-${VER}-${CLASS}.jar" \
    -DpomFile="${JNI_LIB_DIR}/pom.xml"
fi

if [[ -f "${FFM_LIB_DIR}/pom.xml" && -f "${FFM_LIB_DIR}/jarhdf5-${VER}-${CLASS}.jar" ]]; then
  install_file \
    -DgroupId="$G" -DartifactId=hdf5-java-ffm -Dversion="$VER" \
    -Dpackaging=jar -Dclassifier="$CLASS" \
    -Dfile="${FFM_LIB_DIR}/jarhdf5-${VER}-${CLASS}.jar" \
    -DpomFile="${FFM_LIB_DIR}/pom.xml"
fi

# 7) Universal jarhdf5 (no classifier) — copy of platform JAR for examples/tests
if [[ -f "${JNI_LIB_DIR}/jarhdf5-${VER}.jar" ]]; then
  install_file \
    -DgroupId="$G" -DartifactId=hdf5-java-jni -Dversion="$VER" \
    -Dpackaging=jar \
    -Dfile="${JNI_LIB_DIR}/jarhdf5-${VER}.jar" \
    -DpomFile="${JNI_LIB_DIR}/pom.xml"
fi
if [[ -f "${FFM_LIB_DIR}/jarhdf5-${VER}.jar" ]]; then
  install_file \
    -DgroupId="$G" -DartifactId=hdf5-java-ffm -Dversion="$VER" \
    -Dpackaging=jar \
    -Dfile="${FFM_LIB_DIR}/jarhdf5-${VER}.jar" \
    -DpomFile="${FFM_LIB_DIR}/pom.xml"
fi

echo "Done. Installed under ~/.m2/repository/org/hdfgroup/"
