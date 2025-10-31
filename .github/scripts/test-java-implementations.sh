#!/bin/bash
# Test script for validating Java FFM and JNI implementations across different Java versions
# Usage: test-java-implementations.sh [java_version] [implementation] [test_mode]

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"

# Default values
JAVA_VERSION="${1:-24}"
IMPLEMENTATION="${2:-auto}"  # auto, ffm, jni
TEST_MODE="${3:-build}"      # build, maven, full

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Test matrix configuration
declare -A JAVA_VERSIONS=(
    ["11"]="JNI only"
    ["17"]="JNI only"
    ["21"]="JNI only"
    ["24"]="JNI default, FFM optional"
    ["25"]="JNI default, FFM optional"
)

declare -A TEST_PRESETS_FFM=(
    ["build"]="ci-StdShar-GNUC-FFM"
    ["maven"]="ci-MinShar-GNUC-Maven-FFM"
)

declare -A TEST_PRESETS_JNI=(
    ["build"]="ci-StdShar-GNUC"
    ["maven"]="ci-MinShar-GNUC-Maven"
)

# Validate Java version support
validate_java_version() {
    local version=$1
    local impl=$2

    if [[ ! ${JAVA_VERSIONS[$version]+_} ]]; then
        log_error "Unsupported Java version: $version"
        log_info "Supported versions: ${!JAVA_VERSIONS[@]}"
        return 1
    fi

    if [[ $version -lt 25 && "$impl" == "ffm" ]]; then
        log_error "FFM implementation requires Java 25, got Java $version"
        return 1
    fi

    log_info "Java $version validation: ${JAVA_VERSIONS[$version]}"
    return 0
}

# Determine implementation based on Java version and user preference
determine_implementation() {
    local version=$1
    local requested=$2

    case "$requested" in
        "auto")
            # JNI is default for HDF5 2.0, regardless of Java version
            echo "jni"
            ;;
        "ffm")
            if [[ $version -ge 25 ]]; then
                echo "ffm"
            else
                log_error "FFM requires Java 25+, got Java $version"
                return 1
            fi
            ;;
        "jni")
            echo "jni"
            ;;
        *)
            log_error "Invalid implementation: $requested (use auto, ffm, or jni)"
            return 1
            ;;
    esac
}

# Create build directory with unique name
create_build_dir() {
    local impl=$1
    local mode=$2

    BUILD_DIR="${PROJECT_ROOT}/build-test-java${JAVA_VERSION}-${impl}-${mode}"

    if [[ -d "$BUILD_DIR" ]]; then
        log_warning "Removing existing build directory: $BUILD_DIR"
        rm -rf "$BUILD_DIR"
    fi

    mkdir -p "$BUILD_DIR"
    log_info "Created build directory: $BUILD_DIR"
}

# Test basic build configuration
test_build_config() {
    local impl=$1
    local preset_key="build"

    log_info "Testing $impl build configuration..."

    if [[ "$impl" == "ffm" ]]; then
        preset=${TEST_PRESETS_FFM[$preset_key]}
    else
        preset=${TEST_PRESETS_JNI[$preset_key]}
    fi

    log_info "Using preset: $preset"

    cd "$PROJECT_ROOT"

    # Configure with preset
    if ! cmake --preset "$preset" -B "$BUILD_DIR"; then
        log_error "CMake configuration failed for $impl implementation"
        return 1
    fi

    # Verify implementation detection
    # Note: CMake uses HDF5_ENABLE_JNI (not HDF5_ENABLE_FFM)
    # JNI enabled (ON or not set) = JNI implementation
    # JNI disabled (OFF) = FFM implementation
    if [ "$impl" = "jni" ]; then
        # For JNI, verify it's not explicitly disabled
        if grep -q "HDF5_ENABLE_JNI:BOOL=OFF" "$BUILD_DIR/CMakeCache.txt"; then
            log_error "Implementation detection failed - expected JNI but found HDF5_ENABLE_JNI=OFF"
            cat "$BUILD_DIR/CMakeCache.txt" | grep "HDF5_ENABLE_JNI" || true
            return 1
        fi
        log_info "JNI implementation verified (HDF5_ENABLE_JNI not OFF)"
    elif [ "$impl" = "ffm" ]; then
        # For FFM, verify JNI is explicitly disabled
        if ! grep -q "HDF5_ENABLE_JNI:BOOL=OFF" "$BUILD_DIR/CMakeCache.txt"; then
            log_error "Implementation detection failed - expected HDF5_ENABLE_JNI=OFF for FFM"
            cat "$BUILD_DIR/CMakeCache.txt" | grep "HDF5_ENABLE_JNI" || true
            return 1
        fi
        log_info "FFM implementation verified (HDF5_ENABLE_JNI=OFF)"
    fi

    log_success "Build configuration test passed for $impl"
    return 0
}

# Test Maven artifact generation
test_maven_artifacts() {
    local impl=$1
    local preset_key="maven"

    log_info "Testing $impl Maven artifact generation..."

    if [[ "$impl" == "ffm" ]]; then
        preset=${TEST_PRESETS_FFM[$preset_key]}
        expected_artifact="hdf5-java-ffm"
    else
        preset=${TEST_PRESETS_JNI[$preset_key]}
        expected_artifact="hdf5-java-jni"
    fi

    cd "$PROJECT_ROOT"

    # Configure with Maven preset
    if ! cmake --preset "$preset" -B "$BUILD_DIR"; then
        log_error "Maven configuration failed for $impl implementation"
        return 1
    fi

    # Build the project
    if ! cmake --build "$BUILD_DIR" --parallel 4; then
        log_error "Build failed for $impl implementation"
        return 1
    fi

    # Verify artifact generation
    jar_pattern="$BUILD_DIR/java/**/target/${expected_artifact}-*.jar"
    if ! ls $jar_pattern 1> /dev/null 2>&1; then
        log_error "Expected JAR artifact not found: $expected_artifact"
        log_info "Looking for JARs in build directory:"
        find "$BUILD_DIR" -name "*.jar" -type f || true
        return 1
    fi

    # Verify JAR manifest
    jar_file=$(ls $jar_pattern | head -1)
    log_info "Checking JAR manifest: $jar_file"

    if ! unzip -q -c "$jar_file" META-INF/MANIFEST.MF | grep -q "HDF5-Java-Implementation: ${impl^^}"; then
        log_error "JAR manifest missing implementation metadata"
        unzip -q -c "$jar_file" META-INF/MANIFEST.MF || true
        return 1
    fi

    log_success "Maven artifact test passed for $impl"
    return 0
}

# Test POM file generation
test_pom_generation() {
    local impl=$1

    log_info "Testing POM file generation for $impl..."

    if [[ "$impl" == "ffm" ]]; then
        expected_artifact="hdf5-java-ffm"
        expected_desc="Java Foreign Function"
    else
        expected_artifact="hdf5-java-jni"
        expected_desc="Java Native Interface"
    fi

    # Find generated POM file
    pom_file=$(find "$BUILD_DIR" -name "pom.xml" -path "*/java/*" | head -1)

    if [[ ! -f "$pom_file" ]]; then
        log_error "POM file not found for $impl implementation"
        return 1
    fi

    log_info "Checking POM file: $pom_file"

    # Verify artifact ID
    if ! grep -q "<artifactId>$expected_artifact</artifactId>" "$pom_file"; then
        log_error "POM artifact ID incorrect - expected $expected_artifact"
        grep "<artifactId>" "$pom_file" || true
        return 1
    fi

    # Verify description
    if ! grep -q "$expected_desc" "$pom_file"; then
        log_error "POM description missing expected text: $expected_desc"
        grep "<description>" "$pom_file" || true
        return 1
    fi

    log_success "POM generation test passed for $impl"
    return 0
}

# Run comprehensive test suite
run_test_suite() {
    local impl=$1
    local mode=$2

    log_info "Running test suite for Java $JAVA_VERSION with $impl implementation (mode: $mode)"

    case "$mode" in
        "build")
            create_build_dir "$impl" "build"
            test_build_config "$impl"
            ;;
        "maven")
            create_build_dir "$impl" "maven"
            test_maven_artifacts "$impl"
            test_pom_generation "$impl"
            ;;
        "full")
            create_build_dir "$impl" "build"
            test_build_config "$impl"

            create_build_dir "$impl" "maven"
            test_maven_artifacts "$impl"
            test_pom_generation "$impl"
            ;;
        *)
            log_error "Invalid test mode: $mode (use build, maven, or full)"
            return 1
            ;;
    esac
}

# Cleanup function
cleanup() {
    if [[ -n "${BUILD_DIR:-}" && -d "$BUILD_DIR" ]]; then
        log_info "Cleaning up build directory: $BUILD_DIR"
        rm -rf "$BUILD_DIR"
    fi
}

# Main execution
main() {
    log_info "Java Implementation Test Suite"
    log_info "=============================="
    log_info "Java Version: $JAVA_VERSION"
    log_info "Implementation: $IMPLEMENTATION"
    log_info "Test Mode: $TEST_MODE"
    log_info ""

    # Validate inputs
    if ! validate_java_version "$JAVA_VERSION" "$IMPLEMENTATION"; then
        exit 1
    fi

    # Determine actual implementation
    actual_impl=$(determine_implementation "$JAVA_VERSION" "$IMPLEMENTATION")
    if [[ $? -ne 0 ]]; then
        exit 1
    fi

    log_info "Selected implementation: $actual_impl"
    log_info ""

    # Set trap for cleanup
    trap cleanup EXIT

    # Run tests
    if run_test_suite "$actual_impl" "$TEST_MODE"; then
        log_success "All tests passed for Java $JAVA_VERSION with $actual_impl implementation!"
        exit 0
    else
        log_error "Tests failed for Java $JAVA_VERSION with $actual_impl implementation"
        exit 1
    fi
}

# Help function
show_help() {
    cat << EOF
Java Implementation Test Suite

Usage: $0 [java_version] [implementation] [test_mode]

Arguments:
  java_version    Java version to test (11, 17, 21, 24, 25) [default: 24] (25+ required for FFM)
  implementation  Implementation to test (auto, ffm, jni) [default: auto]
  test_mode      Test mode (build, maven, full) [default: build]

Examples:
  $0                          # Test Java 25 with auto implementation (JNI - default)
  $0 25 ffm build            # Test Java 25 with FFM (optional), build only
  $0 11 jni maven            # Test Java 11 with JNI, Maven artifacts
  $0 25 auto full            # Test Java 25 with auto selection (JNI), full suite

Test Modes:
  build   - Basic build configuration test
  maven   - Maven artifact generation and validation
  full    - Both build and Maven tests

Supported Matrix:
EOF

    for version in "${!JAVA_VERSIONS[@]}"; do
        echo "  Java $version: ${JAVA_VERSIONS[$version]}"
    done
}

# Check for help request
if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
    show_help
    exit 0
fi

# Run main function
main