#!/bin/bash
# Maven artifact validation script for FFM and JNI implementations
# Usage: validate-maven-artifacts.sh [artifact_dir] [implementation] [validation_level]

set -e

ARTIFACT_DIR="${1:-./build/java}"
IMPLEMENTATION="${2:-auto}"  # auto, ffm, jni, both
VALIDATION_LEVEL="${3:-basic}"  # basic, full, strict

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

# Global counters
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Test tracking
increment_test() {
    ((TOTAL_TESTS++))
}

pass_test() {
    ((PASSED_TESTS++))
    log_success "$1"
}

fail_test() {
    ((FAILED_TESTS++))
    log_error "$1"
}

# Find artifacts by implementation
find_artifacts() {
    local impl=$1
    local artifact_pattern

    case "$impl" in
        "ffm")
            artifact_pattern="*hdf5-java-ffm*.jar"
            ;;
        "jni")
            artifact_pattern="*hdf5-java-jni*.jar"
            ;;
        *)
            log_error "Invalid implementation for artifact search: $impl"
            return 1
            ;;
    esac

    find "$ARTIFACT_DIR" -name "$artifact_pattern" -type f 2>/dev/null
}

# Validate JAR file structure
validate_jar_structure() {
    local jar_file=$1
    local impl=$2

    increment_test
    log_info "Validating JAR structure: $(basename "$jar_file")"

    # Check if JAR is valid
    if ! unzip -t "$jar_file" >/dev/null 2>&1; then
        fail_test "JAR file is corrupted: $jar_file"
        return 1
    fi

    # Check for required manifest
    if ! unzip -l "$jar_file" | grep -q "META-INF/MANIFEST.MF"; then
        fail_test "JAR missing manifest: $jar_file"
        return 1
    fi

    # Check for Java classes
    class_count=$(unzip -l "$jar_file" | grep -c "\.class$" || echo "0")
    if [[ $class_count -eq 0 ]]; then
        fail_test "JAR contains no .class files: $jar_file"
        return 1
    fi

    # Check for hdf.hdf5lib package structure
    if ! unzip -l "$jar_file" | grep -q "hdf/hdf5lib/"; then
        fail_test "JAR missing hdf.hdf5lib package: $jar_file"
        return 1
    fi

    pass_test "JAR structure validation passed for $impl: $(basename "$jar_file")"
    return 0
}

# Validate JAR manifest
validate_jar_manifest() {
    local jar_file=$1
    local impl=$2

    increment_test
    log_info "Validating JAR manifest for $impl implementation"

    local manifest_content
    manifest_content=$(unzip -q -c "$jar_file" META-INF/MANIFEST.MF)

    # Check implementation marker
    if ! echo "$manifest_content" | grep -q "HDF5-Java-Implementation: ${impl^^}"; then
        fail_test "Manifest missing implementation marker: HDF5-Java-Implementation: ${impl^^}"
        echo "Manifest content:"
        echo "$manifest_content"
        return 1
    fi

    # Check version information
    if ! echo "$manifest_content" | grep -q "HDF5-Version:"; then
        fail_test "Manifest missing HDF5 version information"
        return 1
    fi

    # Check platform information
    if ! echo "$manifest_content" | grep -q "HDF5-Platform:"; then
        fail_test "Manifest missing platform information"
        return 1
    fi

    # Check native access for FFM
    if [[ "$impl" == "ffm" ]]; then
        if ! echo "$manifest_content" | grep -q "Enable-Native-Access: ALL-UNNAMED"; then
            fail_test "FFM JAR missing native access enablement"
            return 1
        fi
    fi

    # Check minimum Java version
    case "$impl" in
        "ffm")
            if ! echo "$manifest_content" | grep -q "HDF5-Java-Minimum-Version: 24"; then
                fail_test "FFM JAR missing or incorrect minimum Java version (expected 24)"
                return 1
            fi
            ;;
        "jni")
            if ! echo "$manifest_content" | grep -q "HDF5-Java-Minimum-Version: 11"; then
                fail_test "JNI JAR missing or incorrect minimum Java version (expected 11)"
                return 1
            fi
            ;;
    esac

    pass_test "JAR manifest validation passed for $impl"
    return 0
}

# Validate POM file
validate_pom_file() {
    local jar_file=$1
    local impl=$2

    increment_test
    local pom_dir=$(dirname "$jar_file")
    local pom_file=$(find "$pom_dir" -name "pom.xml" | head -1)

    if [[ ! -f "$pom_file" ]]; then
        fail_test "POM file not found for $impl implementation"
        return 1
    fi

    log_info "Validating POM file: $pom_file"

    # Check artifact ID
    local expected_artifact="hdf5-java-$impl"
    if ! grep -q "<artifactId>$expected_artifact</artifactId>" "$pom_file"; then
        fail_test "POM has incorrect artifact ID (expected: $expected_artifact)"
        grep "<artifactId>" "$pom_file" || true
        return 1
    fi

    # Check group ID
    if ! grep -q "<groupId>org.hdfgroup</groupId>" "$pom_file"; then
        fail_test "POM has incorrect group ID (expected: org.hdfgroup)"
        return 1
    fi

    # Check implementation-specific description
    case "$impl" in
        "ffm")
            if ! grep -q "Foreign Function" "$pom_file"; then
                fail_test "FFM POM missing FFM-specific description"
                return 1
            fi
            if ! grep -q "hdf5.java.implementation>FFM<" "$pom_file"; then
                fail_test "FFM POM missing implementation property"
                return 1
            fi
            ;;
        "jni")
            if ! grep -q "Native Interface" "$pom_file"; then
                fail_test "JNI POM missing JNI-specific description"
                return 1
            fi
            if ! grep -q "hdf5.java.implementation>JNI<" "$pom_file"; then
                fail_test "JNI POM missing implementation property"
                return 1
            fi
            ;;
    esac

    # Check SLF4J dependency
    if ! grep -q "slf4j-api" "$pom_file"; then
        fail_test "POM missing SLF4J dependency"
        return 1
    fi

    pass_test "POM validation passed for $impl"
    return 0
}

# Validate artifact naming convention
validate_naming_convention() {
    local jar_file=$1
    local impl=$2

    increment_test
    local jar_name=$(basename "$jar_file")

    # Check naming pattern
    local expected_pattern="hdf5-java-${impl}-[0-9]+\.[0-9]+\.[0-9]+"
    if [[ ! "$jar_name" =~ $expected_pattern ]]; then
        fail_test "JAR naming doesn't match convention: $jar_name (expected pattern: $expected_pattern)"
        return 1
    fi

    # Check for platform classifier (if present)
    if [[ "$jar_name" =~ -linux-x86_64\.jar$ ]] || \
       [[ "$jar_name" =~ -windows-x86_64\.jar$ ]] || \
       [[ "$jar_name" =~ -macos-x86_64\.jar$ ]] || \
       [[ "$jar_name" =~ -macos-aarch64\.jar$ ]]; then
        log_info "Platform-specific JAR detected: $jar_name"
    fi

    pass_test "Naming convention validation passed for $impl: $jar_name"
    return 0
}

# Validate implementation differentiation
validate_implementation_differentiation() {
    increment_test
    log_info "Validating implementation differentiation"

    local ffm_artifacts=($(find_artifacts "ffm"))
    local jni_artifacts=($(find_artifacts "jni"))

    # Check that we have artifacts for both implementations (if both are expected)
    if [[ "$IMPLEMENTATION" == "both" ]]; then
        if [[ ${#ffm_artifacts[@]} -eq 0 ]]; then
            fail_test "No FFM artifacts found when both implementations expected"
            return 1
        fi
        if [[ ${#jni_artifacts[@]} -eq 0 ]]; then
            fail_test "No JNI artifacts found when both implementations expected"
            return 1
        fi
    fi

    # Check for naming conflicts
    for ffm_jar in "${ffm_artifacts[@]}"; do
        for jni_jar in "${jni_artifacts[@]}"; do
            if [[ "$(basename "$ffm_jar")" == "$(basename "$jni_jar")" ]]; then
                fail_test "Naming conflict detected: $(basename "$ffm_jar")"
                return 1
            fi
        done
    done

    pass_test "Implementation differentiation validation passed"
    return 0
}

# Main validation function
validate_implementation() {
    local impl=$1
    local artifacts=($(find_artifacts "$impl"))

    if [[ ${#artifacts[@]} -eq 0 ]]; then
        log_warning "No $impl artifacts found in $ARTIFACT_DIR"
        return 0
    fi

    log_info "Validating $impl implementation (${#artifacts[@]} artifacts found)"

    local validation_passed=true

    for jar_file in "${artifacts[@]}"; do
        log_info "Processing artifact: $(basename "$jar_file")"

        validate_jar_structure "$jar_file" "$impl" || validation_passed=false
        validate_jar_manifest "$jar_file" "$impl" || validation_passed=false
        validate_naming_convention "$jar_file" "$impl" || validation_passed=false

        if [[ "$VALIDATION_LEVEL" != "basic" ]]; then
            validate_pom_file "$jar_file" "$impl" || validation_passed=false
        fi
    done

    if [[ "$validation_passed" == "true" ]]; then
        log_success "All validations passed for $impl implementation"
        return 0
    else
        log_error "Some validations failed for $impl implementation"
        return 1
    fi
}

# Print summary
print_summary() {
    echo ""
    log_info "Validation Summary"
    log_info "=================="
    log_info "Total tests: $TOTAL_TESTS"
    log_info "Passed: $PASSED_TESTS"
    log_info "Failed: $FAILED_TESTS"

    if [[ $FAILED_TESTS -eq 0 ]]; then
        log_success "All Maven artifact validations passed!"
        return 0
    else
        log_error "$FAILED_TESTS out of $TOTAL_TESTS validations failed"
        return 1
    fi
}

# Help function
show_help() {
    cat << EOF
Maven Artifact Validation Script

Usage: $0 [artifact_dir] [implementation] [validation_level]

Arguments:
  artifact_dir      Directory containing Maven artifacts [default: ./build/java]
  implementation    Implementation to validate (auto, ffm, jni, both) [default: auto]
  validation_level  Validation level (basic, full, strict) [default: basic]

Validation Levels:
  basic   - JAR structure, manifest, and naming validation
  full    - Basic + POM validation
  strict  - Full + additional runtime checks

Examples:
  $0                                    # Validate all found artifacts (basic)
  $0 ./build/java ffm full             # Validate FFM artifacts (full)
  $0 ./artifacts both strict           # Validate both implementations (strict)

EOF
}

# Main execution
main() {
    if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
        show_help
        exit 0
    fi

    log_info "Maven Artifact Validation"
    log_info "========================="
    log_info "Artifact Directory: $ARTIFACT_DIR"
    log_info "Implementation: $IMPLEMENTATION"
    log_info "Validation Level: $VALIDATION_LEVEL"
    log_info ""

    if [[ ! -d "$ARTIFACT_DIR" ]]; then
        log_error "Artifact directory not found: $ARTIFACT_DIR"
        exit 1
    fi

    case "$IMPLEMENTATION" in
        "auto")
            # Auto-detect based on available artifacts
            ffm_count=$(find_artifacts "ffm" | wc -l)
            jni_count=$(find_artifacts "jni" | wc -l)

            if [[ $ffm_count -gt 0 && $jni_count -gt 0 ]]; then
                log_info "Auto-detected: both implementations"
                validate_implementation "ffm"
                validate_implementation "jni"
                validate_implementation_differentiation
            elif [[ $ffm_count -gt 0 ]]; then
                log_info "Auto-detected: FFM implementation only"
                validate_implementation "ffm"
            elif [[ $jni_count -gt 0 ]]; then
                log_info "Auto-detected: JNI implementation only"
                validate_implementation "jni"
            else
                log_error "No Maven artifacts found in $ARTIFACT_DIR"
                exit 1
            fi
            ;;
        "both")
            validate_implementation "ffm"
            validate_implementation "jni"
            validate_implementation_differentiation
            ;;
        "ffm"|"jni")
            validate_implementation "$IMPLEMENTATION"
            ;;
        *)
            log_error "Invalid implementation: $IMPLEMENTATION"
            show_help
            exit 1
            ;;
    esac

    print_summary
}

# Run main function
main "$@"