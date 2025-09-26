#!/bin/bash
# CI Integration test for Java FFM and JNI implementations
# Tests the complete build-test-deploy cycle for both implementations
# Usage: ci-integration-test.sh [platform] [java_version] [test_scenario]

set -e

PLATFORM="${1:-ubuntu-latest}"
JAVA_VERSION="${2:-24}"
TEST_SCENARIO="${3:-full}"  # quick, build, maven, full

# Script configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
RESULTS_DIR="${PROJECT_ROOT}/ci-test-results"

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

# Test tracking
TOTAL_SCENARIOS=0
PASSED_SCENARIOS=0
FAILED_SCENARIOS=0

# Initialize results tracking
init_results() {
    mkdir -p "$RESULTS_DIR"
    RESULTS_FILE="$RESULTS_DIR/ci-integration-results.json"

    cat > "$RESULTS_FILE" << EOF
{
  "test_run": {
    "timestamp": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
    "platform": "$PLATFORM",
    "java_version": "$JAVA_VERSION",
    "test_scenario": "$TEST_SCENARIO",
    "project_root": "$PROJECT_ROOT"
  },
  "scenarios": []
}
EOF

    log_info "Results will be saved to: $RESULTS_FILE"
}

# Record scenario result
record_scenario() {
    local scenario_name=$1
    local status=$2
    local duration=$3
    local details=$4

    # Update global counters
    ((TOTAL_SCENARIOS++))
    if [[ "$status" == "passed" ]]; then
        ((PASSED_SCENARIOS++))
    else
        ((FAILED_SCENARIOS++))
    fi

    # Add to results file
    local temp_file=$(mktemp)
    jq --arg name "$scenario_name" \
       --arg status "$status" \
       --arg duration "$duration" \
       --arg details "$details" \
       '.scenarios += [{
           "name": $name,
           "status": $status,
           "duration": $duration,
           "details": $details,
           "timestamp": (now | strftime("%Y-%m-%dT%H:%M:%SZ"))
       }]' "$RESULTS_FILE" > "$temp_file" && mv "$temp_file" "$RESULTS_FILE"
}

# Test scenario: Basic build validation
test_basic_build() {
    log_info "Testing basic build configuration..."
    local start_time=$(date +%s)

    # Test both implementations if Java 24+, otherwise just JNI
    local implementations=("jni")
    if [[ $JAVA_VERSION -ge 24 ]]; then
        implementations+=("ffm")
    fi

    local all_passed=true

    for impl in "${implementations[@]}"; do
        log_info "Testing $impl implementation..."

        if "$SCRIPT_DIR/test-java-implementations.sh" "$JAVA_VERSION" "$impl" "build"; then
            log_success "$impl basic build test passed"
        else
            log_error "$impl basic build test failed"
            all_passed=false
        fi
    done

    local end_time=$(date +%s)
    local duration=$((end_time - start_time))

    if [[ "$all_passed" == "true" ]]; then
        record_scenario "basic_build" "passed" "${duration}s" "All implementations built successfully"
        return 0
    else
        record_scenario "basic_build" "failed" "${duration}s" "One or more implementation builds failed"
        return 1
    fi
}

# Test scenario: Maven artifact generation
test_maven_artifacts() {
    log_info "Testing Maven artifact generation..."
    local start_time=$(date +%s)

    # Test both implementations if Java 24+, otherwise just JNI
    local implementations=("jni")
    if [[ $JAVA_VERSION -ge 24 ]]; then
        implementations+=("ffm")
    fi

    local all_passed=true
    local artifact_count=0

    for impl in "${implementations[@]}"; do
        log_info "Testing $impl Maven artifacts..."

        if "$SCRIPT_DIR/test-java-implementations.sh" "$JAVA_VERSION" "$impl" "maven"; then
            log_success "$impl Maven artifact test passed"

            # Count generated artifacts
            local impl_artifacts=$("$SCRIPT_DIR/validate-maven-artifacts.sh" \
                "$PROJECT_ROOT/build-test-java${JAVA_VERSION}-${impl}-maven" "$impl" "basic" 2>/dev/null || echo "0")
            artifact_count=$((artifact_count + 1))
        else
            log_error "$impl Maven artifact test failed"
            all_passed=false
        fi
    done

    local end_time=$(date +%s)
    local duration=$((end_time - start_time))

    if [[ "$all_passed" == "true" ]]; then
        record_scenario "maven_artifacts" "passed" "${duration}s" "Generated $artifact_count artifact sets"
        return 0
    else
        record_scenario "maven_artifacts" "failed" "${duration}s" "Maven artifact generation failed"
        return 1
    fi
}

# Test scenario: Artifact validation
test_artifact_validation() {
    log_info "Testing artifact validation..."
    local start_time=$(date +%s)

    local validation_passed=true
    local validated_artifacts=0

    # Find all generated artifact directories
    for build_dir in "$PROJECT_ROOT"/build-test-java${JAVA_VERSION}-*-maven; do
        if [[ -d "$build_dir" ]]; then
            log_info "Validating artifacts in: $(basename "$build_dir")"

            # Extract implementation from directory name
            local impl
            if [[ "$build_dir" =~ -ffm- ]]; then
                impl="ffm"
            elif [[ "$build_dir" =~ -jni- ]]; then
                impl="jni"
            else
                log_warning "Cannot determine implementation for: $build_dir"
                continue
            fi

            if "$SCRIPT_DIR/validate-maven-artifacts.sh" "$build_dir" "$impl" "full"; then
                log_success "Artifact validation passed for $impl"
                ((validated_artifacts++))
            else
                log_error "Artifact validation failed for $impl"
                validation_passed=false
            fi
        fi
    done

    local end_time=$(date +%s)
    local duration=$((end_time - start_time))

    if [[ "$validation_passed" == "true" ]]; then
        record_scenario "artifact_validation" "passed" "${duration}s" "Validated $validated_artifacts artifact sets"
        return 0
    else
        record_scenario "artifact_validation" "failed" "${duration}s" "Artifact validation failed"
        return 1
    fi
}

# Test scenario: Implementation differentiation
test_implementation_differentiation() {
    if [[ $JAVA_VERSION -lt 24 ]]; then
        log_info "Skipping implementation differentiation test (Java < 24)"
        record_scenario "implementation_differentiation" "skipped" "0s" "Java version < 24, only JNI available"
        return 0
    fi

    log_info "Testing implementation differentiation..."
    local start_time=$(date +%s)

    # Verify that both FFM and JNI artifacts exist and are different
    local ffm_dir="$PROJECT_ROOT/build-test-java${JAVA_VERSION}-ffm-maven"
    local jni_dir="$PROJECT_ROOT/build-test-java${JAVA_VERSION}-jni-maven"

    if [[ ! -d "$ffm_dir" ]] || [[ ! -d "$jni_dir" ]]; then
        log_error "Missing build directories for differentiation test"
        record_scenario "implementation_differentiation" "failed" "0s" "Missing build directories"
        return 1
    fi

    # Check artifact naming
    local ffm_jars=($(find "$ffm_dir" -name "*hdf5-java-ffm*.jar" 2>/dev/null || true))
    local jni_jars=($(find "$jni_dir" -name "*hdf5-java-jni*.jar" 2>/dev/null || true))

    if [[ ${#ffm_jars[@]} -eq 0 ]]; then
        log_error "No FFM artifacts found"
        record_scenario "implementation_differentiation" "failed" "0s" "No FFM artifacts found"
        return 1
    fi

    if [[ ${#jni_jars[@]} -eq 0 ]]; then
        log_error "No JNI artifacts found"
        record_scenario "implementation_differentiation" "failed" "0s" "No JNI artifacts found"
        return 1
    fi

    # Verify different manifest content
    local ffm_jar="${ffm_jars[0]}"
    local jni_jar="${jni_jars[0]}"

    local ffm_impl=$(unzip -q -c "$ffm_jar" META-INF/MANIFEST.MF | grep "HDF5-Java-Implementation:" | cut -d' ' -f2 | tr -d '\r')
    local jni_impl=$(unzip -q -c "$jni_jar" META-INF/MANIFEST.MF | grep "HDF5-Java-Implementation:" | cut -d' ' -f2 | tr -d '\r')

    if [[ "$ffm_impl" != "FFM" ]]; then
        log_error "FFM JAR has incorrect implementation marker: $ffm_impl"
        record_scenario "implementation_differentiation" "failed" "0s" "Incorrect FFM implementation marker"
        return 1
    fi

    if [[ "$jni_impl" != "JNI" ]]; then
        log_error "JNI JAR has incorrect implementation marker: $jni_impl"
        record_scenario "implementation_differentiation" "failed" "0s" "Incorrect JNI implementation marker"
        return 1
    fi

    local end_time=$(date +%s)
    local duration=$((end_time - start_time))

    log_success "Implementation differentiation test passed"
    record_scenario "implementation_differentiation" "passed" "${duration}s" "FFM and JNI artifacts properly differentiated"
    return 0
}

# Test scenario: CMake preset validation
test_cmake_presets() {
    log_info "Testing CMake preset functionality..."
    local start_time=$(date +%s)

    local preset_tests_passed=true

    # Test implementation-specific presets
    local implementations=("jni")
    if [[ $JAVA_VERSION -ge 24 ]]; then
        implementations+=("ffm")
    fi

    for impl in "${implementations[@]}"; do
        local preset="ci-StdShar-GNUC-Java-${impl^^}"
        log_info "Testing preset: $preset"

        local test_build_dir="$PROJECT_ROOT/build-preset-test-$impl"
        rm -rf "$test_build_dir"

        cd "$PROJECT_ROOT"

        if cmake --preset "$preset" -B "$test_build_dir" >/dev/null 2>&1; then
            # Verify implementation was set correctly
            if grep -q "HDF5_JAVA_IMPLEMENTATION:STRING=${impl^^}" "$test_build_dir/CMakeCache.txt"; then
                log_success "Preset $preset works correctly"
            else
                log_error "Preset $preset didn't set correct implementation"
                preset_tests_passed=false
            fi
        else
            log_error "Preset $preset failed to configure"
            preset_tests_passed=false
        fi

        rm -rf "$test_build_dir"
    done

    local end_time=$(date +%s)
    local duration=$((end_time - start_time))

    if [[ "$preset_tests_passed" == "true" ]]; then
        record_scenario "cmake_presets" "passed" "${duration}s" "All CMake presets work correctly"
        return 0
    else
        record_scenario "cmake_presets" "failed" "${duration}s" "Some CMake presets failed"
        return 1
    fi
}

# Generate final report
generate_report() {
    log_info "Generating final CI integration report..."

    # Update results file with summary
    local temp_file=$(mktemp)
    jq --arg total "$TOTAL_SCENARIOS" \
       --arg passed "$PASSED_SCENARIOS" \
       --arg failed "$FAILED_SCENARIOS" \
       '.summary = {
           "total_scenarios": ($total | tonumber),
           "passed_scenarios": ($passed | tonumber),
           "failed_scenarios": ($failed | tonumber),
           "success_rate": (($passed | tonumber) / ($total | tonumber) * 100 | round)
       }' "$RESULTS_FILE" > "$temp_file" && mv "$temp_file" "$RESULTS_FILE"

    echo ""
    log_info "CI Integration Test Summary"
    log_info "==========================="
    log_info "Platform: $PLATFORM"
    log_info "Java Version: $JAVA_VERSION"
    log_info "Test Scenario: $TEST_SCENARIO"
    log_info ""
    log_info "Results:"
    log_info "  Total scenarios: $TOTAL_SCENARIOS"
    log_info "  Passed: $PASSED_SCENARIOS"
    log_info "  Failed: $FAILED_SCENARIOS"

    if [[ $FAILED_SCENARIOS -eq 0 ]]; then
        log_success "All CI integration tests passed!"
        log_info "Results saved to: $RESULTS_FILE"
        return 0
    else
        log_error "$FAILED_SCENARIOS out of $TOTAL_SCENARIOS scenarios failed"
        log_info "Results saved to: $RESULTS_FILE"
        return 1
    fi
}

# Cleanup function
cleanup() {
    log_info "Cleaning up test artifacts..."

    # Remove build directories
    for build_dir in "$PROJECT_ROOT"/build-test-java*; do
        if [[ -d "$build_dir" ]]; then
            log_info "Removing: $(basename "$build_dir")"
            rm -rf "$build_dir"
        fi
    done

    # Remove preset test directories
    for preset_dir in "$PROJECT_ROOT"/build-preset-test-*; do
        if [[ -d "$preset_dir" ]]; then
            rm -rf "$preset_dir"
        fi
    done
}

# Help function
show_help() {
    cat << EOF
CI Integration Test Suite

Usage: $0 [platform] [java_version] [test_scenario]

Arguments:
  platform       Target platform (ubuntu-latest, windows-latest, macos-latest) [default: ubuntu-latest]
  java_version   Java version to test (11, 17, 21, 24, 25) [default: 24]
  test_scenario  Test scenario (quick, build, maven, full) [default: full]

Test Scenarios:
  quick   - Basic build validation only
  build   - Build validation + CMake presets
  maven   - Build + Maven artifacts + validation
  full    - All tests including differentiation

Examples:
  $0                                    # Full test on Ubuntu with Java 24
  $0 ubuntu-latest 24 quick            # Quick test on Ubuntu with Java 24
  $0 windows-latest 11 build           # Build test on Windows with Java 11

EOF
}

# Main execution
main() {
    if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
        show_help
        exit 0
    fi

    log_info "CI Integration Test Suite"
    log_info "========================"
    log_info "Platform: $PLATFORM"
    log_info "Java Version: $JAVA_VERSION"
    log_info "Test Scenario: $TEST_SCENARIO"
    log_info ""

    # Check if jq is available for JSON processing
    if ! command -v jq >/dev/null 2>&1; then
        log_error "jq is required but not installed. Please install jq to continue."
        exit 1
    fi

    init_results

    # Set cleanup trap
    trap cleanup EXIT

    # Run test scenarios based on selected scenario
    case "$TEST_SCENARIO" in
        "quick")
            test_basic_build
            ;;
        "build")
            test_basic_build
            test_cmake_presets
            ;;
        "maven")
            test_basic_build
            test_cmake_presets
            test_maven_artifacts
            test_artifact_validation
            ;;
        "full")
            test_basic_build
            test_cmake_presets
            test_maven_artifacts
            test_artifact_validation
            test_implementation_differentiation
            ;;
        *)
            log_error "Invalid test scenario: $TEST_SCENARIO"
            show_help
            exit 1
            ;;
    esac

    generate_report
}

# Run main function
main "$@"