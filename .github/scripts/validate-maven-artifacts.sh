#!/bin/bash
#
# Enhanced validation framework for Maven artifacts before deployment
# This script validates JAR files, POM files, and deployment readiness
#

set -euo pipefail

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
ARTIFACTS_DIR="${1:-./artifacts}"
VALIDATION_LOG="/tmp/maven-validation-$(date +%s).log"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $*" | tee -a "${VALIDATION_LOG}"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $*" | tee -a "${VALIDATION_LOG}"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $*" | tee -a "${VALIDATION_LOG}"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*" | tee -a "${VALIDATION_LOG}"
}

# Validation counters
VALIDATION_ERRORS=0
VALIDATION_WARNINGS=0

# Error tracking
add_error() {
    VALIDATION_ERRORS=$((VALIDATION_ERRORS + 1))
    log_error "$*"
}

add_warning() {
    VALIDATION_WARNINGS=$((VALIDATION_WARNINGS + 1))
    log_warn "$*"
}

# Java/Maven environment validation
validate_environment() {
    log_info "Validating build environment..."

    # Check Java availability
    if ! command -v java &> /dev/null; then
        add_error "Java is not installed or not in PATH"
        return 1
    fi

    JAVA_VERSION=$(java -version 2>&1 | head -n1 | cut -d'"' -f2)
    log_info "Java version: ${JAVA_VERSION}"

    # Check Maven availability
    if ! command -v mvn &> /dev/null; then
        add_warning "Maven is not installed - some validations will be skipped"
    else
        MVN_VERSION=$(mvn -version | head -n1 | cut -d' ' -f3)
        log_info "Maven version: ${MVN_VERSION}"
    fi

    # Check JAR command
    if ! command -v jar &> /dev/null; then
        add_error "jar command is not available"
        return 1
    fi

    log_success "Environment validation completed"
    return 0
}

# JAR file validation
validate_jar_file() {
    local jar_file="$1"
    local jar_basename
    jar_basename=$(basename "${jar_file}")

    log_info "Validating JAR: ${jar_basename}"

    # Check file exists and is readable
    if [[ ! -f "${jar_file}" ]]; then
        add_error "JAR file not found: ${jar_file}"
        return 1
    fi

    if [[ ! -r "${jar_file}" ]]; then
        add_error "JAR file not readable: ${jar_file}"
        return 1
    fi

    # Check file size (must be > 1KB)
    local file_size
    file_size=$(stat -c%s "${jar_file}" 2>/dev/null || stat -f%z "${jar_file}" 2>/dev/null || echo "0")
    if [[ ${file_size} -lt 1024 ]]; then
        add_error "JAR file too small: ${jar_file} (${file_size} bytes)"
        return 1
    fi
    log_info "JAR size: ${file_size} bytes"

    # Test JAR integrity
    if ! jar tf "${jar_file}" > /dev/null 2>&1; then
        add_error "JAR file is corrupted or invalid: ${jar_file}"
        return 1
    fi

    # Check for required HDF5 Java classes
    local temp_dir
    temp_dir=$(mktemp -d)
    trap "rm -rf '${temp_dir}'" EXIT

    if ! (cd "${temp_dir}" && jar xf "${jar_file}"); then
        add_error "Failed to extract JAR: ${jar_file}"
        rm -rf "${temp_dir}"
        return 1
    fi

    # Check for essential HDF5 classes based on JAR type
    # FFM builds have two separate JARs:
    #   - javahdf5-*.jar: FFM bindings (org/hdfgroup/javahdf5/*)
    #   - jarhdf5-*.jar: Wrapper classes (hdf/hdf5lib/*)
    # JNI builds have single JAR with hdf/hdf5lib/* classes

    if [[ "${jar_basename}" == *"javahdf5"* ]]; then
        # This is the FFM bindings JAR - check for FFM classes
        local ffm_classes=(
            "org/hdfgroup/javahdf5/hdf5_h.class"
        )

        local has_ffm=false
        for class_file in "${ffm_classes[@]}"; do
            if [[ -f "${temp_dir}/${class_file}" ]]; then
                has_ffm=true
                log_info "Found FFM binding class: ${class_file}"
                break
            fi
        done

        if [[ "${has_ffm}" == "false" ]]; then
            add_error "FFM bindings JAR missing required FFM classes (expected org/hdfgroup/javahdf5/hdf5_h.class)"
        fi
    else
        # This is a wrapper/JNI JAR - check for hdf.hdf5lib classes
        local required_classes=(
            "hdf/hdf5lib/H5.class"
            "hdf/hdf5lib/HDF5Constants.class"
            "hdf/hdf5lib/HDFArray.class"
            "hdf/hdf5lib/HDFNativeData.class"
        )

        for class_file in "${required_classes[@]}"; do
            if [[ ! -f "${temp_dir}/${class_file}" ]]; then
                add_error "Missing required class in JAR: ${class_file}"
            fi
        done
    fi

    # Check manifest
    if [[ -f "${temp_dir}/META-INF/MANIFEST.MF" ]]; then
        if grep -q "Enable-Native-Access: ALL-UNNAMED" "${temp_dir}/META-INF/MANIFEST.MF"; then
            log_info "Native access enabled in manifest"
        else
            add_warning "Native access not found in manifest - may cause runtime issues"
        fi
    else
        add_warning "No manifest found in JAR"
    fi

    rm -rf "${temp_dir}"
    log_success "JAR validation completed: ${jar_basename}"
    return 0
}

# POM file validation
validate_pom_file() {
    local pom_file="$1"

    log_info "Validating POM: $(basename "${pom_file}")"

    # Check file exists
    if [[ ! -f "${pom_file}" ]]; then
        add_error "POM file not found: ${pom_file}"
        return 1
    fi

    # Check XML validity
    if command -v xmllint &> /dev/null; then
        if ! xmllint --noout "${pom_file}" 2>/dev/null; then
            add_error "POM file is not valid XML: ${pom_file}"
            return 1
        fi
    else
        add_warning "xmllint not available - skipping XML validation"
    fi

    # Check required Maven coordinates
    if ! grep -q "<groupId>org.hdfgroup</groupId>" "${pom_file}"; then
        add_error "Invalid or missing groupId in POM"
    fi

    if ! grep -qE "<artifactId>hdf5-java(-ffm|-jni)?</artifactId>" "${pom_file}"; then
        add_error "Invalid or missing artifactId in POM (expected hdf5-java, hdf5-java-ffm, or hdf5-java-jni)"
    fi

    # Extract version
    local version
    version=$(grep -o '<version>[^<]*</version>' "${pom_file}" | head -1 | sed 's/<[^>]*>//g' || echo "")
    if [[ -z "${version}" ]]; then
        add_error "No version found in POM"
    else
        log_info "POM version: ${version}"

        # Validate version format
        if [[ ! "${version}" =~ ^[0-9]+\.[0-9]+\.[0-9]+(-[0-9]+)?(-SNAPSHOT)?$ ]]; then
            add_warning "Version format may not comply with Maven conventions: ${version}"
        fi
    fi

    # Check for required sections
    local required_sections=(
        "<name>"
        "<description>"
        "<url>"
        "<licenses>"
        "<developers>"
        "<scm>"
    )

    for section in "${required_sections[@]}"; do
        if ! grep -q "${section}" "${pom_file}"; then
            add_warning "Missing recommended section in POM: ${section}"
        fi
    done

    # Check dependencies
    if grep -q "<dependencies>" "${pom_file}"; then
        log_info "Dependencies section found in POM"
    else
        add_warning "No dependencies section in POM"
    fi

    log_success "POM validation completed"
    return 0
}

# Version consistency validation
validate_version_consistency() {
    local pom_file="$1"
    shift
    local jar_files=("$@")

    log_info "Validating version consistency across artifacts..."

    # Extract version from POM
    local pom_version
    pom_version=$(grep -o '<version>[^<]*</version>' "${pom_file}" | head -1 | sed 's/<[^>]*>//g' || echo "")

    if [[ -z "${pom_version}" ]]; then
        add_error "Cannot extract version from POM for consistency check"
        return 1
    fi

    log_info "POM version: ${pom_version}"

    # Check JAR filenames for version consistency
    for jar_file in "${jar_files[@]}"; do
        local jar_basename
        jar_basename=$(basename "${jar_file}")

        # Extract version from JAR filename (allowing for classifiers)
        local jar_version
        jar_version=$(echo "${jar_basename}" | sed -E 's/.*-([0-9]+\.[0-9]+\.[0-9]+(-[0-9]+)?(-SNAPSHOT)?)(-[^.]+)?\.jar$/\1/' || echo "")

        if [[ -z "${jar_version}" ]]; then
            add_warning "Cannot extract version from JAR filename: ${jar_basename}"
        elif [[ "${jar_version}" != "${pom_version}" ]]; then
            add_error "Version mismatch: POM=${pom_version}, JAR=${jar_version} (${jar_basename})"
        else
            log_info "Version consistency verified: ${jar_basename}"
        fi
    done

    return 0
}

# Platform classifier validation
validate_platform_classifiers() {
    local jar_files=("$@")

    log_info "Validating platform classifiers..."

    local valid_classifiers=(
        "linux-x86_64"
        "windows-x86_64"
        "macos-x86_64"
        "macos-aarch64"
    )

    for jar_file in "${jar_files[@]}"; do
        local jar_basename
        jar_basename=$(basename "${jar_file}")

        # Skip universal JARs (no classifier)
        if [[ ! "${jar_basename}" =~ -[a-z]+-[a-z0-9_]+\.jar$ ]]; then
            log_info "Universal JAR (no classifier): ${jar_basename}"
            continue
        fi

        # Extract classifier
        local classifier
        classifier=$(echo "${jar_basename}" | sed -E 's/.*-([a-z]+-[a-z0-9_]+)\.jar$/\1/' || echo "")

        if [[ -z "${classifier}" ]]; then
            add_warning "Cannot extract classifier from JAR: ${jar_basename}"
            continue
        fi

        # Validate classifier
        local valid=false
        for valid_classifier in "${valid_classifiers[@]}"; do
            if [[ "${classifier}" == "${valid_classifier}" ]]; then
                valid=true
                break
            fi
        done

        if [[ "${valid}" == "true" ]]; then
            log_info "Valid platform classifier: ${classifier} (${jar_basename})"
        else
            add_error "Invalid platform classifier: ${classifier} (${jar_basename})"
        fi
    done

    return 0
}

# Maven dependency simulation
simulate_maven_dependency() {
    local pom_file="$1"

    if ! command -v mvn &> /dev/null; then
        add_warning "Maven not available - skipping dependency simulation"
        return 0
    fi

    log_info "Simulating Maven dependency resolution..."

    # Create temporary Maven project
    local temp_project
    temp_project=$(mktemp -d)
    trap "rm -rf '${temp_project}'" EXIT

    # Extract coordinates from POM
    local group_id artifact_id version
    group_id=$(grep -o '<groupId>[^<]*</groupId>' "${pom_file}" | head -1 | sed 's/<[^>]*>//g' || echo "")
    artifact_id=$(grep -o '<artifactId>[^<]*</artifactId>' "${pom_file}" | head -1 | sed 's/<[^>]*>//g' || echo "")
    version=$(grep -o '<version>[^<]*</version>' "${pom_file}" | head -1 | sed 's/<[^>]*>//g' || echo "")

    # Find the JAR file in the artifacts directory
    local jar_file
    jar_file=$(find "$(dirname "${pom_file}")" -maxdepth 2 -name "${artifact_id}-${version}.jar" -o -name "${artifact_id}-*.jar" | head -1)

    if [ -z "${jar_file}" ]; then
        add_warning "Could not find JAR file for ${artifact_id}:${version} - skipping dependency simulation"
        return 0
    fi

    # Install artifact to local Maven repository first
    log_info "Installing artifact to local Maven repository: ${group_id}:${artifact_id}:${version}"
    if ! mvn install:install-file \
        -Dfile="${jar_file}" \
        -DgroupId="${group_id}" \
        -DartifactId="${artifact_id}" \
        -Dversion="${version}" \
        -Dpackaging=jar \
        -DpomFile="${pom_file}" \
        -q 2>&1 | tee -a "${VALIDATION_LOG}"; then
        add_warning "Failed to install artifact to local Maven repository"
        return 0
    fi

    # Create test POM
    cat > "${temp_project}/pom.xml" << EOF
<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
         http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>
    <groupId>test</groupId>
    <artifactId>maven-validation-test</artifactId>
    <version>1.0.0</version>
    <dependencies>
        <dependency>
            <groupId>${group_id}</groupId>
            <artifactId>${artifact_id}</artifactId>
            <version>${version}</version>
        </dependency>
    </dependencies>
</project>
EOF

    # Test dependency resolution
    if (cd "${temp_project}" && mvn dependency:resolve -q); then
        log_success "Maven dependency simulation passed"
    else
        add_warning "Maven dependency simulation failed - may indicate packaging issues"
    fi

    rm -rf "${temp_project}"
    return 0
}

# Deployment readiness check
check_deployment_readiness() {
    local artifacts_dir="$1"

    log_info "Checking deployment readiness..."

    # Check for required files
    local jar_files pom_files
    # Only count HDF5 JAR files, exclude dependencies like slf4j
    jar_files=($(find "${artifacts_dir}" -name "*hdf5*.jar" -not -name "*test*" 2>/dev/null || true))
    pom_files=($(find "${artifacts_dir}" -name "pom.xml" 2>/dev/null || true))

    if [[ ${#jar_files[@]} -eq 0 ]]; then
        add_error "No JAR files found in artifacts directory"
        return 1
    fi

    if [[ ${#pom_files[@]} -eq 0 ]]; then
        add_error "No POM files found in artifacts directory"
        return 1
    fi

    log_info "Found ${#jar_files[@]} JAR file(s) and ${#pom_files[@]} POM file(s)"

    # Check environment variables for deployment
    if [[ -z "${MAVEN_USERNAME:-}" ]]; then
        add_warning "MAVEN_USERNAME not set - deployment will fail"
    fi

    if [[ -z "${MAVEN_PASSWORD:-}" ]]; then
        add_warning "MAVEN_PASSWORD not set - deployment will fail"
    fi

    return 0
}

# Generate validation report
generate_report() {
    local artifacts_dir="$1"

    log_info "=== Maven Artifact Validation Report ==="
    log_info "Timestamp: $(date)"
    log_info "Artifacts directory: ${artifacts_dir}"
    log_info "Validation log: ${VALIDATION_LOG}"
    echo

    # Summary
    if [[ ${VALIDATION_ERRORS} -eq 0 ]]; then
        if [[ ${VALIDATION_WARNINGS} -eq 0 ]]; then
            log_success "✅ All validations passed with no warnings"
        else
            log_warn "⚠️  All validations passed with ${VALIDATION_WARNINGS} warning(s)"
        fi
    else
        log_error "❌ Validation failed with ${VALIDATION_ERRORS} error(s) and ${VALIDATION_WARNINGS} warning(s)"
    fi

    echo
    log_info "Full validation log available at: ${VALIDATION_LOG}"

    return ${VALIDATION_ERRORS}
}

# Main validation function
main() {
    local artifacts_dir="${1:-./artifacts}"

    log_info "Starting Maven artifact validation..."
    log_info "Artifacts directory: ${artifacts_dir}"

    # Check artifacts directory
    if [[ ! -d "${artifacts_dir}" ]]; then
        add_error "Artifacts directory not found: ${artifacts_dir}"
        generate_report "${artifacts_dir}"
        exit 1
    fi

    # Environment validation
    validate_environment

    # Find artifacts
    local jar_files pom_files all_jars
    # Only validate HDF5 JAR files, exclude dependencies like slf4j
    jar_files=($(find "${artifacts_dir}" -name "*hdf5*.jar" -not -name "*test*" 2>/dev/null || true))
    pom_files=($(find "${artifacts_dir}" -name "pom.xml" 2>/dev/null || true))
    all_jars=($(find "${artifacts_dir}" -name "*.jar" 2>/dev/null || true))

    # Log what we found
    log_info "Found ${#all_jars[@]} total JAR file(s), ${#jar_files[@]} HDF5 JAR file(s) to validate"
    if [[ ${#all_jars[@]} -gt ${#jar_files[@]} ]]; then
        log_info "Skipping non-HDF5 JAR files (dependencies like slf4j, etc.)"
        for jar in "${all_jars[@]}"; do
            if [[ ! "$(basename "$jar")" =~ hdf5 ]]; then
                log_info "  Skipping: $(basename "$jar")"
            fi
        done
    fi

    # Basic readiness check
    check_deployment_readiness "${artifacts_dir}"

    # Validate each JAR file
    for jar_file in "${jar_files[@]}"; do
        validate_jar_file "${jar_file}"
    done

    # Validate each POM file
    for pom_file in "${pom_files[@]}"; do
        validate_pom_file "${pom_file}"
    done

    # Version consistency check
    if [[ ${#pom_files[@]} -gt 0 && ${#jar_files[@]} -gt 0 ]]; then
        validate_version_consistency "${pom_files[0]}" "${jar_files[@]}"
    fi

    # Platform classifier validation
    if [[ ${#jar_files[@]} -gt 0 ]]; then
        validate_platform_classifiers "${jar_files[@]}"
    fi

    # Maven dependency simulation
    if [[ ${#pom_files[@]} -gt 0 ]]; then
        simulate_maven_dependency "${pom_files[0]}"
    fi

    # Generate final report
    generate_report "${artifacts_dir}"
    exit ${VALIDATION_ERRORS}
}

# Show usage if no arguments provided
if [[ $# -eq 0 ]]; then
    echo "Usage: $0 <artifacts_directory>"
    echo
    echo "Enhanced validation framework for Maven artifacts before deployment"
    echo
    echo "This script validates:"
    echo "  - JAR file integrity and content"
    echo "  - POM file structure and compliance"
    echo "  - Version consistency across artifacts"
    echo "  - Platform classifier conventions"
    echo "  - Maven dependency resolution simulation"
    echo "  - Deployment readiness"
    echo
    echo "Environment variables:"
    echo "  MAVEN_USERNAME - Maven repository username (optional for validation)"
    echo "  MAVEN_PASSWORD - Maven repository password (optional for validation)"
    echo
    exit 1
fi

# Run main function with arguments
main "$@"