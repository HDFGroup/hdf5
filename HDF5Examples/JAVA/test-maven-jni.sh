#!/bin/bash
#
# test-maven-jni.sh - Test HDF5 JNI examples against Maven artifacts
#
# Usage: ./test-maven-jni.sh [VERSION] [REPOSITORY_URL] [BUILD_DIR]
#
# Examples:
#   ./test-maven-jni.sh 2.0.1-SNAPSHOT
#   ./test-maven-jni.sh 2.0.1
#   ./test-maven-jni.sh 2.0.1-SNAPSHOT https://maven.pkg.github.com/HDFGroup/hdf5
#   ./test-maven-jni.sh 2.0.1-SNAPSHOT https://maven.pkg.github.com/HDFGroup/hdf5 /tmp/maven-test-jni
#

set -e  # Exit on error

# Determine source directory (where this script lives)
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Default values
VERSION="${1:-2.0.1-SNAPSHOT}"
REPOSITORY_URL="${2:-https://maven.pkg.github.com/HDFGroup/hdf5}"
BUILD_DIR="${3:-${SCRIPT_DIR}/build/maven-test-jni}"
ARTIFACT_ID="hdf5-java-jni"
IMPLEMENTATION="JNI"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Helper functions
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

# Create build directory
mkdir -p "${BUILD_DIR}"

# Print header
echo "============================================"
echo "HDF5 Maven ${IMPLEMENTATION} Examples Test"
echo "============================================"
echo "Version:    ${VERSION}"
echo "Repository: ${REPOSITORY_URL}"
echo "Artifact:   org.hdfgroup:${ARTIFACT_ID}"
echo "Source:     ${SCRIPT_DIR}"
echo "Build:      ${BUILD_DIR}"
echo "============================================"
echo ""

# Check prerequisites
log_info "Checking prerequisites..."

# Check Java version
if ! command -v java &> /dev/null; then
    log_error "Java not found. Please install Java 11 or later."
    exit 1
fi

JAVA_VERSION=$(java -version 2>&1 | awk -F '"' '/version/ {print $2}' | cut -d'.' -f1)
log_info "Java version: $(java -version 2>&1 | head -1)"

if [ "$JAVA_VERSION" -lt 11 ]; then
    log_error "Java 11 or later required for JNI. Current version: $JAVA_VERSION"
    exit 1
fi

# Check Maven
if ! command -v mvn &> /dev/null; then
    log_error "Maven not found. Please install Maven 3.6.0 or later."
    exit 1
fi

log_info "Maven version: $(mvn -version | head -1)"

# Check GitHub authentication (if using GitHub Packages)
if [[ "$REPOSITORY_URL" == *"github.com"* ]]; then
    log_info "Checking GitHub authentication..."

    if command -v gh &> /dev/null; then
        if gh auth status &> /dev/null; then
            log_success "GitHub CLI authenticated"
            GITHUB_TOKEN=$(gh auth token)
        else
            log_warning "GitHub CLI not authenticated. Checking ~/.m2/settings.xml..."
        fi
    fi

    # Check if Maven settings exist
    if [ ! -f ~/.m2/settings.xml ]; then
        if [ -z "$GITHUB_TOKEN" ]; then
            log_error "No GitHub authentication found. Please run 'gh auth login' or create ~/.m2/settings.xml"
            exit 1
        else
            log_info "Creating ~/.m2/settings.xml with GitHub token..."
            mkdir -p ~/.m2
            cat > ~/.m2/settings.xml <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<settings xmlns="http://maven.apache.org/SETTINGS/1.0.0"
          xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
          xsi:schemaLocation="http://maven.apache.org/SETTINGS/1.0.0
          http://maven.apache.org/xsd/settings-1.0.0.xsd">
    <servers>
        <server>
            <id>github-hdfgroup-hdf5</id>
            <username>$(git config user.name || echo "user")</username>
            <password>${GITHUB_TOKEN}</password>
        </server>
    </servers>
    <profiles>
        <profile>
            <id>github-packages</id>
            <repositories>
                <repository>
                    <id>github-hdfgroup-hdf5</id>
                    <url>${REPOSITORY_URL}</url>
                    <snapshots><enabled>true</enabled></snapshots>
                    <releases><enabled>true</enabled></releases>
                </repository>
            </repositories>
        </profile>
    </profiles>
    <activeProfiles>
        <activeProfile>github-packages</activeProfile>
    </activeProfiles>
</settings>
EOF
            log_success "Created ~/.m2/settings.xml"
        fi
    else
        log_success "Found ~/.m2/settings.xml"
    fi
fi

log_success "Prerequisites check passed"
echo ""

# Generate pom-examples.xml in build directory
log_info "Generating pom-examples.xml for ${IMPLEMENTATION}..."

cat > "${BUILD_DIR}/pom-examples.xml" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
         http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>

    <groupId>org.hdfgroup</groupId>
    <artifactId>hdf5-java-examples-test</artifactId>
    <version>1.0.0-TEST</version>
    <packaging>jar</packaging>

    <properties>
        <maven.compiler.source>11</maven.compiler.source>
        <maven.compiler.target>11</maven.compiler.target>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
        <hdf5.version>${VERSION}</hdf5.version>
    </properties>

    <repositories>
        <repository>
            <id>github-hdfgroup-hdf5</id>
            <url>${REPOSITORY_URL}</url>
            <snapshots><enabled>true</enabled></snapshots>
            <releases><enabled>true</enabled></releases>
        </repository>
    </repositories>

    <dependencies>
        <dependency>
            <groupId>org.hdfgroup</groupId>
            <artifactId>${ARTIFACT_ID}</artifactId>
            <version>\${hdf5.version}</version>
        </dependency>
        <dependency>
            <groupId>org.slf4j</groupId>
            <artifactId>slf4j-simple</artifactId>
            <version>2.0.16</version>
            <scope>runtime</scope>
        </dependency>
    </dependencies>

    <build>
        <plugins>
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-compiler-plugin</artifactId>
                <version>3.11.0</version>
                <executions>
                    <execution>
                        <id>compile-h5d</id>
                        <phase>compile</phase>
                        <goals><goal>compile</goal></goals>
                        <configuration>
                            <compileSourceRoots>
                                <compileSourceRoot>${SCRIPT_DIR}/compat/H5D</compileSourceRoot>
                            </compileSourceRoots>
                        </configuration>
                    </execution>
                    <execution>
                        <id>compile-h5t</id>
                        <phase>compile</phase>
                        <goals><goal>compile</goal></goals>
                        <configuration>
                            <compileSourceRoots>
                                <compileSourceRoot>${SCRIPT_DIR}/compat/H5T</compileSourceRoot>
                            </compileSourceRoots>
                        </configuration>
                    </execution>
                    <execution>
                        <id>compile-h5g</id>
                        <phase>compile</phase>
                        <goals><goal>compile</goal></goals>
                        <configuration>
                            <compileSourceRoots>
                                <compileSourceRoot>${SCRIPT_DIR}/compat/H5G</compileSourceRoot>
                            </compileSourceRoots>
                        </configuration>
                    </execution>
                    <execution>
                        <id>compile-tutr</id>
                        <phase>compile</phase>
                        <goals><goal>compile</goal></goals>
                        <configuration>
                            <compileSourceRoots>
                                <compileSourceRoot>${SCRIPT_DIR}/compat/TUTR</compileSourceRoot>
                            </compileSourceRoots>
                        </configuration>
                    </execution>
                </executions>
            </plugin>
            <plugin>
                <groupId>org.codehaus.mojo</groupId>
                <artifactId>exec-maven-plugin</artifactId>
                <version>3.1.0</version>
                <configuration>
                    <mainClass>H5Ex_D_ReadWrite</mainClass>
                </configuration>
            </plugin>
        </plugins>
    </build>
</project>
EOF

log_success "Generated pom-examples.xml"
echo ""

# Clean previous builds
log_info "Cleaning previous builds..."
mvn clean -f "${BUILD_DIR}/pom-examples.xml" -q
rm -f "${BUILD_DIR}"/*.h5 2>/dev/null || true
log_success "Clean complete"
echo ""

# Download dependencies and verify artifact
log_info "Downloading Maven artifact: org.hdfgroup:${ARTIFACT_ID}:${VERSION}..."
if mvn dependency:get \
    -Dartifact=org.hdfgroup:${ARTIFACT_ID}:${VERSION} \
    -DremoteRepositories=${REPOSITORY_URL} \
    -q; then
    log_success "Artifact downloaded successfully"
else
    log_error "Failed to download artifact. Check version and repository URL."
    exit 1
fi
echo ""

# Verify JAR contents
log_info "Verifying JAR contents..."
JAR_PATH=$(find ~/.m2/repository/org/hdfgroup/${ARTIFACT_ID}/${VERSION} -name "*.jar" ! -name "*sources*" ! -name "*javadoc*" | head -1)

if [ -z "$JAR_PATH" ]; then
    log_error "Could not find downloaded JAR file"
    exit 1
fi

log_info "JAR location: ${JAR_PATH}"
log_info "JAR size: $(du -h "$JAR_PATH" | cut -f1)"

# Check for HDF5 classes
if jar tf "$JAR_PATH" | grep -q "hdf/hdf5lib/H5.class"; then
    log_success "JAR contains HDF5 JNI classes (hdf.hdf5lib.*)"
else
    log_error "JAR does not contain HDF5 classes! Only contains:"
    jar tf "$JAR_PATH" | head -20
    log_error "This indicates the Maven artifact was built incorrectly."
    exit 1
fi

# Count example classes that should be present
CLASS_COUNT=$(jar tf "$JAR_PATH" | grep "hdf/hdf5lib.*\.class" | wc -l)
log_info "Found $CLASS_COUNT HDF5 classes in JAR"
echo ""

# Compile examples
log_info "Compiling ${IMPLEMENTATION} examples..."
if mvn compile -f "${BUILD_DIR}/pom-examples.xml"; then
    log_success "Examples compiled successfully"
else
    log_error "Compilation failed"
    exit 1
fi
echo ""

# Count compiled example files
COMPILED_COUNT=$(find "${BUILD_DIR}/target/classes" -name "*.class" 2>/dev/null | wc -l)
log_info "Compiled $COMPILED_COUNT example classes"
echo ""

# Run a test example (change to build directory so .h5 files are created there)
log_info "Running test example: H5Ex_D_ReadWrite..."
if (cd "${BUILD_DIR}" && mvn exec:java -Dexec.mainClass="H5Ex_D_ReadWrite" -f pom-examples.xml -q); then
    log_success "Example executed successfully"

    # Check if HDF5 file was created
    if [ -f "${BUILD_DIR}/H5Ex_D_ReadWrite.h5" ]; then
        log_success "HDF5 file created: ${BUILD_DIR}/H5Ex_D_ReadWrite.h5"
        log_info "File size: $(du -h "${BUILD_DIR}/H5Ex_D_ReadWrite.h5" | cut -f1)"
    else
        log_warning "HDF5 file not found (may have been deleted by example)"
    fi
else
    log_error "Example execution failed"
    exit 1
fi
echo ""

# Final summary
echo "============================================"
echo "               TEST SUMMARY"
echo "============================================"
log_success "All tests passed!"
echo ""
echo "Summary:"
echo "  - Artifact:  org.hdfgroup:${ARTIFACT_ID}:${VERSION}"
echo "  - JAR Size:  $(du -h "$JAR_PATH" | cut -f1)"
echo "  - Classes:   $CLASS_COUNT HDF5 classes"
echo "  - Compiled:  $COMPILED_COUNT example classes"
echo "  - Execution: H5Ex_D_ReadWrite succeeded"
echo "============================================"
echo ""
log_info "Build directory: ${BUILD_DIR}"
echo ""
log_info "To run more examples:"
echo "  cd \"${BUILD_DIR}\" && mvn exec:java -Dexec.mainClass=\"EXAMPLE_NAME\" -f pom-examples.xml"
echo ""
log_info "To clean up test files:"
echo "  mvn clean -f \"${BUILD_DIR}/pom-examples.xml\" && rm -rf \"${BUILD_DIR}\""
