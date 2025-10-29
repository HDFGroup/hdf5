# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build System

HDF5 uses **CMake only** as of version 2.0. Autotools support was dropped in March 2025.

### Essential Build Commands

```bash
# Basic build (out-of-source required)
mkdir build && cd build
cmake ..
cmake --build .

# Quick start with presets (recommended)
cmake --workflow --preset ci-StdShar-GNUC --fresh     # GCC
cmake --workflow --preset ci-StdShar-Clang --fresh    # Clang
cmake --workflow --preset ci-StdShar-MSVC --fresh     # MSVC

# Maven-enabled builds (Java artifacts with deployment support)
# JNI implementation (default - works with Java 8+)
cmake --workflow --preset ci-MinShar-GNUC-Maven-Snapshot --fresh     # Linux JNI snapshots
cmake --workflow --preset ci-MinShar-MSVC-Maven-Snapshot --fresh     # Windows JNI snapshots
cmake --workflow --preset ci-MinShar-Clang-Maven-Snapshot --fresh    # macOS JNI snapshots
cmake --workflow --preset ci-MinShar-GNUC-Maven --fresh              # Linux JNI release
cmake --workflow --preset ci-MinShar-MSVC-Maven --fresh              # Windows JNI release
cmake --workflow --preset ci-MinShar-Clang-Maven --fresh             # macOS JNI release

# FFM implementation (optional - requires Java 24+)
cmake --workflow --preset ci-MinShar-GNUC-Maven-FFM-Snapshot --fresh     # Linux FFM snapshots
cmake --workflow --preset ci-MinShar-MSVC-Maven-FFM-Snapshot --fresh     # Windows FFM snapshots
cmake --workflow --preset ci-MinShar-Clang-Maven-FFM-Snapshot --fresh    # macOS FFM snapshots
cmake --workflow --preset ci-MinShar-GNUC-Maven-FFM --fresh              # Linux FFM release
cmake --workflow --preset ci-MinShar-MSVC-Maven-FFM --fresh              # Windows FFM release
cmake --workflow --preset ci-MinShar-Clang-Maven-FFM --fresh             # macOS FFM release

# ROS3 VFD (S3 cloud storage) - Add to any preset above
# Example: JNI with ROS3
cmake --workflow --preset ci-MinShar-GNUC-Maven --fresh \
  -DHDF5_ENABLE_ROS3_VFD=ON

# Example: FFM with ROS3
cmake --workflow --preset ci-MinShar-GNUC-Maven-FFM --fresh \
  -DHDF5_ENABLE_ROS3_VFD=ON

# Install
cmake --install .
```

### Key CMake Options

- `HDF5_BUILD_TOOLS=ON` - Build HDF5 utilities (h5dump, h5diff, etc.)
- `HDF5_BUILD_EXAMPLES=ON` - Build example programs
- `HDF5_BUILD_FORTRAN=ON` - Build Fortran bindings
- `HDF5_BUILD_CPP_LIB=ON` - Build C++ bindings
- `HDF5_BUILD_JAVA=ON` - Build Java bindings
- `HDF5_ENABLE_JNI=OFF` - Auto-select FFM for Java 24+, JNI for older versions (default)
- `HDF5_ENABLE_JNI=ON` - Force JNI implementation even with Java 24+
- `HDF5_ENABLE_PARALLEL=ON` - Enable MPI parallel support
- `HDF5_ENABLE_THREADSAFE=ON` - Enable thread safety
- `BUILD_TESTING=ON` - Build test suite
- `HDF5_ENABLE_MAVEN_DEPLOY=ON` - Enable Maven repository deployment
- `HDF5_MAVEN_SNAPSHOT=ON` - Build Maven snapshot versions (-SNAPSHOT suffix)

### Preset Naming Convention

HDF5 CMake presets follow a consistent naming pattern:

**Standard Builds** (with Java JNI - default):
- Format: `ci-StdShar-{COMPILER}`
- Examples: `ci-StdShar-GNUC`, `ci-StdShar-MSVC`, `ci-StdShar-Clang`, `ci-StdShar-Intel`
- Description: Full-featured builds with C++, Fortran, Java (JNI), tools, examples, and tests

**FFM Builds** (with Java FFM - requires Java 24+):
- Format: `ci-StdShar-{COMPILER}-FFM`
- Examples: `ci-StdShar-GNUC-FFM`, `ci-StdShar-MSVC-FFM`, `ci-StdShar-Clang-FFM`
- Description: Same as standard builds but with FFM instead of JNI

**Maven Builds** (minimal Java-only builds for Maven artifacts):
- JNI Format: `ci-MinShar-{COMPILER}-Maven[-Snapshot]`
- FFM Format: `ci-MinShar-{COMPILER}-Maven-FFM[-Snapshot]`
- Examples: `ci-MinShar-GNUC-Maven`, `ci-MinShar-GNUC-Maven-FFM-Snapshot`
- Description: Minimal builds (Java only, no C++/Fortran/tools/tests) for Maven deployment

**Testing Builds** (for debugging FFM/JNI issues):
- Format: `ci-Testing-{COMPILER}-(FFM|JNI)`
- Examples: `ci-Testing-GNUC-FFM`, `ci-Testing-MSVC-JNI`
- Description: Builds with testing enabled for debugging Java implementation issues

**Supported Compilers**: `GNUC` (GCC), `MSVC` (Microsoft Visual C++), `Clang`, `Intel`

### Java Implementation Selection

**As of HDF5 2.0**: JNI is the default, FFM is optional

- **JNI (Java Native Interface)**: Default implementation, works with Java 8+, production-stable
- **FFM (Foreign Function & Memory)**: Optional implementation, requires Java 24+, modern native access

**Note**: Future releases may change FFM to default as Java 24+ adoption increases.

### Maven Artifacts

- **FFM Implementation**: `org.hdfgroup:hdf5-java-ffm`
- **JNI Implementation**: `org.hdfgroup:hdf5-java-jni`

Both implementations use the same `hdf.hdf5lib.*` package structure for seamless migration.

### FFM Feature Variants

FFM bindings are platform-specific and support optional ROS3 (S3 cloud storage) VFD feature selection:

**Directory Structure:**
```
java/jsrc/
├── features/
│   ├── plain/          # Standard FFM bindings (no ROS3 VFD support)
│   │   ├── linux/      # Linux-specific hdf5_h*.java files
│   │   ├── macos/      # macOS-specific hdf5_h*.java files
│   │   └── windows/    # Windows-specific hdf5_h*.java files
│   └── ros3/           # FFM bindings with ROS3 VFD support (+9 ROS3 APIs)
│       ├── linux/      # Linux ROS3 hdf5_h*.java + H5FD_ros3_fapl_t.java
│       ├── macos/      # macOS ROS3 hdf5_h*.java + H5FD_ros3_fapl_t.java
│       └── windows/    # Windows ROS3 hdf5_h*.java + H5FD_ros3_fapl_t.java
└── org/                # Platform-specific FFM structs/types
    ├── linux/hdfgroup/javahdf5/    # Linux types (FILE, pthread_*, etc.)
    ├── macos/hdfgroup/javahdf5/    # macOS types
    └── windows/hdfgroup/javahdf5/  # Windows types
```

**Platform-Specific Nature:**
- Both `features/` and `org/` directories are platform-specific
- jextract generates platform-specific code for native types (FILE, pthread_*, callbacks)
- Each platform has its own complete set of FFM bindings

**Build Selection:**
- CMake automatically selects the appropriate variant based on `HDF5_ENABLE_ROS3_VFD`
- CMake automatically selects the platform-specific files for the build platform
- Maven artifacts will be built with the corresponding feature set and platform
- Tests are compatible with both variants (use common API surface)

**Feature Comparison:**
- **Plain**: ~82,000 lines per platform, standard HDF5 VFD support
- **ROS3**: ~83,000 lines per platform, includes H5FD_ros3_* APIs for S3 cloud storage

### FFM Bindings Generation

**As of HDF5 2.0**: FFM bindings are generated automatically at CMake configure time using jextract-25.

**Requirements:**
- Java 24+ (tested with Java 25)
- jextract tool installed and accessible via `JEXTRACT_HOME` or `JAVA_HOME`

**Configure-Time Generation:**
FFM bindings are automatically generated when:
1. Java 24+ is detected by CMake
2. `HDF5_ENABLE_JNI=OFF` (FFM is selected)
3. jextract is found in `$JEXTRACT_HOME/bin` or `$JAVA_HOME/bin`

The CMake build system (java/CMakeLists.txt:36-52) automatically:
- Detects Java version
- Finds jextract executable
- Generates FFM bindings to `build/java/jsrc/`
- Configures the build to use generated bindings

**CI/CD Integration:**
Workflows automatically install jextract-25 for FFM builds:
- `.github/actions/setup-jextract/` - Reusable jextract setup action
- `.github/workflows/maven-staging.yml` - Maven artifact builds (FFM + JNI)
- `.github/workflows/release.yml` - Release builds with FFM support

**Local Development:**
```bash
# Install jextract-25 (one-time setup)
# Download from https://jdk.java.net/jextract/
# Extract and set JEXTRACT_HOME

export JEXTRACT_HOME=/path/to/jextract
export PATH=$JEXTRACT_HOME/bin:$PATH

# Build with FFM (Java 25+)
cmake --workflow --preset ci-StdShar-GNUC-FFM --fresh

# FFM bindings generated automatically during configure step
```

**Benefits of Configure-Time Generation:**
- No separate workflow needed to generate bindings
- Bindings always match the current HDF5 C API
- Faster CI/CD (no separate generation job)
- Simplified development workflow

### FFM Test Coverage

FFM tests provide comprehensive coverage across all major HDF5 modules, with tests for:

**Covered Modules:**
- **H5 (General):** Library initialization, version queries, memory management
- **H5T (Datatypes):** Type creation, conversion, reclamation, enum, array, vlen, opaque, complex types
- **H5P (Properties):** Property lists, VFDs, chunk/filter properties, dataset/file access properties
- **H5S (Dataspaces):** Selections, hyperslabs, extents, dataspace operations
- **H5D (Datasets):** Dataset I/O, chunking, compression, flush/refresh operations
- **H5F (Files):** File operations, VFDs, metadata cache, SWMR
- **H5G (Groups):** Group operations, hierarchy management, iteration
- **H5A (Attributes):** Attribute creation, I/O, metadata operations
- **H5L (Links):** Hard/soft/external links, link iteration
- **H5O (Objects):** Object operations, visitation, metadata queries
- **H5E (Errors):** Error handling, error stack operations
- **H5R (References):** Object, region, and attribute references
- **H5I (Identifiers):** ID management, type operations
- **H5VL (VOL):** Virtual object layer connectors
- **H5PL (Plugins):** Plugin management and discovery
- **H5FD (File Drivers):** Virtual file driver operations
- **H5Z (Filters):** Filter operations and pipeline management

**Note:** FFM tests focus on direct C API bindings via Foreign Function & Memory API. The legacy H5 wrapper class (for JNI compatibility) is separately tested.

**Run FFM tests**:
```bash
cd build && ctest -R "JUnitFFM" -V
```

**Test location**: `java/jtest/TestH5*ffm.java`

## Java Examples Maven Integration

### Java Examples as Maven Artifact

HDF5 Java examples (62 examples) are available as a Maven artifact:

```xml
<dependency>
    <groupId>org.hdfgroup</groupId>
    <artifactId>hdf5-java-examples</artifactId>
    <version>2.0.0-3</version>
</dependency>
```

### Building Java Examples with Maven
```bash
cd HDF5Examples/JAVA
mvn compile -f pom-examples.xml
mvn test -Prun-examples -f pom-examples.xml
```

## Testing

### Running Tests

```bash
# Run all tests
ctest

# Run specific test suites
ctest -R "H5TEST"           # Core library tests
ctest -R "testhdf5"         # Main test driver
ctest -R "h5dump"           # Tool tests

# Express testing levels (0=exhaustive, 3=quick)
ctest -E "MPI|SWMR"         # Exclude parallel/SWMR tests
```

### Test Structure

- `test/` - Core library unit tests
- `testpar/` - Parallel (MPI) tests
- `test/API/` - Comprehensive API tests
- `tools/test/` - Tool-specific tests
- Various VFD (Virtual File Driver) and VOL (Virtual Object Layer) tests

## Architecture Overview

### Core Components

**Library Structure:**
- `src/` - Core HDF5 library implementation
- `src/H5FDsubfiling/` - Subfiling Virtual File Driver for parallel I/O
- `hl/` - High-level APIs (HDF5 Lite, Images, Tables, etc.)
- `c++/`, `fortran/`, `java/` - Language bindings

**Key Modules (H5*.c files):**
- `H5F*` - File operations and Virtual File Drivers (VFDs)
- `H5G*` - Groups and hierarchical structure
- `H5D*` - Datasets and data storage
- `H5T*` - Datatypes and type conversion
- `H5S*` - Dataspaces and hyperslab selections
- `H5P*` - Property lists for configuration
- `H5A*` - Attributes
- `H5L*` - Links (hard, soft, external)
- `H5O*` - Object headers and metadata
- `H5Z*` - Filters and compression
- `H5V*` - Virtual Object Layer (VOL) for storage abstraction

### Key Features

**Storage Features:**
- Chunked and contiguous dataset layouts
- Compression filters (zlib, szip, plugins)
- External file storage
- Virtual datasets
- Complex datatypes (compound, variable-length, etc.)

**Parallel I/O:**
- MPI-based parallel access
- Collective and independent operations
- Subfiling VFD for improved parallel performance

**Advanced Features:**
- Single Writer Multiple Reader (SWMR)
- Virtual Object Layer (VOL) for custom storage backends
- Plugin architecture for filters and VFDs

## Development Guidelines

### Code Organization

- All public APIs start with `H5`
- Private functions use `H5_` prefix
- Module-specific functions use pattern like `H5F_` for file operations
- Header files: `H5public.h` (public), `H5private.h` (internal), `H5*pkg.h` (package-private)

### Testing Guidelines

- Use CTest framework
- API tests in `test/API/` follow comprehensive test patterns
- VFD tests verify Virtual File Driver functionality
- Express levels control test thoroughness (use level 3 for quick testing)

### Common Workflows

1. **Building with specific features:**
   ```bash
   cmake -DHDF5_ENABLE_PARALLEL=ON -DHDF5_BUILD_TOOLS=ON ..
   ```

2. **Running subset of tests:**
   ```bash
   ctest -R "testhdf5-shared" -j4
   ```

3. **Debug builds:**
   ```bash
   cmake -DCMAKE_BUILD_TYPE=Debug ..
   ```

4. **Java FFM/JNI specific builds:**
   ```bash
   # Java FFM (requires Java 24+)
   cmake --workflow --preset ci-StdShar-GNUC-FFM --fresh

   # Java JNI (default - works with Java 11+)
   cmake --workflow --preset ci-StdShar-GNUC --fresh

   # Maven deployment with FFM
   cmake --workflow --preset ci-MinShar-GNUC-Maven-FFM --fresh

   # Maven deployment with JNI (default)
   cmake --workflow --preset ci-MinShar-GNUC-Maven --fresh
   ```

5. **Maven artifact testing:**
   ```bash
   # Test Maven staging workflow (all platforms) - specify implementation
   gh workflow run maven-staging.yml -f platforms=all-platforms -f use_snapshot_version=true -f java_implementation=ffm
   gh workflow run maven-staging.yml -f platforms=all-platforms -f use_snapshot_version=true -f java_implementation=jni
   gh workflow run maven-staging.yml -f platforms=all-platforms -f use_snapshot_version=true -f java_implementation=both

   # Test Maven deployment to HDFGroup packages (dry run)
   gh workflow run test-maven-deployment.yml -f test_mode=dry-run -f target_repository=github-packages

   # Test Maven deployment to HDFGroup packages (live deployment)
   gh workflow run test-maven-deployment.yml -f test_mode=live-deployment -f target_repository=github-packages

   # Full release with Maven deployment (choose implementation)
   gh workflow run release.yml -f deploy_maven=true -f maven_repository=github-packages -f use_tag=snapshot

   # Test consuming deployed artifacts (specify FFM or JNI)
   ./.github/scripts/test-maven-consumer.sh 2.0.0-3 https://maven.pkg.github.com/<fork_name>/hdf5 hdf5-java-ffm
   ./.github/scripts/test-maven-consumer.sh 2.0.0-3 https://maven.pkg.github.com/<fork_name>/hdf5 hdf5-java-jni
   ```

6. **Java Examples testing:**
   ```bash
   # Test Java examples with Maven artifacts (specify implementation)
   gh workflow run java-examples-maven-test.yml -f build_mode=release -f maven_artifacts_version=2.0.0-3-SNAPSHOT -f java_implementation=ffm
   gh workflow run java-examples-maven-test.yml -f build_mode=release -f maven_artifacts_version=2.0.0-3-SNAPSHOT -f java_implementation=jni

   # Quick Java examples test (part of Maven staging) - tests both implementations
   gh workflow run maven-staging.yml -f test_maven_deployment=true
   ```

### Claude Code Assistant Shortcuts

For efficient interaction with Claude Code when working on this repository:

6. **Request clarification and improvements:**
   ```
   Ask questions for clarification and suggest improvements as needed.
   ```

7. **Common analysis patterns:**
   ```bash
   # Analyze current implementation and suggest next steps
   Analyze @*.md and @.github/workflows/*.yml files and suggest improvements

   # Review specific component integration
   Review the @component integration with @related-files and suggest optimizations

   # End-to-end workflow analysis
   Trace the complete workflow from @starting-point to @end-point and identify issues
   ```

## Documentation

- Primary docs: `release_docs/` directory
- Installation: `release_docs/INSTALL`, `release_docs/INSTALL_CMake.txt`
- API documentation generated via Doxygen when `HDF5_BUILD_DOC=ON`
- Examples in `HDF5Examples/` subdirectory
