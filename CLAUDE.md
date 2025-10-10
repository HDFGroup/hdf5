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
# FFM implementation (Java 24+ default)
cmake --workflow --preset ci-MinShar-GNUC-Maven-FFM-Snapshot --fresh     # Linux FFM snapshots
cmake --workflow --preset ci-MinShar-MSVC-Maven-FFM-Snapshot --fresh     # Windows FFM snapshots
cmake --workflow --preset ci-MinShar-Clang-Maven-FFM-Snapshot --fresh    # macOS FFM snapshots
cmake --workflow --preset ci-MinShar-GNUC-Maven-FFM --fresh              # Linux FFM release
cmake --workflow --preset ci-MinShar-MSVC-Maven-FFM --fresh              # Windows FFM release
cmake --workflow --preset ci-MinShar-Clang-Maven-FFM --fresh             # macOS FFM release

# JNI implementation (all Java versions)
cmake --workflow --preset ci-MinShar-GNUC-Maven-JNI-Snapshot --fresh     # Linux JNI snapshots
cmake --workflow --preset ci-MinShar-MSVC-Maven-JNI-Snapshot --fresh     # Windows JNI snapshots
cmake --workflow --preset ci-MinShar-Clang-Maven-JNI-Snapshot --fresh    # macOS JNI snapshots
cmake --workflow --preset ci-MinShar-GNUC-Maven-JNI --fresh              # Linux JNI release
cmake --workflow --preset ci-MinShar-MSVC-Maven-JNI --fresh              # Windows JNI release
cmake --workflow --preset ci-MinShar-Clang-Maven-JNI --fresh             # macOS JNI release

# Install
cmake --install .
```

### Key CMake Options

- `HDF5_BUILD_TOOLS=ON` - Build HDF5 utilities (h5dump, h5diff, etc.)
- `HDF5_BUILD_EXAMPLES=ON` - Build example programs
- `HDF5_BUILD_FORTRAN=ON` - Build Fortran bindings
- `HDF5_BUILD_CPP_LIB=ON` - Build C++ bindings
- `HDF5_BUILD_JAVA=ON` - Build Java bindings
- `HDF5_ENABLE_JNI=ON` - Force JNI implementation (default: OFF, uses FFM for Java 24+)
- `HDF5_ENABLE_PARALLEL=ON` - Enable MPI parallel support
- `HDF5_ENABLE_THREADSAFE=ON` - Enable thread safety
- `BUILD_TESTING=ON` - Build test suite
- `HDF5_ENABLE_MAVEN_DEPLOY=ON` - Enable Maven repository deployment
- `HDF5_MAVEN_SNAPSHOT=ON` - Build Maven snapshot versions (-SNAPSHOT suffix)

### Java Implementation Selection

- **FFM (Foreign Function & Memory)**: Default for Java 24+, provides modern native access
- **JNI (Java Native Interface)**: Available for all Java versions, will be deprecated in future releases

### Maven Artifacts

- **FFM Implementation**: `org.hdfgroup:hdf5-java-ffm`
- **JNI Implementation**: `org.hdfgroup:hdf5-java-jni`

Both implementations use the same `hdf.hdf5lib.*` package structure for seamless migration.

### FFM Test Coverage

**Status as of October 9, 2025**: 233 FFM tests across 11 modules, all passing ✅

| Module | Tests | Coverage | Status |
|--------|-------|----------|--------|
| H5F (Files) | 12 | Files, VFDs | ✅ Active |
| H5D (Datasets) | 23 | Dataset I/O, chunks | ✅ Active |
| H5S (Dataspaces) | 41 | Selections, hyperslabs | ✅ Active |
| H5T (Datatypes) | 26 | Types, conversion | ✅ Active |
| H5A (Attributes) | 26 | Metadata attributes | ✅ Active |
| H5P (Properties) | 55 | Property lists (expanded) | ✅ Active |
| H5E (Errors) | 9 | Error handling | ✅ Active |
| H5G (Groups) | 10 | Group operations | ✅ Active |
| H5I (Identifiers) | 10 | ID mgmt + user-defined types | ✅ Active |
| H5L (Links) | 11 | Hard/soft/external links | ✅ Active |
| H5R (References) | 10 | Object/region refs | ✅ Active |

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

### Example Categories
- **H5D/** - Dataset operations (25 examples)
- **H5T/** - Datatype operations (16 examples)
- **H5G/** - Group operations (8 examples)
- **TUTR/** - Tutorial examples (13 examples)

### Building Java Examples with Maven
```bash
cd HDF5Examples/JAVA
mvn compile -f pom-examples.xml
mvn test -Prun-examples -f pom-examples.xml
```

### Java Examples Testing in CI
- **Staging Integration**: Examples tested in `maven-staging.yml`
- **Multi-Platform Testing**: All platforms tested in parallel (Linux, Windows, macOS x86_64, macOS aarch64)
- **Representative Testing**: 4 examples (1 per category) per platform for quick validation
- **Full Testing**: Available via `java-examples-maven-test.yml` (62 examples)
- **Platform-Specific Artifacts**: Each platform tests against its own Maven artifacts
- **Output Validation**: Pattern-based success/failure detection with native library error handling

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
   # Java FFM (default for Java 24+)
   cmake --workflow --preset ci-StdShar-GNUC-Java-FFM --fresh

   # Java JNI (explicit selection)
   cmake --workflow --preset ci-StdShar-GNUC-Java-JNI --fresh

   # Maven deployment with FFM
   cmake --workflow --preset ci-MinShar-GNUC-Maven-FFM --fresh

   # Maven deployment with JNI
   cmake --workflow --preset ci-MinShar-GNUC-Maven-JNI --fresh
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