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

### FFM Feature Variants

**As of October 13, 2025**: FFM bindings support optional ROS3 (S3 cloud storage) VFD feature selection:

**Directory Structure:**
```
java/jsrc/
├── features/
│   ├── plain/          # Standard FFM bindings (no ROS3 VFD support)
│   │   ├── hdf5_h.java
│   │   ├── hdf5_h_1.java
│   │   └── hdf5_h_2.java
│   └── ros3/           # FFM bindings with ROS3 VFD support (+9 ROS3 APIs)
│       ├── hdf5_h.java
│       ├── hdf5_h_1.java
│       └── hdf5_h_2.java
└── org/hdfgroup/javahdf5/  # Common FFM structs/types (both variants)
```

**Build Selection:**
- CMake automatically selects the appropriate variant based on `HDF5_ENABLE_ROS3_VFD`
- Maven artifacts will be built with the corresponding feature set
- Tests are compatible with both variants (use common API surface)

**Feature Comparison:**
- **Plain**: ~82,000 lines, standard HDF5 VFD support
- **ROS3**: ~83,000 lines, includes H5FD_ros3_* APIs for S3 cloud storage

### FFM Test Coverage

**Status as of October 14, 2025**: 384 FFM tests (383 active, 1 ignored) across 12 modules, **100% PASSING** ✅

**Latest Update:** Complete FFM test suite success! Fixed all remaining test failures:
- **H5T:** 92/92 passing (expanded from 37, fixed 16 FFM memory allocation issues)
- **H5D:** 27/27 passing (fixed 4 dataset name mismatch issues)
- **H5R:** 13/13 passing (fixed 4 dataset reference issues)
- **H5I:** 15/15 passing (fixed 4 library type usage issues)

**Note:** FFM tests focus on direct C API bindings via Foreign Function & Memory API. The legacy H5 wrapper class (for JNI compatibility) is separately tested and complete.

**Coverage Target:** 70%+ for critical modules (H5F, H5D, H5A, H5T, H5S, H5P)

| Module | Tests | C APIs | Coverage | Focus Area | Status |
|--------|-------|--------|----------|------------|--------|
| H5T (Datatypes) | 92 | 176 | 52% | Types, conversion, reclamation, enum, array, vlen, opaque, complex | ✅ **Excellent progress, 100% passing** |
| H5P (Properties) | 80 | 266 | 30% | Property lists, VFDs, virtual datasets, object copy | ✅ Active, expanding |
| H5S (Dataspaces) | 41 | 43 | 95% | Selections, hyperslabs, extents | ✅ Excellent coverage |
| H5A (Attributes) | 29 | 55 | 53% | Metadata attributes, storage, iteration | ✅ Active |
| H5D (Datasets) | 28 | 56 | 50% | Dataset I/O, chunks, flush/refresh | ✅ Active |
| H5F (Files) | 20 | 57 | 35% | Files, VFDs, metadata cache | ✅ Active, needs expansion |
| H5O (Objects) | 19 | 55 | 35% | Object operations, visitation | ✅ Active, needs expansion |
| H5L (Links) | 16 | 38 | 42% | Hard/soft/external links, iteration | ✅ Active |
| H5G (Groups) | 15 | 37 | 41% | Group operations, queries, comments | ✅ Active |
| H5I (Identifiers) | 15 | 21 | 71% | ID management, type operations | ✅ Good coverage |
| H5R (References) | 15 | 27 | 56% | Object/region/attribute refs | ✅ Good coverage |
| H5E (Errors) | 14 | 32 | 44% | Error handling, stack operations | ✅ Active |
| **TOTAL** | **384** | **863** | **45%** | **All core modules** | ✅ **All passing** |

**Implementation Priorities:**
1. **H5T (Datatypes)**: Expand from 92 to 120 tests (target: 68% coverage) - Additional conversion, bitfield, and advanced type tests
2. **H5P (Properties)**: Expand from 80 to 150 tests (target: 56% coverage) - VFD properties, virtual datasets
3. **H5F (Files)**: Expand from 20 to 45 tests (target: 79% coverage) - metadata cache, SWMR, file images
4. **H5O (Objects)**: Expand from 19 to 45 tests (target: 82% coverage) - visitation, metadata operations
5. **Advanced Modules**: H5VL (VOL), H5Z (Filters), H5ES (Event Sets) - deferred to later phase

**FFM Best Practices:**
See `.claude/FFM_MEMORY_PATTERNS.md` for comprehensive guide on correct FFM memory allocation patterns, common pitfalls, and test development guidelines.

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