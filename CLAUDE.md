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
cmake --workflow --preset ci-MinShar-GNUC-Maven-Snapshot --fresh     # Linux with snapshots
cmake --workflow --preset ci-MinShar-MSVC-Maven-Snapshot --fresh     # Windows with snapshots
cmake --workflow --preset ci-MinShar-Clang-Maven-Snapshot --fresh    # macOS with snapshots
cmake --workflow --preset ci-MinShar-GNUC-Maven --fresh              # Linux release
cmake --workflow --preset ci-MinShar-MSVC-Maven --fresh              # Windows release
cmake --workflow --preset ci-MinShar-Clang-Maven --fresh             # macOS release

# Install
cmake --install .
```

### Key CMake Options

- `HDF5_BUILD_TOOLS=ON` - Build HDF5 utilities (h5dump, h5diff, etc.)
- `HDF5_BUILD_EXAMPLES=ON` - Build example programs
- `HDF5_BUILD_FORTRAN=ON` - Build Fortran bindings
- `HDF5_BUILD_CPP_LIB=ON` - Build C++ bindings
- `HDF5_BUILD_JAVA=ON` - Build Java bindings
- `HDF5_ENABLE_PARALLEL=ON` - Enable MPI parallel support
- `HDF5_ENABLE_THREADSAFE=ON` - Enable thread safety
- `BUILD_TESTING=ON` - Build test suite
- `HDF5_ENABLE_MAVEN_DEPLOY=ON` - Enable Maven repository deployment
- `HDF5_MAVEN_SNAPSHOT=ON` - Build Maven snapshot versions (-SNAPSHOT suffix)

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
- **Representative Testing**: 4 examples (1 per category) for quick validation
- **Full Testing**: Available via `java-examples-maven-test.yml`
- **Cross-Platform**: Linux, Windows, macOS support
- **Output Validation**: Pattern-based success/failure detection

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

4. **Maven artifact testing:**
   ```bash
   # Test Maven staging workflow (all platforms)
   gh workflow run maven-staging.yml -f platforms=all-platforms -f use_snapshot_version=true

   # Test Maven deployment (dry run)
   gh workflow run release.yml -f deploy_maven=true -f use_tag=snapshot
   ```

5. **Java Examples testing:**
   ```bash
   # Test Java examples with Maven artifacts
   gh workflow run java-examples-maven-test.yml -f build_mode=release -f maven_artifacts_version=2.0.0-3-SNAPSHOT

   # Quick Java examples test (part of Maven staging)
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