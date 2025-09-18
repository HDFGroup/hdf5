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
cmake --workflow --preset ci-MinShar-GNUC-Maven --fresh          # Linux minimal Maven
cmake --workflow --preset ci-MinShar-MSVC-Maven --fresh          # Windows minimal Maven
cmake --workflow --preset ci-MinShar-Clang-Maven --fresh         # macOS minimal Maven

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

## Documentation

- Primary docs: `release_docs/` directory
- Installation: `release_docs/INSTALL`, `release_docs/INSTALL_CMake.txt`
- API documentation generated via Doxygen when `HDF5_BUILD_DOC=ON`
- Examples in `HDF5Examples/` subdirectory