# HDF5 Build System Summary

A comprehensive analysis of the HDF5 CMake-only build system and CI/CD infrastructure for development planning.

## Build System Architecture

### CMake-Only Migration (March 2025)
- **Complete transition**: Autotools support was completely dropped in HDF5 2.0
- **CMake minimum version**: 3.26 required
- **Out-of-source builds**: Enforced - in-source builds are blocked with clear error messages

### Core CMake Structure

#### Main Configuration Files
- `CMakeLists.txt` - Root build configuration (1,266 lines, highly comprehensive)
- `CMakeBuildOptions.cmake` - Centralized build option definitions (~80 major options, including Maven deployment)
- `CMakeFilters.cmake` - Compression filter support (zlib, szip, libaec)
- `CMakeTests.cmake` - Testing infrastructure configuration
- `CMakeInstallation.cmake` - Installation and packaging setup
- `CMakeVOL.cmake` - Virtual Object Layer connector support
- `CMakePlugins.cmake` - Plugin architecture support
- `java/src/hdf/hdf5lib/pom.xml.in` - Maven POM template for Java artifacts

#### Build Options Categories

**Library Types:**
- `BUILD_STATIC_LIBS=ON` - Static library builds
- `BUILD_SHARED_LIBS=ON` - Shared library builds
- `HDF5_ONLY_SHARED_LIBS=OFF` - Force shared-only builds
- `HDF5_BUILD_STATIC_TOOLS=OFF` - Static vs shared tools

**Language Bindings:**
- `HDF5_BUILD_CPP_LIB=OFF` - C++ bindings
- `HDF5_BUILD_FORTRAN=OFF` - Fortran bindings
- `HDF5_BUILD_JAVA=OFF` - Java bindings (FFM or JNI implementation)
- `HDF5_ENABLE_JNI=OFF` - Force JNI implementation instead of FFM for Java 24+
- `HDF5_ENABLE_MAVEN_DEPLOY=OFF` - Maven repository deployment support
- `HDF5_MAVEN_SNAPSHOT=OFF` - Build Maven snapshot versions with -SNAPSHOT suffix

**Core Features:**
- `HDF5_ENABLE_PARALLEL=OFF` - MPI parallel I/O support
- `HDF5_ENABLE_THREADSAFE=OFF` - Thread safety (mutually exclusive with parallel)
- `HDF5_ENABLE_CONCURRENCY=OFF` - Multi-threaded concurrency
- `HDF5_BUILD_HL_LIB=ON` - High-level APIs
- `HDF5_BUILD_TOOLS=ON` - Command-line utilities

**Compression & Filters:**
- `HDF5_ENABLE_ZLIB_SUPPORT=OFF` - DEFLATE compression
- `HDF5_ENABLE_SZIP_SUPPORT=OFF` - SZIP compression
- `HDF5_ENABLE_PLUGIN_SUPPORT=OFF` - Runtime plugin loading
- `HDF5_USE_ZLIB_NG=OFF` - Use zlib-ng instead of zlib

**Advanced Features:**
- `HDF5_ENABLE_SUBFILING_VFD=OFF` - Parallel subfiling VFD
- `HDF5_ENABLE_MAP_API=OFF` - Map API (experimental)
- `HDF5_ENABLE_HDFS=OFF` - Hadoop HDFS support

### Java Implementation Selection

**FFM vs JNI Implementation:**
- **Java 24+**: FFM (Foreign Function & Memory) implementation by default
- **Java 11-23**: JNI (Java Native Interface) implementation required
- **Force JNI**: Use `HDF5_ENABLE_JNI=ON` to force JNI even with Java 24+
- **Artifact differentiation**: `hdf5-java-ffm` vs `hdf5-java-jni` Maven artifacts
- **Package compatibility**: Both use `hdf.hdf5lib.*` for seamless migration
- **Implementation detection**: Automatic based on Java version and user preferences

**CMake Variables:**
- `HDF5_JAVA_IMPLEMENTATION` - Set to "FFM" or "JNI" after configuration
- `HDF5_JAVA_ARTIFACT_ID` - Set to "hdf5-java-ffm" or "hdf5-java-jni"

## CMake Preset System

### Preset Architecture
- **Layered inheritance**: Base presets + feature-specific + platform-specific
- **Hidden presets**: Reusable components (`ci-base`, `ci-Debug`, `ci-Release`, `ci-Maven`, `ci-Maven-Snapshot`, `ci-Maven-Minimal`, `ci-Maven-Minimal-Snapshot`, `ci-Java-FFM`, `ci-Java-JNI`)
- **Platform presets**: `ci-GNUC`, `ci-Clang`, `ci-MSVC`, `ci-macos`
- **Maven presets**: Hidden base configurations for Maven deployment support
- **Minimal Maven presets**: Streamlined configurations for Java artifact generation only
- **Java implementation presets**: FFM and JNI specific configurations
- **Build type matrix**: Debug, Release (RelWithDebInfo + docs), Maven variants

### Key Preset Patterns
```bash
# Standard shared library builds
cmake --workflow --preset ci-StdShar-GNUC --fresh      # GCC
cmake --workflow --preset ci-StdShar-Clang --fresh     # Clang
cmake --workflow --preset ci-StdShar-MSVC --fresh      # MSVC

# Maven-enabled builds (Java artifacts with deployment support)
cmake --workflow --preset ci-StdShar-GNUC-Maven --fresh          # Maven release (full build)
cmake --workflow --preset ci-StdShar-GNUC-Maven-Snapshot --fresh # Maven snapshot (full build)
cmake --workflow --preset ci-MinShar-GNUC-Maven --fresh          # Maven release (minimal build)
cmake --workflow --preset ci-MinShar-GNUC-Maven-Snapshot --fresh # Maven snapshot (minimal build)

# Multi-platform Maven presets (minimal builds for Java artifacts only)
cmake --workflow --preset ci-MinShar-MSVC-Maven --fresh          # Windows Maven
cmake --workflow --preset ci-MinShar-Clang-Maven --fresh         # macOS Maven

# Java implementation-specific presets
cmake --workflow --preset ci-MinShar-GNUC-Maven-FFM --fresh      # FFM implementation
cmake --workflow --preset ci-MinShar-GNUC-Maven-JNI --fresh      # JNI implementation

# Naming convention: ci-[Features]-[Compiler][-Maven[-Snapshot]]
# Features: Std (standard), Min (minimal), StdShar (standard shared), MinShar (minimal shared)
# Maven: Adds Maven deployment support with platform-specific JARs
# Snapshot: Adds -SNAPSHOT suffix for development versions
# Minimal Maven presets: Skip examples, testing, tools, C++, Fortran - Java artifacts only
# Java Examples Maven Integration: Comprehensive testing of Java examples with Maven artifacts across all platforms
```

### Preset Configuration Strategy
- **Binary directory**: `${sourceParentDir}/build/${presetName}`
- **Install directory**: `${sourceParentDir}/install/${presetName}`
- **Generator**: Ninja (default for most presets)
- **External libraries**: TGZ/GIT support for zlib, szip, libaec

## Testing Infrastructure

### Test Framework Structure
- **CTest integration**: Primary test runner
- **Express levels**: 0 (exhaustive) to 3 (quick) - default level 3
- **Timeout system**: Base 1200s, with short/long/very-long variants
- **Parallel testing**: Separate test infrastructure for MPI builds

### Test Categories
- **Core tests**: `test/` - Library unit tests (~150 test programs)
- **API tests**: `test/API/` - Comprehensive API validation
- **Parallel tests**: `testpar/` - MPI-specific functionality
- **Tool tests**: `tools/test/` - Command-line utility validation
- **VFD tests**: Multiple Virtual File Driver implementations
- **VOL tests**: Virtual Object Layer connector testing
- **Language binding tests**: C++, Fortran, Java specific tests

### Test Execution Patterns
```bash
# Express testing (quick)
export HDF_TEST_EXPRESS=3
ctest -j4

# VFD testing matrix
ctest -R "VFD"

# Parallel testing
ctest -R "MPI\|parallel"

# API comprehensive tests
ctest -R "H5_api_test"
```

## CI/CD Pipeline Architecture

### GitHub Actions Matrix
- **50+ workflow files**: Comprehensive testing across platforms/compilers
- **Multi-dimensional matrix**:
  - Platforms: Windows (MSVC), Ubuntu (GCC), macOS (Clang)
  - Features: Serial, Parallel, Thread-safe, Various language bindings
  - Build types: Debug, Release, specialized configurations

### Key Workflow Categories

**Main CI Workflows:**
- `main.yml` - Primary CI across platforms
- `daily-build.yml` - Nightly comprehensive builds
- `ctest.yml` - Cross-platform testing with preset system

**Specialized Testing:**
- `par-*.yml` - Parallel/MPI testing workflows
- `vfd-*.yml` - Virtual File Driver testing
- `vol_*.yml` - Virtual Object Layer connector testing
- `analysis.yml` - Static analysis integration
- `java-implementation-test.yml` - Java FFM/JNI implementation testing
- `java-examples-maven-test.yml` - Java examples with Maven artifacts
- **FFM Latest Java Testing** - Automated testing with bleeding-edge Java versions via enhanced `main.yml` workflow

**Platform-Specific:**
- `arm-main.yml` - ARM architecture testing
- `cygwin.yml`, `msys2.yml` - Windows alternative environments
- `intel.yml`, `aocc.yml`, `nvhpc.yml` - Vendor compiler support

**Release Infrastructure:**
- `release.yml` - Main release workflow with optional Maven deployment
- `release-files.yml` - Automated release packaging
- `tarball.yml` - Source distribution creation
- `maven-deploy.yml` - Maven repository deployment workflow
- `maven-staging.yml` - PR-based Maven testing and validation workflow
- `daily-schedule.yml` - Scheduled builds with AWS integration

### Build Matrix Strategy
- **Cross-platform validation**: Windows/Linux/macOS for every PR
- **Compiler diversity**: GCC, Clang, MSVC, Intel, AOCC, NVHPC
- **Feature combinations**: Systematic testing of feature interactions
- **Performance variants**: Debug vs Release vs specialized builds
- **Latest Java Testing**: Non-blocking FFM testing with bleeding-edge Java versions
  - Configurable Java version selection (11, 17, 21, 24, latest, auto)
  - Force Java implementation (auto, ffm, jni)
  - Integrated into main CI pipeline via `call-workflows.yml`

## Packaging & Distribution

### Installation System
- **Component-based**: Libraries, headers, tools, docs, examples
- **CMake integration**: Full config package support
- **Cross-platform**: Windows (NSIS/WiX), macOS (DMG/Framework), Linux (DEB/RPM/TGZ)

### Release Process
- **Automated packaging**: CPack integration with platform-specific installers
- **Version management**: Automatic version extraction from source
- **SOVERSION handling**: Complex library versioning for different components
- **External dependencies**: Bundled compression libraries option

### Distribution Formats
- **Source tarballs**: Automated via GitHub Actions
- **Binary packages**: Platform-specific installers
- **Maven repositories**: GitHub Packages and Maven Central deployment
- **Java artifacts**: Platform-specific JARs with classifiers (linux-x86_64, windows-x86_64, macos-x86_64, macos-aarch64)
- **Container support**: Docker environments for CI
- **HPC integration**: Specialized configurations for batch systems

## Development Workflow Recommendations

### Build Strategy
1. **Use presets for consistency**: Leverage the preset system for reproducible builds
2. **Feature isolation**: Test individual features before combining
3. **Express testing**: Use level 3 for development, lower levels for validation
4. **Parallel development**: Separate MPI builds from serial development

### Testing Strategy
1. **Incremental testing**: Start with `testhdf5` core tests
2. **Feature-specific testing**: Use test regex patterns for targeted testing
3. **Cross-platform validation**: Test on primary CI platforms early
4. **Performance testing**: Include Release builds for performance-critical changes

### CI Integration
1. **Workflow triggers**: Understand which changes trigger which test suites
2. **Matrix optimization**: Consider CI time costs for comprehensive testing
3. **Failure isolation**: Use workflow categories to isolate platform/feature issues
4. **External dependency management**: Plan for compression library updates

### Maven Integration Workflow
1. **Java Build Configuration**: Enable Maven deployment with `HDF5_ENABLE_MAVEN_DEPLOY=ON`
2. **Implementation Selection**:
   - Auto-detect based on Java version (FFM for 24+, JNI for 11-23)
   - Force JNI with `HDF5_ENABLE_JNI=ON` regardless of Java version
   - Use implementation-specific presets for explicit control
3. **Version Management**: Use `HDF5_MAVEN_SNAPSHOT=ON` for development builds with `-SNAPSHOT` suffix
4. **Preset Selection**: Choose Maven-enabled presets with optional implementation specification:
   - `ci-StdShar-GNUC-Maven` or `ci-StdShar-GNUC-Maven-Snapshot` (auto-detect)
   - `ci-MinShar-GNUC-Maven-FFM` or `ci-MinShar-GNUC-Maven-JNI` (explicit)
5. **Artifact Differentiation**: Automatic generation of implementation-specific artifacts:
   - `hdf5-java-ffm` for FFM implementation (Java 24+)
   - `hdf5-java-jni` for JNI implementation (all Java versions)
6. **Platform Artifacts**: Automatic generation of platform-specific JARs with classifiers (linux-x86_64, windows-x86_64, macos-x86_64, macos-aarch64)
7. **CI Integration**: Multi-matrix testing across Java versions and implementations
8. **Implementation Testing**: Dedicated workflows for FFM/JNI validation:
   - `java-implementation-test.yml` - Cross-platform implementation testing
   - Matrix testing across Java 11, 17, 21, 24+ with appropriate implementations
9. **PR Testing**: Automated Maven artifact validation for pull requests via `maven-staging.yml`
10. **Validation Framework**: Pre-deployment validation using `.github/scripts/validate-maven-artifacts.sh`
11. **Repository Selection**: Choose between GitHub Packages and Maven Central via workflow inputs
12. **Release Integration**: Optional Maven deployment in release workflow with user control

## Critical Dependencies

### Build Dependencies
- **CMake 3.26+**: Required for preset support and modern features
- **Ninja**: Preferred generator for cross-platform consistency
- **Compression libraries**: zlib, szip/libaec (optional but commonly used)
- **MPI**: Required for parallel builds (MPI-3 standard minimum)
- **Java 11+**: Required for Java bindings and Maven deployment (when `HDF5_BUILD_JAVA=ON`)
  - **Java 11-23**: JNI implementation only
  - **Java 24+**: FFM implementation by default, JNI available with `HDF5_ENABLE_JNI=ON`
- **Maven**: Optional for local Maven operations and validation
- **jextract**: Required for FFM binding generation (typically bundled)

### Platform-Specific Requirements
- **Windows**: Visual Studio 2022, optional NSIS/WiX for packaging
- **macOS**: Xcode command line tools, universal binary support
- **Linux**: GCC/Clang, various package managers for dependencies

### Development Tools Integration
- **Static analysis**: Clang tools integration available
- **Code formatting**: clang-format integration
- **Coverage**: Code coverage support for testing
- **Sanitizers**: Runtime error detection support
- **Maven validation**: `.github/scripts/validate-maven-artifacts.sh` for pre-deployment validation

## Future Considerations

### Build System Evolution
- **CMake modernization**: Potential for newer CMake features as minimum version increases
- **Preset expansion**: More specialized presets for emerging use cases
- **Container integration**: Enhanced Docker/container support for development

### Testing Infrastructure
- **Test parallelization**: Opportunities for faster CI execution
- **Cloud testing**: Integration with cloud-native testing platforms
- **Performance regression**: Automated performance monitoring integration

### Platform Support
- **Emerging architectures**: ARM64, RISC-V support expansion
- **New compilers**: Integration with emerging compiler technologies
- **HPC evolution**: Adaptation to evolving supercomputing environments