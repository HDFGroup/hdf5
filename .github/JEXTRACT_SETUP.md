# jextract-25 Setup for Configure-Time FFM Generation

## Overview

This document describes the jextract-25 setup infrastructure for configure-time FFM binding generation in HDF5 2.0.

## Architecture

### Reusable Composite Action

**Location:** `.github/actions/setup-jextract/action.yml`

**Purpose:** Installs jextract-25 across all platforms (Linux, Windows, macOS) for use in CI workflows.

**Features:**
- Cross-platform support (Linux, Windows, macOS)
- Automatic fallback to multiple jextract download URLs
- Sets `JEXTRACT_HOME` environment variable
- Adds jextract to PATH
- Version detection and output

**Usage:**
```yaml
- name: Setup jextract
  uses: ./.github/actions/setup-jextract
  with:
    java-version: '25'
```

### Workflow Integration

#### maven-staging.yml

**Changes:**
1. Java version selection based on implementation:
   - FFM builds: Java 25 (Oracle distribution)
   - JNI builds: Java 21 (Temurin distribution)

2. Conditional jextract setup:
   ```yaml
   - name: Setup jextract (FFM builds only)
     if: ${{ matrix.implementation == 'ffm' }}
     uses: ./.github/actions/setup-jextract
     with:
       java-version: '25'
   ```

3. Applied to both build and test jobs:
   - `build-maven-artifacts` - Builds FFM artifacts with jextract
   - `test-java-examples-maven` - Tests FFM artifacts with Java 25

#### generate-ffm-bindings.yml

**Changes:**
- Replaced inline jextract installation (150+ lines) with composite action call
- Simplified maintenance and consistency across workflows

### CMake Integration

**Location:** `java/CMakeLists.txt` (lines 26-61)

**Logic:**
```cmake
if (Java_VERSION_STRING VERSION_GREATER_EQUAL "24.0.0")
  if (HDF5_ENABLE_JNI)
    # Force JNI even with Java 24+
    set (HDF5_JAVA_USE_FFM FALSE)
  else ()
    # Use FFM with Java 24+
    set (HDF5_JAVA_USE_FFM TRUE)

    # Find jextract in JEXTRACT_HOME or JAVA_HOME
    find_program (JEXTRACT_EXECUTABLE jextract
      PATHS "$ENV{JAVA_HOME}/bin" "$ENV{JEXTRACT_HOME}/bin"
      REQUIRED)

    # Generate FFM bindings at configure time
    execute_process (
      COMMAND ${JEXTRACT_EXECUTABLE}
        --include-dir ${HDF5_SRC_DIR}
        --output ${JEXTRACT_OUTPUT_DIR}
        --target-package org.hdfgroup.javahdf5
        --library hdf5
        ${HDF5_SRC_DIR}/hdf5.h
    )
  endif ()
endif ()
```

## Testing Requirements

### Local Testing

```bash
# 1. Install jextract-25
export JEXTRACT_HOME=/path/to/jextract-25
export PATH=$JEXTRACT_HOME/bin:$PATH

# 2. Verify jextract
jextract --version

# 3. Build with FFM
cmake --workflow --preset ci-StdShar-GNUC-FFM --fresh

# 4. Verify FFM bindings generated
ls build/ci-StdShar-GNUC-FFM/java/jsrc/org/hdfgroup/javahdf5/

# 5. Run FFM tests
cd build/ci-StdShar-GNUC-FFM && ctest -R JUnitFFM -V
```

### CI Testing

```bash
# Test Maven staging with FFM
gh workflow run maven-staging.yml \
  -f platforms=linux-only \
  -f java_implementation=ffm \
  -f use_snapshot_version=true

# Monitor workflow
gh run list --workflow=maven-staging.yml

# Check logs for jextract setup
gh run view <run-id> --log
```

## Migration Path

### Phase 1: Configure-Time Generation (Current)
- ✅ jextract setup action created
- ✅ maven-staging.yml updated
- ✅ generate-ffm-bindings.yml updated
- ✅ Documentation updated
- ⏳ CI testing pending

### Phase 2: Deprecation (After successful CI testing)
Once configure-time generation is confirmed working:

1. **Deprecate workflows:**
   - `generate-ffm-bindings.yml` (standalone generation no longer needed)

2. **Deprecate scripts:**
   - `bin/jextract-generate.sh`
   - `bin/jextract-generate.bat`
   - `bin/merge-ffm-bindings.py`

3. **Remove pre-generated bindings:**
   - `java/jsrc/features/` directories
   - `java/jsrc/org/` platform-specific files

4. **Update workflows:**
   - Remove daily-build integration with generate-ffm-bindings
   - Simplify CI to only use configure-time generation

### Phase 3: Production (After deprecation)
- All FFM builds use configure-time generation
- Pre-generated bindings removed from repository
- Simplified maintenance and development workflow

## Benefits

### For Developers
- No separate workflow to generate bindings
- Bindings always match current C API
- Local builds work same as CI
- Faster iteration during API changes

### For CI/CD
- Fewer workflow runs needed
- Faster builds (no separate generation job)
- Consistent bindings across platforms
- Reduced artifact storage requirements

### For Maintenance
- Single source of truth (CMake + jextract)
- No manual merge/validation needed
- Automatic updates with C API changes
- Less infrastructure to maintain

## Troubleshooting

### jextract not found
```
CMake Error: Could not find jextract executable
```
**Solution:** Set `JEXTRACT_HOME` or ensure jextract is in `JAVA_HOME/bin`

### Wrong Java version
```
Building HDF5 Java with JNI implementation (Java 21.0.0)
```
**Solution:** Install Java 24+ and ensure it's first in PATH

### FFM generation fails
```
execute_process: jextract command failed
```
**Solution:** Check jextract can find HDF5 headers and libraries

### CI workflow fails on jextract setup
```
ERROR: Failed to download jextract from any known source
```
**Solution:** Update jextract URLs in `.github/actions/setup-jextract/action.yml`

## See Also

- [CLAUDE.md](../CLAUDE.md) - Full HDF5 build documentation
- [java/CMakeLists.txt](../java/CMakeLists.txt) - FFM generation logic
- [CMakePresets.json](../CMakePresets.json) - Build presets with FFM variants
