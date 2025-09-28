# FFM Latest Java Version Workflow Analysis

## Current State Analysis

### Existing Java Testing Infrastructure

The HDF5 project currently has a robust Java testing infrastructure that includes:

1. **Java Implementation Test Workflow** (`java-implementation-test.yml`)
   - Tests both FFM and JNI implementations across Java versions 11, 17, 21, and 24
   - Supports platform matrix testing (Ubuntu, Windows, macOS)
   - Has artifact validation and POM file verification
   - Uses dynamic matrix generation based on Java version capabilities

2. **Main Workflow** (`main.yml`)
   - Reusable workflow with configurable inputs (cmake_version, thread_safety, concurrent, build_mode)
   - Tests Java bindings as part of comprehensive builds on Windows, Ubuntu, and macOS
   - Current Java testing is limited to whatever Java version is available on the runner

3. **CMake Configuration**
   - Automatic FFM/JNI selection based on Java version (FFM for 24+, JNI for <24)
   - `HDF5_ENABLE_JNI` option to force JNI even on Java 24+
   - Platform-specific Maven artifact generation

### Current Limitations

1. **Fixed Java Versions**: The main workflow uses whatever Java version is pre-installed on GitHub runners
2. **Limited FFM Testing**: FFM is only tested in the specialized `java-implementation-test.yml` workflow
3. **No Latest Java Testing**: No mechanism to test against the latest available Java versions automatically
4. **Integration Gap**: The specialized Java workflow is separate from main CI pipeline

## Proposed Solution: Enhanced Main Workflow

### Option 1: Add Java Version Input to Main Workflow (Recommended)

**Advantages:**
- Minimal disruption to existing infrastructure
- Leverages proven main.yml workflow
- Easy to trigger from call-workflows.yml
- Maintains existing test coverage while adding flexibility

**Implementation:**

```yaml
# Addition to main.yml inputs section
inputs:
  # ... existing inputs ...
  java_version:
    description: "Java version for testing (11, 17, 21, 24, latest, auto)"
    required: false
    default: "auto"
    type: string
  force_java_implementation:
    description: "Force specific Java implementation (auto, ffm, jni)"
    required: false
    default: "auto"
    type: string
```

**Matrix Enhancement:**
```yaml
# Addition to main.yml matrix
include:
  # ... existing configurations ...
  # For each OS, add Java version handling
  - name: "Ubuntu gcc"
    java_version_override: ${{ inputs.java_version }}
    java_implementation: ${{ inputs.force_java_implementation }}
```

**Java Setup Step:**
```yaml
- name: Set up Java (if specified)
  if: inputs.java_version != 'auto'
  uses: actions/setup-java@v4
  with:
    distribution: 'temurin'
    java-version: ${{ inputs.java_version == 'latest' && '24' || inputs.java_version }}
```

### Option 2: New Dedicated FFM Latest Java Workflow

**Advantages:**
- Clean separation of concerns
- Can focus specifically on latest Java testing
- No risk of disrupting main workflow

**Disadvantages:**
- Additional workflow to maintain
- Duplicate logic from main workflow
- Less integration with main CI pipeline

## Recommended Implementation Plan

### Phase 1: Enhance Main Workflow

1. **Add Java Version Input Parameter**
   ```yaml
   java_version:
     description: "Java version (11, 17, 21, 24, latest, auto)"
     required: false
     default: "auto"
     type: string
   ```

2. **Add Java Setup Step**
   ```yaml
   - name: Set up Java
     if: inputs.java_version != 'auto'
     uses: actions/setup-java@v4
     with:
       distribution: 'temurin'
       java-version: |
         ${{
           inputs.java_version == 'latest' && '24' ||
           inputs.java_version == 'auto' && '11' ||
           inputs.java_version
         }}
   ```

3. **Add FFM Testing Option**
   ```yaml
   force_java_implementation:
     description: "Force Java implementation (auto, ffm, jni)"
     required: false
     default: "auto"
     type: string
   ```

4. **Update CMake Configuration**
   ```yaml
   # In Configure step, add FFM-specific options
   -DHDF5_ENABLE_JNI:BOOL=${{ inputs.force_java_implementation == 'jni' }}
   ```

### Phase 2: Add Caller Workflow

**Add to call-workflows.yml:**
```yaml
call-ffm-latest-java:
  name: "FFM Latest Java Testing"
  uses: ./.github/workflows/main.yml
  with:
    cmake_version: "latest"
    concurrent: ""
    thread_safety: ""
    build_mode: "Release"
    java_version: "latest"
    force_java_implementation: "ffm"
```

### Phase 3: Weekly Latest Java Testing

**New workflow: ffm-latest-java-weekly.yml**
```yaml
name: Weekly FFM Latest Java Test

on:
  schedule:
    - cron: '0 2 * * 1'  # Every Monday at 2 AM UTC
  workflow_dispatch:

jobs:
  test-latest-java:
    name: "Test Latest Java with FFM"
    uses: ./.github/workflows/main.yml
    with:
      cmake_version: "latest"
      concurrent: ""
      thread_safety: ""
      build_mode: "Release"
      java_version: "latest"
      force_java_implementation: "ffm"
```

## Integration with Existing Infrastructure

### Compatibility Matrix

| Java Version | FFM Support | JNI Support | Default Implementation |
|--------------|-------------|-------------|----------------------|
| 11           | ❌          | ✅          | JNI                  |
| 17           | ❌          | ✅          | JNI                  |
| 21           | ❌          | ✅          | JNI                  |
| 24           | ✅          | ✅          | FFM                  |
| latest       | ✅          | ✅          | FFM                  |

### Configuration Logic

```cmake
# Enhanced Java version detection
if (DEFINED CACHE{FORCE_JAVA_VERSION})
  set(Java_VERSION_STRING ${FORCE_JAVA_VERSION})
endif()

if (DEFINED CACHE{FORCE_JAVA_IMPLEMENTATION})
  if (FORCE_JAVA_IMPLEMENTATION STREQUAL "ffm")
    if (Java_VERSION_STRING VERSION_LESS "24.0.0")
      message(FATAL_ERROR "FFM requires Java 24+, got ${Java_VERSION_STRING}")
    endif()
    set (HDF5_JAVA_USE_FFM TRUE)
  elseif (FORCE_JAVA_IMPLEMENTATION STREQUAL "jni")
    set (HDF5_JAVA_USE_FFM FALSE)
  endif()
endif()
```

## Testing Strategy

### Automated Testing Levels

1. **Pull Request Testing**: Current main workflow (default Java)
2. **Merge Testing**: Enhanced main workflow with Java version matrix
3. **Weekly Testing**: Latest Java version with FFM
4. **Release Testing**: Full Java version matrix with both implementations

### Test Coverage Matrix

| Test Type | Java 11 | Java 17 | Java 21 | Java 24 | Latest |
|-----------|---------|---------|---------|---------|--------|
| PR Tests  | JNI     | -       | -       | FFM     | -      |
| Merge     | JNI     | JNI     | JNI     | FFM     | -      |
| Weekly    | -       | -       | -       | -       | FFM    |
| Release   | JNI     | JNI     | JNI     | FFM/JNI | FFM    |

## Questions for Clarification

1. **Java "Latest" Definition**: Should "latest" mean the latest LTS (21) or the absolute latest (24+)?
   - **Recommendation**: Latest available (24+ currently), with fallback to 24 if newer versions aren't supported

2. **Testing Frequency**: How often should latest Java testing run?
   - **Recommendation**: Weekly for latest, daily for supported matrix

3. **Failure Handling**: How should the project handle failures with bleeding-edge Java versions?
   - **Recommendation**: Non-blocking for latest Java, blocking for supported versions

4. **Platform Priority**: Which platforms should be prioritized for latest Java testing?
   - **Recommendation**: Ubuntu first (fastest), then Windows and macOS

5. **Integration with Existing Maven Workflows**: Should this integrate with existing Maven testing?
   - **Recommendation**: Yes, leverage existing Maven infrastructure for artifact validation

## Suggested Improvements

### 1. Enhanced Error Reporting
- Add Java version detection logs
- Report implementation selection reasoning
- Include FFM-specific error handling

### 2. Artifact Differentiation
- Generate version-specific artifacts
- Include Java version in Maven artifact metadata
- Support parallel deployment of different Java versions

### 3. Performance Monitoring
- Track FFM vs JNI performance differences
- Monitor test execution times across Java versions
- Report compatibility regressions

### 4. Documentation Integration
- Update build documentation with Java version requirements
- Add FFM-specific troubleshooting guides
- Maintain compatibility matrix in repository

## Implementation Status

### ✅ Completed

1. **Enhanced main.yml workflow** with Java version inputs:
   - Added `java_version` input parameter (11, 17, 21, 24, latest, auto)
   - Added `force_java_implementation` input parameter (auto, ffm, jni)
   - Added Java setup steps for both workflows (Build_and_test and Static_build_and_test)
   - Added CMake configuration for Java implementation selection

2. **Added FFM latest Java testing** to call-workflows.yml:
   - New `call-ffm-latest-java` job
   - Non-blocking failures with `continue-on-error: true`
   - Tests all platforms in the main.yml matrix (Windows, Ubuntu, macOS)

### 🔄 Implementation Details

**Main Workflow Changes (`main.yml`):**
- Java setup uses `actions/setup-java@v4` with 'temurin' distribution
- "latest" maps to Java 24 (current latest with FFM support)
- CMake receives `-DHDF5_ENABLE_JNI:BOOL=ON` when JNI is forced
- Both shared and static build workflows updated

**Call Workflow Changes (`call-workflows.yml`):**
- New job runs as part of standard CI pipeline
- Uses `continue-on-error: true` for non-blocking failures
- Tests Java latest with FFM implementation specifically
- Triggers on all standard events (push, PR, manual dispatch)

### 📋 Usage Examples

**Manual Testing:**
```bash
# Test specific Java version with FFM
gh workflow run call-workflows.yml -f java_version=24 -f force_java_implementation=ffm

# Test latest Java with auto implementation selection
gh workflow run call-workflows.yml -f java_version=latest
```

**Custom Workflow Calls:**
```yaml
uses: ./.github/workflows/main.yml
with:
  cmake_version: "latest"
  build_mode: "Release"
  java_version: "latest"  # Java 24
  force_java_implementation: "ffm"
```

### 🎯 Next Steps

1. **Monitor FFM latest Java testing** in CI pipeline
2. **Update documentation** with new Java version requirements
3. **Consider weekly scheduling** if needed for additional coverage
4. **Enhanced reporting** - track FFM vs JNI performance differences

This implementation provides comprehensive FFM testing with the latest Java versions while maintaining full compatibility with existing infrastructure and ensuring non-blocking integration for bleeding-edge Java versions.