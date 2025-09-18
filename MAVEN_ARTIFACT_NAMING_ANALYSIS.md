# Maven Artifact Naming Strategy Analysis

This document analyzes different approaches to naming and packaging HDF5 Java artifacts for Maven deployment.

## Current Implementation

### Current Naming Pattern
- **GroupId**: `org.hdfgroup`
- **ArtifactId**: `hdf5-java`
- **Version**: Direct HDF5 version mapping (e.g., `2.0.0-2`)
- **Classifiers**: Platform-specific (`linux-x86_64`, `windows-x86_64`, `macos-x86_64`, `macos-aarch64`)

### Current Structure
```xml
<dependency>
    <groupId>org.hdfgroup</groupId>
    <artifactId>hdf5-java</artifactId>
    <version>2.0.0-2</version>
    <classifier>linux-x86_64</classifier>
</dependency>
```

## Alternative Approaches

### Option 1: Universal JAR (Current++)
**Description**: Single JAR containing native libraries for all platforms

**Naming**:
```xml
<dependency>
    <groupId>org.hdfgroup</groupId>
    <artifactId>hdf5-java</artifactId>
    <version>2.0.0-2</version>
    <!-- No classifier needed -->
</dependency>
```

**Pros**:
- **Simplicity**: Single dependency for all platforms
- **Ease of use**: No platform detection required by users
- **Maven best practice**: Follows standard Maven conventions

**Cons**:
- **Large artifact size**: Contains natives for all platforms (~20-50MB)
- **Download overhead**: Users download unused platform libraries
- **Complex packaging**: Requires cross-compilation or artifact aggregation
- **Runtime complexity**: Needs platform detection logic for native loading

**Implementation Requirements**:
- Cross-compilation setup for all target platforms in CI
- Native library extraction and loading logic
- Platform detection at runtime

### Option 2: Platform-Specific Artifacts (Current Implementation)
**Description**: Separate artifacts per platform with classifiers

**Pros**:
- **Minimal size**: Only includes natives for target platform
- **Clear separation**: Explicit platform targeting
- **Build simplicity**: Each platform builds its own artifact
- **Selective deployment**: Can deploy subset of platforms

**Cons**:
- **User complexity**: Requires platform-specific dependency management
- **Maven tooling issues**: Some tools don't handle classifiers well
- **Documentation overhead**: Need to explain classifier usage

### Option 3: Separate ArtifactIds per Platform
**Description**: Different artifactIds for each platform

**Naming**:
```xml
<!-- Linux -->
<dependency>
    <groupId>org.hdfgroup</groupId>
    <artifactId>hdf5-java-linux</artifactId>
    <version>2.0.0-2</version>
</dependency>

<!-- Windows -->
<dependency>
    <groupId>org.hdfgroup</groupId>
    <artifactId>hdf5-java-windows</artifactId>
    <version>2.0.0-2</version>
</dependency>
```

**Pros**:
- **Clear platform targeting**: Obvious from artifactId
- **Tooling compatibility**: Works with all Maven tooling
- **Size optimization**: Platform-specific artifacts

**Cons**:
- **Proliferation of artifacts**: Multiple entries in repository
- **Version management complexity**: Need to sync versions across artifacts
- **User confusion**: Multiple similar artifacts

### Option 4: Hybrid Approach (BOM + Platform Artifacts)
**Description**: Bill of Materials (BOM) + platform-specific artifacts

**Naming**:
```xml
<!-- BOM Import -->
<dependencyManagement>
    <dependencies>
        <dependency>
            <groupId>org.hdfgroup</groupId>
            <artifactId>hdf5-java-bom</artifactId>
            <version>2.0.0-2</version>
            <type>pom</type>
            <scope>import</scope>
        </dependency>
    </dependencies>
</dependencyManagement>

<!-- Platform-specific dependency -->
<dependency>
    <groupId>org.hdfgroup</groupId>
    <artifactId>hdf5-java</artifactId>
    <classifier>${platform.classifier}</classifier>
</dependency>
```

**Pros**:
- **Version management**: Centralized version management via BOM
- **Flexibility**: Can choose universal or platform-specific
- **Professional approach**: Used by major libraries (Jackson, etc.)

**Cons**:
- **Complexity**: More artifacts to manage
- **User learning curve**: Need to understand BOM concept

## Decision Framework

### Factors for Consideration

#### 1. User Experience
- **Ease of dependency declaration**
- **Documentation requirements**
- **IDE support and autocomplete**
- **Build tool compatibility**

#### 2. Operational Complexity
- **CI/CD build matrix requirements**
- **Artifact storage costs**
- **Release coordination complexity**
- **Version management overhead**

#### 3. Technical Requirements
- **Native library size constraints**
- **Network bandwidth considerations**
- **Platform coverage needs**
- **Backward compatibility requirements**

#### 4. Ecosystem Alignment
- **Maven Central best practices**
- **Industry standard approaches**
- **Similar libraries' strategies**

## Recommendations by Use Case

### Current Recommendation: Keep Platform-Specific Classifiers (Option 2)
**Rationale**:
- Balances simplicity with flexibility
- Minimizes artifact sizes
- Leverages existing Maven classifier mechanism
- Allows incremental enhancement to universal JAR later

### Short-term Improvements:
1. **Enhanced documentation**: Clear examples for each platform
2. **Build profiles**: Maven profiles to simplify platform selection
3. **Automated platform detection**: Gradle plugin or Maven extension

### Long-term Evolution Path:
1. **Phase 1**: Optimize current classifier approach
2. **Phase 2**: Add universal JAR as additional artifact (no classifier)
3. **Phase 3**: Consider BOM approach for advanced users
4. **Phase 4**: Deprecate classified artifacts in favor of universal (if adoption is good)

## Implementation Recommendations

### Immediate Actions:
- Document classifier usage patterns
- Create example projects for each platform
- Add platform detection utilities

### Future Enhancements:
- Investigate feasibility of universal JAR
- Prototype BOM-based approach
- Gather user feedback on current approach

## Migration Considerations

### If changing from current approach:
1. **Deprecation period**: Maintain current artifacts for 2+ releases
2. **Documentation updates**: Clear migration guides
3. **Tooling support**: Update any existing plugins or utilities
4. **User notification**: Announce changes well in advance

### Backward compatibility strategy:
- Keep existing classifier-based artifacts
- Add new artifacts alongside existing ones
- Gradual migration over multiple releases
- Clear communication about preferred approach

## Conclusion

The current classifier-based approach provides a good balance of simplicity and functionality. Future enhancements should focus on improving user experience rather than fundamental changes to the naming strategy.

**Status**: Current implementation recommended for continued use
**Next Review**: After gathering user feedback and Maven Central deployment experience