# Maven Repository Integration Planning

This document outlines the strategy for adding Maven repository upload capability to HDF5's release workflow.

## Current State Analysis

### Existing Release Workflow (`release.yml`)
The current release workflow follows a sequential pattern:
1. **Tarball Creation** (`tarball.yml`) - Creates source distributions
2. **AWS C-S3 Build** (`vfd-ros3.yml`) - Builds dependencies for S3 VFD testing
3. **CTest Execution** (`ctest.yml`) - Cross-platform testing with `ci-StdShar` preset
4. **ABI Compatibility** (`abi-report.yml`) - Binary compatibility validation against 1.14.5
5. **Release File Management** (`release-files.yml`) - Tag creation and artifact management

### Current Java/JAR Infrastructure
- **JAR Creation**: Uses CMake's `add_jar()` and `install_jar()` commands
- **Build Products**:
  - `jarhdf5-${VERSION}.jar` - Main HDF5 Java bindings
  - `jartest5-${VERSION}.jar` - Test utilities
  - SLF4J logging JARs (slf4j-api, slf4j-nop, slf4j-simple)
- **Installation Path**: `${HDF5_INSTALL_JAR_DIR}` (typically `lib/` or `../Java`)
- **Current Packaging**: JARs are included in CPack-generated installers but no Maven deployment

### Missing Maven Infrastructure
- **No POM generation**: No `pom.xml` files for Maven metadata
- **No Maven coordinates**: No groupId/artifactId/version structure
- **No repository configuration**: No settings for Maven repository deployment
- **No signing**: No GPG signing for Maven Central requirements

## Questions for Clarification

### Repository Target
1. **Which Maven repository?**
   - Maven Central (requires Sonatype OSSRH account, extensive requirements)
   - GitHub Packages (simpler, tied to GitHub repository)
   - Private/corporate repository (Nexus, Artifactory, etc.)
   - Multiple repositories (different environments)
Decision: Target Github Packages and add a plan for future change to Maven Central.

2. **Maven coordinates strategy:**
   - GroupId: `org.hdfgroup` or `org.hdfgroup.hdf5`?
   - ArtifactId: `hdf5-java`, `hdf5`, or separate artifacts?
   - Version mapping: Direct HDF5 version (`2.0.0-2`) or Java-specific versioning?
Decision: GroupId: `org.hdfgroup` ArtifactId: `hdf5-java` Version mapping: Direct HDF5 version (`2.0.0-2`)

### Integration Approach
3. **Workflow integration point:**
   - **Option A**: Add to existing `release.yml` as final step (after all validation)
   - **Option B**: Create separate `maven-deploy.yml` callable workflow
   - **Option C**: Integrate into `ctest.yml` during artifact creation
Decision: Option B: Create separate `maven-deploy.yml` callable workflow

4. **Artifact scope:**
   - Just Java JARs (hdf5-java bindings)?
   - Include native libraries (JNI .so/.dll/.dylib files)?
   - Source JARs and Javadoc JARs for Maven Central compliance?
   - Test artifacts (`jartest5`)?
Decision: Jars, native libs, Maven Central compliance. No test artifacts.

### Build Requirements
5. **Native library handling:**
   - Package platform-specific natives in separate classifier artifacts?
   - Use Maven's platform detection or custom solution?
   - Multi-platform deployment strategy (Windows/Linux/macOS)?
Decision: Need more investigation of pros and cons for a decision.

6. **POM generation strategy:**
   - Generate via CMake configure-time?
   - Template-based with version substitution?
   - Include HDF5 native library dependencies in POM?
Decision: CMake POM Generation

## Recommended Integration Strategy
Decision: Agree with recommended strategy after aligning with above decisions.

### Phase 1: Foundation (MVP)
**Goal**: Basic Maven deployment capability for Java artifacts

**Implementation**:
1. **CMake POM Generation**:
   ```cmake
   # Add to java/src/hdf/hdf5lib/CMakeLists.txt
   configure_file(
       ${CMAKE_CURRENT_SOURCE_DIR}/pom.xml.in
       ${CMAKE_CURRENT_BINARY_DIR}/pom.xml
       @ONLY
   )
   ```

2. **New Callable Workflow**: `maven-deploy.yml`
   - Triggered after successful `ctest.yml` completion
   - Downloads built artifacts from previous workflow runs
   - Uses GitHub Actions Maven deployment actions
   - Supports multiple repository targets via workflow inputs

3. **Integration Point**: Add to `release.yml` after `call-workflow-ctest`
   ```yaml
   call-workflow-maven:
     needs: [call-workflow-ctest]
     if: ${{ inputs.deploy_maven == 'true' }}
     uses: ./.github/workflows/maven-deploy.yml
     with:
       repository_url: ${{ inputs.maven_repo_url }}
       file_base: ${{ needs.call-workflow-tarball.outputs.file_base }}
     secrets:
       MAVEN_USERNAME: ${{ secrets.MAVEN_USERNAME }}
       MAVEN_PASSWORD: ${{ secrets.MAVEN_PASSWORD }}
   ```

### Phase 2: Production Ready
**Enhancements**:
- Multi-platform native library support
- Source and Javadoc JAR generation
- GPG signing for Maven Central
- Staging repository support for release validation

## Implementation Details

### Maven Coordinates Recommendation
```xml
<groupId>org.hdfgroup</groupId>
<artifactId>hdf5-java</artifactId>
<version>${HDF5_PACKAGE_VERSION}</version>
```

### POM Template Structure
```xml
<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0">
    <modelVersion>4.0.0</modelVersion>

    <groupId>org.hdfgroup</groupId>
    <artifactId>hdf5-java</artifactId>
    <version>@HDF5_PACKAGE_VERSION@</version>
    <packaging>jar</packaging>

    <name>HDF5 Java Bindings</name>
    <description>Java bindings for the HDF5 scientific data format library</description>
    <url>https://github.com/HDFGroup/hdf5</url>

    <licenses>
        <license>
            <name>BSD-style License</name>
            <url>https://github.com/HDFGroup/hdf5/blob/develop/LICENSE</url>
        </license>
    </licenses>

    <developers>
        <developer>
            <organization>The HDF Group</organization>
            <organizationUrl>https://www.hdfgroup.org</organizationUrl>
        </developer>
    </developers>

    <scm>
        <connection>scm:git:https://github.com/HDFGroup/hdf5.git</connection>
        <developerConnection>scm:git:git@github.com:HDFGroup/hdf5.git</developerConnection>
        <url>https://github.com/HDFGroup/hdf5</url>
    </scm>
</project>
```

### GitHub Actions Workflow Structure
```yaml
name: Maven Deploy
on:
  workflow_call:
    inputs:
      repository_url:
        description: 'Maven repository URL'
        required: true
        type: string
      file_base:
        description: 'Build artifact base name'
        required: true
        type: string
    secrets:
      MAVEN_USERNAME:
        required: true
      MAVEN_PASSWORD:
        required: true

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - name: Download JAR artifacts
        uses: actions/download-artifact@v4
        with:
          name: java-artifacts

      - name: Setup Java
        uses: actions/setup-java@v4
        with:
          java-version: '11'
          distribution: 'temurin'

      - name: Deploy to Maven Repository
        run: |
          mvn deploy:deploy-file \
            -DgroupId=org.hdfgroup \
            -DartifactId=hdf5-java \
            -Dversion=${{ env.HDF5_VERSION }} \
            -Dfile=jarhdf5-${{ env.HDF5_VERSION }}.jar \
            -DpomFile=pom.xml \
            -DrepositoryId=releases \
            -Durl=${{ inputs.repository_url }} \
            -s maven-settings.xml
        env:
          MAVEN_USERNAME: ${{ secrets.MAVEN_USERNAME }}
          MAVEN_PASSWORD: ${{ secrets.MAVEN_PASSWORD }}
```

## Risk Analysis and Considerations

### Technical Risks
1. **Artifact Availability**: Ensuring JARs are properly built and accessible across workflow jobs
2. **Version Consistency**: Maintaining version synchronization between HDF5 and Maven artifacts
3. **Multi-platform Complexity**: Handling native library dependencies across platforms
4. **Build Dependencies**: Managing CMake Java build requirements in CI environment

### Operational Risks
1. **Repository Access**: Securing and managing Maven repository credentials
2. **Release Coordination**: Ensuring Maven deployment doesn't interfere with existing release process
3. **Rollback Strategy**: Handling failed deployments and artifact cleanup
4. **Compliance**: Meeting repository-specific requirements (especially Maven Central)

### Maintenance Considerations
1. **Long-term Support**: Commitment to maintaining Maven artifacts alongside releases
2. **Versioning Strategy**: Handling pre-releases, snapshots, and version conflicts
3. **Documentation Updates**: User documentation for Maven dependency usage
4. **Compatibility**: Ensuring backward compatibility for existing JAR users

## Next Steps

1. **Clarify Requirements**: Answer repository target and scope questions above
2. **Prototype Development**: Create basic POM generation and test deployment workflow
3. **Repository Setup**: Establish Maven repository access and credentials
4. **Integration Testing**: Test workflow integration without affecting production releases
5. **Documentation**: Create user guides for consuming HDF5 via Maven
6. **Production Deployment**: Enable Maven deployment in release workflow

## Alternative Approaches

### CMake Target Approach
Instead of workflow-based deployment, create a CMake target:
```cmake
add_custom_target(maven-deploy
    COMMAND mvn deploy:deploy-file [args]
    DEPENDS ${HDF5_JAVA_HDF5_LIB_TARGET}
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
```

**Pros**: Simpler integration, local development support
**Cons**: Requires Maven installation on build systems, less CI integration

### Gradle Alternative
Consider Gradle instead of Maven for more flexible build logic:
- Better multi-platform support
- More powerful artifact customization
- Growing adoption in Java ecosystem

**Trade-offs**: Additional tooling complexity, less universal than Maven