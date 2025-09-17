# Maven Repository Integration Implementation Plan

This document provides a detailed implementation plan for adding Maven repository upload capability to HDF5's release workflow, based on the decisions made in `MAVEN_INTEGRATION_PLANNING.md`.

## Executive Summary

**Target**: GitHub Packages (Phase 1) with future migration path to Maven Central
**Scope**: Java JAR artifacts, native libraries, Maven Central compliance artifacts
**Integration**: Separate callable workflow (`maven-deploy.yml`)
**Coordinates**: `org.hdfgroup:hdf5-java:${HDF5_VERSION}`

## Implementation Phases

### Phase 1: GitHub Packages MVP (Target: Next Minor Release)

#### 1.1 CMake POM Generation Infrastructure

**Files to Create/Modify**:
- `java/src/hdf/hdf5lib/pom.xml.in` (new template file)
- `java/src/hdf/hdf5lib/CMakeLists.txt` (modify existing)
- `CMakeFilters.cmake` (minor addition for POM configuration)

**Implementation Details**:

```cmake
# Addition to java/src/hdf/hdf5lib/CMakeLists.txt
if(HDF5_BUILD_JAVA)
    # Generate Maven POM file
    configure_file(
        ${CMAKE_CURRENT_SOURCE_DIR}/pom.xml.in
        ${CMAKE_CURRENT_BINARY_DIR}/pom.xml
        @ONLY
    )

    # Install POM alongside JAR
    install(FILES ${CMAKE_CURRENT_BINARY_DIR}/pom.xml
        DESTINATION ${HDF5_INSTALL_JAR_DIR}
        COMPONENT Libraries
    )

    # Create Maven coordinates properties file
    set(MAVEN_GROUP_ID "org.hdfgroup")
    set(MAVEN_ARTIFACT_ID "hdf5-java")
    set(MAVEN_VERSION "${HDF5_PACKAGE_VERSION}")
endif()
```

**POM Template Structure** (`pom.xml.in`):
```xml
<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
         http://maven.apache.org/xsd/maven-4.0.0.xsd">
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
            <name>HDF5 License</name>
            <url>https://github.com/HDFGroup/hdf5/blob/develop/LICENSE</url>
            <distribution>repo</distribution>
        </license>
    </licenses>

    <organization>
        <name>The HDF Group</name>
        <url>https://www.hdfgroup.org</url>
    </organization>

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
        <tag>HEAD</tag>
    </scm>

    <properties>
        <maven.compiler.source>8</maven.compiler.source>
        <maven.compiler.target>8</maven.compiler.target>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
    </properties>

    <dependencies>
        <!-- SLF4J API for logging -->
        <dependency>
            <groupId>org.slf4j</groupId>
            <artifactId>slf4j-api</artifactId>
            <version>1.7.36</version>
        </dependency>
    </dependencies>
</project>
```

#### 1.2 Maven Deploy Callable Workflow

**File**: `.github/workflows/maven-deploy.yml`

```yaml
name: Maven Deploy

on:
  workflow_call:
    inputs:
      repository_url:
        description: 'Maven repository URL (GitHub Packages or Maven Central)'
        required: true
        type: string
      repository_id:
        description: 'Maven repository ID for settings.xml'
        required: true
        type: string
      file_base:
        description: 'Build artifact base name from tarball workflow'
        required: true
        type: string
      dry_run:
        description: 'Perform dry run without actual deployment'
        required: false
        type: boolean
        default: false
      deploy_snapshots:
        description: 'Deploy snapshot versions'
        required: false
        type: boolean
        default: false
    secrets:
      MAVEN_USERNAME:
        description: 'Maven repository username'
        required: true
      MAVEN_PASSWORD:
        description: 'Maven repository password/token'
        required: true
      GPG_PRIVATE_KEY:
        description: 'GPG private key for signing (Maven Central)'
        required: false
      GPG_PASSPHRASE:
        description: 'GPG passphrase for signing'
        required: false

env:
  MAVEN_OPTS: "-Xmx1024m"

jobs:
  validate-inputs:
    runs-on: ubuntu-latest
    outputs:
      should_deploy: ${{ steps.check.outputs.should_deploy }}
      maven_version: ${{ steps.version.outputs.maven_version }}
    steps:
      - name: Validate deployment conditions
        id: check
        run: |
          # Don't deploy snapshots unless explicitly enabled
          if [[ "${{ inputs.file_base }}" == *"SNAPSHOT"* ]] && [[ "${{ inputs.deploy_snapshots }}" != "true" ]]; then
            echo "should_deploy=false" >> $GITHUB_OUTPUT
            echo "Skipping snapshot deployment (not enabled)"
          else
            echo "should_deploy=true" >> $GITHUB_OUTPUT
          fi

      - name: Extract version
        id: version
        run: |
          # Extract version from file_base (e.g., "hdf5-2.0.0-2" -> "2.0.0-2")
          VERSION=$(echo "${{ inputs.file_base }}" | sed 's/^hdf5-//')
          echo "maven_version=${VERSION}" >> $GITHUB_OUTPUT

  deploy-java-artifacts:
    needs: validate-inputs
    if: needs.validate-inputs.outputs.should_deploy == 'true'
    runs-on: ubuntu-latest
    strategy:
      matrix:
        platform: [ubuntu-latest, windows-latest, macos-latest]
    steps:
      - name: Checkout repository
        uses: actions/checkout@v4

      - name: Download artifacts
        uses: actions/download-artifact@v4
        with:
          pattern: "*Java*"
          merge-multiple: true

      - name: Setup Java
        uses: actions/setup-java@v4
        with:
          java-version: '11'
          distribution: 'temurin'

      - name: Setup Maven settings
        run: |
          mkdir -p ~/.m2
          cat > ~/.m2/settings.xml << 'EOF'
          <?xml version="1.0" encoding="UTF-8"?>
          <settings xmlns="http://maven.apache.org/SETTINGS/1.0.0">
            <servers>
              <server>
                <id>${{ inputs.repository_id }}</id>
                <username>${{ secrets.MAVEN_USERNAME }}</username>
                <password>${{ secrets.MAVEN_PASSWORD }}</password>
              </server>
            </servers>
          </settings>
          EOF

      - name: Import GPG key (if provided)
        if: secrets.GPG_PRIVATE_KEY != ''
        run: |
          echo "${{ secrets.GPG_PRIVATE_KEY }}" | gpg --batch --import
          echo "GPG_ENABLED=true" >> $GITHUB_ENV

      - name: Deploy main JAR
        run: |
          DEPLOY_CMD="mvn deploy:deploy-file"
          DEPLOY_CMD="$DEPLOY_CMD -DgroupId=org.hdfgroup"
          DEPLOY_CMD="$DEPLOY_CMD -DartifactId=hdf5-java"
          DEPLOY_CMD="$DEPLOY_CMD -Dversion=${{ needs.validate-inputs.outputs.maven_version }}"
          DEPLOY_CMD="$DEPLOY_CMD -Dfile=jarhdf5-${{ needs.validate-inputs.outputs.maven_version }}.jar"
          DEPLOY_CMD="$DEPLOY_CMD -DpomFile=pom.xml"
          DEPLOY_CMD="$DEPLOY_CMD -DrepositoryId=${{ inputs.repository_id }}"
          DEPLOY_CMD="$DEPLOY_CMD -Durl=${{ inputs.repository_url }}"

          # Add signing if GPG is available
          if [[ "$GPG_ENABLED" == "true" ]]; then
            DEPLOY_CMD="$DEPLOY_CMD -Dgpg.passphrase=${{ secrets.GPG_PASSPHRASE }}"
          fi

          if [[ "${{ inputs.dry_run }}" == "true" ]]; then
            echo "DRY RUN: Would execute: $DEPLOY_CMD"
          else
            eval $DEPLOY_CMD
          fi

      - name: Deploy sources JAR (if available)
        if: hashFiles('*-sources.jar') != ''
        run: |
          SOURCES_JAR=$(ls *-sources.jar | head -1)
          DEPLOY_CMD="mvn deploy:deploy-file"
          DEPLOY_CMD="$DEPLOY_CMD -DgroupId=org.hdfgroup"
          DEPLOY_CMD="$DEPLOY_CMD -DartifactId=hdf5-java"
          DEPLOY_CMD="$DEPLOY_CMD -Dversion=${{ needs.validate-inputs.outputs.maven_version }}"
          DEPLOY_CMD="$DEPLOY_CMD -Dfile=$SOURCES_JAR"
          DEPLOY_CMD="$DEPLOY_CMD -Dclassifier=sources"
          DEPLOY_CMD="$DEPLOY_CMD -DrepositoryId=${{ inputs.repository_id }}"
          DEPLOY_CMD="$DEPLOY_CMD -Durl=${{ inputs.repository_url }}"

          if [[ "${{ inputs.dry_run }}" == "true" ]]; then
            echo "DRY RUN: Would execute: $DEPLOY_CMD"
          else
            eval $DEPLOY_CMD
          fi

      - name: Deploy Javadoc JAR (if available)
        if: hashFiles('*-javadoc.jar') != ''
        run: |
          JAVADOC_JAR=$(ls *-javadoc.jar | head -1)
          DEPLOY_CMD="mvn deploy:deploy-file"
          DEPLOY_CMD="$DEPLOY_CMD -DgroupId=org.hdfgroup"
          DEPLOY_CMD="$DEPLOY_CMD -DartifactId=hdf5-java"
          DEPLOY_CMD="$DEPLOY_CMD -Dversion=${{ needs.validate-inputs.outputs.maven_version }}"
          DEPLOY_CMD="$DEPLOY_CMD -Dfile=$JAVADOC_JAR"
          DEPLOY_CMD="$DEPLOY_CMD -Dclassifier=javadoc"
          DEPLOY_CMD="$DEPLOY_CMD -DrepositoryId=${{ inputs.repository_id }}"
          DEPLOY_CMD="$DEPLOY_CMD -Durl=${{ inputs.repository_url }}"

          if [[ "${{ inputs.dry_run }}" == "true" ]]; then
            echo "DRY RUN: Would execute: $DEPLOY_CMD"
          else
            eval $DEPLOY_CMD
          fi
```

#### 1.3 Release Workflow Integration

**File**: `.github/workflows/release.yml` (modify existing)

Add after the `call-workflow-ctest` job:

```yaml
  call-workflow-maven-github:
    needs: [call-workflow-tarball, call-workflow-ctest]
    if: ${{ inputs.deploy_maven == 'true' || github.event_name == 'release' }}
    uses: ./.github/workflows/maven-deploy.yml
    with:
      repository_url: "https://maven.pkg.github.com/HDFGroup/hdf5"
      repository_id: "github"
      file_base: ${{ needs.call-workflow-tarball.outputs.file_base }}
      dry_run: ${{ inputs.dry_run_maven || false }}
    secrets:
      MAVEN_USERNAME: ${{ github.actor }}
      MAVEN_PASSWORD: ${{ secrets.GITHUB_TOKEN }}
```

Add new workflow inputs:
```yaml
  deploy_maven:
    description: 'Deploy artifacts to Maven repository'
    required: false
    type: boolean
    default: false
  dry_run_maven:
    description: 'Perform Maven deployment dry run'
    required: false
    type: boolean
    default: false
```

### Phase 2: Maven Central Preparation (Future Release)

#### 2.1 Enhanced Artifact Generation

**Modify**: `java/src/hdf/hdf5lib/CMakeLists.txt`

Add source and javadoc JAR generation:

```cmake
if(HDF5_BUILD_JAVA AND HDF5_ENABLE_MAVEN_ARTIFACTS)
    # Generate sources JAR
    add_custom_target(hdf5-java-sources
        COMMAND ${CMAKE_COMMAND} -E tar "cfv"
                "${CMAKE_CURRENT_BINARY_DIR}/jarhdf5-${HDF5_PACKAGE_VERSION}-sources.jar"
                --format=zip
                "${CMAKE_CURRENT_SOURCE_DIR}"
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMENT "Creating sources JAR"
    )

    # Generate Javadoc JAR (requires javadoc tool)
    find_program(JAVADOC_EXECUTABLE javadoc)
    if(JAVADOC_EXECUTABLE)
        add_custom_target(hdf5-java-javadoc
            COMMAND ${JAVADOC_EXECUTABLE} -d "${CMAKE_CURRENT_BINARY_DIR}/javadoc"
                    -sourcepath "${CMAKE_CURRENT_SOURCE_DIR}"
                    -subpackages hdf.hdf5lib
            COMMAND ${CMAKE_COMMAND} -E tar "cfv"
                    "${CMAKE_CURRENT_BINARY_DIR}/jarhdf5-${HDF5_PACKAGE_VERSION}-javadoc.jar"
                    --format=zip
                    "${CMAKE_CURRENT_BINARY_DIR}/javadoc"
            COMMENT "Creating Javadoc JAR"
        )
    endif()
endif()
```

#### 2.2 Native Library Packaging Strategy

**Research Required**: Multi-platform native library distribution

**Options Analysis**:
1. **Classifier-based approach** (Recommended):
   - `hdf5-java-2.0.0-linux-x86_64.jar`
   - `hdf5-java-2.0.0-windows-x86_64.jar`
   - `hdf5-java-2.0.0-macos-x86_64.jar`

2. **Fat JAR approach**:
   - Single JAR with all platforms
   - Runtime platform detection

3. **Separate native artifacts**:
   - Pure Java JAR + separate native dependencies

## Implementation Decisions (Approved)

### 1. Native Library Distribution Strategy ✅

**Decision**: Platform-specific JARs with classifiers

**Implementation**:
- `hdf5-java-{version}-linux-x86_64.jar`
- `hdf5-java-{version}-windows-x86_64.jar`
- `hdf5-java-{version}-macos-x86_64.jar`
- `hdf5-java-{version}-macos-aarch64.jar`

**Benefits**: Better dependency management, smaller downloads, explicit platform targeting

### 2. Artifact Availability in CI ✅

**Decision**: Use `ctest.yml` workflow with `ci-StdShar` preset as artifact source

**Implementation**:
- Maven deployment workflow will download artifacts from `ctest.yml` runs
- Artifacts include both JAR files and platform-specific native libraries
- Ensure artifact naming consistency between build and deployment workflows

### 3. Version Handling for Snapshots ✅

**Decision**: Use `-SNAPSHOT` suffix for development builds

**Implementation**:
- Release versions: Use HDF5 version directly (`2.0.0-2`)
- Development builds: Append `-SNAPSHOT` (`2.0.0-3-SNAPSHOT`)
- Workflow logic will detect and handle snapshot versions appropriately

### 4. Maven Central Migration Timeline ✅

**Decision**: Target Maven Central after initial GitHub Packages implementation and testing

**Timeline**:
- Phase 1: GitHub Packages MVP (immediate priority)
- Phase 2: Maven Central preparation (after successful Phase 1 deployment)
- Requires OSSRH account setup, GPG signing infrastructure, staging repository workflow

### 5. Backward Compatibility ✅

**Decision**: Maintain existing JAR distribution while adding Maven deployment

**Implementation**:
- CPack-generated installers continue to include JARs
- No changes to existing JAR installation paths
- Maven deployment is additive, not replacement
- Users can choose between traditional installers or Maven dependency management

## Approved Implementation Enhancements

### 1. Enhanced Error Handling and Validation ✅

**Implementation**:
```yaml
  pre-deployment-validation:
    runs-on: ubuntu-latest
    steps:
      - name: Validate Java artifacts exist
        run: |
          if [[ ! -f "jarhdf5-*.jar" ]]; then
            echo "ERROR: Main JAR artifact not found"
            exit 1
          fi
      - name: Validate POM completeness
        run: |
          xmllint --noout pom.xml || { echo "Invalid POM"; exit 1; }
      - name: Validate version consistency
        run: |
          JAR_VERSION=$(echo jarhdf5-*.jar | sed 's/jarhdf5-\(.*\)\.jar/\1/')
          POM_VERSION=$(xmllint --xpath "//version/text()" pom.xml)
          [[ "$JAR_VERSION" == "$POM_VERSION" ]] || { echo "Version mismatch"; exit 1; }
```

### 2. Staging Repository for Release Validation ✅

**Implementation**:
- Add staging deployment for PR builds
- Use GitHub Packages staging area for validation
- Manual promotion step for production releases
- Automated cleanup of failed staging deployments

### 3. Integration Testing for Maven Artifacts ✅

**Implementation**:
```yaml
  test-maven-integration:
    needs: [deploy-maven]
    runs-on: ubuntu-latest
    steps:
      - name: Create test Maven project
        run: |
          mkdir test-project && cd test-project
          cat > pom.xml << 'EOF'
          <project xmlns="http://maven.apache.org/POM/4.0.0">
            <modelVersion>4.0.0</modelVersion>
            <groupId>test</groupId>
            <artifactId>hdf5-integration-test</artifactId>
            <version>1.0</version>
            <dependencies>
              <dependency>
                <groupId>org.hdfgroup</groupId>
                <artifactId>hdf5-java</artifactId>
                <version>${{ needs.deploy-maven.outputs.version }}</version>
              </dependency>
            </dependencies>
          </project>
          EOF
      - name: Test Maven dependency resolution
        run: |
          cd test-project
          mvn dependency:resolve
          mvn compile
      - name: Test basic HDF5 functionality
        run: |
          cd test-project
          cat > src/main/java/Test.java << 'EOF'
          import hdf.hdf5lib.H5;
          public class Test {
            public static void main(String[] args) {
              System.out.println("HDF5 Version: " + H5.H5get_libversion());
            }
          }
          EOF
          mvn exec:java -Dexec.mainClass="Test"
```

### 4. Rollback and Recovery Mechanisms ✅

**Implementation**:
```yaml
  rollback-maven-deployment:
    if: failure() && github.event_name == 'release'
    runs-on: ubuntu-latest
    steps:
      - name: Delete failed GitHub Packages deployment
        run: |
          gh api --method DELETE \
            /orgs/HDFGroup/packages/maven/org.hdfgroup.hdf5-java/versions/$VERSION
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
      - name: Create rollback issue
        uses: actions/github-script@v7
        with:
          script: |
            github.rest.issues.create({
              owner: context.repo.owner,
              repo: context.repo.repo,
              title: 'Maven deployment rollback required',
              body: 'Automated rollback of failed Maven deployment for version ${{ env.VERSION }}'
            })
```

### 5. Multi-Repository Deployment Strategy ✅

**Implementation**:
```yaml
  deploy-maven-matrix:
    strategy:
      matrix:
        repository:
          - name: "GitHub Packages"
            url: "https://maven.pkg.github.com/HDFGroup/hdf5"
            id: "github"
            secrets_suffix: "_GITHUB"
          - name: "Maven Central"
            url: "https://oss.sonatype.org/service/local/staging/deploy/maven2/"
            id: "ossrh"
            secrets_suffix: "_CENTRAL"
            requires_signing: true
    steps:
      - name: Deploy to ${{ matrix.repository.name }}
        run: |
          mvn deploy:deploy-file \
            -DrepositoryId=${{ matrix.repository.id }} \
            -Durl=${{ matrix.repository.url }} \
            [additional parameters]
        env:
          MAVEN_USERNAME: ${{ secrets[format('MAVEN_USERNAME{0}', matrix.repository.secrets_suffix)] }}
          MAVEN_PASSWORD: ${{ secrets[format('MAVEN_PASSWORD{0}', matrix.repository.secrets_suffix)] }}
```

### 6. Performance and Reliability Optimizations ✅

**Implementation**:
- Parallel artifact uploads using matrix strategy
- Exponential backoff retry logic for network failures
- Checksum verification for artifact integrity
- Compressed artifact transfer
- Artifact caching between workflow runs

## Implementation Timeline (Updated)

### Sprint 1 (Weeks 1-2): Foundation
- [ ] CMake POM generation implementation
- [ ] Basic `maven-deploy.yml` workflow creation
- [ ] GitHub Packages integration testing
- [ ] Platform-specific JAR classifier implementation
- [ ] Enhanced validation framework

### Sprint 2 (Weeks 3-4): Integration
- [ ] Release workflow integration with `ctest.yml` artifacts
- [ ] CI artifact flow validation
- [ ] Snapshot version handling (`-SNAPSHOT` suffix)
- [ ] Staging repository workflow

### Sprint 3 (Weeks 5-6): Enhancement
- [ ] Integration testing framework implementation
- [ ] Multi-repository deployment strategy
- [ ] Performance optimizations (parallel uploads, retry logic)
- [ ] Rollback and recovery mechanisms

### Sprint 4 (Weeks 7-8): Production Readiness
- [ ] Comprehensive testing across all platforms
- [ ] Documentation and user guides
- [ ] Maven Central preparation (OSSRH account, GPG setup)
- [ ] Final validation and production deployment

## Success Metrics

### Phase 1 Targets
- [ ] Successful deployment to GitHub Packages
- [ ] Zero breaking changes to existing release process
- [ ] Sub-10-minute deployment time
- [ ] 99% deployment success rate

### Phase 2 Targets
- [ ] Maven Central compliance achieved
- [ ] Multi-platform native library support (classifiers implemented)
- [ ] Integration test coverage >90%
- [ ] User adoption metrics tracked
- [ ] Backward compatibility maintained with existing JAR distribution

## Next Steps

1. **Immediate Actions**:
   - Review and approve implementation plan
   - Set up GitHub Packages repository access
   - Create development branch for Maven integration

2. **First Implementation Sprint**:
   - Implement CMake POM generation
   - Create basic Maven deployment workflow
   - Test with development builds

3. **Validation Phase**:
   - Deploy test artifacts to GitHub Packages
   - Validate integration with existing CI/CD
   - Conduct user acceptance testing

4. **Production Rollout**:
   - Enable Maven deployment in release workflow
   - Monitor deployment success metrics
   - Gather user feedback and iterate
