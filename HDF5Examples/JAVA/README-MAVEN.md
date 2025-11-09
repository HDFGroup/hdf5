# HDF5 Java Examples Maven Integration

This directory contains Java examples demonstrating the usage of HDF5 Java bindings, organized into categories and deployable as a Maven artifact.

## Directory Structure

```
HDF5Examples/JAVA/
├── H5D/          # Dataset operations examples
├── H5T/          # Datatype operations examples
├── H5G/          # Group operations examples
├── TUTR/         # Tutorial examples
├── pom-examples.xml.in    # Maven POM template for examples
├── CMakeLists.txt         # CMake configuration
└── README-MAVEN.md        # This file
```

## Maven Artifact Usage

### Using Examples as Dependency

```xml
<dependency>
    <groupId>org.hdfgroup</groupId>
    <artifactId>hdf5-java-examples</artifactId>
    <version>2.0.0</version>
</dependency>
```

### Platform-Specific Dependencies

The examples depend on platform-specific HDF5 Java libraries:

```xml
<!-- Linux -->
<dependency>
    <groupId>org.hdfgroup</groupId>
    <artifactId>hdf5-java</artifactId>
    <version>2.0.0</version>
    <classifier>linux-x86_64</classifier>
</dependency>

<!-- Windows -->
<dependency>
    <groupId>org.hdfgroup</groupId>
    <artifactId>hdf5-java</artifactId>
    <version>2.0.0</version>
    <classifier>windows-x86_64</classifier>
</dependency>

<!-- macOS Intel -->
<dependency>
    <groupId>org.hdfgroup</groupId>
    <artifactId>hdf5-java</artifactId>
    <version>2.0.0</version>
    <classifier>macos-x86_64</classifier>
</dependency>

<!-- macOS Apple Silicon -->
<dependency>
    <groupId>org.hdfgroup</groupId>
    <artifactId>hdf5-java</artifactId>
    <version>2.0.0</version>
    <classifier>macos-aarch64</classifier>
</dependency>
```

## Building Examples with Maven

### Compile All Examples

```bash
cd HDF5Examples/JAVA
mvn compile -f pom-examples.xml
```

### Run Representative Examples

```bash
mvn test -Prun-examples -f pom-examples.xml
```

### Create Examples JAR

```bash
mvn package -f pom-examples.xml
```

This creates:
- `hdf5-java-examples-{version}.jar` - Compiled examples
- `hdf5-java-examples-{version}-sources.jar` - Source code
- `hdf5-java-examples-{version}-javadoc.jar` - Documentation

## Testing Maven Artifacts

Two standalone scripts are provided to test HDF5 Maven artifacts against the examples in this directory:

### test-maven-jni.sh - Test JNI Implementation

Tests the JNI (Java Native Interface) implementation, compatible with Java 11+.

**Usage:**
```bash
./test-maven-jni.sh [VERSION] [REPOSITORY_URL] [BUILD_DIR]
```

**Examples:**
```bash
# Test latest snapshot from HDFGroup
./test-maven-jni.sh 2.0.1-SNAPSHOT

# Test specific version from custom repository
./test-maven-jni.sh 2.0.0 https://maven.pkg.github.com/myorg/hdf5

# Use custom build directory
./test-maven-jni.sh 2.0.1-SNAPSHOT https://maven.pkg.github.com/HDFGroup/hdf5 /tmp/test
```

**What it does:**
1. Downloads `hdf5-java-jni` artifact from Maven repository
2. Verifies JAR contains HDF5 classes (not just dependencies)
3. Compiles all 55 HDF5 v2.0+ examples from `compat/` subdirectories
4. Executes 12 comprehensive tests covering major HDF5 features
5. Reports results with detailed pass/fail summary

**Prerequisites:**
- Java 21 or later (class version 65.0)
- Maven 3.6.0 or later
- GitHub authentication (for GitHub Packages)
- Optional: HDF5 native libraries or `HDF5_HOME` for execution tests

### test-maven-ffm.sh - Test FFM Implementation

Tests the FFM (Foreign Function & Memory) implementation, requires Java 25+.

**Usage:**
```bash
./test-maven-ffm.sh [VERSION] [REPOSITORY_URL] [BUILD_DIR]
```

**Examples:**
```bash
# Test FFM snapshot
./test-maven-ffm.sh 2.0.1-SNAPSHOT

# Test specific version
./test-maven-ffm.sh 2.0.0-3 https://maven.pkg.github.com/HDFGroup/hdf5
```

**What it does:**
1. Downloads `hdf5-java-ffm` artifact from Maven repository
2. Verifies JAR contains FFM bindings (`org.hdfgroup.javahdf5.*`)
3. Compiles 52 HDF5 v2.0+ examples from `compat/` subdirectories
4. Executes 12 comprehensive tests covering major HDF5 features
5. Reports results with detailed pass/fail summary

**Note:** 3 callback-based examples are excluded (H5Ex_G_Visit, H5Ex_G_Intermediate, H5Ex_G_Traverse) as FFM callback handling differs from JNI and these examples have not yet been adapted.

**Prerequisites:**
- Java 25 or later (class version 69.0)
- Maven 3.6.0 or later
- GitHub authentication (for GitHub Packages)
- Optional: HDF5 native libraries or `HDF5_HOME` for execution tests

### Build Directory Pattern

Both scripts use a separate build directory to keep the source tree clean:

**Default locations:**
- JNI: `HDF5Examples/JAVA/build/maven-test-jni/`
- FFM: `HDF5Examples/JAVA/build/maven-test-ffm/`

**Generated files:**
```
build/
├── maven-test-jni/
│   ├── pom-examples.xml       # Generated Maven POM
│   ├── target/                # Compiled classes
│   │   └── classes/
│   └── *.h5                   # Output HDF5 files
└── maven-test-ffm/
    ├── pom-examples.xml
    ├── target/
    └── *.h5
```

**Benefits:**
- ✅ Source tree stays clean (no generated files)
- ✅ Easy cleanup: `rm -rf build/`
- ✅ Multiple parallel tests possible
- ✅ CMake-like out-of-source build pattern

### GitHub Authentication

For testing artifacts from GitHub Packages, authentication is required:

**Option 1: GitHub CLI (Recommended)**
```bash
gh auth login
gh auth refresh --scopes read:packages
```

Scripts automatically detect GitHub CLI authentication.

**Option 2: Maven settings.xml**
```bash
# Scripts can create settings.xml automatically if gh is authenticated
# Or create manually:
cat > ~/.m2/settings.xml <<EOF
<settings>
  <servers>
    <server>
      <id>github-hdfgroup-hdf5</id>
      <username>YOUR_GITHUB_USERNAME</username>
      <password>YOUR_GITHUB_TOKEN</password>
    </server>
  </servers>
</settings>
EOF
```

### Running Additional Examples

After initial test succeeds, you can run more examples:

**JNI:**
```bash
cd build/maven-test-jni
mvn exec:java -Dexec.mainClass="H5Ex_T_String" -f pom-examples.xml
```

**FFM:**
```bash
cd build/maven-test-ffm
mvn exec:java -Dexec.mainClass="H5Ex_T_String" -f pom-examples.xml
```

### Cleanup

**Remove single test build:**
```bash
rm -rf build/maven-test-jni
rm -rf build/maven-test-ffm
```

**Remove all test builds:**
```bash
rm -rf build/
```

**Clean with Maven (keeps directory structure):**
```bash
mvn clean -f build/maven-test-jni/pom-examples.xml
mvn clean -f build/maven-test-ffm/pom-examples.xml
```

### Troubleshooting Test Scripts

**"Failed to download artifact"**
- Check GitHub authentication: `gh auth status`
- Verify repository URL is correct
- Ensure version exists in repository

**"JAR does not contain HDF5 classes"**
- Indicates incomplete Maven artifact (build issue)
- This is what the verification step catches!
- Report to maintainers if public artifact is incomplete

**"Java version too old"**
- JNI requires Java 11+
- FFM requires Java 25+
- Check: `java -version`

**"UnsatisfiedLinkError: no hdf5_java"**
- This is expected during Maven-only testing
- Indicates JAR structure is correct
- Native libraries would be needed for full execution

## Example Categories

### H5D - Dataset Operations
- Basic read/write operations
- Chunking and compression
- External storage
- Fill values and allocation
- Filters (gzip, checksum, nbit, etc.)

### H5T - Datatype Operations
- Array datatypes
- Compound datatypes
- Enumerated datatypes
- Opaque datatypes
- String handling
- Variable-length datatypes

### H5G - Group Operations
- Creating and managing groups
- Group iteration
- Intermediate group creation
- Group hierarchy traversal

### TUTR - Tutorial Examples
- Step-by-step learning examples
- Basic concepts demonstration
- Progressive complexity

## CI/CD Integration

The examples are automatically tested in CI:

1. **Compilation Testing**: All examples must compile successfully
2. **Execution Testing**: Examples are run and output validated
3. **Cross-Platform Testing**: Tested on Linux, Windows, and macOS
4. **Maven Integration Testing**: Tests against staging Maven artifacts

### Maven-Only Testing Behavior

**Expected Native Library Errors**: During Maven-only testing (without HDF5 installation), examples will compile successfully but fail at runtime with:
```
UnsatisfiedLinkError: no hdf5_java in java.library.path
```

This is **expected behavior** and indicates:
- ✅ **JAR structure is correct**
- ✅ **Dependencies resolve properly**
- ✅ **Compilation succeeds**
- ⚠️ **Native HDF5 libraries not available** (expected in Maven-only environment)

### Running Examples Successfully

To actually execute examples (not just compile them), you need HDF5 native libraries installed:

#### Option 1: Install HDF5 from Package Manager (Recommended)

**Linux (Ubuntu/Debian):**
```bash
sudo apt-get update
sudo apt-get install libhdf5-dev hdf5-tools
```

**Linux (Fedora/RHEL):**
```bash
sudo dnf install hdf5 hdf5-devel
```

**macOS (Homebrew):**
```bash
brew install hdf5
```

**Windows:**
- Download pre-built binaries from [HDF Group Downloads](https://www.hdfgroup.org/downloads/hdf5/)
- Set `HDF5_HOME` environment variable to installation directory
- Alternatively, add HDF5 `bin` directory to system PATH

#### Option 2: Build HDF5 from Source

Build HDF5 with Java support enabled:

```bash
# Clone HDF5 repository
git clone https://github.com/HDFGroup/hdf5.git
cd hdf5

# Build with Java (JNI)
cmake --preset ci-StdShar-GNUC --fresh
cmake --build build/ci-StdShar-GNUC
sudo cmake --install build/ci-StdShar-GNUC

# Or build with Java (FFM) - requires Java 25+
cmake --preset ci-StdShar-GNUC-FFM --fresh
cmake --build build/ci-StdShar-GNUC-FFM
sudo cmake --install build/ci-StdShar-GNUC-FFM
```

#### Option 3: Set HDF5_HOME (Recommended for Custom Installations)

If HDF5 is installed in a non-standard location, set `HDF5_HOME`:

**Linux/macOS:**
```bash
# Point to HDF5 installation directory
export HDF5_HOME=/path/to/hdf5/installation

# Then run examples (scripts automatically find libraries)
cd HDF5Examples/JAVA
./test-maven-jni.sh 2.0.1-SNAPSHOT

# Or run Maven directly
cd build/maven-test-jni
mvn exec:java -Dexec.mainClass="H5Ex_D_ReadWrite" -f pom-examples.xml
```

**Windows (PowerShell):**
```powershell
# Set HDF5_HOME environment variable
$env:HDF5_HOME = "C:\path\to\hdf5\installation"

# Run Maven examples
cd build\maven-test-jni
mvn exec:java -Dexec.mainClass="H5Ex_D_ReadWrite" -f pom-examples.xml
```

**Windows (CMD):**
```cmd
REM Set HDF5_HOME environment variable
set HDF5_HOME=C:\path\to\hdf5\installation

REM Run Maven examples
cd build\maven-test-jni
mvn exec:java -Dexec.mainClass="H5Ex_D_ReadWrite" -f pom-examples.xml
```

**Note:** The test scripts automatically add `${HDF5_HOME}/lib` (Unix) or `%HDF5_HOME%\bin` (Windows) to the library path.

#### Option 4: Specify Library Path in Java (Advanced)

**Note:** This is an advanced option. Prefer using `HDF5_HOME` (Option 3) instead.

```bash
# Run with explicit library path
java -Djava.library.path=/path/to/hdf5/lib \
     -cp "target/classes:~/.m2/repository/org/hdfgroup/hdf5-java-jni/2.0.1-SNAPSHOT/*" \
     H5Ex_D_ReadWrite
```

#### Verify Native Libraries Are Found

After installing HDF5, verify the libraries are accessible:

**Linux:**
```bash
# Check library is in system path
ldconfig -p | grep hdf5

# Or find library location
find /usr -name "libhdf5.so*" 2>/dev/null
```

**macOS:**
```bash
# Check library location
find /usr/local -name "libhdf5*.dylib" 2>/dev/null
```

**Windows:**
```cmd
# Check PATH includes HDF5 bin directory
echo %PATH%

# Verify DLL exists
where hdf5.dll
```

#### Running Examples After Library Installation

Once native libraries are installed, examples should run successfully:

**JNI Examples:**
```bash
cd build/maven-test-jni
mvn exec:java -Dexec.mainClass="H5Ex_D_ReadWrite" -f pom-examples.xml

# Expected output:
# Dataset successfully created and written
# Data read from dataset: [1, 2, 3, 4, ...]
```

**FFM Examples:**
```bash
cd build/maven-test-ffm
mvn exec:java -Dexec.mainClass="H5Ex_D_ReadWrite" -f pom-examples.xml

# Expected output:
# Dataset successfully created and written
# Data read from dataset: [1, 2, 3, 4, ...]
```

#### Why Maven Artifacts Don't Include Native Libraries

Maven artifacts contain only:
- ✅ Java bytecode (.class files)
- ✅ Java source code (in -sources.jar)
- ✅ Javadoc (in -javadoc.jar)

They do **not** include:
- ❌ Native shared libraries (.so, .dll, .dylib)
- ❌ Platform-specific binaries

**Reason:** Native libraries are platform-specific and typically hundreds of MB. Maven artifacts should be small (~2-5 MB) and platform-independent where possible. The JNI/FFM bindings provide the Java interface, but you must install the native HDF5 libraries separately.

### Pattern-Based Output Validation

Examples are validated using pattern matching for:
- **Success patterns**: `dataset|datatype|group|success|created|written|read`
- **Expected failures**: `UnsatisfiedLinkError.*hdf5_java.*java.library.path` (Maven-only testing)
- **Unexpected failures**: Other errors indicating JAR or compilation issues

### Non-Blocking Failures

- Individual example failures don't block CI
- Native library errors are treated as **expected** in Maven-only testing
- Multi-platform failures for the same example trigger alerts
- Results are uploaded as artifacts for debugging

## Development Workflow

### Adding New Examples

1. Add `.java` file to appropriate category directory
2. Update CMakeLists.txt if needed
3. Examples are automatically discovered by Maven and CI

### Testing Changes

```bash
# Test specific category
cd H5D && javac -cp "../../../maven-artifacts/*.jar" *.java

# Run example
java -cp ".:../../../maven-artifacts/*" H5Ex_D_ReadWrite
```

### Expected Output Files

Expected outputs for validation are stored in version control:
- `tfiles/min_hdf_version/H5Ex_D_ReadWrite.txt`
- Pattern-based validation for flexibility
- Platform-specific outputs handled automatically

## Deployment

Examples are deployed alongside main HDF5 Maven artifacts:

1. Built during Maven staging workflow
2. Tested in dedicated Java examples workflow
3. Deployed to GitHub Packages
4. Available for Maven Central deployment

## Usage in Projects

### Quick Start

```java
import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;

// Use examples as reference
// Source code available in JAR resources at examples/
```

### Maven Archetype (Future)

```bash
mvn archetype:generate \
  -DgroupId=com.example \
  -DartifactId=my-hdf5-project \
  -DarchetypeGroupId=org.hdfgroup \
  -DarchetypeArtifactId=hdf5-java-archetype
```

## Troubleshooting

### Common Issues

1. **Platform Mismatch**: Ensure correct classifier for your platform
2. **Native Library Path**: HDF5 native libraries loaded automatically
3. **Java Version**: Requires Java 11 or higher

### Debug Information

Examples JAR includes manifest entries:
- `HDF5-Version`: HDF5 library version
- `HDF5-Platform`: Target platform
- `Examples-Count`: Number of included examples

## Support

- GitHub Issues: https://github.com/HDFGroup/hdf5/issues
- Documentation: https://support.hdfgroup.org/documentation/
- Examples Source: Included in JAR resources
