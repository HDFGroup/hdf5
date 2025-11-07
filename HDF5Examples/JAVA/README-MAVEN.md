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
