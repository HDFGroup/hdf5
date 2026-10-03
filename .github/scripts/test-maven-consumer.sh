#!/bin/bash

# Test script to validate deployed Maven artifacts
# Usage: ./test-maven-consumer.sh [version] [repository-url]

set -e

VERSION="${1:-2.0.0-3}"
REPOSITORY_URL="${2:-https://maven.pkg.github.com/HDFGroup/hdf5}"

echo "=== Testing HDF5 Maven Artifacts ==="
echo "Version: ${VERSION}"
echo "Repository: ${REPOSITORY_URL}"
echo ""

# Create temporary test directory
TEST_DIR=$(mktemp -d)
echo "Test directory: ${TEST_DIR}"
cd "${TEST_DIR}"

# Create a simple Maven test project
cat > pom.xml << EOF
<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>

    <groupId>org.hdfgroup.test</groupId>
    <artifactId>hdf5-maven-test</artifactId>
    <version>1.0.0</version>

    <properties>
        <maven.compiler.source>11</maven.compiler.source>
        <maven.compiler.target>11</maven.compiler.target>
        <hdf5.version>${VERSION}</hdf5.version>
    </properties>

    <repositories>
        <repository>
            <id>github-hdf5</id>
            <url>${REPOSITORY_URL}</url>
        </repository>
    </repositories>

    <dependencies>
        <!-- Java bindings; hdf5-jni-native is transitive (required). libhdf5 stack is explicit opt-in. -->
        <dependency>
            <groupId>org.hdfgroup</groupId>
            <artifactId>hdf5-java-jni</artifactId>
            <version>\${hdf5.version}</version>
            <classifier>linux-x86_64</classifier>
        </dependency>
        <dependency>
            <groupId>org.hdfgroup</groupId>
            <artifactId>hdf5-native</artifactId>
            <version>\${hdf5.version}</version>
            <classifier>linux-x86_64</classifier>
        </dependency>
        <dependency>
            <groupId>org.hdfgroup</groupId>
            <artifactId>hdf5-zlib-native</artifactId>
            <version>\${hdf5.version}</version>
            <classifier>linux-x86_64</classifier>
        </dependency>
        <dependency>
            <groupId>org.hdfgroup</groupId>
            <artifactId>hdf5-szip-native</artifactId>
            <version>\${hdf5.version}</version>
            <classifier>linux-x86_64</classifier>
        </dependency>
    </dependencies>
</project>
EOF

# Create a simple test class
mkdir -p src/main/java/org/hdfgroup/test
cat > src/main/java/org/hdfgroup/test/TestConsumer.java << 'EOF'
package org.hdfgroup.test;

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;

public class TestConsumer {
    public static void main(String[] args) throws Exception {
        System.out.println("Testing HDF5 Maven artifact consumption...");

        try {
            // Try to load HDF5 Java classes
            Class.forName("hdf.hdf5lib.H5");
            System.out.println("✓ HDF5 Java library classes found");
        } catch (ClassNotFoundException e) {
            System.out.println("⚠ HDF5 Java library classes not found: " + e.getMessage());
        }

        H5.loadH5Lib();
        if (H5.H5Zfilter_avail(HDF5Constants.H5Z_FILTER_DEFLATE) <= 0) {
            throw new IllegalStateException("H5Z_FILTER_DEFLATE is not available");
        }
        System.out.println("✓ H5Z_FILTER_DEFLATE is available after H5.loadH5Lib()");

        long dcpl = H5.H5Pcreate(HDF5Constants.H5P_DATASET_CREATE);
        long[] chunk = {10, 20};
        H5.H5Pset_chunk(dcpl, 2, chunk);
        H5.H5Pset_deflate(dcpl, 6);
        H5.H5Pclose(dcpl);
        System.out.println("✓ HDF5 deflate/GZIP filter is available");

        long szDcpl = H5.H5Pcreate(HDF5Constants.H5P_DATASET_CREATE);
        H5.H5Pset_chunk(szDcpl, 2, chunk);
        H5.H5Pset_szip(szDcpl, HDF5Constants.H5_SZIP_NN_OPTION_MASK, 8);
        H5.H5Pclose(szDcpl);
        System.out.println("✓ HDF5 SZIP (libaec) filter is available");

        System.out.println("✓ Maven artifact consumption test completed");
    }
}
EOF

echo "=== Testing Maven Dependency Resolution ==="

# Test dependency resolution
if mvn dependency:resolve -q; then
    echo "✓ Maven dependencies resolved successfully"
else
    echo "❌ Maven dependency resolution failed"
    exit 1
fi

# Test compilation
echo "=== Testing Compilation ==="
if mvn compile -q; then
    echo "✓ Compilation successful"
else
    echo "❌ Compilation failed"
    exit 1
fi

# Test runtime loading and deflate availability
echo "=== Testing Runtime Loading and Deflate ==="
if mvn exec:java -Dexec.mainClass=org.hdfgroup.test.TestConsumer -Dexec.jvmArgs=--enable-native-access=ALL-UNNAMED -q; then
    echo "✓ Runtime loading and deflate test successful"
else
    echo "❌ Runtime loading or deflate test failed"
    exit 1
fi

# List resolved dependencies
echo "=== Resolved Dependencies ==="
mvn dependency:list | grep org.hdfgroup || echo "No org.hdfgroup dependencies found"

# Show artifact details
echo "=== Artifact Details ==="
find ~/.m2/repository/org/hdfgroup -name "*.jar" 2>/dev/null | head -10 | while read jar; do
    echo "Found: $(basename "$jar") ($(du -h "$jar" | cut -f1))"
done

echo ""
echo "=== Test Summary ==="
echo "✓ Maven artifact consumption test completed successfully"
echo "✓ HDF5 Java artifacts are accessible via Maven"
echo "✓ Dependencies resolve and compile correctly"
echo ""
echo "Cleanup: rm -rf ${TEST_DIR}"

# Cleanup
cd /
rm -rf "${TEST_DIR}"