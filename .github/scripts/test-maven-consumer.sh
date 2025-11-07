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
        <!-- HDF5 Java Library (platform-specific) -->
        <dependency>
            <groupId>org.hdfgroup</groupId>
            <artifactId>hdf5-java</artifactId>
            <version>\${hdf5.version}</version>
            <classifier>linux-x86_64</classifier>
        </dependency>

        <!-- HDF5 Java Examples -->
        <dependency>
            <groupId>org.hdfgroup</groupId>
            <artifactId>hdf5-java-examples</artifactId>
            <version>\${hdf5.version}</version>
        </dependency>
    </dependencies>
</project>
EOF

# Create a simple test class
mkdir -p src/main/java/org/hdfgroup/test
cat > src/main/java/org/hdfgroup/test/TestConsumer.java << 'EOF'
package org.hdfgroup.test;

public class TestConsumer {
    public static void main(String[] args) {
        System.out.println("Testing HDF5 Maven artifact consumption...");

        try {
            // Try to load HDF5 Java classes
            Class.forName("hdf.hdf5lib.H5");
            System.out.println("✓ HDF5 Java library classes found");
        } catch (ClassNotFoundException e) {
            System.out.println("⚠ HDF5 Java library classes not found: " + e.getMessage());
        }

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