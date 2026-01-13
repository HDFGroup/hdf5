# HDF5 Java Examples & Maven Integration

This directory contains Java examples demonstrating the usage of HDF5 Java bindings (JNI and FFM), organized into categories and deployable as a Maven artifact.

## Directory Structure

```text
HDF5Examples/JAVA/
├── H5D/          # Dataset operations examples
├── H5T/          # Datatype operations examples
├── H5G/          # Group operations examples
├── TUTR/         # Tutorial examples
├── pom-examples.xml.in    # Maven POM template
└── README.md              # This file
```

## 1. Getting Started

To run these examples, you need two things:
1.  **The Java Dependencies** (Managed by Maven)
2.  **The Native HDF5 Libraries** (Installed on your OS)

### Step 1: Maven Dependency Configuration
Add the examples and the specific platform binding to your `pom.xml`.

**Note:** Replace `X.Y.Z` with your target HDF5 version (e.g., `1.14.0`).

```xml
<dependencies>
    <!-- Core Examples -->
    <dependency>
        <groupId>org.hdfgroup</groupId>
        <artifactId>hdf5-java-examples</artifactId>
        <version>X.Y.Z</version>
    </dependency>

    <!-- Platform-Specific Native Bindings (Select ONE) -->
    
    <!-- Linux -->
    <dependency>
        <groupId>org.hdfgroup</groupId>
        <artifactId>hdf5-java</artifactId>
        <version>X.Y.Z</version>
        <classifier>linux-x86_64</classifier>
    </dependency>

    <!-- Windows -->
    <dependency>
        <groupId>org.hdfgroup</groupId>
        <artifactId>hdf5-java</artifactId>
        <version>X.Y.Z</version>
        <classifier>windows-x86_64</classifier>
    </dependency>

    <!-- macOS (Intel) -->
    <dependency>
        <groupId>org.hdfgroup</groupId>
        <artifactId>hdf5-java</artifactId>
        <version>X.Y.Z</version>
        <classifier>macos-x86_64</classifier>
    </dependency>
</dependencies>
```

### Step 2: Native Library Configuration (Crucial)
Maven provides the Java JARs, but **it does not install the system-level HDF5 libraries** required for execution. You must have the HDF5 binaries installed or built on your system.

**If you see `UnsatisfiedLinkError: no hdf5_java`, the application cannot find your native HDF5 installation.**

#### Method A: Using Environment Variables (Recommended)
If you have built HDF5 from source or installed it to a custom location, point to it:

*   **Linux/macOS:** `export LD_LIBRARY_PATH=/path/to/hdf5/lib:$LD_LIBRARY_PATH`
*   **Windows:** Add `C:\path\to\hdf5\bin` to your system `PATH`.

#### Method B: System Installation
If you installed via package manager (e.g., `apt install libhdf5-dev` or `brew install hdf5`), standard paths generally work automatically.

---

## 2. Building and Running Examples

### Compile All Examples
```bash
cd HDF5Examples/JAVA
mvn compile -f pom-examples.xml
```

### Run a Specific Example
To run an example (e.g., `H5Ex_D_ReadWrite`), use the `exec:java` goal. Ensure your native library path is set (see Step 2 above).

**JNI (Standard):**
```bash
mvn exec:java -Dexec.mainClass="H5Ex_D_ReadWrite" -f pom-examples.xml
```

**FFM (Foreign Function & Memory - Requires Java 25+):**
```bash
# Note: Ensure you are using a JDK supporting FFM
mvn exec:java -Dexec.mainClass="H5Ex_D_ReadWrite" -f pom-examples.xml
```

### Create a Standalone JAR
```bash
mvn package -f pom-examples.xml
```
This generates `target/hdf5-java-examples-{version}.jar` containing all compiled examples.

---

## 3. Example Categories

| Directory | Description | Key Concepts |
| :--- | :--- | :--- |
| **H5D** | **Dataset Operations** | Read/write, chunking, filters (gzip, nbit), fill values, allocation. |
| **H5T** | **Datatype Operations** | Compound types, arrays, enums, opaque types, variable-length strings. |
| **H5G** | **Group Operations** | Creating groups, iteration, hierarchy traversal, links. |
| **TUTR** | **Tutorials** | Step-by-step introductory examples. |

---

## 4. Repo Verification & Testing Scripts
*(For Maintainers and CI/CD)*

We provide scripts to verify the integrity of Maven artifacts against these examples. These scripts simulate a clean environment to ensure the JARs are structured correctly.

### Script: `test-maven-jni.sh` (Standard)
Tests the JNI implementation (Requires Java 11+).

**Usage:**
```bash
./test-maven-jni.sh [VERSION] [REPO_URL] [BUILD_DIR]
```

**Workflow:**
1.  Downloads the `hdf5-java-jni` artifact.
2.  Compiles 55+ examples.
3.  **Note:** During this *artifact structure test*, an `UnsatisfiedLinkError` regarding native libraries is **expected behavior** if the runner lacks a local HDF5 installation. The test verifies that the Java classes load, not that the local machine has the binaries.

### Script: `test-maven-ffm.sh` (Experimental)
Tests the Foreign Function & Memory implementation (Requires Java 25+).

**Usage:**
```bash
./test-maven-ffm.sh [VERSION] [REPO_URL] [BUILD_DIR]
```

**Notes:**
*   Excludes callback-based examples (`H5Ex_G_Visit`, etc.) as FFM callback handling differs from JNI.
*   Requires valid GitHub authentication if pulling from GitHub Packages.

### GitHub Authentication
For testing artifacts hosted on GitHub Packages:
```bash
# Recommended: GitHub CLI
gh auth login
gh auth refresh --scopes read:packages
```

---

## 5. Troubleshooting

**Q: `UnsatisfiedLinkError: no hdf5_java in java.library.path`**
*   **Cause:** Java found the classes, but not the native C library.
*   **Fix:** Ensure HDF5 is installed and `LD_LIBRARY_PATH` (Linux), `DYLD_LIBRARY_PATH` (macOS), or `PATH` (Windows) includes the folder containing `libhdf5.so`, `libhdf5.dylib`, or `hdf5.dll`.

**Q: `package org.hdfgroup.hdf5 does not exist`**
*   **Cause:** Maven dependencies are not resolving.
*   **Fix:** Run `mvn dependency:resolve` and check your `pom.xml` version matches the available release.

**Q: "Java version too old"**
*   **Cause:** FFM examples require cutting-edge Java versions.
*   **Fix:** Use JNI for standard production environments (Java 11+). Only use FFM if you are targeting Java 25+.

## Support
*   **Issues:** [GitHub Issue Tracker](https://github.com/HDFGroup/hdf5/issues)
*   **Documentation:** [HDF Support Portal](https://support.hdfgroup.org/documentation/)
