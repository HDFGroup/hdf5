# The `release_docs` directory

## Intro

This directory contains instructions for building and using the library as
well as the HDF5 history files.

## HISTORY files

The `HISTORY` files contain the history of this branch of HDF5. They fall into
three categories.

### HISTORY-\[VERSION 1\]-\[VERSION 2\].txt

These files are created when we release a new major version and include all
the changes that were made to the `develop` branch while creating a major release.

### HISTORY-\[VERSION\].txt

This file contains the changes that were made to a maintenance branch since
it split off from `develop`. It will also be found in the `develop` branch
when experimental releases have been created.

### CHANGELOG.md (formerly RELEASE.txt)

This is the changelog for the current version of the library.

For a MAJOR release (or in `develop`) this files lists all the changes since the
last major version. For a MINOR release (or in a maintenance branch), this file
lists all the changes since the last release in the maintenance branch.

Examples:

* The file for HDF5 1.14.0 includes all the changes since HDF5 1.12.0
* The file for HDF5 1.10.9 includes all the changes since HDF5 1.10.8
* The file in `develop` includes all the changes since the last major release
* The file in `hdf5_1_14` includes all the changes since the last minor HDF5 1.14 release

Note that we make no effort to bring maintenance branch `HISTORY` files back to
develop. If you want to compare, say, 1.10.4 with 1.12.3, you'd have to get
the history files from those releases and compare them by hand.

## Creating new releases

### MAJOR release

* If there were experimental releases, merge the experimental `HISTORY` file
  and the current `CHANGELOG.md` by category to create a separate, unified
  file that ignores the experimental releases. Don't check this in yet or
  clobber any existing `HISTORY`/`RELEASE` files, but put it someplace handy for
  use in later steps.

* Create the new maintenance branch

In develop:
* Create the new `HISTORY-\[VERSION 1\]-\[VERSION 2\].txt` file
    * If there is an experimental `HISTORY` file, add `CHANGELOG.md` to the beginning of it and use that
    * Otherwise, start with `CHANGELOG.md`
    * Add the introduction boilerplate like in the other `HISTORY` files (TOC, etc.)
* Delete any experimental `HISTORY` file
* Clear out `CHANGELOG.md`

Note that we're KEEPING any experimental release history information in the
`HISTORY-\[VERSION 1\]-\[VERSION 2\].txt` file, so do NOT use the merged file in
the above steps!

In the new maintenance branch:
* Create the new `HISTORY-\[VERSION\].txt` file
    * If there is an experimental `HISTORY` file use the combined file you created earlier
    * Otherwise, start with `CHANGELOG.md`
    * Add the introduction boilerplate like in the other `HISTORY` files (TOC, etc.)
* Delete any experimental `HISTORY` file
* Clear out `CHANGELOG.md`

* Create the new release branch

In the new release branch:
* If there were experimental releases, use the combined file you created earlier as `CHANGELOG.md`
* Otherwise the `CHANGELOG.md` will be used as-is

### MINOR release

* Create the release branch

In the maintenance branch:
* Add the contents of `CHANGELOG.md` to the beginnnig of `HISTORY-\[VERSION\].txt`
* Clear out `CHANGELOG.md`

### EXPERIMENTAL release

* Add the contents of `CHANGELOG.md` to the beginnnig of `HISTORY-\[VERSION\].txt`
* Clear out `CHANGELOG.md`

## INSTALL files

These files include instructions for building and installing HDF5 on various
platforms.

## USING files

These files document how to build HDF5 applications with an installed HDF5
library.

## Java Examples Maven Integration

The HDF5 Java examples are now integrated with Maven deployment to provide
complete example applications that work with HDF5 Java library Maven artifacts:

### Maven Artifacts

HDF5 provides two Java implementation artifacts:

* **`org.hdfgroup:hdf5-java-ffm`** - FFM (Foreign Function & Memory) implementation
  - Default for Java 25+, modern native access via Project Panama
  - Platform-specific JARs: linux-x86_64, windows-x86_64, macos-x86_64, macos-aarch64

* **`org.hdfgroup:hdf5-java-jni`** - JNI (Java Native Interface) implementation
  - Available for all Java versions (11+)
  - Platform-specific JARs: linux-x86_64, windows-x86_64, macos-x86_64, macos-aarch64

* **`org.hdfgroup:hdf5-java-examples`** - Complete collection of 62 Java examples
  - Platform-independent JAR with educational examples
  - Covers all major HDF5 functionality: datasets, datatypes, groups, tutorials
  - Compatible with both FFM and JNI implementations

Both implementations use the same `hdf.hdf5lib.*` package structure for seamless migration.

### FFM Test Suite

**Running FFM Tests:**
```bash
ctest -R "JUnitFFM" -V           # All FFM tests
ctest -R "JUnit-TestH5Affm" -V   # Attribute tests
ctest -R "JUnit-TestH5Pffm" -V   # Property tests
ctest -R "JUnit-TestH5Tffm" -V   # Datatype tests
```

**Test Location:** `java/jtest/TestH5*ffm.java`

### Features
* **Dual Implementation Support:** Choose FFM for modern Java or JNI for compatibility
* **Cross-Platform CI/CD:** Automated testing across all supported platforms
* **Fork-Based Testing:** Complete validation framework for testing on repository forks
* **Dynamic Repository Support:** Workflows adapt automatically to any GitHub repository
* **Maven Integration:** Seamless integration with standard Maven dependency management
* **Documentation:** See `HDF5Examples/JAVA/README-MAVEN.md` for complete usage instructions

### GitHub Packages Deployment

Using FFM implementation (Java 25+):
```xml
<dependency>
    <groupId>org.hdfgroup</groupId>
    <artifactId>hdf5-java-ffm</artifactId>
    <version>2.0.0-3</version>
    <classifier>linux-x86_64</classifier>
</dependency>
```

Using JNI implementation (Java 11+):
```xml
<dependency>
    <groupId>org.hdfgroup</groupId>
    <artifactId>hdf5-java-jni</artifactId>
    <version>2.0.0-3</version>
    <classifier>linux-x86_64</classifier>
</dependency>
```

The Java examples demonstrate proper usage of HDF5 Java bindings in real-world
scenarios and serve as templates for developing HDF5-based Java applications.
