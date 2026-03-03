# Build and Install HDF5 C, C++, Fortran and High-Level Libraries and tools with CMake

---

## Table of Contents

* [Section I: Preconditions](#section-i)
* [Section II: RECOMMENDED: Quick Start with CMake Presets](#section-ii)
* [Section III: Advanced: Building HDF5 Libraries with CMake Script Mode](#section-iii)
* [Section IV: Advanced: Building HDF5 Libraries with CMake Command Mode](#section-iv)
* [Section V: Further Considerations](#section-v)
* [Section VI: Options for building HDF5 Libraries with CMake Command Line](#section-vi)
* [Section VII: CMake Option Defaults for HDF5](#section-vii)
* [Section VIII: User Defined Options for HDF5 Libraries with CMake](#section-viii)
* [Section IX: User Defined Compile Flags for HDF5 Libraries with CMake](#section-ix)
* [Section X: Considerations for Cross-Compiling](#section-x)
* [Section XI: Creating Custom Preset Configurations](#section-xi)
* [Section XII: Using the Library](#section-xii)
* [Section XIII: Using CMake Regex Options for Testing](#section-xiii)
* [Section XIV: Java FFM Testing](#section-xiv)

---

<a id="section-i"></a>
## I. Preconditions

### Obtaining HDF5 source code
1. Create a directory for your development; for example, `myhdfstuff`.
2. Obtain HDF5 source from Github:
   * **development branch:** [https://github.com/HDFGroup/hdf5](https://github.com/HDFGroup/hdf5)
   * **last release:** [https://github.com/HDFGroup/hdf5/releases/latest](https://github.com/HDFGroup/hdf5/releases/latest)
     (`hdf5-2_"X"_"Y".tar.gz` or `hdf5-2_"X"_"Y".zip`)
   * Put it in `myhdfstuff` and uncompress the file. There should be an `hdf5-2."X"."Y"` folder.
3. Obtain HDF5 plugin source from Github:
   * **development branch:** [https://github.com/HDFGroup/hdf5_plugins](https://github.com/HDFGroup/hdf5_plugins)
   * **OR** let the CMake build process download the plugins for you with the following options:
     * `HDF5_ALLOW_EXTERNAL_SUPPORT:STRING="GIT"` (or `"TGZ"`)
     * `HDF5_ENABLE_PLUGIN_SUPPORT:BOOL=ON`

### CMake version
1. We suggest you obtain the latest CMake from the Kitware web site. The HDF5 2."X"."Y" product requires a **minimum CMake version 3.26**.

> **Note:** To change the install prefix from the platform defaults initialize the CMake variable, `CMAKE_INSTALL_PREFIX`. Users of build scripts will use the `INSTALLDIR` option.
>
> **Note:** See the CMake documentation for more information on setting the logging levels: [CMAKE_MESSAGE_LOG_LEVEL](https://cmake.org/cmake/help/latest/variable/CMAKE_MESSAGE_LOG_LEVEL.html)

---

<a id="section-ii"></a>
## II. RECOMMENDED: Quick Start with CMake Presets

⭐ This is the **RECOMMENDED** method for building HDF5 2.0 and later.

### Prerequisites
* **CMake 3.26 or later** (required for HDF5 2.0)
* **Ninja build system** (recommended, should be downloaded if not available)
* **Compiler:** GCC, MSVC, or Clang

### Quick Start (3 steps)
1. Change to the HDF5 source directory:
   ```bash
   cd /path/to/hdf5-2.x.y
   ```
2. Execute a workflow preset:
   ```bash
   cmake --workflow --preset ci-StdShar-GNUC --fresh       # Linux/Mac with GCC
   cmake --workflow --preset ci-StdShar-MSVC --fresh       # Windows with MSVC
   cmake --workflow --preset ci-StdShar-Clang --fresh      # Linux/Mac with Clang
   ```
3. Find your build artifacts in:
   ```text
   ../build/ci-StdShar-<compiler>/
   ```

That's it! The workflow preset automatically:
* Configures the build
* Compiles libraries and tools
* Runs tests
* Creates installation packages

### Available Presets
View all available presets:
```bash
cmake --list-presets
```

**Common presets:**
* **Standard Builds:**
  * `ci-StdShar-GNUC`        (Standard shared libraries - GCC)
  * `ci-StdShar-MSVC`        (Standard shared libraries - MSVC)
  * `ci-StdShar-Clang`       (Standard shared libraries - Clang)
  * `ci-MinShar-GNUC`        (Minimal shared libraries - GCC)
* **Java Builds:**
  * `ci-StdShar-GNUC-Java-FFM`     (Java FFM bindings - GCC)
  * `ci-StdShar-GNUC-Java-JNI`     (Java JNI bindings - GCC)
* **Maven Deployment (JNI default - Java 8+):**
  * `ci-MinShar-GNUC-Maven-Snapshot`               (JNI snapshots for Maven)
  * `ci-MinShar-GNUC-Maven`                        (JNI release for Maven)
* **Maven Deployment (FFM optional - Java 25+):**
  * `ci-MinShar-GNUC-Maven-FFM-Snapshot`           (FFM snapshots for Maven)
  * `ci-MinShar-GNUC-Maven-FFM`                    (FFM release for Maven)

> **Note:** For ROS3 (S3 support), add `-DHDF5_ENABLE_ROS3_VFD=ON` to any preset. See Section XI for creating custom preset configurations.

### Why Use Presets?
* ✅ **Simpler** - No external files to download
* ✅ **Faster** - Optimized settings included
* ✅ **Consistent** - Same settings across platforms
* ✅ **Modern** - CMake 3.26 best practices
* ✅ **Flexible** - Easy to customize via `CMakeUserPresets.json`

### Individual Preset Commands (Advanced)
If you prefer to run preset steps individually (where `<compiler-type>` is `GNUC`, `MSVC`, or `Clang`):

```bash
cd /path/to/hdf5-source
cmake --preset ci-StdShar-<compiler-type>                 # Configure
cmake --build --preset ci-StdShar-<compiler-type>         # Build
ctest --preset ci-StdShar-<compiler-type>                 # Test
cpack --preset ci-StdShar-<compiler-type>                 # Package
```

### Advanced Build Methods
If you cannot use presets or need more control, see:
* **Section III:** Building with CMake Script Mode (requires external files, for automation/CI)
* **Section IV:** Building with CMake Command Mode (manual configuration, for advanced users)

---

<a id="section-iii"></a>
## III. Advanced: Building HDF5 Libraries with CMake Script Mode

> **NOTE:** Most users should use Section II (CMake Presets) instead. This method is provided for advanced users and automated builds.

This short set of instructions is written for users who want to quickly build the HDF5 C, C++ and Fortran shared libraries and tools from the HDF5 source code package using the CMake tools. This procedure will use the default settings in the `config/cmake/cacheinit.cmake` file.

### Individual files needed as mentioned in this document
Download from [GitHub Scripts](https://github.com/HDFGroup/hdf5/blob/develop/config/cmake/scripts):
* `CTestScript.cmake`  -- CMake build script
* `HDF5config.cmake`   -- CMake configuration script
* `HDF5options.cmake`  -- CMake configuration options script

External libraries:
* **Plugins:** `hdf5_plugins.tar.gz` from [GitHub Plugins](https://github.com/HDFGroup/hdf5_plugins)
* **ZLIB:** [zlib-1.3.1.tar.gz](https://github.com/madler/zlib/releases/download/v1.3.1/zlib-1.3.1.tar.gz)
* **ZLIBNG:** [2.2.4.tar.gz](https://github.com/zlib-ng/zlib-ng/archive/refs/tags/2.2.4.tar.gz)
* **LIBAEC:** [libaec-1.1.3.tar.gz](https://github.com/MathisRosenhauer/libaec/releases/download/v1.1.3/libaec-1.1.3.tar.gz)

### Build scripts for Windows or Linux

1. Change to the development directory `myhdfstuff`.
2. Download/copy the individual files mentioned above to `myhdfstuff`. Do not uncompress the `tar.gz` files.
3. Change to the source directory `hdf5-2.x.y`. `CTestScript.cmake` file should not be modified.
4. Edit the platform configuration file, `HDF5options.cmake`, if you want to change the default build environment.
5. From the `myhdfstuff` directory execute the CTest Script with the following options:

   * **32-bit Windows with Visual Studio 2022:**
     `ctest -S HDF5config.cmake,BUILD_GENERATOR=VS2022 -C Release -VV -O hdf5.log`
   * **64-bit Windows with Visual Studio 2022:**
     `ctest -S HDF5config.cmake,BUILD_GENERATOR=VS202264 -C Release -VV -O hdf5.log`
   * **Linux and Mac:**
     `ctest -S HDF5config.cmake,BUILD_GENERATOR=Unix -C Release -VV -O hdf5.log`

   *(Similar commands apply for VS2019 and VS2017).*

   This will configure, build, test, and create an install package (`HDF5-2.X.Y-<platform>.<zip or tar.gz>`).

6. **To install:**
   * **On Windows (with WiX):** execute `HDF5-2."X"."Y"-win32.msi` or `HDF5-2."X"."Y"-win64.msi`. Installs to `C:\Program Files\HDF_Group\HDF5\2."X"."Y"`.
   * **On Linux:** execute `<path-to>/myhdfstuff/HDF5-2."X"."Y"-Linux.sh`.
   * **On Mac:** Click on `HDF5-2."X"."Y"-Darwin.dmg`.

---

<a id="section-iv"></a>
## IV. Advanced: Building HDF5 Libraries with CMake Command Mode

> **NOTE:** Most users should use Section II (CMake Presets) instead.

1. Change to the development directory `myhdfstuff`.
2. Uncompress the HDF5 source file.
3. Create a folder `build` in the `myhdfstuff` directory.
4. Change into the `build` folder.
5. Configure the C library, tools and tests:

   * **On Windows 32 bit:**
     ```cmd
     cmake -G "Visual Studio 16 2019" -A Win32 -DCMAKE_BUILD_TYPE:STRING=Release -DBUILD_SHARED_LIBS:BOOL=OFF -DBUILD_TESTING:BOOL=ON -DHDF5_BUILD_TOOLS:BOOL=ON ..\hdf5-2."X"."Y"
     ```
   * **On Windows 64 bit:**
     ```cmd
     cmake -G "Visual Studio 16 2019" -A x64 -DCMAKE_BUILD_TYPE:STRING=Release -DBUILD_SHARED_LIBS:BOOL=OFF -DBUILD_TESTING:BOOL=ON -DHDF5_BUILD_TOOLS:BOOL=ON ..\hdf5-2."X"."Y"
     ```
   * **On Linux and Mac:**
     ```bash
     cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE:STRING=Release -DBUILD_SHARED_LIBS:BOOL=OFF -DBUILD_TESTING:BOOL=ON -DHDF5_BUILD_TOOLS:BOOL=ON ../hdf5-2."X"."Y"
     ```

6. Build with: `cmake --build . --config Release`
7. Test with: `ctest . -C Release`
8. Create an install image: `cpack -C Release CPackConfig.cmake`
9. Install using the generated installer or script (`.msi`, `.sh`, or `.dmg`).

---

<a id="section-v"></a>
## V. Further Considerations

1. **CMake Version:** The HDF5 2."X"."Y" product requires a minimum CMake version 3.26.
2. **External Libraries (Zlib/Szip):** You can install binaries centrally and point CMake to them (`-DZLIB_LIBRARY`, `-DZLIB_INCLUDE_DIR`, etc.), or use `HDF5_ALLOW_EXTERNAL_SUPPORT` set to `"GIT"` or `"TGZ"` to fetch them automatically.
3. **Compression Plugins:** Similarly configurable via `"GIT"` or `"TGZ"`.
4. **Apple Darwin:** Build static (`BUILD_SHARED_LIBS:BOOL=OFF`), add `CMAKE_ANSI_CFLAGS:STRING=-fPIC`.
5. **Windows Developers:** Install NSIS or WiX to create install images.
6. **CDash Submissions:** Build/test results can be submitted to [https://my.cdash.org](https://my.cdash.org).

---

<a id="section-vi"></a>
## VI. Options for building HDF5 Libraries with CMake Command Line

To build the HDF5 Libraries with CMake, go through these five steps:

### 1. Run CMake
Specify the source and build directories. **Make the build and source directories different.**
Use `cmake-gui` or the command line.
```bash
cmake -C <sourcepath>/config/cmake/cacheinit.cmake -G "<generator>" [-D<options>] <sourcepath>
```

### 2. Configure the cache settings
Visual CMake users, click the Configure button. Resolve any red highlighted values.
Command line example:
```bash
cmake -C ../config/cmake/cacheinit.cmake -G "Visual Studio 16 2019" "-Ax64" \
      -DHDF5_ENABLE_SZIP_SUPPORT:BOOL=OFF -DHDF5_ENABLE_ZLIB_SUPPORT:BOOL=OFF \
      -DCMAKE_BUILD_TYPE:STRING=Release ..
```

### 3. Build HDF5
```bash
cmake --build . --config {Debug | Release}
```

### 4. Test HDF5
```bash
ctest . -C {Debug | Release}
```

### 5. Packaging HDF5
```bash
cpack -C {Debug | Release} CPackConfig.cmake
```

> **API Compatibility:** The 2.0.0 version of the HDF5 library can be configured to set all versioned functions to a specific API version (`HDF5_DEFAULT_API_VERSION:STRING=v114`, `v112`, etc.).
>
> **Parallel / Threadsafe:** > * Parallel requires MPI (`HDF5_ENABLE_PARALLEL:BOOL=ON`). Incompatible with Threadsafe, C++, Java.
> * Threadsafe (`HDF5_ENABLE_THREADSAFE:BOOL=ON`) is incompatible with High-level, C++, Fortran, Java.

---

<a id="section-vii"></a>
## VII. CMake Option Defaults for HDF5

The `config/cmake/cacheinit.cmake` or `CMakePresets.json` file can override the following values.

### General & HDF5 Build Options

| Option Name | Description | Default |
| :--- | :--- | :--- |
| `BUILD_SHARED_LIBS` | Build Shared Libraries | `ON` |
| `BUILD_STATIC_LIBS` | Build Static Libraries | `ON` |
| `BUILD_STATIC_EXECS` | Build Static Executables | `OFF` |
| `BUILD_TESTING` | Build HDF5 Unit Testing | `ON` |
| `HDF5_DISABLE_PDB_FILES` | Do not install PDB files (Windows only) | `OFF` |
| `HDF5_BUILD_CPP_LIB` | Build HDF5 C++ Library | `OFF` |
| `HDF5_BUILD_EXAMPLES` | Build HDF5 Library Examples | `ON` |
| `HDF5_BUILD_FORTRAN` | Build FORTRAN support | `OFF` |
| `HDF5_BUILD_JAVA` | Build JAVA support | `OFF` |
| `HDF5_BUILD_HL_LIB` | Build HIGH Level HDF5 Library | `ON` |
| `HDF5_BUILD_TOOLS` | Build HDF5 Tools | `ON` |
| `HDF5_BUILD_PARALLEL_TOOLS` | Build Parallel HDF5 Tools | `OFF` |
| `HDF5_BUILD_STATIC_TOOLS` | Build Static Tools Not Shared Tools | `OFF` |

### HDF5 Maven Integration Options
* `HDF5_ENABLE_MAVEN_DEPLOY`: Enable Maven repository deployment (`OFF`)
* `HDF5_MAVEN_SNAPSHOT`: Build Maven snapshot versions (`OFF`)
* `HDF5_ENABLE_JNI`: Force JNI implementation (`ON`)

*(Java Implementation Selection: JNI is default for Java 8+. FFM is optional, requires Java 25+)*

### HDF5 Folder Build Options
*Defaults relative to `$<INSTALL_PREFIX>`*
* `HDF5_INSTALL_BIN_DIR`: `bin`
* `HDF5_INSTALL_LIB_DIR`: `lib`
* `HDF5_INSTALL_INCLUDE_DIR`: `include`

*If `HDF5_USE_GNU_DIRS` is `ON` (uses GNU Coding Standard):*
* `HDF5_INSTALL_MODULE_DIR`: `HDF5_INSTALL_INCLUDE_DIR/mod`
* `HDF5_INSTALL_CMAKE_DIR`: `HDF5_INSTALL_LIB_DIR/cmake`

### Advanced Options (Highlights)
| Option Name | Default |
| :--- | :--- |
| `HDF5_ONLY_SHARED_LIBS` | `OFF` |
| `HDF5_ENABLE_PARALLEL` | `OFF` |
| `HDF5_ENABLE_THREADSAFE` | `OFF` |
| `HDF5_ENABLE_COVERAGE` | `OFF` |
| `HDF5_DEFAULT_API_VERSION`| `"v200"` |

---

<a id="section-viii"></a>
## VIII. User Defined Options for HDF5 Libraries with CMake

Support for User Defined macros and options has been added. The file `UserMacros.cmake` has an example of the technique. Replace the template code with your macro in the `UserMacros.cmake` file.

---

<a id="section-ix"></a>
## IX. User Defined Compile Flags for HDF5 Libraries with CMake

Custom compiler flags can be added by defining the `CMAKE_C_FLAGS` and `CMAKE_CXX_FLAGS` variables.

Using a cmake script:
```cmake
set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -O2")
```
Defined on the configure line:
```bash
cmake -G "Visual Studio 17 2019" -DCMAKE_C_FLAGS:STRING=-O2 ..
```

---

<a id="section-x"></a>
## X. Considerations for Cross-Compiling

Cross-compiling requires CMake toolchain files since it cannot automatically detect the target platform. Put the toolchain variables into a separate file (e.g. `<toolchain_name>.cmake`) and set the `CMAKE_TOOLCHAIN_FILE` variable.

```bash
cmake --toolchain path/to/file
# or
cmake -DCMAKE_TOOLCHAIN_FILE=path/to/file
```

Set variables like `CMAKE_C_COMPILER`, `CMAKE_CXX_COMPILER`, `CMAKE_SYSTEM_NAME`, and `CMAKE_FIND_ROOT_PATH` inside your toolchain file.

---

<a id="section-xi"></a>
## XI. Creating Custom Preset Configurations

The quickest way to customize your build is to create a `CMakeUserPresets.json` file in the HDF5 source directory.

**Basic Customization Steps:**
1. Copy `CMakePresets.json` to `CMakeUserPresets.json`.
2. Edit `CMakeUserPresets.json`: Change configuration names from `ci-*` to `my-*` and modify the fields as needed.

**Example Preset (Maven Deployment):**
```json
{
  "name": "my-maven-custom",
  "inherits": "ci-MinShar-GNUC-Maven-Snapshot",
  "cacheVariables": {
    "MAVEN_REPOSITORY_URL": {"type": "STRING", "value": "[https://your-repo.com/maven](https://your-repo.com/maven)"},
    "HDF5_ENABLE_ROS3_VFD": {"type": "BOOL", "value": "ON"}
  }
}
```

Build with:
```bash
cmake --workflow --preset my-maven-custom --fresh
```

---

<a id="section-xii"></a>
## XII. Using the Library

The CMake installation provides a configuration file (`cmake/hdf5-config.cmake`) which can be used to determine features using the `find_package` command.

1. Set the `HDF5_ROOT` CMake variable or environment variable.
2. Add the following to your `CMakeLists.txt` file:
   ```cmake
   find_package (HDF5 NAMES hdf5 COMPONENTS C shared)
   ```
   *(Components can include: shared, static, C, CXX, Fortran, HL, Java, Tools, and VOL.)*

---

<a id="section-xiii"></a>
## XIII. Using CMake Regex Options for Testing

Use regular expressions to control which tests are executed via labels defined in `CMakeTests.cmake`.

* **Run specific label (e.g., C++ tests):**
  ```bash
  ctest . --tests-regex "CPP"
  ```
* **Run multiple labels:**
  ```bash
  ctest . --tests-regex "MPI_TEST|FORTRAN"
  ```
* **Exclude tests:**
  Use the `--exclude-regex` (or `-E`) option.

---

<a id="section-xiv"></a>
## XIV. Java FFM Testing

HDF5 2.0 includes comprehensive Foreign Function & Memory (FFM) API tests for Java 25+.

**Running FFM Tests:**
* Run all FFM tests: `ctest -R "JUnitFFM" -V`
* Run specific module tests:
  ```bash
  ctest -R "JUnit-TestH5Affm" -V    # Attributes
  ctest -R "JUnit-TestH5Pffm" -V    # Properties
  ```

**Test Requirements:**
* Java 25+ with `--enable-native-access=ALL-UNNAMED`
* FFM bindings JAR (`javahdf5-*.jar`)
* JUnit 4.x test framework
* HDF5 native libraries in `LD_LIBRARY_PATH`

---
*For further assistance, send email to help@hdfgroup.org*
