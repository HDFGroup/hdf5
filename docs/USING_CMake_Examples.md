# Build and Test HDF5 Examples with CMake

> **Notes:** This short instruction is written for users who want to quickly test the installation of HDF5 by using the CMake tools to build and test the HDF5 Examples. The following instructions will show the default usage and then present common changes for non-default installations.
>
> For more information, see the [USING_HDF5_CMake.md](./USING_HDF5_CMake.md) file. More information about using CMake can be found at the Kitware site, [www.cmake.org](https://www.cmake.org).

---

## Table of Contents

* [I. Preconditions](#section-i)
* [II. Building HDF5 Examples with CMake](#section-ii)
    * [1. Using presets](#using-presets)
    * [2. Using CTestScript.cmake](#using-ctestscript)
    * [3. Using Command Line CMake](#using-cmd-cmake)
* [III. Defaults in the CMakePresets.json file](#section-iii)
* [IV. Defaults in the HDF5_Examples_options.cmake file](#section-iv)

---

<a id="section-i"></a>
## I. Preconditions

1. We suggest you obtain the latest CMake for your platform from the Kitware web site. The HDF5 `z.y.x` product requires a **minimum CMake version of 3.26**.

2. You have installed the HDF5 library built with CMake, by executing the HDF Install Utility (the `*.msi` file in the binary package for Windows or the `*.sh` on Linux). You can obtain pre-built binaries from The HDF Group's website at [www.hdfgroup.org](https://www.hdfgroup.org).

3. Set the `HDF5_ROOT` CMake variable (`-DHDF5_ROOT=<install_path>`) or environment variable (`set(ENV{HDF5_ROOT} "<install_path>")`) to the installed location of HDF5.
   * **On Windows:**
     ```cmd
     HDF5_ROOT=C:/Program Files/HDF_Group/HDF5/z.y.x/
     ```
   * **On Unix:**
     ```bash
     HDF5_ROOT=<install root folder>/HDF_Group/HDF5/z.y.x/
     ```

   If you are using shared libraries, you may need to add to the path environment variable. Set the path environment variable to the installed location of the library files for HDF5.
   * **On Windows (`*.dll`):**
     ```cmd
     PATH=%PATH%;C:/Program Files/HDF_Group/HDF5/z.y.x/bin
     ```
   * **On Unix (`*.so`):**
     ```bash
     LD_LIBRARY_PATH=$LD_LIBRARY_PATH:<install root folder>/HDF_Group/HDF5/z.y.x/lib
     ```
   *(Note there are no quote characters used on Windows and all platforms use forward slashes)*

4. Use separate source and build directories. *(CMake commands are executed in the build directory)*

---

<a id="section-ii"></a>
## II. Building HDF5 Examples with CMake

<a id="using-presets"></a>
### 1. Using presets

> **NOTE:** The `CMakePresets.json` files created by HDF Group are intended to be used with the Ninja build system, which may need to be installed separately on some platforms.

Files in the `HDF5 install/HDF5Examples` directory:
* `CMakePresets.json`

Default build process:
Create a directory to run the examples, i.e., `\test_hdf5`. Copy the `HDF5Examples` folder to this directory. Change into the `HDF5Examples` directory where `CMakePresets.json` exists.

*(Other options can be changed by creating a `CMakeUsersPresets.json` file - Advanced usage).*

* Available configurations presets can be displayed by executing:
  ```bash
  cmake -S <path-to-source> --list-presets
  ```

* Using individual command presets (where `<compiler-type>` is `GNUC` or `MSVC` or `Clang`):
  ```bash
  cmake --preset ci-StdShar-<compiler-type>
  cmake --build --preset ci-StdShar-<compiler-type>
  ctest --preset ci-StdShar-<compiler-type>
  cpack --preset ci-StdShar-<compiler-type>
  ```

* Using the workflow preset to configure, build, and test the standard configuration:
  ```bash
  cmake --workflow --preset ci-StdShar-<compiler-type> --fresh
  ```

<a id="using-ctestscript"></a>
### 2. Using CTestScript.cmake

Files in the HDF5 install directory:
* `HDF5Examples` folder
* `CTestScript.cmake`
* `HDF5_Examples.cmake`
* `HDF5_Examples_options.cmake`

Default build process:
1. Create a directory to run the examples, i.e., `\test_hdf5`.
2. Copy the `HDF5Examples` folder, `CTestScript.cmake`, `HDF5_Examples.cmake`, and `HDF5_Examples_options.cmake` to this directory.

Configuration Details:
* The default source folder is defined as `HDF5Examples`. It can be changed with the `CTEST_SOURCE_NAME` script option.
* The default installation folder is defined for the platform. It can be changed with the `INSTALLDIR` script option. *(Note: Windows has issues with spaces and paths - The path will need to be set correctly.)*
* The default ctest configuration is defined as `Release`. It can be changed with the `CTEST_CONFIGURATION_TYPE` script option (must be the same as the value used with the `-C` command line option).
  * On Windows, you can set the `CTEST_VSVERS` script option to either `64_VS2022` or `64_VS2019`.
  * Alternately, set `CTEST_CMAKE_GENERATOR` option to `"Visual Studio 16 2019"` or `"Visual Studio 17 2022"`, and `CMAKE_GENERATOR_ARCHITECTURE` to `"x64"`.
* The default build configuration is defined to build and use **static libraries**. Shared libraries and other options can be changed by editing the `HDF5_Examples_options.cmake` file.

Execution:
* If the defaults are okay, execute from this directory:
  ```bash
  ctest -S HDF5_Examples.cmake -C Release -VV -O test.log
  ```
* If the defaults need change, execute:
  ```bash
  ctest -S HDF5_Examples.cmake,CTEST_SOURCE_NAME=MyExamples,INSTALLDIR=MyLocation -C Release -VV -O test.log
  ```

When executed, the `ctest` script will save the results to the log file `test.log`. If you wish to see more build and test information, add `-VV` to the ctest command. The output should show: `100% tests passed, 0 tests failed out of 206.`

<a id="using-cmd-cmake"></a>
### 3. Using Command Line CMake

#### A. Visual Configuration

The visual CMake executable is named `cmake-gui.exe` on Windows and should be available in your Start menu. For Linux, UNIX, and Mac users the executable is named `cmake-gui` or the ncurses-based `ccmake` and can be found where CMake was installed.

1. Specify the source and build directories. **Make the build and source directories different.** For example on Windows, if the source is at `C:\MyHDFstuff\hdf5ex`, then use `C:\MyHDFstuff\hdf5ex\build`.
2. Click the **Configure** button. Prompted for the generator (e.g., Visual Studio 15), CMake will read the `CMakeLists.txt` files and display options.
3. Adjust cache settings as needed (conflicts are highlighted in red).
4. Click the **Generate** button to produce the appropriate build files.
   * On Windows, solution/project files are created.
   * On Linux, Makefiles are created.

#### B. Alternative Command Line Configuration

Users can perform the configuration step without using the visual program. Executed within the build directory:

```bash
cmake -G "<generator>" [-D<options>] <sourcepath>
```

Where `<generator>` is (examples):
* `MSYS Makefiles`
* `MinGW Makefiles`
* `NMake Makefiles`
* `Unix Makefiles`
* `Visual Studio 15 2017`
* `Visual Studio 16 2019` *(add `-A` option for [Win32, x64, ARM, ARM64])*
* `Visual Studio 17 2022` *(add `-A` option for [Win32, x64, ARM, ARM64])*

Where `<options>` is:
* `H5EXAMPLE_BUILD_TESTING:BOOL=ON`
* `BUILD_SHARED_LIBS:BOOL=[ON | OFF]`
* `H5EXAMPLE_BUILD_FORTRAN:BOOL=[ON | OFF]`
* `H5EXAMPLE_BUILD_JAVA:BOOL=[ON | OFF]`

If the hdf5 library was built with a namespace (i.e., `hdf5::`), add:
* `-D HDF5_NAMESPACE:STRING=hdf5::`

Example command line on Windows in `c:\MyHDFstuff\hdf5ex\build`:
```cmd
cmake -G "Visual Studio 16 2019" -DH5EXAMPLE_BUILD_TESTING:BOOL=ON -DBUILD_SHARED_LIBS:BOOL=ON ..
```

#### C. Build HDF5 examples

To build from the command line, navigate to your build directory and execute:
```bash
cmake --build . --config {Debug | Release}
```
> **NOTE:** `--config` may be optional on your platform. We recommend `Release` if using pre-built binaries from HDF.

If you wish to use the Visual Studio environment, open the solution file in your build directory, select Debug or Release, and build.

#### D. Test HDF5 Examples

To test the build, navigate to your build directory and execute:
```bash
ctest . -C {Debug | Release}
```

### 4. CMake Support Files
The files that support building with CMake are all of the files in the `config/cmake` folder and the `CMakeLists.txt` files in each source folder. `CTestConfig.cmake` is specific to the internal testing performed by The HDF Group and should be altered for the user's needs. The `cacheinit.cmake` file settings are used by The HDF Group for daily testing and can also be altered/ignored.

---

<a id="section-iii"></a>
## III. Defaults in the CMakePresets.json file

```json
"generator": "Ninja"
"binaryDir": "${sourceParentDir}/build/${presetName}"
"name": "ci-StdShar"
"BUILD_SHARED_LIBS": "ON"
"USE_SHARED_LIBS": "ON"
"CMAKE_BUILD_TYPE": "RelWithDebInfo"
"H5EXAMPLE_BUILD_FORTRAN": "ON"
"H5EXAMPLE_BUILD_CXX": "ON"
"H5EXAMPLE_BUILD_JAVA": "ON"
"HDF5_NAMESPACE": {"type": "STRING", "value": "hdf5::"}
"HDF5_PACKAGE_NAME": {"type": "STRING", "value": "hdf5"}
"H5EXAMPLE_BUILD_TESTING": "ON"
```

---

<a id="section-iv"></a>
## IV. Defaults in the HDF5_Examples_options.cmake file

```cmake
BUILD_SHARED_LIBS:BOOL=OFF
H5EXAMPLE_BUILD_C:BOOL=ON
H5EXAMPLE_BUILD_HL:BOOL=OFF
H5EXAMPLE_BUILD_CXX:BOOL=OFF
H5EXAMPLE_BUILD_FORTRAN:BOOL=OFF
H5EXAMPLE_BUILD_JAVA:BOOL=OFF
H5EXAMPLE_BUILD_FILTERS:BOOL=OFF
H5EXAMPLE_BUILD_TESTING:BOOL=OFF
H5EXAMPLE_ENABLE_PARALLEL:BOOL=OFF
```
