# Build and Install HDF5 Applications with CMake

> **Notes:** This short instruction is written for users who want to quickly build HDF5 applications using the CMake tools. Users can adapt these instructions for their own applications. For more information, see the "Minimum C Project Files for CMake" section.
>
> More information about using CMake can be found at the Kitware site, [www.cmake.org](https://www.cmake.org).
>
> CMake uses the command line; however, the visual CMake tool is available for the configuration step. The steps are similar for all of the operating systems supported by CMake.
>
> 1. CMake for HDF5 development should be usable on any system where CMake is supported. Please send us any comments on how CMake support can be improved on any system.
> 2. See the appendix at the bottom of this file for an example of using a `ctest` script for building and testing. See [INSTALL_CMake.md](./INSTALL_CMake.md) for more information.
> 3. See the [CMake Config Mode Search Procedure](https://cmake.org/cmake/help/latest/command/find_package.html) for more information on finding packages.

---

## Table of Contents

* [I. Preconditions](#section-i)
* [II. Building HDF5 Applications with CMake](#section-ii)
* [III. Minimum C Project Files for CMake](#section-iii)
* [IV. Appendix](#section-iv)

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

   If you are using filter plugin libraries, you will need to set the `HDF5_PLUGIN_PATH` environment variable.
   * **On Windows:**
     ```cmd
     HDF5_PLUGIN_PATH=C:/Program Files/HDF_Group/HDF5/z.y.x/lib/plugin
     ```
   * **On Unix:**
     ```bash
     HDF5_PLUGIN_PATH=<install root folder>/HDF_Group/HDF5/z.y.x/lib/plugin
     ```
   *(Note: there are no quote characters used on Windows and all platforms use forward slashes).*

4. Create separate source and build directories. *(CMake commands are executed in the build directory)*.

5. Create a `CMakeLists.txt` file(s) for your source. See [Section III](#section-iii) below.

---

<a id="section-ii"></a>
## II. Building HDF5 Applications with CMake

Go through these steps to build HDF5 applications with CMake. *(The application must support building with CMake.)*

1. Run CMake
2. Configure the cache settings
3. Build HDF5 Applications
4. Test HDF5 Applications

These steps are described in more detail below.

### 1. Run CMake
The visual CMake executable is named `cmake-gui.exe` on Windows and should be available in your Start menu. For Linux, UNIX, and Mac users the executable is named `cmake-gui` and can be found where CMake was installed.

Specify the source and build directories. **Make the build and source directories different.** For example on Windows, if the source is at `c:\MyHDFstuff\hdf5`, then use `c:\MyHDFstuff\hdf5\build` or `c:\MyHDFstuff\build\hdf5` for the build directory.

**PREFERRED:**
Users can perform the configuration step without using the visual `cmake-gui` program. The following is an example command line configuration step executed within the build directory:

```bash
cmake -G "<generator>" [-D<options>] <sourcepath>
```

Where `<generator>` is (examples):
* `MinGW Makefiles`
* `NMake Makefiles`
* `Unix Makefiles`
* `Visual Studio 15 2017`
* `Visual Studio 15 2017 Win64`
* `Visual Studio 16 2019` *(in addition VS2019 will need to set the `-A` option, [Win32, x64, ARM, ARM64])*
* `Visual Studio 17 2022` *(in addition VS2022 will need to set the `-A` option, [Win32, x64, ARM, ARM64])*

`<options>` can include:
* `BUILD_TESTING:BOOL=ON`
* `BUILD_SHARED_LIBS:BOOL=[ON | OFF]`

### 2. Configure the cache settings

**2.1 Visual CMake users** Click the Configure button. If this is the first time you are running `cmake-gui` in this directory, you will be prompted for the generator you wish to use (for example on Windows, Visual Studio 16 2019). CMake will read in the `CMakeLists.txt` files from the source directory and display options for the HDF5 project. After the first configure you can adjust the cache settings and/or specify locations of other programs.

Any conflicts or new values will be highlighted by the configure process in red. Once you are happy with all the settings and there are no more values in red, click the Generate button to produce the appropriate build files.

* On Windows, if you are using a Visual Studio generator, the solution and project files will be created in the build folder.
* On linux, if you are using the Unix Makefiles generator, the Makefiles will be created in the build folder.

**2.2 Alternative command line example** On Windows in the `c:\MyHDFstuff\hdf5\build` directory:
```cmd
cmake -G "Visual Studio 16 2019" -A "x64" -DBUILD_TESTING:BOOL=ON ..
```

### 3. Build HDF5 Applications
On Windows, you can build HDF5 applications using either the Visual Studio Environment or the command line. The command line is normally used on Linux, Unix, and Mac.

To build from the command line, navigate to your build directory and execute the following:
```bash
cmake --build . --config {Debug | Release}
```
> **NOTE:** `--config {Debug | Release}` may be optional on your platform. We recommend choosing either `Debug` or `Release` on Windows. If you are using the pre-built binaries from HDF, use `Release`.

**3.1 Visual Studio Environment**
If you wish to use the Visual Studio environment, open the solution file in your build directory. Be sure to select either `Debug` or `Release` and build the solution.

### 4. Test HDF5 Applications
To test the build, navigate to your build directory and execute:
```bash
ctest . -C {Debug | Release}
```
> **NOTE:** `-C {Debug | Release}` may be optional on your platform. We recommend choosing either `Debug` or `Release` to match the build step on Windows.

### 5. CMake Support Files
The files that support building with CMake are all of the files in the `config/cmake` folder, the `CMakeLists.txt` files in each source folder, and `CTestConfig.cmake`.
* `CTestConfig.cmake` is specific to the internal testing performed by The HDF Group. It should be altered for the user's installation and needs.
* The `cacheinit.cmake` file settings are used by The HDF Group for daily testing. It should be altered/ignored for the user's installation and needs.

---

<a id="section-iii"></a>
## III. Minimum C Project Files for CMake

Given the preconditions in Section I, create a `CMakeLists.txt` file at the source root. Include the following text in the file:

```cmake
cmake_minimum_required (VERSION 3.26)
project (HDF5MyApp C)

find_package (HDF5 NAMES hdf5 CONFIG REQUIRED COMPONENTS C)
# find_package (HDF5) # Find non-cmake built HDF5

set (example hdf_example)

add_executable (${example} ${PROJECT_SOURCE_DIR}/${example}.c)
target_link_libraries (${example} PRIVATE hdf5::hdf5)

enable_testing ()
include (CTest)

add_test (NAME test_example COMMAND ${example})
```

`hdf5::hdf5` carries HDF5's include directories and its own dependencies, so
there is no need to set `INCLUDE_DIRECTORIES` or to name the library variables
directly.

#### Public target names

Link against these rather than against `hdf5-shared` or `hdf5-static`. The
names do not encode the linkage, so the same project file works against a
static installation, a shared one, or one providing both:

| Target | Library |
| ------ | ------- |
| `hdf5::hdf5` | C |
| `hdf5::hdf5_hl` | High-level C |
| `hdf5::hdf5_cpp` | C++ |
| `hdf5::hdf5_hl_cpp` | High-level C++ |
| `hdf5::hdf5_fortran` | Fortran |
| `hdf5::hdf5_hl_fortran` | High-level Fortran |
| `HDF5::HDF5` | All of the above that are available |

Each installed tool is available as `hdf5::<tool>`, for example `hdf5::h5diff`.

These are the same names CMake's own `FindHDF5` module provides, so a project
using them does not need to know whether HDF5 was located through `FindHDF5`
or through HDF5's `hdf5-config.cmake`. They are also defined when HDF5 is
built as a subproject with `add_subdirectory()`, so the same
`target_link_libraries()` call works there too.

#### Choosing static or shared

When an installation provides both, `find_package()` resolves to the shared
libraries and falls back to static. To choose deliberately, request the linkage
as a component:

```cmake
find_package (HDF5 NAMES hdf5 CONFIG REQUIRED COMPONENTS C static)
target_link_libraries (${example} PRIVATE hdf5::hdf5)
```

Setting `HDF5_USE_STATIC_LIBRARIES` before `find_package()` has the same
effect, matching `FindHDF5`. Requesting both `static` and `shared` leaves the
public targets on the default and warns, since one name cannot refer to both.

This preference applies to the whole `find_package()` call, not just to the
public targets: it also decides which `HDF5_<lang>_<LINKAGE>_LIBRARY` variables
and Fortran module directory are set. Before HDF5 2.3.0, a call that named no
linkage component preferred the static libraries. A project that relied on that
should request the `static` component or set `HDF5_USE_STATIC_LIBRARIES`.

#### Deprecated names

The linkage-qualified targets, and the library variables that go with them, are
deprecated as of 2.3.0:

| Deprecated | Use instead |
| ---------- | ----------- |
| `hdf5-shared`, `hdf5-static` | `hdf5::hdf5` |
| `hdf5_hl-shared`, `hdf5_hl-static` | `hdf5::hdf5_hl` |
| `hdf5_cpp-shared`, `hdf5_cpp-static` | `hdf5::hdf5_cpp` |
| `hdf5_hl_cpp-shared`, `hdf5_hl_cpp-static` | `hdf5::hdf5_hl_cpp` |
| `hdf5_fortran-shared`, `hdf5_fortran-static` | `hdf5::hdf5_fortran` |
| `hdf5_hl_fortran-shared`, `hdf5_hl_fortran-static` | `hdf5::hdf5_hl_fortran` |
| `HDF5_<lang>_SHARED_LIBRARY`, `HDF5_<lang>_STATIC_LIBRARY` | the matching `hdf5::` target |

They will be removed in a future major release, so new projects should use the `hdf5::` names, and existing
projects should migrate to use the new names.

---

<a id="section-iv"></a>
## IV. APPENDIX

Below is an example of a `ctest` script that can be used to build the examples. Adjust the values as necessary. Note that the defaults can be entered on the command line and the build folder is created as a sub-folder. Windows should adjust the forward slash to double backslashes, except for the `HDF_DIR` environment variable.

> **NOTE:** this file is available in the HDF5 repository, for more information see:
> [USING_CMake_Examples.md](./USING_CMake_Examples.md)

```bash
ctest -S HDF5_Examples.cmake -C Release -V -O test.log
```

Also available at the HDF web site is a CMake application framework template. You can quickly add files to the framework and execute the script to compile your application with an installed HDF5 binary.

---
**For further assistance, send email to help@hdfgroup.org**
