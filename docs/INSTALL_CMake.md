# Build and Install HDF5 C, C++, Fortran, High-Level Libraries and Tools with CMake

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
> **Note:** See the CMake documentation for more information on setting the logging levels: [`CMAKE_MESSAGE_LOG_LEVEL`](https://cmake.org/cmake/help/latest/variable/CMAKE_MESSAGE_LOG_LEVEL.html).

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

> **Note:** For ROS3 (S3 support), add `-DHDF5_ENABLE_ROS3_VFD=ON` to any preset.

See [Section XI]((#section-xi)) for creating custom preset configurations.

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

The workflow preset (shown in Quick Start above) runs all these steps automatically.

### Advanced Build Methods
If you cannot use presets or need more control, see:
* [Section III](#section-iii): Building with CMake Script Mode (requires external files, for automation/CI)
* [Section IV](#section-iv): Building with CMake Command Mode (manual configuration, for advanced users)

---

<a id="section-iii"></a>
## III. Advanced: Building HDF5 Libraries with CMake Script Mode

> **NOTE:** Most users should use Section II (CMake Presets) instead. This method is provided for advanced users and automated builds.

This short set of instructions is written for users who want to quickly build the HDF5 C, C++ and Fortran shared libraries and tools from the HDF5 source code package using the CMake tools. This procedure will use the default settings in the `config/cmake/cacheinit.cmake` file. The HDF Group recommends using the presets process to build HDF5.

> **NOTE:** When using the presets process, the `CMakePresets.json` file in the source directory will configure, build, test, and package HDF5 with the same options that are set in the `cacheinit.cmake` file. In addition, it will get the optional files listed below that are needed, from the appropriate repositories. See [Section II](#section-ii): RECOMMENDED Quick Start with CMake Presets.

### Individual files needed as mentioned in this document

Download from https://github.com/HDFGroup/hdf5/blob/develop/config/cmake/scripts:
* `CTestScript.cmake`  -- CMake build script
* `HDF5config.cmake`   -- CMake configuration script
* `HDF5options.cmake`  -- CMake configuration options script

HDF5 filter plugins:
* **Plugins:** [hdf5_plugins.tar.gz](https://github.com/HDFGroup/hdf5_plugins)

External libraries:
* **ZLIB:** [zlib-1.3.1.tar.gz](https://github.com/madler/zlib/releases/download/v1.3.1/zlib-1.3.1.tar.gz)
* **ZLIBNG:** [2.2.4.tar.gz](https://github.com/zlib-ng/zlib-ng/archive/refs/tags/2.2.4.tar.gz)
* **LIBAEC:** [libaec-1.1.3.tar.gz](https://github.com/MathisRosenhauer/libaec/releases/download/v1.1.3/libaec-1.1.3.tar.gz)

### Build Scripts for Windows or Linux

To build HDF5 with the SZIP and ZLIB external libraries you will need to:

1. Change to the development directory `myhdfstuff`.

2. Download/copy the individual files mentioned above to `myhdfstuff`. Do not uncompress the `tar.gz` files.

3. Change to the source directory `hdf5-2.x.y`. `CTestScript.cmake` file should not be modified.

4. Edit the platform configuration file, `HDF5options.cmake`, if you want to change the default build environment. The file is a compilation of the most used options and by commenting/uncommenting lines the options can easily be changed.

5. From the `myhdfstuff` directory execute the CTest Script with the following options:

    * **32-bit Windows with Visual Studio 2022:**

      `ctest -S HDF5config.cmake,BUILD_GENERATOR=VS2022 -C Release -VV -O hdf5.log`

    * **64-bit Windows with Visual Studio 2022:**

      `ctest -S HDF5config.cmake,BUILD_GENERATOR=VS202264 -C Release -VV -O hdf5.log`

    * **32-bit Windows with Visual Studio 2019**:

      `ctest -S HDF5config.cmake,BUILD_GENERATOR=VS2019 -C Release -VV -O hdf5.log`

    * **64-bit Windows with Visual Studio 2019**:

      `ctest -S HDF5config.cmake,BUILD_GENERATOR=VS201964 -C Release -VV -O hdf5.log`

    * **32-bit Windows with Visual Studio 2017**:

      `ctest -S HDF5config.cmake,BUILD_GENERATOR=VS2017 -C Release -VV -O hdf5.log`

    * **64-bit Windows with Visual Studio 2017**:

      `ctest -S HDF5config.cmake,BUILD_GENERATOR=VS201764 -C Release -VV -O hdf5.log`

    * **Linux and Mac:**

      `ctest -S HDF5config.cmake,BUILD_GENERATOR=Unix -C Release -VV -O hdf5.log`

    The supplied build scripts are versions of the above.


    The command above will configure, build, test, and create an install package in
    the `myhdfstuff` folder. It will have the format: `HDF5-2.X.Y-<platform>.<zip or tar.gz>`.

    On Unix, `<platform>` will be "Linux". A similar `.sh` file will also be created. On Windows, `<platform>` will be "win64" or "win32". If you have an installer on your system, you will also see a similar file that ends in either `.exe` (NSIS) or `.msi` (WiX).

    Notes on the command line options.

    * The `-S` option uses the script version of ctest.

    * The value for the `-C` option (as shown above, `-C Release`) must match the setting for `CTEST_CONFIGURATION_TYPE` in the platform configuration file.

    * The `-VV` option is for most verbose; use `-V` for less verbose.

    * The `-O hdf5.log` option saves the output to a log file `hdf5.log`.

6. To install, `X.Y` is the current release version.

    * **On Windows (with WiX):** Execute `HDF5-2.X.Y-win32.msi` or `HDF5-2.X.Y-win64.msi`. By default this program will install the HDF5 library into the `C:\Program Files` directory and will create the following directory structure:

          HDF_Group
          --HDF5
          ----2."X"."Y"
          ------bin
          ------include
          ------lib
          --------plugin
          ------cmake

    * **On Linux:** Change to the install destination directory (create it if doesn't exist) and execute `<path-to>/myhdfstuff/HDF5-2.X.Y-Linux.sh`. After accepting the license, the script will prompt:

          By default the HDF5 will be installed in:
              <current directory>/HDF5-2.X.Y-Linux
          Do you want to include the subdirectory HDF5-2.X.Y-Linux?
          Saying no will install in: "<current directory>" [Yn]:

      Note that the script will create the following directory structure
      relative to the install point:

          HDF_Group
          --HDF5
          ----2."X"."Y"
          ------bin
          ------include
          ------lib
          --------plugin
          ------share

    * **On macOS:** Locate `HDF5-2.X.Y-Darwin.dmg` file in the `myhdfstuff` folder. Click
      on the file to proceed with installation. After accepting the license,
      there will be a folder with the following structure:

          HDF_Group
          --HDF5
          ----2."X"."Y"
          ------bin
          ------include
          ------lib
          --------plugin
          ------share

    By default the installation will create the `bin`, `include`, `lib` and `cmake`
    folders in the `<install destination directory>/HDF_Group/HDF5/2.X.Y`.
    The `<install destination directory>` depends on the build platform:

    * Windows will set the default to `C:/Program Files/HDF_Group/HDF5/2.X.Y`

    * Linux will set the default to `myhdfstuff/HDF_Group/HDF5/2.X.Y`

    The default can be changed by adding `,INSTALLDIR=<my new dir>` to the
    `ctest -S HDF5config.cmake...` command. For example on Linux:
    `ctest -S HDF5config.cmake,INSTALLDIR=/usr/local/myhdf5,BUILD_GENERATOR=Unix -C Release -VV -O hdf5.log`.

---

<a id="section-iv"></a>
## IV. Advanced: Building HDF5 Libraries with CMake Command Mode

> **NOTE:** Most users should use [Section II](#section-ii) (CMake Presets) instead. This method is provided for advanced users who need manual control.

This short set of instructions is written for users who want to quickly build just the HDF5 C static library and tools from the HDF5 source code package using the CMake command line tools. Avoid the use of drive letters in paths on Windows.

Go through these steps:

1. Change to the development directory `myhdfstuff`.
2. Uncompress the HDF5 source file.
3. Create a folder `build` in the `myhdfstuff` directory.
4. Change into the `build` folder.
5. Configure the C library, tools and tests with one of the following commands:

    * **On Windows 32 bit**
        ```cmd
        cmake -G "Visual Studio 16 2019" -A Win32 -DCMAKE_BUILD_TYPE:STRING=Release -DBUILD_SHARED_LIBS:BOOL=OFF -DBUILD_TESTING:BOOL=ON -DHDF5_BUILD_TOOLS:BOOL=ON ..\hdf5-2."X"."Y"
        ```
    * **On Windows 64 bit**
        ```cmd
        cmake -G "Visual Studio 16 2019" -A x64 -DCMAKE_BUILD_TYPE:STRING=Release -DBUILD_SHARED_LIBS:BOOL=OFF -DBUILD_TESTING:BOOL=ON -DHDF5_BUILD_TOOLS:BOOL=ON ..\hdf5-2."X"."Y"
        ```
    * **On Linux and Mac**
        ```bash
        cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE:STRING=Release -DBUILD_SHARED_LIBS:BOOL=OFF -DBUILD_TESTING:BOOL=ON -DHDF5_BUILD_TOOLS:BOOL=ON ../hdf5-2."X"."Y"
        ```

    where `"X"."Y"` is the current release version.

6. Build with: `cmake --build . --config Release`
7. Test with: `ctest . -C Release`
8. Create an install image: `cpack -C Release CPackConfig.cmake`
9. To install

    * On **Windows (with WiX installed)**, execute `HDF5-2.X.Y-win32.msi or
      HDF5-2.X.Y-win64.msi`. By default this program will install the hdf5
      library into the `C:\Program Files` directory and will create the
      following directory structure:

        ```
        HDF_Group
        --HDF5
        ----2.X.Y
        ------bin
        ------include
        ------lib
        --------plugin
        ------cmake
        ```

    * On **Linux**, change to the install destination directory (create if doesn't exist) and execute: `<path-to>/myhdfstuff/build/HDF5-2.X.Y-Linux.sh`. After accepting the license, the script will prompt:

        ```
        By default the HDF5 will be installed in:
        "<current directory>/HDF5-2.X.Y-Linux"
        Do you want to include the subdirectory HDF5-2.X.Y-Linux?
        Saying no will install in: "<current directory>" [Yn]:
        ```

        Note that the script will create the following directory structure
        relative to the install point:

        ```
        HDF_Group
        --HDF5
        ----2.X.Y
        ------bin
        ------include
        ------lib
        --------plugin
        ------share
        ```

    * On **macOS**, locate `HDF5-2.X.Y-Darwin.dmg` file in the build folder. Click on the  file to proceed with installation. After accepting the license, there will be a folder with the following structure:

        ```
        HDF_Group
        --HDF5
        ----2.X.Y
        ------bin
        ------include
        ------lib
        --------plugin
        ------share
        ```
---

<a id="section-v"></a>
## V. Further Considerations

1. We suggest you obtain the latest CMake from the Kitware web site. The HDF5 2."X"."Y" product **requires a minimum CMake version 3.26**.

2. If you plan to use Zlib/Zlib-ng, Szip (aka libaec) and/or HDF5 filter plugins, refer to the instructions in [INSTALL_Filters.md](./INSTALL_Filters.md).

3. If you are building on Apple Darwin platforms, you should add the following options:

    * Compiler choice - use Xcode by setting the `CC` and `CXX` environment variables.

    * Shared Fortran is not supported, build static with `BUILD_SHARED_LIBS:BOOL=OFF`.

    * Additional options:

            CMAKE_ANSI_CFLAGS:STRING=-fPIC
            CTEST_USE_LAUNCHERS:BOOL=ON

4. Windows developers should install NSIS or WiX to create an install image with CPack. Visual Studio Express users will not be able to package HDF5 into an install image executable.

5. Developers can copy the `config/cmake/cacheinit.cmake` file and alter the settings for the developers' environment. Then the only options needed on the command line are those options that are different. Example using the default cache file:

    ```cmd
    cmake -C ../config/cmake/cacheinit.cmake ^
            -G "Visual Studio 16 2019" ^
            -DHDF5_ENABLE_SZIP_SUPPORT:BOOL=OFF ^
            -DHDF5_ENABLE_ZLIB_SUPPORT:BOOL=OFF ^
            -DCMAKE_BUILD_TYPE:STRING=Release ^
            ..
    ```

6. CMake uses a toolchain of utilities to compile, link libraries,
    create archives, and other tasks to drive the build. The toolchain
    utilities available are determined by the languages enabled. In normal
    builds, CMake automatically determines the toolchain for host builds
    based on system introspection and defaults. In cross-compiling
    scenarios, a toolchain file may be specified with information about
    compiler and utility paths.

### Variables and Properties

Several variables relate to the language components of a toolchain which
are enabled. `CMAKE_<LANG>_COMPILER` is the full path to the compiler used
for `<LANG>`. `CMAKE_<LANG>_COMPILER_ID` is the identifier used by CMake for
the compiler and `CMAKE_<LANG>_COMPILER_VERSION` is the version of the compiler.

The `CMAKE_<LANG>_FLAGS` variables and the configuration-specific equivalents
contain flags that will be added to the compile command when compiling a
file of a particular language.

As the linker is invoked by the compiler driver, CMake needs a way to
determine which compiler to use to invoke the linker. This is calculated
by the `LANGUAGE` of source files in the target, and, in the case of static
libraries, the language of the dependent libraries. The choice CMake makes
may be overridden with the `LINKER_LANGUAGE` target property.

See the CMake help for more information on using toolchain files.

To use a toolchain file with the supplied cmake scripts, see the
`HDF5options.cmake` file under the toolchain section.

### Notes: CMake and HDF5

* CMake support for HDF5 development should be usable on any system where CMake is supported. Please send us any
  comments on how CMake support can be improved on any system. Visit the Kitware site for more information about CMake.

* Build and test results can be submitted to our HDF5 CDash server. The CDash server for community submissions of HDF5
   is at https://my.cdash.org.

    We ask that all submissions include the configuration information and
    contact information in the CTest Notes Files upload step. See the
    current reports on CDash for examples.

    Please follow the convention that "NIGHTLY" submissions maintain the same
    configuration every time. "EXPERIMENTAL" submissions can be expected to
    be different for each submission.

* See the appendix at the bottom of this file for examples of using a ctest script for building and testing. Using a
* ctest script is preferred because of its flexibility.

### Notes: CMake in General

1. More information about using CMake can be found at the Kitware site at
        www.cmake.org.

2. CMake uses the command line; however, the visual CMake tool is
    available for the configuration step. The steps are similar for
    all the operating systems supported by CMake.

3. Setting the installation location from the command line at configure time

    a. Using the `--install-prefix` command line option, which is available since
        CMake version 3.21:

        cmake --install-prefix /my/folder/to/install/to ..

    b. Using `-DCMAKE_INSTALL_PREFIX`:

        cmake -DCMAKE_INSTALL_PREFIX=/my/folder/to/install/to ..

    c. Using the `CMAKE_INSTALL_PREFIX` environment variable, which is available
        since CMake version 3.29:

        CMAKE_INSTALL_PREFIX=/my/folder/to/install/to cmake ..

---

<a id="section-vi"></a>
## VI. Options for building HDF5 Libraries with CMake Command Line

To build the HDF5 Libraries with CMake, go through the five steps below:

### Step 1: Run CMake

The visual CMake executable is named "cmake-gui.exe" on Windows and should be available in your Start menu. For Linux,
UNIX, and macOS users the executable is named `cmake-gui` and can be found where CMake was installed.

Another option is to use the presets file, `CMakePresets.json`, to configure, build, test, and package HDF5, see
[Section XI](#section-xi) for use of that file. You can create a `CMakeUserPresets.json` file to create a specific
configuration for your environment. Note that Visual Studio and XCode can use the presets files.

#### Specify the source and build directories

> **Note:** Make the build and source directories different.

For example, on Windows, if the source is at `c:\MyHDFstuff\hdf5`,
then use `c:\MyHDFstuff\hdf5\build` or `c:\MyHDFstuff\build\hdf5` as the
build directory.

**RECOMMENDED:** Users can perform the configuration step without using the visual `cmake-gui` program. We use the file
`cacheinit.cmake` in the `config/cmake` source folder for our testing. This file enables all of the basic options and we
turn specific options on or off for testing using the following command line within the build directory:

    cmake -C <sourcepath>/config/cmake/cacheinit.cmake -G "<generator>"  [-D<options>]  <sourcepath>

Where `cacheinit.cmake` is a file used to populate an initial CMake cache with some common option settings,

`<generator>` is (examples):
* MinGW Makefiles
* NMake Makefiles
* Unix Makefiles
* Ninja
* Visual Studio 15 2017
* Visual Studio 15 2017 Win64
* Visual Studio 16 2019
* Visual Studio 17 2022

and `<options>` are any CMake options to be added to the configuration:

* `<HDF5OPTION>:BOOL=[ON | OFF]`

### Step 2: Configure the Cache Settings

Visual CMake users, click the Configure button. If this is the first time you are running `cmake-gui` in this directory, you will be prompted for the generator you wish to use (for example on Windows, Visual Studio 14). CMake will read in the `CMakeLists.txt` files from the source directory and display options for the HDF5 project. After the first configure you can adjust the cache settings and/or specify the locations of other programs.

Any conflicts or new values will be highlighted by the configure
process in red. Once you are happy with all the settings and there are no
more values in red, click the Generate button to produce the appropriate
build files.

On Windows, if you are using a Visual Studio generator, the solution and
project files will be created in the build folder.

On Linux, if you are using the Unix Makefiles generator, the Makefiles will
be created in the build folder.

Preferred command line example on Windows in `c:\MyHDFstuff\hdf5\build` directory:

    cmake -C ../config/cmake/cacheinit.cmake -G "Visual Studio 16 2019" "-Ax64" \
          -DHDF5_ENABLE_SZIP_SUPPORT:BOOL=OFF -DHDF5_ENABLE_ZLIB_SUPPORT:BOOL=OFF \
          -DCMAKE_BUILD_TYPE:STRING=Release ..

On Windows, if you are using a Visual Studio Express version you must be sure that the following two options are correctly set/unset:

    HDF5_NO_PACKAGES:BOOL=ON
    HDF5_USE_FOLDERS:BOOL=OFF

### Step 3: Build HDF5

On Windows, you can build HDF5 using either the Visual Studio Environment
or the command line. The command line can be used on all platforms;
Windows, Linux, Unix, and macOS.

To build from the command line, navigate to your build directory and
execute the following:

```bash
cmake --build . --config {Debug | Release}
```

> **NOTE:** `--config {Debug | Release}` may be optional on your platform. We recommend choosing either `Debug` or `Release` on Windows.

If you wish to use the Visual Studio environment, open the solution
file in your build directory. Be sure to select either `Debug` or
`Release` and build the solution.

External libraries (zlib, szip and plugins) can be configured to allow
building the libraries by downloading from a GIT repository or by using
a compressed file. See the instructions in [INSTALL_Filters.md](./INSTALL_Filters.md) to do this.

### Step 4: Test HDF5

To test the build, navigate to your build directory and execute:

```bash
ctest . -C {Debug | Release}
```

> **NOTE:** `-C {Debug | Release}` may be optional on your platform. We recommend choosing either `Debug` or `Release` to match the build step on Windows.

### Step 5: Packaging HDF5 (Create an Install Image)

To package the build into a simple installer using WiX toolset or the NullSoft installer NSIS
on Windows, or into compressed files (`.tar.gz`, `.sh`, `.zip`), use the CPack tool.

To package the build, navigate to your build directory and execute:

    cpack -C {Debug | Release} CPackConfig.cmake

> **NOTE:** See the sections below with NSIS and WiX information below. Also, if you are using a Visual Studio Express version or want to disable the packaging components, set `HDF5_NO_PACKAGES` to `ON` (on the command line add `-DHDF5_NO_PACKAGES:BOOL=ON`).

### Additional Information

#### CMake Files for Building and Testing HDF5

The files that support building HDF5 with CMake are all the files in the
`config/cmake` folder, the `CMakeLists.txt` files in each source folder, and
`CTestConfig.cmake`. `CTestConfig.cmake` is specific to the internal testing
performed by The HDF Group. It should be altered for the user's
installation and needs. The `cacheinit.cmake` file settings are used by
The HDF Group for daily testing. It should be altered/ignored for the user's
installation and needs. More information about using CMake can be found at the Kitware site, www.cmake.org.

#### Nullsoft Scriptable Install System

The Nullsoft Scriptable Install System (NSIS) is an open source installation
system. It was created by the WinAmp authors to distribute that application,
but it is now a general-purpose system which anyone might use. NSIS installers
recognize `/S` for silent installation and `/D=dir` to specify the
output directory, which is where the program will be installed. These
options are case-sensitive, so be sure to type them in upper case.

#### WiX Toolset

WiX--the Windows Installer XML toolset--lets developers create installers for Windows Installer, the Windows installation engine. See http://wixtoolset.org.

#### Backward Compatibility

The 2.0.0 version of the HDF5 library can be configured to
set all versioned functions to the version that was available in one of these HDF5_DEFAULT_API_VERSIONs

    HDF5_DEFAULT_API_VERSION:STRING=v114
    HDF5_DEFAULT_API_VERSION:STRING=v112
    HDF5_DEFAULT_API_VERSION:STRING=v110
    HDF5_DEFAULT_API_VERSION:STRING=v18
    HDF5_DEFAULT_API_VERSION:STRING=v16

This allows existing code to be compiled with the
v2.0 library without requiring immediate changes to the application
source code. Note that because 2.0.0 is a major release, as long as the existing application
code doesn't use symbols removed in 2.0.0, the code can be compiled and run with
the new library.

For additional configuration options and other details,
see "API Compatibility Macros": https://support.hdfgroup.org/documentation/hdf5/latest/api-compat-macros.html.

#### Generating Documentation with Doxygen

One can optionally build the documentation files for the HDF5 C library with Doxygen.
By default, this option is disabled. To build the HTML files, set the
`HDF5_BUILD_DOC` option.

    cmake -G "<generator>" -DHDF5_BUILD_DOC:BOOL=ON  <sourcepath>

Configuration will halt if the required applications are not available.
To build:

    cmake --build . --config {Debug | Release}

or

    make doxygen

#### Parallel vs Serial Library

The HDF5 library can be configured to use MPI and MPI-IO for
parallelism on a distributed multi-processor system:

    HDF5_ENABLE_PARALLEL:BOOL=ON

Read [README_HPC.md](./README_HPC.md) for detailed information.

The threadsafe, C++ ,and Java interfaces are not compatible
with the parallel option. Unless `HDF5_ALLOW_UNSUPPORTED` has been set on the configure line, the following options must be disabled: `HDF5_ENABLE_THREADSAFE`, `HDF5_BUILD_CPP_LIB`, `HDF5_BUILD_JAVA`.

#### Threadsafe Capability

The HDF5 library can be configured to be thread-safe (on a very
large scale) with the `HDF5_ENABLE_THREADSAFE` flag to the configure
script. For further information, see the "Technical Notes" category
in the HDF5 library documentation: https://support.hdfgroup.org/documentation/hdf5/latest/.

The high-level, C++, Fortran and Java interfaces are not compatible
with the thread-safety option because the lock is not hoisted
into the higher-level API calls.
Unless `HDF5_ALLOW_UNSUPPORTED` has been set on the configure line,
the following options must be disabled:
    `HDF5_BUILD_HL_LIB`, `HDF5_BUILD_FORTRAN`, `HDF5_BUILD_CPP_LIB`, `HDF5_BUILD_JAVA`.

---

<a id="section-vii"></a>
## VII. CMake Option Defaults for HDF5

See [INSTALL_CMake_options](./INSTALL_CMake_options.md).

#### Java Implementation Selection (as of HDF5 2.0)

HDF5 Java bindings support two native interface implementations:
- JNI (Java Native Interface). Default, works with Java 8+, production-stable.
- FFM (Foreign Function & Memory). Optional, requires Java 25+, modern native access.

Maven Artifacts:

- `org.hdfgroup:hdf5-java-ffm` is FFM implementation
- `org.hdfgroup:hdf5-java-jni` is JNI implementation

Both implementations use the same `hdf.hdf5lib.*` package structure for seamless migration.

To build HDF5 with Maven deployment support:

    cmake -DHDF5_BUILD_JAVA:BOOL=ON -DHDF5_ENABLE_MAVEN_DEPLOY:BOOL=ON ../hdf5

To build Maven snapshot versions for development:

    cmake -DHDF5_BUILD_JAVA:BOOL=ON -DHDF5_ENABLE_MAVEN_DEPLOY:BOOL=ON -DHDF5_MAVEN_SNAPSHOT:BOOL=ON ../hdf5

> **Note:** FFM is selected for Java 25+ if `HDF5_ENABLE_JNI` is `OFF`.
> To force JNI even with Java 25+:
>
> ```
> cmake -DHDF5_BUILD_JAVA:BOOL=ON -DHDF5_ENABLE_MAVEN_DEPLOY:BOOL=ON -DHDF5_ENABLE_JNI:BOOL=ON ../hdf5
>```

Or use the Maven-enabled CMake presets (recommended):

```bash
# Minimal build for Java artifacts only (recommended for Maven deployment)
# Linux (GCC) - JNI (default, Java 8+):
cmake --workflow --preset ci-MinShar-GNUC-Maven --fresh          # JNI Release
cmake --workflow --preset ci-MinShar-GNUC-Maven-Snapshot --fresh # JNI Snapshot

# Linux (GCC) - FFM (optional, Java 25+):
cmake --workflow --preset ci-MinShar-GNUC-Maven-FFM --fresh          # FFM Release
cmake --workflow --preset ci-MinShar-GNUC-Maven-FFM-Snapshot --fresh # FFM Snapshot

# Windows (MSVC) - JNI (default):
cmake --workflow --preset ci-MinShar-MSVC-Maven --fresh          # JNI Release
cmake --workflow --preset ci-MinShar-MSVC-Maven-Snapshot --fresh # JNI Snapshot

# Windows (MSVC) - FFM (optional):
cmake --workflow --preset ci-MinShar-MSVC-Maven-FFM --fresh          # FFM Release
cmake --workflow --preset ci-MinShar-MSVC-Maven-FFM-Snapshot --fresh # FFM Snapshot

# macOS (Clang) - JNI (default):
cmake --workflow --preset ci-MinShar-Clang-Maven --fresh          # JNI Release
cmake --workflow --preset ci-MinShar-Clang-Maven-Snapshot --fresh # JNI Snapshot

# macOS (Clang) - FFM (optional):
cmake --workflow --preset ci-MinShar-Clang-Maven-FFM --fresh         # FFM Release
cmake --workflow --preset ci-MinShar-Clang-Maven-FFM-Snapshot --fresh # FFM Snapshot

# ROS3 VFD (S3 cloud storage) - Add to any preset above:
cmake --workflow --preset ci-MinShar-GNUC-Maven --fresh -DHDF5_ENABLE_ROS3_VFD=ON
```

>**Note:** Presets are platform-specific. Use `cmake --list-presets` to see available presets for your current platform. Minimal Maven presets skip examples, testing, tools, C++, and Fortran builds to optimize for Java artifact generation only.

#### Java Examples Maven Integration

The HDF5 Java examples are available as a separate Maven artifact: `org.hdfgroup:hdf5-java-examples`. It contains platform-specific dependencies to ensure compatibility with the HDF5 Java library, and complete examples with documentation for all HDF5 Java functionality. See [HDF5Examples/JAVA/README-MAVEN.md](../HDF5Examples/JAVA/README-MAVEN.md) for usage instructions.

#### Testing Java Examples with Maven Artifacts

- Maven staging workflow validates examples against artifacts.
- Cross-platform testing ensures compatibility on all supported platforms.
- Native library error handling validates JAR structure in Maven-only environments.
- Fork-based testing allows validation on repository forks before canonical deployment.
- Dynamic repository workflows adapt to any GitHub repository automatically.

---

<a id="section-viii"></a>
## VIII. User Defined Options for HDF5 Libraries with CMake

Support for User Defined macros and options has been added. The file `UserMacros.cmake` has an example of the technique.
Replace the template code with your macro in the `UserMacros.cmake` file. Then enable the option to the CMake
configuration, build and test process.

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

Debug symbols are enabled with configuration selections `Debug` or `RelWithDebInfo`.
The difference between `Debug` and `RelWithDebInfo` configurations is that
`RelWithDebInfo` optimizes the code similar to Release. It produces fully optimized
code, but also creates the symbol table and the debug metadata to give the
debugger input to map the execution back to the original code.
`RelwithDebInfo` configuration should not affect the performance when the code
is run without a debugger attached.

The `HDF5_ENABLE_COVERAGE` option will add `-g -O0 -fprofile-arcs -ftest-coverage`
to `CMAKE_C_FLAGS`.

---

<a id="section-x"></a>
## X. Considerations for Cross-Compiling

Cross-compiling has several consequences for CMake:

* CMake cannot automatically detect the target platform.
* CMake cannot find libraries and headers in the default system directories.
* Executables built during cross compiling cannot be executed.

Cross-compiling support means that CMake separates information about the
build platform and target platform and gives the user mechanisms to solve
cross-compiling issues without additional requirements such as running
virtual machines, etc.

CMake uses a toolchain of utilities to compile, link libraries, create
archives, and other tasks to drive the build. The toolchain utilities
available are determined by the languages enabled.

CMake stores info about the current toolchain in the variables `CMAKE_C_COMPILER`, `CMAKE_CXX_COMPILER`.
They contain paths to the C and C++ compilers respectively. This is usually
enough on desktop platforms. In the case of embedded systems, a custom
linker and assembler setting may be needed. In more complex projects
you may need to additionally specify binaries to other parts of the toolchain
(size, ranlib, objcopy…). All these tools should be set in the corresponding
variables: `CMAKE_AR`, `CMAKE_ASM_COMPILER`, `CMAKE_LINKER`, `CMAKE_OBJCOPY`, and `CMAKE_RANLIB`.

As for the host and target operating systems, CMake stores their names in the
following variables:

* `CMAKE_HOST_SYSTEM_NAME` – name of the platform, on which CMake is running (host platform). On major operating systems this is set to the `Linux`, `Windows` or `Darwin` (macOS) value.

* `CMAKE_SYSTEM_NAME` – name of the platform, for which we are building (target platform). By default, this value is the same as `CMAKE_HOST_SYSTEM_NAME`, which means that we are building for the local platform (no cross-compilation).

Put the toolchain variables into a separate file (e.g. `<toolchain_name>.cmake`) and set `CMAKE_TOOLCHAIN_FILE` variable
to the path of that file. If `cmake` is invoked with the command line parameter `--toolchain path/to/file` or
`-DCMAKE_TOOLCHAIN_FILE=path/to/file` the file will be loaded early to set values for the compilers. The
`CMAKE_CROSSCOMPILING` variable is set to true when CMake is cross-compiling.

### Structure of the toolchain file

In fact, the toolchain file doesn’t have any structure. You can put anything you
want there. But the best practice is to define at least these settings:
path to the toolchain binaries (C compiler, C++ compiler, linker, etc.),
name of the target platform (and optionally target processor architecture),
required compilation and linking flags on that particular platform, and
toolchain sysroot settings.

It is recommended that you set the `CMAKE_FIND_ROOT_PATH` variable to a path where
you have an exact copy of the root filesystem you have on your target device (with
libraries and binaries pre-compiled for the target processor).

References:

https://cmake.org/cmake/help/latest/manual/cmake-toolchains.7.html<br/>
https://gitlab.com/embeddedlinux/libs/platform<br/>
https://discourse.cmake.org/t/cross-compile-for-aarch64-on-ubuntu/2161/10<br/>
https://stackoverflow.com/questions/54539682/how-to-set-up-cmake-to-cross-compile-with-clang-for-arm-embedded-on-windows?rq=1<br/>
https://developer.android.com/ndk/guides/cmake<br/>


---

<a id="section-xi"></a>
## XI. Creating Custom Preset Configurations

The quickest way to customize your build is to create a `CMakeUserPresets.json` file in the HDF5 source directory.

**Basic Customization Steps:**
1. Copy `CMakePresets.json` to `CMakeUserPresets.json`.
2. Edit `CMakeUserPresets.json`. Change configuration names from `ci-*` to `my-*` and modify the fields as needed.

### Example: Using Local Support Files

To change external support files to use a local directory:

```json
{
  "name": "my-base-tgz",
  "hidden": true,
  "inherits": "ci-base",
  "cacheVariables": {
    "HDF5_ALLOW_EXTERNAL_SUPPORT": {"type": "STRING", "value": "TGZ"},
    "TGZPATH": {"type": "PATH", "value": "${sourceParentDir}/temp"}
  }
},
{
  "name": "my-StdCompression",
  "hidden": true,
  "inherits": "my-base-tgz",
  "cacheVariables": {
    ...
  }
},
{
  "name": "my-StdShar",
  "hidden": true,
  "inherits": "my-StdCompression",
  "cacheVariables": {
    ...
  }
},
{
  "name": "my-StdShar-GNUC",
  "description": "My Custom GNUC Standard Config for x64 (Release)",
  "inherits": [
    "ci-x64-Release-GNUC",
    "ci-CPP",
    "ci-Fortran",
    "ci-Java",
    "my-StdShar",
    "my-StdExamples"
  ]
}
```

Then you can change or add options for your specific case.

### Example: Maven Deployment Preset

For Maven deployment with custom repository URL:

```json
{
  "name": "my-maven-custom",
  "inherits": "ci-MinShar-GNUC-Maven-Snapshot",
  "cacheVariables": {
    "MAVEN_REPOSITORY_URL": {
      "type": "STRING",
      "value": "https://your-repo.com/maven"
    },
    "HDF5_ENABLE_ROS3_VFD": {
      "type": "BOOL",
      "value": "ON"
    }
  }
}
```

Build with:
```bash
cmake --workflow --preset my-maven-custom --fresh
```

> **Note:** This example uses JNI (default). For FFM, inherit from `ci-MinShar-GNUC-Maven-FFM-Snapshot`.

### Preset File Details

`CMakePresets.json` and `CMakeUserPresets.json`:

* Live in the project's root directory.
* Both have the same format.
* `CMakePresets.json` is for project-wide build details (**don't modify**).
* `CMakeUserPresets.json` is for local build customizations (modify to your heart's content).

The HDF Group presets require CMake 3.26 and use the Ninja build system.
Ninja may need to be installed separately on some platforms.

Hidden presets (marked `"hidden": true`) are used for inheritance and
cannot be used directly. They are defined in `config/cmake-presets/hidden-presets.json`.

For more information:
    https://cmake.org/cmake/help/latest/manual/cmake-presets.7.html

---

<a id="section-xii"></a>
## XII. Using the Library

For information on using HDF5 see the documentation, tutorials and examples
found at https://support.hdfgroup.org/documentation/hdf5/latest/.

A summary of the features included in the built HDF5 installation can be found
in the `libhdf5.settings` file in the same directory as the static and/or
shared HDF5 libraries. However CMake provides a programmable method to
determine the features of the library. The CMake installation will
provide a CMake package configuration file, located in the installation folder,
`cmake/hdf5-config.cmake`, and can be used to determine the features of the library.
The file is accessed by using the `find_package` command in your `CMakeLists.txt` file.

Set the `HDF5_ROOT` CMake variable `-DHDF5_ROOT=<install_path>`
or environment variable, `set(ENV{HDF5_ROOT} "<install_path>")`
to the installed location of HDF5. Examples:

* On Windows: `HDF5_ROOT=C:/Program Files/HDF_Group/HDF5/z.y.x/`
* On UNIX and alike: `HDF5_ROOT=<install root folder>/HDF_Group/HDF5/z.y.x/`

If you are using shared libraries, you may need to add to the path
environment variable. Set the path environment variable to the
installed location of the library files for HDF5. Examples:

* On Windows (*.dll): `PATH=%PATH%;C:/Program Files/HDF_Group/HDF5/z.y.x/bin`
* On UNIX and alike (*.so): `LD_LIBRARY_PATH=$LD_LIBRARY_PATH:<install root folder>/HDF_Group/HDF5/z.y.x/lib`

(Note there are no quote characters used on Windows and all platforms
use forward slashes)

Add the following to your `CMakeLists.txt` file:
```cmake
find_package (HDF5 NAMES hdf5 COMPONENTS C shared)
```

The components are optional and can be omitted if not needed. The
components are: `shared`, `static`, `C`, `CXX`, `Fortran`, `HL`, `Java`, `Tools`, and `VOL`.

---

<a id="section-xiii"></a>
## XIII. Using CMake Regex Options for Testing

CMake provides a way to use regular expressions to control which tests are
executed. The HDF5 CMake build system provides labels for each test that
can be used to select tests. The labels are defined in the `CMakeTests.cmake`
files.

Some of the labels are:

PARALLEL
- MPI_TEST for parallel tests.
- MPI_TEST_FORT for just parallel Fortran tests.

SERIAL
- CPP for C++ tests.
- HL_CPP for high-level C++ tests.
- FORTRAN for Fortran tests.
- HL_FORTRAN for high-level Fortran tests.
- HL for high-level tests.
- JUnit for Java tests.
- H5WATCH for tests that use the h5watch SWMR program.
- SWMR for tests that use the SWMR feature.
- h5_api for the API tests.
- VOL for tests that use the VOL feature.
- VFD for tests that use the VFD feature.
- H5TEST for the library tests.
- EX for examples
- H5CLEAR for the h5clear tool tests.
- H5COPY for the h5copy tool tests.
- H5DIFF for the h5diff tool tests.
- H5DUMP for the h5dump tool tests.
- H5FC for the h5fc tool tests.
- H5IMPORT for the h5import tool tests.
- H5JAM for the h5jam tool tests.
- H5LS for the h5ls tool tests.
- H5MKGRP for the h5mkgrp tool tests.
- H5REPACK for the h5repack tool tests.
- H5REPART for the h5repart tool tests.
- H5STAT for the h5stat tool tests.
- PERFORM for performance tests.

To run tests with a specific label, use the `--tests-regex` (or `-R`) option with `ctest`. For example, to run only the C++ tests, use:
```bash
ctest . --tests-regex "CPP"
```

To run tests with multiple labels, use the `|` operator to separate the labels.
To run tests with the `MPI_TEST` and `FORTRAN` labels, use:
```bash
ctest . --tests-regex "MPI_TEST|FORTRAN"
```

To exclude tests with a specific label, use the `--exclude-regex` (or `-E`) option with `ctest`.

For more information on using regular expressions with `ctest`,
see the CMake documentation:
https://cmake.org/cmake/help/latest/manual/ctest.1.html#regular-expressions.

---

<a id="section-xiv"></a>
## XIV. Java FFM Testing

HDF5 2.0 includes comprehensive Foreign Function & Memory (FFM) API tests for Java 25+.

### FFM Test Organization

Tests are organized by HDF5 module in `java/jtest/`:

Module Test Files:
  - TestH5ffm.java     - General library operations
                         H5open, H5close, memory management, version info
  - TestH5Affm.java    - Attribute operations
  - TestH5Dffm.java    - Dataset operations
  - TestH5Effm.java    - Error handling
  - TestH5Fffm.java    - File operations
  - TestH5FDffm.java   - File drivers
  - TestH5Gffm.java    - Group operations
  - TestH5Iffm.java    - Identifier management
  - TestH5Lffm.java    - Link operations
  - TestH5Offm.java    - Object operations
  - TestH5Pffm.java    - Property lists
  - TestH5PLffm.java   - Plugin management
  - TestH5Rffm.java    - References
  - TestH5Sffm.java    - Dataspace operations
  - TestH5Tffm.java    - Datatype operations
  - TestH5VLffm.java   - VOL connector
  - TestH5Zffm.java    - Filter operations

### Running FFM Tests

To run all FFM tests:
```bash
ctest -R "JUnitFFM" -V
```

To run specific module tests:
```bash
ctest -R "JUnit-TestH5Affm" -V    # Attributes
ctest -R "JUnit-TestH5Pffm" -V    # Properties
ctest -R "JUnit-TestH5Tffm" -V    # Datatypes
ctest -R "JUnit-TestH5Sffm" -V    # Dataspaces
```

Test Requirements:

- Java 25+ with `--enable-native-access=ALL-UNNAMED`
- FFM bindings JAR (`javahdf5-*.jar`)
- JUnit 4.x test framework
- HDF5 native libraries in `LD_LIBRARY_PATH`

FFM Test Patterns:
- All tests use `Arena.ofConfined()` for memory management
- Proper cleanup in try-with-resources blocks
- Return value checking with `isSuccess()` / `isValidId()`
- See `java/jtest/FfmTestSupport.java` for utility methods

ROS3 VFD (S3 Object Storage):

- Tests are compatible with both plain and ROS3-enabled builds
- Use `-DHDF5_ENABLE_ROS3_VFD=ON` with any Maven CMake preset for ROS3 builds
- Example: `cmake --preset ci-MinShar-GNUC-Maven -DHDF5_ENABLE_ROS3_VFD=ON`
- Same test suite validates both feature sets

---
*For further assistance, send email to help@hdfgroup.org*
