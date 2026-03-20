# HDF5 Build and Install Instructions for Cygwin

This document is an instruction on how to build, test and install HDF5 library on Cygwin.

> **NOTE:** HDF5 2.0 and later requires CMake for building. See
> [INSTALL_CMake.md](./INSTALL_CMake.md)` for complete build instructions using
> CMake Presets (recommended method).

---

## Table of Contents

* [Preconditions](#preconditions)
* [Build, Test and Install HDF5 on Cygwin](#build-test-install)

---

<a id="preconditions"></a>
## Preconditions

### 1. Cygwin 3.5.1 or higher Installed



To install the Cygwin net release, go to [http://www.cygwin.com](http://www.cygwin.com) and click on `setup-x86_64.exe` under the heading "Current Cygwin DLL version". This will download a GUI installer called `setup-x86_64.exe` which can be run to download a complete Cygwin installation via the internet. Then follow the instructions on each screen to install Cygwin.

Cygwin uses packages to manage installing various software. Users can choose to install or uninstall certain packages by running `setup.exe`. [http://www.cygwin.com/packages/](http://www.cygwin.com/packages/) provides detailed information about Cygwin packages.

Most required dependencies can be satisfied by installing all packages in the **"Devel"** category. However, please verify that you have installed all packages listed below.

### 2. Compilers, Libraries and Utilities Installed

#### 2.1 Compilers Supported

The following compilers are supported by HDF5 and included in the Cygwin package system:
* `gcc`, which includes:
  * `gcc-core` : C compiler
  * `gcc-g++` : C++ compiler
  * `gcc-fortran` : Fortran compiler

##### 2.1.1 Using Compilers Not Supported

By default, the current configuration uses vendor compilers; to use another compiler run the following commands before running configure:

```bash
setenv CC "foo -flags"
setenv FC "fffoo -flags"
```

For example, if users want to use `pgf90` as a fortran compiler, then:

```bash
setenv FC pgf90
```

> **Note:** See the configure help page (`configure --help`) for a list of environment variables that have an effect on building the library.

#### 2.2 HDF5 External Library Dependencies

* **2.2.1 zlib:** `zlib-1.2.8` or later is supported and tested.
* **2.2.2 Szip:** `libaec-1.1.2` or later is supported and tested.

#### 2.3 Additional Utilities

The following standard utilities are also required to build and test HDF5:
* `bison` : yacc implementation
* `flex` : flex utility
* `make` : make utility

#### 2.4 Alternate Build Process

Download the CMake package and follow the notes in the [INSTALL_CMake.md](./INSTALL_CMake.md) file to build HDF5 with the CMake utilities.

---

<a id="build-test-install"></a>
## Build, Test and Install HDF5 on Cygwin

### 1. Get HDF5 source code package
Users can download the HDF5 source code from the official GitHub repository:
[https://github.com/HDFGroup/hdf5](https://github.com/HDFGroup/hdf5).

### 2. Unpacking the distribution
The HDF5 source code is distributed in a variety of formats which can be unpacked with the following commands, each of which creates an `hdf5-2.0.x` directory.

**2.1 Non-compressed tar archive (*.tar)**
```bash
tar xf hdf5-2.0.x.tar
```

**2.2 Gzip'd tar archive (*.tar.gz)**
```bash
gunzip < hdf5-2.0.x.tar.gz | tar xf -
```

**2.3 Bzip'd tar archive (*.tar.bz2)**
```bash
bunzip2 < hdf5-2.0.x.tar.bz2 | tar xf -
```

### 3. Setup Environment
In Cygwin, most compilers and settings are automatically detected during the configure script. However, if you are building Fortran, we recommend that you explicitly set the `FC` variable in your environment to use the `gfortran` compiler. For example, issue the command:

```bash
export FC=gfortran
```

### 4. Follow build and test steps
Follow build and test steps in the [INSTALL_CMake.md](./INSTALL_CMake.md) file.

### 5. Check installed HDF5 library
After installation, go to your installation directory. There should be three subdirectories: `bin`, `include`, and `lib`.

### 6. Known Problems
* `cache_api` tests may fail. This is a known issue with Cygwin.
* `make check` fails when building shared lib files is enabled. The default on Cygwin has been changed to disable shared libraries. It can be enabled with the `BUILD_SHARED_LIBS` configure option but is likely to fail `make check` with GCC compilers.

---

**For further assistance:**
* **HDF Forum:** [https://forum.hdfgroup.org/](https://forum.hdfgroup.org/)
* **HDF Helpdesk:** [https://help.hdfgroup.org/](https://help.hdfgroup.org/)
