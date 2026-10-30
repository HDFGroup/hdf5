# Reproducible Builds

A build is reproducible when building the same HDF5 source twice produces
byte-identical installed files, even if the builds run in different
directories, on different hosts, or at different times. Linux distributions
check this for the packages they ship (see https://reproducible-builds.org).

HDF5 records information about how it was built in `libhdf5.settings`, and
compiles the same information into the library (`H5build_settings.c`, shown
by `h5cc -showconfig`). Anything recorded there that changes between builds
makes the library itself differ, so a reproducible build needs that
information to be the same in every build.

## What HDF5 handles

* **Configuration date.** "Configured on" uses `SOURCE_DATE_EPOCH` when it is
  set, and is always formatted in UTC.
* **Recorded compiler and linker flags.** The flags recorded in the settings
  leave out `-ffile-prefix-map=`, `-fdebug-prefix-map=`, `-fmacro-prefix-map=`,
  `-fprofile-prefix-map=`, `-fcoverage-prefix-map=` and `-fprofile-prefix-path=`
  options, which contain build paths. The flags
  used to compile are not changed.
* **Build-tree RPATH.** `CMAKE_BUILD_RPATH_USE_ORIGIN` defaults to `ON`, so
  libraries and tools are linked in the build tree with a relative RPATH.
  An absolute build-tree RPATH would end up in the linker build ID of the
  installed files, even though CMake replaces the RPATH itself at install
  time.
* **Fortran module directory.** "Module Directory" reports the installed
  directory, not a path in the build tree.

## What the builder sets

* **`SOURCE_DATE_EPOCH`** in the environment, as a fixed time (seconds since
  the epoch). Distribution build tools set it automatically.
* **`HDF5_BUILD_HOST_SYSTEM`** to a fixed value, for example
  `-DHDF5_BUILD_HOST_SYSTEM=Linux`. It is recorded as "Host system" and
  defaults to `CMAKE_HOST_SYSTEM`, which includes the build host's kernel
  release.
* **Prefix maps** for the source and build directories in the C, C++ and
  Fortran flags, so that `__FILE__` strings and debug information do not
  contain build paths:

  ```
  -ffile-prefix-map=<source dir>=/usr/src/hdf5 -ffile-prefix-map=<build dir>=/usr/src/hdf5/build
  ```

* **Deterministic static archives.** Use an `ar` that runs in deterministic
  mode, which most Linux distributions configure by default. Otherwise, pass
  the `D` modifier, for example
  `-DCMAKE_C_ARCHIVE_CREATE="<CMAKE_AR> qcD <TARGET> <LINK_FLAGS> <OBJECTS>"`
  (and the corresponding `CMAKE_<LANG>_ARCHIVE_APPEND` and
  `CMAKE_<LANG>_ARCHIVE_FINISH` settings with `ranlib -D`).

## Example

```
export SOURCE_DATE_EPOCH=$(git log -1 --format=%ct)
map="-ffile-prefix-map=$PWD/hdf5=/usr/src/hdf5 -ffile-prefix-map=$PWD/build=/usr/src/hdf5/build"
cmake -G Ninja -S hdf5 -B build \
    -DCMAKE_INSTALL_PREFIX=/usr \
    -DCMAKE_C_FLAGS="$map" -DCMAKE_CXX_FLAGS="$map" -DCMAKE_Fortran_FLAGS="$map" \
    -DHDF5_BUILD_HOST_SYSTEM=Linux
cmake --build build
DESTDIR=$PWD/install cmake --install build
```

## Verifying

To check a configuration, build and install it twice from different source
and build directories, ideally with a different time zone, locale and number
of parallel jobs, and compare the two install trees, for example with
`diff -r` or `diffoscope`. HDF5's CI does this on Linux with GCC for the C,
C++, Fortran, high-level and tools builds, shared and static. Java, Windows
and macOS builds are not checked yet.
