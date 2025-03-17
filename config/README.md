# The `config` directory

## Intro

HDF5 can be configured using CMake.

Configuration information for the HDF5 library and tools is (unfortunately)
spread across the repository. Basic library configuration will generally
be found in the root's `CMakeLists.txt`.
Each subdirectory of the project also has its own CMake build
and test files.

This directory contains a few important things:

* CMake support files (in `cmake`)
* Warning files (in `*-warnings` directories)
* CMake toolchain files (in `toolchain`)
* CMake sanitizer files (in `sanitizer`)

CMake is documented in the following files:

* INSTALL
* INSTALL_CMake.txt
* USING_HDF5_CMake.txt
* USING_HDF5_VS.txt
* INSTALL_Windows.txt
* USING_CMakae_Examples.txt
