# The `config` directory

## Intro

HDF5 can be configured using CMake.

Configuration information for the HDF5 library and tools is
specific to the repository folders. Each subdirectory of the project
has its own CMake build and test files. Basic library configuration will generally
be found in the root's `CMakeLists.txt` with support for macros and settings
in this config directory.


This directory contains a few important things:

* Support files for optional components (in `cmake`)
* Compiler and platform parameters (in `flags`)
* Warning files (in `*-warnings` directories)
* Toolchain files (in `toolchain`)
* Sanitizer files (in `sanitizer`)
* Example install scripts (in `examples`)
* Installation support files (in `install`)

CMake is documented in the following in the root's release_docs folder files:

* INSTALL
* INSTALL_CMake.txt
* USING_HDF5_CMake.txt
* USING_HDF5_VS.txt
* INSTALL_Windows.txt
* USING_CMake_Examples.txt
