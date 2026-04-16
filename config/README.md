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

CMake is documented in the following files in the `docs/` directory:

* [INSTALL.md](../docs/INSTALL.md)
* [INSTALL_CMake.md](../docs/INSTALL_CMake.md)
* [USING_HDF5_CMake.md](../docs/USING_HDF5_CMake.md)
* [USING_HDF5_VS.md](../docs/USING_HDF5_VS.md)
* [INSTALL_Windows.md](../docs/INSTALL_Windows.md)
* [USING_CMake_Examples.md](../docs/USING_CMake_Examples.md)
