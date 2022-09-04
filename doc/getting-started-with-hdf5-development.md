# Getting Started with HDF5 Development

## A brief tour of the source code

Here's a quick guide to where you can find things in our source tree. Some of these directories have README.md files of their own.

`bin/`
Scripts we use for building the software and misc. tools.

`c++/`
Source, tests, and examples for the C++ language wrapper.

`config/`
Configuration files for both the Autotools and CMake.

`doc/`
Miscellaneous documents, mostly in markdown format.

`doxygen/`
Mainly Doxygen build files and other top-level Doxygen things. The Doxygen content is spread across the library's header files but some content can be found here when it has no other obvious home.

`examples/`
C library examples. Fortran and C++ examples are located in their corresponding wrapper directory.

`fortran/`
Source, tests, and examples for the Fortran language wrapper.

`hl/`
Source, tests, and examples for the high-level library.

`java/`
Source, tests, and examples for the JNI language wrapper and the corresponding OO Java library.

`m4/`
m4 build scripts used by the Autotools. CMake ignores these.

`release_docs/`
Install instructions and release notes.

`src/`
Source code for the C library.

`test/`
C library test code. Described in much more detail below.

`testpar/`
Parallel C library test code. Described in much more detail below.

`tools/`
HDF5 command-line tools code, associated tools tests, and the input test files.

`utils/`
Small utility programs that don't belong anywhere else.



