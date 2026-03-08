# HDF5 Build and Install Instructions for Windows

We recommend that users build, test and install HDF5 using CMake.

Instructions for building and testing HDF5 using CMake can be found in the
[INSTALL_CMake.md](./INSTALL_CMake.md) file found in this folder.

For instructions on building and testing an application with HDF5, see
[USING_HDF5_CMake.md](./USING_HDF5_CMake.md) file found in this folder.

Users who want to build and run an application with HDF5 in Visual Studio
without using CMake should consult the [USING_HDF5_VS.md](./USING_HDF5_VS.md) file. Building
applications with the dynamic/shared hdf5 libraries requires that the
`H5_BUILT_AS_DYNAMIC_LIB` compile definition be used.
