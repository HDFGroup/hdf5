#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the LICENSE file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#
# This is the CMakeCache file.

#########################
# EXTERNAL cache entries
#########################

set (CMAKE_INSTALL_FRAMEWORK_PREFIX "Library/Frameworks" CACHE STRING "Frameworks installation directory" FORCE)

set (HDF_PACKAGE_EXT "" CACHE STRING "Name of HDF package extension" FORCE)

set (HDF_PACKAGE_NAMESPACE "hdf5::" CACHE STRING "Name for HDF package namespace (can be empty)" FORCE)

set (HDF5_BUILD_CPP_LIB ON CACHE BOOL "Build C++ support" FORCE)

set (HDF5_BUILD_FORTRAN ON CACHE BOOL "Build FORTRAN support" FORCE)

set (HDF5_BUILD_JAVA ON CACHE BOOL "Build JAVA support" FORCE)
set (HDF5_ENABLE_JNI ON CACHE BOOL "Build JNI support" FORCE)

set (HDF5_INSTALL_MOD_FORTRAN "NO" CACHE STRING "Copy FORTRAN mod files to include directory (NO SHARED STATIC)" FORCE)
set_property (CACHE HDF5_INSTALL_MOD_FORTRAN PROPERTY STRINGS NO SHARED STATIC)

set (MPIEXEC_MAX_NUMPROCS "4" CACHE STRING "Minimum number of processes for HDF parallel tests" FORCE)

set (HDF5_ENABLE_ALL_WARNINGS ON CACHE BOOL "Enable all warnings" FORCE)

set (HDF_TEST_EXPRESS "2" CACHE STRING "Control testing framework (0-3)" FORCE)

set (HDF5_MINGW_STATIC_GCC_LIBS ON CACHE BOOL "Statically link libgcc/libstdc++" FORCE)

#set the default debug suffix for all library targets
if (NOT CMAKE_DEBUG_POSTFIX)
  if (WIN32)
    set (CMAKE_DEBUG_POSTFIX "_D")
  else ()
    set (CMAKE_DEBUG_POSTFIX "_debug")
  endif ()
endif ()

set (HDF5_ALLOW_EXTERNAL_SUPPORT "TGZ" CACHE STRING "Allow External Library Building (NO GIT TGZ)" FORCE)
set_property (CACHE HDF5_ALLOW_EXTERNAL_SUPPORT PROPERTY STRINGS NO GIT TGZ)

########################
# compression options
########################
set (HDF5_USE_ZLIB_STATIC ON CACHE BOOL "Use static zlib library" FORCE)
set (HDF5_USE_LIBAEC_STATIC ON CACHE BOOL "Use static AEC library" FORCE)

# use local content settings
set (ZLIB_USE_LOCALCONTENT ON CACHE BOOL "Use local file for ZLIB FetchContent" FORCE)
set (LIBAEC_USE_LOCALCONTENT ON CACHE BOOL "Use local file for LIBAEC FetchContent" FORCE)
set (PLUGIN_USE_LOCALCONTENT ON CACHE BOOL "Use local file for PLUGIN FetchContent" FORCE)
set (KWSYS_USE_LOCALCONTENT OFF CACHE BOOL "Use local file for KWSYS FetchContent" FORCE)

