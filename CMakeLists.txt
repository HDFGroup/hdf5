# -----------------------------------------------------------------------------
# HDF5 CMake Top-Level Build Configuration
# -----------------------------------------------------------------------------
# This is the main CMakeLists.txt file for configuring, building, and installing
# the HDF5 library and its components. It supports building HDF5 as a standalone
# project or as a subproject within a larger CMake build. Key features include:
#
# - Enforces out-of-source builds for safety and cleanliness.
# - Supports configuration as a subproject (HDF5_EXTERNALLY_CONFIGURED).
# - Provides options for building shared/static libraries, tools, examples, and bindings (C++, Fortran, Java).
# - Handles versioning, installation paths, and library naming customization.
# - Integrates with external dependencies (zlib, szip, MPI, HDFS, etc.).
# - Supports advanced features: thread-safety, concurrency, plugins, code coverage, and more.
# - Includes options for documentation and testing.
#
# Usage examples:
#   1. Presets:
#      cd <path-to-hdf5-source>
#      cmake --workflow --preset <preset-name>
#
#   2. Command line:
#      mkdir build && cd build
#      cmake [options] <path-to-hdf5-source>
#      cmake --build .
#      cmake --install .
#
# See comments throughout this file for details on configuration variables and options.
# -----------------------------------------------------------------------------

cmake_minimum_required (VERSION 3.26)
project (HDF5 C)

if (POLICY CMP0074)
  # find_package() uses <PackageName>_ROOT variables.
  cmake_policy (SET CMP0074 NEW)
endif ()

if (POLICY CMP0144)
  # <PACKAGENAME> is the upper-cased package name.
  cmake_policy (SET CMP0144 NEW)
endif ()

if (POLICY CMP0083)
  # To control generation of Position Independent Executable (PIE) or not,
  # some flags are required at link time.
  cmake_policy (SET CMP0083 NEW)
endif ()

if (POLICY CMP0127)
  # to evaluate each condition as if(<condition>), where <condition> is re-parsed
  # as if literally written in a call to if().
  cmake_policy (SET CMP0127 NEW)
endif ()

# Avoid warning about DOWNLOAD_EXTRACT_TIMESTAMP in CMake 3.24:
if (CMAKE_VERSION VERSION_GREATER_EQUAL "3.24.0")
    cmake_policy(SET CMP0135 NEW)
endif()

#-----------------------------------------------------------------------------
# Instructions for use : Normal Build
#
# For standard build of HDF5 libraries,tests and tools.
# Run cmake using the HDF5 source tree to generate a build tree.
# Enable/Disable options according to requirements and
# set CMAKE_INSTALL_PREFIX to the required install path.
# Make install can be used to install all components for system-wide use.
#
if (CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_CURRENT_BINARY_DIR)
    message (FATAL_ERROR "\nERROR! ${PROJECT_NAME} DOES NOT SUPPORT IN SOURCE BUILDS!\n"
      "CMAKE_CURRENT_SOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR}"
      " == CMAKE_CURRENT_BINARY_DIR=${CMAKE_CURRENT_BINARY_DIR}\n"
      "NEXT STEPS:\n"
      "(1) Delete the CMakeCache.txt file and the CMakeFiles/ directory\n"
      "     under the source directory for ${PROJECT_NAME}, otherwise you\n"
      "     will not be able to configure ${PROJECT_NAME} correctly!\n"
      "      * For example, on linux machines do:\n"
      "        $ rm -r CMakeCache.txt CMakeFiles/\n"
      "(2) Create a different directory and configure ${PROJECT_NAME} in that directory.\n"
      "      * For example, on linux machines do:\n"
      "        $ mkdir MY_BUILD\n"
      "        $ cd MY_BUILD\n"
      "        $ cmake [OPTIONS] ..\n"
      )
endif ()

# Whether the most recently called project() command, in the current scope or above,
# was in the top level CMakeLists.txt file.
if (CMAKE_VERSION VERSION_GREATER_EQUAL "3.21.0")
  if(NOT PROJECT_IS_TOP_LEVEL)
    set (HDF5_EXTERNALLY_CONFIGURED 1)
  endif()
else()
  if (NOT CMAKE_SOURCE_DIR STREQUAL PROJECT_SOURCE_DIR)
    set (HDF5_EXTERNALLY_CONFIGURED 1)
  endif()
endif()

#-----------------------------------------------------------------------------
# Enable the use of the cmake_dependent_option() function.
include (CMakeDependentOption)

#-----------------------------------------------------------------------------
# Instructions for use : Sub-Project Build
#
# To include HDF5 as a sub-project within another project.
# Set HDF5_EXTERNALLY_CONFIGURED to 1 in the parent project and
# supply values for the following variables...
#
# HDF5_EXPORTED_TARGETS :
#   Set this to the name of the targets variable which controls exports
#   If unset (because parent project does not support/use the
#   install (EXPORT target...) syntax), then targets are not configured
#   for export during install.
#
# HDF5_LIB_DEPENDENCIES :
#   If the build of HDF5 libs is being customized, then rules for the
#   dependencies of the HDF5 libs may be 'incomplete', add additional
#   dependencies to this variable so that external projects pick them up
#
# Put all global top-level build options into one place at the beginning
#-----------------------------------------------------------------------------
include (CMakeBuildOptions.cmake)

#option (HDF5_EXTERNAL_LIB_PREFIX "Use prefix for custom library naming." "")
set (HDF5_EXTERNAL_LIB_PREFIX "" CACHE STRING "Use prefix for custom library naming.")
mark_as_advanced (HDF5_EXTERNAL_LIB_PREFIX)
# HDF5_EXTERNAL_LIB_PREFIX :
#   If the parent project needs to install hdf libraries, but avoid
#   name conflicts with system versions, then a prefix may be added
#   to ensure that the correct versions configured are used.
set (HDF5_LIB_INFIX "" CACHE STRING "Use infix for custom library naming.")
mark_as_advanced (HDF5_LIB_INFIX)
# HDF5_LIB_INFIX :
#   This infix is added to all library names after 'hdf5'.
#   e.g. the infix '_openmpi' results in the library name 'libhdf5_openmpi.so'
#   This name is used in packages on debian based systems.
#   (see https://packages.debian.org/jessie/amd64/libhdf5-openmpi-8/filelist)
#option (HDF5_EXTERNAL_LIB_SUFFIX "Use prefix for custom library naming." "")
set (HDF5_EXTERNAL_LIB_SUFFIX "" CACHE STRING "Use suffix for custom library naming.")
mark_as_advanced (HDF5_EXTERNAL_LIB_SUFFIX)
# HDF5_EXTERNAL_LIB_SUFFIX :
#   If the parent project needs to install hdf libraries, but avoid
#   name conflicts with system versions, then a suffix may be added
#   to ensure that the correct versions configured are used.
#
# HDF5_INSTALL_BIN_DIR, HDF5_INSTALL_LIB_DIR, HDF5_INSTALL_INCLUDE_DIR, HDF5_INSTALL_DATA_DIR :
#   Customize the 'bin', 'lib', 'include', and 'share' installation directories.
#
# HDF5_INSTALL_NO_DEVELOPMENT :
#   Set to true to skip installation of headers and CMake package files.
#
# Consider this example from the ParaView project, it builds its own zlib
# library and tells HDF5 to add it as a dependency - this ensures that
# any project making use of this build of HDF5 will use the correct zlib
#
#   # Tell hdf5 that we are manually overriding certain settings
#   set (HDF5_EXTERNALLY_CONFIGURED 1)
#   # Avoid duplicating names of installed libraries
#   set (HDF5_EXTERNAL_LIB_PREFIX "vtk")
#   # Export configuration to this export variable
#   set (HDF5_EXPORTED_TARGETS "paraview-targets")
#
#   # Setup all necessary overrides for zlib so that HDF5 uses our
#   # internally compiled zlib rather than any other version
#   if (HDF5_ENABLE_ZLIB_SUPPORT)
#     # We must tell the main HDF5 library that it depends on our zlib
#     set (HDF5_LIB_DEPENDENCIES vtkzlib)
#     # Override the zlib header file
#     if (VTK_USE_SYSTEM_ZLIB)
#       set (H5_ZLIB_HEADER "zlib.h")
#     else ()
#       set (H5_ZLIB_HEADER "vtk_zlib.h")
#       # Set vars that FindZlib would have set if used in sub project
#       set (H5_ZLIB_INCLUDE_DIRS "${VTK_H5_ZLIB_INCLUDE_DIRS}")
#       set (H5_ZLIB_LIBRARIES vtkzlib)
#     endif ()
#   endif ()
#
#   # Add the sub project
#   add_subdirectory (Utilities/hdf5-1.8)
#-----------------------------------------------------------------------------
string (TIMESTAMP CONFIG_DATE "%Y-%m-%d")

#-----------------------------------------------------------------------------
# Allow Visual Studio solution directories
#-----------------------------------------------------------------------------
# Provide a way for Visual Studio Express users to turn OFF the new FOLDER
# organization feature. Default to ON for non-Express users. Express users must
# explicitly turn off this option to build HDF5 in the Express IDE...
#
if (HDF5_USE_FOLDERS)
  set_property (GLOBAL PROPERTY USE_FOLDERS ON)
endif ()

#-----------------------------------------------------------------------------
# Set the core names of all the libraries CORENAME is the base library name
# for targets, BASE_CORE
# filename are made of PREFIX_BASE_INFIX_CORE_SUFFIX
#-----------------------------------------------------------------------------
set (HDF5_LIB_BASE              "hdf5")

set (HDF5_LIB_CORE              "")
set (HDF5_TEST_LIB_CORE         "_test")
set (HDF5_TEST_PAR_LIB_CORE     "_testpar")
set (HDF5_CPP_LIB_CORE          "_cpp")
set (HDF5_HL_LIB_CORE           "_hl")
set (HDF5_HL_CPP_LIB_CORE       "_hl_cpp")
set (HDF5_TOOLS_LIB_CORE        "_tools")
set (HDF5_UTILS_LIB_CORE        "_utils")
set (HDF5_F90_LIB_CORE          "_fortran")
set (HDF5_F90_C_LIB_CORE        "_f90cstub")
set (HDF5_F90_TEST_LIB_CORE     "_test_fortran")
set (HDF5_F90_C_TEST_LIB_CORE   "_test_f90cstub")
set (HDF5_HL_F90_LIB_CORE       "_hl_fortran")
set (HDF5_HL_F90_C_LIB_CORE     "_hl_f90cstub")
set (HDF5_JAVA_JNI_LIB_CORE     "_java")

set (HDF5_LIB_CORENAME              "${HDF5_LIB_BASE}")
set (HDF5_TEST_LIB_CORENAME         "${HDF5_LIB_BASE}${HDF5_TEST_LIB_CORE}")
set (HDF5_TEST_PAR_LIB_CORENAME     "${HDF5_LIB_BASE}${HDF5_TEST_PAR_LIB_CORE}")
set (HDF5_CPP_LIB_CORENAME          "${HDF5_LIB_BASE}${HDF5_CPP_LIB_CORE}")
set (HDF5_HL_LIB_CORENAME           "${HDF5_LIB_BASE}${HDF5_HL_LIB_CORE}")
set (HDF5_HL_CPP_LIB_CORENAME       "${HDF5_LIB_BASE}${HDF5_HL_CPP_LIB_CORE}")
set (HDF5_TOOLS_LIB_CORENAME        "${HDF5_LIB_BASE}${HDF5_TOOLS_LIB_CORE}")
set (HDF5_UTILS_LIB_CORENAME        "${HDF5_LIB_BASE}${HDF5_UTILS_LIB_CORE}")
set (HDF5_F90_LIB_CORENAME          "${HDF5_LIB_BASE}${HDF5_F90_LIB_CORE}")
set (HDF5_F90_C_LIB_CORENAME        "${HDF5_LIB_BASE}${HDF5_F90_C_LIB_CORE}")
set (HDF5_F90_TEST_LIB_CORENAME     "${HDF5_LIB_BASE}${HDF5_F90_TEST_LIB_CORE}")
set (HDF5_F90_C_TEST_LIB_CORENAME   "${HDF5_LIB_BASE}${HDF5_F90_C_TEST_LIB_CORE}")
set (HDF5_HL_F90_LIB_CORENAME       "${HDF5_LIB_BASE}${HDF5_HL_F90_LIB_CORE}")
set (HDF5_HL_F90_C_LIB_CORENAME     "${HDF5_LIB_BASE}${HDF5_HL_F90_C_LIB_CORE}")
set (HDF5_JAVA_JNI_LIB_CORENAME     "${HDF5_LIB_BASE}${HDF5_JAVA_JNI_LIB_CORE}")
set (HDF5_JAVA_HDF5_LIB_CORENAME    "jarhdf5")
set (HDF5_JAVA_TEST_LIB_CORENAME    "jartest5")
set (HDF5_JAVA_JSRC_LIB_CORENAME    "javahdf5")
set (HDF5_JAVA_JTEST_LIB_CORENAME   "javatest5")

#-----------------------------------------------------------------------------
# Set the true names of all the libraries if customized by external project
#-----------------------------------------------------------------------------
set (HDF5_LIB_NAME              "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_LIB_BASE}${HDF5_LIB_INFIX}${HDF5_LIB_CORE}${HDF5_EXTERNAL_LIB_SUFFIX}")
set (HDF5_TEST_LIB_NAME         "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_LIB_BASE}${HDF5_LIB_INFIX}${HDF5_TEST_LIB_CORE}${HDF5_EXTERNAL_LIB_SUFFIX}")
set (HDF5_TEST_PAR_LIB_NAME     "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_LIB_BASE}${HDF5_LIB_INFIX}${HDF5_TEST_PAR_LIB_CORE}${HDF5_EXTERNAL_LIB_SUFFIX}")
set (HDF5_CPP_LIB_NAME          "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_LIB_BASE}${HDF5_LIB_INFIX}${HDF5_CPP_LIB_CORE}${HDF5_EXTERNAL_LIB_SUFFIX}")
set (HDF5_HL_LIB_NAME           "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_LIB_BASE}${HDF5_LIB_INFIX}${HDF5_HL_LIB_CORE}${HDF5_EXTERNAL_LIB_SUFFIX}")
set (HDF5_HL_CPP_LIB_NAME       "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_LIB_BASE}${HDF5_LIB_INFIX}${HDF5_HL_CPP_LIB_CORE}${HDF5_EXTERNAL_LIB_SUFFIX}")
set (HDF5_TOOLS_LIB_NAME        "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_LIB_BASE}${HDF5_LIB_INFIX}${HDF5_TOOLS_LIB_CORE}${HDF5_EXTERNAL_LIB_SUFFIX}")
set (HDF5_UTILS_LIB_NAME        "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_LIB_BASE}${HDF5_LIB_INFIX}${HDF5_UTILS_LIB_CORE}${HDF5_EXTERNAL_LIB_SUFFIX}")
set (HDF5_F90_LIB_NAME          "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_LIB_BASE}${HDF5_LIB_INFIX}${HDF5_F90_LIB_CORE}${HDF5_EXTERNAL_LIB_SUFFIX}")
set (HDF5_F90_C_LIB_NAME        "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_LIB_BASE}${HDF5_LIB_INFIX}${HDF5_F90_C_LIB_CORE}${HDF5_EXTERNAL_LIB_SUFFIX}")
set (HDF5_F90_TEST_LIB_NAME     "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_LIB_BASE}${HDF5_LIB_INFIX}${HDF5_F90_TEST_LIB_CORE}${HDF5_EXTERNAL_LIB_SUFFIX}")
set (HDF5_F90_C_TEST_LIB_NAME   "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_LIB_BASE}${HDF5_LIB_INFIX}${HDF5_F90_C_TEST_LIB_CORE}${HDF5_EXTERNAL_LIB_SUFFIX}")
set (HDF5_HL_F90_LIB_NAME       "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_LIB_BASE}${HDF5_LIB_INFIX}${HDF5_HL_F90_LIB_CORE}${HDF5_EXTERNAL_LIB_SUFFIX}")
set (HDF5_HL_F90_C_LIB_NAME     "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_LIB_BASE}${HDF5_LIB_INFIX}${HDF5_HL_F90_C_LIB_CORE}${HDF5_EXTERNAL_LIB_SUFFIX}")
#Because the loading mechanism of the JNI library requires a fixed name, the name of the JNI library cannot be customized
#set (HDF5_JAVA_JNI_LIB_NAME     "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_LIB_BASE}${HDF5_LIB_INFIX}${HDF5_JAVA_JNI_LIB_CORE}${HDF5_EXTERNAL_LIB_SUFFIX}")
set (HDF5_JAVA_JNI_LIB_NAME     "${HDF5_LIB_BASE}${HDF5_JAVA_JNI_LIB_CORE}")
set (HDF5_JAVA_HDF5_LIB_NAME    "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_JAVA_HDF5_LIB_CORENAME}${HDF5_LIB_INFIX}${HDF5_EXTERNAL_LIB_SUFFIX}")
set (HDF5_JAVA_TEST_LIB_NAME    "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_JAVA_TEST_LIB_CORENAME}${HDF5_LIB_INFIX}${HDF5_EXTERNAL_LIB_SUFFIX}")
set (HDF5_JAVA_JSRC_LIB_NAME    "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_JAVA_JSRC_LIB_CORENAME}${HDF5_LIB_INFIX}${HDF5_EXTERNAL_LIB_SUFFIX}")
set (HDF5_JAVA_JTEST_LIB_NAME   "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_JAVA_JTEST_LIB_CORENAME}${HDF5_LIB_INFIX}${HDF5_EXTERNAL_LIB_SUFFIX}")

#-----------------------------------------------------------------------------
# Set the target names of all the libraries
#-----------------------------------------------------------------------------
set (HDF5_LIB_TARGET              "${HDF5_LIB_CORENAME}-static")
set (HDF5_TEST_LIB_TARGET         "${HDF5_TEST_LIB_CORENAME}-static")
set (HDF5_TEST_PAR_LIB_TARGET     "${HDF5_TEST_PAR_LIB_CORENAME}-static")
set (HDF5_CPP_LIB_TARGET          "${HDF5_CPP_LIB_CORENAME}-static")
set (HDF5_HL_LIB_TARGET           "${HDF5_HL_LIB_CORENAME}-static")
set (HDF5_HL_CPP_LIB_TARGET       "${HDF5_HL_CPP_LIB_CORENAME}-static")
set (HDF5_TOOLS_LIB_TARGET        "${HDF5_TOOLS_LIB_CORENAME}-static")
set (HDF5_UTILS_LIB_TARGET        "${HDF5_UTILS_LIB_CORENAME}-static")
set (HDF5_F90_LIB_TARGET          "${HDF5_F90_LIB_CORENAME}-static")
set (HDF5_F90_C_LIB_TARGET        "${HDF5_F90_C_LIB_CORENAME}-static")
set (HDF5_F90_TEST_LIB_TARGET     "${HDF5_F90_TEST_LIB_CORENAME}-static")
set (HDF5_F90_C_TEST_LIB_TARGET   "${HDF5_F90_C_TEST_LIB_CORENAME}-static")
set (HDF5_HL_F90_LIB_TARGET       "${HDF5_HL_F90_LIB_CORENAME}-static")
set (HDF5_HL_F90_C_LIB_TARGET     "${HDF5_HL_F90_C_LIB_CORENAME}-static")
set (HDF5_LIBSH_TARGET            "${HDF5_LIB_CORENAME}-shared")
set (HDF5_TEST_LIBSH_TARGET       "${HDF5_TEST_LIB_CORENAME}-shared")
set (HDF5_TEST_PAR_LIBSH_TARGET   "${HDF5_TEST_PAR_LIB_CORENAME}-shared")
set (HDF5_CPP_LIBSH_TARGET        "${HDF5_CPP_LIB_CORENAME}-shared")
set (HDF5_HL_LIBSH_TARGET         "${HDF5_HL_LIB_CORENAME}-shared")
set (HDF5_HL_CPP_LIBSH_TARGET     "${HDF5_HL_CPP_LIB_CORENAME}-shared")
set (HDF5_TOOLS_LIBSH_TARGET      "${HDF5_TOOLS_LIB_CORENAME}-shared")
set (HDF5_UTILS_LIBSH_TARGET      "${HDF5_UTILS_LIB_CORENAME}-shared")
set (HDF5_F90_LIBSH_TARGET        "${HDF5_F90_LIB_CORENAME}-shared")
set (HDF5_F90_C_LIBSH_TARGET      "${HDF5_F90_C_LIB_CORENAME}-shared")
set (HDF5_F90_TEST_LIBSH_TARGET   "${HDF5_F90_TEST_LIB_CORENAME}-shared")
set (HDF5_F90_C_TEST_LIBSH_TARGET "${HDF5_F90_C_TEST_LIB_CORENAME}-shared")
set (HDF5_HL_F90_LIBSH_TARGET     "${HDF5_HL_F90_LIB_CORENAME}-shared")
set (HDF5_HL_F90_C_LIBSH_TARGET   "${HDF5_HL_F90_C_LIB_CORENAME}-shared")
set (HDF5_JAVA_JNI_LIB_TARGET     "${HDF5_JAVA_JNI_LIB_CORENAME}")
set (HDF5_JAVA_HDF5_LIB_TARGET    "${HDF5_JAVA_HDF5_LIB_CORENAME}")
set (HDF5_JAVA_TEST_LIB_TARGET    "${HDF5_JAVA_TEST_LIB_CORENAME}")
set (HDF5_JAVA_JSRC_LIB_TARGET    "${HDF5_JAVA_JSRC_LIB_CORENAME}")
set (HDF5_JAVA_JTEST_LIB_TARGET   "${HDF5_JAVA_JTEST_LIB_CORENAME}")

#-----------------------------------------------------------------------------
# Define some CMake variables for use later in the project
#-----------------------------------------------------------------------------
set (HDF_CONFIG_DIR            ${HDF5_SOURCE_DIR}/config)
set (HDF_RESOURCES_DIR         ${HDF5_SOURCE_DIR}/config/cmake)
set (HDF5_SRC_DIR              ${HDF5_SOURCE_DIR}/src)
set (HDF5_TEST_SRC_DIR         ${HDF5_SOURCE_DIR}/test)
set (HDF5_TEST_PAR_DIR         ${HDF5_SOURCE_DIR}/testpar)
set (HDF5_TEST_API_SRC_DIR     ${HDF5_SOURCE_DIR}/test/API)
set (HDF5_TEST_API_PAR_SRC_DIR ${HDF5_SOURCE_DIR}/testpar/API)
set (HDF5_CPP_SRC_DIR          ${HDF5_SOURCE_DIR}/c++/src)
set (HDF5_CPP_TST_DIR          ${HDF5_SOURCE_DIR}/c++/test)
set (HDF5_HL_SRC_DIR           ${HDF5_SOURCE_DIR}/hl/src)
set (HDF5_HL_TST_DIR           ${HDF5_SOURCE_DIR}/hl/test)
set (HDF5_HL_CPP_SRC_DIR       ${HDF5_SOURCE_DIR}/hl/c++/src)
set (HDF5_HL_CPP_TST_DIR       ${HDF5_SOURCE_DIR}/hl/c++/test)
set (HDF5_HL_TOOLS_DIR         ${HDF5_SOURCE_DIR}/hl/tools)
set (HDF5_TOOLS_ROOT_DIR       ${HDF5_SOURCE_DIR}/tools)
set (HDF5_TOOLS_SRC_DIR        ${HDF5_SOURCE_DIR}/tools/src)
set (HDF5_TOOLS_TST_DIR        ${HDF5_SOURCE_DIR}/tools/test)
set (HDF5_UTILS_DIR            ${HDF5_SOURCE_DIR}/utils)
set (HDF5_F90_SRC_DIR          ${HDF5_SOURCE_DIR}/fortran)
set (HDF5_JAVA_SRC_PATH            ${HDF5_SOURCE_DIR}/java)
set (HDF5_JAVA_SRCJNI_PATH         ${HDF5_SOURCE_DIR}/java/src-jni)
set (HDF5_JAVA_SRCJNI_JNI_SRC_DIR  ${HDF5_SOURCE_DIR}/java/src-jni/jni)
set (HDF5_JAVA_LIB_DIR             ${HDF5_SOURCE_DIR}/java/lib)
set (HDF5_JAVA_JSRC_DIR            ${HDF5_SOURCE_DIR}/java/jsrc)
set (HDF5_JAVA_LOGGING_JAR         ${HDF5_SOURCE_DIR}/java/lib/slf4j-api-2.0.16.jar CACHE FILEPATH "Path to SLF4J API jar")
set (HDF5_JAVA_LOGGING_NOP_JAR     ${HDF5_SOURCE_DIR}/java/lib/ext/slf4j-nop-2.0.16.jar CACHE FILEPATH "Path to SLF4J NOP binding jar")
set (HDF5_JAVA_LOGGING_SIMPLE_JAR  ${HDF5_SOURCE_DIR}/java/lib/ext/slf4j-simple-2.0.16.jar CACHE FILEPATH "Path to SLF4J simple binding jar")
set (HDF5_JAVA_JUNIT_JAR           ${HDF5_JAVA_LIB_DIR}/org.junit.jar CACHE FILEPATH "Path to JUnit jar")
set (HDF5_JAVA_HAMCREST_JAR        ${HDF5_JAVA_LIB_DIR}/org.hamcrest.jar CACHE FILEPATH "Path to Hamcrest jar")
set (HDF5_DOCS_DIR                 ${HDF5_SOURCE_DIR}/docs)
set (HDF5_DOXYGEN_DIR              ${HDF5_DOCS_DIR}/doxygen)

set (HDF5_SRC_INCLUDE_DIRS ${HDF5_SRC_DIR})

if (HDF5_USE_PREGEN)
  set (HDF5_GENERATED_SOURCE_DIR ${HDF5_USE_PREGEN_DIR})
else ()
  set (HDF5_GENERATED_SOURCE_DIR ${HDF5_F90_SRC_BINARY_DIR})
endif ()

#-----------------------------------------------------------------------------
# parse the full version number from H5public.h and include in H5_VERS_INFO
#-----------------------------------------------------------------------------
file (READ ${HDF5_SRC_DIR}/H5public.h _h5public_h_contents)
string (REGEX REPLACE ".*#define[ \t]+H5_VERS_MAJOR[ \t]+([0-9]*).*$"
    "\\1" H5_VERS_MAJOR ${_h5public_h_contents})
string (REGEX REPLACE ".*#define[ \t]+H5_VERS_MINOR[ \t]+([0-9]*).*$"
    "\\1" H5_VERS_MINOR ${_h5public_h_contents})
string (REGEX REPLACE ".*#define[ \t]+H5_VERS_RELEASE[ \t]+([0-9]*).*$"
    "\\1" H5_VERS_RELEASE ${_h5public_h_contents})
string (REGEX REPLACE ".*#define[ \t]+H5_VERS_SUBRELEASE[ \t]+\"([0-9A-Za-z._-]*)\".*$"
    "\\1" H5_VERS_SUBRELEASE ${_h5public_h_contents})
# Validate that H5_VERS_SUBRELEASE was properly extracted (must be a quoted string in H5public.h)
if (H5_VERS_SUBRELEASE STREQUAL _h5public_h_contents)
  message (FATAL_ERROR "Failed to extract H5_VERS_SUBRELEASE from src/H5public.h. "
                       "Ensure H5_VERS_SUBRELEASE is defined as a quoted string literal, "
                       "e.g., #define H5_VERS_SUBRELEASE \"\" or #define H5_VERS_SUBRELEASE \"-snap0\"")
endif ()
message (TRACE "VERSION: ${H5_VERS_MAJOR}.${H5_VERS_MINOR}.${H5_VERS_RELEASE}-${H5_VERS_SUBRELEASE}")

#-----------------------------------------------------------------------------
# parse the full soversion number from config/lt_vers.am and include in H5_SOVERS_INFO
#-----------------------------------------------------------------------------
file (READ ${HDF_CONFIG_DIR}/lt_vers.am _lt_vers_am_contents)
string (REGEX REPLACE ".*LT_VERS_INTERFACE[ \t]+=[ \t]+([0-9]*).*$"
    "\\1" H5_LIB_SOVERS_INTERFACE ${_lt_vers_am_contents})
string (REGEX REPLACE ".*LT_VERS_REVISION[ \t]+=[ \t]+([0-9]*).*$"
    "\\1" H5_LIB_SOVERS_MINOR ${_lt_vers_am_contents})
string (REGEX REPLACE ".*LT_VERS_AGE[ \t]+=[ \t]+([0-9]*).*$"
    "\\1" H5_LIB_SOVERS_RELEASE ${_lt_vers_am_contents})
math (EXPR H5_LIB_SOVERS_MAJOR ${H5_LIB_SOVERS_INTERFACE}-${H5_LIB_SOVERS_RELEASE})
message (VERBOSE "SOVERSION: ${H5_LIB_SOVERS_MAJOR}.${H5_LIB_SOVERS_RELEASE}.${H5_LIB_SOVERS_MINOR}")
string (REGEX MATCH ".*LT_TOOLS_VERS_INTERFACE[ \t]+=[ \t]+([0-9]*).*$" H5_TOOLS_SOVERS_EXISTS ${_lt_vers_am_contents})
if (H5_TOOLS_SOVERS_EXISTS)
  string (REGEX REPLACE ".*LT_TOOLS_VERS_INTERFACE[ \t]+=[ \t]+([0-9]*).*$"
      "\\1" H5_TOOLS_SOVERS_INTERFACE ${_lt_vers_am_contents})
  string (REGEX REPLACE ".*LT_TOOLS_VERS_REVISION[ \t]+=[ \t]+([0-9]*).*$"
      "\\1" H5_TOOLS_SOVERS_MINOR ${_lt_vers_am_contents})
  string (REGEX REPLACE ".*LT_TOOLS_VERS_AGE[ \t]+=[ \t]+([0-9]*).*$"
      "\\1" H5_TOOLS_SOVERS_RELEASE ${_lt_vers_am_contents})
  math (EXPR H5_TOOLS_SOVERS_MAJOR ${H5_TOOLS_SOVERS_INTERFACE}-${H5_TOOLS_SOVERS_RELEASE})
  message (VERBOSE "SOVERSION_TOOLS: ${H5_TOOLS_SOVERS_MAJOR}.${H5_TOOLS_SOVERS_RELEASE}.${H5_TOOLS_SOVERS_MINOR}")
endif ()
string (REGEX MATCH ".*LT_CXX_VERS_INTERFACE[ \t]+=[ \t]+([0-9]*).*$" H5_CXX_SOVERS_EXISTS ${_lt_vers_am_contents})
if (H5_CXX_SOVERS_EXISTS)
  string (REGEX REPLACE ".*LT_CXX_VERS_INTERFACE[ \t]+=[ \t]+([0-9]*).*$"
      "\\1" H5_CXX_SOVERS_INTERFACE ${_lt_vers_am_contents})
  string (REGEX REPLACE ".*LT_CXX_VERS_REVISION[ \t]+=[ \t]+([0-9]*).*$"
      "\\1" H5_CXX_SOVERS_MINOR ${_lt_vers_am_contents})
  string (REGEX REPLACE ".*LT_CXX_VERS_AGE[ \t]+=[ \t]+([0-9]*).*$"
      "\\1" H5_CXX_SOVERS_RELEASE ${_lt_vers_am_contents})
  math (EXPR H5_CXX_SOVERS_MAJOR ${H5_CXX_SOVERS_INTERFACE}-${H5_CXX_SOVERS_RELEASE})
  message (VERBOSE "SOVERSION_CXX: ${H5_CXX_SOVERS_MAJOR}.${H5_CXX_SOVERS_RELEASE}.${H5_CXX_SOVERS_MINOR}")
endif ()
string (REGEX MATCH ".*LT_F_VERS_INTERFACE[ \t]+=[ \t]+([0-9]*).*$" H5_F_SOVERS_EXISTS ${_lt_vers_am_contents})
if (H5_F_SOVERS_EXISTS)
  string (REGEX REPLACE ".*LT_F_VERS_INTERFACE[ \t]+=[ \t]+([0-9]*).*$"
      "\\1" H5_F_SOVERS_INTERFACE ${_lt_vers_am_contents})
  string (REGEX REPLACE ".*LT_F_VERS_REVISION[ \t]+=[ \t]+([0-9]*).*$"
      "\\1" H5_F_SOVERS_MINOR ${_lt_vers_am_contents})
  string (REGEX REPLACE ".*LT_F_VERS_AGE[ \t]+=[ \t]+([0-9]*).*$"
      "\\1" H5_F_SOVERS_RELEASE ${_lt_vers_am_contents})
  math (EXPR H5_F_SOVERS_MAJOR ${H5_F_SOVERS_INTERFACE}-${H5_F_SOVERS_RELEASE})
  message (VERBOSE "SOVERSION_F: ${H5_F_SOVERS_MAJOR}.${H5_F_SOVERS_RELEASE}.${H5_F_SOVERS_MINOR}")
endif ()
string (REGEX MATCH ".*LT_HL_VERS_INTERFACE[ \t]+=[ \t]+([0-9]*).*$" H5_HL_SOVERS_EXISTS ${_lt_vers_am_contents})
if (H5_HL_SOVERS_EXISTS)
  string (REGEX REPLACE ".*LT_HL_VERS_INTERFACE[ \t]+=[ \t]+([0-9]*).*$"
      "\\1" H5_HL_SOVERS_INTERFACE ${_lt_vers_am_contents})
  string (REGEX REPLACE ".*LT_HL_VERS_REVISION[ \t]+=[ \t]+([0-9]*).*$"
      "\\1" H5_HL_SOVERS_MINOR ${_lt_vers_am_contents})
  string (REGEX REPLACE ".*LT_HL_VERS_AGE[ \t]+=[ \t]+([0-9]*).*$"
      "\\1" H5_HL_SOVERS_RELEASE ${_lt_vers_am_contents})
  math (EXPR H5_HL_SOVERS_MAJOR ${H5_HL_SOVERS_INTERFACE}-${H5_HL_SOVERS_RELEASE})
  message (VERBOSE "SOVERSION_HL: ${H5_HL_SOVERS_MAJOR}.${H5_HL_SOVERS_RELEASE}.${H5_HL_SOVERS_MINOR}")
endif ()
string (REGEX MATCH ".*LT_HL_CXX_VERS_INTERFACE[ \t]+=[ \t]+([0-9]*).*$" H5_HL_CXX_SOVERS_EXISTS ${_lt_vers_am_contents})
if (H5_HL_CXX_SOVERS_EXISTS)
  string (REGEX REPLACE ".*LT_HL_CXX_VERS_INTERFACE[ \t]+=[ \t]+([0-9]*).*$"
      "\\1" H5_HL_CXX_SOVERS_INTERFACE ${_lt_vers_am_contents})
  string (REGEX REPLACE ".*LT_HL_CXX_VERS_REVISION[ \t]+=[ \t]+([0-9]*).*$"
      "\\1" H5_HL_CXX_SOVERS_MINOR ${_lt_vers_am_contents})
  string (REGEX REPLACE ".*LT_HL_CXX_VERS_AGE[ \t]+=[ \t]+([0-9]*).*$"
      "\\1" H5_HL_CXX_SOVERS_RELEASE ${_lt_vers_am_contents})
  math (EXPR H5_HL_CXX_SOVERS_MAJOR ${H5_HL_CXX_SOVERS_INTERFACE}-${H5_HL_CXX_SOVERS_RELEASE})
  message (VERBOSE "SOVERSION_HL_CXX: ${H5_HL_CXX_SOVERS_MAJOR}.${H5_HL_CXX_SOVERS_RELEASE}.${H5_HL_CXX_SOVERS_MINOR}")
endif ()
string (REGEX MATCH ".*LT_HL_F_VERS_INTERFACE[ \t]+=[ \t]+([0-9]*).*$" H5_HL_F_SOVERS_EXISTS ${_lt_vers_am_contents})
if (H5_HL_F_SOVERS_EXISTS)
  string (REGEX REPLACE ".*LT_HL_F_VERS_INTERFACE[ \t]+=[ \t]+([0-9]*).*$"
      "\\1" H5_HL_F_SOVERS_INTERFACE ${_lt_vers_am_contents})
  string (REGEX REPLACE ".*LT_HL_F_VERS_REVISION[ \t]+=[ \t]+([0-9]*).*$"
      "\\1" H5_HL_F_SOVERS_MINOR ${_lt_vers_am_contents})
  string (REGEX REPLACE ".*LT_HL_F_VERS_AGE[ \t]+=[ \t]+([0-9]*).*$"
      "\\1" H5_HL_F_SOVERS_RELEASE ${_lt_vers_am_contents})
  math (EXPR H5_HL_F_SOVERS_MAJOR ${H5_HL_F_SOVERS_INTERFACE}-${H5_HL_F_SOVERS_RELEASE})
  message (VERBOSE "SOVERSION_HL_F: ${H5_HL_F_SOVERS_MAJOR}.${H5_HL_F_SOVERS_RELEASE}.${H5_HL_F_SOVERS_MINOR}")
endif ()

#-----------------------------------------------------------------------------
# Basic HDF5 stuff here
#-----------------------------------------------------------------------------
set (HDF5_PACKAGE "hdf5")
set (HDF5_PACKAGE_NAME "HDF5")
set (HDF5_PACKAGE_VERSION "${H5_VERS_MAJOR}.${H5_VERS_MINOR}.${H5_VERS_RELEASE}")
set (HDF5_PACKAGE_VERSION_MAJOR "${H5_VERS_MAJOR}.${H5_VERS_MINOR}")
set (HDF5_PACKAGE_VERSION_MINOR "${H5_VERS_RELEASE}")
if (H5_VERS_SUBRELEASE)
  set (HDF5_PACKAGE_VERSION_STRING "${HDF5_PACKAGE_VERSION}.${H5_VERS_SUBRELEASE}")
  set (HDF5_RELEASE_VERSION_STRING "${HDF5_PACKAGE_VERSION}-${H5_VERS_SUBRELEASE}")
else ()
  set (HDF5_PACKAGE_VERSION_STRING "${HDF5_PACKAGE_VERSION}")
  set (HDF5_RELEASE_VERSION_STRING "${HDF5_PACKAGE_VERSION}")
endif ()
set (HDF5_LIB_PACKAGE_SOVERSION "${H5_LIB_SOVERS_MAJOR}.${H5_LIB_SOVERS_RELEASE}.${H5_LIB_SOVERS_MINOR}")
set (HDF5_LIB_PACKAGE_SOVERSION_MAJOR "${H5_LIB_SOVERS_MAJOR}")
if (H5_TOOLS_SOVERS_EXISTS)
  set (HDF5_TOOLS_PACKAGE_SOVERSION "${H5_TOOLS_SOVERS_MAJOR}.${H5_TOOLS_SOVERS_RELEASE}.${H5_TOOLS_SOVERS_MINOR}")
  set (HDF5_TOOLS_PACKAGE_SOVERSION_MAJOR "${H5_TOOLS_SOVERS_MAJOR}")
else ()
  set (HDF5_TOOLS_PACKAGE_SOVERSION "${H5_LIB_SOVERS_MAJOR}.${H5_LIB_SOVERS_RELEASE}.${H5_LIB_SOVERS_MINOR}")
  set (HDF5_TOOLS_PACKAGE_SOVERSION_MAJOR "${H5_LIB_SOVERS_MAJOR}")
endif ()
if (H5_CXX_SOVERS_EXISTS)
  set (HDF5_CXX_PACKAGE_SOVERSION "${H5_CXX_SOVERS_MAJOR}.${H5_CXX_SOVERS_RELEASE}.${H5_CXX_SOVERS_MINOR}")
  set (HDF5_CXX_PACKAGE_SOVERSION_MAJOR "${H5_CXX_SOVERS_MAJOR}")
else ()
  set (HDF5_CXX_PACKAGE_SOVERSION "${H5_LIB_SOVERS_MAJOR}.${H5_LIB_SOVERS_RELEASE}.${H5_LIB_SOVERS_MINOR}")
  set (HDF5_CXX_PACKAGE_SOVERSION_MAJOR "${H5_LIB_SOVERS_MAJOR}")
endif ()
if (H5_F_SOVERS_EXISTS)
  set (HDF5_F_PACKAGE_SOVERSION "${H5_F_SOVERS_MAJOR}.${H5_F_SOVERS_RELEASE}.${H5_F_SOVERS_MINOR}")
  set (HDF5_F_PACKAGE_SOVERSION_MAJOR "${H5_F_SOVERS_MAJOR}")
else ()
  set (HDF5_F_PACKAGE_SOVERSION "${H5_LIB_SOVERS_MAJOR}.${H5_LIB_SOVERS_RELEASE}.${H5_LIB_SOVERS_MINOR}")
  set (HDF5_F_PACKAGE_SOVERSION_MAJOR "${H5_LIB_SOVERS_MAJOR}")
endif ()
if (H5_HL_SOVERS_EXISTS)
  set (HDF5_HL_PACKAGE_SOVERSION "${H5_HL_SOVERS_MAJOR}.${H5_HL_SOVERS_RELEASE}.${H5_HL_SOVERS_MINOR}")
  set (HDF5_HL_PACKAGE_SOVERSION_MAJOR "${H5_HL_SOVERS_MAJOR}")
else ()
  set (HDF5_HL_PACKAGE_SOVERSION "${H5_LIB_SOVERS_MAJOR}.${H5_LIB_SOVERS_RELEASE}.${H5_LIB_SOVERS_MINOR}")
  set (HDF5_HL_PACKAGE_SOVERSION_MAJOR "${H5_LIB_SOVERS_MAJOR}")
endif ()
if (H5_HL_F_SOVERS_EXISTS)
  set (HDF5_HL_CXX_PACKAGE_SOVERSION "${H5_HL_CXX_SOVERS_MAJOR}.${H5_HL_CXX_SOVERS_RELEASE}.${H5_HL_CXX_SOVERS_MINOR}")
  set (HDF5_HL_CXX_PACKAGE_SOVERSION_MAJOR "${H5_HL_CXX_SOVERS_MAJOR}")
else ()
  set (HDF5_HL_CXX_PACKAGE_SOVERSION "${H5_LIB_SOVERS_MAJOR}.${H5_LIB_SOVERS_RELEASE}.${H5_LIB_SOVERS_MINOR}")
  set (HDF5_HL_CXX_PACKAGE_SOVERSION_MAJOR "${H5_LIB_SOVERS_MAJOR}")
endif ()
if (H5_HL_F_SOVERS_EXISTS)
  set (HDF5_HL_F_PACKAGE_SOVERSION "${H5_HL_F_SOVERS_MAJOR}.${H5_HL_F_SOVERS_RELEASE}.${H5_HL_F_SOVERS_MINOR}")
  set (HDF5_HL_F_PACKAGE_SOVERSION_MAJOR "${H5_HL_F_SOVERS_MAJOR}")
else ()
  set (HDF5_HL_F_PACKAGE_SOVERSION "${H5_LIB_SOVERS_MAJOR}.${H5_LIB_SOVERS_RELEASE}.${H5_LIB_SOVERS_MINOR}")
  set (HDF5_HL_F_PACKAGE_SOVERSION_MAJOR "${H5_LIB_SOVERS_MAJOR}")
endif ()
set (HDF5_PACKAGE_STRING "${HDF5_PACKAGE_NAME} ${HDF5_PACKAGE_VERSION_STRING}")
set (HDF5_PACKAGE_TARNAME "${HDF5_PACKAGE}${HDF_PACKAGE_EXT}")
set (HDF5_PACKAGE_URL "https://www.hdfgroup.org")
set (HDF5_PACKAGE_BUGREPORT "help@hdfgroup.org")

#-----------------------------------------------------------------------------
# Set variables needed for installation
#-----------------------------------------------------------------------------
set (HDF5_VERSION_STRING ${HDF5_PACKAGE_VERSION})
set (HDF5_VERSION_MAJOR  ${HDF5_PACKAGE_VERSION_MAJOR})
set (HDF5_VERSION_MINOR  ${HDF5_PACKAGE_VERSION_MINOR})
if (H5_VERS_MAJOR GREATER 1)
  if (H5_VERS_MINOR GREATER 9)
    set (H5_LIBVER_DIR ${H5_VERS_MAJOR}${H5_VERS_MINOR})
  else ()
    set (H5_LIBVER_DIR ${H5_VERS_MAJOR}0${H5_VERS_MINOR})
  endif()
else ()
  set (H5_LIBVER_DIR ${H5_VERS_MAJOR}${H5_VERS_MINOR})
endif()
#-----------------------------------------------------------------------------
# Include some macros for reusable code
#-----------------------------------------------------------------------------

get_property(IS_MULTI GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)

if(NOT CMAKE_BUILD_TYPE AND NOT IS_MULTI)
    message(STATUS "No CMAKE_BUILD_TYPE set -- using Release")
    set(CMAKE_BUILD_TYPE Release)
endif()

include (${HDF_CONFIG_DIR}/HDFMacros.cmake)

HDF_DIR_PATHS(${HDF5_PACKAGE_NAME})

include (${HDF_CONFIG_DIR}/HDF5Macros.cmake)

#-----------------------------------------------------------------------------
# Targets built within this project are exported at Install time for use
# by other projects.
#-----------------------------------------------------------------------------
if (NOT HDF5_EXPORTED_TARGETS)
  set (HDF5_EXPORTED_TARGETS "hdf5-targets")
endif ()

#-----------------------------------------------------------------------------
# To include a library in the list exported by the project AT BUILD TIME,
# add it to this variable. This is NOT used by Make Install, but for projects
# which include hdf5 as a sub-project within their build tree
#-----------------------------------------------------------------------------
set_global_variable (HDF5_LIBRARIES_TO_EXPORT "")
set_global_variable (HDF5_UTILS_TO_EXPORT "")

set (EXTERNAL_HEADER_LIST "")
set (EXTERNAL_LIBRARY_LIST "")
set (EXTERNAL_LIBRARYDLL_LIST "")

#-----------------------------------------------------------------------------
# Run all the CMake configuration tests for our build environment
#-----------------------------------------------------------------------------
include (${HDF_CONFIG_DIR}/ConfigureChecks.cmake)

set (CMAKE_INCLUDE_CURRENT_DIR_IN_INTERFACE ON)

#-----------------------------------------------------------------------------
# Include directories in the source or build tree should come before other
# directories to prioritize headers in the sources over installed ones.
#-----------------------------------------------------------------------------
set (CMAKE_INCLUDE_DIRECTORIES_PROJECT_BEFORE ON)
set (HDF5_COMP_INCLUDE_DIRECTORIES)

#-----------------------------------------------------------------------------
# Mac OS X Options
#-----------------------------------------------------------------------------
if (HDF5_BUILD_FRAMEWORKS AND NOT BUILD_SHARED_LIBS)
  set (BUILD_SHARED_LIBS ON CACHE BOOL "Build Shared Libraries" FORCE)
endif ()

#-----------------------------------------------------------------------------
# Option to Build Shared and Static libs, default is both
#-----------------------------------------------------------------------------
set (H5_ENABLE_STATIC_LIB NO)
set (H5_ENABLE_SHARED_LIB NO)

# only shared libraries/tools is true if user forces static OFF
if (NOT BUILD_STATIC_LIBS)
  set (HDF5_ONLY_SHARED_LIBS ON CACHE BOOL "Only Build Shared Libraries" FORCE)
endif ()

# only shared libraries is set ON by user then force settings
if (HDF5_ONLY_SHARED_LIBS)
  set (H5_ENABLE_STATIC_LIB NO)
  set (BUILD_SHARED_LIBS ON CACHE BOOL "Build Shared Libraries" FORCE)
  set (BUILD_STATIC_LIBS OFF CACHE BOOL "Build Static Libraries" FORCE)
  if (HDF5_BUILD_STATIC_TOOLS)
    message (WARNING "Cannot build static tools without static libraries. Building shared tools.")
  endif ()
  set (HDF5_BUILD_STATIC_TOOLS OFF CACHE BOOL "Build Static Tools NOT Shared Tools" FORCE)
endif ()

if (NOT BUILD_SHARED_LIBS AND NOT HDF5_BUILD_STATIC_TOOLS)
  message (VERBOSE "Cannot build shared tools without shared libraries. Building static tools.")
  set (HDF5_BUILD_STATIC_TOOLS ON CACHE BOOL "Build Static Tools NOT Shared Tools" FORCE)
endif ()

if (BUILD_STATIC_LIBS)
  set (H5_ENABLE_STATIC_LIB YES)
endif ()
if (BUILD_SHARED_LIBS)
  set (H5_ENABLE_SHARED_LIB YES)
endif ()

set (CMAKE_POSITION_INDEPENDENT_CODE ON)

#-----------------------------------------------------------------------------
# perl is used in some optional src and tests, check availability
find_package (Perl)
if (PERL_FOUND)
  set (H5_PERL_FOUND YES)
endif ()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Option to Build Static executables
#-----------------------------------------------------------------------------
if (BUILD_STATIC_EXECS)
  if (NOT WIN32)
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -static")
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -static")
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Options to analyze the code with various tools
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_ANALYZER_TOOLS)
  include (${HDF_CONFIG_DIR}/sanitizer/tools.cmake)
endif ()
if (HDF5_ENABLE_SANITIZERS)
  include (${HDF_CONFIG_DIR}/sanitizer/sanitizers.cmake)
endif ()
if (HDF5_ENABLE_FORMATTERS)
  include (${HDF_CONFIG_DIR}/sanitizer/formatting.cmake)
endif ()

#-----------------------------------------------------------------------------
# Option to use code coverage
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_COVERAGE)
  include (${HDF_CONFIG_DIR}/sanitizer/code-coverage.cmake)
  if (CODE_COVERAGE AND CODE_COVERAGE_ADDED)
    message (VERBOSE "Add instrumentation to all targets")
    add_code_coverage () # Adds instrumentation to all targets
  else ()
    message (VERBOSE "Use --coverage option")
    set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g -O0 --coverage -fprofile-arcs -ftest-coverage")
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g --coverage -O0 -fprofile-arcs -ftest-coverage")
    if (CMAKE_C_COMPILER_ID STREQUAL "GNU")
      set (LDFLAGS "${LDFLAGS} -fprofile-arcs -ftest-coverage")
      link_libraries (gcov)
    else ()
      set (CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} --coverage")
    endif ()
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Option to indicate using a memory checker
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_USING_MEMCHECKER)
  set (H5_USING_MEMCHECKER 1)
endif ()

#-----------------------------------------------------------------------------
# Option to enable/disable using pread/pwrite for VFDs
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_PREADWRITE AND H5_HAVE_PREAD AND H5_HAVE_PWRITE)
  set (H5_HAVE_PREADWRITE 1)
endif ()

#-----------------------------------------------------------------------------
# Option to use deprecated public API symbols
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_DEPRECATED_SYMBOLS)
  set (H5_NO_DEPRECATED_SYMBOLS 0)
else ()
  set (H5_NO_DEPRECATED_SYMBOLS 1)
endif ()

#-----------------------------------------------------------------------------
# When building utility executables that generate other (source) files :
# we make use of the following variables defined in the root CMakeLists.
# Certain systems may add /Debug or /Release to output paths
# and we need to call the executable from inside the CMake configuration
#-----------------------------------------------------------------------------
if (WIN32)
  add_compile_definitions (_CRT_SECURE_NO_WARNINGS)
  if (MSVC)
    add_compile_definitions (_BIND_TO_CURRENT_VCLIBS_VERSION=1 _CONSOLE)
  endif ()
endif ()

if (MSVC)
  set (CMAKE_MFC_FLAG 0)
  set (WIN_COMPILE_FLAGS "")
  set (WIN_LINK_FLAGS "")
endif ()

#-----------------------------------------------------------------------------
# Add some definitions for Debug Builds
#-----------------------------------------------------------------------------
if (${HDF_CFG_NAME} MATCHES "Debug" OR ${HDF_CFG_NAME} MATCHES "Developer")
  # Enable instrumenting of the library's internal operations
  cmake_dependent_option (HDF5_ENABLE_INSTRUMENT "Instrument the library" OFF HDF5_ENABLE_PARALLEL OFF)

  # Instrumenting is enabled by default for parallel debug builds
  if (HDF5_ENABLE_PARALLEL)
    set (HDF5_ENABLE_INSTRUMENT ON CACHE BOOL "Instrument The library" FORCE)
  endif ()

  if (HDF5_ENABLE_INSTRUMENT)
    set (H5_HAVE_INSTRUMENTED_LIBRARY 1)
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Add some definitions for Developer Builds
#-----------------------------------------------------------------------------
if (${HDF_CFG_NAME} MATCHES "Developer")
  include (${HDF_RESOURCES_DIR}/HDF5DeveloperBuild.cmake)
endif ()

#-----------------------------------------------------------------------------
# Option to embed library info into executables
#-----------------------------------------------------------------------------
if (CMAKE_SYSTEM_NAME STREQUAL "Emscripten")
  set (H5_HAVE_EMBEDDED_LIBINFO 0)
else ()
  if (HDF5_ENABLE_EMBEDDED_LIBINFO)
    set (H5_HAVE_EMBEDDED_LIBINFO 1)
  endif ()
endif ()

include (${HDF_CONFIG_DIR}/flags/HDFCompilerFlags.cmake)
set (CMAKE_MODULE_PATH ${HDF_CONFIG_DIR} ${HDF_RESOURCES_DIR} ${CMAKE_MODULE_PATH})

#-----------------------------------------------------------------------------
# Option to Enable HDFS
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_HDFS)
  find_package (JNI REQUIRED)
  if (JNI_FOUND)
    set (H5_HAVE_LIBJVM 1)
  endif ()
  find_package (HDFS REQUIRED)
  if (HDFS_FOUND)
    set (H5_HAVE_LIBHDFS 1)
    set (H5_HAVE_HDFS_H 1)
    if (NOT MSVC)
      list (APPEND LINK_LIBS -pthread)
    endif ()
  else ()
    set (HDF5_ENABLE_HDFS OFF CACHE BOOL "Enable HDFS" FORCE)
    message (FATAL_ERROR "Set to use libhdfs library, but could not find or use libhdfs. Please verify that the path to HADOOP_HOME is valid, and/or reconfigure without HDF5_ENABLE_HDFS")
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Option to Require Digitally Signed plugins
#-----------------------------------------------------------------------------
option (HDF5_REQUIRE_SIGNED_PLUGINS "Require digitally signed plugins" OFF)

cmake_dependent_option (HDF5_LOCK_PLUGIN_KEYSTORE "Disable HDF5_PLUGIN_KEYSTORE environment variable override (security hardening)" OFF "HDF5_REQUIRE_SIGNED_PLUGINS" OFF)
mark_as_advanced(HDF5_LOCK_PLUGIN_KEYSTORE)

if (HDF5_REQUIRE_SIGNED_PLUGINS)
  # KeyStore directory for multiple trusted public keys
  set(HDF5_PLUGIN_KEYSTORE_DIR "" CACHE PATH
      "Directory containing trusted public keys (.pem files) for plugin verification")
  # Find OpenSSL for RSA signature verification
  find_package(OpenSSL REQUIRED)
  if (NOT OPENSSL_FOUND)
    message(FATAL_ERROR "OpenSSL is required for HDF5_REQUIRE_SIGNED_PLUGINS but was not found")
  endif ()

  # Check minimum OpenSSL version
  # The signature verification implementation uses modern EVP API (EVP_DigestVerifyInit,
  # EVP_DigestVerifyUpdate, EVP_DigestVerifyFinal) which requires OpenSSL 1.1.0+
  if (OPENSSL_VERSION VERSION_LESS "1.1.0")
    message(FATAL_ERROR
      "OpenSSL 1.1.0 or later is required for HDF5_REQUIRE_SIGNED_PLUGINS\n"
      "  Found: OpenSSL ${OPENSSL_VERSION}\n"
      "  Required: OpenSSL 1.1.0 or later\n"
      "\n"
      "The signature verification implementation uses modern EVP API which is not\n"
      "available in OpenSSL 1.0.2 and earlier versions.\n"
      "\n"
      "Solutions:\n"
      "  1. Upgrade to OpenSSL 3.0 or later (recommended)\n"
      "     - OpenSSL 3.0 is LTS (supported until 2026-09-07)\n"
      "     - OpenSSL 3.4+ is also supported\n"
      "  2. Use LibreSSL 2.7.0 or later (compatible alternative)\n"
      "  3. Disable signed plugins: -DHDF5_REQUIRE_SIGNED_PLUGINS=OFF\n"
      "\n"
      "Note: OpenSSL 1.0.2 reached end-of-life in December 2019\n"
      "      CentOS 7 users should install openssl11 package")
  endif ()

  # Informational message for OpenSSL 3.0+ (APIs are compatible, not deprecated)
  if (OPENSSL_VERSION VERSION_GREATER_EQUAL "3.0.0")
    message(STATUS "OpenSSL 3.0+ detected - all EVP_* APIs are compatible (not deprecated)")
  endif ()

  # KeyStore directory is optional at build time; the HDF5_PLUGIN_KEYSTORE
  # environment variable can be used at runtime instead.  Require a compile-time
  # directory only when the environment variable override is locked out.
  if (HDF5_LOCK_PLUGIN_KEYSTORE AND NOT HDF5_PLUGIN_KEYSTORE_DIR)
    message(FATAL_ERROR
      "HDF5_LOCK_PLUGIN_KEYSTORE=ON requires a compile-time KeyStore directory:\n"
      "  -DHDF5_PLUGIN_KEYSTORE_DIR=/etc/hdf5/trusted_keys")
  endif ()

  # Configure KeyStore directory if provided.
  # Note: the path is embedded as a string literal in the library binary.
  # Use the HDF5_PLUGIN_KEYSTORE environment variable instead if the path
  # should not be visible in the binary.
  if (HDF5_PLUGIN_KEYSTORE_DIR)
    add_compile_definitions(H5PL_KEYSTORE_DIR="${HDF5_PLUGIN_KEYSTORE_DIR}")
  else ()
    message(NOTICE "No compile-time KeyStore directory configured; "
      "set HDF5_PLUGIN_KEYSTORE environment variable at runtime.")
  endif ()

  # Enable digital signature verification (goes into H5pubconf.h)
  set(H5_REQUIRE_DIGITAL_SIGNATURE 1)

  # Security: Disable environment variable override if requested
  if (HDF5_LOCK_PLUGIN_KEYSTORE)
    add_compile_definitions(H5PL_DISABLE_ENV_KEYSTORE)
    message(VERBOSE "HDF5_PLUGIN_KEYSTORE environment variable override: DISABLED (security hardening)")
  endif ()

  # Add OpenSSL to link libraries for the HDF5 library
  # Only libcrypto is needed (EVP, PEM, BIO, ERR APIs); libssl (TLS) is not used
  list(APPEND LINK_LIBS OpenSSL::Crypto)

  message(VERBOSE "Digital signature verification enabled (OpenSSL ${OPENSSL_VERSION})")
endif ()

#-----------------------------------------------------------------------------
# Option to Enable MPI Parallel
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_PARALLEL)
  find_package (MPI REQUIRED COMPONENTS C)
  set (H5_HAVE_PARALLEL 1)

  # Require MPI standard 3.0 and greater
  if (MPI_VERSION LESS 3)
    message (FATAL_ERROR "HDF5 requires MPI standard 3.0 or greater")
  endif ()

  set (CMAKE_REQUIRED_LIBRARIES "${MPI_C_LIBRARIES}")
  set (CMAKE_REQUIRED_INCLUDES "${MPI_C_INCLUDE_DIRS}")
  # Used by Fortran + MPI
  CHECK_SYMBOL_EXISTS (MPI_Comm_c2f "mpi.h"  H5_HAVE_MPI_MULTI_LANG_Comm)
  CHECK_SYMBOL_EXISTS (MPI_Info_c2f "mpi.h"  H5_HAVE_MPI_MULTI_LANG_Info)

  # Used by Parallel Compression feature
  set (PARALLEL_FILTERED_WRITES ON)
  CHECK_SYMBOL_EXISTS (MPI_Ibarrier "mpi.h" H5_HAVE_MPI_Ibarrier)
  CHECK_SYMBOL_EXISTS (MPI_Issend "mpi.h" H5_HAVE_MPI_Issend)
  CHECK_SYMBOL_EXISTS (MPI_Iprobe "mpi.h" H5_HAVE_MPI_Iprobe)
  CHECK_SYMBOL_EXISTS (MPI_Irecv "mpi.h" H5_HAVE_MPI_Irecv)
  if (H5_HAVE_MPI_Ibarrier AND H5_HAVE_MPI_Issend AND H5_HAVE_MPI_Iprobe AND H5_HAVE_MPI_Irecv)
    set (H5_HAVE_PARALLEL_FILTERED_WRITES 1)
  else ()
    message (WARNING "The MPI_Ibarrier/MPI_Issend/MPI_Iprobe/MPI_Irecv functions could not be located.
             Parallel writes of filtered data will be disabled.")
    set (PARALLEL_FILTERED_WRITES OFF)
  endif ()

  # Used by big I/O feature
  set (LARGE_PARALLEL_IO ON)
  CHECK_SYMBOL_EXISTS (MPI_Get_elements_x "mpi.h" H5_HAVE_MPI_Get_elements_x)
  CHECK_SYMBOL_EXISTS (MPI_Type_size_x "mpi.h" H5_HAVE_MPI_Type_size_x)
  if (NOT H5_HAVE_MPI_Get_elements_x OR NOT H5_HAVE_MPI_Type_size_x)
    message (WARNING "The MPI_Get_elements_x and/or MPI_Type_size_x functions could not be located.
             Reading/Writing >2GB of data in a single parallel I/O operation will be disabled.")
    set (LARGE_PARALLEL_IO OFF)
  endif ()

  # Used by Subfiling VFD feature
  CHECK_SYMBOL_EXISTS (MPI_Comm_split_type "mpi.h" H5_HAVE_MPI_Comm_split_type)
endif ()

# Parallel IO usage requires MPI to be Linked and Included
if (H5_HAVE_PARALLEL)
  list (APPEND LINK_PUB_LIBS MPI::MPI_C)
  if (MPI_C_LINK_FLAGS)
    set (CMAKE_EXE_LINKER_FLAGS "${MPI_C_LINK_FLAGS} ${CMAKE_EXE_LINKER_FLAGS}")
  endif ()
endif ()

# Determine if a threading package is available on this system
set (THREADS_PREFER_PTHREAD_FLAG ON)
find_package (Threads)
if (Threads_FOUND)
  set (H5_HAVE_THREADS 1)
  set (CMAKE_REQUIRED_LIBRARIES ${CMAKE_THREAD_LIBS_INIT})

  # Determine which threading package to use
  # Comment out check for C11 threads for now, since it conflicts with the
  # current --std=c99 compile flags at configuration time.  When we switch to
  # --std=c11, this can be uncommented.
  #CHECK_INCLUDE_FILE("threads.h" HAVE_THREADS_H)
  if (WIN32)
    # When Win32 is available, we use those threads
    set (H5_HAVE_WIN_THREADS 1)
  elseif (HAVE_THREADS_H)
    # When C11 threads are available, those are the top choice
    set (H5_HAVE_C11_THREADS 1)
  elseif (CMAKE_USE_PTHREADS_INIT)
    set (H5_HAVE_PTHREAD_H 1)
  else ()
    message (FATAL_ERROR " **** thread support requires C11 threads, Win32 threads or Pthreads **** ")
  endif ()
  set (HDF5_THREADS_ENABLED ON) # Used to init hdf5-config.cmake
  list (APPEND LINK_LIBS Threads::Threads)

  # Check for compiler support for atomic variables
  CHECK_INCLUDE_FILE("stdatomic.h" HAVE_STDATOMIC_H)
  if (HAVE_STDATOMIC_H)
    set (H5_HAVE_STDATOMIC_H 1)
  endif()
else ()
  set (HDF5_THREADS_ENABLED OFF) # Used to init hdf5-config.cmake
endif ()

# Determine whether to build the HDF5 Subfiling VFD
set (H5FD_SUBFILING_DIR ${HDF5_SRC_DIR}/H5FDsubfiling)
set (HDF5_SRC_INCLUDE_DIRS
    ${HDF5_SRC_INCLUDE_DIRS}
    ${H5FD_SUBFILING_DIR}
)

cmake_dependent_option (HDF5_ENABLE_SUBFILING_VFD "Build Parallel HDF5 Subfiling VFD" OFF "HDF5_ENABLE_PARALLEL;NOT WIN32" OFF)
if (HDF5_ENABLE_SUBFILING_VFD)
  # Make sure we found MPI_Comm_split_type previously
  if (NOT H5_HAVE_MPI_Comm_split_type)
    message (FATAL_ERROR "Subfiling VFD requires MPI-3 support for MPI_Comm_split_type")
  endif ()

  # Subfiling requires thread operations
  if (NOT Threads_FOUND)
    message (FATAL_ERROR "Subfiling requires thread operations support")
  endif ()

  set (H5_HAVE_SUBFILING_VFD 1)
  # IOC VFD is currently only built when subfiling is enabled
  set (H5_HAVE_IOC_VFD 1)
endif()

message (VERBOSE "LINK_LIBS=${LINK_LIBS}")
message (VERBOSE "LINK_PUB_LIBS=${LINK_PUB_LIBS}")

set (HDF5_DEFAULT_API_VERSION "v200" CACHE STRING "Enable v2.0 API (v16, v18, v110, v112, v114, v200)")
set_property (CACHE HDF5_DEFAULT_API_VERSION PROPERTY STRINGS v16 v18 v110 v112 v114 v200)
#-----------------------------------------------------------------------------
# Option to use 1.6.x API
#-----------------------------------------------------------------------------
set (H5_USE_16_API_DEFAULT 0)
if (HDF5_DEFAULT_API_VERSION MATCHES "v16")
  set (H5_USE_16_API_DEFAULT 1)
endif ()

#-----------------------------------------------------------------------------
# Option to use 1.8.x API
#-----------------------------------------------------------------------------
set (H5_USE_18_API_DEFAULT 0)
if (HDF5_DEFAULT_API_VERSION MATCHES "v18")
  set (H5_USE_18_API_DEFAULT 1)
endif ()

#-----------------------------------------------------------------------------
# Option to use 1.10.x API
#-----------------------------------------------------------------------------
set (H5_USE_110_API_DEFAULT 0)
if (HDF5_DEFAULT_API_VERSION MATCHES "v110")
  set (H5_USE_110_API_DEFAULT 1)
endif ()

#-----------------------------------------------------------------------------
# Option to use 1.12.x API
#-----------------------------------------------------------------------------
set (H5_USE_112_API_DEFAULT 0)
if (HDF5_DEFAULT_API_VERSION MATCHES "v112")
  set (H5_USE_112_API_DEFAULT 1)
endif ()

#-----------------------------------------------------------------------------
# Option to use 1.14.x API
#-----------------------------------------------------------------------------
set (H5_USE_114_API_DEFAULT 0)
if (HDF5_DEFAULT_API_VERSION MATCHES "v114")
  set (H5_USE_114_API_DEFAULT 1)
endif ()

#-----------------------------------------------------------------------------
# Option to use 2.x.y API
#-----------------------------------------------------------------------------
set (H5_USE_200_API_DEFAULT 0)
if (NOT HDF5_DEFAULT_API_VERSION)
  set (HDF5_DEFAULT_API_VERSION "v200")
endif ()
if (DEFAULT_API_VERSION MATCHES "v200")
  set (H5_USE_200_API_DEFAULT 1)
endif ()

#-----------------------------------------------------------------------------
# Include user macros
#-----------------------------------------------------------------------------
include (UserMacros.cmake)
include (config/CacheURLs.cmake)

#-----------------------------------------------------------------------------
# Include filter (zlib, szip, etc.) support
#-----------------------------------------------------------------------------
unset (SETTINGS_EXTERNAL_FILTERS)
include (${HDF_CONFIG_DIR}/HDF5UseZLIB.cmake)
include (${HDF_CONFIG_DIR}/HDF5UseLibaec.cmake)

# Process list of filters for libhdf5.settings (and the H5build_settings string)
list (REMOVE_DUPLICATES SETTINGS_EXTERNAL_FILTERS)
string (REPLACE ";" " " SETTINGS_EXTERNAL_FILTERS "${SETTINGS_EXTERNAL_FILTERS}")

# Print out compression libraries linked for debugging
message (VERBOSE "LINK_COMP_LIBS=${LINK_COMP_LIBS}")

#-----------------------------------------------------------------------------
# Include external VOL connectors
#-----------------------------------------------------------------------------
include (CMakeVOL.cmake)

#-----------------------------------------------------------------------------
# Option for external libraries on windows
#-----------------------------------------------------------------------------
if (NOT HDF5_EXTERNALLY_CONFIGURED)
  if (HDF5_PACKAGE_EXTLIBS)
    set (HDF5_NO_PACKAGES OFF CACHE BOOL "CPACK - Disable packaging" FORCE)
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Option to use threadsafe
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_THREADSAFE)
  # check for unsupported options
  if (WIN32)
    if (BUILD_STATIC_LIBS)
      message (FATAL_ERROR " **** thread-safety option not supported with static library **** ")
    endif ()
  endif ()
  if (HDF5_ENABLE_PARALLEL)
    if (NOT HDF5_ALLOW_UNSUPPORTED)
      message (FATAL_ERROR " **** Parallel and thread-safety options are not supported, override with HDF5_ALLOW_UNSUPPORTED option **** ")
    else ()
      message (VERBOSE " **** Allowing unsupported parallel and thread-safety options **** ")
    endif ()
  endif ()
  if (HDF5_BUILD_FORTRAN)
    if (NOT HDF5_ALLOW_UNSUPPORTED)
      message (FATAL_ERROR " **** Fortran and thread-safety options are not supported, override with HDF5_ALLOW_UNSUPPORTED option **** ")
    else ()
      message (VERBOSE " **** Allowing unsupported Fortran and thread-safety options **** ")
    endif ()
  endif ()
  if (HDF5_BUILD_CPP_LIB)
    if (NOT HDF5_ALLOW_UNSUPPORTED)
      message (FATAL_ERROR " **** C++ and thread-safety options are not supported, override with HDF5_ALLOW_UNSUPPORTED option **** ")
    else ()
      message (VERBOSE " **** Allowing unsupported C++ and thread-safety options **** ")
    endif ()
  endif ()
  if (HDF5_BUILD_HL_LIB)
    if (NOT HDF5_ALLOW_UNSUPPORTED)
      message (FATAL_ERROR " **** HL and thread-safety options are not supported, override with HDF5_ALLOW_UNSUPPORTED option **** ")
    else ()
      message (VERBOSE " **** Allowing unsupported HL and thread-safety options **** ")
    endif ()
  endif ()

  # Check for threading package
  if (NOT Threads_FOUND)
    message (FATAL_ERROR " **** thread-safety option requires a threading package and none was found **** ")
  endif ()

  set (H5_HAVE_THREADSAFE 1)
endif ()

#-----------------------------------------------------------------------------
# Option to use multi-threaded concurrency
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_CONCURRENCY)
  # check for unsupported options
  if (WIN32)
    if (BUILD_STATIC_LIBS)
      message (FATAL_ERROR " **** multi-threaded concurrency option not supported with static library **** ")
    endif ()
  endif ()
  if (HDF5_ENABLE_PARALLEL)
    if (NOT HDF5_ALLOW_UNSUPPORTED)
      message (FATAL_ERROR " **** Parallel and multi-threaded concurrency options are not supported, override with HDF5_ALLOW_UNSUPPORTED option **** ")
    else ()
      message (VERBOSE " **** Allowing unsupported parallel and multi-threaded concurrency options **** ")
    endif ()
  endif ()
  if (HDF5_BUILD_FORTRAN)
    if (NOT HDF5_ALLOW_UNSUPPORTED)
      message (FATAL_ERROR " **** Fortran and multi-threaded concurrency options are not supported, override with HDF5_ALLOW_UNSUPPORTED option **** ")
    else ()
      message (VERBOSE " **** Allowing unsupported Fortran and multi-threaded concurrency options **** ")
    endif ()
  endif ()
  if (HDF5_BUILD_CPP_LIB)
    if (NOT HDF5_ALLOW_UNSUPPORTED)
      message (FATAL_ERROR " **** C++ and multi-threaded concurrency options are not supported, override with HDF5_ALLOW_UNSUPPORTED option **** ")
    else ()
      message (VERBOSE " **** Allowing unsupported C++ and multi-threaded concurrency options **** ")
    endif ()
  endif ()
  if (HDF5_BUILD_HL_LIB)
    if (NOT HDF5_ALLOW_UNSUPPORTED)
      message (FATAL_ERROR " **** HL and multi-threaded concurrency options are not supported, override with HDF5_ALLOW_UNSUPPORTED option **** ")
    else ()
      message (VERBOSE " **** Allowing unsupported HL and multi-threaded concurrency options **** ")
    endif ()
  endif ()

  # Check for threading package
  if (NOT Threads_FOUND)
    message (FATAL_ERROR " **** multi-threaded concurrency option requires a threading package and none was found **** ")
  endif ()

  # Multi-threaded concurrency and threadsafe options are mutually exclusive
  if (HDF5_ENABLE_THREADSAFE)
    message (FATAL_ERROR " **** multi-threaded concurrency and threadsafe options are mutually exclusive **** ")
  endif ()

  set (H5_HAVE_CONCURRENCY 1)
endif ()

#-----------------------------------------------------------------------------
# Option to build the map API
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_MAP_API)
  set (H5_HAVE_MAP_API 1)
else ()
  set (HDF5_ENABLE_MAP_API OFF CACHE BOOL "Build the map API" FORCE)
endif ()

#-----------------------------------------------------------------------------
# Option to override the compiler for h5cc
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_PARALLEL)
  set (_HDF5_H5CC_C_COMPILER ${MPI_C_COMPILER})
else ()
  set (_HDF5_H5CC_C_COMPILER ${CMAKE_C_COMPILER})
endif ()
set (HDF5_H5CC_C_COMPILER ${_HDF5_H5CC_C_COMPILER} CACHE STRING "C compiler to use in h5cc")
mark_as_advanced (HDF5_H5CC_C_COMPILER)

#-----------------------------------------------------------------------------
# Java implementation detection for version value
# Note: Java version detection happens after find_package(Java)
# Everything else is set by the Java subdirectory based on actual Java version
#-----------------------------------------------------------------------------
if (HDF5_BUILD_JAVA)
  if (NOT BUILD_SHARED_LIBS)
    message (FATAL_ERROR "\nJava requires shared libraries!\n")
  else ()
    find_package (Java)
    message (VERBOSE "Java version is ${Java_VERSION_STRING}")
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Option to build documentation
# NOTE: This block must appear before add_subdirectory(src) so that
# DOXYGEN_FOUND and related variables are visible in src/CMakeLists.txt
# and all subsequent subdirectories that define doc targets.
#-----------------------------------------------------------------------------
if (HDF5_BUILD_DOC AND EXISTS "${HDF5_DOXYGEN_DIR}" AND IS_DIRECTORY "${HDF5_DOXYGEN_DIR}")
  find_package (Doxygen)
  if (DOXYGEN_FOUND)
    option (HDF5_ENABLE_DOXY_WARNINGS "Enable fail if doxygen parsing has warnings." OFF)
    mark_as_advanced (HDF5_ENABLE_DOXY_WARNINGS)
    if (HDF5_ENABLE_DOXY_WARNINGS)
      set (HDF5_DOXY_WARNINGS "FAIL_ON_WARNINGS_PRINT")
    else ()
      set (HDF5_DOXY_WARNINGS "NO")
    endif ()
    message (STATUS "Found Doxygen version: ${DOXYGEN_VERSION}")

    # Set the default directory for the Java sources for doxygen
    if (Java_VERSION_STRING VERSION_GREATER_EQUAL "25.0.0")
      if (HDF5_ENABLE_JNI)
        set (DOXYGEN_JAVA_DIR ${HDF5_JAVA_SRCJNI_PATH})
      else ()
        set (DOXYGEN_JAVA_DIR ${HDF5_JAVA_SRC_PATH})
      endif ()
    else ()
      set (DOXYGEN_JAVA_DIR ${HDF5_JAVA_SRCJNI_PATH})
    endif ()

    # Doxygen content and configuration files shared by all doc targets.
    # Defined here once to avoid duplication. CONFIGURE_DEPENDS tells
    # Ninja/Make to re-run CMake when a file is added or removed from these
    # directories. The per-null-build cost of checking small, stable
    # documentation directories is negligible compared to source code globs.
    # GLOB_RECURSE is required so that files added inside subdirectories
    # (e.g. dox/cookbook/, dox/high_level/, examples/tables/) also trigger
    # CMake re-configuration.
    file (GLOB_RECURSE HDF5_DOXYGEN_CONTENT_INPUTS CONFIGURE_DEPENDS
      "${HDF5_DOXYGEN_DIR}/*.dox"
      "${HDF5_DOXYGEN_DIR}/*.html"
      "${HDF5_DOXYGEN_DIR}/*.css"
      "${HDF5_DOXYGEN_DIR}/*.xml"
      "${HDF5_DOXYGEN_DIR}/*.js"
      "${HDF5_DOXYGEN_DIR}/*.md"
    )

    # Define the umbrella doxygen target once, here, so that ownership is
    # unambiguous. Subdirectories and the end-of-file doc block add
    # dependencies to it; the fallback at the bottom of this file handles
    # the case where Doxygen is not found or HDF5_BUILD_DOC is OFF.
    add_custom_target (doxygen)

    add_subdirectory (docs/doxygen)
  else ()
    message (WARNING "Doxygen needs to be installed to generate the doxygen documentation")
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Add the HDF5 Library Target to the build
#-----------------------------------------------------------------------------
add_subdirectory (src)

#-----------------------------------------------------------------------------
# Dashboard and Testing Settings
#-----------------------------------------------------------------------------
if (BUILD_TESTING)
  include (CMakeTests.cmake)
endif ()

#-----------------------------------------------------------------------------
# Option to build HDF5 Utilities
#-----------------------------------------------------------------------------
if (EXISTS "${HDF5_SOURCE_DIR}/utils" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/utils")
  if (HDF5_BUILD_PARALLEL_TOOLS AND HDF5_ENABLE_PARALLEL)
    set (CMAKE_PREFIX_PATH "$HDF_RESOURCES_DIR")
    find_package (MFU REQUIRED)
    if (MFU_FOUND)
      message (VERBOSE "LL_PATH=${LL_PATH}")
      set (H5_HAVE_LIBMFU 1)
      set (H5_HAVE_MFU_H 1)
      set (CMAKE_REQUIRED_INCLUDES "${MFU_INCLUDE_DIR}")
      set (MFU_LIBRARY_DEBUG "$MFU_LIBRARY")
      set (MFU_LIBRARY_RELEASE "$MFU_LIBRARY")
    endif ()
    find_package (CIRCLE REQUIRED)
    if (CIRCLE_FOUND)
      set (H5_HAVE_LIBCIRCLE 1)
      set (H5_HAVE_CIRCLE_H 1)
      set (CMAKE_REQUIRED_INCLUDES "${CIRCLE_INCLUDE_DIR}")
    endif ()
    find_package (DTCMP REQUIRED)
    if (DTCMP_FOUND)
      set (H5_HAVE_LIBDTCMP 1)
      set (H5_HAVE_DTCMP_H 1)
      set (CMAKE_REQUIRED_INCLUDES "${DTCMP_INCLUDE_DIR}")
    endif ()
  endif ()
  add_subdirectory (utils)
endif ()

#-----------------------------------------------------------------------------
# Option to build HDF5 Tools
#-----------------------------------------------------------------------------
if (EXISTS "${HDF5_SOURCE_DIR}/tools" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/tools")
  if (HDF5_BUILD_TOOLS)
    add_subdirectory (tools)
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Include filter plugins
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_PLUGIN_SUPPORT)
  if (${H5_LIBVER_DIR} EQUAL 16 OR HDF5_DEFAULT_API_VERSION MATCHES "v16")
    set (HDF5_ENABLE_PLUGIN_SUPPORT OFF CACHE BOOL "" FORCE)
    message (VERBOSE "Filter PLUGINs cannot be used with 1.6 API")
  else ()
    include (CMakePlugins.cmake)

#    if (HDF5_PACKAGE_EXTLIBS AND NOT HDF5_NO_PACKAGES AND HDF5_PLUGINS_FOUND)
#      PACKAGE_PLUGIN_LIBRARY (${HDF5_ALLOW_EXTERNAL_SUPPORT})
#    endif ()
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Option to build High Level API's
#-----------------------------------------------------------------------------
if (EXISTS "${HDF5_SOURCE_DIR}/hl" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/hl")
  if (HDF5_BUILD_HL_LIB)
    set (H5_INCLUDE_HL 1)
    add_subdirectory (hl)
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Option to build Fortran bindings/tests
# Make sure this appears before the CONFIGURE_FILE step
# so that fortran name mangling is detected before writing H5pubconf.h
#-----------------------------------------------------------------------------
# Set default name mangling : overridden by Fortran detection in fortran dir
set (H5_FC_FUNC  "H5_FC_FUNC(name,NAME) name ## _" CACHE INTERNAL "Fortran name mangling macro for C identifiers without underscores")
set (H5_FC_FUNC_ "H5_FC_FUNC_(name,NAME) name ## _" CACHE INTERNAL "Fortran name mangling macro for C identifiers with underscores")
if (EXISTS "${HDF5_SOURCE_DIR}/fortran" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/fortran")
  if (HDF5_BUILD_FORTRAN)
    add_subdirectory (fortran)
    if (HDF5_BUILD_HL_LIB)
      if (EXISTS "${HDF5_SOURCE_DIR}/hl/fortran" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/hl/fortran")
        #-- Build the High Level Fortran source codes
        add_subdirectory (hl/fortran)
      endif ()
    endif ()
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Option to build HDF5 C++ Library
#-----------------------------------------------------------------------------
if (EXISTS "${HDF5_SOURCE_DIR}/c++" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/c++")
  if (HDF5_BUILD_CPP_LIB)
    # check for unsupported options
    if (HDF5_ENABLE_PARALLEL)
      if (NOT HDF5_ALLOW_UNSUPPORTED)
        message (FATAL_ERROR " **** Parallel and C++ options are mutually exclusive, override with HDF5_ALLOW_UNSUPPORTED option **** ")
      else ()
        message (VERBOSE " **** Allowing unsupported Parallel and C++ options **** ")
      endif ()
    endif ()

    include (${HDF_CONFIG_DIR}/flags/HDFCompilerCXXFlags.cmake)

    add_subdirectory (c++)
    if (HDF5_BUILD_HL_LIB)
      if (EXISTS "${HDF5_SOURCE_DIR}/hl/c++" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/hl/c++")
        #-- Build the High Level Fortran source codes
        add_subdirectory (hl/c++)
      endif ()
    endif ()
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Generate the H5pubconf.h file containing user settings needed by compilation
#-----------------------------------------------------------------------------
configure_file (${HDF5_SRC_DIR}/H5pubconf.h.in ${HDF5_SRC_BINARY_DIR}/H5pubconf.h @ONLY)

#-----------------------------------------------------------------------------
# Option to build HDF5 Java Library
#-----------------------------------------------------------------------------
if (EXISTS "${HDF5_SOURCE_DIR}/java" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/java")
  if (HDF5_BUILD_JAVA)
    if (NOT BUILD_SHARED_LIBS)
      message (FATAL_ERROR "\nJava requires shared libraries!\n")
    else ()
      if (HDF5_ENABLE_MAVEN_DEPLOY)
        message (STATUS "Maven deployment enabled for ${HDF5_JAVA_ARTIFACT_ID}")
      endif ()
      add_subdirectory (java)
    endif ()
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Option to build examples
#-----------------------------------------------------------------------------
if (EXISTS "${HDF5_SOURCE_DIR}/HDF5Examples" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/HDF5Examples")
  if (HDF5_BUILD_EXAMPLES AND NOT HDF5_USE_SANITIZER)
    include (${HDF_RESOURCES_DIR}/HDF5ExampleCache.cmake)
    set (HDF5_VERSION ${HDF5_PACKAGE_VERSION})
    add_subdirectory (HDF5Examples)
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Define Doxygen documentation target
# This block must appear AFTER all add_subdirectory() calls so that every
# language/tool subdirectory has had the opportunity to append its sources
# to the HDF5_DOXYGEN_LIB_INPUTS and HDF5_DOXYGEN_HL_INPUTS global
# properties via set_property(GLOBAL APPEND ...).
#-----------------------------------------------------------------------------
if (HDF5_BUILD_DOC AND DOXYGEN_FOUND)

  # -----------------------------------------------------------------------
  # Retrieve explicitly tracked source lists contributed by subdirectories.
  # -----------------------------------------------------------------------
  get_property (_hdf5_doxy_inputs GLOBAL PROPERTY HDF5_DOXYGEN_LIB_INPUTS)

  # Merge HL inputs when the HL library is enabled — there is only one
  # Doxyfile / output directory, so a single Doxygen invocation covers both.
  if (HDF5_BUILD_HL_LIB)
    get_property (_hdf5_hl_doxy_inputs GLOBAL PROPERTY HDF5_DOXYGEN_HL_INPUTS)
    list (APPEND _hdf5_doxy_inputs ${_hdf5_hl_doxy_inputs})
  endif ()

  # Java sources: only when Java bindings are enabled. DOXYGEN_JAVA_DIR
  # (set in the HDF5_BUILD_DOC block above) selects the correct path
  # regardless of whether the JNI or pure-Java tree was chosen.
  if (HDF5_BUILD_JAVA)
    list (APPEND _hdf5_doxy_inputs
      "${DOXYGEN_JAVA_DIR}/hdf/hdf5lib/H5.java"
      "${DOXYGEN_JAVA_DIR}/hdf/hdf5lib/HDF5Constants.java"
      "${DOXYGEN_JAVA_DIR}/hdf/hdf5lib/HDFArray.java"
      "${DOXYGEN_JAVA_DIR}/hdf/hdf5lib/HDFNativeData.java"
      "${DOXYGEN_JAVA_DIR}/hdf/hdf5lib/exceptions/HDF5Exception.java"
      "${DOXYGEN_JAVA_DIR}/hdf/hdf5lib/exceptions/HDF5IdException.java"
      "${DOXYGEN_JAVA_DIR}/hdf/hdf5lib/exceptions/HDF5AttributeException.java"
      "${DOXYGEN_JAVA_DIR}/hdf/hdf5lib/exceptions/HDF5BtreeException.java"
      "${DOXYGEN_JAVA_DIR}/hdf/hdf5lib/exceptions/HDF5DataFiltersException.java"
      "${DOXYGEN_JAVA_DIR}/hdf/hdf5lib/exceptions/HDF5DatasetInterfaceException.java"
      "${DOXYGEN_JAVA_DIR}/hdf/hdf5lib/exceptions/HDF5DataspaceInterfaceException.java"
      "${DOXYGEN_JAVA_DIR}/hdf/hdf5lib/exceptions/HDF5DataStorageException.java"
      "${DOXYGEN_JAVA_DIR}/hdf/hdf5lib/exceptions/HDF5DatatypeInterfaceException.java"
      "${DOXYGEN_JAVA_DIR}/hdf/hdf5lib/exceptions/HDF5ExternalFileListException.java"
      "${DOXYGEN_JAVA_DIR}/hdf/hdf5lib/exceptions/HDF5FileInterfaceException.java"
      "${DOXYGEN_JAVA_DIR}/hdf/hdf5lib/exceptions/HDF5FunctionArgumentException.java"
      "${DOXYGEN_JAVA_DIR}/hdf/hdf5lib/exceptions/HDF5FunctionEntryExitException.java"
      "${DOXYGEN_JAVA_DIR}/hdf/hdf5lib/exceptions/HDF5HeapException.java"
      "${DOXYGEN_JAVA_DIR}/hdf/hdf5lib/exceptions/HDF5InternalErrorException.java"
      "${DOXYGEN_JAVA_DIR}/hdf/hdf5lib/exceptions/HDF5JavaException.java"
      "${DOXYGEN_JAVA_DIR}/hdf/hdf5lib/exceptions/HDF5LibraryException.java"
      "${DOXYGEN_JAVA_DIR}/hdf/hdf5lib/exceptions/HDF5LowLevelIOException.java"
      "${DOXYGEN_JAVA_DIR}/hdf/hdf5lib/exceptions/HDF5MetaDataCacheException.java"
      "${DOXYGEN_JAVA_DIR}/hdf/hdf5lib/exceptions/HDF5ObjectHeaderException.java"
      "${DOXYGEN_JAVA_DIR}/hdf/hdf5lib/exceptions/HDF5PropertyListInterfaceException.java"
      "${DOXYGEN_JAVA_DIR}/hdf/hdf5lib/exceptions/HDF5ReferenceException.java"
      "${DOXYGEN_JAVA_DIR}/hdf/hdf5lib/exceptions/HDF5ResourceUnavailableException.java"
      "${DOXYGEN_JAVA_DIR}/hdf/hdf5lib/exceptions/HDF5SymbolTableException.java"
    )
  endif ()

  # Shared doxygen content inputs (*.dox, *.html, *.css, etc.) — defined
  # earlier in the HDF5_BUILD_DOC block via file(GLOB_RECURSE).
  list (APPEND _hdf5_doxy_inputs ${HDF5_DOXYGEN_CONTENT_INPUTS})

  # -----------------------------------------------------------------------
  # hdf5lib_doc: single documentation target for the entire HDF5 API
  # -----------------------------------------------------------------------
  # Make the stamp file config-agnostic so multi-config generators (VS,
  # Xcode, Ninja Multi-Config) deduplicate the custom command and only run
  # Doxygen once. Doxygen writes to a single, config-agnostic output
  # directory, so per-config stamps would race over the same HTML files.
  set (_hdf5lib_doc_stamp "${HDF5_SRC_BINARY_DIR}/hdf5lib_doc.stamp")

  # Use add_custom_command with a stamp file so that CMake can perform a
  # timestamp-based up-to-date check. A bare add_custom_target(... COMMAND
  # ...) would unconditionally invoke Doxygen on every build.
  add_custom_command (
    OUTPUT  "${_hdf5lib_doc_stamp}"
    COMMAND ${DOXYGEN_EXECUTABLE} ${HDF5_BINARY_DIR}/Doxyfile
    COMMAND ${CMAKE_COMMAND} -E touch "${_hdf5lib_doc_stamp}"
    DEPENDS ${_hdf5_doxy_inputs}
            ${HDF5_BINARY_DIR}/Doxyfile
    WORKING_DIRECTORY "${HDF5_SOURCE_DIR}"
    COMMENT "Generating HDF5 API documentation with Doxygen"
    VERBATIM
  )

  # Part of ALL so that HDF5_BUILD_DOC=ON builds docs during a normal build.
  # SOURCES exposes the dependency list in IDE project trees (CLion, VS, Xcode).
  add_custom_target (hdf5lib_doc ALL
    DEPENDS "${_hdf5lib_doc_stamp}"
    SOURCES ${_hdf5_doxy_inputs}
  )
  add_dependencies (doxygen hdf5lib_doc)

endif ()

# Always define the umbrella doxygen target so that scripts and IDEs can
# unconditionally reference it (e.g. cmake --build . --target doxygen).
# When HDF5_BUILD_DOC=OFF or Doxygen is absent this becomes a no-op.
if (NOT TARGET doxygen)
  add_custom_target (doxygen
    COMMENT "Doxygen documentation not enabled (HDF5_BUILD_DOC=OFF or Doxygen not found)"
  )
endif ()

#-----------------------------------------------------------------------------
# Set LINK_LIBS_SETTINGS: linker-flag-formatted version of LINK_LIBS +
# LINK_COMP_LIBS for the libhdf5.settings file.
#-----------------------------------------------------------------------------
include (${HDF_CONFIG_DIR}/HDF5Pkgconfig.cmake)

set (LINK_LIBS_SETTINGS "")
foreach (_lib IN LISTS LINK_LIBS LINK_COMP_LIBS)
  unset (_lib_flags)
  unset (_lib_cflags)
  unset (_lib_skipped)
  extract_lib_pkgconfig_info ("${_lib}" _lib_flags _lib_cflags _lib_skipped)
  if (NOT _lib_skipped AND _lib_flags)
    if (MSVC)
      foreach (_flag IN LISTS _lib_flags)
        if (_flag MATCHES "^-l")
          string (REGEX REPLACE "^-l" "" _clean_flag "${_flag}")
          list (APPEND LINK_LIBS_SETTINGS "${_clean_flag}")
        elseif (IS_ABSOLUTE "${_flag}")
          cmake_path (GET _flag STEM _clean_flag)
          list (APPEND LINK_LIBS_SETTINGS "${_clean_flag}")
        elseif (NOT _flag MATCHES "^-L")
          # Append as-is if it's not a library path (-L)
          list (APPEND LINK_LIBS_SETTINGS "${_flag}")
        endif ()
      endforeach ()
    else ()
      # Standard behavior for non-MSVC platforms (use pkg-config formatted flags).
      list (APPEND LINK_LIBS_SETTINGS ${_lib_flags})
    endif ()
  endif ()
endforeach ()
list (REMOVE_DUPLICATES LINK_LIBS_SETTINGS)

#-----------------------------------------------------------------------------
# Installation
#-----------------------------------------------------------------------------
include (CMakeInstallation.cmake)

#-----------------------------------------------------------------------------
# Hide some miscellaneous CMake variables from the default GUI
#-----------------------------------------------------------------------------
mark_as_advanced (FETCHCONTENT_BASE_DIR FETCHCONTENT_FULLY_DISCONNECTED FETCHCONTENT_QUIET FETCHCONTENT_UPDATES_DISCONNECTED)

