cmake_minimum_required (VERSION 2.8.10)
PROJECT (HDF5 C CXX)

#-----------------------------------------------------------------------------
# Instructions for use : Normal Build
#
# For standard build of HDF5 libraries,tests and tools.
# Run cmake using the HDF5 source tree to generate a build tree.
# Enable/Disable options according to requirements and
# set CMAKE_INSTALL_PREFIX to the required install path.
# Make install can be used to install all components for system-wide use.
#
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
#   INSTALL (EXPORT target...) syntax), then targets are not configured
#   for export during install.
#
# HDF5_LIB_DEPENDENCIES :
#   If the build of HDF5 libs is being customized, then rules for the
#   dependencies of the HDF5 libs may be 'incomplete', add additional
#   dependencies to this variable so that external projects pick them up
#
# HDF5_EXTERNAL_LIB_PREFIX :
#   If the parent project needs to install hdf libraries, but avoid 
#   name conflicts with system versions, then a prefix may be added
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
#   SET(HDF5_EXTERNALLY_CONFIGURED 1)
#   # Avoid duplicating names of installed libraries
#   SET(HDF5_EXTERNAL_LIB_PREFIX "vtk")
#   # Export configuration to this export variable
#   SET(HDF5_EXPORTED_TARGETS "paraview-targets")
#
#   # Setup all necessary overrides for zlib so that HDF5 uses our
#   # internally compiled zlib rather than any other version
#   IF(HDF5_ENABLE_Z_LIB_SUPPORT)
#     # We must tell the main HDF5 library that it depends on our zlib 
#     SET(HDF5_LIB_DEPENDENCIES vtkzlib)    
#     # Override the zlib header file
#     IF(VTK_USE_SYSTEM_ZLIB)
#       SET(H5_ZLIB_HEADER "zlib.h")
#     ELSE(VTK_USE_SYSTEM_ZLIB)
#       SET(H5_ZLIB_HEADER "vtk_zlib.h")
#       # Set vars that FindZlib would have set if used in sub project
#       SET(ZLIB_INCLUDE_DIRS "${VTK_ZLIB_INCLUDE_DIRS}")
#       SET(ZLIB_LIBRARIES vtkzlib)
#     ENDIF(VTK_USE_SYSTEM_ZLIB)
#   ENDIF(HDF5_ENABLE_Z_LIB_SUPPORT)
# 
#   # Add the sub project
#   ADD_SUBDIRECTORY(Utilities/hdf5-1.8)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Allow Visual Studio solution directories
#-----------------------------------------------------------------------------
# Provide a way for Visual Studio Express users to turn OFF the new FOLDER
# organization feature. Default to ON for non-Express users. Express users must
# explicitly turn off this option to build HDF5 in the Express IDE...
#
OPTION (HDF5_USE_FOLDERS "Enable folder grouping of projects in IDEs." ON)
MARK_AS_ADVANCED (HDF5_USE_FOLDERS)
IF (HDF5_USE_FOLDERS)
  SET_PROPERTY (GLOBAL PROPERTY USE_FOLDERS ON)
ENDIF (HDF5_USE_FOLDERS)
OPTION (HDF5_NO_PACKAGES "CPACK - Disable packaging" OFF)
MARK_AS_ADVANCED (HDF5_NO_PACKAGES)

#-----------------------------------------------------------------------------
# Set the core names of all the libraries
#-----------------------------------------------------------------------------
SET (HDF5_LIB_CORENAME              "hdf5")
SET (HDF5_TEST_LIB_CORENAME         "hdf5_test")
SET (HDF5_CPP_LIB_CORENAME          "hdf5_cpp")
SET (HDF5_HL_LIB_CORENAME           "hdf5_hl")
SET (HDF5_HL_CPP_LIB_CORENAME       "hdf5_hl_cpp")
SET (HDF5_TOOLS_LIB_CORENAME        "hdf5_tools")
SET (HDF5_F90_LIB_CORENAME          "hdf5_fortran")
SET (HDF5_F90_C_LIB_CORENAME        "hdf5_f90cstub")
SET (HDF5_F90_TEST_LIB_CORENAME     "hdf5_test_fortran")
SET (HDF5_F90_C_TEST_LIB_CORENAME   "hdf5_test_f90cstub")
SET (HDF5_HL_F90_LIB_CORENAME       "hdf5_hl_fortran")
SET (HDF5_HL_F90_C_LIB_CORENAME     "hdf5_hl_f90cstub")

#-----------------------------------------------------------------------------
# Set the true names of all the libraries if customized by external project
#-----------------------------------------------------------------------------
SET (HDF5_LIB_NAME              "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_LIB_CORENAME}")
SET (HDF5_TEST_LIB_NAME         "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_TEST_LIB_CORENAME}")
SET (HDF5_CPP_LIB_NAME          "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_CPP_LIB_CORENAME}")
SET (HDF5_HL_LIB_NAME           "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_HL_LIB_CORENAME}")
SET (HDF5_HL_CPP_LIB_NAME       "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_HL_CPP_LIB_CORENAME}")
SET (HDF5_TOOLS_LIB_NAME        "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_TOOLS_LIB_CORENAME}")
SET (HDF5_F90_LIB_NAME          "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_F90_LIB_CORENAME}")
SET (HDF5_F90_C_LIB_NAME        "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_F90_C_LIB_CORENAME}")
SET (HDF5_F90_TEST_LIB_NAME     "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_F90_TEST_LIB_CORENAME}")
SET (HDF5_F90_C_TEST_LIB_NAME   "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_F90_C_TEST_LIB_CORENAME}")
SET (HDF5_HL_F90_LIB_NAME       "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_HL_F90_LIB_CORENAME}")
SET (HDF5_HL_F90_C_LIB_NAME     "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_HL_F90_C_LIB_CORENAME}")

#-----------------------------------------------------------------------------
# Set the target names of all the libraries
#-----------------------------------------------------------------------------
SET (HDF5_LIB_TARGET              ${HDF5_LIB_CORENAME})
SET (HDF5_TEST_LIB_TARGET         ${HDF5_TEST_LIB_CORENAME})
SET (HDF5_CPP_LIB_TARGET          ${HDF5_CPP_LIB_CORENAME})
SET (HDF5_HL_LIB_TARGET           ${HDF5_HL_LIB_CORENAME})
SET (HDF5_HL_CPP_LIB_TARGET       ${HDF5_HL_CPP_LIB_CORENAME})
SET (HDF5_TOOLS_LIB_TARGET        ${HDF5_TOOLS_LIB_CORENAME})
SET (HDF5_F90_LIB_TARGET          ${HDF5_F90_LIB_CORENAME})
SET (HDF5_F90_C_LIB_TARGET        ${HDF5_F90_C_LIB_CORENAME})
SET (HDF5_F90_TEST_LIB_TARGET     ${HDF5_F90_TEST_LIB_CORENAME})
SET (HDF5_F90_C_TEST_LIB_TARGET   ${HDF5_F90_C_TEST_LIB_CORENAME})
SET (HDF5_HL_F90_LIB_TARGET       ${HDF5_HL_F90_LIB_CORENAME})
SET (HDF5_HL_F90_C_LIB_TARGET     ${HDF5_HL_F90_C_LIB_CORENAME})

#-----------------------------------------------------------------------------
# Define some CMake variables for use later in the project
#-----------------------------------------------------------------------------
SET (HDF5_RESOURCES_DIR      ${HDF5_SOURCE_DIR}/config/cmake)
SET (HDF5_SRC_DIR            ${HDF5_SOURCE_DIR}/src)
SET (HDF5_TEST_SRC_DIR       ${HDF5_SOURCE_DIR}/test)
SET (HDF5_CPP_SRC_DIR        ${HDF5_SOURCE_DIR}/c++)
SET (HDF5_CPP_TST_DIR        ${HDF5_SOURCE_DIR}/c++/test)
SET (HDF5_HL_SRC_DIR         ${HDF5_SOURCE_DIR}/hl)
SET (HDF5_HL_CPP_SRC_DIR     ${HDF5_SOURCE_DIR}/hl/c++)
SET (HDF5_TOOLS_SRC_DIR      ${HDF5_SOURCE_DIR}/tools)
SET (HDF5_PERFORM_SRC_DIR    ${HDF5_SOURCE_DIR}/perform)
SET (HDF5_F90_SRC_DIR        ${HDF5_SOURCE_DIR}/fortran)

IF (NOT HDF5_INSTALL_BIN_DIR)
  SET (HDF5_INSTALL_BIN_DIR bin)
ENDIF (NOT HDF5_INSTALL_BIN_DIR)
IF (NOT HDF5_INSTALL_LIB_DIR)
  SET (HDF5_INSTALL_LIB_DIR lib)
ENDIF (NOT HDF5_INSTALL_LIB_DIR)
IF (NOT HDF5_INSTALL_INCLUDE_DIR)
  SET (HDF5_INSTALL_INCLUDE_DIR include)
ENDIF (NOT HDF5_INSTALL_INCLUDE_DIR)
IF (NOT HDF5_INSTALL_DATA_DIR)
  IF (NOT WIN32)
    SET (HDF5_INSTALL_DATA_DIR share)
    SET (HDF5_INSTALL_CMAKE_DIR share/cmake)
  ELSE (NOT WIN32)
    SET (HDF5_INSTALL_DATA_DIR ".")
    SET (HDF5_INSTALL_CMAKE_DIR cmake)
  ENDIF (NOT WIN32)
ENDIF (NOT HDF5_INSTALL_DATA_DIR)

#-----------------------------------------------------------------------------
# parse the full version number from H5public.h and include in H5_VERS_INFO
#-----------------------------------------------------------------------------
FILE (READ ${HDF5_SRC_DIR}/H5public.h _h5public_h_contents)
STRING (REGEX REPLACE ".*#define[ \t]+H5_VERS_MAJOR[ \t]+([0-9]*).*$"
    "\\1" H5_VERS_MAJOR ${_h5public_h_contents})
STRING (REGEX REPLACE ".*#define[ \t]+H5_VERS_MINOR[ \t]+([0-9]*).*$"
    "\\1" H5_VERS_MINOR ${_h5public_h_contents})
STRING (REGEX REPLACE ".*#define[ \t]+H5_VERS_RELEASE[ \t]+([0-9]*).*$"
    "\\1" H5_VERS_RELEASE ${_h5public_h_contents})
STRING (REGEX REPLACE ".*#define[ \t]+H5_VERS_SUBRELEASE[ \t]+\"([0-9A-Za-z._]*)\".*$"
    "\\1" H5_VERS_SUBRELEASE ${_h5public_h_contents})
#MESSAGE (STATUS "VERSION: ${H5_VERS_MAJOR}.${H5_VERS_MINOR}.${H5_VERS_RELEASE}-${H5_VERS_SUBRELEASE}")

#-----------------------------------------------------------------------------
# parse the full soversion number from config/lt_vers.am and include in H5_SOVERS_INFO
#-----------------------------------------------------------------------------
FILE (READ ${HDF5_SOURCE_DIR}/config/lt_vers.am _lt_vers_am_contents)
STRING (REGEX REPLACE ".*LT_VERS_INTERFACE[ \t]+=[ \t]+([0-9]*).*$"
    "\\1" H5_SOVERS_INTERFACE ${_lt_vers_am_contents})
STRING (REGEX REPLACE ".*LT_VERS_REVISION[ \t]+=[ \t]+([0-9]*).*$"
    "\\1" H5_SOVERS_MINOR ${_lt_vers_am_contents})
STRING (REGEX REPLACE ".*LT_VERS_AGE[ \t]+=[ \t]+([0-9]*).*$"
    "\\1" H5_SOVERS_RELEASE ${_lt_vers_am_contents})
MATH(EXPR H5_SOVERS_MAJOR ${H5_SOVERS_INTERFACE}-${H5_SOVERS_RELEASE})
MESSAGE (STATUS "SOVERSION: ${H5_SOVERS_MAJOR}.${H5_SOVERS_RELEASE}.${H5_SOVERS_MINOR}")

#-----------------------------------------------------------------------------
# Basic HDF5 stuff here
#-----------------------------------------------------------------------------
SET (HDF5_PACKAGE "hdf5")
SET (HDF5_PACKAGE_NAME "HDF5")
SET (HDF5_PACKAGE_VERSION "${H5_VERS_MAJOR}.${H5_VERS_MINOR}.${H5_VERS_RELEASE}")
SET (HDF5_PACKAGE_VERSION_MAJOR "${H5_VERS_MAJOR}.${H5_VERS_MINOR}")
SET (HDF5_PACKAGE_VERSION_MINOR "${H5_VERS_RELEASE}")
IF (NOT "${H5_VERS_SUBRELEASE}" STREQUAL "")
  SET (HDF5_PACKAGE_VERSION_STRING "${HDF5_PACKAGE_VERSION}-${H5_VERS_SUBRELEASE}")
ELSE (NOT "${H5_VERS_SUBRELEASE}" STREQUAL "")
  SET (HDF5_PACKAGE_VERSION_STRING "${HDF5_PACKAGE_VERSION}")
ENDIF (NOT "${H5_VERS_SUBRELEASE}" STREQUAL "")
SET (HDF5_PACKAGE_SOVERSION "${H5_SOVERS_MAJOR}.${H5_SOVERS_RELEASE}.${H5_SOVERS_MINOR}")
SET (HDF5_PACKAGE_STRING "${HDF5_PACKAGE_NAME} ${HDF5_PACKAGE_VERSION_STRING}")
SET (HDF5_PACKAGE_TARNAME "${HDF5_PACKAGE}${HDF_PACKAGE_EXT}")
SET (HDF5_PACKAGE_URL "http://www.hdfgroup.org")
SET (HDF5_PACKAGE_BUGREPORT "help@hdfgroup.org")

#-----------------------------------------------------------------------------
# Include some macros for reusable code
#-----------------------------------------------------------------------------
INCLUDE (${HDF5_RESOURCES_DIR}/HDFMacros.cmake)
INCLUDE (${HDF5_RESOURCES_DIR}/HDFLibMacros.cmake)
INCLUDE (${HDF5_RESOURCES_DIR}/HDF5Macros.cmake)

#-----------------------------------------------------------------------------
# Setup output Directories
#-----------------------------------------------------------------------------
IF (NOT HDF5_EXTERNALLY_CONFIGURED)
  SET (CMAKE_RUNTIME_OUTPUT_DIRECTORY
      ${PROJECT_BINARY_DIR}/bin CACHE PATH "Single Directory for all Executables."
  )
  SET (CMAKE_LIBRARY_OUTPUT_DIRECTORY
      ${PROJECT_BINARY_DIR}/bin CACHE PATH "Single Directory for all Libraries"
  )
  SET (CMAKE_ARCHIVE_OUTPUT_DIRECTORY
      ${PROJECT_BINARY_DIR}/bin CACHE PATH "Single Directory for all static libraries."
  )
  SET (CMAKE_Fortran_MODULE_DIRECTORY
      ${PROJECT_BINARY_DIR}/bin/fortran CACHE PATH "Single Directory for all fortran modules."
  )
ELSE (NOT HDF5_EXTERNALLY_CONFIGURED)
  # if we are externally configured, but the project uses old cmake scripts
  # this may not be set and utilities like H5detect will fail
  IF (NOT CMAKE_RUNTIME_OUTPUT_DIRECTORY)
    SET (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${EXECUTABLE_OUTPUT_PATH})
  ENDIF (NOT CMAKE_RUNTIME_OUTPUT_DIRECTORY)
ENDIF (NOT HDF5_EXTERNALLY_CONFIGURED)

#-----------------------------------------------------------------------------
# Targets built within this project are exported at Install time for use
# by other projects using FindHDF5. 
#-----------------------------------------------------------------------------
IF (NOT HDF5_EXPORTED_TARGETS)
  SET (HDF5_EXPORTED_TARGETS "hdf5-targets")
ENDIF (NOT HDF5_EXPORTED_TARGETS)

#-----------------------------------------------------------------------------
# To include a library in the list exported by the project AT BUILD TIME,
# add it to this variable. This is NOT used by Make Install, but for projects
# which include hdf5 as a sub-project within their build tree
#-----------------------------------------------------------------------------
SET_GLOBAL_VARIABLE (HDF5_LIBRARIES_TO_EXPORT "")
SET (EXTERNAL_HEADER_LIST "")
SET (EXTERNAL_LIBRARY_LIST "")
SET (EXTERNAL_LIBRARYDLL_LIST "")

#-----------------------------------------------------------------------------
# Run all the CMake configuration tests for our build environment
#-----------------------------------------------------------------------------
INCLUDE (${HDF5_RESOURCES_DIR}/ConfigureChecks.cmake)

#-----------------------------------------------------------------------------
# Option to Build Shared/Static libs, default is static
#-----------------------------------------------------------------------------
OPTION (BUILD_SHARED_LIBS "Build Shared Libraries" OFF)
SET (LIB_TYPE STATIC)
SET (H5_ENABLE_SHARED_LIB NO)
SET (H5_ENABLE_STATIC_LIB NO)
IF (BUILD_SHARED_LIBS)
  SET (LIB_TYPE SHARED)
  SET (H5_BUILT_AS_DYNAMIC_LIB 1)
  SET (H5_ENABLE_SHARED_LIB YES)
ELSE (BUILD_SHARED_LIBS)
  SET (H5_BUILT_AS_STATIC_LIB 1)
  SET (H5_ENABLE_STATIC_LIB YES)
  IF (NOT WIN32)
    # should this be a user setting : Everyone uses it anyway ?
    ADD_DEFINITIONS (-DPIC)
  ENDIF (NOT WIN32)
ENDIF (BUILD_SHARED_LIBS)

#-----------------------------------------------------------------------------
# Option to Build Static executables
#-----------------------------------------------------------------------------
OPTION (BUILD_STATIC_EXECS "Build Static Executabless" OFF)
IF (BUILD_STATIC_EXECS)
  IF (NOT WIN32)
    SET (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -static")
    SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -static")
  ENDIF (NOT WIN32)
ENDIF (BUILD_STATIC_EXECS)

#-----------------------------------------------------------------------------
# Option to use code coverage
#-----------------------------------------------------------------------------
OPTION (HDF5_ENABLE_COVERAGE "Enable code coverage for Libraries and Programs" OFF)
IF (HDF5_ENABLE_COVERAGE)
  SET (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g -O0 -fprofile-arcs -ftest-coverage")
  SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -O0 -fprofile-arcs -ftest-coverage")
  SET (LDFLAGS "${LDFLAGS} -fprofile-arcs -ftest-coverage")   
ENDIF (HDF5_ENABLE_COVERAGE)

#-----------------------------------------------------------------------------
# Option to indicate using dmalloc
#-----------------------------------------------------------------------------
# OPTION (HDF5_ENABLE_USING_DMALLOC "Indicate that dmalloc is used" OFF)
# IF (HDF5_ENABLE_USING_DMALLOC)
#   FIND_PACKAGE (DMALLOC)
#   SET (H5_HAVE_DMALLOC DMALLOC_FOUND)   
# ENDIF (HDF5_ENABLE_USING_DMALLOC)

#-----------------------------------------------------------------------------
# Option to indicate using a memory checker
#-----------------------------------------------------------------------------
OPTION (HDF5_ENABLE_USING_MEMCHECKER "Indicate that a memory checker is used" OFF)
IF (HDF5_ENABLE_USING_MEMCHECKER)
  SET (H5_USING_MEMCHECKER 1)   
ENDIF (HDF5_ENABLE_USING_MEMCHECKER)

#-----------------------------------------------------------------------------
# Option to use deprecated public API symbols
#-----------------------------------------------------------------------------
OPTION (HDF5_ENABLE_DEPRECATED_SYMBOLS "Enable deprecated public API symbols" ON)
IF (HDF5_ENABLE_DEPRECATED_SYMBOLS)
  SET (H5_NO_DEPRECATED_SYMBOLS 0)   
ELSE (HDF5_ENABLE_DEPRECATED_SYMBOLS)
  SET (H5_NO_DEPRECATED_SYMBOLS 1)   
ENDIF (HDF5_ENABLE_DEPRECATED_SYMBOLS)

#-----------------------------------------------------------------------------
# When building utility executables that generate other (source) files :
# we make use of the following variables defined in the root CMakeLists.
# Certain systems may add /Debug or /Release to output paths
# and we need to call the executable from inside the CMake configuration
#-----------------------------------------------------------------------------
SET (EXE_EXT "")
IF (WIN32)
  SET (EXE_EXT ".exe")
  IF (NOT CYGWIN)
    ADD_DEFINITIONS (-D_BIND_TO_CURRENT_VCLIBS_VERSION=1)
    ADD_DEFINITIONS (-D_CRT_SECURE_NO_WARNINGS)
    ADD_DEFINITIONS (-D_CONSOLE)
  ENDIF (NOT CYGWIN)
ENDIF (WIN32)

IF (MSVC)
  SET (CMAKE_MFC_FLAG 0)
  SET (WIN_COMPILE_FLAGS "")
  SET (WIN_LINK_FLAGS "")
ENDIF (MSVC)

SET (MAKE_SYSTEM)
IF (CMAKE_BUILD_TOOL MATCHES "make")
  SET (MAKE_SYSTEM 1)
ENDIF (CMAKE_BUILD_TOOL MATCHES "make")

SET (CFG_INIT "/${CMAKE_CFG_INTDIR}")
IF (MAKE_SYSTEM)
  SET (CFG_INIT "")
ENDIF (MAKE_SYSTEM)

#-----------------------------------------------------------------------------
# Add some definitions for Debug Builds
#-----------------------------------------------------------------------------
IF (CMAKE_BUILD_TYPE MATCHES Debug)
  ADD_DEFINITIONS (-DDEBUG)
  ADD_DEFINITIONS (
      -DH5Z_DEBUG -DH5V_DEBUG -DH5T_DEBUG
      -DH5S_DEBUG -DH5P_DEBUG -DH5O_DEBUG -DH5MM_DEBUG -DH5MF_DEBUG
      -DH5I_DEBUG -DH5HL_DEBUG -DH5HG_DEBUG -DH5G_DEBUG -DH5F_DEBUG
      -DH5E_DEBUG -DH5D_DEBUG -DH5B_DEBUG -DH5AC_DEBUG -UNDEBUG
  )
  # Enable tracing of the API
  OPTION (HDF5_ENABLE_TRACE "Enable API tracing capability" ON)
  IF (HDF5_ENABLE_TRACE)
    ADD_DEFINITIONS (-DH5_DEBUG_API )
  ENDIF (HDF5_ENABLE_TRACE)
  # Enable instrumenting of the library's internal operations
  OPTION (HDF5_ENABLE_INSTRUMENT "Instrument The library" OFF)
  IF (HDF5_ENABLE_INSTRUMENT)
    SET (H5_HAVE_INSTRUMENTED_LIBRARY 1)
  ENDIF (HDF5_ENABLE_INSTRUMENT)
  MARK_AS_ADVANCED (HDF5_Enable_Instrument)
  #-- NMake Makefiles will overwhelm the console with warnings if -Wall is used.
  IF (NOT WIN32)
    ADD_DEFINITIONS (-Wall)
  ENDIF (NOT WIN32)
ELSE (CMAKE_BUILD_TYPE MATCHES Debug)
  ADD_DEFINITIONS (-DNDEBUG)
  OPTION (HDF5_ENABLE_TRACE "Enable API tracing capability" OFF)
  IF (HDF5_ENABLE_TRACE)
    ADD_DEFINITIONS (-DH5_DEBUG_API )
  ENDIF (HDF5_ENABLE_TRACE)
ENDIF (CMAKE_BUILD_TYPE MATCHES Debug)

#-----------------------------------------------------------------------------
# Compiler specific flags : Shouldn't there be compiler tests for these
#-----------------------------------------------------------------------------
IF (CMAKE_COMPILER_IS_GNUCC)
  IF (CMAKE_BUILD_TYPE MATCHES Debug)
    SET (CMAKE_C_FLAGS "${CMAKE_ANSI_CFLAGS} ${CMAKE_C_FLAGS} -std=c99 -finline-functions -fno-common")
  ELSE (CMAKE_BUILD_TYPE MATCHES Debug)
    SET (CMAKE_C_FLAGS "${CMAKE_ANSI_CFLAGS} ${CMAKE_C_FLAGS} -std=c99 -fomit-frame-pointer -finline-functions -fno-common")
  ENDIF (CMAKE_BUILD_TYPE MATCHES Debug)
ENDIF (CMAKE_COMPILER_IS_GNUCC)
IF (CMAKE_COMPILER_IS_GNUCXX)
  IF (CMAKE_BUILD_TYPE MATCHES Debug)
    SET (CMAKE_CXX_FLAGS "${CMAKE_ANSI_CFLAGS} ${CMAKE_CXX_FLAGS} -finline-functions -fno-common")
  ELSE (CMAKE_BUILD_TYPE MATCHES Debug)
    SET (CMAKE_CXX_FLAGS "${CMAKE_ANSI_CFLAGS} ${CMAKE_CXX_FLAGS} -fomit-frame-pointer -finline-functions -fno-common")
  ENDIF (CMAKE_BUILD_TYPE MATCHES Debug)
ENDIF (CMAKE_COMPILER_IS_GNUCXX)

#-----------------------------------------------------------------------------
# Option to embed library info into executables
#-----------------------------------------------------------------------------
OPTION (HDF5_ENABLE_EMBEDDED_LIBINFO "embed library info into executables" ON)
IF (HDF5_ENABLE_EMBEDDED_LIBINFO)
  SET (H5_HAVE_EMBEDDED_LIBINFO 1)
ENDIF (HDF5_ENABLE_EMBEDDED_LIBINFO)

#-----------------------------------------------------------------------------
# Option to allow the user to disable compiler warnings
#-----------------------------------------------------------------------------
OPTION (HDF5_DISABLE_COMPILER_WARNINGS "Disable compiler warnings" OFF)
IF (HDF5_DISABLE_COMPILER_WARNINGS)
  # MSVC uses /w to suppress warnings.  It also complains if another
  # warning level is given, so remove it.
  IF (MSVC)
    SET (HDF5_WARNINGS_BLOCKED 1)
    STRING (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
    SET (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /w")
    STRING (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
    SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /w")
  ENDIF (MSVC)
  IF (WIN32 AND NOT CYGWIN)
    ADD_DEFINITIONS (-D_CRT_SECURE_NO_WARNINGS)
  ENDIF (WIN32 AND NOT CYGWIN)
  # Borland uses -w- to suppress warnings.
  IF (BORLAND)
    SET (HDF5_WARNINGS_BLOCKED 1)
    SET (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -w-")
  ENDIF (BORLAND)

  # Most compilers use -w to suppress warnings.
  IF (NOT HDF5_WARNINGS_BLOCKED)
    SET (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -w")
    SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -w")
  ENDIF (NOT HDF5_WARNINGS_BLOCKED)
ENDIF (HDF5_DISABLE_COMPILER_WARNINGS)

#-----------------------------------------------------------------------------
# CDash is configured to only allow 3000 warnings, so
# break into groups (from the config/gnu-flags file)
#-----------------------------------------------------------------------------
IF (NOT MSVC)
    SET (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -W -Wundef -Wshadow -Wpointer-arith -Wbad-function-cast -Wcast-qual -Wcast-align -Wwrite-strings -Wconversion -Waggregate-return -Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations -Wredundant-decls -Wnested-externs -Winline")
    # Append warning flags
    # Don't use the '-Wtraditional' flag, we're way past having K&R C code
    # SET (H5_CFLAGS "${H5_CFLAGS} -Wtraditional")
    # Don't use the '-Wtraditional-conversion' flag, there's too many warnings
    #  from GCC's assert macro
    # SET (H5_CFLAGS "${H5_CFLAGS} -Wtraditional-conversion")

    # Append warning flags from gcc-3* case
    # (don't use -Wpadded flag for normal builds, many of the warnings its
    #   issuing can't be fixed and they are making it hard to detect other,
    #   more important warnings)
    #SET (H5_CFLAGS "${H5_CFLAGS} -Wfloat-equal -Wmissing-format-attribute -Wpadded")
    SET (H5_CFLAGS1 "${H5_CFLAGS1} -Wfloat-equal -Wmissing-format-attribute")

    # Append warning flags from gcc-3.2* case
    SET (H5_CFLAGS1 "${H5_CFLAGS1} -Wmissing-noreturn -Wpacked -Wdisabled-optimization")

    # Enable more format checking flags, beyond the basic -Wformat included
    # in -Wall
    SET (H5_CFLAGS1 "${H5_CFLAGS1} -Wformat=2")

    # The "unreachable code" warning appears to be reliable now...
    # (this warning was removed in gcc 4.5+)
    #SET (H5_CFLAGS "${H5_CFLAGS} -Wunreachable-code")

    # Append warning flags from gcc-3.3* case
    SET (H5_CFLAGS1 "${H5_CFLAGS1} -Wendif-labels")

    # Append warning flags from gcc-3.4* case
    SET (H5_CFLAGS2 "${H5_CFLAGS2} -Wdeclaration-after-statement -Wold-style-definition -Winvalid-pch")

    # Append more extra warning flags that only gcc4.0+ know about
    SET (H5_CFLAGS2 "${H5_CFLAGS2} -Wvariadic-macros -Winit-self -Wmissing-include-dirs -Wswitch-default -Wswitch-enum -Wunused-macros")

    # Append more extra warning flags that only gcc 4.1+ know about
    SET (H5_CFLAGS3 "${H5_CFLAGS3} -Wunsafe-loop-optimizations -Wc++-compat")

    # Append more extra warning flags that only gcc 4.2+ know about
    SET (H5_CFLAGS3 "${H5_CFLAGS3} -Wstrict-overflow")

    # Append more extra warning flags that only gcc 4.3+ know about
    #
    # Technically, variable-length arrays are part of the C99 standard, but
    #   we should approach them a bit cautiously... -QAK
    SET (H5_CFLAGS3 "${H5_CFLAGS3} -Wlogical-op -Wlarger-than=2048 -Wvla")

    # Append more extra warning flags that only gcc 4.4+ know about
    SET (H5_CFLAGS4 "${H5_CFLAGS4} -Wsync-nand -Wframe-larger-than=16384 -Wpacked-bitfield-compat")

    # Append more extra warning flags that only gcc 4.5+ know about
    SET (H5_CFLAGS4 "${H5_CFLAGS4} -Wstrict-overflow=5 -Wjump-misses-init -Wunsuffixed-float-constants")

    # Append more extra warning flags that only gcc 4.6+ know about
    SET (H5_CFLAGS5 "${H5_CFLAGS5} -Wdouble-promotion -Wsuggest-attribute=const -Wtrampolines")

    # Append more extra warning flags that only gcc 4.7+ know about
    SET (H5_CFLAGS5 "${H5_CFLAGS5} -Wstack-usage=8192 -Wvector-operation-performance -Wsuggest-attribute=pure -Wsuggest-attribute=noreturn")
ENDIF (NOT MSVC)
  
#-----------------------------------------------------------------------------
# Option to allow the user to enable all warnings
#-----------------------------------------------------------------------------
OPTION (HDF5_ENABLE_ALL_WARNINGS "Enable all warnings" OFF)
IF (HDF5_ENABLE_ALL_WARNINGS)
  IF (MSVC)
    STRING (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
    SET (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /Wall")
    STRING (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
    SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Wall")
  ELSE (MSVC)
    SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wextra -pedantic ${H5_CFLAGS1} ${H5_CFLAGS2} ${H5_CFLAGS3} ${H5_CFLAGS4}")
  ENDIF (MSVC)
ENDIF (HDF5_ENABLE_ALL_WARNINGS)

#-----------------------------------------------------------------------------
# Option to allow the user to enable warnings by groups
#-----------------------------------------------------------------------------
OPTION (HDF5_ENABLE_GROUPZERO_WARNINGS "Enable group zero warnings" OFF)
IF (HDF5_ENABLE_GROUPZERO_WARNINGS)
  IF (MSVC)
    STRING (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
    SET (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /W1")
    STRING (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
    SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W1")
  ELSE (MSVC)
    SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wextra -pedantic")
  ENDIF (MSVC)
ENDIF (HDF5_ENABLE_GROUPZERO_WARNINGS)

#-----------------------------------------------------------------------------
# Option to allow the user to enable warnings by groups
#-----------------------------------------------------------------------------
OPTION (HDF5_ENABLE_GROUPONE_WARNINGS "Enable group one warnings" OFF)
IF (HDF5_ENABLE_GROUPONE_WARNINGS)
  IF (MSVC)
    STRING (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
    SET (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /W2")
    STRING (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
    SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W2")
  ELSE (MSVC)
    SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${H5_CFLAGS1}")
  ENDIF (MSVC)
ENDIF (HDF5_ENABLE_GROUPONE_WARNINGS)

#-----------------------------------------------------------------------------
# Option to allow the user to enable warnings by groups
#-----------------------------------------------------------------------------
OPTION (HDF5_ENABLE_GROUPTWO_WARNINGS "Enable group two warnings" OFF)
IF (HDF5_ENABLE_GROUPTWO_WARNINGS)
  IF (MSVC)
    STRING (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
    SET (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /W3")
    STRING (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
    SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W3")
  ELSE (MSVC)
    SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${H5_CFLAGS2}")
  ENDIF (MSVC)
ENDIF (HDF5_ENABLE_GROUPTWO_WARNINGS)

#-----------------------------------------------------------------------------
# Option to allow the user to enable warnings by groups
#-----------------------------------------------------------------------------
OPTION (HDF5_ENABLE_GROUPTHREE_WARNINGS "Enable group three warnings" OFF)
IF (HDF5_ENABLE_GROUPTHREE_WARNINGS)
  IF (MSVC)
    STRING (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
    SET (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /W4")
    STRING (REGEX REPLACE "(^| )([/-])W[0-9]( |$)" " " CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
    SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W4")
  ELSE (MSVC)
    SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${H5_CFLAGS3}")
  ENDIF (MSVC)
ENDIF (HDF5_ENABLE_GROUPTHREE_WARNINGS)

#-----------------------------------------------------------------------------
# Option to allow the user to enable warnings by groups
#-----------------------------------------------------------------------------
OPTION (HDF5_ENABLE_GROUPFOUR_WARNINGS "Enable group four warnings" OFF)
IF (HDF5_ENABLE_GROUPFOUR_WARNINGS)
  IF (NOT MSVC)
    SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${H5_CFLAGS4}")
  ENDIF (NOT MSVC)
ENDIF (HDF5_ENABLE_GROUPFOUR_WARNINGS)

#-----------------------------------------------------------------------------
# Option to allow the user to enable warnings by groups
#-----------------------------------------------------------------------------
OPTION (HDF5_ENABLE_GROUPFIVE_WARNINGS "Enable group five warnings" OFF)
IF (HDF5_ENABLE_GROUPFIVE_WARNINGS)
  IF (NOT MSVC)
    SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${H5_CFLAGS5}")
  ENDIF (NOT MSVC)
ENDIF (HDF5_ENABLE_GROUPFIVE_WARNINGS)

#-----------------------------------------------------------------------------
# This is in here to help some of the GCC based IDES like Eclipse
# and code blocks parse the compiler errors and warnings better.
#-----------------------------------------------------------------------------
IF (CMAKE_COMPILER_IS_GNUCC)
  SET (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fmessage-length=0")
ENDIF (CMAKE_COMPILER_IS_GNUCC)
IF (CMAKE_COMPILER_IS_GNUCXX)
  SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fmessage-length=0")
ENDIF (CMAKE_COMPILER_IS_GNUCXX)

#-----------------------------------------------------------------------------
# All libs/tests/examples need the main include directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_BINARY_DIR} ${HDF5_SRC_DIR} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})

#-----------------------------------------------------------------------------
# Option to Enable MPI Parallel
#-----------------------------------------------------------------------------
SET (CMAKE_MODULE_PATH ${HDF5_RESOURCES_DIR} ${CMAKE_MODULE_PATH})
OPTION (HDF5_ENABLE_PARALLEL "Enable parallel build (requires MPI)" OFF)
IF (HDF5_ENABLE_PARALLEL)
  INCLUDE (FindMPI)
  INCLUDE_DIRECTORIES (${MPI_C_INCLUDE_PATH})
  IF (MPI_C_FOUND)
    SET (H5_HAVE_PARALLEL 1)
    # MPI checks, only do these if MPI_C_FOUND is true, otherwise they always fail
    # and once set, they are cached as false and not regenerated
    SET (CMAKE_REQUIRED_LIBRARIES "${MPI_C_LIBRARIES}" )
    CHECK_FUNCTION_EXISTS (MPI_File_get_size H5_HAVE_MPI_GET_SIZE) 
    SET (H5_MPI_FILE_SET_SIZE_BIG 1)
    # Used by Fortran + MPI
    CHECK_SYMBOL_EXISTS (MPI_Comm_c2f "${MPI_C_INCLUDE_PATH}/mpi.h"  H5_HAVE_MPI_MULTI_LANG_Comm)
    CHECK_SYMBOL_EXISTS (MPI_Info_c2f "${MPI_C_INCLUDE_PATH}/mpi.h"  H5_HAVE_MPI_MULTI_LANG_Info)
  ELSE (MPI_C_FOUND)
    MESSAGE (STATUS "Parallel libraries not found")
  ENDIF (MPI_C_FOUND)
ENDIF (HDF5_ENABLE_PARALLEL)

# Parallel IO usage requires MPI to be Linked and Included
IF (H5_HAVE_PARALLEL)
  SET (LINK_LIBS ${LINK_LIBS} ${MPI_C_LIBRARIES})
  IF (MPI_C_LINK_FLAGS)
    SET (CMAKE_EXE_LINKER_FLAGS ${MPI_C_LINK_FLAGS} ${CMAKE_EXE_LINKER_FLAGS})
  ENDIF (MPI_C_LINK_FLAGS)
ENDIF (H5_HAVE_PARALLEL)

SET (DEFAULT_API_VERSION "v110")
#-----------------------------------------------------------------------------
# Option to use 1.6.x API
#-----------------------------------------------------------------------------
OPTION (HDF5_USE_16_API_DEFAULT "Use the HDF5 1.6.x API by default" OFF)
SET (H5_USE_16_API_DEFAULT 0)
IF (HDF5_USE_16_API_DEFAULT)
  SET (H5_USE_16_API_DEFAULT 1)
  SET (DEFAULT_API_VERSION "v16")
ENDIF (HDF5_USE_16_API_DEFAULT)

#-----------------------------------------------------------------------------
# Option to use 1.8.x API
#-----------------------------------------------------------------------------
OPTION (HDF5_USE_18_API_DEFAULT "Use the HDF5 1.8.x API by default" OFF)
SET (H5_USE_18_API_DEFAULT 0)
IF (HDF5_USE_18_API_DEFAULT)
  SET (H5_USE_18_API_DEFAULT 1)
  SET (DEFAULT_API_VERSION "v18")
ENDIF (HDF5_USE_18_API_DEFAULT)

#-----------------------------------------------------------------------------
# Include user macros
#-----------------------------------------------------------------------------
INCLUDE (UserMacros.cmake)

INCLUDE (CMakeFilters.cmake)

#-----------------------------------------------------------------------------
# Option for external libraries on windows
#-----------------------------------------------------------------------------
OPTION (HDF5_PACKAGE_EXTLIBS "CPACK - include external libraries" OFF)
IF (NOT HDF5_EXTERNALLY_CONFIGURED)
  IF (HDF5_PACKAGE_EXTLIBS)
    SET (HDF5_NO_PACKAGES OFF CACHE BOOL "CPACK - Disable packaging" FORCE)
    IF (HDF5_ENABLE_Z_LIB_SUPPORT AND ZLIB_FOUND)
      PACKAGE_ZLIB_LIBRARY (${HDF5_ALLOW_EXTERNAL_SUPPORT})
    ENDIF (HDF5_ENABLE_Z_LIB_SUPPORT AND ZLIB_FOUND)
    
    IF (HDF5_ENABLE_SZIP_SUPPORT AND SZIP_FOUND)
      PACKAGE_SZIP_LIBRARY (${HDF5_ALLOW_EXTERNAL_SUPPORT})
    ENDIF (HDF5_ENABLE_SZIP_SUPPORT AND SZIP_FOUND)
  ENDIF (HDF5_PACKAGE_EXTLIBS)
ENDIF (NOT HDF5_EXTERNALLY_CONFIGURED)

#-----------------------------------------------------------------------------
# Option to use threadsafe
# Note: Currently CMake only allows configuring of threadsafe on WINDOWS.
#-----------------------------------------------------------------------------
IF (WIN32 AND NOT CYGWIN)
  OPTION (HDF5_ENABLE_THREADSAFE "Enable Threadsafety" OFF)
  IF (HDF5_ENABLE_THREADSAFE)
    # check for unsupported options
    IF (HDF5_ENABLE_PARALLEL)
      MESSAGE (FATAL_ERROR " **** Parallel and Threadsafe options are mutually exclusive **** ")
    ENDIF (HDF5_ENABLE_PARALLEL)
    SET (H5_HAVE_THREADSAFE 1)
    IF (H5_HAVE_IOEO)
      MESSAGE (STATUS " **** Windows Threads only available in WINVER>=0x600 (Vista or Windows 7) **** ")
      SET (H5_HAVE_WIN_THREADS 1)
    ELSE (H5_HAVE_IOEO)
      IF (NOT H5_HAVE_PTHREAD_H)
        SET (H5_HAVE_THREADSAFE 0)
        MESSAGE (FATAL_ERROR " **** Threadsafe option requires thread library **** ")
      ENDIF (NOT H5_HAVE_PTHREAD_H)
    ENDIF (H5_HAVE_IOEO)
  ENDIF (HDF5_ENABLE_THREADSAFE)
ENDIF (WIN32 AND NOT CYGWIN)

# -----------------------------------------------------------------------
# wrapper script variables
# 
#SET (CFLAGS "${C_DEFINES}")
#SET (CXXFLAGS "${CXX_DEFINES}")

#-----------------------------------------------------------------------------
# Add the HDF5 Library Target to the build
#-----------------------------------------------------------------------------
ADD_SUBDIRECTORY (${HDF5_SOURCE_DIR}/src ${PROJECT_BINARY_DIR}/src)

IF (HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "SVN" OR HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "TGZ")
  IF (ZLIB_FOUND AND ZLIB_USE_EXTERNAL)
    ADD_DEPENDENCIES (${HDF5_LIB_TARGET} ZLIB)
  ENDIF (ZLIB_FOUND AND ZLIB_USE_EXTERNAL)
  IF (SZIP_FOUND AND SZIP_USE_EXTERNAL)
    ADD_DEPENDENCIES (${HDF5_LIB_TARGET} SZIP)
  ENDIF (SZIP_FOUND AND SZIP_USE_EXTERNAL)
ENDIF (HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "SVN" OR HDF5_ALLOW_EXTERNAL_SUPPORT MATCHES "TGZ")

#-----------------------------------------------------------------------------
# Build utility to copy and strip X lines of file
#-----------------------------------------------------------------------------
IF (HDF5_BUILD_TOOLS AND BUILD_TESTING)
  SET (XLATE_UTILITY "xlatefile")
  ADD_EXECUTABLE(${XLATE_UTILITY} ${HDF5_RESOURCES_DIR}/xlatefile.c)
ENDIF (HDF5_BUILD_TOOLS AND BUILD_TESTING)

#-----------------------------------------------------------------------------
# Dashboard and Testing Settings
#-----------------------------------------------------------------------------
OPTION (BUILD_TESTING "Build HDF5 Unit Testing" OFF)
IF (BUILD_TESTING)
  SET (DART_TESTING_TIMEOUT 1200
      CACHE INTEGER
      "Timeout in seconds for each test (default 1200=20minutes)"
  )
  ENABLE_TESTING ()
  INCLUDE (CTest)
  IF (NOT HDF5_EXTERNALLY_CONFIGURED)
    IF (EXISTS "${HDF5_SOURCE_DIR}/test" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/test")
      ADD_SUBDIRECTORY (${HDF5_SOURCE_DIR}/tools/lib ${PROJECT_BINARY_DIR}/tools/lib)
      ADD_SUBDIRECTORY (${HDF5_SOURCE_DIR}/test      ${PROJECT_BINARY_DIR}/test)
    ENDIF (EXISTS "${HDF5_SOURCE_DIR}/test" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/test")
    IF (EXISTS "${HDF5_SOURCE_DIR}/perform" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/perform")
      ADD_SUBDIRECTORY (${HDF5_SOURCE_DIR}/perform   ${PROJECT_BINARY_DIR}/perform)
    ENDIF(EXISTS "${HDF5_SOURCE_DIR}/perform" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/perform")
    IF (H5_HAVE_PARALLEL)
      IF (EXISTS "${HDF5_SOURCE_DIR}/testpar" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/testpar")
        ADD_SUBDIRECTORY (${HDF5_SOURCE_DIR}/testpar ${PROJECT_BINARY_DIR}/testpar)
      ENDIF (EXISTS "${HDF5_SOURCE_DIR}/testpar" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/testpar")
    ENDIF (H5_HAVE_PARALLEL)
  ENDIF (NOT HDF5_EXTERNALLY_CONFIGURED)

  OPTION (HDF5_TEST_VFD "Execute tests with different VFDs" OFF)
  MARK_AS_ADVANCED (HDF5_TEST_VFD)
  IF (HDF5_TEST_VFD)
    OPTION (HDF5_TEST_FHEAP_VFD "Execute tests with different VFDs" ON)
    MARK_AS_ADVANCED (HDF5_TEST_FHEAP_VFD)
  ENDIF (HDF5_TEST_VFD)
  
  INCLUDE (${HDF5_SOURCE_DIR}/CTestConfig.cmake)
  CONFIGURE_FILE (${HDF5_RESOURCES_DIR}/CTestCustom.cmake ${HDF5_BINARY_DIR}/CTestCustom.ctest @ONLY)
ENDIF (BUILD_TESTING)

#-----------------------------------------------------------------------------
# Option to build Fortran bindings/tests/examples
# Make sure this appears before the CONFIGURE_FILE step
# so that fortran name mangling is detected before writing H5pubconf.h
#-----------------------------------------------------------------------------
# Set default name mangling : overridden by Fortran detection in fortran dir
SET (H5_FC_FUNC  "H5_FC_FUNC(name,NAME) name ## _")
SET (H5_FC_FUNC_ "H5_FC_FUNC_(name,NAME) name ## _")
IF (EXISTS "${HDF5_SOURCE_DIR}/fortran" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/fortran")
  OPTION (HDF5_BUILD_FORTRAN "Build FORTRAN support" OFF)
  IF (HDF5_BUILD_FORTRAN)
    IF (BUILD_SHARED_LIBS AND APPLE)
      MESSAGE (FATAL_ERROR " **** Shared FORTRAN libraries are unsupported **** ")
    ENDIF (BUILD_SHARED_LIBS AND APPLE)
    OPTION (HDF5_ENABLE_F2003 "Enable FORTRAN 2003 Standard" OFF)
    INCLUDE (${HDF5_RESOURCES_DIR}/HDF5UseFortran.cmake)
    IF (HDF5_ENABLE_F2003)
      IF (NOT FORTRAN_HAVE_ISO_C_BINDING)
        SET (HDF5_ENABLE_F2003 OFF)
      ENDIF (NOT FORTRAN_HAVE_ISO_C_BINDING)
    ENDIF (HDF5_ENABLE_F2003)

    # -----------------------------------------------------------------------
    # wrapper script variables
    # 
#    SET (FCFLAGS "${Fortran_DEFINES}")

    ADD_SUBDIRECTORY (${HDF5_SOURCE_DIR}/fortran ${PROJECT_BINARY_DIR}/fortran)
    IF (HDF5_BUILD_HL_LIB)
      IF (EXISTS "${HDF5_SOURCE_DIR}/hl/fortran" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/hl/fortran")
        #-- Build the High Level Fortran source codes
        ADD_SUBDIRECTORY (${HDF5_SOURCE_DIR}/hl/fortran ${PROJECT_BINARY_DIR}/hl/fortran)
      ENDIF (EXISTS "${HDF5_SOURCE_DIR}/hl/fortran" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/hl/fortran")
    ENDIF (HDF5_BUILD_HL_LIB)
  ENDIF (HDF5_BUILD_FORTRAN)
ENDIF (EXISTS "${HDF5_SOURCE_DIR}/fortran" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/fortran")

#-----------------------------------------------------------------------------
# Option to build examples
#-----------------------------------------------------------------------------
IF (EXISTS "${HDF5_SOURCE_DIR}/examples" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/examples")
  OPTION (HDF5_BUILD_EXAMPLES  "Build HDF5 Library Examples" OFF)
  IF (HDF5_BUILD_EXAMPLES)
    ADD_SUBDIRECTORY (${HDF5_SOURCE_DIR}/examples ${PROJECT_BINARY_DIR}/examples)
  ENDIF (HDF5_BUILD_EXAMPLES)
ENDIF (EXISTS "${HDF5_SOURCE_DIR}/examples" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/examples")

#-----------------------------------------------------------------------------
# Option to build HDF5 C++ Library
#-----------------------------------------------------------------------------
IF (EXISTS "${HDF5_SOURCE_DIR}/c++" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/c++")
  OPTION (HDF5_BUILD_CPP_LIB  "Build HDF5 C++ Library" OFF)
  IF (HDF5_BUILD_CPP_LIB)
    # check for unsupported options
    IF (HDF5_ENABLE_PARALLEL)
      MESSAGE (FATAL_ERROR " **** Parallel and C++ options are mutually exclusive **** ")
    ENDIF (HDF5_ENABLE_PARALLEL)
    IF (CMAKE_NO_STD_NAMESPACE)
      SET (H5_NO_STD 1)
    ENDIF (CMAKE_NO_STD_NAMESPACE)
    ADD_SUBDIRECTORY (${HDF5_SOURCE_DIR}/c++ ${PROJECT_BINARY_DIR}/c++)
  ENDIF (HDF5_BUILD_CPP_LIB)
ENDIF (EXISTS "${HDF5_SOURCE_DIR}/c++" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/c++")

#-----------------------------------------------------------------------------
# Option to build HDF5 Tools
#-----------------------------------------------------------------------------
IF (EXISTS "${HDF5_SOURCE_DIR}/tools" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/tools")
  OPTION (HDF5_BUILD_TOOLS  "Build HDF5 Tools" OFF)
  IF (HDF5_BUILD_TOOLS)
    ADD_SUBDIRECTORY (${HDF5_SOURCE_DIR}/tools ${PROJECT_BINARY_DIR}/tools)
  ENDIF (HDF5_BUILD_TOOLS)
ENDIF (EXISTS "${HDF5_SOURCE_DIR}/tools" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/tools")

#-----------------------------------------------------------------------------
# Option to build High Level API's
#-----------------------------------------------------------------------------
IF (EXISTS "${HDF5_SOURCE_DIR}/hl" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/hl")
  OPTION (HDF5_BUILD_HL_LIB  "Build HIGH Level HDF5 Library" OFF)
  IF (HDF5_BUILD_HL_LIB)
    SET (H5_INCLUDE_HL 1)
    ADD_SUBDIRECTORY (${HDF5_SOURCE_DIR}/hl ${PROJECT_BINARY_DIR}/hl)
  ENDIF (HDF5_BUILD_HL_LIB)
ENDIF (EXISTS "${HDF5_SOURCE_DIR}/hl" AND IS_DIRECTORY "${HDF5_SOURCE_DIR}/hl")

#-----------------------------------------------------------------------------
# Generate the H5pubconf.h file containing user settings needed by compilation
#-----------------------------------------------------------------------------
CONFIGURE_FILE (${HDF5_RESOURCES_DIR}/H5pubconf.h.in ${HDF5_BINARY_DIR}/H5pubconf.h @ONLY)

INCLUDE (CMakeInstallation.cmake)
