cmake_minimum_required (VERSION 3.18)
project (H5EXAMPLES C)

#-----------------------------------------------------------------------------
# Define some CMake variables for use later in the project
#-----------------------------------------------------------------------------
set (H5EX_C_SRC_DIR          ${H5EXAMPLES_SOURCE_DIR}/C)
set (H5EX_CXX_SRC_DIR        ${H5EXAMPLES_SOURCE_DIR}/CXX)
set (H5EX_F90_SRC_DIR        ${H5EXAMPLES_SOURCE_DIR}/FORTRAN)
set (H5EX_JAVA_DIR           ${H5EXAMPLES_SOURCE_DIR}/JAVA)
#-----------------------------------------------------------------------------
# Basic HDF5Examples stuff here
#-----------------------------------------------------------------------------
if (NOT EXAMPLES_EXTERNALLY_CONFIGURED)
  set (H5EX_RESOURCES_DIR      ${H5EXAMPLES_SOURCE_DIR}/config/cmake)

  include (${H5EX_RESOURCES_DIR}/HDFMacros.cmake)
  include (${H5EX_RESOURCES_DIR}/HDFExampleMacros.cmake)
  set (CMAKE_JAVA_INCLUDE_PATH "")

  SET_HDF_BUILD_TYPE()

  BASIC_SETTINGS (EX)

  #-----------------------------------------------------------------------------
  # HDF5 support
  #-----------------------------------------------------------------------------
  HDF5_SUPPORT (TRUE)
  APIVersion(${HDF5_VERSION} H5_LIBVER_DIR)

  #-----------------------------------------------------------------------------
  # Python support check
  #-----------------------------------------------------------------------------
  PYTHON_SUPPORT ()
endif ()
message (STATUS "HDF5 link libs: ${H5EX_HDF5_LINK_LIBS}")
message (STATUS "HDF5 H5_LIBVER_DIR: ${H5_LIBVER_DIR} HDF5_VERSION_MAJOR: ${HDF5_VERSION_MAJOR}")

#-----------------------------------------------------------------------------
# Option to build JAVA examples
#-----------------------------------------------------------------------------
option (H5EX_BUILD_JAVA "Build JAVA support" OFF)
if (H5EX_BUILD_JAVA)
  find_package (Java)

  include (${H5EX_RESOURCES_DIR}/UseJava.cmake)
endif ()

#-----------------------------------------------------------------------------
# parse the full version number from H5public.h and include in H5_VERS_INFO
#-----------------------------------------------------------------------------
if(NOT DEFINED _h5public_h_contents)
  find_file (_h5public_h H5public.h ${H5EX_HDF5_INCLUDE_DIRS})
  file (READ ${_h5public_h} _h5public_h_contents)

  string (REGEX REPLACE ".*#define[ \t]+H5_VERS_MAJOR[ \t]+([0-9]*).*$"
      "\\1" H5_VERS_MAJOR ${_h5public_h_contents})
  string (REGEX REPLACE ".*#define[ \t]+H5_VERS_MINOR[ \t]+([0-9]*).*$"
      "\\1" H5_VERS_MINOR ${_h5public_h_contents})
  string (REGEX REPLACE ".*#define[ \t]+H5_VERS_RELEASE[ \t]+([0-9]*).*$"
      "\\1" H5_VERS_RELEASE ${_h5public_h_contents})
  string (REGEX REPLACE ".*#define[ \t]+H5_VERS_SUBRELEASE[ \t]+\"([0-9A-Za-z._-]*)\".*$"
      "\\1" H5_VERS_SUBRELEASE ${_h5public_h_contents})
endif ()

if (WIN32)
  set(CMAKE_TEST_LIB_DIRECTORY "${HDF5_TOOLS_DIR}")
else ()
  set(CMAKE_TEST_LIB_DIRECTORY "${HDF5_LIBRARY_PATH}")
endif ()

#-----------------------------------------------------------------------------
# Option to Enable MPI Parallel
#-----------------------------------------------------------------------------
set (CMAKE_MODULE_PATH ${H5EX_RESOURCES_DIR} ${CMAKE_MODULE_PATH})
option (H5EX_ENABLE_PARALLEL "Enable parallel build (requires MPI)" OFF)
if (H5EX_ENABLE_PARALLEL)
  find_package(MPI REQUIRED)
  if (MPI_C_FOUND)
    set (H5_HAVE_PARALLEL 1)
    # MPI checks, only do these if MPI_C_FOUND is true, otherwise they always fail
    # and once set, they are cached as false and not regenerated
    set (CMAKE_REQUIRED_LIBRARIES "${MPI_C_LIBRARIES}" )
    # Used by Fortran + MPI
    include (${CMAKE_ROOT}/Modules/CheckSymbolExists.cmake)
    CHECK_SYMBOL_EXISTS (MPI_Comm_c2f "${MPI_C_INCLUDE_DIRS}/mpi.h"  H5_HAVE_MPI_MULTI_LANG_Comm)
    CHECK_SYMBOL_EXISTS (MPI_Info_c2f "${MPI_C_INCLUDE_DIRS}/mpi.h"  H5_HAVE_MPI_MULTI_LANG_Info)
  else ()
    message (WARNING "Parallel libraries not found")
  endif ()
endif ()

# Parallel IO usage requires MPI to be Linked and Included
if (H5_HAVE_PARALLEL)
  set (H5EX_HDF5_LINK_LIBS ${H5EX_HDF5_LINK_LIBS} ${MPI_C_LIBRARIES})
  if (MPI_C_LINK_FLAGS)
    set (CMAKE_EXE_LINKER_FLAGS "${MPI_C_LINK_FLAGS} ${CMAKE_EXE_LINKER_FLAGS}")
  endif ()
  INCLUDE_DIRECTORIES (${MPI_C_INCLUDE_DIRS})
endif ()

set_directory_properties(PROPERTIES INCLUDE_DIRECTORIES 
    "${H5EX_HDF5_INCLUDE_DIRS}"
)

#-----------------------------------------------------------------------------
# Dashboard and Testing Settings
#-----------------------------------------------------------------------------
option (H5EX_BUILD_TESTING "Build HDF5 Example Testing" OFF)
if (H5EX_BUILD_TESTING)
  set (DART_TESTING_TIMEOUT 1200 CACHE STRING
      "Timeout in seconds for each test (default 1200=20minutes)"
  )
  enable_testing ()
  include (CTest)
  include (${PROJECT_SOURCE_DIR}/CTestConfig.cmake)
  configure_file (${H5EX_RESOURCES_DIR}/CTestCustom.cmake ${PROJECT_BINARY_DIR}/CTestCustom.ctest @ONLY)
endif ()

if (${H5_LIBVER_DIR} GREATER 16)
  #-----------------------------------------------------------------------------
  # Option to build Fortran examples
  # Make sure this appears before the CONFIGURE_FILE step
  # so that fortran name mangling is detected before writing H5pubconf.h
  #-----------------------------------------------------------------------------
  # Set default name mangling : overridden by Fortran detection in fortran dir
  set (H5_FC_FUNC  "H5_FC_FUNC(name,NAME) name ## _")
  set (H5_FC_FUNC_ "H5_FC_FUNC_(name,NAME) name ## _")
  if (EXISTS "${H5EXAMPLES_SOURCE_DIR}/FORTRAN" AND IS_DIRECTORY "${H5EXAMPLES_SOURCE_DIR}/FORTRAN")
    option (H5EX_BUILD_FORTRAN "Build examples FORTRAN support" OFF)
    if (H5EX_BUILD_FORTRAN AND HDF5_BUILD_FORTRAN)
      set (H5EX_LINK_Fortran_LIBS ${H5EX_HDF5_LINK_LIBS})

      # Parallel IO usage requires MPI to be Linked and Included
      if (H5_HAVE_PARALLEL)
        set (H5EX_LINK_Fortran_LIBS ${H5EX_LINK_Fortran_LIBS} ${MPI_Fortran_LIBRARIES})
        if (MPI_Fortran_LINK_FLAGS)
          set (CMAKE_Fortran_EXE_LINKER_FLAGS "${MPI_Fortran_LINK_FLAGS} ${CMAKE_EXE_LINKER_FLAGS}")
        endif ()
      endif ()

      configure_file (${H5EX_F90_SRC_DIR}/H5D/h5_version.h.in ${PROJECT_BINARY_DIR}/FORTRAN/H5D/h5_version.h @ONLY)
      configure_file (${H5EX_F90_SRC_DIR}/H5D/h5_version.h.in ${PROJECT_BINARY_DIR}/FORTRAN/H5G/h5_version.h @ONLY)
    else ()
      set (H5EX_BUILD_FORTRAN OFF CACHE BOOL "Build examples FORTRAN support" FORCE)
    endif ()
  else ()
    set (H5EX_BUILD_FORTRAN OFF CACHE BOOL "Build examples FORTRAN support" FORCE)
  endif ()

  if (${H5_LIBVER_DIR} GREATER 18)
    #-----------------------------------------------------------------------------
    # Option to build JAVA examples
    #-----------------------------------------------------------------------------
    if (EXISTS "${H5EXAMPLES_SOURCE_DIR}/JAVA" AND IS_DIRECTORY "${H5EXAMPLES_SOURCE_DIR}/JAVA")
      option (H5EX_BUILD_JAVA "Build examples JAVA support" OFF)
    else ()
      set (H5EX_BUILD_JAVA OFF CACHE BOOL "Build examples JAVA support" FORCE)
    endif ()
  else ()
    set (H5EX_BUILD_JAVA OFF CACHE BOOL "Build examples JAVA support" FORCE)
  endif ()

  #-----------------------------------------------------------------------------
  # Build the CPP Examples
  #-----------------------------------------------------------------------------
  if (EXISTS "${H5EXAMPLES_SOURCE_DIR}/CXX" AND IS_DIRECTORY "${H5EXAMPLES_SOURCE_DIR}/CXX")
    option (H5EX_BUILD_CXX "Build examples C++ support" OFF)
  else ()
    set (H5EX_BUILD_CXX OFF CACHE BOOL "Build examples C++ support" FORCE)
  endif ()

  #-----------------------------------------------------------------------------
  # Option to build filter examples
  #-----------------------------------------------------------------------------
  if (EXISTS "${H5EXAMPLES_SOURCE_DIR}/C/H5FLT" AND IS_DIRECTORY "${H5EXAMPLES_SOURCE_DIR}/C/H5FLT")
    option (H5EX_BUILD_FILTERS "Build examples PLUGIN filter support" OFF)
    if (H5EX_BUILD_FILTERS AND HDF5_ENABLE_PLUGIN_SUPPORT)
      if(DEFINED ENV{HDF5_PLUGIN_PATH})
        message (STATUS "ENV PATH=$ENV{HDF5_PLUGIN_PATH}")
        set (H5EX_HDF5_PLUGIN_PATH $ENV{HDF5_PLUGIN_PATH})
      else ()
        if(NOT DEFINED H5EX_HDF5_PLUGIN_PATH)
          message (STATUS "LIBRARY PATH=${HDF5_LIBRARY_PATH}/plugin")
          set (H5EX_HDF5_PLUGIN_PATH ${HDF5_LIBRARY_PATH}/plugin)
        endif ()
      endif ()
      message (STATUS "H5EX_HDF5_PLUGIN_PATH=${H5EX_HDF5_PLUGIN_PATH}")
    else ()
      set (H5EX_BUILD_FILTERS OFF CACHE BOOL "Build examples PLUGIN filter support" FORCE)
    endif ()
  else ()
    set (H5EX_BUILD_FILTERS OFF CACHE BOOL "Build examples PLUGIN filter support" FORCE)
  endif ()
else ()
  set (H5EX_BUILD_FORTRAN OFF} CACHE BOOL "Build examples FORTRAN support" FORCE)
  set (H5EX_BUILD_JAVA OFF CACHE BOOL "Build examples JAVA support" FORCE)
  set (H5EX_BUILD_CXX OFF CACHE BOOL "Build examples C++ support" FORCE)
  set (H5EX_BUILD_FILTERS OFF CACHE BOOL "Build examples PLUGIN filter support" FORCE)
endif ()

#-----------------------------------------------------------------------------
# Build examples
#-----------------------------------------------------------------------------
add_subdirectory (C)
if (H5EX_BUILD_FORTRAN AND HDF5_BUILD_FORTRAN)
  add_subdirectory (FORTRAN)
endif ()
if (H5EX_BUILD_JAVA AND HDF5_BUILD_JAVA)
  add_subdirectory (JAVA)
endif ()
if (H5EX_BUILD_CXX AND HDF5_BUILD_CPP_LIB)
  add_subdirectory (CXX)
endif ()
if (H5EX_BUILD_PYTHON)
  add_subdirectory (PYTHON)
endif ()

