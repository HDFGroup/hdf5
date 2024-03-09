#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#

#-------------------------------------------------------------------------------
# Setup output Directories
#-----------------------------------------------------------------------------
macro (SET_HDF_OUTPUT_DIRS package_prefix)
  if (NOT ${package_prefix}_EXTERNALLY_CONFIGURED)
    set (CMAKE_RUNTIME_OUTPUT_DIRECTORY
        ${PROJECT_BINARY_DIR}/bin CACHE PATH "Single Directory for all Executables."
    )
    set (CMAKE_LIBRARY_OUTPUT_DIRECTORY
        ${PROJECT_BINARY_DIR}/bin CACHE PATH "Single Directory for all Libraries"
    )
    set (CMAKE_ARCHIVE_OUTPUT_DIRECTORY
        ${PROJECT_BINARY_DIR}/bin CACHE PATH "Single Directory for all static libraries."
    )
    set (CMAKE_Fortran_MODULE_DIRECTORY
        ${PROJECT_BINARY_DIR}/mod CACHE PATH "Single Directory for all fortran modules."
    )
    get_property(_isMultiConfig GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
    if(_isMultiConfig)
      set (CMAKE_TEST_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${HDF_CFG_NAME})
      set (CMAKE_PDB_OUTPUT_DIRECTORY
          ${PROJECT_BINARY_DIR}/bin CACHE PATH "Single Directory for all pdb files."
      )
    else ()
      set (CMAKE_TEST_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
    endif ()
  else ()
    # if we are externally configured, but the project uses old cmake scripts
    # this may not be set and utilities like H5detect will fail
    if (NOT CMAKE_RUNTIME_OUTPUT_DIRECTORY)
      set (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${EXECUTABLE_OUTPUT_PATH})
    endif ()
  endif ()
endmacro ()

#-------------------------------------------------------------------------------
macro (SET_HDF_BUILD_TYPE)
  get_property (_isMultiConfig GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
  if (_isMultiConfig)
    # HDF_CFG_BUILD_TYPE is used in the Fortran install commands for the build location of the .mod files
    set (HDF_CFG_BUILD_TYPE \${CMAKE_INSTALL_CONFIG_NAME})
    if (CMAKE_BUILD_TYPE)
      # set the default to the specified command line define
      set (HDF_CFG_NAME ${CMAKE_BUILD_TYPE})
    else ()
      # set the default to the MultiConfig variable
      set (HDF_CFG_NAME "$<CONFIG>")
    endif ()
  else ()
    set (HDF_CFG_BUILD_TYPE ".")
    if (CMAKE_BUILD_TYPE)
      set (HDF_CFG_NAME ${CMAKE_BUILD_TYPE})
    else ()
      set (HDF_CFG_NAME "Release")
    endif ()
  endif ()
endmacro ()

#-------------------------------------------------------------------------------
macro (TARGET_C_PROPERTIES wintarget libtype)
  target_compile_options(${wintarget} PRIVATE
      "$<$<C_COMPILER_ID:MSVC>:${WIN_COMPILE_FLAGS}>"
      "$<$<CXX_COMPILER_ID:MSVC>:${WIN_COMPILE_FLAGS}>"
  )
  if(MSVC)
    set_property(TARGET ${wintarget} APPEND PROPERTY LINK_FLAGS "${WIN_LINK_FLAGS}")
  endif()
endmacro ()

macro (HDFTEST_COPY_FILE src dest target)
    add_custom_command(
        OUTPUT  "${dest}"
        COMMAND "${CMAKE_COMMAND}"
        ARGS     -E copy_if_different "${src}" "${dest}"
        DEPENDS "${src}"
    )
    list (APPEND ${target}_list "${dest}")
endmacro ()

macro (HDF_DIR_PATHS package_prefix)
  option (H5EX_USE_GNU_DIRS "ON to use GNU Coding Standard install directory variables, OFF to use historical settings" OFF)
  if (H5EX_USE_GNU_DIRS)
    include(GNUInstallDirs)
    if (NOT ${package_prefix}_INSTALL_BIN_DIR)
      set (${package_prefix}_INSTALL_BIN_DIR ${CMAKE_INSTALL_BINDIR})
    endif ()
    if (NOT ${package_prefix}_INSTALL_LIB_DIR)
      set (${package_prefix}_INSTALL_LIB_DIR ${CMAKE_INSTALL_LIBDIR})
    endif ()
    if (NOT ${package_prefix}_INSTALL_JAR_DIR)
      set (${package_prefix}_INSTALL_JAR_DIR ${CMAKE_INSTALL_LIBDIR})
    endif ()
    if (NOT ${package_prefix}_INSTALL_INCLUDE_DIR)
      set (${package_prefix}_INSTALL_INCLUDE_DIR ${CMAKE_INSTALL_INCLUDEDIR})
    endif ()
    if (NOT ${package_prefix}_INSTALL_MODULE_DIR)
      set (${package_prefix}_INSTALL_MODULE_DIR ${CMAKE_INSTALL_INCLUDEDIR}/mod)
    endif ()
    if (NOT ${package_prefix}_INSTALL_DATA_DIR)
      set (${package_prefix}_INSTALL_DATA_DIR ${CMAKE_INSTALL_DATADIR})
    endif ()
    if (NOT ${package_prefix}_INSTALL_CMAKE_DIR)
      set (${package_prefix}_INSTALL_CMAKE_DIR ${CMAKE_INSTALL_LIBDIR}/cmake)
    endif ()
    if (NOT ${package_prefix}_INSTALL_DOC_DIR)
      set (${package_prefix}_INSTALL_DOC_DIR ${CMAKE_INSTALL_DOCDIR})
    endif ()
    message(STATUS "GNU: ${${package_prefix}_INSTALL_DOC_DIR}")
  endif ()

  if (APPLE)
    option (${package_prefix}_BUILD_FRAMEWORKS "ON to build as frameworks libraries, OFF to build according to BUILD_SHARED_LIBS" OFF)
  endif ()

  if (NOT ${package_prefix}_INSTALL_BIN_DIR)
    set (${package_prefix}_INSTALL_BIN_DIR bin)
  endif ()
  if (NOT ${package_prefix}_INSTALL_LIB_DIR)
    if (APPLE)
      if (${package_prefix}_BUILD_FRAMEWORKS)
        set (${package_prefix}_INSTALL_JAR_DIR ../Java)
      else ()
        set (${package_prefix}_INSTALL_JAR_DIR lib)
      endif ()
      set (${package_prefix}_INSTALL_FMWK_DIR ${CMAKE_INSTALL_FRAMEWORK_PREFIX})
    else ()
      set (${package_prefix}_INSTALL_JAR_DIR lib)
    endif ()
    set (${package_prefix}_INSTALL_LIB_DIR lib)
  endif ()
  if (NOT ${package_prefix}_INSTALL_INCLUDE_DIR)
    set (${package_prefix}_INSTALL_INCLUDE_DIR include)
  endif ()
  if (NOT ${package_prefix}_INSTALL_MODULE_DIR)
    set (${package_prefix}_INSTALL_MODULE_DIR mod)
  endif ()
  if (NOT ${package_prefix}_INSTALL_DATA_DIR)
    if (NOT MSVC)
      if (APPLE)
        if (${package_prefix}_BUILD_FRAMEWORKS)
          set (${package_prefix}_INSTALL_EXTRA_DIR ../SharedSupport)
        else ()
          set (${package_prefix}_INSTALL_EXTRA_DIR share)
        endif ()
        set (${package_prefix}_INSTALL_FWRK_DIR ${CMAKE_INSTALL_FRAMEWORK_PREFIX})
      endif ()
      set (${package_prefix}_INSTALL_DATA_DIR share)
    else ()
      set (${package_prefix}_INSTALL_DATA_DIR ".")
    endif ()
  endif ()
  if (NOT ${package_prefix}_INSTALL_CMAKE_DIR)
    set (${package_prefix}_INSTALL_CMAKE_DIR cmake)
  endif ()
  if (NOT ${package_prefix}_INSTALL_DOC_DIR)
    set (${package_prefix}_INSTALL_DOC_DIR ${${package_prefix}_INSTALL_DATA_DIR})
  endif ()
  message(STATUS "Final: ${${package_prefix}_INSTALL_DOC_DIR}")

  # Always use full RPATH, i.e. don't skip the full RPATH for the build tree
  set (CMAKE_SKIP_BUILD_RPATH  OFF)
  # when building, don't use the install RPATH already
  # (but later on when installing)
  set (CMAKE_INSTALL_RPATH_USE_LINK_PATH  OFF)
  # add the automatically determined parts of the RPATH
  # which point to directories outside the build tree to the install RPATH
  set (CMAKE_BUILD_WITH_INSTALL_RPATH ON)
  if (APPLE)
    set (CMAKE_INSTALL_NAME_DIR "@rpath")
    set (CMAKE_INSTALL_RPATH
        "@executable_path/../${${package_prefix}_INSTALL_LIB_DIR}"
        "@executable_path/"
        "@loader_path/../${${package_prefix}_INSTALL_LIB_DIR}"
        "@loader_path/"
    )
  else ()
    set (CMAKE_INSTALL_RPATH "\$ORIGIN/../${${package_prefix}_INSTALL_LIB_DIR}:\$ORIGIN/")
  endif ()

  if (DEFINED ADDITIONAL_CMAKE_PREFIX_PATH AND EXISTS "${ADDITIONAL_CMAKE_PREFIX_PATH}")
    set (CMAKE_PREFIX_PATH ${ADDITIONAL_CMAKE_PREFIX_PATH} ${CMAKE_PREFIX_PATH})
  endif ()

  #set the default debug suffix for all library targets
  if(NOT CMAKE_DEBUG_POSTFIX)
    if (WIN32)
      set (CMAKE_DEBUG_POSTFIX "_D")
    else ()
      set (CMAKE_DEBUG_POSTFIX "_debug")
    endif ()
  endif ()

  SET_HDF_BUILD_TYPE()

  SET_HDF_OUTPUT_DIRS(${package_prefix})

  include (FetchContent)
endmacro ()

macro (ADD_H5_FLAGS h5_flag_var infile)
  file (STRINGS ${infile} TEST_FLAG_STREAM)
  list (LENGTH TEST_FLAG_STREAM len_flag)
  if (len_flag GREATER 0)
    math (EXPR _FP_LEN "${len_flag} - 1")
    foreach (line RANGE 0 ${_FP_LEN})
      list (GET TEST_FLAG_STREAM ${line} str_flag)
      string (REGEX REPLACE "^#.*" "" str_flag "${str_flag}")
      if (str_flag)
        list (APPEND ${h5_flag_var} "${str_flag}")
      endif ()
    endforeach ()
  endif ()
endmacro ()
