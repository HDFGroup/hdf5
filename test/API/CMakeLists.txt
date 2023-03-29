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

#------------------------------------------------------------------------------
# Set module path
#------------------------------------------------------------------------------
set(HDF5_TEST_API_CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/CMake")
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${HDF5_TEST_API_CMAKE_MODULE_PATH})

# TODO: probably not necessary
#------------------------------------------------------------------------------
# Setup CMake Environment
#------------------------------------------------------------------------------
if (WIN32)
  message("The HDF5 API test suite is currently not supported on this platform." FATAL_ERROR)
endif ()

#------------------------------------------------------------------------------
# Setup testing configuration file
#------------------------------------------------------------------------------
if (HDF5_TEST_PARALLEL)
  set (HDF5_TEST_API_HAVE_PARALLEL 1)
endif ()
if (HDF5_TEST_API_ENABLE_ASYNC)
  set (HDF5_TEST_API_HAVE_ASYNC 1)
endif ()

configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/H5_api_test_config.h.in
  ${HDF5_TEST_BINARY_DIR}/H5_api_test_config.h
)

#------------------------------------------------------------------------------
# Compile kwsys library and setup TestDriver
#------------------------------------------------------------------------------
if (HDF5_TEST_API_ENABLE_DRIVER)
  add_subdirectory (driver)
endif ()

#------------------------------------------------------------------------------
# Setup for API tests
#------------------------------------------------------------------------------

# Ported HDF5 tests
set (HDF5_API_TESTS_EXTRA
  testhdf5
)

#-----------------------------------------------------------------------------
# Build the main API test executable
#-----------------------------------------------------------------------------
foreach (api_test ${HDF5_API_TESTS})
  set (HDF5_API_TEST_SRCS
    ${HDF5_API_TEST_SRCS}
    ${CMAKE_CURRENT_SOURCE_DIR}/H5_api_${api_test}_test.c
  )
endforeach ()

set (HDF5_API_TEST_SRCS
  ${HDF5_API_TEST_SRCS}
  ${CMAKE_CURRENT_SOURCE_DIR}/H5_api_test.c
  ${HDF5_TEST_API_SRC_DIR}/H5_api_test_util.c
)

add_executable (h5_api_test ${HDF5_API_TEST_SRCS})
target_include_directories (
  h5_api_test
  PRIVATE
    "${HDF5_SRC_INCLUDE_DIRS}"
    "${HDF5_TEST_SRC_DIR}"
    "${HDF5_TEST_API_SRC_DIR}"
    "${HDF5_SRC_BINARY_DIR}"
    "${HDF5_TEST_BINARY_DIR}"
)
target_compile_options (
  h5_api_test
  PRIVATE
    "${HDF5_CMAKE_C_FLAGS}"
)
target_compile_definitions (
  h5_api_test
  PRIVATE
    $<$<CONFIG:Developer>:${HDF5_DEVELOPER_DEFS}>
)
if (NOT BUILD_SHARED_LIBS)
  TARGET_C_PROPERTIES (h5_api_test STATIC)
  target_link_libraries (
    h5_api_test
    PRIVATE
      ${HDF5_TEST_LIB_TARGET}
  )
else ()
  TARGET_C_PROPERTIES (h5_api_test SHARED)
  target_link_libraries (
    h5_api_test
    PRIVATE
      ${HDF5_TEST_LIBSH_TARGET}
  )
endif ()
set_target_properties (
  h5_api_test
  PROPERTIES
    FOLDER test/API
)
# Add Target to clang-format
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_TEST_h5_api_test_FORMAT h5_api_test)
endif ()

#-----------------------------------------------------------------------------
# Build the ported HDF5 test executables
#-----------------------------------------------------------------------------
foreach (api_test_extra ${HDF5_API_TESTS_EXTRA})
  set (HDF5_API_TEST_EXTRA_SRCS
    ${HDF5_API_TEST_EXTRA_SRCS}
    ${CMAKE_CURRENT_SOURCE_DIR}/${api_test_extra}.c
  )

  if (${api_test_extra} STREQUAL "testhdf5")
    set (HDF5_API_TEST_EXTRA_SRCS
      ${HDF5_API_TEST_EXTRA_SRCS}
      ${CMAKE_CURRENT_SOURCE_DIR}/tarray.c
      ${CMAKE_CURRENT_SOURCE_DIR}/tattr.c
      ${CMAKE_CURRENT_SOURCE_DIR}/tchecksum.c
      ${CMAKE_CURRENT_SOURCE_DIR}/tconfig.c
      ${CMAKE_CURRENT_SOURCE_DIR}/tcoords.c
      ${CMAKE_CURRENT_SOURCE_DIR}/tfile.c
      ${CMAKE_CURRENT_SOURCE_DIR}/tgenprop.c
      ${CMAKE_CURRENT_SOURCE_DIR}/th5o.c
      ${CMAKE_CURRENT_SOURCE_DIR}/th5s.c
      ${CMAKE_CURRENT_SOURCE_DIR}/tid.c
      ${CMAKE_CURRENT_SOURCE_DIR}/titerate.c
      ${CMAKE_CURRENT_SOURCE_DIR}/tmisc.c
      ${CMAKE_CURRENT_SOURCE_DIR}/trefer.c
      ${CMAKE_CURRENT_SOURCE_DIR}/tselect.c
      ${CMAKE_CURRENT_SOURCE_DIR}/ttime.c
      ${CMAKE_CURRENT_SOURCE_DIR}/tunicode.c
      ${CMAKE_CURRENT_SOURCE_DIR}/tvlstr.c
      ${CMAKE_CURRENT_SOURCE_DIR}/tvltypes.c
    )
  endif ()

  add_executable (h5_api_test_${api_test_extra} ${HDF5_API_TEST_EXTRA_SRCS})
  target_include_directories (
    h5_api_test_${api_test_extra}
    PRIVATE
      "${HDF5_SRC_INCLUDE_DIRS}"
      "${HDF5_TEST_SRC_DIR}"
      "${HDF5_TEST_API_SRC_DIR}"
      "${HDF5_SRC_BINARY_DIR}"
      "${HDF5_TEST_BINARY_DIR}"
  )
  target_compile_options (
    h5_api_test_${api_test_extra}
    PRIVATE
      "${HDF5_CMAKE_C_FLAGS}"
  )
  target_compile_definitions (
    h5_api_test_${api_test_extra}
    PRIVATE
      $<$<CONFIG:Developer>:${HDF5_DEVELOPER_DEFS}>
  )
  if (NOT BUILD_SHARED_LIBS)
    TARGET_C_PROPERTIES (h5_api_test_${api_test_extra} STATIC)
    target_link_libraries (h5_api_test_${api_test_extra} PRIVATE ${HDF5_TEST_LIB_TARGET})
  else ()
    TARGET_C_PROPERTIES (h5_api_test_${api_test_extra} SHARED)
    target_link_libraries (h5_api_test_${api_test_extra} PRIVATE ${HDF5_TEST_LIBSH_TARGET})
  endif ()
  set_target_properties (
    h5_api_test_${api_test_extra}
    PROPERTIES
      FOLDER test/API
  )
  # Add Target to clang-format
  if (HDF5_ENABLE_FORMATTERS)
    clang_format (HDF5_TEST_h5_api_test_${api_test_extra}_FORMAT h5_api_test_${api_test_extra})
  endif ()
endforeach ()

#-----------------------------------------------------------------------------
# Add tests if HDF5 serial testing is enabled
#-----------------------------------------------------------------------------
if (HDF5_TEST_SERIAL)
  # List of files generated by the HDF5 API tests which
  # should be cleaned up in case the test failed to remove
  # them
  set (HDF5_API_TESTS_FILES
    h5_api_test.h5
  )

  if (HDF5_TEST_API_ENABLE_DRIVER)

  else ()

  endif ()
endif ()
