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

# TODO: rename vol files to "api"

#------------------------------------------------------------------------------
# Set module path
#------------------------------------------------------------------------------
set(HDF5_TEST_API_CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/CMake")
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${HDF5_TEST_API_CMAKE_MODULE_PATH})

#------------------------------------------------------------------------------
# Setup for API tests
#------------------------------------------------------------------------------

# Ported HDF5 tests
set (HDF5_API_PAR_TESTS_EXTRA
  t_bigio
  t_pshutdown
  t_shapesame
  testphdf5
)

#-----------------------------------------------------------------------------
# Build the main API test executable
#-----------------------------------------------------------------------------
foreach (api_test ${HDF5_API_TESTS})
  set (HDF5_API_PAR_TEST_SRCS
    ${HDF5_API_PAR_TEST_SRCS}
    ${CMAKE_CURRENT_SOURCE_DIR}/vol_${api_test}_test_parallel.c
  )
endforeach ()

set (HDF5_API_PAR_TEST_SRCS
  ${HDF5_API_PAR_TEST_SRCS}
  ${CMAKE_CURRENT_SOURCE_DIR}/vol_test_parallel.c
  ${HDF5_TEST_API_SRC_DIR}/vol_test_util.c
)

add_executable (h5_api_test_parallel ${HDF5_API_PAR_TEST_SRCS})
target_include_directories (
  h5_api_test_parallel
  PRIVATE
    "${HDF5_SRC_INCLUDE_DIRS}"
    "${HDF5_TEST_PAR_DIR}"
    "${HDF5_TEST_API_SRC_DIR}"
    "${HDF5_TEST_API_PAR_SRC_DIR}"
    "${HDF5_SRC_BINARY_DIR}"
    "${HDF5_TEST_BINARY_DIR}"
    "${HDF5_TEST_API_SRC_DIR}"
    "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>"
)
target_compile_options (
  h5_api_test_parallel
  PRIVATE
    "${HDF5_CMAKE_C_FLAGS}"
)
target_compile_definitions (
  h5_api_test_parallel
  PRIVATE
    $<$<CONFIG:Developer>:${HDF5_DEVELOPER_DEFS}>
)
if (NOT BUILD_SHARED_LIBS)
  TARGET_C_PROPERTIES (h5_api_test_parallel STATIC)
  target_link_libraries (
    h5_api_test_parallel
    PRIVATE
      ${HDF5_TEST_LIB_TARGET}
      ${HDF5_LIB_TARGET}
      "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:MPI::MPI_C>"
  )
else ()
  TARGET_C_PROPERTIES (h5_api_test_parallel SHARED)
  target_link_libraries (
    h5_api_test_parallel
    PRIVATE
      ${HDF5_TEST_LIBSH_TARGET}
      ${HDF5_LIBSH_TARGET}
      "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:MPI::MPI_C>"
  )
endif ()
set_target_properties (
  h5_api_test_parallel
  PROPERTIES
    FOLDER test/par/API
)
# Add Target to clang-format
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_TEST_h5_api_test_parallel_FORMAT h5_api_test_parallel)
endif ()

#-----------------------------------------------------------------------------
# Build the ported HDF5 test executables
#-----------------------------------------------------------------------------
foreach (api_test_extra ${HDF5_API_PAR_TESTS_EXTRA})
  set (HDF5_API_PAR_TEST_EXTRA_SRCS
    ${HDF5_API_PAR_TEST_EXTRA_SRCS}
    ${CMAKE_CURRENT_SOURCE_DIR}/${api_test_extra}.c
  )

  if (${api_test_extra} STREQUAL "testphdf5")
    set (HDF5_API_PAR_TEST_EXTRA_SRCS
      ${HDF5_API_PAR_TEST_EXTRA_SRCS}
      ${CMAKE_CURRENT_SOURCE_DIR}/t_ph5basic.c
      ${CMAKE_CURRENT_SOURCE_DIR}/t_file.c
      ${CMAKE_CURRENT_SOURCE_DIR}/t_dset.c
      ${CMAKE_CURRENT_SOURCE_DIR}/t_mdset.c
      ${CMAKE_CURRENT_SOURCE_DIR}/t_coll_chunk.c
      ${CMAKE_CURRENT_SOURCE_DIR}/t_span_tree.c
      ${CMAKE_CURRENT_SOURCE_DIR}/t_prop.c
      ${CMAKE_CURRENT_SOURCE_DIR}/t_file_image.c
      ${CMAKE_CURRENT_SOURCE_DIR}/t_coll_md_read.c
      ${CMAKE_CURRENT_SOURCE_DIR}/t_chunk_alloc.c
      ${CMAKE_CURRENT_SOURCE_DIR}/t_filter_read.c
    )
  endif ()

  add_executable (h5_api_test_parallel_${api_test_extra} ${HDF5_API_PAR_TEST_EXTRA_SRCS})
  target_include_directories (
    h5_api_test_parallel_${api_test_extra}
    PRIVATE
      "${HDF5_SRC_INCLUDE_DIRS}"
      "${HDF5_TEST_PAR_DIR}"
      "${HDF5_TEST_API_SRC_DIR}"
      "${HDF5_TEST_API_PAR_SRC_DIR}"
      "${HDF5_SRC_BINARY_DIR}"
      "${HDF5_TEST_BINARY_DIR}"
      "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>"
  )
  target_compile_options (
    h5_api_test_parallel_${api_test_extra}
    PRIVATE
      "${HDF5_CMAKE_C_FLAGS}"
  )
  target_compile_definitions (
    h5_api_test_parallel_${api_test_extra}
    PRIVATE
      $<$<CONFIG:Developer>:${HDF5_DEVELOPER_DEFS}>
  )
  if (NOT BUILD_SHARED_LIBS)
    TARGET_C_PROPERTIES (h5_api_test_parallel_${api_test_extra} STATIC)
    target_link_libraries (
      h5_api_test_parallel_${api_test_extra}
      PRIVATE
        ${HDF5_TEST_LIB_TARGET}
        ${HDF5_LIB_TARGET}
        "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:MPI::MPI_C>"
    )
  else ()
    TARGET_C_PROPERTIES (h5_api_test_parallel_${api_test_extra} SHARED)
    target_link_libraries (
      h5_api_test_parallel_${api_test_extra}
      PRIVATE
        ${HDF5_TEST_LIBSH_TARGET}
        ${HDF5_LIBSH_TARGET}
        "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:MPI::MPI_C>"
    )
  endif ()
  set_target_properties (
    h5_api_test_parallel_${api_test_extra}
    PROPERTIES
      FOLDER test/par/API
  )
  # Add Target to clang-format
  if (HDF5_ENABLE_FORMATTERS)
    clang_format (HDF5_TEST_h5_api_test_parallel_${api_test_extra}_FORMAT h5_api_test_parallel_${api_test_extra})
  endif ()
endforeach ()

#-----------------------------------------------------------------------------
# Add tests if HDF5 parallel testing is enabled
#-----------------------------------------------------------------------------
if (HDF5_TEST_PARALLEL)
  # List of files generated by the HDF5 API tests which
  # should be cleaned up in case the test failed to remove
  # them
  set (HDF5_API_PAR_TESTS_FILES
    h5_api_test_parallel.h5
  )

  if (HDF5_TEST_API_ENABLE_DRIVER)

  else ()

  endif ()
endif ()
