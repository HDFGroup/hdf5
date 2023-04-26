cmake_minimum_required (VERSION 3.18)
project (HDF5_TOOLS_TEST_MISC C)

# --------------------------------------------------------------------
# Add the misc test executables
# --------------------------------------------------------------------
if (HDF5_BUILD_GENERATORS AND BUILD_STATIC_LIBS)
  add_executable (h5repart_gentest ${HDF5_TOOLS_TEST_MISC_SOURCE_DIR}/h5repart_gentest.c)
  target_include_directories (h5repart_gentest PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (h5repart_gentest STATIC)
  target_link_libraries (h5repart_gentest PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
  set_target_properties (h5repart_gentest PROPERTIES FOLDER generator/tools)
  #add_test (NAME h5repart_gentest COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5repart_gentest>)

  #-----------------------------------------------------------------------------
  # Add Target to clang-format
  #-----------------------------------------------------------------------------
  if (HDF5_ENABLE_FORMATTERS)
    clang_format (HDF5_TOOLS_TEST_MISC_h5repart_gentest_FORMAT h5repart_gentest)
  endif ()

  add_executable (h5clear_gentest ${HDF5_TOOLS_TEST_MISC_SOURCE_DIR}/h5clear_gentest.c)
  target_include_directories (h5clear_gentest PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_TEST_SRC_DIR};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
  TARGET_C_PROPERTIES (h5clear_gentest STATIC)
  target_link_libraries (h5clear_gentest PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
  set_target_properties (h5clear_gentest PROPERTIES FOLDER tools)
  #add_test (NAME H5CLEAR-h5clear_gentest COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5clear_gentest>)

  #-----------------------------------------------------------------------------
  # Add Target to clang-format
  #-----------------------------------------------------------------------------
  if (HDF5_ENABLE_FORMATTERS)
    clang_format (HDF5_TOOLS_TEST_MISC_h5clear_gentest_FORMAT h5clear_gentest)
  endif ()

  add_subdirectory (vds)

endif ()

add_executable (h5repart_test ${HDF5_TOOLS_TEST_MISC_SOURCE_DIR}/repart_test.c)
target_include_directories (h5repart_test PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (BUILD_STATIC_LIBS)
  TARGET_C_PROPERTIES (h5repart_test STATIC)
  target_link_libraries (h5repart_test PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
else ()
  TARGET_C_PROPERTIES (h5repart_test SHARED)
  target_link_libraries (h5repart_test PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
endif ()
set_target_properties (h5repart_test PROPERTIES FOLDER tools)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_TOOLS_TEST_MISC_h5repart_test_FORMAT h5repart_test)
endif ()

add_executable (clear_open_chk ${HDF5_TOOLS_TEST_MISC_SOURCE_DIR}/clear_open_chk.c)
target_include_directories (clear_open_chk PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>")
if (BUILD_STATIC_LIBS)
  TARGET_C_PROPERTIES (clear_open_chk STATIC)
  target_link_libraries (clear_open_chk PRIVATE ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})
else ()
  TARGET_C_PROPERTIES (clear_open_chk SHARED)
  target_link_libraries (clear_open_chk PRIVATE ${HDF5_TOOLS_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
endif ()
set_target_properties (clear_open_chk PROPERTIES FOLDER tools)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_TOOLS_TEST_MISC_clear_open_chk_FORMAT clear_open_chk)
endif ()

if (HDF5_TEST_SERIAL)
  include (CMakeTestsRepart.cmake)
  include (CMakeTestsClear.cmake)
  include (CMakeTestsMkgrp.cmake)
endif ()
