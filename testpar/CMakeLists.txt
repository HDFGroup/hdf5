#-----------------------------------------------------------------------------
# CMake configuration for HDF5 parallel test library and test executables
# This file sets up the build, formatting, and installation rules for the HDF5 parallel test library (static/shared) and its test executables.
# It handles source/header definitions, compiler options, linking, export, and test configuration for parallel HDF5 testing.
#-----------------------------------------------------------------------------
cmake_minimum_required (VERSION 3.26)
project (HDF5_TEST_PAR C)

#################################################################################
# Define Parallel Test Library Sources
#################################################################################
set (TEST_PAR_LIB_SOURCES
    ${HDF5_TEST_PAR_SOURCE_DIR}/testpar.c
)

set (TEST_PAR_LIB_HEADERS
    ${HDF5_TEST_PAR_SOURCE_DIR}/testpar.h
)

#-----------------------------------------------------------------------------
# Define Tests
#-----------------------------------------------------------------------------

set (testphdf5_SOURCES
    ${HDF5_TEST_PAR_SOURCE_DIR}/testphdf5.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_dset.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_file.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_file_image.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_mdset.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_ph5basic.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_coll_chunk.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_span_tree.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_chunk_alloc.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_filter_read.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_prop.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_coll_md.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_oflush.c
)

#################################################################################
# Set private compile-time definitions added when
# compiling test source files
#################################################################################
set (HDF5_TESTPAR_COMPILE_DEFS_PRIVATE
    "$<$<CONFIG:Developer>:${HDF5_DEVELOPER_DEFS}>"
    "$<$<BOOL:HDF5_TEST_API>:H5_HAVE_TEST_API>"
)

#################################################################################
# Setup build for parallel test library
#################################################################################
if (BUILD_STATIC_LIBS)
  add_library (${HDF5_TEST_PAR_LIB_TARGET} STATIC ${TEST_PAR_LIB_SOURCES} ${TEST_PAR_LIB_HEADERS})
  target_include_directories (${HDF5_TEST_PAR_LIB_TARGET}
      PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_TEST_SRC_DIR};${HDF5_SRC_BINARY_DIR};${HDF5_TEST_PAR_BINARY_DIR};${MPI_C_INCLUDE_DIRS}"
      INTERFACE "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
  )
  target_compile_options(${HDF5_TEST_PAR_LIB_TARGET} PRIVATE "${HDF5_CMAKE_C_FLAGS}")
  target_compile_definitions(${HDF5_TEST_PAR_LIB_TARGET}
      PRIVATE
          "H5_TEST_EXPRESS_LEVEL_DEFAULT=${H5_TEST_EXPRESS_LEVEL_DEFAULT}"
          "${HDF5_TEST_COMPILE_DEFS_PRIVATE}"
  )
  TARGET_C_PROPERTIES (${HDF5_TEST_PAR_LIB_TARGET} STATIC)
  target_link_libraries (${HDF5_TEST_PAR_LIB_TARGET}
      PUBLIC ${LINK_LIBS} ${HDF5_LIB_TARGET}
      PRIVATE "$<$<OR:$<PLATFORM_ID:Windows>,$<PLATFORM_ID:MinGW>>:ws2_32.lib>"
  )
  if (MINGW)
    target_link_libraries (${HDF5_TEST_PAR_LIB_TARGET} PRIVATE "wsock32.lib")
  endif ()
  H5_SET_LIB_OPTIONS (${HDF5_TEST_PAR_LIB_TARGET} ${HDF5_TEST_PAR_LIB_NAME} STATIC 0)
  set_target_properties (${HDF5_TEST_PAR_LIB_TARGET} PROPERTIES FOLDER libraries/test/par)

  if (HDF5_EXPORTED_TARGETS AND HDF5_TEST_API_INSTALL)
    INSTALL_TARGET_PDB (${HDF5_TEST_PAR_LIB_TARGET} ${HDF5_INSTALL_LIB_DIR} libraries)

    install (
      TARGETS ${HDF5_TEST_PAR_LIB_TARGET}
      EXPORT ${HDF5_EXPORTED_TARGETS}
      LIBRARY DESTINATION ${HDF5_INSTALL_LIB_DIR} COMPONENT libraries
      ARCHIVE DESTINATION ${HDF5_INSTALL_LIB_DIR} COMPONENT libraries
      RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT libraries
      FRAMEWORK DESTINATION ${HDF5_INSTALL_FWRK_DIR} COMPONENT libraries
      INCLUDES DESTINATION include
    )
  endif ()
endif ()

if (BUILD_SHARED_LIBS)
  add_library (${HDF5_TEST_PAR_LIBSH_TARGET} SHARED ${TEST_PAR_LIB_SOURCES} ${TEST_PAR_LIB_HEADERS})
  target_include_directories (${HDF5_TEST_PAR_LIBSH_TARGET}
      PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_TEST_SRC_DIR};${HDF5_SRC_BINARY_DIR};${HDF5_TEST_PAR_BINARY_DIR};${MPI_C_INCLUDE_DIRS}"
      INTERFACE "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
  )
  target_compile_options(${HDF5_TEST_PAR_LIBSH_TARGET} PRIVATE "${HDF5_CMAKE_C_FLAGS}")
  target_compile_definitions(${HDF5_TEST_PAR_LIBSH_TARGET}
      PUBLIC "H5_BUILT_AS_DYNAMIC_LIB"
      PRIVATE
          "H5_TEST_EXPRESS_LEVEL_DEFAULT=${H5_TEST_EXPRESS_LEVEL_DEFAULT}"
          "${HDF5_TEST_COMPILE_DEFS_PRIVATE}"
  )
  TARGET_C_PROPERTIES (${HDF5_TEST_PAR_LIBSH_TARGET} SHARED)
  target_link_libraries (${HDF5_TEST_PAR_LIBSH_TARGET}
      PUBLIC ${LINK_LIBS} ${HDF5_LIBSH_TARGET}
      PRIVATE "$<$<OR:$<PLATFORM_ID:Windows>,$<PLATFORM_ID:MinGW>>:ws2_32.lib>"
  )
  if (MINGW)
    target_link_libraries (${HDF5_TEST_PAR_LIBSH_TARGET} PRIVATE "wsock32.lib")
  endif ()
  H5_SET_LIB_OPTIONS (${HDF5_TEST_PAR_LIBSH_TARGET} ${HDF5_TEST_PAR_LIB_NAME} SHARED "LIB")
  set_target_properties (${HDF5_TEST_PAR_LIBSH_TARGET} PROPERTIES FOLDER libraries/test/par)

  if (HDF5_EXPORTED_TARGETS AND HDF5_TEST_API_INSTALL)
    INSTALL_TARGET_PDB (${HDF5_TEST_PAR_LIBSH_TARGET} ${HDF5_INSTALL_BIN_DIR} libraries)

    install (
      TARGETS ${HDF5_TEST_PAR_LIBSH_TARGET}
      EXPORT ${HDF5_EXPORTED_TARGETS}
      LIBRARY DESTINATION ${HDF5_INSTALL_LIB_DIR} COMPONENT libraries
      ARCHIVE DESTINATION ${HDF5_INSTALL_LIB_DIR} COMPONENT libraries
      RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT libraries
      FRAMEWORK DESTINATION ${HDF5_INSTALL_FWRK_DIR} COMPONENT libraries
      INCLUDES DESTINATION include
    )
  endif ()
endif ()

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  if (BUILD_STATIC_LIBS)
    clang_format (HDF5_TEST_PAR_SRC_FORMAT ${HDF5_TEST_PAR_LIB_TARGET})
  else ()
    clang_format (HDF5_TEST_PAR_SRC_FORMAT ${HDF5_TEST_PAR_LIBSH_TARGET})
  endif ()
endif ()

#################################################################################
# Tests
#################################################################################

#-- Adding test for testhdf5
add_executable (testphdf5 ${testphdf5_SOURCES})
target_include_directories (testphdf5
    PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};${MPI_C_INCLUDE_DIRS}"
)
target_compile_options(testphdf5 PRIVATE "${HDF5_CMAKE_C_FLAGS}")
target_compile_definitions(testphdf5 PRIVATE "${HDF5_TESTPAR_COMPILE_DEFS_PRIVATE}")
if (NOT BUILD_SHARED_LIBS)
  TARGET_C_PROPERTIES (testphdf5 STATIC)
  target_link_libraries (testphdf5
      PRIVATE ${HDF5_TEST_LIB_TARGET} ${HDF5_TEST_PAR_LIB_TARGET} ${HDF5_LIB_TARGET} MPI::MPI_C
  )
else ()
  TARGET_C_PROPERTIES (testphdf5 SHARED)
  target_link_libraries (testphdf5
      PRIVATE ${HDF5_TEST_LIBSH_TARGET} ${HDF5_TEST_PAR_LIBSH_TARGET} ${HDF5_LIBSH_TARGET} MPI::MPI_C
  )
endif ()
set_target_properties (testphdf5 PROPERTIES FOLDER test/par)

#-----------------------------------------------------------------------------
# Add Target to clang-format
#-----------------------------------------------------------------------------
if (HDF5_ENABLE_FORMATTERS)
  clang_format (HDF5_TEST_PAR_testphdf5_FORMAT testphdf5)
endif ()

macro (ADD_H5P_EXE file)
  add_executable (${file} ${HDF5_TEST_PAR_SOURCE_DIR}/${file}.c)
  target_include_directories (${file}
      PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};${MPI_C_INCLUDE_DIRS}"
  )
  target_compile_options(${file} PRIVATE "${HDF5_CMAKE_C_FLAGS}")
  target_compile_definitions(${file} PRIVATE "${HDF5_TESTPAR_COMPILE_DEFS_PRIVATE}")
  if (NOT BUILD_SHARED_LIBS)
    TARGET_C_PROPERTIES (${file} STATIC)
    target_link_libraries (${file}
        PRIVATE ${HDF5_TEST_LIB_TARGET} ${HDF5_TEST_PAR_LIB_TARGET} ${HDF5_LIB_TARGET}
                "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:MPI::MPI_C>"
                "$<$<OR:$<PLATFORM_ID:Windows>,$<PLATFORM_ID:MinGW>>:ws2_32.lib>"
    )
  else ()
    TARGET_C_PROPERTIES (${file} SHARED)
    target_link_libraries (${file}
        PRIVATE ${HDF5_TEST_LIBSH_TARGET} ${HDF5_TEST_PAR_LIBSH_TARGET} ${HDF5_LIBSH_TARGET}
                "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:MPI::MPI_C>"
                "$<$<OR:$<PLATFORM_ID:Windows>,$<PLATFORM_ID:MinGW>>:ws2_32.lib>"
    )
  endif ()
  set_target_properties (${file} PROPERTIES FOLDER test/par)

  #-----------------------------------------------------------------------------
  # Add Target to clang-format
  #-----------------------------------------------------------------------------
  if (HDF5_ENABLE_FORMATTERS)
    clang_format (HDF5_TEST_PAR_${file}_FORMAT ${file})
  endif ()
endmacro ()

set (H5P_TESTS
    t_mpi
    t_bigio
    t_cache
    t_cache_image
    t_pflush1
    t_pflush2
    t_pread
    t_pshutdown
    t_prestart
    t_init_term
    t_pmulti_dset
    t_select_io_dset
    t_shapesame
    t_filters_parallel
    t_subfiling_vfd
    t_2Gio
    t_vfd
)

foreach (h5_testp ${H5P_TESTS})
  ADD_H5P_EXE(${h5_testp})
endforeach ()

if (HDF5_TEST_PARALLEL)
  include (CMakeTests.cmake)
endif ()

if (HDF5_TEST_API)
  add_subdirectory (API)
endif ()
