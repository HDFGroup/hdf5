cmake_minimum_required (VERSION 3.18)
project (HDF5_TEST_PAR C)

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

#-- Adding test for testhdf5
add_executable (testphdf5 ${testphdf5_SOURCES})
target_compile_options(testphdf5 PRIVATE "${HDF5_CMAKE_C_FLAGS}")
target_compile_definitions(testphdf5 PRIVATE "${HDF5_TESTPAR_COMPILE_DEFS_PRIVATE}")
target_include_directories (testphdf5
    PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>"
)
if (NOT BUILD_SHARED_LIBS)
  TARGET_C_PROPERTIES (testphdf5 STATIC)
  target_link_libraries (testphdf5
      PRIVATE ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET} "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:MPI::MPI_C>"
  )
else ()
  TARGET_C_PROPERTIES (testphdf5 SHARED)
  target_link_libraries (testphdf5
      PRIVATE ${HDF5_TEST_LIBSH_TARGET} ${HDF5_LIBSH_TARGET} "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:MPI::MPI_C>"
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
  target_compile_options(${file} PRIVATE "${HDF5_CMAKE_C_FLAGS}")
  target_compile_definitions(${file} PRIVATE "${HDF5_TESTPAR_COMPILE_DEFS_PRIVATE}")
  target_include_directories (${file}
      PRIVATE "${HDF5_SRC_INCLUDE_DIRS};${HDF5_SRC_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>"
  )
  if (NOT BUILD_SHARED_LIBS)
    TARGET_C_PROPERTIES (${file} STATIC)
    target_link_libraries (${file}
        PRIVATE ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET} "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:MPI::MPI_C>"
                "$<$<OR:$<PLATFORM_ID:Windows>,$<PLATFORM_ID:MinGW>>:ws2_32.lib>"
    )
  else ()
    TARGET_C_PROPERTIES (${file} SHARED)
    target_link_libraries (${file}
        PRIVATE ${HDF5_TEST_LIBSH_TARGET} ${HDF5_LIBSH_TARGET} "$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:MPI::MPI_C>"
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
endmacro (ADD_H5P_EXE file)

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
