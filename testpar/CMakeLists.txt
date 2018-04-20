cmake_minimum_required (VERSION 3.10)
project (HDF5_TEST_PAR C)

#-----------------------------------------------------------------------------
# Apply Definitions to compiler in this directory and below
#-----------------------------------------------------------------------------
add_definitions (${HDF_EXTRA_C_FLAGS})

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
)

#-- Adding test for testhdf5
add_executable (testphdf5 ${testphdf5_SOURCES})
set_property(TARGET testphdf5 APPEND PROPERTY INCLUDE_DIRECTORIES "${HDF5_SRC_DIR};${HDF5_BINARY_DIR}")
TARGET_C_PROPERTIES (testphdf5 STATIC " " " ")
target_link_libraries (testphdf5 PUBLIC ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})
if (HDF5_ENABLE_PARALLEL AND MPI_C_FOUND)
  target_link_libraries (testphdf5 PRIVATE ${MPI_C_LIBRARIES})
endif ()
set_target_properties (testphdf5 PROPERTIES FOLDER test/par)

MACRO (ADD_H5P_EXE file)
  add_executable (${file} ${HDF5_TEST_PAR_SOURCE_DIR}/${file}.c)
  set_property(TARGET ${file} APPEND PROPERTY INCLUDE_DIRECTORIES "${HDF5_SRC_DIR};${HDF5_BINARY_DIR}")
  TARGET_C_PROPERTIES (${file} STATIC " " " ")
  target_link_libraries (${file} PUBLIC ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})
  if (HDF5_ENABLE_PARALLEL AND MPI_C_FOUND)
    target_link_libraries (${file} PRIVATE ${MPI_C_LIBRARIES})
  endif ()
  set_target_properties (${file} PROPERTIES FOLDER test/par)
ENDMACRO (ADD_H5P_EXE file)

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
    t_shapesame
    t_filters_parallel
)

foreach (testp ${H5P_TESTS})
  ADD_H5P_EXE(${testp})
endforeach ()

include (CMakeTests.cmake)
