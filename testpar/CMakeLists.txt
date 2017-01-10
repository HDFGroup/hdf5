cmake_minimum_required (VERSION 3.2.2)
PROJECT (HDF5_TEST_PAR)

#-----------------------------------------------------------------------------
# Apply Definitions to compiler in this directory and below
#-----------------------------------------------------------------------------
add_definitions (${HDF_EXTRA_C_FLAGS})

INCLUDE_DIRECTORIES (${HDF5_TEST_SRC_DIR})
INCLUDE_DIRECTORIES (${HDF5_TOOLS_DIR}/lib )
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
TARGET_NAMING (testphdf5 STATIC)
TARGET_C_PROPERTIES (testphdf5 STATIC " " " ")
target_link_libraries (testphdf5 ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET} ${LINK_LIBS})
set_target_properties (testphdf5 PROPERTIES FOLDER test/par)

MACRO (ADD_H5P_EXE file)
  add_executable (${file} ${HDF5_TEST_PAR_SOURCE_DIR}/${file}.c)
  TARGET_NAMING (${file} STATIC)
  TARGET_C_PROPERTIES (${file} STATIC " " " ")
  target_link_libraries (${file} ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET} ${LINK_LIBS})
  set_target_properties (${file} PROPERTIES FOLDER test/par)
ENDMACRO (ADD_H5P_EXE file)

set (H5P_TESTS
    t_mpi
    t_cache
    t_pflush1
    t_pflush2
    t_pshutdown
    t_prestart
    t_init_term
    t_shapesame
)

foreach (testp ${H5P_TESTS})
  ADD_H5P_EXE(${testp})
endforeach (testp ${H5P_TESTS})

include (CMakeTests.cmake)
