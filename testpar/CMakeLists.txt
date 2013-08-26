cmake_minimum_required (VERSION 2.8.10)
PROJECT (HDF5_TEST_PAR)

#-----------------------------------------------------------------------------
# Apply Definitions to compiler in this directory and below
#-----------------------------------------------------------------------------
ADD_DEFINITIONS (${HDF5_EXTRA_C_FLAGS})

INCLUDE_DIRECTORIES (${HDF5_TEST_SRC_DIR})
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib )
#-----------------------------------------------------------------------------
# Define Tests
#-----------------------------------------------------------------------------

SET (testphdf5_SRCS
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
ADD_EXECUTABLE (testphdf5 ${testphdf5_SRCS})
TARGET_NAMING (testphdf5 ${LIB_TYPE})
TARGET_C_PROPERTIES (testphdf5 " " " ")
TARGET_LINK_LIBRARIES (testphdf5 ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET} ${LINK_LIBS})
SET_TARGET_PROPERTIES (testphdf5 PROPERTIES FOLDER test/par)

MACRO (ADD_H5P_EXE file)
  ADD_EXECUTABLE (${file} ${HDF5_TEST_PAR_SOURCE_DIR}/${file}.c)
  TARGET_NAMING (${file} ${LIB_TYPE})
  TARGET_C_PROPERTIES (${file} " " " ")
  TARGET_LINK_LIBRARIES (${file} ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET} ${LINK_LIBS})
  SET_TARGET_PROPERTIES (${file} PROPERTIES FOLDER test/par)
ENDMACRO (ADD_H5P_EXE file)

SET (H5P_TESTS
    t_mpi
#    t_posix_compliant
    t_cache
    t_pflush1
    t_pflush2
    t_shapesame
)

FOREACH (testp ${H5P_TESTS})
  ADD_H5P_EXE(${testp})
ENDFOREACH (testp ${H5P_TESTS})

IF (NOT WIN32)
  ADD_H5P_EXE(t_posix_compliant)
ENDIF (NOT WIN32)

INCLUDE (CMakeTests.cmake)
