cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_TEST_PAR)

INCLUDE_DIRECTORIES (${HDF5_TEST_SRC_DIR})
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib )
#-----------------------------------------------------------------------------
# Define Tests
#-----------------------------------------------------------------------------

SET (testphdf5_SRCS
    ${HDF5_TEST_PAR_SOURCE_DIR}/testphdf5.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_dset.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_file.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_mdset.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_ph5basic.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_coll_chunk.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_span_tree.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_chunk_alloc.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_filter_read.c
    ${HDF5_TEST_PAR_SOURCE_DIR}/t_rank_projection.c
)

#-- Adding test for testhdf5
ADD_EXECUTABLE (testphdf5 ${testphdf5_SRCS})
H5_NAMING (testphdf5)
TARGET_WIN_PROPERTIES (testphdf5)
TARGET_LINK_LIBRARIES (testphdf5 ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})

ADD_TEST (NAME testphdf5 COMMAND ${MPIEXEC} ${MPIEXEC_PREFLAGS} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_POSTFLAGS} $<TARGET_FILE:testphdf5>)

MACRO (ADD_H5P_TEST file)
  ADD_EXECUTABLE (${file} ${HDF5_TEST_PAR_SOURCE_DIR}/${file}.c)
  H5_NAMING (${file})
  TARGET_WIN_PROPERTIES (${file})
  TARGET_LINK_LIBRARIES (${file} ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET})

  ADD_TEST (NAME ${file} COMMAND ${MPIEXEC} ${MPIEXEC_PREFLAGS} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_POSTFLAGS} $<TARGET_FILE:${file}>)
ENDMACRO (ADD_H5P_TEST file)

SET (H5P_TESTS
    t_mpi
    t_posix_compliant
    t_cache
    t_pflush1
    t_pflush2
)

FOREACH (testp ${H5P_TESTS})
    ADD_H5P_TEST(${testp})
ENDFOREACH (testp ${H5P_TESTS})

