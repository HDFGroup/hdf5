cmake_minimum_required (VERSION 2.8.10)
PROJECT (HDF5_TEST_FF)

#-----------------------------------------------------------------------------
# Apply Definitions to compiler in this directory and below
#-----------------------------------------------------------------------------
ADD_DEFINITIONS (${HDF5_EXTRA_C_FLAGS})

INCLUDE_DIRECTORIES (${HDF5_TEST_SRC_DIR})
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib )

#------------------------------------------------------------------------------
# Compile kwsys library and setup TestDriver
#------------------------------------------------------------------------------
CONFIGURE_FILE (
  ${CMAKE_CURRENT_SOURCE_DIR}/h5ff_test_config.h.in
  ${CMAKE_CURRENT_BINARY_DIR}/h5ff_test_config.h
)

INCLUDE_DIRECTORIES (${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR})

ADD_SUBDIRECTORY (driver)

#------------------------------------------------------------------------------
# Set up test macros
#------------------------------------------------------------------------------
MACRO (BUILD_H5FF_TEST file)
  ADD_EXECUTABLE (${file} ${HDF5_TEST_FF_SOURCE_DIR}/${file}.c)
  TARGET_NAMING (${file} ${LIB_TYPE})
  TARGET_LINK_LIBRARIES (${file}
    ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET} ${LINK_LIBS})
  SET_TARGET_PROPERTIES (${file} PROPERTIES FOLDER test/ff)
ENDMACRO (BUILD_H5FF_TEST)

MACRO (ADD_H5FF_TEST file)
  BUILD_H5FF_TEST (${file})
  ADD_TEST (NAME TEST_FF_${file} COMMAND ${MPIEXEC} ${MPIEXEC_PREFLAGS} 
    ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_POSTFLAGS}
    $<TARGET_FILE:${file}>)
ENDMACRO (ADD_H5FF_TEST)

MACRO (ADD_H5FF_DRIVER_TEST test_name server client)
  BUILD_H5FF_TEST (${server})
  BUILD_H5FF_TEST (${client})

  ADD_TEST (NAME TEST_FF_${test_name}
    COMMAND $<TARGET_FILE:h5ff_test_driver>
    --server $<TARGET_FILE:${server}>
    --client $<TARGET_FILE:${client}>
  )
ENDMACRO (ADD_H5FF_DRIVER_TEST)

# Save that for later if we need to launch using MPMD run
#MACRO (ADD_H5EFF_MPMD_TEST test_name server client)
#  ADD_TEST (NAME "${test_name}"
#    COMMAND ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} 1 ${MPIEXEC_PREFLAGS}
#    $<TARGET_FILE:${server}> : ${MPIEXEC_NUMPROC_FLAG}
#    ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_PREFLAGS} $<TARGET_FILE:${client}>
#  )
#MACRO (ADD_H5EFF_MPMD_TEST)

#-----------------------------------------------------------------------------
# Define Tests
#-----------------------------------------------------------------------------

ADD_H5FF_DRIVER_TEST(hello hello_server hello_client)
ADD_H5FF_DRIVER_TEST(client_server h5ff_test_server h5ff_test_client)
#SET (H5FF_TESTS
#    t_mpi
#    ...
#)

#FOREACH (testff ${H5FF_TESTS})
#  ADD_H5FF_DRIVER_TEST(${testff} ${testff}_server ${testff}_client)
#ENDFOREACH (testff ${H5FF_TESTS})

