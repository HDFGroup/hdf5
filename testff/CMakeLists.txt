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
# BUILD_H5FF_TEST: build and create executable from target file
MACRO (BUILD_H5FF_TEST file)
  ADD_EXECUTABLE (${file} ${HDF5_TEST_FF_SOURCE_DIR}/${file}.c)
  TARGET_NAMING (${file} ${LIB_TYPE})
  TARGET_LINK_LIBRARIES (${file}
    ${HDF5_TEST_LIB_TARGET} ${HDF5_LIB_TARGET} ${LINK_LIBS})
  SET_TARGET_PROPERTIES (${file} PROPERTIES FOLDER test/ff)
ENDMACRO (BUILD_H5FF_TEST)

# ADD_H5FF_TEST: add parallel test from target file
MACRO (ADD_H5FF_TEST file)
  ADD_TEST (NAME TEST_FF_${file} COMMAND ${MPIEXEC} ${MPIEXEC_PREFLAGS} 
    ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_POSTFLAGS}
    $<TARGET_FILE:${file}>)
ENDMACRO (ADD_H5FF_TEST)

# ADD_H5FF_DRIVER_TEST: add client/server test from target server and target client
MACRO (ADD_H5FF_DRIVER_TEST test_name server client)
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
# Build Tests
#-----------------------------------------------------------------------------
# hello
BUILD_H5FF_TEST(hello_server)
BUILD_H5FF_TEST(hello_client)

BUILD_H5FF_TEST(h5ff_query)

# h5ff
# ADD_H5FF_DRIVER_TEST(<target>)
BUILD_H5FF_TEST(h5ff_server)
BUILD_H5FF_TEST(h5ff_client_attr)
BUILD_H5FF_TEST(h5ff_client_dset)
BUILD_H5FF_TEST(h5ff_client_links)
BUILD_H5FF_TEST(h5ff_client_map)
BUILD_H5FF_TEST(h5ff_client_multiple_cont)
BUILD_H5FF_TEST(h5ff_client_obj)
BUILD_H5FF_TEST(h5ff_client_open)
BUILD_H5FF_TEST(h5ff_client_paths)
BUILD_H5FF_TEST(h5ff_client_trans)
BUILD_H5FF_TEST(h5ff_client_analysis)

IF (HDF5_ENABLE_PYTHON)
  BUILD_H5FF_TEST(h5ff_python_exec)
ENDIF (HDF5_ENABLE_PYTHON)
# BUILD_H5FF_TEST(h5ff_client_container_open)

#-----------------------------------------------------------------------------
# Define Tests
#-----------------------------------------------------------------------------
# hello
ADD_H5FF_DRIVER_TEST(hello hello_server hello_client)

# h5ff
# ADD_H5FF_DRIVER_TEST(<test_name> <server_target> <client_target>)
ADD_H5FF_DRIVER_TEST(h5ff_attr h5ff_server h5ff_client_attr)
ADD_H5FF_DRIVER_TEST(h5ff_dset h5ff_server h5ff_client_dset)
ADD_H5FF_DRIVER_TEST(h5ff_links h5ff_server h5ff_client_links)
ADD_H5FF_DRIVER_TEST(h5ff_map h5ff_server h5ff_client_map)
ADD_H5FF_DRIVER_TEST(h5ff_multiple_count h5ff_server h5ff_client_multiple_cont)
ADD_H5FF_DRIVER_TEST(h5ff_obj h5ff_server h5ff_client_obj)
ADD_H5FF_DRIVER_TEST(h5ff_open h5ff_server h5ff_client_open)
ADD_H5FF_DRIVER_TEST(h5ff_paths h5ff_server h5ff_client_paths)
ADD_H5FF_DRIVER_TEST(h5ff_trans h5ff_server h5ff_client_trans)

# ADD_H5FF_DRIVER_TEST(h5ff_container_open h5ff_server h5ff_client_container_open)

# TODO make a list of test and loop over list with foreach
#SET (H5FF_TESTS
#    attr
#    dset
#    links
#    ...
#)

#FOREACH (testff ${H5FF_TESTS})
#  ADD_H5FF_DRIVER_TEST(${testff} ${testff}_server ${testff}_client)
#ENDFOREACH (testff ${H5FF_TESTS})

