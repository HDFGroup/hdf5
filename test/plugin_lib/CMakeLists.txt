cmake_minimum_required (VERSION 2.8.10)
PROJECT (HDF5_TEST_PLUGIN_LIB)

#-----------------------------------------------------------------------------
# Set the core name of the library
#-----------------------------------------------------------------------------
SET (HDF5_TEST_PLUGIN_LIB_CORENAME         "dynlib1")

#-----------------------------------------------------------------------------
# Set the true names of all the libraries if customized by external project
#-----------------------------------------------------------------------------
SET (HDF5_TEST_PLUGIN_LIB_NAME              "${HDF5_EXTERNAL_LIB_PREFIX}${HDF5_TEST_PLUGIN_LIB_CORENAME}")

#-----------------------------------------------------------------------------
# Set the target names of all the libraries
#-----------------------------------------------------------------------------
SET (HDF5_TEST_PLUGIN_LIB_TARGET              ${HDF5_TEST_PLUGIN_LIB_CORENAME})

#-----------------------------------------------------------------------------
# Apply Definitions to compiler in this directory and below
#-----------------------------------------------------------------------------
ADD_DEFINITIONS (${HDF5_EXTRA_C_FLAGS})

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_SRC_DIR})

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------

SET (H5_TEST_PLUGIN_LIB_SRCS
    ${HDF5_TEST_PLUGIN_LIB_SOURCE_DIR}/dynlib1.c
)

SET (H5_TEST_PLUGIN_LIB_HDRS
    ${HDF5_TEST_PLUGIN_LIB_SOURCE_DIR}/dynlib1.h
    ${HDF5_TEST_SOURCE_DIR}/H5srcdir.h
    ${HDF5_TEST_SOURCE_DIR}/h5test.h
)

#-----------------------------------------------------------------------------
# Generate the H5srcdir_str.h file containing user settings needed by compilation
#-----------------------------------------------------------------------------
SET (srcdir ${CMAKE_CURRENT_SOURCE_DIR})
CONFIGURE_FILE (${HDF5_TEST_SOURCE_DIR}/H5srcdir_str.h.in H5srcdir_str.h  @ONLY)
INCLUDE_DIRECTORIES (${CMAKE_CURRENT_BINARY_DIR})

ADD_LIBRARY (${HDF5_TEST_PLUGIN_LIB_TARGET} ${LIB_TYPE} ${H5_TEST_PLUGIN_LIB_SRCS} ${H5_TEST_PLUGIN_LIB_HDRS})
TARGET_LINK_LIBRARIES (${HDF5_TEST_PLUGIN_LIB_TARGET} ${HDF5_TEST_LIB_TARGET})
#SET_GLOBAL_VARIABLE( HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_TEST_PLUGIN_LIB_TARGET}")
H5_SET_LIB_OPTIONS (
    ${HDF5_TEST_PLUGIN_LIB_TARGET} ${HDF5_TEST_PLUGIN_LIB_NAME}
    ${LIB_TYPE}
    HDF5_TEST_PLUGIN_LIB_NAME_RELEASE
  HDF5_TEST_PLUGIN_LIB_NAME_DEBUG
)
SET_TARGET_PROPERTIES (${HDF5_TEST_PLUGIN_LIB_TARGET} PROPERTIES FOLDER libraries/TEST_PLUGIN)

##############################################################################
##############################################################################
###           T E S T S                                                    ###
##############################################################################
##############################################################################

#-- Adding test for plugin
ADD_EXECUTABLE (plugin ${HDF5_TEST_SOURCE_DIR}/plugin.c)
TARGET_NAMING (plugin ${LIB_TYPE})
TARGET_LINK_LIBRARIES (plugin ${HDF5_TEST_PLUGIN_LIB_TARGET})
SET_TARGET_PROPERTIES (plugin PROPERTIES FOLDER test)

ADD_TEST (NAME H5PLUGIN-plugin COMMAND $<TARGET_FILE:plugin>)
SET_TESTS_PROPERTIES (H5PLUGIN-plugin PROPERTIES ENVIRONMENT HDF5_PLUGIN_PATH=${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
