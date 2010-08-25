cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_TOOLS_H5STAT)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SOURCE_DIR}/lib)
INCLUDE_DIRECTORIES (${HDF5_PROJECT_DIR}/test)

# --------------------------------------------------------------------
# Add the h5stat executables
# --------------------------------------------------------------------
ADD_EXECUTABLE (h5stat ${HDF5_TOOLS_H5STAT_SOURCE_DIR}/h5stat.c)
H5_NAMING (h5stat)
IF (WIN32)
  IF (MSVC)
    IF (NOT BUILD_SHARED_LIBS)
      SET_TARGET_PROPERTIES (h5stat
          PROPERTIES
              LINK_FLAGS "/NODEFAULTLIB:MSVCRT"
      ) 
    ENDIF (NOT BUILD_SHARED_LIBS)
  ENDIF (MSVC)
ENDIF (WIN32)
TARGET_LINK_LIBRARIES (h5stat  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})

SET (H5_DEP_EXECUTABLES h5stat)

##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

IF (BUILD_TESTING)
  # --------------------------------------------------------------------
  # Add the h5stat test executables
  # --------------------------------------------------------------------
  IF (NOT BUILD_SHARED_LIBS)
    ADD_EXECUTABLE (h5stat_gentest ${HDF5_TOOLS_H5STAT_SOURCE_DIR}/h5stat_gentest.c)
    H5_NAMING (h5stat_gentest)
    IF (WIN32)
      IF (MSVC)
        SET_TARGET_PROPERTIES (h5stat_gentest
            PROPERTIES
                LINK_FLAGS "/NODEFAULTLIB:MSVCRT"
        ) 
      ENDIF (MSVC)
    ENDIF (WIN32)
    TARGET_LINK_LIBRARIES (h5stat_gentest ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
    
    #ADD_TEST (NAME h5stat_gentest COMMAND $<TARGET_FILE:h5stat_gentest>)
  ENDIF (NOT BUILD_SHARED_LIBS)
  
  # --------------------------------------------------------------------
  # Copy all the HDF5 files from the test directory into the source directory
  # --------------------------------------------------------------------
  SET (HDF5_REFERENCE_FILES
      h5stat_help1.ddl
      h5stat_help2.ddl
      h5stat_filters.ddl
      h5stat_filters-file.ddl
      h5stat_filters-F.ddl
      h5stat_filters-d.ddl
      h5stat_filters-g.ddl
      h5stat_filters-dT.ddl
      h5stat_filters-UD.ddl
      h5stat_filters-UT.ddl
      h5stat_tsohm.ddl
      h5stat_newgrat.ddl
      h5stat_newgrat-UG.ddl
      h5stat_newgrat-UA.ddl
  )
  SET (HDF5_REFERENCE_TEST_FILES
      h5stat_filters.h5
      h5stat_tsohm.h5
      h5stat_newgrat.h5
  )

  FOREACH (ddl_file ${HDF5_REFERENCE_FILES})
    SET (ddldest "${PROJECT_BINARY_DIR}/${ddl_file}")
    #MESSAGE (STATUS " Translating ${ddl_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5stat
        POST_BUILD
        COMMAND    ${XLATE_UTILITY}
        ARGS       ${HDF5_TOOLS_H5STAT_SOURCE_DIR}/testfiles/${ddl_file} ${ddldest} -l3
    )
  ENDFOREACH (ddl_file ${HDF5_REFERENCE_FILES})

  FOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})
    SET (dest "${PROJECT_BINARY_DIR}/${h5_file}")
    #MESSAGE (STATUS " Copying ${h5_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5stat
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_H5STAT_SOURCE_DIR}/testfiles/${h5_file} ${dest}
    )
  ENDFOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})
  
##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  MACRO (ADD_H5_TEST resultfile resultcode)
    ADD_TEST (
        NAME H5STAT-${resultfile}
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:h5stat>"
            -D "TEST_ARGS=${ARGN}"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
            -D "TEST_OUTPUT=${resultfile}.out"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_REFERENCE=${resultfile}.ddl"
            -P "${HDF5_RESOURCES_DIR}/runTest.cmake"
    )
  ENDMACRO (ADD_H5_TEST file)

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

# Test for help flag
  ADD_H5_TEST (h5stat_help1 0 -h)
  ADD_H5_TEST (h5stat_help2 0 --help)

# Test file with groups, compressed datasets, user-applied fileters, etc.
# h5stat_filters.h5 is a copy of ../../testfiles/tfilters.h5 as of release 1.8.0-alpha4
  ADD_H5_TEST (h5stat_filters 0 h5stat_filters.h5)
  ADD_H5_TEST (h5stat_filters-file 0 -f h5stat_filters.h5)
  ADD_H5_TEST (h5stat_filters-F 0 -F h5stat_filters.h5)
  ADD_H5_TEST (h5stat_filters-d 0 -d h5stat_filters.h5)
  ADD_H5_TEST (h5stat_filters-g 0 -g h5stat_filters.h5)
  ADD_H5_TEST (h5stat_filters-dT 0 -dT h5stat_filters.h5)
  ADD_H5_TEST (h5stat_filters-UD 0 -D h5stat_filters.h5)
  ADD_H5_TEST (h5stat_filters-UT 0 -T h5stat_filters.h5)
# h5stat_tsohm.h5 is a copy of ../../../test/tsohm.h5 generated by tsohm.c 
# as of release 1.8.0-alpha4
  ADD_H5_TEST (h5stat_tsohm 0 h5stat_tsohm.h5)
# h5stat_newgrat.h5 is generated by h5stat_gentest.c
  ADD_H5_TEST (h5stat_newgrat 0 h5stat_newgrat.h5)
  ADD_H5_TEST (h5stat_newgrat-UG 0 -G h5stat_newgrat.h5)
  ADD_H5_TEST (h5stat_newgrat-UA 0 -A h5stat_newgrat.h5)
ENDIF (BUILD_TESTING)

##############################################################################
##############################################################################
###           I N S T A L L A T I O N                                      ###
##############################################################################
##############################################################################

#-----------------------------------------------------------------------------
# Rules for Installation of tools using make Install target
#-----------------------------------------------------------------------------
INSTALL (
    TARGETS
        h5stat
    RUNTIME DESTINATION
        bin/tools
    COMPONENT
        toolsapplications
)
