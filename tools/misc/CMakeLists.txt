cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_TOOLS_MISC)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SRC_DIR}/lib)
INCLUDE_DIRECTORIES (${HDF5_PROJECT_DIR}/test)

# --------------------------------------------------------------------
# Add the misc executables
# --------------------------------------------------------------------
#-- Misc Executables
ADD_EXECUTABLE (h5debug ${HDF5_TOOLS_MISC_SOURCE_DIR}/h5debug.c)
H5_NAMING (h5debug)
TARGET_WIN_PROPERTIES (h5debug)
TARGET_LINK_LIBRARIES (h5debug ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})

ADD_EXECUTABLE (h5repart ${HDF5_TOOLS_MISC_SOURCE_DIR}/h5repart.c)
H5_NAMING (h5repart)
TARGET_WIN_PROPERTIES (h5repart)
TARGET_LINK_LIBRARIES (h5repart ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})

ADD_EXECUTABLE (h5mkgrp ${HDF5_TOOLS_MISC_SOURCE_DIR}/h5mkgrp.c)
H5_NAMING (h5mkgrp)
TARGET_WIN_PROPERTIES (h5mkgrp)
TARGET_LINK_LIBRARIES (h5mkgrp  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})

SET (H5_DEP_EXECUTABLES
    h5debug
    h5repart
    h5mkgrp
)

##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

IF (BUILD_TESTING)
  # --------------------------------------------------------------------
  # Add the misc test executables
  # --------------------------------------------------------------------
  IF (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)
    ADD_EXECUTABLE (h5repart_gentest ${HDF5_TOOLS_MISC_SOURCE_DIR}/h5repart_gentest.c)
    H5_NAMING (h5repart_gentest)
    TARGET_WIN_PROPERTIES (h5repart_gentest)
    TARGET_LINK_LIBRARIES (h5repart_gentest ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
    #ADD_TEST (NAME h5repart_gentest COMMAND $<TARGET_FILE:h5repart_gentest>)
  ENDIF (HDF5_BUILD_GENERATORS AND NOT BUILD_SHARED_LIBS)

  ADD_EXECUTABLE (h5repart_test ${HDF5_TOOLS_MISC_SOURCE_DIR}/repart_test.c)
  H5_NAMING (h5repart_test)
  TARGET_WIN_PROPERTIES (h5repart_test)
  TARGET_LINK_LIBRARIES (h5repart_test ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
  
  # --------------------------------------------------------------------
  # Copy all the HDF5 files from the test directory into the source directory
  # --------------------------------------------------------------------
  SET (HDF5_REFERENCE_TEST_FILES
      family_file00000.h5
      family_file00001.h5
      family_file00002.h5
      family_file00003.h5
      family_file00004.h5
      family_file00005.h5
      family_file00006.h5
      family_file00007.h5
      family_file00008.h5
      family_file00009.h5
      family_file00010.h5
      family_file00011.h5
      family_file00012.h5
      family_file00013.h5
      family_file00014.h5
      family_file00015.h5
      family_file00016.h5
      family_file00017.h5
  )

  FOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})
    SET (dest "${PROJECT_BINARY_DIR}/${h5_file}")
    #MESSAGE (STATUS " Copying ${h5_file}")
    ADD_CUSTOM_COMMAND (
        TARGET     h5repart
        POST_BUILD
        COMMAND    ${CMAKE_COMMAND}
        ARGS       -E copy_if_different ${HDF5_TOOLS_SRC_DIR}/testfiles/${h5_file} ${dest}
    )
  ENDFOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})

##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

  # Remove any output file left over from previous test run
  ADD_TEST (
      NAME h5repart_20K-clear-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove 
          fst_family00000.h5
  )
  # repartition family member size to 20,000 bytes.
  ADD_TEST (NAME h5repart_20K COMMAND $<TARGET_FILE:h5repart> -m 20000 family_file%05d.h5 fst_family%05d.h5)

  # Remove any output file left over from previous test run
  ADD_TEST (
      NAME h5repart_5K-clear-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove 
          scd_family00000.h5
          scd_family00001.h5
          scd_family00002.h5
          scd_family00003.h5
  )
  # repartition family member size to 5 KB.
  ADD_TEST (NAME h5repart_5K COMMAND $<TARGET_FILE:h5repart> -m 5k family_file%05d.h5 scd_family%05d.h5)

  # Remove any output file left over from previous test run
  ADD_TEST (
      NAME h5repart_sec2-clear-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove 
          family_to_sec2.h5
  )
  # convert family file to sec2 file of 20,000 bytes
  ADD_TEST (NAME h5repart_sec2 COMMAND $<TARGET_FILE:h5repart> -m 20000 -family_to_sec2 family_file%05d.h5 family_to_sec2.h5)

  # test the output files repartitioned above.
  ADD_TEST (NAME h5repart_test COMMAND $<TARGET_FILE:h5repart_test>)

  SET (H5_DEP_EXECUTABLES ${H5_DEP_EXECUTABLES}
        h5repart_test
  )
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
        h5debug h5repart
    RUNTIME DESTINATION
        ${HDF5_INSTALL_BIN_DIR}/tools
    COMPONENT
        toolsapplications
)
