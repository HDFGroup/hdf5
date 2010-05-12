cmake_minimum_required (VERSION 2.8)
PROJECT (HDF5_TOOLS_MISC)

#-----------------------------------------------------------------------------
# Setup include Directories
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_TOOLS_SOURCE_DIR}/lib)
INCLUDE_DIRECTORIES (${HDF5_PROJECT_DIR}/test)

# --------------------------------------------------------------------
# Add the misc and test executables
# --------------------------------------------------------------------
#-- Misc Executables
ADD_EXECUTABLE (h5debug ${HDF5_TOOLS_MISC_SOURCE_DIR}/h5debug.c)
H5_NAMING (h5debug)
TARGET_LINK_LIBRARIES (h5debug ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})

ADD_EXECUTABLE (h5repart ${HDF5_TOOLS_MISC_SOURCE_DIR}/h5repart.c)
H5_NAMING (h5repart)
TARGET_LINK_LIBRARIES (h5repart ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})

ADD_EXECUTABLE (h5mkgrp ${HDF5_TOOLS_MISC_SOURCE_DIR}/h5mkgrp.c)
H5_NAMING (h5mkgrp)
TARGET_LINK_LIBRARIES (h5mkgrp  ${HDF5_TOOLS_LIB_TARGET} ${HDF5_LIB_TARGET})

SET (H5_DEP_EXECUTABLES
    h5debug
    h5repart
    h5mkgrp
)

IF (BUILD_TESTING)
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
        ARGS       -E copy_if_different ${HDF5_TOOLS_SOURCE_DIR}/testfiles/${h5_file} ${dest}
    )
  ENDFOREACH (h5_file ${HDF5_REFERENCE_TEST_FILES})

  IF (NOT BUILD_SHARED_LIBS)
    ADD_EXECUTABLE (h5repart_gentest ${HDF5_TOOLS_MISC_SOURCE_DIR}/h5repart_gentest.c)
    H5_NAMING (h5repart_gentest)
    TARGET_LINK_LIBRARIES (h5repart_gentest ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})
    #ADD_TEST (NAME h5repart_gentest COMMAND $<TARGET_FILE:h5repart_gentest>)
  ENDIF (NOT BUILD_SHARED_LIBS)

  ADD_EXECUTABLE (h5repart_test ${HDF5_TOOLS_MISC_SOURCE_DIR}/repart_test.c)
  H5_NAMING (h5repart_test)
  TARGET_LINK_LIBRARIES (h5repart_test ${HDF5_LIB_TARGET} ${HDF5_TOOLS_LIB_TARGET})

  # repartition family member size to 20,000 bytes.
  ADD_TEST (NAME h5repart_20K COMMAND $<TARGET_FILE:h5repart> -m 20000 family_file%05d.h5 fst_family%05d.h5)
  # repartition family member size to 5 KB.
  ADD_TEST (NAME h5repart_5K COMMAND $<TARGET_FILE:h5repart> -m 5k family_file%05d.h5 scd_family%05d.h5)
  # convert family file to sec2 file of 20,000 bytes
  ADD_TEST (NAME h5repart_sec2 COMMAND $<TARGET_FILE:h5repart> -m 20000 -family_to_sec2 family_file%05d.h5 family_to_sec2.h5)

  # test the output files repartitioned above.
  ADD_TEST (NAME h5repart_test COMMAND $<TARGET_FILE:h5repart_test>)

  SET (H5_DEP_EXECUTABLES ${H5_DEP_EXECUTABLES}
     #   h5repart_gentest
        h5repart_test
  )
ENDIF (BUILD_TESTING)

#-----------------------------------------------------------------------------
# Rules for Installation of tools using make Install target
#-----------------------------------------------------------------------------
INSTALL (
    TARGETS
        h5debug h5repart
    RUNTIME DESTINATION
        bin/tools
)
