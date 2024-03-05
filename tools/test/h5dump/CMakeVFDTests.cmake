#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#

##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################
H5_CREATE_VFD_DIR()

# --------------------------------------------------------------------
# Copy all the HDF5 files from the source directory into the test directory
# --------------------------------------------------------------------
set (HDF5_VFD_H5DUMP_FILES
  packedbits
)

set (HDF5_SF_VFD_H5DUMP_FILES
  test_subfiling_stripe_sizes.h5
)

set (HDF5_SF2_VFD_H5DUMP_FILES
  test_subfiling_precreate_rank_0.h5
)

foreach (vfdtest ${VFD_LIST})
  if (vfdtest STREQUAL "subfiling")
    foreach (h5_tfile ${HDF5_SF_VFD_H5DUMP_FILES})
      file(COPY "${PROJECT_SOURCE_DIR}/testfiles/${h5_tfile}" DESTINATION "${PROJECT_BINARY_DIR}/${vfdtest}")
      execute_process(
        COMMAND ls -i ${PROJECT_BINARY_DIR}/${vfdtest}/${h5_tfile}
        OUTPUT_VARIABLE OUTPUT_VALUE
        OUTPUT_STRIP_TRAILING_WHITESPACE
      )
      string(REGEX MATCH "^ *([0-9]+) *" INODE_VALUE "${OUTPUT_VALUE}")
      string(STRIP ${INODE_VALUE} INODE_STR)
      HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/${h5_tfile}.subfile_1_of_1" "${PROJECT_BINARY_DIR}/${vfdtest}/${h5_tfile}.subfile_${INODE_STR}_1_of_1" "HDF5_SF_VFD_H5DUMP_files")
      HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/${h5_tfile}.subfile.config" "${PROJECT_BINARY_DIR}/${vfdtest}/${h5_tfile}.subfile_${INODE_STR}.config" "HDF5_SF_VFD_H5DUMP_files")
    endforeach ()
    foreach (h5_tfile ${HDF5_SF2_VFD_H5DUMP_FILES})
      file(COPY "${PROJECT_SOURCE_DIR}/testfiles/${h5_tfile}" DESTINATION "${PROJECT_BINARY_DIR}/${vfdtest}")
      execute_process(
        COMMAND ls -i ${PROJECT_BINARY_DIR}/${vfdtest}/${h5_tfile}
        OUTPUT_VARIABLE OUTPUT_VALUE
        OUTPUT_STRIP_TRAILING_WHITESPACE
      )
      string(REGEX MATCH "^ *([0-9]+) *" INODE_VALUE "${OUTPUT_VALUE}")
      string(STRIP ${INODE_VALUE} INODE_STR)
      HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/${h5_tfile}.subfile_1_of_2" "${PROJECT_BINARY_DIR}/${vfdtest}/${h5_tfile}.subfile_${INODE_STR}_1_of_2" "HDF5_SF2_VFD_H5DUMP_files")
      HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/${h5_tfile}.subfile_2_of_2" "${PROJECT_BINARY_DIR}/${vfdtest}/${h5_tfile}.subfile_${INODE_STR}_2_of_2" "HDF5_SF2_VFD_H5DUMP_files")
      HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/${h5_tfile}.subfile.config" "${PROJECT_BINARY_DIR}/${vfdtest}/${h5_tfile}.subfile_${INODE_STR}.config" "HDF5_SF2_VFD_H5DUMP_files")
    endforeach ()
  endif ()
  foreach (h5_tfile ${HDF5_VFD_H5DUMP_FILES})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/${h5_tfile}.h5" "${PROJECT_BINARY_DIR}/${vfdtest}/${h5_tfile}.h5" "HDF5_VFD_H5DUMP_files")
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/expected/${h5_tfile}.ddl" "${PROJECT_BINARY_DIR}/${vfdtest}/${h5_tfile}.ddl" "HDF5_VFD_H5DUMP_files")
  endforeach ()
endforeach ()

add_custom_target(HDF5_VFD_H5DUMP_files ALL COMMENT "Copying files needed by HDF5_VFD_H5DUMP tests" DEPENDS ${HDF5_VFD_H5DUMP_files_list})
add_custom_target(HDF5_SF_VFD_H5DUMP_files ALL COMMENT "Copying files needed by HDF5_SF_VFD_H5DUMP tests" DEPENDS ${HDF5_SF_VFD_H5DUMP_files_list})
add_custom_target(HDF5_SF2_VFD_H5DUMP_files ALL COMMENT "Copying files needed by HDF5_SF2_VFD_H5DUMP tests" DEPENDS ${HDF5_SF2_VFD_H5DUMP_files_list})

##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

macro (ADD_VFD_H5DUMP_TEST vfdname resultfile resultcode)
  if (NOT HDF5_USING_ANALYSIS_TOOL)
    add_test (
        NAME H5DUMP_VFD-${vfdname}-${resultfile}-h5dump
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
            -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
            -D "TEST_ARGS:STRING=${ARGN}"
            -D "TEST_VFD:STRING=${vfdname}"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_OUTPUT=${resultfile}.out"
            -D "TEST_REFERENCE=${resultfile}.ddl"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/${vfdname}"
            -P "${HDF_RESOURCES_DIR}/vfdTest.cmake"
    )
    set_tests_properties (H5DUMP_VFD-${vfdname}-${resultfile}-h5dump PROPERTIES TIMEOUT ${CTEST_SHORT_TIMEOUT})
    if ("H5DUMP_VFD-${vfdname}-${resultfile}-h5dump" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5DUMP_VFD-${vfdname}-${resultfile}-h5dump PROPERTIES DISABLED true)
    endif ()
  endif ()
endmacro ()

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

# Run test with different Virtual File Driver
foreach (vfd ${VFD_LIST})
  if (vfd STREQUAL "subfiling")
    ADD_VFD_H5DUMP_TEST (${vfd} filedriver_subfiling 0 --enable-error-stack=2 --filedriver=subfiling test_subfiling_stripe_sizes.h5)
    ADD_VFD_H5DUMP_TEST (${vfd} vfd_name_subfiling 0 --enable-error-stack=2 --vfd-name=subfiling test_subfiling_stripe_sizes.h5)
    ADD_VFD_H5DUMP_TEST (${vfd} vfd_value_subfiling 0 --enable-error-stack=2 --vfd-value=12 test_subfiling_stripe_sizes.h5)
    ADD_VFD_H5DUMP_TEST (${vfd} vfd_value_subfiling_2 0 --enable-error-stack=2 --vfd-value=12 -d DSET -s 0 -S 100 -c 10 test_subfiling_precreate_rank_0.h5)
  endif ()
  # test for signed/unsigned datasets  
  ADD_VFD_H5DUMP_TEST (${vfd} packedbits 0 --enable-error-stack packedbits.h5)
endforeach ()
