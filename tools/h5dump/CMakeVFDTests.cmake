#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#

##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

set (VFD_H5DUMP_LIST
    sec2
    stdio
    core
    core_paged
    split
    multi
    family
)

if (DIRECT_VFD)
  set (VFD_H5DUMP_LIST ${VFD_H5DUMP_LIST} direct)
endif ()

# --------------------------------------------------------------------
# Copy all the HDF5 files from the source directory into the test directory
# --------------------------------------------------------------------
set (HDF5_VFD_H5DUMP_FILES
  packedbits
)

foreach (vfdtest ${VFD_H5DUMP_LIST})
  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/${vfdname}")
  foreach (h5_tfile ${HDF5_VFD_H5DUMP_FILES})
    HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/${h5_tfile}.h5" "${PROJECT_BINARY_DIR}/${vfdtest}/${h5_tfile}.h5" "HDF5_VFD_H5DUMP_files")
    HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/${h5_tfile}.ddl" "${PROJECT_BINARY_DIR}/${vfdtest}/${h5_tfile}.ddl" "HDF5_VFD_H5DUMP_files")
  endforeach ()
endforeach ()

add_custom_target(HDF5_VFD_H5DUMP_files ALL COMMENT "Copying files needed by HDF5_VFD_H5DUMP tests" DEPENDS ${HDF5_VFD_H5DUMP_files_list})

##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

if (NOT BUILD_SHARED_LIBS)
  set (tgt_ext "")
else ()
  set (tgt_ext "-shared")
endif ()

macro (ADD_VFD_H5DUMP_TEST vfdname resultfile resultcode)
  if (NOT HDF5_ENABLE_USING_MEMCHECKER)
    add_test (
        NAME H5DUMP_VFD-${vfdname}-${resultfile}-h5dump
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
            -D "TEST_PROGRAM=$<TARGET_FILE:h5dump${tgt_ext}>"
            -D "TEST_ARGS:STRING=${ARGN}"
            -D "TEST_VFD:STRING=${vfdname}"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_OUTPUT=${resultfile}.out"
            -D "TEST_REFERENCE=${resultfile}.ddl"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/${vfdname}"
            -P "${HDF_RESOURCES_DIR}/vfdTest.cmake"
    )
    set_tests_properties (H5DUMP_VFD-${vfdname}-${resultfile}-h5dump PROPERTIES TIMEOUT ${CTEST_SHORT_TIMEOUT})
  endif ()
endmacro ()

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

# Run test with different Virtual File Driver
foreach (vfd ${VFD_H5DUMP_LIST})
  # test for signed/unsigned datasets
  ADD_VFD_H5DUMP_TEST (${vfd} packedbits 0 --enable-error-stack packedbits.h5)
endforeach ()
