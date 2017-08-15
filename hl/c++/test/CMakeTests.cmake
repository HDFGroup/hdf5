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

if (HDF5_ENABLE_USING_MEMCHECKER)
  add_test (NAME HL_CPP_ptableTest COMMAND $<TARGET_FILE:hl_ptableTest>)
else ()
  add_test (NAME HL_CPP_ptableTest COMMAND "${CMAKE_COMMAND}"
      -D "TEST_PROGRAM=$<TARGET_FILE:hl_ptableTest>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_SKIP_COMPARE=TRUE"
      -D "TEST_OUTPUT=hl_ptableTest.txt"
      #-D "TEST_REFERENCE=hl_ptableTest.out"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
      -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
  )
endif ()
