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

set (testhdf5_fortran_CLEANFILES
          a.h5
          b.h5
          c.h5
          d.h5
          dsetf_F03.h5
          enum1.h5
          extern_1a.raw
          extern_2a.raw
          extern_3a.raw
          extern_4a.raw
          extern_raw.h5
          get_info.h5
          nbit.h5
          t_array_F03.h5
          t_bit_F03.h5
          t_controlchar.h5
          t_controlchar_F03.h5
          t_enum_F03.h5
          t_objref_F03.h5
          t_opaque_F03.h5
          t_regref_F03.h5
          t_string_F03.h5
          t_vlen_F03.h5
          t_vlstring.h5
          t_vlstring_F03.h5
          t_vlstringrw_F03.h5
          tarray1.h5
          tarray2.h5
          tarray3.h5
          test_create.h5
          tget_file_image.h5
          th5o_ref.h5
          titerate.h5
          vds.h5
          visit.h5
          voltest.h5
)

# Remove any output file left over from previous test run
add_test (
    NAME FORTRAN_testhdf5-clear-objects
    COMMAND ${CMAKE_COMMAND} -E remove ${testhdf5_fortran_CLEANFILES}
)
set_tests_properties (FORTRAN_testhdf5-clear-objects PROPERTIES
    FIXTURES_SETUP clear_testhdf5_fortran
    WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
)
add_test (
    NAME FORTRAN_testhdf5-clean-objects
    COMMAND ${CMAKE_COMMAND} -E remove ${testhdf5_fortran_CLEANFILES}
)
set_tests_properties (FORTRAN_testhdf5-clean-objects PROPERTIES
    FIXTURES_CLEANUP clear_testhdf5_fortran
    WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
)

if (HDF5_USING_ANALYSIS_TOOL)
  add_test (NAME FORTRAN_testhdf5_fortran COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:testhdf5_fortran>)
else ()
  add_test (NAME FORTRAN_testhdf5_fortran COMMAND "${CMAKE_COMMAND}"
      -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
      -D "TEST_PROGRAM=$<TARGET_FILE:testhdf5_fortran>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_SKIP_COMPARE=TRUE"
      -D "TEST_REGEX= 0 error.s."
      -D "TEST_MATCH= 0 error(s)"
      -D "TEST_OUTPUT=testhdf5_fortran.txt"
      #-D "TEST_REFERENCE=testhdf5_fortran.out"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
      -P "${HDF_RESOURCES_DIR}/runTest.cmake"
  )
endif ()
#set_tests_properties (FORTRAN_testhdf5_fortran PROPERTIES PASS_REGULAR_EXPRESSION "[ ]*0 error.s")
set_tests_properties (FORTRAN_testhdf5_fortran PROPERTIES
    FIXTURES_REQUIRED clear_testhdf5_fortran
)
if ("FORTRAN_testhdf5_fortran" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
  set_tests_properties (FORTRAN_testhdf5_fortran PROPERTIES DISABLED true)
endif ()

#-- Adding test for testhdf5_fortran_1_8
if (HDF5_USING_ANALYSIS_TOOL)
  add_test (NAME FORTRAN_testhdf5_fortran_1_8 COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:testhdf5_fortran_1_8>)
else ()
  add_test (NAME FORTRAN_testhdf5_fortran_1_8 COMMAND "${CMAKE_COMMAND}"
      -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
      -D "TEST_PROGRAM=$<TARGET_FILE:testhdf5_fortran_1_8>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_SKIP_COMPARE=TRUE"
      -D "TEST_REGEX= 0 error.s."
      -D "TEST_MATCH= 0 error(s)"
      -D "TEST_OUTPUT=testhdf5_fortran_1_8.txt"
      #-D "TEST_REFERENCE=testhdf5_fortran_1_8.out"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
      -P "${HDF_RESOURCES_DIR}/runTest.cmake"
  )
endif ()
#set_tests_properties (FORTRAN_testhdf5_fortran_1_8 PROPERTIES PASS_REGULAR_EXPRESSION "[ ]*0 error.s")
set_tests_properties (FORTRAN_testhdf5_fortran_1_8 PROPERTIES
    DEPENDS FORTRAN_testhdf5_fortran
    FIXTURES_REQUIRED clear_testhdf5_fortran
)
if ("FORTRAN_testhdf5_fortran_1_8" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
  set_tests_properties (FORTRAN_testhdf5_fortran_1_8 PROPERTIES DISABLED true)
endif ()

#-- Adding test for fortranlib_test_F03
if (HDF5_USING_ANALYSIS_TOOL)
  add_test (NAME FORTRAN_fortranlib_test_F03 COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:fortranlib_test_F03>)
else ()
  add_test (NAME FORTRAN_fortranlib_test_F03 COMMAND "${CMAKE_COMMAND}"
      -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
      -D "TEST_PROGRAM=$<TARGET_FILE:fortranlib_test_F03>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_SKIP_COMPARE=TRUE"
      -D "TEST_REGEX= 0 error.s."
      -D "TEST_MATCH= 0 error(s)"
      -D "TEST_OUTPUT=fortranlib_test_F03.txt"
      #-D "TEST_REFERENCE=fortranlib_test_F03.out"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
      -P "${HDF_RESOURCES_DIR}/runTest.cmake"
  )
endif ()
#  set_tests_properties (FORTRAN_fortranlib_test_F03 PROPERTIES PASS_REGULAR_EXPRESSION "[ ]*0 error.s")
set_tests_properties (FORTRAN_fortranlib_test_F03 PROPERTIES
    DEPENDS FORTRAN_testhdf5_fortran_1_8
    FIXTURES_REQUIRED clear_testhdf5_fortran
)
if ("FORTRAN_fortranlib_test_F03" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
  set_tests_properties (FORTRAN_fortranlib_test_F03 PROPERTIES DISABLED true)
endif ()

#-- Adding test for vol_connector
if (HDF5_USING_ANALYSIS_TOOL)
  add_test (NAME FORTRAN_vol_connector COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:vol_connector>)
else ()
  add_test (NAME FORTRAN_vol_connector COMMAND "${CMAKE_COMMAND}"
      -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
      -D "TEST_PROGRAM=$<TARGET_FILE:vol_connector>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_SKIP_COMPARE=TRUE"
      -D "TEST_REGEX= 0 error.s."
      -D "TEST_MATCH= 0 error(s)"
      -D "TEST_OUTPUT=vol_connector.txt"
      #-D "TEST_REFERENCE=vol_connector.out"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
      -P "${HDF_RESOURCES_DIR}/runTest.cmake"
  )
endif ()
set_tests_properties (FORTRAN_vol_connector PROPERTIES
    FIXTURES_REQUIRED clear_testhdf5_fortran
)
if ("FORTRAN_vol_connector" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
  set_tests_properties (FORTRAN_vol_connector PROPERTIES DISABLED true)
endif ()

#-- Adding test for fflush1
add_test (
    NAME FORTRAN_flush1-clear-objects
    COMMAND ${CMAKE_COMMAND} -E remove flush.h5
)
add_test (
    NAME FORTRAN_fflush1
    COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:fflush1>
)
set_tests_properties (FORTRAN_fflush1 PROPERTIES
    DEPENDS FORTRAN_flush1-clear-objects
)
if ("FORTRAN_fflush1" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
  set_tests_properties (FORTRAN_fflush1 PROPERTIES DISABLED true)
endif ()

#-- Adding test for fflush2
add_test (
    NAME FORTRAN_fflush2
    COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:fflush2>
)
set_tests_properties (FORTRAN_fflush2 PROPERTIES
    DEPENDS FORTRAN_fflush1
)
if ("FORTRAN_fflush2" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
  set_tests_properties (FORTRAN_fflush2 PROPERTIES DISABLED true)
endif ()
add_test (
    NAME FORTRAN_flush1-clean-objects
    COMMAND ${CMAKE_COMMAND} -E remove flush.h5
)
set_tests_properties (FORTRAN_flush1-clean-objects PROPERTIES
    DEPENDS FORTRAN_fflush2
)
