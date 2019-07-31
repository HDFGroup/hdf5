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

  # --------------------------------------------------------------------
  # Copy all the HDF5 files from the source directory into the test directory
  # --------------------------------------------------------------------
  set (HDF5_REFERENCE_TEST_FILES
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

  foreach (h5_file ${HDF5_REFERENCE_TEST_FILES})
    HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/${h5_file}" "${PROJECT_BINARY_DIR}/${h5_file}" "h5repart_files")
  endforeach ()
  add_custom_target(h5repart_files ALL COMMENT "Copying files needed by h5repart tests" DEPENDS ${h5repart_files_list})

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

  if (NOT BUILD_SHARED_LIBS)
    set (tgt_ext "")
  else ()
    set (tgt_ext "-shared")
  endif ()

  # Remove any output file left over from previous test run
  add_test (
    NAME H5REPART-clearall-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove
        fst_family00000.h5
        scd_family00000.h5
        scd_family00001.h5
        scd_family00002.h5
        scd_family00003.h5
        family_to_single.h5
        family_to_sec2.h5
  )
  set_tests_properties (H5REPART-clearall-objects PROPERTIES FIXTURES_SETUP clear_testrepart)

  # repartition family member size to 20,000 bytes.
  add_test (
      NAME H5REPART-h5repart_20K
      COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5repart${tgt_ext}> -m 20000 family_file%05d.h5 fst_family%05d.h5
  )
  set_tests_properties (H5REPART-h5repart_20K PROPERTIES
      FIXTURES_REQUIRED clear_testrepart
  )

  # repartition family member size to 5 KB.
  add_test (
      NAME H5REPART-h5repart_5K
      COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5repart${tgt_ext}> -m 5k family_file%05d.h5 scd_family%05d.h5
  )
  set_tests_properties (H5REPART-h5repart_5K PROPERTIES
      FIXTURES_REQUIRED clear_testrepart
  )

  # convert family file to sec2 file of 20,000 bytes
  add_test (
      NAME H5REPART-h5repart_single
      COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5repart${tgt_ext}> -m 20000 -family_to_single family_file%05d.h5 family_to_single.h5
  )
  set_tests_properties (H5REPART-h5repart_single PROPERTIES
      FIXTURES_REQUIRED clear_testrepart
  )

  # convert family file to sec2 file of 20,000 bytes (old argument)
  add_test (
      NAME H5REPART-h5repart_sec2
      COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5repart${tgt_ext}> -m 20000 -family_to_sec2 family_file%05d.h5 family_to_sec2.h5
  )
  set_tests_properties (H5REPART-h5repart_sec2 PROPERTIES
      FIXTURES_REQUIRED clear_testrepart
  )

  # test the output files repartitioned above.
  add_test (
      NAME H5REPART-h5repart_test
      COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5repart_test>
  )
  set_tests_properties (H5REPART-h5repart_test PROPERTIES
      DEPENDS "H5REPART-h5repart_20K;H5REPART-h5repart_5K;H5REPART-h5repart_single;H5REPART-h5repart_sec2"
  )

  set (H5_DEP_EXECUTABLES ${H5_DEP_EXECUTABLES}
        h5repart_test
  )
