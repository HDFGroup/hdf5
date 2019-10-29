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
  # Copy all the HDF5 files from the test directory into the source directory
  # --------------------------------------------------------------------
  set (HDF5_REFERENCE_FILES
      h5fc_help.ddl
      h5fc_nooption.ddl
      h5fc_d_file.ddl
      h5fc_dname.ddl
      h5fc_v_non_chunked.ddl
      h5fc_v_bt1.ddl
      h5fc_v_ndata_bt1.ddl
      h5fc_v_all.ddl
      h5fc_v_n_1d.ddl
      h5fc_v_n_all.ddl
      h5fc_ext1_i.ddl
      h5fc_ext1_s.ddl
      h5fc_ext1_f.ddl
      h5fc_ext2_if.ddl
      h5fc_ext2_is.ddl
      h5fc_ext2_sf.ddl
      h5fc_ext3_isf.ddl
      old_h5fc_ext1_i.ddl
      old_h5fc_ext1_s.ddl
      old_h5fc_ext1_f.ddl
      old_h5fc_ext2_if.ddl
      old_h5fc_ext2_is.ddl
      old_h5fc_ext2_sf.ddl
      old_h5fc_ext3_isf.ddl
      h5fc_v_err.ddl
      h5fc_v_err.ddl.err
  )
  set (HDF5_REFERENCE_ERR_FILES
      h5fc_d_file.ddl.err
      h5fc_dname.err
      h5fc_nonexistfile.ddl.err
      h5fc_nonexistdset_file.ddl.err
  )
  set (HDF5_REFERENCE_TEST_FILES
      h5fc_non_v3.h5
      h5fc_edge_v3.h5
      h5fc_ext_none.h5
      old_h5fc_ext_none.h5
      h5fc_ext1_i.h5
      h5fc_ext1_s.h5
      h5fc_ext1_f.h5
      h5fc_ext2_if.h5
      h5fc_ext2_is.h5
      h5fc_ext2_sf.h5
      h5fc_ext3_isf.h5
      old_h5fc_ext1_i.h5
      old_h5fc_ext1_s.h5
      old_h5fc_ext1_f.h5
      old_h5fc_ext2_if.h5
      old_h5fc_ext2_is.h5
      old_h5fc_ext2_sf.h5
      old_h5fc_ext3_isf.h5
      h5fc_err_level.h5
  )

  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")
  add_custom_target(h5fc-files ALL COMMENT "Copying files needed by h5fc tests")

  foreach (ddl_file ${HDF5_REFERENCE_FILES})
    HDFTEST_COPY_FILE("${HDF5_TOOLS_TEST_H5FC_SOURCE_DIR}/testfiles/${ddl_file}" "${PROJECT_BINARY_DIR}/testfiles/${ddl_file}" "h5fc_files")
  endforeach ()

  foreach (h5_file ${HDF5_REFERENCE_ERR_FILES})
    HDFTEST_COPY_FILE("${HDF5_TOOLS_TEST_H5FC_SOURCE_DIR}/testfiles/${h5_file}" "${PROJECT_BINARY_DIR}/testfiles/${h5_file}" "h5fc_files")
  endforeach ()

  foreach (h5_file ${HDF5_REFERENCE_TEST_FILES})
    HDFTEST_COPY_FILE("${HDF5_TOOLS_TEST_H5FC_SOURCE_DIR}/testfiles/${h5_file}" "${PROJECT_BINARY_DIR}/testfiles/${h5_file}" "h5fc_files")
  endforeach ()
  add_custom_target(h5fc_files ALL COMMENT "Copying files needed by h5fc tests" DEPENDS ${h5fc_files_list})

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

  macro (ADD_H5_OUTPUT testname resultfile resultcode testfile)
    # If using memchecker add tests without using scripts
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5FC-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove ./testfiles/outtmp.h5
      )
      if (last_test)
        set_tests_properties (H5FC-${testname}-clear-objects PROPERTIES DEPENDS ${last_test})
      endif ()
      if (${testfile})
          add_test (
              NAME H5FC-${testname}-${testfile}-tmpfile
              COMMAND ${CMAKE_COMMAND} -E copy_if_different ${HDF5_TOOLS_TEST_H5FC_SOURCE_DIR}/testfiles/${testfile} ./testfiles/outtmp.h5
          )
          set_tests_properties (H5FC-${testname}-${testfile}-tmpfile PROPERTIES DEPENDS "H5FC-${testname}-clear-objects")
          add_test (
              NAME H5FC-${testname}-${testfile}
              COMMAND "${CMAKE_COMMAND}"
                  -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                  -D "TEST_PROGRAM=$<TARGET_FILE:h5format_convert${tgt_ext}>"
                  -D "TEST_ARGS=${ARGN};outtmp.h5"
                  -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
                  -D "TEST_OUTPUT=${testname}.out"
                  -D "TEST_EXPECT=${resultcode}"
                  -D "TEST_REFERENCE=${resultfile}"
                  -D "TEST_ERRREF=${resultfile}.err"
                  -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
          )
          set_tests_properties (H5FC-${testname}-${testfile} PROPERTIES DEPENDS "H5FC-${testname}-${testfile}-tmpfile")
          set (last_test "H5FC-${testname}-${testfile}")
      else ()
          add_test (
              NAME H5FC-${testname}-NA
              COMMAND "${CMAKE_COMMAND}"
                  -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                  -D "TEST_PROGRAM=$<TARGET_FILE:h5format_convert${tgt_ext}>"
                  -D "TEST_ARGS=${ARGN}"
                  -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
                  -D "TEST_OUTPUT=${testname}.out"
                  -D "TEST_EXPECT=${resultcode}"
                  -D "TEST_REFERENCE=${resultfile}"
                  -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
          )
          set_tests_properties (H5FC-${testname}-NA PROPERTIES DEPENDS "H5FC-${testname}-clear-objects")
          set (last_test "H5FC-${testname}-NA")
      endif ()
    endif ()
  endmacro ()

  macro (ADD_H5_NOERR_OUTPUT testname resultfile resultcode testfile)
    # If using memchecker add tests without using scripts
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5FC-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove ./testfiles/outtmp.h5
      )
      if (last_test)
        set_tests_properties (H5FC-${testname}-clear-objects PROPERTIES DEPENDS ${last_test})
      endif ()
      add_test (
          NAME H5FC-${testname}-${testfile}-tmpfile
          COMMAND ${CMAKE_COMMAND} -E copy_if_different ${HDF5_TOOLS_TEST_H5FC_SOURCE_DIR}/testfiles/${testfile} ./testfiles/outtmp.h5
      )
      set_tests_properties (H5FC-${testname}-${testfile}-tmpfile PROPERTIES DEPENDS "H5FC-${testname}-clear-objects")
      add_test (
          NAME H5FC-${testname}-${testfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5format_convert${tgt_ext}>"
              -D "TEST_ARGS=${ARGN};outtmp.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=${testname}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5FC-${testname}-${testfile} PROPERTIES DEPENDS "H5FC-${testname}-${testfile}-tmpfile")
      set (last_test "H5FC-${testname}-${testfile}")
    endif ()
  endmacro ()

  macro (ADD_H5_MASK_OUTPUT testname resultfile resultcode result_errcheck testfile)
    # If using memchecker add tests without using scripts
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5FC-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove ./testfiles/outtmp.h5
      )
      if (last_test)
        set_tests_properties (H5FC-${testname}-clear-objects PROPERTIES DEPENDS ${last_test})
      endif ()
      add_test (
          NAME H5FC-${testname}-${testfile}-tmpfile
          COMMAND ${CMAKE_COMMAND} -E copy_if_different ${HDF5_TOOLS_TEST_H5FC_SOURCE_DIR}/testfiles/${testfile} ./testfiles/outtmp.h5
      )
      set_tests_properties (H5FC-${testname}-${testfile}-tmpfile PROPERTIES DEPENDS "H5FC-${testname}-clear-objects")
      add_test (
          NAME H5FC-${testname}-${testfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5format_convert${tgt_ext}>"
              -D "TEST_ARGS=${ARGN};outtmp.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=${testname}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}"
              -D "TEST_ERRREF=${result_errcheck}"
              -P "${HDF_RESOURCES_EXT_DIR}/grepTest.cmake"
      )
      set_tests_properties (H5FC-${testname}-${testfile} PROPERTIES DEPENDS "H5FC-${testname}-${testfile}-tmpfile")
      set (last_test "H5FC-${testname}-${testfile}")
    endif ()
  endmacro ()

  macro (ADD_H5_TEST testname resultcode testfile)
    # If using memchecker add tests without using scripts
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5FC-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove ./testfiles/tmp.h5
      )
      if (last_test)
        set_tests_properties (H5FC-${testname}-clear-objects PROPERTIES DEPENDS ${last_test})
      endif ()
      add_test (
          NAME H5FC-${testname}-tmpfile
          COMMAND ${CMAKE_COMMAND} -E copy_if_different ${HDF5_TOOLS_TEST_H5FC_SOURCE_DIR}/testfiles/${testfile} testfiles/tmp.h5
      )
      set_tests_properties (H5FC-${testname}-tmpfile PROPERTIES DEPENDS "H5FC-${testname}-clear-objects")
      add_test (
          NAME H5FC-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5format_convert${tgt_ext}>"
              -D "TEST_ARGS=${ARGN};./testfiles/tmp.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=testfiles/${testname}.out"
              -D "TEST_SKIP_COMPARE=TRUE"
              -D "TEST_EXPECT=${resultcode}"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5FC-${testname} PROPERTIES DEPENDS "H5FC-${testname}-tmpfile")
      set (last_test "H5FC-${testname}")
     endif ()
  endmacro ()

  macro (ADD_H5_CHECK_IDX dependtest testname)
    # If using memchecker add tests without using scripts
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5FC_CHECK_IDX-${testname}
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5fc_chk_idx> ./testfiles/tmp.h5 ${ARGN}
      )
      set_tests_properties (H5FC_CHECK_IDX-${testname} PROPERTIES DEPENDS "H5FC-${dependtest}")
    endif ()
  endmacro ()

  macro (ADD_H5_TEST_CHECK_IDX testname resultcode testfile)
    # If using memchecker add tests without using scripts
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5FC-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove ./testfiles/chktmp.h5
      )
      if (last_test)
        set_tests_properties (H5FC-${testname}-clear-objects PROPERTIES DEPENDS ${last_test})
      endif ()
      add_test (
          NAME H5FC-${testname}-tmpfile
          COMMAND ${CMAKE_COMMAND} -E copy_if_different ${HDF5_TOOLS_TEST_H5FC_SOURCE_DIR}/testfiles/${testfile} testfiles/chktmp.h5
      )
      set_tests_properties (H5FC-${testname}-tmpfile PROPERTIES DEPENDS "H5FC-${testname}-clear-objects")
      add_test (
          NAME H5FC-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5format_convert${tgt_ext}>"
              -D "TEST_ARGS=-d;${ARGN};./testfiles/chktmp.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=testfiles/${testname}.out"
              -D "TEST_SKIP_COMPARE=TRUE"
              -D "TEST_EXPECT=${resultcode}"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5FC-${testname} PROPERTIES DEPENDS "H5FC-${testname}-tmpfile")
      add_test (
          NAME H5FC_CHECK_IDX-${testname}
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5fc_chk_idx> ./testfiles/chktmp.h5 ${ARGN}
      )
      set_tests_properties (H5FC_CHECK_IDX-${testname} PROPERTIES DEPENDS "H5FC-${testname}")
      set (last_test "H5FC_CHECK_IDX-${testname}")
    endif ()
  endmacro ()

  macro (ADD_H5_H5DUMP_CHECK testname)
    # If using memchecker skip tests
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5FC-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove ./testfiles/dmptmp.h5
      )
      if (last_test)
        set_tests_properties (H5FC-${testname}-clear-objects PROPERTIES DEPENDS ${last_test})
      endif ()
      add_test (
          NAME H5FC-${testname}-tmpfile
          COMMAND ${CMAKE_COMMAND} -E copy_if_different ${HDF5_TOOLS_TEST_H5FC_SOURCE_DIR}/testfiles/${testname}.h5 testfiles/dmptmp.h5
      )
      set_tests_properties (H5FC-${testname}-tmpfile PROPERTIES DEPENDS "H5FC-${testname}-clear-objects")
      add_test (
          NAME H5FC-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5format_convert${tgt_ext}>"
              -D "TEST_ARGS=${ARGN};./testfiles/dmptmp.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=testfiles/${testname}.out"
              -D "TEST_SKIP_COMPARE=TRUE"
              -D "TEST_EXPECT=0"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5FC-${testname} PROPERTIES DEPENDS "H5FC-${testname}-tmpfile")
      add_test (
          NAME H5FC_CHECK_DUMP-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump${tgt_ext}>"
              -D "TEST_ARGS:STRING=-BH;./testfiles/dmptmp.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=testfiles/${testname}_chk.out"
              -D "TEST_EXPECT=0"
              -D "TEST_REFERENCE=testfiles/${testname}.ddl"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5FC_CHECK_DUMP-${testname} PROPERTIES DEPENDS "H5FC-${testname}")
      set (last_test "H5FC_CHECK_DUMP-${testname}")
    endif ()
  endmacro ()

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

  if (HDF5_ENABLE_USING_MEMCHECKER)
    # Remove any output file left over from previous test run
    add_test (
      NAME H5FC-clearall-objects
      COMMAND ${CMAKE_COMMAND} -E remove
          outtmp.h5
          tmp.h5
          chktmp.h5
          dmptmp.h5
    )
    if (last_test)
      set_tests_properties (H5FC-clearall-objects PROPERTIES DEPENDS ${last_test})
    endif ()
    set (last_test "H5FC-clearall-objects")
  endif ()

# h5format_convert --help
# h5format_convert (no options)
# h5format_convert nonexist.h5  (no options, file does not exist)
  ADD_H5_OUTPUT (h5fc_help h5fc_help.ddl 0 "" --help)
  ADD_H5_OUTPUT (h5fc_nooption h5fc_nooption.ddl 1 "")
  ADD_H5_OUTPUT (h5fc_nonexistfile h5fc_nonexistfile.ddl 1 "" nonexist.h5)
#
#
# h5format_convert -d old_h5fc_ext_none.h5 (just -d option, file exists)
# h5format_convert --dname old_h5fc_ext_none.h5 (just --dname option, file exists)
# h5format_convert --dname (just --dname option)
# h5format_convert --dname=nonexist old_h5fc_ext_none.h5 (dataset does not exist, file exists)
  ADD_H5_OUTPUT (h5fc_d_file-d h5fc_d_file.ddl 1 old_h5fc_ext_none.h5 -d)
  ADD_H5_OUTPUT (h5fc_d_file h5fc_d_file.ddl 1 old_h5fc_ext_none.h5 --dname)
  ADD_H5_OUTPUT (h5fc_dname h5fc_dname.ddl 1 "" --dname)
  ADD_H5_OUTPUT (h5fc_nonexistdset_file h5fc_nonexistdset_file.ddl 1 old_h5fc_ext_none.h5 --dname=nonexist)
#
#
#
# h5format_convert -d /DSET_CONTIGUOUS -v old_h5fc_ext_none.h5 (verbose, contiguous dataset)
# h5format_convert -d /GROUP/DSET_BT2 --verbose old_h5fc_ext_none.h5 (verbose, bt1 dataset)
# h5format_convert -d /DSET_NDATA_BT2 -v -n old_h5fc_ext_none.h5 (verbose, noop, bt1+nodata dataset)
# h5format_convert -v old_h5fc_ext_none.h5 (verbose, all datasets)
  ADD_H5_NOERR_OUTPUT (h5fc_v_non_chunked h5fc_v_non_chunked.ddl 0 old_h5fc_ext_none.h5 -d /DSET_CONTIGUOUS -v)
  ADD_H5_NOERR_OUTPUT (h5fc_v_bt1 h5fc_v_bt1.ddl 0 old_h5fc_ext_none.h5 -d /GROUP/DSET_BT2 --verbose)
  ADD_H5_NOERR_OUTPUT (h5fc_v_ndata_bt1 h5fc_v_ndata_bt1.ddl 0 old_h5fc_ext_none.h5 -d /DSET_NDATA_BT2 -v -n)
  ADD_H5_NOERR_OUTPUT (h5fc_v_all h5fc_v_all.ddl 0 old_h5fc_ext_none.h5 -v)
#
#
#
# h5format_convert -d /DSET_EA -v -n h5fc_ext_none.h5 (verbose, noop, one ea dataset)
# h5format_convert -v -n h5fc_non_v3.h5 (verbose, noop, all datasets)
  ADD_H5_NOERR_OUTPUT (h5fc_v_n_1d h5fc_v_n_1d.ddl 0 h5fc_ext_none.h5 -d /DSET_EA -v -n)
  ADD_H5_NOERR_OUTPUT (h5fc_v_n_all h5fc_v_n_all.ddl 0 h5fc_non_v3.h5 -v -n)
#
#
#
# h5format_convert -v h5fc_err_level.h5 (error encountered in converting the dataset)
  ADD_H5_MASK_OUTPUT (h5fc_v_err h5fc_v_err.ddl 1 "h5format_convert error: unable to downgrade dataset \"/DSET_ERR\"" h5fc_err_level.h5 -v)
#
#
#
# No output from tests
# 1) Use the tool to convert the dataset
# 2) Verify the chunk indexing type is correct
# h5format_convert -d /DSET_EA h5fc_ext_none.h5
# h5format_convert -d /GROUP/DSET_NDATA_EA h5fc_ext_none.h5
# h5format_convert -d /GROUP/DSET_BT2 h5fc_ext_none.h5
# h5format_convert -d /DSET_NDATA_BT2 h5fc_ext_none.h5
# h5format_convert -d /DSET_FA h5fc_ext_none.h5
# h5format_convert -d /GROUP/DSET_FA h5fc_ext_none.h5
# h5format_convert -d /DSET_NONE h5fc_ext_none.h5
# h5format_convert -d /GROUP/DSET_NDATA_NONE h5fc_ext_none.h5
  ADD_H5_TEST_CHECK_IDX (h5fc_ext_none_EA 0 h5fc_ext_none.h5 /DSET_EA)
#
  ADD_H5_TEST_CHECK_IDX (h5fc_ext_none_ND_EA 0 h5fc_ext_none.h5 /GROUP/DSET_NDATA_EA)
#
  ADD_H5_TEST_CHECK_IDX (h5fc_ext_none_BT 0 h5fc_ext_none.h5 /GROUP/DSET_BT2)
#
  ADD_H5_TEST_CHECK_IDX (h5fc_ext_none_ND_BT 0 h5fc_ext_none.h5 /DSET_NDATA_BT2)
#
  ADD_H5_TEST_CHECK_IDX (h5fc_ext_none_FA 0 h5fc_ext_none.h5 /DSET_FA)
#
  ADD_H5_TEST_CHECK_IDX (h5fc_ext_none_ND_FA 0 h5fc_ext_none.h5 /GROUP/DSET_NDATA_FA)
#
  ADD_H5_TEST_CHECK_IDX (h5fc_ext_none_NONE 0 h5fc_ext_none.h5 /DSET_NONE)
#
  ADD_H5_TEST_CHECK_IDX (h5fc_ext_none_ND_NONE 0 h5fc_ext_none.h5 /GROUP/DSET_NDATA_NONE)
#
#
#
# No output from tests: just check exit code
# h5format_convert -d /DSET_NDATA_BT2 old_h5fc_ext_none.h5 (v1-btree dataset)
# h5format_convert -d /DSET_CONTIGUOUS h5fc_non_v3.h5 (non-chunked dataset)
  ADD_H5_TEST (old_h5fc_ext_none 0 old_h5fc_ext_none.h5 -d /DSET_NDATA_BT2)
  ADD_H5_TEST (old_h5fc_ext_none_CONT 0 h5fc_non_v3.h5 -d /DSET_CONTIGUOUS)
#
#
#
# No output from tests: just check exit code
# h5format_convert -d /GROUP/DSET_BT2 -n h5fc_non_v3.h5 (noop, one dataset)
# h5format_convert -n h5fc_non_v3.h5 (noop, all datasets)
  ADD_H5_TEST (h5fc_non_v3_BT 0 h5fc_non_v3.h5 -d /GROUP/DSET_BT2 -n)
  ADD_H5_TEST (h5fc_non_v3-n 0 h5fc_non_v3.h5 -n)
#
#
#
# No output from tests: just check exit code
# h5format_convert h5fc_non_v3.h5
# 1) convert all datasets
# 2) verify indexing types
  ADD_H5_TEST (h5fc_non_v3 0 h5fc_non_v3.h5)
  ADD_H5_CHECK_IDX (h5fc_non_v3 h5fc_non_v3-NEA /DSET_NDATA_EA)
  ADD_H5_CHECK_IDX (h5fc_non_v3 h5fc_non_v3-NBT /DSET_NDATA_BT2)
  ADD_H5_CHECK_IDX (h5fc_non_v3 h5fc_non_v3-BT /GROUP/DSET_BT2)
  ADD_H5_CHECK_IDX (h5fc_non_v3 h5fc_non_v3-EA /GROUP/DSET_EA)
#
#
#
# No output from test: just check exit code
# h5format_convert h5fc_edge_v3.h5
# 1) convert the chunked dataset (filter, no-filter-edge-chunk)
# 2) verify the indexing type
  ADD_H5_TEST_CHECK_IDX (h5fc_edge_v3 0 h5fc_edge_v3.h5 /DSET_EDGE)
#
#

# The following test files have messages in the superblock extension.
# Verify h5dump output for correctness after conversion
  ADD_H5_H5DUMP_CHECK (h5fc_ext1_i)
  ADD_H5_H5DUMP_CHECK (h5fc_ext1_s)
  ADD_H5_H5DUMP_CHECK (h5fc_ext1_f)
#
  ADD_H5_H5DUMP_CHECK (h5fc_ext2_if)
  ADD_H5_H5DUMP_CHECK (h5fc_ext2_is)
  ADD_H5_H5DUMP_CHECK (h5fc_ext2_sf)
#
  ADD_H5_H5DUMP_CHECK (h5fc_ext3_isf)
#
#
#
  ADD_H5_H5DUMP_CHECK (old_h5fc_ext1_i)
  ADD_H5_H5DUMP_CHECK (old_h5fc_ext1_s)
  ADD_H5_H5DUMP_CHECK (old_h5fc_ext1_f)
#
  ADD_H5_H5DUMP_CHECK (old_h5fc_ext2_if)
  ADD_H5_H5DUMP_CHECK (old_h5fc_ext2_is)
  ADD_H5_H5DUMP_CHECK (old_h5fc_ext2_sf)
#
  ADD_H5_H5DUMP_CHECK (old_h5fc_ext3_isf)
