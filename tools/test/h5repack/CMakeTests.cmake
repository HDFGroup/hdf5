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

  # --------------------------------------------------------------------
  # Copy all the HDF5 files from the source directory into the test directory
  # --------------------------------------------------------------------
  set (LIST_REPACK_TEST_FILES
      bounds_latest_latest.h5
      h5repack_attr.h5
      h5repack_attr_refs.h5
      h5repack_deflate.h5
      h5repack_early.h5
      h5repack_ext.h5
      h5repack_f32le.h5
      h5repack_f32le_ex-0.dat
      h5repack_f32le_ex.h5
      h5repack_fill.h5
      h5repack_filters.h5
      h5repack_fletcher.h5
      h5repack_hlink.h5
      h5repack_int32le_1d.h5
      h5repack_int32le_1d_ex-0.dat
      h5repack_int32le_1d_ex-1.dat
      h5repack_int32le_1d_ex.h5
      h5repack_int32le_2d.h5
      h5repack_int32le_2d_ex-0.dat
      h5repack_int32le_2d_ex.h5
      h5repack_int32le_3d.h5
      h5repack_int32le_3d_ex-0.dat
      h5repack_int32le_3d_ex.h5
      h5repack_layout.h5
      h5repack_layouto.h5
      h5repack_layout2.h5
      h5repack_layout3.h5
      h5repack_layout.UD.h5
      h5repack_named_dtypes.h5
      h5repack_nested_8bit_enum.h5
      h5repack_nested_8bit_enum_deflated.h5
      h5repack_CVE-2018-17432.h5
      h5repack_CVE-2018-14460.h5
      h5repack_nbit.h5
      h5repack_objs.h5
      h5repack_refs.h5
      h5repack_shuffle.h5
      h5repack_soffset.h5
      h5repack_szip.h5
      h5repack_uint8be.h5
      h5repack_uint8be_ex-0.dat
      h5repack_uint8be_ex-1.dat
      h5repack_uint8be_ex-2.dat
      h5repack_uint8be_ex-3.dat
      h5repack_uint8be_ex.h5
      # fsm
      h5repack_aggr.h5
      h5repack_fsm_aggr_nopersist.h5
      h5repack_fsm_aggr_persist.h5
      h5repack_none.h5
      h5repack_paged_nopersist.h5
      h5repack_paged_persist.h5
  )
  # h5diff/testfile
  set (LIST_DIFF_TEST_FILES
      h5diff_attr1.h5
  )
  # tools/testfiles/vds
  set (LIST_VDS_TEST_FILES
      1_a.h5
      1_b.h5
      1_c.h5
      1_d.h5
      1_e.h5
      1_f.h5
      1_vds.h5
      2_a.h5
      2_b.h5
      2_c.h5
      2_d.h5
      2_e.h5
      2_vds.h5
      3_1_vds.h5
      3_2_vds.h5
      4_0.h5
      4_1.h5
      4_2.h5
      4_vds.h5
      5_a.h5
      5_b.h5
      5_c.h5
      5_vds.h5
  )
  set (LIST_COPY_TEST_FILES
      h5copy_extlinks_src.h5
      h5copy_extlinks_trg.h5
  )
  set (LIST_HDF5_TEST_FILES
      # tools/testfiles for external links
      tsoftlinks.h5
      textlinkfar.h5
      textlinksrc.h5
      textlinktar.h5
      textlink.h5
      # tools/testfiles
      tfamily00000.h5
      tfamily00001.h5
      tfamily00002.h5
      tfamily00003.h5
      tfamily00004.h5
      tfamily00005.h5
      tfamily00006.h5
      tfamily00007.h5
      tfamily00008.h5
      tfamily00009.h5
      tfamily00010.h5
      tordergr.h5
      # reference conversion files
      tattrreg.h5
      tdatareg.h5
      # tools/testfiles onion VFD files
      tst_onion_dset_1d.h5
      tst_onion_dset_1d.h5.onion
      tst_onion_dset_ext.h5
      tst_onion_dset_ext.h5.onion
      tst_onion_objs.h5
      tst_onion_objs.h5.onion
  )

  set (LIST_OTHER_TEST_FILES
      h5repack_ext.bin
      h5repack.info
      ublock.bin
  )

  set (LIST_TST_TEST_FILES
      h5repack-help.txt
      h5repack_filters.h5-gzip_verbose_filters.tst
      h5repack_layout.h5-dset2_chunk_20x10-errstk.tst
      plugin_test.h5repack_layout.h5.tst
      plugin_version_test.h5repack_layout.h5.tst
      plugin_zero.h5repack_layout.h5.tst
      plugin_none.h5repack_layout.UD.h5.tst
      # tools/testfiles for external links
      tsoftlinks-merge.tsoftlinks.h5.tst
      textlinkfar-merge.textlinkfar.h5.tst
      textlinksrc-merge.textlinksrc.h5.tst
      textlinktar-merge.textlinktar.h5.tst
      textlink-merge.textlink.h5.tst
      h5copy_extlinks_src-merge.h5copy_extlinks_src.h5.tst
  )

  set (LIST_DDL_TEST_FILES
      crtorder.tordergr.h5.ddl
      deflate_limit.h5repack_layout.h5.ddl
      h5repack_layout.h5.ddl
      h5repack_layout.h5-plugin_test.ddl
      h5repack_layout.h5-plugin_version_test.ddl
      h5repack_layout.h5-plugin_zero.ddl
      h5repack_layout.UD.h5-plugin_none.ddl
      # fsm
      STG.h5repack_none.h5.ddl
      SPT.h5repack_aggr.h5.ddl
      SP.h5repack_fsm_aggr_nopersist.h5.ddl
      S.h5repack_fsm_aggr_persist.h5.ddl
      GS.h5repack_paged_nopersist.h5.ddl
      SP.h5repack_paged_persist.h5.ddl
      # vds
      1_vds.h5-vds_dset_chunk20x10x5-v.ddl
      2_vds.h5-vds_chunk3x6x9-v.ddl
      3_1_vds.h5-vds_chunk2x5x8-v.ddl
      4_vds.h5-vds_compa-v.ddl
      4_vds.h5-vds_conti-v.ddl
      # refs
      attrregion.tattrreg.h5.ddl
      dataregion.tdatareg.h5.ddl
      # tools/testfiles for external links
      textlinkfar-base.textlinkfar.h5.ddl
      textlinksrc-base.textlinksrc.h5.ddl
      textlinktar-base.textlinktar.h5.ddl
      textlink-base.textlink.h5.ddl
      tsoftlinks-base.tsoftlinks.h5.ddl
      h5copy_extlinks_src-base.h5copy_extlinks_src.h5.ddl
      textlinkfar-prune.textlinkfar.h5.ddl
      textlinksrc-prune.textlinksrc.h5.ddl
      textlinktar-prune.textlinktar.h5.ddl
      textlink-prune.textlink.h5.ddl
      tsoftlinks-prune.tsoftlinks.h5.ddl
      h5copy_extlinks_src-prune.h5copy_extlinks_src.h5.ddl
      textlinkfar-mergeprune.textlinkfar.h5.ddl
      textlinksrc-mergeprune.textlinksrc.h5.ddl
      textlinktar-mergeprune.textlinktar.h5.ddl
      textlink-mergeprune.textlink.h5.ddl
      tsoftlinks-mergeprune.tsoftlinks.h5.ddl
      h5copy_extlinks_src-mergeprune.h5copy_extlinks_src.h5.ddl
  )

  foreach (h5_file ${LIST_REPACK_TEST_FILES})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/${h5_file}" "${PROJECT_BINARY_DIR}/testfiles/${h5_file}" "h5repack_files")
  endforeach ()

  foreach (h5_file ${LIST_COPY_TEST_FILES})
    HDFTEST_COPY_FILE("${HDF5_TOOLS_TST_DIR}/h5copy/testfiles/${h5_file}" "${PROJECT_BINARY_DIR}/testfiles/${h5_file}" "h5repack_files")
  endforeach ()

  foreach (h5_file ${LIST_DIFF_TEST_FILES})
    HDFTEST_COPY_FILE("${HDF5_TOOLS_TST_DIR}/h5diff/testfiles/${h5_file}" "${PROJECT_BINARY_DIR}/testfiles/${h5_file}" "h5repack_files")
  endforeach ()

  foreach (h5_file ${LIST_VDS_TEST_FILES})
    HDFTEST_COPY_FILE("${HDF5_TOOLS_TST_DIR}/h5dump/testfiles/vds/${h5_file}" "${PROJECT_BINARY_DIR}/testfiles/${h5_file}" "h5repack_files")
  endforeach ()

  foreach (h5_file ${LIST_HDF5_TEST_FILES})
    HDFTEST_COPY_FILE("${HDF5_TOOLS_TST_DIR}/h5dump/testfiles/${h5_file}" "${PROJECT_BINARY_DIR}/testfiles/${h5_file}" "h5repack_files")
  endforeach ()

  foreach (h5_file ${LIST_OTHER_TEST_FILES})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/${h5_file}" "${PROJECT_BINARY_DIR}/testfiles/${h5_file}" "h5repack_files")
  endforeach ()

  foreach (h5_file ${LIST_TST_TEST_FILES})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/expected/${h5_file}" "${PROJECT_BINARY_DIR}/testfiles/${h5_file}" "h5repack_files")
  endforeach ()

  foreach (h5_file ${LIST_DDL_TEST_FILES})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/expected/${h5_file}" "${PROJECT_BINARY_DIR}/testfiles/${h5_file}" "h5repack_files")
  endforeach ()
  add_custom_target(h5repack_files ALL COMMENT "Copying files needed by h5repack tests" DEPENDS ${h5repack_files_list})

##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  macro (ADD_HELP_TEST testname resultcode)
    # If using memchecker add tests without using scripts
    if (HDF5_USING_ANALYSIS_TOOL)
      add_test (NAME H5REPACK-h5repack-${testname} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5repack> ${ARGN})
    else ()
      add_test (
          NAME H5REPACK-h5repack-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5repack>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=h5repack-${testname}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=h5repack-${testname}.txt"
              -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
    endif ()
    set_tests_properties (H5REPACK-h5repack-${testname} PROPERTIES
        WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
    )
    if ("H5REPACK-h5repack-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5REPACK-h5repack-${testname} PROPERTIES DISABLED true)
    endif ()
  endmacro ()

  macro (ADD_H5_TEST_OLD testname testtype testfile)
    if (NOT HDF5_USING_ANALYSIS_TOOL)
      if ("${testtype}" STREQUAL "SKIP")
        add_test (
            NAME H5REPACK_OLD-${testname}
            COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${ARGN} -i ${PROJECT_BINARY_DIR}/testfiles/${testfile} -o ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${testfile}"
        )
        set_property(TEST H5REPACK_OLD-${testname} PROPERTY DISABLED true)
      else ()
        add_test (
            NAME H5REPACK_OLD-${testname}-clear-objects
            COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${testfile}
        )
        add_test (
            NAME H5REPACK_OLD-${testname}
            COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5repack> ${ARGN} -i ${PROJECT_BINARY_DIR}/testfiles/${testfile} -o ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${testfile}
        )
        set_tests_properties (H5REPACK_OLD-${testname} PROPERTIES
            DEPENDS H5REPACK_OLD-${testname}-clear-objects
        )
        if ("H5REPACK_OLD-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
          set_tests_properties (H5REPACK_OLD-${testname} PROPERTIES DISABLED true)
        endif ()
        add_test (
            NAME H5REPACK_OLD-${testname}_DFF
            COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5diff> ${PROJECT_BINARY_DIR}/testfiles/${testfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${testfile}
        )
        set_tests_properties (H5REPACK_OLD-${testname}_DFF PROPERTIES
            DEPENDS H5REPACK_OLD-${testname}
        )
        if ("H5REPACK_OLD-${testname}_DFF" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
          set_tests_properties (H5REPACK_OLD-${testname}_DFF PROPERTIES DISABLED true)
        endif ()
        add_test (
            NAME H5REPACK_OLD-${testname}-clean-objects
            COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${testfile}
        )
        set_tests_properties (H5REPACK_OLD-${testname}-clean-objects PROPERTIES
            DEPENDS H5REPACK_OLD-${testname}_DFF
        )
      endif ()
    endif ()
  endmacro ()

  macro (ADD_H5_TEST testname testtype testfile)
    if ("${testtype}" STREQUAL "SKIP")
      if (NOT HDF5_USING_ANALYSIS_TOOL)
        add_test (
            NAME H5REPACK-${testname}
            COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${testfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${testfile}"
        )
        set_property(TEST H5REPACK-${testname} PROPERTY DISABLED true)
      endif ()
    else ()
      add_test (
          NAME H5REPACK-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${testfile}
      )
      add_test (
          NAME H5REPACK-${testname}
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5repack> --enable-error-stack ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${testfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${testfile}
      )
      set_tests_properties (H5REPACK-${testname} PROPERTIES
          DEPENDS H5REPACK-${testname}-clear-objects
      )
      if ("H5REPACK-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5REPACK-${testname} PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME H5REPACK-${testname}_DFF
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5diff> --enable-error-stack ${PROJECT_BINARY_DIR}/testfiles/${testfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${testfile}
      )
      set_tests_properties (H5REPACK-${testname}_DFF PROPERTIES
          DEPENDS H5REPACK-${testname}
      )
      if ("H5REPACK-${testname}_DFF" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5REPACK-${testname}_DFF PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME H5REPACK-${testname}-clean-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${testfile}
      )
      set_tests_properties (H5REPACK-${testname}-clean-objects PROPERTIES
          DEPENDS H5REPACK-${testname}_DFF
      )
    endif ()
  endmacro ()

  macro (ADD_H5_FILTER_TEST testname testfilter replacefilter testtype resultcode resultfile)
    if ("${testtype}" STREQUAL "SKIP")
      if (NOT HDF5_USING_ANALYSIS_TOOL)
        add_test (
            NAME H5REPACK_CMP-${testname}
            COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${resultfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${resultfile}"
        )
        set_property(TEST H5REPACK_CMP-${testname} PROPERTY DISABLED true)
      endif ()
    else ()
      add_test (
          NAME H5REPACK_CMP-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${resultfile}
      )
      # If using memchecker add tests without using scripts
      if (HDF5_USING_ANALYSIS_TOOL)
        add_test (
            NAME H5REPACK_CMP-${testname}
            COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5repack> ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${resultfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${resultfile}
        )
        set_tests_properties (H5REPACK_CMP-${testname} PROPERTIES
            DEPENDS H5REPACK_CMP-${testname}-clear-objects
        )
      else ()
        add_test (
            NAME H5REPACK_CMP-${testname}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                -D "TEST_PROGRAM=$<TARGET_FILE:h5repack>"
                -D "TEST_ARGS:STRING=${ARGN};${resultfile};out-${testname}.${resultfile}"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
                -D "TEST_OUTPUT=${resultfile}-${testname}.out"
                -D "TEST_EXPECT=${resultcode}"
                -D "TEST_MASK:STRING=O?...ing file[^\n]+\n"
                -D "TEST_FILTER:STRING=GZIP   \\(0\\.[0-9][0-9][0-9]:1\\)"
                -D "TEST_FILTER_REPLACE:STRING=GZIP   (0.XXX:1)"
                -D "TEST_REFERENCE=${resultfile}-${testname}.tst"
                -P "${HDF_RESOURCES_DIR}/runTest.cmake"
        )
        set_tests_properties (H5REPACK_CMP-${testname} PROPERTIES
            DEPENDS H5REPACK_CMP-${testname}-clear-objects
        )
        if ("H5REPACK_CMP-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
          set_tests_properties (H5REPACK_CMP-${testname} PROPERTIES DISABLED true)
        endif ()
      endif ()
      add_test (
          NAME H5REPACK_CMP-${testname}-clean-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${resultfile}
      )
      set_tests_properties (H5REPACK_CMP-${testname}-clean-objects PROPERTIES
          DEPENDS H5REPACK_CMP-${testname}
      )
    endif ()
  endmacro ()

  macro (ADD_H5_MASK_TEST testname testtype resultcode result_errcheck resultfile)
    if ("${testtype}" STREQUAL "SKIP")
      if (NOT HDF5_USING_ANALYSIS_TOOL)
        add_test (
            NAME H5REPACK_MASK-${testname}
            COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${resultfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${resultfile}"
        )
        set_property(TEST H5REPACK_MASK-${testname} PROPERTY DISABLED true)
      endif ()
    else ()
      add_test (
          NAME H5REPACK_MASK-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${resultfile}
      )
      # If using memchecker add tests without using scripts
      if (HDF5_USING_ANALYSIS_TOOL)
        add_test (
            NAME H5REPACK_MASK-${testname}
            COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5repack> ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${resultfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${resultfile}
        )
      else ()
        add_test (
            NAME H5REPACK_MASK-${testname}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                -D "TEST_PROGRAM=$<TARGET_FILE:h5repack>"
                -D "TEST_ARGS:STRING=${ARGN};${resultfile};out-${testname}.${resultfile}"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
                -D "TEST_OUTPUT=${resultfile}-${testname}.out"
                -D "TEST_EXPECT=${resultcode}"
                -D "TEST_SKIP_COMPARE=true"
                -D "TEST_REFERENCE=${resultfile}.mty"
                -D "TEST_ERRREF=${result_errcheck}"
                -P "${HDF_RESOURCES_DIR}/grepTest.cmake"
        )
      endif ()
      set_tests_properties (H5REPACK_MASK-${testname} PROPERTIES
          DEPENDS H5REPACK_MASK-${testname}-clear-objects
      )
      if ("H5REPACK_MASK-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5REPACK_MASK-${testname} PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME H5REPACK_MASK-${testname}-clean-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${resultfile}
      )
      set_tests_properties (H5REPACK_MASK-${testname}-clean-objects PROPERTIES
          DEPENDS H5REPACK_MASK-${testname}
      )
    endif ()
  endmacro ()

  macro (ADD_H5_DMP_TEST testname testtype resultcode resultfile)
    if ("${testtype}" STREQUAL "SKIP")
      if (NOT HDF5_USING_ANALYSIS_TOOL)
        add_test (
            NAME H5REPACK_DMP-${testname}
            COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${resultfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${resultfile}"
        )
        set_property(TEST H5REPACK_DMP-${testname} PROPERTY DISABLED true)
      endif ()
    else ()
      add_test (
          NAME H5REPACK_DMP-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${resultfile}
      )
      add_test (
          NAME H5REPACK_DMP-${testname}
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5repack> ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${resultfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${resultfile}
      )
      set_tests_properties (H5REPACK_DMP-${testname} PROPERTIES
          DEPENDS H5REPACK_DMP-${testname}-clear-objects
      )
      if ("H5REPACK_DMP-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5REPACK_DMP-${testname} PROPERTIES DISABLED true)
      endif ()
      if (NOT HDF5_USING_ANALYSIS_TOOL)
        add_test (
            NAME H5REPACK_DMP-h5dump-${testname}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
                -D "TEST_ARGS:STRING=-q;creation_order;-pH;out-${testname}.${resultfile}"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
                -D "TEST_OUTPUT=${resultfile}-${testname}.out"
                -D "TEST_EXPECT=${resultcode}"
                -D "TEST_REFERENCE=${testname}.${resultfile}.ddl"
                -P "${HDF_RESOURCES_DIR}/runTest.cmake"
        )
        set_tests_properties (H5REPACK_DMP-h5dump-${testname} PROPERTIES
            DEPENDS H5REPACK_DMP-${testname}
        )
        if ("H5REPACK_DMP-h5dump-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
          set_tests_properties (H5REPACK_DMP-h5dump-${testname} PROPERTIES DISABLED true)
        endif ()
        add_test (
            NAME H5REPACK_DMP-${testname}-clean-objects
            COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${resultfile}
        )
        set_tests_properties (H5REPACK_DMP-${testname}-clean-objects PROPERTIES
            DEPENDS H5REPACK_DMP-h5dump-${testname}
        )
      else ()
        add_test (
            NAME H5REPACK_DMP-${testname}-clean-objects
            COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${resultfile}
        )
        set_tests_properties (H5REPACK_DMP-${testname}-clean-objects PROPERTIES
            DEPENDS H5REPACK_DMP-${testname}
        )
      endif ()
    endif ()
  endmacro ()

  macro (ADD_H5_DMP_MASK testname testtype resultcode resultfile)
    if ("${testtype}" STREQUAL "SKIP")
      if (NOT HDF5_USING_ANALYSIS_TOOL)
        add_test (
            NAME H5REPACK_DMP-${testname}
            COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${resultfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${resultfile}"
        )
        set_property(TEST H5REPACK_DMP-${testname} PROPERTY DISABLED true)
      endif ()
    else ()
      add_test (
          NAME H5REPACK_DMP-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${resultfile}
      )
      add_test (
          NAME H5REPACK_DMP-${testname}
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5repack> ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${resultfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${resultfile}
      )
      set_tests_properties (H5REPACK_DMP-${testname} PROPERTIES
          DEPENDS H5REPACK_DMP-${testname}-clear-objects
      )
      if ("H5REPACK_DMP-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5REPACK_DMP-${testname} PROPERTIES DISABLED true)
      endif ()
      if (NOT HDF5_USING_ANALYSIS_TOOL)
        add_test (
            NAME H5REPACK_DMP-h5dump-${testname}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
                -D "TEST_ARGS:STRING=-q;creation_order;-pH;out-${testname}.${resultfile}"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
                -D "TEST_OUTPUT=${resultfile}-${testname}.out"
                -D "TEST_EXPECT=${resultcode}"
                -D "TEST_REFERENCE=${testname}.${resultfile}.ddl"
                -D "TEST_FILTER:STRING=SIZE [0-9][0-9][0-9][0-9] \\(2\\\.[0-9][0-9][0-9]:1 COMPRESSION\\)"
                -D "TEST_FILTER_REPLACE:STRING=SIZE XXXX (2.XXX:1 COMPRESSION)"
                -P "${HDF_RESOURCES_DIR}/runTest.cmake"
        )
        set_tests_properties (H5REPACK_DMP-h5dump-${testname} PROPERTIES
            DEPENDS H5REPACK_DMP-${testname}
        )
        if ("H5REPACK_DMP-h5dump-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
          set_tests_properties (H5REPACK_DMP-h5dump-${testname} PROPERTIES DISABLED true)
        endif ()
        add_test (
            NAME H5REPACK_DMP-${testname}-clean-objects
            COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${resultfile}
        )
        set_tests_properties (H5REPACK_DMP-${testname}-clean-objects PROPERTIES
            DEPENDS H5REPACK_DMP-h5dump-${testname}
        )
      else ()
        add_test (
            NAME H5REPACK_DMP-${testname}-clean-objects
            COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${resultfile}
        )
        set_tests_properties (H5REPACK_DMP-${testname}-clean-objects PROPERTIES
            DEPENDS H5REPACK_DMP-${testname}
        )
      endif ()
    endif ()
  endmacro ()

  macro (ADD_H5_DMP_NO_OPT_TEST testname testtype resultcode resultfile)
    if ("${testtype}" STREQUAL "SKIP")
      if (NOT HDF5_USING_ANALYSIS_TOOL)
        add_test (
            NAME H5REPACK_DMP-${testname}
            COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${resultfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${resultfile}"
        )
        set_property(TEST H5REPACK_DMP-${testname} PROPERTY DISABLED true)
      endif ()
    else ()
      add_test (
          NAME H5REPACK_DMP-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${resultfile}
      )
      set_tests_properties (H5REPACK_DMP-${testname}-clear-objects PROPERTIES
          FIXTURES_REQUIRED clear_h5repack
      )
      add_test (
          NAME H5REPACK_DMP-${testname}
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5repack> ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${resultfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${resultfile}
      )
      set_tests_properties (H5REPACK_DMP-${testname} PROPERTIES
          DEPENDS H5REPACK_DMP-${testname}-clear-objects
      )
      if ("H5REPACK_DMP-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5REPACK_DMP-${testname} PROPERTIES DISABLED true)
      endif ()
      if (NOT HDF5_USING_ANALYSIS_TOOL)
        add_test (
            NAME H5REPACK_DMP-h5dump-${testname}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
                -D "TEST_ARGS:STRING=out-${testname}.${resultfile}"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
                -D "TEST_OUTPUT=${resultfile}-${testname}.out"
                -D "TEST_EXPECT=${resultcode}"
                -D "TEST_REFERENCE=${testname}.${resultfile}.ddl"
                -P "${HDF_RESOURCES_DIR}/runTest.cmake"
        )
        set_tests_properties (H5REPACK_DMP-h5dump-${testname} PROPERTIES
            DEPENDS "H5REPACK_DMP-${testname}"
        )
        if ("H5REPACK_DMP-h5dump-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
          set_tests_properties (H5REPACK_DMP-h5dump-${testname} PROPERTIES DISABLED true)
        endif ()
      endif ()
    endif ()
  endmacro ()

  macro (ADD_H5_DIFF_TEST testname testtype resultcode testfile)
    if ("${testtype}" STREQUAL "SKIP")
      if (NOT HDF5_USING_ANALYSIS_TOOL)
        add_test (
            NAME H5REPACK_DIFF-${testname}
            COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${testfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${testfile}"
        )
        set_property(TEST H5REPACK_DIFF-${testname} PROPERTY DISABLED true)
      endif ()
    else ()
      add_test (
          NAME H5REPACK_DIFF-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${testfile}
      )
      add_test (
          NAME H5REPACK_DIFF-${testname}
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5repack> --enable-error-stack ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${testfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${testfile}
      )
      set_tests_properties (H5REPACK_DIFF-${testname} PROPERTIES
          DEPENDS H5REPACK_DIFF-${testname}-clear-objects
      )
      if ("H5REPACK_DIFF-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5REPACK_DIFF-${testname} PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME H5REPACK_DIFF-${testname}_DFF
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5diff>"
              -D "TEST_ARGS:STRING=-v;--enable-error-stack;${testfile};out-${testname}.${testfile}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=out-${testname}.${testfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${testname}.${testfile}.tst"
              -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (H5REPACK_DIFF-${testname}_DFF PROPERTIES
          DEPENDS H5REPACK_DIFF-${testname}
      )
      if ("H5REPACK_DIFF-${testname}_DFF" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5REPACK_DIFF-${testname}_DFF PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME H5REPACK_DIFF-${testname}-clean-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${testfile}
      )
      set_tests_properties (H5REPACK_DIFF-${testname}-clean-objects PROPERTIES
          DEPENDS H5REPACK_DIFF-${testname}_DFF
      )
    endif ()
  endmacro ()

  macro (ADD_H5_STAT_TEST testname testtype resultcode statarg resultfile)
    if ("${testtype}" STREQUAL "SKIP")
      if (NOT HDF5_USING_ANALYSIS_TOOL)
        add_test (
            NAME H5REPACK_STAT-${testname}
            COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${resultfile} ${PROJECT_BINARY_DIR}/testfiles/out-${statarg}.${resultfile}"
        )
        set_property(TEST H5REPACK_STAT-${testname} PROPERTY DISABLED true)
      endif ()
    else ()
      add_test (
          NAME H5REPACK_STAT-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${statarg}.${resultfile}
      )
      add_test (
          NAME H5REPACK_STAT-${testname}
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5repack> ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${resultfile} ${PROJECT_BINARY_DIR}/testfiles/out-${statarg}.${resultfile}
      )
      set_tests_properties (H5REPACK_STAT-${testname} PROPERTIES
          DEPENDS H5REPACK_STAT-${testname}-clear-objects
      )
      if ("H5REPACK_STAT-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5REPACK_STAT-${testname} PROPERTIES DISABLED true)
      endif ()
      if (NOT HDF5_USING_ANALYSIS_TOOL)
        add_test (
            NAME H5REPACK_STAT-h5stat-${testname}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                -D "TEST_PROGRAM=$<TARGET_FILE:h5stat>"
                -D "TEST_ARGS:STRING=-S;-s;out-${statarg}.${resultfile}"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
                -D "TEST_OUTPUT=${resultfile}-${testname}.out"
                -D "TEST_EXPECT=${resultcode}"
                -D "TEST_REFERENCE=${statarg}.${resultfile}.ddl"
                -P "${HDF_RESOURCES_DIR}/runTest.cmake"
        )
        set_tests_properties (H5REPACK_STAT-h5stat-${testname} PROPERTIES
            DEPENDS H5REPACK_STAT-${testname}
        )
        if ("H5REPACK_STAT-h5stat-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
          set_tests_properties (H5REPACK_STAT-h5stat-${testname} PROPERTIES DISABLED true)
        endif ()
        add_test (
            NAME H5REPACK_STAT-${testname}-clean-objects
            COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${statarg}.${resultfile}
        )
        set_tests_properties (H5REPACK_STAT-${testname}-clean-objects PROPERTIES
            DEPENDS H5REPACK_STAT-h5stat-${testname}
        )
      else ()
        add_test (
            NAME H5REPACK_STAT-${testname}-clean-objects
            COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${statarg}.${resultfile}
        )
        set_tests_properties (H5REPACK_STAT-${testname}-clean-objects PROPERTIES
            DEPENDS H5REPACK_STAT-${testname}
        )
      endif ()
    endif ()
  endmacro ()

  macro (ADD_H5_VERIFY_TEST testname testtype resultcode testfile testdset testfilter)
    if ("${testtype}" STREQUAL "SKIP")
      if (NOT HDF5_USING_ANALYSIS_TOOL)
        add_test (
            NAME H5REPACK_VERIFY_LAYOUT-${testname}
            COMMAND ${CMAKE_COMMAND} -E echo "SKIP -d ${testdset} -pH ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${resultfile}"
        )
        set_property(TEST H5REPACK_VERIFY_LAYOUT-${testname} PROPERTY DISABLED true)
      endif ()
    else ()
      if (NOT HDF5_USING_ANALYSIS_TOOL)
        add_test (
            NAME H5REPACK_VERIFY_LAYOUT-${testname}-clear-objects
            COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${testfile}
        )
        add_test (
            NAME H5REPACK_VERIFY_LAYOUT-${testname}
            COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5repack> ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${testfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${testfile}
        )
        set_tests_properties (H5REPACK_VERIFY_LAYOUT-${testname} PROPERTIES
            DEPENDS H5REPACK_VERIFY_LAYOUT-${testname}-clear-objects
        )
        if ("H5REPACK_VERIFY_LAYOUT-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
          set_tests_properties (H5REPACK_VERIFY_LAYOUT-${testname} PROPERTIES DISABLED true)
        endif ()
        add_test (
            NAME H5REPACK_VERIFY_LAYOUT-${testname}_DFF
            COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5diff> ${PROJECT_BINARY_DIR}/testfiles/${testfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${testfile}
        )
        set_tests_properties (H5REPACK_VERIFY_LAYOUT-${testname}_DFF PROPERTIES
            DEPENDS H5REPACK_VERIFY_LAYOUT-${testname}
        )
        if ("H5REPACK_VERIFY_LAYOUT-${testname}_DFF" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
          set_tests_properties (H5REPACK_VERIFY_LAYOUT-${testname}_DFF PROPERTIES DISABLED true)
        endif ()
        if (NOT ${resultcode})
          add_test (
              NAME H5REPACK_VERIFY_LAYOUT-${testname}_DMP
              COMMAND "${CMAKE_COMMAND}"
                  -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                  -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
                  -D "TEST_ARGS:STRING=-d;${testdset};-pH;out-${testname}.${testfile}"
                  -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
                  -D "TEST_OUTPUT=${testfile}-${testname}-v.out"
                  -D "TEST_EXPECT=${resultcode}"
                  -D "TEST_FILTER:STRING=${testfilter}"
                  -D "TEST_REFERENCE=${testfilter}"
                  -P "${HDF_RESOURCES_DIR}/grepTest.cmake"
          )
          set_tests_properties (H5REPACK_VERIFY_LAYOUT-${testname}_DMP PROPERTIES
              DEPENDS H5REPACK_VERIFY_LAYOUT-${testname}_DFF
          )
        else ()
          if ("${testfilter}" STREQUAL "CHUNKED")
            set (nottestfilter "(CONTIGUOUS|COMPACT)")
          endif ()
          if ("${testfilter}" STREQUAL "CONTIGUOUS")
            set (nottestfilter "(CHUNK|COMPACT)")
          endif ()
          if ("${testfilter}" STREQUAL "COMPACT")
            set (nottestfilter "(CONTIGUOUS|CHUNK)")
          endif ()
          add_test (
              NAME H5REPACK_VERIFY_LAYOUT-${testname}_DMP
              COMMAND "${CMAKE_COMMAND}"
                  -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                  -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
                  -D "TEST_ARGS:STRING=-pH;out-${testname}.${testfile}"
                  -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
                  -D "TEST_OUTPUT=${testfile}-${testname}-v.out"
                  -D "TEST_EXPECT=${resultcode}"
                  -D "TEST_FILTER:STRING=${nottestfilter}"
                  -D "TEST_REFERENCE=${testfilter}"
                  -P "${HDF_RESOURCES_DIR}/grepTest.cmake"
          )
          set_tests_properties (H5REPACK_VERIFY_LAYOUT-${testname}_DMP PROPERTIES
              DEPENDS H5REPACK_VERIFY_LAYOUT-${testname}_DFF
          )
        endif ()
        if ("H5REPACK_VERIFY_LAYOUT-${testname}_DMP" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
          set_tests_properties (H5REPACK_VERIFY_LAYOUT-${testname}_DMP PROPERTIES DISABLED true)
        endif ()
        add_test (
            NAME H5REPACK_VERIFY_LAYOUT-${testname}-clean-objects
            COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${testfile}
        )
        set_tests_properties (H5REPACK_VERIFY_LAYOUT-${testname}-clean-objects PROPERTIES
            DEPENDS H5REPACK_VERIFY_LAYOUT-${testname}_DMP
        )
      endif ()
    endif ()
  endmacro ()

  macro (ADD_H5_VERIFY_VDS testname testtype resultcode testfile testdset testfilter)
    if ("${testtype}" STREQUAL "SKIP")
      if (NOT HDF5_USING_ANALYSIS_TOOL)
        add_test (
            NAME H5REPACK_VERIFY_LAYOUT_VDS-${testname}
            COMMAND ${CMAKE_COMMAND} -E echo "SKIP -d ${testdset} -pH ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${resultfile}"
        )
        set_property(TEST H5REPACK_VERIFY_LAYOUT_VDS-${testname} PROPERTY DISABLED true)
      endif ()
    else ()
      if (NOT HDF5_USING_ANALYSIS_TOOL)
        # Remove any output file left over from previous test run
        add_test (
            NAME H5REPACK_VERIFY_LAYOUT_VDS-${testname}-clear-objects
            COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${testfile}
        )
        add_test (
            NAME H5REPACK_VERIFY_LAYOUT_VDS-${testname}
            COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5repack> ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${testfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${testfile}
        )
        set_tests_properties (H5REPACK_VERIFY_LAYOUT_VDS-${testname} PROPERTIES
            WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
            DEPENDS H5REPACK_VERIFY_LAYOUT_VDS-${testname}-clear-objects
        )
        if ("H5REPACK_VERIFY_LAYOUT_VDS-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
          set_tests_properties (H5REPACK_VERIFY_LAYOUT_VDS-${testname} PROPERTIES DISABLED true)
        endif ()
        add_test (
            NAME H5REPACK_VERIFY_LAYOUT_VDS-${testname}_DMP
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
                -D "TEST_ARGS:STRING=-d;${testdset};-p;out-${testname}.${testfile}"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
                -D "TEST_OUTPUT=${testfile}-${testname}-v.out"
                -D "TEST_EXPECT=${resultcode}"
                -D "TEST_REFERENCE=${testfile}-${testname}-v.ddl"
                -P "${HDF_RESOURCES_DIR}/runTest.cmake"
        )
        set_tests_properties (H5REPACK_VERIFY_LAYOUT_VDS-${testname}_DMP PROPERTIES
            WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
            DEPENDS H5REPACK_VERIFY_LAYOUT_VDS-${testname}
        )
        if ("H5REPACK_VERIFY_LAYOUT_VDS-${testname}_DMP" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
          set_tests_properties (H5REPACK_VERIFY_LAYOUT_VDS-${testname}_DMP PROPERTIES DISABLED true)
        endif ()
        add_test (
            NAME H5REPACK_VERIFY_LAYOUT_VDS-${testname}-clean-objects
            COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${testfile}
        )
        set_tests_properties (H5REPACK_VERIFY_LAYOUT_VDS-${testname}-clean-objects PROPERTIES
            DEPENDS H5REPACK_VERIFY_LAYOUT_VDS-${testname}_DMP
        )
      endif ()
    endif ()
  endmacro ()

# VERIFY_SUPERBLOCK
  macro (ADD_H5_VERIFY_SUPERBLOCK testname testfile lowbound highbound superblock)
    if (NOT HDF5_USING_ANALYSIS_TOOL)
      add_test (
          NAME H5REPACK_VERIFY_SUPERBLOCK-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${testfile}
      )
      add_test (
          NAME H5REPACK_VERIFY_SUPERBLOCK-${testname}
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5repack> -j;${lowbound};-k;${highbound} ${PROJECT_BINARY_DIR}/testfiles/${testfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${testfile}
      )
      set_tests_properties (H5REPACK_VERIFY_SUPERBLOCK-${testname} PROPERTIES
          DEPENDS H5REPACK_VERIFY_SUPERBLOCK-${testname}-clear-objects
      )
      if ("H5REPACK_VERIFY_SUPERBLOCK-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5REPACK_VERIFY_SUPERBLOCK-${testname} PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME H5REPACK_VERIFY_SUPERBLOCK-${testname}_DMP
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=-H;-B;out-${testname}.${testfile}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=${testfile}-${testname}-v.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_FILTER:STRING=SUPERBLOCK_VERSION ${superblock}"
              -D "TEST_REFERENCE=SUPERBLOCK_VERSION ${superblock}"
              -P "${HDF_RESOURCES_DIR}/grepTest.cmake"
      )
      set_tests_properties (H5REPACK_VERIFY_SUPERBLOCK-${testname}_DMP PROPERTIES
          DEPENDS H5REPACK_VERIFY_SUPERBLOCK-${testname}
      )
      if ("H5REPACK_VERIFY_SUPERBLOCK-${testname}_DMP" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5REPACK_VERIFY_SUPERBLOCK-${testname}_DMP PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME H5REPACK_VERIFY_SUPERBLOCK-${testname}-clean-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${testfile}
      )
      set_tests_properties (H5REPACK_VERIFY_SUPERBLOCK-${testname}-clean-objects PROPERTIES
          DEPENDS H5REPACK_VERIFY_SUPERBLOCK-${testname}_DMP
      )
    endif ()
  endmacro ()

  macro (ADD_H5_VERIFY_INVALIDBOUNDS testname resultcode lowbound highbound)
      add_test (
          NAME ADD_H5_VERIFY_INVALIDBOUNDS-h5repack-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${testfile}
      )
      add_test (
          NAME ADD_H5_VERIFY_INVALIDBOUNDS-h5repack-${testname}
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5repack> -j;${lowbound};-k;${highbound} ${PROJECT_BINARY_DIR}/testfiles/${testfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${testfile}
      )
      set_tests_properties (ADD_H5_VERIFY_INVALIDBOUNDS-h5repack-${testname} PROPERTIES
          DEPENDS ADD_H5_VERIFY_INVALIDBOUNDS-h5repack-${testname}-clear-objects
          WILL_FAIL "true"
      )
      if ("ADD_H5_VERIFY_INVALIDBOUNDS-h5repack-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (ADD_H5_VERIFY_INVALIDBOUNDS-h5repack-${testname} PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME ADD_H5_VERIFY_INVALIDBOUNDS-h5repack-${testname}-clean-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${testfile}
      )
      set_tests_properties (ADD_H5_VERIFY_INVALIDBOUNDS-h5repack-${testname}-clean-objects PROPERTIES
          DEPENDS ADD_H5_VERIFY_INVALIDBOUNDS-h5repack-${testname}
      )
  endmacro ()

  macro (ADD_H5_TEST_META testname testfile)
      # Remove any output file left over from previous test run
      add_test (
          NAME H5REPACK_META-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove
              testfiles/out-${testname}_N.${testname}.h5
              testfiles/out-${testname}_M.${testname}.h5
      )
      add_test (
          NAME H5REPACK_META-${testname}_N
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5repack> ${PROJECT_BINARY_DIR}/testfiles/${testfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}_N.${testname}.h5
      )
      set_tests_properties (H5REPACK_META-${testname}_N PROPERTIES
          DEPENDS H5REPACK_META-${testname}-clear-objects
      )
      if ("H5REPACK_META-${testname}_N" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5REPACK_META-${testname}_N PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME H5REPACK_META-${testname}_N_DFF
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5diff>"
              -D "TEST_ARGS:STRING=-v;${testfile};out-${testname}_N.${testname}.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=out-${testname}_N.${testname}.out"
              -D "TEST_EXPECT=0"
              -D "TEST_REFERENCE=out-${testname}_N.${testname}.txt"
              -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (H5REPACK_META-${testname}_N_DFF PROPERTIES
          DEPENDS H5REPACK_META-${testname}_N
      )
      if ("H5REPACK_META-${testname}_N_DFF" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5REPACK_META-${testname}_N_DFF PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME H5REPACK_META-${testname}_M
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5repack> ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${testfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}_M.${testname}.h5
      )
      set_tests_properties (H5REPACK_META-${testname}_M PROPERTIES
          DEPENDS H5REPACK_META-${testname}_N_DFF
      )
      if ("H5REPACK_META-${testname}_M" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5REPACK_META-${testname}_M PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME H5REPACK_META-${testname}_M_DFF
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5diff>"
              -D "TEST_ARGS:STRING=-v;${testfile};out-${testname}_M.${testname}.h5"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=out-${testname}_M.${testname}.out"
              -D "TEST_EXPECT=0"
              -D "TEST_REFERENCE=out-${testname}_M.${testname}.txt"
              -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (H5REPACK_META-${testname}_M_DFF PROPERTIES
          DEPENDS H5REPACK_META-${testname}_M
      )
      if ("H5REPACK_META-${testname}_M_DFF" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5REPACK_META-${testname}_M_DFF PROPERTIES DISABLED true)
      endif ()
      add_test (NAME H5REPACK_META-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_ONEFILE=out-${testname}_N.${testname}.h5"
              -D "TEST_TWOFILE=out-${testname}_M.${testname}.h5"
              -D "TEST_FUNCTION=LTEQ"
              -P "${HDF_RESOURCES_DIR}/fileCompareTest.cmake"
      )
      set_tests_properties (H5REPACK_META-${testname} PROPERTIES
          DEPENDS H5REPACK_META-${testname}_M_DFF
      )
      if ("H5REPACK_META-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5REPACK_META-${testname} PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME H5REPACK_META-${testname}-clean-objects
          COMMAND ${CMAKE_COMMAND} -E remove
              testfiles/out-${testname}_N.${testname}.h5
              testfiles/out-${testname}_M.${testname}.h5
      )
      set_tests_properties (H5REPACK_META-${testname}-clean-objects PROPERTIES
          DEPENDS H5REPACK_META-${testname}
      )
  endmacro ()

  macro (ADD_H5_UD_TEST testname resultcode resultfile)
    if (NOT HDF5_USING_ANALYSIS_TOOL)
      # Remove any output file left over from previous test run
      add_test (
          NAME H5REPACK_UD-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${resultfile}
      )
      add_test (
          NAME H5REPACK_UD-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5repack>"
              -D "TEST_ARGS:STRING=${ARGN};${resultfile};out-${testname}.${resultfile}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_MASK:STRING=O?...ing file[^\n]+\n"
              -D "TEST_OUTPUT=${testname}.${resultfile}.out"
              -D "TEST_REFERENCE=${testname}.${resultfile}.tst"
              -D "TEST_ENV_VAR=HDF5_PLUGIN_PATH"
              -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}/plugins"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
              -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (H5REPACK_UD-${testname} PROPERTIES
          DEPENDS H5REPACK_UD-${testname}-clear-objects
      )
      if ("H5REPACK_UD-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5REPACK_UD-${testname} PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME H5REPACK_UD-${testname}-h5dump
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=-pH;out-${testname}.${resultfile}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles"
              -D "TEST_OUTPUT=${resultfile}-${testname}.out"
              -D "TEST_EXPECT=0"
              -D "TEST_REFERENCE=${resultfile}-${testname}.ddl"
              -D "TEST_ENV_VAR=HDF5_PLUGIN_PATH"
              -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}/plugins"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
              -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (H5REPACK_UD-${testname}-h5dump PROPERTIES
          DEPENDS H5REPACK_UD-${testname}
      )
      if ("H5REPACK_UD-${testname}-h5dump" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5REPACK_UD-${testname}-h5dump PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME H5REPACK_UD-${testname}-clean-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/out-${testname}.${resultfile}
      )
      set_tests_properties (H5REPACK_UD-${testname}-clean-objects PROPERTIES
          DEPENDS H5REPACK_UD-${testname}-h5dump
      )
    endif ()
  endmacro ()

  macro (ADD_H5_EXTERNAL_TEST testname testtype testfile)
    if (NOT HDF5_USING_ANALYSIS_TOOL)
      # canonical file = h5repack_${testfile}.h5 - preexist
      # external file = h5repack_${testfile}_ex.h5 - preexist
      # repacked file = h5repack_${testfile}_rp.h5 - created
      # external data file = h5repack_${testfile}_ex-0.dat
      if ("${testtype}" STREQUAL "SKIP")
        add_test (
            NAME H5REPACK_EXTERNAL-${testname}
            COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/${testfile} ${PROJECT_BINARY_DIR}/testfiles/out-${testname}.${testfile}"
        )
        set_property(TEST H5REPACK_EXTERNAL-${testname} PROPERTY DISABLED true)
      else ()
        add_test (
            NAME H5REPACK_EXTERNAL-${testname}-clear-objects
            COMMAND ${CMAKE_COMMAND} -E remove h5repack_${testfile}_rp.h5
        )
        set_tests_properties (H5REPACK_EXTERNAL-${testname}-clear-objects PROPERTIES
            WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
        )
        # make sure external data file 0 is available
        add_test (
            NAME H5REPACK_EXTERNAL-${testname}_CPY
            COMMAND ${CMAKE_COMMAND} -E copy_if_different
                "${PROJECT_SOURCE_DIR}/testfiles/h5repack_${testfile}_ex-0.dat" "${PROJECT_BINARY_DIR}/testfiles/h5repack_${testfile}_ex-0.dat"
        )
        set_tests_properties (H5REPACK_EXTERNAL-${testname}_CPY PROPERTIES
            WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
            DEPENDS H5REPACK_EXTERNAL-${testname}-clear-objects
        )
        if ("H5REPACK_EXTERNAL-${testname}_CPY" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
          set_tests_properties (H5REPACK_EXTERNAL-${testname}_CPY PROPERTIES DISABLED true)
        endif ()
        # comparison of known files
        add_test (
            NAME H5REPACK_EXTERNAL-${testname}_DFF1
            COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5diff> --enable-error-stack ${PROJECT_BINARY_DIR}/testfiles/h5repack_${testfile}.h5 ${PROJECT_BINARY_DIR}/testfiles/h5repack_${testfile}_ex.h5
        )
        set_tests_properties (H5REPACK_EXTERNAL-${testname}_DFF1 PROPERTIES
            WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
            DEPENDS H5REPACK_EXTERNAL-${testname}_CPY
        )
        if ("H5REPACK_EXTERNAL-${testname}_DFF1" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
          set_tests_properties (H5REPACK_EXTERNAL-${testname}_DFF1 PROPERTIES DISABLED true)
        endif ()
        # repack the external file to the repacked file
        add_test (
            NAME H5REPACK_EXTERNAL-${testname}
            COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5repack> --enable-error-stack ${ARGN} ${PROJECT_BINARY_DIR}/testfiles/h5repack_${testfile}_ex.h5 ${PROJECT_BINARY_DIR}/testfiles/h5repack_${testfile}_rp.h5
        )
        set_tests_properties (H5REPACK_EXTERNAL-${testname} PROPERTIES
            WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
            DEPENDS H5REPACK_EXTERNAL-${testname}_DFF1
        )
        if ("H5REPACK_EXTERNAL-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
          set_tests_properties (H5REPACK_EXTERNAL-${testname} PROPERTIES DISABLED true)
        endif ()
        # comparison of repacked file to known files
        add_test (
            NAME H5REPACK_EXTERNAL-${testname}_DFF2
            COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5diff> --enable-error-stack ${PROJECT_BINARY_DIR}/testfiles/h5repack_${testfile}_rp.h5 ${PROJECT_BINARY_DIR}/testfiles/h5repack_${testfile}.h5
        )
        set_tests_properties (H5REPACK_EXTERNAL-${testname}_DFF2 PROPERTIES
            WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
            DEPENDS H5REPACK_EXTERNAL-${testname}
        )
        if ("H5REPACK_EXTERNAL-${testname}_DFF2" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
          set_tests_properties (H5REPACK_EXTERNAL-${testname}_DFF2 PROPERTIES DISABLED true)
        endif ()
        add_test (
            NAME H5REPACK_EXTERNAL-${testname}_DFF3
            COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5diff> --enable-error-stack ${PROJECT_BINARY_DIR}/testfiles/h5repack_${testfile}_rp.h5 ${PROJECT_BINARY_DIR}/testfiles/h5repack_${testfile}_ex.h5
        )
        set_tests_properties (H5REPACK_EXTERNAL-${testname}_DFF3 PROPERTIES
            WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
            DEPENDS H5REPACK_EXTERNAL-${testname}_DFF2
        )
        if ("H5REPACK_EXTERNAL-${testname}_DFF3" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
          set_tests_properties (H5REPACK_EXTERNAL-${testname}_DFF3 PROPERTIES DISABLED true)
        endif ()
        # invalidate external file by removing its first data file
        add_test (
            NAME H5REPACK_EXTERNAL-${testname}_DATA_RMV
            COMMAND ${CMAKE_COMMAND} -E remove h5repack_${testfile}_ex-0.dat
        )
        set_tests_properties (H5REPACK_EXTERNAL-${testname}_DATA_RMV PROPERTIES
            WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
            DEPENDS H5REPACK_EXTERNAL-${testname}_DFF3
        )
        if ("H5REPACK_EXTERNAL-${testname}_DATA_RMV" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
          set_tests_properties (H5REPACK_EXTERNAL-${testname}_DATA_RMV PROPERTIES DISABLED true)
        endif ()
        # verify comparison of repacked file to known file
        add_test (
            NAME H5REPACK_EXTERNAL-${testname}_DFF4
            COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5diff> --enable-error-stack ${PROJECT_BINARY_DIR}/testfiles/h5repack_${testfile}_rp.h5 ${PROJECT_BINARY_DIR}/testfiles/h5repack_${testfile}.h5
        )
        set_tests_properties (H5REPACK_EXTERNAL-${testname}_DFF4 PROPERTIES
            WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
            DEPENDS H5REPACK_EXTERNAL-${testname}_DATA_RMV
        )
        if ("H5REPACK_EXTERNAL-${testname}_DFF4" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
          set_tests_properties (H5REPACK_EXTERNAL-${testname}_DFF4 PROPERTIES DISABLED true)
        endif ()
        # verify comparison of repacked file to known external file fails
        add_test (
            NAME H5REPACK_EXTERNAL-${testname}_DFF_FAIL
            COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5diff> --enable-error-stack ${PROJECT_BINARY_DIR}/testfiles/h5repack_${testfile}_rp.h5 ${PROJECT_BINARY_DIR}/testfiles/h5repack_${testfile}_ex.h5
        )
        set_tests_properties (H5REPACK_EXTERNAL-${testname}_DFF_FAIL PROPERTIES
            WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
            DEPENDS H5REPACK_EXTERNAL-${testname}_DFF4
            WILL_FAIL "true"
        )
        if ("H5REPACK_EXTERNAL-${testname}_DFF_FAIL" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
          set_tests_properties (H5REPACK_EXTERNAL-${testname}_DFF_FAIL PROPERTIES DISABLED true)
        endif ()
        add_test (
            NAME H5REPACK_EXTERNAL-${testname}-clean-objects
            COMMAND ${CMAKE_COMMAND} -E remove h5repack_${testfile}_rp.h5
        )
        set_tests_properties (H5REPACK_EXTERNAL-${testname}-clean-objects PROPERTIES
            WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles"
            DEPENDS H5REPACK_EXTERNAL-${testname}_DFF_FAIL
        )
      endif ()
    endif ()
  endmacro ()

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

  # --------------------------------------------------------------------
  # test file names
  # --------------------------------------------------------------------
  set (INFO_FILE testfiles/h5repack.info)

  set (FILE0 h5repack_fill.h5)
  set (FILE1 h5repack_objs.h5)
  set (FILE2 h5repack_attr.h5)
  set (FILE3 h5repack_hlink.h5)
  set (FILE4 h5repack_layout.h5)
  set (FILE5 h5repack_early.h5)
  set (FILE7 h5repack_szip.h5)
  set (FILE8 h5repack_deflate.h5)
  set (FILE9 h5repack_shuffle.h5)
  set (FILE10 h5repack_fletcher.h5)
  set (FILE11 h5repack_filters.h5)
  set (FILE12 h5repack_nbit.h5)
  set (FILE13 h5repack_soffset.h5)
  set (FILE14 h5repack_layouto.h5 )     # A file with an older version of the layout message (copy of test/tlayouto.h5)
  set (FILE15 h5repack_named_dtypes.h5)
  set (FILE16 tfamily%05d.h5)           # located in common testfiles folder
  set (FILE18 h5repack_layout2.h5)
  set (FILE19 h5repack_layout3.h5)
  set (FILE_REF h5repack_refs.h5)
  set (FILE_ATTR_REF h5repack_attr_refs.h5)
  set (FILEV1 1_vds.h5)
  set (FILEV2 2_vds.h5)
  set (FILEV3_1 3_1_vds.h5)
  set (FILEV3_2 3_2_vds.h5)
  set (FILEV4 4_vds.h5)
  set (FILEV5 5_vds.h5)

  ADD_HELP_TEST(help 0 -h)

  add_test (NAME H5REPACK-testh5repack_detect_szip COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:testh5repack_detect_szip>)
  if (HDF5_ENABLE_SZIP_SUPPORT)
    if (HDF5_ENABLE_SZIP_ENCODING)
      set (passRegex "yes")
      set_tests_properties (H5REPACK-testh5repack_detect_szip PROPERTIES PASS_REGULAR_EXPRESSION "yes")
    else ()
      set (passRegex "no")
      set_tests_properties (H5REPACK-testh5repack_detect_szip PROPERTIES PASS_REGULAR_EXPRESSION "no")
    endif ()
  else ()
    set (passRegex "no")
    set_tests_properties (H5REPACK-testh5repack_detect_szip PROPERTIES PASS_REGULAR_EXPRESSION "no")
  endif ()
  set_tests_properties (H5REPACK-testh5repack_detect_szip PROPERTIES DEPENDS H5REPACK-h5repack-${testname})
  set (last_test "H5REPACK-testh5repack_detect_szip")

#  add_test (NAME H5REPACK-h5repacktest COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5repacktest>)
#  set_tests_properties (H5REPACK-h5repacktest PROPERTIES DEPENDS H5REPACK-testh5repack_detect_szip)
#  set (last_test "H5REPACK-h5repacktest")
#
# The tests
# We use the files generated by h5repacktst
# Each run generates "<file>.out.h5" and the tool h5diff is used to
# compare the input and output files
#
# the tests are the same as the program h5repacktst, but run from the CLI
#

# See which filters are usable (and skip tests for filters we
# don't have).  Do this by searching H5pubconf.h to see which
# filters are defined.

# detect whether the encoder is present.
#  set (USE_FILTER_SZIP_ENCODER 0)
  if (HDF5_ENABLE_SZIP_ENCODING)
    set (USE_FILTER_SZIP_ENCODER ${testh5repack_detect_szip})
  endif ()

  if (H5_HAVE_FILTER_DEFLATE)
    set (USE_FILTER_DEFLATE 1)
  endif ()

  if (H5_HAVE_FILTER_SZIP)
    set (USE_FILTER_SZIP 1)
  endif ()

# copy files (these files have no filters)
  ADD_H5_TEST (fill "TEST" ${FILE0})
  ADD_H5_TEST (objs "TEST" ${FILE1})
  ADD_H5_TEST (attr "TEST" ${FILE2})
  ADD_H5_TEST (hlink "TEST" ${FILE3})
  ADD_H5_TEST (layout "TEST" ${FILE4})
  ADD_H5_TEST (early "TEST" ${FILE5})

# nested 8bit enum in both deflated and non-deflated datafiles
  if (NOT USE_FILTER_DEFLATE)
    ADD_H5_TEST (nested_8bit_enum "TEST" h5repack_nested_8bit_enum.h5)
  else ()
    ADD_H5_TEST (nested_8bit_enum "TEST" h5repack_nested_8bit_enum_deflated.h5)
  endif ()

# use $FILE4 to write some filters  (this file has  no filters)

# gzip with individual object
  set (arg ${FILE4} -f dset1:GZIP=1  -l dset1:CHUNK=20x10)
  set (TESTTYPE "TEST")
  if (NOT USE_FILTER_DEFLATE)
    set (TESTTYPE "SKIP")
  endif ()
  ADD_H5_TEST (gzip_individual ${TESTTYPE} ${arg})

# gzip for all
  set (arg ${FILE4} -f GZIP=1)
  set (TESTTYPE "TEST")
  if (NOT USE_FILTER_DEFLATE)
    set (TESTTYPE "SKIP")
  endif ()
  ADD_H5_TEST (gzip_all ${TESTTYPE} ${arg})

# szip with individual object
  set (arg ${FILE4} -f dset2:SZIP=8,EC  -l dset2:CHUNK=20x10)
  set (TESTTYPE "TEST")
  if (NOT USE_FILTER_SZIP_ENCODER)
    if (NOT USE_FILTER_SZIP)
      set (TESTTYPE "SKIP")
    endif ()
  endif ()
  ADD_H5_TEST (szip_individual ${TESTTYPE} ${arg})

# szip for all
  set (arg ${FILE4} -f SZIP=8,NN)
  set (TESTTYPE "TEST")
  if (NOT USE_FILTER_SZIP_ENCODER)
    if (NOT USE_FILTER_SZIP)
      set (TESTTYPE "SKIP")
    endif ()
  endif ()
  ADD_H5_TEST (szip_all ${TESTTYPE} ${arg})

# shuffle with individual object
  set (arg ${FILE4} -f dset2:SHUF  -l dset2:CHUNK=20x10)
  ADD_H5_TEST (shuffle_individual "TEST" ${arg})

# shuffle for all
  set (arg ${FILE4} -f SHUF)
  ADD_H5_TEST (shuffle_all "TEST" ${arg})

# fletcher32  with individual object
  set (arg ${FILE4} -f dset2:FLET  -l dset2:CHUNK=20x10)
  ADD_H5_TEST (fletcher_individual "TEST" ${arg})

# fletcher32 for all
  set (arg ${FILE4} -f FLET)
  ADD_H5_TEST (fletcher_all "TEST" ${arg})

# all filters
  set (arg ${FILE4} -f dset2:SHUF -f dset2:FLET -f dset2:SZIP=8,NN -f dset2:GZIP=1 -l dset2:CHUNK=20x10)
  set (TESTTYPE "TEST")
  if (NOT USE_FILTER_SZIP_ENCODER)
    if (NOT USE_FILTER_SZIP OR NOT USE_FILTER_DEFLATE)
      set (TESTTYPE "SKIP")
    endif ()
  endif ()
  ADD_H5_TEST (all_filters ${TESTTYPE} ${arg})

# verbose gzip with individual object
  set (arg ${FILE11} -v -f /dset_deflate:GZIP=9)
  set (TESTTYPE "TEST")
  if (NOT USE_FILTER_DEFLATE)
    set (TESTTYPE "SKIP")
  endif ()
  ADD_H5_FILTER_TEST (gzip_verbose_filters "GZIP   \(0\.[0-9][0-9][0-9]:1\)" "GZIP   (0.XXX:1)" ${TESTTYPE} 0 ${arg})

###########################################################
# the following tests assume the input files have filters
###########################################################

# szip copy
  set (arg ${FILE7})
  set (TESTTYPE "TEST")
  if (NOT USE_FILTER_SZIP_ENCODER)
    if (NOT USE_FILTER_SZIP)
      set (TESTTYPE "SKIP")
    endif ()
  endif ()
  ADD_H5_TEST (szip_copy ${TESTTYPE} ${arg})

# szip remove
  set (arg ${FILE7} --filter=dset_szip:NONE)
  set (TESTTYPE "TEST")
  if (NOT USE_FILTER_SZIP_ENCODER)
    if (NOT USE_FILTER_SZIP)
      set (TESTTYPE "SKIP")
    endif ()
  endif ()
  ADD_H5_TEST (szip_remove ${TESTTYPE} ${arg})

# deflate copy
  set (arg ${FILE8})
  set (TESTTYPE "TEST")
  if (NOT USE_FILTER_DEFLATE)
    set (TESTTYPE "SKIP")
  endif ()
  ADD_H5_TEST (deflate_copy ${TESTTYPE} ${arg})

# deflate remove
  set (arg ${FILE8} -f dset_deflate:NONE)
  set (TESTTYPE "TEST")
  if (NOT USE_FILTER_DEFLATE)
    set (TESTTYPE "SKIP")
  endif ()
  ADD_H5_TEST (deflate_remove ${TESTTYPE} ${arg})

# shuffle copy
  set (arg ${FILE9})
  ADD_H5_TEST (shuffle_copy "TEST" ${arg})

# shuffle remove
  set (arg ${FILE9} -f dset_shuffle:NONE)
  ADD_H5_TEST (shuffle_remove "TEST" ${arg})

# fletcher32 copy
  set (arg ${FILE10})
  ADD_H5_TEST (fletcher_copy "TEST" ${arg})

# fletcher32 remove
  set (arg ${FILE10} -f dset_fletcher32:NONE)
  ADD_H5_TEST (fletcher_remove "TEST" ${arg})

# nbit copy
  set (arg ${FILE12})
  ADD_H5_TEST (nbit_copy "TEST" ${arg})

# nbit remove
  set (arg ${FILE12} -f dset_nbit:NONE)
  ADD_H5_TEST (nbit_remove "TEST" ${arg})

# nbit add
  set (arg ${FILE12} -f dset_int31:NBIT)
  ADD_H5_TEST (nbit_add "TEST" ${arg})

# scaleoffset copy
  set (arg ${FILE13})
  ADD_H5_TEST (scale_copy "TEST" ${arg})

# scaleoffset add
  set (arg ${FILE13} -f dset_none:SOFF=31,IN)
  ADD_H5_TEST (scale_add "TEST" ${arg})

# scaleoffset remove
  set (arg ${FILE13} -f dset_scaleoffset:NONE)
  ADD_H5_TEST (scale_remove "TEST" ${arg})

# remove all  filters
  set (arg ${FILE11} -f NONE)
  set (TESTTYPE "TEST")
  if (NOT USE_FILTER_SZIP_ENCODER)
    if (NOT USE_FILTER_SZIP OR NOT USE_FILTER_DEFLATE)
      set (TESTTYPE "SKIP")
    endif ()
  endif ()
  ADD_H5_TEST (remove_all ${TESTTYPE} ${arg})

#filter conversions
  set (arg ${FILE8} -f dset_deflate:SZIP=8,NN)
  set (TESTTYPE "TEST")
  if (NOT USE_FILTER_SZIP_ENCODER)
    if (NOT USE_FILTER_SZIP OR NOT USE_FILTER_DEFLATE)
      set (TESTTYPE "SKIP")
    endif ()
  endif ()
  ADD_H5_TEST (deflate_convert ${TESTTYPE} ${arg})

  set (arg ${FILE7} -f dset_szip:GZIP=1)
  set (TESTTYPE "TEST")
  if (NOT USE_FILTER_SZIP_ENCODER)
    if (NOT USE_FILTER_SZIP OR NOT USE_FILTER_DEFLATE)
      set (TESTTYPE "SKIP")
    endif ()
  endif ()
  ADD_H5_TEST (szip_convert ${TESTTYPE} ${arg})

#limit
  set (arg ${FILE4} -f GZIP=1 -m 1024)
  set (TESTTYPE "TEST")
  if (NOT USE_FILTER_DEFLATE)
    set (TESTTYPE "SKIP")
  endif ()
  ADD_H5_DMP_MASK (deflate_limit ${TESTTYPE} 0 ${arg})

#file
  set (arg ${FILE4} -e ${INFO_FILE})
  set (TESTTYPE "TEST")
  if (NOT USE_FILTER_DEFLATE)
    set (TESTTYPE "SKIP")
  endif ()
  ADD_H5_TEST (deflate_file ${TESTTYPE} ${arg})

#crtorder
  set (arg tordergr.h5 -L)
  set (TESTTYPE "TEST")
  ADD_H5_DMP_TEST (crtorder ${TESTTYPE} 0 ${arg})

###################################################################################################
# Testing paged aggregation related options:
#   -G pagesize
#   -P 1 or 0
#   -S strategy
#   -T threshold
#
# The testfiles used are generated by test/gen_filespace.c and the file names are prepended with "h5repack_":
#   (1) "fsm_aggr_nopersist.h5"  /* H5F_FSPACE_STRATEGY_FSM_AGGR + not persisting free-space */
#   (2) "fsm_aggr_persist.h5"    /* H5F_FSPACE_STRATEGY_FSM_AGGR + persisting free-space */
#   (3) "paged_nopersist.h5"     /* H5F_FSPACE_STRATEGY_PAGE + not persisting free-space */
#   (4) "paged_persist.h5"       /* H5F_FSPACE_STRATEGY_PAGE + persisting free-space */
#   (5) "aggr.h5"                /* H5F_FSPACE_STRATEGY_AGGR */
#   (6) "none.h5"                /* H5F_FSPACE_STRATEGY_NONE */
#
#####################################################################################################
#
  set (arg h5repack_fsm_aggr_nopersist.h5 -S PAGE -P 1)
  set (TESTTYPE "TEST")
  ADD_H5_STAT_TEST (SP_PAGE ${TESTTYPE} 0 SP ${arg})

  set (arg h5repack_fsm_aggr_persist.h5 -S AGGR)
  set (TESTTYPE "TEST")
  ADD_H5_STAT_TEST (S_AGGR ${TESTTYPE} 0 S ${arg})

  set (arg h5repack_none.h5 -S PAGE -T 10 -G 2048)
  set (TESTTYPE "TEST")
  ADD_H5_STAT_TEST (STG_PAGE ${TESTTYPE} 0 STG ${arg})

  set (arg h5repack_paged_nopersist.h5 -G 512 -S AGGR)
  set (TESTTYPE "TEST")
  ADD_H5_STAT_TEST (GS_AGGR ${TESTTYPE} 0 GS ${arg})

  set (arg h5repack_paged_persist.h5 -S NONE -P 1)
  set (TESTTYPE "TEST")
  ADD_H5_STAT_TEST (SP_NONE ${TESTTYPE} 0 SP ${arg})

  set (arg h5repack_aggr.h5 -S FSM_AGGR -P 1 -T 5)
  set (TESTTYPE "TEST")
  ADD_H5_STAT_TEST (SPT_FSM_AGGR ${TESTTYPE} 0 SPT ${arg})


#########################################################
# layout options (these files have no filters)
#########################################################
  ADD_H5_VERIFY_TEST (dset2_chunk_20x10 "TEST" 0 ${FILE4} dset2 CHUNKED --layout=dset2:CHUNK=20x10)
  ADD_H5_VERIFY_TEST (chunk_20x10 "TEST" 1 ${FILE4} null CHUNKED -l CHUNK=20x10)
  ADD_H5_VERIFY_TEST (dset2_conti "TEST" 0 ${FILE4} dset2 CONTIGUOUS -l dset2:CONTI)
  ADD_H5_VERIFY_TEST (conti "TEST" 1 ${FILE4} null CONTIGUOUS -l CONTI)
  ADD_H5_VERIFY_TEST (dset2_compa "TEST" 0 ${FILE4} dset2 COMPACT -l dset2:COMPA)
  ADD_H5_VERIFY_TEST (compa "TEST" 1 ${FILE4} null COMPACT -l COMPA)
  ADD_H5_MASK_TEST (dset2_chunk_20x10-errstk "TEST" 0 "dimensionality of chunks doesn't match the dataspace" ${FILE4} --layout=dset2:CHUNK=20x10x5 --enable-error-stack)

################################################################
# layout conversions (file has no filters)
###############################################################
  ADD_H5_VERIFY_TEST (dset_compa_conti "TEST" 0 ${FILE4} dset_compact CONTIGUOUS -l dset_compact:CONTI)
  ADD_H5_VERIFY_TEST (dset_compa_chunk "TEST" 0 ${FILE4} dset_compact CHUNKED -l dset_compact:CHUNK=2x5)
  ADD_H5_VERIFY_TEST (dset_compa_compa "TEST" 0 ${FILE4} dset_compact COMPACT -l dset_compact:COMPA)
  ADD_H5_VERIFY_TEST (dset_conti_compa "TEST" 0 ${FILE4} dset_contiguous COMPACT -l dset_contiguous:COMPA)
  ADD_H5_VERIFY_TEST (dset_conti_chunk "TEST" 0 ${FILE4} dset_contiguous CHUNKED -l dset_contiguous:CHUNK=3x6)
  ADD_H5_VERIFY_TEST (dset_conti_conti "TEST" 0 ${FILE4} dset_contiguous CONTIGUOUS -l dset_contiguous:CONTI)
  ADD_H5_VERIFY_TEST (chunk_compa "TEST" 0 ${FILE4} dset_chunk COMPACT -l dset_chunk:COMPA)
  ADD_H5_VERIFY_TEST (chunk_conti "TEST" 0 ${FILE4} dset_chunk CONTIGUOUS -l dset_chunk:CONTI)
  ADD_H5_VERIFY_TEST (chunk_18x13 "TEST" 0 ${FILE4} dset_chunk CHUNKED -l dset_chunk:CHUNK=18x13)

# test convert small size dataset ( < 1k) to compact layout without -m
  ADD_H5_VERIFY_TEST (contig_small_compa "TEST" 0 ${FILE18} contig_small COMPACT -l contig_small:COMPA)
  ADD_H5_VERIFY_TEST (contig_small_fixed_compa "TEST" 0 ${FILE18} chunked_small_fixed COMPACT -l chunked_small_fixed:COMPA)

#---------------------------------------------------------------------------
# Test file contains chunked datasets (need multiple dsets) with
# unlimited max dims.   (HDFFV-7933)
# Use first dset to test.
#---------------------------------------------------------------------------
# chunk to chunk - specify chunk dim bigger than any current dim
  ADD_H5_VERIFY_TEST (chunk2chunk "TEST" 0 ${FILE19} chunk_unlimit1 CHUNK -l chunk_unlimit1:CHUNK=100x300)

# chunk to contiguous
  ADD_H5_VERIFY_TEST (chunk2conti "TEST" 0 ${FILE19} chunk_unlimit1 CONTI -l chunk_unlimit1:CONTI)

# chunk to compact - convert big dataset (should be > 64k) for this purpose,
# should remain as original layout (chunk)
  ADD_H5_VERIFY_TEST (chunk2compa "TEST" 0 ${FILE19} chunk_unlimit1 CHUNK -l chunk_unlimit1:COMPA)

#--------------------------------------------------------------------------
# Test -f for some specific cases. Chunked dataset with unlimited max dims.
# (HDFFV-8012)
#--------------------------------------------------------------------------
# - should not fail
# - should not change max dims from unlimit

# chunk dim is bigger than dataset dim. ( dset size < 64k )
  ADD_H5_VERIFY_TEST (error1 "TEST" 0 ${FILE19} chunk_unlimit1 H5S_UNLIMITED -f chunk_unlimit1:NONE)

# chunk dim is bigger than dataset dim. ( dset size > 64k )
  ADD_H5_VERIFY_TEST (error2 "TEST" 0 ${FILE19} chunk_unlimit2 H5S_UNLIMITED -f chunk_unlimit2:NONE)

# chunk dims are smaller than dataset dims. ( dset size < 64k )
  ADD_H5_VERIFY_TEST (error3 "TEST" 0 ${FILE19} chunk_unlimit3 H5S_UNLIMITED -f chunk_unlimit3:NONE)

# file input - should not fail
  ADD_H5_TEST (error4 "TEST" ${FILE19} -f NONE)

#--------------------------------------------------------------------------
# Test base: Convert CHUNK to CONTI for a chunked dataset with small dataset
# (dset size < 64K) and with unlimited max dims on a condition as follow.
# (HDFFV-8214)
#--------------------------------------------------------------------------
# chunk dim is bigger than dataset dim. should succeed.
  ADD_H5_VERIFY_TEST (ckdim_biger "TEST" 0 ${FILE19} chunk_unlimit2 CONTI -l chunk_unlimit2:CONTI)
# chunk dim is smaller than dataset dim. should succeed.
  ADD_H5_VERIFY_TEST (ckdim_smaller "TEST" 0 ${FILE19} chunk_unlimit3 CONTI -l chunk_unlimit3:CONTI)



# Native option
# Do not use FILE1, as the named dtype will be converted to native, and h5diff will
# report a difference.
  ADD_H5_TEST (native_fill "TEST" ${FILE0} -n)
  ADD_H5_TEST (native_attr "TEST" ${FILE2} -n)

# latest file format with long switches. use FILE4=h5repack_layout.h5 (no filters)
  set (arg --layout CHUNK=20x10 --filter GZIP=1 --minimum=10 --native --latest --compact=8 --indexed=6 --ssize=8[:dtype])
  set (TESTTYPE "TEST")
  if (NOT USE_FILTER_DEFLATE)
    set (TESTTYPE "SKIP")
  endif ()
  ADD_H5_VERIFY_TEST (layout_long_switches ${TESTTYPE} 1 ${FILE4} null CHUNKED ${arg})

# latest file format with short switches. use FILE4=h5repack_layout.h5 (no filters)
  set (arg -l CHUNK=20x10 -f GZIP=1 -m 10 -n -L -c 8 -d 6 -s 8[:dtype])
  set (TESTTYPE "TEST")
  if (NOT USE_FILTER_DEFLATE)
    set (TESTTYPE "SKIP")
  endif ()
  ADD_H5_VERIFY_TEST (layout_short_switches ${TESTTYPE} 1 ${FILE4} null CHUNKED ${arg})

# several global filters
  set (arg ${FILE4} --filter GZIP=1 --filter SHUF)
  set (TESTTYPE "TEST")
  if (NOT USE_FILTER_DEFLATE)
    set (TESTTYPE "SKIP")
  endif ()
  ADD_H5_TEST (global_filters ${TESTTYPE} ${arg})

# syntax of -i infile -o outfile
# latest file format with short switches. use FILE4=h5repack_layout.h5 (no filters)
  set (arg ${FILE4} -l CHUNK=20x10 -f GZIP=1 -m 10 -n -L -c 8 -d 6 -s 8[:dtype])
  set (TESTTYPE "LEGACY")
  if (NOT USE_FILTER_DEFLATE)
    set (TESTTYPE "SKIP")
  endif ()
  ADD_H5_TEST_OLD (old_style_layout_short_switches ${TESTTYPE} ${arg})

# add a userblock to file
  set (arg ${FILE1} -u ${PROJECT_BINARY_DIR}/testfiles/ublock.bin -b 2048)
  ADD_H5_TEST (add_userblock "TEST" ${arg})

# add alignment
  set (arg ${FILE1} -t 1 -a 1)
  ADD_H5_TEST (add_alignment "TEST" ${arg})

# Check repacking file with old version of layout message (should get upgraded
# to new version and be readable, etc.)
  ADD_H5_TEST (upgrade_layout "TEST" ${FILE14})

# test for datum size > H5TOOLS_MALLOCSIZE
  if (NOT USE_FILTER_DEFLATE)
    set (TESTTYPE "SKIP")
  endif ()
  ADD_H5_TEST (gt_mallocsize ${TESTTYPE} ${FILE1} -f GZIP=1)

# Check repacking file with committed datatypes in odd configurations
  ADD_H5_TEST (committed_dt "TEST" ${FILE15})

# tests family driver (file is located in common testfiles folder, uses TOOLTEST1
  ADD_H5_TEST (family "TEST" ${FILE16})

# test various references (bug 1814 and 1726)
  ADD_H5_TEST (bug1814 "TEST" ${FILE_REF})

# test attribute with various references (bug1797 / HDFFV-5932)
# the references in attribute of compound or vlen datatype
  ADD_H5_TEST (HDFFV-5932 "TEST" ${FILE_ATTR_REF})

# Add test for memory leak in attribute. This test is verified by CTEST.
# 1. leak from vlen string
# 2. leak from compound type without reference member
# (HDFFV-7840, )
# Note: this test is experimental for sharing test file among tools
  ADD_H5_TEST (HDFFV-7840 "TEST" h5diff_attr1.h5)

# test CVE-2018-17432 fix
  set (arg --low=1 --high=2 -f GZIP=8 -l dset1:CHUNK=5x6)
  add_test (
      NAME H5REPACK-HDFFV-10590
      COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5repack> ${arg} ${PROJECT_BINARY_DIR}/testfiles/h5repack_CVE-2018-17432.h5 ${PROJECT_BINARY_DIR}/testfiles/out-HDFFV-10590.h5repack_CVE-2018-17432.h5
  )
  set_tests_properties (H5REPACK-HDFFV-10590 PROPERTIES WILL_FAIL "true")

# test CVE-2018-14460 fix
  add_test (
      NAME H5REPACK-HDFFV-11223
      COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5repack> ${PROJECT_BINARY_DIR}/testfiles/h5repack_CVE-2018-14460.h5 ${PROJECT_BINARY_DIR}/testfiles/out-HDFFV-11223.h5repack_CVE-2018-14460.h5
  )
  set_tests_properties (H5REPACK-HDFFV-11223 PROPERTIES WILL_FAIL "true")

# tests for metadata block size option ('-M')
  ADD_H5_TEST_META (meta_short h5repack_layout.h5 -M 8192)
  ADD_H5_TEST_META (meta_long h5repack_layout.h5 --metadata_block_size=8192)

# VDS tests

################################################################
# layout conversions
###############################################################
  set (TESTTYPE "TEST")
  if (NOT USE_FILTER_DEFLATE)
    set (TESTTYPE "SKIP")
  endif ()
  ADD_H5_VERIFY_VDS (vds_dset_chunk20x10x5 ${TESTTYPE} 0 ${FILEV1} vds_dset CHUNKED -l vds_dset:CHUNK=20x10x5)

  set (TESTTYPE "TEST")
  if (NOT USE_FILTER_DEFLATE)
    set (TESTTYPE "SKIP")
  endif ()
  ADD_H5_VERIFY_VDS (vds_chunk2x5x8 ${TESTTYPE} 0 ${FILEV3_1} vds_dset CHUNKED -l vds_dset:CHUNK=2x5x8)

  set (TESTTYPE "TEST")
  if (NOT USE_FILTER_DEFLATE)
    set (TESTTYPE "SKIP")
  endif ()
  ADD_H5_VERIFY_VDS (vds_chunk3x6x9 ${TESTTYPE} 0 ${FILEV2} vds_dset CHUNKED -l vds_dset:CHUNK=3x6x9)

  set (TESTTYPE "TEST")
  if (NOT USE_FILTER_DEFLATE)
    set (TESTTYPE "SKIP")
  endif ()
  ADD_H5_VERIFY_VDS (vds_compa ${TESTTYPE} 0 ${FILEV4} vds_dset COMPACT -l vds_dset:COMPA)

  set (TESTTYPE "TEST")
  if (NOT USE_FILTER_DEFLATE)
    set (TESTTYPE "SKIP")
  endif ()
  ADD_H5_VERIFY_VDS (vds_conti ${TESTTYPE} 0 ${FILEV4} vds_dset CONTIGUOUS -l vds_dset:CONTI)

################################################################
# reference new api conversions
###############################################################
ADD_H5_DMP_TEST (attrregion "TEST" 0 tattrreg.h5)
ADD_H5_DMP_TEST (dataregion "TEST" 0 tdatareg.h5)

##############################################################################
###    V E R S I O N  B O U N D S  T E S T S
##############################################################################
# -j 0 -k 2, superblock will be 0
ADD_H5_VERIFY_SUPERBLOCK (SB_IS_0 h5repack_layout.h5 0 2 0)
# -j 1 -k 2, superblock will be 2
ADD_H5_VERIFY_SUPERBLOCK (SB_IS_2 h5repack_layout.h5 1 2 2)
# -j 2 -k 2, superblock will be 3
ADD_H5_VERIFY_SUPERBLOCK (SB_IS_3 h5repack_layout.h5 2 2 3)
# -j 0 -k 1, file cannot be opened
ADD_H5_VERIFY_INVALIDBOUNDS (latest_latest_invalid bounds_latest_latest.h5 0 1)

##############################################################################
###    E X T E R N A L  S T O R A G E  T E S T S
##############################################################################
ADD_H5_EXTERNAL_TEST (ext_f32le "TEST" f32le -l CONTI)
ADD_H5_EXTERNAL_TEST (ext_int32le_1d "TEST" int32le_1d -l CONTI)
ADD_H5_EXTERNAL_TEST (ext_int32le_2d "TEST" int32le_2d -l CONTI)
ADD_H5_EXTERNAL_TEST (ext_int32le_3d "TEST" int32le_3d -l CONTI)
ADD_H5_EXTERNAL_TEST (ext_uint8be "TEST" uint8be -l CONTI)

##############################################################################
###    E X T E R N A L  L I N K  T E S T S
##############################################################################
### HDFFV-11128 needs fixed to enable the following test
#ADD_H5_DMP_TEST (h5copy_extlinks_src-base "TEST" 0 h5copy_extlinks_src.h5 --enable-error-stack)
ADD_H5_DMP_TEST (tsoftlinks-base "TEST" 0 tsoftlinks.h5 --enable-error-stack)
ADD_H5_DMP_TEST (textlink-base "TEST" 0 textlink.h5 --enable-error-stack)
ADD_H5_DMP_TEST (textlinkfar-base "TEST" 0 textlinkfar.h5 --enable-error-stack)
ADD_H5_DMP_TEST (textlinksrc-base "TEST" 0 textlinksrc.h5 --enable-error-stack)
ADD_H5_DMP_TEST (textlinktar-base "TEST" 0 textlinktar.h5 --enable-error-stack)

ADD_H5_DIFF_TEST (h5copy_extlinks_src-merge "TEST" 0 h5copy_extlinks_src.h5 --merge)
ADD_H5_DIFF_TEST (tsoftlinks-merge "TEST" 1 tsoftlinks.h5 --merge)
ADD_H5_DIFF_TEST (textlink-merge "TEST" 0 textlink.h5 --merge)
### HDFFV-11128 needs fixed to enable the following test
#ADD_H5_DIFF_TEST (textlinkfar-merge "TEST" 1 textlinkfar.h5 --merge)
### HDFFV-11128 needs fixed to enable the following test
#ADD_H5_DIFF_TEST (textlinksrc-merge "TEST" 1 textlinksrc.h5 --merge)
### HDFFV-11128 needs fixed to enable the following test
#ADD_H5_DIFF_TEST (textlinktar-merge "TEST" 1 textlinktar.h5 --merge)

ADD_H5_DMP_TEST (h5copy_extlinks_src-prune "TEST" 0 h5copy_extlinks_src.h5 --prune --enable-error-stack)
ADD_H5_DMP_TEST (tsoftlinks-prune "TEST" 0 tsoftlinks.h5 --prune --enable-error-stack)
ADD_H5_DMP_TEST (textlink-prune "TEST" 0 textlink.h5 --prune --enable-error-stack)
ADD_H5_DMP_TEST (textlinkfar-prune "TEST" 0 textlinkfar.h5 --prune --enable-error-stack)
ADD_H5_DMP_TEST (textlinksrc-prune "TEST" 0 textlinksrc.h5 --prune --enable-error-stack)
ADD_H5_DMP_TEST (textlinktar-prune "TEST" 0 textlinktar.h5 --prune --enable-error-stack)

ADD_H5_DMP_TEST (h5copy_extlinks_src-mergeprune "TEST" 0 h5copy_extlinks_src.h5 --merge --prune --enable-error-stack)
ADD_H5_DMP_TEST (tsoftlinks-mergeprune "TEST" 0 tsoftlinks.h5 --merge --prune --enable-error-stack)
ADD_H5_DMP_TEST (textlink-mergeprune "TEST" 0 textlink.h5 --merge --prune --enable-error-stack)
### HDFFV-11128 needs fixed to enable the following test
#ADD_H5_DMP_TEST (textlinkfar-mergeprune "TEST" 0 textlinkfar.h5 --merge --prune --enable-error-stack)
### HDFFV-11128 needs fixed to enable the following test
#ADD_H5_DMP_TEST (textlinksrc-mergeprune "TEST" 0 textlinksrc.h5 --merge --prune --enable-error-stack)
### HDFFV-11128 needs fixed to enable the following test
#ADD_H5_DMP_TEST (textlinktar-mergeprune "TEST" 0 textlinktar.h5 --merge --prune --enable-error-stack)

ADD_H5_DMP_NO_OPT_TEST (tst_onion_dset_1d "TEST" 0 tst_onion_dset_1d.h5 --src-vfd-name onion --src-vfd-info 1)
ADD_H5_DMP_NO_OPT_TEST (tst_onion_dset_ext "TEST" 0 tst_onion_dset_ext.h5 --src-vfd-name onion --src-vfd-info 1)
ADD_H5_DMP_NO_OPT_TEST (tst_onion_objs "TEST" 0 tst_onion_objs.h5 --src-vfd-name onion --src-vfd-info 1)

##############################################################################
###    P L U G I N  T E S T S
##############################################################################
if (BUILD_SHARED_LIBS)
  ADD_H5_UD_TEST (plugin_version_test 0 h5repack_layout.h5 -v -f UD=260,0,4,9,${H5_VERS_MAJOR},${H5_VERS_MINOR},${H5_VERS_RELEASE})
  ADD_H5_UD_TEST (plugin_test 0 h5repack_layout.h5 -v -f UD=257,0,1,9)
  ADD_H5_UD_TEST (plugin_none 0 h5repack_layout.UD.h5 -v -f NONE)
  # check for no parameters
  ADD_H5_UD_TEST (plugin_zero 0 h5repack_layout.h5 -v -f UD=250,0,0)
endif ()

##############################################################################
##############################################################################
###                         V F D   T E S T S                              ###
##############################################################################
##############################################################################

if (HDF5_TEST_VFD)
  include (CMakeVFDTests.cmake)
endif ()
