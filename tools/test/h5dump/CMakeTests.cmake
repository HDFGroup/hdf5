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
  # Copy all the HDF5 files from the test directory into the source directory
  # --------------------------------------------------------------------
  set (HDF5_REFERENCE_FILES
      charsets.ddl
      err_attr_dspace.ddl
      file_space.ddl
      filter_fail.ddl
      non_existing.ddl
      packedbits.ddl
      tall-1.ddl
      tall-2.ddl
      tall-2A.ddl
      tall-2A0.ddl
      tall-2B.ddl
      tall-3.ddl
      tall-4s.ddl
      tall-5s.ddl
      tall-6.ddl
      tall-7.ddl
      tall-7N.ddl
      tallfilters.ddl
      tarray1.ddl
      tarray1_big.ddl
      tarray2.ddl
      tarray3.ddl
      tarray4.ddl
      tarray5.ddl
      tarray6.ddl
      tarray7.ddl
      tarray8.ddl
      tattr-1.ddl
      tattr-2.ddl
      tattr-3.ddl
      tattr-4_be.ddl
      tattrcontents1.ddl
      tattrcontents2.ddl
      tattrintsize.ddl
      tattrreg.ddl
      tattrregR.ddl
      tbin1.ddl
      tbin1.ddl
      tbin2.ddl
      tbin3.ddl
      tbin4.ddl
      tbinregR.ddl
      tbigdims.ddl
      tbitnopaque_be.ddl
      tbitnopaque_le.ddl
      tboot1.ddl
      tboot2.ddl
      tboot2A.ddl
      tboot2B.ddl
      tchar1.ddl
      tchunked.ddl
      tcmpdattrintsize.ddl
      tcmpdintarray.ddl
      tcmpdints.ddl
      tcmpdintsize.ddl
      tcompound_complex2.ddl
      tcomp-1.ddl
      tcomp-2.ddl
      tcomp-3.ddl
      tcomp-4.ddl
      tcompact.ddl
      tcontents.ddl
      tcontiguos.ddl
      tdatareg.ddl
      tdataregR.ddl
      tdeflate.ddl
      tdset-1.ddl
      tdset-2.ddl
      tdset-3s.ddl
      tempty.ddl
      texceedsubstart.ddl
      texceedsubcount.ddl
      texceedsubstride.ddl
      texceedsubblock.ddl
      texternal.ddl
      textlinksrc.ddl
      textlinkfar.ddl
      textlink.ddl
      tfamily.ddl
      tfill.ddl
      tfletcher32.ddl
      #tfloatsattrs.ddl #native
      #tfloatsattrs.wddl #special for windows
      tfloat16.ddl
      tfloat16_be.ddl
      tfpformat.ddl
      tgroup-1.ddl
      tgroup-2.ddl
      tgrp_comments.ddl
      tgrpnullspace.ddl
      thlink-1.ddl
      thlink-2.ddl
      thlink-3.ddl
      thlink-4.ddl
      thlink-5.ddl
      thyperslab.ddl
      tindicesno.ddl
      tindicessub1.ddl
      tindicessub2.ddl
      tindicessub3.ddl
      tindicessub4.ddl
      tindicesyes.ddl
      tints4dims.ddl
      tints4dimsBlock2.ddl
      tints4dimsBlockEq.ddl
      tints4dimsCount2.ddl
      tints4dimsCountEq.ddl
      tints4dimsStride2.ddl
      tintsattrs.ddl
      tintsnodata.ddl
      tlarge_objname.ddl
      tldouble.ddl
      tldouble_scalar.ddl
      tlonglinks.ddl
      tloop-1.ddl
      tmulti.ddl
      tmultifile.ddl
      #tqmarkfile.ddl
      #tstarfile.ddl
      tnamed_dtype_attr.ddl
      tnestcomp-1.ddl
      tnestedcmpddt.ddl
      tnbit.ddl
      tnoattrdata.ddl
      tnoattrddl.ddl
      tnodata.ddl
      tnoddl.ddl
      tnoddlfile.ddl
      tno-subset.ddl
      tnullspace.ddl
      tordergr1.ddl
      tordergr2.ddl
      tordergr3.ddl
      tordergr4.ddl
      tordergr5.ddl
      torderattr1.ddl
      torderattr2.ddl
      torderattr3.ddl
      torderattr4.ddl
      tordercontents1.ddl
      tordercontents2.ddl
      torderlinks1.ddl
      torderlinks2.ddl
      tperror.ddl
      trawdatafile.ddl
      trawssetfile.ddl
      treadfilter.ddl
      treadintfilter.ddl
      treference.ddl
      tsaf.ddl
      tscalarattrintsize.ddl
      tscalarintattrsize.ddl
      tscalarintsize.ddl
      tscalarstring.ddl
      tscaleoffset.ddl
      tshuffle.ddl
      tslink-1.ddl
      tslink-2.ddl
      tslink-D.ddl
      tsplit_file.ddl
      tstr-1.ddl
      tstr-2.ddl
      tstring.ddl
      tstring2.ddl
      tstringe.ddl
      tszip.ddl
      tudfilter.ddl
      tudlink-1.ddl
      tudlink-2.ddl
      tuserfilter.ddl
      tvldtypes1.ddl
      tvldtypes2.ddl
      tvldtypes3.ddl
      tvldtypes4.ddl
      tvldtypes5.ddl
      tvlenstr_array.ddl
      tvlstr.ddl
      tvms.ddl
      twidedisplay.ddl
      twithddlfile.ddl
      h5dump-help.txt
      out3.h5import
      zerodim.ddl
      #STD_REF_OBJ files
      trefer_attrR.ddl
      trefer_compatR.ddl
      trefer_extR.ddl
      trefer_grpR.ddl
      trefer_obj_delR.ddl
      trefer_objR.ddl
      trefer_paramR.ddl
      trefer_reg_1dR.ddl
      trefer_regR.ddl
      # Onion VFD files
      tst_onion_objs.ddl
      tst_onion_dset_ext.ddl
      tst_onion_dset_1d.ddl
      tst_onion_revision_count.ddl
  )
  set (HDF5_N_REFERENCE_FILES
      tall-3.ddl
      tattr-2.ddl
      tcomp-2.ddl
      thlink-4.ddl
      thlink-5.ddl
      tslink-2.ddl
  )
  set (HDF5_REFERENCE_EXP_FILES
      tall-6.exp
      tnoddlfile.exp
      trawdatafile.exp
      trawssetfile.exp
      tstr2bin2.exp
      tstr2bin6.exp
      twithddl.exp
      twithddlfile.exp
  )
  set (HDF5_REFERENCE_TEST_FILES
      charsets.h5
      err_attr_dspace.h5
      file_space.h5
      filter_fail.h5
      packedbits.h5
      taindices.h5
      tall.h5
      tarray1.h5
      tarray1_big.h5
      tarray2.h5
      tarray3.h5
      tarray4.h5
      tarray5.h5
      tarray6.h5
      tarray7.h5
      tarray8.h5
      tattr.h5
      tattr2.h5
      tattr4_be.h5
      tattrintsize.h5
      tattrreg.h5
      tbigdims.h5
      tbinary.h5
      tbitnopaque.h5
      tchar.h5
      tcmpdattrintsize.h5
      tcmpdintarray.h5
      tcmpdints.h5
      tcmpdintsize.h5
      tcompound.h5
      tcompound_complex.h5
      tcompound_complex2.h5
      tdatareg.h5
      tdset.h5
      tempty.h5
      tsoftlinks.h5
      textlinkfar.h5
      textlinksrc.h5
      textlinktar.h5
      textlink.h5
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
      tfcontents1.h5
      tfcontents2.h5
      tfilters.h5
      tfloatsattrs.h5
      tfloat16.h5
      tfloat16_be.h5
      tfpformat.h5
      tfvalues.h5
      tgroup.h5
      tgrp_comments.h5
      tgrpnullspace.h5
      thlink.h5
      thyperslab.h5
      tints4dims.h5
      tintsattrs.h5
      tintsnodata.h5
      tlarge_objname.h5
      tldouble.h5
      tldouble_scalar.h5
      tlonglinks.h5
      tloop.h5
      tmulti-b.h5
      tmulti-g.h5
      tmulti-l.h5
      tmulti-o.h5
      tmulti-r.h5
      tmulti-s.h5
      tnamed_dtype_attr.h5
      tnestedcomp.h5
      tnestedcmpddt.h5
      tno-subset.h5
      tnullspace.h5
      torderattr.h5
      tordergr.h5
      tsaf.h5
      tscalarattrintsize.h5
      tscalarintattrsize.h5
      tscalarintsize.h5
      tscalarstring.h5
      tslink.h5
      tsplit_file-m.h5
      tsplit_file-r.h5
      tstr.h5
      tstr2.h5
      tstr3.h5
      tudfilter.h5
      tudlink.h5
      tvldtypes1.h5
      tvldtypes2.h5
      tvldtypes3.h5
      tvldtypes4.h5
      tvldtypes5.h5
      tvlenstr_array.h5
      tvlstr.h5
      tvms.h5
      t128bit_float.h5
      tCVE_2018_11206_fill_old.h5
      tCVE_2018_11206_fill_new.h5
      zerodim.h5
      tCVE-2021-37501_attr_decode.h5
      #STD_REF_OBJ files
      trefer_attr.h5
      trefer_compat.h5
      trefer_ext1.h5
      trefer_ext2.h5
      trefer_grp.h5
      trefer_obj_del.h5
      trefer_obj.h5
      trefer_param.h5
      trefer_reg_1d.h5
      trefer_reg.h5
      # Onion VFD files
      tst_onion_objs.h5
      tst_onion_objs.h5.onion
      tst_onion_dset_ext.h5
      tst_onion_dset_ext.h5.onion
      tst_onion_dset_1d.h5
      tst_onion_dset_1d.h5.onion
  )
  set (HDF5_ERROR_REFERENCE_TEST_FILES
      filter_fail.err
      non_existing.err
      tall-1.err
      tall-2A.err
      tall-2A0.err
      tall-2B.err
      tarray1_big.err
      tattrregR.err
      tattr-3.err
      tcomp-3.err
      tdataregR.err
      tdset-2.err
      texceedsubblock.err
      texceedsubcount.err
      texceedsubstart.err
      texceedsubstride.err
      textlink.err
      textlinkfar.err
      textlinksrc.err
      torderlinks1.err
      torderlinks2.err
      tgroup-2.err
      tperror.err
      tslink-D.err
  )

  # make test dir
  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")

  #
  # copy test files from source dir to test dir
  #
  foreach (tst_h5_file ${HDF5_REFERENCE_TEST_FILES})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/${tst_h5_file}" "${PROJECT_BINARY_DIR}/testfiles/std/${tst_h5_file}" "h5dump_std_files")
  endforeach ()

  foreach (tst_exp_file ${HDF5_REFERENCE_EXP_FILES})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/exportfiles/${tst_exp_file}" "${PROJECT_BINARY_DIR}/testfiles/std/${tst_exp_file}" "h5dump_std_files")
  endforeach ()

  foreach (tst_other_file ${HDF5_REFERENCE_FILES})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/expected/${tst_other_file}" "${PROJECT_BINARY_DIR}/testfiles/std/${tst_other_file}" "h5dump_std_files")
  endforeach ()
  
  foreach (tst_h5N_file ${HDF5_N_REFERENCE_FILES})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/expected/${tst_h5N_file}" "${PROJECT_BINARY_DIR}/testfiles/std/${tst_h5N_file}-N" "h5dump_std_files")
  endforeach ()

  foreach (tst_error_file ${HDF5_ERROR_REFERENCE_TEST_FILES})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/errfiles/${tst_error_file}" "${PROJECT_BINARY_DIR}/testfiles/std/${tst_error_file}" "h5dump_std_files")
  endforeach ()

  # --------------------------------------------------------------------
  # Special file handling
  # --------------------------------------------------------------------
  HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/expected/tbin1.ddl" "${PROJECT_BINARY_DIR}/testfiles/std/tbin1LE.ddl" "h5dump_std_files")

  if (WIN32 AND CMAKE_VS_WINDOWS_TARGET_PLATFORM_VERSION VERSION_LESS 10.0.18362.0)
    configure_file(${PROJECT_SOURCE_DIR}/exportfiles/tbinregR.exp ${PROJECT_BINARY_DIR}/testfiles/std/tbinregR.exp NEWLINE_STYLE CRLF)
    #file (READ ${PROJECT_SOURCE_DIR}/exportfiles/tbinregR.exp TEST_STREAM)
    #file (WRITE ${PROJECT_BINARY_DIR}/testfiles/std/tbinregR.exp "${TEST_STREAM}")
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/expected/tfloatsattrs.wddl" "${PROJECT_BINARY_DIR}/testfiles/std/tfloatsattrs.ddl" "h5dump_std_files")
  else ()
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/exportfiles/tbinregR.exp" "${PROJECT_BINARY_DIR}/testfiles/std/tbinregR.exp" "h5dump_std_files")
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/expected/tfloatsattrs.ddl" "${PROJECT_BINARY_DIR}/testfiles/std/tfloatsattrs.ddl" "h5dump_std_files")
  endif ()
  add_custom_target(h5dump_std_files ALL COMMENT "Copying files needed by h5dump_std tests" DEPENDS ${h5dump_std_files_list})

##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  macro (ADD_HELP_TEST testname resultcode)
    # If using memchecker add tests without using scripts
    if (HDF5_USING_ANALYSIS_TOOL)
      add_test (NAME H5DUMP-${testname} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5dump> ${ARGN})
    else ()
      add_test (
          NAME H5DUMP-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=h5dump-${testname}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=h5dump-${testname}.txt"
              -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
    endif ()
    set_tests_properties (H5DUMP-${testname} PROPERTIES
        WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
    )
    if ("H5DUMP-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5DUMP-${testname} PROPERTIES DISABLED true)
    endif ()
  endmacro ()

  macro (ADD_SKIP_H5_TEST skipresultfile skipresultcode testtype)
    if ("${testtype}" STREQUAL "SKIP")
      if (NOT HDF5_USING_ANALYSIS_TOOL)
        add_test (
            NAME H5DUMP-${skipresultfile}
            COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${skipresultfile} ${ARGN}"
        )
        set_property(TEST H5DUMP-${skipresultfile} PROPERTY DISABLED true)
      endif ()
    else ()
      ADD_H5_TEST (${skipresultfile} ${skipresultcode} ${ARGN})
    endif ()
  endmacro ()

  macro (ADD_H5_TEST resultfile resultcode)
    # If using memchecker add tests without using scripts
    if (HDF5_USING_ANALYSIS_TOOL)
      add_test (NAME H5DUMP-${resultfile} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5dump> ${ARGN})
      if (${resultcode})
        set_tests_properties (H5DUMP-${resultfile} PROPERTIES WILL_FAIL "true")
      endif ()
      set_tests_properties (H5DUMP-${resultfile} PROPERTIES
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
      )
    else ()
      add_test (
          NAME H5DUMP-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
    endif ()
    set_tests_properties (H5DUMP-${resultfile} PROPERTIES
        WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
    )
    if ("H5DUMP-${resultfile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5DUMP-${resultfile} PROPERTIES DISABLED true)
    endif ()
  endmacro ()

  macro (ADD_H5_COMP_TEST resultfile resultcode resultvalue)
    # If using memchecker add tests without using scripts
    if (HDF5_USING_ANALYSIS_TOOL)
      add_test (NAME H5DUMP-${resultfile} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5dump> ${ARGN})
      if (${resultcode})
        set_tests_properties (H5DUMP-${resultfile} PROPERTIES WILL_FAIL "true")
      endif ()
      set_tests_properties (H5DUMP-${resultfile} PROPERTIES
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
      )
    else ()
      add_test (
          NAME H5DUMP-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -D "TEST_FILTER:STRING=SIZE [0-9]* \\(${resultvalue}\\\.[0-9][0-9][0-9]:1 COMPRESSION\\)"
              -D "TEST_FILTER_REPLACE:STRING=SIZE XXXX (${resultvalue}.XXX:1 COMPRESSION)"
              -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
    endif ()
    set_tests_properties (H5DUMP-${resultfile} PROPERTIES
        WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
    )
    if ("H5DUMP-${resultfile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5DUMP-${resultfile} PROPERTIES DISABLED true)
    endif ()
  endmacro ()

  macro (ADD_H5_TEST_N resultfile resultcode)
    add_test (
        NAME H5DUMP-N-${resultfile}-clear-objects
        COMMAND ${CMAKE_COMMAND} -E remove
            ${resultfile}-N.bin
    )
    set_tests_properties (H5DUMP-N-${resultfile}-clear-objects PROPERTIES
        WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
    )
    # If using memchecker add tests without using scripts
    if (HDF5_USING_ANALYSIS_TOOL)
      add_test (NAME H5DUMP-N-${resultfile} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5dump> ${ARGN})
      if (${resultcode})
        set_tests_properties (H5DUMP-N-${resultfile} PROPERTIES WILL_FAIL "true")
      endif ()
      set_tests_properties (H5DUMP-N-${resultfile} PROPERTIES
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
      )
    else ()
      add_test (
          NAME H5DUMP-N-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=${resultfile}-N.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}-N.ddl"
              -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
    endif ()
    set_tests_properties (H5DUMP-N-${resultfile} PROPERTIES
        DEPENDS H5DUMP-N-${resultfile}-clear-objects
    )
    if ("H5DUMP-N-${resultfile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5DUMP-N-${resultfile} PROPERTIES DISABLED true)
    endif ()
    add_test (
        NAME H5DUMP-N-${resultfile}-clean-objects
        COMMAND ${CMAKE_COMMAND} -E remove
            ${resultfile}-N.bin
    )
    set_tests_properties (H5DUMP-N-${resultfile}-clean-objects PROPERTIES
        DEPENDS H5DUMP-N-${resultfile}
        WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
    )
  endmacro ()

  macro (ADD_H5_TEST_EXPORT resultfile targetfile resultcode)
    add_test (
        NAME H5DUMP-${resultfile}-clear-objects
        COMMAND ${CMAKE_COMMAND} -E remove
            ${resultfile}.txt
    )
    set_tests_properties (H5DUMP-${resultfile}-clear-objects PROPERTIES
        WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
    )
    # If using memchecker add tests without using scripts
    if (HDF5_USING_ANALYSIS_TOOL)
      add_test (NAME H5DUMP-${resultfile} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5dump> ${ARGN} ${resultfile}.txt ${targetfile})
      if (${resultcode})
        set_tests_properties (H5DUMP-${resultfile} PROPERTIES WILL_FAIL "true")
      endif ()
      set_tests_properties (H5DUMP-${resultfile} PROPERTIES
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
      )
    else ()
      add_test (
          NAME H5DUMP-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${ARGN};${resultfile}.txt;${targetfile}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (H5DUMP-${resultfile} PROPERTIES
          DEPENDS H5DUMP-${resultfile}-clear-objects
      )
      if ("H5DUMP-${resultfile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5DUMP-${resultfile} PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME H5DUMP-${resultfile}-output-cmp
          COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol ${resultfile}.txt ${resultfile}.exp
      )
      set_tests_properties (H5DUMP-${resultfile}-output-cmp PROPERTIES
          DEPENDS H5DUMP-${resultfile}
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
      )
      set_tests_properties (H5DUMP-${resultfile}-output-cmp PROPERTIES DEPENDS H5DUMP-${resultfile})
      if ("H5DUMP-${resultfile}-output-cmp" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5DUMP-${resultfile}-output-cmp PROPERTIES DISABLED true)
      endif ()
    endif ()
    add_test (
        NAME H5DUMP-${resultfile}-clean-objects
        COMMAND ${CMAKE_COMMAND} -E remove
            ${resultfile}.txt
    )
    if (HDF5_USING_ANALYSIS_TOOL)
      set_tests_properties (H5DUMP-${resultfile}-clean-objects PROPERTIES
          DEPENDS H5DUMP-${resultfile}
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
      )
    else ()
      set_tests_properties (H5DUMP-${resultfile}-clean-objects PROPERTIES
          DEPENDS H5DUMP-${resultfile}-output-cmp
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
      )
    endif ()
  endmacro ()

  macro (ADD_H5_TEST_EXPORT_DDL resultfile targetfile resultcode ddlfile)
    add_test (
        NAME H5DUMP-${resultfile}-clear-objects
        COMMAND ${CMAKE_COMMAND} -E remove
            ${ddlfile}.txt
            ${resultfile}.txt
    )
    set_tests_properties (H5DUMP-${resultfile}-clear-objects PROPERTIES
        WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
    )
    # If using memchecker add tests without using scripts
    if (HDF5_USING_ANALYSIS_TOOL)
      add_test (NAME H5DUMP-${resultfile} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5dump> --ddl=${ddlfile}.txt ${ARGN} ${resultfile}.txt ${targetfile})
      if (${resultcode})
        set_tests_properties (H5DUMP-${resultfile} PROPERTIES WILL_FAIL "true")
      endif ()
      set_tests_properties (H5DUMP-${resultfile} PROPERTIES
          DEPENDS H5DUMP-${resultfile}-clear-objects
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
      )
    else ()
      add_test (
          NAME H5DUMP-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=--ddl=${ddlfile}.txt;${ARGN};${resultfile}.txt;${targetfile}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (H5DUMP-${resultfile} PROPERTIES
          DEPENDS H5DUMP-${resultfile}-clear-objects
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
      )
      if ("H5DUMP-${resultfile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5DUMP-${resultfile} PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME H5DUMP-${resultfile}-output-cmp
          COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol ${resultfile}.txt ${resultfile}.exp
      )
      set_tests_properties (H5DUMP-${resultfile}-output-cmp PROPERTIES
          DEPENDS H5DUMP-${resultfile}
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
      )
      add_test (
          NAME H5DUMP-${resultfile}-output-cmp-ddl
          COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol ${ddlfile}.txt ${ddlfile}.exp
      )
      set_tests_properties (H5DUMP-${resultfile}-output-cmp-ddl PROPERTIES
          DEPENDS H5DUMP-${resultfile}-output-cmp
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
      )
      if ("H5DUMP-${resultfile}-output-cmp-ddl" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5DUMP-${resultfile}-output-cmp-ddl PROPERTIES DISABLED true)
      endif ()
    endif ()
    add_test (
        NAME H5DUMP-${resultfile}-clean-objects
        COMMAND ${CMAKE_COMMAND} -E remove
            ${ddlfile}.txt
            ${resultfile}.txt
    )
    if (HDF5_USING_ANALYSIS_TOOL)
      set_tests_properties (H5DUMP-${resultfile}-clean-objects PROPERTIES
          DEPENDS H5DUMP-${resultfile}
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
      )
    else ()
      set_tests_properties (H5DUMP-${resultfile}-clean-objects PROPERTIES
          DEPENDS H5DUMP-${resultfile}-output-cmp-ddl
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
      )
    endif ()
  endmacro ()

  macro (ADD_H5_EXPORT_TEST resultfile targetfile resultcode)
    if (NOT HDF5_USING_ANALYSIS_TOOL)
      add_test (
          NAME H5DUMP-output-${resultfile}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove
              ${resultfile}.txt
      )
      set_tests_properties (H5DUMP-output-${resultfile}-clear-objects PROPERTIES
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
      )
      add_test (
          NAME H5DUMP-output-${resultfile}
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5dump> ${ARGN} ${resultfile}.txt ${targetfile}
      )
      set_tests_properties (H5DUMP-output-${resultfile} PROPERTIES
          DEPENDS H5DUMP-output-${resultfile}-clear-objects
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
      )
      if ("H5DUMP-output-${resultfile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5DUMP-output-${resultfile} PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME H5DUMP-output-cmp-${resultfile}
          COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol ${resultfile}.txt ${resultfile}.exp
      )
      set_tests_properties (H5DUMP-output-cmp-${resultfile} PROPERTIES
          DEPENDS H5DUMP-output-${resultfile}
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
      )
      if ("H5DUMP-output-cmp-${resultfile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5DUMP-output-cmp-${resultfile} PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME H5DUMP-output-${resultfile}-clean-objects
          COMMAND ${CMAKE_COMMAND} -E remove
              ${resultfile}.txt
      )
      set_tests_properties (H5DUMP-output-${resultfile}-clean-objects PROPERTIES
          DEPENDS H5DUMP-output-cmp-${resultfile}
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
      )
    endif ()
  endmacro ()

  macro (ADD_H5_MASK_TEST resultfile resultcode)
    if (NOT HDF5_USING_ANALYSIS_TOOL)
      add_test (
          NAME H5DUMP-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -D "TEST_MASK_ERROR=true"
              -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (H5DUMP-${resultfile} PROPERTIES
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
      )
      if ("H5DUMP-${resultfile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5DUMP-${resultfile} PROPERTIES DISABLED true)
      endif ()
    endif ()
  endmacro ()

  macro (ADD_H5_GREP_TEST resultfile resultcode result_check)
    if (NOT HDF5_USING_ANALYSIS_TOOL)
      add_test (
          NAME H5DUMP-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${result_check}"
              -P "${HDF_RESOURCES_DIR}/grepTest.cmake"
      )
      set_tests_properties (H5DUMP-${resultfile} PROPERTIES
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
      )
      if ("H5DUMP-${resultfile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5DUMP-${resultfile} PROPERTIES DISABLED true)
      endif ()
    endif ()
  endmacro ()

  macro (ADD_H5ERR_MASK_TEST resultfile resultcode result_errcheck)
    if (NOT HDF5_USING_ANALYSIS_TOOL)
      add_test (
          NAME H5DUMP-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -D "TEST_ERRREF=${result_errcheck}"
              -P "${HDF_RESOURCES_DIR}/grepTest.cmake"
      )
      set_tests_properties (H5DUMP-${resultfile} PROPERTIES
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
      )
      if ("H5DUMP-${resultfile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5DUMP-${resultfile} PROPERTIES DISABLED true)
      endif ()
    endif ()
  endmacro ()

  macro (ADD_H5ERR_MASK_ENV_TEST resultfile resultcode result_errcheck envvar envval)
    if (NOT HDF5_USING_ANALYSIS_TOOL)
      add_test (
          NAME H5DUMP-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -D "TEST_ERRREF=${result_errcheck}"
              -D "TEST_ENV_VAR:STRING=${envvar}"
              -D "TEST_ENV_VALUE:STRING=${envval}"
              -P "${HDF_RESOURCES_DIR}/grepTest.cmake"
      )
      set_tests_properties (H5DUMP-${resultfile} PROPERTIES
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
      )
      if ("H5DUMP-${resultfile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5DUMP-${resultfile} PROPERTIES DISABLED true)
      endif ()
    endif ()
  endmacro ()

  macro (ADD_H5_BIN_EXPORT conffile resultcode testfile)
    if (NOT HDF5_USING_ANALYSIS_TOOL)
      add_test (
          NAME H5DUMP-BIN_EXPORT-${conffile}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove
              ${conffile}.bin
      )
      set_tests_properties (H5DUMP-BIN_EXPORT-${conffile}-clear-objects PROPERTIES
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
      )
      add_test (
          NAME H5DUMP-BIN_EXPORT-${conffile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${ARGN};-o;${conffile}.bin;${testfile}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=${conffile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${conffile}.ddl"
              -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (H5DUMP-BIN_EXPORT-${conffile} PROPERTIES
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
      )
      if ("H5DUMP-BIN_EXPORT-${conffile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5DUMP-BIN_EXPORT-${conffile} PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME H5DUMP-BIN_EXPORT-${conffile}-clean-objects
          COMMAND ${CMAKE_COMMAND} -E remove
              ${conffile}.bin
      )
      set_tests_properties (H5DUMP-BIN_EXPORT-${conffile}-clean-objects PROPERTIES
          DEPENDS H5DUMP-BIN_EXPORT-${conffile}
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
      )
    endif ()
  endmacro ()

  macro (ADD_H5_TEST_IMPORT conffile resultfile testfile resultcode)
    # If using memchecker add tests without using scripts
    if (NOT HDF5_USING_ANALYSIS_TOOL)
      add_test (
          NAME H5DUMP-IMPORT-${resultfile}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove
              ${resultfile}.bin
              ${resultfile}.h5
      )
      set_tests_properties (H5DUMP-IMPORT-${resultfile}-clear-objects PROPERTIES
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
      )
      add_test (
          NAME H5DUMP-IMPORT-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${ARGN};-o;${resultfile}.bin;${testfile}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=${conffile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${conffile}.ddl"
              -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (H5DUMP-IMPORT-${resultfile} PROPERTIES
          DEPENDS H5DUMP-IMPORT-${resultfile}-clear-objects
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
      )
      if ("H5DUMP-IMPORT-${resultfile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5DUMP-IMPORT-${resultfile} PROPERTIES DISABLED true)
      endif ()
      add_test (NAME H5DUMP-IMPORT-h5import-${resultfile} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5import> ${resultfile}.bin -c ${conffile}.out -o ${resultfile}.h5)
      set_tests_properties (H5DUMP-IMPORT-h5import-${resultfile} PROPERTIES
          DEPENDS H5DUMP-IMPORT-${resultfile}
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
      )
      if ("H5DUMP-IMPORT-h5import-${resultfile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5DUMP-IMPORT-h5import-${resultfile} PROPERTIES DISABLED true)
      endif ()
      add_test (NAME H5DUMP-IMPORT-h5diff-${resultfile} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5diff> ${testfile} ${resultfile}.h5 /integer /integer)
      set_tests_properties (H5DUMP-IMPORT-h5diff-${resultfile} PROPERTIES
          DEPENDS H5DUMP-IMPORT-h5import-${resultfile}
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
      )
      if ("H5DUMP-IMPORT-h5diff-${resultfile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5DUMP-IMPORT-h5diff-${resultfile} PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME H5DUMP-IMPORT-${resultfile}-clean-objects
          COMMAND ${CMAKE_COMMAND} -E remove
              ${resultfile}.bin
              ${resultfile}.h5
      )
      set_tests_properties (H5DUMP-IMPORT-${resultfile}-clean-objects PROPERTIES
          DEPENDS H5DUMP-IMPORT-h5diff-${resultfile}
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
      )
    endif ()
  endmacro ()

  macro (ADD_H5_UD_TEST testname resultcode resultfile)
    if (NOT HDF5_USING_ANALYSIS_TOOL)
      add_test (
          NAME H5DUMP_UD-${testname}-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5dump>"
              -D "TEST_ARGS:STRING=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/testfiles/std"
              -D "TEST_OUTPUT=${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=${resultfile}.ddl"
              -D "TEST_ENV_VAR=HDF5_PLUGIN_PATH"
              -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}/plugins"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
              -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (H5DUMP_UD-${testname}-${resultfile} PROPERTIES
          WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/std"
      )
      if ("H5DUMP_UD-${testname}-${resultfile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5DUMP_UD-${testname}-${resultfile} PROPERTIES DISABLED true)
      endif ()
    endif ()
  endmacro ()

##############################################################################
##############################################################################
###           T H E   T E S T S                                            ###
##############################################################################
##############################################################################

  ADD_HELP_TEST(help 0 -h)

  # test data output redirection
  #ADD_H5_TEST (tnoddl 0 --enable-error-stack -O -y packedbits.h5)
  ADD_H5_TEST (tnoddl 0 --enable-error-stack --ddl -y packedbits.h5)
  #ADD_H5_TEST (tnodata 0 --enable-error-stack -o packedbits.h5)
  ADD_H5_TEST (tnodata 0 --enable-error-stack --output packedbits.h5)
  ADD_H5_TEST (tnoattrddl 0 --enable-error-stack -O -y tattr.h5)
  ADD_H5_TEST (tnoattrdata 0 --enable-error-stack -A -o tattr.h5)
  ADD_H5_TEST_EXPORT (trawdatafile packedbits.h5 0 --enable-error-stack -y -o)
  ADD_H5_TEST_EXPORT (tnoddlfile packedbits.h5 0 --enable-error-stack -O -y -o)
  ADD_H5_TEST_EXPORT (trawssetfile tdset.h5 0 --enable-error-stack -d "/dset1[1,1;;;]" -y -o)

  ADD_H5_TEST_EXPORT_DDL (twithddlfile packedbits.h5 0 twithddl --enable-error-stack --ddl=twithddl.txt -y -o)

  # test for maximum display datasets
  ADD_H5_TEST (twidedisplay 0 --enable-error-stack -w0 packedbits.h5)

  # test for unwritten datasets
  ADD_H5_TEST (tintsnodata 0 --enable-error-stack -p tintsnodata.h5)

  # test for signed/unsigned datasets
  ADD_H5_TEST (packedbits 0 --enable-error-stack packedbits.h5)
  # test for compound signed/unsigned datasets
  ADD_H5_TEST (tcmpdintarray 0 --enable-error-stack tcmpdintarray.h5)
  ADD_H5_TEST (tcmpdints 0 --enable-error-stack tcmpdints.h5)
  ADD_H5_TEST (tcmpdintsize 0 --enable-error-stack tcmpdintsize.h5)
  # test for signed/unsigned scalar datasets
  ADD_H5_TEST (tscalarintsize 0 --enable-error-stack tscalarintsize.h5)
  # test for signed/unsigned attributes
  ADD_H5_TEST (tattrintsize 0 --enable-error-stack tattrintsize.h5)
  # test for compound signed/unsigned attributes
  ADD_H5_TEST (tcmpdattrintsize 0 --enable-error-stack tcmpdattrintsize.h5)
  # test for signed/unsigned scalar attributes
  ADD_H5_TEST (tscalarattrintsize 0 --enable-error-stack tscalarattrintsize.h5)
  # test for string scalar dataset and attribute
  ADD_H5_TEST (tscalarstring 0 --enable-error-stack tscalarstring.h5)
  # test for signed/unsigned scalar datasets with attributes
  ADD_H5_TEST (tscalarintattrsize 0 --enable-error-stack tscalarintattrsize.h5)
  # test for signed/unsigned datasets attributes
  ADD_H5_TEST (tintsattrs 0 --enable-error-stack tintsattrs.h5)
  # test for displaying groups
  ADD_H5_TEST (tgroup-1 0 --enable-error-stack tgroup.h5)
  # test for displaying the selected groups
  ADD_H5ERR_MASK_TEST (tgroup-2 1 "h5dump error: unable to open group \"/y\"" --enable-error-stack --group=/g2 --group / -g /y tgroup.h5)

  # test for displaying simple space datasets
  ADD_H5_TEST (tdset-1 0 --enable-error-stack tdset.h5)
  # test for displaying selected datasets
  ADD_H5ERR_MASK_TEST (tdset-2 1 "h5dump error: unable to get link info from \"dset3\"" --enable-error-stack -H -d dset1 -d /dset2 --dataset=dset3 tdset.h5)

  # test for displaying attributes
  ADD_H5_TEST (tattr-1 0 --enable-error-stack tattr.h5)
  # test for displaying the selected attributes of string type and scalar space
  ADD_H5_TEST (tattr-2 0 --enable-error-stack -a /\\\\/attr1 --attribute /attr4 --attribute=/attr5 tattr.h5)
  ADD_H5_TEST_N (tattr-2 0 --enable-error-stack -N /\\\\/attr1 --any_path /attr4 --any_path=/attr5 tattr.h5)
  # test for header and error messages
  ADD_H5ERR_MASK_TEST (tattr-3 1 "h5dump error: unable to open attribute \"attr\"" --enable-error-stack --header -a /attr2 --attribute=/attr tattr.h5)
  # test for displaying at least 9 attributes on root from a be machine
  ADD_H5_TEST (tattr-4_be 0 --enable-error-stack tattr4_be.h5)
  # test for displaying attributes in shared datatype (also in group and dataset)
  ADD_H5_TEST (tnamed_dtype_attr 0 --enable-error-stack tnamed_dtype_attr.h5)

  # test for displaying soft links and user-defined links
  ADD_H5_TEST (tslink-1 0 --enable-error-stack tslink.h5)
  ADD_H5_TEST (tudlink-1 0 --enable-error-stack tudlink.h5)
  # test for displaying the selected link
  ADD_H5_TEST (tslink-2 0 --enable-error-stack -l slink2 tslink.h5)
  ADD_H5_TEST_N (tslink-2 0 --enable-error-stack -N slink2 tslink.h5)
  ADD_H5_TEST (tudlink-2 0 --enable-error-stack -l udlink2 tudlink.h5)
  # test for displaying dangling soft links
  ADD_H5ERR_MASK_TEST (tslink-D 0 "component not found" --enable-error-stack -d /slink1 tslink.h5)

  # tests for hard links
  ADD_H5_TEST (thlink-1 0 --enable-error-stack thlink.h5)
  ADD_H5_TEST (thlink-2 0 --enable-error-stack -d /g1/dset2 --dataset /dset1 --dataset=/g1/g1.1/dset3 thlink.h5)
  ADD_H5_TEST (thlink-3 0 --enable-error-stack -d /g1/g1.1/dset3 --dataset /g1/dset2 --dataset=/dset1 thlink.h5)
  ADD_H5_TEST (thlink-4 0 --enable-error-stack -g /g1 thlink.h5)
  ADD_H5_TEST_N (thlink-4 0 --enable-error-stack -N /g1 thlink.h5)
  ADD_H5_TEST (thlink-5 0 --enable-error-stack -d /dset1 -g /g2 -d /g1/dset2 thlink.h5)
  ADD_H5_TEST_N (thlink-5 0 --enable-error-stack -N /dset1 -N /g2 -N /g1/dset2 thlink.h5)

  # tests for compound data types
  ADD_H5_TEST (tcomp-1 0 --enable-error-stack tcompound.h5)
  # test for named data types
  ADD_H5_TEST (tcomp-2 0 --enable-error-stack -t /type1 --datatype /type2 --datatype=/group1/type3 tcompound.h5)
  ADD_H5_TEST_N (tcomp-2 0 --enable-error-stack -N /type1 --any_path /type2 --any_path=/group1/type3 tcompound.h5)
  # test for unnamed type
  ADD_H5ERR_MASK_TEST (tcomp-3 0 "object '#6632' doesn't exist" "--enable-error-stack;-t;/#6632;-g;/group2;tcompound.h5")
  # test complicated compound datatype
  ADD_H5_TEST (tcomp-4 0 --enable-error-stack tcompound_complex.h5)
  ADD_H5_TEST (tcompound_complex2 0 --enable-error-stack tcompound_complex2.h5)
  # tests for bitfields and opaque data types
  if (H5_WORDS_BIGENDIAN)
    ADD_H5_TEST (tbitnopaque_be 0 --enable-error-stack tbitnopaque.h5)
  else ()
    ADD_H5_TEST (tbitnopaque_le 0 --enable-error-stack tbitnopaque.h5)
  endif ()

  # test for the nested compound type
  ADD_H5_TEST (tnestcomp-1 0 --enable-error-stack tnestedcomp.h5)
  ADD_H5_TEST (tnestedcmpddt 0 --enable-error-stack tnestedcmpddt.h5)

  # test for options
  ADD_H5ERR_MASK_TEST (tall-1 0 "unable to open external file, external link file name = 'somefile'" --enable-error-stack tall.h5)
  ADD_H5_TEST (tall-2 0 --enable-error-stack --header -g /g1/g1.1 -a attr2 tall.h5)
  ADD_H5_TEST (tall-3 0 --enable-error-stack -d /g2/dset2.1 -l /g1/g1.2/g1.2.1/slink tall.h5)
  ADD_H5_TEST_N (tall-3 0 --enable-error-stack -N /g2/dset2.1 -N /g1/g1.2/g1.2.1/slink tall.h5)
  ADD_H5_TEST (tall-7 0 --enable-error-stack -a attr1 tall.h5)
  ADD_H5_TEST (tall-7N 0 --enable-error-stack -N attr1 tall.h5)

  # test for loop detection
  ADD_H5_TEST (tloop-1 0 --enable-error-stack tloop.h5)

  # test for string
  ADD_H5_TEST (tstr-1 0 --enable-error-stack tstr.h5)
  ADD_H5_TEST (tstr-2 0 --enable-error-stack tstr2.h5)

  # test for file created by Lib SAF team
  ADD_H5_TEST (tsaf 0 --enable-error-stack tsaf.h5)

  # test for file with variable length data
  ADD_H5_TEST (tvldtypes1 0 --enable-error-stack tvldtypes1.h5)
  ADD_H5_TEST (tvldtypes2 0 --enable-error-stack tvldtypes2.h5)
  ADD_H5_TEST (tvldtypes3 0 --enable-error-stack tvldtypes3.h5)
  ADD_H5_TEST (tvldtypes4 0 --enable-error-stack tvldtypes4.h5)
  ADD_H5_TEST (tvldtypes5 0 --enable-error-stack tvldtypes5.h5)

  # test for file with variable length string data
  ADD_H5_TEST (tvlstr 0 --enable-error-stack tvlstr.h5)
  ADD_H5_TEST (tvlenstr_array 0 --enable-error-stack tvlenstr_array.h5)

  # test for files with array data
  ADD_H5_TEST (tarray1 0 --enable-error-stack tarray1.h5)
  # # added for bug# 2092 - tarray1_big.h5
  ADD_H5ERR_MASK_TEST (tarray1_big 0 "NULL token size" --enable-error-stack -R tarray1_big.h5)
  ADD_H5_TEST (tarray2 0 --enable-error-stack tarray2.h5)
  ADD_H5_TEST (tarray3 0 --enable-error-stack tarray3.h5)
  ADD_H5_TEST (tarray4 0 --enable-error-stack tarray4.h5)
  ADD_H5_TEST (tarray5 0 --enable-error-stack tarray5.h5)
  ADD_H5_TEST (tarray6 0 --enable-error-stack tarray6.h5)
  ADD_H5_TEST (tarray7 0 --enable-error-stack tarray7.h5)
  ADD_H5_TEST (tarray8 0 --enable-error-stack tarray8.h5)

  # test for wildcards in filename (does not work with cmake)
  #ADD_H5_MASK_TEST (tstarfile 0 --enable-error-stack -H -d Dataset1 tarr*.h5)
  #ADD_H5_MASK_TEST (tqmarkfile 0 --enable-error-stack -H -d Dataset1 tarray?.h5)
  ADD_H5_TEST (tmultifile 0 --enable-error-stack -H -d Dataset1 tarray2.h5 tarray3.h5 tarray4.h5 tarray5.h5 tarray6.h5 tarray7.h5)

  # test for files with empty data
  ADD_H5_TEST (tempty 0 --enable-error-stack tempty.h5)

  # test for files with groups that have comments
  ADD_H5_TEST (tgrp_comments 0 --enable-error-stack tgrp_comments.h5)

  # test the --filedriver flag
  ADD_H5_TEST (tsplit_file 0 --enable-error-stack --filedriver=split tsplit_file)
  ADD_H5_TEST (tfamily 0 --enable-error-stack --filedriver=family tfamily%05d.h5)
  ADD_H5_TEST (tmulti 0 --enable-error-stack --filedriver=multi tmulti)

  # test for files with group names which reach > 1024 bytes in size
  ADD_H5_TEST (tlarge_objname 0 --enable-error-stack -w157 tlarge_objname.h5)

  # test '-A' to suppress data but print attr's
  ADD_H5ERR_MASK_TEST (tall-2A 0 "unable to open external file, external link file name = 'somefile'" --enable-error-stack -A tall.h5)

  # test '-A' to suppress attr's but print data
  ADD_H5ERR_MASK_TEST (tall-2A0 0 "unable to open external file, external link file name = 'somefile'" --enable-error-stack -A 0 tall.h5)

  # test '-r' to print attributes in ASCII instead of decimal
  ADD_H5ERR_MASK_TEST (tall-2B 0 "unable to open external file, external link file name = 'somefile'" --enable-error-stack -A -r tall.h5)

  # test Subsetting
  ADD_H5_TEST (tall-4s 0 --enable-error-stack --dataset=/g1/g1.1/dset1.1.1 --start=1,1 --stride=2,3 --count=3,2 --block=1,1 tall.h5)
  ADD_H5_TEST (tall-5s 0 --enable-error-stack -d "/g1/g1.1/dset1.1.2[0;2;10;]" tall.h5)
  ADD_H5_TEST (tdset-3s 0 --enable-error-stack -d "/dset1[1,1;;;]" tdset.h5)
  ADD_H5_TEST (tno-subset 0 --enable-error-stack --no-compact-subset -d "AHFINDERDIRECT::ah_centroid_t[0] it=0 tl=0" tno-subset.h5)

  ADD_H5_TEST (tints4dimsCount2 0 --enable-error-stack -d FourDimInts -s 0,0,0,0 -c 2,2,2,2 tints4dims.h5)
  ADD_H5_TEST (tints4dimsBlock2 0 --enable-error-stack -d FourDimInts -s 0,0,0,0 -c 1,1,1,1 -k 2,2,2,2 tints4dims.h5)
  ADD_H5_TEST (tints4dimsStride2 0 --enable-error-stack -d FourDimInts -s 0,0,0,0 -S 2,2,2,2 -c 2,2,2,2 tints4dims.h5)
  ADD_H5_TEST (tints4dimsCountEq 0 --enable-error-stack -d FourDimInts -s 0,0,0,0 -S 2,2,1,1 -k 1,2,1,1 -c 2,2,4,4 tints4dims.h5)
  ADD_H5_TEST (tints4dimsBlockEq 0 --enable-error-stack -d FourDimInts -s 0,0,0,0 -S 2,2,1,1 -c 2,2,1,1 -k 1,2,4,4 tints4dims.h5)

  # test printing characters in ASCII instead of decimal
  ADD_H5_TEST (tchar1 0 --enable-error-stack -r tchar.h5)

  # test datatypes in ASCII and UTF8
  ADD_H5_TEST (charsets 0 --enable-error-stack charsets.h5)

  # rev. 2004
  # tests for super block
  ADD_H5_TEST (tboot1 0 --enable-error-stack -H -B -d dset tfcontents1.h5)
  ADD_H5_TEST (tboot2 0 --enable-error-stack -B tfcontents2.h5)
  ADD_H5_TEST (tboot2A 0 --enable-error-stack --boot-block tfcontents2.h5)
  ADD_H5_TEST (tboot2B 0 --enable-error-stack --superblock tfcontents2.h5)
  ADD_H5_TEST (file_space 0 --enable-error-stack -B file_space.h5)

  # test -p with a non existing dataset
  ADD_H5ERR_MASK_TEST (tperror 1 "h5dump error: unable to get link info from \"bogus\"" --enable-error-stack -p -d bogus tfcontents1.h5)

  # test for file contents
  ADD_H5_TEST (tcontents 0 --enable-error-stack -n tfcontents1.h5)
  ADD_H5_TEST (tordercontents1 0 --enable-error-stack -n --sort_by=name --sort_order=ascending tfcontents1.h5)
  ADD_H5_TEST (tordercontents2 0 --enable-error-stack -n --sort_by=name --sort_order=descending tfcontents1.h5)
  ADD_H5_TEST (tattrcontents1 0 --enable-error-stack -n 1 --sort_order=ascending tall.h5)
  ADD_H5_TEST (tattrcontents2 0 --enable-error-stack -n 1 --sort_order=descending tall.h5)

  # tests for storage layout
  # compact
  ADD_H5_TEST (tcompact 0 --enable-error-stack -H -p -d compact tfilters.h5)
  # contiguous
  ADD_H5_TEST (tcontiguos 0 --enable-error-stack -H -p -d contiguous tfilters.h5)
  # chunked
  ADD_H5_TEST (tchunked 0 --enable-error-stack -H -p -d chunked tfilters.h5)
  # external
  ADD_H5_TEST (texternal 0 --enable-error-stack -H -p -d external tfilters.h5)

  # fill values
  ADD_H5_TEST (tfill 0 --enable-error-stack -p tfvalues.h5)

  # several datatype, with references , print path
  ADD_H5_TEST (treference 0 --enable-error-stack  tattr2.h5)

  # escape/not escape non printable characters
  ADD_H5_TEST (tstringe 0 --enable-error-stack -e tstr3.h5)
  ADD_H5_TEST (tstring 0 --enable-error-stack tstr3.h5)
  # char data as ASCII with non escape
  ADD_H5_TEST (tstring2 0 --enable-error-stack -r -d str4 tstr3.h5)

  # array indices print/not print
  ADD_H5_TEST (tindicesyes 0 --enable-error-stack taindices.h5)
  ADD_H5_TEST (tindicesno 0 --enable-error-stack -y taindices.h5)

  ########## array indices with subsetting
  # 1D case
  ADD_H5_TEST (tindicessub1 0 --enable-error-stack -d 1d -s 1 -S 10 -c 2  -k 3 taindices.h5)

  # 2D case
  ADD_H5_TEST (tindicessub2 0 --enable-error-stack -d 2d -s 1,2  -S 3,3 -c 3,2 -k 2,2 taindices.h5)

  # 3D case
  ADD_H5_TEST (tindicessub3 0 --enable-error-stack -d 3d -s 0,1,2 -S 1,3,3 -c 2,2,2  -k 1,2,2  taindices.h5)

  # 4D case
  ADD_H5_TEST (tindicessub4 0 --enable-error-stack -d 4d -s 0,0,1,2  -c 2,2,3,2 -S 1,1,3,3 -k 1,1,2,2  taindices.h5)

  # Exceed the dimensions for subsetting
  ADD_H5_TEST (texceedsubstart 1 --enable-error-stack -d 1d -s 1,3 taindices.h5)
  ADD_H5_TEST (texceedsubcount 1 --enable-error-stack -d 1d -c 1,3 taindices.h5)
  ADD_H5_TEST (texceedsubstride 1 --enable-error-stack -d 1d -S 1,3 taindices.h5)
  ADD_H5_TEST (texceedsubblock 1 --enable-error-stack -d 1d -k 1,3 taindices.h5)

  # tests for filters
  # SZIP
  ADD_H5_COMP_TEST (tszip 0 2 --enable-error-stack -H -p -d szip tfilters.h5)

  # deflate
  ADD_H5_COMP_TEST (tdeflate 0 2 --enable-error-stack -H -p -d deflate tfilters.h5)

  # shuffle
  ADD_H5_TEST (tshuffle 0 --enable-error-stack -H -p -d shuffle tfilters.h5)

  # fletcher32
  ADD_H5_COMP_TEST (tfletcher32 0 0 --enable-error-stack -H -p -d fletcher32  tfilters.h5)

  # nbit
  ADD_H5_COMP_TEST (tnbit 0 1 --enable-error-stack -H -p -d nbit  tfilters.h5)

  # scaleoffset
  ADD_H5_COMP_TEST (tscaleoffset 0 4 --enable-error-stack -H -p -d scaleoffset  tfilters.h5)

  # all
  ADD_H5_COMP_TEST (tallfilters 0 1 --enable-error-stack -H -p -d all  tfilters.h5)

  # user defined
  ADD_H5_TEST (tuserfilter 0 --enable-error-stack -H  -p -d myfilter  tfilters.h5)


# See which filters are usable (and skip tests for filters we
# don't have).  Do this by searching H5pubconf.h to see which
# filters are defined.

# detect whether the encoder is present.
  if (H5_HAVE_FILTER_DEFLATE)
    set (USE_FILTER_DEFLATE "true")
  endif ()

  if (H5_HAVE_FILTER_SZIP)
    set (USE_FILTER_SZIP "true")
  endif ()

  if (USE_FILTER_DEFLATE)
    # data read internal filters
    ADD_H5_TEST (treadintfilter 0 --enable-error-stack -d deflate -d shuffle -d fletcher32 -d nbit -d scaleoffset tfilters.h5)
    if (HDF5_ENABLE_SZIP_SUPPORT)
      # data read all filters
      ADD_H5_TEST (treadfilter 0 --enable-error-stack -d all -d szip tfilters.h5)
    endif ()
  endif ()

  # test for displaying objects with very long names
  ADD_H5_TEST (tlonglinks 0 --enable-error-stack tlonglinks.h5)

  # dimensions over 4GB, print boundary
  ADD_H5_TEST (tbigdims 0 --enable-error-stack -d dset4gb -s 4294967284 -c 22 tbigdims.h5)

  # hyperslab read
  ADD_H5_TEST (thyperslab 0 --enable-error-stack thyperslab.h5)

  # test for displaying dataset and attribute of null space
  ADD_H5_TEST (tnullspace 0 --enable-error-stack tnullspace.h5)
  ADD_H5_TEST (tgrpnullspace 0 -p --enable-error-stack tgrpnullspace.h5)

  # test for displaying dataset and attribute of space with 0 dimension size
  ADD_H5_TEST (zerodim 0 --enable-error-stack zerodim.h5)

  # test for long double (some systems do not have long double)
  ADD_H5_TEST (tfloatsattrs 0 -p --enable-error-stack tfloatsattrs.h5)
  ADD_H5_TEST (tldouble 0 --enable-error-stack tldouble.h5)
  ADD_H5_TEST (tldouble_scalar 0 -p --enable-error-stack tldouble_scalar.h5)

  # Add tests for _Float16 type
  ADD_H5_TEST (tfloat16 0 --enable-error-stack tfloat16.h5)
  ADD_H5_TEST (tfloat16_be 0 --enable-error-stack tfloat16_be.h5)

  # test for vms
  ADD_H5_TEST (tvms 0 --enable-error-stack tvms.h5)

  # test for binary output
  ADD_H5_BIN_EXPORT (tbin1LE 0 tbinary.h5 --enable-error-stack -d integer -b LE)

  # test for string binary output
  ADD_H5_EXPORT_TEST (tstr2bin2 tstr2.h5 0 --enable-error-stack -d /g2/dset2 -b -o)
  ADD_H5_EXPORT_TEST (tstr2bin6 tstr2.h5 0 --enable-error-stack -d /g6/dset6 -b -o)

  # NATIVE default. the NATIVE test can be validated with h5import/h5diff
#  ADD_H5_TEST_IMPORT (tbin1 out1D tbinary.h5 0 --enable-error-stack -d integer -b)

  if (NOT HDF5_USING_ANALYSIS_TOOL)
    ADD_H5_BIN_EXPORT (tbin2 0 tbinary.h5 --enable-error-stack -b BE -d float)
  endif ()

  # the NATIVE test can be validated with h5import/h5diff
#  ADD_H5_TEST_IMPORT (tbin3 out3D tbinary.h5 0 --enable-error-stack -d integer -b NATIVE)

  if (NOT HDF5_USING_ANALYSIS_TOOL)
    ADD_H5_BIN_EXPORT (tbin4 0 tbinary.h5 --enable-error-stack -d double -b FILE)
  endif ()

  # test for dataset region references
  ADD_H5_TEST (tdatareg 0 --enable-error-stack tdatareg.h5)
  ADD_H5ERR_MASK_TEST (tdataregR 0 "NULL token size" --enable-error-stack -R tdatareg.h5)
  ADD_H5_TEST (tattrreg 0 --enable-error-stack tattrreg.h5)
  ADD_H5ERR_MASK_TEST (tattrregR 0 "NULL token size" -R --enable-error-stack tattrreg.h5)
  ADD_H5_EXPORT_TEST (tbinregR tdatareg.h5 0 --enable-error-stack -d /Dataset1 -s 0 -R -y -o)

  # test for 1.12 region references
  ADD_H5_TEST (trefer_attrR 0 --enable-error-stack -R trefer_attr.h5)
  ADD_H5_TEST (trefer_compatR 0 --enable-error-stack -R trefer_compat.h5)
  ADD_H5_TEST (trefer_extR 0 --enable-error-stack -R trefer_ext2.h5)
  ADD_H5_TEST (trefer_grpR 0 --enable-error-stack -R trefer_grp.h5)
  ADD_H5_TEST (trefer_obj_delR 0 --enable-error-stack -R trefer_obj_del.h5)
  ADD_H5_TEST (trefer_objR 0 --enable-error-stack -R trefer_obj.h5)
  ADD_H5_TEST (trefer_paramR 0 --enable-error-stack -R trefer_param.h5)
  ADD_H5_TEST (trefer_regR 0 --enable-error-stack -R trefer_reg.h5)
  ADD_H5_TEST (trefer_reg_1dR 0 --enable-error-stack -R trefer_reg_1d.h5)

  # tests for group creation order
  # "1" tracked, "2" name, root tracked
  ADD_H5_TEST (tordergr1 0 --enable-error-stack --group=1 --sort_by=creation_order --sort_order=ascending tordergr.h5)
  ADD_H5_TEST (tordergr2 0 --enable-error-stack --group=1 --sort_by=creation_order --sort_order=descending tordergr.h5)
  ADD_H5_TEST (tordergr3 0 --enable-error-stack -g 2 -q name -z ascending tordergr.h5)
  ADD_H5_TEST (tordergr4 0 --enable-error-stack -g 2 -q name -z descending tordergr.h5)
  ADD_H5_TEST (tordergr5 0 --enable-error-stack -q creation_order tordergr.h5)

  # tests for attribute order
  ADD_H5_TEST (torderattr1 0 --enable-error-stack -H --sort_by=name --sort_order=ascending torderattr.h5)
  ADD_H5_TEST (torderattr2 0 --enable-error-stack -H --sort_by=name --sort_order=descending torderattr.h5)
  ADD_H5_TEST (torderattr3 0 --enable-error-stack -H --sort_by=creation_order --sort_order=ascending torderattr.h5)
  ADD_H5_TEST (torderattr4 0 --enable-error-stack -H --sort_by=creation_order --sort_order=descending torderattr.h5)

  # tests for link references and order
  ADD_H5ERR_MASK_TEST (torderlinks1 0 "unable to open external file, external link file name = 'fname'" --enable-error-stack --sort_by=name --sort_order=ascending tfcontents1.h5)
  ADD_H5ERR_MASK_TEST (torderlinks2 0 "unable to open external file, external link file name = 'fname'" --enable-error-stack --sort_by=name --sort_order=descending tfcontents1.h5)

  # tests for floating point user defined printf format
  ADD_H5_TEST (tfpformat 0 --enable-error-stack --format=%.7f tfpformat.h5)

  # tests for traversal of external links
  ADD_H5ERR_MASK_TEST (textlinksrc 0 "Too many soft links in path" --enable-error-stack textlinksrc.h5)
  ADD_H5ERR_MASK_TEST (textlinkfar 0 "Too many soft links in path" --enable-error-stack textlinkfar.h5)

  # test for dangling external links
  ADD_H5ERR_MASK_TEST (textlink 0 "unable to open external file, external link file name = 'anotherfile'" --enable-error-stack textlink.h5)

  # test for error stack display (BZ2048)
  ADD_H5ERR_MASK_ENV_TEST (filter_fail 1 "filter plugins disabled" "HDF5_PLUGIN_PRELOAD" "::" --enable-error-stack filter_fail.h5)

  # test for -o -y for dataset with attributes
  ADD_H5_TEST_EXPORT (tall-6 tall.h5 0 --enable-error-stack -d /g1/g1.1/dset1.1.1 -y -o)

  # test for non-existing file
  ADD_H5_TEST (non_existing 1 --enable-error-stack tgroup.h5 non_existing.h5)

  # test to verify HDFFV-10333: error similar to H5O_attr_decode in the jira issue
  ADD_H5_TEST (err_attr_dspace 1 err_attr_dspace.h5)

  # test to verify HDFFV-9407: long double full precision
#  ADD_H5_GREP_TEST (t128bit_float 1 "1.123456789012345" -m %.35Lg t128bit_float.h5)

  # test to verify HDFFV-10480: out of bounds read in H5O_fill_new[old]_decode
  ADD_H5_TEST (tCVE_2018_11206_fill_old 1 tCVE_2018_11206_fill_old.h5)
  ADD_H5_TEST (tCVE_2018_11206_fill_new 1 tCVE_2018_11206_fill_new.h5)

  # test to verify fix for CVE-2021-37501: multiplication overflow in H5O__attr_decode()
  # https://github.com/ST4RF4LL/Something_Found/blob/main/HDF5_v1.13.0_h5dump_heap_overflow.assets/poc
  ADD_H5_TEST (tCVE-2021-37501_attr_decode 1 tCVE-2021-37501_attr_decode.h5)

  # onion VFD tests
  ADD_H5_TEST (tst_onion_objs 0 --enable-error-stack --vfd-name onion --vfd-info 3 tst_onion_objs.h5)
  ADD_H5_TEST (tst_onion_dset_ext 0 --enable-error-stack --vfd-name onion --vfd-info 1 tst_onion_dset_ext.h5)
  ADD_H5_TEST (tst_onion_dset_1d 0 --enable-error-stack --vfd-name onion --vfd-info 1 tst_onion_dset_1d.h5)
  ADD_H5_TEST (tst_onion_revision_count 0 --enable-error-stack --vfd-name onion --vfd-info revision_count tst_onion_objs.h5)


##############################################################################
###    P L U G I N  T E S T S
##############################################################################
if (BUILD_SHARED_LIBS)
  ADD_H5_UD_TEST (h5dump_plugin_test 0 tudfilter --enable-error-stack tudfilter.h5)
endif ()

##############################################################################
##############################################################################
###                         V F D   T E S T S                              ###
##############################################################################
##############################################################################

if (HDF5_TEST_VFD)
  include (CMakeVFDTests.cmake)
endif ()
