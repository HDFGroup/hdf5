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
  set (LIST_HDF5_TEST_FILES
      ${HDF5_TOOLS_TEST_H5COPY_SOURCE_DIR}/testfiles/h5copy_extlinks_src.h5
      ${HDF5_TOOLS_TEST_H5COPY_SOURCE_DIR}/testfiles/h5copy_extlinks_trg.h5
      ${HDF5_TOOLS_TEST_H5COPY_SOURCE_DIR}/testfiles/h5copy_ref.h5
      ${HDF5_TOOLS_TEST_H5COPY_SOURCE_DIR}/testfiles/h5copytst.h5
      ${HDF5_TOOLS_TEST_H5COPY_SOURCE_DIR}/testfiles/tudfilter.h5
      ${HDF5_TOOLS_TEST_H5COPY_SOURCE_DIR}/testfiles/tudfilter2.h5
  )

  set (LIST_OTHER_TEST_FILES
      ${HDF5_TOOLS_TEST_H5COPY_SOURCE_DIR}/testfiles/h5copy_misc1.out
      ${HDF5_TOOLS_TEST_H5COPY_SOURCE_DIR}/testfiles/h5copy_misc1.err
      ${HDF5_TOOLS_TEST_H5COPY_SOURCE_DIR}/testfiles/tudfilter.h5.txt
      ${HDF5_TOOLS_TEST_H5COPY_SOURCE_DIR}/testfiles/tudfilter.h5_ERR.txt
      ${HDF5_TOOLS_TEST_H5COPY_SOURCE_DIR}/testfiles/h5copy_plugin_fail_ERR.out.h5.txt
      ${HDF5_TOOLS_TEST_H5COPY_SOURCE_DIR}/testfiles/h5copy_plugin_test.out.h5.txt
  )

  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")

  foreach (listfiles ${LIST_HDF5_TEST_FILES} ${LIST_OTHER_TEST_FILES})
    get_filename_component(fname "${listfiles}" NAME)
    HDFTEST_COPY_FILE("${listfiles}" "${PROJECT_BINARY_DIR}/testfiles/${fname}" "h5copy_files")
  endforeach ()
  add_custom_target(h5copy_files ALL COMMENT "Copying files needed by h5copy tests" DEPENDS ${h5copy_files_list})

  if (NOT BUILD_SHARED_LIBS)
    set (tgt_ext "")
  else ()
    set (tgt_ext "-shared")
  endif ()

##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  #
  # Perform h5copy according to passing parameters
  #
  macro (ADD_H5_F_TEST testname resultcode infile fparam vparam sparam srcname dparam dstname)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      # Remove any output file left over from previous test run
      add_test (
          NAME H5COPY_F-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove ./testfiles/${testname}.out.h5
      )
    endif ()

    add_test (
        NAME H5COPY_F-${testname}
        COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5copy${tgt_ext}> -f ${fparam} -i ./testfiles/${infile} -o ./testfiles/${testname}.out.h5 ${vparam} ${sparam} ${srcname} ${dparam} ${dstname} ${ARGN}
    )
    if (HDF5_ENABLE_USING_MEMCHECKER)
      if (last_test)
        set_tests_properties (H5COPY_F-${testname} PROPERTIES DEPENDS ${last_test})
      endif ()
    else ()
      set_tests_properties (H5COPY_F-${testname} PROPERTIES DEPENDS H5COPY_F-${testname}-clear-objects)
    endif ()

    # resultcode=2 will cause the test to skip the diff test
    if (NOT ${resultcode} EQUAL 2)
      add_test (
          NAME H5COPY_F-${testname}-DIFF
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5diff${tgt_ext}> -v ./testfiles/${infile} ./testfiles/${testname}.out.h5 ${srcname} ${dstname}
      )
      set_tests_properties (H5COPY_F-${testname}-DIFF PROPERTIES DEPENDS H5COPY_F-${testname})
      if (${resultcode} EQUAL 1)
        set_tests_properties (H5COPY_F-${testname}-DIFF PROPERTIES WILL_FAIL "true")
      endif ()
    endif ()
  endmacro ()

  macro (ADD_H5_TEST testname resultcode infile vparam sparam srcname dparam dstname)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      # Remove any output file left over from previous test run
      add_test (
          NAME H5COPY-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove ./testfiles/${testname}.out.h5
      )
    endif ()

    add_test (
        NAME H5COPY-${testname}
        COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5copy${tgt_ext}> -i ./testfiles/${infile} -o ./testfiles/${testname}.out.h5 ${vparam} ${sparam} ${srcname} ${dparam} ${dstname} ${ARGN}
    )
    if (HDF5_ENABLE_USING_MEMCHECKER)
      if (last_test)
        set_tests_properties (H5COPY-${testname} PROPERTIES DEPENDS ${last_test})
      endif ()
    else ()
      set_tests_properties (H5COPY-${testname} PROPERTIES DEPENDS H5COPY-${testname}-clear-objects)
    endif ()

    # resultcode=2 will cause the test to skip the diff test
    if (NOT ${resultcode} EQUAL 2)
      add_test (
          NAME H5COPY-${testname}-DIFF
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5diff${tgt_ext}> -v ./testfiles/${infile} ./testfiles/${testname}.out.h5 ${srcname} ${dstname}
      )
      set_tests_properties (H5COPY-${testname}-DIFF PROPERTIES DEPENDS H5COPY-${testname})
      if (${resultcode} EQUAL 1)
        set_tests_properties (H5COPY-${testname}-DIFF PROPERTIES WILL_FAIL "true")
      endif ()
    endif ()
  endmacro ()

  macro (ADD_SKIP_H5_TEST testname skipresultfile)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      add_test (
          NAME H5COPY-${testname}-${skipresultfile}
          COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${testname}-${skipresultfile} ${ARGN}"
      )
      set_property(TEST H5COPY-${testname}-${skipresultfile} PROPERTY DISABLED)
    endif ()
  endmacro ()

  macro (ADD_H5_TEST2 testname resultcode infile  psparam pdparam vparam sparam srcname dparam dstname)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      # Remove any output file left over from previous test run
      add_test (
          NAME H5COPY-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove ./testfiles/${testname}.out.h5
      )
    endif ()

    add_test (
        NAME H5COPY-${testname}-prefill
        COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5copy${tgt_ext}> -i ./testfiles/${infile} -o ./testfiles/${testname}.out.h5 -v -s ${psparam} -d ${pdparam}
    )
    if (HDF5_ENABLE_USING_MEMCHECKER)
      if (last_test)
        set_tests_properties (H5COPY-${testname}-prefill PROPERTIES DEPENDS ${last_test})
      endif ()
    else ()
      set_tests_properties (H5COPY-${testname}-prefill PROPERTIES DEPENDS H5COPY-${testname}-clear-objects)
    endif ()

    add_test (
        NAME H5COPY-${testname}
        COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5copy${tgt_ext}> -i ./testfiles/${infile} -o ./testfiles/${testname}.out.h5 ${vparam} ${sparam} ${srcname} ${dparam} ${dstname} ${ARGN}
    )
    set_tests_properties (H5COPY-${testname} PROPERTIES DEPENDS H5COPY-${testname}-prefill)
    # resultcode=2 will cause the test to skip the diff test
    if (NOT ${resultcode} EQUAL 2)
      add_test (
          NAME H5COPY-${testname}-DIFF
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5diff${tgt_ext}> -v ./testfiles/${infile} ./testfiles/${testname}.out.h5 ${srcname} ${dstname}
      )
      set_tests_properties (H5COPY-${testname}-DIFF PROPERTIES DEPENDS H5COPY-${testname})
      if (${resultcode} EQUAL 1)
        set_tests_properties (H5COPY-${testname}-DIFF PROPERTIES WILL_FAIL "true")
      endif ()
    endif ()
  endmacro ()

  macro (ADD_H5_TEST_SAME testname resultcode pfile psparam pdparam vparam sparam srcname dparam dstname)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      # Remove any output file left over from previous test run
      add_test (
          NAME H5COPY_SAME-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove ./testfiles/${testname}.out.h5
      )
    endif ()

    add_test (
        NAME H5COPY_SAME-${testname}-prefill
        COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5copy${tgt_ext}> -i ./testfiles/${pfile} -o ./testfiles/${testname}.out.h5 -v -s ${psparam} -d ${pdparam}
    )
    if (HDF5_ENABLE_USING_MEMCHECKER)
      if (last_test)
        set_tests_properties (H5COPY_SAME-${testname}-prefill PROPERTIES DEPENDS ${last_test})
      endif ()
    else (HDF5_ENABLE_USING_MEMCHECKER)
      set_tests_properties (H5COPY_SAME-${testname}-prefill PROPERTIES DEPENDS H5COPY_SAME-${testname}-clear-objects)
    endif ()

    add_test (
        NAME H5COPY_SAME-${testname}
        COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5copy${tgt_ext}> -i ./testfiles/${testname}.out.h5 -o ./testfiles/${testname}.out.h5 ${vparam} ${sparam} ${srcname} ${dparam} ${dstname} ${ARGN}
    )
    set_tests_properties (H5COPY_SAME-${testname} PROPERTIES DEPENDS H5COPY_SAME-${testname}-prefill)
    # resultcode=2 will cause the test to skip the diff test
    if (NOT ${resultcode} EQUAL 2)
      add_test (
          NAME H5COPY_SAME-${testname}-DIFF
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5diff${tgt_ext}> -v ./testfiles/${testname}.out.h5 ./testfiles/${testname}.out.h5 ${srcname} ${dstname}
      )
      set_tests_properties (H5COPY_SAME-${testname}-DIFF PROPERTIES DEPENDS H5COPY_SAME-${testname})
      if (${resultcode} EQUAL 1)
        set_tests_properties (H5COPY_SAME-${testname}-DIFF PROPERTIES WILL_FAIL "true")
      endif ()
    endif ()
  endmacro ()

  #
  # Similiar to ADD_H5_TEST macro. Compare to outputs from source & target
  # files instead of checking with h5ls.
  #
  macro (ADD_H5_CMP_TEST testname resultcode infile vparam sparam srcname dparam dstname)
    # If using memchecker add tests without using scripts
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME H5COPY-CMP-${testname} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5copy${tgt_ext}> -i ./testfiles/${infile} -o ./testfiles/${testname}.out.h5 ${vparam} ${sparam} ${srcname} ${dparam} ${dstname} ${ARGN})
      if (${resultcode} EQUAL 1)
        set_tests_properties (H5COPY-CMP-${testname} PROPERTIES WILL_FAIL "true")
      endif ()
      if (last_test)
        set_tests_properties (H5COPY-CMP-${testname} PROPERTIES DEPENDS ${last_test})
      endif ()
    else ()
      # Remove any output file left over from previous test run
      add_test (
          NAME H5COPY-CMP-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove ./testfiles/${testname}.out.h5
      )
      add_test (
          NAME H5COPY-CMP-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5copy${tgt_ext}>"
              -D "TEST_ARGS=-i;./testfiles/${infile};-o;./testfiles/${testname}.out.h5;${vparam};${sparam};${srcname};${dparam};${dstname}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=./testfiles/${testname}.out.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=./testfiles/${testname}.out"
              -D "TEST_ERRREF=./testfiles/${testname}.err"
              -D "TEST_MASK=true"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5COPY-CMP-${testname} PROPERTIES DEPENDS H5COPY-CMP-${testname}-clear-objects)
    endif ()
  endmacro ()

  macro (ADD_H5_UD_TEST testname resultcode infile sparam srcname dparam dstname cmpfile)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      # Remove any output file left over from previous test run
      add_test (
          NAME H5COPY_UD-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/${testname}.out.h5
      )
      if (${resultcode} EQUAL 2)
        add_test (
            NAME H5COPY_UD-${testname}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                -D "TEST_PROGRAM=$<TARGET_FILE:h5copy-shared>"
                -D "TEST_ARGS:STRING=-v;-i;./testfiles/${infile};-o;./testfiles/${testname}.out.h5;${sparam};${srcname};${dparam};${dstname}"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                -D "TEST_OUTPUT=./testfiles/${infile}.out"
                -D "TEST_EXPECT=${resultcode}"
                -D "TEST_REFERENCE=./testfiles/${infile}.txt"
                -D "TEST_APPEND=EXIT CODE:"
                -D "TEST_ENV_VAR=HDF5_PLUGIN_PATH"
                -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}"
                -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
                -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
        )
      else ()
        add_test (
            NAME H5COPY_UD-${testname}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                -D "TEST_PROGRAM=$<TARGET_FILE:h5copy-shared>"
                -D "TEST_ARGS:STRING=-v;-i;./testfiles/${infile};-o;./testfiles/${testname}.out.h5;${sparam};${srcname};${dparam};${dstname}"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                -D "TEST_OUTPUT=./testfiles/${infile}.out"
                -D "TEST_EXPECT=${resultcode}"
                -D "TEST_REFERENCE=./testfiles/${infile}.txt"
                -D "TEST_APPEND=EXIT CODE:"
                -D "TEST_ENV_VAR=HDF5_PLUGIN_PATH"
                -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}/plugins"
                -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
                -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
        )
      endif ()
      set_tests_properties (H5COPY_UD-${testname} PROPERTIES DEPENDS H5COPY_UD-${testname}-clear-objects)
      add_test (
          NAME H5COPY_UD-${testname}-DIFF
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5diff-shared>"
              -D "TEST_ARGS:STRING=-v;./testfiles/${cmpfile};./testfiles/${testname}.out.h5;${srcname};${dstname}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=./testfiles/${testname}.out.h5.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=./testfiles/${testname}.out.h5.txt"
              -D "TEST_APPEND=EXIT CODE:"
              -D "TEST_ENV_VAR=HDF5_PLUGIN_PATH"
              -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}/plugins"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5COPY_UD-${testname}-DIFF PROPERTIES DEPENDS H5COPY_UD-${testname})
    endif ()
  endmacro ()

  macro (ADD_H5_UD_ERR_TEST testname resultcode infile sparam srcname dparam dstname cmpfile)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      # Remove any output file left over from previous test run
      add_test (
          NAME H5COPY_UD_ERR-${testname}-clearall-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/${testname}_ERR.out.h5
      )
      if (${resultcode} EQUAL 2)
        add_test (
            NAME H5COPY_UD_ERR-${testname}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                -D "TEST_PROGRAM=$<TARGET_FILE:h5copy-shared>"
                -D "TEST_ARGS:STRING=-v;--enable-error-stack;-i;./testfiles/${infile};-o;./testfiles/${testname}_ERR.out.h5;${sparam};${srcname};${dparam};${dstname}"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                -D "TEST_OUTPUT=./testfiles/${infile}_ERR.out"
                -D "TEST_EXPECT=0"
                -D "TEST_REFERENCE=./testfiles/${infile}_ERR.txt"
                -D "TEST_MASK_ERROR=true"
                -D "TEST_APPEND=EXIT CODE:"
                -D "TEST_ENV_VAR=HDF5_PLUGIN_PATH"
                -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}"
                -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
                -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
        )
      else ()
        add_test (
            NAME H5COPY_UD_ERR-${testname}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                -D "TEST_PROGRAM=$<TARGET_FILE:h5copy-shared>"
                -D "TEST_ARGS:STRING=-v;--enable-error-stack;-i;./testfiles/${infile};-o;./testfiles/${testname}_ERR.out.h5;${sparam};${srcname};${dparam};${dstname}"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                -D "TEST_OUTPUT=./testfiles/${infile}_ERR.out"
                -D "TEST_EXPECT=${resultcode}"
                -D "TEST_REFERENCE=./testfiles/${infile}_ERR.txt"
                -D "TEST_MASK_ERROR=true"
                -D "TEST_APPEND=EXIT CODE:"
                -D "TEST_ENV_VAR=HDF5_PLUGIN_PATH"
                -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}/plugins"
                -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
                -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
        )
      endif ()
      set_tests_properties (H5COPY_UD_ERR-${testname} PROPERTIES DEPENDS H5COPY_UD_ERR-${testname}-clearall-objects)
      add_test (
          NAME H5COPY_UD_ERR-${testname}-DIFF
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5diff-shared>"
              -D "TEST_ARGS:STRING=-v;./testfiles/${cmpfile};./testfiles/${testname}_ERR.out.h5;${srcname};${dstname}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=./testfiles/${testname}_ERR.out.h5.out"
              -D "TEST_EXPECT=0"
              -D "TEST_REFERENCE=./testfiles/${testname}_ERR.out.h5.txt"
              -D "TEST_APPEND=EXIT CODE:"
              -D "TEST_ENV_VAR=HDF5_PLUGIN_PATH"
              -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}/plugins"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
              -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
      set_tests_properties (H5COPY_UD_ERR-${testname}-DIFF PROPERTIES DEPENDS H5COPY_UD_ERR-${testname})
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
  set (HDF_FILE1 h5copytst)
  set (HDF_FILE2 h5copy_ref)
  set (HDF_EXT_SRC_FILE h5copy_extlinks_src)
  set (HDF_EXT_TRG_FILE h5copy_extlinks_trg)

  if (HDF5_ENABLE_USING_MEMCHECKER)
    # Remove any output file left over from previous test run
    add_test (
        NAME H5COPY-clearall-objects
        COMMAND ${CMAKE_COMMAND} -E remove
            simple.out.h5
            chunk.out.h5
            compact.out.h5
            compound.out.h5
            compressed.out.h5
            named_vl.out.h5
            nested_vl.out.h5
            simple_top.out.h5
            dsrename.out.h5
            grp_empty.out.h5
            grp_dsets.out.h5
            grp_nested.out.h5
            simple_group.out.h5
            grp_rename.out.h5
            grp_dsets_rename.out.h5
            A_B1_simple.out.h5
            A_B2_simple2.out.h5
            C_D_simple.out.h5
            E_F_grp_dsets.out.h5
            G_H_grp_nested.out.h5
            region_ref.out.h5
            ext_link.out.h5
            ext_link_f.out.h5
            ext_dangle_noobj.out.h5
            ext_dangle_noobj_f.out.h5
            ext_dangle_nofile.out.h5
            ext_dangle_nofile_f.out.h5
            ext_link_group.out.h5
            ext_link_group_f.out.h5
            samefile1.out.h5
            samefile2.out.h5
            h5copy_misc1.out.h5
    )
    set_tests_properties (H5COPY-clearall-objects PROPERTIES WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")
    if (last_test)
      set_tests_properties (H5COPY-clearall-objects PROPERTIES DEPENDS ${last_test})
    endif ()
    set (last_test "H5COPY-clearall-objects")
  endif ()

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

  # "Test copying various forms of datasets"
  ADD_H5_TEST (simple 0 ${HDF_FILE1}.h5 -v -s simple -d simple)
  ADD_H5_TEST (chunk 0 ${HDF_FILE1}.h5 -v -s chunk -d chunk)
  ADD_H5_TEST (compact 0 ${HDF_FILE1}.h5 -v -s compact -d compact)
  ADD_H5_TEST (compound 0 ${HDF_FILE1}.h5 -v -s compound -d compound)

  if (USE_FILTER_DEFLATE)
    ADD_H5_TEST (compressed 0 ${HDF_FILE1}.h5 -v -s compressed -d compressed)
  else ()
    ADD_H5_TEST (compressed 2 ${HDF_FILE1}.h5 -v -s compressed -d compressed)
  endif ()

  ADD_H5_TEST (named_vl 0 ${HDF_FILE1}.h5 -v -s named_vl -d named_vl)
  ADD_H5_TEST (nested_vl 0 ${HDF_FILE1}.h5 -v -s nested_vl -d nested_vl)
  ADD_H5_TEST (dset_attr 0 ${HDF_FILE1}.h5 -v -s dset_attr -d dset_attr)

  # "Test copying dataset within group in source file to root of destination"
  ADD_H5_TEST (simple_top 0 ${HDF_FILE1}.h5 -v -s grp_dsets/simple -d simple_top)

  # "Test copying & renaming dataset"
  ADD_H5_TEST (dsrename 0 ${HDF_FILE1}.h5 -v -s compound -d rename)

  # "Test copying empty, 'full' & 'nested' groups"
  ADD_H5_TEST (grp_empty 0 ${HDF_FILE1}.h5 -v -s grp_empty -d grp_empty)
  if (USE_FILTER_DEFLATE)
    ADD_H5_TEST (grp_dsets 0 ${HDF_FILE1}.h5 -v -s grp_dsets -d grp_dsets)
    ADD_H5_TEST (grp_nested 0 ${HDF_FILE1}.h5 -v -s grp_nested -d grp_nested)
  else ()
    ADD_H5_TEST (grp_dsets 2 ${HDF_FILE1}.h5 -v -s grp_dsets -d grp_dsets)
    ADD_H5_TEST (grp_nested 2 ${HDF_FILE1}.h5 -v -s grp_nested -d grp_nested)
  endif ()
  ADD_H5_TEST (grp_attr 0 ${HDF_FILE1}.h5 -v -s grp_attr -d grp_attr)

  # "Test copying dataset within group in source file to group in destination"
  ADD_H5_TEST2 (simple_group 0 ${HDF_FILE1}.h5 grp_dsets grp_dsets -v -s /grp_dsets/simple -d /grp_dsets/simple_group)

  if (USE_FILTER_DEFLATE)
    # "Test copying & renaming group"
    ADD_H5_TEST (grp_rename 0 ${HDF_FILE1}.h5 -v -s grp_dsets -d grp_rename)
    # "Test copying 'full' group hierarchy into group in destination file"
    ADD_H5_TEST2 (grp_dsets_rename 0 ${HDF_FILE1}.h5 grp_dsets grp_rename -v -s grp_dsets -d /grp_rename/grp_dsets)
  else ()
    # "Test copying & renaming group"
    ADD_H5_TEST (grp_rename 2 ${HDF_FILE1}.h5 -v -s grp_dsets -d grp_rename)
    # "Test copying 'full' group hierarchy into group in destination file"
    ADD_H5_TEST2 (grp_dsets_rename 2 ${HDF_FILE1}.h5 grp_dsets grp_rename -v -s grp_dsets -d /grp_rename/grp_dsets)
  endif ()

  # "Test copying objects into group hier. that doesn't exist yet in destination file"
  ADD_H5_TEST (A_B1_simple 0 ${HDF_FILE1}.h5 -vp -s simple -d /A/B1/simple)
  ADD_H5_TEST (A_B2_simple2 0 ${HDF_FILE1}.h5 -vp -s simple -d /A/B2/simple2)
  ADD_H5_TEST (C_D_simple 0 ${HDF_FILE1}.h5 -vp -s /grp_dsets/simple -d /C/D/simple)
  if (USE_FILTER_DEFLATE)
    ADD_H5_TEST (E_F_grp_dsets 0 ${HDF_FILE1}.h5 -vp -s /grp_dsets -d /E/F/grp_dsets)
    ADD_H5_TEST (G_H_grp_nested 0 ${HDF_FILE1}.h5 -vp -s /grp_nested -d /G/H/grp_nested)
  else ()
    ADD_H5_TEST (E_F_grp_dsets 2 ${HDF_FILE1}.h5 -vp -s /grp_dsets -d /E/F/grp_dsets)
    ADD_H5_TEST (G_H_grp_nested 2 ${HDF_FILE1}.h5 -vp -s /grp_nested -d /G/H/grp_nested)
  endif ()

############# COPY REFERENCES ##############

  # "Test copying object and region references"
  ADD_H5_F_TEST (region_ref 2 ${HDF_FILE2}.h5 ref -v -s / -d /COPY)

############# COPY EXT LINKS ##############

  # "Test copying external link directly without -f ext"
  ADD_H5_TEST (ext_link 2 ${HDF_EXT_SRC_FILE}.h5 -v -s /group_ext/extlink_dset -d /copy1_dset)

  # "Test copying external link directly with -f ext"
  ADD_H5_F_TEST (ext_link_f 2 ${HDF_EXT_SRC_FILE}.h5 ext -v -s /group_ext/extlink_dset -d /copy2_dset)

  # "Test copying dangling external link (no obj) directly without -f ext"
  ADD_H5_TEST (ext_dangle_noobj 2 ${HDF_EXT_SRC_FILE}.h5 -v -s /group_ext/extlink_notyet1 -d /copy_dangle1_1)

  # "Test copying dangling external link (no obj) directly with -f ext"
  ADD_H5_F_TEST (ext_dangle_noobj_f 2 ${HDF_EXT_SRC_FILE}.h5 ext -v -s /group_ext/extlink_notyet1 -d /copy_dangle1_2)

  # "Test copying dangling external link (no file) directly without -f ext"
  ADD_H5_TEST (ext_dangle_nofile 2 ${HDF_EXT_SRC_FILE}.h5 -v -s /group_ext/extlink_notyet2 -d /copy_dangle2_1)

  # "Test copying dangling external link (no file) directly with -f ext"
  ADD_H5_F_TEST (ext_dangle_nofile_f 2 ${HDF_EXT_SRC_FILE}.h5 ext -v -s /group_ext/extlink_notyet2 -d /copy_dangle2_2)

  # "Test copying a group contains external links without -f ext"
  ADD_H5_TEST (ext_link_group 2 ${HDF_EXT_SRC_FILE}.h5 -v -s /group_ext -d /copy1_group)

  # "Test copying a group contains external links with -f ext"
  ADD_H5_F_TEST (ext_link_group_f 2 ${HDF_EXT_SRC_FILE}.h5 ext -v -s /group_ext -d /copy2_group)

############# Test misc. ##############

  #-----------------------------------------------------------------
  # "Test copying object into group which doesn't exist, without -p"
  #
  ADD_H5_CMP_TEST (h5copy_misc1 1 ${HDF_FILE1}.h5 -v -s /simple -d /g1/g2/simple)

  #-------------------------------------------
  # "Test copying objects to the same file "
  #
  # - dataset
  ADD_H5_TEST_SAME (samefile1 0 ${HDF_FILE1}.h5 /simple /simple -v -s /simple -d /simple_cp)
  # - group with some datasets
  if (USE_FILTER_DEFLATE)
    ADD_H5_TEST_SAME (samefile2 0 ${HDF_FILE1}.h5 /grp_dsets /grp_dsets -v -s /grp_dsets -d /grp_dsets_cp)
  else ()
    ADD_H5_TEST_SAME (samefile2 2 ${HDF_FILE1}.h5 /grp_dsets /grp_dsets -v -s /grp_dsets -d /grp_dsets_cp)
  endif ()

##############################################################################
###    P L U G I N  T E S T S
##############################################################################
if (BUILD_SHARED_LIBS)
  ADD_H5_UD_TEST (h5copy_plugin_test 0 tudfilter.h5 -s /dynlibud -d /dynlibud tudfilter2.h5 )
  ADD_H5_UD_ERR_TEST (h5copy_plugin_fail 2 tudfilter.h5 -s /dynlibud -d /dynlibud tudfilter2.h5)
endif ()
