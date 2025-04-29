#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the LICENSE file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#

##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################
  # System-independent path separator
  if (WIN32)
    set (CMAKE_SEP "\;")
  else ()
    set (CMAKE_SEP ":")
  endif ()

  # --------------------------------------------------------------------
  # Copy all the HDF5 files from the source directory into the test directory
  # --------------------------------------------------------------------
  set (LIST_HDF5_TEST_FILES
      h5copy_extlinks_src.h5
      h5copy_extlinks_trg.h5
      h5copy_ref.h5
      h5copytst.h5
      tudfilter.h5
      tudfilter2.h5
  )

  set (LIST_OTHER_TEST_FILES
      h5copy_misc1.out
      tudfilter.h5.txt
      tudfilter.h5_ERR.txt
      h5copy_plugin_fail_ERR.out.h5.txt
      h5copy_plugin_test.out.h5.txt
      h5copy_help1.ddl
      h5copy_help2.ddl
  )

  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")
  foreach (external_vol_tgt ${HDF5_EXTERNAL_VOL_TARGETS})
    file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles/${external_vol_tgt}")
  endforeach()

  # Setup testfiles
  foreach (listfiles ${LIST_HDF5_TEST_FILES})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/${listfiles}" "${PROJECT_BINARY_DIR}/testfiles/${listfiles}" "h5copy_files")
  endforeach ()

  foreach (listothers ${LIST_OTHER_TEST_FILES})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/expected/${listothers}" "${PROJECT_BINARY_DIR}/testfiles/${listothers}" "h5copy_files")
  endforeach ()

  # Setup testfiles for any external VOL connectors
  foreach (external_vol_tgt ${HDF5_EXTERNAL_VOL_TARGETS})
    set (vol_env "")
    set (vol_plugin_paths "${CMAKE_BINARY_DIR}/${HDF5_INSTALL_BIN_DIR}")

    get_target_property (ext_vol_name "${external_vol_tgt}" HDF5_VOL_NAME)

    # If this VOL has an info string, separate it from the VOL name before calling repack
    string(FIND ${ext_vol_name} " " idx)
    if (idx GREATER -1)
      math(EXPR next "${idx} + 1")
      string(SUBSTRING ${ext_vol_name} ${next} -1 ext_vol_info )
      string(SUBSTRING ${ext_vol_name} 0 ${idx} ext_vol_name)
    else()
      set(ext_vol_info "NONE")
    endif()

    foreach (listfiles ${LIST_HDF5_TEST_FILES})
      HDFTEST_REPACK_FILE("${PROJECT_SOURCE_DIR}/testfiles/${listfiles}"
        "${PROJECT_BINARY_DIR}/testfiles/${external_vol_tgt}/${listfiles}"
        "h5copy_files"
        ${ext_vol_name}
        ${ext_vol_info}
      )
    endforeach ()

    foreach (listothers ${LIST_OTHER_TEST_FILES})
      HDFTEST_REPACK_FILE("${PROJECT_SOURCE_DIR}/expected/${listothers}"
      "${PROJECT_BINARY_DIR}/testfiles/${external_vol_tgt}/${listothers}"
      "h5copy_files"
      ${ext_vol_name}
      ${ext_vol_info}
      )
    endforeach ()
  endforeach()

  add_custom_target(h5copy_files ALL COMMENT "Copying files needed by h5copy tests" DEPENDS ${h5copy_files_list})

##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  #
  # Perform h5copy according to passing parameters
  #
  macro (ADD_H5_F_TEST testname vol env resultcode infile fparam vparam sparam srcname dparam dstname)
    # Remove any output file left over from previous test run
    add_test (
        NAME HDF5_VOL_${vol}-H5COPY_F-${testname}-clear-objects
        COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5delete> ./testfiles/${testname}.out.h5
    )

    set_tests_properties (HDF5_VOL_${vol}-H5COPY_F-${testname}-clear-objects PROPERTIES
      ENVIRONMENT "${env}"
      # h5delete will return an error code if targeted file does not exist - accept any result
      PASS_REGULAR_EXPRESSION "^$|"
    )
    add_test (
        NAME HDF5_VOL_${vol}-H5COPY_F-${testname}
        COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5copy> -f ${fparam} -i ./testfiles/${infile} -o ./testfiles/${testname}.out.h5 ${vparam} ${sparam} ${srcname} ${dparam} ${dstname} ${ARGN}
    )
    set_tests_properties (HDF5_VOL_${vol}-H5COPY_F-${testname} PROPERTIES
      DEPENDS HDF5_VOL_${vol}-H5COPY_F-${testname}-clear-objects
      ENVIRONMENT "${env}"
    )
    if ("HDF5_VOL_${vol}-H5COPY_F-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (HDF5_VOL_${vol}-H5COPY_F-${testname} PROPERTIES DISABLED true)
    endif ()

    # resultcode=2 will cause the test to skip the diff test
    if (NOT "${resultcode}" STREQUAL "2")
      add_test (
          NAME HDF5_VOL_${vol}-H5COPY_F-${testname}-DIFF
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5diff> -v ./testfiles/${infile} ./testfiles/${testname}.out.h5 ${srcname} ${dstname}
      )
      set_tests_properties (HDF5_VOL_${vol}-H5COPY_F-${testname}-DIFF PROPERTIES
        DEPENDS HDF5_VOL_${vol}-H5COPY_F-${testname}
        ENVIRONMENT "${env}"
      )
      if ("${resultcode}" STREQUAL "1")
        set_tests_properties (HDF5_VOL_${vol}-H5COPY_F-${testname}-DIFF PROPERTIES WILL_FAIL "true")
      endif ()
      if ("HDF5_VOL_${vol}-H5COPY_F-${testname}-DIFF" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (HDF5_VOL_${vol}-H5COPY_F-${testname}-DIFF PROPERTIES DISABLED true)
      endif ()
    endif ()
    add_test (
        NAME HDF5_VOL_${vol}-H5COPY_F-${testname}-clean-objects
        COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5delete> ./testfiles/${testname}.out.h5
    )
    set_tests_properties (HDF5_VOL_${vol}-H5COPY_F-${testname}-clean-objects PROPERTIES
      ENVIRONMENT "${env}"
      # h5delete will return an error code if targeted file does not exist - accept any result
      PASS_REGULAR_EXPRESSION "^$|"
    )
    if (NOT "${resultcode}" STREQUAL "2")
      set_tests_properties (HDF5_VOL_${vol}-H5COPY_F-${testname}-clean-objects PROPERTIES DEPENDS HDF5_VOL_${vol}-H5COPY_F-${testname}-DIFF)
    else ()
      set_tests_properties (HDF5_VOL_${vol}-H5COPY_F-${testname}-clean-objects PROPERTIES DEPENDS HDF5_VOL_${vol}-H5COPY_F-${testname})
    endif ()
  endmacro ()

  macro (ADD_H5_TEST testname vol env resultcode infile vparam sparam srcname dparam dstname)
    # Remove any output file left over from previous test run
    add_test (
        NAME HDF5_VOL_${vol}-H5COPY-${testname}-clear-objects
        COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5delete> ./testfiles/${testname}.out.h5
    )

    set_tests_properties(HDF5_VOL_${vol}-H5COPY-${testname}-clear-objects PROPERTIES
      ENVIRONMENT "${env}"
      # h5delete will return an error code if targeted file does not exist - accept any result
      PASS_REGULAR_EXPRESSION "^$|"
    )
    add_test (
        NAME HDF5_VOL_${vol}-H5COPY-${testname}
        COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5copy> -i ./testfiles/${infile} -o ./testfiles/${testname}.out.h5 ${vparam} ${sparam} ${srcname} ${dparam} ${dstname} ${ARGN}
    )
    set_tests_properties (HDF5_VOL_${vol}-H5COPY-${testname} PROPERTIES
      DEPENDS HDF5_VOL_${vol}-H5COPY-${testname}-clear-objects
      ENVIRONMENT "${env}"
    )
    if ("HDF5_VOL_${vol}-H5COPY-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (HDF5_VOL_${vol}-H5COPY-${testname} PROPERTIES DISABLED true)
    endif ()

    # resultcode=2 will cause the test to skip the diff test
    if (NOT "${resultcode}" STREQUAL "2")
      add_test (
          NAME HDF5_VOL_${vol}-H5COPY-${testname}-DIFF
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5diff> -v ./testfiles/${infile} ./testfiles/${testname}.out.h5 ${srcname} ${dstname}
      )
      set_tests_properties (HDF5_VOL_${vol}-H5COPY-${testname}-DIFF PROPERTIES
        DEPENDS HDF5_VOL_${vol}-H5COPY-${testname}
        ENVIRONMENT "${env}"
      )
      if ("${resultcode}" STREQUAL "1")
        set_tests_properties (HDF5_VOL_${vol}-H5COPY-${testname}-DIFF PROPERTIES WILL_FAIL "true")
      endif ()
      if ("HDF5_VOL_${vol}-H5COPY-${testname}-DIFF" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (HDF5_VOL_${vol}-H5COPY-${testname}-DIFF PROPERTIES DISABLED true)
      endif ()
    endif ()
    add_test (
        NAME HDF5_VOL_${vol}-H5COPY-${testname}-clean-objects
        COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5delete> ./testfiles/${testname}.out.h5
    )
    set_tests_properties (HDF5_VOL_${vol}-H5COPY-${testname}-clean-objects PROPERTIES
      ENVIRONMENT "${env}"
      # h5delete will return an error code if targeted file does not exist - accept any result
      PASS_REGULAR_EXPRESSION "^$|"
    )

    if (NOT "${resultcode}" STREQUAL "2")
      set_tests_properties (HDF5_VOL_${vol}-H5COPY-${testname}-clean-objects PROPERTIES DEPENDS HDF5_VOL_${vol}-H5COPY-${testname}-DIFF)
    else ()
      set_tests_properties (HDF5_VOL_${vol}-H5COPY-${testname}-clean-objects PROPERTIES DEPENDS HDF5_VOL_${vol}-H5COPY-${testname})
    endif ()
  endmacro ()

  macro (ADD_SKIP_H5_TEST testname vol skipresultfile)
    if (NOT HDF5_USING_ANALYSIS_TOOL)
      add_test (
          NAME HDF5_VOL_${vol}-H5COPY-${testname}-${skipresultfile}
          COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${testname}-${skipresultfile} ${ARGN}"
      )
      set_property(TEST HDF5_VOL_${vol}-H5COPY-${testname}-${skipresultfile} PROPERTY DISABLED true)
    endif ()
  endmacro ()

  macro (ADD_H5_TEST2 testname vol env resultcode infile  psparam pdparam vparam sparam srcname dparam dstname)
    # Remove any output file left over from previous test run
    add_test (
        NAME HDF5_VOL_${vol}-H5COPY-${testname}-clear-objects
        COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5delete> ./testfiles/${testname}.out.h5
    )

    set_tests_properties(HDF5_VOL_${vol}-H5COPY-${testname}-clear-objects PROPERTIES
      ENVIRONMENT "${env}"
      # h5delete will return an error code if targeted file does not exist - accept any result
      PASS_REGULAR_EXPRESSION "^$|"
    )

    add_test (
        NAME HDF5_VOL_${vol}-H5COPY-${testname}-prefill
        COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5copy> -i ./testfiles/${infile} -o ./testfiles/${testname}.out.h5 -v -s ${psparam} -d ${pdparam}
    )
    set_tests_properties (HDF5_VOL_${vol}-H5COPY-${testname}-prefill PROPERTIES
      DEPENDS HDF5_VOL_${vol}-H5COPY-${testname}-clear-objects
      ENVIRONMENT "${ENV}"
    )
    if ("HDF5_VOL_${vol}-H5COPY-${testname}-prefill" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (HDF5_VOL_${vol}-H5COPY-${testname}-prefill PROPERTIES DISABLED true)
    endif ()

    add_test (
        NAME HDF5_VOL_${vol}-H5COPY-${testname}
        COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5copy> -i ./testfiles/${infile} -o ./testfiles/${testname}.out.h5 ${vparam} ${sparam} ${srcname} ${dparam} ${dstname} ${ARGN}
    )
    set_tests_properties (HDF5_VOL_${vol}-H5COPY-${testname} PROPERTIES
      DEPENDS HDF5_VOL_${vol}-H5COPY-${testname}-prefill
      ENVIRONMENT "${ENV}"
    )
    if ("HDF5_VOL_${vol}-H5COPY-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (HDF5_VOL_${vol}-H5COPY-${testname} PROPERTIES DISABLED true)
    endif ()
    # resultcode=2 will cause the test to skip the diff test
    if (NOT "${resultcode}" STREQUAL "2")
      add_test (
          NAME HDF5_VOL_${vol}-H5COPY-${testname}-DIFF
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5diff> -v ./testfiles/${infile} ./testfiles/${testname}.out.h5 ${srcname} ${dstname}
      )
      set_tests_properties (HDF5_VOL_${vol}-H5COPY-${testname}-DIFF PROPERTIES
        DEPENDS HDF5_VOL_${vol}-H5COPY-${testname}
        ENVIRONMENT "${ENV}"
      )
      if ("${resultcode}" STREQUAL "1")
        set_tests_properties (HDF5_VOL_${vol}-H5COPY-${testname}-DIFF PROPERTIES WILL_FAIL "true")
      endif ()
      if ("HDF5_VOL_${vol}-H5COPY-${testname}-DIFF" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (HDF5_VOL_${vol}-H5COPY-${testname}-DIFF PROPERTIES DISABLED true)
      endif ()
    endif ()
    add_test (
        NAME HDF5_VOL_${vol}-H5COPY-${testname}-clean-objects
        COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5delete> ./testfiles/${testname}.out.h5
    )
    set_tests_properties (HDF5_VOL_${vol}-H5COPY-${testname}-clean-objects PROPERTIES
      ENVIRONMENT "${ENV}"
      # h5delete will return an error code if targeted file does not exist - accept any result
      PASS_REGULAR_EXPRESSION "^$|"
    )
    if (NOT "${resultcode}" STREQUAL "2")
      set_tests_properties (HDF5_VOL_${vol}-H5COPY-${testname}-clean-objects PROPERTIES DEPENDS HDF5_VOL_${vol}-H5COPY-${testname}-DIFF)
    else ()
      set_tests_properties (HDF5_VOL_${vol}-H5COPY-${testname}-clean-objects PROPERTIES DEPENDS HDF5_VOL_${vol}-H5COPY-${testname})
    endif ()
  endmacro ()

  macro (ADD_H5_TEST_SAME testname vol env resultcode pfile psparam pdparam vparam sparam srcname dparam dstname)
    # Remove any output file left over from previous test run
    add_test (
        NAME HDF5_VOL_${vol}-H5COPY_SAME-${testname}-clear-objects
        COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5delete> ./testfiles/${testname}.out.h5
    )
    set_tests_properties (HDF5_VOL_${vol}-H5COPY_SAME-${testname}-clear-objects PROPERTIES
      ENVIRONMENT "${ENV}"
      # h5delete will return an error code if targeted file does not exist - accept any result
      PASS_REGULAR_EXPRESSION "^$|"
    )
    add_test (
        NAME HDF5_VOL_${vol}-H5COPY_SAME-${testname}-prefill
        COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5copy> -i ./testfiles/${pfile} -o ./testfiles/${testname}.out.h5 -v -s ${psparam} -d ${pdparam}
    )
    set_tests_properties (HDF5_VOL_${vol}-H5COPY_SAME-${testname}-prefill PROPERTIES
      DEPENDS HDF5_VOL_${vol}-H5COPY_SAME-${testname}-clear-objects
      ENVIRONMENT "${ENV}"
    )
    if ("HDF5_VOL_${vol}-H5COPY_SAME-${testname}-prefill" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (HDF5_VOL_${vol}-H5COPY_SAME-${testname}-prefill PROPERTIES DISABLED true)
    endif ()

    add_test (
        NAME HDF5_VOL_${vol}-H5COPY_SAME-${testname}
        COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5copy> -i ./testfiles/${testname}.out.h5 -o ./testfiles/${testname}.out.h5 ${vparam} ${sparam} ${srcname} ${dparam} ${dstname} ${ARGN}
    )
    set_tests_properties (HDF5_VOL_${vol}-H5COPY_SAME-${testname} PROPERTIES
      DEPENDS HDF5_VOL_${vol}-H5COPY_SAME-${testname}-prefill
      ENVIRONMENT "${ENV}"
    )
    if ("HDF5_VOL_${vol}-H5COPY_SAME-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (HDF5_VOL_${vol}-H5COPY_SAME-${testname} PROPERTIES DISABLED true)
    endif ()
    # resultcode=2 will cause the test to skip the diff test
    if (NOT "${resultcode}" STREQUAL "2")
      add_test (
          NAME HDF5_VOL_${vol}-H5COPY_SAME-${testname}-DIFF
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5diff> -v ./testfiles/${testname}.out.h5 ./testfiles/${testname}.out.h5 ${srcname} ${dstname}
      )
      set_tests_properties (HDF5_VOL_${vol}-H5COPY_SAME-${testname}-DIFF PROPERTIES
        DEPENDS HDF5_VOL_${vol}-H5COPY_SAME-${testname}
        ENVIRONMENT "${ENV}"
      )
      if ("${resultcode}" STREQUAL "1")
        set_tests_properties (HDF5_VOL_${vol}-H5COPY_SAME-${testname}-DIFF PROPERTIES WILL_FAIL "true")
      endif ()
      if ("HDF5_VOL_${vol}-H5COPY_SAME-${testname}-DIFF" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (HDF5_VOL_${vol}-H5COPY_SAME-${testname}-DIFF PROPERTIES DISABLED true)
      endif ()
    endif ()
    add_test (
        NAME HDF5_VOL_${vol}-H5COPY_SAME-${testname}-clean-objects
        COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5delete> ./testfiles/${testname}.out.h5
    )
    set_tests_properties (HDF5_VOL_${vol}-H5COPY_SAME-${testname}-clean-objects PROPERTIES
      ENVIRONMENT "${ENV}"
      # h5delete will return an error code if targeted file does not exist - accept any result
      PASS_REGULAR_EXPRESSION "^$|"
    )
    if (NOT "${resultcode}" STREQUAL "2")
      set_tests_properties (HDF5_VOL_${vol}-H5COPY_SAME-${testname}-clean-objects PROPERTIES DEPENDS HDF5_VOL_${vol}-H5COPY_SAME-${testname}-DIFF)
    else ()
      set_tests_properties (HDF5_VOL_${vol}-H5COPY_SAME-${testname}-clean-objects PROPERTIES DEPENDS HDF5_VOL_${vol}-H5COPY_SAME-${testname})
    endif ()
  endmacro ()

  #
  # Similar to ADD_H5_TEST macro. Compare to outputs from source & target
  # files instead of checking with h5ls.
  #
  macro (ADD_H5_CMP_TEST testname vol env resultcode result_errcheck infile vparam sparam srcname dparam dstname)
    # Remove any output file left over from previous test run
    add_test (
        NAME HDF5_VOL_${vol}-H5COPY-CMP-${testname}-clear-objects
        COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5delete> ./testfiles/${testname}.out.h5
    )
    set_tests_properties (HDF5_VOL_${vol}-H5COPY-CMP-${testname}-clear-objects PROPERTIES
      ENVIRONMENT "${env}"
      # h5delete will return an error code if targeted file does not exist - accept any result
      PASS_REGULAR_EXPRESSION "^$|"
    )
    # If using memchecker add tests without using scripts
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME HDF5_VOL_${vol}-H5COPY-CMP-${testname} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5copy> -i ./testfiles/${infile} -o ./testfiles/${testname}.out.h5 ${vparam} ${sparam} ${srcname} ${dparam} ${dstname} ${ARGN})
      if ("${resultcode}" STREQUAL "1")
        set_tests_properties (HDF5_VOL_${vol}-H5COPY-CMP-${testname} PROPERTIES WILL_FAIL "true")
      endif ()
    else ()
      add_test (
          NAME HDF5_VOL_${vol}-H5COPY-CMP-${testname}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5copy>"
              -D "TEST_ARGS=-i;./testfiles/${infile};-o;./testfiles/${testname}.out.h5;${vparam};${sparam};${srcname};${dparam};${dstname}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=./testfiles/${testname}.out.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=./testfiles/${testname}.out"
              -D "TEST_ERRREF=${result_errcheck}"
              -D "TEST_MASK_STORE=true"
              -P "${HDF_RESOURCES_DIR}/grepTest.cmake"
      )
    endif ()
    set_tests_properties (HDF5_VOL_${vol}-H5COPY-CMP-${testname} PROPERTIES
      DEPENDS HDF5_VOL_${vol}-H5COPY-CMP-${testname}-clear-objects
      ENVIRONMENT "${env}"
    )
    if ("HDF5_VOL_${vol}-H5COPY-CMP-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (HDF5_VOL_${vol}-H5COPY-CMP-${testname} PROPERTIES DISABLED true)
    endif ()
    add_test (
        NAME HDF5_VOL_${vol}-H5COPY-CMP-${testname}-clean-objects
        COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5delete> ./testfiles/${testname}.out.h5
    )
    set_tests_properties (HDF5_VOL_${vol}-H5COPY-CMP-${testname}-clean-objects PROPERTIES
      DEPENDS HDF5_VOL_${vol}-H5COPY-CMP-${testname}
      ENVIRONMENT "${env}"
      # h5delete will return an error code if targeted file does not exist - accept any result
      PASS_REGULAR_EXPRESSION "^$|"
    )
  endmacro ()

  macro (ADD_H5_UD_TEST testname vol env resultcode infile sparam srcname dparam dstname cmpfile)
    string(FIND "${env}" "HDF5_PLUGIN_PATH=" vol_plugin_path_posn)
    if (vol_plugin_path_posn GREATER -1)
      # Grab path string after HDF5_PLUGIN_PATH=
      math(EXPR vol_plugin_path_posn "${vol_plugin_path_posn} + 17")
      string(SUBSTRING "${env}" ${vol_plugin_path_posn} -1 vol_plugin_path)
    else()
      set(vol_plugin_path "")
    endif()

    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      # Remove any output file left over from previous test run
      add_test (
          NAME HDF5_VOL_${vol}-H5COPY_UD-${testname}-clear-objects
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5delete> testfiles/${testname}.out.h5
      )
      set_tests_properties(HDF5_VOL_${vol}-H5COPY_UD-${testname}-clear-objects PROPERTIES
        ENVIRONMENT "${env}"
        # h5delete will return an error code if targeted file does not exist - accept any result
        PASS_REGULAR_EXPRESSION "^$|"
      )
      if ("${resultcode}" STREQUAL "2")
        add_test (
            NAME HDF5_VOL_${vol}-H5COPY_UD-${testname}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                -D "TEST_PROGRAM=$<TARGET_FILE:h5copy>"
                -D "TEST_ARGS:STRING=-v;-i;./testfiles/${infile};-o;./testfiles/${testname}.out.h5;${sparam};${srcname};${dparam};${dstname}"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                -D "TEST_OUTPUT=./testfiles/${infile}.out"
                -D "TEST_EXPECT=${resultcode}"
                -D "TEST_REFERENCE=./testfiles/${infile}.txt"
                -D "TEST_APPEND=EXIT CODE:"
                -D "TEST_ENV_VAR=HDF5_PLUGIN_PATH"
                -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}${CMAKE_SEP}${vol_plugin_path}"
                -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
                -P "${HDF_RESOURCES_DIR}/runTest.cmake"
        )
      else ()
        add_test (
            NAME HDF5_VOL_${vol}-H5COPY_UD-${testname}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                -D "TEST_PROGRAM=$<TARGET_FILE:h5copy>"
                -D "TEST_ARGS:STRING=-v;-i;./testfiles/${infile};-o;./testfiles/${testname}.out.h5;${sparam};${srcname};${dparam};${dstname}"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                -D "TEST_OUTPUT=./testfiles/${infile}.out"
                -D "TEST_EXPECT=${resultcode}"
                -D "TEST_REFERENCE=./testfiles/${infile}.txt"
                -D "TEST_APPEND=EXIT CODE:"
                -D "TEST_ENV_VAR=HDF5_PLUGIN_PATH"
                -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}/plugins${CMAKE_SEP}${vol_plugin_path}"
                -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
                -P "${HDF_RESOURCES_DIR}/runTest.cmake"
        )
      endif ()
      set_tests_properties (HDF5_VOL_${vol}-H5COPY_UD-${testname} PROPERTIES DEPENDS
        HDF5_VOL_${vol}-H5COPY_UD-${testname}-clear-objects
        ENVIRONMENT "${env}"
      )
      if ("HDF5_VOL_${vol}-H5COPY_UD-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (HDF5_VOL_${vol}-H5COPY_UD-${testname} PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME HDF5_VOL_${vol}-H5COPY_UD-${testname}-DIFF
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5diff>"
              -D "TEST_ARGS:STRING=-v;./testfiles/${cmpfile};./testfiles/${testname}.out.h5;${srcname};${dstname}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=./testfiles/${testname}.out.h5.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=./testfiles/${testname}.out.h5.txt"
              -D "TEST_APPEND=EXIT CODE:"
              -D "TEST_ENV_VAR=HDF5_PLUGIN_PATH"
              -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}/plugins${CMAKE_SEP}${vol_plugin_path}"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
              -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (HDF5_VOL_${vol}-H5COPY_UD-${testname}-DIFF PROPERTIES
        DEPENDS HDF5_VOL_${vol}-H5COPY_UD-${testname}
        ENVIRONMENT "${env}"
      )
      if ("HDF5_VOL_${vol}-H5COPY_UD-${testname}-DIFF" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (HDF5_VOL_${vol}-H5COPY_UD-${testname}-DIFF PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME HDF5_VOL_${vol}-H5COPY_UD-${testname}-clean-objects
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5delete> testfiles/${testname}.out.h5
      )
      set_tests_properties (HDF5_VOL_${vol}-H5COPY_UD-${testname}-clean-objects PROPERTIES
        DEPENDS HDF5_VOL_${vol}-H5COPY_UD-${testname}-DIFF
        ENVIRONMENT "${env}"
        # h5delete will return an error code if targeted file does not exist - accept any result
        PASS_REGULAR_EXPRESSION "^$|"
      )
    endif ()
  endmacro ()

  macro (ADD_H5_UD_ERR_TEST testname vol env resultcode infile sparam srcname dparam dstname cmpfile)
    string(FIND "${env}" "HDF5_PLUGIN_PATH=" vol_plugin_path_posn)
    if (vol_plugin_path_posn GREATER -1)
      # Grab path string after HDF5_PLUGIN_PATH=
      math(EXPR vol_plugin_path_posn "${vol_plugin_path_posn} + 17")
      string(SUBSTRING "${env}" ${vol_plugin_path_posn} -1 vol_plugin_path)
    else()
      set(vol_plugin_path "")
    endif()

    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      # Remove any output file left over from previous test run
      add_test (
          NAME HDF5_VOL_${vol}-H5COPY_UD_ERR-${testname}-clear-objects
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5delete> testfiles/${testname}_ERR.out.h5
      )
      set_tests_properties (HDF5_VOL_${vol}-H5COPY_UD_ERR-${testname}-clear-objects PROPERTIES
        ENVIRONMENT "${env}"
        # h5delete will return an error code if targeted file does not exist - accept any result
        PASS_REGULAR_EXPRESSION "^$|"
      )
      if ("${resultcode}" STREQUAL "2")
        add_test (
            NAME HDF5_VOL_${vol}-H5COPY_UD_ERR-${testname}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                -D "TEST_PROGRAM=$<TARGET_FILE:h5copy>"
                -D "TEST_ARGS:STRING=-v;--enable-error-stack;-i;./testfiles/${infile};-o;./testfiles/${testname}_ERR.out.h5;${sparam};${srcname};${dparam};${dstname}"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                -D "TEST_OUTPUT=./testfiles/${infile}_ERR.out"
                -D "TEST_EXPECT=0"
                -D "TEST_REFERENCE=./testfiles/${infile}_ERR.txt"
                -D "TEST_MASK_ERROR=true"
                -D "TEST_APPEND=EXIT CODE:"
                -D "TEST_ENV_VAR=HDF5_PLUGIN_PATH"
                -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}${CMAKE_SEP}${vol_plugin_path}"
                -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
                -P "${HDF_RESOURCES_DIR}/runTest.cmake"
        )
      else ()
        add_test (
            NAME HDF5_VOL_${vol}-H5COPY_UD_ERR-${testname}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                -D "TEST_PROGRAM=$<TARGET_FILE:h5copy>"
                -D "TEST_ARGS:STRING=-v;--enable-error-stack;-i;./testfiles/${infile};-o;./testfiles/${testname}_ERR.out.h5;${sparam};${srcname};${dparam};${dstname}"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
                -D "TEST_OUTPUT=./testfiles/${infile}_ERR.out"
                -D "TEST_EXPECT=${resultcode}"
                -D "TEST_REFERENCE=./testfiles/${infile}_ERR.txt"
                -D "TEST_MASK_ERROR=true"
                -D "TEST_APPEND=EXIT CODE:"
                -D "TEST_ENV_VAR=HDF5_PLUGIN_PATH"
                -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}/plugins${CMAKE_SEP}${vol_plugin_path}"
                -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
                -P "${HDF_RESOURCES_DIR}/runTest.cmake"
        )
      endif ()
      set_tests_properties (HDF5_VOL_${vol}-H5COPY_UD_ERR-${testname} PROPERTIES
        DEPENDS HDF5_VOL_${vol}-H5COPY_UD_ERR-${testname}-clear-objects
        ENVIRONMENT "${env}"
      )
      if ("HDF5_VOL_${vol}-H5COPY_UD_ERR-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (HDF5_VOL_${vol}-H5COPY_UD_ERR-${testname} PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME HDF5_VOL_${vol}-H5COPY_UD_ERR-${testname}-DIFF
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5diff>"
              -D "TEST_ARGS:STRING=-v;./testfiles/${cmpfile};./testfiles/${testname}_ERR.out.h5;${srcname};${dstname}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=./testfiles/${testname}_ERR.out.h5.out"
              -D "TEST_EXPECT=0"
              -D "TEST_REFERENCE=./testfiles/${testname}_ERR.out.h5.txt"
              -D "TEST_APPEND=EXIT CODE:"
              -D "TEST_ENV_VAR=HDF5_PLUGIN_PATH"
              -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}/plugins${CMAKE_SEP}${vol_plugin_path}"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
              -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (HDF5_VOL_${vol}-H5COPY_UD_ERR-${testname}-DIFF PROPERTIES
        DEPENDS HDF5_VOL_${vol}-H5COPY_UD_ERR-${testname}
        ENVIRONMENT "${env}"
      )
      if ("HDF5_VOL_${vol}-H5COPY_UD_ERR-${testname}-DIFF" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (HDF5_VOL_${vol}-H5COPY_UD_ERR-${testname}-DIFF PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME HDF5_VOL_${vol}-H5COPY_UD_ERR-${testname}-clean-objects
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5delete> testfiles/${testname}_ERR.out.h5
      )
      set_tests_properties (HDF5_VOL_${vol}-H5COPY_UD_ERR-${testname}-clean-objects PROPERTIES
        DEPENDS HDF5_VOL_${vol}-H5COPY_UD_ERR-${testname}-DIFF
        ENVIRONMENT "${env}"
        # h5delete will return an error code if targeted file does not exist - accept any result
        PASS_REGULAR_EXPRESSION "^$|"
      )
    endif ()
  endmacro ()

  macro (ADD_SIMPLE_TEST resultfile vol env resultcode)
    # If using memchecker add tests without using scripts
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME HDF5_VOL_${vol}-H5COPY-${resultfile} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5copy> ${ARGN})
      if (${resultcode})
        set_tests_properties (HDF5_VOL_${vol}-H5COPY-${resultfile} PROPERTIES WILL_FAIL "true")
      endif ()
    else ()
      add_test (
          NAME HDF5_VOL_${vol}-H5COPY-${resultfile}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:h5copy>"
              -D "TEST_ARGS=${ARGN}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -D "TEST_OUTPUT=./testfiles/${resultfile}.out"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_REFERENCE=./testfiles/${resultfile}.ddl"
              -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
    endif ()
    set_tests_properties (HDF5_VOL_${vol}-H5COPY-${resultfile} PROPERTIES
        WORKING_DIRECTORY "${PROJECT_BINARY_DIR}"
        ENVIRONMENT "${env}"
    )
    if ("HDF5_VOL_${vol}-H5COPY-${resultfile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (HDF5_VOL_${vol}-H5COPY-${resultfile} PROPERTIES DISABLED true)
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

# Add a test for the native connector and each external VOL connector
# TBD: Will need to be modified to support using fetchcontent for multiple connectors in one build
  if (DEFINED HDF5_EXTERNAL_VOL_TARGETS)
    set(max_idx 1)
  else()
    set(max_idx 0)
  endif()

  foreach(vol_idx RANGE 0 ${max_idx})
    if (${vol_idx} EQUAL 0)
      set(vol_name "native")
      set(vol_env "")
    else ()
      # An external VOL connector
      set(vol_env "")

      # Retrieve VOL connector name/info
      math(EXPR ext_idx "${vol_idx} - 1")
      list(GET HDF5_EXTERNAL_VOL_TARGETS ${ext_idx} ext_vol_tgt)
      get_target_property(vol_conn_string ${ext_vol_tgt} HDF5_VOL_NAME)
      list(APPEND vol_env "HDF5_VOL_CONNECTOR=${vol_conn_string}")

      # The 'name' of the connector used to set up the test names is drawn from the path to its source

      # Set up number string to append to var name
      set (vol_idx_num_digits 2) # Based on HDF5_MAX_EXTERNAL_VOLS
      set (vol_idx_fixed "${vol_idx}")
      string (LENGTH "${vol_idx_fixed}" vol_idx_len)
      while (vol_idx_len LESS vol_idx_num_digits)
        string (PREPEND vol_idx_fixed "0")
        math (EXPR vol_idx_len "${vol_idx_len}+1")
      endwhile ()

      if (HDF5_VOL_ALLOW_EXTERNAL MATCHES "GIT")
        set (HDF5_VOL_URL${vol_idx_fixed} "" CACHE STRING "Git repository URL of an external HDF5 VOL connector to build")
        mark_as_advanced (HDF5_VOL_URL${vol_idx_fixed})
        set (HDF5_VOL_SOURCE "${HDF5_VOL_URL${vol_idx_fixed}}")
      elseif(HDF5_VOL_ALLOW_EXTERNAL MATCHES "LOCAL_DIR")
        set (HDF5_VOL_PATH${vol_idx_fixed} "" CACHE STRING "Path to the source directory of an external HDF5 VOL connector to build")
        mark_as_advanced (HDF5_VOL_PATH${vol_idx_fixed})
        set (HDF5_VOL_SOURCE "${HDF5_VOL_PATH${vol_idx_fixed}}")
      endif()

      if ("${HDF5_VOL_SOURCE}" STREQUAL "")
        message(FATALE_ERROR "${HDF5_VOL_SOURCE} was unexpectedly empty")
      endif()

      # Deal with trailing slash in path for LOCAL_DIR case
      if (HDF5_VOL_ALLOW_EXTERNAL MATCHES "LOCAL_DIR")
        # Erase trailing slash
        string (REGEX REPLACE "/$" "" HDF5_VOL_SOURCE ${HDF5_VOL_SOURCE})
      endif()

      # Extract the name of the VOL connector
      string (FIND "${HDF5_VOL_SOURCE}" "/" hdf5_vol_name_pos REVERSE)
      if (hdf5_vol_name_pos EQUAL -1)
        if (HDF5_VOL_ALLOW_EXTERNAL MATCHES "GIT")
          message (SEND_ERROR "Invalid URL '${HDF5_VOL_SOURCE}' specified for HDF5_VOL_URL${vol_idx_fixed}")
        elseif (HDF5_VOL_ALLOW_EXTERNAL MATCHES "LOCAL_DIR")
          message (SEND_ERROR "Invalid source path '${HDF5_VOL_SOURCE}' specified for HDF5_VOL_PATH${vol_idx_fixed}")
        endif()
      endif ()

      math (EXPR hdf5_vol_name_pos "${hdf5_vol_name_pos}+1")

      string (SUBSTRING "${HDF5_VOL_SOURCE}" ${hdf5_vol_name_pos} -1 hdf5_vol_name)
      string (REPLACE ".git" "" hdf5_vol_name "${hdf5_vol_name}")
      string (STRIP "${hdf5_vol_name}" hdf5_vol_name)
      string (TOUPPER "${hdf5_vol_name}" hdf5_vol_name_upper)
      string (TOLOWER "${hdf5_vol_name}" hdf5_vol_name_lower)
      set (vol_name "${hdf5_vol_name_lower}")
      # string(FIND ${vol_conn_string} " " space_pos)
      # string(SUBSTRING ${vol_conn_string} 0 ${space_pos} vol_name)

      # Set up HDF5_PLUGIN_PATH
      set (vol_plugin_paths "${CMAKE_BINARY_DIR}/${HDF5_INSTALL_BIN_DIR}")
      get_target_property(vol_lib_targets "${ext_vol_tgt}" HDF5_VOL_TARGETS)

      # Retrieve plugin path for connector if not default
      foreach (lib_target ${vol_lib_targets})
        get_target_property (lib_target_output_dir "${lib_target}" LIBRARY_OUTPUT_DIRECTORY)
        if (NOT "${lib_target_output_dir}" STREQUAL "lib_target_output_dir-NOTFOUND"
            AND NOT "${lib_target_output_dir}" STREQUAL ""
            AND NOT "${lib_target_output_dir}" STREQUAL "${CMAKE_BINARY_DIR}/${HDF5_INSTALL_BIN_DIR}")
          set (vol_plugin_paths "${vol_plugin_paths}${CMAKE_SEP}${lib_target_output_dir}")
        endif ()
      endforeach ()

      list(APPEND vol_env "HDF5_PLUGIN_PATH=${vol_plugin_paths}")

      list(GET vol_env 1 vol_plugin_path)
    endif ()

    # Test for help flag
    ADD_SIMPLE_TEST (h5copy_help1 "${vol_name}" "${vol_env}" 0 -h)
    ADD_SIMPLE_TEST (h5copy_help2 "${vol_name}" "${vol_env}" 0 --help)

    # "Test copying various forms of datasets"
    ADD_H5_TEST (simple "${vol_name}" "${vol_env}" 0 ${HDF_FILE1}.h5 -v -s simple -d simple)
    ADD_H5_TEST (chunk "${vol_name}" "${vol_env}" 0 ${HDF_FILE1}.h5 -v -s chunk -d chunk)
    ADD_H5_TEST (compact "${vol_name}" "${vol_env}" 0 ${HDF_FILE1}.h5 -v -s compact -d compact)
    ADD_H5_TEST (compound "${vol_name}" "${vol_env}" 0 ${HDF_FILE1}.h5 -v -s compound -d compound)

    if (USE_FILTER_DEFLATE)
      ADD_H5_TEST (compressed "${vol_name}" "${vol_env}" 0 ${HDF_FILE1}.h5 -v -s compressed -d compressed)
    else ()
      ADD_H5_TEST (compressed "${vol_name}" "${vol_env}" 2 ${HDF_FILE1}.h5 -v -s compressed -d compressed)
    endif ()

    ADD_H5_TEST (named_vl "${vol_name}" "${vol_env}" 0 ${HDF_FILE1}.h5 -v -s named_vl -d named_vl)
    ADD_H5_TEST (nested_vl "${vol_name}" "${vol_env}" 0 ${HDF_FILE1}.h5 -v -s nested_vl -d nested_vl)
    ADD_H5_TEST (dset_attr "${vol_name}" "${vol_env}" 0 ${HDF_FILE1}.h5 -v -s dset_attr -d dset_attr)

    # "Test copying dataset within group in source file to root of destination"
    ADD_H5_TEST (simple_top "${vol_name}" "${vol_env}" 0 ${HDF_FILE1}.h5 -v -s grp_dsets/simple -d simple_top)

    # "Test copying & renaming dataset"
    ADD_H5_TEST (dsrename "${vol_name}" "${vol_env}" 0 ${HDF_FILE1}.h5 -v -s compound -d rename)

    # "Test copying empty, 'full' & 'nested' groups"
    ADD_H5_TEST (grp_empty "${vol_name}" "${vol_env}" 0 ${HDF_FILE1}.h5 -v -s grp_empty -d grp_empty)
    if (USE_FILTER_DEFLATE)
      ADD_H5_TEST (grp_dsets "${vol_name}" "${vol_env}" 0 ${HDF_FILE1}.h5 -v -s grp_dsets -d grp_dsets)
      ADD_H5_TEST (grp_nested "${vol_name}" "${vol_env}" 0 ${HDF_FILE1}.h5 -v -s grp_nested -d grp_nested)
    else ()
      ADD_H5_TEST (grp_dsets "${vol_name}" "${vol_env}" 2 ${HDF_FILE1}.h5 -v -s grp_dsets -d grp_dsets)
      ADD_H5_TEST (grp_nested "${vol_name}" "${vol_env}" 2 ${HDF_FILE1}.h5 -v -s grp_nested -d grp_nested)
    endif ()
    ADD_H5_TEST (grp_attr "${vol_name}" "${vol_env}" 0 ${HDF_FILE1}.h5 -v -s grp_attr -d grp_attr)

    # "Test copying dataset within group in source file to group in destination"
    ADD_H5_TEST2 (simple_group "${vol_name}" "${vol_env}" 0 ${HDF_FILE1}.h5 grp_dsets grp_dsets -v -s /grp_dsets/simple -d /grp_dsets/simple_group)

    if (USE_FILTER_DEFLATE)
      # "Test copying & renaming group"
      ADD_H5_TEST (grp_rename "${vol_name}" "${vol_env}" 0 ${HDF_FILE1}.h5 -v -s grp_dsets -d grp_rename)
      # "Test copying 'full' group hierarchy into group in destination file"
      ADD_H5_TEST2 (grp_dsets_rename "${vol_name}" "${vol_env}" 0 ${HDF_FILE1}.h5 grp_dsets grp_rename -v -s grp_dsets -d /grp_rename/grp_dsets)
    else ()
      # "Test copying & renaming group"
      ADD_H5_TEST (grp_rename "${vol_name}" "${vol_env}" 2 ${HDF_FILE1}.h5 -v -s grp_dsets -d grp_rename)
      # "Test copying 'full' group hierarchy into group in destination file"
      ADD_H5_TEST2 (grp_dsets_rename "${vol_name}" "${vol_env}" 2 ${HDF_FILE1}.h5 grp_dsets grp_rename -v -s grp_dsets -d /grp_rename/grp_dsets)
    endif ()

    # "Test copying objects into group that doesn't exist yet in destination file"
    ADD_H5_TEST (A_B1_simple "${vol_name}" "${vol_env}" 0 ${HDF_FILE1}.h5 -vp -s simple -d /A/B1/simple)
    ADD_H5_TEST (A_B2_simple2 "${vol_name}" "${vol_env}" 0 ${HDF_FILE1}.h5 -vp -s simple -d /A/B2/simple2)
    ADD_H5_TEST (C_D_simple "${vol_name}" "${vol_env}" 0 ${HDF_FILE1}.h5 -vp -s /grp_dsets/simple -d /C/D/simple)
    if (USE_FILTER_DEFLATE)
      ADD_H5_TEST (E_F_grp_dsets "${vol_name}" "${vol_env}" 0 ${HDF_FILE1}.h5 -vp -s /grp_dsets -d /E/F/grp_dsets)
      ADD_H5_TEST (G_H_grp_nested "${vol_name}" "${vol_env}" 0 ${HDF_FILE1}.h5 -vp -s /grp_nested -d /G/H/grp_nested)
    else ()
      ADD_H5_TEST (E_F_grp_dsets "${vol_name}" "${vol_env}" 2 ${HDF_FILE1}.h5 -vp -s /grp_dsets -d /E/F/grp_dsets)
      ADD_H5_TEST (G_H_grp_nested "${vol_name}" "${vol_env}" 2 ${HDF_FILE1}.h5 -vp -s /grp_nested -d /G/H/grp_nested)
    endif ()

  ############# COPY REFERENCES ##############

    # "Test copying object and region references"
    ADD_H5_F_TEST (region_ref "${vol_name}" "${vol_env}" 2 ${HDF_FILE2}.h5 ref -v -s / -d /COPY)

  ############# COPY EXT LINKS ##############

    # "Test copying external link directly without -f ext"
    ADD_H5_TEST (ext_link "${vol_name}" "${vol_env}" 2 ${HDF_EXT_SRC_FILE}.h5 -v -s /group_ext/extlink_dset -d /copy1_dset)

    # "Test copying external link directly with -f ext"
    ADD_H5_F_TEST (ext_link_f "${vol_name}" "${vol_env}" 2 ${HDF_EXT_SRC_FILE}.h5 ext -v -s /group_ext/extlink_dset -d /copy2_dset)

    # "Test copying dangling external link (no obj) directly without -f ext"
    ADD_H5_TEST (ext_dangle_noobj "${vol_name}" "${vol_env}" 2 ${HDF_EXT_SRC_FILE}.h5 -v -s /group_ext/extlink_notyet1 -d /copy_dangle1_1)

    # "Test copying dangling external link (no obj) directly with -f ext"
    ADD_H5_F_TEST (ext_dangle_noobj_f "${vol_name}" "${vol_env}" 2 ${HDF_EXT_SRC_FILE}.h5 ext -v -s /group_ext/extlink_notyet1 -d /copy_dangle1_2)

    # "Test copying dangling external link (no file) directly without -f ext"
    ADD_H5_TEST (ext_dangle_nofile "${vol_name}" "${vol_env}" 2 ${HDF_EXT_SRC_FILE}.h5 -v -s /group_ext/extlink_notyet2 -d /copy_dangle2_1)

    # "Test copying dangling external link (no file) directly with -f ext"
    ADD_H5_F_TEST (ext_dangle_nofile_f "${vol_name}" "${vol_env}" 2 ${HDF_EXT_SRC_FILE}.h5 ext -v -s /group_ext/extlink_notyet2 -d /copy_dangle2_2)

    # "Test copying a group contains external links without -f ext"
    ADD_H5_TEST (ext_link_group "${vol_name}" "${vol_env}" 2 ${HDF_EXT_SRC_FILE}.h5 -v -s /group_ext -d /copy1_group)

    # "Test copying a group contains external links with -f ext"
    ADD_H5_F_TEST (ext_link_group_f "${vol_name}" "${vol_env}" 2 ${HDF_EXT_SRC_FILE}.h5 ext -v -s /group_ext -d /copy2_group)

  ############# Test misc. ##############

    #-----------------------------------------------------------------
    # "Test copying object into group which doesn't exist, without -p"
    #
    ADD_H5_CMP_TEST (h5copy_misc1 "${vol_name}" "${vol_env}" 1 "h5copy error" ${HDF_FILE1}.h5 -v -s /simple -d /g1/g2/simple)

    #-------------------------------------------
    # "Test copying objects to the same file "
    #
    # - dataset
    ADD_H5_TEST_SAME (samefile1 "${vol_name}" "${vol_env}" 0 ${HDF_FILE1}.h5 /simple /simple -v -s /simple -d /simple_cp)
    # - group with some datasets
    if (USE_FILTER_DEFLATE)
      ADD_H5_TEST_SAME (samefile2 "${vol_name}" "${vol_env}" 0 ${HDF_FILE1}.h5 /grp_dsets /grp_dsets -v -s /grp_dsets -d /grp_dsets_cp)
    else ()
      ADD_H5_TEST_SAME (samefile2 "${vol_name}" "${vol_env}" 2 ${HDF_FILE1}.h5 /grp_dsets /grp_dsets -v -s /grp_dsets -d /grp_dsets_cp)
    endif ()

  ##############################################################################
  ###    P L U G I N  T E S T S
  ##############################################################################
  if (BUILD_SHARED_LIBS)
    ADD_H5_UD_TEST (h5copy_plugin_test "${vol_name}" "${vol_env}" 0 tudfilter.h5 -s /dynlibud -d /dynlibud tudfilter2.h5 )
    ADD_H5_UD_ERR_TEST (h5copy_plugin_fail "${vol_name}" "${vol_env}" 2 tudfilter.h5 -s /dynlibud -d /dynlibud tudfilter2.h5)
  endif ()
endforeach()