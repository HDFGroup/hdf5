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
      h5copy_misc1.err
      tudfilter.h5.txt
      tudfilter.h5_ERR.txt
      h5copy_plugin_fail_ERR.out.h5.txt
      h5copy_plugin_test.out.h5.txt
      h5copy_help1.ddl
      h5copy_help2.ddl
  )

  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/testfiles")

  foreach (listfiles ${LIST_HDF5_TEST_FILES})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/${listfiles}" "${PROJECT_BINARY_DIR}/testfiles/${listfiles}" "h5copy_files")
  endforeach ()

  foreach (listothers ${LIST_OTHER_TEST_FILES})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/expected/${listothers}" "${PROJECT_BINARY_DIR}/testfiles/${listothers}" "h5copy_files")
  endforeach ()
   add_custom_target(h5copy_files ALL COMMENT "Copying files needed by h5copy tests" DEPENDS ${h5copy_files_list})

##############################################################################
##############################################################################
###           T H E   T E S T S  M A C R O S                               ###
##############################################################################
##############################################################################

  #
  # Perform h5copy according to passing parameters
  #
  macro (ADD_H5_F_TEST testname resultcode infile fparam vparam sparam srcname dparam dstname)
    # Remove any output file left over from previous test run
    add_test (
        NAME H5COPY_F-${testname}-clear-objects
        COMMAND ${CMAKE_COMMAND} -E remove ./testfiles/${testname}.out.h5
    )

    add_test (
        NAME H5COPY_F-${testname}
        COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5copy> -f ${fparam} -i ./testfiles/${infile} -o ./testfiles/${testname}.out.h5 ${vparam} ${sparam} ${srcname} ${dparam} ${dstname} ${ARGN}
    )
    set_tests_properties (H5COPY_F-${testname} PROPERTIES DEPENDS H5COPY_F-${testname}-clear-objects)
    if ("H5COPY_F-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5COPY_F-${testname} PROPERTIES DISABLED true)
    endif ()

    # resultcode=2 will cause the test to skip the diff test
    if (NOT "${resultcode}" STREQUAL "2")
      add_test (
          NAME H5COPY_F-${testname}-DIFF
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5diff> -v ./testfiles/${infile} ./testfiles/${testname}.out.h5 ${srcname} ${dstname}
      )
      set_tests_properties (H5COPY_F-${testname}-DIFF PROPERTIES DEPENDS H5COPY_F-${testname})
      if ("${resultcode}" STREQUAL "1")
        set_tests_properties (H5COPY_F-${testname}-DIFF PROPERTIES WILL_FAIL "true")
      endif ()
      if ("H5COPY_F-${testname}-DIFF" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5COPY_F-${testname}-DIFF PROPERTIES DISABLED true)
      endif ()
    endif ()
    add_test (
        NAME H5COPY_F-${testname}-clean-objects
        COMMAND ${CMAKE_COMMAND} -E remove ./testfiles/${testname}.out.h5
    )
    if (NOT "${resultcode}" STREQUAL "2")
      set_tests_properties (H5COPY_F-${testname}-clean-objects PROPERTIES DEPENDS H5COPY_F-${testname}-DIFF)
    else ()
      set_tests_properties (H5COPY_F-${testname}-clean-objects PROPERTIES DEPENDS H5COPY_F-${testname})
    endif ()
  endmacro ()

  macro (ADD_H5_TEST testname resultcode infile vparam sparam srcname dparam dstname)
    # Remove any output file left over from previous test run
    add_test (
        NAME H5COPY-${testname}-clear-objects
        COMMAND ${CMAKE_COMMAND} -E remove ./testfiles/${testname}.out.h5
    )

    add_test (
        NAME H5COPY-${testname}
        COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5copy> -i ./testfiles/${infile} -o ./testfiles/${testname}.out.h5 ${vparam} ${sparam} ${srcname} ${dparam} ${dstname} ${ARGN}
    )
    set_tests_properties (H5COPY-${testname} PROPERTIES DEPENDS H5COPY-${testname}-clear-objects)
    if ("H5COPY-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5COPY-${testname} PROPERTIES DISABLED true)
    endif ()

    # resultcode=2 will cause the test to skip the diff test
    if (NOT "${resultcode}" STREQUAL "2")
      add_test (
          NAME H5COPY-${testname}-DIFF
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5diff> -v ./testfiles/${infile} ./testfiles/${testname}.out.h5 ${srcname} ${dstname}
      )
      set_tests_properties (H5COPY-${testname}-DIFF PROPERTIES DEPENDS H5COPY-${testname})
      if ("${resultcode}" STREQUAL "1")
        set_tests_properties (H5COPY-${testname}-DIFF PROPERTIES WILL_FAIL "true")
      endif ()
      if ("H5COPY-${testname}-DIFF" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5COPY-${testname}-DIFF PROPERTIES DISABLED true)
      endif ()
    endif ()
    add_test (
        NAME H5COPY-${testname}-clean-objects
        COMMAND ${CMAKE_COMMAND} -E remove ./testfiles/${testname}.out.h5
    )
    if (NOT "${resultcode}" STREQUAL "2")
      set_tests_properties (H5COPY-${testname}-clean-objects PROPERTIES DEPENDS H5COPY-${testname}-DIFF)
    else ()
      set_tests_properties (H5COPY-${testname}-clean-objects PROPERTIES DEPENDS H5COPY-${testname})
    endif ()
  endmacro ()

  macro (ADD_SKIP_H5_TEST testname skipresultfile)
    if (NOT HDF5_USING_ANALYSIS_TOOL)
      add_test (
          NAME H5COPY-${testname}-${skipresultfile}
          COMMAND ${CMAKE_COMMAND} -E echo "SKIP ${testname}-${skipresultfile} ${ARGN}"
      )
      set_property(TEST H5COPY-${testname}-${skipresultfile} PROPERTY DISABLED true)
    endif ()
  endmacro ()

  macro (ADD_H5_TEST2 testname resultcode infile  psparam pdparam vparam sparam srcname dparam dstname)
    # Remove any output file left over from previous test run
    add_test (
        NAME H5COPY-${testname}-clear-objects
        COMMAND ${CMAKE_COMMAND} -E remove ./testfiles/${testname}.out.h5
    )

    add_test (
        NAME H5COPY-${testname}-prefill
        COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5copy> -i ./testfiles/${infile} -o ./testfiles/${testname}.out.h5 -v -s ${psparam} -d ${pdparam}
    )
    set_tests_properties (H5COPY-${testname}-prefill PROPERTIES DEPENDS H5COPY-${testname}-clear-objects)
    if ("H5COPY-${testname}-prefill" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5COPY-${testname}-prefill PROPERTIES DISABLED true)
    endif ()

    add_test (
        NAME H5COPY-${testname}
        COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5copy> -i ./testfiles/${infile} -o ./testfiles/${testname}.out.h5 ${vparam} ${sparam} ${srcname} ${dparam} ${dstname} ${ARGN}
    )
    set_tests_properties (H5COPY-${testname} PROPERTIES DEPENDS H5COPY-${testname}-prefill)
    if ("H5COPY-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5COPY-${testname} PROPERTIES DISABLED true)
    endif ()
    # resultcode=2 will cause the test to skip the diff test
    if (NOT "${resultcode}" STREQUAL "2")
      add_test (
          NAME H5COPY-${testname}-DIFF
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5diff> -v ./testfiles/${infile} ./testfiles/${testname}.out.h5 ${srcname} ${dstname}
      )
      set_tests_properties (H5COPY-${testname}-DIFF PROPERTIES DEPENDS H5COPY-${testname})
      if ("${resultcode}" STREQUAL "1")
        set_tests_properties (H5COPY-${testname}-DIFF PROPERTIES WILL_FAIL "true")
      endif ()
      if ("H5COPY-${testname}-DIFF" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5COPY-${testname}-DIFF PROPERTIES DISABLED true)
      endif ()
    endif ()
    add_test (
        NAME H5COPY-${testname}-clean-objects
        COMMAND ${CMAKE_COMMAND} -E remove ./testfiles/${testname}.out.h5
    )
    if (NOT "${resultcode}" STREQUAL "2")
      set_tests_properties (H5COPY-${testname}-clean-objects PROPERTIES DEPENDS H5COPY-${testname}-DIFF)
    else ()
      set_tests_properties (H5COPY-${testname}-clean-objects PROPERTIES DEPENDS H5COPY-${testname})
    endif ()
  endmacro ()

  macro (ADD_H5_TEST_SAME testname resultcode pfile psparam pdparam vparam sparam srcname dparam dstname)
    # Remove any output file left over from previous test run
    add_test (
        NAME H5COPY_SAME-${testname}-clear-objects
        COMMAND ${CMAKE_COMMAND} -E remove ./testfiles/${testname}.out.h5
    )

    add_test (
        NAME H5COPY_SAME-${testname}-prefill
        COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5copy> -i ./testfiles/${pfile} -o ./testfiles/${testname}.out.h5 -v -s ${psparam} -d ${pdparam}
    )
    set_tests_properties (H5COPY_SAME-${testname}-prefill PROPERTIES DEPENDS H5COPY_SAME-${testname}-clear-objects)
    if ("H5COPY_SAME-${testname}-prefill" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5COPY_SAME-${testname}-prefill PROPERTIES DISABLED true)
    endif ()

    add_test (
        NAME H5COPY_SAME-${testname}
        COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5copy> -i ./testfiles/${testname}.out.h5 -o ./testfiles/${testname}.out.h5 ${vparam} ${sparam} ${srcname} ${dparam} ${dstname} ${ARGN}
    )
    set_tests_properties (H5COPY_SAME-${testname} PROPERTIES DEPENDS H5COPY_SAME-${testname}-prefill)
    if ("H5COPY_SAME-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5COPY_SAME-${testname} PROPERTIES DISABLED true)
    endif ()
    # resultcode=2 will cause the test to skip the diff test
    if (NOT "${resultcode}" STREQUAL "2")
      add_test (
          NAME H5COPY_SAME-${testname}-DIFF
          COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5diff> -v ./testfiles/${testname}.out.h5 ./testfiles/${testname}.out.h5 ${srcname} ${dstname}
      )
      set_tests_properties (H5COPY_SAME-${testname}-DIFF PROPERTIES DEPENDS H5COPY_SAME-${testname})
      if ("${resultcode}" STREQUAL "1")
        set_tests_properties (H5COPY_SAME-${testname}-DIFF PROPERTIES WILL_FAIL "true")
      endif ()
      if ("H5COPY_SAME-${testname}-DIFF" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5COPY_SAME-${testname}-DIFF PROPERTIES DISABLED true)
      endif ()
    endif ()
    add_test (
        NAME H5COPY_SAME-${testname}-clean-objects
        COMMAND ${CMAKE_COMMAND} -E remove ./testfiles/${testname}.out.h5
    )
    if (NOT "${resultcode}" STREQUAL "2")
      set_tests_properties (H5COPY_SAME-${testname}-clean-objects PROPERTIES DEPENDS H5COPY_SAME-${testname}-DIFF)
    else ()
      set_tests_properties (H5COPY_SAME-${testname}-clean-objects PROPERTIES DEPENDS H5COPY_SAME-${testname})
    endif ()
  endmacro ()

  #
  # Similar to ADD_H5_TEST macro. Compare to outputs from source & target
  # files instead of checking with h5ls.
  #
  macro (ADD_H5_CMP_TEST testname resultcode result_errcheck infile vparam sparam srcname dparam dstname)
    # Remove any output file left over from previous test run
    add_test (
        NAME H5COPY-CMP-${testname}-clear-objects
        COMMAND ${CMAKE_COMMAND} -E remove ./testfiles/${testname}.out.h5
    )
    # If using memchecker add tests without using scripts
    if (HDF5_USING_ANALYSIS_TOOL)
      add_test (NAME H5COPY-CMP-${testname} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5copy> -i ./testfiles/${infile} -o ./testfiles/${testname}.out.h5 ${vparam} ${sparam} ${srcname} ${dparam} ${dstname} ${ARGN})
      if ("${resultcode}" STREQUAL "1")
        set_tests_properties (H5COPY-CMP-${testname} PROPERTIES WILL_FAIL "true")
      endif ()
    else ()
      add_test (
          NAME H5COPY-CMP-${testname}
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
    set_tests_properties (H5COPY-CMP-${testname} PROPERTIES DEPENDS H5COPY-CMP-${testname}-clear-objects)
    if ("H5COPY-CMP-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5COPY-CMP-${testname} PROPERTIES DISABLED true)
    endif ()
    add_test (
        NAME H5COPY-CMP-${testname}-clean-objects
        COMMAND ${CMAKE_COMMAND} -E remove ./testfiles/${testname}.out.h5
    )
    set_tests_properties (H5COPY-CMP-${testname}-clean-objects PROPERTIES DEPENDS H5COPY-CMP-${testname})
  endmacro ()

  macro (ADD_H5_UD_TEST testname resultcode infile sparam srcname dparam dstname cmpfile)
    if (NOT HDF5_USING_ANALYSIS_TOOL)
      # Remove any output file left over from previous test run
      add_test (
          NAME H5COPY_UD-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/${testname}.out.h5
      )
      if ("${resultcode}" STREQUAL "2")
        add_test (
            NAME H5COPY_UD-${testname}
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
                -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}"
                -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
                -P "${HDF_RESOURCES_DIR}/runTest.cmake"
        )
      else ()
        add_test (
            NAME H5COPY_UD-${testname}
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
                -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}/plugins"
                -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
                -P "${HDF_RESOURCES_DIR}/runTest.cmake"
        )
      endif ()
      set_tests_properties (H5COPY_UD-${testname} PROPERTIES DEPENDS H5COPY_UD-${testname}-clear-objects)
      if ("H5COPY_UD-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5COPY_UD-${testname} PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME H5COPY_UD-${testname}-DIFF
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
              -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}/plugins"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
              -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (H5COPY_UD-${testname}-DIFF PROPERTIES DEPENDS H5COPY_UD-${testname})
      if ("H5COPY_UD-${testname}-DIFF" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5COPY_UD-${testname}-DIFF PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME H5COPY_UD-${testname}-clean-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/${testname}.out.h5
      )
      set_tests_properties (H5COPY_UD-${testname}-clean-objects PROPERTIES DEPENDS H5COPY_UD-${testname}-DIFF)
    endif ()
  endmacro ()

  macro (ADD_H5_UD_ERR_TEST testname resultcode infile sparam srcname dparam dstname cmpfile)
    if (NOT HDF5_USING_ANALYSIS_TOOL)
      # Remove any output file left over from previous test run
      add_test (
          NAME H5COPY_UD_ERR-${testname}-clear-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/${testname}_ERR.out.h5
      )
      if ("${resultcode}" STREQUAL "2")
        add_test (
            NAME H5COPY_UD_ERR-${testname}
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
                -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}"
                -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
                -P "${HDF_RESOURCES_DIR}/runTest.cmake"
        )
      else ()
        add_test (
            NAME H5COPY_UD_ERR-${testname}
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
                -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}/plugins"
                -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
                -P "${HDF_RESOURCES_DIR}/runTest.cmake"
        )
      endif ()
      set_tests_properties (H5COPY_UD_ERR-${testname} PROPERTIES DEPENDS H5COPY_UD_ERR-${testname}-clear-objects)
      if ("H5COPY_UD_ERR-${testname}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5COPY_UD_ERR-${testname} PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME H5COPY_UD_ERR-${testname}-DIFF
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
              -D "TEST_ENV_VALUE=${CMAKE_BINARY_DIR}/plugins"
              -D "TEST_LIBRARY_DIRECTORY=${CMAKE_TEST_OUTPUT_DIRECTORY}"
              -P "${HDF_RESOURCES_DIR}/runTest.cmake"
      )
      set_tests_properties (H5COPY_UD_ERR-${testname}-DIFF PROPERTIES DEPENDS H5COPY_UD_ERR-${testname})
      if ("H5COPY_UD_ERR-${testname}-DIFF" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (H5COPY_UD_ERR-${testname}-DIFF PROPERTIES DISABLED true)
      endif ()
      add_test (
          NAME H5COPY_UD_ERR-${testname}-clean-objects
          COMMAND ${CMAKE_COMMAND} -E remove testfiles/${testname}_ERR.out.h5
      )
      set_tests_properties (H5COPY_UD_ERR-${testname}-clean-objects PROPERTIES DEPENDS H5COPY_UD_ERR-${testname}-DIFF)
    endif ()
  endmacro ()

  macro (ADD_SIMPLE_TEST resultfile resultcode)
    # If using memchecker add tests without using scripts
    if (HDF5_USING_ANALYSIS_TOOL)
      add_test (NAME H5COPY-${resultfile} COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5copy> ${ARGN})
      if (${resultcode})
        set_tests_properties (H5COPY-${resultfile} PROPERTIES WILL_FAIL "true")
      endif ()
    else (HDF5_USING_ANALYSIS_TOOL)
      add_test (
          NAME H5COPY-${resultfile}
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
    set_tests_properties (H5COPY-${resultfile} PROPERTIES
        WORKING_DIRECTORY "${PROJECT_BINARY_DIR}"
    )
    if ("H5COPY-${resultfile}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5COPY-${resultfile} PROPERTIES DISABLED true)
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

# Test for help flag
  ADD_SIMPLE_TEST (h5copy_help1 0 -h)
  ADD_SIMPLE_TEST (h5copy_help2 0 --help)

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
  ADD_H5_CMP_TEST (h5copy_misc1 1 "h5copy error" ${HDF_FILE1}.h5 -v -s /simple -d /g1/g2/simple)

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
