
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
# included from CMakeTests.cmake

set (VOL_LIST
    vol_native
    vol_pass_through1
    vol_pass_through2
)

# native VOL = 0
# pass-through VOL = 1
set (vol_native native)
set (vol_pass_through1 "pass_through under_vol=0\;under_info={}")
set (vol_pass_through2 "pass_through under_vol=1\;under_info={under_vol=0\;under_info={}}")

foreach (voltest ${VOL_LIST})
  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/${voltest}")
  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/${voltest}/testfiles")
  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/${voltest}/testfiles/plist_files")
endforeach ()

foreach (voltest ${VOL_LIST})
  foreach (h5_tfile ${HDF5_TEST_FILES})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/${h5_tfile}" "${PROJECT_BINARY_DIR}/${voltest}/testfiles/${h5_tfile}" "HDF5_VOLTEST_LIB_files")
  endforeach ()
endforeach ()

foreach (voltest ${VOL_LIST})
  foreach (ref_file ${HDF5_REFERENCE_FILES})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/${ref_file}" "${PROJECT_BINARY_DIR}/${voltest}/testfiles/${ref_file}" "HDF5_VOLTEST_LIB_files")
  endforeach ()
endforeach ()

foreach (voltest ${VOL_LIST})
  foreach (h5_file ${HDF5_REFERENCE_TEST_FILES})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/${h5_file}" "${PROJECT_BINARY_DIR}/${voltest}/testfiles/${h5_file}" "HDF5_VOLTEST_LIB_files")
  endforeach ()
endforeach ()

foreach (voltest ${VOL_LIST})
  foreach (plistfile ${HDF5_REFERENCE_PLIST_FILES})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/plist_files/${plistfile}" "${PROJECT_BINARY_DIR}/${voltest}/testfiles/plist_files/${plistfile}" "HDF5_VOLTEST_LIB_files")
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/plist_files/def_${plistfile}" "${PROJECT_BINARY_DIR}/${voltest}/testfiles/plist_files/def_${plistfile}" "HDF5_VOLTEST_LIB_files")
  endforeach ()
endforeach ()

add_custom_target(HDF5_VOLTEST_LIB_files ALL COMMENT "Copying files needed by HDF5_VOLTEST_LIB tests" DEPENDS ${HDF5_VOLTEST_LIB_files_list})

##############################################################################
##############################################################################
###                         V O L   T E S T S                              ###
##############################################################################
##############################################################################

  set (H5_VOL_SKIP_TESTS
      cache
      cache_image
      accum
      fheap
      big
      vol
      error_test
      err_compat
      tcheck_version
      testmeta
      links_env
      external_env
      vds_env
  )
  if (NOT CYGWIN)
    list (REMOVE_ITEM H5_VOL_SKIP_TESTS big cache)
  endif ()

  # Windows only macro
  macro (CHECK_VOL_TEST voltest volname volinfo resultcode)
    if ("${voltest}" STREQUAL "flush1" OR "${voltest}" STREQUAL "flush2")
      if ("${volname}" STREQUAL "multi" OR "${volname}" STREQUAL "split")
        if (NOT BUILD_SHARED_LIBS AND NOT ${HDF_CFG_NAME} MATCHES "Debug")
          add_test (NAME VOL-${volname}-${voltest}
              COMMAND "${CMAKE_COMMAND}"
                  -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                  -D "TEST_PROGRAM=$<TARGET_FILE:${voltest}>"
                  -D "TEST_ARGS:STRING="
                  -D "TEST_VOL:STRING=${volinfo}"
                  -D "TEST_EXPECT=${resultcode}"
                  -D "TEST_OUTPUT=${volname}-${voltest}.out"
                  -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/${volname}"
                  -P "${HDF_RESOURCES_DIR}/volTest.cmake"
          )
          set_tests_properties (VOL-${volname}-${voltest} PROPERTIES
              ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/${volname}"
              WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/${volname}
          )
          if ("VOL-${volname}-${voltest}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
            set_tests_properties (VOL-${volname}-${voltest} PROPERTIES DISABLED true)
          endif ()
        else ()
          add_test (NAME VOL-${volname}-${voltest}
              COMMAND ${CMAKE_COMMAND} -E echo "SKIP VOL-${volname}-${voltest}"
          )
          set_tests_properties (VOL-${volname}-${voltest} PROPERTIES DISABLED true)
        endif ()
      else ()
        add_test (NAME VOL-${volname}-${voltest}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                -D "TEST_PROGRAM=$<TARGET_FILE:${voltest}>"
                -D "TEST_ARGS:STRING="
                -D "TEST_VOL:STRING=${volinfo}"
                -D "TEST_EXPECT=${resultcode}"
                -D "TEST_OUTPUT=${volname}-${voltest}.out"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/${volname}"
                -P "${HDF_RESOURCES_DIR}/volTest.cmake"
        )
        set_tests_properties (VOL-${volname}-${voltest} PROPERTIES
            ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/${volname}"
            WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/${volname}
        )
        if ("VOL-${volname}-${voltest}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
          set_tests_properties (VOL-${volname}-${voltest} PROPERTIES DISABLED true)
        endif ()
      endif ()
    else ()
      add_test (NAME VOL-${volname}-${voltest}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:${voltest}>"
              -D "TEST_ARGS:STRING="
              -D "TEST_VOL:STRING=${volinfo}"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_OUTPUT=${volname}-${voltest}.out"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/${volname}"
              -P "${HDF_RESOURCES_DIR}/volTest.cmake"
      )
      set_tests_properties (VOL-${volname}-${voltest} PROPERTIES
          ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/${volname}"
          WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/${volname}
      )
      if ("VOL-${volname}-${voltest}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (VOL-${volname}-${voltest} PROPERTIES DISABLED true)
      endif ()
    endif ()
  endmacro ()

  macro (DO_VOL_TEST voltest volname volinfo resultcode)
      #message(STATUS "${voltest}-${volname} with ${volinfo}")
      add_test (NAME VOL-${volname}-${voltest}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:${voltest}>"
              -D "TEST_ARGS:STRING="
              -D "TEST_VOL:STRING=${volinfo}"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_OUTPUT=${volname}-${voltest}.out"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/${volname}"
              -P "${HDF_RESOURCES_DIR}/volTest.cmake"
      )
      set_tests_properties (VOL-${volname}-${voltest} PROPERTIES
          ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/${volname}"
          WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/${volname}
      )
      if ("VOL-${volname}-${voltest}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (VOL-${volname}-${voltest} PROPERTIES DISABLED true)
      endif ()
  endmacro ()

  macro (ADD_VOL_TEST volname volinfo resultcode)
    #message(STATUS "volname=${volname} volinfo=${volinfo}")
    foreach (h5_test ${H5_EXPRESS_TESTS})
      if (NOT h5_test IN_LIST H5_VOL_SKIP_TESTS)
        if (WIN32)
          CHECK_VOL_TEST (${h5_test} ${volname} "${volinfo}" ${resultcode})
        else ()
          DO_VOL_TEST (${h5_test} ${volname} "${volinfo}" ${resultcode})
        endif ()
      endif ()
    endforeach ()
    foreach (h5_test ${H5_TESTS})
      if (NOT h5_test IN_LIST H5_VOL_SKIP_TESTS)
        if (WIN32)
          CHECK_VOL_TEST (${h5_test} ${volname} "${volinfo}" ${resultcode})
        else ()
          DO_VOL_TEST (${h5_test} ${volname} "${volinfo}" ${resultcode})
        endif ()
      endif ()
    endforeach ()
    set_tests_properties (VOL-${volname}-flush2 PROPERTIES DEPENDS VOL-${volname}-flush1)
    set_tests_properties (VOL-${volname}-flush1 PROPERTIES TIMEOUT 10)
    set_tests_properties (VOL-${volname}-flush2 PROPERTIES TIMEOUT 10)
    set_tests_properties (VOL-${volname}-istore PROPERTIES TIMEOUT ${CTEST_VERY_LONG_TIMEOUT})
    if (NOT CYGWIN)
      set_tests_properties (VOL-${volname}-cache PROPERTIES TIMEOUT ${CTEST_VERY_LONG_TIMEOUT})
    endif ()
    if (HDF5_TEST_FHEAP_PASSTHROUGH_VOL)
      add_test (NAME VOL-${volname}-fheap
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:fheap>"
              -D "TEST_ARGS:STRING="
              -D "TEST_VOL:STRING=${volinfo}"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_OUTPUT=${volname}-fheap.out"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/${volname}"
              -P "${HDF_RESOURCES_DIR}/volTest.cmake"
      )
      set_tests_properties (VOL-${volname}-fheap PROPERTIES
          TIMEOUT ${CTEST_VERY_LONG_TIMEOUT}
          ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/${volname}"
          WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/${volname}
      )
      if ("VOL-${volname}-fheap" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (VOL-${volname}-fheap PROPERTIES DISABLED true)
      endif ()
    endif ()
  endmacro ()

  # Run test with different Virtual File Driver
  foreach (volname ${VOL_LIST})
    #message(STATUS "volname=${volname}")
    foreach (volinfo IN LISTS ${volname})
      #message(STATUS "${volname} volinfo=${volinfo}")
      ADD_VOL_TEST (${volname} "${volinfo}" 0)
    endforeach ()
  endforeach ()

