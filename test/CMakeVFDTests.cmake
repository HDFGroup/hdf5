
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
H5_CREATE_VFD_DIR()

# create more test folders for each VFD
foreach (vfdtest ${VFD_LIST})
  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/${vfdtest}/testfiles")
  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/${vfdtest}/testfiles/plist_files")
endforeach ()

foreach (vfdtest ${VFD_LIST})
  foreach (h5_tfile ${HDF5_TEST_FILES})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/${h5_tfile}" "${PROJECT_BINARY_DIR}/${vfdtest}/testfiles/${h5_tfile}" "HDF5_VFDTEST_LIB_files")
  endforeach ()
endforeach ()

foreach (vfdtest ${VFD_LIST})
  foreach (ref_file ${HDF5_REFERENCE_FILES})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/${ref_file}" "${PROJECT_BINARY_DIR}/${vfdtest}/testfiles/${ref_file}" "HDF5_VFDTEST_LIB_files")
  endforeach ()
endforeach ()

foreach (vfdtest ${VFD_LIST})
  foreach (h5_file ${HDF5_REFERENCE_TEST_FILES})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/${h5_file}" "${HDF5_TEST_BINARY_DIR}/${vfdtest}/testfiles/${h5_file}" "HDF5_VFDTEST_LIB_files")
  endforeach ()
endforeach ()

foreach (vfdtest ${VFD_LIST})
  foreach (plistfile ${HDF5_REFERENCE_PLIST_FILES})
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/plist_files/${plistfile}" "${PROJECT_BINARY_DIR}/${vfdtest}/testfiles/plist_files/${plistfile}" "HDF5_VFDTEST_LIB_files")
    HDFTEST_COPY_FILE("${PROJECT_SOURCE_DIR}/testfiles/plist_files/def_${plistfile}" "${PROJECT_BINARY_DIR}/${vfdtest}/testfiles/plist_files/def_${plistfile}" "HDF5_VFDTEST_LIB_files")
  endforeach ()
endforeach ()

add_custom_target(HDF5_VFDTEST_LIB_files ALL COMMENT "Copying files needed by HDF5_VFDTEST_LIB tests" DEPENDS ${HDF5_VFDTEST_LIB_files_list})

##############################################################################
##############################################################################
###                         V F D   T E S T S                              ###
##############################################################################
##############################################################################

  set (H5_VFD_SKIP_TESTS
      cache
      cache_image
      accum
      fheap
      big
      vfd
      error_test
      err_compat
      tcheck_version
      testmeta
      links_env
      external_env
      vds_env
      mirror_vfd
      ros3
      hdfs
  )

  # Skip several tests with subfiling VFD, mostly due
  # to no support for collective I/O
  set (H5_VFD_subfiling_SKIP_TESTS
    cache_api
    chunk_info
    cmpd_dset
    cork
    dangle
    direct_chunk
    dsets
    dt_arith
    dtransform
    extend
    fillval
    filter_fail
    istore
    links
    mf
    objcopy
    objcopy_ref
    ohdr
    set_extent
    testhdf5
    unlink
    unregister
    vol
  )

  if (NOT CYGWIN)
    list (REMOVE_ITEM H5_VFD_SKIP_TESTS big cache)
  endif ()

  # Windows only macro
  macro (CHECK_VFD_TEST vfdtest vfdname resultcode)
    if ("${vfdtest}" STREQUAL "flush1" OR "${vfdtest}" STREQUAL "flush2")
      if ("${vfdname}" STREQUAL "multi" OR "${vfdname}" STREQUAL "split")
        if (NOT BUILD_SHARED_LIBS AND NOT HDF_CFG_NAME MATCHES "Debug")
          add_test (NAME VFD-${vfdname}-${vfdtest}
              COMMAND "${CMAKE_COMMAND}"
                  -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                  -D "TEST_PROGRAM=$<TARGET_FILE:${vfdtest}>"
                  -D "TEST_ARGS:STRING="
                  -D "TEST_VFD:STRING=${vfdname}"
                  -D "TEST_EXPECT=${resultcode}"
                  -D "TEST_OUTPUT=${vfdname}-${vfdtest}.out"
                  -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/${vfdname}"
                  -P "${HDF_RESOURCES_DIR}/vfdTest.cmake"
          )
          set_tests_properties (VFD-${vfdname}-${vfdtest} PROPERTIES
              ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/${vfdname}"
              WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/${vfdname}
          )
          if ("VFD-${vfdname}-${vfdtest}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
            set_tests_properties (VFD-${vfdname}-${vfdtest} PROPERTIES DISABLED true)
          endif ()
        else ()
          add_test (NAME VFD-${vfdname}-${vfdtest}
              COMMAND ${CMAKE_COMMAND} -E echo "SKIP VFD-${vfdname}-${vfdtest}"
          )
          set_tests_properties (VFD-${vfdname}-${vfdtest} PROPERTIES DISABLED true)
        endif ()
      else ()
        add_test (NAME VFD-${vfdname}-${vfdtest}
            COMMAND "${CMAKE_COMMAND}"
                -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
                -D "TEST_PROGRAM=$<TARGET_FILE:${vfdtest}>"
                -D "TEST_ARGS:STRING="
                -D "TEST_VFD:STRING=${vfdname}"
                -D "TEST_EXPECT=${resultcode}"
                -D "TEST_OUTPUT=${vfdname}-${vfdtest}.out"
                -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/${vfdname}"
                -P "${HDF_RESOURCES_DIR}/vfdTest.cmake"
        )
        set_tests_properties (VFD-${vfdname}-${vfdtest} PROPERTIES
            ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/${vfdname}"
            WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/${vfdname}
        )
        if ("VFD-${vfdname}-${vfdtest}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
          set_tests_properties (VFD-${vfdname}-${vfdtest} PROPERTIES DISABLED true)
        endif ()
      endif ()
    else ()
      add_test (NAME VFD-${vfdname}-${vfdtest}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:${vfdtest}>"
              -D "TEST_ARGS:STRING="
              -D "TEST_VFD:STRING=${vfdname}"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_OUTPUT=${vfdname}-${vfdtest}.out"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/${vfdname}"
              -P "${HDF_RESOURCES_DIR}/vfdTest.cmake"
      )
      set_tests_properties (VFD-${vfdname}-${vfdtest} PROPERTIES
          ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/${vfdname}"
          WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/${vfdname}
      )
      if ("VFD-${vfdname}-${vfdtest}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (VFD-${vfdname}-${vfdtest} PROPERTIES DISABLED true)
      endif ()
    endif ()
  endmacro ()

  macro (DO_VFD_TEST vfdtest vfdname resultcode)
    if (NOT "${vfdtest}" IN_LIST H5_VFD_${vfdname}_SKIP_TESTS)
      add_test (NAME VFD-${vfdname}-${vfdtest}
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:${vfdtest}>"
              -D "TEST_ARGS:STRING="
              -D "TEST_VFD:STRING=${vfdname}"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_OUTPUT=${vfdname}-${vfdtest}.out"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/${vfdname}"
              -P "${HDF_RESOURCES_DIR}/vfdTest.cmake"
      )
      set_tests_properties (VFD-${vfdname}-${vfdtest} PROPERTIES
          ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/${vfdname}"
          WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/${vfdname}
      )
      if ("VFD-${vfdname}-${vfdtest}" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (VFD-${vfdname}-${vfdtest} PROPERTIES DISABLED true)
      endif ()
    endif ()
  endmacro ()

  macro (ADD_VFD_TEST vfdname resultcode)
    foreach (h5_test ${H5_EXPRESS_TESTS})
      if (NOT h5_test IN_LIST H5_VFD_SKIP_TESTS)
        if (WIN32)
          CHECK_VFD_TEST (${h5_test} ${vfdname} ${resultcode})
        else ()
          DO_VFD_TEST (${h5_test} ${vfdname} ${resultcode})
        endif ()
      endif ()
    endforeach ()
    foreach (h5_test ${H5_TESTS})
      if (NOT h5_test IN_LIST H5_VFD_SKIP_TESTS)
        if (WIN32)
          CHECK_VFD_TEST (${h5_test} ${vfdname} ${resultcode})
        else ()
          DO_VFD_TEST (${h5_test} ${vfdname} ${resultcode})
        endif ()
      endif ()
    endforeach ()
    if (NOT "flush2" IN_LIST H5_VFD_${vfdname}_SKIP_TESTS)
      if (NOT "flush1" IN_LIST H5_VFD_${vfdname}_SKIP_TESTS)
        set_tests_properties (VFD-${vfdname}-flush2 PROPERTIES DEPENDS VFD-${vfdname}-flush1)
      endif ()
      set_tests_properties (VFD-${vfdname}-flush2 PROPERTIES TIMEOUT 10)
    endif ()
    if (NOT "flush1" IN_LIST H5_VFD_${vfdname}_SKIP_TESTS)
      set_tests_properties (VFD-${vfdname}-flush1 PROPERTIES TIMEOUT 10)
    endif ()
    if (NOT "istore" IN_LIST H5_VFD_${vfdname}_SKIP_TESTS)
      set_tests_properties (VFD-${vfdname}-istore PROPERTIES TIMEOUT ${CTEST_VERY_LONG_TIMEOUT})
    endif ()
    if (NOT CYGWIN)
      set_tests_properties (VFD-${vfdname}-cache PROPERTIES TIMEOUT ${CTEST_VERY_LONG_TIMEOUT})
    endif ()
    if (HDF5_TEST_FHEAP_VFD)
      add_test (NAME VFD-${vfdname}-fheap
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
              -D "TEST_PROGRAM=$<TARGET_FILE:fheap>"
              -D "TEST_ARGS:STRING="
              -D "TEST_VFD:STRING=${vfdname}"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_OUTPUT=${vfdname}-fheap.out"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/${vfdname}"
              -P "${HDF_RESOURCES_DIR}/vfdTest.cmake"
      )
      set_tests_properties (VFD-${vfdname}-fheap PROPERTIES
          TIMEOUT ${CTEST_VERY_LONG_TIMEOUT}
          ENVIRONMENT "srcdir=${HDF5_TEST_BINARY_DIR}/${vfdname}"
          WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}/${vfdname}
      )
      if ("VFD-${vfdname}-fheap" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
        set_tests_properties (VFD-${vfdname}-fheap PROPERTIES DISABLED true)
      endif ()
    endif ()
  endmacro ()

  # Run test with different Virtual File Driver
  foreach (h5_vfd ${VFD_LIST})
    ADD_VFD_TEST (${h5_vfd} 0)
  endforeach ()

  ##############################################################################
  ###    V F D  P L U G I N  T E S T S
  ##############################################################################
  if (BUILD_SHARED_LIBS)
    if (WIN32)
      set (CMAKE_SEP "\;")
      set (BIN_REL_PATH "../../")
    else ()
      set (CMAKE_SEP ":")
      set (BIN_REL_PATH "../")
    endif ()

    add_test (NAME H5PLUGIN-vfd_plugin COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:vfd_plugin>)
    set_tests_properties (H5PLUGIN-vfd_plugin PROPERTIES
        ENVIRONMENT "HDF5_PLUGIN_PATH=${CMAKE_BINARY_DIR}/null_vfd_plugin_dir;srcdir=${HDF5_TEST_BINARY_DIR}"
        WORKING_DIRECTORY ${HDF5_TEST_BINARY_DIR}
    )
    if ("H5PLUGIN-vfd_plugin" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
      set_tests_properties (H5PLUGIN-vfd_plugin PROPERTIES DISABLED true)
     endif ()
  endif ()
