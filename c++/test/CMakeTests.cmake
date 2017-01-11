HDFTEST_COPY_FILE("${HDF5_CPP_TEST_SOURCE_DIR}/th5s.h5" "${PROJECT_BINARY_DIR}/th5s.h5" "cpp_testhdf5_files")
add_custom_target(cpp_testhdf5_files ALL COMMENT "Copying files needed by cpp_testhdf5 tests" DEPENDS ${cpp_testhdf5_files_list})

##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################
# Remove any output file left over from previous test run
add_test (
    NAME CPP_testhdf5-clear-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove
            tattr_basic.h5
            tattr_compound.h5
            tattr_dtype.h5
            tattr_multi.h5
            tattr_scalar.h5
            tfattrs.h5
            titerate.h5
)

if (HDF5_ENABLE_USING_MEMCHECKER)
  add_test (NAME CPP_testhdf5 COMMAND $<TARGET_FILE:cpp_testhdf5>)
else ()
  add_test (NAME CPP_testhdf5 COMMAND "${CMAKE_COMMAND}"
      -D "TEST_PROGRAM=$<TARGET_FILE:cpp_testhdf5>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_SKIP_COMPARE=TRUE"
      -D "TEST_OUTPUT=cpp_testhdf5.txt"
      #-D "TEST_REFERENCE=cpp_testhdf5.out"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
      -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
  )
endif ()
set_tests_properties (CPP_testhdf5 PROPERTIES DEPENDS CPP_testhdf5-clear-objects)

if (HDF5_TEST_VFD)

  set (VFD_LIST
      sec2
      stdio
      core
      split
      multi
      family
  )

  if (DIRECT_VFD)
    set (VFD_LIST ${VFD_LIST} direct)
  endif ()

  MACRO (ADD_VFD_TEST vfdname resultcode)
    if (NOT HDF5_ENABLE_USING_MEMCHECKER)
      file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/${vfdname}")
      add_test (
          NAME CPP_VFD-${vfdname}-cpp_testhdf5-clear-objects
          COMMAND    ${CMAKE_COMMAND}
              -E remove
                  tattr_basic.h5
                  tattr_compound.h5
                  tattr_dtype.h5
                  tattr_multi.h5
                  tattr_scalar.h5
                  tfattrs.h5
                  titerate.h5
      )
      add_test (
        NAME CPP_VFD-${vfdname}-cpp_testhdf5
        COMMAND "${CMAKE_COMMAND}"
            -D "TEST_PROGRAM=$<TARGET_FILE:cpp_testhdf5>"
            -D "TEST_ARGS:STRING="
            -D "TEST_VFD:STRING=${vfdname}"
            -D "TEST_EXPECT=${resultcode}"
            -D "TEST_OUTPUT=cpp_testhdf5"
            -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/${vfdname}"
            -P "${HDF_RESOURCES_DIR}/vfdTest.cmake"
      )
      set_tests_properties (CPP_VFD-${vfdname}-cpp_testhdf5 PROPERTIES DEPENDS CPP_VFD-${vfdname}-cpp_testhdf5-clear-objects)
      set_tests_properties (CPP_VFD-${vfdname}-cpp_testhdf5 PROPERTIES TIMEOUT 30)
    endif ()
  ENDMACRO ()

  # Run test with different Virtual File Driver
  foreach (vfd ${VFD_LIST})
    ADD_VFD_TEST (${vfd} 0)
  endforeach ()

endif ()
