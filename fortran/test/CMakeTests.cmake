
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################
if (BUILD_SHARED_LIBS)
  file (MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/fshared")
endif ()

# Remove any output file left over from previous test run
add_test (
    NAME FORTRAN_testhdf5-clear-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove
        a.h5
        b.h5
        c.h5
        d.h5
        dsetf_F03.h5
        enum1.h5
        extern_1a.raw
        extern_2a.raw
        extern_3a.raw
        extern_4a.raw
        extren_raw.raw
        get_info.h5
        nbit.h5
        t_array_F03.h5
        t_bit_F03.h5
        t_controlchar_F03.h5
        t_enum_F03.h5
        t_objref_F03.h5
        t_opaque_F03.h5
        t_regref_F03.h5
        t_string_F03.h5
        t_vlen_F03.h5
        t_vlstring_F03.h5
        t_vlstringrw_F03.h5
        tarray1.h5
        tarray2.h5
        tarray3.h5
        test_create.h5
        tget_file_image.h5
        th5o_ref.h5
        titerate.h5
        vds.h5
        visit.h5
)
if (NOT "${last_test}" STREQUAL "")
  set_tests_properties (FORTRAN_testhdf5-clear-objects PROPERTIES DEPENDS ${last_test})
endif ()
set (last_test "FORTRAN_testhdf5-clear-objects")

if (HDF5_ENABLE_USING_MEMCHECKER)
  add_test (NAME FORTRAN_testhdf5_fortran COMMAND $<TARGET_FILE:testhdf5_fortran>)
else ()
  add_test (NAME FORTRAN_testhdf5_fortran COMMAND "${CMAKE_COMMAND}"
      -D "TEST_PROGRAM=$<TARGET_FILE:testhdf5_fortran>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_SKIP_COMPARE=TRUE"
      -D "TEST_REGEX= 0 error.s."
      -D "TEST_MATCH= 0 error(s)"
      -D "TEST_OUTPUT=testhdf5_fortran.txt"
      #-D "TEST_REFERENCE=testhdf5_fortran.out"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
      -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
  )
endif ()
#set_tests_properties (FORTRAN_testhdf5_fortran PROPERTIES PASS_REGULAR_EXPRESSION "[ ]*0 error.s")
set_tests_properties (FORTRAN_testhdf5_fortran PROPERTIES DEPENDS FORTRAN_testhdf5-clear-objects)

#-- Adding test for testhdf5_fortran_1_8
if (HDF5_ENABLE_USING_MEMCHECKER)
  add_test (NAME FORTRAN_testhdf5_fortran_1_8 COMMAND $<TARGET_FILE:testhdf5_fortran_1_8>)
else ()
  add_test (NAME FORTRAN_testhdf5_fortran_1_8 COMMAND "${CMAKE_COMMAND}"
      -D "TEST_PROGRAM=$<TARGET_FILE:testhdf5_fortran_1_8>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_SKIP_COMPARE=TRUE"
      -D "TEST_REGEX= 0 error.s."
      -D "TEST_MATCH= 0 error(s)"
      -D "TEST_OUTPUT=testhdf5_fortran_1_8.txt"
      #-D "TEST_REFERENCE=testhdf5_fortran_1_8.out"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
      -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
  )
endif ()
#set_tests_properties (FORTRAN_testhdf5_fortran_1_8 PROPERTIES PASS_REGULAR_EXPRESSION "[ ]*0 error.s")
set_tests_properties (FORTRAN_testhdf5_fortran_1_8 PROPERTIES DEPENDS FORTRAN_testhdf5_fortran)

#-- Adding test for fortranlib_test_F03
if (HDF5_ENABLE_F2003)
  if (HDF5_ENABLE_USING_MEMCHECKER)
    add_test (NAME FORTRAN_fortranlib_test_F03 COMMAND $<TARGET_FILE:fortranlib_test_F03>)
  else ()
    add_test (NAME FORTRAN_fortranlib_test_F03 COMMAND "${CMAKE_COMMAND}"
        -D "TEST_PROGRAM=$<TARGET_FILE:fortranlib_test_F03>"
        -D "TEST_ARGS:STRING="
        -D "TEST_EXPECT=0"
        -D "TEST_SKIP_COMPARE=TRUE"
        -D "TEST_REGEX= 0 error.s."
        -D "TEST_MATCH= 0 error(s)"
        -D "TEST_OUTPUT=fortranlib_test_F03.txt"
        #-D "TEST_REFERENCE=fortranlib_test_F03.out"
        -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
        -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
    )
  endif ()
#  set_tests_properties (FORTRAN_fortranlib_test_F03 PROPERTIES PASS_REGULAR_EXPRESSION "[ ]*0 error.s")
  set_tests_properties (FORTRAN_fortranlib_test_F03 PROPERTIES DEPENDS FORTRAN_testhdf5_fortran_1_8)
endif ()

#-- Adding test for fflush1
add_test (NAME FORTRAN_fflush1 COMMAND $<TARGET_FILE:fflush1>)
set_tests_properties (FORTRAN_fflush1 PROPERTIES DEPENDS FORTRAN_testhdf5-clear-objects)

#-- Adding test for fflush2
add_test (NAME FORTRAN_fflush2 COMMAND $<TARGET_FILE:fflush2>)
set_tests_properties (FORTRAN_fflush2 PROPERTIES DEPENDS FORTRAN_fflush1)

if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  add_test (
    NAME FORTRAN_testhdf5-shared-clear-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove
        a.h5
        b.h5
        c.h5
        d.h5
        dsetf_F03.h5
        enum1.h5
        extern_1a.raw
        extern_2a.raw
        extern_3a.raw
        extern_4a.raw
        extren_raw.raw
        get_info.h5
        nbit.h5
        t_array_F03.h5
        t_bit_F03.h5
        t_controlchar_F03.h5
        t_enum_F03.h5
        t_objref_F03.h5
        t_opaque_F03.h5
        t_regref_F03.h5
        t_string_F03.h5
        t_vlen_F03.h5
        t_vlstring_F03.h5
        t_vlstringrw_F03.h5
        tarray1.h5
        tarray2.h5
        tarray3.h5
        test_create.h5
        tget_file_image.h5
        th5o_ref.h5
        titerate.h5
        vds.h5
        visit.h5
        WORKING_DIRECTORY
            ${PROJECT_BINARY_DIR}/fshared
  )
  set_tests_properties (FORTRAN_testhdf5-shared-clear-objects PROPERTIES DEPENDS FORTRAN_testhdf5-clear-objects)

  if (HDF5_ENABLE_USING_MEMCHECKER)
    add_test (NAME FORTRAN_testhdf5_fortran-shared COMMAND $<TARGET_FILE:testhdf5_fortran-shared>)
  else ()
    add_test (NAME FORTRAN_testhdf5_fortran-shared COMMAND "${CMAKE_COMMAND}"
        -D "TEST_PROGRAM=$<TARGET_FILE:testhdf5_fortran-shared>"
        -D "TEST_ARGS:STRING="
        -D "TEST_EXPECT=0"
        -D "TEST_SKIP_COMPARE=TRUE"
        -D "TEST_REGEX= 0 error.s."
        -D "TEST_MATCH= 0 error(s)"
        -D "TEST_OUTPUT=testhdf5_fortran.txt"
        #-D "TEST_REFERENCE=testhdf5_fortran.out"
        -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/fshared"
        -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
    )
  endif ()
#  set_tests_properties (FORTRAN_testhdf5_fortran-shared PROPERTIES PASS_REGULAR_EXPRESSION "[ ]*0 error.s")
  set_tests_properties (FORTRAN_testhdf5_fortran-shared PROPERTIES DEPENDS "FORTRAN_testhdf5_fortran;FORTRAN_testhdf5-shared-clear-objects")

  #-- Adding test for testhdf5_fortran_1_8
  if (HDF5_ENABLE_USING_MEMCHECKER)
    add_test (NAME FORTRAN_testhdf5_fortran_1_8-shared COMMAND $<TARGET_FILE:testhdf5_fortran_1_8-shared>)
  else ()
    add_test (NAME FORTRAN_testhdf5_fortran_1_8-shared COMMAND "${CMAKE_COMMAND}"
        -D "TEST_PROGRAM=$<TARGET_FILE:testhdf5_fortran_1_8-shared>"
        -D "TEST_ARGS:STRING="
        -D "TEST_EXPECT=0"
        -D "TEST_SKIP_COMPARE=TRUE"
        -D "TEST_REGEX= 0 error.s."
        -D "TEST_MATCH= 0 error(s)"
        -D "TEST_OUTPUT=testhdf5_fortran_1_8.txt"
        #-D "TEST_REFERENCE=testhdf5_fortran_1_8.out"
        -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/fshared"
        -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
    )
  endif ()
#  set_tests_properties (FORTRAN_testhdf5_fortran_1_8-shared PROPERTIES PASS_REGULAR_EXPRESSION "[ ]*0 error.s")
  set_tests_properties (FORTRAN_testhdf5_fortran_1_8-shared PROPERTIES DEPENDS FORTRAN_testhdf5_fortran_1_8)

  #-- Adding test for fortranlib_test_F03
  if (HDF5_ENABLE_F2003)
    if (HDF5_ENABLE_USING_MEMCHECKER)
      add_test (NAME FORTRAN_fortranlib_test_F03-shared COMMAND $<TARGET_FILE:fortranlib_test_F03-shared>)
    else ()
      add_test (NAME FORTRAN_fortranlib_test_F03-shared COMMAND "${CMAKE_COMMAND}"
          -D "TEST_PROGRAM=$<TARGET_FILE:fortranlib_test_F03-shared>"
          -D "TEST_ARGS:STRING="
          -D "TEST_EXPECT=0"
          -D "TEST_SKIP_COMPARE=TRUE"
          -D "TEST_REGEX= 0 error.s."
          -D "TEST_MATCH= 0 error(s)"
          -D "TEST_OUTPUT=fortranlib_test_F03.txt"
          #-D "TEST_REFERENCE=fortranlib_test_F03.out"
          -D "TEST_FOLDER=${PROJECT_BINARY_DIR}/fshared"
          -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
      )
    endif ()
#    set_tests_properties (FORTRAN_fortranlib_test_F03-shared PROPERTIES PASS_REGULAR_EXPRESSION "[ ]*0 error.s")
    set_tests_properties (FORTRAN_fortranlib_test_F03-shared PROPERTIES DEPENDS FORTRAN_fortranlib_test_F03)
  endif ()

  #-- Adding test for fflush1
  add_test (NAME FORTRAN_fflush1-shared COMMAND $<TARGET_FILE:fflush1-shared>)
  set_tests_properties (FORTRAN_fflush1-shared PROPERTIES DEPENDS FORTRAN_fflush2)

  #-- Adding test for fflush2
  add_test (NAME FORTRAN_fflush2-shared COMMAND $<TARGET_FILE:fflush2-shared>)
  set_tests_properties (FORTRAN_fflush2-shared PROPERTIES DEPENDS FORTRAN_fflush1-shared)
endif ()
