
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

# Remove any output file left over from previous test run
add_test (
    NAME HL_FORTRAN_test-clear-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove
        dsetf1.h5
        dsetf2.h5
        dsetf3.h5
        dsetf4.h5
        dsetf5.h5
        f1img.h5
        f1tab.h5
        f2tab.h5
        tstds.h5
)

if (HDF5_ENABLE_USING_MEMCHECKER)
  add_test (NAME HL_FORTRAN_f90_tstds COMMAND $<TARGET_FILE:hl_f90_tstds>)
else ()
  add_test (NAME HL_FORTRAN_f90_tstds COMMAND "${CMAKE_COMMAND}"
      -D "TEST_PROGRAM=$<TARGET_FILE:hl_f90_tstds>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_SKIP_COMPARE=TRUE"
      -D "TEST_OUTPUT=hl_f90_tstds.txt"
      #-D "TEST_REFERENCE=hl_f90_tstds.out"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
      -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
  )
endif ()
set_tests_properties (HL_FORTRAN_f90_tstds PROPERTIES DEPENDS HL_FORTRAN_test-clear-objects)

if (HDF5_ENABLE_USING_MEMCHECKER)
  add_test (NAME HL_FORTRAN_f90_tstlite COMMAND $<TARGET_FILE:hl_f90_tstlite>)
else ()
  add_test (NAME HL_FORTRAN_f90_tstlite COMMAND "${CMAKE_COMMAND}"
      -D "TEST_PROGRAM=$<TARGET_FILE:hl_f90_tstlite>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_SKIP_COMPARE=TRUE"
      -D "TEST_OUTPUT=hl_f90_tstlite.txt"
      #-D "TEST_REFERENCE=hl_f90_tstlite.out"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
      -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
  )
endif ()
set_tests_properties (HL_FORTRAN_f90_tstlite PROPERTIES DEPENDS HL_FORTRAN_test-clear-objects)

if (HDF5_ENABLE_USING_MEMCHECKER)
  add_test (NAME HL_FORTRAN_f90_tstimage COMMAND $<TARGET_FILE:hl_f90_tstimage>)
else ()
  add_test (NAME HL_FORTRAN_f90_tstimage COMMAND "${CMAKE_COMMAND}"
      -D "TEST_PROGRAM=$<TARGET_FILE:hl_f90_tstimage>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_SKIP_COMPARE=TRUE"
      -D "TEST_OUTPUT=hl_f90_tstimage.txt"
      #-D "TEST_REFERENCE=hl_f90_tstimage.out"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
      -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
  )
endif ()
set_tests_properties (HL_FORTRAN_f90_tstimage PROPERTIES DEPENDS HL_FORTRAN_test-clear-objects)

if (HDF5_ENABLE_USING_MEMCHECKER)
  add_test (NAME HL_FORTRAN_f90_tsttable COMMAND $<TARGET_FILE:hl_f90_tsttable>)
else ()
  add_test (NAME HL_FORTRAN_f90_tsttable COMMAND "${CMAKE_COMMAND}"
      -D "TEST_PROGRAM=$<TARGET_FILE:hl_f90_tsttable>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_SKIP_COMPARE=TRUE"
      -D "TEST_OUTPUT=hl_f90_tsttable.txt"
      #-D "TEST_REFERENCE=hl_f90_tsttable.out"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
      -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
  )
endif ()
set_tests_properties (HL_FORTRAN_f90_tsttable PROPERTIES DEPENDS HL_FORTRAN_test-clear-objects)

if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  add_test (
      NAME HL_FORTRAN_test-shared-clear-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove
          dsetf1.h5
          dsetf2.h5
          dsetf3.h5
          dsetf4.h5
          dsetf5.h5
          f1img.h5
          f1tab.h5
          f2tab.h5
          tstds.h5
  )
  set_tests_properties (HL_FORTRAN_test-shared-clear-objects
      PROPERTIES DEPENDS "HL_FORTRAN_f90_tsttable;HL_FORTRAN_f90_tstimage;HL_FORTRAN_f90_tstlite;HL_FORTRAN_f90_tstds"
  )

  if (HDF5_ENABLE_USING_MEMCHECKER)
    add_test (NAME HL_FORTRAN_f90_tstds-shared COMMAND $<TARGET_FILE:hl_f90_tstds-shared>)
  else ()
    add_test (NAME HL_FORTRAN_f90_tstds-shared COMMAND "${CMAKE_COMMAND}"
        -D "TEST_PROGRAM=$<TARGET_FILE:hl_f90_tstds-shared>"
        -D "TEST_ARGS:STRING="
        -D "TEST_EXPECT=0"
        -D "TEST_SKIP_COMPARE=TRUE"
        -D "TEST_OUTPUT=hl_f90_tstds-shared.txt"
        #-D "TEST_REFERENCE=hl_f90_tstds-shared.out"
        -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
        -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
    )
  endif ()
  set_tests_properties (HL_FORTRAN_f90_tstds-shared PROPERTIES DEPENDS HL_FORTRAN_test-shared-clear-objects)

  if (HDF5_ENABLE_USING_MEMCHECKER)
    add_test (NAME HL_FORTRAN_f90_tstlite-shared COMMAND $<TARGET_FILE:hl_f90_tstlite-shared>)
  else ()
    add_test (NAME HL_FORTRAN_f90_tstlite-shared COMMAND "${CMAKE_COMMAND}"
        -D "TEST_PROGRAM=$<TARGET_FILE:hl_f90_tstlite-shared>"
        -D "TEST_ARGS:STRING="
        -D "TEST_EXPECT=0"
        -D "TEST_SKIP_COMPARE=TRUE"
        -D "TEST_OUTPUT=hl_f90_tstlite-shared.txt"
        #-D "TEST_REFERENCE=hl_f90_tstlite-shared.out"
        -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
        -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
    )
  endif ()
  set_tests_properties (HL_FORTRAN_f90_tstlite-shared PROPERTIES DEPENDS HL_FORTRAN_test-shared-clear-objects)

  if (HDF5_ENABLE_USING_MEMCHECKER)
    add_test (NAME HL_FORTRAN_f90_tstimage-shared COMMAND $<TARGET_FILE:hl_f90_tstimage-shared>)
  else ()
    add_test (NAME HL_FORTRAN_f90_tstimage-shared COMMAND "${CMAKE_COMMAND}"
        -D "TEST_PROGRAM=$<TARGET_FILE:hl_f90_tstimage-shared>"
        -D "TEST_ARGS:STRING="
        -D "TEST_EXPECT=0"
        -D "TEST_SKIP_COMPARE=TRUE"
        -D "TEST_OUTPUT=hl_f90_tstimage-shared.txt"
        #-D "TEST_REFERENCE=hl_f90_tstimage-shared.out"
        -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
        -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
    )
  endif ()
  set_tests_properties (HL_FORTRAN_f90_tstimage-shared PROPERTIES DEPENDS HL_FORTRAN_test-shared-clear-objects)

  if (HDF5_ENABLE_USING_MEMCHECKER)
    add_test (NAME HL_FORTRAN_f90_tsttable-shared COMMAND $<TARGET_FILE:hl_f90_tsttable-shared>)
  else ()
    add_test (NAME HL_FORTRAN_f90_tsttable-shared COMMAND "${CMAKE_COMMAND}"
        -D "TEST_PROGRAM=$<TARGET_FILE:hl_f90_tsttable-shared>"
        -D "TEST_ARGS:STRING="
        -D "TEST_EXPECT=0"
        -D "TEST_SKIP_COMPARE=TRUE"
        -D "TEST_OUTPUT=hl_f90_tsttable-shared.txt"
        #-D "TEST_REFERENCE=hl_f90_tsttable-shared.out"
        -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
        -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
    )
  endif ()
  set_tests_properties (HL_FORTRAN_f90_tsttable-shared PROPERTIES DEPENDS HL_FORTRAN_test-shared-clear-objects)
endif ()
