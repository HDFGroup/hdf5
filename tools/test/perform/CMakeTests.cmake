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

HDFTEST_COPY_FILE("${HDF5_TOOLS_TST_DIR}/h5dump/testfiles/tfilters.h5" "${PROJECT_BINARY_DIR}/tfilters.h5" "zip_perf_files")
add_custom_target(zip_perf_files ALL COMMENT "Copying files needed by zip_perf tests" DEPENDS ${zip_perf_list})

#-----------------------------------------------------------------------------
# Add Tests
#-----------------------------------------------------------------------------
if (HDF5_TEST_SERIAL)
  set (PERFORM_CLEANFILES
          chunk.h5
          direct_write.h5
          unix.raw
          iopipe.h5
          iopipe.raw
          x-diag-rd.dat
          x-diag-wr.dat
          x-rowmaj-rd.dat
          x-rowmaj-wr.dat
          x-gnuplot
  )
  # Remove any output file left over from previous test run
  add_test (
      NAME PERFORM_h5perform-clear-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove ${PERFORM_CLEANFILES}
  )
  set_tests_properties (PERFORM_h5perform-clear-objects PROPERTIES FIXTURES_SETUP clear_perform)

  add_test (
      NAME PERFORM_h5perform-clean-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove ${PERFORM_CLEANFILES}
  )
  set_tests_properties (PERFORM_h5perform-clean-objects PROPERTIES FIXTURES_CLEANUP clear_perform)

  if (HDF5_USING_ANALYSIS_TOOL)
    add_test (NAME PERFORM_h5perf_serial COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:h5perf_serial>)
  else ()
    add_test (NAME PERFORM_h5perf_serial COMMAND "${CMAKE_COMMAND}"
        -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
        -D "TEST_PROGRAM=$<TARGET_FILE:h5perf_serial>"
        -D "TEST_ARGS:STRING="
        -D "TEST_EXPECT=0"
        -D "TEST_SKIP_COMPARE=TRUE"
        -D "TEST_OUTPUT=h5perf_serial.txt"
        #-D "TEST_REFERENCE=h5perf_serial.out"
        -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
        -P "${HDF_RESOURCES_DIR}/runTest.cmake"
    )
  endif ()
  set_tests_properties (PERFORM_h5perf_serial PROPERTIES
      TIMEOUT ${CTEST_VERY_LONG_TIMEOUT}
      FIXTURES_REQUIRED clear_perform
  )
  if ("PERFORM_h5perf_serial" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
    set_tests_properties (PERFORM_h5perf_serial PROPERTIES DISABLED true)
  endif ()

  if (HDF5_USING_ANALYSIS_TOOL)
    add_test (NAME PERFORM_chunk COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:chunk>)
  else ()
    add_test (NAME PERFORM_chunk COMMAND "${CMAKE_COMMAND}"
        -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
        -D "TEST_PROGRAM=$<TARGET_FILE:chunk>"
        -D "TEST_ARGS:STRING="
        -D "TEST_EXPECT=0"
        -D "TEST_SKIP_COMPARE=TRUE"
        -D "TEST_OUTPUT=chunk.txt"
        #-D "TEST_REFERENCE=chunk.out"
        -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
        -P "${HDF_RESOURCES_DIR}/runTest.cmake"
    )
  endif ()
  set_tests_properties (PERFORM_chunk PROPERTIES
      FIXTURES_REQUIRED clear_perform
  )
  if ("PERFORM_chunk" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
    set_tests_properties (PERFORM_chunk PROPERTIES DISABLED true)
  endif ()

  if (HDF5_USING_ANALYSIS_TOOL)
    add_test (NAME PERFORM_iopipe COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:iopipe>)
  else ()
    add_test (NAME PERFORM_iopipe COMMAND "${CMAKE_COMMAND}"
        -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
        -D "TEST_PROGRAM=$<TARGET_FILE:iopipe>"
        -D "TEST_ARGS:STRING="
        -D "TEST_EXPECT=0"
        -D "TEST_SKIP_COMPARE=TRUE"
        -D "TEST_OUTPUT=iopipe.txt"
        #-D "TEST_REFERENCE=iopipe.out"
        -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
        -P "${HDF_RESOURCES_DIR}/runTest.cmake"
    )
  endif ()
  set_tests_properties (PERFORM_iopipe PROPERTIES
      FIXTURES_REQUIRED clear_perform
  )
  if ("PERFORM_iopipe" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
    set_tests_properties (PERFORM_iopipe PROPERTIES DISABLED true)
  endif ()

  if (HDF5_USING_ANALYSIS_TOOL)
    add_test (NAME PERFORM_overhead COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:overhead>)
  else ()
    add_test (NAME PERFORM_overhead COMMAND "${CMAKE_COMMAND}"
        -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
        -D "TEST_PROGRAM=$<TARGET_FILE:overhead>"
        -D "TEST_ARGS:STRING="
        -D "TEST_EXPECT=0"
        -D "TEST_SKIP_COMPARE=TRUE"
        -D "TEST_OUTPUT=overhead.txt"
        #-D "TEST_REFERENCE=overhead.out"
        -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
        -P "${HDF_RESOURCES_DIR}/runTest.cmake"
    )
  endif ()
  set_tests_properties (PERFORM_overhead PROPERTIES
      FIXTURES_REQUIRED clear_perform
  )
  if ("PERFORM_overhead" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
    set_tests_properties (PERFORM_overhead PROPERTIES DISABLED true)
  endif ()

  if (HDF5_USING_ANALYSIS_TOOL)
    add_test (NAME PERFORM_perf_meta COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:perf_meta>)
  else ()
    add_test (NAME PERFORM_perf_meta COMMAND "${CMAKE_COMMAND}"
        -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
        -D "TEST_PROGRAM=$<TARGET_FILE:perf_meta>"
        -D "TEST_ARGS:STRING="
        -D "TEST_EXPECT=0"
        -D "TEST_SKIP_COMPARE=TRUE"
        -D "TEST_OUTPUT=perf_meta.txt"
        #-D "TEST_REFERENCE=perf_meta.out"
        -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
        -P "${HDF_RESOURCES_DIR}/runTest.cmake"
    )
  endif ()
  set_tests_properties (PERFORM_perf_meta PROPERTIES
      FIXTURES_REQUIRED clear_perform
  )
  if ("PERFORM_perf_meta" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
    set_tests_properties (PERFORM_perf_meta PROPERTIES DISABLED true)
  endif ()

  if (HDF5_USING_ANALYSIS_TOOL)
    add_test (NAME PERFORM_zip_perf_help COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:zip_perf> "-h")
  else ()
    add_test (NAME PERFORM_zip_perf_help COMMAND "${CMAKE_COMMAND}"
        -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
        -D "TEST_PROGRAM=$<TARGET_FILE:zip_perf>"
        -D "TEST_ARGS:STRING="
        -D "TEST_EXPECT=0"
        -D "TEST_SKIP_COMPARE=TRUE"
        -D "TEST_OUTPUT=zip_perf-h.txt"
        #-D "TEST_REFERENCE=zip_perf-h.out"
        -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
        -P "${HDF_RESOURCES_DIR}/runTest.cmake"
    )
  endif ()
  set_tests_properties (PERFORM_zip_perf_help PROPERTIES
      FIXTURES_REQUIRED clear_perform
  )
  if ("PERFORM_zip_perf_help" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
    set_tests_properties (PERFORM_zip_perf_help PROPERTIES DISABLED true)
  endif ()

  if (HDF5_USING_ANALYSIS_TOOL)
    add_test (NAME PERFORM_zip_perf COMMAND ${CMAKE_CROSSCOMPILING_EMULATOR} $<TARGET_FILE:zip_perf> tfilters.h5)
  else ()
    add_test (NAME PERFORM_zip_perf COMMAND "${CMAKE_COMMAND}"
        -D "TEST_EMULATOR=${CMAKE_CROSSCOMPILING_EMULATOR}"
        -D "TEST_PROGRAM=$<TARGET_FILE:zip_perf>"
        -D "TEST_ARGS:STRING="
        -D "TEST_EXPECT=0"
        -D "TEST_SKIP_COMPARE=TRUE"
        -D "TEST_OUTPUT=zip_perf.txt"
        #-D "TEST_REFERENCE=zip_perf.out"
        -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
        -P "${HDF_RESOURCES_DIR}/runTest.cmake"
    )
  endif ()
  set_tests_properties (PERFORM_zip_perf PROPERTIES
      DEPENDS "PERFORM_zip_perf_help"
      FIXTURES_REQUIRED clear_perform
  )
  if ("PERFORM_zip_perf" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
    set_tests_properties (PERFORM_zip_perf PROPERTIES DISABLED true)
  endif ()
endif ()

if (H5_HAVE_PARALLEL AND HDF5_TEST_PARALLEL)
  add_test (NAME MPI_TEST_PERFORM_h5perf COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_PREFLAGS} $<TARGET_FILE:h5perf> ${MPIEXEC_POSTFLAGS})

endif ()
