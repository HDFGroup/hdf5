
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

HDFTEST_COPY_FILE("${HDF5_TOOLS_DIR}/testfiles/tfilters.h5" "${PROJECT_BINARY_DIR}/tfilters.h5" "zip_perf_files")
add_custom_target(zip_perf_files ALL COMMENT "Copying files needed by zip_perf tests" DEPENDS ${zip_perf_list})

#-----------------------------------------------------------------------------
# Add Tests
#-----------------------------------------------------------------------------

# Remove any output file left over from previous test run
add_test (
    NAME PERFORM_h5perform-clear-objects
    COMMAND    ${CMAKE_COMMAND}
        -E remove
        chunk.h5
        iopipe.h5
        iopipe.raw
        x-diag-rd.dat
        x-diag-wr.dat
        x-rowmaj-rd.dat
        x-rowmaj-wr.dat
        x-gnuplot
)

if (HDF5_ENABLE_USING_MEMCHECKER)
  add_test (NAME PERFORM_h5perf_serial COMMAND $<TARGET_FILE:h5perf_serial>)
else ()
  add_test (NAME PERFORM_h5perf_serial COMMAND "${CMAKE_COMMAND}"
      -D "TEST_PROGRAM=$<TARGET_FILE:h5perf_serial>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_SKIP_COMPARE=TRUE"
      -D "TEST_OUTPUT=h5perf_serial.txt"
      #-D "TEST_REFERENCE=h5perf_serial.out"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
      -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
  )
endif ()
set_tests_properties (PERFORM_h5perf_serial PROPERTIES TIMEOUT 1800)

if (HDF5_BUILD_PERFORM_STANDALONE)
  add_test (NAME PERFORM_h5perf_serial_alone COMMAND $<TARGET_FILE:h5perf_serial_alone>)
endif ()

if (HDF5_ENABLE_USING_MEMCHECKER)
  add_test (NAME PERFORM_chunk COMMAND $<TARGET_FILE:chunk>)
else ()
  add_test (NAME PERFORM_chunk COMMAND "${CMAKE_COMMAND}"
      -D "TEST_PROGRAM=$<TARGET_FILE:chunk>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_SKIP_COMPARE=TRUE"
      -D "TEST_OUTPUT=chunk.txt"
      #-D "TEST_REFERENCE=chunk.out"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
      -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
  )
endif ()

if (HDF5_ENABLE_USING_MEMCHECKER)
  add_test (NAME PERFORM_iopipe COMMAND $<TARGET_FILE:iopipe>)
else ()
  add_test (NAME PERFORM_iopipe COMMAND "${CMAKE_COMMAND}"
      -D "TEST_PROGRAM=$<TARGET_FILE:iopipe>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_SKIP_COMPARE=TRUE"
      -D "TEST_OUTPUT=iopipe.txt"
      #-D "TEST_REFERENCE=iopipe.out"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
      -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
  )
endif ()

if (HDF5_ENABLE_USING_MEMCHECKER)
  add_test (NAME PERFORM_overhead COMMAND $<TARGET_FILE:overhead>)
else ()
  add_test (NAME PERFORM_overhead COMMAND "${CMAKE_COMMAND}"
      -D "TEST_PROGRAM=$<TARGET_FILE:overhead>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_SKIP_COMPARE=TRUE"
      -D "TEST_OUTPUT=overhead.txt"
      #-D "TEST_REFERENCE=overhead.out"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
      -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
  )
endif ()

if (HDF5_ENABLE_USING_MEMCHECKER)
  add_test (NAME PERFORM_perf_meta COMMAND $<TARGET_FILE:perf_meta>)
else ()
  add_test (NAME PERFORM_perf_meta COMMAND "${CMAKE_COMMAND}"
      -D "TEST_PROGRAM=$<TARGET_FILE:perf_meta>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_SKIP_COMPARE=TRUE"
      -D "TEST_OUTPUT=perf_meta.txt"
      #-D "TEST_REFERENCE=perf_meta.out"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
      -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
  )
endif ()

if (HDF5_ENABLE_USING_MEMCHECKER)
  add_test (NAME PERFORM_zip_perf_help COMMAND $<TARGET_FILE:zip_perf> "-h")
else ()
  add_test (NAME PERFORM_zip_perf_help COMMAND "${CMAKE_COMMAND}"
      -D "TEST_PROGRAM=$<TARGET_FILE:zip_perf>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_SKIP_COMPARE=TRUE"
      -D "TEST_OUTPUT=zip_perf-h.txt"
      #-D "TEST_REFERENCE=zip_perf-h.out"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
      -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
  )
endif ()
if (HDF5_ENABLE_USING_MEMCHECKER)
  add_test (NAME PERFORM_zip_perf COMMAND $<TARGET_FILE:zip_perf> tfilters.h5)
else ()
  add_test (NAME PERFORM_zip_perf COMMAND "${CMAKE_COMMAND}"
      -D "TEST_PROGRAM=$<TARGET_FILE:zip_perf>"
      -D "TEST_ARGS:STRING="
      -D "TEST_EXPECT=0"
      -D "TEST_SKIP_COMPARE=TRUE"
      -D "TEST_OUTPUT=zip_perf.txt"
      #-D "TEST_REFERENCE=zip_perf.out"
      -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
      -P "${HDF_RESOURCES_EXT_DIR}/runTest.cmake"
  )
endif ()

if (H5_HAVE_PARALLEL)
  add_test (NAME PERFORM_h5perf COMMAND ${MPIEXEC} ${MPIEXEC_PREFLAGS} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_POSTFLAGS} $<TARGET_FILE:h5perf>)

  if (HDF5_BUILD_PERFORM_STANDALONE)
    add_test (NAME PERFORM_h5perf_alone COMMAND ${MPIEXEC} ${MPIEXEC_PREFLAGS} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_POSTFLAGS} $<TARGET_FILE:h5perf_alone>)
  endif ()
endif ()
