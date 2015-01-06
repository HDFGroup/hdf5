
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

add_custom_command (
    TARGET     zip_perf
    POST_BUILD
    COMMAND    ${CMAKE_COMMAND}
    ARGS       -E copy_if_different ${HDF5_TOOLS_SRC_DIR}/testfiles/tfilters.h5 ${PROJECT_BINARY_DIR}/tfilters.h5
)

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

add_test (NAME PERFORM_h5perf_serial COMMAND $<TARGET_FILE:h5perf_serial>)
set_tests_properties (PERFORM_h5perf_serial PROPERTIES TIMEOUT 1800)

if (HDF5_BUILD_PERFORM_STANDALONE)
  add_test (NAME PERFORM_h5perf_serial_alone COMMAND $<TARGET_FILE:h5perf_serial_alone>)
endif (HDF5_BUILD_PERFORM_STANDALONE)

add_test (NAME PERFORM_chunk COMMAND $<TARGET_FILE:chunk>)

add_test (NAME PERFORM_iopipe COMMAND $<TARGET_FILE:iopipe>)

add_test (NAME PERFORM_overhead COMMAND $<TARGET_FILE:overhead>)

add_test (NAME PERFORM_perf_meta COMMAND $<TARGET_FILE:perf_meta>)

add_test (NAME PERFORM_zip_perf_help COMMAND $<TARGET_FILE:zip_perf> "-h")
add_test (NAME PERFORM_zip_perf COMMAND $<TARGET_FILE:zip_perf> tfilters.h5)

if (H5_HAVE_PARALLEL)
  add_test (NAME PERFORM_h5perf COMMAND ${MPIEXEC} ${MPIEXEC_PREFLAGS} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_POSTFLAGS} $<TARGET_FILE:h5perf>)

  if (HDF5_BUILD_PERFORM_STANDALONE)
    add_test (NAME PERFORM_h5perf_alone COMMAND ${MPIEXEC} ${MPIEXEC_PREFLAGS} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_POSTFLAGS} $<TARGET_FILE:h5perf_alone>)
  endif (HDF5_BUILD_PERFORM_STANDALONE)
endif (H5_HAVE_PARALLEL)
