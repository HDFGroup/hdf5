
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

ADD_CUSTOM_COMMAND (
    TARGET     zip_perf
    POST_BUILD
    COMMAND    ${CMAKE_COMMAND}
    ARGS       -E copy_if_different ${HDF5_TOOLS_SRC_DIR}/testfiles/tfilters.h5 ${PROJECT_BINARY_DIR}/tfilters.h5
)

#-----------------------------------------------------------------------------
# Add Tests
#-----------------------------------------------------------------------------

# Remove any output file left over from previous test run
ADD_TEST (
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

ADD_TEST (NAME PERFORM_h5perf_serial COMMAND $<TARGET_FILE:h5perf_serial>)

IF (HDF5_BUILD_PERFORM_STANDALONE)
  ADD_TEST (NAME PERFORM_h5perf_serial_alone COMMAND $<TARGET_FILE:h5perf_serial_alone>)
ENDIF (HDF5_BUILD_PERFORM_STANDALONE)

ADD_TEST (NAME PERFORM_chunk COMMAND $<TARGET_FILE:chunk>)

ADD_TEST (NAME PERFORM_iopipe COMMAND $<TARGET_FILE:iopipe>)

ADD_TEST (NAME PERFORM_overhead COMMAND $<TARGET_FILE:overhead>)

ADD_TEST (NAME PERFORM_perf_meta COMMAND $<TARGET_FILE:perf_meta>)

ADD_TEST (NAME PERFORM_zip_perf_help COMMAND $<TARGET_FILE:zip_perf> "-h")
ADD_TEST (NAME PERFORM_zip_perf COMMAND $<TARGET_FILE:zip_perf> tfilters.h5)

IF (H5_HAVE_PARALLEL)
  ADD_TEST (NAME PERFORM_h5perf COMMAND ${MPIEXEC} ${MPIEXEC_PREFLAGS} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_POSTFLAGS} $<TARGET_FILE:h5perf>)

  IF (HDF5_BUILD_PERFORM_STANDALONE)
    ADD_TEST (NAME PERFORM_h5perf_alone COMMAND ${MPIEXEC} ${MPIEXEC_PREFLAGS} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_POSTFLAGS} $<TARGET_FILE:h5perf_alone>)
  ENDIF (HDF5_BUILD_PERFORM_STANDALONE)

  IF (HDF5_BUILD_PARALLEL_ALL)
    ADD_TEST (NAME PERFORM_benchpar COMMAND ${MPIEXEC} ${MPIEXEC_PREFLAGS} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_POSTFLAGS} $<TARGET_FILE:benchpar>)
  ENDIF (HDF5_BUILD_PARALLEL_ALL)
ENDIF (H5_HAVE_PARALLEL)
