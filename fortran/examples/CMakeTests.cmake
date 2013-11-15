
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

  # Remove any output file left over from previous test run
  ADD_TEST (
      NAME f90_ex-clear-objects
      COMMAND    ${CMAKE_COMMAND}
          -E remove 
          compound.h5
          copy1.h5
          copy2.h5
          dsetf.h5
          extend.h5 
          FORTRAN.h5
          groupf.h5
          groupsf.h5
          h5_cmprss.h5
          mount1.h5
          mount2.h5
          sdsf.h5
          subset.h5
  )
  IF (NOT "${last_test}" STREQUAL "")
    SET_TESTS_PROPERTIES (f90_ex-clear-objects PROPERTIES DEPENDS ${last_test})
  ENDIF (NOT "${last_test}" STREQUAL "")
  SET (last_test "f90_ex-clear-objects")

FOREACH (example ${examples})
  ADD_TEST (NAME f90_ex_${example} COMMAND $<TARGET_FILE:f90_ex_${example}>)
  IF (NOT "${last_test}" STREQUAL "")
    SET_TESTS_PROPERTIES (f90_ex_${example} PROPERTIES DEPENDS ${last_test})
  ENDIF (NOT "${last_test}" STREQUAL "")
  SET (last_test "f90_ex_${example}")
ENDFOREACH (example ${examples})

IF (HDF5_ENABLE_F2003)
  FOREACH (example ${F2003_examples})
    ADD_TEST (NAME f03_ex_${example} COMMAND $<TARGET_FILE:f03_ex_${example}>)
    IF (NOT "${last_test}" STREQUAL "")
      SET_TESTS_PROPERTIES (f03_ex_${example} PROPERTIES DEPENDS ${last_test})
    ENDIF (NOT "${last_test}" STREQUAL "")
    SET (last_test "f03_ex_${example}")
  ENDFOREACH (example ${F2003_examples})
ENDIF (HDF5_ENABLE_F2003)

IF (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)
  ADD_TEST (NAME f90_ex_ph5example COMMAND ${MPIEXEC} ${MPIEXEC_PREFLAGS} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_POSTFLAGS} $<TARGET_FILE:f90_ex_ph5example>)
ENDIF (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)
