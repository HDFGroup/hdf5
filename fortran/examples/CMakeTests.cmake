
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

  # Remove any output file left over from previous test run
  add_test (
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
  if (NOT "${last_test}" STREQUAL "")
    set_tests_properties (f90_ex-clear-objects PROPERTIES DEPENDS ${last_test})
  endif (NOT "${last_test}" STREQUAL "")
  set (last_test "f90_ex-clear-objects")

foreach (example ${examples})
  add_test (NAME f90_ex_${example} COMMAND $<TARGET_FILE:f90_ex_${example}>)
  if (NOT "${last_test}" STREQUAL "")
    set_tests_properties (f90_ex_${example} PROPERTIES DEPENDS ${last_test})
  endif (NOT "${last_test}" STREQUAL "")
  set (last_test "f90_ex_${example}")
endforeach (example ${examples})

if (HDF5_ENABLE_F2003)
  foreach (example ${F2003_examples})
    add_test (NAME f03_ex_${example} COMMAND $<TARGET_FILE:f03_ex_${example}>)
    if (NOT "${last_test}" STREQUAL "")
      set_tests_properties (f03_ex_${example} PROPERTIES DEPENDS ${last_test})
    endif (NOT "${last_test}" STREQUAL "")
    set (last_test "f03_ex_${example}")
  endforeach (example ${F2003_examples})
endif (HDF5_ENABLE_F2003)

if (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)
  add_test (NAME f90_ex_ph5example COMMAND ${MPIEXEC} ${MPIEXEC_PREFLAGS} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_POSTFLAGS} $<TARGET_FILE:f90_ex_ph5example>)
endif (H5_HAVE_PARALLEL AND MPI_Fortran_FOUND)
