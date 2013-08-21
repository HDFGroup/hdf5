
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

ADD_TEST (NAME TEST_PAR_testphdf5 COMMAND ${MPIEXEC} ${MPIEXEC_PREFLAGS} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_POSTFLAGS} $<TARGET_FILE:testphdf5>)

FOREACH (testp ${H5P_TESTS})
  ADD_TEST (NAME TEST_PAR_${testp} COMMAND ${MPIEXEC} ${MPIEXEC_PREFLAGS} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_POSTFLAGS} $<TARGET_FILE:${testp}>)
ENDFOREACH (testp ${H5P_TESTS})

SET_TESTS_PROPERTIES(TEST_PAR_t_pflush2 PROPERTIES DEPENDS TEST_PAR_t_pflush1)

IF (NOT WIN32)
  ADD_TEST (NAME TEST_PAR_t_posix_compliant COMMAND ${MPIEXEC} ${MPIEXEC_PREFLAGS} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_POSTFLAGS} $<TARGET_FILE:t_posix_compliant>)
ENDIF (NOT WIN32)
  
IF (HDF5_TEST_VFD)

  SET (VFD_LIST
      sec2
      stdio
      core
      split
      multi
      family
  )

  SET (H5P_VFD_TESTS
      t_pflush1
      t_pflush2
  )
  
  IF (DIRECT_VFD)
    SET (VFD_LIST ${VFD_LIST} direct)
  ENDIF (DIRECT_VFD)

  MACRO (ADD_VFD_TEST vfdname resultcode)
    IF (NOT HDF5_ENABLE_USING_MEMCHECKER)
      FOREACH (test ${H5P_VFD_TESTS})
        ADD_TEST (
          NAME VFD-${vfdname}-${test} 
          COMMAND "${CMAKE_COMMAND}"
              -D "TEST_PROGRAM=$<TARGET_FILE:${test}>"
              -D "TEST_ARGS:STRING="
              -D "TEST_VFD:STRING=${vfdname}"
              -D "TEST_EXPECT=${resultcode}"
              -D "TEST_OUTPUT=${test}"
              -D "TEST_FOLDER=${PROJECT_BINARY_DIR}"
              -P "${HDF5_RESOURCES_DIR}/vfdTest.cmake"
        )
      ENDFOREACH (test ${H5P_VFD_TESTS})
    ENDIF (NOT HDF5_ENABLE_USING_MEMCHECKER)
  ENDMACRO (ADD_VFD_TEST)
  
  # Run test with different Virtual File Driver
  FOREACH (vfd ${VFD_LIST})
    ADD_VFD_TEST (${vfd} 0)
  ENDFOREACH (vfd ${VFD_LIST})

ENDIF (HDF5_TEST_VFD)
