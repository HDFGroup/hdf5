
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

ADD_TEST (NAME testhdf5_fortran COMMAND $<TARGET_FILE:testhdf5_fortran>)
SET_TESTS_PROPERTIES(testhdf5_fortran PROPERTIES PASS_REGULAR_EXPRESSION "[ ]*0 error.s")

#-- Adding test for testhdf5_fortran_1_8
ADD_TEST (NAME testhdf5_fortran_1_8 COMMAND $<TARGET_FILE:testhdf5_fortran_1_8>)
SET_TESTS_PROPERTIES(testhdf5_fortran_1_8 PROPERTIES PASS_REGULAR_EXPRESSION "[ ]*0 error.s")

#-- Adding test for fortranlib_test_F03
IF (HDF5_ENABLE_F2003)
  ADD_TEST (NAME fortranlib_test_F03 COMMAND $<TARGET_FILE:fortranlib_test_F03>)
  SET_TESTS_PROPERTIES(fortranlib_test_F03 PROPERTIES PASS_REGULAR_EXPRESSION "[ ]*0 error.s")
ENDIF (HDF5_ENABLE_F2003)

#-- Adding test for fflush1
ADD_TEST (NAME fflush1 COMMAND $<TARGET_FILE:fflush1>)

#-- Adding test for fflush2
ADD_TEST (NAME fflush2 COMMAND $<TARGET_FILE:fflush2>)
SET_TESTS_PROPERTIES(fflush2 PROPERTIES DEPENDS fflush1)
