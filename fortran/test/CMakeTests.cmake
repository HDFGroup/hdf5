
##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

add_test (NAME FORTRAN_testhdf5_fortran COMMAND $<TARGET_FILE:testhdf5_fortran>)
set_tests_properties (FORTRAN_testhdf5_fortran PROPERTIES PASS_REGULAR_EXPRESSION "[ ]*0 error.s")

#-- Adding test for testhdf5_fortran_1_8
add_test (NAME FORTRAN_testhdf5_fortran_1_8 COMMAND $<TARGET_FILE:testhdf5_fortran_1_8>)
set_tests_properties (FORTRAN_testhdf5_fortran_1_8 PROPERTIES PASS_REGULAR_EXPRESSION "[ ]*0 error.s")

#-- Adding test for fortranlib_test_F03
if (HDF5_ENABLE_F2003)
  add_test (NAME FORTRAN_fortranlib_test_F03 COMMAND $<TARGET_FILE:fortranlib_test_F03>)
  set_tests_properties (FORTRAN_fortranlib_test_F03 PROPERTIES PASS_REGULAR_EXPRESSION "[ ]*0 error.s")
endif (HDF5_ENABLE_F2003)

#-- Adding test for fflush1
add_test (NAME FORTRAN_fflush1 COMMAND $<TARGET_FILE:fflush1>)

#-- Adding test for fflush2
add_test (NAME FORTRAN_fflush2 COMMAND $<TARGET_FILE:fflush2>)
set_tests_properties (FORTRAN_fflush2 PROPERTIES DEPENDS FORTRAN_fflush1)
