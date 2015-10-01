
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
set_tests_properties (FORTRAN_testhdf5_fortran_1_8 PROPERTIES DEPENDS FORTRAN_testhdf5_fortran)

#-- Adding test for fortranlib_test_F03
if (HDF5_ENABLE_F2003)
  add_test (NAME FORTRAN_fortranlib_test_F03 COMMAND $<TARGET_FILE:fortranlib_test_F03>)
  set_tests_properties (FORTRAN_fortranlib_test_F03 PROPERTIES PASS_REGULAR_EXPRESSION "[ ]*0 error.s")
  set_tests_properties (FORTRAN_fortranlib_test_F03 PROPERTIES DEPENDS FORTRAN_testhdf5_fortran_1_8)
endif (HDF5_ENABLE_F2003)

#-- Adding test for fflush1
add_test (NAME FORTRAN_fflush1 COMMAND $<TARGET_FILE:fflush1>)

#-- Adding test for fflush2
add_test (NAME FORTRAN_fflush2 COMMAND $<TARGET_FILE:fflush2>)
set_tests_properties (FORTRAN_fflush2 PROPERTIES DEPENDS FORTRAN_fflush1)

if (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
  add_test (NAME FORTRAN_testhdf5_fortran-shared COMMAND $<TARGET_FILE:testhdf5_fortran-shared>)
  set_tests_properties (FORTRAN_testhdf5_fortran-shared PROPERTIES PASS_REGULAR_EXPRESSION "[ ]*0 error.s")
  set_tests_properties (FORTRAN_testhdf5_fortran-shared PROPERTIES DEPENDS FORTRAN_testhdf5_fortran)

  #-- Adding test for testhdf5_fortran_1_8
  add_test (NAME FORTRAN_testhdf5_fortran_1_8-shared COMMAND $<TARGET_FILE:testhdf5_fortran_1_8-shared>)
  set_tests_properties (FORTRAN_testhdf5_fortran_1_8-shared PROPERTIES PASS_REGULAR_EXPRESSION "[ ]*0 error.s")
  set_tests_properties (FORTRAN_testhdf5_fortran_1_8-shared PROPERTIES DEPENDS FORTRAN_testhdf5_fortran_1_8)

  #-- Adding test for fortranlib_test_F03
  if (HDF5_ENABLE_F2003)
    add_test (NAME FORTRAN_fortranlib_test_F03-shared COMMAND $<TARGET_FILE:fortranlib_test_F03-shared>)
    set_tests_properties (FORTRAN_fortranlib_test_F03-shared PROPERTIES PASS_REGULAR_EXPRESSION "[ ]*0 error.s")
    set_tests_properties (FORTRAN_fortranlib_test_F03-shared PROPERTIES DEPENDS FORTRAN_fortranlib_test_F03)
  endif (HDF5_ENABLE_F2003)

  #-- Adding test for fflush1
  add_test (NAME FORTRAN_fflush1-shared COMMAND $<TARGET_FILE:fflush1-shared>)
  set_tests_properties (FORTRAN_fflush1-shared PROPERTIES DEPENDS FORTRAN_fflush2)

  #-- Adding test for fflush2
  add_test (NAME FORTRAN_fflush2-shared COMMAND $<TARGET_FILE:fflush2-shared>)
  set_tests_properties (FORTRAN_fflush2-shared PROPERTIES DEPENDS FORTRAN_fflush1-shared)
endif (BUILD_SHARED_LIBS AND NOT SKIP_HDF5_FORTRAN_SHARED)
