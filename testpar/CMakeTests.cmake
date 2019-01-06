#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#

##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################

add_test (NAME TEST_PAR_testphdf5 COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_PREFLAGS} $<TARGET_FILE:testphdf5> ${MPIEXEC_POSTFLAGS})

foreach (testp ${H5P_TESTS})
  add_test (NAME TEST_PAR_${testp} COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_PREFLAGS} $<TARGET_FILE:${testp}> ${MPIEXEC_POSTFLAGS})
endforeach ()

# The t_pflush1 test is hard-coded to fail.
set_tests_properties (TEST_PAR_t_pflush1 PROPERTIES WILL_FAIL "true")
#set_property (TEST TEST_PAR_t_pflush1 PROPERTY PASS_REGULAR_EXPRESSION "PASSED")
set_tests_properties (TEST_PAR_t_pflush2 PROPERTIES DEPENDS TEST_PAR_t_pflush1)

##############################################################################
##############################################################################
###                         V F D   T E S T S                              ###
##############################################################################
##############################################################################

if (HDF5_TEST_VFD)
  include (CMakeVFDTests.cmake)
endif ()
