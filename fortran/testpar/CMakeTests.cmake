#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#

##############################################################################
##############################################################################
###           T E S T I N G                                                ###
##############################################################################
##############################################################################
add_test (NAME MPI_TEST_FORT_parallel_test COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_PREFLAGS} $<TARGET_FILE:parallel_test> ${MPIEXEC_POSTFLAGS})
if ("MPI_TEST_FORT_parallel_test" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
  set_tests_properties (MPI_TEST_FORT_parallel_test PROPERTIES DISABLED true)
endif ()

add_test (NAME MPI_TEST_FORT_subfiling_test COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_PREFLAGS} $<TARGET_FILE:subfiling_test> ${MPIEXEC_POSTFLAGS})
if ("MPI_TEST_FORT_subfiling_test" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
  set_tests_properties (MPI_TEST_FORT_subfiling_test PROPERTIES DISABLED true)
endif ()

add_test (NAME MPI_TEST_FORT_async_test COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS} ${MPIEXEC_PREFLAGS} $<TARGET_FILE:async_test> ${MPIEXEC_POSTFLAGS})
if ("MPI_TEST_FORT_async_test" MATCHES "${HDF5_DISABLE_TESTS_REGEX}")
  set_tests_properties (MPI_TEST_FORT_async_test PROPERTIES DISABLED true)
endif ()
