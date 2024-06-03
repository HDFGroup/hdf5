#
# Copyright by The HDF Group.
# Copyright by the Board of Trustees of the University of Illinois.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
##
## Makefile.am
## Run automake to generate a Makefile.in from this file.
##
noinst_PROGRAMS = h5ex_g_compact h5ex_g_create  \
  h5ex_g_corder  h5ex_g_phase

h5ex_g_compact_SOURCES       = h5ex_g_compact.F90
h5ex_g_create_SOURCES        = h5ex_g_create.F90
h5ex_g_corder_SOURCES        = h5ex_g_corder.F90
h5ex_g_phase_SOURCES         = h5ex_g_phase.F90

EXTRA_DIST = tfiles/h5ex_g_create.ddl \
tfiles/h5ex_g_compact1.ddl tfiles/h5ex_g_compact2.ddl tfiles/h5ex_g_corder.tst \
tfiles/h5ex_g_phase.tst \
test.sh

TESTS = test.sh

CLEANFILES = Makefile test.sh
