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
##
## Makefile.am
## Run automake to generate a Makefile.in from this file.
##
#
# Tools HDF5 Makefile(.in)
#

include $(top_srcdir)/config/commence.am

if PARALLEL_TOOLS_CONDITIONAL
  H5DWALK=h5dwalk
else
  H5DWALK=
endif

if BUILD_TESTS_CONDITIONAL
   TESTSERIAL_DIR =test
else
   TESTSERIAL_DIR=
endif

CONFIG=ordered

# All subdirectories
SUBDIRS=$(H5DWALK) $(TESTSERIAL_DIR)

include $(top_srcdir)/config/conclude.am
