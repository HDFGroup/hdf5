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
# Utils HDF5 Makefile(.in)
#

include $(top_srcdir)/config/commence.am

CONFIG=ordered

if BUILD_TESTS_CONDITIONAL
   TESTUTIL_DIR =test
else
   TESTUTIL_DIR=
endif

if MIRROR_VFD_CONDITIONAL
   MIRROR_VFD_DIR = mirror_vfd
else
   MIRROR_VFD_DIR=
endif

if BUILD_TOOLS_CONDITIONAL
   TOOLS_DIR =tools
else
   TOOLS_DIR=
endif

# All subdirectories
SUBDIRS=$(MIRROR_VFD_DIR) $(TESTUTIL_DIR) $(TOOLS_DIR)

bin_SCRIPTS = subfiling_vfd/h5fuse.sh

include $(top_srcdir)/config/conclude.am
