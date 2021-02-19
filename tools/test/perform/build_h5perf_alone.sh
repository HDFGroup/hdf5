#! /bin/sh -x
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
#
# Name: build_h5perf_alone.sh
#
# Puporse: Build the h5perf tool by standalone mode.
#
# Created: Albert Cheng, 2005/09/18
#
# Modification:
#

# Default to use h5pcc to build h5perf unless $H5CC is defined.
# 
h5pcc=${H5CC:-h5pcc}
$h5pcc -DSTANDALONE pio_perf.c pio_engine.c pio_timer.c -o h5perf
