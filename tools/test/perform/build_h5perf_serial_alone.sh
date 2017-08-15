#! /bin/sh -x
#
# Copyright by The HDF Group.
# Copyright by the Board of Trustees of the University of Illinois.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#
# Name: build_h5perf_serial_alone.sh
#
# Puporse: Build the h5perf_serial tool by standalone mode.
#
# Created: Christian Chilan, 2008/09/02 
#
# Modification:
#

# Default to use h5cc to build h5perf_serial unless $H5CC is defined.
# 
h5cc=${H5CC:-h5cc}
$h5cc -DSTANDALONE sio_perf.c sio_engine.c sio_timer.c -o h5perf_serial
