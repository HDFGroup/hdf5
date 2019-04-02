#! /bin/sh
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
# This source code was developed under the Mochi project
# (https://www.mcs.anl.gov/research/projects/mochi), supported by the U.S.
# Department of Energy, Office of Science, under contract DE-AC02-06CH11357.

# Build mobject examples.
# Assumes mobject stuff can be found
echo "Building mobject examples"
../../../bin/h5pcc -DHDF5_USE_MOBJECT -g -o h5rados_file_create h5rados_file_create.c
../../../bin/h5pcc -DHDF5_USE_MOBJECT -g -o h5rados_file_open h5rados_file_open.c
../../../bin/h5pcc -DHDF5_USE_MOBJECT -g -o h5rados_dset_create h5rados_dset_create.c
../../../bin/h5pcc -DHDF5_USE_MOBJECT -g -o h5rados_dset_open h5rados_dset_open.c
../../../bin/h5pcc -DHDF5_USE_MOBJECT -g -o h5rados_group_create h5rados_group_create.c
../../../bin/h5pcc -DHDF5_USE_MOBJECT -g -o h5rados_group_open h5rados_group_open.c
../../../bin/h5pcc -DHDF5_USE_MOBJECT -g -o h5rados_dset_write h5rados_dset_write.c
../../../bin/h5pcc -DHDF5_USE_MOBJECT -g -o h5rados_dset_read h5rados_dset_read.c
../../../bin/h5pcc -DHDF5_USE_MOBJECT -g -o h5rados_dset_rpartial h5rados_dset_rpartial.c
../../../bin/h5pcc -DHDF5_USE_MOBJECT -g -o h5rados_dset_wpartial h5rados_dset_wpartial.c
echo "DONE"
echo

# Run the mobject examples
echo "Running mobject examples"
# File create/open
echo "./h5rados_file_create testfile"
mpiexec -n 2 ./h5rados_file_create testfile
echo "./h5rados_file_open testfile"
mpiexec -n 2 ./h5rados_file_open testfile
# Dataset create/open
echo "./h5rados_dset_create testfile testdset"
mpiexec -n 2 ./h5rados_dset_create testfile testdset
echo "./h5rados_dset_open testfile testdset"
mpiexec -n 2 ./h5rados_dset_open testfile testdset
# Group create/open
echo "./h5rados_group_create testfile testgroup"
mpiexec -n 2 ./h5rados_group_create testfile testgroup
echo "./h5rados_group_open testfile testgroup"
mpiexec -n 2 ./h5rados_group_open testfile testgroup
# Dataset read/write
echo "./h5rados_dset_write testfile testdset"
mpiexec -n 2 ./h5rados_dset_write testfile testdset
echo "./h5rados_dset_read testfile testdset"
mpiexec -n 2 ./h5rados_dset_read testfile testdset
# Dataset read/write (partial)
echo "./h5rados_dset_wpartial testfile testdset"
mpiexec -n 2 ./h5rados_dset_wpartial testfile testdset
echo "./h5rados_dset_rpartial testfile testdset"
mpiexec -n 2 ./h5rados_dset_rpartial testfile testdset
echo

