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

# Build RADOS examples.
# Assumes RADOS can be found in the usual location.
echo "Building RADOS examples"
../../../bin/h5pcc -g -o h5rados_file_create h5rados_file_create.c -lrados
../../../bin/h5pcc -g -o h5rados_file_open h5rados_file_open.c -lrados
../../../bin/h5pcc -g -o h5rados_dset_create h5rados_dset_create.c -lrados
../../../bin/h5pcc -g -o h5rados_dset_open h5rados_dset_open.c -lrados
../../../bin/h5pcc -g -o h5rados_group_create h5rados_group_create.c -lrados
../../../bin/h5pcc -g -o h5rados_group_open h5rados_group_open.c -lrados
../../../bin/h5pcc -g -o h5rados_dset_write h5rados_dset_write.c -lrados
../../../bin/h5pcc -g -o h5rados_dset_read h5rados_dset_read.c -lrados
../../../bin/h5pcc -g -o h5rados_dset_rpartial h5rados_dset_rpartial.c -lrados
../../../bin/h5pcc -g -o h5rados_dset_wpartial h5rados_dset_wpartial.c -lrados
echo "DONE"
echo

# Create the pool
echo "Creating the test pool"
echo "(Ceph makes it hard to delete pools, so we never do that"
echo "and you may see a message that the pool already exists.)"
ceph osd pool create mypool 128
echo

# Dump some Ceph info
echo "Pool status at start of tests:"
rados -p mypool ls
echo

# Clean out the test pool
echo "Cleaning out the test pool"
for i in $(rados -p mypool ls); do echo $i; rados -p mypool rm $i; done
echo

# Run the RADOS examples
echo "Running RADOS examples"
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


# Dump the Ceph info again
echo "Pool status at end of tests:"
rados -p mypool ls
echo

