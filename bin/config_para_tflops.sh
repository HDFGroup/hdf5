#! /bin/sh
#
# Copyright by the Board of Trustees of the University of Illinois.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the files COPYING and Copyright.html.  COPYING can be found at the root
# of the source code distribution tree; Copyright.html can be found at the
# root level of an installed copy of the electronic HDF5 document set and
# is linked from the top-level documents page.  It can also be found at
# http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have
# access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu.
#

# How to configure a parallel version of HDF5 on the Sandia National Laboratory
# TFLOPS System that uses MPI and MPI-IO.

# Read the INSTALL_TFLOPS file to understand the configure/make process
# for the sequential (i.e., uniprocessor) version of HDF5.
# The process for creating the parallel version of HDF5 using MPI-IO
# is similar, but first you will have to set up some environment variables
# with values specific to your local installation.
# The relevant variables are shown below, with values that work for Sandia'a
# ASCI Red Tflops machine as of the writing of these instructions (980421).

# Don't try to make a parallel version of HDF5 from the same hdf5 root
# directory where you made a sequential version of HDF5 -- start with
# a fresh copy.
# Here are the flags you must set before running the ./configure program
# to create the parallel version of HDF5.
# (We use sh here, but of course you can adapt to whatever shell you like.)

# compile for MPI jobs
#CC=cicc

# The following flags are only needed when compiling/linking a user program
# for execution.
#

# Using the MPICH libary by Daniel Sands.
# It contains both MPI-1 and MPI-IO functions.
ROMIO="${HOME}/MPIO/mpich"
if [ ! -d $ROMIO ]
then
    echo "ROMIO directory ($ROMIO) not found"
    echo "Aborted"
    exit 1
fi
mpi1_inc=""
mpi1_lib=""
mpio_inc="-I$ROMIO/include"
mpio_lib="-L$ROMIO/lib"


MPI_INC="$mpi1_inc $mpio_inc"
MPI_LIB="$mpi1_lib $mpio_lib"


# Once these variables are set to the proper values for your installation,
# you can run the configure program (i.e., ./configure tflop --enable-parallel=mpio)
# to set up the Makefiles, etc.
# After configuring, run the make as described in the INSTALL file.

# When compiling and linking your application, don't forget to compile with
# cicc and link to the MPI-IO library and the parallel version of the HDF5
# library (that was created and installed with the configure/make process).

CPPFLAGS=$MPI_INC \
LDFLAGS=$MPI_LIB \
LIBS="-lmpich" \
./configure --enable-parallel tflop $@
