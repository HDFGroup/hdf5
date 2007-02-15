# Copyright by The HDF Group.
# Copyright by the Board of Trustees of the University of Illinois.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the files COPYING and Copyright.html.  COPYING can be found at the root
# of the source code distribution tree; Copyright.html can be found at the
# root level of an installed copy of the electronic HDF5 document set and
# is linked from the top-level documents page.  It can also be found at
# http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have
# access to either file, you may request a copy from help@hdfgroup.org.
#

# How to create a parallel version of HDF5 on an IBM SP system
# that uses MPI and MPI-IO.

# Unfortunately, the configure/make process to create the parallel version of
# HDF5 has not yet been automated to the same extent that the sequential
# version has.
# Read the INSTALL file to understand the configure/make process for the
# sequential (i.e., uniprocess) version of HDF5.
# The process for creating the parallel version of HDF5 using MPI-IO
# is similar, but first you will have to set up some environment variables
# with values specific to your local installation.
# The relevant variables are shown below, with values that work for LLNL's
# ASCI baby blue pacific SP as of the writing of these instructions (980210).

# In addition to the environment variables, you _might_ also have to
# create a new file in the config directory.
# You will need to create this file only if the execution of the ./configure
# program aborts with an error after printing the message 
# "checking whether byte ordering is bigendian..."
#
# If this is the case, create a new file in the config directory
# whose name is of the form architecture-vendor-OSversion
# (e.g., for baby blue pacific, this file is named powerpc-ibm-aix4.2.1.0)
# and which contains the line
#	ac_cv_c_bigendian=${ac_cv_c_bigendian='yes'}
# if the target architecture is bigendian, or
#	ac_cv_c_bigendian=${ac_cv_c_bigendian='no'}
# otherwise.
# Running the program ./bin/config.guess will print out the name
# of the new file you must create.

# Don't try to make a parallel version of HDF5 from the same hdf5 root
# directory where you made a sequential version of HDF5 -- start with
# a fresh copy.
# Here are the flags you must set before running the ./configure program
# to create the parallel version of HDF5.
# (We use csh here, but of course you can adapt to whatever shell you like.)

# compile for MPI jobs
setenv CC "/usr/local/mpich-1.1.2+romio_lgfiles/bin/mpicc"

#
# next 4 for IBM mpi
#
#setenv CC /usr/lpp/ppe.poe/bin/mpcc_r

#
# for both
#
setenv MP_PROCS 1


# These compiler flags work on ASCI baby blue pacific (IBM SP),
# using IBM's MPI and Argonne's MPI-IO (ROMIO):
#  -DHAVE_FUNCTION			compiler accepts __FUNCTION__ notation
#  -I/usr/local/mpio/include/ibm	using ROMIO's MPI-IO header files
#
# The following flags are only needed when compiling/linking a user program
# for execution.
#  -bI:/usr/include/piofs/piofs.exp	this MPI-IO uses PIOFS file system
#  -L/usr  /local/mpio/lib/ibm -lmpio	link to this MPI-IO lib
#
#setenv CFLAGS "-D_LARGE_FILES $CFLAGS"

# The configure/make process needs to be able to run some programs,
# need to specify a processor pool.
# Also, don't prepend the process id in the output of the programs
# run by config/make.
setenv MP_RMPOOL 0
setenv MP_LABELIO no

# Once these variables are set to the proper values for your installation,
# you can run the configure program (i.e., ./configure)
# to set up the Makefiles, etc.
# After configuring, run the make as described in the INSTALL file.
# Once the configuration is complete, you can set any of your
# environment variables to whatever you like.

# the files in the config directory, such as 
# config/powerpc-ibm-aix4.2.1.0  
# config/powerpc-ibm-aix4.x 
# config/powerpc-ibm-aix4.3.2.0
# sometimes will need some help depending on subtlties of the installation


# When compiling and linking your application, don't forget to compile with
# mpcc and link to the MPI-IO library and the parallel version of the HDF5
# library (that was created and installed with the configure/make process).
