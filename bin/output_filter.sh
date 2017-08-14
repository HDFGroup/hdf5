## Copyright by The HDF Group.
## All rights reserved.
##
## This file is part of HDF5.  The full HDF5 copyright notice, including
## terms governing use, modification, and redistribution, is contained in
## the COPYING file, which can be found at the root of the source code
## distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.
## If you do not have access to either file, you may request a copy from
## help@hdfgroup.org.

# This contains function definitions of output filtering.
# This file should only be sourced in by another shell script.
#
# Programmer: Albert Cheng
# Created Date: 2011/5/3


# Comment added to address HDFFV-8270:
# As I understand it, the purpose of this file is to remove extraneous messages 
# that appear in stdout and stderr on some machines that have been tested outside 
# of the HDF Group realm.  The purpose of this script is to filter those 
# extraneous messages from stdout and stderr so that when the output files are
# compared to the expected output, the extra messages will not cause failures in 
# the tests.  The system messages in the comments below are out of date, meaning
# I suppose that while the script code to filter messages on the system was 
# correct correct when last used, the output in the comments doesn't match the
# script code that follows.  I don't currently have access to any of these 
# systems to see the current output and the effect of the script code. If using
# this file in the future, please update the comments to match the scripts in use.
# Larry Knox 2017/3/15


# Some systems will dump some messages to stdout for various reasons.
# Remove them from the stdout result file.
# $1 is the file name of the file to be filtered.
# Cases of filter needed:
# 1. Sandia Red-Storm
#    yod always prints these two lines at the beginning.
#    LibLustre: NAL NID: 0004a605 (5)
#    Lustre: OBD class driver Build Version: 1, info@clusterfs.com
# 2. LANL Lambda
#    mpijob mirun -np always add an extra line at the end like:
#    P4 procgroup file is /users/acheng/.lsbatch/host10524.l82
STDOUT_FILTER() {
    result_file=$1
    tmp_file=/tmp/h5test_tmp_$$
    # Filter Sandia Red-Storm yod messages.
    cp $result_file $tmp_file
    sed -e '/^LibLustre:/d' -e '/^Lustre:/d' \
	< $tmp_file > $result_file
    # Filter LANL Lambda mpirun message.
    cp $result_file $tmp_file
    sed -e '/^P4 procgroup file is/d' \
	< $tmp_file > $result_file
    # cleanup
    rm -f $tmp_file
}


# Some systems will dump some messages to stderr for various reasons.
# Remove them from the stderr result file.
# $1 is the file name of the file to be filtered.
# Cases of filter needed:
# 1. MPE:
# In parallel mode and if MPE library is used, it prints the following
# two message lines whether the MPE tracing is used or not.
#    Writing logfile.
#    Finished writing logfile.
# 2. LANL MPI:
# The LANL MPI will print some messages like the following,
#    LA-MPI: *** mpirun (1.5.10)
#    LA-MPI: *** 3 process(es) on 2 host(s): 2*fln21 1*fln22
#    LA-MPI: *** libmpi (1.5.10)
#    LA-MPI: *** Copyright 2001-2004, ACL, Los Alamos National Laboratory
# 3. h5diff debug output:
#    Debug output all have prefix "h5diff debug: ".
# 4. AIX system prints messages like these when it is aborting:
#    ERROR: 0031-300  Forcing all remote tasks to exit due to exit code 1 in task 0
#    ERROR: 0031-250  task 4: Terminated
#    ERROR: 0031-250  task 3: Terminated
#    ERROR: 0031-250  task 2: Terminated
#    ERROR: 0031-250  task 1: Terminated
# 5. LLNL Blue-Gene mpirun prints messages like there when it exit non-zero:
#    <Apr 12 15:01:49.075658> BE_MPI (ERROR): The error message in the job record is as follows:
#    <Apr 12 15:01:49.075736> BE_MPI (ERROR):   "killed by exit(1) on node 0"
STDERR_FILTER() {
    result_file=$1
    tmp_file=/tmp/h5test_tmp_$$
    # Filter LLNL Blue-Gene error messages in both serial and parallel modes
    # since mpirun is used in both modes.
    cp $result_file $tmp_file
    sed -e '/ BE_MPI (ERROR): /d' \
	< $tmp_file > $result_file
    # Filter MPE messages
    if test -n "$pmode"; then
	cp $result_file $tmp_file
	sed -e '/^Writing logfile./d' -e '/^Finished writing logfile./d' \
	    < $tmp_file > $result_file
    fi
    # Filter LANL MPI messages
    # and LLNL srun messages
    # and AIX error messages
    if test -n "$pmode"; then
	cp $result_file $tmp_file
	sed -e '/^LA-MPI:/d' -e '/^srun:/d' -e '/^ERROR:/d' \
	    < $tmp_file > $result_file
    fi
    # Filter h5diff debug output
	cp $result_file $tmp_file
	sed -e '/^h5diff debug: /d' \
	    < $tmp_file > $result_file
    # clean up temporary files.
    rm -f $tmp_file
}
