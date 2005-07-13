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
# Tests for the h5diff tool
#
# Modification:
#   Albert Cheng, 2005/2/3
#   Added -p option for parallel h5diff tests.

H5DIFF=h5diff               # The tool name
H5DIFF_BIN=`pwd`/$H5DIFF    # The path of the tool binary

CMP='cmp -s'
DIFF='diff -c'

nerrors=0
verbose=yes
pmode=			    # default to run h5diff tests

# The build (current) directory might be different than the source directory.
if test -z "$srcdir"; then
   srcdir=.
fi

test -d ../testfiles || mkdir ../testfiles

# Parse option
#   -p   run ph5diff tests
#   -h   print help page
while [ $# -gt 0 ]; do
    case "$1" in
    -p)	# run ph5diff tests
	H5DIFF_BIN=`pwd`/ph5diff
	pmode=yes
	shift
	;;
    -h) # print help page
	echo "$0 [-p] [-h]"
	echo "    -p   run ph5diff tests"
	echo "    -h   print help page"
	shift
	exit 0
	;;
    *)  # unknown option
        echo "$0: Unknown option ($1)"
	exit 1
	;;
    esac
done

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Testing".
#
TESTING() {
   SPACES="                                                               "
   echo "Testing $* $SPACES" | cut -c1-70 | tr -d '\012'
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
STDERR_FILTER() {
    result_file=$1
    tmp_file=/tmp/h5diff_tmp_$$
    # Filter MPE messages
    if test -n "$pmode"; then
	cp $result_file $tmp_file
	sed -e '/^Writing logfile./d' -e '/^Finished writing logfile./d' \
	    < $tmp_file > $result_file
    fi
    # Filter LANL MPI messages
    # and LLNL srun messages
    if test -n "$pmode"; then
	cp $result_file $tmp_file
	sed -e '/^LA-MPI:/d' -e '/^srun:/d' \
	    < $tmp_file > $result_file
    fi
    rm -f $tmp_file
}

# Run a test and print PASS or *FAIL*.  If a test fails then increment
# the `nerrors' global variable and (if $verbose is set) display the
# difference between the actual output and the expected output. The
# expected output is given as the first argument to this function and
# the actual output file is calculated by replacing the `.ddl' with
# `.out'.  The actual output is not removed if $HDF5_NOCLEANUP has a
# non-zero value.
#
# Need eval before the RUNCMD command because some machines like
# AIX, has RUNPARALLEL in the style as
#   MP_PROCS=3 MP_TASKS_PER_NODE=3 poe ./a.out
# that throws the shell script off.
#
TOOLTEST() {
    expect="$srcdir/../testfiles/$1"
    actual="../testfiles/`basename $1 .txt`.out"
    actual_err="../testfiles/`basename $1 .txt`.err"
    tmp_err=${actual_err}-tmp
    shift
    if test -n "$pmode"; then
	RUNCMD=$RUNPARALLEL
    else
	RUNCMD=$RUNSERIAL
    fi

    # Run test.
    # Tflops interprets "$@" as "" when no parameter is given (e.g., the
    # case of missing file name).  Changed it to use $@ till Tflops fixes it.
    TESTING $H5DIFF $@
    (
	echo "#############################"
	echo "Expected output for '$H5DIFF $@'" 
	echo "#############################"
	cd $srcdir/../testfiles
	if [ "`uname -s`" = "TFLOPS O/S" ]; then
	    eval $RUNCMD $H5DIFF_BIN $@
	else
	    eval $RUNCMD $H5DIFF_BIN "$@"
	fi
    ) >$actual 2>$actual_err
    # save actual_err in case it is needed later.
    cp $actual_err $tmp_err
    STDERR_FILTER $tmp_err
    cat $tmp_err >> $actual

    if [ ! -f $expect ]; then
    # Create the expect file if it doesn't yet exist.
        echo " CREATED"
	cp $actual $expect
    elif $CMP $expect $actual; then
	echo " PASSED"
    elif test -z "$pmode"; then
	echo "*FAILED*"
	echo "    Expected result ($expect) differs from actual result ($actual)"
	nerrors="`expr $nerrors + 1`"
	test yes = "$verbose" && $DIFF $expect $actual |sed 's/^/    /'
    else
	# parallel mode output are often of different ordering from serial
	# output.  If the sorted expected and actual files compare the same,
	# it is safe to assume the actual output match the expected file.
	expect_sorted=expect_sorted
	actual_sorted=actual_sorted
	sort $expect -o $expect_sorted
	sort $actual -o $actual_sorted
	if $CMP $expect_sorted $actual_sorted; then
	    echo " PASSED"
	else
	    echo "*FAILED*"
	    nerrors="`expr $nerrors + 1`"
	    if test yes = "$verbose"; then
		echo "====Expected result ($expect_sorted) differs from actual result ($actual_sorted)"
		$DIFF $expect_sorted $actual_sorted |sed 's/^/    /'
		echo "====The actual result ($actual)"
		sed 's/^/    /' < $actual 
		echo "====The part that is actual stderr ($actual_err)"
		sed 's/^/    /' < $actual_err 
		echo "====End of actual stderr ($actual_err)"
		echo ""
	    fi
	fi
    fi

    # Clean up output file
    if test -z "$HDF5_NOCLEANUP"; then
	rm -f $actual $actual_err $actual_sorted $tmp_err $expect_sorted
    fi
}


##############################################################################
##############################################################################
###			  T H E   T E S T S                                            ###
##############################################################################
##############################################################################

# ##############################################################################
# # Common usage
# ##############################################################################


# 1.0
TOOLTEST h5diff_10.txt -h

# 1.1 normal mode
TOOLTEST h5diff_11.txt  file1.h5 file2.h5 

# 1.2 normal mode with objects
TOOLTEST h5diff_12.txt  file1.h5 file2.h5  g1/dset1 g1/dset2

# 1.3 report mode
TOOLTEST h5diff_13.txt file1.h5 file2.h5 -r

# 1.4 report  mode with objects
TOOLTEST h5diff_14.txt  file1.h5 file2.h5  -r g1/dset1 g1/dset2

# 1.5 with -d
TOOLTEST h5diff_15.txt file1.h5 file2.h5 -r -d 5 g1/dset3 g1/dset4

# 1.6 with -p
TOOLTEST h5diff_16.txt file1.h5 file2.h5 -r -p 0.05 g1/dset3 g1/dset4

# 1.7 verbose mode
TOOLTEST h5diff_17.txt file1.h5 file2.h5 -v  

# 1.8 quiet mode 
TOOLTEST h5diff_18.txt file1.h5 file2.h5 -q

# ##############################################################################
# # not comparable types
# ##############################################################################

# 2.0
TOOLTEST h5diff_20.txt file3.h5 file3.h5 -v dset group

# 2.1
TOOLTEST h5diff_21.txt file3.h5 file3.h5 -v dset link

# 2.2
TOOLTEST h5diff_22.txt file3.h5 file3.h5 -v dset type

# 2.3
TOOLTEST h5diff_23.txt file3.h5 file3.h5 -v group group

# 2.4
TOOLTEST h5diff_24.txt file3.h5 file3.h5 -v type type

# 2.5
TOOLTEST h5diff_25.txt file3.h5 file3.h5 -v link link


# ##############################################################################
# # Dataset types
# ##############################################################################

# 5.0
TOOLTEST h5diff_50.txt file4.h5 file4.h5 -v dset0a dset0b

# 5.1
TOOLTEST h5diff_51.txt file4.h5 file4.h5 -v dset1a dset1b

# 5.2
TOOLTEST h5diff_52.txt file4.h5 file4.h5 -v dset2a dset2b

# 5.3
TOOLTEST h5diff_53.txt file4.h5 file4.h5 -v dset3a dset4b

# 5.4
TOOLTEST h5diff_54.txt file4.h5 file4.h5 -v dset4a dset4b

# 5.5
TOOLTEST h5diff_55.txt file4.h5 file4.h5 -v dset5a dset5b

# 5.6
TOOLTEST h5diff_56.txt file4.h5 file4.h5 -v dset6a dset6b

# 5.7
TOOLTEST h5diff_57.txt file4.h5 file4.h5 -v dset7a dset7b

# 5.8 long_long test; different format of long_long print  in Linux and IRIX
#TOOLTEST h5diff_58.txt file4.h5 file4.h5 dset8a dset8b

# ##############################################################################
# # Error messages
# ##############################################################################


# 6.0: Check if the command line number of arguments is less than 3
TOOLTEST h5diff_600.txt file1.h5 

# 6.1: Check for invalid options
TOOLTEST h5diff_601.txt file1.h5 file2.h5 -x 

# ##############################################################################
# # -d 
# ##############################################################################

# 6.2: no value
TOOLTEST h5diff_602.txt file1.h5 file2.h5  -d g1/dset3 g1/dset4

# 6.3: negative value
TOOLTEST h5diff_603.txt file1.h5 file2.h5  -d -4 g1/dset3 g1/dset4

# 6.4: zero
TOOLTEST h5diff_604.txt file1.h5 file2.h5  -d 0 g1/dset3 g1/dset4

# 6.5: non number
TOOLTEST h5diff_605.txt file1.h5 file2.h5  -d u g1/dset3 g1/dset4

# 6.6: hexadecimal
TOOLTEST h5diff_606.txt file1.h5 file2.h5 -d 0x1 g1/dset3 g1/dset4

# 6.7: string
TOOLTEST h5diff_607.txt file1.h5 file2.h5  -d "1" g1/dset3 g1/dset4

# 6.8: repeated option
TOOLTEST h5diff_608.txt file1.h5 file2.h5  -d 1 -d 2 g1/dset3 g1/dset4

# 6.9: number larger than biggest difference
TOOLTEST h5diff_609.txt file1.h5 file2.h5  -d 200 g1/dset3 g1/dset4

# 6.10: number smaller than smallest difference
TOOLTEST h5diff_610.txt file1.h5 file2.h5  -d 1 g1/dset3 g1/dset4


# ##############################################################################
# # -p
# ##############################################################################


# 6.11: no value
TOOLTEST h5diff_611.txt file1.h5 file2.h5 -r -p g1/dset3 g1/dset4

# 6.12: negative value
TOOLTEST h5diff_612.txt file1.h5 file2.h5 -p -4 g1/dset3 g1/dset4

# 6.13: zero
TOOLTEST h5diff_613.txt file1.h5 file2.h5 -p 0 g1/dset3 g1/dset4

# 6.14: non number
TOOLTEST h5diff_614.txt file1.h5 file2.h5 -p u g1/dset3 g1/dset4

# 6.15: hexadecimal
TOOLTEST h5diff_615.txt file1.h5 file2.h5 -p 0x1 g1/dset3 g1/dset4

# 6.16: string
TOOLTEST h5diff_616.txt file1.h5 file2.h5 -p "0.21" g1/dset3 g1/dset4

# 6.17: repeated option
TOOLTEST h5diff_617.txt file1.h5 file2.h5 -p 0.21 -p 0.22 g1/dset3 g1/dset4

# 6.18: number larger than biggest difference
TOOLTEST h5diff_618.txt file1.h5 file2.h5 -p 2 g1/dset3 g1/dset4

# 6.19: number smaller than smallest difference
TOOLTEST h5diff_619.txt file1.h5 file2.h5 -p 0.005 g1/dset3 g1/dset4



# ##############################################################################
# # -n
# ##############################################################################


# 6.20: no value
TOOLTEST h5diff_620.txt file1.h5 file2.h5 -n g1/dset3 g1/dset4

# 6.21: negative value
TOOLTEST h5diff_621.txt file1.h5 file2.h5 -n -4 g1/dset3 g1/dset4

# 6.22: zero
TOOLTEST h5diff_622.txt file1.h5 file2.h5 -n 0 g1/dset3 g1/dset4

# 6.23: non number
TOOLTEST h5diff_623.txt file1.h5 file2.h5 -n u g1/dset3 g1/dset4

# 6.24: hexadecimal
TOOLTEST h5diff_624.txt file1.h5 file2.h5 -n 0x1 g1/dset3 g1/dset4

# 6.25: string
TOOLTEST h5diff_625.txt file1.h5 file2.h5 -n "2" g1/dset3 g1/dset4

# 6.26: repeated option
TOOLTEST h5diff_626.txt file1.h5 file2.h5 -n 2 -n 3 g1/dset3 g1/dset4

# 6.27: number larger than biggest difference
TOOLTEST h5diff_627.txt file1.h5 file2.h5 -n 200 g1/dset3 g1/dset4

# 6.28: number smaller than smallest difference
TOOLTEST h5diff_628.txt file1.h5 file2.h5 -n 1 g1/dset3 g1/dset4

# ##############################################################################
# # non valid files
# ##############################################################################

TOOLTEST h5diff_629.txt file1.h6 file2.h6

# ##############################################################################
# # attributes
# ##############################################################################

TOOLTEST h5diff_70.txt file5.h5 file6.h5 -v

# ##############################################################################
# # all dataset datatypes
# ##############################################################################

TOOLTEST h5diff_80.txt file7.h5 file8.h5 -v









# ##############################################################################
# # END
# ##############################################################################



if test $nerrors -eq 0 ; then
   echo "All $H5DIFF tests passed."
fi

exit $nerrors
