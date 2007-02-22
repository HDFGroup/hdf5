#! /bin/sh
#
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
# Tests for the h5diff tool
#
# Modification:
#   Albert Cheng, 2005/08/17
#   Added the SKIP feature.
#   Albert Cheng, 2005/2/3
#   Added -p option for parallel h5diff tests.
#   Pedro Vicente Nunes:
#    10/25/2005: Added test #9
#    11/27/2006: Added test #10, #11


###############################################################################
## test file names 
###############################################################################

FILE1=h5diff_basic1.h5
FILE2=h5diff_basic2.h5
FILE3=h5diff_types.h5
FILE4=h5diff_dtypes.h5
FILE5=h5diff_attr1.h5
FILE6=h5diff_attr2.h5
FILE7=h5diff_dset1.h5
FILE8=h5diff_dset2.h5
FILE9=h5diff_hyper1.h5
FILE10=h5diff_hyper2.h5


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
STDERR_FILTER() {
    result_file=$1
    tmp_file=/tmp/h5test_tmp_$$
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
    # Filter h5diff debug output
	cp $result_file $tmp_file
	sed -e '/^h5diff debug: /d' \
	    < $tmp_file > $result_file
    # clean up temporary files.
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
    actual_sav=${actual}-sav
    actual_err_sav=${actual_err}-sav
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
    # save actual and actual_err in case they are needed later.
    cp $actual $actual_sav
    STDOUT_FILTER $actual
    cp $actual_err $actual_err_sav
    STDERR_FILTER $actual_err
    cat $actual_err >> $actual

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
		echo "====The actual output ($actual_sav)"
		sed 's/^/    /' < $actual_sav 
		echo "====The actual stderr ($actual_err_sav)"
		sed 's/^/    /' < $actual_err_sav 
		echo "====End of actual stderr ($actual_err_sav)"
		echo ""
	    fi
	fi
    fi

    # Clean up output file
    if test -z "$HDF5_NOCLEANUP"; then
	rm -f $actual $actual_err $actual_sav $actual_err_sav
	rm -f $actual_sorted $expect_sorted
    fi
}


# Print a "SKIP" message
SKIP() {
	 TESTING $H5DIFF $@
	  echo  " -SKIP-"
}


##############################################################################
##############################################################################
###			  T H E   T E S T S                                            ###
##############################################################################
##############################################################################

# ############################################################################
# # Common usage
# ############################################################################


# 1.0
TOOLTEST h5diff_10.txt -h

# 1.1 normal mode
TOOLTEST h5diff_11.txt  $FILE1 $FILE2 

# 1.2 normal mode with objects
TOOLTEST h5diff_12.txt  $FILE1 $FILE2  g1/dset1 g1/dset2

# 1.3 report mode
TOOLTEST h5diff_13.txt $FILE1 $FILE2 -r

# 1.4 report  mode with objects
TOOLTEST h5diff_14.txt  $FILE1 $FILE2  -r g1/dset1 g1/dset2

# 1.5 with -d
TOOLTEST h5diff_15.txt $FILE1 $FILE2 -r -d 5 g1/dset3 g1/dset4

# 1.6 with -p (test divide by zero case)
TOOLTEST h5diff_16.txt $FILE1 $FILE1 g1/dset9 g1/dset10 -p 0.01 -v

# 1.7 verbose mode
TOOLTEST h5diff_17.txt $FILE1 $FILE2 -v  

# 1.8 quiet mode 
TOOLTEST h5diff_18.txt $FILE1 $FILE2 -q

# 1.9.1 with -p (int)
TOOLTEST h5diff_191.txt $FILE1 $FILE1 -v -p 0.02 g1/dset5 g1/dset6

# 1.9.2 with -p (unsigned long_long)
#TOOLTEST h5diff_192.txt $FILE1 $FILE1 -v -p 0.02 g1/dset7 g1/dset8


# ##############################################################################
# # not comparable types
# ##############################################################################

# 2.0
TOOLTEST h5diff_20.txt $FILE3 $FILE3 -v dset g1

# 2.1
TOOLTEST h5diff_21.txt $FILE3 $FILE3 -v dset l1

# 2.2
TOOLTEST h5diff_22.txt $FILE3 $FILE3 -v dset t1

# ##############################################################################
# # compare groups, types, links (no differences and differences)
# ##############################################################################

# 2.3
TOOLTEST h5diff_23.txt $FILE3 $FILE3 -v g1 g1

# 2.4
TOOLTEST h5diff_24.txt $FILE3 $FILE3 -v t1 t1

# 2.5
TOOLTEST h5diff_25.txt $FILE3 $FILE3 -v l1 l1 

# 2.6
TOOLTEST h5diff_26.txt $FILE3 $FILE3 -v g1 g2

# 2.7
TOOLTEST h5diff_27.txt $FILE3 $FILE3 -v t1 t2

# 2.8
TOOLTEST h5diff_28.txt $FILE3 $FILE3 -v l1 l2



# ##############################################################################
# # Dataset datatypes
# ##############################################################################

# 5.0
TOOLTEST h5diff_50.txt $FILE4 $FILE4 -v dset0a dset0b

# 5.1
TOOLTEST h5diff_51.txt $FILE4 $FILE4 -v dset1a dset1b

# 5.2
TOOLTEST h5diff_52.txt $FILE4 $FILE4 -v dset2a dset2b

# 5.3
TOOLTEST h5diff_53.txt $FILE4 $FILE4 -v dset3a dset4b

# 5.4
TOOLTEST h5diff_54.txt $FILE4 $FILE4 -v dset4a dset4b

# 5.5
TOOLTEST h5diff_55.txt $FILE4 $FILE4 -v dset5a dset5b

# 5.6
TOOLTEST h5diff_56.txt $FILE4 $FILE4 -v dset6a dset6b

# 5.7
TOOLTEST h5diff_57.txt $FILE4 $FILE4 -v dset7a dset7b

# 5.8 (region reference)
TOOLTEST h5diff_58.txt $FILE7 $FILE8 -v refreg

# ##############################################################################
# # Error messages
# ##############################################################################


# 6.0: Check if the command line number of arguments is less than 3
TOOLTEST h5diff_600.txt $FILE1 

# 6.1: Check for invalid options
TOOLTEST h5diff_601.txt $FILE1 $FILE2 -x 

# ##############################################################################
# # -d 
# ##############################################################################

# 6.2: no value
TOOLTEST h5diff_602.txt $FILE1 $FILE2  -d g1/dset3 g1/dset4

# 6.3: negative value
TOOLTEST h5diff_603.txt $FILE1 $FILE2  -d -4 g1/dset3 g1/dset4

# 6.4: zero
TOOLTEST h5diff_604.txt $FILE1 $FILE2  -d 0 g1/dset3 g1/dset4

# 6.5: non number
TOOLTEST h5diff_605.txt $FILE1 $FILE2  -d u g1/dset3 g1/dset4

# 6.6: hexadecimal
TOOLTEST h5diff_606.txt $FILE1 $FILE2 -d 0x1 g1/dset3 g1/dset4

# 6.7: string
TOOLTEST h5diff_607.txt $FILE1 $FILE2  -d "1" g1/dset3 g1/dset4

# 6.8: repeated option
TOOLTEST h5diff_608.txt $FILE1 $FILE2  -d 1 -d 2 g1/dset3 g1/dset4

# 6.9: number larger than biggest difference
TOOLTEST h5diff_609.txt $FILE1 $FILE2  -d 200 g1/dset3 g1/dset4

# 6.10: number smaller than smallest difference
TOOLTEST h5diff_610.txt $FILE1 $FILE2  -d 1 g1/dset3 g1/dset4


# ##############################################################################
# # -p
# ##############################################################################


# 6.11: no value
TOOLTEST h5diff_611.txt $FILE1 $FILE2 -r -p g1/dset3 g1/dset4

# 6.12: negative value
TOOLTEST h5diff_612.txt $FILE1 $FILE2 -p -4 g1/dset3 g1/dset4

# 6.13: zero
TOOLTEST h5diff_613.txt $FILE1 $FILE2 -p 0 g1/dset3 g1/dset4

# 6.14: non number
TOOLTEST h5diff_614.txt $FILE1 $FILE2 -p u g1/dset3 g1/dset4

# 6.15: hexadecimal
TOOLTEST h5diff_615.txt $FILE1 $FILE2 -p 0x1 g1/dset3 g1/dset4

# 6.16: string
TOOLTEST h5diff_616.txt $FILE1 $FILE2 -p "0.21" g1/dset3 g1/dset4

# 6.17: repeated option
TOOLTEST h5diff_617.txt $FILE1 $FILE2 -p 0.21 -p 0.22 g1/dset3 g1/dset4

# 6.18: number larger than biggest difference
TOOLTEST h5diff_618.txt $FILE1 $FILE2 -p 2 g1/dset3 g1/dset4

# 6.19: number smaller than smallest difference
TOOLTEST h5diff_619.txt $FILE1 $FILE2 -p 0.005 g1/dset3 g1/dset4



# ##############################################################################
# # -n
# ##############################################################################


# 6.20: no value
TOOLTEST h5diff_620.txt $FILE1 $FILE2 -n g1/dset3 g1/dset4

# 6.21: negative value
TOOLTEST h5diff_621.txt $FILE1 $FILE2 -n -4 g1/dset3 g1/dset4

# 6.22: zero
TOOLTEST h5diff_622.txt $FILE1 $FILE2 -n 0 g1/dset3 g1/dset4

# 6.23: non number
TOOLTEST h5diff_623.txt $FILE1 $FILE2 -n u g1/dset3 g1/dset4

# 6.24: hexadecimal
TOOLTEST h5diff_624.txt $FILE1 $FILE2 -n 0x1 g1/dset3 g1/dset4

# 6.25: string
TOOLTEST h5diff_625.txt $FILE1 $FILE2 -n "2" g1/dset3 g1/dset4

# 6.26: repeated option
TOOLTEST h5diff_626.txt $FILE1 $FILE2 -n 2 -n 3 g1/dset3 g1/dset4

# 6.27: number larger than biggest difference
TOOLTEST h5diff_627.txt $FILE1 $FILE2 -n 200 g1/dset3 g1/dset4

# 6.28: number smaller than smallest difference
TOOLTEST h5diff_628.txt $FILE1 $FILE2 -n 1 g1/dset3 g1/dset4

# ##############################################################################
# 6.29  non valid files
# ##############################################################################

TOOLTEST h5diff_629.txt file1.h6 file2.h6

# ##############################################################################
# 7.  attributes
# ##############################################################################

TOOLTEST h5diff_70.txt $FILE5 $FILE6 -v

# ##############################################################################
# 8.  all dataset datatypes
# ##############################################################################

TOOLTEST h5diff_80.txt $FILE7 $FILE8 -v

# 9. compare a file with itself

TOOLTEST h5diff_90.txt $FILE1 $FILE1

# 10. read by hyperslab, print indexes

TOOLTEST h5diff_100.txt $FILE9 $FILE10 -v

# 11. floating point comparison

TOOLTEST h5diff_101.txt $FILE1 $FILE1 /g1/d1  g1/d2  -v
TOOLTEST h5diff_102.txt $FILE1 $FILE1 /g1/fp1 g1/fp2 -v

# ##############################################################################
# # END
# ##############################################################################

if test $nerrors -eq 0 ; then
   echo "All $H5DIFF tests passed."
fi

exit $nerrors
