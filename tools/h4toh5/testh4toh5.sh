#!/bin/sh
#
# Copyright (C) 2001 National Center for Supercomputing Applications.
#                    All rights reserved.
#
# Test script for the h4toh5 tests.
# Using the h4toh5 convert to convert a pre-created hdf file to
# an hdf5 file (output file), then compare it with a pre-created
# corresponding hdf5 file (expected file).
# If the same, that particular test passes.
# If not the same, the output file and expected file are processed
# by the h5dump tool to see if they produce the same results.
# If the same, the test passes.
# If not, show the difference of the two results and report the test failed.
#
# h5dump is default to use the one just built.  It can be overridden
# by setting $H5DUMP to a different value such as /usr/local/bin/h5dump.

H4TOH5=h4toh5               # The tool name
H4TOH5_BIN=`pwd`/$H4TOH5    # The path of the tool binary

CMP='cmp -s'
DIFF='diff -c'

RM='rm -f'
SED='sed '
H5DUMP=${H5DUMP:-`pwd`/'../h5dump/h5dump'}  # Default to use the h5dumper
                                            # in the same tools directory

# Verify if $H5DUMP is a valid command.
tmpfile=/tmp/testh4toh5.$$
$H5DUMP -V > $tmpfile
if test -s "$tmpfile"; then
    :
else
    echo "    Could not run the '$H5DUMP' command. The test can still proceed"
    echo "    but it may fail if '$H5DUMP' is needed to verify the output."
    echo "    You can make sure '$H5DUMP' is among your shell PATH and run"
    echo "    the test again. You may also visit http://hdf.ncsa.uiuc.edu"
    echo "    or email hdfhelp@ncsa.uiuc.edu for more information."
    H5DUMP=:
fi
$RM $tmpfile

# The build (current) directory might be different than the source directory.
if test -z "$srcdir"; then
    srcdir=.
fi

mkdir ../testfiles >/dev/null 2>&1

SRCDIR="$srcdir/../testfiles"
OUTDIR="../testfiles/Results"

test -d "$OUTDIR" || mkdir $OUTDIR

nerrors=0
verbose=yes

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Testing".
TESTING() {
    SPACES="                                                               "
    echo "Testing $* $SPACES" |cut -c1-70 |tr -d '\012'
}

# Run a test and print PASS or *FAIL*.  If a test fails then increment
# the `nerrors' global variable and (if $verbose is set) display the
# difference between the actual and the expected hdf4 files. The
# expected hdf5 files are in testfiles/Expected directory.
# The actual hdf5 file is not removed if $HDF5_NOCLEANUP is to a non-null
# value.
CONVERT() {
    # Run h4toh5 convert.
    TESTING $H4TOH5 $@

    #
    # Set up arguments to run the conversion test.
    # The converter assumes all hdf4 files has the .hdf suffix as in the form
    # of foo.hdf.  It creates the corresponding hdf5 files with the .h5 suffix
    # as in the form of foo.h5.  One exception is that if exactly two file
    # names are given, it treats the first argument as an hdf4 file and creates
    # the corresponding hdf5 file with the name as the second argument, WITOUT
    # any consideration of the suffix.  (For this test script, in order to
    # match the output hdf5 file with the expected hdf5 file, it expects the
    # second file of the two-files tests has the .h5 suffix too.)
    #
    # If SRCDIR != OUTDIR, need to copy the input hdf4 files from the SRCDIR
    # to the OUTDIR and transform the input file pathname because of the suffix
    # convention mentioned above.  This way, the hdf5 files are always created
    # in the OUTDIR directory.
    #

    INFILES=""
    OUTFILES=""
    MULTIRUN=""

    case "$1" in
    -m)		# multiple files conversion
	MULTIRUN="-m"
	shift
	for f in $*
	do
	    if test "$SRCDIR" != "$OUTDIR"; then
		cp $SRCDIR/$f $OUTDIR/$f
	    fi
	    INFILES="$INFILES $f"
	    OUTFILES="$OUTFILES `basename $f .hdf`.h5"
	    shift
	done
	;;

    *)			# Single file conversion
	case $# in
	1)  if test "$SRCDIR" != "$OUTDIR"; then
		cp $SRCDIR/$1 $OUTDIR/$1
	    fi
	    INFILES="$1"
	    OUTFILES="`basename $1 .hdf`.h5"
	    ;;

	2) 		# hdf4 file specified
	    if test "$SRCDIR" != "$OUTDIR"; then
		cp $SRCDIR/$1 $OUTDIR/$1
	    fi
	    INFILES="$1"
	    OUTFILES="$2"
	    ;;

	*)		# Illegal
	    echo "Illegal arguments"
	    exit 1
	    ;;
	esac
	;;
    esac

    # run the conversion and remove input files that have been copied over
    (
	cd $OUTDIR
	$H4TOH5_BIN $INFILES $OUTFILES 2>/dev/null
	if test "$SRCDIR" != "$OUTDIR"; then
	    $RM $INFILES
	fi
    )

    # Verify results
    result="passed"
    for f in $OUTFILES
    do
	if $CMP $SRCDIR/Expected/$f $OUTDIR/$f
	then
	    :
	else
	    # Use h5dump to dump the files and verify the output.
	    outfile=`basename $f .h5`
	    expect_out=$outfile.expect
	    actual_out=$outfile.actual

	    (cd $SRCDIR/Expected
	     $H5DUMP $outfile.h5 ) > $expect_out
	    (cd $OUTDIR
	     $H5DUMP $outfile.h5 ) > $actual_out

	    if [ "passed" = $result -a ! -s $actual_out ] ; then
		echo "*FAILED*"
		nerrors="`expr $nerrors + 1`"
		result=failed
		test yes = "$verbose" &&
		    echo "    H5DUMP failed to produce valid output"
	    elif $CMP $expect_out $actual_out; then
		:
	    else
		if test "passed" = $result; then
		    echo "*FAILED*"
		    nerrors="`expr $nerrors + 1`"
		    result=failed
		fi
		test yes = "$verbose" &&
		echo "    Actual result (*.actual) differs from expected result (*.expect)" &&
		$DIFF $expect_out $actual_out |sed 's/^/    /'
	    fi
	fi

	# Clean up output file
	if test -z "$HDF5_NOCLEANUP"; then
	    $RM $expect_out $actual_out
	    $RM $OUTDIR/$f
	fi
    done
    if test "passed" = "$result"; then
	    echo " PASSED"
    fi
}

##############################################################################
##############################################################################
###			  T H E   T E S T S                                ###
##############################################################################
##############################################################################

$RM $OUTDIR/*.hdf $OUTDIR/*.tmp

#
# The HDF5 filenames are created based upon the HDF4 filenames
# without the extension.
#

# test for converting H5 groups to H4 Vgroups.
#CONVERT vg.hdf


#
# The test for conversion are the same as above with the only difference
# being that the HDF5 filenames are given explicitly.
#

$RM $OUTDIR/*.tmp
CONVERT anno_test.hdf anno_test.h5
CONVERT gr_typ_test.hdf gr_typ_test.h5
CONVERT grnameclash_test.hdf grnameclash_test.h5
CONVERT image_attr_test.hdf image_attr_test.h5
#CONVERT image_maxsize.hdf image_maxsize.h5
CONVERT ras24il.hdf ras24il.h5
CONVERT ras_24_test.hdf ras_24_test.h5
CONVERT ras_8_test.hdf ras_8_test.h5
CONVERT sds_attr_test.hdf sds_attr_test.h5
CONVERT sds_dim_test.hdf sds_dim_test.h5
CONVERT sds_typ_test.hdf sds_typ_test.h5
CONVERT sdsnameclash_test.hdf sdsnameclash_test.h5
CONVERT vdata_test.hdf vdata_test.h5
CONVERT vdnameclash_test.hdf vdnameclash_test.h5
CONVERT vg_hl_test.hdf vg_hl_test.h5
CONVERT vg_loop_test.hdf vg_loop_test.h5
CONVERT vgnameclash_test.hdf vgnameclash_test.h5
CONVERT vg_all_test.hdf vg_all_test.h5
#
# Again, the test for conversion are the same as the first set of test.
# Here, multiple conversion are done on HDF4 files at one time.
#

$RM $OUTDIR/*.hdf $OUTDIR/*.tmp
#CONVERT -m vg.hdf

if test $nerrors -eq 0 ; then
	echo "All h4toh5 tests passed."
fi

$RM -r $OUTDIR
exit $nerrors
