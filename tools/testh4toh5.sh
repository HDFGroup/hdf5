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

h4toh5=h4toh5		# a relative name

COMMAND=`pwd`/"$h4toh5"	# an absolute command name

cmp='cmp -s'
diff='diff -c'

RM='rm -f'
SED='sed '
H5DUMP=${H5DUMP:-`pwd`/'h5dump'}	# Default to use the h5dumper
					# in the same tools directory

# Verify if $H5DUMP is a valid command.
tmpfile=/tmp/testh4toh5.$$
$H5DUMP -V > $tmpfile
if [ ! -s $tmpfile ]; then
    echo "    Could not run the '$H5DUMP' command.  The test can still proceed"
    echo "    but it may fail if '$H5DUMP' is needed to verify the output."
    echo "    You can make sure '$H5DUMP' is among your shell PATH and run"
    echo "    the test again.  You may also visit http://hdf.ncsa.uiuc.edu"
    echo "    or email hdfhelp@ncsa.uiuc.edu for more information."
    H5DUMP=:
fi
$RM $tmpfile

# The build (current) directory might be different than the source directory.
if test "X$srcdir" = X; then
    srcdir=.
fi
mkdir testfiles >/dev/null 2>&1

SRCDIR="$srcdir/testfiles"
OUTDIR="./testfiles/Results"

test -d "$OUTDIR" || mkdir $OUTDIR

nerrors=0
verbose=yes

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Testing".
TESTING()
{
    SPACES="                                                               "
    echo "Testing $* $SPACES" |cut -c1-70 |tr -d '\012'
}

# Run a test and print PASS or *FAIL*.  If a test fails then increment
# the `nerrors' global variable and (if $verbose is set) display the
# difference between the actual and the expected hdf4 files. The
# expected hdf5 files are in testfiles/Expected directory.
# The actual hdf5 file is not removed if $HDF5_NOCLEANUP is to a non-null
# value.
CONVERT()
{
    # Run h4toh5 convert.
    TESTING $h4toh5 $@

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
    "-m")		# multiple files conversion
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
    * )			# Single file conversion
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
	$COMMAND $INFILES $OUTFILES 2>/dev/null
	if test "$SRCDIR" != "$OUTDIR"; then
	    $RM $INFILES
	fi
    )

    # Verify results
    result="passed"
    for f in $OUTFILES
    do
	if $cmp $SRCDIR/Expected/$f $OUTDIR/$f
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
	    elif $cmp $expect_out $actual_out; then
		:
	    else
		if test "passed" = $result; then
		    echo "*FAILED*"
		    nerrors="`expr $nerrors + 1`"
		    result=failed
		fi
		test yes = "$verbose" &&
		echo "    Actual result (*.actual) differs from expected result (*.expect)" &&
		$diff $expect_out $actual_out |sed 's/^/    /'
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
CONVERT vg.hdf vg.h5
CONVERT anfile.hdf anfile.h5
CONVERT anno_obj.hdf anno_obj.h5
CONVERT image_attr.hdf image_attr.h5
#CONVERT image_maxsize.hdf image_maxsize.h5
CONVERT image_pal.hdf image_pal.h5
CONVERT image_uint16.hdf image_uint16.h5
CONVERT image_uint161.hdf image_uint161.h5
CONVERT image_uint32.hdf image_uint32.h5
CONVERT image_uint321.hdf image_uint321.h5
CONVERT image_uint8.hdf image_uint8.h5
CONVERT image_uint81.hdf image_uint81.h5
CONVERT sds_dimsca_data.hdf sds_dimsca_data.h5
CONVERT sds_dimscaunl_data.hdf sds_dimscaunl_data.h5
#CONVERT sds_maxdim.hdf sds_maxdim.h5
#CONVERT sds_maxdimsizedata.hdf sds_maxdimsizedata.h5
CONVERT sds_typattr.hdf sds_typattr.h5
CONVERT sds_typchar8.hdf sds_typchar8.h5
CONVERT sds_typchunk_comp.hdf sds_typchunk_comp.h5
CONVERT sds_typfloat32.hdf sds_typfloat32.h5
CONVERT sds_typfloat64.hdf sds_typfloat64.h5
CONVERT sds_typnfloat32.hdf sds_typnfloat32.h5
CONVERT sds_typnfloat64.hdf sds_typnfloat64.h5
CONVERT sds_typint16.hdf sds_typint16.h5
CONVERT sds_typint32.hdf sds_typint32.h5
CONVERT sds_typint8.hdf sds_typint8.h5
CONVERT sds_typlint16.hdf sds_typlint16.h5
CONVERT sds_typlint32.hdf sds_typlint32.h5
CONVERT sds_typlint8.hdf sds_typlint8.h5
CONVERT sds_typluint16.hdf sds_typluint16.h5
CONVERT sds_typluint32.hdf sds_typluint32.h5
CONVERT sds_typluint8.hdf sds_typluint8.h5
CONVERT sds_typuint16.hdf sds_typuint16.h5
CONVERT sds_typuint32.hdf sds_typuint32.h5
CONVERT sds_typuint8.hdf sds_typuint8.h5
CONVERT vdata_attrtest.hdf vdata_attrtest.h5
CONVERT vdata_typtest.hdf vdata_typtest.h5
CONVERT vg_attrtest.hdf vg_attrtest.h5
CONVERT vg_hl.hdf vg_hl.h5
CONVERT vg_loop.hdf vg_loop.h5
CONVERT vg_nameclash.hdf vg_nameclash.h5
CONVERT vg_nameclash2.hdf vg_nameclash2.h5
CONVERT vg_simple.hdf vg_simple.h5

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
