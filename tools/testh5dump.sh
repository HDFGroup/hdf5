#!/bin/sh
# Tests for the h5dump tool

h5tool=h5dump			# The tool name
h5tool_bin=`pwd`/$h5tool	# The path of the tool binary
CMP='cmp -s'
DIFF='diff -c'

nerrors=0
verbose=yes

# The build (current) directory might be different than the source directory.
if test "X$srcdir" = X; then
    srcdir=.
fi
test -d testfiles || mkdir testfiles

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Testing".
TESTING()
{
    SPACES="                                                               "
    echo "Testing $* $SPACES" |cut -c1-70 |tr -d '\012'
}

# Run a test and print PASS or *FAIL*.  If a test fails then increment
# the `nerrors' global variable and (if $verbose is set) display the
# difference between the actual output and the expected output. The
# expected output is given as the first argument to this function and
# the actual output file is calculated by replacing the `.ddl' with
# `.out'.  The actual output is not removed if $HDF5_NOCLEANUP has a
# non-zero value.
TOOLTEST()
{
    expect=$srcdir/testfiles/$1
    actual="testfiles/`basename $1 .ddl`.out"
    shift
    full=`pwd`/$h5tool

    # Run test.
    # Stderr is included in stdout so that the diff can detect
    # any unexpected output from that stream too.
    TESTING $h5tool $@
    (
	echo "#############################"
	echo "Expected output for '$h5tool $@'" 
	echo "#############################"
	cd $srcdir/testfiles
        $RUNSERIAL $h5tool_bin "$@"
    ) >$actual 2>& 1
    
    if $CMP $expect $actual; then
	echo " PASSED"
    else
	echo "*FAILED*"
	echo "    Expected result (*.ddl) differs from actual result (*.out)"
	nerrors="`expr $nerrors + 1`"
	test yes = "$verbose" && $DIFF $expect $actual |sed 's/^/    /'
    fi

    # Clean up output file
    if [ X = ${HDF5_NOCLEANUP:-X} ]; then
	rm -f $actual
    fi
}



##############################################################################
##############################################################################
###			  T H E   T E S T S                                ###
##############################################################################
##############################################################################

# test for displaying groups
TOOLTEST tgroup-1.ddl tgroup.h5
# test for displaying the selected groups
TOOLTEST tgroup-2.ddl -g /g2 -g / -g /y tgroup.h5

# test for displaying simple space datasets
TOOLTEST tdset-1.ddl tdset.h5
# test for displaying selected datasets
TOOLTEST tdset-2.ddl -H -d dset1 -d /dset2 --dataset=dset3 tdset.h5

# test for displaying attributes
TOOLTEST tattr-1.ddl tattr.h5
# test for displaying the selected attributes of string type and scalar space
TOOLTEST tattr-2.ddl -a attr1 --attribute attr4 --attribute=attr5 tattr.h5
# test for header and error messages
TOOLTEST tattr-3.ddl --header -a attr2 --attribute=attr tattr.h5

# test for displaying soft links
TOOLTEST tslink-1.ddl tslink.h5
# test for displaying the selected link
TOOLTEST tslink-2.ddl -l slink2 tslink.h5

# tests for hard links
TOOLTEST thlink-1.ddl thlink.h5
TOOLTEST thlink-2.ddl -d /g1/dset2 --dataset /dset1 --dataset=/g1/g1.1/dset3 thlink.h5
TOOLTEST thlink-3.ddl -d /g1/g1.1/dset3 --dataset /g1/dset2 --dataset=/dset1 thlink.h5
TOOLTEST thlink-4.ddl -g /g1 thlink.h5
TOOLTEST thlink-5.ddl -d /dset1 -g /g2 -d /g1/dset2 thlink.h5

# tests for compound data types
TOOLTEST tcomp-1.ddl tcompound.h5
# test for named data types
TOOLTEST tcomp-2.ddl -t /type1 --datatype /type2 --datatype=/group1/type3 tcompound.h5
# test for unamed type 
TOOLTEST tcomp-3.ddl -t /#5992:0 -g /group2 tcompound.h5

#test for the nested compound type
TOOLTEST tnestcomp-1.ddl tnestedcomp.h5

# test for options
TOOLTEST tall-1.ddl tall.h5
TOOLTEST tall-2.ddl --header -g /g1/g1.1 -a attr2 tall.h5
TOOLTEST tall-3.ddl -d /g2/dset2.1 -l /g1/g1.2/g1.2.1/slink tall.h5

# test for loop detection
TOOLTEST tloop-1.ddl tloop.h5

# test for string 
TOOLTEST tstr-1.ddl tstr.h5
TOOLTEST tstr-2.ddl tstr2.h5

# test for file created by Lib SAF team
TOOLTEST tsaf.ddl tsaf.h5

# test for file with variable length data
TOOLTEST tvldtypes1.ddl tvldtypes1.h5
TOOLTEST tvldtypes2.ddl tvldtypes2.h5
TOOLTEST tvldtypes3.ddl tvldtypes3.h5
TOOLTEST tvldtypes4.ddl tvldtypes4.h5

# test for files with array data
TOOLTEST tarray1.ddl tarray1.h5
TOOLTEST tarray2.ddl tarray2.h5
TOOLTEST tarray3.ddl tarray3.h5
TOOLTEST tarray4.ddl tarray4.h5
TOOLTEST tarray5.ddl tarray5.h5
TOOLTEST tarray6.ddl tarray6.h5
TOOLTEST tarray7.ddl tarray7.h5

# test for files with empty data
TOOLTEST tempty.ddl tempty.h5

if test $nerrors -eq 0 ; then
	echo "All $h5tool tests passed."
fi

exit $nerrors
