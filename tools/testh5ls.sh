#!/bin/sh
# Tests for the h5ls tool

h5tool=h5ls			# The tool name
h5tool_bin=`pwd`/$h5tool	# The path of the tool binary
CMP='cmp -s'
DIFF='diff -c'
NLINES=20			# Max. lines of output to display if test fails

nerrors=0
verbose=yes

# The build (current) directory might be different than the source directory.
if test "X$srcdir" = X; then
    srcdir=.
fi
mkdir testfiles >/dev/null 2>&1

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Testing".
TESTING()
{
    SPACES="                                                               "
    echo "Testing $* $SPACES" |cut -c1-70 |tr -d '\012'
}

# Run a test and print PASS or *FAIL*. For now, if h5ls can complete
# with exit status 0, consider it pass. If a test fails then increment
# the `nerrors' global variable and (if $verbose is set) display up to $NLINS
# lines of the actual output from the tool test.  The actual output is not
# removed if $HDF5_NOCLEANUP has a non-zero value.
# Arguemnts:
# $1 -- actual output filename to use
# $2 and on -- argument for the h5ls tool
TOOLTEST()
{
    actual="testfiles/$1"
    shift

    # Run test.
    TESTING $h5tool $@
    (
	echo "#############################"
	echo " output for '$h5tool $@'" 
	echo "#############################"
	cd $srcdir/testfiles
        $RUNSERIAL $h5tool_bin "$@"
    ) >$actual 2>& 1
    
    exitcode=$?
    if [ $exitcode -eq 0 ]; then
	echo " PASSED"
    else
	echo "*FAILED*"
	nerrors="`expr $nerrors + 1`"
	if [ yes = "$verbose" ]; then
	    echo "test returned with exit code $exitcode"
	    echo "test output: (up to $NLINES lines)"
	    head -$NLINES $actual
	    echo "***end of test output***"
	    echo ""
	fi
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

# Toss in a bunch of tests.  Not sure if they are the right kinds.
# test the help syntax
TOOLTEST help.ls -h
TOOLTEST help.ls -help
TOOLTEST help.ls -?

# test simple command
TOOLTEST tall-1.ls tall.h5
TOOLTEST tall-2.ls -r -d tall.h5
TOOLTEST tgroup.ls tgroup.h5

# test for displaying groups
TOOLTEST tgroup-1.ls  -r -g tgroup.h5

# test for displaying simple space datasets
TOOLTEST tdset-1.ls -r -d tdset.h5

# test for displaying soft links
TOOLTEST tslink-1.ls -r tslink.h5

# tests for hard links
TOOLTEST thlink-1.ls thlink.h5

# tests for compound data types
TOOLTEST tcomp-1.ls -r -d tcompound.h5

#test for the nested compound type
TOOLTEST tnestcomp-1.ls -r -d tnestedcomp.h5

# test for loop detection
TOOLTEST tloop-1.ls -r -d tloop.h5

# test for string 
TOOLTEST tstr-1.ls -r -d tstr.h5

# test test file created from lib SAF team
TOOLTEST tsaf.ls -r -d tsaf.h5

if test $nerrors -eq 0 ; then
	echo "All $h5tool tests passed."
fi

exit $nerrors
