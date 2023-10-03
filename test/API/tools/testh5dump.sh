#! /bin/sh
#
# Copyright by The HDF Group.
# Copyright by the Board of Trustees of the University of Illinois.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#
# Tests for the h5dump tool

# Assume we are on a little-endian system
WORDS_BIGENDIAN="no"

TESTNAME=h5dump
EXIT_SUCCESS=0
EXIT_FAILURE=1

H5DUMP=h5dump               # The h5dump tool name
H5DELETE='h5delete -f'      # The h5delete tool name
H5REPACK=h5repack           # The h5repack tool name

RM='rm -rf'
CMP='cmp'
DIFF='diff -c'
GREP='grep'
CP='cp'
DIRNAME='dirname'
LS='ls'
AWK='awk'

nerrors=0
verbose=yes

######################################################################
# Input files
# --------------------------------------------------------------------

# Where the tool's HDF5 input files are located
TESTFILES_HDF5_DIR="./testfiles/hdf5"

# Where the tool's expected output files are located
H5DUMP_TESTFILES_OUT_DIR="./testfiles/expected/h5dump"

######################################################################
# Output files
# --------------------------------------------------------------------

# Where the text output goes
TEXT_OUTPUT_DIR=./h5dump_test_output

# Where the repacked HDF5 input files go
REPACK_OUTPUT_DIR=./h5dump_repack_output

######################################################################
# test files
# --------------------------------------------------------------------

# HDF5 test files.
#
# Kept in       $H5REPACK_TESTFILES_HDF5_DIR
# Repacked to   $REPACK_OUTPUT_DIR
#
# These files fail to repack w/ native VOL (investigate later)
# tall.h5
# tattrreg.h5 <-- This one does not even cause h5repack to fail!
# tbigdims.h5 (repack hangs w/ passthru VOL)
# err_attr_dspace.h5
# tfcontents1.h5
# tfcontents2.h5
#
# These files have native-specific content
# tfamily00000.h5
# tfamily00001.h5
# tfamily00002.h5
# tfamily00003.h5
# tfamily00004.h5
# tfamily00005.h5
# tfamily00006.h5
# tfamily00007.h5
# tfamily00008.h5
# tfamily00009.h5
# tfamily00010.h5
# tfilters.h5
# filter_fail.h5 (can't be repacked w/o missing filter)
# tgrp_comments.h5
# tmulti-b.h5
# tmulti-g.h5
# tmulti-l.h5
# tmulti-o.h5
# tmulti-r.h5
# tmulti-s.h5
# tsplit_file-m.h5
# tsplit_file-r.h5
#
# Link files require an h5repack H5Lcopy work-around
# textlink.h5
# textlinksrc.h5
# textlinktar.h5
# tslink.h5
# tsoftlinks.h5
# tudlink.h5
#
# These files have problems with DAOS, usually due to old-school references
# tarray1_big.h5
# tattr2.h5
# tdatareg.h5
# thlink.h5     This appears to have a cycle that causes an eventual segfault.
# tloop.h5      Ditto on the cycle problem
# torderattr.h5
# tordergr.h5
# tnestedcmpddt.h5  Basically identical output, but type description text differs enough to trigger a problem
# tvlstr.h5         As above
HDF5_FILES="
h5copytst.h5
charsets.h5
file_space.h5
packedbits.h5
t128bit_float.h5
taindices.h5
tarray1.h5
tarray2.h5
tarray3.h5
tarray4.h5
tarray5.h5
tarray6.h5
tarray7.h5
tarray8.h5
tattr4_be.h5
tattr.h5
tattrintsize.h5
tbinary.h5
tbitnopaque.h5
tchar.h5
tcmpdattrintsize.h5
tcmpdintsize.h5
tcompound_complex2.h5
tcompound_complex.h5
tcompound.h5
tdset.h5
tempty.h5
tfpformat.h5
tfvalues.h5
tgroup.h5
tgrp_comments.h5
tgrpnullspace.h5
thyperslab.h5
tints4dims.h5
tintsattrs.h5
tlarge_objname.h5
tldouble.h5
tnamed_dtype_attr.h5
tnestedcomp.h5
tno-subset.h5
tnullspace.h5
tsaf.h5
tscalarattrintsize.h5
tscalarintattrsize.h5
tscalarintsize.h5
tscalarstring.h5
tstr2.h5
tstr3.h5
tstr.h5
tvldtypes1.h5
tvldtypes2.h5
tvldtypes3.h5
tvldtypes4.h5
tvldtypes5.h5
tvlenstr_array.h5
tvms.h5
zerodim.h5
"

# Expected output files.
#
# Kept in       $H5DUMP_TESTFILES_OUT_DIR
# Copied to     $TEXT_OUTPUT_DIR
#
# NOTE: This is ALL the files - they have not been culled based on the HDF5
#       files in the above list.
#
EXPECTED_OUTPUT_FILES="
charsets.ddl
err_attr_dspace.ddl
file_space.ddl
filter_fail.ddl
filter_fail.err
h5dump-help.txt
non_existing.ddl
non_existing.err
out3.h5import
packedbits.ddl
tall-1.ddl
tall-1.err
tall-2A0.ddl
tall-2A0.err
tall-2A.ddl
tall-2A.err
tall-2B.ddl
tall-2B.err
tall-2.ddl
tall-3.ddl
tall-4s.ddl
tall-5s.ddl
tall-6.ddl
tall-6.exp
tall-7.ddl
tall-7N.ddl
tallfilters.ddl
tarray1_big.ddl
tarray1_big.err
tarray1.ddl
tarray2.ddl
tarray3.ddl
tarray4.ddl
tarray5.ddl
tarray6.ddl
tarray7.ddl
tarray8.ddl
tattr-1.ddl
tattr-2.ddl
tattr-3.ddl
tattr-3.err
tattr-4_be.ddl
tattrcontents1.ddl
tattrcontents2.ddl
tattrintsize.ddl
tattrreg.ddl
tattrregR.ddl
tattrregR.err
tbigdims.ddl
tbin1.ddl
tbin1.ddl
tbin2.ddl
tbin3.ddl
tbin4.ddl
tbinregR.ddl
tbinregR.exp
tbitnopaque_be.ddl
tbitnopaque_le.ddl
tboot1.ddl
tboot2A.ddl
tboot2B.ddl
tboot2.ddl
tchar1.ddl
tchunked.ddl
tcmpdattrintsize.ddl
tcmpdintsize.ddl
tcomp-1.ddl
tcomp-2.ddl
tcomp-3.ddl
tcomp-3.err
tcomp-4.ddl
tcompact.ddl
tcompound_complex2.ddl
tcontents.ddl
tcontiguos.ddl
tdatareg.ddl
tdataregR.ddl
tdataregR.err
tdeflate.ddl
tdset-1.ddl
tdset-2.ddl
tdset-2.err
tdset-3s.ddl
tempty.ddl
texceedsubblock.ddl
texceedsubblock.err
texceedsubcount.ddl
texceedsubcount.err
texceedsubstart.ddl
texceedsubstart.err
texceedsubstride.ddl
texceedsubstride.err
texternal.ddl
textlink.ddl
textlink.err
textlinkfar.ddl
textlinkfar.err
textlinksrc.ddl
textlinksrc.err
tfamily.ddl
tfill.ddl
tfletcher32.ddl
tfpformat.ddl
tgroup-1.ddl
tgroup-2.ddl
tgroup-2.err
tgrp_comments.ddl
tgrpnullspace.ddl
thlink-1.ddl
thlink-2.ddl
thlink-3.ddl
thlink-4.ddl
thlink-5.ddl
thyperslab.ddl
tindicesno.ddl
tindicessub1.ddl
tindicessub2.ddl
tindicessub3.ddl
tindicessub4.ddl
tindicesyes.ddl
tints4dimsBlock2.ddl
tints4dimsBlockEq.ddl
tints4dimsCount2.ddl
tints4dimsCountEq.ddl
tints4dims.ddl
tints4dimsStride2.ddl
tintsattrs.ddl
tlarge_objname.ddl
tlonglinks.ddl
tloop-1.ddl
tmulti.ddl
tmultifile.ddl
tnamed_dtype_attr.ddl
tnbit.ddl
tnestcomp-1.ddl
tnestedcmpddt.ddl
tnoattrdata.ddl
tnoattrddl.ddl
tnodata.ddl
tnoddl.ddl
tnoddlfile.ddl
tnoddlfile.exp
tno-subset.ddl
tnullspace.ddl
torderattr1.ddl
torderattr2.ddl
torderattr3.ddl
torderattr4.ddl
tordercontents1.ddl
tordercontents2.ddl
tordergr1.ddl
tordergr2.ddl
tordergr3.ddl
tordergr4.ddl
tordergr5.ddl
torderlinks1.ddl
torderlinks1.err
torderlinks2.ddl
torderlinks2.err
tperror.ddl
tperror.err
tqmarkfile.ddl
tqmarkfile.err
trawdatafile.ddl
trawdatafile.exp
trawssetfile.ddl
trawssetfile.exp
treadfilter.ddl
treadintfilter.ddl
treference.ddl
tsaf.ddl
tscalarattrintsize.ddl
tscalarintattrsize.ddl
tscalarintsize.ddl
tscalarstring.ddl
tscaleoffset.ddl
tshuffle.ddl
tslink-1.ddl
tslink-2.ddl
tslink-D.ddl
tslink-D.err
tsplit_file.ddl
tstarfile.ddl
tstr-1.ddl
tstr2bin2.exp
tstr2bin6.exp
tstr-2.ddl
tstring2.ddl
tstring.ddl
tstringe.ddl
tszip.ddl
tudlink-1.ddl
tudlink-2.ddl
tuserfilter.ddl
tvldtypes1.ddl
tvldtypes2.ddl
tvldtypes3.ddl
tvldtypes4.ddl
tvldtypes5.ddl
tvlenstr_array.ddl
tvlstr.ddl
tvms.ddl
twidedisplay.ddl
twithddl.exp
twithddlfile.ddl
twithddlfile.exp
zerodim.ddl
"

######################################################################
# Utility functions
# --------------------------------------------------------------------

# Copy the expected text output files to the text output directory
# to make it easier to diff the expected and actual output.
#
COPY_EXPECTED_OUTPUT_FILES()
{
    for outfile in $EXPECTED_OUTPUT_FILES
    do
        filepath="$H5DUMP_TESTFILES_OUT_DIR/$outfile"

        # Use -f to make sure get a new copy
        $CP -f $filepath $TEXT_OUTPUT_DIR
        if [ $? -ne 0 ]; then
            echo "Error: FAILED to copy expected output file: $filepath ."
        fi
    done
}

# Repack the HDF5 files to the repack directory.
#
REPACK_HDF5_FILES()
{
    for repackfile in $HDF5_FILES
    do
        inpath="$TESTFILES_HDF5_DIR/$repackfile"
        outpath="$REPACK_OUTPUT_DIR/$repackfile"

        # Repack the file
        $H5REPACK --src-vol-name=native --enable-error-stack $inpath $outpath
        if [ $? -ne 0 ]; then
            echo "Error: FAILED to repack HDF5 file: $inpath ."
        fi
    done
}

# Cleans up HDF5 and text output from the tests. Only cleans if the
# HDF5_NOCLEANUP variable is unset.
#
CLEAN_OUTPUT()
{
    # Remove output if the "no cleanup" environment variable is not defined
    if test -z "$HDF5_NOCLEANUP"; then
        # Text output
        $RM $TEXT_OUTPUT_DIR

        # HDF5 output
        #
        # Can't just rm -rf the directory if the VOL storage doesn't map to
        # a normal file, so we'll use h5delete to delete the file.
        for hdf5file in $HDF5_FILES
        do
            filepath="$REPACK_OUTPUT_DIR/$hdf5file"
            $H5DELETE $filepath
        done

        # The HDF5 output directory is always created, even if the VOL
        # storage won't use it. Delete it here.
        $RM $REPACK_OUTPUT_DIR
   fi
}

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Testing".
#
TESTING() {
   SPACES="                                                               "
   echo "Testing $* $SPACES" | cut -c1-70 | tr -d '\012'
}

# Source in the output filter function definitions.
. ./output_filter.sh

######################################################################
# Main testing functions
# --------------------------------------------------------------------

# Run a test and print PASS or *FAIL*.  If a test fails then increment
# the `nerrors' global variable and (if $verbose is set) display the
# difference between the actual output and the expected output. The
# expected output is given as the first argument to this function and
# the actual output file is calculated by replacing the `.ddl' with
# `.out'.  The actual output is not removed if $HDF5_NOCLEANUP has a
# non-zero value.
# If $1 == ignorecase then do caseless CMP and DIFF.
# ADD_H5_TEST
RUNTEST() {
    # check if caseless compare and diff requested
    if [ "$1" = ignorecase ]; then
    caseless="-i"
    # replace cmp with diff which runs much longer.
    xCMP="$DIFF -i"
    shift
    else
    caseless=""
    # stick with faster cmp if ignorecase is not requested.
    xCMP="$CMP"
    fi

    expect="$TEXT_OUTPUT_DIR/$1"
    actual="$TEXT_OUTPUT_DIR/`basename $1 .ddl`.out"
    actual_err="$TEXT_OUTPUT_DIR/`basename $1 .ddl`.err"
    actual_sav=${actual}-sav
    actual_err_sav=${actual_err}-sav
    shift

    # Run test.
    TESTING $H5DUMP $@
    (
      $RUNSERIAL $H5DUMP "$@"
    ) >$actual 2>$actual_err

    # save actual and actual_err in case they are needed later.
    cp $actual $actual_sav
    STDOUT_FILTER $actual
    cp $actual_err $actual_err_sav
    STDERR_FILTER $actual_err

    # Clean h5dump stdout files
    H5DUMP_FILTER $actual

    # Strip the HDF5 output directory name from the output file
    # (use | to avoid sed and directory delimiter clash)
    cp $actual $tmp_file
    sed "s|$REPACK_OUTPUT_DIR/||" < $tmp_file > $actual

    if [ ! -f $expect ]; then
        # Create the expect file if it doesn't yet exist.
        echo " CREATED"
        cp $actual $expect
        echo "    Expected result (*.ddl) missing"
        nerrors="`expr $nerrors + 1`"
    elif $xCMP $expect $actual > /dev/null 2>&1 ; then
        echo " PASSED"
    else
        echo "*FAILED*"
        echo "    Expected result (*.ddl) differs from actual result (*.out)"
        nerrors="`expr $nerrors + 1`"
        test yes = "$verbose" && $DIFF $caseless $expect $actual |sed 's/^/    /'
    fi
}


# same as RUNTEST but compares generated file to expected output
# and compares the generated data file to the expected data file
# used for the binary tests that expect a full path in -o without -b
# ADD_H5_EXPORT_TEST
RUNTEST2() {

    expectdata="$TEXT_OUTPUT_DIR/$1"
    expect="$TEXT_OUTPUT_DIR/`basename $1 .exp`.ddl"
    actualdata="$TEXT_OUTPUT_DIR/`basename $1 .exp`.txt"
    actual="$TEXT_OUTPUT_DIR/`basename $1 .exp`.out"
    actual_err="$TEXT_OUTPUT_DIR/`basename $1 .exp`.err"
    shift

    # Run test.
    TESTING $H5DUMP $@
    (
        $RUNSERIAL $H5DUMP "$@"
    ) >$actual 2>$actual_err

    # Strip the HDF5 output directory name from the output file
    # (use | to avoid sed and directory delimiter clash)
    cp $actual $tmp_file
    sed "s|$REPACK_OUTPUT_DIR/||" < $tmp_file > $actual

    if [ ! -f $expect ]; then
        # Create the expect file if it doesn't yet exist.
        echo " CREATED"
        $CP $actual $expect
        echo "    Expected result (*.ddl) missing"
        nerrors="`expr $nerrors + 1`"
    elif $CMP $expect $actual; then
        if [ ! -f $expectdata ]; then
            # Create the expect data file if it doesn't yet exist.
            echo " CREATED"
            $CP $actualdata $expectdata
            echo "    Expected data (*.exp) missing"
            nerrors="`expr $nerrors + 1`"
        elif $CMP $expectdata $actualdata; then
            echo " PASSED"
        else
            echo "*FAILED*"
            echo "    Expected datafile (*.exp) differs from actual datafile (*.txt)"
            nerrors="`expr $nerrors + 1`"
            test yes = "$verbose" && $DIFF $expectdata $actualdata |sed 's/^/    /'
        fi
    else
        echo "*FAILED*"
        echo "    Expected result (*.ddl) differs from actual result (*.out)"
        nerrors="`expr $nerrors + 1`"
        test yes = "$verbose" && $DIFF $expect $actual |sed 's/^/    /'
    fi
}

# same as RUNTEST2 but compares generated file to expected ddl file
# and compares the generated data file to the expected data file
# used for the binary tests that expect a full path in -o without -b
# ADD_H5_TEST_EXPORT
RUNTEST2A() {

    expectdata="$TEXT_OUTPUT_DIR/$1"
    expect="$TEXT_OUTPUT_DIR/`basename $1 .exp`.ddl"
    actualdata="$TEXT_OUTPUT_DIR/`basename $1 .exp`.txt"
    actual="$TEXT_OUTPUT_DIR/`basename $1 .exp`.out"
    actual_err="$TEXT_OUTPUT_DIR/`basename $1 .exp`.err"
    shift
    expectmeta="$TEXT_OUTPUT_DIR/$1"
    actualmeta="$TEXT_OUTPUT_DIR/`basename $1 .exp`.txt"
    shift

    # Run test.
    TESTING $H5DUMP $@
    (
        $RUNSERIAL $H5DUMP "$@"
    ) >$actual 2>$actual_err

    # Strip the HDF5 output directory name from the output file
    # (use | to avoid sed and directory delimiter clash)
    cp $actual $tmp_file
    sed "s|$REPACK_OUTPUT_DIR/||" < $tmp_file > $actual
    cp $actualdata $tmp_file
    sed "s|$REPACK_OUTPUT_DIR/||" < $tmp_file > $actualdata
    cp $actualmeta $tmp_file
    sed "s|$REPACK_OUTPUT_DIR/||" < $tmp_file > $actualmeta

    if [ ! -f $expect ]; then
        # Create the expect file if it doesn't yet exist.
        echo " CREATED"
        cp $actual $expect
        echo "    Expected result (*.ddl) missing"
        nerrors="`expr $nerrors + 1`"
    elif $CMP $expect $actual; then
        if [ ! -f $expectdata ]; then
            # Create the expect data file if it doesn't yet exist.
            echo " CREATED"
            cp $actualdata $expectdata
            echo "    Expected data (*.exp) missing"
            nerrors="`expr $nerrors + 1`"
        elif $DIFF $expectdata $actualdata; then
            if [ ! -f $expectmeta ]; then
                # Create the expect meta file if it doesn't yet exist.
                echo " CREATED"
                cp $actualmeta $expectmeta
                echo "    Expected metafile (*.ddl) missing"
                nerrors="`expr $nerrors + 1`"
            elif $CMP $expectmeta $actualmeta; then
                echo " PASSED"
            else
                echo "*FAILED*"
                echo "    Expected metafile (*.ddl) differs from actual metafile (*.txt)"
                nerrors="`expr $nerrors + 1`"
                test yes = "$verbose" && $DIFF $expectmeta $actualmeta |sed 's/^/    /'
            fi
        else
            echo "*FAILED*"
            echo "    Expected datafile (*.exp) differs from actual datafile (*.txt)"
            nerrors="`expr $nerrors + 1`"
            test yes = "$verbose" && $DIFF $expectdata $actualdata |sed 's/^/    /'
        fi
    else
        echo "*FAILED*"
        echo "    Expected result (*.ddl) differs from actual result (*.out)"
        nerrors="`expr $nerrors + 1`"
        test yes = "$verbose" && $DIFF $expect $actual |sed 's/^/    /'
    fi
}

# same as RUNTEST2 but only compares the generated data file to the expected data file
# used for the binary tests that expect a full path in -o with -b
# ADD_H5_EXPORT_TEST
RUNTEST2B() {

    expectdata="$TEXT_OUTPUT_DIR/$1"
    actualdata="$TEXT_OUTPUT_DIR/`basename $1 .exp`.txt"
    actual="$TEXT_OUTPUT_DIR/`basename $1 .exp`.out"
    actual_err="$TEXT_OUTPUT_DIR/`basename $1 .exp`.err"
    shift

    # Run test.
    TESTING $H5DUMP $@
    (
        $RUNSERIAL $H5DUMP "$@"
    ) >$actual 2>$actual_err

    # Strip the HDF5 output directory name from the output file
    # (use | to avoid sed and directory delimiter clash)
    cp $actual $tmp_file
    sed "s|$REPACK_OUTPUT_DIR/||" < $tmp_file > $actual

    if [ ! -f $expectdata ]; then
        # Create the expect data file if it doesn't yet exist.
        echo " CREATED"
        cp $actualdata $expectdata
        echo "    Expected data (*.exp) missing"
        nerrors="`expr $nerrors + 1`"
    elif $CMP $expectdata $actualdata; then
        echo " PASSED"
    else
        echo "*FAILED*"
        echo "    Expected datafile (*.exp) differs from actual datafile (*.txt)"
        nerrors="`expr $nerrors + 1`"
        test yes = "$verbose" && $DIFF $expectdata $actualdata |sed 's/^/    /'
    fi
}

RUNTEST_HELP() {

    expect="$TEXT_OUTPUT_DIR/$1"
    actual="$TEXT_OUTPUT_DIR/`basename $1 .txt`.out"
    actual_err="$TEXT_OUTPUT_DIR/`basename $1 .txt`.err"
    shift

    # Run test.
    TESTING $H5DUMP $@
    (
        $RUNSERIAL $H5DUMP "$@"
    ) >$actual 2>$actual_err

    if [ ! -f $expectdata ]; then
        # Create the expect data file if it doesn't yet exist.
        echo " CREATED"
        cp $actual $expect-CREATED
        echo "    Expected output (*.txt) missing"
        nerrors="`expr $nerrors + 1`"
    elif $CMP $expect $actual; then
        echo " PASSED"
    else
        echo "*FAILED*"
        echo "    Expected output (*.txt) differs from actual output (*.out)"
        nerrors="`expr $nerrors + 1`"
    fi
}

# Call the h5dump tool and grep for a value
# txttype ERRTXT greps test error output, otherwise greps test output
RUNTEST_GREP()
{
    txttype=$1
    expectdata=$2
    actual=$TEXT_OUTPUT_DIR/$3
    actual_err="$TEXT_OUTPUT_DIR/`basename $3 .ddl`.oerr"
    shift
    shift
    shift

    # Run test.
    TESTING $H5DUMP -p $@
    (
      $ENVCMD $RUNSERIAL $H5DUMP -p "$@"
    ) >$actual 2>$actual_err
    if [ "$txttype" = "ERRTXT" ]; then
        $GREP "$expectdata" $actual_err > /dev/null
    else
        $GREP "$expectdata" $actual > /dev/null
    fi
    if [ $? -eq 0 ]; then
        echo " PASSED"
    else
        echo " FAILED"
        nerrors="`expr $nerrors + 1`"
    fi
}

# Call the h5dump tool and grep for a value but disables plugin filter loading
# txttype ERRTXT greps test error output, otherwise greps test output
RUNTEST_GREP2()
{
    txttype=$1
    expectdata=$2
    actual=$TEXT_OUTPUT_DIR/$3
    actual_err="$TEXT_OUTPUT_DIR/`basename $3 .ddl`.oerr"
    shift
    shift
    shift

    # Run test.
    TESTING $H5DUMP -p $@
    (
      $ENVCMD $RUNSERIAL $H5DUMP -p "$@"
    ) >$actual 2>$actual_err
    if [ "$txttype" = "ERRTXT" ]; then
        $GREP "$expectdata" $actual_err > /dev/null
    else
        $GREP "$expectdata" $actual > /dev/null
    fi
    if [ $? -eq 0 ]; then
        echo " PASSED"
    else
        echo " FAILED"
        nerrors="`expr $nerrors + 1`"
    fi
}


##############################################################################
##############################################################################
###        T H E   T E S T S                                               ###
##############################################################################
##############################################################################
# Prepare for test
#
# We create the repack output dir in case it's needed. If a VOL connector doesn't
# use normal files, it'll just stay empty and get deleted later.
CLEAN_OUTPUT
test -d $TEXT_OUTPUT_DIR || mkdir -p $TEXT_OUTPUT_DIR
test -d $REPACK_OUTPUT_DIR || mkdir -p $REPACK_OUTPUT_DIR
COPY_EXPECTED_OUTPUT_FILES
REPACK_HDF5_FILES


# Run h5dump tests
#
# NOTE: In the official HDF5 library tools tests, the emitted error stack
#       output is checked against canonical error stack output. This is too
#       brittle to be done with arbitrary VOL connectors, so all of the tests
#       which do that now solely test normal output.
#
#       The tests are still run with --enable-error-stack, however, in case
#       that helps with debugging issues.

# test the help syntax
RUNTEST_HELP h5dump-help.txt -h

# test data output redirection
RUNTEST tnoddl.ddl --enable-error-stack --ddl -y $REPACK_OUTPUT_DIR/packedbits.h5
RUNTEST tnodata.ddl --enable-error-stack --output $REPACK_OUTPUT_DIR/packedbits.h5
RUNTEST tnoattrddl.ddl --enable-error-stack -O -y $REPACK_OUTPUT_DIR/tattr.h5
RUNTEST tnoattrdata.ddl --enable-error-stack -A -o $REPACK_OUTPUT_DIR/tattr.h5
RUNTEST2 trawdatafile.exp --enable-error-stack -y -o $TEXT_OUTPUT_DIR/trawdatafile.txt $REPACK_OUTPUT_DIR/packedbits.h5
RUNTEST2 tnoddlfile.exp --enable-error-stack -O -y -o $TEXT_OUTPUT_DIR/tnoddlfile.txt $REPACK_OUTPUT_DIR/packedbits.h5
RUNTEST2A twithddlfile.exp twithddl.exp --enable-error-stack --ddl=$TEXT_OUTPUT_DIR/twithddl.txt -y -o $TEXT_OUTPUT_DIR/twithddlfile.txt $REPACK_OUTPUT_DIR/packedbits.h5
RUNTEST2 trawssetfile.exp --enable-error-stack -d "/dset1[1,1;;;]" -y -o $TEXT_OUTPUT_DIR/trawssetfile.txt $REPACK_OUTPUT_DIR/tdset.h5

# test for maximum display datasets
RUNTEST twidedisplay.ddl --enable-error-stack -w0 $REPACK_OUTPUT_DIR/packedbits.h5

# test for signed/unsigned datasets
RUNTEST packedbits.ddl --enable-error-stack $REPACK_OUTPUT_DIR/packedbits.h5
# test for compound signed/unsigned datasets
RUNTEST tcmpdintsize.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tcmpdintsize.h5
# test for signed/unsigned scalar datasets
RUNTEST tscalarintsize.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tscalarintsize.h5
# test for signed/unsigned attributes
RUNTEST tattrintsize.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tattrintsize.h5
# test for compound signed/unsigned attributes
RUNTEST tcmpdattrintsize.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tcmpdattrintsize.h5
# test for signed/unsigned scalar attributes
RUNTEST tscalarattrintsize.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tscalarattrintsize.h5
# test for signed/unsigned scalar datasets with attributes
RUNTEST tscalarintattrsize.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tscalarintattrsize.h5
# test for signed/unsigned datasets attributes
RUNTEST tintsattrs.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tintsattrs.h5
# test for string scalar dataset attribute
RUNTEST tscalarstring.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tscalarstring.h5
# test for displaying groups
RUNTEST tgroup-1.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tgroup.h5
# test for displaying the selected groups
RUNTEST tgroup-2.ddl --enable-error-stack --group=/g2 --group / -g /y $REPACK_OUTPUT_DIR/tgroup.h5

# test for displaying simple space datasets
RUNTEST tdset-1.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tdset.h5
# test for displaying selected datasets
RUNTEST tdset-2.ddl --enable-error-stack -H -d dset1 -d /dset2 --dataset=dset3 $REPACK_OUTPUT_DIR/tdset.h5

# test for displaying attributes
RUNTEST tattr-1.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tattr.h5
# test for displaying the selected attributes of string type and scalar space
RUNTEST tattr-2.ddl --enable-error-stack -a "/\/attr1" --attribute /attr4 --attribute=/attr5 $REPACK_OUTPUT_DIR/tattr.h5
RUNTEST tattr-2.ddl --enable-error-stack -N "/\/attr1" --any_path /attr4 --any_path=/attr5 $REPACK_OUTPUT_DIR/tattr.h5
# test for header and error messages
RUNTEST tattr-3.ddl --enable-error-stack --header -a /attr2 --attribute=/attr $REPACK_OUTPUT_DIR/tattr.h5
# test for displaying at least 9 attributes on root from a BE machine
RUNTEST tattr-4_be.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tattr4_be.h5
# test for displaying attributes in shared datatype (also in group and dataset)
# TODO: This test looks like it misses a hard link to a named datatype. Might be an issue with the h5repack H5Ocopy work-around.
#RUNTEST tnamed_dtype_attr.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tnamed_dtype_attr.h5

# test for displaying soft links and user-defined links
#RUNTEST tslink-1.ddl --enable-error-stack tslink.h5
#RUNTEST tudlink-1.ddl --enable-error-stack tudlink.h5
# test for displaying the selected link
#RUNTEST tslink-2.ddl --enable-error-stack -l slink2 tslink.h5
#RUNTEST tslink-2.ddl --enable-error-stack -N slink2 tslink.h5
#RUNTEST tudlink-2.ddl --enable-error-stack -l udlink2 tudlink.h5
# test for displaying dangling soft links
#RUNTEST tslink-D.ddl --enable-error-stack -d /slink1 tslink.h5

# tests for hard links
#RUNTEST thlink-1.ddl --enable-error-stack $REPACK_OUTPUT_DIR/thlink.h5
#RUNTEST thlink-2.ddl --enable-error-stack -d /g1/dset2 --dataset /dset1 --dataset=/g1/g1.1/dset3 $REPACK_OUTPUT_DIR/thlink.h5
#RUNTEST thlink-3.ddl --enable-error-stack -d /g1/g1.1/dset3 --dataset /g1/dset2 --dataset=/dset1 $REPACK_OUTPUT_DIR/thlink.h5
#RUNTEST thlink-4.ddl --enable-error-stack -g /g1 $REPACK_OUTPUT_DIR/thlink.h5
#RUNTEST thlink-4.ddl --enable-error-stack -N /g1 $REPACK_OUTPUT_DIR/thlink.h5
#RUNTEST thlink-5.ddl --enable-error-stack -d /dset1 -g /g2 -d /g1/dset2 $REPACK_OUTPUT_DIR/thlink.h5
#RUNTEST thlink-5.ddl --enable-error-stack -N /dset1 -N /g2 -N /g1/dset2 $REPACK_OUTPUT_DIR/thlink.h5

# tests for compound data types
# Looks like the unnamed type number is different after repacking?
#RUNTEST tcomp-1.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tcompound.h5
# test for named data types
RUNTEST tcomp-2.ddl --enable-error-stack -t /type1 --datatype /type2 --datatype=/group1/type3 $REPACK_OUTPUT_DIR/tcompound.h5
RUNTEST tcomp-2.ddl --enable-error-stack -N /type1 --any_path /type2 --any_path=/group1/type3 $REPACK_OUTPUT_DIR/tcompound.h5
# test for unnamed type
# Looks like the unnamed type number is different after repacking?
#RUNTEST tcomp-3.ddl --enable-error-stack -t /#6632 -g /group2 $REPACK_OUTPUT_DIR/tcompound.h5
# test complicated compound datatype
RUNTEST tcomp-4.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tcompound_complex.h5
RUNTEST tcompound_complex2.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tcompound_complex2.h5
# tests for bitfields and opaque data types
if test $WORDS_BIGENDIAN != "yes"; then
RUNTEST tbitnopaque_le.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tbitnopaque.h5
else
RUNTEST tbitnopaque_be.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tbitnopaque.h5
fi

#test for the nested compound type
RUNTEST tnestcomp-1.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tnestedcomp.h5
#RUNTEST tnestedcmpddt.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tnestedcmpddt.h5

# test for options (TODO: tall does not repack)
#RUNTEST tall-1.ddl --enable-error-stack tall.h5
#RUNTEST tall-2.ddl --enable-error-stack --header -g /g1/g1.1 -a attr2 tall.h5
#RUNTEST tall-3.ddl --enable-error-stack -d /g2/dset2.1 -l /g1/g1.2/g1.2.1/slink tall.h5
#RUNTEST tall-3.ddl --enable-error-stack -N /g2/dset2.1 -N /g1/g1.2/g1.2.1/slink tall.h5
#RUNTEST tall-7.ddl --enable-error-stack -a attr1 tall.h5
#RUNTEST tall-7N.ddl --enable-error-stack -N attr1 tall.h5

# test for loop detection
#RUNTEST tloop-1.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tloop.h5

# test for string
RUNTEST tstr-1.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tstr.h5
RUNTEST tstr-2.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tstr2.h5

# test for file created by Lib SAF team
RUNTEST tsaf.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tsaf.h5

# test for file with variable length data
RUNTEST tvldtypes1.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tvldtypes1.h5
RUNTEST tvldtypes2.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tvldtypes2.h5
RUNTEST tvldtypes3.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tvldtypes3.h5
RUNTEST tvldtypes4.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tvldtypes4.h5
RUNTEST tvldtypes5.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tvldtypes5.h5

#test for file with variable length string data
#RUNTEST tvlstr.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tvlstr.h5
RUNTEST tvlenstr_array.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tvlenstr_array.h5

# test for files with array data
RUNTEST tarray1.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tarray1.h5
# # added for bug# 2092 - tarray1_big.h
#RUNTEST_GREP ERRTXT "NULL token size" tarray1_big.ddl --enable-error-stack -R $REPACK_OUTPUT_DIR/tarray1_big.h5
RUNTEST tarray2.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tarray2.h5
RUNTEST tarray3.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tarray3.h5
RUNTEST tarray4.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tarray4.h5
RUNTEST tarray5.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tarray5.h5
RUNTEST tarray6.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tarray6.h5
RUNTEST tarray7.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tarray7.h5
RUNTEST tarray8.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tarray8.h5

# test for wildcards in filename (does not work with cmake)
# inconsistent across platforms RUNTEST tstarfile.ddl --enable-error-stack -H -d Dataset1 tarr*.h5
#RUNTEST tqmarkfile.ddl --enable-error-stack -H -d Dataset1 tarray?.h5
RUNTEST tmultifile.ddl --enable-error-stack -H -d Dataset1 $REPACK_OUTPUT_DIR/tarray2.h5 $REPACK_OUTPUT_DIR/tarray3.h5 $REPACK_OUTPUT_DIR/tarray4.h5 $REPACK_OUTPUT_DIR/tarray5.h5 $REPACK_OUTPUT_DIR/tarray6.h5 $REPACK_OUTPUT_DIR/tarray7.h5

# test for files with empty data
RUNTEST tempty.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tempty.h5

# test for files with group names which reach > 1024 bytes in size
RUNTEST tlarge_objname.ddl --enable-error-stack -w157 $REPACK_OUTPUT_DIR/tlarge_objname.h5

# test '-A' to suppress data but print attr's
#RUNTEST tall-2A.ddl --enable-error-stack -A tall.h5

# test '-A' to suppress attr's but print data
#RUNTEST tall-2A0.ddl --enable-error-stack -A 0 tall.h5

# test '-r' to print attributes in ASCII instead of decimal
#RUNTEST tall-2B.ddl --enable-error-stack -A -r tall.h5

# test Subsetting
#RUNTEST tall-4s.ddl --enable-error-stack --dataset=/g1/g1.1/dset1.1.1 --start=1,1 --stride=2,3 --count=3,2 --block=1,1 tall.h5
#RUNTEST tall-5s.ddl --enable-error-stack -d "/g1/g1.1/dset1.1.2[0;2;10;]" tall.h5
RUNTEST tdset-3s.ddl --enable-error-stack -d "/dset1[1,1;;;]" $REPACK_OUTPUT_DIR/tdset.h5
RUNTEST tno-subset.ddl --enable-error-stack --no-compact-subset -d "AHFINDERDIRECT::ah_centroid_t[0] it=0 tl=0" $REPACK_OUTPUT_DIR/tno-subset.h5

RUNTEST tints4dimsCount2.ddl --enable-error-stack -d FourDimInts -s 0,0,0,0 -c 2,2,2,2 $REPACK_OUTPUT_DIR/tints4dims.h5
RUNTEST tints4dimsBlock2.ddl --enable-error-stack -d FourDimInts -s 0,0,0,0 -c 1,1,1,1 -k 2,2,2,2 $REPACK_OUTPUT_DIR/tints4dims.h5
RUNTEST tints4dimsStride2.ddl --enable-error-stack -d FourDimInts -s 0,0,0,0 -S 2,2,2,2 -c 2,2,2,2 $REPACK_OUTPUT_DIR/tints4dims.h5
RUNTEST tints4dimsCountEq.ddl --enable-error-stack -d FourDimInts -s 0,0,0,0 -S 2,2,1,1 -k 1,2,1,1 -c 2,2,4,4 $REPACK_OUTPUT_DIR/tints4dims.h5
RUNTEST tints4dimsBlockEq.ddl --enable-error-stack -d FourDimInts -s 0,0,0,0 -S 2,2,1,1 -c 2,2,1,1 -k 1,2,4,4 $REPACK_OUTPUT_DIR/tints4dims.h5

# test printing characters in ASCII instead of decimal
RUNTEST tchar1.ddl --enable-error-stack -r $REPACK_OUTPUT_DIR/tchar.h5

# test datatypes in ASCII and UTF8
RUNTEST charsets.ddl --enable-error-stack $REPACK_OUTPUT_DIR/charsets.h5

# test -p with a non existing dataset
#RUNTEST tperror.ddl --enable-error-stack -p -d bogus tfcontents1.h5

# test for file contents
#RUNTEST tcontents.ddl --enable-error-stack -n tfcontents1.h5
#RUNTEST tordercontents1.ddl --enable-error-stack -n --sort_by=name --sort_order=ascending tfcontents1.h5
#RUNTEST tordercontents2.ddl --enable-error-stack -n --sort_by=name --sort_order=descending tfcontents1.h5
#RUNTEST tattrcontents1.ddl --enable-error-stack -n 1 --sort_order=ascending tall.h5
#RUNTEST tattrcontents2.ddl --enable-error-stack -n 1 --sort_order=descending tall.h5

# fill values
# TODO: Reports offsets, which change after repacking
#RUNTEST tfill.ddl --enable-error-stack -p $REPACK_OUTPUT_DIR/tfvalues.h5

# several datatype, with references , print path
#RUNTEST treference.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tattr2.h5

# escape/not escape non printable characters
RUNTEST tstringe.ddl --enable-error-stack -e $REPACK_OUTPUT_DIR/tstr3.h5
RUNTEST tstring.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tstr3.h5
# char data as ASCII with non escape
RUNTEST tstring2.ddl --enable-error-stack -r -d str4 $REPACK_OUTPUT_DIR/tstr3.h5

# array indices print/not print
RUNTEST tindicesyes.ddl --enable-error-stack $REPACK_OUTPUT_DIR/taindices.h5
RUNTEST tindicesno.ddl --enable-error-stack -y $REPACK_OUTPUT_DIR/taindices.h5

########## array indices with subsetting
# 1D case
RUNTEST tindicessub1.ddl --enable-error-stack -d 1d -s 1 -S 10 -c 2  -k 3 $REPACK_OUTPUT_DIR/taindices.h5

# 2D case
RUNTEST tindicessub2.ddl --enable-error-stack -d 2d -s 1,2  -S 3,3 -c 3,2 -k 2,2 $REPACK_OUTPUT_DIR/taindices.h5

# 3D case
RUNTEST tindicessub3.ddl --enable-error-stack -d 3d -s 0,1,2 -S 1,3,3 -c 2,2,2  -k 1,2,2  $REPACK_OUTPUT_DIR/taindices.h5

# 4D case
RUNTEST tindicessub4.ddl --enable-error-stack -d 4d -s 0,0,1,2  -c 2,2,3,2 -S 1,1,3,3 -k 1,1,2,2  $REPACK_OUTPUT_DIR/taindices.h5

# Exceed the dimensions for subsetting
RUNTEST texceedsubstart.ddl --enable-error-stack -d 1d -s 1,3 $REPACK_OUTPUT_DIR/taindices.h5
RUNTEST texceedsubcount.ddl --enable-error-stack -d 1d -c 1,3 $REPACK_OUTPUT_DIR/taindices.h5
RUNTEST texceedsubstride.ddl --enable-error-stack -d 1d -S 1,3 $REPACK_OUTPUT_DIR/taindices.h5
RUNTEST texceedsubblock.ddl --enable-error-stack -d 1d -k 1,3 $REPACK_OUTPUT_DIR/taindices.h5

# test for displaying objects with very long names
# TODO: Has diff issues
#RUNTEST tlonglinks.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tlonglinks.h5

# dimensions over 4GB, print boundary
# TODO: tbigdims.h5 hangs when repacking
#RUNTEST tbigdims.ddl --enable-error-stack -d dset4gb -s 4294967284 -c 22 tbigdims.h5

# hyperslab read
RUNTEST thyperslab.ddl --enable-error-stack $REPACK_OUTPUT_DIR/thyperslab.h5

# test for displaying dataset and attribute of null space
RUNTEST tnullspace.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tnullspace.h5
RUNTEST tgrpnullspace.ddl -p --enable-error-stack $REPACK_OUTPUT_DIR/tgrpnullspace.h5

# test for displaying dataset and attribute of space with 0 dimension size
RUNTEST zerodim.ddl --enable-error-stack $REPACK_OUTPUT_DIR/zerodim.h5

# test for long double (some systems do not have long double)
#RUNTEST tldouble.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tldouble.h5

# test for vms
RUNTEST tvms.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tvms.h5

# test for binary output
RUNTEST tbin1.ddl --enable-error-stack -d integer -o $TEXT_OUTPUT_DIR/out1.bin -b LE $REPACK_OUTPUT_DIR/tbinary.h5

# test for string binary output
RUNTEST2B tstr2bin2.exp --enable-error-stack -d /g2/dset2 -b -o $TEXT_OUTPUT_DIR/tstr2bin2.txt $REPACK_OUTPUT_DIR/tstr2.h5
RUNTEST2B tstr2bin6.exp --enable-error-stack -d /g6/dset6 -b -o $TEXT_OUTPUT_DIR/tstr2bin6.txt $REPACK_OUTPUT_DIR/tstr2.h5

# test for dataset region references
# TODO: The tattrreg file fails to repack
#RUNTEST  tdatareg.ddl --enable-error-stack $REPACK_OUTPUT_DIR/tdatareg.h5
#RUNTEST_GREP ERRTXT "NULL token size" tdataregR.ddl --enable-error-stack -R $REPACK_OUTPUT_DIR/tdatareg.h5
#RUNTEST  tattrreg.ddl --enable-error-stack tattrreg.h5
#RUNTEST_GREP ERRTXT "NULL token size" tattrregR.ddl --enable-error-stack -R tattrreg.h5
#RUNTEST2 tbinregR.exp --enable-error-stack -d /Dataset1 -s 0 -R -y -o $TEXT_OUTPUT_DIR/tbinregR.txt    $REPACK_OUTPUT_DIR/tdatareg.h5

# tests for group creation order
# "1" tracked, "2" name, root tracked
#RUNTEST tordergr1.ddl --enable-error-stack --group=1 --sort_by=creation_order --sort_order=ascending $REPACK_OUTPUT_DIR/tordergr.h5
#RUNTEST tordergr2.ddl --enable-error-stack --group=1 --sort_by=creation_order --sort_order=descending $REPACK_OUTPUT_DIR/tordergr.h5
#RUNTEST tordergr3.ddl --enable-error-stack -g 2 -q name -z ascending $REPACK_OUTPUT_DIR/tordergr.h5
#RUNTEST tordergr4.ddl --enable-error-stack -g 2 -q name -z descending $REPACK_OUTPUT_DIR/tordergr.h5
#RUNTEST tordergr5.ddl --enable-error-stack -q creation_order $REPACK_OUTPUT_DIR/tordergr.h5

# tests for attribute order
# TODO: Creation order does not always survive repack
#RUNTEST torderattr1.ddl --enable-error-stack -H --sort_by=name --sort_order=ascending $REPACK_OUTPUT_DIR/torderattr.h5
#RUNTEST torderattr2.ddl --enable-error-stack -H --sort_by=name --sort_order=descending $REPACK_OUTPUT_DIR/torderattr.h5
#RUNTEST torderattr3.ddl --enable-error-stack -H --sort_by=creation_order --sort_order=ascending $REPACK_OUTPUT_DIR/torderattr.h5
#RUNTEST torderattr4.ddl --enable-error-stack -H --sort_by=creation_order --sort_order=descending $REPACK_OUTPUT_DIR/torderattr.h5

# tests for link references and order
#RUNTEST torderlinks1.ddl --enable-error-stack --sort_by=name --sort_order=ascending tfcontents1.h5
#RUNTEST torderlinks2.ddl --enable-error-stack --sort_by=name --sort_order=descending tfcontents1.h5

# tests for floating point user defined printf format
RUNTEST tfpformat.ddl --enable-error-stack -m %.7f $REPACK_OUTPUT_DIR/tfpformat.h5

# tests for traversal of external links
#RUNTEST textlinksrc.ddl --enable-error-stack textlinksrc.h5
#RUNTEST textlinkfar.ddl --enable-error-stack textlinkfar.h5

# test for dangling external links
#RUNTEST textlink.ddl --enable-error-stack textlink.h5

# test for -o -y for dataset with attributes
#RUNTEST2 tall-6.exp --enable-error-stack -y -o tall-6.txt -d /g1/g1.1/dset1.1.1 tall.h5

# test for non-existing file
#RUNTEST non_existing.ddl --enable-error-stack tgroup.h5 non_existing.h5

# test to verify HDFFV-10333: error similar to H5O_attr_decode in the jira issue
# TODO: err_attr_dspace.h5 does not repack (may be a native issue?)
#RUNTEST err_attr_dspace.ddl err_attr_dspace.h5

# test to verify HDFFV-9407: long double full precision
RUNTEST_GREP OUTTXT "1.123456789012345" t128bit_float.ddl -m %.35Lf $REPACK_OUTPUT_DIR/t128bit_float.h5

# Clean up generated files/directories
CLEAN_OUTPUT

# Report test results and exit
if test $nerrors -eq 0 ; then
    echo "All $TESTNAME tests passed."
    exit $EXIT_SUCCESS
else
    echo "$TESTNAME tests failed with $nerrors errors."
    exit $EXIT_FAILURE
fi

