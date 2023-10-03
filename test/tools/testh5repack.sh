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
# Tests for the h5repack tool

# Assume the HDF5 library was built with gzip support
USE_FILTER_DEFLATE="no"

TESTNAME=h5repack
EXIT_SUCCESS=0
EXIT_FAILURE=1

H5DELETE='h5delete -f'      # The h5delete tool name
H5REPACK=h5repack           # The h5repack tool name
H5DIFF=h5diff               # The h5diff tool name
H5DUMP=h5dump               # The h5dump tool name


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
H5REPACK_TESTFILES_OUT_DIR="./testfiles/expected/h5repack"

######################################################################
# Output files
# --------------------------------------------------------------------

# Where the text output goes
TEXT_OUTPUT_DIR=./h5repack_test_output

# Where the repacked HDF5 files go (TO VOL storage, FROM native)
REPACK_TO_VOL_DIR=./h5repack_to_vol

# Where the repacked HDF5 files go (FROM VOL storage, TO native)
REPACK_FROM_VOL_DIR=./h5repack_from_vol

######################################################################
# test files
# --------------------------------------------------------------------

# HDF5 test files.
#
# Kept in       $TESTFILES_HDF5_DIR
# Repacked to   $REPACK_OUTPUT_DIR
#
# These files fail to repack w/ native VOL (investigate later)
#
# These files have native-specific content
#
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
#
# These files give DAOS problems, usually due to old-school references
# h5repack_refs.h5
# h5repack_fill.h5
# h5repack_attr.h5
# h5repack_early.h5
# tordergr.h5
# h5repack_nested_8bit_enum.h5
HDF5_FILES="
h5repack_hlink.h5
h5repack_layout.h5
h5repack_deflate.h5
h5repack_layouto.h5
h5repack_layout2.h5
h5repack_layout3.h5
h5repack_named_dtypes.h5
h5repack_nested_8bit_enum_deflated.h5
h5repack_objs.h5
h5repack_none.h5
h5repack_f32le.h5
h5repack_f32le_ex.h5
h5repack_int32le_1d.h5
h5repack_int32le_1d_ex.h5
h5repack_int32le_2d.h5
h5repack_int32le_2d_ex.h5
h5repack_int32le_3d.h5
h5repack_int32le_3d_ex.h5
h5repack_uint8be.h5
h5repack_uint8be_ex.h5
h5diff_attr1.h5
"

# Expected output files.
#
# Kept in       $H5REPACK_TESTFILES_OUT_DIR
# Copied to     $TEXT_OUTPUT_DIR
#
# NOTE: This is ALL the files - they have not been culled based on the HDF5
#       files in the above list.
#
EXPECTED_OUTPUT_FILES="
h5repack-help.txt
h5repack_ext.bin
ublock.bin
h5repack.info
crtorder.tordergr.h5.ddl
deflate_limit.h5repack_layout.h5.ddl
h5repack_f32le_ex-0.dat
h5repack_int32le_1d_ex-0.dat
h5repack_int32le_1d_ex-1.dat
h5repack_int32le_2d_ex-0.dat
h5repack_int32le_3d_ex-0.dat
h5repack_layout.h5.ddl
h5repack_filters.h5-gzip_verbose_filters.tst
h5repack_layout.h5-dset2_chunk_20x10-errstk.tst
h5repack_layout.h5-plugin_test.ddl
h5repack_uint8be_ex-0.dat
h5repack_uint8be_ex-1.dat
h5repack_uint8be_ex-2.dat
h5repack_uint8be_ex-3.dat
plugin_test.h5repack_layout.h5.tst
1_vds.h5-vds_dset_chunk20x10x5-v.ddl
2_vds.h5-vds_chunk3x6x9-v.ddl
3_1_vds.h5-vds_chunk2x5x8-v.ddl
4_vds.h5-vds_compa-v.ddl
4_vds.h5-vds_conti-v.ddl
SP.h5repack_fsm_aggr_nopersist.h5.ddl
S.h5repack_fsm_aggr_persist.h5.ddl
STG.h5repack_none.h5.ddl
SPT.h5repack_aggr.h5.ddl
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
        filepath="$H5REPACK_TESTFILES_OUT_DIR/$outfile"

        # Use -f to make sure get a new copy
        $CP -f $filepath $TEXT_OUTPUT_DIR
        if [ $? -ne 0 ]; then
            echo "Error: FAILED to copy expected output file: $filepath ."
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
            filepath="$REPACK_TO_VOL_DIR/$hdf5file"
            $H5DELETE $filepath
        done

        # The VOL-specific HDF5 output directory is always created, even if
        # the VOL storage won't use it. Delete it here.
        $RM $REPACK_TO_VOL_DIR

        # The re-repacked directory should only contain native HDF5
        # files and can be deleted normally.
        $RM $REPACK_FROM_VOL_DIR
   fi
}

# Print a $* message left justified in a field of 70 characters
#
MESSAGE() {
   SPACES="                                                               "
   echo "$* $SPACES" | cut -c1-70 | tr -d '\012'
}

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Testing".
#
TESTING() {
    MESSAGE "Testing $*"
}

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Verifying".
#
VERIFY() {
    MESSAGE "Verifying $*"
}

# Print a message that a test has been skipped (because a required filter
# was unavailable)
SKIP() {
    TESTING $H5REPACK $@
    echo " -SKIP-"
}

######################################################################
# Main testing functions
# --------------------------------------------------------------------

# Call the h5diff tool
#
DIFF_HDF5()
{
    native_file=$1
    shift

    if [ $native_file = "1_NATIVE" ]; then
        vol_args="--vol-name-1=native"
    else
        vol_args="--vol-name-2=native"
    fi

    VERIFY $H5DIFF output $@
    (
        $RUNSERIAL $H5DIFF -q  $vol_args "$@"
    )
    RET=$?
    if [ $RET != 0 ] ; then
         echo "*FAILED*"
         nerrors="`expr $nerrors + 1`"
    else
         echo " PASSED"
    fi

}

RUNTEST()
{
    args=$@
    infile=$2
    outfile=out-$1.$2
    shift
    shift

    # Run "to VOL storage" test.

    inpath="$TESTFILES_HDF5_DIR/$infile"
    outpath="$REPACK_TO_VOL_DIR/$outfile"

    echo "   native-->VOL"
    TESTING $H5REPACK "($args $@)"
    (
        $ENVCMD $RUNSERIAL $H5REPACK --src-vol-name=native -q name "$@" $inpath $outpath
    )
    RET=$?
    if [ $RET != 0 ] ; then
        echo "*FAILED*"
        nerrors="`expr $nerrors + 1`"
    else
        echo " PASSED"
        DIFF_HDF5 "1_NATIVE" $inpath $outpath
    fi


    # Run "from VOL storage" test.

    inpath="$REPACK_TO_VOL_DIR/$outfile"
    outpath="$REPACK_FROM_VOL_DIR/$outfile"

    echo "   VOL-->native"
    TESTING $H5REPACK "($args $@)"
    (
        $ENVCMD $RUNSERIAL $H5REPACK --dst-vol-name=native -q name "$@" $inpath $outpath
    )
    RET=$?
    if [ $RET != 0 ] ; then
        echo "*FAILED*"
        nerrors="`expr $nerrors + 1`"
    else
        echo " PASSED"
        DIFF_HDF5 "2_NATIVE" $inpath $outpath
    fi

    echo
}

#------------------------------------------
# Verifying layouts of a dataset
VERIFY_LAYOUT_DSET()
{
    layoutfile=layout-$1.$2
    dset=$3
    expectlayout=$4
    infile=$2
    outfile=out-$1.$2
    shift
    shift
    shift
    shift

    inpath="$TESTFILES_HDF5_DIR/$infile"
    outpath="$REPACK_TO_VOL_DIR/$outfile"
    layoutpath="$TEXT_OUTPUT_DIR/$layoutfile"

    TESTING  $H5REPACK $@
    (
        $RUNSERIAL $H5REPACK --src-vol-name=native "$@" $inpath $outpath
    )
    RET=$?
    if [ $RET != 0 ] ; then
        echo "*FAILED*"
        nerrors="`expr $nerrors + 1`"
    else
        echo " PASSED"
        DIFF_HDF5 "1_NATIVE" $inpath $outpath
    fi

    #---------------------------------
    # check the layout from a dataset
    VERIFY  "a dataset layout"
    (
        $RUNSERIAL $H5DUMP -d $dset -pH $outpath > $layoutpath
    )
    $GREP $expectlayout $layoutpath > /dev/null
    if [ $? -eq 0 ]; then
        echo " PASSED"
    else
        echo " FAILED"
        nerrors="`expr $nerrors + 1`"
    fi
}

#----------------------------------------
# Verifying layouts from entire file
VERIFY_LAYOUT_ALL()
{
    infile=$2
    outfile=out-$1.$2
    layoutfile=layout-$1.$2
    expectlayout=$3
    shift
    shift
    shift

    inpath="$TESTFILES_HDF5_DIR/$infile"
    outpath="$REPACK_TO_VOL_DIR/$outfile"
    layoutpath="$TEXT_OUTPUT_DIR/$layoutfile"

    TESTING  $H5REPACK $@
    (
        $RUNSERIAL $H5REPACK --src-vol-name=native "$@" $inpath $outpath
    )
    RET=$?
    if [ $RET != 0 ] ; then
        echo "*FAILED*"
        nerrors="`expr $nerrors + 1`"
    else
        echo " PASSED"
        DIFF_HDF5 "1_NATIVE" $inpath $outpath
    fi


    #---------------------------------
    # check the layout from a dataset
    # check if the other layouts still exist
    VERIFY  "layouts"
    (
        echo
        # if CONTIGUOUS
        if [ $expectlayout = "CONTIGUOUS" ]; then
            TESTING $H5DUMP -pH $outpath
            (
                $RUNSERIAL $H5DUMP -pH $outpath > $layoutpath
            )
            $GREP "COMPACT" $layoutpath  > /dev/null
            if [ $? -eq 0 ]; then
                echo " FAILED"
                nerrors="`expr $nerrors + 1`"
            else
                $GREP "CHUNKED" $layoutpath  > /dev/null
                if [ $? -eq 0 ]; then
                    echo " FAILED"
                    nerrors="`expr $nerrors + 1`"
                else
                    echo " PASSED"
                fi
            fi
        else
            # if COMPACT
            if [ $expectlayout = "COMPACT" ]; then
                TESTING $H5DUMP -pH $outpath
                (
                    $RUNSERIAL $H5DUMP -pH $outpath > $layoutpath
                )
                $GREP "CHUNKED" $layoutpath  > /dev/null
                if [ $? -eq 0 ]; then
                    echo " FAILED"
                    nerrors="`expr $nerrors + 1`"
                else
                    $GREP "CONTIGUOUS" $layoutpath  > /dev/null
                    if [ $? -eq 0 ]; then
                        echo " FAILED"
                        nerrors="`expr $nerrors + 1`"
                    else
                        echo " PASSED"
                    fi
                fi
            else
                # if CHUNKED
                if [ $expectlayout = "CHUNKED" ]; then
                    TESTING $H5DUMP -pH $outpath
                    (
                        $RUNSERIAL $H5DUMP -pH $outpath > $layoutpath
                    )
                    $GREP "CONTIGUOUS" $layoutpath  > /dev/null
                    if [ $? -eq 0 ]; then
                        echo " FAILED"
                        nerrors="`expr $nerrors + 1`"
                    else
                        $GREP "COMPACT" $layoutpath  > /dev/null
                        if [ $? -eq 0 ]; then
                            echo " FAILED"
                            nerrors="`expr $nerrors + 1`"
                        else
                            echo " PASSED"
                        fi
                    fi
                fi
           fi
        fi
    )
}

RUNTEST_HELP() {

    expect="$TEXT_OUTPUT_DIR/$1"
    actual="$TEXT_OUTPUT_DIR/`basename $1 .txt`.out"
    actual_err="$TEXT_OUTPUT_DIR/`basename $1 .txt`.err"
    shift

    # Run test.
    TESTING $H5REPACK $@
    (
      $RUNSERIAL $H5REPACK "$@"
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

# This is different from output_filter.sh
STDOUT_FILTER() {
    result_file=$1
    tmp_file=/tmp/h5test_tmp_$$
    # Filter name of files.
    cp $result_file $tmp_file
    sed -e '/^Opening file/d' -e '/^Making file/d' \
    < $tmp_file > $result_file
    # cleanup
    rm -f $tmp_file
}

##############################################################################
##############################################################################
###              T H E   T E S T S                                         ###
##############################################################################
##############################################################################

# Prepare for test
#
# We create the repack output dir in case it's needed. If a VOL connector doesn't
# use normal files, it'll just stay empty and get deleted later.
CLEAN_OUTPUT
test -d $TEXT_OUTPUT_DIR || mkdir -p $TEXT_OUTPUT_DIR
test -d $REPACK_TO_VOL_DIR || mkdir -p $REPACK_TO_VOL_DIR
test -d $REPACK_FROM_VOL_DIR || mkdir -p $REPACK_FROM_VOL_DIR
COPY_EXPECTED_OUTPUT_FILES

RUNTEST_HELP h5repack-help.txt -h

# Basic files (these files have no filters)
#RUNTEST fill h5repack_fill.h5
#RUNTEST objs h5repack_objs.h5
#RUNTEST attr h5repack_attr.h5
RUNTEST hlink h5repack_hlink.h5
#RUNTEST layout h5repack_layout.h5
#RUNTEST early h5repack_early.h5

# nested 8bit enum in both deflated and non-deflated datafiles
#if [ $USE_FILTER_DEFLATE != "yes" ]; then
#RUNTEST nested_8bit_enum h5repack_nested_8bit_enum.h5
#else
#RUNTEST nested_8bit_enum h5repack_nested_8bit_enum_deflated.h5
#fi

#########################################################
# layout options (these files have no filters)
#########################################################
VERIFY_LAYOUT_DSET dset2_chunk_20x10 h5repack_layout.h5 dset2 CHUNKED --layout dset2:CHUNK=20x10

VERIFY_LAYOUT_ALL chunk_20x10 h5repack_layout.h5 CHUNKED -l CHUNK=20x10

VERIFY_LAYOUT_DSET dset2_conti h5repack_layout.h5 dset2 CONTIGUOUS -l dset2:CONTI

VERIFY_LAYOUT_ALL conti h5repack_layout.h5 CONTIGUOUS -l CONTI

################################################################
# layout conversions (file has no filters)
###############################################################

VERIFY_LAYOUT_DSET dset_compa_conti h5repack_layout.h5 dset_compact CONTIGUOUS -l dset_compact:CONTI

VERIFY_LAYOUT_DSET dset_compa_chunk h5repack_layout.h5 dset_compact CHUNKED -l dset_compact:CHUNK=2x5

VERIFY_LAYOUT_DSET dset_conti_chunk h5repack_layout.h5 dset_contiguous CHUNKED -l dset_contiguous:CHUNK=3x6

VERIFY_LAYOUT_DSET dset_conti_conti h5repack_layout.h5 dset_contiguous CONTIGUOUS -l dset_contiguous:CONTI

VERIFY_LAYOUT_DSET chunk_conti h5repack_layout.h5 dset_chunk CONTIGUOUS -l dset_chunk:CONTI

VERIFY_LAYOUT_DSET chunk_18x13 h5repack_layout.h5 dset_chunk CHUNKED -l dset_chunk:CHUNK=18x13

#---------------------------------------------------------------------------
# Test file contains chunked datasets (need multiple dsets) with
# unlimited max dims.   (HDFFV-7933)
# Use first dset to test.
#---------------------------------------------------------------------------
# chunk to chunk - specify chunk dim bigger than any current dim
VERIFY_LAYOUT_DSET chunk2chunk h5repack_layout3.h5 chunk_unlimit1 CHUNK -l chunk_unlimit1:CHUNK=100x300

# chunk to contiguous
VERIFY_LAYOUT_DSET chunk2conti h5repack_layout3.h5 chunk_unlimit1 CONTI -l chunk_unlimit1:CONTI

#--------------------------------------------------------------------------
# Test -f for some specific cases. Chunked dataset with unlimited max dims.
# (HDFFV-8012)
#--------------------------------------------------------------------------
# - should not fail
# - should not change max dims from unlimit

# chunk dim is bigger than dataset dim. ( dset size < 64k )
VERIFY_LAYOUT_DSET error1 h5repack_layout3.h5 chunk_unlimit1 H5S_UNLIMITED -f chunk_unlimit1:NONE
# chunk dim is bigger than dataset dim. ( dset size > 64k )
VERIFY_LAYOUT_DSET error2 h5repack_layout3.h5 chunk_unlimit2 H5S_UNLIMITED -f chunk_unlimit2:NONE

# chunk dims are smaller than dataset dims. ( dset size < 64k )
#RUNTEST_MAIN h5repack_layout3.h5  -f chunk_unlimit3:NONE
VERIFY_LAYOUT_DSET error3 h5repack_layout3.h5 chunk_unlimit3 H5S_UNLIMITED -f chunk_unlimit3:NONE

# Clean up generated files/directories
CLEAN_OUTPUT

if test $nerrors -eq 0 ; then
    echo "All $TESTNAME tests passed."
    exit $EXIT_SUCCESS
else
    echo "$TESTNAME tests failed with $nerrors errors."
    exit $EXIT_FAILURE
fi

