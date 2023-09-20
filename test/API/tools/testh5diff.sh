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
# Tests for the h5diff tool
#

TESTNAME=h5diff
EXIT_SUCCESS=0
EXIT_FAILURE=1

H5DIFF=h5diff               # The tool name
H5DELETE='h5delete -f'      # The h5delete tool name
H5REPACK=h5repack           # The h5repack tool name

RUNSERIAL=""
RUNPARALLEL="mpiexec -n 6"

RM='rm -rf'
CMP='cmp -s'
DIFF='diff -c'
CP='cp'
DIRNAME='dirname'
LS='ls'
AWK='awk'

nerrors=0
verbose=yes
h5haveexitcode=yes        # default is yes
pmode=                # default to run h5diff tests
mydomainname=`domainname 2>/dev/null`

######################################################################
# Input files
# --------------------------------------------------------------------

# Where the tool's HDF5 input files are located
TESTFILES_HDF5_DIR="./testfiles/hdf5"

# Where the tool's expected output files are located
H5DIFF_TESTFILES_OUT_DIR="./testfiles/expected/h5diff"

######################################################################
# Output files
# --------------------------------------------------------------------

# Where the text output goes
TEXT_OUTPUT_DIR=./h5diff_test_output

# Where the repacked HDF5 input files go
REPACK_OUTPUT_DIR=./h5diff_repack_output

######################################################################
# test files
# --------------------------------------------------------------------

# HDF5 test files.
#
# Kept in       $TESTFILES_HDF5_DIR
# Repacked to   $REPACK_OUTPUT_DIR
#
# These files don't repack between VOL connectors (usually due to links)
# h5diff_types.h5
# h5diff_links.h5
# h5diff_softlinks.h5
# h5diff_linked_softlink.h5
# h5diff_extlink_src.h5
# h5diff_extlink_trg.h5
# h5diff_ext2softlink_src.h5
# h5diff_ext2softlink_trg.h5
# h5diff_danglelinks1.h5
# h5diff_danglelinks2.h5
# h5diff_grp_recurse1.h5
# h5diff_grp_recurse2.h5
# h5diff_grp_recurse_ext1.h5
# h5diff_grp_recurse_ext2-1.h5
# h5diff_grp_recurse_ext2-2.h5
# h5diff_grp_recurse_ext2-3.h5
#
# These files have problems with the passthrough after being repacked
# tvlstr.h5
# h5diff_comp_vl_strs.h5
# compounds_array_vlen1.h5
#
# These files have problems with DAOS (usually due to references)
# h5diff_attr1.h5
# h5diff_attr2.h5
# h5diff_attr3.h5
# h5diff_dset1.h5
# h5diff_dset2.h5
HDF5_FILES="
h5diff_basic1.h5
h5diff_basic2.h5
h5diff_dtypes.h5
h5diff_hyper1.h5
h5diff_hyper2.h5
h5diff_empty.h5
h5diff_dset_zero_dim_size1.h5
h5diff_dset_zero_dim_size2.h5
h5diff_exclude1-1.h5
h5diff_exclude1-2.h5
h5diff_exclude2-1.h5
h5diff_exclude2-2.h5
h5diff_exclude3-1.h5
h5diff_exclude3-2.h5
compounds_array_vlen2.h5
h5diff_attr_v_level1.h5
h5diff_attr_v_level2.h5
h5diff_enum_invalid_values.h5
non_comparables1.h5
non_comparables2.h5
diff_strings1.h5
diff_strings2.h5
"

# Expected output files.
#
# Kept in       $H5DIFF_TESTFILES_OUT_DIR
# Copied to     $TEXT_OUTPUT_DIR
#
# NOTE: This is ALL the files - they have not been culled based on the HDF5
#       files in the above list.
#
EXPECTED_OUTPUT_FILES="
h5diff_10.txt
h5diff_100.txt
h5diff_101.txt
h5diff_102.txt
h5diff_103.txt
h5diff_104.txt
h5diff_11.txt
h5diff_12.txt
h5diff_13.txt
h5diff_14.txt
h5diff_15.txt
h5diff_16_1.txt
h5diff_16_2.txt
h5diff_16_3.txt
h5diff_17.txt
h5diff_171.txt
h5diff_172.txt
h5diff_18_1.txt
h5diff_18.txt
h5diff_20.txt
h5diff_200.txt
h5diff_201.txt
h5diff_202.txt
h5diff_203.txt
h5diff_204.txt
h5diff_205.txt
h5diff_206.txt
h5diff_207.txt
h5diff_208.txt
h5diff_220.txt
h5diff_221.txt
h5diff_222.txt
h5diff_223.txt
h5diff_224.txt
h5diff_21.txt
h5diff_22.txt
h5diff_23.txt
h5diff_24.txt
h5diff_25.txt
h5diff_26.txt
h5diff_27.txt
h5diff_28.txt
h5diff_30.txt
h5diff_300.txt
h5diff_400.txt
h5diff_401.txt
h5diff_402.txt
h5diff_403.txt
h5diff_404.txt
h5diff_405.txt
h5diff_406.txt
h5diff_407.txt
h5diff_408.txt
h5diff_409.txt
h5diff_410.txt
h5diff_411.txt
h5diff_412.txt
h5diff_413.txt
h5diff_414.txt
h5diff_415.txt
h5diff_416.txt
h5diff_417.txt
h5diff_418.txt
h5diff_419.txt
h5diff_420.txt
h5diff_421.txt
h5diff_422.txt
h5diff_423.txt
h5diff_424.txt
h5diff_425.txt
h5diff_450.txt
h5diff_451.txt
h5diff_452.txt
h5diff_453.txt
h5diff_454.txt
dangling_link.err
h5diff_455.txt
h5diff_456.txt
h5diff_457.txt
h5diff_458.txt
h5diff_459.txt
h5diff_465.txt
h5diff_466.txt
h5diff_467.txt
h5diff_468.txt
h5diff_469.txt
h5diff_471.txt
h5diff_472.txt
h5diff_473.txt
h5diff_474.txt
h5diff_475.txt
h5diff_480.txt
h5diff_481.txt
h5diff_482.txt
h5diff_483.txt
h5diff_484.txt
h5diff_485.txt
h5diff_486.txt
h5diff_487.txt
h5diff_50.txt
h5diff_51.txt
h5diff_52.txt
h5diff_53.txt
h5diff_54.txt
h5diff_55.txt
h5diff_56.txt
h5diff_57.txt
h5diff_58.txt
h5diff_58_ref.txt
h5diff_59.txt
h5diff_500.txt
h5diff_501.txt
h5diff_502.txt
h5diff_503.txt
h5diff_504.txt
h5diff_505.txt
h5diff_506.txt
h5diff_507.txt
h5diff_508.txt
h5diff_509.txt
h5diff_510.txt
h5diff_511.txt
h5diff_512.txt
h5diff_513.txt
h5diff_514.txt
h5diff_515.txt
h5diff_516.txt
h5diff_517.txt
h5diff_518.txt
h5diff_530.txt
h5diff_540.txt
h5diff_60.txt
h5diff_61.txt
h5diff_62.txt
h5diff_63.txt
h5diff_600.txt
h5diff_601.txt
h5diff_601_ERR.err
h5diff_603.txt
h5diff_604.txt
h5diff_605.txt
h5diff_606.txt
h5diff_607.txt
h5diff_608.txt
h5diff_609.txt
h5diff_610.txt
h5diff_612.txt
h5diff_613.txt
h5diff_614.txt
h5diff_615.txt
h5diff_616.txt
h5diff_617.txt
h5diff_618.txt
h5diff_619.txt
h5diff_621.txt
h5diff_622.txt
h5diff_623.txt
h5diff_624.txt
h5diff_625.txt
h5diff_626.txt
h5diff_627.txt
h5diff_628.txt
h5diff_629.txt
h5diff_630.txt
h5diff_631.txt
h5diff_640.txt
h5diff_641.txt
h5diff_642.txt
h5diff_643.txt
h5diff_644.txt
h5diff_645.txt
h5diff_646.txt
h5diff_70.txt
h5diff_700.txt
h5diff_701.txt
h5diff_702.txt
h5diff_703.txt
h5diff_704.txt
h5diff_705.txt
h5diff_706.txt
h5diff_707.txt
h5diff_708.txt
h5diff_709.txt
h5diff_710.txt
h5diff_80.txt
h5diff_90.txt
h5diff_8625.txt
h5diff_8639.txt
h5diff_reg.txt
h5diff_v1.txt
h5diff_v2.txt
h5diff_v3.txt
h5diff_vlstr.txt
"

######################################################################
# Utility functions
# --------------------------------------------------------------------

# RUNSERIAL is used. Check if it can return exit code from executable correctly.
if [ -n "$RUNSERIAL_NOEXITCODE" ]; then
    echo "***Warning*** Serial Exit Code is not passed back to shell corretly."
    echo "***Warning*** Exit code checking is skipped."
    h5haveexitcode=no
fi

# Parse option
#   -p   run ph5diff tests
#   -h   print help page
while [ $# -gt 0 ]; do
    case "$1" in
    -p)    # reset the tool name and bin to run ph5diff tests
    TESTNAME=ph5diff
    H5DIFF=ph5diff  # The tool name
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

# Copy the expected text output files to the text output directory
# to make it easier to diff the expected and actual output.
#
COPY_EXPECTED_OUTPUT_FILES()
{
    for outfile in $EXPECTED_OUTPUT_FILES
    do
        filepath="$H5DIFF_TESTFILES_OUT_DIR/$outfile"

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
#
RUNTEST() {
    expect="$TEXT_OUTPUT_DIR/$1"
    expect_err="$TEXT_OUTPUT_DIR/`basename $1 .txt`.err"
    actual="$TEXT_OUTPUT_DIR/`basename $1 .txt`.out"
    actual_err="$TEXT_OUTPUT_DIR/`basename $1 .txt`.out.err"
    actual_sav=${actual}-sav
    actual_err_sav=${actual_err}-sav
    shift
    if test -n "$pmode"; then
        # $RUNPARALLEL in HDF5 tests (TODO: handle better)
        RUNCMD="mpirun -n 6"
    else
        # $RUNSERIAL in HDF5 tests
        RUNCMD=
    fi

    # Run test.
    TESTING $H5DIFF $@
    (
        #echo "#############################"
        #echo "Expected output for '$H5DIFF $@'"
        #echo "#############################"
        eval $RUNCMD $H5DIFF "$@"
    ) >$actual 2>$actual_err
    EXIT_CODE=$?

    # Clean h5diff stdout files
    H5DIFF_FILTER $actual

    # save actual and actual_err in case they are needed later.
    cp $actual $actual_sav
    STDOUT_FILTER $actual
    cp $actual_err $actual_err_sav
    STDERR_FILTER $actual_err

    # don't add exit code check in pmode, as it causes failure. (exit code
    # is from mpirun not tool)
    # if any problem occurs relate to an exit code, it will be caught in
    # serial mode, so the test is fullfilled.
    if test $h5haveexitcode = 'yes' -a -z "$pmode"; then
      echo "EXIT CODE: $EXIT_CODE" >> $actual
    fi

    if [ ! -f $expect ]; then
        # Create the expect file if it doesn't yet exist.
        echo " CREATED"
        cp $actual $expect
        echo "    Expected result (*.ddl) missing"
        nerrors="`expr $nerrors + 1`"
    elif $CMP $expect $actual; then
        echo " PASSED"
    elif test $h5haveexitcode = 'yes' -a -z "$pmode"; then
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
        # remove "EXIT CODE:" line from expect file. test for exit code
        # is done by serial mode.
        grep -v "EXIT CODE:" $expect_sorted > $expect_sorted.noexit
        mv $expect_sorted.noexit $expect_sorted
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
}


##############################################################################
##############################################################################
###              T H E   T E S T S                                         ###
##############################################################################
##############################################################################

##############################################################################
# To avoid the printing of the complete full path of the test file, that hides
# all the other parameters for long paths, the printing of the command line
# is done first in
# TESTING with the name only of the test file $TOOL, not its full path $TESTFILE
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

# Run h5diff tests

# ############################################################################
# # Common usage
# ############################################################################

# 1.0
RUNTEST h5diff_10.txt -h

# 1.1 normal mode
#RUNTEST h5diff_11.txt  $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5

# 1.2 normal mode with objects
RUNTEST h5diff_12.txt  $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5  g1/dset1 g1/dset2

# 1.3 report mode
RUNTEST h5diff_13.txt -r $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5

# 1.4 report  mode with objects
RUNTEST h5diff_14.txt  -r $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 g1/dset1 g1/dset2

# 1.5 with -d
RUNTEST h5diff_15.txt --report --delta=5 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 g1/dset3 g1/dset4

# 1.6.1 with -p (int)
RUNTEST h5diff_16_1.txt -v -p 0.02 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 g1/dset5 g1/dset6

# 1.6.2 with -p (unsigned long_long)
RUNTEST h5diff_16_2.txt --verbose --relative=0.02 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 g1/dset7 g1/dset8

# 1.6.3 with -p (double)
RUNTEST h5diff_16_3.txt -v -p 0.02 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 g1/dset9 g1/dset10

# 1.7 verbose mode
RUNTEST h5diff_17.txt -v $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5

# 1.7 test 32-bit INFINITY
RUNTEST h5diff_171.txt -v $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 /g1/fp19 /g1/fp19_COPY

# 1.7 test 64-bit INFINITY
RUNTEST h5diff_172.txt -v $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 /g1/fp20 /g1/fp20_COPY

# 1.8 quiet mode
RUNTEST h5diff_18.txt -q $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5

# 1.8 -v and -q
RUNTEST h5diff_18_1.txt -v -q $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5


# ##############################################################################
# # not comparable types
# ##############################################################################

# 2.0
#RUNTEST h5diff_20.txt -v h5diff_types.h5 h5diff_types.h5  dset g1

# 2.1
#RUNTEST h5diff_21.txt -v h5diff_types.h5 h5diff_types.h5 dset l1

# 2.2
#RUNTEST h5diff_22.txt -v  h5diff_types.h5 h5diff_types.h5 dset t1

# ##############################################################################
# # compare groups, types, links (no differences and differences)
# ##############################################################################

# 2.3
#RUNTEST h5diff_23.txt -v h5diff_types.h5 h5diff_types.h5 g1 g1

# 2.4
#RUNTEST h5diff_24.txt -v h5diff_types.h5 h5diff_types.h5 t1 t1

# 2.5
#RUNTEST h5diff_25.txt -v h5diff_types.h5 h5diff_types.h5 l1 l1

# 2.6
#RUNTEST h5diff_26.txt -v h5diff_types.h5 h5diff_types.h5 g1 g2

# 2.7
#RUNTEST h5diff_27.txt -v h5diff_types.h5 h5diff_types.h5 t1 t2

# 2.8
#RUNTEST h5diff_28.txt -v h5diff_types.h5 h5diff_types.h5 l1 l2


# ##############################################################################
# # Enum value tests (may become more comprehensive in the future)
# ##############################################################################

# 3.0
# test enum types which may have invalid values
RUNTEST h5diff_30.txt -v $REPACK_OUTPUT_DIR/h5diff_enum_invalid_values.h5 $REPACK_OUTPUT_DIR/h5diff_enum_invalid_values.h5 dset1 dset2


# ##############################################################################
# # Dataset datatypes
# ##############################################################################

# 5.0
RUNTEST h5diff_50.txt -v $REPACK_OUTPUT_DIR/h5diff_dtypes.h5 $REPACK_OUTPUT_DIR/h5diff_dtypes.h5 dset0a dset0b

# 5.1
RUNTEST h5diff_51.txt -v $REPACK_OUTPUT_DIR/h5diff_dtypes.h5 $REPACK_OUTPUT_DIR/h5diff_dtypes.h5 dset1a dset1b

# 5.2
RUNTEST h5diff_52.txt -v $REPACK_OUTPUT_DIR/h5diff_dtypes.h5 $REPACK_OUTPUT_DIR/h5diff_dtypes.h5 dset2a dset2b

# 5.3
RUNTEST h5diff_53.txt -v $REPACK_OUTPUT_DIR/h5diff_dtypes.h5 $REPACK_OUTPUT_DIR/h5diff_dtypes.h5 dset3a dset4b

# 5.4
RUNTEST h5diff_54.txt -v $REPACK_OUTPUT_DIR/h5diff_dtypes.h5 $REPACK_OUTPUT_DIR/h5diff_dtypes.h5 dset4a dset4b

# 5.5
RUNTEST h5diff_55.txt -v $REPACK_OUTPUT_DIR/h5diff_dtypes.h5 $REPACK_OUTPUT_DIR/h5diff_dtypes.h5 dset5a dset5b

# 5.6
RUNTEST h5diff_56.txt -v $REPACK_OUTPUT_DIR/h5diff_dtypes.h5 $REPACK_OUTPUT_DIR/h5diff_dtypes.h5 dset6a dset6b

# 5.7
RUNTEST h5diff_57.txt -v $REPACK_OUTPUT_DIR/h5diff_dtypes.h5 $REPACK_OUTPUT_DIR/h5diff_dtypes.h5 dset7a dset7b

# 5.8 (region reference)
#RUNTEST h5diff_58.txt -v2 $REPACK_OUTPUT_DIR/h5diff_dset1.h5 $REPACK_OUTPUT_DIR/h5diff_dset2.h5 refreg
#RUNTEST h5diff_58_ref.txt -v2 $REPACK_OUTPUT_DIR/h5diff_dset1.h5 $REPACK_OUTPUT_DIR/h5diff_dset2.h5 /g1/reference2D
# STD_REF_OBJ
#RUNTEST h5diff_reg.txt -v2 $REPACK_OUTPUT_DIR/trefer_attr.h5 $REPACK_OUTPUT_DIR/trefer_ext2.h5 Dataset3 Dataset3

# test for both dset and attr with same type but with different size
# ( HDDFV-7942 )
RUNTEST h5diff_59.txt -v $REPACK_OUTPUT_DIR/h5diff_dtypes.h5 $REPACK_OUTPUT_DIR/h5diff_dtypes.h5 dset11a dset11b

# Strings
# ( HDFFV-10128 )
RUNTEST h5diff_60.txt -v $REPACK_OUTPUT_DIR/diff_strings1.h5 $REPACK_OUTPUT_DIR/diff_strings2.h5 string1 string1
RUNTEST h5diff_61.txt -v $REPACK_OUTPUT_DIR/diff_strings1.h5 $REPACK_OUTPUT_DIR/diff_strings2.h5 string2 string2
RUNTEST h5diff_62.txt -v $REPACK_OUTPUT_DIR/diff_strings1.h5 $REPACK_OUTPUT_DIR/diff_strings2.h5 string3 string3
RUNTEST h5diff_63.txt -v $REPACK_OUTPUT_DIR/diff_strings1.h5 $REPACK_OUTPUT_DIR/diff_strings2.h5 string4 string4

# ##############################################################################
# # Error messages
# ##############################################################################


# 6.0: Check if the command line number of arguments is less than 3
RUNTEST h5diff_600.txt $REPACK_OUTPUT_DIR/h5diff_basic1.h5

# 6.1: Check if non-exist object name is specified
RUNTEST h5diff_601.txt $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 nono_obj


# ##############################################################################
# # -d
# ##############################################################################


# 6.3: negative value
RUNTEST h5diff_603.txt -d -4 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 g1/dset3 g1/dset4

# 6.4: zero
RUNTEST h5diff_604.txt -d 0 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 g1/dset3 g1/dset4

# 6.5: non number
RUNTEST h5diff_605.txt -d u $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 g1/dset3 g1/dset4

# 6.6: hexadecimal
RUNTEST h5diff_606.txt -d 0x1 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 g1/dset3 g1/dset4

# 6.7: string
RUNTEST h5diff_607.txt -d "1" $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 g1/dset3 g1/dset4

# 6.8: use system epsilon
RUNTEST h5diff_608.txt --use-system-epsilon $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5  g1/dset3 g1/dset4

# 6.9: number larger than biggest difference
RUNTEST h5diff_609.txt -d 200 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 g1/dset3 g1/dset4

# 6.10: number smaller than smallest difference
RUNTEST h5diff_610.txt -d 1 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 g1/dset3 g1/dset4


# ##############################################################################
# # -p
# ##############################################################################


# 6.12: negative value
RUNTEST h5diff_612.txt -p -4 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 g1/dset3 g1/dset4

# 6.13: zero
RUNTEST h5diff_613.txt -p 0 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 g1/dset3 g1/dset4

# 6.14: non number
RUNTEST h5diff_614.txt -p u $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5  g1/dset3 g1/dset4

# 6.15: hexadecimal
RUNTEST h5diff_615.txt -p 0x1 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 g1/dset3 g1/dset4

# 6.16: string
RUNTEST h5diff_616.txt -p "0.21" $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 g1/dset3 g1/dset4

# 6.17: repeated option
RUNTEST h5diff_617.txt -p 0.21 -p 0.22 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 g1/dset3 g1/dset4

# 6.18: number larger than biggest difference
RUNTEST h5diff_618.txt -p 2 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 g1/dset3 g1/dset4

# 6.19: number smaller than smallest difference
RUNTEST h5diff_619.txt -p 0.005 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 g1/dset3 g1/dset4

# ##############################################################################
# # -n
# ##############################################################################

# 6.21: negative value
RUNTEST h5diff_621.txt -n -4 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 g1/dset3 g1/dset4

# 6.22: zero
RUNTEST h5diff_622.txt -n 0 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 g1/dset3 g1/dset4

# 6.23: non number
RUNTEST h5diff_623.txt -n u $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 g1/dset3 g1/dset4

# 6.24: hexadecimal
RUNTEST h5diff_624.txt -n 0x1 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 g1/dset3 g1/dset4

# 6.25: string
RUNTEST h5diff_625.txt -n "2" $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5  g1/dset3 g1/dset4

# 6.26: repeated option
RUNTEST h5diff_626.txt -n 2 -n 3 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 g1/dset3 g1/dset4

# 6.27: number larger than biggest difference
RUNTEST h5diff_627.txt --count=200 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 g1/dset3 g1/dset4

# 6.28: number smaller than smallest difference
RUNTEST h5diff_628.txt -n 1 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 g1/dset3 g1/dset4

# ##############################################################################
# # NaN
# ##############################################################################
# 6.30: test (NaN == NaN) must be true based on our documentation -- XCAO
RUNTEST h5diff_630.txt -v -d "0.0001" $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 g1/fp18 g1/fp18_COPY
RUNTEST h5diff_631.txt -v --use-system-epsilon $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 g1/fp18 g1/fp18_COPY


# ##############################################################################
# 7.  attributes
# ##############################################################################
#RUNTEST h5diff_70.txt -v $REPACK_OUTPUT_DIR/h5diff_attr1.h5 $REPACK_OUTPUT_DIR/h5diff_attr2.h5

# ##################################################
#  attrs with verbose option level
# ##################################################
# Attributes seem to have type problems under DAOS
#RUNTEST h5diff_700.txt -v1 $REPACK_OUTPUT_DIR/h5diff_attr1.h5 $REPACK_OUTPUT_DIR/h5diff_attr2.h5
#RUNTEST h5diff_701.txt -v2 $REPACK_OUTPUT_DIR/h5diff_attr1.h5 $REPACK_OUTPUT_DIR/h5diff_attr2.h5
#RUNTEST h5diff_702.txt --verbose=1 $REPACK_OUTPUT_DIR/h5diff_attr1.h5 $REPACK_OUTPUT_DIR/h5diff_attr2.h5
#RUNTEST h5diff_703.txt --verbose=2 $REPACK_OUTPUT_DIR/h5diff_attr1.h5 $REPACK_OUTPUT_DIR/h5diff_attr2.h5

# same attr number , all same attr name
#RUNTEST h5diff_704.txt -v2 $REPACK_OUTPUT_DIR/h5diff_attr_v_level1.h5 $REPACK_OUTPUT_DIR/h5diff_attr_v_level2.h5 /g

# same attr number , some same attr name
#RUNTEST h5diff_705.txt -v2 $REPACK_OUTPUT_DIR/h5diff_attr_v_level1.h5 $REPACK_OUTPUT_DIR/h5diff_attr_v_level2.h5 /dset

# same attr number , all different attr name
#RUNTEST h5diff_706.txt -v2 $REPACK_OUTPUT_DIR/h5diff_attr_v_level1.h5 $REPACK_OUTPUT_DIR/h5diff_attr_v_level2.h5 /ntype

# different attr number , same attr name (intersected)
#RUNTEST h5diff_707.txt -v2 $REPACK_OUTPUT_DIR/h5diff_attr_v_level1.h5 $REPACK_OUTPUT_DIR/h5diff_attr_v_level2.h5 /g2

# different attr number , all different attr name
#RUNTEST h5diff_708.txt -v2 $REPACK_OUTPUT_DIR/h5diff_attr_v_level1.h5 $REPACK_OUTPUT_DIR/h5diff_attr_v_level2.h5 /g3

# when no attributes exist in both objects
#RUNTEST h5diff_709.txt -v2 $REPACK_OUTPUT_DIR/h5diff_attr_v_level1.h5 $REPACK_OUTPUT_DIR/h5diff_attr_v_level2.h5 /g4

# file vs file
#RUNTEST h5diff_710.txt -v2 $REPACK_OUTPUT_DIR/h5diff_attr_v_level1.h5 $REPACK_OUTPUT_DIR/h5diff_attr_v_level2.h5

# ##############################################################################
# 8.  all dataset datatypes
# ##############################################################################
#RUNTEST h5diff_80.txt -v $REPACK_OUTPUT_DIR/h5diff_dset1.h5 $REPACK_OUTPUT_DIR/h5diff_dset2.h5

# 9. compare a file with itself
# Non-comparable empty datasets on DAOS
#RUNTEST h5diff_90.txt -v $REPACK_OUTPUT_DIR/h5diff_basic2.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5

# 10. read by hyperslab, print indexes
RUNTEST h5diff_100.txt -v $REPACK_OUTPUT_DIR/h5diff_hyper1.h5 $REPACK_OUTPUT_DIR/h5diff_hyper2.h5

# 11. floating point comparison
# double value
RUNTEST h5diff_101.txt -v $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 g1/d1  g1/d2

# float value
RUNTEST h5diff_102.txt -v $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 g1/fp1 g1/fp2

# with --use-system-epsilon for double value
RUNTEST h5diff_103.txt -v --use-system-epsilon $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 g1/d1  g1/d2

# with --use-system-epsilon for float value
RUNTEST h5diff_104.txt -v --use-system-epsilon $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 g1/fp1 g1/fp2

# not comparable -c flag
#RUNTEST h5diff_200.txt $REPACK_OUTPUT_DIR/h5diff_basic2.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 g2/dset1  g2/dset2

#RUNTEST h5diff_201.txt -c $REPACK_OUTPUT_DIR/h5diff_basic2.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 g2/dset1  g2/dset2

RUNTEST h5diff_202.txt -c $REPACK_OUTPUT_DIR/h5diff_basic2.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 g2/dset2  g2/dset3

RUNTEST h5diff_203.txt -c $REPACK_OUTPUT_DIR/h5diff_basic2.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 g2/dset3  g2/dset4

RUNTEST h5diff_204.txt -c $REPACK_OUTPUT_DIR/h5diff_basic2.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 g2/dset4  g2/dset5

RUNTEST h5diff_205.txt -c $REPACK_OUTPUT_DIR/h5diff_basic2.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 g2/dset5  g2/dset6

# not comparable in compound
RUNTEST h5diff_206.txt -c $REPACK_OUTPUT_DIR/h5diff_basic2.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 g2/dset7  g2/dset8

# Non-comparable empty datasets on DAOS
#RUNTEST h5diff_207.txt -c $REPACK_OUTPUT_DIR/h5diff_basic2.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 g2/dset8  g2/dset9

# not comparable in dataspace of zero dimension size
#RUNTEST h5diff_208.txt -c $REPACK_OUTPUT_DIR/h5diff_dset_zero_dim_size1.h5 $REPACK_OUTPUT_DIR/h5diff_dset_zero_dim_size2.h5

# non-comparable dataset with comparable attribute, and other comparable datasets.
# Also test non-compatible attributes with different type, dimention, rank.
# All the comparables should display differences.
RUNTEST h5diff_220.txt -c $REPACK_OUTPUT_DIR/non_comparables1.h5 $REPACK_OUTPUT_DIR/non_comparables2.h5 /g1

# comparable dataset with non-comparable attribute and other comparable attributes.
# All the comparables should display differences.
RUNTEST h5diff_221.txt -c $REPACK_OUTPUT_DIR/non_comparables1.h5 $REPACK_OUTPUT_DIR/non_comparables2.h5 /g2

# entire file
# All the comparables should display differences.
RUNTEST h5diff_222.txt -c $REPACK_OUTPUT_DIR/non_comparables1.h5 $REPACK_OUTPUT_DIR/non_comparables2.h5

# non-comparable test for common objects (same name) with different object types
# (HDFFV-7644)
RUNTEST h5diff_223.txt -c $REPACK_OUTPUT_DIR/non_comparables1.h5 $REPACK_OUTPUT_DIR/non_comparables2.h5 /diffobjtypes
# swap files
RUNTEST h5diff_224.txt -c $REPACK_OUTPUT_DIR/non_comparables2.h5 $REPACK_OUTPUT_DIR/non_comparables1.h5 /diffobjtypes

# ##############################################################################
# # Links compare without --follow-symlinks nor --no-dangling-links
# ##############################################################################
# test for bug1749
#RUNTEST h5diff_300.txt -v h5diff_links.h5 h5diff_links.h5 /link_g1 /link_g2

# ##############################################################################
# # Links compare with --follow-symlinks Only
# ##############################################################################
# soft links file to file
#RUNTEST h5diff_400.txt --follow-symlinks -v h5diff_softlinks.h5 h5diff_softlinks.h5

# softlink vs dset"
#RUNTEST h5diff_401.txt --follow-symlinks -v h5diff_softlinks.h5 h5diff_softlinks.h5 /softlink_dset1_1 /target_dset2

# dset vs softlink"
#RUNTEST h5diff_402.txt --follow-symlinks -v h5diff_softlinks.h5 h5diff_softlinks.h5 /target_dset2 /softlink_dset1_1

# softlink vs softlink"
#RUNTEST h5diff_403.txt --follow-symlinks -v h5diff_softlinks.h5 h5diff_softlinks.h5 /softlink_dset1_1 /softlink_dset2

# extlink vs extlink (FILE)"
#RUNTEST h5diff_404.txt --follow-symlinks -v h5diff_extlink_src.h5 h5diff_extlink_src.h5

# extlink vs dset"
#RUNTEST h5diff_405.txt --follow-symlinks -v h5diff_extlink_src.h5 h5diff_extlink_trg.h5 /ext_link_dset1 /target_group2/x_dset

# dset vs extlink"
#RUNTEST h5diff_406.txt --follow-symlinks -v h5diff_extlink_trg.h5 h5diff_extlink_src.h5 /target_group2/x_dset /ext_link_dset1

# extlink vs extlink"
#RUNTEST h5diff_407.txt --follow-symlinks -v h5diff_extlink_src.h5 h5diff_extlink_src.h5 /ext_link_dset1 /ext_link_dset2

# softlink vs extlink"
#RUNTEST h5diff_408.txt --follow-symlinks -v h5diff_softlinks.h5 h5diff_extlink_src.h5 /softlink_dset1_1 /ext_link_dset2

# extlink vs softlink "
#RUNTEST h5diff_409.txt --follow-symlinks -v h5diff_extlink_src.h5 h5diff_softlinks.h5 /ext_link_dset2 /softlink_dset1_1

# linked_softlink vs linked_softlink (FILE)"
#RUNTEST h5diff_410.txt --follow-symlinks -v h5diff_linked_softlink.h5 h5diff_linked_softlink.h5

# dset2 vs linked_softlink_dset1"
#RUNTEST h5diff_411.txt --follow-symlinks -v h5diff_linked_softlink.h5 h5diff_linked_softlink.h5 /target_dset2 /softlink1_to_slink2

# linked_softlink_dset1 vs dset2"
#RUNTEST h5diff_412.txt --follow-symlinks -v h5diff_linked_softlink.h5 h5diff_linked_softlink.h5 /softlink1_to_slink2 /target_dset2

# linked_softlink_to_dset1 vs linked_softlink_to_dset2"
#RUNTEST h5diff_413.txt --follow-symlinks -v h5diff_linked_softlink.h5 h5diff_linked_softlink.h5 /softlink1_to_slink2 /softlink2_to_slink2

# group vs linked_softlink_group1"
#RUNTEST h5diff_414.txt --follow-symlinks -v h5diff_linked_softlink.h5 h5diff_linked_softlink.h5 /target_group /softlink3_to_slink2

# linked_softlink_group1 vs group"
#RUNTEST h5diff_415.txt --follow-symlinks -v h5diff_linked_softlink.h5 h5diff_linked_softlink.h5 /softlink3_to_slink2 /target_group

# linked_softlink_to_group1 vs linked_softlink_to_group2"
#RUNTEST h5diff_416.txt --follow-symlinks -v h5diff_linked_softlink.h5 h5diff_linked_softlink.h5 /softlink3_to_slink2 /softlink4_to_slink2

# non-exist-softlink vs softlink"
#RUNTEST h5diff_417.txt --follow-symlinks -v h5diff_softlinks.h5 h5diff_softlinks.h5 /softlink_noexist /softlink_dset2

# softlink vs non-exist-softlink"
#RUNTEST h5diff_418.txt --follow-symlinks -v h5diff_softlinks.h5 h5diff_softlinks.h5 /softlink_dset2 /softlink_noexist

# non-exist-extlink_file vs extlink"
#RUNTEST h5diff_419.txt --follow-symlinks -v h5diff_extlink_src.h5 h5diff_extlink_src.h5 /ext_link_noexist2 /ext_link_dset2

# exlink vs non-exist-extlink_file"
#RUNTEST h5diff_420.txt --follow-symlinks -v h5diff_extlink_src.h5 h5diff_extlink_src.h5 /ext_link_dset2 /ext_link_noexist2

# extlink vs non-exist-extlink_obj"
#RUNTEST h5diff_421.txt --follow-symlinks -v h5diff_extlink_src.h5 h5diff_extlink_src.h5 /ext_link_dset2 /ext_link_noexist1

# non-exist-extlink_obj vs extlink"
#RUNTEST h5diff_422.txt --follow-symlinks -v h5diff_extlink_src.h5 h5diff_extlink_src.h5 /ext_link_noexist1 /ext_link_dset2

# extlink_to_softlink_to_dset1 vs dset2"
#RUNTEST h5diff_423.txt --follow-symlinks -v h5diff_ext2softlink_src.h5 h5diff_ext2softlink_trg.h5 /ext_link_to_slink1 /dset2

# dset2 vs extlink_to_softlink_to_dset1"
#RUNTEST h5diff_424.txt --follow-symlinks -v h5diff_ext2softlink_trg.h5 h5diff_ext2softlink_src.h5 /dset2 /ext_link_to_slink1

# extlink_to_softlink_to_dset1 vs extlink_to_softlink_to_dset2"
#RUNTEST h5diff_425.txt --follow-symlinks -v h5diff_ext2softlink_src.h5 h5diff_ext2softlink_src.h5 /ext_link_to_slink1 /ext_link_to_slink2

# ##############################################################################
# # Dangling links compare (--follow-symlinks and --no-dangling-links)
# ##############################################################################
# dangling links --follow-symlinks (FILE to FILE)
#RUNTEST h5diff_450.txt  --follow-symlinks -v h5diff_danglelinks1.h5 h5diff_danglelinks2.h5

# dangling links --follow-symlinks and --no-dangling-links (FILE to FILE)
#RUNTEST h5diff_451.txt  --follow-symlinks -v --no-dangling-links  h5diff_danglelinks1.h5 h5diff_danglelinks2.h5

# try --no-dangling-links without --follow-symlinks options
#RUNTEST h5diff_452.txt  --no-dangling-links  h5diff_softlinks.h5 h5diff_softlinks.h5

# dangling link found for soft links (FILE to FILE)
#RUNTEST h5diff_453.txt  --follow-symlinks -v --no-dangling-links  h5diff_softlinks.h5 h5diff_softlinks.h5

# dangling link found for soft links (obj to obj)
#RUNTEST h5diff_454.txt  --follow-symlinks -v --no-dangling-links  h5diff_softlinks.h5 h5diff_softlinks.h5 /softlink_dset2 /softlink_noexist

# dangling link found for soft links (obj to obj) Both dangle links
#RUNTEST h5diff_455.txt  --follow-symlinks -v --no-dangling-links  h5diff_softlinks.h5 h5diff_softlinks.h5 /softlink_noexist /softlink_noexist

# dangling link found for ext links (FILE to FILE)
#RUNTEST h5diff_456.txt  --follow-symlinks -v --no-dangling-links  h5diff_extlink_src.h5 h5diff_extlink_src.h5

# dangling link found for ext links (obj to obj). target file exist
#RUNTEST h5diff_457.txt  --follow-symlinks -v --no-dangling-links  h5diff_extlink_src.h5 h5diff_extlink_src.h5 /ext_link_dset1 /ext_link_noexist1

# dangling link found for ext links (obj to obj). target file NOT exist
#RUNTEST h5diff_458.txt  --follow-symlinks -v --no-dangling-links  h5diff_extlink_src.h5 h5diff_extlink_src.h5 /ext_link_dset1 /ext_link_noexist2

# dangling link found for ext links (obj to obj). Both dangle links
#RUNTEST h5diff_459.txt  --follow-symlinks -v --no-dangling-links  h5diff_extlink_src.h5 h5diff_extlink_src.h5 /ext_link_noexist1 /ext_link_noexist2

# dangling link --follow-symlinks (obj vs obj)
# (HDFFV-7836)
#RUNTEST h5diff_465.txt --follow-symlinks h5diff_danglelinks1.h5 h5diff_danglelinks2.h5 /soft_link1
# (HDFFV-7835)
# soft dangling vs. soft dangling
#RUNTEST h5diff_466.txt -v --follow-symlinks h5diff_danglelinks1.h5 h5diff_danglelinks2.h5 /soft_link1
# soft link  vs. soft dangling
#RUNTEST h5diff_467.txt -v --follow-symlinks h5diff_danglelinks1.h5 h5diff_danglelinks2.h5 /soft_link2
# ext dangling vs. ext dangling
#RUNTEST h5diff_468.txt -v --follow-symlinks h5diff_danglelinks1.h5 h5diff_danglelinks2.h5 /ext_link4
# ext link vs. ext dangling
#RUNTEST h5diff_469.txt -v --follow-symlinks h5diff_danglelinks1.h5 h5diff_danglelinks2.h5 /ext_link2

#----------------------------------------
# dangling links without follow symlink
# (HDFFV-7998)
# test - soft dangle links (same and different paths),
#      - external dangle links (same and different paths)
#RUNTEST h5diff_471.txt -v h5diff_danglelinks1.h5 h5diff_danglelinks2.h5
#RUNTEST h5diff_472.txt -v h5diff_danglelinks1.h5 h5diff_danglelinks2.h5 /soft_link1
#RUNTEST h5diff_473.txt -v h5diff_danglelinks1.h5 h5diff_danglelinks2.h5 /soft_link4
#RUNTEST h5diff_474.txt -v h5diff_danglelinks1.h5 h5diff_danglelinks2.h5 /ext_link4
#RUNTEST h5diff_475.txt -v h5diff_danglelinks1.h5 h5diff_danglelinks2.h5 /ext_link1

# ##############################################################################
# # test for group diff recursivly
# ##############################################################################
# root
#RUNTEST h5diff_500.txt -v h5diff_grp_recurse1.h5 h5diff_grp_recurse2.h5 / /
#RUNTEST h5diff_501.txt -v --follow-symlinks h5diff_grp_recurse1.h5 h5diff_grp_recurse2.h5 / /

# root vs group
#RUNTEST h5diff_502.txt -v h5diff_grp_recurse1.h5 h5diff_grp_recurse2.h5 / /grp1/grp2/grp3

# group vs group (same name and structure)
#RUNTEST h5diff_503.txt -v h5diff_grp_recurse1.h5 h5diff_grp_recurse2.h5 /grp1 /grp1

# group vs group (different name and structure)
#RUNTEST h5diff_504.txt -v h5diff_grp_recurse1.h5 h5diff_grp_recurse2.h5 /grp1/grp2 /grp1/grp2/grp3

# groups vs soft-link
#RUNTEST h5diff_505.txt -v h5diff_grp_recurse1.h5 h5diff_grp_recurse2.h5 /grp1 /slink_grp1
#RUNTEST h5diff_506.txt -v --follow-symlinks h5diff_grp_recurse1.h5 h5diff_grp_recurse2.h5 /grp1/grp2 /slink_grp2

# groups vs ext-link
#RUNTEST h5diff_507.txt -v h5diff_grp_recurse1.h5 h5diff_grp_recurse2.h5 /grp1 /elink_grp1
#RUNTEST h5diff_508.txt -v --follow-symlinks h5diff_grp_recurse1.h5 h5diff_grp_recurse2.h5 /grp1 /elink_grp1

# soft-link vs ext-link
#RUNTEST h5diff_509.txt -v h5diff_grp_recurse1.h5 h5diff_grp_recurse2.h5 /slink_grp1 /elink_grp1
#RUNTEST h5diff_510.txt -v --follow-symlinks h5diff_grp_recurse1.h5 h5diff_grp_recurse2.h5 /slink_grp1 /elink_grp1

# circled ext links
#RUNTEST h5diff_511.txt -v h5diff_grp_recurse1.h5 h5diff_grp_recurse2.h5 /grp10 /grp11
#RUNTEST h5diff_512.txt -v --follow-symlinks h5diff_grp_recurse1.h5 h5diff_grp_recurse2.h5 /grp10 /grp11

# circled soft2ext-link vs soft2ext-link
#RUNTEST h5diff_513.txt -v h5diff_grp_recurse1.h5 h5diff_grp_recurse2.h5 /slink_grp10 /slink_grp11
#RUNTEST h5diff_514.txt -v --follow-symlinks h5diff_grp_recurse1.h5 h5diff_grp_recurse2.h5 /slink_grp10 /slink_grp11

###############################################################################
# Test for group recursive diff via multi-linked external links
# With follow-symlinks, file h5diff_grp_recurse_ext1.h5 and h5diff_grp_recurse_ext2-1.h5 should
# be same with the external links.
###############################################################################
# file vs file
#RUNTEST h5diff_515.txt -v h5diff_grp_recurse_ext1.h5 h5diff_grp_recurse_ext2-1.h5
#RUNTEST h5diff_516.txt -v --follow-symlinks h5diff_grp_recurse_ext1.h5 h5diff_grp_recurse_ext2-1.h5
# group vs group
#RUNTEST h5diff_517.txt -v h5diff_grp_recurse_ext1.h5 h5diff_grp_recurse_ext2-1.h5 /g1
#RUNTEST h5diff_518.txt -v --follow-symlinks h5diff_grp_recurse_ext1.h5 h5diff_grp_recurse_ext2-1.h5 /g1

# ##############################################################################
# # Exclude objects (--exclude-path)
# ##############################################################################
#
# Same structure, same names and different value.
#
# Exclude the object with different value. Expect return - same
RUNTEST h5diff_480.txt -v --exclude-path /group1/dset3 $REPACK_OUTPUT_DIR/h5diff_exclude1-1.h5 $REPACK_OUTPUT_DIR/h5diff_exclude1-2.h5
# Verify different by not excluding. Expect return - diff
RUNTEST h5diff_481.txt -v $REPACK_OUTPUT_DIR/h5diff_exclude1-1.h5 $REPACK_OUTPUT_DIR/h5diff_exclude1-2.h5

#
# Different structure, different names.
#
# Exclude all the different objects. Expect return - same
RUNTEST h5diff_482.txt -v --exclude-path "/group1" --exclude-path "/dset1" $REPACK_OUTPUT_DIR/h5diff_exclude2-1.h5 $REPACK_OUTPUT_DIR/h5diff_exclude2-2.h5
# Exclude only some different objects. Expect return - diff
RUNTEST h5diff_483.txt -v --exclude-path "/group1" $REPACK_OUTPUT_DIR/h5diff_exclude2-1.h5 $REPACK_OUTPUT_DIR/h5diff_exclude2-2.h5

# Exclude from group compare
RUNTEST h5diff_484.txt -v --exclude-path "/dset3" $REPACK_OUTPUT_DIR/h5diff_exclude1-1.h5 $REPACK_OUTPUT_DIR/h5diff_exclude1-2.h5 /group1

#
# Only one file contains unique objs. Common objs are same.
# (HDFFV-7837)
#
RUNTEST h5diff_485.txt -v --exclude-path "/group1" $REPACK_OUTPUT_DIR/h5diff_exclude3-1.h5 $REPACK_OUTPUT_DIR/h5diff_exclude3-2.h5
RUNTEST h5diff_486.txt -v --exclude-path "/group1" $REPACK_OUTPUT_DIR/h5diff_exclude3-2.h5 $REPACK_OUTPUT_DIR/h5diff_exclude3-1.h5
RUNTEST h5diff_487.txt -v --exclude-path "/group1/dset" $REPACK_OUTPUT_DIR/h5diff_exclude3-1.h5 $REPACK_OUTPUT_DIR/h5diff_exclude3-2.h5


# ##############################################################################
# # diff various multiple vlen and fixed strings in a compound type dataset
# ##############################################################################
#RUNTEST h5diff_530.txt -v  $REPACK_OUTPUT_DIR/h5diff_comp_vl_strs.h5 $REPACK_OUTPUT_DIR/h5diff_comp_vl_strs.h5 /group /group_copy
# test to verify HDFFV-8625
#RUNTEST h5diff_8625.txt -v --enable-error-stack $REPACK_OUTPUT_DIR/h5diff_comp_vl_strs.h5 $REPACK_OUTPUT_DIR/h5diff_comp_vl_strs.h5 /group/Compound_dset1 /group_copy/Compound_dset3
# test to verify HDFFV-8639
#RUNTEST h5diff_8639.txt -v $REPACK_OUTPUT_DIR/h5diff_attr3.h5 $REPACK_OUTPUT_DIR/h5diff_attr2.h5 /g1
#RUNTEST h5diff_vlstr.txt -v  $REPACK_OUTPUT_DIR/tvlstr.h5 $REPACK_OUTPUT_DIR/tvlstr2.h5

# ##############################################################################
# # Test container types (array,vlen) with multiple nested compound types
# # Complex compound types in dataset and attribute
# ##############################################################################
#RUNTEST h5diff_540.txt -v $REPACK_OUTPUT_DIR/compounds_array_vlen1.h5 $REPACK_OUTPUT_DIR/compounds_array_vlen2.h5

# ##############################################################################
# # Test mutually exclusive options
# ##############################################################################
#
# Test with -d , -p and --use-system-epsilon.
RUNTEST h5diff_640.txt -v -d 5 -p 0.05 --use-system-epsilon $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 /g1/dset3 /g1/dset4
RUNTEST h5diff_641.txt -v -d 5 -p 0.05 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 /g1/dset3 /g1/dset4
RUNTEST h5diff_642.txt -v -p 0.05 -d 5 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 /g1/dset3 /g1/dset4
RUNTEST h5diff_643.txt -v -d 5 --use-system-epsilon $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 /g1/dset3 /g1/dset4
RUNTEST h5diff_644.txt -v --use-system-epsilon -d 5 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 /g1/dset3 /g1/dset4
RUNTEST h5diff_645.txt -v -p 0.05 --use-system-epsilon $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 /g1/dset3 /g1/dset4
RUNTEST h5diff_646.txt -v --use-system-epsilon -p 0.05 $REPACK_OUTPUT_DIR/h5diff_basic1.h5 $REPACK_OUTPUT_DIR/h5diff_basic2.h5 /g1/dset3 /g1/dset4

# ##############################################################################
# # END
# ##############################################################################

# Clean up generated files/directories
CLEAN_OUTPUT

if test $nerrors -eq 0 ; then
    echo "All $TESTNAME tests passed."
    exit $EXIT_SUCCESS
else
    echo "$TESTNAME tests failed with $nerrors errors."
    exit $EXIT_FAILURE
fi
