#! /bin/sh
#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5. The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#
srcdir=.
TOP_BUILDDIR=..

# Determine if backward compatibility options enabled
DEPRECATED_SYMBOLS="yes"

EXIT_SUCCESS=0
EXIT_FAILURE=1

nerrors=0
verbose=yes
exit_code=$EXIT_SUCCESS

TOOLS_TEST_DIR=
TOOLS_TESTFILES=
TOOLS_H5DIFF_BIN=
TOOLS_H5LS_BIN=

if [ $# -ge 2 ]
then
	TOOLS_TEST_DIR=$1
    TOOLS_H5DIFF_BIN=$2
    TOOLS_TESTFILES="$TOOLS_TEST_DIR"/../testfiles
    echo "tools_testdir = $TOOLS_TEST_DIR"
    echo "tools_h5diffbin = $TOOLS_H5DIFF_BIN"
    echo "tools_testfiles = $TOOLS_TESTFILES"

    if [ $# -ge 3 ]
    then
		TOOLS_H5LS_BIN=$3
	else
		TOOLS_H5LS_BIN=$2
    fi
else
	echo "Unable to assign input"
    exit $EXIT_FAILURE
fi


echo "$TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5"         > "$TOOLS_TEST_DIR"/demo_srcfiles.test
echo "$TOOLS_TEST_DIR/h5diff/testfiles/h5diff_strings1.h5"       >> "$TOOLS_TEST_DIR"/demo_srcfiles.test
echo "$TOOLS_TEST_DIR/h5diff/testfiles/h5diff_eps1.h5"           >> "$TOOLS_TEST_DIR"/demo_srcfiles.test
echo "$TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr1.h5"          >> "$TOOLS_TEST_DIR"/demo_srcfiles.test
echo "$TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr_v_level1.h5"  >> "$TOOLS_TEST_DIR"/demo_srcfiles.test
echo "$TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dset1.h5"          >> "$TOOLS_TEST_DIR"/demo_srcfiles.test
echo "$TOOLS_TEST_DIR/h5diff/testfiles/h5diff_hyper1.h5"         >> "$TOOLS_TEST_DIR"/demo_srcfiles.test
echo "$TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dset_zero_dim_size1.h5"  >> "$TOOLS_TEST_DIR"/demo_srcfiles.test
echo "$TOOLS_TEST_DIR/h5diff/testfiles/non_comparables1.h5"      >> "$TOOLS_TEST_DIR"/demo_srcfiles.test


echo "$TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5"         > "$TOOLS_TEST_DIR"/demo_destfiles.test
echo "$TOOLS_TEST_DIR/h5diff/testfiles/h5diff_strings2.h5"       >> "$TOOLS_TEST_DIR"/demo_destfiles.test
echo "$TOOLS_TEST_DIR/h5diff/testfiles/h5diff_eps2.h5"           >> "$TOOLS_TEST_DIR"/demo_destfiles.test
echo "$TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr2.h5"          >> "$TOOLS_TEST_DIR"/demo_destfiles.test
echo "$TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr_v_level2.h5"  >> "$TOOLS_TEST_DIR"/demo_destfiles.test
echo "$TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dset2.h5"          >> "$TOOLS_TEST_DIR"/demo_destfiles.test
echo "$TOOLS_TEST_DIR/h5diff/testfiles/h5diff_hyper2.h5"         >> "$TOOLS_TEST_DIR"/demo_destfiles.test
echo "$TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dset_zero_dim_size2.h5"  >> "$TOOLS_TEST_DIR"/demo_destfiles.test
echo "$TOOLS_TEST_DIR/h5diff/testfiles/non_comparables2.h5"      >> "$TOOLS_TEST_DIR"/demo_destfiles.test
echo "$TOOLS_H5DIFF_BIN/h5diff $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5" > $TOOLS_TEST_DIR/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset1 g1/dset2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -r $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -r $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset1 g1/dset2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --report --delta=5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v -p 0.02 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 g1/dset5 g1/dset6" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --verbose --relative=0.02 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 g1/dset7 g1/dset8" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v -p 0.02 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 g1/dset9 g1/dset10" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 /g1/fp19 /g1/fp19_COPY" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 /g1/fp20 /g1/fp20_COPY" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -q $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v -q $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 dset g1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 dset l1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 dset t1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 g1 g1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 t1 t1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 l1 l1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 g1 g2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 t1 t2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 l1 l2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_enum_invalid_values.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_enum_invalid_values.h5 dset1 dset2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 dset0a dset0b" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 dset1a dset1b" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 dset2a dset2b" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 dset3a dset4b" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 dset4a dset4b" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 dset5a dset5b" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 dset6a dset6b" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 dset7a dset7b" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v2 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dset1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dset2.h5 refreg" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v2 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dset1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dset2.h5 /g1/reference2D" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v2 $TOOLS_TEST_DIR/h5diff/testfiles/trefer_attr.h5 $TOOLS_TEST_DIR/h5diff/testfiles/trefer_ext2.h5 Dataset3 Dataset3" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 dset11a dset11b" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_strings1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_strings2.h5 string1 string1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_strings1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_strings2.h5 string2 string2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_strings1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_strings2.h5 string3 string3" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_strings1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_strings2.h5 string4 string4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 nono_obj" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -d -4 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -d 0 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -d u $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -d 0x1 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -d 1 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --use-system-epsilon $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -d 200 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -d 1 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v3 -d 1e-16 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_eps1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_eps2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -p -4 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -p 0 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -p u $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -p 0x1 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -p 0.21 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -p 0.21 -p 0.22 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -p 2 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -p 0.005 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -n -4 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -n 0 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -n 0x1 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -n 2 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -n 2 -n 3 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --count=200 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -n 1 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v -d 0.0001 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 g1/fp18 g1/fp18_COPY" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v --use-system-epsilon $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 g1/fp18 g1/fp18_COPY" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v1 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v2 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --verbose=1 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --verbose=2 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v2 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr_v_level1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr_v_level2.h5 /g" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v2 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr_v_level1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr_v_level2.h5 /dset" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v2 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr_v_level1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr_v_level2.h5 /ntype" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v2 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr_v_level1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr_v_level2.h5 /g2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v2 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr_v_level1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr_v_level2.h5 /g3" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v2 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr_v_level1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr_v_level2.h5 /g4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v2 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr_v_level1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr_v_level2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dset1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dset2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_hyper1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_hyper2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 g1/d1 g1/d2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 g1/fp1 g1/fp2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v --use-system-epsilon $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 g1/d1 g1/d2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v --use-system-epsilon $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 g1/fp1 g1/fp2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g2/dset1 g2/dset2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -c $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g2/dset1 g2/dset2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -c $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g2/dset2 g2/dset3" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -c $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g2/dset3 g2/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -c $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g2/dset4 g2/dset5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -c $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g2/dset5 g2/dset6" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -c $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g2/dset7 g2/dset8" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -c $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g2/dset8 g2/dset9" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -c $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dset_zero_dim_size1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dset_zero_dim_size2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -c $TOOLS_TEST_DIR/h5diff/testfiles/non_comparables1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/non_comparables2.h5 /g1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -c $TOOLS_TEST_DIR/h5diff/testfiles/non_comparables1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/non_comparables2.h5 /g2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -c $TOOLS_TEST_DIR/h5diff/testfiles/non_comparables1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/non_comparables2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -c $TOOLS_TEST_DIR/h5diff/testfiles/non_comparables1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/non_comparables2.h5 /diffobjtypes" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -c $TOOLS_TEST_DIR/h5diff/testfiles/non_comparables2.h5 $TOOLS_TEST_DIR/h5diff/testfiles/non_comparables1.h5 /diffobjtypes" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_links.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_links.h5 /link_g1 /link_g2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 /softlink_dset1_1 /target_dset2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 /target_dset2 /softlink_dset1_1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 /softlink_dset1_1 /softlink_dset2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_trg.h5 /ext_link_dset1 /target_group2/x_dset" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_trg.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 /target_group2/x_dset /ext_link_dset1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 /ext_link_dset1 /ext_link_dset2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 /softlink_dset1_1 /ext_link_dset2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 /ext_link_dset2 /softlink_dset1_1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_linked_softlink.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_linked_softlink.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_linked_softlink.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_linked_softlink.h5 /target_dset2 /softlink1_to_slink2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_linked_softlink.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_linked_softlink.h5 /softlink1_to_slink2 /target_dset2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_linked_softlink.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_linked_softlink.h5 /softlink1_to_slink2 /softlink2_to_slink2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_linked_softlink.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_linked_softlink.h5 /target_group /softlink3_to_slink2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_linked_softlink.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_linked_softlink.h5 /softlink3_to_slink2 /target_group" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_linked_softlink.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_linked_softlink.h5 /softlink3_to_slink2 /softlink4_to_slink2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 /softlink_noexist /softlink_dset2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 /softlink_dset2 /softlink_noexist" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 /ext_link_noexist2 /ext_link_dset2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 /ext_link_dset2 /ext_link_noexist2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 /ext_link_dset2 /ext_link_noexist1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 /ext_link_noexist1 /ext_link_dset2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_ext2softlink_src.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_ext2softlink_trg.h5 /ext_link_to_slink1 /dset2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_ext2softlink_trg.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_ext2softlink_src.h5 /dset2 /ext_link_to_slink1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_ext2softlink_src.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_ext2softlink_src.h5 /ext_link_to_slink1 /ext_link_to_slink2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v --no-dangling-links $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --no-dangling-links $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v --no-dangling-links $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v --no-dangling-links $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 /softlink_dset2 /softlink_noexist" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v --no-dangling-links $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 /softlink_noexist /softlink_noexist" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v --no-dangling-links $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v --no-dangling-links $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 /ext_link_dset1 /ext_link_noexist1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v --no-dangling-links $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 /ext_link_dset1 /ext_link_noexist2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v --no-dangling-links $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 /ext_link_noexist1 /ext_link_noexist2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks2.h5 /soft_link1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v --follow-symlinks $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks2.h5 /soft_link1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v --follow-symlinks $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks2.h5 /soft_link2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v --follow-symlinks $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks2.h5 /ext_link4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v --follow-symlinks $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks2.h5 /ext_link2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks2.h5 /soft_link1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks2.h5 /soft_link4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks2.h5 /ext_link4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset1 g1/dset2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -r $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -r $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset1 g1/dset2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --report --delta=5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v -p 0.02 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 g1/dset5 g1/dset6" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --verbose --relative=0.02 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 g1/dset7 g1/dset8" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v -p 0.02 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 g1/dset9 g1/dset10" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 /g1/fp19 /g1/fp19_COPY" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 /g1/fp20 /g1/fp20_COPY" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -q $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v -q $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 dset g1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 dset l1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 dset t1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 g1 g1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 t1 t1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 l1 l1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 g1 g2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 t1 t2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_types.h5 l1 l2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_enum_invalid_values.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_enum_invalid_values.h5 dset1 dset2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 dset0a dset0b" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 dset1a dset1b" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 dset2a dset2b" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 dset3a dset4b" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 dset4a dset4b" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 dset5a dset5b" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 dset6a dset6b" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 dset7a dset7b" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v2 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dset1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dset2.h5 refreg" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v2 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dset1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dset2.h5 /g1/reference2D" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v2 $TOOLS_TEST_DIR/h5diff/testfiles/trefer_attr.h5 $TOOLS_TEST_DIR/h5diff/testfiles/trefer_ext2.h5 Dataset3 Dataset3" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dtypes.h5 dset11a dset11b" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_strings1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_strings2.h5 string1 string1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_strings1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_strings2.h5 string2 string2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_strings1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_strings2.h5 string3 string3" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_strings1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_strings2.h5 string4 string4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 nono_obj" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -d -4 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -d 0 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -d u $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -d 0x1 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -d 1 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --use-system-epsilon $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -d 200 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -d 1 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v3 -d 1e-16 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_eps1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_eps2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -p -4 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -p 0 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -p u $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -p 0x1 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -p 0.21 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -p 0.21 -p 0.22 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -p 2 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -p 0.005 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -n -4 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -n 0 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -n u $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -n 0x1 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -n 2 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -n 2 -n 3 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --count=200 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -n 1 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g1/dset3 g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v -d 0.0001 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 g1/fp18 g1/fp18_COPY" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v --use-system-epsilon $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 g1/fp18 g1/fp18_COPY" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v1 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v2 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --verbose=1 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --verbose=2 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v2 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr_v_level1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr_v_level2.h5 /g" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v2 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr_v_level1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr_v_level2.h5 /dset" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v2 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr_v_level1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr_v_level2.h5 /ntype" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v2 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr_v_level1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr_v_level2.h5 /g2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v2 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr_v_level1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr_v_level2.h5 /g3" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v2 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr_v_level1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr_v_level2.h5 /g4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v2 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr_v_level1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr_v_level2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dset1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dset2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_hyper1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_hyper2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 g1/d1 g1/d2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 g1/fp1 g1/fp2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v --use-system-epsilon $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 g1/d1 g1/d2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v --use-system-epsilon $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 g1/fp1 g1/fp2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g2/dset1 g2/dset2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -c $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g2/dset1 g2/dset2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -c $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g2/dset2 g2/dset3" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -c $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g2/dset3 g2/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -c $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g2/dset4 g2/dset5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -c $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g2/dset5 g2/dset6" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -c $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g2/dset7 g2/dset8" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -c $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 g2/dset8 g2/dset9" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -c $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dset_zero_dim_size1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dset_zero_dim_size2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -c $TOOLS_TEST_DIR/h5diff/testfiles/non_comparables1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/non_comparables2.h5 /g1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -c $TOOLS_TEST_DIR/h5diff/testfiles/non_comparables1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/non_comparables2.h5 /g2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -c $TOOLS_TEST_DIR/h5diff/testfiles/non_comparables1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/non_comparables2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -c $TOOLS_TEST_DIR/h5diff/testfiles/non_comparables1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/non_comparables2.h5 /diffobjtypes" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -c $TOOLS_TEST_DIR/h5diff/testfiles/non_comparables2.h5 $TOOLS_TEST_DIR/h5diff/testfiles/non_comparables1.h5 /diffobjtypes" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_links.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_links.h5 /link_g1 /link_g2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 /softlink_dset1_1 /target_dset2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 /target_dset2 /softlink_dset1_1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 /softlink_dset1_1 /softlink_dset2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_trg.h5 /ext_link_dset1 /target_group2/x_dset" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_trg.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 /target_group2/x_dset /ext_link_dset1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 /ext_link_dset1 /ext_link_dset2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 /softlink_dset1_1 /ext_link_dset2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 /ext_link_dset2 /softlink_dset1_1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_linked_softlink.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_linked_softlink.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_linked_softlink.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_linked_softlink.h5 /target_dset2 /softlink1_to_slink2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_linked_softlink.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_linked_softlink.h5 /softlink1_to_slink2 /target_dset2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_linked_softlink.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_linked_softlink.h5 /softlink1_to_slink2 /softlink2_to_slink2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_linked_softlink.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_linked_softlink.h5 /target_group /softlink3_to_slink2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_linked_softlink.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_linked_softlink.h5 /softlink3_to_slink2 /target_group" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_linked_softlink.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_linked_softlink.h5 /softlink3_to_slink2 /softlink4_to_slink2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 /softlink_noexist /softlink_dset2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 /softlink_dset2 /softlink_noexist" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 /ext_link_noexist2 /ext_link_dset2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 /ext_link_dset2 /ext_link_noexist2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 /ext_link_dset2 /ext_link_noexist1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 /ext_link_noexist1 /ext_link_dset2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_ext2softlink_src.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_ext2softlink_trg.h5 /ext_link_to_slink1 /dset2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_ext2softlink_trg.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_ext2softlink_src.h5 /dset2 /ext_link_to_slink1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_ext2softlink_src.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_ext2softlink_src.h5 /ext_link_to_slink1 /ext_link_to_slink2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v --no-dangling-links $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --no-dangling-links $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v --no-dangling-links $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v --no-dangling-links $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 /softlink_dset2 /softlink_noexist" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v --no-dangling-links $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_softlinks.h5 /softlink_noexist /softlink_noexist" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v --no-dangling-links $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v --no-dangling-links $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 /ext_link_dset1 /ext_link_noexist1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v --no-dangling-links $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 /ext_link_dset1 /ext_link_noexist2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks -v --no-dangling-links $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_extlink_src.h5 /ext_link_noexist1 /ext_link_noexist2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --follow-symlinks $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks2.h5 /soft_link1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v --follow-symlinks $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks2.h5 /soft_link1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v --follow-symlinks $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks2.h5 /soft_link2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v --follow-symlinks $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks2.h5 /ext_link4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v --follow-symlinks $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks2.h5 /ext_link2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks2.h5 /soft_link1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks2.h5 /soft_link4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks2.h5 /ext_link4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_danglelinks2.h5 /ext_link1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse2.h5 / /" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v --follow-symlinks $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse2.h5 / /" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse2.h5 / /grp1/grp2/grp3" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse2.h5 /grp1 /grp1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse2.h5 /grp1/grp2 /grp1/grp2/grp3" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse2.h5 /grp1 /slink_grp1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v --follow-symlinks $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse2.h5 /grp1/grp2 /slink_grp2" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse2.h5 /grp1 /elink_grp1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v --follow-symlinks $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse2.h5 /grp1 /elink_grp1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse2.h5 /slink_grp1 /elink_grp1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v --follow-symlinks $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse2.h5 /slink_grp1 /elink_grp1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse2.h5 /grp10 /grp11" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v --follow-symlinks $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse2.h5 /grp10 /grp11" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse2.h5 /slink_grp10 /slink_grp11" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v --follow-symlinks $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse2.h5 /slink_grp10 /slink_grp11" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse_ext1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse_ext2-1.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v --follow-symlinks $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse_ext1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse_ext2-1.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse_ext1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse_ext2-1.h5 /g1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v --follow-symlinks $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse_ext1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_grp_recurse_ext2-1.h5 /g1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v --exclude-path /group1/dset3 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_exclude1-1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_exclude1-2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_exclude1-1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_exclude1-2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v --exclude-path /group1 --exclude-path /dset1 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_exclude2-1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_exclude2-2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v --exclude-path /group1 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_exclude2-1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_exclude2-2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v --exclude-path /dset3 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_exclude1-1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_exclude1-2.h5 /group1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v --exclude-path /group1 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_exclude3-1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_exclude3-2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v --exclude-path /group1 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_exclude3-2.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_exclude3-1.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v --exclude-path /group1/dset $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_exclude3-1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_exclude3-2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_comp_vl_strs.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_comp_vl_strs.h5 /group /group_copy" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v --enable-error-stack $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_comp_vl_strs.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_comp_vl_strs.h5 /group/Compound_dset1 /group_copy/Compound_dset3" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr3.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_attr2.h5 /g1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/tvlstr.h5 $TOOLS_TEST_DIR/h5diff/testfiles/tvlstr2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/compounds_array_vlen1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/compounds_array_vlen2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v -d 5 -p 0.05 --use-system-epsilon $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 /g1/dset3 /g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v -d 5 -p 0.05 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 /g1/dset3 /g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v -p 0.05 -d 5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 /g1/dset3 /g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v -d 5 --use-system-epsilon $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 /g1/dset3 /g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v --use-system-epsilon -d 5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 /g1/dset3 /g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v -p 0.05 --use-system-epsilon $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 /g1/dset3 /g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v --use-system-epsilon -p 0.05 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_basic2.h5 /g1/dset3 /g1/dset4" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dset1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dset2.h5 /g1/array /g1/array" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dset1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dset3.h5 /g1/array /g1/array" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff --enable-error-stack -v $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dset1.h5 $TOOLS_TEST_DIR/h5diff/testfiles/h5diff_dset2.h5 /g1/array3D[0,0,0 2,2,1 2,2,2 ] /g1/array3D[0,0,0 2,2,1 2,2,2 ]" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v 1_vds.h5 2_vds.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -r $TOOLS_TEST_DIR/h5diff/testfiles/1_vds.h5 $TOOLS_TEST_DIR/h5diff/testfiles/2_vds.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -c $TOOLS_TEST_DIR/h5diff/testfiles/1_vds.h5 $TOOLS_TEST_DIR/h5diff/testfiles/2_vds.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/tudfilter.h5 $TOOLS_TEST_DIR/h5diff/testfiles/tudfilter2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5DIFF_BIN/h5diff -v $TOOLS_TEST_DIR/h5diff/testfiles/tudfilter.h5 $TOOLS_TEST_DIR/h5diff/testfiles/tudfilter2.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5LS_BIN/h5ls -w80 $TOOLS_TESTFILES/tall.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5LS_BIN/h5ls -w80 -r -d $TOOLS_TESTFILES/tall.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5LS_BIN/h5ls -w80 $TOOLS_TESTFILES/tgroup.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5LS_BIN/h5ls -w80 $TOOLS_TESTFILES/tgroup.h5 /g1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5LS_BIN/h5ls -w80 -r -g $TOOLS_TESTFILES/tgroup.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5LS_BIN/h5ls -w80 -g $TOOLS_TESTFILES/tgroup.h5 /g1" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5LS_BIN/h5ls -w80 -v -g $TOOLS_TESTFILES/tgrp_comments.h5 /glongcomment" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5LS_BIN/h5ls -w80 -r -d $TOOLS_TESTFILES/tdset.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5LS_BIN/h5ls -w80 -r $TOOLS_TESTFILES/tslink.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5LS_BIN/h5ls --follow-symlinks $TOOLS_TESTFILES/tsoftlinks.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
echo "$TOOLS_H5LS_BIN/h5ls --follow-symlinks -r $TOOLS_TESTFILES/tsoftlinks.h5" >> "$TOOLS_TEST_DIR"/demo_dh5walk.test
