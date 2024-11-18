#! /bin/sh
#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the LICENSE file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.

# This file is for use of h5cc created with the CMake process
# HDF5_HOME is expected to be set

srcdir=..
builddir=.
verbose=yes
nerrors=0

# HDF5 compile commands, assuming they are in your $PATH.
H5CC=$HDF5_HOME/bin/h5cc
LD_LIBRARY_PATH=$HDF5_HOME/lib
export LD_LIBRARY_PATH

if ! test -f $H5CC; then
    echo "Set paths for H5CC and LD_LIBRARY_PATH in test.sh"
    echo "Set environment variable HDF5_HOME to the hdf5 install dir"
    echo "h5cc was not found at $H5CC"
    exit $EXIT_FAILURE
fi

H5DUMP=`echo $H5CC | sed -e 's/\/[^/]*$/\/h5dump/'`;
H5_LIBVER=$($H5CC -showconfig | grep -i "HDF5 Version:" | sed 's/^.* //g' | sed 's/[-].*//g')
H5_APIVER=$($H5CC -showconfig | grep -i "Default API mapping:" | sed 's/^.* //g' | sed 's/v//g' | sed 's/1/1_/')

H5_MAJORVER=$(echo $H5_LIBVER | cut -f1 -d'.'  | sed -E 's/\./_/g')
H5_MINORVER=$(echo $H5_LIBVER | cut -f2 -d'.'  | sed -E 's/\./_/g')
H5_RELEASEVER=$(echo $H5_LIBVER | cut -f3 -d'.'  | sed -E 's/\./_/g')
H5_LIBVER_DIR=$H5_MAJORVER$H5_MINORVER

# Shell commands used in Makefiles
RM="rm -rf"
DIFF="diff -c"
CMP="cmp -s"
GREP='grep'
CP="cp -p"  # Use -p to preserve mode,ownership,timestamps
DIRNAME='dirname'
LS='ls'
AWK='awk'

# setup plugin path
ENVCMD="env HDF5_PLUGIN_PATH=$LD_LIBRARY_PATH/plugin"

TESTDIR=$builddir


case `echo "testing\c"; echo 1,2,3`,`echo -n testing; echo 1,2,3` in
  *c*,-n*) ECHO_N= ECHO_C='
' ;;
  *c*,*  ) ECHO_N=-n ECHO_C= ;;
  *)       ECHO_N= ECHO_C='\c' ;;
esac
ECHO_N="echo $ECHO_N"


exout() {
    $*
}

dumpout() {
    $H5DUMP $*
}

# compare current version, required version.
# returns if cur_ver < req_ver is true.
version_compare() {
  version_lt=0
  if [ ! "$(printf '%s\n' "$1" "$2" | sort -V | head -n1)" = "$2" ]; then
          version_lt=1
  fi
}


topics="h5ex_lite3 h5ex_packet_table_FL \
            h5ex_image1 h5ex_image2 \
            h5ex_table_01 h5ex_table_02 h5ex_table_03 h5ex_table_04 \
            h5ex_table_05 h5ex_table_06 h5ex_table_07 h5ex_table_08 \
            h5ex_table_09 h5ex_table_10 h5ex_table_11 h5ex_table_12 \
            h5ex_ds1"

return_val=0

for topic in $topics
do
    $H5CC $srcdir/$topic.c -o $topic
done

for topic in $topics
do
    fname=$topic
    $ECHO_N "Testing C/HL/$fname...$ECHO_C"
    exout ./$fname >tmp.test
    cmp -s tmp.test $srcdir/tfiles/$fname.tst
    status=$?
    if test $status -ne 0
    then
        echo "  FAILED!"
    else
        dumpout $fname.h5 >tmp.test
        rm -f $fname.h5
        cmp -s tmp.test $srcdir/tfiles/$fname.ddl
        status=$?
        if test $status -ne 0
        then
            echo "  FAILED!"
        else
            echo "  Passed"
        fi
    fi
    return_val=`expr $status + $return_val`
done


$H5CC $srcdir/h5ex_lite1.c -o h5ex_lite1
$H5CC $srcdir/h5ex_lite2.c -o h5ex_lite2

$ECHO_N "Testing C/HL/h5ex_lite1...$ECHO_C"
exout ./h5ex_lite1 >tmp.test
cmp -s tmp.test $srcdir/tfiles/h5ex_lite1.tst
status=$?
if test $status -ne 0
then
    echo "  FAILED!"
else
    $ECHO_N "Testing C/HL/h5ex_lite2...$ECHO_C"
    exout ./h5ex_lite2 >tmp.test
    cmp -s tmp.test $srcdir/tfiles/h5ex_lite2.tst
    status=$?
    if test $status -ne 0
    then
        echo "  FAILED!"
    else
        echo "  Passed"
    fi
fi
return_val=`expr $status + $return_val`


rm -f tmp.test
echo "$return_val tests failed in C/HL/"
exit $return_val
