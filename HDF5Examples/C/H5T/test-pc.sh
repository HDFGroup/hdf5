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

# This file is for use of h5cc created with the CMake process.
# Environment variable, HDF5_HOME is expected to be set.
# $1 is the path name of the source directory.
# $2 is the path name of the build directory.
# $3 is the current path name.

top_srcdir=$1
top_builddir=$2
currentpath=$3
verbose=yes
nerrors=0

echo "Current build directory: $top_builddir/$currentpath"

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

TESTDIR=$top_builddir/$currentpath


case `echo "testing\c"; echo 1,2,3`,`echo -n testing; echo 1,2,3` in
  *c*,-n*) ECHO_N= ECHO_C='
' ;;
  *c*,*  ) ECHO_N=-n ECHO_C= ;;
  *)       ECHO_N= ECHO_C='\c' ;;
esac
ECHO_N="echo $ECHO_N"


exout() {
    cd $TESTDIR
    "$@"
}

dumpout() {
    cd $TESTDIR
    $H5DUMP "$@"
}

compileout() {
    cd $TESTDIR
    $H5CC "$@"
}

# compare current version, required version.
# returns if cur_ver < req_ver is true.
version_compare() {
  version_lt=0
  if [ ! "$(printf '%s\n' "$1" "$2" | sort -V | head -n1)" = "$2" ]; then
          version_lt=1
  fi
}


topics="array arrayatt bit bitatt cmpd cmpdatt cpxcmpd cpxcmpdatt enum enumatt float floatatt \
int intatt opaque opaqueatt string stringatt vlstring vlstringatt \
commit"

return_val=0

for topic in $topics
do
    compileout $top_srcdir/$currentpath/h5ex_t_$topic.c -o h5ex_t_$topic
done

for topic in $topics
do
    fname=h5ex_t_$topic
    $ECHO_N "Testing C/H5T/$fname...$ECHO_C"
    exout ./$fname >tmp.test
    cmp -s $TESTDIR/tmp.test $top_srcdir/$currentpath/tfiles/16/$fname.tst
    status=$?
    if test $status -ne 0
    then
        echo "  FAILED!"
    else
        if [ "$fname" = "h5ex_t_cpxcmpd" -o "$fname" = "h5ex_t_cpxcmpdatt" ]; then
            targ="-n"
        else
            targ=""
        fi
        dumpout $targ $fname.h5 >tmp.test
        rm -f $TESTDIR/$fname.h5
        cmp -s $TESTDIR/tmp.test $top_srcdir/$currentpath/tfiles/18/$fname.ddl
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


#######Non-standard tests#######

USE_ALT=""
if [ "$H5_LIBVER_DIR" = "110" ]; then
   # check if HDF5 version is < 1.10.7
   version_compare "$H5_LIBVER" "1.10.7"
   if [ "$version_lt" = 1 ]; then
      USE_ALT="06"
   fi
else
  if [ "$H5_LIBVER_DIR" = "18" ]; then
   # check if HDF5 version is < 1.8.22
   version_compare "$H5_LIBVER" "1.8.22"
   if [ "$version_lt" = 1 ]; then
      USE_ALT="21"
   fi
  fi
fi

topics="objref objrefatt regref regrefatt"

for topic in $topics
do
    compileout $top_srcdir/$currentpath/h5ex_t_$topic.c -o h5ex_t_$topic
done

for topic in $topics
do
    fname=h5ex_t_$topic
    $ECHO_N "Testing C/H5T/$fname...$ECHO_C"
    exout ./$fname >tmp.test
    cmp -s $TESTDIR/tmp.test $top_srcdir/$currentpath/tfiles/16/$fname.tst
    status=$?
    if test $status -ne 0
    then
        echo "  FAILED!"
    else
        dumpout $fname.h5 >tmp.test
        rm -f $TESTDIR/$fname.h5
        version_compare "$H5_LIBVER" "1.10.0"
        if [ "$version_lt" = 1 ]; then
            cmp -s $TESTDIR/tmp.test $top_srcdir/$currentpath/tfiles/18/$fname$USE_ALT.ddl
        else
            version_compare "$H5_LIBVER" "1.12.0"
            if [ "$version_lt" = 1 ]; then
               version_compare "$H5_LIBVER" "1.10.7"
               if [ "$version_lt" = 1 ]; then
                  cmp -s $TESTDIR/tmp.test $top_srcdir/$currentpath/tfiles/110/$fname$USE_ALT.ddl
               else
                  cmp -s $TESTDIR/tmp.test $top_srcdir/$currentpath/tfiles/18/$fname.ddl
               fi
            else
                cmp -s $TESTDIR/tmp.test $top_srcdir/$currentpath/tfiles/112/$fname.ddl
            fi
        fi
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

topics="vlen vlenatt"

for topic in $topics
do
    compileout $top_srcdir/$currentpath/h5ex_t_$topic.c -o h5ex_t_$topic
done

for topic in $topics
do
    fname=h5ex_t_$topic
    $ECHO_N "Testing C/H5T/$fname...$ECHO_C"
    exout ./$fname >tmp.test
    cmp -s $TESTDIR/tmp.test $top_srcdir/$currentpath/tfiles/16/$fname.tst
    status=$?
    if test $status -ne 0
    then
        echo "  FAILED!"
    else
        dumpout $fname.h5 >tmp.test
        rm -f $TESTDIR/$fname.h5
        version_compare "$H5_LIBVER" "1.14.3"
        if [ "$version_lt" = 1 ]; then
            cmp -s tmp.test $top_srcdir/$currentpath/tfiles/18/$fname.ddl
        else
            cmp -s tmp.test $top_srcdir/$currentpath/tfiles/114/$fname.ddl
        fi
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

compileout $top_srcdir/$currentpath/h5ex_t_convert.c -o h5ex_t_convert

fname=h5ex_t_convert
$ECHO_N "Testing C/H5T/$fname...$ECHO_C"
exout ./$fname >tmp.test
cmp -s $TESTDIR/tmp.test $top_srcdir/$currentpath/tfiles/16/$fname.tst
status=$?
if test $status -ne 0
then
    echo "  FAILED!"
else
    echo "  Passed"
fi
return_val=`expr $status + $return_val`


rm -f $TESTDIR/tmp.test
echo "$return_val tests failed in C/H5T/"
exit $return_val
