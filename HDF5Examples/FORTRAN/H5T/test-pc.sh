#! /bin/sh
#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
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
H5FC=$HDF5_HOME/bin/h5fc
LD_LIBRARY_PATH=$HDF5_HOME/lib
export LD_LIBRARY_PATH

if ! test -f $H5FC; then
    echo "Set paths for H5FC and LD_LIBRARY_PATH in test.sh"
    echo "Set environment variable HDF5_HOME to the hdf5 install dir"
    echo "h5fc was not found at $H5FC"
    exit $EXIT_FAILURE
fi

H5DUMP=`echo $H5FC | sed -e 's/\/[^/]*$/\/h5dump/'`;
H5_LIBVER=$($H5FC -showconfig | grep -i "HDF5 Version:" | sed 's/^.* //g' | sed 's/[-].*//g')
H5_APIVER=$($H5FC -showconfig | grep -i "Default API mapping:" | sed 's/^.* //g' | sed 's/v//g' | sed 's/1/1_/')

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

FORTRAN_2003_CONDITIONAL_F="@FORTRAN_2003_CONDITIONAL_F@"

topics="vlstring"

if [ "$FORTRAN_2003_CONDITIONAL_F" = "Xyes" ]; then
    topics="arrayatt_F03 array_F03 bitatt_F03 bit_F03 cmpdatt_F03 cmpd_F03 \
            Cstring_F03 enumatt_F03 enum_F03 floatatt_F03 float_F03 \
            intatt_F03 int_F03 opaqueatt_F03 opaque_F03 \
            string_F03 $topics"
fi

return_val=0

for topic in $topics
do
    $H5FC $srcdir/h5ex_t_$topic.F90 -o h5ex_t_$topic
done

for topic in $topics
do
    fname=h5ex_t_$topic
    $ECHO_N "Testing FORTRAN/H5T/$fname...$ECHO_C"
    exout ./$fname >tmp.test
    cmp -s tmp.test $srcdir/tfiles/18/$fname.tst
    status=$?
    if test $status -ne 0
    then
        echo "  FAILED!"
    else
        if [[ $fname == "h5ex_t_cpxcmpd_F03" || $fname == "h5ex_t_cpxcmpdatt_F03" ]]
        then
            targ="-n"
        else
            targ=""
        fi
        dumpout $targ $fname.h5 >tmp.test
        rm -f $fname.h5
        cmp -s tmp.test $srcdir/tfiles/18/$fname.ddl
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

if [ "$FORTRAN_2003_CONDITIONAL_F" = "Xyes" ]; then
    topics="objrefatt_F03 objref_F03 regrefatt_F03 regref_F03"
else
    topics=""
fi

for topic in $topics
do
    $H5FC $srcdir/h5ex_t_$topic.F90 -o h5ex_t_$topic
done

for topic in $topics
do
    fname=h5ex_t_$topic
    $ECHO_N "Testing FORTRAN/H5T/$fname...$ECHO_C"
    exout ./$fname >tmp.test
    cmp -s tmp.test $srcdir/tfiles/18/$fname.tst
    status=$?
    if test $status -ne 0
    then
        echo "  FAILED!"
    else
        dumpout $fname.h5 >tmp.test
        rm -f $fname.h5
        version_compare "$H5_LIBVER" "1.10.0"
        if [ "$version_lt" = 1 ]; then
            cmp -s tmp.test $srcdir/tfiles/18/$fname$USE_ALT.ddl
        else
            version_compare "$H5_LIBVER" "1.12.0"
            if [ "$version_lt" = 1 ]; then
               version_compare "$H5_LIBVER" "1.10.7"
               if [ "$version_lt" = 1 ]; then
                  cmp -s tmp.test $srcdir/tfiles/110/$fname$USE_ALT.ddl
               else
                  cmp -s tmp.test $srcdir/tfiles/18/$fname.ddl
               fi
            else
                cmp -s tmp.test $srcdir/tfiles/112/$fname.ddl
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

topics=""
version_compare "$H5_LIBVER" "1.10.0"
if [ "$version_lt" = 0 ]; then
  topics=" vlenatt_F03 vlen_F03"
fi

for topic in $topics
do
    $H5FC $srcdir/h5ex_t_$topic.F90 -o h5ex_t_$topic
done

for topic in $topics
do
    fname=h5ex_t_$topic
    $ECHO_N "Testing C/H5T/$fname...$ECHO_C"
    exout ./$fname >tmp.test
    cmp -s tmp.test $srcdir/tfiles/18/$fname.tst
    status=$?
    if test $status -ne 0
    then
        echo "  FAILED!"
    else
        dumpout $fname.h5 >tmp.test
        rm -f $fname.h5
        version_compare "$H5_LIBVER" "1.14.3"
        if [ "$version_lt" = 1 ]; then
            cmp -s tmp.test $srcdir/tfiles/18/$fname.ddl
        else
            cmp -s tmp.test $srcdir/tfiles/114/$fname.ddl
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

$H5FC $srcdir/h5ex_t_convert.F90 -o h5ex_t_convert

#fname=h5ex_t_convert
#$ECHO_N "Testing FORTRAN/H5T/$fname...$ECHO_C"
#exout ./$fname >tmp.test
#cmp -s tmp.test $srcdir/tfiles/18/$fname.test
#status=$?
#if test $status -ne 0
#then
#    echo "  FAILED!"
#else
#    echo "  Passed"
#fi
#return_val=`expr $status + $return_val`


rm -f tmp.test
echo "$return_val tests failed in /FORTRAN/H5T/"
exit $return_val
