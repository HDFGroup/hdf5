#!/bin/sh

JH5INSTALLDIR=/afs/ncsa/projects/hdf/java/java2/mcgrath/arabica/New5
HDF5LIB=/afs/ncsa/projects/hdf/release/prehdf5-1.2.1/SunOS_5.7/lib

#make this relative to the source root...
PWD=/afs/ncsa.uiuc.edu/projects/hdf/java/java2/mcgrath/arabica/java-hdf5
LIBDIR=$JH5INSTALLDIR"/lib"

CLASSPATH=".:"$LIBDIR"/jhdf5.jar"

LD_LIBRARY_PATH=$HDF5LIB":"$LIBDIR"/solaris"

export CLASSPATH
export LD_LIBRARY_PATH

/usr/java1.2/bin/java  CreateGroupDataset $*
