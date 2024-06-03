#! /bin/bash
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
#
# Tests for the Mirror VFD feature.
#

###############################################################################
## test parameters
###############################################################################

nerrors=0

SERVER_VERBOSITY="--verbosity=1"
# Choose random ephemeral port number
RANDOM_PORT=$[ $RANDOM % 16384 + 49152 ]
echo "Using port: $RANDOM_PORT"
SERVER_PORT="--port=$RANDOM_PORT"


###############################################################################
## Main
###############################################################################

## TODO: arguments for main port, port range, verbosity?
# Parse options (none accepted at this time)
while [ $# -gt 0 ]; do
    case "$1" in
    *)  # unknown option
        echo "$0: Unknown option ($1)"
        exit 1
        ;;
    esac
done

RUN_DIR=mirror_vfd_test
MIRROR_UTILS=../utils/mirror_vfd # TODO: presupposes from test/

# Start clean
if test -d $RUN_DIR ; then
    rm -rf $RUN_DIR
fi
mkdir $RUN_DIR

# Copy program files into dedicated test directory
for FILE in $MIRROR_UTILS/mirror_* ; do
    case "$FILE" in
        *.o) continue ;; # Don't copy .o files
    esac
    cp $FILE $RUN_DIR
done
cp mirror_vfd $RUN_DIR

# With the --disable-shared option, program files are built in their main
# directories; otherwise they are built in dir/.libs with a corresponding
# wrapper script. Copy these libs builds if appropriate.
if [ -f $MIRROR_UTILS/.libs/mirror_server ] ; then
    RUN_LIBS=$RUN_DIR/.libs
    # Delete previous .libs directory, to remove any generated libtool files
    if test -d $RUN_LIBS ; then
        rm -rf $RUN_LIBS
    fi
    mkdir $RUN_LIBS
    for FILE in $MIRROR_UTILS/.libs/mirror_* ; do
        case "$FILE" in
            *.o) continue ;; # Don't copy .o files
        esac
        cp $FILE $RUN_LIBS
    done
    cp .libs/mirror_vfd $RUN_LIBS
fi

cd $RUN_DIR

echo "Launching Mirror Server"
SERVER_ARGS="$SERVER_PORT $SERVER_VERBOSITY"
./mirror_server $SERVER_ARGS &

./mirror_vfd $SERVER_PORT
nerrors=$?

echo "Stopping Mirror Server"
./mirror_server_stop $SERVER_PORT

# Wait for background server process to exit
wait

###############################################################################
## Report and exit
###############################################################################
cd ..
if test $nerrors -eq 0 ; then
    echo "Mirror VFD tests passed."
    if test -z "$HDF5_NOCLEANUP" ; then
        rm -rf $RUN_DIR
    fi
    exit 0
else
    echo "Mirror VFD tests FAILED."
    exit 1
fi

