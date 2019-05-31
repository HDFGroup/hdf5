#!/bin/bash -l
if [ $# -gt 0 ]; then
    SUMMARY_FILE=$1
fi
ACCOUNT_ID=@ACCOUNT_ID@

echo "Run parallel test command. Test output will be in build/${SUMMARY_FILE}"
CTEST_CMD=`which ctest`

#SKIPTESTS <<KEYWORD:script inserts list of skips tests here -- don't remove>>

cd @HDF5_BINARY_DIR@
if [[ $SUMMARY_FILE == *"ctestS"* ]]; then
  CMD="${CTEST_CMD} . -E MPI_TEST_ -C Release -j 32 -T test"
else
  CMD="${CTEST_CMD} . -R MPI_TEST_ ${SKIP_TESTS} -C Release -T test"
fi

qsub -t 60 -n 1 -q debug-flat-quad -A ${ACCOUNT_ID} ${CMD} >& ${SUMMARY_FILE}

echo "Done running ctest parallel command."
