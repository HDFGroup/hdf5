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
  CMD="${CTEST_CMD} -S ctest_serial.cmake"
  qsub -t 60 -n 1 -q debug-flat-quad -A ${ACCOUNT_ID} ${CMD} >& ${SUMMARY_FILE}
  echo "Done running ctest serial command."
  touch ctestS.done
else
  CMD="${CTEST_CMD} -S ctest_parallel.cmake"
  qsub -t 60 -n 1 -q debug-flat-quad -A ${ACCOUNT_ID} ${CMD} >& ${SUMMARY_FILE}
  echo "Done running ctest parallel command."
  touch ctestP.done
fi
