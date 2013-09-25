## This file should be placed in the root directory of your project.
## Then modify the CMakeLists.txt file in the root directory of your
## project to incorporate the testing dashboard.
## # The following are required to uses Dart and the Cdash dashboard
##   ENABLE_TESTING()
##   INCLUDE(CTest)
SET (CTEST_PROJECT_NAME "HDF5")
SET (CTEST_NIGHTLY_START_TIME "18:00:00 CST")

SET (CTEST_DROP_METHOD "http")
IF (CDASH_LOCAL)
  SET (CTEST_DROP_SITE "72.36.68.252")
  SET (CTEST_DROP_LOCATION "/submit.php?project=HDF5.1.8")
ELSE (CDASH_LOCAL)
  SET (CTEST_DROP_SITE "cdash.hdfgroup.uiuc.edu")
  SET (CTEST_DROP_LOCATION "/submit.php?project=HDF518")
ENDIF (CDASH_LOCAL)
SET (CTEST_DROP_SITE_CDASH TRUE)

SET (UPDATE_TYPE svn)
SET (VALGRIND_COMMAND "/usr/bin/valgrind")
SET (VALGRIND_COMMAND_OPTIONS "-v --tool=memcheck --leak-check=full --track-fds=yes --num-callers=50 --show-reachable=yes --track-origins=yes --malloc-fill=0xff --free-fill=0xfe")
SET (CTEST_MEMORYCHECK_COMMAND "/usr/bin/valgrind")
SET (CTEST_MEMORYCHECK_COMMAND_OPTIONS "-v --tool=memcheck --leak-check=full --track-fds=yes --num-callers=50 --show-reachable=yes --track-origins=yes --malloc-fill=0xff --free-fill=0xfe")

SET (CTEST_TEST_TIMEOUT 3600 CACHE STRING 
    "Maximum time allowed before CTest will kill the test.") 
SET (DART_TESTING_TIMEOUT 3600 CACHE STRING 
    "Maximum time allowed before CTest will kill the test." FORCE)

SET(CTEST_SUBMIT_RETRY_DELAY 20 CACHE STRING
    "How long to wait between timed-out CTest submissions.")
