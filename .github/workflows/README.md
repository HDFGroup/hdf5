# Workflows for Testing
The workflows are contained in .yml files and use the callable workflow method.
Workflows can be triggered from PR, creation or merge, and on a scheduled timer.
There are a few that only get triggered manually.

## Scheduled Workflows
- daily-schedule.yml executes the daily-build.yml which first checks that there are changes
    * tarball.yml to create a source.zip and source.tar.gz
    * cygwin.yml to test on cygwin
    * script.yml to test and report to my.cdash
    * par-script.yml to test with released MPI and report to my.cdash
    * par-source.yml to test with MPI default branch and report to my.cdash
    * analysis.yml to test with LEAK and ADDRESS sanitizers and report to my.cdash
    * ctest.yml to create signed binaries with commit hash in the name
    * abi-report.yml to compare ABI to the last released binaries
    * release-files.yml uploads new binaries to snapshots
    * remove-files.yml remove previous binaries
- h5py.yml executes Python tests for h5py
- markdown-link-check.yml checks the links in markdown files
- scorecard.yml executes code-scanning and uploads to Github dashboard
- vfd.yml executes vfd-main.yml with combos of Release and Debug
    * vfd-subfiling.yml configures, builds, and tests MPI with subfiling feature
- vol.yml calls the following workflows
    * vol_rest.yml tests the REST VOL connector
    * vol_ext_passthru.yml tests the external passthrough VOL connector
    * vol_async.yml tests the asynchronous I/O VOL connector
    * vol_cache.yml tests the cache VOL connector
    * vol_adios2.yml tests the ADIOS2 VOL connector
    * vol_log.yml tests the Log-based VOL connector

## Manual Only Workflows
- publish-branch.yml publishes a local folder to the support.hdfgroup bucket
- publish-release.yml publishes release binaries to the support.hdfgroup bucket
- release.yml creates binaries for an official release or snapshot
    * tarball.yml to create a source.zip and source.tar.gz
    * ctest.yml to create signed binaries
    * abi-report.yml to compare ABI to last released binaries
    * maven-staging.yml to generate and test Maven artifacts with Java examples across all platforms
    * maven-deploy.yml to deploy Maven artifacts to repositories
    * release-files.yml uploads new binaries to releases page
- java-examples-maven-test.yml comprehensive Java examples testing with Maven artifacts

## Triggered Workflows
- clang-format-check.yml runs clang-format and reports issues
- call-workflows.yml
- codespell.yml checks spelling
- cve.yml executes test_hdf5_cve.sh script
- hdfeos5.yml configures and builds HDF5 then tests HDF-EOS5 (runs on a daily schedule, not on pull requests)
- linkchecker.yml verifies the links in generated doxygen files
- netcdf.yml configures and builds HDF5 then tests NetCDF

## Workflows called by call-workflows.yml
- main-spc.yml configure, build, and test HDF5 with:
    * API default v1_6
    * API default v1_8
    * API default v1_10
    * API default v1_12
    * API default v1_14
    * API default v2_0
    * using system zlib
    * using zlibng
    * using no filters
    * in debug mode and -Werror compiler option
    * in release mode and -Werror compiler option
    * with minimum CMake Version 3.18
- main.yml configure, build, test, and package HDF5 on Ubuntu, macOS, and Windows
- main-static.yml configure, build, test static only HDF5 on Ubuntu, macOS, and Windows
- bintest.yml test binary packages created by main.yml
- main-par.yml configure, build, and test HDF5 with openmpi
- main-par-spc.yml configure, build, and test HDF5 with HDF5_ENABLE_WARNINGS_AS_ERRORS=ON
- intel.yml configure, build, and test HDF5 with Intel OneAPI on Linux and Windows
- nvhpc.yml configure, build, and test HDF5 with nvhpc
- aocc.yml configure, build, and test HDF5 with AOCC and OpenMPI
- testxpr.yml configure, build, and test HDF5 with HDF_TEST_EXPRESS=0
- julia.yml configure and build HDF5, then test Julia hdf5 source
- msys2.yml configure, build, and test HDF5 on mingw32, mingw64, ucrt64, clang64
- i386.yml configure, build, and test HDF5 on 32-bit Linux

