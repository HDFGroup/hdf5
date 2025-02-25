# Workflows for Testing
The workflows are contained in .yml files and use the callable workflow method.
Workflows can be triggered from PR, creation or merge, and on a scheduled timer.
There are a few that only get triggered manually.

## Scheduled Workflows
- daily-schedule.yml executes the daily-build.yml which first checks that there are changes
    * tarball.yml to create a source.zip and source.tar.gz
    * cygwin-cmake.yml to test on cygwin
    * cmake-script.yml to test and report to my.cdash
    * cmake-par-script.yml to test with released MPI and report to my.cdash
    * cmake-par-source.yml to test with MPI default branch and report to my.cdash
    * cmake-analysis.yml to test with LEAK and ADDRESS sanitizers and report to my.cdash
    * cmake-ctest.yml to create signed binaries with commit hash in the name
    * abi-report.yml to compare ABI to last released binaries
    * release-files.yml uploads new binaries to snapshots
    * remove-files.yml remove previous binaries
- h5py.yml executes python tests for h5py
- markdown-link-check.yml checks the links in markdown files
- scorecard.yml executes code-scanning and uploads to Github dashboard
- vfd.yml executes vfd-main.yml with combos of Release and Debug
    * vfd-subfiling.yml configures, builds, and tests MPI with subfiling feature
- vol.yml calls the following workflows
    * vol_rest.yml tests the REST VOL connector
    * vol_ext_passthru.yml tests the external passthrough VOL connector
    * vol_async.yml tests the asynchronous I/O VOL connector
    * vol_cache.yml tests the cache VOL connector"
    * vol_adios2.yml tests the ADIOS2 VOL connector
    * vol_log.yml tests the Log-based VOL connector

## Manual Only Workflows
- publish-branch.yml publishes a local folder to the support.hdfgroup bucket
- publish-release.yml publishes release binaries to the support.hdfgroup bucket
- release.yml creates binaries for an official release or snapshot
    * tarball.yml to create a source.zip and source.tar.gz
    * cmake-ctest.yml to create signed binaries
    * abi-report.yml to compare ABI to last released binaries
    * release-files.yml uploads new binaries to releases page

## Triggered Workflows
- autotools.yml
- clang-format-check.yml runs clang-foramt and reports issues
- cmake.yml
- codespell.yml checks spelling
- cve.yml executes test_hdf5_cve.sh script
- hdfeos5.yml configures and builds HDF5 then tests HDF-EOS5
- linkchecker.yml verifies the links in generated doxygen files
- netcdf.yml configures and builds HDF5 then tests netcdf

## Workflows called by autotools.yml
- main-auto-spc.yml
- main-auto-par-spc.yml
- main-auto-par.yml
- main-auto.yml
- intel-auto.yml
- nvhpc-auto.yml
- aocc-auto.yml
- testxpr-auto.yml

## Workflows called by cmake.yml
- main-cmake-spc.yml
- main-cmake.yml
- cmake-bintest.yml
- main-cmake-par.yml
- main-cmake-par-spc.yml
- intel-cmake.yml
- nvhpc-cmake.yml
- aocc-cmake.yml
- testxpr-cmake.yml
- julia-cmake.yml
- msys2-cmake.yml
- i386-cmake.yml

