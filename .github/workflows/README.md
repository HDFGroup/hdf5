# Workflows for Testing
The workflows are contained in .yml files and use the callable workflow method.
Workflows can be triggered from PR, creation or merge, and on a scheduled timer.
There are a few that only get triggered manually.

## Scheduled Workflows
- daily-schedule.yml
- h5py.yml
- markdown-link-check.yml
- scorecard.yml
- vfd.yml
- vol.yml

## Manual Only Workflows
- publish-branch.yml
- publish-release.yml
- release.yml

## Triggered Workflows
- autotools.yml
- clang-format-check.yml
- cmake.yml
- codespell.yml
- cve.yml
- hdfeos5.yml
- linkchecker.yml
- netcdf.yml

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

