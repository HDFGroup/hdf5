# HDF5 Git Branching Model Explained

This document describes current HDF5 branches. 

Branches are tested nightly and testing results are available at https://cdash-internal.hdfgroup.org/ and https://cdash.hdfgroup.org/. 
Commits that break daily testing should be fixed by 3:00 pm Central time or reverted.  
We encourage code contributors to check the status of their commits. If you have any questions, please contact help@hdfgroup.org.
 
## `develop`
Develop is the main branch whose source code always reflects a state with the latest delivered development changes for the next major release of HDF5. 
This is also considered the integration branch, as **all** new features are integrated into this branch from respective feature branches. Although
develop is considered an integration branch, it is not an unstable branch. All code merged to develop is expected to pass all GitHub actions and daily tests.

## `Maintenance branches`
Each currently supported release line of HDF5 (e.g. 1.8.x, 1.10.x, 1.12.x) has an associated branch with the name hdf5\_1\_10, etc.. 
Maintenance branches are similar to the develop branch, except the source code in a maintenance branch always reflects a state 
with the latest delivered development changes for the next **maintenance** release of that particular supported release-line of HDF5. 
**Some** new features will be integrated into a release maintenance branch, depending on whether or not those features can be 
introduced in minor releases.  Maintenance branches are removed when a release-line is retired from support.

## `Release branches`
Release branches are used to prepare a new production release. They are primarily used to allow for last minute dotting of i's and crossing of t's 
(things like setting the release version, finalizing release notes, and generating Autotools files) and do not include new development. 
They are created from the maintenance branch at the time of the maintenance release and have 
names like hdf5\_1\_10\_N, where N is the minor release number. Once the release is  done it is tagged, with a slightly different format: hdf5-1\_\10\_N. 
Release branches are deleted after the tag has been created. If we have to create a patch version of a release (which is rare), we create a branch off of the tag.

## `feature/*`
Feature branches are temporary branches used to develop new features in HDF5.
Feature branches branch off of develop and exist as long as the feature is under development. 
When the feature is complete, the branch is merged back into develop, as well as into any support branches in which the change will be included, and then the feature branch is removed.

Ideally, all feature branches should contain a BRANCH.md file in the root directory that explains the purpose of the branch, contact information for the person responsible, and, if possible, some clues about the branch's life cycle (so we have an idea about when it can be deleted, merged, or declared inactive).

Minor bug fixes and refactoring work usually takes place on personal forks, not feature branches.

## `inactive/*`
These branches are for experimental features that were developed in the past, have not been merged to develop, and are not under active development. The exception to this is that some feature branches are labeled inactive and preserved for a short time after merging to develop. Integration branches are usually not kept in sync with the develop branch.

As for feature branches, inactive branches should have a BRANCH.md file as described above.
