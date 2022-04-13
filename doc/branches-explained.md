# HDF5 Git Branching Model Explained

This document describes current HDF5 branches. 

Branches are tested nightly and testing results are available at https://cdash-internal.hdfgroup.org/ and https://cdash.hdfgroup.org/. 
Commits that break daily testing should be fixed by 3:00 pm Central time or reverted.  
We encourage code contributors to check the status of their commits. If you have any questions, please contact help@hdfgroup.org.
 
## `develop`
Develop is the main branch whose source code always reflects a state with the latest delivered development changes for the next major release of HDF5. 
This is also considered the integration branch, as **all** new features are integrated into this branch from respective feature branches. 

## `Maintenance branches`

Each currently supported release-line of HDF5 (e.g. 1.8.x, 1.10.x, 1.12.x) has a support branch with the name 1_8, 1_10, 1_12. 
Maintenance branches are similar to the develop branch, except the source code in a maintenance branch always reflects a state 
with the latest delivered development changes for the next **maintenance** release of that particular supported release-line of HDF5. 
**Some** new features will be integrated into a release maintenance branch, depending on whether or not those features can be 
introduced in minor releases.  Maintenance branches are removed when a release-line is retired from support.

## `feature/*`
Feature branches are temporary branches used to develop new features in HDF5.
Feature branches branch off of develop and exist as long as the feature is under development. 
When the feature is complete, the branch is merged back into develop, as well as into any support branches in which the change will be included, and then the feature branch is removed.

## `release/*`
Release branches are used to prepare a new production release. They are primarily used to allow for last minute dotting of i's and crossing of t's 
(things like setting the release version, finalizing release notes, et cetera) and do not include new development. 
They are created from the maintenance branch at the time of the maintenance release and have 
names 1_8_N, 1_10_N, 1_12_N, where N is the minor release number. Once the release is  done it is tagged. 
Patches can be applied to the release branch for patch releases that are treated as "scaled down" maintenance releases as defined by Release coordinator.

## `1.X/master/*` where X is 8, 10 or 12
These branches are used to tag 1.X.* maintenance releases.

## `inactive/<name>/*`
These branches are for experimental features that were developed in the past  and have not been merged to develop, and are not under active development. The features 
can be out of sync with the develop branch.

This document was last updated on March 16, 2021

