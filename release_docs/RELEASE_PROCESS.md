# Release Process

## Objective:
Release a new version of HDF software, which corrects defects, adds functionality, improves maintainability, and/or ports it to new operational environments.

## Description (General):
At regularly scheduled intervals throughout each year, The HDF Group releases a new version of each of its maintained/core software products and corresponding documentation set. This is referred to as a maintenance release. The main purpose of a maintenance release is to address defects reported against previous releases of the software, to port the software to new operating environments, and to improve maintainability by way of refactoring code and improving documentation. A maintenance release may also include new functionality in any of the product's components.

Depending on the software being released, there may be specific guidelines that are followed or steps that are performed in addition to the 'standard' maintenance release procedure - in such a case, product-specific tabs are provided within the page to view information specific to only that product.

## Description (HDF5):
Twice each year, The HDF Group releases a new version of the HDF5 C, C++, and Fortran libraries and command line utilities and associated documentation set. This is referred to as a maintenance release of HDF5. The main purpose of a maintenance release is to address defects reported against previous releases of the software, to port HDF5 to new operating environments, and to improve maintainability by way of refactoring code and improving documentation. A maintenance release may also include new functionality in the C, C++, and Fortran libraries and/or command line utilities if requested and/or funded by the user community and/or The HDF Group's customers. The new features included in a maintenance release expand the feature set only and do not change the HDF5 file format, with the exception of addressing critical defects that corrupt data.

Maintenance releases are always backward compatible with regards to the HDF5 file format, meaning that:
- New libraries and command line utilities can access HDF5 files created by the previous versions of the libraries.

Maintenance releases are always forward compatible with regards to the HDF5 file format, meaning that:
- HDF5 libraries and command line utilities can access files created by future maintenance versions of the library.
Note that maintenance releases are NOT guaranteed to be interface-compatible, meaning that, on occasion, application source code will need updated and re-compiled against a new maintenance release when the interface changes. Interface changes are only made when absolutely necessary as deemed by the HDF5 product manager(s), and interface compatibility reports are published with each release to inform customers and users of any incompatibilities in the interface.

For more information on the HDF5 versioning and backward and forward compatibility issues, see the [API Compatibility Macros][u13] on the public website.

## Participants:
- Product Manager — The individual responsible for the overall direction and development of a software product at The HDF Group.
- Release Manager — The individual responsible for coordinating and overseeing the activities specific to releasing a software product at The HDF Group.
- Software Developer — An individual or group responsible for performing maintenance on supported HDF software.
- Software Tester — An individual responsible for identifying, implementing, and conducting tests of software code, modules, packages, executables, and/or release binaries.
- Test Automation Team — A team at The HDF Group responsible for overseeing the daily automated regression tests of software maintained by the company's Sustaining Engineering program.

## Tasks:
### 1. Plan Maintenance Release (Product Manager | Release Manager)

### 2. Perform Software Maintenance (Software Developers | Product Manager | Release Manager)

### 3. Prepare Release Notes (Release Manager)
1. Confirm that all non-trivial changes made to the source are reflected in the release notes. Verify the following: 
    - [HDF5 Milestones Projects](https://github.com/HDFGroup/hdf5/milestones)
    - Each entry in [RELEASE.txt][u1] traces to one or more resolved GH issues marked with FixVersion="X.Y.Z". 
    - Each resolved GH milestone issue traces to an entry in [RELEASE.txt][u1].
    - Each resolved GH milestone issue traces to one or more revisions to the HDF5 source.
    - Each resolved GH milestone issue traces to one or more pull requests.
2. For each previously authored KNOWN ISSUE in the [RELEASE.txt][u1], if the issue has been resolved or can no longer be confirmed, remove the issue from the [RELEASE.txt][u1].
    - Document any new known issues at the top of the list.
3. Update the TESTED CONFIGURATION FEATURES SUMMARY in [RELEASE.txt][u1] to correspond to features and options that have been tested during the maintenance period by the automated daily regression tests. 
    - **See: Testing/Testing Systems(this is a page in confluence)**
4. Update current compiler information for each platform in the PLATFORMS TESTED section of [RELEASE.txt][u1].
5. Review the [RELEASE.txt][u1] for formatting and language to verify that it corresponds to guidelines found in **[Writing Notes in a RELEASE.txt(this is missing)]()** File.
6. Review and update, if needed, the [README][u2] and [LICENSE][u3] files.
7. Review and update all INSTALL_* files in [release_docs][u4], if needed.
    - [INSTALL][u5] should be general info and not require extensive changes
    - [INSTALL_CMake.txt][u7] are the instructions for building under CMake.

### 4. Freeze Code (Release Manager | Test Automation Team)
1. Transition from performing maintenance on software to preparing for its delivery.
2. A few days before the code freeze, announce (via a product's developer mailing list and during team meetings) the pending freeze of the code for the release. On the day of the code freeze, send a "no more commits" message for the software being released and any third party software we develop that it depends on, as well as a "no more upgrades" message for other third party software the release depends on.
        - Recently we haven’t announced a code freeze since it doesn’t take long to create the release branch and the support branch doesn’t need to remain frozen once the release branch is created. There are a few things that can be done on the support branch before the release branch is created, in particular updating the .so numbers.  
3. Be sure to complete all four steps to update so numbers for each deployed lib file in the process described in config/lt_vers.am and check that the .so numbers for lib files in binaries correctly indicate compatibility status with the previous release.  
4. Move all unresolved Milestone issues to the next release version in GitHub.
5. Verify that frozen code branch satisfies all existing regression test cases, and give the 'OK' to the release coordinator once all daily test configurations are passing as expected after the date of the code freeze. If there are failing tests after the code freeze date, coordinate with maintainers responsible for the failures to ensure that either the changes causing the failures are corrected or reverted. 
6. Verify release branches for third-party software used: SZIP, ZLIB, and Plugins; and announce release versions to hdf5lib@hdfgroup.org.

### 5. Update Interface Version (Release Manager | Product Manager)
1. Verify interface additions, changes, and removals, and update the shared library interface version number.
2. Execute the CI snapshot workflow.
    - Actions - “[hdf5 release build][u8]” workflow and use the defaults.
3. Download and inspect release build source and binary files.  Downloaded source files should build correctly, one or more binaries should install and run correctly.  There should be nothing missing nor any extraneous files that aren’t meant for release.
4. Verify the interface compatibility reports between the current source and the previous release on the Github [Snapshots]u14] page.
    - The compatibility reports are produced by the CI and are viewable in the Github [Releases/snapshot][u15] section.
5. Verify the interface compatibility reports between the current source and the previous release on the Github [Snapshots][u14] page.
    - The compatibility reports are produced by the CI and are viewable in the Github [Releases/snapshot][u15] section.
6. Confirm the necessity of and approve of any interface-breaking changes. If any changes need to be reverted, task the developer who made the change to do so as soon as possible. If a change is reverted, return to the previous step and regenerate the compatibility report after the changes is made. Otherwise, continue to the next step.
7. Update the .so version numbers in the [config/lt_vers.am][u9] file in the support branch according to [libtool's library interface version](https://www.gnu.org/software/libtool/manual/libtool.html#Versioning) scheme. 
    - See [Updating version info (Libtool)](https://www.gnu.org/software/libtool/manual/html_node/Updating-version-info.html#Updating-version-info) for rules to help update library version numbers. 
8. After the release branch has been created, run `./autogen.sh` to regenerate build system files on the release branch and commit the changes.    

### 6. Prepare Release Branch (Release Manager)
1. Get the release branch ready for pre-release testing and packaging.
2. For all release preparation operations, the release coordinator will clone and push directly to canonical HDF5:
    - `$ git clone ​https://github.com/HDFGroup/hdf5.git`
3. Create a release preparation branch (hdf5_X_Y_Z) by branching off of the support branch (hdf5_X_Y):
    - `$ git checkout hdf5_X_Y`
    - `$ git checkout -b hdf5_X_Y_Z`
    - or create the new branch in GitHub GUI.
4. Check that required CMake files point to the specific versions of the third-party software (szip, zlib and plugins) that they depend on.
    - Update as needed.
5. Change the **support** branch to X.Y.{Z+1}-1 using the [bin/h5vers][u10] script: 
    - `$ git checkout hdf5_X_Y`
    - `$ bin/h5vers -s X.Y.{Z+1}-1;`
    - `$ git commit -m "Updated support branch version number to X.Y.{Z+1}-1"`
    - `$ git push`
6. Change the **release preparation branch**'s version number to X.Y.Z-{SR+1} using the [bin/h5vers][u10]/bin/h5vers script: 
    - `$ git checkout hdf5_X_Y_Z;` 
    - `$ bin/h5vers -s X.Y.Z-{SR+1};` 
    - `$ git commit -m "Updated release preparation branch version number to X.Y.Z-{SR+1}"` 
    - `$ git push` 
7. ** OBSOLETE CURRENTLY **
   Update default configuration mode
    - `$ git checkout hdf5_X_Y_Z;`. 
    - Need to set option `HDF5_GENERATE_HEADERS` to `OFF`, currently in line 996 of [src/CMakeLists.txt][u11].
    - (use `git status --ignored` to see the changes and `git add -f` to add all files. First delete any new files not to be committed, notably `src/H5public.h~`.)
    - `$ git push with commit message listing change steps for creating release branch`
    ** END OBSOLETE CURRENTLY **
8. E-mail hdf5lib@hdfgroup.org to indicate that the code freeze on the release support branch (i.e. hdf5_X_Y) has been lifted and development on the next maintenance release can resume. The code freeze will remain in place on the release preparation branch (i.e. hdf5_X_Y_Z) indefinitely.

### 7. Perform Release Testing (Test Automation Team | Release Manager | Project Leads)
1. Verify that source and binary distributions of HDF5 are acceptable on all target operating environments.
2. Create a page on Confluence as a sub-page of the current release version's project collaboration page (see HDF5 Maintenance Releases) to document release testing results. 
3. Document the test procedure that will be used for this release on the new sub-page. 
4. Schedule daily stand-up meetings or other regular checkpoints to assure progress on testing tasks, and inform testing team of your expectations.
5. Schedule and enable daily automated regression testing of the release preparation branch (i.e. hdf5_X_Y_Z). Give the 'OK' to proceed once all required tests have verified that HDF5 is functioning as intended on all target operating environments. 
6. Select release build from workflow.
7. Choose the release branch
8. Change ‘Release version tag’ name to 'hdf5_X.Y.Z.P'
    - P is some pre-release number.
9. Send a message to the HDF forum indicating that a pre-release source package is available for testing at <e.g., <github.com releases URL>/{hdf5-X.Y.Z-P}> and that feedback from the user community on their test results is being accepted.
10. Contact paying clients who are interested in testing the pre-release source package and inform them that it is available for testing and that feedback on their test results of the pre-release is appreciated.
11. This should be automated and currently github binaries are not signed.
    - Follow the [How to sign binaries with digital certificates(this is missing)]() work instructions to sign each Windows and Mac binary package with a digital certificate.
12. Once binaries are ready to be tested, send an e-mail notification or update the Confluence test dashboard page indicating source and binary test assignments and when results should be made available. 
13. Use the pre-release source packages to build and test HDF5 on assigned platforms by hand. Build both shared and static libraries, Fortran, C++, and szip, and any additional configurations required on specific remote platforms based on customer support needs.
14. Use the pre-release binary packages found in /mnt/scr1/pre-release/hdf5/vXYZ/pre-\<n\>/binaries/{UNIX, Windows} to test according to the binary testing procedures for your assigned platforms. 
15. Initial Testing: 
    - Installation Using Installer Binary
        - Execute the install package
        - Follow prompts
    - Uncompress Directory Image Binary
        - Extract the package
    - After Installation
        - The examples folder, HDF5Examples, located in the HDF5 install folder, can be built and tested with CMake and the supplied
            HDF5_Examples.cmake file. The HDF5_Examples.cmake expects HDF5 to have been installed in the default location with same compilers (see the
            libhdf5.settings file in the lib install folder). Also, the CMake utility should be installed.

    - To test the installation with the examples;
        - Create a directory to run the examples.
        - Copy HDF5Examples folder to this directory.
        - Copy CTestScript.cmake to this directory.
        - Copy HDF5_Examples.cmake to this directory.
        - Copy HDF5_Examples_options.cmake to this directory.
        - The default source folder is defined as "HDF5Examples". It can be changed with the CTEST_SOURCE_NAME script option.
        - The default installation folder should be visible in the script. It can be changed with the INSTALLDIR script option.
        - The default ctest configuration is defined as "Release". It can be changed
            with the CTEST_CONFIGURATION_TYPE script option. Note that this must
            be the same as the value used with the -C command line option.
        - The default build configuration is defined to build and use static libraries.
            Shared libraries can be used with the STATICONLYLIBRARIES script option set to "NO".
        - Other options can be changed by editing the HDF5_Examples_options.cmake file.
    - If the defaults are okay, execute from this directory:
        - ctest -S HDF5_Examples.cmake -C Release -V -O test.log
    - If the defaults need change, execute from this directory:
        - ctest -S HDF5_Examples.cmake,CTEST_SOURCE_NAME=MyExamples,INSTALLDIR=MyLocation -C Release -V -O test.log
        - When executed, the ctest script will save the results to the log file, test.log, as
            indicated by the ctest command. If you wish to see more build and test information,
            add "-VV" to the ctest command. The output should show;
            100% tests passed, 0 tests failed out of 206 (all options).
    - For more information see USING_CMake_Examples.txt in the install folder.
16. Manual Testing (i.e. verifying correct test outcomes via visual inspection): 
    - Inspect text documents for correct versions and names. 
    - Inspect the doxygen files in the share/html directory open index.html .
17. Update the test results Confluence page with status/outcome of all test assignments.
18. If any test source (hdf-forum, clients, internal testers, automated regression suite) identifies any issues: 
    - a) Enter the issue in JIRA summarizing the failure if it is not already there. 
    - b) Decide whether to resolve the issue for this release or simply document it as a known issue.
    - c) Document and assign action items resulting from a) or b), and monitor them to closure.
19. Decide if another cycle of pre-release testing should occur based on the issue reports received and the actions taken during this cycle. If another round of testing is required (i.e. there were significant issues in pre-release testing which resulted in code changes), increment the subrelease version number and go back to step 7.2. If no further testing is required (i.e. no code changes were made and issues were documented as known issues, or code changes were trivial, unit tested, and exhaustive testing is unneeded), then proceed.


### 8. Finalize Release Notes (Release Manager)
1. Perform a final review of release notes and ensure that any new changes made to the source, any new known issues discovered, and any additional tests run since the code freeze have been reflected in RELEASE.txt and other appropriate in-source documentation files (INSTALL_*, etc.). (Refer to the sub-steps of step 3 for what to check).
2. Update the [RELEASE.txt][u1] in the **support** branch (i.e. hdf5_X_Y) to remove entries in “Bugs fixed” and “New Features” sections and increment the version number for the following release (“Bug fixes since X.Y.Z” - occurs twice).
    - `$ git checkout hdf5_X_Y` 
    - `$ vi RELEASE.txt # update RELEASE.txt to clear it out` 
    - `$ git commit -m "Reset RELEASE.txt in preparation for the next release."` 
    - `$ git push` 
3. Update Release Notes in **release** branch (Release Manager)

### 9. Package and Distribute Release (Release Manager)
1. h5vers could run genparser, which can change the generated files if certain code files have been changed since the files generated by genparser were committed on the release branch.  This should be checked by running `git status --ignored;`, then running genparser, then repeating `git status --ignored;`. If there are modified files from either git status command, they should be committed (or deleted if there are backup files or an autom4te.cache directory), and at least minimal testing should be done to see that the software is still good with the changes. 
2. Set version for release, removing the subrelease string, initially `$ bin/h5vers -s X.Y.Z;`. Any subsequent patch releases will need the subrelease number.
3. Run `bin/release` (similar to 8.2) and commit all the changed files.
4. Select the actions tab and the release build workflow, then click the 'Run workflow' drop-down.
    - Choose the release branch
    - Enter the ‘Release version tag’ name as 'X.Y.Z'
    - Press "Run Workflow"
5. Review the release files in Github
6. Edit the Github Release and change status to Release
    - Change status from Pre-release to Release
7. Select publish-release build from workflow, then click the 'Run workflow' drop-down.
    - Choose the release branch
    - Enter the ‘HDF5 Release version tag’ name as 'X.Y.Z'
    - Enter the 'HDF5 Release file name base' as 'hdf5-X.Y.Z'
    - Enter the 'HDF5 target bucket directory' as 'vX_Y/vX_Y_Z'
    - Press "Run Workflow"
8. Release hdf5_plugins following the same steps.

### 10. Add the contents of the RELEASE.txt file in the release code to the HISTORY-X_Y file in the **support** branch, just below the introductory lines at the top of the HISTORY file.

### 11. Conduct Release Retrospective (Release Manager)
1. Schedule time and solicit comments from retrospective
2. Identify issues and document them

[u1]: https://github.com/HDFGroup/hdf5/blob/develop/release_docs/RELEASE.txt
[u2]: https://github.com/HDFGroup/hdf5/blob/develop/README.md
[u3]: https://github.com/HDFGroup/hdf5/blob/develop/LICENSE
[u4]: https://github.com/HDFGroup/hdf5/blob/develop/release_docs
[u5]: https://github.com/HDFGroup/hdf5/blob/develop/release_docs/INSTALL
[u7]: https://github.com/HDFGroup/hdf5/blob/develop/release_docs/INSTALL_CMake.txt
[u8]: https://github.com/HDFGroup/hdf5/blob/develop/.github/workflows/release.yml
[u9]: https://github.com/HDFGroup/hdf5/blob/develop/config/lt_vers.am
[u10]: https://github.com/HDFGroup/hdf5/blob/develop/bin/h5vers
[u11]: https://github.com/HDFGroup/hdf5/blob/develop/src/CMakeLists.txt
[u12]: https://github.com/HDFGroup/hdf5/blob/develop/configure.ac
[u13]: https://support.hdfgroup.org/documentation/hdf5/latest/api-compat-macros.html
[u14]: https://github.com/HDFGroup/hdf5/releases/tag/snapshot-1.14
[u15]: https://github.com/HDFGroup/hdf5/releases/tag/snapshot
