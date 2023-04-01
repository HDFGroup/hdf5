HDF5 version 1.10.10 released on 2023-03-31

![HDF5 Logo](doxygen/img/HDF5.png)

[![develop build status](https://img.shields.io/github/actions/workflow/status/HDFGroup/hdf5/main.yml?branch=develop&label=develop)](https://github.com/HDFGroup/hdf5/actions?query=branch%3Adevelop)
[![1.14 build status](https://img.shields.io/github/actions/workflow/status/HDFGroup/hdf5/main.yml?branch=hdf5_1_14&label=1.14)](https://github.com/HDFGroup/hdf5/actions?query=branch%3Ahdf5_1_14)
[![1.12 build status](https://img.shields.io/github/actions/workflow/status/HDFGroup/hdf5/main.yml?branch=hdf5_1_12&label=1.12)](https://github.com/HDFGroup/hdf5/actions?query=branch%3Ahdf5_1_12)
[![1.10 build status](https://img.shields.io/github/actions/workflow/status/HDFGroup/hdf5/main.yml?branch=hdf5_1_10&label=1.10)](https://github.com/HDFGroup/hdf5/actions?query=branch%3Ahdf5_1_10)
[![1.8 build status](https://img.shields.io/github/actions/workflow/status/HDFGroup/hdf5/main.yml?branch=hdf5_1_8&label=1.8)](https://github.com/HDFGroup/hdf5/actions?query=branch%3Ahdf5_1_8)
[![BSD](https://img.shields.io/badge/License-BSD-blue.svg)](https://github.com/HDFGroup/hdf5/blob/develop/COPYING)

*Please refer to the release_docs/INSTALL file for installation instructions.*

This repository contains a high-performance library's source code and a file format
specification that implement the HDF5® data model. The model has been adopted across
many industries and this implementation has become a de facto data management standard
in science, engineering, and research communities worldwide.

The HDF Group is the developer, maintainer, and steward of HDF5 software. Find more
information about The HDF Group, the HDF5 Community, and other HDF5 software projects,
tools, and services at The HDF Group's website.
    
   https://www.hdfgroup.org/


DOCUMENTATION
-------------
This release is fully functional for the API described in the documentation.
    
   https://portal.hdfgroup.org/display/HDF5/The+HDF5+API

Full Documentation and Programming Resources for this release can be found at

   https://portal.hdfgroup.org/display/HDF5

See the RELEASE.txt file in the release_docs/ directory for information specific
to the features and updates included in this release of the library.

Several more files are located within the release_docs/ directory with specific
details for several common platforms and configurations.

    INSTALL - Start Here. General instructions for compiling and installing the library
    INSTALL_CMAKE  - instructions for building with CMake (Kitware.com)
    INSTALL_parallel - instructions for building and configuring Parallel HDF5
    INSTALL_Windows and INSTALL_Cygwin - MS Windows installations.



HELP AND SUPPORT
----------------
Information regarding Help Desk and Support services is available at

   https://portal.hdfgroup.org/display/support/The+HDF+Help+Desk



FORUM and NEWS
--------------
The following public forums are provided for public announcements and discussions
of interest to the general HDF5 Community.

   - Homepage of the Forum
   https://forum.hdfgroup.org

   - News and Announcement
   https://forum.hdfgroup.org/c/news-and-announcements-from-the-hdf-group

   - HDF5 and HDF4 Topics
   https://forum.hdfgroup.org/c/hdf5

These forums are provided as an open and public service for searching and reading.
Posting requires completing a simple registration and allows one to join in the
conversation.  Please read the following instructions pertaining to the Forum's
use and configuration
    https://forum.hdfgroup.org/t/quickstart-guide-welcome-to-the-new-hdf-forum


RELEASE SCHEDULE
----------------

![HDF5 release schedule](doc/img/release-schedule.png) 

HDF5 does not release on a regular schedule. Instead, releases are driven by
new features and bug fixes, though we try to have at least one release of each
maintenance branch per year. Future HDF5 releases indicated on this schedule
are tentative.

**NOTE**: HDF5 1.12 is being retired early due to its incomplete and incompatible VOL
layer.

| Release | New Features |
| ------- | ------------ |
| 1.8.23 | last HDF5 1.8 release |
| 1.10.10 | CVE fixes, performance improvements, H5Dchunk\_iter() |
| 1.12.3 | CVE fixes, performance improvements, H5Dchunk\_iter(), last HDF5 1.12 release |
| 1.14.1 | selection I/O with datatype conversion |
| 2.0.0 | TBD |
| TBD | VFD SWMR |

This list of feature release versions is also tentative, and the specific release
in which a feature is introduced may change.


SNAPSHOTS, PREVIOUS RELEASES AND SOURCE CODE
--------------------------------------------
Periodically development code snapshots are provided at the following URL:
    
   https://gamma.hdfgroup.org/ftp/pub/outgoing/hdf5/snapshots/

Source packages for current and previous releases are located at:
    
   https://portal.hdfgroup.org/display/support/Downloads

Development code is available at our Github location:
    
   https://github.com/HDFGroup/hdf5.git

