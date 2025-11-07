HDF5 Examples

*Please refer to the Using_CMake.txt file for CMake instructions.*

Note that this HDF5Examples directory structure is also provided as a stand-alone project
distributed with library binaries as well as compiled and tested during a HDF5 Library build.

This repository contains a high-performance library's example code that demonstrate the HDF5® data
model API. The HDF5® data model has been adopted across
many industries and this implementation has become a de facto data management standard
in science, engineering, and research communities worldwide.

The HDF Group is the developer, maintainer, and steward of HDF5 software. Find more
information about The HDF Group, the HDF5 Community, and other HDF5 software projects,
tools, and services at [The HDF Group's website](https://www.hdfgroup.org/).

We suggest using the presets method with CMake for building the examples. However, if you prefer to use the
h5cc pkg-config wrappers, you can use the following commands to build the examples:

    export HDF5_HOME="hdf5 installation root"
    export PKG_CONFIG_PATH="$HDF5_HOME/lib/pkgconfig"
    export LD_LIBRARY_PATH="$HDF5_HOME/lib:$LD_LIBRARY_PATH"
    export PATH="$HDF5_HOME/bin:$PATH"

Then, you can compile the examples with:

    h5cc -o example1 example1.c
    h5c++ -o example2 example2.cpp
    h5fc -o example3 example3.f90

For Java examples with Maven integration, see the JAVA/README-MAVEN.md file for complete instructions on using the `org.hdfgroup:hdf5-java-examples` Maven artifact.

The test-pc.sh script can test the examples with the h5*cc pkg-config wrappers with:
    cd \<path to examples\>
    export HDF5_HOME="hdf5 installation root"; sh ./test-pc.sh \<path to examples\> \<path to build dir\> .
    Notice that period (.) at the end of the command is important, it tells the script to use the current
    directory as the source directory.

HELP AND SUPPORT
----------------
Information regarding Help Desk and Support services is available at

   https://help.hdfgroup.org 



FORUM and NEWS
--------------
The [HDF Forum](https://forum.hdfgroup.org) is provided for public announcements and discussions
of interest to the general HDF5 Community.

   - News and Announcements
   https://forum.hdfgroup.org/c/news-and-announcements-from-the-hdf-group

   - HDF5 Topics
   https://forum.hdfgroup.org/c/hdf5

These forums are provided as an open and public service for searching and reading.
Posting requires completing a simple registration and allows one to join in the
conversation.  Please read the [instructions](https://forum.hdfgroup.org/t/quickstart-guide-welcome-to-the-new-hdf-forum
) pertaining to the Forum's use and configuration.


HDF5 SNAPSHOTS, PREVIOUS RELEASES AND SOURCE CODE
--------------------------------------------
Full Documentation and Programming Resources for this HDF5 can be found at

   https://support.hdfgroup.org/documentation/index.html

Periodically development code snapshots are provided at the following URL:

   https://github.com/HDFGroup/hdf5/releases/tag/snapshot

Source packages for current and previous releases are located at:

   hdf5 1.14 releases:
   https://support.hdfgroup.org/releases/hdf5/v1_14/index.html

   Archived releases:
   https://support.hdfgroup.org/archive/support/ftp/HDF5/releases/index.html

Development code is available at our Github location:

   https://github.com/HDFGroup/hdf5.git

