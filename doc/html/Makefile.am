# HDF5 Library Doc Makefile(.in)
#
# Copyright (C) 1997, 2002
#     National Center for Supercomputing Applications.
#     All rights reserved.
#
# 
# This is the top level makefile of the Doc directory.  It mostly just
# reinvokes make in the various subdirectories.
# You can alternatively invoke make from each subdirectory manually.
##
## Makefile.am
## Run automake to generate a Makefile.in from this file.
#

include $(top_srcdir)/config/commence-doc.am

localdocdir=$(docdir)/hdf5

# Subdirectories in build-order
SUBDIRS=ADGuide Graphics Intro PSandPDF TechNotes Tutor \
        cpplus ed_libs ed_styles fortran

# Public doc files (to be installed)...
localdoc_DATA=ADGuide.html Attributes.html Big.html Caching.html Chunk_f1.gif  \
         Chunk_f2.gif Chunk_f3.gif Chunk_f4.gif Chunk_f5.gif Chunk_f6.gif \
         Chunking.html Coding.html Copyright.html Datasets.html           \
         Dataspaces.html Datatypes.html DatatypesEnum.html Debugging.html \
         EnumMap.gif Environment.html Errors.html FF-IH_FileGroup.gif     \
         FF-IH_FileObject.gif Files.html Filters.html Glossary.html       \
         Groups.html H5.api_map.html H5.format.html H5.intro.html         \
         H5.sample_code.html H5.user.PrintGen.html H5.user.PrintTpg.html  \
         H5.user.html IH_map1.gif IH_map2.gif IH_map3.gif IH_map4.gif     \
         IH_mapFoot.gif IH_mapHead.gif IOPipe.html MountingFiles.html     \
         NCSAfooterlogo.gif Performance.html PredefDTypes.html            \
         Properties.html RM_H5.html RM_H5A.html RM_H5D.html RM_H5E.html   \
         RM_H5F.html RM_H5Front.html RM_H5G.html RM_H5I.html RM_H5P.html  \
         RM_H5R.html RM_H5S.html RM_H5T.html RM_H5Z.html References.html  \
         TechNotes.html Tools.html Version.html chunk1.gif compat.html    \
         dataset_p1.gif ddl.html extern1.gif extern2.gif group_p1.gif     \
         group_p2.gif group_p3.gif h5s.examples hdf2.jpg ph5design.html   \
         ph5example.c ph5implement.txt pipe1.gif pipe2.gif pipe3.gif      \
         pipe4.gif pipe5.gif index.html version.gif
