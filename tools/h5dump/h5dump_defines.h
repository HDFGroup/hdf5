/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
#ifndef H5DUMP_DEFINES_H__
#define H5DUMP_DEFINES_H__

#define H5DUMP_MAX_RANK     H5S_MAX_RANK

#define ATTRIBUTE_DATA  0
#define DATASET_DATA    1
#define ENUM_DATA       2
#define COL             3

/* Macros for displaying objects */
#define begin_obj(obj,name,begin)                                           \
    do {                                                                    \
        if ((name)) {                                                       \
            PRINTSTREAM(rawoutstream, "%s \"%s\" %s", (obj), (name), (begin));   \
        }                                                                   \
        else {                                                              \
            PRINTSTREAM(rawoutstream, "%s %s", (obj), (begin));             \
        }                                                                   \
    } while(0);

#define end_obj(obj,end)                                                    \
    do {                                                                    \
        if(HDstrlen(end)) {                                                 \
            PRINTSTREAM(rawoutstream, "%s", end);                           \
            if(HDstrlen(obj))                                               \
                PRINTVALSTREAM(rawoutstream, " ");                          \
        }                                                                   \
        if(HDstrlen(obj))                                                   \
            PRINTSTREAM(rawoutstream, "%s", obj);                           \
    } while(0);


/* 3 private values: can't be set, but can be read.
   Note: these are defined in H5Zprivate, they are
   duplicated here.
 */
#define H5_SZIP_LSB_OPTION_MASK         8
#define H5_SZIP_MSB_OPTION_MASK         16
#define H5_SZIP_RAW_OPTION_MASK         128

#endif  /* !H5DUMP_DEFINES_H__ */
