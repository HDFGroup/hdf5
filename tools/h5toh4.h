#ifndef _H5TOH4_H
#define _H5TOH4_H

#include <mfhdf.h>
#include <hdf5.h>

#ifdef HAVE_SYS_STAT_H
#   include <sys/stat.h>
#endif

/*
 * Copyright © 1998 NCSA
 *                  All rights reserved.
 *
 * Programmer:  Paul Harten <pharten@ncsa.uiuc.edu>
 *              Friday, October 16th, 1998
 *
 * Purpose:	Convert H5 files to H4 files.
 */


typedef struct op_data_t {
    /*
     * information being carried between iterations.
     *
     */

    int32 hfile_id;
    int32 vgroup_id;
    int32 sd_id;
    int32 sds_id;
    int32 vdata_id;
    int32 obj_idx;
    
} op_data_t;

#ifdef H5TOH4_DEBUG
#define DEBUG_PRINT(s1,s2,s3,n1) ( fprintf(stderr,s1,s2,s3,n1) )
#else
#define DEBUG_PRINT(s1,s2,s3,n1) ( fprintf(stderr," ") )
#endif

#endif
