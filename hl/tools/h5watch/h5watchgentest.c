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

#include "hdf5.h"
#include "H5HLprivate2.h"

/*
 *  WATCH.h5: file with various types of datasets for testing--                                   
 *
 *  The following datasets are chunked, H5D_ALLOC_TIME_INCR, max. dimensional setting:       
 *      DSET_ONE: one-dimensional dataset                                                   
 *      DSET_TWO: two-dimensional dataset                                                  
 *      DSET_CMPD: one-dimensional dataset with compound type                             
 *      DSET_CMPD_ESC: one-dimensional dataset with compound type and member names with
 *		       escape/separator characters 
 *      DSET_CMPD_TWO: two-dimensional dataset with compound type                             
 *                                                                                           
 *  The following datasets are one-dimensional, chunked, max. dimension setting:              
 *      DSET_ALLOC_EARLY: dataset with H5D_ALLOC_TIME_EARLY                                  
 *      DSET_ALLOC_LATE: dataset H5D_ALLOC_TIME_LATE                                        
 *                                                                                       
 *  The following datasets are one-dimensional:                                           
 *      DSET_NONE: fixed dimension setting, contiguous, H5D_ALLOC_TIME_LATE              
 *      DSET_NOMAX: fixed dimension setting, chunked, H5D_ALLOC_TIME_INCR      
 */
#define ONE_DIMS0	10
#define MAX_ONE_DIMS0	100

#define DSET_ONE 	"DSET_ONE"
#define DSET_NONE	"DSET_NONE"
#define DSET_NOMAX	"DSET_NOMAX"
#define DSET_ALLOC_LATE "DSET_ALLOC_LATE"
#define DSET_ALLOC_EARLY "DSET_ALLOC_EARLY"
#define DSET_CMPD 	"DSET_CMPD"
#define DSET_CMPD_ESC 	"DSET_CMPD_ESC"

#define TWO_DIMS0	4
#define TWO_DIMS1	10
#define MAX_TWO_DIMS0	60
#define MAX_TWO_DIMS1	100

#define DSET_TWO 	"DSET_TWO"
#define DSET_CMPD_TWO 	"DSET_CMPD_TWO"

#define CHUNK_SIZE	2

#define FILE "WATCH.h5"

/* Data structures for datasets with compound types */
typedef struct sub22_t {
    int a;
    int b;
    int c;
} sub22_t;

typedef struct sub2_t {
    int a;
    sub22_t b;
    int c;
} sub2_t;

typedef struct sub4_t {
    int a;
    int b;
} sub4_t;

typedef struct set_t {
    int field1;
    sub2_t field2;
    double field3;
    sub4_t field4;
} set_t;

/*
 **************************************************************************************
 *
 * Create a dataset with the given input parameters
 * Write to the dataset with the given "data"
 *
 **************************************************************************************
 */
static herr_t
generate_dset(hid_t fid, const char *dname, int ndims, hsize_t *dims, hsize_t *maxdims, hid_t dtid, void *data)
{
    hid_t dcpl=-1;		/* Dataset creation property */
    hid_t did=-1;		/* Dataset id */
    hid_t sid=-1;		/* Dataspace id */
    int i;		/* Local index variable */
    hsize_t chunk_dims[H5S_MAX_RANK];	/* Dimension sizes for chunks */

    /* Create the dataspace */
    if((sid = H5Screate_simple(ndims, dims, maxdims)) < 0)
        goto done;

    /* Set up dataset's creation properties */
    if(!HDstrcmp(dname, DSET_NONE))
        dcpl = H5P_DEFAULT;
    else {
        if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
            goto done;
        for(i = 0; i < ndims; i++)
            chunk_dims[i] = CHUNK_SIZE;
        if(H5Pset_chunk(dcpl, ndims, chunk_dims) < 0)
            goto done;
    } /* end else */

    if(!HDstrcmp(dname, DSET_ALLOC_LATE)) {
        if(H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_LATE) < 0)
            goto done;
    } else if(!HDstrcmp(dname, DSET_ALLOC_EARLY)) {
        if(H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0)
            goto done;
    } /* end if-else */

    /* Create the dataset */
    if((did = H5Dcreate2(fid, dname, dtid, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto done;

    /* Write to the dataset */
    if(H5Dwrite(did, dtid, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0)
        goto done;

    /* Closing */
    if(H5Pclose(dcpl) < 0) goto done;
    if(H5Sclose(sid) < 0) goto done;
    if(H5Dclose(did) < 0) goto done;

    return SUCCEED;

done:
    H5E_BEGIN_TRY
	H5Sclose(sid);
	H5Pclose(dcpl);
	H5Dclose(did);
    H5E_END_TRY

    return FAIL;
} /* end generate_dset() */

int
main(void)
{
    hid_t fid=-1;			/* File id */
    hid_t fapl=-1;			/* File access property list id */
    hsize_t cur_dims[1];		/* Dimension sizes */
    hsize_t max_dims[1];		/* Maximum dimension sizes */
    hsize_t cur2_dims[2];		/* Current dimension sizes */
    hsize_t max2_dims[2];		/* Maximum dimension sizes */
    hid_t set_tid=-1, esc_set_tid=-1;	/* Compound type id */
    hid_t sub22_tid=-1;			/* Compound type id */
    hid_t sub2_tid=-1, esc_sub2_tid=-1;	/* Compound type id */
    hid_t sub4_tid=-1, esc_sub4_tid=-1;	/* Compound type id */
    int one_data[ONE_DIMS0];		/* Buffer for data */
    int two_data[TWO_DIMS0*TWO_DIMS1];	/* Buffer for data */
    set_t one_cbuf[ONE_DIMS0];		/* Buffer for data with compound type */
    set_t two_cbuf[TWO_DIMS0*TWO_DIMS1];	/* Buffer for data with compound type */
    int i;				/* Local index variable */

    /* Create a copy of file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        HDexit(EXIT_FAILURE);
    /* Set to use the latest library format */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        HDexit(EXIT_FAILURE);

    /* Create a file with the latest format */
    if((fid = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        HDexit(EXIT_FAILURE);

    /* Initialization for one-dimensional dataset */
    cur_dims[0] = ONE_DIMS0;
    max_dims[0] = MAX_ONE_DIMS0;
    for(i = 0; i < ONE_DIMS0; i++)
        one_data[i] = i;

    /* Generate DSET_ONE, DSET_NONE, DSET_NOMAX, DSET_ALLOC_LATE, DSET_EARLY */
    if(generate_dset(fid, DSET_ONE, 1, cur_dims, max_dims, H5T_NATIVE_INT, one_data) < 0)
        goto done;
    if(generate_dset(fid, DSET_NONE, 1, cur_dims, NULL, H5T_NATIVE_INT, one_data) < 0)
        goto done;
    if(generate_dset(fid, DSET_NOMAX, 1, cur_dims, NULL, H5T_NATIVE_INT, one_data) < 0)
        goto done;
    if(generate_dset(fid, DSET_ALLOC_LATE, 1, cur_dims, max_dims, H5T_NATIVE_INT, one_data) < 0)
        goto done;
    if(generate_dset(fid, DSET_ALLOC_EARLY, 1, cur_dims, max_dims, H5T_NATIVE_INT, one_data) < 0)
        goto done;

    /* Initialization for two-dimensional dataset */
    cur2_dims[0] = TWO_DIMS0;
    cur2_dims[1] = TWO_DIMS1;
    max2_dims[0] = MAX_TWO_DIMS0;
    max2_dims[1] = MAX_TWO_DIMS1;

    for(i = 0; i < (TWO_DIMS0 * TWO_DIMS1); i++)
        two_data[i] = i;

    /* Generate DSET_TWO */
    if(generate_dset(fid, DSET_TWO, 2, cur2_dims, max2_dims, H5T_NATIVE_INT, two_data) < 0)
        goto done;

    /* Initialization for one-dimensional compound typed dataset */
    cur_dims[0] = ONE_DIMS0;
    max_dims[0] = MAX_ONE_DIMS0;

    for(i = 0; i < ONE_DIMS0; i++) {
        one_cbuf[i].field1 = 1;
        one_cbuf[i].field2.a = 2;
        one_cbuf[i].field2.c = 4;
        one_cbuf[i].field2.b.a = 20;
        one_cbuf[i].field2.b.b = 40;
        one_cbuf[i].field2.b.c = 80;
        one_cbuf[i].field3 = 3.0F;
        one_cbuf[i].field4.a = 4;
        one_cbuf[i].field4.b = 8;
    } /* end for */

    /* Create the compound type */
    if((sub22_tid = H5Tcreate(H5T_COMPOUND, sizeof(sub22_t))) < 0)
        goto done;
    if(H5Tinsert(sub22_tid, "a", HOFFSET(sub22_t, a), H5T_NATIVE_INT) < 0)
        goto done;
    if(H5Tinsert(sub22_tid, "b", HOFFSET(sub22_t, b), H5T_NATIVE_INT) < 0)
        goto done;
    if(H5Tinsert(sub22_tid, "c", HOFFSET(sub22_t, c), H5T_NATIVE_INT) < 0)
        goto done;

    if((sub2_tid = H5Tcreate(H5T_COMPOUND, sizeof(sub2_t))) < 0)
        goto done;
    if(H5Tinsert(sub2_tid, "a", HOFFSET(sub2_t, a), H5T_NATIVE_INT) < 0)
        goto done;
    if(H5Tinsert(sub2_tid, "b", HOFFSET(sub2_t, b), sub22_tid) < 0)
        goto done;
    if(H5Tinsert(sub2_tid, "c", HOFFSET(sub2_t, c), H5T_NATIVE_INT) < 0)
        goto done;

    if((sub4_tid = H5Tcreate(H5T_COMPOUND, sizeof(sub4_t))) < 0)
        goto done;
    if(H5Tinsert(sub4_tid, "a", HOFFSET(sub4_t, a), H5T_NATIVE_INT) < 0)
        goto done;
    if(H5Tinsert(sub4_tid, "b", HOFFSET(sub4_t, b), H5T_NATIVE_INT) < 0)
        goto done;

    if((set_tid = H5Tcreate(H5T_COMPOUND, sizeof(set_t))) < 0)
        goto done;
    if(H5Tinsert(set_tid, "field1", HOFFSET(set_t, field1), H5T_NATIVE_INT) < 0)
        goto done;
    if(H5Tinsert(set_tid, "field2", HOFFSET(set_t, field2), sub2_tid) < 0)
        goto done;
    if(H5Tinsert(set_tid, "field3", HOFFSET(set_t, field3), H5T_NATIVE_DOUBLE) < 0)
        goto done;
    if(H5Tinsert(set_tid, "field4", HOFFSET(set_t, field4), sub4_tid) < 0)
        goto done;

    /* Create the compound type with escape/separator characters */
    if((esc_sub2_tid = H5Tcreate(H5T_COMPOUND, sizeof(sub2_t))) < 0)
        goto done;
    if(H5Tinsert(esc_sub2_tid, ".a", HOFFSET(sub2_t, a), H5T_NATIVE_INT) < 0)
        goto done;
    if(H5Tinsert(esc_sub2_tid, ",b", HOFFSET(sub2_t, b), sub22_tid) < 0)
        goto done;
    if(H5Tinsert(esc_sub2_tid, "\\K", HOFFSET(sub2_t, c), H5T_NATIVE_INT) < 0)
        goto done;

    if((esc_sub4_tid = H5Tcreate(H5T_COMPOUND, sizeof(sub4_t))) < 0)
        goto done;
    if(H5Tinsert(esc_sub4_tid, "a.", HOFFSET(sub4_t, a), H5T_NATIVE_INT) < 0)
        goto done;
    if(H5Tinsert(esc_sub4_tid, "b,", HOFFSET(sub4_t, b), H5T_NATIVE_INT) < 0)
        goto done;

    if((esc_set_tid = H5Tcreate(H5T_COMPOUND, sizeof(set_t))) < 0)
        goto done;
    if(H5Tinsert(esc_set_tid, "field,1", HOFFSET(set_t, field1), H5T_NATIVE_INT) < 0)
        goto done;
    if(H5Tinsert(esc_set_tid, "field2.", HOFFSET(set_t, field2), esc_sub2_tid) < 0)
        goto done;
    if(H5Tinsert(esc_set_tid, "field\\3", HOFFSET(set_t, field3), H5T_NATIVE_DOUBLE) < 0)
        goto done;
    if(H5Tinsert(esc_set_tid, "field4,", HOFFSET(set_t, field4), esc_sub4_tid) < 0)
        goto done;

    /* Generate DSET_CMPD, DSET_CMPD_ESC */
    if(generate_dset(fid, DSET_CMPD, 1, cur_dims, max_dims, set_tid, one_cbuf) < 0)
        goto done;
    if(generate_dset(fid, DSET_CMPD_ESC, 1, cur_dims, max_dims, esc_set_tid, one_cbuf) < 0)
        goto done;

    /* Initialization for two-dimensional compound typed dataset */
    cur2_dims[0] = TWO_DIMS0;
    cur2_dims[1] = TWO_DIMS1;
    max2_dims[0] = MAX_TWO_DIMS0;
    max2_dims[0] = MAX_TWO_DIMS1;

    for(i = 0; i < (TWO_DIMS0 * TWO_DIMS1); i++) {
        two_cbuf[i].field1 = 1;
        two_cbuf[i].field2.a = 2;
        two_cbuf[i].field2.c = 4;
        two_cbuf[i].field2.b.a = 20;
        two_cbuf[i].field2.b.b = 40;
        two_cbuf[i].field2.b.c = 80;
        two_cbuf[i].field3 = 3.0F;
        two_cbuf[i].field4.a = 4;
        two_cbuf[i].field4.b = 8;
    } /* end for */

    /* Generate DSET_CMPD_TWO */
    if(generate_dset(fid, DSET_CMPD_TWO, 2, cur2_dims, max2_dims, set_tid, two_cbuf) < 0)
        goto done;

    /* Closing */
    if(H5Tclose(sub22_tid) < 0) goto done;
    if(H5Tclose(sub2_tid) < 0) goto done;
    if(H5Tclose(sub4_tid) < 0) goto done;
    if(H5Tclose(set_tid) < 0) goto done;
    if(H5Tclose(esc_sub2_tid) < 0) goto done;
    if(H5Tclose(esc_sub4_tid) < 0) goto done;
    if(H5Tclose(esc_set_tid) < 0) goto done;
    if(H5Pclose(fapl) < 0) goto done;
    if(H5Fclose(fid) < 0) goto done;

    HDexit(EXIT_SUCCESS);

done:
    H5E_BEGIN_TRY
	H5Tclose(sub22_tid);
	H5Tclose(sub2_tid);
	H5Tclose(sub4_tid);
	H5Tclose(set_tid);
	H5Tclose(esc_sub2_tid);
	H5Tclose(esc_sub4_tid);
	H5Tclose(esc_set_tid);
	H5Pclose(fapl);
	H5Fclose(fid);
    H5E_END_TRY

    HDexit(EXIT_FAILURE);
} /* end main() */

