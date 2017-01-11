/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Generate the binary hdf5 files for the h5format_convert tests.
 * Usage: just execute the program without any arguments will
 * generate all the binary hdf5 files 
 *
 * If you regenerate the test files (e.g., changing some code,
 * trying it on a new platform, ...), you need to verify the correctness
 * of the expected output and update the corresponding *.ddl files.
 */

#include "hdf5.h"
#include "H5private.h"

#define NON_V3_FILE		"h5fc_non_v3.h5"
#define EDGE_V3_FILE		"h5fc_edge_v3.h5"
#define ERR_LEVEL_FILE		"h5fc_err_level.h5"

const char *FILENAME[] = {
    "h5fc_ext1_i.h5",	/* 0 */
    "h5fc_ext1_s.h5",	/* 1 */
    "h5fc_ext1_f.h5",	/* 2 */
    "h5fc_ext2_is.h5", 	/* 3 */
    "h5fc_ext2_if.h5",	/* 4 */
    "h5fc_ext2_sf.h5", 	/* 5 */
    "h5fc_ext3_isf.h5",	/* 6 */ 
    "h5fc_ext_none.h5",	/* 7 */ 
    NULL
};

#define GROUP			"GROUP"

#define DSET_BT1		"DSET_BT1"
#define DSET_NDATA_BT1		"DSET_NDATA_BT1"
#define DSET_COMPACT		"DSET_COMPACT"
#define DSET_CONTIGUOUS		"DSET_CONTIGUOUS"

#define DSET_EA			"DSET_EA"
#define DSET_NDATA_EA		"DSET_NDATA_EA"
#define DSET_BT2		"DSET_BT2"
#define DSET_NDATA_BT2		"DSET_NDATA_BT2"
#define DSET_FA			"DSET_FA"
#define DSET_NDATA_FA		"DSET_NDATA_FA"
#define DSET_NONE		"DSET_NONE"
#define DSET_NDATA_NONE		"DSET_NDATA_NONE"

#define DSET_EDGE		"DSET_EDGE"
#define DSET_ERR		"DSET_ERR"

#define ISTORE_IK  64
#define ISTORE_ERR 1 

#define NUM 500


/*
 * Function: gen_non() 
 *
 * Create a file with SWMR write+non-latest-format--this will result in v3 superbock+latest version support:
 *	1) 1 chunked dataset with extensible array chunk indexing type (without data)
 *	2) 1 chunked dataset with version 2 B-tree chunk indexing type (with data)
 * Re-open the file with write+non-latest-format and create:
 *	3) 1 chunked dataset with version 2 B-tree chunk indexing type (without data)
 * 	4) 1 chunked dataset with extensible array indexing type (with data)
 *	5) 1 compact and 1 contiguous datasets
 */
static void
gen_non(const char *fname)
{
    hid_t	fid = -1;		/* file id */
    hid_t	fcpl = -1;		/* file creation property list */
    hid_t	gid = -1;		/* group id */
    hid_t   	sid = -1;       	/* space id */
    hid_t	dcpl = -1;		/* dataset creation property id */
    hid_t	did1 = -1, did2 = -1;	/* dataset id */
    hsize_t 	dims1[1] = {10};     	/* dataset dimension */
    hsize_t 	dims2[2] = {4, 6};     	/* dataset dimension */
    hsize_t	max_dims[2];		/* maximum dataset dimension */
    hsize_t 	c_dims[2] = {2, 3};    	/* chunk dimension */
    int		i;		    	/* local index variable */
    int     	buf[24];            	/* data buffer */

    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        goto error;
    if(H5Pset_shared_mesg_nindexes(fcpl, 4) < 0)
        goto error;
    if(H5Pset_istore_k(fcpl, 64) < 0)
        goto error;

    /* Create an empty file with latest-format */
    if((fid = H5Fcreate(fname, H5F_ACC_TRUNC|H5F_ACC_SWMR_WRITE, fcpl, H5P_DEFAULT)) < 0)
        goto error;

    /* Create a group */
    if((gid = H5Gcreate2(fid, GROUP, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    /* Create data */
    for(i = 0; i < 24; i++)
        buf[i] = i;

    /* Set chunk */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if(H5Pset_chunk(dcpl, 2, c_dims) < 0)
        goto error;

    /* 
     * Create a chunked dataset with extensible array chunk indexing type (without data)
     */

    /* Create dataspace */
    max_dims[0] = 10;
    max_dims[1] = H5S_UNLIMITED;
    if((sid = H5Screate_simple(2, dims2, max_dims)) < 0)
        goto error;

    /* Create the dataset */
    if((did1  = H5Dcreate2(fid, DSET_NDATA_EA, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    /* Closing */
    if(H5Sclose(sid) < 0)
        goto error;
    if(H5Dclose(did1) < 0)
        goto error;

    /* 
     * Create a chunked dataset with version 2 B-tree chunk indexing type (with data)
     */

    /* Create dataspace */
    max_dims[0] = 10;
    max_dims[0] = H5S_UNLIMITED;
    max_dims[1] = H5S_UNLIMITED;
    if((sid = H5Screate_simple(2, dims2, max_dims)) < 0)
        goto error;

    /* Create the dataset */
    if((did1  = H5Dcreate2(gid, DSET_BT2, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    /* Write to the dataset */
    if(H5Dwrite(did1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        goto error;

    /* Closing */
    if(H5Sclose(sid) < 0)
        goto error;
    if(H5Dclose(did1) < 0)
        goto error;
    if(H5Pclose(dcpl) < 0)
        goto error;
    if(H5Gclose(gid) < 0)
        goto error;
    if(H5Fclose(fid) < 0)
        goto error;

    /* Re-open the file with old format */
    if((fid = H5Fopen(fname, H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
        goto error;

    /* Open the group */
    if((gid = H5Gopen2(fid, GROUP, H5P_DEFAULT)) < 0)
        goto error;

    /*
     * Create a dataset with version 2 B-btree chunk indexing type (without data)
     */

    /* Set chunk */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if(H5Pset_chunk(dcpl, 2, c_dims) < 0)
        goto error;

    /* Create dataspace */
    max_dims[0] = H5S_UNLIMITED;
    max_dims[1] = H5S_UNLIMITED;
    if((sid = H5Screate_simple(2, dims2, max_dims)) < 0)
        goto error;

    /* Create the dataset */
    if((did1 = H5Dcreate2(fid, DSET_NDATA_BT2, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    /* Close the dataspace */
    if(H5Sclose(sid) < 0)
        goto error;

    /*
     * Create a dataset with extensible array chunk indexing type (with data) in the group
     */

    /* Create dataspace */
    max_dims[0] = 10;
    max_dims[1] = H5S_UNLIMITED;
    if((sid = H5Screate_simple(2, dims2, max_dims)) < 0)
        goto error;

    /* Create the dataset */
    if((did2 = H5Dcreate2(gid, DSET_EA, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    /* Write to the dataset */
    if(H5Dwrite(did2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        goto error;

    /* Closing */
    if(H5Sclose(sid) < 0)
        goto error;
    if(H5Pclose(dcpl) < 0)
        goto error;
    if(H5Dclose(did1) < 0)
        goto error;
    if(H5Dclose(did2) < 0)
        goto error;

    /* 
     * Create a compact dataset in the group
     */

    /* Create dataspace */
    if((sid = H5Screate_simple(1, dims1, NULL)) < 0)
        goto error;

    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if(H5Pset_layout(dcpl, H5D_COMPACT) < 0)
        goto error;

    /* Create the dataset */
    if((did1  = H5Dcreate2(gid, DSET_COMPACT, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    /* Closing */
    if(H5Dclose(did1) < 0)
        goto error;
    if(H5Pclose(dcpl) < 0)
        goto error;
    if(H5Sclose(sid) < 0)
        goto error;

    /* 
     * Create a contiguous dataset with (2d with data) in the file
     */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if(H5Pset_layout(dcpl, H5D_CONTIGUOUS) < 0)
        goto error;

    if((sid = H5Screate_simple(2, dims2, NULL)) < 0)
        goto error;

    /* Create the dataset */
    if((did2  = H5Dcreate2(fid, DSET_CONTIGUOUS, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;
    /* Write to the dataset */
    if(H5Dwrite(did2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        goto error;

    /* Closing */
    if(H5Dclose(did2) < 0)
        goto error;
    if(H5Pclose(dcpl) < 0)
        goto error;
    if(H5Sclose(sid) < 0)
        goto error;

    if(H5Gclose(gid) < 0)
	goto error;
    if(H5Pclose(fcpl) < 0)
	goto error;
    if(H5Fclose(fid) < 0)
        goto error;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Sclose(sid);
        H5Dclose(did1);
        H5Dclose(did2);
        H5Gclose(gid);
        H5Fclose(fcpl);
        H5Fclose(fid);
    } H5E_END_TRY;

} /* gen_non() */

/*
 * Function: gen_edge() 
 *
 * Create a file with write+latest-format--this will result in v3 superblock+latest version support:
 *	A dataset: chunked, filtered, H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS enabled
 *	(i.e. the dataset does not filter partial edge chunks)
 */
static void
gen_edge(const char *fname)
{
    hid_t	fid = -1;	    	/* file id */
    hid_t	fapl = -1;	       	/* file access property list */
    hid_t   	sid = -1;       	/* dataspace id */
    hid_t	dcpl = -1;	    	/* dataset creation property id */
    hid_t	did = -1;		/* dataset id */
    hsize_t 	dims2[2] = {12, 6};	/* Dataset dimensions */
    hsize_t 	c_dims[2] = {5, 5};	/* Chunk dimensions */
    float 	buf[12][6];            	/* Buffer for writing data */
    int		i, j;		    	/* local index variable */

    /* Create a new format file */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        goto error;
    if((fid = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        goto error;

    /* Set chunk, filter, no-filter-edge-chunk */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if(H5Pset_chunk(dcpl, 2, c_dims) < 0)
        goto error;
    if(H5Pset_deflate(dcpl, 9) < 0)
        goto error;
    if(H5Pset_chunk_opts(dcpl, H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS) < 0)
        goto error;

    /* Create dataspace */
    if((sid = H5Screate_simple(2, dims2, NULL)) < 0)
        goto error;

    /* Create the dataset */
    if((did = H5Dcreate2(fid, DSET_EDGE, H5T_NATIVE_FLOAT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    /* Create data */
    for (i = 0; i< 12; i++)
        for (j = 0; j< 6; j++)
            buf[i][j] = 100.0F;

    /* Write to the dataset */
    if(H5Dwrite(did, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        goto error;

    /* Closing */
    if(H5Pclose(dcpl) < 0)
        goto error;
    if(H5Sclose(sid) < 0)
        goto error;
    if(H5Dclose(did) < 0)
        goto error;
    if(H5Pclose(fapl) < 0)
        goto error;
    if(H5Fclose(fid) < 0)
        goto error;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Sclose(sid);
        H5Dclose(did);
        H5Fclose(fid);
        H5Pclose(fapl);
    } H5E_END_TRY;

} /* gen_edge() */


/*
 * Function: gen_err_level() 
 *
 * Generate a file to test the situtation described in HDFFV-9434:
 *	Exceed the limit of v1-btree level
 *
 *	Create a file with H5Pset_istore_k(fcpl, 1).
 *	Create a chunked dataset with extensible array chunk index and 
 *	appends many chunks to the dataset.
 *
 *	When h5format_convert tries to convert the dataset with
 *	extensive array index in the file to v1-btree chunk index, 
 *	it will insert the dataset chunks to the v1-btree chunk index.
 *	The tree will split quickly due to the 'K' value of 1 and the 
 *	tree level will eventually hit the maximum: 2^8(256).
 */
static void
gen_err_level(const char *fname)
{
    hid_t fid = -1;	    	/* file ID */
    hid_t fapl = -1;	       	/* file access property list */
    hid_t fcpl = -1;	       	/* file creation property list */
    hid_t sid = -1;       	/* dataspace id */
    hid_t dcpl = -1;		/* dataset creation property list */
    hid_t did = -1;		/* dataset ID */
    hid_t fsid = -1;		/* file dataspace ID */
    hid_t msid = -1;		/* memory dataspace ID */
    unsigned char *buf = NULL;	/* buffer for data */
    hsize_t dims[2] = {0, 1};			/* dataset dimension sizes */
    hsize_t max_dims[2] = {1, H5S_UNLIMITED};	/* dataset maximum dimension sizes */
    hsize_t chunk_dims[2] = {1, 1};		/* chunk dimension sizes */
    int	n = 0;			/* local index variable */

    /* Create a new format file */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        goto error;

    /* Set 'K' value to 1 in file creation property list */
    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        goto error;
    if(H5Pset_istore_k(fcpl, ISTORE_ERR) < 0)
        goto error;

    /* Initialize data buffer */
    buf = (unsigned char *)HDmalloc(NUM * sizeof(unsigned char *));
    HDmemset(buf, 42, NUM * sizeof(unsigned char));

    /* Create the test file */
    if((fid = H5Fcreate(fname, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        goto error;

    /* Create a chunked dataset with extensible array chunk index */
    if((sid = H5Screate_simple(2, dims, max_dims)) < 0)
        goto error;
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        goto error;
    if((did = H5Dcreate2(fid, DSET_ERR, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    /* Closing */
    if(H5Pclose(dcpl) < 0)
        goto error;
    if(H5Sclose(sid) < 0)
        goto error;
    if(H5Dclose(did) < 0)
        goto error;
    if(H5Fclose(fid) < 0)
        goto error;

    /* Re-open the file */
    if((fid = H5Fopen(fname, H5F_ACC_RDWR, fapl)) < 0)
        goto error;

    /* Open the dataset */
    if((did = H5Dopen2(fid, DSET_ERR, H5P_DEFAULT)) < 0)
        goto error;

    /* Loop through appending 1 element at a time */
    for(n = 0; n < NUM; n++) {
	hsize_t start[2] = {0, 0};
	hsize_t count[2] = {1, 1};
	hsize_t extent[2] = {0, 0};

	start[0] = 0;
	start[1] = (hsize_t)n;
	extent[0] = 1;
	extent[1] = (hsize_t)(n + 1);

	/* Set current dimension sizes for the dataset */
	if(H5Dset_extent(did, extent) < 0)
	    goto error;

	/* Set up memory dataspace */
	if((msid = H5Screate_simple(2, count, NULL)) < 0)
	    goto error;

	/* Get file dataspace */
	if((fsid = H5Dget_space(did)) < 0)
	    goto error;

	if((H5Sselect_hyperslab(fsid, H5S_SELECT_SET, start, NULL, count, NULL)) < 0)
	    goto error;

	/* Write to the dataset */
	if(H5Dwrite(did, H5T_NATIVE_UCHAR, msid, fsid, H5P_DEFAULT, buf) < 0)
	    goto error;

	if(H5Sclose(fsid) < 0)
	    goto error;
	if(H5Sclose(msid) < 0)
	    goto error;
    }

    /* Closing */
    if(H5Dclose(did) < 0)
	goto error;
    if(H5Fclose(fid) < 0)
	goto error;
    if(H5Pclose(fapl) < 0)
	goto error;
    if(buf) free(buf);

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Sclose(sid);
        H5Dclose(did);
        H5Sclose(msid);
        H5Sclose(fsid);
        H5Fclose(fid);
        H5Pclose(fapl);
        H5Pclose(fcpl);
    } H5E_END_TRY;

} /* gen_err_level() */

/*
 * Function: gen_ext() 
 *
 * Create a file with/without latest format with:
 *	1) 1 contiguous dataset (without data)
 *	2) 2 chunked datasets with extensible array chunk indexing type (with/without data)
 *	3) 2 chunked datasets with version 2 B-tree chunk indexing type (with/without data)
 *	4) 2 chunked datasets with fixed array chunk indexing type (with/without data)
 *	5) 2 chunked datasets with implicit array chunk indexing type (with/without data)
 * It will create the file with/without messages in the superblock extension depending
 * on the parameter "what".
 */
static void
gen_ext(const char *fname, unsigned new_format, unsigned what)
{
    hid_t	fid = -1;	 	/* file id */
    hid_t	fapl = -1;	       	/* file access property list */
    hid_t	fcpl = -1;	       	/* file creation property list */
    hid_t	gid = -1;	       	/* group id */
    hid_t   	sid = -1;       	/* space id */
    hid_t	dcpl = -1;	    	/* dataset creation property id */
    hid_t	did1 = -1, did2 = -1;	/* dataset id */
    hsize_t 	dims1[1] = {10};     	/* dataset dimension */
    hsize_t 	dims2[2] = {4, 6};     	/* dataset dimension */
    hsize_t	max_dims[2];		/* maximum dataset dimension */
    hsize_t 	c_dims[2] = {2, 3};    	/* chunk dimension */
    int		i;		    	/* local index variable */
    int     	buf[24];            	/* data buffer */

    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;

    if(new_format) {
        /* Create a new format file */
        if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
            goto error;
    } /* end if */

    /* Create a file creation property list */
    fcpl = H5Pcreate(H5P_FILE_CREATE);

    /* Generate messages that might be placed in superblock extension */
    switch(what) {
	case 0:
	    H5Pset_istore_k(fcpl, ISTORE_IK);
	    break;
	case 1:
	    H5Pset_shared_mesg_nindexes(fcpl, 4);
	    break;
	case 2:
	    H5Pset_file_space(fcpl, H5F_FILE_SPACE_ALL_PERSIST, (hsize_t)0);
	    break;
	case 3:
	    H5Pset_istore_k(fcpl, ISTORE_IK);
	    H5Pset_shared_mesg_nindexes(fcpl, 4);
	    break;
	case 4:
	    H5Pset_istore_k(fcpl, ISTORE_IK);
	    H5Pset_file_space(fcpl, H5F_FILE_SPACE_DEFAULT, (hsize_t)2);
	    break;
	case 5:
	    H5Pset_shared_mesg_nindexes(fcpl, 4);
	    H5Pset_file_space(fcpl, H5F_FILE_SPACE_VFD, (hsize_t)0);
	    break;
	case 6:
	    H5Pset_istore_k(fcpl, ISTORE_IK);
	    H5Pset_shared_mesg_nindexes(fcpl, 4);
	    H5Pset_file_space(fcpl, H5F_FILE_SPACE_AGGR_VFD, (hsize_t)0);
	    break;
	default:
	    break;
    }

    /* Create the file */
    if((fid = H5Fcreate(fname, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        goto error;

    /* Create a group */
    if((gid = H5Gcreate2(fid, GROUP, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    /* Set chunk */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if(H5Pset_chunk(dcpl, 2, c_dims) < 0)
        goto error;


    /* 
     * Create a contiguous dataset 
     */

    /* Create dataspace */
    if((sid = H5Screate_simple(1, dims1, NULL)) < 0)
        goto error;

    /* Create the dataset */
    if((did1  = H5Dcreate2(fid, DSET_CONTIGUOUS, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    /* Closing */
    if(H5Sclose(sid) < 0)
        goto error;
    if(H5Dclose(did1) < 0)
        goto error;

    /* 
     * Create 2 chunked datasets with extensible array chunk indexing type 
     * (one with data; one without data)
     */

    /* Create dataspace */
    max_dims[0] = 10;
    max_dims[1] = H5S_UNLIMITED;
    if((sid = H5Screate_simple(2, dims2, max_dims)) < 0)
        goto error;

    /* Create the 2 datasets */
    if((did1  = H5Dcreate2(gid, DSET_NDATA_EA, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    if((did2  = H5Dcreate2(fid, DSET_EA, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    /* Create data */
    for(i = 0; i < 24; i++)
        buf[i] = i;

    /* Write to one dataset */
    if(H5Dwrite(did2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        goto error;

    /* Closing */
    if(H5Sclose(sid) < 0)
        goto error;
    if(H5Dclose(did1) < 0)
        goto error;
    if(H5Dclose(did2) < 0)
        goto error;


    /* 
     * Create 2 chunked datasets with version 2 B-tree chunk indexing type 
     * (one with data; one without data)
     */

    /* Create dataspace */
    max_dims[0] = 10;
    max_dims[0] = H5S_UNLIMITED;
    max_dims[1] = H5S_UNLIMITED;
    if((sid = H5Screate_simple(2, dims2, max_dims)) < 0)
        goto error;

    /* Create the 2 datasets */
    if((did1  = H5Dcreate2(fid, DSET_NDATA_BT2, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    if((did2  = H5Dcreate2(gid, DSET_BT2, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    /* Write to one dataset */
    if(H5Dwrite(did2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        goto error;

    /* Closing */
    if(H5Sclose(sid) < 0)
        goto error;
    if(H5Dclose(did1) < 0)
        goto error;
    if(H5Dclose(did2) < 0)
        goto error;

    /*
     * Create 2 chunked datasets with fixed array chunk indexing type 
     * (one with data; one without data)
     */

    /* Create dataspace */
    max_dims[0] = 20;
    max_dims[1] = 10;
    if((sid = H5Screate_simple(2, dims2, max_dims)) < 0)
        goto error;

    /* Create the datasets */
    if((did1  = H5Dcreate2(fid, DSET_FA, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    if((did2  = H5Dcreate2(gid, DSET_NDATA_FA, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    /* Write to the dataset */
    if(H5Dwrite(did1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        goto error;

    /* Closing */
    if(H5Sclose(sid) < 0)
        goto error;
    if(H5Dclose(did1) < 0)
        goto error;
    if(H5Dclose(did2) < 0)
        goto error;


    /* 
     * Create 2 chunked datasets with implicit chunk indexing type 
     * (one with data; one without data)
     */

    /* Create dataspace */
    if((sid = H5Screate_simple(2, dims2, NULL)) < 0)
        goto error;

    /* Set early allocation */
    if(H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0)
        goto error;

    /* Create the 2 datasets */
    if((did1  = H5Dcreate2(fid, DSET_NONE, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    if((did2  = H5Dcreate2(gid, DSET_NDATA_NONE, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    /* Write to one dataset */
    if(H5Dwrite(did1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        goto error;

    /* Closing */
    if(H5Dclose(did1) < 0)
        goto error;
    if(H5Dclose(did2) < 0)
        goto error;
    if(H5Sclose(sid) < 0)
        goto error;

    if(H5Pclose(dcpl) < 0)
        goto error;
    if(H5Gclose(gid) < 0)
        goto error;
    if(H5Fclose(fid) < 0)
        goto error;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Sclose(sid);
        H5Dclose(did1);
        H5Dclose(did2);
        H5Gclose(gid);
        H5Fclose(fid);
        H5Pclose(fcpl);
    } H5E_END_TRY;

} /* end gen_ext() */

int
main(void)
{
    unsigned i, new_format;

    /* Generate a non-latest-format file with v3 superblock */
    gen_non(NON_V3_FILE);

    /* Generate a new format file with a no-filter-edge-chunk dataset */
    gen_edge(EDGE_V3_FILE);

    /* Generate a new format file with 'K' value of 1 in H5Pset_istore_k() */
    gen_err_level(ERR_LEVEL_FILE);

    /* Generate old/new format file with/without messages in the superblock extension */
    for(new_format = FALSE; new_format <= TRUE; new_format++) {
        for(i = 0; i < 8; i++) {
            char filename[50];

            HDmemset(filename, 0, sizeof(filename));
            if(!new_format)
                HDstrcat(filename, "old_");
            HDstrcat(filename, FILENAME[i]);

            gen_ext(filename, new_format, i);
        } /* end for */
    } /* end for */

    return 0;
} /* end main */

