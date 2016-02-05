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

#define ISTORE_IK  64

/*
 * Function: gen_latest() 
 *
 * Create a file with write+latest-format--this will result in v3 superblock+latest version support:
 *	1) 2 chunked datasets with extensible array chunk indexing type (with/without data)
 *	2) 2 chunked datasets with version 2 B-tree chunk indexing type (with/without data)
 *	3) 2 chunked datasets with fixed array chunk indexing type (with/without data)
 *	4) 2 chunked datasets with implicit array chunk indexing type (with/without data)
 */

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

    /* Create a new file with SWMR_WRITE + non-latest-format */
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
gen_ext(const char *fname, unsigned new, unsigned what)
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

    if(new) {
	/* Create a new format file */
	if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
	    goto error;
    }

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
	    H5Pset_file_space(fcpl, 0, (hsize_t)2);
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
        H5Pclose(fapl);
        H5Pclose(fcpl);
    } H5E_END_TRY;

} /* gen_ext() */

int main(void)
{
    unsigned i, new;

    /* Generate a non-latest-format file with v3 superblock */
    gen_non(NON_V3_FILE);

    /* Generate a new format file with a no-filter-edge-chunk dataset */
    gen_edge(EDGE_V3_FILE);

    /* Generate old/new format file with/without messages in the superblock extension */
    for(new = FALSE; new <= TRUE; new++) {
	for(i = 0; i < 8; i++) {
            char filename[50];

	    HDmemset(filename, 0, sizeof(filename));
	    if(!new)
		HDstrcat(filename, "old_");
	    HDstrcat(filename, FILENAME[i]);

	    gen_ext(filename, new, i);
	}
    } /* end for new */

    return 0;
}
