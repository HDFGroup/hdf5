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
 * Generate the binary hdf5 files for the h5stat tests.
 * Usage: just execute the program without any arguments will
 * generate all the binary hdf5 files 
 *
 * If you regenerate the test files (e.g., changing some code,
 * trying it on a new platform, ...), you need to verify the correctness
 * of the expected output and update the corresponding *.ddl files.
 */
#include "hdf5.h"

/* For gen_newgrat_file() */
#define NEWGRAT_FILE 	"h5stat_newgrat.h5"
#define DATASET_NAME	"DATASET_NAME"
#define GROUP_NAME	"GROUP"
#define ATTR_NAME	"ATTR"
#define NUM_GRPS 	35000
#define NUM_ATTRS	100

/* For gen_threshold_file() */
#define THRESHOLD_FILE 		"h5stat_threshold.h5"
#define THRES_ATTR_NAME		"attr"
#define THRES_ATTR_GRP_NAME	"grp_attr"
#define THRES_DSET_NAME 	"dset"
#define THRES_NUM		10
#define THRES_NUM_25		25

/*
 * Generate HDF5 file with latest format with
 * NUM_GRPS groups and NUM_ATTRS attributes for the dataset
 *
 */
static void 
gen_newgrat_file(const char *fname)
{
    hid_t fapl; 	/* File access property */
    hid_t fid;		/* File id */
    hid_t gid;		/* Group id */
    hid_t tid;		/* Datatype id */
    hid_t sid; 		/* Dataspace id */
    hid_t attr_id; 	/* Attribute id */
    hid_t did;		/* Dataset id */
    char name[30];	/* Group name */
    char attrname[30];	/* Attribute name */
    int  i;		/* Local index variable */

    /* Get a copy file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
	goto error;

    /* Set to use latest library format */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
	goto error;

     /* Create dataset */
    if((fid = H5Fcreate(NEWGRAT_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
	goto error;

    /* Create NUM_GRPS groups in the root group */
    for(i = 1; i <= NUM_GRPS; i++) {
        sprintf(name, "%s%d", GROUP_NAME,i);
        if((gid = H5Gcreate2(fid, name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
	    goto error;
        if(H5Gclose(gid) < 0)
	    goto error;
    } /* end for */

    /* Create a datatype to commit and use */
    if((tid = H5Tcopy(H5T_NATIVE_INT)) < 0)
	goto error;

    /* Create dataspace for dataset */
    if((sid = H5Screate(H5S_SCALAR)) < 0)
	goto error;

    /* Create dataset */
    if((did = H5Dcreate2(fid, DATASET_NAME, tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
	goto error;

    /* Create NUM_ATTRS for the dataset */
    for(i = 1; i <= NUM_ATTRS; i++) {
        sprintf(attrname, "%s%d", ATTR_NAME,i);
        if((attr_id = H5Acreate2(did, attrname, tid, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
	    goto error;
        if(H5Aclose(attr_id) < 0)
	    goto error;
    } /* end for */

    /* Close dataset, dataspace, datatype, file */
    if(H5Dclose(did) < 0)
	goto error;
    if(H5Sclose(sid) < 0)
	goto error;
    if(H5Tclose(tid) < 0)
	goto error;
    if(H5Fclose(fid) < 0)
	goto error;

error:
    H5E_BEGIN_TRY {
	H5Aclose(attr_id);
        H5Dclose(did);
        H5Tclose(tid);
        H5Sclose(sid);
        H5Gclose(gid);
        H5Fclose(fid);
    } H5E_END_TRY;

} /* gen_newgrat_file() */

/*
 * Generate an HDF5 file with groups, datasets, attributes for testing the options:
 *	-l N (--links=N): Set the threshold for # of links when printing information for small groups.
 *	-m N (--dims=N): Set the threshold for the # of dimension sizes when printing information for small datasets.
 *	-a N (--numattrs=N): Set the threshold for the # of attributes when printing information for small # of attributes.
 */
static void
gen_threshold_file(const char *fname)
{
    hid_t fid;				/* File ID */
    hid_t sid0, sid1, sid2, sid3, sid4;	/* Dataspace IDs */
    hid_t did;				/* Dataset ID */
    hid_t attr_id;			/* Attribute ID */
    hid_t gid;				/* Group ID */
    hsize_t two_dims[] = {2, 5};	/* Dimension array */
    hsize_t one_dims[] = {6};		/* Dimension array */
    hsize_t zero_dims[] = {0};		/* Dimension array */
    char name[30];			/* Name */
    unsigned i;				/* Local index variable */

    /* Create file */
    if((fid = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
	goto error;

    /* Create 1-D dataspace with zero dimension size */
    if((sid0 = H5Screate_simple(1, zero_dims, NULL)) < 0)
	goto error;

    /* Create 1-D dataspace with non-zero dimension size*/
    if((sid1 = H5Screate_simple(1, one_dims, NULL)) < 0)
	goto error;

    /* Create 2-D dataspace */
    if((sid2 = H5Screate_simple(2, two_dims, NULL)) < 0)
	goto error;

    /* Create scalar dataspace */
    if((sid3 = H5Screate(H5S_SCALAR)) < 0)
	goto error;

    /* Create null dataspace */
    if((sid4 = H5Screate(H5S_NULL)) < 0)
	goto error;

    /* Create an attribute for the root group */
    if((attr_id = H5Acreate2(fid, "attr", H5T_NATIVE_INT, sid1, H5P_DEFAULT, H5P_DEFAULT)) < 0)
	goto error;
    if(H5Aclose(attr_id) < 0)
	goto error;

    /* Create 1-D dataset with zero dimension size for the root group */
    if((did = H5Dcreate2(fid, "zero_dset", H5T_NATIVE_UCHAR, sid0, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
	goto error;

    /* Create 11 attributes for the dataset */
    for(i = 1; i <= (THRES_NUM+1); i++) {
        sprintf(name, "%s%d", THRES_ATTR_NAME,i);
        if((attr_id = H5Acreate2(did, name, H5T_NATIVE_INT, sid1, H5P_DEFAULT, H5P_DEFAULT)) < 0)
	    goto error;
        if(H5Aclose(attr_id) < 0)
	    goto error;
    }
    if(H5Dclose(did) < 0)
	goto error;

    /* Create dataset with scalar dataspace for the root group */
    if((did = H5Dcreate2(fid, "scalar_dset", H5T_NATIVE_UCHAR, sid3, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
	goto error;
    if(H5Dclose(did) < 0)
	goto error;

    /* Create dataset with null dataspace for the root group */
    if((did = H5Dcreate2(fid, "null_dset", H5T_NATIVE_UCHAR, sid4, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
	goto error;
    if(H5Dclose(did) < 0)
	goto error;

    /* Create 2-D dataset for the root group */
    if((did = H5Dcreate2(fid, "dset", H5T_NATIVE_UCHAR, sid2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
	goto error;

    /* Create 10 attributes for the 2-D dataset */
    for(i = 1; i <= THRES_NUM; i++) {
        sprintf(name, "%s%d", THRES_ATTR_NAME,i);
        if((attr_id = H5Acreate2(did, name, H5T_NATIVE_INT, sid1, H5P_DEFAULT, H5P_DEFAULT)) < 0)
	    goto error;
        if(H5Aclose(attr_id) < 0)
	    goto error;
    }
    if(H5Dclose(did) < 0)
	goto error;

    /* Create first group */
    if((gid = H5Gcreate2(fid, "group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
	goto error;

    /* Create an attribute for the group */
    if((attr_id = H5Acreate2(gid, "ATTR", H5T_NATIVE_INT, sid3, H5P_DEFAULT, H5P_DEFAULT)) < 0)
	goto error;

    /* Close attribute */
    if(H5Aclose(attr_id) < 0)
	goto error;

    /* Create 10 1-D datasets with non-zero dimension size for the group */
    for(i = 1; i <= THRES_NUM; i++) {
	/* set up dataset name */
        sprintf(name, "%s%d", THRES_DSET_NAME,i);

	/* Create the dataset */
	if((did = H5Dcreate2(gid, name, H5T_NATIVE_UCHAR, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
	    goto error;

	/* Close the dataset */
        if(H5Dclose(did) < 0)
	    goto error;
    }

    /* Close the group */
    if(H5Gclose(gid) < 0)
	goto error;


    /* Create second group */
    if((gid = H5Gcreate2(fid, "group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
	goto error;

    /* Create 25 attributes for the group */
    for(i = 1; i <= THRES_NUM_25; i++) {
	/* Set up attribute name */
        sprintf(name, "%s%d", THRES_ATTR_GRP_NAME,i);

	/* Create the attribute */
        if((attr_id = H5Acreate2(gid, name, H5T_NATIVE_INT, sid2, H5P_DEFAULT, H5P_DEFAULT)) < 0)
	    goto error;

	/* Close the attribute */
        if(H5Aclose(attr_id) < 0)
	    goto error;
    }

    /* Close the group */
    if(H5Gclose(gid) < 0)
	goto error;

    /* Create third group */
    if((gid = H5Gcreate2(fid, "group3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
	goto error;

    /* Create 9 1-D datasets with non-zero dimension size for the group */
    for(i = 1; i < THRES_NUM; i++) {
	/* set up dataset name */
        sprintf(name, "%s%d", THRES_DSET_NAME,i);

	/* Create the dataset */
	if((did = H5Dcreate2(gid, name, H5T_NATIVE_UCHAR, sid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
	    goto error;

	/* Close the dataset */
        if(H5Dclose(did) < 0)
	    goto error;
    }

    /* Close the group */
    if(H5Gclose(gid) < 0)
	goto error;


    /* Close dataspaces */
    if(H5Sclose(sid0) < 0)
	goto error;
    if(H5Sclose(sid1) < 0)
	goto error;
    if(H5Sclose(sid2) < 0)
	goto error;
    if(H5Sclose(sid3) < 0)
	goto error;
    if(H5Sclose(sid4) < 0)
	goto error;

    /* Close file */
    if(H5Fclose(fid) < 0)
	goto error;

error:
    H5E_BEGIN_TRY {
        H5Gclose(gid);
	H5Aclose(attr_id);
        H5Dclose(did);
        H5Sclose(sid0);
        H5Sclose(sid1);
        H5Sclose(sid2);
        H5Sclose(sid3);
        H5Sclose(sid4);
        H5Fclose(fid);
    } H5E_END_TRY;

} /* gen_threshold_file() */

int main(void)
{
    gen_newgrat_file(NEWGRAT_FILE);
    gen_threshold_file(THRESHOLD_FILE);

    return 0;
}

