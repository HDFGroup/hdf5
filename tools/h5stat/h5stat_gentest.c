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
 * generate all the binary hdf5 files in the ./testfiles directory.
 *
 * If you regenerate the test files (e.g., changing some code,
 * trying it on a new platform, ...), you need to verify the correctness
 * of the expected output and update the corresponding *.ddl files.
 */

#include <assert.h>
#include "hdf5.h"

#define FILE 		"h5stat_newgrat.h5"
#define DATASET_NAME	"DATASET_NAME"
#define GROUP_NAME	"GROUP"
#define ATTR_NAME	"ATTR"
#define NUM_GRPS 	35000
#define NUM_ATTRS	100

/* Declarations for gen_idx_file() */
#define FILE_IDX 	"h5stat_idx.h5"
#define DSET		"dset"
#define DSET_FILTER	"dset_filter"

/*
 * Generate 1.8 HDF5 file
 * with NUM_GRPS groups
 * with NUM_ATTRS attributes on the dataset
 */
static void gen_file(void)
{
    hid_t	fcpl; 	/* File creation property */
    hid_t	fapl; 	/* File access property */
    hid_t   	file;	/* File id */
    hid_t	gid;	/* Group id */
    hid_t	type_id;	/* Datatype id */
    hid_t	space_id; 	/* Dataspace id */
    hid_t	attr_id; 	/* Attribute id */
    hid_t	dset_id;	/* Dataset id */
    char	name[30];	/* Group name */
    char	attrname[30];	/* Attribute name */
    int     	ret;	/* Return value */
    int 	i;	/* Local index variable */

    fapl = H5Pcreate(H5P_FILE_ACCESS);
    ret = H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST);
    assert(ret >= 0);

    /* Set file space handling strategy */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    ret = H5Pset_file_space(fcpl, H5F_FILE_SPACE_ALL_PERSIST, (hsize_t)0);
    assert(ret >= 0);

     /* Create dataset */
    file = H5Fcreate(FILE, H5F_ACC_TRUNC, fcpl, fapl);
    for(i = 1; i <= NUM_GRPS; i++) {
        sprintf(name, "%s%d", GROUP_NAME,i);
        gid = H5Gcreate2(file, name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        H5Gclose(gid);
    } /* end for */

    /* Create a datatype to commit and use */
    type_id = H5Tcopy(H5T_NATIVE_INT);

    /* Create dataspace for dataset */
    space_id = H5Screate(H5S_SCALAR);

     /* Create dataset */
    dset_id = H5Dcreate2(file, DATASET_NAME, type_id, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    for(i = 1; i <= NUM_ATTRS; i++) {
        sprintf(attrname, "%s%d", ATTR_NAME,i);
        attr_id = H5Acreate2(dset_id, attrname, type_id, space_id, H5P_DEFAULT, H5P_DEFAULT);
        ret = H5Aclose(attr_id);
        assert(ret >= 0);
    } /* end for */

    ret = H5Dclose(dset_id);
    assert(ret >= 0);
    ret = H5Sclose(space_id);
    assert(ret >= 0);
    ret = H5Tclose(type_id);
    assert(ret >= 0);
    ret = H5Fclose(file);
    assert(ret >= 0);

} /* gen_file() */

/*
 * Function: gen_idx_file
 *
 * Purpose: Create a file with datasets that use Fixed Array indexing:
 *   	one dataset: fixed dimension, chunked layout, w/o filters
 *     	one dataset: fixed dimension, chunked layout, w/ filters
 *
 */
static void gen_idx_file(void)
{
    hid_t	fapl;		    /* file access property id */
    hid_t	fid;	            /* file id */
    hid_t   	sid;	            /* space id */
    hid_t	dcpl;	    	    /* dataset creation property id */
    hid_t	did, did2;	    /* dataset id */
    hsize_t 	dims[1] = {10};     /* dataset dimension */
    hsize_t 	c_dims[1] = {2};    /* chunk dimension */
    herr_t  	status;             /* return status */
    int		i;		    /* local index variable */
    int     	buf[10];            /* data buffer */


    /* Get a copy of the file aaccess property */
    fapl = H5Pcreate(H5P_FILE_ACCESS);

    /* Set the "use the latest format" bounds for creating objects in the file */
    status  = H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST);
    assert(status >= 0);

     /* Create dataset */
    fid = H5Fcreate(FILE_IDX, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);

    /* Create data */
    for(i = 0; i < 10; i++)
	buf[i] = i;

    /* Set chunk */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    status = H5Pset_chunk(dcpl, 1, c_dims);
    assert(status >= 0);

    /* Create a 1D dataset */
    sid = H5Screate_simple(1, dims, NULL);
    did  = H5Dcreate2(fid, DSET, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    
    /* Write to the dataset */
    status = H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
    assert(status >= 0);

#if defined (H5_HAVE_FILTER_DEFLATE)
    /* set deflate data */
    status = H5Pset_deflate(dcpl, 9);
    assert(status >= 0);

    /* Create and write the dataset */
    did2  = H5Dcreate2(fid, DSET_FILTER, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    status = H5Dwrite(did2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
    assert(status >= 0);

    /* Close the dataset */
    status = H5Dclose(did2);
    assert(status >= 0);

#endif

    /* closing: dataspace, dataset, file */
    status = H5Sclose(sid);
    assert(status >= 0);
    status = H5Dclose(did);
    assert(status >= 0);
    status = H5Fclose(fid);
    assert(status >= 0);

} /* gen_idx_file() */

int main(void)
{
    gen_file();

    /* Generate an HDF file to test for datasets with Fixed Array indexing */
    gen_idx_file();

    return 0;
}

