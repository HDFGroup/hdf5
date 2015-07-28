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

#include "hdf5.h"

#define NELMTS(X)    	(sizeof(X)/sizeof(X[0]))	/* # of elements */
#define TEST_THRESHOLD2	2             			/* Free space section threshold */

const char *FILENAMES[] = {
    "filespace_persist.h5",	/* H5F_FILE_SPACE_ALL_PERSIST */
    "filespace_default.h5",	/* H5F_FILE_SPACE_ALL */
    "filespace_aggr_vfd.h5",	/* H5F_FILE_SPACE_AGGR_VFD */
    "filespace_vfd.h5",		/* H5F_FILE_SPACE_VFD */
    "filespace_threshold.h5"	/* H5F_FILE_SPACE_ALL, non-default threshold */
};

#define DATASET		"dset"
#define NUM_ELMTS	100

/*
 * Compile and run this program in file-space branch to generate
 * HDF5 files with different kinds of file space strategies
 * Move the HDF5 files to the 1.6 and 1.8 branch for compatibility
 * testing:test_filespace_compatible() will use the files
 */
static void gen_file(void)
{
    hid_t   	fid;
    hid_t   	fcpl;
    hid_t       dataset, space;
    hsize_t     dim[1];
    int         data[NUM_ELMTS];
    size_t      j;			/* Local index variable */
    int         i;			/* Local index variable */
    H5F_file_space_type_t fs_type;	/* File space handling strategy */

    for(j = 0, fs_type = H5F_FILE_SPACE_ALL_PERSIST; j < NELMTS(FILENAMES); j++, fs_type = (H5F_file_space_type_t)(fs_type + 1)) {
	/* Get a copy of the default file creation property */
	fcpl = H5Pcreate(H5P_FILE_CREATE);

	if(fs_type == H5F_FILE_SPACE_NTYPES) /* last file */
	    /* Set default strategy but non-default threshold */
	    H5Pset_file_space(fcpl, H5F_FILE_SPACE_ALL, (hsize_t)TEST_THRESHOLD2);
	else
	    /* Set specified file space strategy and free space section threshold */
	    H5Pset_file_space(fcpl, fs_type, (hsize_t)0);

	/* Create the file with the file space info */
	fid = H5Fcreate(FILENAMES[j], H5F_ACC_TRUNC, fcpl, H5P_DEFAULT);

	dim[0] = NUM_ELMTS;
	space = H5Screate_simple(1, dim, NULL);
	dataset = H5Dcreate2(fid, DATASET, H5T_NATIVE_INT, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

	for(i = 0; i < NUM_ELMTS; i++)
	    data[i] = i;

	H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
	H5Dclose(dataset);
	H5Sclose(space);
	H5Fclose(fid);
    }
}

int main(void)
{
    gen_file();

    return 0;
}
