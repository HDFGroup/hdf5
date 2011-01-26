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
 * Programmer:  Raymond Lu <slu@ncsa.uiuc.edu>
 *              Thursday, March 23, 2006
 *
 *  This program writes floating-point data to the HDF5 file.  It generates
 *  the file to be read by cross_read.c, to test reading data from cross-systems.
 *  Run it on an OpenVMS, a little-endian, and a big-endian machine.  Change the
 *  output file names to vms_data.h5, le_data.h5, and be_data.h5, and put them
 *  under hdf5/test/ directory.
 */

#include "h5test.h"

#define H5FILE_NAME        "data.h5"
#define DATASETNAME        "Array"
#define DATASETNAME2       "Scale_offset_double_data"
#define DATASETNAME3       "Scale_offset_int_data"
#define NX                 6
#define NY                 6
#define RANK               2
#define CHUNK0             3
#define CHUNK1             3

int create_normal_dset(hid_t fid, hid_t sid);
int create_scale_offset_dset_double(hid_t fid, hid_t sid);
int create_scale_offset_dset_int(hid_t fid, hid_t sid);


/*-------------------------------------------------------------------------
 * Function:    create_normal_dset
 *
 * Purpose:     Create a regular dataset of DOUBLE datatype.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              Some time ago
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
create_normal_dset(hid_t fid, hid_t sid)
{
    hid_t       dataset;         /* file and dataset handles */
    herr_t      status;
    float       data[NX][NY];          /* data to write */
    int         i, j;

    /*
     * Data and output buffer initialization.
     */
    for (j = 0; j < NX; j++) {
	for (i = 0; i < NY; i++)
	    data[j][i] = i + j;
    }
    /*
     * 0 1 2 3 4 5
     * 1 2 3 4 5 6
     * 2 3 4 5 6 7
     * 3 4 5 6 7 8
     * 4 5 6 7 8 9
     * 5 6 7 8 9 10
     */

    /*
     * Create a new dataset within the file using defined dataspace and
     * datatype and default dataset creation properties.
     */
    dataset = H5Dcreate2(fid, DATASETNAME, H5T_NATIVE_FLOAT, sid,
			H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Write the data to the dataset using default transfer properties.
     */
    status = H5Dwrite(dataset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL,
		      H5P_DEFAULT, data);

    /*
     * Close/release resources.
     */
    H5Dclose(dataset);

    return 0;
}


/*-------------------------------------------------------------------------
 * Function:    create_scale_offset_dset_double
 *
 * Purpose:     Create a dataset of DOUBLE datatype with scale-offset filter
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              21 January 2011
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
create_scale_offset_dset_double(hid_t fid, hid_t sid)
{
#ifdef H5_HAVE_FILTER_SCALEOFFSET
    hid_t       dataset;         /* dataset handles */
    hid_t       dcpl;
    herr_t      status;
    double       data[NX][NY];          /* data to write */
    hsize_t     chunk[RANK] = {CHUNK0, CHUNK1};
    int         i, j;

    /*
     * Data and output buffer initialization.
     */
    for (j = 0; j < NX; j++) {
	for (i = 0; i < NY; i++)
	    data[j][i] = ((double)(i + j + 1))/3;
    }

    /*
     * Create the dataset creation property list, add the Scale-Offset
     * filter and set the chunk size.
     */
    dcpl = H5Pcreate (H5P_DATASET_CREATE);
    status = H5Pset_scaleoffset (dcpl, H5Z_SO_FLOAT_DSCALE, 3);
    status = H5Pset_chunk (dcpl, RANK, chunk);

    /*
     * Create a new dataset within the file using defined dataspace and
     * datatype and default dataset creation properties.
     */
    dataset = H5Dcreate2(fid, DATASETNAME2, H5T_NATIVE_DOUBLE, sid,
			H5P_DEFAULT, dcpl, H5P_DEFAULT);

    /*
     * Write the data to the dataset using default transfer properties.
     */
    status = H5Dwrite(dataset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,
		      H5P_DEFAULT, data);

    /*
     * Close/release resources.
     */
    H5Pclose(dcpl);
    H5Dclose(dataset);

#else /* H5_HAVE_FILTER_SCALEOFFSET */
    const char          *not_supported= "Scaleoffset filter is not enabled. Can't create the dataset.";

    puts(not_supported);
#endif /* H5_HAVE_FILTER_SCALEOFFSET */

    return 0;
}


/*-------------------------------------------------------------------------
 * Function:    create_scale_offset_dset_int
 *
 * Purpose:     Create a dataset of INT datatype with scale-offset filter
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              21 January 2011
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
create_scale_offset_dset_int(hid_t fid, hid_t sid)
{
#ifdef H5_HAVE_FILTER_SCALEOFFSET
    hid_t       dataset;         /* dataset handles */
    hid_t       dcpl;
    herr_t      status;
    int         data[NX][NY];          /* data to write */
    hsize_t     chunk[RANK] = {CHUNK0, CHUNK1};
    int         i, j;

    /*
     * Data and output buffer initialization.
     */
    for (j = 0; j < NX; j++) {
	for (i = 0; i < NY; i++)
	    data[j][i] = i + j;
    }
    /*
     * 0 1 2 3 4 5
     * 1 2 3 4 5 6
     * 2 3 4 5 6 7
     * 3 4 5 6 7 8
     * 4 5 6 7 8 9
     * 5 6 7 8 9 10
     */

    /*
     * Create the dataset creation property list, add the Scale-Offset
     * filter and set the chunk size.
     */
    dcpl = H5Pcreate (H5P_DATASET_CREATE);
    status = H5Pset_scaleoffset (dcpl, H5Z_SO_INT, H5Z_SO_INT_MINBITS_DEFAULT);
    status = H5Pset_chunk (dcpl, RANK, chunk);

    /*
     * Create a new dataset within the file using defined dataspace and
     * datatype and default dataset creation properties.
     */
    dataset = H5Dcreate2(fid, DATASETNAME3, H5T_NATIVE_INT, sid,
			H5P_DEFAULT, dcpl, H5P_DEFAULT);

    /*
     * Write the data to the dataset using default transfer properties.
     */
    status = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
		      H5P_DEFAULT, data);

    /*
     * Close/release resources.
     */
    H5Pclose(dcpl);
    H5Dclose(dataset);

#else /* H5_HAVE_FILTER_SCALEOFFSET */
    const char          *not_supported= "Scaleoffset filter is not enabled. Can't create the dataset.";

    puts(not_supported);
#endif /* H5_HAVE_FILTER_SCALEOFFSET */

    return 0;
}


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Create a file for cross_read.c test.
 *
 * Return:      Success:        exit(0)
 *              Failure:        exit(1)
 *
 * Programmer:  Raymond Lu
 *              Some time ago
 *
 *-------------------------------------------------------------------------
 */
int
main (void)
{
    hid_t       file;         /* file and dataset handles */
    hid_t       dataspace;
    hsize_t     dimsf[RANK];

    /*
     * Create a new file using H5F_ACC_TRUNC access,
     * default file creation properties, and default file
     * access properties.
     */
    file = H5Fcreate(H5FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Describe the size of the array and create the data space for fixed
     * size dataset.
     */
    dimsf[0] = NX;
    dimsf[1] = NY;
    dataspace = H5Screate_simple(RANK, dimsf, NULL);

    /* Create a regular dataset */
    create_normal_dset(file, dataspace);

    /* Create a dataset of DOUBLE with scale-offset filter */
    create_scale_offset_dset_double(file, dataspace);

    /* Create a dataset of INT with scale-offset filter */
    create_scale_offset_dset_int(file, dataspace);

    /*
     * Close/release resources.
     */
    H5Sclose(dataspace);
    H5Fclose(file);

    return 0;
}
