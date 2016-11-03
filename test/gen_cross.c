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
 *
 *  Note that we no longer support OpenVMS. The OpenVMS file will eventually
 *  have to go away since we won't be able to re-create it but it's probably
 *  worth keeping around for now.
 */

#include <stdio.h>
#include "h5test.h"

#define H5FILE_NAME        "data.h5"
#define DATASETNAME        "Array_le"
#define DATASETNAME1       "Array_be"
#define DATASETNAME2       "Scale_offset_float_data_le"
#define DATASETNAME3       "Scale_offset_float_data_be"
#define DATASETNAME4       "Scale_offset_double_data_le"
#define DATASETNAME5       "Scale_offset_double_data_be"
#define DATASETNAME6       "Scale_offset_char_data_le"
#define DATASETNAME7       "Scale_offset_char_data_be"
#define DATASETNAME8       "Scale_offset_short_data_le"
#define DATASETNAME9       "Scale_offset_short_data_be"
#define DATASETNAME10      "Scale_offset_int_data_le"
#define DATASETNAME11      "Scale_offset_int_data_be"
#define DATASETNAME12      "Scale_offset_long_long_data_le"
#define DATASETNAME13      "Scale_offset_long_long_data_be"

#define DATASETNAME14      "Fletcher_float_data_le"
#define DATASETNAME15      "Fletcher_float_data_be"
#define DATASETNAME16      "Deflate_float_data_le"
#define DATASETNAME17      "Deflate_float_data_be"
#ifdef H5_HAVE_FILTER_SZIP
#define DATASETNAME18      "Szip_float_data_le"
#define DATASETNAME19      "Szip_float_data_be"
#endif /* H5_HAVE_FILTER_SZIP */
#define DATASETNAME20      "Shuffle_float_data_le"
#define DATASETNAME21      "Shuffle_float_data_be"
#define DATASETNAME22      "Nbit_float_data_le"
#define DATASETNAME23      "Nbit_float_data_be"

#define NX                 6
#define NY                 6
#define RANK               2
#define CHUNK0             4
#define CHUNK1             3

int create_normal_dset(hid_t fid, hid_t fsid, hid_t msid);
int create_scale_offset_dsets_float(hid_t fid, hid_t fsid, hid_t msid);
int create_scale_offset_dsets_double(hid_t fid, hid_t fsid, hid_t msid);
int create_scale_offset_dsets_char(hid_t fid, hid_t fsid, hid_t msid);
int create_scale_offset_dsets_short(hid_t fid, hid_t fsid, hid_t msid);
int create_scale_offset_dsets_int(hid_t fid, hid_t fsid, hid_t msid);
int create_scale_offset_dsets_long_long(hid_t fid, hid_t fsid, hid_t msid);
int create_fletcher_dsets_float(hid_t fid, hid_t fsid, hid_t msid);
int create_deflate_dsets_float(hid_t fid, hid_t fsid, hid_t msid);
#ifdef H5_HAVE_FILTER_SZIP
int create_szip_dsets_float(hid_t fid, hid_t fsid, hid_t msid);
#endif /* H5_HAVE_FILTER_SZIP */
int create_shuffle_dsets_float(hid_t fid, hid_t fsid, hid_t msid);
int create_nbit_dsets_float(hid_t fid, hid_t fsid, hid_t msid);


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
create_normal_dset(hid_t fid, hid_t fsid, hid_t msid)
{
    hid_t       dataset         = -1;   /* file and dataset handles */
    hid_t       dcpl            = -1;
    float       data[NX][NY];           /* data to write */
    float       fillvalue = -2.2f;
    int         i, j;

    /*
     * Data and output buffer initialization.
     */
    for (j = 0; j < NX; j++) {
	for (i = 0; i < NY; i++)
	    data[j][i] = ((float)(i + j + 1)) / 3;
    }
    /*
     * 1/3 2/3 3/3 4/3 5/3 6/3
     * 2/3 3/3 4/3 5/3 6/3 7/3
     * 3/3 4/3 5/3 6/3 7/3 8/3
     * 4/3 5/3 6/3 7/3 8/3 9/3
     * 5/3 6/3 7/3 8/3 9/3 10/3
     * 6/3 7/3 8/3 9/3 10/3 11/3
     * -2.2 -2.2 -2.2 -2.2 -2.2 -2.2
     */

    /*
     * Create the dataset creation property list, set the fill value.
     */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR
    if(H5Pset_fill_value(dcpl, H5T_NATIVE_FLOAT, &fillvalue) < 0)
        TEST_ERROR

    /*
     * Create a new dataset within the file using defined dataspace and
     * little-endian datatype and default dataset creation properties.
     */
    if((dataset = H5Dcreate2(fid, DATASETNAME, H5T_IEEE_F64LE, fsid,
            H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /*
     * Write the data to the dataset using default transfer properties.
     */
    if(H5Dwrite(dataset, H5T_NATIVE_FLOAT, msid, fsid, H5P_DEFAULT, data) < 0)
        TEST_ERROR

    /* 
     * Close dataset
     */
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /*
     * Create a new dataset within the file using defined dataspace and
     * big-endian datatype and default dataset creation properties.
     */
    if((dataset = H5Dcreate2(fid, DATASETNAME1, H5T_IEEE_F64BE, fsid,
            H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /*
     * Write the data to the dataset using default transfer properties.
     */
    if(H5Dwrite(dataset, H5T_NATIVE_FLOAT, msid, fsid, H5P_DEFAULT, data) < 0)
        TEST_ERROR

    /* 
     * Close dataset
     */
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /*
     * Close/release resources.
     */
    if(H5Pclose(dcpl) < 0)
        TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Dclose(dataset);
    } H5E_END_TRY;

    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    create_scale_offset_dsets_float
 *
 * Purpose:     Create a dataset of FLOAT datatype with scale-offset filter
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Neil Fortner
 *              27 January 2011
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
create_scale_offset_dsets_float(hid_t fid, hid_t fsid, hid_t msid)
{
    hid_t       dataset             = -1;   /* dataset handles */
    hid_t       dcpl                = -1;
    float       data[NX][NY];               /* data to write */
    float       fillvalue = -2.2f;
    hsize_t     chunk[RANK] = {CHUNK0, CHUNK1};
    int         i, j;

    /*
     * Data and output buffer initialization.
     */
    for (j = 0; j < NX; j++) {
        for (i = 0; i < NY; i++)
            data[j][i] = ((float)(i + j + 1))/3;
    }

    /*
     * Create the dataset creation property list, add the Scale-Offset
     * filter, set the chunk size, and set the fill value.
     */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR
    if(H5Pset_scaleoffset(dcpl, H5Z_SO_FLOAT_DSCALE, 3) < 0)
        TEST_ERROR
    if(H5Pset_chunk(dcpl, RANK, chunk) < 0)
        TEST_ERROR
    if(H5Pset_fill_value(dcpl, H5T_NATIVE_FLOAT, &fillvalue) < 0)
        TEST_ERROR

    /*
     * Create a new dataset within the file using defined dataspace, little
     * endian datatype and default dataset creation properties.
     */
    if((dataset = H5Dcreate2(fid, DATASETNAME2, H5T_IEEE_F32LE, fsid,
            H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /*
     * Write the data to the dataset using default transfer properties.
     */
    if(H5Dwrite(dataset, H5T_NATIVE_FLOAT, msid, fsid, H5P_DEFAULT, data) < 0)
        TEST_ERROR

    /* Close dataset */
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /* Now create a dataset with a big-endian type */
    if((dataset = H5Dcreate2(fid, DATASETNAME3, H5T_IEEE_F32BE, fsid,
            H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Dwrite(dataset, H5T_NATIVE_FLOAT, msid, fsid, H5P_DEFAULT, data) < 0)
        TEST_ERROR
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /*
     * Close/release resources.
     */
    if(H5Pclose(dcpl) < 0)
        TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Dclose(dataset);
    } H5E_END_TRY;

    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    create_scale_offset_dsets_double
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
create_scale_offset_dsets_double(hid_t fid, hid_t fsid, hid_t msid)
{
    hid_t       dataset         = -1;   /* dataset handles */
    hid_t       dcpl            = -1;
    double      data[NX][NY];           /* data to write */
    double      fillvalue = -2.2f;
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
     * filter, set the chunk size, and set the fill value.
     */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR
    if(H5Pset_scaleoffset(dcpl, H5Z_SO_FLOAT_DSCALE, 3) < 0)
        TEST_ERROR
    if(H5Pset_chunk(dcpl, RANK, chunk) < 0)
        TEST_ERROR
    if(H5Pset_fill_value(dcpl, H5T_NATIVE_DOUBLE, &fillvalue) < 0)
        TEST_ERROR

    /*
     * Create a new dataset within the file using defined dataspace, little
     * endian datatype and default dataset creation properties.
     */
    if((dataset = H5Dcreate2(fid, DATASETNAME4, H5T_IEEE_F64LE, fsid,
            H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /*
     * Write the data to the dataset using default transfer properties.
     */
    if(H5Dwrite(dataset, H5T_NATIVE_DOUBLE, msid, fsid, H5P_DEFAULT, data) < 0)
        TEST_ERROR

    /* Close dataset */
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /* Now create a dataset with a big-endian type */
    if((dataset = H5Dcreate2(fid, DATASETNAME5, H5T_IEEE_F64BE, fsid,
            H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Dwrite(dataset, H5T_NATIVE_DOUBLE, msid, fsid, H5P_DEFAULT, data) < 0)
        TEST_ERROR
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /*
     * Close/release resources.
     */
    if(H5Pclose(dcpl) < 0)
        TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Dclose(dataset);
    } H5E_END_TRY;

    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    create_scale_offset_dset_char
 *
 * Purpose:     Create a dataset of CHAR datatype with scale-offset filter
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Neil Fortner
 *              27 January 2011
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
create_scale_offset_dsets_char(hid_t fid, hid_t fsid, hid_t msid)
{
    hid_t       dataset         = -1;   /* dataset handles */
    hid_t       dcpl            = -1;
    char        data[NX][NY];           /* data to write */
    char        fillvalue = -2;
    hsize_t     chunk[RANK] = {CHUNK0, CHUNK1};
    int         i, j;

    /*
     * Data and output buffer initialization.
     */
    for (j = 0; j < NX; j++) {
        for (i = 0; i < NY; i++)
            data[j][i] = (char)(i + j);
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
     * filter, set the chunk size, and set the fill value.
     */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR
    if(H5Pset_scaleoffset(dcpl, H5Z_SO_INT, H5Z_SO_INT_MINBITS_DEFAULT) < 0)
        TEST_ERROR
    if(H5Pset_chunk(dcpl, RANK, chunk) < 0)
        TEST_ERROR
    if(H5Pset_fill_value(dcpl, H5T_NATIVE_CHAR, &fillvalue) < 0)
        TEST_ERROR

    /*
     * Create a new dataset within the file using defined dataspace, little
     * endian datatype and default dataset creation properties.
     */
    if((dataset = H5Dcreate2(fid, DATASETNAME6, H5T_STD_I8LE, fsid, H5P_DEFAULT,
            dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /*
     * Write the data to the dataset using default transfer properties.
     */
    if(H5Dwrite(dataset, H5T_NATIVE_CHAR, msid, fsid, H5P_DEFAULT, data) < 0)
        TEST_ERROR

    /* Close dataset */
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /* Now create a dataset with a big-endian type */
    if((dataset = H5Dcreate2(fid, DATASETNAME7, H5T_STD_I8BE, fsid, H5P_DEFAULT,
            dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Dwrite(dataset, H5T_NATIVE_CHAR, msid, fsid, H5P_DEFAULT, data) < 0)
        TEST_ERROR
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /*
     * Close/release resources.
     */
    if(H5Pclose(dcpl) < 0)
        TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Dclose(dataset);
    } H5E_END_TRY;

    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    create_scale_offset_dset_short
 *
 * Purpose:     Create a dataset of SHORT datatype with scale-offset filter
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Neil Fortner
 *              27 January 2011
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
create_scale_offset_dsets_short(hid_t fid, hid_t fsid, hid_t msid)
{
    hid_t       dataset         = -1;   /* dataset handles */
    hid_t       dcpl            = -1;
    short       data[NX][NY];           /* data to write */
    short       fillvalue = -2;
    hsize_t     chunk[RANK] = {CHUNK0, CHUNK1};
    int         i, j;

    /*
     * Data and output buffer initialization.
     */
    for (j = 0; j < NX; j++) {
        for (i = 0; i < NY; i++)
            data[j][i] = (short)(i + j);
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
     * filter, set the chunk size, and set the fill value.
     */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR
    if(H5Pset_scaleoffset(dcpl, H5Z_SO_INT, H5Z_SO_INT_MINBITS_DEFAULT) < 0)
        TEST_ERROR
    if(H5Pset_chunk(dcpl, RANK, chunk) < 0)
        TEST_ERROR
    if(H5Pset_fill_value(dcpl, H5T_NATIVE_SHORT, &fillvalue) < 0)
        TEST_ERROR

    /*
     * Create a new dataset within the file using defined dataspace, little
     * endian datatype and default dataset creation properties.
     */
    if((dataset = H5Dcreate2(fid, DATASETNAME8, H5T_STD_I16LE, fsid,
            H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /*
     * Write the data to the dataset using default transfer properties.
     */
    if(H5Dwrite(dataset, H5T_NATIVE_SHORT, msid, fsid, H5P_DEFAULT, data) < 0)
        TEST_ERROR

    /* Close dataset */
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /* Now create a dataset with a big-endian type */
    if((dataset = H5Dcreate2(fid, DATASETNAME9, H5T_STD_I16BE, fsid,
            H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Dwrite(dataset, H5T_NATIVE_SHORT, msid, fsid, H5P_DEFAULT, data) < 0)
        TEST_ERROR
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /*
     * Close/release resources.
     */
    if(H5Pclose(dcpl) < 0)
        TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Dclose(dataset);
    } H5E_END_TRY;

    return -1;
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
create_scale_offset_dsets_int(hid_t fid, hid_t fsid, hid_t msid)
{
    hid_t       dataset         = -1;   /* dataset handles */
    hid_t       dcpl            = -1;
    int         data[NX][NY];           /* data to write */
    int         fillvalue = -2;
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
     * filter, set the chunk size, and set the fill value.
     */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR
    if(H5Pset_scaleoffset(dcpl, H5Z_SO_INT, H5Z_SO_INT_MINBITS_DEFAULT) < 0)
        TEST_ERROR
    if(H5Pset_chunk(dcpl, RANK, chunk) < 0)
        TEST_ERROR
    if(H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillvalue) < 0)
        TEST_ERROR

    /*
     * Create a new dataset within the file using defined dataspace, little
     * endian datatype and default dataset creation properties.
     */
    if((dataset = H5Dcreate2(fid, DATASETNAME10, H5T_STD_I32LE, fsid,
            H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /*
     * Write the data to the dataset using default transfer properties.
     */
    if(H5Dwrite(dataset, H5T_NATIVE_INT, msid, fsid, H5P_DEFAULT, data) < 0)
        TEST_ERROR

    /* Close dataset */
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /* Now create a dataset with a big-endian type */
    if((dataset = H5Dcreate2(fid, DATASETNAME11, H5T_STD_I32BE, fsid,
            H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Dwrite(dataset, H5T_NATIVE_INT, msid, fsid, H5P_DEFAULT, data) < 0)
        TEST_ERROR
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /*
     * Close/release resources.
     */
    if(H5Pclose(dcpl) < 0)
        TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Dclose(dataset);
    } H5E_END_TRY;

    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    create_scale_offset_dset_long_long
 *
 * Purpose:     Create a dataset of LONG LONG datatype with scale-offset
 *              filter
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Neil Fortner
 *              27 January 2011
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
create_scale_offset_dsets_long_long(hid_t fid, hid_t fsid, hid_t msid)
{
    hid_t       dataset         = -1;   /* dataset handles */
    hid_t       dcpl            = -1;
    long long   data[NX][NY];           /* data to write */
    long long   fillvalue = -2;
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
     * filter, set the chunk size, and set the fill value.
     */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR
    if(H5Pset_scaleoffset(dcpl, H5Z_SO_INT, H5Z_SO_INT_MINBITS_DEFAULT) < 0)
        TEST_ERROR
    if(H5Pset_chunk(dcpl, RANK, chunk) < 0)
        TEST_ERROR
    if(H5Pset_fill_value(dcpl, H5T_NATIVE_LLONG, &fillvalue) < 0)
        TEST_ERROR

    /*
     * Create a new dataset within the file using defined dataspace, little
     * endian datatype and default dataset creation properties.
     */
    if((dataset = H5Dcreate2(fid, DATASETNAME12, H5T_STD_I64LE, fsid,
            H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /*
     * Write the data to the dataset using default transfer properties.
     */
    if(H5Dwrite(dataset, H5T_NATIVE_LLONG, msid, fsid, H5P_DEFAULT, data) < 0)
        TEST_ERROR

    /* Close dataset */
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /* Now create a dataset with a big-endian type */
    if((dataset = H5Dcreate2(fid, DATASETNAME13, H5T_STD_I64BE, fsid,
            H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Dwrite(dataset, H5T_NATIVE_LLONG, msid, fsid, H5P_DEFAULT, data) < 0)
        TEST_ERROR
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /*
     * Close/release resources.
     */
    if(H5Pclose(dcpl) < 0)
        TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Dclose(dataset);
    } H5E_END_TRY;

    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    create_fletcher_dsets_float
 *
 * Purpose:     Create a dataset of FLOAT datatype with fletcher filter
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              29 March 2011
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
create_fletcher_dsets_float(hid_t fid, hid_t fsid, hid_t msid)
{
    hid_t       dataset         = -1;   /* dataset handles */
    hid_t       dcpl            = -1;
    float       data[NX][NY];           /* data to write */
    float       fillvalue = -2.2f;
    hsize_t     chunk[RANK] = {CHUNK0, CHUNK1};
    int         i, j;

    /*
     * Data and output buffer initialization.
     */
    for (j = 0; j < NX; j++) {
        for (i = 0; i < NY; i++)
            data[j][i] = ((float)(i + j + 1))/3;
    }

    /*
     * Create the dataset creation property list, add the Scale-Offset
     * filter, set the chunk size, and set the fill value.
     */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR
    if(H5Pset_filter(dcpl, H5Z_FILTER_FLETCHER32, 0, (size_t)0, NULL) < 0)
        TEST_ERROR
    if(H5Pset_chunk(dcpl, RANK, chunk) < 0)
        TEST_ERROR
    if(H5Pset_fill_value(dcpl, H5T_NATIVE_FLOAT, &fillvalue) < 0)
        TEST_ERROR

    /*
     * Create a new dataset within the file using defined dataspace, little
     * endian datatype and default dataset creation properties.
     */
    if((dataset = H5Dcreate2(fid, DATASETNAME14, H5T_IEEE_F32LE, fsid,
            H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /*
     * Write the data to the dataset using default transfer properties.
     */
    if(H5Dwrite(dataset, H5T_NATIVE_FLOAT, msid, fsid, H5P_DEFAULT, data) < 0)
        TEST_ERROR

    /* Close dataset */
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /* Now create a dataset with a big-endian type */
    if((dataset = H5Dcreate2(fid, DATASETNAME15, H5T_IEEE_F32BE, fsid,
            H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Dwrite(dataset, H5T_NATIVE_FLOAT, msid, fsid, H5P_DEFAULT, data) < 0)
        TEST_ERROR
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /*
     * Close/release resources.
     */
    if(H5Pclose(dcpl) < 0)
        TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Dclose(dataset);
    } H5E_END_TRY;

    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    create_deflate_dsets_float
 *
 * Purpose:     Create a dataset of FLOAT datatype with deflate filter
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              29 March 2011
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
create_deflate_dsets_float(hid_t fid, hid_t fsid, hid_t msid)
{
#ifdef H5_HAVE_FILTER_DEFLATE
    hid_t       dataset         = -1;   /* dataset handles */
    hid_t       dcpl            = -1;
    float       data[NX][NY];           /* data to write */
    float       fillvalue = -2.2f;
    hsize_t     chunk[RANK] = {CHUNK0, CHUNK1};
    int         i, j;

    /*
     * Data and output buffer initialization.
     */
    for (j = 0; j < NX; j++) {
        for (i = 0; i < NY; i++)
            data[j][i] = ((float)(i + j + 1))/3;
    }

    /*
     * Create the dataset creation property list, add the Scale-Offset
     * filter, set the chunk size, and set the fill value.
     */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR
    if(H5Pset_deflate (dcpl, 6) < 0)
        TEST_ERROR
    if(H5Pset_chunk(dcpl, RANK, chunk) < 0)
        TEST_ERROR
    if(H5Pset_fill_value(dcpl, H5T_NATIVE_FLOAT, &fillvalue) < 0)
        TEST_ERROR

    /*
     * Create a new dataset within the file using defined dataspace, little
     * endian datatype and default dataset creation properties.
     */
    if((dataset = H5Dcreate2(fid, DATASETNAME16, H5T_IEEE_F32LE, fsid,
            H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /*
     * Write the data to the dataset using default transfer properties.
     */
    if(H5Dwrite(dataset, H5T_NATIVE_FLOAT, msid, fsid, H5P_DEFAULT, data) < 0)
        TEST_ERROR

    /* Close dataset */
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /* Now create a dataset with a big-endian type */
    if((dataset = H5Dcreate2(fid, DATASETNAME17, H5T_IEEE_F32BE, fsid,
            H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Dwrite(dataset, H5T_NATIVE_FLOAT, msid, fsid, H5P_DEFAULT, data) < 0)
        TEST_ERROR
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /*
     * Close/release resources.
     */
    if(H5Pclose(dcpl) < 0)
        TEST_ERROR

#else /* H5_HAVE_FILTER_DEFLATE */
    const char          *not_supported= "Deflate filter is not enabled. Can't create the dataset.";

    puts(not_supported);
#endif /* H5_HAVE_FILTER_DEFLATE */

    return 0;

#ifdef H5_HAVE_FILTER_DEFLATE
error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Dclose(dataset);
    } H5E_END_TRY;

    return -1;
#endif /* H5_HAVE_FILTER_DEFLATE */
}

#ifdef H5_HAVE_FILTER_SZIP

/*-------------------------------------------------------------------------
 * Function:    create_szip_dsets_float
 *
 * Purpose:     Create a dataset of FLOAT datatype with szip filter
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              29 March 2011
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
create_szip_dsets_float(hid_t fid, hid_t fsid, hid_t msid)
{
    hid_t       dataset;         /* dataset handles */
    hid_t       dcpl;
    float       data[NX][NY];          /* data to write */
    float       fillvalue = -2.2f;
    hsize_t     chunk[RANK] = {CHUNK0, CHUNK1};
    int         i, j;

    /*
     * Data and output buffer initialization.
     */
    for (j = 0; j < NX; j++) {
        for (i = 0; i < NY; i++)
            data[j][i] = ((float)(i + j + 1))/3;
    }

    /*
     * Create the dataset creation property list, add the Scale-Offset
     * filter, set the chunk size, and set the fill value.
     */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR
    if(H5Pset_szip(dcpl, H5_SZIP_NN_OPTION_MASK, 4) < 0)
        TEST_ERROR
    if(H5Pset_chunk(dcpl, RANK, chunk) < 0)
        TEST_ERROR
    if(H5Pset_fill_value(dcpl, H5T_NATIVE_FLOAT, &fillvalue) < 0)
        TEST_ERROR

    /*
     * Create a new dataset within the file using defined dataspace, little
     * endian datatype and default dataset creation properties.
     */
    if((dataset = H5Dcreate2(fid, DATASETNAME18, H5T_IEEE_F32LE, fsid,
            H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /*
     * Write the data to the dataset using default transfer properties.
     */
    if(H5Dwrite(dataset, H5T_NATIVE_FLOAT, msid, fsid, H5P_DEFAULT, data) < 0)
        TEST_ERROR

    /* Close dataset */
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /* Now create a dataset with a big-endian type */
    if((dataset = H5Dcreate2(fid, DATASETNAME19, H5T_IEEE_F32BE, fsid,
            H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Dwrite(dataset, H5T_NATIVE_FLOAT, msid, fsid, H5P_DEFAULT, data) < 0)
        TEST_ERROR
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /*
     * Close/release resources.
     */
    if(H5Pclose(dcpl) < 0)
        TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Dclose(dataset);
    } H5E_END_TRY;

    return -1;
}
#endif /* H5_HAVE_FILTER_SZIP */


/*-------------------------------------------------------------------------
 * Function:    create_shuffle_dsets_float
 *
 * Purpose:     Create a dataset of FLOAT datatype with shuffle filter
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              29 March 2011
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
create_shuffle_dsets_float(hid_t fid, hid_t fsid, hid_t msid)
{
    hid_t       dataset         = -1;   /* dataset handles */
    hid_t       dcpl            = -1;
    float       data[NX][NY];           /* data to write */
    float       fillvalue = -2.2f;
    hsize_t     chunk[RANK] = {CHUNK0, CHUNK1};
    int         i, j;

    /*
     * Data and output buffer initialization.
     */
    for (j = 0; j < NX; j++) {
        for (i = 0; i < NY; i++)
            data[j][i] = ((float)(i + j + 1))/3;
    }

    /*
     * Create the dataset creation property list, add the Scale-Offset
     * filter, set the chunk size, and set the fill value.
     */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR
    if(H5Pset_shuffle (dcpl) < 0)
        TEST_ERROR
    if(H5Pset_chunk(dcpl, RANK, chunk) < 0)
        TEST_ERROR
    if(H5Pset_fill_value(dcpl, H5T_NATIVE_FLOAT, &fillvalue) < 0)
        TEST_ERROR

    /*
     * Create a new dataset within the file using defined dataspace, little
     * endian datatype and default dataset creation properties.
     */
    if((dataset = H5Dcreate2(fid, DATASETNAME20, H5T_IEEE_F32LE, fsid,
            H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /*
     * Write the data to the dataset using default transfer properties.
     */
    if(H5Dwrite(dataset, H5T_NATIVE_FLOAT, msid, fsid, H5P_DEFAULT, data) < 0)
        TEST_ERROR

    /* Close dataset */
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /* Now create a dataset with a big-endian type */
    if((dataset = H5Dcreate2(fid, DATASETNAME21, H5T_IEEE_F32BE, fsid,
            H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Dwrite(dataset, H5T_NATIVE_FLOAT, msid, fsid, H5P_DEFAULT, data) < 0)
        TEST_ERROR
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /*
     * Close/release resources.
     */
    if(H5Pclose(dcpl) < 0)
        TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Dclose(dataset);
    } H5E_END_TRY;

    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    create_nbit_dsets_float
 *
 * Purpose:     Create a dataset of FLOAT datatype with nbit filter
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              29 March 2011
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
create_nbit_dsets_float(hid_t fid, hid_t fsid, hid_t msid)
{
    hid_t       dataset         = -1;   /* dataset handles */
    hid_t       datatype        = -1;
    hid_t       dcpl            = -1;
    size_t      precision, offset;
    float       data[NX][NY];           /* data to write */
    float       fillvalue = -2.2f;
    hsize_t     chunk[RANK] = {CHUNK0, CHUNK1};
    int         i, j;

    /*
     * Data and output buffer initialization.
     */
    for (j = 0; j < NX; j++) {
        for (i = 0; i < NY; i++)
            data[j][i] = ((float)(i + j + 1))/3;
    }

    /*
     * Create the dataset creation property list, add the Scale-Offset
     * filter, set the chunk size, and set the fill value.
     */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR
    if(H5Pset_nbit(dcpl) < 0)
        TEST_ERROR
    if(H5Pset_chunk(dcpl, RANK, chunk) < 0)
        TEST_ERROR
    if(H5Pset_fill_value(dcpl, H5T_NATIVE_FLOAT, &fillvalue) < 0)
        TEST_ERROR

    /* Define user-defined single-precision floating-point type for dataset.
     * A 20-bit little-endian data type. */
    if((datatype = H5Tcopy(H5T_IEEE_F32LE)) < 0)
        TEST_ERROR
    if(H5Tset_fields(datatype, (size_t)26, (size_t)20, (size_t)6, (size_t)7, (size_t)13) < 0)
        TEST_ERROR
    offset = 7;
    if(H5Tset_offset(datatype,offset) < 0)
        TEST_ERROR
    precision = 20;
    if(H5Tset_precision(datatype,precision) < 0)
        TEST_ERROR
    if(H5Tset_size(datatype, (size_t)4) < 0)
        TEST_ERROR
    if(H5Tset_ebias(datatype, (size_t)31) < 0)
        TEST_ERROR

    /*
     * Create a new dataset within the file using defined dataspace,
     * user-defined datatype, and default dataset creation properties.
     */
    if((dataset = H5Dcreate2(fid, DATASETNAME22, datatype, fsid,
            H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /*
     * Write the data to the dataset using default transfer properties.
     */
    if(H5Dwrite(dataset, H5T_NATIVE_FLOAT, msid, fsid, H5P_DEFAULT, data) < 0)
        TEST_ERROR

    /* Close dataset */
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /* Now create a dataset with a big-endian type */
    if(H5Tset_order(datatype, H5T_ORDER_BE) < 0) 
        TEST_ERROR
    if((dataset = H5Dcreate2(fid, DATASETNAME23, datatype, fsid,
            H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Dwrite(dataset, H5T_NATIVE_FLOAT, msid, fsid, H5P_DEFAULT, data) < 0)
        TEST_ERROR
    if(H5Dclose(dataset) < 0)
        TEST_ERROR

    /*
     * Close/release resources.
     */
    if(H5Pclose(dcpl) < 0)
        TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Dclose(dataset);
    } H5E_END_TRY;

    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Create a file for cross_read.c test.
 *
 * Return:      Success:        exit(EXIT_SUCCESS)
 *              Failure:        exit(EXIT_FAILURE)
 *
 * Programmer:  Raymond Lu
 *              Some time ago
 *
 *-------------------------------------------------------------------------
 */
int
main (void)
{
    hid_t       file = -1;
    hid_t       filespace = -1;
    hid_t       memspace = -1;
    hsize_t     dimsf[RANK];
    hsize_t     start[RANK] = {0, 0};

    /*
     * Create a new file using H5F_ACC_TRUNC access,
     * default file creation properties, and default file
     * access properties.
     */
    if((file = H5Fcreate(H5FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT))
            < 0)
        {H5_FAILED(); AT(); return 1;}

    /*
     * Describe the size of the array and create the data space for fixed
     * size dataset.  Increase the size in the X direction to have some fill
     * values.
     */
    dimsf[0] = NX + 1;
    dimsf[1] = NY;
    if((filespace = H5Screate_simple(RANK, dimsf, NULL)) < 0)
        {H5_FAILED(); AT(); return 1;}
    dimsf[0] = NX;
    if(H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, NULL, dimsf, NULL)
            < 0)
        {H5_FAILED(); AT(); return 1;}

    /* Create memory space.  This does not include the extra row for fill
     * values. */
    HDassert(dimsf[0] == NX);
    HDassert(dimsf[1] == NY);
    if((memspace = H5Screate_simple(RANK, dimsf, NULL)) < 0)
        {H5_FAILED(); AT(); return 1;}

    /* Create a regular dataset */
    if(create_normal_dset(file, filespace, memspace) < 0)
        {H5_FAILED(); AT(); return 1;}

    /* Create a dataset of FLOAT with scale-offset filter */
    if(create_scale_offset_dsets_float(file, filespace, memspace) < 0)
        {H5_FAILED(); AT(); return 1;}

    /* Create a dataset of DOUBLE with scale-offset filter */
    if(create_scale_offset_dsets_double(file, filespace, memspace) < 0)
        {H5_FAILED(); AT(); return 1;}

    /* Create a dataset of CHAR with scale-offset filter */
    if(create_scale_offset_dsets_char(file, filespace, memspace) < 0)
        {H5_FAILED(); AT(); return 1;}

    /* Create a dataset of SHORT with scale-offset filter */
    if(create_scale_offset_dsets_short(file, filespace, memspace) < 0)
        {H5_FAILED(); AT(); return 1;}

    /* Create a dataset of INT with scale-offset filter */
    if(create_scale_offset_dsets_int(file, filespace, memspace) < 0)
        {H5_FAILED(); AT(); return 1;}

    /* Create a dataset of LONG LONG with scale-offset filter */
    if(create_scale_offset_dsets_long_long(file, filespace, memspace) < 0)
        {H5_FAILED(); AT(); return 1;}

    /* Create a dataset of FLOAT with fletcher filter */
    if(create_fletcher_dsets_float(file, filespace, memspace) < 0)
        {H5_FAILED(); AT(); return 1;}

    /* Create a dataset of FLOAT with deflate filter */
    if(create_deflate_dsets_float(file, filespace, memspace) < 0)
        {H5_FAILED(); AT(); return 1;}

#ifdef H5_HAVE_FILTER_SZIP
    /* Create a dataset of FLOAT with szip filter */
    if(create_szip_dsets_float(file, filespace, memspace) < 0)
        {H5_FAILED(); AT(); return 1;}
#else /* H5_HAVE_FILTER_SZIP */
    puts("Szip filter is not enabled. Can't create the dataset.");
#endif /* H5_HAVE_FILTER_SZIP */

    /* Create a dataset of FLOAT with shuffle filter */
    if(create_shuffle_dsets_float(file, filespace, memspace) < 0)
        {H5_FAILED(); AT(); return 1;}

    /* Create a dataset of FLOAT with nbit filter */
    if(create_nbit_dsets_float(file, filespace, memspace) < 0)
        {H5_FAILED(); AT(); return 1;}


    /*
     * Close/release resources.
     */
    if(H5Sclose(memspace) < 0)
        {H5_FAILED(); AT(); return 1;}
    if(H5Sclose(filespace) < 0)
        {H5_FAILED(); AT(); return 1;}
    if(H5Fclose(file) < 0)
        {H5_FAILED(); AT(); return 1;}

    return 0;
}

