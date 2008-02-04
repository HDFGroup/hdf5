/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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
 * writer HDF5 API module of the trecover test program.
 *
 * Creator: Albert Cheng, Jan 28, 2008.
 */
 
#include "trecover.h"

#define H5FILE_NAME        "SDS.h5"
#define DATASETNAME "IntArray" 
#define ZDATASETNAME "IntArrayZCompressed" 
#define SZDATASETNAME "IntArraySZCompressed" 
/* Dataset dimensions. Intentional small for easier dumping of data. */
#define RANK   2
#define NX     8000                    /* dataset dimensions */
#define NY     16
#define ChunkX 8		    /* Dataset chunk sizes */
#define ChunkY 8

void
writer(void)
{
    hid_t       file, dataset;         /* file and dataset handles */
    hid_t       datatype, dataspace, plist;      /* handles */
    hsize_t     dims[RANK]={NX,NY};              /* dataset dimensions */
    hsize_t     dimschunk[RANK]={ChunkX,ChunkY}; /* dataset chunk dimensions */
    herr_t      status;                             
    int         data[NX][NY];          /* data to write */
    int         i, j;

    /* 
     * Data  and output buffer initialization. 
     */
    for (i = 0; i < NX; i++) {
	for (j = 0; j < NY; j++)
	    data[i][j] = i*100 + j;
    }     

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
    dataspace = H5Screate_simple(RANK, dims, NULL); 

    /* 
     * Define datatype for the data in the file.
     * We will store little endian INT numbers.
     */
    datatype = H5Tcopy(H5T_NATIVE_INT);
    status = H5Tset_order(datatype, H5T_ORDER_LE);

    /* =============================================================
     * Create a new dataset within the file using defined dataspace and
     * datatype and default dataset creation properties.
     * =============================================================
     */
    dataset = H5Dcreate(file, DATASETNAME, datatype, dataspace,
			H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Write the data to the dataset using default transfer properties.
     */
    status = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
		      H5P_DEFAULT, data);
    CRASH;
    /*
     * Close/release resources.
     */
    H5Dclose(dataset);
    printf("%s created.\n", DATASETNAME);

#ifdef H5_HAVE_FILTER_DEFLATE
    /* =============================================================
     * Create similar dataset but using Zlib compression.
     * Dataset creation property list is modified to use
     * GZIP compression with the compression effort set to 6.
     * Note that compression can be used only when dataset is chunked.
     * =============================================================
     */
    plist     = H5Pcreate(H5P_DATASET_CREATE);
                H5Pset_chunk(plist, RANK, dimschunk);
                H5Pset_deflate( plist, 6);
    dataset = H5Dcreate(file, ZDATASETNAME, H5T_NATIVE_INT,
                        dataspace, plist, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
		      H5P_DEFAULT, data);

    /*
     * Close/release resources.
     */
    H5Pclose(plist);
    H5Dclose(dataset);
    printf("%s created.\n", ZDATASETNAME);
#else
    printf("%s is not created because of no GZIP (deflate) support.\n",
	ZDATASETNAME);
#endif

#ifdef H5_HAVE_FILTER_SZIP
    /* =============================================================
     * Create similar dataset but using SZLIB compression.
     * Dataset creation property list is modified to use
     * SZIP compression with 8 pixels_per_block.
     * Note that compression can be used only when dataset is chunked.
     * =============================================================
     */
    plist     = H5Pcreate(H5P_DATASET_CREATE);
                H5Pset_chunk(plist, RANK, dimschunk);
		H5Pset_szip (plist, H5_SZIP_NN_OPTION_MASK, 8);
    dataset = H5Dcreate(file, SZDATASETNAME, H5T_NATIVE_INT,
                        dataspace, plist, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
		      H5P_DEFAULT, data);

    /*
     * Close/release resources.
     */
    H5Pclose(plist);
    H5Dclose(dataset);
    printf("%s created.\n", SZDATASETNAME);
#else
    printf("%s is not created because of no SZIP encode support.\n",
	SZDATASETNAME);
#endif

    /*
     * Close/release resources.
     */
    H5Sclose(dataspace);
    H5Tclose(datatype);

    /* Close the file */
    H5Fclose(file);

    printf("HDF5 C Sample program ran successfully. File %s generated.\n", H5FILE_NAME);
 
    return;
}     
