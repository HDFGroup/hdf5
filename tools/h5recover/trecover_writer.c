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

#define DATASETNAME "IntArray" 
#define CHUNKDATASETNAME "IntArrayChunked" 
#define ZDATASETNAME "IntArrayZCompressed" 
#define SZDATASETNAME "IntArraySZCompressed" 

int
create_files(char *filename, char *ctl_filename, char *jnl_filename)
{
    /*
     * Create a new file and the control file using H5F_ACC_TRUNC access,
     * default file creation properties, and default file
     * access properties but turn on journaling for the regular file.
     */
    hid_t       fapl_id;         /* file access property list handle */

    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    if (H5Pset_journal(fapl_id, jnl_filename) < 0){
	fprintf(stderr, "H5Pset_journal on data file failed\n");
	H5Pclose(fapl_id);
	return(-1);
    }
    /* remove any existing test files. */
    remove(filename);
    remove(ctl_filename);
    remove(jnl_filename);
    file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    ctl_file = H5Fcreate(ctl_filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    return(0);
}

int
close_file(hid_t fid)
{
    H5Fclose(fid);

    return(0);
}

void
writer(hid_t file, int dstype, int rank, hsize_t *dims, hsize_t *dimschunk)
{
    hid_t       dataset;         /* dataset handle */
    hid_t       dataspace, plist;      /* handles */
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
     * Describe the size of the array and create the data space for fixed
     * size dataset. 
     */
    dataspace = H5Screate_simple(RANK, dims, NULL); 

    if (dstype & DSContig) {
    /* =============================================================
     * Create a new dataset within the file using defined dataspace and
     * datatype and default dataset creation properties.
     * =============================================================
     */
    dataset = H5Dcreate2(file, DATASETNAME, H5T_STD_I32LE, dataspace,
			H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Write the data to the dataset using default transfer properties.
     */
    status = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
		      H5P_DEFAULT, data);
    /*
     * Close/release resources.
     */
    H5Dclose(dataset);
    printf("%s created.\n", DATASETNAME);
    }

    if (dstype & DSChunked) {
    /* =============================================================
     * Create a new dataset within the file using defined dataspace and
     * datatype and default dataset creation properties.
     * =============================================================
     */
    plist     = H5Pcreate(H5P_DATASET_CREATE);
                H5Pset_chunk(plist, RANK, dimschunk);
    dataset = H5Dcreate2(file, CHUNKDATASETNAME, H5T_STD_I32LE,
                        dataspace, H5P_DEFAULT, plist, H5P_DEFAULT);
    /*
     * Write the data to the dataset using default transfer properties.
     */
    status = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
		      H5P_DEFAULT, data);
    /*
     * Close/release resources.
     */
    H5Pclose(plist);
    H5Dclose(dataset);
    printf("%s created.\n", CHUNKDATASETNAME);
    }

    if (dstype & DSZip){
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
    dataset = H5Dcreate2(file, ZDATASETNAME, H5T_STD_I32LE,
                        dataspace, H5P_DEFAULT, plist, H5P_DEFAULT);
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
    }

    if (dstype & DSSZip){
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
    dataset = H5Dcreate2(file, SZDATASETNAME, H5T_STD_I32LE,
                        dataspace, H5P_DEFAULT, plist, H5P_DEFAULT);
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
    }



    /* 
     * All done, close/release resources.
     */
    H5Sclose(dataspace);

    return;
}     
