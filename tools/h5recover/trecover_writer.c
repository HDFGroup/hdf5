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

int
create_files(const char *filename, const char *ctl_filename)
{
    /*
     * Create a new file and the control file using H5F_ACC_TRUNC access,
     * default file creation properties, and latest lib version file
     * access properties.
     */
    hid_t       faccpl;         /* file access property list handle */

    faccpl = H5Pcreate(H5P_FILE_ACCESS);
    if (H5Pset_libver_bounds(faccpl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0){
	fprintf(stderr, "H5Pset_libver_bounds on data file failed\n");
	H5Pclose(faccpl);
	return(-1);
    }
    /* remove any existing test files. */
    remove(filename);
    remove(ctl_filename);
    datafile = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, faccpl);
    ctl_file = H5Fcreate(ctl_filename, H5F_ACC_TRUNC, H5P_DEFAULT, faccpl);

    return(0);
}

int
journal_files(const char *filename, const char *ctl_filename, const char *jnl_filename, int patch)
{
    /*
     * Create a new file and the control file using H5F_ACC_TRUNC access,
     * default file creation properties, and latest lib version file
     * access properties.
     */
    hid_t       faccpl;         /* file access property list handle */
    H5AC2_jnl_config_t jnl_config;

    faccpl = H5Pcreate(H5P_FILE_ACCESS);
    /* Turn journaling on if not patch mode */
    if (!patch){

    /* set latest format */
    if ( H5Pset_libver_bounds(faccpl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST ) 
         < 0 ) {
        fprintf(stderr, "H5Pset_libver_bounds on data file failed\n");
        H5Pclose(faccpl);
        return(-1);
    }

    /* get current journaling configuration */
    jnl_config.version = H5AC2__CURR_JNL_CONFIG_VER;

    if ( H5Pget_jnl_config(faccpl, &jnl_config) < 0 ) {
        fprintf(stderr, "H5Pget_jnl_config on faccpl failed\n");
        H5Pclose(faccpl);
        return(-1);
    }

    jnl_config.enable_journaling = 1;     /* turn on journaling */
    jnl_config.journal_recovered = 0;
    jnl_config.jbrb_buf_size = 8*1024;    /* multiples of sys buffer size*/
    jnl_config.jbrb_num_bufs = 2;
    jnl_config.jbrb_use_aio = 0;          /* only sync IO is supported */
    jnl_config.jbrb_human_readable = 1;   /* only readable form is supported */
    strcpy(jnl_config.journal_file_path, jnl_filename);

    if ( H5Pset_jnl_config(faccpl, &jnl_config) < 0 ) {
        fprintf(stderr, "H5Pset_jnl_config on faccpl failed\n");
        H5Pclose(faccpl);
        return(-1);
    }

	/* remove any existing journal file. */
	remove(jnl_filename);
    }
    datafile = H5Fopen(filename, H5F_ACC_RDWR, faccpl);
    ctl_file = H5Fopen(ctl_filename, H5F_ACC_RDWR, H5P_DEFAULT);
    H5Pclose(faccpl);

    return(0);
}

int
close_file(hid_t fid)
{
    H5Fclose(fid);

    return(0);
}

void
create_dataset(hid_t f, int dstype, int rank, hsize_t *dims, hsize_t *dimschunk)
{
    hid_t       dataset;         /* dataset handle */
    hid_t       dataspace, plist;      /* handles */
    herr_t      status;                             
    hsize_t 	maxdims[RANK]; 		/* Dataset dimensions */

    /* Default to create an unlimited dimension dataset unless contigous
     * storeage is used.
     */
    if (dstype & DSContig)
	dataspace = H5Screate_simple(rank, dims, NULL); 
    else{
	maxdims[0]=H5S_UNLIMITED;
	maxdims[1]=NY;
	dataspace = H5Screate_simple(rank, dims, maxdims); 
    }
    /*
     * Describe the size of the array and create the data space for fixed
     * size dataset. 
     */

    if (dstype & DSContig) {
    /* =============================================================
     * Create a new dataset within the file using defined dataspace and
     * datatype and default dataset creation properties.
     * =============================================================
     */
    dataset = H5Dcreate2(f, DATASETNAME, H5T_STD_I32LE, dataspace,
			H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Write the data to the dataset using default transfer properties.
     */
    status = writedata(dataset, 0, NX-1);

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
                H5Pset_chunk(plist, rank, dimschunk);
    dataset = H5Dcreate2(f, CHUNKDATASETNAME, H5T_STD_I32LE,
                        dataspace, H5P_DEFAULT, plist, H5P_DEFAULT);
    /*
     * Write the data to the dataset using default transfer properties.
     */
    status = writedata(dataset, 0, NX-1);
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
                H5Pset_chunk(plist, rank, dimschunk);
                H5Pset_deflate( plist, 6);
    dataset = H5Dcreate2(f, ZDATASETNAME, H5T_STD_I32LE,
                        dataspace, H5P_DEFAULT, plist, H5P_DEFAULT);
    status = writedata(dataset, 0, NX-1);


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
                H5Pset_chunk(plist, rank, dimschunk);
		H5Pset_szip (plist, H5_SZIP_NN_OPTION_MASK, 8);
    dataset = H5Dcreate2(f, SZDATASETNAME, H5T_STD_I32LE,
                        dataspace, H5P_DEFAULT, plist, H5P_DEFAULT);
    status = writedata(dataset, 0, NX-1);


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


/* extend the dataset size to end rows.  If it is not patch,
 * write data from begin to end rows. (begin and end are 0 based).
 */
int
extend_dataset(hid_t f, int begin, int end, int patch)
{
    hid_t       dataset;         /* dataset handle */
    hid_t       dataspace;      /* handles */
    hsize_t 	currdims[RANK]; 	/* Dataset current dimensions */

    /* argument checks */
    if ((end < begin) || (begin < 0)){
	fprintf(stderr, "writedata: bad arguments (begin=%d, end=%d)\n", begin, end);
	return(-1);
    }

    dataset=H5Dopen2(f, dsetname, H5P_DEFAULT);
    /* get dimensions */
    if ( (dataspace = H5Dget_space(dataset)) < 0 )
        return(-1);
    if ( H5Sget_simple_extent_dims(dataspace,currdims,NULL) < 0 )
        return(-1);

    /* extend the dataset if needed. */
    if (end >= currdims[0]){
	currdims[0] = end+1;
	if (H5Dset_extent(dataset, currdims)<0)
	    return(-1);
    }
    if (!patch)
	writedata(dataset, begin, end);
    H5Dclose(dataset);
    return(0);
}


/* writedata():
 * write rows of data to dataset starting from begin to end rows inclusive.
 */
int
writedata(hid_t dataset, int begin, int end)
{
    int         data[NX][NY];          /* data to write */
    int         nrows, i, j;
    hid_t	memspace, dataspace;
    hsize_t	dims[RANK], start[RANK], count[RANK];
    int		beginInc;

    /* Argument values check */
    if ((end < begin) || (begin < 0)){
	fprintf(stderr, "writedata: bad arguments (begin=%d, end=%d)\n", begin, end);
	return(-1);
    }

    dims[0]=NX;
    dims[1]=NY;
    memspace = H5Screate_simple(RANK, dims, NULL);
    dataspace = H5Dget_space(dataset);

    /* write data NX rows at a time. */
    beginInc = begin;
    nrows = NX;
    while (beginInc <= end){
	if (beginInc+nrows-1 > end)
	    nrows=end-beginInc+1;	/* last fragment of rows */
	
	start[0]=beginInc;
	start[1]=0;
	count[0]=nrows;
	count[1]=NY;
	if (H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, start,
			NULL, count, NULL) <0){
	    fprintf(stderr, "H5Sselect_hyperslab failed\n");
	    return(-1);
	};

	/* Initialize data buffer */
	for(i = 0; i < nrows; i++)
	    for(j = 0; j < NY; j++)
		data[i][j] = (i+beginInc)*NY + j;

	/*
	 * Write the data to the dataset using default transfer properties.
	 */
	if (H5Dwrite(dataset, H5T_NATIVE_INT, memspace, dataspace, H5P_DEFAULT,
		    data)<0)
	    return(-1);
	beginInc += nrows;
    }
    H5Sclose(memspace);
    H5Sclose(dataspace);
    return(0);
}
