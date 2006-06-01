/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* 
 * This verifies if the storage space allocation methods are compatible between
 * serial and parallel modes.
 *
 * Created by: Christian Chilan and Albert Cheng
 * Date: 2006/05/25
 */

#include "testphdf5.h"
static int	mpi_size, mpi_rank;

#define DATASETNAME "ExtendibleArray" 
#define CHUNKSIZE	1000		/* #elements per chunk */
#define DSETCHUNKS		20000
#define CLOSE           1
#define NO_CLOSE        0

static MPI_Offset
get_filesize(const char *filename)
{
    int		mpierr;
    MPI_File	fd;
    MPI_Offset	filesize;

    mpierr = MPI_File_open(MPI_COMM_SELF, (char*)filename, MPI_MODE_RDONLY,
	MPI_INFO_NULL, &fd);
    VRFY((mpierr == MPI_SUCCESS), "");

    mpierr = MPI_File_get_size(fd, &filesize);
    VRFY((mpierr == MPI_SUCCESS), "");

    mpierr = MPI_File_close(&fd);
    VRFY((mpierr == MPI_SUCCESS), "");

    return(filesize);
}

typedef enum write_ {
    none,
    sec_last,
    all
} write_type;

typedef enum access_ {
    write_all,
    open_only,
    extend_only
} access_type;


/*
 * This creates a dataset serially with 'nchunks' chunks, each of CHUNKSIZE
 * elements. The allocation time is set to H5D_ALLOC_TIME_EARLY. Another
 * routine will open this in parallel for extension test.
 */
void
create_chunked_dataset(const char *filename, int nchunks, write_type write)
{
    hid_t       file_id, dataset;                          /* handles */
    hid_t       dataspace,memspace;  
    hid_t       cparms;
    hsize_t      dims[1];
    hsize_t      maxdims[1] = {H5S_UNLIMITED};
    
    hsize_t      chunk_dims[1] ={CHUNKSIZE};
    hsize_t     count[1];
    hsize_t     stride[1];
    hsize_t     block[1];
    hsize_t     offset[1];            /* Selection offset within dataspace */
    /* Variables used in reading data back */
    char         buffer[CHUNKSIZE];
    int           i;

    herr_t       hrc;                             

    MPI_Offset  filesize,	    /* actual file size */
		est_filesize;	    /* estimated file size */

    /* set up MPI parameters */
    MPI_Comm_size(MPI_COMM_WORLD,&mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);

    /* Only MAINPROCESS should create the file.  Others just wait. */
    if (MAINPROCESS){

	dims[0]=nchunks*CHUNKSIZE;
	/* Create the data space with unlimited dimensions. */
	dataspace = H5Screate_simple (1, dims, maxdims); 
	VRFY((dataspace >= 0), "");

	memspace = H5Screate_simple(1, chunk_dims, NULL);
	VRFY((memspace >= 0), "");
 
	/* Create a new file. If file exists its contents will be overwritten. */
	file_id = H5Fcreate(h5_rmprefix(filename), H5F_ACC_TRUNC, H5P_DEFAULT,
		    H5P_DEFAULT);
	VRFY((file_id >= 0), "H5Fcreate");

	/* Modify dataset creation properties, i.e. enable chunking  */
	cparms = H5Pcreate (H5P_DATASET_CREATE);
	VRFY((cparms >= 0), "");

	hrc = H5Pset_alloc_time(cparms, H5D_ALLOC_TIME_EARLY);
	VRFY((hrc >= 0), "");

	hrc = H5Pset_chunk ( cparms, 1, chunk_dims);
	VRFY((hrc >= 0), "");

	/* Create a new dataset within the file using cparms creation properties. */
	dataset = H5Dcreate (file_id, DATASETNAME, H5T_NATIVE_UCHAR, dataspace, cparms);
	VRFY((dataset >= 0), "");

	switch (write) {

	    /* writes only the second to last chunk */
	    case sec_last:

		memset(buffer, 100, CHUNKSIZE);

		count[0] = 1;
		stride[0] = 1;
		block[0] = chunk_dims[0];
		offset[0] = (nchunks-2)*chunk_dims[0];

		hrc = H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, stride, count, block);
		    VRFY((hrc >= 0), "");

		/* Write sec_last chunk */
		hrc = H5Dwrite(dataset, H5T_NATIVE_UCHAR, memspace, dataspace, H5P_DEFAULT, buffer);
		VRFY((hrc >= 0), "H5Dwrite");

		break;


	    /* doesn't write anything */
	    case none:

		break;
	}

	/* Close resources */
	hrc = H5Dclose (dataset);
	VRFY((hrc >= 0), "");
	dataset = -1;

	hrc = H5Sclose (dataspace);
	VRFY((hrc >= 0), "");

	hrc = H5Sclose (memspace);
	VRFY((hrc >= 0), "");

	hrc = H5Pclose (cparms);
	VRFY((hrc >= 0), "");

	hrc = H5Fclose (file_id);
	VRFY((hrc >= 0), "");
	file_id = -1;

	/* verify file size */
	filesize = get_filesize(filename);
	est_filesize = nchunks*CHUNKSIZE*sizeof(unsigned char);
	VRFY((filesize >= est_filesize), "file size check");

    }

    /* Make sure all processes are done before exiting this routine.  Otherwise,
     * other tests may start and change the test data file before some processes
     * of this test are still accessing the file.
     */

    MPI_Barrier(MPI_COMM_WORLD);
}     


/*
 * This program performs three different types of parallel access. It writes on
 * the entire dataset, it extends the dataset to nchunks*CHUNKSIZE, and it only
 * opens the dataset. At the end, it verifies the size of the dataset to be
 * consistent with argument 'nchunks'.
 */
void
parallel_access_dataset(const char *filename, int nchunks, access_type action, hid_t *file_id, hid_t *dataset)
{
    /* HDF5 gubbins */
    hid_t    memspace, dataspace;     /* HDF5 file identifier */
    hid_t    access_plist;         /* HDF5 ID for file access property list */
    herr_t   hrc;                  /* HDF5 return code */
    hsize_t  size[1];
    hsize_t  dim_size;

    hsize_t     chunk_dims[1] ={CHUNKSIZE};
    hsize_t     count[1];
    hsize_t     stride[1];
    hsize_t     block[1];
    hsize_t     offset[1];            /* Selection offset within dataspace */
    /* Variables used in reading data back */
    char         buffer[CHUNKSIZE];
    int         i;

    /* MPI Gubbins */
    MPI_Offset  filesize,	    /* actual file size */
		est_filesize;	    /* estimated file size */
    int         mpierr;

    /* Initialize MPI */
    MPI_Comm_size(MPI_COMM_WORLD,&mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);

    /* Set up MPIO file access property lists */
    access_plist  = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((access_plist >= 0), "");

    hrc = H5Pset_fapl_mpio(access_plist, MPI_COMM_WORLD, MPI_INFO_NULL);
    VRFY((hrc >= 0), "");

    /* Open the file */
    if (*file_id<0){
        *file_id = H5Fopen(filename, H5F_ACC_RDWR, access_plist);
        VRFY((*file_id >= 0), "");
    }

    /* Open dataset*/
    if (*dataset<0){
        *dataset = H5Dopen(*file_id, DATASETNAME);
        VRFY((*dataset >= 0), "");
    }

    memspace = H5Screate_simple(1, chunk_dims, NULL);
    VRFY((memspace >= 0), "");

    dataspace = H5Dget_space(*dataset);            
    VRFY((dataspace >= 0), "");

    size[0] = nchunks*CHUNKSIZE;

    switch (action) {

        /* all chunks are written by all the processes in an interleaved way*/
        case write_all:

            for (i=0; i<nchunks/mpi_size; i++){ 

                memset(buffer, mpi_rank+1, CHUNKSIZE);

                offset[0] = (i*mpi_size+mpi_rank)*chunk_dims[0];
                count[0] = 1;
                stride[0] = 1;
                block[0] = chunk_dims[0];

                hrc = H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, stride, count, block);
                VRFY((hrc >= 0), "");

                /* Write the buffer out */
                hrc = H5Dwrite(*dataset, H5T_NATIVE_UCHAR, memspace, dataspace, H5P_DEFAULT, buffer);
                VRFY((hrc >= 0), "H5Dwrite");

            }

            /* remainder writing */
            if (mpi_rank < nchunks%mpi_size){

                memset(buffer, mpi_rank+1, CHUNKSIZE);

                offset[0] = ((nchunks/mpi_size)*mpi_size+mpi_rank)*chunk_dims[0];
                count[0] = 1;
                stride[0] = 1;
                block[0] = chunk_dims[0];

                hrc = H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, stride, count, block);
                VRFY((hrc >= 0), "");

                /* Write the buffer out */
                hrc = H5Dwrite(*dataset, H5T_NATIVE_UCHAR, memspace, dataspace, H5P_DEFAULT, buffer);
                VRFY((hrc >= 0), "H5Dwrite");
            }

            break;

        /* only extends the dataset */
        case extend_only:
            /* Extend dataset*/
            hrc = H5Dextend(*dataset, size);
            VRFY((hrc >= 0), "");

            break;

        /* only opens the dataset */
        case open_only:

            break;
    }

    /* Close up */
    hrc = H5Dclose(*dataset);
    VRFY((hrc >= 0), "");
    *dataset = -1;

    hrc = H5Sclose (dataspace);
	VRFY((hrc >= 0), "");

	hrc = H5Sclose (memspace);
	VRFY((hrc >= 0), "");

    hrc = H5Fclose(*file_id);
    VRFY((hrc >= 0), "");
    *file_id = -1;

    /* verify file size */
    filesize = get_filesize(filename);
    est_filesize = nchunks*CHUNKSIZE*sizeof(unsigned char);
    VRFY((filesize >= est_filesize), "file size check");

    /* Can close some plists */
    hrc = H5Pclose(access_plist);
    VRFY((hrc >= 0), "");

    /* Make sure all processes are done before exiting this routine.  Otherwise,
     * other tests may start and change the test data file before some processes
     * of this test are still accessing the file.
     */
    MPI_Barrier(MPI_COMM_WORLD);
}

/*
 * This routine verifies the data written in the dataset. It does one of the
 * three cases according to the value of parameter `write'.
 * 1. it returns correct fill values though the dataset has not been written;
 * 2. it still returns correct fill values though only a small part is written;
 * 3. it returns correct values when the whole dataset has been written in an
 *    interleaved pattern.
 */
void verify_data(const char *filename, int nchunks, write_type write, int close, hid_t *file_id, hid_t *dataset)
{
    /* HDF5 gubbins */
    hid_t    dataspace, memspace;     /* HDF5 file identifier */
    hid_t    access_plist;         /* HDF5 ID for file access property list */
    herr_t   hrc;                  /* HDF5 return code */

    hsize_t     chunk_dims[1] ={CHUNKSIZE};
    hsize_t     count[1];
    hsize_t     stride[1];
    hsize_t     block[1];
    hsize_t     offset[1];            /* Selection offset within dataspace */
    /* Variables used in reading data back */
    char         buffer[CHUNKSIZE];
    int         value, i;
    int         index, current;

    /* MPI Gubbins */
    MPI_Offset  filesize,	    /* actual file size */
		est_filesize;	    /* estimated file size */
    int         mpierr;

    /* Initialize MPI */
    MPI_Comm_size(MPI_COMM_WORLD,&mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);

    /* Set up MPIO file access property lists */
    access_plist  = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((access_plist >= 0), "");

    hrc = H5Pset_fapl_mpio(access_plist, MPI_COMM_WORLD, MPI_INFO_NULL);
    VRFY((hrc >= 0), "");

    /* Open the file */
    if (*file_id<0){
        *file_id = H5Fopen(filename, H5F_ACC_RDWR, access_plist);
        VRFY((*file_id >= 0), "");
    }

    /* Open dataset*/
    if (*dataset<0){
        *dataset = H5Dopen(*file_id, DATASETNAME);
        VRFY((*dataset >= 0), "");
    }

    memspace = H5Screate_simple(1, chunk_dims, NULL);
    VRFY((memspace >= 0), "");

    dataspace = H5Dget_space(*dataset);            
    VRFY((dataspace >= 0), "");

    /* expected value in the dataset */
    if (write == all)
        value = mpi_rank + 1;
    else
        value =0;

    /* checks main portion of the dataset */
    for (i=0; i<nchunks/mpi_size; i++){ 

        memset(buffer, -1, CHUNKSIZE);

        offset[0] = (i*mpi_size+mpi_rank)*chunk_dims[0];
        count[0] = 1;
        stride[0] = 1;
        block[0] = chunk_dims[0];

        hrc = H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, stride, count, block);
        VRFY((hrc >= 0), "");

        /* Read the chunk */
        hrc = H5Dread(*dataset, H5T_NATIVE_UCHAR, memspace, dataspace, H5P_DEFAULT, buffer);
        VRFY((hrc >= 0), "H5Dread");

        /* adjust expected value for sec_last chunk */
        if (i == nchunks/mpi_size-1 && !(nchunks%mpi_size) && write==sec_last){
            if (mpi_rank == mpi_size-2)
                value = 100;
            else
                value = 0;
        }

        /* verify content of the chunk */
        for (index = 0; index < CHUNKSIZE; index++)
            VRFY((buffer[index] == value), "data verification");
        
    }

    /* remainder checking */
    if (mpi_rank < nchunks%mpi_size){

        memset(buffer, -1, CHUNKSIZE);

        offset[0] = ((nchunks/mpi_size)*mpi_size+mpi_rank)*chunk_dims[0];
        count[0] = 1;
        stride[0] = 1;
        block[0] = chunk_dims[0];

        hrc = H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, stride, count, block);
        VRFY((hrc >= 0), "");

        /* read the buffer out */
        hrc = H5Dread(*dataset, H5T_NATIVE_UCHAR, memspace, dataspace, H5P_DEFAULT, buffer);
        VRFY((hrc >= 0), "H5Dread");

        /* adjust expected value for sec_last chunk */
        if (write == sec_last){
            if (mpi_rank == nchunks%mpi_size-2)
                value = 100;
            else
                value = 0;
        }

        /* verify content of the chunk */
        for (index = 0; index < CHUNKSIZE; index++)
            VRFY((buffer[index] == value), "data verification");
    
    }

    hrc = H5Sclose (dataspace);
	VRFY((hrc >= 0), "");

	hrc = H5Sclose (memspace);
	VRFY((hrc >= 0), "");

    /* Can close some plists */
    hrc = H5Pclose(access_plist);
    VRFY((hrc >= 0), "");

    /* Close up */
    if (close){
        hrc = H5Dclose(*dataset);
        VRFY((hrc >= 0), "");
        *dataset = -1;

        hrc = H5Fclose(*file_id);
        VRFY((hrc >= 0), "");
        *file_id = -1;
    }

    /* Make sure all processes are done before exiting this routine.  Otherwise,
     * other tests may start and change the test data file before some processes
     * of this test are still accessing the file.
     */
    MPI_Barrier(MPI_COMM_WORLD);
}



/*
 * Test following possible scenarios,
 * Case 1:
 * Sequential create a file and dataset with H5D_ALLOC_TIME_EARLY and large
 * size, no write, close, reopen in parallel, read to verify all return
 * the fill value.
 * Case 2:
 * Sequential create a file and dataset with H5D_ALLOC_TIME_EARLY but small
 * size, no write, close, reopen in parallel, extend to large size, then close,
 * then reopen in parallel and read to verify all return the fill value.
 * Case 3:
 * Sequential create a file and dataset with H5D_ALLOC_TIME_EARLY and large
 * size, write just a small part of the dataset (second to the last), close,
 * then reopen in parallel, read to verify all return the fill value except
 * those small portion that has been written.  Without closing it, writes
 * all parts of the dataset in a interleave pattern, close it, and reopen
 * it, read to verify all data are as written.
 */
void
test_chunk_alloc(void)
{
    const char *filename;
    hid_t file_id, dataset;

    file_id = dataset = -1;
  
    filename = GetTestParameters();
    if (VERBOSE_MED)
	printf("Extend Chunked allocation test on file %s\n", filename);

    /* Case 1 */
    /* Create chunked dataset without writing anything.*/
    create_chunked_dataset(filename, DSETCHUNKS, none);
    /* reopen dataset in parallel and check for file size */
    parallel_access_dataset(filename, DSETCHUNKS, open_only, &file_id, &dataset);
    /* reopen dataset in parallel, read and verify the data */
    verify_data(filename, DSETCHUNKS, none, CLOSE, &file_id, &dataset); 

    /* Case 2 */
    /* Create chunked dataset without writing anything */
    create_chunked_dataset(filename, 20, none);
    /* reopen dataset in parallel and only extend it */
    parallel_access_dataset(filename, DSETCHUNKS, extend_only, &file_id, &dataset);
    /* reopen dataset in parallel, read and verify the data */
    verify_data(filename, DSETCHUNKS, none, CLOSE, &file_id, &dataset);

    /* Case 3 */
    /* Create chunked dataset and write in the second to last chunk */
    create_chunked_dataset(filename, DSETCHUNKS, sec_last);
    /* Reopen dataset in parallel, read and verify the data. The file and dataset are not closed*/
    verify_data(filename, DSETCHUNKS, sec_last, NO_CLOSE, &file_id, &dataset); 
    /* All processes write in all the chunks in a interleaved way */
    parallel_access_dataset(filename, DSETCHUNKS, write_all, &file_id, &dataset);
    /* reopen dataset in parallel, read and verify the data */
    verify_data(filename, DSETCHUNKS, all, CLOSE, &file_id, &dataset); 

}
