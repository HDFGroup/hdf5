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
 * Date: 2005/07/05
 */

#include "testphdf5.h"
static int	mpi_size, mpi_rank;

#define DATASETNAME "ExtendibleArray" 
#define CHUNKSIZE	1000		/* #elements per chunk */
#define DSETSIZE		20000*CHUNKSIZE

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

/*
 * This creates a dataset serially with 20000 chunks, each of 1000
 * elements. It does not perform any writing on it. Another routine
 * will open this in parallel for extension test.
 */
void
create_chunked_dataset(const char *filename)
{
    hid_t       file;                          /* handles */
    hid_t       dataspace, dataset;  
    hid_t       cparms;                     
    hsize_t      dims[1]  = {DSETSIZE};     /* dataset dimensions
                                                 at creation time */

    hsize_t      maxdims[1] = {H5S_UNLIMITED};
    herr_t       hrc;                             

    /* Variables used in reading data back */
    hsize_t      chunk_dims[1] ={CHUNKSIZE};

    /* set up MPI parameters */
    MPI_Comm_size(MPI_COMM_WORLD,&mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);

    /* Create the data space with unlimited dimensions. */
    dataspace = H5Screate_simple (1, dims, maxdims); 
    VRFY((dataspace >= 0), "");

    /* Create a new file. If file exists its contents will be overwritten. */
    file = H5Fcreate (filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((file >= 0), "");

    /* Modify dataset creation properties, i.e. enable chunking  */
    cparms = H5Pcreate (H5P_DATASET_CREATE);
    VRFY((cparms >= 0), "");

    hrc = H5Pset_chunk ( cparms, 1, chunk_dims);
    VRFY((hrc >= 0), "");

    /* Create a new dataset within the file using cparms creation properties. */
    dataset = H5Dcreate (file, DATASETNAME, H5T_NATIVE_INT, dataspace, cparms);
    VRFY((dataset >= 0), "");

    /* Close resources */
    hrc = H5Dclose (dataset);
    VRFY((hrc >= 0), "");

    hrc = H5Fclose (file);
    VRFY((hrc >= 0), "");
}     

/*
 * This program extends the size of a small dataset to 20000 chunks (each of 
 * 1000 elements) in parallel. The dataset was initially created in a serial
 * environment. A correct extend operation should increase the file size
 * considerably.
 */
void
extend_chunked_dataset(const char *filename)
{
    /* HDF5 gubbins */
    hid_t    file_id, dataset;     /* HDF5 file identifier */
    hid_t    access_plist;         /* HDF5 ID for file access property list */
    herr_t   hrc;                  /* HDF5 return code */
    hsize_t  size[1];

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
    file_id = H5Fopen(filename, H5F_ACC_RDWR, access_plist);
    VRFY((file_id >= 0), "");

    /* Open dataset*/
    dataset   = H5Dopen(file_id, DATASETNAME);
    VRFY((dataset >= 0), "");

#if 0
    size[0] = DSETSIZE;

    /* Extend dataset*/
    hrc = H5Dextend(dataset, size);
    VRFY((hrc >= 0), "");
#endif

    /* Close up */
    hrc = H5Dclose(dataset);
    VRFY((hrc >= 0), "");

    hrc = H5Fclose(file_id);
    VRFY((hrc >= 0), "");

    /* verify file size has grown */
    filesize = get_filesize(filename);
    est_filesize = DSETSIZE*sizeof(H5T_NATIVE_INT);
    VRFY((filesize>=est_filesize), "file size check");

    /* Can close some plists */
    hrc = H5Pclose(access_plist);
    VRFY((hrc >= 0), "");
}

void
test_chunk_alloc(void)
{
    const char *filename;
  
    filename = GetTestParameters();
    if (VERBOSE_MED)
	printf("Extend Chunked allocation test on file %s\n", filename);

    /* create the datafile first */
    create_chunked_dataset(filename);
    /* reopen it in parallel and extend it. */
    extend_chunked_dataset(filename);
}
