/* $Id$ */

/*
 * Parallel tests for datasets
 */

/*
 * Example of using the parallel HDF5 library to access datasets.
 *
 * This program contains two major parts.  Part 1 tests fixed dimension
 * datasets, for both independent and collective transfer modes.
 * Part 2 tests extendible datasets, for independent transfer mode
 * only.  Collective mode for extendible datasets are not supported yet.
 */

#include <testphdf5.h>

/*
 * The following are various utility routines used by the tests.
 */

/*
 * Setup the dimensions of the hyperslab.
 * Two modes--by rows or by columns.
 * Assume dimension rank is 2.
 */
void
slab_set(int mpi_rank, int mpi_size, hssize_t start[], hsize_t count[],
	 hsize_t stride[], int mode)
{
    switch (mode){
    case BYROW:
	/* Each process takes a slabs of rows. */
	stride[0] = 1;
	stride[1] = 1;
	count[0] = dim0/mpi_size;
	count[1] = dim1;
	start[0] = mpi_rank*count[0];
	start[1] = 0;
if (verbose) printf("slab_set BYROW\n");
	break;
    case BYCOL:
	/* Each process takes a block of columns. */
	stride[0] = 1;
	stride[1] = 1;
	count[0] = dim0;
	count[1] = dim1/mpi_size;
	start[0] = 0;
	start[1] = mpi_rank*count[1];
#ifdef DISABLED
	/* change the above macro to #ifndef if you want to test */
	/* zero elements access. */
	printf("set to size 0\n");
	if (!(mpi_rank % 3))
	    count[1]=0;
#endif
if (verbose) printf("slab_set BYCOL\n");
	break;
    default:
	/* Unknown mode.  Set it to cover the whole dataset. */
	printf("unknown slab_set mode (%d)\n", mode);
	stride[0] = 1;
	stride[1] = 1;
	count[0] = dim0;
	count[1] = dim1;
	start[0] = 0;
	start[1] = 0;
if (verbose) printf("slab_set wholeset\n");
	break;
    }
if (verbose){
    printf("start[]=(%ld,%ld), count[]=(%lu,%lu), total datapoints=%lu\n",
	start[0], start[1], count[0], count[1], count[0]*count[1]);
    }
}


/*
 * Fill the dataset with trivial data for testing.
 * Assume dimension rank is 2 and data is stored contiguous.
 */
void
dataset_fill(hssize_t start[], hsize_t count[], hsize_t stride[], DATATYPE * dataset)
{
    DATATYPE *dataptr = dataset;
    int i, j;

    /* put some trivial data in the data_array */
    for (i=0; i < count[0]; i++){
	for (j=0; j < count[1]; j++){
	    *dataptr = (i*stride[0]+start[0])*100 + (j*stride[1]+start[1]+1);
	    dataptr++;
	}
    }
}


/*
 * Print the content of the dataset.
 */
void dataset_print(hssize_t start[], hsize_t count[], hsize_t stride[], DATATYPE * dataset)
{
    DATATYPE *dataptr = dataset;
    int i, j;

    /* print the column heading */
    printf("%-8s", "Cols:");
    for (j=0; j < count[1]; j++){
	printf("%3ld ", start[1]+j);
    }
    printf("\n");

    /* print the slab data */
    for (i=0; i < count[0]; i++){
	printf("Row %2ld: ", i*stride[0]+start[0]);
	for (j=0; j < count[1]; j++){
	    printf("%03d ", *dataptr++);
	}
	printf("\n");
    }
}


/*
 * Print the content of the dataset.
 */
int dataset_vrfy(hssize_t start[], hsize_t count[], hsize_t stride[], DATATYPE *dataset, DATATYPE *original)
{
#define MAX_ERR_REPORT	10		/* Maximum number of errors reported */
    DATATYPE *dataptr = dataset;
    DATATYPE *originptr = original;

    int i, j, vrfyerrs;

    /* print it if verbose */
    if (verbose) {
	printf("dataset_vrfy dumping:::\n");
	printf("start(%ld, %ld), count(%lu, %lu), stride(%lu, %lu)\n",
	    start[0], start[1], count[0], count[1], stride[0], stride[1]);
	printf("original values:\n");
	dataset_print(start, count, stride, original);
	printf("compared values:\n");
	dataset_print(start, count, stride, dataset);
    }

    vrfyerrs = 0;
    for (i=0; i < count[0]; i++){
	for (j=0; j < count[1]; j++){
	    if (*dataset != *original){
		if (vrfyerrs++ < MAX_ERR_REPORT || verbose){
		    printf("Dataset Verify failed at [%d][%d](row %d, col %d): expect %d, got %d\n",
			i, j,
			(int)(i*stride[0]+start[0]), (int)(j*stride[1]+start[1]),
			*(original), *(dataset));
		}
		dataset++;
		original++;
	    }
	}
    }
    if (vrfyerrs > MAX_ERR_REPORT && !verbose)
	printf("[more errors ...]\n");
    if (vrfyerrs)
	printf("%d errors found in dataset_vrfy\n", vrfyerrs);
    return(vrfyerrs);
}


/*
 * Part 1.a--Independent read/write for fixed dimension datasets.
 */

/*
 * Example of using the parallel HDF5 library to create two datasets
 * in one HDF5 files with parallel MPIO access support.
 * The Datasets are of sizes (number-of-mpi-processes x dim0) x dim1.
 * Each process controls only a slab of size dim0 x dim1 within each
 * dataset.
 */

void
dataset_writeInd(char *filename)
{
    hid_t fid;                  /* HDF5 file ID */
    hid_t acc_tpl;		/* File access templates */
    hid_t sid;   		/* Dataspace ID */
    hid_t file_dataspace;	/* File dataspace ID */
    hid_t mem_dataspace;	/* memory dataspace ID */
    hid_t dataset1, dataset2;	/* Dataset ID */
    hsize_t dims[RANK];   	/* dataset dim sizes */
    DATATYPE *data_array1 = NULL;	/* data buffer */

    hssize_t   start[RANK];			/* for hyperslab setting */
    hsize_t count[RANK], stride[RANK];		/* for hyperslab setting */

    herr_t ret;         	/* Generic return value */
    int   i, j;
    int mpi_size, mpi_rank;
    char *fname;
    
    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;

    if (verbose)
	printf("Independent write test on file %s\n", filename);

    /* set up MPI parameters */
    MPI_Comm_size(MPI_COMM_WORLD,&mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);

    /* allocate memory for data buffer */
    data_array1 = (DATATYPE *)malloc(dim0*dim1*sizeof(DATATYPE));
    VRFY((data_array1 != NULL), "data_array1 malloc succeeded");

    /* ----------------------------------------
     * CREATE AN HDF5 FILE WITH PARALLEL ACCESS
     * ---------------------------------------*/
    /* setup file access template with parallel IO access. */
    acc_tpl = H5Pcreate (H5P_FILE_ACCESS);
    VRFY((acc_tpl >= 0), "H5Pcreate access succeeded");
    /* set Parallel access with communicator */
    ret = H5Pset_fapl_mpio(acc_tpl, comm, info);
    VRFY((ret >= 0), "H5Pset_fapl_mpio succeeded");

    /* create the file collectively */
    fid=H5Fcreate(filename,H5F_ACC_TRUNC,H5P_DEFAULT,acc_tpl);
    VRFY((fid >= 0), "H5Fcreate succeeded");

    /* Release file-access template */
    ret=H5Pclose(acc_tpl);
    VRFY((ret >= 0), "");


    /* ---------------------------------------------
     * Define the dimensions of the overall datasets
     * and the slabs local to the MPI process.
     * ------------------------------------------- */
    /* setup dimensionality object */
    dims[0] = dim0;
    dims[1] = dim1;
    sid = H5Screate_simple (RANK, dims, NULL);
    VRFY((sid >= 0), "H5Screate_simple succeeded");

    
    /* create a dataset collectively */
    dataset1 = H5Dcreate(fid, DATASETNAME1, H5T_NATIVE_INT, sid,
			H5P_DEFAULT);
    VRFY((dataset1 >= 0), "H5Dcreate succeeded");

    /* create another dataset collectively */
    dataset2 = H5Dcreate(fid, DATASETNAME2, H5T_NATIVE_INT, sid,
			H5P_DEFAULT);
    VRFY((dataset2 >= 0), "H5Dcreate succeeded");


    /*
     * To test the independent orders of writes between processes, all
     * even number processes write to dataset1 first, then dataset2.
     * All odd number processes write to dataset2 first, then dataset1.
     */

    /* set up dimensions of the slab this process accesses */
    slab_set(mpi_rank, mpi_size, start, count, stride, BYROW);

    /* put some trivial data in the data_array */
    dataset_fill(start, count, stride, data_array1);
    MESG("data_array initialized");

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);				    
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, NULL); 
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, count, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* write data independently */
    ret = H5Dwrite(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,	    
	    H5P_DEFAULT, data_array1);					    
    VRFY((ret >= 0), "H5Dwrite dataset1 succeeded");
    /* write data independently */
    ret = H5Dwrite(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,	    
	    H5P_DEFAULT, data_array1);					    
    VRFY((ret >= 0), "H5Dwrite dataset2 succeeded");

    /* release dataspace ID */
    H5Sclose(file_dataspace);

    /* close dataset collectively */					    
    ret=H5Dclose(dataset1);
    VRFY((ret >= 0), "H5Dclose1 succeeded");
    ret=H5Dclose(dataset2);
    VRFY((ret >= 0), "H5Dclose2 succeeded");

    /* release all IDs created */
    H5Sclose(sid);

    /* close the file collectively */					    
    H5Fclose(fid);							    

    /* release data buffers */
    if (data_array1) free(data_array1);
}

/* Example of using the parallel HDF5 library to read a dataset */
void
dataset_readInd(char *filename)
{
    hid_t fid;                  /* HDF5 file ID */
    hid_t acc_tpl;		/* File access templates */
    hid_t sid;   		/* Dataspace ID */
    hid_t file_dataspace;	/* File dataspace ID */
    hid_t mem_dataspace;	/* memory dataspace ID */
    hid_t dataset1, dataset2;	/* Dataset ID */
    DATATYPE *data_array1 = NULL;	/* data buffer */
    DATATYPE *data_origin1 = NULL; 	/* expected data buffer */

    hssize_t   start[RANK];			/* for hyperslab setting */
    hsize_t count[RANK], stride[RANK];	/* for hyperslab setting */

    herr_t ret;         	/* Generic return value */
    int   i, j;
    int mpi_size, mpi_rank;

    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;

    if (verbose)
	printf("Independent read test on file %s\n", filename);

    /* set up MPI parameters */
    MPI_Comm_size(MPI_COMM_WORLD,&mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);

    /* allocate memory for data buffer */
    data_array1 = (DATATYPE *)malloc(dim0*dim1*sizeof(DATATYPE));
    VRFY((data_array1 != NULL), "data_array1 malloc succeeded");
    data_origin1 = (DATATYPE *)malloc(dim0*dim1*sizeof(DATATYPE));
    VRFY((data_origin1 != NULL), "data_origin1 malloc succeeded");


    /* setup file access template */
    acc_tpl = H5Pcreate (H5P_FILE_ACCESS);
    VRFY((acc_tpl >= 0), "");
    /* set Parallel access with communicator */
    ret = H5Pset_fapl_mpio(acc_tpl, comm, info);     
    VRFY((ret >= 0), "");


    /* open the file collectively */
    fid=H5Fopen(filename,H5F_ACC_RDONLY,acc_tpl);
    VRFY((fid >= 0), "");

    /* Release file-access template */
    ret=H5Pclose(acc_tpl);
    VRFY((ret >= 0), "");

    /* open the dataset1 collectively */
    dataset1 = H5Dopen(fid, DATASETNAME1);
    VRFY((dataset1 >= 0), "");

    /* open another dataset collectively */
    dataset2 = H5Dopen(fid, DATASETNAME1);
    VRFY((dataset2 >= 0), "");


    /* set up dimensions of the slab this process accesses */
    slab_set(mpi_rank, mpi_size, start, count, stride, BYROW);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);
    VRFY((file_dataspace >= 0), "");
    ret=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, NULL); 
    VRFY((ret >= 0), "");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, count, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* fill dataset with test data */
    dataset_fill(start, count, stride, data_origin1);

    /* read data independently */
    ret = H5Dread(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    H5P_DEFAULT, data_array1);
    VRFY((ret >= 0), "");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, data_array1, data_origin1);
    if (ret) nerrors++;

    /* read data independently */
    ret = H5Dread(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    H5P_DEFAULT, data_array1);
    VRFY((ret >= 0), "");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, data_array1, data_origin1);
    if (ret) nerrors++;

    /* close dataset collectively */
    ret=H5Dclose(dataset1);
    VRFY((ret >= 0), "");
    ret=H5Dclose(dataset2);
    VRFY((ret >= 0), "");

    /* release all IDs created */
    H5Sclose(file_dataspace);

    /* close the file collectively */
    H5Fclose(fid);

    /* release data buffers */
    if (data_array1) free(data_array1);
    if (data_origin1) free(data_origin1);
}


/*
 * Part 1.b--Collective read/write for fixed dimension datasets.
 */

/*
 * Example of using the parallel HDF5 library to create two datasets
 * in one HDF5 file with collective parallel access support.
 * The Datasets are of sizes (number-of-mpi-processes x dim0) x dim1.
 * Each process controls only a slab of size dim0 x dim1 within each
 * dataset. [Note: not so yet.  Datasets are of sizes dim0xdim1 and
 * each process controls a hyperslab within.]
 */

void
dataset_writeAll(char *filename)
{
    hid_t fid;                  /* HDF5 file ID */
    hid_t acc_tpl;		/* File access templates */
    hid_t xfer_plist;		/* Dataset transfer properties list */
    hid_t sid;   		/* Dataspace ID */
    hid_t file_dataspace;	/* File dataspace ID */
    hid_t mem_dataspace;	/* memory dataspace ID */
    hid_t dataset1, dataset2;	/* Dataset ID */
    hid_t datatype;		/* Datatype ID */
    hsize_t dims[RANK];   	/* dataset dim sizes */
    DATATYPE *data_array1 = NULL;	/* data buffer */

    hssize_t   start[RANK];			/* for hyperslab setting */
    hsize_t count[RANK], stride[RANK];		/* for hyperslab setting */

    herr_t ret;         	/* Generic return value */
    int mpi_size, mpi_rank;
    
    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;

    if (verbose)
	printf("Collective write test on file %s\n", filename);

    /* set up MPI parameters */
    MPI_Comm_size(MPI_COMM_WORLD,&mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);

    /* allocate memory for data buffer */
    data_array1 = (DATATYPE *)malloc(dim0*dim1*sizeof(DATATYPE));
    VRFY((data_array1 != NULL), "data_array1 malloc succeeded");

    /* -------------------
     * START AN HDF5 FILE 
     * -------------------*/
    /* setup file access template with parallel IO access. */
    acc_tpl = H5Pcreate (H5P_FILE_ACCESS);
    VRFY((acc_tpl >= 0), "H5Pcreate access succeeded");
    /* set Parallel access with communicator */
    ret = H5Pset_fapl_mpio(acc_tpl, comm, info);     
    VRFY((ret >= 0), "H5Pset_fapl_mpio succeeded");

    /* create the file collectively */
    fid=H5Fcreate(filename,H5F_ACC_TRUNC,H5P_DEFAULT,acc_tpl);
    VRFY((fid >= 0), "H5Fcreate succeeded");

    /* Release file-access template */
    ret=H5Pclose(acc_tpl);
    VRFY((ret >= 0), "");


    /* --------------------------
     * Define the dimensions of the overall datasets
     * and create the dataset
     * ------------------------- */
    /* setup dimensionality object */
    dims[0] = dim0;
    dims[1] = dim1;
    sid = H5Screate_simple (RANK, dims, NULL);
    VRFY((sid >= 0), "H5Screate_simple succeeded");

    
    /* create a dataset collectively */
    dataset1 = H5Dcreate(fid, DATASETNAME1, H5T_NATIVE_INT, sid, H5P_DEFAULT);
    VRFY((dataset1 >= 0), "H5Dcreate succeeded");

    /* create another dataset collectively */
    datatype = H5Tcopy(H5T_NATIVE_INT);
    ret = H5Tset_order(datatype, H5T_ORDER_LE);
    VRFY((ret >= 0), "H5Tset_order succeeded");

    dataset2 = H5Dcreate(fid, DATASETNAME2, datatype, sid, H5P_DEFAULT);
    VRFY((dataset2 >= 0), "H5Dcreate 2 succeeded");

    /*
     * Set up dimensions of the slab this process accesses.
     */

    /* Dataset1: each process takes a block of rows. */
    slab_set(mpi_rank, mpi_size, start, count, stride, BYROW);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);				    
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, NULL); 
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, count, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* fill the local slab with some trivial data */
    dataset_fill(start, count, stride, data_array1);
    MESG("data_array initialized");
    if (verbose){
	MESG("data_array created");
	dataset_print(start, count, stride, data_array1);
    }

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate (H5P_DATA_XFER);
    VRFY((xfer_plist >= 0), "");
    ret=H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pcreate xfer succeeded");

    /* write data collectively */
    ret = H5Dwrite(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);					    
    VRFY((ret >= 0), "H5Dwrite dataset1 succeeded");

    /* release all temporary handles. */
    /* Could have used them for dataset2 but it is cleaner */
    /* to create them again.*/
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);

    /* Dataset2: each process takes a block of columns. */
    slab_set(mpi_rank, mpi_size, start, count, stride, BYCOL);

    /* put some trivial data in the data_array */
    dataset_fill(start, count, stride, data_array1);
    MESG("data_array initialized");
    if (verbose){
	MESG("data_array created");
	dataset_print(start, count, stride, data_array1);
    }

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);				    
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, NULL); 
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, count, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* fill the local slab with some trivial data */
    dataset_fill(start, count, stride, data_array1);
    MESG("data_array initialized");
    if (verbose){
	MESG("data_array created");
	dataset_print(start, count, stride, data_array1);
    }

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate (H5P_DATA_XFER);
    VRFY((xfer_plist >= 0), "");
    ret=H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pcreate xfer succeeded");

    /* write data independently */
    ret = H5Dwrite(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);					    
    VRFY((ret >= 0), "H5Dwrite dataset2 succeeded");

    /* release all temporary handles. */
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);


    /*
     * All writes completed.  Close datasets collectively
     */					    
    ret=H5Dclose(dataset1);
    VRFY((ret >= 0), "H5Dclose1 succeeded");
    ret=H5Dclose(dataset2);
    VRFY((ret >= 0), "H5Dclose2 succeeded");

    /* release all IDs created */
    H5Sclose(sid);

    /* close the file collectively */					    
    H5Fclose(fid);							    

    /* release data buffers */
    if (data_array1) free(data_array1);
}

/*
 * Example of using the parallel HDF5 library to read two datasets
 * in one HDF5 file with collective parallel access support.
 * The Datasets are of sizes (number-of-mpi-processes x dim0) x dim1.
 * Each process controls only a slab of size dim0 x dim1 within each
 * dataset. [Note: not so yet.  Datasets are of sizes dim0xdim1 and
 * each process controls a hyperslab within.]
 */

void
dataset_readAll(char *filename)
{
    hid_t fid;                  /* HDF5 file ID */
    hid_t acc_tpl;		/* File access templates */
    hid_t xfer_plist;		/* Dataset transfer properties list */
    hid_t sid;   		/* Dataspace ID */
    hid_t file_dataspace;	/* File dataspace ID */
    hid_t mem_dataspace;	/* memory dataspace ID */
    hid_t dataset1, dataset2;	/* Dataset ID */
    DATATYPE *data_array1 = NULL;	/* data buffer */
    DATATYPE *data_origin1 = NULL; 	/* expected data buffer */

    hssize_t   start[RANK];			/* for hyperslab setting */
    hsize_t count[RANK], stride[RANK];		/* for hyperslab setting */

    herr_t ret;         	/* Generic return value */
    int mpi_size, mpi_rank;

    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;

    if (verbose)
	printf("Collective read test on file %s\n", filename);

    /* set up MPI parameters */
    MPI_Comm_size(MPI_COMM_WORLD,&mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);

    /* allocate memory for data buffer */
    data_array1 = (DATATYPE *)malloc(dim0*dim1*sizeof(DATATYPE));
    VRFY((data_array1 != NULL), "data_array1 malloc succeeded");
    data_origin1 = (DATATYPE *)malloc(dim0*dim1*sizeof(DATATYPE));
    VRFY((data_origin1 != NULL), "data_origin1 malloc succeeded");

    /* -------------------
     * OPEN AN HDF5 FILE 
     * -------------------*/
    /* setup file access template with parallel IO access. */
    acc_tpl = H5Pcreate (H5P_FILE_ACCESS);
    VRFY((acc_tpl >= 0), "H5Pcreate access succeeded");
    /* set Parallel access with communicator */
    ret = H5Pset_fapl_mpio(acc_tpl, comm, info);     
    VRFY((ret >= 0), "H5Pset_fapl_mpio succeeded");

    /* open the file collectively */
    fid=H5Fopen(filename,H5F_ACC_RDONLY,acc_tpl);
    VRFY((fid >= 0), "H5Fopen succeeded");

    /* Release file-access template */
    ret=H5Pclose(acc_tpl);
    VRFY((ret >= 0), "");


    /* --------------------------
     * Open the datasets in it
     * ------------------------- */
    /* open the dataset1 collectively */
    dataset1 = H5Dopen(fid, DATASETNAME1);
    VRFY((dataset1 >= 0), "H5Dopen succeeded");

    /* open another dataset collectively */
    dataset2 = H5Dopen(fid, DATASETNAME2);
    VRFY((dataset2 >= 0), "H5Dopen 2 succeeded");

    /*
     * Set up dimensions of the slab this process accesses.
     */

    /* Dataset1: each process takes a block of columns. */
    slab_set(mpi_rank, mpi_size, start, count, stride, BYCOL);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);				    
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, NULL); 
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, count, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* fill dataset with test data */
    dataset_fill(start, count, stride, data_origin1);
    MESG("data_array initialized");
    if (verbose){
	MESG("data_array created");
	dataset_print(start, count, stride, data_origin1);
    }

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate (H5P_DATA_XFER);
    VRFY((xfer_plist >= 0), "");
    ret=H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pcreate xfer succeeded");

    /* read data collectively */
    ret = H5Dread(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);					    
    VRFY((ret >= 0), "H5Dread succeeded");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, data_array1, data_origin1);
    if (ret) nerrors++;

    /* release all temporary handles. */
    /* Could have used them for dataset2 but it is cleaner */
    /* to create them again.*/
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);

    /* Dataset2: each process takes a block of rows. */
    slab_set(mpi_rank, mpi_size, start, count, stride, BYROW);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);				    
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, NULL); 
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, count, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* fill dataset with test data */
    dataset_fill(start, count, stride, data_origin1);
    MESG("data_array initialized");
    if (verbose){
	MESG("data_array created");
	dataset_print(start, count, stride, data_origin1);
    }

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate (H5P_DATA_XFER);
    VRFY((xfer_plist >= 0), "");
    ret=H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pcreate xfer succeeded");

    /* read data independently */
    ret = H5Dread(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);					    
    VRFY((ret >= 0), "H5Dread succeeded");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, data_array1, data_origin1);
    if (ret) nerrors++;

    /* release all temporary handles. */
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);


    /*
     * All reads completed.  Close datasets collectively
     */					    
    ret=H5Dclose(dataset1);
    VRFY((ret >= 0), "H5Dclose1 succeeded");
    ret=H5Dclose(dataset2);
    VRFY((ret >= 0), "H5Dclose2 succeeded");

    /* close the file collectively */					    
    H5Fclose(fid);							    

    /* release data buffers */
    if (data_array1) free(data_array1);
    if (data_origin1) free(data_origin1);
}


/*
 * Part 2--Independent read/write for extendible datasets.
 */

/*
 * Example of using the parallel HDF5 library to create two extendible
 * datasets in one HDF5 file with independent parallel MPIO access support.
 * The Datasets are of sizes (number-of-mpi-processes x dim0) x dim1.
 * Each process controls only a slab of size dim0 x dim1 within each
 * dataset.
 */

void
extend_writeInd(char *filename)
{
    hid_t fid;                  /* HDF5 file ID */
    hid_t acc_tpl;		/* File access templates */
    hid_t sid;   		/* Dataspace ID */
    hid_t file_dataspace;	/* File dataspace ID */
    hid_t mem_dataspace;	/* memory dataspace ID */
    hid_t dataset1, dataset2;	/* Dataset ID */
    hsize_t dims[RANK];   	/* dataset dim sizes */
    hsize_t max_dims[RANK] =
		{H5S_UNLIMITED, H5S_UNLIMITED};	/* dataset maximum dim sizes */
    DATATYPE	*data_array1 = NULL;		/* data buffer */
    hsize_t	chunk_dims[RANK];		/* chunk sizes */
    hid_t	dataset_pl;			/* dataset create prop. list */

    hssize_t	start[RANK];	/* for hyperslab setting */
    hsize_t	count[RANK];	/* for hyperslab setting */
    hsize_t	stride[RANK];	/* for hyperslab setting */

    herr_t ret;         	/* Generic return value */
    int   i, j;
    int mpi_size, mpi_rank;
    char *fname;
    
    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;

    if (verbose)
	printf("Extend independent write test on file %s\n", filename);

    /* set up MPI parameters */
    MPI_Comm_size(MPI_COMM_WORLD,&mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);

    /* setup chunk-size. Make sure sizes are > 0 */
    chunk_dims[0] = chunkdim0;
    chunk_dims[1] = chunkdim1;

    /* allocate memory for data buffer */
    data_array1 = (DATATYPE *)malloc(dim0*dim1*sizeof(DATATYPE));
    VRFY((data_array1 != NULL), "data_array1 malloc succeeded");

    /* -------------------
     * START AN HDF5 FILE 
     * -------------------*/
    /* setup file access template with parallel IO access. */
    acc_tpl = H5Pcreate (H5P_FILE_ACCESS);
    VRFY((acc_tpl >= 0), "H5Pcreate access succeeded");
    /* set Parallel access with communicator */
    ret = H5Pset_fapl_mpio(acc_tpl, comm, info);     
    VRFY((ret >= 0), "H5Pset_fapl_mpio succeeded");

    /* create the file collectively */
    fid=H5Fcreate(filename,H5F_ACC_TRUNC,H5P_DEFAULT,acc_tpl);
    VRFY((fid >= 0), "H5Fcreate succeeded");

    /* Release file-access template */
    ret=H5Pclose(acc_tpl);
    VRFY((ret >= 0), "");


    /* --------------------------------------------------------------
     * Define the dimensions of the overall datasets and create them.
     * ------------------------------------------------------------- */

    /* set up dataset storage chunk sizes and creation property list */
    if (verbose)
	printf("chunks[]=%lu,%lu\n", chunk_dims[0], chunk_dims[1]);
    dataset_pl = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((dataset_pl >= 0), "H5Pcreate succeeded");
    ret = H5Pset_chunk(dataset_pl, RANK, chunk_dims);
    VRFY((ret >= 0), "H5Pset_chunk succeeded");

    /* setup dimensionality object */
    /* start out with no rows, extend it later. */
    dims[0] = dims[1] = 0;
    sid = H5Screate_simple (RANK, dims, max_dims);
    VRFY((sid >= 0), "H5Screate_simple succeeded");

    /* create an extendible dataset collectively */
    dataset1 = H5Dcreate(fid, DATASETNAME1, H5T_NATIVE_INT, sid, dataset_pl);
    VRFY((dataset1 >= 0), "H5Dcreate succeeded");

    /* create another extendible dataset collectively */
    dataset2 = H5Dcreate(fid, DATASETNAME2, H5T_NATIVE_INT, sid, dataset_pl);
    VRFY((dataset2 >= 0), "H5Dcreate succeeded");

    /* release resource */
    H5Sclose(sid);



    /* -------------------------
     * Test writing to dataset1
     * -------------------------*/
    /* set up dimensions of the slab this process accesses */
    slab_set(mpi_rank, mpi_size, start, count, stride, BYROW);

    /* put some trivial data in the data_array */
    dataset_fill(start, count, stride, data_array1);
    MESG("data_array initialized");
    if (verbose){
	MESG("data_array created");
	dataset_print(start, count, stride, data_array1);
    }

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, count, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* Extend its current dim sizes before writing */
    dims[0] = dim0;
    dims[1] = dim1;
    ret = H5Dextend (dataset1, dims);
    VRFY((ret >= 0), "H5Dextend succeeded");

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);				    
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, NULL); 
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* write data independently */
    ret = H5Dwrite(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    H5P_DEFAULT, data_array1);					    
    VRFY((ret >= 0), "H5Dwrite succeeded");

    /* release resource */
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);


    /* -------------------------
     * Test writing to dataset2
     * -------------------------*/
    /* set up dimensions of the slab this process accesses */
    slab_set(mpi_rank, mpi_size, start, count, stride, BYCOL);

    /* put some trivial data in the data_array */
    dataset_fill(start, count, stride, data_array1);
    MESG("data_array initialized");
    if (verbose){
	MESG("data_array created");
	dataset_print(start, count, stride, data_array1);
    }

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, count, NULL);
    VRFY((mem_dataspace >= 0), "");

#ifndef DISABLE
    /* Try write to dataset2 beyond its current dim sizes.  Should fail. */
    /* Temporary turn off auto error reporting */
    H5Eget_auto(&old_func, &old_client_data);
    H5Eset_auto(NULL, NULL);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset2);				    
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, NULL); 
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* write data independently.  Should fail. */
    ret = H5Dwrite(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,    
	    H5P_DEFAULT, data_array1);					    
    VRFY((ret < 0), "H5Dwrite failed as expected");

    /* restore auto error reporting */
    H5Eset_auto(old_func, old_client_data);
    H5Sclose(file_dataspace);
#else
    /* Skip test because H5Dwrite is not failing as expected */
    printf("***Skip test of write-beyond-current-dim-size\n");
#endif

    /* Extend dataset2 and try again.  Should succeed. */
    dims[0] = dim0;
    dims[1] = dim1;
    ret = H5Dextend (dataset2, dims);
    VRFY((ret >= 0), "H5Dextend succeeded");

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset2);				    
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, NULL); 
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* write data independently */
    ret = H5Dwrite(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,    
	    H5P_DEFAULT, data_array1);					    
    VRFY((ret >= 0), "H5Dwrite succeeded");

    /* release resource */
    ret=H5Sclose(file_dataspace);
    VRFY((ret >= 0), "H5Sclose succeeded");
    ret=H5Sclose(mem_dataspace);
    VRFY((ret >= 0), "H5Sclose succeeded");


    /* close dataset collectively */					    
    ret=H5Dclose(dataset1);
    VRFY((ret >= 0), "H5Dclose1 succeeded");
    ret=H5Dclose(dataset2);
    VRFY((ret >= 0), "H5Dclose2 succeeded");

    /* close the file collectively */					    
    H5Fclose(fid);							    

    /* release data buffers */
    if (data_array1) free(data_array1);
}

/* Example of using the parallel HDF5 library to read an extendible dataset */
void
extend_readInd(char *filename)
{
    hid_t fid;			/* HDF5 file ID */
    hid_t acc_tpl;		/* File access templates */
    hid_t sid;   		/* Dataspace ID */
    hid_t file_dataspace;	/* File dataspace ID */
    hid_t mem_dataspace;	/* memory dataspace ID */
    hid_t dataset1, dataset2;	/* Dataset ID */
    hsize_t dims[RANK];   	/* dataset dim sizes */
    DATATYPE *data_array1 = NULL;	/* data buffer */
    DATATYPE *data_array2 = NULL;	/* data buffer */
    DATATYPE *data_origin1 = NULL; 	/* expected data buffer */

    hssize_t   start[RANK];			/* for hyperslab setting */
    hsize_t count[RANK], stride[RANK];		/* for hyperslab setting */

    herr_t ret;         	/* Generic return value */
    int   i, j;
    int mpi_size, mpi_rank;

    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;

    if (verbose)
	printf("Extend independent read test on file %s\n", filename);

    /* set up MPI parameters */
    MPI_Comm_size(MPI_COMM_WORLD,&mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);

    /* allocate memory for data buffer */
    data_array1 = (DATATYPE *)malloc(dim0*dim1*sizeof(DATATYPE));
    VRFY((data_array1 != NULL), "data_array1 malloc succeeded");
    data_array2 = (DATATYPE *)malloc(dim0*dim1*sizeof(DATATYPE));
    VRFY((data_array2 != NULL), "data_array2 malloc succeeded");
    data_origin1 = (DATATYPE *)malloc(dim0*dim1*sizeof(DATATYPE));
    VRFY((data_origin1 != NULL), "data_origin1 malloc succeeded");

    /* -------------------
     * OPEN AN HDF5 FILE 
     * -------------------*/
    /* setup file access template */
    acc_tpl = H5Pcreate (H5P_FILE_ACCESS);
    VRFY((acc_tpl >= 0), "");
    /* set Parallel access with communicator */
    ret = H5Pset_fapl_mpio(acc_tpl, comm, info);     
    VRFY((ret >= 0), "");

    /* open the file collectively */
    fid=H5Fopen(filename,H5F_ACC_RDONLY,acc_tpl);
    VRFY((fid >= 0), "");

    /* Release file-access template */
    ret=H5Pclose(acc_tpl);
    VRFY((ret >= 0), "");

    /* open the dataset1 collectively */
    dataset1 = H5Dopen(fid, DATASETNAME1);
    VRFY((dataset1 >= 0), "");

    /* open another dataset collectively */
    dataset2 = H5Dopen(fid, DATASETNAME1);
    VRFY((dataset2 >= 0), "");

    /* Try extend dataset1 which is open RDONLY.  Should fail. */
    /* first turn off auto error reporting */
    H5Eget_auto(&old_func, &old_client_data);
    H5Eset_auto(NULL, NULL);

    file_dataspace = H5Dget_space (dataset1);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret=H5Sget_simple_extent_dims(file_dataspace, dims, NULL);
    VRFY((ret > 0), "H5Sget_simple_extent_dims succeeded");
    dims[0]++;
    ret=H5Dextend(dataset1, dims);
    VRFY((ret < 0), "H5Dextend failed as expected");

    /* restore auto error reporting */
    H5Eset_auto(old_func, old_client_data);
    H5Sclose(file_dataspace);


    /* Read dataset1 using BYROW pattern */
    /* set up dimensions of the slab this process accesses */
    slab_set(mpi_rank, mpi_size, start, count, stride, BYROW);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);
    VRFY((file_dataspace >= 0), "");
    ret=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, NULL); 
    VRFY((ret >= 0), "");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, count, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* fill dataset with test data */
    dataset_fill(start, count, stride, data_origin1);
    if (verbose){
	MESG("data_array created");
	dataset_print(start, count, stride, data_array1);
    }

    /* read data independently */
    ret = H5Dread(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    H5P_DEFAULT, data_array1);
    VRFY((ret >= 0), "H5Dread succeeded");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, data_array1, data_origin1);
    VRFY((ret == 0), "dataset1 read verified correct");
    if (ret) nerrors++;

    H5Sclose(mem_dataspace);
    H5Sclose(file_dataspace);


    /* Read dataset2 using BYCOL pattern */
    /* set up dimensions of the slab this process accesses */
    slab_set(mpi_rank, mpi_size, start, count, stride, BYCOL);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset2);
    VRFY((file_dataspace >= 0), "");
    ret=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, NULL); 
    VRFY((ret >= 0), "");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, count, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* fill dataset with test data */
    dataset_fill(start, count, stride, data_origin1);
    if (verbose){
	MESG("data_array created");
	dataset_print(start, count, stride, data_array1);
    }

    /* read data independently */
    ret = H5Dread(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    H5P_DEFAULT, data_array1);
    VRFY((ret >= 0), "H5Dread succeeded");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, data_array1, data_origin1);
    VRFY((ret == 0), "dataset2 read verified correct");
    if (ret) nerrors++;

    H5Sclose(mem_dataspace);
    H5Sclose(file_dataspace);

    /* close dataset collectively */
    ret=H5Dclose(dataset1);
    VRFY((ret >= 0), "");
    ret=H5Dclose(dataset2);
    VRFY((ret >= 0), "");


    /* close the file collectively */
    H5Fclose(fid);

    /* release data buffers */
    if (data_array1) free(data_array1);
    if (data_array2) free(data_array2);
    if (data_origin1) free(data_origin1);
}
