/* $Id$ */

/*
 * Parallel tests for datasets
 */

/*
 * Example of using the parallel HDF5 library to access datasets.
 *
 * This program contains two major parts.  Part 1 tests fixed dimension
 * datasets, for both independent and collective transfer modes.
 * Part 2 tests extendable datasets, for independent transfer mode
 * only.  Collective mode for extendable datasets are not supported yet.
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
	count[0] = DIM1/mpi_size;
	count[1] = DIM2;
	start[0] = mpi_rank*count[0];
	start[1] = 0;
if (verbose) printf("slab_set BYROW\n");
	break;
    case BYCOL:
	/* Each process takes a block of columns. */
	stride[0] = 1;
	stride[1] = 1;
	count[0] = DIM1;
	count[1] = DIM2/mpi_size;
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
	count[0] = DIM1;
	count[1] = DIM2;
	start[0] = 0;
	start[1] = 0;
if (verbose) printf("slab_set wholeset\n");
	break;
    }
if (verbose){
    printf("start[]=(%d,%d), count[]=(%d,%d), total datapoints=%d\n",
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
	printf("%3d ", start[1]+j);
    }
    printf("\n");

    /* print the slab data */
    for (i=0; i < count[0]; i++){
	printf("Row %2d: ", (int)(i*stride[0]+start[0]));
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
    if (verbose)
	dataset_print(start, count, stride, dataset);

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
 * The Datasets are of sizes (number-of-mpi-processes x DIM1) x DIM2.
 * Each process controls only a slab of size DIM1 x DIM2 within each
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
    hsize_t dims[RANK] = {DIM1,DIM2};		/* dataset dim sizes */
    hsize_t dimslocal1[RANK] = {DIM1,DIM2}; 	/* local dataset dim sizes */
    DATATYPE data_array1[DIM1][DIM2];	/* data buffer */

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

    /* -------------------
     * START AN HDF5 FILE 
     * -------------------*/
    /* setup file access template with parallel IO access. */
    acc_tpl = H5Pcreate (H5P_FILE_ACCESS);
    VRFY((acc_tpl != FAIL), "H5Pcreate access succeeded");
    /* set Parallel access with communicator */
    ret = H5Pset_mpi(acc_tpl, comm, info);     
    VRFY((ret != FAIL), "H5Pset_mpi succeeded");

    /* create the file collectively */
    fid=H5Fcreate(filename,H5F_ACC_TRUNC,H5P_DEFAULT,acc_tpl);
    VRFY((fid != FAIL), "H5Fcreate succeeded");

    /* Release file-access template */
    ret=H5Pclose(acc_tpl);
    VRFY((ret != FAIL), "");


    /* --------------------------
     * Define the dimensions of the overall datasets
     * and the slabs local to the MPI process.
     * ------------------------- */
    /* setup dimensionality object */
    sid = H5Screate_simple (RANK, dims, NULL);
    VRFY((sid != FAIL), "H5Screate_simple succeeded");

    
    /* create a dataset collectively */
    dataset1 = H5Dcreate(fid, DATASETNAME1, H5T_NATIVE_INT, sid,
			H5P_DEFAULT);
    VRFY((dataset1 != FAIL), "H5Dcreate succeeded");

    /* create another dataset collectively */
    dataset2 = H5Dcreate(fid, DATASETNAME2, H5T_NATIVE_INT, sid,
			H5P_DEFAULT);
    VRFY((dataset2 != FAIL), "H5Dcreate succeeded");



    /* set up dimensions of the slab this process accesses */
    slab_set(mpi_rank, mpi_size, start, count, stride, BYROW);

    /* put some trivial data in the data_array */
    dataset_fill(start, count, stride, &data_array1[0][0]);
    MESG("data_array initialized");

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);				    
    VRFY((file_dataspace != FAIL), "H5Dget_space succeeded");
    ret=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, NULL); 
    VRFY((ret != FAIL), "H5Sset_hyperslab succeeded");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, count, NULL);
    VRFY((mem_dataspace != FAIL), "");

    /* write data independently */
    ret = H5Dwrite(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,	    
	    H5P_DEFAULT, data_array1);					    
    VRFY((ret != FAIL), "H5Dwrite succeeded");

    /* write data independently */
    ret = H5Dwrite(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,	    
	    H5P_DEFAULT, data_array1);					    
    VRFY((ret != FAIL), "H5Dwrite succeeded");

    /* release dataspace ID */
    H5Sclose(file_dataspace);

    /* close dataset collectively */					    
    ret=H5Dclose(dataset1);
    VRFY((ret != FAIL), "H5Dclose1 succeeded");
    ret=H5Dclose(dataset2);
    VRFY((ret != FAIL), "H5Dclose2 succeeded");

    /* release all IDs created */
    H5Sclose(sid);

    /* close the file collectively */					    
    H5Fclose(fid);							    
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
    hsize_t dims[] = {DIM1,DIM2};   	/* dataset dim sizes */
    DATATYPE data_array1[DIM1][DIM2];	/* data buffer */
    DATATYPE data_origin1[DIM1][DIM2];	/* expected data buffer */

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


    /* setup file access template */
    acc_tpl = H5Pcreate (H5P_FILE_ACCESS);
    VRFY((acc_tpl != FAIL), "");
    /* set Parallel access with communicator */
    ret = H5Pset_mpi(acc_tpl, comm, info);     
    VRFY((ret != FAIL), "");


    /* open the file collectively */
    fid=H5Fopen(filename,H5F_ACC_RDONLY,acc_tpl);
    VRFY((fid != FAIL), "");

    /* Release file-access template */
    ret=H5Pclose(acc_tpl);
    VRFY((ret != FAIL), "");

    /* open the dataset1 collectively */
    dataset1 = H5Dopen(fid, DATASETNAME1);
    VRFY((dataset1 != FAIL), "");

    /* open another dataset collectively */
    dataset2 = H5Dopen(fid, DATASETNAME1);
    VRFY((dataset2 != FAIL), "");


    /* set up dimensions of the slab this process accesses */
    slab_set(mpi_rank, mpi_size, start, count, stride, BYROW);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);
    VRFY((file_dataspace != FAIL), "");
    ret=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, NULL); 
    VRFY((ret != FAIL), "");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, count, NULL);
    VRFY((mem_dataspace != FAIL), "");

    /* fill dataset with test data */
    dataset_fill(start, count, stride, &data_origin1[0][0]);

    /* read data independently */
    ret = H5Dread(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    H5P_DEFAULT, data_array1);
    VRFY((ret != FAIL), "");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, &data_array1[0][0], &data_origin1[0][0]);
    if (ret) nerrors++;

    /* read data independently */
    ret = H5Dread(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    H5P_DEFAULT, data_array1);
    VRFY((ret != FAIL), "");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, &data_array1[0][0], &data_origin1[0][0]);
    if (ret) nerrors++;

    /* close dataset collectively */
    ret=H5Dclose(dataset1);
    VRFY((ret != FAIL), "");
    ret=H5Dclose(dataset2);
    VRFY((ret != FAIL), "");

    /* release all IDs created */
    H5Sclose(file_dataspace);

    /* close the file collectively */
    H5Fclose(fid);
}


/*
 * Part 1.b--Collective read/write for fixed dimension datasets.
 */

/*
 * Example of using the parallel HDF5 library to create two datasets
 * in one HDF5 file with collective parallel access support.
 * The Datasets are of sizes (number-of-mpi-processes x DIM1) x DIM2.
 * Each process controls only a slab of size DIM1 x DIM2 within each
 * dataset. [Note: not so yet.  Datasets are of sizes DIM1xDIM2 and
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
    hsize_t dims[RANK] = {DIM1,DIM2};	/* dataset dim sizes */
    DATATYPE data_array1[DIM1][DIM2];	/* data buffer */

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

    /* -------------------
     * START AN HDF5 FILE 
     * -------------------*/
    /* setup file access template with parallel IO access. */
    acc_tpl = H5Pcreate (H5P_FILE_ACCESS);
    VRFY((acc_tpl != FAIL), "H5Pcreate access succeeded");
    /* set Parallel access with communicator */
    ret = H5Pset_mpi(acc_tpl, comm, info);     
    VRFY((ret != FAIL), "H5Pset_mpi succeeded");

    /* create the file collectively */
    fid=H5Fcreate(filename,H5F_ACC_TRUNC,H5P_DEFAULT,acc_tpl);
    VRFY((fid != FAIL), "H5Fcreate succeeded");

    /* Release file-access template */
    ret=H5Pclose(acc_tpl);
    VRFY((ret != FAIL), "");


    /* --------------------------
     * Define the dimensions of the overall datasets
     * and create the dataset
     * ------------------------- */
    /* setup dimensionality object */
    sid = H5Screate_simple (RANK, dims, NULL);
    VRFY((sid != FAIL), "H5Screate_simple succeeded");

    
    /* create a dataset collectively */
    dataset1 = H5Dcreate(fid, DATASETNAME1, H5T_NATIVE_INT, sid, H5P_DEFAULT);
    VRFY((dataset1 != FAIL), "H5Dcreate succeeded");

    /* create another dataset collectively */
    datatype = H5Tcopy(H5T_NATIVE_INT);
    ret = H5Tset_order(datatype, H5T_ORDER_LE);
    VRFY((ret != FAIL), "H5Tset_order succeeded");

    dataset2 = H5Dcreate(fid, DATASETNAME2, datatype, sid, H5P_DEFAULT);
    VRFY((dataset2 != FAIL), "H5Dcreate 2 succeeded");

    /*
     * Set up dimensions of the slab this process accesses.
     */

    /* Dataset1: each process takes a block of rows. */
    slab_set(mpi_rank, mpi_size, start, count, stride, BYROW);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);				    
    VRFY((file_dataspace != FAIL), "H5Dget_space succeeded");
    ret=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, NULL); 
    VRFY((ret != FAIL), "H5Sset_hyperslab succeeded");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, count, NULL);
    VRFY((mem_dataspace != FAIL), "");

    /* fill the local slab with some trivial data */
    dataset_fill(start, count, stride, &data_array1[0][0]);
    MESG("data_array initialized");
    if (verbose){
	MESG("data_array created");
	dataset_print(start, count, stride, &data_array1[0][0]);
    }

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    VRFY((xfer_plist != FAIL), "");
    ret=H5Pset_xfer(xfer_plist, H5D_XFER_COLLECTIVE);
    VRFY((ret != FAIL), "H5Pcreate xfer succeeded");

    /* write data collectively */
    ret = H5Dwrite(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);					    
    VRFY((ret != FAIL), "H5Dwrite dataset1 succeeded");

    /* release all temporary handles. */
    /* Could have used them for dataset2 but it is cleaner */
    /* to create them again.*/
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);

    /* Dataset2: each process takes a block of columns. */
    slab_set(mpi_rank, mpi_size, start, count, stride, BYCOL);

    /* put some trivial data in the data_array */
    dataset_fill(start, count, stride, &data_array1[0][0]);
    MESG("data_array initialized");
    if (verbose){
	MESG("data_array created");
	dataset_print(start, count, stride, &data_array1[0][0]);
    }

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);				    
    VRFY((file_dataspace != FAIL), "H5Dget_space succeeded");
    ret=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, NULL); 
    VRFY((ret != FAIL), "H5Sset_hyperslab succeeded");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, count, NULL);
    VRFY((mem_dataspace != FAIL), "");

    /* fill the local slab with some trivial data */
    dataset_fill(start, count, stride, &data_array1[0][0]);
    MESG("data_array initialized");
    if (verbose){
	MESG("data_array created");
	dataset_print(start, count, stride, &data_array1[0][0]);
    }

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    VRFY((xfer_plist != FAIL), "");
    ret=H5Pset_xfer(xfer_plist, H5D_XFER_COLLECTIVE);
    VRFY((ret != FAIL), "H5Pcreate xfer succeeded");

    /* write data independently */
    ret = H5Dwrite(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);					    
    VRFY((ret != FAIL), "H5Dwrite dataset2 succeeded");

    /* release all temporary handles. */
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);


    /*
     * All writes completed.  Close datasets collectively
     */					    
    ret=H5Dclose(dataset1);
    VRFY((ret != FAIL), "H5Dclose1 succeeded");
    ret=H5Dclose(dataset2);
    VRFY((ret != FAIL), "H5Dclose2 succeeded");

    /* release all IDs created */
    H5Sclose(sid);

    /* close the file collectively */					    
    H5Fclose(fid);							    
}

/*
 * Example of using the parallel HDF5 library to read two datasets
 * in one HDF5 file with collective parallel access support.
 * The Datasets are of sizes (number-of-mpi-processes x DIM1) x DIM2.
 * Each process controls only a slab of size DIM1 x DIM2 within each
 * dataset. [Note: not so yet.  Datasets are of sizes DIM1xDIM2 and
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
    hsize_t dims[] = {DIM1,DIM2};   	/* dataset dim sizes */
    DATATYPE data_array1[DIM1][DIM2];	/* data buffer */
    DATATYPE data_origin1[DIM1][DIM2];	/* expected data buffer */

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

    /* -------------------
     * OPEN AN HDF5 FILE 
     * -------------------*/
    /* setup file access template with parallel IO access. */
    acc_tpl = H5Pcreate (H5P_FILE_ACCESS);
    VRFY((acc_tpl != FAIL), "H5Pcreate access succeeded");
    /* set Parallel access with communicator */
    ret = H5Pset_mpi(acc_tpl, comm, info);     
    VRFY((ret != FAIL), "H5Pset_mpi succeeded");

    /* open the file collectively */
    fid=H5Fopen(filename,H5F_ACC_RDONLY,acc_tpl);
    VRFY((fid != FAIL), "H5Fopen succeeded");

    /* Release file-access template */
    ret=H5Pclose(acc_tpl);
    VRFY((ret != FAIL), "");


    /* --------------------------
     * Open the datasets in it
     * ------------------------- */
    /* open the dataset1 collectively */
    dataset1 = H5Dopen(fid, DATASETNAME1);
    VRFY((dataset1 != FAIL), "H5Dopen succeeded");

    /* open another dataset collectively */
    dataset2 = H5Dopen(fid, DATASETNAME2);
    VRFY((dataset2 != FAIL), "H5Dopen 2 succeeded");

    /*
     * Set up dimensions of the slab this process accesses.
     */

    /* Dataset1: each process takes a block of columns. */
    slab_set(mpi_rank, mpi_size, start, count, stride, BYCOL);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);				    
    VRFY((file_dataspace != FAIL), "H5Dget_space succeeded");
    ret=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, NULL); 
    VRFY((ret != FAIL), "H5Sset_hyperslab succeeded");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, count, NULL);
    VRFY((mem_dataspace != FAIL), "");

    /* fill dataset with test data */
    dataset_fill(start, count, stride, &data_origin1[0][0]);
    MESG("data_array initialized");
    if (verbose){
	MESG("data_array created");
	dataset_print(start, count, stride, &data_origin1[0][0]);
    }

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    VRFY((xfer_plist != FAIL), "");
    ret=H5Pset_xfer(xfer_plist, H5D_XFER_COLLECTIVE);
    VRFY((ret != FAIL), "H5Pcreate xfer succeeded");

    /* read data collectively */
    ret = H5Dread(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);					    
    VRFY((ret != FAIL), "H5Dread succeeded");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, &data_array1[0][0], &data_origin1[0][0]);
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
    VRFY((file_dataspace != FAIL), "H5Dget_space succeeded");
    ret=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, NULL); 
    VRFY((ret != FAIL), "H5Sset_hyperslab succeeded");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, count, NULL);
    VRFY((mem_dataspace != FAIL), "");

    /* fill dataset with test data */
    dataset_fill(start, count, stride, &data_origin1[0][0]);
    MESG("data_array initialized");
    if (verbose){
	MESG("data_array created");
	dataset_print(start, count, stride, &data_origin1[0][0]);
    }

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    VRFY((xfer_plist != FAIL), "");
    ret=H5Pset_xfer(xfer_plist, H5D_XFER_COLLECTIVE);
    VRFY((ret != FAIL), "H5Pcreate xfer succeeded");

    /* read data independently */
    ret = H5Dread(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);					    
    VRFY((ret != FAIL), "H5Dread succeeded");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, &data_array1[0][0], &data_origin1[0][0]);
    if (ret) nerrors++;

    /* release all temporary handles. */
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);


    /*
     * All reads completed.  Close datasets collectively
     */					    
    ret=H5Dclose(dataset1);
    VRFY((ret != FAIL), "H5Dclose1 succeeded");
    ret=H5Dclose(dataset2);
    VRFY((ret != FAIL), "H5Dclose2 succeeded");

    /* close the file collectively */					    
    H5Fclose(fid);							    
}


/*
 * Part 2--Independent read/write for extendable datasets.
 */

/*
 * Example of using the parallel HDF5 library to create two extendable
 * datasets in one HDF5 file with independent parallel MPIO access support.
 * The Datasets are of sizes (number-of-mpi-processes x DIM1) x DIM2.
 * Each process controls only a slab of size DIM1 x DIM2 within each
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
    hsize_t dims[RANK] = {DIM1,DIM2};		/* dataset initial dim sizes */
    hsize_t max_dims[RANK] =
		{H5S_UNLIMITED, H5S_UNLIMITED};	/* dataset maximum dim sizes */
    hsize_t	dimslocal1[RANK] = {DIM1,DIM2};	/* local dataset dim sizes */
    DATATYPE	data_array1[DIM1][DIM2];	/* data buffer */
    hsize_t	chunk_dims[RANK] = {7, 13};	/* chunk sizes */
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

    /* -------------------
     * START AN HDF5 FILE 
     * -------------------*/
    /* setup file access template with parallel IO access. */
    acc_tpl = H5Pcreate (H5P_FILE_ACCESS);
    VRFY((acc_tpl != FAIL), "H5Pcreate access succeeded");
    /* set Parallel access with communicator */
    ret = H5Pset_mpi(acc_tpl, comm, info);     
    VRFY((ret != FAIL), "H5Pset_mpi succeeded");

    /* create the file collectively */
    fid=H5Fcreate(filename,H5F_ACC_TRUNC,H5P_DEFAULT,acc_tpl);
    VRFY((fid != FAIL), "H5Fcreate succeeded");

    /* Release file-access template */
    ret=H5Pclose(acc_tpl);
    VRFY((ret != FAIL), "");


    /* --------------------------
     * Define the dimensions of the overall datasets and create them.
     * ------------------------- */

    /* set up dataset storage chunk sizes and creation property list */
    if (verbose)
	printf("chunks[]=%d,%d\n", chunk_dims[0], chunk_dims[1]);
    dataset_pl = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((dataset_pl != FAIL), "H5Pcreate succeeded");
    ret = H5Pset_chunk(dataset_pl, RANK, chunk_dims);
    VRFY((ret != FAIL), "H5Pset_chunk succeeded");

    /* setup dimensionality object */
    /* start out with no rows, extend it later. */
    dims[0] = dims[1] = 0;
    sid = H5Screate_simple (RANK, dims, max_dims);
    VRFY((sid != FAIL), "H5Screate_simple succeeded");

    /* create an extendable dataset collectively */
    dataset1 = H5Dcreate(fid, DATASETNAME1, H5T_NATIVE_INT, sid, dataset_pl);
    VRFY((dataset1 != FAIL), "H5Dcreate succeeded");

    /* create another extendable dataset collectively */
    dataset2 = H5Dcreate(fid, DATASETNAME2, H5T_NATIVE_INT, sid, dataset_pl);
    VRFY((dataset2 != FAIL), "H5Dcreate succeeded");

    /* release resource */
    H5Sclose(sid);



    /* -------------------------
     * Test writing to dataset1
     * -------------------------*/
    /* set up dimensions of the slab this process accesses */
    slab_set(mpi_rank, mpi_size, start, count, stride, BYROW);

    /* put some trivial data in the data_array */
    dataset_fill(start, count, stride, &data_array1[0][0]);
    MESG("data_array initialized");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, count, NULL);
    VRFY((mem_dataspace != FAIL), "");

    /* Extend its current dim sizes before writing */
    dims[0] = DIM1;
    dims[1] = DIM2;
    ret = H5Dextend (dataset1, dims);
    VRFY((ret != FAIL), "H5Dextend succeeded");

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);				    
    VRFY((file_dataspace != FAIL), "H5Dget_space succeeded");
    ret=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, NULL); 
    VRFY((ret != FAIL), "H5Sset_hyperslab succeeded");

    /* write data independently */
    ret = H5Dwrite(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    H5P_DEFAULT, data_array1);					    
    VRFY((ret != FAIL), "H5Dwrite succeeded");

    /* release resource */
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);


    /* -------------------------
     * Test writing to dataset2
     * -------------------------*/
    /* set up dimensions of the slab this process accesses */
    slab_set(mpi_rank, mpi_size, start, count, stride, BYCOL);

    /* put some trivial data in the data_array */
    dataset_fill(start, count, stride, &data_array1[0][0]);
    MESG("data_array initialized");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, count, NULL);
    VRFY((mem_dataspace != FAIL), "");

#ifdef DISABLE
    /* Try write to dataset2 without extending it first.  Should fail. */
    /* first turn off auto error reporting */
    H5Eget_auto(&old_func, &old_client_data);
    H5Eset_auto(NULL, NULL);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset2);				    
    VRFY((file_dataspace != FAIL), "H5Dget_space succeeded");
    ret=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, NULL); 
    VRFY((ret != FAIL), "H5Sset_hyperslab succeeded");

    /* write data independently.  Should fail. */
    ret = H5Dwrite(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,    
	    H5P_DEFAULT, data_array1);					    
    VRFY((ret == FAIL), "H5Dwrite failed as expected");

    /* restore auto error reporting */
    H5Eset_auto(old_func, old_client_data);
    H5Sclose(file_dataspace);
#else
    /* Skip test because H5Dwrite is not failing as expected */
    printf("Skip test of write before extend\n");
#endif

    /* Extend dataset2 and try again.  Should succeed. */
    dims[0] = DIM1;
    dims[1] = DIM2;
    ret = H5Dextend (dataset2, dims);
    VRFY((ret != FAIL), "H5Dextend succeeded");

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset2);				    
    VRFY((file_dataspace != FAIL), "H5Dget_space succeeded");
    ret=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, NULL); 
    VRFY((ret != FAIL), "H5Sset_hyperslab succeeded");

    /* write data independently */
    ret = H5Dwrite(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,    
	    H5P_DEFAULT, data_array1);					    
    VRFY((ret != FAIL), "H5Dwrite succeeded");

    /* release resource */
    ret=H5Sclose(file_dataspace);
    VRFY((ret != FAIL), "H5Sclose succeeded");
    ret=H5Sclose(mem_dataspace);
    VRFY((ret != FAIL), "H5Sclose succeeded");


    /* close dataset collectively */					    
    ret=H5Dclose(dataset1);
    VRFY((ret != FAIL), "H5Dclose1 succeeded");
    ret=H5Dclose(dataset2);
    VRFY((ret != FAIL), "H5Dclose2 succeeded");

    /* close the file collectively */					    
    H5Fclose(fid);							    
}

/* Example of using the parallel HDF5 library to read an extendable dataset */
void
extend_readInd(char *filename)
{
    hid_t fid;			/* HDF5 file ID */
    hid_t acc_tpl;		/* File access templates */
    hid_t sid;   		/* Dataspace ID */
    hid_t file_dataspace;	/* File dataspace ID */
    hid_t mem_dataspace;	/* memory dataspace ID */
    hid_t dataset1, dataset2;	/* Dataset ID */
    hsize_t dims[] = {DIM1,DIM2};   	/* dataset dim sizes */
    DATATYPE data_array1[DIM1][DIM2];	/* data buffer */
    DATATYPE data_array2[DIM1][DIM2];	/* data buffer */
    DATATYPE data_origin1[DIM1][DIM2];	/* expected data buffer */

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


    /* setup file access template */
    acc_tpl = H5Pcreate (H5P_FILE_ACCESS);
    VRFY((acc_tpl != FAIL), "");
    /* set Parallel access with communicator */
    ret = H5Pset_mpi(acc_tpl, comm, info);     
    VRFY((ret != FAIL), "");


    /* open the file collectively */
    fid=H5Fopen(filename,H5F_ACC_RDONLY,acc_tpl);
    VRFY((fid != FAIL), "");

    /* Release file-access template */
    ret=H5Pclose(acc_tpl);
    VRFY((ret != FAIL), "");

    /* open the dataset1 collectively */
    dataset1 = H5Dopen(fid, DATASETNAME1);
    VRFY((dataset1 != FAIL), "");

    /* open another dataset collectively */
    dataset2 = H5Dopen(fid, DATASETNAME1);
    VRFY((dataset2 != FAIL), "");

    /* Try extend dataset1 which is open RDONLY.  Should fail. */
    /* first turn off auto error reporting */
    H5Eget_auto(&old_func, &old_client_data);
    H5Eset_auto(NULL, NULL);

    file_dataspace = H5Dget_space (dataset1);
    VRFY((file_dataspace != FAIL), "H5Dget_space succeeded");
    ret=H5Sget_dims(file_dataspace, dims, NULL);
    VRFY((ret > 0), "H5Sget_dims succeeded");
    dims[0]=dims[0]*2;
    ret=H5Dextend(dataset1, dims);
    VRFY((ret == FAIL), "H5Dextend failed as expected");

    /* restore auto error reporting */
    H5Eset_auto(old_func, old_client_data);


    /* Read dataset1 using BYROW pattern */
    /* set up dimensions of the slab this process accesses */
    slab_set(mpi_rank, mpi_size, start, count, stride, BYROW);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);
    VRFY((file_dataspace != FAIL), "");
    ret=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, NULL); 
    VRFY((ret != FAIL), "");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, count, NULL);
    VRFY((mem_dataspace != FAIL), "");

    /* fill dataset with test data */
    dataset_fill(start, count, stride, &data_origin1[0][0]);

    /* read data independently */
    ret = H5Dread(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    H5P_DEFAULT, data_array1);
    VRFY((ret != FAIL), "H5Dread succeeded");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, &data_array1[0][0], &data_origin1[0][0]);
    VRFY((ret == 0), "dataset1 read verified correct");
    if (ret) nerrors++;

    H5Sclose(mem_dataspace);
    H5Sclose(file_dataspace);


    /* Read dataset2 using BYCOL pattern */
    /* set up dimensions of the slab this process accesses */
    slab_set(mpi_rank, mpi_size, start, count, stride, BYCOL);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset2);
    VRFY((file_dataspace != FAIL), "");
    ret=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, NULL); 
    VRFY((ret != FAIL), "");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, count, NULL);
    VRFY((mem_dataspace != FAIL), "");

    /* fill dataset with test data */
    dataset_fill(start, count, stride, &data_origin1[0][0]);

    /* read data independently */
    ret = H5Dread(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    H5P_DEFAULT, data_array1);
    VRFY((ret != FAIL), "H5Dread succeeded");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, &data_array1[0][0], &data_origin1[0][0]);
    VRFY((ret == 0), "dataset2 read verified correct");
    if (ret) nerrors++;

    H5Sclose(mem_dataspace);
    H5Sclose(file_dataspace);

    /* close dataset collectively */
    ret=H5Dclose(dataset1);
    VRFY((ret != FAIL), "");
    ret=H5Dclose(dataset2);
    VRFY((ret != FAIL), "");


    /* close the file collectively */
    H5Fclose(fid);
}
