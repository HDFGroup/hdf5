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
 * Parallel tests for datasets
 */

/*
 * Example of using the parallel HDF5 library to access datasets.
 *
 * This program contains three major parts.  Part 1 tests fixed dimension
 * datasets, for both independent and collective transfer modes.
 * Part 2 tests extendible datasets, for independent transfer mode
 * only.
 * Part 3 tests extendible datasets, for collective transfer mode
 * only.
 */

#include "testphdf5.h"

/*
 * The following are various utility routines used by the tests.
 */

/*
 * Setup the dimensions of the hyperslab.
 * Two modes--by rows or by columns.
 * Assume dimension rank is 2.
 * BYROW	divide into slabs of rows
 * BYCOL	divide into blocks of columns
 * ZROW		same as BYROW except process 0 gets 0 rows
 * ZCOL		same as BYCOL except process 0 gets 0 columns
 */
static void
slab_set(int mpi_rank, int mpi_size, hsize_t start[], hsize_t count[],
	 hsize_t stride[], hsize_t block[], int mode)
{
    switch (mode){
    case BYROW:
	/* Each process takes a slabs of rows. */
	block[0] = dim0/mpi_size;
	block[1] = dim1;
	stride[0] = block[0];
	stride[1] = block[1];
	count[0] = 1;
	count[1] = 1;
	start[0] = mpi_rank*block[0];
	start[1] = 0;
if(VERBOSE_MED) printf("slab_set BYROW\n");
	break;
    case BYCOL:
	/* Each process takes a block of columns. */
	block[0] = dim0;
	block[1] = dim1/mpi_size;
	stride[0] = block[0];
	stride[1] = block[1];
	count[0] = 1;
	count[1] = 1;
	start[0] = 0;
	start[1] = mpi_rank*block[1];
if(VERBOSE_MED) printf("slab_set BYCOL\n");
	break;
    case ZROW:
	/* Similar to BYROW except process 0 gets 0 row */
	block[0] = (mpi_rank ? dim0/mpi_size : 0);
	block[1] = dim1;
        stride[0] = (mpi_rank ? block[0] : 1);  /* avoid setting stride to 0 */
	stride[1] = block[1];
	count[0] = 1;
	count[1] = 1;
	start[0] = (mpi_rank? mpi_rank*block[0] : 0);
	start[1] = 0;
if(VERBOSE_MED) printf("slab_set ZROW\n");
	break;
    case ZCOL:
	/* Similar to BYCOL except process 0 gets 0 column */
	block[0] = dim0;
	block[1] = (mpi_rank ? dim1/mpi_size : 0);
	stride[0] = block[0];
        stride[1] = (mpi_rank ? block[1] : 1);  /* avoid setting stride to 0 */
	count[0] = 1;
	count[1] = 1;
	start[0] = 0;
	start[1] = (mpi_rank? mpi_rank*block[1] : 0);
if(VERBOSE_MED) printf("slab_set ZCOL\n");
	break;
    default:
	/* Unknown mode.  Set it to cover the whole dataset. */
	printf("unknown slab_set mode (%d)\n", mode);
	block[0] = dim0;
	block[1] = dim1;
	stride[0] = block[0];
	stride[1] = block[1];
	count[0] = 1;
	count[1] = 1;
	start[0] = 0;
	start[1] = 0;
if(VERBOSE_MED) printf("slab_set wholeset\n");
	break;
    }
if(VERBOSE_MED){
    printf("start[]=(%lu,%lu), count[]=(%lu,%lu), stride[]=(%lu,%lu), block[]=(%lu,%lu), total datapoints=%lu\n",
	(unsigned long)start[0], (unsigned long)start[1], (unsigned long)count[0], (unsigned long)count[1],
	(unsigned long)stride[0], (unsigned long)stride[1], (unsigned long)block[0], (unsigned long)block[1],
	(unsigned long)(block[0]*block[1]*count[0]*count[1]));
    }
}


/*
 * Fill the dataset with trivial data for testing.
 * Assume dimension rank is 2 and data is stored contiguous.
 */
static void
dataset_fill(hsize_t start[], hsize_t block[], DATATYPE * dataset)
{
    DATATYPE *dataptr = dataset;
    hsize_t i, j;

    /* put some trivial data in the data_array */
    for (i=0; i < block[0]; i++){
	for (j=0; j < block[1]; j++){
	    *dataptr = (DATATYPE)((i+start[0])*100 + (j+start[1]+1));
	    dataptr++;
	}
    }
}


/*
 * Print the content of the dataset.
 */
static void
dataset_print(hsize_t start[], hsize_t block[], DATATYPE * dataset)
{
    DATATYPE *dataptr = dataset;
    hsize_t i, j;

    /* print the column heading */
    printf("%-8s", "Cols:");
    for (j=0; j < block[1]; j++){
	printf("%3lu ", (unsigned long)(start[1]+j));
    }
    printf("\n");

    /* print the slab data */
    for (i=0; i < block[0]; i++){
	printf("Row %2lu: ", (unsigned long)(i+start[0]));
	for (j=0; j < block[1]; j++){
	    printf("%03d ", *dataptr++);
	}
	printf("\n");
    }
}


/*
 * Print the content of the dataset.
 */
int
dataset_vrfy(hsize_t start[], hsize_t count[], hsize_t stride[], hsize_t block[], DATATYPE *dataset, DATATYPE *original)
{
    hsize_t i, j;
    int vrfyerrs;

    /* print it if VERBOSE_MED */
    if(VERBOSE_MED) {
	printf("dataset_vrfy dumping:::\n");
	printf("start(%lu, %lu), count(%lu, %lu), stride(%lu, %lu), block(%lu, %lu)\n",
	    (unsigned long)start[0], (unsigned long)start[1], (unsigned long)count[0], (unsigned long)count[1],
	    (unsigned long)stride[0], (unsigned long)stride[1], (unsigned long)block[0], (unsigned long)block[1]);
	printf("original values:\n");
	dataset_print(start, block, original);
	printf("compared values:\n");
	dataset_print(start, block, dataset);
    }

    vrfyerrs = 0;
    for (i=0; i < block[0]; i++){
	for (j=0; j < block[1]; j++){
	    if(*dataset != *original){
		if(vrfyerrs++ < MAX_ERR_REPORT || VERBOSE_MED){
		    printf("Dataset Verify failed at [%lu][%lu](row %lu, col %lu): expect %d, got %d\n",
			(unsigned long)i, (unsigned long)j,
			(unsigned long)(i+start[0]), (unsigned long)(j+start[1]),
			*(original), *(dataset));
		}
		dataset++;
		original++;
	    }
	}
    }
    if(vrfyerrs > MAX_ERR_REPORT && !VERBOSE_MED)
	printf("[more errors ...]\n");
    if(vrfyerrs)
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
dataset_writeInd(void)
{
    hid_t fid;                  /* HDF5 file ID */
    hid_t acc_tpl;		/* File access templates */
    hid_t sid;   		/* Dataspace ID */
    hid_t file_dataspace;	/* File dataspace ID */
    hid_t mem_dataspace;	/* memory dataspace ID */
    hid_t dataset1, dataset2;	/* Dataset ID */
    hbool_t use_gpfs = FALSE;   /* Use GPFS hints */
    hsize_t dims[RANK];   	/* dataset dim sizes */
    DATATYPE *data_array1 = NULL;	/* data buffer */
    const char *filename;

    hsize_t   start[RANK];			/* for hyperslab setting */
    hsize_t count[RANK], stride[RANK];		/* for hyperslab setting */
    hsize_t block[RANK];			/* for hyperslab setting */

    herr_t ret;         	/* Generic return value */
    int mpi_size, mpi_rank;

    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;

    filename = GetTestParameters();
    if(VERBOSE_MED)
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
    /* setup file access template */
    acc_tpl = create_faccess_plist(comm, info, facc_type, use_gpfs);
    VRFY((acc_tpl >= 0), "");

    /* create the file collectively */
    fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, acc_tpl);
    VRFY((fid >= 0), "H5Fcreate succeeded");

    /* Release file-access template */
    ret = H5Pclose(acc_tpl);
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
    dataset1 = H5Dcreate2(fid, DATASETNAME1, H5T_NATIVE_INT, sid,
			H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((dataset1 >= 0), "H5Dcreate2 succeeded");

    /* create another dataset collectively */
    dataset2 = H5Dcreate2(fid, DATASETNAME2, H5T_NATIVE_INT, sid,
			H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((dataset2 >= 0), "H5Dcreate2 succeeded");


    /*
     * To test the independent orders of writes between processes, all
     * even number processes write to dataset1 first, then dataset2.
     * All odd number processes write to dataset2 first, then dataset1.
     */

    /* set up dimensions of the slab this process accesses */
    slab_set(mpi_rank, mpi_size, start, count, stride, block, BYROW);

    /* put some trivial data in the data_array */
    dataset_fill(start, block, data_array1);
    MESG("data_array initialized");

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* write data independently */
    ret = H5Dwrite(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    H5P_DEFAULT, data_array1);
    VRFY((ret >= 0), "H5Dwrite dataset1 succeeded");
    /* write data independently */
    ret = H5Dwrite(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    H5P_DEFAULT, data_array1);
    VRFY((ret >= 0), "H5Dwrite dataset2 succeeded");

    /* setup dimensions again to write with zero rows for process 0 */
    if(VERBOSE_MED)
	printf("writeInd by some with zero row\n");
    slab_set(mpi_rank, mpi_size, start, count, stride, block, ZROW);
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");
    /* need to make mem_dataspace to match for process 0 */
    if(MAINPROCESS){
	ret = H5Sselect_hyperslab(mem_dataspace, H5S_SELECT_SET, start, stride, count, block);
	VRFY((ret >= 0), "H5Sset_hyperslab mem_dataspace succeeded");
    }
    MESG("writeInd by some with zero row");
if((mpi_rank/2)*2 != mpi_rank){
    ret = H5Dwrite(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    H5P_DEFAULT, data_array1);
    VRFY((ret >= 0), "H5Dwrite dataset1 by ZROW succeeded");
}
#ifdef BARRIER_CHECKS
MPI_Barrier(MPI_COMM_WORLD);
#endif /* BARRIER_CHECKS */

    /* release dataspace ID */
    H5Sclose(file_dataspace);

    /* close dataset collectively */
    ret = H5Dclose(dataset1);
    VRFY((ret >= 0), "H5Dclose1 succeeded");
    ret = H5Dclose(dataset2);
    VRFY((ret >= 0), "H5Dclose2 succeeded");

    /* release all IDs created */
    H5Sclose(sid);

    /* close the file collectively */
    H5Fclose(fid);

    /* release data buffers */
    if(data_array1) free(data_array1);
}

/* Example of using the parallel HDF5 library to read a dataset */
void
dataset_readInd(void)
{
    hid_t fid;                  /* HDF5 file ID */
    hid_t acc_tpl;		/* File access templates */
    hid_t file_dataspace;	/* File dataspace ID */
    hid_t mem_dataspace;	/* memory dataspace ID */
    hid_t dataset1, dataset2;	/* Dataset ID */
    hbool_t use_gpfs = FALSE;   /* Use GPFS hints */
    DATATYPE *data_array1 = NULL;	/* data buffer */
    DATATYPE *data_origin1 = NULL; 	/* expected data buffer */
    const char *filename;

    hsize_t start[RANK];			/* for hyperslab setting */
    hsize_t count[RANK], stride[RANK];		/* for hyperslab setting */
    hsize_t block[RANK];			/* for hyperslab setting */

    herr_t ret;         	/* Generic return value */
    int mpi_size, mpi_rank;

    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;

    filename = GetTestParameters();
    if(VERBOSE_MED)
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
    acc_tpl = create_faccess_plist(comm, info, facc_type, use_gpfs);
    VRFY((acc_tpl >= 0), "");

    /* open the file collectively */
    fid=H5Fopen(filename,H5F_ACC_RDONLY,acc_tpl);
    VRFY((fid >= 0), "");

    /* Release file-access template */
    ret = H5Pclose(acc_tpl);
    VRFY((ret >= 0), "");

    /* open the dataset1 collectively */
    dataset1 = H5Dopen2(fid, DATASETNAME1, H5P_DEFAULT);
    VRFY((dataset1 >= 0), "");

    /* open another dataset collectively */
    dataset2 = H5Dopen2(fid, DATASETNAME1, H5P_DEFAULT);
    VRFY((dataset2 >= 0), "");


    /* set up dimensions of the slab this process accesses */
    slab_set(mpi_rank, mpi_size, start, count, stride, block, BYROW);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);
    VRFY((file_dataspace >= 0), "");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* fill dataset with test data */
    dataset_fill(start, block, data_origin1);

    /* read data independently */
    ret = H5Dread(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    H5P_DEFAULT, data_array1);
    VRFY((ret >= 0), "");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, block, data_array1, data_origin1);
    if(ret) nerrors++;

    /* read data independently */
    ret = H5Dread(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    H5P_DEFAULT, data_array1);
    VRFY((ret >= 0), "");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, block, data_array1, data_origin1);
    if(ret) nerrors++;

    /* close dataset collectively */
    ret = H5Dclose(dataset1);
    VRFY((ret >= 0), "");
    ret = H5Dclose(dataset2);
    VRFY((ret >= 0), "");

    /* release all IDs created */
    H5Sclose(file_dataspace);

    /* close the file collectively */
    H5Fclose(fid);

    /* release data buffers */
    if(data_array1) free(data_array1);
    if(data_origin1) free(data_origin1);
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
dataset_writeAll(void)
{
    hid_t fid;                  /* HDF5 file ID */
    hid_t acc_tpl;		/* File access templates */
    hid_t xfer_plist;		/* Dataset transfer properties list */
    hid_t sid;   		/* Dataspace ID */
    hid_t file_dataspace;	/* File dataspace ID */
    hid_t mem_dataspace;	/* memory dataspace ID */
    hid_t dataset1, dataset2, dataset3, dataset4;	/* Dataset ID */
    hid_t datatype;		/* Datatype ID */
    hbool_t use_gpfs = FALSE;   /* Use GPFS hints */
    hsize_t dims[RANK];   	/* dataset dim sizes */
    DATATYPE *data_array1 = NULL;	/* data buffer */
    const char *filename;

    hsize_t start[RANK];			/* for hyperslab setting */
    hsize_t count[RANK], stride[RANK];		/* for hyperslab setting */
    hsize_t block[RANK];			/* for hyperslab setting */

    herr_t ret;         	/* Generic return value */
    int mpi_size, mpi_rank;

    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;

    filename = GetTestParameters();
    if(VERBOSE_MED)
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
    /* setup file access template */
    acc_tpl = create_faccess_plist(comm, info, facc_type, use_gpfs);
    VRFY((acc_tpl >= 0), "");

    /* create the file collectively */
    fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, acc_tpl);
    VRFY((fid >= 0), "H5Fcreate succeeded");

    /* Release file-access template */
    ret = H5Pclose(acc_tpl);
    VRFY((ret >= 0), "");


    /* --------------------------
     * Define the dimensions of the overall datasets
     * and create the dataset
     * ------------------------- */
    /* setup 2-D dimensionality object */
    dims[0] = dim0;
    dims[1] = dim1;
    sid = H5Screate_simple (RANK, dims, NULL);
    VRFY((sid >= 0), "H5Screate_simple succeeded");


    /* create a dataset collectively */
    dataset1 = H5Dcreate2(fid, DATASETNAME1, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((dataset1 >= 0), "H5Dcreate2 succeeded");

    /* create another dataset collectively */
    datatype = H5Tcopy(H5T_NATIVE_INT);
    ret = H5Tset_order(datatype, H5T_ORDER_LE);
    VRFY((ret >= 0), "H5Tset_order succeeded");

    dataset2 = H5Dcreate2(fid, DATASETNAME2, datatype, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((dataset2 >= 0), "H5Dcreate2 2 succeeded");

    /* create a third dataset collectively */
    dataset3 = H5Dcreate2(fid, DATASETNAME3, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((dataset3 >= 0), "H5Dcreate2 succeeded");

    /* release 2-D space ID created */
    H5Sclose(sid);

    /* setup scalar dimensionality object */
    sid = H5Screate(H5S_SCALAR);
    VRFY((sid >= 0), "H5Screate succeeded");

    /* create a fourth dataset collectively */
    dataset4 = H5Dcreate2(fid, DATASETNAME4, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    VRFY((dataset4 >= 0), "H5Dcreate2 succeeded");

    /* release scalar space ID created */
    H5Sclose(sid);

    /*
     * Set up dimensions of the slab this process accesses.
     */

    /* Dataset1: each process takes a block of rows. */
    slab_set(mpi_rank, mpi_size, start, count, stride, block, BYROW);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* fill the local slab with some trivial data */
    dataset_fill(start, block, data_array1);
    MESG("data_array initialized");
    if(VERBOSE_MED){
	MESG("data_array created");
	dataset_print(start, block, data_array1);
    }

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "H5Pcreate xfer succeeded");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");
    if(dxfer_coll_type == DXFER_INDEPENDENT_IO) {
     ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist,H5FD_MPIO_INDIVIDUAL_IO);
     VRFY((ret>= 0),"set independent IO collectively succeeded");
    }


    /* write data collectively */
    MESG("writeAll by Row");
    ret = H5Dwrite(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dwrite dataset1 succeeded");

    /* setup dimensions again to writeAll with zero rows for process 0 */
    if(VERBOSE_MED)
	printf("writeAll by some with zero row\n");
    slab_set(mpi_rank, mpi_size, start, count, stride, block, ZROW);
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");
    /* need to make mem_dataspace to match for process 0 */
    if(MAINPROCESS){
	ret = H5Sselect_hyperslab(mem_dataspace, H5S_SELECT_SET, start, stride, count, block);
	VRFY((ret >= 0), "H5Sset_hyperslab mem_dataspace succeeded");
    }
    MESG("writeAll by some with zero row");
    ret = H5Dwrite(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dwrite dataset1 by ZROW succeeded");

    /* release all temporary handles. */
    /* Could have used them for dataset2 but it is cleaner */
    /* to create them again.*/
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);

    /* Dataset2: each process takes a block of columns. */
    slab_set(mpi_rank, mpi_size, start, count, stride, block, BYCOL);

    /* put some trivial data in the data_array */
    dataset_fill(start, block, data_array1);
    MESG("data_array initialized");
    if(VERBOSE_MED){
	MESG("data_array created");
	dataset_print(start, block, data_array1);
    }

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* fill the local slab with some trivial data */
    dataset_fill(start, block, data_array1);
    MESG("data_array initialized");
    if(VERBOSE_MED){
	MESG("data_array created");
	dataset_print(start, block, data_array1);
    }

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pcreate xfer succeeded");
    if(dxfer_coll_type == DXFER_INDEPENDENT_IO) {
      ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist,H5FD_MPIO_INDIVIDUAL_IO);
      VRFY((ret>= 0),"set independent IO collectively succeeded");
    }


    /* write data independently */
    ret = H5Dwrite(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dwrite dataset2 succeeded");

    /* setup dimensions again to writeAll with zero columns for process 0 */
    if(VERBOSE_MED)
	printf("writeAll by some with zero col\n");
    slab_set(mpi_rank, mpi_size, start, count, stride, block, ZCOL);
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");
    /* need to make mem_dataspace to match for process 0 */
    if(MAINPROCESS){
	ret = H5Sselect_hyperslab(mem_dataspace, H5S_SELECT_SET, start, stride, count, block);
	VRFY((ret >= 0), "H5Sset_hyperslab mem_dataspace succeeded");
    }
    MESG("writeAll by some with zero col");
    ret = H5Dwrite(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dwrite dataset1 by ZCOL succeeded");

    /* release all temporary handles. */
    /* Could have used them for dataset3 but it is cleaner */
    /* to create them again.*/
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);


    /* Dataset3: each process takes a block of rows, except process zero uses "none" selection. */
    slab_set(mpi_rank, mpi_size, start, count, stride, block, BYROW);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset3);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    if(MAINPROCESS) {
	ret = H5Sselect_none(file_dataspace);
	VRFY((ret >= 0), "H5Sselect_none file_dataspace succeeded");
    } /* end if */
    else {
        ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
        VRFY((ret >= 0), "H5Sselect_hyperslab succeeded");
    } /* end else */

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");
    if(MAINPROCESS) {
	ret = H5Sselect_none(mem_dataspace);
	VRFY((ret >= 0), "H5Sselect_none mem_dataspace succeeded");
    } /* end if */

    /* fill the local slab with some trivial data */
    dataset_fill(start, block, data_array1);
    MESG("data_array initialized");
    if(VERBOSE_MED) {
	MESG("data_array created");
	dataset_print(start, block, data_array1);
    } /* end if */

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pcreate xfer succeeded");
    if(dxfer_coll_type == DXFER_INDEPENDENT_IO) {
     ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist,H5FD_MPIO_INDIVIDUAL_IO);
     VRFY((ret>= 0),"set independent IO collectively succeeded");
    }


    /* write data collectively */
    MESG("writeAll with none");
    ret = H5Dwrite(dataset3, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dwrite dataset3 succeeded");

    /* write data collectively (with datatype conversion) */
    MESG("writeAll with none");
    ret = H5Dwrite(dataset3, H5T_NATIVE_UCHAR, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dwrite dataset3 succeeded");

    /* release all temporary handles. */
    /* Could have used them for dataset4 but it is cleaner */
    /* to create them again.*/
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);

    /* Dataset4: each process writes no data, except process zero uses "all" selection. */
    /* Additionally, these are in a scalar dataspace */

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset4);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    if(MAINPROCESS) {
	ret = H5Sselect_none(file_dataspace);
	VRFY((ret >= 0), "H5Sselect_all file_dataspace succeeded");
    } /* end if */
    else {
        ret = H5Sselect_all(file_dataspace);
        VRFY((ret >= 0), "H5Sselect_none succeeded");
    } /* end else */

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate(H5S_SCALAR);
    VRFY((mem_dataspace >= 0), "");
    if(MAINPROCESS) {
	ret = H5Sselect_none(mem_dataspace);
	VRFY((ret >= 0), "H5Sselect_all mem_dataspace succeeded");
    } /* end if */
    else {
        ret = H5Sselect_all(mem_dataspace);
        VRFY((ret >= 0), "H5Sselect_none succeeded");
    } /* end else */

    /* fill the local slab with some trivial data */
    dataset_fill(start, block, data_array1);
    MESG("data_array initialized");
    if(VERBOSE_MED) {
	MESG("data_array created");
	dataset_print(start, block, data_array1);
    } /* end if */

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pcreate xfer succeeded");
    if(dxfer_coll_type == DXFER_INDEPENDENT_IO) {
      ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist,H5FD_MPIO_INDIVIDUAL_IO);
      VRFY((ret>= 0),"set independent IO collectively succeeded");
    }



    /* write data collectively */
    MESG("writeAll with scalar dataspace");
    ret = H5Dwrite(dataset4, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dwrite dataset4 succeeded");

    /* write data collectively (with datatype conversion) */
    MESG("writeAll with scalar dataspace");
    ret = H5Dwrite(dataset4, H5T_NATIVE_UCHAR, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dwrite dataset4 succeeded");

    /* release all temporary handles. */
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);

    /*
     * All writes completed.  Close datasets collectively
     */
    ret = H5Dclose(dataset1);
    VRFY((ret >= 0), "H5Dclose1 succeeded");
    ret = H5Dclose(dataset2);
    VRFY((ret >= 0), "H5Dclose2 succeeded");
    ret = H5Dclose(dataset3);
    VRFY((ret >= 0), "H5Dclose3 succeeded");
    ret = H5Dclose(dataset4);
    VRFY((ret >= 0), "H5Dclose3 succeeded");

    /* close the file collectively */
    H5Fclose(fid);

    /* release data buffers */
    if(data_array1) free(data_array1);
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
dataset_readAll(void)
{
    hid_t fid;                  /* HDF5 file ID */
    hid_t acc_tpl;		/* File access templates */
    hid_t xfer_plist;		/* Dataset transfer properties list */
    hid_t file_dataspace;	/* File dataspace ID */
    hid_t mem_dataspace;	/* memory dataspace ID */
    hid_t dataset1, dataset2;	/* Dataset ID */
    hbool_t use_gpfs = FALSE;   /* Use GPFS hints */
    DATATYPE *data_array1 = NULL;	/* data buffer */
    DATATYPE *data_origin1 = NULL; 	/* expected data buffer */
    const char *filename;

    hsize_t start[RANK];			/* for hyperslab setting */
    hsize_t count[RANK], stride[RANK];		/* for hyperslab setting */
    hsize_t block[RANK];			/* for hyperslab setting */

    herr_t ret;         	/* Generic return value */
    int mpi_size, mpi_rank;

    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;

    filename = GetTestParameters();
    if(VERBOSE_MED)
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
    /* setup file access template */
    acc_tpl = create_faccess_plist(comm, info, facc_type, use_gpfs);
    VRFY((acc_tpl >= 0), "");

    /* open the file collectively */
    fid=H5Fopen(filename,H5F_ACC_RDONLY,acc_tpl);
    VRFY((fid >= 0), "H5Fopen succeeded");

    /* Release file-access template */
    ret = H5Pclose(acc_tpl);
    VRFY((ret >= 0), "");


    /* --------------------------
     * Open the datasets in it
     * ------------------------- */
    /* open the dataset1 collectively */
    dataset1 = H5Dopen2(fid, DATASETNAME1, H5P_DEFAULT);
    VRFY((dataset1 >= 0), "H5Dopen2 succeeded");

    /* open another dataset collectively */
    dataset2 = H5Dopen2(fid, DATASETNAME2, H5P_DEFAULT);
    VRFY((dataset2 >= 0), "H5Dopen2 2 succeeded");

    /*
     * Set up dimensions of the slab this process accesses.
     */

    /* Dataset1: each process takes a block of columns. */
    slab_set(mpi_rank, mpi_size, start, count, stride, block, BYCOL);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* fill dataset with test data */
    dataset_fill(start, block, data_origin1);
    MESG("data_array initialized");
    if(VERBOSE_MED){
	MESG("data_array created");
	dataset_print(start, block, data_origin1);
    }

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pcreate xfer succeeded");
    if(dxfer_coll_type == DXFER_INDEPENDENT_IO) {
      ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist,H5FD_MPIO_INDIVIDUAL_IO);
      VRFY((ret>= 0),"set independent IO collectively succeeded");
    }


    /* read data collectively */
    ret = H5Dread(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dread dataset1 succeeded");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, block, data_array1, data_origin1);
    if(ret) nerrors++;

    /* setup dimensions again to readAll with zero columns for process 0 */
    if(VERBOSE_MED)
	printf("readAll by some with zero col\n");
    slab_set(mpi_rank, mpi_size, start, count, stride, block, ZCOL);
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");
    /* need to make mem_dataspace to match for process 0 */
    if(MAINPROCESS){
	ret = H5Sselect_hyperslab(mem_dataspace, H5S_SELECT_SET, start, stride, count, block);
	VRFY((ret >= 0), "H5Sset_hyperslab mem_dataspace succeeded");
    }
    MESG("readAll by some with zero col");
    ret = H5Dread(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dread dataset1 by ZCOL succeeded");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, block, data_array1, data_origin1);
    if(ret) nerrors++;

    /* release all temporary handles. */
    /* Could have used them for dataset2 but it is cleaner */
    /* to create them again.*/
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);

    /* Dataset2: each process takes a block of rows. */
    slab_set(mpi_rank, mpi_size, start, count, stride, block, BYROW);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* fill dataset with test data */
    dataset_fill(start, block, data_origin1);
    MESG("data_array initialized");
    if(VERBOSE_MED){
	MESG("data_array created");
	dataset_print(start, block, data_origin1);
    }

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pcreate xfer succeeded");
    if(dxfer_coll_type == DXFER_INDEPENDENT_IO) {
     ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist,H5FD_MPIO_INDIVIDUAL_IO);
     VRFY((ret>= 0),"set independent IO collectively succeeded");
    }


    /* read data collectively */
    ret = H5Dread(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dread dataset2 succeeded");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, block, data_array1, data_origin1);
    if(ret) nerrors++;

    /* setup dimensions again to readAll with zero rows for process 0 */
    if(VERBOSE_MED)
	printf("readAll by some with zero row\n");
    slab_set(mpi_rank, mpi_size, start, count, stride, block, ZROW);
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");
    /* need to make mem_dataspace to match for process 0 */
    if(MAINPROCESS){
	ret = H5Sselect_hyperslab(mem_dataspace, H5S_SELECT_SET, start, stride, count, block);
	VRFY((ret >= 0), "H5Sset_hyperslab mem_dataspace succeeded");
    }
    MESG("readAll by some with zero row");
    ret = H5Dread(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dread dataset1 by ZROW succeeded");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, block, data_array1, data_origin1);
    if(ret) nerrors++;

    /* release all temporary handles. */
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);


    /*
     * All reads completed.  Close datasets collectively
     */
    ret = H5Dclose(dataset1);
    VRFY((ret >= 0), "H5Dclose1 succeeded");
    ret = H5Dclose(dataset2);
    VRFY((ret >= 0), "H5Dclose2 succeeded");

    /* close the file collectively */
    H5Fclose(fid);

    /* release data buffers */
    if(data_array1) free(data_array1);
    if(data_origin1) free(data_origin1);
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
extend_writeInd(void)
{
    hid_t fid;                  /* HDF5 file ID */
    hid_t acc_tpl;		/* File access templates */
    hid_t sid;   		/* Dataspace ID */
    hid_t file_dataspace;	/* File dataspace ID */
    hid_t mem_dataspace;	/* memory dataspace ID */
    hid_t dataset1, dataset2;	/* Dataset ID */
    hbool_t use_gpfs = FALSE;   /* Use GPFS hints */
    const char *filename;
    hsize_t dims[RANK];   	/* dataset dim sizes */
    hsize_t max_dims[RANK] =
		{H5S_UNLIMITED, H5S_UNLIMITED};	/* dataset maximum dim sizes */
    DATATYPE	*data_array1 = NULL;		/* data buffer */
    hsize_t	chunk_dims[RANK];		/* chunk sizes */
    hid_t	dataset_pl;			/* dataset create prop. list */

    hsize_t	start[RANK];			/* for hyperslab setting */
    hsize_t	count[RANK];			/* for hyperslab setting */
    hsize_t	stride[RANK];			/* for hyperslab setting */
    hsize_t 	block[RANK];			/* for hyperslab setting */

    herr_t ret;         	/* Generic return value */
    int mpi_size, mpi_rank;

    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;

    filename = GetTestParameters();
    if(VERBOSE_MED)
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
    /* setup file access template */
    acc_tpl = create_faccess_plist(comm, info, facc_type, use_gpfs);
    VRFY((acc_tpl >= 0), "");

/* Reduce the number of metadata cache slots, so that there are cache
 * collisions during the raw data I/O on the chunked dataset.  This stresses
 * the metadata cache and tests for cache bugs. -QAK
 */
{
    int mdc_nelmts;
    size_t rdcc_nelmts;
    size_t rdcc_nbytes;
    double rdcc_w0;

    ret = H5Pget_cache(acc_tpl,&mdc_nelmts,&rdcc_nelmts,&rdcc_nbytes,&rdcc_w0);
    VRFY((ret >= 0), "H5Pget_cache succeeded");
    mdc_nelmts=4;
    ret = H5Pset_cache(acc_tpl,mdc_nelmts,rdcc_nelmts,rdcc_nbytes,rdcc_w0);
    VRFY((ret >= 0), "H5Pset_cache succeeded");
}

    /* create the file collectively */
    fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, acc_tpl);
    VRFY((fid >= 0), "H5Fcreate succeeded");

    /* Release file-access template */
    ret = H5Pclose(acc_tpl);
    VRFY((ret >= 0), "");


    /* --------------------------------------------------------------
     * Define the dimensions of the overall datasets and create them.
     * ------------------------------------------------------------- */

    /* set up dataset storage chunk sizes and creation property list */
    if(VERBOSE_MED)
	printf("chunks[]=%lu,%lu\n", (unsigned long)chunk_dims[0], (unsigned long)chunk_dims[1]);
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
    dataset1 = H5Dcreate2(fid, DATASETNAME1, H5T_NATIVE_INT, sid, H5P_DEFAULT, dataset_pl, H5P_DEFAULT);
    VRFY((dataset1 >= 0), "H5Dcreate2 succeeded");

    /* create another extendible dataset collectively */
    dataset2 = H5Dcreate2(fid, DATASETNAME2, H5T_NATIVE_INT, sid, H5P_DEFAULT, dataset_pl, H5P_DEFAULT);
    VRFY((dataset2 >= 0), "H5Dcreate2 succeeded");

    /* release resource */
    H5Sclose(sid);
    H5Pclose(dataset_pl);



    /* -------------------------
     * Test writing to dataset1
     * -------------------------*/
    /* set up dimensions of the slab this process accesses */
    slab_set(mpi_rank, mpi_size, start, count, stride, block, BYROW);

    /* put some trivial data in the data_array */
    dataset_fill(start, block, data_array1);
    MESG("data_array initialized");
    if(VERBOSE_MED) {
	MESG("data_array created");
	dataset_print(start, block, data_array1);
    }

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* Extend its current dim sizes before writing */
    dims[0] = dim0;
    dims[1] = dim1;
    ret = H5Dset_extent(dataset1, dims);
    VRFY((ret >= 0), "H5Dset_extent succeeded");

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
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
    slab_set(mpi_rank, mpi_size, start, count, stride, block, BYCOL);

    /* put some trivial data in the data_array */
    dataset_fill(start, block, data_array1);
    MESG("data_array initialized");
    if(VERBOSE_MED){
	MESG("data_array created");
	dataset_print(start, block, data_array1);
    }

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* Try write to dataset2 beyond its current dim sizes.  Should fail. */
    /* Temporary turn off auto error reporting */
    H5Eget_auto2(H5E_DEFAULT, &old_func, &old_client_data);
    H5Eset_auto2(H5E_DEFAULT, NULL, NULL);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset2);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* write data independently.  Should fail. */
    ret = H5Dwrite(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    H5P_DEFAULT, data_array1);
    VRFY((ret < 0), "H5Dwrite failed as expected");

    /* restore auto error reporting */
    H5Eset_auto2(H5E_DEFAULT, old_func, old_client_data);
    H5Sclose(file_dataspace);

    /* Extend dataset2 and try again.  Should succeed. */
    dims[0] = dim0;
    dims[1] = dim1;
    ret = H5Dset_extent(dataset2, dims);
    VRFY((ret >= 0), "H5Dset_extent succeeded");

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset2);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* write data independently */
    ret = H5Dwrite(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    H5P_DEFAULT, data_array1);
    VRFY((ret >= 0), "H5Dwrite succeeded");

    /* release resource */
    ret = H5Sclose(file_dataspace);
    VRFY((ret >= 0), "H5Sclose succeeded");
    ret = H5Sclose(mem_dataspace);
    VRFY((ret >= 0), "H5Sclose succeeded");


    /* close dataset collectively */
    ret = H5Dclose(dataset1);
    VRFY((ret >= 0), "H5Dclose1 succeeded");
    ret = H5Dclose(dataset2);
    VRFY((ret >= 0), "H5Dclose2 succeeded");

    /* close the file collectively */
    H5Fclose(fid);

    /* release data buffers */
    if(data_array1) free(data_array1);
}

/*
 * Example of using the parallel HDF5 library to create an extendable dataset
 * and perform I/O on it in a way that verifies that the chunk cache is
 * bypassed for parallel I/O.
 */

void
extend_writeInd2(void)
{
    const char *filename;
    hid_t fid;                  /* HDF5 file ID */
    hid_t fapl;			/* File access templates */
    hid_t fs;   		/* File dataspace ID */
    hid_t ms;   		/* Memory dataspace ID */
    hid_t dataset;		/* Dataset ID */
    hbool_t use_gpfs = FALSE;   /* Use GPFS hints */
    hsize_t orig_size=10;   	/* Original dataset dim size */
    hsize_t new_size=20;   	/* Extended dataset dim size */
    hsize_t one=1;
    hsize_t max_size = H5S_UNLIMITED;	/* dataset maximum dim size */
    hsize_t chunk_size = 16384;	/* chunk size */
    hid_t dcpl;	       		/* dataset create prop. list */
    int   written[10],          /* Data to write */
        retrieved[10];          /* Data read in */
    int mpi_size, mpi_rank;     /* MPI settings */
    int i;                      /* Local index variable */
    herr_t ret;         	/* Generic return value */

    filename = GetTestParameters();
    if(VERBOSE_MED)
	printf("Extend independent write test #2 on file %s\n", filename);

    /* set up MPI parameters */
    MPI_Comm_size(MPI_COMM_WORLD,&mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);

    /* -------------------
     * START AN HDF5 FILE
     * -------------------*/
    /* setup file access template */
    fapl = create_faccess_plist(MPI_COMM_WORLD, MPI_INFO_NULL, facc_type, use_gpfs);
    VRFY((fapl >= 0), "create_faccess_plist succeeded");

    /* create the file collectively */
    fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    VRFY((fid >= 0), "H5Fcreate succeeded");

    /* Release file-access template */
    ret = H5Pclose(fapl);
    VRFY((ret >= 0), "H5Pclose succeeded");


    /* --------------------------------------------------------------
     * Define the dimensions of the overall datasets and create them.
     * ------------------------------------------------------------- */

    /* set up dataset storage chunk sizes and creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((dcpl >= 0), "H5Pcreate succeeded");
    ret = H5Pset_chunk(dcpl, 1, &chunk_size);
    VRFY((ret >= 0), "H5Pset_chunk succeeded");

    /* setup dimensionality object */
    fs = H5Screate_simple (1, &orig_size, &max_size);
    VRFY((fs >= 0), "H5Screate_simple succeeded");

    /* create an extendible dataset collectively */
    dataset = H5Dcreate2(fid, DATASETNAME1, H5T_NATIVE_INT, fs, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    VRFY((dataset >= 0), "H5Dcreat2e succeeded");

    /* release resource */
    ret = H5Pclose(dcpl);
    VRFY((ret >= 0), "H5Pclose succeeded");


    /* -------------------------
     * Test writing to dataset
     * -------------------------*/
    /* create a memory dataspace independently */
    ms = H5Screate_simple(1, &orig_size, &max_size);
    VRFY((ms >= 0), "H5Screate_simple succeeded");

    /* put some trivial data in the data_array */
    for(i = 0; i < (int)orig_size; i++)
        written[i] = i;
    MESG("data array initialized");
    if(VERBOSE_MED) {
	MESG("writing at offset zero: ");
        for(i = 0; i < (int)orig_size; i++)
            printf("%s%d", i?", ":"", written[i]);
        printf("\n");
    }
    ret = H5Dwrite(dataset, H5T_NATIVE_INT, ms, fs, H5P_DEFAULT, written);
    VRFY((ret >= 0), "H5Dwrite succeeded");

    /* -------------------------
     * Read initial data from dataset.
     * -------------------------*/
    ret = H5Dread(dataset, H5T_NATIVE_INT, ms, fs, H5P_DEFAULT, retrieved);
    VRFY((ret >= 0), "H5Dread succeeded");
    for (i=0; i<(int)orig_size; i++)
        if(written[i]!=retrieved[i]) {
            printf("Line #%d: written!=retrieved: written[%d]=%d, retrieved[%d]=%d\n",__LINE__,
                i,written[i], i,retrieved[i]);
            nerrors++;
        }
    if(VERBOSE_MED){
	MESG("read at offset zero: ");
        for (i=0; i<(int)orig_size; i++)
            printf("%s%d", i?", ":"", retrieved[i]);
        printf("\n");
    }

    /* -------------------------
     * Extend the dataset & retrieve new dataspace
     * -------------------------*/
    ret = H5Dset_extent(dataset, &new_size);
    VRFY((ret >= 0), "H5Dset_extent succeeded");
    ret = H5Sclose(fs);
    VRFY((ret >= 0), "H5Sclose succeeded");
    fs = H5Dget_space(dataset);
    VRFY((fs >= 0), "H5Dget_space succeeded");

    /* -------------------------
     * Write to the second half of the dataset
     * -------------------------*/
    for (i=0; i<(int)orig_size; i++)
        written[i] = orig_size + i;
    MESG("data array re-initialized");
    if(VERBOSE_MED) {
	MESG("writing at offset 10: ");
        for (i=0; i<(int)orig_size; i++)
            printf("%s%d", i?", ":"", written[i]);
        printf("\n");
    }
    ret = H5Sselect_hyperslab(fs, H5S_SELECT_SET, &orig_size, NULL, &one, &orig_size);
    VRFY((ret >= 0), "H5Sselect_hyperslab succeeded");
    ret = H5Dwrite(dataset, H5T_NATIVE_INT, ms, fs, H5P_DEFAULT, written);
    VRFY((ret >= 0), "H5Dwrite succeeded");

    /* -------------------------
     * Read the new data
     * -------------------------*/
    ret = H5Dread(dataset, H5T_NATIVE_INT, ms, fs, H5P_DEFAULT, retrieved);
    VRFY((ret >= 0), "H5Dread succeeded");
    for (i=0; i<(int)orig_size; i++)
        if(written[i]!=retrieved[i]) {
            printf("Line #%d: written!=retrieved: written[%d]=%d, retrieved[%d]=%d\n",__LINE__,
                i,written[i], i,retrieved[i]);
            nerrors++;
        }
    if(VERBOSE_MED){
	MESG("read at offset 10: ");
        for (i=0; i<(int)orig_size; i++)
            printf("%s%d", i?", ":"", retrieved[i]);
        printf("\n");
    }


    /* Close dataset collectively */
    ret = H5Dclose(dataset);
    VRFY((ret >= 0), "H5Dclose succeeded");

    /* Close the file collectively */
    ret = H5Fclose(fid);
    VRFY((ret >= 0), "H5Fclose succeeded");
}

/* Example of using the parallel HDF5 library to read an extendible dataset */
void
extend_readInd(void)
{
    hid_t fid;			/* HDF5 file ID */
    hid_t acc_tpl;		/* File access templates */
    hid_t file_dataspace;	/* File dataspace ID */
    hid_t mem_dataspace;	/* memory dataspace ID */
    hid_t dataset1, dataset2;	/* Dataset ID */
    hbool_t use_gpfs = FALSE;   /* Use GPFS hints */
    hsize_t dims[RANK];   	/* dataset dim sizes */
    DATATYPE *data_array1 = NULL;	/* data buffer */
    DATATYPE *data_array2 = NULL;	/* data buffer */
    DATATYPE *data_origin1 = NULL; 	/* expected data buffer */
    const char *filename;

    hsize_t start[RANK];			/* for hyperslab setting */
    hsize_t count[RANK], stride[RANK];		/* for hyperslab setting */
    hsize_t block[RANK];			/* for hyperslab setting */

    herr_t ret;         	/* Generic return value */
    int mpi_size, mpi_rank;

    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;

    filename = GetTestParameters();
    if(VERBOSE_MED)
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
    acc_tpl = create_faccess_plist(comm, info, facc_type, use_gpfs);
    VRFY((acc_tpl >= 0), "");

    /* open the file collectively */
    fid=H5Fopen(filename,H5F_ACC_RDONLY,acc_tpl);
    VRFY((fid >= 0), "");

    /* Release file-access template */
    ret = H5Pclose(acc_tpl);
    VRFY((ret >= 0), "");

    /* open the dataset1 collectively */
    dataset1 = H5Dopen2(fid, DATASETNAME1, H5P_DEFAULT);
    VRFY((dataset1 >= 0), "");

    /* open another dataset collectively */
    dataset2 = H5Dopen2(fid, DATASETNAME1, H5P_DEFAULT);
    VRFY((dataset2 >= 0), "");

    /* Try extend dataset1 which is open RDONLY.  Should fail. */
    /* first turn off auto error reporting */
    H5Eget_auto2(H5E_DEFAULT, &old_func, &old_client_data);
    H5Eset_auto2(H5E_DEFAULT, NULL, NULL);

    file_dataspace = H5Dget_space (dataset1);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sget_simple_extent_dims(file_dataspace, dims, NULL);
    VRFY((ret > 0), "H5Sget_simple_extent_dims succeeded");
    dims[0]++;
    ret = H5Dset_extent(dataset1, dims);
    VRFY((ret < 0), "H5Dset_extent failed as expected");

    /* restore auto error reporting */
    H5Eset_auto2(H5E_DEFAULT, old_func, old_client_data);
    H5Sclose(file_dataspace);


    /* Read dataset1 using BYROW pattern */
    /* set up dimensions of the slab this process accesses */
    slab_set(mpi_rank, mpi_size, start, count, stride, block, BYROW);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);
    VRFY((file_dataspace >= 0), "");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* fill dataset with test data */
    dataset_fill(start, block, data_origin1);
    if(VERBOSE_MED){
	MESG("data_array created");
	dataset_print(start, block, data_array1);
    }

    /* read data independently */
    ret = H5Dread(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    H5P_DEFAULT, data_array1);
    VRFY((ret >= 0), "H5Dread succeeded");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, block, data_array1, data_origin1);
    VRFY((ret == 0), "dataset1 read verified correct");
    if(ret) nerrors++;

    H5Sclose(mem_dataspace);
    H5Sclose(file_dataspace);


    /* Read dataset2 using BYCOL pattern */
    /* set up dimensions of the slab this process accesses */
    slab_set(mpi_rank, mpi_size, start, count, stride, block, BYCOL);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset2);
    VRFY((file_dataspace >= 0), "");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* fill dataset with test data */
    dataset_fill(start, block, data_origin1);
    if(VERBOSE_MED){
	MESG("data_array created");
	dataset_print(start, block, data_array1);
    }

    /* read data independently */
    ret = H5Dread(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    H5P_DEFAULT, data_array1);
    VRFY((ret >= 0), "H5Dread succeeded");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, block, data_array1, data_origin1);
    VRFY((ret == 0), "dataset2 read verified correct");
    if(ret) nerrors++;

    H5Sclose(mem_dataspace);
    H5Sclose(file_dataspace);

    /* close dataset collectively */
    ret = H5Dclose(dataset1);
    VRFY((ret >= 0), "");
    ret = H5Dclose(dataset2);
    VRFY((ret >= 0), "");


    /* close the file collectively */
    H5Fclose(fid);

    /* release data buffers */
    if(data_array1) free(data_array1);
    if(data_array2) free(data_array2);
    if(data_origin1) free(data_origin1);
}

/*
 * Part 3--Collective read/write for extendible datasets.
 */

/*
 * Example of using the parallel HDF5 library to create two extendible
 * datasets in one HDF5 file with collective parallel MPIO access support.
 * The Datasets are of sizes (number-of-mpi-processes x dim0) x dim1.
 * Each process controls only a slab of size dim0 x dim1 within each
 * dataset.
 */

void
extend_writeAll(void)
{
    hid_t fid;                  /* HDF5 file ID */
    hid_t acc_tpl;		/* File access templates */
    hid_t xfer_plist;		/* Dataset transfer properties list */
    hid_t sid;   		/* Dataspace ID */
    hid_t file_dataspace;	/* File dataspace ID */
    hid_t mem_dataspace;	/* memory dataspace ID */
    hid_t dataset1, dataset2;	/* Dataset ID */
    hbool_t use_gpfs = FALSE;   /* Use GPFS hints */
    const char *filename;
    hsize_t dims[RANK];   	/* dataset dim sizes */
    hsize_t max_dims[RANK] =
		{H5S_UNLIMITED, H5S_UNLIMITED};	/* dataset maximum dim sizes */
    DATATYPE	*data_array1 = NULL;		/* data buffer */
    hsize_t	chunk_dims[RANK];		/* chunk sizes */
    hid_t	dataset_pl;			/* dataset create prop. list */

    hsize_t	start[RANK];			/* for hyperslab setting */
    hsize_t	count[RANK];			/* for hyperslab setting */
    hsize_t	stride[RANK];			/* for hyperslab setting */
    hsize_t 	block[RANK];			/* for hyperslab setting */

    herr_t ret;         	/* Generic return value */
    int mpi_size, mpi_rank;

    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;

    filename = GetTestParameters();
    if(VERBOSE_MED)
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
    /* setup file access template */
    acc_tpl = create_faccess_plist(comm, info, facc_type, use_gpfs);
    VRFY((acc_tpl >= 0), "");

/* Reduce the number of metadata cache slots, so that there are cache
 * collisions during the raw data I/O on the chunked dataset.  This stresses
 * the metadata cache and tests for cache bugs. -QAK
 */
{
    int mdc_nelmts;
    size_t rdcc_nelmts;
    size_t rdcc_nbytes;
    double rdcc_w0;

    ret = H5Pget_cache(acc_tpl,&mdc_nelmts,&rdcc_nelmts,&rdcc_nbytes,&rdcc_w0);
    VRFY((ret >= 0), "H5Pget_cache succeeded");
    mdc_nelmts=4;
    ret = H5Pset_cache(acc_tpl,mdc_nelmts,rdcc_nelmts,rdcc_nbytes,rdcc_w0);
    VRFY((ret >= 0), "H5Pset_cache succeeded");
}

    /* create the file collectively */
    fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, acc_tpl);
    VRFY((fid >= 0), "H5Fcreate succeeded");

    /* Release file-access template */
    ret = H5Pclose(acc_tpl);
    VRFY((ret >= 0), "");


    /* --------------------------------------------------------------
     * Define the dimensions of the overall datasets and create them.
     * ------------------------------------------------------------- */

    /* set up dataset storage chunk sizes and creation property list */
    if(VERBOSE_MED)
	printf("chunks[]=%lu,%lu\n", (unsigned long)chunk_dims[0], (unsigned long)chunk_dims[1]);
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
    dataset1 = H5Dcreate2(fid, DATASETNAME1, H5T_NATIVE_INT, sid, H5P_DEFAULT, dataset_pl, H5P_DEFAULT);
    VRFY((dataset1 >= 0), "H5Dcreate2 succeeded");

    /* create another extendible dataset collectively */
    dataset2 = H5Dcreate2(fid, DATASETNAME2, H5T_NATIVE_INT, sid, H5P_DEFAULT, dataset_pl, H5P_DEFAULT);
    VRFY((dataset2 >= 0), "H5Dcreate2 succeeded");

    /* release resource */
    H5Sclose(sid);
    H5Pclose(dataset_pl);



    /* -------------------------
     * Test writing to dataset1
     * -------------------------*/
    /* set up dimensions of the slab this process accesses */
    slab_set(mpi_rank, mpi_size, start, count, stride, block, BYROW);

    /* put some trivial data in the data_array */
    dataset_fill(start, block, data_array1);
    MESG("data_array initialized");
    if(VERBOSE_MED) {
	MESG("data_array created");
	dataset_print(start, block, data_array1);
    }

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* Extend its current dim sizes before writing */
    dims[0] = dim0;
    dims[1] = dim1;
    ret = H5Dset_extent(dataset1, dims);
    VRFY((ret >= 0), "H5Dset_extent succeeded");

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "H5Pcreate xfer succeeded");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");
    if(dxfer_coll_type == DXFER_INDEPENDENT_IO) {
     ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist,H5FD_MPIO_INDIVIDUAL_IO);
     VRFY((ret>= 0),"set independent IO collectively succeeded");
    }


    /* write data collectively */
    ret = H5Dwrite(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dwrite succeeded");

    /* release resource */
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);


    /* -------------------------
     * Test writing to dataset2
     * -------------------------*/
    /* set up dimensions of the slab this process accesses */
    slab_set(mpi_rank, mpi_size, start, count, stride, block, BYCOL);

    /* put some trivial data in the data_array */
    dataset_fill(start, block, data_array1);
    MESG("data_array initialized");
    if(VERBOSE_MED){
	MESG("data_array created");
	dataset_print(start, block, data_array1);
    }

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "H5Pcreate xfer succeeded");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");
    if(dxfer_coll_type == DXFER_INDEPENDENT_IO) {
     ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist,H5FD_MPIO_INDIVIDUAL_IO);
     VRFY((ret>= 0),"set independent IO collectively succeeded");
    }


    /* Try write to dataset2 beyond its current dim sizes.  Should fail. */
    /* Temporary turn off auto error reporting */
    H5Eget_auto2(H5E_DEFAULT, &old_func, &old_client_data);
    H5Eset_auto2(H5E_DEFAULT, NULL, NULL);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset2);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* write data independently.  Should fail. */
    ret = H5Dwrite(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);
    VRFY((ret < 0), "H5Dwrite failed as expected");

    /* restore auto error reporting */
    H5Eset_auto2(H5E_DEFAULT, old_func, old_client_data);
    H5Sclose(file_dataspace);

    /* Extend dataset2 and try again.  Should succeed. */
    dims[0] = dim0;
    dims[1] = dim1;
    ret = H5Dset_extent(dataset2, dims);
    VRFY((ret >= 0), "H5Dset_extent succeeded");

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset2);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* write data independently */
    ret = H5Dwrite(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dwrite succeeded");

    /* release resource */
    ret = H5Sclose(file_dataspace);
    VRFY((ret >= 0), "H5Sclose succeeded");
    ret = H5Sclose(mem_dataspace);
    VRFY((ret >= 0), "H5Sclose succeeded");
    ret = H5Pclose(xfer_plist);
    VRFY((ret >= 0), "H5Pclose succeeded");


    /* close dataset collectively */
    ret = H5Dclose(dataset1);
    VRFY((ret >= 0), "H5Dclose1 succeeded");
    ret = H5Dclose(dataset2);
    VRFY((ret >= 0), "H5Dclose2 succeeded");

    /* close the file collectively */
    H5Fclose(fid);

    /* release data buffers */
    if(data_array1) free(data_array1);
}

/* Example of using the parallel HDF5 library to read an extendible dataset */
void
extend_readAll(void)
{
    hid_t fid;			/* HDF5 file ID */
    hid_t acc_tpl;		/* File access templates */
    hid_t xfer_plist;		/* Dataset transfer properties list */
    hid_t file_dataspace;	/* File dataspace ID */
    hid_t mem_dataspace;	/* memory dataspace ID */
    hid_t dataset1, dataset2;	/* Dataset ID */
    hbool_t use_gpfs = FALSE;   /* Use GPFS hints */
    const char *filename;
    hsize_t dims[RANK];   	/* dataset dim sizes */
    DATATYPE *data_array1 = NULL;	/* data buffer */
    DATATYPE *data_array2 = NULL;	/* data buffer */
    DATATYPE *data_origin1 = NULL; 	/* expected data buffer */

    hsize_t start[RANK];			/* for hyperslab setting */
    hsize_t count[RANK], stride[RANK];		/* for hyperslab setting */
    hsize_t block[RANK];			/* for hyperslab setting */

    herr_t ret;         	/* Generic return value */
    int mpi_size, mpi_rank;

    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;

    filename = GetTestParameters();
    if(VERBOSE_MED)
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
    acc_tpl = create_faccess_plist(comm, info, facc_type, use_gpfs);
    VRFY((acc_tpl >= 0), "");

    /* open the file collectively */
    fid=H5Fopen(filename,H5F_ACC_RDONLY,acc_tpl);
    VRFY((fid >= 0), "");

    /* Release file-access template */
    ret = H5Pclose(acc_tpl);
    VRFY((ret >= 0), "");

    /* open the dataset1 collectively */
    dataset1 = H5Dopen2(fid, DATASETNAME1, H5P_DEFAULT);
    VRFY((dataset1 >= 0), "");

    /* open another dataset collectively */
    dataset2 = H5Dopen2(fid, DATASETNAME1, H5P_DEFAULT);
    VRFY((dataset2 >= 0), "");

    /* Try extend dataset1 which is open RDONLY.  Should fail. */
    /* first turn off auto error reporting */
    H5Eget_auto2(H5E_DEFAULT, &old_func, &old_client_data);
    H5Eset_auto2(H5E_DEFAULT, NULL, NULL);

    file_dataspace = H5Dget_space (dataset1);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sget_simple_extent_dims(file_dataspace, dims, NULL);
    VRFY((ret > 0), "H5Sget_simple_extent_dims succeeded");
    dims[0]++;
    ret = H5Dset_extent(dataset1, dims);
    VRFY((ret < 0), "H5Dset_extent failed as expected");

    /* restore auto error reporting */
    H5Eset_auto2(H5E_DEFAULT, old_func, old_client_data);
    H5Sclose(file_dataspace);


    /* Read dataset1 using BYROW pattern */
    /* set up dimensions of the slab this process accesses */
    slab_set(mpi_rank, mpi_size, start, count, stride, block, BYROW);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);
    VRFY((file_dataspace >= 0), "");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* fill dataset with test data */
    dataset_fill(start, block, data_origin1);
    if(VERBOSE_MED){
	MESG("data_array created");
	dataset_print(start, block, data_array1);
    }

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "H5Pcreate xfer succeeded");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");
    if(dxfer_coll_type == DXFER_INDEPENDENT_IO) {
     ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist,H5FD_MPIO_INDIVIDUAL_IO);
     VRFY((ret>= 0),"set independent IO collectively succeeded");
    }


    /* read data collectively */
    ret = H5Dread(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dread succeeded");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, block, data_array1, data_origin1);
    VRFY((ret == 0), "dataset1 read verified correct");
    if(ret) nerrors++;

    H5Sclose(mem_dataspace);
    H5Sclose(file_dataspace);
    H5Pclose(xfer_plist);


    /* Read dataset2 using BYCOL pattern */
    /* set up dimensions of the slab this process accesses */
    slab_set(mpi_rank, mpi_size, start, count, stride, block, BYCOL);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset2);
    VRFY((file_dataspace >= 0), "");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* fill dataset with test data */
    dataset_fill(start, block, data_origin1);
    if(VERBOSE_MED){
	MESG("data_array created");
	dataset_print(start, block, data_array1);
    }

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "H5Pcreate xfer succeeded");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");
    if(dxfer_coll_type == DXFER_INDEPENDENT_IO) {
     ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist,H5FD_MPIO_INDIVIDUAL_IO);
     VRFY((ret>= 0),"set independent IO collectively succeeded");
    }


    /* read data collectively */
    ret = H5Dread(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);
    VRFY((ret >= 0), "H5Dread succeeded");

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, stride, block, data_array1, data_origin1);
    VRFY((ret == 0), "dataset2 read verified correct");
    if(ret) nerrors++;

    H5Sclose(mem_dataspace);
    H5Sclose(file_dataspace);
    H5Pclose(xfer_plist);

    /* close dataset collectively */
    ret = H5Dclose(dataset1);
    VRFY((ret >= 0), "");
    ret = H5Dclose(dataset2);
    VRFY((ret >= 0), "");


    /* close the file collectively */
    H5Fclose(fid);

    /* release data buffers */
    if(data_array1) free(data_array1);
    if(data_array2) free(data_array2);
    if(data_origin1) free(data_origin1);
}

/*
 * Example of using the parallel HDF5 library to read a compressed
 * dataset in an HDF5 file with collective parallel access support.
 */

#ifdef H5_HAVE_FILTER_DEFLATE
void
compress_readAll(void)
{
    hid_t fid;                  /* HDF5 file ID */
    hid_t acc_tpl;		/* File access templates */
    hid_t dcpl;                 /* Dataset creation property list */
    hid_t xfer_plist;		/* Dataset transfer properties list */
    hid_t dataspace;		/* Dataspace ID */
    hid_t dataset;		/* Dataset ID */
    int rank=1;                 /* Dataspace rank */
    hsize_t dim=dim0;           /* Dataspace dimensions */
    unsigned u;                 /* Local index variable */
    hbool_t use_gpfs = FALSE;   /* Use GPFS hints */
    DATATYPE *data_read = NULL;	/* data buffer */
    DATATYPE *data_orig = NULL; /* expected data buffer */
    const char *filename;
    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;
    int mpi_size, mpi_rank;
    herr_t ret;         	/* Generic return value */

    filename = GetTestParameters();
    if(VERBOSE_MED)
	printf("Collective chunked dataset read test on file %s\n", filename);

    /* Retrieve MPI parameters */
    MPI_Comm_size(comm,&mpi_size);
    MPI_Comm_rank(comm,&mpi_rank);

    /* Allocate data buffer */
    data_orig = (DATATYPE *)HDmalloc((size_t)dim*sizeof(DATATYPE));
    VRFY((data_orig != NULL), "data_origin1 malloc succeeded");
    data_read = (DATATYPE *)HDmalloc((size_t)dim*sizeof(DATATYPE));
    VRFY((data_read != NULL), "data_array1 malloc succeeded");

    /* Initialize data buffers */
    for(u=0; u<dim;u++)
        data_orig[u]=u;

    /* Process zero creates the file with a compressed, chunked dataset */
    if(mpi_rank==0) {
        hsize_t chunk_dim;           /* Chunk dimensions */

        /* Create the file */
        fid = H5Fcreate(h5_rmprefix(filename), H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
        VRFY((fid > 0), "H5Fcreate succeeded");

        /* Create property list for chunking and compression */
        dcpl = H5Pcreate(H5P_DATASET_CREATE);
        VRFY((dcpl > 0), "H5Pcreate succeeded");

        ret = H5Pset_layout(dcpl, H5D_CHUNKED);
        VRFY((ret >= 0), "H5Pset_layout succeeded");

        /* Use eight chunks */
        chunk_dim = dim / 8;
        ret = H5Pset_chunk(dcpl, rank, &chunk_dim);
        VRFY((ret >= 0), "H5Pset_chunk succeeded");

        ret = H5Pset_deflate(dcpl, 9);
        VRFY((ret >= 0), "H5Pset_deflate succeeded");

        /* Create dataspace */
        dataspace = H5Screate_simple(rank, &dim, NULL);
        VRFY((dataspace > 0), "H5Screate_simple succeeded");

        /* Create dataset */
        dataset = H5Dcreate2(fid, "compressed_data", H5T_NATIVE_INT, dataspace, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        VRFY((dataset > 0), "H5Dcreate2 succeeded");

        /* Write compressed data */
        ret = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data_orig);
        VRFY((ret >= 0), "H5Dwrite succeeded");

        /* Close objects */
        ret = H5Pclose(dcpl);
        VRFY((ret >= 0), "H5Pclose succeeded");
        ret = H5Sclose(dataspace);
        VRFY((ret >= 0), "H5Sclose succeeded");
        ret = H5Dclose(dataset);
        VRFY((ret >= 0), "H5Dclose succeeded");
        ret = H5Fclose(fid);
        VRFY((ret >= 0), "H5Fclose succeeded");
    }

    /* Wait for file to be created */
    MPI_Barrier(comm);

    /* -------------------
     * OPEN AN HDF5 FILE
     * -------------------*/

    /* setup file access template */
    acc_tpl = create_faccess_plist(comm, info, facc_type, use_gpfs);
    VRFY((acc_tpl >= 0), "");

    /* open the file collectively */
    fid=H5Fopen(filename,H5F_ACC_RDWR,acc_tpl);
    VRFY((fid > 0), "H5Fopen succeeded");

    /* Release file-access template */
    ret = H5Pclose(acc_tpl);
    VRFY((ret >= 0), "H5Pclose succeeded");


    /* Open dataset with compressed chunks */
    dataset = H5Dopen2(fid, "compressed_data", H5P_DEFAULT);
    VRFY((dataset > 0), "H5Dopen2 succeeded");

    /* Try reading & writing data */
    if(dataset>0) {
        /* Create dataset transfer property list */
        xfer_plist = H5Pcreate(H5P_DATASET_XFER);
        VRFY((xfer_plist > 0), "H5Pcreate succeeded");

        ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
        VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");
        if(dxfer_coll_type == DXFER_INDEPENDENT_IO) {
          ret = H5Pset_dxpl_mpio_collective_opt(xfer_plist,H5FD_MPIO_INDIVIDUAL_IO);
          VRFY((ret>= 0),"set independent IO collectively succeeded");
        }


        /* Try reading the data */
        ret = H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, xfer_plist, data_read);
        VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");

        /* Verify data read */
        for(u=0; u<dim; u++)
            if(data_orig[u]!=data_read[u]) {
                printf("Line #%d: written!=retrieved: data_orig[%u]=%d, data_read[%u]=%d\n",__LINE__,
                    (unsigned)u,data_orig[u],(unsigned)u,data_read[u]);
                nerrors++;
            }

        /* Writing to the compressed, chunked dataset in parallel should fail */
        H5E_BEGIN_TRY {
            ret = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, xfer_plist, data_read);
        } H5E_END_TRY;
        VRFY((ret < 0), "H5Dwrite failed");

        ret = H5Pclose(xfer_plist);
        VRFY((ret >= 0), "H5Pclose succeeded");
        ret = H5Dclose(dataset);
        VRFY((ret >= 0), "H5Dclose succeeded");
    } /* end if */

    ret = H5Fclose(fid);
    VRFY((ret >= 0), "H5Fclose succeeded");

    /* release data buffers */
    if(data_read) HDfree(data_read);
    if(data_orig) HDfree(data_orig);
}
#endif /* H5_HAVE_FILTER_DEFLATE */

/*
 * Part 4--Non-selection for chunked dataset
 */

/*
 * Example of using the parallel HDF5 library to create chunked
 * dataset in one HDF5 file with collective and independent parallel
 * MPIO access support.  The Datasets are of sizes dim0 x dim1.
 * Each process controls only a slab of size dim0 x dim1 within the
 * dataset with the exception that one processor selects no element.
 */

void
none_selection_chunk(void)
{
    hid_t fid;                  /* HDF5 file ID */
    hid_t acc_tpl;		/* File access templates */
    hid_t xfer_plist;		/* Dataset transfer properties list */
    hid_t sid;   		/* Dataspace ID */
    hid_t file_dataspace;	/* File dataspace ID */
    hid_t mem_dataspace;	/* memory dataspace ID */
    hid_t dataset1, dataset2;	/* Dataset ID */
    hbool_t use_gpfs = FALSE;   /* Use GPFS hints */
    const char *filename;
    hsize_t dims[RANK];   	/* dataset dim sizes */
    DATATYPE	*data_origin = NULL;		/* data buffer */
    DATATYPE	*data_array = NULL;		/* data buffer */
    hsize_t	chunk_dims[RANK];		/* chunk sizes */
    hid_t	dataset_pl;			/* dataset create prop. list */

    hsize_t	start[RANK];			/* for hyperslab setting */
    hsize_t	count[RANK];			/* for hyperslab setting */
    hsize_t	stride[RANK];			/* for hyperslab setting */
    hsize_t 	block[RANK];			/* for hyperslab setting */
    hsize_t	mstart[RANK];			/* for data buffer in memory */

    herr_t ret;         	/* Generic return value */
    int mpi_size, mpi_rank;

    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;

    filename = GetTestParameters();
    if(VERBOSE_MED)
	printf("Extend independent write test on file %s\n", filename);

    /* set up MPI parameters */
    MPI_Comm_size(MPI_COMM_WORLD,&mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);

    /* setup chunk-size. Make sure sizes are > 0 */
    chunk_dims[0] = chunkdim0;
    chunk_dims[1] = chunkdim1;

    /* -------------------
     * START AN HDF5 FILE
     * -------------------*/
    /* setup file access template */
    acc_tpl = create_faccess_plist(comm, info, facc_type, use_gpfs);
    VRFY((acc_tpl >= 0), "");

    /* create the file collectively */
    fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, acc_tpl);
    VRFY((fid >= 0), "H5Fcreate succeeded");

    /* Release file-access template */
    ret = H5Pclose(acc_tpl);
    VRFY((ret >= 0), "");

    /* --------------------------------------------------------------
     * Define the dimensions of the overall datasets and create them.
     * ------------------------------------------------------------- */

    /* set up dataset storage chunk sizes and creation property list */
    if(VERBOSE_MED)
	printf("chunks[]=%lu,%lu\n", (unsigned long)chunk_dims[0], (unsigned long)chunk_dims[1]);
    dataset_pl = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((dataset_pl >= 0), "H5Pcreate succeeded");
    ret = H5Pset_chunk(dataset_pl, RANK, chunk_dims);
    VRFY((ret >= 0), "H5Pset_chunk succeeded");

    /* setup dimensionality object */
    dims[0] = dim0;
    dims[1] = dim1;
    sid = H5Screate_simple(RANK, dims, NULL);
    VRFY((sid >= 0), "H5Screate_simple succeeded");

    /* create an extendible dataset collectively */
    dataset1 = H5Dcreate2(fid, DATASETNAME1, H5T_NATIVE_INT, sid, H5P_DEFAULT, dataset_pl, H5P_DEFAULT);
    VRFY((dataset1 >= 0), "H5Dcreate2 succeeded");

    /* create another extendible dataset collectively */
    dataset2 = H5Dcreate2(fid, DATASETNAME2, H5T_NATIVE_INT, sid, H5P_DEFAULT, dataset_pl, H5P_DEFAULT);
    VRFY((dataset2 >= 0), "H5Dcreate2 succeeded");

    /* release resource */
    H5Sclose(sid);
    H5Pclose(dataset_pl);

    /* -------------------------
     * Test collective writing to dataset1
     * -------------------------*/
    /* set up dimensions of the slab this process accesses */
    slab_set(mpi_rank, mpi_size, start, count, stride, block, BYROW);

    /* allocate memory for data buffer. Only allocate enough buffer for
     * each processor's data. */
    if(mpi_rank) {
        data_origin = (DATATYPE *)malloc(block[0]*block[1]*sizeof(DATATYPE));
        VRFY((data_origin != NULL), "data_origin malloc succeeded");

        data_array = (DATATYPE *)malloc(block[0]*block[1]*sizeof(DATATYPE));
        VRFY((data_array != NULL), "data_array malloc succeeded");

        /* put some trivial data in the data_array */
        mstart[0] = mstart[1] = 0;
        dataset_fill(mstart, block, data_origin);
        MESG("data_array initialized");
        if(VERBOSE_MED){
	    MESG("data_array created");
	    dataset_print(mstart, block, data_origin);
        }
    }

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (RANK, block, NULL);
    VRFY((mem_dataspace >= 0), "");

    /* Process 0 has no selection */
    if(!mpi_rank) {
        ret = H5Sselect_none(mem_dataspace);
        VRFY((ret >= 0), "H5Sselect_none succeeded");
    }

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);
    VRFY((file_dataspace >= 0), "H5Dget_space succeeded");
    ret = H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");

    /* Process 0 has no selection */
    if(!mpi_rank) {
        ret = H5Sselect_none(file_dataspace);
        VRFY((ret >= 0), "H5Sselect_none succeeded");
    }

    /* set up the collective transfer properties list */
    xfer_plist = H5Pcreate (H5P_DATASET_XFER);
    VRFY((xfer_plist >= 0), "H5Pcreate xfer succeeded");
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_COLLECTIVE);
    VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");

    /* write data collectively */
    ret = H5Dwrite(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_origin);
    VRFY((ret >= 0), "H5Dwrite succeeded");

    /* read data independently */
    ret = H5Dread(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    H5P_DEFAULT, data_array);
    VRFY((ret >= 0), "");

    /* verify the read data with original expected data */
    if(mpi_rank) {
        ret = dataset_vrfy(mstart, count, stride, block, data_array, data_origin);
        if(ret) nerrors++;
    }

    /* -------------------------
     * Test independent writing to dataset2
     * -------------------------*/
    ret = H5Pset_dxpl_mpio(xfer_plist, H5FD_MPIO_INDEPENDENT);
    VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");

    /* write data collectively */
    ret = H5Dwrite(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_origin);
    VRFY((ret >= 0), "H5Dwrite succeeded");

    /* read data independently */
    ret = H5Dread(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    H5P_DEFAULT, data_array);
    VRFY((ret >= 0), "");

    /* verify the read data with original expected data */
    if(mpi_rank) {
        ret = dataset_vrfy(mstart, count, stride, block, data_array, data_origin);
        if(ret) nerrors++;
    }

    /* release resource */
    ret = H5Sclose(file_dataspace);
    VRFY((ret >= 0), "H5Sclose succeeded");
    ret = H5Sclose(mem_dataspace);
    VRFY((ret >= 0), "H5Sclose succeeded");
    ret = H5Pclose(xfer_plist);
    VRFY((ret >= 0), "H5Pclose succeeded");


    /* close dataset collectively */
    ret = H5Dclose(dataset1);
    VRFY((ret >= 0), "H5Dclose1 succeeded");
    ret = H5Dclose(dataset2);
    VRFY((ret >= 0), "H5Dclose2 succeeded");

    /* close the file collectively */
    H5Fclose(fid);

    /* release data buffers */
    if(data_origin) free(data_origin);
    if(data_array) free(data_array);
}


/* Function: test_actual_io_mode
 *
 * Purpose: tests one specific case of collective I/O and checks that the 
 *          actual_chunk_opt_mode property and the actual_io_mode
 *          properties in the DXPL have the correct values.
 *
 * Input:   selection_mode: changes the way processes select data from the space, as well
 *          as some dxpl flags to get collective I/O to break in different ways.
 *          
 *          The relevant I/O function and expected response for each mode:
 *              TEST_ACTUAL_IO_MULTI_CHUNK_IND:
 *                  H5D_mpi_chunk_collective_io, each process reports independent I/O
 *              
 *              TEST_ACTUAL_IO_MULTI_CHUNK_COL:
 *                  H5D_mpi_chunk_collective_io, each process reports collective I/O
 *
 *              TEST_ACTUAL_IO_MULTI_CHUNK_MIX:
 *                  H5D_mpi_chunk_collective_io, each process reports mixed I/O
 *
 *              TEST_ACTUAL_IO_MULTI_CHUNK_MIX_DISAGREE:
 *                  H5D_mpi_chunk_collective_io, processes disagree. The root reports
 *                  collective, the rest report independent I/O
 *
 *              TEST_ACTUAL_IO_MULTI_CHUNK_NO_OPT_COL:
 *                  H5D_mpi_chunk_collective_io_no_opt, each process reports collective I/O
 *
 *              TEST_ACTUAL_IO_MULTI_CHUNK_NO_OPT_MIX_DISAGREE:
 *                  H5D_mpi_chunk_collective_io_no_opt, processes disagree
 *                  (collective and mixed I/O)
 *
 *              TEST_ACTUAL_IO_LINK_CHUNK:
 *                  H5D_link_chunk_collective_io, processes report linked chunk I/O
 *
 *              TEST_ACTUAL_IO_CONTIGUOUS:
 *                  H5D_contig_collective_write or H5D_contig_collective_read
 *                  each process reports contiguous collective I/O
 *
 *              TEST_ACTUAL_IO_NO_COLLECTIVE:
 *                  Simple independent I/O. This tests that the defaults are properly set.
 *
 *              TEST_ACTUAL_IO_RESET:
 *                  Perfroms collective and then independent I/O wit hthe same dxpl to
 *                  make sure the peroperty is correctly reset to the default on each use.
 *                  Specifically, this test runs TEST_ACTUAL_IO_MULTI_CHUNK_NO_OPT_MIX_DISAGREE
 *                  (The most complex case that works on all builds) and then performs
 *                  an independent read and write with the same dxpls.
 *
 *          It may seem like TEST_ACTUAL_IO_MULTI_CHUNK_NO_OPT_IND and 
 *          TEST_ACTUAL_IO_MULTI_CHUNK_NO_OPT_MIX have been accidentally
 *          left out. This is intentional; the other test cases sufficiently
 *          cover all cases for Multi Chunk No Opt I/O.
 * 
 * Programmer: Jacob Gruber
 * Date: 2011-04-06
 */
static void 
test_actual_io_mode(int selection_mode) {
    H5D_mpio_actual_chunk_opt_mode_t   actual_chunk_opt_mode_write = -1;
    H5D_mpio_actual_chunk_opt_mode_t   actual_chunk_opt_mode_read = -1;
    H5D_mpio_actual_chunk_opt_mode_t   actual_chunk_opt_mode_expected = -1;
    H5D_mpio_actual_io_mode_t   actual_io_mode_write = -1;
    H5D_mpio_actual_io_mode_t   actual_io_mode_read = -1;
    H5D_mpio_actual_io_mode_t   actual_io_mode_expected = -1;
    const char  * filename;
    const char  * test_name;
    hbool_t     multi_chunk_no_opt;
    hbool_t     multi_chunk_with_opt;
    hbool_t     is_chunked;
    hbool_t     is_collective;
    int         mpi_size = -1; 
    int         mpi_rank = -1;
    int         length;
    int         * buffer;
    int         i;
    MPI_Comm    mpi_comm = MPI_COMM_NULL;
    MPI_Info    mpi_info = MPI_INFO_NULL;
    hid_t       fid = -1;
    hid_t       sid = -1;
    hid_t       dataset = -1;
    hid_t       data_type = H5T_NATIVE_INT;
    hid_t       fapl = -1;
    hid_t       mem_space = -1;
    hid_t       file_space = -1;
    hid_t       dcpl = -1;
    hid_t       dxpl_write = -1;
    hid_t       dxpl_read = -1;
    hsize_t     dims[RANK];
    hsize_t     chunk_dims[RANK];
    hsize_t     start[RANK];
    hsize_t     stride[RANK];
    hsize_t     count[RANK];
    hsize_t     block[RANK];
    hbool_t     use_gpfs = FALSE;
    herr_t      ret;
   
    /* Set up some flags to make some future if statements slightly more readable */
    multi_chunk_no_opt = (
        selection_mode == TEST_ACTUAL_IO_MULTI_CHUNK_NO_OPT_IND ||
        selection_mode == TEST_ACTUAL_IO_MULTI_CHUNK_NO_OPT_COL ||
        selection_mode == TEST_ACTUAL_IO_MULTI_CHUNK_NO_OPT_MIX_DISAGREE );
    
    /* Note: RESET performs the same tests as MULTI_CHUNK_MIX_DISAGREE and then
     * tests independent I/O
     */
    multi_chunk_with_opt = (
        selection_mode == TEST_ACTUAL_IO_MULTI_CHUNK_IND ||
        selection_mode == TEST_ACTUAL_IO_MULTI_CHUNK_COL ||
        selection_mode == TEST_ACTUAL_IO_MULTI_CHUNK_MIX ||
        selection_mode == TEST_ACTUAL_IO_MULTI_CHUNK_MIX_DISAGREE ||
        selection_mode == TEST_ACTUAL_IO_RESET );
 
    is_chunked = (
        selection_mode != TEST_ACTUAL_IO_CONTIGUOUS &&
        selection_mode != TEST_ACTUAL_IO_NO_COLLECTIVE);
    
    is_collective = selection_mode != TEST_ACTUAL_IO_NO_COLLECTIVE;

    /* Set up MPI parameters */
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    MPI_Barrier(MPI_COMM_WORLD);
    
    HDassert(mpi_size >= 1);

    mpi_comm = MPI_COMM_WORLD;
    mpi_info = MPI_INFO_NULL;

    filename = (const char *)GetTestParameters();
    HDassert(filename != NULL);

    /* Setup the file access template */
    fapl = create_faccess_plist(mpi_comm, mpi_info, facc_type, use_gpfs);
    VRFY((fapl >= 0), "create_faccess_plist() succeeded");

    /* Create the file */
    fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    VRFY((fid >= 0), "H5Fcreate succeeded");

    /* Create the basic Space */    
    dims[0] = dim0;
    dims[1] = dim1;
    sid = H5Screate_simple (RANK, dims, NULL);
    VRFY((sid >= 0), "H5Screate_simple succeeded");

    /* Create the dataset creation plist */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    VRFY((dcpl >= 0), "dataset creation plist created successfully");

    /* If we are not testing contiguous datasets */
    if(is_chunked) {
        /* Set up chunk information.  */
        chunk_dims[0] = dims[0]/mpi_size;
        chunk_dims[1] = dims[1];
        ret = H5Pset_chunk(dcpl, 2, chunk_dims);
        VRFY((ret >= 0),"chunk creation property list succeeded");
    }

    /* Create the dataset */
    dataset = H5Dcreate2(fid, "actual_io", data_type, sid, H5P_DEFAULT,
            dcpl, H5P_DEFAULT);
    VRFY((dataset >= 0), "H5Dcreate2() dataset succeeded");

    /* Create the file dataspace */
    file_space = H5Dget_space(dataset);
    VRFY((file_space >= 0), "H5Dget_space succeeded");

    /* Choose a selection method based on the type of I/O we want to occur, 
     * and also set up some selection-dependeent test info. */
    switch(selection_mode) {
        
        /* Independent I/O with optimization */
        case TEST_ACTUAL_IO_MULTI_CHUNK_IND:
            /* Since the dataset is chunked by row and each process selects a row,
             * each process  writes to a different chunk. This forces all I/O to be
             * independent.
             */
            slab_set(mpi_rank, mpi_size, start, count, stride, block, BYROW);
            
            test_name = "Multi Chunk - Independent";
            actual_chunk_opt_mode_expected = H5D_MPIO_MULTI_CHUNK;
            actual_io_mode_expected = H5D_MPIO_CHUNK_INDEPENDENT;
            break;

        /* Collective I/O with optimization */
        case TEST_ACTUAL_IO_MULTI_CHUNK_COL:
            /* The dataset is chunked by rows, so each process takes a column which
             * spans all chunks. Since the processes write non-overlapping regular
             * selections to each chunk, the operation is purely collective.
             */
            slab_set(mpi_rank, mpi_size, start, count, stride, block, BYCOL);
            
            test_name = "Multi Chunk - Collective";
            actual_chunk_opt_mode_expected = H5D_MPIO_MULTI_CHUNK;
            actual_io_mode_expected = H5D_MPIO_CHUNK_COLLECTIVE;
            break;
        
        /* Mixed I/O with optimization */
        case TEST_ACTUAL_IO_MULTI_CHUNK_MIX:
            /* A chunk will be assigned collective I/O only if it is selected by each
             * process. To get mixed I/O, have the root select all chunks and each
             * subsequent process select the first and nth chunk. The first chunk,
             * accessed by all, will be assigned collective I/O while each other chunk
             * will be accessed only by the root and the nth procecess and will be
             * assigned independent I/O. Each process will access one chunk collectively
             * and at least one chunk independently, reporting mixed I/O.
             */
           
            if(mpi_rank == 0) {
                 /* Select the first column */
                 slab_set(mpi_rank, mpi_size, start, count, stride, block, BYCOL);
            } else {
                /* Select the first and the nth chunk in the nth column */
                block[0] = dim0 / mpi_size;
                block[1] = dim1 / mpi_size;
                count[0] = 2;
                count[1] = 1;
                stride[0] = mpi_rank * block[0];
                stride[1] = 1;
                start[0] = 0;
                start[1] = mpi_rank*block[1];
            }
                
            test_name = "Multi Chunk - Mixed";
            actual_chunk_opt_mode_expected = H5D_MPIO_MULTI_CHUNK;
            actual_io_mode_expected = H5D_MPIO_CHUNK_MIXED;
            break;

        /* RESET tests that the properties are properly reset to defaults each time I/O is
         * performed. To acheive this, we have RESET perform collective I/O (which would change
         * the values from the defaults) followed by independent I/O (which should report the
         * default values). RESET doesn't need to have a unique selection, so we reuse
         * MULTI_CHUMK_MIX_DISAGREE, which was chosen because it is a complex case that works 
         * on all builds. The independent section of RESET can be found at the end of this function.
         */
        case TEST_ACTUAL_IO_RESET:

        /* Mixed I/O with optimization and internal disagreement */
        case TEST_ACTUAL_IO_MULTI_CHUNK_MIX_DISAGREE:
            /* A chunk will be assigned collective I/O only if it is selected by each
             * process. To get mixed I/O with disagreement, assign process n to the
             * first chunk and the nth chunk. The first chunk, selected by all, is 
             * assgigned collective I/O, while each other process gets independent I/O.
             * Since the root process with only access the first chunk, it will report
             * collective I/O. The subsequent processes will access the first chunk
             * collectively, and their other chunk indpendently, reporting mixed I/O.
             */

            if(mpi_rank == 0) {
                 /* Select the first chunk in the first column */
                 slab_set(mpi_rank, mpi_size, start, count, stride, block, BYCOL);
                 block[0] = block[0] / mpi_size;
            } else {
                /* Select the first and the nth chunk in the nth column */
                block[0] = dim0 / mpi_size;
                block[1] = dim1 / mpi_size;
                count[0] = 2;
                count[1] = 1;
                stride[0] = mpi_rank * block[0];
                stride[1] = 1;
                start[0] = 0;
                start[1] = mpi_rank*block[1];
            }
            
            /* If the testname was not already set by the RESET case */
            if (selection_mode == TEST_ACTUAL_IO_RESET)
                test_name = "RESET";
            else
                test_name = "Multi Chunk - Mixed (Disagreement)";
            
            actual_chunk_opt_mode_expected = H5D_MPIO_MULTI_CHUNK;
               
            if(mpi_rank == 0)
                actual_io_mode_expected = H5D_MPIO_CHUNK_COLLECTIVE;
            else
                actual_io_mode_expected = H5D_MPIO_CHUNK_MIXED;
            
            break; 

        /* Collective I/O without optimization */
        case TEST_ACTUAL_IO_MULTI_CHUNK_NO_OPT_COL:
            /* The dataset is chunked by rows, so when each process takes a column, its
             * selection spans all chunks. Since no process writes more chunks than any
             * other, colective I/O is never broken. */ 
            slab_set(mpi_rank, mpi_size, start, count, stride, block, BYCOL);
                
            test_name = "Multi Chunk No Opt - Collective";
            actual_chunk_opt_mode_expected = H5D_MPIO_MULTI_CHUNK_NO_OPT;
            actual_io_mode_expected = H5D_MPIO_CHUNK_COLLECTIVE;
            break;
       

        /* Mixed I/O without optimization with disagreement */
        case TEST_ACTUAL_IO_MULTI_CHUNK_NO_OPT_MIX_DISAGREE:
            /* Each process takes a column, but the root's column is shortened so that
             * it only reads the first chunk. Since all the other processes are writing
             * to more chunks, they will break collective after the first chunk.
             */
            slab_set(mpi_rank, mpi_size, start, count, stride, block, BYCOL);
            if(mpi_rank == 0) 
                  block[0] = block[0] / mpi_size;
                
            test_name = "Multi Chunk No Opt - Mixed (Disagreement)";
            actual_chunk_opt_mode_expected = H5D_MPIO_MULTI_CHUNK_NO_OPT;

            if(mpi_rank == 0)
                actual_io_mode_expected = H5D_MPIO_CHUNK_COLLECTIVE;
            else
                actual_io_mode_expected = H5D_MPIO_CHUNK_MIXED;

            break;
        
        /* Linked Chunk I/O */
        case TEST_ACTUAL_IO_LINK_CHUNK:        
            /* Nothing special; link chunk I/O is forced in the dxpl settings. */
            slab_set(mpi_rank, mpi_size, start, count, stride, block, BYROW);
              
            test_name = "Link Chunk";
            actual_chunk_opt_mode_expected = H5D_MPIO_LINK_CHUNK;
            actual_io_mode_expected = H5D_MPIO_CHUNK_COLLECTIVE;
            break;

        /* Contiguous Dataset */
        case TEST_ACTUAL_IO_CONTIGUOUS:
            /* A non overlapping, regular selection in a contiguous dataset leads to
             * collective I/O */
            slab_set(mpi_rank, mpi_size, start, count, stride, block, BYROW);
                
            test_name = "Contiguous";
            actual_chunk_opt_mode_expected = H5D_MPIO_NO_CHUNK_OPTIMIZATION;
            actual_io_mode_expected = H5D_MPIO_CONTIGUOUS_COLLECTIVE;
            break;

        case TEST_ACTUAL_IO_NO_COLLECTIVE:
            slab_set(mpi_rank, mpi_size, start, count, stride, block, BYROW);
                
            test_name = "Independent";
            actual_chunk_opt_mode_expected = H5D_MPIO_NO_CHUNK_OPTIMIZATION;
            actual_io_mode_expected = H5D_MPIO_NO_COLLECTIVE;
            break;

        default:
            test_name = "Undefined Selection Mode";
            actual_chunk_opt_mode_expected = -1;
            actual_io_mode_expected = -1;
            break;
    }

    /* Reset the expected values to defulats if the MPI_POSIX driver is in use. 
     * This property is defined only for mpio, not MPI POSIX. */
    if (facc_type == FACC_MPIPOSIX) {
        actual_chunk_opt_mode_expected = H5D_MPIO_NO_CHUNK_OPTIMIZATION;
        actual_io_mode_expected = H5D_MPIO_NO_COLLECTIVE;
    }

    ret = H5Sselect_hyperslab(file_space, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");
 
    /* Create a memory dataspace mirroring the dataset and select the same hyperslab
     * as in the file space. 
     */
    mem_space = H5Screate_simple (RANK, dims, NULL);
    VRFY((mem_space >= 0), "mem_space created");
    
    ret = H5Sselect_hyperslab(mem_space, H5S_SELECT_SET, start, stride, count, block);
    VRFY((ret >= 0), "H5Sset_hyperslab succeeded");


    /* Get the number of elements in the selection */
    length = dim0 * dim1;

    /* Allocate and initialize the buffer */
    buffer = (int *)HDmalloc(sizeof(int) * length);
    VRFY((buffer != NULL), "malloc of buffer succeeded"); 
    for(i = 0; i < length; i++) 
        buffer[i] = i;

    /* Set up the dxpl for the write */
    dxpl_write = H5Pcreate(H5P_DATASET_XFER);
    VRFY((dxpl_write >= 0), "H5Pcreate(H5P_DATASET_XFER) succeeded");
    
    /* Set collective I/O properties in the dxpl. */
    if(is_collective) {
        /* Request collective I/O */
        ret = H5Pset_dxpl_mpio(dxpl_write, H5FD_MPIO_COLLECTIVE);
        VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");
        
        /* Set the threshold number of processes per chunk for link chunk I/O
         * to twice mpi_size. This will prevent the threshold from ever being
         * met, thus forcing multi chunk io instead of link chunk io.
         */
        if(multi_chunk_with_opt) {
            ret = H5Pset_dxpl_mpio_chunk_opt_num(dxpl_write, (unsigned) mpi_size*2);
            VRFY((ret >= 0), "H5Pset_dxpl_mpio_chunk_opt_num succeeded");

            ret = H5Pset_dxpl_mpio_chunk_opt_ratio(dxpl_write, (unsigned) 99);
            VRFY((ret >= 0), "H5Pset_dxpl_mpio_chunk_opt_ratio succeeded");
        }

        /* Request multi chunk I/O without optimization */
        if(multi_chunk_no_opt) {
            ret = H5Pset_dxpl_mpio_chunk_opt(dxpl_write, H5FD_MPIO_CHUNK_MULTI_IO);
            VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");
        }
    }

    /* Make a copy of the dxpl to test the read operation */
    dxpl_read = H5Pcopy(dxpl_write);
    VRFY((dxpl_read >= 0), "H5Pcopy succeeded");

    /* Write */
    ret = H5Dwrite(dataset, data_type, mem_space, file_space, dxpl_write, buffer);
    if(ret < 0) H5Eprint2(H5E_DEFAULT, stdout);
    VRFY((ret >= 0), "H5Dwrite() dataset multichunk write succeeded");

    /* Retreive Actual io valuess */
    ret = H5Pget_mpio_actual_io_mode(dxpl_write, &actual_io_mode_write);
    VRFY((ret >= 0), "retriving actual io mode suceeded" );

    ret = H5Pget_mpio_actual_chunk_opt_mode(dxpl_write, &actual_chunk_opt_mode_write);
    VRFY((ret >= 0), "retriving actual chunk opt mode succeeded" );
    
    /* Read */
    ret = H5Dread(dataset, data_type, mem_space, file_space, dxpl_read, buffer);
    if(ret < 0) H5Eprint2(H5E_DEFAULT, stdout);
    VRFY((ret >= 0), "H5Dread() dataset multichunk read succeeded");
   
    /* Retreive Actual io values */
    ret = H5Pget_mpio_actual_io_mode(dxpl_read, &actual_io_mode_read);
    VRFY((ret >= 0), "retriving actual io mode succeeded" );

    ret = H5Pget_mpio_actual_chunk_opt_mode(dxpl_read, &actual_chunk_opt_mode_read);
    VRFY((ret >= 0), "retriving actual chunk opt mode succeeded" );

    /* Check write vs read */
    VRFY((actual_io_mode_read == actual_io_mode_write),
        "reading and writing are the same for actual_io_mode");
    VRFY((actual_chunk_opt_mode_read == actual_chunk_opt_mode_write),
        "reading and writing are the same for actual_chunk_opt_mode");

    
    /* Test values */
    if(actual_chunk_opt_mode_expected != (unsigned) -1 && actual_io_mode_expected != (unsigned) -1) {
        char message[100];
        sprintf(message, "Actual Chunk Opt Mode has the correct value for %s.\n",test_name);
        VRFY((actual_chunk_opt_mode_write == actual_chunk_opt_mode_expected), message);
        sprintf(message, "Actual IO Mode has the correct value for %s.\n",test_name);
        VRFY((actual_io_mode_write == actual_io_mode_expected), message);
    } else {
        HDfprintf(stderr, "%s %d -> (%d,%d)\n", test_name, mpi_rank,
            actual_chunk_opt_mode_write, actual_io_mode_write);
    }

    /* To test that the property is succesfully reset to the default, we perform some
     * independent I/O after the collective I/O
     */
    if (selection_mode == TEST_ACTUAL_IO_RESET) {
        if (mpi_rank == 0) {
            /* Switch to independent io */
            ret = H5Pset_dxpl_mpio(dxpl_write, H5FD_MPIO_INDEPENDENT);
            VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");
            ret = H5Pset_dxpl_mpio(dxpl_read, H5FD_MPIO_INDEPENDENT);
            VRFY((ret >= 0), "H5Pset_dxpl_mpio succeeded");

            /* Write */
            ret = H5Dwrite(dataset, data_type, H5S_ALL, H5S_ALL, dxpl_write, buffer);
            VRFY((ret >= 0), "H5Dwrite() dataset multichunk write succeeded");

            /* Check Properties */
            ret = H5Pget_mpio_actual_io_mode(dxpl_write, &actual_io_mode_write);
            VRFY( (ret >= 0), "retriving actual io mode succeeded" );
            ret = H5Pget_mpio_actual_chunk_opt_mode(dxpl_write, &actual_chunk_opt_mode_write);
            VRFY( (ret >= 0), "retriving actual chunk opt mode succeeded" );

            VRFY(actual_chunk_opt_mode_write == H5D_MPIO_NO_CHUNK_OPTIMIZATION,
             "actual_chunk_opt_mode has correct value for reset write (independent)");
            VRFY(actual_io_mode_write == H5D_MPIO_NO_COLLECTIVE,
             "actual_io_mode has correct value for reset write (independent)");
            
            /* Read */
            ret = H5Dread(dataset, data_type, H5S_ALL, H5S_ALL, dxpl_read, buffer);
            VRFY((ret >= 0), "H5Dwrite() dataset multichunk write succeeded");

            /* Check Properties */
            ret = H5Pget_mpio_actual_io_mode(dxpl_read, &actual_io_mode_read);
            VRFY( (ret >= 0), "retriving actual io mode succeeded" );
            ret = H5Pget_mpio_actual_chunk_opt_mode(dxpl_read, &actual_chunk_opt_mode_read);
            VRFY( (ret >= 0), "retriving actual chunk opt mode succeeded" );
            
            VRFY(actual_chunk_opt_mode_read == H5D_MPIO_NO_CHUNK_OPTIMIZATION,
             "actual_chunk_opt_mode has correct value for reset read (independent)");
            VRFY(actual_io_mode_read == H5D_MPIO_NO_COLLECTIVE,
             "actual_io_mode has correct value for reset read (independent)");
         }
    }

    /* Release some resources */
    ret = H5Sclose(sid);
    ret = H5Pclose(fapl);
    ret = H5Pclose(dcpl);
    ret = H5Pclose(dxpl_write);
    ret = H5Pclose(dxpl_read);
    ret = H5Dclose(dataset);
    ret = H5Sclose(mem_space);
    ret = H5Sclose(file_space);
    ret = H5Fclose(fid);
    HDfree(buffer);
    return;
}


/* Function: actual_io_mode_tests
 *
 * Purpose: Tests all possible cases of the actual_io_mode property. 
 *
 * Programmer: Jacob Gruber
 * Date: 2011-04-06
 */
void
actual_io_mode_tests(void) {
    int mpi_size = -1;
    int mpi_rank = -1;
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_rank);
    
    test_actual_io_mode(TEST_ACTUAL_IO_NO_COLLECTIVE);
    
    test_actual_io_mode(TEST_ACTUAL_IO_MULTI_CHUNK_IND);
    test_actual_io_mode(TEST_ACTUAL_IO_MULTI_CHUNK_COL);
    
    /* The Multi Chunk Mixed test requires atleast three processes. */
    if (mpi_size > 2)
        test_actual_io_mode(TEST_ACTUAL_IO_MULTI_CHUNK_MIX);
    else
        HDfprintf(stdout, "Multi Chunk Mixed test requires 3 proceses minimum\n");
    
    test_actual_io_mode(TEST_ACTUAL_IO_MULTI_CHUNK_MIX_DISAGREE);

    test_actual_io_mode(TEST_ACTUAL_IO_MULTI_CHUNK_NO_OPT_COL);
    test_actual_io_mode(TEST_ACTUAL_IO_MULTI_CHUNK_NO_OPT_MIX_DISAGREE);

    test_actual_io_mode(TEST_ACTUAL_IO_LINK_CHUNK);
    test_actual_io_mode(TEST_ACTUAL_IO_CONTIGUOUS);
 
    test_actual_io_mode(TEST_ACTUAL_IO_RESET);
    return;
}
