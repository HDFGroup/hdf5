
/* $Id$ */

/*
 * Example of using the parallel HDF5 library to access datasets.
 *
 * This program contains two parts.  In the first part, the mpi processes
 * collectively create a new parallel HDF5 file and create two fixed
 * dimension datasets in it.  Then each process writes a hyperslab into
 * each dataset in an independent mode.  All processes collectively
 * close the datasets and the file.
 * In the second part, the processes collectively open the created file
 * and the two datasets in it.  Then each process reads a hyperslab from
 * each dataset in an independent mode and prints them out.
 * All processes collectively close the datasets and the file.
 */

#include <assert.h>
#include <hdf5.h>
#include <mpi.h>
#include <mpio.h>

/* Temporary source code */
#define FAIL -1
/* temporary code end */

/* Define some handy debugging shorthands, routines, ... */
/* debugging tools */

#define MESG(x)								      \
	if (verbose) printf("%s\n", x);					      \

#define VRFY(val, mesg) do {                                                  \
    if (val) {                                                                \
	if (*mesg != '\0'){						      \
	    MESG(mesg);							      \
	}								      \
    }								      	      \
    else{								      \
        printf("*** Assertion failed (%s) at line %4d in %s\n",               \
	    mesg, (int)__LINE__, __FILE__);     			      \
        nerrors++;                                                            \
        H5Eprint (stdout);                                                    \
	if (!verbose) exit(nerrors);					      \
    }                                                                         \
    H5Eclear();                                                               \
} while(0)

#define MPI_BANNER(mesg)\
    {printf("--------------------------------\n");\
    printf("Proc %d: ", mpi_rank); \
    printf("*** %s\n", mesg);\
    printf("--------------------------------\n");}

#define SYNC(comm)\
    {MPI_BANNER("doing a SYNC"); MPI_Barrier(comm); MPI_BANNER("SYNC DONE");}
/* End of Define some handy debugging shorthands, routines, ... */

/* Constants definitions */
/* 24 is a multiple of 2, 3, 4, 6, 8, 12.  Neat for parallel tests. */
#define SPACE1_DIM1	24
#define SPACE1_DIM2	24
#define SPACE1_RANK	2
#define DATASETNAME1	"Data1"
#define DATASETNAME2	"Data2"
#define DATASETNAME3	"Data3"
/* hyperslab layout styles */
#define BYROW		1	/* divide into slabs of rows */
#define BYCOL		2	/* divide into blocks of columns */


/* dataset data type.  Int's can be easily octo dumped. */
typedef int DATATYPE;

/* global variables */
int nerrors = 0;				/* errors count */

int mpi_size, mpi_rank;				/* mpi variables */

/* option flags */
int verbose = 0;			/* verbose, default as no. */
int doread=1;				/* read test */
int dowrite=1;				/* write test */


#ifdef USE_PAUSE
/* pause the process for a moment to allow debugger to attach if desired. */
/* Will pause more if greenlight file is not persent but will eventually */
/* continue. */
#include <sys/types.h>
#include <sys/stat.h>
void pause_proc(MPI_Comm comm, int mpi_rank, char* mpi_name, int mpi_namelen,
    int argc, char **argv)
{

    int pid;
    struct stat statbuf;
    char greenlight[] = "go";
    int maxloop = 10;
    int loops = 0;
    int time_int = 10;

#ifdef DISABLED
    /* check if an pause interval option is given */
    if (--argc > 0 && isdigit(*++argv))
	time_int = atoi(*argv);
#endif
    pid = getpid();

    if (mpi_rank == 0)
	while ((stat(greenlight, &statbuf) == -1) && loops < maxloop){
	    if (!loops++){
		printf("Proc %d (%*s, %d): You may attach %d for debugging.\n",
		    mpi_rank, mpi_namelen, mpi_name, pid, pid);
	    }
	    printf("waiting(%ds) for file %s ...\n", time_int, greenlight);
	    fflush(stdout);
	    sleep(time_int);
	}
    MPI_Barrier(comm);
}
#endif	/* USE_PAUSE */

/*
 * Setup the dimensions of the hyperslab.
 * Two modes--by rows or by columns.
 * Assume dimension rank is 2.
 */
void
slab_set(hssize_t start[], hsize_t count[], hsize_t stride[], int mode)
{
    switch (mode){
    case BYROW:
	/* Each process takes a slabs of rows. */
	stride[0] = 1;
	stride[1] = 1;
	count[0] = SPACE1_DIM1/mpi_size;
	count[1] = SPACE1_DIM2;
	start[0] = mpi_rank*count[0];
	start[1] = 0;
if (verbose) printf("slab_set BYROW\n");
	break;
    case BYCOL:
	/* Each process takes a block of columns. */
	stride[0] = 1;
	stride[1] = 1;
	count[0] = SPACE1_DIM1;
	count[1] = SPACE1_DIM2/mpi_size;
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
	count[0] = SPACE1_DIM1;
	count[1] = SPACE1_DIM2;
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
		if (vrfyerrs++ < MAX_ERR_REPORT){
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
    if (vrfyerrs > MAX_ERR_REPORT)
	printf("[more errors ...]\n");
    if (vrfyerrs)
	printf("%d errors found in dataset_vrfy\n", vrfyerrs);
    return(vrfyerrs);
}


/*
 * Example of using the parallel HDF5 library to create two datasets
 * in one HDF5 files with parallel MPIO access support.
 * The Datasets are of sizes (number-of-mpi-processes x DIM1) x DIM2.
 * Each process controls only a slab of size DIM1 x DIM2 within each
 * dataset.
 */

void
phdf5writeInd(char *filename)
{
    hid_t fid1, fid2;		/* HDF5 file IDs */
    hid_t acc_tpl1;		/* File access templates */
    hid_t sid1,sid2;   		/* Dataspace ID */
    hid_t file_dataspace;	/* File dataspace ID */
    hid_t mem_dataspace;	/* memory dataspace ID */
    hid_t dataset1, dataset2;	/* Dataset ID */
    int rank = SPACE1_RANK; 	/* Logical rank of dataspace */
    hsize_t dims1[SPACE1_RANK] =
	{SPACE1_DIM1,SPACE1_DIM2};	/* dataspace dim sizes */
    hsize_t dimslocal1[SPACE1_RANK] =
	{SPACE1_DIM1,SPACE1_DIM2}; 	/* local dataspace dim sizes */
    DATATYPE data_array1[SPACE1_DIM1][SPACE1_DIM2];	/* data buffer */

    hssize_t   start[SPACE1_RANK];			/* for hyperslab setting */
    hsize_t count[SPACE1_RANK], stride[SPACE1_RANK];	/* for hyperslab setting */

    herr_t ret;         	/* Generic return value */
    int   i, j;
    int mpi_size, mpi_rank;
    char *fname;
    int mrc;			/* mpi return code */
    
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
    acc_tpl1 = H5Pcreate (H5P_FILE_ACCESS);
    VRFY((acc_tpl1 != FAIL), "H5Pcreate access succeed");
    /* set Parallel access with communicator */
    ret = H5Pset_mpi(acc_tpl1, comm, info);     
    VRFY((ret != FAIL), "H5Pset_mpi succeed");

    /* create the file collectively */
    fid1=H5Fcreate(filename,H5F_ACC_TRUNC,H5P_DEFAULT,acc_tpl1);
    VRFY((fid1 != FAIL), "H5Fcreate succeed");

    /* Release file-access template */
    ret=H5Pclose(acc_tpl1);
    VRFY((ret != FAIL), "");


    /* --------------------------
     * Define the dimensions of the overall datasets
     * and the slabs local to the MPI process.
     * ------------------------- */
    /* setup dimensionality object */
    sid1 = H5Screate_simple (SPACE1_RANK, dims1, NULL);
    VRFY((sid1 != FAIL), "H5Screate_simple succeed");

    
    /* create a dataset collectively */
    dataset1 = H5Dcreate(fid1, DATASETNAME1, H5T_NATIVE_INT, sid1,
			H5P_DEFAULT);
    VRFY((dataset1 != FAIL), "H5Dcreate succeed");

    /* create another dataset collectively */
    dataset2 = H5Dcreate(fid1, DATASETNAME2, H5T_NATIVE_INT, sid1,
			H5P_DEFAULT);
    VRFY((dataset2 != FAIL), "H5Dcreate succeed");



    /* set up dimensions of the slab this process accesses */
    slab_set(start, count, stride, BYROW);

    /* put some trivial data in the data_array */
    dataset_fill(start, count, stride, &data_array1[0][0]);
    MESG("data_array initialized");

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);				    
    VRFY((file_dataspace != FAIL), "H5Dget_space succeed");
    ret=H5Sset_hyperslab(file_dataspace, start, count, stride); 
    VRFY((ret != FAIL), "H5Sset_hyperslab succeed");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (SPACE1_RANK, count, NULL);
    VRFY((mem_dataspace != FAIL), "");

    /* write data independently */
    ret = H5Dwrite(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,	    
	    H5P_DEFAULT, data_array1);					    
    VRFY((ret != FAIL), "H5Dwrite succeed");

    /* write data independently */
    ret = H5Dwrite(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,	    
	    H5P_DEFAULT, data_array1);					    
    VRFY((ret != FAIL), "H5Dwrite succeed");

    /* release dataspace ID */
    H5Sclose(file_dataspace);

    /* close dataset collectively */					    
    ret=H5Dclose(dataset1);
    VRFY((ret != FAIL), "H5Dclose1 succeed");
    ret=H5Dclose(dataset2);
    VRFY((ret != FAIL), "H5Dclose2 succeed");

    /* release all IDs created */
    H5Sclose(sid1);

    /* close the file collectively */					    
    H5Fclose(fid1);							    
}

/* Example of using the parallel HDF5 library to read a dataset */
void
phdf5readInd(char *filename)
{
    hid_t fid1, fid2;		/* HDF5 file IDs */
    hid_t acc_tpl1;		/* File access templates */
    hid_t sid1,sid2;   		/* Dataspace ID */
    hid_t file_dataspace;	/* File dataspace ID */
    hid_t mem_dataspace;	/* memory dataspace ID */
    hid_t dataset1, dataset2;	/* Dataset ID */
    int rank = SPACE1_RANK; 	/* Logical rank of dataspace */
    hsize_t dims1[] = {SPACE1_DIM1,SPACE1_DIM2};   	/* dataspace dim sizes */
    DATATYPE data_array1[SPACE1_DIM1][SPACE1_DIM2];	/* data buffer */
    DATATYPE data_origin1[SPACE1_DIM1][SPACE1_DIM2];	/* expected data buffer */

    hssize_t   start[SPACE1_RANK];			/* for hyperslab setting */
    hsize_t count[SPACE1_RANK], stride[SPACE1_RANK];	/* for hyperslab setting */

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
    acc_tpl1 = H5Pcreate (H5P_FILE_ACCESS);
    VRFY((acc_tpl1 != FAIL), "");
    /* set Parallel access with communicator */
    ret = H5Pset_mpi(acc_tpl1, comm, info);     
    VRFY((ret != FAIL), "");


    /* open the file collectively */
    fid1=H5Fopen(filename,H5F_ACC_RDWR,acc_tpl1);
    VRFY((fid1 != FAIL), "");

    /* Release file-access template */
    ret=H5Pclose(acc_tpl1);
    VRFY((ret != FAIL), "");

    /* open the dataset1 collectively */
    dataset1 = H5Dopen(fid1, DATASETNAME1);
    VRFY((dataset1 != FAIL), "");

    /* open another dataset collectively */
    dataset2 = H5Dopen(fid1, DATASETNAME1);
    VRFY((dataset2 != FAIL), "");


    /* set up dimensions of the slab this process accesses */
    slab_set(start, count, stride, BYROW);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);
    VRFY((file_dataspace != FAIL), "");
    ret=H5Sset_hyperslab(file_dataspace, start, count, stride); 
    VRFY((ret != FAIL), "");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (SPACE1_RANK, count, NULL);
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
    H5Fclose(fid1);
}


/*
 * Example of using the parallel HDF5 library to create two datasets
 * in one HDF5 file with collective parallel access support.
 * The Datasets are of sizes (number-of-mpi-processes x DIM1) x DIM2.
 * Each process controls only a slab of size DIM1 x DIM2 within each
 * dataset. [Note: not so yet.  Datasets are of sizes DIM1xDIM2 and
 * each process controls a hyperslab within.]
 */

void
phdf5writeAll(char *filename)
{
    hid_t fid1, fid2;		/* HDF5 file IDs */
    hid_t acc_tpl1;		/* File access templates */
    hid_t xfer_plist;		/* Dataset transfer properties list */
    hid_t sid1,sid2;   		/* Dataspace ID */
    hid_t file_dataspace;	/* File dataspace ID */
    hid_t mem_dataspace;	/* memory dataspace ID */
    hid_t dataset1, dataset2;	/* Dataset ID */
    hid_t datatype;		/* Datatype ID */
    int rank = SPACE1_RANK; 	/* Logical rank of dataspace */
    hsize_t dims1[SPACE1_RANK] =
	{SPACE1_DIM1,SPACE1_DIM2};	/* dataspace dim sizes */
    DATATYPE data_array1[SPACE1_DIM1][SPACE1_DIM2];	/* data buffer */

    hssize_t   start[SPACE1_RANK];			/* for hyperslab setting */
    hsize_t count[SPACE1_RANK], stride[SPACE1_RANK];	/* for hyperslab setting */

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
    acc_tpl1 = H5Pcreate (H5P_FILE_ACCESS);
    VRFY((acc_tpl1 != FAIL), "H5Pcreate access succeed");
    /* set Parallel access with communicator */
    ret = H5Pset_mpi(acc_tpl1, comm, info);     
    VRFY((ret != FAIL), "H5Pset_mpi succeed");

    /* create the file collectively */
    fid1=H5Fcreate(filename,H5F_ACC_TRUNC,H5P_DEFAULT,acc_tpl1);
    VRFY((fid1 != FAIL), "H5Fcreate succeed");

    /* Release file-access template */
    ret=H5Pclose(acc_tpl1);
    VRFY((ret != FAIL), "");


    /* --------------------------
     * Define the dimensions of the overall datasets
     * and create the dataset
     * ------------------------- */
    /* setup dimensionality object */
    sid1 = H5Screate_simple (SPACE1_RANK, dims1, NULL);
    VRFY((sid1 != FAIL), "H5Screate_simple succeed");

    
    /* create a dataset collectively */
    dataset1 = H5Dcreate(fid1, DATASETNAME1, H5T_NATIVE_INT, sid1, H5P_DEFAULT);
    VRFY((dataset1 != FAIL), "H5Dcreate succeed");

    /* create another dataset collectively */
    datatype = H5Tcopy(H5T_NATIVE_INT32);
    ret = H5Tset_order(datatype, H5T_ORDER_LE);
    VRFY((ret != FAIL), "H5Tset_order succeed");

    dataset2 = H5Dcreate(fid1, DATASETNAME2, datatype, sid1, H5P_DEFAULT);
    VRFY((dataset2 != FAIL), "H5Dcreate 2 succeed");

    /*
     * Set up dimensions of the slab this process accesses.
     */

    /* Dataset1: each process takes a block of rows. */
    slab_set(start, count, stride, BYROW);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);				    
    VRFY((file_dataspace != FAIL), "H5Dget_space succeed");
    ret=H5Sset_hyperslab(file_dataspace, start, count, stride); 
    VRFY((ret != FAIL), "H5Sset_hyperslab succeed");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (SPACE1_RANK, count, NULL);
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
    VRFY((ret != FAIL), "H5Pcreate xfer succeed");

    /* write data collectively */
    ret = H5Dwrite(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);					    
    VRFY((ret != FAIL), "H5Dwrite dataset1 succeed");

    /* release all temporary handles. */
    /* Could have used them for dataset2 but it is cleaner */
    /* to create them again.*/
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);

    /* Dataset2: each process takes a block of columns. */
    slab_set(start, count, stride, BYCOL);

    /* put some trivial data in the data_array */
    dataset_fill(start, count, stride, &data_array1[0][0]);
    MESG("data_array initialized");
    if (verbose){
	MESG("data_array created");
	dataset_print(start, count, stride, &data_array1[0][0]);
    }

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);				    
    VRFY((file_dataspace != FAIL), "H5Dget_space succeed");
    ret=H5Sset_hyperslab(file_dataspace, start, count, stride); 
    VRFY((ret != FAIL), "H5Sset_hyperslab succeed");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (SPACE1_RANK, count, NULL);
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
    VRFY((ret != FAIL), "H5Pcreate xfer succeed");

    /* write data independently */
printf("WRITING TO DATASET2\n");
    ret = H5Dwrite(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);					    
    VRFY((ret != FAIL), "H5Dwrite dataset2 succeed");

    /* release all temporary handles. */
    H5Sclose(file_dataspace);
    H5Sclose(mem_dataspace);
    H5Pclose(xfer_plist);


    /*
     * All writes completed.  Close datasets collectively
     */					    
    ret=H5Dclose(dataset1);
    VRFY((ret != FAIL), "H5Dclose1 succeed");
    ret=H5Dclose(dataset2);
    VRFY((ret != FAIL), "H5Dclose2 succeed");

    /* release all IDs created */
    H5Sclose(sid1);

    /* close the file collectively */					    
    H5Fclose(fid1);							    
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
phdf5readAll(char *filename)
{
    hid_t fid1, fid2;		/* HDF5 file IDs */
    hid_t acc_tpl1;		/* File access templates */
    hid_t xfer_plist;		/* Dataset transfer properties list */
    hid_t sid1,sid2;   		/* Dataspace ID */
    hid_t file_dataspace;	/* File dataspace ID */
    hid_t mem_dataspace;	/* memory dataspace ID */
    hid_t dataset1, dataset2;	/* Dataset ID */
    int rank = SPACE1_RANK; 	/* Logical rank of dataspace */
    hsize_t dims1[] = {SPACE1_DIM1,SPACE1_DIM2};   	/* dataspace dim sizes */
    DATATYPE data_array1[SPACE1_DIM1][SPACE1_DIM2];	/* data buffer */
    DATATYPE data_origin1[SPACE1_DIM1][SPACE1_DIM2];	/* expected data buffer */

    hssize_t   start[SPACE1_RANK];			/* for hyperslab setting */
    hsize_t count[SPACE1_RANK], stride[SPACE1_RANK];	/* for hyperslab setting */

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
    acc_tpl1 = H5Pcreate (H5P_FILE_ACCESS);
    VRFY((acc_tpl1 != FAIL), "H5Pcreate access succeed");
    /* set Parallel access with communicator */
    ret = H5Pset_mpi(acc_tpl1, comm, info);     
    VRFY((ret != FAIL), "H5Pset_mpi succeed");

    /* open the file collectively */
    fid1=H5Fopen(filename,H5F_ACC_RDWR,acc_tpl1);
    VRFY((fid1 != FAIL), "H5Fopen succeed");

    /* Release file-access template */
    ret=H5Pclose(acc_tpl1);
    VRFY((ret != FAIL), "");


    /* --------------------------
     * Open the datasets in it
     * ------------------------- */
    /* open the dataset1 collectively */
    dataset1 = H5Dopen(fid1, DATASETNAME1);
    VRFY((dataset1 != FAIL), "H5Dopen succeed");

    /* open another dataset collectively */
    dataset2 = H5Dopen(fid1, DATASETNAME2);
    VRFY((dataset2 != FAIL), "H5Dopen 2 succeed");

    /*
     * Set up dimensions of the slab this process accesses.
     */

    /* Dataset1: each process takes a block of columns. */
    slab_set(start, count, stride, BYCOL);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);				    
    VRFY((file_dataspace != FAIL), "H5Dget_space succeed");
    ret=H5Sset_hyperslab(file_dataspace, start, count, stride); 
    VRFY((ret != FAIL), "H5Sset_hyperslab succeed");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (SPACE1_RANK, count, NULL);
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
    VRFY((ret != FAIL), "H5Pcreate xfer succeed");

    /* read data collectively */
    ret = H5Dread(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);					    
    VRFY((ret != FAIL), "H5Dread succeed");

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
    slab_set(start, count, stride, BYROW);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);				    
    VRFY((file_dataspace != FAIL), "H5Dget_space succeed");
    ret=H5Sset_hyperslab(file_dataspace, start, count, stride); 
    VRFY((ret != FAIL), "H5Sset_hyperslab succeed");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (SPACE1_RANK, count, NULL);
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
    VRFY((ret != FAIL), "H5Pcreate xfer succeed");

    /* read data independently */
    ret = H5Dread(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    xfer_plist, data_array1);					    
    VRFY((ret != FAIL), "H5Dread succeed");

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
    VRFY((ret != FAIL), "H5Dclose1 succeed");
    ret=H5Dclose(dataset2);
    VRFY((ret != FAIL), "H5Dclose2 succeed");

    /* close the file collectively */					    
    H5Fclose(fid1);							    
}

/*
 * test file access by communicator besides COMM_WORLD.
 * Split COMM_WORLD into two, one (even_comm) contains the original
 * processes of even ranks.  The other (odd_comm) contains the original
 * processes of odd ranks.  Processes in even_comm creates a file, then
 * cloose it, using even_comm.  Processes in old_comm just do a barrier
 * using odd_comm.  Then they all do a barrier using COMM_WORLD.
 * If the file creation and cloose does not do correct collective action
 * according to the communicator argument, the processes will freeze up
 * sooner or later due to barrier mixed up.
 */
void
test_split_comm_access(char *filename[])
{
    int mpi_size, mpi_rank;
    MPI_Comm comm;
    MPI_Info info = MPI_INFO_NULL;
    int color, mrc;
    int newrank, newprocs;
    hid_t fid;			/* file IDs */
    hid_t acc_tpl;		/* File access properties */
    herr_t ret;			/* generic return value */

    if (verbose)
	printf("Split Communicator access test on file %s %s\n",
	    filename[0], filename[1]);

    /* set up MPI parameters */
    MPI_Comm_size(MPI_COMM_WORLD,&mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);
    color = mpi_rank%2;
    mrc = MPI_Comm_split (MPI_COMM_WORLD, color, mpi_rank, &comm);
    VRFY((mrc==MPI_SUCCESS), "");
    MPI_Comm_size(comm,&newprocs);
    MPI_Comm_rank(comm,&newrank);

    if (color){
	/* odd-rank processes */
	mrc = MPI_Barrier(comm);
	VRFY((mrc==MPI_SUCCESS), "");
    }else{
	/* even-rank processes */
	int sub_mpi_rank;	/* rank in the sub-comm */
	MPI_Comm_rank(comm,&sub_mpi_rank);

	/* setup file access template */
	acc_tpl = H5Pcreate (H5P_FILE_ACCESS);
	VRFY((acc_tpl != FAIL), "");
	
	/* set Parallel access with communicator */
	ret = H5Pset_mpi(acc_tpl, comm, info);     
	VRFY((ret != FAIL), "");

	/* create the file collectively */
	fid=H5Fcreate(filename[color],H5F_ACC_TRUNC,H5P_DEFAULT,acc_tpl);
	VRFY((fid != FAIL), "H5Fcreate succeed");

	/* Release file-access template */
	ret=H5Pclose(acc_tpl);
	VRFY((ret != FAIL), "");

	/* close the file */
	ret=H5Fclose(fid);
	VRFY((ret != FAIL), "");

	/* detele the test file */
	if (sub_mpi_rank == 0){
	    mrc = MPI_File_delete(filename[color], info);
	    VRFY((mrc==MPI_SUCCESS), "");
	}
    }
}

/*
 * Show command usage
 */
void
usage()
{
    printf("Usage: testphdf5 [-r] [-w] [-v]\n");
    printf("\t-r\tno read\n");
    printf("\t-w\tno write\n");
    printf("\t-v\tverbose on\n");
    printf("\tdefault do write then read\n");
    printf("\n");
}


/*
 * parse the command line options
 */
int
parse_options(int argc, char **argv){
    while (--argc){
	if (**(++argv) != '-'){
	    break;
	}else{
	    switch(*(*argv+1)){
		case 'r':   doread = 0;
			    break;
		case 'w':   dowrite = 0;
			    break;
		case 'v':   verbose = 1;
			    break;
		default:    usage();
			    nerrors++;
			    return(1);
	    }
	}
    }
    return(0);
}


main(int argc, char **argv)
{
    char    *filenames[]={ "ParaEg1.h5f", "ParaEg2.h5f" };

    int mpi_namelen;		
    char mpi_name[MPI_MAX_PROCESSOR_NAME];

    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD,&mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);
    MPI_Get_processor_name(mpi_name,&mpi_namelen);
    /* Make sure datasets can be divided into equal chunks by the processes */
    if ((SPACE1_DIM1 % mpi_size) || (SPACE1_DIM2 % mpi_size)){
	printf("DIM1(%d) and DIM2(%d) must be multiples of processes (%d)\n",
	    SPACE1_DIM1, SPACE1_DIM2, mpi_size);
	nerrors++;
	goto finish;
    }

#ifdef USE_PAUSE
    pause_proc(MPI_COMM_WORLD, mpi_rank, mpi_name, mpi_namelen, argc, argv);
#endif

    if (parse_options(argc, argv) != 0)
	goto finish;

    if (dowrite){
	MPI_BANNER("testing PHDF5 dataset using split communicators...");
	test_split_comm_access(filenames);
	MPI_BANNER("testing PHDF5 dataset independent write...");
	phdf5writeInd(filenames[0]);
	MPI_BANNER("testing PHDF5 dataset collective write...");
	phdf5writeAll(filenames[1]);
    }
    if (doread){
	MPI_BANNER("testing PHDF5 dataset independent read...");
	phdf5readInd(filenames[0]);
	MPI_BANNER("testing PHDF5 dataset collective read...");
	phdf5readAll(filenames[1]);
    }

    if (!(dowrite || doread)){
	usage();
	nerrors++;
    }

finish:
    if (mpi_rank == 0){		/* only process 0 reports */
	printf("===================================\n");
	if (nerrors){
	    printf("***PHDF5 tests detected %d errors***\n", nerrors);
	}
	else{
	    printf("PHDF5 tests finished with no errors\n");
	}
	printf("===================================\n");
    }
    MPI_Finalize();

    return(nerrors);
}

