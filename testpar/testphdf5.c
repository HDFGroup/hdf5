
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
#define MESG(x)\
	if (verbose) printf("%s\n", x);\

#ifdef HAVE_PARALLEL
#define MPI_BANNER(mesg)\
    {printf("--------------------------------\n");\
    printf("Proc %d: ", mympirank); \
    printf("*** %s\n", mesg);\
    printf("--------------------------------\n");}
#else
#define MPI_BANNER(mesg)\
    {printf("================================\n");\
    printf("*** %s\n", mesg);\
    printf("================================\n");}
#endif

#ifdef HAVE_PARALLEL
#define SYNC(comm)\
    {MPI_BANNER("doing a SYNC"); MPI_Barrier(comm); MPI_BANNER("SYNC DONE");}

/* pause the process for a moment to allow debugger to attach if desired. */
/* Will pause more if greenlight file is not persent but will eventually */
/* continue. */
#include <sys/types.h>
#include <sys/stat.h>
void pause_proc(MPI_Comm comm, int mympirank, char* processor_name, int namelen,
    int argc, char **argv)
{

    int pid;
    struct stat statbuf;
    char greenlight[] = "go";
    int maxloop = 10;
    int time_int = 10;

    /* check if an pause interval option is given */
    if (--argc > 0 && isdigit(*++argv))
	time_int = atoi(*argv);
    pid = getpid();
    printf("Proc %d (%*s): pid = %d\n",
	mympirank, namelen, processor_name, pid);

    if (mympirank == 0)
	while ((stat(greenlight, &statbuf) == -1) && maxloop-- > 0){
	    printf("waiting(%ds) for file %s ...", time_int, greenlight);
	    fflush(stdout);
	    sleep(time_int);
	}
    MPI_Barrier(comm);
}
#endif /*HAVE_PARALLEL*/
/* End of Define some handy debugging shorthands, routines, ... */

/* Constants definitions */
/* 24 is a multiple of 2, 3, 4, 6, 8, 12.  Neat for parallel tests. */
#define SPACE1_DIM1	8
#define SPACE1_DIM2	12
#define SPACE1_RANK	2
#define DATASETNAME1	"Data1"
#define DATASETNAME2	"Data2"
#define DATASETNAME3	"Data3"

/* dataset data type.  Int's can be easily octo dumped. */
typedef int DATATYPE;

/* global variables */
char    *filenames[]={
#ifdef HAVE_PARALLEL
"ParaEg1.h5f", "ParaEg2.h5f"
#else
"Eg1.h5f", "Eg2.h5f"
#endif
};

int nerrors = 0;				/* errors count */

/* option flags */
int verbose = 0;			/* verbose, default as no. */
int doread=1;				/* read test */
int dowrite=1;				/* write test */


/*
 * Fill the dataset with trivial data for testing.
 * Assume dimension rank is 2 and data is stored contiguous.
 */
void
dataset_data(int start[], size_t count[], DATATYPE * dataset)
{
    DATATYPE *dataptr = dataset;
    int i, j;

    /* put some trivial data in the data_array */
    for (i=0; i < count[0]; i++){
	for (j=0; j < count[1]; j++){
	    *dataptr++ = (i+start[0])*100 + (j+1);
	}
    }
}


/*
 * Print the content of the dataset.
 */
void dataset_print(int start[], size_t count[], DATATYPE * dataset)
{
    DATATYPE *dataptr = dataset;
    int i, j;

    /* print the slab read */
    for (i=0; i < count[0]; i++){
	printf("Row %d: ", i+start[0]);
	for (j=0; j < count[1]; j++){
	    printf("%03d ", *dataptr++);
	}
	printf("\n");
    }
}


/*
 * Print the content of the dataset.
 */
int dataset_vrfy(int start[], size_t count[], DATATYPE *dataset, DATATYPE *original)
{
    DATATYPE *dataptr = dataset;
    DATATYPE *originptr = original;

    int i, j, nerrors;

    nerrors = 0;
    for (i=0; i < count[0]; i++){
	for (j=0; j < count[1]; j++){
	    if (*dataset++ != *original++){
		    printf("Dataset Verify failed at [%d][%d]: expect %d, got %d\n",
			i, j, *(dataset-1), *(original-1));
		    nerrors++;
		}
	}
    }
    if (nerrors)
	printf("%d errors found in dataset_vrfy\n", nerrors);
    return(nerrors);
}


/* Example of using the parallel HDF5 library to create a dataset */
void
phdf5writeInd()
{
    hid_t fid1, fid2;		/* HDF5 file IDs */
    hid_t acc_tpl1;		/* File access templates */
    hid_t sid1,sid2;   		/* Dataspace ID */
    hid_t file_dataspace;	/* File dataspace ID */
    hid_t mem_dataspace;	/* memory dataspace ID */
    hid_t dataset1, dataset2;	/* Dataset ID */
    int rank = SPACE1_RANK; 	/* Logical rank of dataspace */
    size_t dims1[SPACE1_RANK] = {SPACE1_DIM1,SPACE1_DIM2};   	/* dataspace dim sizes */
    DATATYPE data_array1[SPACE1_DIM1][SPACE1_DIM2];	/* data buffer */

    int   start[SPACE1_RANK];				/* for hyperslab setting */
    size_t count[SPACE1_RANK], stride[SPACE1_RANK];	/* for hyperslab setting */

    herr_t ret;         	/* Generic return value */
    int   i, j;
    int numprocs, mympirank;
    char *fname;
    int	color = 0;			/* used for MPI_Comm_split */
    int mrc;			/* mpi return code */
    
#ifdef HAVE_PARALLEL
    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;

    /* set up MPI parameters */
    MPI_Comm_size(MPI_COMM_WORLD,&numprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&mympirank);
#else
    numprocs = 1;
    mympirank = 0;
#endif

#ifdef NO
    /* split into two new communicators, one contains the originally */
    /* odd rank processes, the other the even ones. */
    color = mympirank%2;
    mrc = MPI_Comm_split (MPI_COMM_WORLD, color, mympirank, &comm);
    assert(mrc==MPI_SUCCESS);
#endif

    /* setup file access template */
    acc_tpl1 = H5Pcreate (H5P_FILE_ACCESS);
    assert(acc_tpl1 != FAIL);
    MESG("H5Pcreate access succeed");
#ifdef HAVE_PARALLEL
    /* set Independent Parallel access with communicator */
    ret = H5Pset_mpi(acc_tpl1, comm, info, H5ACC_INDEPENDENT);     
    assert(ret != FAIL);
    MESG("H5Pset_mpi succeed");
#endif

    /* create the file collectively */
    fid1=H5Fcreate(filenames[color],H5F_ACC_TRUNC,H5P_DEFAULT,acc_tpl1);
    assert(fid1 != FAIL);
    MESG("H5Fcreate succeed");

    /* Release file-access template */
    ret=H5Pclose(acc_tpl1);
    assert(ret != FAIL);


    /* setup dimensionality object */
    sid1 = H5Screate_simple (SPACE1_RANK, dims1, NULL);
    assert (sid1 != FAIL);
    MESG("H5Screate_simple succeed");

    
    /* create a dataset collectively */
    dataset1 = H5Dcreate(fid1, DATASETNAME1, H5T_NATIVE_INT, sid1,
			H5P_DEFAULT);
    assert(dataset1 != FAIL);
    MESG("H5Dcreate succeed");

    /* create another dataset collectively */
    dataset2 = H5Dcreate(fid1, DATASETNAME2, H5T_NATIVE_INT, sid1,
			H5P_DEFAULT);
    assert(dataset2 != FAIL);
    MESG("H5Dcreate succeed");



    /* set up dimensions of the slab this process accesses */
    start[0] = mympirank*SPACE1_DIM1/numprocs;
    start[1] = 0;
    count[0] = SPACE1_DIM1/numprocs;
    count[1] = SPACE1_DIM2;
    stride[0] = 1;
    stride[1] =1;
if (verbose)
    printf("start[]=(%d,%d), count[]=(%lu,%lu), total datapoints=%lu\n",
	start[0], start[1], count[0], count[1], count[0]*count[1]);

    /* put some trivial data in the data_array */
    dataset_data(start, count, &data_array1[0][0]);
    MESG("data_array initialized");

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);				    
    assert(file_dataspace != FAIL);					    
    MESG("H5Dget_space succeed");
    ret=H5Sset_hyperslab(file_dataspace, start, count, stride); 
    assert(ret != FAIL);
    MESG("H5Sset_hyperslab succeed");

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (SPACE1_RANK, count, NULL);
    assert (mem_dataspace != FAIL);

    /* write data independently */
    ret = H5Dwrite(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,	    
	    H5P_DEFAULT, data_array1);					    
    assert(ret != FAIL);
    MESG("H5Dwrite succeed");

    /* write data independently */
    ret = H5Dwrite(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,	    
	    H5P_DEFAULT, data_array1);					    
    assert(ret != FAIL);
    MESG("H5Dwrite succeed");

    /* release dataspace ID */
    H5Sclose(file_dataspace);

    /* close dataset collectively */					    
    ret=H5Dclose(dataset1);
    assert(ret != FAIL);
    MESG("H5Dclose1 succeed");
    ret=H5Dclose(dataset2);
    assert(ret != FAIL);
    MESG("H5Dclose2 succeed");

    /* release all IDs created */
    H5Sclose(sid1);

    /* close the file collectively */					    
    H5Fclose(fid1);							    
}

/* Example of using the parallel HDF5 library to read a dataset */
void
phdf5readInd()
{
    hid_t fid1, fid2;		/* HDF5 file IDs */
    hid_t acc_tpl1;		/* File access templates */
    hid_t sid1,sid2;   		/* Dataspace ID */
    hid_t file_dataspace;	/* File dataspace ID */
    hid_t mem_dataspace;	/* memory dataspace ID */
    hid_t dataset1, dataset2;	/* Dataset ID */
    int rank = SPACE1_RANK; 	/* Logical rank of dataspace */
    size_t dims1[] = {SPACE1_DIM1,SPACE1_DIM2};   	/* dataspace dim sizes */
    DATATYPE data_array1[SPACE1_DIM1][SPACE1_DIM2];	/* data buffer */
    DATATYPE data_origin1[SPACE1_DIM1][SPACE1_DIM2];	/* expected data buffer */

    int   start[SPACE1_RANK];				/* for hyperslab setting */
    size_t count[SPACE1_RANK], stride[SPACE1_RANK];	/* for hyperslab setting */

    herr_t ret;         	/* Generic return value */
    int   i, j;
    int numprocs, mympirank;
#ifdef HAVE_PARALLEL
    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Info info = MPI_INFO_NULL;

    /* set up MPI parameters */
    MPI_Comm_size(MPI_COMM_WORLD,&numprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&mympirank);
#else
    numprocs = 1;
    mympirank = 0;
#endif


    /* setup file access template */
    acc_tpl1 = H5Pcreate (H5P_FILE_ACCESS);
    assert(acc_tpl1 != FAIL);
#ifdef HAVE_PARALLEL
    /* set Independent Parallel access with communicator */
    ret = H5Pset_mpi(acc_tpl1, comm, info, H5ACC_INDEPENDENT);     
    assert(ret != FAIL);
#endif


    /* open the file collectively */
    fid1=H5Fopen(filenames[0],H5F_ACC_RDWR,acc_tpl1);
    assert(fid1 != FAIL);

    /* Release file-access template */
    ret=H5Pclose(acc_tpl1);
    assert(ret != FAIL);

    /* open the dataset1 collectively */
    dataset1 = H5Dopen(fid1, DATASETNAME1);
    assert(dataset1 != FAIL);

    /* open another dataset collectively */
    dataset2 = H5Dopen(fid1, DATASETNAME1);
    assert(dataset2 != FAIL);


    /* set up dimensions of the slab this process accesses */
    start[0] = mympirank*SPACE1_DIM1/numprocs;
    start[1] = 0;
    count[0] = SPACE1_DIM1/numprocs;
    count[1] = SPACE1_DIM2;
    stride[0] = 1;
    stride[1] =1;
if (verbose)
    printf("start[]=(%d,%d), count[]=(%lu,%lu), total datapoints=%lu\n",
    start[0], start[1], count[0], count[1], count[0]*count[1]);

    /* create a file dataspace independently */
    file_dataspace = H5Dget_space (dataset1);
    assert(file_dataspace != FAIL);
    ret=H5Sset_hyperslab(file_dataspace, start, count, stride); 
    assert(ret != FAIL);

    /* create a memory dataspace independently */
    mem_dataspace = H5Screate_simple (SPACE1_RANK, count, NULL);
    assert (mem_dataspace != FAIL);

    /* fill dataset with test data */
    dataset_data(start, count, &data_origin1[0][0]);

    /* read data independently */
    ret = H5Dread(dataset1, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    H5P_DEFAULT, data_array1);
    assert(ret != FAIL);

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, &data_array1[0][0], &data_origin1[0][0]);
    assert(ret != FAIL);

    /* read data independently */
    ret = H5Dread(dataset2, H5T_NATIVE_INT, mem_dataspace, file_dataspace,
	    H5P_DEFAULT, data_array1);
    assert(ret != FAIL);

    /* verify the read data with original expected data */
    ret = dataset_vrfy(start, count, &data_array1[0][0], &data_origin1[0][0]);
    assert(ret == 0);

    /* close dataset collectively */
    ret=H5Dclose(dataset1);
    assert(ret != FAIL);
    ret=H5Dclose(dataset2);
    assert(ret != FAIL);

    /* release all IDs created */
    H5Sclose(file_dataspace);

    /* close the file collectively */
    H5Fclose(fid1);
}

#ifdef HAVE_PARALLEL
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
test_split_comm_access()
{
    int numprocs, myrank;
    MPI_Comm comm;
    MPI_Info info = MPI_INFO_NULL;
    int color, mrc;
    int newrank, newprocs;
    hid_t fid;			/* file IDs */
    hid_t acc_tpl;		/* File access properties */
    herr_t ret;			/* generic return value */

    /* set up MPI parameters */
    MPI_Comm_size(MPI_COMM_WORLD,&numprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&myrank);
    color = myrank%2;
    mrc = MPI_Comm_split (MPI_COMM_WORLD, color, myrank, &comm);
    assert(mrc==MPI_SUCCESS);
    MPI_Comm_size(comm,&newprocs);
    MPI_Comm_rank(comm,&newrank);

    if (color){
	/* odd-rank processes */
	mrc = MPI_Barrier(comm);
	assert(mrc==MPI_SUCCESS);
    }else{
	/* even-rank processes */
	/* setup file access template */
	acc_tpl = H5Pcreate (H5P_FILE_ACCESS);
	assert(acc_tpl != FAIL);
	
	/* set Independent Parallel access with communicator */
	ret = H5Pset_mpi(acc_tpl, comm, info, H5ACC_INDEPENDENT);     
	assert(ret != FAIL);

	/* create the file collectively */
	fid=H5Fcreate(filenames[color],H5F_ACC_TRUNC,H5P_DEFAULT,acc_tpl);
	assert(fid != FAIL);
	MESG("H5Fcreate succeed");

	/* Release file-access template */
	ret=H5Pclose(acc_tpl);
	assert(ret != FAIL);

	ret=H5Fclose(fid);
	assert(ret != FAIL);
    }
    if (myrank == 0){
	mrc = MPI_File_delete(filenames[color], info);
	assert(mrc==MPI_SUCCESS);
    }
}
#endif

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


main(int argc, char **argv)
{
    int numprocs, mympirank, namelen;
    char processor_name[MPI_MAX_PROCESSOR_NAME];

#ifdef HAVE_PARALLEL
    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD,&numprocs);
    MPI_Comm_rank(MPI_COMM_WORLD,&mympirank);
    MPI_Get_processor_name(processor_name,&namelen);
#ifdef USE_PAUSE
    pause_proc(MPI_COMM_WORLD, mympirank, processor_name, namelen, argc, argv);
#endif
#endif

    /* parse option */
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
			    goto finish;
	    }
	}
    }


    if (dowrite){
#ifdef HAVE_PARALLEL
	MPI_BANNER("testing PHDF5 dataset using split communicators...");
	test_split_comm_access();
#endif
	MPI_BANNER("testing PHDF5 dataset independent write...");
	phdf5writeInd();
    }
    if (doread){
	MPI_BANNER("testing PHDF5 dataset independent read...");
	phdf5readInd();
    }

    if (!(dowrite || doread)){
	usage();
	nerrors++;
    }

finish:
    if (mympirank == 0){		/* only process 0 reports */
	if (nerrors)
	    printf("***PHDF5 tests detected %d errors***\n", nerrors);
	else{
	    printf("===================================\n");
	    printf("PHDF5 tests finished with no errors\n");
	    printf("===================================\n");
	}
    }
#ifdef HAVE_PARALLEL
    MPI_Finalize();
#endif

    return(nerrors);
}

