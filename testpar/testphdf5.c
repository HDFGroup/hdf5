/* $Id$ */

/*
 * Main driver of the Parallel HDF5 tests
 */

#include <testphdf5.h>

#ifndef FILENAME_MAX
#define FILENAME_MAX 512
#endif

/* global variables */
int dim0 = DIM0;
int dim1 = DIM1;
int chunkdim0;
int chunkdim1;
int nerrors = 0;			/* errors count */
int verbose = 0;			/* verbose, default as no. */
int ndatasets = 300;			/*number of datasets to create*/

herr_t (*old_func)(void*);		/* previous error handler */
void *old_client_data;			/* previous error handler arg.*/

/* other option flags */
int doread=1;				/* read test */
int dowrite=1;				/* write test */
/* FILENAME and filenames must have the same number of names */
const char *FILENAME[5]={
	    "ParaEg1",
	    "ParaEg2",
	    "ParaEg3",
	    "ParaMdset",
	    NULL};
char	filenames[5][FILENAME_MAX];
hid_t	fapl;				/* file access property list */



#ifdef USE_PAUSE
/* pause the process for a moment to allow debugger to attach if desired. */
/* Will pause more if greenlight file is not persent but will eventually */
/* continue. */
#include <sys/types.h>
#include <sys/stat.h>

void pause_proc(void)
{

    int pid;
    struct stat statbuf;
    char greenlight[] = "go";
    int maxloop = 10;
    int loops = 0;
    int time_int = 10;

    /* mpi variables */
    int  mpi_size, mpi_rank;
    int  mpi_namelen;		
    char mpi_name[MPI_MAX_PROCESSOR_NAME];

    pid = getpid();
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    MPI_Get_processor_name(mpi_name, &mpi_namelen);

    if (MAINPROCESS)
	while ((stat(greenlight, &statbuf) == -1) && loops < maxloop){
	    if (!loops++){
		printf("Proc %d (%*s, %d): to debug, attach %d\n",
		    mpi_rank, mpi_namelen, mpi_name, pid, pid);
	    }
	    printf("waiting(%ds) for file %s ...\n", time_int, greenlight);
	    fflush(stdout);
	    sleep(time_int);
	}
    MPI_Barrier(MPI_COMM_WORLD);
}

/* Use the Profile feature of MPI to call the pause_proc() */
int MPI_Init(int *argc, char ***argv)
{
    int ret_code;
    ret_code=PMPI_Init(argc, argv);
    pause_proc();
    return (ret_code);
}
#endif	/* USE_PAUSE */

#if 0		/* temp. disabled */
int MPI_Type_commit(MPI_Datatype *mpi_type)
{
    int ret_code;
    ret_code=PMPI_Type_commit(mpi_type);
    printf("PMPI_Type_commit ret_code=%d, mpi_type=%d\n", ret_code, *mpi_type);
    return (ret_code);
}

int MPI_Type_free(MPI_Datatype *mpi_type)
{
    int ret_code;
    printf("PMPI_Type_free mpi_type=%d, ", *mpi_type);
    ret_code=PMPI_Type_free(mpi_type);
    printf("ret_code=%d\n", ret_code);
    return (ret_code);
}

int MPI_Type_contiguous(int count, MPI_Datatype oldtype, MPI_Datatype *newtype)
{
    int ret_code;
    ret_code=PMPI_Type_contiguous(count, oldtype, newtype);
    printf("PMPI_Type_contiguous ret_code=%d, count=%d, old_type=%d, new_type=%d\n",
	    ret_code, count, oldtype, *newtype);
    return (ret_code);
}

int MPI_Type_vector(int count, int blocklength, int stride, MPI_Datatype oldtype, MPI_Datatype *newtype) 
{
    int ret_code;
    ret_code=PMPI_Type_vector(count, blocklength, stride, oldtype, newtype);
    printf("PMPI_Type_vector ret_code=%d, count=%d, blocklength=%d, stride=%d, "
	"old_type=%d, new_type=%d\n",
	    ret_code, count, blocklength, stride, oldtype, *newtype);
    return (ret_code);
}

int MPI_Type_struct(int count, int *array_of_blocklengths, MPI_Aint *array_of_displacements, MPI_Datatype *array_of_types, MPI_Datatype *newtype)
{
    int ret_code;
    ret_code=PMPI_Type_struct(count, array_of_blocklengths, array_of_displacements, array_of_types, newtype);
    printf("PMPI_Type_struct ret_code=%d, new_type=%d\n",
	    ret_code, *newtype);
    return (ret_code);
}

#ifdef HAVE_MPI2
int MPI_Type_create_resized(MPI_Datatype oldtype, MPI_Aint lb, MPI_Aint extent, MPI_Datatype *newtype)
{
    int ret_code;
    ret_code=PMPI_Type_create_resized(oldtype, lb, extent, newtype);
    printf("PMPI_Type_create_resized ret_code=%d, lb=%d, extent=%d, old_type=%d, new_type=%d\n",
	    ret_code, lb, extent, oldtype, *newtype);
    return (ret_code);
}
#endif

#endif



/*
 * Show command usage
 */
void
usage(void)
{
    printf("Usage: testphdf5 [-r] [-w] [-v] [-m<n_datasets>] "
	"[-f <prefix>] [-d <dim0> <dim1>]\n");
    printf("\t-r\t\tno read test\n");
    printf("\t-w\t\tno write test\n");
    printf("\t-m<n_datasets>"
	"\tset number of datasets for the multiple dataset test\n");
    printf("\t-v\t\tverbose on\n");
    printf("\t-f <prefix>\tfilename prefix\n");
    printf("\t-d <dim0> <dim1>\tdataset dimensions\n");
    printf("\t-c <dim0> <dim1>\tdataset chunk dimensions\n");
    printf("\tDefault: do write then read with dimensions %dx%d\n",
	DIM0, DIM1);
    printf("\n");
}


/*
 * parse the command line options
 */
int
parse_options(int argc, char **argv)
{
    int mpi_size, mpi_rank;				/* mpi variables */

    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    /* setup default chunk-size. Make sure sizes are > 0 */
    chunkdim0 = (dim0+9)/10;
    chunkdim1 = (dim1+9)/10;

    while (--argc){
	if (**(++argv) != '-'){
	    break;
	}else{
	    switch(*(*argv+1)){
		case 'r':   doread = 0;
			    break;
		case 'w':   dowrite = 0;
			    break;
		case 'm':   ndatasets = atoi((*argv+1)+1);
			    if (ndatasets < 0){
				nerrors++;
				return(1);
			    }
			    break;
		case 'v':   verbose = 1;
			    break;
		case 'f':   if (--argc < 1) {
				nerrors++;
				return(1);
			    }
			    if (**(++argv) == '-') {
				nerrors++;
				return(1);
			    }
			    paraprefix = *argv;
			    break;
		case 'd':   /* dimensizes */
			    if (--argc < 2){
				nerrors++;
				return(1);
			    }
			    dim0 = atoi(*(++argv));
			    argc--;
			    dim1 = atoi(*(++argv));
			    /* set default chunkdim sizes too */
			    chunkdim0 = (dim0+9)/10;
			    chunkdim1 = (dim1+9)/10;
			    break;
		case 'c':   /* chunk dimensions */
			    if (--argc < 2){
				nerrors++;
				return(1);
			    }
			    chunkdim0 = atoi(*(++argv));
			    argc--;
			    chunkdim1 = atoi(*(++argv));
			    break;
		case 'h':   /* print help message--return with nerrors set */
			    return(1);
		default:    nerrors++;
			    return(1);
	    }
	}
    } /*while*/

    /* check validity of dimension and chunk sizes */
    if (dim0 <= 0 || dim1 <= 0){
	printf("Illegal dim sizes (%d, %d)\n", dim0, dim1);
	nerrors++;
	return(1);
    }
    if (chunkdim0 <= 0 || chunkdim1 <= 0){
	printf("Illegal chunkdim sizes (%d, %d)\n", chunkdim0, chunkdim1);
	nerrors++;
	return(1);
    }

    /* Make sure datasets can be divided into equal portions by the processes */
    if ((dim0 % mpi_size) || (dim1 % mpi_size)){
	if (MAINPROCESS)
	    printf("dim0(%d) and dim1(%d) must be multiples of processes(%d)\n",
		    dim0, dim1, mpi_size);
	nerrors++;
	return(1);
    }

    /* compose the test filenames */
    {
	int i, n;

	n = sizeof(FILENAME)/sizeof(FILENAME[0]) - 1;	/* exclude the NULL */

	for (i=0; i < n; i++)
	    if (h5_fixname(FILENAME[i],fapl,filenames[i],sizeof(filenames[i]))
		== NULL){
		printf("h5_fixname failed\n");
		nerrors++;
		return(1);
	    }
	printf("Test filenames are:\n");
	for (i=0; i < n; i++)
	    printf("    %s\n", filenames[i]);
    }

    return(0);
}


main(int argc, char **argv)
{
    int mpi_size, mpi_rank;				/* mpi variables */

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    if (MAINPROCESS){
	printf("===================================\n");
	printf("PHDF5 TESTS START\n");
	printf("===================================\n");
    }
    fapl = H5Pcreate (H5P_FILE_ACCESS);
    H5Pset_fapl_mpio(fapl, MPI_COMM_WORLD, MPI_INFO_NULL);

    if (parse_options(argc, argv) != 0){
	if (MAINPROCESS)
	    usage();
	goto finish;
    }

	if (ndatasets){
	    MPI_BANNER("multiple datasets write ...");
	    multiple_dset_write(filenames[3], ndatasets);
	}
	else{
	    MPI_BANNER("Multiple datasets test skipped");
	}

    if (dowrite){
	MPI_BANNER("dataset using split communicators...");
	test_split_comm_access(filenames[0]);

	MPI_BANNER("dataset independent write...");
	dataset_writeInd(filenames[0]);

	MPI_BANNER("dataset collective write...");
	dataset_writeAll(filenames[1]);

	MPI_BANNER("extendible dataset independent write...");
	extend_writeInd(filenames[2]);
    }
    else{
	MPI_BANNER("write tests skipped");
    }

    if (doread){
	MPI_BANNER("dataset independent read...");
	dataset_readInd(filenames[0]);

	MPI_BANNER("dataset collective read...");
	dataset_readAll(filenames[1]);

	MPI_BANNER("extendible dataset independent read...");
	extend_readInd(filenames[2]);
    }
    else{
	MPI_BANNER("read tests skipped");
    }

    if (!(dowrite || doread || ndatasets)){
	usage();
	nerrors++;
    }

finish:
    if (MAINPROCESS){		/* only process 0 reports */
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
    if (dowrite)
	h5_cleanup(FILENAME, fapl);
    else
	H5Pclose(fapl);
    return(nerrors);
}

