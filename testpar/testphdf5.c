/* $Id$ */

/*
 * Main driver of the Parallel HDF5 tests
 */

#include <testphdf5.h>

/* global variables */
int dim0 = DIM0;
int dim1 = DIM1;
int chunkdim0;
int chunkdim1;
int nerrors = 0;			/* errors count */
int verbose = 0;			/* verbose, default as no. */

/* FilePrefix defines where the temporary parallel test files should be. */
/* In a parallel system, filesystem suitable for compiling are unlikly */
/* the right place for parallel I/O.  There is no common used pathname */
/* for the parallel file system.  So, /tmp is used as the default. */
#ifdef __PUMAGON__
/* For the PFS of TFLOPS */
char *fileprefix = "pfs:/pfs_grande/multi/tmp_1/";
#else
char *fileprefix = "/tmp/";
#endif
size_t fileprefixlen;			/* file prefix length */

herr_t (*old_func)(void*);		/* previous error handler */
void *old_client_data;			/* previous error handler arg.*/

/* other option flags */
int doread=1;				/* read test */
int dowrite=1;				/* write test */
char    *filenames[]={ "ParaEg1.h5f",
		       "ParaEg2.h5f",
		       "ParaEg3.h5f",
                       "Mdset.h5f" };



#ifdef USE_PAUSE
/* pause the process for a moment to allow debugger to attach if desired. */
/* Will pause more if greenlight file is not persent but will eventually */
/* continue. */
#include <sys/types.h>
#include <sys/stat.h>
void pause_proc(MPI_Comm comm, int argc, char **argv)
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

#ifdef DISABLED
    /* check if an pause interval option is given */
    if (--argc > 0 && isdigit(*++argv))
	time_int = atoi(*argv);
#endif
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
    MPI_Barrier(comm);
}
#endif	/* USE_PAUSE */


/*
 * Show command usage
 */
void
usage(void)
{
    printf("Usage: testphdf5 [-r] [-w] [-v] [-f <prefix>] [-d <dim0> <dim1>]\n");
    printf("\t-r\t\tno read\n");
    printf("\t-w\t\tno write\n");
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
			    fileprefix = *argv;
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

    /* compose the filenames if file prefix is defined */
    if (fileprefix != NULL) {
	char *tmpptr;
	int i;

	fileprefixlen = strlen(fileprefix);
	i = sizeof(filenames)/sizeof(filenames[0]);
	while (i-- > 0){
	    tmpptr = filenames[i];
            filenames[i] = (char *)malloc (fileprefixlen + strlen(tmpptr) + 1);
            if (!filenames[i]) {
		printf("%s\n","memory allocation failed");
		nerrors++;
		return(1);
	    }
	    strcpy(filenames[i],fileprefix);
	    strcat(filenames[i],tmpptr);
	}
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

#ifdef USE_PAUSE
    pause_proc(MPI_COMM_WORLD, argc, argv);
#endif

    if (parse_options(argc, argv) != 0){
	if (MAINPROCESS)
	    usage();
	goto finish;
    }

    if (dowrite){
	MPI_BANNER("testing MPIO independent overlapping writes...");
	test_mpio_overlap_writes(filenames);

	MPI_BANNER("testing dataset using split communicators...");
	test_split_comm_access(filenames);

	MPI_BANNER("testing dataset independent write...");
	dataset_writeInd(filenames[0]);

	MPI_BANNER("testing dataset collective write...");
	dataset_writeAll(filenames[1]);

	MPI_BANNER("testing extendible dataset independent write...");
	extend_writeInd(filenames[2]);

	MPI_BANNER("testing multiple datasets write ...");
	multiple_dset_write(filenames[3]);

    }
    if (doread){
	MPI_BANNER("testing dataset independent read...");
	dataset_readInd(filenames[0]);

	MPI_BANNER("testing dataset collective read...");
	dataset_readAll(filenames[1]);

	MPI_BANNER("testing extendible dataset independent read...");
	extend_readInd(filenames[2]);
    }

    if (!(dowrite || doread)){
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

    return(nerrors);
}

