/* $Id$ */

/*
 * Main driver of the Parallel HDF5 tests
 */

#include <testphdf5.h>

/* global variables */
int nerrors = 0;			/* errors count */
int verbose = 0;			/* verbose, default as no. */
herr_t (*old_func)(void*);		/* previous error handler */
void *old_client_data;			/* previous error handler arg.*/

/* other option flags */
int doread=1;				/* read test */
int dowrite=1;				/* write test */


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
#ifdef POOMA_ARCH
    char    *filenames[]={ "pfs:/pfs/multi/tmp_1/your_own/ParaEg1.h5f",
			   "pfs:/pfs/multi/tmp_1/your_own/ParaEg2.h5f",
			   "pfs:/pfs/multi/tmp_1/your_own/ParaEg3.h5f" };
#else
    char    *filenames[]={ "ParaEg1.h5f",
			   "ParaEg2.h5f",
			   "ParaEg3.h5f" };
#endif

    int mpi_size, mpi_rank;				/* mpi variables */

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    if (MAINPROCESS){
	printf("===================================\n");
	printf("PHDF5 TESTS START\n");
	printf("===================================\n");
    }

    /* Make sure datasets can be divided into equal chunks by the processes */
    if ((DIM1 % mpi_size) || (DIM2 % mpi_size)){
	if (MAINPROCESS)
	    printf("DIM1(%d) and DIM2(%d) must be multiples of processes(%d)\n",
		    DIM1, DIM2, mpi_size);
	nerrors++;
	goto finish;
    }

#ifdef USE_PAUSE
    pause_proc(MPI_COMM_WORLD, argc, argv);
#endif

    if (parse_options(argc, argv) != 0)
	goto finish;

    if (dowrite){
	MPI_BANNER("testing dataset using split communicators...");
	test_split_comm_access(filenames);

	MPI_BANNER("testing dataset independent write...");
	dataset_writeInd(filenames[0]);

	MPI_BANNER("testing dataset collective write...");
	dataset_writeAll(filenames[1]);

	MPI_BANNER("testing extendable dataset independent write...");
	extend_writeInd(filenames[2]);
    }
    if (doread){
	MPI_BANNER("testing dataset independent read...");
	dataset_readInd(filenames[0]);

	MPI_BANNER("testing dataset collective read...");
	dataset_readAll(filenames[1]);

	MPI_BANNER("testing extendable dataset independent read...");
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

