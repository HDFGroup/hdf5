/*
 * MPIO independent overlapping writes.
 *
 * First n-1 processes open 1 file.
 * Each of the n-1 process writes chunks of data to the file in round-robin
 * fashion, in a interleaved but not overlapped fashion.  Using increasing
 * chunk sizes for the benefits of testing different write sizes and also
 * reducing the numbers of writes.
 *
 * Last process (n-1) just waits.
 * First n-1 processes finish writing and cloose the file.
 * Last process opens the same file and verifies the data.
 */

#include <testphdf5.h>

/* FILENAME and filenames must have the same number of names */
const char *FILENAME[2]={
	    "MPItest",
	    NULL};
char	filenames[2][200];
int	nerrors;
int	verbose;
hid_t	fapl;				/* file access property list */

#define MPIO_TEST_WRITE_SIZE 1024*1024     /* 1 MB */

void
test_mpio_overlap_writes(char *filename)
{
    int mpi_size, mpi_rank;
    MPI_Comm comm;
    MPI_Info info = MPI_INFO_NULL;
    int color, mrc;
    MPI_File	fh;
    int newrank, newprocs;
    hid_t fid;			/* file IDs */
    hid_t acc_tpl;		/* File access properties */
    herr_t ret;			/* generic return value */
    int i;
    char  buf[4093];		/* use some prime number for size */
    int bufsize = sizeof(buf);
    int stride;
    MPI_Offset  mpi_off;
    MPI_Status  mpi_stat;


    if (verbose)
	printf("MPIO independent overlapping writes test on file %s\n",
	    filename);

    /* set up MPI parameters */
    MPI_Comm_size(MPI_COMM_WORLD,&mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);

    /* Need at least 2 processes */
    if (mpi_size < 2) {
	if (MAINPROCESS)
	    printf("Need at least 2 processes to run MPIO test.\n");
	    printf(" -SKIP- \n");
	return;
    }

    /* splits processes 0 to n-2 into one comm. and the last one into another */
    color = ((mpi_rank < (mpi_size - 1)) ? 0 : 1);
    mrc = MPI_Comm_split (MPI_COMM_WORLD, color, mpi_rank, &comm);
    VRFY((mrc==MPI_SUCCESS), "Comm_split succeeded");

    if (color==0){
	/* First n-1 processes (color==0) open a file and write it */
	mrc = MPI_File_open(comm, filename, MPI_MODE_CREATE|MPI_MODE_RDWR,
		info, &fh);
	VRFY((mrc==MPI_SUCCESS), "");

	stride = 1;
	mpi_off = mpi_rank*stride;
	while (mpi_off < MPIO_TEST_WRITE_SIZE){
	    /* make sure the write does not exceed the TEST_WRITE_SIZE */
	    if (mpi_off+stride > MPIO_TEST_WRITE_SIZE)
		stride = MPIO_TEST_WRITE_SIZE - mpi_off;

	    /* set data to some trivial pattern for easy verification */
	    for (i=0; i<stride; i++)
		buf[i] = (mpi_off+i) & 0x7f;
	    mrc = MPI_File_write_at(fh, mpi_off, buf, stride, MPI_BYTE,
		    &mpi_stat);
	    VRFY((mrc==MPI_SUCCESS), "");
	    
	    /* move the offset pointer to last byte written by all processes */
	    mpi_off += (mpi_size - 1 - mpi_rank) * stride;

	    /* Increase chunk size without exceeding buffer size. */
	    /* Then move the starting offset for next write. */
	    stride *= 2;
	    if (stride > bufsize)
		stride = bufsize;
	    mpi_off += mpi_rank*stride;
	}

	/* close file and free the communicator */
	mrc = MPI_File_close(&fh);
	VRFY((mrc==MPI_SUCCESS), "MPI_FILE_CLOSE");
	mrc = MPI_Comm_free(&comm);
	VRFY((mrc==MPI_SUCCESS), "MPI_Comm_free");
    
	/* sync with the other waiting processes */
	mrc = MPI_Barrier(MPI_COMM_WORLD);
	VRFY((mrc==MPI_SUCCESS), "Sync after writes");
    }else{
	/* last process waits till writes are done,
	 * then opens file to verify data.
	 */
	mrc = MPI_Barrier(MPI_COMM_WORLD);
	VRFY((mrc==MPI_SUCCESS), "Sync after writes");

	mrc = MPI_File_open(comm, filename, MPI_MODE_RDONLY,
		info, &fh);
	VRFY((mrc==MPI_SUCCESS), "");

	stride = bufsize;
	for (mpi_off=0; mpi_off < MPIO_TEST_WRITE_SIZE; mpi_off += bufsize){
	    /* make sure it does not read beyond end of data */
	    if (mpi_off+stride > MPIO_TEST_WRITE_SIZE)
		stride = MPIO_TEST_WRITE_SIZE - mpi_off;
	    mrc = MPI_File_read_at(fh, mpi_off, buf, stride, MPI_BYTE,
		    &mpi_stat);
	    VRFY((mrc==MPI_SUCCESS), "");
	    for (i=0; i<stride; i++){
		char expected;
		expected = (mpi_off+i) & 0x7f;
		if (buf[i] != expected)
		    printf("proc %d: found data error at [%ld], expect %d, got %d\n",
		    mpi_rank, mpi_off+i, expected, buf[i]);
	    }
	}

	/* close file and free the communicator */
	mrc = MPI_File_close(&fh);
	VRFY((mrc==MPI_SUCCESS), "MPI_FILE_CLOSE");
	mrc = MPI_Comm_free(&comm);
	VRFY((mrc==MPI_SUCCESS), "MPI_Comm_free");
    }

    /*
     * one more sync to ensure all processes have done reading
     * before ending this test.
     */
    mrc = MPI_Barrier(MPI_COMM_WORLD);
    VRFY((mrc==MPI_SUCCESS), "Sync before leaving test");
}


/*
 * parse the command line options
 */
int
parse_options(int argc, char **argv)
{
    while (--argc){
	if (**(++argv) != '-'){
	    break;
	}else{
	    switch(*(*argv+1)){
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
		case 'h':   /* print help message--return with nerrors set */
			    return(1);
		default:    nerrors++;
			    return(1);
	    }
	}
    } /*while*/

    /* compose the test filenames */
    {
	int i, n;
	hid_t plist;

	plist = H5Pcreate (H5P_FILE_ACCESS);
	H5Pset_fapl_mpio(plist, MPI_COMM_WORLD, MPI_INFO_NULL);
	n = sizeof(FILENAME)/sizeof(FILENAME[0]) - 1;	/* exclude the NULL */

	for (i=0; i < n; i++)
	    if (h5_fixname(FILENAME[i],plist,filenames[i],sizeof(filenames[i]))
		== NULL){
		printf("h5_fixname failed\n");
		nerrors++;
		return(1);
	    }
	H5Pclose(plist);
	printf("Test filenames are:\n");
	for (i=0; i < n; i++)
	    printf("    %s\n", filenames[i]);
    }

    return(0);
}


/*
 * Show command usage
 */
void
usage(void)
{
    printf("Usage: t_mpi [-v] [-f <prefix>]\n");
    printf("\t-v\t\tverbose on\n");
    printf("\t-f <prefix>\tfilename prefix\n");
    printf("\n");
}


main(int argc, char **argv)
{
    int mpi_size, mpi_rank;				/* mpi variables */

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    if (MAINPROCESS){
	printf("===================================\n");
	printf("MPI functionality tests\n");
	printf("===================================\n");
    }
    fapl = H5Pcreate (H5P_FILE_ACCESS);
    H5Pset_fapl_mpio(fapl, MPI_COMM_WORLD, MPI_INFO_NULL);

    if (parse_options(argc, argv) != 0){
	if (MAINPROCESS)
	    usage();
	goto finish;
    }

	MPI_BANNER("MPIO independent overlapping writes...");
	test_mpio_overlap_writes(filenames[0]);

finish:
    if (MAINPROCESS){		/* only process 0 reports */
	printf("===================================\n");
	if (nerrors){
	    printf("***MPI tests detected %d errors***\n", nerrors);
	}
	else{
	    printf("MPI tests finished with no errors\n");
	}
	printf("===================================\n");
    }
    MPI_Finalize();
    h5_cleanup(FILENAME, fapl);
    return(nerrors);
}

