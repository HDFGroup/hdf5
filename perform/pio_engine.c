/* 
 * Author: Albert Cheng of NCSA, Oct 24, 2001.
 */

#include "hdf5.h"
#ifdef H5_HAVE_PARALLEL
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include "pio_perf.h"
#include "pio_timer.h"
#ifdef OLDSTUFF
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <sys/time.h>
#endif /* OLDSTUFF */
#include <mpi.h>
#ifndef MPI_FILE_NULL           /*MPIO may be defined in mpi.h already       */
#   include <mpio.h>
#endif


/* Macro definitions */
#define GOTOERROR(errcode)	{ret_code=errcode; goto done;}
#define GOTODONE		{goto done;}
#define ERRMSG(mesg) {                                            \
	fprintf(stderr, "Proc %d: ", myrank);				       \
        fprintf(stderr, "*** Assertion failed (%s) at line %4d in %s\n",       \
	    mesg, (int)__LINE__, __FILE__);     			       \
}
#define MSG(mesg) {                                            \
	fprintf(stderr, "Proc %d: ", myrank);				       \
        fprintf(stderr, "(%s) at line %4d in %s\n",       \
	    mesg, (int)__LINE__, __FILE__);     			       \
}
/* Verify:
 * if val is false (0), print mesg.
 */
#define VRFY(val, mesg) do {                                    \
    if (!val) {                                                 \
	ERRMSG(mesg);						\
	GOTOERROR(1);					        \
    }                                                           \
} while(0)

#ifndef HDmalloc
#define HDmalloc(x)		malloc(x)
#endif

#ifndef HDfree
#define HDfree(x)		free(x)
#endif

#ifndef HDopen
#ifdef O_BINARY
#define HDopen(S,F,M)           open(S,F|_O_BINARY,M)
#else
#define HDopen(S,F,M)           open(S,F,M)
#endif
#endif

#ifndef HDclose
#define HDclose(F)		close(F)
#endif

#ifndef HDseek
#define HDseek(F,L,W)		lseek(F,L,W)
#endif

#ifndef HDwrite
#define HDwrite(F,B,S)		write(F,B,S)
#endif

#ifndef HDread
#define HDread(F,B,S)		read(F,B,S)
#endif

/* Raw I/O macros */
#define RAWCREATE(fn)		HDopen(fn, O_CREAT|O_TRUNC|O_RDWR, 0600)
#define RAWOPEN(fn, F)	HDopen(fn, F, 0600)
#define RAWCLOSE(F)	HDclose(F)
#define RAWSEEK(F,L)	HDseek(F,L,SEEK_SET)
#define RAWWRITE(F,B,S)	HDwrite(F,B,S)
#define RAWREAD(F,B,S)	HDread(F,B,S)



#ifdef OLDSTUFF
hid_t dataset;		/* Dataset ID */
char	*meta_ext, *raw_ext;	/* holds the meta and raw file extension if */
				/* opt_split_vfd is set */


/* DEFAULT VALUES FOR OPTIONS */
int64_t opt_block     = 1048576*16;
int     opt_iter      = 1;
int     opt_stripe    = -1;
int     opt_correct   = 0;
int     amode         = O_RDWR | O_CREAT;
char    opt_file[256] = "/tmp/test.out\0";
char    opt_pvfstab[256] = "notset\0";
int     opt_pvfstab_set = 0;

/* function prototypes */
double Wtime(void);

extern int errno;
extern int debug_on;

/* globals needed for getopt */
extern char *optarg;
extern int optind, opterr;
#endif  /* old stuff */

int
dopio(parameters param)
{
    MPI_Comm	comm = MPI_COMM_NULL;
    int		myrank;
    int		nprocs=1;
    int 		ret_code=0;			/* return code */
    iotype		iot;
    char		fname[256];
    unsigned int	maxprocs;
    unsigned int	nfiles, nf;
    unsigned long	ndsets, nd;
    unsigned long	nelmts;
    unsigned int	niters;
    unsigned long	nelmts_towrite, nelmts_written;
    unsigned long	nelmts_toread, nelmts_read;
    size_t		elmt_size;
    off_t		dset_offset;		/*dataset offset in a file */
    off_t		next_offset;		/*offset of next I/O */
    int			rc;			/*routine return code */
    int			color;			/*for communicator creation */
    int			mrc;			/*mpi return code */
    char		*buffer=NULL;		/*data buffer pointer */
    size_t		buffer_size=1024*1024;	/*data buffer size, 1MB */
    size_t		nelmts_in_buffer;	/*Number of elmts the buffer*/
    						/*can hold.*/
    size_t		dset_size;		/*one dataset size in Byte*/
    int			rawfd=-1;		/*Raw IO file handle */
    MPI_File		mpifd=MPI_FILE_NULL;	/*MPI IO file handle */
    /* hdf5 variables */
    hid_t		acc_tpl=-1;		/* File access templates */
    hid_t		h5fd=-1;		/*HDF5 IO file handle */
    herr_t		hrc;         		/*HDF5 return code */
    hsize_t		h5dims[1];   		/* dataset dim sizes */
    hsize_t		h5block[1], h5stride[1], h5count[1];
    hssize_t		h5start[1];
    hid_t		h5dset_space_id = -1;  	/*Dataset space ID */
    hid_t		h5mem_space_id = -1;	/*memory dataspace ID */
    char		dname[64];		/*dataset name */
    hid_t		h5ds_id = -1;		/*Dataset handle*/

#ifdef OLDSTUFF
	char *buf, *tmp, *buf2, *tmp2, *check;
	int i, j, myrank=0, nprocs=1, err, my_correct = 1, correct, myerrno;
	double stim, etim;
	double write_tim = 0;
	double read_tim = 0;
	double read_bw, write_bw;
	double max_read_tim, max_write_tim;
	double min_read_tim, min_write_tim;
	double ave_read_tim, ave_write_tim;
	int64_t iter_jump = 0;
	int64_t seek_position = 0;
	MPI_File fh;
	MPI_Status status;
	int nchars;
#endif		/* OLDSTUFF */


    /*
     *Setup parameters and sanity check
     */
    /* IO type */
    iot = param.io_type;
    switch (iot) {
    case RAW:
	/* nothing */
	break;
    case MPIO:
	break;
    case PHDF5:
	break;
    default:
	/* unknown request */
	fprintf(stderr, "Unknown IO type request (%d)\n", iot);
	GOTOERROR(1);
    }

    /* number of files */
    nfiles = param.num_files;

    /* number of datasets per file */
    ndsets = param.num_dsets;

    /* number of elements per dataset */
    nelmts = param.num_elmts;
    if (nelmts == 0 ){
	fprintf(stderr,
	    "number of elements per dataset must be positive (%lu)\n",
	    nelmts);
	GOTOERROR(1);
    }

    /* number of iterations of reads or writes */
    niters = param.num_iters;

    /* maximun number of mpi-processes to use */
    maxprocs = param.max_num_procs;
    if (maxprocs == 0 ){
	fprintf(stderr,
	    "maximun number of process to use must be positive (%u)\n",
	    maxprocs);
	GOTOERROR(1);
    }
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
    if (maxprocs > nprocs){
	fprintf(stderr,
	    "maximun number of process(%d) must be <= process in MPI_COMM_WORLD(%d)\n",
	    maxprocs, nprocs);
	GOTOERROR(1);
    }


/* DEBUG*/
fprintf(stderr, "nfiles=%u\n", nfiles);
fprintf(stderr, "ndsets=%lu\n", ndsets);
fprintf(stderr, "nelmts=%lu\n", nelmts);
fprintf(stderr, "niters=%u\n", niters);
fprintf(stderr, "maxprocs=%u\n", maxprocs);
nfiles=3;
/*ndsets=5; */

    /*
     *Create a sub communicator for this run
     *Easier to use the first N processes.
     */
    MPI_Comm_rank(comm, &myrank);
    color = (myrank < maxprocs);
    mrc = MPI_Comm_split (MPI_COMM_WORLD, color, myrank, &comm);
    if (mrc!=MPI_SUCCESS) {
	fprintf(stderr, "MPI_Comm_split failed\n");
	GOTOERROR(1);
    }

    if (!color){	/* not involved in this run */
	mrc = MPI_Comm_free(&comm);
	GOTODONE;
    }
    
    /* determine the mpi rank of in the new comm */
    MPI_Comm_size(comm, &nprocs);
    MPI_Comm_rank(comm, &myrank);

    /* Calculate dataset parameters */
    /* Data type is always native C int */
    elmt_size = sizeof(int);
    dset_size = nelmts * elmt_size;

    /* allocate data buffer */
    buffer = HDmalloc(buffer_size);
    if (buffer == NULL){
	fprintf(stderr, "malloc for data buffer failed\n");
	GOTOERROR(1);
    }
    nelmts_in_buffer = buffer_size/elmt_size;

    /* hdf5 dataset setup */
    if (iot == PHDF5){
	/* define a contiquous dataset of nelmts native ints */
	h5dims[0] = nelmts;
	h5dset_space_id = H5Screate_simple (1, h5dims, NULL);
	VRFY((h5dset_space_id >= 0), "H5Screate_simple");

	/* create the memory dataspace */
	h5dims[0] = nelmts_in_buffer;
	h5mem_space_id = H5Screate_simple (1, h5dims, NULL);
	VRFY((h5mem_space_id >= 0), "H5Screate_simple");
    }
    




    for (nf=1; nf <= nfiles; nf++){
	/*
	 *Open file for write
	 */
MSG("creating file");
	sprintf(fname, "#pio_tmp_%u", nf);
	switch (iot) {
	case RAW:
	    strcat(fname, ".raw");
	    rawfd = RAWCREATE(fname);
	    if (rawfd < 0 ){
		fprintf(stderr, "Raw File Create failed(%s)\n", fname);
		GOTOERROR(1);
	    }
	    break;
	case MPIO:
	    strcat(fname, ".mpio");
	    mrc = MPI_File_open(comm, fname, MPI_MODE_CREATE|MPI_MODE_RDWR,
		MPI_INFO_NULL, &mpifd);
	    if (mrc != MPI_SUCCESS){
		fprintf(stderr, "MPI File Create failed(%s)\n", fname);
		GOTOERROR(1);
	    }
	    break;
	case PHDF5:
	    strcat(fname, ".h5");
	    acc_tpl = H5Pcreate (H5P_FILE_ACCESS);
	    VRFY((acc_tpl >= 0), "");
	    hrc = H5Pset_fapl_mpio(acc_tpl, comm, MPI_INFO_NULL);     
	    VRFY((hrc >= 0), "");
	    /*do not close acc_tpl. It will used to open file for read later*/
	    /* create the parallel file */
	    h5fd=H5Fcreate(fname,H5F_ACC_TRUNC,H5P_DEFAULT,acc_tpl);
	    if (h5fd < 0){
		fprintf(stderr, "HDF5 file Create failed(%s)\n", fname);
		GOTOERROR(1);
	    }
	    break;
	} 

	for (nd=1; nd <= ndsets; nd++){
	    /* Calculate dataset offset within a file */

	    /* create dataset */
	    switch (iot){
	    case RAW:
	    case MPIO:
		/* both raw and mpi io just need dataset offset in file*/
		dset_offset = (nd-1)*dset_size;
		break;
	    case PHDF5:
		sprintf(dname, "Dataset_%lu", nd);
		h5ds_id = H5Dcreate(h5fd, dname, H5T_NATIVE_INT, h5dset_space_id,
				    H5P_DEFAULT);
		VRFY((h5ds_id >= 0), "H5Dcreate");
		break;
	    }
		

	    nelmts_written = 0 ;
	    while (nelmts_written < nelmts){
		nelmts_towrite = nelmts - nelmts_written;
		if (nelmts - nelmts_written >= nelmts_in_buffer){
		    nelmts_towrite = nelmts_in_buffer;
		}else{
		    /* last write of a partial buffer */
		    nelmts_towrite = nelmts - nelmts_written;
		}

		/*Prepare write data*/
		{
		    int *intptr = (int*)buffer;
		    int i;
		    for (i=0; i<nelmts_towrite; i++){
			*intptr++ = nelmts_towrite+i;
		    }
		}


		/* Write */

		/* Calculate offset of write within a dataset/file */
		switch (iot){
		case RAW:
		    RAWSEEK(rawfd, dset_offset+nelmts_written*elmt_size);
		    RAWWRITE(rawfd, buffer, nelmts_towrite*elmt_size);
		    break;
		case MPIO:
		    break;
		}


		nelmts_written += nelmts_towrite;
fprintf(stderr, "wrote %lu elmts, %lu written\n", nelmts_towrite, nelmts_written);
	    }
	/* Calculate write time */

	    /* Close dataset. Only HDF5 needs to do an explicit close. */
	    if (iot == PHDF5){
		hrc = H5Dclose(h5ds_id);
		VRFY((hrc>=0), "HDF5 Dataset Close failed\n");
		h5ds_id = -1;
	    }
	}

	/* Close file for write */
MSG("closing write file");
	switch (iot) {
	case RAW:
	    rc = RAWCLOSE(rawfd);
	    VRFY((rc==0), "HDclose");
	    rawfd = -1;
	    break;
	case MPIO:
	    mrc = MPI_File_close(&mpifd);
	    if (mrc != MPI_SUCCESS){
		fprintf(stderr, "MPI File close failed\n");
		GOTOERROR(1);
	    }
	    break;
	case PHDF5:
	    hrc=H5Fclose(h5fd);
	    if (hrc < 0){
		fprintf(stderr, "HDF5 File Close failed(%s)\n", fname);
		GOTOERROR(1);
	    }else{
		h5fd=-1;
	    }
	    break;
	} 




    /* Open file for read */
MSG("opening file to read");
	switch (iot) {
	case RAW:
	    rawfd = RAWOPEN(fname, O_RDONLY);
	    if (rawfd < 0 ){
		fprintf(stderr, "Raw File Open failed(%s)\n", fname);
		GOTOERROR(1);
	    }
	    break;
	case MPIO:
	    mrc = MPI_File_open(comm, fname, MPI_MODE_RDONLY,
		MPI_INFO_NULL, &mpifd);
	    if (mrc != MPI_SUCCESS){
		fprintf(stderr, "MPI File Open failed(%s)\n", fname);
		GOTOERROR(1);
	    }
	    break;
	case PHDF5:
	    /* open the parallel file */
	    h5fd=H5Fopen(fname,H5P_DEFAULT,acc_tpl);
	    if (h5fd < 0){
		fprintf(stderr, "HDF5 file Open failed(%s)\n", fname);
		GOTOERROR(1);
	    }
	    /* can release acc_tpl now */
	    hrc = H5Pclose(acc_tpl);
	    VRFY((hrc >= 0), "H5Pclose");
	    acc_tpl = -1;
	    break;
	} 

    /* Calculate dataset offset within a file */

    /* Open dataset for read */

    /* Prepare read */

    /* Calculate offset of read within a dataset/file */

    /* Read */

    /* Calculate read time */

    /* Close dataset for read */

    /* Close file for read */
MSG("closing read file");
	switch (iot) {
	case RAW:
	    rc = RAWCLOSE(rawfd);
	    VRFY((rc==0), "HDclose");
	    rawfd = -1;
	    break;
	case MPIO:
	    mrc = MPI_File_close(&mpifd);
	    if (mrc != MPI_SUCCESS){
		fprintf(stderr, "MPI File close failed\n");
		GOTOERROR(1);
	    }
	    break;
	case PHDF5:
	    hrc=H5Fclose(h5fd);
	    if (h5fd < 0){
		fprintf(stderr, "HDF5 file Create failed(%s)\n", fname);
		GOTOERROR(1);
	    }else{
		h5fd=-1;
	    }
	    break;
	} 

    }

done:
    /* clean up */
    /* release HDF5 objects */
    if (acc_tpl != -1){
	hrc = H5Pclose(acc_tpl);
	if (hrc < 0){
	    fprintf(stderr, "HDF5 Property List Close failed\n");
	    ret_code=1;
	}
	else
	    acc_tpl = -1;
    }
    if (h5dset_space_id != -1){
	hrc = H5Sclose(h5dset_space_id);
	if (hrc < 0){
	    fprintf(stderr, "HDF5 Dataset Space Close failed\n");
	    ret_code=1;
	}
	else
	    h5dset_space_id = -1;
    }
    if (h5mem_space_id != -1){
	hrc = H5Sclose(h5mem_space_id);
	if (hrc < 0){
	    fprintf(stderr, "HDF5 Memory Space Close failed\n");
	    ret_code=1;
	}
	else
	    h5mem_space_id = -1;
    }
    if (h5ds_id != -1){
	hrc = H5Dclose(h5ds_id);
	if (hrc < 0){
	    fprintf(stderr, "HDF5 Dataset Close failed\n");
	    ret_code=1;
	}
	else
	    h5ds_id = -1;
    }

    /* close any opened files */
    if (rawfd != -1){
	rc = HDclose(rawfd);
	if (rc != 0){
	    ERRMSG("Raw file close failed");
	    ret_code=1;
	}
	else
	    rawfd = -1;
    }
    if (mpifd != MPI_FILE_NULL){
	MPI_File_close(&mpifd);
    }
    if (h5fd != -1){
	H5Fclose(h5fd);
	h5fd=-1;
    }
    /* release MPI resources */
    if (comm != MPI_COMM_NULL){
	MPI_Comm_free(&comm);
    }
    /* release generic resources */
    if (buffer != NULL){
	HDfree(buffer);
	buffer = NULL;
    }
fprintf(stderr, "returning with ret_code=%d\n", ret_code);
    return(ret_code);
}

#ifdef OLDSTUFF
int
original_main()
{

	/* parse the command line arguments */
	parse_args(argc, argv);

	if (mynod == 0) printf("# Using hdf5-io calls.\n");

	
	/* kindof a weird hack- if the location of the pvfstab file was 
	 * specified on the command line, then spit out this location into
	 * the appropriate environment variable: */
	
#if H5_HAVE_SETENV
/* no setenv or unsetenv */
	if (opt_pvfstab_set) {
		if((setenv("PVFSTAB_FILE", opt_pvfstab, 1)) < 0){
			perror("setenv");
			goto die_jar_jar_die;
		}
	}
#endif
	
	/* this is how much of the file data is covered on each iteration of
	 * the test.  used to help determine the seek offset on each
	 * iteration */
	iter_jump = nprocs * opt_block;
		
	/* setup a buffer of data to write */
	if (!(tmp = (char *) malloc(opt_block + 256))) {
		perror("malloc");
		goto die_jar_jar_die;
	}
	buf = tmp + 128 - (((long)tmp) % 128);  /* align buffer */

	if (opt_correct) {
		/* do the same buffer setup for verifiable data */
		if (!(tmp2 = (char *) malloc(opt_block + 256))) {
			perror("malloc2");
			goto die_jar_jar_die;
		 }
		buf2 = tmp + 128 - (((long)tmp) % 128);
	}

    /* setup file access template with parallel IO access. */
    if (opt_split_vfd){
	hid_t mpio_pl;

	mpio_pl = H5Pcreate (H5P_FILE_ACCESS);
	VRFY((acc_tpl >= 0), "", H5FATAL);
	ret = H5Pset_fapl_mpio(mpio_pl, MPI_COMM_WORLD, MPI_INFO_NULL);     
	VRFY((ret >= 0), "", H5FATAL);

	/* set optional allocation alignment */
	if (opt_alignment*opt_threshold != 1){
	    ret = H5Pset_alignment(acc_tpl, opt_threshold, opt_alignment );
	    VRFY((ret >= 0), "H5Pset_alignment succeeded", !H5FATAL);
	}

	/* setup file access template */
	acc_tpl = H5Pcreate (H5P_FILE_ACCESS);
	VRFY((acc_tpl >= 0), "", H5FATAL);
	ret = H5Pset_fapl_split(acc_tpl, meta_ext, mpio_pl, raw_ext, mpio_pl);
	VRFY((ret >= 0), "H5Pset_fapl_split succeeded", H5FATAL);
    }else{

	/* set optional allocation alignment */
	if (opt_alignment*opt_threshold != 1){
	    ret = H5Pset_alignment(acc_tpl, opt_threshold, opt_alignment );
	    VRFY((ret >= 0), "H5Pset_alignment succeeded", !H5FATAL);
	}
    }



	/* now each process writes a block of opt_block chars in round robbin
	 * fashion until the whole dataset is covered.
	 */
	for (j=0; j < opt_iter; j++) {
	    /* setup a file dataspace selection */
	    start[0] = (j*iter_jump)+(mynod*opt_block);
	    stride[0] = block[0] = opt_block;
	    count[0]= 1;
	    ret=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block); 
	    VRFY((ret >= 0), "H5Sset_hyperslab succeeded", H5FATAL);

		if (opt_correct) /* fill in buffer for iteration */ {
			for (i=mynod+j, check=buf; i<opt_block; i++,check++) *check=(char)i;
		}

		/* discover the starting time of the operation */
	   MPI_Barrier(MPI_COMM_WORLD);
	   stim = MPI_Wtime();

    /* write data */
    ret = H5Dwrite(dataset, H5T_NATIVE_CHAR, mem_dataspace, file_dataspace,
	    H5P_DEFAULT, buf);					    
    VRFY((ret >= 0), "H5Dwrite dataset1 succeeded", !H5FATAL);

		/* discover the ending time of the operation */
	   etim = MPI_Wtime();

	   write_tim += (etim - stim);
		
		/* we are done with this "write" iteration */
	}

    /* close dataset and file */					    
    ret=H5Dclose(dataset);
    VRFY((ret >= 0), "H5Dclose succeeded", H5FATAL);
    ret=H5Fclose(fid);							    
    VRFY((ret >= 0), "H5Fclose succeeded", H5FATAL);



	/* wait for everyone to synchronize at this point */
	MPI_Barrier(MPI_COMM_WORLD);

    /* reopen the file for reading */
    fid=H5Fopen(opt_file,H5F_ACC_RDONLY,acc_tpl);
    VRFY((fid >= 0), "", H5FATAL);

    /* open the dataset */
    dataset = H5Dopen(fid, "Dataset1");
    VRFY((dataset >= 0), "H5Dopen succeeded", H5FATAL);

    /* we can re-use the same mem_dataspace and file_dataspace
     * the H5Dwrite used since the dimension size is the same.
     */

	/* we are going to repeat the read the same pattern the write used */
	for (j=0; j < opt_iter; j++) {
	    /* setup a file dataspace selection */
	    start[0] = (j*iter_jump)+(mynod*opt_block);
	    stride[0] = block[0] = opt_block;
	    count[0]= 1;
	    ret=H5Sselect_hyperslab(file_dataspace, H5S_SELECT_SET, start, stride, count, block); 
	    VRFY((ret >= 0), "H5Sset_hyperslab succeeded", H5FATAL);
		/* seek to the appropriate spot give the current iteration and
		 * rank within the MPI processes */

		/* discover the start time */
	   MPI_Barrier(MPI_COMM_WORLD);
	   stim = MPI_Wtime();

    /* read data */
		/* read in the file data */
		if (!opt_correct){
    ret = H5Dread(dataset, H5T_NATIVE_CHAR, mem_dataspace, file_dataspace,
	    H5P_DEFAULT, buf);					    
		}
		else{
    ret = H5Dread(dataset, H5T_NATIVE_CHAR, mem_dataspace, file_dataspace,
	    H5P_DEFAULT, buf2);					    
		}
		myerrno = errno;
		/* discover the end time */
	   etim = MPI_Wtime();
	   read_tim += (etim - stim);
    VRFY((ret >= 0), "H5Dwrite dataset1 succeeded", !H5FATAL);


	   if (ret < 0) fprintf(stderr, "node %d, read error, loc = %Ld: %s\n",
			mynod, mynod*opt_block, strerror(myerrno));

		/* if the user wanted to check correctness, compare the write
		 * buffer to the read buffer */
		if (opt_correct && memcmp(buf, buf2, opt_block)) {
			fprintf(stderr, "node %d, correctness test failed\n", mynod);
			my_correct = 0;
			MPI_Allreduce(&my_correct, &correct, 1, MPI_INT, MPI_MIN,
				MPI_COMM_WORLD);
		}

		/* we are done with this read iteration */
	}

    /* close dataset and file */					    
    ret=H5Dclose(dataset);
    VRFY((ret >= 0), "H5Dclose succeeded", H5FATAL);
    ret=H5Fclose(fid);							    
    VRFY((ret >= 0), "H5Fclose succeeded", H5FATAL);

	/* compute the read and write times */
	MPI_Allreduce(&read_tim, &max_read_tim, 1, MPI_DOUBLE, MPI_MAX,
		MPI_COMM_WORLD);
	MPI_Allreduce(&read_tim, &min_read_tim, 1, MPI_DOUBLE, MPI_MIN,
		MPI_COMM_WORLD);
	MPI_Allreduce(&read_tim, &ave_read_tim, 1, MPI_DOUBLE, MPI_SUM,
		MPI_COMM_WORLD);

	/* calculate the average from the sum */
	ave_read_tim = ave_read_tim / nprocs; 

	MPI_Allreduce(&write_tim, &max_write_tim, 1, MPI_DOUBLE, MPI_MAX,
		MPI_COMM_WORLD);
	MPI_Allreduce(&write_tim, &min_write_tim, 1, MPI_DOUBLE, MPI_MIN,
		MPI_COMM_WORLD);
	MPI_Allreduce(&write_tim, &ave_write_tim, 1, MPI_DOUBLE, MPI_SUM,
		MPI_COMM_WORLD);

	/* calculate the average from the sum */
	ave_write_tim = ave_write_tim / nprocs; 
	
	/* print out the results on one node */
	if (mynod == 0) {
	   read_bw = ((int64_t)(opt_block*nprocs*opt_iter))/(max_read_tim*1000000.0);
	   write_bw = ((int64_t)(opt_block*nprocs*opt_iter))/(max_write_tim*1000000.0);
		
			printf("nr_procs = %d, nr_iter = %d, blk_sz = %ld\n", nprocs,
		opt_iter, (long)opt_block);
			
			printf("# total_size = %ld\n", (long)(opt_block*nprocs*opt_iter));
			
			printf("# Write:  min_time = %f, max_time = %f, mean_time = %f\n", 
				min_write_tim, max_write_tim, ave_write_tim);
			printf("# Read:  min_time = %f, max_time = %f, mean_time = %f\n", 
				min_read_tim, max_read_tim, ave_read_tim);
		
	   printf("Write bandwidth = %f Mbytes/sec\n", write_bw);
	   printf("Read bandwidth = %f Mbytes/sec\n", read_bw);
		
		if (opt_correct) {
			printf("Correctness test %s.\n", correct ? "passed" : "failed");
		}
	}


die_jar_jar_die:	

#if H5_HAVE_SETENV
/* no setenv or unsetenv */
	/* clear the environment variable if it was set earlier */
	if	(opt_pvfstab_set){
		unsetenv("PVFSTAB_FILE");
	}
#endif
	
	free(tmp);
	if (opt_correct) free(tmp2);
	MPI_Finalize();
	return(0);
}

/* Wtime() - returns current time in sec., in a double */
double Wtime()
{
	struct timeval t;
	
	gettimeofday(&t, NULL);
	return((double)t.tv_sec + (double)t.tv_usec / 1000000);
}

#endif 	/* OLDSTUFF */

#else /* H5_HAVE_PARALLEL */
/* dummy program since H5_HAVE_PARALLE is not configured in */
int
main()
{
printf("No parallel IO performance because parallel is not configured in\n");
return(0);
}
#endif /* H5_HAVE_PARALLEL */
