/*
 * Copyright (C) 2001, 2002
 *     National Center for Supercomputing Applications
 *     All rights reserved.
 * 
 * Author: Albert Cheng of NCSA, Oct 24, 2001.
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>

#include "hdf5.h"

#ifdef H5_HAVE_PARALLEL

#include <mpi.h>

#ifndef MPI_FILE_NULL           /*MPIO may be defined in mpi.h already       */
#   include <mpio.h>
#endif  /* !MPI_FILE_NULL */

#ifdef H5_HAVE_GPFS
#   include <gpfs_fcntl.h>
#endif  /* H5_HAVE_GPFS */

#include "pio_perf.h"
#include "pio_timer.h"

/* Macro definitions */

/* sizes of various items. these sizes won't change during program execution */
/* The following three must have the same type */
#define ELMT_SIZE           (sizeof(int))          /* we're doing ints */
#define ELMT_MPI_TYPE       MPI_INT
#define ELMT_H5_TYPE        H5T_NATIVE_INT

#define GOTOERROR(errcode)	{ ret_code = errcode; goto done; }
#define GOTODONE		{ goto done; }
#define ERRMSG(mesg) {                                                  \
    fprintf(stderr, "Proc %d: ", pio_mpi_rank_g);                       \
    fprintf(stderr, "*** Assertion failed (%s) at line %4d in %s\n",    \
            mesg, (int)__LINE__, __FILE__);                             \
}

#define MSG(mesg) {                                     \
    fprintf(stderr, "Proc %d: ", pio_mpi_rank_g);       \
    fprintf(stderr, "(%s) at line %4d in %s\n",         \
            mesg, (int)__LINE__, __FILE__);             \
}

/* verify: if val is false (0), print mesg. */
#define VRFY(val, mesg) do {                            \
    if (!val) {                                         \
        ERRMSG(mesg);                                   \
        GOTOERROR(FAIL);                                \
    }                                                   \
} while(0)


/* POSIX I/O macros */
#define POSIXCREATE(fn)           HDopen(fn, O_CREAT|O_TRUNC|O_RDWR, 0600)
#define POSIXOPEN(fn, F)          HDopen(fn, F, 0600)
#define POSIXCLOSE(F)             HDclose(F)
#define POSIXSEEK(F,L)            HDlseek(F, L, SEEK_SET)
#define POSIXWRITE(F,B,S)         HDwrite(F,B,S)
#define POSIXREAD(F,B,S)          HDread(F,B,S)

enum {
    PIO_CREATE = 1,
    PIO_WRITE = 2,
    PIO_READ = 4
};

/* Global variables */
static int	clean_file_g = -1;	/*whether to cleanup temporary test     */
                                        /*files. -1 is not defined;             */
                                        /*0 is no cleanup; 1 is do cleanup      */

/*
 * In a parallel machine, the filesystem suitable for compiling is
 * unlikely a parallel file system that is suitable for parallel I/O.
 * There is no standard pathname for the parallel file system.  /tmp
 * is about the best guess.
 */
#ifndef HDF5_PARAPREFIX
#  ifdef __PUMAGON__
     /* For the PFS of TFLOPS */
#    define HDF5_PARAPREFIX     "pfs:/pfs_grande/multi/tmp_1"
#  else
#    define HDF5_PARAPREFIX     "/tmp"
#  endif    /* __PUMAGON__ */
#endif  /* !HDF5_PARAPREFIX */

#ifndef MIN
#   define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif  /* !MIN */

/* the different types of file descriptors we can expect */
typedef union _file_descr {
    int         posixfd;    /* POSIX file handle*/
    MPI_File    mpifd;      /* MPI file         */
    hid_t       h5fd;       /* HDF5 file        */
} file_descr;

/* local functions */
static char  *pio_create_filename(iotype iot, const char *base_name,
                                  char *fullname, size_t size);
static herr_t do_write(results *res, file_descr *fd, parameters *parms,
    long ndsets, off_t nelmts, size_t blk_size, size_t buf_size, void *buffer);
static herr_t do_read(results *res, file_descr *fd, parameters *parms,
    long ndsets, off_t nelmts, size_t blk_size, size_t buf_size, void *buffer /*out*/);
static herr_t do_fopen(parameters *param, char *fname, file_descr *fd /*out*/,
                       int flags);
static herr_t do_fclose(iotype iot, file_descr *fd);
static void do_cleanupfile(iotype iot, char *fname);

/* GPFS-specific functions */
static void access_range(int handle, off_t start, off_t length, int is_write);
static void free_range(int handle, off_t start, off_t length);
static void clear_file_cache(int handle);
static void cancel_hints(int handle);
static void start_data_shipping(int handle, int num_insts);
static void stop_data_shipping(int handle);
static void invalidate_file_cache(const char *filename);

/*
 * Function:    do_pio
 * Purpose:     PIO Engine where Parallel IO are executed.
 * Return:      results
 * Programmer:  Albert Cheng, Bill Wendling 2001/12/12
 * Modifications:
 */
results
do_pio(parameters param)
{
    /* return codes */
    herr_t      ret_code = 0;   /*return code                           */
    results     res;

    file_descr	fd;
    iotype      iot;

    char        fname[FILENAME_MAX];
    int         maxprocs;
    int		nfiles, nf;
    long        ndsets;
    off_t       nelmts;
    char        *buffer = NULL;         /*data buffer pointer           */
    size_t      buf_size;               /*data buffer size in bytes     */
    size_t      blk_size;       	/*interleaved I/O block size            */

    /* HDF5 variables */
    herr_t          hrc;                /*HDF5 return code              */

    /* Sanity check parameters */

    /* IO type */
    iot = param.io_type;

    switch (iot) {
    case MPIO:
        fd.mpifd = MPI_FILE_NULL;
        res.timers = pio_time_new(MPI_TIMER);
        break;
    case POSIXIO:
        fd.posixfd = -1;
        res.timers = pio_time_new(MPI_TIMER);
        break;
    case PHDF5:
        fd.h5fd = -1;
        res.timers = pio_time_new(MPI_TIMER);
        break;
    default:
        /* unknown request */
        fprintf(stderr, "Unknown IO type request (%d)\n", iot);
        GOTOERROR(FAIL);
    }

    nfiles = param.num_files;       /* number of files                      */
    ndsets = param.num_dsets;       /* number of datasets per file          */
    nelmts = param.num_elmts;       /* number of elements per dataset       */
    maxprocs = param.num_procs;     /* max number of mpi-processes to use   */
    buf_size = param.buf_size;
    blk_size = param.block_size;    /* interleaved IO block size            */

    if (nfiles < 0 ) {
        fprintf(stderr,
                "number of files must be >= 0 (%d)\n",
                nfiles);
        GOTOERROR(FAIL);
    }

    if (ndsets < 0 ) {
        fprintf(stderr,
                "number of datasets per file must be >= 0 (%ld)\n",
                ndsets);
        GOTOERROR(FAIL);
    }

    if (maxprocs <= 0 ) {
        fprintf(stderr,
                "maximum number of process to use must be > 0 (%d)\n",
                maxprocs);
        GOTOERROR(FAIL);
    }

    /* allocate transfer buffer */
    if(buf_size<=0) {
        HDfprintf(stderr,
	    "Transfer buffer size (%Hd) must be > 0\n", (long_long)buf_size);
	GOTOERROR(FAIL);
    }else{
        buffer = malloc(buf_size);

        if (buffer == NULL){
            HDfprintf(stderr, "malloc for transfer buffer size (%Hd) failed\n",
                (long_long)buf_size);
            GOTOERROR(FAIL);
        }
    }

    /* Should only need blk_size <= buf_size. */
    /* More restrictive condition for easier implementation for now. */
    if (blk_size > 0 && (buf_size % blk_size)){
        HDfprintf(stderr,
	    "Transfer buffer size (%Hd) must be a multiple of the "
	    "interleaved I/O block size (%Hd)\n",
	    (long_long)buf_size, (long_long)blk_size);
        GOTOERROR(FAIL);
    }

    if (pio_debug_level >= 4) {
        int myrank;

        MPI_Comm_rank(pio_comm_g, &myrank);

        /* output all of the times for all iterations */
        if (myrank == 0)
            fprintf(output, "Timer details:\n");
    }

    for (nf = 1; nf <= nfiles; nf++) {
	/*
	 * Write performance measurement
	 */
        /* Open file for write */
        char base_name[256];

        MPI_Barrier(pio_comm_g);

        sprintf(base_name, "#pio_tmp_%u", nf);
        pio_create_filename(iot, base_name, fname, sizeof(fname));

        set_time(res.timers, HDF5_GROSS_WRITE_FIXED_DIMS, START);
        hrc = do_fopen(&param, fname, &fd, PIO_CREATE | PIO_WRITE);

        VRFY((hrc == SUCCESS), "do_fopen failed");

        set_time(res.timers, HDF5_FINE_WRITE_FIXED_DIMS, START);
        hrc = do_write(&res, &fd, &param, ndsets, nelmts, blk_size, buf_size, buffer);
        set_time(res.timers, HDF5_FINE_WRITE_FIXED_DIMS, STOP);

        VRFY((hrc == SUCCESS), "do_write failed");

        /* Close file for write */
        hrc = do_fclose(iot, &fd);

        set_time(res.timers, HDF5_GROSS_WRITE_FIXED_DIMS, STOP);
        VRFY((hrc == SUCCESS), "do_fclose failed");

        if (!param.h5_write_only) {
            /*
             * Read performance measurement
             */
            MPI_Barrier(pio_comm_g);

            /* Open file for read */
            set_time(res.timers, HDF5_GROSS_READ_FIXED_DIMS, START);
            hrc = do_fopen(&param, fname, &fd, PIO_READ);

            VRFY((hrc == SUCCESS), "do_fopen failed");

            set_time(res.timers, HDF5_FINE_READ_FIXED_DIMS, START);
            hrc = do_read(&res, &fd, &param, ndsets, nelmts, blk_size, buf_size, buffer);
            set_time(res.timers, HDF5_FINE_READ_FIXED_DIMS, STOP);
            VRFY((hrc == SUCCESS), "do_read failed");

            /* Close file for read */
            hrc = do_fclose(iot, &fd);

            set_time(res.timers, HDF5_GROSS_READ_FIXED_DIMS, STOP);
            VRFY((hrc == SUCCESS), "do_fclose failed");
        }

        MPI_Barrier(pio_comm_g);
        do_cleanupfile(iot, fname);
    }

done:
    /* clean up */
    /* release HDF5 objects */

    /* close any opened files */
    /* no remove(fname) because that should have happened normally. */
    switch (iot) {
    case POSIXIO:
        if (fd.posixfd != -1)
            hrc = do_fclose(iot, &fd);
        break;
    case MPIO:
        if (fd.mpifd != MPI_FILE_NULL)
            hrc = do_fclose(iot, &fd);
        break;
    case PHDF5:
        if (fd.h5fd != -1)
            hrc = do_fclose(iot, &fd);
        break;
    }

    /* release generic resources */
    if(buffer)
        free(buffer);
    res.ret_code = ret_code;
    return res;
}

/*
 * Function:    pio_create_filename
 * Purpose:     Create a new filename to write to. Determine the correct
 *              suffix to append to the filename by the type of I/O we're
 *              doing. Also, place in the /tmp/{$USER,$LOGIN} directory if
 *              USER or LOGIN are specified in the environment.
 * Return:      Pointer to filename or NULL
 * Programmer:  Bill Wendling, 21. November 2001
 * Modifications:
 */
static char *
pio_create_filename(iotype iot, const char *base_name, char *fullname, size_t size)
{
    const char *prefix, *suffix;
    char *ptr, last = '\0';
    size_t i, j;

    if (!base_name || !fullname || size < 1)
        return NULL;

    memset(fullname, 0, size);

    switch (iot) {
    case POSIXIO:
        suffix = ".posix";
        break;
    case MPIO:
        suffix = ".mpio";
        break;
    case PHDF5:
        suffix = ".h5";
        break;
    }

    /* First use the environment variable and then try the constant */
    prefix = getenv("HDF5_PARAPREFIX");

#ifdef HDF5_PARAPREFIX
    if (!prefix)
        prefix = HDF5_PARAPREFIX;
#endif  /* HDF5_PARAPREFIX */

    /* Prepend the prefix value to the base name */
    if (prefix && *prefix) {
        /* If the prefix specifies the HDF5_PARAPREFIX directory, then
         * default to using the "/tmp/$USER" or "/tmp/$LOGIN"
         * directory instead. */
        register char *user, *login, *subdir;

        user = getenv("USER");
        login = getenv("LOGIN");
        subdir = (user ? user : login);

        if (subdir) {
            for (i = 0; i < size && prefix[i]; i++)
                fullname[i] = prefix[i];

            fullname[i++] = '/';

            for (j = 0; i < size && subdir[j]; i++, j++)
                fullname[i] = subdir[j];
        } else {
            /* We didn't append the prefix yet */
            strncpy(fullname, prefix, MIN(strlen(prefix), size));
        }

        if ((strlen(fullname) + strlen(base_name) + 1) < size) {
            /* Append the base_name with a slash first. Multiple slashes are
             * handled below. */
            h5_stat_t buf;

            if (HDstat(fullname, &buf) < 0)
                /* The directory doesn't exist just yet */
                if (mkdir(fullname, (mode_t)0755) < 0 && errno != EEXIST) {
                    /* We couldn't make the "/tmp/${USER,LOGIN}" subdirectory.
                     * Default to PREFIX's original prefix value. */
                    strcpy(fullname, prefix);
                }

            strcat(fullname, "/");
            strcat(fullname, base_name);
        } else {
            /* Buffer is too small */
            return NULL;
        }
    } else if (strlen(base_name) >= size) {
        /* Buffer is too small */
        return NULL;
    } else {
        strcpy(fullname, base_name);
    }

    /* Append a suffix */
    if (suffix) {
        if (strlen(fullname) + strlen(suffix) >= size)
            return NULL;

        strcat(fullname, suffix);
    }

    /* Remove any double slashes in the filename */
    for (ptr = fullname, i = j = 0; ptr && i < size; i++, ptr++) {
        if (*ptr != '/' || last != '/')
            fullname[j++] = *ptr;

        last = *ptr;
    }

    return fullname;
}

/*
 * Function:    do_write
 * Purpose:     Write the required amount of data to the file.
 * Return:      SUCCESS or FAIL
 * Programmer:  Albert Cheng, Bill Wendling, 2001/12/13
 * Modifications:
 */
static herr_t
do_write(results *res, file_descr *fd, parameters *parms, long ndsets,
         off_t nelmts, size_t blk_size, size_t buf_size, void *buffer)
{
    int         ret_code = SUCCESS;
    int         rc;             /*routine return code                   */
    int         mrc;            /*MPI return code                       */
    MPI_Offset	mpi_offset;
    MPI_Status	mpi_status;
    long        ndset;
    off_t       nelmts_xfer;
    size_t      nelmts_toxfer;
    char        dname[64];
    off_t       dset_offset;    /*dataset offset in a file              */
    off_t       file_offset;    /*file offset of the next transfer      */
    off_t       dset_size;      /*one dataset size in bytes             */
    size_t      nelmts_in_buf;  /*how many element the buffer holds     */
    size_t      nelmts_in_blk;  /*how many element a block holds        */
    off_t       elmts_begin;    /*first elmt this process transfer      */
    off_t       elmts_count;    /*number of elmts this process transfer */
    hid_t       dcpl = -1;      /* Dataset creation property list       */

    /* HDF5 variables */
    herr_t      hrc;                    /*HDF5 return code              */
    hsize_t     h5dims[1];              /*dataset dim sizes             */
    hid_t       h5dset_space_id = -1;   /*dataset space ID              */
    hid_t       h5mem_space_id = -1;    /*memory dataspace ID           */
    hid_t       h5ds_id = -1;           /*dataset handle                */
    hsize_t	h5mem_block[1];		/*memory space selection        */
    hsize_t	h5mem_stride[1];
    hsize_t	h5mem_count[1];
    hssize_t	h5mem_start[1];
#if 0
    /* for future implementation */
    hsize_t	h5dset_block[1];	/*dset space selection          */
    hsize_t	h5dset_stride[1];
    hsize_t	h5dset_count[1];
    hssize_t	h5dset_start[1];
#endif

    /* calculate dataset parameters. data type is always native C int */
    dset_size = nelmts * ELMT_SIZE;
    nelmts_in_buf = buf_size/ELMT_SIZE;

    /* hdf5 data space setup */
    if (parms->io_type == PHDF5){
        if(nelmts>0) {
            /* define a contiquous dataset of nelmts native ints */
            h5dims[0] = nelmts;
            h5dset_space_id = H5Screate_simple(1, h5dims, NULL);
            VRFY((h5dset_space_id >= 0), "H5Screate_simple");
        } /* end if */
        else {
            h5dset_space_id = H5Screate(H5S_SCALAR);
            VRFY((h5dset_space_id >= 0), "H5Screate");
        } /* end else */

        /* create the memory dataspace that corresponds to the xfer buffer */
        if(nelmts_in_buf>0) {
            h5dims[0] = nelmts_in_buf;
            h5mem_space_id = H5Screate_simple(1, h5dims, NULL);
            VRFY((h5mem_space_id >= 0), "H5Screate_simple");
        } /* end if */
        else {
            h5mem_space_id = H5Screate(H5S_SCALAR);
            VRFY((h5mem_space_id >= 0), "H5Screate");
        } /* end else */
    }

    for (ndset = 1; ndset <= ndsets; ++ndset) {

        /* Calculate dataset offset within a file */

        /* create dataset */
        switch (parms->io_type) {
        case POSIXIO:
        case MPIO:
            /* both posix and mpi io just need dataset offset in file*/
            dset_offset = (ndset - 1) * dset_size;
            break;

        case PHDF5:
            dcpl = H5Pcreate(H5P_DATASET_CREATE);
            if (dcpl < 0) {
                fprintf(stderr, "HDF5 Property List Create failed\n");
                GOTOERROR(FAIL);
            }

            /* Make the dataset chunked if asked */
            if(parms->h5_use_chunks) {
                /* Set the chunk size to be the same as the buffer size */
                h5dims[0] = nelmts_in_buf;
                hrc = H5Pset_chunk(dcpl, 1, h5dims);
                if (hrc < 0) {
                    fprintf(stderr, "HDF5 Property List Set failed\n");
                    GOTOERROR(FAIL);
                } /* end if */
            } /* end if */

#ifdef H5_HAVE_NOFILL
            /* Disable writing fill values if asked */
            if(parms->h5_no_fill) {
                hrc = H5Pset_fill_time(dcpl, H5D_FILL_TIME_NEVER);
                if (hrc < 0) {
                    fprintf(stderr, "HDF5 Property List Set failed\n");
                    GOTOERROR(FAIL);
                } /* end if */
            } /* end if */
#endif

            sprintf(dname, "Dataset_%ld", ndset);
            h5ds_id = H5Dcreate(fd->h5fd, dname, ELMT_H5_TYPE,
                                h5dset_space_id, dcpl);

            if (h5ds_id < 0) {
                fprintf(stderr, "HDF5 Dataset Create failed\n");
                GOTOERROR(FAIL);
            }

            hrc = H5Pclose(dcpl);
            /* verifying the close of the dcpl */
            if (hrc < 0) {
                fprintf(stderr, "HDF5 Property List Close failed\n");
                GOTOERROR(FAIL);
            }

            break;
        }

	/* There are two kinds of transfer patterns, contiguous and interleaved.
	 * Let 0,1,2,...,n be data accessed by process 0,1,2,...,n
	 *     where n is rank of the last process.
	 * In contiguous pattern, data are accessed as
	 *    000...111...222...nnn...
	 * In interleaved pattern, data are accessed as
	 *    012...n012...n...
	 * These are all in the scope of one dataset.
	 */
	/* Calculate the total number of elements (elmts_count) to be
	 * transferred by this process. It may be different for different
	 * transfer pattern due to rounding to integral values.
	 */
	if (blk_size==0){
	    /* Contiguous Pattern:
	     * Calculate the beginning element of this process and the next.
	     * elmts_count is the difference between these two beginnings.
	     * This way, it eliminates any rounding errors.
	     */
	    elmts_begin = (off_t)(((double)nelmts)/pio_mpi_nprocs_g*pio_mpi_rank_g);

	    /* Do not cast elmt_begin to other types, especially non-integral
	     * types, else it may introduce rounding discrepency. */
	    if (pio_mpi_rank_g < (pio_mpi_nprocs_g - 1))
		elmts_count = (off_t)(((double)nelmts) / pio_mpi_nprocs_g * (pio_mpi_rank_g + 1))
				- elmts_begin;
	    else
		/* last process.  Take whatever are left */
		elmts_count = nelmts - elmts_begin;
	}else{
	    /* Interleaved Pattern:
	     * Each process takes blk_size of elements, starting with the first
	     * process.  So, the last process may have fewer or even none.
	     * Calculate the beginning element of this process and the next.
	     * The elmnts_begin here marks only the beginning of the first
	     * block accessed by this process.
	     */
	    /* Algorithm:
	     * First allocate equal blocks per process, i.e. one block each
	     * process for every block_size*nprocs.
	     * If there is remaining unallocated, give a block each to process
	     * starting at proc 0.  The last process may get a partial block.
	     */ 
	    off_t remain_nelmts, remain_begin;	/* unallocated remaining*/

	    nelmts_in_blk = blk_size/ELMT_SIZE;
	    elmts_begin = (off_t)(nelmts_in_blk*pio_mpi_rank_g);

	    /* must use integer calculation next */
	    /* allocate equal blocks per process */
	    elmts_count = (nelmts / (off_t)(nelmts_in_blk*pio_mpi_nprocs_g)) *
			    (off_t)nelmts_in_blk;
	    remain_nelmts = nelmts % ((off_t)(nelmts_in_blk*pio_mpi_nprocs_g));

	    /* allocate any remaining */
	    remain_begin = (off_t)(nelmts_in_blk*pio_mpi_rank_g);
	    if (remain_nelmts > remain_begin){
		/* it gets something */
		if (remain_nelmts > (remain_begin+(off_t)nelmts_in_blk)){
		    /* one full block */
		    elmts_count += nelmts_in_blk;
		}else{
		    /* only a partial block */
		    elmts_count += remain_nelmts - remain_begin;
		}
	    }
	}
	/* debug */
	if (pio_debug_level >= 4) {
	    HDprint_rank(output);
	    HDfprintf(output, "Debug(do_write): "
		"nelmts_in_blk=%Hd, elmts_begin=%Hd, elmts_count=%Hd\n",
		(long_long)nelmts_in_blk, (long_long)elmts_begin,
		(long_long)elmts_count);
	}


	/* The task is to transfer elmts_count elements, starting at
	 * elmts_begin position, using transfer buffer of buf_size bytes.
	 * If blk_size > 0, select blk_size at a time, in round robin
	 * fashion, according to number of process. Otherwise, select
	 * all elmt_count in contiguous.
	 */
        nelmts_xfer = 0 ;

        /* Start "raw data" write timer */
        set_time(res->timers, HDF5_RAW_WRITE_FIXED_DIMS, START);

        while (nelmts_xfer < elmts_count){
	    /* transfer one buffer of data each round */
	    /* Note: because size_t is unsigned, avoid expressions that */
	    /* can be negative. */
            if ((nelmts_xfer + (off_t)nelmts_in_buf) <= elmts_count) {
                nelmts_toxfer = nelmts_in_buf;
            } else {
                /* last transfer of a partial buffer */
                nelmts_toxfer = elmts_count - nelmts_xfer;
            }

	    if (parms->verify) {
		/*Prepare write data for verify later*/
                int *intptr = (int *)buffer;
                register int i;

                for (i = 0; i < nelmts_toxfer; ++i)
                    *intptr++ = pio_mpi_rank_g;
            }

            /* Write */
            /* Calculate offset of write within a dataset/file */
            switch (parms->io_type) {
            case POSIXIO:
		if (blk_size==0){
		    /* Contiguous pattern */
		    /* need to (off_t) the elmnts_begin expression because they */
		    /* may be of smaller sized integer types */
		    file_offset = dset_offset + (off_t)(elmts_begin + nelmts_xfer)*ELMT_SIZE;

		    /* only care if seek returns error */
		    rc = POSIXSEEK(fd->posixfd, file_offset) < 0 ? -1 : 0;
		    VRFY((rc==0), "POSIXSEEK");
		    /* check if all bytes are transferred */
		    rc = ((ssize_t)(nelmts_toxfer*ELMT_SIZE) ==
			POSIXWRITE(fd->posixfd, buffer, nelmts_toxfer*ELMT_SIZE));
		    VRFY((rc != 0), "POSIXWRITE");
		}else{
		    /* interleaved access pattern */
		    char 	*buf_p=buffer;
		    size_t	xferred=0;
		    size_t	toxfer=0;

		    file_offset = dset_offset + (off_t)(elmts_begin + nelmts_xfer)*ELMT_SIZE;
	if (pio_debug_level >= 4) {
HDprint_rank(output);
HDfprintf(output,
"Debug(do_write): "
"nelmts_toxfer=%Hd, nelmts_xfer=%Hd\n"
,
(long_long)nelmts_toxfer, (long_long)nelmts_xfer);
}
		    while (xferred < nelmts_toxfer){
			if ((nelmts_toxfer - xferred) >= nelmts_in_blk)
			    toxfer = nelmts_in_blk;
			else
			    toxfer = nelmts_toxfer - xferred;
			/* Skip offset over blocks of other processes */
			file_offset = dset_offset +
			    (off_t)(elmts_begin + (nelmts_xfer+xferred)*pio_mpi_nprocs_g)*ELMT_SIZE;
	if (pio_debug_level >= 4) {
HDprint_rank(output);
HDfprintf(output,
"Debug(do_write): "
"nelmts_toxfer=%Hd, nelmts_xfer=%Hd"
", toxfer=%Hd, xferred=%Hd"
", file_offset=%Hd"
"\n",
(long_long)nelmts_toxfer, (long_long)nelmts_xfer, 
(long_long)toxfer, (long_long)xferred,
(long_long)file_offset);
}
			/* only care if seek returns error */
			rc = POSIXSEEK(fd->posixfd, file_offset) < 0 ? -1 : 0;
			VRFY((rc==0), "POSIXSEEK");
			/* check if all bytes are written */
			rc = ((ssize_t)(toxfer*ELMT_SIZE) ==
			    POSIXWRITE(fd->posixfd, buf_p, toxfer*ELMT_SIZE));
			VRFY((rc != 0), "POSIXWRITE");
			xferred += toxfer;
		    }
		}
                break;

            case MPIO:
                mpi_offset = dset_offset + (elmts_begin + nelmts_xfer)*ELMT_SIZE;
                mrc = MPI_File_write_at(fd->mpifd, mpi_offset, buffer,
                                        (int)(nelmts_toxfer), ELMT_MPI_TYPE,
                                        &mpi_status);
                VRFY((mrc==MPI_SUCCESS), "MPIO_WRITE");
                break;
            case PHDF5:
                /*set up the dset space id to select the segment to process */
                {
                    h5mem_start[0] = elmts_begin + nelmts_xfer;
                    h5mem_stride[0] = h5mem_block[0] = nelmts_toxfer;
                    h5mem_count[0] = 1;
                    hrc = H5Sselect_hyperslab(h5dset_space_id, H5S_SELECT_SET,
			      h5mem_start, h5mem_stride, h5mem_count, h5mem_block); 
                    VRFY((hrc >= 0), "H5Sset_hyperslab");

                    /*setup the memory space id too.  Only start is different */
                    h5mem_start[0] = 0;
                    hrc = H5Sselect_hyperslab(h5mem_space_id, H5S_SELECT_SET,
			      h5mem_start, h5mem_stride, h5mem_count, h5mem_block); 
                    VRFY((hrc >= 0), "H5Sset_hyperslab");
                }

                /* set write time here */
                hrc = H5Dwrite(h5ds_id, ELMT_H5_TYPE, h5mem_space_id,
                               h5dset_space_id, H5P_DEFAULT, buffer);
                VRFY((hrc >= 0), "H5Dwrite");
                break;
            }

            nelmts_xfer += nelmts_toxfer;
        }

        /* Stop "raw data" write timer */
        set_time(res->timers, HDF5_RAW_WRITE_FIXED_DIMS, STOP);

        /* Calculate write time */

        /* Close dataset. Only HDF5 needs to do an explicit close. */
        if (parms->io_type == PHDF5){
            hrc = H5Dclose(h5ds_id);

            if (hrc < 0) {
                fprintf(stderr, "HDF5 Dataset Close failed\n");
                GOTOERROR(FAIL);
            }

            h5ds_id = -1;
        }
    }

done:
    /* release HDF5 objects */
    if (h5dset_space_id != -1) {
        hrc = H5Sclose(h5dset_space_id);
        if (hrc < 0){
            fprintf(stderr, "HDF5 Dataset Space Close failed\n");
            ret_code = FAIL;
        } else {
            h5dset_space_id = -1;
        }
    }

    if (h5mem_space_id != -1) {
        hrc = H5Sclose(h5mem_space_id);
        if (hrc < 0) {
            fprintf(stderr, "HDF5 Memory Space Close failed\n");
            ret_code = FAIL;
        } else {
            h5mem_space_id = -1;
        }
    }

    return ret_code;
}

/*
 * Function:    do_read
 * Purpose:     read the required amount of data from the file.
 * Return:      SUCCESS or FAIL
 * Programmer:  Albert Cheng 2001/12/13
 * Modifications:
 */
static herr_t
do_read(results *res, file_descr *fd, parameters *parms, long ndsets,
        off_t nelmts, size_t blk_size, size_t buf_size, void *buffer /*out*/)
{
    int         ret_code = SUCCESS;
    int         rc;             /*routine return code                   */
    int         mrc;            /*MPI return code                       */
    MPI_Offset  mpi_offset;
    MPI_Status  mpi_status;
    long        ndset;
    off_t       nelmts_xfer;
    size_t      nelmts_toxfer;
    char        dname[64];
    off_t       dset_offset;    /*dataset offset in a file              */
    off_t       file_offset;	/*file offset of the next transfer      */
    off_t       dset_size;      /*one dataset size in bytes             */
    size_t      nelmts_in_buf;  /*how many element the buffer holds     */
    size_t      nelmts_in_blk;  /*how many element a block holds        */
    off_t       elmts_begin;    /*first elmt this process transfer      */
    off_t       elmts_count;    /*number of elmts this process transfer */

    /* HDF5 variables */
    herr_t      hrc;            	/*HDF5 return code              */
    hsize_t     h5dims[1];      	/*dataset dim sizes             */
    hid_t       h5dset_space_id = -1;   /*dataset space ID              */
    hid_t       h5mem_space_id = -1;    /*memory dataspace ID           */
    hid_t       h5ds_id = -1;   	/*dataset handle                */
    hsize_t	h5mem_block[1];		/*memory space selection        */
    hsize_t	h5mem_stride[1];
    hsize_t	h5mem_count[1];
    hssize_t	h5mem_start[1];
#if 0
    /* for future implementation */
    hsize_t	h5dset_block[1];	/*dset space selection          */
    hsize_t	h5dset_stride[1];
    hsize_t	h5dset_count[1];
    hssize_t	h5dset_start[1];
#endif

    /* calculate dataset parameters. data type is always native C int */
    dset_size = nelmts * ELMT_SIZE;
    nelmts_in_buf = buf_size/ELMT_SIZE;

    /* hdf5 data space setup */
    if (parms->io_type == PHDF5){
        if(nelmts>0) {
            /* define a contiquous dataset of nelmts native ints */
            h5dims[0] = nelmts;
            h5dset_space_id = H5Screate_simple(1, h5dims, NULL);
            VRFY((h5dset_space_id >= 0), "H5Screate_simple");
        } /* end if */
        else {
            h5dset_space_id = H5Screate(H5S_SCALAR);
            VRFY((h5dset_space_id >= 0), "H5Screate");
        } /* end else */

        /* create the memory dataspace that corresponds to the xfer buffer */
        if(nelmts_in_buf>0) {
            h5dims[0] = nelmts_in_buf;
            h5mem_space_id = H5Screate_simple(1, h5dims, NULL);
            VRFY((h5mem_space_id >= 0), "H5Screate_simple");
        } /* end if */
        else {
            h5mem_space_id = H5Screate(H5S_SCALAR);
            VRFY((h5mem_space_id >= 0), "H5Screate");
        } /* end else */
    }

    for (ndset = 1; ndset <= ndsets; ++ndset) {
        /* Calculate dataset offset within a file */

        /* create dataset */
        switch (parms->io_type) {
        case POSIXIO:
        case MPIO:
            /* both posix and mpi io just need dataset offset in file*/
            dset_offset = (ndset - 1) * dset_size;
            break;

        case PHDF5:
            sprintf(dname, "Dataset_%ld", ndset);
            h5ds_id = H5Dopen(fd->h5fd, dname);
            if (h5ds_id < 0) {
                fprintf(stderr, "HDF5 Dataset open failed\n");
                GOTOERROR(FAIL);
            }

            break;
        }

	/* There are two kinds of transfer patterns, contiguous and interleaved.
	 * Let 0,1,2,...,n be data accessed by process 0,1,2,...,n
	 *     where n is rank of the last process.
	 * In contiguous pattern, data are accessed as
	 *    000...111...222...nnn...
	 * In interleaved pattern, data are accessed as
	 *    012...n012...n...
	 * These are all in the scope of one dataset.
	 */
	/* Calculate the total number of elements (elmts_count) to be
	 * transferred by this process. It may be different for different
	 * transfer pattern due to rounding to integral values.
	 */
	if (blk_size==0){
	    /* Contiguous Pattern:
	     * Calculate the beginning element of this process and the next.
	     * elmts_count is the difference between these two beginnings.
	     * This way, it eliminates any rounding errors.
	     */
	    elmts_begin = (off_t)(((double)nelmts)/pio_mpi_nprocs_g*pio_mpi_rank_g);

	    /* Do not cast elmt_begin to other types, especially non-integral
	     * types, else it may introduce rounding discrepency. */
	    if (pio_mpi_rank_g < (pio_mpi_nprocs_g - 1))
		elmts_count = (off_t)(((double)nelmts) / pio_mpi_nprocs_g * (pio_mpi_rank_g + 1))
				- elmts_begin;
	    else
		/* last process.  Take whatever are left */
		elmts_count = nelmts - elmts_begin;
	}else{
	    /* Interleaved Pattern:
	     * Each process takes blk_size of elements, starting with the first
	     * process.  So, the last process may have fewer or even none.
	     * Calculate the beginning element of this process and the next.
	     * The elmnts_begin here marks only the beginning of the first
	     * block accessed by this process.
	     */
	    /* Algorithm:
	     * First allocate equal blocks per process, i.e. one block each
	     * process for every block_size*nprocs.
	     * If there is remaining unallocated, give a block each to process
	     * starting at proc 0.  The last process may get a partial block.
	     */ 
	    off_t remain_nelmts, remain_begin;	/* unallocated remaining*/

	    nelmts_in_blk = blk_size/ELMT_SIZE;
	    elmts_begin = (off_t)(nelmts_in_blk*pio_mpi_rank_g);

	    /* must use integer calculation next */
	    /* allocate equal blocks per process */
	    elmts_count = (nelmts / (off_t)(nelmts_in_blk*pio_mpi_nprocs_g)) *
			    (off_t)nelmts_in_blk;
	    remain_nelmts = nelmts % ((off_t)(nelmts_in_blk*pio_mpi_nprocs_g));

	    /* allocate any remaining */
	    remain_begin = (off_t)(nelmts_in_blk*pio_mpi_rank_g);
	    if (remain_nelmts > remain_begin){
		/* it gets something */
		if (remain_nelmts > (remain_begin+(off_t)nelmts_in_blk)){
		    /* one full block */
		    elmts_count += nelmts_in_blk;
		}else{
		    /* only a partial block */
		    elmts_count += remain_nelmts - remain_begin;
		}
	    }
	}
	/* debug */
	if (pio_debug_level >= 4) {
	    HDprint_rank(output);
	    HDfprintf(output, "Debug(do_read): "
		"nelmts_in_blk=%Hd, elmts_begin=%Hd, elmts_count=%Hd\n",
		(long_long)nelmts_in_blk, (long_long)elmts_begin,
		(long_long)elmts_count);
	}


	/* The task is to transfer elmts_count elements, starting at
	 * elmts_begin position, using transfer buffer of buf_size bytes.
	 * If blk_size > 0, select blk_size at a time, in round robin
	 * fashion, according to number of process. Otherwise, select
	 * all elmt_count in contiguous.
	 */
        nelmts_xfer = 0 ;

        /* Start "raw data" read timer */
        set_time(res->timers, HDF5_RAW_READ_FIXED_DIMS, START);

        while (nelmts_xfer < elmts_count){
	    /* transfer one buffer of data each round */
	    /* Note: because size_t is unsigned, avoid expressions that */
	    /* can be negative. */
            if ((nelmts_xfer + (off_t)nelmts_in_buf) <= elmts_count) {
                nelmts_toxfer = nelmts_in_buf;
            } else {
                /* last transfer of a partial buffer */
                nelmts_toxfer = elmts_count - nelmts_xfer;
            }

            /* read */
            /* Calculate offset of read within a dataset/file */
            switch (parms->io_type){
            case POSIXIO:
		if (blk_size==0){
		    /* Contiguous pattern */
		    /* need to (off_t) the elmnts_begin expression because they */
		    /* may be of smaller sized integer types */
		    file_offset = dset_offset + (off_t)(elmts_begin + nelmts_xfer)*ELMT_SIZE;

		    /* only care if seek returns error */
		    rc = POSIXSEEK(fd->posixfd, file_offset) < 0 ? -1 : 0;
		    VRFY((rc==0), "POSIXSEEK");
		    /* check if all bytes are transferred */
		    rc = ((ssize_t)(nelmts_toxfer*ELMT_SIZE) ==
			POSIXREAD(fd->posixfd, buffer, nelmts_toxfer*ELMT_SIZE));
		    VRFY((rc != 0), "POSIXREAD");
		}else{
		    /* interleaved access pattern */
		    char 	*buf_p=buffer;
		    size_t	xferred=0;
		    size_t	toxfer=0;

		    file_offset = dset_offset + (off_t)(elmts_begin + nelmts_xfer)*ELMT_SIZE;
	if (pio_debug_level >= 4) {
HDprint_rank(output);
HDfprintf(output,
"Debug(do_read): "
"nelmts_toxfer=%Hd, nelmts_xfer=%Hd\n"
,
(long_long)nelmts_toxfer, (long_long)nelmts_xfer);
}
		    while (xferred < nelmts_toxfer){
			if ((nelmts_toxfer - xferred) >= nelmts_in_blk)
			    toxfer = nelmts_in_blk;
			else
			    toxfer = nelmts_toxfer - xferred;
			/* Skip offset over blocks of other processes */
			file_offset = dset_offset +
			    (off_t)(elmts_begin + (nelmts_xfer+xferred)*pio_mpi_nprocs_g)*ELMT_SIZE;
	if (pio_debug_level >= 4) {
HDprint_rank(output);
HDfprintf(output,
"Debug(do_read):"
"nelmts_toxfer=%Hd, nelmts_xfer=%Hd"
", toxfer=%Hd, xferred=%Hd"
", file_offset=%Hd"
"\n",
(long_long)nelmts_toxfer, (long_long)nelmts_xfer, 
(long_long)toxfer, (long_long)xferred,
(long_long)file_offset);
}
			/* only care if seek returns error */
			rc = POSIXSEEK(fd->posixfd, file_offset) < 0 ? -1 : 0;
			VRFY((rc==0), "POSIXSEEK");
			/* check if all bytes are transferred */
			rc = ((ssize_t)(toxfer*ELMT_SIZE) ==
			    POSIXREAD(fd->posixfd, buf_p, toxfer*ELMT_SIZE));
			VRFY((rc != 0), "POSIXREAD");
			xferred += toxfer;
		    }
		}
                break;

            case MPIO:
                mpi_offset = dset_offset + (elmts_begin + nelmts_xfer)*ELMT_SIZE;

                mrc = MPI_File_read_at(fd->mpifd, mpi_offset, buffer,
                                       (int)(nelmts_toxfer), ELMT_MPI_TYPE,
                                       &mpi_status);
                VRFY((mrc==MPI_SUCCESS), "MPIO_read");
                break;

            case PHDF5:
                /*set up the dset space id to select the segment to process */
                {
                    h5mem_start[0] = elmts_begin + nelmts_xfer;
                    h5mem_stride[0] = h5mem_block[0] = nelmts_toxfer;
                    h5mem_count[0] = 1;
                    hrc = H5Sselect_hyperslab(h5dset_space_id, H5S_SELECT_SET,
			      h5mem_start, h5mem_stride, h5mem_count, h5mem_block); 
                    VRFY((hrc >= 0), "H5Sset_hyperslab");

                    /*setup the memory space id too.  Only start is different */
                    h5mem_start[0] = 0;
                    hrc = H5Sselect_hyperslab(h5mem_space_id, H5S_SELECT_SET,
			      h5mem_start, h5mem_stride, h5mem_count, h5mem_block); 
                    VRFY((hrc >= 0), "H5Sset_hyperslab");
                }

                /* set read time here */
                hrc = H5Dread(h5ds_id, ELMT_H5_TYPE, h5mem_space_id,
                              h5dset_space_id, H5P_DEFAULT, buffer);
                VRFY((hrc >= 0), "H5Dread");
                break;
            } /* switch (parms->io_type) */

	    if (parms->verify) {
		/*verify read data*/
                int *intptr = (int *)buffer;
                register int i;
		register int nerror=0;

                for (i = 0; i < nelmts_toxfer; ++i){
                    if (*intptr++ != pio_mpi_rank_g){
			if (++nerror < 20){
			    /* report at most 20 errors */
			    HDprint_rank(output);
			    HDfprintf(output, "read data error, expected (%Hd), "
				     "got (%Hd)\n",
				     (long_long)pio_mpi_rank_g,
				     (long_long)*(intptr-1));
			}
		    }
		}
		if (nerror >= 20) {
		    HDprint_rank(output);
		    HDfprintf(output, "...");
		    HDfprintf(output, "total read data errors=%Hd\n",
			    nerror);
		}
            }	/* if (parms->verify) */

            nelmts_xfer += nelmts_toxfer;
        }

        /* Stop "raw data" read timer */
        set_time(res->timers, HDF5_RAW_READ_FIXED_DIMS, STOP);

        /* Calculate read time */

        /* Close dataset. Only HDF5 needs to do an explicit close. */
        if (parms->io_type == PHDF5){
            hrc = H5Dclose(h5ds_id);

            if (hrc < 0) {
                fprintf(stderr, "HDF5 Dataset Close failed\n");
                GOTOERROR(FAIL);
            }

            h5ds_id = -1;
        }
    }

done:
    /* release HDF5 objects */
    if (h5dset_space_id != -1) {
        hrc = H5Sclose(h5dset_space_id);
        if (hrc < 0){
            fprintf(stderr, "HDF5 Dataset Space Close failed\n");
            ret_code = FAIL;
        } else {
            h5dset_space_id = -1;
        }
    }

    if (h5mem_space_id != -1) {
        hrc = H5Sclose(h5mem_space_id);
        if (hrc < 0) {
            fprintf(stderr, "HDF5 Memory Space Close failed\n");
            ret_code = FAIL;
        } else {
            h5mem_space_id = -1;
        }
    }

    return ret_code;
}

/*
 * Function:    do_fopen
 * Purpose:     Open the specified file.
 * Return:      SUCCESS or FAIL
 * Programmer:  Albert Cheng, Bill Wendling, 2001/12/13
 * Modifications:
 */
static herr_t
do_fopen(parameters *param, char *fname, file_descr *fd /*out*/, int flags)
{
    int ret_code = SUCCESS, mrc;
    herr_t hrc;
    hid_t acc_tpl = -1;     /* file access templates */

    switch (param->io_type) {
    case POSIXIO:
        if (flags & (PIO_CREATE | PIO_WRITE))
            fd->posixfd = POSIXCREATE(fname);
        else
            fd->posixfd = POSIXOPEN(fname, O_RDONLY);

        if (fd->posixfd < 0 ) {
            fprintf(stderr, "POSIX File Open failed(%s)\n", fname);
            GOTOERROR(FAIL);
        }

         
        /* The perils of POSIX I/O in a parallel environment. The problem is:
         *
         *      - Process n opens a file with truncation and then starts
         *        writing to the file.
         *      - Process m also opens the file with truncation, but after
         *        process n has already started to write to the file. Thus,
         *        all of the stuff process n wrote is now lost.
         */
        MPI_Barrier(pio_comm_g);

        break;

    case MPIO:
        if (flags & (PIO_CREATE | PIO_WRITE)) {
            MPI_File_delete(fname, h5_io_info_g);
            mrc = MPI_File_open(pio_comm_g, fname, MPI_MODE_CREATE | MPI_MODE_RDWR,
                                h5_io_info_g, &fd->mpifd);

            if (mrc != MPI_SUCCESS) {
                fprintf(stderr, "MPI File Open failed(%s)\n", fname);
                GOTOERROR(FAIL);
            }

            /*since MPI_File_open with MPI_MODE_CREATE does not truncate  */
            /*filesize , set size to 0 explicitedly.	*/
            mrc = MPI_File_set_size(fd->mpifd, (MPI_Offset)0);

            if (mrc != MPI_SUCCESS) {
                fprintf(stderr, "MPI_File_set_size failed\n");
                GOTOERROR(FAIL);
            }
        } else {
            mrc = MPI_File_open(pio_comm_g, fname, MPI_MODE_RDONLY,
                                h5_io_info_g, &fd->mpifd);

            if (mrc != MPI_SUCCESS) {
                fprintf(stderr, "MPI File Open failed(%s)\n", fname);
                GOTOERROR(FAIL);
            }
        }

        break;

    case PHDF5:
        acc_tpl = H5Pcreate(H5P_FILE_ACCESS);
        if (acc_tpl < 0) {
            fprintf(stderr, "HDF5 Property List Create failed\n");
            GOTOERROR(FAIL);
        }

        /* Set the file driver to the MPI-I/O driver */
        hrc = H5Pset_fapl_mpio(acc_tpl, pio_comm_g, h5_io_info_g);     
        if (hrc < 0) {
            fprintf(stderr, "HDF5 Property List Set failed\n");
            GOTOERROR(FAIL);
        }

        /* Set the alignment of objects in HDF5 file */
        hrc = H5Pset_alignment(acc_tpl, param->h5_thresh, param->h5_align);
        if (hrc < 0) {
            fprintf(stderr, "HDF5 Property List Set failed\n");
            GOTOERROR(FAIL);
        }

        /* create the parallel file */
        if (flags & (PIO_CREATE | PIO_WRITE)) {
            fd->h5fd = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, acc_tpl);
        } else {
            fd->h5fd = H5Fopen(fname, H5F_ACC_RDONLY, acc_tpl);
        }

        hrc = H5Pclose(acc_tpl);

        if (fd->h5fd < 0) {
            fprintf(stderr, "HDF5 File Create failed(%s)\n", fname);
            GOTOERROR(FAIL);
        }

        /* verifying the close of the acc_tpl */
        if (hrc < 0) {
            fprintf(stderr, "HDF5 Property List Close failed\n");
            GOTOERROR(FAIL);
        }

        break;
    } 

done:
    return ret_code;
}

/*
 * Function:    do_fclose
 * Purpose:     Close the specified file descriptor.
 * Return:      SUCCESS or FAIL
 * Programmer:  Albert Cheng, Bill Wendling, 2001/12/13
 * Modifications:
 */
static herr_t
do_fclose(iotype iot, file_descr *fd /*out*/)
{
    herr_t ret_code = SUCCESS, hrc;
    int mrc = 0, rc = 0;

    switch (iot) {
    case POSIXIO:
        rc = POSIXCLOSE(fd->posixfd);

        if (rc != 0){
            fprintf(stderr, "POSIX File Close failed\n");
            GOTOERROR(FAIL);
        }

        fd->posixfd = -1;
        break;

    case MPIO:
        mrc = MPI_File_close(&fd->mpifd);

        if (mrc != MPI_SUCCESS){
            fprintf(stderr, "MPI File close failed\n");
            GOTOERROR(FAIL);
        }

        fd->mpifd = MPI_FILE_NULL;
        break;

    case PHDF5:
        hrc = H5Fclose(fd->h5fd);

        if (hrc < 0) {
            fprintf(stderr, "HDF5 File Close failed\n");
            GOTOERROR(FAIL);
        }

        fd->h5fd = -1;
        break;
    }

done:
    return ret_code;
}


/*
 * Function:    do_fclose
 * Purpose:     Cleanup temporary file unless HDF5_NOCLEANUP is set.
 * 		Only Proc 0 of the PIO communicator will do the cleanup.
 *		Other processes just return.
 * Return:      void
 * Programmer:  Albert Cheng 2001/12/12
 * Modifications:
 */
static void
do_cleanupfile(iotype iot, char *fname)
{
    if (pio_mpi_rank_g != 0)
        return;
    
    if (clean_file_g == -1)
        clean_file_g = (getenv("HDF5_NOCLEANUP")==NULL) ? 1 : 0;
    
    if (clean_file_g){
        switch (iot){
        case POSIXIO:
            remove(fname);
            break;
        case MPIO:
        case PHDF5:
            MPI_File_delete(fname, h5_io_info_g);
            break;
        }
    }
}

#ifdef H5_HAVE_GPFS

    /* Descriptions here come from the IBM GPFS Manual */

/*
 * Function:    access_range
 * Purpose:     Declares an access range within a file for an
 *              application.
 *
 *              The application will access file offsets within the given
 *              range, and will not access offsets outside the range.
 *              Violating this hint may produce worse performance than if
 *              no hint was specified.
 *
 *              This hint is useful in situations where a file is
 *              partitioned coarsely among several nodes. If the ranges
 *              do not overlap, each node can specify which range of the
 *              file it will access, with a performance improvement in
 *              some cases, such as for sequential writing within a
 *              range.
 *
 *              Subsequent GPFS_ACCESS_RANGE hints will replace a hint
 *              passed earlier.
 *
 *                  START  - The start of the access range offset, in
 *                           bytes, from the beginning of the file.
 *                  LENGTH - Length of the access range. 0 indicates to
 *                           the end of the file.
 * Return:      Nothing
 * Programmer:  Bill Wendling, 03. June 2002
 * Modifications:
 */
static void
access_range(int handle, off_t start, off_t length, int is_write)
{
    struct {
        gpfsFcntlHeader_t hdr;
        gpfsAccessRange_t access;
    } access_range;

    access_range.hdr.totalLength = sizeof(access_range);
    access_range.hdr.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
    access_range.hdr.fcntlReserved = 0;
    access_range.start.structLen = sizeof(gpfsAccessRange_t);
    access_range.start.structType = GPFS_ACCESS_RANGE;
    access_range.start.start = start;
    access_range.start.length = length;
    access_range.start.isWrite = is_write;

    if (gpfs_fcntl(handle, &access_range) != 0) {
        fprintf(stderr,
                "gpfs_fcntl DS start directive failed. errno=%d errorOffset=%d\n",
                errno, ds_start.hdr.errorOffset);
        exit(EXIT_FAILURE);
    }
}

/*
 * Function:    free_range
 * Purpose:     Undeclares an access range within a file for an
 *              application.
 *
 *              The application will no longer access file offsets within
 *              the given range. GPFS flushes the data at the file
 *              offsets and removes it from the cache.
 *
 *              Multi-node applications that have finished one phase of
 *              their computation may wish to use this hint before the
 *              file is accessed in a conflicting mode from another node
 *              in a later phase. The potential performance benefit is
 *              that GPFS can avoid later synchronous cache consistency
 *              operations.
 *
 *                  START  - The start of the access range offset, in
 *                           bytes from the beginning of the file.
 *                  LENGTH - Length of the access range. 0 indicates to
 *                           the end of the file.
 * Return:      Nothing
 * Programmer:  Bill Wendling, 03. June 2002
 * Modifications:
 */
static void
free_range(int handle, off_t start, off_t length)
{
    struct {
        gpfsFcntlHeader_t hdr;
        gpfsFreeRange_t range;
    } free_range;

    /* Issue the invalidate hint */
    free_range.hdr.totalLength = sizeof(free_range);
    free_range.hdr.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
    free_range.hdr.fcntlReserved = 0;
    free_range.range.structLen = sizeof(gpfsFreeRange_t);
    free_range.range.structType = GPFS_FREE_RANGE;
    free_range.range.start = start;
    free_range.range.length = length;

    if (gpfs_fcntl(handle, &free_range) != 0) {
        fprintf(stderr,
                "gpfs_fcntl free range failed for range %d:%d. errno=%d errorOffset=%d\n",
                start, length, errno, free_range.hdr.errorOffset);
        exit(EXIT_FAILURE);
    }
}

/*
 * Function:    clear_file_cache
 * Purpose:     Indicates file access in the near future is not expected.
 *
 *              The application does not expect to make any further
 *              accesses to the file in the near future, so GPFS removes
 *              any data or metadata pertaining to the file from its
 *              cache.
 *
 *              Multi-node applications that have finished one phase of
 *              their computation may wish to use this hint before the
 *              file is accessed in a conflicting mode from another node
 *              in a later phase. The potential performance benefit is
 *              that GPFS can avoid later synchronous cache consistency
 *              operations.
 * Return:      Nothing
 * Programmer:  Bill Wendling, 03. June 2002
 * Modifications:
 */
static void
clear_file_cache(int handle)
{
    struct {
        gpfsFcntlHeader_t hdr;
        gpfsClearFileCache_t clear;
    } clear_cache;

    clear_cache.hdr.totalLength = sizeof(clear_cache);
    clear_cache.hdr.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
    clear_cache.hdr.fcntlReserved = 0;
    clear_cache.start.structLen = sizeof(gpfsClearFileCache_t);
    clear_cache.start.structType = GPFS_CLEAR_FILE_CACHE;

    if (gpfs_fcntl(handle, &clear_cache) != 0) {
        fprintf(stderr,
                "gpfs_fcntl clear file cache directive failed. errno=%d errorOffset=%d\n",
                errno, clear_cache.hdr.errorOffset);
        exit(EXIT_FAILURE);
    }
}

/*
 * Function:    cancel_hints
 * Purpose:     Indicates to remove any hints against the open file
 *              handle.
 *
 *              GPFS removes any hints that may have been issued against
 *              this open file handle:
 *
 *                  - The hint status of the file is restored ot what it
 *                    would have been immediately after being opened, but
 *                    does not affect the contents of the GPFS file
 *                    cache. Cancelling an earlier hint that resulted in
 *                    data being removed from the GPFS file cache does
 *                    not bring that data back int othe cache; data
 *                    re-enters the cache only pon access by the
 *                    application or by user-driven or automatic
 *                    prefetching.
 *                  - Only the GPFS_MULTIPLE_ACCESS_RANGE hint has a
 *                    state that might be removed by the
 *                    GPFS_CANCEL_HINTS directive.
 * Return:      Nothing
 * Programmer:  Bill Wendling, 03. June 2002
 * Modifications:
 */
static void
cancel_hints(int handle)
{
    struct {
        gpfsFcntlHeader_t hdr;
        gpfsCancelHints_t cancel;
    } cancel_hints;

    cancel_hints.hdr.totalLength = sizeof(cancel_hints);
    cancel_hints.hdr.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
    cancel_hints.hdr.fcntlReserved = 0;
    cancel_hints.start.structLen = sizeof(gpfsCancelHints_t);
    cancel_hints.start.structType = GPFS_CANCEL_HINTS;

    if (gpfs_fcntl(handle, &cancel_hints) != 0) {
        fprintf(stderr,
                "gpfs_fcntl cancel hints directive failed. errno=%d errorOffset=%d\n",
                errno, ds_start.hdr.errorOffset);
        exit(EXIT_FAILURE);
    }
}

/*
 * Function:    start_data_shipping
 * Purpose:     Start up data shipping. The second parameter is the total
 *              number of open instances on all nodes that will be
 *              operating on the file. Must be called for every such
 *              instance with the same value of NUM_INSTS.
 * Return:      Nothing
 * Programmer:  Bill Wendling, 28. May 2002
 * Modifications:
 */
static void
start_data_shipping(int handle, int num_insts)
{
    struct {
        gpfsFcntlHeader_t hdr;
        gpfsDataShipStart_t start;
    } ds_start;

    ds_start.hdr.totalLength = sizeof(ds_start);
    ds_start.hdr.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
    ds_start.hdr.fcntlReserved = 0;
    ds_start.start.structLen = sizeof(gpfsDataShipStart_t);
    ds_start.start.structType = GPFS_DATA_SHIP_START;
    ds_start.start.numInstances = num_insts;
    ds_start.start.reserved = 0;

    if (gpfs_fcntl(handle, &ds_start) != 0) {
        fprintf(stderr,
                "gpfs_fcntl DS start directive failed. errno=%d errorOffset=%d\n",
                errno, ds_start.hdr.errorOffset);
        exit(EXIT_FAILURE);
    }
}

/*
 * Function:    stop_data_shipping
 * Purpose:     Shut down data shipping. Must be called for every handle
 *              for which start_data_shipping was called.
 * Return:      Nothing
 * Programmer:  Bill Wendling, 28. May 2002
 * Modifications:
 */
static void
stop_data_shipping(int handle)
{
    struct {
        gpfsFcntlHeader_t hdr;
        gpfsDataShipStop_t stop;
    } ds_stop;

    ds_stop.hdr.totalLength = sizeof(ds_stop);
    ds_stop.hdr.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
    ds_stop.hdr.fcntlReserved = 0;
    ds_stop.stop.structLen = sizeof(ds_stop.stop);
    ds_stop.stop.structType = GPFS_DATA_SHIP_STOP;

    if (gpfs_fcntl(handle, &ds_stop) != 0)
        fprintf(stderr,
                "gpfs_fcntl DS stop directive failed. errno=%d errorOffset=%d\n",
                errno, ds_stop.hdr.errorOffset);
}

/*
 * Function:    invalidate_file_cache
 * Purpose:     Invalidate all cached data held on behalf of a file on
 *              this node.
 * Return:      Nothing
 * Programmer:  Bill Wendling, 03. June 2002
 * Modifications:
 */
static void
invalidate_file_cache(const char *filename)
{
    int handle;
    struct {
        gpfsFcntlHeader_t hdr;
        gpfsClearFileCache_t inv;
    } inv_cache_hint;

    /* Open the file.  If the open fails, the file cannot be cached. */
    handle = open(filename, O_RDONLY, 0);

    if (handle == -1)
        return;

    /* Issue the invalidate hint */
    inv_cache_hint.hdr.totalLength = sizeof(inv_cache_hint);
    inv_cache_hint.hdr.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
    inv_cache_hint.hdr.fcntlReserved = 0;
    inv_cache_hint.inv.structLen = sizeof(gpfsClearFileCache_t);
    inv_cache_hint.inv.structType = GPFS_CLEAR_FILE_CACHE;

    if (gpfs_fcntl(handle, &inv_cache_hint) != 0) {
        fprintf(stderr,
                "gpfs_fcntl clear cache hint failed for file '%s'.",
                filename);
        fprintf(stderr, " errno=%d errorOffset=%d\n",
                errno, inv_cache_hint.hdr.errorOffset);
        exit(1);
    }

    /* Close the file */
    if (close(handle) == -1) {
        fprintf(stderr,
                "could not close file '%s' after flushing file cache,",
                filename);
        fprintf(stderr, "errno=%d\n", errno);
        exit(1);
    }
}

#else

/* H5_HAVE_GPFS isn't defined...stub functions */

static void
access_range(int UNUSED handle, off_t UNUSED start, off_t UNUSED length, int UNUSED is_write)
{
    return;
}

static void
free_range(int UNUSED handle, off_t UNUSED start, off_t UNUSED length)
{
    return;
}

static void
clear_file_cache(int UNUSED handle)
{
    return;
}

static void
cancel_hints(int UNUSED handle)
{
    return;
}

static void
start_data_shipping(int UNUSED handle, int UNUSED num_insts)
{
    return;
}

static void
stop_data_shipping(int UNUSED handle)
{
    return;
}

static void
invalidate_file_cache(const char UNUSED *filename)
{
    return;
}

#endif  /* H5_HAVE_GPFS */

#ifdef TIME_MPI
/* instrument the MPI_File_wrirte_xxx and read_xxx calls to measure
 * pure time spent in MPI_File code.
 */
int MPI_File_read_at(MPI_File fh, MPI_Offset offset, void *buf,
	  int count, MPI_Datatype datatype, MPI_Status *status)
{
    int err;
    set_time(timer_g, HDF5_MPI_READ, START);
    err=PMPI_File_read_at(fh, offset, buf, count, datatype, status);
    set_time(timer_g, HDF5_MPI_READ, STOP);
    return err;
}


int MPI_File_read_at_all(MPI_File fh, MPI_Offset offset, void *buf,
	int count, MPI_Datatype datatype, MPI_Status *status)
{
    int err;
    set_time(timer_g, HDF5_MPI_READ, START);
    err=PMPI_File_read_at_all(fh, offset, buf, count, datatype, status);
    set_time(timer_g, HDF5_MPI_READ, STOP);
    return err;
}

int MPI_File_write_at(MPI_File fh, MPI_Offset offset, void *buf,
      int count, MPI_Datatype datatype, MPI_Status *status)
{
    int err;
    set_time(timer_g, HDF5_MPI_WRITE, START);
    err=PMPI_File_write_at(fh, offset, buf, count, datatype, status);
    set_time(timer_g, HDF5_MPI_WRITE, STOP);
    return err;
}

int MPI_File_write_at_all(MPI_File fh, MPI_Offset offset, void *buf,
    int count, MPI_Datatype datatype, MPI_Status *status)
{
    int err;
    set_time(timer_g, HDF5_MPI_WRITE, START);
    err=PMPI_File_write_at_all(fh, offset, buf, count, datatype, status);
    set_time(timer_g, HDF5_MPI_WRITE, STOP);
    return err;
}

#endif	/* TIME_MPI */
#endif /* H5_HAVE_PARALLEL */
