/*
 * Copyright (C) 2001
 *     National Center for Supercomputing Applications
 *     All rights reserved.
 * 
 * Author: Albert Cheng of NCSA, Oct 24, 2001.
 */

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>

#include "hdf5.h"

#ifdef H5_HAVE_PARALLEL

#include <mpi.h>

#ifndef MPI_FILE_NULL           /*MPIO may be defined in mpi.h already       */
#   include <mpio.h>
#endif  /* !MPI_FILE_NULL */

#include "pio_perf.h"
#include "pio_timer.h"

/* Macro definitions */

/* sizes of various items. these sizes won't change during program execution */
#define BUFFER_SIZE         (1024 * 1024)               /* 1M buffer size   */
#define ELMT_SIZE           sizeof(int)                 /* we're doing ints */
#define NELMTS_IN_BUFFER    (BUFFER_SIZE / ELMT_SIZE)

#define GOTOERROR(errcode)	{ ret_code = errcode; goto done; }
#define GOTODONE		{ goto done; }
#define ERRMSG(mesg) {                                                  \
    fprintf(stderr, "Proc %d: ", myrank);                               \
    fprintf(stderr, "*** Assertion failed (%s) at line %4d in %s\n",    \
            mesg, (int)__LINE__, __FILE__);                             \
}

#define MSG(mesg) {                                     \
    fprintf(stderr, "Proc %d: ", myrank);               \
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

#ifndef HDmalloc
#define HDmalloc(x)             malloc(x)
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
#define RAWCREATE(fn)           HDopen(fn, O_CREAT|O_TRUNC|O_RDWR, 0600)
#define RAWOPEN(fn, F)          HDopen(fn, F, 0600)
#define RAWCLOSE(F)             HDclose(F)
#define RAWSEEK(F,L)            HDseek(F,L,SEEK_SET)
#define RAWWRITE(F,B,S)         HDwrite(F,B,S)
#define RAWREAD(F,B,S)          HDread(F,B,S)

enum {
    PIO_CREATE = 1,
    PIO_WRITE = 2,
    PIO_READ = 4
};

/* the different types of file descriptors we can expect */
typedef union _file_descr {
    int         rawfd;      /* raw/Unix file    */
    MPI_File    mpifd;      /* MPI file         */
    hid_t       h5fd;       /* HDF5 file        */
} file_descr;

/* local functions */
static herr_t do_write(file_descr fd, iotype iot, unsigned long ndsets,
                       unsigned long nelmts, hid_t h5dset_space_id, char * buffer);
static herr_t do_open(iotype iot, char *fname, file_descr fd /*out*/, int flags);
static herr_t do_close(iotype iot, file_descr fd);

herr_t
do_pio(parameters param)
{
    /* return codes */
    herr_t          rc;             /*routine return code                   */
    int             mrc;            /*MPI return code                       */
    herr_t          ret_code = 0;   /*return code                           */

    file_descr      fd;
    iotype          iot;

    char            fname[256];
    unsigned int    maxprocs, nfiles, nf;
    unsigned long   ndsets;
    unsigned long   nelmts;
    unsigned int    niters;
    unsigned long   nelmts_toread, nelmts_read;
    off_t           next_offset;            /*offset of next I/O            */
    int             color;                  /*for communicator creation     */
    char           *buffer = NULL;          /*data buffer pointer           */

    /* HDF5 variables */
    herr_t          hrc;                    /*HDF5 return code              */
    hsize_t         h5dims[1];              /*dataset dim sizes             */
    hsize_t         h5block[1], h5stride[1], h5count[1];
    hssize_t        h5start[1];
    hid_t           h5dset_space_id = -1;   /*dataset space ID              */
    hid_t           h5mem_space_id = -1;    /*memory dataspace ID           */

    /* MPI variables */
    MPI_Comm	    comm = MPI_COMM_NULL;
    int		    myrank, nprocs = 1;

    /* Sanity check parameters */

    /* IO type */
    iot = param.io_type;

    switch (iot) {
    case RAW:
    case MPIO:
    case PHDF5:
	/* nothing */
	break;
    default:
	/* unknown request */
	fprintf(stderr, "Unknown IO type request (%d)\n", iot);
	GOTOERROR(FAIL);
    }

    nfiles = param.num_files;       /* number of files                      */
    ndsets = param.num_dsets;       /* number of datasets per file          */
    nelmts = param.num_elmts;       /* number of elements per dataset       */
    niters = param.num_iters;       /* number of iterations of reads/writes */
    maxprocs = param.max_num_procs; /* max number of mpi-processes to use   */

    if (nelmts == 0 ){
        fprintf(stderr,
                "number of elements per dataset must be > 0 (%lu)\n",
                nelmts);
        GOTOERROR(FAIL);
    }

    if (maxprocs == 0 ){
        fprintf(stderr,
                "maximun number of process to use must be > 0 (%u)\n",
                maxprocs);
        GOTOERROR(FAIL);
    }

    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);

    if (maxprocs > nprocs){
        fprintf(stderr,
                "maximun number of process(%d) must be <= process in MPI_COMM_WORLD(%d)\n",
                maxprocs, nprocs);
        GOTOERROR(FAIL);
    }

/* DEBUG*/
fprintf(stderr, "nfiles=%u\n", nfiles);
fprintf(stderr, "ndsets=%lu\n", ndsets);
fprintf(stderr, "nelmts=%lu\n", nelmts);
fprintf(stderr, "niters=%u\n", niters);
fprintf(stderr, "maxprocs=%u\n", maxprocs);
nfiles=3;
/*ndsets=5; */

    /* Create a sub communicator for this run. Easier to use the first N
     * processes. */
    MPI_Comm_rank(comm, &myrank);
    color = (myrank < maxprocs);
    mrc = MPI_Comm_split (MPI_COMM_WORLD, color, myrank, &comm);

    if (mrc != MPI_SUCCESS) {
        fprintf(stderr, "MPI_Comm_split failed\n");
        GOTOERROR(FAIL);
    }

    if (!color){
        /* not involved in this run */
        mrc = MPI_Comm_free(&comm);
        GOTODONE;
    }

    /* determine the mpi rank of in the new comm */
    MPI_Comm_size(comm, &nprocs);
    MPI_Comm_rank(comm, &myrank);

    /* allocate data buffer */
    buffer = HDmalloc(BUFFER_SIZE);

    if (buffer == NULL){
        fprintf(stderr, "malloc for data buffer failed\n");
        GOTOERROR(FAIL);
    }

    /* hdf5 dataset setup */
    if (iot == PHDF5){
        /* define a contiquous dataset of nelmts native ints */
        h5dims[0] = nelmts;
        h5dset_space_id = H5Screate_simple(1, h5dims, NULL);
        VRFY((h5dset_space_id >= 0), "H5Screate_simple");

        /* create the memory dataspace */
        h5dims[0] = NELMTS_IN_BUFFER;
        h5mem_space_id = H5Screate_simple(1, h5dims, NULL);
        VRFY((h5mem_space_id >= 0), "H5Screate_simple");
    }

    for (nf = 1; nf <= nfiles; nf++) {
        /* Open file for write */
MSG("creating file");
        sprintf(fname, "#pio_tmp_%u", nf);

        switch (iot) {
        case RAW:
            strcat(fname, ".raw");
            break;
        case MPIO:
            strcat(fname, ".mpio");
            break;
        case PHDF5:
            strcat(fname, ".h5");
            break;
        }

        rc = do_open(iot, fname, fd, PIO_CREATE | PIO_WRITE);
        VRFY((rc == SUCCESS), "do_open failed\n");
        rc = do_write(fd, iot, ndsets, nelmts, h5dset_space_id, buffer);
        VRFY((rc == SUCCESS), "do_write failed\n");

	/* Close file for write */
MSG("closing write file");
        rc = do_close(iot, fd);
        VRFY((rc == SUCCESS), "do_close failed\n");

        /* Open file for read */
MSG("opening file to read");
        hrc = do_open(iot, fname, fd, PIO_READ);
        VRFY((rc == SUCCESS), "do_open failed\n");

        /* Calculate dataset offset within a file */

        /* Open dataset for read */

        /* Prepare read */

        /* Calculate offset of read within a dataset/file */

        /* Read */

        /* Calculate read time */

        /* Close dataset for read */

        /* Close file for read */
MSG("closing read file");
        rc = do_close(iot, fd);
        VRFY((rc == SUCCESS), "do_close failed\n");
    }

done:
    /* clean up */
    /* release HDF5 objects */
    if (h5dset_space_id != -1){
        rc = H5Sclose(h5dset_space_id);

        if (rc < 0){
            fprintf(stderr, "HDF5 Dataset Space Close failed\n");
            ret_code = FAIL;
        } else {
            h5dset_space_id = -1;
        }
    }

    if (h5mem_space_id != -1){
        rc = H5Sclose(h5mem_space_id);

        if (rc < 0){
            fprintf(stderr, "HDF5 Memory Space Close failed\n");
            ret_code = FAIL;
        } else {
            h5mem_space_id = -1;
        }
    }

    /* close any opened files */
    rc = do_close(iot, fd);

    /* release generic resources */
    HDfree(buffer);

fprintf(stderr, "returning with ret_code=%d\n", ret_code);
    return ret_code;
}

/*
 * Function:    do_write
 * Purpose:     Write 
 * Return:      SUCCESS or FAIL
 * Programmer:  Bill Wendling, 14. November 2001
 * Modifications:
 */
static herr_t
do_write(file_descr fd, iotype iot, unsigned long ndsets,
         unsigned long nelmts, hid_t h5dset_space_id, char *buffer)
{
    int ret_code = SUCCESS;
    register unsigned long ndset;
    unsigned long nelmts_towrite, nelmts_written;
    char dname[64];
    off_t dset_offset;          /*dataset offset in a file  */
    unsigned long dset_size;    /*one dataset size in bytes */

    /* calculate dataset parameters. data type is always native C int */
    dset_size = nelmts * ELMT_SIZE;

    for (ndset = 1; ndset <= ndsets; ++ndset) {
        hid_t h5ds_id = -1;     /* dataset handle   */

        /* Calculate dataset offset within a file */

        /* create dataset */
        switch (iot) {
        case RAW:
        case MPIO:
            /* both raw and mpi io just need dataset offset in file*/
            dset_offset = (ndset - 1) * dset_size;
            break;

        case PHDF5:
            sprintf(dname, "Dataset_%lu", ndset);
            h5ds_id = H5Dcreate(fd.h5fd, dname, H5T_NATIVE_INT,
                                h5dset_space_id, H5P_DEFAULT);
            VRFY((h5ds_id >= 0), "H5Dcreate");
            break;
        }

        nelmts_written = 0 ;

        while (nelmts_written < nelmts){
            nelmts_towrite = nelmts - nelmts_written;

            if (nelmts - nelmts_written >= NELMTS_IN_BUFFER) {
                nelmts_towrite = NELMTS_IN_BUFFER;
            } else {
                /* last write of a partial buffer */
                nelmts_towrite = nelmts - nelmts_written;
            }

            /*Prepare write data*/
            {
                int *intptr = (int*)buffer;
                register int i;

                for (i = 0; i < nelmts_towrite; ++i)
                    *intptr++ = nelmts_towrite + i;
            }

            /* Write */

            /* Calculate offset of write within a dataset/file */
            switch (iot){
            case RAW:
                RAWSEEK(fd.rawfd, dset_offset + nelmts_written * ELMT_SIZE);
                RAWWRITE(fd.rawfd, buffer, nelmts_towrite * ELMT_SIZE);
                break;

            case MPIO:
                break;

            case PHDF5:
                break;
            }


            nelmts_written += nelmts_towrite;
fprintf(stderr, "wrote %lu elmts, %lu written\n", nelmts_towrite, nelmts_written);
        }

        /* Calculate write time */

        /* Close dataset. Only HDF5 needs to do an explicit close. */
        if (iot == PHDF5){
            herr_t hrc = H5Dclose(h5ds_id);

            VRFY((hrc >= 0), "HDF5 Dataset Close failed\n");
            h5ds_id = -1;
        }
    }

done:
    return ret_code;
}

static herr_t
do_open(iotype iot, char *fname, file_descr fd /*out*/, int flags)
{
    int ret_code = SUCCESS, mrc;
    herr_t hrc;
    hid_t acc_tpl = -1;     /* file access templates */

    switch (iot) {
    case RAW:
        if ((flags | PIO_CREATE) || (flags | PIO_WRITE)) {
            fd.rawfd = RAWCREATE(fname);
        } else {
	    fd.rawfd = RAWOPEN(fname, O_RDONLY);
        }

        if (fd.rawfd < 0 ) {
            fprintf(stderr, "Raw File Open failed(%s)\n", fname);
            GOTOERROR(FAIL);
        }

        break;

    case MPIO:
        if ((flags | PIO_CREATE) || (flags | PIO_WRITE)) {
            mrc = MPI_File_open(comm, fname, MPI_MODE_CREATE | MPI_MODE_RDWR,
                                MPI_INFO_NULL, &fd.mpifd);
        } else {
	    mrc = MPI_File_open(comm, fname, MPI_MODE_RDONLY,
                                MPI_INFO_NULL, &fd.mpifd);
        }

        if (mrc != MPI_SUCCESS) {
            fprintf(stderr, "MPI File Open failed(%s)\n", fname);
            GOTOERROR(FAIL);
        }

        break;

    case PHDF5:
        acc_tpl = H5Pcreate(H5P_FILE_ACCESS);
        VRFY((acc_tpl >= 0), "");
        hrc = H5Pset_fapl_mpio(acc_tpl, comm, MPI_INFO_NULL);     
        VRFY((hrc >= 0), "");

        /* create the parallel file */
        if ((flags | PIO_CREATE) || (flags | PIO_WRITE)) {
            fd.h5fd = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, acc_tpl);
        } else {
	    fd.h5fd = H5Fopen(fname, H5P_DEFAULT, acc_tpl);
        }

        hrc = H5Pclose(acc_tpl);

        if (fd.h5fd < 0) {
            fprintf(stderr, "HDF5 file Create failed(%s)\n", fname);
            GOTOERROR(FAIL);
        }

        /* verifying the close of the acc_tpl */
        VRFY((hrc >= 0), "H5Pclose");
        break;
    } 

done:
    return ret_code;
}

/*
 * Function:    do_close
 * Purpose:     Close the specified file descriptor.
 * Return:      SUCCESS or FAIL
 * Programmer:  Bill Wendling, 14. November 2001
 * Modifications:
 */
static herr_t
do_close(iotype iot, file_descr fd)
{
    herr_t ret_code = SUCCESS, hrc;
    int mrc = 0, rc = 0;

    switch (iot) {
    case RAW:
        rc = RAWCLOSE(fd.rawfd);

        if (rc != 0){
            fprintf(stderr, "Raw File Close failed\n");
            GOTOERROR(FAIL);
        }

        fd.rawfd = -1;
        break;

    case MPIO:
        mrc = MPI_File_close(&fd.mpifd);

        if (mrc != MPI_SUCCESS){
            fprintf(stderr, "MPI File close failed\n");
            GOTOERROR(FAIL);
        }

        fd.mpifd = MPI_FILE_NULL;
        break;

    case PHDF5:
        hrc = H5Fclose(fd.h5fd);

        if (hrc < 0) {
            fprintf(stderr, "HDF5 File Close failed\n");
            GOTOERROR(FAIL);
        }

        fd.h5fd = -1;
        break;
    }

done:
    return ret_code;
}

#endif /* H5_HAVE_PARALLEL */
