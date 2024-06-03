/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include "hdf5.h"

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef H5_HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif

#ifdef H5_HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef H5_HAVE_PARALLEL

#include <mpi.h>

#ifndef MPI_FILE_NULL /*MPIO may be defined in mpi.h already       */
#include <mpio.h>
#endif /* !MPI_FILE_NULL */

#include "pio_perf.h"

/* Macro definitions */

#if H5_VERS_MAJOR == 1 && H5_VERS_MINOR == 6
#define H5DCREATE(fd, name, type, space, dcpl) H5Dcreate(fd, name, type, space, dcpl)
#define H5DOPEN(fd, name)                      H5Dopen(fd, name)
#else
#define H5DCREATE(fd, name, type, space, dcpl)                                                               \
    H5Dcreate2(fd, name, type, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)
#define H5DOPEN(fd, name) H5Dopen2(fd, name, H5P_DEFAULT)
#endif

/* sizes of various items. these sizes won't change during program execution */
/* The following three must have the same type */
#define ELMT_H5_TYPE H5T_NATIVE_UCHAR

#define GOTOERROR(errcode)                                                                                   \
    do {                                                                                                     \
        ret_code = errcode;                                                                                  \
        goto done;                                                                                           \
    } while (0)
#define ERRMSG(mesg)                                                                                         \
    do {                                                                                                     \
        fprintf(stderr, "Proc %d: ", pio_mpi_rank_g);                                                        \
        fprintf(stderr, "*** Assertion failed (%s) at line %4d in %s\n", mesg, (int)__LINE__, __FILE__);     \
    } while (0)

/* verify: if val is false (0), print mesg. */
#define VRFY(val, mesg)                                                                                      \
    do {                                                                                                     \
        if (!val) {                                                                                          \
            ERRMSG(mesg);                                                                                    \
            GOTOERROR(FAIL);                                                                                 \
        }                                                                                                    \
    } while (0)

/* POSIX I/O macros */
#ifdef H5_HAVE_WIN32_API
/* Can't link against the library, so this test will use the older, non-Unicode
 * _open() call on Windows.
 */
#define HDopen(S, F, ...) _open(S, F | _O_BINARY, __VA_ARGS__)
#endif /* H5_HAVE_WIN32_API */
#define POSIXCREATE(fn)     HDopen(fn, O_CREAT | O_TRUNC | O_RDWR, 0600)
#define POSIXOPEN(fn, F)    HDopen(fn, F, 0600)
#define POSIXCLOSE(F)       HDclose(F)
#define POSIXSEEK(F, L)     HDlseek(F, L, SEEK_SET)
#define POSIXWRITE(F, B, S) HDwrite(F, B, S)
#define POSIXREAD(F, B, S)  HDread(F, B, S)

enum { PIO_CREATE = 1, PIO_WRITE = 2, PIO_READ = 4 };

/* Global variables */
static int clean_file_g = -1; /*whether to cleanup temporary test     */
/*files. -1 is not defined;             */
/*0 is no cleanup; 1 is do cleanup      */

/*
 * In a parallel machine, the filesystem suitable for compiling is
 * unlikely a parallel file system that is suitable for parallel I/O.
 * There is no standard pathname for the parallel file system.  /tmp
 * is about the best guess.
 */
#ifndef HDF5_PARAPREFIX
#define HDF5_PARAPREFIX ""
#endif /* !HDF5_PARAPREFIX */

#ifndef MIN
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#endif /* !MIN */

/* the different types of file descriptors we can expect */
typedef union _file_descr {
    int      posixfd; /* POSIX file handle*/
    MPI_File mpifd;   /* MPI file         */
    hid_t    h5fd;    /* HDF5 file        */
} file_descr;

/* local functions */
static char  *pio_create_filename(iotype iot, const char *base_name, char *fullname, size_t size);
static herr_t do_write(results *res, file_descr *fd, parameters *parms, long ndsets, off_t nelmts,
                       size_t buf_size, void *buffer);
static herr_t do_read(results *res, file_descr *fd, parameters *parms, long ndsets, off_t nelmts,
                      size_t buf_size, void *buffer /*out*/);
static herr_t do_fopen(parameters *param, char *fname, file_descr *fd /*out*/, int flags);
static herr_t do_fclose(iotype iot, file_descr *fd);
static void   do_cleanupfile(iotype iot, char *fname);
static off_t  sqrto(off_t);

/*
 * Function:        do_pio
 * Purpose:         PIO Engine where Parallel IO are executed.
 * Return:          results
 */
results
do_pio(parameters param)
{
    /* return codes */
    herr_t  ret_code = 0; /*return code                           */
    results res;

    file_descr fd;
    iotype     iot;

    char *fname = NULL;
    long  nf;
    long  ndsets;
    off_t nbytes;         /*number of bytes per dataset  */
    off_t snbytes;        /*general dataset size          */
                          /*for 1D, it is the actual dataset size */
                          /*for 2D, it is the size of a side of the dataset square */
    char  *buffer = NULL; /*data buffer pointer           */
    size_t buf_size;      /*general buffer size in bytes     */
                          /*for 1D, it is the actual buffer size */
                          /*for 2D, it is the length of the buffer rectangle */
    size_t blk_size;      /*data block size in bytes      */
    size_t bsize;         /*actual buffer size            */

    /* HDF5 variables */
    herr_t hrc; /*HDF5 return code              */

    /* Sanity check parameters */

    /* IO type */
    iot = param.io_type;

    if (NULL == (fname = calloc(FILENAME_MAX, sizeof(char))))
        GOTOERROR(FAIL);

    switch (iot) {
        case MPIO:
            fd.mpifd   = MPI_FILE_NULL;
            res.timers = io_time_new(MPI_CLOCK);
            break;
        case POSIXIO:
            fd.posixfd = -1;
            res.timers = io_time_new(MPI_CLOCK);
            break;
        case PHDF5:
            fd.h5fd    = -1;
            res.timers = io_time_new(MPI_CLOCK);
            break;
        default:
            /* unknown request */
            fprintf(stderr, "Unknown IO type request (%d)\n", iot);
            GOTOERROR(FAIL);
    }

    ndsets   = param.num_dsets; /* number of datasets per file          */
    nbytes   = param.num_bytes; /* number of bytes per dataset          */
    buf_size = param.buf_size;
    blk_size = param.blk_size;

    if (!param.dim2d) {
        snbytes = nbytes;   /* General dataset size     */
        bsize   = buf_size; /* Actual buffer size       */
    }
    else {
        snbytes = sqrto(nbytes);       /* General dataset size     */
        bsize   = buf_size * blk_size; /* Actual buffer size       */
    }

    if (param.num_files < 0) {
        fprintf(stderr, "number of files must be >= 0 (%ld)\n", param.num_files);
        GOTOERROR(FAIL);
    }

    if (ndsets < 0) {
        fprintf(stderr, "number of datasets per file must be >= 0 (%ld)\n", ndsets);
        GOTOERROR(FAIL);
    }

    if (param.num_procs <= 0) {
        fprintf(stderr, "maximum number of process to use must be > 0 (%d)\n", param.num_procs);
        GOTOERROR(FAIL);
    }

    /* Validate transfer buffer size & block size*/
    if (blk_size <= 0) {
        fprintf(stderr, "Transfer block size (%zu) must be > 0\n", blk_size);
        GOTOERROR(FAIL);
    }
    if (buf_size <= 0) {
        fprintf(stderr, "Transfer buffer size (%zu) must be > 0\n", buf_size);
        GOTOERROR(FAIL);
    }
    if ((buf_size % blk_size) != 0) {
        fprintf(stderr,
                "Transfer buffer size (%zu) must be a multiple of the "
                "interleaved I/O block size (%zu)\n",
                buf_size, blk_size);
        GOTOERROR(FAIL);
    }
    if ((snbytes % pio_mpi_nprocs_g) != 0) {
        fprintf(stderr,
                "Dataset size (%lld) must be a multiple of the "
                "number of processes (%d)\n",
                (long long)snbytes, pio_mpi_nprocs_g);
        GOTOERROR(FAIL);
    }

    if (!param.dim2d) {
        if (((size_t)(snbytes / pio_mpi_nprocs_g) % buf_size) != 0) {
            fprintf(stderr,
                    "Dataset size/process (%lld) must be a multiple of the "
                    "transfer buffer size (%zu)\n",
                    (long long)(snbytes / pio_mpi_nprocs_g), buf_size);
            GOTOERROR(FAIL);
        }
    }
    else {
        if (((size_t)snbytes % buf_size) != 0) {
            fprintf(stderr,
                    "Dataset side size (%lld) must be a multiple of the "
                    "transfer buffer size (%zu)\n",
                    (long long)snbytes, buf_size);
            GOTOERROR(FAIL);
        }
    }

    /* Allocate transfer buffer */
    if ((buffer = malloc(bsize)) == NULL) {
        fprintf(stderr, "malloc for transfer buffer size (%zu) failed\n", bsize);
        GOTOERROR(FAIL);
    }

    if (pio_debug_level >= 4) {
        int myrank;

        MPI_Comm_rank(pio_comm_g, &myrank);

        /* output all of the times for all iterations */
        if (myrank == 0)
            fprintf(output, "Timer details:\n");
    }

    for (nf = 1; nf <= param.num_files; nf++) {
        /*
         * Write performance measurement
         */
        /* Open file for write */
        char base_name[256];

        snprintf(base_name, sizeof(base_name), "#pio_tmp_%lu", nf);
        pio_create_filename(iot, base_name, fname, FILENAME_MAX);
        if (pio_debug_level > 0)
            fprintf(output, "rank %d: data filename=%s\n", pio_mpi_rank_g, fname);

        /* Need barrier to make sure everyone starts at the same time */
        MPI_Barrier(pio_comm_g);

        io_time_set(res.timers, HDF5_GROSS_WRITE_FIXED_DIMS, TSTART);
        hrc = do_fopen(&param, fname, &fd, PIO_CREATE | PIO_WRITE);

        VRFY((hrc == SUCCESS), "do_fopen failed");

        io_time_set(res.timers, HDF5_FINE_WRITE_FIXED_DIMS, TSTART);
        hrc = do_write(&res, &fd, &param, ndsets, nbytes, buf_size, buffer);
        io_time_set(res.timers, HDF5_FINE_WRITE_FIXED_DIMS, TSTOP);

        VRFY((hrc == SUCCESS), "do_write failed");

        /* Close file for write */
        hrc = do_fclose(iot, &fd);

        io_time_set(res.timers, HDF5_GROSS_WRITE_FIXED_DIMS, TSTOP);
        VRFY((hrc == SUCCESS), "do_fclose failed");

        if (!param.h5_write_only) {
            /*
             * Read performance measurement
             */
            /* Need barrier to make sure everyone is done writing and has
             * closed the file.  Also to make sure everyone starts reading
             * at the same time.
             */
            MPI_Barrier(pio_comm_g);

            /* Open file for read */
            io_time_set(res.timers, HDF5_GROSS_READ_FIXED_DIMS, TSTART);
            hrc = do_fopen(&param, fname, &fd, PIO_READ);

            VRFY((hrc == SUCCESS), "do_fopen failed");

            io_time_set(res.timers, HDF5_FINE_READ_FIXED_DIMS, TSTART);
            hrc = do_read(&res, &fd, &param, ndsets, nbytes, buf_size, buffer);
            io_time_set(res.timers, HDF5_FINE_READ_FIXED_DIMS, TSTOP);
            VRFY((hrc == SUCCESS), "do_read failed");

            /* Close file for read */
            hrc = do_fclose(iot, &fd);

            io_time_set(res.timers, HDF5_GROSS_READ_FIXED_DIMS, TSTOP);
            VRFY((hrc == SUCCESS), "do_fclose failed");
        }

        /* Need barrier to make sure everyone is done with the file */
        /* before it may be removed by do_cleanupfile */
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
        default:
            break;
    }

    /* release generic resources */
    free(buffer);
    free(fname);
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
 */
static char *
pio_create_filename(iotype iot, const char *base_name, char *fullname, size_t size)
{
    const char *prefix, *suffix = "";
    char       *ptr, last       = '\0';
    size_t      i, j;

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
        default:
            break;
    }

    /* First use the environment variable and then try the constant */
    prefix = getenv("HDF5_PARAPREFIX");

#ifdef HDF5_PARAPREFIX
    if (!prefix)
        prefix = HDF5_PARAPREFIX;
#endif /* HDF5_PARAPREFIX */

    /* Prepend the prefix value to the base name */
    if (prefix && *prefix) {
        /* If the prefix specifies the HDF5_PARAPREFIX directory, then
         * default to using the "/tmp/$USER" or "/tmp/$LOGIN"
         * directory instead. */
        char *user, *login, *subdir;

        user   = getenv("USER");
        login  = getenv("LOGIN");
        subdir = (user ? user : login);

        if (subdir) {
            for (i = 0; i < size - 1 && prefix[i]; i++)
                fullname[i] = prefix[i];

            fullname[i++] = '/';

            for (j = 0; i < size && subdir[j]; i++, j++)
                fullname[i] = subdir[j];
        }
        else {
            /* We didn't append the prefix yet */
            strncpy(fullname, prefix, size);
            fullname[size - 1] = '\0';
        }

        if ((strlen(fullname) + strlen(base_name) + 1) < size) {
            /* Append the base_name with a slash first. Multiple slashes are
             * handled below. */
            h5_stat_t buf;

            memset(&buf, 0, sizeof(h5_stat_t));
            if (HDstat(fullname, &buf) < 0)
                /* The directory doesn't exist just yet */
                if (HDmkdir(fullname, (mode_t)0755) < 0 && errno != EEXIST) {
                    /* We couldn't make the "/tmp/${USER,LOGIN}" subdirectory.
                     * Default to PREFIX's original prefix value. */
                    strcpy(fullname, prefix);
                }

            strcat(fullname, "/");
            strcat(fullname, base_name);
        }
        else {
            /* Buffer is too small */
            return NULL;
        }
    }
    else if (strlen(base_name) >= size) {
        /* Buffer is too small */
        return NULL;
    }
    else {
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
 * Function:        do_write
 * Purpose:         Write the required amount of data to the file.
 * Return:          SUCCESS or FAIL
 */
static herr_t
do_write(results *res, file_descr *fd, parameters *parms, long ndsets, off_t nbytes, size_t buf_size,
         void *buffer)
{
    int            ret_code = SUCCESS;
    int            rc; /* Return code */
    long           ndset;
    size_t         blk_size;            /* The block size to subdivide the xfer buffer into */
    off_t          nbytes_xfer;         /* Total number of bytes transferred so far */
    size_t         nbytes_xfer_advance; /* Number of bytes transferred in a single I/O operation */
    size_t         nbytes_toxfer;       /* Number of bytes to transfer a particular time */
    char           dname[64];
    off_t          dset_offset    = 0;      /* Dataset offset in a file */
    off_t          bytes_begin[2] = {0, 0}; /* First elmt this process transfer */
    off_t          bytes_count;             /* Number of elmts this process transfer */
    off_t          snbytes = 0;             /* Size of a side of the dataset square */
    unsigned char *buf_p;                   /* Current buffer pointer */

    /* POSIX variables */
    off_t file_offset;         /* File offset of the next transfer     */
    off_t file_offset_advance; /* File offset advance after each I/O operation */
    off_t posix_file_offset;   /* Base file offset of the next transfer      */

    /* MPI variables */
    MPI_Offset   mpi_file_offset;          /* Base file offset of the next transfer*/
    MPI_Offset   mpi_offset;               /* Offset in MPI file                   */
    MPI_Offset   mpi_offset_advance;       /* Offset advance after each I/O operation */
    MPI_Datatype mpi_file_type;            /* MPI derived type for 1D file            */
    MPI_Datatype mpi_blk_type;             /* MPI derived type for 1D buffer          */
    MPI_Datatype mpi_cont_type;            /* MPI derived type for 2D contiguous file */
    MPI_Datatype mpi_partial_buffer_cont;  /* MPI derived type for partial 2D contiguous buffer */
    MPI_Datatype mpi_inter_type;           /* MPI derived type for 2D interleaved file  */
    MPI_Datatype mpi_partial_buffer_inter; /* MPI derived type for partial 2D interleaved buffer */
    MPI_Datatype mpi_full_buffer;          /* MPI derived type for 2D full buffer */
    MPI_Datatype mpi_full_chunk;           /* MPI derived type for 2D full chunk */
    MPI_Datatype mpi_chunk_inter_type;     /* MPI derived type for 2D chunk interleaved file */
    MPI_Datatype mpi_collective_type;      /* Generic MPI derived type for 2D collective access */
    MPI_Status   mpi_status;
    int          mrc; /* MPI return code                      */

    /* HDF5 variables */
    herr_t   hrc;                               /*HDF5 return code              */
    hsize_t  h5dims[2];                         /*dataset dim sizes             */
    hid_t    h5dset_space_id = H5I_INVALID_HID; /*dataset space ID              */
    hid_t    h5mem_space_id  = H5I_INVALID_HID; /*memory dataspace ID           */
    hid_t    h5ds_id         = H5I_INVALID_HID; /*dataset handle                */
    hsize_t  h5block[2];                        /*dataspace selection           */
    hsize_t  h5stride[2];
    hsize_t  h5count[2];
    hsize_t  h5start[2];
    hssize_t h5offset[2];              /* Selection offset within dataspace */
    hid_t    h5dcpl = H5I_INVALID_HID; /* Dataset creation property list */
    hid_t    h5dxpl = H5I_INVALID_HID; /* Dataset transfer property list */

    /* Get the parameters from the parameter block */
    blk_size = parms->blk_size;

    /* There are two kinds of transfer patterns, contiguous and interleaved.
     * Let 0,1,2,...,n be data accessed by process 0,1,2,...,n
     *     where n is rank of the last process.
     * In contiguous pattern, data are accessed as
     *    000...111...222...nnn...
     * In interleaved pattern, data are accessed as
     *    012...n012...n...
     * These are all in the scope of one dataset.
     */

    /* 1D dataspace */
    if (!parms->dim2d) {
        /* Contiguous Pattern: */
        if (!parms->interleaved) {
            bytes_begin[0] = (off_t)(((double)nbytes * pio_mpi_rank_g) / pio_mpi_nprocs_g);
        } /* end if */
        /* Interleaved Pattern: */
        else {
            bytes_begin[0] = (off_t)(blk_size * (size_t)pio_mpi_rank_g);
        } /* end else */

        /* Prepare buffer for verifying data */
        if (parms->verify)
            memset(buffer, pio_mpi_rank_g + 1, buf_size);
    } /* end if */
    /* 2D dataspace */
    else {
        /* nbytes is always the number of bytes per dataset (1D or 2D). If the
           dataspace is 2D, snbytes is the size of a side of the dataset square.
         */
        snbytes = sqrto(nbytes);

        /* Contiguous Pattern: */
        if (!parms->interleaved) {
            bytes_begin[0] = (off_t)((double)snbytes * pio_mpi_rank_g / pio_mpi_nprocs_g);
            bytes_begin[1] = 0;
        } /* end if */
        /* Interleaved Pattern: */
        else {
            bytes_begin[0] = 0;

            if (!parms->h5_use_chunks || parms->io_type == PHDF5)
                bytes_begin[1] = (off_t)(blk_size * (size_t)pio_mpi_rank_g);
            else
                bytes_begin[1] = (off_t)(blk_size * blk_size * (size_t)pio_mpi_rank_g);
        } /* end else */

        /* Prepare buffer for verifying data */
        if (parms->verify)
            memset(buffer, pio_mpi_rank_g + 1, buf_size * blk_size);
    } /* end else */

    /* Calculate the total number of bytes (bytes_count) to be
     * transferred by this process. It may be different for different
     * transfer pattern due to rounding to integral values.
     */
    /*
     * Calculate the beginning bytes of this process and the next.
     * bytes_count is the difference between these two beginnings.
     * This way, it eliminates any rounding errors.
     * (This is tricky, don't mess with the formula, rounding errors
     * can easily get introduced) */
    bytes_count = (off_t)(((double)nbytes * (pio_mpi_rank_g + 1)) / pio_mpi_nprocs_g) -
                  (off_t)(((double)nbytes * pio_mpi_rank_g) / pio_mpi_nprocs_g);

    /* debug */
    if (pio_debug_level >= 4) {
        HDprint_rank(output);
        if (!parms->dim2d) {
            fprintf(output,
                    "Debug(do_write): "
                    "buf_size=%zu, bytes_begin=%lld, bytes_count=%lld\n",
                    buf_size, (long long)bytes_begin[0], (long long)bytes_count);
        }
        else {
            fprintf(output,
                    "Debug(do_write): "
                    "linear buf_size=%zu, bytes_begin=(%lld,%lld), bytes_count=%lld\n",
                    buf_size * blk_size, (long long)bytes_begin[0], (long long)bytes_begin[1],
                    (long long)bytes_count);
        }
    }

    /* I/O Access specific setup */
    switch (parms->io_type) {
        case POSIXIO:
            /* No extra setup */
            break;

        case MPIO: /* MPI-I/O setup */
            /* 1D dataspace */
            if (!parms->dim2d) {
                /* Build block's derived type */
                mrc = MPI_Type_contiguous((int)blk_size, MPI_BYTE, &mpi_blk_type);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_CREATE");

                /* Build file's derived type */
                mrc = MPI_Type_vector((int)(buf_size / blk_size), (int)1, (int)pio_mpi_nprocs_g, mpi_blk_type,
                                      &mpi_file_type);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_CREATE");

                /* Commit file type */
                mrc = MPI_Type_commit(&mpi_file_type);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_COMMIT");

                /* Commit buffer type */
                mrc = MPI_Type_commit(&mpi_blk_type);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_COMMIT");
            } /* end if */
            /* 2D dataspace */
            else {
                /* Build partial buffer derived type for contiguous access */

                mrc = MPI_Type_contiguous((int)buf_size, MPI_BYTE, &mpi_partial_buffer_cont);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_CREATE");

                /* Commit partial buffer derived type */
                mrc = MPI_Type_commit(&mpi_partial_buffer_cont);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_COMMIT");

                /* Build contiguous file's derived type */
                mrc = MPI_Type_vector((int)blk_size, (int)1, (int)((size_t)snbytes / buf_size),
                                      mpi_partial_buffer_cont, &mpi_cont_type);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_CREATE");

                /* Commit contiguous file type */
                mrc = MPI_Type_commit(&mpi_cont_type);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_COMMIT");

                /* Build partial buffer derived type for interleaved access */
                mrc = MPI_Type_contiguous((int)blk_size, MPI_BYTE, &mpi_partial_buffer_inter);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_CREATE");

                /* Commit partial buffer derived type */
                mrc = MPI_Type_commit(&mpi_partial_buffer_inter);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_COMMIT");

                /* Build interleaved file's derived type */
                mrc = MPI_Type_vector((int)buf_size, (int)1, (int)((size_t)snbytes / blk_size),
                                      mpi_partial_buffer_inter, &mpi_inter_type);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_CREATE");

                /* Commit interleaved file type */
                mrc = MPI_Type_commit(&mpi_inter_type);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_COMMIT");

                /* Build full buffer derived type */
                mrc = MPI_Type_contiguous((int)(blk_size * buf_size), MPI_BYTE, &mpi_full_buffer);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_CREATE");

                /* Commit full buffer derived type */
                mrc = MPI_Type_commit(&mpi_full_buffer);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_COMMIT");

                /* Build full chunk derived type */
                mrc = MPI_Type_contiguous((int)(blk_size * blk_size), MPI_BYTE, &mpi_full_chunk);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_CREATE");

                /* Commit full chunk derived type */
                mrc = MPI_Type_commit(&mpi_full_chunk);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_COMMIT");

                /* Build chunk interleaved file's derived type */
                mrc = MPI_Type_vector((int)(buf_size / blk_size), (int)1, (int)((size_t)snbytes / blk_size),
                                      mpi_full_chunk, &mpi_chunk_inter_type);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_CREATE");

                /* Commit chunk interleaved file type */
                mrc = MPI_Type_commit(&mpi_chunk_inter_type);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_COMMIT");

            } /* end else */
            break;

        case PHDF5: /* HDF5 setup */
            /* 1D dataspace */
            if (!parms->dim2d) {
                if (nbytes > 0) {
                    /* define a contiguous dataset of nbytes native bytes */
                    h5dims[0]       = (hsize_t)nbytes;
                    h5dset_space_id = H5Screate_simple(1, h5dims, NULL);
                    VRFY((h5dset_space_id >= 0), "H5Screate_simple");

                    /* Set up the file dset space id to select the pattern to access */
                    if (!parms->interleaved) {
                        /* Contiguous pattern */
                        h5start[0]  = (hsize_t)bytes_begin[0];
                        h5stride[0] = h5block[0] = blk_size;
                        h5count[0]               = buf_size / blk_size;
                    } /* end if */
                    else {
                        /* Interleaved access pattern */
                        /* Skip offset over blocks of other processes */
                        h5start[0]  = (hsize_t)bytes_begin[0];
                        h5stride[0] = blk_size * (size_t)pio_mpi_nprocs_g;
                        h5block[0]  = blk_size;
                        h5count[0]  = buf_size / blk_size;
                    } /* end else */
                    hrc = H5Sselect_hyperslab(h5dset_space_id, H5S_SELECT_SET, h5start, h5stride, h5count,
                                              h5block);
                    VRFY((hrc >= 0), "H5Sselect_hyperslab");
                } /* end if */
                else {
                    h5dset_space_id = H5Screate(H5S_SCALAR);
                    VRFY((h5dset_space_id >= 0), "H5Screate");
                } /* end else */

                /* Create the memory dataspace that corresponds to the xfer buffer */
                if (buf_size > 0) {
                    h5dims[0]      = buf_size;
                    h5mem_space_id = H5Screate_simple(1, h5dims, NULL);
                    VRFY((h5mem_space_id >= 0), "H5Screate_simple");
                } /* end if */
                else {
                    h5mem_space_id = H5Screate(H5S_SCALAR);
                    VRFY((h5mem_space_id >= 0), "H5Screate");
                } /* end else */
            }     /* end if */
            /* 2D dataspace */
            else {
                if (nbytes > 0) {
                    /* define a contiguous dataset of nbytes native bytes */
                    h5dims[0]       = (hsize_t)snbytes;
                    h5dims[1]       = (hsize_t)snbytes;
                    h5dset_space_id = H5Screate_simple(2, h5dims, NULL);
                    VRFY((h5dset_space_id >= 0), "H5Screate_simple");

                    /* Set up the file dset space id to select the pattern to access */
                    if (!parms->interleaved) {
                        /* Contiguous pattern */
                        h5start[0]  = (hsize_t)bytes_begin[0];
                        h5start[1]  = (hsize_t)bytes_begin[1];
                        h5stride[0] = 1;
                        h5stride[1] = h5block[0] = h5block[1] = blk_size;
                        h5count[0]                            = 1;
                        h5count[1]                            = buf_size / blk_size;
                    } /* end if */
                    else {
                        /* Interleaved access pattern */
                        /* Skip offset over blocks of other processes */
                        h5start[0]  = (hsize_t)bytes_begin[0];
                        h5start[1]  = (hsize_t)bytes_begin[1];
                        h5stride[0] = blk_size;
                        h5stride[1] = blk_size * (size_t)pio_mpi_nprocs_g;
                        h5block[0] = h5block[1] = blk_size;
                        h5count[0]              = buf_size / blk_size;
                        h5count[1]              = 1;
                    } /* end else */
                    hrc = H5Sselect_hyperslab(h5dset_space_id, H5S_SELECT_SET, h5start, h5stride, h5count,
                                              h5block);
                    VRFY((hrc >= 0), "H5Sselect_hyperslab");
                } /* end if */
                else {
                    h5dset_space_id = H5Screate(H5S_SCALAR);
                    VRFY((h5dset_space_id >= 0), "H5Screate");
                } /* end else */

                /* Create the memory dataspace that corresponds to the xfer buffer */
                if (buf_size > 0) {
                    if (!parms->interleaved) {
                        h5dims[0] = blk_size;
                        h5dims[1] = buf_size;
                    }
                    else {
                        h5dims[0] = buf_size;
                        h5dims[1] = blk_size;
                    }
                    h5mem_space_id = H5Screate_simple(2, h5dims, NULL);
                    VRFY((h5mem_space_id >= 0), "H5Screate_simple");
                } /* end if */
                else {
                    h5mem_space_id = H5Screate(H5S_SCALAR);
                    VRFY((h5mem_space_id >= 0), "H5Screate");
                } /* end else */
            }     /* end else */

            /* Create the dataset transfer property list */
            h5dxpl = H5Pcreate(H5P_DATASET_XFER);
            if (h5dxpl < 0) {
                fprintf(stderr, "HDF5 Property List Create failed\n");
                GOTOERROR(FAIL);
            }

            /* Change to collective I/O, if asked */
            if (parms->collective) {
                hrc = H5Pset_dxpl_mpio(h5dxpl, H5FD_MPIO_COLLECTIVE);
                if (hrc < 0) {
                    fprintf(stderr, "HDF5 Property List Set failed\n");
                    GOTOERROR(FAIL);
                } /* end if */
            }     /* end if */
            break;

        default:
            break;
    } /* end switch */

    for (ndset = 1; ndset <= ndsets; ++ndset) {

        /* Calculate dataset offset within a file */

        /* create dataset */
        switch (parms->io_type) {
            case POSIXIO:
            case MPIO:
                /* both posix and mpi io just need dataset offset in file*/
                dset_offset = (ndset - 1) * nbytes;
                break;

            case PHDF5:
                h5dcpl = H5Pcreate(H5P_DATASET_CREATE);
                if (h5dcpl < 0) {
                    fprintf(stderr, "HDF5 Property List Create failed\n");
                    GOTOERROR(FAIL);
                }
                /* 1D dataspace */
                if (!parms->dim2d) {
                    /* Make the dataset chunked if asked */
                    if (parms->h5_use_chunks) {
                        /* Set the chunk size to be the same as the buffer size */
                        h5dims[0] = blk_size;
                        hrc       = H5Pset_chunk(h5dcpl, 1, h5dims);
                        if (hrc < 0) {
                            fprintf(stderr, "HDF5 Property List Set failed\n");
                            GOTOERROR(FAIL);
                        } /* end if */
                    }     /* end if */
                }         /* end if */
                else {
                    /* 2D dataspace */
                    if (parms->h5_use_chunks) {
                        /* Set the chunk size to be the same as the block size */
                        h5dims[0] = blk_size;
                        h5dims[1] = blk_size;
                        hrc       = H5Pset_chunk(h5dcpl, 2, h5dims);
                        if (hrc < 0) {
                            fprintf(stderr, "HDF5 Property List Set failed\n");
                            GOTOERROR(FAIL);
                        } /* end if */
                    }     /* end if */
                }         /* end else */

                snprintf(dname, sizeof(dname), "Dataset_%ld", ndset);
                h5ds_id = H5DCREATE(fd->h5fd, dname, ELMT_H5_TYPE, h5dset_space_id, h5dcpl);

                if (h5ds_id < 0) {
                    fprintf(stderr, "HDF5 Dataset Create failed\n");
                    GOTOERROR(FAIL);
                }

                hrc = H5Pclose(h5dcpl);
                /* verifying the close of the dcpl */
                if (hrc < 0) {
                    fprintf(stderr, "HDF5 Property List Close failed\n");
                    GOTOERROR(FAIL);
                }
                break;

            default:
                break;
        }

        /* The task is to transfer bytes_count bytes, starting at
         * bytes_begin position, using transfer buffer of buf_size bytes.
         * If interleaved, select buf_size at a time, in round robin
         * fashion, according to number of process. Otherwise, select
         * all bytes_count in contiguous.
         */
        nbytes_xfer = 0;

        /* 1D dataspace */
        if (!parms->dim2d) {
            /* Set base file offset for all I/O patterns and POSIX access */
            posix_file_offset = dset_offset + bytes_begin[0];

            /* Set base file offset for all I/O patterns and MPI access */
            mpi_file_offset = (MPI_Offset)(dset_offset + bytes_begin[0]);
        } /* end if */
        else {
            /* Set base file offset for all I/O patterns and POSIX access */
            posix_file_offset = dset_offset + bytes_begin[0] * snbytes + bytes_begin[1];

            /* Set base file offset for all I/O patterns and MPI access */
            mpi_file_offset = (MPI_Offset)(dset_offset + bytes_begin[0] * snbytes + bytes_begin[1]);
        } /* end else */

        /* Start "raw data" write timer */
        io_time_set(res->timers, HDF5_RAW_WRITE_FIXED_DIMS, TSTART);

        while (nbytes_xfer < bytes_count) {
            /* Write */
            /* Calculate offset of write within a dataset/file */
            switch (parms->io_type) {
                case POSIXIO:
                    /* 1D dataspace */
                    if (!parms->dim2d) {
                        /* Contiguous pattern */
                        if (!parms->interleaved) {
                            /* Compute file offset */
                            file_offset = posix_file_offset + (off_t)nbytes_xfer;

                            /* only care if seek returns error */
                            rc = POSIXSEEK(fd->posixfd, file_offset) < 0 ? -1 : 0;
                            VRFY((rc == 0), "POSIXSEEK");

                            /* check if all bytes are written */
                            rc = ((ssize_t)buf_size == POSIXWRITE(fd->posixfd, buffer, buf_size));
                            VRFY((rc != 0), "POSIXWRITE");

                            /* Advance global offset in dataset */
                            nbytes_xfer += (ssize_t)buf_size;
                        } /* end if */
                        /* Interleaved access pattern */
                        else {
                            /* Set the base of user's buffer */
                            buf_p = (unsigned char *)buffer;

                            /* Set the number of bytes to transfer this time */
                            nbytes_toxfer = buf_size;

                            /* Loop over the buffers to write */
                            while (nbytes_toxfer > 0) {
                                /* Skip offset over blocks of other processes */
                                file_offset = posix_file_offset + (off_t)(nbytes_xfer * pio_mpi_nprocs_g);

                                /* only care if seek returns error */
                                rc = POSIXSEEK(fd->posixfd, file_offset) < 0 ? -1 : 0;
                                VRFY((rc == 0), "POSIXSEEK");

                                /* check if all bytes are written */
                                rc = ((ssize_t)blk_size == POSIXWRITE(fd->posixfd, buf_p, blk_size));
                                VRFY((rc != 0), "POSIXWRITE");

                                /* Advance location in buffer */
                                buf_p += blk_size;

                                /* Advance global offset in dataset */
                                nbytes_xfer += (ssize_t)blk_size;

                                /* Decrement number of bytes left this time */
                                nbytes_toxfer -= blk_size;
                            } /* end while */
                        }     /* end else */
                    }         /* end if */
                    /* 2D dataspace */
                    else {
                        /* Contiguous storage */
                        if (!parms->h5_use_chunks) {
                            /* Contiguous access pattern */
                            if (!parms->interleaved) {
                                /* Compute file offset */
                                file_offset = posix_file_offset +
                                              (off_t)((((size_t)nbytes_xfer / blk_size) / (size_t)snbytes) *
                                                          (blk_size * (size_t)snbytes) +
                                                      (((size_t)nbytes_xfer / blk_size) % (size_t)snbytes));

                                /* Number of bytes to be transferred per I/O operation */
                                nbytes_xfer_advance = buf_size;

                                /* Global offset advance after each I/O operation */
                                file_offset_advance = (off_t)snbytes;
                            } /* end if */
                            /* Interleaved access pattern */
                            else {
                                /* Compute file offset */
                                file_offset =
                                    posix_file_offset +
                                    (off_t)(((((size_t)nbytes_xfer / buf_size) * (size_t)pio_mpi_nprocs_g) /
                                             (size_t)snbytes) *
                                                (buf_size * (size_t)snbytes) +
                                            (((size_t)nbytes_xfer / buf_size) * (size_t)pio_mpi_nprocs_g) %
                                                (size_t)snbytes);

                                /* Number of bytes to be transferred per I/O operation */
                                nbytes_xfer_advance = blk_size;

                                /* Global offset advance after each I/O operation */
                                file_offset_advance = (off_t)snbytes;
                            } /* end else */
                        }     /* end if */
                        /* Chunked storage */
                        else {
                            /*Contiguous access pattern */
                            if (!parms->interleaved) {
                                /* Compute file offset */
                                file_offset = posix_file_offset + (off_t)nbytes_xfer;

                                /* Number of bytes to be transferred per I/O operation */
                                nbytes_xfer_advance = blk_size * buf_size;

                                /* Global offset advance after each I/O operation */
                                file_offset_advance = 0;
                            } /* end if */
                            /*Interleaved access pattern */
                            else {
                                /* Compute file offset */
                                /* Before simplification */
                                /* file_offset=posix_file_offset+(off_t)((nbytes_xfer/(buf_size/blk_size)
                                 *pio_mpi_nprocs_g)/(snbytes/blk_size*(blk_size*blk_size))*(buf_size/blk_size
                                 *snbytes/blk_size*(blk_size*blk_size))+((nbytes_xfer/(buf_size/blk_size))
                                 *pio_mpi_nprocs_g)%(snbytes/blk_size*(blk_size*blk_size))); */

                                file_offset = posix_file_offset +
                                              (off_t)((((size_t)nbytes_xfer / (buf_size / blk_size) *
                                                        (size_t)pio_mpi_nprocs_g) /
                                                       ((size_t)snbytes * blk_size)) *
                                                          (buf_size * (size_t)snbytes) +
                                                      (((size_t)nbytes_xfer / (buf_size / blk_size)) *
                                                       (size_t)pio_mpi_nprocs_g) %
                                                          ((size_t)snbytes * blk_size));

                                /* Number of bytes to be transferred per I/O operation */
                                nbytes_xfer_advance = blk_size * blk_size;

                                /* Global offset advance after each I/O operation */
                                /* file_offset_advance = (off_t)(snbytes/blk_size*(blk_size*blk_size)); */
                                file_offset_advance = (off_t)snbytes * (off_t)blk_size;
                            } /* end else */
                        }     /* end else */

                        /* Common code for file access */

                        /* Set the base of user's buffer */
                        buf_p = (unsigned char *)buffer;

                        /* Set the number of bytes to transfer this time */
                        nbytes_toxfer = buf_size * blk_size;

                        /* Loop over portions of the buffer to write */
                        while (nbytes_toxfer > 0) {
                            /* only care if seek returns error */
                            rc = POSIXSEEK(fd->posixfd, file_offset) < 0 ? -1 : 0;
                            VRFY((rc == 0), "POSIXSEEK");

                            /* check if all bytes are written */
                            rc = ((ssize_t)nbytes_xfer_advance ==
                                  POSIXWRITE(fd->posixfd, buf_p, nbytes_xfer_advance));
                            VRFY((rc != 0), "POSIXWRITE");

                            /* Advance location in buffer */
                            buf_p += nbytes_xfer_advance;

                            /* Advance global offset in dataset */
                            nbytes_xfer += (ssize_t)nbytes_xfer_advance;

                            /* Decrement number of bytes left this time */
                            nbytes_toxfer -= nbytes_xfer_advance;

                            /* Partially advance file offset */
                            file_offset += file_offset_advance;
                        } /* end while */

                    } /* end else */

                    break;

                case MPIO:
                    /* 1D dataspace */
                    if (!parms->dim2d) {
                        /* Independent file access */
                        if (!parms->collective) {
                            /* Contiguous pattern */
                            if (!parms->interleaved) {
                                /* Compute offset in file */
                                mpi_offset = mpi_file_offset + nbytes_xfer;

                                /* Perform independent write */
                                mrc =
                                    MPI_File_write_at(fd->mpifd, mpi_offset, buffer,
                                                      (int)(buf_size / blk_size), mpi_blk_type, &mpi_status);
                                VRFY((mrc == MPI_SUCCESS), "MPIO_WRITE");

                                /* Advance global offset in dataset */
                                nbytes_xfer += (ssize_t)buf_size;
                            } /* end if */
                            /* Interleaved access pattern */
                            else {
                                /* Set the base of user's buffer */
                                buf_p = (unsigned char *)buffer;

                                /* Set the number of bytes to transfer this time */
                                nbytes_toxfer = buf_size;

                                /* Loop over the buffers to write */
                                while (nbytes_toxfer > 0) {
                                    /* Skip offset over blocks of other processes */
                                    mpi_offset = mpi_file_offset + (nbytes_xfer * pio_mpi_nprocs_g);

                                    /* Perform independent write */
                                    mrc = MPI_File_write_at(fd->mpifd, mpi_offset, buf_p, (int)1,
                                                            mpi_blk_type, &mpi_status);
                                    VRFY((mrc == MPI_SUCCESS), "MPIO_WRITE");

                                    /* Advance location in buffer */
                                    buf_p += blk_size;

                                    /* Advance global offset in dataset */
                                    nbytes_xfer += (ssize_t)blk_size;

                                    /* Decrement number of bytes left this time */
                                    nbytes_toxfer -= blk_size;
                                } /* end while */
                            }     /* end else */
                        }         /* end if */
                        /* Collective file access */
                        else {
                            /* Contiguous access pattern */
                            if (!parms->interleaved) {
                                /* Compute offset in file */
                                mpi_offset = mpi_file_offset + nbytes_xfer;

                                /* Perform independent write */
                                mrc = MPI_File_write_at_all(fd->mpifd, mpi_offset, buffer,
                                                            (int)(buf_size / blk_size), mpi_blk_type,
                                                            &mpi_status);
                                VRFY((mrc == MPI_SUCCESS), "MPIO_WRITE");

                                /* Advance global offset in dataset */
                                nbytes_xfer += (ssize_t)buf_size;
                            } /* end if */
                            /* Interleaved access pattern */
                            else {
                                /* Compute offset in file */
                                mpi_offset = mpi_file_offset + (nbytes_xfer * pio_mpi_nprocs_g);

                                /* Set the file view */
                                mrc = MPI_File_set_view(fd->mpifd, mpi_offset, mpi_blk_type, mpi_file_type,
                                                        "native", h5_io_info_g);
                                VRFY((mrc == MPI_SUCCESS), "MPIO_VIEW");

                                /* Perform write */
                                mrc = MPI_File_write_at_all(fd->mpifd, 0, buffer, (int)(buf_size / blk_size),
                                                            mpi_blk_type, &mpi_status);
                                VRFY((mrc == MPI_SUCCESS), "MPIO_WRITE");

                                /* Advance global offset in dataset */
                                nbytes_xfer += (ssize_t)buf_size;
                            } /* end else */
                        }     /* end else */
                    }         /* end if */
                    /* 2D dataspace */
                    else {
                        /* Contiguous storage */
                        if (!parms->h5_use_chunks) {
                            /* Contiguous access pattern */
                            if (!parms->interleaved) {
                                /* Compute offset in file */
                                mpi_offset =
                                    mpi_file_offset +
                                    (MPI_Offset)((((size_t)nbytes_xfer / blk_size) / (size_t)snbytes) *
                                                 (blk_size * (size_t)snbytes)) +
                                    (MPI_Offset)(((size_t)nbytes_xfer / blk_size) % (size_t)snbytes);

                                /* Number of bytes to be transferred per I/O operation */
                                nbytes_xfer_advance = buf_size;

                                /* Global offset advance after each I/O operation */
                                mpi_offset_advance = snbytes;

                                /* MPI type to be used for collective access */
                                mpi_collective_type = mpi_cont_type;
                            } /* end if */
                            /* Interleaved access pattern */
                            else {
                                /* Compute offset in file */
                                mpi_offset = mpi_file_offset +
                                             (MPI_Offset)(((((size_t)nbytes_xfer / buf_size) *
                                                            (size_t)pio_mpi_nprocs_g) /
                                                           (size_t)snbytes) *
                                                          (buf_size * (size_t)snbytes)) +
                                             (MPI_Offset)((((size_t)nbytes_xfer / buf_size) *
                                                           (size_t)pio_mpi_nprocs_g) %
                                                          (size_t)snbytes);

                                /* Number of bytes to be transferred per I/O operation */
                                nbytes_xfer_advance = blk_size;

                                /* Global offset advance after each I/O operation */
                                mpi_offset_advance = snbytes;

                                /* MPI type to be used for collective access */
                                mpi_collective_type = mpi_inter_type;
                            } /* end else */
                        }     /* end if */
                        /* Chunked storage */
                        else {
                            /*Contiguous access pattern */
                            if (!parms->interleaved) {
                                /* Compute offset in file */
                                mpi_offset = mpi_file_offset + nbytes_xfer;

                                /* Number of bytes to be transferred per I/O operation */
                                nbytes_xfer_advance = blk_size * buf_size;

                                /* Global offset advance after each I/O operation */
                                mpi_offset_advance = 0;

                                /* MPI type to be used for collective access */
                                mpi_collective_type = mpi_full_buffer;
                            } /* end if */
                            /*Interleaved access pattern */
                            else {
                                /* Compute offset in file */
                                /* Before simplification */
                                /* mpi_offset=mpi_file_offset+(nbytes_xfer/(buf_size/blk_size)
                                    *pio_mpi_nprocs_g)/(snbytes/blk_size*(blk_size*blk_size))*
                                    (buf_size/blk_size*snbytes/blk_size*(blk_size*blk_size))+
                                    ((nbytes_xfer/(buf_size/blk_size))*pio_mpi_nprocs_g)%(snbytes
                                    /blk_size*(blk_size*blk_size)); */
                                mpi_offset = mpi_file_offset +
                                             (MPI_Offset)((((size_t)nbytes_xfer / (buf_size / blk_size) *
                                                            (size_t)pio_mpi_nprocs_g) /
                                                           ((size_t)snbytes * blk_size)) *
                                                          (buf_size * (size_t)snbytes)) +
                                             (MPI_Offset)((((size_t)nbytes_xfer / (buf_size / blk_size)) *
                                                           (size_t)pio_mpi_nprocs_g) %
                                                          ((size_t)snbytes * blk_size));

                                /* Number of bytes to be transferred per I/O operation */
                                nbytes_xfer_advance = blk_size * blk_size;

                                /* Global offset advance after each I/O operation */
                                /* mpi_offset_advance = (MPI_Offset)(snbytes/blk_size*(blk_size*blk_size)); */
                                mpi_offset_advance = (MPI_Offset)((size_t)snbytes * blk_size);

                                /* MPI type to be used for collective access */
                                mpi_collective_type = mpi_chunk_inter_type;
                            } /* end else */
                        }     /* end else */

                        /* Common code for independent file access */
                        if (!parms->collective) {
                            /* Set the base of user's buffer */
                            buf_p = (unsigned char *)buffer;

                            /* Set the number of bytes to transfer this time */
                            nbytes_toxfer = buf_size * blk_size;

                            /* Loop over portions of the buffer to write */
                            while (nbytes_toxfer > 0) {
                                /* Perform independent write */
                                mrc = MPI_File_write_at(fd->mpifd, mpi_offset, buf_p,
                                                        (int)nbytes_xfer_advance, MPI_BYTE, &mpi_status);
                                VRFY((mrc == MPI_SUCCESS), "MPIO_WRITE");

                                /* Advance location in buffer */
                                buf_p += nbytes_xfer_advance;

                                /* Advance global offset in dataset */
                                nbytes_xfer += (ssize_t)nbytes_xfer_advance;

                                /* Decrement number of bytes left this time */
                                nbytes_toxfer -= nbytes_xfer_advance;

                                /* Partially advance global offset in dataset */
                                mpi_offset += mpi_offset_advance;
                            } /* end while */
                        }     /* end if */

                        /* Common code for collective file access */
                        else {
                            /* Set the file view */
                            mrc = MPI_File_set_view(fd->mpifd, mpi_offset, MPI_BYTE, mpi_collective_type,
                                                    "native", h5_io_info_g);
                            VRFY((mrc == MPI_SUCCESS), "MPIO_VIEW");

                            /* Perform write */
                            MPI_File_write_at_all(fd->mpifd, 0, buffer, (int)(buf_size * blk_size), MPI_BYTE,
                                                  &mpi_status);
                            VRFY((mrc == MPI_SUCCESS), "MPIO_WRITE");

                            /* Advance global offset in dataset */
                            nbytes_xfer += (off_t)buf_size * (off_t)blk_size;
                        } /* end else */

                    } /* end else */

                    break;

                case PHDF5:
                    /* 1D dataspace */
                    if (!parms->dim2d) {
                        /* Set up the file dset space id to move the selection to process */
                        if (!parms->interleaved) {
                            /* Contiguous pattern */
                            h5offset[0] = nbytes_xfer;
                        } /* end if */
                        else {
                            /* Interleaved access pattern */
                            /* Skip offset over blocks of other processes */
                            h5offset[0] = (nbytes_xfer * pio_mpi_nprocs_g);
                        } /* end else */
                        hrc = H5Soffset_simple(h5dset_space_id, h5offset);
                        VRFY((hrc >= 0), "H5Soffset_simple");

                        /* Write the buffer out */
                        hrc =
                            H5Dwrite(h5ds_id, ELMT_H5_TYPE, h5mem_space_id, h5dset_space_id, h5dxpl, buffer);
                        VRFY((hrc >= 0), "H5Dwrite");

                        /* Increment number of bytes transferred */
                        nbytes_xfer += (ssize_t)buf_size;
                    } /* end if */
                    /* 2D dataspace */
                    else {
                        /* Set up the file dset space id to move the selection to process */
                        if (!parms->interleaved) {
                            /* Contiguous pattern */
                            h5offset[0] =
                                (hssize_t)(((size_t)nbytes_xfer / ((size_t)snbytes * blk_size)) * blk_size);
                            h5offset[1] =
                                (hssize_t)(((size_t)nbytes_xfer % ((size_t)snbytes * blk_size)) / blk_size);

                        } /* end if */
                        else {
                            /* Interleaved access pattern */
                            /* Skip offset over blocks of other processes */
                            h5offset[0] = (hssize_t)((((size_t)nbytes_xfer * (size_t)pio_mpi_nprocs_g) /
                                                      ((size_t)snbytes * buf_size)) *
                                                     buf_size);
                            h5offset[1] = (hssize_t)((((size_t)nbytes_xfer * (size_t)pio_mpi_nprocs_g) %
                                                      ((size_t)snbytes * buf_size)) /
                                                     buf_size);

                        } /* end else */
                        hrc = H5Soffset_simple(h5dset_space_id, h5offset);
                        VRFY((hrc >= 0), "H5Soffset_simple");

                        /* Write the buffer out */
                        hrc =
                            H5Dwrite(h5ds_id, ELMT_H5_TYPE, h5mem_space_id, h5dset_space_id, h5dxpl, buffer);
                        VRFY((hrc >= 0), "H5Dwrite");

                        /* Increment number of bytes transferred */
                        nbytes_xfer += (off_t)buf_size * (off_t)blk_size;

                    } /* end else */

                    break;

                default:
                    break;
            } /* switch (parms->io_type) */
        }     /* end while */

        /* Stop "raw data" write timer */
        io_time_set(res->timers, HDF5_RAW_WRITE_FIXED_DIMS, TSTOP);

        /* Calculate write time */

        /* Close dataset. Only HDF5 needs to do an explicit close. */
        if (parms->io_type == PHDF5) {
            hrc = H5Dclose(h5ds_id);

            if (hrc < 0) {
                fprintf(stderr, "HDF5 Dataset Close failed\n");
                GOTOERROR(FAIL);
            }

            h5ds_id = H5I_INVALID_HID;
        } /* end if */
    }     /* end for */

done:
    /* release MPI-I/O objects */
    if (parms->io_type == MPIO) {
        /* 1D dataspace */
        if (!parms->dim2d) {
            /* Free file type */
            mrc = MPI_Type_free(&mpi_file_type);
            VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_FREE");

            /* Free buffer type */
            mrc = MPI_Type_free(&mpi_blk_type);
            VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_FREE");
        } /* end if */
        /* 2D dataspace */
        else {
            /* Free partial buffer type for contiguous access */
            mrc = MPI_Type_free(&mpi_partial_buffer_cont);
            VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_FREE");

            /* Free contiguous file type */
            mrc = MPI_Type_free(&mpi_cont_type);
            VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_FREE");

            /* Free partial buffer type for interleaved access */
            mrc = MPI_Type_free(&mpi_partial_buffer_inter);
            VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_FREE");

            /* Free interleaved file type */
            mrc = MPI_Type_free(&mpi_inter_type);
            VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_FREE");

            /* Free full buffer type */
            mrc = MPI_Type_free(&mpi_full_buffer);
            VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_FREE");

            /* Free full chunk type */
            mrc = MPI_Type_free(&mpi_full_chunk);
            VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_FREE");

            /* Free chunk interleaved file type */
            mrc = MPI_Type_free(&mpi_chunk_inter_type);
            VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_FREE");
        } /* end else */
    }     /* end if */

    /* release HDF5 objects */
    if (h5dset_space_id != -1) {
        hrc = H5Sclose(h5dset_space_id);
        if (hrc < 0) {
            fprintf(stderr, "HDF5 Dataset Space Close failed\n");
            ret_code = FAIL;
        }
        else {
            h5dset_space_id = H5I_INVALID_HID;
        }
    }

    if (h5mem_space_id != -1) {
        hrc = H5Sclose(h5mem_space_id);
        if (hrc < 0) {
            fprintf(stderr, "HDF5 Memory Space Close failed\n");
            ret_code = FAIL;
        }
        else {
            h5mem_space_id = H5I_INVALID_HID;
        }
    }

    if (h5dxpl != -1) {
        hrc = H5Pclose(h5dxpl);
        if (hrc < 0) {
            fprintf(stderr, "HDF5 Dataset Transfer Property List Close failed\n");
            ret_code = FAIL;
        }
        else {
            h5dxpl = H5I_INVALID_HID;
        }
    }

    return ret_code;
}

static off_t
sqrto(off_t x)
{
    double root_x = sqrt((double)x);
    return (off_t)root_x;
}

/*
 * Function:        do_read
 * Purpose:         read the required amount of data from the file.
 * Return:          SUCCESS or FAIL
 */
static herr_t
do_read(results *res, file_descr *fd, parameters *parms, long ndsets, off_t nbytes, size_t buf_size,
        void *buffer /*out*/)
{
    int            ret_code = SUCCESS;
    int            rc; /*routine return code                   */
    long           ndset;
    size_t         blk_size;            /* The block size to subdivide the xfer buffer into */
    size_t         bsize;               /* Size of the actual buffer */
    off_t          nbytes_xfer;         /* Total number of bytes transferred so far */
    size_t         nbytes_xfer_advance; /* Number of bytes transferred in a single I/O operation */
    size_t         nbytes_toxfer;       /* Number of bytes to transfer a particular time */
    char           dname[64];
    off_t          dset_offset    = 0;      /* Dataset offset in a file */
    off_t          bytes_begin[2] = {0, 0}; /* First elmt this process transfer */
    off_t          bytes_count;             /* Number of elmts this process transfer */
    off_t          snbytes = 0;             /* Size of a side of the dataset square */
    unsigned char *buf_p;                   /* Current buffer pointer */

    /* POSIX variables */
    off_t file_offset;         /* File offset of the next transfer    */
    off_t file_offset_advance; /* File offset advance after each I/O operation */
    off_t posix_file_offset;   /* Base file offset of the next transfer      */

    /* MPI variables */
    MPI_Offset   mpi_file_offset;          /* Base file offset of the next transfer*/
    MPI_Offset   mpi_offset;               /* Offset in MPI file                   */
    MPI_Offset   mpi_offset_advance;       /* Offset advance after each I/O operation */
    MPI_Datatype mpi_file_type;            /* MPI derived type for 1D file            */
    MPI_Datatype mpi_blk_type;             /* MPI derived type for 1D buffer          */
    MPI_Datatype mpi_cont_type;            /* MPI derived type for 2D contiguous file */
    MPI_Datatype mpi_partial_buffer_cont;  /* MPI derived type for partial 2D contiguous buffer */
    MPI_Datatype mpi_inter_type;           /* MPI derived type for 2D interleaved file  */
    MPI_Datatype mpi_partial_buffer_inter; /* MPI derived type for partial 2D interleaved buffer */
    MPI_Datatype mpi_full_buffer;          /* MPI derived type for 2D full buffer */
    MPI_Datatype mpi_full_chunk;           /* MPI derived type for 2D full chunk */
    MPI_Datatype mpi_chunk_inter_type;     /* MPI derived type for 2D chunk interleaved file */
    MPI_Datatype mpi_collective_type;      /* Generic MPI derived type for 2D collective access */
    MPI_Status   mpi_status;
    int          mrc; /* MPI return code                      */

    /* HDF5 variables */
    herr_t   hrc;                               /*HDF5 return code              */
    hsize_t  h5dims[2];                         /*dataset dim sizes             */
    hid_t    h5dset_space_id = H5I_INVALID_HID; /*dataset space ID              */
    hid_t    h5mem_space_id  = H5I_INVALID_HID; /*memory dataspace ID           */
    hid_t    h5ds_id         = H5I_INVALID_HID; /*dataset handle                */
    hsize_t  h5block[2];                        /*dataspace selection           */
    hsize_t  h5stride[2];
    hsize_t  h5count[2];
    hsize_t  h5start[2];
    hssize_t h5offset[2];              /* Selection offset within dataspace */
    hid_t    h5dxpl = H5I_INVALID_HID; /* Dataset transfer property list */

    /* Get the parameters from the parameter block */
    blk_size = parms->blk_size;

    /* There are two kinds of transfer patterns, contiguous and interleaved.
     * Let 0,1,2,...,n be data accessed by process 0,1,2,...,n
     *     where n is rank of the last process.
     * In contiguous pattern, data are accessed as
     *    000...111...222...nnn...
     * In interleaved pattern, data are accessed as
     *    012...n012...n...
     * These are all in the scope of one dataset.
     */

    /* 1D dataspace */
    if (!parms->dim2d) {
        bsize = buf_size;
        /* Contiguous Pattern: */
        if (!parms->interleaved) {
            bytes_begin[0] = (off_t)(((double)nbytes * pio_mpi_rank_g) / pio_mpi_nprocs_g);
        } /* end if */
        /* Interleaved Pattern: */
        else {
            bytes_begin[0] = (off_t)blk_size * (off_t)pio_mpi_rank_g;
        } /* end else */
    }     /* end if */
    /* 2D dataspace */
    else {
        /* nbytes is always the number of bytes per dataset (1D or 2D). If the
           dataspace is 2D, snbytes is the size of a side of the 'dataset square'.
         */
        snbytes = sqrto(nbytes);

        bsize = buf_size * blk_size;

        /* Contiguous Pattern: */
        if (!parms->interleaved) {
            bytes_begin[0] = (off_t)((double)snbytes * pio_mpi_rank_g / pio_mpi_nprocs_g);
            bytes_begin[1] = 0;
        } /* end if */
        /* Interleaved Pattern: */
        else {
            bytes_begin[0] = 0;

            if (!parms->h5_use_chunks || parms->io_type == PHDF5)
                bytes_begin[1] = (off_t)blk_size * (off_t)pio_mpi_rank_g;
            else
                bytes_begin[1] = (off_t)blk_size * (off_t)blk_size * (off_t)pio_mpi_rank_g;
        } /* end else */
    }     /* end else */

    /* Calculate the total number of bytes (bytes_count) to be
     * transferred by this process. It may be different for different
     * transfer pattern due to rounding to integral values.
     */
    /*
     * Calculate the beginning bytes of this process and the next.
     * bytes_count is the difference between these two beginnings.
     * This way, it eliminates any rounding errors.
     * (This is tricky, don't mess with the formula, rounding errors
     * can easily get introduced) */
    bytes_count = (off_t)(((double)nbytes * (pio_mpi_rank_g + 1)) / pio_mpi_nprocs_g) -
                  (off_t)(((double)nbytes * pio_mpi_rank_g) / pio_mpi_nprocs_g);

    /* debug */
    if (pio_debug_level >= 4) {
        HDprint_rank(output);
        if (!parms->dim2d) {
            fprintf(output,
                    "Debug(do_write): "
                    "buf_size=%zu, bytes_begin=%lld, bytes_count=%lld\n",
                    buf_size, (long long)bytes_begin[0], (long long)bytes_count);
        }
        else {
            fprintf(output,
                    "Debug(do_write): "
                    "linear buf_size=%zu, bytes_begin=(%lld,%lld), bytes_count=%lld\n",
                    buf_size * blk_size, (long long)bytes_begin[0], (long long)bytes_begin[1],
                    (long long)bytes_count);
        }
    }

    /* I/O Access specific setup */
    switch (parms->io_type) {
        case POSIXIO:
            /* No extra setup */
            break;

        case MPIO: /* MPI-I/O setup */
            /* 1D dataspace */
            if (!parms->dim2d) {
                /* Build block's derived type */
                mrc = MPI_Type_contiguous((int)blk_size, MPI_BYTE, &mpi_blk_type);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_CREATE");

                /* Build file's derived type */
                mrc = MPI_Type_vector((int)(buf_size / blk_size), (int)1, (int)pio_mpi_nprocs_g, mpi_blk_type,
                                      &mpi_file_type);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_CREATE");

                /* Commit file type */
                mrc = MPI_Type_commit(&mpi_file_type);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_COMMIT");

                /* Commit buffer type */
                mrc = MPI_Type_commit(&mpi_blk_type);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_COMMIT");
            } /* end if */
            /* 2D dataspace */
            else {
                /* Build partial buffer derived type for contiguous access */
                mrc = MPI_Type_contiguous((int)buf_size, MPI_BYTE, &mpi_partial_buffer_cont);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_CREATE");

                /* Commit partial buffer derived type */
                mrc = MPI_Type_commit(&mpi_partial_buffer_cont);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_COMMIT");

                /* Build contiguous file's derived type */
                mrc = MPI_Type_vector((int)blk_size, (int)1, (int)((size_t)snbytes / buf_size),
                                      mpi_partial_buffer_cont, &mpi_cont_type);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_CREATE");

                /* Commit contiguous file type */
                mrc = MPI_Type_commit(&mpi_cont_type);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_COMMIT");

                /* Build partial buffer derived type for interleaved access */
                mrc = MPI_Type_contiguous((int)blk_size, MPI_BYTE, &mpi_partial_buffer_inter);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_CREATE");

                /* Commit partial buffer derived type */
                mrc = MPI_Type_commit(&mpi_partial_buffer_inter);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_COMMIT");

                /* Build interleaved file's derived type */
                mrc = MPI_Type_vector((int)buf_size, (int)1, (int)((size_t)snbytes / blk_size),
                                      mpi_partial_buffer_inter, &mpi_inter_type);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_CREATE");

                /* Commit interleaved file type */
                mrc = MPI_Type_commit(&mpi_inter_type);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_COMMIT");

                /* Build full buffer derived type */
                mrc = MPI_Type_contiguous((int)(blk_size * buf_size), MPI_BYTE, &mpi_full_buffer);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_CREATE");

                /* Commit full buffer derived type */
                mrc = MPI_Type_commit(&mpi_full_buffer);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_COMMIT");

                /* Build full chunk derived type */
                mrc = MPI_Type_contiguous((int)(blk_size * blk_size), MPI_BYTE, &mpi_full_chunk);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_CREATE");

                /* Commit full chunk derived type */
                mrc = MPI_Type_commit(&mpi_full_chunk);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_COMMIT");

                /* Build chunk interleaved file's derived type */
                mrc = MPI_Type_vector((int)(buf_size / blk_size), (int)1, (int)((size_t)snbytes / blk_size),
                                      mpi_full_chunk, &mpi_chunk_inter_type);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_CREATE");

                /* Commit chunk interleaved file type */
                mrc = MPI_Type_commit(&mpi_chunk_inter_type);
                VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_COMMIT");
            } /* end else */
            break;

        case PHDF5: /* HDF5 setup */
            /* 1D dataspace */
            if (!parms->dim2d) {
                if (nbytes > 0) {
                    /* define a contiguous dataset of nbytes native bytes */
                    h5dims[0]       = (hsize_t)nbytes;
                    h5dset_space_id = H5Screate_simple(1, h5dims, NULL);
                    VRFY((h5dset_space_id >= 0), "H5Screate_simple");

                    /* Set up the file dset space id to select the pattern to access */
                    if (!parms->interleaved) {
                        /* Contiguous pattern */
                        h5start[0]  = (hsize_t)bytes_begin[0];
                        h5stride[0] = h5block[0] = blk_size;
                        h5count[0]               = buf_size / blk_size;
                    } /* end if */
                    else {
                        /* Interleaved access pattern */
                        /* Skip offset over blocks of other processes */
                        h5start[0]  = (hsize_t)bytes_begin[0];
                        h5stride[0] = blk_size * (size_t)pio_mpi_nprocs_g;
                        h5block[0]  = blk_size;
                        h5count[0]  = buf_size / blk_size;
                    } /* end else */
                    hrc = H5Sselect_hyperslab(h5dset_space_id, H5S_SELECT_SET, h5start, h5stride, h5count,
                                              h5block);
                    VRFY((hrc >= 0), "H5Sselect_hyperslab");
                } /* end if */
                else {
                    h5dset_space_id = H5Screate(H5S_SCALAR);
                    VRFY((h5dset_space_id >= 0), "H5Screate");
                } /* end else */

                /* Create the memory dataspace that corresponds to the xfer buffer */
                if (buf_size > 0) {
                    h5dims[0]      = buf_size;
                    h5mem_space_id = H5Screate_simple(1, h5dims, NULL);
                    VRFY((h5mem_space_id >= 0), "H5Screate_simple");
                } /* end if */
                else {
                    h5mem_space_id = H5Screate(H5S_SCALAR);
                    VRFY((h5mem_space_id >= 0), "H5Screate");
                } /* end else */
            }     /* end if */
            /* 2D dataspace */
            else {
                if (nbytes > 0) {
                    /* define a contiguous dataset of nbytes native bytes */
                    h5dims[0]       = (hsize_t)snbytes;
                    h5dims[1]       = (hsize_t)snbytes;
                    h5dset_space_id = H5Screate_simple(2, h5dims, NULL);
                    VRFY((h5dset_space_id >= 0), "H5Screate_simple");

                    /* Set up the file dset space id to select the pattern to access */
                    if (!parms->interleaved) {
                        /* Contiguous pattern */
                        h5start[0]  = (hsize_t)bytes_begin[0];
                        h5start[1]  = (hsize_t)bytes_begin[1];
                        h5stride[0] = 1;
                        h5stride[1] = h5block[0] = h5block[1] = blk_size;
                        h5count[0]                            = 1;
                        h5count[1]                            = buf_size / blk_size;
                    } /* end if */
                    else {
                        /* Interleaved access pattern */
                        /* Skip offset over blocks of other processes */
                        h5start[0]  = (hsize_t)bytes_begin[0];
                        h5start[1]  = (hsize_t)bytes_begin[1];
                        h5stride[0] = blk_size;
                        h5stride[1] = blk_size * (size_t)pio_mpi_nprocs_g;
                        h5block[0] = h5block[1] = blk_size;
                        h5count[0]              = buf_size / blk_size;
                        h5count[1]              = 1;
                    } /* end else */
                    hrc = H5Sselect_hyperslab(h5dset_space_id, H5S_SELECT_SET, h5start, h5stride, h5count,
                                              h5block);
                    VRFY((hrc >= 0), "H5Sselect_hyperslab");
                } /* end if */
                else {
                    h5dset_space_id = H5Screate(H5S_SCALAR);
                    VRFY((h5dset_space_id >= 0), "H5Screate");
                } /* end else */

                /* Create the memory dataspace that corresponds to the xfer buffer */
                if (buf_size > 0) {
                    if (!parms->interleaved) {
                        h5dims[0] = blk_size;
                        h5dims[1] = buf_size;
                    }
                    else {
                        h5dims[0] = buf_size;
                        h5dims[1] = blk_size;
                    }
                    h5mem_space_id = H5Screate_simple(2, h5dims, NULL);
                    VRFY((h5mem_space_id >= 0), "H5Screate_simple");
                } /* end if */
                else {
                    h5mem_space_id = H5Screate(H5S_SCALAR);
                    VRFY((h5mem_space_id >= 0), "H5Screate");
                } /* end else */
            }     /* end else */

            /* Create the dataset transfer property list */
            h5dxpl = H5Pcreate(H5P_DATASET_XFER);
            if (h5dxpl < 0) {
                fprintf(stderr, "HDF5 Property List Create failed\n");
                GOTOERROR(FAIL);
            }

            /* Change to collective I/O, if asked */
            if (parms->collective) {
                hrc = H5Pset_dxpl_mpio(h5dxpl, H5FD_MPIO_COLLECTIVE);
                if (hrc < 0) {
                    fprintf(stderr, "HDF5 Property List Set failed\n");
                    GOTOERROR(FAIL);
                } /* end if */
            }     /* end if */
            break;

        default:
            break;
    } /* end switch */

    for (ndset = 1; ndset <= ndsets; ++ndset) {

        /* Calculate dataset offset within a file */

        /* create dataset */
        switch (parms->io_type) {
            case POSIXIO:
            case MPIO:
                /* both posix and mpi io just need dataset offset in file*/
                dset_offset = (ndset - 1) * nbytes;
                break;

            case PHDF5:
                snprintf(dname, sizeof(dname), "Dataset_%ld", ndset);
                h5ds_id = H5DOPEN(fd->h5fd, dname);
                if (h5ds_id < 0) {
                    fprintf(stderr, "HDF5 Dataset open failed\n");
                    GOTOERROR(FAIL);
                }
                break;

            default:
                break;
        }

        /* The task is to transfer bytes_count bytes, starting at
         * bytes_begin position, using transfer buffer of buf_size bytes.
         * If interleaved, select buf_size at a time, in round robin
         * fashion, according to number of process. Otherwise, select
         * all bytes_count in contiguous.
         */
        nbytes_xfer = 0;

        /* 1D dataspace */
        if (!parms->dim2d) {
            /* Set base file offset for all I/O patterns and POSIX access */
            posix_file_offset = dset_offset + bytes_begin[0];

            /* Set base file offset for all I/O patterns and MPI access */
            mpi_file_offset = (MPI_Offset)(dset_offset + bytes_begin[0]);
        } /* end if */
        else {
            /* Set base file offset for all I/O patterns and POSIX access */
            posix_file_offset = dset_offset + bytes_begin[0] * snbytes + bytes_begin[1];

            /* Set base file offset for all I/O patterns and MPI access */
            mpi_file_offset = (MPI_Offset)(dset_offset + bytes_begin[0] * snbytes + bytes_begin[1]);
        } /* end else */

        /* Start "raw data" read timer */
        io_time_set(res->timers, HDF5_RAW_READ_FIXED_DIMS, TSTART);

        while (nbytes_xfer < bytes_count) {
            /* Read */
            /* Calculate offset of read within a dataset/file */
            switch (parms->io_type) {
                case POSIXIO:
                    /* 1D dataspace */
                    if (!parms->dim2d) {
                        /* Contiguous pattern */
                        if (!parms->interleaved) {
                            /* Compute file offset */
                            file_offset = posix_file_offset + (off_t)nbytes_xfer;

                            /* only care if seek returns error */
                            rc = POSIXSEEK(fd->posixfd, file_offset) < 0 ? -1 : 0;
                            VRFY((rc == 0), "POSIXSEEK");

                            /* check if all bytes are read */
                            rc = ((ssize_t)buf_size == POSIXREAD(fd->posixfd, buffer, buf_size));
                            VRFY((rc != 0), "POSIXREAD");

                            /* Advance global offset in dataset */
                            nbytes_xfer += (off_t)buf_size;
                        } /* end if */
                        /* Interleaved access pattern */
                        else {
                            /* Set the base of user's buffer */
                            buf_p = (unsigned char *)buffer;

                            /* Set the number of bytes to transfer this time */
                            nbytes_toxfer = buf_size;

                            /* Loop over the buffers to read */
                            while (nbytes_toxfer > 0) {
                                /* Skip offset over blocks of other processes */
                                file_offset = posix_file_offset + (off_t)(nbytes_xfer * pio_mpi_nprocs_g);

                                /* only care if seek returns error */
                                rc = POSIXSEEK(fd->posixfd, file_offset) < 0 ? -1 : 0;
                                VRFY((rc == 0), "POSIXSEEK");

                                /* check if all bytes are read */
                                rc = ((ssize_t)blk_size == POSIXREAD(fd->posixfd, buf_p, blk_size));
                                VRFY((rc != 0), "POSIXREAD");

                                /* Advance location in buffer */
                                buf_p += blk_size;

                                /* Advance global offset in dataset */
                                nbytes_xfer += (off_t)blk_size;

                                /* Decrement number of bytes left this time */
                                nbytes_toxfer -= blk_size;
                            } /* end while */
                        }     /* end else */
                    }         /* end if */
                    /* 2D dataspace */
                    else {
                        /* Contiguous storage */
                        if (!parms->h5_use_chunks) {
                            /* Contiguous access pattern */
                            if (!parms->interleaved) {
                                /* Compute file offset */
                                file_offset = posix_file_offset +
                                              (off_t)((((size_t)nbytes_xfer / blk_size) / (size_t)snbytes) *
                                                          (blk_size * (size_t)snbytes) +
                                                      (((size_t)nbytes_xfer / blk_size) % (size_t)snbytes));

                                /* Number of bytes to be transferred per I/O operation */
                                nbytes_xfer_advance = buf_size;

                                /* Global offset advance after each I/O operation */
                                file_offset_advance = (off_t)snbytes;
                            } /* end if */
                            /* Interleaved access pattern */
                            else {
                                /* Compute file offset */
                                file_offset =
                                    posix_file_offset +
                                    (off_t)(((((size_t)nbytes_xfer / buf_size) * (size_t)pio_mpi_nprocs_g) /
                                             (size_t)snbytes) *
                                                (buf_size * (size_t)snbytes) +
                                            (((size_t)nbytes_xfer / buf_size) * (size_t)pio_mpi_nprocs_g) %
                                                (size_t)snbytes);

                                /* Number of bytes to be transferred per I/O operation */
                                nbytes_xfer_advance = blk_size;

                                /* Global offset advance after each I/O operation */
                                file_offset_advance = (off_t)snbytes;
                            } /* end else */
                        }     /* end if */
                        /* Chunked storage */
                        else {
                            /*Contiguous access pattern */
                            if (!parms->interleaved) {
                                /* Compute file offset */
                                file_offset = posix_file_offset + (off_t)nbytes_xfer;

                                /* Number of bytes to be transferred per I/O operation */
                                nbytes_xfer_advance = blk_size * buf_size;

                                /* Global offset advance after each I/O operation */
                                file_offset_advance = 0;
                            } /* end if */
                            /*Interleaved access pattern */
                            else {
                                /* Compute file offset */
                                /* Before simplification */
                                /* file_offset=posix_file_offset+(off_t)((nbytes_xfer/(buf_size/blk_size)
                                 *pio_mpi_nprocs_g)/(snbytes/blk_size*(blk_size*blk_size))*(buf_size/blk_size
                                 *snbytes/blk_size*(blk_size*blk_size))+((nbytes_xfer/(buf_size/blk_size))
                                 *pio_mpi_nprocs_g)%(snbytes/blk_size*(blk_size*blk_size))); */

                                file_offset = posix_file_offset +
                                              (off_t)((((size_t)nbytes_xfer / (buf_size / blk_size) *
                                                        (size_t)pio_mpi_nprocs_g) /
                                                       ((size_t)snbytes * blk_size)) *
                                                          (buf_size * (size_t)snbytes) +
                                                      (((size_t)nbytes_xfer / (buf_size / blk_size)) *
                                                       (size_t)pio_mpi_nprocs_g) %
                                                          ((size_t)snbytes * blk_size));

                                /* Number of bytes to be transferred per I/O operation */
                                nbytes_xfer_advance = blk_size * blk_size;

                                /* Global offset advance after each I/O operation */
                                /* file_offset_advance = (off_t)(snbytes/blk_size*(blk_size*blk_size)); */
                                file_offset_advance = (off_t)((size_t)snbytes * blk_size);
                            } /* end else */
                        }     /* end else */

                        /* Common code for file access */

                        /* Set the base of user's buffer */
                        buf_p = (unsigned char *)buffer;

                        /* Set the number of bytes to transfer this time */
                        nbytes_toxfer = buf_size * blk_size;

                        /* Loop over portions of the buffer to read */
                        while (nbytes_toxfer > 0) {
                            /* only care if seek returns error */
                            rc = POSIXSEEK(fd->posixfd, file_offset) < 0 ? -1 : 0;
                            VRFY((rc == 0), "POSIXSEEK");

                            /* check if all bytes are read */
                            rc = ((ssize_t)nbytes_xfer_advance ==
                                  POSIXREAD(fd->posixfd, buf_p, nbytes_xfer_advance));
                            VRFY((rc != 0), "POSIXREAD");

                            /* Advance location in buffer */
                            buf_p += nbytes_xfer_advance;

                            /* Advance global offset in dataset */
                            nbytes_xfer += (off_t)nbytes_xfer_advance;

                            /* Decrement number of bytes left this time */
                            nbytes_toxfer -= nbytes_xfer_advance;

                            /* Partially advance file offset */
                            file_offset += file_offset_advance;
                        } /* end while */

                    } /* end else */
                    break;

                case MPIO:
                    /* 1D dataspace */
                    if (!parms->dim2d) {
                        /* Independent file access */
                        if (!parms->collective) {
                            /* Contiguous pattern */
                            if (!parms->interleaved) {
                                /* Compute offset in file */
                                mpi_offset = mpi_file_offset + nbytes_xfer;

                                /* Perform independent read */
                                mrc = MPI_File_read_at(fd->mpifd, mpi_offset, buffer,
                                                       (int)(buf_size / blk_size), mpi_blk_type, &mpi_status);
                                VRFY((mrc == MPI_SUCCESS), "MPIO_READ");

                                /* Advance global offset in dataset */
                                nbytes_xfer += (off_t)buf_size;
                            } /* end if */
                            /* Interleaved access pattern */
                            else {
                                /* Set the base of user's buffer */
                                buf_p = (unsigned char *)buffer;

                                /* Set the number of bytes to transfer this time */
                                nbytes_toxfer = buf_size;

                                /* Loop over the buffers to read */
                                while (nbytes_toxfer > 0) {
                                    /* Skip offset over blocks of other processes */
                                    mpi_offset = mpi_file_offset + (nbytes_xfer * pio_mpi_nprocs_g);

                                    /* Perform independent read */
                                    mrc = MPI_File_read_at(fd->mpifd, mpi_offset, buf_p, (int)1, mpi_blk_type,
                                                           &mpi_status);
                                    VRFY((mrc == MPI_SUCCESS), "MPIO_READ");

                                    /* Advance location in buffer */
                                    buf_p += blk_size;

                                    /* Advance global offset in dataset */
                                    nbytes_xfer += (off_t)blk_size;

                                    /* Decrement number of bytes left this time */
                                    nbytes_toxfer -= blk_size;
                                } /* end while */
                            }     /* end else */
                        }         /* end if */
                        /* Collective file access */
                        else {
                            /* Contiguous access pattern */
                            if (!parms->interleaved) {
                                /* Compute offset in file */
                                mpi_offset = mpi_file_offset + nbytes_xfer;

                                /* Perform collective read */
                                mrc = MPI_File_read_at_all(fd->mpifd, mpi_offset, buffer,
                                                           (int)(buf_size / blk_size), mpi_blk_type,
                                                           &mpi_status);
                                VRFY((mrc == MPI_SUCCESS), "MPIO_READ");

                                /* Advance global offset in dataset */
                                nbytes_xfer += (off_t)buf_size;
                            } /* end if */
                            /* Interleaved access pattern */
                            else {
                                /* Compute offset in file */
                                mpi_offset = mpi_file_offset + (nbytes_xfer * pio_mpi_nprocs_g);

                                /* Set the file view */
                                mrc = MPI_File_set_view(fd->mpifd, mpi_offset, mpi_blk_type, mpi_file_type,
                                                        "native", h5_io_info_g);
                                VRFY((mrc == MPI_SUCCESS), "MPIO_VIEW");

                                /* Perform collective read */
                                mrc = MPI_File_read_at_all(fd->mpifd, 0, buffer, (int)(buf_size / blk_size),
                                                           mpi_blk_type, &mpi_status);
                                VRFY((mrc == MPI_SUCCESS), "MPIO_READ");

                                /* Advance global offset in dataset */
                                nbytes_xfer += (off_t)buf_size;
                            } /* end else */
                        }     /* end else */
                    }         /* end if */
                    /* 2D dataspace */
                    else {
                        /* Contiguous storage */
                        if (!parms->h5_use_chunks) {
                            /* Contiguous access pattern */
                            if (!parms->interleaved) {
                                /* Compute offset in file */
                                mpi_offset =
                                    mpi_file_offset +
                                    (MPI_Offset)((((size_t)nbytes_xfer / blk_size) / (size_t)snbytes) *
                                                 (blk_size * (size_t)snbytes)) +
                                    (MPI_Offset)(((size_t)nbytes_xfer / blk_size) % (size_t)snbytes);

                                /* Number of bytes to be transferred per I/O operation */
                                nbytes_xfer_advance = buf_size;

                                /* Global offset advance after each I/O operation */
                                mpi_offset_advance = snbytes;

                                /* MPI type to be used for collective access */
                                mpi_collective_type = mpi_cont_type;
                            } /* end if */
                            /* Interleaved access pattern */
                            else {
                                /* Compute offset in file */
                                mpi_offset = mpi_file_offset +
                                             (MPI_Offset)(((((size_t)nbytes_xfer / buf_size) *
                                                            (size_t)pio_mpi_nprocs_g) /
                                                           (size_t)snbytes) *
                                                          (buf_size * (size_t)snbytes)) +
                                             (MPI_Offset)((((size_t)nbytes_xfer / buf_size) *
                                                           (size_t)pio_mpi_nprocs_g) %
                                                          (size_t)snbytes);

                                /* Number of bytes to be transferred per I/O operation */
                                nbytes_xfer_advance = blk_size;

                                /* Global offset advance after each I/O operation */
                                mpi_offset_advance = snbytes;

                                /* MPI type to be used for collective access */
                                mpi_collective_type = mpi_inter_type;
                            } /* end else */
                        }     /* end if */
                        /* Chunked storage */
                        else {
                            /*Contiguous access pattern */
                            if (!parms->interleaved) {
                                /* Compute offset in file */
                                mpi_offset = mpi_file_offset + nbytes_xfer;

                                /* Number of bytes to be transferred per I/O operation */
                                nbytes_xfer_advance = blk_size * buf_size;

                                /* Global offset advance after each I/O operation */
                                mpi_offset_advance = 0;

                                /* MPI type to be used for collective access */
                                mpi_collective_type = mpi_full_buffer;
                            } /* end if */
                            /*Interleaved access pattern */
                            else {
                                /* Compute offset in file */
                                /* Before simplification */
                                /* mpi_offset=mpi_file_offset+(nbytes_xfer/(buf_size/blk_size)
                                    *pio_mpi_nprocs_g)/(snbytes/blk_size*(blk_size*blk_size))*
                                    (buf_size/blk_size*snbytes/blk_size*(blk_size*blk_size))+
                                    ((nbytes_xfer/(buf_size/blk_size))*pio_mpi_nprocs_g)%(snbytes
                                    /blk_size*(blk_size*blk_size)); */
                                mpi_offset = mpi_file_offset +
                                             (MPI_Offset)((((size_t)nbytes_xfer / (buf_size / blk_size) *
                                                            (size_t)pio_mpi_nprocs_g) /
                                                           ((size_t)snbytes * blk_size)) *
                                                          (buf_size * (size_t)snbytes)) +
                                             (MPI_Offset)((((size_t)nbytes_xfer / (buf_size / blk_size)) *
                                                           (size_t)pio_mpi_nprocs_g) %
                                                          ((size_t)snbytes * blk_size));

                                /* Number of bytes to be transferred per I/O operation */
                                nbytes_xfer_advance = blk_size * blk_size;

                                /* Global offset advance after each I/O operation */
                                /* mpi_offset_advance = (MPI_Offset)(snbytes/blk_size*(blk_size*blk_size)); */
                                mpi_offset_advance = (MPI_Offset)((size_t)snbytes * blk_size);

                                /* MPI type to be used for collective access */
                                mpi_collective_type = mpi_chunk_inter_type;
                            } /* end else */
                        }     /* end else */

                        /* Common code for independent file access */
                        if (!parms->collective) {
                            /* Set the base of user's buffer */
                            buf_p = (unsigned char *)buffer;

                            /* Set the number of bytes to transfer this time */
                            nbytes_toxfer = buf_size * blk_size;

                            /* Loop over portions of the buffer to read */
                            while (nbytes_toxfer > 0) {
                                /* Perform independent read */
                                mrc = MPI_File_read_at(fd->mpifd, mpi_offset, buf_p, (int)nbytes_xfer_advance,
                                                       MPI_BYTE, &mpi_status);
                                VRFY((mrc == MPI_SUCCESS), "MPIO_READ");

                                /* Advance location in buffer */
                                buf_p += nbytes_xfer_advance;

                                /* Advance global offset in dataset */
                                nbytes_xfer += (off_t)nbytes_xfer_advance;

                                /* Decrement number of bytes left this time */
                                nbytes_toxfer -= nbytes_xfer_advance;

                                /* Partially advance global offset in dataset */
                                mpi_offset += mpi_offset_advance;
                            } /* end while */
                        }     /* end if */

                        /* Common code for collective file access */
                        else {
                            /* Set the file view */
                            mrc = MPI_File_set_view(fd->mpifd, mpi_offset, MPI_BYTE, mpi_collective_type,
                                                    "native", h5_io_info_g);
                            VRFY((mrc == MPI_SUCCESS), "MPIO_VIEW");

                            /* Perform read */
                            MPI_File_read_at_all(fd->mpifd, 0, buffer, (int)(buf_size * blk_size), MPI_BYTE,
                                                 &mpi_status);
                            VRFY((mrc == MPI_SUCCESS), "MPIO_READ");

                            /* Advance global offset in dataset */
                            nbytes_xfer += (off_t)buf_size * (off_t)blk_size;
                        } /* end else */

                    } /* end else */
                    break;

                case PHDF5:
                    /* 1D dataspace */
                    if (!parms->dim2d) {
                        /* Set up the file dset space id to move the selection to process */
                        if (!parms->interleaved) {
                            /* Contiguous pattern */
                            h5offset[0] = nbytes_xfer;
                        } /* end if */
                        else {
                            /* Interleaved access pattern */
                            /* Skip offset over blocks of other processes */
                            h5offset[0] = (nbytes_xfer * pio_mpi_nprocs_g);
                        } /* end else */
                        hrc = H5Soffset_simple(h5dset_space_id, h5offset);
                        VRFY((hrc >= 0), "H5Soffset_simple");

                        /* Read the buffer in */
                        hrc = H5Dread(h5ds_id, ELMT_H5_TYPE, h5mem_space_id, h5dset_space_id, h5dxpl, buffer);
                        VRFY((hrc >= 0), "H5Dread");

                        /* Increment number of bytes transferred */
                        nbytes_xfer += (off_t)buf_size;
                    } /* end if */
                    /* 2D dataspace */
                    else {
                        /* Set up the file dset space id to move the selection to process */
                        if (!parms->interleaved) {
                            /* Contiguous pattern */
                            h5offset[0] =
                                (hssize_t)(((size_t)nbytes_xfer / ((size_t)snbytes * blk_size)) * blk_size);
                            h5offset[1] =
                                (hssize_t)(((size_t)nbytes_xfer % ((size_t)snbytes * blk_size)) / blk_size);
                        } /* end if */
                        else {
                            /* Interleaved access pattern */
                            /* Skip offset over blocks of other processes */
                            h5offset[0] = (hssize_t)((((size_t)nbytes_xfer * (size_t)pio_mpi_nprocs_g) /
                                                      ((size_t)snbytes * buf_size)) *
                                                     buf_size);
                            h5offset[1] = (hssize_t)((((size_t)nbytes_xfer * (size_t)pio_mpi_nprocs_g) %
                                                      ((size_t)snbytes * buf_size)) /
                                                     buf_size);

                        } /* end else */
                        hrc = H5Soffset_simple(h5dset_space_id, h5offset);
                        VRFY((hrc >= 0), "H5Soffset_simple");

                        /* Write the buffer out */
                        hrc = H5Dread(h5ds_id, ELMT_H5_TYPE, h5mem_space_id, h5dset_space_id, h5dxpl, buffer);
                        VRFY((hrc >= 0), "H5Dread");

                        /* Increment number of bytes transferred */
                        nbytes_xfer += (off_t)buf_size * (off_t)blk_size;

                    } /* end else */
                    break;

                default:
                    break;
            } /* switch (parms->io_type) */

            /* Verify raw data, if asked */
            if (parms->verify) {
                /* Verify data read */
                unsigned char *ucharptr = (unsigned char *)buffer;
                size_t         i;
                int            nerror = 0;

                for (i = 0; i < bsize; ++i) {
                    if (*ucharptr++ != pio_mpi_rank_g + 1) {
                        if (++nerror < 20) {
                            /* report at most 20 errors */
                            HDprint_rank(output);
                            fprintf(output,
                                    "read data error, expected (%d), "
                                    "got (%d)\n",
                                    pio_mpi_rank_g + 1, (int)*(ucharptr - 1));
                        } /* end if */
                    }     /* end if */
                }         /* end for */
                if (nerror >= 20) {
                    HDprint_rank(output);
                    fprintf(output, "...");
                    fprintf(output, "total read data errors=%d\n", nerror);
                } /* end if */
            }     /* if (parms->verify) */

        } /* end while */

        /* Stop "raw data" read timer */
        io_time_set(res->timers, HDF5_RAW_READ_FIXED_DIMS, TSTOP);

        /* Calculate read time */

        /* Close dataset. Only HDF5 needs to do an explicit close. */
        if (parms->io_type == PHDF5) {
            hrc = H5Dclose(h5ds_id);

            if (hrc < 0) {
                fprintf(stderr, "HDF5 Dataset Close failed\n");
                GOTOERROR(FAIL);
            }

            h5ds_id = H5I_INVALID_HID;
        } /* end if */
    }     /* end for */

done:
    /* release MPI-I/O objects */
    if (parms->io_type == MPIO) {
        /* 1D dataspace */
        if (!parms->dim2d) {
            /* Free file type */
            mrc = MPI_Type_free(&mpi_file_type);
            VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_FREE");

            /* Free buffer type */
            mrc = MPI_Type_free(&mpi_blk_type);
            VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_FREE");
        } /* end if */
        /* 2D dataspace */
        else {
            /* Free partial buffer type for contiguous access */
            mrc = MPI_Type_free(&mpi_partial_buffer_cont);
            VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_FREE");

            /* Free contiguous file type */
            mrc = MPI_Type_free(&mpi_cont_type);
            VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_FREE");

            /* Free partial buffer type for interleaved access */
            mrc = MPI_Type_free(&mpi_partial_buffer_inter);
            VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_FREE");

            /* Free interleaved file type */
            mrc = MPI_Type_free(&mpi_inter_type);
            VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_FREE");

            /* Free full buffer type */
            mrc = MPI_Type_free(&mpi_full_buffer);
            VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_FREE");

            /* Free full chunk type */
            mrc = MPI_Type_free(&mpi_full_chunk);
            VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_FREE");

            /* Free chunk interleaved file type */
            mrc = MPI_Type_free(&mpi_chunk_inter_type);
            VRFY((mrc == MPI_SUCCESS), "MPIO_TYPE_FREE");
        } /* end else */
    }     /* end if */

    /* release HDF5 objects */
    if (h5dset_space_id != -1) {
        hrc = H5Sclose(h5dset_space_id);
        if (hrc < 0) {
            fprintf(stderr, "HDF5 Dataset Space Close failed\n");
            ret_code = FAIL;
        }
        else {
            h5dset_space_id = H5I_INVALID_HID;
        }
    }

    if (h5mem_space_id != -1) {
        hrc = H5Sclose(h5mem_space_id);
        if (hrc < 0) {
            fprintf(stderr, "HDF5 Memory Space Close failed\n");
            ret_code = FAIL;
        }
        else {
            h5mem_space_id = H5I_INVALID_HID;
        }
    }

    if (h5dxpl != -1) {
        hrc = H5Pclose(h5dxpl);
        if (hrc < 0) {
            fprintf(stderr, "HDF5 Dataset Transfer Property List Close failed\n");
            ret_code = FAIL;
        }
        else {
            h5dxpl = H5I_INVALID_HID;
        }
    }

    return ret_code;
}

/*
 * Function:    do_fopen
 * Purpose:     Open the specified file.
 * Return:      SUCCESS or FAIL
 */
static herr_t
do_fopen(parameters *param, char *fname, file_descr *fd /*out*/, int flags)
{
    int   ret_code = SUCCESS, mrc;
    hid_t acc_tpl  = H5I_INVALID_HID; /* file access templates */

    switch (param->io_type) {
        case POSIXIO:
            if (flags & (PIO_CREATE | PIO_WRITE))
                fd->posixfd = POSIXCREATE(fname);
            else
                fd->posixfd = POSIXOPEN(fname, O_RDONLY);

            if (fd->posixfd < 0) {
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
                mrc = MPI_File_open(pio_comm_g, fname, MPI_MODE_CREATE | MPI_MODE_RDWR, h5_io_info_g,
                                    &fd->mpifd);

                if (mrc != MPI_SUCCESS) {
                    fprintf(stderr, "MPI File Open failed(%s)\n", fname);
                    GOTOERROR(FAIL);
                }

                /*since MPI_File_open with MPI_MODE_CREATE does not truncate  */
                /*filesize , set size to 0 explicitedly.    */
                mrc = MPI_File_set_size(fd->mpifd, (MPI_Offset)0);
                if (mrc != MPI_SUCCESS) {
                    fprintf(stderr, "MPI_File_set_size failed\n");
                    GOTOERROR(FAIL);
                }
            }
            else {
                mrc = MPI_File_open(pio_comm_g, fname, MPI_MODE_RDONLY, h5_io_info_g, &fd->mpifd);
                if (mrc != MPI_SUCCESS) {
                    fprintf(stderr, "MPI File Open failed(%s)\n", fname);
                    GOTOERROR(FAIL);
                }
            }

            break;

        case PHDF5:
            if ((acc_tpl = H5Pcreate(H5P_FILE_ACCESS)) < 0) {
                fprintf(stderr, "HDF5 Property List Create failed\n");
                GOTOERROR(FAIL);
            }

            /* Set the file driver to the MPI-IO driver */
            if (H5Pset_fapl_mpio(acc_tpl, pio_comm_g, h5_io_info_g) < 0) {
                fprintf(stderr, "HDF5 Property List Set failed\n");
                GOTOERROR(FAIL);
            }

            /* Set the alignment of objects in HDF5 file */
            if (H5Pset_alignment(acc_tpl, param->h5_thresh, param->h5_align) < 0) {
                fprintf(stderr, "HDF5 Property List Set failed\n");
                GOTOERROR(FAIL);
            }

            /* create the parallel file */
            if (flags & (PIO_CREATE | PIO_WRITE))
                fd->h5fd = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, acc_tpl);
            else
                fd->h5fd = H5Fopen(fname, H5F_ACC_RDONLY, acc_tpl);
            if (fd->h5fd < 0) {
                fprintf(stderr, "HDF5 File Create failed(%s)\n", fname);
                GOTOERROR(FAIL);
            }

            /* verifying the close of the acc_tpl */
            if (H5Pclose(acc_tpl) < 0) {
                fprintf(stderr, "HDF5 Property List Close failed\n");
                GOTOERROR(FAIL);
            }

            break;

        default:
            break;
    }

done:
    return ret_code;
}

/*
 * Function:    do_fclose
 * Purpose:     Close the specified file descriptor.
 * Return:      SUCCESS or FAIL
 */
static herr_t
do_fclose(iotype iot, file_descr *fd /*out*/)
{
    herr_t ret_code = SUCCESS, hrc;
    int    mrc = 0, rc = 0;

    switch (iot) {
        case POSIXIO:
            rc = POSIXCLOSE(fd->posixfd);

            if (rc != 0) {
                fprintf(stderr, "POSIX File Close failed\n");
                GOTOERROR(FAIL);
            }

            fd->posixfd = -1;
            break;

        case MPIO:
            mrc = MPI_File_close(&fd->mpifd);

            if (mrc != MPI_SUCCESS) {
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

        default:
            break;
    }

done:
    return ret_code;
}

/*
 * Function:    do_fclose
 * Purpose:     Cleanup temporary file unless HDF5_NOCLEANUP is set.
 *      Only Proc 0 of the PIO communicator will do the cleanup.
 *      Other processes just return.
 * Return:      void
 */
static void
do_cleanupfile(iotype iot, char *fname)
{
    if (pio_mpi_rank_g != 0)
        return;

    if (clean_file_g == -1)
        clean_file_g = (getenv(HDF5_NOCLEANUP) == NULL) ? 1 : 0;

    if (clean_file_g) {
        switch (iot) {
            case POSIXIO:
                HDremove(fname);
                break;
            case MPIO:
            case PHDF5:
                MPI_File_delete(fname, h5_io_info_g);
                break;
            default:
                break;
        }
    }
}

#ifdef TIME_MPI
/* instrument the MPI_File_wrirte_xxx and read_xxx calls to measure
 * pure time spent in MPI_File code.
 */
int
MPI_File_read_at(MPI_File fh, MPI_Offset offset, void *buf, int count, MPI_Datatype datatype,
                 MPI_Status *status)
{
    int err;
    io_time_set(timer_g, HDF5_MPI_READ, TSTART);
    err = PMPI_File_read_at(fh, offset, buf, count, datatype, status);
    io_time_set(timer_g, HDF5_MPI_READ, TSTOP);
    return err;
}

int
MPI_File_read_at_all(MPI_File fh, MPI_Offset offset, void *buf, int count, MPI_Datatype datatype,
                     MPI_Status *status)
{
    int err;
    io_time_set(timer_g, HDF5_MPI_READ, TSTART);
    err = PMPI_File_read_at_all(fh, offset, buf, count, datatype, status);
    io_time_set(timer_g, HDF5_MPI_READ, TSTOP);
    return err;
}

int
MPI_File_write_at(MPI_File fh, MPI_Offset offset, void *buf, int count, MPI_Datatype datatype,
                  MPI_Status *status)
{
    int err;
    io_time_set(timer_g, HDF5_MPI_WRITE, TSTART);
    err = PMPI_File_write_at(fh, offset, buf, count, datatype, status);
    io_time_set(timer_g, HDF5_MPI_WRITE, TSTOP);
    return err;
}

int
MPI_File_write_at_all(MPI_File fh, MPI_Offset offset, void *buf, int count, MPI_Datatype datatype,
                      MPI_Status *status)
{
    int err;
    io_time_set(timer_g, HDF5_MPI_WRITE, TSTART);
    err = PMPI_File_write_at_all(fh, offset, buf, count, datatype, status);
    io_time_set(timer_g, HDF5_MPI_WRITE, TSTOP);
    return err;
}

#endif /* TIME_MPI */
#endif /* H5_HAVE_PARALLEL */
