/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Author: Christian Chilan, April 2008
 */

#include "hdf5.h"

#ifdef H5_STDC_HEADERS
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#endif

#ifdef H5_HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif

#ifdef H5_HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#include "sio_perf.h"

/* Macro definitions */

/* sizes of various items. these sizes won't change during program execution */
#define ELMT_H5_TYPE        H5T_NATIVE_UCHAR

#define GOTOERROR(errcode)  { ret_code = errcode; goto done; }
#define ERRMSG(mesg) {                                                  \
    HDfprintf(stderr, "*** Assertion failed (%s) at line %4d in %s\n",    \
        mesg, (int)__LINE__, __FILE__);                             \
}

/* verify: if val is false (0), print mesg. */
#define VRFY(val, mesg) do {                            \
    if (!val) {                                         \
    ERRMSG(mesg);                                   \
    GOTOERROR(FAIL);                                \
    }                                                   \
} while(0)

/* POSIX I/O macros */
#ifdef H5_HAVE_WIN32_API
/* Can't link against the library, so this test will use the older, non-Unicode
 * _open() call on Windows.
 */
#define HDopen(S,F,...)           _open(S, F | _O_BINARY, __VA_ARGS__)
#endif /* H5_HAVE_WIN32_API */
#define POSIXCREATE(fn)           HDopen(fn, O_CREAT|O_TRUNC|O_RDWR, 0600)
#define POSIXOPEN(fn, F)          HDopen(fn, F, 0600)
#define POSIXCLOSE(F)             HDclose(F)
#define POSIXSEEK(F,L)            HDlseek(F, L, SEEK_SET)
#define POSIXWRITE(F,B,S)         HDwrite(F,B,S)
#define POSIXREAD(F,B,S)          HDread(F,B,S)

enum {
    SIO_CREATE = 1,
    SIO_WRITE = 2,
    SIO_READ = 4
};

/* Global variables */
static int  clean_file_g = -1;  /*whether to cleanup temporary test     */
/*files. -1 is not defined;             */
/*0 is no cleanup; 1 is do cleanup      */

/* the different types of file descriptors we can expect */
typedef union _file_descr {
    int         posixfd;    /* POSIX file handle*/
    hid_t       h5fd;       /* HDF5 file        */
} file_descr;

/* local functions */
static char  *sio_create_filename(iotype iot, const char *base_name,
    char *fullname, size_t size, parameters *param);
static herr_t do_write(results *res, file_descr *fd, parameters *parms, void *buffer);
static herr_t do_read(results *res, file_descr *fd, parameters *parms, void *buffer);
static herr_t dset_write(int local_dim, file_descr *fd, parameters *parms, void *buffer);
static herr_t posix_buffer_write(int local_dim, file_descr *fd, parameters *parms, void *buffer);
static herr_t dset_read(int localrank, file_descr *fd, parameters *parms, void *buffer, const char *buffer2);
static herr_t posix_buffer_read(int local_dim, file_descr *fd, parameters *parms, void *buffer);
static herr_t do_fopen(parameters *param, char *fname, file_descr *fd /*out*/,
    int flags);
hid_t set_vfd(parameters *param);
static herr_t do_fclose(iotype iot, file_descr *fd);
static void do_cleanupfile(iotype iot, char *fname);

/* global variables */
static HDoff_t offset[MAX_DIMS];       /* dataset size in bytes     */
static size_t buf_offset[MAX_DIMS];   /* dataset size in bytes     */
static int order[MAX_DIMS];        /* dimension access order */
static size_t      linear_buf_size;        /* linear buffer size     */
static int         cont_dim;       /* lowest dimension for contiguous POSIX
                                      access */
static size_t      cont_size;      /* size of contiguous POSIX access */
static hid_t       fapl;           /* file access list */
static unsigned char *buf_p;       /* buffer pointer */
static const char *multi_letters = "msbrglo"; /* string for multi driver */

/* HDF5 global variables */
static hsize_t     h5count[MAX_DIMS];      /*selection count               */
static hssize_t    h5offset[MAX_DIMS];     /* Selection offset within dataspace */
static hid_t       h5dset_space_id = -1;   /*dataset space ID              */
static hid_t       h5mem_space_id = -1;    /*memory dataspace ID           */
static hid_t       h5ds_id = -1;           /*dataset handle                */
static hid_t       h5dcpl = -1;            /* Dataset creation property list */
static hid_t       h5dxpl = -1;            /* Dataset transfer property list */

/*
 * Function:        do_sio
 * Purpose:         SIO Engine where IO are executed.
 * Return:          results
 * Programmer:      Christian Chilan, April, 2008
 * Modifications:
 */
void
do_sio(parameters param, results *res)
{
    char       *buffer = NULL; /*data buffer pointer           */
    size_t      buf_size[MAX_DIMS];     /* general buffer size in bytes     */
    file_descr  fd;                     /* file handles */
    iotype      iot;                    /* API type */
    char base_name[256];                /* test file base name */
    /* return codes */
    herr_t      ret_code = 0;   /*return code                           */

    char        fname[FILENAME_MAX];    /* test file name */
    int     i;
    /* HDF5 variables */
    herr_t          hrc;        /*HDF5 return code              */

    /* Sanity check parameters */

    /* IO type */
    iot = param.io_type;

    switch (iot) {
    case POSIXIO:
        fd.posixfd = -1;
        res->timers = io_time_new(SYS_CLOCK);
        break;
    case HDF5:
        fd.h5fd = -1;
        res->timers = io_time_new(SYS_CLOCK);
        break;
    default:
        /* unknown request */
        HDfprintf(stderr, "Unknown IO type request (%d)\n", (int)iot);
        GOTOERROR(FAIL);
    }

    linear_buf_size = 1;

    for (i=0; i<param.rank; i++){
        buf_size[i] = param.buf_size[i];
        order[i] = param.order[i];
        linear_buf_size *= buf_size[i];
        buf_offset[i] = 0;
        offset[i] = 0;

        /* Validate transfer buffer size */
        if (param.buf_size[i]<=0) {
            HDfprintf(stderr,
            "Transfer buffer size[%d] (%zu) must be > 0\n", i,buf_size[i]);
            GOTOERROR(FAIL);
        }

        if ((param.dset_size[i]%param.buf_size[i])!=0) {
            HDfprintf(stderr,
            "Dataset size[%d] (%" H5_PRINTF_LL_WIDTH "d) must be a multiple of the "
            "trasfer buffer size[%d] (%zu)\n",param.rank,
            (long long)param.dset_size[i], param.rank, param.buf_size[i]);
            GOTOERROR(FAIL);
        }

    }

    /* Allocate transfer buffer */
    if ((buffer = (char *)malloc(linear_buf_size)) == NULL){
        HDfprintf(stderr, "malloc for transfer buffer size (%zu) failed\n", linear_buf_size);
        GOTOERROR(FAIL);
    }

    if (sio_debug_level >= 4)

    /* output all of the times for all iterations */
        HDfprintf(output, "Timer details:\n");

    /*
     * Write performance measurement
     */
    /* Open file for write */

    HDstrcpy(base_name, "#sio_tmp");
    sio_create_filename(iot, base_name, fname, sizeof(fname), &param);

    if (sio_debug_level > 0)
        HDfprintf(output, "data filename=%s\n",
             fname);

    set_time(res->timers, HDF5_GROSS_WRITE_FIXED_DIMS, TSTART);
    hrc = do_fopen(&param, fname, &fd, SIO_CREATE | SIO_WRITE);
    VRFY((hrc == SUCCESS), "do_fopen failed");

    set_time(res->timers, HDF5_FINE_WRITE_FIXED_DIMS, TSTART);
    hrc = do_write(res, &fd, &param, buffer);
    set_time(res->timers, HDF5_FINE_WRITE_FIXED_DIMS, TSTOP);
    VRFY((hrc == SUCCESS), "do_write failed");

    /* Close file for write */
    hrc = do_fclose(iot, &fd);
    set_time(res->timers, HDF5_GROSS_WRITE_FIXED_DIMS, TSTOP);
    VRFY((hrc == SUCCESS), "do_fclose failed");

    if (!param.h5_write_only) {
        /*
         * Read performance measurement
         */

        /* Open file for read */
        set_time(res->timers, HDF5_GROSS_READ_FIXED_DIMS, TSTART);
        hrc = do_fopen(&param, fname, &fd, SIO_READ);
        VRFY((hrc == SUCCESS), "do_fopen failed");

        set_time(res->timers, HDF5_FINE_READ_FIXED_DIMS, TSTART);
        hrc = do_read(res, &fd, &param, buffer);
        set_time(res->timers, HDF5_FINE_READ_FIXED_DIMS, TSTOP);
        VRFY((hrc == SUCCESS), "do_read failed");

        /* Close file for read */
        hrc = do_fclose(iot, &fd);

        set_time(res->timers, HDF5_GROSS_READ_FIXED_DIMS, TSTOP);
        VRFY((hrc == SUCCESS), "do_fclose failed");
    }

    do_cleanupfile(iot, fname);

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
        case HDF5:
            if (fd.h5fd != -1)
                hrc = do_fclose(iot, &fd);
            break;
        default:
            /* unknown request */
            HDassert(0 && "Unknown IO type");
            break;
    }

    /* release generic resources */
    if (buffer)
        free(buffer);

    res->ret_code = ret_code;
}

/*
 * Function:    sio_create_filename
 * Purpose:     Create a new filename to write to. Determine the correct
 *              suffix to append to the filename by the type of I/O we're
 *              doing. Also, place in the /tmp/{$USER,$LOGIN} directory if
 *              USER or LOGIN are specified in the environment.
 * Return:      Pointer to filename or NULL
 * Programmer:  Bill Wendling, 21. November 2001
 * Modifications: Support for file drivers. Christian Chilan, April, 2008
 */
    static char *
sio_create_filename(iotype iot, const char *base_name, char *fullname, size_t size, parameters *param)
{
    const char *prefix, *suffix="";
    char *ptr, last = '\0';
    size_t i, j;
    vfdtype vfd;
    vfd = param->vfd;

    if (!base_name || !fullname || size < 1)
    return NULL;

    memset(fullname, 0, size);

    switch (iot) {
    case POSIXIO:
        suffix = ".posix";
        break;
    case HDF5:
        suffix = ".h5";
        if (vfd == family)
            suffix = "%05d.h5";
        else if (vfd == multi)
            suffix = NULL;
        break;
    default:
        /* unknown request */
        HDfprintf(stderr, "Unknown IO type request (%d)\n", (int)iot);
        HDassert(0 && "Unknown IO type");
        break;
    }

    /* First use the environment variable and then try the constant */
    prefix = HDgetenv("HDF5_PREFIX");

#ifdef HDF5_PREFIX
    if (!prefix)
    prefix = HDF5_PREFIX;
#endif  /* HDF5_PREFIX */

    /* Prepend the prefix value to the base name */
    if (prefix && *prefix) {
    /* If the prefix specifies the HDF5_PREFIX directory, then
     * default to using the "/tmp/$USER" or "/tmp/$LOGIN"
     * directory instead. */
    register char *user, *login, *subdir;

    user = HDgetenv("USER");
    login = HDgetenv("LOGIN");
    subdir = (user ? user : login);

    if (subdir) {
        for (i = 0; i < size-1 && prefix[i]; i++)
        fullname[i] = prefix[i];

        fullname[i++] = '/';

        for (j = 0; i < size && subdir[j]; i++, j++)
        fullname[i] = subdir[j];
    } else {
        /* We didn't append the prefix yet */
        HDstrncpy(fullname, prefix, size);
        fullname[size - 1] = '\0';
    }

    if ((HDstrlen(fullname) + HDstrlen(base_name) + 1) < size) {
        /* Append the base_name with a slash first. Multiple slashes are
         * handled below. */
        h5_stat_t buf;

        if (HDstat(fullname, &buf) < 0)
        /* The directory doesn't exist just yet */
        if (HDmkdir(fullname, 0755) < 0 && errno != EEXIST) {
            /* We couldn't make the "/tmp/${USER,LOGIN}" subdirectory.
             * Default to PREFIX's original prefix value. */
            HDstrcpy(fullname, prefix);
        }

        HDstrcat(fullname, "/");
        HDstrcat(fullname, base_name);
    } else {
        /* Buffer is too small */
        return NULL;
    }
    } else if (strlen(base_name) >= size) {
    /* Buffer is too small */
        return NULL;
    } else {
        HDstrcpy(fullname, base_name);
    }

    /* Append a suffix */
    if (suffix) {
        if (HDstrlen(fullname) + HDstrlen(suffix) >= size)
            return NULL;

        HDstrcat(fullname, suffix);
    }

    /* Remove any double slashes in the filename */
    for (ptr = fullname, i = j = 0; ptr && (i < size); i++, ptr++) {
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
 * Programmer:      Christian Chilan, April, 2008
 * Modifications:
 */
static herr_t
do_write(results *res, file_descr *fd, parameters *parms, void *buffer)
{
    int         ret_code = SUCCESS;
    char        dname[64];
    int        i;
    size_t     u;
    /* HDF5 variables */
    herr_t      hrc;                    /*HDF5 return code              */
    hsize_t     h5dims[MAX_DIMS];       /*dataset dim sizes             */
    hsize_t     h5chunk[MAX_DIMS];      /*dataset dim sizes             */
    hsize_t     h5block[MAX_DIMS];      /*dataspace selection           */
    hsize_t     h5stride[MAX_DIMS];     /*selection stride              */
    hsize_t     h5start[MAX_DIMS];      /*selection start               */
    hsize_t     h5maxdims[MAX_DIMS];
    int         rank;                   /*rank of dataset */

    /* Prepare buffer for verifying data */
/*    if (parms->verify)
            memset(buffer,1,linear_buf_size); */

    buf_p=(unsigned char *)buffer;

    for(u = 0; u < linear_buf_size; u++)
        buf_p[u] = u % 128;

    rank = parms->rank;

    for(i = 0; i < rank; i++)
        h5offset[i] = offset[i] = 0;

    /* I/O Access specific setup */
    switch (parms->io_type) {
    case POSIXIO:

            /* determine lowest dimension for contiguous POSIX access */
            cont_dim = rank;

            for (i=rank-1; i>=0; i--) {
                if (parms->buf_size[i]==parms->dset_size[i])
                    cont_dim = i;
                else
                    break;
            }

            /* determine size of the contiguous POSIX access */
            cont_size = (!cont_dim)? 1 : parms->buf_size[cont_dim-1];
            for (i=cont_dim; i<rank; i++)
                cont_size *= parms->buf_size[i];

        break;

    case HDF5: /* HDF5 setup */

        for (i=0; i < rank; i++){
            h5dims[i] = parms->dset_size[i];
            h5start[i] = 0;
            h5stride[i] = 1;
            h5block[i] = 1;
            h5count[i] = parms->buf_size[i];
            h5chunk[i] = parms->chk_size[i];
            h5maxdims[i] = H5S_UNLIMITED;

        }

        if (parms->h5_use_chunks && parms->h5_extendable) {
            h5dset_space_id = H5Screate_simple(rank, h5count, h5maxdims);
            VRFY((h5dset_space_id >= 0), "H5Screate_simple");
        }
        else {
            h5dset_space_id = H5Screate_simple(rank, h5dims, NULL);
            VRFY((h5dset_space_id >= 0), "H5Screate_simple");
        }

        hrc = H5Sselect_hyperslab(h5dset_space_id, H5S_SELECT_SET,
                    h5start, h5stride, h5count, h5block);
        VRFY((hrc >= 0), "H5Sselect_hyperslab");

        /* Create the memory dataspace that corresponds to the xfer buffer */
        h5mem_space_id = H5Screate_simple(rank, h5count, NULL);
        VRFY((h5mem_space_id >= 0), "H5Screate_simple");

        /* Create the dataset transfer property list */
        h5dxpl = H5Pcreate(H5P_DATASET_XFER);
        if (h5dxpl < 0) {
            HDfprintf(stderr, "HDF5 Property List Create failed\n");
            GOTOERROR(FAIL);
        }

        break;

    default:
        HDfprintf(stderr, "Unknown IO type request (%d)\n", (int)parms->io_type);
        GOTOERROR(FAIL);
        break;
    } /* end switch */


    /* create dataset */
    switch (parms->io_type) {
        case POSIXIO:
        break;

        case HDF5:
            h5dcpl = H5Pcreate(H5P_DATASET_CREATE);

        if (h5dcpl < 0) {
            HDfprintf(stderr, "HDF5 Property List Create failed\n");
            GOTOERROR(FAIL);
        }

        if(parms->h5_use_chunks) {
        /* Set the chunk size to be the same as the buffer size */
            hrc = H5Pset_chunk(h5dcpl, rank, h5chunk);
            if (hrc < 0) {
                HDfprintf(stderr, "HDF5 Property List Set failed\n");
                GOTOERROR(FAIL);
            } /* end if */
        } /* end if */

        HDsprintf(dname, "Dataset_%ld", (unsigned long)parms->num_bytes);
        h5ds_id = H5Dcreate2(fd->h5fd, dname, ELMT_H5_TYPE,
            h5dset_space_id, H5P_DEFAULT, h5dcpl, H5P_DEFAULT);

        if (h5ds_id < 0) {
            HDfprintf(stderr, "HDF5 Dataset Create failed\n");
            GOTOERROR(FAIL);
        }

        hrc = H5Pclose(h5dcpl);
        /* verifying the close of the dcpl */
        if (hrc < 0) {
            HDfprintf(stderr, "HDF5 Property List Close failed\n");
            GOTOERROR(FAIL);
        }
        break;

        default:
        /* unknown request */
        HDfprintf(stderr, "Unknown IO type request (%d)\n", (int)parms->io_type);
        GOTOERROR(FAIL);
        break;
    }

    /* Start "raw data" write timer */
    set_time(res->timers, HDF5_RAW_WRITE_FIXED_DIMS, TSTART);

    /* Perform write */
    hrc = dset_write(rank-1, fd, parms, buffer);

    if (hrc < 0) {
        HDfprintf(stderr, "Error in dataset write\n");
        GOTOERROR(FAIL);
    }


    /* Stop "raw data" write timer */
    set_time(res->timers, HDF5_RAW_WRITE_FIXED_DIMS, TSTOP);

    /* Calculate write time */

    /* Close dataset. Only HDF5 needs to do an explicit close. */
    if (parms->io_type == HDF5) {
        hrc = H5Dclose(h5ds_id);

        if (hrc < 0) {
            HDfprintf(stderr, "HDF5 Dataset Close failed\n");
            GOTOERROR(FAIL);
        }

        h5ds_id = -1;
    } /* end if */

done:

    /* release HDF5 objects */
    if (h5dset_space_id != -1) {
        hrc = H5Sclose(h5dset_space_id);
        if (hrc < 0){
            HDfprintf(stderr, "HDF5 Dataset Space Close failed\n");
            ret_code = FAIL;
        } else {
            h5dset_space_id = -1;
        }
    }

    if (h5mem_space_id != -1) {
        hrc = H5Sclose(h5mem_space_id);
        if (hrc < 0) {
            HDfprintf(stderr, "HDF5 Memory Space Close failed\n");
            ret_code = FAIL;
        } else {
            h5mem_space_id = -1;
        }
    }

    if (h5dxpl != -1) {
        hrc = H5Pclose(h5dxpl);
        if (hrc < 0) {
            HDfprintf(stderr, "HDF5 Dataset Transfer Property List Close failed\n");
            ret_code = FAIL;
        } else {
            h5dxpl = -1;
        }
    }

    return ret_code;
}

/*
 * Function:        dset_write
 * Purpose:         Write buffer into the dataset.
 * Return:          SUCCESS or FAIL
 * Programmer:      Christian Chilan, April, 2008
 * Modifications:
 */
static herr_t
dset_write(int local_dim, file_descr *fd, parameters *parms, void *buffer)
{
    int cur_dim = order[local_dim]-1;
    int         ret_code = SUCCESS;
    int         k;
    hsize_t  dims[MAX_DIMS], maxdims[MAX_DIMS];
    hsize_t i;
    int j;
    herr_t hrc;

    /* iterates according to the dimensions in order array */
    for (i=0; i < parms->dset_size[cur_dim]; i += parms->buf_size[cur_dim]){

        h5offset[cur_dim] = (hssize_t)i;
        offset[cur_dim] = (HDoff_t)i;

        if (local_dim > 0){

            dset_write(local_dim-1, fd, parms, buffer);

        }else{

            switch (parms->io_type) {

            case POSIXIO:
                /* initialize POSIX offset in the buffer */
                for(j = 0; j < parms->rank; j++)
                    buf_offset[j] = 0;
                buf_p = (unsigned char *)buffer;
                /* write POSIX buffer */
                posix_buffer_write(0, fd, parms, buffer);
                break;

            case HDF5:
                /* if dimensions are extendable, extend them as needed during access */
                if (parms->h5_use_chunks && parms->h5_extendable) {

                    hrc=H5Sget_simple_extent_dims(h5dset_space_id,dims,maxdims);
                    VRFY((hrc >= 0), "H5Sget_simple_extent_dims");

                    for (k=0; k < parms->rank; k++){

                        HDassert(h5offset[k] >= 0);
                        if (dims[k] <= (hsize_t)h5offset[k]) {
                            dims[k] = dims[k]+h5count[k];
                            hrc=H5Sset_extent_simple(h5dset_space_id,parms->rank,dims,maxdims);
                            VRFY((hrc >= 0), "H5Sset_extent_simple");
                            hrc=H5Dset_extent(h5ds_id,dims);
                            VRFY((hrc >= 0), "H5Dextend");
                        }
                    }
                }
                /* applies offset */
                hrc = H5Soffset_simple(h5dset_space_id, h5offset);
                VRFY((hrc >= 0), "H5Soffset_simple");

                /* Write the buffer out */
                hrc=H5Sget_simple_extent_dims(h5dset_space_id,dims,maxdims);
                hrc = H5Dwrite(h5ds_id, ELMT_H5_TYPE, h5mem_space_id,
                    h5dset_space_id, h5dxpl, buffer);
                VRFY((hrc >= 0), "H5Dwrite");

                break;

            default:
                /* unknown request */
                HDfprintf(stderr, "Unknown IO type request (%d)\n", (int)parms->io_type);
                HDassert(0 && "Unknown IO type");
                break;
            } /* switch (parms->io_type) */
        }
    }
done:
    return ret_code;
}

/*
 * Function:        posix_buffer_write
 * Purpose:         Write buffer into the POSIX file considering contiguity.
 * Return:          SUCCESS or FAIL
 * Programmer:      Christian Chilan, April, 2008
 * Modifications:
 */

static herr_t
posix_buffer_write(int local_dim, file_descr *fd, parameters *parms, void *buffer)
{
    int         ret_code = SUCCESS;

    /* if dimension is not contiguous, call recursively */
    if (local_dim < parms->rank-1 && local_dim != cont_dim) {
        size_t u;

        for(u = 0; u < parms->buf_size[local_dim]; u ++) {
            buf_offset[local_dim] = u;
            posix_buffer_write(local_dim+1, fd, parms, buffer);

            /* if next dimension is cont_dim, it will fill out the buffer
               traversing the entire dimension local_dim without the need
               of performing iteration */
            if (local_dim+1==cont_dim)
                break;
        }
    /* otherwise, perform contiguous POSIX access */
    } else {
        HDoff_t d_offset;
        HDoff_t linear_dset_offset = 0;
        int i, j, rc;

        buf_offset[local_dim] = 0;

        /* determine offset in the buffer */
        for(i = 0; i < parms->rank; i++) {
            d_offset = 1;

            for(j = i + 1; j < parms->rank; j++)
                d_offset *= (HDoff_t)parms->dset_size[j];

            linear_dset_offset += (offset[i] + (HDoff_t)buf_offset[i]) * d_offset;
        }

        /* only care if seek returns error */
        rc = POSIXSEEK(fd->posixfd, linear_dset_offset) < 0 ? -1 : 0;
        VRFY((rc==0), "POSIXSEEK");
        /* check if all bytes are written */
        rc = ((ssize_t)cont_size ==
             POSIXWRITE(fd->posixfd, buf_p, cont_size));
        VRFY((rc != 0), "POSIXWRITE");

        /* Advance location in buffer */
        buf_p += cont_size;

    }

done:
    return ret_code;
}

/*
 * Function:        do_read
 * Purpose:         Read the required amount of data to the file.
 * Return:          SUCCESS or FAIL
 * Programmer:      Christian Chilan, April, 2008
 * Modifications:
 */
static herr_t
do_read(results *res, file_descr *fd, parameters *parms, void *buffer)
{
    char        *buffer2 = NULL;       /* Buffer for data verification */
    int         ret_code = SUCCESS;
    char        dname[64];
    int         i;
    size_t      u;
    /* HDF5 variables */
    herr_t      hrc;                    /*HDF5 return code              */
    hsize_t     h5dims[MAX_DIMS];       /*dataset dim sizes             */
    hsize_t     h5block[MAX_DIMS];      /*dataspace selection           */
    hsize_t     h5stride[MAX_DIMS];     /*selection stride              */
    hsize_t     h5start[MAX_DIMS];      /*selection start               */
    int         rank;

    /* Allocate data verification buffer */
    if(NULL == (buffer2 = (char *)malloc(linear_buf_size))) {
        HDfprintf(stderr, "malloc for data verification buffer size (%Zu) failed\n", linear_buf_size);
        GOTOERROR(FAIL);
    } /* end if */

    /* Prepare buffer for verifying data */
    for(u = 0; u < linear_buf_size; u++)
        buffer2[u] = (char)(u % 128);

    rank = parms->rank;
    for(i = 0; i < rank; i++)
        h5offset[i] = offset[i] = 0;

    /* I/O Access specific setup */
    switch (parms->io_type) {
    case POSIXIO:
            cont_dim = rank;

            for (i=rank-1; i>=0; i--) {
                if (parms->buf_size[i]==parms->dset_size[i])
                    cont_dim = i;
                else
                    break;
            }
            cont_size = (!cont_dim)? 1 : parms->buf_size[cont_dim-1];
            for (i=cont_dim; i<rank; i++)
                cont_size *= parms->buf_size[i];

        break;

    case HDF5: /* HDF5 setup */
        for (i=0; i < rank; i++){
            h5dims[i] = parms->dset_size[i];
            h5start[i] = 0;
            h5stride[i] = 1;
            h5block[i] = 1;
            h5count[i] = parms->buf_size[i];
        }

        h5dset_space_id = H5Screate_simple(rank, h5dims, NULL);
        VRFY((h5dset_space_id >= 0), "H5Screate_simple");

        hrc = H5Sselect_hyperslab(h5dset_space_id, H5S_SELECT_SET,
                    h5start, h5stride, h5count, h5block);
        VRFY((hrc >= 0), "H5Sselect_hyperslab");

        /* Create the memory dataspace that corresponds to the xfer buffer */
        h5mem_space_id = H5Screate_simple(rank, h5count, NULL);
        VRFY((h5mem_space_id >= 0), "H5Screate_simple");

        /* Create the dataset transfer property list */
        h5dxpl = H5Pcreate(H5P_DATASET_XFER);
        if (h5dxpl < 0) {
            HDfprintf(stderr, "HDF5 Property List Create failed\n");
            GOTOERROR(FAIL);
        }
        break;

    default:
        /* unknown request */
        HDfprintf(stderr, "Unknown IO type request (%d)\n", (int)parms->io_type);
        GOTOERROR(FAIL);
        break;
    } /* end switch */


    /* create dataset */
    switch (parms->io_type) {
        case POSIXIO:
        break;

        case HDF5:
        HDsprintf(dname, "Dataset_%ld", (long)parms->num_bytes);
        h5ds_id = H5Dopen2(fd->h5fd, dname, H5P_DEFAULT);
        if (h5ds_id < 0) {
            HDfprintf(stderr, "HDF5 Dataset open failed\n");
            GOTOERROR(FAIL);
        }
        break;

        default:
        /* unknown request */
        HDfprintf(stderr, "Unknown IO type request (%d)\n", (int)parms->io_type);
        GOTOERROR(FAIL);
        break;
    } /* end switch */

    /* Start "raw data" read timer */
    set_time(res->timers, HDF5_RAW_READ_FIXED_DIMS, TSTART);
    hrc = dset_read(rank-1, fd, parms, buffer, buffer2);

    if (hrc < 0) {
        HDfprintf(stderr, "Error in dataset read\n");
        GOTOERROR(FAIL);
    }

    /* Stop "raw data" read timer */
    set_time(res->timers, HDF5_RAW_READ_FIXED_DIMS, TSTOP);

    /* Calculate read time */

    /* Close dataset. Only HDF5 needs to do an explicit close. */
    if (parms->io_type == HDF5) {
        hrc = H5Dclose(h5ds_id);

        if (hrc < 0) {
        HDfprintf(stderr, "HDF5 Dataset Close failed\n");
        GOTOERROR(FAIL);
        }

        h5ds_id = -1;
    } /* end if */

done:

    /* release HDF5 objects */
    if (h5dset_space_id != -1) {
    hrc = H5Sclose(h5dset_space_id);
    if (hrc < 0){
        HDfprintf(stderr, "HDF5 Dataset Space Close failed\n");
        ret_code = FAIL;
    } else {
        h5dset_space_id = -1;
    }
    }

    if (h5mem_space_id != -1) {
    hrc = H5Sclose(h5mem_space_id);
    if (hrc < 0) {
        HDfprintf(stderr, "HDF5 Memory Space Close failed\n");
        ret_code = FAIL;
    } else {
        h5mem_space_id = -1;
    }
    }

    if (h5dxpl != -1) {
    hrc = H5Pclose(h5dxpl);
    if (hrc < 0) {
        HDfprintf(stderr, "HDF5 Dataset Transfer Property List Close failed\n");
        ret_code = FAIL;
    } else {
        h5dxpl = -1;
    }
    }

    /* release generic resources */
    if(buffer2)
        free(buffer2);

    return ret_code;
}

/*
 * Function:        dset_read
 * Purpose:         Read buffer into the dataset.
 * Return:          SUCCESS or FAIL
 * Programmer:      Christian Chilan, April, 2008
 * Modifications:
 */

static herr_t
dset_read(int local_dim, file_descr *fd, parameters *parms, void *buffer,
    const char *buffer2)
{
    int cur_dim = order[local_dim]-1;
    hsize_t i;
    int j;
    herr_t hrc;
    int         ret_code = SUCCESS;

    /* iterate on the current dimension */
    for (i=0; i < parms->dset_size[cur_dim]; i += parms->buf_size[cur_dim]){

        h5offset[cur_dim] = (hssize_t)i;
        offset[cur_dim] = (HDoff_t)i;

        /* if traverse in order array is incomplete, recurse */
        if (local_dim > 0){

            ret_code = dset_read(local_dim-1, fd, parms, buffer, buffer2);

        /* otherwise, write buffer into dataset */
        }else{

            switch (parms->io_type) {

            case POSIXIO:
                for (j=0; j<parms->rank; j++) {
                   buf_offset[j] = 0;
                }
                buf_p = (unsigned char*)buffer;
                posix_buffer_read(0, fd, parms, buffer);
                break;

            case HDF5:
                hrc = H5Soffset_simple(h5dset_space_id, h5offset);
                VRFY((hrc >= 0), "H5Soffset_simple");
                /* Read the buffer out */
                hrc = H5Dread(h5ds_id, ELMT_H5_TYPE, h5mem_space_id,
                    h5dset_space_id, h5dxpl, buffer);
                VRFY((hrc >= 0), "H5Dread");
                break;

            default:
                /* unknown request */
                HDfprintf(stderr, "Unknown IO type request (%d)\n", (int)parms->io_type);
                HDassert(0 && "Unknown IO type");
                break;
            } /* switch (parms->io_type) */
        }
    }
done:
    return ret_code;
}

/*
 * Function:        posix_buffer_read
 * Purpose:         Read buffer into the POSIX file considering contiguity.
 * Return:          SUCCESS or FAIL
 * Programmer:      Christian Chilan, April, 2008
 * Modifications:
 */

static herr_t
posix_buffer_read(int local_dim, file_descr *fd, parameters *parms, void *buffer)
{
    int         ret_code = SUCCESS;

    /* if local dimension is not contiguous, recurse */
    if (local_dim < parms->rank-1 && local_dim != cont_dim) {
        size_t u;

        for(u = 0; u < parms->buf_size[local_dim]; u++) {
            buf_offset[local_dim] = u;
            ret_code = posix_buffer_read(local_dim+1, fd, parms, buffer);
            if (local_dim+1==cont_dim)
                break;
        }
    /* otherwise, perform contiguous POSIX access */
    } else {
        HDoff_t d_offset;
        HDoff_t linear_dset_offset = 0;
        int i, j, rc;

        buf_offset[local_dim] = 0;
        /* determine offset in buffer */
        for (i=0; i<parms->rank; i++){
            d_offset=1;

            for (j=i+1; j<parms->rank; j++)
                d_offset *= (HDoff_t)parms->dset_size[j];

            linear_dset_offset += (offset[i] + (HDoff_t)buf_offset[i]) * d_offset;
        }

        /* only care if seek returns error */
        rc = POSIXSEEK(fd->posixfd, linear_dset_offset) < 0 ? -1 : 0;
        VRFY((rc==0), "POSIXSEEK");
        /* check if all bytes are read */
        rc = ((ssize_t)cont_size ==
             POSIXREAD(fd->posixfd, buf_p, cont_size));
        VRFY((rc != 0), "POSIXREAD");

        /* Advance location in buffer */
        buf_p += cont_size;

    }
done:
    return ret_code;
}


/*
 * Function:    do_fopen
 * Purpose:     Open the specified file.
 * Return:      SUCCESS or FAIL
 * Programmer:  Albert Cheng, Bill Wendling, 2001/12/13
 * Modifications: Support for file drivers, Christian Chilan, April, 2008
 */
    static herr_t
do_fopen(parameters *param, char *fname, file_descr *fd /*out*/, int flags)
{
    int ret_code = SUCCESS;
    hid_t fcpl;

    switch (param->io_type) {
    case POSIXIO:
        if (flags & (SIO_CREATE | SIO_WRITE))
            fd->posixfd = POSIXCREATE(fname);
        else
            fd->posixfd = POSIXOPEN(fname, O_RDONLY);

        if (fd->posixfd < 0 ) {
            HDfprintf(stderr, "POSIX File Open failed(%s)\n", fname);
            GOTOERROR(FAIL);
        }

        break;

    case HDF5:

        fapl = set_vfd(param);

        if (fapl < 0) {
            HDfprintf(stderr, "HDF5 Property List Create failed\n");
            GOTOERROR(FAIL);
        }

        fcpl = H5Pcreate(H5P_FILE_CREATE);
        if(param->page_size) {
            H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, 0, (hsize_t)1);
            H5Pset_file_space_page_size(fcpl, param->page_size);
            if(param->page_buffer_size)
                H5Pset_page_buffer_size(fapl, param->page_buffer_size, 0, 0);
        }

        /* create the parallel file */
        if (flags & (SIO_CREATE | SIO_WRITE)) {
            fd->h5fd = H5Fcreate(fname, H5F_ACC_TRUNC, fcpl, fapl);
        } else {
            fd->h5fd = H5Fopen(fname, H5F_ACC_RDONLY, fapl);
        }


        if (fd->h5fd < 0) {
            HDfprintf(stderr, "HDF5 File Create failed(%s)\n", fname);
            GOTOERROR(FAIL);
        }
        break;

    default:
        /* unknown request */
        HDfprintf(stderr, "Unknown IO type request (%d)\n", (int)param->io_type);
        GOTOERROR(FAIL);
        break;
    }

done:
    return ret_code;
}

/*
 * Function:    set_vfd
 * Purpose:     Sets file driver.
 * Return:      SUCCESS or FAIL
 * Programmer:  Christian Chilan, April, 2008
 * Modifications:
 */

hid_t
set_vfd(parameters *param)
{
    hid_t my_fapl = -1;
    vfdtype  vfd;

    vfd = param->vfd;

    if ((my_fapl=H5Pcreate(H5P_FILE_ACCESS))<0) return -1;

    if (vfd == sec2) {
        /* Unix read() and write() system calls */
        if (H5Pset_fapl_sec2(my_fapl)<0) return -1;
    } else if (vfd == stdio) {
        /* Standard C fread() and fwrite() system calls */
        if (H5Pset_fapl_stdio(my_fapl)<0) return -1;
    } else if (vfd == core) {
        /* In-core temporary file with 1MB increment */
        if (H5Pset_fapl_core(my_fapl, (size_t)1024*1024, TRUE)<0) return -1;
    } else if (vfd == split) {
        /* Split meta data and raw data each using default driver */
        if (H5Pset_fapl_split(my_fapl,
                              "-m.h5", H5P_DEFAULT,
                              "-r.h5", H5P_DEFAULT)<0)
            return -1;
    } else if (vfd == multi) {
        /* Multi-file driver, general case of the split driver */
        H5FD_mem_t memb_map[H5FD_MEM_NTYPES];
        hid_t memb_fapl[H5FD_MEM_NTYPES];
        const char *memb_name[H5FD_MEM_NTYPES];
        char sv[H5FD_MEM_NTYPES][1024];
        haddr_t memb_addr[H5FD_MEM_NTYPES];
        H5FD_mem_t      mt;

        HDmemset(memb_map, 0, sizeof memb_map);
        HDmemset(memb_fapl, 0, sizeof memb_fapl);
        HDmemset(memb_name, 0, sizeof memb_name);
        HDmemset(memb_addr, 0, sizeof memb_addr);

        HDassert(HDstrlen(multi_letters)==H5FD_MEM_NTYPES);
        for (mt=H5FD_MEM_DEFAULT; mt<H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t,mt)) {
            memb_fapl[mt] = H5P_DEFAULT;
            HDsprintf(sv[mt], "%%s-%c.h5", multi_letters[mt]);
            memb_name[mt] = sv[mt];
            memb_addr[mt] = (haddr_t)MAX(mt - 1,0) * (HADDR_MAX / 10);
        }

        if (H5Pset_fapl_multi(my_fapl, memb_map, memb_fapl, memb_name,
                              memb_addr, FALSE)<0) {
            return -1;
        }
    } else if (vfd == family) {
        hsize_t fam_size = 1*1024*1024; /*100 MB*/

        /* Family of files, each 1MB and using the default driver */
        /* if ((val=HDstrtok(NULL, " \t\n\r")))
            fam_size = (hsize_t)(HDstrtod(val, NULL) * 1024*1024); */
        if (H5Pset_fapl_family(my_fapl, fam_size, H5P_DEFAULT)<0)
            return -1;
    } else if (vfd == direct) {
#ifdef H5_HAVE_DIRECT
        /* Linux direct read() and write() system calls.  Set memory boundary, file block size,
         * and copy buffer size to the default values. */
        if (H5Pset_fapl_direct(my_fapl, 1024, 4096, 8*4096)<0) return -1;
#endif
    } else {
        /* Unknown driver */
        return -1;
    }

    return my_fapl;
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
    int  rc = 0;

    switch (iot) {
    case POSIXIO:
        rc = POSIXCLOSE(fd->posixfd);

        if (rc != 0){
        HDfprintf(stderr, "POSIX File Close failed\n");
        GOTOERROR(FAIL);
        }

        fd->posixfd = -1;
        break;

    case HDF5:
        hrc = H5Fclose(fd->h5fd);

        if (hrc < 0) {
        HDfprintf(stderr, "HDF5 File Close failed\n");
        GOTOERROR(FAIL);
        }

        fd->h5fd = -1;
        break;

    default:
        /* unknown request */
        HDfprintf(stderr, "Unknown IO type request (%d)\n", (int)iot);
        GOTOERROR(FAIL);
        break;
    }

done:
    return ret_code;
}


/*
 * Function:    do_cleanupfile
 * Purpose:     Cleanup temporary file unless HDF5_NOCLEANUP is set.
 * Return:      void
 * Programmer:  Albert Cheng 2001/12/12
 * Modifications: Support for file drivers. Christian Chilan, April, 2008
 */
/* Disable warning for "format not a string literal" here -QAK */
/*
 *      This pragma only needs to surround the snprintf() calls with
 *      'temp' in the code below, but early (4.4.7, at least) gcc only
 *      allows diagnostic pragmas to be toggled outside of functions.
 */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wformat-nonliteral"
static void
do_cleanupfile(iotype iot, char *filename)
{
    char        temp[2048];
    int         j;
    hid_t       driver;

    if (clean_file_g == -1)
    clean_file_g = (HDgetenv("HDF5_NOCLEANUP")==NULL) ? 1 : 0;

    if (clean_file_g){

    switch (iot) {
        case POSIXIO:
            HDremove(filename);
            break;

        case HDF5:
            driver = H5Pget_driver(fapl);

            if (driver == H5FD_FAMILY) {
                for (j = 0; /*void*/; j++) {
                    HDsnprintf(temp, sizeof temp, filename, j);

                    if (HDaccess(temp, F_OK) < 0)
                        break;

                    HDremove(temp);
                }
            } else if (driver == H5FD_CORE) {
                hbool_t backing;        /* Whether the core file has backing store */

                H5Pget_fapl_core(fapl,NULL,&backing);

                /* If the file was stored to disk with bacing store, remove it */
                if(backing)
                    HDremove(filename);

            } else if (driver == H5FD_MULTI) {
                H5FD_mem_t mt;
                assert(HDstrlen(multi_letters)==H5FD_MEM_NTYPES);

                for (mt = H5FD_MEM_DEFAULT; mt < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t,mt)) {
                    HDsnprintf(temp, sizeof temp, "%s-%c.h5",
                               filename, multi_letters[mt]);
                    HDremove(temp); /*don't care if it fails*/
                }
            } else {
                HDremove(filename);
            }
            H5Pclose(fapl);
            break;
                            
        default:
            /* unknown request */
            HDfprintf(stderr, "Unknown IO type request (%d)\n", (int)iot);
            HDassert(0 && "Unknown IO type");
            break;
        }
    }
}
#pragma GCC diagnostic pop

