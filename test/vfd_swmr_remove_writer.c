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

/*-------------------------------------------------------------------------
 *
 * Created:     vfd_swmr_remove_writer.c
 *              (copied and modified from swmr_remove_writer.c)
 *
 * Purpose:     Removes data from a randomly selected subset of the datasets
 *              in the VFD SWMR test file.
 *
 *              This program is intended to run concurrently with the
 *              vfd_swmr_remove_reader program.  It is also run AFTER a sequential
 *              (not concurrent!) invoking of vfd_swmr_writer so the writer
 *              can dump a bunch of data into the datasets.  Otherwise,
 *              there wouldn't be much to shrink :)
 *
 *-------------------------------------------------------------------------
 */

/***********/
/* Headers */
/***********/

#include "h5test.h"
#include "vfd_swmr_common.h"
#include "swmr_common.h"

#ifndef H5_HAVE_WIN32_API

/****************/
/* Local Macros */
/****************/

/* The maximum number of records to remove in one step */
#define MAX_REMOVE_SIZE 10

/********************/
/* Local Prototypes */
/********************/

static hid_t open_skeleton(const char *filename, unsigned verbose, unsigned old);
static int   remove_records(hid_t fid, unsigned verbose, unsigned long nshrinks, unsigned long flush_count);
static void  usage(void);

/*-------------------------------------------------------------------------
 * Function:    open_skeleton
 *
 * Purpose:     Opens the SWMR HDF5 file and datasets.
 *
 * Parameters:  const char *filename
 *              The filename of the SWMR HDF5 file to open
 *
 *              unsigned verbose
 *              Whether or not to emit verbose console messages
 *
 * Return:      Success:    The file ID of the opened SWMR file
 *                          The dataset IDs are stored in a global array
 *
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static hid_t
open_skeleton(const char *filename, unsigned verbose, unsigned old H5_ATTR_UNUSED)
{
    hid_t                  fid  = -1;     /* File ID for new HDF5 file */
    hid_t                  fapl = -1;     /* File access property list */
    hid_t                  sid  = -1;     /* Dataspace ID */
    hsize_t                dim[2];        /* Dataspace dimensions */
    unsigned               u, v;          /* Local index variable */
    H5F_vfd_swmr_config_t *config = NULL; /* Configuration for VFD SWMR */

    HDassert(filename);

    /* Allocate memory for the configuration structure */
    if ((config = (H5F_vfd_swmr_config_t *)HDcalloc(1, sizeof(H5F_vfd_swmr_config_t))) == NULL)
        goto error;

    /* config, tick_len, max_lag, writer, flush_raw_data, md_pages_reserved, md_file_path */
    init_vfd_swmr_config(config, 4, 5, TRUE, FALSE, 128, "./rw-shadow");

    /* use_latest_format, use_vfd_swmr, only_meta_page, config */
    if ((fapl = vfd_swmr_create_fapl(TRUE, TRUE, FALSE, config)) < 0)
        goto error;

    /* Open the file */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        goto error;

    /* Close file access property list */
    if (H5Pclose(fapl) < 0)
        goto error;

    if (config)
        HDfree(config);

    /* Emit informational message */
    if (verbose)
        HDfprintf(stderr, "WRITER: Opening datasets\n");

    /* Open the datasets */
    for (u = 0; u < NLEVELS; u++)
        for (v = 0; v < symbol_count[u]; v++) {
            if ((symbol_info[u][v].dsid = H5Dopen2(fid, symbol_info[u][v].name, H5P_DEFAULT)) < 0)
                goto error;
            if ((sid = H5Dget_space(symbol_info[u][v].dsid)) < 0)
                goto error;
            if (2 != H5Sget_simple_extent_ndims(sid))
                goto error;
            if (H5Sget_simple_extent_dims(sid, dim, NULL) < 0)
                goto error;
            symbol_info[u][v].nrecords = dim[1];

            if (H5Sclose(sid) < 0)
                goto error;
        } /* end for */

    return fid;

error:
    if (config)
        HDfree(config);

    H5E_BEGIN_TRY
    {
        for (u = 0; u < NLEVELS; u++)
            for (v = 0; v < symbol_count[u]; v++)
                H5Dclose(symbol_info[u][v].dsid);
        H5Sclose(sid);
        H5Pclose(fapl);
        H5Fclose(fid);
    }
    H5E_END_TRY;

    return -1;

} /* open_skeleton() */

/*-------------------------------------------------------------------------
 * Function:    remove_records
 *
 * Purpose:     Removes a specified number of records from random datasets in
 *              the SWMR test file.
 *
 * Parameters:  hid_t fid
 *              The file ID of the SWMR HDF5 file
 *
 *              unsigned verbose
 *              Whether or not to emit verbose console messages
 *
 *              unsigned long nshrinks
 *              # of records to remove from the datasets
 *
 *              unsigned long flush_count
 *              # of records to write before flushing the file to disk
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static int
remove_records(hid_t fid, unsigned verbose, unsigned long nshrinks, unsigned long flush_count)
{
    unsigned long shrink_to_flush; /* # of removals before flush */
    hsize_t       dim[2] = {1, 0}; /* Dataspace dimensions */
    unsigned long u, v;            /* Local index variables */

    HDassert(fid >= 0);

    /* Remove records from random datasets, according to frequency distribution */
    shrink_to_flush = flush_count;
    for (u = 0; u < nshrinks; u++) {
        symbol_info_t *symbol;      /* Symbol to remove record from */
        hsize_t        remove_size; /* Size to reduce dataset dimension by */

        /* Get a random dataset, according to the symbol distribution */
        symbol = choose_dataset(NULL, NULL);

        /* Shrink the dataset's dataspace */
        remove_size = (hsize_t)HDrandom() % MAX_REMOVE_SIZE + 1;
        if (remove_size > symbol->nrecords)
            symbol->nrecords = 0;
        else
            symbol->nrecords -= remove_size;
        dim[1] = symbol->nrecords;
        if (H5Dset_extent(symbol->dsid, dim) < 0)
            goto error;

        /* Check for flushing file */
        if (flush_count > 0) {
            /* Decrement count of records to write before flushing */
            shrink_to_flush--;

            /* Check for counter being reached */
            if (0 == shrink_to_flush) {
                /* Reset flush counter */
                shrink_to_flush = flush_count;
            } /* end if */
        }     /* end if */
    }         /* end for */

    /* Emit informational message */
    if (verbose)
        HDfprintf(stderr, "WRITER: Closing datasets\n");

    /* Close the datasets */
    for (u = 0; u < NLEVELS; u++)
        for (v = 0; v < symbol_count[u]; v++)
            if (H5Dclose(symbol_info[u][v].dsid) < 0)
                goto error;

    return 0;

error:
    H5E_BEGIN_TRY
    {
        for (u = 0; u < NLEVELS; u++)
            for (v = 0; v < symbol_count[u]; v++)
                H5Dclose(symbol_info[u][v].dsid);
    }
    H5E_END_TRY;

    return -1;
} /* remove_records() */

static void
usage(void)
{
    HDprintf("\n");
    HDprintf("Usage error!\n");
    HDprintf("\n");
    HDprintf("Usage: swmr_remove_writer [-q] [-o] [-f <# of shrinks between flushing\n");
    HDprintf("    file contents>] [-r <random seed>] <# of shrinks>\n");
    HDprintf("\n");
    HDprintf("<# of shrinks between flushing file contents> should be 0 (for no\n");
    HDprintf("flushing) or between 1 and (<# of shrinks> - 1)\n");
    HDprintf("\n");
    HDprintf("Defaults to verbose (no '-q' given), latest format when opening file (no '-o' given),\n");
    HDprintf("flushing every 1000 shrinks ('-f 1000'), and will generate a random seed (no -r given).\n");
    HDprintf("\n");
    HDexit(1);
} /* usage() */

int
main(int argc, const char *argv[])
{
    sigset_t oldset;
    hid_t    fid;                /* File ID for file opened */
    long     nshrinks    = 0;    /* # of times to shrink the dataset */
    long     flush_count = 1000; /* # of records to write between flushing file */
    unsigned verbose     = 1;    /* Whether to emit some informational messages */
    unsigned old         = 0;    /* Whether to use non-latest-format when opening file */
    unsigned use_seed    = 0;    /* Set to 1 if a seed was set on the command line */
    unsigned random_seed = 0;    /* Random # seed */
    unsigned u;                  /* Local index variable */
    int      temp;

    block_signals(&oldset);

    /* Parse command line options */
    if (argc < 2)
        usage();
    if (argc > 1) {
        u = 1;
        while (u < (unsigned)argc) {
            if (argv[u][0] == '-') {
                switch (argv[u][1]) {
                    /* # of records to write between flushing file */
                    case 'f':
                        flush_count = HDatol(argv[u + 1]);
                        if (flush_count < 0)
                            usage();
                        u += 2;
                        break;

                    /* Be quiet */
                    case 'q':
                        verbose = 0;
                        u++;
                        break;

                    /* Random # seed */
                    case 'r':
                        use_seed    = 1;
                        temp        = HDatoi(argv[u + 1]);
                        random_seed = (unsigned)temp;
                        u += 2;
                        break;

                    /* Use non-latest-format when opening file */
                    case 'o':
                        old = 1;
                        u++;
                        break;

                    default:
                        usage();
                        break;
                } /* end switch */
            }     /* end if */
            else {
                /* Get the number of records to append */
                nshrinks = HDatol(argv[u]);
                if (nshrinks <= 0)
                    usage();

                u++;
            } /* end else */
        }     /* end while */
    }         /* end if */
    if (nshrinks <= 0)
        usage();
    if (flush_count >= nshrinks)
        usage();

    /* Emit informational message */
    if (verbose) {
        HDfprintf(stderr, "WRITER: Parameters:\n");
        HDfprintf(stderr, "\t# of shrinks between flushes = %ld\n", flush_count);
        HDfprintf(stderr, "\t# of shrinks = %ld\n", nshrinks);
    } /* end if */

    /* Set the random seed */
    if (0 == use_seed) {
        struct timeval t;
        HDgettimeofday(&t, NULL);
        random_seed = (unsigned)(t.tv_usec);
    } /* end if */
    HDsrandom(random_seed);
    /* ALWAYS emit the random seed for possible debugging */
    HDfprintf(stderr, "WRITER: Using writer random seed: %u\n", random_seed);

    /* Emit informational message */
    if (verbose)
        HDfprintf(stderr, "WRITER: Generating symbol names\n");

    /* Generate dataset names */
    if (generate_symbols() < 0)
        return -1;

    /* Emit informational message */
    if (verbose) {
        HDfprintf(stderr, "WRITER: Opening skeleton file: %s\n", VFD_SWMR_FILENAME);
    }

    /* Open file skeleton */
    if ((fid = open_skeleton(VFD_SWMR_FILENAME, verbose, old)) < 0) {
        HDfprintf(stderr, "WRITER: Error opening skeleton file!\n");
        HDexit(1);
    } /* end if */

    /* Send a message to indicate "H5Fopen" is complete--releasing the file lock */
    h5_send_message(VFD_SWMR_WRITER_MESSAGE, NULL, NULL);

    /* Emit informational message */
    if (verbose)
        HDfprintf(stderr, "WRITER: Removing records\n");

    /* Remove records from datasets */
    if (remove_records(fid, verbose, (unsigned long)nshrinks, (unsigned long)flush_count) < 0) {
        HDfprintf(stderr, "WRITER: Error removing records from datasets!\n");
        HDexit(1);
    } /* end if */

    /* Emit informational message */
    if (verbose)
        HDfprintf(stderr, "WRITER: Releasing symbols\n");

    /* Clean up the symbols */
    if (shutdown_symbols() < 0) {
        HDfprintf(stderr, "WRITER: Error releasing symbols!\n");
        HDexit(1);
    } /* end if */

    await_signal(fid);

    restore_signals(&oldset);

    /* Emit informational message */
    if (verbose)
        HDfprintf(stderr, "WRITER: Closing objects\n");

    /* Close objects opened */
    if (H5Fclose(fid) < 0) {
        HDfprintf(stderr, "WRITER: Error closing file!\n");
        HDexit(1);
    } /* end if */

    return 0;
} /* main() */

#else /* H5_HAVE_WIN32_API */

int
main(void)
{
    HDfprintf(stderr, "Non-POSIX platform. Skipping.\n");
    return EXIT_SUCCESS;
} /* end main() */

#endif /* H5_HAVE_WIN32_API */
