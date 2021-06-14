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
 * Created:     vfd_swmr_sparse_writer.c
 *              (copied and modified from swmr_sparse_writer.c)
 *
 * Purpose:     Writes data to a randomly selected subset of the datasets
 *              in the VFD_SWMR test file.
 *
 *              This program is intended to run concurrently with the
 *              vfd_swmr_sparse_reader program.
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

#include <err.h>

/****************/
/* Local Macros */
/****************/

#ifdef OUT
#define BUSY_WAIT 100000
#endif /* OUT */

/********************/
/* Local Prototypes */
/********************/

static hid_t open_skeleton(const char *filename, unsigned verbose);
static int   add_records(hid_t fid, unsigned verbose, unsigned long nrecords, unsigned long flush_count);
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
open_skeleton(const char *filename, unsigned verbose)
{
    hid_t                  fid  = -1;     /* File ID for new HDF5 file */
    hid_t                  fapl = -1;     /* File access property list */
    hid_t                  aid  = -1;     /* Attribute ID */
    unsigned               seed;          /* Seed for random number generator */
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

    /* Seed the random number generator with the attribute in the file */
    if ((aid = H5Aopen(fid, "seed", H5P_DEFAULT)) < 0)
        goto error;
    if (H5Aread(aid, H5T_NATIVE_UINT, &seed) < 0)
        goto error;
    if (H5Aclose(aid) < 0)
        goto error;
    HDsrandom(seed);

    /* Open the datasets */
    for (u = 0; u < NLEVELS; u++)
        for (v = 0; v < symbol_count[u]; v++) {
            if ((symbol_info[u][v].dsid = H5Dopen2(fid, symbol_info[u][v].name, H5P_DEFAULT)) < 0)
                return (-1);
            symbol_info[u][v].nrecords = 0;
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
        H5Aclose(aid);
        H5Pclose(fapl);
        H5Fclose(fid);
    }
    H5E_END_TRY;

    return -1;

} /* open_skeleton() */

/*-------------------------------------------------------------------------
 * Function:    add_records
 *
 * Purpose:     Writes a specified number of records to random datasets in
 *              the SWMR test file.
 *
 * Parameters:  hid_t fid
 *              The file ID of the SWMR HDF5 file
 *
 *              unsigned verbose
 *              Whether or not to emit verbose console messages
 *
 *              unsigned long nrecords
 *              # of records to write to the datasets
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
add_records(hid_t fid, unsigned verbose, unsigned long nrecords, unsigned long flush_count)
{
    hid_t         tid      = -1;     /* Datatype ID for records */
    hid_t         mem_sid  = -1;     /* Memory dataspace ID */
    hid_t         file_sid = -1;     /* Dataset's space ID */
    hid_t         aid      = -1;     /* Attribute ID */
    hsize_t       start[2] = {0, 0}; /* Hyperslab selection values */
    hsize_t       count[2] = {1, 1}; /* Hyperslab selection values */
    symbol_t      record;            /* The record to add to the dataset */
    unsigned long rec_to_flush;      /* # of records left to write before flush */
#ifdef OUT
    volatile int dummy;            /* Dummy varialbe for busy sleep */
#endif                             /* OUT */
    hsize_t       dim[2] = {1, 0}; /* Dataspace dimensions */
    unsigned long u, v;            /* Local index variables */

    HDassert(fid >= 0);

    /* Reset the record */
    /* (record's 'info' field might need to change for each record written, also) */
    HDmemset(&record, 0, sizeof(record));

    /* Create a dataspace for the record to add */
    if ((mem_sid = H5Screate(H5S_SCALAR)) < 0)
        goto error;

    /* Create datatype for appending records */
    if ((tid = create_symbol_datatype()) < 0)
        goto error;

    /* Add records to random datasets, according to frequency distribution */
    rec_to_flush = flush_count;
    for (u = 0; u < nrecords; u++) {
        symbol_info_t *symbol; /* Symbol to write record to */

        /* Get a random dataset, according to the symbol distribution */
        symbol = choose_dataset(NULL, NULL);

        /* If this is the first time the dataset has been opened, extend it and
         * add the sequence attribute */
        if (symbol->nrecords == 0) {
            symbol->nrecords = nrecords / 5;
            dim[1]           = symbol->nrecords;

            if (H5Dset_extent(symbol->dsid, dim) < 0)
                goto error;

            if ((file_sid = H5Screate(H5S_SCALAR)) < 0)
                goto error;
            if ((aid = H5Acreate2(symbol->dsid, "seq", H5T_NATIVE_ULONG, file_sid, H5P_DEFAULT,
                                  H5P_DEFAULT)) < 0)
                goto error;
            if (H5Sclose(file_sid) < 0)
                goto error;
        }
        else if ((aid = H5Aopen(symbol->dsid, "seq", H5P_DEFAULT)) < 0)
            goto error;

        /* Get the coordinate to write */
        start[1] = (hsize_t)HDrandom() % symbol->nrecords;

        /* Set the record's ID (equal to its position) */
        record.rec_id = start[1];

        /* Get the dataset's dataspace */
        if ((file_sid = H5Dget_space(symbol->dsid)) < 0)
            goto error;

        /* Choose a random record in the dataset */
        if (H5Sselect_hyperslab(file_sid, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
            goto error;

        /* Write record to the dataset */
        if (H5Dwrite(symbol->dsid, tid, mem_sid, file_sid, H5P_DEFAULT, &record) < 0)
            goto error;

        /* Write the sequence number attribute.  Since we synchronize the random
         * number seed, the readers will always generate the same sequence of
         * randomly chosen datasets and offsets.  Therefore, and because of the
         * flush dependencies on the object header, the reader will be
         * guaranteed to see the written data if the sequence attribute is >=u.
         */
        if (H5Awrite(aid, H5T_NATIVE_ULONG, &u) < 0)
            goto error;

        /* Close the attribute */
        if (H5Aclose(aid) < 0)
            goto error;

        /* Close the dataset's dataspace */
        if (H5Sclose(file_sid) < 0)
            goto error;

        /* Check for flushing file */
        if (flush_count > 0) {
            /* Decrement count of records to write before flushing */
            rec_to_flush--;

            /* Check for counter being reached */
            if (0 == rec_to_flush) {
                /* Reset flush counter */
                rec_to_flush = flush_count;
            } /* end if */
        }     /* end if */

#ifdef OUT
        /* Busy wait, to let readers catch up */
        /* If this is removed, also remove the BUSY_WAIT symbol
         * at the top of the file.
         */
        dummy = 0;
        for (v = 0; v < BUSY_WAIT; v++)
            dummy++;
        if ((unsigned long)dummy != v)
            return -1;
#endif /* OUT */

    } /* end for */

    /* Close the memory dataspace */
    if (H5Sclose(mem_sid) < 0)
        goto error;

    /* Close the datatype */
    if (H5Tclose(tid) < 0)
        goto error;

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
        H5Sclose(mem_sid);
        H5Sclose(file_sid);
        H5Tclose(tid);
        H5Aclose(aid);

        for (u = 0; u < NLEVELS; u++)
            for (v = 0; v < symbol_count[u]; v++)
                H5Dclose(symbol_info[u][v].dsid);
    }
    H5E_END_TRY;

    return -1;
} /* add_records() */

static void
usage(void)
{
    HDprintf("\n");
    HDprintf("Usage error!\n");
    HDprintf("\n");
    HDprintf("Usage: vfd_swmr_sparse_writer [-q] [-f <# of records to write between\n");
    HDprintf("    flushing file contents>] <# of records>\n");
    HDprintf("\n");
    HDprintf("<# of records to write between flushing file contents> should be 0\n");
    HDprintf("(for no flushing) or between 1 and (<# of records> - 1)\n");
    HDprintf("\n");
    HDprintf("Defaults to verbose (no '-q' given) and flushing every 1000 records\n");
    HDprintf("('-f 1000')\n");
    HDprintf("\n");
    HDexit(1);
} /* usage() */

int
main(int argc, const char *argv[])
{
    sigset_t oldset;
    hid_t    fid;                /* File ID for file opened */
    long     nrecords    = 0;    /* # of records to append */
    long     flush_count = 1000; /* # of records to write between flushing file */
    unsigned verbose     = 1;    /* Whether to emit some informational messages */
    unsigned u;                  /* Local index variable */

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

                    default:
                        usage();
                        break;
                } /* end switch */
            }     /* end if */
            else {
                /* Get the number of records to append */
                nrecords = HDatol(argv[u]);
                if (nrecords <= 0)
                    usage();

                u++;
            } /* end else */
        }     /* end while */
    }         /* end if */
    if (nrecords <= 0)
        usage();
    if (flush_count >= nrecords)
        usage();

    /* Emit informational message */
    if (verbose) {
        HDfprintf(stderr, "WRITER: Parameters:\n");
        HDfprintf(stderr, "\t# of records between flushes = %ld\n", flush_count);
        HDfprintf(stderr, "\t# of records to write = %ld\n", nrecords);
    } /* end if */

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
    if ((fid = open_skeleton(VFD_SWMR_FILENAME, verbose)) < 0) {
        HDfprintf(stderr, "WRITER: Error opening skeleton file!\n");
        HDexit(1);
    } /* end if */

    /* Send a message to indicate "H5Fopen" is complete--releasing the file lock */
    h5_send_message(VFD_SWMR_WRITER_MESSAGE, NULL, NULL);

    /* Emit informational message */
    if (verbose)
        HDfprintf(stderr, "WRITER: Adding records\n");

    /* Append records to datasets */
    if (add_records(fid, verbose, (unsigned long)nrecords, (unsigned long)flush_count) < 0) {
        HDfprintf(stderr, "WRITER: Error appending records to datasets!\n");
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
        HDfprintf(stderr, "WRITER: Closing objects/file\n");

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
