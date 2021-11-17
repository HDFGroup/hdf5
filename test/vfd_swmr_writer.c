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
 * Created:     vfd_swmr_writer.c
 *              (copied and modified from swmr_writer.c)
 *
 * Purpose:     Writes data to a randomly selected subset of the datasets
 *              in the VFD SWMR test file.
 *
 *              This program is intended to run concurrently with the
 *              vfd_swmr_reader program.
 *
 *-------------------------------------------------------------------------
 */

/***********/
/* Headers */
/***********/

#include "h5test.h"
#include "vfd_swmr_common.h"
#include "swmr_common.h"

/* Uses getopt */
#ifndef H5_HAVE_WIN32_API

/********************/
/* Local Prototypes */
/********************/

static hid_t open_skeleton(const char *filename, hbool_t verbose, FILE *verbose_file, unsigned random_seed,
                           hbool_t old);
static int   add_records(hid_t fid, hbool_t verbose, FILE *verbose_file, unsigned long nrecords,
                         unsigned long flush_count);
static void  usage(void);

/*-------------------------------------------------------------------------
 * Function:    open_skeleton
 *
 * Purpose:     Opens the SWMR HDF5 file and datasets.
 *
 * Parameters:  const char *filename
 *              The filename of the SWMR HDF5 file to open
 *
 *              hbool_t verbose
 *              Whether or not to emit verbose console messages
 *
 *              FILE *verbose_file
 *              File handle for verbose output
 *
 *              unsigned random_seed
 *              Random seed for the file (used for verbose logging)
 *
 *              hbool_t old
 *              Whether to write in "old" file format
 *
 * Return:      Success:    The file ID of the opened SWMR file
 *                          The dataset IDs are stored in a global array
 *
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static hid_t
open_skeleton(const char *filename, hbool_t verbose, FILE *verbose_file, unsigned random_seed,
              hbool_t old H5_ATTR_UNUSED)
{
    hid_t                  fid;                 /* File ID for new HDF5 file */
    hid_t                  fapl;                /* File access property list */
    unsigned               u, v;                /* Local index variable */
    hbool_t                use_log_vfd = FALSE; /* Use the log VFD (set this manually) */
    H5F_vfd_swmr_config_t *config      = NULL;  /* Configuration for VFD SWMR */

    HDassert(filename);

    /* Allocate memory for the configuration structure */
    if ((config = (H5F_vfd_swmr_config_t *)HDcalloc(1, sizeof(H5F_vfd_swmr_config_t))) == NULL)
        return -1;

     /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
      * flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
    init_vfd_swmr_config(config, 4, 5, TRUE, TRUE, FALSE, TRUE, 128, "rw-shadow", NULL);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    if ((fapl = vfd_swmr_create_fapl(TRUE, TRUE, FALSE, 4096, config)) < 0)
        return -1;

    if (use_log_vfd) {
        char verbose_name[1024];

        HDsnprintf(verbose_name, sizeof(verbose_name), "vfd_swmr_writer.log.%u", random_seed);

        H5Pset_fapl_log(fapl, verbose_name, H5FD_LOG_ALL, (size_t)(512 * 1024 * 1024));
    } /* end if */

    /* Open the file with VFD SWMR configured */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        return -1;

    /* Close file access property list */
    if (H5Pclose(fapl) < 0)
        return -1;

    if (config)
        HDfree(config);

    /* Emit informational message */
    if (verbose)
        HDfprintf(verbose_file, "WRITER: Opening datasets\n");

    /* Open the datasets */
    for (u = 0; u < NLEVELS; u++)
        for (v = 0; v < symbol_count[u]; v++) {
            if ((symbol_info[u][v].dsid = H5Dopen2(fid, symbol_info[u][v].name, H5P_DEFAULT)) < 0)
                return -1;
            symbol_info[u][v].nrecords = 0;
        } /* end for */

    return fid;
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
 *              hbool_t verbose
 *              Whether or not to emit verbose console messages
 *
 *              FILE *verbose_file
 *              File handle for verbose output
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
add_records(hid_t fid, hbool_t verbose, FILE *verbose_file, unsigned long nrecords, unsigned long flush_count)
{
    hid_t         tid;                                  /* Datatype ID for records */
    hid_t         mem_sid;                              /* Memory dataspace ID */
    hsize_t       start[2] = {0, 0}, count[2] = {1, 1}; /* Hyperslab selection values */
    hsize_t       dim[2] = {1, 0};                      /* Dataspace dimensions */
    symbol_t      record;                               /* The record to add to the dataset */
    unsigned long rec_to_flush;                         /* # of records left to write before flush */
    unsigned long u, v;                                 /* Local index variables */

    HDassert(fid >= 0);

    /* Reset the record */
    /* (record's 'info' field might need to change for each record written, also) */
    HDmemset(&record, 0, sizeof(record));

    /* Create a dataspace for the record to add */
    if ((mem_sid = H5Screate(H5S_SCALAR)) < 0)
        return -1;

    /* Create datatype for appending records */
    if ((tid = create_symbol_datatype()) < 0)
        return -1;

    /* Add records to random datasets, according to frequency distribution */
    rec_to_flush = flush_count;
    for (u = 0; u < nrecords; u++) {
        symbol_info_t *symbol;   /* Symbol to write record to */
        hid_t          file_sid; /* Dataset's space ID */

        /* Get a random dataset, according to the symbol distribution */
        symbol = choose_dataset(NULL, NULL, verbose);

        /* Set the record's ID (equal to its position) */
        record.rec_id = symbol->nrecords;

        /* Get the coordinate to write */
        start[1] = symbol->nrecords;

        /* Extend the dataset's dataspace to hold the new record */
        symbol->nrecords++;
        dim[1] = symbol->nrecords;
        if (H5Dset_extent(symbol->dsid, dim) < 0)
            return -1;

        /* Get the dataset's dataspace */
        if ((file_sid = H5Dget_space(symbol->dsid)) < 0)
            return -1;

        /* Choose the last record in the dataset */
        if (H5Sselect_hyperslab(file_sid, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
            return -1;

        /* Write record to the dataset */
        if (H5Dwrite(symbol->dsid, tid, mem_sid, file_sid, H5P_DEFAULT, &record) < 0)
            return -1;

        /* Close the dataset's dataspace */
        if (H5Sclose(file_sid) < 0)
            return -1;

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
    }         /* end for */

    /* Close the memory dataspace */
    if (H5Sclose(mem_sid) < 0)
        return -1;

    /* Close the datatype */
    if (H5Tclose(tid) < 0)
        return -1;

    /* Emit informational message */
    if (verbose)
        HDfprintf(verbose_file, "WRITER: Closing datasets\n");

    /* Close the datasets */
    for (u = 0; u < NLEVELS; u++)
        for (v = 0; v < symbol_count[u]; v++)
            if (H5Dclose(symbol_info[u][v].dsid) < 0)
                return -1;

    return 0;
}

static void
usage(void)
{
    HDprintf("\n");
    HDprintf("Usage error!\n");
    HDprintf("\n");
    HDprintf("Usage: vfd_swmr_writer [-q] [-o] [-f <# of records to write between flushing\n");
    HDprintf("    file contents>] [-r <random seed>] <# of records>\n");
    HDprintf("\n");
    HDprintf("<# of records to write between flushing file contents> should be 0\n");
    HDprintf("(for no flushing) or between 1 and (<# of records> - 1).\n");
    HDprintf("\n");
    HDprintf("<# of records> must be specified.\n");
    HDprintf("\n");
    HDprintf("Defaults to verbose (no '-q' given), latest format when opening file (no '-o' given),\n");
    HDprintf("flushing every 10000 records ('-f 10000'), and will generate a random seed (no -r given).\n");
    HDprintf("\n");
    HDexit(1);
}

int
main(int argc, char *const *argv)
{
    sigset_t oldset;
    hid_t    fid;                     /* File ID for file opened */
    long     nrecords        = 0;     /* # of records to append */
    long     flush_count     = 10000; /* # of records to write between flushing file */
    hbool_t  verbose         = TRUE;  /* Whether to emit some informational messages */
    FILE *   verbose_file    = NULL;  /* File handle for verbose output */
    hbool_t  old             = FALSE; /* Whether to use non-latest-format when opening file */
    hbool_t  use_seed        = FALSE; /* Set to TRUE if a seed was set on the command line */
    hbool_t  wait_for_signal = TRUE;
    unsigned random_seed     = 0; /* Random # seed */
    int      ch, temp;

    block_signals(&oldset);

    while ((ch = getopt(argc, argv, "Wf:qr:o")) != -1) {
        switch (ch) {
            /* # of records to write between flushing file */
            case 'f':
                flush_count = HDatol(optarg);
                if (flush_count < 0)
                    usage();
                break;

            /* Be quiet */
            case 'q':
                verbose = FALSE;
                break;

            /* Random # seed */
            case 'r':
                use_seed    = TRUE;
                temp        = HDatoi(optarg);
                random_seed = (unsigned)temp;
                break;

            case 'W':
                wait_for_signal = FALSE;
                break;

            /* Use non-latest-format when opening file */
            case 'o':
                old = TRUE;
                break;

            default:
                usage();
                break;
        }
    }
    argv += optind;
    argc -= optind;
    /* Parse command line options */
    if (argc < 1)
        usage();
    /* Get the number of records to append */
    nrecords = HDatol(argv[0]);
    if (nrecords <= 0 || flush_count >= nrecords)
        usage();

    /* Set the random seed */
    if (!use_seed) {
        struct timeval t;

        HDgettimeofday(&t, NULL);
        random_seed = (unsigned)(t.tv_usec);
    } /* end if */
    HDsrandom(random_seed);

    /* Open output file */
    if (verbose) {
        char verbose_name[1024];

        HDsnprintf(verbose_name, sizeof(verbose_name), "vfd_swmr_writer.out.%u", random_seed);
        if (NULL == (verbose_file = HDfopen(verbose_name, "w"))) {
            HDfprintf(stderr, "WRITER: Can't open verbose output file!\n");
            HDexit(1);
        }
    } /* end if */

    /* Emit informational message */
    if (verbose) {
        HDfprintf(verbose_file, "WRITER: Parameters:\n");
        HDfprintf(verbose_file, "\t# of records between flushes = %ld\n", flush_count);
        HDfprintf(verbose_file, "\t# of records to write = %ld\n", nrecords);
    } /* end if */

    /* ALWAYS emit the random seed for possible debugging */
    HDfprintf(stdout, "Using writer random seed: %u\n", random_seed);

    /* Emit informational message */
    if (verbose)
        HDfprintf(verbose_file, "WRITER: Generating symbol names\n");

    /* Generate dataset names */
    if (generate_symbols() < 0)
        return -1;

    /* Emit informational message */
    if (verbose) {
        HDfprintf(verbose_file, "WRITER: Opening skeleton file: %s\n", VFD_SWMR_FILENAME);
    }

    /* Open file skeleton */
    if ((fid = open_skeleton(VFD_SWMR_FILENAME, verbose, verbose_file, random_seed, old)) < 0) {
        HDfprintf(stderr, "WRITER: Error opening skeleton file!\n");
        HDexit(1);
    } /* end if */

    /* Send a message to indicate "H5Fopen" is complete--releasing the file lock */
    h5_send_message(VFD_SWMR_WRITER_MESSAGE, NULL, NULL);

    /* Emit informational message */
    if (verbose)
        HDfprintf(verbose_file, "WRITER: Adding records\n");

    /* Append records to datasets */
    if (add_records(fid, verbose, verbose_file, (unsigned long)nrecords, (unsigned long)flush_count) < 0) {
        HDfprintf(stderr, "WRITER: Error appending records to datasets!\n");
        HDexit(1);
    } /* end if */

    /* Emit informational message */
    if (verbose)
        HDfprintf(verbose_file, "WRITER: Releasing symbols\n");

    /* Clean up the symbols */
    if (shutdown_symbols() < 0) {
        HDfprintf(stderr, "WRITER: Error releasing symbols!\n");
        HDexit(1);
    } /* end if */

    if (wait_for_signal)
        await_signal(fid);

    restore_signals(&oldset);

    /* Emit informational message */
    if (verbose)
        HDfprintf(verbose_file, "WRITER: Closing objects/file\n");

    /* Close objects opened */
    if (H5Fclose(fid) < 0) {
        HDfprintf(stderr, "WRITER: Error closing file!\n");
        HDexit(1);
    } /* end if */

    return 0;
}

#else /* H5_HAVE_WIN32_API */

int
main(void)
{
    HDfprintf(stderr, "Non-POSIX platform. Skipping.\n");
    return EXIT_SUCCESS;
} /* end main() */

#endif /* H5_HAVE_WIN32_API */
