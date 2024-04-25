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
 * Created:     swmr_writer.c
 *
 * Purpose:     Writes data to a randomly selected subset of the datasets
 *              in the SWMR test file.
 *
 *              This program is intended to run concurrently with the
 *              swmr_reader program.
 *
 *-------------------------------------------------------------------------
 */

/***********/
/* Headers */
/***********/

#include "h5test.h"
#include "swmr_common.h"

/********************/
/* Local Prototypes */
/********************/

static hid_t open_skeleton(const char *filename, bool verbose, FILE *verbose_file, unsigned random_seed,
                           bool old);
static int   add_records(hid_t fid, bool verbose, FILE *verbose_file, unsigned long nrecords,
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
 *              bool verbose
 *              Whether or not to emit verbose console messages
 *
 *              FILE *verbose_file
 *              File handle for verbose output
 *
 *              unsigned random_seed
 *              Random seed for the file (used for verbose logging)
 *
 *              bool old
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
open_skeleton(const char *filename, bool verbose, FILE *verbose_file, unsigned random_seed, bool old)
{
    hid_t    fid;                 /* File ID for new HDF5 file */
    hid_t    fapl;                /* File access property list */
    unsigned u, v;                /* Local index variable */
    bool     use_log_vfd = false; /* Use the log VFD (set this manually) */

    assert(filename);

    /* Create file access property list */
    if ((fapl = h5_fileaccess()) < 0)
        return -1;

    if (!old) {
        /* Set to use the latest library format */
        if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
            return -1;
    }

    if (use_log_vfd) {
        char verbose_name[1024];

        snprintf(verbose_name, sizeof(verbose_name), "swmr_writer.log.%u", random_seed);

        H5Pset_fapl_log(fapl, verbose_name, H5FD_LOG_ALL, (size_t)(512 * 1024 * 1024));
    } /* end if */

    /* Open the file */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR | H5F_ACC_SWMR_WRITE, fapl)) < 0)
        return -1;

    /* Close file access property list */
    if (H5Pclose(fapl) < 0)
        return -1;

    /* Emit informational message */
    if (verbose)
        fprintf(verbose_file, "Opening datasets\n");

    /* Open the datasets */
    for (u = 0; u < NLEVELS; u++)
        for (v = 0; v < symbol_count[u]; v++) {
            if ((symbol_info[u][v].dsid = H5Dopen2(fid, symbol_info[u][v].name, H5P_DEFAULT)) < 0)
                return -1;
            symbol_info[u][v].nrecords = 0;
        } /* end for */

    return fid;
}

/*-------------------------------------------------------------------------
 * Function:    add_records
 *
 * Purpose:     Writes a specified number of records to random datasets in
 *              the SWMR test file.
 *
 * Parameters:  hid_t fid
 *              The file ID of the SWMR HDF5 file
 *
 *              bool verbose
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
add_records(hid_t fid, bool verbose, FILE *verbose_file, unsigned long nrecords, unsigned long flush_count)
{
    hid_t         tid;                                  /* Datatype ID for records */
    hid_t         mem_sid;                              /* Memory dataspace ID */
    hsize_t       start[2] = {0, 0}, count[2] = {1, 1}; /* Hyperslab selection values */
    hsize_t       dim[2] = {1, 0};                      /* Dataspace dimensions */
    symbol_t      record;                               /* The record to add to the dataset */
    unsigned long rec_to_flush;                         /* # of records left to write before flush */
    unsigned long u, v;                                 /* Local index variables */

    assert(fid >= 0);

    /* Reset the record */
    /* (record's 'info' field might need to change for each record written, also) */
    memset(&record, 0, sizeof(record));

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
        symbol = choose_dataset();

        /* Set the record's ID (equal to its position) */
        record.rec_id = symbol->nrecords;

        /* Get the coordinate to write */
        start[1] = symbol->nrecords;

        /* Cork the metadata cache, to prevent the object header from being
         * flushed before the data has been written */
        if (H5Odisable_mdc_flushes(symbol->dsid) < 0)
            return -1;

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

        /* Uncork the metadata cache */
        if (H5Oenable_mdc_flushes(symbol->dsid) < 0)
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
                /* Flush contents of file */
                if (H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
                    return -1;

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
        fprintf(verbose_file, "Closing datasets\n");

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
    printf("\n");
    printf("Usage error!\n");
    printf("\n");
    printf("Usage: swmr_writer [-q] [-o] [-f <# of records to write between flushing\n");
    printf("    file contents>] [-r <random seed>] <# of records>\n");
    printf("\n");
    printf("<# of records to write between flushing file contents> should be 0\n");
    printf("(for no flushing) or between 1 and (<# of records> - 1).\n");
    printf("\n");
    printf("<# of records> must be specified.\n");
    printf("\n");
    printf("Defaults to verbose (no '-q' given), latest format when opening file (no '-o' given),\n");
    printf("flushing every 10000 records ('-f 10000'), and will generate a random seed (no -r given).\n");
    printf("\n");
    exit(EXIT_FAILURE);
}

int
main(int argc, char *argv[])
{
    hid_t    fid;                  /* File ID for file opened */
    long     nrecords     = 0;     /* # of records to append */
    long     flush_count  = 10000; /* # of records to write between flushing file */
    bool     verbose      = true;  /* Whether to emit some informational messages */
    FILE    *verbose_file = NULL;  /* File handle for verbose output */
    bool     old          = false; /* Whether to use non-latest-format when opening file */
    bool     use_seed     = false; /* Set to true if a seed was set on the command line */
    unsigned random_seed  = 0;     /* Random # seed */
    unsigned u;                    /* Local index variable */
    int      temp;

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
                        flush_count = atol(argv[u + 1]);
                        if (flush_count < 0)
                            usage();
                        u += 2;
                        break;

                    /* Be quiet */
                    case 'q':
                        verbose = false;
                        u++;
                        break;

                    /* Random # seed */
                    case 'r':
                        use_seed    = true;
                        temp        = atoi(argv[u + 1]);
                        random_seed = (unsigned)temp;
                        u += 2;
                        break;

                    /* Use non-latest-format when opening file */
                    case 'o':
                        old = true;
                        u++;
                        break;

                    default:
                        usage();
                        break;
                } /* end switch */
            }     /* end if */
            else {
                /* Get the number of records to append */
                nrecords = atol(argv[u]);
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

        snprintf(verbose_name, sizeof(verbose_name), "swmr_writer.out.%u", random_seed);
        if (NULL == (verbose_file = fopen(verbose_name, "w"))) {
            fprintf(stderr, "Can't open verbose output file!\n");
            exit(EXIT_FAILURE);
        }
    } /* end if */

    /* Emit informational message */
    if (verbose) {
        fprintf(verbose_file, "Parameters:\n");
        fprintf(verbose_file, "\t# of records between flushes = %ld\n", flush_count);
        fprintf(verbose_file, "\t# of records to write = %ld\n", nrecords);
    } /* end if */

    /* ALWAYS emit the random seed for possible debugging */
    fprintf(stdout, "Using writer random seed: %u\n", random_seed);

    /* Emit informational message */
    if (verbose)
        fprintf(verbose_file, "Generating symbol names\n");

    /* Generate dataset names */
    if (generate_symbols() < 0)
        return -1;

    /* Emit informational message */
    if (verbose)
        fprintf(verbose_file, "Opening skeleton file: %s\n", FILENAME);

    /* Open file skeleton */
    if ((fid = open_skeleton(FILENAME, verbose, verbose_file, random_seed, old)) < 0) {
        fprintf(stderr, "Error opening skeleton file!\n");
        exit(EXIT_FAILURE);
    } /* end if */

    /* Send a message to indicate "H5Fopen" is complete--releasing the file lock */
    h5_send_message(WRITER_MESSAGE, NULL, NULL);

    /* Emit informational message */
    if (verbose)
        fprintf(verbose_file, "Adding records\n");

    /* Append records to datasets */
    if (add_records(fid, verbose, verbose_file, (unsigned long)nrecords, (unsigned long)flush_count) < 0) {
        fprintf(stderr, "Error appending records to datasets!\n");
        exit(EXIT_FAILURE);
    } /* end if */

    /* Emit informational message */
    if (verbose)
        fprintf(verbose_file, "Releasing symbols\n");

    /* Clean up the symbols */
    if (shutdown_symbols() < 0) {
        fprintf(stderr, "Error releasing symbols!\n");
        exit(EXIT_FAILURE);
    } /* end if */

    /* Emit informational message */
    if (verbose)
        fprintf(verbose_file, "Closing objects\n");

    /* Close objects opened */
    if (H5Fclose(fid) < 0) {
        fprintf(stderr, "Error closing file!\n");
        exit(EXIT_FAILURE);
    } /* end if */

    return 0;
}
