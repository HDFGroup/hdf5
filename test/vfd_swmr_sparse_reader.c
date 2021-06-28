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
 * Created:     vfd_swmr_sparse_reader.c
 *              (copied and modified from swmr_sparse_reader.c)
 *
 * Purpose:     Reads data from a randomly selected subset of the datasets
 *              in the VFD SWMR test file.  Unlike the regular reader, these
 *              datasets will be shrinking.
 *
 *              This program is intended to run concurrently with the
 *              vfd_swmr_sparse_writer program.
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

#define TIMEOUT 30

/*******************/
/* Local Variables */
/*******************/

static hid_t symbol_tid = H5I_INVALID_HID;

/********************/
/* Local Prototypes */
/********************/

static int  check_dataset(hid_t fid, unsigned verbose, const symbol_info_t *symbol, symbol_t *record,
                          hid_t rec_sid);
static int  read_records(const char *filename, unsigned verbose, unsigned long nrecords, unsigned poll_time,
                         unsigned reopen_count);
static void usage(void);

/*-------------------------------------------------------------------------
 * Function:    check_dataset
 *
 * Purpose:     For a given dataset, checks to make sure that the stated
 *              and actual sizes are the same.  If they are not, then
 *              we have an inconsistent dataset due to a SWMR error.
 *
 * Parameters:  hid_t fid
 *              The SWMR test file's ID.
 *
 *              unsigned verbose
 *              Whether verbose console output is desired.
 *
 *              const symbol_info_t *symbol
 *              The dataset from which to read (the ID is in the struct).
 *              Must be pre-allocated.
 *
 *              symbol_t *record
 *              Memory for the record.  Must be pre-allocated.
 *
 *              hid_t rec_sid
 *              The memory dataspace for access.  It's always the same so
 *              there is no need to re-create it every time this function
 *              is called.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static int
check_dataset(hid_t fid, unsigned verbose, const symbol_info_t *symbol, symbol_t *record, hid_t rec_sid)
{
    hid_t   dsid     = -1;     /* Dataset ID */
    hid_t   file_sid = -1;     /* Dataset's space ID */
    hsize_t start[2] = {0, 0}; /* Hyperslab selection values */
    hsize_t count[2] = {1, 1}; /* Hyperslab selection values */

    HDassert(fid >= 0);
    HDassert(symbol);
    HDassert(record);
    HDassert(rec_sid >= 0);

    /* Open dataset for symbol */
    if ((dsid = H5Dopen2(fid, symbol->name, H5P_DEFAULT)) < 0)
        goto error;

    /* Get the dataset's dataspace */
    if ((file_sid = H5Dget_space(dsid)) < 0)
        goto error;

    /* Choose the random record in the dataset (will be the same as chosen by
     * the writer) */
    start[1] = (hsize_t)HDrandom() % symbol->nrecords;
    if (H5Sselect_hyperslab(file_sid, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        goto error;

    /* Emit informational message */
    if (verbose)
        HDfprintf(stderr,
                  "READER: Symbol = '%s', nrecords = %" PRIuHSIZE ", name = %s, location = %" PRIuHSIZE
                  ", %" PRIuHSIZE "\n",
                  symbol->name, symbol->nrecords, symbol->name, start[0], start[1]);

    /* Read record from dataset */
    record->rec_id = UINT64_MAX;
    if (H5Dread(dsid, symbol_tid, rec_sid, file_sid, H5P_DEFAULT, record) < 0)
        goto error;

    /* Verify record value */
    if (record->rec_id != start[1]) {
        HDfprintf(stderr, "*** READER: ERROR ***\n");
        HDfprintf(stderr, "Incorrect record value!\n");
        HDfprintf(stderr,
                  "Symbol = '%s', location = %" PRIuHSIZE ", %" PRIuHSIZE ", record->rec_id = %" PRIu64 "\n",
                  symbol->name, start[0], start[1], record->rec_id);
        goto error;
    } /* end if */

    /* Close the dataset's dataspace */
    if (H5Sclose(file_sid) < 0)
        goto error;

    /* Close dataset for symbol */
    if (H5Dclose(dsid) < 0)
        goto error;

    return 0;

error:

    H5E_BEGIN_TRY
    {
        H5Sclose(file_sid);
        H5Dclose(dsid);
    }
    H5E_END_TRY;

    return -1;
} /* end check_dataset() */

/*-------------------------------------------------------------------------
 * Function:    read_records
 *
 * Purpose:     For a given dataset, checks to make sure that the stated
 *              and actual sizes are the same.  If they are not, then
 *              we have an inconsistent dataset due to a SWMR error.
 *
 * Parameters:  const char *filename
 *              The SWMR test file's name.
 *
 *              unsigned verbose
 *              Whether verbose console output is desired.
 *
 *              unsigned long nrecords
 *              The total number of records to read.
 *
 *              unsigned poll_time
 *              The amount of time to sleep (s).
 *
 *              unsigned reopen_count
 *
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static int
read_records(const char *filename, unsigned verbose, unsigned long nrecords, unsigned poll_time,
             unsigned reopen_count)
{
    hid_t                  fid = H5I_INVALID_HID;
    hid_t                  aid = H5I_INVALID_HID;
    time_t                 start_time; /* Starting time */
    hid_t                  mem_sid = H5I_INVALID_HID;
    symbol_t               record;                        /* The record to add to the dataset */
    unsigned               seed;                          /* Seed for random number generator */
    unsigned               iter_to_reopen = reopen_count; /* # of iterations until reopen */
    unsigned long          u;                             /* Local index variable */
    hid_t                  fapl   = H5I_INVALID_HID;
    H5F_vfd_swmr_config_t *config = NULL; /* Configuration for VFD SWMR */

    HDassert(filename);
    HDassert(poll_time != 0);

    /* Allocate memory for the configuration structure */
    if ((config = HDcalloc(1, sizeof(H5F_vfd_swmr_config_t))) == NULL)
        goto error;

    /* config, tick_len, max_lag, writer, flush_raw_data, md_pages_reserved, md_file_path */
    init_vfd_swmr_config(config, 4, 5, FALSE, FALSE, 128, "./rw-shadow");

    /* use_latest_format, use_vfd_swmr, only_meta_page, config */
    if ((fapl = vfd_swmr_create_fapl(FALSE, TRUE, FALSE, config)) < 0) {
        HDfprintf(stderr, "%s.%d: vfd_swmr_create_fapl failed\n", __func__, __LINE__);
        goto error;
    }

    if (H5Pset_fclose_degree(fapl, H5F_CLOSE_SEMI) < 0)
        goto error;

    /* Emit informational message */
    if (verbose)
        HDfprintf(stderr, "READER: Opening file: %s\n", filename);

    /* Open the file */
    /* Remove H5E_BEGIN_TRY/END_TRY to see the error stack if error */
    H5E_BEGIN_TRY
    {
        fid = H5Fopen(filename, H5F_ACC_RDONLY, fapl);
    }
    H5E_END_TRY;
    if (fid < 0) {
        HDfprintf(stderr, "READER: Error in opening the file: %s\n", filename);
        goto error;
    }
    else
        HDfprintf(stderr, "READER: SUCCESS in opening the file: %s\n", filename);

    /* Seed the random number generator with the attribute in the file */
    if ((aid = H5Aopen(fid, "seed", H5P_DEFAULT)) < 0)
        goto error;
    if (H5Aread(aid, H5T_NATIVE_UINT, &seed) < 0)
        goto error;
    if (H5Aclose(aid) < 0)
        goto error;
    HDsrandom(seed);

    /* Reset the record */
    /* (record's 'info' field might need to change for each record written, also) */
    HDmemset(&record, 0, sizeof(record));

    /* Create a dataspace for the record to read */
    if ((mem_sid = H5Screate(H5S_SCALAR)) < 0)
        goto error;

    /* Emit informational message */
    if (verbose)
        HDfprintf(stderr, "READER: Reading records\n");

    /* Read records */
    for (u = 0; u < nrecords; u++) {
        unsigned       level, offset;
        symbol_info_t *symbol = NULL; /* Symbol (dataset) */
        htri_t         attr_exists;   /* Whether the sequence number attribute exists */
        unsigned long  file_u;        /* Attribute sequence number (writer's "u") */

        /* Get a random dataset, according to the symbol distribution */
        symbol = choose_dataset(&level, &offset, verbose);

        /* Fill in "nrecords" field.  Note that this depends on the writer
         * using the same algorithm and "nrecords" */
        symbol->nrecords = nrecords / 5;

        /* Get the starting time */
        if ((start_time = HDtime(NULL)) == (time_t)-1) {
            HDfprintf(stderr, "READER: could not read time.\n");
            goto error;
        }

        /* Wait until we can read the dataset */
        for (;;) {
            if ((attr_exists = H5Aexists_by_name(fid, symbol->name, "seq", H5P_DEFAULT)) < 0)
                goto error;

            if (attr_exists) {
                if ((aid = H5Aopen_by_name(fid, symbol->name, "seq", H5P_DEFAULT, H5P_DEFAULT)) < 0)
                    goto error;
                if (H5Aread(aid, H5T_NATIVE_ULONG, &file_u) < 0)
                    goto error;
                if (H5Aclose(aid) < 0)
                    goto error;

                if (file_u >= u)
                    break;
            }

            if (HDtime(NULL) >= (time_t)(start_time + (time_t)TIMEOUT)) {
                HDfprintf(stderr, "READER: Reader timed at record %lu level %u offset %u", u, level, offset);
                if (attr_exists) {
                    HDfprintf(stderr, ", read sequence %lu\n", file_u);
                }
                else {
                    HDfprintf(stderr, ", read no sequence\n");
                    HDfprintf(stderr, ", read no sequence\n");
                }
                goto error;
            }

            HDsleep(poll_time);

            if (verbose)
                HDfprintf(stderr, "READER: Reopening file (do while loop): %s\n", filename);

            if (print_metadata_retries_info(fid) < 0)
                HDfprintf(stderr, "READER: Warning: could not obtain metadata retries info\n");

            if (H5Fclose(fid) < 0)
                goto error;

            H5E_BEGIN_TRY
            {
                fid = H5Fopen(filename, H5F_ACC_RDONLY, fapl);
            }
            H5E_END_TRY;
            if (fid < 0) {
                HDfprintf(stderr, "READER: Error in reopening the file (do while loop): %s\n", filename);
                goto error;
            }
            iter_to_reopen = reopen_count;
        }

        /* Emit informational message */
        if (verbose)
            HDfprintf(stderr, "READER: Checking dataset %lu\n", u);

        /* Check dataset */
        if (check_dataset(fid, verbose, symbol, &record, mem_sid) < 0)
            goto error;
        HDmemset(&record, 0, sizeof(record));

        /* Check for reopen */
        iter_to_reopen--;
        if (iter_to_reopen == 0) {
            /* Emit informational message */
            if (verbose)
                HDfprintf(stderr, "READER: Reopening file (iter_to_reopen): %s\n", filename);

            /* Retrieve and print the collection of metadata read retries */
            if (print_metadata_retries_info(fid) < 0)
                HDfprintf(stderr, "READER: Warning: could not obtain metadata retries info\n");

            /* Reopen the file */
            if (H5Fclose(fid) < 0)
                goto error;

            /* Remove H5E_BEGIN_TRY/END_TRY to see the error stack if error */
            //            H5E_BEGIN_TRY {
            fid = H5Fopen(filename, H5F_ACC_RDONLY, fapl);
            //           } H5E_END_TRY;
            if (fid < 0) {
                HDfprintf(stderr, "READER: Error in reopening the file (iter_to_reopen): %s\n", filename);
                goto error;
            }

            iter_to_reopen = reopen_count;
        } /* end if */
    }     /* end while */

    /* Retrieve and print the collection of metadata read retries */
    if (print_metadata_retries_info(fid) < 0)
        HDfprintf(stderr, "READER: Warning: could not obtain metadata retries info\n");

    /* Close file */
    if (H5Fclose(fid) < 0)
        goto error;

    /* Close the memory dataspace */
    if (H5Sclose(mem_sid) < 0)
        goto error;

    /* Close the file access property list */
    if (H5Pclose(fapl) < 0)
        goto error;

    if (config)
        HDfree(config);

    return 0;

error:
    if (config)
        HDfree(config);

    H5E_BEGIN_TRY
    {
        H5Aclose(aid);
        H5Sclose(mem_sid);
        H5Fclose(fid);
        H5Pclose(fapl);
    }
    H5E_END_TRY;

    return -1;
} /* end read_records() */

static void
usage(void)
{
    HDprintf("\n");
    HDprintf("Usage error!\n");
    HDprintf("\n");
    HDprintf("Usage: vfd_swmr_sparse_reader [-q] [-s <# of seconds to wait for writer>]\n");
    HDprintf("    [-n <# of reads between reopens>] <# of records>\n");
    HDprintf("\n");
    HDprintf("Defaults to verbose (no '-q' given), 1 second wait ('-s 1') and 1 read\n");
    HDprintf("between reopens ('-r 1')\n");
    HDprintf("\n");
    HDprintf("Note that the # of records *must* be the same as that supplied to\n");
    HDprintf("vfd_swmr_sparse_writer\n");
    HDprintf("\n");
    HDexit(1);
} /* end usage() */

int
main(int argc, const char *argv[])
{
    long     nrecords     = 0; /* # of records to read */
    int      poll_time    = 1; /* # of seconds to sleep when waiting for writer */
    int      reopen_count = 1; /* # of reads between reopens */
    unsigned verbose      = 1; /* Whether to emit some informational messages */
    unsigned u;                /* Local index variables */

    /* Parse command line options */
    if (argc < 2)
        usage();
    if (argc > 1) {
        u = 1;
        while (u < (unsigned)argc) {
            if (argv[u][0] == '-') {
                switch (argv[u][1]) {
                    /* # of reads between reopens */
                    case 'n':
                        reopen_count = HDatoi(argv[u + 1]);
                        if (reopen_count < 0)
                            usage();
                        u += 2;
                        break;

                    /* Be quiet */
                    case 'q':
                        verbose = 0;
                        u++;
                        break;

                    /* # of seconds between polling */
                    case 's':
                        poll_time = HDatoi(argv[u + 1]);
                        if (poll_time < 0)
                            usage();
                        u += 2;
                        break;

                    default:
                        usage();
                        break;
                } /* end switch */
            }     /* end if */
            else {
                /* Get the number of records to read */
                nrecords = HDatol(argv[u]);
                if (nrecords <= 0)
                    usage();

                u++;
            } /* end else */
        }     /* end while */
    }         /* end if */

    /* Emit informational message */
    if (verbose) {
        HDfprintf(stderr, "READER: Parameters:\n");
        HDfprintf(stderr, "\t# of seconds between polling = %d\n", poll_time);
        HDfprintf(stderr, "\t# of reads between reopens = %d\n", reopen_count);
        HDfprintf(stderr, "\t# of records to read = %ld\n", nrecords);
    } /* end if */

    /* Emit informational message */
    if (verbose)
        HDfprintf(stderr, "READER: Generating symbol names\n");

    /* Generate dataset names */
    if (generate_symbols() < 0) {
        HDfprintf(stderr, "READER: Error generating symbol names!\n");
        HDexit(1);
    } /* end if */

    /* Create datatype for creating datasets */
    if ((symbol_tid = create_symbol_datatype()) < 0) {
        HDfprintf(stderr, "READER: Error creating symbol datatype!\n");
        HDexit(1);
    }

    /* Reading records from datasets */
    if (read_records(VFD_SWMR_FILENAME, verbose, (unsigned long)nrecords, (unsigned)poll_time,
                     (unsigned)reopen_count) < 0) {
        HDfprintf(stderr, "READER: Error reading records from datasets!\n");
        HDexit(1);
    } /* end if */

    /* Emit informational message */
    if (verbose)
        HDfprintf(stderr, "READER: Releasing symbols\n");

    /* Clean up the symbols */
    if (shutdown_symbols() < 0) {
        HDfprintf(stderr, "READER: Error releasing symbols!\n");
        HDexit(1);
    } /* end if */

    /* Emit informational message */
    if (verbose)
        HDfprintf(stderr, "READER: Closing objects\n");

    /* Close objects created */
    if (H5Tclose(symbol_tid) < 0) {
        HDfprintf(stderr, "READER: Error closing symbol datatype!\n");
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
