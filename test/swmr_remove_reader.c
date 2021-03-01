/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
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
 * Created:     swmr_remove_reader.c
 *
 * Purpose:     Reads data from a randomly selected subset of the datasets
 *              in the SWMR test file.  Unlike the regular reader, these
 *              datasets will be shrinking.
 *
 *              This program is intended to run concurrently with the
 *              swmr_remove_writer program.
 *
 *-------------------------------------------------------------------------
 */

/***********/
/* Headers */
/***********/

#include "h5test.h"
#include "swmr_common.h"

/*******************/
/* Local Variables */
/*******************/

static hid_t symbol_tid = -1;

/********************/
/* Local Prototypes */
/********************/

static int  check_dataset(hid_t fid, unsigned verbose, const char *sym_name, symbol_t *record, hid_t rec_sid);
static int  read_records(const char *filename, unsigned verbose, unsigned long nseconds, unsigned poll_time,
                         unsigned ncommon, unsigned nrandom);
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
 *              const char *sym_name
 *              The name of the dataset from which to read.
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
check_dataset(hid_t fid, unsigned verbose, const char *sym_name, symbol_t *record, hid_t rec_sid)
{
    hid_t    dsid;                                 /* Dataset ID */
    hid_t    file_sid;                             /* Dataset's space ID */
    hssize_t snpoints;                             /* Number of elements in dataset */
    hsize_t  start[2] = {0, 0}, count[2] = {1, 1}; /* Hyperslab selection values */

    HDassert(fid >= 0);
    HDassert(sym_name);
    HDassert(record);
    HDassert(rec_sid >= 0);

    /* Open dataset for symbol */
    if ((dsid = H5Dopen2(fid, sym_name, H5P_DEFAULT)) < 0)
        return -1;

    /* Get the dataset's dataspace */
    if ((file_sid = H5Dget_space(dsid)) < 0)
        return -1;

    /* Get the number of elements (= records, for 1-D datasets) */
    if ((snpoints = H5Sget_simple_extent_npoints(file_sid)) < 0)
        return -1;

    /* Emit informational message */
    if (verbose)
        HDfprintf(stderr, "Symbol = '%s', # of records = %lld\n", sym_name, (long long)snpoints);

    /* Check if there are records for symbol */
    if (snpoints > 0) {
        /* Choose a random record in the dataset, choosing the last record half
         * the time */
        start[1] = (hsize_t)(HDrandom() % (snpoints * 2));
        if (start[1] > (hsize_t)(snpoints - 1))
            start[1] = (hsize_t)(snpoints - 1);
        if (H5Sselect_hyperslab(file_sid, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
            return -1;

            /* Read record from dataset */
#ifdef FILLVAL_WORKS
        /* When shrinking the dataset, we cannot guarantee that the buffer will
         * even be touched, unless there is a fill value.  Since fill values do
         * not work with SWMR currently (see note in swmr_generator.c), we
         * simply initialize rec_id to 0. */
        record->rec_id = (uint64_t)ULLONG_MAX - 1;
#else  /* FILLVAL_WORKS */
        record->rec_id = (uint64_t)0;
#endif /* FILLVAL_WORKS */
        if (H5Dread(dsid, symbol_tid, rec_sid, file_sid, H5P_DEFAULT, record) < 0)
            return -1;

        /* Verify record value - note that it may be the fill value, because the
         * chunk may be deleted before the object header has the updated
         * dimensions */
        if (record->rec_id != start[1] && record->rec_id != (uint64_t)0) {
            HDfprintf(stderr, "*** ERROR ***\n");
            HDfprintf(stderr, "Incorrect record value!\n");
            HDfprintf(stderr, "Symbol = '%s', # of records = %lld, record->rec_id = %llx\n", sym_name,
                      (long long)snpoints, (unsigned long long)record->rec_id);
            return -1;
        } /* end if */
    }     /* end if */

    /* Close the dataset's dataspace */
    if (H5Sclose(file_sid) < 0)
        return -1;

    /* Close dataset for symbol */
    if (H5Dclose(dsid) < 0)
        return -1;

    return 0;
} /* end check_dataset() */

/*-------------------------------------------------------------------------
 * Function:    read_records
 *
 * Purpose:     For a given dataset, checks to make sure that the stated
 *              and actual sizes are the same.  If they are not, then
 *              we have an inconsistent dataset due to a SWMR error.
 *
 *              The "common" datasets are a random selection from among
 *              the level 0 datasets.  The "random" datasets are a random
 *              selection from among all the file's datasets.  This scheme
 *              ensures that the level 0 datasets are interrogated vigorously.
 *
 * Parameters:  const char *filename
 *              The SWMR test file's name.
 *
 *              unsigned verbose
 *              Whether verbose console output is desired.
 *
 *              unsigned long nseconds
 *              The amount of time to read records (ns).
 *
 *              unsigned poll_time
 *              The amount of time to sleep (s).
 *
 *              unsigned ncommon
 *              The number of common/non-random datasets that will be opened.
 *
 *              unsigned nrandom
 *              The number of random datasets that will be opened.
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static int
read_records(const char *filename, unsigned verbose, unsigned long nseconds, unsigned poll_time,
             unsigned ncommon, unsigned nrandom)
{
    time_t          start_time;      /* Starting time */
    time_t          curr_time;       /* Current time */
    symbol_info_t **sym_com  = NULL; /* Pointers to array of common dataset IDs */
    symbol_info_t **sym_rand = NULL; /* Pointers to array of random dataset IDs */
    hid_t           mem_sid;         /* Memory dataspace ID */
    hid_t           fid;             /* SWMR test file ID */
    hid_t           fapl;            /* File access property list */
    symbol_t        record;          /* The record to add to the dataset */
    unsigned        v;               /* Local index variable */

    HDassert(filename);
    HDassert(nseconds != 0);
    HDassert(poll_time != 0);

    /* Reset the record */
    /* (record's 'info' field might need to change for each record written, also) */
    HDmemset(&record, 0, sizeof(record));

    /* Emit informational message */
    if (verbose)
        HDfprintf(stderr, "Choosing datasets\n");

    /* Allocate space for 'common' datasets, if any */
    if (ncommon > 0) {
        /* Allocate array to hold pointers to symbols for common datasets */
        if (NULL == (sym_com = (symbol_info_t **)HDmalloc(sizeof(symbol_info_t *) * ncommon)))
            return -1;

        /* Open the common datasets */
        for (v = 0; v < ncommon; v++) {
            unsigned offset; /* Offset of symbol to use */

            /* Determine the offset of the symbol, within level 0 symbols */
            /* (level 0 symbols are the most common symbols) */
            offset     = (unsigned)HDrandom() % symbol_count[0];
            sym_com[v] = &symbol_info[0][offset];

            /* Emit informational message */
            if (verbose)
                HDfprintf(stderr, "Common symbol #%u = '%s'\n", v, symbol_info[0][offset].name);
        } /* end for */
    }     /* end if */

    /* Allocate space for 'random' datasets, if any */
    if (nrandom > 0) {
        /* Allocate array to hold pointers to symbols for random datasets */
        if (NULL == (sym_rand = (symbol_info_t **)HDmalloc(sizeof(symbol_info_t *) * nrandom)))
            return -1;

        /* Determine the random datasets */
        for (v = 0; v < nrandom; v++) {
            symbol_info_t *sym; /* Symbol to use */

            /* Determine the symbol, within all symbols */
            if (NULL == (sym = choose_dataset()))
                return -1;
            sym_rand[v] = sym;

            /* Emit informational message */
            if (verbose)
                HDfprintf(stderr, "Random symbol #%u = '%s'\n", v, sym->name);
        } /* end for */
    }     /* end if */

    /* Create a dataspace for the record to read */
    if ((mem_sid = H5Screate(H5S_SCALAR)) < 0)
        return -1;

    /* Emit informational message */
    if (verbose)
        HDfprintf(stderr, "Reading records\n");

    /* Get the starting time */
    start_time = HDtime(NULL);
    curr_time  = start_time;

    /* Create file access property list */
    if ((fapl = h5_fileaccess()) < 0)
        return -1;

    /* Loop over reading records until [at least] the correct # of seconds have passed */
    while (curr_time < (time_t)(start_time + (time_t)nseconds)) {

        /* Emit informational message */
        if (verbose)
            HDfprintf(stderr, "Opening file: %s\n", filename);

        /* Open the file */
        if ((fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl)) < 0)
            return -1;

        /* Check 'common' datasets, if any */
        if (ncommon > 0) {
            /* Emit informational message */
            if (verbose)
                HDfprintf(stderr, "Checking common symbols\n");

            /* Iterate over common datasets */
            for (v = 0; v < ncommon; v++) {
                /* Check common dataset */
                if (check_dataset(fid, verbose, sym_com[v]->name, &record, mem_sid) < 0)
                    return -1;
                HDmemset(&record, 0, sizeof(record));
            } /* end for */
        }     /* end if */

        /* Check 'random' datasets, if any */
        if (nrandom > 0) {
            /* Emit informational message */
            if (verbose)
                HDfprintf(stderr, "Checking random symbols\n");

            /* Iterate over random datasets */
            for (v = 0; v < nrandom; v++) {
                /* Check random dataset */
                if (check_dataset(fid, verbose, sym_rand[v]->name, &record, mem_sid) < 0)
                    return -1;
                HDmemset(&record, 0, sizeof(record));
            } /* end for */
        }     /* end if */

        /* Emit informational message */
        if (verbose)
            HDfprintf(stderr, "Closing file\n");

        /* Close the file */
        if (H5Fclose(fid) < 0)
            return -1;

        /* Sleep for the appropriate # of seconds */
        HDsleep(poll_time);

        /* Retrieve the current time */
        curr_time = HDtime(NULL);
    } /* end while */

    /* Close the fapl */
    if (H5Pclose(fapl) < 0)
        return -1;

    /* Close the memory dataspace */
    if (H5Sclose(mem_sid) < 0)
        return -1;

    /* Emit informational message */
    if (verbose)
        HDfprintf(stderr, "Closing datasets\n");

    /* Close 'random' datasets, if any */
    if (nrandom > 0) {
        /* Release array holding dataset ID's for random datasets */
        HDfree(sym_rand);
    } /* end if */

    /* Close 'common' datasets, if any */
    if (ncommon > 0) {
        /* Release array holding dataset ID's for common datasets */
        HDfree(sym_com);
    } /* end if */

    return 0;
} /* end read_records() */

static void
usage(void)
{
    HDprintf("\n");
    HDprintf("Usage error!\n");
    HDprintf("\n");
    HDprintf("Usage: swmr_remove_reader [-q] [-s <# of seconds to sleep between\n");
    HDprintf("    polling>] [-h <# of common symbols to poll>] [-l <# of random symbols\n");
    HDprintf("    to poll>] [-r <random seed>] <# of seconds to test>\n");
    HDprintf("\n");
    HDprintf("Defaults to verbose (no '-q' given), 1 second between polling ('-s 1'),\n");
    HDprintf("5 common symbols to poll ('-h 5'), 10 random symbols to poll ('-l 10'),\n");
    HDprintf("and will generate a random seed (no -r given).\n");
    HDprintf("\n");
    HDexit(EXIT_FAILURE);
}

int
main(int argc, const char *argv[])
{
    long     nseconds    = 0;  /* # of seconds to test */
    int      poll_time   = 1;  /* # of seconds between polling */
    int      ncommon     = 5;  /* # of common symbols to poll */
    int      nrandom     = 10; /* # of random symbols to poll */
    unsigned verbose     = 1;  /* Whether to emit some informational messages */
    unsigned use_seed    = 0;  /* Set to 1 if a seed was set on the command line */
    unsigned random_seed = 0;  /* Random # seed */
    unsigned u;                /* Local index variables */
    int      temp;

    /* Parse command line options */
    if (argc < 2)
        usage();
    if (argc > 1) {
        u = 1;
        while (u < (unsigned)argc) {
            if (argv[u][0] == '-') {
                switch (argv[u][1]) {
                    /* # of common symbols to poll */
                    case 'h':
                        ncommon = HDatoi(argv[u + 1]);
                        if (ncommon < 0)
                            usage();
                        u += 2;
                        break;

                    /* # of random symbols to poll */
                    case 'l':
                        nrandom = HDatoi(argv[u + 1]);
                        if (nrandom < 0)
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
                        use_seed = 1;
                        temp     = HDatoi(argv[u + 1]);
                        if (temp < 0)
                            usage();
                        else
                            random_seed = (unsigned)temp;
                        u += 2;
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
                /* Get the number of records to append */
                nseconds = HDatol(argv[u]);
                if (nseconds <= 0)
                    usage();

                u++;
            } /* end else */
        }     /* end while */
    }         /* end if */
    if (nseconds <= 0)
        usage();
    if (poll_time >= nseconds)
        usage();

    /* Emit informational message */
    if (verbose) {
        HDfprintf(stderr, "Parameters:\n");
        HDfprintf(stderr, "\t# of seconds between polling = %d\n", poll_time);
        HDfprintf(stderr, "\t# of common symbols to poll = %d\n", ncommon);
        HDfprintf(stderr, "\t# of random symbols to poll = %d\n", nrandom);
        HDfprintf(stderr, "\t# of seconds to test = %ld\n", nseconds);
    } /* end if */

    /* Set the random seed */
    if (0 == use_seed) {
        struct timeval t;
        HDgettimeofday(&t, NULL);
        random_seed = (unsigned)(t.tv_usec);
    } /* end if */
    HDsrandom(random_seed);
    /* ALWAYS emit the random seed for possible debugging */
    HDfprintf(stderr, "Using reader random seed: %u\n", random_seed);

    /* Emit informational message */
    if (verbose)
        HDfprintf(stderr, "Generating symbol names\n");

    /* Generate dataset names */
    if (generate_symbols() < 0) {
        HDfprintf(stderr, "Error generating symbol names!\n");
        HDexit(EXIT_FAILURE);
    } /* end if */

    /* Create datatype for creating datasets */
    if ((symbol_tid = create_symbol_datatype()) < 0)
        return -1;

    /* Reading records from datasets */
    if (read_records(FILENAME, verbose, (unsigned long)nseconds, (unsigned)poll_time, (unsigned)ncommon,
                     (unsigned)nrandom) < 0) {
        HDfprintf(stderr, "Error reading records from datasets!\n");
        HDexit(EXIT_FAILURE);
    } /* end if */

    /* Emit informational message */
    if (verbose)
        HDfprintf(stderr, "Releasing symbols\n");

    /* Clean up the symbols */
    if (shutdown_symbols() < 0) {
        HDfprintf(stderr, "Error releasing symbols!\n");
        HDexit(EXIT_FAILURE);
    } /* end if */

    /* Emit informational message */
    if (verbose)
        HDfprintf(stderr, "Closing objects\n");

    /* Close objects created */
    if (H5Tclose(symbol_tid) < 0) {
        HDfprintf(stderr, "Error closing symbol datatype!\n");
        HDexit(EXIT_FAILURE);
    } /* end if */

    return 0;
}
