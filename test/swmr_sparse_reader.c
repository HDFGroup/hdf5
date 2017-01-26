/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:     swmr_sparse_reader.c
 *
 * Purpose:     Reads data from a randomly selected subset of the datasets
 *              in the SWMR test file.  Unlike the regular reader, these
 *              datasets will be shrinking.
 *
 *              This program is intended to run concurrently with the
 *              swmr_sparse_writer program.
 *
 *-------------------------------------------------------------------------
 */

/***********/
/* Headers */
/***********/

#include "h5test.h"
#include "swmr_common.h"

/****************/
/* Local Macros */
/****************/

#define TIMEOUT 300

/*******************/
/* Local Variables */
/*******************/

static hid_t symbol_tid = (-1);

/********************/
/* Local Prototypes */
/********************/

static int check_dataset(hid_t fid, unsigned verbose, const symbol_info_t *symbol,
    symbol_t *record, hid_t rec_sid);
static int read_records(const char *filename, unsigned verbose, unsigned long nrecords,
    unsigned poll_time, unsigned reopen_count);
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
check_dataset(hid_t fid, unsigned verbose, const symbol_info_t *symbol, symbol_t *record,
    hid_t rec_sid)
{
    hid_t dsid;                 /* Dataset ID */
    hid_t file_sid;             /* Dataset's space ID */
    hsize_t start[2] = {0, 0};  /* Hyperslab selection values */
    hsize_t count[2] = {1, 1};  /* Hyperslab selection values */

    HDassert(fid >= 0);
    HDassert(symbol);
    HDassert(record);
    HDassert(rec_sid >= 0);

    /* Open dataset for symbol */
    if((dsid = H5Dopen2(fid, symbol->name, H5P_DEFAULT)) < 0)
        return -1;

    /* Get the dataset's dataspace */
    if((file_sid = H5Dget_space(dsid)) < 0)
        return -1;

    /* Choose the random record in the dataset (will be the same as chosen by
     * the writer) */
    start[1] = (hsize_t)HDrandom() % symbol->nrecords;
    if(H5Sselect_hyperslab(file_sid, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
        return -1;

    /* Emit informational message */
    if(verbose)
        HDfprintf(stderr, "Symbol = '%s', location = %lld\n", symbol->name, (long long)start);

    /* Read record from dataset */
    record->rec_id = (uint64_t)ULLONG_MAX;
    if(H5Dread(dsid, symbol_tid, rec_sid, file_sid, H5P_DEFAULT, record) < 0)
        return -1;

    /* Verify record value */
    if(record->rec_id != start[1]) {
        HDfprintf(stderr, "*** ERROR ***\n");
        HDfprintf(stderr, "Incorrect record value!\n");
        HDfprintf(stderr, "Symbol = '%s', location = %lld, record->rec_id = %llu\n", symbol->name, (long long)start, (unsigned long long)record->rec_id);
        return -1;
    } /* end if */

    /* Close the dataset's dataspace */
    if(H5Sclose(file_sid) < 0)
        return -1;

    /* Close dataset for symbol */
    if(H5Dclose(dsid) < 0)
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
read_records(const char *filename, unsigned verbose, unsigned long nrecords,
    unsigned poll_time, unsigned reopen_count)
{
    hid_t fid;                  /* File ID */
    hid_t aid;                  /* Attribute ID */
    time_t start_time;          /* Starting time */
    hid_t mem_sid;              /* Memory dataspace ID */
    symbol_t record;            /* The record to add to the dataset */
    unsigned seed;              /* Seed for random number generator */
    unsigned iter_to_reopen = reopen_count; /* # of iterations until reopen */
    unsigned long u;            /* Local index variable */
    hid_t fapl;

    HDassert(filename);
    HDassert(poll_time != 0);
    
    /* Create file access property list */
    if((fapl = h5_fileaccess()) < 0)
        return -1;

    H5Pset_fclose_degree(fapl, H5F_CLOSE_SEMI);

    /* Emit informational message */
    if(verbose)
        HDfprintf(stderr, "Opening file: %s\n", filename);

    /* Open the file */
    if((fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl)) < 0)
        return -1;

    /* Seed the random number generator with the attribute in the file */
    if((aid = H5Aopen(fid, "seed", H5P_DEFAULT)) < 0)
        return -1;
    if(H5Aread(aid, H5T_NATIVE_UINT, &seed) < 0)
        return -1;
    if(H5Aclose(aid) < 0)
        return -1;
    HDsrandom(seed);

    /* Reset the record */
    /* (record's 'info' field might need to change for each record written, also) */
    HDmemset(&record, 0, sizeof(record));

    /* Create a dataspace for the record to read */
    if((mem_sid = H5Screate(H5S_SCALAR)) < 0)
        return -1;

    /* Emit informational message */
    if(verbose)
        HDfprintf(stderr, "Reading records\n");

    /* Get the starting time */
    start_time = HDtime(NULL);

    /* Read records */
    for(u = 0; u < nrecords; u++) {
        symbol_info_t *symbol = NULL;   /* Symbol (dataset) */
        htri_t attr_exists;             /* Whether the sequence number attribute exists */
        unsigned long file_u;           /* Attribute sequence number (writer's "u") */

        /* Get a random dataset, according to the symbol distribution */
        symbol = choose_dataset();

        /* Fill in "nrecords" field.  Note that this depends on the writer
         * using the same algorithm and "nrecords" */
        symbol->nrecords = nrecords / 5;

        /* Wait until we can read the dataset */
        do {
            /* Check if sequence attribute exists */
            if((attr_exists = H5Aexists_by_name(fid, symbol->name, "seq", H5P_DEFAULT)) < 0)
                return -1;

            if(attr_exists) {
                /* Read sequence number attribute */
                if((aid = H5Aopen_by_name(fid, symbol->name, "seq", H5P_DEFAULT, H5P_DEFAULT)) < 0)
                    return -1;
                if(H5Aread(aid, H5T_NATIVE_ULONG, &file_u) < 0)
                    return -1;
                if(H5Aclose(aid) < 0)
                    return -1;

                /* Check if sequence number is at least u - if so, this should
                 * guarantee that this record has been written */
                if(file_u >= u)
                    break;
            } /* end if */

            /* Check for timeout */
            if(HDtime(NULL) >= (time_t)(start_time + (time_t)TIMEOUT)) {
                HDfprintf(stderr, "Reader timed out\n");
                return -1;
            } /* end if */

            /* Pause */
            HDsleep(poll_time);

            /* Retrieve and print the collection of metadata read retries */
            if(print_metadata_retries_info(fid) < 0)
                HDfprintf(stderr, "Warning: could not obtain metadata retries info\n");

            /* Reopen the file */
            if(H5Fclose(fid) < 0)
                return -1;
            if((fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl)) < 0)
                return -1;
            iter_to_reopen = reopen_count;
        } while(1);

        /* Emit informational message */
        if(verbose)
            HDfprintf(stderr, "Checking dataset %lu\n", u);

        /* Check dataset */
        if(check_dataset(fid, verbose, symbol, &record, mem_sid) < 0)
            return -1;
        HDmemset(&record, 0, sizeof(record));

        /* Check for reopen */
        iter_to_reopen--;
        if(iter_to_reopen == 0) {
            /* Emit informational message */
            if(verbose)
                HDfprintf(stderr, "Reopening file: %s\n", filename);

            /* Retrieve and print the collection of metadata read retries */
            if(print_metadata_retries_info(fid) < 0)
                HDfprintf(stderr, "Warning: could not obtain metadata retries info\n");

            /* Reopen the file */
            if(H5Fclose(fid) < 0)
                return -1;
            if((fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl)) < 0)
                return -1;
            iter_to_reopen = reopen_count;
        } /* end if */
    } /* end while */

    /* Retrieve and print the collection of metadata read retries */
    if(print_metadata_retries_info(fid) < 0)
        HDfprintf(stderr, "Warning: could not obtain metadata retries info\n");

    /* Close file */
    if(H5Fclose(fid) < 0)
        return -1;

    /* Close the memory dataspace */
    if(H5Sclose(mem_sid) < 0)
        return -1;

    return 0;
} /* end read_records() */

static void
usage(void)
{
    printf("\n");
    printf("Usage error!\n");
    printf("\n");
    printf("Usage: swmr_sparse_reader [-q] [-s <# of seconds to wait for writer>]\n");
    printf("    [-n <# of reads between reopens>] <# of records>\n");
    printf("\n");
    printf("Defaults to verbose (no '-q' given), 1 second wait ('-s 1') and 1 read\n");
    printf("between reopens ('-r 1')\n");
    printf("\n");
    printf("Note that the # of records *must* be the same as that supplied to\n");
    printf("swmr_sparse_writer\n");
    printf("\n");
    HDexit(1);
} /* end usage() */

int main(int argc, const char *argv[])
{
    long nrecords = 0;      /* # of records to read */
    int poll_time = 1;      /* # of seconds to sleep when waiting for writer */
    int reopen_count = 1;   /* # of reads between reopens */
    unsigned verbose = 1;   /* Whether to emit some informational messages */
    unsigned u;             /* Local index variables */

    /* Parse command line options */
    if(argc < 2)
        usage();
    if(argc > 1) {
        u = 1;
        while(u < (unsigned)argc) {
            if(argv[u][0] == '-') {
                switch(argv[u][1]) {
                    /* # of reads between reopens */
                    case 'n':
                        reopen_count = HDatoi(argv[u + 1]);
                        if(reopen_count < 0)
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
                        if(poll_time < 0)
                            usage();
                        u += 2;
                        break;

                    default:
                        usage();
                        break;
                } /* end switch */
            } /* end if */
            else {
                /* Get the number of records to read */
                nrecords = HDatol(argv[u]);
                if(nrecords <= 0)
                    usage();

                u++;
            } /* end else */
        } /* end while */
    } /* end if */

    /* Emit informational message */
    if(verbose) {
        HDfprintf(stderr, "Parameters:\n");
        HDfprintf(stderr, "\t# of seconds between polling = %d\n", poll_time);
        HDfprintf(stderr, "\t# of reads between reopens = %d\n", reopen_count);
        HDfprintf(stderr, "\t# of records to read = %ld\n", nrecords);
    } /* end if */

    /* Emit informational message */
    if(verbose)
        HDfprintf(stderr, "Generating symbol names\n");

    /* Generate dataset names */
    if(generate_symbols() < 0) {
        HDfprintf(stderr, "Error generating symbol names!\n");
        HDexit(1);
    } /* end if */

    /* Create datatype for creating datasets */
    if((symbol_tid = create_symbol_datatype()) < 0)
        return -1;

    /* Reading records from datasets */
    if(read_records(FILENAME, verbose, (unsigned long) nrecords, (unsigned)poll_time, (unsigned)reopen_count) < 0) {
        HDfprintf(stderr, "Error reading records from datasets!\n");
        HDexit(1);
    } /* end if */

    /* Emit informational message */
    if(verbose)
        HDfprintf(stderr, "Releasing symbols\n");

    /* Clean up the symbols */
    if(shutdown_symbols() < 0) {
        HDfprintf(stderr, "Error releasing symbols!\n");
        HDexit(1);
    } /* end if */

    /* Emit informational message */
    if(verbose)
        HDfprintf(stderr, "Closing objects\n");

    /* Close objects created */
    if(H5Tclose(symbol_tid) < 0) {
        HDfprintf(stderr, "Error closing symbol datatype!\n");
        HDexit(1);
    } /* end if */

    return 0;
}
