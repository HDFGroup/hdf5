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

#include <sys/time.h>

#include "swmr_common.h"

static hid_t
open_skeleton(const char *filename, unsigned verbose)
{
    hid_t fid;          /* File ID for new HDF5 file */
    hid_t fapl;         /* File access property list */
    unsigned u, v;      /* Local index variable */

    /* Create file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        return -1;

#ifdef QAK
    /* Increase the initial size of the metadata cache */
    {
        H5AC_cache_config_t mdc_config;

        mdc_config.version = H5AC__CURR_CACHE_CONFIG_VERSION;
        H5Pget_mdc_config(fapl, &mdc_config);
        fprintf(stderr, "mdc_config.initial_size = %lu\n", (unsigned long)mdc_config.initial_size);
        fprintf(stderr, "mdc_config.epoch_length = %lu\n", (unsigned long)mdc_config.epoch_length);
        mdc_config.set_initial_size = 1;
        mdc_config.initial_size = 16 * 1024 * 1024;
        /* mdc_config.epoch_length = 5000; */
        H5Pset_mdc_config(fapl, &mdc_config);
    }
#endif /* QAK */

#ifdef QAK
    H5Pset_fapl_log(fapl, "append.log", H5FD_LOG_ALL, (size_t)(512 * 1024 * 1024));
#endif /* QAK */

    /* Open the file */
    if((fid = H5Fopen(filename, H5F_ACC_RDWR | H5F_ACC_SWMR_WRITE, fapl)) < 0)
        return -1;

    /* Close file access property list */
    if(H5Pclose(fapl) < 0)
        return -1;

    /* Emit informational message */
    if(verbose)
        fprintf(stderr, "Opening datasets\n");

    /* Open the datasets */
    for(u = 0; u < NLEVELS; u++)
        for(v = 0; v < symbol_count[u]; v++) {
            if((symbol_info[u][v].dsid = H5Dopen2(fid, symbol_info[u][v].name, H5P_DEFAULT)) < 0)
                return -1;
            symbol_info[u][v].nrecords = 0;
        } /* end for */

    return(fid);
}

static int
add_records(hid_t fid, unsigned verbose, unsigned long nrecords, unsigned long flush_count)
{
    hid_t tid;                              /* Datatype ID for records */
    hid_t mem_sid;                          /* Memory dataspace ID */
    hsize_t start[2] = {0, 0}, count[2] = {1, 1}; /* Hyperslab selection values */
    hsize_t dim[2] = {1, 0};                /* Dataspace dimensions */
    symbol_t record;                        /* The record to add to the dataset */
    H5AC_cache_config_t mdc_config_orig;    /* Original metadata cache configuration */
    H5AC_cache_config_t mdc_config_cork;    /* Corked metadata cache configuration */
    unsigned long rec_to_flush;             /* # of records left to write before flush */
    unsigned long u, v;                     /* Local index variables */

    /* Reset the record */
    /* (record's 'info' field might need to change for each record written, also) */
    memset(&record, 0, sizeof(record));

    /* Create a dataspace for the record to add */
    if((mem_sid = H5Screate(H5S_SCALAR)) < 0)
        return -1;

    /* Create datatype for appending records */
    if((tid = create_symbol_datatype()) < 0)
        return -1;

    /* Get the current metadata cache configuration, and set up the corked
     * configuration */
    mdc_config_orig.version = H5AC__CURR_CACHE_CONFIG_VERSION;
    if(H5Fget_mdc_config(fid, &mdc_config_orig) < 0)
        return -1;
    memcpy(&mdc_config_cork, &mdc_config_orig, sizeof(mdc_config_cork));
    mdc_config_cork.evictions_enabled = FALSE;
    mdc_config_cork.incr_mode = H5C_incr__off;
    mdc_config_cork.flash_incr_mode = H5C_flash_incr__off;
    mdc_config_cork.decr_mode = H5C_decr__off;

    /* Add records to random datasets, according to frequency distribution */
    rec_to_flush = flush_count;
    for(u = 0; u < nrecords; u++) {
        symbol_info_t *symbol;  /* Symbol to write record to */
        hid_t file_sid;         /* Dataset's space ID */

        /* Get a random dataset, according to the symbol distribution */
        symbol = choose_dataset();

        /* Set the record's ID (equal to its position) */
        record.rec_id = symbol->nrecords;

        /* Get the coordinate to write */
        start[1] = symbol->nrecords;

        /* Cork the metadata cache, to prevent the object header from being
         * flushed before the data has been written */
        /*if(H5Fset_mdc_config(fid, &mdc_config_cork) < 0)
            return(-1);*/

        /* Extend the dataset's dataspace to hold the new record */
        symbol->nrecords++;
        dim[1] = symbol->nrecords;
        if(H5Dset_extent(symbol->dsid, dim) < 0)
            return -1;

        /* Get the dataset's dataspace */
        if((file_sid = H5Dget_space(symbol->dsid)) < 0)
            return -1;

        /* Choose the last record in the dataset */
        if(H5Sselect_hyperslab(file_sid, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
            return -1;

        /* Write record to the dataset */
        if(H5Dwrite(symbol->dsid, tid, mem_sid, file_sid, H5P_DEFAULT, &record) < 0)
            return -1;

        /* Uncork the metadata cache */
        /*if(H5Fset_mdc_config(fid, &mdc_config_orig) < 0)
            return -1;*/

        /* Close the dataset's dataspace */
        if(H5Sclose(file_sid) < 0)
            return -1;

        /* Check for flushing file */
        if(flush_count > 0) {
            /* Decrement count of records to write before flushing */
            rec_to_flush--;

            /* Check for counter being reached */
            if(0 == rec_to_flush) {
                /* Flush contents of file */
                if(H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
                    return -1;

                /* Reset flush counter */
                rec_to_flush = flush_count;
            } /* end if */
        } /* end if */
    } /* end for */

    /* Close the memory dataspace */
    if(H5Sclose(mem_sid) < 0)
        return -1;

    /* Close the datatype */
    if(H5Tclose(tid) < 0)
        return -1;

    /* Emit informational message */
    if(verbose)
        fprintf(stderr, "Closing datasets\n");

    /* Close the datasets */
    for(u = 0; u < NLEVELS; u++)
        for(v = 0; v < symbol_count[u]; v++)
            if(H5Dclose(symbol_info[u][v].dsid) < 0)
                return -1;

    return 0;
}

static void
usage(void)
{
    printf("Usage error!\n");
    printf("Usage: swmr_writer [-q] [-f <# of records to write between flushing file contents>] [-r <random # seed>] <# of records>\n");
    printf("<# of records to write between flushing file contents> should be 0 (for no flushing) or between 1 and (<# of records> - 1)\n");
    printf("Defaults to verbose (no '-q' given) and flushing every 10000 records('-f 10000')\n");
    exit(1);
}

int main(int argc, const char *argv[])
{
    hid_t fid;                  /* File ID for file opened */
    long nrecords = 0;          /* # of records to append */
    long flush_count = 10000;   /* # of records to write between flushing file */
    unsigned verbose = 1;       /* Whether to emit some informational messages */
    unsigned use_seed = 0;      /* Set to 1 if a seed was set on the command line */
    unsigned random_seed = 0;   /* Random # seed */
    unsigned u;                 /* Local index variable */
    int temp;

    /* Parse command line options */
    if(argc < 2)
        usage();
    if(argc > 1) {
        u = 1;
        while(u < (unsigned)argc) {
            if(argv[u][0] == '-') {
                switch(argv[u][1]) {
                    /* # of records to write between flushing file */
                    case 'f':
                        flush_count = atol(argv[u + 1]);
                        if(flush_count < 0)
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
                        temp = atoi(argv[u + 1]);
                        if(temp < 0)
                            usage();
                        else
                            random_seed = (unsigned)temp;
                        u += 2;
                        break;

                    default:
                        usage();
                        break;
                } /* end switch */
            } /* end if */
            else {
                /* Get the number of records to append */
                nrecords = atol(argv[u]);
                if(nrecords <= 0)
                    usage();

                u++;
            } /* end else */
        } /* end while */
    } /* end if */
    if(nrecords <= 0)
        usage();
    if(flush_count >= nrecords)
        usage();

    /* Emit informational message */
    if(verbose) {
        fprintf(stderr, "Parameters:\n");
        fprintf(stderr, "\t# of records between flushes = %ld\n", flush_count);
        fprintf(stderr, "\t# of records to write = %ld\n", nrecords);
    } /* end if */

    /* Set the random seed */
    if(0 == use_seed) {
        struct timeval t;
        gettimeofday(&t, NULL);
        random_seed = (unsigned)((t.tv_sec * 1000) + t.tv_usec);
    } /* end if */
    srandom(random_seed);
    /* ALWAYS emit the random seed for possible debugging */
    fprintf(stderr, "Using writer random seed: %u\n", random_seed);

    /* Emit informational message */
    if(verbose)
        fprintf(stderr, "Generating symbol names\n");

    /* Generate dataset names */
    if(generate_symbols() < 0)
        return -1;

    /* Emit informational message */
    if(verbose)
        fprintf(stderr, "Opening skeleton file: %s\n", FILENAME);

    /* Open file skeleton */
    if((fid = open_skeleton(FILENAME, verbose)) < 0) {
        fprintf(stderr, "Error opening skeleton file!\n");
        exit(1);
    } /* end if */

    /* Emit informational message */
    if(verbose)
        fprintf(stderr, "Adding records\n");

    /* Append records to datasets */
    if(add_records(fid, verbose, (unsigned long)nrecords, (unsigned long)flush_count) < 0) {
        fprintf(stderr, "Error appending records to datasets!\n");
        exit(1);
    } /* end if */

    /* Emit informational message */
    if(verbose)
        fprintf(stderr, "Releasing symbols\n");

    /* Clean up the symbols */
    if(shutdown_symbols() < 0) {
        fprintf(stderr, "Error releasing symbols!\n");
        exit(1);
    } /* end if */

    /* Emit informational message */
    if(verbose)
        fprintf(stderr, "Closing objects\n");

    /* Close objects opened */
    if(H5Fclose(fid) < 0) {
        fprintf(stderr, "Error closing file!\n");
        exit(1);
    } /* end if */

    return 0;
}
