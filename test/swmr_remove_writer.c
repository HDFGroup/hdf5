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

#define MAX_REMOVE_SIZE 10

static hid_t
open_skeleton(const char *filename, unsigned verbose)
{
    hid_t fid;          /* File ID for new HDF5 file */
    hid_t fapl;         /* File access property list */
    hid_t sid;          /* Dataspace ID */
    hsize_t dim[2];     /* Dataspace dimensions */
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
            if((sid = H5Dget_space(symbol_info[u][v].dsid)) < 0)
                return -1;
            if(2 != H5Sget_simple_extent_ndims(sid))
                return -1;
            if(H5Sget_simple_extent_dims(sid, dim, NULL) < 0)
                return -1;
            symbol_info[u][v].nrecords = dim[1];
        } /* end for */

    return fid;
}

static int
remove_records(hid_t fid, unsigned verbose, unsigned long nshrinks, unsigned long flush_count)
{
    unsigned long shrink_to_flush;      /* # of removals before flush */
    hsize_t dim[2] = {1,0};             /* Dataspace dimensions */
    unsigned long u, v;                 /* Local index variables */

    /* Remove records from random datasets, according to frequency distribution */
    shrink_to_flush = flush_count;
    for(u = 0; u < nshrinks; u++) {
        symbol_info_t *symbol;  /* Symbol to remove record from */
        hsize_t remove_size;    /* Size to reduce dataset dimension by */

        /* Get a random dataset, according to the symbol distribution */
        symbol = choose_dataset();

        /* Shrink the dataset's dataspace */
        remove_size = (hsize_t)random() % MAX_REMOVE_SIZE + 1;
        if(remove_size > symbol->nrecords)
            symbol->nrecords = 0;
        else
            symbol->nrecords -= remove_size;
            dim[1] = symbol->nrecords;
        if(H5Dset_extent(symbol->dsid, dim) < 0)
            return -1;

        /* Check for flushing file */
        if(flush_count > 0) {
            /* Decrement count of records to write before flushing */
            shrink_to_flush--;

            /* Check for counter being reached */
            if(0 == shrink_to_flush) {
                /* Flush contents of file */
                if(H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
                    return -1;

                /* Reset flush counter */
                shrink_to_flush = flush_count;
            } /* end if */
        } /* end if */
    } /* end for */

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
    printf("Usage: swmr_remove_writer [-q] [-f <# of shrinks between flushing file contents>] [-r <random # seed>] <# of shrinks>\n");
    printf("<# of shrinks between flushing file contents> should be 0 (for no flushing) or between 1 and (<# of shrinks> - 1)\n");
    printf("Defaults to verbose (no '-q' given) and flushing every 1000 shrinks('-f 1000')\n");
    exit(1);
}

int main(int argc, const char *argv[])
{
    hid_t fid;                  /* File ID for file opened */
    long nshrinks = 0;          /* # of times to shrink the dataset */
    long flush_count = 1000;    /* # of records to write between flushing file */
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
                nshrinks = atol(argv[u]);
                if(nshrinks <= 0)
                    usage();

                u++;
            } /* end else */
        } /* end while */
    } /* end if */
    if(nshrinks <= 0)
        usage();
    if(flush_count >= nshrinks)
        usage();

    /* Emit informational message */
    if(verbose) {
        fprintf(stderr, "Parameters:\n");
        fprintf(stderr, "\t# of shrinks between flushes = %ld\n", flush_count);
        fprintf(stderr, "\t# of shrinks = %ld\n", nshrinks);
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
        fprintf(stderr, "Removing records\n");

    /* Remove records from datasets */
    if(remove_records(fid, verbose, (unsigned long)nshrinks, (unsigned long)flush_count) < 0) {
        fprintf(stderr, "Error removing records from datasets!\n");
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
