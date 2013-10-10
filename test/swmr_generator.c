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
 * Created:     swmr_generator.c
 *
 * Purpose:     Functions for building and setting up the SWMR test file
 *              and datasets.
 *
 *-------------------------------------------------------------------------
 */

 /***********/
/* Headers */
/***********/

#include <assert.h>
#include <sys/time.h>

#include "swmr_common.h"

/****************/
/* Local Macros */
/****************/

#define CHUNK_SIZE      50      /* Chunk size for created datasets */

/********************/
/* Local Prototypes */
/********************/

static int gen_skeleton(const char *filename, unsigned verbose,
    unsigned swmr_write, int comp_level, const char *index_type,
    unsigned random_seed);
static void usage(void);


/*-------------------------------------------------------------------------
 * Function:    gen_skeleton
 *
 * Purpose:     Creates the HDF5 file and datasets which will be used in
 *              the SWMR testing.
 *
 * Parameters:  const char *filename
 *              The SWMR test file's name.
 *
 *              unsigned verbose
 *              Whether verbose console output is desired.
 *
 *              int comp_level
 *              The zlib compression level to use. -1 = no compression.
 *
 *              const char *index_type
 *              The chunk index type (b1 | b2 | ea | fa)
 *
 *              unsigned random_seed
 *              The random seed to store in the file.  The sparse tests use
 *              this value.
 *
 * Return:      Success:    0
 *                          
 *              Failure:    Can't fail
 *
 *-------------------------------------------------------------------------
 */
static int
gen_skeleton(const char *filename, unsigned verbose, unsigned swmr_write,
    int comp_level, const char *index_type, unsigned random_seed)
{
    hid_t fid;          /* File ID for new HDF5 file */
    hid_t fcpl;         /* File creation property list */
    hid_t fapl;         /* File access property list */
    hid_t dcpl;         /* Dataset creation property list */
    hid_t tid;          /* Datatype for dataset elements */
    hid_t sid;          /* Dataspace ID */
    hid_t aid;          /* Attribute ID */
    hsize_t dims[2] = {1, 0}; /* Dataset starting dimensions */
    hsize_t max_dims[2] = {1, H5S_UNLIMITED}; /* Dataset maximum dimensions */
    hsize_t chunk_dims[2] = {1, CHUNK_SIZE}; /* Chunk dimensions */
#ifdef FILLVAL_WORKS
    symbol_t fillval;   /* Dataset fill value */
#endif /* FILLVAL_WORKS */
    unsigned u, v;      /* Local index variable */

    assert(filename);
    assert(index_type);

    /* Create file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        return -1;

    /* We ALWAYS select the latest file format for SWMR */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        return -1;

    /* There are two chunk indexes tested here.
     * With one unlimited dimension, we get the extensible array index
     * type, with two unlimited dimensions, we get a v-2 B-tree.
     */
    if(!strcmp(index_type, "b2"))
        max_dims[0] = H5S_UNLIMITED;

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
    H5Pset_small_data_block_size(fapl, (hsize_t)(50 * CHUNK_SIZE * DTYPE_SIZE));
#endif /* QAK */

#ifdef QAK
    H5Pset_fapl_log(fapl, "append.log", H5FD_LOG_ALL, (size_t)(512 * 1024 * 1024));
#endif /* QAK */

    /* Create file creation property list */
    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        return -1;

#ifdef QAK
    H5Pset_link_phase_change(fcpl, 0, 0);
#endif /* QAK */

    /* Emit informational message */
    if(verbose)
        fprintf(stderr, "Creating file\n");

    /* Create the file */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC | (swmr_write ? H5F_ACC_SWMR_WRITE : 0), fcpl, fapl)) < 0)
        return -1;

    /* Close file creation property list */
    if(H5Pclose(fcpl) < 0)
        return -1;

    /* Close file access property list */
    if(H5Pclose(fapl) < 0)
        return -1;

    /* Create attribute with (shared) random number seed - for sparse test */
    if((sid = H5Screate(H5S_SCALAR)) < 0)
        return -1;
    if((aid = H5Acreate2(fid, "seed", H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        return -1;
    if(H5Awrite(aid, H5T_NATIVE_UINT, &random_seed) < 0)
        return -1;
    if(H5Sclose(sid) < 0)
        return -1;

    /* Create datatype for creating datasets */
    if((tid = create_symbol_datatype()) < 0)
        return -1;

    /* Create dataspace for creating datasets */
    if((sid = H5Screate_simple(2, dims, max_dims)) < 0)
        return -1;

    /* Create dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        return -1;
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        return -1;
    if(comp_level >= 0) {
        if(H5Pset_deflate(dcpl, (unsigned)comp_level) < 0)
            return -1;
    } /* end if */
#ifdef FILLVAL_WORKS
    /* Currently fill values do not work because they can bump the dataspace
     * message to the second object header chunk.  We should enable the fillval
     * here when this is fixed.  -NAF 8/11/11 */
    memset(&fillval, 0, sizeof(fillval));
    fillval.rec_id = (uint64_t)ULLONG_MAX;
    if(H5Pset_fill_value(dcpl, tid, &fillval) < 0)
        return -1;
#endif /* FILLVAL_WORKS */

    /* Emit informational message */
    if(verbose)
        fprintf(stderr, "Creating datasets\n");

    /* Create the datasets */
    for(u = 0; u < NLEVELS; u++)
        for(v = 0; v < symbol_count[u]; v++) {
            hid_t dsid;         /* Dataset ID */
            char name_buf[64];

            generate_name(name_buf, u, v);
            if((dsid = H5Dcreate2(fid, name_buf, tid, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
                return -1;

            if(H5Dclose(dsid) < 0)
                return -1;
        } /* end for */

    /* Emit informational message */
    if(verbose)
        fprintf(stderr, "Closing objects\n");

    /* Close everythign */
    if(H5Pclose(dcpl) < 0)
        return -1;
    if(H5Sclose(sid) < 0)
        return -1;
    if(H5Tclose(tid) < 0)
        return -1;
    if(H5Fclose(fid) < 0)
        return -1;

    return 0;
} /* end gen_skeleton() */

static void
usage(void)
{
    printf("\n");
    printf("Usage error!\n");
    printf("\n");
    printf("Usage: swmr_generator [-q] [-s] [-c <deflate compression level>]\n");
    printf("    [-i <index type>] [-r <random seed>]\n");
    printf("\n");
    printf("NOTE: The random seed option is only used by the sparse test.  Other\n");
    printf("      tests specify the random seed as a reader/writer option.\n");
    printf("\n");
    printf("<deflate compression level> should be -1 (for no compression) or 0-9\n");
    printf("\n");
    printf("<index type> should be b2 or ea\n");
    printf("\n");
    printf("Defaults to verbose (no '-q' given), no SWMR_WRITE mode (no '-s' given) no\n");
    printf("compression ('-c -1'), v1 b-tree indexing (-i b1), and will generate a random\n");
    printf("seed (no -r given).\n");
    printf("\n");
    exit(1);
} /* end usage() */

int main(int argc, const char *argv[])
{
    int comp_level = -1;            /* Compression level (-1 is no compression) */
    unsigned verbose = 1;           /* Whether to emit some informational messages */
    unsigned swmr_write = 0;        /* Whether to create file with SWMR_WRITE access */
    const char *index_type = "b1";  /* Chunk index type */
    unsigned use_seed = 0;          /* Set to 1 if a seed was set on the command line */
    unsigned random_seed = 0;       /* Random # seed */
    unsigned u;                     /* Local index variables */
    int temp;

    /* Parse command line options */
    if(argc > 1) {
        u = 1;
        while(u < (unsigned)argc) {
            if(argv[u][0] == '-') {
                switch(argv[u][1]) {
                    /* Compress dataset chunks */
                    case 'c':
                        comp_level = atoi(argv[u + 1]);
                        if(comp_level < -1 || comp_level > 9)
                            usage();
                        u += 2;
                        break;

                    /* Chunk index type */
                    case 'i':
                        index_type = argv[u + 1];
                        if(strcmp(index_type, "ea")
                                && strcmp(index_type, "b2"))
                            usage();
                        u += 2;
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

                    /* Be quiet */
                    case 'q':
                        verbose = 0;
                        u++;
                        break;

                    /* Run with SWMR_WRITE */
                    case 's':
                        swmr_write = 1;
                        u++;
                        break;

                    default:
                        usage();
                        break;
                } /* end switch */
            } /* end if */
        } /* end while */
    } /* end if */

    /* Emit informational message */
    if(verbose) {
        fprintf(stderr, "Parameters:\n");
        fprintf(stderr, "\tswmr writes %s\n", swmr_write ? "on" : "off");
        fprintf(stderr, "\tcompression level = %d\n", comp_level);
        fprintf(stderr, "\tindex type = %s\n", index_type);
    } /* end if */
    
    /* Set the random seed */
    if(0 == use_seed) {
        struct timeval t;
        gettimeofday(&t, NULL);
        random_seed = (unsigned)((t.tv_sec * 1000) + t.tv_usec);
    } /* end if */
    srandom(random_seed);
    /* ALWAYS emit the random seed for possible debugging */
    fprintf(stderr, "Using generator random seed (used in sparse test only): %u\n", random_seed);

    /* Emit informational message */
    if(verbose)
        fprintf(stderr, "Generating skeleton file: %s\n", FILENAME);

    /* Generate file skeleton */
    if(gen_skeleton(FILENAME, verbose, swmr_write, comp_level, index_type, random_seed) < 0) {
        fprintf(stderr, "Error generating skeleton file!\n");
        exit(1);
    } /* end if */

    return 0;
}
