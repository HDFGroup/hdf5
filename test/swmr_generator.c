/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
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

#include "h5test.h"
#include "swmr_common.h"

/*
 * This file needs to access testing codefrom the H5O package.
 */
#define H5O_FRIEND	/*suppress error about including H5Opkg	  */
#define H5O_TESTING
#include "H5Opkg.h"     /* Object headers			*/


/****************/
/* Local Macros */
/****************/

#define CHUNK_SIZE      50      /* Chunk size for created datasets */

/********************/
/* Local Prototypes */
/********************/

static int gen_skeleton(const char *filename, hbool_t verbose,
    hbool_t swmr_write, int comp_level, const char *index_type,
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
 *              hbool_t verbose
 *              Whether verbose console output is desired.
 *
 *              hbool_t swmr_write
 *              Whether to create the file with SWMR writing enabled
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
 *              Failure:    Can't fail
 *
 *-------------------------------------------------------------------------
 */
static int
gen_skeleton(const char *filename, hbool_t verbose, hbool_t swmr_write,
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

    HDassert(filename);
    HDassert(index_type);

    /* Create file access property list */
    if((fapl = h5_fileaccess()) < 0)
        return -1;

    /* Can create a file for SWMR support with: (a) (write+latest-format) or (b) (SWMR write+non-latest-format) */
    if(!swmr_write) {
        if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
            return -1;
    }

    /* There are two chunk indexes tested here.
     * With one unlimited dimension, we get the extensible array index
     * type, with two unlimited dimensions, we get a v2 B-tree.
     */
    if(!HDstrcmp(index_type, "b2"))
        max_dims[0] = H5S_UNLIMITED;

#ifdef QAK
    /* Increase the initial size of the metadata cache */
    {
        H5AC_cache_config_t mdc_config;

        mdc_config.version = H5AC__CURR_CACHE_CONFIG_VERSION;
        H5Pget_mdc_config(fapl, &mdc_config);
        HDfprintf(stderr, "mdc_config.initial_size = %lu\n", (unsigned long)mdc_config.initial_size);
        HDfprintf(stderr, "mdc_config.epoch_length = %lu\n", (unsigned long)mdc_config.epoch_length);
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
        HDfprintf(stderr, "Creating file\n");

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
    HDmemset(&fillval, 0, sizeof(fillval));
    fillval.rec_id = (uint64_t)ULLONG_MAX;
    if(H5Pset_fill_value(dcpl, tid, &fillval) < 0)
        return -1;
#endif /* FILLVAL_WORKS */

    /* Emit informational message */
    if(verbose)
        HDfprintf(stderr, "Creating datasets\n");

    /* Create the datasets */
    for(u = 0; u < NLEVELS; u++)
        for(v = 0; v < symbol_count[u]; v++) {
            hid_t dsid;         /* Dataset ID */
            char name_buf[64];
            hbool_t move_dataspace_message = FALSE;     /* Whether to move the dataspace message out of object header chunk #0 */

            generate_name(name_buf, u, v);
            if((dsid = H5Dcreate2(fid, name_buf, tid, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
                return -1;

            /* Determine if the dataspace message for this dataset should be
             * moved out of chunk #0 of the object header
             * (Set to TRUE for every fourth dataset)
             */
            move_dataspace_message = !(HDrandom() % 4);
            if(move_dataspace_message) {
                unsigned chunk_num;        /* Object header chunk # for dataspace message */

                /* Move the dataspace message to a new object header chunk */
                if(H5O_msg_move_to_new_chunk_test(dsid, H5O_SDSPACE_ID) < 0)
                    return -1;

                /* Retrieve the chunk # for the dataspace message */
                chunk_num = UINT_MAX;
                if(H5O_msg_get_chunkno_test(dsid, H5O_SDSPACE_ID, &chunk_num) < 0)
                    return -1;
                /* Should not be in chunk #0 for now */
                if(0 == chunk_num)
                    return -1;
            } /* end if */

            if(H5Dclose(dsid) < 0)
                return -1;
        } /* end for */

    /* Emit informational message */
    if(verbose)
        HDfprintf(stderr, "Closing objects\n");

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
    HDprintf("\n");
    HDprintf("Usage error!\n");
    HDprintf("\n");
    HDprintf("Usage: swmr_generator [-q] [-s] [-c <deflate compression level>]\n");
    HDprintf("    [-i <index type>] [-r <random seed>]\n");
    HDprintf("\n");
    HDprintf("NOTE: The random seed option is only used by the sparse test.  Other\n");
    HDprintf("      tests specify the random seed as a reader/writer option.\n");
    HDprintf("\n");
    HDprintf("<deflate compression level> should be -1 (for no compression) or 0-9\n");
    HDprintf("\n");
    HDprintf("<index type> should be b2 or ea\n");
    HDprintf("\n");
    HDprintf("Defaults to verbose (no '-q' given), no SWMR_WRITE mode (no '-s' given) no\n");
    HDprintf("compression ('-c -1'), v1 b-tree indexing (-i b1), and will generate a random\n");
    HDprintf("seed (no -r given).\n");
    HDprintf("\n");
    HDexit(EXIT_FAILURE);
} /* end usage() */

int main(int argc, const char *argv[])
{
    int comp_level = -1;            /* Compression level (-1 is no compression) */
    hbool_t verbose = TRUE;         /* Whether to emit some informational messages */
    hbool_t swmr_write = FALSE;     /* Whether to create file with SWMR_WRITE access */
    const char *index_type = "b1";  /* Chunk index type */
    hbool_t use_seed = FALSE;       /* Set to TRUE if a seed was set on the command line */
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
                        comp_level = HDatoi(argv[u + 1]);
                        if(comp_level < -1 || comp_level > 9)
                            usage();
                        u += 2;
                        break;

                    /* Chunk index type */
                    case 'i':
                        index_type = argv[u + 1];
                        if(HDstrcmp(index_type, "ea")
                                && HDstrcmp(index_type, "b2"))
                            usage();
                        u += 2;
                        break;

                    /* Random # seed */
                    case 'r':
                        use_seed = TRUE;
                        temp = HDatoi(argv[u + 1]);
                        if(temp < 0)
                            usage();
                        else
                            random_seed = (unsigned)temp;
                        u += 2;
                        break;

                    /* Be quiet */
                    case 'q':
                        verbose = FALSE;
                        u++;
                        break;

                    /* Run with SWMR_WRITE */
                    case 's':
                        swmr_write = TRUE;
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
        HDfprintf(stderr, "Parameters:\n");
        HDfprintf(stderr, "\tswmr writes %s\n", swmr_write ? "on" : "off");
        HDfprintf(stderr, "\tcompression level = %d\n", comp_level);
        HDfprintf(stderr, "\tindex type = %s\n", index_type);
    } /* end if */
    
    /* Set the random seed */
    if(!use_seed) {
        struct timeval t;

        HDgettimeofday(&t, NULL);
        random_seed = (unsigned)(t.tv_usec);
    } /* end if */
    HDsrandom(random_seed);
    /* ALWAYS emit the random seed for possible debugging */
    HDfprintf(stderr, "Using generator random seed (used in sparse test only): %u\n", random_seed);

    /* Emit informational message */
    if(verbose)
        HDfprintf(stderr, "Generating skeleton file: %s\n", FILENAME);

    /* Generate file skeleton */
    if(gen_skeleton(FILENAME, verbose, swmr_write, comp_level, index_type, random_seed) < 0) {
        HDfprintf(stderr, "Error generating skeleton file!\n");
        HDexit(EXIT_FAILURE);
    } /* end if */

    return 0;
}
