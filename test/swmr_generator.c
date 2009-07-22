#include "swmr_common.h"

#define CHUNK_SIZE      50

static int
gen_skeleton(const char *filename, unsigned verbose, int comp_level)
{
    hid_t fid;          /* File ID for new HDF5 file */
    hid_t fcpl;         /* File creation property list */
    hid_t fapl;         /* File access property list */
    hid_t dcpl;         /* Dataset creation property list */
    hid_t tid;          /* Datatype for dataset elements */
    hid_t sid;          /* Dataspace ID */
    hsize_t dims = 0;   /* Dataset starting dimensions */
    hsize_t max_dims = H5S_UNLIMITED;   /* Dataset maximum dimensions */
    hsize_t chunk_dims = CHUNK_SIZE;    /* Chunk dimensions */
    unsigned u, v;      /* Local index variable */

    /* Create file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        return(-1);
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        return(-1);

#ifdef QAK
/* Increase the initial size of the metadata cache */
{
    H5AC_cache_config_t mdc_config;

    mdc_config.version = H5AC__CURR_CACHE_CONFIG_VERSION;
    H5Pget_mdc_config(fapl, &mdc_config);
printf("mdc_config.initial_size = %lu\n", (unsigned long)mdc_config.initial_size);
printf("mdc_config.epoch_length = %lu\n", (unsigned long)mdc_config.epoch_length);
    mdc_config.set_initial_size = 1;
    mdc_config.initial_size = 16 * 1024 * 1024;
/*    mdc_config.epoch_length = 5000; */
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
        return(-1);

#ifdef QAK
    H5Pset_link_phase_change(fcpl, 0, 0);
#endif /* QAK */

    /* Emit informational message */
    if(verbose)
        printf("Creating file\n");

    /* Create the file */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        return(-1);

    /* Close file creation property list */
    if(H5Pclose(fcpl) < 0)
        return(-1);

    /* Close file access property list */
    if(H5Pclose(fapl) < 0)
        return(-1);

    /* Create datatype for creating datasets */
    if((tid = create_symbol_datatype()) < 0)
        return(-1);

    /* Create dataspace for creating datasets */
    if((sid = H5Screate_simple(1, &dims, &max_dims)) < 0)
        return(-1);

    /* Create dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        return(-1);
    if(H5Pset_chunk(dcpl, 1, &chunk_dims) < 0)
        return(-1);
    if(comp_level >= 0) {
        if(H5Pset_deflate(dcpl, (unsigned)comp_level) < 0)
            return(-1);
    } /* end if */

    /* Emit informational message */
    if(verbose)
        printf("Creating datasets\n");

    /* Create the datasets */
    for(u = 0; u < NLEVELS; u++)
        for(v = 0; v < symbol_count[u]; v++) {
            hid_t dsid;         /* Dataset ID */
            char name_buf[64];

            generate_name(name_buf, u, v);
            if((dsid = H5Dcreate2(fid, name_buf, tid, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
                return(-1);

            if(H5Dclose(dsid) < 0)
                return(-1);
        } /* end for */

    /* Emit informational message */
    if(verbose)
        printf("Closing objects\n");

    /* Close everythign */
    if(H5Pclose(dcpl) < 0)
        return(-1);
    if(H5Sclose(sid) < 0)
        return(-1);
    if(H5Tclose(tid) < 0)
        return(-1);
    if(H5Fclose(fid) < 0)
        return(-1);

    return(0);
} /* end gen_skeleton() */

static void
usage(void)
{
    printf("Usage error!\n");
    printf("Usage: swmr_generator [-q] [-c <deflate compression level>]\n");
    printf("<deflate compression level> should be -1 (for no compression) or 0-9\n");
    printf("Defaults to verbose (no '-q' given) and no compression ('-c -1')\n");
    exit(1);
} /* end usage() */

int main(int argc, const char *argv[])
{
    int comp_level = (-1);      /* Compression level (-1 is no compression) */
    unsigned verbose = 1;       /* Whether to emit some informational messages */
    unsigned u;                 /* Local index variables */

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

                    /* Be quiet */
                    case 'q':
                        verbose = 0;
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
        printf("Parameters:\n");
        printf("\tcompression level = %d\n", comp_level);
    } /* end if */

    /* Emit informational message */
    if(verbose)
        printf("Generating skeleton file: %s\n", FILENAME);

    /* Generate file skeleton */
    if(gen_skeleton(FILENAME, verbose, comp_level) < 0) {
        printf("Error generating skeleton file!\n");
        exit(1);
    } /* end if */

    return(0);
}

