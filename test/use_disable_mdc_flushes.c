/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * This is copied from use_append_chunk.c with modifications to show
 * the usage of H5Odisable_mdc_flushes/H5Oenable_mdc_flushes/H5Oare_mdc_flushes_disabled public routines.
 */

#include "h5test.h"

/* This test uses many POSIX things that are not available on
 * Windows. We're using a check for fork(2) here as a proxy for
 * all POSIX/Unix/Linux things until this test can be made
 * more platform-independent.
 */
#ifdef H5_HAVE_FORK

#define H5D_FRIEND        /*suppress error about including H5Dpkg      */
#define H5D_TESTING
#include "H5Dpkg.h"

/* Global Variable definitions */
const char *progname_g="use_disable_mdc_flushes";    /* program name */

/* these two definitions must match each other */
#define UC_DATATYPE             H5T_NATIVE_SHORT    /* use case HDF5 data type */
#define UC_CTYPE                short               /* use case C data type */
#define UC_RANK                 3                   /* use case dataset rank */
#define Chunksize_DFT           256     /* chunksize default */
#define Hgoto_error(val)        {ret_value=val; goto done;}


char *filename_g;
hsize_t nplanes_g;
int use_swmr_g;
int chunkplanes_g;
int chunksize_g;
hsize_t dims_g[UC_RANK];
hsize_t max_dims_g[UC_RANK];
hsize_t chunkdims_g[UC_RANK];

static void usage(const char *prog);
static int parse_option(int argc, char * const argv[]);
static void show_parameters(void);
static int create_file(void);
static int setup_parameters(int argc, char * const argv[]);

/*
 * Note: Long options are not yet implemented.
 *
 * usage: use_disable_mdc_flushes [OPTIONS]
 * OPTIONS
 *  -h, --help           Print a usage message and exit
 *  -f FN                 Test file name [default: use_disable_mdc_flushes.h5]
 *  -n N, --nplanes=N     Number of planes to write. [default: 1000]
 *  -s N, --swmr=N        Use SWMR mode (0: no, non-0: yes) default is yes
 *  -z N, --chunksize=N   Chunk size [default: 256]
 *  -y N, --chunkplanes=N Number of planes per chunk [default: 1]
 */
static void
usage(const char *prog)
{
    HDfprintf(stderr, "usage: %s [OPTIONS]\n", prog);
    HDfprintf(stderr, "  OPTIONS\n");
    HDfprintf(stderr, "     -h          Print a usage message and exit\n");
    HDfprintf(stderr, "     -f FN       Test file name [default: %s.h5]\n", prog);
    HDfprintf(stderr, "     -n N        Number of planes to write. [default: 1000]\n");
    HDfprintf(stderr, "     -s N        Use SWMR mode (0: no, non-0: yes) default is yes\n");
    HDfprintf(stderr, "     -z N        Chunk size [default: %d]\n", Chunksize_DFT);
    HDfprintf(stderr, "     -y N        Number of planes per chunk [default: 1]\n");
    HDfprintf(stderr, "\n");
} /* usage() */


/*
 * Setup Use Case parameters by parsing command line options.
 * Setup default values if not set by options. */
static int
parse_option(int argc, char * const argv[])
{
    int ret_value=0;
    int c;
    /* command line options: See function usage for a description */
    const char *cmd_options = "f:hn:s:y:z:";

    /* suppress getopt from printing error */
    opterr = 0;

    while (1){
    c = getopt (argc, argv, cmd_options);
    if (-1 == c)
        break;
    switch (c) {
    case 'h':
        usage(progname_g);
        HDexit(EXIT_SUCCESS);
        break;
    case 'f':    /* usecase data file name */
        filename_g = optarg;
        break;
    case 'n':    /* number of planes to write/read */
        if ((nplanes_g = HDatoi(optarg)) <= 0){
        HDfprintf(stderr, "bad number of planes %s, must be a positive integer\n", optarg);
        usage(progname_g);
        Hgoto_error(-1);
        };
        break;
    case 's':    /* use swmr file open mode */
        if ((use_swmr_g = HDatoi(optarg)) < 0){
        HDfprintf(stderr, "swmr value should be 0(no) or 1(yes)\n");
        usage(progname_g);
        Hgoto_error(-1);
        };
        break;
    case 'y':    /* Number of planes per chunk */
        if ((chunkplanes_g = HDatoi(optarg)) <= 0){
        HDfprintf(stderr, "bad number of planes per chunk %s, must be a positive integer\n", optarg);
        usage(progname_g);
        Hgoto_error(-1);
        };
        break;
    case 'z':    /* size of chunk=(z,z) */
        if ((chunksize_g = HDatoi(optarg)) <= 0){
        HDfprintf(stderr, "bad chunksize %s, must be a positive integer\n", optarg);
        usage(progname_g);
        Hgoto_error(-1);
        };
        break;
    case '?':
        HDfprintf(stderr, "getopt returned '%c'.\n", c);
        Hgoto_error(-1);
    default:
        HDfprintf(stderr, "getopt returned unexpected value.\n");
        HDfprintf(stderr, "Unexpected value is %d\n", c);
        Hgoto_error(-1);
    }
    }

    /* set test file name if not given */
    if (!filename_g){
    /* default data file name is <progname>.h5 */
    if ((filename_g = (char*)HDmalloc(HDstrlen(progname_g)+4))==NULL) {
        HDfprintf(stderr, "malloc: failed\n");
        Hgoto_error(-1);
    };
    HDstrcpy(filename_g, progname_g);
    HDstrcat(filename_g, ".h5");
    }

done:
    /* All done. */
    return(ret_value);
} /* parse_option() */

/* Show parameters used for this use case */
static void
show_parameters(void)
{
    HDprintf("===Parameters used:===\n");
    HDprintf("chunk dims=(%llu, %llu, %llu)\n", (unsigned long long)chunkdims_g[0],
        (unsigned long long)chunkdims_g[1], (unsigned long long)chunkdims_g[2]);
    HDprintf("dataset max dims=(%llu, %llu, %llu)\n", (unsigned long long)max_dims_g[0],
        (unsigned long long)max_dims_g[1], (unsigned long long)max_dims_g[2]);
    HDprintf("number of planes to write=%llu\n", (unsigned long long)nplanes_g);
    HDprintf("using SWMR mode=%s\n", use_swmr_g ? "yes(1)" : "no(0)");
    HDprintf("data filename=%s\n", filename_g);
    HDprintf("===Parameters shown===\n");
} /* show_parameters() */

/*
 * Setup parameters for the use case.
 * Return: 0 succeed; -1 fail.
 */
static int
setup_parameters(int argc, char * const argv[])
{
    /* use case defaults */
    chunksize_g = Chunksize_DFT;
    use_swmr_g = 1;    /* use swmr open */
    chunkplanes_g = 1;

    /* parse options */
    if (parse_option(argc, argv) < 0){
    return(-1);
    }
    /* set chunk dims */
    chunkdims_g[0] = chunkplanes_g;
    chunkdims_g[1]= chunkdims_g[2] = chunksize_g;

    /* set dataset initial and max dims */
    dims_g[0] = 0;
    max_dims_g[0] = H5S_UNLIMITED;
    dims_g[1] = dims_g[2] = max_dims_g[1] = max_dims_g[2] = chunksize_g;

    /* set nplanes */
    if (nplanes_g == 0)
        nplanes_g = chunksize_g;

    /* show parameters and return */
    show_parameters();
    return(0);
} /* setup_parameters() */

/*
 * Create the skeleton use case file for testing.
 * It has one 3d dataset using chunked storage.
 * The dataset is (unlimited, chunksize, chunksize).
 * Dataset type is 2 bytes integer.
 * It starts out "empty", i.e., first dimension is 0.
 *
 * Return: 0 succeed; -1 fail.
 */
static int
create_file(void)
{
    hsize_t dims[3];        /* Dataset starting dimensions */
    hid_t fid;          /* File ID for new HDF5 file */
    hid_t dcpl;         /* Dataset creation property list */
    hid_t sid;          /* Dataspace ID */
    hid_t dsid;         /* Dataset ID */
    hid_t fapl;         /* File access property list */
    H5D_chunk_index_t idx_type; /* Chunk index type */

    /* Create the file */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        return -1;
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        return -1;
    if((fid = H5Fcreate(filename_g, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        return -1;

    /* Set up dimension sizes */
    dims[0] = 0;
    dims[1] = dims[2] = max_dims_g[1];

    /* Create dataspace for creating datasets */
    if((sid = H5Screate_simple(3, dims, max_dims_g)) < 0)
        return -1;

    /* Create dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        return -1;
    if(H5Pset_chunk(dcpl, 3, chunkdims_g) < 0)
        return -1;

    /* create dataset of progname */
    if((dsid = H5Dcreate2(fid, progname_g, UC_DATATYPE, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
    return -1;

    /* Check that the chunk index type is not version 1 B-tree.
     * Version 1 B-trees are not supported under SWMR.
     */
    if(H5D__layout_idx_type_test(dsid, &idx_type) < 0)
        return -1;
    if(idx_type == H5D_CHUNK_IDX_BTREE) {
        HDfprintf(stderr, "ERROR: Chunk index is version 1 B-tree: aborting.\n");
        return -1;
    }

    /* Close everything */
    if(H5Dclose(dsid) < 0)
    return -1;
    if(H5Pclose(fapl) < 0)
        return -1;
    if(H5Pclose(dcpl) < 0)
        return -1;
    if(H5Sclose(sid) < 0)
        return -1;
    if(H5Fclose(fid) < 0)
        return -1;

    return 0;
} /* create_file() */

/*
 * Append planes, each of (1,2*chunksize,2*chunksize) to the dataset.
 * In other words, 4 chunks are appended to the dataset at a time.
 * Fill each plane with the plane number and then write it at the nth plane.
 * Increase the plane number and repeat till the end of dataset, when it
 * reaches chunksize long. End product is a (2*chunksize)^3 cube.
 *
 * Return: 0 succeed; -1 fail.
 */
static int
write_file(void)
{
    hid_t    fid;              /* File ID for new HDF5 file */
    hid_t    dsid;             /* dataset ID */
    hid_t       fapl;             /* File access property list */
    hid_t    dcpl;          /* Dataset creation property list */
    char    *name;
    UC_CTYPE    *buffer, *bufptr;    /* data buffer */
    hsize_t    cz=chunksize_g;        /* Chunk size */
    hid_t    f_sid;            /* dataset file space id */
    hid_t    m_sid;            /* memory space id */
    int        rank;            /* rank */
    hsize_t     chunk_dims[3];    /* Chunk dimensions */
    hsize_t    dims[3];        /* Dataspace dimensions */
    hsize_t    memdims[3];     /* Memory space dimensions */
    hsize_t    start[3] = {0,0,0}, count[3];    /* Hyperslab selection values */
    hbool_t     disabled;       /* Object's disabled status */
    hsize_t     i, j, k;

    name = filename_g;

    /* Open the file */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        return -1;
    if(use_swmr_g)
        if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
            return -1;
    if((fid = H5Fopen(name, H5F_ACC_RDWR | (use_swmr_g ? H5F_ACC_SWMR_WRITE : 0), fapl)) < 0){
    HDfprintf(stderr, "H5Fopen failed\n");
        return -1;
    }

    /* Open the dataset of the program name */
    if((dsid = H5Dopen2(fid, progname_g, H5P_DEFAULT)) < 0){
    HDfprintf(stderr, "H5Dopen2 failed\n");
    return -1;
    }

    /* Disabled mdc flushed for the dataset */
    if(H5Odisable_mdc_flushes(dsid) < 0) {
    HDfprintf(stderr, "H5Odisable_mdc_flushes failed\n");
    return -1;
    }

    /* Get mdc disabled status of the dataset */
    if(H5Oare_mdc_flushes_disabled(dsid, &disabled) < 0) {
    HDfprintf(stderr, "H5Oare_mdc_flushes_disabled failed\n");
    return -1;
    } else if(disabled)
    HDprintf("Dataset has disabled mdc flushes.\n");
    else
    HDprintf("Dataset should have disabled its mdc flushes.\n");

    /* Find chunksize used */
    if ((dcpl = H5Dget_create_plist(dsid)) < 0){
    HDfprintf(stderr, "H5Dget_create_plist failed\n");
    return -1;
    }
    if (H5D_CHUNKED != H5Pget_layout(dcpl)){
    HDfprintf(stderr, "storage layout is not chunked\n");
    return -1;
    }
    if ((rank = H5Pget_chunk(dcpl, 3, chunk_dims)) != 3){
    HDfprintf(stderr, "storage rank is not 3\n");
    return -1;
    }

    /* verify chunk_dims against set paramenters */
    if (chunk_dims[0]!= chunkdims_g[0] || chunk_dims[1] != cz || chunk_dims[2] != cz){
    HDfprintf(stderr, "chunk size is not as expected. Got dims=(%llu,%llu,%llu)\n",
        (unsigned long long)chunk_dims[0], (unsigned long long)chunk_dims[1],
            (unsigned long long)chunk_dims[2]);
    return -1;
    }

    /* allocate space for data buffer 1 X dims[1] X dims[2] of UC_CTYPE */
    memdims[0]=1;
    memdims[1] = dims_g[1];
    memdims[2] = dims_g[2];
    if ((buffer=(UC_CTYPE*)HDmalloc((size_t)memdims[1]*(size_t)memdims[2]*sizeof(UC_CTYPE)))==NULL) {
    HDfprintf(stderr, "malloc: failed\n");
    return -1;
    };

    /*
     * Get dataset rank and dimension.
     */
    f_sid = H5Dget_space(dsid);    /* Get filespace handle first. */
    rank  = H5Sget_simple_extent_ndims(f_sid);
    if (rank != UC_RANK){
    HDfprintf(stderr, "rank(%d) of dataset does not match\n", rank);
    return -1;
    }
    if (H5Sget_simple_extent_dims(f_sid, dims, NULL) < 0){
    HDfprintf(stderr, "H5Sget_simple_extent_dims got error\n");
    return -1;
    }
    HDprintf("dataset rank %d, dimensions %llu x %llu x %llu\n",
    rank, (unsigned long long)(dims[0]), (unsigned long long)(dims[1]),
           (unsigned long long)(dims[2]));
    /* verify that file space dims are as expected and are consistent with memory space dims */
    if (dims[0] != 0 || dims[1] != memdims[1] || dims[2] != memdims[2]){
    HDfprintf(stderr, "dataset is not empty. Got dims=(%llu,%llu,%llu)\n",
        (unsigned long long)dims[0], (unsigned long long)dims[1],
            (unsigned long long)dims[2]);
    return -1;
    }

    /* setup mem-space for buffer */
    if ((m_sid=H5Screate_simple(rank, memdims, NULL))<0){
    HDfprintf(stderr, "H5Screate_simple for memory failed\n");
    return -1;
    };

    /* write planes */
    count[0]=1;
    count[1]=dims[1];
    count[2]=dims[2];
    for (i=0; i<nplanes_g; i++){
    /* fill buffer with value i+1 */
    bufptr = buffer;
    for (j=0; j<dims[1]; j++)
        for (k=0; k<dims[2]; k++)
        *bufptr++ = i;

    /* extend the dataset by one for new plane */
    dims[0]=i+1;
        if(H5Dset_extent(dsid, dims) < 0){
        HDfprintf(stderr, "H5Dset_extent failed\n");
            return -1;
    }

        /* Get the dataset's dataspace */
        if((f_sid = H5Dget_space(dsid)) < 0){
        HDfprintf(stderr, "H5Dset_extent failed\n");
            return -1;
    }

    start[0]=i;
        /* Choose the next plane to write */
        if(H5Sselect_hyperslab(f_sid, H5S_SELECT_SET, start, NULL, count, NULL) < 0){
        HDfprintf(stderr, "Failed H5Sselect_hyperslab\n");
            return -1;
    }

        /* Write plane to the dataset */
        if(H5Dwrite(dsid, UC_DATATYPE, m_sid, f_sid, H5P_DEFAULT, buffer) < 0){
        HDfprintf(stderr, "Failed H5Dwrite\n");
            return -1;
    }

    /* Flush the dataset for every "chunkplanes_g" planes */
    if(!((i + 1) % (hsize_t)chunkplanes_g)) {
        if(H5Dflush(dsid) < 0) {
        HDfprintf(stderr, "Failed to H5Dflush dataset\n");
        return -1;
        }
    }
    }

    if(H5Dflush(dsid) < 0) {
    HDfprintf(stderr, "Failed to H5Dflush dataset\n");
    return -1;
    }

    /* Enable mdc flushes for the dataset */
    /* Closing the dataset later will enable mdc flushes automatically if this is not done */
    if(disabled)
    if(H5Oenable_mdc_flushes(dsid) < 0) {
        HDfprintf(stderr, "Failed to H5Oenable_mdc_flushes\n");
        return -1;
    }

    /* Done writing. Free/Close all resources including data file */
    HDfree(buffer);

    if(H5Dclose(dsid) < 0){
    HDfprintf(stderr, "Failed to close datasete\n");
    return -1;
    }
    if(H5Sclose(m_sid) < 0){
    HDfprintf(stderr, "Failed to close memory space\n");
    return -1;
    }
    if(H5Sclose(f_sid) < 0){
    HDfprintf(stderr, "Failed to close file space\n");
    return -1;
    }
    if(H5Pclose(fapl) < 0){
    HDfprintf(stderr, "Failed to property list\n");
    return -1;
    }
    if(H5Fclose(fid) < 0){
    HDfprintf(stderr, "Failed to close file id\n");
    return -1;
    }

    return 0;
} /* write_file() */



/* Overall Algorithm:
 * Parse options from user;
 * Generate/pre-created test files needed and close it;
 * Write to the file.
 */
int
main(int argc, char *argv[])
{
    int ret_value = 0;

    /* initialization */
    if(setup_parameters(argc, argv) < 0)
    Hgoto_error(1);

    /* ============*/
    /* Create file */
    /* ============*/
    HDprintf("Creating skeleton data file for testing H5Odisable_mdc_flushes()...\n");
    if(create_file() < 0) {
        HDfprintf(stderr, "***encounter error\n");
        Hgoto_error(1);
    } /* end if */
    else
        HDprintf("File created.\n");

    HDprintf("writing to the file\n");
    if(write_file() < 0) {
        HDfprintf(stderr, "write_file encountered error\n");
        Hgoto_error(1);
    }

done:
    /* Print result and exit */
    if(ret_value != 0)
        HDprintf("Error(s) encountered\n");
    else
        HDprintf("All passed\n");

    return(ret_value);
}

#else /* H5_HAVE_FORK */

int
main(void)
{
    HDfprintf(stderr, "Non-POSIX platform. Skipping.\n");
    HDexit(EXIT_SUCCESS);
} /* end main() */

#endif /* H5_HAVE_FORK */

