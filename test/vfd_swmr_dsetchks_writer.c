/*
 * Copyright by The HDF Group.
 * Copyright by the Board of Trustees of the University of Illinois.
 * All rights reserved.
 *
 * This file is part of HDF5.  The full HDF5 copyright notice, including
 * terms governing use, modification, and redistribution, is contained in
 * the COPYING file, which can be found at the root of the source code
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.
 * If you do not have access to either file, you may request a copy from
 * help@hdfgroup.org.
 */

/*
 *  Purpose: To test chunk operations for chunked dataset specifically:
 *      --chunked datasets with the 5 indexing types:
 *          1. single (dims=max_dims=chunk_dims, default incremental allocation)
 *          2. implicit (dims=max_dims, early allocation, no filter)
 *          3. fixed array (fixed max_dims, default incremental allocation)
 *          4. extensible array (1 unlimited max_dims, default incremental allocation)
 *          5. btree2 (2 unlimited max_dims, default incremental allocation)
 *
 *      --with compression--H5Pset_deflate(dcpl)
 *      --with fill values--H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval);
 *
 *      Types of chunk operations:
 *      -- writes that cover a single chunk
 *      -- writes that cover a partial chunk
 *      -- writes that cover multiple chunks
 *      -- writes that cover multiple partial chunks
 *
 *      For fa, ea, bt2 indexes:
 *      --increase size of dataset dimensions
 *      --decrease size of dataset dimensions
 */
#include "hdf5.h"
#include "testhdf5.h"
#include "vfd_swmr_common.h"

#ifndef H5_HAVE_WIN32_API

#define READER_WAIT_TICKS 4

/* Names for datasets */
#define DSET_SINGLE_NAME   "chunked_single"
#define DSET_IMPLICIT_NAME "chunked_implicit"
#define DSET_FA_NAME       "chunked_fa"
#define DSET_EA_NAME       "chunked_ea"
#define DSET_BT2_NAME      "chunked_bt2"

/* Operations for testing */
#define GWRITES  1 /* Writes that cover a single chunk per write */
#define PWRITES  2 /* Writes that cover a partial chunk per write */
#define TWRITES  3 /* Writes that cover multiple chunks per write  */
#define LWRITES  4 /* Writes that cover multiple partial chunks per write */
#define INCR_EXT 5 /* Increase dataset dimenion sizes */
#define DECR_EXT 6 /* Decrease dataset dimension sizes */

/* Fill values */
#define FILL_INIT    1 /* Fill value used when creating the datasets */
#define FILL_FULL    7 /* Fill value used when writing a full chunk */
#define FILL_PARTIAL 9 /* Fill value used when writing partial chunk(s) */

#define MULTI_CHUNKS 2

/* Structure to hold info for options specified */
typedef struct {
    char         filename[PATH_MAX]; /* File name */
    char         progname[PATH_MAX]; /* Program name */
    hid_t        file;               /* File ID */
    hid_t        filetype;           /* Datatype ID */
    unsigned int update_interval;    /* For -u option */
    unsigned int csteps;             /* For -c <csteps> option */
    bool         use_np;             /* For -N option */
    bool         use_vfd_swmr;       /* For -S option */
    bool         use_filter;         /* For -o option */
    bool         flush_raw_data;     /* For -U option */

    bool single_index;   /* -s option: create a chunked dataset with single chunk index */
    bool implicit_index; /* -i option: create a chunked datasets with implicit chunk index */
    bool fa_index;       /* -f option: create a chunked dataset with fixed array index */
    bool ea_index;       /* -e option: create a chunked dataset with extensible array index */
    bool bt2_index;      /* -r option: create a chunked dataset with version 2 btree index */

    unsigned int rows; /* -m <rows> option for the chunked datasets */
    unsigned int cols; /* -n <cols option for the chunked datasets */

    unsigned int gwrites; /* -s <gwrites> option: writes that cover a single chunk per write */
    unsigned int pwrites; /* -p <pwrites> option: writes that cover a partial chunk per write */
    unsigned int twrites; /* -t <twrites> option: writes that cover multiple chunks per write */
    unsigned int lwrites; /* -l <lwrites> option: writes that cover multiple partial chunks per write */

    unsigned int xincrs; /* -x <xincrs> option */
    unsigned int ydecrs; /* -y <ydecrs> option */
} state_t;

/* Initializations for state_t */
#define ALL_HID_INITIALIZER                                                                                  \
    (state_t)                                                                                                \
    {                                                                                                        \
        .filename = "", .file = H5I_INVALID_HID, .filetype = H5T_NATIVE_UINT32,                              \
        .update_interval = READER_WAIT_TICKS, .csteps = 1, .use_np = true, .use_vfd_swmr = true,             \
        .use_filter = false, .flush_raw_data = true, .single_index = false, .implicit_index = false,         \
        .fa_index = false, .ea_index = false, .bt2_index = false, .rows = 10, .cols = 5, .gwrites = 0,       \
        .pwrites = 0, .twrites = 0, .lwrites = 0, .xincrs = 0, .ydecrs = 0                                   \
    }

/* Structure to hold info for different dataset types */
typedef struct {
    hsize_t chunk_dims[2]; /* Chunk dimensions for all datasets except single_did */
    hsize_t scaled_dims[2];
    hsize_t multi_scaled[2];
    hid_t   single_did;   /* ID for chunked dataset: single index */
    hid_t   implicit_did; /* ID for chunked dataset: implicit index */
    hid_t   fa_did;       /* ID for chunked dataset: fixed array index  */
    hid_t   ea_did;       /* ID for chunked dataset: extensible array index */
    hid_t   bt2_did;      /* ID for chunked dataset: version 2 btree index */
} dsets_state_t;

/* Initializations for dsets_state_t */
#define DSETS_INITIALIZER                                                                                    \
    (dsets_state_t)                                                                                          \
    {                                                                                                        \
        .single_did = H5I_INVALID_HID, .implicit_did = H5I_INVALID_HID, .fa_did = H5I_INVALID_HID,           \
        .ea_did = H5I_INVALID_HID, .bt2_did = H5I_INVALID_HID                                                \
    }

/* Structure to hold info for named pipes */
typedef struct {
    const char *fifo_writer_to_reader; /* Name of fifo for writer to reader */
    const char *fifo_reader_to_writer; /* Name of fifo for reader to writer */
    int         fd_writer_to_reader;   /* File ID for fifo from writer to reader */
    int         fd_reader_to_writer;   /* File ID for fifo from reader to writer */
    int         notify;                /* Value to notify between writer and reader */
    int         verify;                /* Value to verify between writer and reader */
} np_state_t;

/* Initializations for np_state_t */
#define NP_INITIALIZER                                                                                       \
    (np_state_t)                                                                                             \
    {                                                                                                        \
        .fifo_writer_to_reader = "./fifo_dsetchks_writer_to_reader",                                         \
        .fifo_reader_to_writer = "./fifo_dsetchks_reader_to_writer", .fd_writer_to_reader = -1,              \
        .fd_reader_to_writer = -1, .notify = 0, .verify = 0                                                  \
    }

static bool state_init(state_t *, int, char **);

static bool np_init(np_state_t *np, bool writer);
static bool np_close(np_state_t *np, bool writer);
static bool np_writer(bool result, unsigned step, const state_t *s, np_state_t *np,
                      H5F_vfd_swmr_config_t *config);
static bool np_reader(bool result, unsigned step, const state_t *s, np_state_t *np);
static bool np_confirm_verify_notify(int fd, unsigned step, const state_t *s, np_state_t *np);

static bool create_dsets(const state_t *s, dsets_state_t *ds);
static bool open_dsets(const state_t *s, dsets_state_t *ds);
static bool close_dsets(const dsets_state_t *ds);
static void set_chunk_scaled_dims(const state_t *s, dsets_state_t *ds);

static bool perform_dsets_operations(state_t *s, dsets_state_t *ds, H5F_vfd_swmr_config_t *config,
                                     np_state_t *np);

static bool write_dsets_chunks(unsigned action, const state_t *s, const dsets_state_t *ds, unsigned step);
static void setup_selection(unsigned action, unsigned which, const state_t *s, const dsets_state_t *ds,
                            hsize_t *start, hsize_t *stride, hsize_t *count, hsize_t *block);
static void check_set_edge_block(const state_t *s, const dsets_state_t *ds, unsigned i, unsigned j,
                                 hsize_t *block);
static void check_set_partial_block(unsigned action, const hsize_t *dims, hsize_t *block, hsize_t *start);
static bool write_chunks(unsigned action, hid_t did, hid_t tid, hsize_t *start, hsize_t *stride, hsize_t *count,
                       hsize_t *block);
static bool write_dset_single(unsigned action, const state_t *s, const dsets_state_t *ds);

static bool dsets_extent(unsigned action, const state_t *s, const dsets_state_t *ds);
static bool dset_extent_real(unsigned action, hid_t did, const hsize_t *chunk_dims);

static bool verify_dsets_operations(state_t *s, dsets_state_t *ds, H5F_vfd_swmr_config_t *config,
                                    np_state_t *np, bool fileclosed);

static bool verify_dsets_chunks(unsigned action, const state_t *s, const dsets_state_t *ds, unsigned which, bool fileclosed);
static bool verify_chunks(unsigned action, hid_t did, hid_t tid, hsize_t *start, hsize_t *stride,
                             hsize_t *count, hsize_t *block, bool fileclosed, bool flush_raw_data);
static bool verify_dset_single(unsigned action, const state_t *s, const dsets_state_t *ds, bool fileclosed);

static bool verify_dsets_extent(unsigned action, const state_t *s, const dsets_state_t *ds, unsigned which);
static bool verify_dset_extent_real(unsigned action, hid_t did, unsigned rows, unsigned cols, unsigned which);

static const hid_t badhid = H5I_INVALID_HID;

static void
usage(const char *progname)
{
    fprintf(stderr,
            "usage: %s \n"
            "           [-s] [-i] [-f] [-e] [-r]\n"
            "           [-m rows] [-n cols]\n"
            "           [-g gwrites] [-p pwrites] [-t twrites] [-l lwrites]\n"
            "           [-x xincrs] [-y decrs]\n"
            "           [-u nticks] [-c csteps] [-U] [-S] [-N] [-q] [-b] [-o]\n",
            progname);

    fprintf(
        stderr,
        "\n"
        "-s:              create a 2-d chunked dataset with single index\n"
        "-i:              create a 2-d chunked dataset with implicit index\n"
        "-f:              create a 2-d chunked dataset with fixed array index\n"
        "-e:              create a 2-d chunked dataset with extensible array index\n"
        "-r:              create a 2-d chunked dataset with v2 btree index\n"
        "-m rows:         # of <rows> rows for the datasets\n"
        "-n cols:         # of <cols> columns for the chunked datasets\n"
        "-g gwrites:      perform <gwrites> writes that cover a single chunk per write to datasets\n"
        "-p pwrites:      perform <pwrites> writes that cover a single partial chunk per write to datasets\n"
        "-t twrites:      perform <twrites> writes that cover multiple chunks per write to datasets\n"
        "-l lwrites:      perform <lwrites> writes that cover multiple partial chunks per write to datasets\n"
        "-x xincrs:       increase dataset dimension size by 1 for <xincrs> times to datasets\n"
        "-y ydecrs:       decrease dataset dimension size by 1 for <ydecrs> times to datasets\n"
        "-u nticks:       `nticks` ticks for the reader to wait before verification\n"
        "                 (default is 4)\n"
        "-c csteps:       `csteps` steps communication interval between reader and writer\n"
        "                 (default is 1)\n"
        "-U:              disable flush of raw data (default is flushing raw data)\n"
        "-S:              do not use VFD SWMR\n"
        "-N:              do not use named pipes for test synchronization\n"
        "-q:              silence printouts, few messages\n"
        "-b:              write data in big-endian byte order\n"
        "                 (default is H5T_NATIVE_UINT32)\n\n"
        "-o:              enable compression (deflate filter) for the datasets\n");

    fprintf(
        stderr,
        "\n"
        "Note:\n"
        "1. Require to specify at least -s, -i, -f, -e or -r option\n"
        "2. -m and -n options: <rows> and <cols> have to be > 0.\n"
        "3. The chunk size for datasets is <rows>/2 by <cols/2>\n"
        "4. The maximum dimension for dataset with fixed array index is: <rows>*2 by <cols>*2\n"
        "5. Writes to dataset with single index:\n"
        "     -g and -t options will be the same: write one single chunk\n"
        "     -p and -l options will be the same: write one single partial chunk\n"
        "6. Writes to dataset with implicit/fixed array/extensible array/v2 btree index:\n"
        "   i)  Partial chunk writes to datasets via -p or -l option:\n"
        "       Apply only to a single or a multiple chunk with size > 2 in both dimensions\n"
        "       Otherwise, the whole chunk (single or multiple) is written\n"
        "   ii) Multiple chunk writes to datasets via -t or -l option:\n"
        "       Will expand the chunk size by 2 for both dimensions\n"
        "7. Increase/decrease dataset dimension sizes to datasets:\n"
        "     Apply only for datasets with fixed array/extensible array/v2 btree index\n"
        "     -x option: for dataset with fixed array index, the increase cannot exceed maximum dimension\n"
        "     -y option: the decrease cannot go below the dataset's chunk size\n"
        "8. -c <csteps> option cannot exceed the input for the following options:\n"
        "     -g <gwrites> or -p <pwrites> or\n"
        "     -t <twrites> or -l <lwrites> or\n"
        "     -x <xincrs> or -y <ydecrs>\n"
        "\n");

    exit(EXIT_FAILURE);
} /* usage() */

/*
 * Initialize option info in state_t
 */
static bool
state_init(state_t *s, int argc, char **argv)
{
    unsigned long tmp;
    int           ch;
    char          *tfile = NULL;
    char *        end;

    *s = ALL_HID_INITIALIZER;

    if (H5_basename(argv[0], &tfile) < 0) {
        HDprintf("H5_basename failed\n"); 
        TEST_ERROR
    }

    esnprintf(s->progname, sizeof(s->progname), "%s", tfile);

    if (tfile)
        HDfree(tfile);

    while ((ch = getopt(argc, argv, "siferom:n:x:y:g:p:t:l:bqSNUu:c:")) != -1) {
        switch (ch) {

            case 's': /* A chunked dataset with single index */
                s->single_index = true;
                break;

            case 'i': /* A chunked dataset with implicit index */
                s->implicit_index = true;
                break;

            case 'f': /* A chunked dataset with fixed array index */
                s->fa_index = true;
                break;

            case 'e': /* A chunked dataset with extensible array index */
                s->ea_index = true;
                break;

            case 'r': /* A chunked dataset with version 2 btree index */
                s->bt2_index = true;
                break;

            case 'o': /* A chunked dataset with version 2 btree index */
                s->use_filter = true;
                break;

            case 'q': /* Be quiet: few/no progress messages */
                verbosity = 0;
                break;

            case 'b': /* Write data in big-endian byte order */
                s->filetype = H5T_STD_U32BE;
                break;

            case 'S': /* Disable VFD SWMR */
                s->use_vfd_swmr = false;
                break;

            case 'U':   /* Disable flush of raw data */
                s->flush_raw_data = false;
                break;

            case 'N': /* Disable named pipes synchronization */
                s->use_np = false;
                break;

            case 'm': /* # of rows for datasets */
            case 'n': /* # of cols for datasets */
            case 'x': /* Increase by 1 for <xincrs> times */
            case 'y': /* Decrease by 1 for <ydecdrs> times */
            case 'g': /* # of writes that cover a single chunk per write */
            case 'p': /* # of writes that cover a single partial chunk per write */
            case 't': /* # of writes that cover multiple chunks per write */
            case 'l': /* # of writes that cover multiple partial chunks per write */
            case 'u': /* Ticks for reader to wait before verification */
            case 'c': /* Communication interval */
                errno = 0;
                tmp   = strtoul(optarg, &end, 0);
                if (end == optarg || *end != '\0') {
                    printf("couldn't parse `-%c` argument `%s`\n", ch, optarg);
                    TEST_ERROR;
                }
                else if (errno != 0) {
                    printf("couldn't parse `-%c` argument `%s`\n", ch, optarg);
                    TEST_ERROR;
                }
                else if (tmp > UINT_MAX) {
                    printf("`-%c` argument `%lu` too large\n", ch, tmp);
                    TEST_ERROR;
                }

                if (ch == 'm')
                    s->rows = (unsigned)tmp;
                else if (ch == 'n')
                    s->cols = (unsigned)tmp;
                else if (ch == 'x')
                    s->xincrs = (unsigned)tmp;
                else if (ch == 'y')
                    s->ydecrs = (unsigned)tmp;
                else if (ch == 'g')
                    s->gwrites = (unsigned)tmp;
                else if (ch == 'p')
                    s->pwrites = (unsigned)tmp;
                else if (ch == 't')
                    s->twrites = (unsigned)tmp;
                else if (ch == 'l')
                    s->lwrites = (unsigned)tmp;
                else if (ch == 'u')
                    s->update_interval = (unsigned)tmp;
                else if (ch == 'c')
                    s->csteps = (unsigned)tmp;

                break;

            case '?':
            default:
                usage(s->progname);
                break;
        }
    }
    argc -= optind;
    argv += optind;

    /* Require to specify at least -s or -i or -f or -e or -r option */
    if (!s->single_index && !s->implicit_index && !s->fa_index && !s->ea_index && !s->bt2_index) {
        printf("Require to specify at least -s or -i or -f or -e or -r option\n");
        usage(s->progname);
        goto error;
    }

    /* -x or -y option only apply to dataset with fixed/extensible array/v2 btree index */
    if ((s->single_index || s->implicit_index) && (s->xincrs || s->ydecrs)) {
        printf("-x or -y option not applicable to dataset with single or implicit index\n");
        usage(s->progname);
        goto error;
    }

    /* rows and cols cannot be zero */
    if (s->rows == 0 || s->cols == 0) {
        printf("-m <rows> or -n <cols> cannot be zero\n");
        TEST_ERROR;
    }

    /* -c <csteps> cannot be zero */
    if (!s->csteps) {
        printf("communication interval cannot be zero\n");
        TEST_ERROR;
    }

    /* -c <csteps> and -g <gwrites> options */
    if (s->gwrites && s->csteps > s->gwrites) {
        printf("communication interval with -g <gwrites> is out of bounds\n");
        TEST_ERROR;
    }

    /* -c <csteps> and -p <pwrites> options */
    if (s->pwrites && s->csteps > s->pwrites) {
        printf("communication interval with -p <pwrites> is out of bounds\n");
        TEST_ERROR;
    }

    /* -c <csteps> and -t <twrites> options */
    if (s->twrites && s->csteps > s->twrites) {
        printf("communication interval with -t <twrites> is out of bounds\n");
        TEST_ERROR;
    }

    /* -c <csteps> and -l <lwrites> options */
    if (s->lwrites && s->csteps > s->lwrites) {
        printf("communication interval with -l <lwrites> is out of bounds\n");
        TEST_ERROR;
    }

    /* -c <csteps> and -x <xincrs> options */
    if (s->xincrs && s->csteps > s->xincrs) {
        printf("communication interval with -x <xincrs> is out of bounds\n");
        TEST_ERROR;
    }

    /* -c <csteps> and -y <ydecrs> options */
    if (s->ydecrs && s->csteps > s->ydecrs) {
        printf("communication interval with -y <ydecrs> is out of bounds\n");
        TEST_ERROR;
    }

    /* The test file name */
    esnprintf(s->filename, sizeof(s->filename), "vfd_swmr_dsetchks.h5");

    return true;

error:
    if (tfile)
        HDfree(tfile);
    return false;

} /* state_init() */

/*
 *  Create the specified datasets:
 *  --2-dimensional chunked datasets
 *  --chunk dimension is rows/2 by cols/2
 *  --fill value is FILL_INIT
 *  --deflate filter if specified
 *  --dataset with single index if specified
 *  --dataset with implicit index if specified
 *  --dataset with fixeda array index if specified
 *  --dataset with extensible array index if specified
 *  --dataset with bt2 index if specified
 */
static bool
create_dsets(const state_t *s, dsets_state_t *ds)
{
    hid_t    dcpl  = badhid;
    hid_t    dcpl2 = badhid;
    hid_t    sid   = badhid;
    hsize_t  dims[2];
    unsigned fillval = FILL_INIT;

    *ds = DSETS_INITIALIZER;
    set_chunk_scaled_dims(s, ds);

    dims[0] = s->rows;
    dims[1] = s->cols;

    /* Create dataset creation property list */
    /* Set properties in dcpl that are common for all the datasets */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
        printf("H5Pcreate failed\n");
        TEST_ERROR;
    }

    /* Set to chunked layout */
    if (H5Pset_layout(dcpl, H5D_CHUNKED) < 0) {
        printf("H5Pset_layout failed\n");
        TEST_ERROR;
    }

    /* Set fill value to FILL_INIT */
    if (H5Pset_fill_value(dcpl, s->filetype, &fillval) < 0) {
        printf("H5Pset_fill_value failed\n");
        goto error;
    }

    /* Set to use filter as specified */
    if (s->use_filter) {
        if (H5Pset_deflate(dcpl, 5) < 0) {
            printf("H5Pset_deflate failed\n");
            goto error;
        }
    }

    /* Create 2-D chunked dataset with single index */
    /* Chunked, dims=max_dims=chunk_dims */
    if (s->single_index) {

        if ((dcpl2 = H5Pcopy(dcpl)) < 0) {
            printf("H5Tcopy failed\n");
            TEST_ERROR;
        }

        if (H5Pset_chunk(dcpl2, 2, dims) < 0) {
            printf("H5Pset_chunk failed\n");
            TEST_ERROR;
        }

        if ((sid = H5Screate_simple(2, dims, dims)) < 0) {
            printf("H5Screate_simple failed\n");
            TEST_ERROR;
        }

        /* Create the chunked dataset: single index */
        if ((ds->single_did = H5Dcreate2(s->file, DSET_SINGLE_NAME, s->filetype, sid, H5P_DEFAULT, dcpl2,
                                         H5P_DEFAULT)) < 0) {
            printf("H5Dcreate2 chunked dataset:single index failed\n");
            TEST_ERROR;
        }

        if (H5Pclose(dcpl2) < 0) {
            printf("H5Pclose failed\n");
            TEST_ERROR;
        }

        if (H5Sclose(sid) < 0) {
            printf("H5Sclose failed\n");
            TEST_ERROR;
        }
    }

    /* Chunk size is common for datasets with implicit/fa/ea/bt2 index */
    if (H5Pset_chunk(dcpl, 2, ds->chunk_dims) < 0) {
        printf("H5Pset_chunk failed\n");
        TEST_ERROR;
    }

    /* Create 2-D chunked dataset with implicit index */
    /* Chunked, dims=max_dims, early allocation */
    if (s->implicit_index) {

        if ((dcpl2 = H5Pcopy(dcpl)) < 0) {
            printf("H5Pcopy failed\n");
            TEST_ERROR;
        }

        if (H5Pset_alloc_time(dcpl2, H5D_ALLOC_TIME_EARLY) < 0) {
            printf("H5Pset_alloc_time\n");
            TEST_ERROR;
        }

        if ((sid = H5Screate_simple(2, dims, dims)) < 0) {
            printf("H5Screate_simple failed\n");
            TEST_ERROR;
        }

        /* Create the chunked dataset: implicit index */
        if ((ds->implicit_did = H5Dcreate2(s->file, DSET_IMPLICIT_NAME, s->filetype, sid, H5P_DEFAULT, dcpl2,
                                           H5P_DEFAULT)) < 0) {
            printf("H5Dcreate2 chunked dataset:implicit index failed\n");
            TEST_ERROR;
        }

        if (H5Pclose(dcpl2) < 0) {
            printf("H5Pclose failed\n");
            TEST_ERROR;
        }

        if (H5Sclose(sid) < 0) {
            printf("H5Sclose failed\n");
            TEST_ERROR;
        }
    }

    /* Create 2-D chunked dataset with fixed array index */
    /* Chunked, fixed max_dims */
    if (s->fa_index) {
        hsize_t max_dims[2];

        max_dims[0] = dims[0] * 2;
        max_dims[1] = dims[1] * 2;

        if ((sid = H5Screate_simple(2, dims, max_dims)) < 0) {
            printf("H5Screate_simple failed\n");
            TEST_ERROR;
        }

        /* Create the chunked dataset (fixed array index) with the named datatype */
        if ((ds->fa_did =
                 H5Dcreate2(s->file, DSET_FA_NAME, s->filetype, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) {
            printf("H5Dcreate2 chunked dataset: fa index failed\n");
            TEST_ERROR;
        }

        if (H5Sclose(sid) < 0) {
            printf("H5Sclose failed\n");
            TEST_ERROR;
        }
    }

    /* Create 2-D chunked dataset with extensible array index */
    /* Chunked, 1 unlimited max_dims */
    if (s->ea_index) {
        hsize_t max_dims[2];

        max_dims[0] = dims[0] * 2;
        max_dims[1] = H5S_UNLIMITED;

        if ((sid = H5Screate_simple(2, dims, max_dims)) < 0) {
            printf("H5Screate_simple failed\n");
            TEST_ERROR;
        }

        /* Create the chunked dataset (extensible array index) with the named datatype */
        if ((ds->ea_did =
                 H5Dcreate2(s->file, DSET_EA_NAME, s->filetype, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) {
            printf("H5Dcreate2 chunked dataset: ea index failed\n");
            TEST_ERROR;
        }

        if (H5Sclose(sid) < 0) {
            printf("H5Sclose failed\n");
            TEST_ERROR;
        }
    }

    /* Create 2-D chunked dataset with bt2 index */
    /* Chunked, 2 unlimited max_dims */
    if (s->bt2_index) {
        hsize_t max_dims[2];

        max_dims[0] = max_dims[1] = H5S_UNLIMITED;

        if ((sid = H5Screate_simple(2, dims, max_dims)) < 0) {
            printf("H5Screate_simple failed\n");
            TEST_ERROR;
        }

        /* Create the chunked dataset (btree2 index) with the named datatype */
        if ((ds->bt2_did =
                 H5Dcreate2(s->file, DSET_BT2_NAME, s->filetype, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) {
            printf("H5Dcreate2 chunked dataset: bt2 index failed\n");
            TEST_ERROR;
        }

        if (H5Sclose(sid) < 0) {
            printf("H5Sclose failed\n");
            TEST_ERROR;
        }
    }

    if (H5Pclose(dcpl) < 0) {
        printf("H5Pclose failed\n");
        TEST_ERROR;
    }

    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(dcpl);
        H5Pclose(dcpl2);
        H5Sclose(sid);
        H5Dclose(ds->single_did);
        H5Dclose(ds->implicit_did);
        H5Dclose(ds->fa_did);
        H5Dclose(ds->ea_did);
        H5Dclose(ds->bt2_did);
    }
    H5E_END_TRY;

    return false;

} /* create_dsets() */

/*
 * Open the specified datasets.
 */
static bool
open_dsets(const state_t *s, dsets_state_t *ds)
{
    *ds = DSETS_INITIALIZER;
    set_chunk_scaled_dims(s, ds);

    /* Dataset with single index */
    if (s->single_index) {
        if ((ds->single_did = H5Dopen2(s->file, DSET_SINGLE_NAME, H5P_DEFAULT)) < 0) {
            printf("H5Dopen dataset with single index failed\n");
            TEST_ERROR;
        }
    }

    /* Dataset with implicit index */
    if (s->implicit_index) {
        if ((ds->implicit_did = H5Dopen2(s->file, DSET_IMPLICIT_NAME, H5P_DEFAULT)) < 0) {
            printf("H5Dopen dataset with implicit index failed\n");
            TEST_ERROR;
        }
    }

    /* Dataset with fixed array index */
    if (s->fa_index) {
        if ((ds->fa_did = H5Dopen2(s->file, DSET_FA_NAME, H5P_DEFAULT)) < 0) {
            printf("H5Dopen dataset with fa index failed\n");
            TEST_ERROR;
        }
    }

    /* Dataset with extensible array index */
    if (s->ea_index) {
        if ((ds->ea_did = H5Dopen2(s->file, DSET_EA_NAME, H5P_DEFAULT)) < 0) {
            printf("H5Dopen dataset with ea index failed\n");
            TEST_ERROR;
        }
    }

    /* Dataset with v2 btree index */
    if (s->bt2_index) {
        if ((ds->bt2_did = H5Dopen2(s->file, DSET_BT2_NAME, H5P_DEFAULT)) < 0) {
            printf("H5Dopen dataset with ea index failed\n");
            TEST_ERROR;
        }
    }

    return true;

error:
    return false;

} /* open_dsets() */

/*
 * Initialize the following 3 fields in ds:
 * --chunk_dims[]: chunk sizes for datasets with implicit/fa/ea/bt2 index
 * --scaled_dims[]: # of chunks in x and y dimensions
 * --multi_scaled[]: # of multiple chunks (2 chunks) in x and y dimensions
 */
static void
set_chunk_scaled_dims(const state_t *s, dsets_state_t *ds)
{
    /* Default chunk size is s->rows/2 or s->cols/2 but not less than 1 */
    ds->chunk_dims[0] = MAX(1, s->rows / 2);
    ds->chunk_dims[1] = MAX(1, s->cols / 2);

    /* # of chunks in x and y dimensions */
    ds->scaled_dims[0] = (s->rows + ds->chunk_dims[0] - 1) / ds->chunk_dims[0];
    ds->scaled_dims[1] = (s->cols + ds->chunk_dims[1] - 1) / ds->chunk_dims[1];

    /* # of "multiple chunks" (2-chunks) in x and y dimensions */
    ds->multi_scaled[0] = (ds->scaled_dims[0] + MULTI_CHUNKS - 1) / MULTI_CHUNKS;
    ds->multi_scaled[1] = (ds->scaled_dims[1] + MULTI_CHUNKS - 1) / MULTI_CHUNKS;

} /* set_chunk_scaled_dims() */

/*
 * Close the specified datasets
 */
static bool
close_dsets(const dsets_state_t *ds)
{
    /* Close dataset with single index */
    if (ds->single_did != badhid && H5Dclose(ds->single_did) < 0) {
        printf("close_dset_real() dataset: single index failed\n");
        TEST_ERROR;
    }

    /* Close dataset with implicit index */
    if (ds->implicit_did != badhid && H5Dclose(ds->implicit_did) < 0) {
        printf("close_dset_real() dataset: implicit index failed\n");
        TEST_ERROR;
    }

    /* Close dataset with fixed array index */
    if (ds->fa_did != badhid && H5Dclose(ds->fa_did) < 0) {
        printf("close_dset_real() dataset: fa index failed\n");
        TEST_ERROR;
    }

    /* Close dataset with extensible array index */
    if (ds->ea_did != badhid && H5Dclose(ds->ea_did) < 0) {
        printf("close_dset_real() : ea index failed\n");
        TEST_ERROR;
    }

    /* Close dataset with v2 btree index */
    if (ds->bt2_did != badhid && H5Dclose(ds->bt2_did) < 0) {
        printf("close_dset_real() dataset: bt2 index failed\n");
        TEST_ERROR;
    }

    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(ds->single_did);
        H5Dclose(ds->implicit_did);
        H5Dclose(ds->fa_did);
        H5Dclose(ds->ea_did);
        H5Dclose(ds->bt2_did);
    }
    H5E_END_TRY;

    return false;

} /* close_dsets() */

/*
 *  Writer
 */

/*
 * Perform the operations for the specified datasets:
 *
 * Dataset with single index:
 * --only 1 write is performed because this dataset only has a single chunk
 *
 * Dataset with implicit/fixed array/extensible array/version 2 btree index:
 * --GWRITES: writes that cover a single chunk per write
 * --PWRITES: writes that cover a partial chunk per write
 * --TWRITES: writes that cover multiple chunks per write
 * --LWRITES: writes that cover multiple partial chunks per write
 *
 * Dataset with fixed array/extensible array/version 2 btree index:
 * --INCR_EXT: increase dataset dimension sizes
 * --DECR_EXT: decrease dataset dimenions sizes
 */
static bool
perform_dsets_operations(state_t *s, dsets_state_t *ds, H5F_vfd_swmr_config_t *config, np_state_t *np)
{
    unsigned step;
    unsigned allowed_writes;
    bool     result;

    /* Dataset with single index */
    if (s->single_index) {
        /* Perform single full chunk write */
        /* gwrites and twrites are the same */
        /* Doesn't matter how many writes, only perform once */
        if (s->gwrites || s->twrites) {
            dbgf(2, "Perform single full chunk write to dataset with single index; only perform 1 write\n");

            result = write_dset_single(GWRITES, s, ds);

            if (s->use_np && !np_writer(result, 0, s, np, config)) {
                printf("np_writer() for addition failed\n");
                TEST_ERROR;
            }
        }

        /* Perform a single partial chunk write */
        /* pwrites and lwrites are the same */
        /* Doesn't matter how many writes, only perform once */
        if (s->pwrites || s->lwrites) {
            dbgf(2,
                 "Perform single partial chunk write to dataset with single index; only perform 1 write\n");

            result = write_dset_single(PWRITES, s, ds);

            if (s->use_np && !np_writer(result, 0, s, np, config)) {
                printf("np_writer() for addition failed\n");
                TEST_ERROR;
            }
        }
    }

    /* Datasets with implicit/fa/ea/bt2 index */
    if (s->implicit_index || s->fa_index || s->ea_index || s->bt2_index) {

        /* Perform single full chunk writes */
        if (s->gwrites) {
            allowed_writes = (unsigned)(ds->scaled_dims[0] * ds->scaled_dims[1]);
            dbgf(2, "The allowed -g writes is %u; you specify %u writes\n", allowed_writes, s->gwrites);

            for (step = 0; (step < s->gwrites && step < allowed_writes); step++) {
                dbgf(2, "Perform single full chunk writes #%u to datasets with implicit/fa/ea/bt2 index\n",
                     step);

                result = write_dsets_chunks(GWRITES, s, ds, step);

                if (s->use_np && !np_writer(result, step, s, np, config)) {
                    printf("np_writer() for single full chunk writes failed\n");
                    TEST_ERROR;
                }
            }
        }

        /* Perform single partial chunk writes */
        if (s->pwrites) {
            allowed_writes = (unsigned)(ds->scaled_dims[0] * ds->scaled_dims[1]);
            dbgf(2, "The allowed -p writes is %u; you specify %u writes\n", allowed_writes, s->pwrites);

            for (step = 0; (step < s->pwrites && step < allowed_writes); step++) {
                dbgf(2, "Perform single partial chunk writes #%u to datasets with implicit/fa/ea/bt2 index\n",
                     step);

                result = write_dsets_chunks(PWRITES, s, ds, step);

                if (s->use_np && !np_writer(result, step, s, np, config)) {
                    printf("np_writer() for partial single chunk writes failed\n");
                    TEST_ERROR;
                }
            }
        }

        /* Perform multiple full chunk writes */
        if (s->twrites) {
            allowed_writes = (unsigned)(ds->multi_scaled[0] * ds->multi_scaled[1]);
            dbgf(2, "The allowed -t writes is %u; you specify %u writes\n", allowed_writes, s->twrites);

            for (step = 0; (step < s->twrites && step < allowed_writes); step++) {
                dbgf(2, "Perform multiple full chunk writes #%u to datasets with implicit/fa/ea/bt2 index\n",
                     step);

                result = write_dsets_chunks(TWRITES, s, ds, step);

                if (s->use_np && !np_writer(result, step, s, np, config)) {
                    printf("np_writer() for multiple full chunk writes failed\n");
                    TEST_ERROR;
                }
            }
        }

        /* Perform multiple partial chunk writes */
        if (s->lwrites) {
            allowed_writes = (unsigned)(ds->multi_scaled[0] * ds->multi_scaled[1]);
            dbgf(2, "The allowed -l writes is %u; you specify %u writes\n", allowed_writes, s->lwrites);

            for (step = 0; (step < s->lwrites && step < allowed_writes); step++) {
                dbgf(2,
                     "Perform multiple partial chunk writes #%u to datasets with implicit/fa/ea/bt2 index\n",
                     step);

                result = write_dsets_chunks(LWRITES, s, ds, step);

                if (s->use_np && !np_writer(result, step, s, np, config)) {
                    printf("np_writer() for multiple partial chunk writes failed\n");
                    TEST_ERROR;
                }
            }
        }

        /* Increase dataset dimensions: apply only to fa/ea/bt2 index */
        if (!s->implicit_index && s->xincrs) {
            for (step = 0; step < s->xincrs; step++) {
                dbgf(2, "Increase dataset dimension sizes by %u for datasets with fa/ea/bt2 index\n",
                     step + 1);

                result = dsets_extent(INCR_EXT, s, ds);

                if (s->use_np && !np_writer(result, step, s, np, config)) {
                    printf("np_writer() for increasing dimension sizes failed\n");
                    TEST_ERROR;
                }
            }
        }

        /* Decrease dataset dimensions: apply only to fa/ea/bt2 index */
        if (!s->implicit_index && s->ydecrs) {
            for (step = 0; step < s->ydecrs; step++) {
                dbgf(2, "Decrease dataset dimension sizes by %u for datasets with fa/ea/bt2 index\n",
                     step + 1);

                result = dsets_extent(DECR_EXT, s, ds);

                if (s->use_np && !np_writer(result, step, s, np, config)) {
                    printf("np_writer() for decreasing dimension sizes failed\n");
                    TEST_ERROR;
                }
            }
        }
    }

    return true;

error:
    return false;

} /* perform_dsets_operations() */

/*
 * Perform the "action" for each of the specified datasets:
 *      GWRITES: perform `which` write that covers a single chunk
 *      PWRITES: perform `which` write that covers a partial chunk
 *      TWRITES: perform `which` write that covers multiple chunks
 *      LWRITEs: perform `which` write that covers multiple partial chunks
 */
static bool
write_dsets_chunks(unsigned action, const state_t *s, const dsets_state_t *ds, unsigned which)
{
    hsize_t start[2]  = {0, 0};
    hsize_t stride[2] = {0, 0};
    hsize_t count[2]  = {0, 0};
    hsize_t block[2]  = {0, 0};

    HDassert(s->implicit_index || s->fa_index || s->ea_index || s->bt2_index);

    /* Set up selection info according to the specified action */
    setup_selection(action, which, s, ds, start, stride, count, block);

    if (s->implicit_index) {
        if (!write_chunks(action, ds->implicit_did, s->filetype, start, stride, count, block)) {
            printf("H5Dwrite to chunked dataset: implicit index dataset failed\n");
            TEST_ERROR;
        }
    }

    if (s->fa_index) {
        if (!write_chunks(action, ds->fa_did, s->filetype, start, stride, count, block)) {
            printf("H5Dwrite to chunked dataset: fa index dataset failed\n");
            TEST_ERROR;
        }
    }

    if (s->ea_index) {
        if (!write_chunks(action, ds->ea_did, s->filetype, start, stride, count, block)) {
            printf("H5Dwrite to chunked dataset: ea index dataset failed\n");
            TEST_ERROR;
        }
    }

    if (s->bt2_index) {
        if (!write_chunks(action, ds->bt2_did, s->filetype, start, stride, count, block)) {
            printf("H5Dwrite to chunked dataset: bt2 index dataset failed\n");
            TEST_ERROR;
        }
    }

    return true;

error:
    return false;

} /* write_dsets_chunks() */

/*
 *  Set up selection info: start, stride, count, block
 */
static void
setup_selection(unsigned action, unsigned which, const state_t *s, const dsets_state_t *ds, hsize_t *start,
                hsize_t *stride, hsize_t *count, hsize_t *block)
{
    unsigned i, j, m, n;
    bool     end = false;
    hsize_t  chunk_dims[2];

    HDassert(action == GWRITES || action == PWRITES || action == TWRITES || action == LWRITES);

    count[0]  = 1;
    count[1]  = 1;
    stride[0] = 1;
    stride[1] = 1;

    /* Single or multiple chunk writes */
    if (action == GWRITES || action == PWRITES) {
        block[0] = chunk_dims[0] = ds->chunk_dims[0];
        block[1] = chunk_dims[1] = ds->chunk_dims[1];

        for (i = 0; i < ds->scaled_dims[0] && !end; i++) {
            for (j = 0; j < ds->scaled_dims[1]; j++) {

                /* Determine which chunk to write */
                if (which == (i * ds->scaled_dims[1] + j)) {
                    start[0] = i * ds->chunk_dims[0];
                    start[1] = j * ds->chunk_dims[1];

                    /* If an edge chunk, determine the block size */
                    check_set_edge_block(s, ds, i, j, block);

                    end = true;
                    break;
                } /* end if */
            }     /* end for */
        }         /* end for */

        /* Check and set partial chunk write */
        if (action == PWRITES)
            check_set_partial_block(action, chunk_dims, block, start);

        /* Partial or multiple partial chunk writes */
    }
    else if (action == TWRITES || action == LWRITES) {
        /* Multiple chunk writes: the block covers 2 chunks in each dimension */
        block[0] = chunk_dims[0] = ds->chunk_dims[0] * 2;
        block[1] = chunk_dims[1] = ds->chunk_dims[1] * 2;

        for (i = 0, m = 0; i < ds->scaled_dims[0] && !end; i += 2, m++) {
            for (j = 0, n = 0; j < ds->scaled_dims[1]; j += 2, n++) {
                if (which == (m * ds->multi_scaled[1] + n)) {
                    start[0] = i * ds->chunk_dims[0];
                    start[1] = j * ds->chunk_dims[1];

                    /* If an edge chunk, determine the block size */
                    check_set_edge_block(s, ds, i, j, block);

                    end = true;
                    break;

                } /* end if */
            }     /* end for */
        }         /* end for */

        /* Check and set multiple partial chunk write */
        if (action == LWRITES)
            check_set_partial_block(action, chunk_dims, block, start);
    }

} /* setup_selection() */

/*
 * Check if "i" or "j" is an edge block.
 * If so, determine the block size.
 */
static void
check_set_edge_block(const state_t *s, const dsets_state_t *ds, unsigned i, unsigned j, hsize_t *block)
{

    if (i == (ds->scaled_dims[0] - 1)) {
        if ((ds->scaled_dims[0] * ds->chunk_dims[0]) >= s->rows)
            block[0] = s->rows - i * ds->chunk_dims[0];
    }

    if (j == (ds->scaled_dims[1] - 1)) {
        if ((ds->scaled_dims[1] * ds->chunk_dims[1]) >= s->cols)
            block[1] = s->cols - (j * ds->chunk_dims[1]);
    }

} /* check_set_edge_block() */

/*
 * Determine the starting offset and the partial block size if the block is:
 *  --a full chunk or a multiple full chunks
 *  --the block size is at least 2
 * Otherwise, nothing is done i.e. the whole block is applied
 */
static void
check_set_partial_block(unsigned action, const hsize_t *chunk_dims, hsize_t *block, hsize_t *start)
{
    HDassert(action == PWRITES || action == LWRITES);

    /* Apply only to full chunk or multi full chunks with block size > 2 */
    if (block[0] == chunk_dims[0] && block[1] == chunk_dims[1]) {

        if (block[0] > 2) {
            start[0] += 1;
            block[0] -= 2;
        };

        if (block[1] > 2) {
            start[1] += 1;
            block[1] -= 2;
        };
    }

} /* check_set_partial_block() */

/*
 * Make the selection and then write to the dataset.
 */
static bool
write_chunks(unsigned action, hid_t did, hid_t tid, hsize_t *start, hsize_t *stride, hsize_t *count,
           hsize_t *block)
{
    hid_t         sid     = badhid;
    hid_t         mem_sid = badhid;
    hsize_t       mem_dims[2];
    unsigned int *buf = NULL;
    unsigned      i;

    if ((sid = H5Dget_space(did)) < 0) {
        printf("H5Sget_space failed\n");
        TEST_ERROR;
    }

    if (H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, stride, count, block) < 0) {
        printf("H5Sselect_hyperslab failed\n");
        TEST_ERROR;
    }

    mem_dims[0] = block[0];
    mem_dims[1] = block[1];

    if ((mem_sid = H5Screate_simple(2, mem_dims, NULL)) < 0) {
        printf("H5Screate_simple failed\n");
        TEST_ERROR;
    }

    /* Allocate the buffer for writing */
    if ((buf = HDmalloc(block[0] * block[1] * sizeof(unsigned int))) == NULL) {
        printf("HDmalloc failed\n");
        TEST_ERROR;
    }

    /* Fill the value to be written depending on full or partial writes */
    for (i = 0; i < (block[0] * block[1]); i++) {
        if (action == GWRITES || action == TWRITES)
            buf[i] = FILL_FULL;
        else
            buf[i] = FILL_PARTIAL;
    }

    if (H5Dwrite(did, tid, mem_sid, sid, H5P_DEFAULT, buf) < 0) {
        printf("H5Dwrite failed\n");
        TEST_ERROR;
    }

    if (H5Sclose(sid) < 0) {
        printf("H5Sclose failed\n");
        TEST_ERROR;
    }

    if (buf)
        HDfree(buf);

    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(sid);
        H5Sclose(mem_sid);
    }
    H5E_END_TRY;

    if (buf)
        HDfree(buf);

    return false;

} /* write_chunks() */

/*
 * Increase or decrease the dimenion sizes for the specified datasets.
 */
static bool
dsets_extent(unsigned action, const state_t *s, const dsets_state_t *ds)
{
    unsigned nerrors = 0;
    bool     ret     = true;

    HDassert(s->fa_index || s->ea_index || s->bt2_index);
    HDassert(action == INCR_EXT || action == DECR_EXT);

    if (s->fa_index) {
        dbgf(2, "Setting dataset extent for FA dataset\n");
        if (!dset_extent_real(action, ds->fa_did, ds->chunk_dims)) {
            ++nerrors;
        }
    }

    if (s->ea_index) {
        dbgf(2, "Setting dataset extent for EA dataset\n");
        if (!dset_extent_real(action, ds->ea_did, ds->chunk_dims))
            ++nerrors;
    }

    if (s->bt2_index) {
        dbgf(2, "Setting dataset extent for BT2 dataset\n");
        if (!dset_extent_real(action, ds->bt2_did, ds->chunk_dims))
            ++nerrors;
    }

    if (nerrors)
        ret = false;

    return (ret);

} /* dsets_extent() */

/*
 * Perform the real work of increasing/decreasing the dataset dimension sizes
 */
static bool
dset_extent_real(unsigned action, hid_t did, const hsize_t *chunk_dims)
{
    hsize_t dims[2];
    hsize_t max_dims[2];
    hsize_t new[2];
    hid_t sid = badhid;

    if ((sid = H5Dget_space(did)) < 0) {
        printf("H5Sget_space failed\n");
        TEST_ERROR;
    }

    if (H5Sget_simple_extent_dims(sid, dims, max_dims) < 0) {
        printf("H5Sget_simple_extent_dims failed\n");
        TEST_ERROR;
    }

    switch (action) {

        case INCR_EXT:
            new[0] = dims[0] + 1;
            new[1] = dims[1] + 1;

            /* Cannot increase to more than maximum dimension (both dims) for FA dataset */
            /* Cannot increase to more than maximum dimension (dim 0) for EA dataset */
            if ((max_dims[0] != H5S_UNLIMITED && new[0] > max_dims[0]) ||
                (max_dims[1] != H5S_UNLIMITED && new[1] > max_dims[1])) {
                printf("Cannot exceed maximum dimension for dataset\n");
                TEST_ERROR;
            }

            break;

        case DECR_EXT:
            new[0] = dims[0] - 1;
            new[1] = dims[1] - 1;

            if (new[0] < chunk_dims[0] || new[1] < chunk_dims[1]) {
                printf("Cannot decrease to less than chunk dimension\n");
                TEST_ERROR;
            }
            break;

        default:
            HDassert(0 && "Unknown action?!?");

    } /* end switch */

    if (H5Dset_extent(did, new) < 0) {
        printf("H5Dset_extent for dataset failed\n");
        TEST_ERROR;
    }

    if (H5Sclose(sid) < 0) {
        printf("H5Sclose failed\n");
        TEST_ERROR;
    }

    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(sid);
    }
    H5E_END_TRY;

    return false;
} /* dset_extent_real() */

/*
 *  Write to dataset with single index: only 1 chunk is written
 */
static bool
write_dset_single(unsigned action, const state_t *s, const dsets_state_t *ds)
{
    hsize_t count[2]  = {1, 1};
    hsize_t stride[2] = {1, 1};
    hsize_t start[2]  = {0, 0};
    hsize_t block[2]  = {s->rows, s->cols};

    HDassert(action == GWRITES || action == PWRITES || action == TWRITES || action == LWRITES);
    HDassert(s->single_index);

    /* Check and set partial chunk write */
    if (action == PWRITES || action == LWRITES)
        check_set_partial_block(action, block, block, start);

    if (!write_chunks(action, ds->single_did, s->filetype, start, stride, count, block)) {
        printf("H5Dwrite to dataset with single index dataset failed\n");
        TEST_ERROR;
    }

    return true;

error:
    return false;

} /* write_dset_single() */

/*
 * Reader
 */

/*
 * Verify the operations for the specified datasets:
 *
 * Dataset with single index:
 * --verify only 1 write because this dataset only has a single chunk
 *
 * Dataset with implicit/fixed array/extensible array/version 2 btree index:
 * --GWRITES: verify writes that cover a single chunk per write
 * --PWRITES: verify writes that cover a partial chunk per write
 * --TWRITES: verify writes that cover multiple chunks per write
 * --LWRITES: verify writes that cover multiple partial chunks per write
 *
 * Dataset with fixed array/extensible array/version 2 btree index:
 * --INCR_EXT: verify the increase to dataset dimension sizes
 * --DECR_EXT: verify the decrease to dataset dimensions sizes
 */
static bool
verify_dsets_operations(state_t *s, dsets_state_t *ds, H5F_vfd_swmr_config_t *config, np_state_t *np, bool fileclosed)
{
    unsigned step;
    unsigned allowed_writes;
    bool     result;

    /* Verify dataset with single index */
    if (s->single_index) {
        /* Verify a single full chunk write to dataset with single index */
        /* gwrites and twrites are the same */
        /* Doesn't matter how many writes, only perform once */
        if (s->gwrites || s->twrites) {
            dbgf(2, "Verify single full chunk write to dataset with single index; only verify 1 write\n");

            if (s->use_np && !np_confirm_verify_notify(np->fd_writer_to_reader, 0, s, np)) {
                printf("np_confirm_verify_notify() verify/notify not in sync failed\n");
                TEST_ERROR;
            }

            /* Wait for a few ticks for the update to happen */
            if (!fileclosed)    
                decisleep(config->tick_len * s->update_interval);

            result = verify_dset_single(GWRITES, s, ds, fileclosed);

            if (s->use_np && !np_reader(result, 0, s, np)) {
                printf("np_reader() for verifying addition failed\n");
                TEST_ERROR;
            } else if (!result)
                TEST_ERROR;
        }

        /* Verify a single partial chunk write to dataset with single index */
        /* pwrites and lwrites are the same */
        /* Doesn't matter how many writes, only perform once */
        if (s->pwrites || s->lwrites) {
            dbgf(2, "Verify single partial chunk write to dataset with single index; only verify 1 write\n");

            if(s->use_np && !np_confirm_verify_notify(np->fd_writer_to_reader, 0, s, np)) {
                printf("np_confirm_verify_notify() verify/notify not in sync failed\n");
                TEST_ERROR;
            }
    
            /* Wait for a few ticks for the update to happen */
            if (!fileclosed)
                decisleep(config->tick_len * s->update_interval);

            result = verify_dset_single(PWRITES, s, ds, fileclosed);

            if (s->use_np && !np_reader(result, 0, s, np)) {
                printf("np_reader() for verifying addition failed\n");
                TEST_ERROR;
            } if (!result)
                TEST_ERROR;
        }
    }

    /* Verify datasets with implicit/fa/ea/bt2 index */
    if (s->implicit_index || s->fa_index || s->ea_index || s->bt2_index) {

        /* Verify single full chunk writes */
        if (s->gwrites) {
            allowed_writes = (unsigned)(ds->scaled_dims[0] * ds->scaled_dims[1]);
            dbgf(2, "The allowed -g writes is %u; you specify %u writes\n", allowed_writes, s->gwrites);

            for (step = 0; (step < s->gwrites && step < allowed_writes); step++) {
                dbgf(2, "Verify single full chunk writes #%u to datasets with implicit/fa/ea/bt2 index\n",
                     step);

                if (s->use_np && !np_confirm_verify_notify(np->fd_writer_to_reader, step, s, np)) {
                    printf("np_confirm_verify_notify() verify/notify not in sync failed\n");
                    TEST_ERROR;
                }

                /* Wait for a few ticks for the update to happen */
                if(!fileclosed) 
                    decisleep(config->tick_len * s->update_interval);

                result = verify_dsets_chunks(GWRITES, s, ds, step, fileclosed);

                if (s->use_np && !np_reader(result, step, s, np)) {
                    printf("np_reader() for verification failed\n");
                    TEST_ERROR;
                } else if (!result)
                    TEST_ERROR;
            }
        }

        /* Verify single partial chunk writes */
        if (s->pwrites) {
            allowed_writes = (unsigned)(ds->scaled_dims[0] * ds->scaled_dims[1]);
            dbgf(2, "The allowed -p writes is %u; you specify %u writes\n", allowed_writes, s->pwrites);

            for (step = 0; (step < s->pwrites && step < allowed_writes); step++) {
                dbgf(2, "Verify single partial chunk writes #%u to datasets with implicit/fa/ea/bt2 index\n",
                     step);

                if (s->use_np && !np_confirm_verify_notify(np->fd_writer_to_reader, step, s, np)) {
                    printf("np_confirm_verify_notify() verify/notify not in sync failed\n");
                    TEST_ERROR;
                }

                /* Wait for a few ticks for the update to happen */
                if(!fileclosed)
                    decisleep(config->tick_len * s->update_interval);

                result = verify_dsets_chunks(PWRITES, s, ds, step, fileclosed);

                if (s->use_np && !np_reader(result, step, s, np)) {
                    printf("np_reader() for verification failed\n");
                    TEST_ERROR;
                } else if (!result)
                    TEST_ERROR;
            }
        }

        /* Verify multiple full chunk writes */
        if (s->twrites) {
            allowed_writes = (unsigned)(ds->multi_scaled[0] * ds->multi_scaled[1]);
            dbgf(2, "The allowed -t writes is %u; you specify %u writes\n", allowed_writes, s->twrites);

            for (step = 0; (step < s->twrites && step < allowed_writes); step++) {
                dbgf(2, "Verify multiple full chunk writes #%u to datasets with implicit/fa/ea/bt2 index\n",
                     step);

                if (s->use_np && !np_confirm_verify_notify(np->fd_writer_to_reader, step, s, np)) {
                    printf("np_confirm_verify_notify() verify/notify not in sync failed\n");
                    TEST_ERROR;
                }

                /* Wait for a few ticks for the update to happen */
                if(!fileclosed)
                    decisleep(config->tick_len * s->update_interval);

                result = verify_dsets_chunks(TWRITES, s, ds, step, fileclosed);

                if (s->use_np && !np_reader(result, step, s, np)) {
                    printf("np_reader() for verification failed\n");
                    TEST_ERROR;
                } else if (!result)
                    TEST_ERROR;
            }
        }

        /* Verify multiple partial chunk writes */
        if (s->lwrites) {
            allowed_writes = (unsigned)(ds->multi_scaled[0] * ds->multi_scaled[1]);
            dbgf(2, "The allowed -l writes is %u; you specify %u writes\n", allowed_writes, s->lwrites);

            for (step = 0; (step < s->lwrites && step < allowed_writes); step++) {
                dbgf(2,
                     "Verify multiple partial chunk writes #%u to datasets with implicit/fa/ea/bt2 index\n",
                     step);

                if (s->use_np && !np_confirm_verify_notify(np->fd_writer_to_reader, step, s, np)) {
                    printf("np_confirm_verify_notify() verify/notify not in sync failed\n");
                    TEST_ERROR;
                }

                /* Wait for a few ticks for the update to happen */
                if(!fileclosed)
                    decisleep(config->tick_len * s->update_interval);

                result = verify_dsets_chunks(LWRITES, s, ds, step, fileclosed);

                if (s->use_np && !np_reader(result, step, s, np)) {
                    printf("np_reader() for verification failed\n");
                    TEST_ERROR;
                } else if (!result)
                    TEST_ERROR;
            }
        }

        /* Verify increase to dataset dimensions: apply only for fa, ea and bt2 index */
        if (!s->implicit_index && s->xincrs) {
            for (step = 0; step < s->xincrs; step++) {
                dbgf(2, "Verify increase to dimension sizes by %u for datasets with fa/ea/bt2 index\n",
                     step + 1);

                if (s->use_np && !np_confirm_verify_notify(np->fd_writer_to_reader, step, s, np)) {
                    printf("np_confirm_verify_notify() verify/notify not in sync failed\n");
                    TEST_ERROR;
                }

                /* Wait for a few ticks for the update to happen */
                decisleep(config->tick_len * s->update_interval);

                result = verify_dsets_extent(INCR_EXT, s, ds, step + 1);

                if (s->use_np && !np_reader(result, step, s, np)) {
                    printf("np_reader() for failed\n");
                    TEST_ERROR;
                } else if (!result)
                    TEST_ERROR;
            }
        }

        /* Verify decrease to dataset dimensions: apply only for fa, ea and bt2 index */
        if (!s->implicit_index && s->ydecrs) {
            for (step = 0; step < s->ydecrs; step++) {
                dbgf(2, "Verify decrease to dimension sizes by %u for datasets with fa/ea/bt2 index\n",
                     step + 1);

                if (s->use_np && !np_confirm_verify_notify(np->fd_writer_to_reader, step, s, np)) {
                    printf("np_confirm_verify_notify() verify/notify not in sync failed\n");
                    TEST_ERROR;
                }
                /* Wait for a few ticks for the update to happen */
                decisleep(config->tick_len * s->update_interval);

                result = verify_dsets_extent(DECR_EXT, s, ds, step + 1);

                if (s->use_np && !np_reader(result, step, s, np)) { 
                    printf("np_reader() for verification failed\n");
                    TEST_ERROR;
                } else if (!result)
                    TEST_ERROR;
            }
        }
    }

    return true;

error:

    return false;

} /* verify_dsets_operations() */

/*
 * Verify the data read from each of the specified datasets:
 *      GWRITES: verify `which` write that covers a single chunk
 *      PWRITES: verify `which` write that covers a partial chunk
 *      TWRITES: verify `which` write that covers multiple chunks
 *      LWRITEs: verify `which` write that covers multiple partial chunks
 */
static bool
verify_dsets_chunks(unsigned action, const state_t *s, const dsets_state_t *ds, unsigned which, bool fileclosed)
{
    hsize_t start[2]  = {0, 0};
    hsize_t stride[2] = {0, 0};
    hsize_t count[2]  = {0, 0};
    hsize_t block[2]  = {0, 0};

    HDassert(s->implicit_index || s->fa_index || s->ea_index || s->bt2_index);

    /* Set up selection according to the specified action */
    setup_selection(action, which, s, ds, start, stride, count, block);

    if (s->implicit_index) {
        if (!verify_chunks(action, ds->implicit_did, s->filetype, start, stride, count, block, fileclosed, s->flush_raw_data)) {
            printf("verify_chunks() to dataset with implicit index failed\n");
            TEST_ERROR;
        }
    }

    if (s->fa_index) {
        if (!verify_chunks(action, ds->fa_did, s->filetype, start, stride, count, block, fileclosed, s->flush_raw_data)) {
            printf("verify_chunks() to dataset with fixed array index failed\n");
            TEST_ERROR;
        }
    }

    if (s->ea_index) {
        if (!verify_chunks(action, ds->ea_did, s->filetype, start, stride, count, block, fileclosed, s->flush_raw_data)) {
            printf("verify_chunks() to dataset with extensible array index failed\n");
            TEST_ERROR;
        }
    }

    if (s->bt2_index) {
        if (!verify_chunks(action, ds->bt2_did, s->filetype, start, stride, count, block, fileclosed, s->flush_raw_data)) {
            printf("verify_chunks() to dataset with bt2 index failed\n");
            TEST_ERROR;
        }
    }

    return true;

error:

    return false;

} /* verify_dsets_chunks() */

/*
 * Verify the data read from the dataset is as expected.
 */
static bool
verify_chunks(unsigned action, hid_t did, hid_t tid, hsize_t *start, hsize_t *stride, hsize_t *count,
                 hsize_t *block, bool fileclosed, bool flush_raw_data)
{
    hid_t         mem_sid = badhid;
    hid_t         sid     = badhid;
    hsize_t       mem_dims[2];
    unsigned int *rbuf = NULL;
    unsigned      i;

    /* Refresh the dataset */
    if (H5Drefresh(did) < 0) {
        printf("H5Drefresh dataset failed\n");
        TEST_ERROR;
    }

    if ((sid = H5Dget_space(did)) < 0) {
        printf("H5Dget_space dataset failed\n");
        TEST_ERROR;
    }

    /* Make the selection the file dataspace */
    if (H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, stride, count, block) < 0) {
        printf("H5Sselect to dataset failed\n");
        TEST_ERROR;
    }

    mem_dims[0] = block[0];
    mem_dims[1] = block[1];
    if ((mem_sid = H5Screate_simple(2, mem_dims, NULL)) < 0) {
        printf("H5Screate_simple failed\n");
        TEST_ERROR;
    }

    /* Allocate the buffer for reading */
    if ((rbuf = HDmalloc(block[0] * block[1] * sizeof(unsigned int))) == NULL) {
        printf("HDmalloc failed\n");
        TEST_ERROR;
    }

    /* Read the data from the dataset into `rbuf` */
    if (H5Dread(did, tid, mem_sid, sid, H5P_DEFAULT, rbuf) < 0) {
        printf("H5Dread from dataset failed\n");
        TEST_ERROR;
    }

    /* Verify the data read in `rbuf` is as the fill value expected */
    for (i = 0; i < block[0] * block[1]; i++) {
        if(flush_raw_data || fileclosed) {
            if (action == GWRITES || action == TWRITES) {
                if (rbuf[i] != FILL_FULL) {
                    printf("Invalid value for dataset for GWRITES/TWRITES: %d\n", rbuf[i]);
                    TEST_ERROR;
                }
            }
            else {
                HDassert(action == PWRITES || action == LWRITES);
                if (rbuf[i] != FILL_PARTIAL) {
                    printf("Invalid value for dataset for GWRITES/TWRITES: %d\n", rbuf[i]);
                    TEST_ERROR;
                }
            }
        } else { /* No flush && not closing file */
            if (action == GWRITES || action == TWRITES) {
                if (rbuf[i] != FILL_FULL && rbuf[i] != FILL_INIT) {
                    printf("Invalid value for dataset for GWRITES/TWRITES\n");
                    TEST_ERROR;
                }
            }
            else {
                if (rbuf[i] != FILL_PARTIAL && rbuf[i] != FILL_INIT) {
                    printf("Invalid value for dataset for GWRITES/TWRITES\n");
                    TEST_ERROR;
                }
            }
        }
    }

    if (H5Sclose(sid) < 0) {
        printf("H5Sclose failed\n");
        TEST_ERROR;
    }

    if (H5Sclose(mem_sid) < 0) {
        printf("H5Sclose failed\n");
        TEST_ERROR;
    }

    if (rbuf)
        HDfree(rbuf);

    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(sid);
        H5Sclose(mem_sid);
    }
    H5E_END_TRY;

    if (rbuf)
        HDfree(rbuf);

    return false;

} /* verify_chunks() */

/*
 * Verify the increase or decrease of dimenion sizes for the specified datasets.
 */
static bool
verify_dsets_extent(unsigned action, const state_t *s, const dsets_state_t *ds, unsigned which)
{
    unsigned rows = s->rows;
    unsigned cols = s->cols;

    HDassert(action == INCR_EXT || action == DECR_EXT);
    HDassert(s->fa_index || s->ea_index || s->bt2_index);

    /* s->xincrs can be 0 or the increased extent of the dataset */
    if (action == DECR_EXT) {
        rows = s->rows + s->xincrs;
        cols = s->cols + s->xincrs;
    }

    if (s->fa_index) {
        dbgf(2, "Verify dataset extent for FA dataset\n");
        if (!verify_dset_extent_real(action, ds->fa_did, rows, cols, which)) {
            printf("verify_read_dset() to dataset with fixed array index failed\n");
            TEST_ERROR;
        }
    }

    if (s->ea_index) {
        dbgf(2, "Verify dataset extent for EA dataset\n");
        if (!verify_dset_extent_real(action, ds->fa_did, rows, cols, which)) {
            printf("verify_read_dset() to dataset with fixed array index failed\n");
            TEST_ERROR;
        }
    }

    if (s->bt2_index) {
        dbgf(2, "Verify dataset extent for BT2 dataset\n");
        if (!verify_dset_extent_real(action, ds->bt2_did, rows, cols, which)) {
            printf("verify_read_dset() to dataset with fixed array index failed\n");
            TEST_ERROR;
        }
    }

    return true;

error:
    return false;

} /* verify_dsets_extent() */

/*
 * Do the real work of verifying the increase/decrease for the dataset dimension sizes
 */
static bool
verify_dset_extent_real(unsigned action, hid_t did, unsigned rows, unsigned cols, unsigned which)
{
    hsize_t dims[2];
    hid_t   sid = badhid;

    /* Refresh the dataset */
    if (H5Drefresh(did) < 0) {
        printf("H5Drefresh dataset failed\n");
        TEST_ERROR;
    }

    if ((sid = H5Dget_space(did)) < 0) {
        printf("H5Dget_space dataset failed\n");
        TEST_ERROR;
    }

    if (H5Sget_simple_extent_dims(sid, dims, NULL) < 0) {
        printf("H5Sget_simple_extent_dims() failed\n");
        TEST_ERROR;
    }

    switch (action) {

        case INCR_EXT:
            if (dims[0] != (rows + which) || dims[1] != (cols + which))
                TEST_ERROR

            break;

        case DECR_EXT:
            if (dims[0] != (rows - which) || dims[1] != (cols - which))
                TEST_ERROR
            break;

        default:
            HDassert(0 && "Unknown action?!?");

    } /* end switch */

    if (H5Sclose(sid) < 0) {
        printf("H5Sclose failed\n");
        TEST_ERROR;
    }

    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(sid);
    }
    H5E_END_TRY;

    return false;
} /* verify_dset_extent_real() */

/*
 * Verify that the data read from the dataset with single index is as unexpected.
 */
static bool
verify_dset_single(unsigned action, const state_t *s, const dsets_state_t *ds, bool fileclosed)
{
    hsize_t block[2]  = {s->rows, s->cols};
    hsize_t count[2]  = {1, 1};
    hsize_t stride[2] = {1, 1};
    hsize_t start[2]  = {0, 0};

    HDassert(action == GWRITES || action == PWRITES);
    HDassert(s->single_index);

    if (action == PWRITES)
        check_set_partial_block(action, block, block, start);

    if (!verify_chunks(action, ds->single_did, s->filetype, start, stride, count, block, fileclosed, s->flush_raw_data)) {
        printf("verify_read_dset() to dataset with single index failed\n");
        TEST_ERROR;
    }

    return true;

error:
    return false;

} /* verify_dset_single() */

/*
 * Named pipes handling
 */

/*
 * Initialize the named pipes for test synchronization.
 */
static bool
np_init(np_state_t *np, bool writer)
{
    *np = NP_INITIALIZER;

    /*
     * Use two named pipes(FIFO) to coordinate the writer and reader for
     * two-way communication so that the two sides can move forward together.
     * One is for the writer to write to the reader.
     * The other one is for the reader to signal the writer.
     */
    if (writer) {
        /* If the named pipes are present at the start of the test, remove them */
        if (HDaccess(np->fifo_writer_to_reader, F_OK) == 0)
            if (HDremove(np->fifo_writer_to_reader) != 0) {
                printf("HDremove fifo_writer_to_reader failed\n");
                TEST_ERROR;
            }

        if (HDaccess(np->fifo_reader_to_writer, F_OK) == 0)
            if (HDremove(np->fifo_reader_to_writer) != 0) {
                printf("HDremove fifo_reader_to_writer failed\n");
                TEST_ERROR;
            }

        /* Writer creates two named pipes(FIFO) */
        if (HDmkfifo(np->fifo_writer_to_reader, 0600) < 0) {
            printf("HDmkfifo fifo_writer_to_reader failed\n");
            TEST_ERROR;
        }

        if (HDmkfifo(np->fifo_reader_to_writer, 0600) < 0) {
            printf("HDmkfifo fifo_reader_to_writer failed\n");
            TEST_ERROR;
        }
    }

    /* Both the writer and reader open the pipes */
    if ((np->fd_writer_to_reader = HDopen(np->fifo_writer_to_reader, O_RDWR)) < 0) {
        printf("HDopen fifo_writer_to_reader failed\n");
        TEST_ERROR;
    }

    if ((np->fd_reader_to_writer = HDopen(np->fifo_reader_to_writer, O_RDWR)) < 0) {
        printf("HDopen fifo_reader_to_writer failed\n");
        TEST_ERROR;
    }

    return true;

error:
    return false;

} /* np_init() */

/*
 * Close the named pipes.
 */
static bool
np_close(np_state_t *np, bool writer)
{
    /* Both the writer and reader close the named pipes */
    if (HDclose(np->fd_writer_to_reader) < 0) {
        printf("HDclose fd_writer_to_reader failed\n");
        TEST_ERROR;
    }

    if (HDclose(np->fd_reader_to_writer) < 0) {
        printf("HDclose fd_reader_to_writer failed\n");
        TEST_ERROR;
    }

    /* Reader finishes last and deletes the named pipes */
    if (!writer) {
        if (HDremove(np->fifo_writer_to_reader) != 0) {
            printf("HDremove fifo_writer_to_reader failed\n");
            TEST_ERROR;
        }

        if (HDremove(np->fifo_reader_to_writer) != 0) {
            printf("HDremove fifo_reader_to_writer failed\n");
            TEST_ERROR;
        }
    }
    return true;

error:
    return false;
} /* np_close() */

/*
 *  Writer synchronization depending on the result from the action performed.
 */
static bool
np_writer(bool result, unsigned step, const state_t *s, np_state_t *np, H5F_vfd_swmr_config_t *config)
{
    unsigned int i;

    /* The action fails */
    if (!result) {
        printf("action failed\n");
        H5_FAILED();
        AT();

        /* At communication interval, notify the reader about the failure and quit */
        if (step % s->csteps == 0) {
            np->notify = -1;
            HDwrite(np->fd_writer_to_reader, &np->notify, sizeof(int));
            goto error;
        }
        /* The action succeeds */
    }
    else {
        /* At communication interval, notify the reader and wait for its response */
        if (step % s->csteps == 0) {
            /* Bump up the value of notify to tell the reader to start reading */
            np->notify++;
            if (HDwrite(np->fd_writer_to_reader, &np->notify, sizeof(int)) < 0) {
                printf("HDwrite failed\n");
                TEST_ERROR;
            }

            /* During the wait, writer makes repeated HDF5 API calls
             * to trigger EOT at approximately the correct time */
            for (i = 0; i < config->max_lag + 1; i++) {
                decisleep(config->tick_len);
                H5E_BEGIN_TRY
                {
                    H5Aexists(s->file, "nonexistent");
                }
                H5E_END_TRY;
            }

            /* Handshake between writer and reader */
            if (!np_confirm_verify_notify(np->fd_reader_to_writer, step, s, np)) {
                printf("np_confirm_verify_notify() verify/notify not in sync failed\n");
                TEST_ERROR;
            }
        }
    }
    return true;

error:
    return false;

} /* np_writer() */

/*
 *
 *  Reader synchronization depending on the result from the verification.
 */
static bool
np_reader(bool result, unsigned step, const state_t *s, np_state_t *np)
{
    /* The verification fails */
    if (!result) {
        printf("verify action failed\n");
        H5_FAILED();
        AT();

        /* At communication interval, tell the writer about the failure and exit */
        if (step % s->csteps == 0) {
            np->notify = -1;
            HDwrite(np->fd_reader_to_writer, &np->notify, sizeof(int));
            goto error;
        }
        /* The verification succeeds */
    }
    else {
        if (step % s->csteps == 0) {
            /* Send back the same notify value for acknowledgement:
             *   --inform the writer to move to the next step */
            if (HDwrite(np->fd_reader_to_writer, &np->notify, sizeof(int)) < 0) {
                printf("HDwrite failed\n");
                TEST_ERROR;
            }
        }
    }
    return true;

error:
    return false;

} /* np_reader() */

/*
 *  Handshake between writer and reader:
 *      Confirm `verify` is same as `notify`.
 */
static bool
np_confirm_verify_notify(int fd, unsigned step, const state_t *s, np_state_t *np)
{
    if (step % s->csteps == 0) {
        np->verify++;
        if (HDread(fd, &np->notify, sizeof(int)) < 0) {
            printf("HDread failed\n");
            TEST_ERROR;
        }

        if (np->notify == -1) {
            printf("reader/writer failed to verify\n");
            TEST_ERROR;
        }

        if (np->notify != np->verify) {
            printf("received message %d, expecting %d\n", np->notify, np->verify);
            TEST_ERROR;
        }
    }

    return true;

error:
    return false;
} /* np_confirm_verify_notify() */

/*
 *  When flush of raw data is disabled, the following is done by the writer and reader:
 *  Writer:
 *      Close the file
 *      Notify the reader that the file is closed
 *  Reader:
 *      Confirm the message from the writer that the file is closed
 *      Verify the data
 */
static bool
closing_on_noflush(bool writer, state_t *s, dsets_state_t *ds, H5F_vfd_swmr_config_t *config, np_state_t *np)
{
    HDassert(s->use_np);

    if(writer) {
        if (!close_dsets(ds)) {
            printf("close_dsets() failed\n");
            TEST_ERROR;
        }

        dbgf(2, "Writer closes the file (flush of raw data is disabled)\n");
        if (H5Fclose(s->file) < 0) {
            printf("H5Fclose failed\n");
            TEST_ERROR;
        }

        /* Bump up the value of notify to tell the reader the file is closed */
        dbgf(2, "Writer notifies reader that the file is closed (flush of raw data is disabled)\n");
        np->notify++;
        if (HDwrite(np->fd_writer_to_reader, &np->notify, sizeof(int)) < 0) {
            printf("HDwrite failed\n");
            TEST_ERROR;
        }

        if (!np_close(np, writer)) {
            printf("np_close() failed\n");
            TEST_ERROR;
        }

    } else {
        /* Wait for a few ticks for the file to close in writer */
        decisleep(config->tick_len * s->update_interval);

        dbgf(2, "Reader checks notify value from writer (flush of raw data is disabled)\n");
        if (!np_confirm_verify_notify(np->fd_writer_to_reader, 0, s, np)) {
            printf("np_confirm_verify_notify() verify/notify not in sync failed\n");
            TEST_ERROR;
        }

        /* Close the named pipes */
        if (!np_close(np, writer)) {
            printf("np_close() failed\n");
            TEST_ERROR;
        }

        /* Turn off named pipes */
        s->use_np = false;
    
        /* Verify the dataset again without named pipes */
        dbgf(2, "Reader verifies data after writer closes the file (flush of raw data is disabled)\n");
        if(!verify_dsets_operations(s, ds, config, np, true)) {
            printf("verify_dsets_operations() failed\n");
            TEST_ERROR
        }

        if (!close_dsets(ds)) {
            printf("close_dsets() failed\n");
            TEST_ERROR;
        }

        dbgf(2, "Reader closes the file (flush of raw data is disabled)\n");
        if (H5Fclose(s->file) < 0) {
            printf("H5Fclose failed\n");
            TEST_ERROR;
        }

    }

    return true;

error:
    return false;

} /* closing_on_noflush() */
/*
 * Main
 */
int
main(int argc, char **argv)
{
    hid_t                 fapl, fcpl;
    bool                  writer;
    state_t               s;
    const char *          personality;
    H5F_vfd_swmr_config_t config;
    np_state_t            np;
    dsets_state_t         ds;

    if (!state_init(&s, argc, argv)) {
        printf("state_init() failed\n");
        TEST_ERROR;
    }

    personality = HDstrstr(s.progname, "vfd_swmr_dsetchks_");

    if (personality != NULL && HDstrcmp(personality, "vfd_swmr_dsetchks_writer") == 0)
        writer = true;
    else if (personality != NULL && HDstrcmp(personality, "vfd_swmr_dsetchks_reader") == 0)
        writer = false;
    else {
        printf("unknown personality, expected vfd_swmr_dsetchks_{reader,writer}\n");
        TEST_ERROR;
    }

    /* config, tick_len, max_lag, writer, flush_raw_data, md_pages_reserved, md_file_path */
    init_vfd_swmr_config(&config, 4, 7, writer, s.flush_raw_data, 128, "./dsetchks-shadow");

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    if ((fapl = vfd_swmr_create_fapl(true, s.use_vfd_swmr, true, 4096, &config)) < 0) {
        printf("vfd_swmr_create_fapl() failed\n");
        TEST_ERROR;
    }

    /* Set fs_strategy (file space strategy) and fs_page_size (file space page size) */
    if ((fcpl = vfd_swmr_create_fcpl(H5F_FSPACE_STRATEGY_PAGE, 4096)) < 0) {
        HDprintf("vfd_swmr_create_fcpl() failed");
        TEST_ERROR;
    }

    if (writer) {
        if ((s.file = H5Fcreate(s.filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0) {
            printf("H5Fcreate failed\n");
            TEST_ERROR;
        }

        if (!create_dsets(&s, &ds)) {
            printf("create_dsets() failed\n");
            TEST_ERROR;
        }
    }
    else {
        if ((s.file = H5Fopen(s.filename, H5F_ACC_RDONLY, fapl)) < 0) {
            printf("H5Fopen failed\n");
            TEST_ERROR;
        }
        if (!open_dsets(&s, &ds)) {
            printf("open_dsets() failed\n");
            TEST_ERROR;
        }
    }

    /* Initiailze named pipes */
    if (s.use_np && !np_init(&np, writer)) {
        printf("np_init() failed\n");
        TEST_ERROR;
    }

    if (writer) {

        if (!perform_dsets_operations(&s, &ds, &config, &np)) {
            printf("perform_dsets_operations() failed\n");
            TEST_ERROR;
        }
    }
    else {

        if (!verify_dsets_operations(&s, &ds, &config, &np, false)) {
            printf("perform_dsets_operations() failed\n");
            TEST_ERROR;
        }
    }

    if (H5Pclose(fapl) < 0) {
        printf("H5Pclose failed\n");
        TEST_ERROR;
    }

    if (H5Pclose(fcpl) < 0) {
        printf("H5Pclose failed\n");
        TEST_ERROR;
    }

    /* When flush of raw data is disabled, special handling is performed 
     * via closing_on_noflush() when closing the file.
     * Nothing needs to be done for -x or -y options 
     * (increase and decrease dataset dimension sizes).
     */
    if(!s.flush_raw_data && !s.xincrs && !s.ydecrs && s.use_np) {

        if(!closing_on_noflush(writer, &s, &ds, &config, &np))
            TEST_ERROR
    } else {

        if (!close_dsets(&ds)) {
            printf("close_dsets() failed\n");
            TEST_ERROR;
        }

        if (H5Fclose(s.file) < 0) {
            printf("H5Fclose failed\n");
            TEST_ERROR;
        }

        if (s.use_np && !np_close(&np, writer)) {
            printf("np_close() failed\n");
            TEST_ERROR;
        }
    }

    return EXIT_SUCCESS;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
        H5Pclose(fcpl);
        H5Fclose(s.file);
    }
    H5E_END_TRY;

    if (s.use_np && np.fd_writer_to_reader >= 0)
        HDclose(np.fd_writer_to_reader);

    if (s.use_np && np.fd_reader_to_writer >= 0)
        HDclose(np.fd_reader_to_writer);

    if (s.use_np && !writer) {
        HDremove(np.fifo_writer_to_reader);
        HDremove(np.fifo_reader_to_writer);
    }

    return EXIT_FAILURE;
} /* main */

#else /* H5_HAVE_WIN32_API */

int
main(void)
{
    HDfprintf(stderr, "Non-POSIX platform. Skipping.\n");
    return EXIT_SUCCESS;
} /* end main() */

#endif /* H5_HAVE_WIN32_API */
