/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by Akadio, Inc.                                                 *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 *  Purpose: To test writing operations for different dataset types.
 *      Dataset types:
 *      --dataset with compact layout
 *      --dataset with contiguous layout
 *      --datasets with chunked layout: single/implicit/fixed array/extensible array/btree2 indexes
 *
 *      Types of writes:
 *      -- Sequential writes
 *      -- Random writes
 *      -- Regular hyperslab writes
 *      -- Raw data modifications
 */
/*
 * Modifications for testing references
 *
 *  -O option for object reference
 *      --Writer:
 *          When creating the datasets via create_dsets():
 *              --Create an object reference dataset
 *              --Store the dataset references via H5Rcreate_object() and save
 *                them in the object reference dataset
 *      --Reader:
 *          When opening the datasets via open_dsets():
 *              --Open the object reference dataset
 *              --Retrieve the object references from the dataset
 *              --Open the dataset object via H5Ropen_object()
 *
 *  -R option for region reference
 *      Writer:
 *          --When creating the datasets via create_dsets():
 *              --Create a region reference dataset
 *          --When writing to the datasets via write_dset():
 *              --Store the dataset selections via H5Rcreate_region() and save
 *                them in the region reference dataset
 *      Reader:
 *          --When opening the datasets via open_dsets():
 *              --Open the region reference dataset
 *          --When verifying data written to a dataset via verify_dset():
 *              --Retrieve the dataset selections from the region reference dataset
 *              --Get the selection via H5Ropen_region()
 */

#include "hdf5.h"
#include "testhdf5.h"
#include "vfd_swmr_common.h"

#ifndef H5_HAVE_WIN32_API

#define READER_WAIT_TICKS 4
#define MAX_COMPACT_SIZE  65520 /* max obj header message size 65536) - other layout message fields (16) */
#define MAX_COMPACT_ELMS  (MAX_COMPACT_SIZE / sizeof(unsigned int))
#define RANDOM_SEED       9 /* Random seed used by both writer and reader for random writes */

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
    bool         flush_raw_data;     /* For -U option */
    bool         obj_ref;            /* For -O option */
    bool         reg_ref;            /* For -R option */
    bool         compact;            /* -p option: create compact dataset */
    bool         compact_write;      /* -t option: write to the whole compact dataset */
    unsigned int compact_elmts;      /* -e <elmts> option: # of elements for the compact dataset */
    bool         contig;             /* -g option: create contiguous dataset */
    bool         chunked;            /* -k option: create chunked datasets with 5 indexing types */
    unsigned int rows;               /* -m <rows> option for contiguous and/or chunked datasets */
    unsigned int cols;               /* -n <cols option for contiguous and/or chunked datasets */
    unsigned int swrites;   /* -s <swrites> option: sequential writes to contiguous and/or chunked datasets */
    unsigned int rwrites;   /* -r <rwrites> option: random writes to contiguous and/or chunked datasets */
    unsigned int lwrites;   /* -l <lwrites> option: hyperslab writes to contiguous and/or chunked datasets */
    unsigned int wwrites;   /* -w <wwrites> option: modify raw data to contiguous and/or chunked datasets */
    unsigned int lastwrite; /* The last operation (-s, -r, -l or -w) performed. */
} state_t;

/* Initializations for state_t */
#define ALL_HID_INITIALIZER                                                                                  \
    (state_t)                                                                                                \
    {                                                                                                        \
        .filename = "", .file = H5I_INVALID_HID, .filetype = H5T_NATIVE_UINT32,                              \
        .update_interval = READER_WAIT_TICKS, .csteps = 1, .use_np = true, .use_vfd_swmr = true,             \
        .flush_raw_data = true, .compact = false, .compact_write = false, .compact_elmts = MAX_COMPACT_ELMS, \
        .contig = false, .rows = 10, .cols = 5, .swrites = 0, .rwrites = 0, .lwrites = 0, .wwrites = 0,      \
        .lastwrite = 0, .obj_ref = false, .reg_ref = false                                                   \
    }

/* Structure to hold info for different dataset types */
typedef struct {
    hid_t      compact_did;  /* ID for compact dataset */
    hid_t      contig_did;   /* ID for contiguous dataset */
    hid_t      single_did;   /* ID for chunked dataset: single index */
    hid_t      implicit_did; /* ID for chunked dataset: implicit index */
    hid_t      fa_did;       /* ID for chunked dataset: fixed array index  */
    hid_t      ea_did;       /* ID for chunked dataset: extensible array index */
    hid_t      bt2_did;      /* ID for chunked dataset: version 2 btree index */
    hid_t      compact_sid;  /* Dataspace ID for compact dataset */
    hid_t      contig_sid;   /* Dataspace ID for contiguous dataset */
    hid_t      single_sid;   /* Dataspace ID for chunked dataset */
    hid_t      implicit_sid; /* Dataspace ID for chunked dataset */
    hid_t      fa_sid;       /* Dataspace ID for chunked dataset */
    hid_t      ea_sid;       /* Dataspace ID for chunked dataset */
    hid_t      bt2_sid;      /* Dataspace ID for chunked dataset */
    hid_t      obj_did;      /* ID for object reference dataset */
    hid_t      reg_did;      /* ID for region reference dataset */
    H5R_ref_t *reg_buf;      /* Buffer for holding the region references */
} dsets_state_t;

/* Initializations for dsets_state_t */
#define DSETS_INITIALIZER                                                                                    \
    (dsets_state_t)                                                                                          \
    {                                                                                                        \
        .compact_did = H5I_INVALID_HID, .compact_sid = H5I_INVALID_HID, .contig_did = H5I_INVALID_HID,       \
        .contig_sid = H5I_INVALID_HID, .single_did = H5I_INVALID_HID, .single_sid = H5I_INVALID_HID,         \
        .implicit_did = H5I_INVALID_HID, .implicit_sid = H5I_INVALID_HID, .fa_did = H5I_INVALID_HID,         \
        .fa_sid = H5I_INVALID_HID, .ea_did = H5I_INVALID_HID, .ea_sid = H5I_INVALID_HID,                     \
        .bt2_did = H5I_INVALID_HID, .bt2_sid = H5I_INVALID_HID, .obj_did = H5I_INVALID_HID,                  \
        .reg_did = H5I_INVALID_HID, .reg_buf = NULL                                                          \
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
        .fifo_writer_to_reader = "./fifo_dsetops_writer_to_reader",                                          \
        .fifo_reader_to_writer = "./fifo_dsetops_reader_to_writer", .fd_writer_to_reader = -1,               \
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
static bool open_dset_real(const state_t *s, hid_t *did, hid_t *sid, const char *name, H5R_ref_t *obj_buf);
static bool close_dsets(dsets_state_t *ds);
static bool close_dset_real(hid_t did, hid_t sid);

static bool perform_dsets_operations(state_t *s, dsets_state_t *ds, H5F_vfd_swmr_config_t *config,
                                     np_state_t *np);
static bool dsets_action(unsigned action, const state_t *s, const dsets_state_t *ds, unsigned step);
static bool dset_setup(unsigned action, unsigned which, const state_t *s, hsize_t *start, hsize_t *stride,
                       hsize_t *count, hsize_t *block, hid_t *mem_sid, unsigned int **buf);
static bool write_dset(const state_t *s, const char *name, hid_t did, hid_t sid, hid_t mem_sid,
                       hsize_t *start, hsize_t *stride, hsize_t *count, hsize_t *block, unsigned int *buf,
                       H5R_ref_t *reg_buf);
static bool write_dset_compact(const state_t *s, const dsets_state_t *ds);

static bool verify_dsets_operations(state_t *s, dsets_state_t *ds, H5F_vfd_swmr_config_t *config,
                                    np_state_t *np, bool fileclosed);
static bool verify_dsets_action(unsigned action, const state_t *s, const dsets_state_t *ds, unsigned which,
                                bool fileclosed);
static bool verify_dset(hid_t did, hid_t tid, hid_t sid, hid_t mem_sid, hsize_t *start, hsize_t *stride,
                        hsize_t *count, hsize_t *block, unsigned int *vbuf, bool fileclosed,
                        bool flush_raw_data, H5R_ref_t *rbuf);
static bool verify_dset_compact(const state_t *s, const dsets_state_t *ds, bool fileclosed,
                                bool flush_raw_data);

static bool closing_on_noflush(bool writer, state_t *s, dsets_state_t *ds, H5F_vfd_swmr_config_t *config,
                               np_state_t *np);

static const hid_t badhid = H5I_INVALID_HID;

/* Names for datasets */
#define DSET_COMPACT_NAME  "compact_dset"
#define DSET_CONTIG_NAME   "contig_dset"
#define DSET_SINGLE_NAME   "chunked_single"
#define DSET_IMPLICIT_NAME "chunked_implicit"
#define DSET_FA_NAME       "chunked_fa"
#define DSET_EA_NAME       "chunked_ea"
#define DSET_BT2_NAME      "chunked_bt2"

#define DSET_OBJ_REF_NAME "obj_ref_dset" /* Object reference dataset */
#define DSET_REG_REF_NAME "reg_ref_dset" /* Region reference dataset */
#define OBJ_REF_DIMS      7              /* Dimension size for object reference dataset */
#define REG_REF_DIMS      6              /* Dimension size for region reference dataset */

/* Action for writes */
#define SEQ_WRITE    1 /* Sequential write */
#define RANDOM_WRITE 2 /* Random write */
#define HYPER_WRITE  3 /* Hyperslab write */
#define MODIFY_DATA  4 /* Modify raw data */

/* Test program usage info */
static void
usage(const char *progname)
{
    HDfprintf(stderr,
              "usage: %s \n"
              "           [-p] [-e elmts] [-o]\n"
              "           [-g] [-k] [-m rows] [-n cols]\n"
              "           [-s swrites] [-r rwrites] [-l lwrites] [-w writes]\n"
              "           [-u nticks] [-c csteps] [-U] [-O] [-R] [-S] [-N] [-q] [-b]\n"
              "\n"
              "-p:              create a dataset with compact layout\n"
              "-e elmts:        # of <elmts> for the compact dataset\n"
              "                 (default is 16380)\n"
              "-t:              perform write to the compact dataset\n"
              "-g:              create a dataset with contiguous layout\n"
              "-k:              create 5 datasets with chunked layout for the 5 indexing types\n"
              "-m rows:         # of <rows> rows for the contiguous and/or chunked datasets\n"
              "-n cols:         # of <cols> columns for the contiguous and/or chunked datasets\n"
              "-s swrites:      perform sequential writes to all datasets\n"
              "-r rwrites:      perform random writes to all datasets\n"
              "-l lwrites:      perform hyperslab writes to all datasets\n"
              "                 # of rows to write: every other element is selected per row\n"
              "-w wwrites:      perform raw data modifications to all datasets\n"
              "-u nticks:       `nticks` ticks for the reader to wait before verification\n"
              "                 (default is 4)\n"
              "-c csteps:       `csteps` steps communication interval between reader and writer\n"
              "                 (default is 1)\n"
              "-U:              disable flush of raw data (default is flushing raw data)\n"
              "-O:              create object references to datasets\n"
              "-R:              create region references to datasets\n"
              "-S:              do not use VFD SWMR\n"
              "-N:              do not use named pipes for test synchronization\n"
              "-q:              silence printouts, few messages\n"
              "-b:              write data in big-endian byte order\n"
              "                 (default is H5T_NATIVE_UINT32)\n\n"
              "Note:\n"
              "1. Require to specify at least -p, -g or -k option\n"
              "2. -c <csteps> option cannot exceed -s <swrites> or -r <rwrites>\n"
              "   or -l <lwrites> or -w <wwrites> option\n"
              "\n",
              progname);
    HDexit(EXIT_FAILURE);
} /* usage() */

/*
 * Initialize option info in state_t
 */
static bool
state_init(state_t *s, int argc, char **argv)
{
    unsigned long tmp;
    int           ch;
    char *        tfile = NULL;
    char *        end;

    *s = ALL_HID_INITIALIZER;

    if (H5_basename(argv[0], &tfile) < 0) {
        HDprintf("H5_basename failed\n");
        TEST_ERROR;
    }

    esnprintf(s->progname, sizeof(s->progname), "%s", tfile);

    if (tfile) {
        HDfree(tfile);
        tfile = NULL;
    }

    while ((ch = getopt(argc, argv, "pte:gkm:n:s:r:l:w:bqSNUORu:c:")) != -1) {
        switch (ch) {

            case 'p': /* compact dataset */
                s->compact = true;
                break;

            case 't': /* compact write */
                s->compact_write = true;
                break;

            case 'g': /* contiguous dataset */
                s->contig = true;
                break;

            case 'k': /* chunked datasets */
                s->chunked = true;
                break;

            case 'U': /* Disable flush of raw data */
                s->flush_raw_data = false;
                break;

            case 'O': /* Create object reference to datasets */
                s->obj_ref = true;
                break;

            case 'R': /* Create region reference to datasets */
                s->reg_ref = true;
                break;

            case 'q':
                verbosity = 0;
                break;

            case 'b':
                s->filetype = H5T_STD_U32BE;
                break;

            case 'S':
                s->use_vfd_swmr = false;
                break;

            case 'N':
                s->use_np = false;
                break;

            case 'e': /* # of elements for compact dataset */
            case 'm': /* # of rows for -g and/or -k */
            case 'n': /* # of cols for -g and/or -k */
            case 's': /* # of sequential writes for -g and/or -k */
            case 'r': /* # of random writes for -g and/or -k */
            case 'l': /* # of hyperslab writes for -g and/or -k */
            case 'w': /* # of raw data modifications for -g and/or -k */
            case 'u': /* ticks for raeder to wait before verification */
            case 'c': /* communication interval */
                errno = 0;
                tmp   = HDstrtoul(optarg, &end, 0);
                if (end == optarg || *end != '\0') {
                    HDprintf("couldn't parse `-%c` argument `%s`\n", ch, optarg);
                    TEST_ERROR;
                }
                else if (errno != 0) {
                    HDprintf("couldn't parse `-%c` argument `%s`\n", ch, optarg);
                    TEST_ERROR;
                }
                else if (tmp > UINT_MAX) {
                    HDprintf("`-%c` argument `%lu` too large\n", ch, tmp);
                    TEST_ERROR;
                }

                if (ch == 'e')
                    s->compact_elmts = (unsigned)tmp;
                else if (ch == 'm')
                    s->rows = (unsigned)tmp;
                else if (ch == 'n')
                    s->cols = (unsigned)tmp;
                else if (ch == 's')
                    s->swrites = (unsigned)tmp;
                else if (ch == 'r')
                    s->rwrites = (unsigned)tmp;
                else if (ch == 'l')
                    s->lwrites = (unsigned)tmp;
                else if (ch == 'w')
                    s->wwrites = (unsigned)tmp;
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

    /* Require to specify at least -p or -g or -k option */
    if (!s->compact && !s->contig && !s->chunked) {
        HDprintf("Require to specify at least -p or -g or -k option\n");
        usage(s->progname);
        goto error;
    }

    /* -e <elmts> */
    if (s->compact_elmts > MAX_COMPACT_ELMS) {
        HDprintf("size of compact dataset cannot exceed 16380 elements\n");
        TEST_ERROR;
    }

    /* Enable compact write (-t) without compact dataset (-p) */
    if (s->compact_write && !s->compact) {
        HDprintf("Enable compact write without compact dataset\n");
        usage(s->progname);
        goto error;
    }

    /* Object reference applies to either compact or contiguous or chunked datasets */
    if (s->obj_ref && !(s->compact || s->contig || s->chunked)) {
        HDprintf("Enable object reference without compact/contig/chunked dataset\n");
        usage(s->progname);
        goto error;
    }

    /* Region reference applies to either contiguous or chunked datasets */
    if (s->reg_ref && !(s->contig || s->chunked)) {
        HDprintf("Enable region reference without contig/chunked dataset\n");
        usage(s->progname);
        goto error;
    }

    /* Enable sequential/random/hyperslab/raw data writes (-s/-r/-l/-w) without contiguous/chunked dataset
     * (-g/-k) */
    if ((s->swrites || s->rwrites || s->lwrites || s->wwrites) && !(s->contig || s->chunked)) {
        HDprintf("Enable sequential/random/hypuerslab/raw data writes without contiguous/chunked dataset\n");
        usage(s->progname);
        goto error;
    }

    /* -c <csteps> cannot be zero */
    if (!s->csteps) {
        HDprintf("communication interval cannot be zero\n");
        TEST_ERROR;
    }

    /* -c <csteps> and -s <swrites> options */
    if (s->swrites && s->csteps > s->swrites) {
        HDprintf("communication interval with sequential writes is out of bounds\n");
        TEST_ERROR;
    }

    /* -c <csteps> and -r <rwrites> options */
    if (s->rwrites && s->csteps > s->rwrites) {
        HDprintf("communication interval with random writes is out of bounds\n");
        TEST_ERROR;
    }

    /* -c <csteps> and -l <lwrites> options */
    if (s->lwrites && s->csteps > s->lwrites) {
        HDprintf("communication interval with hyperslab writes is out of bounds\n");
        TEST_ERROR;
    }

    /* -c <csteps> and -w <wwrites> options */
    if (s->wwrites && s->csteps > s->wwrites) {
        HDprintf("communication interval with raw data modification is out of bounds\n");
        TEST_ERROR;
    }

    /* The test file name */
    esnprintf(s->filename, sizeof(s->filename), "vfd_swmr_dsetops.h5");

    return true;

error:
    if (tfile)
        HDfree(tfile);

    return false;

} /* state_init() */

/*
 *  Create the datasets as specified on the command line.
 */
static bool
create_dsets(const state_t *s, dsets_state_t *ds)
{
    hid_t      dcpl = badhid;
    hid_t      dtid = badhid;
    unsigned   i;
    H5R_ref_t *obj_buf  = NULL; /* Buffer for object references */
    hid_t      sid      = badhid;
    hsize_t    obj_dims = OBJ_REF_DIMS; /* Dimension for object reference dataset */
    hsize_t    reg_dims = REG_REF_DIMS; /* Dimension for region reference dataset */

    *ds = DSETS_INITIALIZER;

    /* Create the named datatype that will be used by compact dataset */
    if ((dtid = H5Tcopy(s->filetype)) < 0) {
        HDprintf("H5Tcopy failed\n");
        TEST_ERROR;
    }

    if (H5Tcommit2(s->file, "named_dtype", dtid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) {
        HDprintf("H5Tcommit2 failed\n");
        TEST_ERROR;
    }

    /* Dataset with compact layout, 1d, named datatype */
    if (s->compact) {
        hsize_t dims[1];

        if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
            HDprintf("H5Pcreate failed\n");
            TEST_ERROR;
        }
        if (H5Pset_layout(dcpl, H5D_COMPACT) < 0) {
            HDprintf("H5Pset_layout failed\n");
            TEST_ERROR;
        }

        dims[0] = s->compact_elmts;

        /* Dataspace for compact dataset */
        if ((ds->compact_sid = H5Screate_simple(1, dims, dims)) < 0) {
            HDprintf("H5Screate_simple failed\n");
            TEST_ERROR;
        }

        /* Create the compact dataset with named datatype */
        if ((ds->compact_did = H5Dcreate2(s->file, DSET_COMPACT_NAME, dtid, ds->compact_sid, H5P_DEFAULT,
                                          dcpl, H5P_DEFAULT)) < 0) {
            HDprintf("H5Dcreate2 compact dataset failed\n");
            TEST_ERROR;
        }

        if (H5Pclose(dcpl) < 0) {
            HDprintf("H5Pclose failed\n");
            TEST_ERROR;
        }
    }

    /* Dataset with contiguous layout, 2d */
    if (s->contig) {
        hsize_t dims[2];

        dims[0] = s->rows;
        dims[1] = s->cols;

        /* Dataspace for contiguous dataset */
        if ((ds->contig_sid = H5Screate_simple(2, dims, dims)) < 0) {
            HDprintf("H5Screate_simple failed\n");
            TEST_ERROR;
        }

        if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
            HDprintf("H5Pcreate failed\n");
            TEST_ERROR;
        }

        if (H5Pset_layout(dcpl, H5D_CONTIGUOUS) < 0) {
            HDprintf("H5Pset_layout failed\n");
            TEST_ERROR;
        }

        /* Create the contiguous dataset with DEFAULT datatype */
        if ((ds->contig_did = H5Dcreate2(s->file, DSET_CONTIG_NAME, s->filetype, ds->contig_sid, H5P_DEFAULT,
                                         dcpl, H5P_DEFAULT)) < 0) {
            HDprintf("H5Dcreate2 contiguous dataset failed\n");
            TEST_ERROR;
        }

        if (H5Pclose(dcpl) < 0) {
            HDprintf("H5Pclose failed\n");
            TEST_ERROR;
        }
    }

    /* Dataset with chunked layout, 2d */
    if (s->chunked) {
        hsize_t dims[2];
        hsize_t max_dims[2];
        hsize_t chunk_dims[2];

        dims[0] = s->rows;
        dims[1] = s->cols;

        /* Default chunk size is 2x2 unless s->rows or s->cols indicates otherwise */
        chunk_dims[0] = MAX(1, s->rows / 2);
        chunk_dims[1] = MAX(1, s->cols / 2);

        if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
            HDprintf("H5Pcreate failed\n");
            TEST_ERROR;
        }

        if (H5Pset_layout(dcpl, H5D_CHUNKED) < 0) {
            HDprintf("H5Pset_layout failed\n");
            TEST_ERROR;
        }

        /* Create 2-D chunked dataset with single index */
        /* Chunked, dims=max_dims=chunk_dims */

        if (H5Pset_chunk(dcpl, 2, dims) < 0) {
            HDprintf("H5Pset_chunk failed\n");
            TEST_ERROR;
        }

        if ((ds->single_sid = H5Screate_simple(2, dims, dims)) < 0) {
            HDprintf("H5Screate_simple failed\n");
            TEST_ERROR;
        }

        /* Create the chunked dataset (single index) with the default datatype */
        if ((ds->single_did = H5Dcreate2(s->file, DSET_SINGLE_NAME, s->filetype, ds->single_sid, H5P_DEFAULT,
                                         dcpl, H5P_DEFAULT)) < 0) {
            HDprintf("H5Dcreate2 chunked dataset:single index failed\n");
            TEST_ERROR;
        }

        /* Create 2-D chunked dataset with implicit index */
        /* Chunked, dims=max_dims, early allocation */

        if (H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0) {
            HDprintf("H5Pset_alloc_time\n");
            TEST_ERROR;
        }

        if (H5Pset_chunk(dcpl, 2, chunk_dims) < 0) {
            HDprintf("H5Pset_chunk failed\n");
            TEST_ERROR;
        }

        if ((ds->implicit_sid = H5Screate_simple(2, dims, dims)) < 0) {
            HDprintf("H5Screate_simple failed\n");
            TEST_ERROR;
        }

        /* Create the chunked dataset (implicit index) with the default datatype */
        if ((ds->implicit_did = H5Dcreate2(s->file, DSET_IMPLICIT_NAME, s->filetype, ds->implicit_sid,
                                           H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) {
            HDprintf("H5Dcreate2 chunked dataset:implicit index failed\n");
            TEST_ERROR;
        }

        if (H5Pclose(dcpl) < 0) {
            HDprintf("H5Pclose failed\n");
            TEST_ERROR;
        }

        /* Create 2-D chunked dataset with fixed array index */
        /* Chunked, fixed max_dims */

        if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
            HDprintf("H5Pcreate failed\n");
            TEST_ERROR;
        }

        if (H5Pset_chunk(dcpl, 2, chunk_dims) < 0) {
            HDprintf("H5Pset_chunk failed\n");
            TEST_ERROR;
        }

        max_dims[0] = dims[0] + 10;
        max_dims[1] = dims[1] + 10;

        if ((ds->fa_sid = H5Screate_simple(2, dims, max_dims)) < 0) {
            HDprintf("H5Screate_simple failed\n");
            TEST_ERROR;
        }

        /* Create the chunked dataset (fixed array index) with the default datatype */
        if ((ds->fa_did = H5Dcreate2(s->file, DSET_FA_NAME, s->filetype, ds->fa_sid, H5P_DEFAULT, dcpl,
                                     H5P_DEFAULT)) < 0) {
            HDprintf("H5Dcreaet2 chunked dataset: fa index failed\n");
            TEST_ERROR;
        }

        /* Create 2-D chunked dataset with extensible array index */
        /* Chunked, 1 unlimited max_dims */

        max_dims[1] = H5S_UNLIMITED;
        if ((ds->ea_sid = H5Screate_simple(2, dims, max_dims)) < 0) {
            HDprintf("H5Screate_simple failed\n");
            TEST_ERROR;
        }

        /* Create the chunked dataset (extensible array index) with the default datatype */
        if ((ds->ea_did = H5Dcreate2(s->file, DSET_EA_NAME, s->filetype, ds->ea_sid, H5P_DEFAULT, dcpl,
                                     H5P_DEFAULT)) < 0) {
            HDprintf("H5Dcreate2 chunked dataset: ea index failed\n");
            TEST_ERROR;
        }

        /* Create 2-D chunked dataset with bt2 index */
        /* Chunked, 2 unlimited max_dims */
        max_dims[0] = H5S_UNLIMITED;

        if ((ds->bt2_sid = H5Screate_simple(2, dims, max_dims)) < 0) {
            HDprintf("H5Screate_simple failed\n");
            TEST_ERROR;
        }

        /* Create the chunked dataset (btree2 index) with the default datatype */
        if ((ds->bt2_did = H5Dcreate2(s->file, DSET_BT2_NAME, s->filetype, ds->bt2_sid, H5P_DEFAULT, dcpl,
                                      H5P_DEFAULT)) < 0) {
            HDprintf("H5Dcreate2 chunked dataset: bt2 index failed\n");
            TEST_ERROR;
        }

        if (H5Pclose(dcpl) < 0) {
            HDprintf("H5Pclose failed\n");
            TEST_ERROR;
        }
    }

    if (H5Tclose(dtid) < 0) {
        HDprintf("H5Tclose failed\n");
        TEST_ERROR;
    }

    /* If object reference is enabled: */
    /* create the object reference dataset and the references to the dataset objects */
    if (s->obj_ref) {
        if ((obj_buf = HDcalloc(sizeof(H5R_ref_t), OBJ_REF_DIMS)) == NULL) {
            HDprintf("HDcalloc failed\n");
            TEST_ERROR;
        }
        if ((sid = H5Screate_simple(1, &obj_dims, NULL)) < 0) {
            HDprintf("H5Screate_simple failed\n");
            TEST_ERROR;
        }
        if ((ds->obj_did = H5Dcreate2(s->file, DSET_OBJ_REF_NAME, H5T_STD_REF, sid, H5P_DEFAULT, H5P_DEFAULT,
                                      H5P_DEFAULT)) < 0) {
            HDprintf("H5Dcreate2 object reference dataset failed\n");
            TEST_ERROR;
        }

        if (s->compact)
            if (H5Rcreate_object(s->file, DSET_COMPACT_NAME, H5P_DEFAULT, &obj_buf[0]) < 0) {
                HDprintf("H5Rcreate_object failed\n");
                TEST_ERROR;
            }
        if (s->contig)
            if (H5Rcreate_object(s->file, DSET_CONTIG_NAME, H5P_DEFAULT, &obj_buf[1]) < 0) {
                HDprintf("H5Rcreate_object failed\n");
                TEST_ERROR;
            }
        if (s->chunked) {
            if (H5Rcreate_object(s->file, DSET_SINGLE_NAME, H5P_DEFAULT, &obj_buf[2]) < 0) {
                HDprintf("H5Rcreate_object failed\n");
                TEST_ERROR;
            }

            if (H5Rcreate_object(s->file, DSET_IMPLICIT_NAME, H5P_DEFAULT, &obj_buf[3]) < 0) {
                HDprintf("H5Rcreate_object failed\n");
                TEST_ERROR;
            }
            if (H5Rcreate_object(s->file, DSET_FA_NAME, H5P_DEFAULT, &obj_buf[4]) < 0) {
                HDprintf("H5Rcreate_object failed\n");
                TEST_ERROR;
            }
            if (H5Rcreate_object(s->file, DSET_EA_NAME, H5P_DEFAULT, &obj_buf[5]) < 0) {
                HDprintf("H5Rcreate_object failed\n");
                TEST_ERROR;
            }
            if (H5Rcreate_object(s->file, DSET_BT2_NAME, H5P_DEFAULT, &obj_buf[6]) < 0) {
                HDprintf("H5Rcreate_object failed\n");
                TEST_ERROR;
            }
        }

        /* Write the object references to the reference dataset */
        if (H5Dwrite(ds->obj_did, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, obj_buf) < 0) {
            HDprintf("H5Dwrite object reference dataset failed\n");
            TEST_ERROR;
        }

        /* Destroy the references */
        for (i = 0; i < OBJ_REF_DIMS; i++)
            if (H5Rdestroy(&obj_buf[i]) < 0) {
                HDprintf("H5Rdestroy object reference failed\n");
                TEST_ERROR;
            }

        if (H5Sclose(sid) < 0) {
            HDprintf("H5Sclose \n");
            TEST_ERROR;
        }

        if (obj_buf)
            HDfree(obj_buf);
    }

    /* If region reference is enabled: */
    /* create the region reference dataset and allocate the buffer for holding the references */
    if (s->reg_ref) {
        if ((sid = H5Screate_simple(1, &reg_dims, NULL)) < 0) {
            HDprintf("H5Screate_simple failed\n");
            TEST_ERROR;
        }

        if ((ds->reg_did = H5Dcreate2(s->file, DSET_REG_REF_NAME, H5T_STD_REF, sid, H5P_DEFAULT, H5P_DEFAULT,
                                      H5P_DEFAULT)) < 0) {
            HDprintf("H5Dcreate2 failed\n");
            TEST_ERROR;
        }

        if (H5Sclose(sid) < 0) {
            HDprintf("H5Sclose failed\n");
            TEST_ERROR;
        }

        if ((ds->reg_buf = HDcalloc(sizeof(H5R_ref_t), REG_REF_DIMS)) == NULL) {
            HDprintf("HDcalloc failed\n");
            TEST_ERROR;
        }
    }

    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(dcpl);
        H5Tclose(dtid);
        H5Sclose(ds->compact_sid);
        H5Sclose(ds->contig_sid);
        H5Sclose(ds->single_sid);
        H5Sclose(ds->implicit_sid);
        H5Sclose(ds->fa_sid);
        H5Sclose(ds->ea_sid);
        H5Sclose(ds->bt2_sid);
        H5Sclose(sid);
        H5Dclose(ds->compact_did);
        H5Dclose(ds->contig_did);
        H5Dclose(ds->single_did);
        H5Sclose(ds->implicit_did);
        H5Dclose(ds->fa_did);
        H5Dclose(ds->ea_did);
        H5Dclose(ds->bt2_did);
        H5Dclose(ds->obj_did);
        H5Dclose(ds->reg_did);
    }
    H5E_END_TRY;

    if (obj_buf)
        HDfree(obj_buf);
    if (ds->reg_buf)
        HDfree(ds->reg_buf);

    return false;

} /* create_dsets() */

/*
 * Open the datasets as specified.
 */
static bool
open_dsets(const state_t *s, dsets_state_t *ds)
{
    H5R_ref_t *obj_buf = NULL; /* Buffer for holding object references */
    unsigned   i;

    *ds = DSETS_INITIALIZER;

    /* If object reference is specified: */
    /* open the object reference dataset and retrieve the dataset object references */
    if (s->obj_ref) {
        if ((ds->obj_did = H5Dopen2(s->file, DSET_OBJ_REF_NAME, H5P_DEFAULT)) < 0) {
            HDprintf("HDopen2 object reference dataset failed\n");
            TEST_ERROR;
        }

        if (H5Drefresh(ds->obj_did) < 0) {
            HDprintf("HDrefresh failed\n");
            TEST_ERROR;
        }

        if ((obj_buf = HDcalloc(sizeof(H5R_ref_t), OBJ_REF_DIMS)) == NULL) {
            HDprintf("HDcalloc failed\n");
            TEST_ERROR;
        }

        /* Obtain the object references */
        if (H5Dread(ds->obj_did, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, obj_buf) < 0) {
            HDprintf("HDread failed\n");
            TEST_ERROR;
        }
    }

    /* If region reference is specified: */
    /* open the region reference dataset and allocate buffer for holding the region references */
    if (s->reg_ref) {
        if ((ds->reg_did = H5Dopen2(s->file, DSET_REG_REF_NAME, H5P_DEFAULT)) < 0) {
            HDprintf("HDopen2 region reference dataset failed\n");
            TEST_ERROR;
        }

        /* Allocate the buffer for holding the region references */
        if ((ds->reg_buf = HDcalloc(sizeof(H5R_ref_t), REG_REF_DIMS)) == NULL) {
            HDprintf("HDcalloc failed\n");
            TEST_ERROR;
        }
    }

    if (s->compact) {
        if (!open_dset_real(s, &ds->compact_did, &ds->compact_sid, DSET_COMPACT_NAME,
                            s->obj_ref ? &obj_buf[0] : NULL)) {
            HDprintf("open_dset_real() for compact dataset failed\n");
            TEST_ERROR;
        }
    }

    if (s->contig) {

        if (!open_dset_real(s, &ds->contig_did, &ds->contig_sid, DSET_CONTIG_NAME,
                            s->obj_ref ? &obj_buf[1] : NULL)) {
            HDprintf("open_dset_real() for contiguous dataset failed\n");
            TEST_ERROR;
        }
    }

    if (s->chunked) {

        if (!open_dset_real(s, &ds->single_did, &ds->single_sid, DSET_SINGLE_NAME,
                            s->obj_ref ? &obj_buf[2] : NULL)) {
            HDprintf("open_dset_real() for chunked dataset: single index failed\n");
            TEST_ERROR;
        }

        if (!open_dset_real(s, &ds->implicit_did, &ds->implicit_sid, DSET_IMPLICIT_NAME,
                            s->obj_ref ? &obj_buf[3] : NULL)) {
            HDprintf("open_dset_real() for chunked dataset: implicit index failed\n");
            TEST_ERROR;
        }

        if (!open_dset_real(s, &ds->fa_did, &ds->fa_sid, DSET_FA_NAME, s->obj_ref ? &obj_buf[4] : NULL)) {
            HDprintf("open_dset_real() for chunked dataset: fa index failed\n");
            TEST_ERROR;
        }

        if (!open_dset_real(s, &ds->ea_did, &ds->ea_sid, DSET_EA_NAME, s->obj_ref ? &obj_buf[5] : NULL)) {
            HDprintf("open_dset_real() for chunked dataset: ea index failed\n");
            TEST_ERROR;
        }

        if (!open_dset_real(s, &ds->bt2_did, &ds->bt2_sid, DSET_BT2_NAME, s->obj_ref ? &obj_buf[6] : NULL)) {
            HDprintf("open_dset_real() for chunked dataset: bt2 index failed\n");
            TEST_ERROR;
        }
    }

    /* Destroy the object references */
    if (s->obj_ref) {
        for (i = 0; i < OBJ_REF_DIMS; i++)
            if (H5Rdestroy(&obj_buf[i]) < 0) {
                HDprintf("H5Rdestroy object references\n");
                TEST_ERROR;
            }

        if (obj_buf)
            HDfree(obj_buf);
    }

    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(ds->obj_did);
        H5Dclose(ds->reg_did);
    }
    H5E_END_TRY;

    if (obj_buf)
        HDfree(obj_buf);
    if (ds->reg_buf)
        HDfree(ds->reg_buf);

    return false;

} /* open_dsets() */

/*
 * Do the real work of opening the dataset.
 * Verify the dimension sizes are as expected.
 */
static bool
open_dset_real(const state_t *s, hid_t *did, hid_t *sid, const char *name, H5R_ref_t *obj_buf)
{
    hsize_t dims[2];

    /* If object reference is enabled, obtain the dataset object ID
        via H5Ropen_object(). */
    if (s->obj_ref) {
        H5E_BEGIN_TRY
        {
            *did = H5Ropen_object(obj_buf, H5P_DEFAULT, H5P_DEFAULT);
        }
        H5E_END_TRY;
        if (*did < 0) {
            HDprintf("H5Ropen_object failed\n");
            TEST_ERROR;
        }
    }
    else {
        HDassert(obj_buf == NULL);

        if ((*did = H5Dopen2(s->file, name, H5P_DEFAULT)) < 0) {
            HDprintf("H5Dopen dataset failed\n");
            TEST_ERROR;
        }
    }

    if ((*sid = H5Dget_space(*did)) < 0) {
        HDprintf("H5Dget_space failed\n");
        TEST_ERROR;
    }
    if (H5Sget_simple_extent_dims(*sid, dims, NULL) < 0)
        TEST_ERROR;

    if (!HDstrcmp(name, DSET_COMPACT_NAME)) {
        if (dims[0] != s->compact_elmts)
            TEST_ERROR;
    }
    else { /* contiguous or chunked dataset */
        if (dims[0] != s->rows)
            TEST_ERROR;
        if (dims[1] != s->cols)
            TEST_ERROR;
    }

    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(*did);
        H5Dclose(*sid);
    }
    H5E_END_TRY;

    return false;
} /* open_dset_real() */

/*
 * Close all the datasets and dataspaces as specified.
 */
static bool
close_dsets(dsets_state_t *ds)
{
    if (!close_dset_real(ds->compact_did, ds->compact_sid)) {
        HDprintf("H5Dclose compact dataset failed\n");
        TEST_ERROR;
    }

    if (!close_dset_real(ds->contig_did, ds->contig_sid)) {
        HDprintf("H5Dclose contiguous dataset failed\n");
        TEST_ERROR;
    }

    if (!close_dset_real(ds->single_did, ds->single_sid)) {
        HDprintf("H5Dclose chunked dataset: single index failed\n");
        TEST_ERROR;
    }

    if (!close_dset_real(ds->implicit_did, ds->implicit_sid)) {
        HDprintf("H5Dclose chunked dataset: implicit index failed\n");
        TEST_ERROR;
    }

    if (!close_dset_real(ds->fa_did, ds->fa_sid)) {
        HDprintf("H5Dclose chunked dataset: fa index failed\n");
        TEST_ERROR;
    }

    if (!close_dset_real(ds->ea_did, ds->ea_sid)) {
        HDprintf("H5Dclose chunked dataset: ea index failed\n");
        TEST_ERROR;
    }

    if (!close_dset_real(ds->bt2_did, ds->bt2_sid)) {
        HDprintf("H5Dclose chunked dataset: bt2 index failed\n");
        TEST_ERROR;
    }

    if (ds->reg_buf) {
        HDfree(ds->reg_buf);
        ds->reg_buf = NULL;
    }

    return true;

error:
    return false;

} /* close_dsets() */

/*
 * Do the real work of closing the dataset.
 */
static bool
close_dset_real(hid_t did, hid_t sid)
{
    if (did != badhid && H5Dclose(did) < 0) {
        HDprintf("H5Dclose dataset failed\n");
        TEST_ERROR;
    }

    if (sid != badhid && H5Sclose(sid) < 0) {
        HDprintf("H5Sclose dataspace for dataset failed\n");
        TEST_ERROR;
    }

    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(did);
        H5Sclose(sid);
    }
    H5E_END_TRY;

    return false;
} /* close_dset_real() */

/*
 *  Writer
 */

/*
 * Write to whole compact dataset
 *
 * Perform writes for contiguous and chunked datasets:
 *  --SEQ_WRITE: sequential writes
 *  --RANDOM_WRITE: random writes
 *  --HYPER_WRITE: hyperslab writes
 *  --MODIFY_DATA: raw data modifications
 */
static bool
perform_dsets_operations(state_t *s, dsets_state_t *ds, H5F_vfd_swmr_config_t *config, np_state_t *np)
{
    unsigned step;
    bool     result;

    /* Perform writes to the whole compact dataset */
    if (s->compact) {

        if (s->compact_write) {
            dbgf(2, "Writes all to compact dataset\n");

            result = write_dset_compact(s, ds);

            if (s->use_np && !np_writer(result, 0, s, np, config)) {
                HDprintf("np_writer() for addition failed\n");
                TEST_ERROR;
            }
        }
    }

    /* Perform writes for contiguous and/or chunked datasets */
    if (s->contig || s->chunked) {

        /* Perform sequential writes */
        if (s->swrites) {

            for (step = 0; (step < s->swrites && step < (s->rows * s->cols)); step++) {
                dbgf(2, "Sequential writes %u to dataset\n", step);

                result = dsets_action(SEQ_WRITE, s, ds, step);

                if (s->use_np && !np_writer(result, step, s, np, config)) {
                    HDprintf("np_writer() for sequential writes failed\n");
                    TEST_ERROR;
                }
            }
        }

        /* Perform random writes */
        if (s->rwrites) {
            unsigned newstep;

            /* Set up random seed which will be the same for both writer and reader */
            HDsrandom(RANDOM_SEED);

            for (step = 0; (step < s->rwrites && step < (s->rows * s->cols)); step++) {
                dbgf(2, "Random writes %u to dataset\n", step);

                newstep = (unsigned int)HDrandom() % (s->rows * s->cols);
                dbgf(2, "Random step is %u\n", newstep);
                result = dsets_action(RANDOM_WRITE, s, ds, newstep);

                if (s->use_np && !np_writer(result, step, s, np, config)) {
                    HDprintf("np_writer() for random writes failed\n");
                    TEST_ERROR;
                }
            }
        }

        /* Perform hyperslab writes */
        if (s->lwrites) {
            unsigned k;

            for (step = 0, k = 0; (step < s->lwrites && k < (s->rows * s->cols)); step++, k += s->cols) {
                dbgf(2, "Hyperslab writes %u to dataset\n", step);

                result = dsets_action(HYPER_WRITE, s, ds, k);

                if (s->use_np && !np_writer(result, step, s, np, config)) {
                    HDprintf("np_writer() for hyperslab writes failed\n");
                    TEST_ERROR;
                }
            }
        }

        /* Perform raw data modifications */
        if (s->wwrites) {

            for (step = 0; (step < s->wwrites && step < (s->rows * s->cols)); step++) {
                dbgf(2, "Modify raw data %u to dataset\n", step);

                result = dsets_action(MODIFY_DATA, s, ds, step);

                if (s->use_np && !np_writer(result, step, s, np, config)) {
                    HDprintf("np_writer() for modify raw data failed\n");
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
 * Perform the "action" for each of the datasets specified on the command line:
 *      SEQ_WRITE: perform `which` sequential write
 *      RANDOM_WRITE: perform `which` random write
 *      HYPER_WRITE: perform `which` hyperslab write
 *      MODIFY_DATA: perform `which` raw data modification
 */
static bool
dsets_action(unsigned action, const state_t *s, const dsets_state_t *ds, unsigned which)
{
    hsize_t       start[2];
    hsize_t       stride[2];
    hsize_t       count[2];
    hsize_t       block[2];
    hid_t         mem_sid;
    unsigned int *wbuf = NULL;
    unsigned      i;

    /* Set up selection, dataspace and data buffer according to the specified action */
    if (!dset_setup(action, which, s, start, stride, count, block, &mem_sid, &wbuf)) {
        HDprintf("dset_setup() failed\n");
        TEST_ERROR;
    }

    /* Write to the contiguous dataset */
    if (s->contig) {

        if (!write_dset(s, DSET_CONTIG_NAME, ds->contig_did, ds->contig_sid, mem_sid, start, stride, count,
                        block, wbuf, s->reg_ref ? &ds->reg_buf[0] : NULL)) {
            HDprintf("H5Dwrite to contiguous dataset failed\n");
            TEST_ERROR;
        }
    }

    /* Write to the 5 chunked datasets */
    if (s->chunked) {

        if (!write_dset(s, DSET_SINGLE_NAME, ds->single_did, ds->single_sid, mem_sid, start, stride, count,
                        block, wbuf, s->reg_ref ? &ds->reg_buf[1] : NULL)) {
            HDprintf("H5Dwrite to chunked dataset: single index dataset failed\n");
            TEST_ERROR;
        }

        if (!write_dset(s, DSET_IMPLICIT_NAME, ds->implicit_did, ds->implicit_sid, mem_sid, start, stride,
                        count, block, wbuf, s->reg_ref ? &ds->reg_buf[2] : NULL)) {
            HDprintf("H5Dwrite to chunked dataset: implicit index dataset failed\n");
            TEST_ERROR;
        }

        if (!write_dset(s, DSET_FA_NAME, ds->fa_did, ds->fa_sid, mem_sid, start, stride, count, block, wbuf,
                        s->reg_ref ? &ds->reg_buf[3] : NULL)) {
            HDprintf("H5Dwrite to chunked dataset: fa index dataset failed\n");
            TEST_ERROR;
        }

        if (!write_dset(s, DSET_EA_NAME, ds->ea_did, ds->ea_sid, mem_sid, start, stride, count, block, wbuf,
                        s->reg_ref ? &ds->reg_buf[4] : NULL)) {
            HDprintf("H5Dwrite to chunked dataset: ea index dataset failed\n");
            TEST_ERROR;
        }

        if (!write_dset(s, DSET_BT2_NAME, ds->bt2_did, ds->bt2_sid, mem_sid, start, stride, count, block,
                        wbuf, s->reg_ref ? &ds->reg_buf[5] : NULL)) {
            HDprintf("H5Dwrite to chunked dataset: bt2 index dataset failed\n");
            TEST_ERROR;
        }
    }

    /* If region reference is enabled, store the region references to the reference dataset */
    if (s->reg_ref) {
        if (H5Dwrite(ds->reg_did, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, ds->reg_buf) < 0) {
            HDprintf("H5Dwrite failed\n");
            TEST_ERROR;
        }
        for (i = 0; i < 6; i++)
            if (H5Rdestroy(&ds->reg_buf[i]) < 0) {
                HDprintf("H5Rdestroy failed\n");
                TEST_ERROR;
            }
    }

    if (wbuf)
        HDfree(wbuf);

    return true;

error:
    if (wbuf)
        HDfree(wbuf);
    return false;

} /* dsets_action() */

/*
 *  Set up selection info: start, stride, count, block
 *  Set up the memory dataspace
 *  Initialize the data buffer
 */
static bool
dset_setup(unsigned action, unsigned which, const state_t *s, hsize_t *start, hsize_t *stride, hsize_t *count,
           hsize_t *block, hid_t *mem_sid, unsigned int **buf)
{
    hsize_t       mem_dims[1];
    unsigned      kk, i;
    unsigned int *tmp_buf;

    start[0] = which / s->cols;
    start[1] = which % s->cols;
    block[0] = block[1] = 1;

    /* For SEQ_WRITE, RANDOM_WRITE, MODIFY_DATA: writing 1 element at a time */
    /* For HYPER_WRITE: writing 1 row at a time: selecting every other element per row */
    switch (action) {
        case SEQ_WRITE:
        case RANDOM_WRITE:
        case MODIFY_DATA:
            count[0] = count[1] = 1;
            stride[0] = stride[1] = 1;
            mem_dims[0]           = 1;
            *mem_sid              = H5Screate_simple(1, mem_dims, NULL);
            break;

        case HYPER_WRITE:
            count[0]  = 1;
            count[1]  = MAX(1, s->cols / 2);
            stride[0] = 1;
            stride[1] = 2;

            mem_dims[0] = count[1];
            *mem_sid    = H5Screate_simple(1, mem_dims, NULL);
            break;

        default:
            HDassert(0 && "Unknown action?!?");
    } /* end switch */

    /* Allocating the buffer for writing */
    if ((tmp_buf = HDmalloc(count[1] * sizeof(unsigned int))) == NULL) {
        HDprintf("HDmalloc failed\n");
        TEST_ERROR;
    }

    /* Initialize the data in the buffer */
    switch (action) {
        case SEQ_WRITE:
            tmp_buf[0] = which + 1;
            break;

        case RANDOM_WRITE:
            tmp_buf[0] = 777;
            break;

        case MODIFY_DATA:
            tmp_buf[0] = 999;
            break;

        case HYPER_WRITE:
            kk = which + 1;
            for (i = 0; i < count[1]; i++) {
                tmp_buf[i] = kk;
                kk += 2;
            }

            break;

        default:
            HDassert(0 && "Unknown action?!?");
    } /* end switch */

    *buf = tmp_buf;
    return true;

error:
    return false;

} /* dset_setup() */

/*
 * Make the selection and then write to the dataset.
 */
static bool
write_dset(const state_t *s, const char *name, hid_t did, hid_t sid, hid_t mem_sid, hsize_t *start,
           hsize_t *stride, hsize_t *count, hsize_t *block, unsigned int *buf, H5R_ref_t *reg_buf)
{
    if (H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, stride, count, block) < 0) {
        HDprintf("H5Sselect to dataset failed\n");
        TEST_ERROR;
    }

    if (H5Dwrite(did, s->filetype, mem_sid, sid, H5P_DEFAULT, buf) < 0) {
        HDprintf("H5Dwrite to dataset failed\n");
        TEST_ERROR;
    }

    /* If region reference is enabled, store the region reference to the reference buffer */
    if (s->reg_ref) {
        HDassert(reg_buf != NULL);
        if (H5Rcreate_region(s->file, name, sid, H5P_DEFAULT, reg_buf) < 0) {
            HDprintf("H5Rcreate_region failed\n");
            TEST_ERROR;
        }
    }

    return true;

error:
    return false;

} /* write_dset() */

/*
 *  Write to whole compact dataset.
 */
static bool
write_dset_compact(const state_t *s, const dsets_state_t *ds)
{
    unsigned int *buf = NULL;
    unsigned      i;

    if ((buf = HDmalloc(s->compact_elmts * sizeof(unsigned int))) == NULL) {
        HDprintf("HDmalloc buffer for compact dataset failed\n");
        goto error;
    }

    for (i = 0; i < s->compact_elmts; i++)
        buf[i] = i + 1;

    if (H5Dwrite(ds->compact_did, s->filetype, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) {
        HDprintf("H5Dwrite to compact dataset failed\n");
        TEST_ERROR;
    }

    if (buf)
        HDfree(buf);
    return true;

error:
    if (buf)
        HDfree(buf);
    return false;

} /* write_dset_compact() */

/*
 * Reader
 */

/*
 * Verify writes to the compact dataset.
 *
 * Verify writes for contiguous and chunked datasets:
 *  --SEQ_WRITE: sequential writes
 *  --RANDOM_WRITE: random writes
 *  --HYPER_WRITE: hyperslab writes
 *  --MODIFY_DATA: raw data modifications
 */
static bool
verify_dsets_operations(state_t *s, dsets_state_t *ds, H5F_vfd_swmr_config_t *config, np_state_t *np,
                        bool fileclosed)
{
    unsigned step;
    bool     result;

    /* Start verifying data written to the compact dataset */
    if (s->compact) {

        if (s->compact_write) {
            dbgf(2, "Verify writes to compact dataset\n");

            if (s->use_np && !np_confirm_verify_notify(np->fd_writer_to_reader, 0, s, np)) {
                HDprintf("np_confirm_verify_notify() verify/notify not in sync failed\n");
                TEST_ERROR;
            }

            /* Wait for a few ticks for the update to happen */
            if (!fileclosed)
                decisleep(config->tick_len * s->update_interval);

            result = verify_dset_compact(s, ds, fileclosed, config->flush_raw_data);

            if (s->use_np && !np_reader(result, 0, s, np)) {
                HDprintf("np_reader() for verifying writes failed\n");
                TEST_ERROR;
            }
            else if (!result)
                TEST_ERROR;
        }
    }

    /* Verify writes for contiguous and/or chunked datasets */
    if (s->contig || s->chunked) {

        /* Start verifying sequential writes */
        /* When flush of raw data is disabled, only verify data for the last write operation on file close */
        if ((s->swrites && !fileclosed) || (fileclosed && s->lastwrite == SEQ_WRITE)) {

            s->lastwrite = SEQ_WRITE;

            for (step = 0; (step < s->swrites && step < (s->rows * s->cols)); step++) {
                dbgf(2, "Verify sequential writes %u to dataset\n", step);

                if (s->use_np && !np_confirm_verify_notify(np->fd_writer_to_reader, step, s, np)) {
                    HDprintf("np_confirm_verify_notify() verify/notify not in sync failed\n");
                    TEST_ERROR;
                }

                /* Wait for a few ticks for the update to happen */
                if (!fileclosed)
                    decisleep(config->tick_len * s->update_interval);

                result = verify_dsets_action(SEQ_WRITE, s, ds, step, fileclosed);

                if (s->use_np && !np_reader(result, step, s, np)) {
                    HDprintf("np_reader() for verifying writes failed\n");
                    TEST_ERROR;
                }
                else if (!result)
                    TEST_ERROR;
            }
        }

        /* Start verifying random writes */
        /* When flush of raw data is disabled, only verify data for the last write operation on file close */
        if ((s->rwrites && !fileclosed) || (fileclosed && s->lastwrite == RANDOM_WRITE)) {
            unsigned newstep;

            s->lastwrite = RANDOM_WRITE;

            /* Set up random seed which will be the same for both writer and reader */
            HDsrandom(RANDOM_SEED);

            for (step = 0; (step < s->rwrites && step < (s->rows * s->cols)); step++) {
                dbgf(2, "Verify random writes %u to dataset\n", step);

                newstep = (unsigned int)HDrandom() % (s->rows * s->cols);
                dbgf(2, "Random step is %u\n", newstep);

                if (s->use_np && !np_confirm_verify_notify(np->fd_writer_to_reader, step, s, np)) {
                    HDprintf("np_confirm_verify_notify() verify/notify not in sync failed\n");
                    TEST_ERROR;
                }

                /* Wait for a few ticks for the update to happen */
                if (!fileclosed)
                    decisleep(config->tick_len * s->update_interval);

                result = verify_dsets_action(RANDOM_WRITE, s, ds, newstep, fileclosed);

                if (s->use_np && !np_reader(result, step, s, np)) {
                    HDprintf("np_reader() for verifying writes failed\n");
                    TEST_ERROR;
                }
                else if (!result)
                    TEST_ERROR;
            }
        }

        /* Start verifying hyperslab writes */
        /* When flush of raw data is disabled, only verify data for the last write operation on file close */
        if ((s->lwrites && !fileclosed) || (fileclosed && s->lastwrite == HYPER_WRITE)) {
            unsigned k;

            s->lastwrite = HYPER_WRITE;

            for (step = 0, k = 0; (step < s->lwrites && k < (s->rows * s->cols)); step++, k += s->cols) {
                dbgf(2, "Verify hyperslab writes %u to dataset\n", step);

                if (s->use_np && !np_confirm_verify_notify(np->fd_writer_to_reader, step, s, np)) {
                    HDprintf("np_confirm_verify_notify() verify/notify not in sync failed\n");
                    TEST_ERROR;
                }

                /* Wait for a few ticks for the update to happen */
                if (!fileclosed)
                    decisleep(config->tick_len * s->update_interval);

                result = verify_dsets_action(HYPER_WRITE, s, ds, k, fileclosed);

                if (s->use_np && !np_reader(result, step, s, np)) {
                    HDprintf("np_reader() for verifying writes failed\n");
                    TEST_ERROR;
                }
                else if (!result)
                    TEST_ERROR;
            }
        }

        /* Start verifying raw data modifications */
        /* When flush of raw data is disabled, only verify data for the last write operation on file close */
        if ((s->wwrites && !fileclosed) || (fileclosed && s->lastwrite == MODIFY_DATA)) {

            s->lastwrite = MODIFY_DATA;

            for (step = 0; (step < s->wwrites && step < (s->rows * s->cols)); step++) {
                dbgf(2, "Verify raw data modification %u to dataset\n", step);

                if (s->use_np && !np_confirm_verify_notify(np->fd_writer_to_reader, step, s, np)) {
                    HDprintf("np_confirm_verify_notify() verify/notify not in sync failed\n");
                    TEST_ERROR;
                }

                /* Wait for a few ticks for the update to happen */
                if (!fileclosed)
                    decisleep(config->tick_len * s->update_interval);

                result = verify_dsets_action(MODIFY_DATA, s, ds, step, fileclosed);

                if (s->use_np && !np_reader(result, step, s, np)) {
                    HDprintf("np_reader() for verifying writes failed\n");
                    TEST_ERROR;
                }
                else if (!result)
                    TEST_ERROR;
            }
        }
    }

    return true;

error:
    return false;

} /* verify_dsets_operations() */

/*
 * Verify the data read from each of the datasets specified on the command line
 * according to "action":
 *      SEQ_WRITE: `which` sequential write
 *      RANDOM_WRITE: `which` random write
 *      HYPER_WRITE: `which` hyperslab write
 *      MODIFY_DATA: `which` raw data modification
 */
static bool
verify_dsets_action(unsigned action, const state_t *s, const dsets_state_t *ds, unsigned which,
                    bool fileclosed)
{
    hsize_t       start[2];
    hsize_t       stride[2];
    hsize_t       count[2];
    hsize_t       block[2];
    hid_t         mem_sid;
    unsigned int *vbuf = NULL;
    unsigned      i;

    /* If region reference is enabled, obtain the region references from the reference dataset */
    if (s->reg_ref) {
        if (H5Drefresh(ds->reg_did) < 0) {
            HDprintf("H5Drefresh region reference dataset failed\n");
            TEST_ERROR;
        }
        if (H5Dread(ds->reg_did, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, ds->reg_buf) < 0) {
            HDprintf("H5Dread failed\n");
            TEST_ERROR;
        }
    }

    /* Set up selection, dataspace and data buffer according to the specified action */
    if (!dset_setup(action, which, s, start, stride, count, block, &mem_sid, &vbuf)) {
        HDprintf("dset_setup() failed\n");
        TEST_ERROR;
    }

    /* Verify the data read for the contiguous dataset */
    if (s->contig) {

        if (!verify_dset(ds->contig_did, s->filetype, ds->contig_sid, mem_sid, start, stride, count, block,
                         vbuf, fileclosed, s->flush_raw_data, s->reg_ref ? &ds->reg_buf[0] : NULL)) {
            HDprintf("verify_dset() to contiguous dataset failed\n");
            TEST_ERROR;
        }
    }

    /* Verify the data read for the chunked datasets */
    if (s->chunked) {

        if (!verify_dset(ds->single_did, s->filetype, ds->single_sid, mem_sid, start, stride, count, block,
                         vbuf, fileclosed, s->flush_raw_data, s->reg_ref ? &ds->reg_buf[1] : NULL)) {
            HDprintf("verify_dset() to chunked dataset: single index dataset failed\n");
            TEST_ERROR;
        }

        if (!verify_dset(ds->implicit_did, s->filetype, ds->implicit_sid, mem_sid, start, stride, count,
                         block, vbuf, fileclosed, s->flush_raw_data, s->reg_ref ? &ds->reg_buf[2] : NULL)) {
            HDprintf("verify_dset() to chunked dataset: implicit index dataset failed\n");
            TEST_ERROR;
        }

        if (!verify_dset(ds->fa_did, s->filetype, ds->fa_sid, mem_sid, start, stride, count, block, vbuf,
                         fileclosed, s->flush_raw_data, s->reg_ref ? &ds->reg_buf[3] : NULL)) {
            HDprintf("verify_dset() to chunked dataset: fa index dataset failed\n");
            TEST_ERROR;
        }

        if (!verify_dset(ds->ea_did, s->filetype, ds->ea_sid, mem_sid, start, stride, count, block, vbuf,
                         fileclosed, s->flush_raw_data, s->reg_ref ? &ds->reg_buf[4] : NULL)) {
            HDprintf("verify_dset() to chunked dataset: ea index dataset failed\n");
            TEST_ERROR;
        }

        if (!verify_dset(ds->bt2_did, s->filetype, ds->bt2_sid, mem_sid, start, stride, count, block, vbuf,
                         fileclosed, s->flush_raw_data, s->reg_ref ? &ds->reg_buf[5] : NULL)) {
            HDprintf("verify_dset() to chunked dataset: bt2 index dataset failed\n");
            TEST_ERROR;
        }
    }

    if (vbuf)
        HDfree(vbuf);

    if (s->reg_ref) {
        for (i = 0; i < 6; i++)
            if (H5Rdestroy(&ds->reg_buf[i]) < 0) {
                HDprintf("H5Rdestroy failed\n");
                TEST_ERROR;
            }
    }

    return true;

error:
    if (vbuf)
        HDfree(vbuf);

    return false;

} /* verify_dsets_action() */

/*
 * Verify the data read from the dataset is as expected.
 * `vbuf` contains the data expected from the read.
 */
static bool
verify_dset(hid_t did, hid_t tid, hid_t sid, hid_t mem_sid, hsize_t *start, hsize_t *stride, hsize_t *count,
            hsize_t *block, unsigned int *vbuf, bool fileclosed, bool flush_raw_data, H5R_ref_t *reg_buf)
{
    unsigned int *rbuf = NULL;
    unsigned      i;

    /* Refresh the dataset */
    if (H5Drefresh(did) < 0) {
        HDprintf("H5Drefresh dataset failed\n");
        TEST_ERROR;
    }

    /* Allocate the buffer for reading */
    if ((rbuf = HDmalloc(count[1] * sizeof(unsigned int))) == NULL) {
        HDprintf("HDmalloc failed\n");
        TEST_ERROR;
    }

    /* If region reference is enabled, obtain the dataset selection from the reference buffer */
    if (reg_buf != NULL) { /* Imply region reference is enabled */
        hid_t temp_sid = badhid;

        H5E_BEGIN_TRY
        {
            temp_sid = H5Ropen_region(reg_buf, H5P_DEFAULT, H5P_DEFAULT);
        }
        H5E_END_TRY;

        if (temp_sid < 0) {
            HDprintf("H5Ropen_region failed\n");
            TEST_ERROR;
        }
        sid = temp_sid;
    }
    else {
        /* Make the selection the file dataspace */
        if (H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, stride, count, block) < 0) {
            HDprintf("H5Sselect_hyperslab failed\n");
            TEST_ERROR;
        }
    }

    /* Read the data from the dataset into `rbuf` */
    if (H5Dread(did, tid, mem_sid, sid, H5P_DEFAULT, rbuf) < 0) {
        HDprintf("H5Dread from dataset failed\n");
        TEST_ERROR;
    }

    /* Verify the data read in `rbuf` is as `vbuf` */
    for (i = 0; i < count[1]; i++) {
        if (flush_raw_data || fileclosed) {
            if (rbuf[i] != vbuf[i])
                TEST_ERROR;
        }
        else {                                      /* No flush && not closing file */
            if (rbuf[i] != vbuf[i] && rbuf[0] != 0) /* FILL VALUE ?? */
                TEST_ERROR;
        }
    }

    if (rbuf)
        HDfree(rbuf);

    return true;

error:
    if (rbuf)
        HDfree(rbuf);
    return false;

} /* verify_dset() */

/*
 * Verify that the data read from the compact dataset is as unexpected.
 */
static bool
verify_dset_compact(const state_t *s, const dsets_state_t *ds, bool fileclosed, bool flush_raw_data)
{
    unsigned int *rbuf;
    unsigned      i;

    /* Refresh the dataset */
    if (H5Drefresh(ds->compact_did) < 0) {
        HDprintf("H5Drefresh dataset failed\n");
        TEST_ERROR;
    }

    if ((rbuf = HDmalloc(s->compact_elmts * sizeof(unsigned int))) == NULL) {
        HDprintf("HDmalloc buffer for compact dataset failed\n");
        goto error;
    }

    if (H5Dread(ds->compact_did, s->filetype, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0) {
        HDprintf("H5Dwrite to compact dataset failed\n");
        TEST_ERROR;
    }

    for (i = 0; i < s->compact_elmts; i++) {
        if (flush_raw_data || fileclosed) {
            if (rbuf[i] != (i + 1)) {
                HDprintf("Invalid value for compact dataset element\n");
                TEST_ERROR;
            }
        }
        else {                                      /* No flush && not closing file */
            if (rbuf[i] != (i + 1) && rbuf[0] != 0) /* FILL VALUE ?? */
                TEST_ERROR;
        }
    }

    if (rbuf)
        HDfree(rbuf);

    return true;

error:
    if (rbuf)
        HDfree(rbuf);
    return false;

} /* verify_dset_compact() */

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
                HDprintf("HDremove fifo_writer_to_reader failed\n");
                TEST_ERROR;
            }

        if (HDaccess(np->fifo_reader_to_writer, F_OK) == 0)
            if (HDremove(np->fifo_reader_to_writer) != 0) {
                HDprintf("HDremove fifo_reader_to_writer failed\n");
                TEST_ERROR;
            }

        /* Writer creates two named pipes(FIFO) */
        if (HDmkfifo(np->fifo_writer_to_reader, 0600) < 0) {
            HDprintf("HDmkfifo fifo_writer_to_reader failed\n");
            TEST_ERROR;
        }

        if (HDmkfifo(np->fifo_reader_to_writer, 0600) < 0) {
            HDprintf("HDmkfifo fifo_reader_to_writer failed\n");
            TEST_ERROR;
        }
    }

    /* Both the writer and reader open the pipes */
    if ((np->fd_writer_to_reader = HDopen(np->fifo_writer_to_reader, O_RDWR)) < 0) {
        HDprintf("HDopen fifo_writer_to_reader failed\n");
        TEST_ERROR;
    }

    if ((np->fd_reader_to_writer = HDopen(np->fifo_reader_to_writer, O_RDWR)) < 0) {
        HDprintf("HDopen fifo_reader_to_writer failed\n");
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
        HDprintf("HDclose fd_writer_to_reader failed\n");
        TEST_ERROR;
    }

    if (HDclose(np->fd_reader_to_writer) < 0) {
        HDprintf("HDclose fd_reader_to_writer failed\n");
        TEST_ERROR;
    }

    /* Reader finishes last and deletes the named pipes */
    if (!writer) {
        if (HDremove(np->fifo_writer_to_reader) != 0) {
            HDprintf("HDremove fifo_writer_to_reader failed\n");
            TEST_ERROR;
        }

        if (HDremove(np->fifo_reader_to_writer) != 0) {
            HDprintf("HDremove fifo_reader_to_writer failed\n");
            TEST_ERROR;
        }
    }
    return true;

error:
    return false;
} /* np_close() */

/*
 *  Writer synchronization depending on the result from the attribute action performed.
 */
static bool
np_writer(bool result, unsigned step, const state_t *s, np_state_t *np, H5F_vfd_swmr_config_t *config)
{
    unsigned int i;

    /* The action fails */
    if (!result) {
        HDprintf("attribute action failed\n");
        H5_FAILED();
        AT();

        /* At communication interval, notify the reader about the failure and quit */
        if (step % s->csteps == 0) {
            np->notify = -1;
            HDwrite(np->fd_writer_to_reader, &np->notify, sizeof(int));
            goto error;
        }
    }
    else { /* The action succeeds */
        /* At communication interval, notify the reader and wait for its response */
        if (step % s->csteps == 0) {
            /* Bump up the value of notify to tell the reader to start reading */
            np->notify++;
            if (HDwrite(np->fd_writer_to_reader, &np->notify, sizeof(int)) < 0) {
                HDprintf("HDwrite failed\n");
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
                HDprintf("np_confirm_verify_notify() verify/notify not in sync failed\n");
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
        HDprintf("verify action failed\n");
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
                HDprintf("HDwrite failed\n");
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
            HDprintf("HDread failed\n");
            TEST_ERROR;
        }

        if (np->notify == -1) {
            HDprintf("reader/writer failed to verify\n");
            TEST_ERROR;
        }

        if (np->notify != np->verify) {
            HDprintf("received message %d, expecting %d\n", np->notify, np->verify);
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

    if (writer) {
        if (!close_dsets(ds)) {
            HDprintf("close_dsets() failed\n");
            TEST_ERROR;
        }

        dbgf(2, "Writer closes the file (flush of raw data is disabled)\n");
        if (H5Fclose(s->file) < 0) {
            HDprintf("H5Fclose failed\n");
            TEST_ERROR;
        }

        /* Bump up the value of notify to tell the reader the file is closed */
        dbgf(2, "Writer notifies reader that the file is closed (flush of raw data is disabled)\n");
        np->notify++;
        if (HDwrite(np->fd_writer_to_reader, &np->notify, sizeof(int)) < 0) {
            HDprintf("HDwrite failed\n");
            TEST_ERROR;
        }

        if (!np_close(np, writer)) {
            HDprintf("np_close() failed\n");
            TEST_ERROR;
        }
    }
    else {
        /* Wait for a few ticks for the file to close in writer ?? need to this or not? */
        decisleep(config->tick_len * s->update_interval);

        dbgf(2, "Reader checks notify value from writer (flush of raw data is disabled)\n");
        if (!np_confirm_verify_notify(np->fd_writer_to_reader, 0, s, np)) {
            HDprintf("np_confirm_verify_notify() verify/notify not in sync failed\n");
            TEST_ERROR;
        }

        /* Close the named pipes */
        if (!np_close(np, writer)) {
            HDprintf("np_close() failed\n");
            TEST_ERROR;
        }

        /* Turn off named pipes */
        s->use_np = false;

        /* Verify the dataset again without named pipes */
        dbgf(2, "Reader verifies data after writer closes the file (flush of raw data is disabled)\n");
        if (!verify_dsets_operations(s, ds, config, np, true)) {
            HDprintf("verify_dsets_operations() failed\n");
            TEST_ERROR
        }

        if (!close_dsets(ds)) {
            HDprintf("close_dsets() failed\n");
            TEST_ERROR;
        }

        dbgf(2, "Reader closes the file (flush of raw data is disabled)\n");
        if (H5Fclose(s->file) < 0) {
            HDprintf("H5Fclose failed\n");
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
    hid_t                 fapl   = H5I_INVALID_HID;
    hid_t                 fcpl   = H5I_INVALID_HID;
    bool                  writer = FALSE;
    state_t               s;
    const char *          personality;
    H5F_vfd_swmr_config_t config;
    np_state_t            np;
    dsets_state_t         ds;

    if (!state_init(&s, argc, argv)) {
        HDprintf("state_init() failed\n");
        TEST_ERROR;
    }

    personality = HDstrstr(s.progname, "vfd_swmr_dsetops_");

    if (personality != NULL && HDstrcmp(personality, "vfd_swmr_dsetops_writer") == 0)
        writer = true;
    else if (personality != NULL && HDstrcmp(personality, "vfd_swmr_dsetops_reader") == 0)
        writer = false;
    else {
        HDprintf("unknown personality, expected vfd_swmr_dsetops_{reader,writer}\n");
        TEST_ERROR;
    }

    /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
     * flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
    init_vfd_swmr_config(&config, 4, 7, writer, TRUE, FALSE, s.flush_raw_data, 128, "./dsetops-shadow", NULL);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    if ((fapl = vfd_swmr_create_fapl(true, s.use_vfd_swmr, true, 4096, &config)) < 0) {
        HDprintf("vfd_swmr_create_fapl() failed\n");
        TEST_ERROR;
    }

    /* Set fs_strategy (file space strategy) and fs_page_size (file space page size) */
    if ((fcpl = vfd_swmr_create_fcpl(H5F_FSPACE_STRATEGY_PAGE, 4096)) < 0) {
        HDprintf("vfd_swmr_create_fcpl() failed");
        TEST_ERROR;
    }

    if (writer) {
        if ((s.file = H5Fcreate(s.filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0) {
            HDprintf("H5Fcreate failed\n");
            TEST_ERROR;
        }

        if (!create_dsets(&s, &ds)) {
            HDprintf("create_dsets() failed\n");
            TEST_ERROR;
        }
    }
    else {
        if ((s.file = H5Fopen(s.filename, H5F_ACC_RDONLY, fapl)) < 0) {
            HDprintf("H5Fopen failed\n");
            TEST_ERROR;
        }
        if (!open_dsets(&s, &ds)) {
            HDprintf("open_dsets() failed\n");
            TEST_ERROR;
        }
    }

    /* Initiailze named pipes */
    if (s.use_np && !np_init(&np, writer)) {
        HDprintf("np_init() failed\n");
        TEST_ERROR;
    }

    if (writer) {

        if (!perform_dsets_operations(&s, &ds, &config, &np)) {
            HDprintf("perform_dsets_operations() failed\n");
            TEST_ERROR;
        }
    }
    else {

        if (!verify_dsets_operations(&s, &ds, &config, &np, false)) {
            HDprintf("perform_dsets_operations() failed\n");
            TEST_ERROR;
        }
    }

    if (H5Pclose(fapl) < 0) {
        HDprintf("H5Pclose failed\n");
        TEST_ERROR;
    }

    if (H5Pclose(fcpl) < 0) {
        HDprintf("H5Pclose failed\n");
        TEST_ERROR;
    }

    if (!s.flush_raw_data && s.use_np) {

        if (!closing_on_noflush(writer, &s, &ds, &config, &np))
            TEST_ERROR
    }
    else {

        if (!close_dsets(&ds)) {
            HDprintf("close_dsets() failed\n");
            TEST_ERROR;
        }

        if (H5Fclose(s.file) < 0) {
            HDprintf("H5Fclose failed\n");
            TEST_ERROR;
        }

        if (s.use_np && !np_close(&np, writer)) {
            HDprintf("np_close() failed\n");
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
