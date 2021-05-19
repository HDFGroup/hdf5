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
    bool         compact;            /* -p option: create compact dataset */
    bool         compact_write;      /* -o option: write to the whole compact dataset */
    unsigned int compact_elmts;      /* -e <elmts> option: # of elments for the compact dataset */
    bool         contig;             /* -g option: create contiguous dataset */
    bool         chunked;            /* -k option: create chunked datasets with 5 indexing types */
    unsigned int rows;               /* -m <rows> option for contiguous and/or chunked datasets */
    unsigned int cols;               /* -n <cols option for contiguous and/or chunked datasets */
    unsigned int swrites; /* -s <swrites> option: sequential writes to contiguous and/or chunked datasets */
    unsigned int rwrites; /* -r <rwrites> option: random writes to contiguous and/or chunked datasets */
    unsigned int lwrites; /* -l <lwrites> option: hyperslab writes to contiguous and/or chunked datasets */
    unsigned int wwrites; /* -w <wwrites> option: modify raw data to contiguous and/or chunked datasets */
} state_t;

/* Initializations for state_t */
#define ALL_HID_INITIALIZER                                                                                  \
    (state_t)                                                                                                \
    {                                                                                                        \
        .filename = "", .file = H5I_INVALID_HID, .filetype = H5T_NATIVE_UINT32,                              \
        .update_interval = READER_WAIT_TICKS, .csteps = 1, .use_np = true, .use_vfd_swmr = true,             \
        .compact = false, .compact_write = false, .compact_elmts = MAX_COMPACT_ELMS, .contig = false,        \
        .rows = 256, .cols = 512, .swrites = 0, .rwrites = 0, .lwrites = 0, .wwrites = 0                     \
    }

/* Structure to hold info for different dataset types */
typedef struct {
    hid_t compact_did;  /* ID for compact dataset */
    hid_t contig_did;   /* ID for contiguous dataset */
    hid_t single_did;   /* ID for chunked dataset: single index */
    hid_t implicit_did; /* ID for chunked dataset: implicit index */
    hid_t fa_did;       /* ID for chunked dataset: fixed array index  */
    hid_t ea_did;       /* ID for chunked dataset: extensible array index */
    hid_t bt2_did;      /* ID for chunked dataset: version 2 btree index */
    hid_t compact_sid;  /* Dataspace ID for compact dataset */
    hid_t contig_sid;   /* Dataspace ID for contiguous dataset */
    hid_t single_sid;   /* Dataspace ID for chunked dataset */
    hid_t implicit_sid; /* Dataspace ID for chunked dataset */
    hid_t fa_sid;       /* Dataspace ID for chunked dataset */
    hid_t ea_sid;       /* Dataspace ID for chunked dataset */
    hid_t bt2_sid;      /* Dataspace ID for chunked dataset */
} dsets_state_t;

/* Initializations for dsets_state_t */
#define DSETS_INITIALIZER                                                                                    \
    (dsets_state_t)                                                                                          \
    {                                                                                                        \
        .compact_did = H5I_INVALID_HID, .compact_sid = H5I_INVALID_HID, .contig_did = H5I_INVALID_HID,       \
        .contig_sid = H5I_INVALID_HID, .single_did = H5I_INVALID_HID, .single_sid = H5I_INVALID_HID,         \
        .implicit_did = H5I_INVALID_HID, .implicit_sid = H5I_INVALID_HID, .fa_did = H5I_INVALID_HID,         \
        .fa_sid = H5I_INVALID_HID, .ea_did = H5I_INVALID_HID, .ea_sid = H5I_INVALID_HID,                     \
        .bt2_did = H5I_INVALID_HID, .bt2_sid = H5I_INVALID_HID                                               \
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
static bool open_dset_real(const state_t *s, hid_t *did, hid_t *sid, const char *name);
static bool close_dsets(const dsets_state_t *ds);
static bool close_dset_real(hid_t did, hid_t sid);

static bool write_dset_contig_chunked(state_t *s, dsets_state_t *ds, 
    H5F_vfd_swmr_config_t *config, np_state_t *np);
static bool dsets_action(unsigned action, const state_t *s, const dsets_state_t *ds, unsigned step);
static bool dset_setup(unsigned action, unsigned which, const state_t *s, hsize_t *start, hsize_t *stride,
                       hsize_t *count, hsize_t *block, hid_t *mem_sid, unsigned int **buf);
static bool write_dset(hid_t did, hid_t tid, hid_t mem_sid, hid_t file_sid, hsize_t *start, hsize_t *stride,
                       hsize_t *count, hsize_t *block, unsigned int *buf);
static bool write_dset_compact(const state_t *s, const dsets_state_t *ds);

static bool verify_write_dset_contig_chunked(state_t *s, dsets_state_t *ds, 
    H5F_vfd_swmr_config_t *config, np_state_t *np);
static bool verify_dsets_action(unsigned action, const state_t *s, const dsets_state_t *ds, unsigned which);
static bool verify_read_dset(hid_t did, hid_t tid, hid_t mem_sid, hid_t file_sid, hsize_t *start,
                             hsize_t *stride, hsize_t *count, hsize_t *block, unsigned int *vbuf);
static bool verify_read_dset_compact(const state_t *s, const dsets_state_t *ds);

static const hid_t badhid = H5I_INVALID_HID;

/* Names for datasets */
#define DSET_COMPACT_NAME  "compact_dset"
#define DSET_CONTIG_NAME   "contig_dset"
#define DSET_SINGLE_NAME   "chunked_single"
#define DSET_IMPLICIT_NAME "chunked_implicit"
#define DSET_FA_NAME       "chunked_fa"
#define DSET_EA_NAME       "chunked_ea"
#define DSET_BT2_NAME      "chunked_bt2"

/* Action for writes */
#define SEQ_WRITE    1 /* Sequential write */
#define RANDOM_WRITE 2 /* Random write */
#define HYPER_WRITE  3 /* Hyperslab write */
#define MODIFY_DATA  4 /* Modify raw data */

/* Test program usage info */
static void
usage(const char *progname)
{
    fprintf(stderr,
            "usage: %s \n"
            "           [-p] [-e elmts] [-o]\n"
            "           [-g] [-k] [-m rows] [-n cols]\n"
            "           [-s swrites] [-r rwrites] [-l lwrites] [-w writes]\n"
            "           [-u nticks] [-c csteps] [-S] [-N] [-q] [-b]\n"
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
    char *        tfile;
    char *        end;

    *s = ALL_HID_INITIALIZER;

    if (H5_basename(argv[0], &tfile) < 0) {
        printf("H5_basename failed\n");
        TEST_ERROR;
    }

    esnprintf(s->progname, sizeof(s->progname), "%s", tfile);

    while ((ch = getopt(argc, argv, "pte:gkm:n:s:r:l:w:bqSNu:c:")) != -1) {
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
        printf("Require to specify at least -p or -g or -k option\n");
        usage(s->progname);
        goto error;
    }

    /* -e <elmts> */
    if (s->compact_elmts > MAX_COMPACT_ELMS) {
        printf("size of compact dataset cannot exceed 16380 elements\n");
        TEST_ERROR;
    }

    /* -c <csteps> cannot be zero */
    if (!s->csteps) {
        printf("communication interval cannot be zero\n");
        TEST_ERROR;
    }

    /* -c <csteps> and -s <swrites> options */
    if (s->swrites && s->csteps > s->swrites) {
        printf("communication interval with sequential writes is out of bounds\n");
        TEST_ERROR;
    }

    /* -c <csteps> and -r <rwrites> options */
    if (s->rwrites && s->csteps > s->rwrites) {
        printf("communication interval with random writes is out of bounds\n");
        TEST_ERROR;
    }

    /* -c <csteps> and -l <lwrites> options */
    if (s->lwrites && s->csteps > s->lwrites) {
        printf("communication interval with hyperslab writes is out of bounds\n");
        TEST_ERROR;
    }

    /* -c <csteps> and -w <wwrites> options */
    if (s->wwrites && s->csteps > s->wwrites) {
        printf("communication interval with raw data modification is out of bounds\n");
        TEST_ERROR;
    }

    /* The test file name */
    esnprintf(s->filename, sizeof(s->filename), "vfd_swmr_dsetops.h5");

    return true;

error:
    return false;

} /* state_init() */

/*
 *  Create the datasets as specified on the command line.
 */
static bool
create_dsets(const state_t *s, dsets_state_t *ds)
{
    hid_t dcpl = badhid;
    hid_t dtid = badhid;

    *ds = DSETS_INITIALIZER;

    /* Create the named datatype that will be used by compact and contiguous datasets */
    if ((dtid = H5Tcopy(s->filetype)) < 0) {
        printf("H5Tcopy failed\n");
        TEST_ERROR;
    }

    if (H5Tcommit2(s->file, "named_dtype", dtid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) {
        printf("H5Tcommit2 failed\n");
        TEST_ERROR;
    }

    /* Dataset with compact layout, 1d, named datatype */
    if (s->compact) {
        hsize_t dims[1];

        if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
            printf("H5Pcreate failed\n");
            TEST_ERROR;
        }
        if (H5Pset_layout(dcpl, H5D_COMPACT) < 0) {
            printf("H5Pset_layout failed\n");
            TEST_ERROR;
        }

        dims[0] = s->compact_elmts;

        /* Dataspace for compact dataset */
        if ((ds->compact_sid = H5Screate_simple(1, dims, dims)) < 0) {
            printf("H5Screate_simple failed\n");
            TEST_ERROR;
        }

        /* Create the compact dataset with named datatype */
        if ((ds->compact_did = H5Dcreate2(s->file, DSET_COMPACT_NAME, dtid, ds->compact_sid, H5P_DEFAULT,
                                          dcpl, H5P_DEFAULT)) < 0) {
            printf("H5Dcreate2 compact dataset failed\n");
            TEST_ERROR;
        }

        if (H5Pclose(dcpl) < 0) {
            printf("H5Pclose failed\n");
            TEST_ERROR;
        }
    }

    /* Dataset with contiguous layout, 2d, named datatype */
    if (s->contig) {
        hsize_t dims[2];

        dims[0] = s->rows;
        dims[1] = s->cols;

        /* Dataspace for contiguous dataset */
        if ((ds->contig_sid = H5Screate_simple(2, dims, dims)) < 0) {
            printf("H5Screate_simple failed\n");
            TEST_ERROR;
        }

        if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
            printf("H5Pcreate failed\n");
            TEST_ERROR;
        }

        if (H5Pset_layout(dcpl, H5D_CONTIGUOUS) < 0) {
            printf("H5Pset_layout failed\n");
            TEST_ERROR;
        }

        /* Create the contiguous dataset with the named datatype */
        if ((ds->contig_did = H5Dcreate2(s->file, DSET_CONTIG_NAME, dtid, ds->contig_sid, H5P_DEFAULT, dcpl,
                                         H5P_DEFAULT)) < 0) {
            printf("H5Dcreate2 contiguous dataset failed\n");
            TEST_ERROR;
        }

        if (H5Pclose(dcpl) < 0) {
            printf("H5Pclose failed\n");
            TEST_ERROR;
        }
    }

    /* Dataset with chunked layout, 2d, named datatype */
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
            printf("H5Pcreate failed\n");
            TEST_ERROR;
        }

        if (H5Pset_layout(dcpl, H5D_CHUNKED) < 0) {
            printf("H5Pset_layout failed\n");
            TEST_ERROR;
        }

        /* Create 2-D chunked dataset with single index */
        /* Chunked, dims=max_dims=chunk_dims */

        if (H5Pset_chunk(dcpl, 2, dims) < 0) {
            printf("H5Pset_chunk failed\n");
            TEST_ERROR;
        }

        if ((ds->single_sid = H5Screate_simple(2, dims, dims)) < 0) {
            printf("H5Screate_simple failed\n");
            TEST_ERROR;
        }

        /* Create the chunked dataset (single index) with the named datatype */
        if ((ds->single_did = H5Dcreate2(s->file, DSET_SINGLE_NAME, dtid, ds->single_sid, H5P_DEFAULT, dcpl,
                                         H5P_DEFAULT)) < 0) {
            printf("H5Dcreate2 chunked dataset:single index failed\n");
            TEST_ERROR;
        }

        /* Create 2-D chunked dataset with implicit index */
        /* Chunked, dims=max_dims, early allocation */

        if (H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0) {
            printf("H5Pset_alloc_time\n");
            TEST_ERROR;
        }

        if (H5Pset_chunk(dcpl, 2, chunk_dims) < 0) {
            printf("H5Pset_chunk failed\n");
            TEST_ERROR;
        }

        if ((ds->implicit_sid = H5Screate_simple(2, dims, dims)) < 0) {
            printf("H5Screate_simple failed\n");
            TEST_ERROR;
        }

        /* Create the chunked dataset (implicit index) with the named datatype */
        if ((ds->implicit_did = H5Dcreate2(s->file, DSET_IMPLICIT_NAME, dtid, ds->implicit_sid, H5P_DEFAULT,
                                           dcpl, H5P_DEFAULT)) < 0) {
            printf("H5Dcreate2 chunked dataset:implicit index failed\n");
            TEST_ERROR;
        }

        if (H5Pclose(dcpl) < 0) {
            printf("H5Pclose failed\n");
            TEST_ERROR;
        }

        /* Create 2-D chunked dataset with fixed array index */
        /* Chunked, fixed max_dims */

        if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
            printf("H5Pcreate failed\n");
            TEST_ERROR;
        }

        if (H5Pset_chunk(dcpl, 2, chunk_dims) < 0) {
            printf("H5Pset_chunk failed\n");
            TEST_ERROR;
        }

        max_dims[0] = dims[0] + 10;
        max_dims[1] = dims[1] + 10;

        if ((ds->fa_sid = H5Screate_simple(2, dims, max_dims)) < 0) {
            printf("H5Screate_simple failed\n");
            TEST_ERROR;
        }

        /* Create the chunked dataset (fixed array index) with the named datatype */
        if ((ds->fa_did =
                 H5Dcreate2(s->file, DSET_FA_NAME, dtid, ds->fa_sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) {
            printf("H5Dcreaet2 chunked dataset: fa index failed\n");
            TEST_ERROR;
        }

        /* Create 2-D chunked dataset with extensible array index */
        /* Chunked, 1 unlimited max_dims */

        max_dims[1] = H5S_UNLIMITED;
        if ((ds->ea_sid = H5Screate_simple(2, dims, max_dims)) < 0) {
            printf("H5Screate_simple failed\n");
            TEST_ERROR;
        }

        /* Create the chunked dataset (extensible array index) with the named datatype */
        if ((ds->ea_did =
                 H5Dcreate2(s->file, DSET_EA_NAME, dtid, ds->ea_sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) {
            printf("H5Dcreate2 chunked dataset: ea index failed\n");
            TEST_ERROR;
        }

        /* Create 2-D chunked dataset with bt2 index */
        /* Chunked, 2 unlimited max_dims */
        max_dims[0] = H5S_UNLIMITED;

        if ((ds->bt2_sid = H5Screate_simple(2, dims, max_dims)) < 0) {
            printf("H5Screate_simple failed\n");
            TEST_ERROR;
        }

        /* Create the chunked dataset (btree2 index) with the named datatype */
        if ((ds->bt2_did =
                 H5Dcreate2(s->file, DSET_BT2_NAME, dtid, ds->bt2_sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) {
            printf("H5Dcreate2 chunked dataset: bt2 index failed\n");
            TEST_ERROR;
        }

        if (H5Pclose(dcpl) < 0) {
            printf("H5Pclose failed\n");
            TEST_ERROR;
        }
    }

    if (H5Tclose(dtid) < 0) {
        printf("H5Tclose failed\n");
        TEST_ERROR;
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
        H5Dclose(ds->compact_did);
        H5Dclose(ds->contig_did);
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
 * Open the datasets as specified.
 */
static bool
open_dsets(const state_t *s, dsets_state_t *ds)
{
    *ds = DSETS_INITIALIZER;

    if (s->compact) {
        if (!open_dset_real(s, &ds->compact_did, &ds->compact_sid, DSET_COMPACT_NAME)) {
            printf("open_dset_real() for compact dataset failed\n");
            TEST_ERROR;
        }
    }

    if (s->contig) {

        if (!open_dset_real(s, &ds->contig_did, &ds->contig_sid, DSET_CONTIG_NAME)) {
            printf("open_dset_real() for contiguous dataset failed\n");
            TEST_ERROR;
        }
    }

    if (s->chunked) {

        if (!open_dset_real(s, &ds->single_did, &ds->single_sid, DSET_SINGLE_NAME)) {
            printf("open_dset_real() for chunked dataset: single index failed\n");
            TEST_ERROR;
        }

        if (!open_dset_real(s, &ds->implicit_did, &ds->implicit_sid, DSET_IMPLICIT_NAME)) {
            printf("open_dset_real() for chunked dataset: implicit index failed\n");
            TEST_ERROR;
        }

        if (!open_dset_real(s, &ds->fa_did, &ds->fa_sid, DSET_FA_NAME)) {
            printf("open_dset_real() for chunked dataset: fa index failed\n");
            TEST_ERROR;
        }

        if (!open_dset_real(s, &ds->ea_did, &ds->ea_sid, DSET_EA_NAME)) {
            printf("open_dset_real() for chunked dataset: ea index failed\n");
            TEST_ERROR;
        }

        if (!open_dset_real(s, &ds->bt2_did, &ds->bt2_sid, DSET_BT2_NAME)) {
            printf("open_dset_real() for chunked dataset: bt2 index failed\n");
            TEST_ERROR;
        }
    }

    return true;

error:
    return false;

} /* open_dsets() */

/*
 * Do the real work of opening the dataset.
 * Verify the dimension sizes are as expected.
 */
static bool
open_dset_real(const state_t *s, hid_t *did, hid_t *sid, const char *name)
{
    hsize_t dims[2];

    if ((*did = H5Dopen2(s->file, name, H5P_DEFAULT)) < 0) {
        printf("H5Dopen dataset failed\n");
        TEST_ERROR;
    }

    if ((*sid = H5Dget_space(*did)) < 0) {
        printf("H5Dget_space failed\n");
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
close_dsets(const dsets_state_t *ds)
{
    if (!close_dset_real(ds->compact_did, ds->compact_sid)) {
        printf("H5Dclose compact dataset failed\n");
        TEST_ERROR;
    }

    if (!close_dset_real(ds->contig_did, ds->contig_sid)) {
        printf("H5Dclose contiguous dataset failed\n");
        TEST_ERROR;
    }

    if (!close_dset_real(ds->single_did, ds->single_sid)) {
        printf("H5Dclose chunked dataset: single index failed\n");
        TEST_ERROR;
    }

    if (!close_dset_real(ds->implicit_did, ds->implicit_sid)) {
        printf("H5Dclose chunked dataset: implicit index failed\n");
        TEST_ERROR;
    }

    if (!close_dset_real(ds->fa_did, ds->fa_sid)) {
        printf("H5Dclose chunked dataset: fa index failed\n");
        TEST_ERROR;
    }

    if (!close_dset_real(ds->ea_did, ds->ea_sid)) {
        printf("H5Dclose chunked dataset: ea index failed\n");
        TEST_ERROR;
    }

    if (!close_dset_real(ds->bt2_did, ds->bt2_sid)) {
        printf("H5Dclose chunked dataset: bt2 index failed\n");
        TEST_ERROR;
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
        printf("H5Dclose dataset failed\n");
        TEST_ERROR;
    }

    if (sid != badhid && H5Sclose(sid) < 0) {
        printf("H5Sclose dataspace for dataset failed\n");
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
 * Perform writes for contiguous and chunked datasets:
 *  --SEQ_WRITE: sequential writes
 *  --RANDOM_WRITE: random writes
 *  --HYPER_WRITE: hyperslab writes
 *  --MODIFY_DATA: raw data modifications
 */
static bool
write_dset_contig_chunked(state_t *s, dsets_state_t *ds, H5F_vfd_swmr_config_t *config, np_state_t *np)
{
    unsigned              step;
    bool                  result;
    
    HDassert(s->contig || s->chunked);

    /* Perform sequential writes for contiguous and/or chunked datasets */
    if (s->swrites) {

        for (step = 0; (step < s->swrites && step < (s->rows * s->cols)); step++) {
            dbgf(2, "Sequential writes %u to dataset\n", step);

            result = dsets_action(SEQ_WRITE, s, ds, step);

            if (s->use_np && !np_writer(result, step, s, np, config)) {
                printf("np_writer() for sequential writes failed\n");
                TEST_ERROR;
            }
        }
    }

    /* Perform random writes for contiguous and/or chunked datasets */
    if (s->rwrites) {
        unsigned newstep;

        /* Set up random seed which will be the same for both writer and reader */
        HDsrandom(RANDOM_SEED);

        for (step = 0; (step < s->rwrites && step < (s->rows * s->cols)); step++) {
            dbgf(2, "Random writes %u to dataset\n", step);

            newstep = (unsigned int)HDrandom() % (s->rows * s->cols);
            printf("Random step is %u\n", newstep);
            result = dsets_action(RANDOM_WRITE, s, ds, newstep);

            if (s->use_np && !np_writer(result, step, s, np, config)) {
                printf("np_writer() for random writes failed\n");
                TEST_ERROR;
            }
        }
    }

    /* Perform hyperslab writes for contiguous and/or chunked datasets */
    if (s->lwrites) {
        unsigned k;

        for (step = 0, k = 0; (step < s->lwrites && k < (s->rows * s->cols)); step++, k += s->cols) {
            dbgf(2, "Hyperslab writes %u to dataset\n", step);

            result = dsets_action(HYPER_WRITE, s, ds, k);

            if (s->use_np && !np_writer(result, step, s, np, config)) {
                printf("np_writer() for hyperslab writes failed\n");
                TEST_ERROR;
            }
        }
   }

   /* Perform raw data modifications for contiguous and/or chunked datasets */
    if (s->wwrites) {

        for (step = 0; (step < s->wwrites && step < (s->rows * s->cols)); step++) {
            dbgf(2, "Modify raw data %u to dataset\n", step);

            result = dsets_action(MODIFY_DATA, s, ds, step);

            if (s->use_np && !np_writer(result, step, s, np, config)) {
                printf("np_writer() for modify raw data failed\n");
                TEST_ERROR;
            }
        }
    }

    return true;

error:
    return false;

} /* write_dset_contig_chunked() */

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

    /* Set up selection, dataspace and data buffer according to the specified action */
    if (!dset_setup(action, which, s, start, stride, count, block, &mem_sid, &wbuf)) {
        printf("dset_setup() failed\n");
        TEST_ERROR;
    }

    /* Write to the contiguous dataset */
    if (s->contig) {

        if (!write_dset(ds->contig_did, s->filetype, mem_sid, ds->contig_sid, start, stride, count, block,
                        wbuf)) {
            printf("H5Dwrite to contiguous dataset failed\n");
            TEST_ERROR;
        }
    }

    /* Write to the 5 chunked datasets */
    if (s->chunked) {

        if (!write_dset(ds->single_did, s->filetype, mem_sid, ds->single_sid, start, stride, count, block,
                        wbuf)) {
            printf("H5Dwrite to chunked dataset: single index dataset failed\n");
            TEST_ERROR;
        }

        if (!write_dset(ds->implicit_did, s->filetype, mem_sid, ds->implicit_sid, start, stride, count, block,
                        wbuf)) {
            printf("H5Dwrite to chunked dataset: implicit index dataset failed\n");
            TEST_ERROR;
        }

        if (!write_dset(ds->fa_did, s->filetype, mem_sid, ds->fa_sid, start, stride, count, block, wbuf)) {
            printf("H5Dwrite to chunked dataset: fa index dataset failed\n");
            TEST_ERROR;
        }

        if (!write_dset(ds->ea_did, s->filetype, mem_sid, ds->ea_sid, start, stride, count, block, wbuf)) {
            printf("H5Dwrite to chunked dataset: ea index dataset failed\n");
            TEST_ERROR;
        }

        if (!write_dset(ds->bt2_did, s->filetype, mem_sid, ds->bt2_sid, start, stride, count, block, wbuf)) {
            printf("H5Dwrite to chunked dataset: bt2 index dataset failed\n");
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
        printf("HDmalloc failed\n");
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
write_dset(hid_t did, hid_t tid, hid_t mem_sid, hid_t file_sid, hsize_t *start, hsize_t *stride,
           hsize_t *count, hsize_t *block, unsigned int *buf)
{

    if (H5Sselect_hyperslab(file_sid, H5S_SELECT_SET, start, stride, count, block) < 0) {
        printf("H5Sselect to dataset failed\n");
        TEST_ERROR;
    }

    if (H5Dwrite(did, tid, mem_sid, file_sid, H5P_DEFAULT, buf) < 0) {
        printf("H5Dwrite to dataset failed\n");
        TEST_ERROR;
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
        printf("HDmalloc buffer for compact dataset failed\n");
        goto error;
    }

    for (i = 0; i < s->compact_elmts; i++)
        buf[i] = i + 1;

    if (H5Dwrite(ds->compact_did, s->filetype, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) {
        printf("H5Dwrite to compact dataset failed\n");
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
 * Verify writes for contiguous and chunked datasets:
 *  --SEQ_WRITE: sequential writes
 *  --RANDOM_WRITE: random writes
 *  --HYPER_WRITE: hyperslab writes
 *  --MODIFY_DATA: raw data modifications
 */
static bool
verify_write_dset_contig_chunked(state_t *s, dsets_state_t *ds, H5F_vfd_swmr_config_t *config, np_state_t *np)
{
    unsigned              step;
    bool                  result;
    
    HDassert(s->contig || s->chunked);

    /* Start verifying sequential writes for contiguous and/or chunked datasets */
    if (s->swrites) {

        for (step = 0; (step < s->swrites && step < (s->rows * s->cols)); step++) {
            dbgf(2, "Verify sequential writes %u to dataset\n", step);

            if (s->use_np && !np_confirm_verify_notify(np->fd_writer_to_reader, step, s, np)) {
                printf("np_confirm_verify_notify() verify/notify not in sync failed\n");
                TEST_ERROR;
            }

            /* Wait for a few ticks for the update to happen */
            decisleep(config->tick_len * s->update_interval);

            result = verify_dsets_action(SEQ_WRITE, s, ds, step);

            if (s->use_np && !np_reader(result, step, s, np)) {
                printf("np_reader() for verifying addition failed\n");
                TEST_ERROR;
            }
        }
    }

    /* Start verifying random writes for contiguous and/or chunked datasets */
    if (s->rwrites) {
        unsigned newstep;

        /* Set up random seed which will be the same for both writer and reader */
        HDsrandom(RANDOM_SEED);

        for (step = 0; (step < s->rwrites && step < (s->rows * s->cols)); step++) {
            dbgf(2, "Verify random writes %u to dataset\n", step);

            newstep = (unsigned int)HDrandom() % (s->rows * s->cols);
            printf("Random step is %u\n", newstep);

            if (s->use_np && !np_confirm_verify_notify(np->fd_writer_to_reader, step, s, np)) {
                printf("np_confirm_verify_notify() verify/notify not in sync failed\n");
                TEST_ERROR;
            }

            /* Wait for a few ticks for the update to happen */
            decisleep(config->tick_len * s->update_interval);

            result = verify_dsets_action(RANDOM_WRITE, s, ds, newstep);

            if (s->use_np && !np_reader(result, step, s, np)) {
                printf("np_reader() for verifying addition failed\n");
                TEST_ERROR;
            }
        }
    }

    /* Start verifying hyperslab writes for contiguous and/or chunked datasets */
    if (s->lwrites) {
        unsigned k;

        for (step = 0, k = 0; (step < s->lwrites && k < (s->rows * s->cols)); step++, k += s->cols) {
            dbgf(2, "Verify hyperslab writes %u to dataset\n", step);

            if (s->use_np && !np_confirm_verify_notify(np->fd_writer_to_reader, step, s, np)) {
                printf("np_confirm_verify_notify() verify/notify not in sync failed\n");
                TEST_ERROR;
            }

            /* Wait for a few ticks for the update to happen */
            decisleep(config->tick_len * s->update_interval);

            result = verify_dsets_action(HYPER_WRITE, s, ds, k);

            if (s->use_np && !np_reader(result, step, s, np)) {
                printf("np_reader() for verifying addition failed\n");
                TEST_ERROR;
            }
        }
    }

    /* Start verifying raw data modifications for contiguous and/or chunked datasets */
    if (s->wwrites) {

        for (step = 0; (step < s->wwrites && step < (s->rows * s->cols)); step++) {
            dbgf(2, "Verify raw data modification %u to dataset\n", step);

            if (s->use_np && !np_confirm_verify_notify(np->fd_writer_to_reader, step, s, np)) {
                printf("np_confirm_verify_notify() verify/notify not in sync failed\n");
                TEST_ERROR;
            }

            /* Wait for a few ticks for the update to happen */
            decisleep(config->tick_len * s->update_interval);

            result = verify_dsets_action(MODIFY_DATA, s, ds, step);

            if (s->use_np && !np_reader(result, step, s, np)) {
                printf("np_reader() for verifying addition failed\n");
                TEST_ERROR;
            }
        }
    }

    return true;

error:
    return false;

} /* verify_write_dset_contig_chunked() */

/*
 * Verify the data read from each of the datasets specified on the command line
 * according to "action":
 *      SEQ_WRITE: `which` sequential write
 *      RANDOM_WRITE: `which` random write
 *      HYPER_WRITE: `which` hyperslab write
 *      MODIFY_DATA: `which` raw data modification
 */
static bool
verify_dsets_action(unsigned action, const state_t *s, const dsets_state_t *ds, unsigned which)
{
    hsize_t       start[2];
    hsize_t       stride[2];
    hsize_t       count[2];
    hsize_t       block[2];
    hid_t         mem_sid;
    unsigned int *vbuf = NULL;

    /* Set up selection, dataspace and data buffer according to the specified action */
    if (!dset_setup(action, which, s, start, stride, count, block, &mem_sid, &vbuf)) {
        printf("dset_setup() failed\n");
        TEST_ERROR;
    }

    /* Verify the data read for the contiguous dataset */
    if (s->contig) {
        if (!verify_read_dset(ds->contig_did, s->filetype, mem_sid, ds->contig_sid, start, stride, count,
                              block, vbuf)) {
            printf("H5Dwrite to contiguous dataset failed\n");
            TEST_ERROR;
        }
    }

    /* Verify the data read for the chunked datasets */
    if (s->chunked) {

        if (!verify_read_dset(ds->single_did, s->filetype, mem_sid, ds->single_sid, start, stride, count,
                              block, vbuf)) {
            printf("H5Dwrite to chunked dataset: single index dataset failed\n");
            TEST_ERROR;
        }

        if (!verify_read_dset(ds->implicit_did, s->filetype, mem_sid, ds->implicit_sid, start, stride, count,
                              block, vbuf)) {
            printf("H5Dwrite to chunked dataset: implicit index dataset failed\n");
            TEST_ERROR;
        }

        if (!verify_read_dset(ds->fa_did, s->filetype, mem_sid, ds->fa_sid, start, stride, count, block,
                              vbuf)) {
            printf("H5Dwrite to chunked dataset: fa index dataset failed\n");
            TEST_ERROR;
        }

        if (!verify_read_dset(ds->ea_did, s->filetype, mem_sid, ds->ea_sid, start, stride, count, block,
                              vbuf)) {
            printf("H5Dwrite to chunked dataset: ea index dataset failed\n");
            TEST_ERROR;
        }

        if (!verify_read_dset(ds->bt2_did, s->filetype, mem_sid, ds->bt2_sid, start, stride, count, block,
                              vbuf)) {
            printf("H5Dwrite to chunked dataset: bt2 index dataset failed\n");
            TEST_ERROR;
        }
    }

    if (vbuf)
        HDfree(vbuf);

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
verify_read_dset(hid_t did, hid_t tid, hid_t mem_sid, hid_t file_sid, hsize_t *start, hsize_t *stride,
                 hsize_t *count, hsize_t *block, unsigned int *vbuf)
{
    unsigned int *rbuf = NULL;
    unsigned      i;

    /* Refresh the dataset */
    if (H5Drefresh(did) < 0) {
        printf("H5Drefresh dataset failed\n");
        TEST_ERROR;
    }

    /* Allocate the buffer for reading */
    if ((rbuf = HDmalloc(count[1] * sizeof(unsigned int))) == NULL) {
        printf("HDmalloc failed\n");
        TEST_ERROR;
    }

    /* Make the selection the file dataspace */
    if (H5Sselect_hyperslab(file_sid, H5S_SELECT_SET, start, stride, count, block) < 0) {
        printf("H5Sselect to dataset failed\n");
        TEST_ERROR;
    }

    /* Read the data from the dataset into `rbuf` */
    if (H5Dread(did, tid, mem_sid, file_sid, H5P_DEFAULT, rbuf) < 0) {
        printf("H5Dread from dataset failed\n");
        TEST_ERROR;
    }

    /* Verify the data read in `rbuf` is as `vbuf` */
    for (i = 0; i < count[1]; i++)
        if (rbuf[i] != vbuf[i])
            TEST_ERROR;

    if (rbuf)
        HDfree(rbuf);

    return true;

error:
    if (rbuf)
        HDfree(rbuf);
    return false;

} /* verify_read_dset() */

/*
 * Verify that the data read from the compact dataset is as unexpected.
 */
static bool
verify_read_dset_compact(const state_t *s, const dsets_state_t *ds)
{
    unsigned int *rbuf;
    unsigned      i;

    if ((rbuf = HDmalloc(s->compact_elmts * sizeof(unsigned int))) == NULL) {
        printf("HDmalloc buffer for compact dataset failed\n");
        goto error;
    }

    if (H5Dread(ds->compact_did, s->filetype, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0) {
        printf("H5Dwrite to compact dataset failed\n");
        TEST_ERROR;
    }

    for (i = 0; i < s->compact_elmts; i++)
        if (rbuf[i] != (i + 1)) {
            printf("Invalid value for compact dataset element\n");
            TEST_ERROR;
        }

    if (rbuf)
        HDfree(rbuf);

    return true;

error:
    if (rbuf)
        HDfree(rbuf);
    return false;

} /* verify_read_dset_compact() */

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
 *  Writer synchronization depending on the result from the attribute action performed.
 */
static bool
np_writer(bool result, unsigned step, const state_t *s, np_state_t *np, H5F_vfd_swmr_config_t *config)
{
    unsigned int i;

    /* The action fails */
    if (!result) {
        printf("attribute action failed\n");
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
 * Main
 */
int
main(int argc, char **argv)
{
    hid_t                 fapl, fcpl;
    unsigned              step;
    bool                  writer;
    state_t               s;
    const char *          personality;
    H5F_vfd_swmr_config_t config;
    np_state_t            np;
    dsets_state_t         ds;
    bool                  result;

    if (!state_init(&s, argc, argv)) {
        printf("state_init() failed\n");
        TEST_ERROR;
    }

    personality = HDstrstr(s.progname, "vfd_swmr_dsetops_");

    if (personality != NULL && strcmp(personality, "vfd_swmr_dsetops_writer") == 0)
        writer = true;
    else if (personality != NULL && strcmp(personality, "vfd_swmr_dsetops_reader") == 0)
        writer = false;
    else {
        printf("unknown personality, expected vfd_swmr_dsetops_{reader,writer}\n");
        TEST_ERROR;
    }

    /* config, tick_len, max_lag, writer, flush_raw_data, md_pages_reserved, md_file_path */
    init_vfd_swmr_config(&config, 4, 7, writer, FALSE, 128, "./dsetops-shadow");

    /* use_latest_format, use_vfd_swmr, only_meta_page, config */
    if ((fapl = vfd_swmr_create_fapl(true, s.use_vfd_swmr, true, &config)) < 0) {
        printf("vfd_swmr_create_fapl() failed\n");
        TEST_ERROR;
    }

    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0) {
        printf("H5Pcreate failed\n");
        TEST_ERROR;
    }

    if (H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, false, 1) < 0) {
        printf("H5Pset_file_space_strategy failed\n");
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

        /* Perform writes to the whole compact dataset */
        if (s.compact && s.compact_write) {
            dbgf(2, "Writes all to compact dataset\n");

            result = write_dset_compact(&s, &ds);

            if (s.use_np && !np_writer(result, 0, &s, &np, &config)) {
                printf("np_writer() for addition failed\n");
                TEST_ERROR;
            }
        }

        if (s.contig || s.chunked) {
            /* Perform writes for contiguous and/or chunked datasets */
            if(!write_dset_contig_chunked(&s, &ds, &config, &np)) {
                printf("write_dset_contig_chunked() failed\n");
                TEST_ERROR;
            }
        }

    }
    else {

        /* Start verifying data written to the compact dataset */
        if (s.compact && s.compact_write) {
            dbgf(2, "Verify writes to compact dataset\n");

            if (s.use_np && !np_confirm_verify_notify(np.fd_writer_to_reader, 0, &s, &np)) {
                printf("np_confirm_verify_notify() verify/notify not in sync failed\n");
                TEST_ERROR;
            }
            /* Wait for a few ticks for the update to happen */
            decisleep(config.tick_len * s.update_interval);

            result = verify_read_dset_compact(&s, &ds);

            if (s.use_np && !np_reader(result, 0, &s, &np)) {
                printf("np_reader() for verifying addition failed\n");
                TEST_ERROR;
            }
        }

        if (s.contig || s.chunked) {
    
            /* Verify writes for contiguous and/or chunked datasets */
            if(!verify_write_dset_contig_chunked(&s, &ds, &config, &np)) {
                printf("verify_write_dset_contig_chunked() failed\n");
                TEST_ERROR;
            }
        }
    }

    if (!close_dsets(&ds)) {
        printf("close_dsets() failed\n");
        TEST_ERROR;
    }

    if (H5Pclose(fapl) < 0) {
        printf("H5Pclose failed\n");
        TEST_ERROR;
    }

    if (H5Pclose(fcpl) < 0) {
        printf("H5Pclose failed\n");
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
