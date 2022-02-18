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

/* This program performs "bigset" tests for VFD SWMR.  In VFD SWMR mode,
 * the bigset tests exercise
 *
 * 1 the two major indices for extensible, chunked datasets: the
 *   extensible array and the version-2 B-tree, with VFD SWMR active.
 *   The maximal dimension can be either fixed or unlimited.
 *
 * 2 reading and writing virtual datasets with source datasets residing
 *   in the same HDF5 file
 *
 * 3 virtual datasets with source datasets spread over a small number of
 *   HDF5 files
 *
 * The program selects between two personalities, reader or writer,
 * using the name it is invoked with (the last component of argv[0]).
 * Reader and writer should run simultaneously.
 *
 * The bigset tests use datasets extensible in one dimension to exercise
 * the extensible array, and tests extensible in two dimensions to
 * exercise the v2 B-tree.
 *
 * The writer opens an HDF5 file and creates `n` chunked datasets
 * extensible in `1 <= d <= 2` dimensions and runs for `i` iterations.
 * The chunk size, `w` x `h`, is user-selectable, as are `d`, `i`, and
 * `n`.  In each iteration, the writer extends each dataset by the width
 * (or the width and height) of a chunk, and writes a test pattern to
 * the dataset on chunk boundaries.
 *
 * For 3D dataset, the extension is always along the first dimension.
 * e.g. the chunk size is `l` x `m` x `n`, after `i` iterations, the
 * dataset size becomes `i x l` x `m` x `n`.
 * It does not test VDS for 3D dataset.
 *
 * The reader should be started with the same user-selectable parameters
 * as the writer: iterations, number of datasets, chunk width and
 * height and depth, dimensions.
 *
 * The reader opens the same HDF5 file, reads and re-reads it until all
 * `n` datasets appear, and then reads and re-reads the datasets until
 * all iteration 0 data is available and contains the expected test
 * pattern.  The reader repeats for the iteration 1 data, iteration 2,
 * and so on, until `i` iterations are complete.
 *
 * The reader reads datasets in chunk-sized units.  To challenge the
 * chunk index a bit, the reader reads on a chunk boundary on even
 * iterations and reads with a small offset from a chunk boundary on odd
 * iterations.
 *
 * The writer adds an attribute to every `a`th dataset, where `a` is
 * a user-selectable parameter.  The reader reads and verifies an
 * attribute on every `a`th dataset.
 *
 * To help ensure that the reader and the writer are simultaneously
 * reading and writing the HDF5 file, both reader and writer pause
 * between each dataset written/verified (if there are at least as
 * many iterations as datasets) or between each iteration (if there
 * are fewer iterations than datasets).  The duration of the pause is
 * user-selectable.
 */

#define H5C_FRIEND /*suppress error about including H5Cpkg   */
#define H5F_FRIEND /*suppress error about including H5Fpkg   */

#include "hdf5.h"

#include "H5Cpkg.h"
#include "H5Fpkg.h"
#include "H5HGprivate.h"
#include "H5VLprivate.h"

#include "testhdf5.h"
#include "vfd_swmr_common.h"

#ifndef H5_HAVE_WIN32_API

#define MAX_READ_LEN_IN_SECONDS 2
#define TICK_LEN                4
#define MAX_LAG                 7
#define FSP_SIZE                4096
#define PAGE_BUF_SIZE           4096
#define ROWS                    256
#define COLS                    512
#define DEPTH                   1
#define RANK2                   2
#define RANK3                   3
#define NUM_ATTEMPTS            100
#define SKIP_CHUNK              0

/* Calculate the time passed in seconds.
 * X is the beginning time; Y is the ending time.
 * Expects X, Y to be struct timespec from the function call HDclock_gettime.
 */
#define TIME_PASSED(X, Y)                                                                                    \
    ((double)((Y.tv_sec - X.tv_sec) * 1000000000 + (Y.tv_nsec - X.tv_nsec))) / 1000000000.0

typedef struct _base {
    hsize_t depth, row, col;
} base_t;

typedef struct _mat {
    unsigned depth, rows, cols;
    uint32_t elt[1];
} mat_t;

typedef struct _quadrant {
    hsize_t start[RANK2];
    hsize_t stride[RANK2];
    hsize_t block[RANK2];
    hsize_t count[RANK2];
    hid_t   space, src_space;
} quadrant_t;

typedef struct _sources {
    hid_t ul, ur, bl, br;
} sources_t;

#define MANY_FILES 4

typedef struct {
    hid_t *     dataset;
    sources_t * sources;
    hid_t       file[MANY_FILES];
    hid_t       dapl, filetype, memspace, one_by_one_sid, quadrant_dcpl;
    unsigned    ndatasets;
    const char *filename[MANY_FILES];
    char        progname[PATH_MAX];
    struct {
        quadrant_t ul, ur, bl, br, src;
    } quadrants;
    unsigned int depth, cols, rows;
    unsigned int asteps;
    unsigned int nsteps;
    unsigned int part_chunk;
    unsigned int skip_chunk;
    unsigned int over_extend;
    bool         expand_2d;
    bool         test_3d;
    enum { vds_off, vds_single, vds_multi } vds;
    bool            use_vfd_swmr;
    bool            use_legacy_swmr;
    bool            use_named_pipe;
    bool            use_aux_proc;
    bool            do_perf;
    bool            cross_chunk_read;
    bool            writer;
    bool            fixed_array;
    bool            flush_raw_data;
    hsize_t         chunk_dims[RANK2];
    hsize_t         one_dee_max_dims[RANK2];
    hsize_t         fsp_size;
    size_t          page_buf_size;
    uint32_t        tick_len;
    uint32_t        max_lag;
    unsigned        mdc_init_size;
    size_t          chunk_cache_size;
    unsigned int    deflate_level;
    struct timespec ival;
} state_t;

/* Structure to hold info for named pipes */
typedef struct {
    const char *fifo_writer_to_reader; /* Name of fifo for writer to reader */
    const char *fifo_reader_to_writer; /* Name of fifo for reader to writer */
    int         fd_writer_to_reader;   /* File ID for fifo from writer to reader */
    int         fd_reader_to_writer;   /* File ID for fifo from reader to writer */
    int         notify;                /* Value to notify between writer and reader */
    int         verify;                /* Value to verify between writer and reader */
} np_state_t;

typedef struct {
    unsigned        step;
    struct timespec time;
} exchange_info_t;

/* Initializations for np_state_t */
#define NP_INITIALIZER                                                                                       \
    (np_state_t)                                                                                             \
    {                                                                                                        \
        .fifo_writer_to_reader = "./fifo_bigset_writer_to_reader",                                           \
        .fifo_reader_to_writer = "./fifo_bigset_reader_to_writer", .fd_writer_to_reader = -1,                \
        .fd_reader_to_writer = -1, .notify = 0, .verify = 0                                                  \
    }

static inline state_t
state_initializer(void)
{
    return (state_t){.memspace         = H5I_INVALID_HID,
                     .dapl             = H5I_INVALID_HID,
                     .file             = {H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID},
                     .filetype         = H5T_NATIVE_UINT32,
                     .one_by_one_sid   = H5I_INVALID_HID,
                     .quadrant_dcpl    = H5I_INVALID_HID,
                     .depth            = DEPTH,
                     .rows             = ROWS,
                     .cols             = COLS,
                     .ndatasets        = 5,
                     .asteps           = 10,
                     .nsteps           = 100,
                     .part_chunk       = 0,
                     .skip_chunk       = SKIP_CHUNK,
                     .over_extend      = 1,
                     .filename         = {"", "", "", ""},
                     .expand_2d        = false,
                     .test_3d          = false,
                     .vds              = vds_off,
                     .use_vfd_swmr     = true,
                     .use_legacy_swmr  = false,
                     .use_named_pipe   = true,
                     .use_aux_proc     = false,
                     .do_perf          = false,
                     .cross_chunk_read = false,
                     .writer           = true,
                     .fixed_array      = false,
                     .one_dee_max_dims = {ROWS, H5S_UNLIMITED},
                     .chunk_dims       = {ROWS, COLS},
                     .fsp_size         = FSP_SIZE,
                     .page_buf_size    = PAGE_BUF_SIZE,
                     .tick_len         = TICK_LEN,
                     .max_lag          = MAX_LAG,
                     .flush_raw_data   = false,
                     .mdc_init_size    = 0,
                     .chunk_cache_size = 0,
                     .deflate_level    = 0,
                     .ival             = (struct timespec){.tv_sec = MAX_READ_LEN_IN_SECONDS, .tv_nsec = 0}};
}

static bool state_init(state_t *, int, char **);

static const hid_t badhid = H5I_INVALID_HID;

static hsize_t two_dee_max_dims[RANK2], three_dee_max_dims[RANK3];

static void
usage(const char *progname)
{
    HDfprintf(
        stderr,
        "usage: %s [-A] [-C] [-F] [-M] [-P] [-R] [-S] [-V] [-W] [-a steps] [-b] [-c cols]\n"
        "    [-d dims] [-e depth] [-f tick_len] [-g max_lag] [-j skip_chunk] [-k part_chunk]\n"
        "    [-l tick_num] [-n iterations] [-o page_buf_size] [-p fsp_size] [-r rows]\n"
        "    [-s datasets] [-t] [-u over_extend] [-v chunk_cache_size] [-w deflate_level]\n"
        "\n"
        "-A:                   use the auxiliary process to update the metadata file\n"
        "-C:                   cross-over chunk read during chunk verification\n"
        "-F:                   fixed maximal dimension for the chunked datasets\n"
        "-M:	               use virtual datasets and many source\n"
        "                      files\n"
        "-N:                   do not use named pipes\n"
        "-P:                   do the performance measurement\n"
        "-R:                   flush raw data\n"
        "-S:	               do not use VFD SWMR\n"
        "-T:                   use legacy SWMR (-S and -N must also be specified)\n"
        "-V:	               use virtual datasets and a single\n"
        "                      source file\n"
        "-a steps:	       `steps` between adding attributes\n"
        "-b:	               write data in big-endian byte order\n"
        "-c cols:	       `cols` columns of the chunk\n"
        "-d 1|one|2|two|both:  select dataset expansion in one or\n"
        "                      both dimensions\n"
        "-e depth:	       the first dimension of the 3D chunk\n"
        "-f tick_len:          tick length\n"
        "-g max_lag:           maximal lag\n"
        "-j skip_chunk:        skip the Nth (skip_chunk) chunks during chunk writing\n"
        "-k part_chunk:        the size for partial chunk write (only along the first dimension)\n"
        "-l tick_num:          expected maximal number of ticks from\n"
        "                      the writer's finishing creation to the reader's finishing validation\n"
        "-m mdc_init_size:     the initial size of metadata cache in megabytes (must be between 1 and 32MB)\n"
        "-n iterations:        how many times to expand each dataset\n"
        "-o page_buf_size:     page buffer size\n"
        "-p fsp_size:          file space page size\n"
        "-r rows:	       `rows` rows of the chunk\n"
        "-s datasets:          number of datasets to create\n"
        "-t:                   enable test for 3D datasets (dataset expansion is along one dimension)\n"
        "                      currently, 3D datasets isn't tested with VDS\n"
        "-u over_extend:       extend the size of the dataset in multiple chunks or partial chunks\n"
        "-v chunk_cache_size:  the size of raw data chunk cache in bytes\n"
        "-w deflate_level:     the level (0 - 9) of gzip compression\n"
        "\n",
        progname);
    exit(EXIT_FAILURE);
}

static bool
make_quadrant_dataspace(state_t *s, quadrant_t *q)
{
    if ((q->space = H5Screate_simple(NELMTS(s->chunk_dims), s->chunk_dims,
                                     s->expand_2d ? two_dee_max_dims : s->one_dee_max_dims)) < 0) {
        HDfprintf(stderr, "H5Screate_simple failed\n");
        TEST_ERROR;
    }

    if (H5Sselect_hyperslab(q->space, H5S_SELECT_SET, q->start, q->stride, q->count, q->block) < 0) {
        HDfprintf(stderr, "H5Sselect_hyperslab failed\n");
        TEST_ERROR;
    }

    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(q->space);
    }
    H5E_END_TRY;

    return false;
}

static bool
state_init(state_t *s, int argc, char **argv)
{
    unsigned long     tmp;
    int               ch;
    unsigned          i;
    const hsize_t     dims  = 1;
    char *            tfile = NULL;
    char *            end;
    size_t            rdcc_nslots, rdcc_nbytes;
    double            rdcc_w0;
    quadrant_t *const ul = &s->quadrants.ul, *const ur = &s->quadrants.ur, *const bl = &s->quadrants.bl,
                      *const br = &s->quadrants.br, *const src = &s->quadrants.src;
    const char *personality;

    *s = state_initializer();

    if (H5_basename(argv[0], &tfile) < 0) {
        HDfprintf(stderr, "H5_basename failed\n");
        TEST_ERROR;
    }

    esnprintf(s->progname, sizeof(s->progname), "%s", tfile);

    if (tfile)
        HDfree(tfile);

    while ((ch = getopt(argc, argv, "ACFMNPRSTVa:bc:d:e:f:g:j:k:l:m:n:o:p:qr:s:tu:v:w:")) != -1) {
        switch (ch) {
            case 'A':
                s->use_aux_proc = true;
                break;
            case 'C':
                /* This flag indicates cross-over chunk read during data validation */
                s->cross_chunk_read = true;
                break;
            case 'F':
                /* The flag to indicate whether the maximal dimension of the chunked datasets is fixed or
                 * unlimited */
                s->fixed_array = true;
                break;
            case 'M':
                s->vds = vds_multi;
                break;
            case 'P':
                s->do_perf = true;
                break;
            case 'R':
                s->flush_raw_data = true;
                break;
            case 'S':
                s->use_vfd_swmr = false;
                break;
            case 'T':
                s->use_legacy_swmr = true;
                break;
            case 'V':
                s->vds = vds_single;
                break;
            case 'N':
                /* Disable named pipes, mainly for running the writer and reader seperately */
                s->use_named_pipe = false;
                break;
            case 'd':
                if (strcmp(optarg, "1") == 0 || strcmp(optarg, "one") == 0)
                    s->expand_2d = false;
                else if (strcmp(optarg, "2") == 0 || strcmp(optarg, "two") == 0 ||
                         strcmp(optarg, "both") == 0)
                    s->expand_2d = true;
                else {
                    HDfprintf(stderr, "bad -d argument %s\n", optarg);
                    TEST_ERROR;
                }
                break;
            case 'a':
            case 'c':
            case 'e':
            case 'f':
            case 'g':
            case 'j':
            case 'k':
            case 'l':
            case 'm':
            case 'n':
            case 'o':
            case 'p':
            case 'r':
            case 's':
            case 'u':
            case 'v':
            case 'w':
                errno = 0;
                tmp   = HDstrtoul(optarg, &end, 0);
                if (end == optarg || *end != '\0') {
                    HDfprintf(stderr, "couldn't parse -%c argument %s\n", ch, optarg);
                    TEST_ERROR;
                }
                else if (errno != 0) {
                    HDfprintf(stderr, "couldn't parse -%c argument %s\n", ch, optarg);
                    TEST_ERROR;
                }
                else if (tmp > UINT_MAX) {
                    HDfprintf(stderr, "-%c argument %lu too large", ch, tmp);
                    TEST_ERROR;
                }

                if ((ch == 'c' || ch == 'r') && tmp == 0) {
                    HDfprintf(stderr, "-%c argument %lu must be >= 1", ch, tmp);
                    TEST_ERROR;
                }

                if (ch == 'a')
                    s->asteps = (unsigned)tmp;
                else if (ch == 'c')
                    s->cols = (unsigned)tmp;
                else if (ch == 'e')
                    s->depth = (unsigned)tmp;
                else if (ch == 'f')
                    s->tick_len = (unsigned)tmp;
                else if (ch == 'g')
                    s->max_lag = (unsigned)tmp;
                else if (ch == 'j')
                    s->skip_chunk = (unsigned)tmp;
                else if (ch == 'k')
                    s->part_chunk = (unsigned)tmp;
                else if (ch == 'l') {
                    /* Translate the tick number to time represented by the timespec struct */
                    float    time = (float)(((unsigned)tmp * TICK_LEN) / 10.0);
                    unsigned sec  = (unsigned)time;
                    unsigned nsec = (unsigned)((time - sec) * 10 * 1000 * 1000);

                    s->ival.tv_sec  = sec;
                    s->ival.tv_nsec = nsec;
                }
                else if (ch == 'm')
                    s->mdc_init_size = (unsigned)tmp;
                else if (ch == 'n')
                    s->nsteps = (unsigned)tmp;
                else if (ch == 'o')
                    s->page_buf_size = (unsigned)tmp;
                else if (ch == 'p')
                    s->fsp_size = (unsigned)tmp;
                else if (ch == 'r')
                    s->rows = (unsigned)tmp;
                else if (ch == 'u')
                    s->over_extend = (unsigned)tmp;
                else if (ch == 'v')
                    s->chunk_cache_size = (unsigned)tmp;
                else if (ch == 'w')
                    s->deflate_level = (unsigned)tmp;
                else
                    s->ndatasets = (unsigned)tmp;
                break;
            case 't':
                s->test_3d = true;
                break;
            case 'b':
                s->filetype = H5T_STD_U32BE;
                break;
            case 'q':
                verbosity = 0;
                break;
            case '?':
            default:
                usage(s->progname);
                break;
        }
    }
    argc -= optind;
    argv += optind;

    if (argc > 0) {
        HDfprintf(stderr, "unexpected command-line arguments\n");
        TEST_ERROR;
    }

#ifdef H5_HAVE_AUX_PROCESS
    if (s->vds == vds_multi)
        exit(EXIT_SUCCESS);
#endif

    if (s->vds != vds_off && s->expand_2d) {
        HDfprintf(stderr, "virtual datasets and 2D datasets are mutually exclusive\n");
        TEST_ERROR;
    }

    if (s->test_3d) {
        if (s->depth < 1) {
            HDfprintf(stderr, "The depth of 3D dataset can't be less than 1\n");
            TEST_ERROR;
        }

        if (s->expand_2d) {
            HDfprintf(stderr, "3D dataset test doesn't support 2D expansion\n");
            TEST_ERROR;
        }

        if (s->vds != vds_off) {
            HDfprintf(stderr, "3D dataset test doesn't support VDS\n");
            TEST_ERROR;
        }
    }

    s->chunk_dims[0] = s->rows;
    s->chunk_dims[1] = s->cols;

    s->one_dee_max_dims[0] = s->rows;
    if (s->fixed_array) {
        s->one_dee_max_dims[1] = s->cols * s->nsteps;
        two_dee_max_dims[0]    = s->rows * s->nsteps;
        two_dee_max_dims[1]    = s->cols * s->nsteps;

        if (s->test_3d) {
            three_dee_max_dims[0] = s->depth * s->nsteps;
            three_dee_max_dims[1] = s->rows;
            three_dee_max_dims[2] = s->cols;
        }
    }
    else {
        s->one_dee_max_dims[1] = H5S_UNLIMITED;
        two_dee_max_dims[0] = two_dee_max_dims[1] = H5S_UNLIMITED;

        if (s->test_3d) {
            three_dee_max_dims[0] = H5S_UNLIMITED;
            three_dee_max_dims[1] = s->rows;
            three_dee_max_dims[2] = s->cols;
        }
    }

    if (s->vds != vds_off) {
        const hsize_t half_chunk_dims[RANK2] = {s->rows / 2, s->cols / 2};
        hsize_t       half_max_dims[RANK2];

        if (s->fixed_array) {
            half_max_dims[0] = s->rows / 2;
            half_max_dims[1] = (s->cols * s->nsteps) / 2;
        }
        else {
            half_max_dims[0] = s->rows / 2;
            half_max_dims[1] = H5S_UNLIMITED;
        }

        if ((s->quadrant_dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
            HDfprintf(stderr, "H5Pcreate failed\n");
            TEST_ERROR;
        }

        if (H5Pset_chunk(s->quadrant_dcpl, RANK2, half_chunk_dims) < 0) {
            HDfprintf(stderr, "H5Pset_chunk failed\n");
            TEST_ERROR;
        }

        *ul = (quadrant_t){.start  = {0, 0},
                           .stride = {s->rows, s->cols},
                           .block  = {s->rows / 2, s->cols / 2},
                           .count  = {1, H5S_UNLIMITED}};

        *ur = (quadrant_t){.start  = {s->rows / 2, 0},
                           .stride = {s->rows, s->cols},
                           .block  = {s->rows / 2, s->cols / 2},
                           .count  = {1, H5S_UNLIMITED}};

        *bl = (quadrant_t){.start  = {0, s->cols / 2},
                           .stride = {s->rows, s->cols},
                           .block  = {s->rows / 2, s->cols / 2},
                           .count  = {1, H5S_UNLIMITED}};

        *br = (quadrant_t){.start  = {s->rows / 2, s->cols / 2},
                           .stride = {s->rows, s->cols},
                           .block  = {s->rows / 2, s->cols / 2},
                           .count  = {1, H5S_UNLIMITED}};

        if (!make_quadrant_dataspace(s, ul)) {
            HDfprintf(stderr, "make_quadrant_dataspace failed\n");
            TEST_ERROR;
        }

        if (!make_quadrant_dataspace(s, ur)) {
            HDfprintf(stderr, "make_quadrant_dataspace failed\n");
            TEST_ERROR;
        }

        if (!make_quadrant_dataspace(s, bl)) {
            HDfprintf(stderr, "make_quadrant_dataspace failed\n");
            TEST_ERROR;
        }

        if (!make_quadrant_dataspace(s, br)) {
            HDfprintf(stderr, "make_quadrant_dataspace failed\n");
            TEST_ERROR;
        }

        *src = (quadrant_t){.start  = {0, 0},
                            .stride = {s->rows / 2, s->cols / 2},
                            .block  = {s->rows / 2, s->cols / 2},
                            .count  = {1, H5S_UNLIMITED}};

        if ((src->space = H5Screate_simple(RANK2, half_chunk_dims, half_max_dims)) < 0) {
            HDfprintf(stderr, "H5Screate_simple failed\n");
            TEST_ERROR;
        }

        if (H5Sselect_hyperslab(src->space, H5S_SELECT_SET, src->start, src->stride, src->count, src->block) <
            0) {
            HDfprintf(stderr, "H5Sselect_hyperslab failed\n");
            TEST_ERROR;
        }

        if ((ul->src_space = H5Screate_simple(RANK2, half_chunk_dims, half_max_dims)) < 0) {
            HDfprintf(stderr, "H5Screate_simple failed\n");
            TEST_ERROR;
        }

        if ((ur->src_space = H5Screate_simple(RANK2, half_chunk_dims, half_max_dims)) < 0) {
            HDfprintf(stderr, "H5Screate_simple failed\n");
            TEST_ERROR;
        }

        if ((bl->src_space = H5Screate_simple(RANK2, half_chunk_dims, half_max_dims)) < 0) {
            HDfprintf(stderr, "H5Screate_simple failed\n");
            TEST_ERROR;
        }

        if ((br->src_space = H5Screate_simple(RANK2, half_chunk_dims, half_max_dims)) < 0) {
            HDfprintf(stderr, "H5Screate_simple failed\n");
            TEST_ERROR;
        }
    }

    /* space for attributes */
    if ((s->one_by_one_sid = H5Screate_simple(1, &dims, &dims)) < 0) {
        HDfprintf(stderr, "H5Screate_simple failed\n");
        TEST_ERROR;
    }

    if ((s->dataset = HDmalloc(sizeof(hid_t) * s->ndatasets)) == NULL) {
        HDfprintf(stderr, "HDmalloc failed\n");
        TEST_ERROR;
    }

    if ((s->sources = HDmalloc(sizeof(*s->sources) * s->ndatasets)) == NULL) {
        HDfprintf(stderr, "HDmalloc failed\n");
        TEST_ERROR;
    }

    for (i = 0; i < s->ndatasets; i++) {
        s->dataset[i]    = badhid;
        s->sources[i].ul = s->sources[i].ur = s->sources[i].bl = s->sources[i].br = badhid;
    }

    if (s->test_3d) {
        hsize_t dims3[RANK3] = {s->depth, s->chunk_dims[0], s->chunk_dims[1]};

        if (s->part_chunk)
            dims3[0] = s->part_chunk;

        if ((s->memspace = H5Screate_simple(RANK3, dims3, NULL)) < 0) {
            HDfprintf(stderr, "H5Screate_simple failed\n");
            TEST_ERROR;
        }
    }
    else {
        hsize_t dims2[RANK2];

        if (s->expand_2d) {
            dims2[0] = s->chunk_dims[0];
            dims2[1] = s->chunk_dims[1];
        }
        else {
            dims2[0] = s->chunk_dims[0];

            if (s->part_chunk)
                dims2[1] = s->part_chunk;
            else
                dims2[1] = s->chunk_dims[1];
        }

        if ((s->memspace = H5Screate_simple(RANK2, dims2, NULL)) < 0) {
            HDfprintf(stderr, "H5Screate_simple failed\n");
            TEST_ERROR;
        }
    }

    /* The default is zero, meaning no skip */
    if (s->skip_chunk == 1) {
        HDfprintf(stderr, "can't skip every chunk\n");
        TEST_ERROR;
    }

    if (s->over_extend == 0) {
        HDfprintf(stderr, "Extension of the dataset can't be zero\n");
        TEST_ERROR;
    }

    s->filename[0] = "vfd_swmr_bigset.h5";
    if (s->vds == vds_multi) {
        s->filename[1] = "vfd_swmr_bigset-ur.h5";
        s->filename[2] = "vfd_swmr_bigset-bl.h5";
        s->filename[3] = "vfd_swmr_bigset-br.h5";
    }
    else {
        s->filename[1] = s->filename[0];
        s->filename[2] = s->filename[0];
        s->filename[3] = s->filename[0];
    }

    personality = HDstrstr(s->progname, "vfd_swmr_bigset_");

    if (personality != NULL && HDstrcmp(personality, "vfd_swmr_bigset_writer") == 0)
        s->writer = true;
    else if (personality != NULL && HDstrcmp(personality, "vfd_swmr_bigset_reader") == 0)
        s->writer = false;
    else {
        HDfprintf(stderr, "unknown personality, expected vfd_swmr_bigset_{reader,writer}\n");
        TEST_ERROR;
    }

    if ((s->dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0) {
        HDfprintf(stderr, "H5Pcreate failed\n");
        TEST_ERROR;
    }

    if (s->chunk_cache_size) {
        if (H5Pget_chunk_cache(s->dapl, &rdcc_nslots, &rdcc_nbytes, &rdcc_w0) < 0) {
            HDfprintf(stderr, "H5Pget_chunk_cache failed\n");
            TEST_ERROR;
        }

        if (H5Pset_chunk_cache(s->dapl, rdcc_nslots, s->chunk_cache_size, rdcc_w0) < 0) {
            HDfprintf(stderr, "H5Pset_chunk_cache failed\n");
            TEST_ERROR;
        }
    }

    if (s->deflate_level > 9) {
        HDfprintf(stderr, "deflation level must be between 0 and 9\n");
        TEST_ERROR;
    }

    if (s->vds != vds_off && H5Pset_virtual_view(s->dapl, H5D_VDS_FIRST_MISSING) < 0) {
        HDfprintf(stderr, "H5Pset_virtual_view failed\n");
        TEST_ERROR;
    }

    if (s->use_legacy_swmr) {
        if (s->use_vfd_swmr) {
            HDfprintf(stderr, "Can't use both VFD SWMR and Legacy SWMR\n");
            TEST_ERROR;
        }

        if (s->use_named_pipe) {
            HDfprintf(stderr, "Can't use named pipe for the Legacy SWMR\n");
            TEST_ERROR;
        }
    }

    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(s->quadrant_dcpl);
        H5Sclose(ul->space);
        H5Sclose(ur->space);
        H5Sclose(bl->space);
        H5Sclose(br->space);
        H5Sclose(ul->src_space);
        H5Sclose(ur->src_space);
        H5Sclose(bl->src_space);
        H5Sclose(br->src_space);
        H5Sclose(src->space);
        H5Sclose(s->one_by_one_sid);
        H5Sclose(s->memspace);
    }
    H5E_END_TRY;

    if (tfile)
        HDfree(tfile);

    if (s->dataset)
        HDfree(s->dataset);

    if (s->sources)
        HDfree(s->sources);

    return false;
}

static bool
state_destroy(state_t *s)
{
    size_t          i;
    struct timespec start_time, end_time;

    if (H5Pclose(s->dapl) < 0) {
        HDfprintf(stderr, "H5Pclose failed\n");
        TEST_ERROR;
    }

    if (s->vds != vds_off) {
        quadrant_t *const ul = &s->quadrants.ul, *const ur = &s->quadrants.ur, *const bl = &s->quadrants.bl,
                          *const br = &s->quadrants.br;

        if (H5Sclose(ul->src_space) < 0 || H5Sclose(ur->src_space) < 0 || H5Sclose(bl->src_space) < 0 ||
            H5Sclose(br->src_space) < 0) {
            HDfprintf(stderr, "H5Sclose failed\n");
            TEST_ERROR;
        }

        if (H5Sclose(ul->space) < 0 || H5Sclose(ur->space) < 0 || H5Sclose(bl->space) < 0 ||
            H5Sclose(br->space) < 0) {
            HDfprintf(stderr, "H5Sclose failed\n");
            TEST_ERROR;
        }

        if (H5Pclose(s->quadrant_dcpl) < 0) {
            HDfprintf(stderr, "H5Pclose failed\n");
            TEST_ERROR;
        }
    }

    if (H5Sclose(s->one_by_one_sid) < 0) {
        HDfprintf(stderr, "H5Sclose failed\n");
        TEST_ERROR;
    }

    if (H5Sclose(s->memspace) < 0) {
        HDfprintf(stderr, "H5Sclose failed\n");
        TEST_ERROR;
    }

    /* The writer ends the tick before closing the file to make the data visible to the reader */
    if (s->writer && s->use_vfd_swmr) {
        unsigned long j;

        if (s->vds != vds_multi) {
            if (H5Fvfd_swmr_end_tick(s->file[0]) < 0) {
                HDfprintf(stderr, "H5Fvfd_swmr_end_tick failed\n");
                TEST_ERROR;
            }
        }
        else {
            for (j = 0; j < NELMTS(s->file); j++)
                if (H5Fvfd_swmr_end_tick(s->file[j]) < 0) {
                    HDfprintf(stderr, "H5Fvfd_swmr_end_tick failed\n");
                    TEST_ERROR;
                }
        }
    }

    /* For checking the time spent in file close.  It's for running the writer alone */
    if (s->do_perf) {
        if (HDclock_gettime(CLOCK_MONOTONIC, &start_time) == -1) {
            HDfprintf(stderr, "HDclock_gettime failed");
            TEST_ERROR;
        }
    }

    for (i = 0; i < NELMTS(s->file); i++) {
        hid_t fid = s->file[i];

        s->file[i] = badhid;

        if (s->vds != vds_multi && i > 0)
            continue;

        if (H5Fclose(fid) < 0) {
            HDfprintf(stderr, "H5Fclose failed\n");
            TEST_ERROR;
        }
    }

    /* For checking the time spent in file close.  It's for running the writer alone */
    if (s->do_perf) {
        if (HDclock_gettime(CLOCK_MONOTONIC, &end_time) == -1) {
            HDfprintf(stderr, "HDclock_gettime failed");
            TEST_ERROR;
        }

        HDfprintf(stdout, "File close time (for running the writer alone) = %lf seconds\n",
                  TIME_PASSED(start_time, end_time));
    }

    if (s->dataset)
        HDfree(s->dataset);

    if (s->sources)
        HDfree(s->sources);

    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(s->quadrant_dcpl);
        H5Sclose(s->one_by_one_sid);
        H5Sclose(s->memspace);
    }
    H5E_END_TRY;

    if (s->dataset)
        HDfree(s->dataset);

    if (s->sources)
        HDfree(s->sources);

    return false;
}

/*
 * Initialize the named pipes for test synchronization.
 * (This function can go to vfd_swmr_common.c.  It's the same as
 * the one in vfd_swmr_attrdset_writer.c)
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
                HDfprintf(stderr, "HDremove fifo_writer_to_reader failed\n");
                TEST_ERROR;
            }

        if (HDaccess(np->fifo_reader_to_writer, F_OK) == 0)
            if (HDremove(np->fifo_reader_to_writer) != 0) {
                HDfprintf(stderr, "HDremove fifo_reader_to_writer failed\n");
                TEST_ERROR;
            }

        /* Writer creates two named pipes(FIFO) */
        if (HDmkfifo(np->fifo_writer_to_reader, 0600) < 0) {
            HDfprintf(stderr, "HDmkfifo fifo_writer_to_reader failed\n");
            TEST_ERROR;
        }

        if (HDmkfifo(np->fifo_reader_to_writer, 0600) < 0) {
            HDfprintf(stderr, "HDmkfifo fifo_reader_to_writer failed\n");
            TEST_ERROR;
        }
    }

    /* Both the writer and reader open the pipes */
    if ((np->fd_writer_to_reader = HDopen(np->fifo_writer_to_reader, O_RDWR)) < 0) {
        HDfprintf(stderr, "HDopen fifo_writer_to_reader failed\n");
        TEST_ERROR;
    }

    if ((np->fd_reader_to_writer = HDopen(np->fifo_reader_to_writer, O_RDWR)) < 0) {
        HDfprintf(stderr, "HDopen fifo_reader_to_writer failed\n");
        TEST_ERROR;
    }

    return true;

error:
    return false;
} /* np_init() */

/*
 * Close and remove the named pipes.
 * (This function can go to vfd_swmr_common.c.  It's the same as
 * the one in vfd_swmr_attrdset_writer.c)
 */
static bool
np_close(np_state_t *np, bool writer)
{
    /* Both the writer and reader close the named pipes */
    if (HDclose(np->fd_writer_to_reader) < 0) {
        HDfprintf(stderr, "HDclose fd_writer_to_reader failed\n");
        TEST_ERROR;
    }

    if (HDclose(np->fd_reader_to_writer) < 0) {
        HDfprintf(stderr, "HDclose fd_reader_to_writer failed\n");
        TEST_ERROR;
    }

    /* Reader finishes last and deletes the named pipes */
    if (!writer) {
        if (HDremove(np->fifo_writer_to_reader) != 0) {
            HDfprintf(stderr, "HDremove fifo_writer_to_reader failed\n");
            TEST_ERROR;
        }

        if (HDremove(np->fifo_reader_to_writer) != 0) {
            HDfprintf(stderr, "HDremove fifo_reader_to_writer failed\n");
            TEST_ERROR;
        }
    }
    return true;

error:
    return false;
} /* np_close() */

/* Wait for the writer's notice before starting validation */
static int
reader_verify(np_state_t np, int verify)
{
    int notify;

    if (HDread(np.fd_writer_to_reader, &notify, sizeof(int)) < 0) {
        HDfprintf(stderr, "HDread failed\n");
        TEST_ERROR;
    }

    if (notify != verify) {
        HDfprintf(stderr, "expected %d but read %d\n", verify, notify);
        TEST_ERROR;
    }

    return 0;

error:
    return -1;
}

/* Notify the reader of finishing creation by sending the timestamp
 * and wait for the reader to finish validation before proceeding */
static int
notify_and_wait_for_reader(state_t *s, np_state_t *np)
{
    int             notify;
    unsigned int    i;
    struct timespec last = {0, 0};

    /* Get the time when finishing creation */
    if (HDclock_gettime(CLOCK_MONOTONIC, &last) < 0) {
        HDfprintf(stderr, "HDclock_gettime failed\n");
        TEST_ERROR;
    }

    /* Notify the reader of finishing creation by sending the timestamp */
    if (HDwrite(np->fd_writer_to_reader, &last, sizeof(last)) < 0) {
        HDfprintf(stderr, "HDwrite failed\n");
        TEST_ERROR;
    }

    /* During the wait, writer makes repeated HDF5 API calls so as to trigger
     * EOT at approximately the correct time */
    for (i = 0; i < MAX_LAG + 1; i++) {
        decisleep(TICK_LEN);

        H5E_BEGIN_TRY
        {
            H5Aexists(s->file[0], "nonexistent");
        }
        H5E_END_TRY;
    }

    /* Wait until the reader finishes validating creation */
    if (HDread(np->fd_reader_to_writer, &notify, sizeof(int)) < 0) {
        HDfprintf(stderr, "HDread failed\n");
        TEST_ERROR;
    }

    if (notify != np->verify) {
        HDfprintf(stderr, "expected %d but read %d\n", np->verify, notify);
        TEST_ERROR;
    }

    return 0;

error:
    return -1;
}

/* Receive the notice of the writer finishing dataset creation (timestamp)
 * Make sure the dataset validation doesn't take longer than the expected time.
 * This time period is from the writer finishing dataset creation to the reader finishing
 * the validation of dataset creation */
static int
reader_check_time_and_notify_writer(np_state_t *np, state_t s)
{
    struct timespec last = {0, 0};

    /* Receive the notice of the writer finishing creation (timestamp) */
    if (HDread(np->fd_writer_to_reader, &last, sizeof(last)) < 0) {
        HDfprintf(stderr, "HDread failed\n");
        TEST_ERROR;
    }

    /* If the dataset validation takes longer than the expected time, issue a warning.
     * This time period is from the writer finishing dataset creation to the reader finishing
     * the validation of dataset creation */
    if (below_speed_limit(&last, &(s.ival))) {
        AT();
        HDfprintf(stderr, "Warning: dataset validation took too long to finish\n");
    }

    /* Notify the writer that dataset validation is finished */
    if (HDwrite(np->fd_reader_to_writer, &(np->notify), sizeof(int)) < 0) {
        HDfprintf(stderr, "HDwrite failed\n");
        TEST_ERROR;
    }

    return 0;

error:
    return -1;
}

/* Notify the reader by sending the timestamp and the number of chunks written */
static int
notify_reader(np_state_t *np, unsigned step)
{
    exchange_info_t *last = HDcalloc(1, sizeof(exchange_info_t));

    /* Get the time */
    if (HDclock_gettime(CLOCK_MONOTONIC, &(last->time)) < 0) {
        HDfprintf(stderr, "HDclock_gettime failed\n");
        TEST_ERROR;
    }

    last->step = step;

    /* Notify the reader by sending the timestamp and the number of chunks written */
    if (HDwrite(np->fd_writer_to_reader, last, sizeof(exchange_info_t)) < 0) {
        HDfprintf(stderr, "HDwrite failed");
        TEST_ERROR;
    }

    if (last)
        HDfree(last);

    return 0;

error:
    return -1;
}

/*-------------------------------------------------------------------------
 * Function:    md_ck_cb()
 *
 * Purpose:     This is the callback function for debugging only.  It's used
 *              when the H5F_ACS_GENERATE_MD_CK_CB_NAME property is set in fapl.
 *                  --Opens and read the metadata file into a buffer.
 *                  --Generate checksum for the metadata file
 *                  --Write the tick number and the checksum to the checksum file
 *
 * Return:      0 if test is sucessful
 *              1 if test fails
 *
 *-------------------------------------------------------------------------
 */
static herr_t
md_ck_cb(char *md_file_path, uint64_t updater_seq_num)
{
    FILE *   md_fp  = NULL;      /* Metadata file pointer */
    FILE *   chk_fp = NULL;      /* Checksum file pointer */
    long     size   = 0;         /* File size returned from HDftell() */
    void *   buf    = NULL;      /* Buffer for holding the metadata file content */
    uint32_t chksum = 0;         /* The checksum generated for the metadata file */
    char     chk_name[1024 + 4]; /* Buffer for the checksum file name */
    size_t   ret;                /* Return value */

    /* Open the metadata file */
    if ((md_fp = HDfopen(md_file_path, "r")) == NULL)
        FAIL_STACK_ERROR;

    /* Set file pointer at end of file.*/
    if (HDfseek(md_fp, 0, SEEK_END) < 0)
        FAIL_STACK_ERROR;

    /* Get the current position of the file pointer.*/
    if ((size = HDftell(md_fp)) < 0)
        FAIL_STACK_ERROR;

    if (size != 0) {

        HDrewind(md_fp);

        if ((buf = HDmalloc((size_t)size)) == NULL)
            FAIL_STACK_ERROR;

        /* Read the metadata file to buf */
        if ((ret = HDfread(buf, 1, (size_t)size, md_fp)) != (size_t)size)
            FAIL_STACK_ERROR;

        /* Calculate checksum of the metadata file */
        chksum = H5_checksum_metadata(buf, (size_t)size, 0);
    }

    /* Close the metadata file */
    if (md_fp && HDfclose(md_fp) < 0)
        FAIL_STACK_ERROR;

    /*
     *  Checksum file
     */

    /* Generate checksum file name: <md_file_path>.chk */
    HDsprintf(chk_name, "%s.chk", md_file_path);

    /* Open checksum file for append */
    if ((chk_fp = HDfopen(chk_name, "a")) == NULL)
        FAIL_STACK_ERROR;

    /* Write the updater sequence number to the checksum file */
    if ((ret = HDfwrite(&updater_seq_num, sizeof(uint64_t), 1, chk_fp)) != 1)
        FAIL_STACK_ERROR;

    /* Write the checksum to the checksum file */
    if ((ret = HDfwrite(&chksum, sizeof(uint32_t), 1, chk_fp)) != 1)
        FAIL_STACK_ERROR;

    /* Close the checksum file */
    if (chk_fp && HDfclose(chk_fp) != 0)
        FAIL_STACK_ERROR;

    if (buf)
        HDfree(buf);

    return 0;

error:
    if (buf)
        HDfree(buf);
    if (md_fp)
        HDfclose(md_fp);
    if (chk_fp)
        HDfclose(chk_fp);

    return -1;
} /* md_ck_cb() */

static bool
create_extensible_dset(state_t *s, unsigned int which)
{
    quadrant_t *const ul = &s->quadrants.ul, *const ur = &s->quadrants.ur, *const bl = &s->quadrants.bl,
                      *const br = &s->quadrants.br, *const src = &s->quadrants.src;
    char dname[sizeof("/dataset-9999999999")];
    char ul_dname[sizeof("/ul-dataset-9999999999")], ur_dname[sizeof("/ur-dataset-9999999999")],
        bl_dname[sizeof("/bl-dataset-9999999999")], br_dname[sizeof("/br-dataset-9999999999")];
    hid_t   dcpl = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, filespace = H5I_INVALID_HID;
    hsize_t dims3[3] = {s->depth, s->chunk_dims[0], s->chunk_dims[1]};

    esnprintf(dname, sizeof(dname), "/dataset-%d", which);

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
        HDfprintf(stderr, "H5Pcreate failed\n");
        TEST_ERROR;
    }

    if (s->test_3d) {
        /* The chunk is L x M x N and grows along the first dimension */
        if (H5Pset_chunk(dcpl, RANK3, dims3) < 0) {
            HDfprintf(stderr, "H5Pset_chunk for 3D dataset failed\n");
            TEST_ERROR;
        }
    }
    else {
        if (H5Pset_chunk(dcpl, RANK2, s->chunk_dims) < 0) {
            HDfprintf(stderr, "H5Pset_chunk for 2D dataset failed\n");
            TEST_ERROR;
        }
    }

    /* Never write fill value when new chunks are allocated */
    if (H5Pset_fill_time(dcpl, H5D_FILL_TIME_NEVER) < 0) {
        HDfprintf(stderr, "H5Pset_fill_time failed\n");
        TEST_ERROR;
    }

    /* Early space allocation */
    if (H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0) {
        HDfprintf(stderr, "H5Pset_alloc_time failed\n");
        TEST_ERROR;
    }

    /* GZIP compression */
    if (s->deflate_level && H5Pset_deflate(dcpl, s->deflate_level) < 0) {
        HDfprintf(stderr, "H5Pset_deflate failed\n");
        TEST_ERROR;
    }

    if (s->vds != vds_off) {
        sources_t *const srcs = &s->sources[which];

        esnprintf(ul_dname, sizeof(ul_dname), "/ul-dataset-%d", which);
        esnprintf(ur_dname, sizeof(ur_dname), "/ur-dataset-%d", which);
        esnprintf(bl_dname, sizeof(bl_dname), "/bl-dataset-%d", which);
        esnprintf(br_dname, sizeof(br_dname), "/br-dataset-%d", which);

        if ((srcs->ul = H5Dcreate2(s->file[0], ul_dname, s->filetype, ul->src_space, H5P_DEFAULT,
                                   s->quadrant_dcpl, s->dapl)) < 0) {
            HDfprintf(stderr, "H5Dcreate2 failed\n");
            TEST_ERROR;
        }

        if ((srcs->ur = H5Dcreate2(s->file[1], ur_dname, s->filetype, ur->src_space, H5P_DEFAULT,
                                   s->quadrant_dcpl, s->dapl)) < 0) {
            HDfprintf(stderr, "H5Dcreate2 failed\n");
            TEST_ERROR;
        }

        if ((srcs->bl = H5Dcreate2(s->file[2], bl_dname, s->filetype, bl->src_space, H5P_DEFAULT,
                                   s->quadrant_dcpl, s->dapl)) < 0) {
            HDfprintf(stderr, "H5Dcreate2 failed\n");
            TEST_ERROR;
        }

        if ((srcs->br = H5Dcreate2(s->file[3], br_dname, s->filetype, br->src_space, H5P_DEFAULT,
                                   s->quadrant_dcpl, s->dapl)) < 0) {
            HDfprintf(stderr, "H5Dcreate2 failed\n");
            TEST_ERROR;
        }

        if (H5Pset_virtual(dcpl, ul->space, s->filename[0], ul_dname, src->space) < 0) {
            HDfprintf(stderr, "H5Pset_virtual failed\n");
            TEST_ERROR;
        }

        if (H5Pset_virtual(dcpl, ur->space, s->filename[1], ur_dname, src->space) < 0) {
            HDfprintf(stderr, "H5Pset_virtual failed\n");
            TEST_ERROR;
        }

        if (H5Pset_virtual(dcpl, bl->space, s->filename[2], bl_dname, src->space) < 0) {
            HDfprintf(stderr, "H5Pset_virtual failed\n");
            TEST_ERROR;
        }

        if (H5Pset_virtual(dcpl, br->space, s->filename[3], br_dname, src->space) < 0) {
            HDfprintf(stderr, "H5Pset_virtual failed\n");
            TEST_ERROR;
        }
    }

    if (s->test_3d) {
        if ((filespace = H5Screate_simple(RANK3, dims3, three_dee_max_dims)) < 0) {
            HDfprintf(stderr, "H5Screate_simple 3D dataspace failed\n");
            TEST_ERROR;
        }
    }
    else {
        if ((filespace = H5Screate_simple(RANK2, s->chunk_dims,
                                          s->expand_2d ? two_dee_max_dims : s->one_dee_max_dims)) < 0) {
            HDfprintf(stderr, "H5Screate_simple 2D dataspace failed\n");
            TEST_ERROR;
        }
    }

    if ((dset_id = H5Dcreate2(s->file[0], dname, s->filetype, filespace, H5P_DEFAULT, dcpl, s->dapl)) < 0) {
        HDfprintf(stderr, "H5Dcreate2 failed\n");
        TEST_ERROR;
    }

    if (H5Sclose(filespace) < 0) {
        HDfprintf(stderr, "H5Sclose failed\n");
        TEST_ERROR;
    }

    if (H5Pclose(dcpl) < 0) {
        HDfprintf(stderr, "H5Pclose failed\n");
        TEST_ERROR;
    }

    s->dataset[which] = dset_id;

    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(dset_id);
        H5Pclose(dcpl);
        H5Sclose(filespace);
    }
    H5E_END_TRY;

    return false;
}

static bool
close_extensible_dset(state_t *s, unsigned int which)
{
    char  dname[sizeof("/dataset-9999999999")];
    hid_t dset_id = H5I_INVALID_HID;

    if (which >= s->ndatasets) {
        HDfprintf(stderr, "index is out of range\n");
        TEST_ERROR;
    }

    esnprintf(dname, sizeof(dname), "/dataset-%d", which);

    dset_id = s->dataset[which];

    if (H5Dclose(dset_id) < 0) {
        HDfprintf(stderr, "H5Dclose failed\n");
        TEST_ERROR;
    }

    s->dataset[which] = badhid;

    if (s->vds != vds_off && s->writer) {
        sources_t *const srcs = &s->sources[which];

        if (H5Dclose(srcs->ul) < 0 || H5Dclose(srcs->ur) < 0 || H5Dclose(srcs->bl) < 0 ||
            H5Dclose(srcs->br) < 0) {
            HDfprintf(stderr, "H5Dclose failed\n");
            TEST_ERROR;
        }
    }

    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(dset_id);
    }
    H5E_END_TRY;

    return false;
}

static bool
open_extensible_dset(state_t *s)
{
    hsize_t      dims2[RANK2], maxdims2[RANK2];
    hsize_t      dims3[RANK3], maxdims3[RANK3];
    char         dname[sizeof("/dataset-9999999999")];
    hid_t        dset_id, filespace, dtype;
    int          rank;
    unsigned int which, i;

    for (which = 0; which < s->ndatasets; which++) {
        esnprintf(dname, sizeof(dname), "/dataset-%d", which);

        /* Tries to open the dataset repeatedly until successful.  After trying
         * NUM_ATTEMPTS times without success, report it as a failure
         */
        for (i = 0; i < NUM_ATTEMPTS; i++) {
            H5E_BEGIN_TRY
            {
                dset_id = H5Dopen2(s->file[0], dname, s->dapl);
            }
            H5E_END_TRY;

            if (dset_id >= 0)
                break;
            else
                decisleep(1);
        }

        if (i == NUM_ATTEMPTS) {
            HDfprintf(stderr, "chunk verification reached the maximal number of attempts\n");
            TEST_ERROR;
        }

        if ((dtype = H5Dget_type(dset_id)) < 0) {
            HDfprintf(stderr, "H5Dget_type failed\n");
            TEST_ERROR;
        }

        if (H5Tequal(dtype, s->filetype) <= 0) {
            HDfprintf(stderr, "Unexpected data type\n");
            TEST_ERROR;
        }

        if ((filespace = H5Dget_space(dset_id)) < 0) {
            HDfprintf(stderr, "H5Dget_space failed\n");
            TEST_ERROR;
        }

        if ((rank = H5Sget_simple_extent_ndims(filespace)) < 0) {
            HDfprintf(stderr, "H5Sget_simple_extent_ndims failed\n");
            TEST_ERROR;
        }

        if ((s->test_3d && rank != RANK3) || (!s->test_3d && rank != RANK2)) {
            HDfprintf(stderr, "Unexpected data rank: %d\n", rank);
            TEST_ERROR;
        }

        if (s->test_3d) {
            if (H5Sget_simple_extent_dims(filespace, dims3, maxdims3) < 0) {
                HDfprintf(stderr, "H5Sget_simple_extent_dims failed\n");
                TEST_ERROR;
            }
        }
        else {
            if (H5Sget_simple_extent_dims(filespace, dims2, maxdims2) < 0) {
                HDfprintf(stderr, "H5Sget_simple_extent_dims failed\n");
                TEST_ERROR;
            }
        }

        if (H5Sclose(filespace) < 0) {
            HDfprintf(stderr, "H5Sclose failed\n");
            TEST_ERROR;
        }

        if (H5Tclose(dtype) < 0) {
            HDfprintf(stderr, "H5Tclose failed\n");
            TEST_ERROR;
        }

        if (s->test_3d) {
            if (maxdims3[0] != three_dee_max_dims[0] || maxdims3[1] != three_dee_max_dims[1] ||
                maxdims3[2] != three_dee_max_dims[2]) {
                HDfprintf(stderr,
                          "Unexpected maximum dimensions %" PRIuHSIZE " x %" PRIuHSIZE " x %" PRIuHSIZE,
                          maxdims3[0], maxdims3[1], maxdims3[2]);
                TEST_ERROR;
            }
        }
        else {
            if (s->expand_2d) {
                if (maxdims2[0] != two_dee_max_dims[0] || maxdims2[1] != two_dee_max_dims[1] ||
                    maxdims2[0] != maxdims2[1]) {
                    HDfprintf(stderr, "Unexpected maximum dimensions %" PRIuHSIZE " x %" PRIuHSIZE,
                              maxdims2[0], maxdims2[1]);
                    TEST_ERROR;
                }
            }
            else if (maxdims2[0] != s->one_dee_max_dims[0] || maxdims2[1] != s->one_dee_max_dims[1] ||
                     dims2[0] != s->chunk_dims[0]) {
                HDfprintf(stderr,
                          "Unexpected maximum dimensions %" PRIuHSIZE " x %" PRIuHSIZE
                          " or columns %" PRIuHSIZE,
                          maxdims2[0], maxdims2[1], dims2[1]);
            }
        }

        s->dataset[which] = dset_id;
    }

    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(dset_id);
        H5Tclose(dtype);
        H5Sclose(filespace);
    }
    H5E_END_TRY;

    return false;
}

static bool
create_dsets(state_t s)
{
    struct timespec start_time, end_time;
    unsigned int    which;

    /* For checking the time spent in dataset creation.  It's for running the writer alone */
    if (s.do_perf) {
        if (HDclock_gettime(CLOCK_MONOTONIC, &start_time) == -1) {
            HDfprintf(stderr, "HDclock_gettime failed");
            TEST_ERROR;
        }
    }

    /* Create NDATASETS datasets as the reader is doing verification.  So no communication with
     * the reader during the creation of datasets.
     */
    for (which = 0; which < s.ndatasets; which++)
        if (!create_extensible_dset(&s, which)) {
            HDfprintf(stderr, "create_extensible_dset failed: number %u\n", which);
            TEST_ERROR;
        }

    /* For checking the time spent in dataset creation.  It's for running the writer alone */
    if (s.do_perf) {
        if (HDclock_gettime(CLOCK_MONOTONIC, &end_time) == -1) {
            HDfprintf(stderr, "HDclock_gettime failed");
            TEST_ERROR;
        }

        HDfprintf(stdout, "Dataset creation time (for running the writer alone) = %lf seconds\n",
                  TIME_PASSED(start_time, end_time));
    }

    return true;

error:
    return false;
}

static uint32_t
matget(const mat_t *mat, unsigned k, unsigned i, unsigned j)
{
    return mat->elt[k * mat->rows * mat->cols + i * mat->cols + j];
}

static bool
matset(mat_t *mat, unsigned k, unsigned i, unsigned j, uint32_t v)
{
    if (k >= mat->depth || i >= mat->rows || j >= mat->cols) {
        HDfprintf(stderr, "index out of boundary\n");
        TEST_ERROR;
    }

    mat->elt[k * mat->rows * mat->cols + i * mat->cols + j] = v;

    return true;

error:
    return false;
}

static mat_t *
newmat(state_t s)
{
    mat_t *mat;

    /*
     * If partial chunk is enabled, the chunk size along the growing dimension
     * is replaced with the partial size
     */
    if (s.test_3d) {
        if (s.part_chunk) {
            mat        = HDmalloc(sizeof(*mat) + (s.part_chunk * s.rows * s.cols - 1) * sizeof(mat->elt[0]));
            mat->depth = s.part_chunk;
        }
        else {
            mat        = HDmalloc(sizeof(*mat) + (s.depth * s.rows * s.cols - 1) * sizeof(mat->elt[0]));
            mat->depth = s.depth;
        }

        mat->rows = s.rows;
        mat->cols = s.cols;
    }
    else {
        if (s.part_chunk && !s.expand_2d) {
            mat        = HDmalloc(sizeof(*mat) + (s.rows * s.part_chunk - 1) * sizeof(mat->elt[0]));
            mat->depth = 1;
            mat->rows  = s.rows;
            mat->cols  = s.part_chunk;
        }
        else {
            mat        = HDmalloc(sizeof(*mat) + (s.rows * s.cols - 1) * sizeof(mat->elt[0]));
            mat->depth = 1;
            mat->rows  = s.rows;
            mat->cols  = s.cols;
        }
    }

    if (mat == NULL) {
        HDfprintf(stderr, "HDmalloc failed\n");
        TEST_ERROR;
    }

    return mat;

error:
    return NULL;
}

/* Write or verify the dataset test pattern in the matrix `mat`.
 * `mat` is a "subview" of the `which`th dataset with origin
 * `(base.row, base.col)`.
 *
 * If `do_set` is true, write the pattern; otherwise, verify.
 *
 * For 2D datasets, the basic test pattern consists of increasing
 * integers written in nested corners of the dataset
 * starting at element (0, 0):
 *
 *  0
 *
 *  0  1
 *  3  2
 *
 *  0  1  4
 *  3  2  5
 *  8  7  6
 *
 *  0  1  4  9
 *  3  2  5 10
 *  8  7  6 11
 * 15 14 13 12
 *
 * In an actual pattern, the dataset number, `which`, is added to each integer.
 *
 * For 3D datasets, the increment of chunks is along the first dimension.
 */
static bool
set_or_verify_matrix(mat_t *mat, unsigned int which, base_t base, bool do_set)
{
    unsigned depth, row, col;
    bool     ret = true;

    /* For 2D datasets, `depth` is one */
    for (depth = 0; depth < mat->depth; depth++) {
        for (row = 0; row < mat->rows; row++) {
            for (col = 0; col < mat->cols; col++) {
                uint32_t v;
                hsize_t  k = base.depth + depth, i = base.row + row, j = base.col + col, u;

                if (j <= i)
                    u = k * 10 + (i + 1) * (i + 1) - 1 - j;
                else
                    u = k * 10 + j * j + i;

                v = (uint32_t)(u + which);

                if (do_set) {
                    if (!matset(mat, depth, row, col, v)) {
                        HDfprintf(stderr, "data initialization failed\n");
                        ret = false;
                        break;
                    }
                }
                else if (matget(mat, depth, row, col) != v) {
                    /* If the data doesn't match, simply return false and
                     * let the caller repeat this step
                     */
                    ret = false;
                    break;
                }
            }
        }
    }

    return ret;
}

static bool
init_matrix(mat_t *mat, unsigned int which, base_t base)
{
    return set_or_verify_matrix(mat, which, base, true);
}

static bool
verify_matrix(mat_t *mat, unsigned int which, base_t base)
{
    return set_or_verify_matrix(mat, which, base, false);
}

static unsigned int
calc_total_steps(state_t s)
{
    unsigned int total_steps = 0;

    /* Calculate the number of steps depending on if partial chunk is enabled.
     * e.g. the original number of steps is 10 and the size of the chunk along
     * the growing dimension is 6.  The number of elements for this dimension is
     * 60.  When the size of the partial chunk along the growing dimension is 5
     * (treated as the new chunk size), the number of steps becomes 12.
     */
    if (s.test_3d) {
        if (s.part_chunk)
            total_steps = s.nsteps * s.depth / s.part_chunk;
        else
            total_steps = s.nsteps;
    }
    else if (s.expand_2d) {
        total_steps = s.nsteps;
    }
    else {
        if (s.part_chunk)
            total_steps = s.nsteps * s.cols / s.part_chunk;
        else
            total_steps = s.nsteps;
    }

    return total_steps;
}

static bool
verify_chunk(state_t *s, hid_t filespace, mat_t *mat, unsigned which, base_t base)
{
    herr_t status;
    hid_t  dset_id;

    if (which >= s->ndatasets) {
        HDfprintf(stderr, "the dataset order is bigger than the number of datasets");
        TEST_ERROR;
    }

    dset_id = s->dataset[which];

    if (s->test_3d) {
        hsize_t offset3[RANK3] = {base.depth, base.row, base.col};
        hsize_t count3[RANK3]  = {s->depth, s->chunk_dims[0], s->chunk_dims[1]};

        if (s->part_chunk)
            count3[0] = s->part_chunk;

        if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset3, NULL, count3, NULL) < 0) {
            HDfprintf(stderr, "H5Sselect_hyperslab failed\n");
            TEST_ERROR;
        }
    }
    else {
        hsize_t offset2[RANK2] = {base.row, base.col};
        hsize_t count2[RANK2];

        if (s->expand_2d) {
            count2[0] = s->chunk_dims[0];
            count2[1] = s->chunk_dims[1];
        }
        else {
            count2[0] = s->chunk_dims[0];

            if (s->part_chunk)
                count2[1] = s->part_chunk;
            else
                count2[1] = s->chunk_dims[1];
        }

        if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset2, NULL, count2, NULL) < 0) {
            HDfprintf(stderr, "H5Sselect_hyperslab failed\n");
            TEST_ERROR;
        }
    }

    /* A failure to read the data may indicate the data isn't ready yet.  Instead of displaying the error
     * stack, simply return false and let the caller repeat this step.
     */
    H5E_BEGIN_TRY
    {
        status = H5Dread(dset_id, H5T_NATIVE_UINT32, s->memspace, filespace, H5P_DEFAULT, mat->elt);
    }
    H5E_END_TRY;

    if (status < 0)
        TEST_ERROR;

    return verify_matrix(mat, which, base);

error:
    return false;
}

/* Try to verify a chunk NUM_ATTEMPTS times until the data is correct */
static bool
repeat_verify_chunk(state_t *s, hid_t filespace, mat_t *mat, unsigned which, base_t base)
{
    hid_t    dset_id = s->dataset[which];
    unsigned i;

    /* If the chunk data isn't good after reading it NUM_ATTEMPTS times, report it as a failure */
    for (i = 0; i < NUM_ATTEMPTS; i++) {
        if (verify_chunk(s, filespace, mat, which, base))
            break;
        else {
            decisleep(1);

            /* Refresh the dataset and try it again */
            if (H5Drefresh(dset_id) < 0) {
                HDfprintf(stderr, "H5Drefresh failed\n");
                TEST_ERROR;
            }
        }
    }

    if (i == NUM_ATTEMPTS) {
        HDfprintf(stderr, "chunk verification reached the maximal number of attempts\n");
        TEST_ERROR;
    }

    return true;

error:
    return false;
}

static bool
init_and_write_chunk(state_t *s, hid_t filespace, mat_t *mat, unsigned which, base_t base)
{
    hid_t dset_id;

    dset_id = s->dataset[which];

    if (!init_matrix(mat, which, base)) {
        HDfprintf(stderr, "data initialization failed\n");
        TEST_ERROR;
    }

    if (s->test_3d) {
        hsize_t offset3[RANK3] = {base.depth, base.row, base.col};
        hsize_t count3[RANK3]  = {s->depth, s->chunk_dims[0], s->chunk_dims[1]};

        /* Handling partial chunk */
        if (s->part_chunk)
            count3[0] = s->part_chunk;

        /* The chunk dimensions are L x M x N.  It grows along the first dimension */
        if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset3, NULL, count3, NULL) < 0) {
            HDfprintf(stderr, "H5Sselect_hyperslab for 2D dataset failed\n");
            TEST_ERROR;
        }
    }
    else {
        hsize_t offset2[RANK2] = {base.row, base.col};
        hsize_t count2[RANK2];

        if (s->expand_2d) {
            count2[0] = s->chunk_dims[0];
            count2[1] = s->chunk_dims[1];
        }
        else {
            count2[0] = s->chunk_dims[0];

            /* Handling partial chunk */
            if (s->part_chunk)
                count2[1] = s->part_chunk;
            else
                count2[1] = s->chunk_dims[1];
        }

        if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset2, NULL, count2, NULL) < 0) {
            HDfprintf(stderr, "H5Sselect_hyperslab for 2D dataset failed\n");
            TEST_ERROR;
        }
    }

    if (H5Dwrite(dset_id, H5T_NATIVE_UINT32, s->memspace, filespace, H5P_DEFAULT, mat->elt) < 0) {
        HDfprintf(stderr, "H5Dwrite failed\n");
        TEST_ERROR;
    }

    return true;

error:
    return false;
}

static bool
verify_dset_attribute(hid_t dset_id, unsigned int which, unsigned int step)
{
    unsigned int read_step;
    hid_t        aid;
    char         name[sizeof("attr-9999999999")];

    esnprintf(name, sizeof(name), "attr-%u", step);

    dbgf(1, "verifying attribute %s on dataset %u equals %u\n", name, which, step);

    if ((aid = H5Aopen(dset_id, name, H5P_DEFAULT)) < 0) {
        HDfprintf(stderr, "H5Aopen failed\n");
        TEST_ERROR;
    }

    if (H5Aread(aid, H5T_NATIVE_UINT, &read_step) < 0) {
        HDfprintf(stderr, "H5Aread failed\n");
        TEST_ERROR;
    }

    if (H5Aclose(aid) < 0) {
        HDfprintf(stderr, "H5Aclose failed\n");
        TEST_ERROR;
    }

    if (read_step != step) {
        HDfprintf(stderr, "expected %u read %u\n", step, read_step);
        TEST_ERROR;
    }

    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Aclose(aid);
    }
    H5E_END_TRY;

    return false;
}

static bool
verify_extensible_dset(state_t *s, unsigned int which, mat_t *mat, unsigned finished_step, unsigned last_step)
{
    hid_t        dset_id = H5I_INVALID_HID, filespace = H5I_INVALID_HID;
    hsize_t      size2[RANK2], size3[RANK3];
    base_t       base, last;
    unsigned int nchunks, step, ofs;
    int          i;

    if (which >= s->ndatasets) {
        HDfprintf(stderr, "the dataset order is bigger than the number of datasets");
        TEST_ERROR;
    }

    dset_id = s->dataset[which];

    /* Attempt to check the availablity of the chunks for a number times
     * (NUM_ATTEMPTS) before reporting it as a failure */
    for (i = 0; i < NUM_ATTEMPTS; i++) {
        if (H5Drefresh(dset_id) < 0) {
            HDfprintf(stderr, "H5Drefresh failed\n");
            TEST_ERROR;
        }

        if ((filespace = H5Dget_space(dset_id)) < 0) {
            HDfprintf(stderr, "H5Dget_space failed\n");
            TEST_ERROR;
        }

        if (s->test_3d) {
            if (H5Sget_simple_extent_dims(filespace, size3, NULL) < 0) {
                HDfprintf(stderr, "H5Sget_simple_extent_dims failed\n");
                TEST_ERROR;
            }

            /* Handling partial chunks */
            if (s->part_chunk)
                nchunks = (unsigned)size3[0] / s->part_chunk;
            else
                nchunks = (unsigned)size3[0] / s->depth;
        }
        else {
            if (H5Sget_simple_extent_dims(filespace, size2, NULL) < 0) {
                HDfprintf(stderr, "H5Sget_simple_extent_dims failed\n");
                TEST_ERROR;
            }

            /* Handling partial chunks */
            if (s->part_chunk)
                nchunks = (unsigned)(size2[1] / s->part_chunk);
            else
                nchunks = (unsigned)(size2[1] / s->chunk_dims[1]);
        }

        /* Make sure the chunks show up on the reader side.  Otherwise sleep a while and try again */
        if (nchunks >= last_step)
            break;
        else
            decisleep(1);
    }

    if (i == NUM_ATTEMPTS) {
        HDfprintf(stderr, "chunk verification reached the maximal number of attempts");
        TEST_ERROR;
    }

    for (step = finished_step; step < last_step; step++) {
        dbgf(1, "%s: which %u step %u\n", __func__, which, step);

        if (s->skip_chunk && step % s->skip_chunk == 0)
            continue;

        /* Read data that randomly crosses over chunks. But it should not happen to
         * the last chunk being written
         */
        if (s->cross_chunk_read) {
            if (step == last_step - 1)
                ofs = 0;
            else
                ofs = step % 2;
        }
        else
            ofs = 0;

        if (s->test_3d) {
            if (s->part_chunk) {
                last.depth = s->part_chunk * step + ofs;
            }
            else {
                last.depth = s->depth * step + ofs;
            }

            last.row = 0;
            last.col = 0;
        }
        else {
            last.depth = 0;

            if (s->expand_2d) {
                last.row = s->chunk_dims[0] * step + ofs;
                last.col = s->chunk_dims[1] * step + ofs;
            }
            else {
                last.row = 0;

                if (s->part_chunk) {
                    last.col = s->part_chunk * step + ofs;
                }
                else {
                    last.col = s->chunk_dims[1] * step + ofs;
                }
            }
        }

        if (s->test_3d)
            dbgf(1, "last row %" PRIuHSIZE " col %" PRIuHSIZE " depth %" PRIuHSIZE "\n", last.row, last.col,
                 last.depth);
        else
            dbgf(1, "last row %" PRIuHSIZE " col %" PRIuHSIZE "\n", last.row, last.col);

        if (s->test_3d || !s->expand_2d) {
            if (!repeat_verify_chunk(s, filespace, mat, which, last)) {
                HDfprintf(stderr, "chunk verification failed\n");
                TEST_ERROR;
            }
        }
        else {
            /* Down the right side, intersecting the bottom row. */
            base.col   = last.col;
            base.depth = 0;
            for (base.row = 0; base.row <= last.row; base.row += s->chunk_dims[0]) {
                if (!repeat_verify_chunk(s, filespace, mat, which, base)) {
                    HDfprintf(stderr, "chunk verification failed\n");
                    TEST_ERROR;
                }
            }

            /* Across the bottom, stopping before the last column to
             * avoid re-reading the bottom-right chunk.
             */
            base.row = last.row;
            for (base.col = 0; base.col < last.col; base.col += s->chunk_dims[1]) {
                if (!repeat_verify_chunk(s, filespace, mat, which, base)) {
                    HDfprintf(stderr, "chunk verification failed\n");
                    TEST_ERROR;
                }
            }
        }

        if (s->asteps != 0 && step % s->asteps == 0) {
            if (!verify_dset_attribute(dset_id, which, step)) {
                HDfprintf(stderr, "verify_dset_attribute failed\n");
                TEST_ERROR;
            }
        }
    }

    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(filespace);
    }
    H5E_END_TRY;

    return false;
}

static bool
verify_dsets(state_t s, np_state_t *np, mat_t *mat)
{
    unsigned        finished_step = 0;
    unsigned        which;
    unsigned        counter     = 0;
    unsigned        total_steps = 0;
    double          passed_time = 0.0, total_time = 0.0, min_time = 1000000.0, max_time = 0.0;
    exchange_info_t last;
    struct timespec end_time;

    total_steps = calc_total_steps(s);

    do {
        /* Receive the notice of the writer finishing creation,
         * including the number of chunks finished and the timestamp
         */
        if (s.use_named_pipe && HDread(np->fd_writer_to_reader, &last, sizeof(last)) < 0) {
            HDfprintf(stderr, "HDread failed\n");
            TEST_ERROR;
        }

        for (which = 0; which < s.ndatasets; which++) {
            /* Verify the chunks starting from the finished one in last round
             * to the ones written in this round
             */
            if (!verify_extensible_dset(&s, which, mat, finished_step, last.step)) {
                HDfprintf(stderr, "verify_extensible_dset failed\n");
                TEST_ERROR;
            }

            /* Reset the finished one */
            finished_step = last.step;
        }

        /* Make sure the chunk verification doesn't take longer than the expected time.
         * This time period is from the writer finishing chunks to the reader finishing
         * the validation of the chunks */
        if (s.use_named_pipe && below_speed_limit(&(last.time), &(s.ival))) {
            AT();
            HDfprintf(stderr, "Warning: verify_extensible_dset took too long to finish\n");
        }

        /* For checking the time lapse between the writer's finishing writing a batch of chunks
         * within a tick and the reader's finishing verifying those chunks
         */
        if (s.use_named_pipe && s.do_perf) {
            if (HDclock_gettime(CLOCK_MONOTONIC, &end_time) == -1) {
                HDfprintf(stderr, "HDclock_gettime failed");
                TEST_ERROR;
            }

            counter++;
            passed_time = TIME_PASSED(last.time, end_time);

            total_time += passed_time;

            if (passed_time > max_time)
                max_time = passed_time;

            if (passed_time < min_time)
                min_time = passed_time;
        }
    } while (finished_step < total_steps);

    /* Print out the performance information */
    if (s.use_named_pipe && s.do_perf && counter)
        HDfprintf(stdout, "Dataset verification: mean time = %lf, max time = %lf, min time = %lf\n",
                  total_time / (double)counter, max_time, min_time);

    return true;

error:
    return false;
}

static bool
add_dset_attribute(const state_t *s, hid_t ds, hid_t sid, unsigned int which, unsigned int step)
{
    hid_t aid;
    char  name[sizeof("attr-9999999999")];

    esnprintf(name, sizeof(name), "attr-%u", step);

    dbgf(1, "setting attribute %s on dataset %u to %u\n", name, which, step);

    if ((aid = H5Acreate2(ds, name, s->filetype, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        HDfprintf(stderr, "H5Acreate2 failed\n");
        TEST_ERROR;
    }

    if (H5Awrite(aid, H5T_NATIVE_UINT, &step) < 0) {
        HDfprintf(stderr, "H5Awrite failed\n");
        TEST_ERROR;
    }

    if (H5Aclose(aid) < 0) {
        HDfprintf(stderr, "H5Aclose failed\n");
        TEST_ERROR;
    }

    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Aclose(aid);
    }
    H5E_END_TRY;

    return false;
}

static bool
write_extensible_dset(state_t *s, unsigned int which, unsigned int step, mat_t *mat)
{
    hid_t   dset_id = H5I_INVALID_HID, filespace = H5I_INVALID_HID;
    hsize_t size2[RANK2], size3[RANK3];
    base_t  base, last;
    char    dname[sizeof("/dataset-9999999999")];

    esnprintf(dname, sizeof(dname), "/dataset-%d", which);

    dbgf(1, "%s: which %u step %u\n", __func__, which, step);

    if (which >= s->ndatasets) {
        HDfprintf(stderr, "index is out of range\n");
        TEST_ERROR;
    }

    dset_id = s->dataset[which];

    if (s->asteps != 0 && step % s->asteps == 0) {
        if (!add_dset_attribute(s, dset_id, s->one_by_one_sid, which, step)) {
            HDfprintf(stderr, "add_dset_attribute failed\n");
            TEST_ERROR;
        }
    }

    /* Handling both over extension of the datasets and partial chunks.  Datasets
     * can be extended multiple chunks instead of one chunk at a time.
     * e.g. if the over extension is set to 10 chunks, the datasets are extended
     * 10 chunks along the growing dimension after every 10 chunks are written.
     */
    if (s->test_3d) {
        if (s->part_chunk) {
            size3[0]   = s->over_extend * s->part_chunk * (1 + step / s->over_extend);
            last.depth = s->part_chunk * step;
        }
        else {
            size3[0]   = s->over_extend * s->depth * (1 + step / s->over_extend);
            last.depth = s->depth * step;
        }

        size3[1] = s->chunk_dims[0];
        size3[2] = s->chunk_dims[1];

        last.row = 0;
        last.col = 0;
    }
    else {
        if (s->expand_2d) {
            size2[0] = s->over_extend * s->chunk_dims[0] * (1 + step / s->over_extend);
            size2[1] = s->over_extend * s->chunk_dims[1] * (1 + step / s->over_extend);

            last.row = s->chunk_dims[0] * step;
            last.col = s->chunk_dims[1] * step;
        }
        else {
            size2[0] = s->chunk_dims[0];
            last.row = 0;

            if (s->part_chunk) {
                size2[1] = s->over_extend * s->part_chunk * (1 + step / s->over_extend);
                last.col = s->part_chunk * step;
            }
            else {
                size2[1] = s->over_extend * s->chunk_dims[1] * (1 + step / s->over_extend);
                last.col = s->chunk_dims[1] * step;
            }
        }
        last.depth = 0;
    }

    if (s->test_3d)
        dbgf(1, "new size %" PRIuHSIZE ", %" PRIuHSIZE ", %" PRIuHSIZE "\n", size3[0], size3[1], size3[2]);
    else
        dbgf(1, "new size %" PRIuHSIZE ", %" PRIuHSIZE "\n", size2[0], size2[1]);

    if (s->vds != vds_off) {
        const hsize_t    half_size[RANK2] = {size2[0] / 2, size2[1] / 2};
        sources_t *const srcs             = &s->sources[which];

        if (H5Dset_extent(srcs->ul, half_size) < 0) {
            HDfprintf(stderr, "H5Dset_extent failed\n");
            TEST_ERROR;
        }

        if (H5Dset_extent(srcs->ur, half_size) < 0) {
            HDfprintf(stderr, "H5Dset_extent failed\n");
            TEST_ERROR;
        }

        if (H5Dset_extent(srcs->bl, half_size) < 0) {
            HDfprintf(stderr, "H5Dset_extent failed\n");
            TEST_ERROR;
        }

        if (H5Dset_extent(srcs->br, half_size) < 0) {
            HDfprintf(stderr, "H5Dset_extent failed\n");
            TEST_ERROR;
        }
    }
    else {
        /* Handling over extension.  Making sure the dataset size doesn't exceed the fixed maximal size */
        if (step % s->over_extend == 0) {
            if (s->test_3d) {
                if (size3[0] <= three_dee_max_dims[0] && H5Dset_extent(dset_id, size3) < 0) {
                    HDfprintf(stderr, "H5Dset_extent for 3D dataset failed\n");
                    TEST_ERROR;
                }
            }
            else {
                if ((s->expand_2d && size2[0] <= two_dee_max_dims[0] && size2[0] <= two_dee_max_dims[0]) ||
                    (!s->expand_2d && size2[1] <= two_dee_max_dims[1])) {
                    if (H5Dset_extent(dset_id, size2) < 0) {
                        HDfprintf(stderr, "H5Dset_extent for 2D dataset failed\n");
                        TEST_ERROR;
                    }
                }
            }
        }
    }

    if ((filespace = H5Dget_space(dset_id)) < 0) {
        HDfprintf(stderr, "H5Dget_space failed\n");
        TEST_ERROR;
    }

    if (s->test_3d || !s->expand_2d) {
        if (!init_and_write_chunk(s, filespace, mat, which, last)) {
            HDfprintf(stderr, "init_and_write_chunk failed\n");
            TEST_ERROR;
        }
    }
    else if (s->expand_2d) {
        base.col   = last.col;
        base.depth = 0;
        for (base.row = 0; base.row <= last.row; base.row += s->chunk_dims[0]) {
            dbgf(1, "writing chunk %" PRIuHSIZE ", %" PRIuHSIZE "\n", base.row, base.col);
            if (!init_and_write_chunk(s, filespace, mat, which, base)) {
                HDfprintf(stderr, "init_and_write_chunk failed\n");
                TEST_ERROR;
            }
        }

        base.row = last.row;
        for (base.col = 0; base.col < last.col; base.col += s->chunk_dims[1]) {
            dbgf(1, "writing chunk %" PRIuHSIZE ", %" PRIuHSIZE "\n", base.row, base.col);
            if (!init_and_write_chunk(s, filespace, mat, which, base)) {
                HDfprintf(stderr, "init_and_write_chunk failed\n");
                TEST_ERROR;
            }
        }
    }

    if (H5Sclose(filespace) < 0) {
        HDfprintf(stderr, "H5Sclose failed\n");
        TEST_ERROR;
    }

    return true;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(filespace);
    }
    H5E_END_TRY;

    return false;
}

static bool
write_dsets(state_t s, np_state_t *np, mat_t *mat)
{
    unsigned           last_step, step, total_steps, which;
    unsigned long long old_tick_num;
    H5F_t *            f = NULL;
    struct timespec    start_time, end_time;

    if (NULL == (f = (H5F_t *)H5VL_object(s.file[0]))) {
        HDfprintf(stderr, "H5VL_object failed\n");
        TEST_ERROR;
    }

    /* For checking the time spent in writing data.  It's for running the writer alone */
    if (s.do_perf) {
        if (HDclock_gettime(CLOCK_MONOTONIC, &start_time) == -1) {
            HDfprintf(stderr, "HDclock_gettime failed");
            TEST_ERROR;
        }
    }

    old_tick_num = f->shared->tick_num;

    /* Write as many as chunks within the same tick number before notifying
     * the reader to verify them.  Take account of partial chunk write
     * here by multiplying the dividing factor for partial chunk. Treat each
     * partial chunk as if it's a chunk.
     */
    total_steps = calc_total_steps(s);

    for (step = 0; step < total_steps; step++) {
        /* Write as many as chunks before the tick number changes */
        if (f->shared->tick_num == old_tick_num) {
            if (!s.skip_chunk || (s.skip_chunk && step % s.skip_chunk != 0)) {
                for (which = 0; which < s.ndatasets; which++) {
                    dbgf(2, "step %d which %d\n", step, which);
                    if (!write_extensible_dset(&s, which, step, mat)) {
                        HDfprintf(stderr, "write_extensible_dset failed\n");
                        TEST_ERROR;
                    }
                }
            }
        }

        /* Notify the reader to start verification by
         * sending the timestamp and the number of chunks written
         */
        if (f->shared->tick_num > old_tick_num || step == (total_steps - 1)) {
            last_step = step + 1;
            if (s.use_named_pipe && notify_reader(np, last_step) < 0) {
                HDfprintf(stderr, "notify_reader failed\n");
                TEST_ERROR;
            }

            old_tick_num = f->shared->tick_num;
        }
    }

    /* For checking the time spent in writing data.  It's for running the writer alone */
    if (s.do_perf) {
        double throughput;
        double time_passed;

        if (HDclock_gettime(CLOCK_MONOTONIC, &end_time) == -1) {
            HDfprintf(stderr, "HDclock_gettime failed");
            TEST_ERROR;
        }

        time_passed = TIME_PASSED(start_time, end_time);

        /* Calculate the write speed */
        if (s.test_3d)
            throughput =
                ((double)(sizeof(unsigned int) * s.depth * s.rows * s.cols * s.nsteps * s.ndatasets)) /
                time_passed;
        else
            throughput =
                ((double)(sizeof(unsigned int) * s.rows * s.cols * s.nsteps * s.ndatasets)) / time_passed;

        /* Print out the performance information */
        HDfprintf(stdout,
                  "Dataset write time (for running the writer alone) = %lf seconds, write speed = %.2lf "
                  "bytes/second\n",
                  time_passed, throughput);
    }

    return true;

error:
    return false;
}

int
main(int argc, char **argv)
{
    mat_t *                 mat;
    hid_t                   fcpl = H5I_INVALID_HID;
    unsigned                which;
    state_t                 s;
    np_state_t              np;
    size_t                  i;
    H5F_generate_md_ck_cb_t cb_info; /* Callback */

    if (!state_init(&s, argc, argv)) {
        HDfprintf(stderr, "state_init failed\n");
        TEST_ERROR;
    }

    if ((mat = newmat(s)) == NULL) {
        HDfprintf(stderr, "could not allocate matrix\n");
        TEST_ERROR;
    }

    /* Set fs_strategy (file space strategy) and fs_page_size (file space page size) */
    if ((fcpl = vfd_swmr_create_fcpl(H5F_FSPACE_STRATEGY_PAGE, s.fsp_size)) < 0) {
        HDfprintf(stderr, "vfd_swmr_create_fcpl failed\n");
        TEST_ERROR;
    }

    for (i = 0; i < NELMTS(s.file); i++) {
        hid_t                 fapl;
        H5F_vfd_swmr_config_t config;
        H5AC_cache_config_t   mdc_config;

        if (s.vds != vds_multi && i > 0) {
            s.file[i] = s.file[0];
            continue;
        }

        /* config, tick_len, max_lag, writer, maintain_metadata_file, generate_updater_files,
         * flush_raw_data, md_pages_reserved, md_file_path, updater_file_path */
#ifdef H5_HAVE_AUX_PROCESS
        /* If using the auxiliary process, the writer creates the updater files.
         * The reader uses the metadata file generated by the auxiliary process. */
        if (s.writer) {
            init_vfd_swmr_config(&config, s.tick_len, s.max_lag, s.writer, FALSE, TRUE, s.flush_raw_data, 128,
                                 "./bigset-shadow-%zu", "bigset_updater", i);
        }
        else {
            init_vfd_swmr_config(&config, s.tick_len, s.max_lag, s.writer, TRUE, FALSE, s.flush_raw_data, 128,
                                 "./mdfile", NULL);
        }
#else
        init_vfd_swmr_config(&config, s.tick_len, s.max_lag, s.writer, TRUE, FALSE, s.flush_raw_data, 128,
                             "./bigset-shadow-%zu", NULL, i);
#endif

        /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
        if ((fapl = vfd_swmr_create_fapl(true, s.use_vfd_swmr, true, s.page_buf_size, &config)) < 0) {
            HDfprintf(stderr, "vfd_swmr_create_fapl failed");
            TEST_ERROR;
        }

        /* Set the initial size for the metadata cache between 1 and 32 in megabytes.
         * Zero means using the default value, which is no-op.
         */
        if (s.mdc_init_size) {
            mdc_config.version = H5AC__CURR_CACHE_CONFIG_VERSION;

            if (H5Pget_mdc_config(fapl, &mdc_config) < 0) {
                HDfprintf(stderr, "H5Pget_mdc_config failed");
                TEST_ERROR;
            }

            /* Convert the value to megabytes */
            mdc_config.set_initial_size = TRUE;
            mdc_config.initial_size     = s.mdc_init_size * 1024 * 1024;

            if (H5Pset_mdc_config(fapl, &mdc_config) < 0) {
                HDfprintf(stderr, "H5Pset_mdc_config failed");
                TEST_ERROR;
            }
        }

        /* This part is for debugging only */
#ifdef TMP
        {
            /* Set up callback to generate checksums for updater's metadata files */
            cb_info.func = md_ck_cb;

            /* Activate private property to generate checksums for updater's metadata file */
            H5Pset(fapl, H5F_ACS_GENERATE_MD_CK_CB_NAME, &cb_info);
        }
#endif

        s.file[i] = s.writer ? H5Fcreate(s.filename[i], H5F_ACC_TRUNC, fcpl, fapl)
                             : H5Fopen(s.filename[i], H5F_ACC_RDONLY, fapl);

        if (s.file[i] == badhid) {
            HDfprintf(stderr, s.writer ? "H5Fcreate failed" : "H5Fopen failed");
            TEST_ERROR;
        }

        if (H5Pclose(fapl) < 0) {
            HDfprintf(stderr, "H5Pclose failed\n");
            TEST_ERROR;
        }
    }

    /* Initiailze named pipes */
    if (s.use_named_pipe && !np_init(&np, s.writer)) {
        HDfprintf(stderr, "np_init() failed\n");
        TEST_ERROR;
    }

    if (s.writer) {
        /* Writer tells reader to start */
        np.notify = 1;
        if (s.use_named_pipe && HDwrite(np.fd_writer_to_reader, &np.notify, sizeof(int)) < 0) {
            HDfprintf(stderr, "HDwrite failed\n");
            TEST_ERROR;
        }

        /* Creates multiple datasets */
        if (!create_dsets(s)) {
            HDfprintf(stderr, "create_dsets failed");
            TEST_ERROR;
        }

        /* Call H5Fvfd_swmr_end_tick to end the tick.  No communication with the reader in this step */
        if (s.use_vfd_swmr && s.use_named_pipe) {
            unsigned long j;

            if (s.vds != vds_multi) {
                if (H5Fvfd_swmr_end_tick(s.file[0]) < 0) {
                    HDfprintf(stderr, "H5Fvfd_swmr_end_tick failed\n");
                    TEST_ERROR;
                }
            }
            else {
                for (j = 0; j < NELMTS(s.file); j++)
                    if (H5Fvfd_swmr_end_tick(s.file[j]) < 0) {
                        HDfprintf(stderr, "H5Fvfd_swmr_end_tick failed\n");
                        TEST_ERROR;
                    }
            }
        }

        /* Notify the reader of finishing dataset creation by sending the timestamp
         * and wait for the reader to finish validation before proceeding */
        np.verify = 2;
        if (s.use_named_pipe && notify_and_wait_for_reader(&s, &np) < 0) {
            HDfprintf(stderr, "notify_and_wait_for_reader failed\n");
            TEST_ERROR;
        }

        /* Enable the Legacy SWMR writing mode if specified */
        if (s.use_legacy_swmr && H5Fstart_swmr_write(s.file[0]) < 0) {
            HDfprintf(stderr, "failed to start the Legacy SWMR writing mode\n");
            TEST_ERROR;
        }

        /* Start to write chunks.  The writer writes as many chunks as possible within a tick, then
         * notify the reader.  But it doesn't receive back the reader's notice. */
        if (!write_dsets(s, &np, mat)) {
            fprintf(stderr, "write_dsets failed");
            TEST_ERROR;
        }
    }
    else {
        /* Wait for the writer's notice before starting the validation of dataset creation */
        np.verify = 1;
        if (s.use_named_pipe && reader_verify(np, np.verify) < 0) {
            HDfprintf(stderr, "reader_verify failed\n");
            TEST_ERROR;
        }

        /* Open all the datasets as the writer is creating them.  No communication with
         * the writer during this step.
         */
        if (!open_extensible_dset(&s)) {
            HDfprintf(stderr, "open_extensible_dset failed\n");
            TEST_ERROR;
        }

        /* Receive the notice of the writer finishing dataset creation (timestamp)
         * Make sure the dataset creation doesn't take longer than the expected time.
         * This time period is from the writer finishing dataset creation to the reader finishing
         * the validation of dataset creation */
        np.notify = 2;
        if (s.use_named_pipe && reader_check_time_and_notify_writer(&np, s) < 0) {
            HDfprintf(stderr, "reader_check_time_and_notify_writer failed\n");
            TEST_ERROR;
        }

        /* Once the reader starts to verify the datasets, it doesn't notify the writer any info.
         * Both the reader and writer finish by themselves.
         */
        if (!verify_dsets(s, &np, mat)) {
            HDfprintf(stderr, "verify_dsets failed\n");
            TEST_ERROR;
        }
    }

    for (which = 0; which < s.ndatasets; which++)
        if (!close_extensible_dset(&s, which)) {
            HDfprintf(stderr, "close_extensible_dset failed\n");
            TEST_ERROR;
        }

    if (H5Pclose(fcpl) < 0) {
        HDfprintf(stderr, "H5Pclose failed\n");
        TEST_ERROR;
    }

    if (s.use_named_pipe && !np_close(&np, s.writer)) {
        HDfprintf(stderr, "np_close() failed\n");
        TEST_ERROR;
    }

    if (!state_destroy(&s)) {
        HDfprintf(stderr, "state_destroy failed\n");
        TEST_ERROR;
    }

    if (mat)
        HDfree(mat);

    return EXIT_SUCCESS;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fcpl);

        for (i = 0; i < NELMTS(s.file); i++)
            H5Fclose(s.file[i]);
    }
    H5E_END_TRY;

    if (s.use_named_pipe && np.fd_writer_to_reader >= 0)
        HDclose(np.fd_writer_to_reader);

    if (s.use_named_pipe && np.fd_reader_to_writer >= 0)
        HDclose(np.fd_reader_to_writer);

    if (s.use_named_pipe && !s.writer) {
        HDremove(np.fifo_writer_to_reader);
        HDremove(np.fifo_reader_to_writer);
    }

    if (mat)
        HDfree(mat);

    return EXIT_FAILURE;
}
#else /* H5_HAVE_WIN32_API */

int
main(void)
{
    HDfprintf(stderr, "Non-POSIX platform. Skipping.\n");
    return EXIT_SUCCESS;
} /* end main() */

#endif /* H5_HAVE_WIN32_API */
