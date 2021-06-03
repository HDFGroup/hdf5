/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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
 * The reader should be started with the same user-selectable parameters
 * as the writer: iterations, number of datasets, chunk width and
 * height, dimensions.
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
// #include "H5Iprivate.h"
#include "H5HGprivate.h"
#include "H5VLprivate.h"

#include "testhdf5.h"
#include "vfd_swmr_common.h"

#ifndef H5_HAVE_WIN32_API

#define MAX_READ_LEN_IN_SECONDS 2
#define TICK_LEN                4
#define MAX_LAG                 7
#define ROWS                  256
#define COLS                  512
#define RANK                    2
#define NUM_ATTEMPTS          100

typedef struct _base {
    hsize_t row, col;
} base_t;

typedef struct _mat {
    unsigned rows, cols;
    uint32_t elt[1];
} mat_t;

typedef struct _quadrant {
    hsize_t start[RANK];
    hsize_t stride[RANK];
    hsize_t block[RANK];
    hsize_t count[RANK];
    hid_t   space, src_space;
} quadrant_t;

typedef struct _sources {
    hid_t ul, ur, bl, br;
} sources_t;

#define MANY_FILES 4

typedef struct {
    hid_t *         dataset;
    sources_t *     sources;
    hid_t           file[MANY_FILES];
    hid_t           dapl, filetype, memspace, one_by_one_sid, quadrant_dcpl;
    unsigned        ndatasets;
    const char *    filename[MANY_FILES];
    char            progname[PATH_MAX];
    struct {
        quadrant_t ul, ur, bl, br, src;
    } quadrants;
    unsigned int cols, rows;
    unsigned int asteps;
    unsigned int nsteps;
    bool         two_dee;
    enum { vds_off, vds_single, vds_multi } vds;
    bool    use_vfd_swmr;
    bool    use_named_pipe;
    bool    writer;
    bool    fixed_array;
    hsize_t chunk_dims[RANK];
    hsize_t one_dee_max_dims[RANK];
    struct timespec ival;
} state_t;

/* Structure to hold info for named pipes */
typedef struct {
    const char *fifo_writer_to_reader;  /* Name of fifo for writer to reader */
    const char *fifo_reader_to_writer;  /* Name of fifo for reader to writer */
    int fd_writer_to_reader;            /* File ID for fifo from writer to reader */
    int fd_reader_to_writer;            /* File ID for fifo from reader to writer */
    int notify;                         /* Value to notify between writer and reader */
    int verify;                         /* Value to verify between writer and reader */
} np_state_t;

typedef struct {
    unsigned        step;
    struct timespec time;
} exchange_info_t;

/* Initializations for np_state_t */
#define NP_INITIALIZER (np_state_t) {		                        \
	  .fifo_writer_to_reader = "./fifo_bigset_writer_to_reader"     \
	, .fifo_reader_to_writer = "./fifo_bigset_reader_to_writer"     \
	, .fd_writer_to_reader = -1                                     \
	, .fd_reader_to_writer = -1                                     \
        , .notify = 0                                                   \
        , .verify = 0                                                   \
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
                     .rows             = ROWS,
                     .cols             = COLS,
                     .ndatasets        = 5,
                     .asteps           = 10,
                     .nsteps           = 100,
                     .filename         = {"", "", "", ""},
                     .two_dee          = false,
                     .vds              = vds_off,
                     .use_vfd_swmr     = true,
                     .use_named_pipe   = true,
                     .writer           = true,
                     .fixed_array      = false,
                     .one_dee_max_dims = {ROWS, H5S_UNLIMITED},
                     .chunk_dims       = {ROWS, COLS},
                     .ival             = (struct timespec){.tv_sec = MAX_READ_LEN_IN_SECONDS, .tv_nsec = 0}};
}

static bool state_init(state_t *, int, char **);

static const hid_t badhid = H5I_INVALID_HID;

static hsize_t two_dee_max_dims[RANK];

static uint32_t
matget(const mat_t *mat, unsigned i, unsigned j)
{
    return mat->elt[i * mat->cols + j];
}

static bool
matset(mat_t *mat, unsigned i, unsigned j, uint32_t v)
{
    if (i >= mat->rows || j >= mat->cols) {
        fprintf(stderr, "index out of boundary\n");
        TEST_ERROR;
    }

    mat->elt[i * mat->cols + j] = v;

    return true;

error:
    return false;
}

static mat_t *
newmat(unsigned rows, unsigned cols)
{
    mat_t *mat;

    mat = HDmalloc(sizeof(*mat) + (rows * cols - 1) * sizeof(mat->elt[0]));

    if (mat == NULL) {
        fprintf(stderr, "HDmalloc failed\n");
        TEST_ERROR;
    }

    mat->rows = rows;
    mat->cols = cols;

    return mat;

error:
    return NULL;
}

static void
usage(const char *progname)
{
    fprintf(stderr,
            "usage: %s [-F] [-M] [-S] [-V] [-W] [-a steps] [-b] [-c cols]\n"
            "    [-d dims]\n"
            "    [-l tick_num] [-n iterations] [-r rows] [-s datasets]\n"
            "    [-u milliseconds]\n"
            "\n"
            "-F:                   fixed maximal dimension for the chunked datasets\n"
            "-M:	               use virtual datasets and many source\n"
            "                      files\n"
            "-S:	               do not use VFD SWMR\n"
            "-V:	               use virtual datasets and a single\n"
            "                      source file\n"
            "-a steps:	       `steps` between adding attributes\n"
            "-b:	               write data in big-endian byte order\n"
            "-c cols:	       `cols` columns per chunk\n"
            "-d 1|one|2|two|both:  select dataset expansion in one or\n"
            "                      both dimensions\n"
            "-l tick_num:          expected maximal number of ticks from\n"
            "                      the writer's finishing creation to the reader's finishing validation\n"
            "-N:                   do not use named pipes\n"
            "-n iterations:        how many times to expand each dataset\n"
            "-r rows:	       `rows` rows per chunk\n"
            "-s datasets:          number of datasets to create\n"
            "\n",
            progname);
    exit(EXIT_FAILURE);
}

static bool
make_quadrant_dataspace(state_t *s, quadrant_t *q)
{
    if ((q->space = H5Screate_simple(NELMTS(s->chunk_dims), s->chunk_dims,
                                s->two_dee ? two_dee_max_dims : s->one_dee_max_dims)) < 0) {
        fprintf(stderr, "H5Screate_simple failed\n");
        TEST_ERROR;
    }

    if (H5Sselect_hyperslab(q->space, H5S_SELECT_SET, q->start, q->stride, q->count, q->block) < 0) {
        fprintf(stderr, "H5Sselect_hyperslab failed\n");
        TEST_ERROR;
    }

    return true;

error:
    H5E_BEGIN_TRY {
        H5Sclose(q->space);
    } H5E_END_TRY;

    return false;
}

static bool
state_init(state_t *s, int argc, char **argv)
{
    unsigned long     tmp;
    int               ch;
    unsigned          i;
    const hsize_t     dims = 1;
    char              *tfile = NULL;
    char *            end;
    quadrant_t *const ul = &s->quadrants.ul, *const ur = &s->quadrants.ur, *const bl = &s->quadrants.bl,
                      *const br = &s->quadrants.br, *const src = &s->quadrants.src;
    const char *personality;

    *s = state_initializer();

    if (H5_basename(argv[0], &tfile) < 0) {
        fprintf(stderr, "H5_basename failed\n");
        TEST_ERROR;
    }

    esnprintf(s->progname, sizeof(s->progname), "%s", tfile);

    if (tfile)
        HDfree(tfile);

    while ((ch = getopt(argc, argv, "FMNSVa:bc:d:l:n:qr:s:")) != -1) {
        switch (ch) {
            case 'F':
                /* The flag to indicate whether the maximal dimension of the chunked datasets is fixed or
                 * unlimited */
                s->fixed_array = true;
                break;
            case 'M':
                s->vds = vds_multi;
                break;
            case 'S':
                s->use_vfd_swmr = false;
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
                    s->two_dee = false;
                else if (strcmp(optarg, "2") == 0 || strcmp(optarg, "two") == 0 ||
                         strcmp(optarg, "both") == 0)
                    s->two_dee = true;
                else {
                    fprintf(stderr, "bad -d argument %s\n", optarg);
                    TEST_ERROR;
                }
                break;
            case 'a':
            case 'c':
            case 'l':
            case 'n':
            case 'r':
            case 's':
                errno = 0;
                tmp   = HDstrtoul(optarg, &end, 0);
                if (end == optarg || *end != '\0') {
                    fprintf(stderr, "couldn't parse -%c argument %s\n", ch, optarg);
                    TEST_ERROR;
                }
                else if (errno != 0) {
                    fprintf(stderr, "couldn't parse -%c argument %s\n", ch, optarg);
                    TEST_ERROR;
                }
                else if (tmp > UINT_MAX) {
                    fprintf(stderr, "-%c argument %lu too large", ch, tmp);
                    TEST_ERROR;
                }

                if ((ch == 'c' || ch == 'r') && tmp == 0) {
                    fprintf(stderr, "-%c argument %lu must be >= 1", ch, tmp);
                    TEST_ERROR;
                }

                if (ch == 'a')
                    s->asteps = (unsigned)tmp;
                else if (ch == 'c')
                    s->cols = (unsigned)tmp;
                else if (ch == 'l') {
                    /* Translate the tick number to time represented by the timespec struct */
                    float time = (float)(((unsigned)tmp * TICK_LEN) / 10.0);
                    unsigned sec = (unsigned)time;
                    unsigned nsec = (unsigned)((time - sec) * 10 * 1000 * 1000);

                    s->ival.tv_sec = sec;
                    s->ival.tv_nsec = nsec;
                } else if (ch == 'n')
                    s->nsteps = (unsigned)tmp;
                else if (ch == 'r')
                    s->rows = (unsigned)tmp;
                else
                    s->ndatasets = (unsigned)tmp;
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
        fprintf(stderr, "unexpected command-line arguments\n");
        TEST_ERROR;
    }

    if (s->vds != vds_off && s->two_dee) {
        fprintf(stderr, "virtual datasets and 2D datasets are mutually exclusive\n");
        TEST_ERROR;
    }

    s->chunk_dims[0]       = s->rows;
    s->chunk_dims[1]       = s->cols;
    s->one_dee_max_dims[0] = s->rows;
    if (s->fixed_array) {
        s->one_dee_max_dims[1] = s->cols * s->nsteps;
        two_dee_max_dims[0]    = s->rows * s->nsteps;
        two_dee_max_dims[1]    = s->cols * s->nsteps;
    }
    else {
        s->one_dee_max_dims[1] = H5S_UNLIMITED;
        two_dee_max_dims[0] = two_dee_max_dims[1] = H5S_UNLIMITED;
    }

    if (s->vds != vds_off) {
        const hsize_t half_chunk_dims[RANK] = {s->rows / 2, s->cols / 2};
        hsize_t       half_max_dims[RANK];

        if (s->fixed_array) {
            half_max_dims[0] = s->rows / 2;
            half_max_dims[1] = (s->cols * s->nsteps) / 2;
        }
        else {
            half_max_dims[0] = s->rows / 2;
            half_max_dims[1] = H5S_UNLIMITED;
        }

        if ((s->quadrant_dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
            fprintf(stderr, "H5Pcreate failed\n");
            TEST_ERROR;
        }

        if (H5Pset_chunk(s->quadrant_dcpl, RANK, half_chunk_dims) < 0) {
            fprintf(stderr, "H5Pset_chunk failed\n");
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
            fprintf(stderr, "make_quadrant_dataspace failed\n");
            TEST_ERROR;
        }

        if (!make_quadrant_dataspace(s, ur)) {
            fprintf(stderr, "make_quadrant_dataspace failed\n");
            TEST_ERROR;
        }

        if (!make_quadrant_dataspace(s, bl)) {
            fprintf(stderr, "make_quadrant_dataspace failed\n");
            TEST_ERROR;
        }

        if (!make_quadrant_dataspace(s, br)) {
            fprintf(stderr, "make_quadrant_dataspace failed\n");
            TEST_ERROR;
        }

        *src = (quadrant_t){.start  = {0, 0},
                            .stride = {s->rows / 2, s->cols / 2},
                            .block  = {s->rows / 2, s->cols / 2},
                            .count  = {1, H5S_UNLIMITED}};

        if ((src->space = H5Screate_simple(RANK, half_chunk_dims, half_max_dims)) < 0) {
            fprintf(stderr, "H5Screate_simple failed\n");
            TEST_ERROR;
        }

        if (H5Sselect_hyperslab(src->space, H5S_SELECT_SET, src->start, src->stride, src->count, src->block) < 0) {
            fprintf(stderr, "H5Sselect_hyperslab failed\n");
            TEST_ERROR;
        }

        if ((ul->src_space = H5Screate_simple(RANK, half_chunk_dims, half_max_dims)) < 0) {
            fprintf(stderr, "H5Screate_simple failed\n");
            TEST_ERROR;
        }

        if ((ur->src_space = H5Screate_simple(RANK, half_chunk_dims, half_max_dims)) < 0) {
            fprintf(stderr, "H5Screate_simple failed\n");
            TEST_ERROR;
        }

        if ((bl->src_space = H5Screate_simple(RANK, half_chunk_dims, half_max_dims)) < 0) {
            fprintf(stderr, "H5Screate_simple failed\n");
            TEST_ERROR;
        }

        if ((br->src_space = H5Screate_simple(RANK, half_chunk_dims, half_max_dims)) < 0) {
            fprintf(stderr, "H5Screate_simple failed\n");
            TEST_ERROR;
        }
    }

    /* space for attributes */
    if ((s->one_by_one_sid = H5Screate_simple(1, &dims, &dims)) < 0) {
        fprintf(stderr, "H5Screate_simple failed\n");
        TEST_ERROR;
    }

    if ((s->dataset = HDmalloc(sizeof(*s->dataset) * s->ndatasets)) == NULL) {
        fprintf(stderr, "HDmalloc failed\n");
        TEST_ERROR;
    }

    if ((s->sources = HDmalloc(sizeof(*s->sources) * s->ndatasets)) == NULL) {
        fprintf(stderr, "HDmalloc failed\n");
        TEST_ERROR;
    }

    for (i = 0; i < s->ndatasets; i++) {
        s->dataset[i]    = badhid;
        s->sources[i].ul = s->sources[i].ur = s->sources[i].bl = s->sources[i].br = badhid;
    }

    if ((s->memspace = H5Screate_simple(RANK, s->chunk_dims, NULL)) < 0) {
        fprintf(stderr, "H5Screate_simple failed\n");
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
        fprintf(stderr, "unknown personality, expected vfd_swmr_bigset_{reader,writer}\n");
        TEST_ERROR;
    }

    if ((s->dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0) {
        fprintf(stderr, "H5Pcreate failed\n");
        TEST_ERROR;
    }

    if (H5Pset_chunk_cache(s->dapl, 0, 0, H5D_CHUNK_CACHE_W0_DEFAULT) < 0) {
        fprintf(stderr, "H5Pset_chunk_cache failed\n");
        TEST_ERROR;
    }

    if (s->vds != vds_off && H5Pset_virtual_view(s->dapl, H5D_VDS_FIRST_MISSING) < 0) {
        fprintf(stderr, "H5Pset_virtual_view failed\n");
        TEST_ERROR;
    }

    return true;

error:
    H5E_BEGIN_TRY {
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
    } H5E_END_TRY;

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
    size_t i;

    if (H5Pclose(s->dapl) < 0) {
        fprintf(stderr, "H5Pclose failed\n");
        TEST_ERROR;
    }

    if (s->vds != vds_off) {
        quadrant_t *const ul = &s->quadrants.ul, *const ur = &s->quadrants.ur, *const bl = &s->quadrants.bl,
                          *const br = &s->quadrants.br;

        if (H5Sclose(ul->src_space) < 0 || H5Sclose(ur->src_space) < 0 || H5Sclose(bl->src_space) < 0 ||
            H5Sclose(br->src_space) < 0) {
            fprintf(stderr, "H5Sclose failed\n");
            TEST_ERROR;
        }

        if (H5Sclose(ul->space) < 0 || H5Sclose(ur->space) < 0 || H5Sclose(bl->space) < 0 ||
            H5Sclose(br->space) < 0) {
            fprintf(stderr, "H5Sclose failed\n");
            TEST_ERROR;
        }

        if (H5Pclose(s->quadrant_dcpl) < 0) {
            fprintf(stderr, "H5Pclose failed\n");
            TEST_ERROR;
        }
    }

    if (H5Sclose(s->one_by_one_sid) < 0) {
        fprintf(stderr, "H5Sclose failed\n");
        TEST_ERROR;
    }

    if (H5Sclose(s->memspace) < 0) {
        fprintf(stderr, "H5Sclose failed\n");
        TEST_ERROR;
    }

    for (i = 0; i < NELMTS(s->file); i++) {
        hid_t fid = s->file[i];

        s->file[i] = badhid;

        if (s->vds != vds_multi && i > 0)
            continue;

        if (H5Fclose(fid) < 0) {
            fprintf(stderr, "H5Fclose failed\n");
            TEST_ERROR;
        }
    }

    if (s->dataset)
        HDfree(s->dataset);

    if (s->sources)
        HDfree(s->sources);

    return true;

error:
    H5E_BEGIN_TRY {
        H5Pclose(s->quadrant_dcpl);
        H5Sclose(s->one_by_one_sid);
        H5Sclose(s->memspace);
    } H5E_END_TRY;

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
            if(HDremove(np->fifo_writer_to_reader) != 0) {
                fprintf(stderr, "HDremove fifo_writer_to_reader failed\n");
                TEST_ERROR;
            }

        if (HDaccess(np->fifo_reader_to_writer, F_OK) == 0)
            if(HDremove(np->fifo_reader_to_writer) != 0) {
                fprintf(stderr, "HDremove fifo_reader_to_writer failed\n");
                TEST_ERROR;
            }

        /* Writer creates two named pipes(FIFO) */
        if (HDmkfifo(np->fifo_writer_to_reader, 0600) < 0) {
            fprintf(stderr, "HDmkfifo fifo_writer_to_reader failed\n");
            TEST_ERROR;
        }

        if (HDmkfifo(np->fifo_reader_to_writer, 0600) < 0) {
            fprintf(stderr, "HDmkfifo fifo_reader_to_writer failed\n");
            TEST_ERROR;
        }
    }

    /* Both the writer and reader open the pipes */
    if ((np->fd_writer_to_reader = HDopen(np->fifo_writer_to_reader, O_RDWR)) < 0) {
        fprintf(stderr, "HDopen fifo_writer_to_reader failed\n");
        TEST_ERROR;
    }

    if ((np->fd_reader_to_writer = HDopen(np->fifo_reader_to_writer, O_RDWR)) < 0) {
        fprintf(stderr, "HDopen fifo_reader_to_writer failed\n");
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
        fprintf(stderr, "HDclose fd_writer_to_reader failed\n");
        TEST_ERROR;
    }

    if (HDclose(np->fd_reader_to_writer) < 0) {
        fprintf(stderr, "HDclose fd_reader_to_writer failed\n");
        TEST_ERROR;
    }

    /* Reader finishes last and deletes the named pipes */
    if(!writer) {
        if(HDremove(np->fifo_writer_to_reader) != 0) {
            fprintf(stderr, "HDremove fifo_writer_to_reader failed\n");
            TEST_ERROR;
        }

        if(HDremove(np->fifo_reader_to_writer) != 0) {
            fprintf(stderr, "HDremove fifo_reader_to_writer failed\n");
            TEST_ERROR;
        }
    }
    return true;

error:
    return false;
} /* np_close() */

/* Wait for the writer's notice before starting to zoo validation */
static int
reader_verify(np_state_t np, int verify)
{
    int notify;

    if (HDread(np.fd_writer_to_reader, &notify, sizeof(int)) < 0) {
        fprintf(stderr, "HDread failed\n");
        TEST_ERROR;
    }

    if (notify != verify) {
        fprintf(stderr, "expected %d but read %d\n", verify, notify);
        TEST_ERROR;
    }

    return 0;

error:
    return -1;
}

/* Notify the reader of finishing zoo creation by sending the timestamp
 * and wait for the reader to finish validation before proceeding */
static int
notify_and_wait_for_reader(state_t *s, np_state_t *np)
{
    int notify;
    unsigned int i;
    struct timespec last = {0, 0};

    /* Get the time when finishing zoo creation */
    if (HDclock_gettime(CLOCK_MONOTONIC, &last) < 0) {
        fprintf(stderr, "HDclock_gettime failed\n");
        TEST_ERROR;
    }

    /* Notify the reader of finishing zoo creation by sending the timestamp */
    if (HDwrite(np->fd_writer_to_reader, &last, sizeof(last)) < 0) {
        fprintf(stderr, "HDwrite failed\n");
        TEST_ERROR;
    }

    /* During the wait, writer makes repeated HDF5 API calls so as to trigger
     * EOT at approximately the correct time */
    for(i = 0; i < MAX_LAG + 1; i++) {
        decisleep(TICK_LEN);

        H5E_BEGIN_TRY {
            H5Aexists(s->file[0], "nonexistent");
        } H5E_END_TRY;
    }

    /* Wait until the reader finishes validating zoo creation */
    if (HDread(np->fd_reader_to_writer, &notify, sizeof(int)) < 0) {
        fprintf(stderr, "HDread failed\n");
        TEST_ERROR;
    }

    if (notify != np->verify) {
        fprintf(stderr, "expected %d but read %d\n", np->verify, notify);
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

    /* Receive the notice of the writer finishing zoo creation (timestamp) */
    if (HDread(np->fd_writer_to_reader, &last, sizeof(last)) < 0) {
        fprintf(stderr, "HDread failed\n");
        TEST_ERROR;
    }

    /* Make sure the dataset validation doesn't take longer than the expected time.
     * This time period is from the writer finishing dataset creation to the reader finishing
     * the validation of dataset creation */
    if (below_speed_limit(&last, &(s.ival))) {
        AT();
        fprintf(stderr, "dataset validation took too long to finish\n");
    }

    /* Notify the writer that dataset validation is finished */
    if (HDwrite(np->fd_reader_to_writer, &(np->notify), sizeof(int)) < 0) {
        fprintf(stderr, "HDwrite failed\n");
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
    exchange_info_t last;

    /* Get the time */
    if (HDclock_gettime(CLOCK_MONOTONIC, &(last.time)) < 0) {
        fprintf(stderr, "HDclock_gettime failed\n");
        TEST_ERROR;
    }

    last.step = step;

    /* Notify the reader by sending the timestamp and the number of chunks written */
    if (HDwrite(np->fd_writer_to_reader, &last, sizeof(last)) < 0) {
        H5_FAILED(); AT();
        fprintf(stderr, "HDwrite failed");
        goto error;
    }

    return 0;

error:
    return -1;
}

static bool
create_extensible_dset(state_t *s, unsigned int which)
{
    quadrant_t *const ul = &s->quadrants.ul, *const ur = &s->quadrants.ur, *const bl = &s->quadrants.bl,
                      *const br = &s->quadrants.br, *const src = &s->quadrants.src;
    char dname[sizeof("/dataset-9999999999")];
    char ul_dname[sizeof("/ul-dataset-9999999999")], ur_dname[sizeof("/ur-dataset-9999999999")],
        bl_dname[sizeof("/bl-dataset-9999999999")], br_dname[sizeof("/br-dataset-9999999999")];
    hid_t dcpl = H5I_INVALID_HID, dset_id = H5I_INVALID_HID, filespace = H5I_INVALID_HID;

    esnprintf(dname, sizeof(dname), "/dataset-%d", which);

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
        fprintf(stderr, "H5Pcreate failed\n");
        TEST_ERROR;
    }

    if (H5Pset_chunk(dcpl, RANK, s->chunk_dims) < 0) {
        fprintf(stderr, "H5Pset_chunk failed\n");
        TEST_ERROR;
    }

    if (s->vds != vds_off) {
        sources_t *const srcs = &s->sources[which];

        esnprintf(ul_dname, sizeof(ul_dname), "/ul-dataset-%d", which);
        esnprintf(ur_dname, sizeof(ur_dname), "/ur-dataset-%d", which);
        esnprintf(bl_dname, sizeof(bl_dname), "/bl-dataset-%d", which);
        esnprintf(br_dname, sizeof(br_dname), "/br-dataset-%d", which);

        if ((srcs->ul = H5Dcreate2(s->file[0], ul_dname, s->filetype, ul->src_space, H5P_DEFAULT, s->quadrant_dcpl, s->dapl)) < 0) {
            fprintf(stderr, "H5Dcreate2 failed\n");
            TEST_ERROR;
        }

        if ((srcs->ur = H5Dcreate2(s->file[1], ur_dname, s->filetype, ur->src_space, H5P_DEFAULT, s->quadrant_dcpl, s->dapl)) < 0) {
            fprintf(stderr, "H5Dcreate2 failed\n");
            TEST_ERROR;
        }

        if ((srcs->bl = H5Dcreate2(s->file[2], bl_dname, s->filetype, bl->src_space, H5P_DEFAULT, s->quadrant_dcpl, s->dapl)) < 0) {
            fprintf(stderr, "H5Dcreate2 failed\n");
            TEST_ERROR;
        }

        if ((srcs->br = H5Dcreate2(s->file[3], br_dname, s->filetype, br->src_space, H5P_DEFAULT, s->quadrant_dcpl, s->dapl)) < 0) {
            fprintf(stderr, "H5Dcreate2 failed\n");
            TEST_ERROR;
        }

        if (H5Pset_virtual(dcpl, ul->space, s->filename[0], ul_dname, src->space) < 0) {
            fprintf(stderr, "H5Pset_virtual failed\n");
            TEST_ERROR;
        }

        if (H5Pset_virtual(dcpl, ur->space, s->filename[1], ur_dname, src->space) < 0) {
            fprintf(stderr, "H5Pset_virtual failed\n");
            TEST_ERROR;
        }

        if (H5Pset_virtual(dcpl, bl->space, s->filename[2], bl_dname, src->space) < 0) {
            fprintf(stderr, "H5Pset_virtual failed\n");
            TEST_ERROR;
        }

        if (H5Pset_virtual(dcpl, br->space, s->filename[3], br_dname, src->space) < 0) {
            fprintf(stderr, "H5Pset_virtual failed\n");
            TEST_ERROR;
        }
    }

    if ((filespace = H5Screate_simple(NELMTS(s->chunk_dims), s->chunk_dims,
                                 s->two_dee ? two_dee_max_dims : s->one_dee_max_dims)) < 0) {
        fprintf(stderr, "H5Screate_simple failed\n");
        TEST_ERROR;
    }

    if ((dset_id = H5Dcreate2(s->file[0], dname, s->filetype, filespace, H5P_DEFAULT, dcpl, s->dapl)) < 0) {
        fprintf(stderr, "H5Dcreate2 failed\n");
        TEST_ERROR;
    }

    if (H5Sclose(filespace) < 0) {
        fprintf(stderr, "H5Sclose failed\n");
        TEST_ERROR;
    }

    if (H5Pclose(dcpl) < 0) {
        fprintf(stderr, "H5Pclose failed\n");
        TEST_ERROR;
    }

    s->dataset[which] = dset_id;

    return true;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset_id);
        H5Pclose(dcpl);
        H5Sclose(filespace);
    } H5E_END_TRY;

    return false;
}

static bool
close_extensible_dset(state_t *s, unsigned int which)
{
    char  dname[sizeof("/dataset-9999999999")];
    hid_t dset_id = H5I_INVALID_HID;

    if (which >= s->ndatasets) {
        fprintf(stderr, "index is out of range\n");
        TEST_ERROR;
    }

    esnprintf(dname, sizeof(dname), "/dataset-%d", which);

    dset_id = s->dataset[which];

    if (H5Dclose(dset_id) < 0) {
        fprintf(stderr, "H5Dclose failed\n");
        TEST_ERROR;
    }

    s->dataset[which] = badhid;

    if (s->vds != vds_off && s->writer) {
        sources_t *const srcs = &s->sources[which];

        if (H5Dclose(srcs->ul) < 0 || H5Dclose(srcs->ur) < 0 || H5Dclose(srcs->bl) < 0 ||
            H5Dclose(srcs->br) < 0) {
            fprintf(stderr, "H5Dclose failed\n");
            TEST_ERROR;
        }
    }

    return true;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset_id);
    } H5E_END_TRY;

    return false;
}

static bool
open_extensible_dset(state_t *s)
{
    hsize_t                      dims[RANK], maxdims[RANK];
    char                         dname[sizeof("/dataset-9999999999")];
    hid_t                        dset_id, filespace, dtype;
    unsigned int                 which, i;

    for (which = 0; which < s->ndatasets; which++) {
        esnprintf(dname, sizeof(dname), "/dataset-%d", which);

        /* Tries to open the dataset repeatedly until successful.  After trying
         * NUM_ATTEMPTS times without success, report it as a failure
         */
        for (i = 0; i < NUM_ATTEMPTS; i++) {
            H5E_BEGIN_TRY {
                dset_id = H5Dopen2(s->file[0], dname, s->dapl);
            } H5E_END_TRY;

            if (dset_id >= 0)
                break;
            else
                decisleep(1);
        }

        if (i == NUM_ATTEMPTS) {
            fprintf(stderr, "chunk verification reached the maximal number of attempts\n");
            TEST_ERROR;
        }

        if ((dtype = H5Dget_type(dset_id)) < 0) {
            fprintf(stderr, "H5Dget_type failed\n");
            TEST_ERROR;
        }

        if (H5Tequal(dtype, s->filetype) <= 0) {
            fprintf(stderr, "Unexpected data type\n");
            TEST_ERROR;
        }

        if ((filespace = H5Dget_space(dset_id)) < 0) {
            fprintf(stderr, "H5Dget_space failed\n");
            TEST_ERROR;
        }

        if (H5Sget_simple_extent_ndims(filespace) != RANK) {
            fprintf(stderr, "Unexpected data rank\n");
            TEST_ERROR;
        }

        if (H5Sget_simple_extent_dims(filespace, dims, maxdims) < 0) {
            fprintf(stderr, "H5Sget_simple_extent_dims failed\n");
            TEST_ERROR;
        }

        if (H5Sclose(filespace) < 0) {
            fprintf(stderr, "H5Sclose failed\n");
            TEST_ERROR;
        }

        if (H5Tclose(dtype) < 0) {
            fprintf(stderr, "H5Tclose failed\n");
            TEST_ERROR;
        }

        if (s->two_dee) {
            if (maxdims[0] != two_dee_max_dims[0] || maxdims[1] != two_dee_max_dims[1] ||
                maxdims[0] != maxdims[1]) {
                fprintf(stderr, "Unexpected maximum dimensions %" PRIuHSIZE " x %" PRIuHSIZE, maxdims[0],
                     maxdims[1]);
                TEST_ERROR;
            }
        } else if (maxdims[0] != s->one_dee_max_dims[0] || maxdims[1] != s->one_dee_max_dims[1] ||
                dims[0] != s->chunk_dims[0]) {
            fprintf(stderr,
                 "Unexpected maximum dimensions %" PRIuHSIZE " x %" PRIuHSIZE " or columns %" PRIuHSIZE,
                 maxdims[0], maxdims[1], dims[1]);
        }

        s->dataset[which] = dset_id;
    }

    return true;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset_id);
        H5Tclose(dtype);
        H5Sclose(filespace);
    } H5E_END_TRY;

    return false;
}

/* Write or verify the dataset test pattern in the matrix `mat`.
 * `mat` is a "subview" of the `which`th dataset with origin
 * `(base.row, base.col)`.
 *
 * If `do_set` is true, write the pattern; otherwise, verify.
 *
 * The basic test pattern consists of increasing
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
 */
static bool
set_or_verify_matrix(mat_t *mat, unsigned int which, base_t base, bool do_set)
{
    unsigned row, col;
    bool ret = true;

    for (row = 0; row < mat->rows; row++) {
        for (col = 0; col < mat->cols; col++) {
            uint32_t v;
            hsize_t  i = base.row + row, j = base.col + col, u;

            if (j <= i)
                u = (i + 1) * (i + 1) - 1 - j;
            else
                u = j * j + i;

            v = (uint32_t)(u + which);
            if (do_set) {
                if (!matset(mat, row, col, v)) {
                    fprintf(stderr, "data initialization failed\n");
                    ret = false;
                    break;
                }
            } else if (matget(mat, row, col) != v) {
                /* If the data doesn't match, simply return false and
                 * let the caller repeat this step
                 */
                ret = false;
                break;
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

static bool
verify_chunk(state_t *s, hid_t filespace, mat_t *mat, unsigned which, base_t base)
{
    hsize_t offset[RANK] = {base.row, base.col};
    herr_t  status;
    hid_t   dset_id;

    if (which >= s->ndatasets) {
        fprintf(stderr, "the dataset order is bigger than the number of datasets");
        TEST_ERROR;
    }

    dbgf(1, "verifying chunk %" PRIuHSIZE ", %" PRIuHSIZE "\n", base.row, base.col);

    dset_id = s->dataset[which];

    if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, NULL, s->chunk_dims, NULL) < 0) {
        fprintf(stderr, "H5Sselect_hyperslab failed\n");
        TEST_ERROR;
    }

    /* A failure to read the data may indicate the data isn't ready yet.  Instead of displaying the error stack,
     * simply return false and let the caller repeat this step.
     */
    H5E_BEGIN_TRY {
        status = H5Dread(dset_id, H5T_NATIVE_UINT32, s->memspace, filespace, H5P_DEFAULT, mat->elt);
    } H5E_END_TRY;

    if (status < 0)
        TEST_ERROR;

    return verify_matrix(mat, which, base);

error:
    return false;
}

static bool
repeat_verify_chunk(state_t *s, hid_t filespace, mat_t *mat, unsigned which, base_t base)
{
    hid_t dset_id = s->dataset[which];
    unsigned i;

    /* If the chunk data isn't good after reading it NUM_ATTEMPTS times, report it as a failure */
    for (i = 0; i < NUM_ATTEMPTS; i++) {
        if (verify_chunk(s, filespace, mat, which, base))
            break;
        else {
            decisleep(1);

            /* Refresh the dataset and try it again */
            if (H5Drefresh(dset_id) < 0) {
                fprintf(stderr, "H5Drefresh failed\n");
                TEST_ERROR;
            }
        }
    }

    if (i == NUM_ATTEMPTS) {
        fprintf(stderr, "chunk verification reached the maximal number of attempts\n");
        TEST_ERROR;
    }

    return true;

error:
    return false;
}

static bool
init_and_write_chunk(state_t *s, hid_t filespace, mat_t *mat, unsigned which, base_t base)
{
    hsize_t offset[RANK] = {base.row, base.col};
    hid_t   dset_id;

    dset_id = s->dataset[which];

    if (!init_matrix(mat, which, base)) {
        fprintf(stderr, "data initialization failed\n");
        TEST_ERROR;
    }

    if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, NULL, s->chunk_dims, NULL) < 0) {
        fprintf(stderr, "H5Sselect_hyperslab failed\n");
        TEST_ERROR;
    }

    if (H5Dwrite(dset_id, H5T_NATIVE_UINT32, s->memspace, filespace, H5P_DEFAULT, mat->elt) < 0) {
        fprintf(stderr, "H5Dwrite failed\n");
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
        fprintf(stderr, "H5Aopen failed\n");
        TEST_ERROR;
    }

    if (H5Aread(aid, H5T_NATIVE_UINT, &read_step) < 0) {
        fprintf(stderr, "H5Aread failed\n");
        TEST_ERROR;
    }

    if (H5Aclose(aid) < 0) {
        fprintf(stderr, "H5Aclose failed\n");
        TEST_ERROR;
    }

    if (read_step != step) {
        fprintf(stderr, "expected %u read %u\n", step, read_step);
        TEST_ERROR;
    }

    return true;

error:
    H5E_BEGIN_TRY {
        H5Aclose(aid);
    } H5E_END_TRY;

    return false;
}

static bool
verify_extensible_dset(state_t *s, unsigned int which, mat_t *mat, unsigned finished_step, unsigned last_step)
{
    hid_t        dset_id = H5I_INVALID_HID, filespace = H5I_INVALID_HID;
    hsize_t      size[RANK];
    base_t       base, last;
    unsigned int ncols, step;
    int          i;

    if (which >= s->ndatasets) {
        fprintf(stderr, "the dataset order is bigger than the number of datasets");
        TEST_ERROR;
    }

    dset_id = s->dataset[which];

    /* Attempt to check the availablity of the chunks for a number time before reporting it as a failure */
    for (i = 0; i < NUM_ATTEMPTS; i++) {
        if (H5Drefresh(dset_id) < 0) {
            fprintf(stderr, "H5Drefresh failed\n");
            TEST_ERROR;
        }

        if ((filespace = H5Dget_space(dset_id)) < 0) {
            fprintf(stderr, "H5Dget_space failed\n");
            TEST_ERROR;
        }

        if (H5Sget_simple_extent_dims(filespace, size, NULL) < 0) {
            fprintf(stderr, "H5Sget_simple_extent_dims failed\n");
            TEST_ERROR;
        }

        ncols = (unsigned)(size[1] / s->chunk_dims[1]);

        /* Make sure the chunks show up on the reader side.  Otherwise sleep a while and try again */
        if (ncols >= last_step)
            break;
        else
            decisleep(1);
    }

    if (i == NUM_ATTEMPTS) {
        fprintf(stderr, "chunk verification reached the maximal number of attempts");
        TEST_ERROR;
    }

    for (step = finished_step; step < last_step; step++) {
        dbgf(1, "%s: which %u step %u\n", __func__, which, step);

        if (s->two_dee) {
            size[0]  = s->chunk_dims[0] * (1 + step);
            size[1]  = s->chunk_dims[1] * (1 + step);
            last.row = s->chunk_dims[0] * step;
            last.col = s->chunk_dims[1] * step;
        }
        else {
            size[0]  = s->chunk_dims[0];
            size[1]  = s->chunk_dims[1] * (1 + step);
            last.row = 0;
            last.col = s->chunk_dims[1] * step;
        }

        dbgf(1, "new size %" PRIuHSIZE ", %" PRIuHSIZE "\n", size[0], size[1]);
        dbgf(1, "last row %" PRIuHSIZE " col %" PRIuHSIZE "\n", last.row, last.col);

        if (s->two_dee) {

            /* Down the right side, intersecting the bottom row. */
            base.col = last.col;
            for (base.row = 0; base.row <= last.row; base.row += s->chunk_dims[0]) {
                if (!repeat_verify_chunk(s, filespace, mat, which, base)) {
                    fprintf(stderr, "chunk verification failed\n");
                    TEST_ERROR;
                }
            }

            /* Across the bottom, stopping before the last column to
             * avoid re-reading the bottom-right chunk.
             */
            base.row = last.row;
            for (base.col = 0; base.col < last.col; base.col += s->chunk_dims[1]) {
                if (!repeat_verify_chunk(s, filespace, mat, which, base)) {
                    fprintf(stderr, "chunk verification failed\n");
                    TEST_ERROR;
                }
            }
        }
        else {
            if (!repeat_verify_chunk(s, filespace, mat, which, last)) {
                fprintf(stderr, "chunk verification failed\n");
                TEST_ERROR;
            }
        }

        if (s->asteps != 0 && step % s->asteps == 0) {
            if (!verify_dset_attribute(dset_id, which, step)) {
                fprintf(stderr, "verify_dset_attribute failed\n");
                TEST_ERROR;
            }
        }
    }

    return true;

error:
    H5E_BEGIN_TRY {
        H5Sclose(filespace);
    } H5E_END_TRY;

    return false;
}

static bool
verify_dsets(state_t s, np_state_t *np, mat_t *mat)
{
    unsigned *nextstep = NULL;
    unsigned  finished_step = 0;
    unsigned which;
    exchange_info_t last;

    if (!(nextstep = HDcalloc(s.ndatasets, sizeof(*nextstep)))) {
        fprintf(stderr, "memory allocation failed\n");
        TEST_ERROR;
    }

    do {
        /* Receive the notice of the writer finishing zoo creation,
         * including the number of chunks finished and the timestamp
         */
        if (s.use_named_pipe && HDread(np->fd_writer_to_reader, &last, sizeof(last)) < 0) {
            fprintf(stderr, "HDread failed\n");
            TEST_ERROR;
        }

        for (which = 0; which < s.ndatasets; which++) {
            dbgf(1, "step %d which %d\n", nextstep[which], which);

            /* Verify the chunks starting from the finished one in last round
             * to the ones written in this round
             */
            if (!verify_extensible_dset(&s, which, mat, finished_step, last.step)) {
                fprintf(stderr, "verify_extensible_dset failed\n");
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
            fprintf(stderr, "verify_extensible_dset took too long to finish\n");
        }
    } while (finished_step < s.nsteps);

    if (nextstep)
        HDfree(nextstep);

    return true;

error:
    if (nextstep)
        HDfree(nextstep);

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
        fprintf(stderr, "H5Acreate2 failed\n");
        TEST_ERROR;
    }

    if (H5Awrite(aid, H5T_NATIVE_UINT, &step) < 0) {
        fprintf(stderr, "H5Awrite failed\n");
        TEST_ERROR;
    }

    if (H5Aclose(aid) < 0) {
        fprintf(stderr, "H5Aclose failed\n");
        TEST_ERROR;
    }

    return true;

error:
    H5E_BEGIN_TRY {
        H5Aclose(aid);
    } H5E_END_TRY;

    return false;
}

static bool
write_extensible_dset(state_t *s, unsigned int which, unsigned int step, mat_t *mat)
{
    hid_t   ds = H5I_INVALID_HID, filespace = H5I_INVALID_HID;
    hsize_t size[RANK];
    base_t  base, last;
    char dname[sizeof("/dataset-9999999999")];

    esnprintf(dname, sizeof(dname), "/dataset-%d", which);

    dbgf(1, "%s: which %u step %u\n", __func__, which, step);

    if (which >= s->ndatasets) {
        fprintf(stderr, "index is out of range\n");
        TEST_ERROR;
    }

    ds = s->dataset[which];

    if (s->asteps != 0 && step % s->asteps == 0) {
        if (!add_dset_attribute(s, ds, s->one_by_one_sid, which, step)) {
            fprintf(stderr, "add_dset_attribute failed\n");
            TEST_ERROR;
        }
    }

    if (s->two_dee) {
        size[0]  = s->chunk_dims[0] * (1 + step);
        size[1]  = s->chunk_dims[1] * (1 + step);
        last.row = s->chunk_dims[0] * step;
        last.col = s->chunk_dims[1] * step;
    }
    else {
        size[0]  = s->chunk_dims[0];
        size[1]  = s->chunk_dims[1] * (1 + step);
        last.row = 0;
        last.col = s->chunk_dims[1] * step;
    }

    dbgf(1, "new size %" PRIuHSIZE ", %" PRIuHSIZE "\n", size[0], size[1]);

    if (s->vds != vds_off) {
        const hsize_t    half_size[RANK] = {size[0] / 2, size[1] / 2};
        sources_t *const srcs            = &s->sources[which];

        if (H5Dset_extent(srcs->ul, half_size) < 0) {
            fprintf(stderr, "H5Dset_extent failed\n");
            TEST_ERROR;
        }

        if (H5Dset_extent(srcs->ur, half_size) < 0) {
            fprintf(stderr, "H5Dset_extent failed\n");
            TEST_ERROR;
        }

        if (H5Dset_extent(srcs->bl, half_size) < 0) {
            fprintf(stderr, "H5Dset_extent failed\n");
            TEST_ERROR;
        }

        if (H5Dset_extent(srcs->br, half_size) < 0) {
            fprintf(stderr, "H5Dset_extent failed\n");
            TEST_ERROR;
        }
    } else if (H5Dset_extent(ds, size) < 0) {
        fprintf(stderr, "H5Dset_extent failed\n");
        TEST_ERROR;
    }

    if ((filespace = H5Dget_space(ds)) < 0) {
        fprintf(stderr, "H5Dget_space failed\n");
        TEST_ERROR;
    }

    if (s->two_dee) {
        base.col = last.col;
        for (base.row = 0; base.row <= last.row; base.row += s->chunk_dims[0]) {
            dbgf(1, "writing chunk %" PRIuHSIZE ", %" PRIuHSIZE "\n", base.row, base.col);
            if (!init_and_write_chunk(s, filespace, mat, which, base)) {
                fprintf(stderr, "init_and_write_chunk failed\n");
                TEST_ERROR;
            }
        }

        base.row = last.row;
        for (base.col = 0; base.col < last.col; base.col += s->chunk_dims[1]) {
            dbgf(1, "writing chunk %" PRIuHSIZE ", %" PRIuHSIZE "\n", base.row, base.col);
            if (!init_and_write_chunk(s, filespace, mat, which, base)) {
                fprintf(stderr, "init_and_write_chunk failed\n");
                TEST_ERROR;
            }
        }
    }
    else {
        if (!init_and_write_chunk(s, filespace, mat, which, last)) {
            fprintf(stderr, "init_and_write_chunk failed\n");
            TEST_ERROR;
        }
    }

    if (H5Sclose(filespace) < 0) {
        fprintf(stderr, "H5Sclose failed\n");
        TEST_ERROR;
    }

    return true;

error:
    H5E_BEGIN_TRY {
        H5Sclose(filespace);
    } H5E_END_TRY;

    return false;
}

static bool
write_dsets(state_t s, np_state_t *np, mat_t *mat)
{
    unsigned last_step, step, which;
    unsigned long long old_tick_num;
    H5F_t *f = NULL;

    if (NULL == (f = (H5F_t *)H5VL_object(s.file[0]))) {
        fprintf(stderr, "H5VL_object failed\n");
        TEST_ERROR;
    }

    old_tick_num = f->shared->tick_num;

    /* Write as many as chunks within the same tick number before notifying
     * the reader to verify them.
     */
    for (step = 0; step < s.nsteps; step++) {
        /* Write as many as chunks before the tick number changes */
        if (f->shared->tick_num == old_tick_num) {
            for (which = 0; which < s.ndatasets; which++) {
                dbgf(2, "step %d which %d\n", step, which);
                if (!write_extensible_dset(&s, which, step, mat)) {
                    fprintf(stderr, "write_extensible_dset failed\n");
                    TEST_ERROR;
                }
            }
        }

        /* After finishing writing all the chunks, end the tick */
        if(s.use_vfd_swmr && step == (s.nsteps - 1)) {
           unsigned long i;

           if (s.vds != vds_multi)
               H5Fvfd_swmr_end_tick(s.file[0]);
           else
               for (i = 0; i < NELMTS(s.file); i++)
                   H5Fvfd_swmr_end_tick(s.file[i]);
        }

        /* Notify the reader to start verification by
         * sending the timestamp and the number of chunks written
         */
        if (f->shared->tick_num > old_tick_num || step == (s.nsteps - 1)) {
            last_step = step + 1;
            if (s.use_named_pipe && notify_reader(np, last_step) < 0) {
                fprintf(stderr, "notify_reader failed\n");
                TEST_ERROR;
            }

            old_tick_num = f->shared->tick_num;
        }
    }

    return true;

error:
    return false;
}

int
main(int argc, char **argv)
{
    mat_t *  mat;
    hid_t    fcpl = H5I_INVALID_HID;
    unsigned which;
    state_t  s;
    np_state_t np;
    size_t   i;

    if (!state_init(&s, argc, argv)) {
        fprintf(stderr, "state_init failed\n");
        TEST_ERROR;
    }

    if ((mat = newmat(s.rows, s.cols)) == NULL) {
        fprintf(stderr, "could not allocate matrix\n");
        TEST_ERROR;
    }

    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0) {
        fprintf(stderr, "H5Pcreate failed\n");
        TEST_ERROR;
    }

    if (H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, false, 1) < 0) {
        fprintf(stderr, "H5Pset_file_space_strategy failed\n");
        TEST_ERROR;
    }

    for (i = 0; i < NELMTS(s.file); i++) {
        hid_t                 fapl;
        H5F_vfd_swmr_config_t config;

        if (s.vds != vds_multi && i > 0) {
            s.file[i] = s.file[0];
            continue;
        }

        /* config, tick_len, max_lag, writer, flush_raw_data, md_pages_reserved, md_file_path */
        init_vfd_swmr_config(&config, TICK_LEN, MAX_LAG, s.writer, FALSE, 128, "./bigset-shadow-%zu", i);

        /* use_latest_format, use_vfd_swmr, only_meta_page, config */
        if ((fapl = vfd_swmr_create_fapl(true, s.use_vfd_swmr, true, &config)) < 0) {
            fprintf(stderr, "vfd_swmr_create_fapl failed\n");
            TEST_ERROR;
        }

        s.file[i] = s.writer ? H5Fcreate(s.filename[i], H5F_ACC_TRUNC, fcpl, fapl)
                             : H5Fopen(s.filename[i], H5F_ACC_RDONLY, fapl);

        if (s.file[i] == badhid) {
            fprintf(stderr, s.writer ? "H5Fcreate failed" : "H5Fopen failed");
            TEST_ERROR;
        }

        if (H5Pclose(fapl) < 0) {
            fprintf(stderr, "H5Pclose failed\n");
            TEST_ERROR;
        }
    }

    /* Initiailze named pipes */
    if(s.use_named_pipe && !np_init(&np, s.writer)) {
        fprintf(stderr, "np_init() failed\n");
        TEST_ERROR;
    }

    if (s.writer) {
        /* Writer tells reader to start */
        np.notify = 1;
        if (s.use_named_pipe && HDwrite(np.fd_writer_to_reader, &np.notify, sizeof(int)) < 0) {
            fprintf(stderr, "HDwrite failed\n");
            TEST_ERROR;
        }

        /* Create NDATASETS datasets as the reader is doing verification.  So no communication with
         * the reader during the creation of datasets.
         */
        for (which = 0; which < s.ndatasets; which++)
            if (!create_extensible_dset(&s, which)) {
                fprintf(stderr, "create_extensible_dset failed: number %u\n", which);
                TEST_ERROR;
        }

        /* Call H5Fvfd_swmr_end_tick to end the tick.  No communication with the reader in this step */
        if(s.use_vfd_swmr && s.use_named_pipe) {
           unsigned long j;

           if (s.vds != vds_multi) {
               if (H5Fvfd_swmr_end_tick(s.file[0]) < 0) {
                   fprintf(stderr, "H5Fvfd_swmr_end_tick failed\n");
                   TEST_ERROR;
               }
           } else {
               for (j = 0; j < NELMTS(s.file); j++)
                   if (H5Fvfd_swmr_end_tick(s.file[j]) < 0) {
                       fprintf(stderr, "H5Fvfd_swmr_end_tick failed\n");
                       TEST_ERROR;
                   }
           }
        }

        /* Notify the reader of finishing dataset creation by sending the timestamp
         * and wait for the reader to finish validation before proceeding */
        np.verify = 2;
        if (s.use_named_pipe && notify_and_wait_for_reader(&s, &np) < 0) {
            fprintf(stderr, "notify_and_wait_for_reader failed\n");
            TEST_ERROR;
        }

        /* Start to write chunks.  The writer writes as many chunks as possible within a tick, then
         * notify the reader.  But it doesn't receive back the reader's notice. */
        if (!write_dsets(s, &np, mat)) {
            fprintf(stderr, "write_dsets failed");
            TEST_ERROR;
        }
    } else {
        /* Wait for the writer's notice before starting the validation of dataset creation */
        np.verify = 1;
        if (s.use_named_pipe && reader_verify(np, np.verify) < 0) {
            fprintf(stderr, "reader_verify failed\n");
            TEST_ERROR;
        }

        /* Open all the datasets as the writer is creating them.  No communication with
         * the writer during this step.
         */
        if (!open_extensible_dset(&s)) {
            fprintf(stderr, "open_extensible_dset failed\n");
            TEST_ERROR;
        }

        /* Receive the notice of the writer finishing dataset creation (timestamp)
         * Make sure the dataset creation doesn't take longer than the expected time.
         * This time period is from the writer finishing dataset creation to the reader finishing
         * the validation of dataset creation */
        np.notify = 2;
        if (s.use_named_pipe && reader_check_time_and_notify_writer(&np, s) < 0) {
            fprintf(stderr, "reader_check_time_and_notify_writer failed\n");
            TEST_ERROR;
        }

        /* Once the reader starts to verify the datasets, it doesn't notify the writer any info.
         * Both the reader and writer finish by themselves.
         */
        if (!verify_dsets(s, &np, mat)) {
            fprintf(stderr, "verify_dsets failed\n");
            TEST_ERROR;
        }
    }

    for (which = 0; which < s.ndatasets; which++)
        if (!close_extensible_dset(&s, which)) {
            fprintf(stderr, "close_extensible_dset failed\n");
            TEST_ERROR;
        }

    if (H5Pclose(fcpl) < 0) {
        fprintf(stderr, "H5Pclose failed\n");
        TEST_ERROR;
    }

    if(s.use_named_pipe && !np_close(&np, s.writer)) {
        fprintf(stderr, "np_close() failed\n");
        TEST_ERROR;
    }

    if (!state_destroy(&s)) {
        fprintf(stderr, "state_destroy failed\n");
        TEST_ERROR;
    }

    HDfree(mat);

    return EXIT_SUCCESS;

error:
    H5E_BEGIN_TRY {
        H5Pclose(fcpl);

        for (i = 0; i < NELMTS(s.file); i++)
            H5Fclose(s.file[i]);
    } H5E_END_TRY;

    if (s.use_named_pipe && np.fd_writer_to_reader >= 0)
        HDclose(np.fd_writer_to_reader);

    if (s.use_named_pipe && np.fd_reader_to_writer >= 0)
        HDclose(np.fd_reader_to_writer);

    if(s.use_named_pipe && !s.writer) {
        HDremove(np.fifo_writer_to_reader);
        HDremove(np.fifo_reader_to_writer);
    }

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
