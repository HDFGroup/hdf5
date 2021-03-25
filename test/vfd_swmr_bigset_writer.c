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

#include <err.h>
#include <libgen.h>

#define ROWS 256
#define COLS 512
#define RANK 2

static const unsigned int hang_back = 3;

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
    struct timespec update_interval;
    struct {
        quadrant_t ul, ur, bl, br, src;
    } quadrants;
    unsigned int cols, rows;
    unsigned int asteps;
    unsigned int nsteps;
    bool         two_dee;
    bool         wait_for_signal;
    enum { vds_off, vds_single, vds_multi } vds;
    bool    use_vfd_swmr;
    bool    writer;
    bool    fixed_array;
    hsize_t chunk_dims[RANK];
    hsize_t one_dee_max_dims[RANK];
} state_t;

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
                     .wait_for_signal  = true,
                     .vds              = vds_off,
                     .use_vfd_swmr     = true,
                     .writer           = true,
                     .fixed_array      = false,
                     .one_dee_max_dims = {ROWS, H5S_UNLIMITED},
                     .chunk_dims       = {ROWS, COLS},
                     .update_interval =
                         (struct timespec){.tv_sec = 0, .tv_nsec = 1000000000UL / 30 /* 1/30 second */}};
}

static void state_init(state_t *, int, char **);

static const hid_t badhid = H5I_INVALID_HID;

static hsize_t two_dee_max_dims[RANK];

static uint32_t
matget(const mat_t *mat, unsigned i, unsigned j)
{
    assert(i < mat->rows && j < mat->cols);

    return mat->elt[i * mat->cols + j];
}

static void
matset(mat_t *mat, unsigned i, unsigned j, uint32_t v)
{
    assert(i < mat->rows && j < mat->cols);

    mat->elt[i * mat->cols + j] = v;
}

static mat_t *
newmat(unsigned rows, unsigned cols)
{
    mat_t *mat;

    mat = malloc(sizeof(*mat) + (rows * cols - 1) * sizeof(mat->elt[0]));

    if (mat == NULL)
        err(EXIT_FAILURE, "%s: malloc", __func__);

    mat->rows = rows;
    mat->cols = cols;

    return mat;
}

static void
usage(const char *progname)
{
    fprintf(stderr,
            "usage: %s [-F] [-M] [-S] [-V] [-W] [-a steps] [-b] [-c cols]\n"
            "    [-d dims]\n"
            "    [-n iterations] [-r rows] [-s datasets]\n"
            "    [-u milliseconds]\n"
            "\n"
            "-F:                   fixed maximal dimension for the chunked datasets\n"
            "-M:	               use virtual datasets and many source\n"
            "                      files\n"
            "-S:	               do not use VFD SWMR\n"
            "-V:	               use virtual datasets and a single\n"
            "                      source file\n"
            "-W:	               do not wait for a signal before\n"
            "                      exiting\n"
            "-a steps:	       `steps` between adding attributes\n"
            "-b:	               write data in big-endian byte order\n"
            "-c cols:	       `cols` columns per chunk\n"
            "-d 1|one|2|two|both:  select dataset expansion in one or\n"
            "                      both dimensions\n"
            "-n iterations:        how many times to expand each dataset\n"
            "-r rows:	       `rows` rows per chunk\n"
            "-s datasets:          number of datasets to create\n"
            "-u ms:                milliseconds interval between updates\n"
            "                      to %s.h5\n"
            "\n",
            progname, progname);
    exit(EXIT_FAILURE);
}

static void
make_quadrant_dataspace(state_t *s, quadrant_t *q)
{
    q->space = H5Screate_simple(NELMTS(s->chunk_dims), s->chunk_dims,
                                s->two_dee ? two_dee_max_dims : s->one_dee_max_dims);

    if (q->space < 0) {
        errx(EXIT_FAILURE, "%s.%d: H5Screate_simple failed", __func__, __LINE__);
    }

    if (H5Sselect_hyperslab(q->space, H5S_SELECT_SET, q->start, q->stride, q->count, q->block) < 0)
        errx(EXIT_FAILURE, "%s: H5Sselect_hyperslab failed", __func__);
}

static void
state_init(state_t *s, int argc, char **argv)
{
    unsigned long     tmp;
    int               ch;
    unsigned          i;
    const hsize_t     dims = 1;
    char              tfile[PATH_MAX];
    char *            end;
    unsigned long     millis;
    quadrant_t *const ul = &s->quadrants.ul, *const ur = &s->quadrants.ur, *const bl = &s->quadrants.bl,
                      *const br = &s->quadrants.br, *const src = &s->quadrants.src;
    const char *personality;

    *s = state_initializer();
    esnprintf(tfile, sizeof(tfile), "%s", argv[0]);
    esnprintf(s->progname, sizeof(s->progname), "%s", basename(tfile));

    while ((ch = getopt(argc, argv, "FMSVWa:bc:d:n:qr:s:u:")) != -1) {
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
            case 'W':
                s->wait_for_signal = false;
                break;
            case 'd':
                if (strcmp(optarg, "1") == 0 || strcmp(optarg, "one") == 0)
                    s->two_dee = false;
                else if (strcmp(optarg, "2") == 0 || strcmp(optarg, "two") == 0 ||
                         strcmp(optarg, "both") == 0)
                    s->two_dee = true;
                else {
                    errx(EXIT_FAILURE, "bad -d argument \"%s\"", optarg);
                }
                break;
            case 'a':
            case 'c':
            case 'n':
            case 'r':
            case 's':
                errno = 0;
                tmp   = strtoul(optarg, &end, 0);
                if (end == optarg || *end != '\0') {
                    errx(EXIT_FAILURE, "couldn't parse `-%c` argument `%s`", ch, optarg);
                }
                else if (errno != 0) {
                    err(EXIT_FAILURE, "couldn't parse `-%c` argument `%s`", ch, optarg);
                }
                else if (tmp > UINT_MAX)
                    errx(EXIT_FAILURE, "`-%c` argument `%lu` too large", ch, tmp);

                if ((ch == 'c' || ch == 'r') && tmp == 0) {
                    errx(EXIT_FAILURE, "`-%c` argument `%lu` must be >= 1", ch, tmp);
                }

                if (ch == 'a')
                    s->asteps = (unsigned)tmp;
                else if (ch == 'c')
                    s->cols = (unsigned)tmp;
                else if (ch == 'n')
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
            case 'u':
                errno  = 0;
                millis = strtoul(optarg, &end, 0);
                if (millis == ULONG_MAX && errno == ERANGE) {
                    err(EXIT_FAILURE, "option -p argument \"%s\"", optarg);
                }
                else if (*end != '\0') {
                    errx(EXIT_FAILURE, "garbage after -p argument \"%s\"", optarg);
                }
                s->update_interval.tv_sec  = (time_t)(millis / 1000UL);
                s->update_interval.tv_nsec = (long)((millis * 1000000UL) % 1000000000UL);
                break;
            case '?':
            default:
                usage(s->progname);
                break;
        }
    }
    argc -= optind;
    argv += optind;

    if (argc > 0)
        errx(EXIT_FAILURE, "unexpected command-line arguments");

    if (s->vds != vds_off && s->two_dee) {
        errx(EXIT_FAILURE, "virtual datasets and 2D datasets are mutually exclusive");
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
            errx(EXIT_FAILURE, "%s.%d: H5Pcreate failed", __func__, __LINE__);
        }

        if (H5Pset_chunk(s->quadrant_dcpl, RANK, half_chunk_dims) < 0)
            errx(EXIT_FAILURE, "H5Pset_chunk failed");

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

        make_quadrant_dataspace(s, ul);
        make_quadrant_dataspace(s, ur);
        make_quadrant_dataspace(s, bl);
        make_quadrant_dataspace(s, br);

        *src = (quadrant_t){.start  = {0, 0},
                            .stride = {s->rows / 2, s->cols / 2},
                            .block  = {s->rows / 2, s->cols / 2},
                            .count  = {1, H5S_UNLIMITED}};

        src->space = H5Screate_simple(RANK, half_chunk_dims, half_max_dims);

        if (src->space < 0) {
            errx(EXIT_FAILURE, "%s.%d: H5Screate_simple failed", __func__, __LINE__);
        }

        if (H5Sselect_hyperslab(src->space, H5S_SELECT_SET, src->start, src->stride, src->count, src->block) <
            0)
            errx(EXIT_FAILURE, "%s: H5Sselect_hyperslab failed", __func__);

        ul->src_space = H5Screate_simple(RANK, half_chunk_dims, half_max_dims);

        if (ul->src_space < 0) {
            errx(EXIT_FAILURE, "%s.%d: H5Screate_simple failed", __func__, __LINE__);
        }

        ur->src_space = H5Screate_simple(RANK, half_chunk_dims, half_max_dims);

        if (ur->src_space < 0) {
            errx(EXIT_FAILURE, "%s.%d: H5Screate_simple failed", __func__, __LINE__);
        }

        bl->src_space = H5Screate_simple(RANK, half_chunk_dims, half_max_dims);

        if (bl->src_space < 0) {
            errx(EXIT_FAILURE, "%s.%d: H5Screate_simple failed", __func__, __LINE__);
        }

        br->src_space = H5Screate_simple(RANK, half_chunk_dims, half_max_dims);

        if (br->src_space < 0) {
            errx(EXIT_FAILURE, "%s.%d: H5Screate_simple failed", __func__, __LINE__);
        }
    }

    /* space for attributes */
    if ((s->one_by_one_sid = H5Screate_simple(1, &dims, &dims)) < 0)
        errx(EXIT_FAILURE, "H5Screate_simple failed");

    s->dataset = malloc(sizeof(*s->dataset) * s->ndatasets);
    if (s->dataset == NULL)
        err(EXIT_FAILURE, "could not allocate dataset handles");

    s->sources = malloc(sizeof(*s->sources) * s->ndatasets);
    if (s->sources == NULL)
        err(EXIT_FAILURE, "could not allocate quadrant dataset handles");

    for (i = 0; i < s->ndatasets; i++) {
        s->dataset[i]    = badhid;
        s->sources[i].ul = s->sources[i].ur = s->sources[i].bl = s->sources[i].br = badhid;
    }

    s->memspace = H5Screate_simple(RANK, s->chunk_dims, NULL);

    if (s->memspace < 0) {
        errx(EXIT_FAILURE, "%s.%d: H5Screate_simple failed", __func__, __LINE__);
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

    personality = strstr(s->progname, "vfd_swmr_bigset_");

    if (personality != NULL && strcmp(personality, "vfd_swmr_bigset_writer") == 0)
        s->writer = true;
    else if (personality != NULL && strcmp(personality, "vfd_swmr_bigset_reader") == 0)
        s->writer = false;
    else {
        errx(EXIT_FAILURE, "unknown personality, expected vfd_swmr_bigset_{reader,writer}");
    }

    if ((s->dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
        errx(EXIT_FAILURE, "%s.%d: H5Pcreate failed", __func__, __LINE__);

    if (H5Pset_chunk_cache(s->dapl, 0, 0, H5D_CHUNK_CACHE_W0_DEFAULT) < 0)
        errx(EXIT_FAILURE, "H5Pset_chunk_cache failed");

    if (s->vds != vds_off && H5Pset_virtual_view(s->dapl, H5D_VDS_FIRST_MISSING) < 0)
        errx(EXIT_FAILURE, "H5Pset_virtual_view failed");
}

static void
state_destroy(state_t *s)
{
    size_t i;

    if (H5Pclose(s->dapl) < 0)
        errx(EXIT_FAILURE, "H5Pclose(fapl)");

    s->dapl = badhid;

    if (s->vds != vds_off) {
        quadrant_t *const ul = &s->quadrants.ul, *const ur = &s->quadrants.ur, *const bl = &s->quadrants.bl,
                          *const br = &s->quadrants.br;

        if (H5Sclose(ul->src_space) < 0 || H5Sclose(ur->src_space) < 0 || H5Sclose(bl->src_space) < 0 ||
            H5Sclose(br->src_space) < 0)
            errx(EXIT_FAILURE, "H5Sclose failed");

        ul->src_space = ur->src_space = bl->src_space = br->src_space = badhid;

        if (H5Pclose(s->quadrant_dcpl) < 0)
            errx(EXIT_FAILURE, "H5Pclose(dcpl)");

        s->quadrant_dcpl = badhid;

        /* TBD destroy spaces belonging to quadrants */
    }

    for (i = 0; i < NELMTS(s->file); i++) {
        hid_t fid = s->file[i];

        s->file[i] = badhid;

        if (s->vds != vds_multi && i > 0)
            continue;

        if (H5Fclose(fid) < 0)
            errx(EXIT_FAILURE, "H5Fclose");
    }
}

static void
create_extensible_dset(state_t *s, unsigned int which)
{
    quadrant_t *const ul = &s->quadrants.ul, *const ur = &s->quadrants.ur, *const bl = &s->quadrants.bl,
                      *const br = &s->quadrants.br, *const src = &s->quadrants.src;
    char dname[sizeof("/dataset-9999999999")];
    char ul_dname[sizeof("/ul-dataset-9999999999")], ur_dname[sizeof("/ur-dataset-9999999999")],
        bl_dname[sizeof("/bl-dataset-9999999999")], br_dname[sizeof("/br-dataset-9999999999")];
    hid_t dcpl, ds, filespace;

    assert(which < s->ndatasets);
    assert(s->dataset[which] == badhid);

    esnprintf(dname, sizeof(dname), "/dataset-%d", which);

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
        errx(EXIT_FAILURE, "%s.%d: H5Pcreate failed", __func__, __LINE__);
    }

    if (H5Pset_chunk(dcpl, RANK, s->chunk_dims) < 0)
        errx(EXIT_FAILURE, "H5Pset_chunk failed");

    if (s->vds != vds_off) {
        sources_t *const srcs = &s->sources[which];

        esnprintf(ul_dname, sizeof(ul_dname), "/ul-dataset-%d", which);
        esnprintf(ur_dname, sizeof(ur_dname), "/ur-dataset-%d", which);
        esnprintf(bl_dname, sizeof(bl_dname), "/bl-dataset-%d", which);
        esnprintf(br_dname, sizeof(br_dname), "/br-dataset-%d", which);

        srcs->ul = H5Dcreate2(s->file[0], ul_dname, s->filetype, ul->src_space, H5P_DEFAULT, s->quadrant_dcpl,
                              s->dapl);

        if (srcs->ul < 0)
            errx(EXIT_FAILURE, "H5Dcreate(, \"%s\", ) failed", ul_dname);

        srcs->ur = H5Dcreate2(s->file[1], ur_dname, s->filetype, ur->src_space, H5P_DEFAULT, s->quadrant_dcpl,
                              s->dapl);

        if (srcs->ur < 0)
            errx(EXIT_FAILURE, "H5Dcreate(, \"%s\", ) failed", ur_dname);

        srcs->bl = H5Dcreate2(s->file[2], bl_dname, s->filetype, bl->src_space, H5P_DEFAULT, s->quadrant_dcpl,
                              s->dapl);

        if (srcs->bl < 0)
            errx(EXIT_FAILURE, "H5Dcreate(, \"%s\", ) failed", bl_dname);

        srcs->br = H5Dcreate2(s->file[3], br_dname, s->filetype, br->src_space, H5P_DEFAULT, s->quadrant_dcpl,
                              s->dapl);

        if (srcs->br < 0)
            errx(EXIT_FAILURE, "H5Dcreate(, \"%s\", ) failed", br_dname);

        if (H5Pset_virtual(dcpl, ul->space, s->filename[0], ul_dname, src->space) < 0)
            errx(EXIT_FAILURE, "%s: H5Pset_virtual failed", __func__);

        if (H5Pset_virtual(dcpl, ur->space, s->filename[1], ur_dname, src->space) < 0)
            errx(EXIT_FAILURE, "%s: H5Pset_virtual failed", __func__);

        if (H5Pset_virtual(dcpl, bl->space, s->filename[2], bl_dname, src->space) < 0)
            errx(EXIT_FAILURE, "%s: H5Pset_virtual failed", __func__);

        if (H5Pset_virtual(dcpl, br->space, s->filename[3], br_dname, src->space) < 0)
            errx(EXIT_FAILURE, "%s: H5Pset_virtual failed", __func__);
    }

    filespace = H5Screate_simple(NELMTS(s->chunk_dims), s->chunk_dims,
                                 s->two_dee ? two_dee_max_dims : s->one_dee_max_dims);

    if (filespace < 0) {
        errx(EXIT_FAILURE, "%s.%d: H5Screate_simple failed", __func__, __LINE__);
    }

    ds = H5Dcreate2(s->file[0], dname, s->filetype, filespace, H5P_DEFAULT, dcpl, s->dapl);

    if (ds < 0)
        errx(EXIT_FAILURE, "H5Dcreate(, \"%s\", ) failed", dname);

    if (H5Sclose(filespace) < 0)
        errx(EXIT_FAILURE, "%s: H5Sclose failed", __func__);

    if (H5Pclose(dcpl) < 0)
        errx(EXIT_FAILURE, "%s: H5Pclose failed", __func__);

    s->dataset[which] = ds;
}

static void
close_extensible_dset(state_t *s, unsigned int which)
{
    char  dname[sizeof("/dataset-9999999999")];
    hid_t ds;

    assert(which < s->ndatasets);

    esnprintf(dname, sizeof(dname), "/dataset-%d", which);

    ds = s->dataset[which];

    if (H5Dclose(ds) < 0)
        errx(EXIT_FAILURE, "H5Dclose failed for \"%s\"", dname);

    s->dataset[which] = badhid;

    if (s->vds != vds_off && s->writer) {
        sources_t *const srcs = &s->sources[which];

        if (H5Dclose(srcs->ul) < 0 || H5Dclose(srcs->ur) < 0 || H5Dclose(srcs->bl) < 0 ||
            H5Dclose(srcs->br) < 0)
            errx(EXIT_FAILURE, "H5Dclose failed");

        srcs->ul = srcs->ur = srcs->bl = srcs->br = badhid;
    }
}

static void
open_extensible_dset(state_t *s, unsigned int which)
{
    hsize_t                      dims[RANK], maxdims[RANK];
    char                         dname[sizeof("/dataset-9999999999")];
    hid_t                        ds, filespace, ty;
    const int                    tries = 10000;
    int                          i;
    struct timespec              last = {0, 0};
    static const struct timespec ival = {5, 0};
    estack_state_t               es;

    assert(which < s->ndatasets);
    assert(s->dataset[which] == badhid);

    esnprintf(dname, sizeof(dname), "/dataset-%d", which);

    es = disable_estack();
    for (i = 0; i < tries; i++) {
        struct timespec one_tenth = {.tv_sec = 0, .tv_nsec = 1000000000L / 10};

        ds = H5Dopen(s->file[0], dname, s->dapl);

        if (ds >= 0)
            break;

        if (below_speed_limit(&last, &ival)) {
            warnx("H5Dopen(, \"%s\", ) transient failure, %d retries remain", dname, tries - i - 1);
        }
        while (nanosleep(&one_tenth, &one_tenth) == -1 && errno == EINTR)
            ; // do nothing
    }
    restore_estack(es);

    if (i == tries) {
        errx(EXIT_FAILURE, "H5Dopen(, \"%s\", ) failed after %d tries", dname, tries);
    }

    if ((ty = H5Dget_type(ds)) < 0)
        errx(EXIT_FAILURE, "H5Dget_type failed");

    if (H5Tequal(ty, s->filetype) <= 0)
        errx(EXIT_FAILURE, "Unexpected data type");

    if ((filespace = H5Dget_space(ds)) < 0)
        errx(EXIT_FAILURE, "H5Dget_space failed");

    if (H5Sget_simple_extent_ndims(filespace) != RANK)
        errx(EXIT_FAILURE, "Unexpected rank");

    if (H5Sget_simple_extent_dims(filespace, dims, maxdims) < 0)
        errx(EXIT_FAILURE, "H5Sget_simple_extent_dims failed");

    if (H5Sclose(filespace) < 0)
        errx(EXIT_FAILURE, "H5Sclose failed");

    filespace = badhid;

    if (s->two_dee) {
        if (maxdims[0] != two_dee_max_dims[0] || maxdims[1] != two_dee_max_dims[1] ||
            maxdims[0] != maxdims[1]) {
            errx(EXIT_FAILURE, "Unexpected maximum dimensions %" PRIuHSIZE " x %" PRIuHSIZE, maxdims[0],
                 maxdims[1]);
        }
    }
    else if (maxdims[0] != s->one_dee_max_dims[0] || maxdims[1] != s->one_dee_max_dims[1] ||
             dims[0] != s->chunk_dims[0]) {
        errx(EXIT_FAILURE,
             "Unexpected maximum dimensions %" PRIuHSIZE " x %" PRIuHSIZE " or columns %" PRIuHSIZE,
             maxdims[0], maxdims[1], dims[1]);
    }

    s->dataset[which] = ds;
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
static void
set_or_verify_matrix(mat_t *mat, unsigned int which, base_t base, bool do_set)
{
    unsigned row, col;

    for (row = 0; row < mat->rows; row++) {
        for (col = 0; col < mat->cols; col++) {
            uint32_t v;
            hsize_t  i = base.row + row, j = base.col + col, u;

            if (j <= i)
                u = (i + 1) * (i + 1) - 1 - j;
            else
                u = j * j + i;

            assert(UINT32_MAX - u >= which);
            v = (uint32_t)(u + which);
            if (do_set)
                matset(mat, row, col, v);
            else if (matget(mat, row, col) != v) {
                errx(EXIT_FAILURE,
                     "matrix mismatch "
                     "at %" PRIuHSIZE ", %" PRIuHSIZE " (%u, %u), "
                     "read %" PRIu32 " expecting %" PRIu32,
                     i, j, row, col, matget(mat, row, col), v);
            }
        }
    }
}

static void
init_matrix(mat_t *mat, unsigned int which, base_t base)
{
    set_or_verify_matrix(mat, which, base, true);
}

static void
verify_matrix(mat_t *mat, unsigned int which, base_t base)
{
    set_or_verify_matrix(mat, which, base, false);
}

static void
verify_chunk(state_t *s, hid_t filespace, mat_t *mat, unsigned which, base_t base)
{
    hsize_t offset[RANK] = {base.row, base.col};
    herr_t  status;
    hid_t   ds;

    assert(which < s->ndatasets);

    dbgf(1, "verifying chunk %" PRIuHSIZE ", %" PRIuHSIZE "\n", base.row, base.col);

    ds = s->dataset[which];

    status = H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, NULL, s->chunk_dims, NULL);

    if (status < 0)
        errx(EXIT_FAILURE, "H5Sselect_hyperslab failed");

    status = H5Dread(ds, H5T_NATIVE_UINT32, s->memspace, filespace, H5P_DEFAULT, mat->elt);

    if (status < 0)
        errx(EXIT_FAILURE, "H5Dread failed");

    verify_matrix(mat, which, base);
}

static void
init_and_write_chunk(state_t *s, hid_t filespace, mat_t *mat, unsigned which, base_t base)
{
    hsize_t offset[RANK] = {base.row, base.col};
    herr_t  status;
    hid_t   ds;

    assert(which < s->ndatasets);

    ds = s->dataset[which];

    init_matrix(mat, which, base);

    status = H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, NULL, s->chunk_dims, NULL);

    if (status < 0)
        errx(EXIT_FAILURE, "H5Sselect_hyperslab failed");

    status = H5Dwrite(ds, H5T_NATIVE_UINT32, s->memspace, filespace, H5P_DEFAULT, mat->elt);

    if (status < 0)
        errx(EXIT_FAILURE, "H5Dwrite failed");
}

static void
verify_dset_attribute(hid_t ds, unsigned int which, unsigned int step)
{
    unsigned int read_step;
    hid_t        aid;
    char         name[sizeof("attr-9999999999")];

    esnprintf(name, sizeof(name), "attr-%u", step);

    dbgf(1, "verifying attribute %s on dataset %u equals %u\n", name, which, step);

    if ((aid = H5Aopen(ds, name, H5P_DEFAULT)) < 0)
        errx(EXIT_FAILURE, "H5Aopen failed");

    if (H5Aread(aid, H5T_NATIVE_UINT, &read_step) < 0)
        errx(EXIT_FAILURE, "H5Aread failed");

    if (H5Aclose(aid) < 0)
        errx(EXIT_FAILURE, "H5Aclose failed");

    if (read_step != step)
        errx(EXIT_FAILURE, "expected %u read %u", step, read_step);
}

static void
verify_extensible_dset(state_t *s, unsigned int which, mat_t *mat, unsigned *stepp)
{
    hid_t        ds, filespace;
    hsize_t      size[RANK];
    base_t       base, last;
    unsigned int ncols, last_step, step;

    assert(which < s->ndatasets);

    ds = s->dataset[which];

    if (H5Drefresh(ds) < 0)
        errx(EXIT_FAILURE, "H5Drefresh failed");

    filespace = H5Dget_space(ds);

    if (filespace == badhid)
        errx(EXIT_FAILURE, "H5Dget_space failed");

    if (H5Sget_simple_extent_dims(filespace, size, NULL) < 0)
        errx(EXIT_FAILURE, "H5Sget_simple_extent_dims failed");

    ncols = (unsigned)(size[1] / s->chunk_dims[1]);
    if (ncols < hang_back)
        goto out;

    last_step = ncols - hang_back;

    for (step = *stepp; step <= last_step; step++) {
        const unsigned ofs = step % 2;

        dbgf(1, "%s: which %u step %u\n", __func__, which, step);

        if (s->two_dee) {
            size[0]  = s->chunk_dims[0] * (1 + step);
            size[1]  = s->chunk_dims[1] * (1 + step);
            last.row = s->chunk_dims[0] * step + ofs;
            last.col = s->chunk_dims[1] * step + ofs;
        }
        else {
            size[0]  = s->chunk_dims[0];
            size[1]  = s->chunk_dims[1] * (1 + step);
            last.row = 0;
            last.col = s->chunk_dims[1] * step + ofs;
        }

        dbgf(1, "new size %" PRIuHSIZE ", %" PRIuHSIZE "\n", size[0], size[1]);
        dbgf(1, "last row %" PRIuHSIZE " col %" PRIuHSIZE "\n", last.row, last.col);

        if (s->two_dee) {

            /* Down the right side, intersecting the bottom row. */
            base.col = last.col;
            for (base.row = ofs; base.row <= last.row; base.row += s->chunk_dims[0]) {
                verify_chunk(s, filespace, mat, which, base);
            }

            /* Across the bottom, stopping before the last column to
             * avoid re-reading the bottom-right chunk.
             */
            base.row = last.row;
            for (base.col = ofs; base.col < last.col; base.col += s->chunk_dims[1]) {
                verify_chunk(s, filespace, mat, which, base);
            }
        }
        else {
            verify_chunk(s, filespace, mat, which, last);
        }
        if (s->asteps != 0 && step % s->asteps == 0)
            verify_dset_attribute(ds, which, step);
    }

    *stepp = step;

out:
    if (H5Sclose(filespace) < 0)
        errx(EXIT_FAILURE, "H5Sclose failed");
}

static void
add_dset_attribute(const state_t *s, hid_t ds, hid_t sid, unsigned int which, unsigned int step)
{
    hid_t aid;
    char  name[sizeof("attr-9999999999")];

    esnprintf(name, sizeof(name), "attr-%u", step);

    dbgf(1, "setting attribute %s on dataset %u to %u\n", name, which, step);

    if ((aid = H5Acreate2(ds, name, s->filetype, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        errx(EXIT_FAILURE, "H5Acreate2 failed");

    if (H5Awrite(aid, H5T_NATIVE_UINT, &step) < 0)
        errx(EXIT_FAILURE, "H5Awrite failed");
    if (H5Aclose(aid) < 0)
        errx(EXIT_FAILURE, "H5Aclose failed");
}

static void
write_extensible_dset(state_t *s, unsigned int which, unsigned int step, mat_t *mat)
{
    hid_t   ds, filespace;
    hsize_t size[RANK];
    base_t  base, last;

    dbgf(1, "%s: which %u step %u\n", __func__, which, step);

    assert(which < s->ndatasets);

    ds = s->dataset[which];

    if (s->asteps != 0 && step % s->asteps == 0)
        add_dset_attribute(s, ds, s->one_by_one_sid, which, step);

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
            errx(EXIT_FAILURE, "%s.%d: H5Dset_extent failed", __func__, __LINE__);
        }
        if (H5Dset_extent(srcs->ur, half_size) < 0) {
            errx(EXIT_FAILURE, "%s.%d: H5Dset_extent failed", __func__, __LINE__);
        }
        if (H5Dset_extent(srcs->bl, half_size) < 0) {
            errx(EXIT_FAILURE, "%s.%d: H5Dset_extent failed", __func__, __LINE__);
        }
        if (H5Dset_extent(srcs->br, half_size) < 0) {
            errx(EXIT_FAILURE, "%s.%d: H5Dset_extent failed", __func__, __LINE__);
        }
    }
    else if (H5Dset_extent(ds, size) < 0)
        errx(EXIT_FAILURE, "H5Dset_extent failed");

    filespace = H5Dget_space(ds);

    if (filespace == badhid)
        errx(EXIT_FAILURE, "H5Dget_space failed");

    if (s->two_dee) {
        base.col = last.col;
        for (base.row = 0; base.row <= last.row; base.row += s->chunk_dims[0]) {
            dbgf(1, "writing chunk %" PRIuHSIZE ", %" PRIuHSIZE "\n", base.row, base.col);
            init_and_write_chunk(s, filespace, mat, which, base);
        }

        base.row = last.row;
        for (base.col = 0; base.col < last.col; base.col += s->chunk_dims[1]) {
            dbgf(1, "writing chunk %" PRIuHSIZE ", %" PRIuHSIZE "\n", base.row, base.col);
            init_and_write_chunk(s, filespace, mat, which, base);
        }
    }
    else {
        init_and_write_chunk(s, filespace, mat, which, last);
    }

    if (H5Sclose(filespace) < 0)
        errx(EXIT_FAILURE, "H5Sclose failed");
}

int
main(int argc, char **argv)
{
    mat_t *  mat;
    hid_t    fcpl;
    sigset_t oldsigs;
    herr_t   ret;
    unsigned step, which;
    state_t  s;
    size_t   i;

    state_init(&s, argc, argv);

    if ((mat = newmat(s.rows, s.cols)) == NULL)
        err(EXIT_FAILURE, "%s: could not allocate matrix", __func__);

    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        errx(EXIT_FAILURE, "H5Pcreate");

    ret = H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, false, 1);
    if (ret < 0)
        errx(EXIT_FAILURE, "H5Pset_file_space_strategy");

    for (i = 0; i < NELMTS(s.file); i++) {
        hid_t                 fapl;
        H5F_vfd_swmr_config_t config;

        if (s.vds != vds_multi && i > 0) {
            s.file[i] = s.file[0];
            continue;
        }

        /* config, tick_len, max_lag, writer, flush_raw_data, md_pages_reserved, md_file_path */
        init_vfd_swmr_config(&config, 4, 7, s.writer, FALSE, 128, "./bigset-shadow-%zu", i);

        /* use_latest_format, use_vfd_swmr, only_meta_page, config */
        fapl = vfd_swmr_create_fapl(true, s.use_vfd_swmr, true, &config);

        if (fapl < 0)
            errx(EXIT_FAILURE, "vfd_swmr_create_fapl");

        s.file[i] = s.writer ? H5Fcreate(s.filename[i], H5F_ACC_TRUNC, fcpl, fapl)
                             : H5Fopen(s.filename[i], H5F_ACC_RDONLY, fapl);

        if (s.file[i] == badhid)
            errx(EXIT_FAILURE, s.writer ? "H5Fcreate" : "H5Fopen");

        if (H5Pclose(fapl) < 0)
            errx(EXIT_FAILURE, "H5Pclose(fapl)");
    }

    block_signals(&oldsigs);

    if (s.writer) {
        for (which = 0; which < s.ndatasets; which++)
            create_extensible_dset(&s, which);

        for (step = 0; step < s.nsteps; step++) {
            for (which = 0; which < s.ndatasets; which++) {
                dbgf(2, "step %d which %d\n", step, which);
                write_extensible_dset(&s, which, step, mat);
                if (s.ndatasets <= s.nsteps)
                    nanosleep(&s.update_interval, NULL);
            }
            if (s.ndatasets > s.nsteps)
                nanosleep(&s.update_interval, NULL);
        }
    }
    else {
        unsigned *nextstep = calloc(s.ndatasets, sizeof(*nextstep));
        unsigned  finished_step;

        if (nextstep == NULL)
            err(EXIT_FAILURE, "could not allocate `nextstep` array");

        for (which = s.ndatasets; which > 0; which--)
            open_extensible_dset(&s, which - 1);

        do {
            finished_step = UINT_MAX; /* the greatest step finished on
                                       * *all* datasets
                                       */

            for (which = s.ndatasets; which-- > 0;) {
                dbgf(2, "step %d which %d\n", nextstep[which], which);
                verify_extensible_dset(&s, which, mat, &nextstep[which]);
                if (nextstep[which] < finished_step)
                    finished_step = nextstep[which];
                if (s.ndatasets <= s.nsteps)
                    nanosleep(&s.update_interval, NULL);
            }
            if (s.ndatasets > s.nsteps)
                nanosleep(&s.update_interval, NULL);
        } while (hang_back + finished_step < s.nsteps);

        free(nextstep);
    }

    for (which = 0; which < s.ndatasets; which++)
        close_extensible_dset(&s, which);

    if (s.use_vfd_swmr && s.wait_for_signal)
        await_signal(s.file[0]);

    restore_signals(&oldsigs);

    if (H5Pclose(fcpl) < 0)
        errx(EXIT_FAILURE, "H5Pclose(fcpl)");

    state_destroy(&s);

    free(mat);

    return EXIT_SUCCESS;
}

#else /* H5_HAVE_WIN32_API */

int
main(void)
{
    HDfprintf(stderr, "Non-POSIX platform. Skipping.\n");
    return EXIT_SUCCESS;
} /* end main() */

#endif /* H5_HAVE_WIN32_API */
