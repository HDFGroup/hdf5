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

#include <err.h>
#include <libgen.h>
#include <time.h> /* nanosleep(2) */
#include <unistd.h> /* getopt(3) */

#define H5C_FRIEND              /*suppress error about including H5Cpkg   */
#define H5F_FRIEND              /*suppress error about including H5Fpkg   */

#include "hdf5.h"

#include "H5Cpkg.h"
#include "H5Fpkg.h"
// #include "H5Iprivate.h"
#include "H5HGprivate.h"
#include "H5VLprivate.h"

#include "testhdf5.h"
#include "vfd_swmr_common.h"

#define ROWS 256
#define COLS 512
#define RANK 2

typedef struct _base {
    hsize_t row, col;
} base_t;

typedef struct {
	/* main-loop statistics */
	uint64_t max_elapsed_ns, min_elapsed_ns, total_elapsed_ns;
	uint64_t total_loops;
	hid_t *dataset, memspace, file;
        unsigned ndatasets;
	char filename[PATH_MAX];
	char progname[PATH_MAX];
	struct timespec update_interval;
	bool fuzz;
	bool constantrate;
	unsigned int nsteps;
        bool two_dee;
        bool wait_for_signal;
        bool use_vfd_swmr;
} state_t;

#define ALL_HID_INITIALIZER (state_t){					\
	  .total_elapsed_ns = 0						\
	, .total_loops = 0						\
	, .min_elapsed_ns = UINT64_MAX					\
	, .max_elapsed_ns = 0						\
	, .memspace = H5I_INVALID_HID					\
	, .file = H5I_INVALID_HID					\
	, .constantrate = false						\
        , .ndatasets = 5                                                \
	, .nsteps = 100							\
	, .filename = ""						\
        , .two_dee = false                                              \
        , .wait_for_signal = true                                       \
        , .use_vfd_swmr = true                                          \
	, .update_interval = (struct timespec){				\
		  .tv_sec = 0						\
		, .tv_nsec = 1000000000UL / 30 /* 1/30 second */}}

static void state_init(state_t *, int, char **);

static const hid_t badhid = H5I_INVALID_HID;

static const hsize_t original_dims[RANK] = {ROWS, COLS};
static const hsize_t one_dee_max_dims[RANK] =
    {H5S_UNLIMITED, COLS};
static const hsize_t two_dee_max_dims[RANK] =
    {H5S_UNLIMITED, H5S_UNLIMITED};
static const hsize_t *chunk_dims = original_dims;

static void
usage(const char *progname)
{
	fprintf(stderr, "usage: %s [-c] [-d] [-u milliseconds]\n"
		"\n"
		"-S:	               do not use VFD SWMR\n"
		"-W:	               do not wait for a signal before\n"
                "                      exiting\n"
		"-c:	               increase the frame number continously\n"
                "                      (reader mode)\n"
		"-d 1|one|2|two|both:  select dataset expansion in one or\n"
                "                      both dimensions\n"
		"-n iterations:        how many times to expand each dataset\n"
		"-s datasets:          number of datasets to create\n"
		"-u ms:                milliseconds interval between updates\n"
                "                      to %s.h5\n"
		"\n",
		progname, progname);
	exit(EXIT_FAILURE);
}

static void
state_init(state_t *s, int argc, char **argv)
{
    unsigned long tmp;
    int ch;
    unsigned i;
    char tfile[PATH_MAX];
    char *end;
    unsigned long millis;

    *s = ALL_HID_INITIALIZER;
    esnprintf(tfile, sizeof(tfile), "%s", argv[0]);
    esnprintf(s->progname, sizeof(s->progname), "%s", basename(tfile));

    while ((ch = getopt(argc, argv, "SWcd:n:s:qu:")) != -1) {
        switch (ch) {
        case 'S':
            s->use_vfd_swmr = false;
            break;
        case 'W':
            s->wait_for_signal = false;
            break;
        case 'c':
            s->constantrate = true;
            break;
        case 'd':
            if (strcmp(optarg, "1") == 0 ||
                strcmp(optarg, "one") == 0)
                s->two_dee = false;
            else if (strcmp(optarg, "2") == 0 ||
                     strcmp(optarg, "two") == 0 ||
                     strcmp(optarg, "both") == 0)
                s->two_dee = true;
            else {
                    errx(EXIT_FAILURE,
                        "bad -d argument \"%s\"", optarg);
            }
            break;
        case 'n':
        case 's':
            errno = 0;
            tmp = strtoul(optarg, &end, 0);
            if (end == optarg || *end != '\0')
                errx(EXIT_FAILURE, "couldn't parse `-n` argument `%s`", optarg);
            else if (errno != 0)
                err(EXIT_FAILURE, "couldn't parse `-n` argument `%s`", optarg);
            else if (tmp > UINT_MAX)
                errx(EXIT_FAILURE, "`-n` argument `%lu` too large", tmp);
            if (ch == 'n')
                s->nsteps = (unsigned)tmp;
            else
                s->ndatasets = (unsigned)tmp;
            break;
        case 'q':
            verbosity = 1;
            break;
        case 'u':
            errno = 0;
            millis = strtoul(optarg, &end, 0);
            if (millis == ULONG_MAX && errno == ERANGE) {
                    err(EXIT_FAILURE,
                        "option -p argument \"%s\"", optarg);
            } else if (*end != '\0') {
                    errx(EXIT_FAILURE,
                        "garbage after -p argument \"%s\"", optarg);
            }
            s->update_interval.tv_sec = millis / 1000UL;
            s->update_interval.tv_nsec =
                (long)((millis * 1000000UL) % 1000000000UL);
            dbgf(1, "%lu milliseconds between updates", millis);
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

    s->dataset = malloc(sizeof(*s->dataset) * s->ndatasets);
    if (s->dataset == NULL)
        err(EXIT_FAILURE, "could not allocate dataset handles");

    for (i = 0; i < s->ndatasets; i++)
        s->dataset[i] = badhid;

    s->memspace = H5Screate_simple(RANK, chunk_dims, NULL);

    if (s->memspace < 0) {
            errx(EXIT_FAILURE, "%s.%d: H5Screate_simple failed",
                __func__, __LINE__);
    }

    esnprintf(s->filename, sizeof(s->filename), "vfd_swmr_bigset.h5");
}

static void
create_extensible_dset(state_t *s, unsigned int which)
{
    char dname[sizeof("/dataset-10")];
    hid_t dcpl, ds, filespace;

    assert(which <= 99);
    assert(which < s->ndatasets);

    esnprintf(dname, sizeof(dname), "/dataset-%d", which);

    assert(s->dataset[which] == badhid);

    filespace = H5Screate_simple(NELMTS(original_dims), original_dims,
        s->two_dee ? two_dee_max_dims : one_dee_max_dims);

    if (filespace < 0) {
        errx(EXIT_FAILURE, "%s.%d: H5Screate_simple failed",
                __func__, __LINE__);
    }

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
        errx(EXIT_FAILURE, "%s.%d: H5Pcreate failed",
            __func__, __LINE__);
    }

    if (H5Pset_chunk(dcpl, RANK, chunk_dims) < 0)
        errx(EXIT_FAILURE, "H5Pset_chunk failed");

    ds = H5Dcreate2(s->file, dname, H5T_STD_U32BE, filespace,
        H5P_DEFAULT, dcpl, H5P_DEFAULT);

    if (H5Pclose(dcpl) < 0)
        errx(EXIT_FAILURE, "H5Pclose(dcpl)");

    if (H5Sclose(filespace) < 0)
        errx(EXIT_FAILURE, "H5Sclose failed");

    filespace = badhid;

    if (ds < 0)
        errx(EXIT_FAILURE, "H5Dcreate(, \"%s\", ) failed", dname);

    s->dataset[which] = ds;
}

static void
open_extensible_dset(state_t *s, unsigned int which)
{
    hsize_t dims[RANK], maxdims[RANK];
    char dname[sizeof("/dataset-10")];
    hid_t ds, filespace, ty;

    assert(which <= 99);
    assert(which < s->ndatasets);

    esnprintf(dname, sizeof(dname), "/dataset-%d", which);

    assert(s->dataset[which] == badhid);

    ds = H5Dopen(s->file, dname, H5P_DEFAULT);

    if (ds < 0)
        errx(EXIT_FAILURE, "H5Dopen(, \"%s\", ) failed", dname);

    if ((ty = H5Dget_type(ds)) < 0)
        errx(EXIT_FAILURE, "H5Dget_type failed");

    if (H5Tequal(ty, H5T_STD_U32BE) <= 0)
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
        if (maxdims[0] != two_dee_max_dims[0] ||
            maxdims[1] != two_dee_max_dims[1] ||
            maxdims[0] != maxdims[1]) {
                errx(EXIT_FAILURE, "Unexpected maximum dimensions %"
                    PRIuHSIZE " x %" PRIuHSIZE, maxdims[0], maxdims[1]);
        }
    } else if (maxdims[0] != one_dee_max_dims[0] ||
               maxdims[1] != one_dee_max_dims[1] ||
               dims[1] != original_dims[1]) {
        errx(EXIT_FAILURE, "Unexpected maximum dimensions %"
            PRIuHSIZE " x %" PRIuHSIZE " or columns %" PRIuHSIZE,
            maxdims[0], maxdims[1], dims[1]);
    }

    s->dataset[which] = ds;
}

static void
set_or_verify_matrix(uint32_t mat[ROWS][COLS], unsigned int which, base_t base,
    bool do_set)
{
    unsigned row, col;

    for (row = 0; row < ROWS; row++) {
        for (col = 0; col < COLS; col++) {
            uint32_t v;
            hsize_t i = base.row + row,
                    j = base.col + col,
                    u;

            if (j <= i)
                u = (i + 1) * (i + 1) - 1 - j;
            else
                u = j * j + i;

            assert(UINT32_MAX - u >= which);
            v = (uint32_t)(u + which);
            if (do_set)
                mat[row][col] = v;
            else if (mat[row][col] != v) {
                errx(EXIT_FAILURE, "matrix mismatch "
                    "at %" PRIuHSIZE ", %" PRIuHSIZE " (%u, %u), "
                    "read %" PRIu32 " expecting %" PRIu32,
                    i, j, row, col, mat[row][col], v);
            }
        }
    }
}

static void
init_matrix(uint32_t mat[ROWS][COLS], unsigned int which, base_t base)
{
    set_or_verify_matrix(mat, which, base, true);
}

static void
verify_matrix(uint32_t mat[ROWS][COLS], unsigned int which, base_t base)
{
    set_or_verify_matrix(mat, which, base, false);
}

static void
verify_chunk(hid_t ds, hid_t filespace, hid_t memspace,
    uint32_t mat[ROWS][COLS], unsigned which, base_t base)
{
    hsize_t offset[RANK] = {base.row, base.col};
    herr_t status;

    status = H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset,
        NULL, chunk_dims, NULL);

    if (status < 0)
        errx(EXIT_FAILURE, "H5Sselect_hyperslab failed");

    status = H5Dread(ds, H5T_NATIVE_UINT32, memspace, filespace,
        H5P_DEFAULT, mat);

    if (status < 0)
        errx(EXIT_FAILURE, "H5Dread failed");

    verify_matrix(mat, which, base);
}

static void
init_and_write_chunk(hid_t ds, hid_t filespace, hid_t memspace,
    uint32_t mat[ROWS][COLS], unsigned which, base_t base)
{
    hsize_t offset[RANK] = {base.row, base.col};
    herr_t status;

    init_matrix(mat, which, base);

    status = H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset,
        NULL, chunk_dims, NULL);

    if (status < 0)
        errx(EXIT_FAILURE, "H5Sselect_hyperslab failed");

    status = H5Dwrite(ds, H5T_NATIVE_UINT32, memspace, filespace,
        H5P_DEFAULT, mat);

    if (status < 0)
        errx(EXIT_FAILURE, "H5Dwrite failed");
}

static void
verify_extensible_dset(state_t *s, unsigned int which, uint32_t mat[ROWS][COLS],
    unsigned *stepp)
{
    hid_t ds, filespace;
    hsize_t size[RANK];
    base_t base, last;
    unsigned int nrows, last_step, step;

    assert(which < s->ndatasets);

    ds = s->dataset[which];

    if (H5Drefresh(ds) < 0)
        errx(EXIT_FAILURE, "H5Drefresh failed");

    filespace = H5Dget_space(ds);

    if (filespace == badhid)
        errx(EXIT_FAILURE, "H5Dget_space failed");

    if (H5Sget_simple_extent_dims(filespace, size, NULL) < 0)
        errx(EXIT_FAILURE, "H5Sget_simple_extent_dims failed");

    nrows = (unsigned)(size[0] / original_dims[0]);
    if (nrows < 2)
        goto out;

    last_step = nrows - 2;

    for (step = *stepp; step <= last_step; step++) {
        dbgf(1, "%s: which %u step %u\n", __func__, which, step);

        size[0] = original_dims[0] * (1 + step);
        last.row = original_dims[0] * step;

        if (s->two_dee) {
            size[1] = original_dims[1] * (1 + step);
            last.col = original_dims[1] * step;
        } else {
            size[1] = original_dims[1];
            last.col = 0;
        }

        dbgf(1, "new size %" PRIuHSIZE ", %" PRIuHSIZE "\n", size[0], size[1]);

        if (s->two_dee) {
            base.col = last.col;
            for (base.row = 0; base.row <= last.row; base.row += original_dims[0]) {
                dbgf(1, "verifying chunk %" PRIuHSIZE ", %" PRIuHSIZE "\n",
                    base.row, base.col);
                verify_chunk(ds, filespace, s->memspace, mat, which, base);
            }

            base.row = last.row;
            for (base.col = 0; base.col < last.col; base.col += original_dims[1]) {
                dbgf(1, "verifying chunk %" PRIuHSIZE ", %" PRIuHSIZE "\n",
                    base.row, base.col);
                verify_chunk(ds, filespace, s->memspace, mat, which, base);
            }
        } else {
            dbgf(1, "verifying chunk %" PRIuHSIZE ", %" PRIuHSIZE "\n",
                last.row, last.col);
            verify_chunk(ds, filespace, s->memspace, mat, which, last);
        }
    }

    *stepp = last_step;

out:
    if (H5Sclose(filespace) < 0)
        errx(EXIT_FAILURE, "H5Sclose failed");
}

static void
write_extensible_dset(state_t *s, unsigned int which, unsigned int step,
    uint32_t mat[ROWS][COLS])
{
    hid_t ds, filespace;
    hsize_t size[RANK];
    base_t base, last;

    dbgf(1, "%s: which %u step %u\n", __func__, which, step);

    assert(which < s->ndatasets);

    ds = s->dataset[which];

    size[0] = original_dims[0] * (1 + step);
    last.row = original_dims[0] * step;

    if (s->two_dee) {
        size[1] = original_dims[1] * (1 + step);
        last.col = original_dims[1] * step;
    } else {
        size[1] = original_dims[1];
        last.col = 0;
    }


    dbgf(1, "new size %" PRIuHSIZE ", %" PRIuHSIZE "\n", size[0], size[1]);

    if (H5Dset_extent(ds, size) < 0)
        errx(EXIT_FAILURE, "H5Dset_extent failed");

    filespace = H5Dget_space(ds);

    if (filespace == badhid)
        errx(EXIT_FAILURE, "H5Dget_space failed");

    if (s->two_dee) {
        base.col = last.col;
        for (base.row = 0; base.row <= last.row; base.row += original_dims[0]) {
            dbgf(1, "writing chunk %" PRIuHSIZE ", %" PRIuHSIZE "\n",
                base.row, base.col);
            init_and_write_chunk(ds, filespace, s->memspace, mat, which, base);
        }

        base.row = last.row;
        for (base.col = 0; base.col < last.col; base.col += original_dims[1]) {
            dbgf(1, "writing chunk %" PRIuHSIZE ", %" PRIuHSIZE "\n",
                base.row, base.col);
            init_and_write_chunk(ds, filespace, s->memspace, mat, which, base);
        }
    } else {
        init_and_write_chunk(ds, filespace, s->memspace, mat, which, last);
    }

    if (H5Sclose(filespace) < 0)
            errx(EXIT_FAILURE, "H5Sclose failed");
}

int
main(int argc, char **argv)
{
    struct {
        uint32_t mat[ROWS][COLS];
    } *onheap;
    hid_t fapl, fcpl;
    sigset_t oldsigs;
    herr_t ret;
    unsigned step, which;
    bool writer;
    state_t s;

    state_init(&s, argc, argv);

    if (strcmp(s.progname, "vfd_swmr_bigset_writer") == 0)
        writer = true;
    else if (strcmp(s.progname, "vfd_swmr_bigset_reader") == 0)
        writer = false;
    else
        errx(EXIT_FAILURE, "no program personality matches '%s'", s.progname);

    if ((onheap = malloc(sizeof(*onheap))) == NULL)
        err(EXIT_FAILURE, "%s: could not allocate matrix on heap", __func__);

    fapl = vfd_swmr_create_fapl(writer, true, s.use_vfd_swmr);

    if (fapl < 0)
        errx(EXIT_FAILURE, "vfd_swmr_create_fapl");

    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        errx(EXIT_FAILURE, "H5Pcreate");

    ret = H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, false, 1);
    if (ret < 0)
        errx(EXIT_FAILURE, "H5Pset_file_space_strategy");

    if (writer)
        s.file = H5Fcreate(s.filename, H5F_ACC_TRUNC, fcpl, fapl);
    else
        s.file = H5Fopen(s.filename, H5F_ACC_RDONLY, fapl);

    if (s.file == badhid)
        errx(EXIT_FAILURE, writer ? "H5Fcreate" : "H5Fopen");

    block_signals(&oldsigs);

    if (writer) {
        for (which = 0; which < s.ndatasets; which++)
            create_extensible_dset(&s, which);

        for (step = 0; step < s.nsteps; step++) {
            for (which = 0; which < s.ndatasets; which++) {
                dbgf(2, "step %d which %d\n", step, which);
                write_extensible_dset(&s, which, step, onheap->mat);
                nanosleep(&s.update_interval, NULL);
            }
        }
    } else {
        for (which = 0; which < s.ndatasets; which++)
            open_extensible_dset(&s, which);

        for (step = 0; 2 + step < s.nsteps;) {
            for (which = 0; which < s.ndatasets; which++) {
                dbgf(2, "step %d which %d\n", step, which);
                verify_extensible_dset(&s, which, onheap->mat, &step);
                nanosleep(&s.update_interval, NULL);
            }
        }
    }

    if (s.use_vfd_swmr && s.wait_for_signal)
        await_signal(s.file);

    restore_signals(&oldsigs);

    if (H5Pclose(fapl) < 0)
        errx(EXIT_FAILURE, "H5Pclose(fapl)");

    if (H5Pclose(fcpl) < 0)
        errx(EXIT_FAILURE, "H5Pclose(fcpl)");

    if (H5Fclose(s.file) < 0)
        errx(EXIT_FAILURE, "H5Fclose");

    free(onheap);

    return EXIT_SUCCESS;
}
