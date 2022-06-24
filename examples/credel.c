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

#include <assert.h>
#include <err.h>
#include <errno.h>
#include <libgen.h> /* basename(3) */
#include <signal.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>   /* struct timespec, nanosleep */
#include <unistd.h> /* getopt, PATH_MAX, ... */

#include "hdf5.h"
#include "nbcompat.h"

#define DATAROWS 1
#define DATACOLS 10
static const hsize_t         dims[2]       = {DATAROWS, DATACOLS};
static const hsize_t         chunk_dims[2] = {1, 1};
static volatile sig_atomic_t unbroken      = 1;

typedef struct {
    uint64_t created, deleted;
} stats_pair_t;

typedef struct {
    stats_pair_t datasets, groups;
    uint64_t     iterations;
} stats_t;

typedef struct {
    hid_t           dataset[4], dataspace, dapl, dcpl, file, group[2];
    char            output_file[PATH_MAX];
    char            progname[PATH_MAX];
    struct timespec update_interval;
    int             verbose;
    bool            oneshot;
    bool            print_stats;
    bool            use_vfd_swmr;
    uint64_t        iterations_limit;
    stats_t         stats;
} state_t;

#define ALL_HID_INITIALIZER                                                                                  \
    (state_t)                                                                                                \
    {                                                                                                        \
        .dataspace = H5I_INVALID_HID, .file = H5I_INVALID_HID, .verbose = 0, .oneshot = false,               \
        .use_vfd_swmr = true, .print_stats = false, .iterations_limit = UINT64_MAX, .output_file = "",       \
        .update_interval = (struct timespec){.tv_sec = 0, .tv_nsec = 1000000000UL / 10 /* 1/10 second */},   \
        .stats           = {                                                                                 \
            {0, 0},                                                                                \
            {0, 0}                                                                                 \
        }                                                                                                    \
    }

static void state_init(state_t *, int, char **);

static void
write_dataset(state_t *s, int didx)
{
    const int    ndatasets = __arraycount(s->dataset);
    hid_t        ds;
    int32_t      data[DATAROWS][DATACOLS];
    herr_t       status;
    unsigned int i, j;

    for (i = 0; i < __arraycount(data); i++) {
        for (j = 0; j < __arraycount(data[i]); j++) {
            int k      = (didx + j + i) % __arraycount(data[i]);
            data[i][j] = (0 <= k && k < 3) ? 1 : 0;
            if (s->verbose > 1)
                fprintf(stderr, " %" PRId32, data[i][j]);
        }
        if (s->verbose > 1)
            fprintf(stderr, "\n");
    }

    ds     = s->dataset[didx % ndatasets];
    status = H5Dwrite(ds, H5T_NATIVE_INT32, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);

    if (status < 0)
        errx(EXIT_FAILURE, "H5Dwrite failed");

    if (H5Dflush(ds) < 0)
        errx(EXIT_FAILURE, "H5Dflush failed");
}

static void
usage(const char *progname)
{
    fprintf(stderr,
            "usage: %s [-u milliseconds]\n"
            "\n"
            "-o: oneshot mode, perform one iteration, wait for a signal, "
            "then quit.\n"
            "-s: print statistics at end\n"
            "-S: disable VFD SWMR mode\n"
            "-u ms: milliseconds interval between updates to %s.h5\n"
            "-v: verbose mode, mention creation/deletion; -vv: print data\n"
            "\n",
            progname, progname);
    exit(EXIT_FAILURE);
}

static void
print_stats(stats_t *s)
{
    printf("%10" PRIu64 " groups created\n", s->groups.created);
    printf("%10" PRIu64 " groups deleted\n", s->groups.deleted);
    printf("%10" PRIu64 " datasets created\n", s->datasets.created);
    printf("%10" PRIu64 " datasets deleted\n", s->datasets.deleted);
    printf("%10" PRIu64 " iterations\n", s->iterations);
}

static void
state_init(state_t *s, int argc, char **argv)
{
    int           ch, i, rc;
    char          tfile[PATH_MAX];
    char *        end;
    unsigned long millis;
    uintmax_t     niters;

    *s = ALL_HID_INITIALIZER;
    strlcpy(tfile, argv[0], sizeof(tfile));
    strlcpy(s->progname, basename(tfile), sizeof(s->progname));
    while ((ch = getopt(argc, argv, "n:ou:sSv")) != -1) {
        switch (ch) {
            case 'n':
                niters = strtoumax(optarg, &end, 0);
                if (niters == UINTMAX_MAX && errno == ERANGE) {
                    err(EXIT_FAILURE, "option -%c argument \"%s\"", ch, optarg);
                }
                else if (*end != '\0') {
                    errx(EXIT_FAILURE, "garbage after -%c argument \"%s\"", ch, optarg);
                }
                s->iterations_limit = niters;
                break;
            case 'o':
                s->oneshot = true;
                break;
            case 's':
                s->print_stats = true;
                break;
            case 'S':
                s->use_vfd_swmr = false;
                break;
            case 'u':
                errno  = 0;
                millis = strtoul(optarg, &end, 0);
                if (millis == ULONG_MAX && errno == ERANGE) {
                    err(EXIT_FAILURE, "option -%c argument \"%s\"", ch, optarg);
                }
                else if (*end != '\0') {
                    errx(EXIT_FAILURE, "garbage after -%c argument \"%s\"", ch, optarg);
                }
                s->update_interval.tv_sec  = millis / 1000UL;
                s->update_interval.tv_nsec = (millis * 1000000UL) % 1000000000UL;
                warnx("%lu milliseconds between updates", millis);
                break;
            case 'v':
                s->verbose++;
                break;
            case '?':
            default:
                usage(s->progname);
        }
    }
    argc -= optind;
    argv += optind;

    for (i = 0; i < __arraycount(s->dataset); i++)
        s->dataset[i] = H5I_INVALID_HID;

    for (i = 0; i < __arraycount(s->group); i++)
        s->group[i] = H5I_INVALID_HID;

    rc = snprintf(s->output_file, sizeof(s->output_file), "%s.h5", s->progname);
    if (rc == -1 || rc >= sizeof(s->output_file))
        errx(EXIT_FAILURE, "output filename was truncated");
}

static void
delete_group(state_t *s, const int64_t gidx)
{
    hid_t     g;
    const int ngroups = __arraycount(s->group);
    char      gname[32];

    assert(0 <= gidx);

    snprintf(gname, sizeof(gname), "/group-%" PRId64, gidx);
    g = s->group[gidx % ngroups];

    if (H5Ldelete(s->file, gname, H5P_DEFAULT) < 0) {
        errx(EXIT_FAILURE, "%s: H5Ldelete(, \"%s\", ) failed", __func__, gname);
    }

    if (H5Gclose(g) < 0)
        errx(EXIT_FAILURE, "H5Gclose failed");

    if (s->verbose > 0)
        fprintf(stderr, "Deleted group %s\n", gname);

    s->group[gidx % ngroups] = H5I_INVALID_HID;
    s->stats.groups.deleted++;
}

static void
create_group(state_t *s, const int64_t gidx)
{
    const int ngroups = __arraycount(s->group);
    hid_t     g;
    char      gname[32];

    assert(0 <= gidx);
    assert(s->group[gidx % ngroups] == H5I_INVALID_HID);

    snprintf(gname, sizeof(gname), "/group-%" PRId64, gidx);
    g = H5Gcreate(s->file, gname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    if (g < 0)
        errx(EXIT_FAILURE, "H5Gcreate failed");

    if (s->verbose > 0)
        fprintf(stderr, "Created group %s\n", gname);
    s->group[gidx % ngroups] = g;
    s->stats.groups.created++;
}

static void
delete_dataset(state_t *s, const int64_t didx)
{
    const int     ndatasets = __arraycount(s->dataset);
    char          dname[32];
    const int64_t gidx = didx / 2;

    assert(0 <= gidx && 0 <= didx);

    snprintf(dname, sizeof(dname), "/group-%" PRId64 "/dataset-%" PRId64, gidx, didx);
    if (H5Ldelete(s->file, dname, H5P_DEFAULT) < 0) {
        errx(EXIT_FAILURE, "%s: H5Ldelete(, \"%s\", ) failed", __func__, dname);
    }

    if (s->verbose > 0)
        fprintf(stderr, "Deleted dataset %s\n", dname);
#if 1
    const hid_t ds = s->dataset[didx % ndatasets];
    if (H5Dclose(ds) < 0)
        errx(EXIT_FAILURE, "H5Dclose failed");
#endif
    s->dataset[didx % ndatasets] = H5I_INVALID_HID;
    s->stats.datasets.deleted++;
}

static void
create_dataset(state_t *s, const int64_t didx)
{
    const int     ndatasets = __arraycount(s->dataset);
    char          dname[32];
    const int64_t gidx = didx / 2;
    hid_t         ds;

    assert(0 <= gidx && 0 <= didx);
    assert(s->dataset[didx % ndatasets] == H5I_INVALID_HID);

    s->dataspace = H5Screate_simple(__arraycount(dims), dims, NULL);

    if (s->dataspace < 0)
        errx(EXIT_FAILURE, "H5Screate_simple failed");

    snprintf(dname, sizeof(dname), "/group-%" PRId64 "/dataset-%" PRId64, gidx, didx);
    ds = H5Dcreate2(s->file, dname, H5T_STD_I32BE, s->dataspace, H5P_DEFAULT, s->dcpl, s->dapl);

    if (H5Sclose(s->dataspace) < 0)
        errx(EXIT_FAILURE, "H5Sclose failed");

    s->dataspace = H5I_INVALID_HID;

    if (ds < 0)
        errx(EXIT_FAILURE, "H5Dcreate(, \"%s\", ) failed", dname);

    if (s->verbose > 0)
        fprintf(stderr, "Created dataset %s\n", dname);
    s->dataset[didx % ndatasets] = ds;
    s->stats.datasets.created++;
}

static void
create_and_write_dataset(state_t *s, const int64_t didx)
{
#if 0
	const int64_t gidx = didx / 2;
	const int ngroups = __arraycount(s->group);
	const hid_t g = s->group[gidx % ngroups];

	if (H5Odisable_mdc_flushes(g) < 0)
		err(EXIT_FAILURE, "H5Odisable_mdc_flushes failed");
#endif

    create_dataset(s, didx);
    write_dataset(s, didx);

#if 0
	if (H5Oenable_mdc_flushes(g) < 0)
		err(EXIT_FAILURE, "H5Oenable_mdc_flushes failed");
#endif
}

static void
handle_signal(int signo)
{
    char msg[] = "Handling signal\n";
    write(STDERR_FILENO, msg, sizeof(msg) - 1);
    unbroken = 0;
}

static void
disestablish_handler(const struct sigaction *osa)
{
    if (sigaction(SIGINT, osa, NULL) == -1)
        err(EXIT_FAILURE, "%s: sigaction", __func__);
}

static void
establish_handler(struct sigaction *osa)
{
    struct sigaction sa;

    memset(&sa, '\0', sizeof(sa));
    sa.sa_handler = handle_signal;
    sigemptyset(&sa.sa_mask);
    if (sigaction(SIGINT, &sa, osa) == -1)
        err(EXIT_FAILURE, "%s: sigaction", __func__);
}

int
main(int argc, char **argv)
{
    hid_t                 fapl, fcpl;
    struct sigaction      osa;
    state_t               storage;
    state_t *             s = &storage;
    int64_t               i;
    H5F_vfd_swmr_config_t config;

    memset(&config, '\0', sizeof(config));

    state_init(s, argc, argv);

    fapl = H5Pcreate(H5P_FILE_ACCESS);
    if (fapl < 0) {
        errx(EXIT_FAILURE, "%s.%d H5Pcreate failed", __func__, __LINE__);
    }

    fcpl = H5Pcreate(H5P_FILE_CREATE);
    if (fcpl < 0) {
        errx(EXIT_FAILURE, "%s.%d H5Pcreate failed", __func__, __LINE__);
    }

    config.version  = H5F__CURR_VFD_SWMR_CONFIG_VERSION;
    config.tick_len = 4;
    config.max_lag  = 5;
#if 0 /* raw-data flushing is not implemented */
	config.flush_raw_data = true;
#endif
    config.writer            = true;
    config.md_pages_reserved = 128;
    strlcpy(config.md_file_path, "./my_md_file", sizeof(config.md_file_path));

    /* Enable page buffering */
    if (H5Pset_page_buffer_size(fapl, 4096, 100, 0) < 0)
        errx(EXIT_FAILURE, "H5Pset_page_buffer_size failed");

    /* Enable VFD SWMR configuration */
    if (s->use_vfd_swmr && H5Pset_vfd_swmr_config(fapl, &config) < 0)
        errx(EXIT_FAILURE, "H5Pset_vfd_swmr_config failed");

    /* Set file space strategy to paged aggregation in fcpl.
     * Page buffering *requires* this strategy.
     *
     * I set the free-space threshold to 1GB so that deleted
     * datasets are not recycled.
     */
    if (H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, false, 1024 * 1024 * 1024) < 0)
        errx(EXIT_FAILURE, "H5Pset_file_space_strategy failed");

    s->file = H5Fcreate(s->output_file, H5F_ACC_TRUNC, fcpl, fapl);

    H5Pclose(fapl);

    if (s->file < 0)
        errx(EXIT_FAILURE, "H5Fcreate failed");

    if ((s->dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
        errx(EXIT_FAILURE, "%s.%d: H5Pcreate failed", __func__, __LINE__);
    }
    if ((s->dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0) {
        errx(EXIT_FAILURE, "%s.%d: H5Pcreate failed", __func__, __LINE__);
    }
    if (H5Pset_chunk(s->dcpl, 2, chunk_dims) < 0)
        errx(EXIT_FAILURE, "H5Pset_chunk failed");
    if (H5Pset_chunk_cache(s->dapl, H5D_CHUNK_CACHE_NSLOTS_DEFAULT, 0, H5D_CHUNK_CACHE_W0_DEFAULT) < 0)
        errx(EXIT_FAILURE, "H5Pset_chunk_cache failed");

    establish_handler(&osa);

    for (i = 0; i < 4; i++) {
        s->stats.iterations++;
        if (i % 2 == 0)
            create_group(s, i / 2);
        create_and_write_dataset(s, i);
    }

    for (i = 5; unbroken; i += 2) {
        delete_dataset(s, i - 5);
        delete_dataset(s, i - 4);
        delete_group(s, (i - 4) / 2);
        create_group(s, i / 2);
        create_and_write_dataset(s, i - 1);
        create_and_write_dataset(s, i);
        if (s->oneshot || ++s->stats.iterations >= s->iterations_limit)
            break;
        nanosleep(&s->update_interval, NULL);
    }

    if (s->oneshot) {
        sigset_t mask;
        sigemptyset(&mask);
        H5Fvfd_swmr_end_tick(s->file);
        (void)sigsuspend(&mask);
    }
#if 0
	fprintf(stderr, "Interrupted.  Cleaning up.\n");

	int j;
	for (--i, j = 0; j < 4; j++, --i) {
		if (i % 2 == 1) {
			delete_dataset(s, i - 1);
			delete_dataset(s, i);
			delete_group(s, i / 2);
		}
	}

	for (j = 0; j < 4; j++) {
		assert(s->dataset[j] == H5I_INVALID_HID);
		assert(s->group[j / 2] == H5I_INVALID_HID);
	}
#endif

    if (s->print_stats)
        print_stats(&s->stats);

    if (H5Fclose(s->file) < 0)
        errx(EXIT_FAILURE, "H5Fclose failed");

    if (H5Pclose(s->dapl) < 0)
        errx(EXIT_FAILURE, "H5Pclose failed");

    if (H5Pclose(s->dcpl) < 0)
        errx(EXIT_FAILURE, "H5Pclose failed");

    disestablish_handler(&osa);

    return EXIT_SUCCESS;
}
