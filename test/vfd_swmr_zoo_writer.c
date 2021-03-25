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

#include <err.h>
#include <libgen.h>

#define H5C_FRIEND /* suppress error about including H5Cpkg */
#define H5F_FRIEND /* suppress error about including H5Fpkg */

#include "hdf5.h"

#include "H5private.h"
#include "H5retry_private.h"
#include "H5Cpkg.h"
#include "H5Fpkg.h"
#include "H5HGprivate.h"
#include "H5VLprivate.h"

#include "testhdf5.h"
#include "genall5.h"
#include "vfd_swmr_common.h"

#ifndef _arraycount
#define _arraycount(_a) (sizeof(_a) / sizeof(_a[0]))
#endif

typedef struct _shared_ticks {
    uint64_t reader_tick;
} shared_ticks_t;

typedef struct _tick_stats {
    uint64_t writer_tried_increase;
    uint64_t writer_aborted_increase;
    uint64_t writer_read_shared_file;
    uint64_t reader_tick_was_zero;     // writer read reader tick equal to 0
    uint64_t reader_tick_lead_writer;  // writer read reader tick greater than
                                       // proposed writer tick
    uint64_t writer_lead_reader_by[1]; // proposed writer tick lead reader
                                       // tick by `lead` ticks
                                       // `writer_lead_reader_by[lead]`
                                       // times, for `0 <= lead <= max_lag - 1`
} tick_stats_t;

static H5F_vfd_swmr_config_t swmr_config;
static tick_stats_t *        tick_stats = NULL;
static const hid_t           badhid     = H5I_INVALID_HID;
static bool                  writer;

static void
#ifndef H5C_COLLECT_CACHE_STATS
print_cache_hits(H5C_t *cache)
{
    int i;

    for (i = 0; i < H5AC_NTYPES; i++) {
        dbgf(3, "type-%d cache hits %" PRId64 "%s\n", i, cache->hits[i], (i == H5AC_GHEAP_ID) ? " *" : "");
    }
    dbgf(3, "\n");
}
#else
print_cache_hits(H5C_t H5_ATTR_UNUSED *cache)
{
    return;
}
#endif

void
zoo_create_hook(hid_t fid)
{
    dbgf(3, "%s: enter\n", __func__);
    if (writer)
        H5Fvfd_swmr_end_tick(fid);
}

static void
usage(const char *progname)
{
    fprintf(stderr, "usage: %s [-C] [-S] [-W] [-a] [-e] [-m] [-q] [-v]\n", progname);
    fprintf(stderr, "\n  -C: skip compact dataset tests\n");
    fprintf(stderr, "  -S: do not use VFD SWMR\n");
    fprintf(stderr, "  -W: do not wait for SIGINT or SIGUSR1\n");
    fprintf(stderr, "  -a: run all tests, including variable-length data\n");
    fprintf(stderr, "  -e: print error stacks\n");
    fprintf(stderr, "  -m ms: maximum `ms` milliseconds pause between\n");
    fprintf(stderr, "         each create/delete step\n");
    fprintf(stderr, "  -q: be quiet: few/no progress messages\n");
    fprintf(stderr, "  -v: be verbose: most progress messages\n");
    exit(EXIT_FAILURE);
}

bool
vfd_swmr_writer_may_increase_tick_to(uint64_t new_tick, bool wait_for_reader)
{
    static int     fd = -1;
    shared_ticks_t shared;
    ssize_t        nread;
    h5_retry_t     retry;
    bool           do_try;

    dbgf(3, "%s: enter\n", __func__);

    if (fd == -1) {
        fd = open("./shared_tick_num", O_RDONLY);
        if (fd == -1) {
            warn("%s: open", __func__); // TBD ratelimit/silence this warning
            return true;
        }
        assert(tick_stats == NULL);
        tick_stats = calloc(1, sizeof(*tick_stats) +
                                   (swmr_config.max_lag - 1) * sizeof(tick_stats->writer_lead_reader_by[0]));
        if (tick_stats == NULL)
            err(EXIT_FAILURE, "%s: calloc", __func__);
    }

    tick_stats->writer_tried_increase++;

    for (do_try = h5_retry_init(&retry, 14, 10 * 1000 * 1000, 100 * 1000 * 1000); do_try;
         do_try = wait_for_reader && h5_retry_next(&retry)) {

        tick_stats->writer_read_shared_file++;

        if ((nread = pread(fd, &shared, sizeof(shared), 0)) == -1)
            err(EXIT_FAILURE, "%s: pread", __func__);

        if (nread != sizeof(shared))
            errx(EXIT_FAILURE, "%s: pread", __func__);

        // TBD convert endianness

        if (shared.reader_tick == 0) {
            tick_stats->reader_tick_was_zero++;
            return true;
        }

        if (new_tick < shared.reader_tick) {
            tick_stats->reader_tick_lead_writer++;
            return true;
        }
        if (new_tick <= shared.reader_tick + swmr_config.max_lag - 1) {
            uint64_t lead = new_tick - shared.reader_tick;
            assert(lead <= swmr_config.max_lag - 1);
            tick_stats->writer_lead_reader_by[lead]++;
            return true;
        }
    }
    if (wait_for_reader && !do_try)
        errx(EXIT_FAILURE, "%s: timed out waiting for reader", __func__);

    tick_stats->writer_aborted_increase++;

    return false;
}

void
vfd_swmr_reader_did_increase_tick_to(uint64_t new_tick)
{
    static int     fd = -1;
    shared_ticks_t shared;
    ssize_t        nwritten;

    dbgf(3, "%s: enter\n", __func__);

    if (fd == -1) {
        // TBD create a temporary file, here, and move it to its final path
        // after writing it.
        fd = open("./shared_tick_num", O_RDWR | O_CREAT, 0600);
        if (fd == -1)
            err(EXIT_FAILURE, "%s: open", __func__);
    }

    shared.reader_tick = new_tick;

    // TBD convert endianness

    if ((nwritten = pwrite(fd, &shared, sizeof(shared), 0)) == -1)
        errx(EXIT_FAILURE, "%s: pwrite", __func__);

    if (nwritten != sizeof(shared))
        errx(EXIT_FAILURE, "%s: pwrite", __func__);

    if (new_tick == 0) {
        if (unlink("./shared_tick_num") == -1)
            warn("%s: unlink", __func__);
        if (close(fd) == -1)
            err(EXIT_FAILURE, "%s: close", __func__);
        fd = -1;
    }
}

int
main(int argc, char **argv)
{
    hid_t                 fapl, fcpl, fid;
    H5F_t *               f;
    H5C_t *               cache;
    sigset_t              oldsigs;
    herr_t                ret;
    zoo_config_t          config      = {.proc_num        = 0,
                           .skip_compact    = false,
                           .skip_varlen     = true,
                           .max_pause_msecs = 0,
                           .msgival         = {.tv_sec = 5, .tv_nsec = 0}};
    struct timespec       lastmsgtime = {.tv_sec = 0, .tv_nsec = 0};
    bool                  wait_for_signal;
    int                   ch;
    unsigned              seed;
    unsigned long         tmpl;
    char *                end;
    const char *          seedvar      = "H5_ZOO_STEP_SEED";
    bool                  use_vfd_swmr = true;
    bool                  print_estack = false;
    const char *          progname     = basename(argv[0]);
    const char *          personality  = strstr(progname, "vfd_swmr_zoo_");
    estack_state_t        es;
    char                  step = 'b';
    H5F_vfd_swmr_config_t vfd_swmr_config;

    if (personality != NULL && strcmp(personality, "vfd_swmr_zoo_writer") == 0)
        writer = wait_for_signal = true;
    else if (personality != NULL && strcmp(personality, "vfd_swmr_zoo_reader") == 0)
        writer = false;
    else {
        errx(EXIT_FAILURE, "unknown personality, expected vfd_swmr_zoo_{reader,writer}");
    }

    if (writer)
        config.max_pause_msecs = 50;

    while ((ch = getopt(argc, argv, "CSWaem:qv")) != -1) {
        switch (ch) {
            case 'C':
                config.skip_compact = true;
                break;
            case 'S':
                use_vfd_swmr = false;
                break;
            case 'W':
                wait_for_signal = false;
                break;
            case 'a':
                config.skip_varlen = false;
                break;
            case 'e':
                print_estack = true;
                break;
            case 'm':
                errno = 0;
                tmpl  = strtoul(optarg, &end, 0);
                if (end == optarg || *end != '\0')
                    errx(EXIT_FAILURE, "couldn't parse `-m` argument `%s`", optarg);
                else if (errno != 0)
                    err(EXIT_FAILURE, "couldn't parse `-m` argument `%s`", optarg);
                else if (tmpl > UINT_MAX)
                    errx(EXIT_FAILURE, "`-m` argument `%lu` too large", tmpl);
                config.max_pause_msecs = (unsigned)tmpl;
                break;
            case 'q':
                verbosity = 1;
                break;
            case 'v':
                verbosity = 3;
                break;
            default:
                usage(argv[0]);
                break;
        }
    }
    argv += optind;
    argc -= optind;

    if (argc > 0)
        errx(EXIT_FAILURE, "unexpected command-line arguments");

    /* config, tick_len, max_lag, writer, flush_raw_data, md_pages_reserved, md_file_path */
    init_vfd_swmr_config(&vfd_swmr_config, 4, 7, writer, FALSE, 128, "./zoo-shadow");

    /* ? turn off use latest format argument via 1st argument? since later on it reset to early format */
    /* use_latest_format, use_vfd_swmr, only_meta_page, config */
    fapl = vfd_swmr_create_fapl(true, use_vfd_swmr, true, &vfd_swmr_config);

    if (use_vfd_swmr && H5Pget_vfd_swmr_config(fapl, &swmr_config) < 0)
        errx(EXIT_FAILURE, "H5Pget_vfd_swmr_config");

    if (fapl < 0)
        errx(EXIT_FAILURE, "vfd_swmr_create_fapl");

    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_EARLIEST, H5F_LIBVER_LATEST) < 0) {
        errx(EXIT_FAILURE, "%s.%d: H5Pset_libver_bounds", __func__, __LINE__);
    }

    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        errx(EXIT_FAILURE, "H5Pcreate");

    ret = H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, false, 1);
    if (ret < 0)
        errx(EXIT_FAILURE, "H5Pset_file_space_strategy");

    if (writer)
        fid = H5Fcreate("vfd_swmr_zoo.h5", H5F_ACC_TRUNC, fcpl, fapl);
    else
        fid = H5Fopen("vfd_swmr_zoo.h5", H5F_ACC_RDONLY, fapl);

    if (fid == badhid)
        errx(EXIT_FAILURE, writer ? "H5Fcreate" : "H5Fopen");

    if ((f = H5VL_object_verify(fid, H5I_FILE)) == NULL)
        errx(EXIT_FAILURE, "H5VL_object_verify");

    cache = f->shared->cache;

    if (wait_for_signal)
        block_signals(&oldsigs);

    print_cache_hits(cache);

    es = print_estack ? estack_get_state() : disable_estack();
    if (writer) {

        dbgf(2, "Writing zoo...\n");

        /* get seed from environment or else from time(3) */
        switch (fetch_env_ulong(seedvar, UINT_MAX, &tmpl)) {
            case -1:
                errx(EXIT_FAILURE, "%s: fetch_env_ulong", __func__);
            case 0:
                seed = (unsigned int)time(NULL);
                break;
            default:
                seed = (unsigned int)tmpl;
                break;
        }

        dbgf(1, "To reproduce, set seed (%s) to %u.\n", seedvar, seed);

        HDsrandom(seed);

        if (!create_zoo(fid, ".", &lastmsgtime, config))
            errx(EXIT_FAILURE, "create_zoo didn't pass self-check");

        /* Avoid deadlock: flush the file before waiting for the reader's
         * message.
         */
        if (H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
            errx(EXIT_FAILURE, "%s: H5Fflush failed", __func__);

        if (read(STDIN_FILENO, &step, sizeof(step)) == -1)
            err(EXIT_FAILURE, "read");

        if (step != 'b')
            errx(EXIT_FAILURE, "expected 'b' read '%c'", step);

        if (!delete_zoo(fid, ".", &lastmsgtime, config))
            errx(EXIT_FAILURE, "delete_zoo failed");
    }
    else {
        dbgf(2, "Reading zoo...\n");

        while (!validate_zoo(fid, ".", &lastmsgtime, config))
            ;

        if (write(STDOUT_FILENO, &step, sizeof(step)) == -1)
            err(EXIT_FAILURE, "write");
        while (!validate_deleted_zoo(fid, ".", &lastmsgtime, config))
            ;
    }
    restore_estack(es);

    if (use_vfd_swmr && wait_for_signal)
        await_signal(fid);

    if (writer && tick_stats != NULL) {
        uint64_t lead;

        dbgf(2, "writer tried tick increase %" PRIu64 "\n", tick_stats->writer_tried_increase);
        dbgf(2, "writer aborted tick increase %" PRIu64 "\n", tick_stats->writer_aborted_increase);
        dbgf(2, "writer read shared file %" PRIu64 "\n", tick_stats->writer_read_shared_file);
        dbgf(2, "writer read reader tick equal to 0 %" PRIu64 "\n", tick_stats->reader_tick_was_zero);
        dbgf(2, "writer read reader tick leading writer %" PRIu64 "\n", tick_stats->reader_tick_lead_writer);

        for (lead = 0; lead < swmr_config.max_lag; lead++) {
            dbgf(2, "writer tick lead writer by %" PRIu64 " %" PRIu64 "\n", lead,
                 tick_stats->writer_lead_reader_by[lead]);
        }
    }

    if (H5Pclose(fapl) < 0)
        errx(EXIT_FAILURE, "H5Pclose(fapl)");

    if (H5Pclose(fcpl) < 0)
        errx(EXIT_FAILURE, "H5Pclose(fcpl)");

    if (H5Fclose(fid) < 0)
        errx(EXIT_FAILURE, "H5Fclose");

    if (wait_for_signal)
        restore_signals(&oldsigs);

    return EXIT_SUCCESS;
}
