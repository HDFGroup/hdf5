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
#include <libgen.h> /* basename(3) */
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
#include "genall5.h"
#include "vfd_swmr_common.h"

enum _step {
  CREATE = 0
, LENGTHEN
, SHORTEN
, DELETE
, NSTEPS
} step_t;

static const hid_t badhid = H5I_INVALID_HID;
static bool caught_out_of_bounds = false;
static bool writer;

static void
print_cache_hits(H5C_t *cache)
{
    int i;

    for (i = 0; i < H5AC_NTYPES; i++) {
        dbgf(3, "type-%d cache hits %" PRId64 "%s\n",
            i, cache->hits[i], (i == H5AC_GHEAP_ID) ? " *" : "");
    }
    dbgf(3, "\n");
}

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
    fprintf(stderr, "usage: %s [-W] [-V]\n", progname);
    fprintf(stderr, "\n  -W: do not wait for SIGINT or SIGUSR1\n");
    fprintf(stderr, "\n  -S: do not use VFD SWMR\n");
    fprintf(stderr,   "  -a: run all tests, including variable-length data\n");
    fprintf(stderr,   "  -n: number of test steps to perform\n");
    fprintf(stderr,   "  -q: be quiet: few/no progress messages\n");
    fprintf(stderr,   "  -v: be verbose: most progress messages\n");
    exit(EXIT_FAILURE);
}

bool
H5HG_trap(const char *reason)
{
    if (strcmp(reason, "out of bounds") == 0) {
        caught_out_of_bounds = true;
        return false;
    }
    return true;
}

int
main(int argc, char **argv)
{
    hid_t fapl, fcpl, fid;
    H5F_t *f;
    H5C_t *cache;
    sigset_t oldsigs;
    herr_t ret;
    bool skip_varlen = true, wait_for_signal;
    int ch;
    bool use_vfd_swmr = true;
#if 0
    unsigned long tmp;
    char *end;
    int i, ntimes = 100;
    const struct timespec delay =
        {.tv_sec = 0, .tv_nsec = 1000 * 1000 * 1000 / 10};
#endif
    const char *progname = basename(argv[0]);
    estack_state_t es;

    dbgf(1, "0th arg %s, personality `%s`\n", argv[0], progname);

    if (strcmp(progname, "vfd_swmr_zoo_writer") == 0)
        writer = wait_for_signal = true;
    else if (strcmp(progname, "vfd_swmr_zoo_reader") == 0)
        writer = false;
    else {
        errx(EXIT_FAILURE,
             "unknown personality, expected vfd_swmr_zoo_{reader,writer}");
    }

    while ((ch = getopt(argc, argv, "SWan:qv")) != -1) {
        switch(ch) {
        case 'S':
            use_vfd_swmr = false;
            break;
        case 'W':
            wait_for_signal = false;
            break;
        case 'a':
            skip_varlen = false;
            break;
#if 0
        case 'n':
            errno = 0;
            tmp = strtoul(optarg, &end, 0);
            if (end == optarg || *end != '\0')
                errx(EXIT_FAILURE, "couldn't parse `-n` argument `%s`", optarg);
            else if (errno != 0)
                err(EXIT_FAILURE, "couldn't parse `-n` argument `%s`", optarg);
            else if (tmp > INT_MAX)
                errx(EXIT_FAILURE, "`-n` argument `%lu` too large", tmp);
            ntimes = (int)tmp;
            break;
#endif
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

    fapl = vfd_swmr_create_fapl(writer, true, use_vfd_swmr);

    if (fapl < 0)
        errx(EXIT_FAILURE, "vfd_swmr_create_fapl");

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

    es = disable_estack();
    if (writer) {
        dbgf(1, "Writing zoo...\n");
        if (!create_zoo(fid, ".", 0, skip_varlen))
            errx(EXIT_FAILURE, "create_zoo didn't pass self-check");
    } else {
        dbgf(1, "Reading zoo...\n");
        while (!validate_zoo(fid, ".", 0, skip_varlen))
            ;
    }
    restore_estack(es);

    if (use_vfd_swmr && wait_for_signal)
        await_signal(fid);

    if (wait_for_signal)
        restore_signals(&oldsigs);

    if (H5Pclose(fapl) < 0)
        errx(EXIT_FAILURE, "H5Pclose(fapl)");

    if (H5Pclose(fcpl) < 0)
        errx(EXIT_FAILURE, "H5Pclose(fcpl)");

    if (H5Fclose(fid) < 0)
        errx(EXIT_FAILURE, "H5Fclose");

    return EXIT_SUCCESS;
}
