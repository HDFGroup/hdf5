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

enum _step { CREATE = 0, LENGTHEN, SHORTEN, DELETE, NSTEPS } step_t;

static const hid_t badhid               = H5I_INVALID_HID; // abbreviate
static bool        caught_out_of_bounds = false;

static void
write_vl_dset(hid_t dset, hid_t type, hid_t space, char *data)
{
    if (H5Dwrite(dset, type, space, space, H5P_DEFAULT, &data) < 0)
        errx(EXIT_FAILURE, "%s: H5Dwrite", __func__);
    if (H5Dflush(dset) < 0)
        errx(EXIT_FAILURE, "%s: H5Dflush", __func__);
}

#if 0
static hid_t
initialize_dset(hid_t file, hid_t type, hid_t space, const char *name,
    void *data)
{
    hid_t dset;

    dset = H5Dcreate2(file, name, type, space, H5P_DEFAULT, H5P_DEFAULT,
        H5P_DEFAULT);

    if (dset == badhid)
        errx(EXIT_FAILURE, "H5Dcreate2");

    if (H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0)
        errx(EXIT_FAILURE, "H5Dwrite");

    if (H5Dflush(dset) < 0)
        errx(EXIT_FAILURE, "%s: H5Dflush", __func__);

    return dset;
}

static void
rewrite_dset(hid_t dset, hid_t type, char *data)
{
    if (H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0)
        errx(EXIT_FAILURE, "%s: H5Dwrite", __func__);
    if (H5Dflush(dset) < 0)
        errx(EXIT_FAILURE, "%s: H5Dflush", __func__);
}
#endif

static hid_t
create_vl_dset(hid_t file, hid_t type, hid_t space, const char *name)
{
    hid_t dset;

    dset = H5Dcreate2(file, name, type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    if (dset == badhid)
        errx(EXIT_FAILURE, "H5Dcreate2");

    return dset;
}

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

static void
usage(const char *progname)
{
    fprintf(stderr, "usage: %s [-W] [-V]\n", progname);
    fprintf(stderr, "\n  -W: do not wait for SIGINT or SIGUSR1\n");
    fprintf(stderr, "\n  -S: do not use VFD SWMR\n");
    fprintf(stderr, "  -f: use fixed-length string\n");
    fprintf(stderr, "      (default: variable-length string)\n");
    fprintf(stderr, "  -n: number of test steps to perform\n");
    fprintf(stderr, "  -q: be quiet: few/no progress messages\n");
    fprintf(stderr, "  -t (oob|null): select out-of-bounds or NULL test\n");
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
    hid_t                 fapl, fcpl, fid, space, type;
    hid_t                 dset[2];
    char                  content[2][96];
    char                  name[2][96];
    H5F_t *               f;
    H5C_t *               cache;
    sigset_t              oldsigs;
    herr_t                ret;
    bool                  variable = true, wait_for_signal = true;
    const hsize_t         dims = 1;
    int                   ch, i, ntimes = 100;
    unsigned long         tmp;
    char *                end;
    bool                  use_vfd_swmr = true;
    const struct timespec delay        = {.tv_sec = 0, .tv_nsec = 1000 * 1000 * 1000 / 10};
    testsel_t             sel          = TEST_NONE;
    H5F_vfd_swmr_config_t config;

    assert(H5T_C_S1 != badhid);

    while ((ch = getopt(argc, argv, "SWfn:qt:")) != -1) {
        switch (ch) {
            case 'S':
                use_vfd_swmr = false;
                break;
            case 'W':
                wait_for_signal = false;
                break;
            case 'f':
                variable = false;
                break;
            case 'n':
                errno = 0;
                tmp   = strtoul(optarg, &end, 0);
                if (end == optarg || *end != '\0')
                    errx(EXIT_FAILURE, "couldn't parse `-n` argument `%s`", optarg);
                else if (errno != 0)
                    err(EXIT_FAILURE, "couldn't parse `-n` argument `%s`", optarg);
                else if (tmp > INT_MAX)
                    errx(EXIT_FAILURE, "`-n` argument `%lu` too large", tmp);
                ntimes = (int)tmp;
                break;
            case 'q':
                verbosity = 1;
                break;
            case 't':
                if (strcmp(optarg, "oob") == 0)
                    sel = TEST_OOB;
                else if (strcmp(optarg, "null") == 0)
                    sel = TEST_NULL;
                else
                    usage(argv[0]);
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
    init_vfd_swmr_config(&config, 4, 7, true, FALSE, 128, "./vlstr-shadow");

    /* use_latest_format, use_vfd_swmr, only_meta_page, config */
    fapl = vfd_swmr_create_fapl(true, use_vfd_swmr, sel == TEST_OOB, &config);

    if (fapl < 0)
        errx(EXIT_FAILURE, "vfd_swmr_create_fapl");

    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        errx(EXIT_FAILURE, "H5Pcreate");

    ret = H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, false, 1);
    if (ret < 0)
        errx(EXIT_FAILURE, "H5Pset_file_space_strategy");

    fid = H5Fcreate("vfd_swmr_vlstr.h5", H5F_ACC_TRUNC, fcpl, fapl);

    /* Create the VL string datatype and a scalar dataspace, or a
     * fixed-length string datatype and a simple dataspace.
     */
    if ((type = H5Tcopy(H5T_C_S1)) == badhid)
        errx(EXIT_FAILURE, "H5Tcopy");

    if (!variable) {
        if (H5Tset_size(type, 32) < 0)
            errx(EXIT_FAILURE, "H5Tset_size");
        space = H5Screate_simple(1, &dims, NULL);
    }
    else {
        if (H5Tset_size(type, H5T_VARIABLE) < 0)
            errx(EXIT_FAILURE, "H5Tset_size");
        space = H5Screate(H5S_SCALAR);
    }

    if (space == badhid)
        errx(EXIT_FAILURE, "H5Screate");

    if ((f = H5VL_object_verify(fid, H5I_FILE)) == NULL)
        errx(EXIT_FAILURE, "H5VL_object_verify");

    cache = f->shared->cache;

    if (fid == badhid)
        errx(EXIT_FAILURE, "H5Fcreate");

    block_signals(&oldsigs);

    print_cache_hits(cache);

    /* content 1 seq 1 short
     * content 1 seq 1 long long long long long long long long
     * content 1 seq 1 medium medium medium
     */
    for (i = 0; i < ntimes; i++) {
        const int ndsets = 2;
        const int step   = i % NSTEPS;
        const int which  = (i / NSTEPS) % ndsets;
        const int seq    = i / (ndsets * NSTEPS);
        dbgf(2, "iteration %d which %d step %d seq %d\n", i, which, step, seq);
        switch (step) {
            case CREATE:
                (void)snprintf(name[which], sizeof(name[which]), "dset-%d", which);
                (void)snprintf(content[which], sizeof(content[which]), "content %d seq %d short", which, seq);
                dset[which] = create_vl_dset(fid, type, space, name[which]);
                write_vl_dset(dset[which], type, space, content[which]);
                break;
            case LENGTHEN:
                (void)snprintf(content[which], sizeof(content[which]),
                               "content %d seq %d long long long long long long long long", which, seq);
                write_vl_dset(dset[which], type, space, content[which]);
                break;
            case SHORTEN:
                (void)snprintf(content[which], sizeof(content[which]),
                               "content %d seq %d medium medium medium", which, seq);
                write_vl_dset(dset[which], type, space, content[which]);
                break;
            case DELETE:
                if (H5Dclose(dset[which]) < 0)
                    errx(EXIT_FAILURE, "H5Dclose");
                if (H5Ldelete(fid, name[which], H5P_DEFAULT) < 0) {
                    errx(EXIT_FAILURE, "%s: H5Ldelete(, \"%s\", ) failed", __func__, name[which]);
                }
                break;
            default:
                errx(EXIT_FAILURE, "%s: unknown step %d", __func__, step);
        }
        if (caught_out_of_bounds) {
            fprintf(stderr, "caught out of bounds\n");
            break;
        }
        nanosleep(&delay, NULL);
    }

    if (use_vfd_swmr && wait_for_signal)
        await_signal(fid);

    restore_signals(&oldsigs);

    if (H5Pclose(fapl) < 0)
        errx(EXIT_FAILURE, "H5Pclose(fapl)");

    if (H5Pclose(fcpl) < 0)
        errx(EXIT_FAILURE, "H5Pclose(fcpl)");

    if (H5Tclose(type) < 0)
        errx(EXIT_FAILURE, "H5Tclose");

    if (H5Sclose(space) < 0)
        errx(EXIT_FAILURE, "H5Sclose");

    if (H5Fclose(fid) < 0)
        errx(EXIT_FAILURE, "H5Fclose");

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
