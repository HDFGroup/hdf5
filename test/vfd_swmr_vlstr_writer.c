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

#define H5C_FRIEND /*suppress error about including H5Cpkg   */
#define H5F_FRIEND /*suppress error about including H5Fpkg   */

#include "hdf5.h"

#include "H5Cpkg.h"
#include "H5Fpkg.h"
#include "H5VLprivate.h"

#include "testhdf5.h"
#include "vfd_swmr_common.h"

#ifndef H5_HAVE_WIN32_API

enum _step { CREATE = 0, LENGTHEN, SHORTEN, DELETE, NSTEPS } step_t;

static hbool_t caught_out_of_bounds = FALSE;

static herr_t
write_vl_dset(hid_t dset, hid_t type, hid_t space, char *data)
{
    if (H5Dwrite(dset, type, space, space, H5P_DEFAULT, &data) < 0)
        STACK_ERROR;
    if (H5Dflush(dset) < 0)
        STACK_ERROR;

    return SUCCEED;
error:
    return FAIL;
}

static hid_t
create_vl_dset(hid_t file, hid_t type, hid_t space, const char *name)
{
    hid_t dset = H5I_INVALID_HID;

    if ((dset = H5Dcreate2(file, name, type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        STACK_ERROR;

    return dset;

error:
    return H5I_INVALID_HID;
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
    HDfprintf(stderr, "usage: %s [-W] [-V]\n", progname);
    HDfprintf(stderr, "\n  -W: do not wait for SIGINT or SIGUSR1\n");
    HDfprintf(stderr, "\n  -S: do not use VFD SWMR\n");
    HDfprintf(stderr, "  -f: use fixed-length string\n");
    HDfprintf(stderr, "      (default: variable-length string)\n");
    HDfprintf(stderr, "  -n: number of test steps to perform\n");
    HDfprintf(stderr, "  -q: be quiet: few/no progress messages\n");
    HDfprintf(stderr, "  -t (oob|null): select out-of-bounds or NULL test\n");
    HDexit(EXIT_FAILURE);
}

int
main(int argc, char **argv)
{
    hid_t                  fapl = H5I_INVALID_HID;
    hid_t fcpl = H5I_INVALID_HID;
    hid_t  fid = H5I_INVALID_HID;
    hid_t space = H5I_INVALID_HID;
    hid_t type = H5I_INVALID_HID;
    hid_t                  dset[2] = {H5I_INVALID_HID, H5I_INVALID_HID};
    char                   content[2][96];
    char                   name[2][96];
    H5F_t *                f;
    H5C_t *                cache;
    sigset_t               oldsigs;
    hbool_t                variable = TRUE, wait_for_signal = TRUE;
    const hsize_t          dims = 1;
    int                    opt, i, ntimes = 100;
    unsigned long          tmp;
    char *                 end;
    hbool_t                use_vfd_swmr = TRUE;
    const uint64_t         delay_ns     = 100 * 1000 * 1000; /* 100 ms */
    testsel_t              sel          = TEST_NONE;
    H5F_vfd_swmr_config_t  *config = NULL;
    const char *           s_opts   = "SWfn:qt:";
    struct h5_long_options l_opts[] = {{NULL, 0, '\0'}};

    HDassert(H5T_C_S1 != H5I_INVALID_HID);

    if (NULL == (config = HDcalloc(1, sizeof(H5F_vfd_swmr_config_t))))
        PUTS_ERROR("unable to allocate memory");

    while ((opt = H5_get_option(argc, (const char *const *)argv, s_opts, l_opts)) != EOF) {
        switch (opt) {
            case 'S':
                use_vfd_swmr = FALSE;
                break;
            case 'W':
                wait_for_signal = FALSE;
                break;
            case 'f':
                variable = FALSE;
                break;
            case 'n':
                errno = 0;
                tmp   = HDstrtoul(H5_optarg, &end, 0);
                if (end == H5_optarg || *end != '\0') {
                    HDfprintf(stderr, "couldn't parse `-n` argument `%s`", H5_optarg);
                    AT();
                    goto error;
                }
                else if (errno != 0) {
                    HDfprintf(stderr, "couldn't parse `-n` argument `%s`", H5_optarg);
                    AT();
                    goto error;
                }
                else if (tmp > INT_MAX) {
                    HDfprintf(stderr, "`-n` argument `%lu` too large", tmp);
                    AT();
                    goto error;
                }
                ntimes = (int)tmp;
                break;
            case 'q':
                verbosity = 1;
                break;
            case 't':
                if (HDstrcmp(H5_optarg, "oob") == 0)
                    sel = TEST_OOB;
                else if (HDstrcmp(H5_optarg, "null") == 0)
                    sel = TEST_NULL;
                else
                    usage(argv[0]);
                break;
            default:
                usage(argv[0]);
                break;
        }
    }
    argv += H5_optind;
    argc -= H5_optind;

    if (argc > 0)
        PUTS_ERROR("unexpected command-line arguments");

    /* config, tick_len, max_lag, presume_posix_semantics, writer,
     * maintain_metadata_file, generate_updater_files, flush_raw_data, md_pages_reserved,
     * md_file_path, md_file_name, updater_file_path */
    init_vfd_swmr_config(config, 4, 7, FALSE, true, TRUE, FALSE, TRUE, 128, "./", "vlstr-shadow", NULL);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    if ((fapl = vfd_swmr_create_fapl(TRUE, use_vfd_swmr, sel == TEST_OOB, 4096, config)) < 0)
        STACK_ERROR;

    /* Set fs_strategy (file space strategy) and fs_page_size (file space page size) */
    if ((fcpl = vfd_swmr_create_fcpl(H5F_FSPACE_STRATEGY_PAGE, 4096)) < 0)
        STACK_ERROR;

    if ((fid = H5Fcreate("vfd_swmr_vlstr.h5", H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        STACK_ERROR;

    /* Create the VL string datatype and a scalar dataspace, or a
     * fixed-length string datatype and a simple dataspace.
     */
    if ((type = H5Tcopy(H5T_C_S1)) == H5I_INVALID_HID)
        STACK_ERROR;

    if (!variable) {
        if (H5Tset_size(type, 32) < 0)
            STACK_ERROR;
        if ((space = H5Screate_simple(1, &dims, NULL)) < 0)
            STACK_ERROR;
    }
    else {
        if (H5Tset_size(type, H5T_VARIABLE) < 0)
            STACK_ERROR;
        if ((space = H5Screate(H5S_SCALAR)) < 0)
            STACK_ERROR;
    }

    if ((f = H5VL_object_verify(fid, H5I_FILE)) == NULL)
        STACK_ERROR;

    cache = f->shared->cache;

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
                (void)HDsnprintf(name[which], sizeof(name[which]), "dset-%d", which);
                (void)HDsnprintf(content[which], sizeof(content[which]), "content %d seq %d short", which,
                                 seq);
                dset[which] = create_vl_dset(fid, type, space, name[which]);
                if (write_vl_dset(dset[which], type, space, content[which]) < 0)
                    PUTS_ERROR("failed to write to VL dataset");
                break;
            case LENGTHEN:
                (void)HDsnprintf(content[which], sizeof(content[which]),
                                 "content %d seq %d long long long long long long long long", which, seq);
                if (write_vl_dset(dset[which], type, space, content[which]) < 0)
                    PUTS_ERROR("failed to write to VL dataset");
                break;
            case SHORTEN:
                (void)HDsnprintf(content[which], sizeof(content[which]),
                                 "content %d seq %d medium medium medium", which, seq);
                if (write_vl_dset(dset[which], type, space, content[which]) < 0)
                    PUTS_ERROR("failed to write to VL dataset");
                break;
            case DELETE:
                if (H5Dclose(dset[which]) < 0)
                    STACK_ERROR;
                if (H5Ldelete(fid, name[which], H5P_DEFAULT) < 0)
                    STACK_ERROR;
                break;
            default:
                PUTS_ERROR("Unknown operation");
        }

        if (caught_out_of_bounds) {
            HDfprintf(stderr, "caught out of bounds\n");
            break;
        }

        H5_nanosleep(delay_ns);
    }

    if (use_vfd_swmr && wait_for_signal)
        await_signal(fid);

    restore_signals(&oldsigs);

    if (H5Pclose(fapl) < 0)
        STACK_ERROR;
    if (H5Pclose(fcpl) < 0)
        STACK_ERROR;
    if (H5Tclose(type) < 0)
        STACK_ERROR;
    if (H5Sclose(space) < 0)
        STACK_ERROR;
    if (H5Fclose(fid) < 0)
        STACK_ERROR;

    HDfree(config);

    return EXIT_SUCCESS;

error:

    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
        H5Pclose(fcpl);
        H5Tclose(type);
        H5Sclose(space);
        H5Fclose(fid);
    }
    H5E_END_TRY;

    HDfree(config);

    return EXIT_FAILURE;
}

#else /* H5_HAVE_WIN32_API */

int
main(void)
{
    HDfprintf(stderr, "Non-POSIX platform. Skipping.\n");
    return EXIT_SUCCESS;
}

#endif /* H5_HAVE_WIN32_API */
