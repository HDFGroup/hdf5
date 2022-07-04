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

#include "testhdf5.h"
#include "vfd_swmr_common.h"

#ifndef H5_HAVE_WIN32_API

typedef enum _step { CREATE = 0, LENGTHEN, SHORTEN, DELETE, NSTEPS } step_t;

static hbool_t caught_out_of_bounds = FALSE;
static hbool_t read_null            = FALSE;

static hbool_t
read_vl_dset(hid_t dset, hid_t type, char **data)
{
    hbool_t        success;
    estack_state_t es;

    es      = disable_estack();
    success = H5Dread(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) >= 0;
    if (*data == NULL) {
        read_null = TRUE;
        return FALSE;
    }
    restore_estack(es);

    return success;
}

static void
usage(const char *progname)
{
    HDfprintf(stderr, "usage: %s [-W] [-V] [-t (oob|null)] \n", progname);
    HDfprintf(stderr, "\n  -S: do not use VFD SWMR\n");
    HDfprintf(stderr, "  -n: number of test steps to perform\n");
    HDfprintf(stderr, "  -q: be quiet: few/no progress messages\n");
    HDfprintf(stderr, "  -t (oob|null): select out-of-bounds or NULL test\n");
    HDexit(EXIT_FAILURE);
}

int
main(int argc, char **argv)
{
    hid_t                  fapl = H5I_INVALID_HID;
    hid_t fid = H5I_INVALID_HID;
    hid_t  space = H5I_INVALID_HID;
    hid_t type = H5I_INVALID_HID;
    hid_t                  dset[2] = {H5I_INVALID_HID, H5I_INVALID_HID};
    char *                 content[2] = {NULL, NULL};
    char                   name[2][96];
    int                    opt;
    int                    ntimes = 100;
    unsigned long          tmp;
    hbool_t                use_vfd_swmr = TRUE;
    char *                 end;
    const uint64_t         delay_ns = 1100 * 1000; /* 1.1 ms */
    testsel_t              sel      = TEST_NONE;
    H5F_vfd_swmr_config_t  *config;
    const char *           s_opts   = "Sn:qt:";
    struct h5_long_options l_opts[] = {{NULL, 0, '\0'}};

    HDassert(H5T_C_S1 != H5I_INVALID_HID);

    if (NULL == (config = HDcalloc(1, sizeof(H5F_vfd_swmr_config_t))))
        PUTS_ERROR("memory allocation failed");

    while ((opt = H5_get_option(argc, (const char *const *)argv, s_opts, l_opts)) != EOF) {
        switch (opt) {
            case 'S':
                use_vfd_swmr = FALSE;
                break;
            case 'n':
                errno = 0;
                tmp   = HDstrtoul(H5_optarg, &end, 0);
                if (end == optarg || *end != '\0') {
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
    init_vfd_swmr_config(config, 4, 7, FALSE, false, TRUE, FALSE, TRUE, 128, "./", "vlstr-shadow", NULL);

    /* use_latest_format, use_vfd_swmr, only_meta_page, page_buf_size, config */
    if ((fapl = vfd_swmr_create_fapl(TRUE, use_vfd_swmr, sel == TEST_OOB, 4096, config)) < 0)
        STACK_ERROR;

    if ((fid = H5Fopen("vfd_swmr_vlstr.h5", H5F_ACC_RDONLY, fapl)) < 0)
        STACK_ERROR;

    /* Create the VL string datatype and a scalar dataspace */
    if ((type = H5Tcopy(H5T_C_S1)) == H5I_INVALID_HID)
        STACK_ERROR;

    if (H5Tset_size(type, H5T_VARIABLE) < 0)
        STACK_ERROR;
    if ((space = H5Screate(H5S_SCALAR)) < 0)
        STACK_ERROR;

    /* content 0 seq 1 short
     * content 1 seq 1 long long long long long long long long
     * content 1 seq 1 medium medium medium
     */
    for (int i = 0; !caught_out_of_bounds && i < ntimes; i++) {
        estack_state_t es;
        const int      ndsets = 2;
        const int      which  = i % ndsets;
        int            nconverted;
        struct {
            int  which;
            int  seq;
            char tail[96];
        } scanned_content;

        dbgf(2, "iteration %d which %d", i, which);
        (void)HDsnprintf(name[which], sizeof(name[which]), "dset-%d", which);
        es          = disable_estack();

        dset[which] = H5Dopen2(fid, name[which], H5P_DEFAULT);
        restore_estack(es);
        if (caught_out_of_bounds || dset[which] == H5I_INVALID_HID) {
            dbgf(2, ": couldn't open\n");
            continue;
        }
        if (!read_vl_dset(dset[which], type, &content[which])) {
            H5Dclose(dset[which]);
            dbgf(2, ": couldn't read\n");
            continue;
        }
        nconverted = HDsscanf(content[which], "content %d seq %d %96s", &scanned_content.which,
                              &scanned_content.seq, scanned_content.tail);
        if (nconverted != 3) {
            dbgf(2, ": couldn't scan\n");
            continue;
        }
        dbgf(2, ": read which %d seq %d tail %s\n", scanned_content.which, scanned_content.seq,
             scanned_content.tail);
        H5Dclose(dset[which]);

        if (content[which] != NULL) {
            HDfree(content[which]);
            content[which] = NULL;
        }

        if (i % 2 == 0)
            H5_nanosleep(delay_ns);
    }

    if (caught_out_of_bounds)
        HDfprintf(stderr, "caught out of bounds\n");

    if (read_null)
        HDfprintf(stderr, "read NULL\n");

    if (H5Pclose(fapl) < 0)
        STACK_ERROR;
    if (H5Tclose(type) < 0)
        STACK_ERROR;
    if (H5Sclose(space) < 0)
        STACK_ERROR;
    if (H5Fclose(fid) < 0)
        STACK_ERROR;

    HDfree(config);

    if (sel == TEST_OOB)
        return caught_out_of_bounds ? EXIT_SUCCESS : EXIT_FAILURE;
    else if (sel == TEST_NULL)
        return read_null ? EXIT_SUCCESS : EXIT_FAILURE;

    return EXIT_SUCCESS;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
        H5Tclose(type);
        H5Sclose(space);
        H5Fclose(fid);
    }
    H5E_END_TRY;

    HDfree(config);
}

#else /* H5_HAVE_WIN32_API */

int
main(void)
{
    HDfprintf(stderr, "Non-POSIX platform. Skipping.\n");
    return EXIT_SUCCESS;
}

#endif /* H5_HAVE_WIN32_API */
