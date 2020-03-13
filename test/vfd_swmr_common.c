/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:     swmr_common.c
 *
 * Purpose:     Utility functions for the SWMR test code.
 *
 *-------------------------------------------------------------------------
 */

/***********/
/* Headers */
/***********/

#include <err.h>    /* for err(3) */

#include "h5test.h"
#include "vfd_swmr_common.h"

static const hid_t badhid = H5I_INVALID_HID;

int verbosity = 2;

void
dbgf(int level, const char *fmt, ...)
{
    va_list ap;

    if (verbosity < level)
        return;

    va_start(ap, fmt);
    (void)vprintf(fmt, ap);
    va_end(ap);
}

estack_state_t
disable_estack(void)
{
    estack_state_t es;

    (void)H5Eget_auto(H5E_DEFAULT, &es.efunc, &es.edata);
    (void)H5Eset_auto(H5E_DEFAULT, NULL, NULL);

    return es;
}

void
restore_estack(estack_state_t es)
{
    (void)H5Eset_auto(H5E_DEFAULT, es.efunc, es.edata);
}

void
block_signals(sigset_t *oldset)
{
    sigset_t fullset;

    if (sigfillset(&fullset) == -1) {
        err(EXIT_FAILURE, "%s.%d: could not initialize signal masks",
            __func__, __LINE__);
    }

    if (sigprocmask(SIG_BLOCK, &fullset, oldset) == -1)
        err(EXIT_FAILURE, "%s.%d: sigprocmask", __func__, __LINE__);
}

void
restore_signals(sigset_t *oldset)
{
    if (sigprocmask(SIG_SETMASK, oldset, NULL) == -1)
        err(EXIT_FAILURE, "%s.%d: sigprocmask", __func__, __LINE__);
}

void
await_signal(hid_t fid)
{
    sigset_t sleepset;
    struct timespec tick = {.tv_sec = 0, .tv_nsec = 1000000000 / 100};

    if (sigemptyset(&sleepset) == -1 ||
        sigaddset(&sleepset, SIGINT) == -1 ||
        sigaddset(&sleepset, SIGUSR1) == -1) {
        err(EXIT_FAILURE, "%s.%d: could not initialize signal masks",
            __func__, __LINE__);
    }

    for (;;) {
        const int rc = sigtimedwait(&sleepset, NULL, &tick);

        if (rc == SIGUSR1 || rc == SIGINT) {
            printf("Received %s, wrapping things up.\n", (rc == SIGUSR1) ? "SIGUSR1" : "SIGINT");
            break;
        } else if (rc == -1 && errno == EAGAIN) {
            estack_state_t es;

            /* Avoid deadlock with peer: periodically enter the API so that
             * tick processing occurs and data is flushed so that the peer
             * can see it.
             *
             * The call we make will fail, but that's ok,
             * so squelch errors.
             */
            es = disable_estack();
            (void)H5Aexists_by_name(fid, "nonexistent", "nonexistent",
                H5P_DEFAULT);
            restore_estack(es);
        } else if (rc == -1)
            err(EXIT_FAILURE, "%s: sigtimedwait", __func__);
    }
}

hid_t
vfd_swmr_create_fapl(bool writer, bool only_meta_pages, bool use_vfd_swmr)
{
    H5F_vfd_swmr_config_t config;
    hid_t fapl;

    /* Create file access property list */
    if((fapl = h5_fileaccess()) < 0) {
        warnx("h5_fileaccess");
        return badhid;
    }

    /* FOR NOW: set to use latest format, the "old" parameter is not used */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0) {
        warnx("H5Pset_libver_bounds");
        return badhid;
    }

    /*
     * Set up to open the file with VFD SWMR configured.
     */

    /* Enable page buffering */
    if(H5Pset_page_buffer_size(fapl, 4096, only_meta_pages ? 100 : 0, 0) < 0) {
        warnx("H5Pset_page_buffer_size");
        return badhid;
    }

    memset(&config, 0, sizeof(config));

    config.version = H5F__CURR_VFD_SWMR_CONFIG_VERSION;
    config.tick_len = 1;
    config.max_lag = 5;
    config.writer = writer;
    config.md_pages_reserved = 128;
    HDstrcpy(config.md_file_path, "./my_md_file");

    /* Enable VFD SWMR configuration */
    if(use_vfd_swmr && H5Pset_vfd_swmr_config(fapl, &config) < 0) {
        warnx("H5Pset_vfd_swmr_config");
        return badhid;
    }
    return fapl;
}
