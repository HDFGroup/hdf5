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

        if (rc == SIGUSR1) {
            printf("Received SIGUSR1, wrapping things up.\n");
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
