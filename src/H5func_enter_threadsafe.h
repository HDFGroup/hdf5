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

/*-------------------------------------------------------------------------
 * H5func_enter_threadsafe.h
 *
 * Macros used to handle treading and thread-safety concerns (like the
 * global API lock) in HDF5 library API calls
 *-------------------------------------------------------------------------
 */

#ifndef H5func_enter_threadsafe_H
#define H5func_enter_threadsafe_H

#ifdef H5_HAVE_THREADSAFE

/* Lock headers */
#include "H5TSprivate.h"

/* Thread cancellation is only possible w/pthreads */
#if defined(H5_HAVE_PTHREAD_H)
/* Local variable for saving cancellation state */
#define H5CANCEL_DECL int oldstate = 0;

/* Disable & restore canceling the thread */
#define H5TS_DISABLE_CANCEL                                                                                  \
    do {                                                                                                     \
        pthread_setcancelstate(PTHREAD_CANCEL_DISABLE, &oldstate);                                           \
    } while (0)
#define H5TS_RESTORE_CANCEL                                                                                  \
    do {                                                                                                     \
        pthread_setcancelstate(oldstate, NULL);                                                              \
    } while (0)
#else
/* Local variable for saving cancellation state */
#define H5CANCEL_DECL       /* */

/* Disable & restore canceling the thread */
#define H5TS_DISABLE_CANCEL /* */
#define H5TS_RESTORE_CANCEL /* */
#endif

/* Macros for entering & leaving an API routine in a threadsafe manner */
#define H5_API_LOCK                                                                                          \
    /* Acquire the API lock */                                                                               \
    H5TS_api_lock();                                                                                         \
                                                                                                             \
    /* Set thread cancellation state to 'disable', and remember previous state */                            \
    H5TS_DISABLE_CANCEL;
#define H5_API_UNLOCK                                                                                        \
    /* Release the API lock */                                                                               \
    H5TS_api_unlock();                                                                                       \
                                                                                                             \
    /* Restore previous thread cancellation state */                                                         \
    H5TS_RESTORE_CANCEL;
#else                 /* H5_HAVE_THREADSAFE */

/* Local variable for saving cancellation state */
#define H5CANCEL_DECL /* */

/* No locks (non-threadsafe builds) */
#define H5_API_LOCK   /* */
#define H5_API_UNLOCK /* */

#endif /* H5_HAVE_THREADSAFE */

#endif /* H5func_enter_threadsafe_H */
