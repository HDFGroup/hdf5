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

/*
 * Purpose: Pthread threadsafety routines
 *
 * Note:  Because this threadsafety framework operates outside the library,
 *        it does not use the error stack (although it does use error macros
 *        that don't push errors on a stack) and only uses the "namecheck only"
 *        FUNC_ENTER_* / FUNC_LEAVE_* macros.
 */

/****************/
/* Module Setup */
/****************/

#include "H5TSmodule.h" /* This source code file is part of the H5TS module */
#define H5TS_TESTING    /* Suppress warning about H5TS testing funcs */

/***********/
/* Headers */
/***********/
#include "H5private.h"  /* Generic Functions                   */
#include "H5Eprivate.h" /* Error handling                      */
#include "H5TSpkg.h"    /* Threadsafety                        */

#ifdef H5_HAVE_THREADSAFE

#ifndef H5_HAVE_WIN_THREADS

/****************/
/* Local Macros */
/****************/

/******************/
/* Local Typedefs */
/******************/

/********************/
/* Local Prototypes */
/********************/

/*********************/
/* Package Variables */
/*********************/

/*****************************/
/* Library Private Variables */
/*****************************/

/*******************/
/* Local Variables */
/*******************/

/*--------------------------------------------------------------------------
 * Function:    H5TS__pthread_first_thread_init
 *
 * Purpose:     Initialize global API lock, keys for per-thread error stacks
 *              and cancallability information. Called by the first thread
 *              that enters the library.
 *
 * Return:      None
 *
 *--------------------------------------------------------------------------
 */
void
H5TS__pthread_first_thread_init(void)
{
    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

    /* Initialize global API lock */
    H5TS__ex_lock_init(&H5TS_api_info_p.api_lock, true);

    /* Initialize the "lock acquisition attempt" mutex & counter */
    H5TS_mutex_init(&H5TS_api_info_p.attempt_mutex);
    H5TS_api_info_p.attempt_lock_count = 0;

    /* Set up thread-local thread-info struct */
    H5TS__tinfo_init();

    FUNC_LEAVE_NOAPI_VOID_NAMECHECK_ONLY
} /* end H5TS__pthread_first_thread_init() */

#endif /* H5_HAVE_WIN_THREADS */

#endif /* H5_HAVE_THREADSAFE */
