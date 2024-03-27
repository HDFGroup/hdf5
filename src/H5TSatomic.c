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
 * Purpose: C11 atomicr emulation outines
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

/***********/
/* Headers */
/***********/
#include "H5private.h"  /* Generic Functions                   */
#include "H5Eprivate.h" /* Error handling                      */
#include "H5TSpkg.h"    /* Threadsafety                        */

#ifdef H5_HAVE_THREADSAFE

#ifndef H5_HAVE_STDATOMIC_H

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
 * Function:    H5TS_atomic_init_int
 *
 * Purpose:     Initializes an atomic variable object with a value.
 *
 * Note:        Per the C11 standard, this function is not atomic and
 *              concurrent execution from multiple threads is a data race.
 *
 * Return:      None
 *
 *--------------------------------------------------------------------------
 */
void
H5TS_atomic_init_int(H5TS_atomic_int_t *obj, int desired)
{
    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

        /* Initialize mutex that protects the "atomic" value */
        (void) H5TS_mutex_init(&obj->mutex);

    /* Set the value */
    obj->value = desired;

    FUNC_LEAVE_NOAPI_VOID_NAMECHECK_ONLY
} /* end H5TS_atomic_init_int() */

/*--------------------------------------------------------------------------
 * Function:    H5TS_atomic_load_int
 *
 * Purpose:     Retrives the value of atomic variable object.
 *
 * Return:      None
 *
 *--------------------------------------------------------------------------
 */
int
H5TS_atomic_load_int(H5TS_atomic_int_t *obj)
{
    int ret_value;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

        /* Lock mutex that protects the "atomic" value */
        (void) H5TS_mutex_lock(&obj->mutex);

    /* Get the value */
    ret_value = obj->value;

    /* Release the object's mutex */
    H5TS_mutex_unlock(&obj->mutex);

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_atomic_init_int() */

/*--------------------------------------------------------------------------
 * Function:    H5TS_atomic_store_int
 *
 * Purpose:     Atomically replaces the value of the atomic variable
 *
 * Return:      None
 *
 *--------------------------------------------------------------------------
 */
void
H5TS_atomic_store_int(H5TS_atomic_int_t *obj, int desired)
{
    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

        /* Lock mutex that protects the "atomic" value */
        (void) H5TS_mutex_lock(&obj->mutex);

    /* Set the value */
    obj->value = desired;

    /* Release the object's mutex */
    H5TS_mutex_unlock(&obj->mutex);

    FUNC_LEAVE_NOAPI_VOID_NAMECHECK_ONLY
} /* end H5TS_atomic_store_int() */

/*--------------------------------------------------------------------------
 * Function:    H5TS_atomic_fetch_add_int
 *
 * Purpose:     Atomically replaces the value of an atomic variable with the
 *              result of addition of the 'arg' to the old value of the
 *              atomic variable.
 *
 * Return:      Returns the value of the atomic variable held previously
 *
 *--------------------------------------------------------------------------
 */
int
H5TS_atomic_fetch_add_int(H5TS_atomic_int_t *obj, int arg)
{
    int ret_value;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

        /* Lock mutex that protects the "atomic" value */
        (void) H5TS_mutex_lock(&obj->mutex);

    /* Get the current value */
    ret_value = obj->value;

    /* Increment the value */
    obj->value += arg;

    /* Release the object's mutex */
    H5TS_mutex_unlock(&obj->mutex);

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_atomic_fetch_add_int() */

/*--------------------------------------------------------------------------
 * Function:    H5TS_atomic_fetch_sub_int
 *
 * Purpose:     Atomically replaces the value of an atomic variable with the
 *              result of subtracting the 'arg' from the old value of the
 *              atomic variable.
 *
 * Return:      Returns the value of the atomic variable held previously
 *
 *--------------------------------------------------------------------------
 */
int
H5TS_atomic_fetch_sub_int(H5TS_atomic_int_t *obj, int arg)
{
    int ret_value;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

        /* Lock mutex that protects the "atomic" value */
        (void) H5TS_mutex_lock(&obj->mutex);

    /* Get the current value */
    ret_value = obj->value;

    /* Decrement the value */
    obj->value -= arg;

    /* Release the object's mutex */
    H5TS_mutex_unlock(&obj->mutex);

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_atomic_fetch_sub_int() */

/*--------------------------------------------------------------------------
 * Function:    H5TS_atomic_destroy_int
 *
 * Purpose:     Destroys / releases resources for an atomic variable
 *
 * Note:        No equivalent in the C11 atomics, but needed here, to destroy
 *              the mutex used to protect the atomic value.
 *
 * Return:      None
 *
 *--------------------------------------------------------------------------
 */
void
H5TS_atomic_destroy_int(H5TS_atomic_int_t *obj)
{
    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

        /* Destroy mutex that protects the "atomic" value */
        (void) H5TS_mutex_destroy(&obj->mutex);

    FUNC_LEAVE_NOAPI_VOID_NAMECHECK_ONLY
} /* end H5TS_atomic_destroy_int() */

#endif /* H5_HAVE_THREADS_H */

#endif /* H5_HAVE_THREADSAFE */
