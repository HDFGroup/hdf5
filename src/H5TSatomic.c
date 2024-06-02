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
 * Purpose: C11 atomic emulation outines
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

#ifdef H5_HAVE_THREADS

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
 * Purpose:     Initializes an atomic 'int' variable object with a value.
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
        (void)
    H5TS_mutex_init(&obj->mutex, H5TS_MUTEX_TYPE_PLAIN);

    /* Set the value */
    obj->value = desired;

    FUNC_LEAVE_NOAPI_VOID_NAMECHECK_ONLY
} /* end H5TS_atomic_init_int() */

/*--------------------------------------------------------------------------
 * Function:    H5TS_atomic_load_int
 *
 * Purpose:     Retrieves the value of atomic 'int' variable object.
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
        (void)
    H5TS_mutex_lock(&obj->mutex);

    /* Get the value */
    ret_value = obj->value;

    /* Release the object's mutex */
    H5TS_mutex_unlock(&obj->mutex);

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_atomic_load_int() */

/*--------------------------------------------------------------------------
 * Function:    H5TS_atomic_store_int
 *
 * Purpose:     Atomically replaces the value of the atomic 'int' variable
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
        (void)
    H5TS_mutex_lock(&obj->mutex);

    /* Set the value */
    obj->value = desired;

    /* Release the object's mutex */
    H5TS_mutex_unlock(&obj->mutex);

    FUNC_LEAVE_NOAPI_VOID_NAMECHECK_ONLY
} /* end H5TS_atomic_store_int() */

/*--------------------------------------------------------------------------
 * Function:    H5TS_atomic_fetch_add_int
 *
 * Purpose:     Atomically replaces the value of an atomic 'int' variable with the
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
        (void)
    H5TS_mutex_lock(&obj->mutex);

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
 * Purpose:     Atomically replaces the value of an atomic 'int' variable with the
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
        (void)
    H5TS_mutex_lock(&obj->mutex);

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
 * Purpose:     Destroys / releases resources for an atomic 'int' variable
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
        (void)
    H5TS_mutex_destroy(&obj->mutex);

    FUNC_LEAVE_NOAPI_VOID_NAMECHECK_ONLY
} /* end H5TS_atomic_destroy_int() */

/*--------------------------------------------------------------------------
 * Function:    H5TS_atomic_init_uint
 *
 * Purpose:     Initializes an atomic 'unsigned' variable object with a value.
 *
 * Note:        Per the C11 standard, this function is not atomic and
 *              concurrent execution from multiple threads is a data race.
 *
 * Return:      None
 *
 *--------------------------------------------------------------------------
 */
void
H5TS_atomic_init_uint(H5TS_atomic_uint_t *obj, unsigned desired)
{
    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

        /* Initialize mutex that protects the "atomic" value */
        (void)
    H5TS_mutex_init(&obj->mutex, H5TS_MUTEX_TYPE_PLAIN);

    /* Set the value */
    obj->value = desired;

    FUNC_LEAVE_NOAPI_VOID_NAMECHECK_ONLY
} /* end H5TS_atomic_init_uint() */

/*--------------------------------------------------------------------------
 * Function:    H5TS_atomic_load_uint
 *
 * Purpose:     Retrieves the value of atomic 'unsigned' variable object.
 *
 * Return:      None
 *
 *--------------------------------------------------------------------------
 */
unsigned
H5TS_atomic_load_uint(H5TS_atomic_uint_t *obj)
{
    unsigned ret_value;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

        /* Lock mutex that protects the "atomic" value */
        (void)
    H5TS_mutex_lock(&obj->mutex);

    /* Get the value */
    ret_value = obj->value;

    /* Release the object's mutex */
    H5TS_mutex_unlock(&obj->mutex);

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_atomic_load_uint() */

/*--------------------------------------------------------------------------
 * Function:    H5TS_atomic_store_uint
 *
 * Purpose:     Atomically replaces the value of the atomic 'unsigned' variable
 *
 * Return:      None
 *
 *--------------------------------------------------------------------------
 */
void
H5TS_atomic_store_uint(H5TS_atomic_uint_t *obj, unsigned desired)
{
    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

        /* Lock mutex that protects the "atomic" value */
        (void)
    H5TS_mutex_lock(&obj->mutex);

    /* Set the value */
    obj->value = desired;

    /* Release the object's mutex */
    H5TS_mutex_unlock(&obj->mutex);

    FUNC_LEAVE_NOAPI_VOID_NAMECHECK_ONLY
} /* end H5TS_atomic_store_uint() */

/*--------------------------------------------------------------------------
 * Function:    H5TS_atomic_fetch_add_uint
 *
 * Purpose:     Atomically replaces the value of an atomic 'unsigned' variable with the
 *              result of addition of the 'arg' to the old value of the
 *              atomic variable.
 *
 * Return:      Returns the value of the atomic variable held previously
 *
 *--------------------------------------------------------------------------
 */
unsigned
H5TS_atomic_fetch_add_uint(H5TS_atomic_uint_t *obj, unsigned arg)
{
    unsigned ret_value;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

        /* Lock mutex that protects the "atomic" value */
        (void)
    H5TS_mutex_lock(&obj->mutex);

    /* Get the current value */
    ret_value = obj->value;

    /* Increment the value */
    obj->value += arg;

    /* Release the object's mutex */
    H5TS_mutex_unlock(&obj->mutex);

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_atomic_fetch_add_uint() */

/*--------------------------------------------------------------------------
 * Function:    H5TS_atomic_fetch_sub_uint
 *
 * Purpose:     Atomically replaces the value of an atomic 'unsigned' variable with the
 *              result of subtracting the 'arg' from the old value of the
 *              atomic variable.
 *
 * Return:      Returns the value of the atomic variable held previously
 *
 *--------------------------------------------------------------------------
 */
unsigned
H5TS_atomic_fetch_sub_uint(H5TS_atomic_uint_t *obj, unsigned arg)
{
    unsigned ret_value;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

        /* Lock mutex that protects the "atomic" value */
        (void)
    H5TS_mutex_lock(&obj->mutex);

    /* Get the current value */
    ret_value = obj->value;

    /* Decrement the value */
    obj->value -= arg;

    /* Release the object's mutex */
    H5TS_mutex_unlock(&obj->mutex);

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_atomic_fetch_sub_uint() */

/*--------------------------------------------------------------------------
 * Function:    H5TS_atomic_destroy_uint
 *
 * Purpose:     Destroys / releases resources for an atomic 'unsigned' variable
 *
 * Note:        No equivalent in the C11 atomics, but needed here, to destroy
 *              the mutex used to protect the atomic value.
 *
 * Return:      None
 *
 *--------------------------------------------------------------------------
 */
void
H5TS_atomic_destroy_uint(H5TS_atomic_uint_t *obj)
{
    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

        /* Destroy mutex that protects the "atomic" value */
        (void)
    H5TS_mutex_destroy(&obj->mutex);

    FUNC_LEAVE_NOAPI_VOID_NAMECHECK_ONLY
} /* end H5TS_atomic_destroy_uint() */

/*--------------------------------------------------------------------------
 * Function:    H5TS_atomic_init_voidp
 *
 * Purpose:     Initializes an atomic 'void *' variable object with a value.
 *
 * Note:        Per the C11 standard, this function is not atomic and
 *              concurrent execution from multiple threads is a data race.
 *
 * Return:      None
 *
 *--------------------------------------------------------------------------
 */
void
H5TS_atomic_init_voidp(H5TS_atomic_voidp_t *obj, void *desired)
{
    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

        /* Initialize mutex that protects the "atomic" value */
        (void)
    H5TS_mutex_init(&obj->mutex, H5TS_MUTEX_TYPE_PLAIN);

    /* Set the value */
    obj->value = desired;

    FUNC_LEAVE_NOAPI_VOID_NAMECHECK_ONLY
} /* end H5TS_atomic_init_voidp() */

/*--------------------------------------------------------------------------
 * Function:    H5TS_atomic_exchange_voidp
 *
 * Purpose:     Atomically replaces the value of an atomic 'void *' variable
 *              and returns the value held previously.
 *
 * Return:      Returns the value of the atomic variable held previously
 *
 *--------------------------------------------------------------------------
 */
void *
H5TS_atomic_exchange_voidp(H5TS_atomic_voidp_t *obj, void *desired)
{
    void *ret_value;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

        /* Lock mutex that protects the "atomic" value */
        (void)
    H5TS_mutex_lock(&obj->mutex);

    /* Get the current value */
    ret_value = obj->value;

    /* Set the value */
    obj->value = desired;

    /* Release the object's mutex */
    H5TS_mutex_unlock(&obj->mutex);

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_atomic_exchange_voidp() */

/*--------------------------------------------------------------------------
 * Function:    H5TS_atomic_compare_exchange_strong_voidp
 *
 * Purpose:     Atomically compares the contents of 'obj' with 'expected', and
 *              if those are bitwise equal, replaces the former with 'desired'
 *              (performs read-modify-write operation). Otherwise, loads the
 *              actual contents of 'obj' into '*expected' (performs load
 *              operation).
 *
 * Return:      The result of the comparison: true if 'obj' was equal to
 *              'expected', false otherwise.
 *
 *--------------------------------------------------------------------------
 */
bool
H5TS_atomic_compare_exchange_strong_voidp(H5TS_atomic_voidp_t *obj, void **expected, void *desired)
{
    bool ret_value;

    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

        /* Lock mutex that protects the "atomic" value */
        (void)
    H5TS_mutex_lock(&obj->mutex);

    /* Compare 'obj' w/'expected' */
    if (obj->value == *expected) {
        obj->value = desired;
        ret_value  = true;
    }
    else {
        *expected = obj->value;
        ret_value = false;
    }
    /* Release the object's mutex */
    H5TS_mutex_unlock(&obj->mutex);

    FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)
} /* end H5TS_atomic_compare_exchange_strong_voidp() */

/*--------------------------------------------------------------------------
 * Function:    H5TS_atomic_destroy_voidp
 *
 * Purpose:     Destroys / releases resources for an atomic 'void *' variable
 *
 * Note:        No equivalent in the C11 atomics, but needed here, to destroy
 *              the mutex used to protect the atomic value.
 *
 * Return:      None
 *
 *--------------------------------------------------------------------------
 */
void
H5TS_atomic_destroy_voidp(H5TS_atomic_voidp_t *obj)
{
    FUNC_ENTER_NOAPI_NAMECHECK_ONLY

        /* Destroy mutex that protects the "atomic" value */
        (void)
    H5TS_mutex_destroy(&obj->mutex);

    FUNC_LEAVE_NOAPI_VOID_NAMECHECK_ONLY
} /* end H5TS_atomic_destroy_voidp() */

#endif /* H5_HAVE_STDATOMIC_H */

#endif /* H5_HAVE_THREADS */
