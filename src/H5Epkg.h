/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:	Quincey Koziol <koziol@hdfgroup.org>
 *		Wednesday, April 11, 2007
 *
 * Purpose:	This file contains declarations which are visible only within
 *		the H5E package.  Source files outside the H5E package should
 *		include H5Eprivate.h instead.
 */
#ifndef H5E_PACKAGE
#error "Do not include this file outside the H5E package!"
#endif

#ifndef _H5Epkg_H
#define _H5Epkg_H

/* Get package's private header */
#include "H5Eprivate.h"

/* Other private headers needed by this file */


/**************************/
/* Package Private Macros */
/**************************/

/* Amount to indent each error */
#define H5E_INDENT              2

#ifdef H5_HAVE_THREADSAFE
/*
 * The per-thread error stack. pthread_once() initializes a special
 * key that will be used by all threads to create a stack specific to
 * each thread individually. The association of stacks to threads will
 * be handled by the pthread library.
 *
 * In order for this macro to work, H5E_get_my_stack() must be preceeded
 * by "H5E_t *estack =".
 */
#define H5E_get_my_stack()  H5E_get_stack()
#else /* H5_HAVE_THREADSAFE */
/*
 * The current error stack.
 */
#define H5E_get_my_stack() (H5E_stack_g + 0)
#endif /* H5_HAVE_THREADSAFE */


/****************************/
/* Package Private Typedefs */
/****************************/

/* Some syntactic sugar to make the compiler happy with two different kinds of callbacks */
typedef union {
    H5E_auto_t efunc;           /* Old-style callback, NO error stack param. */
    H5E_auto2_t efunc2;         /* New-style callback, with error stack param. */
} H5E_auto_op_t;


/*****************************/
/* Package Private Variables */
/*****************************/

#ifndef H5_HAVE_THREADSAFE
/*
 * The current error stack.
 */
H5_DLLVAR H5E_t	H5E_stack_g[1];
#endif /* H5_HAVE_THREADSAFE */


/******************************/
/* Package Private Prototypes */
/******************************/
#ifdef H5_HAVE_THREADSAFE
H5_DLL H5E_t *H5E_get_stack(void);
#endif /* H5_HAVE_THREADSAFE */
H5_DLL ssize_t H5E_get_msg(const H5E_msg_t *msg_ptr, H5E_type_t *type,
    char *msg, size_t size);
H5_DLL herr_t H5E_print2(const H5E_t *estack, FILE *stream, hbool_t bk_compat);
H5_DLL herr_t H5E_walk2(const H5E_t *estack, H5E_direction_t direction,
    H5E_walk_t func, H5E_walk2_t stack_func, hbool_t bk_compatible,
    void *client_data);
H5_DLL herr_t H5E_get_auto2(const H5E_t *estack, hbool_t new_api,
    H5E_auto_op_t *func, void **client_data);
H5_DLL herr_t H5E_set_auto2(H5E_t *estack, hbool_t new_api,
    H5E_auto_op_t *func, void *client_data);
H5_DLL herr_t H5E_pop(H5E_t *err_stack, size_t count);

#endif /* _H5HFpkg_H */

