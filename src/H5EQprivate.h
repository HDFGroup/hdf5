/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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
 * This file contains private information about the H5EQ module
 */
#ifndef _H5EQprivate_H
#define _H5EQprivate_H

/* Include package's public header */
#include "H5EQpublic.h"

/* Private headers needed by this file */
#include "H5private.h"		/* Generic Functions			*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5VLprivate.h"	/* VOL plugins				*/

/**************************/
/* Library Private Macros */
/**************************/

/****************************/
/* Library Private Typedefs */
/****************************/

/* The private request structure */
typedef struct H5_priv_request_t {
    void   *req;            /* pointer to the request belonging to a VOL plugin */
    H5VL_t *vol_plugin;     /* the vol plugin that owns this request */
    struct H5_priv_request_t *prev;
    struct H5_priv_request_t *next;
} H5_priv_request_t;

/* The private request structure */
typedef struct H5EQ_t {
    int size;
    H5_priv_request_t *head;
    H5_priv_request_t *tail;
} H5EQ_t;


/*****************************/
/* Library Private Variables */
/*****************************/


/******************************/
/* Library Private Prototypes */
/******************************/

H5_DLL int H5EQ_term_interface(void);
herr_t H5EQ_init(void);

/* API wrappers */
H5_DLL H5EQ_t *H5EQ_create(H5VL_t **plugin, H5P_genplist_t *plist);
H5_DLL herr_t H5EQ_insert(H5EQ_t *eq, H5_priv_request_t *req);
H5_DLL herr_t H5EQ_wait(H5EQ_t *eq, H5VL_t *vol_plugin, int *num_requests, H5_status_t **status);
H5_DLL herr_t H5EQ_test(H5EQ_t *eq, int *num_remaining);
H5_DLL herr_t H5EQ_close(void *grp, H5VL_t *vol_plugin);

#endif /* _H5EQprivate_H */
