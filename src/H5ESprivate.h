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
 * This file contains private information about the H5ES module
 */
#ifndef _H5ESprivate_H
#define _H5ESprivate_H

/* Include package's public header */
#include "H5ESpublic.h"

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
    H5ES_status_t status;
    struct H5_priv_request_t *prev;
    struct H5_priv_request_t *next;
} H5_priv_request_t;

/* The private request structure */
typedef struct H5ES_t {
    size_t size;             /* number of events in the stack */
    size_t in_progress;      /* number of events that the user did not call wait on */
    H5_priv_request_t *head;
    H5_priv_request_t *tail;
} H5ES_t;


/*****************************/
/* Library Private Variables */
/*****************************/


/******************************/
/* Library Private Prototypes */
/******************************/

herr_t H5ES_init(void);

/* API wrappers */
H5_DLL H5ES_t *H5ES_create(H5VL_t **plugin, H5P_genplist_t *plist);
H5_DLL herr_t H5ES_insert(hid_t es_id, H5_priv_request_t *req);
H5_DLL herr_t H5ES_wait(H5ES_t *e_stack, size_t event_idx, H5ES_status_t *_status/*OUT*/);
H5_DLL herr_t H5ES_wait_all(H5ES_t *e_stack, H5ES_status_t *_status/*OUT*/);
H5_DLL herr_t H5ES_test(H5ES_t *e_stack, size_t event_idx, H5ES_status_t *_status/*OUT*/);
H5_DLL herr_t H5ES_test_all(H5ES_t *e_stack, H5ES_status_t *_status/*OUT*/);
H5_DLL herr_t H5ES_cancel(H5ES_t *e_stack, size_t event_idx, H5ES_status_t *_status/*OUT*/);
H5_DLL herr_t H5ES_cancel_all(H5ES_t *e_stack, H5ES_status_t *_status/*OUT*/);
H5_DLL herr_t H5ES_close(H5ES_t *e_stack);

#endif /* _H5ESprivate_H */
