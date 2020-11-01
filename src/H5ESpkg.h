/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:	Quincey Koziol <koziol@lbl.gov>
 *		Wednesday, April 8, 2020
 *
 * Purpose:	This file contains declarations which are visible only within
 *		the H5ES package.  Source files outside the H5ES package should
 *		include H5ESprivate.h instead.
 */
#if !(defined H5ES_FRIEND || defined H5ES_MODULE)
#error "Do not include this file outside the H5ES package!"
#endif

#ifndef _H5ESpkg_H
#define _H5ESpkg_H

/* Get package's private header */
#include "H5ESprivate.h"

/* Other private headers needed by this file */

/**************************/
/* Package Private Macros */
/**************************/

/****************************/
/* Package Private Typedefs */
/****************************/

/* Event nodes */
typedef struct H5ES_event_t H5ES_event_t;

/* Typedef for event set objects */
struct H5ES_t {
    uint64_t tot_count; /* Total # of operations inserted into this set */

    size_t        act_count;   /* # of active events in set */
    H5ES_event_t *head, *tail; /* Head & tail of active events */

    hbool_t       err_occurred;        /* Flag for error from an operation */
    size_t        err_count;           /* # of failed events in set */
    H5ES_event_t *err_head, *err_tail; /* Head & tail of failed events */
};

/*****************************/
/* Package Private Variables */
/*****************************/

/******************************/
/* Package Private Prototypes */
/******************************/
H5_DLL H5ES_t *H5ES__create(void);
H5_DLL herr_t  H5ES__test(H5ES_t *es, H5ES_status_t *status);
H5_DLL herr_t  H5ES__wait(H5ES_t *es, uint64_t timeout, H5ES_status_t *status, hbool_t allow_early_exit);
H5_DLL herr_t  H5ES__close(H5ES_t *es);

#endif /* _H5ESpkg_H */
