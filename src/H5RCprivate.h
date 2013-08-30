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
 * This file contains private information about the H5RC module
 */
#ifndef _H5RCprivate_H
#define _H5RCprivate_H

/* Include package's public header */
#include "H5RCpublic.h"

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
/* the client Read Context struct */
typedef struct H5RC_t {
    void *file;
    uint64_t c_version;
} H5RC_t;

/*****************************/
/* Library Private Variables */
/*****************************/

/******************************/
/* Library Private Prototypes */
/******************************/
#if 0
herr_t H5RC_init(void);

/* API wrappers */
H5_DLL H5RC_t *H5RC_create(void *file, uint64_t c_version);
H5_DLL herr_t H5RC_close(H5RC_t *rc);

H5_DLL herr_t H5VL_iod_rc_acquire(H5VL_iod_file_t *file, H5RC_t *rc, void **req);
H5_DLL herr_t H5VL_iod_rc_release(H5RC_t *rc, void **req);
#endif
 
#endif /* _H5RCprivate_H */
