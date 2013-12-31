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
 * This file contains private information about the H5TR module
 */
#ifndef _H5TRprivate_H
#define _H5TRprivate_H

/* Include package's public header */
#include "H5TRpublic.h"

/* Private headers needed by this file */
#include "H5RCprivate.h"	/* Read Contexts			*/

/**************************/
/* Library Private Macros */
/**************************/
#define H5TR_FINISH_ACQUIRE_NAME "acquire_after_finishing"

#ifdef H5_HAVE_EFF

/****************************/
/* Library Private Typedefs */
/****************************/

/* the transaction struct */
typedef struct H5TR_t {
    H5VL_iod_req_info_t req_info; /* must be first */
    struct H5VL_iod_file_t *file;
    uint64_t c_version;
    uint64_t trans_num;
} H5TR_t;

/*****************************/
/* Library Private Variables */
/*****************************/

/******************************/
/* Library Private Prototypes */
/******************************/
herr_t H5TR_init(void);

H5_DLL H5TR_t *H5TR_create(void *file, H5RC_t *rc, uint64_t trans_num);
H5_DLL herr_t H5TR_close(H5TR_t *tr);

#endif /* H5_HAVE_EFF */
#endif /* _H5TRprivate_H */
