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
 * This file contains private information about the H5FF module
 */
#ifndef _H5FFprivate_H
#define _H5FFprivate_H

/* Include package's public header */
#include "H5FFpublic.h"

/* Private headers needed by this file */
#include "H5private.h"          /* Generic Functions			*/
#include "H5VLdaosm.h"          /* DAOS-M VOL plugin			*/

/**************************/
/* Library Private Macros */
/**************************/

/* DXPL property to store the transaction ID from the FF wrappers */
#define H5VL_TRANS_ID        "transaction_id"

/****************************/
/* Library Private Typedefs */
/****************************/


/*****************************/
/* Library Private Variables */
/*****************************/


/******************************/
/* Library Private Prototypes */
/******************************/

#ifdef H5_HAVE_EFF

#endif /* H5_HAVE_EFF */

#endif /* _H5FFprivate_H */

