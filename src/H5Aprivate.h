/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * This file contains private information about the H5D module
 */
#ifndef _H5Aprivate_H
#define _H5Aprivate_H

/* Include package's public header */
#include "H5Apublic.h"

/* Private headers needed by this file */
#include "H5Gprivate.h"		/* Groups				*/


/**************************/
/* Library Private Macros */
/**************************/


/****************************/
/* Library Private Typedefs */
/****************************/

/* Forward references of package typedefs */
typedef struct H5A_t H5A_t;
typedef struct H5A_attr_iter_op_t H5A_attr_iter_op_t;


/*****************************/
/* Library-private Variables */
/*****************************/


/***************************************/
/* Library-private Function Prototypes */
/***************************************/
struct H5O_t; /*forward decl*/

/* General attribute routines */
H5_DLL struct H5O_loc_t *H5A_oloc(H5A_t *attr);
H5_DLL H5G_name_t *H5A_nameof(H5A_t *attr);

/* Attribute "dense" storage routines */
H5_DLL herr_t H5A_dense_create(H5F_t *f, hid_t dxpl_id, struct H5O_t *oh);
H5_DLL herr_t H5A_dense_insert(H5F_t *f, hid_t dxpl_id, const struct H5O_t *oh,
    unsigned mesg_flags, const H5A_t *attr);
H5_DLL herr_t H5A_dense_delete(H5F_t *f, hid_t dxpl_id, struct H5O_t *oh);

#endif /* _H5Aprivate_H */

