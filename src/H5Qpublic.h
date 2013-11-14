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
 * This file contains function prototypes for each exported function in the
 * H5Q module.
 */
#ifndef _H5Qpublic_H
#define _H5Qpublic_H

/* Public headers needed by this file */
#include "H5public.h"
#include "H5Ipublic.h"

/*****************/
/* Public Macros */
/*****************/


/*******************/
/* Public Typedefs */
/*******************/

/* Query type */
typedef enum H5Q_type_t {
    H5Q_TYPE_DATA_ELEM,  /* selects data elements */
    H5Q_TYPE_ATTR_NAME,  /* selects attributes */
    H5Q_TYPE_LINK_NAME   /* selects objects */
} H5Q_type_t;

/* Query match conditions */
typedef enum H5Q_match_op_t {
    H5Q_MATCH_EQUAL,        /* equal */
    H5Q_MATCH_NOT_EQUAL,    /* not equal */
    H5Q_MATCH_LESS_THAN,    /* less than */
    H5Q_MATCH_GREATER_THAN  /* greater than */
} H5Q_match_op_t;

/* Query combine operators */
typedef enum H5Q_combine_op_t {
    H5Q_COMBINE_AND,
    H5Q_COMBINE_OR
} H5Q_combine_op_t;

/********************/
/* Public Variables */
/********************/

/*********************/
/* Public Prototypes */
/*********************/
#ifdef __cplusplus
extern "C" {
#endif

/* Function prototypes */
H5_DLL hid_t H5Qcreate(H5Q_type_t query_type, H5Q_match_op_t match_op, ...);
H5_DLL herr_t H5Qclose(hid_t query_id);
H5_DLL hid_t H5Qcombine(hid_t query_id1, H5Q_combine_op_t combine_op, hid_t query_id2);
H5_DLL herr_t H5Qapply(hid_t query_id, hbool_t *result, hid_t type_id, const void *elem);
/* or return dataspace and have
 * hid_t H5Qapply(query_id, result, enum data_elem/link/attr, nelem, void*);
 */

/* Encode / decode */
H5_DLL herr_t H5Qencode(hid_t query_id, void *buf, size_t *nalloc);
H5_DLL hid_t H5Qdecode(const void *buf);

#ifdef __cplusplus
}
#endif
#endif /* _H5Qpublic_H */
