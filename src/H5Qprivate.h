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
 * This file contains private information about the H5Q module
 */
#ifndef _H5Qprivate_H
#define _H5Qprivate_H

/* Include package's public header */
#include "H5Qpublic.h"

/* Private headers needed by this file */
#include "H5Tprivate.h"

/**************************/
/* Library Private Macros */
/**************************/

/****************************/
/* Library Private Typedefs */
/****************************/

/* Data element */
struct H5Q_data_elem {
    H5T_t *type; /* type */
    size_t type_size; /* type size */
    void  *value; /* value */
};

/* Attribute name */
struct H5Q_attr_name {
    char *name; /* name */
};

/* Link name */
struct H5Q_link_name {
    char *name; /* name */
};

/* Query */
typedef struct H5Q_t H5Q_t;

/* Combine query */
struct H5Q_combine {
    H5Q_combine_op_t op; /* op */
    H5Q_t *l_query; /* left op query */
    H5Q_t *r_query; /* right op query */
};

/* Select query */
struct H5Q_select {
    H5Q_type_t type; /* type */
    H5Q_match_op_t match_op; /* match op */
    union {
        struct H5Q_data_elem data_elem;
        struct H5Q_attr_name attr_name;
        struct H5Q_link_name link_name;
    } elem;
};

/* Query */
struct H5Q_t {
    unsigned ref_count; /* ref count */
    hbool_t is_combined;
    union {
        struct H5Q_select select;
        struct H5Q_combine combine;
    } query;
};

/*****************************/
/* Library Private Variables */
/*****************************/

/******************************/
/* Library Private Prototypes */
/******************************/
H5_DLL herr_t H5Q_init(void);
H5_DLL H5Q_t *H5Q_create(H5Q_type_t query_type, H5Q_match_op_t match_op, ...);
H5_DLL herr_t H5Q_close(H5Q_t *query);
H5_DLL H5Q_t *H5Q_combine(H5Q_t *query1, H5Q_combine_op_t combine_op, H5Q_t *query2);
H5_DLL herr_t H5Q_apply(H5Q_t *query, hbool_t *result, H5T_t *type, const void *elem);

H5_DLL herr_t H5Q_encode(H5Q_t *query, unsigned char *buf, size_t *nalloc);
H5_DLL H5Q_t *H5Q_decode(const unsigned char **buf);

#endif /* _H5Qprivate_H */
