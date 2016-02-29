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
#include "H5Gprivate.h"
#include "H5Oprivate.h"

/**************************/
/* Library Private Macros */
/**************************/

/****************************/
/* Library Private Typedefs */
/****************************/

/* Data element */
typedef struct H5Q_data_elem_t {
    H5T_t *type; /* type */
    size_t type_size; /* type size */
    void  *value; /* value */
} H5Q_data_elem_t;

/* Attribute name */
typedef struct H5Q_attr_name_t {
    char *name; /* name */
} H5Q_attr_name_t;

/* Link name */
typedef struct H5Q_link_name_t {
    char *name; /* name */
} H5Q_link_name_t;

/* Query */
typedef struct H5Q_t H5Q_t;

/* Combine query */
typedef struct H5Q_combine_t {
    H5Q_type_t type; /* type */
    H5Q_combine_op_t op; /* op */
    H5Q_t *l_query; /* left op query */
    H5Q_t *r_query; /* right op query */
} H5Q_combine_t;

/* Select query */
typedef struct H5Q_select_t {
    H5Q_type_t type; /* type */
    H5Q_match_op_t match_op; /* match op */
    union {
        H5Q_data_elem_t data_elem;
        H5Q_attr_name_t attr_name;
        H5Q_link_name_t link_name;
    } elem;
} H5Q_select_t;

/* Query */
struct H5Q_t {
    hid_t query_id; /* ID of this query, allows to conveniently pass the ID to
                     * the indexing interface */
    unsigned ref_count; /* ref count */
    hbool_t is_combined;
    union {
        H5Q_select_t select;
        H5Q_combine_t combine;
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
H5_DLL herr_t H5Q_get_type(const H5Q_t *query, H5Q_type_t *query_type);
H5_DLL herr_t H5Q_get_match_op(const H5Q_t *query, H5Q_match_op_t *match_op);
H5_DLL herr_t H5Q_get_components(const H5Q_t *query, H5Q_t **sub_query1, H5Q_t **sub_query2);
H5_DLL herr_t H5Q_get_combine_op(const H5Q_t *query, H5Q_combine_op_t *op_type);

H5_DLL herr_t H5Q_encode(const H5Q_t *query, unsigned char *buf, size_t *nalloc);
H5_DLL H5Q_t *H5Q_decode(const unsigned char **buf);

/* Apply query */
H5_DLL herr_t H5Q_apply_atom(const H5Q_t *query, hbool_t *result, ...);
H5_DLL H5G_t *H5Q_apply(hid_t loc_id, const H5Q_t *query, unsigned *result, hid_t vcpl_id);
H5_DLL H5G_t *H5Q_apply_multi(size_t loc_count, hid_t *loc_ids, const H5Q_t *query, unsigned *result, hid_t vcpl_id);

#endif /* _H5Qprivate_H */
