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
 * H5V module.
 */
#ifndef _H5Vpublic_H
#define _H5Vpublic_H

/* System headers needed by this file */

/* Public headers needed by this file */
#include "H5public.h"
#include "H5Ipublic.h"

/*****************/
/* Public Macros */
/*****************/

#define H5V_CRT_ELMT_SCOPE_NAME "element_scope"

/*******************/
/* Public Typedefs */
/*******************/

/********************/
/* Public Variables */
/********************/


#ifdef __cplusplus
extern "C" {
#endif

/*********************/
/* Public Prototypes */
/*********************/

H5_DLL herr_t H5Pset_view_elmt_scope(hid_t vcpl_id, hid_t space_id);
H5_DLL herr_t H5Pget_view_elmt_scope(hid_t vcpl_id, hid_t *space_id);

/* API wrappers */
H5_DLL hid_t H5Vcreate_ff(hid_t loc_id, hid_t query_id, hid_t vcpl_id, hid_t rcxt_id, hid_t estack_id);
H5_DLL herr_t H5Vget_query(hid_t view_id, hid_t *query_id);
H5_DLL herr_t H5Vget_counts(hid_t view_id, hsize_t *attr_count, hsize_t *obj_count, hsize_t *elem_region_count);
H5_DLL herr_t H5Vget_location_ff(hid_t view_id, hid_t *location_id, hid_t estack_id);
H5_DLL herr_t H5Vget_attrs_ff(hid_t view_id, hsize_t start, hsize_t count, hid_t attr_id[], hid_t estack_id);
H5_DLL herr_t H5Vget_objs_ff(hid_t view_id, hsize_t start, hsize_t count, hid_t obj_id[], hid_t estack_id);
H5_DLL herr_t H5Vget_elem_regions_ff(hid_t view_id, hsize_t start, hsize_t count, 
                                     hid_t dataset_id[], hid_t dataspace_id[], hid_t estack_id);
H5_DLL herr_t H5Vclose(hid_t view_id);

#ifdef __cplusplus
}
#endif
#endif /* _H5Vpublic_H */
