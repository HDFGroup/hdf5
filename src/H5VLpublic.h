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
 * Programmer:  Mohamad Chaarawi <chaarawi@hdfgroup.org>
 *              January 23, 2012
 */
#ifndef _H5VLpublic_H
#define _H5VLpublic_H

#include "H5public.h"
#include "H5Fpublic.h"
#include "H5Lpublic.h"

/* types for all file get API routines */
typedef enum H5VL_file_get_t {
    H5F_GET_FAPL	    = 0, 	/*file access property list		*/
    H5F_GET_FCPL	    = 1,	/*file creation property list		*/
    H5F_GET_SIZE	    = 2,	/*file size             		*/
    H5F_GET_FREE_SPACE	    = 3,	/*file freespace         		*/
    H5F_GET_INFO	    = 4,	/*file info             		*/
    H5F_GET_INTENT	    = 5,	/*file intent           		*/
    H5F_GET_NAME	    = 6,	/*file name             		*/
    H5F_GET_OBJ_COUNT	    = 7,	/*object count in file	        	*/
    H5F_GET_OBJ_IDS	    = 8,	/*object ids in file     		*/
    H5F_GET_VFD_HANDLE	    = 9,	/*file VFD handle       		*/
    H5F_GET_MDC_CONF	    = 10,	/*file metadata cache configuration	*/
    H5F_GET_MDC_HR	    = 11,	/*file metadata cache hit rate		*/
    H5F_GET_MDC_SIZE	    = 12,       /*file metadata cache size		*/
    H5F_GET_FREE_SECTIONS   = 13        /*file free selections                  */
} H5VL_file_get_t;

/* types for all group get API routines */
typedef enum H5VL_group_get_t {
    H5G_GET_GCPL	    = 0,	/*group creation property list		*/
    H5G_GET_INFO	    = 1 	/*group info             		*/
} H5VL_group_get_t;

/* types for all object get API routines */
typedef enum H5VL_object_get_t {
    H5O_GET_INFO	    = 0,	/*object info	                	*/
    H5O_GET_COMMENT	    = 1 	/*object comment            		*/
} H5VL_object_get_t;

/* types for all object lookup API routines */
typedef enum H5VL_object_lookup_t {
    H5O_LOOKUP_BY_NAME	    = 0,
    H5O_LOOKUP_BY_IDX	    = 1,
    H5O_LOOKUP_BY_ADDR	    = 2 
} H5VL_object_lookup_t;

#define H5VL_VOL_DEFAULT 0   /* Default VOL plugin value */

/* H5F routines */
typedef struct H5VL_file_class_t {
    hid_t  (*open)  (const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t dxpl_id);
    herr_t (*close) (hid_t file_id);
    hid_t  (*create)(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id);
    herr_t (*flush) (hid_t file_id, H5F_scope_t scope);
    herr_t (*get)   (hid_t file_id, H5VL_file_get_t get_type, int num_args, va_list arguments);
} H5VL_file_class_t;

/* H5D routines */
typedef struct H5VL_dataset_class_t {
    hid_t  (*open)  (hid_t loc_id, const char *name, hid_t dapl_id);
    herr_t (*close) (hid_t dataset_id);
    hid_t  (*create)(hid_t loc_id, const char *name, hid_t dtype_id, hid_t space_id, 
                     hid_t lcpl_id, hid_t dcpl_id, hid_t dapl_id);
    herr_t (*read)  (hid_t dataset_id, hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id,
                     hid_t xfer_plist_id, void * buf);
    herr_t (*write) (hid_t dataset_id, hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id,
                     hid_t xfer_plist_id, const void * buf );
    herr_t (*extend)(hid_t dset_id, const hsize_t size[] );
} H5VL_dataset_class_t;

/* H5A routines */
typedef struct H5VL_attribute_class_t {
    hid_t  (*open)  (hid_t obj_id, const char *attr_name, hid_t aapl_id);
    herr_t (*close) (hid_t attr_id);
    hid_t  (*create)( hid_t loc_id, const char *attr_name, hid_t type_id, hid_t space_id, 
                      hid_t acpl_id, hid_t aapl_id);
    herr_t (*delete)(hid_t loc_id, const char *attr_name );
    herr_t (*read)  (hid_t attr_id, hid_t mem_type_id, void *buf );
    herr_t (*write) (hid_t attr_id, hid_t mem_type_id, 
                     const void *buf );
} H5VL_attribute_class_t;

/* H5T routines*/
typedef struct H5VL_datatype_class_t {
    herr_t (*commit)(hid_t loc_id, const char *name, hid_t type_id, 
                     hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id);
    hid_t  (*open)  (hid_t loc_id, const char * name, hid_t tapl_id);
}H5VL_datatype_class_t;

/* H5L routines */
typedef struct H5VL_link_class_t {
    herr_t (*create) (hid_t obj_id, const char *name, hid_t loc_id, const char *link_name, 
                      hid_t lcpl_id, hid_t lapl_id);
    herr_t (*delete) (hid_t loc_id, const char *name, hid_t lapl_id);
    herr_t (*move)   (hid_t src_loc_id, const char *src_name, hid_t dst_loc_id, 
                      const char *dest_name, hid_t lcpl, hid_t lapl);
    herr_t (*copy)   (hid_t src_loc_id, const char *src_name, hid_t dest_loc_id, 
                      const char *dest_name, hid_t lcpl_id, hid_t lapl_id);
} H5VL_link_class_t;

/* H5G routines */
typedef struct H5VL_group_class_t {
    hid_t  (*create)(hid_t loc_id, const char *name, hid_t lcpl_id, hid_t gcpl_id, hid_t gapl_id);
    hid_t  (*open)  (hid_t loc_id, const char *name, hid_t gapl_id);
    herr_t (*close) (hid_t group_id);
    herr_t (*get)   (hid_t file_id, H5VL_group_get_t get_type, int num_args, va_list arguments);
} H5VL_group_class_t;

/* H5O routines */
typedef struct H5VL_object_class_t {
    hid_t  (*open)  (hid_t loc_id, void *obj_loc, hid_t lapl_id);
    herr_t (*close) (hid_t obj_id);
    herr_t (*move)  (hid_t src_loc_id, const char *src_name, hid_t dst_loc_id, 
                     const char *dest_name, hid_t lcpl, hid_t lapl);
    herr_t (*copy)  (hid_t src_loc_id, const char *src_name, hid_t dst_loc_id, const char *dst_name,
                     hid_t ocpypl_id, hid_t lcpl_id );
    herr_t (*lookup)(hid_t loc_id, H5VL_object_lookup_t lookup_type, int num_args, va_list arguments);
    herr_t (*get)   (hid_t loc_id, H5VL_object_get_t get_type, int num_args, va_list arguments);
} H5VL_object_class_t;

/* Class information for each VOL driver */
typedef struct H5VL_class_t {
    const char *name;
    herr_t  (*terminate)(void);
    size_t  fapl_size;
    void *  (*fapl_get)(hid_t fid);
    void *  (*fapl_copy)(const void *fapl);
    herr_t  (*fapl_free)(void *fapl);
    H5VL_attribute_class_t     attribute_cls;
    H5VL_datatype_class_t      datatype_cls;
    H5VL_dataset_class_t       dataset_cls;
    H5VL_group_class_t         group_cls;
    H5VL_file_class_t          file_cls;
    H5VL_link_class_t          link_cls;
    H5VL_object_class_t        object_cls;
} H5VL_class_t;

/* Function prototypes */
H5_DLL hid_t H5VLregister(const H5VL_class_t *cls);
H5_DLL herr_t H5VLunregister(hid_t driver_id);

#endif
