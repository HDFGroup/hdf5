/****************************************************************************
 * NCSA HDF                                                                 *
 * Software Development Group                                               *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING file.                                                        *
 *                                                                          *
 ****************************************************************************/

/*
 * This file contains private information about the H5P module
 */
#ifndef _H5Pprivate_H
#define _H5Pprivate_H

#include "H5Ppublic.h"

/* Private headers needed by this file */
#include "H5private.h"
#include "H5Fprivate.h"
#include "H5Dprivate.h"

/* Define enum for modifications to class */
typedef enum {
    H5P_MOD_ERR=(-1),   /* Indicate an error */
    H5P_MOD_INC_CLS,    /* Increment the dependant class count*/
    H5P_MOD_DEC_CLS,    /* Decrement the dependant class count*/
    H5P_MOD_INC_LST,    /* Increment the dependant list count*/
    H5P_MOD_DEC_LST,    /* Decrement the dependant list count*/
    H5P_MOD_INC_REF,    /* Increment the ID reference count*/
    H5P_MOD_DEC_REF,    /* Decrement the ID reference count*/
    H5P_MOD_CHECK,      /* Just check about deleting the class */
    H5P_MOD_MAX         /* Upper limit on class modifications */
} H5P_class_mod_t;

/* Define structure to hold property information */
typedef struct H5P_genprop_tag {
    /* Values for this property */
    unsigned xor_val;      /* XOR'ed version of the name, for faster comparisons */
    char *name;         /* Name of property */
    size_t size;        /* Size of property value */
    void *value;        /* Pointer to property value */

    /* Callback function pointers & info */
    H5P_prp_create_func_t create;   /* Function to call when a property is created */
    void *def_value;      /* Pointer to default value to pass along to create callback */
    H5P_prp_set_func_t set; /* Function to call when a property value is set */
    H5P_prp_get_func_t get; /* Function to call when a property value is retrieved */
    H5P_prp_delete_func_t del; /* Function to call when a property is deleted */
    H5P_prp_copy_func_t copy;  /* Function to call when a property is copied */
    H5P_prp_close_func_t close; /* Function to call when a property is closed */

    struct H5P_genprop_tag *next;  /* Pointer to the next property in this list */
} H5P_genprop_t;

/* Define structure to hold class information */
typedef struct H5P_genclass_tag {
    struct H5P_genclass_tag *parent;     /* Pointer to parent class */
    char *name;         /* Name of property list class */
    size_t  nprops;     /* Number of properties in class */
    unsigned   hashsize;   /* Hash table size */
    unsigned   plists;     /* Number of property lists that have been created since the last modification to the class */
    unsigned   classes;    /* Number of classes that have been derived since the last modification to the class */
    unsigned   ref_count;  /* Number of oustanding ID's open on this class object */
    unsigned   internal;   /* Whether this class is internal to the library or not */
    unsigned   deleted;    /* Whether this class has been deleted and is waiting for dependent classes & proplists to close */

    /* Callback function pointers & info */
    H5P_cls_create_func_t create_func;  /* Function to call when a property list is created */
    void *create_data;  /* Pointer to user data to pass along to create callback */
    H5P_cls_copy_func_t copy_func;   /* Function to call when a property list is copied */
    void *copy_data;    /* Pointer to user data to pass along to copy callback */
    H5P_cls_close_func_t close_func;   /* Function to call when a property list is closed */
    void *close_data;   /* Pointer to user data to pass along to close callback */

    H5P_genprop_t *props[1];  /* Hash table of pointers to properties in the class */
} H5P_genclass_t;

/* Define structure to hold property list information */
typedef struct H5P_genplist_tag {
    H5P_genclass_t *pclass; /* Pointer to class info */
    size_t  nprops;         /* Number of properties in class */
    unsigned   class_init:1;   /* Whether the class initialization callback finished successfully */

    /* Hash size for a property list is same as class */
    H5P_genprop_t *props[1];  /* Hash table of pointers to properties in the list */
} H5P_genplist_t;

/* Master property list structure */
typedef struct {
    /* Union of all the different kinds of property lists */
    union {
        H5F_create_t fcreate;   /* File creation properties */
        H5F_access_t faccess;   /* File access properties */
        H5D_create_t dcreate;   /* Dataset creation properties */
        H5F_mprop_t mount;      /* Mounting properties */
    } u;
    H5P_class_t_old cls;        /* Property list class */
} H5P_t;

/* Private functions, not part of the publicly documented API */
__DLL__ herr_t H5P_init(void);
__DLL__ hid_t H5P_create(H5P_class_t_old type, H5P_t *plist);
__DLL__ void *H5P_copy(H5P_class_t_old type, const void *src);
__DLL__ herr_t H5P_close(void *plist);
__DLL__ herr_t H5P_register(H5P_genclass_t *pclass, const char *name, size_t size,
            void *def_value, H5P_prp_create_func_t prp_create, H5P_prp_set_func_t prp_set,
            H5P_prp_get_func_t prp_get, H5P_prp_delete_func_t prp_delete,
            H5P_prp_copy_func_t prp_copy, H5P_prp_close_func_t prp_close);
__DLL__ herr_t H5P_get(hid_t plist_id, const char *name, void *value);
__DLL__ herr_t H5P_set(hid_t plist_id, const char *name, void *value);
__DLL__ H5P_class_t_old H5P_get_class(hid_t tid);
__DLL__ hid_t H5P_get_driver(hid_t plist_id);

/* Private functions to "peek" at properties of a certain type */
__DLL__ unsigned H5P_peek_unsigned(hid_t plist_id, const char *name);
__DLL__ hid_t H5P_peek_hid_t(hid_t plist_id, const char *name);
__DLL__ void *H5P_peek_voidp(hid_t plist_id, const char *name);
__DLL__ hsize_t H5P_peek_hsize_t(hid_t plist_id, const char *name);
__DLL__ size_t H5P_peek_size_t(hid_t plist_id, const char *name);

#endif
