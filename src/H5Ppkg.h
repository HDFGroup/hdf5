/*
 * Copyright (C) 1997-2001 NCSA
 *		           All rights reserved.
 *
 * Programmer:	Quincey Koziol <koziol@ncsa.uiuc.edu>
 *		Friday, November 16, 2001
 *
 * Purpose:	This file contains declarations which are visible only within
 *		the H5P package.  Source files outside the H5P package should
 *		include H5Pprivate.h instead.
 */
#ifndef H5P_PACKAGE
#error "Do not include this file outside the H5P package!"
#endif

#ifndef _H5Ppkg_H
#define _H5Ppkg_H

/*
 * Define this to enable debugging.
 */
#ifdef NDEBUG
#  undef H5P_DEBUG
#endif

#include "H5Pprivate.h"

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
struct H5P_genclass_tag {
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
};

/* Define structure to hold property list information */
struct H5P_genplist_tag {
    H5P_genclass_t *pclass; /* Pointer to class info */
    hid_t   plist_id;       /* Copy of the property list ID (for use in close callback) */
    size_t  nprops;         /* Number of properties in class */
    unsigned   class_init:1;   /* Whether the class initialization callback finished successfully */

    /* Hash size for a property list is same as class */
    H5P_genprop_t *props[1];  /* Hash table of pointers to properties in the list */
};

/* Private functions, not part of the publicly documented API */

#endif /* _H5Ppkg_H */

