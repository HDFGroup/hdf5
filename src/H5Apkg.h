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
 * Programmer:  Quincey Koziol
 *              Monday, Apr 20
 *
 * Purpose:     This file contains declarations which are visible only within
 *              the H5A package.  Source files outside the H5A package should
 *              include H5Aprivate.h instead.
 */
#ifndef H5A_PACKAGE
#error "Do not include this file outside the H5A package!"
#endif

#ifndef _H5Apkg_H
#define _H5Apkg_H

/*
 * Define this to enable debugging.
 */
#ifdef NDEBUG
#  undef H5A_DEBUG
#endif

/* Get package's private header */
#include "H5Aprivate.h"

/* Other private headers needed by this file */
#include "H5B2private.h"	/* v2 B-trees				*/
#include "H5FLprivate.h"	/* Free Lists				*/
#include "H5HFprivate.h"	/* Fractal heaps			*/
#include "H5Oprivate.h"		/* Object headers		  	*/
#include "H5Sprivate.h"		/* Dataspace				*/
#include "H5Tprivate.h"		/* Datatype functions			*/


/**************************/
/* Package Private Macros */
/**************************/

/* Standard length of fractal heap ID for attribute */
#define H5A_DENSE_FHEAP_ID_LEN  7


/****************************/
/* Package Private Typedefs */
/****************************/

/* Define the main attribute structure */
struct H5A_t {
    hbool_t     initialized;/* Indicate whether the attribute has been modified */
    hbool_t     obj_opened; /* Object header entry opened? */
    H5O_loc_t   oloc;       /* Object location for object attribute is on */
    H5G_name_t  path;       /* Group hierarchy path */
    char        *name;      /* Attribute's name */
    H5T_t       *dt;        /* Attribute's datatype */
    size_t      dt_size;    /* Size of datatype on disk */
    H5S_t       *ds;        /* Attribute's dataspace */
    size_t      ds_size;    /* Size of dataspace on disk */
    void        *data;      /* Attribute data (on a temporary basis) */
    size_t      data_size;  /* Size of data on disk */
    H5T_cset_t  encoding;   /* Character encoding of attribute */
    H5O_shared_t sh_loc;    /* Location of shared message */
};

/* Typedefs for "dense" attribute storage */
/* (fractal heap & v2 B-tree info) */

/* Typedef for native 'name' field index records in the v2 B-tree */
/* (Keep 'id' field first so generic record handling in callbacks works) */
typedef struct H5A_dense_bt2_name_rec_t {
    uint8_t id[H5A_DENSE_FHEAP_ID_LEN]; /* Heap ID for link */
    uint8_t flags;                      /* Message flags for attribute */
    uint32_t hash;                      /* Hash of 'name' field value */
} H5A_dense_bt2_name_rec_t;

/*
 * Common data exchange structure for dense attribute storage.  This structure
 * is passed through the v2 B-tree layer to the methods for the objects
 * to which the v2 B-tree points.
 */
typedef struct H5A_bt2_ud_common_t {
    /* downward */
    H5F_t       *f;                     /* Pointer to file that fractal heap is in */
    hid_t       dxpl_id;                /* DXPL for operation                */
    H5HF_t      *fheap;                 /* Fractal heap handle               */
    const char  *name;                  /* Name of attribute to compare      */
    uint32_t    name_hash;              /* Hash of name of attribute to compare */
    uint8_t     flags;                  /* Flags for attribute storage location */
    int64_t     corder;                 /* Creation order value of attribute to compare */
    H5B2_found_t found_op;              /* Callback when correct attribute is found */
    void        *found_op_data;         /* Callback data when correct attribute is found */
} H5A_bt2_ud_common_t;

/*
 * Data exchange structure for dense attribute storage.  This structure is
 * passed through the v2 B-tree layer when inserting attributes.
 */
typedef struct H5A_bt2_ud_ins_t {
    /* downward */
    H5A_bt2_ud_common_t common;         /* Common info for B-tree user data (must be first) */
    uint8_t id[H5A_DENSE_FHEAP_ID_LEN]; /* Heap ID of attribute to insert    */
} H5A_bt2_ud_ins_t;

/* Data structure to hold table of attributes for an object */
typedef struct {
    size_t      nattrs;         /* # of attributes in table */
    H5A_t       *attrs;         /* Pointer to array of attributes */
    uint8_t     *flags;         /* Pointer to array of message flags for attributes */
} H5A_attr_table_t;

/* Attribute iteration operator for internal library callbacks */
typedef herr_t (*H5A_lib_iterate_t)(const H5A_t *attr, uint8_t mesg_flags,
    void *op_data);

/* Describe kind of callback to make for each attribute */
typedef struct {
    enum {
        H5A_ATTR_OP_APP,                /* Application callback */
        H5A_ATTR_OP_LIB                 /* Library internal callback */
    } op_type;
    union {
        H5A_operator_t app_op;           /* Application callback for each attribute */
        H5A_lib_iterate_t lib_op;       /* Library internal callback for each attribute */
    } u;
} H5A_attr_iterate_t;


/*****************************/
/* Package Private Variables */
/*****************************/

/* Declare extern the free list for H5A_t's */
H5FL_EXTERN(H5A_t);

/* Declare extern a free list to manage blocks of type conversion data */
H5FL_BLK_EXTERN(attr_buf);

/* The v2 B-tree class for indexing 'name' field on attributes */
H5_DLLVAR const H5B2_class_t H5A_BT2_NAME[1];


/******************************/
/* Package Private Prototypes */
/******************************/

/* Function prototypes for H5A package scope */
H5_DLL herr_t H5A_init(void);
H5_DLL H5A_t *H5A_copy(H5A_t *new_attr, const H5A_t *old_attr);
H5_DLL herr_t H5A_free(H5A_t *attr);
H5_DLL herr_t H5A_close(H5A_t *attr);

/* Attribute "dense" storage routines */
H5_DLL H5A_t *H5A_dense_open(H5F_t *f, hid_t dxpl_id, const H5O_t *oh,
    const char *name);
H5_DLL herr_t H5A_dense_write(H5F_t *f, hid_t dxpl_id, const H5O_t *oh,
    const H5A_t *attr);
H5_DLL herr_t H5A_dense_iterate(H5F_t *f, hid_t dxpl_id, hid_t loc_id,
    haddr_t attr_fheap_addr, haddr_t name_bt2_addr, unsigned skip,
    unsigned *last_attr, const H5A_attr_iterate_t *attr_op, void *op_data);
H5_DLL herr_t H5A_dense_remove(H5F_t *f, hid_t dxpl_id, const H5O_t *oh,
    const char *name);

/* Attribute table operations */
H5_DLL herr_t H5A_dense_build_table(H5F_t *f, hid_t dxpl_id, const H5O_t *oh,
    H5_index_t idx_type, H5_iter_order_t order, H5A_attr_table_t *atable);
H5_DLL herr_t H5A_attr_release_table(H5A_attr_table_t *atable);

/* Attribute object header routines */
H5_DLL herr_t H5O_attr_reset(void *_mesg);
H5_DLL herr_t H5O_attr_delete(H5F_t *f, hid_t dxpl_id, const void *_mesg, hbool_t adj_link);
H5_DLL herr_t H5O_attr_get_share(const void *_mesg, H5O_shared_t *sh);

#endif /* _H5Apkg_H */

