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

/* $Id$ */

/*
 * This file contains macros & information for meta-objects
 */

#ifndef H5MPRIVATE_H
#define H5MPRIVATE_H

#include "H5Mproto.h"   /* Include Public Definitions */

/*
 * A function table record for accessing interfaces which use the "meta"
 *  interface to create/access/release objects.
 */

typedef struct meta_func_t
  {
    hobjtype_t type;            /* Object type this interface is for */
    hatom_t (*create) (hatom_t , hobjtype_t, const char *);   /* Object creation function */
    hatom_t (*access) (hatom_t );   /* Object access function */
    hatom_t (*copy) (hatom_t );     /* Object copy function */
    hatom_t (*find_name) (hatom_t , hobjtype_t, const char *); /* Find first object */
    uint32  (*name_len) (hatom_t );     /* Get length of object name */
    herr_t  (*get_name) (hatom_t , char *);     /* Get object name */
    herr_t  (*set_name) (hatom_t , const char *);   /* Set object name */
    hatom_t (*search) (hatom_t , hobjtype_t, const char *); /* Search for list of objects */
    hatom_t (*index) (hatom_t , hobjtype_t, uint32);    /* Get the OID for the n'th object */
    herr_t  (*flush) (hatom_t );    /* Flush the object to disk */
    herr_t  (*delete) (hatom_t );   /* Delete an object from file */
    hatom_t (*get_parent) (hatom_t );   /* Get the parent object of an object */
    herr_t  (*release) (hatom_t );  /* End access to an object */
  }
meta_func_t;

meta_func_t meta_func_arr[]={
    {   /* Template object meta-functions (defined in H5C.c) */
        H5_TEMPLATE,            /* File-Creation Template Type ID */
        H5C_create,             /* File-Creation Template Create */
        NULL,                   /* File-Creation Template Access */
        H5C_copy,               /* File-Creation Template Copy */
        NULL,                   /* File-Creation Template FindName */
        NULL,                   /* File-Creation Template NameLen */
        NULL,                   /* File-Creation Template GetName */
        NULL,                   /* File-Creation Template SetName */
        NULL,                   /* File-Creation Template Search */
        NULL,                   /* File-Creation Template Index */
        NULL,                   /* File-Creation Template Flush */
        NULL,                   /* File-Creation Template Delete */
        NULL,                   /* File-Creation Template GetParent */
        H5C_release             /* File-Creation Template Release */
    },
    {   /* Datatype object meta-functions (defined in H5T.c) */
        H5_DATATYPE,            /* Datatype Type ID */
        H5T_create,             /* Datatype Create */
        NULL,                   /* Datatype Access */
        NULL,                   /* Dataspace Copy */
        NULL,                   /* Datatype FindName */
        NULL,                   /* Datatype NameLen */
        NULL,                   /* Datatype GetName */
        NULL,                   /* Datatype SetName */
        NULL,                   /* Datatype Search */
        NULL,                   /* Datatype Index */
        NULL,                   /* Datatype Flush */
        NULL,                   /* Datatype Delete */
        NULL,                   /* Datatype GetParent */
        H5T_release             /* Datatype Release */
    },
    {   /* Dimensionality object meta-functions (defined in H5P.c) */
        H5_DATASPACE,           /* Dimensionality Type ID */
        H5P_create,             /* Dimensionality Create */
        NULL,                   /* Dimensionality Access */
        NULL,                   /* Dimensionality Copy */
        NULL,                   /* Dimensionality FindName */
        NULL,                   /* Dimensionality NameLen */
        NULL,                   /* Dimensionality GetName */
        NULL,                   /* Dimensionality SetName */
        NULL,                   /* Dimensionality Search */
        NULL,                   /* Dimensionality Index */
        NULL,                   /* Dimensionality Flush */
        NULL,                   /* Dimensionality Delete */
        NULL,                   /* Dimensionality GetParent */
        H5P_release             /* Dimensionality Release */
    },
    {   /* Dataset object meta-functions (defined in H5D.c) */
        H5_DATASPACE,           /* Dataset Type ID */
        H5D_create,             /* Dataset Create */
        NULL,                   /* Dataset Access */
        NULL,                   /* Dataset Copy */
        NULL,                   /* Dataset FindName */
        NULL,                   /* Dataset NameLen */
        NULL,                   /* Dataset GetName */
        NULL,                   /* Dataset SetName */
        NULL,                   /* Dataset Search */
        NULL,                   /* Dataset Index */
        H5D_flush,              /* Dataset Flush */
        NULL,                   /* Dataset Delete */
        NULL,                   /* Dataset GetParent */
        H5D_release             /* Dataset Release */
    }
  };

/* Private functions, not part of the publicly documented API */

#endif /* H5MPRIVATE_H */

