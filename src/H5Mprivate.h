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
        NULL,
        H5C_copy,               /* File-Creation Template Copy */
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        H5C_release             /* File-Creation Template Release */
    },
    {   /* Datatype object meta-functions (defined in H5T.c) */
        H5_DATATYPE,            /* Datatype Type ID */
        H5T_create,             /* Datatype object functions */
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        H5T_release
    }
  };

/* Private functions, not part of the publicly documented API */

#endif /* H5MPRIVATE_H */

