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

#ifndef _H5Mprivate_H
#define _H5Mprivate_H
#include <H5Mpublic.h>          /* Include Public Definitions */

/* Private headers needed by this file */
#include <H5private.h>

/*
 * A function table record for accessing interfaces which use the "meta"
 *  interface to create/open/close objects.
 */
typedef struct meta_func_t {
    group_t                 type;       /* Object type this interface is for */
    hid_t                   (*create) (hid_t, group_t, const char *);   /* Object creation function */
    hid_t                   (*access) (hid_t);  /* Object access function */
    hid_t                   (*copy) (hid_t);    /* Object copy function */
    hid_t                   (*find_name) (hid_t, group_t, const char *);        /* Find first object */
    uint32                  (*name_len) (hid_t);        /* Get length of object name */
    herr_t                  (*get_name) (hid_t, char *);        /* Get object name */
    herr_t                  (*set_name) (hid_t, const char *);  /* Set object name */
    hid_t                   (*search) (hid_t, group_t, const char *);   /* Search for list of objects */
    hid_t                   (*index) (hid_t, group_t, uint32);  /* Get the OID for the n'th object */
    herr_t                  (*flush) (hid_t);   /* Flush the object to disk */
    herr_t                  (*delete) (hid_t);  /* Delete an object from file */
    hid_t                   (*get_parent) (hid_t);      /* Get the parent object of an object */
    hid_t                   (*get_file) (hid_t);        /* Get the file ID of an object */
    herr_t                  (*close) (hid_t);   /* End access to an object */
} meta_func_t;

/* Private functions, not part of the publicly documented API */

#endif
