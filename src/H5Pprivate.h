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

#include <H5Ppublic.h>

/* Private headers needed by this file */
#include <H5private.h>
#include <H5Fprivate.h>
#include <H5Dprivate.h>

/* Master property list structure */
typedef struct {
    /* Union of all the different kinds of property lists */
    union {
        H5F_create_t fcreate;   /* File creation properties */
        H5F_access_t faccess;   /* File access properties */
        H5D_create_t dcreate;   /* Dataset creation properties */
        H5F_xfer_t dxfer;       /* Data transfer properties */
        H5F_mprop_t mount;      /* Mounting properties */
    } u;
    H5P_class_t class;          /* Property list class */
} H5P_t;

__DLL__ hid_t H5P_create(H5P_class_t type, H5P_t *plist);
__DLL__ void *H5P_copy(H5P_class_t type, const void *src);
__DLL__ herr_t H5P_close(void *plist);
__DLL__ H5P_class_t H5P_get_class(hid_t tid);

#endif
