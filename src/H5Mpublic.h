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
 * This file contains public declarations for the H5M module.
 */
#ifndef _H5Mpublic_H
#define _H5Mpublic_H

/* Public headers needed by this file */
#include <H5public.h>
#include <H5Ppublic.h>          /*for hobjtype_t defn                        */

#ifdef __cplusplus
extern "C" {
#endif

/* Functions in H5M.c */
hid_t H5Maccess (hid_t oid);
hid_t H5Mcopy (hid_t oid);
hid_t H5Mfind_name (hid_t oid, group_t type, const char *name);
unsigned H5Mname_len (hid_t oid);
herr_t H5Mget_name (hid_t oid, char *name);
herr_t H5Mset_name (hid_t oid, const char *name);
hid_t H5Msearch (hid_t oid, group_t type, const char *name);
hid_t H5Mindex (hid_t oid, group_t type, unsigned idx);
hid_t H5Mflush (hid_t oid);
herr_t H5Mdelete (hid_t oid);
hid_t H5Mget_file (hid_t oid);
hid_t H5Mget_parent (hid_t oid);
herr_t H5Mclose (hid_t oid);

#ifdef __cplusplus
}
#endif
#endif
