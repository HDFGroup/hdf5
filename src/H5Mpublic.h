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
 * This file contains public declarations for the H5M module.
 */

#ifndef _H5Mpublic_H
#define _H5Mpublic_H

/* Public headers needed by this file */
#include <H5public.h>
#include <H5Cpublic.h>		/*for hobjtype_t defn*/


#ifdef __cplusplus
extern "C" {
#endif

/* Functions in H5M.c */
hatom_t H5Mcreate(hatom_t owner_id, hobjtype_t type, const char *name);
hatom_t H5Maccess(hatom_t oid);
hatom_t H5Mcopy(hatom_t oid);
hatom_t H5Mfind_name(hatom_t oid, hobjtype_t type, const char *name);
uint32 H5Mname_len(hatom_t oid);
herr_t H5Mget_name(hatom_t oid, char *name);
herr_t H5Mset_name(hatom_t oid, const char *name);
hatom_t H5Msearch(hatom_t oid, hobjtype_t type, const char *name);
hatom_t H5Mindex(hatom_t oid, hobjtype_t type, uint32 idx);
hatom_t H5Mflush(hatom_t oid);
herr_t H5Mdelete(hatom_t oid);
hatom_t H5Mget_file(hatom_t oid);
hatom_t H5Mget_parent(hatom_t oid);
herr_t H5Mrelease(hatom_t oid);

#ifdef __cplusplus
}
#endif

#endif
