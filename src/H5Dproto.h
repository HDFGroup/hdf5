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
 * This file contains function prototypes for each exported function in the H5D module
 */

#ifndef H5DPROTO_H
#define H5DPROTO_H

#if defined c_plusplus || defined __cplusplus
extern      "C"
{
#endif                          /* c_plusplus || __cplusplus */

/* Functions in H5D.c */
hatom_t H5D_create(hatom_t owner_id, hobjtype_t type, const char *name);
herr_t H5Dset_info(hatom_t oid, hatom_t tid, hatom_t did);
herr_t H5D_release(hatom_t oid);
herr_t H5Dwrite(hatom_t oid, hatom_t did, VOIDP buf);

#if defined c_plusplus || defined __cplusplus
}
#endif                          /* c_plusplus || __cplusplus */

#endif /* H5DPROTO_H */

