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
 * This file contains function prototypes for each exported function in the H5C module
 */

#ifndef H5CPROTO_H
#define H5CPROTO_H

#if defined c_plusplus || defined __cplusplus
extern      "C"
{
#endif                          /* c_plusplus || __cplusplus */

/* Functions in H5C.c */
hatom_t H5C_create(hatom_t owner_id, hobjtype_t type, const char *name);
hatom_t H5C_copy(hatom_t tid);
herr_t H5C_release(hatom_t oid);
hatom_t H5C_get_default_atom(hobjtype_t type);
herr_t H5C_init(hatom_t dst_atm, const file_create_temp_t *src);
herr_t H5Cgetparm(hatom_t tid, file_create_param_t parm, VOIDP buf);
herr_t H5Csetparm(hatom_t tid, file_create_param_t parm, const VOIDP buf);

#if defined c_plusplus || defined __cplusplus
}
#endif                          /* c_plusplus || __cplusplus */

#endif /* H5CPROTO_H */

