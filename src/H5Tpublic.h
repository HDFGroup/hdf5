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
 * This file contains public declarations for the H5T module.
 */

#ifndef _H5Tpublic_H
#define _H5Tpublic_H

/* Public headers needed by this file */
#include <H5public.h>
#include <H5Apublic.h>

/* Define atomic datatypes */
#define H5T_CHAR    MAKE_ATOM(H5_DATATYPE,0)
#define H5T_INT     MAKE_ATOM(H5_DATATYPE,1)
#define H5T_FLOAT   MAKE_ATOM(H5_DATATYPE,2)
#define H5T_DATE    MAKE_ATOM(H5_DATATYPE,3)
#define H5T_TIME    MAKE_ATOM(H5_DATATYPE,4)
#define H5T_SPTR    MAKE_ATOM(H5_DATATYPE,5)
#define H5T_PPTR    MAKE_ATOM(H5_DATATYPE,6)
#define H5T_COMPOUND MAKE_ATOM(H5_DATATYPE,7)

typedef struct {
    hatom_t base;           /* Basic datatype */
    uint8 len;              /* Length of base-type, in bytes */
    uint8 arch;             /* Architecture of the base-type */
 } h5_atomic_type_t;

#ifdef __cplusplus
extern "C" {
#endif

/* Functions in H5T.c */
uint32 H5Tget_num_fields(hatom_t tid);
hbool_t H5Tis_field_atomic(hatom_t tid,uintn fidx);
hbool_t H5Tis_atomic(hatom_t tid);
herr_t H5Tset_type(hatom_t tid,hatom_t base,uint8 len,uint8 arch);
uintn H5Tsize(hatom_t tid, uint8 len, uint8 arch, hbool_t mem_flag);
herr_t H5Tadd_field (hatom_t tid, const char *name, hatom_t base, uint8 len,
		     uint8 arch, hatom_t space);
herr_t H5Tget_fields(hatom_t tid, hatom_t *field_list);

#ifdef __cplusplus
}
#endif

#endif
