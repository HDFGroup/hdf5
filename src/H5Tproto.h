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
 * This file contains function prototypes for each exported function in the H5T module
 */

#ifndef H5TPROTO_H
#define H5TPROTO_H

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

#if defined c_plusplus || defined __cplusplus
extern      "C"
{
#endif                          /* c_plusplus || __cplusplus */

/* Functions in H5T.c */
hatom_t H5T_create(hatom_t owner_id, hobjtype_t type, const char *name);
uint32 H5Tget_num_fields(hatom_t tid);
hbool_t H5Tis_field_atomic(hatom_t tid,uintn fidx);
hbool_t H5Tis_atomic(hatom_t tid);
herr_t H5Tset_type(hatom_t tid,hatom_t base,uint8 len,uint8 arch);
uintn H5Tsize(hatom_t tid, uint8 len, uint8 arch, hbool_t mem_flag);
herr_t H5T_release(hatom_t oid);

#if defined c_plusplus || defined __cplusplus
}
#endif                          /* c_plusplus || __cplusplus */

#endif /* H5TPROTO_H */

