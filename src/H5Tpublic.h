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

/* Define atomic datatype bases */
#define H5T_CHAR    MAKE_ATOM(H5_DATATYPE,0)
#define H5T_INT     MAKE_ATOM(H5_DATATYPE,1)
#define H5T_FLOAT   MAKE_ATOM(H5_DATATYPE,2)
#define H5T_DATE    MAKE_ATOM(H5_DATATYPE,3)
#define H5T_TIME    MAKE_ATOM(H5_DATATYPE,4)
#define H5T_SPTR    MAKE_ATOM(H5_DATATYPE,5)
#define H5T_PPTR    MAKE_ATOM(H5_DATATYPE,6)
#define H5T_COMPOUND MAKE_ATOM(H5_DATATYPE,7)

/* Define atomic datatype architectures */
#define H5T_BIGENDIAN       0
#define H5T_LITTLEENDIAN    1

/* Define the machine's architecture */
/*
WARNING!
    This is _extremly_ crude is is only valid for very generic architectures,
    anything with a wierd size of integer or wacky floating-point format will
    _not_ work with this hack.  It needs to be replaced with Robb's much more
    comprehensive code from H5detect.c. -QAK
WARNING!
*/
#define H5T_ARCH_BIGENDIAN  0
#define H5T_ARCH_LITTLEENDIAN  1
#ifdef WORDS_BIGENDIAN
#define H5T_ARCH_TYPE  H5T_ARCH_BIGENDIAN
#else /* WORDS_BIGENDIAN */
#define H5T_ARCH_TYPE  H5T_ARCH_LITTLEENDIAN
#endif /* WORDS_BIGENDIAN */

typedef struct {
    hid_t base;           /* Basic datatype */
    uint8 len;              /* Length of base-type, in bytes */
    uint8 arch;             /* Architecture of the base-type */
 } h5_atomic_type_t;

#ifdef __cplusplus
extern "C" {
#endif

/* Functions in H5T.c */
uint32 H5Tget_num_fields(hid_t tid);
hbool_t H5Tis_field_atomic(hid_t tid,uintn fidx);
hbool_t H5Tis_atomic(hid_t tid);
herr_t H5Tset_type(hid_t tid,hid_t base,uint8 len,uint8 arch);
herr_t H5Tget_type(hid_t tid,hid_t *base,uint8 *len,uint8 *arch);
uintn H5Tsize(hid_t tid, uint8 len, uint8 arch, hbool_t mem_flag);
herr_t H5Tadd_field (hid_t tid, const char *name, hid_t base, uint8 len,
		     uint8 arch, hid_t space);
herr_t H5Tget_fields(hid_t tid, hid_t *field_list);
void H5T_term_interface (void);

#ifdef __cplusplus
}
#endif

#endif
