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

/* These are the various classes of data types */
typedef enum H5T_class_t {
   H5T_NO_CLASS		=-1,	/*error					*/
   H5T_INTEGER		=0,	/*integer types				*/
   H5T_FLOAT		=1,	/*floating-point types			*/
   H5T_TIME		=2,	/*date and time	types			*/
   H5T_STRING		=3,	/*character string types		*/
   H5T_BITFIELD		=4,	/*bit field types			*/
   H5T_OPAQUE		=5, 	/*opaque types				*/
   H5T_COMPOUND		=6	/*compound types			*/
} H5T_class_t;

/* Byte orders */
typedef enum H5T_order_t {
   H5T_ORDER_ERROR	=-1, 	/*error					*/
   H5T_ORDER_LE		=0, 	/*little endian				*/
   H5T_ORDER_BE		=1, 	/*bit endian				*/
   H5T_ORDER_VAX	=2, 	/*VAX mixed endian			*/
   H5T_ORDER_NONE	=3	/*no particular order (strings, bits,..)*/
   /*H5T_ORDER_NONE must be last*/
} H5T_order_t;

/* Types of integer sign schemes */
typedef enum H5T_sign_t {
   H5T_SGN_ERROR	=-1, 	/*error					*/
   H5T_SGN_NONE		=0, 	/*this is an unsigned type		*/
   H5T_SGN_2		=1,  	/*two's complement			*/
   
   H5T_SGN_N		=2	/*this must be last			*/
} H5T_sign_t;

/* Floating-point normalization schemes */
typedef enum H5T_norm_t {
   H5T_NORM_ERROR	=-1, 	/*error					*/
   H5T_NORM_IMPLIED	=0,	/*msb of mantissa isn't stored, always 1*/
   H5T_NORM_MSBSET	=1, 	/*msb of mantissa is always 1		*/
   H5T_NORM_NONE	=2	/*not normalized			*/
   /*H5T_NORM_NONE must be last*/
} H5T_norm_t;

/* Character set to use for text strings */
typedef enum H5T_cset_t {
   H5T_CSET_ERROR	=-1,	/*error					*/
   H5T_CSET_ASCII	=0	/*US ASCII				*/
} H5T_cset_t;

/* Type of padding to use in character strings */
typedef enum H5T_str_t {
   H5T_STR_ERROR	=-1, 	/*error					*/
   H5T_STR_NULL		=0, 	/*pad with null term like in C		*/
   H5T_STR_SPACE	=1 	/*pad with spaces like in Fortran	*/
} H5T_str_t;

/* Type of padding to use in other atomic types */
typedef enum H5T_pad_t {
   H5T_PAD_ERROR	=-1, 	/*error					*/
   H5T_PAD_ZERO		=0, 	/*always set to zero			*/
   H5T_PAD_ONE		=1, 	/*always set to one			*/
   H5T_PAD_BACKGROUND	=2, 	/*set to background value		*/
   
   H5T_NPAD		=3	/*THIS MUST BE LAST			*/
} H5T_pad_t;

#define HOFFSET(S,M)	((const char*)&S.M-(const char*)&S)
#define HPOFFSET(P,M)	((const char*)&(P->M)-(const char*)P)

/* The predefined types */
#define H5T_NATIVE_CHAR		(H5init(), H5T_NATIVE_CHAR_g)
#define H5T_NATIVE_UCHAR	(H5init(), H5T_NATIVE_UCHAR_g)
#define H5T_NATIVE_SHORT	(H5init(), H5T_NATIVE_SHORT_g)
#define H5T_NATIVE_USHORT	(H5init(), H5T_NATIVE_USHORT_g)
#define H5T_NATIVE_INT		(H5init(), H5T_NATIVE_INT_g)
#define H5T_NATIVE_UINT		(H5init(), H5T_NATIVE_UINT_g)
#define H5T_NATIVE_LONG		(H5init(), H5T_NATIVE_LONG_g)
#define H5T_NATIVE_ULONG	(H5init(), H5T_NATIVE_ULONG_g)
#define H5T_NATIVE_FLOAT	(H5init(), H5T_NATIVE_FLOAT_g)
#define H5T_NATIVE_DOUBLE	(H5init(), H5T_NATIVE_DOUBLE_g)

extern hid_t H5T_NATIVE_CHAR_g;
extern hid_t H5T_NATIVE_UCHAR_g;
extern hid_t H5T_NATIVE_SHORT_g;
extern hid_t H5T_NATIVE_USHORT_g;
extern hid_t H5T_NATIVE_INT_g;
extern hid_t H5T_NATIVE_UINT_g;
extern hid_t H5T_NATIVE_LONG_g;
extern hid_t H5T_NATIVE_ULONG_g;
extern hid_t H5T_NATIVE_FLOAT_g;
extern hid_t H5T_NATIVE_DOUBLE_g;

#ifdef __cplusplus
extern "C" {
#endif

/* Operations defined on all data types */
hid_t H5Tcreate (H5T_class_t type, size_t size);
hid_t H5Tcopy (hid_t type_id);
herr_t H5Tclose (hid_t type_id);
hbool_t H5Tequal (hid_t type1_id, hid_t type2_id);

/* Operations defined on compound data types */
herr_t H5Tinsert (hid_t parent_id, const char *name, off_t offset,
		  hid_t member_id);
herr_t H5Tpack (hid_t type_id);

/* Querying property values */
H5T_class_t H5Tget_class (hid_t type_id);
size_t H5Tget_size (hid_t type_id);
H5T_order_t H5Tget_order (hid_t type_id);
size_t H5Tget_precision (hid_t type_id);
size_t H5Tget_offset (hid_t type_id);
H5T_sign_t H5Tget_sign (hid_t type_id);
herr_t H5Tget_fields (hid_t type_id, size_t *spos/*out*/,
		      size_t *epos/*out*/, size_t *esize/*out*/,
		      size_t *mpos/*out*/, size_t *msize/*out*/);
size_t H5Tget_ebias (hid_t type_id);
H5T_norm_t H5Tget_norm (hid_t type_id);
intn H5Tget_nmembers (hid_t type_id);
char *H5Tget_member_name (hid_t type_id, int membno);
size_t H5Tget_member_offset (hid_t type_id, int membno);
int H5Tget_member_dims (hid_t type_id, int membno,
			int dims[]/*out*/, int perm[]/*out*/);
hid_t H5Tget_member_type (hid_t type_id, int membno);

/* Setting property values */
herr_t H5Tset_size (hid_t type_id, size_t size);
herr_t H5Tset_order (hid_t type_id, H5T_order_t order);
herr_t H5Tset_precision (hid_t type_id, size_t prec);
herr_t H5Tset_offset (hid_t type_id, size_t offset);
herr_t H5Tset_sign (hid_t type_id, H5T_sign_t sign);
herr_t H5Tset_fields (hid_t type_id, size_t spos, size_t epos, size_t esize,
		      size_t mpos, size_t msize);
herr_t H5Tset_ebias (hid_t type_id, size_t ebias);
herr_t H5Tset_norm (hid_t type_id, H5T_norm_t norm);


#ifdef __cplusplus
}
#endif

#endif
