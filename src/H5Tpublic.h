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
   H5T_FIXED		=0,	/*fixed-point types			*/
   H5T_FLOAT		=1,	/*floating-point types			*/
   H5T_DATE		=2,	/*date and time	types			*/
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
} H5T_order_t;

/* Types of fixed-point sign schemes */
typedef enum H5T_sign_t {
   H5T_SGN_ERROR	=-1, 	/*error					*/
   H5T_SGN_NONE		=0, 	/*this is an unsigned type		*/
   H5T_SGN_2		=1 	/*two's complement			*/
} H5T_sign_t;

/* Floating-point normalization schemes */
typedef enum H5T_norm_t {
   H5T_NORM_ERROR	=-1, 	/*error					*/
   H5T_NORM_IMPLIED	=0,	/*msb of mantissa isn't stored, always 1*/
   H5T_NORM_MSBSET	=1, 	/*msb of mantissa is always 1		*/
   H5T_NORM_NONE	=2	/*not normalized			*/
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
   H5T_STR_SPACE	=1, 	/*pad with spaces like in Fortran	*/
} H5T_str_t;



#define H5T_PAD_ZERO	(-1)	/*pad with all zeros			*/
#define H5T_PAD_ONE	(-2)	/*pad with all ones			*/
#define H5T_PAD_FROM(X)	(X)	/*pad with value of bit X		*/

/* The predefined types */
extern hid_t H5T_NATIVE_CHAR;
extern hid_t H5T_NATIVE_UCHAR;
extern hid_t H5T_NATIVE_SHORT;
extern hid_t H5T_NATIVE_USHORT;
extern hid_t H5T_NATIVE_INT;
extern hid_t H5T_NATIVE_UINT;
extern hid_t H5T_NATIVE_LONG;
extern hid_t H5T_NATIVE_ULONG;
extern hid_t H5T_NATIVE_FLOAT;
extern hid_t H5T_NATIVE_DOUBLE;

#ifdef __cplusplus
extern "C" {
#endif

hid_t H5Tcreate (H5T_class_t type, size_t size);
hid_t H5Tcopy (hid_t type_id);
herr_t H5Tclose (hid_t type_id);
hbool_t H5Tequal (hid_t type1_id, hid_t type2_id);

H5T_class_t H5Tget_class (hid_t type_id);
size_t H5Tget_size (hid_t type_id);
intn H5Tget_num_members (hid_t type_id);

herr_t H5Tinsert_member (hid_t parent_id, const char *name, off_t offset,
			 hid_t member_id);


#ifdef __cplusplus
}
#endif

#endif
