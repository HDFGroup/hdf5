/*
 * Copyright (C) 1997 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Monday, December  8, 1997
 *
 * Purpose:	This file contains declarations which are visible only within
 *		the H5T package.  Source files outside the H5T package should
 *		include H5Tprivate.h instead.
 */
#ifndef H5T_PACKAGE
#error "Do not include this file outside the H5T package!"
#endif

#ifndef _H5Tpkg_H
#define _H5Tpkg_H

#include <H5Tprivate.h>

typedef struct H5T_atomic_t {
   H5T_order_t		order;	/*byte order				*/
   size_t		prec;	/*precision in bits			*/
   size_t		offset;	/*bit position of lsb of value		*/
   intn			lsb_pad;/*type of lsb padding			*/
   intn			msb_pad;/*type of msb padding			*/
   union {
      struct {
	 H5T_sign_t	sign;	/*type of integer sign			*/
      } i;			/*integer; integer types		*/

      struct {
	 size_t		sign;	/*bit position of sign bit		*/
	 size_t		epos;	/*position of lsb of exponent		*/
	 size_t		esize;	/*size of exponent in bits		*/
	 uint64		ebias;	/*exponent bias				*/
	 size_t		mpos;	/*position of lsb of mantissa		*/
	 size_t		msize;	/*size of mantissa			*/
	 H5T_norm_t	norm;	/*normalization				*/
	 intn		pad;	/*type of padding for internal bits	*/
      } f;			/*floating-point types			*/

      struct {
	 H5T_cset_t	cset;	/*character set				*/
	 H5T_str_t	pad;	/*space or null padding of extra bytes	*/
      } s;
   } u;
} H5T_atomic_t;

typedef struct H5T_compnd_t {
   intn			nalloc;	/*num entries allocated in MEMB array	*/
   intn			nmembs;	/*number of members defined in struct	*/
   struct H5T_member_t	*memb;	/*array of struct members		*/
} H5T_compnd_t;

struct H5T_t {
   hbool_t		locked;	/*if locked, then can't be modified	*/
   H5T_class_t		type;	/*which class of type is this?		*/
   size_t		size;	/*total size of an instance of this type*/
   union {
      H5T_atomic_t	atomic;	/*an atomic data type			*/
      H5T_compnd_t	compnd;	/*a compound data type (struct)		*/
   } u;
};

typedef struct H5T_member_t {
   char			*name;	/*name of this member			*/
   size_t		offset;	/*offset from beginning of struct	*/
   intn			ndims;	/*member dimensionality			*/
   size_t		dim[4];	/*size in each dimension		*/
   size_t		perm[4];/*index permutation			*/
   struct H5T_t		type;	/*type of this member			*/
} H5T_member_t;

/* The data type conversion database */
typedef struct H5T_path_t {
   H5T_t		*src;	/*source data type ID			*/
   H5T_t		*dst;	/*destination data type	ID		*/
   H5T_conv_t		hard;	/*hard conversion function or null	*/
   H5T_conv_t		soft;	/*soft conversion function or null	*/
} H5T_path_t;

/* The master list of soft conversion functions */
typedef struct H5T_soft_t {
   H5T_class_t		src;	/*source data type class		*/
   H5T_class_t		dst;	/*destination data type class		*/
   H5T_conv_t		func;	/*the conversion function		*/
} H5T_soft_t;

H5T_path_t *H5T_path_find (const H5T_t *src, const H5T_t *dst, hbool_t create);

/* Conversion functions */
herr_t H5T_conv_noop (hid_t src_id, hid_t dst_id, size_t nelmts,
		      void *buf, const void *background);
herr_t H5T_conv_order (hid_t src_id, hid_t dst_id, size_t nelmts,
		       void *_buf, const void *background);

#endif
