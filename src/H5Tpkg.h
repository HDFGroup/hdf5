/*
 * Copyright (C) 1997 NCSA
 *		      All rights reserved.
 *
 * Programmer:	Robb Matzke <matzke@llnl.gov>
 *		Monday, December  8, 1997
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

/*
 * Define this to enable debugging.
 */
#ifdef NDEBUG
#  undef H5T_DEBUG
#endif

#include <H5HGprivate.h>
#include <H5Tprivate.h>

#define H5T_NAMELEN	10	/*length of debugging name buffer	     */

typedef struct H5T_atomic_t {
    H5T_order_t		order;	/*byte order				     */
    size_t		prec;	/*precision in bits			     */
    size_t		offset; /*bit position of lsb of value		     */
    H5T_pad_t	        lsb_pad;/*type of lsb padding			     */
    H5T_pad_t		msb_pad;/*type of msb padding			     */
    union {
	struct {
	    H5T_sign_t	sign;	/*type of integer sign			     */
	} i;			/*integer; integer types		     */

	struct {
	    size_t	sign;	/*bit position of sign bit		     */
	    size_t	epos;	/*position of lsb of exponent		     */
	    size_t	esize;	/*size of exponent in bits		     */
	    uint64	ebias;	/*exponent bias				     */
	    size_t	mpos;	/*position of lsb of mantissa		     */
	    size_t	msize;	/*size of mantissa			     */
	    H5T_norm_t	norm;	/*normalization				     */
	    H5T_pad_t	pad;	/*type of padding for internal bits	     */
	} f;			/*floating-point types			     */

	struct {
	    H5T_cset_t	cset;	/*character set				     */
	    H5T_str_t	pad;	/*space or null padding of extra bytes	     */
	} s;
    } u;
} H5T_atomic_t;

typedef struct H5T_compnd_t {
    intn		nalloc;		/*num entries allocated in MEMB array*/
    intn		nmembs;		/*number of members defined in struct*/
    struct H5T_member_t *memb;		/*array of struct members	     */
} H5T_compnd_t;

typedef enum H5T_state_t {
    H5T_STATE_TRANSIENT, 		/*type is a modifiable transient     */
    H5T_STATE_RDONLY,			/*transient, not modifiable, closable*/
    H5T_STATE_IMMUTABLE,		/*constant, not closable	     */
    H5T_STATE_NAMED,			/*named constant, not open	     */
    H5T_STATE_OPEN			/*named constant, open object header */
} H5T_state_t;

struct H5T_t {
    H5T_state_t		state;	/*current state of the type		     */
    H5G_entry_t		ent;	/*the type is a named type		     */
    H5F_t		*sh_file;/*file pointer if this is a shared type     */
    H5T_class_t		type;	/*which class of type is this?		     */
    size_t		size;	/*total size of an instance of this type     */
    union {
	H5T_atomic_t	atomic; /*an atomic data type			     */
	H5T_compnd_t	compnd; /*a compound data type (struct)		     */
    } u;
};

typedef struct H5T_member_t {
    char		*name;		/*name of this member		     */
    size_t		offset;		/*offset from beginning of struct    */
    intn		ndims;		/*member dimensionality		     */
    size_t		dim[4];		/*size in each dimension	     */
    intn		perm[4];	/*index permutation		     */
    struct H5T_t	*type;		/*type of this member		     */
} H5T_member_t;

/* The data type conversion database */
typedef struct H5T_path_t {
    char	name[H5T_NAMELEN];	/*name for debugging only	     */
    H5T_t	*src;			/*source data type ID		     */
    H5T_t	*dst;			/*destination data type ID	     */
    H5T_conv_t	func;			/*data conversion function	     */
    hbool_t	is_hard;		/*is it a hard function?	     */
    H5T_cdata_t	cdata;			/*data for this function	     */
} H5T_path_t;

/* The master list of soft conversion functions */
typedef struct H5T_soft_t {
    char	name[H5T_NAMELEN];	/*name for debugging only	     */
    H5T_class_t src;			/*source data type class	     */
    H5T_class_t dst;			/*destination data type class	     */
    H5T_conv_t	func;			/*the conversion function	     */
} H5T_soft_t;

/* Bit search direction */
typedef enum H5T_sdir_t {
    H5T_BIT_LSB,			/*search lsb toward msb		     */
    H5T_BIT_MSB				/*search msb toward lsb		     */
} H5T_sdir_t;

/* Function prototypes for H5T package scope */
H5T_path_t *H5T_path_find (const char *name, const H5T_t *src,
			   const H5T_t *dst, hbool_t create, H5T_conv_t func);

/* Conversion functions */
herr_t H5T_conv_order (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, void *_buf, void *bkg);
herr_t H5T_conv_struct (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
			size_t nelmts, void *_buf, void *bkg);
herr_t H5T_conv_i_i (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, void *_buf, void __unused__ *bkg);
herr_t H5T_conv_i32le_f64le (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
			     size_t nelmts, void *_buf, void __unused__ *bkg);

/* Bit twiddling functions */
void H5T_bit_copy (uint8 *dst, size_t dst_offset, const uint8 *src,
		   size_t src_offset, size_t size);
void H5T_bit_set (uint8 *buf, size_t offset, size_t size, hbool_t value);
ssize_t H5T_bit_find (uint8 *buf, size_t offset, size_t size,
		      H5T_sdir_t direction, hbool_t value);

#endif
