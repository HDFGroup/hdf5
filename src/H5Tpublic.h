/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * This file contains public declarations for the H5T module.
 */
#ifndef _H5Tpublic_H
#define _H5Tpublic_H

/* Public headers needed by this file */
#include "H5public.h"
#include "H5Ipublic.h"

#define HOFFSET(S,M)    (offsetof(S,M))

/**
 * These are the various classes of datatypes
 * internal If this goes over 16 types (0-15), the file format will need to
 *          change.
 */
typedef enum H5T_class_t {
    H5T_NO_CLASS         = -1,  /**< error                                   */
    H5T_INTEGER          = 0,   /**< integer types                           */
    H5T_FLOAT            = 1,   /**< floating-point types                    */
    H5T_TIME             = 2,   /**< date and time types                     */
    H5T_STRING           = 3,   /**< character string types                  */
    H5T_BITFIELD         = 4,   /**< bit field types                         */
    H5T_OPAQUE           = 5,   /**< opaque types                            */
    H5T_COMPOUND         = 6,   /**< compound types                          */
    H5T_REFERENCE        = 7,   /**< reference types                         */
    H5T_ENUM		 = 8,	    /**< enumeration types                       */
    H5T_VLEN		 = 9,	    /**< variable-Length types                   */
    H5T_ARRAY	         = 10,	/**< array types                             */

    H5T_NCLASSES                /*this must be last                          */
} H5T_class_t;

/**
 * Byte orders
 */
typedef enum H5T_order_t {
    H5T_ORDER_ERROR      = -1,  /**< error                                   */
    H5T_ORDER_LE         = 0,   /**< little endian                           */
    H5T_ORDER_BE         = 1,   /**< bit endian                              */
    H5T_ORDER_VAX        = 2,   /**< VAX mixed endian                        */
    H5T_ORDER_MIXED      = 3,   /**< Compound type with mixed member orders  */
    H5T_ORDER_NONE       = 4    /**< no particular order (strings, bits,..)  */
    /*H5T_ORDER_NONE must be last */
} H5T_order_t;

/**
 * Types of integer sign schemes
 */
typedef enum H5T_sign_t {
    H5T_SGN_ERROR        = -1,  /**< error                                   */
    H5T_SGN_NONE         = 0,   /**< this is an unsigned type                */
    H5T_SGN_2            = 1,   /**< two's complement                        */

    H5T_NSGN             = 2    /*this must be last!                         */
} H5T_sign_t;

/**
 * Floating-point normalization schemes
 */
typedef enum H5T_norm_t {
    H5T_NORM_ERROR       = -1,  /**< error                                   */
    H5T_NORM_IMPLIED     = 0,   /**< msb of mantissa isn't stored, always 1  */
    H5T_NORM_MSBSET      = 1,   /**< msb of mantissa is always 1             */
    H5T_NORM_NONE        = 2    /**< not normalized                          */
    /*H5T_NORM_NONE must be last */
} H5T_norm_t;

/**
 * Character set to use for text strings.
 * \internal Do not change these values since they appear in HDF5 files!
 */
typedef enum H5T_cset_t {
    H5T_CSET_ERROR       = -1,  /**< error                           */
    H5T_CSET_ASCII       = 0,   /**< US ASCII                        */
    H5T_CSET_UTF8        = 1,   /**< UTF-8 Unicode encoding		     */
    H5T_CSET_RESERVED_2  = 2,   /**< reserved for later use		     */
    H5T_CSET_RESERVED_3  = 3,   /**< reserved for later use		     */
    H5T_CSET_RESERVED_4  = 4,   /**< reserved for later use		     */
    H5T_CSET_RESERVED_5  = 5,   /**< reserved for later use		     */
    H5T_CSET_RESERVED_6  = 6,   /**< reserved for later use		     */
    H5T_CSET_RESERVED_7  = 7,   /**< reserved for later use		     */
    H5T_CSET_RESERVED_8  = 8,   /**< reserved for later use		     */
    H5T_CSET_RESERVED_9  = 9,   /**< reserved for later use		     */
    H5T_CSET_RESERVED_10 = 10,  /**< reserved for later use		     */
    H5T_CSET_RESERVED_11 = 11,  /**< reserved for later use		     */
    H5T_CSET_RESERVED_12 = 12,  /**< reserved for later use		     */
    H5T_CSET_RESERVED_13 = 13,  /**< reserved for later use		     */
    H5T_CSET_RESERVED_14 = 14,  /**< reserved for later use		     */
    H5T_CSET_RESERVED_15 = 15   /**< reserved for later use		     */
} H5T_cset_t;
#define H5T_NCSET H5T_CSET_RESERVED_2    		/*Number of character sets actually defined  */

/**
 * Type of padding to use in character strings.
 * \internal  Do not change these values since they appear in HDF5 files!
 */
typedef enum H5T_str_t {
    H5T_STR_ERROR        = -1,  /**< error                           */
    H5T_STR_NULLTERM     = 0,   /**< null terminate like in C        */
    H5T_STR_NULLPAD      = 1,   /**< pad with nulls                  */
    H5T_STR_SPACEPAD     = 2,   /**< pad with spaces like in Fortran */
    H5T_STR_RESERVED_3   = 3,   /**< reserved for later use		     */
    H5T_STR_RESERVED_4   = 4,   /**< reserved for later use		     */
    H5T_STR_RESERVED_5   = 5,   /**< reserved for later use		     */
    H5T_STR_RESERVED_6   = 6,   /**< reserved for later use		     */
    H5T_STR_RESERVED_7   = 7,   /**< reserved for later use		     */
    H5T_STR_RESERVED_8   = 8,   /**< reserved for later use		     */
    H5T_STR_RESERVED_9   = 9,   /**< reserved for later use		     */
    H5T_STR_RESERVED_10  = 10,  /**< reserved for later use		     */
    H5T_STR_RESERVED_11  = 11,  /**< reserved for later use		     */
    H5T_STR_RESERVED_12  = 12,  /**< reserved for later use		     */
    H5T_STR_RESERVED_13  = 13,  /**< reserved for later use		     */
    H5T_STR_RESERVED_14  = 14,  /**< reserved for later use		     */
    H5T_STR_RESERVED_15  = 15   /**< reserved for later use		     */
} H5T_str_t;
#define H5T_NSTR H5T_STR_RESERVED_3		/*num H5T_str_t types actually defined	     */

/**
 * Type of padding to use in other atomic types
 */
typedef enum H5T_pad_t {
    H5T_PAD_ERROR        = -1,  /**< error                           */
    H5T_PAD_ZERO         = 0,   /**< always set to zero              */
    H5T_PAD_ONE          = 1,   /**< always set to one               */
    H5T_PAD_BACKGROUND   = 2,   /**< set to background value         */

    H5T_NPAD             = 3    /*THIS MUST BE LAST                          */
} H5T_pad_t;

/**
 * Commands sent to conversion functions
 */
typedef enum H5T_cmd_t {
    H5T_CONV_INIT	= 0,	/**< query and/or initialize private data	     */
    H5T_CONV_CONV	= 1, 	/**< convert data from source to dest datatype */
    H5T_CONV_FREE	= 2	/**< function is being removed from path	     */
} H5T_cmd_t;

/**
 * How is the `bkg' buffer used by the conversion function?
 */
typedef enum H5T_bkg_t {
    H5T_BKG_NO		= 0, 	/**< background buffer is not needed, send NULL */
    H5T_BKG_TEMP	= 1,	/**< bkg buffer used as temp storage only       */
    H5T_BKG_YES		= 2	/**< init bkg buf with data before conversion   */
} H5T_bkg_t;

/**
 * Type conversion client data
 */
//! [H5T_cdata_t_snip]
typedef struct H5T_cdata_t {
    H5T_cmd_t		command;/**< what should the conversion function do?    */
    H5T_bkg_t		need_bkg;/**< is the background buffer needed?	     */
    hbool_t		recalc;	/**< recalculate private data		     */
    void		*priv;	/**< private data				     */
} H5T_cdata_t;
//! [H5T_cdata_t_snip]

/**
 * Conversion function persistence
 */
typedef enum H5T_pers_t {
    H5T_PERS_DONTCARE	= -1, 	/**< wild card				     */
    H5T_PERS_HARD	= 0,	/**< hard conversion function		     */
    H5T_PERS_SOFT	= 1 	/**< soft conversion function		     */
} H5T_pers_t;

/**
 * The order to retrieve atomic native datatype
 */
typedef enum H5T_direction_t {
    H5T_DIR_DEFAULT     = 0,    /**< default direction is inscendent        */
    H5T_DIR_ASCEND      = 1,    /**< in inscendent order                    */
    H5T_DIR_DESCEND     = 2     /**< in descendent order                    */
} H5T_direction_t;

/**
 * The exception type passed into the conversion callback function
 */
typedef enum H5T_conv_except_t {
    H5T_CONV_EXCEPT_RANGE_HI       = 0,   /**< source value is greater than destination's range */
    H5T_CONV_EXCEPT_RANGE_LOW      = 1,   /**< source value is less than destination's range    */
    H5T_CONV_EXCEPT_PRECISION      = 2,   /**< source value loses precision in destination      */
    H5T_CONV_EXCEPT_TRUNCATE       = 3,   /**< source value is truncated in destination         */
    H5T_CONV_EXCEPT_PINF           = 4,   /**< source value is positive infinity(floating number) */
    H5T_CONV_EXCEPT_NINF           = 5,   /**< source value is negative infinity(floating number) */
    H5T_CONV_EXCEPT_NAN            = 6    /**< source value is NaN(floating number)             */
} H5T_conv_except_t;

/**
 * The return value from conversion callback function H5T_conv_except_func_t()
 */
typedef enum H5T_conv_ret_t {
    H5T_CONV_ABORT      = -1,   /**< abort conversion                           */
    H5T_CONV_UNHANDLED  = 0,    /**< callback function failed to handle the exception      */
    H5T_CONV_HANDLED    = 1     /**< callback function handled the exception successfully  */
} H5T_conv_ret_t;

/**
 * Variable Length Datatype struct in memory (This is only used for VL
 * sequences, not VL strings, which are stored in char *'s)
 */
typedef struct {
    size_t len; /**< Length of VL data (in base type units) */
    void *p;    /**< Pointer to VL data */
} hvl_t;

/* Variable Length String information */
/**
 * Indicate that a string is variable length (null-terminated in C, instead of
 * fixed length)
 */
#define H5T_VARIABLE    ((size_t)(-1))

/* Opaque information */
/**
 * Maximum length of an opaque tag
 * \internal This could be raised without too much difficulty
 */
#define H5T_OPAQUE_TAG_MAX      256


#ifdef __cplusplus
extern "C" {
#endif

/**
 * All datatype conversion functions are...
 */
//! [H5T_conv_t_snip]
typedef herr_t (*H5T_conv_t) (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
      size_t nelmts, size_t buf_stride, size_t bkg_stride, void *buf,
      void *bkg, hid_t dset_xfer_plist);
//! [H5T_conv_t_snip]

/**
 * Exception handler.  If an exception like overflow happenes during conversion,
 * this function is called if it's registered through H5Pset_type_conv_cb().
 */
typedef H5T_conv_ret_t (*H5T_conv_except_func_t)(H5T_conv_except_t except_type,
    hid_t src_id, hid_t dst_id, void *src_buf, void *dst_buf, void *user_data);

/* When this header is included from a private header, don't make calls to H5open() */
#undef H5OPEN
#ifndef _H5private_H
#define H5OPEN          H5open(),
#else   /* _H5private_H */
#define H5OPEN
#endif  /* _H5private_H */

/**
 * \ingroup PDTIEEE
 * 32-bit big-endian IEEE floating-point numbers
 */
#define H5T_IEEE_F32BE		(H5OPEN H5T_IEEE_F32BE_g)
/**
 * \ingroup PDTIEEE
 * 32-bit little-endian IEEE floating-point numbers
 */
#define H5T_IEEE_F32LE		(H5OPEN H5T_IEEE_F32LE_g)
/**
 * \ingroup PDTIEEE
 * 64-bit big-endian IEEE floating-point numbers
 */
#define H5T_IEEE_F64BE		(H5OPEN H5T_IEEE_F64BE_g)
/**
 * \ingroup PDTIEEE
 * 64-bit little-endian IEEE floating-point numbers
 */
#define H5T_IEEE_F64LE		(H5OPEN H5T_IEEE_F64LE_g)
H5_DLLVAR hid_t H5T_IEEE_F32BE_g;
H5_DLLVAR hid_t H5T_IEEE_F32LE_g;
H5_DLLVAR hid_t H5T_IEEE_F64BE_g;
H5_DLLVAR hid_t H5T_IEEE_F64LE_g;

/**
 * \ingroup PDTSTD
 * 8-bit big-endian signed integers
 */
#define H5T_STD_I8BE		(H5OPEN H5T_STD_I8BE_g)
/**
 * \ingroup PDTSTD
 * 8-bit little-endian signed integers
 */
#define H5T_STD_I8LE		(H5OPEN H5T_STD_I8LE_g)
/**
 * \ingroup PDTSTD
 * 16-bit big-endian signed integers
 */
#define H5T_STD_I16BE		(H5OPEN H5T_STD_I16BE_g)
/**
 * \ingroup PDTSTD
 * 16-bit little-endian signed integers
 */
#define H5T_STD_I16LE		(H5OPEN H5T_STD_I16LE_g)
/**
 * \ingroup PDTSTD
 * 32-bit big-endian signed integers
 */
#define H5T_STD_I32BE		(H5OPEN H5T_STD_I32BE_g)
/**
 * \ingroup PDTSTD
 * 32-bit little-endian signed integers
 */
#define H5T_STD_I32LE		(H5OPEN H5T_STD_I32LE_g)
/**
 * \ingroup PDTSTD
 * 64-bit big-endian signed integers
 */
#define H5T_STD_I64BE		(H5OPEN H5T_STD_I64BE_g)
/**
 * \ingroup PDTSTD
 * 64-bit little-endian signed integers
 */
#define H5T_STD_I64LE		(H5OPEN H5T_STD_I64LE_g)
/**
 * \ingroup PDTSTD
 * 8-bit big-endian unsigned integers
 */
#define H5T_STD_U8BE		(H5OPEN H5T_STD_U8BE_g)
/**
 * \ingroup PDTSTD
 * 8-bit little-endian unsigned integers
 */
#define H5T_STD_U8LE		(H5OPEN H5T_STD_U8LE_g)
/**
 * \ingroup PDTSTD
 * 16-bit big-endian unsigned integers
 */
#define H5T_STD_U16BE		(H5OPEN H5T_STD_U16BE_g)
/**
 * \ingroup PDTSTD
 * 16-bit little-endian unsigned integers
 */
#define H5T_STD_U16LE		(H5OPEN H5T_STD_U16LE_g)
/**
 * \ingroup PDTSTD
 * 32-bit big-endian unsigned integers
 */
#define H5T_STD_U32BE		(H5OPEN H5T_STD_U32BE_g)
/**
 * \ingroup PDTSTD
 * 32-bit little-endian unsigned integers
 */
#define H5T_STD_U32LE		(H5OPEN H5T_STD_U32LE_g)
/**
 * \ingroup PDTSTD
 * 64-bit big-endian unsigned integers
 */
#define H5T_STD_U64BE		(H5OPEN H5T_STD_U64BE_g)
/**
 * \ingroup PDTSTD
 * 64-bit little-endian unsigned integers
 */
#define H5T_STD_U64LE		(H5OPEN H5T_STD_U64LE_g)
/**
 * \ingroup PDTSTD
 * 8-bit big-endian bitfield
 */
#define H5T_STD_B8BE		(H5OPEN H5T_STD_B8BE_g)
/**
 * \ingroup PDTSTD
 * 8-bit little-endian bitfield
 */
#define H5T_STD_B8LE		(H5OPEN H5T_STD_B8LE_g)
/**
 * \ingroup PDTSTD
 * 16-bit big-endian bitfield
 */
#define H5T_STD_B16BE		(H5OPEN H5T_STD_B16BE_g)
/**
 * \ingroup PDTSTD
 * 16-bit little-endian bitfield
 */
#define H5T_STD_B16LE		(H5OPEN H5T_STD_B16LE_g)
/**
 * \ingroup PDTSTD
 * 32-bit big-endian bitfield
 */
#define H5T_STD_B32BE		(H5OPEN H5T_STD_B32BE_g)
/**
 * \ingroup PDTSTD
 * 32-bit little-endian bitfield
 */
#define H5T_STD_B32LE		(H5OPEN H5T_STD_B32LE_g)
/**
 * \ingroup PDTSTD
 * 64-bit big-endian bitfield
 */
#define H5T_STD_B64BE		(H5OPEN H5T_STD_B64BE_g)
/**
 * \ingroup PDTSTD
 * 64-bit little-endian bitfield
 */
#define H5T_STD_B64LE		(H5OPEN H5T_STD_B64LE_g)
/**
 * \ingroup PDTSTD
 * Object reference
 */
#define H5T_STD_REF_OBJ         (H5OPEN H5T_STD_REF_OBJ_g)
/**
 * \ingroup PDTSTD
 * Dataset region reference
 */
#define H5T_STD_REF_DSETREG     (H5OPEN H5T_STD_REF_DSETREG_g)
/**
 * \ingroup PDTSTD
 * Generic reference
 */
#define H5T_STD_REF             (H5OPEN H5T_STD_REF_g)
H5_DLLVAR hid_t H5T_STD_I8BE_g;
H5_DLLVAR hid_t H5T_STD_I8LE_g;
H5_DLLVAR hid_t H5T_STD_I16BE_g;
H5_DLLVAR hid_t H5T_STD_I16LE_g;
H5_DLLVAR hid_t H5T_STD_I32BE_g;
H5_DLLVAR hid_t H5T_STD_I32LE_g;
H5_DLLVAR hid_t H5T_STD_I64BE_g;
H5_DLLVAR hid_t H5T_STD_I64LE_g;
H5_DLLVAR hid_t H5T_STD_U8BE_g;
H5_DLLVAR hid_t H5T_STD_U8LE_g;
H5_DLLVAR hid_t H5T_STD_U16BE_g;
H5_DLLVAR hid_t H5T_STD_U16LE_g;
H5_DLLVAR hid_t H5T_STD_U32BE_g;
H5_DLLVAR hid_t H5T_STD_U32LE_g;
H5_DLLVAR hid_t H5T_STD_U64BE_g;
H5_DLLVAR hid_t H5T_STD_U64LE_g;
H5_DLLVAR hid_t H5T_STD_B8BE_g;
H5_DLLVAR hid_t H5T_STD_B8LE_g;
H5_DLLVAR hid_t H5T_STD_B16BE_g;
H5_DLLVAR hid_t H5T_STD_B16LE_g;
H5_DLLVAR hid_t H5T_STD_B32BE_g;
H5_DLLVAR hid_t H5T_STD_B32LE_g;
H5_DLLVAR hid_t H5T_STD_B64BE_g;
H5_DLLVAR hid_t H5T_STD_B64LE_g;
H5_DLLVAR hid_t H5T_STD_REF_OBJ_g;
H5_DLLVAR hid_t H5T_STD_REF_DSETREG_g;
H5_DLLVAR hid_t H5T_STD_REF_g;

/**
 * \ingroup PDTUNIX
 */
#define H5T_UNIX_D32BE		(H5OPEN H5T_UNIX_D32BE_g)
/**
 * \ingroup PDTUNIX
 */
#define H5T_UNIX_D32LE		(H5OPEN H5T_UNIX_D32LE_g)
/**
 * \ingroup PDTUNIX
 */
#define H5T_UNIX_D64BE		(H5OPEN H5T_UNIX_D64BE_g)
/**
 * \ingroup PDTUNIX
 */
#define H5T_UNIX_D64LE		(H5OPEN H5T_UNIX_D64LE_g)
H5_DLLVAR hid_t H5T_UNIX_D32BE_g;
H5_DLLVAR hid_t H5T_UNIX_D32LE_g;
H5_DLLVAR hid_t H5T_UNIX_D64BE_g;
H5_DLLVAR hid_t H5T_UNIX_D64LE_g;

/**
 * \ingroup PDTS
 * String datatype in C (size defined in bytes rather than in bits)
 */
#define H5T_C_S1		(H5OPEN H5T_C_S1_g)
H5_DLLVAR hid_t H5T_C_S1_g;

/**
 * \ingroup PDTS
 * String datatype in Fortran (as defined for the HDF5 C library)
 */
#define H5T_FORTRAN_S1		(H5OPEN H5T_FORTRAN_S1_g)
H5_DLLVAR hid_t H5T_FORTRAN_S1_g;

/*
 * These types are for Intel CPU's.  They are little endian with IEEE
 * floating point.
 */
/**
 * \ingroup PDTX86
 * 8-bit little-endian signed (2's complement) integers for Intel CPUs
 */
#define H5T_INTEL_I8		H5T_STD_I8LE
/**
 * \ingroup PDTX86
 * 16-bit little-endian signed (2's complement) integers for Intel CPUs
 */
#define H5T_INTEL_I16		H5T_STD_I16LE
/**
 * \ingroup PDTX86
 * 32-bit little-endian signed (2's complement) integers for Intel CPUs
 */
#define H5T_INTEL_I32		H5T_STD_I32LE
/**
 * \ingroup PDTX86
 * 64-bit little-endian signed (2's complement) integers for Intel CPUs
 */
#define H5T_INTEL_I64		H5T_STD_I64LE
/**
 * \ingroup PDTX86
 * 8-bit little-endian unsigned integers for Intel CPUs
 */
#define H5T_INTEL_U8		H5T_STD_U8LE
/**
 * \ingroup PDTX86
 * 16-bit little-endian unsigned integers for Intel CPUs
 */
#define H5T_INTEL_U16		H5T_STD_U16LE
/**
 * \ingroup PDTX86
 * 32-bit little-endian unsigned integers for Intel CPUs
 */
#define H5T_INTEL_U32		H5T_STD_U32LE
/**
 * \ingroup PDTX86
 * 64-bit little-endian unsigned integers for Intel CPUs
 */
#define H5T_INTEL_U64		H5T_STD_U64LE
/**
 * \ingroup PDTX86
 * 8-bit little-endian bitfield for Intel CPUs
 */
#define H5T_INTEL_B8		H5T_STD_B8LE
/**
 * \ingroup PDTX86
 * 16-bit little-endian bitfield for Intel CPUs
 */
#define H5T_INTEL_B16		H5T_STD_B16LE
/**
 * \ingroup PDTX86
 * 32-bit little-endian bitfield for Intel CPUs
 */
#define H5T_INTEL_B32		H5T_STD_B32LE
/**
 * \ingroup PDTX86
 * 64-bit little-endian bitfield for Intel CPUs
 */
#define H5T_INTEL_B64		H5T_STD_B64LE
/**
 * \ingroup PDTX86
 * 32-bit little-endian IEEE floating-point numbers for Intel CPUs
 */
#define H5T_INTEL_F32		H5T_IEEE_F32LE
/**
 * \ingroup PDTX86
 * 64-bit little-endian IEEE floating-point numbers for Intel CPUs
 */
#define H5T_INTEL_F64		H5T_IEEE_F64LE

/*
 * These types are for DEC Alpha CPU's.  They are little endian with IEEE
 * floating point.
 */
/**
 * \ingroup PDTALPHA
 * 8-bit little-endian signed (2's complement) integers for DEC Alpha CPUs
 */
#define H5T_ALPHA_I8		H5T_STD_I8LE
/**
 * \ingroup PDTALPHA
 * 16-bit little-endian signed (2's complement) integers for DEC Alpha CPUs
 */
#define H5T_ALPHA_I16		H5T_STD_I16LE
/**
 * \ingroup PDTALPHA
 * 32-bit little-endian signed (2's complement) integers for DEC Alpha CPUs
 */
#define H5T_ALPHA_I32		H5T_STD_I32LE
/**
 * \ingroup PDTALPHA
 * 64-bit little-endian signed (2's complement) integers for DEC Alpha CPUs
 */
#define H5T_ALPHA_I64		H5T_STD_I64LE
/**
 * \ingroup PDTALPHA
 * 8-bit little-endian unsigned integers for DEC Alpha CPUs
 */
#define H5T_ALPHA_U8		H5T_STD_U8LE
/**
 * \ingroup PDTALPHA
 * 16-bit little-endian unsigned integers for DEC Alpha CPUs
 */
#define H5T_ALPHA_U16		H5T_STD_U16LE
/**
 * \ingroup PDTALPHA
 * 32-bit little-endian unsigned integers for DEC Alpha CPUs
 */
#define H5T_ALPHA_U32		H5T_STD_U32LE
/**
 * \ingroup PDTALPHA
 * 64-bit little-endian unsigned integers for DEC Alpha CPUs
 */
#define H5T_ALPHA_U64		H5T_STD_U64LE
/**
 * \ingroup PDTALPHA
 * 8-bit little-endian bitfield for DEC Alpha CPUs
 */
#define H5T_ALPHA_B8		H5T_STD_B8LE
/**
 * \ingroup PDTALPHA
 * 16-bit little-endian bitfield for DEC Alpha CPUs
 */
#define H5T_ALPHA_B16		H5T_STD_B16LE
/**
 * \ingroup PDTALPHA
 * 32-bit little-endian bitfield for DEC Alpha CPUs
 */
#define H5T_ALPHA_B32		H5T_STD_B32LE
/**
 * \ingroup PDTALPHA
 * 64-bit little-endian bitfield for DEC Alpha CPUs
 */
#define H5T_ALPHA_B64		H5T_STD_B64LE
/**
 * \ingroup PDTALPHA
 * 32-bit little-endian IEEE floating-point numbers for DEC Alpha CPUs
 */
#define H5T_ALPHA_F32		H5T_IEEE_F32LE
/**
 * \ingroup PDTALPHA
 * 64-bit little-endian IEEE floating-point numbers for DEC Alpha CPUs
 */
#define H5T_ALPHA_F64		H5T_IEEE_F64LE

/*
 * These types are for MIPS cpu's commonly used in SGI systems. They are big
 * endian with IEEE floating point.
 */
/**
 * \ingroup PDTMIPS
 * 8-bit big-endian signed (2's complement) integers for SGI MIPS CPUs
 */
#define H5T_MIPS_I8		H5T_STD_I8BE
/**
 * \ingroup PDTMIPS
 * 16-bit big-endian signed (2's complement) integers for SGI MIPS CPUs
 */
#define H5T_MIPS_I16		H5T_STD_I16BE
/**
 * \ingroup PDTMIPS
 * 32-bit big-endian signed (2's complement) integers for SGI MIPS CPUs
 */
#define H5T_MIPS_I32		H5T_STD_I32BE
/**
 * \ingroup PDTMIPS
 * 64-bit big-endian signed (2's complement) integers for SGI MIPS CPUs
 */
#define H5T_MIPS_I64		H5T_STD_I64BE
/**
 * \ingroup PDTMIPS
 * 8-bit big-endian unsigned integers for SGI MIPS CPUs
 */
#define H5T_MIPS_U8		H5T_STD_U8BE
/**
 * \ingroup PDTMIPS
 * 16-bit big-endian unsigned integers for SGI MIPS CPUs
 */
#define H5T_MIPS_U16		H5T_STD_U16BE
/**
 * \ingroup PDTMIPS
 * 32-bit big-endian unsigned integers for SGI MIPS CPUs
 */
#define H5T_MIPS_U32		H5T_STD_U32BE
/**
 * \ingroup PDTMIPS
 * 64-bit big-endian unsigned integers for SGI MIPS CPUs
 */
#define H5T_MIPS_U64		H5T_STD_U64BE
/**
 * \ingroup PDTMIPS
 * 8-bit big-endian bitfield for SGI MIPS CPUs
 */
#define H5T_MIPS_B8		H5T_STD_B8BE
/**
 * \ingroup PDTMIPS
 * 16-bit big-endian bitfield for SGI MIPS CPUs
 */
#define H5T_MIPS_B16		H5T_STD_B16BE
/**
 * \ingroup PDTMIPS
 * 32-bit big-endian bitfield for SGI MIPS CPUs
 */
#define H5T_MIPS_B32		H5T_STD_B32BE
/**
 * \ingroup PDTMIPS
 * 64-bit big-endian bitfield for SGI MIPS CPUs
 */
#define H5T_MIPS_B64		H5T_STD_B64BE
/**
 * \ingroup PDTMIPS
 * 32-bit big-endian IEEE floating-point numbers for MIPS CPUs
 */
#define H5T_MIPS_F32		H5T_IEEE_F32BE
/**
 * \ingroup PDTMIPS
 * 64-bit big-endian IEEE floating-point numbers for MIPS CPUs
 */
#define H5T_MIPS_F64		H5T_IEEE_F64BE

/*
 * The VAX floating point types (i.e. in VAX byte order)
 */
/**
 * \ingroup PDTALPHA
 * 32-bit VAX byte order floating-point numbers for OpenVMS on DEC Alpha CPUs
 */
#define H5T_VAX_F32		(H5OPEN H5T_VAX_F32_g)
/**
 * \ingroup PDTALPHA
 * 64-bit VAX byte order floating-point numbers for OpenVMS on DEC Alpha CPUs
 */
#define H5T_VAX_F64		(H5OPEN H5T_VAX_F64_g)
H5_DLLVAR hid_t H5T_VAX_F32_g;
H5_DLLVAR hid_t H5T_VAX_F64_g;

/**
 * \ingroup PDTNAT
 * C-style \c char
 */
#define H5T_NATIVE_CHAR		(CHAR_MIN?H5T_NATIVE_SCHAR:H5T_NATIVE_UCHAR)
/**
 * \ingroup PDTNAT
 * C-style \Code{signed char}
 */
#define H5T_NATIVE_SCHAR        (H5OPEN H5T_NATIVE_SCHAR_g)
/**
 * \ingroup PDTNAT
 * C-style \Code{unsigned char}
 */
#define H5T_NATIVE_UCHAR        (H5OPEN H5T_NATIVE_UCHAR_g)
/**
 * \ingroup PDTNAT
 * C-style \Code{short}
 */
#define H5T_NATIVE_SHORT        (H5OPEN H5T_NATIVE_SHORT_g)
/**
 * \ingroup PDTNAT
 * C-style \Code{unsigned short}
 */
#define H5T_NATIVE_USHORT       (H5OPEN H5T_NATIVE_USHORT_g)
/**
 * \ingroup PDTNAT
 * C-style \Code{int}
 */
#define H5T_NATIVE_INT          (H5OPEN H5T_NATIVE_INT_g)
/**
 * \ingroup PDTNAT
 * C-style \Code{unsigned int}
 */
#define H5T_NATIVE_UINT         (H5OPEN H5T_NATIVE_UINT_g)
/**
 * \ingroup PDTNAT
 * C-style \Code{long}
 */
#define H5T_NATIVE_LONG         (H5OPEN H5T_NATIVE_LONG_g)
/**
 * \ingroup PDTNAT
 * C-style \Code{unsigned long}
 */
#define H5T_NATIVE_ULONG        (H5OPEN H5T_NATIVE_ULONG_g)
/**
 * \ingroup PDTNAT
 * C-style \Code{long long}
 */
#define H5T_NATIVE_LLONG        (H5OPEN H5T_NATIVE_LLONG_g)
/**
 * \ingroup PDTNAT
 * C-style \Code{unsigned long long}
 */
#define H5T_NATIVE_ULLONG       (H5OPEN H5T_NATIVE_ULLONG_g)
/**
 * \ingroup PDTNAT
 * C-style \Code{float}
 */
#define H5T_NATIVE_FLOAT        (H5OPEN H5T_NATIVE_FLOAT_g)
/**
 * \ingroup PDTNAT
 * C-style \Code{double}
 */
#define H5T_NATIVE_DOUBLE       (H5OPEN H5T_NATIVE_DOUBLE_g)
#if H5_SIZEOF_LONG_DOUBLE !=0
/**
 * \ingroup PDTNAT
 * C-style \Code{long double}
 */
#define H5T_NATIVE_LDOUBLE	(H5OPEN H5T_NATIVE_LDOUBLE_g)
#endif
/**
 * \ingroup PDTNAT
 * HDF5 8-bit bitfield based on native types
 */
#define H5T_NATIVE_B8		(H5OPEN H5T_NATIVE_B8_g)
/**
 * \ingroup PDTNAT
 * HDF5 16-bit bitfield based on native types
 */
#define H5T_NATIVE_B16		(H5OPEN H5T_NATIVE_B16_g)
/**
 * \ingroup PDTNAT
 * HDF5 32-bit bitfield based on native types
 */
#define H5T_NATIVE_B32		(H5OPEN H5T_NATIVE_B32_g)
/**
 * \ingroup PDTNAT
 * HDF5 64-bit bitfield based on native types
 */
#define H5T_NATIVE_B64		(H5OPEN H5T_NATIVE_B64_g)
/**
 * \ingroup PDTNAT
 * HDF5 opaque unit based on native types
 */
#define H5T_NATIVE_OPAQUE       (H5OPEN H5T_NATIVE_OPAQUE_g)
/**
 * \ingroup PDTNAT
 * HDF5 address type based on native types
 */
#define H5T_NATIVE_HADDR	(H5OPEN H5T_NATIVE_HADDR_g)
/**
 * \ingroup PDTNAT
 * HDF5 size type based on native types
 */
#define H5T_NATIVE_HSIZE	(H5OPEN H5T_NATIVE_HSIZE_g)
/**
 * \ingroup PDTNAT
 * HDF5 signed size type based on native types
 */
#define H5T_NATIVE_HSSIZE	(H5OPEN H5T_NATIVE_HSSIZE_g)
/**
 * \ingroup PDTNAT
 * HDF5 error code type based on native types
 */
#define H5T_NATIVE_HERR		(H5OPEN H5T_NATIVE_HERR_g)
/**
 * \ingroup PDTNAT
 * HDF5 Boolean type based on native types
 */
#define H5T_NATIVE_HBOOL	(H5OPEN H5T_NATIVE_HBOOL_g)
H5_DLLVAR hid_t H5T_NATIVE_SCHAR_g;
H5_DLLVAR hid_t H5T_NATIVE_UCHAR_g;
H5_DLLVAR hid_t H5T_NATIVE_SHORT_g;
H5_DLLVAR hid_t H5T_NATIVE_USHORT_g;
H5_DLLVAR hid_t H5T_NATIVE_INT_g;
H5_DLLVAR hid_t H5T_NATIVE_UINT_g;
H5_DLLVAR hid_t H5T_NATIVE_LONG_g;
H5_DLLVAR hid_t H5T_NATIVE_ULONG_g;
H5_DLLVAR hid_t H5T_NATIVE_LLONG_g;
H5_DLLVAR hid_t H5T_NATIVE_ULLONG_g;
H5_DLLVAR hid_t H5T_NATIVE_FLOAT_g;
H5_DLLVAR hid_t H5T_NATIVE_DOUBLE_g;
#if H5_SIZEOF_LONG_DOUBLE !=0
H5_DLLVAR hid_t H5T_NATIVE_LDOUBLE_g;
#endif
H5_DLLVAR hid_t H5T_NATIVE_B8_g;
H5_DLLVAR hid_t H5T_NATIVE_B16_g;
H5_DLLVAR hid_t H5T_NATIVE_B32_g;
H5_DLLVAR hid_t H5T_NATIVE_B64_g;
H5_DLLVAR hid_t H5T_NATIVE_OPAQUE_g;
H5_DLLVAR hid_t H5T_NATIVE_HADDR_g;
H5_DLLVAR hid_t H5T_NATIVE_HSIZE_g;
H5_DLLVAR hid_t H5T_NATIVE_HSSIZE_g;
H5_DLLVAR hid_t H5T_NATIVE_HERR_g;
H5_DLLVAR hid_t H5T_NATIVE_HBOOL_g;

/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_INT8			(H5OPEN H5T_NATIVE_INT8_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_UINT8		(H5OPEN H5T_NATIVE_UINT8_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_INT_LEAST8		(H5OPEN H5T_NATIVE_INT_LEAST8_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_UINT_LEAST8		(H5OPEN H5T_NATIVE_UINT_LEAST8_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_INT_FAST8 		(H5OPEN H5T_NATIVE_INT_FAST8_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_UINT_FAST8		(H5OPEN H5T_NATIVE_UINT_FAST8_g)
H5_DLLVAR hid_t H5T_NATIVE_INT8_g;
H5_DLLVAR hid_t H5T_NATIVE_UINT8_g;
H5_DLLVAR hid_t H5T_NATIVE_INT_LEAST8_g;
H5_DLLVAR hid_t H5T_NATIVE_UINT_LEAST8_g;
H5_DLLVAR hid_t H5T_NATIVE_INT_FAST8_g;
H5_DLLVAR hid_t H5T_NATIVE_UINT_FAST8_g;

/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_INT16		(H5OPEN H5T_NATIVE_INT16_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_UINT16		(H5OPEN H5T_NATIVE_UINT16_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_INT_LEAST16		(H5OPEN H5T_NATIVE_INT_LEAST16_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_UINT_LEAST16		(H5OPEN H5T_NATIVE_UINT_LEAST16_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_INT_FAST16		(H5OPEN H5T_NATIVE_INT_FAST16_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_UINT_FAST16		(H5OPEN H5T_NATIVE_UINT_FAST16_g)
H5_DLLVAR hid_t H5T_NATIVE_INT16_g;
H5_DLLVAR hid_t H5T_NATIVE_UINT16_g;
H5_DLLVAR hid_t H5T_NATIVE_INT_LEAST16_g;
H5_DLLVAR hid_t H5T_NATIVE_UINT_LEAST16_g;
H5_DLLVAR hid_t H5T_NATIVE_INT_FAST16_g;
H5_DLLVAR hid_t H5T_NATIVE_UINT_FAST16_g;

/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_INT32		(H5OPEN H5T_NATIVE_INT32_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_UINT32		(H5OPEN H5T_NATIVE_UINT32_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_INT_LEAST32		(H5OPEN H5T_NATIVE_INT_LEAST32_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_UINT_LEAST32		(H5OPEN H5T_NATIVE_UINT_LEAST32_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_INT_FAST32		(H5OPEN H5T_NATIVE_INT_FAST32_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_UINT_FAST32		(H5OPEN H5T_NATIVE_UINT_FAST32_g)
H5_DLLVAR hid_t H5T_NATIVE_INT32_g;
H5_DLLVAR hid_t H5T_NATIVE_UINT32_g;
H5_DLLVAR hid_t H5T_NATIVE_INT_LEAST32_g;
H5_DLLVAR hid_t H5T_NATIVE_UINT_LEAST32_g;
H5_DLLVAR hid_t H5T_NATIVE_INT_FAST32_g;
H5_DLLVAR hid_t H5T_NATIVE_UINT_FAST32_g;

/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_INT64		(H5OPEN H5T_NATIVE_INT64_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_UINT64		(H5OPEN H5T_NATIVE_UINT64_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_INT_LEAST64		(H5OPEN H5T_NATIVE_INT_LEAST64_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_UINT_LEAST64 	(H5OPEN H5T_NATIVE_UINT_LEAST64_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_INT_FAST64		(H5OPEN H5T_NATIVE_INT_FAST64_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_UINT_FAST64		(H5OPEN H5T_NATIVE_UINT_FAST64_g)
H5_DLLVAR hid_t H5T_NATIVE_INT64_g;
H5_DLLVAR hid_t H5T_NATIVE_UINT64_g;
H5_DLLVAR hid_t H5T_NATIVE_INT_LEAST64_g;
H5_DLLVAR hid_t H5T_NATIVE_UINT_LEAST64_g;
H5_DLLVAR hid_t H5T_NATIVE_INT_FAST64_g;
H5_DLLVAR hid_t H5T_NATIVE_UINT_FAST64_g;

/* Operations defined on all datatypes */

/**
 * --------------------------------------------------------------------------
 * \ingroup GTO
 *
 * \brief Creates a new datatype.
 *
 * \param[in] class Class of datatype to create
 * \param[in] size  Size, in bytes, of the datatype being created
 *
 * \return hid_t{datatype}
 *
 * \details H5Tcreate() creates a new datatype of the specified class with the
 *          specified number of bytes. This function is used only with the
 *          following datatype classes:
 *          - #H5T_COMPOUND
 *          - #H5T_OPAQUE
 *          - #H5T_ENUM
 *          - #H5T_STRING
 *
 *          Other datatypes, including integer and floating-point datatypes,
 *          are typically created by using H5Tcopy() to copy and modify a
 *          predefined datatype.
 *
 *          When creating a variable-length string datatype, \p size must
 *          be #H5T_VARIABLE.
 *
 *          When creating a fixed-length string datatype, \p size will
 *          be the length of the string in bytes. The length of the
 *          string in characters will depend on i the encoding used; see
 *          H5Pset_char_encoding().
 *
 *          ENUMs created with this function have a signed native integer
 *          base datatype.  Use H5Tenum_create() if a different integer base
 *          datatype is required.
 *
 *          The datatype identifier returned from this function should be
 *          released with H5Tclose or resource leaks will result.
 *
 * \since 1.2.0
 *
 * \see H5Tclose()
 *
 * \todo Original has a reference to “Creating variable-length string
 *       datatypes”.
 * \todo Create an example for H5Tcreate.
 *
 */
H5_DLL hid_t H5Tcreate(H5T_class_t class, size_t size);
/**
 * --------------------------------------------------------------------------
 * \ingroup GTO
 *
 * \brief Copies an existing datatype.
 *
 * \tpd_id
 *
 * \return \hid_t{datatype}
 *
 * \details H5Tcopy() makes a copy of an existing datatype. The returned type
 *          is always transient and unlocked.
 *
 *          The \p obj_id argument can be either a datatype identifier,
 *          a predefined datatype (defined in H5Tpublic.h), or a dataset
 *          identifier.  If \p obj_id is a dataset identifier, this function
 *          returns a transient, modifiable datatype which is a copy of the
 *          dataset's datatype.
 *
 *          The returned datatype identifier should be released with H5Tclose()
 *          to prevent resource leak.
 *
 * \since 1.2.0
 *
 * \see H5Tclose()
 *
 * \todo Create an example for H5Tcopy().
 *
 */
H5_DLL hid_t H5Tcopy(hid_t obj_id);

H5_DLL herr_t H5Tclose(hid_t type_id);
H5_DLL htri_t H5Tequal(hid_t type1_id, hid_t type2_id);
H5_DLL herr_t H5Tlock(hid_t type_id);
H5_DLL herr_t H5Tcommit2(hid_t loc_id, const char *name, hid_t type_id,
    hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id);
H5_DLL hid_t H5Topen2(hid_t loc_id, const char *name, hid_t tapl_id);
H5_DLL herr_t H5Tcommit_anon(hid_t loc_id, hid_t type_id, hid_t tcpl_id, hid_t tapl_id);
H5_DLL hid_t H5Tget_create_plist(hid_t type_id);
H5_DLL htri_t H5Tcommitted(hid_t type_id);
H5_DLL herr_t H5Tencode(hid_t obj_id, void *buf, size_t *nalloc);
H5_DLL hid_t H5Tdecode(const void *buf);
H5_DLL herr_t H5Tflush(hid_t type_id);
H5_DLL herr_t H5Trefresh(hid_t type_id);

/* Operations defined on compound datatypes */

/**
 * --------------------------------------------------------------------------
 * \ingroup COMPOUND
 *
 * \brief Adds a new member to a compound datatype.
 *
 * \dtype_id
 * \param[in] name      Name of the field to insert
 * \param[in] offset    Offset in memory structure of the field to insert
 * \param[in] field_id  Datatype identifier of the field to insert
 *
 * \return \herr_t
 *
 * \details H5Tinsert() adds another member to the compound datatype, specified
 *          \p dtype_id.
 *
 *          The new member has a \p name which must be unique within the
 *          compound datatype. The \p offset argument defines the start of the
 *          member in an instance of the compound datatype, and \p field_id
 *          is the datatype identifier of the new member.
 *
 *          \note Members of a compound datatype do not have to be atomic
 *          datatypes; a compound datatype can have a member which is a
 *          compound datatype.
 *
 * \since 1.2.0
 *
 * \see H5Tcreate()
 *
 * \todo Create example for  H5Tinsert
 *
 */
H5_DLL herr_t H5Tinsert(hid_t dtype_id, const char *name, size_t offset,
			 hid_t field_id);
H5_DLL herr_t H5Tpack(hid_t type_id);

/* Operations defined on enumeration datatypes */
H5_DLL hid_t H5Tenum_create(hid_t base_id);
H5_DLL herr_t H5Tenum_insert(hid_t type, const char *name, const void *value);
H5_DLL herr_t H5Tenum_nameof(hid_t type, const void *value, char *name/*out*/,
			     size_t size);
H5_DLL herr_t H5Tenum_valueof(hid_t type, const char *name,
			      void *value/*out*/);

/* Operations defined on variable-length datatypes */
/**
 * --------------------------------------------------------------------------
 * \ingroup VLEN
 *
 * \brief Creates a new variable-length array datatype
 *
 * \type_id{base_id}, the element type of the datatype to create
 *
 * \return \hid_t{datatype}
 *
 * \details H5Tvlen_create() creates a new one-dimensional array datatype of
 *          variable-length (VL) with the base datatype \p base_id.
 *
 *          This one-dimensional array often represents a data sequence of the
 *          base datatype, such as characters for character sequences or vertex
 *          coordinates for polygon lists. The base type specified for the VL
 *          datatype can be any HDF5 datatype, including another VL datatype, a
 *          compound datatype, or an atomic datatype.
 *
 *          When necessary, use H5Tget_super() to determine the base type of
 *          the VL datatype.
 *
 *          The datatype identifier returned from this function should be
 *          released with H5Tclose() or resource leaks will result. Under
 *          certain circumstances, H5Dvlen_reclaim() must also be used.
 *
 * \attention H5Tvlen_create() cannot be used to create a variable-length
 *            string datatype. H5Tvlen_create() called with a string or
 *            character base type creates a variable-length sequence of strings
 *            (a variable-length, 1-dimensional array), with each element of
 *            the array being of the string or character base type.\n
 *            To create a variable-length string datatype, see "Creating
 *            variable-length string datatypes."
 *
 * \todo Fix the reference.
 *
 */
H5_DLL hid_t H5Tvlen_create(hid_t base_id);

/* Operations defined on array datatypes */
H5_DLL hid_t H5Tarray_create2(hid_t base_id, unsigned ndims,
            const hsize_t dim[/* ndims */]);
H5_DLL int H5Tget_array_ndims(hid_t type_id);
H5_DLL int H5Tget_array_dims2(hid_t type_id, hsize_t dims[]);

/* Operations defined on opaque datatypes */
/**
 * --------------------------------------------------------------------------
 * \ingroup OPAQUE
 *
 * \brief Tags an opaque datatype
 *
 * \type_id{type} of an opaque datatype
 * \param[in] tag Descriptive ASCII string with which the opaque datatype is
 *                to be tagged
 *
 * \return \herr_t
 *
 * \details H5Tset_tag() tags an opaque datatype \p type with a descriptive
 *          ASCII identifier, \p tag.
 *
 *          \p tag is intended to provide a concise description; the maximum
 *          size is hard-coded in the HDF5 library as 256 bytes
 *          (#H5T_OPAQUE_TAG_MAX).
 *
 * \version 1.6.5 The #H5T_OPAQUE_TAG_MAX macro constant, specifying the
 *                maximum size of an opaque datatype tag, was added in
 *                H5Tpublic.h.
 *
 */
H5_DLL herr_t H5Tset_tag(hid_t type, const char *tag);
/**
 * --------------------------------------------------------------------------
 * \ingroup OPAQUE
 *
 * \brief Gets the tag associated with an opaque datatype
 *
 * \type_id{type} of an opaque datatype
 *
 * \return Returns a pointer to an allocated string if successful; otherwise
 *         returns NULL.
 *
 * \details H5Tget_tag() returns the tag associated with the opaque datatype
 *         \p type.
 *
 * \attention The tag is returned via a pointer to an allocated string, which
 *            the caller must free.
 *
 */
H5_DLL char *H5Tget_tag(hid_t type);

/* Querying property values */
H5_DLL hid_t H5Tget_super(hid_t type);
H5_DLL H5T_class_t H5Tget_class(hid_t type_id);
H5_DLL htri_t H5Tdetect_class(hid_t type_id, H5T_class_t cls);
/**
 * --------------------------------------------------------------------------
 * \ingroup GTO
 *
 * \brief Returns the size of a datatype
 *
 * \type_id
 *
 * \return Returns the size of the datatype in bytes if successful; otherwise,
 *         returns 0.
 *
 * \details H5Tget_size() returns the size of a datatype in bytes.
 *
 *          For atomic datatypes, array datatypes, compound datatypes, and
 *          other datatypes of a constant size, the returned value is the
 *          size of the actual datatype in bytes.
 *
 *          For variable-length string datatypes the returned value is
 *          the size of the pointer to the actual string, or \c sizeof(\c
 *          char \c *). This function does not return the size of actual
 *          variable-length string data.
 *
 *          For variable-length sequence datatypes (see H5Tvlen_create()),
 *          the returned value is the size of the \p hvl_t struct, or \c
 *          sizeof(\p hvl_t). The \p hvl_t struct contains a pointer to the
 *          actual data and a size value.  This function does not return the
 *          size of actual variable-length sequence data.
 *
 * \since 1.2.0
 *
 * \see H5Tset_size()
 *
 *\todo Original has a reference to “Creating variable-length string datatypes”.
 *\todo Create an example for H5Tget_size.
 *
 */
H5_DLL size_t H5Tget_size(hid_t type_id);
H5_DLL H5T_order_t H5Tget_order(hid_t type_id);
H5_DLL size_t H5Tget_precision(hid_t type_id);
H5_DLL int H5Tget_offset(hid_t type_id);
H5_DLL herr_t H5Tget_pad(hid_t type_id, H5T_pad_t *lsb/*out*/,
			  H5T_pad_t *msb/*out*/);
H5_DLL H5T_sign_t H5Tget_sign(hid_t type_id);
H5_DLL herr_t H5Tget_fields(hid_t type_id, size_t *spos/*out*/,
			     size_t *epos/*out*/, size_t *esize/*out*/,
			     size_t *mpos/*out*/, size_t *msize/*out*/);
H5_DLL size_t H5Tget_ebias(hid_t type_id);
H5_DLL H5T_norm_t H5Tget_norm(hid_t type_id);
H5_DLL H5T_pad_t H5Tget_inpad(hid_t type_id);
H5_DLL H5T_str_t H5Tget_strpad(hid_t type_id);
H5_DLL int H5Tget_nmembers(hid_t type_id);
H5_DLL char *H5Tget_member_name(hid_t type_id, unsigned membno);
H5_DLL int H5Tget_member_index(hid_t type_id, const char *name);
H5_DLL size_t H5Tget_member_offset(hid_t type_id, unsigned membno);
H5_DLL H5T_class_t H5Tget_member_class(hid_t type_id, unsigned membno);
H5_DLL hid_t H5Tget_member_type(hid_t type_id, unsigned membno);
H5_DLL herr_t H5Tget_member_value(hid_t type_id, unsigned membno, void *value/*out*/);
H5_DLL H5T_cset_t H5Tget_cset(hid_t type_id);
H5_DLL htri_t H5Tis_variable_str(hid_t type_id);
H5_DLL hid_t H5Tget_native_type(hid_t type_id, H5T_direction_t direction);

/* Setting property values */
/**
 * --------------------------------------------------------------------------
 * \ingroup GTO
 *
 * \brief Sets size for a datatype.
 *
 * \type_id for which the size is being set
 * \param[in] size New datatype size is bytes or #H5T_VARIABLE
 *
 * \return \herr_t
 *
 * \details H5Tset_size() sets the total size, \p size, in bytes, for a
 *          datatype.
 *
 * The parameter \p size must have a positive value, unless it is passed as
 * #H5T_VARIABLE and the datatype is a string datatype.
 *
 *          \li Numeric datatypes: If the datatype is atomic and the size
 *          is decreased so that significant bits of the datatype extend
 *          beyond the edge of the new size, then the offset property of the
 *          datatype is decreased toward zero.  If the offset becomes zero
 *          and the significant bits of the datatype still hang over the edge
 *          of the new size, then the number of significant bits is decreased.
 *
 *          \li String or character datatypes: The size set for a string
 *          datatype should include space for the null-terminator character,
 *          otherwise it will not be stored on (or retrieved from)
 *          disk. Adjusting the size of a string automatically sets the
 *          precision to \p 8*size.
 *
 *          \li Variable-length string datatypes: If \p dtype_id is a
 *          variable-length string, size must normally be set to #H5T_VARIABLE.
 *
 *          \li Compound datatypes: This function may be used to increase or
 *          decrease the size of a compound datatype, but the function will
 *          fail if the new size is too small to accommodate all member fields.
 *
 *          \li Ineligible datatypes: This function cannot be used with
 *          enumerated datatypes (#H5T_ENUM), array datatypes (#H5T_ARRAY),
 *          variable-length array datatypes (#H5T_VLEN), or reference datatypes
 *          (#H5T_REFERENCE).
 *
 * \since 1.2.0
 *
 * \see H5Tget_size()
 *
 *\todo Create an example for H5Tset_size().
 *\todo Original has a reference to “Creating variable-length string datatypes”.
 *
 */
H5_DLL herr_t H5Tset_size(hid_t type_id, size_t size);
H5_DLL herr_t H5Tset_order(hid_t type_id, H5T_order_t order);
H5_DLL herr_t H5Tset_precision(hid_t type_id, size_t prec);
H5_DLL herr_t H5Tset_offset(hid_t type_id, size_t offset);
H5_DLL herr_t H5Tset_pad(hid_t type_id, H5T_pad_t lsb, H5T_pad_t msb);
H5_DLL herr_t H5Tset_sign(hid_t type_id, H5T_sign_t sign);
H5_DLL herr_t H5Tset_fields(hid_t type_id, size_t spos, size_t epos,
			     size_t esize, size_t mpos, size_t msize);
H5_DLL herr_t H5Tset_ebias(hid_t type_id, size_t ebias);
H5_DLL herr_t H5Tset_norm(hid_t type_id, H5T_norm_t norm);
H5_DLL herr_t H5Tset_inpad(hid_t type_id, H5T_pad_t pad);
H5_DLL herr_t H5Tset_cset(hid_t type_id, H5T_cset_t cset);
H5_DLL herr_t H5Tset_strpad(hid_t type_id, H5T_str_t strpad);

/* Type conversion database */
/**
 * --------------------------------------------------------------------------
 * \ingroup CONV
 *
 * \brief Registers a datatype conversion function
 *
 * \param[in] pers Conversion function type
 * \param[in] name Name displayed in diagnostic output
 * \type_id{src_id} of source datatype
 * \type_id{dst_id} of destination datatype
 * \param[in] func Function to convert between source and destination datatypes
 *
 * \return \herr_t
 *
 * \details H5Tregister() registers a hard or soft conversion function for a
 *          datatype conversion path. The parameter \p pers indicates whether a
 *          conversion function is hard (#H5T_PERS_HARD) or soft
 *          (#H5T_PERS_SOFT). User-defined functions employing compiler casting
 *          are designated as \Emph{hard}; other user-defined conversion
 *          functions registered with the HDF5 library (with H5Tregister() )
 *          are designated as \Emph{soft}. The HDF5 library also has its own
 *          hard and soft conversion functions.
 *
 *          A conversion path can have only one hard function. When type is
 *          #H5T_PERS_HARD, \p func replaces any previous hard function.
 *
 *          When type is #H5T_PERS_SOFT, H5Tregister() adds the function to the
 *          end of the master soft list and replaces the soft function in all
 *          applicable existing conversion paths. Soft functions are used when
 *          determining which conversion function is appropriate for this path.
 *
 *          The \p name is used only for debugging and should be a short
 *          identifier for the function.
 *
 *          The path is specified by the source and destination datatypes \p
 *          src_id and \p dst_id. For soft conversion functions, only the class
 *          of these types is important.
 *
 *          The type of the conversion function pointer is declared as:
 *          \snippet this H5T_conv_t_snip
 *
 *          The \ref H5T_cdata_t \c struct is declared as:
 *          \snippet this H5T_cdata_t_snip
 *
 * \since 1.6.3 The following change occurred in the \ref H5T_conv_t function:
 *              the \c nelmts parameter type changed to size_t.
 *
 */
H5_DLL herr_t H5Tregister(H5T_pers_t pers, const char *name, hid_t src_id,
			   hid_t dst_id, H5T_conv_t func);
/**
 * --------------------------------------------------------------------------
 * \ingroup CONV
 *
 * \brief Removes a conversion function
 *
 * \param[in] pers Conversion function type
 * \param[in] name Name displayed in diagnostic output
 * \type_id{src_id} of source datatype
 * \type_id{dst_id} of destination datatype
 * \param[in] func Function to convert between source and destination datatypes
 *
 * \return \herr_t
 *
 * \details H5Tunregister() removes a conversion function matching criteria
 *          such as soft or hard conversion, source and destination types, and
 *          the conversion function.
 *
 *          If a user is trying to remove a conversion function he registered,
 *          all parameters can be used. If he is trying to remove a library’s
 *          default conversion function, there is no guarantee the \p name and
 *          \p func parameters will match the user’s chosen values. Passing in
 *          some values may cause this function to fail. A good practice is to
 *          pass in NULL as their values.
 *
 *          All parameters are optional. The missing parameters will be used to
 *          generalize the search criteria.
 *
 *          The conversion function pointer type declaration is described in
 *          H5Tregister().
 *
 * \since 1.6.3 The following change occurred in the \ref H5T_conv_t function:
 *              the \c nelmts parameter type changed to size_t.
 *
 */
H5_DLL herr_t H5Tunregister(H5T_pers_t pers, const char *name, hid_t src_id,
			     hid_t dst_id, H5T_conv_t func);
/**
 * --------------------------------------------------------------------------
 * \ingroup CONV
 *
 * \brief Finds a conversion function
 *
 * \type_id{src_id} of source datatype
 * \type_id{dst_id} of destination datatype
 * \param[out] pcdata Pointer to type conversion data
 *
 * \return Returns a pointer to a suitable conversion function if successful.
 *         Otherwise returns NULL.
 *
 * \details H5Tfind() finds a conversion function that can handle a conversion
 *          from type \p src_id to type \p dst_id. The \p pcdata argument is a
 *          pointer to a pointer to type conversion data which was created and
 *          initialized by the soft type conversion function of this path when
 *          the conversion function was installed on the path.
 *
 */
H5_DLL H5T_conv_t H5Tfind(hid_t src_id, hid_t dst_id, H5T_cdata_t **pcdata);
/**
 * --------------------------------------------------------------------------
 * \ingroup CONV
 *
 * \brief Check whether the library’s default conversion is hard conversion
 *
 * \type_id{src_id} of source datatype
 * \type_id{dst_id} of destination datatype
 *
 * \return \htri_t
 *
 * \details H5Tcompiler_conv() determines whether the library’s conversion
 *          function from type \p src_id to type \p dst_id is a compiler (hard)
 *          conversion or not. A compiler conversion uses compiler’s casting; a
 *          library (soft) conversion uses the library’s own conversion
 *          function.
 *
 * \since 1.8.0
 *
 */
H5_DLL htri_t H5Tcompiler_conv(hid_t src_id, hid_t dst_id);
/**
 * --------------------------------------------------------------------------
 * \ingroup CONV
 *
 * \brief Converts data from one specified datatype to another
 *
 * \type_id{src_id} of source datatype
 * \type_id{dst_id} of destination datatype
 * \param[in] nelmts Size of array \p buf
 * \param[in,out] buf Array containing pre- and post-conversion values
 * \param[in] background Optional background buffer
 * \dxpl_id{plist_id}
 *
 * \return \herr_t
 *
 * \note H5Tconvert() will not resize the buffer \p buf; it must be large
 *       enough to hold the larger of the input and output data.
 *
 * \details H5Tconvert() converts \p nelmts elements from a source datatype,
 *          specified by \p src_id, to a destination datatype, \p dst_id. The
 *          source elements are packed in \p buf and on return the destination
 *          elements will be packed in \p buf. That is, the conversion is
 *          performed in place.
 *
 *          The optional background buffer is for use with compound datatypes.
 *          It is an array of \p nelmts values for the destination datatype
 *          which can then be merged with the converted values to recreate the
 *          compound datatype. For instance, background might be an array of
 *          structs with the \c a and \c b fields already initialized and the
 *          conversion of buf supplies the \c c and \c d field values.
 *
 *          The parameter plist_id contains the dataset transfer property list
 *          identifier which is passed to the conversion functions. As of
 *          Release 1.2, this parameter is only used to pass along the
 *          variable-length datatype custom allocation information.
 *
 * \version 1.6.3 \p nelmts parameter type changed to size_t.
 * \version 1.4.0 \p nelmts parameter type changed to \ref hsize_t.
 *
 */
H5_DLL herr_t H5Tconvert(hid_t src_id, hid_t dst_id, size_t nelmts,
			  void *buf, void *background, hid_t plist_id);
/**
 * --------------------------------------------------------------------------
 * \ingroup VLEN
 *
 * \brief Reclaims the variable length (VL) datatype memory buffers
 *
 * \type_id
 * \space_id
 * \dxpl_id{plist_id} used to create the buffer
 * \param[in] buf Pointer to the buffer to be reclaimed
 *
 * \return \herr_t
 *
 * \details H5Treclaim() reclaims memory buffers created to store VL datatypes.
 *          It only frees the variable length data in the selection defined in
 *          the dataspace specified by \p space_id. The dataset transfer
 *          property list \p plist_id is required to find the correct
 *          allocation and/or free methods for the variable-length data in the
 *          buffer.
 *
 * \since 1.12.0
 *
 */
H5_DLL herr_t H5Treclaim(hid_t type_id, hid_t space_id, hid_t plist_id, void *buf);

/* Symbols defined for compatibility with previous versions of the HDF5 API.
 *
 * Use of these symbols is deprecated.
 */
#ifndef H5_NO_DEPRECATED_SYMBOLS

/* Macros */


/* Typedefs */


/* Function prototypes */
H5_DLL herr_t H5Tcommit1(hid_t loc_id, const char *name, hid_t type_id);
H5_DLL hid_t H5Topen1(hid_t loc_id, const char *name);
H5_DLL hid_t H5Tarray_create1(hid_t base_id, int ndims,
            const hsize_t dim[/* ndims */],
            const int perm[/* ndims */]);
H5_DLL int H5Tget_array_dims1(hid_t type_id, hsize_t dims[], int perm[]);

#endif /* H5_NO_DEPRECATED_SYMBOLS */

#ifdef __cplusplus
}
#endif
#endif /* _H5Tpublic_H */
