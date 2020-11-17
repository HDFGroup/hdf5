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

#define HOFFSET(S, M) (offsetof(S, M))

/**
 * These are the various classes of datatypes
 * internal If this goes over 16 types (0-15), the file format will need to
 *          change.
 */
//! [H5T_class_t_snip]
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
    H5T_ENUM             = 8,	/**< enumeration types                       */
    H5T_VLEN             = 9,   /**< variable-Length types                   */
    H5T_ARRAY	         = 10,	/**< array types                             */

    H5T_NCLASSES                /**< sentinel: this must be last             */
} H5T_class_t;
//! [H5T_class_t_snip]

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

    H5T_NSGN             = 2    /** sentinel: this must be last!             */
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
//! [H5T_direction_t_snip]
typedef enum H5T_direction_t {
    H5T_DIR_DEFAULT     = 0,    /**< default direction is inscendent        */
    H5T_DIR_ASCEND      = 1,    /**< in inscendent order                    */
    H5T_DIR_DESCEND     = 2     /**< in descendent order                    */
} H5T_direction_t;
//! [H5T_direction_t_snip]

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
#define H5OPEN H5open(),
#else /* _H5private_H */
#define H5OPEN
#endif /* _H5private_H */

/*
 * The IEEE floating point types in various byte orders.
 */
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

/*
 * These are "standard" types.  For instance, signed (2's complement) and
 * unsigned integers of various sizes and byte orders.
 */
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

/*
 * Types which are particular to Unix.
 */
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

/*
 * Types particular to the C language.  String types use `bytes' instead
 * of `bits' as their size.
 */
/**
 * \ingroup PDTS
 * String datatype in C (size defined in bytes rather than in bits)
 */
#define H5T_C_S1 (H5OPEN H5T_C_S1_g)
H5_DLLVAR hid_t H5T_C_S1_g;

/*
 * Types particular to Fortran.
 */
/**
 * \ingroup PDTS
 * String datatype in Fortran (as defined for the HDF5 C library)
 */
#define H5T_FORTRAN_S1 (H5OPEN H5T_FORTRAN_S1_g)
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
#define H5T_VAX_F32 (H5OPEN H5T_VAX_F32_g)
/**
 * \ingroup PDTALPHA
 * 64-bit VAX byte order floating-point numbers for OpenVMS on DEC Alpha CPUs
 */
#define H5T_VAX_F64 (H5OPEN H5T_VAX_F64_g)
H5_DLLVAR hid_t H5T_VAX_F32_g;
H5_DLLVAR hid_t H5T_VAX_F64_g;

/*
 * The predefined native types. These are the types detected by H5detect and
 * they violate the naming scheme a little.  Instead of a class name,
 * precision and byte order as the last component, they have a C-like type
 * name.  If the type begins with `U' then it is the unsigned version of the
 * integer type; other integer types are signed.  The type LLONG corresponds
 * to C's `long long' and LDOUBLE is `long double' (these types might be the
 * same as `LONG' and `DOUBLE' respectively).
 */
/**
 * \ingroup PDTNAT
 * C-style \c char
 */
#define H5T_NATIVE_CHAR	  (CHAR_MIN?H5T_NATIVE_SCHAR:H5T_NATIVE_UCHAR)
/**
 * \ingroup PDTNAT
 * C-style \Code{signed char}
 */
#define H5T_NATIVE_SCHAR  (H5OPEN H5T_NATIVE_SCHAR_g)
/**
 * \ingroup PDTNAT
 * C-style \Code{unsigned char}
 */
#define H5T_NATIVE_UCHAR  (H5OPEN H5T_NATIVE_UCHAR_g)
/**
 * \ingroup PDTNAT
 * C-style \Code{short}
 */
#define H5T_NATIVE_SHORT  (H5OPEN H5T_NATIVE_SHORT_g)
/**
 * \ingroup PDTNAT
 * C-style \Code{unsigned short}
 */
#define H5T_NATIVE_USHORT (H5OPEN H5T_NATIVE_USHORT_g)
/**
 * \ingroup PDTNAT
 * C-style \Code{int}
 */
#define H5T_NATIVE_INT    (H5OPEN H5T_NATIVE_INT_g)
/**
 * \ingroup PDTNAT
 * C-style \Code{unsigned int}
 */
#define H5T_NATIVE_UINT   (H5OPEN H5T_NATIVE_UINT_g)
/**
 * \ingroup PDTNAT
 * C-style \Code{long}
 */
#define H5T_NATIVE_LONG   (H5OPEN H5T_NATIVE_LONG_g)
/**
 * \ingroup PDTNAT
 * C-style \Code{unsigned long}
 */
#define H5T_NATIVE_ULONG  (H5OPEN H5T_NATIVE_ULONG_g)
/**
 * \ingroup PDTNAT
 * C-style \Code{long long}
 */
#define H5T_NATIVE_LLONG  (H5OPEN H5T_NATIVE_LLONG_g)
/**
 * \ingroup PDTNAT
 * C-style \Code{unsigned long long}
 */
#define H5T_NATIVE_ULLONG (H5OPEN H5T_NATIVE_ULLONG_g)
/**
 * \ingroup PDTNAT
 * C-style \Code{float}
 */
#define H5T_NATIVE_FLOAT  (H5OPEN H5T_NATIVE_FLOAT_g)
/**
 * \ingroup PDTNAT
 * C-style \Code{double}
 */
#define H5T_NATIVE_DOUBLE (H5OPEN H5T_NATIVE_DOUBLE_g)
#if H5_SIZEOF_LONG_DOUBLE !=0
/**
 * \ingroup PDTNAT
 * C-style \Code{long double}
 */
#define H5T_NATIVE_LDOUBLE (H5OPEN H5T_NATIVE_LDOUBLE_g)
#endif
/**
 * \ingroup PDTNAT
 * HDF5 8-bit bitfield based on native types
 */
#define H5T_NATIVE_B8     (H5OPEN H5T_NATIVE_B8_g)
/**
 * \ingroup PDTNAT
 * HDF5 16-bit bitfield based on native types
 */
#define H5T_NATIVE_B16    (H5OPEN H5T_NATIVE_B16_g)
/**
 * \ingroup PDTNAT
 * HDF5 32-bit bitfield based on native types
 */
#define H5T_NATIVE_B32    (H5OPEN H5T_NATIVE_B32_g)
/**
 * \ingroup PDTNAT
 * HDF5 64-bit bitfield based on native types
 */
#define H5T_NATIVE_B64	  (H5OPEN H5T_NATIVE_B64_g)
/**
 * \ingroup PDTNAT
 * HDF5 opaque unit based on native types
 */
#define H5T_NATIVE_OPAQUE (H5OPEN H5T_NATIVE_OPAQUE_g)
/**
 * \ingroup PDTNAT
 * HDF5 address type based on native types
 */
#define H5T_NATIVE_HADDR  (H5OPEN H5T_NATIVE_HADDR_g)
/**
 * \ingroup PDTNAT
 * HDF5 size type based on native types
 */
#define H5T_NATIVE_HSIZE  (H5OPEN H5T_NATIVE_HSIZE_g)
/**
 * \ingroup PDTNAT
 * HDF5 signed size type based on native types
 */
#define H5T_NATIVE_HSSIZE (H5OPEN H5T_NATIVE_HSSIZE_g)
/**
 * \ingroup PDTNAT
 * HDF5 error code type based on native types
 */
#define H5T_NATIVE_HERR	  (H5OPEN H5T_NATIVE_HERR_g)
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
#if H5_SIZEOF_LONG_DOUBLE != 0
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

/* C9x integer types */
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_INT8	       (H5OPEN H5T_NATIVE_INT8_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_UINT8	   (H5OPEN H5T_NATIVE_UINT8_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_INT_LEAST8  (H5OPEN H5T_NATIVE_INT_LEAST8_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_UINT_LEAST8  (H5OPEN H5T_NATIVE_UINT_LEAST8_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_INT_FAST8 	(H5OPEN H5T_NATIVE_INT_FAST8_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_UINT_FAST8	(H5OPEN H5T_NATIVE_UINT_FAST8_g)
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
#define H5T_NATIVE_INT_LEAST16	(H5OPEN H5T_NATIVE_INT_LEAST16_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_UINT_LEAST16	(H5OPEN H5T_NATIVE_UINT_LEAST16_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_INT_FAST16	(H5OPEN H5T_NATIVE_INT_FAST16_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_UINT_FAST16	(H5OPEN H5T_NATIVE_UINT_FAST16_g)
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
#define H5T_NATIVE_INT_LEAST32	(H5OPEN H5T_NATIVE_INT_LEAST32_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_UINT_LEAST32	(H5OPEN H5T_NATIVE_UINT_LEAST32_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_INT_FAST32	(H5OPEN H5T_NATIVE_INT_FAST32_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_UINT_FAST32	(H5OPEN H5T_NATIVE_UINT_FAST32_g)
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
#define H5T_NATIVE_INT_LEAST64	(H5OPEN H5T_NATIVE_INT_LEAST64_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_UINT_LEAST64 (H5OPEN H5T_NATIVE_UINT_LEAST64_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_INT_FAST64	(H5OPEN H5T_NATIVE_INT_FAST64_g)
/**
 * \ingroup PDTC9x
 */
#define H5T_NATIVE_UINT_FAST64	(H5OPEN H5T_NATIVE_UINT_FAST64_g)
H5_DLLVAR hid_t H5T_NATIVE_INT64_g;
H5_DLLVAR hid_t H5T_NATIVE_UINT64_g;
H5_DLLVAR hid_t H5T_NATIVE_INT_LEAST64_g;
H5_DLLVAR hid_t H5T_NATIVE_UINT_LEAST64_g;
H5_DLLVAR hid_t H5T_NATIVE_INT_FAST64_g;
H5_DLLVAR hid_t H5T_NATIVE_UINT_FAST64_g;

/* Operations defined on all datatypes */
/**
 * \ingroup H5T
 *
 * \brief Creates a new datatype.
 *
 * \param[in] type Class of datatype to create
 * \param[in] size  Size, in bytes, of the datatype being created
 *
 * \return \hid_t{datatype}
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
H5_DLL hid_t  H5Tcreate(H5T_class_t type, size_t size);
/**
 * \ingroup H5T
 *
 * \brief Copies an existing datatype.
 *
 * \type_id
 *
 * \return \hid_t{datatype}
 *
 * \details H5Tcopy() makes a copy of an existing datatype. The returned type
 *          is always transient and unlocked.
 *
 *          The \p type_id argument can be either a datatype identifier,
 *          a predefined datatype (defined in H5Tpublic.h), or a dataset
 *          identifier.  If \p type_id is a dataset identifier, this function
 *          returns a transient, modifiable datatype which is a copy of the
 *          dataset's datatype.
 *
 *          The returned datatype identifier should be released with H5Tclose()
 *          to prevent resource leak.
 *
 * \todo Create an example for H5Tcopy().
 *
 */
H5_DLL hid_t  H5Tcopy(hid_t type_id);
/**
 * \ingroup H5T
 *
 * \brief Releases a datatype
 *
 * \type_id
 *
 * \return \herr_t
 *
 * \details H5Tclose() releases the datatype \p dtype_id. Further access
 *          through this datatype identifier is illegal. Failure to release
 *          a datatype with this call will result in resource leaks.
 *
 */
H5_DLL herr_t H5Tclose(hid_t type_id);
/**
 * \ingroup H5T
 *
 * \brief Determines whether two datatype identifiers refer to the same datatype
 *
 * \type_id{type1_id}
 * \type_id{type2_id}
 *
 * \return \htri_t
 *
 * \details H5Tequal() determines whether two datatype identifiers refer to
 *          the same datatype.
 *
 * \since 1.6 or earlier
 *
 */
H5_DLL htri_t H5Tequal(hid_t type1_id, hid_t type2_id);
H5_DLL herr_t H5Tlock(hid_t type_id);
/**
 * \ingroup H5T
 *
 * \brief Commits a transient datatype, linking it into the file and creating
 *        a new committed datatype
 *
 * \fg_loc_id
 * \param[in] name Name given to committed datatype
 * \type_id Identifier of datatype to be committed and, upon function’s
 *          return, identifier for the committed datatype
 * \lcpl_id
 * \tcpl_id
 * \tapl_id
 *
 * \return \herr_t
 *
 * \details H5Tcommit2() saves a transient datatype as an immutable committed
 *          datatype in a file. The datatype specified by \p dtype_id is
 *          committed to the file with the name name at the location specified
 *          by \p loc_id and with the datatype creation and access property
 *          lists \p tcpl_id and \p tapl_id, respectively.
 *
 *          \p loc_id may be a file identifier, or a group identifier within
 *          that file. \p name may be either an absolute path in the file or
 *          a relative path from \p loc_id naming the newly-commited datatype.
 *
 *          The link creation property list, \p lcpl_id, governs creation of
 *          the link(s) by which the new committed datatype is accessed and
 *          the creation of any intermediate groups that may be missing.
 *
 *          Once commited, this datatype may be used to define the datatype
 *          of any other dataset or attribute in the file.
 *
 *          This function will not accept a datatype that cannot actually hold
 *          information. This currently includes compound datatypes with no
 *          fields and enumerated datatypes with no members.
 *
 *          Committed datatypes are sometimes referred to as named datatypes.
 *
 * \version 1.8.7 Function modified in this release to reject datatypes that
 *          will not accomodate actual data, such as a compound datatype
 *          with no fields or an enumerated datatype with no members.
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Tcommit2(hid_t loc_id, const char *name, hid_t type_id, hid_t lcpl_id, hid_t tcpl_id,
                         hid_t tapl_id);
H5_DLL hid_t  H5Topen2(hid_t loc_id, const char *name, hid_t tapl_id);
/**
 * \ingroup H5T
 *
 * \brief Commits a transient datatype to a file, creating a new named
 *        datatype, but does not link it into the file structure
 *
 * \fg_loc_id
 * \type_id
 * \tcpl_id
 * \tapl_id
 *
 * \return \herr_t
 *
 * \details H5Tcommit_anon() commits a transient datatype (not immutable)
 *          to a file, turning it into a named datatype with the specified
 *          creation and property lists. With default property lists,
 *          #H5P_DEFAULT, H5Tcommit_anon() provides similar functionality to
 *          that of H5Tcommit(), with the differences described below.
 *
 *          #H5P_DEFAULT can be passed in for the datatype creation property
 *          list identifier, \p tcpl_id.  The datatype access property list
 *          identifier, \p tapl_id, is provided for future functionality and
 *          is not used at this time. This parameter should always be passed
 *          as the value #H5P_DEFAULT.
 *
 *          Note that H5Tcommit_anon() does not link this newly-committed
 *          datatype into the file. After the H5Tcommit_anon() call, the
 *          datatype identifier \p type_id must be linked into the HDF5 file
 *          structure with H5Olink() or it will be deleted from the file when
 *          the file is closed.
 *
 *          The differences between this function and H5Tcommit() are as follows:
 *          \li H5Tcommit_anon() explicitly includes property lists,
 *              which provides for greater control of the creation process
 *              and of the properties of the new named datatype. H5Tcommit()
 *              always uses default properties.
 *          \li H5Tcommit_anon() neither provides the new named datatype’s
 *              name nor links it into the HDF5 file structure; those actions
 *              must be performed separately through a call to H5Olink(),
 *              which offers greater control over linking.
 *
 *          This function will not accept a datatype that cannot actually
 *          hold data. This currently includes compound datatypes with no
 *          fields and enumerated datatypes with no members.
 *
 * \version 1.8.7 Function modified in this release to reject datatypes that
 *                will not accomodate actual data, such as a compound datatype
 *                with no fields or an enumerated datatype with no members.
 *
 * \since 1.2.0
 *
 */
H5_DLL herr_t H5Tcommit_anon(hid_t loc_id, hid_t type_id, hid_t tcpl_id, hid_t tapl_id);
/**
 * \ingroup H5T
 *
 * \brief Returns a copy of a datatype's creation property list
 *
 * \type_id
 *
 * \return \hid_t{datatype creation property list}
 *
 * \details H5Tget_create_plist() returns a property list identifier
 *          for the datatype creation property list associated with the datatype
 *          specified by \p type_id.
 *
 *          The creation property list identifier should be released with
 *          H5Pclose() to prevent memory leaks.
 *
 * \since 1.8.0
 *
 */
H5_DLL hid_t  H5Tget_create_plist(hid_t type_id);
/**
 * \ingroup H5T
 *
 * \brief Determines whether a datatype is a committed type or a transient type
 *
 * \type_id
 *
 * \return \htri_t
 *
 * \details H5Tcommitted() queries a type to determine whether the type
 *          specified by the \p dtype_id identifier is a committed (formerly
 *          known as a \Emph{named}) type or a transient type. If this function returns
 *          a positive value, then the type is committed (that is, it has been
 *          committed, perhaps by some other application). Datasets which
 *          return committed datatypes with H5Dget_type() are able to share
 *          the datatype with other datasets in the same file.
 *
 * \version 1.8.0 Fortran API was added
 *
 * \since 1.6 or earlier
 *
 */
H5_DLL htri_t H5Tcommitted(hid_t type_id);
H5_DLL herr_t H5Tencode(hid_t obj_id, void *buf, size_t *nalloc);
H5_DLL hid_t  H5Tdecode(const void *buf);
/**
 * \ingroup H5T
 *
 * \brief Flushes all buffers associated with a committed datatype to disk
 *
 * \type_id
 *
 * \return \herr_t
 *
 * \details H5Tflush() causes all buffers associated with a committed datatype
 *          \p type_id to be immediately flushed to disk without removing the
 *          data from the cache.
 *
 *          HDF5 does not possess full control over buffering.  H5Tflush()
 *          flushes the internal HDF5 buffers and then asks the operating
 *          system (the OS) to flush the system buffers for the open
 *          files. After that, the OS is responsible for ensuring that the
 *          data is actually flushed to disk.
 *
 * \return \herr_t
 *
 * \since 1.10.0  C function introduced with this release.
 *
 * \see     H5Dflush()
 *          H5Drefresh()
 *          H5Tflush()
 *          H5Grefresh()
 *          H5Oflush()
 *          H5Orefresh()
 *          H5Tflush()
 *          H5Trefresh()
 *          H5Fstart_swmr_write()
 *          H5Pget_append_flush()
 *          H5Pget_object_flush_cb()
 *          H5Pset_append_flush()
 *          H5Pset_object_flush_cb()
 *
 */
H5_DLL herr_t H5Tflush(hid_t type_id);
H5_DLL herr_t H5Trefresh(hid_t type_id);

/* Operations defined on compound datatypes */
H5_DLL herr_t H5Tinsert(hid_t parent_id, const char *name, size_t offset, hid_t member_id);
H5_DLL herr_t H5Tpack(hid_t type_id);

/* Operations defined on enumeration datatypes */
H5_DLL hid_t  H5Tenum_create(hid_t base_id);
H5_DLL herr_t H5Tenum_insert(hid_t type, const char *name, const void *value);
H5_DLL herr_t H5Tenum_nameof(hid_t type, const void *value, char *name /*out*/, size_t size);
H5_DLL herr_t H5Tenum_valueof(hid_t type, const char *name, void *value /*out*/);

/* Operations defined on variable-length datatypes */
H5_DLL hid_t H5Tvlen_create(hid_t base_id);

/* Operations defined on array datatypes */
H5_DLL hid_t H5Tarray_create2(hid_t base_id, unsigned ndims, const hsize_t dim[/* ndims */]);
H5_DLL int   H5Tget_array_ndims(hid_t type_id);
H5_DLL int   H5Tget_array_dims2(hid_t type_id, hsize_t dims[]);

/* Operations defined on opaque datatypes */
H5_DLL herr_t H5Tset_tag(hid_t type, const char *tag);
H5_DLL char * H5Tget_tag(hid_t type);

/* Querying property values */
/**
 * --------------------------------------------------------------------------
 * \ingroup H5T
 *
 * \brief Returns the base datatype from which a datatype is derived
 *
 * \type_id{type}
 *
 * \return \hid_t{datatype}
 *
 * \details H5Tget_super() returns the base datatype from which the datatype
 *          \p type_id is derived.  In the case of an enumeration type, the
 *          return value is an integer type.
 *
 *          The datatype identifier returned by this function must be released
 *          with H5Tclose()  when the identifier is no longer needed so that
 *          resource leaks will not develop.
 *
 */
H5_DLL hid_t       H5Tget_super(hid_t type);
/**
 * --------------------------------------------------------------------------
 * \ingroup H5T
 *
 * \brief Returns a datatype class
 *
 * \type_id
 *
 * \return Returns the datatype class if successful; otherwise #H5T_NO_CLASS.
 *
 * \details H5Tget_class() returns the class of the datatype \p type_id.
 *          Valid class identifiers, as defined in H5Tpublic.h, are:
 *          \snippet this H5T_class_t_snip
 *
 * \note The library returns #H5T_STRING for both fixed-length and
 *       variable-length strings.
 *
 * \note Unsupported datatype: The time datatype class, #H5T_TIME,
 *       is not supported. If #H5T_TIME is used, the resulting data will
 *       be readable and modifiable only on the originating computing
 *       platform; it will not be portable to other platforms.
 *
 */
H5_DLL H5T_class_t H5Tget_class(hid_t type_id);
/**
 * \ingroup H5T
 *
 * \brief Determines whether a datatype contains any datatypes of the given
 *        datatype class
 *
 * \type_id
 * \param[in] cls Datatype class
 *
 * \return \htri_t
 *
 * \details H5Tdetect_class() determines whether the datatype specified in
 *          \p type_id contains any datatypes of the datatype class specified
 *          in \p dtype_class.
 *
 *          This function is useful primarily in recursively examining all the
 *          fields and/or base types of compound, array, and variable-length
 *          datatypes.
 *
 *          Valid class identifiers, as defined in H5Tpublic.h, are:
 *          \snippet this H5T_class_t_snip
 *
 * \since 1.6.0
 *
 */
H5_DLL htri_t      H5Tdetect_class(hid_t type_id, H5T_class_t cls);
/**
 * \ingroup H5T
 *
 * \brief Returns the size of a datatype
 *
 * \type_id
 *
 * \return Returns the size of the datatype in bytes if successful; otherwise,
 *         returns 0.
 *
 * \details H5Tget_size() returns the size of a datatype in bytes.
 *          \li For atomic datatypes, array datatypes, compound datatypes, and
 *          other datatypes of a constant size, the returned value is the
 *          size of the actual datatype in bytes.
 *          \li For variable-length string datatypes the returned value is
 *          the size of the pointer to the actual string, or \c sizeof(\c
 *          char \c *). This function does not return the size of actual
 *          variable-length string data.
 *          \li For variable-length sequence datatypes (see H5Tvlen_create()),
 *          the returned value is the size of the \p hvl_t struct, or \c
 *          sizeof(\p hvl_t). The \p hvl_t struct contains a pointer to the
 *          actual data and a size value.  This function does not return the
 *          size of actual variable-length sequence data.
 *
 * \since 1.2.0
 *
 * \see H5Tset_size()
 *
 * \todo Original has a reference to “Creating variable-length string datatypes”.
 * \todo Create an example for H5Tget_size().
 *
 */
H5_DLL size_t      H5Tget_size(hid_t type_id);
H5_DLL H5T_order_t H5Tget_order(hid_t type_id);
H5_DLL size_t      H5Tget_precision(hid_t type_id);
H5_DLL int         H5Tget_offset(hid_t type_id);
H5_DLL herr_t      H5Tget_pad(hid_t type_id, H5T_pad_t *lsb /*out*/, H5T_pad_t *msb /*out*/);
H5_DLL H5T_sign_t  H5Tget_sign(hid_t type_id);
H5_DLL herr_t H5Tget_fields(hid_t type_id, size_t *spos /*out*/, size_t *epos /*out*/, size_t *esize /*out*/,
                            size_t *mpos /*out*/, size_t *msize /*out*/);
H5_DLL size_t H5Tget_ebias(hid_t type_id);
H5_DLL H5T_norm_t  H5Tget_norm(hid_t type_id);
H5_DLL H5T_pad_t   H5Tget_inpad(hid_t type_id);
H5_DLL H5T_str_t   H5Tget_strpad(hid_t type_id);
H5_DLL int         H5Tget_nmembers(hid_t type_id);
H5_DLL char *      H5Tget_member_name(hid_t type_id, unsigned membno);
H5_DLL int         H5Tget_member_index(hid_t type_id, const char *name);
H5_DLL size_t      H5Tget_member_offset(hid_t type_id, unsigned membno);
H5_DLL H5T_class_t H5Tget_member_class(hid_t type_id, unsigned membno);
H5_DLL hid_t       H5Tget_member_type(hid_t type_id, unsigned membno);
H5_DLL herr_t      H5Tget_member_value(hid_t type_id, unsigned membno, void *value /*out*/);
H5_DLL H5T_cset_t  H5Tget_cset(hid_t type_id);
H5_DLL htri_t      H5Tis_variable_str(hid_t type_id);
/**
 * \ingroup H5T
 *
 * \brief Returns the native datatype identifier of a specified datatype
 *
 * \type_id
 * \param[in] direction Direction of search
 *
 * \return \hid_t{native datatype}
 *
 * \details H5Tget_native_type() returns the equivalent native datatype
 *          identifier for the datatype specified by \p type_id.
 *
 *          H5Tget_native_type() is designed primarily to facilitate use of
 *          the H5Dread() function, for which users otherwise must undertake a
 *          multi-step process to determine the native datatype of a dataset
 *          prior to reading it into memory. This function can be used for
 *          the following purposes:
 *
 *          \li To determine the native datatype of an atomic datatype
 *          \li To determine the base datatype of an array, enumerated, or
 *              variable-length datatype
 *          \li To determine the native atomic datatypes of the individual
 *              components of a compound datatype
 *
 *          For example, if \p type_id is a compound datatype, the returned
 *          datatype identifier will be for a similar compound datatype with
 *          each element converted to the corresponding native datatype;
 *          nested compound datatypes will be unwound. If \p type_id is an
 *          array, the returned datatype identifier will be for the native
 *          datatype of a single array element.
 *
 *          H5Tget_native_type() selects the first matching native datatype
 *          from the following list:
 *
 *          \li #H5T_NATIVE_CHAR
 *          \li #H5T_NATIVE_SHORT
 *          \li #H5T_NATIVE_INT
 *          \li #H5T_NATIVE_LONG
 *          \li #H5T_NATIVE_LLONG
 *
 *          \li #H5T_NATIVE_UCHAR
 *          \li #H5T_NATIVE_USHORT
 *          \li #H5T_NATIVE_UINT
 *          \li #H5T_NATIVE_ULONG
 *          \li #H5T_NATIVE_ULLONG
 *
 *          \li #H5T_NATIVE_FLOAT
 *          \li #H5T_NATIVE_DOUBLE
 *          \li #H5T_NATIVE_LDOUBLE
 *
 *          \li #H5T_NATIVE_B8
 *          \li #H5T_NATIVE_B16
 *          \li #H5T_NATIVE_B32
 *          \li #H5T_NATIVE_B64
 *
 *          The direction parameter indicates the order in which the library
 *          searches for a native datatype match. Valid values for direction
 *          are as follows:
 *          \snippet this H5T_direction_t_snip
 *
 *          H5Tget_native_type() is designed primarily for use with integer,
 *          floating point, and bitfield datatypes. String, time, opaque, and
 *          reference datatypes are returned as a copy of dtype_id. See above
 *          for compound, array, enumerated, and variable-length datatypes.
 *
 *          The identifier returned by H5Tget_native_type() should eventually
 *          be closed by calling H5Tclose() to release resources.
 *
 *          \note Please note that a datatype is actually an object
 *          identifier or handle returned from opening the datatype. It
 *          is not persistent and its value can be different from one HDF5
 *          session to the next.
 *
 *          \note H5Tequal() can be used to compare datatypes.
 *
 *          \note HDF5 High Level APIs that may also be of interest are: H5LTdtype_to_text()
 *                creates a text description of a datatype. H5LTtext_to_dtype() creates an
 *                HDF5 datatype given a text description.
 *
 * \since 1.6.0
 *
 */
H5_DLL hid_t       H5Tget_native_type(hid_t type_id, H5T_direction_t direction);

/* Setting property values */
H5_DLL herr_t H5Tset_size(hid_t type_id, size_t size);
H5_DLL herr_t H5Tset_order(hid_t type_id, H5T_order_t order);
H5_DLL herr_t H5Tset_precision(hid_t type_id, size_t prec);
H5_DLL herr_t H5Tset_offset(hid_t type_id, size_t offset);
H5_DLL herr_t H5Tset_pad(hid_t type_id, H5T_pad_t lsb, H5T_pad_t msb);
H5_DLL herr_t H5Tset_sign(hid_t type_id, H5T_sign_t sign);
H5_DLL herr_t H5Tset_fields(hid_t type_id, size_t spos, size_t epos, size_t esize, size_t mpos, size_t msize);
H5_DLL herr_t H5Tset_ebias(hid_t type_id, size_t ebias);
H5_DLL herr_t H5Tset_norm(hid_t type_id, H5T_norm_t norm);
H5_DLL herr_t H5Tset_inpad(hid_t type_id, H5T_pad_t pad);
H5_DLL herr_t H5Tset_cset(hid_t type_id, H5T_cset_t cset);
H5_DLL herr_t H5Tset_strpad(hid_t type_id, H5T_str_t strpad);

/* Type conversion database */
H5_DLL herr_t H5Tregister(H5T_pers_t pers, const char *name, hid_t src_id, hid_t dst_id, H5T_conv_t func);
H5_DLL herr_t H5Tunregister(H5T_pers_t pers, const char *name, hid_t src_id, hid_t dst_id, H5T_conv_t func);
H5_DLL H5T_conv_t H5Tfind(hid_t src_id, hid_t dst_id, H5T_cdata_t **pcdata);
H5_DLL htri_t     H5Tcompiler_conv(hid_t src_id, hid_t dst_id);
H5_DLL herr_t     H5Tconvert(hid_t src_id, hid_t dst_id, size_t nelmts, void *buf, void *background,
                             hid_t plist_id);
H5_DLL herr_t     H5Treclaim(hid_t type_id, hid_t space_id, hid_t plist_id, void *buf);

/* Symbols defined for compatibility with previous versions of the HDF5 API.
 *
 * Use of these symbols is deprecated.
 */
#ifndef H5_NO_DEPRECATED_SYMBOLS

/* Macros */

/* Typedefs */

/* Function prototypes */
H5_DLL herr_t H5Tcommit1(hid_t loc_id, const char *name, hid_t type_id);
H5_DLL hid_t  H5Topen1(hid_t loc_id, const char *name);
H5_DLL hid_t  H5Tarray_create1(hid_t base_id, int ndims, const hsize_t dim[/* ndims */],
                               const int perm[/* ndims */]);
H5_DLL int    H5Tget_array_dims1(hid_t type_id, hsize_t dims[], int perm[]);

#endif /* H5_NO_DEPRECATED_SYMBOLS */

#ifdef __cplusplus
}
#endif
#endif /* _H5Tpublic_H */
