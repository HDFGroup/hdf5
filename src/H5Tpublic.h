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

/*
 * This file contains public declarations for the H5T module.
 */
#ifndef _H5Tpublic_H
#define _H5Tpublic_H

/* Public headers needed by this file */
#include <H5public.h>
#include <H5Ipublic.h>

#define HOFFSET(S,M)    ((size_t)((const char*)&S.M-(const char*)&S))
#define HPOFFSET(P,M)   ((size_t)((const char*)&(P->M)-(const char*)P))

/* These are the various classes of data types */
typedef enum H5T_class_t {
    H5T_NO_CLASS         = -1,  /*error                                      */
    H5T_INTEGER          = 0,   /*integer types                              */
    H5T_FLOAT            = 1,   /*floating-point types                       */
    H5T_TIME             = 2,   /*date and time types                        */
    H5T_STRING           = 3,   /*character string types                     */
    H5T_BITFIELD         = 4,   /*bit field types                            */
    H5T_OPAQUE           = 5,   /*opaque types                               */
    H5T_COMPOUND         = 6,   /*compound types                             */

    H5T_NCLASSES         = 7    /*this must be last                          */
} H5T_class_t;

/* Byte orders */
typedef enum H5T_order_t {
    H5T_ORDER_ERROR      = -1,  /*error                                      */
    H5T_ORDER_LE         = 0,   /*little endian                              */
    H5T_ORDER_BE         = 1,   /*bit endian                                 */
    H5T_ORDER_VAX        = 2,   /*VAX mixed endian                           */
    H5T_ORDER_NONE       = 3    /*no particular order (strings, bits,..)     */
    /*H5T_ORDER_NONE must be last */
} H5T_order_t;

/* Types of integer sign schemes */
typedef enum H5T_sign_t {
    H5T_SGN_ERROR        = -1,  /*error                                      */
    H5T_SGN_NONE         = 0,   /*this is an unsigned type                   */
    H5T_SGN_2            = 1,   /*two's complement                           */

    H5T_NSGN             = 2    /*this must be last!                         */
} H5T_sign_t;

/* Floating-point normalization schemes */
typedef enum H5T_norm_t {
    H5T_NORM_ERROR       = -1,  /*error                                      */
    H5T_NORM_IMPLIED     = 0,   /*msb of mantissa isn't stored, always 1     */
    H5T_NORM_MSBSET      = 1,   /*msb of mantissa is always 1                */
    H5T_NORM_NONE        = 2    /*not normalized                             */
    /*H5T_NORM_NONE must be last */
} H5T_norm_t;

/* Character set to use for text strings */
typedef enum H5T_cset_t {
    H5T_CSET_ERROR       = -1,  /*error                                      */
    H5T_CSET_ASCII       = 0,   /*US ASCII                                   */

    H5T_NCSET            = 1    /*this must be last!                         */
} H5T_cset_t;

/* Type of padding to use in character strings */
typedef enum H5T_str_t {
    H5T_STR_ERROR        = -1,  /*error                                      */
    H5T_STR_NULL         = 0,   /*pad with null term like in C               */
    H5T_STR_SPACE        = 1,   /*pad with spaces like in Fortran            */

    H5T_NSTR             = 2    /*this must be last!                         */
} H5T_str_t;

/* Type of padding to use in other atomic types */
typedef enum H5T_pad_t {
    H5T_PAD_ERROR        = -1,  /*error                                      */
    H5T_PAD_ZERO         = 0,   /*always set to zero                         */
    H5T_PAD_ONE          = 1,   /*always set to one                          */
    H5T_PAD_BACKGROUND   = 2,   /*set to background value                    */

    H5T_NPAD             = 3    /*THIS MUST BE LAST                          */
} H5T_pad_t;

/* How is the `bkg' buffer used by the conversion function? */
typedef enum H5T_bkg_t {
    H5T_BKG_NO		= 0, 	/*background buffer is not needed, send NULL */
    H5T_BKG_TEMP	= 1, 	/*bkg buffer used as temp storage only	     */
    H5T_BKG_YES		= 2	/*init bkg buf with data before conversion   */
} H5T_bkg_t;

/* Commands sent to conversion functions */
typedef enum H5T_cmd_t {
    H5T_CONV_INIT	= 0,	/*query and/or initialize private data	     */
    H5T_CONV_CONV	= 1, 	/*convert data from source to dest data type */
    H5T_CONV_FREE	= 2	/*function is being removed from path	     */
} H5T_cmd_t;

/* Type conversion client data */
typedef struct H5T_cdata_t {
    H5T_cmd_t		command;/*what should the conversion function do?    */
    H5T_bkg_t		need_bkg;/*is the background buffer needed?	     */
    hbool_t		recalc;	/*recalculate private data		     */
    unsigned long	ncalls;	/*number of calls to conversion function     */
    unsigned long	nelmts; /*total number of data points converted	     */
    void		*priv;	/*private data				     */
} H5T_cdata_t;

/* All data type conversion functions are... */
typedef herr_t (*H5T_conv_t) (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
			      size_t nelmts, void *buf, void *bkg);

/* The predefined types */
#define H5T_NATIVE_CHAR         (H5open(), H5T_NATIVE_CHAR_g)
#define H5T_NATIVE_UCHAR        (H5open(), H5T_NATIVE_UCHAR_g)
#define H5T_NATIVE_SHORT        (H5open(), H5T_NATIVE_SHORT_g)
#define H5T_NATIVE_USHORT       (H5open(), H5T_NATIVE_USHORT_g)
#define H5T_NATIVE_INT          (H5open(), H5T_NATIVE_INT_g)
#define H5T_NATIVE_UINT         (H5open(), H5T_NATIVE_UINT_g)
#define H5T_NATIVE_LONG         (H5open(), H5T_NATIVE_LONG_g)
#define H5T_NATIVE_ULONG        (H5open(), H5T_NATIVE_ULONG_g)
#define H5T_NATIVE_LLONG        (H5open(), H5T_NATIVE_LLONG_g)
#define H5T_NATIVE_ULLONG       (H5open(), H5T_NATIVE_ULLONG_g)
#define H5T_NATIVE_HYPER        (H5open(), H5T_NATIVE_HYPER_g)
#define H5T_NATIVE_UHYPER       (H5open(), H5T_NATIVE_UHYPER_g)
#define H5T_NATIVE_INT8         (H5open(), H5T_NATIVE_INT8_g)
#define H5T_NATIVE_UINT8        (H5open(), H5T_NATIVE_UINT8_g)
#define H5T_NATIVE_INT16        (H5open(), H5T_NATIVE_INT16_g)
#define H5T_NATIVE_UINT16       (H5open(), H5T_NATIVE_UINT16_g)
#define H5T_NATIVE_INT32        (H5open(), H5T_NATIVE_INT32_g)
#define H5T_NATIVE_UINT32       (H5open(), H5T_NATIVE_UINT32_g)
#define H5T_NATIVE_INT64        (H5open(), H5T_NATIVE_INT64_g)
#define H5T_NATIVE_UINT64       (H5open(), H5T_NATIVE_UINT64_g)
#define H5T_NATIVE_FLOAT        (H5open(), H5T_NATIVE_FLOAT_g)
#define H5T_NATIVE_DOUBLE       (H5open(), H5T_NATIVE_DOUBLE_g)
#define H5T_NATIVE_TIME         (H5open(), H5T_NATIVE_TIME_g)
#define H5T_NATIVE_STRING       (H5open(), H5T_NATIVE_STRING_g)
#define H5T_NATIVE_BITFIELD     (H5open(), H5T_NATIVE_BITFIELD_g)
#define H5T_NATIVE_OPAQUE       (H5open(), H5T_NATIVE_OPAQUE_g)

#ifdef __cplusplus
extern "C" {
#endif
extern hid_t H5T_NATIVE_CHAR_g;
extern hid_t H5T_NATIVE_UCHAR_g;
extern hid_t H5T_NATIVE_SHORT_g;
extern hid_t H5T_NATIVE_USHORT_g;
extern hid_t H5T_NATIVE_INT_g;
extern hid_t H5T_NATIVE_UINT_g;
extern hid_t H5T_NATIVE_LONG_g;
extern hid_t H5T_NATIVE_INT8_g;
extern hid_t H5T_NATIVE_UINT8_g;
extern hid_t H5T_NATIVE_INT16_g;
extern hid_t H5T_NATIVE_UINT16_g;
extern hid_t H5T_NATIVE_INT32_g;
extern hid_t H5T_NATIVE_UINT32_g;
extern hid_t H5T_NATIVE_INT64_g;
extern hid_t H5T_NATIVE_UINT64_g;
extern hid_t H5T_NATIVE_ULONG_g;
extern hid_t H5T_NATIVE_LLONG_g;
extern hid_t H5T_NATIVE_ULLONG_g;
extern hid_t H5T_NATIVE_HYPER_g;
extern hid_t H5T_NATIVE_UHYPER_g;
extern hid_t H5T_NATIVE_FLOAT_g;
extern hid_t H5T_NATIVE_DOUBLE_g;
extern hid_t H5T_NATIVE_TIME_g;
extern hid_t H5T_NATIVE_STRING_g;
extern hid_t H5T_NATIVE_BITFIELD_g;
extern hid_t H5T_NATIVE_OPAQUE_g;

/* Operations defined on all data types */
hid_t H5Tcreate (H5T_class_t type, size_t size);
hid_t H5Tcopy (hid_t type_id);
herr_t H5Tclose (hid_t type_id);
hbool_t H5Tequal (hid_t type1_id, hid_t type2_id);
herr_t H5Tlock (hid_t type_id);
herr_t H5Tshare (hid_t location_id, hid_t type_id);
hbool_t H5Tis_shared (hid_t location_id, hid_t type_id);

/* Operations defined on compound data types */
herr_t H5Tinsert (hid_t parent_id, const char *name, size_t offset,
                  hid_t member_id);
herr_t H5Tpack (hid_t type_id);

/* Querying property values */
H5T_class_t H5Tget_class (hid_t type_id);
size_t H5Tget_size (hid_t type_id);
H5T_order_t H5Tget_order (hid_t type_id);
size_t H5Tget_precision (hid_t type_id);
size_t H5Tget_offset (hid_t type_id);
herr_t H5Tget_pad (hid_t type_id, H5T_pad_t *lsb/*out*/,
                   H5T_pad_t *msb/*out*/);
H5T_sign_t H5Tget_sign (hid_t type_id);
herr_t H5Tget_fields (hid_t type_id, size_t *spos/*out*/,
                      size_t *epos/*out*/, size_t *esize/*out*/,
                      size_t *mpos/*out*/, size_t *msize/*out*/);
size_t H5Tget_ebias (hid_t type_id);
H5T_norm_t H5Tget_norm (hid_t type_id);
H5T_pad_t H5Tget_inpad (hid_t type_id);
H5T_str_t H5Tget_strpad (hid_t type_id);
int H5Tget_nmembers (hid_t type_id);
char *H5Tget_member_name (hid_t type_id, int membno);
size_t H5Tget_member_offset (hid_t type_id, int membno);
int H5Tget_member_dims (hid_t type_id, int membno, size_t dims[]/*out*/,
                        int perm[]/*out*/);
hid_t H5Tget_member_type (hid_t type_id, int membno);
H5T_cset_t H5Tget_cset (hid_t type_id);

/* Setting property values */
herr_t H5Tset_size (hid_t type_id, size_t size);
herr_t H5Tset_order (hid_t type_id, H5T_order_t order);
herr_t H5Tset_precision (hid_t type_id, size_t prec);
herr_t H5Tset_offset (hid_t type_id, size_t offset);
herr_t H5Tset_pad (hid_t type_id, H5T_pad_t lsb, H5T_pad_t msb);
herr_t H5Tset_sign (hid_t type_id, H5T_sign_t sign);
herr_t H5Tset_fields (hid_t type_id, size_t spos, size_t epos, size_t esize,
                      size_t mpos, size_t msize);
herr_t H5Tset_ebias (hid_t type_id, size_t ebias);
herr_t H5Tset_norm (hid_t type_id, H5T_norm_t norm);
herr_t H5Tset_inpad (hid_t type_id, H5T_pad_t pad);
herr_t H5Tset_cset (hid_t type_id, H5T_cset_t cset);
herr_t H5Tset_strpad (hid_t type_id, H5T_str_t strpad);

/* Type conversion database */
herr_t H5Tregister_hard (hid_t src_id, hid_t dst_id, H5T_conv_t func);
herr_t H5Tregister_soft (H5T_class_t src, H5T_class_t dst, H5T_conv_t func);
herr_t H5Tunregister (H5T_conv_t func);
H5T_conv_t H5Tfind (hid_t src_id, hid_t dst_id, H5T_cdata_t **pcdata);

#ifdef __cplusplus
}
#endif
#endif
