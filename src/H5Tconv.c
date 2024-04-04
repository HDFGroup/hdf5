/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Module Info:    Datatype conversions for the H5T interface.
 */

/****************/
/* Module Setup */
/****************/

#include "H5Tmodule.h" /* This source code file is part of the H5T module */
#define H5R_FRIEND     /* Suppress error about including H5Rpkg */

/***********/
/* Headers */
/***********/
#include "H5private.h"   /* Generic Functions                        */
#include "H5CXprivate.h" /* API Contexts                             */
#include "H5Dprivate.h"  /* Datasets                                 */
#include "H5Eprivate.h"  /* Error handling                           */
#include "H5FLprivate.h" /* Free Lists                               */
#include "H5Iprivate.h"  /* IDs                                      */
#include "H5MMprivate.h" /* Memory management                        */
#include "H5Pprivate.h"  /* Property lists                           */
#include "H5Rpkg.h"      /* References                               */
#include "H5Tpkg.h"      /* Datatypes                                */

/****************/
/* Local Macros */
/****************/

/*
 * These macros are for the bodies of functions that convert buffers of one
 * atomic type to another using hardware.
 *
 * They all start with `H5T_CONV_' and end with two letters that represent the
 * source and destination types, respectively. The letters `s' and `S' refer to
 * signed integers while the letters `u' and `U' refer to unsigned integers, and
 * the letters `f' and `F' refer to floating-point values.
 *
 * The letter which is capitalized indicates that the corresponding type
 * (source or destination) is at least as large as the other type.
 *
 * Certain conversions may experience overflow conditions which arise when the
 * source value has a magnitude that cannot be represented by the destination
 * type.
 *
 * Suffix    Description
 * ------    -----------
 * sS:        Signed integers to signed integers where the destination is
 *        at least as wide as the source.     This case cannot generate
 *        overflows.
 *
 * sU:        Signed integers to unsigned integers where the destination is
 *        at least as wide as the source.     This case experiences
 *        overflows when the source value is negative.
 *
 * uS:        Unsigned integers to signed integers where the destination is
 *        at least as wide as the source.     This case can experience
 *        overflows when the source and destination are the same size.
 *
 * uU:        Unsigned integers to unsigned integers where the destination
 *        is at least as wide as the source.  Overflows are not
 *        possible in this case.
 *
 * Ss:        Signed integers to signed integers where the source is at
 *        least as large as the destination.  Overflows can occur when
 *        the destination is narrower than the source.
 *
 * Su:        Signed integers to unsigned integers where the source is at
 *        least as large as the destination.  Overflows occur when the
 *        source value is negative and can also occur if the
 *        destination is narrower than the source.
 *
 * Us:        Unsigned integers to signed integers where the source is at
 *        least as large as the destination.  Overflows can occur for
 *        all sizes.
 *
 * Uu:        Unsigned integers to unsigned integers where the source is at
 *        least as large as the destination. Overflows can occur if the
 *        destination is narrower than the source.
 *
 * su:        Conversion from signed integers to unsigned integers where
 *        the source and destination are the same size. Overflow occurs
 *        when the source value is negative.
 *
 * us:        Conversion from unsigned integers to signed integers where
 *        the source and destination are the same size.  Overflow
 *        occurs when the source magnitude is too large for the
 *        destination.
 *
 * fF:        Floating-point values to floating-point values where the
 *              destination is at least as wide as the source.     This case
 *              cannot generate overflows.
 *
 * Ff:        Floating-point values to floating-point values the source is at
 *        least as large as the destination.  Overflows can occur when
 *        the destination is narrower than the source.
 *
 * xF:          Integers to float-point(float or double) values where the destination
 *              is at least as wide as the source.  This case cannot generate
 *              overflows.
 *
 * Fx:          Float-point(float or double) values to integer where the source is
 *              at least as wide as the destination.  Overflow can occur
 *              when the source magnitude is too large for the destination.
 *
 * fX:    Floating-point values to integers where the destination is at least
 *        as wide as the source. This case cannot generate overflows.
 *
 * Xf:    Integers to floating-point values where the source is at least as
 *        wide as the destination. Overflows can occur when the destination is
 *        narrower than the source.
 *
 *
 * The macros take a subset of these arguments in the order listed here:
 *
 * CDATA:    A pointer to the H5T_cdata_t structure that was passed to the
 *           conversion function.
 *
 * STYPE:    The hid_t value for the source datatype.
 *
 * DTYPE:    The hid_t value for the destination datatype.
 *
 * BUF:      A pointer to the conversion buffer.
 *
 * NELMTS:   The number of values to be converted.
 *
 * ST:       The C name for source datatype (e.g., int)
 *
 * DT:       The C name for the destination datatype (e.g., signed char)
 *
 * D_MIN:    The minimum possible destination value.     For unsigned
 *        destination types this should be zero.    For signed
 *        destination types it's a negative value with a magnitude that
 *        is usually one greater than D_MAX.  Source values which are
 *        smaller than D_MIN generate overflows.
 *
 * D_MAX:    The maximum possible destination value. Source values which
 *        are larger than D_MAX generate overflows.
 *
 * The macros are implemented with a generic programming technique, similar
 * to templates in C++.  The macro which defines the "core" part of the
 * conversion (which actually moves the data from the source to the destination)
 * is invoked inside the H5T_CONV "template" macro by "gluing" it together,
 * which allows the core conversion macro to be invoked as necessary.
 *
 * "Core" macros come in two flavors: one which calls the exception handling
 * routine and one which doesn't (the "_NOEX" variant).  The presence of the
 * exception handling routine is detected before the loop over the values and
 * the appropriate core routine loop is executed.
 *
 * The generic "core" macros are: (others are specific to particular conversion)
 *
 * Suffix    Description
 * ------    -----------
 * xX:        Generic Conversion where the destination is at least as
 *              wide as the source.  This case cannot generate overflows.
 *
 * Xx:        Generic signed conversion where the source is at least as large
 *              as the destination.  Overflows can occur when the destination is
 *              narrower than the source.
 *
 * Ux:        Generic conversion for the `Us', `Uu' & `us' cases
 *        Overflow occurs when the source magnitude is too large for the
 *        destination.
 *
 */
#define H5T_CONV_xX_CORE(STYPE, DTYPE, S, D, ST, DT, D_MIN, D_MAX)                                           \
    {                                                                                                        \
        *(D) = (DT)(*(S));                                                                                   \
    }
#define H5T_CONV_xX_NOEX_CORE(STYPE, DTYPE, S, D, ST, DT, D_MIN, D_MAX)                                      \
    {                                                                                                        \
        *(D) = (DT)(*(S));                                                                                   \
    }

/* Added a condition branch(else if (*(S) == (DT)(D_MAX))) which seems redundant.
 * It handles a special situation when the source is "float" and assigned the value
 * of "INT_MAX".  A compiler may do roundup making this value "INT_MAX+1".  However,
 * when do comparison "if (*(S) > (DT)(D_MAX))", the compiler may consider them
 * equal. In this case, do not return exception but make sure the maximum is assigned
 * to the destination.   SLU - 2005/06/29
 */
#define H5T_CONV_Xx_CORE(STYPE, DTYPE, S, D, ST, DT, D_MIN, D_MAX)                                           \
    {                                                                                                        \
        if (*(S) > (ST)(D_MAX)) {                                                                            \
            H5T_conv_ret_t except_ret = (conv_ctx->u.conv.cb_struct.func)(                                   \
                H5T_CONV_EXCEPT_RANGE_HI, conv_ctx->u.conv.src_type_id, conv_ctx->u.conv.dst_type_id, S, D,  \
                conv_ctx->u.conv.cb_struct.user_data);                                                       \
            if (except_ret == H5T_CONV_UNHANDLED)                                                            \
                /* Let compiler convert if case is ignored by user handler*/                                 \
                *(D) = (DT)(D_MAX);                                                                          \
            else if (except_ret == H5T_CONV_ABORT)                                                           \
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception");       \
            /* if(except_ret==H5T_CONV_HANDLED): Fall through, user handled it */                            \
        }                                                                                                    \
        else if (*(S) < (ST)(D_MIN)) {                                                                       \
            H5T_conv_ret_t except_ret = (conv_ctx->u.conv.cb_struct.func)(                                   \
                H5T_CONV_EXCEPT_RANGE_LOW, conv_ctx->u.conv.src_type_id, conv_ctx->u.conv.dst_type_id, S, D, \
                conv_ctx->u.conv.cb_struct.user_data);                                                       \
            if (except_ret == H5T_CONV_UNHANDLED)                                                            \
                /* Let compiler convert if case is ignored by user handler*/                                 \
                *(D) = (DT)(D_MIN);                                                                          \
            else if (except_ret == H5T_CONV_ABORT)                                                           \
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception");       \
            /* if(except_ret==H5T_CONV_HANDLED): Fall through, user handled it */                            \
        }                                                                                                    \
        else                                                                                                 \
            *(D) = (DT)(*(S));                                                                               \
    }
#define H5T_CONV_Xx_NOEX_CORE(STYPE, DTYPE, S, D, ST, DT, D_MIN, D_MAX)                                      \
    {                                                                                                        \
        if (*(S) > (ST)(D_MAX)) {                                                                            \
            *(D) = (DT)(D_MAX);                                                                              \
        }                                                                                                    \
        else if (*(S) < (ST)(D_MIN)) {                                                                       \
            *(D) = (DT)(D_MIN);                                                                              \
        }                                                                                                    \
        else                                                                                                 \
            *(D) = (DT)(*(S));                                                                               \
    }

#define H5T_CONV_Ux_CORE(STYPE, DTYPE, S, D, ST, DT, D_MIN, D_MAX)                                           \
    {                                                                                                        \
        if (*(S) > (ST)(D_MAX)) {                                                                            \
            H5T_conv_ret_t except_ret = (conv_ctx->u.conv.cb_struct.func)(                                   \
                H5T_CONV_EXCEPT_RANGE_HI, conv_ctx->u.conv.src_type_id, conv_ctx->u.conv.dst_type_id, S, D,  \
                conv_ctx->u.conv.cb_struct.user_data);                                                       \
            if (except_ret == H5T_CONV_UNHANDLED)                                                            \
                /* Let compiler convert if case is ignored by user handler*/                                 \
                *(D) = (DT)(D_MAX);                                                                          \
            else if (except_ret == H5T_CONV_ABORT)                                                           \
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception");       \
            /* if(except_ret==H5T_CONV_HANDLED): Fall through, user handled it */                            \
        }                                                                                                    \
        else                                                                                                 \
            *(D) = (DT)(*(S));                                                                               \
    }
#define H5T_CONV_Ux_NOEX_CORE(STYPE, DTYPE, S, D, ST, DT, D_MIN, D_MAX)                                      \
    {                                                                                                        \
        if (*(S) > (ST)(D_MAX)) {                                                                            \
            *(D) = (DT)(D_MAX);                                                                              \
        }                                                                                                    \
        else                                                                                                 \
            *(D) = (DT)(*(S));                                                                               \
    }

#define H5T_CONV_sS(STYPE, DTYPE, ST, DT, D_MIN, D_MAX)                                                      \
    do {                                                                                                     \
        HDcompile_assert(sizeof(ST) <= sizeof(DT));                                                          \
        H5T_CONV(H5T_CONV_xX, STYPE, DTYPE, ST, DT, D_MIN, D_MAX, N)                                         \
    } while (0)

#define H5T_CONV_sU_CORE(STYPE, DTYPE, S, D, ST, DT, D_MIN, D_MAX)                                           \
    {                                                                                                        \
        if (*(S) < 0) {                                                                                      \
            H5T_conv_ret_t except_ret = (conv_ctx->u.conv.cb_struct.func)(                                   \
                H5T_CONV_EXCEPT_RANGE_LOW, conv_ctx->u.conv.src_type_id, conv_ctx->u.conv.dst_type_id, S, D, \
                conv_ctx->u.conv.cb_struct.user_data);                                                       \
            if (except_ret == H5T_CONV_UNHANDLED)                                                            \
                /* Let compiler convert if case is ignored by user handler*/                                 \
                *(D) = 0;                                                                                    \
            else if (except_ret == H5T_CONV_ABORT)                                                           \
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception");       \
            /* if(except_ret==H5T_CONV_HANDLED): Fall through, user handled it */                            \
        }                                                                                                    \
        else                                                                                                 \
            *(D) = (DT)(*(S));                                                                               \
    }
#define H5T_CONV_sU_NOEX_CORE(STYPE, DTYPE, S, D, ST, DT, D_MIN, D_MAX)                                      \
    {                                                                                                        \
        if (*(S) < 0)                                                                                        \
            *(D) = 0;                                                                                        \
        else                                                                                                 \
            *(D) = (DT)(*(S));                                                                               \
    }

#define H5T_CONV_sU(STYPE, DTYPE, ST, DT, D_MIN, D_MAX)                                                      \
    do {                                                                                                     \
        HDcompile_assert(sizeof(ST) <= sizeof(DT));                                                          \
        H5T_CONV(H5T_CONV_sU, STYPE, DTYPE, ST, DT, D_MIN, D_MAX, N)                                         \
    } while (0)

/* Define to 1 if overflow is possible during conversion, 0 otherwise
 * Because destination is at least as wide as the source, this should only
 * occur between types of equal size */
#define H5T_CONV_uS_UCHAR_SHORT 0
#define H5T_CONV_uS_UCHAR_INT   0
#define H5T_CONV_uS_UCHAR_LONG  0
#define H5T_CONV_uS_UCHAR_LLONG 0
#if H5_SIZEOF_SHORT == H5_SIZEOF_INT
#define H5T_CONV_uS_USHORT_INT 1
#else
#define H5T_CONV_uS_USHORT_INT 0
#endif
#define H5T_CONV_uS_USHORT_LONG  0
#define H5T_CONV_uS_USHORT_LLONG 0
#if H5_SIZEOF_INT == H5_SIZEOF_LONG
#define H5T_CONV_uS_UINT_LONG 1
#else
#define H5T_CONV_uS_UINT_LONG 0
#endif
#define H5T_CONV_uS_UINT_LLONG 0
#if H5_SIZEOF_LONG == H5_SIZEOF_LONG_LONG
#define H5T_CONV_uS_ULONG_LLONG 1
#else
#define H5T_CONV_uS_ULONG_LLONG 0
#endif

/* Note. If an argument is stringified or concatenated, the prescan does not
 * occur. To expand the macro, then stringify or concatenate its expansion,
 * one macro must call another macro that does the stringification or
 * concatenation. */
#define H5T_CONV_uS_EVAL_TYPES(STYPE, DTYPE) H5_GLUE4(H5T_CONV_uS_, STYPE, _, DTYPE)

/* Called if overflow is possible */
#define H5T_CONV_uS_CORE_1(S, D, ST, DT, D_MIN, D_MAX)                                                       \
    if (*(S) > (DT)(D_MAX)) {                                                                                \
        H5T_conv_ret_t except_ret = (conv_ctx->u.conv.cb_struct.func)(                                       \
            H5T_CONV_EXCEPT_RANGE_HI, conv_ctx->u.conv.src_type_id, conv_ctx->u.conv.dst_type_id, S, D,      \
            conv_ctx->u.conv.cb_struct.user_data);                                                           \
        if (except_ret == H5T_CONV_UNHANDLED)                                                                \
            /* Let compiler convert if case is ignored by user handler */                                    \
            *(D) = (DT)(D_MAX);                                                                              \
        else if (except_ret == H5T_CONV_ABORT)                                                               \
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception");           \
        /* if (except_ret==H5T_CONV_HANDLED): Fall through, user handled it */                               \
    }                                                                                                        \
    else                                                                                                     \
        *(D) = (DT)(*(S));

/* Called if no overflow is possible */
#define H5T_CONV_uS_CORE_0(S, D, ST, DT, D_MIN, D_MAX) *(D) = (DT)(*(S));

#define H5T_CONV_uS_CORE_I(over, S, D, ST, DT, D_MIN, D_MAX)                                                 \
    H5_GLUE(H5T_CONV_uS_CORE_, over)(S, D, ST, DT, D_MIN, D_MAX)

#define H5T_CONV_uS_CORE(STYPE, DTYPE, S, D, ST, DT, D_MIN, D_MAX)                                           \
    {                                                                                                        \
        H5T_CONV_uS_CORE_I(H5T_CONV_uS_EVAL_TYPES(STYPE, DTYPE), S, D, ST, DT, D_MIN, D_MAX)                 \
    }

/* Called if overflow is possible */
#define H5T_CONV_uS_NOEX_CORE_1(S, D, ST, DT, D_MIN, D_MAX)                                                  \
    if (*(S) > (DT)(D_MAX))                                                                                  \
        *(D) = (D_MAX);                                                                                      \
    else                                                                                                     \
        *(D) = (DT)(*(S));

/* Called if no overflow is possible */
#define H5T_CONV_uS_NOEX_CORE_0(S, D, ST, DT, D_MIN, D_MAX) *(D) = (DT)(*(S));

#define H5T_CONV_uS_NOEX_CORE_I(over, S, D, ST, DT, D_MIN, D_MAX)                                            \
    H5_GLUE(H5T_CONV_uS_NOEX_CORE_, over)(S, D, ST, DT, D_MIN, D_MAX)

#define H5T_CONV_uS_NOEX_CORE(STYPE, DTYPE, S, D, ST, DT, D_MIN, D_MAX)                                      \
    {                                                                                                        \
        H5T_CONV_uS_NOEX_CORE_I(H5T_CONV_uS_EVAL_TYPES(STYPE, DTYPE), S, D, ST, DT, D_MIN, D_MAX)            \
    }

#define H5T_CONV_uS(STYPE, DTYPE, ST, DT, D_MIN, D_MAX)                                                      \
    do {                                                                                                     \
        HDcompile_assert(sizeof(ST) <= sizeof(DT));                                                          \
        H5T_CONV(H5T_CONV_uS, STYPE, DTYPE, ST, DT, D_MIN, D_MAX, N)                                         \
    } while (0)

#define H5T_CONV_uU(STYPE, DTYPE, ST, DT, D_MIN, D_MAX)                                                      \
    do {                                                                                                     \
        HDcompile_assert(sizeof(ST) <= sizeof(DT));                                                          \
        H5T_CONV(H5T_CONV_xX, STYPE, DTYPE, ST, DT, D_MIN, D_MAX, N)                                         \
    } while (0)

#define H5T_CONV_Ss(STYPE, DTYPE, ST, DT, D_MIN, D_MAX)                                                      \
    do {                                                                                                     \
        HDcompile_assert(sizeof(ST) >= sizeof(DT));                                                          \
        H5T_CONV(H5T_CONV_Xx, STYPE, DTYPE, ST, DT, D_MIN, D_MAX, N)                                         \
    } while (0)

#define H5T_CONV_Su_CORE(STYPE, DTYPE, S, D, ST, DT, D_MIN, D_MAX)                                           \
    {                                                                                                        \
        if (*(S) < 0) {                                                                                      \
            H5T_conv_ret_t except_ret = (conv_ctx->u.conv.cb_struct.func)(                                   \
                H5T_CONV_EXCEPT_RANGE_LOW, conv_ctx->u.conv.src_type_id, conv_ctx->u.conv.dst_type_id, S, D, \
                conv_ctx->u.conv.cb_struct.user_data);                                                       \
            if (except_ret == H5T_CONV_UNHANDLED)                                                            \
                /* Let compiler convert if case is ignored by user handler*/                                 \
                *(D) = 0;                                                                                    \
            else if (except_ret == H5T_CONV_ABORT)                                                           \
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception");       \
            /* if(except_ret==H5T_CONV_HANDLED): Fall through, user handled it */                            \
        }                                                                                                    \
        else if (sizeof(ST) > sizeof(DT) && *(S) > (ST)(D_MAX)) {                                            \
            H5T_conv_ret_t except_ret = (conv_ctx->u.conv.cb_struct.func)(                                   \
                H5T_CONV_EXCEPT_RANGE_HI, conv_ctx->u.conv.src_type_id, conv_ctx->u.conv.dst_type_id, S, D,  \
                conv_ctx->u.conv.cb_struct.user_data);                                                       \
            if (except_ret == H5T_CONV_UNHANDLED)                                                            \
                /* Let compiler convert if case is ignored by user handler*/                                 \
                *(D) = (DT)(D_MAX);                                                                          \
            else if (except_ret == H5T_CONV_ABORT)                                                           \
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception");       \
            /* if(except_ret==H5T_CONV_HANDLED): Fall through, user handled it */                            \
        }                                                                                                    \
        else                                                                                                 \
            *(D) = (DT)(*(S));                                                                               \
    }
#define H5T_CONV_Su_NOEX_CORE(STYPE, DTYPE, S, D, ST, DT, D_MIN, D_MAX)                                      \
    {                                                                                                        \
        if (*(S) < 0)                                                                                        \
            *(D) = 0;                                                                                        \
        else if (sizeof(ST) > sizeof(DT) && *(S) > (ST)(D_MAX))                                              \
            *(D) = (DT)(D_MAX);                                                                              \
        else                                                                                                 \
            *(D) = (DT)(*(S));                                                                               \
    }

#define H5T_CONV_Su(STYPE, DTYPE, ST, DT, D_MIN, D_MAX)                                                      \
    do {                                                                                                     \
        HDcompile_assert(sizeof(ST) >= sizeof(DT));                                                          \
        H5T_CONV(H5T_CONV_Su, STYPE, DTYPE, ST, DT, D_MIN, D_MAX, N)                                         \
    } while (0)

#define H5T_CONV_Us(STYPE, DTYPE, ST, DT, D_MIN, D_MAX)                                                      \
    do {                                                                                                     \
        HDcompile_assert(sizeof(ST) >= sizeof(DT));                                                          \
        H5T_CONV(H5T_CONV_Ux, STYPE, DTYPE, ST, DT, D_MIN, D_MAX, N)                                         \
    } while (0)

#define H5T_CONV_Uu(STYPE, DTYPE, ST, DT, D_MIN, D_MAX)                                                      \
    do {                                                                                                     \
        HDcompile_assert(sizeof(ST) >= sizeof(DT));                                                          \
        H5T_CONV(H5T_CONV_Ux, STYPE, DTYPE, ST, DT, D_MIN, D_MAX, N)                                         \
    } while (0)

#define H5T_CONV_su_CORE(STYPE, DTYPE, S, D, ST, DT, D_MIN, D_MAX)                                           \
    {                                                                                                        \
        /* Assumes memory format of unsigned & signed integers is same */                                    \
        if (*(S) < 0) {                                                                                      \
            H5T_conv_ret_t except_ret = (conv_ctx->u.conv.cb_struct.func)(                                   \
                H5T_CONV_EXCEPT_RANGE_LOW, conv_ctx->u.conv.src_type_id, conv_ctx->u.conv.dst_type_id, S, D, \
                conv_ctx->u.conv.cb_struct.user_data);                                                       \
            if (except_ret == H5T_CONV_UNHANDLED)                                                            \
                /* Let compiler convert if case is ignored by user handler*/                                 \
                *(D) = 0;                                                                                    \
            else if (except_ret == H5T_CONV_ABORT)                                                           \
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception");       \
            /* if(except_ret==H5T_CONV_HANDLED): Fall through, user handled it */                            \
        }                                                                                                    \
        else                                                                                                 \
            *(D) = (DT)(*(S));                                                                               \
    }
#define H5T_CONV_su_NOEX_CORE(STYPE, DTYPE, S, D, ST, DT, D_MIN, D_MAX)                                      \
    {                                                                                                        \
        /* Assumes memory format of unsigned & signed integers is same */                                    \
        if (*(S) < 0)                                                                                        \
            *(D) = 0;                                                                                        \
        else                                                                                                 \
            *(D) = (DT)(*(S));                                                                               \
    }

#define H5T_CONV_su(STYPE, DTYPE, ST, DT, D_MIN, D_MAX)                                                      \
    do {                                                                                                     \
        HDcompile_assert(sizeof(ST) == sizeof(DT));                                                          \
        H5T_CONV(H5T_CONV_su, STYPE, DTYPE, ST, DT, D_MIN, D_MAX, N)                                         \
    } while (0)

#define H5T_CONV_us_CORE(STYPE, DTYPE, S, D, ST, DT, D_MIN, D_MAX)                                           \
    {                                                                                                        \
        /* Assumes memory format of unsigned & signed integers is same */                                    \
        if (*(S) > (ST)(D_MAX)) {                                                                            \
            H5T_conv_ret_t except_ret = (conv_ctx->u.conv.cb_struct.func)(                                   \
                H5T_CONV_EXCEPT_RANGE_HI, conv_ctx->u.conv.src_type_id, conv_ctx->u.conv.dst_type_id, S, D,  \
                conv_ctx->u.conv.cb_struct.user_data);                                                       \
            if (except_ret == H5T_CONV_UNHANDLED)                                                            \
                /* Let compiler convert if case is ignored by user handler*/                                 \
                *(D) = (DT)(D_MAX);                                                                          \
            else if (except_ret == H5T_CONV_ABORT)                                                           \
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception");       \
            /* if(except_ret==H5T_CONV_HANDLED): Fall through, user handled it */                            \
        }                                                                                                    \
        else                                                                                                 \
            *(D) = (DT)(*(S));                                                                               \
    }
#define H5T_CONV_us_NOEX_CORE(STYPE, DTYPE, S, D, ST, DT, D_MIN, D_MAX)                                      \
    {                                                                                                        \
        /* Assumes memory format of unsigned & signed integers is same */                                    \
        if (*(S) > (ST)(D_MAX))                                                                              \
            *(D) = (DT)(D_MAX);                                                                              \
        else                                                                                                 \
            *(D) = (DT)(*(S));                                                                               \
    }

#define H5T_CONV_us(STYPE, DTYPE, ST, DT, D_MIN, D_MAX)                                                      \
    do {                                                                                                     \
        HDcompile_assert(sizeof(ST) == sizeof(DT));                                                          \
        H5T_CONV(H5T_CONV_us, STYPE, DTYPE, ST, DT, D_MIN, D_MAX, N)                                         \
    } while (0)

#define H5T_CONV_fF(STYPE, DTYPE, ST, DT, D_MIN, D_MAX)                                                      \
    do {                                                                                                     \
        HDcompile_assert(sizeof(ST) <= sizeof(DT));                                                          \
        H5T_CONV(H5T_CONV_xX, STYPE, DTYPE, ST, DT, D_MIN, D_MAX, N)                                         \
    } while (0)

/* Same as H5T_CONV_Xx_CORE, except that instead of using D_MAX and D_MIN
 * when an overflow occurs, use the 'float' infinity values.
 */
#define H5T_CONV_Ff_CORE(STYPE, DTYPE, S, D, ST, DT, D_MIN, D_MAX)                                           \
    {                                                                                                        \
        if (*(S) > (ST)(D_MAX)) {                                                                            \
            H5T_conv_ret_t except_ret = (conv_ctx->u.conv.cb_struct.func)(                                   \
                H5T_CONV_EXCEPT_RANGE_HI, conv_ctx->u.conv.src_type_id, conv_ctx->u.conv.dst_type_id, S, D,  \
                conv_ctx->u.conv.cb_struct.user_data);                                                       \
            if (except_ret == H5T_CONV_UNHANDLED)                                                            \
                /* Let compiler convert if case is ignored by user handler*/                                 \
                *(D) = H5_GLUE3(H5T_NATIVE_, DTYPE, _POS_INF_g);                                             \
            else if (except_ret == H5T_CONV_ABORT)                                                           \
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception");       \
            /* if(except_ret==H5T_CONV_HANDLED): Fall through, user handled it */                            \
        }                                                                                                    \
        else if (*(S) < (ST)(D_MIN)) {                                                                       \
            H5T_conv_ret_t except_ret = (conv_ctx->u.conv.cb_struct.func)(                                   \
                H5T_CONV_EXCEPT_RANGE_LOW, conv_ctx->u.conv.src_type_id, conv_ctx->u.conv.dst_type_id, S, D, \
                conv_ctx->u.conv.cb_struct.user_data);                                                       \
            if (except_ret == H5T_CONV_UNHANDLED)                                                            \
                /* Let compiler convert if case is ignored by user handler*/                                 \
                *(D) = H5_GLUE3(H5T_NATIVE_, DTYPE, _NEG_INF_g);                                             \
            else if (except_ret == H5T_CONV_ABORT)                                                           \
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception");       \
            /* if(except_ret==H5T_CONV_HANDLED): Fall through, user handled it */                            \
        }                                                                                                    \
        else                                                                                                 \
            *(D) = (DT)(*(S));                                                                               \
    }
#define H5T_CONV_Ff_NOEX_CORE(STYPE, DTYPE, S, D, ST, DT, D_MIN, D_MAX)                                      \
    {                                                                                                        \
        if (*(S) > (ST)(D_MAX))                                                                              \
            *(D) = H5_GLUE3(H5T_NATIVE_, DTYPE, _POS_INF_g);                                                 \
        else if (*(S) < (ST)(D_MIN))                                                                         \
            *(D) = H5_GLUE3(H5T_NATIVE_, DTYPE, _NEG_INF_g);                                                 \
        else                                                                                                 \
            *(D) = (DT)(*(S));                                                                               \
    }

#define H5T_CONV_Ff(STYPE, DTYPE, ST, DT, D_MIN, D_MAX)                                                      \
    do {                                                                                                     \
        HDcompile_assert(sizeof(ST) >= sizeof(DT));                                                          \
        H5T_CONV(H5T_CONV_Ff, STYPE, DTYPE, ST, DT, D_MIN, D_MAX, N)                                         \
    } while (0)

#define H5T_HI_LO_BIT_SET(TYP, V, LO, HI)                                                                    \
    {                                                                                                        \
        unsigned      count;                                                                                 \
        unsigned char p;                                                                                     \
        unsigned      u;                                                                                     \
                                                                                                             \
        count = 0;                                                                                           \
        for (u = 0; u < sizeof(TYP); u++) {                                                                  \
            count = (((unsigned)sizeof(TYP) - 1) - u) * 8;                                                   \
            p     = (unsigned char)((V) >> count);                                                           \
            if (p > 0) {                                                                                     \
                if (p & 0x80)                                                                                \
                    count += 7;                                                                              \
                else if (p & 0x40)                                                                           \
                    count += 6;                                                                              \
                else if (p & 0x20)                                                                           \
                    count += 5;                                                                              \
                else if (p & 0x10)                                                                           \
                    count += 4;                                                                              \
                else if (p & 0x08)                                                                           \
                    count += 3;                                                                              \
                else if (p & 0x04)                                                                           \
                    count += 2;                                                                              \
                else if (p & 0x02)                                                                           \
                    count += 1;                                                                              \
                break;                                                                                       \
            } /* end if */                                                                                   \
        }     /* end for */                                                                                  \
                                                                                                             \
        HI = count;                                                                                          \
                                                                                                             \
        count = 0;                                                                                           \
        for (u = 0; u < sizeof(TYP); u++) {                                                                  \
            p = (unsigned char)((V) >> (u * 8));                                                             \
            if (p > 0) {                                                                                     \
                count = u * 8;                                                                               \
                                                                                                             \
                if (p & 0x01)                                                                                \
                    ;                                                                                        \
                else if (p & 0x02)                                                                           \
                    count += 1;                                                                              \
                else if (p & 0x04)                                                                           \
                    count += 2;                                                                              \
                else if (p & 0x08)                                                                           \
                    count += 3;                                                                              \
                else if (p & 0x10)                                                                           \
                    count += 4;                                                                              \
                else if (p & 0x20)                                                                           \
                    count += 5;                                                                              \
                else if (p & 0x40)                                                                           \
                    count += 6;                                                                              \
                else if (p & 0x80)                                                                           \
                    count += 7;                                                                              \
                break;                                                                                       \
            } /* end if */                                                                                   \
        }     /* end for */                                                                                  \
                                                                                                             \
        LO = count;                                                                                          \
    }

#define H5T_CONV_xF_CORE(STYPE, DTYPE, S, D, ST, DT, D_MIN, D_MAX)                                           \
    {                                                                                                        \
        if (sprec > dprec) {                                                                                 \
            unsigned low_bit_pos, high_bit_pos;                                                              \
                                                                                                             \
            /* Detect high & low bits set in source */                                                       \
            H5T_HI_LO_BIT_SET(ST, *(S), low_bit_pos, high_bit_pos)                                           \
                                                                                                             \
            /* Check for more bits of precision in src than available in dst */                              \
            if ((high_bit_pos - low_bit_pos) >= dprec) {                                                     \
                H5T_conv_ret_t except_ret = (conv_ctx->u.conv.cb_struct.func)(                               \
                    H5T_CONV_EXCEPT_PRECISION, conv_ctx->u.conv.src_type_id, conv_ctx->u.conv.dst_type_id,   \
                    S, D, conv_ctx->u.conv.cb_struct.user_data);                                             \
                if (except_ret == H5T_CONV_UNHANDLED)                                                        \
                    /* Let compiler convert if case is ignored by user handler*/                             \
                    *(D) = (DT)(*(S));                                                                       \
                else if (except_ret == H5T_CONV_ABORT)                                                       \
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception");   \
                /* if(except_ret==H5T_CONV_HANDLED): Fall through, user handled it */                        \
            }                                                                                                \
            else                                                                                             \
                *(D) = (DT)(*(S));                                                                           \
        }                                                                                                    \
        else                                                                                                 \
            *(D) = (DT)(*(S));                                                                               \
    }
#define H5T_CONV_xF_NOEX_CORE(STYPE, DTYPE, S, D, ST, DT, D_MIN, D_MAX)                                      \
    {                                                                                                        \
        *(D) = (DT)(*(S));                                                                                   \
    }

#define H5T_CONV_xF(STYPE, DTYPE, ST, DT, D_MIN, D_MAX)                                                      \
    do {                                                                                                     \
        H5T_CONV(H5T_CONV_xF, STYPE, DTYPE, ST, DT, D_MIN, D_MAX, Y)                                         \
    } while (0)

/* Quincey added the condition branch (else if (*(S) != (ST)((DT)(*(S))))).
 * It handles a special situation when the source is "float" and assigned the value
 * of "INT_MAX".  Compilers do roundup making this value "INT_MAX+1".  This branch
 * is to check that situation and return exception for some compilers, mainly GCC.
 * The branch if (*(S) > (DT)(D_MAX) || (sprec < dprec && *(S) ==
 * (ST)(D_MAX))) is for some compilers like Sun, HP, IBM, and SGI where under
 * the same situation the "int" doesn't overflow.  SLU - 2005/9/12
 */
#define H5T_CONV_Fx_CORE(STYPE, DTYPE, S, D, ST, DT, D_MIN, D_MAX)                                           \
    {                                                                                                        \
        if (*(S) > (ST)(D_MAX) || (sprec < dprec && *(S) == (ST)(D_MAX))) {                                  \
            H5T_conv_ret_t except_ret = (conv_ctx->u.conv.cb_struct.func)(                                   \
                H5T_CONV_EXCEPT_RANGE_HI, conv_ctx->u.conv.src_type_id, conv_ctx->u.conv.dst_type_id, S, D,  \
                conv_ctx->u.conv.cb_struct.user_data);                                                       \
            if (except_ret == H5T_CONV_UNHANDLED)                                                            \
                /* Let compiler convert if case is ignored by user handler*/                                 \
                *(D) = (DT)(D_MAX);                                                                          \
            else if (except_ret == H5T_CONV_ABORT)                                                           \
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception");       \
            /* if(except_ret==H5T_CONV_HANDLED): Fall through, user handled it */                            \
        }                                                                                                    \
        else if (*(S) < (ST)(D_MIN)) {                                                                       \
            H5T_conv_ret_t except_ret = (conv_ctx->u.conv.cb_struct.func)(                                   \
                H5T_CONV_EXCEPT_RANGE_LOW, conv_ctx->u.conv.src_type_id, conv_ctx->u.conv.dst_type_id, S, D, \
                conv_ctx->u.conv.cb_struct.user_data);                                                       \
            if (except_ret == H5T_CONV_UNHANDLED)                                                            \
                /* Let compiler convert if case is ignored by user handler*/                                 \
                *(D) = (DT)(D_MIN);                                                                          \
            else if (except_ret == H5T_CONV_ABORT)                                                           \
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception");       \
            /* if(except_ret==H5T_CONV_HANDLED): Fall through, user handled it */                            \
        }                                                                                                    \
        else if (*(S) != (ST)((DT)(*(S)))) {                                                                 \
            H5T_conv_ret_t except_ret = (conv_ctx->u.conv.cb_struct.func)(                                   \
                H5T_CONV_EXCEPT_TRUNCATE, conv_ctx->u.conv.src_type_id, conv_ctx->u.conv.dst_type_id, S, D,  \
                conv_ctx->u.conv.cb_struct.user_data);                                                       \
            if (except_ret == H5T_CONV_UNHANDLED)                                                            \
                /* Let compiler convert if case is ignored by user handler*/                                 \
                *(D) = (DT)(*(S));                                                                           \
            else if (except_ret == H5T_CONV_ABORT)                                                           \
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception");       \
            /* if(except_ret==H5T_CONV_HANDLED): Fall through, user handled it */                            \
        }                                                                                                    \
        else                                                                                                 \
            *(D) = (DT)(*(S));                                                                               \
    }
#define H5T_CONV_Fx_NOEX_CORE(STYPE, DTYPE, S, D, ST, DT, D_MIN, D_MAX)                                      \
    {                                                                                                        \
        if (*(S) > (ST)(D_MAX))                                                                              \
            *(D) = (DT)(D_MAX);                                                                              \
        else if (*(S) < (ST)(D_MIN))                                                                         \
            *(D) = (DT)(D_MIN);                                                                              \
        else                                                                                                 \
            *(D) = (DT)(*(S));                                                                               \
    }

#define H5T_CONV_Fx(STYPE, DTYPE, ST, DT, D_MIN, D_MAX)                                                      \
    do {                                                                                                     \
        H5T_CONV(H5T_CONV_Fx, STYPE, DTYPE, ST, DT, D_MIN, D_MAX, Y)                                         \
    } while (0)

#define H5T_CONV_fX(STYPE, DTYPE, ST, DT, D_MIN, D_MAX)                                                      \
    do {                                                                                                     \
        HDcompile_assert(sizeof(ST) <= sizeof(DT));                                                          \
        H5T_CONV(H5T_CONV_xX, STYPE, DTYPE, ST, DT, D_MIN, D_MAX, N)                                         \
    } while (0)

#define H5T_CONV_Xf_CORE(STYPE, DTYPE, S, D, ST, DT, D_MIN, D_MAX)                                           \
    {                                                                                                        \
        if (*(S) > (ST)(D_MAX) || (sprec < dprec && *(S) == (ST)(D_MAX))) {                                  \
            H5T_conv_ret_t except_ret = (conv_ctx->u.conv.cb_struct.func)(                                   \
                H5T_CONV_EXCEPT_RANGE_HI, conv_ctx->u.conv.src_type_id, conv_ctx->u.conv.dst_type_id, S, D,  \
                conv_ctx->u.conv.cb_struct.user_data);                                                       \
            if (except_ret == H5T_CONV_UNHANDLED)                                                            \
                /* Let compiler convert if case is ignored by user handler*/                                 \
                *(D) = H5_GLUE3(H5T_NATIVE_, DTYPE, _POS_INF_g);                                             \
            else if (except_ret == H5T_CONV_ABORT)                                                           \
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception");       \
            /* if(except_ret==H5T_CONV_HANDLED): Fall through, user handled it */                            \
        }                                                                                                    \
        else if (*(S) < (ST)(D_MIN)) {                                                                       \
            H5T_conv_ret_t except_ret = (conv_ctx->u.conv.cb_struct.func)(                                   \
                H5T_CONV_EXCEPT_RANGE_LOW, conv_ctx->u.conv.src_type_id, conv_ctx->u.conv.dst_type_id, S, D, \
                conv_ctx->u.conv.cb_struct.user_data);                                                       \
            if (except_ret == H5T_CONV_UNHANDLED)                                                            \
                /* Let compiler convert if case is ignored by user handler*/                                 \
                *(D) = H5_GLUE3(H5T_NATIVE_, DTYPE, _NEG_INF_g);                                             \
            else if (except_ret == H5T_CONV_ABORT)                                                           \
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception");       \
            /* if(except_ret==H5T_CONV_HANDLED): Fall through, user handled it */                            \
        }                                                                                                    \
        else if (sprec > dprec) {                                                                            \
            unsigned low_bit_pos, high_bit_pos;                                                              \
                                                                                                             \
            /* Detect high & low bits set in source */                                                       \
            H5T_HI_LO_BIT_SET(ST, *(S), low_bit_pos, high_bit_pos)                                           \
                                                                                                             \
            /* Check for more bits of precision in src than available in dst */                              \
            if ((high_bit_pos - low_bit_pos) >= dprec) {                                                     \
                H5T_conv_ret_t except_ret = (conv_ctx->u.conv.cb_struct.func)(                               \
                    H5T_CONV_EXCEPT_PRECISION, conv_ctx->u.conv.src_type_id, conv_ctx->u.conv.dst_type_id,   \
                    S, D, conv_ctx->u.conv.cb_struct.user_data);                                             \
                if (except_ret == H5T_CONV_UNHANDLED)                                                        \
                    /* Let compiler convert if case is ignored by user handler*/                             \
                    *(D) = (DT)(*(S));                                                                       \
                else if (except_ret == H5T_CONV_ABORT)                                                       \
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception");   \
                /* if(except_ret==H5T_CONV_HANDLED): Fall through, user handled it */                        \
            }                                                                                                \
            else                                                                                             \
                *(D) = (DT)(*(S));                                                                           \
        }                                                                                                    \
        else                                                                                                 \
            *(D) = (DT)(*(S));                                                                               \
    }
#define H5T_CONV_Xf_NOEX_CORE(STYPE, DTYPE, S, D, ST, DT, D_MIN, D_MAX)                                      \
    {                                                                                                        \
        if (*(S) > (ST)(D_MAX))                                                                              \
            *(D) = H5_GLUE3(H5T_NATIVE_, DTYPE, _POS_INF_g);                                                 \
        else {                                                                                               \
            intmax_t s_cast = (intmax_t)(*(S));                                                              \
            intmax_t d_cast = (intmax_t)(D_MAX);                                                             \
                                                                                                             \
            /* Check if source value would underflow destination. Do NOT do this                             \
             * by comparing against D_MIN casted to type ST here, as this will                               \
             * generally be undefined behavior (casting negative float value <= 1.0                          \
             * to integer) for all floating point types and some compilers optimize                          \
             * this in a way that causes unexpected behavior. Instead, grab the                              \
             * absolute value of the source value first, then compare it to D_MAX.                           \
             */                                                                                              \
            if (s_cast != INTMAX_MIN)                                                                        \
                s_cast = imaxabs(s_cast);                                                                    \
            else {                                                                                           \
                /* Handle two's complement integer representations where abs(INTMAX_MIN)                     \
                 * can't be represented. Other representations will fall here as well,                       \
                 * but this should be fine.                                                                  \
                 */                                                                                          \
                s_cast = INTMAX_MAX;                                                                         \
                d_cast -= 1;                                                                                 \
            }                                                                                                \
                                                                                                             \
            if (s_cast > d_cast)                                                                             \
                *(D) = H5_GLUE3(H5T_NATIVE_, DTYPE, _NEG_INF_g);                                             \
            else                                                                                             \
                *(D) = (DT)(*(S));                                                                           \
        }                                                                                                    \
    }

#define H5T_CONV_Xf(STYPE, DTYPE, ST, DT, D_MIN, D_MAX)                                                      \
    do {                                                                                                     \
        HDcompile_assert(sizeof(ST) >= sizeof(DT));                                                          \
        H5T_CONV(H5T_CONV_Xf, STYPE, DTYPE, ST, DT, D_MIN, D_MAX, Y)                                         \
    } while (0)

/* Since all "no exception" cores do the same thing (assign the value in the
 * source location to the destination location, using casting), use one "core"
 * to do them all.
 */
#ifndef H5_WANT_DCONV_EXCEPTION
#define H5T_CONV_NO_EXCEPT_CORE(STYPE, DTYPE, S, D, ST, DT, D_MIN, D_MAX)                                    \
    {                                                                                                        \
        *(D) = (DT)(*(S));                                                                                   \
    }
#endif /* H5_WANT_DCONV_EXCEPTION */

/* The main part of every integer hardware conversion macro */
#define H5T_CONV(GUTS, STYPE, DTYPE, ST, DT, D_MIN, D_MAX, PREC)                                             \
    {                                                                                                        \
        herr_t ret_value = SUCCEED; /* Return value         */                                               \
                                                                                                             \
        FUNC_ENTER_PACKAGE                                                                                   \
                                                                                                             \
        {                                                                                                    \
            size_t elmtno;              /*element number        */                                           \
            H5T_CONV_DECL_PREC(PREC)    /*declare precision variables, or not */                             \
            void   *src_buf;            /*'raw' source buffer        */                                      \
            void   *dst_buf;            /*'raw' destination buffer    */                                     \
            ST     *src, *s;            /*source buffer            */                                        \
            DT     *dst, *d;            /*destination buffer        */                                       \
            ST      src_aligned;        /*source aligned type        */                                      \
            DT      dst_aligned;        /*destination aligned type    */                                     \
            bool    s_mv, d_mv;         /*move data to align it?    */                                       \
            ssize_t s_stride, d_stride; /*src and dst strides        */                                      \
            size_t  safe;               /*how many elements are safe to process in each pass */              \
                                                                                                             \
            switch (cdata->command) {                                                                        \
                case H5T_CONV_INIT:                                                                          \
                    /* Sanity check and initialize statistics */                                             \
                    cdata->need_bkg = H5T_BKG_NO;                                                            \
                    if (NULL == st || NULL == dt)                                                            \
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "invalid datatype");                   \
                    if (st->shared->size != sizeof(ST) || dt->shared->size != sizeof(DT))                    \
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "disagreement about datatype size");   \
                    CI_ALLOC_PRIV                                                                            \
                    break;                                                                                   \
                                                                                                             \
                case H5T_CONV_FREE:                                                                          \
                    /* Print and free statistics */                                                          \
                    CI_PRINT_STATS(STYPE, DTYPE);                                                            \
                    CI_FREE_PRIV                                                                             \
                    break;                                                                                   \
                                                                                                             \
                case H5T_CONV_CONV:                                                                          \
                    if (NULL == st || NULL == dt)                                                            \
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "invalid datatype");                   \
                    if (NULL == conv_ctx)                                                                    \
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL,                                        \
                                    "invalid datatype conversion context pointer");                          \
                                                                                                             \
                    /* Initialize source & destination strides */                                            \
                    if (buf_stride) {                                                                        \
                        assert(buf_stride >= sizeof(ST));                                                    \
                        assert(buf_stride >= sizeof(DT));                                                    \
                        s_stride = d_stride = (ssize_t)buf_stride;                                           \
                    }                                                                                        \
                    else {                                                                                   \
                        s_stride = sizeof(ST);                                                               \
                        d_stride = sizeof(DT);                                                               \
                    }                                                                                        \
                                                                                                             \
                    /* Is alignment required for source or dest? */                                          \
                    s_mv = H5T_NATIVE_##STYPE##_ALIGN_g > 1 &&                                               \
                           ((size_t)buf % H5T_NATIVE_##STYPE##_ALIGN_g ||                                    \
                            /* Cray */ ((size_t)((ST *)buf) != (size_t)buf) ||                               \
                            (size_t)s_stride % H5T_NATIVE_##STYPE##_ALIGN_g);                                \
                    d_mv = H5T_NATIVE_##DTYPE##_ALIGN_g > 1 &&                                               \
                           ((size_t)buf % H5T_NATIVE_##DTYPE##_ALIGN_g ||                                    \
                            /* Cray */ ((size_t)((DT *)buf) != (size_t)buf) ||                               \
                            (size_t)d_stride % H5T_NATIVE_##DTYPE##_ALIGN_g);                                \
                    CI_INC_SRC(s_mv)                                                                         \
                    CI_INC_DST(d_mv)                                                                         \
                                                                                                             \
                    H5T_CONV_SET_PREC(PREC) /*init precision variables, or not */                            \
                                                                                                             \
                    /* The outer loop of the type conversion macro, controlling which */                     \
                    /* direction the buffer is walked */                                                     \
                    while (nelmts > 0) {                                                                     \
                        /* Check if we need to go backwards through the buffer */                            \
                        if (d_stride > s_stride) {                                                           \
                            /* Compute the number of "safe" destination elements at */                       \
                            /* the end of the buffer (Those which don't overlap with */                      \
                            /* any source elements at the beginning of the buffer) */                        \
                            safe = nelmts - (((nelmts * (size_t)s_stride) + (size_t)(d_stride - 1)) /        \
                                             (size_t)d_stride);                                              \
                                                                                                             \
                            /* If we're down to the last few elements, just wrap up */                       \
                            /* with a "real" reverse copy */                                                 \
                            if (safe < 2) {                                                                  \
                                src      = (ST *)(src_buf = (void *)((uint8_t *)buf +                        \
                                                                (nelmts - 1) * (size_t)s_stride));      \
                                dst      = (DT *)(dst_buf = (void *)((uint8_t *)buf +                        \
                                                                (nelmts - 1) * (size_t)d_stride));      \
                                s_stride = -s_stride;                                                        \
                                d_stride = -d_stride;                                                        \
                                                                                                             \
                                safe = nelmts;                                                               \
                            } /* end if */                                                                   \
                            else {                                                                           \
                                src = (ST *)(src_buf = (void *)((uint8_t *)buf +                             \
                                                                (nelmts - safe) * (size_t)s_stride));        \
                                dst = (DT *)(dst_buf = (void *)((uint8_t *)buf +                             \
                                                                (nelmts - safe) * (size_t)d_stride));        \
                            } /* end else */                                                                 \
                        }     /* end if */                                                                   \
                        else {                                                                               \
                            /* Single forward pass over all data */                                          \
                            src  = (ST *)(src_buf = buf);                                                    \
                            dst  = (DT *)(dst_buf = buf);                                                    \
                            safe = nelmts;                                                                   \
                        } /* end else */                                                                     \
                                                                                                             \
                        /* Perform loop over elements to convert */                                          \
                        if (s_mv && d_mv) {                                                                  \
                            /* Alignment is required for both source and dest */                             \
                            s = &src_aligned;                                                                \
                            H5T_CONV_LOOP_OUTER(PRE_SALIGN, PRE_DALIGN, POST_SALIGN, POST_DALIGN, GUTS,      \
                                                STYPE, DTYPE, s, d, ST, DT, D_MIN, D_MAX)                    \
                        }                                                                                    \
                        else if (s_mv) {                                                                     \
                            /* Alignment is required only for source */                                      \
                            s = &src_aligned;                                                                \
                            H5T_CONV_LOOP_OUTER(PRE_SALIGN, PRE_DNOALIGN, POST_SALIGN, POST_DNOALIGN, GUTS,  \
                                                STYPE, DTYPE, s, dst, ST, DT, D_MIN, D_MAX)                  \
                        }                                                                                    \
                        else if (d_mv) {                                                                     \
                            /* Alignment is required only for destination */                                 \
                            H5T_CONV_LOOP_OUTER(PRE_SNOALIGN, PRE_DALIGN, POST_SNOALIGN, POST_DALIGN, GUTS,  \
                                                STYPE, DTYPE, src, d, ST, DT, D_MIN, D_MAX)                  \
                        }                                                                                    \
                        else {                                                                               \
                            /* Alignment is not required for both source and destination */                  \
                            H5T_CONV_LOOP_OUTER(PRE_SNOALIGN, PRE_DNOALIGN, POST_SNOALIGN, POST_DNOALIGN,    \
                                                GUTS, STYPE, DTYPE, src, dst, ST, DT, D_MIN, D_MAX)          \
                        }                                                                                    \
                                                                                                             \
                        /* Decrement number of elements left to convert */                                   \
                        nelmts -= safe;                                                                      \
                    } /* end while */                                                                        \
                    break;                                                                                   \
                                                                                                             \
                default:                                                                                     \
                    HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unknown conversion command");          \
            }                                                                                                \
        }                                                                                                    \
                                                                                                             \
done:                                                                                                        \
        FUNC_LEAVE_NOAPI(ret_value)                                                                          \
    }

/* Declare the source & destination precision variables */
#define H5T_CONV_DECL_PREC(PREC) H5_GLUE(H5T_CONV_DECL_PREC_, PREC)

#define H5T_CONV_DECL_PREC_Y                                                                                 \
    size_t      sprec;  /*source precision        */                                                         \
    size_t      dprec;  /*destination precision        */                                                    \
    H5T_class_t tclass; /*datatype's class        */

#define H5T_CONV_DECL_PREC_N /*no precision variables        */

/* Initialize the source & destination precision variables */
#define H5T_CONV_SET_PREC(PREC) H5_GLUE(H5T_CONV_SET_PREC_, PREC)

#define H5T_CONV_SET_PREC_Y                                                                                  \
    /* Get source & destination precisions into a variable */                                                \
    tclass = st->shared->type;                                                                               \
    assert(tclass == H5T_INTEGER || tclass == H5T_FLOAT);                                                    \
    if (tclass == H5T_INTEGER)                                                                               \
        sprec = st->shared->u.atomic.prec;                                                                   \
    else                                                                                                     \
        sprec = 1 + st->shared->u.atomic.u.f.msize;                                                          \
    tclass = dt->shared->type;                                                                               \
    assert(tclass == H5T_INTEGER || tclass == H5T_FLOAT);                                                    \
    if (tclass == H5T_INTEGER)                                                                               \
        dprec = dt->shared->u.atomic.prec;                                                                   \
    else                                                                                                     \
        dprec = 1 + dt->shared->u.atomic.u.f.msize;

#define H5T_CONV_SET_PREC_N /*don't init precision variables */

/* Macro defining action on source data which needs to be aligned (before main action) */
#define H5T_CONV_LOOP_PRE_SALIGN(ST)                                                                         \
    {                                                                                                        \
        /* The uint8_t * cast is required to avoid tripping over undefined behavior.                         \
         *                                                                                                   \
         * The typed pointer arrives via a void pointer, which may have any alignment.                       \
         * We then cast it to a pointer to a type that is assumed to be aligned, which                       \
         * is undefined behavior (section 6.3.2.3 paragraph 7 of the C99 standard).                          \
         * In the past this hasn't caused many problems, but in some cases (e.g.                             \
         * converting long doubles on macOS), an optimizing compiler might do the                            \
         * wrong thing (in the macOS case, the conversion uses SSE, which has stricter                       \
         * requirements about alignment).                                                                    \
         */                                                                                                  \
        H5MM_memcpy(&src_aligned, (const uint8_t *)src, sizeof(ST));                                         \
    }

/* Macro defining action on source data which doesn't need to be aligned (before main action) */
#define H5T_CONV_LOOP_PRE_SNOALIGN(ST)                                                                       \
    {                                                                                                        \
    }

/* Macro defining action on destination data which needs to be aligned (before main action) */
#define H5T_CONV_LOOP_PRE_DALIGN(DT)                                                                         \
    {                                                                                                        \
        d = &dst_aligned;                                                                                    \
    }

/* Macro defining action on destination data which doesn't need to be aligned (before main action) */
#define H5T_CONV_LOOP_PRE_DNOALIGN(DT)                                                                       \
    {                                                                                                        \
    }

/* Macro defining action on source data which needs to be aligned (after main action) */
#define H5T_CONV_LOOP_POST_SALIGN(ST)                                                                        \
    {                                                                                                        \
    }

/* Macro defining action on source data which doesn't need to be aligned (after main action) */
#define H5T_CONV_LOOP_POST_SNOALIGN(ST)                                                                      \
    {                                                                                                        \
    }

/* Macro defining action on destination data which needs to be aligned (after main action) */
#define H5T_CONV_LOOP_POST_DALIGN(DT)                                                                        \
    {                                                                                                        \
        /* The uint8_t * cast is required to avoid tripping over undefined behavior.                         \
         *                                                                                                   \
         * The typed pointer arrives via a void pointer, which may have any alignment.                       \
         * We then cast it to a pointer to a type that is assumed to be aligned, which                       \
         * is undefined behavior (section 6.3.2.3 paragraph 7 of the C99 standard).                          \
         * In the past this hasn't caused many problems, but in some cases (e.g.                             \
         * converting long doubles on macOS), an optimizing compiler might do the                            \
         * wrong thing (in the macOS case, the conversion uses SSE, which has stricter                       \
         * requirements about alignment).                                                                    \
         */                                                                                                  \
        H5MM_memcpy((uint8_t *)dst, &dst_aligned, sizeof(DT));                                               \
    }

/* Macro defining action on destination data which doesn't need to be aligned (after main action) */
#define H5T_CONV_LOOP_POST_DNOALIGN(DT)                                                                      \
    {                                                                                                        \
    }

/* The outer wrapper for the type conversion loop, to check for an exception handling routine */
#define H5T_CONV_LOOP_OUTER(PRE_SALIGN_GUTS, PRE_DALIGN_GUTS, POST_SALIGN_GUTS, POST_DALIGN_GUTS, GUTS,      \
                            STYPE, DTYPE, S, D, ST, DT, D_MIN, D_MAX)                                        \
    if (conv_ctx->u.conv.cb_struct.func) {                                                                   \
        H5T_CONV_LOOP(PRE_SALIGN_GUTS, PRE_DALIGN_GUTS, POST_SALIGN_GUTS, POST_DALIGN_GUTS, GUTS, STYPE,     \
                      DTYPE, S, D, ST, DT, D_MIN, D_MAX)                                                     \
    }                                                                                                        \
    else {                                                                                                   \
        H5T_CONV_LOOP(PRE_SALIGN_GUTS, PRE_DALIGN_GUTS, POST_SALIGN_GUTS, POST_DALIGN_GUTS,                  \
                      H5_GLUE(GUTS, _NOEX), STYPE, DTYPE, S, D, ST, DT, D_MIN, D_MAX)                        \
    }

/* The inner loop of the type conversion macro, actually converting the elements */
#define H5T_CONV_LOOP(PRE_SALIGN_GUTS, PRE_DALIGN_GUTS, POST_SALIGN_GUTS, POST_DALIGN_GUTS, GUTS, STYPE,     \
                      DTYPE, S, D, ST, DT, D_MIN, D_MAX)                                                     \
    for (elmtno = 0; elmtno < safe; elmtno++) {                                                              \
        /* Handle source pre-alignment */                                                                    \
        H5_GLUE(H5T_CONV_LOOP_, PRE_SALIGN_GUTS)                                                             \
        (ST)                                                                                                 \
                                                                                                             \
            /* Handle destination pre-alignment */                                                           \
            H5_GLUE(H5T_CONV_LOOP_, PRE_DALIGN_GUTS)(DT)                                                     \
                                                                                                             \
            /* ... user-defined stuff here -- the conversion ... */                                          \
            H5T_CONV_LOOP_GUTS(GUTS, STYPE, DTYPE, S, D, ST, DT, D_MIN, D_MAX)                               \
                                                                                                             \
            /* Handle source post-alignment */                                                               \
            H5_GLUE(H5T_CONV_LOOP_, POST_SALIGN_GUTS)(ST)                                                    \
                                                                                                             \
            /* Handle destination post-alignment */                                                          \
            H5_GLUE(H5T_CONV_LOOP_, POST_DALIGN_GUTS)(DT)                                                    \
                                                                                                             \
            /* Advance pointers */                                                                           \
            src_buf = (void *)((uint8_t *)src_buf + s_stride);                                               \
        src         = (ST *)src_buf;                                                                         \
        dst_buf     = (void *)((uint8_t *)dst_buf + d_stride);                                               \
        dst         = (DT *)dst_buf;                                                                         \
    }

/* Macro to call the actual "guts" of the type conversion, or call the "no exception" guts */
#ifdef H5_WANT_DCONV_EXCEPTION
#define H5T_CONV_LOOP_GUTS(GUTS, STYPE, DTYPE, S, D, ST, DT, D_MIN, D_MAX)                                   \
    /* ... user-defined stuff here -- the conversion ... */                                                  \
    H5_GLUE(GUTS, _CORE)(STYPE, DTYPE, S, D, ST, DT, D_MIN, D_MAX)
#else /* H5_WANT_DCONV_EXCEPTION */
#define H5T_CONV_LOOP_GUTS(GUTS, STYPE, DTYPE, S, D, ST, DT, D_MIN, D_MAX)                                   \
    H5_GLUE(H5T_CONV_NO_EXCEPT, _CORE)(STYPE, DTYPE, S, D, ST, DT, D_MIN, D_MAX)
#endif /* H5_WANT_DCONV_EXCEPTION */

#ifdef H5T_DEBUG

/* Print alignment statistics */
#define CI_PRINT_STATS(STYPE, DTYPE)                                                                         \
    do {                                                                                                     \
        if (H5DEBUG(T) && ((H5T_conv_hw_t *)cdata->priv)->s_aligned) {                                       \
            fprintf(H5DEBUG(T), "      %zu src elements aligned on %zu-byte boundaries\n",                   \
                    ((H5T_conv_hw_t *)cdata->priv)->s_aligned, H5T_NATIVE_##STYPE##_ALIGN_g);                \
        }                                                                                                    \
        if (H5DEBUG(T) && ((H5T_conv_hw_t *)cdata->priv)->d_aligned) {                                       \
            fprintf(H5DEBUG(T), "      %zu dst elements aligned on %zu-byte boundaries\n",                   \
                    ((H5T_conv_hw_t *)cdata->priv)->d_aligned, H5T_NATIVE_##DTYPE##_ALIGN_g);                \
        }                                                                                                    \
    } while (0)

/* Allocate private alignment structure for atomic types */
#define CI_ALLOC_PRIV                                                                                        \
    if (NULL == (cdata->priv = H5MM_calloc(sizeof(H5T_conv_hw_t)))) {                                        \
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");                            \
    }

/* Free private alignment structure for atomic types */
#define CI_FREE_PRIV                                                                                         \
    if (cdata->priv != NULL)                                                                                 \
        cdata->priv = H5MM_xfree(cdata->priv);

/* Increment source alignment counter */
#define CI_INC_SRC(s)                                                                                        \
    if (s)                                                                                                   \
        ((H5T_conv_hw_t *)cdata->priv)->s_aligned += nelmts;

/* Increment destination alignment counter */
#define CI_INC_DST(d)                                                                                        \
    if (d)                                                                                                   \
        ((H5T_conv_hw_t *)cdata->priv)->d_aligned += nelmts;
#else                                /* H5T_DEBUG */
#define CI_PRINT_STATS(STYPE, DTYPE) /*void*/
#define CI_ALLOC_PRIV                cdata->priv = NULL;
#define CI_FREE_PRIV                 /* void */
#define CI_INC_SRC(s)                /* void */
#define CI_INC_DST(d)                /* void */
#endif                               /* H5T_DEBUG */

/* Swap two elements (I & J) of an array using a temporary variable */
#define H5_SWAP_BYTES(ARRAY, I, J)                                                                           \
    do {                                                                                                     \
        uint8_t _tmp;                                                                                        \
        _tmp     = ARRAY[I];                                                                                 \
        ARRAY[I] = ARRAY[J];                                                                                 \
        ARRAY[J] = _tmp;                                                                                     \
    } while (0)

/* Minimum size of variable-length conversion buffer */
#define H5T_VLEN_MIN_CONF_BUF_SIZE 4096

/******************/
/* Local Typedefs */
/******************/

/* Conversion data for H5T__conv_struct() */
typedef struct H5T_conv_struct_t {
    int              *src2dst;     /*mapping from src to dst member num */
    H5T_t           **src_memb;    /*source member datatypes */
    H5T_t           **dst_memb;    /*destination member datatypes */
    hid_t            *src_memb_id; /*source member type ID's */
    hid_t            *dst_memb_id; /*destination member type ID's         */
    H5T_path_t      **memb_path;   /*conversion path for each member    */
    H5T_subset_info_t subset_info; /*info related to compound subsets   */
    unsigned          src_nmembs;  /*needed by free function            */
} H5T_conv_struct_t;

/* Conversion data for H5T__conv_enum() */
typedef struct H5T_conv_enum_t {
    H5T_t   *src_copy; /* cached copy of source datatype */
    H5T_t   *dst_copy; /* cached copy of destination datatype */
    int      base;     /*lowest `in' value             */
    unsigned length;   /*num elements in arrays         */
    int     *src2dst;  /*map from src to dst index         */
} H5T_conv_enum_t;

/* Conversion data for H5T__conv_array() */
typedef struct H5T_conv_array_t {
    H5T_path_t *tpath; /* Conversion path for parent types */
} H5T_conv_array_t;

/* Conversion data for the hardware conversion functions */
typedef struct H5T_conv_hw_t {
    size_t s_aligned; /*number source elements aligned     */
    size_t d_aligned; /*number destination elements aligned*/
} H5T_conv_hw_t;

/********************/
/* Package Typedefs */
/********************/

/********************/
/* Local Prototypes */
/********************/

static herr_t H5T__reverse_order(uint8_t *rev, uint8_t *s, size_t size, H5T_order_t order);

/*********************/
/* Public Variables */
/*********************/

/*********************/
/* Package Variables */
/*********************/

/*****************************/
/* Library Private Variables */
/*****************************/

/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage pieces of vlen data */
H5FL_BLK_DEFINE_STATIC(vlen_seq);

/* Declare a free list to manage pieces of reference data */
H5FL_BLK_DEFINE_STATIC(ref_seq);

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_noop
 *
 * Purpose:    The no-op conversion.  The library knows about this
 *        conversion without it being registered.
 *
 * Return:     Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_noop(const H5T_t H5_ATTR_UNUSED *src, const H5T_t H5_ATTR_UNUSED *dst, H5T_cdata_t *cdata,
               const H5T_conv_ctx_t H5_ATTR_UNUSED *conv_ctx, size_t H5_ATTR_UNUSED nelmts,
               size_t H5_ATTR_UNUSED buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void H5_ATTR_UNUSED *buf,
               void H5_ATTR_UNUSED *background)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    switch (cdata->command) {
        case H5T_CONV_INIT:
            cdata->need_bkg = H5T_BKG_NO;
            break;

        case H5T_CONV_CONV:
            /* Nothing to convert */
            break;

        case H5T_CONV_FREE:
            break;

        default:
            HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unknown conversion command");
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5T__conv_noop() */

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_order_opt
 *
 * Purpose:    Convert one type to another when byte order is the only
 *        difference. This is the optimized version of H5T__conv_order()
 *              for a handful of different sizes.
 *
 * Note:    This is a soft conversion function.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_order_opt(const H5T_t *src, const H5T_t *dst, H5T_cdata_t *cdata,
                    const H5T_conv_ctx_t H5_ATTR_UNUSED *conv_ctx, size_t nelmts, size_t buf_stride,
                    size_t H5_ATTR_UNUSED bkg_stride, void *_buf, void H5_ATTR_UNUSED *background)
{
    uint8_t *buf = (uint8_t *)_buf;
    size_t   i;
    herr_t   ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    switch (cdata->command) {
        case H5T_CONV_INIT:
            /* Capability query */
            if (NULL == src || NULL == dst)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype");
            if (src->shared->size != dst->shared->size || 0 != src->shared->u.atomic.offset ||
                0 != dst->shared->u.atomic.offset)
                HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "conversion not supported");
            if ((src->shared->type == H5T_REFERENCE && dst->shared->type != H5T_REFERENCE) ||
                (dst->shared->type == H5T_REFERENCE && src->shared->type != H5T_REFERENCE))
                HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "conversion not supported");
            if (src->shared->type != H5T_REFERENCE && !((H5T_ORDER_BE == src->shared->u.atomic.order &&
                                                         H5T_ORDER_LE == dst->shared->u.atomic.order) ||
                                                        (H5T_ORDER_LE == src->shared->u.atomic.order &&
                                                         H5T_ORDER_BE == dst->shared->u.atomic.order)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "conversion not supported");
            if (src->shared->size != 1 && src->shared->size != 2 && src->shared->size != 4 &&
                src->shared->size != 8 && src->shared->size != 16)
                HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "conversion not supported");
            switch (src->shared->type) {
                case H5T_INTEGER:
                case H5T_BITFIELD:
                case H5T_REFERENCE:
                    /* nothing to check */
                    break;

                case H5T_FLOAT:
                    if (src->shared->u.atomic.u.f.sign != dst->shared->u.atomic.u.f.sign ||
                        src->shared->u.atomic.u.f.epos != dst->shared->u.atomic.u.f.epos ||
                        src->shared->u.atomic.u.f.esize != dst->shared->u.atomic.u.f.esize ||
                        src->shared->u.atomic.u.f.ebias != dst->shared->u.atomic.u.f.ebias ||
                        src->shared->u.atomic.u.f.mpos != dst->shared->u.atomic.u.f.mpos ||
                        src->shared->u.atomic.u.f.msize != dst->shared->u.atomic.u.f.msize ||
                        src->shared->u.atomic.u.f.norm != dst->shared->u.atomic.u.f.norm ||
                        src->shared->u.atomic.u.f.pad != dst->shared->u.atomic.u.f.pad)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "conversion not supported");
                    break;

                case H5T_NO_CLASS:
                case H5T_TIME:
                case H5T_STRING:
                case H5T_OPAQUE:
                case H5T_COMPOUND:
                case H5T_ENUM:
                case H5T_VLEN:
                case H5T_ARRAY:
                case H5T_NCLASSES:
                default:
                    HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "conversion not supported");
            }
            cdata->need_bkg = H5T_BKG_NO;
            break;

        case H5T_CONV_CONV:
            /* The conversion */
            if (NULL == src || NULL == dst)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype");

            /* Check for "no op" reference conversion */
            if (src->shared->type == H5T_REFERENCE) {
                /* Sanity check */
                if (dst->shared->type != H5T_REFERENCE)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "not a H5T_REFERENCE datatype");

                /* Check if we are on a little-endian machine (the order that
                 * the addresses in the file must be) and just get out now, there
                 * is no need to convert the object reference.  Yes, this is
                 * icky and non-portable, but I can't think of a better way to
                 * support allowing the objno in the H5O_info_t struct and the
                 * hobj_ref_t type to be compared directly without introducing a
                 * "native" hobj_ref_t datatype and I think that would break a
                 * lot of existing programs.  -QAK
                 */
                if (H5T_native_order_g == H5T_ORDER_LE)
                    break;
            } /* end if */

            buf_stride = buf_stride ? buf_stride : src->shared->size;
            switch (src->shared->size) {
                case 1:
                    /*no-op*/
                    break;

                case 2:
                    for (/*void*/; nelmts >= 20; nelmts -= 20) {
                        H5_SWAP_BYTES(buf, 0, 1); /*  0 */
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 1); /*  1 */
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 1); /*  2 */
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 1); /*  3 */
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 1); /*  4 */
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 1); /*  5 */
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 1); /*  6 */
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 1); /*  7 */
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 1); /*  8 */
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 1); /*  9 */
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 1); /* 10 */
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 1); /* 11 */
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 1); /* 12 */
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 1); /* 13 */
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 1); /* 14 */
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 1); /* 15 */
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 1); /* 16 */
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 1); /* 17 */
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 1); /* 18 */
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 1); /* 19 */
                        buf += buf_stride;
                    } /* end for */
                    for (i = 0; i < nelmts; i++, buf += buf_stride)
                        H5_SWAP_BYTES(buf, 0, 1);
                    break;

                case 4:
                    for (/*void*/; nelmts >= 20; nelmts -= 20) {
                        H5_SWAP_BYTES(buf, 0, 3); /*  0 */
                        H5_SWAP_BYTES(buf, 1, 2);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 3); /*  1 */
                        H5_SWAP_BYTES(buf, 1, 2);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 3); /*  2 */
                        H5_SWAP_BYTES(buf, 1, 2);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 3); /*  3 */
                        H5_SWAP_BYTES(buf, 1, 2);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 3); /*  4 */
                        H5_SWAP_BYTES(buf, 1, 2);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 3); /*  5 */
                        H5_SWAP_BYTES(buf, 1, 2);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 3); /*  6 */
                        H5_SWAP_BYTES(buf, 1, 2);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 3); /*  7 */
                        H5_SWAP_BYTES(buf, 1, 2);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 3); /*  8 */
                        H5_SWAP_BYTES(buf, 1, 2);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 3); /*  9 */
                        H5_SWAP_BYTES(buf, 1, 2);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 3); /* 10 */
                        H5_SWAP_BYTES(buf, 1, 2);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 3); /* 11 */
                        H5_SWAP_BYTES(buf, 1, 2);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 3); /* 12 */
                        H5_SWAP_BYTES(buf, 1, 2);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 3); /* 13 */
                        H5_SWAP_BYTES(buf, 1, 2);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 3); /* 14 */
                        H5_SWAP_BYTES(buf, 1, 2);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 3); /* 15 */
                        H5_SWAP_BYTES(buf, 1, 2);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 3); /* 16 */
                        H5_SWAP_BYTES(buf, 1, 2);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 3); /* 17 */
                        H5_SWAP_BYTES(buf, 1, 2);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 3); /* 18 */
                        H5_SWAP_BYTES(buf, 1, 2);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 3); /* 19 */
                        H5_SWAP_BYTES(buf, 1, 2);
                        buf += buf_stride;
                    } /* end for */
                    for (i = 0; i < nelmts; i++, buf += buf_stride) {
                        H5_SWAP_BYTES(buf, 0, 3);
                        H5_SWAP_BYTES(buf, 1, 2);
                    } /* end for */
                    break;

                case 8:
                    for (/*void*/; nelmts >= 10; nelmts -= 10) {
                        H5_SWAP_BYTES(buf, 0, 7); /*  0 */
                        H5_SWAP_BYTES(buf, 1, 6);
                        H5_SWAP_BYTES(buf, 2, 5);
                        H5_SWAP_BYTES(buf, 3, 4);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 7); /*  1 */
                        H5_SWAP_BYTES(buf, 1, 6);
                        H5_SWAP_BYTES(buf, 2, 5);
                        H5_SWAP_BYTES(buf, 3, 4);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 7); /*  2 */
                        H5_SWAP_BYTES(buf, 1, 6);
                        H5_SWAP_BYTES(buf, 2, 5);
                        H5_SWAP_BYTES(buf, 3, 4);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 7); /*  3 */
                        H5_SWAP_BYTES(buf, 1, 6);
                        H5_SWAP_BYTES(buf, 2, 5);
                        H5_SWAP_BYTES(buf, 3, 4);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 7); /*  4 */
                        H5_SWAP_BYTES(buf, 1, 6);
                        H5_SWAP_BYTES(buf, 2, 5);
                        H5_SWAP_BYTES(buf, 3, 4);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 7); /*  5 */
                        H5_SWAP_BYTES(buf, 1, 6);
                        H5_SWAP_BYTES(buf, 2, 5);
                        H5_SWAP_BYTES(buf, 3, 4);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 7); /*  6 */
                        H5_SWAP_BYTES(buf, 1, 6);
                        H5_SWAP_BYTES(buf, 2, 5);
                        H5_SWAP_BYTES(buf, 3, 4);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 7); /*  7 */
                        H5_SWAP_BYTES(buf, 1, 6);
                        H5_SWAP_BYTES(buf, 2, 5);
                        H5_SWAP_BYTES(buf, 3, 4);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 7); /*  8 */
                        H5_SWAP_BYTES(buf, 1, 6);
                        H5_SWAP_BYTES(buf, 2, 5);
                        H5_SWAP_BYTES(buf, 3, 4);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 7); /*  9 */
                        H5_SWAP_BYTES(buf, 1, 6);
                        H5_SWAP_BYTES(buf, 2, 5);
                        H5_SWAP_BYTES(buf, 3, 4);
                        buf += buf_stride;
                    } /* end for */
                    for (i = 0; i < nelmts; i++, buf += buf_stride) {
                        H5_SWAP_BYTES(buf, 0, 7);
                        H5_SWAP_BYTES(buf, 1, 6);
                        H5_SWAP_BYTES(buf, 2, 5);
                        H5_SWAP_BYTES(buf, 3, 4);
                    } /* end for */
                    break;

                case 16:
                    for (/*void*/; nelmts >= 10; nelmts -= 10) {
                        H5_SWAP_BYTES(buf, 0, 15); /*  0 */
                        H5_SWAP_BYTES(buf, 1, 14);
                        H5_SWAP_BYTES(buf, 2, 13);
                        H5_SWAP_BYTES(buf, 3, 12);
                        H5_SWAP_BYTES(buf, 4, 11);
                        H5_SWAP_BYTES(buf, 5, 10);
                        H5_SWAP_BYTES(buf, 6, 9);
                        H5_SWAP_BYTES(buf, 7, 8);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 15); /*  1 */
                        H5_SWAP_BYTES(buf, 1, 14);
                        H5_SWAP_BYTES(buf, 2, 13);
                        H5_SWAP_BYTES(buf, 3, 12);
                        H5_SWAP_BYTES(buf, 4, 11);
                        H5_SWAP_BYTES(buf, 5, 10);
                        H5_SWAP_BYTES(buf, 6, 9);
                        H5_SWAP_BYTES(buf, 7, 8);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 15); /*  2 */
                        H5_SWAP_BYTES(buf, 1, 14);
                        H5_SWAP_BYTES(buf, 2, 13);
                        H5_SWAP_BYTES(buf, 3, 12);
                        H5_SWAP_BYTES(buf, 4, 11);
                        H5_SWAP_BYTES(buf, 5, 10);
                        H5_SWAP_BYTES(buf, 6, 9);
                        H5_SWAP_BYTES(buf, 7, 8);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 15); /*  3 */
                        H5_SWAP_BYTES(buf, 1, 14);
                        H5_SWAP_BYTES(buf, 2, 13);
                        H5_SWAP_BYTES(buf, 3, 12);
                        H5_SWAP_BYTES(buf, 4, 11);
                        H5_SWAP_BYTES(buf, 5, 10);
                        H5_SWAP_BYTES(buf, 6, 9);
                        H5_SWAP_BYTES(buf, 7, 8);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 15); /*  4 */
                        H5_SWAP_BYTES(buf, 1, 14);
                        H5_SWAP_BYTES(buf, 2, 13);
                        H5_SWAP_BYTES(buf, 3, 12);
                        H5_SWAP_BYTES(buf, 4, 11);
                        H5_SWAP_BYTES(buf, 5, 10);
                        H5_SWAP_BYTES(buf, 6, 9);
                        H5_SWAP_BYTES(buf, 7, 8);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 15); /*  5 */
                        H5_SWAP_BYTES(buf, 1, 14);
                        H5_SWAP_BYTES(buf, 2, 13);
                        H5_SWAP_BYTES(buf, 3, 12);
                        H5_SWAP_BYTES(buf, 4, 11);
                        H5_SWAP_BYTES(buf, 5, 10);
                        H5_SWAP_BYTES(buf, 6, 9);
                        H5_SWAP_BYTES(buf, 7, 8);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 15); /*  6 */
                        H5_SWAP_BYTES(buf, 1, 14);
                        H5_SWAP_BYTES(buf, 2, 13);
                        H5_SWAP_BYTES(buf, 3, 12);
                        H5_SWAP_BYTES(buf, 4, 11);
                        H5_SWAP_BYTES(buf, 5, 10);
                        H5_SWAP_BYTES(buf, 6, 9);
                        H5_SWAP_BYTES(buf, 7, 8);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 15); /*  7 */
                        H5_SWAP_BYTES(buf, 1, 14);
                        H5_SWAP_BYTES(buf, 2, 13);
                        H5_SWAP_BYTES(buf, 3, 12);
                        H5_SWAP_BYTES(buf, 4, 11);
                        H5_SWAP_BYTES(buf, 5, 10);
                        H5_SWAP_BYTES(buf, 6, 9);
                        H5_SWAP_BYTES(buf, 7, 8);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 15); /*  8 */
                        H5_SWAP_BYTES(buf, 1, 14);
                        H5_SWAP_BYTES(buf, 2, 13);
                        H5_SWAP_BYTES(buf, 3, 12);
                        H5_SWAP_BYTES(buf, 4, 11);
                        H5_SWAP_BYTES(buf, 5, 10);
                        H5_SWAP_BYTES(buf, 6, 9);
                        H5_SWAP_BYTES(buf, 7, 8);
                        buf += buf_stride;
                        H5_SWAP_BYTES(buf, 0, 15); /*  9 */
                        H5_SWAP_BYTES(buf, 1, 14);
                        H5_SWAP_BYTES(buf, 2, 13);
                        H5_SWAP_BYTES(buf, 3, 12);
                        H5_SWAP_BYTES(buf, 4, 11);
                        H5_SWAP_BYTES(buf, 5, 10);
                        H5_SWAP_BYTES(buf, 6, 9);
                        H5_SWAP_BYTES(buf, 7, 8);
                        buf += buf_stride;
                    } /* end for */
                    for (i = 0; i < nelmts; i++, buf += buf_stride) {
                        H5_SWAP_BYTES(buf, 0, 15);
                        H5_SWAP_BYTES(buf, 1, 14);
                        H5_SWAP_BYTES(buf, 2, 13);
                        H5_SWAP_BYTES(buf, 3, 12);
                        H5_SWAP_BYTES(buf, 4, 11);
                        H5_SWAP_BYTES(buf, 5, 10);
                        H5_SWAP_BYTES(buf, 6, 9);
                        H5_SWAP_BYTES(buf, 7, 8);
                    } /* end for */
                    break;

                default:
                    HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "invalid conversion size");
            } /* end switch */
            break;

        case H5T_CONV_FREE:
            /* Free private data */
            break;

        default:
            HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unknown conversion command");
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5T__conv_order_opt() */

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_order
 *
 * Purpose:    Convert one type to another when byte order is the only
 *        difference.
 *
 * Note:    This is a soft conversion function.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_order(const H5T_t *src, const H5T_t *dst, H5T_cdata_t *cdata,
                const H5T_conv_ctx_t H5_ATTR_UNUSED *conv_ctx, size_t nelmts, size_t buf_stride,
                size_t H5_ATTR_UNUSED bkg_stride, void *_buf, void H5_ATTR_UNUSED *background)
{
    uint8_t *buf = (uint8_t *)_buf;
    size_t   i;
    size_t   j, md;
    herr_t   ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    switch (cdata->command) {
        case H5T_CONV_INIT:
            /* Capability query */
            if (NULL == src || NULL == dst)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype");
            if (src->shared->size != dst->shared->size || 0 != src->shared->u.atomic.offset ||
                0 != dst->shared->u.atomic.offset ||
                !((H5T_ORDER_BE == src->shared->u.atomic.order &&
                   H5T_ORDER_LE == dst->shared->u.atomic.order) ||
                  (H5T_ORDER_LE == src->shared->u.atomic.order &&
                   H5T_ORDER_BE == dst->shared->u.atomic.order)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "conversion not supported");
            switch (src->shared->type) {
                case H5T_INTEGER:
                case H5T_BITFIELD:
                    /* nothing to check */
                    break;

                case H5T_FLOAT:
                    if (src->shared->u.atomic.u.f.sign != dst->shared->u.atomic.u.f.sign ||
                        src->shared->u.atomic.u.f.epos != dst->shared->u.atomic.u.f.epos ||
                        src->shared->u.atomic.u.f.esize != dst->shared->u.atomic.u.f.esize ||
                        src->shared->u.atomic.u.f.ebias != dst->shared->u.atomic.u.f.ebias ||
                        src->shared->u.atomic.u.f.mpos != dst->shared->u.atomic.u.f.mpos ||
                        src->shared->u.atomic.u.f.msize != dst->shared->u.atomic.u.f.msize ||
                        src->shared->u.atomic.u.f.norm != dst->shared->u.atomic.u.f.norm ||
                        src->shared->u.atomic.u.f.pad != dst->shared->u.atomic.u.f.pad) {
                        HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "conversion not supported");
                    } /* end if */
                    break;

                case H5T_NO_CLASS:
                case H5T_TIME:
                case H5T_STRING:
                case H5T_OPAQUE:
                case H5T_COMPOUND:
                case H5T_REFERENCE:
                case H5T_ENUM:
                case H5T_VLEN:
                case H5T_ARRAY:
                case H5T_NCLASSES:
                default:
                    HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "conversion not supported");
            } /* end switch */
            cdata->need_bkg = H5T_BKG_NO;
            break;

        case H5T_CONV_CONV:
            /* The conversion */
            if (NULL == src)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype");

            buf_stride = buf_stride ? buf_stride : src->shared->size;
            md         = src->shared->size / 2;
            for (i = 0; i < nelmts; i++, buf += buf_stride)
                for (j = 0; j < md; j++)
                    H5_SWAP_BYTES(buf, j, src->shared->size - (j + 1));
            break;

        case H5T_CONV_FREE:
            /* Free private data */
            break;

        default:
            HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unknown conversion command");
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5T__conv_order() */

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_b_b
 *
 * Purpose:    Convert from one bitfield to any other bitfield.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_b_b(const H5T_t *src, const H5T_t *dst, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
              size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *_buf,
              void H5_ATTR_UNUSED *background)
{
    uint8_t       *buf = (uint8_t *)_buf;
    ssize_t        direction;       /*direction of traversal    */
    size_t         elmtno;          /*element number        */
    size_t         olap;            /*num overlapping elements    */
    size_t         half_size;       /*1/2 of total size for swapping*/
    uint8_t       *s, *sp, *d, *dp; /*source and dest traversal ptrs*/
    uint8_t        dbuf[256] = {0}; /*temp destination buffer    */
    size_t         msb_pad_offset;  /*offset for dest MSB padding    */
    size_t         i;
    uint8_t       *src_rev = NULL;      /*order-reversed source buffer  */
    H5T_conv_ret_t except_ret;          /*return of callback function   */
    bool           reverse;             /*if reverse the order of destination        */
    herr_t         ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    switch (cdata->command) {
        case H5T_CONV_INIT:
            /* Capability query */
            if (NULL == src || NULL == dst)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype");
            if (H5T_ORDER_LE != src->shared->u.atomic.order && H5T_ORDER_BE != src->shared->u.atomic.order)
                HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unsupported byte order");
            if (H5T_ORDER_LE != dst->shared->u.atomic.order && H5T_ORDER_BE != dst->shared->u.atomic.order)
                HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unsupported byte order");
            cdata->need_bkg = H5T_BKG_NO;
            break;

        case H5T_CONV_FREE:
            break;

        case H5T_CONV_CONV:
            if (NULL == src || NULL == dst)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype");
            if (NULL == conv_ctx)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid datatype conversion context pointer");

            /*
             * Do we process the values from beginning to end or vice versa? Also,
             * how many of the elements have the source and destination areas
             * overlapping?
             */
            if (src->shared->size == dst->shared->size || buf_stride) {
                sp = dp   = (uint8_t *)buf;
                direction = 1;
                olap      = nelmts;
            }
            else if (src->shared->size >= dst->shared->size) {
                double olap_d =
                    ceil((double)(dst->shared->size) / (double)(src->shared->size - dst->shared->size));

                olap = (size_t)olap_d;
                sp = dp   = (uint8_t *)buf;
                direction = 1;
            }
            else {
                double olap_d =
                    ceil((double)(src->shared->size) / (double)(dst->shared->size - src->shared->size));
                olap      = (size_t)olap_d;
                sp        = (uint8_t *)buf + (nelmts - 1) * src->shared->size;
                dp        = (uint8_t *)buf + (nelmts - 1) * dst->shared->size;
                direction = -1;
            }

            /* Allocate space for order-reversed source buffer */
            src_rev = (uint8_t *)H5MM_calloc(src->shared->size);

            /* The conversion loop */
            H5_CHECK_OVERFLOW(buf_stride, size_t, ssize_t);
            H5_CHECK_OVERFLOW(src->shared->size, size_t, ssize_t);
            H5_CHECK_OVERFLOW(dst->shared->size, size_t, ssize_t);
            for (elmtno = 0; elmtno < nelmts; elmtno++) {

                /*
                 * If the source and destination buffers overlap then use a
                 * temporary buffer for the destination.
                 */
                if (direction > 0) {
                    s = sp;
                    d = elmtno < olap ? dbuf : dp;
                } /* end if */
                else {
                    s = sp;
                    d = (elmtno + olap) >= nelmts ? dbuf : dp;
                } /* end else */
#ifndef NDEBUG
                /* I don't quite trust the overlap calculations yet --rpm */
                if (d == dbuf)
                    assert((dp >= sp && dp < sp + src->shared->size) ||
                           (sp >= dp && sp < dp + dst->shared->size));
                else
                    assert((dp < sp && dp + dst->shared->size <= sp) ||
                           (sp < dp && sp + src->shared->size <= dp));
#endif

                /*
                 * Put the data in little endian order so our loops aren't so
                 * complicated.  We'll do all the conversion stuff assuming
                 * little endian and then we'll fix the order at the end.
                 */
                if (H5T_ORDER_BE == src->shared->u.atomic.order) {
                    half_size = src->shared->size / 2;
                    for (i = 0; i < half_size; i++) {
                        uint8_t tmp                    = s[src->shared->size - (i + 1)];
                        s[src->shared->size - (i + 1)] = s[i];
                        s[i]                           = tmp;
                    } /* end for */
                }     /* end if */

                /* Initiate these variables */
                except_ret = H5T_CONV_UNHANDLED;
                reverse    = true;

                /*
                 * Copy the significant part of the value. If the source is larger
                 * than the destination then invoke the overflow function or copy
                 * as many bits as possible. Zero extra bits in the destination.
                 */
                if (src->shared->u.atomic.prec > dst->shared->u.atomic.prec) {
                    /*overflow*/
                    if (conv_ctx->u.conv.cb_struct.func) { /*If user's exception handler is present, use it*/
                        H5T__reverse_order(src_rev, s, src->shared->size,
                                           src->shared->u.atomic.order); /*reverse order first*/
                        except_ret = (conv_ctx->u.conv.cb_struct.func)(
                            H5T_CONV_EXCEPT_RANGE_HI, conv_ctx->u.conv.src_type_id,
                            conv_ctx->u.conv.dst_type_id, src_rev, d, conv_ctx->u.conv.cb_struct.user_data);
                    } /* end if */

                    if (except_ret == H5T_CONV_UNHANDLED) {
                        H5T__bit_copy(d, dst->shared->u.atomic.offset, s, src->shared->u.atomic.offset,
                                      dst->shared->u.atomic.prec);
                    }
                    else if (except_ret == H5T_CONV_ABORT)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception");
                    else if (except_ret == H5T_CONV_HANDLED)
                        /*Don't reverse because user handles it*/
                        reverse = false;
                }
                else {
                    H5T__bit_copy(d, dst->shared->u.atomic.offset, s, src->shared->u.atomic.offset,
                                  src->shared->u.atomic.prec);
                    H5T__bit_set(d, dst->shared->u.atomic.offset + src->shared->u.atomic.prec,
                                 dst->shared->u.atomic.prec - src->shared->u.atomic.prec, false);
                }

                /*
                 * Fill the destination padding areas.
                 */
                switch (dst->shared->u.atomic.lsb_pad) {
                    case H5T_PAD_ZERO:
                        H5T__bit_set(d, (size_t)0, dst->shared->u.atomic.offset, false);
                        break;

                    case H5T_PAD_ONE:
                        H5T__bit_set(d, (size_t)0, dst->shared->u.atomic.offset, true);
                        break;

                    case H5T_PAD_ERROR:
                    case H5T_PAD_BACKGROUND:
                    case H5T_NPAD:
                    default:
                        HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unsupported LSB padding");
                } /* end switch */
                msb_pad_offset = dst->shared->u.atomic.offset + dst->shared->u.atomic.prec;
                switch (dst->shared->u.atomic.msb_pad) {
                    case H5T_PAD_ZERO:
                        H5T__bit_set(d, msb_pad_offset, 8 * dst->shared->size - msb_pad_offset, false);
                        break;

                    case H5T_PAD_ONE:
                        H5T__bit_set(d, msb_pad_offset, 8 * dst->shared->size - msb_pad_offset, true);
                        break;

                    case H5T_PAD_ERROR:
                    case H5T_PAD_BACKGROUND:
                    case H5T_NPAD:
                    default:
                        HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unsupported MSB padding");
                } /* end switch */

                /*
                 * Put the destination in the correct byte order.  See note at
                 * beginning of loop.
                 */
                if (H5T_ORDER_BE == dst->shared->u.atomic.order && reverse) {
                    half_size = dst->shared->size / 2;
                    for (i = 0; i < half_size; i++) {
                        uint8_t tmp                    = d[dst->shared->size - (i + 1)];
                        d[dst->shared->size - (i + 1)] = d[i];
                        d[i]                           = tmp;
                    } /* end for */
                }     /* end if */

                /*
                 * If we had used a temporary buffer for the destination then we
                 * should copy the value to the true destination buffer.
                 */
                if (d == dbuf)
                    H5MM_memcpy(dp, d, dst->shared->size);
                if (buf_stride) {
                    sp += direction *
                          (ssize_t)buf_stride; /* Note that cast is checked with H5_CHECK_OVERFLOW, above */
                    dp += direction *
                          (ssize_t)buf_stride; /* Note that cast is checked with H5_CHECK_OVERFLOW, above */
                }                              /* end if */
                else {
                    sp += direction *
                          (ssize_t)
                              src->shared->size; /* Note that cast is checked with H5_CHECK_OVERFLOW, above */
                    dp += direction *
                          (ssize_t)
                              dst->shared->size; /* Note that cast is checked with H5_CHECK_OVERFLOW, above */
                }                                /* end else */
            }                                    /* end for */

            break;

        default:
            HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unknown conversion command");
    } /* end switch */

done:
    if (src_rev)
        H5MM_free(src_rev);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5T__conv_b_b() */

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_struct_free
 *
 * Purpose:     Free the private data structure used by the compound
 *              conversion functions.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5T__conv_struct_free(H5T_conv_struct_t *priv)
{
    int    *src2dst     = priv->src2dst;
    H5T_t **src_memb    = priv->src_memb;
    H5T_t **dst_memb    = priv->dst_memb;
    hid_t  *src_memb_id = priv->src_memb_id;
    hid_t  *dst_memb_id = priv->dst_memb_id;
    herr_t  ret_value   = SUCCEED;

    FUNC_ENTER_PACKAGE_NOERR

    for (unsigned i = 0; i < priv->src_nmembs; i++)
        if (src2dst[i] >= 0) {
            if (src_memb_id[i] >= 0) {
                if (H5I_dec_ref(src_memb_id[i]) < 0)
                    ret_value = FAIL; /* set return value, but keep going */
                src_memb_id[i] = H5I_INVALID_HID;
                src_memb[i]    = NULL;
            }
            else {
                if (H5T_close(src_memb[i]) < 0)
                    ret_value = FAIL; /* set return value, but keep going */
                src_memb[i] = NULL;
            }
            if (dst_memb_id[src2dst[i]] >= 0) {
                if (H5I_dec_ref(dst_memb_id[src2dst[i]]) < 0)
                    ret_value = FAIL; /* set return value, but keep going */
                dst_memb_id[src2dst[i]] = H5I_INVALID_HID;
                dst_memb[src2dst[i]]    = NULL;
            }
            else {
                if (H5T_close(dst_memb[src2dst[i]]) < 0)
                    ret_value = FAIL; /* set return value, but keep going */
                dst_memb[src2dst[i]] = NULL;
            }
        } /* end if */

    H5MM_xfree(src2dst);
    H5MM_xfree(src_memb);
    H5MM_xfree(dst_memb);
    H5MM_xfree(src_memb_id);
    H5MM_xfree(dst_memb_id);

    H5MM_xfree(priv->memb_path);
    H5MM_xfree(priv);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5T__conv_struct_free() */

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_struct_init
 *
 * Purpose:    Initialize the `priv' field of `cdata' with conversion
 *        information that is relatively constant.  If `priv' is
 *        already initialized then the member conversion functions
 *        are recalculated.
 *
 *        Priv fields are indexed by source member number or
 *        destination member number depending on whether the field
 *        contains information about the source datatype or the
 *        destination datatype (fields that contains the same
 *        information for both source and destination are indexed by
 *        source member number).  The src2dst[] priv array maps source
 *        member numbers to destination member numbers, but if the
 *        source member doesn't have a corresponding destination member
 *        then the src2dst[i]=-1.
 *
 *              Special optimization case when the source and destination
 *              members are a subset of each other, and the order is the same,
 *              and no conversion is needed.  For example:
 *                  struct source {            struct destination {
 *                      TYPE1 A;      -->          TYPE1 A;
 *                      TYPE2 B;      -->          TYPE2 B;
 *                      TYPE3 C;      -->          TYPE3 C;
 *                  };                             TYPE4 D;
 *                                                 TYPE5 E;
 *                                             };
 *              or
 *                  struct destination {       struct source {
 *                      TYPE1 A;      <--          TYPE1 A;
 *                      TYPE2 B;      <--          TYPE2 B;
 *                      TYPE3 C;      <--          TYPE3 C;
 *                  };                             TYPE4 D;
 *                                                 TYPE5 E;
 *                                             };
 *              The optimization is simply moving data to the appropriate
 *              places in the buffer.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5T__conv_struct_init(const H5T_t *src, const H5T_t *dst, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx)
{
    H5T_conv_struct_t *priv    = (H5T_conv_struct_t *)(cdata->priv);
    int               *src2dst = NULL;
    unsigned           src_nmembs, dst_nmembs;
    unsigned           i, j;
    herr_t             ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    src_nmembs = src->shared->u.compnd.nmembs;
    dst_nmembs = dst->shared->u.compnd.nmembs;

    if (!priv) {
        /*
         * Allocate private data structure and arrays.
         */
        if (NULL == (priv = (H5T_conv_struct_t *)(cdata->priv = H5MM_calloc(sizeof(H5T_conv_struct_t)))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "couldn't allocate private conversion data");
        if (NULL == (priv->src2dst = (int *)H5MM_malloc(src_nmembs * sizeof(int))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                        "couldn't allocate source to destination member mapping array");
        if (NULL == (priv->src_memb = (H5T_t **)H5MM_malloc(src_nmembs * sizeof(H5T_t *))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                        "couldn't allocate source compound member datatype array");
        if (NULL == (priv->dst_memb = (H5T_t **)H5MM_malloc(dst_nmembs * sizeof(H5T_t *))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                        "couldn't allocate destination compound member datatype array");

        /* Allocate and initialize arrays for datatype IDs */
        if (NULL == (priv->src_memb_id = (hid_t *)H5MM_malloc(src_nmembs * sizeof(hid_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                        "couldn't allocate source compound member datatype ID array");
        for (i = 0; i < src_nmembs; i++)
            priv->src_memb_id[i] = H5I_INVALID_HID;

        if (NULL == (priv->dst_memb_id = (hid_t *)H5MM_malloc(dst_nmembs * sizeof(hid_t))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                        "couldn't allocate destination compound member datatype ID array");
        for (i = 0; i < dst_nmembs; i++)
            priv->dst_memb_id[i] = H5I_INVALID_HID;

        src2dst          = priv->src2dst;
        priv->src_nmembs = src_nmembs;

        /* The flag of special optimization to indicate if source members and destination
         * members are a subset of each other.  Initialize it to false */
        priv->subset_info.subset    = H5T_SUBSET_FALSE;
        priv->subset_info.copy_size = 0;

        /*
         * Ensure that members are sorted.
         */
        H5T__sort_value(src, NULL);
        H5T__sort_value(dst, NULL);

        /*
         * Build a mapping from source member number to destination member
         * number. If some source member is not a destination member then that
         * mapping element will be negative.  Also create atoms for each
         * source and destination member datatype if necessary.
         */
        for (i = 0; i < src_nmembs; i++) {
            src2dst[i] = -1;
            for (j = 0; j < dst_nmembs; j++) {
                if (!strcmp(src->shared->u.compnd.memb[i].name, dst->shared->u.compnd.memb[j].name)) {
                    H5_CHECKED_ASSIGN(src2dst[i], int, j, unsigned);
                    break;
                } /* end if */
            }     /* end for */
            if (src2dst[i] >= 0) {
                H5T_t *type;

                if (NULL == (type = H5T_copy(src->shared->u.compnd.memb[i].type, H5T_COPY_ALL)))
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCOPY, FAIL,
                                "can't copy source compound member datatype");
                priv->src_memb[i] = type;

                if (NULL == (type = H5T_copy(dst->shared->u.compnd.memb[src2dst[i]].type, H5T_COPY_ALL)))
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCOPY, FAIL,
                                "can't copy destination compound member datatype");
                priv->dst_memb[src2dst[i]] = type;
            } /* end if */
        }     /* end for */
    }         /* end if */
    else {
        /* Restore sorted conditions for the datatypes */
        /* (Required for the src2dst array to be valid) */
        H5T__sort_value(src, NULL);
        H5T__sort_value(dst, NULL);
    } /* end else */

    /*
     * (Re)build the cache of member conversion functions and pointers to
     * their cdata entries.
     */
    src2dst = priv->src2dst;
    H5MM_xfree(priv->memb_path);
    if (NULL ==
        (priv->memb_path = (H5T_path_t **)H5MM_malloc(src->shared->u.compnd.nmembs * sizeof(H5T_path_t *))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    for (i = 0; i < src_nmembs; i++) {
        if (src2dst[i] >= 0) {
            H5T_path_t *tpath;
            bool        need_ids;

            tpath = H5T_path_find(src->shared->u.compnd.memb[i].type,
                                  dst->shared->u.compnd.memb[src2dst[i]].type);

            if (NULL == (priv->memb_path[i] = tpath)) {
                H5T__conv_struct_free(priv);
                cdata->priv = NULL;
                HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unable to convert member datatype");
            } /* end if */

            /* Create IDs for the compound member datatypes if the conversion path uses
             * an application conversion function or if a conversion exception function
             * was provided.
             */
            need_ids = tpath->conv.is_app ||
                       (cdata->command == H5T_CONV_INIT && conv_ctx->u.init.cb_struct.func) ||
                       (cdata->command == H5T_CONV_CONV && conv_ctx->u.conv.cb_struct.func);

            if (need_ids) {
                hid_t tid;

                if ((tid = H5I_register(H5I_DATATYPE, priv->src_memb[i], false)) < 0) {
                    H5T__conv_struct_free(priv);
                    cdata->priv = NULL;
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, FAIL,
                                "can't register ID for source compound member datatype");
                }
                priv->src_memb_id[i] = tid;

                if ((tid = H5I_register(H5I_DATATYPE, priv->dst_memb[src2dst[i]], false)) < 0) {
                    H5T__conv_struct_free(priv);
                    cdata->priv = NULL;
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, FAIL,
                                "can't register ID for destination compound member datatype");
                }
                priv->dst_memb_id[src2dst[i]] = tid;
            }
        } /* end if */
    }     /* end for */

    /* The compound conversion functions need a background buffer */
    cdata->need_bkg = H5T_BKG_YES;

    if (src_nmembs < dst_nmembs) {
        priv->subset_info.subset = H5T_SUBSET_SRC;
        for (i = 0; i < src_nmembs; i++) {
            /* If any of source members doesn't have counterpart in the same
             * order or there's conversion between members, don't do the
             * optimization.
             */
            if (src2dst[i] != (int)i ||
                (src->shared->u.compnd.memb[i].offset != dst->shared->u.compnd.memb[i].offset) ||
                (priv->memb_path[i])->is_noop == false) {
                priv->subset_info.subset = H5T_SUBSET_FALSE;
                break;
            } /* end if */
        }     /* end for */
        /* Compute the size of the data to be copied for each element.  It
         * may be smaller than either src or dst if there is extra space at
         * the end of src.
         */
        if (priv->subset_info.subset == H5T_SUBSET_SRC)
            priv->subset_info.copy_size = src->shared->u.compnd.memb[src_nmembs - 1].offset +
                                          src->shared->u.compnd.memb[src_nmembs - 1].size;
    }
    else if (dst_nmembs < src_nmembs) {
        priv->subset_info.subset = H5T_SUBSET_DST;
        for (i = 0; i < dst_nmembs; i++) {
            /* If any of source members doesn't have counterpart in the same order or
             * there's conversion between members, don't do the optimization. */
            if (src2dst[i] != (int)i ||
                (src->shared->u.compnd.memb[i].offset != dst->shared->u.compnd.memb[i].offset) ||
                (priv->memb_path[i])->is_noop == false) {
                priv->subset_info.subset = H5T_SUBSET_FALSE;
                break;
            }
        } /* end for */
        /* Compute the size of the data to be copied for each element.  It
         * may be smaller than either src or dst if there is extra space at
         * the end of dst.
         */
        if (priv->subset_info.subset == H5T_SUBSET_DST)
            priv->subset_info.copy_size = dst->shared->u.compnd.memb[dst_nmembs - 1].offset +
                                          dst->shared->u.compnd.memb[dst_nmembs - 1].size;
    }
    else /* If the numbers of source and dest members are equal and no conversion is needed,
          * the case should have been handled as noop earlier in H5Dio.c. */
    {
    }

    cdata->recalc = false;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5T__conv_struct_init() */

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_struct_subset
 *
 * Purpose:     A quick way to return a field in a struct private in this
 *              file.  The flag SMEMBS_SUBSET indicates whether the source
 *              members are a subset of destination or the destination
 *              members are a subset of the source, and the order is the
 *              same, and no conversion is needed.  For example:
 *                  struct source {            struct destination {
 *                      TYPE1 A;      -->          TYPE1 A;
 *                      TYPE2 B;      -->          TYPE2 B;
 *                      TYPE3 C;      -->          TYPE3 C;
 *                  };                             TYPE4 D;
 *                                                 TYPE5 E;
 *                                             };
 *
 * Return:      A pointer to the subset info struct in p.  Points directly
 *              into the structure.
 *
 *-------------------------------------------------------------------------
 */
H5T_subset_info_t *
H5T__conv_struct_subset(const H5T_cdata_t *cdata)
{
    H5T_conv_struct_t *priv = NULL;

    FUNC_ENTER_PACKAGE_NOERR

    assert(cdata);
    assert(cdata->priv);

    priv = (H5T_conv_struct_t *)(cdata->priv);

    FUNC_LEAVE_NOAPI((H5T_subset_info_t *)&priv->subset_info)
} /* end H5T__conv_struct_subset() */

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_struct
 *
 * Purpose:    Converts between compound datatypes.  This is a soft
 *        conversion function.  The algorithm is basically:
 *
 *         For each element do
 *          For I=1..NELMTS do
 *            If sizeof destination type <= sizeof source type then
 *              Convert member to destination type;
 *            Move member as far left as possible;
 *
 *          For I=NELMTS..1 do
 *            If not destination type then
 *              Convert member to destination type;
 *            Move member to correct position in BKG
 *
 *          Copy BKG to BUF
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_struct(const H5T_t *src, const H5T_t *dst, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                 size_t nelmts, size_t buf_stride, size_t bkg_stride, void *_buf, void *_bkg)
{
    uint8_t           *buf  = (uint8_t *)_buf;  /*cast for pointer arithmetic    */
    uint8_t           *bkg  = (uint8_t *)_bkg;  /*background pointer arithmetic    */
    uint8_t           *xbuf = buf, *xbkg = bkg; /*temp pointers into buf and bkg*/
    int               *src2dst  = NULL;         /*maps src member to dst member    */
    H5T_cmemb_t       *src_memb = NULL;         /*source struct member descript.*/
    H5T_cmemb_t       *dst_memb = NULL;         /*destination struct memb desc.    */
    size_t             offset;                  /*byte offset wrt struct    */
    ssize_t            src_delta;               /*source stride    */
    ssize_t            bkg_delta;               /*background stride    */
    size_t             elmtno;
    unsigned           u; /*counters */
    H5T_conv_struct_t *priv         = (H5T_conv_struct_t *)(cdata->priv);
    H5T_conv_ctx_t     tmp_conv_ctx = {0};
    herr_t             ret_value    = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    switch (cdata->command) {
        case H5T_CONV_INIT:
            /*
             * First, determine if this conversion function applies to the
             * conversion path SRC-->DST.  If not, return failure;
             * otherwise initialize the `priv' field of `cdata' with information
             * that remains (almost) constant for this conversion path.
             */
            if (NULL == src || NULL == dst)
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "not a datatype");
            if (H5T_COMPOUND != src->shared->type)
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "not a H5T_COMPOUND datatype");
            if (H5T_COMPOUND != dst->shared->type)
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "not a H5T_COMPOUND datatype");

            if (H5T__conv_struct_init(src, dst, cdata, conv_ctx) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to initialize conversion data");
            break;

        case H5T_CONV_FREE: {
            /*
             * Free the private conversion data.
             */
            herr_t status = H5T__conv_struct_free(priv);
            cdata->priv   = NULL;
            if (status < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTFREE, FAIL, "unable to free private conversion data");

            break;
        }

        case H5T_CONV_CONV:
            /*
             * Conversion.
             */
            if (NULL == src || NULL == dst)
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "not a datatype");
            if (NULL == conv_ctx)
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, FAIL, "invalid datatype conversion context pointer");
            assert(priv);
            assert(bkg && cdata->need_bkg);

            /* Initialize temporary conversion context */
            tmp_conv_ctx = *conv_ctx;

            if (cdata->recalc && H5T__conv_struct_init(src, dst, cdata, conv_ctx) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to initialize conversion data");

            /*
             * Insure that members are sorted.
             */
            H5T__sort_value(src, NULL);
            H5T__sort_value(dst, NULL);
            src2dst = priv->src2dst;

            /*
             * Direction of conversion and striding through background.
             */
            if (buf_stride) {
                H5_CHECKED_ASSIGN(src_delta, ssize_t, buf_stride, size_t);
                if (!bkg_stride) {
                    H5_CHECKED_ASSIGN(bkg_delta, ssize_t, dst->shared->size, size_t);
                } /* end if */
                else
                    H5_CHECKED_ASSIGN(bkg_delta, ssize_t, bkg_stride, size_t);
            } /* end if */
            else if (dst->shared->size <= src->shared->size) {
                H5_CHECKED_ASSIGN(src_delta, ssize_t, src->shared->size, size_t);
                H5_CHECKED_ASSIGN(bkg_delta, ssize_t, dst->shared->size, size_t);
            } /* end else-if */
            else {
                H5_CHECK_OVERFLOW(src->shared->size, size_t, ssize_t);
                src_delta = -(ssize_t)src->shared->size;
                H5_CHECK_OVERFLOW(dst->shared->size, size_t, ssize_t);
                bkg_delta = -(ssize_t)dst->shared->size;
                xbuf += (nelmts - 1) * src->shared->size;
                xbkg += (nelmts - 1) * dst->shared->size;
            } /* end else */

            /* Conversion loop... */
            for (elmtno = 0; elmtno < nelmts; elmtno++) {
                /*
                 * For each source member which will be present in the
                 * destination, convert the member to the destination type unless
                 * it is larger than the source type.  Then move the member to the
                 * left-most unoccupied position in the buffer.  This makes the
                 * data point as small as possible with all the free space on the
                 * right side.
                 */
                tmp_conv_ctx.u.conv.recursive = true;
                for (u = 0, offset = 0; u < src->shared->u.compnd.nmembs; u++) {
                    if (src2dst[u] < 0)
                        continue; /*subsetting*/
                    src_memb = src->shared->u.compnd.memb + u;
                    dst_memb = dst->shared->u.compnd.memb + src2dst[u];

                    if (dst_memb->size <= src_memb->size) {
                        /* Update IDs in conversion context */
                        tmp_conv_ctx.u.conv.src_type_id = priv->src_memb_id[u];
                        tmp_conv_ctx.u.conv.dst_type_id = priv->dst_memb_id[src2dst[u]];

                        if (H5T_convert_with_ctx(priv->memb_path[u], priv->src_memb[u],
                                                 priv->dst_memb[src2dst[u]], &tmp_conv_ctx, (size_t)1,
                                                 (size_t)0, (size_t)0, /*no striding (packed array)*/
                                                 xbuf + src_memb->offset, xbkg + dst_memb->offset) < 0)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL,
                                        "unable to convert compound datatype member");

                        memmove(xbuf + offset, xbuf + src_memb->offset, dst_memb->size);
                        offset += dst_memb->size;
                    } /* end if */
                    else {
                        memmove(xbuf + offset, xbuf + src_memb->offset, src_memb->size);
                        offset += src_memb->size;
                    } /* end else */
                }     /* end for */
                tmp_conv_ctx.u.conv.recursive = false;

                /*
                 * For each source member which will be present in the
                 * destination, convert the member to the destination type if it
                 * is larger than the source type (that is, has not been converted
                 * yet).  Then copy the member to the destination offset in the
                 * background buffer.
                 */
                tmp_conv_ctx.u.conv.recursive = true;
                H5_CHECK_OVERFLOW(src->shared->u.compnd.nmembs, size_t, int);
                for (int i = (int)src->shared->u.compnd.nmembs - 1; i >= 0; --i) {
                    if (src2dst[i] < 0)
                        continue; /*subsetting*/
                    src_memb = src->shared->u.compnd.memb + i;
                    dst_memb = dst->shared->u.compnd.memb + src2dst[i];

                    if (dst_memb->size > src_memb->size) {
                        /* Update IDs in conversion context */
                        tmp_conv_ctx.u.conv.src_type_id = priv->src_memb_id[i];
                        tmp_conv_ctx.u.conv.dst_type_id = priv->dst_memb_id[src2dst[i]];

                        offset -= src_memb->size;
                        if (H5T_convert_with_ctx(priv->memb_path[i], priv->src_memb[i],
                                                 priv->dst_memb[src2dst[i]], &tmp_conv_ctx, (size_t)1,
                                                 (size_t)0, (size_t)0, /*no striding (packed array)*/
                                                 xbuf + offset, xbkg + dst_memb->offset) < 0)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL,
                                        "unable to convert compound datatype member");
                    } /* end if */
                    else
                        offset -= dst_memb->size;
                    memcpy(xbkg + dst_memb->offset, xbuf + offset, dst_memb->size);
                } /* end for */
                tmp_conv_ctx.u.conv.recursive = false;

                assert(0 == offset);

                /*
                 * Update pointers
                 */
                xbuf += src_delta;
                xbkg += bkg_delta;
            } /* end for */

            /* If the bkg_delta was set to -(dst->shared->size), make it positive now */
            if (buf_stride == 0 && dst->shared->size > src->shared->size)
                H5_CHECKED_ASSIGN(bkg_delta, ssize_t, dst->shared->size, size_t);

            /*
             * Copy the background buffer back into the in-place conversion
             * buffer.
             */
            for (xbuf = buf, xbkg = bkg, elmtno = 0; elmtno < nelmts; elmtno++) {
                memcpy(xbuf, xbkg, dst->shared->size);
                xbuf += buf_stride ? buf_stride : dst->shared->size;
                xbkg += bkg_delta;
            } /* end for */
            break;

        default:
            /* Some other command we don't know about yet.*/
            HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unknown conversion command");
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5T__conv_struct() */

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_struct_opt
 *
 * Purpose:    Converts between compound datatypes in a manner more
 *        efficient than the general-purpose H5T__conv_struct()
 *        function.  This function isn't applicable if the destination
 *        is larger than the source type. This is a soft conversion
 *        function.  The algorithm is basically:
 *
 *         For each member of the struct
 *          If sizeof destination type <= sizeof source type then
 *            Convert member to destination type for all elements
 *            Move memb to BKG buffer for all elements
 *          Else
 *            Move member as far left as possible for all elements
 *
 *        For each member of the struct (in reverse order)
 *          If not destination type then
 *            Convert member to destination type for all elements
 *            Move member to correct position in BKG for all elements
 *
 *        Copy BKG to BUF for all elements
 *
 *              Special case when the source and destination members
 *              are a subset of each other, and the order is the same, and no
 *              conversion is needed.  For example:
 *                  struct source {            struct destination {
 *                      TYPE1 A;      -->          TYPE1 A;
 *                      TYPE2 B;      -->          TYPE2 B;
 *                      TYPE3 C;      -->          TYPE3 C;
 *                  };                             TYPE4 D;
 *                                                 TYPE5 E;
 *                                             };
 *              The optimization is simply moving data to the appropriate
 *              places in the buffer.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_struct_opt(const H5T_t *src, const H5T_t *dst, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                     size_t nelmts, size_t buf_stride, size_t bkg_stride, void *_buf, void *_bkg)
{
    uint8_t           *buf      = (uint8_t *)_buf; /*cast for pointer arithmetic    */
    uint8_t           *bkg      = (uint8_t *)_bkg; /*background pointer arithmetic    */
    uint8_t           *xbuf     = NULL;            /*temporary pointer into `buf'    */
    uint8_t           *xbkg     = NULL;            /*temporary pointer into `bkg'    */
    int               *src2dst  = NULL;            /*maps src member to dst member    */
    H5T_cmemb_t       *src_memb = NULL;            /*source struct member descript.*/
    H5T_cmemb_t       *dst_memb = NULL;            /*destination struct memb desc.    */
    size_t             offset;                     /*byte offset wrt struct    */
    size_t             elmtno;                     /*element counter        */
    size_t             copy_size;                  /*size of element for copying   */
    H5T_conv_struct_t *priv         = NULL;        /*private data            */
    H5T_conv_ctx_t     tmp_conv_ctx = {0};         /*temporary conversion context */
    bool               no_stride    = false;       /*flag to indicate no stride    */
    unsigned           u;                          /*counters */
    herr_t             ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_PACKAGE

    switch (cdata->command) {
        case H5T_CONV_INIT:
            /*
             * First, determine if this conversion function applies to the
             * conversion path SRC-->DST.  If not, return failure;
             * otherwise initialize the `priv' field of `cdata' with information
             * that remains (almost) constant for this conversion path.
             */
            if (NULL == src || NULL == dst)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype");
            if (H5T_COMPOUND != src->shared->type)
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "not a H5T_COMPOUND datatype");
            if (H5T_COMPOUND != dst->shared->type)
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "not a H5T_COMPOUND datatype");

            /* Initialize data which is relatively constant */
            if (H5T__conv_struct_init(src, dst, cdata, conv_ctx) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to initialize conversion data");
            priv    = (H5T_conv_struct_t *)(cdata->priv);
            src2dst = priv->src2dst;

            /*
             * If the destination type is not larger than the source type then
             * this conversion function is guaranteed to work (provided all
             * members can be converted also). Otherwise the determination is
             * quite a bit more complicated. Essentially we have to make sure
             * that there is always room in the source buffer to do the
             * conversion of a member in place. This is basically the same pair
             * of loops as in the actual conversion except it checks that there
             * is room for each conversion instead of actually doing anything.
             */
            if (dst->shared->size > src->shared->size) {
                for (u = 0, offset = 0; u < src->shared->u.compnd.nmembs; u++) {
                    if (src2dst[u] < 0)
                        continue;
                    src_memb = src->shared->u.compnd.memb + u;
                    dst_memb = dst->shared->u.compnd.memb + src2dst[u];
                    if (dst_memb->size > src_memb->size)
                        offset += src_memb->size;
                } /* end for */
                H5_CHECK_OVERFLOW(src->shared->u.compnd.nmembs, size_t, int);
                for (int i = (int)src->shared->u.compnd.nmembs - 1; i >= 0; --i) {
                    if (src2dst[i] < 0)
                        continue;
                    src_memb = src->shared->u.compnd.memb + i;
                    dst_memb = dst->shared->u.compnd.memb + src2dst[i];
                    if (dst_memb->size > src_memb->size) {
                        offset -= src_memb->size;
                        if (dst_memb->size > src->shared->size - offset) {
                            H5T__conv_struct_free(priv);
                            cdata->priv = NULL;
                            HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
                                        "conversion is unsupported by this function");
                        } /* end if */
                    }     /* end if */
                }         /* end for */
            }             /* end if */
            break;

        case H5T_CONV_FREE: {
            /*
             * Free the private conversion data.
             */
            herr_t status = H5T__conv_struct_free((H5T_conv_struct_t *)(cdata->priv));
            cdata->priv   = NULL;
            if (status < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTFREE, FAIL, "unable to free private conversion data");

            break;
        }

        case H5T_CONV_CONV:
            /*
             * Conversion.
             */
            if (NULL == src || NULL == dst)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype");
            if (NULL == conv_ctx)
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, FAIL, "invalid datatype conversion context pointer");
            if (!bkg)
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, FAIL, "invalid background buffer pointer");

            /* Initialize temporary conversion context */
            tmp_conv_ctx = *conv_ctx;

            /* Update cached data if necessary */
            if (cdata->recalc && H5T__conv_struct_init(src, dst, cdata, conv_ctx) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to initialize conversion data");
            priv = (H5T_conv_struct_t *)(cdata->priv);
            assert(priv);
            src2dst = priv->src2dst;
            assert(cdata->need_bkg);

            /*
             * Insure that members are sorted.
             */
            H5T__sort_value(src, NULL);
            H5T__sort_value(dst, NULL);

            /*
             * Calculate strides. If BUF_STRIDE is non-zero then convert one
             * data element at every BUF_STRIDE bytes through the main buffer
             * (BUF), leaving the result of each conversion at the same
             * location; otherwise assume the source and destination data are
             * packed tightly based on src->shared->size and dst->shared->size.  Also, if
             * BUF_STRIDE and BKG_STRIDE are both non-zero then place
             * background data into the BKG buffer at multiples of BKG_STRIDE;
             * otherwise assume BKG buffer is the packed destination datatype.
             */
            if (!buf_stride || !bkg_stride)
                bkg_stride = dst->shared->size;
            if (!buf_stride) {
                no_stride  = true;
                buf_stride = src->shared->size;
            } /* end if */

            if (priv->subset_info.subset == H5T_SUBSET_SRC || priv->subset_info.subset == H5T_SUBSET_DST) {
                /* If the optimization flag is set to indicate source members are a subset and
                 * in the top of the destination, simply copy the source members to background buffer.
                 */
                xbuf      = buf;
                xbkg      = bkg;
                copy_size = priv->subset_info.copy_size;

                for (elmtno = 0; elmtno < nelmts; elmtno++) {
                    memcpy(xbkg, xbuf, copy_size);

                    /* Update pointers */
                    xbuf += buf_stride;
                    xbkg += bkg_stride;
                } /* end for */
            }     /* end if */
            else {
                /*
                 * For each member where the destination is not larger than the
                 * source, stride through all the elements converting only that member
                 * in each element and then copying the element to its final
                 * destination in the bkg buffer. Otherwise move the element as far
                 * left as possible in the buffer.
                 */
                tmp_conv_ctx.u.conv.recursive = true;
                for (u = 0, offset = 0; u < src->shared->u.compnd.nmembs; u++) {
                    if (src2dst[u] < 0)
                        continue; /*subsetting*/
                    src_memb = src->shared->u.compnd.memb + u;
                    dst_memb = dst->shared->u.compnd.memb + src2dst[u];

                    if (dst_memb->size <= src_memb->size) {
                        /* Update IDs in conversion context */
                        tmp_conv_ctx.u.conv.src_type_id = priv->src_memb_id[u];
                        tmp_conv_ctx.u.conv.dst_type_id = priv->dst_memb_id[src2dst[u]];

                        xbuf = buf + src_memb->offset;
                        xbkg = bkg + dst_memb->offset;
                        if (H5T_convert_with_ctx(priv->memb_path[u], priv->src_memb[u],
                                                 priv->dst_memb[src2dst[u]], &tmp_conv_ctx, nelmts,
                                                 buf_stride, bkg_stride, xbuf, xbkg) < 0)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL,
                                        "unable to convert compound datatype member");

                        for (elmtno = 0; elmtno < nelmts; elmtno++) {
                            memcpy(xbkg, xbuf, dst_memb->size);
                            xbuf += buf_stride;
                            xbkg += bkg_stride;
                        } /* end for */
                    }     /* end if */
                    else {
                        for (xbuf = buf, elmtno = 0; elmtno < nelmts; elmtno++) {
                            memmove(xbuf + offset, xbuf + src_memb->offset, src_memb->size);
                            xbuf += buf_stride;
                        } /* end for */
                        offset += src_memb->size;
                    } /* end else */
                }     /* end else */
                tmp_conv_ctx.u.conv.recursive = false;

                /*
                 * Work from right to left, converting those members that weren't
                 * converted in the previous loop (those members where the destination
                 * is larger than the source) and them to their final position in the
                 * bkg buffer.
                 */
                tmp_conv_ctx.u.conv.recursive = true;
                H5_CHECK_OVERFLOW(src->shared->u.compnd.nmembs, size_t, int);
                for (int i = (int)src->shared->u.compnd.nmembs - 1; i >= 0; --i) {
                    if (src2dst[i] < 0)
                        continue;
                    src_memb = src->shared->u.compnd.memb + i;
                    dst_memb = dst->shared->u.compnd.memb + src2dst[i];

                    if (dst_memb->size > src_memb->size) {
                        /* Update IDs in conversion context */
                        tmp_conv_ctx.u.conv.src_type_id = priv->src_memb_id[i];
                        tmp_conv_ctx.u.conv.dst_type_id = priv->dst_memb_id[src2dst[i]];

                        offset -= src_memb->size;
                        xbuf = buf + offset;
                        xbkg = bkg + dst_memb->offset;
                        if (H5T_convert_with_ctx(priv->memb_path[i], priv->src_memb[i],
                                                 priv->dst_memb[src2dst[i]], &tmp_conv_ctx, nelmts,
                                                 buf_stride, bkg_stride, xbuf, xbkg) < 0)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL,
                                        "unable to convert compound datatype member");
                        for (elmtno = 0; elmtno < nelmts; elmtno++) {
                            memcpy(xbkg, xbuf, dst_memb->size);
                            xbuf += buf_stride;
                            xbkg += bkg_stride;
                        } /* end for */
                    }     /* end if */
                }         /* end for */
                tmp_conv_ctx.u.conv.recursive = false;
            } /* end else */

            if (no_stride)
                buf_stride = dst->shared->size;

            /* Move background buffer into result buffer */
            for (xbuf = buf, xbkg = bkg, elmtno = 0; elmtno < nelmts; elmtno++) {
                memcpy(xbuf, xbkg, dst->shared->size);
                xbuf += buf_stride;
                xbkg += bkg_stride;
            } /* end for */
            break;

        default:
            /* Some other command we don't know about yet.*/
            HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unknown conversion command");
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5T__conv_struct_opt() */

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_enum_free
 *
 * Purpose:     Free the private data structure used by the enum conversion
 *              functions.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5T__conv_enum_free(H5T_conv_enum_t *priv)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    if (priv) {
        free(priv->src2dst);

        if (priv->dst_copy && H5T_close(priv->dst_copy) < 0)
            HDONE_ERROR(H5E_DATATYPE, H5E_CANTCLOSEOBJ, FAIL, "unable to close copied source datatype");
        if (priv->src_copy && H5T_close(priv->src_copy) < 0)
            HDONE_ERROR(H5E_DATATYPE, H5E_CANTCLOSEOBJ, FAIL, "unable to close copied destination datatype");

        free(priv);
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5T__conv_enum_free() */

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_enum_init
 *
 * Purpose:    Initialize information for H5T__conv_enum().
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5T__conv_enum_init(const H5T_t *src, const H5T_t *dst, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx)
{
    H5T_conv_enum_t *priv          = NULL; /* Private conversion data */
    int             *map           = NULL; /* Map from src value to dst idx */
    bool             rebuild_cache = false;
    herr_t           ret_value     = SUCCEED;

    FUNC_ENTER_PACKAGE

    cdata->need_bkg = H5T_BKG_NO;

    priv = (H5T_conv_enum_t *)(cdata->priv);
    if (!priv) {
        if (NULL == (priv = (H5T_conv_enum_t *)(cdata->priv = calloc(1, sizeof(*priv)))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
        rebuild_cache = true;
    }
    else {
        /* Check if we need to rebuild our cache. For now, treat
         * enums as different even if one is just a subset of the
         * other
         */
        if (cdata->command == H5T_CONV_CONV && conv_ctx->u.conv.recursive)
            /* Recursive conversion; we can reuse the cache */
            rebuild_cache = false;
        else {
            if (0 != H5T_cmp(src, priv->src_copy, false) || 0 != H5T_cmp(dst, priv->dst_copy, false))
                rebuild_cache = true;
        }
    }

    if (rebuild_cache) {
        H5T_shared_t *src_sh;
        H5T_shared_t *dst_sh;
        size_t        src_nmembs;
        size_t        dst_nmembs;
        void         *tmp_realloc;

        /* Allocate everything we need to cache */
        if (priv->src_copy && H5T_close(priv->src_copy) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCLOSEOBJ, FAIL, "unable to close copied source datatype");
        if (priv->dst_copy && H5T_close(priv->dst_copy) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCLOSEOBJ, FAIL, "unable to close copied destination datatype");

        if (NULL == (priv->src_copy = H5T_copy(src, H5T_COPY_ALL)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCOPY, FAIL, "unable to copy source datatype");
        if (NULL == (priv->dst_copy = H5T_copy(dst, H5T_COPY_ALL)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCOPY, FAIL, "unable to copy destination datatype");

        /* Nothing more to do if enum has no members */
        if (0 == src->shared->u.enumer.nmembs)
            HGOTO_DONE(SUCCEED);

        src_sh     = priv->src_copy->shared;
        dst_sh     = priv->src_copy->shared;
        src_nmembs = src_sh->u.enumer.nmembs;
        dst_nmembs = dst_sh->u.enumer.nmembs;

        if (NULL == (tmp_realloc = realloc(priv->src2dst, src_nmembs * sizeof(int)))) {
            free(priv->src2dst);
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                        "unable to allocate space for source to destination enum mapping");
        }
        priv->src2dst = tmp_realloc;

        /*
         * Check that the source symbol names are a subset of the destination
         * symbol names and build a map from source member index to destination
         * member index.
         */
        H5T__sort_name(priv->src_copy, NULL);
        H5T__sort_name(priv->dst_copy, NULL);
        for (size_t i = 0, j = 0; i < src_nmembs && j < dst_nmembs; i++, j++) {
            char *src_name = src_sh->u.enumer.name[i];
            char *dst_name = dst_sh->u.enumer.name[j];

            while (j < dst_nmembs && strcmp(src_name, dst_name) != 0)
                j++;

            if (j >= dst_nmembs)
                HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
                            "source enum type is not a subset of destination enum type");

            H5_CHECKED_ASSIGN(priv->src2dst[i], int, j, size_t);
        }

        /*
         * The conversion function will use an O(log N) lookup method for each
         * value converted. However, if all of the following constraints are met
         * then we can build a perfect hash table and use an O(1) lookup method.
         *
         *      A: The source datatype size matches one of our native datatype
         *         sizes.
         *
         *      B: After casting the source value bit pattern to a native type
         *         the size of the range of values is less than 20% larger than
         *         the number of values.
         *
         * If this special case is met then we use the source bit pattern cast as
         * a native integer type as an index into the `val2dst'. The values of
         * that array are the index numbers in the destination type or negative
         * if the entry is unused.
         *
         * (This optimized algorithm doesn't work when the byte orders are different.
         * The code such as "n = *((int *)((void *)((uint8_t *)src_sh->u.enumer.value + (i *
         * src_sh->size))));" can change the value significantly. i.g. if the source value is big-endian
         * 0x0000000f, executing the casting on little-endian machine will get a big number 0x0f000000. Then
         * it can't meet the condition "if (src_nmembs < 2 || ((double)length / (double)src_nmembs <
         * (double)(1.2F)))" Because this is the optimized code, we won't fix it. It should still work in some
         * situations. SLU - 2011/5/24)
         */
        if (1 == src_sh->size || sizeof(short) == src_sh->size || sizeof(int) == src_sh->size) {
            unsigned length;
            int      domain[2] = {0, 0}; /* Min and max source values */

            for (size_t i = 0; i < src_nmembs; i++) {
                int n;

                if (1 == src_sh->size)
                    n = *((signed char *)((uint8_t *)src_sh->u.enumer.value + i));
                else if (sizeof(short) == src_sh->size)
                    n = *((short *)((void *)((uint8_t *)src_sh->u.enumer.value + (i * src_sh->size))));
                else
                    n = *((int *)((void *)((uint8_t *)src_sh->u.enumer.value + (i * src_sh->size))));
                if (0 == i) {
                    domain[0] = domain[1] = n;
                }
                else {
                    domain[0] = MIN(domain[0], n);
                    domain[1] = MAX(domain[1], n);
                }
            }
            assert(domain[1] >= domain[0]);

            length = (unsigned)(domain[1] - domain[0]) + 1;
            if (src_nmembs < 2 || ((double)length / (double)src_nmembs < (double)(1.2F))) {
                priv->base   = domain[0];
                priv->length = length;

                if (NULL == (map = malloc(length * sizeof(int))))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory allocation failed");

                for (size_t i = 0; i < length; i++)
                    map[i] = -1; /*entry unused*/

                for (size_t i = 0; i < src_nmembs; i++) {
                    int n;

                    if (1 == src_sh->size)
                        n = *((signed char *)((uint8_t *)src_sh->u.enumer.value + i));
                    else if (sizeof(short) == src_sh->size)
                        n = *((short *)((void *)((uint8_t *)src_sh->u.enumer.value + (i * src_sh->size))));
                    else
                        n = *((int *)((void *)((uint8_t *)src_sh->u.enumer.value + (i * src_sh->size))));
                    n -= priv->base;
                    assert(n >= 0 && (unsigned)n < priv->length);
                    assert(map[n] < 0);
                    map[n] = priv->src2dst[i];
                }

                /*
                 * Replace original src2dst array with our new one. The original
                 * was indexed by source member number while the new one is
                 * indexed by source values.
                 */
                free(priv->src2dst);
                priv->src2dst = map;

                HGOTO_DONE(SUCCEED);
            }
        }

        /* Sort source type by value and adjust src2dst[] appropriately */
        H5T__sort_value(priv->src_copy, priv->src2dst);
    }

#ifdef H5T_DEBUG
    if (H5DEBUG(T)) {
        fprintf(H5DEBUG(T), "      Using %s mapping function%s\n", priv->length ? "O(1)" : "O(log N)",
                priv->length ? "" : ", where N is the number of enum members");
    }
#endif

done:
    if (ret_value < 0 && priv) {
        if (map) {
            free(map);
            priv->src2dst = NULL;
        }

        if (H5T__conv_enum_free(priv) < 0)
            HDONE_ERROR(H5E_DATATYPE, H5E_CANTFREE, FAIL, "can't free enum conversion data");

        cdata->priv = NULL;
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5T__conv_enum_init() */

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_enum
 *
 * Purpose:    Converts one type of enumerated data to another.
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_enum(const H5T_t *src, const H5T_t *dst, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
               size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *_buf,
               void H5_ATTR_UNUSED *bkg)
{
    H5T_conv_enum_t *priv   = (H5T_conv_enum_t *)(cdata->priv);
    H5T_shared_t    *src_sh = NULL;
    H5T_shared_t    *dst_sh = NULL;
    uint8_t         *buf    = (uint8_t *)_buf; /*cast for pointer arithmetic    */
    uint8_t         *s = NULL, *d = NULL;      /*src and dst BUF pointers    */
    ssize_t          src_delta, dst_delta;     /*conversion strides        */
    int              n;                        /*src value cast as native int    */
    H5T_conv_ret_t   except_ret;               /*return of callback function   */
    size_t           i;                        /*counters            */
    herr_t           ret_value = SUCCEED;      /* Return value                 */

    FUNC_ENTER_PACKAGE

    switch (cdata->command) {
        case H5T_CONV_INIT:
            /*
             * Determine if this conversion function applies to the conversion
             * path SRC->DST.  If not return failure; otherwise initialize
             * the `priv' field of `cdata' with information about the underlying
             * integer conversion.
             */
            if (NULL == src || NULL == dst)
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "not a datatype");
            if (H5T_ENUM != src->shared->type)
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "not a H5T_ENUM datatype");
            if (H5T_ENUM != dst->shared->type)
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "not a H5T_ENUM datatype");

            if (H5T__conv_enum_init(src, dst, cdata, conv_ctx) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to initialize private data");
            break;

        case H5T_CONV_FREE: {
            herr_t status = H5T__conv_enum_free(priv);
            cdata->priv   = NULL;
            if (status < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTFREE, FAIL, "unable to free private conversion data");

            break;
        }

        case H5T_CONV_CONV:
            if (NULL == src || NULL == dst)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype");
            if (NULL == conv_ctx)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid datatype conversion context pointer");
            if (H5T_ENUM != src->shared->type)
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "not a H5T_ENUM datatype");
            if (H5T_ENUM != dst->shared->type)
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "not a H5T_ENUM datatype");

            /* Reuse cache if possible, rebuild otherwise */
            if (H5T__conv_enum_init(src, dst, cdata, conv_ctx) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to initialize private data");

            src_sh = priv->src_copy->shared;
            dst_sh = priv->dst_copy->shared;

            /*
             * Direction of conversion.
             */
            if (buf_stride) {
                H5_CHECK_OVERFLOW(buf_stride, size_t, ssize_t);
                src_delta = dst_delta = (ssize_t)buf_stride;
                s = d = buf;
            }
            else if (dst_sh->size <= src_sh->size) {
                H5_CHECKED_ASSIGN(src_delta, ssize_t, src_sh->size, size_t);
                H5_CHECKED_ASSIGN(dst_delta, ssize_t, dst_sh->size, size_t);
                s = d = buf;
            }
            else {
                H5_CHECK_OVERFLOW(src_sh->size, size_t, ssize_t);
                H5_CHECK_OVERFLOW(dst_sh->size, size_t, ssize_t);
                src_delta = -(ssize_t)src_sh->size;
                dst_delta = -(ssize_t)dst_sh->size;
                s         = buf + (nelmts - 1) * src_sh->size;
                d         = buf + (nelmts - 1) * dst_sh->size;
            }

            if (priv->length) {
                for (i = 0; i < nelmts; i++, s += src_delta, d += dst_delta) {
                    /* Use O(1) lookup */
                    /* (The casting won't work when the byte orders are different. i.g. if the source value
                     * is big-endian 0x0000000f, the direct casting "n = *((int *)((void *)s));" will make
                     * it a big number 0x0f000000 on little-endian machine. But we won't fix it because it's
                     * an optimization code. Please also see the comment in the H5T__conv_enum_init()
                     * function. SLU - 2011/5/24)
                     */
                    if (1 == src_sh->size)
                        n = *((signed char *)s);
                    else if (sizeof(short) == src_sh->size)
                        n = *((short *)((void *)s));
                    else
                        n = *((int *)((void *)s));
                    n -= priv->base;
                    if (n < 0 || (unsigned)n >= priv->length || priv->src2dst[n] < 0) {
                        /*overflow*/
                        except_ret = H5T_CONV_UNHANDLED;
                        /*If user's exception handler is present, use it*/
                        if (conv_ctx->u.conv.cb_struct.func)
                            except_ret = (conv_ctx->u.conv.cb_struct.func)(
                                H5T_CONV_EXCEPT_RANGE_HI, conv_ctx->u.conv.src_type_id,
                                conv_ctx->u.conv.dst_type_id, s, d, conv_ctx->u.conv.cb_struct.user_data);

                        if (except_ret == H5T_CONV_UNHANDLED)
                            memset(d, 0xff, dst_sh->size);
                        else if (except_ret == H5T_CONV_ABORT)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL,
                                        "can't handle conversion exception");
                    }
                    else
                        H5MM_memcpy(d,
                                    (uint8_t *)dst_sh->u.enumer.value +
                                        ((unsigned)priv->src2dst[n] * dst_sh->size),
                                    dst_sh->size);
                }
            }
            else {
                for (i = 0; i < nelmts; i++, s += src_delta, d += dst_delta) {
                    /* Use O(log N) lookup */
                    unsigned lt = 0;
                    unsigned rt = src_sh->u.enumer.nmembs;
                    unsigned md = 0;
                    int      cmp;

                    while (lt < rt) {
                        md = (lt + rt) / 2;
                        cmp =
                            memcmp(s, (uint8_t *)src_sh->u.enumer.value + (md * src_sh->size), src_sh->size);
                        if (cmp < 0)
                            rt = md;
                        else if (cmp > 0)
                            lt = md + 1;
                        else
                            break;
                    } /* end while */
                    if (lt >= rt) {
                        except_ret = H5T_CONV_UNHANDLED;
                        /*If user's exception handler is present, use it*/
                        if (conv_ctx->u.conv.cb_struct.func)
                            except_ret = (conv_ctx->u.conv.cb_struct.func)(
                                H5T_CONV_EXCEPT_RANGE_HI, conv_ctx->u.conv.src_type_id,
                                conv_ctx->u.conv.dst_type_id, s, d, conv_ctx->u.conv.cb_struct.user_data);

                        if (except_ret == H5T_CONV_UNHANDLED)
                            memset(d, 0xff, dst_sh->size);
                        else if (except_ret == H5T_CONV_ABORT)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL,
                                        "can't handle conversion exception");
                    } /* end if */
                    else {
                        assert(priv->src2dst[md] >= 0);
                        H5MM_memcpy(d,
                                    (uint8_t *)dst_sh->u.enumer.value +
                                        ((unsigned)priv->src2dst[md] * dst_sh->size),
                                    dst_sh->size);
                    } /* end else */
                }
            }

            break;

        default:
            /* Some other command we don't know about yet.*/
            HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unknown conversion command");
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5T__conv_enum() */

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_enum_numeric
 *
 * Purpose:    Converts enumerated data to a numeric type (integer or
 *              floating-point number). This function is registered into
 *              the conversion table twice in H5T_init_interface in H5T.c.
 *              Once for enum-integer conversion. Once for enum-float conversion.
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_enum_numeric(const H5T_t *src, const H5T_t *dst, H5T_cdata_t *cdata,
                       const H5T_conv_ctx_t H5_ATTR_UNUSED *conv_ctx, size_t nelmts,
                       size_t H5_ATTR_UNUSED buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *_buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_t      *src_parent;          /*parent type for src           */
    H5T_path_t *tpath;               /* Conversion information       */
    herr_t      ret_value = SUCCEED; /* Return value                 */

    FUNC_ENTER_PACKAGE

    switch (cdata->command) {
        case H5T_CONV_INIT:
            /*
             * Determine if this conversion function applies to the conversion
             * path SRC->DST.  If not, return failure.
             */
            if (NULL == src || NULL == dst)
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "not a datatype");
            if (H5T_ENUM != src->shared->type)
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "source type is not a H5T_ENUM datatype");
            if (H5T_INTEGER != dst->shared->type && H5T_FLOAT != dst->shared->type)
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "destination is not an integer type");

            cdata->need_bkg = H5T_BKG_NO;
            break;

        case H5T_CONV_FREE:
            break;

        case H5T_CONV_CONV:
            if (NULL == src || NULL == dst)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype");

            src_parent = src->shared->parent;

            if (NULL == (tpath = H5T_path_find(src_parent, dst))) {
                HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
                            "unable to convert between src and dest datatype");
            }
            else if (!H5T_path_noop(tpath)) {
                /* Convert the data */
                if (H5T_convert(tpath, src_parent, dst, nelmts, buf_stride, bkg_stride, _buf, bkg) < 0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "datatype conversion failed");
            }
            break;

        default:
            /* Some other command we don't know about yet.*/
            HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unknown conversion command");
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5T__conv_enum_numeric() */

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_vlen_nested_free
 *
 * Purpose:     Recursively locates and frees any nested VLEN components of
 *              complex data types (including COMPOUND).
 *
 * Return:      Non-negative on success/Negative on failure.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5T__conv_vlen_nested_free(uint8_t *buf, H5T_t *dt)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    switch (dt->shared->type) {
        case H5T_VLEN:
            /* Pointer buf refers to VLEN data; free it (always reset tmp) */
            if ((*(dt->shared->u.vlen.cls->del))(dt->shared->u.vlen.file, buf) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTFREE, FAIL, "can't free nested vlen");
            break;

        case H5T_COMPOUND:
            /* Pointer buf refers to COMPOUND data; recurse for each member. */
            for (unsigned i = 0; i < dt->shared->u.compnd.nmembs; ++i)
                if (H5T__conv_vlen_nested_free(buf + dt->shared->u.compnd.memb[i].offset,
                                               dt->shared->u.compnd.memb[i].type) < 0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTFREE, FAIL, "can't free compound member");
            break;

        case H5T_ARRAY:
            /* Pointer buf refers to ARRAY data; recurse for each element. */
            for (unsigned i = 0; i < dt->shared->u.array.nelem; ++i)
                if (H5T__conv_vlen_nested_free(buf + i * dt->shared->parent->shared->size,
                                               dt->shared->parent) < 0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTFREE, FAIL, "can't free array data");
            break;

        case H5T_INTEGER:
        case H5T_FLOAT:
        case H5T_TIME:
        case H5T_STRING:
        case H5T_BITFIELD:
        case H5T_OPAQUE:
        case H5T_REFERENCE:
        case H5T_ENUM:
            /* These types cannot contain vl data */
            break;

        case H5T_NO_CLASS:
        case H5T_NCLASSES:
        default:
            HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "invalid datatype class");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5T__conv_vlen_nested_free() */

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_vlen
 *
 * Purpose:    Converts between VL datatypes in memory and on disk.
 *        This is a soft conversion function.  The algorithm is
 *        basically:
 *
 *          For every VL struct in the main buffer:
 *          1. Allocate space for temporary dst VL data (reuse buffer
 *             if possible)
 *                2. Copy VL data from src buffer into dst buffer
 *                3. Convert VL data into dst representation
 *                4. Allocate buffer in dst heap
 *          5. Free heap objects storing old data
 *                6. Write dst VL data into dst heap
 *                7. Store (heap ID or pointer) and length in main dst buffer
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_vlen(const H5T_t *src, const H5T_t *dst, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
               size_t nelmts, size_t buf_stride, size_t bkg_stride, void *buf, void *bkg)
{
    H5T_vlen_alloc_info_t vl_alloc_info;         /* VL allocation info */
    H5T_conv_ctx_t        tmp_conv_ctx  = {0};   /* Temporary conversion context */
    H5T_path_t           *tpath         = NULL;  /* Type conversion path             */
    bool                  noop_conv     = false; /* Flag to indicate a noop conversion */
    bool                  write_to_file = false; /* Flag to indicate writing to file */
    htri_t                parent_is_vlen;        /* Flag to indicate parent is vlen datatype */
    size_t                bg_seq_len = 0;        /* The number of elements in the background sequence */
    H5T_t                *tsrc_cpy   = NULL;     /* Temporary copy of source base datatype */
    H5T_t                *tdst_cpy   = NULL;     /* Temporary copy of destination base datatype */
    hid_t                 tsrc_id    = H5I_INVALID_HID; /* Temporary type atom */
    hid_t                 tdst_id    = H5I_INVALID_HID; /* Temporary type atom */
    uint8_t              *s          = NULL;            /* Source buffer            */
    uint8_t              *d          = NULL;            /* Destination buffer        */
    uint8_t              *b          = NULL;            /* Background buffer        */
    ssize_t               s_stride   = 0;               /* Src stride */
    ssize_t               d_stride   = 0;               /* Dst stride */
    ssize_t               b_stride;                     /* Bkg stride            */
    size_t                safe = 0;              /* How many elements are safe to process in each pass */
    size_t                src_base_size;         /* Source base size*/
    size_t                dst_base_size;         /* Destination base size*/
    void                 *conv_buf      = NULL;  /* Temporary conversion buffer          */
    size_t                conv_buf_size = 0;     /* Size of conversion buffer in bytes */
    void                 *tmp_buf       = NULL;  /* Temporary background buffer          */
    size_t                tmp_buf_size  = 0;     /* Size of temporary bkg buffer         */
    bool                  nested        = false; /* Flag of nested VL case             */
    size_t                elmtno        = 0;     /* Element number counter               */
    size_t                orig_d_stride = 0;     /* Original destination stride (used for error handling) */
    size_t orig_nelmts = nelmts; /* Original # of elements to convert (used for error handling) */
    bool   convert_forward =
        true; /* Current direction of conversion (forward or backward, used for error handling) */
    bool conversions_made =
        false; /* Flag to indicate conversions have been performed, used for error handling */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    switch (cdata->command) {
        case H5T_CONV_INIT:
            /*
             * First, determine if this conversion function applies to the
             * conversion path SRC_ID-->DST_ID.  If not, return failure;
             * otherwise initialize the `priv' field of `cdata' with
             * information that remains (almost) constant for this
             * conversion path.
             */
            if (NULL == src || NULL == dst)
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "not a datatype");
            if (H5T_VLEN != src->shared->type)
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "not a H5T_VLEN datatype");
            if (H5T_VLEN != dst->shared->type)
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "not a H5T_VLEN datatype");
            if (H5T_VLEN_STRING == src->shared->u.vlen.type && H5T_VLEN_STRING == dst->shared->u.vlen.type) {
                if ((H5T_CSET_ASCII == src->shared->u.vlen.cset &&
                     H5T_CSET_UTF8 == dst->shared->u.vlen.cset) ||
                    (H5T_CSET_ASCII == dst->shared->u.vlen.cset && H5T_CSET_UTF8 == src->shared->u.vlen.cset))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                                "The library doesn't convert between strings of ASCII and UTF");
            } /* end if */

            /* Variable-length types don't need a background buffer */
            cdata->need_bkg = H5T_BKG_NO;

            break;

        case H5T_CONV_FREE:
            /* QAK - Nothing to do currently */
            break;

        case H5T_CONV_CONV:
            /*
             * Conversion.
             */
            if (NULL == src || NULL == dst)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype");
            if (NULL == conv_ctx)
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, FAIL, "invalid datatype conversion context pointer");

            /* Initialize temporary conversion context */
            tmp_conv_ctx = *conv_ctx;

            /* Initialize source & destination strides */
            if (buf_stride) {
                assert(buf_stride >= src->shared->size);
                assert(buf_stride >= dst->shared->size);
                H5_CHECK_OVERFLOW(buf_stride, size_t, ssize_t);
                s_stride = d_stride = (ssize_t)buf_stride;
            } /* end if */
            else {
                H5_CHECK_OVERFLOW(src->shared->size, size_t, ssize_t);
                H5_CHECK_OVERFLOW(dst->shared->size, size_t, ssize_t);
                s_stride = (ssize_t)src->shared->size;
                d_stride = (ssize_t)dst->shared->size;
            } /* end else */
            if (bkg) {
                if (bkg_stride)
                    b_stride = (ssize_t)bkg_stride;
                else
                    b_stride = d_stride;
            } /* end if */
            else
                b_stride = 0;

            /* Get the size of the base types in src & dst */
            src_base_size = H5T_get_size(src->shared->parent);
            dst_base_size = H5T_get_size(dst->shared->parent);

            /* Set up conversion path for base elements */
            if (NULL == (tpath = H5T_path_find(src->shared->parent, dst->shared->parent)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
                            "unable to convert between src and dest datatypes");
            else if (!H5T_path_noop(tpath)) {
                if (NULL == (tsrc_cpy = H5T_copy(src->shared->parent, H5T_COPY_ALL)))
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCOPY, FAIL,
                                "unable to copy src base type for conversion");
                /* References need to know about the src file */
                if (tsrc_cpy->shared->type == H5T_REFERENCE)
                    if (H5T_set_loc(tsrc_cpy, src->shared->u.vlen.file, src->shared->u.vlen.loc) < 0)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTSET, FAIL, "can't set datatype location");

                if (NULL == (tdst_cpy = H5T_copy(dst->shared->parent, H5T_COPY_ALL)))
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCOPY, FAIL,
                                "unable to copy dst base type for conversion");
                /* References need to know about the dst file */
                if (tdst_cpy->shared->type == H5T_REFERENCE)
                    if (H5T_set_loc(tdst_cpy, dst->shared->u.vlen.file, dst->shared->u.vlen.loc) < 0)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTSET, FAIL, "can't set datatype location");

                /* Create IDs for the variable-length base datatypes if the conversion path
                 * uses an application conversion function or if a conversion exception function
                 * was provided.
                 */
                if (tpath->conv.is_app || conv_ctx->u.conv.cb_struct.func) {
                    if ((tsrc_id = H5I_register(H5I_DATATYPE, tsrc_cpy, false)) < 0)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, FAIL,
                                    "unable to register ID for source base datatype");
                    if ((tdst_id = H5I_register(H5I_DATATYPE, tdst_cpy, false)) < 0)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, FAIL,
                                    "unable to register ID for destination base datatype");
                }

                /* Update IDs in conversion context */
                tmp_conv_ctx.u.conv.src_type_id = tsrc_id;
                tmp_conv_ctx.u.conv.dst_type_id = tdst_id;
            } /* end else-if */
            else
                noop_conv = true;

            /* Check if we need a temporary buffer for this conversion */
            if ((parent_is_vlen = H5T_detect_class(dst->shared->parent, H5T_VLEN, false)) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_SYSTEM, FAIL,
                            "internal error when detecting variable-length class");
            if (tpath->cdata.need_bkg || parent_is_vlen) {
                /* Set up initial background buffer */
                tmp_buf_size = MAX(src_base_size, dst_base_size);
                if (NULL == (tmp_buf = H5FL_BLK_CALLOC(vlen_seq, tmp_buf_size)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                "memory allocation failed for type conversion");
            } /* end if */

            /* Get the allocation info */
            if (H5CX_get_vlen_alloc_info(&vl_alloc_info) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "unable to retrieve VL allocation info");

            /* Set flags to indicate we are writing to or reading from the file */
            if (dst->shared->u.vlen.file != NULL)
                write_to_file = true;

            /* Set the flag for nested VL case */
            if (write_to_file && parent_is_vlen && bkg != NULL)
                nested = true;

            /* Save info for unraveling on errors */
            orig_d_stride   = (size_t)d_stride;
            convert_forward = !(d_stride > s_stride);

            /* The outer loop of the type conversion macro, controlling which */
            /* direction the buffer is walked */
            while (nelmts > 0) {
                /* Check if we need to go backwards through the buffer */
                if (d_stride > s_stride) {
                    /* Sanity check */
                    assert(s_stride > 0);
                    assert(d_stride > 0);
                    assert(b_stride >= 0);

                    /* Compute the number of "safe" destination elements at */
                    /* the end of the buffer (Those which don't overlap with */
                    /* any source elements at the beginning of the buffer) */
                    safe =
                        nelmts - (((nelmts * (size_t)s_stride) + ((size_t)d_stride - 1)) / (size_t)d_stride);

                    /* If we're down to the last few elements, just wrap up */
                    /* with a "real" reverse copy */
                    if (safe < 2) {
                        s = (uint8_t *)buf + (nelmts - 1) * (size_t)s_stride;
                        d = (uint8_t *)buf + (nelmts - 1) * (size_t)d_stride;
                        if (bkg)
                            b = (uint8_t *)bkg + (nelmts - 1) * (size_t)b_stride;
                        s_stride = -s_stride;
                        d_stride = -d_stride;
                        b_stride = -b_stride;

                        safe = nelmts;
                    } /* end if */
                    else {
                        s = (uint8_t *)buf + (nelmts - safe) * (size_t)s_stride;
                        d = (uint8_t *)buf + (nelmts - safe) * (size_t)d_stride;
                        if (bkg)
                            b = (uint8_t *)bkg + (nelmts - safe) * (size_t)b_stride;
                    } /* end else */
                }     /* end if */
                else {
                    /* Single forward pass over all data */
                    s = d = (uint8_t *)buf;
                    b     = (uint8_t *)bkg;
                    safe  = nelmts;
                } /* end else */

                for (elmtno = 0; elmtno < safe; elmtno++) {
                    bool is_nil; /* Whether sequence is "nil" */

                    /* Check for "nil" source sequence */
                    if ((*(src->shared->u.vlen.cls->isnull))(src->shared->u.vlen.file, s, &is_nil) < 0)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "can't check if VL data is 'nil'");
                    else if (is_nil) {
                        /* Write "nil" sequence to destination location */
                        if ((*(dst->shared->u.vlen.cls->setnull))(dst->shared->u.vlen.file, d, b) < 0)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_WRITEERROR, FAIL, "can't set VL data to 'nil'");
                    } /* end else-if */
                    else {
                        size_t seq_len; /* The number of elements in the current sequence */

                        /* Get length of element sequences */
                        if ((*(src->shared->u.vlen.cls->getlen))(src->shared->u.vlen.file, s, &seq_len) < 0)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "bad sequence length");

                        /* If we are reading from memory and there is no conversion, just get the pointer to
                         * sequence */
                        if (write_to_file && noop_conv) {
                            /* Get direct pointer to sequence */
                            if (NULL == (conv_buf = (*(src->shared->u.vlen.cls->getptr))(s)))
                                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid source pointer");
                        } /* end if */
                        else {
                            size_t src_size, dst_size; /*source & destination total size in bytes*/

                            src_size = seq_len * src_base_size;
                            dst_size = seq_len * dst_base_size;

                            /* Check if conversion buffer is large enough, resize if
                             * necessary.  If the SEQ_LEN is 0, allocate a minimal size buffer.
                             */
                            if (!seq_len && !conv_buf) {
                                conv_buf_size = H5T_VLEN_MIN_CONF_BUF_SIZE;
                                if (NULL == (conv_buf = H5FL_BLK_CALLOC(vlen_seq, conv_buf_size)))
                                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                                                "memory allocation failed for type conversion");
                            } /* end if */
                            else if (conv_buf_size < MAX(src_size, dst_size)) {
                                /* Only allocate conversion buffer in H5T_VLEN_MIN_CONF_BUF_SIZE increments */
                                conv_buf_size = ((MAX(src_size, dst_size) / H5T_VLEN_MIN_CONF_BUF_SIZE) + 1) *
                                                H5T_VLEN_MIN_CONF_BUF_SIZE;
                                if (NULL == (conv_buf = H5FL_BLK_REALLOC(vlen_seq, conv_buf, conv_buf_size)))
                                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                                                "memory allocation failed for type conversion");
                                memset(conv_buf, 0, conv_buf_size);
                            } /* end else-if */

                            /* Read in VL sequence */
                            if ((*(src->shared->u.vlen.cls->read))(src->shared->u.vlen.file, s, conv_buf,
                                                                   src_size) < 0)
                                HGOTO_ERROR(H5E_DATATYPE, H5E_READERROR, FAIL, "can't read VL data");
                        } /* end else */

                        if (!noop_conv) {
                            /* Check if temporary buffer is large enough, resize if necessary */
                            /* (Chain off the conversion buffer size) */
                            if (tmp_buf && tmp_buf_size < conv_buf_size) {
                                /* Set up initial background buffer */
                                tmp_buf_size = conv_buf_size;
                                if (NULL == (tmp_buf = H5FL_BLK_REALLOC(vlen_seq, tmp_buf, tmp_buf_size)))
                                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                                                "memory allocation failed for type conversion");
                                memset(tmp_buf, 0, tmp_buf_size);
                            } /* end if */

                            /* If we are writing and there is a nested VL type, read
                             * the sequence into the background buffer */
                            if (nested) {
                                /* Sanity check */
                                assert(write_to_file);

                                /* Get length of background element sequence */
                                if ((*(dst->shared->u.vlen.cls->getlen))(dst->shared->u.vlen.file, b,
                                                                         &bg_seq_len) < 0)
                                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "bad sequence length");

                                /* Read sequence if length > 0 */
                                if (bg_seq_len > 0) {
                                    if (tmp_buf_size < (bg_seq_len * MAX(src_base_size, dst_base_size))) {
                                        tmp_buf_size = (bg_seq_len * MAX(src_base_size, dst_base_size));
                                        if (NULL ==
                                            (tmp_buf = H5FL_BLK_REALLOC(vlen_seq, tmp_buf, tmp_buf_size)))
                                            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                                                        "memory allocation failed for type conversion");
                                        memset(tmp_buf, 0, tmp_buf_size);
                                    } /* end if */

                                    /* Read in background VL sequence */
                                    if ((*(dst->shared->u.vlen.cls->read))(dst->shared->u.vlen.file, b,
                                                                           tmp_buf,
                                                                           bg_seq_len * dst_base_size) < 0)
                                        HGOTO_ERROR(H5E_DATATYPE, H5E_READERROR, FAIL, "can't read VL data");
                                } /* end if */

                                /* If the sequence gets shorter, pad out the original sequence with zeros */
                                if (bg_seq_len < seq_len)
                                    memset((uint8_t *)tmp_buf + dst_base_size * bg_seq_len, 0,
                                           (seq_len - bg_seq_len) * dst_base_size);
                            } /* end if */

                            /* Convert VL sequence */
                            tmp_conv_ctx.u.conv.recursive = true;
                            if (H5T_convert_with_ctx(tpath, tsrc_cpy, tdst_cpy, &tmp_conv_ctx, seq_len,
                                                     (size_t)0, (size_t)0, conv_buf, tmp_buf) < 0)
                                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL,
                                            "datatype conversion failed");
                            tmp_conv_ctx.u.conv.recursive = false;
                        } /* end if */

                        /* Write sequence to destination location */
                        if ((*(dst->shared->u.vlen.cls->write))(dst->shared->u.vlen.file, &vl_alloc_info, d,
                                                                conv_buf, b, seq_len, dst_base_size) < 0)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_WRITEERROR, FAIL, "can't write VL data");

                        if (!noop_conv) {
                            /* For nested VL case, free leftover heap objects from the deeper level if the
                             * length of new data elements is shorter than the old data elements.*/
                            if (nested && seq_len < bg_seq_len) {
                                uint8_t *tmp;
                                size_t   u;

                                /* Sanity check */
                                assert(write_to_file);

                                tmp = (uint8_t *)tmp_buf + seq_len * dst_base_size;
                                for (u = seq_len; u < bg_seq_len; u++, tmp += dst_base_size) {
                                    /* Recursively free destination data */
                                    if (H5T__conv_vlen_nested_free(tmp, dst->shared->parent) < 0)
                                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREMOVE, FAIL,
                                                    "unable to remove heap object");
                                } /* end for */
                            }     /* end if */
                        }         /* end if */
                    }             /* end else */

                    /* Indicate that elements have been converted, in case of error */
                    conversions_made = true;

                    /* Advance pointers */
                    s += s_stride;
                    d += d_stride;

                    if (b)
                        b += b_stride;
                } /* end for */

                /* Decrement number of elements left to convert */
                nelmts -= safe;
            } /* end while */

            break;

        default: /* Some other command we don't know about yet.*/
            HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unknown conversion command");
    } /* end switch */

done:
    /* Release converted elements on error */
    if (ret_value < 0 && conversions_made) {
        size_t dest_count;

        /* Set up for first pass to destroy references */
        if (nelmts < orig_nelmts || (convert_forward && elmtno < safe)) {
            dest_count = orig_nelmts - nelmts;

            /* Set pointer to correct location, based on direction chosen */
            if (convert_forward) {
                d = (uint8_t *)buf;
                dest_count += elmtno; /* Include partial iteration in first pass, for forward conversions */
            }
            else
                d = (uint8_t *)buf + (nelmts * orig_d_stride);

            /* Destroy vlen elements that have already been converted */
            while (dest_count > 0) {
                H5T_vlen_reclaim_elmt(d, dst); /* Ignore errors at this point */
                d += orig_d_stride;
                dest_count--;
            }
        }

        /* Do any remaining partial iteration, if converting backwards */
        if (!convert_forward && elmtno < safe) {
            dest_count = elmtno;

            /* Set pointer to correct location */
            if (d_stride > 0)
                d = (uint8_t *)buf + ((nelmts - safe) * orig_d_stride);
            else
                d = (uint8_t *)buf + ((nelmts - elmtno) * orig_d_stride);

            /* Destroy references that have already been converted */
            while (dest_count > 0) {
                H5T_vlen_reclaim_elmt(d, dst); /* Ignore errors at this point */
                d += orig_d_stride;
                dest_count--;
            }
        }
    }

    if (tsrc_id >= 0) {
        if (H5I_dec_ref(tsrc_id) < 0)
            HDONE_ERROR(H5E_DATATYPE, H5E_CANTDEC, FAIL, "can't decrement reference on temporary ID");
    }
    else if (tsrc_cpy) {
        if (H5T_close(tsrc_cpy) < 0)
            HDONE_ERROR(H5E_DATATYPE, H5E_CANTCLOSEOBJ, FAIL, "can't close temporary datatype");
    }
    if (tdst_id >= 0) {
        if (H5I_dec_ref(tdst_id) < 0)
            HDONE_ERROR(H5E_DATATYPE, H5E_CANTDEC, FAIL, "can't decrement reference on temporary ID");
    }
    else if (tdst_cpy) {
        if (H5T_close(tdst_cpy) < 0)
            HDONE_ERROR(H5E_DATATYPE, H5E_CANTCLOSEOBJ, FAIL, "can't close temporary datatype");
    }

    /* If the conversion buffer doesn't need to be freed, reset its pointer */
    if (write_to_file && noop_conv)
        conv_buf = NULL;
    /* Release the conversion buffer (always allocated, except on errors) */
    if (conv_buf)
        conv_buf = H5FL_BLK_FREE(vlen_seq, conv_buf);
    /* Release the background buffer, if we have one */
    if (tmp_buf)
        tmp_buf = H5FL_BLK_FREE(vlen_seq, tmp_buf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5T__conv_vlen() */

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_array
 *
 * Purpose:    Converts between array datatypes in memory and on disk.
 *        This is a soft conversion function.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_array(const H5T_t *src, const H5T_t *dst, H5T_cdata_t *cdata,
                const H5T_conv_ctx_t H5_ATTR_UNUSED *conv_ctx, size_t nelmts, size_t buf_stride,
                size_t bkg_stride, void *_buf, void *_bkg)
{
    H5T_conv_array_t *priv         = NULL;             /* Private conversion data */
    H5T_conv_ctx_t    tmp_conv_ctx = {0};              /* Temporary conversion context */
    H5T_t            *tsrc_cpy     = NULL;             /* Temporary copy of source base datatype */
    H5T_t            *tdst_cpy     = NULL;             /* Temporary copy of destination base datatype */
    hid_t             tsrc_id      = H5I_INVALID_HID;  /* Temporary type atom */
    hid_t             tdst_id      = H5I_INVALID_HID;  /* Temporary type atom */
    uint8_t          *sp, *dp, *bp;                    /* Source, dest, and bkg traversal ptrs */
    ssize_t           src_delta, dst_delta, bkg_delta; /* Source, dest, and bkg strides */
    int               direction;                       /* Direction of traversal */
    herr_t            ret_value = SUCCEED;             /* Return value */

    FUNC_ENTER_PACKAGE

    switch (cdata->command) {
        case H5T_CONV_INIT:
            /*
             * First, determine if this conversion function applies to the
             * conversion path SRC-->DST.  If not, return failure;
             * otherwise initialize the `priv' field of `cdata' with
             * information that remains (almost) constant for this
             * conversion path.
             */
            if (NULL == src || NULL == dst)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype");
            assert(H5T_ARRAY == src->shared->type);
            assert(H5T_ARRAY == dst->shared->type);

            /* Check the number and sizes of the dimensions */
            if (src->shared->u.array.ndims != dst->shared->u.array.ndims)
                HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
                            "array datatypes do not have the same number of dimensions");
            for (unsigned u = 0; u < src->shared->u.array.ndims; u++)
                if (src->shared->u.array.dim[u] != dst->shared->u.array.dim[u])
                    HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
                                "array datatypes do not have the same sizes of dimensions");

            /* Initialize parent type conversion if necessary. We need to do this here because we need to
             * report whether we need a background buffer or not. */
            if (!cdata->priv) {
                /* Allocate private data */
                if (NULL == (priv = (H5T_conv_array_t *)(cdata->priv = calloc(1, sizeof(*priv)))))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

                /* Find conversion path between parent types */
                if (NULL == (priv->tpath = H5T_path_find(src->shared->parent, dst->shared->parent))) {
                    free(priv);
                    cdata->priv = NULL;
                    HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
                                "unable to convert between src and dest datatype");
                }

                /* Array datatypes don't need a background buffer by themselves, but the parent type might.
                 * Pass the need_bkg field through to the upper layer. */
                cdata->need_bkg = priv->tpath->cdata.need_bkg;
            }

            break;

        case H5T_CONV_FREE:
            /*
             * Free private data
             */
            free(cdata->priv);
            cdata->priv = NULL;

            break;

        case H5T_CONV_CONV:
            /*
             * Conversion.
             */
            if (NULL == src || NULL == dst)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype");
            if (NULL == conv_ctx)
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADVALUE, FAIL, "invalid datatype conversion context pointer");
            priv = (H5T_conv_array_t *)cdata->priv;

            /* Initialize temporary conversion context */
            tmp_conv_ctx = *conv_ctx;

            /*
             * Do we process the values from beginning to end or vice
             * versa? Also, how many of the elements have the source and
             * destination areas overlapping?
             */
            if (src->shared->size >= dst->shared->size || buf_stride > 0) {
                sp = dp   = (uint8_t *)_buf;
                bp        = _bkg;
                direction = 1;
            }
            else {
                sp = (uint8_t *)_buf + (nelmts - 1) * (buf_stride ? buf_stride : src->shared->size);
                dp = (uint8_t *)_buf + (nelmts - 1) * (buf_stride ? buf_stride : dst->shared->size);
                bp = _bkg ? (uint8_t *)_bkg + (nelmts - 1) * (bkg_stride ? bkg_stride : dst->shared->size)
                          : NULL;
                direction = -1;
            }

            /*
             * Direction & size of buffer traversal.
             */
            H5_CHECK_OVERFLOW(buf_stride, size_t, ssize_t);
            H5_CHECK_OVERFLOW(src->shared->size, size_t, ssize_t);
            H5_CHECK_OVERFLOW(dst->shared->size, size_t, ssize_t);
            src_delta = (ssize_t)direction * (ssize_t)(buf_stride ? buf_stride : src->shared->size);
            dst_delta = (ssize_t)direction * (ssize_t)(buf_stride ? buf_stride : dst->shared->size);
            bkg_delta = (ssize_t)direction * (ssize_t)(bkg_stride ? bkg_stride : dst->shared->size);

            /* Set up conversion path for base elements */
            if (!H5T_path_noop(priv->tpath)) {
                if (NULL == (tsrc_cpy = H5T_copy(src->shared->parent, H5T_COPY_ALL)))
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCOPY, FAIL,
                                "unable to copy src base type for conversion");

                if (NULL == (tdst_cpy = H5T_copy(dst->shared->parent, H5T_COPY_ALL)))
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCOPY, FAIL,
                                "unable to copy dst base type for conversion");

                /* Create IDs for the array base datatypes if the conversion path uses an
                 * application conversion function or if a conversion exception function
                 * was provided.
                 */
                if (priv->tpath->conv.is_app || conv_ctx->u.conv.cb_struct.func) {
                    if ((tsrc_id = H5I_register(H5I_DATATYPE, tsrc_cpy, false)) < 0)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, FAIL,
                                    "unable to register ID for source base datatype");
                    if ((tdst_id = H5I_register(H5I_DATATYPE, tdst_cpy, false)) < 0)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, FAIL,
                                    "unable to register ID for destination base datatype");
                }

                /* Update IDs in conversion context */
                tmp_conv_ctx.u.conv.src_type_id = tsrc_id;
                tmp_conv_ctx.u.conv.dst_type_id = tdst_id;
            }

            /* Perform the actual conversion */
            tmp_conv_ctx.u.conv.recursive = true;
            for (size_t elmtno = 0; elmtno < nelmts; elmtno++) {
                /* Copy the source array into the correct location for the destination */
                memmove(dp, sp, src->shared->size);

                /* Convert array */
                if (H5T_convert_with_ctx(priv->tpath, tsrc_cpy, tdst_cpy, &tmp_conv_ctx,
                                         src->shared->u.array.nelem, (size_t)0, (size_t)0, dp, bp) < 0)
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "datatype conversion failed");

                /* Advance the source, destination, and background pointers */
                sp += src_delta;
                dp += dst_delta;
                if (bp)
                    bp += bkg_delta;
            } /* end for */
            tmp_conv_ctx.u.conv.recursive = false;

            break;

        default: /* Some other command we don't know about yet.*/
            HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unknown conversion command");
    } /* end switch */

done:
    if (tsrc_id >= 0) {
        if (H5I_dec_ref(tsrc_id) < 0)
            HDONE_ERROR(H5E_DATATYPE, H5E_CANTDEC, FAIL, "can't decrement reference on temporary ID");
    }
    else if (tsrc_cpy) {
        if (H5T_close(tsrc_cpy) < 0)
            HDONE_ERROR(H5E_DATATYPE, H5E_CANTCLOSEOBJ, FAIL, "can't close temporary datatype");
    }
    if (tdst_id >= 0) {
        if (H5I_dec_ref(tdst_id) < 0)
            HDONE_ERROR(H5E_DATATYPE, H5E_CANTDEC, FAIL, "can't decrement reference on temporary ID");
    }
    else if (tdst_cpy) {
        if (H5T_close(tdst_cpy) < 0)
            HDONE_ERROR(H5E_DATATYPE, H5E_CANTCLOSEOBJ, FAIL, "can't close temporary datatype");
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5T__conv_array() */

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ref
 *
 * Purpose: Converts between reference datatypes in memory and on disk.
 *      This is a soft conversion function.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ref(const H5T_t *src, const H5T_t *dst, H5T_cdata_t *cdata,
              const H5T_conv_ctx_t H5_ATTR_UNUSED *conv_ctx, size_t nelmts, size_t buf_stride,
              size_t bkg_stride, void *buf, void *bkg)
{
    uint8_t *s        = NULL;        /* source buffer                        */
    uint8_t *d        = NULL;        /* destination buffer                   */
    uint8_t *b        = NULL;        /* background buffer                    */
    ssize_t  s_stride = 0;           /* src stride                  */
    ssize_t  d_stride = 0;           /* dst stride                  */
    ssize_t  b_stride;               /* bkg stride                           */
    size_t   safe          = 0;      /* how many elements are safe to process in each pass */
    void    *conv_buf      = NULL;   /* temporary conversion buffer          */
    size_t   conv_buf_size = 0;      /* size of conversion buffer in bytes   */
    size_t   elmtno        = 0;      /* element number counter               */
    size_t   orig_d_stride = 0;      /* Original destination stride (used for error handling) */
    size_t   orig_nelmts   = nelmts; /* Original # of elements to convert (used for error handling) */
    bool     convert_forward =
        true; /* Current direction of conversion (forward or backward, used for error handling) */
    bool conversions_made =
        false; /* Flag to indicate conversions have been performed, used for error handling */
    herr_t ret_value = SUCCEED; /* return value                         */

    FUNC_ENTER_PACKAGE

    switch (cdata->command) {
        case H5T_CONV_INIT:
            /*
             * First, determine if this conversion function applies to the
             * conversion path SRC-->DST.  If not, return failure;
             * otherwise initialize the `priv' field of `cdata' with
             * information that remains (almost) constant for this
             * conversion path.
             */
            if (NULL == src || NULL == dst)
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "not a datatype");
            if (H5T_REFERENCE != src->shared->type)
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "not a H5T_REFERENCE datatype");
            if (H5T_REFERENCE != dst->shared->type)
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "not a H5T_REFERENCE datatype");
            /* Only allow for source reference that is not an opaque type, destination must be opaque */
            if (!dst->shared->u.atomic.u.r.opaque)
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADTYPE, FAIL, "not an H5T_STD_REF datatype");

            /* Reference types don't need a background buffer */
            cdata->need_bkg = H5T_BKG_NO;
            break;

        case H5T_CONV_FREE:
            break;

        case H5T_CONV_CONV: {
            /*
             * Conversion.
             */
            if (NULL == src || NULL == dst)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype");

            assert(src->shared->u.atomic.u.r.cls);

            /* Initialize source & destination strides */
            if (buf_stride) {
                assert(buf_stride >= src->shared->size);
                assert(buf_stride >= dst->shared->size);
                H5_CHECK_OVERFLOW(buf_stride, size_t, ssize_t);
                s_stride = d_stride = (ssize_t)buf_stride;
            } /* end if */
            else {
                H5_CHECK_OVERFLOW(src->shared->size, size_t, ssize_t);
                H5_CHECK_OVERFLOW(dst->shared->size, size_t, ssize_t);
                s_stride = (ssize_t)src->shared->size;
                d_stride = (ssize_t)dst->shared->size;
            } /* end else */
            if (bkg) {
                if (bkg_stride)
                    b_stride = (ssize_t)bkg_stride;
                else
                    b_stride = d_stride;
            } /* end if */
            else
                b_stride = 0;

            /* Save info for unraveling on errors */
            orig_d_stride   = (size_t)d_stride;
            convert_forward = !(d_stride > s_stride);

            /* The outer loop of the type conversion macro, controlling which */
            /* direction the buffer is walked */
            while (nelmts > 0) {
                /* Check if we need to go backwards through the buffer */
                if (d_stride > s_stride) {
                    /* Sanity check */
                    assert(s_stride > 0);
                    assert(d_stride > 0);
                    assert(b_stride >= 0);

                    /* Compute the number of "safe" destination elements at */
                    /* the end of the buffer (Those which don't overlap with */
                    /* any source elements at the beginning of the buffer) */
                    safe =
                        nelmts - (((nelmts * (size_t)s_stride) + ((size_t)d_stride - 1)) / (size_t)d_stride);

                    /* If we're down to the last few elements, just wrap up */
                    /* with a "real" reverse copy */
                    if (safe < 2) {
                        s = (uint8_t *)buf + (nelmts - 1) * (size_t)s_stride;
                        d = (uint8_t *)buf + (nelmts - 1) * (size_t)d_stride;
                        if (bkg)
                            b = (uint8_t *)bkg + (nelmts - 1) * (size_t)b_stride;
                        s_stride = -s_stride;
                        d_stride = -d_stride;
                        b_stride = -b_stride;

                        safe = nelmts;
                    } /* end if */
                    else {
                        s = (uint8_t *)buf + (nelmts - safe) * (size_t)s_stride;
                        d = (uint8_t *)buf + (nelmts - safe) * (size_t)d_stride;
                        if (bkg)
                            b = (uint8_t *)bkg + (nelmts - safe) * (size_t)b_stride;
                    } /* end else */
                }     /* end if */
                else {
                    /* Single forward pass over all data */
                    s = d = (uint8_t *)buf;
                    b     = (uint8_t *)bkg;
                    safe  = nelmts;
                } /* end else */

                for (elmtno = 0; elmtno < safe; elmtno++) {
                    size_t buf_size;
                    bool   dst_copy = false;
                    bool   is_nil; /* Whether reference is "nil" */

                    /* Check for "nil" source reference */
                    if ((*(src->shared->u.atomic.u.r.cls->isnull))(src->shared->u.atomic.u.r.file, s,
                                                                   &is_nil) < 0)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL,
                                    "can't check if reference data is 'nil'");

                    if (is_nil) {
                        /* Write "nil" reference to destination location */
                        if ((*(dst->shared->u.atomic.u.r.cls->setnull))(dst->shared->u.atomic.u.r.file, d,
                                                                        b) < 0)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_WRITEERROR, FAIL,
                                        "can't set reference data to 'nil'");
                    } /* end else-if */
                    else {
                        /* Get size of references */
                        if (0 == (buf_size = src->shared->u.atomic.u.r.cls->getsize(
                                      src->shared->u.atomic.u.r.file, s, src->shared->size,
                                      dst->shared->u.atomic.u.r.file, &dst_copy)))
                            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "unable to obtain size of reference");

                        /* Check if conversion buffer is large enough, resize if necessary. */
                        if (conv_buf_size < buf_size) {
                            conv_buf_size = buf_size;
                            if (NULL == (conv_buf = H5FL_BLK_REALLOC(ref_seq, conv_buf, conv_buf_size)))
                                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                                            "memory allocation failed for type conversion");
                            memset(conv_buf, 0, conv_buf_size);
                        } /* end if */

                        if (dst_copy && (src->shared->u.atomic.u.r.loc == H5T_LOC_DISK))
                            H5MM_memcpy(conv_buf, s, buf_size);
                        else {
                            /* Read reference */
                            if (src->shared->u.atomic.u.r.cls->read(
                                    src->shared->u.atomic.u.r.file, s, src->shared->size,
                                    dst->shared->u.atomic.u.r.file, conv_buf, buf_size) < 0)
                                HGOTO_ERROR(H5E_DATATYPE, H5E_READERROR, FAIL, "can't read reference data");
                        } /* end else */

                        if (dst_copy && (dst->shared->u.atomic.u.r.loc == H5T_LOC_DISK))
                            H5MM_memcpy(d, conv_buf, buf_size);
                        else {
                            /* Write reference to destination location */
                            if (dst->shared->u.atomic.u.r.cls->write(
                                    src->shared->u.atomic.u.r.file, conv_buf, buf_size,
                                    src->shared->u.atomic.u.r.rtype, dst->shared->u.atomic.u.r.file, d,
                                    dst->shared->size, b) < 0)
                                HGOTO_ERROR(H5E_DATATYPE, H5E_WRITEERROR, FAIL, "can't write reference data");
                        } /* end else */
                    }     /* end else */

                    /* Indicate that elements have been converted, in case of error */
                    conversions_made = true;

                    /* Advance pointers */
                    s += s_stride;
                    d += d_stride;

                    if (b)
                        b += b_stride;
                } /* end for */

                /* Decrement number of elements left to convert */
                nelmts -= safe;
            } /* end while */
        }     /* end case */
        break;

        default: /* Some other command we don't know about yet.*/
            HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unknown conversion command");
    } /* end switch */

done:
    /* Release converted elements on error */
    if (ret_value < 0 && conversions_made) {
        H5R_ref_priv_t ref_priv;
        size_t         dest_count;

        /* Set up for first pass to destroy references */
        if (nelmts < orig_nelmts || (convert_forward && elmtno < safe)) {
            dest_count = orig_nelmts - nelmts;

            /* Set pointer to correct location, based on direction chosen */
            if (convert_forward) {
                d = (uint8_t *)buf;
                dest_count += elmtno; /* Include partial iteration in first pass, for forward conversions */
            }
            else
                d = (uint8_t *)buf + (nelmts * orig_d_stride);

            /* Destroy references that have already been converted */
            while (dest_count > 0) {
                memcpy(&ref_priv, d, sizeof(H5R_ref_priv_t));
                H5R__destroy(&ref_priv); /* Ignore errors at this point */
                d += orig_d_stride;
                dest_count--;
            }
        }

        /* Do any remaining partial iteration, if converting backwards */
        if (!convert_forward && elmtno < safe) {
            dest_count = elmtno;

            /* Set pointer to correct location */
            if (d_stride > 0)
                d = (uint8_t *)buf + ((nelmts - safe) * orig_d_stride);
            else
                d = (uint8_t *)buf + ((nelmts - elmtno) * orig_d_stride);

            /* Destroy references that have already been converted */
            while (dest_count > 0) {
                memcpy(&ref_priv, d, sizeof(H5R_ref_priv_t));
                H5R__destroy(&ref_priv); /* Ignore errors at this point */
                d += orig_d_stride;
                dest_count--;
            }
        }
    }

    /* Release the conversion buffer (always allocated, except on errors) */
    if (conv_buf)
        conv_buf = H5FL_BLK_FREE(ref_seq, conv_buf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5T__conv_ref() */

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_i_i
 *
 * Purpose:    Convert one integer type to another.  This is the catch-all
 *        function for integer conversions and is probably not
 *        particularly fast.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_i_i(const H5T_t *src, const H5T_t *dst, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
              size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
              void H5_ATTR_UNUSED *bkg)
{
    ssize_t        src_delta, dst_delta; /*source & destination stride    */
    int            direction;            /*direction of traversal    */
    size_t         elmtno;               /*element number        */
    size_t         half_size;            /*half the type size        */
    size_t         olap;                 /*num overlapping elements    */
    uint8_t       *s, *sp, *d, *dp;      /*source and dest traversal ptrs*/
    uint8_t       *src_rev  = NULL;      /*order-reversed source buffer  */
    uint8_t        dbuf[64] = {0};       /*temp destination buffer    */
    size_t         first;
    ssize_t        sfirst;              /*a signed version of `first'    */
    size_t         i;                   /*Local index variables         */
    H5T_conv_ret_t except_ret;          /*return of callback function   */
    bool           reverse;             /*if reverse the order of destination        */
    herr_t         ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    switch (cdata->command) {
        case H5T_CONV_INIT:
            if (NULL == src || NULL == dst)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype");
            if (H5T_ORDER_LE != src->shared->u.atomic.order && H5T_ORDER_BE != src->shared->u.atomic.order)
                HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unsupported byte order");
            if (H5T_ORDER_LE != dst->shared->u.atomic.order && H5T_ORDER_BE != dst->shared->u.atomic.order)
                HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unsupported byte order");
            if (dst->shared->size > sizeof dbuf)
                HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "destination size is too large");
            cdata->need_bkg = H5T_BKG_NO;
            break;

        case H5T_CONV_FREE:
            break;

        case H5T_CONV_CONV:
            if (NULL == src || NULL == dst)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype");
            if (NULL == conv_ctx)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid datatype conversion context pointer");

            /*
             * Do we process the values from beginning to end or vice versa? Also,
             * how many of the elements have the source and destination areas
             * overlapping?
             */
            if (src->shared->size == dst->shared->size || buf_stride) {
                sp = dp   = (uint8_t *)buf;
                direction = 1;
                olap      = nelmts;
            }
            else if (src->shared->size >= dst->shared->size) {
                double olap_d =
                    ceil((double)(dst->shared->size) / (double)(src->shared->size - dst->shared->size));

                olap = (size_t)olap_d;
                sp = dp   = (uint8_t *)buf;
                direction = 1;
            }
            else {
                double olap_d =
                    ceil((double)(src->shared->size) / (double)(dst->shared->size - src->shared->size));
                olap      = (size_t)olap_d;
                sp        = (uint8_t *)buf + (nelmts - 1) * src->shared->size;
                dp        = (uint8_t *)buf + (nelmts - 1) * dst->shared->size;
                direction = -1;
            }

            /*
             * Direction & size of buffer traversal.
             */
            H5_CHECK_OVERFLOW(buf_stride, size_t, ssize_t);
            H5_CHECK_OVERFLOW(src->shared->size, size_t, ssize_t);
            H5_CHECK_OVERFLOW(dst->shared->size, size_t, ssize_t);
            src_delta = (ssize_t)direction * (ssize_t)(buf_stride ? buf_stride : src->shared->size);
            dst_delta = (ssize_t)direction * (ssize_t)(buf_stride ? buf_stride : dst->shared->size);

            /* Allocate space for order-reversed source buffer */
            src_rev = (uint8_t *)H5MM_calloc(src->shared->size);

            /* The conversion loop */
            for (elmtno = 0; elmtno < nelmts; elmtno++) {

                /*
                 * If the source and destination buffers overlap then use a
                 * temporary buffer for the destination.
                 */
                if (direction > 0) {
                    s = sp;
                    d = elmtno < olap ? dbuf : dp;
                }
                else {
                    s = sp;
                    d = elmtno + olap >= nelmts ? dbuf : dp;
                }
#ifndef NDEBUG
                /* I don't quite trust the overlap calculations yet --rpm */
                if (d == dbuf) {
                    assert((dp >= sp && dp < sp + src->shared->size) ||
                           (sp >= dp && sp < dp + dst->shared->size));
                }
                else {
                    assert((dp < sp && dp + dst->shared->size <= sp) ||
                           (sp < dp && sp + src->shared->size <= dp));
                }
#endif

                /*
                 * Put the data in little endian order so our loops aren't so
                 * complicated.  We'll do all the conversion stuff assuming
                 * little endian and then we'll fix the order at the end.
                 */
                if (H5T_ORDER_BE == src->shared->u.atomic.order) {
                    half_size = src->shared->size / 2;
                    for (i = 0; i < half_size; i++) {
                        uint8_t tmp                    = s[src->shared->size - (i + 1)];
                        s[src->shared->size - (i + 1)] = s[i];
                        s[i]                           = tmp;
                    }
                }

                /*
                 * What is the bit number for the msb bit of S which is set? The
                 * bit number is relative to the significant part of the number.
                 */
                sfirst = H5T__bit_find(s, src->shared->u.atomic.offset, src->shared->u.atomic.prec,
                                       H5T_BIT_MSB, true);
                first  = (size_t)sfirst;

                /* Set these variables to default */
                except_ret = H5T_CONV_UNHANDLED;
                reverse    = true;

                if (sfirst < 0) {
                    /*
                     * The source has no bits set and must therefore be zero.
                     * Set the destination to zero.
                     */
                    H5T__bit_set(d, dst->shared->u.atomic.offset, dst->shared->u.atomic.prec, false);
                }
                else if (H5T_SGN_NONE == src->shared->u.atomic.u.i.sign &&
                         H5T_SGN_NONE == dst->shared->u.atomic.u.i.sign) {
                    /*
                     * Source and destination are both unsigned, but if the
                     * source has more precision bits than the destination then
                     * it's possible to overflow.  When overflow occurs the
                     * destination will be set to the maximum possible value.
                     */
                    if (src->shared->u.atomic.prec <= dst->shared->u.atomic.prec) {
                        H5T__bit_copy(d, dst->shared->u.atomic.offset, s, src->shared->u.atomic.offset,
                                      src->shared->u.atomic.prec);
                        H5T__bit_set(d, dst->shared->u.atomic.offset + src->shared->u.atomic.prec,
                                     dst->shared->u.atomic.prec - src->shared->u.atomic.prec, false);
                    }
                    else if (first >= dst->shared->u.atomic.prec) {
                        /*overflow*/
                        if (conv_ctx->u.conv.cb_struct
                                .func) { /*If user's exception handler is present, use it*/
                            H5T__reverse_order(src_rev, s, src->shared->size,
                                               src->shared->u.atomic.order); /*reverse order first*/
                            except_ret = (conv_ctx->u.conv.cb_struct.func)(
                                H5T_CONV_EXCEPT_RANGE_HI, conv_ctx->u.conv.src_type_id,
                                conv_ctx->u.conv.dst_type_id, src_rev, d,
                                conv_ctx->u.conv.cb_struct.user_data);
                        }

                        if (except_ret == H5T_CONV_UNHANDLED) {
                            H5T__bit_set(d, dst->shared->u.atomic.offset, dst->shared->u.atomic.prec, true);
                        }
                        else if (except_ret == H5T_CONV_ABORT)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL,
                                        "can't handle conversion exception");
                        else if (except_ret == H5T_CONV_HANDLED)
                            /*Don't reverse because user handles it already*/
                            reverse = false;
                    }
                    else {
                        H5T__bit_copy(d, dst->shared->u.atomic.offset, s, src->shared->u.atomic.offset,
                                      dst->shared->u.atomic.prec);
                    }
                }
                else if (H5T_SGN_2 == src->shared->u.atomic.u.i.sign &&
                         H5T_SGN_NONE == dst->shared->u.atomic.u.i.sign) {
                    /*
                     * If the source is signed and the destination isn't then we
                     * can have overflow if the source contains more bits than
                     * the destination (destination is set to the maximum
                     * possible value) or overflow if the source is negative
                     * (destination is set to zero).
                     */
                    if (first + 1 == src->shared->u.atomic.prec) {
                        /*overflow - source is negative*/
                        if (conv_ctx->u.conv.cb_struct
                                .func) { /*If user's exception handler is present, use it*/
                            H5T__reverse_order(src_rev, s, src->shared->size,
                                               src->shared->u.atomic.order); /*reverse order first*/
                            except_ret = (conv_ctx->u.conv.cb_struct.func)(
                                H5T_CONV_EXCEPT_RANGE_LOW, conv_ctx->u.conv.src_type_id,
                                conv_ctx->u.conv.dst_type_id, src_rev, d,
                                conv_ctx->u.conv.cb_struct.user_data);
                        }

                        if (except_ret == H5T_CONV_UNHANDLED) {
                            H5T__bit_set(d, dst->shared->u.atomic.offset, dst->shared->u.atomic.prec, false);
                        }
                        else if (except_ret == H5T_CONV_ABORT)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL,
                                        "can't handle conversion exception");
                        else if (except_ret == H5T_CONV_HANDLED)
                            /*Don't reverse because user handles it already*/
                            reverse = false;
                    }
                    else if (src->shared->u.atomic.prec < dst->shared->u.atomic.prec) {
                        H5T__bit_copy(d, dst->shared->u.atomic.offset, s, src->shared->u.atomic.offset,
                                      src->shared->u.atomic.prec - 1);
                        H5T__bit_set(d, dst->shared->u.atomic.offset + src->shared->u.atomic.prec - 1,
                                     (dst->shared->u.atomic.prec - src->shared->u.atomic.prec) + 1, false);
                    }
                    else if (first >= dst->shared->u.atomic.prec) {
                        /*overflow - source is positive*/
                        if (conv_ctx->u.conv.cb_struct
                                .func) { /*If user's exception handler is present, use it*/
                            H5T__reverse_order(src_rev, s, src->shared->size,
                                               src->shared->u.atomic.order); /*reverse order first*/
                            except_ret = (conv_ctx->u.conv.cb_struct.func)(
                                H5T_CONV_EXCEPT_RANGE_HI, conv_ctx->u.conv.src_type_id,
                                conv_ctx->u.conv.dst_type_id, src_rev, d,
                                conv_ctx->u.conv.cb_struct.user_data);
                        }

                        if (except_ret == H5T_CONV_UNHANDLED)
                            H5T__bit_set(d, dst->shared->u.atomic.offset, dst->shared->u.atomic.prec, true);
                        else if (except_ret == H5T_CONV_ABORT)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL,
                                        "can't handle conversion exception");
                        else if (except_ret == H5T_CONV_HANDLED)
                            /*Don't reverse because user handles it already*/
                            reverse = false;
                    }
                    else {
                        H5T__bit_copy(d, dst->shared->u.atomic.offset, s, src->shared->u.atomic.offset,
                                      dst->shared->u.atomic.prec);
                    }
                }
                else if (H5T_SGN_NONE == src->shared->u.atomic.u.i.sign &&
                         H5T_SGN_2 == dst->shared->u.atomic.u.i.sign) {
                    /*
                     * If the source is not signed but the destination is then
                     * overflow can occur in which case the destination is set to
                     * the largest possible value (all bits set except the msb).
                     */
                    if (first + 1 >= dst->shared->u.atomic.prec) {
                        /*overflow*/
                        if (conv_ctx->u.conv.cb_struct
                                .func) { /*If user's exception handler is present, use it*/
                            H5T__reverse_order(src_rev, s, src->shared->size,
                                               src->shared->u.atomic.order); /*reverse order first*/
                            except_ret = (conv_ctx->u.conv.cb_struct.func)(
                                H5T_CONV_EXCEPT_RANGE_HI, conv_ctx->u.conv.src_type_id,
                                conv_ctx->u.conv.dst_type_id, src_rev, d,
                                conv_ctx->u.conv.cb_struct.user_data);
                        }

                        if (except_ret == H5T_CONV_UNHANDLED) {
                            H5T__bit_set(d, dst->shared->u.atomic.offset, dst->shared->u.atomic.prec - 1,
                                         true);
                            H5T__bit_set(d, (dst->shared->u.atomic.offset + dst->shared->u.atomic.prec - 1),
                                         (size_t)1, false);
                        }
                        else if (except_ret == H5T_CONV_ABORT)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL,
                                        "can't handle conversion exception");
                        else if (except_ret == H5T_CONV_HANDLED)
                            /*Don't reverse because user handles it already*/
                            reverse = false;
                    }
                    else if (src->shared->u.atomic.prec < dst->shared->u.atomic.prec) {
                        H5T__bit_copy(d, dst->shared->u.atomic.offset, s, src->shared->u.atomic.offset,
                                      src->shared->u.atomic.prec);
                        H5T__bit_set(d, dst->shared->u.atomic.offset + src->shared->u.atomic.prec,
                                     dst->shared->u.atomic.prec - src->shared->u.atomic.prec, false);
                    }
                    else {
                        H5T__bit_copy(d, dst->shared->u.atomic.offset, s, src->shared->u.atomic.offset,
                                      dst->shared->u.atomic.prec);
                    }
                }
                else if (first + 1 == src->shared->u.atomic.prec) {
                    /*
                     * Both the source and the destination are signed and the
                     * source value is negative.  We could experience overflow
                     * if the destination isn't wide enough in which case the
                     * destination is set to a negative number with the largest
                     * possible magnitude.
                     */
                    ssize_t sfz = H5T__bit_find(s, src->shared->u.atomic.offset,
                                                src->shared->u.atomic.prec - 1, H5T_BIT_MSB, false);
                    size_t  fz  = (size_t)sfz;

                    if (sfz >= 0 && fz + 1 >= dst->shared->u.atomic.prec) {
                        /*overflow*/
                        if (conv_ctx->u.conv.cb_struct
                                .func) { /*If user's exception handler is present, use it*/
                            H5T__reverse_order(src_rev, s, src->shared->size,
                                               src->shared->u.atomic.order); /*reverse order first*/
                            except_ret = (conv_ctx->u.conv.cb_struct.func)(
                                H5T_CONV_EXCEPT_RANGE_LOW, conv_ctx->u.conv.src_type_id,
                                conv_ctx->u.conv.dst_type_id, src_rev, d,
                                conv_ctx->u.conv.cb_struct.user_data);
                        }

                        if (except_ret == H5T_CONV_UNHANDLED) {
                            H5T__bit_set(d, dst->shared->u.atomic.offset, dst->shared->u.atomic.prec - 1,
                                         false);
                            H5T__bit_set(d, (dst->shared->u.atomic.offset + dst->shared->u.atomic.prec - 1),
                                         (size_t)1, true);
                        }
                        else if (except_ret == H5T_CONV_ABORT)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL,
                                        "can't handle conversion exception");
                        else if (except_ret == H5T_CONV_HANDLED)
                            /*Don't reverse because user handles it already*/
                            reverse = false;
                    }
                    else if (src->shared->u.atomic.prec < dst->shared->u.atomic.prec) {
                        H5T__bit_copy(d, dst->shared->u.atomic.offset, s, src->shared->u.atomic.offset,
                                      src->shared->u.atomic.prec);
                        H5T__bit_set(d, dst->shared->u.atomic.offset + src->shared->u.atomic.prec,
                                     dst->shared->u.atomic.prec - src->shared->u.atomic.prec, true);
                    }
                    else {
                        H5T__bit_copy(d, dst->shared->u.atomic.offset, s, src->shared->u.atomic.offset,
                                      dst->shared->u.atomic.prec);
                    }
                }
                else {
                    /*
                     * Source and destination are both signed but the source
                     * value is positive.  We could have an overflow in which
                     * case the destination is set to the largest possible
                     * positive value.
                     */
                    if (first + 1 >= dst->shared->u.atomic.prec) {
                        /*overflow*/
                        if (conv_ctx->u.conv.cb_struct
                                .func) { /*If user's exception handler is present, use it*/
                            H5T__reverse_order(src_rev, s, src->shared->size,
                                               src->shared->u.atomic.order); /*reverse order first*/
                            except_ret = (conv_ctx->u.conv.cb_struct.func)(
                                H5T_CONV_EXCEPT_RANGE_HI, conv_ctx->u.conv.src_type_id,
                                conv_ctx->u.conv.dst_type_id, src_rev, d,
                                conv_ctx->u.conv.cb_struct.user_data);
                        }

                        if (except_ret == H5T_CONV_UNHANDLED) {
                            H5T__bit_set(d, dst->shared->u.atomic.offset, dst->shared->u.atomic.prec - 1,
                                         true);
                            H5T__bit_set(d, (dst->shared->u.atomic.offset + dst->shared->u.atomic.prec - 1),
                                         (size_t)1, false);
                        }
                        else if (except_ret == H5T_CONV_ABORT)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL,
                                        "can't handle conversion exception");
                        else if (except_ret == H5T_CONV_HANDLED)
                            /*Don't reverse because user handles it already*/
                            reverse = false;
                    }
                    else if (src->shared->u.atomic.prec < dst->shared->u.atomic.prec) {
                        H5T__bit_copy(d, dst->shared->u.atomic.offset, s, src->shared->u.atomic.offset,
                                      src->shared->u.atomic.prec);
                        H5T__bit_set(d, dst->shared->u.atomic.offset + src->shared->u.atomic.prec,
                                     dst->shared->u.atomic.prec - src->shared->u.atomic.prec, false);
                    }
                    else {
                        H5T__bit_copy(d, dst->shared->u.atomic.offset, s, src->shared->u.atomic.offset,
                                      dst->shared->u.atomic.prec);
                    }
                }

                /*
                 * Set padding areas in destination.
                 */
                if (dst->shared->u.atomic.offset > 0) {
                    assert(H5T_PAD_ZERO == dst->shared->u.atomic.lsb_pad ||
                           H5T_PAD_ONE == dst->shared->u.atomic.lsb_pad);
                    H5T__bit_set(d, (size_t)0, dst->shared->u.atomic.offset,
                                 (bool)(H5T_PAD_ONE == dst->shared->u.atomic.lsb_pad));
                }
                if (dst->shared->u.atomic.offset + dst->shared->u.atomic.prec != 8 * dst->shared->size) {
                    assert(H5T_PAD_ZERO == dst->shared->u.atomic.msb_pad ||
                           H5T_PAD_ONE == dst->shared->u.atomic.msb_pad);
                    H5T__bit_set(d, dst->shared->u.atomic.offset + dst->shared->u.atomic.prec,
                                 8 * dst->shared->size -
                                     (dst->shared->u.atomic.offset + dst->shared->u.atomic.prec),
                                 (bool)(H5T_PAD_ONE == dst->shared->u.atomic.msb_pad));
                }

                /*
                 * Put the destination in the correct byte order.  See note at
                 * beginning of loop.
                 */
                if (H5T_ORDER_BE == dst->shared->u.atomic.order && reverse) {
                    half_size = dst->shared->size / 2;
                    for (i = 0; i < half_size; i++) {
                        uint8_t tmp                    = d[dst->shared->size - (i + 1)];
                        d[dst->shared->size - (i + 1)] = d[i];
                        d[i]                           = tmp;
                    }
                }

                /*
                 * If we had used a temporary buffer for the destination then we
                 * should copy the value to the true destination buffer.
                 */
                if (d == dbuf)
                    H5MM_memcpy(dp, d, dst->shared->size);

                /* Advance source & destination pointers by delta amounts */
                sp += src_delta;
                dp += dst_delta;
            } /* end for */

            break;

        default:
            HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unknown conversion command");
    } /* end switch */

done:
    if (src_rev)
        H5MM_free(src_rev);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5T__conv_i_i() */

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_f_f
 *
 * Purpose:    Convert one floating point type to another.  This is a catch
 *        all for floating point conversions and is probably not
 *        particularly fast!
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_f_f(const H5T_t *src_p, const H5T_t *dst_p, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
              size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
              void H5_ATTR_UNUSED *bkg)
{
    /* Traversal-related variables */
    H5T_atomic_t src;                  /*atomic source info        */
    H5T_atomic_t dst;                  /*atomic destination info    */
    ssize_t      src_delta, dst_delta; /*source & destination stride    */
    int          direction;            /*forward or backward traversal    */
    size_t       elmtno;               /*element number        */
    size_t       half_size;            /*half the type size        */
    size_t       tsize;                /*type size for swapping bytes  */
    size_t       olap;                 /*num overlapping elements    */
    ssize_t      bitno = 0;            /*bit number            */
    uint8_t     *s, *sp, *d, *dp;      /*source and dest traversal ptrs*/
    uint8_t     *src_rev  = NULL;      /*order-reversed source buffer  */
    uint8_t      dbuf[64] = {0};       /*temp destination buffer    */
    uint8_t      tmp1, tmp2;           /*temp variables for swapping bytes*/

    /* Conversion-related variables */
    int64_t        expo;                 /*exponent            */
    hssize_t       expo_max;             /*maximum possible dst exponent    */
    size_t         msize = 0;            /*useful size of mantissa in src*/
    size_t         mpos;                 /*offset to useful mant is src    */
    uint64_t       sign;                 /*source sign bit value         */
    size_t         mrsh;                 /*amount to right shift mantissa*/
    bool           carry = false;        /*carry after rounding mantissa    */
    size_t         i;                    /*miscellaneous counters    */
    size_t         implied;              /*destination implied bits    */
    bool           denormalized = false; /*is either source or destination denormalized?*/
    H5T_conv_ret_t except_ret;           /*return of callback function   */
    bool           reverse;              /*if reverse the order of destination        */
    herr_t         ret_value = SUCCEED;  /*return value                 */

    FUNC_ENTER_PACKAGE

    switch (cdata->command) {
        case H5T_CONV_INIT:
            if (NULL == src_p || NULL == dst_p)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype");
            src = src_p->shared->u.atomic;
            dst = dst_p->shared->u.atomic;
            if (H5T_ORDER_LE != src.order && H5T_ORDER_BE != src.order && H5T_ORDER_VAX != src.order)
                HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unsupported byte order");
            if (H5T_ORDER_LE != dst.order && H5T_ORDER_BE != dst.order && H5T_ORDER_VAX != dst.order)
                HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unsupported byte order");
            if (dst_p->shared->size > sizeof(dbuf))
                HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "destination size is too large");
            if (8 * sizeof(expo) - 1 < src.u.f.esize || 8 * sizeof(expo) - 1 < dst.u.f.esize)
                HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "exponent field is too large");
            cdata->need_bkg = H5T_BKG_NO;
            break;

        case H5T_CONV_FREE:
            break;

        case H5T_CONV_CONV:
            if (NULL == src_p || NULL == dst_p)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype");
            if (NULL == conv_ctx)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid datatype conversion context pointer");

            src      = src_p->shared->u.atomic;
            dst      = dst_p->shared->u.atomic;
            expo_max = ((hssize_t)1 << dst.u.f.esize) - 1;

            /*
             * Do we process the values from beginning to end or vice versa? Also,
             * how many of the elements have the source and destination areas
             * overlapping?
             */
            if (src_p->shared->size == dst_p->shared->size || buf_stride) {
                sp = dp   = (uint8_t *)buf;
                direction = 1;
                olap      = nelmts;
            }
            else if (src_p->shared->size >= dst_p->shared->size) {
                double olap_d =
                    ceil((double)(dst_p->shared->size) / (double)(src_p->shared->size - dst_p->shared->size));
                olap = (size_t)olap_d;
                sp = dp   = (uint8_t *)buf;
                direction = 1;
            }
            else {
                double olap_d =
                    ceil((double)(src_p->shared->size) / (double)(dst_p->shared->size - src_p->shared->size));
                olap      = (size_t)olap_d;
                sp        = (uint8_t *)buf + (nelmts - 1) * src_p->shared->size;
                dp        = (uint8_t *)buf + (nelmts - 1) * dst_p->shared->size;
                direction = -1;
            }

            /*
             * Direction & size of buffer traversal.
             */
            H5_CHECK_OVERFLOW(buf_stride, size_t, ssize_t);
            H5_CHECK_OVERFLOW(src_p->shared->size, size_t, ssize_t);
            H5_CHECK_OVERFLOW(dst_p->shared->size, size_t, ssize_t);
            src_delta = (ssize_t)direction * (ssize_t)(buf_stride ? buf_stride : src_p->shared->size);
            dst_delta = (ssize_t)direction * (ssize_t)(buf_stride ? buf_stride : dst_p->shared->size);

            /* Allocate space for order-reversed source buffer */
            src_rev = (uint8_t *)H5MM_calloc(src_p->shared->size);

            /* The conversion loop */
            for (elmtno = 0; elmtno < nelmts; elmtno++) {
                /* Set these variables to default */
                except_ret = H5T_CONV_UNHANDLED;
                reverse    = true;

                /*
                 * If the source and destination buffers overlap then use a
                 * temporary buffer for the destination.
                 */
                if (direction > 0) {
                    s = sp;
                    d = elmtno < olap ? dbuf : dp;
                }
                else {
                    s = sp;
                    d = elmtno + olap >= nelmts ? dbuf : dp;
                }
#ifndef NDEBUG
                /* I don't quite trust the overlap calculations yet --rpm */
                if (d == dbuf) {
                    assert((dp >= sp && dp < sp + src_p->shared->size) ||
                           (sp >= dp && sp < dp + dst_p->shared->size));
                }
                else {
                    assert((dp < sp && dp + dst_p->shared->size <= sp) ||
                           (sp < dp && sp + src_p->shared->size <= dp));
                }
#endif

                /*
                 * Put the data in little endian order so our loops aren't so
                 * complicated.  We'll do all the conversion stuff assuming
                 * little endian and then we'll fix the order at the end.
                 */
                if (H5T_ORDER_BE == src.order) {
                    half_size = src_p->shared->size / 2;
                    for (i = 0; i < half_size; i++) {
                        tmp1                             = s[src_p->shared->size - (i + 1)];
                        s[src_p->shared->size - (i + 1)] = s[i];
                        s[i]                             = tmp1;
                    }
                }
                else if (H5T_ORDER_VAX == src.order) {
                    tsize = src_p->shared->size;
                    assert(0 == tsize % 2);

                    for (i = 0; i < tsize; i += 4) {
                        tmp1 = s[i];
                        tmp2 = s[i + 1];

                        s[i]     = s[(tsize - 2) - i];
                        s[i + 1] = s[(tsize - 1) - i];

                        s[(tsize - 2) - i] = tmp1;
                        s[(tsize - 1) - i] = tmp2;
                    }
                }

                /*
                 * Find the sign bit value of the source.
                 */
                sign = H5T__bit_get_d(s, src.u.f.sign, (size_t)1);

                /*
                 * Check for special cases: +0, -0, +Inf, -Inf, NaN
                 */
                if (H5T__bit_find(s, src.u.f.mpos, src.u.f.msize, H5T_BIT_LSB, true) < 0) {
                    if (H5T__bit_find(s, src.u.f.epos, src.u.f.esize, H5T_BIT_LSB, true) < 0) {
                        /* +0 or -0 */
                        H5T__bit_copy(d, dst.u.f.sign, s, src.u.f.sign, (size_t)1);
                        H5T__bit_set(d, dst.u.f.epos, dst.u.f.esize, false);
                        H5T__bit_set(d, dst.u.f.mpos, dst.u.f.msize, false);
                        goto padding;
                    }
                    else if (H5T__bit_find(s, src.u.f.epos, src.u.f.esize, H5T_BIT_LSB, false) < 0) {
                        /* +Inf or -Inf */
                        if (conv_ctx->u.conv.cb_struct
                                .func) { /*If user's exception handler is present, use it*/
                            /*reverse order first*/
                            H5T__reverse_order(src_rev, s, src_p->shared->size,
                                               src_p->shared->u.atomic.order);
                            if (sign)
                                except_ret = (conv_ctx->u.conv.cb_struct.func)(
                                    H5T_CONV_EXCEPT_NINF, conv_ctx->u.conv.src_type_id,
                                    conv_ctx->u.conv.dst_type_id, src_rev, d,
                                    conv_ctx->u.conv.cb_struct.user_data);
                            else
                                except_ret = (conv_ctx->u.conv.cb_struct.func)(
                                    H5T_CONV_EXCEPT_PINF, conv_ctx->u.conv.src_type_id,
                                    conv_ctx->u.conv.dst_type_id, src_rev, d,
                                    conv_ctx->u.conv.cb_struct.user_data);
                        }

                        if (except_ret == H5T_CONV_UNHANDLED) {
                            H5T__bit_copy(d, dst.u.f.sign, s, src.u.f.sign, (size_t)1);
                            H5T__bit_set(d, dst.u.f.epos, dst.u.f.esize, true);
                            H5T__bit_set(d, dst.u.f.mpos, dst.u.f.msize, false);
                            /*If the destination no implied mantissa bit, we'll need to set
                             *the 1st bit of mantissa to 1.  The Intel-Linux long double is
                             *this case.*/
                            if (H5T_NORM_NONE == dst.u.f.norm)
                                H5T__bit_set(d, dst.u.f.mpos + dst.u.f.msize - 1, (size_t)1, true);
                        }
                        else if (except_ret == H5T_CONV_HANDLED) {
                            /*No need to reverse the order of destination because user handles it*/
                            reverse = false;
                            goto next;
                        }
                        else if (except_ret == H5T_CONV_ABORT)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL,
                                        "can't handle conversion exception");

                        goto padding;
                    }
                }
                else if (H5T_NORM_NONE == src.u.f.norm &&
                         H5T__bit_find(s, src.u.f.mpos, src.u.f.msize - 1, H5T_BIT_LSB, true) < 0 &&
                         H5T__bit_find(s, src.u.f.epos, src.u.f.esize, H5T_BIT_LSB, false) < 0) {
                    /*This is a special case for the source of no implied mantissa bit.
                     *If the exponent bits are all 1s and only the 1st bit of mantissa
                     *is set to 1.  It's infinity. The Intel-Linux "long double" is this case.*/
                    /* +Inf or -Inf */
                    if (conv_ctx->u.conv.cb_struct.func) { /*If user's exception handler is present, use it*/
                        /*reverse order first*/
                        H5T__reverse_order(src_rev, s, src_p->shared->size, src_p->shared->u.atomic.order);
                        if (sign)
                            except_ret = (conv_ctx->u.conv.cb_struct.func)(
                                H5T_CONV_EXCEPT_NINF, conv_ctx->u.conv.src_type_id,
                                conv_ctx->u.conv.dst_type_id, src_rev, d,
                                conv_ctx->u.conv.cb_struct.user_data);
                        else
                            except_ret = (conv_ctx->u.conv.cb_struct.func)(
                                H5T_CONV_EXCEPT_PINF, conv_ctx->u.conv.src_type_id,
                                conv_ctx->u.conv.dst_type_id, src_rev, d,
                                conv_ctx->u.conv.cb_struct.user_data);
                    }

                    if (except_ret == H5T_CONV_UNHANDLED) {
                        H5T__bit_copy(d, dst.u.f.sign, s, src.u.f.sign, (size_t)1);
                        H5T__bit_set(d, dst.u.f.epos, dst.u.f.esize, true);
                        H5T__bit_set(d, dst.u.f.mpos, dst.u.f.msize, false);
                        /*If the destination no implied mantissa bit, we'll need to set
                         *the 1st bit of mantissa to 1.  The Intel-Linux long double is
                         *this case.*/
                        if (H5T_NORM_NONE == dst.u.f.norm)
                            H5T__bit_set(d, dst.u.f.mpos + dst.u.f.msize - 1, (size_t)1, true);
                    }
                    else if (except_ret == H5T_CONV_HANDLED) {
                        /*No need to reverse the order of destination because user handles it*/
                        reverse = false;
                        goto next;
                    }
                    else if (except_ret == H5T_CONV_ABORT)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception");

                    goto padding;
                    /* Temporary solution to handle VAX special values.
                     * Note that even though we don't support VAX anymore, we
                     * still need to handle legacy VAX files so this code must
                     * remain in place.
                     */
                }
                else if (H5T__bit_find(s, src.u.f.epos, src.u.f.esize, H5T_BIT_LSB, false) < 0) {
                    /* NaN */
                    if (conv_ctx->u.conv.cb_struct.func) { /*If user's exception handler is present, use it*/
                        /*reverse order first*/
                        H5T__reverse_order(src_rev, s, src_p->shared->size, src_p->shared->u.atomic.order);
                        except_ret = (conv_ctx->u.conv.cb_struct.func)(
                            H5T_CONV_EXCEPT_NAN, conv_ctx->u.conv.src_type_id, conv_ctx->u.conv.dst_type_id,
                            src_rev, d, conv_ctx->u.conv.cb_struct.user_data);
                    }

                    if (except_ret == H5T_CONV_UNHANDLED) {
                        /* There are many NaN values, so we just set all bits of
                         * the significand. */
                        H5T__bit_copy(d, dst.u.f.sign, s, src.u.f.sign, (size_t)1);
                        H5T__bit_set(d, dst.u.f.epos, dst.u.f.esize, true);
                        H5T__bit_set(d, dst.u.f.mpos, dst.u.f.msize, true);
                    }
                    else if (except_ret == H5T_CONV_HANDLED) {
                        /*No need to reverse the order of destination because user handles it*/
                        reverse = false;
                        goto next;
                    }
                    else if (except_ret == H5T_CONV_ABORT)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception");

                    goto padding;
                }

                /*
                 * Get the exponent as an unsigned quantity from the section of
                 * the source bit field where it's located.     Don't worry about
                 * the exponent bias yet.
                 */
                expo = (int64_t)H5T__bit_get_d(s, src.u.f.epos, src.u.f.esize);

                if (expo == 0)
                    denormalized = true;

                /*
                 * Set markers for the source mantissa, excluding the leading `1'
                 * (might be implied).
                 */
                implied = 1;
                mpos    = src.u.f.mpos;
                mrsh    = 0;
                if (0 == expo || H5T_NORM_NONE == src.u.f.norm) {
                    if ((bitno = H5T__bit_find(s, src.u.f.mpos, src.u.f.msize, H5T_BIT_MSB, true)) > 0) {
                        msize = (size_t)bitno;
                    }
                    else if (0 == bitno) {
                        msize = 1;
                        H5T__bit_set(s, src.u.f.mpos, (size_t)1, false);
                    }
                }
                else if (H5T_NORM_IMPLIED == src.u.f.norm) {
                    msize = src.u.f.msize;
                }
                else {
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL,
                                "normalization method not implemented yet");
                }

                /*
                 * The sign for the destination is the same as the sign for the
                 * source in all cases.
                 */
                H5T__bit_copy(d, dst.u.f.sign, s, src.u.f.sign, (size_t)1);

                /*
                 * Calculate the true source exponent by adjusting according to
                 * the source exponent bias.
                 */
                if (0 == expo || H5T_NORM_NONE == src.u.f.norm) {
                    assert(bitno >= 0);
                    expo -= (int64_t)((src.u.f.ebias - 1) + (src.u.f.msize - (size_t)bitno));
                }
                else if (H5T_NORM_IMPLIED == src.u.f.norm) {
                    expo -= (int64_t)src.u.f.ebias;
                }
                else {
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL,
                                "normalization method not implemented yet");
                }

                /*
                 * If the destination is not normalized then right shift the
                 * mantissa by one.
                 */
                if (H5T_NORM_NONE == dst.u.f.norm)
                    mrsh++;

                /*
                 * Calculate the destination exponent by adding the destination
                 * bias and clipping by the minimum and maximum possible
                 * destination exponent values.
                 */
                expo += (int64_t)dst.u.f.ebias;

                if (expo < -(hssize_t)(dst.u.f.msize)) {
                    /* The exponent is way too small.  Result is zero. */
                    expo = 0;
                    H5T__bit_set(d, dst.u.f.mpos, dst.u.f.msize, false);
                    msize = 0;
                }
                else if (expo <= 0) {
                    /*
                     * The exponent is too small to fit in the exponent field,
                     * but by shifting the mantissa to the right we can
                     * accommodate that value.  The mantissa of course is no
                     * longer normalized.
                     */
                    mrsh += (size_t)(1 - expo);
                    expo         = 0;
                    denormalized = true;
                }
                else if (expo >= expo_max) {
                    /*
                     * The exponent is too large to fit in the available region
                     * or it results in the maximum possible value.     Use positive
                     * or negative infinity instead unless the application
                     * specifies something else.  Before calling the overflow
                     * handler make sure the source buffer we hand it is in the
                     * original byte order.
                     */
                    if (conv_ctx->u.conv.cb_struct.func) { /*If user's exception handler is present, use it*/
                        /*reverse order first*/
                        H5T__reverse_order(src_rev, s, src_p->shared->size, src_p->shared->u.atomic.order);
                        except_ret = (conv_ctx->u.conv.cb_struct.func)(
                            H5T_CONV_EXCEPT_RANGE_HI, conv_ctx->u.conv.src_type_id,
                            conv_ctx->u.conv.dst_type_id, src_rev, d, conv_ctx->u.conv.cb_struct.user_data);
                    }

                    if (except_ret == H5T_CONV_UNHANDLED) {
                        expo = expo_max;
                        H5T__bit_set(d, dst.u.f.mpos, dst.u.f.msize, false);
                        msize = 0;
                    }
                    else if (except_ret == H5T_CONV_ABORT)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception");
                    else if (except_ret == H5T_CONV_HANDLED) {
                        reverse = false;
                        goto next;
                    }
                }

                /*
                 * If the destination mantissa is smaller than the source
                 * mantissa then round the source mantissa. Rounding may cause a
                 * carry in which case the exponent has to be re-evaluated for
                 * overflow.  That is, if `carry' is clear then the implied
                 * mantissa bit is `1', else it is `10' binary.
                 */
                if (msize > 0 && mrsh <= dst.u.f.msize && mrsh + msize > dst.u.f.msize) {
                    bitno = (ssize_t)(mrsh + msize - dst.u.f.msize);
                    assert(bitno >= 0 && (size_t)bitno <= msize);
                    /* If the 1st bit being cut off is set and source isn't denormalized.*/
                    if (H5T__bit_get_d(s, (mpos + (size_t)bitno) - 1, (size_t)1) && !denormalized) {
                        /* Don't do rounding if exponent is 111...110 and mantissa is 111...11.
                         * To do rounding and increment exponent in this case will create an infinity value.*/
                        if ((H5T__bit_find(s, mpos + (size_t)bitno, msize - (size_t)bitno, H5T_BIT_LSB,
                                           false) >= 0 ||
                             expo < expo_max - 1)) {
                            carry = H5T__bit_inc(s, mpos + (size_t)bitno - 1, 1 + msize - (size_t)bitno);
                            if (carry)
                                implied = 2;
                        }
                    }
                    else if (H5T__bit_get_d(s, (mpos + (size_t)bitno) - 1, (size_t)1) && denormalized)
                        /* For either source or destination, denormalized value doesn't increment carry.*/
                        H5T__bit_inc(s, mpos + (size_t)bitno - 1, 1 + msize - (size_t)bitno);
                }
                else
                    carry = false;

                /*
                 * Write the mantissa to the destination
                 */
                if (mrsh > dst.u.f.msize + 1) {
                    H5T__bit_set(d, dst.u.f.mpos, dst.u.f.msize, false);
                }
                else if (mrsh == dst.u.f.msize + 1) {
                    H5T__bit_set(d, dst.u.f.mpos + 1, dst.u.f.msize - 1, false);
                    H5T__bit_set(d, dst.u.f.mpos, (size_t)1, true);
                }
                else if (mrsh == dst.u.f.msize) {
                    H5T__bit_set(d, dst.u.f.mpos, dst.u.f.msize, false);
                    H5T__bit_set_d(d, dst.u.f.mpos, MIN(2, dst.u.f.msize), (hsize_t)implied);
                }
                else {
                    if (mrsh > 0) {
                        H5T__bit_set(d, dst.u.f.mpos + dst.u.f.msize - mrsh, mrsh, false);
                        H5T__bit_set_d(d, dst.u.f.mpos + dst.u.f.msize - mrsh, (size_t)2, (hsize_t)implied);
                    }
                    if (mrsh + msize >= dst.u.f.msize) {
                        H5T__bit_copy(d, dst.u.f.mpos, s, (mpos + msize + mrsh - dst.u.f.msize),
                                      dst.u.f.msize - mrsh);
                    }
                    else {
                        H5T__bit_copy(d, dst.u.f.mpos + dst.u.f.msize - (mrsh + msize), s, mpos, msize);
                        H5T__bit_set(d, dst.u.f.mpos, dst.u.f.msize - (mrsh + msize), false);
                    }
                }

                /* Write the exponent */
                if (carry) {
                    expo++;
                    if (expo >= expo_max) {
                        /*
                         * The exponent is too large to fit in the available
                         * region or it results in the maximum possible value.
                         * Use positive or negative infinity instead unless the
                         * application specifies something else.  Before
                         * calling the overflow handler make sure the source
                         * buffer we hand it is in the original byte order.
                         */
                        if (conv_ctx->u.conv.cb_struct
                                .func) { /*If user's exception handler is present, use it*/
                            /*reverse order first*/
                            H5T__reverse_order(src_rev, s, src_p->shared->size,
                                               src_p->shared->u.atomic.order);
                            except_ret = (conv_ctx->u.conv.cb_struct.func)(
                                H5T_CONV_EXCEPT_RANGE_HI, conv_ctx->u.conv.src_type_id,
                                conv_ctx->u.conv.dst_type_id, src_rev, d,
                                conv_ctx->u.conv.cb_struct.user_data);
                        }

                        if (except_ret == H5T_CONV_UNHANDLED) {
                            expo = expo_max;
                            H5T__bit_set(d, dst.u.f.mpos, dst.u.f.msize, false);
                        }
                        else if (except_ret == H5T_CONV_ABORT)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL,
                                        "can't handle conversion exception");
                        else if (except_ret == H5T_CONV_HANDLED) {
                            reverse = false;
                            goto next;
                        }
                    }
                }
                /*reset CARRY*/
                carry = false;

                H5_CHECK_OVERFLOW(expo, hssize_t, hsize_t);
                H5T__bit_set_d(d, dst.u.f.epos, dst.u.f.esize, (hsize_t)expo);

padding:

                /*
                 * Set external padding areas
                 */
                if (dst.offset > 0) {
                    assert(H5T_PAD_ZERO == dst.lsb_pad || H5T_PAD_ONE == dst.lsb_pad);
                    H5T__bit_set(d, (size_t)0, dst.offset, (bool)(H5T_PAD_ONE == dst.lsb_pad));
                }
                if (dst.offset + dst.prec != 8 * dst_p->shared->size) {
                    assert(H5T_PAD_ZERO == dst.msb_pad || H5T_PAD_ONE == dst.msb_pad);
                    H5T__bit_set(d, dst.offset + dst.prec, 8 * dst_p->shared->size - (dst.offset + dst.prec),
                                 (bool)(H5T_PAD_ONE == dst.msb_pad));
                }

                /*
                 * Put the destination in the correct byte order.  See note at
                 * beginning of loop.
                 */
                if (H5T_ORDER_BE == dst.order && reverse) {
                    half_size = dst_p->shared->size / 2;
                    for (i = 0; i < half_size; i++) {
                        uint8_t tmp                      = d[dst_p->shared->size - (i + 1)];
                        d[dst_p->shared->size - (i + 1)] = d[i];
                        d[i]                             = tmp;
                    }
                }
                else if (H5T_ORDER_VAX == dst.order && reverse) {
                    tsize = dst_p->shared->size;
                    assert(0 == tsize % 2);

                    for (i = 0; i < tsize; i += 4) {
                        tmp1 = d[i];
                        tmp2 = d[i + 1];

                        d[i]     = d[(tsize - 2) - i];
                        d[i + 1] = d[(tsize - 1) - i];

                        d[(tsize - 2) - i] = tmp1;
                        d[(tsize - 1) - i] = tmp2;
                    }
                }

                /*
                 * If we had used a temporary buffer for the destination then we
                 * should copy the value to the true destination buffer.
                 */
next:
                if (d == dbuf)
                    H5MM_memcpy(dp, d, dst_p->shared->size);

                /* Advance source & destination pointers by delta amounts */
                sp += src_delta;
                dp += dst_delta;
            }

            break;

        default:
            HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unknown conversion command");
    } /* end switch */

done:
    if (src_rev)
        H5MM_free(src_rev);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5T__conv_f_f() */

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_s_s
 *
 * Purpose:    Convert one fixed-length string type to another.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_s_s(const H5T_t *src, const H5T_t *dst, H5T_cdata_t *cdata,
              const H5T_conv_ctx_t H5_ATTR_UNUSED *conv_ctx, size_t nelmts, size_t buf_stride,
              size_t H5_ATTR_UNUSED bkg_stride, void *buf, void H5_ATTR_UNUSED *bkg)
{
    ssize_t  src_delta, dst_delta; /*source & destination stride    */
    int      direction;            /*direction of traversal    */
    size_t   elmtno;               /*element number        */
    size_t   olap;                 /*num overlapping elements    */
    size_t   nchars = 0;           /*number of characters copied    */
    uint8_t *s, *sp, *d, *dp;      /*src and dst traversal pointers*/
    uint8_t *dbuf      = NULL;     /*temp buf for overlap converts.    */
    herr_t   ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_PACKAGE

    switch (cdata->command) {
        case H5T_CONV_INIT:
            if (NULL == src || NULL == dst)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype");
            if (8 * src->shared->size != src->shared->u.atomic.prec ||
                8 * dst->shared->size != dst->shared->u.atomic.prec)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bad precision");
            if (0 != src->shared->u.atomic.offset || 0 != dst->shared->u.atomic.offset)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bad offset");
            if (H5T_CSET_ASCII != src->shared->u.atomic.u.s.cset &&
                H5T_CSET_UTF8 != src->shared->u.atomic.u.s.cset)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bad source character set");
            if (H5T_CSET_ASCII != dst->shared->u.atomic.u.s.cset &&
                H5T_CSET_UTF8 != dst->shared->u.atomic.u.s.cset)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bad destination character set");
            if ((H5T_CSET_ASCII == src->shared->u.atomic.u.s.cset &&
                 H5T_CSET_UTF8 == dst->shared->u.atomic.u.s.cset) ||
                (H5T_CSET_ASCII == dst->shared->u.atomic.u.s.cset &&
                 H5T_CSET_UTF8 == src->shared->u.atomic.u.s.cset))
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                            "The library doesn't convert between strings of ASCII and UTF");
            if (src->shared->u.atomic.u.s.pad < 0 || src->shared->u.atomic.u.s.pad >= H5T_NSTR ||
                dst->shared->u.atomic.u.s.pad < 0 || dst->shared->u.atomic.u.s.pad >= H5T_NSTR)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bad character padding");
            cdata->need_bkg = H5T_BKG_NO;
            break;

        case H5T_CONV_FREE:
            break;

        case H5T_CONV_CONV:
            /* Get the datatypes */
            if (NULL == src || NULL == dst)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype");

            /*
             * Do we process the values from beginning to end or vice versa? Also,
             * how many of the elements have the source and destination areas
             * overlapping?
             */
            if (src->shared->size == dst->shared->size || buf_stride) {
                /*
                 * When the source and destination are the same size we can do
                 * all the conversions in place.
                 */
                sp = dp   = (uint8_t *)buf;
                direction = 1;
                olap      = 0;
            }
            else if (src->shared->size >= dst->shared->size) {
                double olapd =
                    ceil((double)(dst->shared->size) / (double)(src->shared->size - dst->shared->size));
                olap = (size_t)olapd;
                sp = dp   = (uint8_t *)buf;
                direction = 1;
            }
            else {
                double olapd =
                    ceil((double)(src->shared->size) / (double)(dst->shared->size - src->shared->size));
                olap      = (size_t)olapd;
                sp        = (uint8_t *)buf + (nelmts - 1) * src->shared->size;
                dp        = (uint8_t *)buf + (nelmts - 1) * dst->shared->size;
                direction = -1;
            }

            /*
             * Direction & size of buffer traversal.
             */
            H5_CHECK_OVERFLOW(buf_stride, size_t, ssize_t);
            H5_CHECK_OVERFLOW(src->shared->size, size_t, ssize_t);
            H5_CHECK_OVERFLOW(dst->shared->size, size_t, ssize_t);
            src_delta = (ssize_t)direction * (ssize_t)(buf_stride ? buf_stride : src->shared->size);
            dst_delta = (ssize_t)direction * (ssize_t)(buf_stride ? buf_stride : dst->shared->size);

            /* Allocate the overlap buffer */
            if (NULL == (dbuf = (uint8_t *)H5MM_calloc(dst->shared->size)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                            "memory allocation failed for string conversion");

            /* The conversion loop. */
            for (elmtno = 0; elmtno < nelmts; elmtno++) {

                /*
                 * If the source and destination buffers overlap then use a
                 * temporary buffer for the destination.
                 */
                if (direction > 0) {
                    s = sp;
                    d = elmtno < olap ? dbuf : dp;
                }
                else {
                    s = sp;
                    d = elmtno + olap >= nelmts ? dbuf : dp;
                }
#ifndef NDEBUG
                /* I don't quite trust the overlap calculations yet --rpm */
                if (src->shared->size == dst->shared->size || buf_stride) {
                    assert(s == d);
                }
                else if (d == dbuf) {
                    assert((dp >= sp && dp < sp + src->shared->size) ||
                           (sp >= dp && sp < dp + dst->shared->size));
                }
                else {
                    assert((dp < sp && dp + dst->shared->size <= sp) ||
                           (sp < dp && sp + src->shared->size <= dp));
                }
#endif

                /* Copy characters from source to destination */
                switch (src->shared->u.atomic.u.s.pad) {
                    case H5T_STR_NULLTERM:
                        for (nchars = 0;
                             nchars < dst->shared->size && nchars < src->shared->size && s[nchars];
                             nchars++) {
                            d[nchars] = s[nchars];
                        }
                        break;

                    case H5T_STR_NULLPAD:
                        for (nchars = 0;
                             nchars < dst->shared->size && nchars < src->shared->size && s[nchars];
                             nchars++) {
                            d[nchars] = s[nchars];
                        }
                        break;

                    case H5T_STR_SPACEPAD:
                        nchars = src->shared->size;
                        while (nchars > 0 && ' ' == s[nchars - 1])
                            --nchars;
                        nchars = MIN(dst->shared->size, nchars);
                        if (d != s)
                            H5MM_memcpy(d, s, nchars);
                        break;

                    case H5T_STR_RESERVED_3:
                    case H5T_STR_RESERVED_4:
                    case H5T_STR_RESERVED_5:
                    case H5T_STR_RESERVED_6:
                    case H5T_STR_RESERVED_7:
                    case H5T_STR_RESERVED_8:
                    case H5T_STR_RESERVED_9:
                    case H5T_STR_RESERVED_10:
                    case H5T_STR_RESERVED_11:
                    case H5T_STR_RESERVED_12:
                    case H5T_STR_RESERVED_13:
                    case H5T_STR_RESERVED_14:
                    case H5T_STR_RESERVED_15:
                    case H5T_STR_ERROR:
                    default:
                        HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
                                    "source string padding method not supported");
                } /* end switch */

                /* Terminate or pad the destination */
                switch (dst->shared->u.atomic.u.s.pad) {
                    case H5T_STR_NULLTERM:
                        while (nchars < dst->shared->size)
                            d[nchars++] = '\0';
                        d[dst->shared->size - 1] = '\0';
                        break;

                    case H5T_STR_NULLPAD:
                        while (nchars < dst->shared->size)
                            d[nchars++] = '\0';
                        break;

                    case H5T_STR_SPACEPAD:
                        while (nchars < dst->shared->size)
                            d[nchars++] = ' ';
                        break;

                    case H5T_STR_RESERVED_3:
                    case H5T_STR_RESERVED_4:
                    case H5T_STR_RESERVED_5:
                    case H5T_STR_RESERVED_6:
                    case H5T_STR_RESERVED_7:
                    case H5T_STR_RESERVED_8:
                    case H5T_STR_RESERVED_9:
                    case H5T_STR_RESERVED_10:
                    case H5T_STR_RESERVED_11:
                    case H5T_STR_RESERVED_12:
                    case H5T_STR_RESERVED_13:
                    case H5T_STR_RESERVED_14:
                    case H5T_STR_RESERVED_15:
                    case H5T_STR_ERROR:
                    default:
                        HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
                                    "destination string padding method not supported");
                } /* end switch */

                /*
                 * If we used a temporary buffer for the destination then we
                 * should copy the value to the true destination buffer.
                 */
                if (d == dbuf)
                    H5MM_memcpy(dp, d, dst->shared->size);

                /* Advance source & destination pointers by delta amounts */
                sp += src_delta;
                dp += dst_delta;
            } /* end for */
            break;

        default:
            HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unknown conversion command");
    } /* end switch */

done:
    H5MM_xfree(dbuf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5T__conv_s_s() */

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_schar_uchar
 *
 * Purpose:    Converts `signed char' to `unsigned char'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_schar_uchar(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_su(SCHAR, UCHAR, signed char, unsigned char, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_uchar_schar
 *
 * Purpose:    Converts `unsigned char' to `signed char'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_uchar_schar(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_us(UCHAR, SCHAR, unsigned char, signed char, -, SCHAR_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_schar_short
 *
 * Purpose:    Converts `signed char' to `short'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_schar_short(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_sS(SCHAR, SHORT, signed char, short, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_schar_ushort
 *
 * Purpose:    Converts `signed char' to `unsigned short'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_schar_ushort(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_sU(SCHAR, USHORT, signed char, unsigned short, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_uchar_short
 *
 * Purpose:    Converts `unsigned char' to `short'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_uchar_short(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_uS(UCHAR, SHORT, unsigned char, short, -, SHRT_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_uchar_ushort
 *
 * Purpose:    Converts `unsigned char' to `unsigned short'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_uchar_ushort(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_uU(UCHAR, USHORT, unsigned char, unsigned short, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_schar_int
 *
 * Purpose:    Converts `signed char' to `int'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_schar_int(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                    size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                    void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_sS(SCHAR, INT, signed char, int, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_schar_uint
 *
 * Purpose:    Converts `signed char' to `unsigned int'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_schar_uint(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                     size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                     void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_sU(SCHAR, UINT, signed char, unsigned, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_uchar_int
 *
 * Purpose:    Converts `unsigned char' to `int'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_uchar_int(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                    size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                    void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_uS(UCHAR, INT, unsigned char, int, -, INT_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_uchar_uint
 *
 * Purpose:    Converts `unsigned char' to `unsigned int'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_uchar_uint(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                     size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                     void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_uU(UCHAR, UINT, unsigned char, unsigned, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_schar_long
 *
 * Purpose:    Converts `signed char' to `long'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_schar_long(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                     size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                     void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_sS(SCHAR, LONG, signed char, long, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_schar_ulong
 *
 * Purpose:    Converts `signed char' to `unsigned long'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_schar_ulong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_sU(SCHAR, ULONG, signed char, unsigned long, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_uchar_long
 *
 * Purpose:    Converts `unsigned char' to `long'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_uchar_long(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                     size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                     void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_uS(UCHAR, LONG, unsigned char, long, -, LONG_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_uchar_ulong
 *
 * Purpose:    Converts `unsigned char' to `unsigned long'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_uchar_ulong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_uU(UCHAR, ULONG, unsigned char, unsigned long, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_schar_llong
 *
 * Purpose:    Converts `signed char' to `long long'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_schar_llong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_sS(SCHAR, LLONG, signed char, long long, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_schar_ullong
 *
 * Purpose:    Converts `signed char' to `unsigned long long'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_schar_ullong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_sU(SCHAR, ULLONG, signed char, unsigned long long, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_uchar_llong
 *
 * Purpose:    Converts `unsigned char' to `long long'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_uchar_llong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_uS(UCHAR, LLONG, unsigned char, long long, -, LLONG_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_uchar_ullong
 *
 * Purpose:    Converts `unsigned char' to `unsigned long long'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_uchar_ullong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_uU(UCHAR, ULLONG, unsigned char, unsigned long long, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_short_schar
 *
 * Purpose:    Converts `short' to `signed char'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_short_schar(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Ss(SHORT, SCHAR, short, signed char, SCHAR_MIN, SCHAR_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_short_uchar
 *
 * Purpose:    Converts `short' to `unsigned char'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_short_uchar(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Su(SHORT, UCHAR, short, unsigned char, -, UCHAR_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ushort_schar
 *
 * Purpose:    Converts `unsigned short' to `signed char'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ushort_schar(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Us(USHORT, SCHAR, unsigned short, signed char, -, SCHAR_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ushort_uchar
 *
 * Purpose:    Converts `unsigned short' to `unsigned char'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ushort_uchar(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Uu(USHORT, UCHAR, unsigned short, unsigned char, -, UCHAR_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_short_ushort
 *
 * Purpose:    Converts `short' to `unsigned short'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_short_ushort(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_su(SHORT, USHORT, short, unsigned short, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ushort_short
 *
 * Purpose:    Converts `unsigned short' to `short'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ushort_short(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_us(USHORT, SHORT, unsigned short, short, -, SHRT_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_short_int
 *
 * Purpose:    Converts `short' to `int'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_short_int(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                    size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                    void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_sS(SHORT, INT, short, int, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_short_uint
 *
 * Purpose:    Converts `short' to `unsigned int'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_short_uint(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                     size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                     void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_sU(SHORT, UINT, short, unsigned, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ushort_int
 *
 * Purpose:    Converts `unsigned short' to `int'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ushort_int(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                     size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                     void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_uS(USHORT, INT, unsigned short, int, -, INT_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ushort_uint
 *
 * Purpose:    Converts `unsigned short' to `unsigned int'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ushort_uint(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_uU(USHORT, UINT, unsigned short, unsigned, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_short_long
 *
 * Purpose:    Converts `short' to `long'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_short_long(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                     size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                     void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_sS(SHORT, LONG, short, long, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_short_ulong
 *
 * Purpose:    Converts `short' to `unsigned long'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_short_ulong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_sU(SHORT, ULONG, short, unsigned long, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ushort_long
 *
 * Purpose:    Converts `unsigned short' to `long'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ushort_long(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_uS(USHORT, LONG, unsigned short, long, -, LONG_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ushort_ulong
 *
 * Purpose:    Converts `unsigned short' to `unsigned long'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ushort_ulong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_uU(USHORT, ULONG, unsigned short, unsigned long, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_short_llong
 *
 * Purpose:    Converts `short' to `long long'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_short_llong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_sS(SHORT, LLONG, short, long long, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_short_ullong
 *
 * Purpose:    Converts `short' to `unsigned long long'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_short_ullong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_sU(SHORT, ULLONG, short, unsigned long long, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ushort_llong
 *
 * Purpose:    Converts `unsigned short' to `long long'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ushort_llong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_uS(USHORT, LLONG, unsigned short, long long, -, LLONG_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ushort_ullong
 *
 * Purpose:    Converts `unsigned short' to `unsigned long long'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ushort_ullong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                        size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                        void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_uU(USHORT, ULLONG, unsigned short, unsigned long long, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_int_schar
 *
 * Purpose:    Converts `int' to `signed char'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_int_schar(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                    size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                    void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Ss(INT, SCHAR, int, signed char, SCHAR_MIN, SCHAR_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_int_uchar
 *
 * Purpose:    Converts `int' to `unsigned char'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_int_uchar(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                    size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                    void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Su(INT, UCHAR, int, unsigned char, -, UCHAR_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_uint_schar
 *
 * Purpose:    Converts `unsigned int' to `signed char'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_uint_schar(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                     size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                     void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Us(UINT, SCHAR, unsigned, signed char, -, SCHAR_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_uint_uchar
 *
 * Purpose:    Converts `unsigned int' to `unsigned char'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_uint_uchar(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                     size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                     void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Uu(UINT, UCHAR, unsigned, unsigned char, -, UCHAR_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_int_short
 *
 * Purpose:    Converts `int' to `short'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_int_short(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                    size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                    void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Ss(INT, SHORT, int, short, SHRT_MIN, SHRT_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_int_ushort
 *
 * Purpose:    Converts `int' to `unsigned short'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_int_ushort(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                     size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                     void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Su(INT, USHORT, int, unsigned short, -, USHRT_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_uint_short
 *
 * Purpose:    Converts `unsigned int' to `short'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_uint_short(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                     size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                     void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Us(UINT, SHORT, unsigned, short, -, SHRT_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_uint_ushort
 *
 * Purpose:    Converts `unsigned int' to `unsigned short'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_uint_ushort(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Uu(UINT, USHORT, unsigned, unsigned short, -, USHRT_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_int_uint
 *
 * Purpose:    Converts `int' to `unsigned int'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_int_uint(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                   size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                   void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_su(INT, UINT, int, unsigned, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_uint_int
 *
 * Purpose:    Converts `unsigned int' to `int'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_uint_int(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                   size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                   void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_us(UINT, INT, unsigned, int, -, INT_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_int_long
 *
 * Purpose:    Converts `int' to `long'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_int_long(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                   size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                   void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_sS(INT, LONG, int, long, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_int_ulong
 *
 * Purpose:    Converts `int' to `unsigned long'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_int_ulong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                    size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                    void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_sU(INT, LONG, int, unsigned long, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_uint_long
 *
 * Purpose:    Converts `unsigned int' to `long'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_uint_long(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                    size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                    void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_uS(UINT, LONG, unsigned, long, -, LONG_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_uint_ulong
 *
 * Purpose:    Converts `unsigned int' to `unsigned long'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_uint_ulong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                     size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                     void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_uU(UINT, ULONG, unsigned, unsigned long, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_int_llong
 *
 * Purpose:    Converts `int' to `long long'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_int_llong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                    size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                    void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_sS(INT, LLONG, int, long long, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_int_ullong
 *
 * Purpose:    Converts `int' to `unsigned long long'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_int_ullong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                     size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                     void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_sU(INT, ULLONG, int, unsigned long long, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_uint_llong
 *
 * Purpose:    Converts `unsigned int' to `long long'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_uint_llong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                     size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                     void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_uS(UINT, LLONG, unsigned, long long, -, LLONG_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_uint_ullong
 *
 * Purpose:    Converts `unsigned int' to `unsigned long long'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_uint_ullong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_uU(UINT, ULLONG, unsigned, unsigned long long, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_long_schar
 *
 * Purpose:    Converts `long' to `signed char'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_long_schar(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                     size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                     void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Ss(LONG, SCHAR, long, signed char, SCHAR_MIN, SCHAR_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_long_uchar
 *
 * Purpose:    Converts `long' to `unsigned char'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_long_uchar(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                     size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                     void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Su(LONG, UCHAR, long, unsigned char, -, UCHAR_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ulong_schar
 *
 * Purpose:    Converts `unsigned long' to `signed char'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ulong_schar(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Us(ULONG, SCHAR, unsigned long, signed char, -, SCHAR_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ulong_uchar
 *
 * Purpose:    Converts `unsigned long' to `unsigned char'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ulong_uchar(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Uu(ULONG, UCHAR, unsigned long, unsigned char, -, UCHAR_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_long_short
 *
 * Purpose:    Converts `long' to `short'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_long_short(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                     size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                     void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Ss(LONG, SHORT, long, short, SHRT_MIN, SHRT_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_long_ushort
 *
 * Purpose:    Converts `long' to `unsigned short'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_long_ushort(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Su(LONG, USHORT, long, unsigned short, -, USHRT_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ulong_short
 *
 * Purpose:    Converts `unsigned long' to `short'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ulong_short(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Us(ULONG, SHORT, unsigned long, short, -, SHRT_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ulong_ushort
 *
 * Purpose:    Converts `unsigned long' to `unsigned short'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ulong_ushort(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Uu(ULONG, USHORT, unsigned long, unsigned short, -, USHRT_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_long_int
 *
 * Purpose:    Converts `long' to `int'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_long_int(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                   size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                   void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Ss(LONG, INT, long, int, INT_MIN, INT_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_long_uint
 *
 * Purpose:    Converts `long' to `unsigned int'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_long_uint(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                    size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                    void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Su(LONG, UINT, long, unsigned, -, UINT_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ulong_int
 *
 * Purpose:    Converts `unsigned long' to `int'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ulong_int(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                    size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                    void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Us(ULONG, INT, unsigned long, int, -, INT_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ulong_uint
 *
 * Purpose:    Converts `unsigned long' to `unsigned int'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ulong_uint(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                     size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                     void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Uu(ULONG, UINT, unsigned long, unsigned, -, UINT_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_long_ulong
 *
 * Purpose:    Converts `long' to `unsigned long'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_long_ulong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                     size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                     void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_su(LONG, ULONG, long, unsigned long, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ulong_long
 *
 * Purpose:    Converts `unsigned long' to `long'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ulong_long(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                     size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                     void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_us(ULONG, LONG, unsigned long, long, -, LONG_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_long_llong
 *
 * Purpose:    Converts `long' to `long long'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_long_llong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                     size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                     void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_sS(LONG, LLONG, long, long long, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_long_ullong
 *
 * Purpose:    Converts `long' to `unsigned long long'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_long_ullong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_sU(LONG, ULLONG, long, unsigned long long, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ulong_llong
 *
 * Purpose:    Converts `unsigned long' to `long long'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ulong_llong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_uS(ULONG, LLONG, unsigned long, long long, -, LLONG_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ulong_ullong
 *
 * Purpose:    Converts `unsigned long' to `unsigned long long'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ulong_ullong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_uU(ULONG, ULLONG, unsigned long, unsigned long long, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_llong_schar
 *
 * Purpose:    Converts `long long' to `signed char'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_llong_schar(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Ss(LLONG, SCHAR, long long, signed char, SCHAR_MIN, SCHAR_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_llong_uchar
 *
 * Purpose:    Converts `long long' to `unsigned char'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_llong_uchar(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Su(LLONG, UCHAR, long long, unsigned char, -, UCHAR_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ullong_schar
 *
 * Purpose:    Converts `unsigned long long' to `signed char'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ullong_schar(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Us(ULLONG, SCHAR, unsigned long long, signed char, -, SCHAR_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ullong_uchar
 *
 * Purpose:    Converts `unsigned long long' to `unsigned char'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ullong_uchar(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Uu(ULLONG, UCHAR, unsigned long long, unsigned char, -, UCHAR_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_llong_short
 *
 * Purpose:    Converts `long long' to `short'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_llong_short(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Ss(LLONG, SHORT, long long, short, SHRT_MIN, SHRT_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_llong_ushort
 *
 * Purpose:    Converts `long long' to `unsigned short'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_llong_ushort(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Su(LLONG, USHORT, long long, unsigned short, -, USHRT_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ullong_short
 *
 * Purpose:    Converts `unsigned long long' to `short'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ullong_short(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Us(ULLONG, SHORT, unsigned long long, short, -, SHRT_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ullong_ushort
 *
 * Purpose:    Converts `unsigned long long' to `unsigned short'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ullong_ushort(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                        size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                        void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Uu(ULLONG, USHORT, unsigned long long, unsigned short, -, USHRT_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_llong_int
 *
 * Purpose:    Converts `long long' to `int'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_llong_int(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                    size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                    void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Ss(LLONG, INT, long long, int, INT_MIN, INT_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_llong_uint
 *
 * Purpose:    Converts `long long' to `unsigned int'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_llong_uint(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                     size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                     void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Su(LLONG, UINT, long long, unsigned, -, UINT_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ullong_int
 *
 * Purpose:    Converts `unsigned long long' to `int'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ullong_int(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                     size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                     void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Us(ULLONG, INT, unsigned long long, int, -, INT_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ullong_uint
 *
 * Purpose:    Converts `unsigned long long' to `unsigned int'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ullong_uint(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Uu(ULLONG, UINT, unsigned long long, unsigned, -, UINT_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_llong_long
 *
 * Purpose:    Converts `long long' to `long'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_llong_long(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                     size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                     void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Ss(LLONG, LONG, long long, long, LONG_MIN, LONG_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_llong_ulong
 *
 * Purpose:    Converts `long long' to `unsigned long'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_llong_ulong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Su(LLONG, ULONG, long long, unsigned long, -, ULONG_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ullong_long
 *
 * Purpose:    Converts `unsigned long long' to `long'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ullong_long(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Us(ULLONG, LONG, unsigned long long, long, -, LONG_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ullong_ulong
 *
 * Purpose:    Converts `unsigned long long' to `unsigned long'
 *
 * Return:    Success:    Non-negative
 *
 *            Failure:    Negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ullong_ulong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Uu(ULLONG, ULONG, unsigned long long, unsigned long, -, ULONG_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_llong_ullong
 *
 * Purpose:    Converts `long long' to `unsigned long long'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_llong_ullong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_su(LLONG, ULLONG, long long, unsigned long long, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ullong_llong
 *
 * Purpose:    Converts `unsigned long long' to `long long'
 *
 * Return:    Success:    non-negative
 *
 *            Failure:    negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ullong_llong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_us(ULLONG, LLONG, unsigned long long, long long, -, LLONG_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_float_double
 *
 * Purpose:    Convert native `float' to native `double' using hardware.
 *        This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_float_double(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_fF(FLOAT, DOUBLE, float, double, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_float_ldouble
 *
 * Purpose:    Convert native `float' to native `long double' using hardware.
 *        This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_float_ldouble(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                        size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                        void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_fF(FLOAT, LDOUBLE, float, long double, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_double_float
 *
 * Purpose:    Convert native `double' to native `float' using hardware.
 *        This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_double_float(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Ff(DOUBLE, FLOAT, double, float, -FLT_MAX, FLT_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_double_ldouble
 *
 * Purpose:    Convert native `double' to native `long double' using hardware.
 *        This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_double_ldouble(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                         size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                         void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_fF(DOUBLE, LDOUBLE, double, long double, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ldouble_float
 *
 * Purpose:    Convert native `long double' to native `float' using hardware.
 *        This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ldouble_float(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                        size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                        void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Ff(LDOUBLE, FLOAT, long double, float, -FLT_MAX, FLT_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ldouble_double
 *
 * Purpose:    Convert native `long double' to native `double' using hardware.
 *        This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ldouble_double(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                         size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                         void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_Ff(LDOUBLE, DOUBLE, long double, double, -DBL_MAX, DBL_MAX);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_schar_float
 *
 * Purpose:    Convert native signed char to native float using hardware.
 *        This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_schar_float(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(SCHAR, FLOAT, signed char, float, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_schar_double
 *
 * Purpose:    Convert native signed char to native double using hardware.
 *        This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_schar_double(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(SCHAR, DOUBLE, signed char, double, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_schar_ldouble
 *
 * Purpose:    Convert native signed char to native long double using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_schar_ldouble(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                        size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                        void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(SCHAR, LDOUBLE, signed char, long double, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_uchar_float
 *
 * Purpose:    Convert native unsigned char to native float using hardware.
 *        This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_uchar_float(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(UCHAR, FLOAT, unsigned char, float, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_uchar_double
 *
 * Purpose:    Convert native unsigned char to native double using hardware.
 *        This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_uchar_double(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(UCHAR, DOUBLE, unsigned char, double, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_uchar_ldouble
 *
 * Purpose:    Convert native unsigned char to native long double using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_uchar_ldouble(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                        size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                        void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(UCHAR, LDOUBLE, unsigned char, long double, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_short_float
 *
 * Purpose:    Convert native short to native float using hardware.
 *        This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_short_float(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(SHORT, FLOAT, short, float, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_short_double
 *
 * Purpose:    Convert native short to native double using hardware.
 *        This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_short_double(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(SHORT, DOUBLE, short, double, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_short_ldouble
 *
 * Purpose:    Convert native short to native long double using hardware.
 *        This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_short_ldouble(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                        size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                        void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(SHORT, LDOUBLE, short, long double, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ushort_float
 *
 * Purpose:    Convert native unsigned short to native float using hardware.
 *        This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ushort_float(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(USHORT, FLOAT, unsigned short, float, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ushort_double
 *
 * Purpose:    Convert native unsigned short to native double using hardware.
 *        This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ushort_double(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                        size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                        void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(USHORT, DOUBLE, unsigned short, double, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ushort_ldouble
 *
 * Purpose:    Convert native unsigned short to native long double using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ushort_ldouble(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                         size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                         void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(USHORT, LDOUBLE, unsigned short, long double, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_int_float
 *
 * Purpose:    Convert native integer to native float using hardware.
 *        This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_int_float(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                    size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                    void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(INT, FLOAT, int, float, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_int_double
 *
 * Purpose:    Convert native integer to native double using hardware.
 *        This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_int_double(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                     size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                     void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(INT, DOUBLE, int, double, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_int_ldouble
 *
 * Purpose:    Convert native integer to native long double using hardware.
 *        This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_int_ldouble(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(INT, LDOUBLE, int, long double, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_uint_float
 *
 * Purpose:    Convert native unsigned integer to native float using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_uint_float(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                     size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                     void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(UINT, FLOAT, unsigned int, float, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_uint_double
 *
 * Purpose:    Convert native unsigned integer to native double using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_uint_double(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(UINT, DOUBLE, unsigned int, double, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_uint_ldouble
 *
 * Purpose:    Convert native unsigned integer to native long double using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_uint_ldouble(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(UINT, LDOUBLE, unsigned int, long double, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_long_float
 *
 * Purpose:    Convert native long to native float using hardware.
 *        This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_long_float(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                     size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                     void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(LONG, FLOAT, long, float, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_long_double
 *
 * Purpose:    Convert native long to native double using hardware.
 *        This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_long_double(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(LONG, DOUBLE, long, double, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_long_ldouble
 *
 * Purpose:    Convert native long to native long double using hardware.
 *        This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_long_ldouble(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(LONG, LDOUBLE, long, long double, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ulong_float
 *
 * Purpose:    Convert native unsigned long to native float using hardware.
 *        This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ulong_float(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(ULONG, FLOAT, unsigned long, float, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ulong_double
 *
 * Purpose:    Convert native unsigned long to native double using hardware.
 *        This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ulong_double(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(ULONG, DOUBLE, unsigned long, double, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ulong_ldouble
 *
 * Purpose:    Convert native unsigned long to native long double using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ulong_ldouble(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                        size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                        void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(ULONG, LDOUBLE, unsigned long, long double, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_llong_float
 *
 * Purpose:    Convert native long long to native float using hardware.
 *        This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_llong_float(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(LLONG, FLOAT, long long, float, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_llong_double
 *
 * Purpose:    Convert native long long to native double using hardware.
 *        This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_llong_double(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(LLONG, DOUBLE, long long, double, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_llong_ldouble
 *
 * Purpose:    Convert native long long to native long double using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5T_CONV_INTERNAL_LLONG_LDOUBLE
herr_t
H5T__conv_llong_ldouble(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                        size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                        void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(LLONG, LDOUBLE, long long, long double, -, -);
}
#endif /* H5T_CONV_INTERNAL_LLONG_LDOUBLE */

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ullong_float
 *
 * Purpose:    Convert native unsigned long long to native float using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ullong_float(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(ULLONG, FLOAT, unsigned long long, float, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ullong_double
 *
 * Purpose:    Convert native unsigned long long to native double using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ullong_double(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                        size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                        void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(ULLONG, DOUBLE, unsigned long long, double, -, -);
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ullong_ldouble
 *
 * Purpose:    Convert native unsigned long long to native long double using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5T_CONV_INTERNAL_ULLONG_LDOUBLE
herr_t
H5T__conv_ullong_ldouble(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                         size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                         void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(ULLONG, LDOUBLE, unsigned long long, long double, -, -);
}
#endif /*H5T_CONV_INTERNAL_ULLONG_LDOUBLE*/

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_float_schar
 *
 * Purpose:    Convert native float to native signed char using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_float_schar(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(FLOAT, SCHAR, float, signed char, SCHAR_MIN, SCHAR_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_float_uchar
 *
 * Purpose:    Convert native float to native unsigned char using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_float_uchar(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(FLOAT, UCHAR, float, unsigned char, 0, UCHAR_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_double_schar
 *
 * Purpose:    Convert native double to native signed char using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_double_schar(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(DOUBLE, SCHAR, double, signed char, SCHAR_MIN, SCHAR_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_double_uchar
 *
 * Purpose:    Convert native double to native unsigned char using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_double_uchar(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(DOUBLE, UCHAR, double, unsigned char, 0, UCHAR_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ldouble_schar
 *
 * Purpose:    Convert native long double to native signed char using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ldouble_schar(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                        size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                        void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(LDOUBLE, SCHAR, long double, signed char, SCHAR_MIN, SCHAR_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ldouble_uchar
 *
 * Purpose:    Convert native long double to native unsigned char using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ldouble_uchar(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                        size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                        void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(LDOUBLE, UCHAR, long double, unsigned char, 0, UCHAR_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_float_short
 *
 * Purpose:    Convert native float to native short using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_float_short(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(FLOAT, SHORT, float, short, SHRT_MIN, SHRT_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_float_ushort
 *
 * Purpose:    Convert native float to native unsigned short using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_float_ushort(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(FLOAT, USHORT, float, unsigned short, 0, USHRT_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_double_short
 *
 * Purpose:    Convert native double to native short using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_double_short(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(DOUBLE, SHORT, double, short, SHRT_MIN, SHRT_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_double_ushort
 *
 * Purpose:    Convert native double to native unsigned short using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_double_ushort(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                        size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                        void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(DOUBLE, USHORT, double, unsigned short, 0, USHRT_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ldouble_short
 *
 * Purpose:    Convert native long double to native short using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ldouble_short(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                        size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                        void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(LDOUBLE, SHORT, long double, short, SHRT_MIN, SHRT_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ldouble_ushort
 *
 * Purpose:    Convert native long double to native unsigned short using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ldouble_ushort(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                         size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                         void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(LDOUBLE, USHORT, long double, unsigned short, 0, USHRT_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_float_int
 *
 * Purpose:    Convert native float to native int using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_float_int(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                    size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                    void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(FLOAT, INT, float, int, INT_MIN, INT_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_float_uint
 *
 * Purpose:    Convert native float to native unsigned int using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_float_uint(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                     size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                     void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(FLOAT, UINT, float, unsigned int, 0, UINT_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_double_int
 *
 * Purpose:    Convert native double to native int using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_double_int(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                     size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                     void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(DOUBLE, INT, double, int, INT_MIN, INT_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_double_uint
 *
 * Purpose:    Convert native double to native unsigned int using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_double_uint(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(DOUBLE, UINT, double, unsigned int, 0, UINT_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ldouble_int
 *
 * Purpose:    Convert native long double to native int using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ldouble_int(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(LDOUBLE, INT, long double, int, INT_MIN, INT_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ldouble_uint
 *
 * Purpose:    Convert native long double to native unsigned int using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ldouble_uint(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(LDOUBLE, UINT, long double, unsigned int, 0, UINT_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_float_long
 *
 * Purpose:    Convert native float to native long using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_float_long(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                     size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                     void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(FLOAT, LONG, float, long, LONG_MIN, LONG_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_float_ulong
 *
 * Purpose:    Convert native float to native unsigned long using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_float_ulong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(FLOAT, ULONG, float, unsigned long, 0, ULONG_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_double_long
 *
 * Purpose:    Convert native double to native long using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_double_long(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(DOUBLE, LONG, double, long, LONG_MIN, LONG_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_double_ulong
 *
 * Purpose:    Convert native double to native unsigned long using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_double_ulong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(DOUBLE, ULONG, double, unsigned long, 0, ULONG_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ldouble_long
 *
 * Purpose:    Convert native long double to native long using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ldouble_long(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(LDOUBLE, LONG, long double, long, LONG_MIN, LONG_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ldouble_ulong
 *
 * Purpose:    Convert native long double to native unsigned long using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_ldouble_ulong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                        size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                        void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(LDOUBLE, ULONG, long double, unsigned long, 0, ULONG_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_float_llong
 *
 * Purpose:    Convert native float to native long long using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_float_llong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                      size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                      void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(FLOAT, LLONG, float, long long, LLONG_MIN, LLONG_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_float_ullong
 *
 * Purpose:    Convert native float to native unsigned long long using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_float_ullong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(FLOAT, ULLONG, float, unsigned long long, 0, ULLONG_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_double_llong
 *
 * Purpose:    Convert native double to native long long using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_double_llong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(DOUBLE, LLONG, double, long long, LLONG_MIN, LLONG_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_double_ullong
 *
 * Purpose:    Convert native double to native unsigned long long using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_double_ullong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                        size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                        void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(DOUBLE, ULLONG, double, unsigned long long, 0, ULLONG_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ldouble_llong
 *
 * Purpose:    Convert native long double to native long long using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5T_CONV_INTERNAL_LDOUBLE_LLONG
herr_t
H5T__conv_ldouble_llong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                        size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                        void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(LDOUBLE, LLONG, long double, long long, LLONG_MIN, LLONG_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}
#endif /*H5T_CONV_INTERNAL_LDOUBLE_LLONG*/

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_ldouble_ullong
 *
 * Purpose:    Convert native long double to native unsigned long long using
 *              hardware.  This is a fast special case.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5T_CONV_INTERNAL_LDOUBLE_ULLONG
herr_t
H5T__conv_ldouble_ullong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                         size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                         void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(LDOUBLE, ULLONG, long double, unsigned long long, 0, ULLONG_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}
#endif /*H5T_CONV_INTERNAL_LDOUBLE_ULLONG*/

/* Conversions for _Float16 type */
#ifdef H5_HAVE__FLOAT16
herr_t
H5T__conv_schar__Float16(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                         size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                         void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(SCHAR, FLOAT16, signed char, H5__Float16, -, -);
}

herr_t
H5T__conv_uchar__Float16(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                         size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                         void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(UCHAR, FLOAT16, unsigned char, H5__Float16, -, -);
}

herr_t
H5T__conv_short__Float16(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                         size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                         void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_xF(SHORT, FLOAT16, short, H5__Float16, -, -);
}

herr_t
H5T__conv_ushort__Float16(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata,
                          const H5T_conv_ctx_t *conv_ctx, size_t nelmts, size_t buf_stride,
                          size_t H5_ATTR_UNUSED bkg_stride, void *buf, void H5_ATTR_UNUSED *bkg)
{
    /* Suppress warning about non-standard floating-point literal suffix */
    H5_GCC_CLANG_DIAG_OFF("pedantic")
    H5T_CONV_Xf(USHORT, FLOAT16, unsigned short, H5__Float16, -FLT16_MAX, FLT16_MAX);
    H5_GCC_CLANG_DIAG_ON("pedantic")
}

herr_t
H5T__conv_int__Float16(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    /* Suppress warning about non-standard floating-point literal suffix */
    H5_GCC_CLANG_DIAG_OFF("pedantic")
    H5T_CONV_Xf(INT, FLOAT16, int, H5__Float16, -FLT16_MAX, FLT16_MAX);
    H5_GCC_CLANG_DIAG_ON("pedantic")
}

herr_t
H5T__conv_uint__Float16(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                        size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                        void H5_ATTR_UNUSED *bkg)
{
    /* Suppress warning about non-standard floating-point literal suffix */
    H5_GCC_CLANG_DIAG_OFF("pedantic")
    H5T_CONV_Xf(UINT, FLOAT16, unsigned int, H5__Float16, -FLT16_MAX, FLT16_MAX);
    H5_GCC_CLANG_DIAG_ON("pedantic")
}

herr_t
H5T__conv_long__Float16(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                        size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                        void H5_ATTR_UNUSED *bkg)
{
    /* Suppress warning about non-standard floating-point literal suffix */
    H5_GCC_CLANG_DIAG_OFF("pedantic")
    H5T_CONV_Xf(LONG, FLOAT16, long, H5__Float16, -FLT16_MAX, FLT16_MAX);
    H5_GCC_CLANG_DIAG_ON("pedantic")
}

herr_t
H5T__conv_ulong__Float16(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                         size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                         void H5_ATTR_UNUSED *bkg)
{
    /* Suppress warning about non-standard floating-point literal suffix */
    H5_GCC_CLANG_DIAG_OFF("pedantic")
    H5T_CONV_Xf(ULONG, FLOAT16, unsigned long, H5__Float16, -FLT16_MAX, FLT16_MAX);
    H5_GCC_CLANG_DIAG_ON("pedantic")
}

herr_t
H5T__conv_llong__Float16(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                         size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                         void H5_ATTR_UNUSED *bkg)
{
    /* Suppress warning about non-standard floating-point literal suffix */
    H5_GCC_CLANG_DIAG_OFF("pedantic")
    H5T_CONV_Xf(LLONG, FLOAT16, long long, H5__Float16, -FLT16_MAX, FLT16_MAX);
    H5_GCC_CLANG_DIAG_ON("pedantic")
}

herr_t
H5T__conv_ullong__Float16(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata,
                          const H5T_conv_ctx_t *conv_ctx, size_t nelmts, size_t buf_stride,
                          size_t H5_ATTR_UNUSED bkg_stride, void *buf, void H5_ATTR_UNUSED *bkg)
{
    /* Suppress warning about non-standard floating-point literal suffix */
    H5_GCC_CLANG_DIAG_OFF("pedantic")
    H5T_CONV_Xf(ULLONG, FLOAT16, unsigned long long, H5__Float16, -FLT16_MAX, FLT16_MAX);
    H5_GCC_CLANG_DIAG_ON("pedantic")
}

herr_t
H5T__conv_float__Float16(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                         size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                         void H5_ATTR_UNUSED *bkg)
{
    /* Suppress warning about non-standard floating-point literal suffix */
    H5_GCC_CLANG_DIAG_OFF("pedantic")
    H5T_CONV_Ff(FLOAT, FLOAT16, float, H5__Float16, -FLT16_MAX, FLT16_MAX);
    H5_GCC_CLANG_DIAG_ON("pedantic")
}

herr_t
H5T__conv_double__Float16(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata,
                          const H5T_conv_ctx_t *conv_ctx, size_t nelmts, size_t buf_stride,
                          size_t H5_ATTR_UNUSED bkg_stride, void *buf, void H5_ATTR_UNUSED *bkg)
{
    /* Suppress warning about non-standard floating-point literal suffix */
    H5_GCC_CLANG_DIAG_OFF("pedantic")
    H5T_CONV_Ff(DOUBLE, FLOAT16, double, H5__Float16, -FLT16_MAX, FLT16_MAX);
    H5_GCC_CLANG_DIAG_ON("pedantic")
}

#ifdef H5T_CONV_INTERNAL_LDOUBLE_FLOAT16
herr_t
H5T__conv_ldouble__Float16(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata,
                           const H5T_conv_ctx_t *conv_ctx, size_t nelmts, size_t buf_stride,
                           size_t H5_ATTR_UNUSED bkg_stride, void *buf, void H5_ATTR_UNUSED *bkg)
{
    /* Suppress warning about non-standard floating-point literal suffix */
    H5_GCC_CLANG_DIAG_OFF("pedantic")
    H5T_CONV_Ff(LDOUBLE, FLOAT16, long double, H5__Float16, -FLT16_MAX, FLT16_MAX);
    H5_GCC_CLANG_DIAG_ON("pedantic")
}
#endif

herr_t
H5T__conv__Float16_schar(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                         size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                         void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(FLOAT16, SCHAR, H5__Float16, signed char, SCHAR_MIN, SCHAR_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}

herr_t
H5T__conv__Float16_uchar(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                         size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                         void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(FLOAT16, UCHAR, H5__Float16, unsigned char, 0, UCHAR_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}

herr_t
H5T__conv__Float16_short(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                         size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                         void H5_ATTR_UNUSED *bkg)
{
    H5_GCC_CLANG_DIAG_OFF("float-equal")
    H5T_CONV_Fx(FLOAT16, SHORT, H5__Float16, short, SHRT_MIN, SHRT_MAX);
    H5_GCC_CLANG_DIAG_ON("float-equal")
}

herr_t
H5T__conv__Float16_ushort(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata,
                          const H5T_conv_ctx_t *conv_ctx, size_t nelmts, size_t buf_stride,
                          size_t H5_ATTR_UNUSED bkg_stride, void *buf, void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_fX(FLOAT16, USHORT, H5__Float16, unsigned short, 0, USHRT_MAX);
}

herr_t
H5T__conv__Float16_int(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                       size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                       void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_fX(FLOAT16, INT, H5__Float16, int, INT_MIN, INT_MAX);
}

herr_t
H5T__conv__Float16_uint(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                        size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                        void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_fX(FLOAT16, UINT, H5__Float16, unsigned int, 0, UINT_MAX);
}

herr_t
H5T__conv__Float16_long(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                        size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                        void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_fX(FLOAT16, LONG, H5__Float16, long, LONG_MIN, LONG_MAX);
}

herr_t
H5T__conv__Float16_ulong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                         size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                         void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_fX(FLOAT16, ULONG, H5__Float16, unsigned long, 0, ULONG_MAX);
}

herr_t
H5T__conv__Float16_llong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                         size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                         void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_fX(FLOAT16, LLONG, H5__Float16, long long, LLONG_MIN, LLONG_MAX);
}

herr_t
H5T__conv__Float16_ullong(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata,
                          const H5T_conv_ctx_t *conv_ctx, size_t nelmts, size_t buf_stride,
                          size_t H5_ATTR_UNUSED bkg_stride, void *buf, void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_fX(FLOAT16, ULLONG, H5__Float16, unsigned long long, 0, ULLONG_MAX);
}

herr_t
H5T__conv__Float16_float(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
                         size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
                         void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_fF(FLOAT16, FLOAT, H5__Float16, float, -, -);
}

herr_t
H5T__conv__Float16_double(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata,
                          const H5T_conv_ctx_t *conv_ctx, size_t nelmts, size_t buf_stride,
                          size_t H5_ATTR_UNUSED bkg_stride, void *buf, void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_fF(FLOAT16, DOUBLE, H5__Float16, double, -, -);
}

herr_t
H5T__conv__Float16_ldouble(const H5T_t *st, const H5T_t *dt, H5T_cdata_t *cdata,
                           const H5T_conv_ctx_t *conv_ctx, size_t nelmts, size_t buf_stride,
                           size_t H5_ATTR_UNUSED bkg_stride, void *buf, void H5_ATTR_UNUSED *bkg)
{
    H5T_CONV_fF(FLOAT16, LDOUBLE, H5__Float16, long double, -, -);
}
#endif

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_f_i
 *
 * Purpose:    Convert one floating-point type to an integer.  This is
 *              the catch-all function for float-integer conversions and
 *              is probably not particularly fast.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_f_i(const H5T_t *src_p, const H5T_t *dst_p, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
              size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
              void H5_ATTR_UNUSED *bkg)
{
    /* Traversal-related variables */
    H5T_atomic_t src;             /*atomic source info        */
    H5T_atomic_t dst;             /*atomic destination info    */
    int          direction;       /*forward or backward traversal    */
    size_t       elmtno;          /*element number        */
    size_t       half_size;       /*half the type size        */
    size_t       tsize;           /*type size for swapping bytes  */
    size_t       olap;            /*num overlapping elements    */
    uint8_t     *s, *sp, *d, *dp; /*source and dest traversal ptrs*/
    uint8_t     *src_rev  = NULL; /*order-reversed source buffer  */
    uint8_t      dbuf[64] = {0};  /*temp destination buffer    */
    uint8_t      tmp1, tmp2;      /*temp variables for swapping bytes*/

    /* Conversion-related variables */
    hssize_t       expo;                /*source exponent        */
    hssize_t       sign;                /*source sign bit value         */
    uint8_t       *int_buf = NULL;      /*buffer for temporary value    */
    size_t         buf_size;            /*buffer size for temporary value */
    size_t         i;                   /*miscellaneous counters    */
    ssize_t        msb_pos_s;           /*first bit(MSB) in an integer */
    ssize_t        new_msb_pos;         /*MSB position after shifting mantissa by exponent */
    hssize_t       shift_val;           /*shift value when shifting mantissa by exponent */
    bool           truncated;           /*if fraction value is dropped  */
    bool           reverse;             /*if reverse order of destination at the end */
    H5T_conv_ret_t except_ret;          /*return of callback function   */
    herr_t         ret_value = SUCCEED; /* Return value                 */

    FUNC_ENTER_PACKAGE

    switch (cdata->command) {
        case H5T_CONV_INIT:
            if (NULL == src_p || NULL == dst_p)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype");
            src = src_p->shared->u.atomic;
            dst = dst_p->shared->u.atomic;
            if (H5T_ORDER_LE != src.order && H5T_ORDER_BE != src.order && H5T_ORDER_VAX != src.order)
                HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unsupported byte order");
            if (dst_p->shared->size > sizeof(dbuf))
                HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "destination size is too large");
            if (8 * sizeof(expo) - 1 < src.u.f.esize)
                HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "exponent field is too large");
            cdata->need_bkg = H5T_BKG_NO;
            break;

        case H5T_CONV_FREE:
            break;

        case H5T_CONV_CONV:
            if (NULL == src_p || NULL == dst_p)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype");
            if (NULL == conv_ctx)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid datatype conversion context pointer");

            src = src_p->shared->u.atomic;
            dst = dst_p->shared->u.atomic;

            /*
             * Do we process the values from beginning to end or vice versa? Also,
             * how many of the elements have the source and destination areas
             * overlapping?
             */
            if (src_p->shared->size == dst_p->shared->size || buf_stride) {
                sp = dp   = (uint8_t *)buf;
                direction = 1;
                olap      = nelmts;
            }
            else if (src_p->shared->size >= dst_p->shared->size) {
                double olap_d =
                    ceil((double)(dst_p->shared->size) / (double)(src_p->shared->size - dst_p->shared->size));
                olap = (size_t)olap_d;
                sp = dp   = (uint8_t *)buf;
                direction = 1;
            }
            else {
                double olap_d =
                    ceil((double)(src_p->shared->size) / (double)(dst_p->shared->size - src_p->shared->size));
                olap      = (size_t)olap_d;
                sp        = (uint8_t *)buf + (nelmts - 1) * src_p->shared->size;
                dp        = (uint8_t *)buf + (nelmts - 1) * dst_p->shared->size;
                direction = -1;
            }

            /* Allocate enough space for the buffer holding temporary
             * converted value
             */
            if (dst.prec / 8 > src_p->shared->size)
                buf_size = (dst.prec + 7) / 8;
            else
                buf_size = src_p->shared->size;
            int_buf = (uint8_t *)H5MM_calloc(buf_size);

            /* Allocate space for order-reversed source buffer */
            src_rev = (uint8_t *)H5MM_calloc(src_p->shared->size);

            /* The conversion loop */
            for (elmtno = 0; elmtno < nelmts; elmtno++) {
                /* Set these variables to default */
                except_ret = H5T_CONV_UNHANDLED;
                truncated  = false;
                reverse    = true;

                /*
                 * If the source and destination buffers overlap then use a
                 * temporary buffer for the destination.
                 */
                if (direction > 0) {
                    s = sp;
                    d = elmtno < olap ? dbuf : dp;
                }
                else {
                    s = sp;
                    d = elmtno + olap >= nelmts ? dbuf : dp;
                }
#ifndef NDEBUG
                /* I don't quite trust the overlap calculations yet --rpm */
                if (d == dbuf) {
                    assert((dp >= sp && dp < sp + src_p->shared->size) ||
                           (sp >= dp && sp < dp + dst_p->shared->size));
                }
                else {
                    assert((dp < sp && dp + dst_p->shared->size <= sp) ||
                           (sp < dp && sp + src_p->shared->size <= dp));
                }
#endif
                /*
                 * Put the data in little endian order so our loops aren't so
                 * complicated.  We'll do all the conversion stuff assuming
                 * little endian and then we'll fix the order at the end.
                 */
                if (H5T_ORDER_BE == src.order) {
                    half_size = src_p->shared->size / 2;
                    for (i = 0; i < half_size; i++) {
                        tmp1                             = s[src_p->shared->size - (i + 1)];
                        s[src_p->shared->size - (i + 1)] = s[i];
                        s[i]                             = tmp1;
                    }
                }
                else if (H5T_ORDER_VAX == src.order) {
                    tsize = src_p->shared->size;
                    assert(0 == tsize % 2);

                    for (i = 0; i < tsize; i += 4) {
                        tmp1 = s[i];
                        tmp2 = s[i + 1];

                        s[i]     = s[(tsize - 2) - i];
                        s[i + 1] = s[(tsize - 1) - i];

                        s[(tsize - 2) - i] = tmp1;
                        s[(tsize - 1) - i] = tmp2;
                    }
                }

                /*zero-set all destination bits*/
                H5T__bit_set(d, dst.offset, dst.prec, false);

                /*
                 * Find the sign bit value of the source.
                 */
                sign = (hssize_t)H5T__bit_get_d(s, src.u.f.sign, (size_t)1);

                /*
                 * Check for special cases: +0, -0, +Inf, -Inf, NaN
                 */
                if (H5T__bit_find(s, src.u.f.mpos, src.u.f.msize, H5T_BIT_LSB, true) < 0) {
                    if (H5T__bit_find(s, src.u.f.epos, src.u.f.esize, H5T_BIT_LSB, true) < 0) {
                        /* +0 or -0 */
                        /* Set all bits to zero */
                        goto padding;
                    }
                    else if (H5T__bit_find(s, src.u.f.epos, src.u.f.esize, H5T_BIT_LSB, false) < 0) {
                        /* +Infinity or -Infinity */
                        if (sign) { /* -Infinity */
                            if (conv_ctx->u.conv.cb_struct
                                    .func) { /*If user's exception handler is present, use it*/
                                /*reverse order first*/
                                H5T__reverse_order(src_rev, s, src_p->shared->size,
                                                   src_p->shared->u.atomic.order);
                                except_ret = (conv_ctx->u.conv.cb_struct.func)(
                                    H5T_CONV_EXCEPT_NINF, conv_ctx->u.conv.src_type_id,
                                    conv_ctx->u.conv.dst_type_id, src_rev, d,
                                    conv_ctx->u.conv.cb_struct.user_data);
                            }

                            if (except_ret == H5T_CONV_UNHANDLED) {
                                if (H5T_SGN_2 == dst.u.i.sign)
                                    H5T__bit_set(d, dst.prec - 1, (size_t)1, true);
                            }
                            else if (except_ret == H5T_CONV_HANDLED) {
                                /*No need to reverse the order of destination because user handles it*/
                                reverse = false;
                                goto next;
                            }
                            else if (except_ret == H5T_CONV_ABORT)
                                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL,
                                            "can't handle conversion exception");
                        }
                        else { /* +Infinity */
                            if (conv_ctx->u.conv.cb_struct
                                    .func) { /*If user's exception handler is present, use it*/
                                /*reverse order first*/
                                H5T__reverse_order(src_rev, s, src_p->shared->size,
                                                   src_p->shared->u.atomic.order);
                                except_ret = (conv_ctx->u.conv.cb_struct.func)(
                                    H5T_CONV_EXCEPT_PINF, conv_ctx->u.conv.src_type_id,
                                    conv_ctx->u.conv.dst_type_id, src_rev, d,
                                    conv_ctx->u.conv.cb_struct.user_data);
                            }

                            if (except_ret == H5T_CONV_UNHANDLED) {
                                if (H5T_SGN_NONE == dst.u.i.sign)
                                    H5T__bit_set(d, dst.offset, dst.prec, true);
                                else if (H5T_SGN_2 == dst.u.i.sign)
                                    H5T__bit_set(d, dst.offset, dst.prec - 1, true);
                            }
                            else if (except_ret == H5T_CONV_HANDLED) {
                                /*No need to reverse the order of destination because user handles it*/
                                reverse = false;
                                goto next;
                            }
                            else if (except_ret == H5T_CONV_ABORT)
                                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL,
                                            "can't handle conversion exception");
                        }
                        goto padding;
                    }
                }
                else if (H5T_NORM_NONE == src.u.f.norm &&
                         H5T__bit_find(s, src.u.f.mpos, src.u.f.msize - 1, H5T_BIT_LSB, true) < 0 &&
                         H5T__bit_find(s, src.u.f.epos, src.u.f.esize, H5T_BIT_LSB, false) < 0) {
                    /*This is a special case for the source of no implied mantissa bit.
                     *If the exponent bits are all 1s and only the 1st bit of mantissa
                     *is set to 1.  It's infinity. The Intel-Linux "long double" is this case.*/
                    /* +Infinity or -Infinity */
                    if (sign) { /* -Infinity */
                        if (conv_ctx->u.conv.cb_struct
                                .func) { /*If user's exception handler is present, use it*/
                            /*reverse order first*/
                            H5T__reverse_order(src_rev, s, src_p->shared->size,
                                               src_p->shared->u.atomic.order);
                            except_ret = (conv_ctx->u.conv.cb_struct.func)(
                                H5T_CONV_EXCEPT_NINF, conv_ctx->u.conv.src_type_id,
                                conv_ctx->u.conv.dst_type_id, src_rev, d,
                                conv_ctx->u.conv.cb_struct.user_data);
                        }

                        if (except_ret == H5T_CONV_UNHANDLED) {
                            if (H5T_SGN_2 == dst.u.i.sign)
                                H5T__bit_set(d, dst.prec - 1, (size_t)1, true);
                        }
                        else if (except_ret == H5T_CONV_HANDLED) {
                            /*No need to reverse the order of destination because user handles it*/
                            reverse = false;
                            goto next;
                        }
                        else if (except_ret == H5T_CONV_ABORT)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL,
                                        "can't handle conversion exception");
                    }
                    else { /* +Infinity */
                        if (conv_ctx->u.conv.cb_struct
                                .func) { /*If user's exception handler is present, use it*/
                            /*reverse order first*/
                            H5T__reverse_order(src_rev, s, src_p->shared->size,
                                               src_p->shared->u.atomic.order);
                            except_ret = (conv_ctx->u.conv.cb_struct.func)(
                                H5T_CONV_EXCEPT_PINF, conv_ctx->u.conv.src_type_id,
                                conv_ctx->u.conv.dst_type_id, src_rev, d,
                                conv_ctx->u.conv.cb_struct.user_data);
                        }

                        if (except_ret == H5T_CONV_UNHANDLED) {
                            if (H5T_SGN_NONE == dst.u.i.sign)
                                H5T__bit_set(d, dst.offset, dst.prec, true);
                            else if (H5T_SGN_2 == dst.u.i.sign)
                                H5T__bit_set(d, dst.offset, dst.prec - 1, true);
                        }
                        else if (except_ret == H5T_CONV_HANDLED) {
                            /*No need to reverse the order of destination because user handles it*/
                            reverse = false;
                            goto next;
                        }
                        else if (except_ret == H5T_CONV_ABORT)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL,
                                        "can't handle conversion exception");
                    }
                    goto padding;
                }
                else if (H5T__bit_find(s, src.u.f.epos, src.u.f.esize, H5T_BIT_LSB, false) < 0) {
                    /* NaN */
                    if (conv_ctx->u.conv.cb_struct.func) { /*If user's exception handler is present, use it*/
                        /*reverse order first*/
                        H5T__reverse_order(src_rev, s, src_p->shared->size, src_p->shared->u.atomic.order);
                        except_ret = (conv_ctx->u.conv.cb_struct.func)(
                            H5T_CONV_EXCEPT_NAN, conv_ctx->u.conv.src_type_id, conv_ctx->u.conv.dst_type_id,
                            src_rev, d, conv_ctx->u.conv.cb_struct.user_data);
                    }

                    if (except_ret == H5T_CONV_UNHANDLED) {
                        /*Just set all bits to zero.*/
                        goto padding;
                    }
                    else if (except_ret == H5T_CONV_HANDLED) {
                        /*No need to reverse the order of destination because user handles it*/
                        reverse = false;
                        goto next;
                    }
                    else if (except_ret == H5T_CONV_ABORT)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception");

                    goto padding;
                }

                /*
                 * Get the exponent as an unsigned quantity from the section of
                 * the source bit field where it's located.   Not expecting
                 * exponent to be greater than the maximal value of hssize_t.
                 */
                expo = (hssize_t)H5T__bit_get_d(s, src.u.f.epos, src.u.f.esize);

                /*
                 * Calculate the true source exponent by adjusting according to
                 * the source exponent bias.
                 */
                if (0 == expo || H5T_NORM_NONE == src.u.f.norm) {
                    expo -= (hssize_t)(src.u.f.ebias - 1);
                }
                else if (H5T_NORM_IMPLIED == src.u.f.norm) {
                    expo -= (hssize_t)src.u.f.ebias;
                }
                else {
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL,
                                "normalization method not implemented yet");
                }

                /*
                 * Get the mantissa as bit vector from the section of
                 * the source bit field where it's located.
                 * Keep the little-endian order in the buffer.
                 * A sequence 0x01020304 will be like in the buffer,
                 *      04      03      02      01
                 *      |       |       |       |
                 *      V       V       V       V
                 *    buf[0]  buf[1]  buf[2]  buf[3]
                 */
                H5T__bit_copy(int_buf, (size_t)0, s, src.u.f.mpos, src.u.f.msize);

                /*
                 * Restore the implicit bit for mantissa if it's implied.
                 * Equivalent to mantissa |= (hsize_t)1<<src.u.f.msize.
                 */
                if (H5T_NORM_IMPLIED == src.u.f.norm)
                    H5T__bit_inc(int_buf, src.u.f.msize, 8 * buf_size - src.u.f.msize);

                /*
                 * What is the bit position for the most significant bit(MSB) of S
                 * which is set?  This is checked before shifting and before possibly
                 * converting to a negative integer. Note that later use of this value
                 * assumes that H5T__bit_shift will always shift in 0 during a right
                 * shift.
                 */
                msb_pos_s = H5T__bit_find(int_buf, (size_t)0, src.prec, H5T_BIT_MSB, true);

                /*
                 * The temporary buffer has no bits set and must therefore be
                 * zero; nothing to do.
                 */
                if (msb_pos_s < 0)
                    goto padding;

                /*
                 * Shift mantissa part by exponent minus mantissa size(right shift),
                 * or by mantissa size minus exponent(left shift).  Example: Sequence
                 * 10...010111, expo=20, expo-msize=-3.  Right-shift the sequence, we get
                 * 00010...10.  The last three bits were dropped.
                 */
                shift_val = expo - (ssize_t)src.u.f.msize;
                H5T__bit_shift(int_buf, shift_val, (size_t)0, buf_size * 8);

                /* Calculate the new position of the MSB after shifting and
                 * skip to the padding section if we shifted exactly to 0
                 * (MSB position is -1)
                 */
                new_msb_pos = msb_pos_s + shift_val;
                if (new_msb_pos == -1)
                    goto padding;

                /*
                 * If expo is less than mantissa size, the fractional value is dropped off
                 * during conversion.  Set exception type to be "truncate"
                 */
                if ((size_t)expo < src.u.f.msize && conv_ctx->u.conv.cb_struct.func)
                    truncated = true;

                if (H5T_SGN_NONE == dst.u.i.sign) { /*destination is unsigned*/
                    /*
                     * Destination is unsigned.  Library's default way: If the source value
                     * is greater than the maximal destination value then it overflows, the
                     * destination will be set to the maximum possible value.  When the
                     * source is negative, underflow happens.  Set the destination to be
                     * zero(do nothing).  If user's exception handler is set, call it and
                     * let user handle it.
                     */
                    if (sign) { /*source is negative*/
                        if (conv_ctx->u.conv.cb_struct
                                .func) { /*If user's exception handler is present, use it*/
                            /*reverse order first*/
                            H5T__reverse_order(src_rev, s, src_p->shared->size,
                                               src_p->shared->u.atomic.order);
                            except_ret = (conv_ctx->u.conv.cb_struct.func)(
                                H5T_CONV_EXCEPT_RANGE_LOW, conv_ctx->u.conv.src_type_id,
                                conv_ctx->u.conv.dst_type_id, src_rev, d,
                                conv_ctx->u.conv.cb_struct.user_data);
                            if (except_ret == H5T_CONV_ABORT)
                                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL,
                                            "can't handle conversion exception");
                            else if (except_ret == H5T_CONV_HANDLED) {
                                /*No need to reverse the order of destination because user handles it*/
                                reverse = false;
                                goto next;
                            }
                        }
                    }
                    else { /*source is positive*/
                        if (new_msb_pos >= (ssize_t)dst.prec) {
                            /*overflow*/
                            if (conv_ctx->u.conv.cb_struct
                                    .func) { /*If user's exception handler is present, use it*/
                                /*reverse order first*/
                                H5T__reverse_order(src_rev, s, src_p->shared->size,
                                                   src_p->shared->u.atomic.order);
                                except_ret = (conv_ctx->u.conv.cb_struct.func)(
                                    H5T_CONV_EXCEPT_RANGE_HI, conv_ctx->u.conv.src_type_id,
                                    conv_ctx->u.conv.dst_type_id, src_rev, d,
                                    conv_ctx->u.conv.cb_struct.user_data);
                            }

                            if (except_ret == H5T_CONV_UNHANDLED)
                                H5T__bit_set(d, dst.offset, dst.prec, true);
                            else if (except_ret == H5T_CONV_HANDLED) {
                                /*No need to reverse the order of destination because user handles it*/
                                reverse = false;
                                goto next;
                            }
                            else if (except_ret == H5T_CONV_ABORT)
                                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL,
                                            "can't handle conversion exception");
                        }
                        else {
                            if (truncated && conv_ctx->u.conv.cb_struct
                                                 .func) { /*If user's exception handler is present, use it*/
                                /*reverse order first*/
                                H5T__reverse_order(src_rev, s, src_p->shared->size,
                                                   src_p->shared->u.atomic.order);
                                except_ret = (conv_ctx->u.conv.cb_struct.func)(
                                    H5T_CONV_EXCEPT_TRUNCATE, conv_ctx->u.conv.src_type_id,
                                    conv_ctx->u.conv.dst_type_id, src_rev, d,
                                    conv_ctx->u.conv.cb_struct.user_data);
                            }

                            if (except_ret == H5T_CONV_UNHANDLED) {
                                /*copy source value into it if case is ignored by user handler*/
                                if (new_msb_pos >= 0)
                                    H5T__bit_copy(d, dst.offset, int_buf, (size_t)0, (size_t)new_msb_pos + 1);
                            }
                            else if (except_ret == H5T_CONV_HANDLED) {
                                /*No need to reverse the order of destination because user handles it*/
                                reverse = false;
                                goto next;
                            }
                            else if (except_ret == H5T_CONV_ABORT)
                                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL,
                                            "can't handle conversion exception");
                        }
                    }
                }
                else if (H5T_SGN_2 == dst.u.i.sign) { /*Destination is signed*/
                    if (sign) {                       /*source is negative*/
                        if ((new_msb_pos >= 0) && ((size_t)new_msb_pos < dst.prec - 1)) {
                            if (truncated && conv_ctx->u.conv.cb_struct
                                                 .func) { /*If user's exception handler is present, use it*/
                                /*reverse order first*/
                                H5T__reverse_order(src_rev, s, src_p->shared->size,
                                                   src_p->shared->u.atomic.order);
                                except_ret = (conv_ctx->u.conv.cb_struct.func)(
                                    H5T_CONV_EXCEPT_TRUNCATE, conv_ctx->u.conv.src_type_id,
                                    conv_ctx->u.conv.dst_type_id, src_rev, d,
                                    conv_ctx->u.conv.cb_struct.user_data);
                            }

                            if (except_ret == H5T_CONV_UNHANDLED) { /*If this case ignored by user handler*/
                                /*Convert to integer representation.  Equivalent to ~(value - 1).*/
                                H5T__bit_dec(int_buf, (size_t)0, dst.prec);
                                H5T__bit_neg(int_buf, (size_t)0, dst.prec);

                                /*copy source value into destination*/
                                H5T__bit_copy(d, dst.offset, int_buf, (size_t)0, dst.prec - 1);
                                H5T__bit_set(d, (dst.offset + dst.prec - 1), (size_t)1, true);
                            }
                            else if (except_ret == H5T_CONV_ABORT)
                                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL,
                                            "can't handle conversion exception");
                            else if (except_ret == H5T_CONV_HANDLED) {
                                /*No need to reverse the order of destination because user handles it*/
                                reverse = false;
                                goto next;
                            }
                        }
                        else {
                            /* if underflows and no callback, do nothing except turn on
                             * the sign bit because 0x80...00 is the biggest negative value.
                             */
                            if (conv_ctx->u.conv.cb_struct
                                    .func) { /*If user's exception handler is present, use it*/
                                /*reverse order first*/
                                H5T__reverse_order(src_rev, s, src_p->shared->size,
                                                   src_p->shared->u.atomic.order);
                                except_ret = (conv_ctx->u.conv.cb_struct.func)(
                                    H5T_CONV_EXCEPT_RANGE_LOW, conv_ctx->u.conv.src_type_id,
                                    conv_ctx->u.conv.dst_type_id, src_rev, d,
                                    conv_ctx->u.conv.cb_struct.user_data);
                            }

                            if (except_ret == H5T_CONV_UNHANDLED)
                                H5T__bit_set(d, (dst.offset + dst.prec - 1), (size_t)1, true);
                            else if (except_ret == H5T_CONV_ABORT)
                                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL,
                                            "can't handle conversion exception");
                            else if (except_ret == H5T_CONV_HANDLED) {
                                /*No need to reverse the order of destination because user handles it*/
                                reverse = false;
                                goto next;
                            }
                        }
                    }
                    else { /*source is positive*/
                        if (new_msb_pos >= (ssize_t)dst.prec - 1) {
                            /*overflow*/
                            if (conv_ctx->u.conv.cb_struct
                                    .func) { /*If user's exception handler is present, use it*/
                                /*reverse order first*/
                                H5T__reverse_order(src_rev, s, src_p->shared->size,
                                                   src_p->shared->u.atomic.order);
                                except_ret = (conv_ctx->u.conv.cb_struct.func)(
                                    H5T_CONV_EXCEPT_RANGE_HI, conv_ctx->u.conv.src_type_id,
                                    conv_ctx->u.conv.dst_type_id, src_rev, d,
                                    conv_ctx->u.conv.cb_struct.user_data);
                            }

                            if (except_ret == H5T_CONV_UNHANDLED)
                                H5T__bit_set(d, dst.offset, dst.prec - 1, true);
                            else if (except_ret == H5T_CONV_ABORT)
                                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL,
                                            "can't handle conversion exception");
                            else if (except_ret == H5T_CONV_HANDLED) {
                                /*No need to reverse the order of destination because user handles it*/
                                reverse = false;
                                goto next;
                            }
                        }
                        else if (new_msb_pos < (ssize_t)dst.prec - 1) {
                            if (truncated && conv_ctx->u.conv.cb_struct
                                                 .func) { /*If user's exception handler is present, use it*/
                                /*reverse order first*/
                                H5T__reverse_order(src_rev, s, src_p->shared->size,
                                                   src_p->shared->u.atomic.order);
                                except_ret = (conv_ctx->u.conv.cb_struct.func)(
                                    H5T_CONV_EXCEPT_TRUNCATE, conv_ctx->u.conv.src_type_id,
                                    conv_ctx->u.conv.dst_type_id, src_rev, d,
                                    conv_ctx->u.conv.cb_struct.user_data);
                            }

                            if (except_ret == H5T_CONV_UNHANDLED) {
                                /*copy source value into it if case is ignored by user handler*/
                                if (new_msb_pos >= 0)
                                    H5T__bit_copy(d, dst.offset, int_buf, (size_t)0, (size_t)new_msb_pos + 1);
                            }
                            else if (except_ret == H5T_CONV_ABORT)
                                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL,
                                            "can't handle conversion exception");
                            else if (except_ret == H5T_CONV_HANDLED) {
                                /*No need to reverse the order of destination because user handles it*/
                                reverse = false;
                                goto next;
                            }
                        }
                    }
                }

padding:
                /*
                 * Set padding areas in destination.
                 */
                if (dst.offset > 0) {
                    assert(H5T_PAD_ZERO == dst.lsb_pad || H5T_PAD_ONE == dst.lsb_pad);
                    H5T__bit_set(d, (size_t)0, dst.offset, (bool)(H5T_PAD_ONE == dst.lsb_pad));
                }
                if (dst.offset + dst.prec != 8 * dst_p->shared->size) {
                    assert(H5T_PAD_ZERO == dst.msb_pad || H5T_PAD_ONE == dst.msb_pad);
                    H5T__bit_set(d, dst.offset + dst.prec, 8 * dst_p->shared->size - (dst.offset + dst.prec),
                                 (bool)(H5T_PAD_ONE == dst.msb_pad));
                }

                /*
                 * Put the destination in the correct byte order.  See note at
                 * beginning of loop.
                 */
                if (H5T_ORDER_BE == dst.order && reverse) {
                    half_size = dst_p->shared->size / 2;
                    for (i = 0; i < half_size; i++) {
                        tmp1                             = d[dst_p->shared->size - (i + 1)];
                        d[dst_p->shared->size - (i + 1)] = d[i];
                        d[i]                             = tmp1;
                    }
                }

next:
                /*
                 * If we had used a temporary buffer for the destination then we
                 * should copy the value to the true destination buffer.
                 */
                if (d == dbuf)
                    H5MM_memcpy(dp, d, dst_p->shared->size);
                if (buf_stride) {
                    sp += direction * (ssize_t)buf_stride;
                    dp += direction * (ssize_t)buf_stride;
                }
                else {
                    sp += direction * (ssize_t)src_p->shared->size;
                    dp += direction * (ssize_t)dst_p->shared->size;
                }

                memset(int_buf, 0, buf_size);
            }

            break;

        default:
            HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unknown conversion command");
    } /* end switch */

done:
    if (int_buf)
        H5MM_xfree(int_buf);
    if (src_rev)
        H5MM_free(src_rev);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5T__conv_f_i() */

/*-------------------------------------------------------------------------
 * Function:    H5T__conv_i_f
 *
 * Purpose:    Convert one integer type to a floating-point type.  This is
 *              the catch-all function for integer-float conversions and
 *              is probably not particularly fast.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T__conv_i_f(const H5T_t *src_p, const H5T_t *dst_p, H5T_cdata_t *cdata, const H5T_conv_ctx_t *conv_ctx,
              size_t nelmts, size_t buf_stride, size_t H5_ATTR_UNUSED bkg_stride, void *buf,
              void H5_ATTR_UNUSED *bkg)
{
    /* Traversal-related variables */
    H5T_atomic_t src;             /*atomic source info        */
    H5T_atomic_t dst;             /*atomic destination info    */
    int          direction;       /*forward or backward traversal    */
    size_t       elmtno;          /*element number        */
    size_t       half_size;       /*half the type size        */
    size_t       tsize;           /*type size for swapping bytes  */
    size_t       olap;            /*num overlapping elements    */
    uint8_t     *s, *sp, *d, *dp; /*source and dest traversal ptrs*/
    uint8_t     *src_rev  = NULL; /*order-reversed source buffer  */
    uint8_t      dbuf[64] = {0};  /*temp destination buffer    */
    uint8_t      tmp1, tmp2;      /*temp variables for swapping bytes*/

    /* Conversion-related variables */
    hsize_t        expo;                /*destination exponent        */
    hsize_t        expo_max;            /*maximal possible exponent value       */
    size_t         sign;                /*source sign bit value         */
    bool           is_max_neg;          /*source is maximal negative value*/
    bool           do_round;            /*whether there is roundup      */
    uint8_t       *int_buf = NULL;      /*buffer for temporary value    */
    size_t         buf_size;            /*buffer size for temporary value */
    size_t         i;                   /*miscellaneous counters    */
    size_t         first;               /*first bit(MSB) in an integer  */
    ssize_t        sfirst;              /*a signed version of `first'    */
    H5T_conv_ret_t except_ret;          /*return of callback function   */
    bool           reverse;             /*if reverse the order of destination   */
    herr_t         ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    switch (cdata->command) {
        case H5T_CONV_INIT:
            if (NULL == src_p || NULL == dst_p)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype");
            src = src_p->shared->u.atomic;
            dst = dst_p->shared->u.atomic;
            if (H5T_ORDER_LE != dst.order && H5T_ORDER_BE != dst.order && H5T_ORDER_VAX != dst.order)
                HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unsupported byte order");
            if (dst_p->shared->size > sizeof(dbuf))
                HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "destination size is too large");
            if (8 * sizeof(expo) - 1 < src.u.f.esize)
                HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "exponent field is too large");
            cdata->need_bkg = H5T_BKG_NO;
            break;

        case H5T_CONV_FREE:
            break;

        case H5T_CONV_CONV:
            if (NULL == src_p || NULL == dst_p)
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype");
            if (NULL == conv_ctx)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid datatype conversion context pointer");

            src = src_p->shared->u.atomic;
            dst = dst_p->shared->u.atomic;

            /*
             * Do we process the values from beginning to end or vice versa? Also,
             * how many of the elements have the source and destination areas
             * overlapping?
             */
            if (src_p->shared->size == dst_p->shared->size || buf_stride) {
                sp = dp   = (uint8_t *)buf;
                direction = 1;
                olap      = nelmts;
            }
            else if (src_p->shared->size >= dst_p->shared->size) {
                double olap_d =
                    ceil((double)(dst_p->shared->size) / (double)(src_p->shared->size - dst_p->shared->size));
                olap = (size_t)olap_d;
                sp = dp   = (uint8_t *)buf;
                direction = 1;
            }
            else {
                double olap_d =
                    ceil((double)(src_p->shared->size) / (double)(dst_p->shared->size - src_p->shared->size));
                olap      = (size_t)olap_d;
                sp        = (uint8_t *)buf + (nelmts - 1) * src_p->shared->size;
                dp        = (uint8_t *)buf + (nelmts - 1) * dst_p->shared->size;
                direction = -1;
            }

            /* Allocate enough space for the buffer holding temporary
             * converted value
             */
            buf_size = ((src.prec > dst.u.f.msize ? src.prec : dst.u.f.msize) + 7) / 8;
            int_buf  = (uint8_t *)H5MM_calloc(buf_size);

            /* Allocate space for order-reversed source buffer */
            src_rev = (uint8_t *)H5MM_calloc(src_p->shared->size);

            /* The conversion loop */
            for (elmtno = 0; elmtno < nelmts; elmtno++) {
                /* Set these variables to default */
                except_ret = H5T_CONV_UNHANDLED;
                reverse    = true;

                /* Make sure these variables are reset to 0. */
                sign       = 0; /*source sign bit value         */
                is_max_neg = 0; /*source is maximal negative value*/
                do_round   = 0; /*whether there is roundup      */
                sfirst     = 0;

                /*
                 * If the source and destination buffers overlap then use a
                 * temporary buffer for the destination.
                 */
                if (direction > 0) {
                    s = sp;
                    d = elmtno < olap ? dbuf : dp;
                }
                else {
                    s = sp;
                    d = elmtno + olap >= nelmts ? dbuf : dp;
                }
#ifndef NDEBUG
                /* I don't quite trust the overlap calculations yet --rpm */
                if (d == dbuf) {
                    assert((dp >= sp && dp < sp + src_p->shared->size) ||
                           (sp >= dp && sp < dp + dst_p->shared->size));
                }
                else {
                    assert((dp < sp && dp + dst_p->shared->size <= sp) ||
                           (sp < dp && sp + src_p->shared->size <= dp));
                }
#endif

                /* Put the data in little endian order so our loops aren't so
                 * complicated.  We'll do all the conversion stuff assuming
                 * little endian and then we'll fix the order at the end.
                 */
                if (H5T_ORDER_BE == src.order) {
                    half_size = src_p->shared->size / 2;
                    for (i = 0; i < half_size; i++) {
                        tmp1                             = s[src_p->shared->size - (i + 1)];
                        s[src_p->shared->size - (i + 1)] = s[i];
                        s[i]                             = tmp1;
                    }
                }

                /* Zero-set all destination bits*/
                H5T__bit_set(d, dst.offset, dst.prec, false);

                /* Copy source into a temporary buffer */
                H5T__bit_copy(int_buf, (size_t)0, s, src.offset, src.prec);

                /* Find the sign bit value of the source */
                if (H5T_SGN_2 == src.u.i.sign)
                    sign = (size_t)H5T__bit_get_d(int_buf, src.prec - 1, (size_t)1);

                /* What is the bit position(starting from 0 as first one) for the most significant
                 * bit(MSB) of S which is set?
                 */
                if (H5T_SGN_2 == src.u.i.sign) {
                    sfirst = H5T__bit_find(int_buf, (size_t)0, src.prec - 1, H5T_BIT_MSB, true);
                    if (sign && sfirst < 0)
                        /* The case 0x80...00, which is negative with maximal value */
                        is_max_neg = 1;
                }
                else if (H5T_SGN_NONE == src.u.i.sign)
                    sfirst = H5T__bit_find(int_buf, (size_t)0, src.prec, H5T_BIT_MSB, true);

                /* Handle special cases here.  Integer is zero */
                if (!sign && sfirst < 0)
                    goto padding;

                /* Convert source integer if it's negative */
                if (H5T_SGN_2 == src.u.i.sign && sign) {
                    if (!is_max_neg) {
                        /* Equivalent to ~(i - 1) */
                        H5T__bit_dec(int_buf, (size_t)0, buf_size * 8);
                        H5T__bit_neg(int_buf, (size_t)0, buf_size * 8);
                        sfirst = H5T__bit_find(int_buf, (size_t)0, src.prec - 1, H5T_BIT_MSB, true);
                    }
                    else {
                        /* If it's maximal negative number 0x80...000, treat it as if it overflowed
                         * (create a carry) to help conversion.  i.e. a character type number 0x80
                         * is treated as 0x100.
                         */
                        sfirst     = (ssize_t)(src.prec - 1);
                        is_max_neg = 0;
                    }
                    if (sfirst < 0)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "zero bit not found");

                    /* Sign bit has been negated if bit vector isn't 0x80...00.  Set all bits in front of
                     * sign bit to 0 in the temporary buffer because they're all negated from the previous
                     * step.
                     */
                    H5T__bit_set(int_buf, src.prec, (buf_size * 8) - src.prec, 0);

                    /* Set sign bit in destination */
                    H5T__bit_set_d(d, dst.u.f.sign, (size_t)1, (hsize_t)sign);
                } /* end if */

                first = (size_t)sfirst;

                /* Calculate the true destination exponent by adjusting according to
                 * the destination exponent bias.  Implied and non-implied normalization
                 * should be the same.
                 */
                if (H5T_NORM_NONE == dst.u.f.norm || H5T_NORM_IMPLIED == dst.u.f.norm) {
                    expo = first + dst.u.f.ebias;
                }
                else {
                    HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL,
                                "normalization method not implemented yet");
                }

                /* Handle mantissa part here */
                if (H5T_NORM_IMPLIED == dst.u.f.norm) {
                    /* Imply first bit */
                    H5T__bit_set(int_buf, first, (size_t)1, 0);
                }
                else if (H5T_NORM_NONE == dst.u.f.norm) {
                    first++;
                }

                /* Roundup for mantissa */
                if (first > dst.u.f.msize) {
                    /* If the bit sequence is bigger than the mantissa part, there'll be some
                     * precision loss.  Let user's handler deal with the case if it's present
                     */
                    if (conv_ctx->u.conv.cb_struct.func) {
                        H5T__reverse_order(src_rev, s, src_p->shared->size,
                                           src_p->shared->u.atomic.order); /*reverse order first*/
                        except_ret = (conv_ctx->u.conv.cb_struct.func)(
                            H5T_CONV_EXCEPT_PRECISION, conv_ctx->u.conv.src_type_id,
                            conv_ctx->u.conv.dst_type_id, src_rev, d, conv_ctx->u.conv.cb_struct.user_data);
                    }

                    if (except_ret == H5T_CONV_HANDLED) {
                        reverse = false;
                        goto padding;
                    }
                    else if (except_ret == H5T_CONV_ABORT)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL, "can't handle conversion exception");

                    /* If user's exception handler does deal with it, we do it by dropping off the
                     * extra bits at the end and do rounding.  If we have .50...0(decimal) after radix
                     * point, we do roundup when the least significant digit before radix is odd, we do
                     * rounddown if it's even.
                     */

                    /* Check 1st dropoff bit, see if it's set. */
                    if (H5T__bit_get_d(int_buf, ((first - dst.u.f.msize) - 1), (size_t)1)) {
                        /* Check all bits after 1st dropoff bit, see if any of them is set. */
                        if (((first - dst.u.f.msize) - 1) > 0 &&
                            H5T__bit_get_d(int_buf, (size_t)0, ((first - dst.u.f.msize) - 1)))
                            do_round = 1;
                        else { /* The .50...0 case */
                            /* Check if the least significant bit is odd. */
                            if (H5T__bit_get_d(int_buf, (first - dst.u.f.msize), (size_t)1))
                                do_round = 1;
                        }
                    }

                    /* Right shift to drop off extra bits */
                    H5T__bit_shift(int_buf, (ssize_t)(dst.u.f.msize - first), (size_t)0, buf_size * 8);

                    if (do_round) {
                        H5T__bit_inc(int_buf, (size_t)0, buf_size * 8);
                        do_round = 0;

                        /* If integer is like 0x0ff...fff and we need to round up the
                         * last f, we get 0x100...000.  Treat this special case here.
                         */
                        if (H5T__bit_get_d(int_buf, dst.u.f.msize, (size_t)1)) {
                            if (H5T_NORM_IMPLIED == dst.u.f.norm) {
                                /* The bit at this 1's position was impled already, so this
                                 * number should be 0x200...000.  We need to increment the
                                 * exponent in this case.
                                 */
                                expo++;
                            }
                            else if (H5T_NORM_NONE == dst.u.f.norm) {
                                /* Right shift 1 bit to let the carried 1 fit in the mantissa,
                                 * and increment exponent by 1.
                                 */
                                H5T__bit_shift(int_buf, (ssize_t)-1, (size_t)0, buf_size * 8);
                                expo++;
                            }
                        }
                    }
                }
                else {
                    /* The bit sequence can fit mantissa part.  Left shift to fit in from high-order of
                     * bit position. */
                    H5T__bit_shift(int_buf, (ssize_t)(dst.u.f.msize - first), (size_t)0, dst.u.f.msize);
                }

                /* Check if the exponent is too big */
                expo_max = (hsize_t)(pow(2.0, (double)dst.u.f.esize) - 1);

                if (expo > expo_max) { /*overflows*/
                    if (conv_ctx->u.conv.cb_struct
                            .func) { /*user's exception handler.  Reverse back source order*/
                        H5T__reverse_order(src_rev, s, src_p->shared->size,
                                           src_p->shared->u.atomic.order); /*reverse order first*/
                        except_ret = (conv_ctx->u.conv.cb_struct.func)(
                            H5T_CONV_EXCEPT_RANGE_HI, conv_ctx->u.conv.src_type_id,
                            conv_ctx->u.conv.dst_type_id, src_rev, d, conv_ctx->u.conv.cb_struct.user_data);

                        if (except_ret == H5T_CONV_ABORT)
                            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCONVERT, FAIL,
                                        "can't handle conversion exception");
                        else if (except_ret == H5T_CONV_HANDLED) {
                            reverse = false;
                            goto padding;
                        }
                    }

                    if (!conv_ctx->u.conv.cb_struct.func || (except_ret == H5T_CONV_UNHANDLED)) {
                        /*make destination infinity by setting exponent to maximal number and
                         *mantissa to zero.*/
                        expo = expo_max;
                        memset(int_buf, 0, buf_size);
                    }
                }

                if (except_ret == H5T_CONV_UNHANDLED) {
                    /* Set exponent in destination */
                    H5T__bit_set_d(d, dst.u.f.epos, dst.u.f.esize, expo);

                    /* Copy mantissa into destination */
                    H5T__bit_copy(d, dst.u.f.mpos, int_buf, (size_t)0,
                                  (buf_size * 8) > dst.u.f.msize ? dst.u.f.msize : buf_size * 8);
                }

padding:
                /*
                 * Set padding areas in destination.
                 */
                if (dst.offset > 0) {
                    assert(H5T_PAD_ZERO == dst.lsb_pad || H5T_PAD_ONE == dst.lsb_pad);
                    H5T__bit_set(d, (size_t)0, dst.offset, (bool)(H5T_PAD_ONE == dst.lsb_pad));
                }
                if (dst.offset + dst.prec != 8 * dst_p->shared->size) {
                    assert(H5T_PAD_ZERO == dst.msb_pad || H5T_PAD_ONE == dst.msb_pad);
                    H5T__bit_set(d, dst.offset + dst.prec, 8 * dst_p->shared->size - (dst.offset + dst.prec),
                                 (bool)(H5T_PAD_ONE == dst.msb_pad));
                }

                /*
                 * Put the destination in the correct byte order.  See note at
                 * beginning of loop.
                 */
                if (H5T_ORDER_BE == dst.order && reverse) {
                    half_size = dst_p->shared->size / 2;
                    for (i = 0; i < half_size; i++) {
                        uint8_t tmp                      = d[dst_p->shared->size - (i + 1)];
                        d[dst_p->shared->size - (i + 1)] = d[i];
                        d[i]                             = tmp;
                    }
                }
                else if (H5T_ORDER_VAX == dst.order && reverse) {
                    tsize = dst_p->shared->size;
                    assert(0 == tsize % 2);

                    for (i = 0; i < tsize; i += 4) {
                        tmp1 = d[i];
                        tmp2 = d[i + 1];

                        d[i]     = d[(tsize - 2) - i];
                        d[i + 1] = d[(tsize - 1) - i];

                        d[(tsize - 2) - i] = tmp1;
                        d[(tsize - 1) - i] = tmp2;
                    }
                }

                /*
                 * If we had used a temporary buffer for the destination then we
                 * should copy the value to the true destination buffer.
                 */
                if (d == dbuf)
                    H5MM_memcpy(dp, d, dst_p->shared->size);
                if (buf_stride) {
                    sp += direction * (ssize_t)buf_stride;
                    dp += direction * (ssize_t)buf_stride;
                }
                else {
                    sp += direction * (ssize_t)src_p->shared->size;
                    dp += direction * (ssize_t)dst_p->shared->size;
                }

                memset(int_buf, 0, buf_size);
            }

            break;

        default:
            HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "unknown conversion command");
    } /* end switch */

done:
    if (int_buf)
        H5MM_xfree(int_buf);
    if (src_rev)
        H5MM_free(src_rev);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5T__conv_i_f() */

/*-------------------------------------------------------------------------
 * Function:    H5T__reverse_order
 *
 * Purpose:    Internal assisting function to reverse the order of
 *              a sequence of byte when it's big endian or VAX order.
 *              The byte sequence simulates the endian order.
 *
 * Return:      Success:        A pointer to the reversed byte sequence
 *
 *              Failure:        Null
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5T__reverse_order(uint8_t *rev, uint8_t *s, size_t size, H5T_order_t order)
{
    size_t i;

    FUNC_ENTER_PACKAGE_NOERR

    assert(s);
    assert(size);

    if (H5T_ORDER_VAX == order) {
        for (i = 0; i < size; i += 2) {
            rev[i]     = s[(size - 2) - i];
            rev[i + 1] = s[(size - 1) - i];
        }
    }
    else if (H5T_ORDER_BE == order) {
        for (i = 0; i < size; i++)
            rev[size - (i + 1)] = s[i];
    }
    else {
        for (i = 0; i < size; i++)
            rev[i] = s[i];
    }

    FUNC_LEAVE_NOAPI(SUCCEED)
}

/*-------------------------------------------------------------------------
 * Function:    H5T_reclaim
 *
 * Purpose: Frees the buffers allocated for storing variable-length data
 *          in memory. Only frees the VL data in the selection defined in the
 *          dataspace.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_reclaim(const H5T_t *type, H5S_t *space, void *buf)
{
    H5S_sel_iter_op_t     dset_op;          /* Operator for iteration */
    H5T_vlen_alloc_info_t vl_alloc_info;    /* VL allocation info */
    herr_t                ret_value = FAIL; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check args */
    assert(type);
    assert(space);
    assert(buf);

    /* Get the allocation info */
    if (H5CX_get_vlen_alloc_info(&vl_alloc_info) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "unable to retrieve VL allocation info");

    /* Call H5S_select_iterate with args, etc. */
    dset_op.op_type  = H5S_SEL_ITER_OP_LIB;
    dset_op.u.lib_op = H5T_reclaim_cb;

    ret_value = H5S_select_iterate(buf, type, space, &dset_op, &vl_alloc_info);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5T_reclaim() */

/*-------------------------------------------------------------------------
 * Function:    H5T_reclaim_cb
 *
 * Purpose: Iteration callback to reclaim conversion allocated memory for a
 *          buffer element.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_reclaim_cb(void *elem, const H5T_t *dt, unsigned H5_ATTR_UNUSED ndim, const hsize_t H5_ATTR_UNUSED *point,
               void *op_data)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Sanity check */
    assert(elem);
    assert(dt);

    if (dt->shared->type == H5T_REFERENCE) {
        if (H5T__ref_reclaim(elem, dt) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTFREE, FAIL, "can't reclaim ref elements");
    }
    else {
        assert(op_data);

        /* Allow vlen reclaim to recurse into that routine */
        if (H5T__vlen_reclaim(elem, dt, (H5T_vlen_alloc_info_t *)op_data) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTFREE, FAIL, "can't reclaim vlen elements");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5T_reclaim_cb() */
