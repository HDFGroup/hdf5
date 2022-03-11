/*
 * Copyright (C) 2013-2020 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

#ifndef MERCURY_MACROS_H
#define MERCURY_MACROS_H

#include "mercury.h"
#include "mercury_bulk.h"
#include "mercury_proc.h"
#include "mercury_proc_bulk.h"

#ifdef HG_HAS_BOOST
#include <boost/preprocessor.hpp>

/**
 * The purpose of these macros is to facilitate generation of encoding/decoding
 * procs as well as the registration of new routines to an existing HG class.
 * HG_XXX macros are private macros / MERCURY_XXX are public macros.
 * Macros defined in this file are:
 *   - MERCURY_REGISTER
 *   - MERCURY_GEN_PROC
 *   - MERCURY_GEN_STRUCT_PROC
 */

/****************/
/* Local Macros */
/****************/

/* Get type / name */
#define HG_GEN_GET_TYPE(field) BOOST_PP_SEQ_HEAD(field)
#define HG_GEN_GET_NAME(field) BOOST_PP_SEQ_CAT(BOOST_PP_SEQ_TAIL(field))

/* Get struct field */
#define HG_GEN_STRUCT_FIELD(r, data, param) HG_GEN_GET_TYPE(param) HG_GEN_GET_NAME(param);

/* Generate structure */
#define HG_GEN_STRUCT(struct_type_name, fields)                                                              \
    typedef struct {                                                                                         \
        BOOST_PP_SEQ_FOR_EACH(HG_GEN_STRUCT_FIELD, , fields)                                                 \
                                                                                                             \
    } struct_type_name;

/* Generate proc for struct field */
#define HG_GEN_PROC(r, struct_name, field)                                                                   \
    ret = BOOST_PP_CAT(hg_proc_, HG_GEN_GET_TYPE(field)(proc, &struct_name->HG_GEN_GET_NAME(field)));        \
    if (unlikely(ret != HG_SUCCESS)) {                                                                       \
        return ret;                                                                                          \
    }

/* Generate proc for struct */
#define HG_GEN_STRUCT_PROC(struct_type_name, fields)                                                         \
    static HG_INLINE hg_return_t BOOST_PP_CAT(hg_proc_, struct_type_name)(hg_proc_t proc, void *data)        \
    {                                                                                                        \
        hg_return_t       ret         = HG_SUCCESS;                                                          \
        struct_type_name *struct_data = (struct_type_name *)data;                                            \
                                                                                                             \
        BOOST_PP_SEQ_FOR_EACH(HG_GEN_PROC, struct_data, fields)                                              \
                                                                                                             \
        return ret;                                                                                          \
    }

/*****************/
/* Public Macros */
/*****************/

/* Register func_name */
#define MERCURY_REGISTER(hg_class, func_name, in_struct_type_name, out_struct_type_name, rpc_cb)             \
    HG_Register_name(hg_class, func_name, BOOST_PP_CAT(hg_proc_, in_struct_type_name),                       \
                     BOOST_PP_CAT(hg_proc_, out_struct_type_name), rpc_cb)

/* Generate struct and corresponding struct proc */
#define MERCURY_GEN_PROC(struct_type_name, fields)                                                           \
    HG_GEN_STRUCT(struct_type_name, fields)                                                                  \
    HG_GEN_STRUCT_PROC(struct_type_name, fields)

/* In the case of user defined structures / MERCURY_GEN_STRUCT_PROC can be
 * used to generate the corresponding proc routine.
 * E.g., if user defined struct:
 *   typedef struct {
 *     uint64_t cookie;
 *   } bla_handle_t;
 * MERCURY_GEN_STRUCT_PROC( struct_type_name, field sequence ):
 *   MERCURY_GEN_STRUCT_PROC( bla_handle_t, ((uint64_t)(cookie)) )
 */
#define MERCURY_GEN_STRUCT_PROC(struct_type_name, fields) HG_GEN_STRUCT_PROC(struct_type_name, fields)

#else /* HG_HAS_BOOST */

/* Register func_name */
#define MERCURY_REGISTER(hg_class, func_name, in_struct_type_name, out_struct_type_name, rpc_cb)             \
    HG_Register_name(hg_class, func_name, hg_proc_##in_struct_type_name, hg_proc_##out_struct_type_name,     \
                     rpc_cb)

#endif /* HG_HAS_BOOST */

/* If no input args or output args, a void type can be
 * passed to MERCURY_REGISTER
 */
#define hg_proc_void NULL

#endif /* MERCURY_MACROS_H */
