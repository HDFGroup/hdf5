/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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
 * Purpose: The public header file for the pass-through VOL connector.
 */

#ifndef _H5subfiling_vol_H
#define _H5subfiling_vol_H

/* Public headers needed by this file */
#include "H5VLpublic.h"        /* Virtual Object Layer                 */
#include "H5PLextern.h"        /* External HDF5 plugins                */
#include "H5FDsubfiling.h"	   /* H5FD subfiling                       */

/* Identifier for the pass-through VOL connector */
#define H5VL_SUBFILING  (H5VL_subfiling_register())

/* Characteristics of the pass-through VOL connector */
#define H5VL_SUBFILING_NAME        "subfiling"
#define H5VL_SUBFILING_VALUE       ((H5VL_class_value_t)13031) /* VOL connector ID */
#define H5VL_SUBFILING_VERSION     1

/* Pass-through VOL connector info */
typedef struct H5VL_subfiling_info_t {
    hid_t under_vol_id;         /* VOL ID for under VOL */
    void *under_vol_info;       /* VOL info for under VOL */
} H5VL_subfiling_info_t;


#include <string.h>
#include "mercury/mercury_thread_mutex.h"

#define H5_LIST_HEAD_INITIALIZER(name)  { NULL }

#define H5_LIST_HEAD_INIT(struct_head_name, var_name)   \
    struct struct_head_name var_name = H5_LIST_HEAD_INITIALIZER(var_name)

#define H5_LIST_HEAD_DECL(struct_head_name, struct_entry_name) \
    struct struct_head_name {                                   \
        struct struct_entry_name *head;                         \
    }

#define H5_LIST_HEAD(struct_entry_name)    \
    struct {                                \
        struct struct_entry_name *head;     \
        hg_thread_mutex_t lock;             \
    }

#define H5_LIST_ENTRY(struct_entry_name)   \
    struct {                                \
        struct struct_entry_name *next;     \
        struct struct_entry_name **prev;    \
    }

#define H5_LIST_INIT(head_ptr) do {        \
    (head_ptr)->head = NULL;                \
    hg_thread_mutex_init(&(head_ptr)->lock);\
} while (/*CONSTCOND*/0)

#define H5_LIST_IS_EMPTY(head_ptr)          \
    ((head_ptr)->head == NULL)

#define H5_LIST_FIRST(head_ptr)             \
    ((head_ptr)->head)

#define H5_LIST_GET_FIRST(var, head_ptr)             \
    (var = (head_ptr)->head)

#define H5_LIST_NEXT(entry_ptr, entry_field_name)   \
    ((entry_ptr)->entry_field_name.next)

#define H5_LIST_INSERT_HEAD(head_ptr, entry_ptr, entry_field_name) do {     \
    if (((entry_ptr)->entry_field_name.next = (head_ptr)->head) != NULL)    \
        (head_ptr)->head->entry_field_name.prev =                           \
            &(entry_ptr)->entry_field_name.next;                            \
    (head_ptr)->head = (entry_ptr);                                         \
    (entry_ptr)->entry_field_name.prev = &(head_ptr)->head;                 \
} while (/*CONSTCOND*/0)

/* TODO would be nice to not have any condition */
#define H5_LIST_REMOVE(entry_ptr, entry_field_name) do {                      \
    if ((entry_ptr)->entry_field_name.next != NULL)                           \
        (entry_ptr)->entry_field_name.next->entry_field_name.prev =           \
            (entry_ptr)->entry_field_name.prev;                               \
    *(entry_ptr)->entry_field_name.prev = (entry_ptr)->entry_field_name.next; \
} while (/*CONSTCOND*/0)

#define H5_LIST_FOREACH(var, head_ptr, entry_field_name)    \
    for ((var) = ((head_ptr)->head);                        \
        (var);                                              \
        (var) = ((var)->entry_field_name.next))


#ifdef __cplusplus
extern "C" {
#endif

H5_DLL hid_t H5VL_subfiling_register(void);

#ifdef __cplusplus
}
#endif

#endif /* _H5subfiling_vol_H */

