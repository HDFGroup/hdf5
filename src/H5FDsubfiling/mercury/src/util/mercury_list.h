/**
 * Copyright (c) 2013-2021 UChicago Argonne, LLC and The HDF Group.
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

/* Code below is derived from sys/queue.h which follows the below notice:
 *
 * Copyright (c) 1991, 1993
 *  The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *  @(#)queue.h 8.5 (Berkeley) 8/20/94
 */

#ifndef MERCURY_LIST_H
#define MERCURY_LIST_H

#define HG_LIST_HEAD_INITIALIZER(name)                                                                       \
    {                                                                                                        \
        NULL                                                                                                 \
    }

#define HG_LIST_HEAD_INIT(struct_head_name, var_name)                                                        \
    struct struct_head_name var_name = HG_LIST_HEAD_INITIALIZER(var_name)

#define HG_LIST_HEAD_DECL(struct_head_name, struct_entry_name)                                               \
    struct struct_head_name {                                                                                \
        struct struct_entry_name *head;                                                                      \
    }

#define HG_LIST_HEAD(struct_entry_name)                                                                      \
    struct {                                                                                                 \
        struct struct_entry_name *head;                                                                      \
    }

#define HG_LIST_ENTRY(struct_entry_name)                                                                     \
    struct {                                                                                                 \
        struct struct_entry_name  *next;                                                                     \
        struct struct_entry_name **prev;                                                                     \
    }

#define HG_LIST_INIT(head_ptr)                                                                               \
    do {                                                                                                     \
        (head_ptr)->head = NULL;                                                                             \
    } while (/*CONSTCOND*/ 0)

#define HG_LIST_IS_EMPTY(head_ptr) ((head_ptr)->head == NULL)

#define HG_LIST_FIRST(head_ptr) ((head_ptr)->head)

#define HG_LIST_NEXT(entry_ptr, entry_field_name) ((entry_ptr)->entry_field_name.next)

#define HG_LIST_INSERT_AFTER(list_entry_ptr, entry_ptr, entry_field_name)                                    \
    do {                                                                                                     \
        if (((entry_ptr)->entry_field_name.next = (list_entry_ptr)->entry_field_name.next) != NULL)          \
            (list_entry_ptr)->entry_field_name.next->entry_field_name.prev =                                 \
                &(entry_ptr)->entry_field_name.next;                                                         \
        (list_entry_ptr)->entry_field_name.next = (entry_ptr);                                               \
        (entry_ptr)->entry_field_name.prev      = &(list_entry_ptr)->entry_field_name.next;                  \
    } while (/*CONSTCOND*/ 0)

#define HG_LIST_INSERT_BEFORE(list_entry_ptr, entry_ptr, entry_field_name)                                   \
    do {                                                                                                     \
        (entry_ptr)->entry_field_name.prev       = (list_entry_ptr)->entry_field_name.prev;                  \
        (entry_ptr)->entry_field_name.next       = (list_entry_ptr);                                         \
        *(list_entry_ptr)->entry_field_name.prev = (entry_ptr);                                              \
        (list_entry_ptr)->entry_field_name.prev  = &(entry_ptr)->entry_field_name.next;                      \
    } while (/*CONSTCOND*/ 0)

#define HG_LIST_INSERT_HEAD(head_ptr, entry_ptr, entry_field_name)                                           \
    do {                                                                                                     \
        if (((entry_ptr)->entry_field_name.next = (head_ptr)->head) != NULL)                                 \
            (head_ptr)->head->entry_field_name.prev = &(entry_ptr)->entry_field_name.next;                   \
        (head_ptr)->head                   = (entry_ptr);                                                    \
        (entry_ptr)->entry_field_name.prev = &(head_ptr)->head;                                              \
    } while (/*CONSTCOND*/ 0)

/* TODO would be nice to not have any condition */
#define HG_LIST_REMOVE(entry_ptr, entry_field_name)                                                          \
    do {                                                                                                     \
        if ((entry_ptr)->entry_field_name.next != NULL)                                                      \
            (entry_ptr)->entry_field_name.next->entry_field_name.prev = (entry_ptr)->entry_field_name.prev;  \
        *(entry_ptr)->entry_field_name.prev = (entry_ptr)->entry_field_name.next;                            \
    } while (/*CONSTCOND*/ 0)

#define HG_LIST_FOREACH(var, head_ptr, entry_field_name)                                                     \
    for ((var) = ((head_ptr)->head); (var); (var) = ((var)->entry_field_name.next))

#endif /* MERCURY_LIST_H */
