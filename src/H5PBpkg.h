/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#if !(defined H5PB_FRIEND || defined H5PB_MODULE)
#error "Do not include this file outside the H5PB package!"
#endif

#ifndef H5PBpkg_H
#define H5PBpkg_H

/* Get package's private header */
#include "H5PBprivate.h"

/*
 * File:        H5PBpkg.h
 *
 * Purpose:     This file contains declarations which are normally visible
 *              only within the H5PB package.
 *
 *              Source files outside the H5PB package should include
 *              H5PBprivate.h instead.
 *
 *              Programmer: John Mainzer -- 10/07/18
 */

/**************************/
/* Package Private Macros */
/**************************/

/* page buffer configuration settings */

#define H5PB__H5PB_ENTRY_T_MAGIC        0x02030405
#define H5PB__DO_SANITY_CHECKS          TRUE
#define H5PB__COLLECT_PAGE_BUFFER_STATS TRUE

/****************************************************************************
 *
 * We maintain doubly linked lists of instances of H5PB_entry_t for a
 * variety of reasons -- LRU list, tick list, and the delayed write list
 * at present.  The following macros support linking and unlinking
 * instances of H5PB_entry_t by both their regular and tick list next
 * and previous pointers.  Note that the tick list and the delayed write
 * list are only used in the context of VFD SWMR
 *
 * The size and length fields are also maintained.
 *
 * Note that the relevant pair of prev and next pointers are presumed to be
 * NULL on entry in the insertion macros.
 *
 * Finally, observe that the sanity checking macros evaluate to the empty
 * string when H5PB__DO_SANITY_CHECKS is FALSE.  They also contain calls
 * to the HGOTO_ERROR macro, which may not be appropriate in all cases.
 * If so, we will need versions of the insertion and deletion macros which
 * do not reference the sanity checking macros.
 *                                              JRM -- 10/07/18
 *
 ****************************************************************************/

#if H5PB__DO_SANITY_CHECKS

#define H5PB__DLL_PRE_REMOVE_SC(entry_ptr, head_ptr, tail_ptr, len, Size, fv)                                \
    if (((head_ptr) == NULL) || ((tail_ptr) == NULL) || ((entry_ptr) == NULL) || ((len) <= 0) ||             \
        ((Size) < (int64_t)((entry_ptr)->size)) ||                                                           \
        (((Size) == (int64_t)((entry_ptr)->size)) && (!((len) == 1))) ||                                     \
        (((entry_ptr)->prev == NULL) && ((head_ptr) != (entry_ptr))) ||                                      \
        (((entry_ptr)->next == NULL) && ((tail_ptr) != (entry_ptr))) ||                                      \
        (((len) == 1) &&                                                                                     \
         (!(((head_ptr) == (entry_ptr)) && ((tail_ptr) == (entry_ptr)) && ((entry_ptr)->next == NULL) &&     \
            ((entry_ptr)->prev == NULL) && ((Size) == (int64_t)((entry_ptr)->size)))))) {                    \
        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, (fv), "DLL pre remove SC failed")                               \
    }

#define H5PB__DLL_SC(head_ptr, tail_ptr, len, Size, fv)                                                      \
    if (((((head_ptr) == NULL) || ((tail_ptr) == NULL)) && ((head_ptr) != (tail_ptr))) || ((len) < 0) ||     \
        ((Size) < 0) ||                                                                                      \
        (((len) == 1) &&                                                                                     \
         (((head_ptr) != (tail_ptr)) || ((head_ptr) == NULL) || ((head_ptr)->size != (size_t)(Size)))) ||    \
        (((len) >= 1) && (((head_ptr) == NULL) || ((head_ptr)->prev != NULL) || ((tail_ptr) == NULL) ||      \
                          ((tail_ptr)->next != NULL)))) {                                                    \
        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, (fv), "DLL sanity check failed")                                \
    }

#define H5PB__DLL_PRE_INSERT_SC(entry_ptr, head_ptr, tail_ptr, len, Size, fv)                                \
    if (((entry_ptr) == NULL) || ((entry_ptr)->next != NULL) || ((entry_ptr)->prev != NULL) ||               \
        ((((head_ptr) == NULL) || ((tail_ptr) == NULL)) && ((head_ptr) != (tail_ptr))) ||                    \
        (((len) == 1) &&                                                                                     \
         (((head_ptr) != (tail_ptr)) || ((head_ptr) == NULL) || ((head_ptr)->size != (size_t)(Size)))) ||    \
        (((len) >= 1) && (((head_ptr) == NULL) || ((head_ptr)->prev != NULL) || ((tail_ptr) == NULL) ||      \
                          ((tail_ptr)->next != NULL)))) {                                                    \
        HDassert(FALSE);                                                                                     \
        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, (fv), "DLL pre insert SC failed")                               \
    }

#else /* H5PB__DO_SANITY_CHECKS */

#define H5PB__DLL_PRE_REMOVE_SC(entry_ptr, head_ptr, tail_ptr, len, Size, fv)
#define H5PB__DLL_SC(head_ptr, tail_ptr, len, Size, fv)
#define H5PB__DLL_PRE_INSERT_SC(entry_ptr, head_ptr, tail_ptr, len, Size, fv)

#endif /* H5PB__DO_SANITY_CHECKS */

#define H5PB__DLL_APPEND(entry_ptr, head_ptr, tail_ptr, len, Size, fail_val)                                 \
    {                                                                                                        \
        H5PB__DLL_PRE_INSERT_SC(entry_ptr, head_ptr, tail_ptr, len, Size, fail_val)                          \
        if ((head_ptr) == NULL) {                                                                            \
            (head_ptr) = (entry_ptr);                                                                        \
            (tail_ptr) = (entry_ptr);                                                                        \
        }                                                                                                    \
        else {                                                                                               \
            (tail_ptr)->next  = (entry_ptr);                                                                 \
            (entry_ptr)->prev = (tail_ptr);                                                                  \
            (tail_ptr)        = (entry_ptr);                                                                 \
        }                                                                                                    \
        (len)++;                                                                                             \
        (Size) += (int64_t)((entry_ptr)->size);                                                              \
    } /* H5PB__DLL_APPEND() */

#define H5PB__DLL_PREPEND(entry_ptr, head_ptr, tail_ptr, len, Size, fail_val)                                \
    {                                                                                                        \
        H5PB__DLL_PRE_INSERT_SC(entry_ptr, head_ptr, tail_ptr, len, Size, fail_val)                          \
        if ((head_ptr) == NULL) {                                                                            \
            (head_ptr) = (entry_ptr);                                                                        \
            (tail_ptr) = (entry_ptr);                                                                        \
        }                                                                                                    \
        else {                                                                                               \
            (head_ptr)->prev  = (entry_ptr);                                                                 \
            (entry_ptr)->next = (head_ptr);                                                                  \
            (head_ptr)        = (entry_ptr);                                                                 \
        }                                                                                                    \
        (len)++;                                                                                             \
        (Size) += (int64_t)((entry_ptr)->size);                                                              \
    } /* H5PB__DLL_PREPEND() */

#define H5PB__DLL_INSERT_BEFORE(entry_ptr, suc_ptr, head_ptr, tail_ptr, len, Size, fail_val)                 \
    {                                                                                                        \
        HDassert(((suc_ptr) == NULL) || ((suc_ptr)->magic == H5PB__H5PB_ENTRY_T_MAGIC));                     \
                                                                                                             \
        if (suc_ptr == NULL)                                                                                 \
            /* list empty or no successor -- append */                                                       \
            H5PB__DLL_APPEND(entry_ptr, head_ptr, tail_ptr, len, Size, fail_val)                             \
                                                                                                             \
        else if (suc_ptr->prev == NULL)                                                                      \
            /* successor at head of list -- prepend */                                                       \
            H5PB__DLL_PREPEND(entry_ptr, head_ptr, tail_ptr, len, Size, fail_val)                            \
                                                                                                             \
        else /* sucessor in body of list -- insert before it */                                              \
        {                                                                                                    \
            H5PB__DLL_PRE_INSERT_SC(entry_ptr, head_ptr, tail_ptr, len, Size, fail_val)                      \
            HDassert(suc_ptr->prev->magic == H5PB__H5PB_ENTRY_T_MAGIC);                                      \
            HDassert(suc_ptr->prev->next == suc_ptr);                                                        \
            entry_ptr->prev       = suc_ptr->prev;                                                           \
            entry_ptr->prev->next = entry_ptr;                                                               \
            entry_ptr->next       = suc_ptr;                                                                 \
            suc_ptr->prev         = entry_ptr;                                                               \
            (len)++;                                                                                         \
            (Size) += (int64_t)((entry_ptr)->size);                                                          \
        }                                                                                                    \
    } /* H5PB__DLL_INSERT_BEFORE() */

#define H5PB__DLL_REMOVE(entry_ptr, head_ptr, tail_ptr, len, Size, fail_val)                                 \
    {                                                                                                        \
        H5PB__DLL_PRE_REMOVE_SC(entry_ptr, head_ptr, tail_ptr, len, Size, fail_val)                          \
        {                                                                                                    \
            if ((head_ptr) == (entry_ptr)) {                                                                 \
                (head_ptr) = (entry_ptr)->next;                                                              \
                if ((head_ptr) != NULL)                                                                      \
                    (head_ptr)->prev = NULL;                                                                 \
            }                                                                                                \
            else                                                                                             \
                (entry_ptr)->prev->next = (entry_ptr)->next;                                                 \
            if ((tail_ptr) == (entry_ptr)) {                                                                 \
                (tail_ptr) = (entry_ptr)->prev;                                                              \
                if ((tail_ptr) != NULL)                                                                      \
                    (tail_ptr)->next = NULL;                                                                 \
            }                                                                                                \
            else                                                                                             \
                (entry_ptr)->next->prev = (entry_ptr)->prev;                                                 \
            entry_ptr->next = NULL;                                                                          \
            entry_ptr->prev = NULL;                                                                          \
            (len)--;                                                                                         \
            (Size) -= (int64_t)((entry_ptr)->size);                                                          \
        }                                                                                                    \
    } /* H5PB__DLL_REMOVE() */

#if H5PB__DO_SANITY_CHECKS

#define H5PB__IL_DLL_PRE_REMOVE_SC(entry_ptr, hd_ptr, tail_ptr, len, Size, fv)                               \
    if (((hd_ptr) == NULL) || ((tail_ptr) == NULL) || ((entry_ptr) == NULL) || ((len) <= 0) ||               \
        ((Size) < (int64_t)((entry_ptr)->size)) ||                                                           \
        (((Size) == (int64_t)((entry_ptr)->size)) && (!((len) == 1))) ||                                     \
        (((entry_ptr)->il_prev == NULL) && ((hd_ptr) != (entry_ptr))) ||                                     \
        (((entry_ptr)->il_next == NULL) && ((tail_ptr) != (entry_ptr))) ||                                   \
        (((len) == 1) &&                                                                                     \
         (!(((hd_ptr) == (entry_ptr)) && ((tail_ptr) == (entry_ptr)) && ((entry_ptr)->il_next == NULL) &&    \
            ((entry_ptr)->il_prev == NULL) && ((Size) == (int64_t)((entry_ptr)->size)))))) {                 \
        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, (fv), "il DLL pre remove SC failed")                            \
    }

#define H5PB__IL_DLL_PRE_INSERT_SC(entry_ptr, hd_ptr, tail_ptr, len, Size, fv)                               \
    if (((entry_ptr) == NULL) || ((entry_ptr)->il_next != NULL) || ((entry_ptr)->il_prev != NULL) ||         \
        ((((hd_ptr) == NULL) || ((tail_ptr) == NULL)) && ((hd_ptr) != (tail_ptr))) ||                        \
        (((len) == 1) && (((hd_ptr) != (tail_ptr)) || ((Size) <= 0) || ((hd_ptr) == NULL) ||                 \
                          ((int64_t)((hd_ptr)->size) != (Size)))) ||                                         \
        (((len) >= 1) && (((hd_ptr) == NULL) || ((hd_ptr)->il_prev != NULL) || ((tail_ptr) == NULL) ||       \
                          ((tail_ptr)->il_next != NULL)))) {                                                 \
        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, (fv), "IL DLL pre insert SC failed")                            \
    }

#define H5PB__IL_DLL_SC(head_ptr, tail_ptr, len, Size, fv)                                                   \
    if (((((head_ptr) == NULL) || ((tail_ptr) == NULL)) && ((head_ptr) != (tail_ptr))) ||                    \
        (((len) == 1) &&                                                                                     \
         (((head_ptr) != (tail_ptr)) || ((head_ptr) == NULL) || ((int64_t)((head_ptr)->size) != (Size)))) || \
        (((len) >= 1) && (((head_ptr) == NULL) || ((head_ptr)->il_prev != NULL) || ((tail_ptr) == NULL) ||   \
                          ((tail_ptr)->il_next != NULL)))) {                                                 \
        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, (fv), "IL DLL sanity check failed")                             \
    }

#else /* H5PB__DO_SANITY_CHECKS */

#define H5PB__IL_DLL_PRE_REMOVE_SC(entry_ptr, hd_ptr, tail_ptr, len, Size, fv)
#define H5PB__IL_DLL_PRE_INSERT_SC(entry_ptr, hd_ptr, tail_ptr, len, Size, fv)
#define H5PB__IL_DLL_SC(head_ptr, tail_ptr, len, Size, fv)

#endif /* H5PB__DO_SANITY_CHECKS */

#define H5PB__IL_DLL_APPEND(entry_ptr, head_ptr, tail_ptr, len, Size, fail_val)                              \
    {                                                                                                        \
        H5PB__IL_DLL_PRE_INSERT_SC(entry_ptr, head_ptr, tail_ptr, len, Size, fail_val)                       \
        if ((head_ptr) == NULL) {                                                                            \
            (head_ptr) = (entry_ptr);                                                                        \
            (tail_ptr) = (entry_ptr);                                                                        \
        }                                                                                                    \
        else {                                                                                               \
            (tail_ptr)->il_next  = (entry_ptr);                                                              \
            (entry_ptr)->il_prev = (tail_ptr);                                                               \
            (tail_ptr)           = (entry_ptr);                                                              \
        }                                                                                                    \
        (len)++;                                                                                             \
        (Size) += (int64_t)((entry_ptr)->size);                                                              \
        H5PB__IL_DLL_SC(head_ptr, tail_ptr, len, Size, fail_val)                                             \
    } /* H5PB__IL_DLL_APPEND() */

#define H5PB__IL_DLL_PREPEND(entry_ptr, head_ptr, tail_ptr, len, Size, fail_val)                             \
    {                                                                                                        \
        H5PB__DLL_PRE_INSERT_SC(entry_ptr, head_ptr, tail_ptr, len, Size, fail_val)                          \
        if ((head_ptr) == NULL) {                                                                            \
            (head_ptr) = (entry_ptr);                                                                        \
            (tail_ptr) = (entry_ptr);                                                                        \
        }                                                                                                    \
        else {                                                                                               \
            (head_ptr)->il_prev  = (entry_ptr);                                                              \
            (entry_ptr)->il_next = (head_ptr);                                                               \
            (head_ptr)           = (entry_ptr);                                                              \
        }                                                                                                    \
        (len)++;                                                                                             \
        (Size) += (int64_t)((entry_ptr)->size);                                                              \
    } /* H5PB__DLL_PREPEND() */

#define H5PB__IL_DLL_INSERT_BEFORE(entry_ptr, suc_ptr, head_ptr, tail_ptr, len, Size, fail_val)              \
    {                                                                                                        \
        HDassert(((suc_ptr) == NULL) || ((suc_ptr)->magic == H5PB__H5PB_ENTRY_T_MAGIC));                     \
                                                                                                             \
        if (suc_ptr == NULL)                                                                                 \
            /* list empty or no successor -- append */                                                       \
            H5PB__IL_DLL_APPEND(entry_ptr, head_ptr, tail_ptr, len, Size, fail_val)                          \
                                                                                                             \
        else if (suc_ptr->il_prev == NULL)                                                                   \
            /* successor at head of list -- prepend */                                                       \
            H5PB__IL_DLL_PREPEND(entry_ptr, head_ptr, tail_ptr, len, Size, fail_val)                         \
                                                                                                             \
        else /* sucessor in body of list -- insert before it */                                              \
        {                                                                                                    \
            H5PB__IL_DLL_PRE_INSERT_SC(entry_ptr, head_ptr, tail_ptr, len, Size, fail_val)                   \
            HDassert(suc_ptr->il_prev->magic == H5PB__H5PB_ENTRY_T_MAGIC);                                   \
            HDassert(suc_ptr->il_prev->il_next == suc_ptr);                                                  \
            entry_ptr->il_prev          = suc_ptr->il_prev;                                                  \
            entry_ptr->il_prev->il_next = entry_ptr;                                                         \
            entry_ptr->il_next          = suc_ptr;                                                           \
            suc_ptr->il_prev            = entry_ptr;                                                         \
            (len)++;                                                                                         \
            (Size) += (int64_t)((entry_ptr)->size);                                                          \
        }                                                                                                    \
    } /* H5PB__DLL_INSERT_BEFORE() */

#define H5PB__IL_DLL_REMOVE(entry_ptr, head_ptr, tail_ptr, len, Size, fv)                                    \
    {                                                                                                        \
        H5PB__IL_DLL_PRE_REMOVE_SC(entry_ptr, head_ptr, tail_ptr, len, Size, fv)                             \
        {                                                                                                    \
            if ((head_ptr) == (entry_ptr)) {                                                                 \
                (head_ptr) = (entry_ptr)->il_next;                                                           \
                if ((head_ptr) != NULL)                                                                      \
                    (head_ptr)->il_prev = NULL;                                                              \
            }                                                                                                \
            else                                                                                             \
                (entry_ptr)->il_prev->il_next = (entry_ptr)->il_next;                                        \
            if ((tail_ptr) == (entry_ptr)) {                                                                 \
                (tail_ptr) = (entry_ptr)->il_prev;                                                           \
                if ((tail_ptr) != NULL)                                                                      \
                    (tail_ptr)->il_next = NULL;                                                              \
            }                                                                                                \
            else                                                                                             \
                (entry_ptr)->il_next->il_prev = (entry_ptr)->il_prev;                                        \
            entry_ptr->il_next = NULL;                                                                       \
            entry_ptr->il_prev = NULL;                                                                       \
            (len)--;                                                                                         \
            (Size) -= (int64_t)((entry_ptr)->size);                                                          \
        }                                                                                                    \
        H5PB__IL_DLL_SC(head_ptr, tail_ptr, len, Size, fv)                                                   \
    } /* H5PB__IL_DLL_REMOVE() */

#if H5PB__DO_SANITY_CHECKS

#define H5PB__TL_DLL_PRE_REMOVE_SC(entry_ptr, hd_ptr, tail_ptr, len, Size, fv)                               \
    if (((hd_ptr) == NULL) || ((tail_ptr) == NULL) || ((entry_ptr) == NULL) || ((len) <= 0) ||               \
        ((Size) < (int64_t)((entry_ptr)->size)) ||                                                           \
        (((Size) == (int64_t)((entry_ptr)->size)) && (!((len) == 1))) ||                                     \
        (((entry_ptr)->tl_prev == NULL) && ((hd_ptr) != (entry_ptr))) ||                                     \
        (((entry_ptr)->tl_next == NULL) && ((tail_ptr) != (entry_ptr))) ||                                   \
        (((len) == 1) &&                                                                                     \
         (!(((hd_ptr) == (entry_ptr)) && ((tail_ptr) == (entry_ptr)) && ((entry_ptr)->tl_next == NULL) &&    \
            ((entry_ptr)->tl_prev == NULL) && ((Size) == (int64_t)((entry_ptr)->size)))))) {                 \
        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, (fv), "TL DLL pre remove SC failed")                            \
    }

#define H5PB__TL_DLL_SC(head_ptr, tail_ptr, len, Size, fv)                                                   \
    if (((((head_ptr) == NULL) || ((tail_ptr) == NULL)) && ((head_ptr) != (tail_ptr))) || ((len) < 0) ||     \
        ((Size) < 0) ||                                                                                      \
        (((len) == 1) && (((head_ptr) != (tail_ptr)) || ((Size) <= 0) || ((head_ptr) == NULL) ||             \
                          ((head_ptr)->size != (Size)))) ||                                                  \
        (((len) >= 1) && (((head_ptr) == NULL) || ((head_ptr)->tl_prev != NULL) || ((tail_ptr) == NULL) ||   \
                          ((tail_ptr)->tl_next != NULL)))) {                                                 \
        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, (fv), "TL DLL sanity check failed")                             \
    }

#define H5PB__TL_DLL_PRE_INSERT_SC(entry_ptr, hd_ptr, tail_ptr, len, Size, fv)                               \
    if (((entry_ptr) == NULL) || ((entry_ptr)->tl_next != NULL) || ((entry_ptr)->tl_prev != NULL) ||         \
        ((((hd_ptr) == NULL) || ((tail_ptr) == NULL)) && ((hd_ptr) != (tail_ptr))) ||                        \
        (((len) == 1) && (((hd_ptr) != (tail_ptr)) || ((Size) <= 0) || ((hd_ptr) == NULL) ||                 \
                          ((int64_t)((hd_ptr)->size) != (Size)))) ||                                         \
        (((len) >= 1) && (((hd_ptr) == NULL) || ((hd_ptr)->tl_prev != NULL) || ((tail_ptr) == NULL) ||       \
                          ((tail_ptr)->tl_next != NULL)))) {                                                 \
        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, (fv), "TL DLL pre insert SC failed")                            \
    }

#else /* H5PB__DO_SANITY_CHECKS */

#define H5PB__TL_DLL_PRE_REMOVE_SC(entry_ptr, hd_ptr, tail_ptr, len, Size, fv)
#define H5PB__TL_DLL_SC(head_ptr, tail_ptr, len, Size, fv)
#define H5PB__TL_DLL_PRE_INSERT_SC(entry_ptr, hd_ptr, tail_ptr, len, Size, fv)

#endif /* H5PB__DO_SANITY_CHECKS */

#define H5PB__TL_DLL_APPEND(entry_ptr, head_ptr, tail_ptr, len, Size, fail_val)                              \
    {                                                                                                        \
        H5PB__TL_DLL_PRE_INSERT_SC(entry_ptr, head_ptr, tail_ptr, len, Size, fail_val)                       \
        if ((head_ptr) == NULL) {                                                                            \
            (head_ptr) = (entry_ptr);                                                                        \
            (tail_ptr) = (entry_ptr);                                                                        \
        }                                                                                                    \
        else {                                                                                               \
            (tail_ptr)->tl_next  = (entry_ptr);                                                              \
            (entry_ptr)->tl_prev = (tail_ptr);                                                               \
            (tail_ptr)           = (entry_ptr);                                                              \
        }                                                                                                    \
        (len)++;                                                                                             \
        (Size) += entry_ptr->size;                                                                           \
    } /* H5PB__AUX_DLL_APPEND() */

#define H5PB__TL_DLL_PREPEND(entry_ptr, head_ptr, tail_ptr, len, Size, fv)                                   \
    {                                                                                                        \
        H5PB__TL_DLL_PRE_INSERT_SC(entry_ptr, head_ptr, tail_ptr, len, Size, fv)                             \
        if ((head_ptr) == NULL) {                                                                            \
            (head_ptr) = (entry_ptr);                                                                        \
            (tail_ptr) = (entry_ptr);                                                                        \
        }                                                                                                    \
        else {                                                                                               \
            (head_ptr)->tl_prev  = (entry_ptr);                                                              \
            (entry_ptr)->tl_next = (head_ptr);                                                               \
            (head_ptr)           = (entry_ptr);                                                              \
        }                                                                                                    \
        (len)++;                                                                                             \
        (Size) += (int64_t)(entry_ptr->size);                                                                \
    } /* H5PB__TL_DLL_PREPEND() */

#define H5PB__TL_DLL_REMOVE(entry_ptr, head_ptr, tail_ptr, len, Size, fv)                                    \
    {                                                                                                        \
        H5PB__TL_DLL_PRE_REMOVE_SC(entry_ptr, head_ptr, tail_ptr, len, Size, fv)                             \
        {                                                                                                    \
            if ((head_ptr) == (entry_ptr)) {                                                                 \
                (head_ptr) = (entry_ptr)->tl_next;                                                           \
                if ((head_ptr) != NULL)                                                                      \
                    (head_ptr)->tl_prev = NULL;                                                              \
            }                                                                                                \
            else                                                                                             \
                (entry_ptr)->tl_prev->tl_next = (entry_ptr)->tl_next;                                        \
            if ((tail_ptr) == (entry_ptr)) {                                                                 \
                (tail_ptr) = (entry_ptr)->tl_prev;                                                           \
                if ((tail_ptr) != NULL)                                                                      \
                    (tail_ptr)->tl_next = NULL;                                                              \
            }                                                                                                \
            else                                                                                             \
                (entry_ptr)->tl_next->tl_prev = (entry_ptr)->tl_prev;                                        \
            entry_ptr->tl_next = NULL;                                                                       \
            entry_ptr->tl_prev = NULL;                                                                       \
            (len)--;                                                                                         \
            (Size) -= (int64_t)(entry_ptr->size);                                                            \
        }                                                                                                    \
    } /* H5PB__TL_DLL_REMOVE() */

/***********************************************************************
 *
 * Stats collection macros
 *
 * The following macros must handle stats collection when this collection
 * is enabled, and evaluate to the empty string when it is not.
 *
 * The sole exception to this rule is
 * H5PB__UPDATE_PB_HIT_RATE_STATS(), which is always active as
 * the page buffer hit rate stats are always collected and available.
 *
 ***********************************************************************/

#if H5PB__COLLECT_PAGE_BUFFER_STATS

#define H5PB__UPDATE_PB_HIT_RATE_STATS(page_buf, hit, is_metadata, is_mpmde)                                 \
    {                                                                                                        \
        int ii;                                                                                              \
                                                                                                             \
        HDassert(page_buf);                                                                                  \
        HDassert((page_buf)->magic == H5PB__H5PB_T_MAGIC);                                                   \
                                                                                                             \
        if (is_metadata) {                                                                                   \
            if (is_mpmde) {                                                                                  \
                ii = H5PB__STATS_MPMDE;                                                                      \
            }                                                                                                \
            else {                                                                                           \
                ii = H5PB__STATS_MD;                                                                         \
            }                                                                                                \
        }                                                                                                    \
        else {                                                                                               \
            ii = H5PB__STATS_RD;                                                                             \
        }                                                                                                    \
        if (hit)                                                                                             \
            ((page_buf)->hits[ii])++;                                                                        \
        else                                                                                                 \
            ((page_buf)->misses[ii])++;                                                                      \
    } /* H5PB__UPDATE_PB_HIT_RATE_STATS */

#define H5PB__UPDATE_HT_SIZE_STATS(page_buf)                                                                 \
    if ((page_buf)->index_len > (page_buf)->max_index_len)                                                   \
        (page_buf)->max_index_len = (page_buf)->index_len;                                                   \
    if ((page_buf)->clean_index_len > (page_buf)->max_clean_index_len)                                       \
        (page_buf)->max_clean_index_len = (page_buf)->clean_index_len;                                       \
    if ((page_buf)->dirty_index_len > (page_buf)->max_dirty_index_len)                                       \
        (page_buf)->max_dirty_index_len = (page_buf)->dirty_index_len;                                       \
    if ((page_buf)->index_size > (page_buf)->max_index_size)                                                 \
        (page_buf)->max_index_size = (page_buf)->index_size;                                                 \
    if ((page_buf)->clean_index_size > (page_buf)->max_clean_index_size)                                     \
        (page_buf)->max_clean_index_size = (page_buf)->clean_index_size;                                     \
    if ((page_buf)->dirty_index_size > (page_buf)->max_dirty_index_size)                                     \
        (page_buf)->max_dirty_index_size = (page_buf)->dirty_index_size;                                     \
    if ((page_buf)->curr_md_pages > (page_buf)->max_md_pages)                                                \
        (page_buf)->max_md_pages = (page_buf)->curr_md_pages;                                                \
    if ((page_buf)->curr_rd_pages > (page_buf)->max_rd_pages)                                                \
        (page_buf)->max_rd_pages = (page_buf)->curr_rd_pages;                                                \
    if ((page_buf)->mpmde_count > (page_buf)->max_mpmde_count)                                               \
        (page_buf)->max_rd_pages = (page_buf)->curr_rd_pages;

#define H5PB__UPDATE_STATS_FOR_HT_INSERTION(page_buf) ((page_buf)->total_ht_insertions)++;

#define H5PB__UPDATE_STATS_FOR_HT_DELETION(page_buf) (page_buf)->total_ht_deletions++;

#define H5PB__UPDATE_STATS_FOR_HT_SEARCH(page_buf, success, depth)                                           \
    HDassert(depth >= 0);                                                                                    \
    if (success) {                                                                                           \
        (page_buf)->successful_ht_searches++;                                                                \
        (page_buf)->total_successful_ht_search_depth += (int64_t)depth;                                      \
    }                                                                                                        \
    else {                                                                                                   \
        (page_buf)->failed_ht_searches++;                                                                    \
        (page_buf)->total_failed_ht_search_depth += (int64_t)depth;                                          \
    }

#define H5PB__UPDATE_LRU_SIZE_STATS(page_buf)                                                                \
    if ((page_buf)->LRU_len > (page_buf)->max_lru_len)                                                       \
        (page_buf)->max_lru_len = (page_buf)->LRU_len;                                                       \
    if ((page_buf)->LRU_size > (page_buf)->max_lru_size)                                                     \
        (page_buf)->max_lru_size = (page_buf)->LRU_size;

#define H5PB__UPDATE_STATS_FOR_LRU_MD_SKIP(page_buf) ((page_buf)->lru_md_skips)++;

#define H5PB__UPDATE_STATS_FOR_LRU_RD_SKIP(page_buf) ((page_buf)->lru_rd_skips)++;

#define H5PB__UPDATE_STATS_FOR_LRU_TL_SKIP(page_buf)                                                         \
    {                                                                                                        \
        HDassert(page_buf->vfd_swmr_writer);                                                                 \
        ((page_buf)->lru_tl_skips)++;                                                                        \
    }

#define H5PB__UPDATE_TL_SIZE_STATS(page_buf)                                                                 \
    {                                                                                                        \
        HDassert((page_buf)->vfd_swmr_writer);                                                               \
        if ((page_buf)->tl_len > (page_buf)->max_tl_len)                                                     \
            (page_buf)->max_tl_len = (page_buf)->tl_len;                                                     \
        if ((page_buf)->tl_size > (page_buf)->max_tl_size)                                                   \
            (page_buf)->max_tl_size = (page_buf)->tl_size;                                                   \
    }

#define H5PB__UPDATE_DWL_SIZE_STATS(page_buf)                                                                \
    {                                                                                                        \
        HDassert((page_buf)->vfd_swmr_writer);                                                               \
        if ((page_buf)->dwl_len > (page_buf)->max_dwl_len)                                                   \
            (page_buf)->max_dwl_len = (page_buf)->dwl_len;                                                   \
        if ((page_buf)->dwl_size > (page_buf)->max_dwl_size)                                                 \
            (page_buf)->max_dwl_size = (page_buf)->dwl_size;                                                 \
    }

#define H5PB__UPDATE_DWL_DELAYED_WRITES(page_buf, insertion_depth, delay)                                    \
    {                                                                                                        \
        HDassert((page_buf)->vfd_swmr_writer);                                                               \
        (page_buf)->delayed_writes++;                                                                        \
        (page_buf)->total_delay += (int64_t)(delay);                                                         \
        (page_buf)->total_dwl_ins_depth += (insertion_depth);                                                \
    }

#define H5PB__UPDATE_STATS_FOR_ACCESS(page_buf, type, size)                                                  \
    {                                                                                                        \
        int _i;                                                                                              \
                                                                                                             \
        HDassert(page_buf);                                                                                  \
        HDassert((page_buf)->magic == H5PB__H5PB_T_MAGIC);                                                   \
                                                                                                             \
        if (H5FD_MEM_DRAW == (type)) {                                                                       \
            _i = H5PB__STATS_RD;                                                                             \
        }                                                                                                    \
        else if ((size) > (page_buf)->page_size) {                                                           \
            _i = H5PB__STATS_MPMDE;                                                                          \
        }                                                                                                    \
        else {                                                                                               \
            _i = H5PB__STATS_MD;                                                                             \
        }                                                                                                    \
        ((page_buf)->accesses[_i])++;                                                                        \
    } /* H5PB__UPDATE_STATS_FOR_ACCESS */

#define H5PB__UPDATE_STATS_FOR_BYPASS(page_buf, type, size)                                                  \
    {                                                                                                        \
        int ii;                                                                                              \
                                                                                                             \
        HDassert(page_buf);                                                                                  \
        HDassert((page_buf)->magic == H5PB__H5PB_T_MAGIC);                                                   \
                                                                                                             \
        if (H5FD_MEM_DRAW == (type)) {                                                                       \
            ii = H5PB__STATS_RD;                                                                             \
        }                                                                                                    \
        else if ((size) > (page_buf)->page_size) {                                                           \
            ii = H5PB__STATS_MPMDE;                                                                          \
        }                                                                                                    \
        else {                                                                                               \
            ii = H5PB__STATS_MD;                                                                             \
        }                                                                                                    \
        ((page_buf)->bypasses[ii])++;                                                                        \
    } /* H5PB__UPDATE_STATS_FOR_BYPASS */

#define H5PB__UPDATE_STATS_FOR_FLUSH(page_buf, entry_ptr)                                                    \
    {                                                                                                        \
        int i;                                                                                               \
                                                                                                             \
        HDassert(page_buf);                                                                                  \
        HDassert((page_buf)->magic == H5PB__H5PB_T_MAGIC);                                                   \
        HDassert(entry_ptr);                                                                                 \
        HDassert((entry_ptr)->magic == H5PB__H5PB_ENTRY_T_MAGIC);                                            \
                                                                                                             \
        if ((entry_ptr)->is_metadata) {                                                                      \
            if ((entry_ptr)->is_mpmde) {                                                                     \
                i = H5PB__STATS_MPMDE;                                                                       \
            }                                                                                                \
            else {                                                                                           \
                i = H5PB__STATS_MD;                                                                          \
            }                                                                                                \
        }                                                                                                    \
        else {                                                                                               \
            i = H5PB__STATS_RD;                                                                              \
        }                                                                                                    \
        ((page_buf)->flushes[i])++;                                                                          \
    } /* H5PB__UPDATE_STATS_FOR_FLUSH */

#define H5PB__UPDATE_STATS_FOR_EVICTION(page_buf, entry_ptr)                                                 \
    {                                                                                                        \
        int i;                                                                                               \
                                                                                                             \
        HDassert(page_buf);                                                                                  \
        HDassert((page_buf)->magic == H5PB__H5PB_T_MAGIC);                                                   \
        HDassert(entry_ptr);                                                                                 \
        HDassert((entry_ptr)->magic == H5PB__H5PB_ENTRY_T_MAGIC);                                            \
                                                                                                             \
        if ((entry_ptr)->is_metadata) {                                                                      \
            if ((entry_ptr)->is_mpmde) {                                                                     \
                i = H5PB__STATS_MPMDE;                                                                       \
            }                                                                                                \
            else {                                                                                           \
                i = H5PB__STATS_MD;                                                                          \
            }                                                                                                \
        }                                                                                                    \
        else {                                                                                               \
            i = H5PB__STATS_RD;                                                                              \
        }                                                                                                    \
        ((page_buf)->evictions[i])++;                                                                        \
    } /* H5PB__UPDATE_STATS_FOR_EVICTION */

#define H5PB__UPDATE_STATS_FOR_CLEAR(page_buf, entry_ptr)                                                    \
    {                                                                                                        \
        int i;                                                                                               \
                                                                                                             \
        HDassert(page_buf);                                                                                  \
        HDassert((page_buf)->magic == H5PB__H5PB_T_MAGIC);                                                   \
        HDassert(entry_ptr);                                                                                 \
        HDassert((entry_ptr)->magic == H5PB__H5PB_ENTRY_T_MAGIC);                                            \
                                                                                                             \
        if ((entry_ptr)->is_metadata) {                                                                      \
            if ((entry_ptr)->is_mpmde) {                                                                     \
                i = H5PB__STATS_MPMDE;                                                                       \
            }                                                                                                \
            else {                                                                                           \
                i = H5PB__STATS_MD;                                                                          \
            }                                                                                                \
        }                                                                                                    \
        else {                                                                                               \
            i = H5PB__STATS_RD;                                                                              \
        }                                                                                                    \
        ((page_buf)->clears[i])++;                                                                           \
    } /* H5PB__UPDATE_STATS_FOR_CLEAR */

#define H5PB__UPDATE_STATS_FOR_INSERTION(page_buf, entry_ptr)                                                \
    {                                                                                                        \
        int i;                                                                                               \
                                                                                                             \
        HDassert(page_buf);                                                                                  \
        HDassert((page_buf)->magic == H5PB__H5PB_T_MAGIC);                                                   \
        HDassert(entry_ptr);                                                                                 \
        HDassert((entry_ptr)->magic == H5PB__H5PB_ENTRY_T_MAGIC);                                            \
                                                                                                             \
        if ((entry_ptr)->is_metadata) {                                                                      \
            if ((entry_ptr)->is_mpmde) {                                                                     \
                i = H5PB__STATS_MPMDE;                                                                       \
            }                                                                                                \
            else {                                                                                           \
                i = H5PB__STATS_MD;                                                                          \
            }                                                                                                \
        }                                                                                                    \
        else {                                                                                               \
            i = H5PB__STATS_RD;                                                                              \
        }                                                                                                    \
        ((page_buf)->insertions[i])++;                                                                       \
    } /* H5PB__UPDATE_STATS_FOR_INSERTION */

#define H5PB__UPDATE_STATS_FOR_LOAD(page_buf, entry_ptr)                                                     \
    {                                                                                                        \
        int i;                                                                                               \
                                                                                                             \
        HDassert(page_buf);                                                                                  \
        HDassert((page_buf)->magic == H5PB__H5PB_T_MAGIC);                                                   \
        HDassert(entry_ptr);                                                                                 \
        HDassert((entry_ptr)->magic == H5PB__H5PB_ENTRY_T_MAGIC);                                            \
                                                                                                             \
        if ((entry_ptr)->is_metadata) {                                                                      \
            if ((entry_ptr)->is_mpmde) {                                                                     \
                i = H5PB__STATS_MPMDE;                                                                       \
            }                                                                                                \
            else {                                                                                           \
                i = H5PB__STATS_MD;                                                                          \
            }                                                                                                \
        }                                                                                                    \
        else {                                                                                               \
            i = H5PB__STATS_RD;                                                                              \
        }                                                                                                    \
        ((page_buf)->loads[i])++;                                                                            \
    } /* H5PB__UPDATE_STATS_FOR_LOAD */

#define H5PB__UPDATE_STATS_FOR_READ_SPLIT(page_buf)                                                          \
    {                                                                                                        \
        HDassert(page_buf);                                                                                  \
        HDassert((page_buf)->magic == H5PB__H5PB_T_MAGIC);                                                   \
        (page_buf->md_read_splits)++;                                                                        \
    } /* H5PB__UPDATE_STATS_FOR_READ_SPLIT */

#define H5PB__UPDATE_STATS_FOR_WRITE_SPLIT(page_buf)                                                         \
    {                                                                                                        \
        HDassert(page_buf);                                                                                  \
        HDassert((page_buf)->magic == H5PB__H5PB_T_MAGIC);                                                   \
        (page_buf->md_write_splits)++;                                                                       \
    } /* H5PB__UPDATE_STATS_FOR_READ_SPLIT */

#else /* H5PB__COLLECT_PAGE_BUFFER_STATS */

#define H5PB__UPDATE_PB_HIT_RATE_STATS(page_buf, hit, is_metadata, is_mpmde)
#define H5PB__UPDATE_HT_SIZE_STATS(page_buf)
#define H5PB__UPDATE_STATS_FOR_HT_INSERTION(page_buf)
#define H5PB__UPDATE_STATS_FOR_HT_DELETION(page_buf)
#define H5PB__UPDATE_HT_SEARCH_STATS(page_buf, success, depth)
#define H5PB__UPDATE_LRU_SIZE_STATS(page_buf)
#define H5PB__UPDATE_STATS_FOR_LRU_MD_SKIP(page_buf)
#define H5PB__UPDATE_STATS_FOR_LRU_RD_SKIP(page_buf)
#define H5PB__UPDATE_STATS_FOR_LRU_TL_SKIP(page_buf)
#define H5PB__UPDATE_STATS_FOR_LRU_DWL_SKIP(page_buf)
#define H5PB__UPDATE_TL_SIZE_STATS(page_buf)
#define H5PB__UPDATE_DWL_SIZE_STATS(page_buf)
#define H5PB__UPDATE_DWL_DELAYED_WRITES(page_buf, insertion_depth, delay)
#define H5PB__UPDATE_STATS_FOR_ACCESS(page_buf, type, size)
#define H5PB__UPDATE_STATS_FOR_BYPASS(page_buf, type, size)
#define H5PB__UPDATE_STATS_FOR_FLUSH(page_buf, entry_ptr)
#define H5PB__UPDATE_STATS_FOR_EVICTION(page_buf, entry_ptr)
#define H5PB__UPDATE_STATS_FOR_CLEAR(page_buf, entry_ptr)
#define H5PB__UPDATE_STATS_FOR_INSERTION(page_buf, entry_ptr)
#define H5PB__UPDATE_STATS_FOR_LOAD(page_buf, entry_ptr)
#define H5PB__UPDATE_STATS_FOR_READ_SPLIT(page_buf)
#define H5PB__UPDATE_STATS_FOR_WRITE_SPLIT(page_buf)

#endif /* H5PB__COLLECT_PAGE_BUFFER_STATS */

/***********************************************************************
 *
 * Hash table access and manipulation macros:
 *
 * The following macros handle searches, insertions, and deletion in
 * the hash table.
 *
 * Note that the input to the hash function is the page of the page
 * buffer entry, not it address (recall that page * page_size) == addr).
 *
 *                                              JRM -- 10/09/18
 *
 * Changes:
 *
 *   - None
 *
 ***********************************************************************/

#define H5PB__HASH_MASK ((uint64_t)(H5PB__HASH_TABLE_LEN - 1))

#define H5PB__HASH_FCN(x) (int)(((uint64_t)(x)) & H5PB__HASH_MASK)

#if H5PB__DO_SANITY_CHECKS

#define H5PB__PRE_HT_INSERT_SC(page_buf, entry_ptr, fail_val)                                                \
    if (((page_buf) == NULL) || ((page_buf)->magic != H5PB__H5PB_T_MAGIC) || ((entry_ptr) == NULL) ||        \
        ((entry_ptr)->ht_next != NULL) || ((entry_ptr)->ht_prev != NULL) || ((entry_ptr)->size <= 0) ||      \
        (H5PB__HASH_FCN((entry_ptr)->page) < 0) ||                                                           \
        (H5PB__HASH_FCN((entry_ptr)->page) >= H5PB__HASH_TABLE_LEN) ||                                       \
        ((page_buf)->index_size != ((page_buf)->clean_index_size + (page_buf)->dirty_index_size)) ||         \
        ((page_buf)->index_size < ((page_buf)->clean_index_size)) ||                                         \
        ((page_buf)->index_size < ((page_buf)->dirty_index_size)) ||                                         \
        ((page_buf)->index_len != (page_buf)->il_len) || ((page_buf)->index_size != (page_buf)->il_size) ||  \
        ((page_buf)->curr_pages < 0) || ((page_buf)->curr_rd_pages < 0) ||                                   \
        ((page_buf)->curr_md_pages < 0) ||                                                                   \
        (((page_buf)->curr_pages != ((page_buf)->curr_md_pages + (page_buf)->curr_rd_pages))) ||             \
        ((page_buf)->mpmde_count < 0) ||                                                                     \
        ((page_buf)->index_len != ((page_buf)->curr_pages + (page_buf)->mpmde_count))) {                     \
        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, fail_val, "pre HT insert SC failed")                            \
    }

#define H5PB__POST_HT_INSERT_SC(page_buf, entry_ptr, fail_val)                                               \
    if (((page_buf) == NULL) || ((page_buf)->magic != H5PB__H5PB_T_MAGIC) ||                                 \
        ((page_buf)->index_size != ((page_buf)->clean_index_size + (page_buf)->dirty_index_size)) ||         \
        ((page_buf)->index_size < ((page_buf)->clean_index_size)) ||                                         \
        ((page_buf)->index_size < ((page_buf)->dirty_index_size)) ||                                         \
        ((page_buf)->index_len != (page_buf)->il_len) ||                                                     \
        ((page_buf)->index_len != ((page_buf)->curr_pages + (page_buf)->mpmde_count)) ||                     \
        ((page_buf)->index_size != (page_buf)->il_size)) {                                                   \
        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, fail_val, "post HT insert SC failed")                           \
    }

#define H5PB__PRE_HT_REMOVE_SC(page_buf, entry_ptr)                                                          \
    if (((page_buf) == NULL) || ((page_buf)->magic != H5PB__H5PB_T_MAGIC) || ((page_buf)->index_len < 1) ||  \
        ((entry_ptr) == NULL) || ((page_buf)->index_size < (int64_t)((entry_ptr)->size)) ||                  \
        ((entry_ptr)->size <= 0) || (H5PB__HASH_FCN((entry_ptr)->page) < 0) ||                               \
        (H5PB__HASH_FCN((entry_ptr)->page) >= H5PB__HASH_TABLE_LEN) ||                                       \
        (((page_buf)->ht)[(H5PB__HASH_FCN((entry_ptr)->page))] == NULL) ||                                   \
        ((((page_buf)->ht)[(H5PB__HASH_FCN((entry_ptr)->page))] != (entry_ptr)) &&                           \
         ((entry_ptr)->ht_prev == NULL)) ||                                                                  \
        ((((page_buf)->ht)[(H5PB__HASH_FCN((entry_ptr)->page))] == (entry_ptr)) &&                           \
         ((entry_ptr)->ht_prev != NULL)) ||                                                                  \
        ((page_buf)->index_size != ((page_buf)->clean_index_size + (page_buf)->dirty_index_size)) ||         \
        ((page_buf)->index_size < ((page_buf)->clean_index_size)) ||                                         \
        ((page_buf)->index_size < ((page_buf)->dirty_index_size)) ||                                         \
        ((page_buf)->index_len != (page_buf)->il_len) || ((page_buf)->index_size != (page_buf)->il_size)) {  \
        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "pre HT remove SC failed")                                \
    }

#define H5PB__POST_HT_REMOVE_SC(page_buf, entry_ptr)                                                         \
    if (((page_buf) == NULL) || ((page_buf)->magic != H5PB__H5PB_T_MAGIC) || ((entry_ptr) == NULL) ||        \
        ((entry_ptr)->size <= 0) || ((entry_ptr)->ht_prev != NULL) || ((entry_ptr)->ht_prev != NULL) ||      \
        ((page_buf)->index_size != ((page_buf)->clean_index_size + (page_buf)->dirty_index_size)) ||         \
        ((page_buf)->index_size < ((page_buf)->clean_index_size)) ||                                         \
        ((page_buf)->index_size < ((page_buf)->dirty_index_size)) ||                                         \
        ((page_buf)->index_len != (page_buf)->il_len) || ((page_buf)->index_size != (page_buf)->il_size) ||  \
        ((page_buf)->curr_pages < 0) || ((page_buf)->curr_rd_pages < 0) ||                                   \
        ((page_buf)->curr_md_pages < 0) ||                                                                   \
        (((page_buf)->curr_pages != ((page_buf)->curr_md_pages + (page_buf)->curr_rd_pages))) ||             \
        ((page_buf)->mpmde_count < 0) ||                                                                     \
        ((page_buf)->index_len != ((page_buf)->curr_pages + (page_buf)->mpmde_count))) {                     \
        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "post HT remove SC failed")                               \
    }

#define H5PB__PRE_HT_SEARCH_SC(page_buf, page, fail_val)                                                     \
    if (((page_buf) == NULL) || ((page_buf)->magic != H5PB__H5PB_T_MAGIC) ||                                 \
        ((page_buf)->index_size != ((page_buf)->clean_index_size + (page_buf)->dirty_index_size)) ||         \
        (H5PB__HASH_FCN(page) < 0) || (H5PB__HASH_FCN(page) >= H5PB__HASH_TABLE_LEN)) {                      \
        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, fail_val, "pre HT search SC failed")                            \
    }

#define H5PB__POST_SUC_HT_SEARCH_SC(page_buf, entry_ptr, k, fail_val)                                        \
    if (((page_buf) == NULL) || ((page_buf)->magic != H5PB__H5PB_T_MAGIC) || ((page_buf)->index_len < 1) ||  \
        ((entry_ptr) == NULL) || ((page_buf)->index_size < (int64_t)((entry_ptr)->size)) ||                  \
        ((page_buf)->index_size != ((page_buf)->clean_index_size + (page_buf)->dirty_index_size)) ||         \
        ((entry_ptr)->size <= 0) || (((page_buf)->ht)[k] == NULL) ||                                         \
        ((((page_buf)->ht)[k] != (entry_ptr)) && ((entry_ptr)->ht_prev == NULL)) ||                          \
        ((((page_buf)->ht)[k] == (entry_ptr)) && ((entry_ptr)->ht_prev != NULL)) ||                          \
        (((entry_ptr)->ht_prev != NULL) && ((entry_ptr)->ht_prev->ht_next != (entry_ptr))) ||                \
        (((entry_ptr)->ht_next != NULL) && ((entry_ptr)->ht_next->ht_prev != (entry_ptr)))) {                \
        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, fail_val, "post successful HT search SC failed")                \
    }

#define H5PB__POST_HT_SHIFT_TO_FRONT_SC(page_buf, entry_ptr, k, fail_val)                                    \
    if (((page_buf) == NULL) || (((page_buf)->ht)[k] != (entry_ptr)) || ((entry_ptr)->ht_prev != NULL)) {    \
        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, fail_val, "post HT shift to front SC failed")                   \
    }

#define H5PB__PRE_HT_ENTRY_SIZE_CHANGE_SC(page_buf, old_size, new_size, entry_ptr, was_clean)                \
    if (((page_buf) == NULL) || ((page_buf)->index_len <= 0) || ((page_buf)->index_size <= 0) ||             \
        ((new_size) <= 0) || ((old_size) > (page_buf)->index_size) ||                                        \
        (((page_buf)->index_len == 1) && ((page_buf)->index_size != (old_size))) ||                          \
        ((page_buf)->index_size != ((page_buf)->clean_index_size + (page_buf)->dirty_index_size)) ||         \
        ((page_buf)->index_size < ((page_buf)->clean_index_size)) ||                                         \
        ((page_buf)->index_size < ((page_buf)->dirty_index_size)) ||                                         \
        ((!(was_clean) || ((page_buf)->clean_index_size < (old_size))) &&                                    \
         (((was_clean)) || ((page_buf)->dirty_index_size < (old_size)))) ||                                  \
        ((entry_ptr) == NULL) || ((page_buf)->index_len != (page_buf)->il_len) ||                            \
        ((page_buf)->index_size != (page_buf)->il_size)) {                                                   \
        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "pre HT entry size change SC failed")                     \
    }

#define H5PB__POST_HT_ENTRY_SIZE_CHANGE_SC(page_buf, old_size, new_size, entry_ptr)                          \
    if (((page_buf) == NULL) || ((page_buf)->index_len <= 0) || ((page_buf)->index_size <= 0) ||             \
        ((new_size) > (page_buf)->index_size) ||                                                             \
        ((page_buf)->index_size != ((page_buf)->clean_index_size + (page_buf)->dirty_index_size)) ||         \
        ((page_buf)->index_size < ((page_buf)->clean_index_size)) ||                                         \
        ((page_buf)->index_size < ((page_buf)->dirty_index_size)) ||                                         \
        ((!((entry_ptr)->is_dirty) || ((page_buf)->dirty_index_size < (new_size))) &&                        \
         ((((entry_ptr)->is_dirty)) || ((page_buf)->clean_index_size < (new_size)))) ||                      \
        (((page_buf)->index_len == 1) && ((page_buf)->index_size != (new_size))) ||                          \
        ((page_buf)->index_len != (page_buf)->il_len) || ((page_buf)->index_size != (page_buf)->il_size)) {  \
        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "post HT entry size change SC failed")                    \
    }

#define H5PB__PRE_HT_UPDATE_FOR_ENTRY_CLEAN_SC(page_buf, entry_ptr)                                          \
    if (((page_buf) == NULL) || ((page_buf)->magic != H5PB__H5PB_T_MAGIC) || ((page_buf)->index_len <= 0) || \
        ((entry_ptr) == NULL) || ((entry_ptr)->is_dirty != FALSE) ||                                         \
        ((page_buf)->index_size < (int64_t)((entry_ptr)->size)) ||                                           \
        ((page_buf)->dirty_index_size < (int64_t)((entry_ptr)->size)) ||                                     \
        ((page_buf)->index_size != ((page_buf)->clean_index_size + (page_buf)->dirty_index_size)) ||         \
        ((page_buf)->index_size < ((page_buf)->clean_index_size)) ||                                         \
        ((page_buf)->index_size < ((page_buf)->dirty_index_size))) {                                         \
        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "pre HT update for entry clean SC failed")                \
    }

#define H5PB__PRE_HT_UPDATE_FOR_ENTRY_DIRTY_SC(page_buf, entry_ptr)                                          \
    if (((page_buf) == NULL) || ((page_buf)->magic != H5PB__H5PB_T_MAGIC) || ((page_buf)->index_len <= 0) || \
        ((entry_ptr) == NULL) || ((entry_ptr)->is_dirty != TRUE) ||                                          \
        ((page_buf)->index_size < (int64_t)((entry_ptr)->size)) ||                                           \
        ((page_buf)->clean_index_size < (int64_t)((entry_ptr)->size)) ||                                     \
        ((page_buf)->index_size != ((page_buf)->clean_index_size + (page_buf)->dirty_index_size)) ||         \
        ((page_buf)->index_size < ((page_buf)->clean_index_size)) ||                                         \
        ((page_buf)->index_size < ((page_buf)->dirty_index_size))) {                                         \
        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "pre HT update for entry dirty SC failed")                \
    }

#define H5PB__POST_HT_UPDATE_FOR_ENTRY_CLEAN_SC(page_buf, entry_ptr)                                         \
    if (((page_buf)->index_size != ((page_buf)->clean_index_size + (page_buf)->dirty_index_size)) ||         \
        ((page_buf)->index_size < ((page_buf)->clean_index_size)) ||                                         \
        ((page_buf)->index_size < ((page_buf)->dirty_index_size))) {                                         \
        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "post HT update for entry clean SC failed")               \
    }

#define H5PB__POST_HT_UPDATE_FOR_ENTRY_DIRTY_SC(page_buf, entry_ptr)                                         \
    if (((page_buf)->index_size != ((page_buf)->clean_index_size + (page_buf)->dirty_index_size)) ||         \
        ((page_buf)->index_size < ((page_buf)->clean_index_size)) ||                                         \
        ((page_buf)->index_size < ((page_buf)->dirty_index_size))) {                                         \
        HGOTO_ERROR(H5E_PAGEBUF, H5E_SYSTEM, FAIL, "post HT update for entry dirty SC failed")               \
    }

#else /* H5PB__DO_SANITY_CHECKS */

#define H5PB__PRE_HT_INSERT_SC(page_buf, entry_ptr, fail_val)
#define H5PB__POST_HT_INSERT_SC(page_buf, entry_ptr, fail_val)
#define H5PB__PRE_HT_REMOVE_SC(page_buf, entry_ptr)
#define H5PB__POST_HT_REMOVE_SC(page_buf, entry_ptr)
#define H5PB__PRE_HT_SEARCH_SC(page_buf, Addr, fail_val)
#define H5PB__POST_SUC_HT_SEARCH_SC(page_buf, entry_ptr, k, fail_val)
#define H5PB__POST_HT_SHIFT_TO_FRONT_SC(page_buf, entry_ptr, k, fail_val)
#define H5PB__PRE_HT_UPDATE_FOR_ENTRY_CLEAN_SC(page_buf, entry_ptr)
#define H5PB__PRE_HT_UPDATE_FOR_ENTRY_DIRTY_SC(page_buf, entry_ptr)
#define H5PB__PRE_HT_ENTRY_SIZE_CHANGE_SC(page_buf, old_size, new_size, entry_ptr, was_clean)
#define H5PB__POST_HT_ENTRY_SIZE_CHANGE_SC(page_buf, old_size, new_size, entry_ptr)
#define H5PB__POST_HT_UPDATE_FOR_ENTRY_CLEAN_SC(page_buf, entry_ptr)
#define H5PB__POST_HT_UPDATE_FOR_ENTRY_DIRTY_SC(page_buf, entry_ptr)

#endif /* H5PB__DO_SANITY_CHECKS */

#define H5PB__INSERT_IN_INDEX(page_buf, entry_ptr, fail_val)                                                 \
    {                                                                                                        \
        int k;                                                                                               \
        H5PB__PRE_HT_INSERT_SC(page_buf, entry_ptr, fail_val)                                                \
        k = H5PB__HASH_FCN((entry_ptr)->page);                                                               \
        if (((page_buf)->ht)[k] != NULL) {                                                                   \
            (entry_ptr)->ht_next          = ((page_buf)->ht)[k];                                             \
            (entry_ptr)->ht_next->ht_prev = (entry_ptr);                                                     \
        }                                                                                                    \
        ((page_buf)->ht)[k] = (entry_ptr);                                                                   \
        (page_buf)->index_len++;                                                                             \
        (page_buf)->index_size += (int64_t)((entry_ptr)->size);                                              \
        if ((entry_ptr)->is_dirty) {                                                                         \
            (page_buf)->dirty_index_size += (int64_t)((entry_ptr)->size);                                    \
        }                                                                                                    \
        else {                                                                                               \
            (page_buf)->clean_index_size += (int64_t)((entry_ptr)->size);                                    \
        }                                                                                                    \
        if ((entry_ptr)->is_metadata) {                                                                      \
            if ((entry_ptr)->is_mpmde) {                                                                     \
                ((page_buf)->mpmde_count)++;                                                                 \
            }                                                                                                \
            else {                                                                                           \
                ((page_buf)->curr_md_pages)++;                                                               \
                (page_buf)->curr_pages++;                                                                    \
            }                                                                                                \
        }                                                                                                    \
        else {                                                                                               \
            ((page_buf)->curr_rd_pages)++;                                                                   \
            (page_buf)->curr_pages++;                                                                        \
        }                                                                                                    \
        H5PB__IL_DLL_APPEND((entry_ptr), (page_buf)->il_head, (page_buf)->il_tail, (page_buf)->il_len,       \
                            (page_buf)->il_size, fail_val)                                                   \
        H5PB__UPDATE_STATS_FOR_HT_INSERTION(page_buf)                                                        \
        H5PB__UPDATE_HT_SIZE_STATS(page_buf)                                                                 \
        H5PB__POST_HT_INSERT_SC(page_buf, entry_ptr, fail_val)                                               \
    }

#define H5PB__DELETE_FROM_INDEX(page_buf, entry_ptr, fail_val)                                               \
    {                                                                                                        \
        int k;                                                                                               \
        H5PB__PRE_HT_REMOVE_SC(page_buf, entry_ptr)                                                          \
        k = H5PB__HASH_FCN((entry_ptr)->page);                                                               \
        if ((entry_ptr)->ht_next)                                                                            \
            (entry_ptr)->ht_next->ht_prev = (entry_ptr)->ht_prev;                                            \
        if ((entry_ptr)->ht_prev)                                                                            \
            (entry_ptr)->ht_prev->ht_next = (entry_ptr)->ht_next;                                            \
        if (((page_buf)->ht)[k] == (entry_ptr))                                                              \
            ((page_buf)->ht)[k] = (entry_ptr)->ht_next;                                                      \
        (entry_ptr)->ht_next = NULL;                                                                         \
        (entry_ptr)->ht_prev = NULL;                                                                         \
        (page_buf)->index_len--;                                                                             \
        (page_buf)->index_size -= (int64_t)((entry_ptr)->size);                                              \
        if ((entry_ptr)->is_dirty) {                                                                         \
            (page_buf)->dirty_index_size -= (int64_t)((entry_ptr)->size);                                    \
        }                                                                                                    \
        else {                                                                                               \
            (page_buf)->clean_index_size -= (int64_t)((entry_ptr)->size);                                    \
        }                                                                                                    \
        if ((entry_ptr)->is_metadata) {                                                                      \
            if ((entry_ptr)->is_mpmde) {                                                                     \
                ((page_buf)->mpmde_count)--;                                                                 \
            }                                                                                                \
            else {                                                                                           \
                ((page_buf)->curr_md_pages)--;                                                               \
                (page_buf)->curr_pages--;                                                                    \
            }                                                                                                \
        }                                                                                                    \
        else {                                                                                               \
            ((page_buf)->curr_rd_pages)--;                                                                   \
            (page_buf)->curr_pages--;                                                                        \
        }                                                                                                    \
        H5PB__IL_DLL_REMOVE((entry_ptr), (page_buf)->il_head, (page_buf)->il_tail, (page_buf)->il_len,       \
                            (page_buf)->il_size, fail_val)                                                   \
        H5PB__UPDATE_STATS_FOR_HT_DELETION(page_buf)                                                         \
        H5PB__POST_HT_REMOVE_SC(page_buf, entry_ptr)                                                         \
    }

#define H5PB__SEARCH_INDEX(page_buf, Page, entry_ptr, fail_val)                                              \
    {                                                                                                        \
        int k;                                                                                               \
        int depth = 0;                                                                                       \
        H5PB__PRE_HT_SEARCH_SC(page_buf, Page, fail_val)                                                     \
        k         = H5PB__HASH_FCN(Page);                                                                    \
        entry_ptr = ((page_buf)->ht)[k];                                                                     \
        while (entry_ptr) {                                                                                  \
            if ((Page) == (entry_ptr)->page) {                                                               \
                H5PB__POST_SUC_HT_SEARCH_SC(page_buf, entry_ptr, k, fail_val)                                \
                if ((entry_ptr) != ((page_buf)->ht)[k]) {                                                    \
                    if ((entry_ptr)->ht_next)                                                                \
                        (entry_ptr)->ht_next->ht_prev = (entry_ptr)->ht_prev;                                \
                    HDassert((entry_ptr)->ht_prev != NULL);                                                  \
                    (entry_ptr)->ht_prev->ht_next = (entry_ptr)->ht_next;                                    \
                    ((page_buf)->ht)[k]->ht_prev  = (entry_ptr);                                             \
                    (entry_ptr)->ht_next          = ((page_buf)->ht)[k];                                     \
                    (entry_ptr)->ht_prev          = NULL;                                                    \
                    ((page_buf)->ht)[k]           = (entry_ptr);                                             \
                    H5PB__POST_HT_SHIFT_TO_FRONT_SC(page_buf, entry_ptr, k, fail_val)                        \
                }                                                                                            \
                break;                                                                                       \
            }                                                                                                \
            (entry_ptr) = (entry_ptr)->ht_next;                                                              \
            (depth)++;                                                                                       \
        }                                                                                                    \
        H5PB__UPDATE_STATS_FOR_HT_SEARCH(page_buf, (entry_ptr != NULL), depth)                               \
    }

#define H5PB__UPDATE_INDEX_FOR_ENTRY_CLEAN(page_buf, entry_ptr)                                              \
    {                                                                                                        \
        H5PB__PRE_HT_UPDATE_FOR_ENTRY_CLEAN_SC(page_buf, entry_ptr);                                         \
        (page_buf)->dirty_index_size -= (int64_t)((entry_ptr)->size);                                        \
        (page_buf)->clean_index_size += (int64_t)((entry_ptr)->size);                                        \
        H5PB__POST_HT_UPDATE_FOR_ENTRY_CLEAN_SC(page_buf, entry_ptr);                                        \
    }

#define H5PB__UPDATE_INDEX_FOR_ENTRY_DIRTY(page_buf, entry_ptr)                                              \
    {                                                                                                        \
        H5PB__PRE_HT_UPDATE_FOR_ENTRY_DIRTY_SC(page_buf, entry_ptr);                                         \
        (page_buf)->clean_index_size -= (int64_t)((entry_ptr)->size);                                        \
        (page_buf)->dirty_index_size += (int64_t)((entry_ptr)->size);                                        \
        H5PB__POST_HT_UPDATE_FOR_ENTRY_DIRTY_SC(page_buf, entry_ptr);                                        \
    }

#define H5PB__UPDATE_INDEX_FOR_SIZE_CHANGE(page_buf, old_size, new_size, entry_ptr, was_clean)               \
    {                                                                                                        \
        H5PB__PRE_HT_ENTRY_SIZE_CHANGE_SC(page_buf, old_size, new_size, entry_ptr, was_clean)                \
        (page_buf)->index_size -= (old_size);                                                                \
        (page_buf)->index_size += (new_size);                                                                \
        if (was_clean) {                                                                                     \
            (page_buf)->clean_index_size -= (old_size);                                                      \
        }                                                                                                    \
        else {                                                                                               \
            (page_buf)->dirty_index_size -= (old_size);                                                      \
        }                                                                                                    \
        if ((entry_ptr)->is_dirty) {                                                                         \
            (page_buf)->dirty_index_size += (new_size);                                                      \
        }                                                                                                    \
        else {                                                                                               \
            (page_buf)->clean_index_size += (new_size);                                                      \
        }                                                                                                    \
        H5PB__DLL_UPDATE_FOR_SIZE_CHANGE((page_buf)->il_len, (page_buf)->il_size, (old_size), (new_size))    \
        H5PB__POST_HT_ENTRY_SIZE_CHANGE_SC(page_buf, old_size, new_size, entry_ptr)                          \
    }

/***********************************************************************
 *
 * Replacement policy update macros
 *
 * The following macros handle updates to the replacement policy for
 * insertions, flushes, and evictions.
 *
 * At present, the only replacement policy is a modified LRU policy.
 *
 *                                              JRM -- 10/09/18
 *
 ***********************************************************************/

/*-------------------------------------------------------------------------
 *
 * Macro:	H5PB__UPDATE_RP_FOR_EVICTION
 *
 * Purpose:     Update the replacement policy data structures for an
 *		eviction of the specified page buffer entry.
 *
 *		At present, we only support the modified LRU policy, so
 *		this function deals with that case unconditionally.  If
 *		we ever support other replacement policies, the function
 *		should switch on the current policy and act accordingly.
 *
 * Return:      Non-negative on success/Negative on failure.
 *
 * Programmer:  John Mainzer, 10/09/18
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

#define H5PB__UPDATE_RP_FOR_EVICTION(page_buf, entry_ptr, fail_val)                                          \
    {                                                                                                        \
        HDassert((page_buf));                                                                                \
        HDassert((page_buf)->magic == H5PB__H5PB_T_MAGIC);                                                   \
        HDassert((entry_ptr));                                                                               \
        HDassert((entry_ptr)->magic == H5PB__H5PB_ENTRY_T_MAGIC);                                            \
        HDassert(!((entry_ptr)->is_dirty));                                                                  \
        HDassert((entry_ptr)->size >= page_buf->page_size);                                                  \
                                                                                                             \
        /* modified LRU specific code */                                                                     \
                                                                                                             \
        /* remove the entry from the LRU list. */                                                            \
                                                                                                             \
        H5PB__DLL_REMOVE((entry_ptr), (page_buf)->LRU_head_ptr, (page_buf)->LRU_tail_ptr,                    \
                         (page_buf)->LRU_len, (page_buf)->LRU_size, (fail_val))                              \
                                                                                                             \
        /* End modified LRU specific code. */                                                                \
                                                                                                             \
    } /* H5PB__UPDATE_RP_FOR_EVICTION */

/*-------------------------------------------------------------------------
 *
 * Macro:	H5PB__UPDATE_RP_FOR_REMOVE
 *
 * Purpose:     Update the replacement policy data structures for the
 *		removal of the specified page buffer entry from the
 *              replacement policy, but not from the page buffer.
 *
 *              At present, this this only happens when an entry is
 *              dirtied, and subject to a delayed write.
 *
 *		At present, we only support the modified LRU policy, so
 *		this function deals with that case unconditionally.  If
 *		we ever support other replacement policies, the function
 *		should switch on the current policy and act accordingly.
 *
 * Return:      Non-negative on success/Negative on failure.
 *
 * Programmer:  John Mainzer, 10/09/18
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

#define H5PB__UPDATE_RP_FOR_REMOVE(page_buf, entry_ptr, fail_val)                                            \
    {                                                                                                        \
        HDassert((page_buf));                                                                                \
        HDassert((page_buf)->magic == H5PB__H5PB_T_MAGIC);                                                   \
        HDassert((entry_ptr));                                                                               \
        HDassert((entry_ptr)->magic == H5PB__H5PB_ENTRY_T_MAGIC);                                            \
        HDassert(!((entry_ptr)->is_mpmde));                                                                  \
        HDassert((entry_ptr)->size == page_buf->page_size);                                                  \
                                                                                                             \
        /* modified LRU specific code */                                                                     \
                                                                                                             \
        /* remove the entry from the LRU list. */                                                            \
                                                                                                             \
        H5PB__DLL_REMOVE((entry_ptr), (page_buf)->LRU_head_ptr, (page_buf)->LRU_tail_ptr,                    \
                         (page_buf)->LRU_len, (page_buf)->LRU_size, (fail_val))                              \
                                                                                                             \
        /* End modified LRU specific code. */                                                                \
                                                                                                             \
    } /* H5PB__UPDATE_RP_FOR_EVICTION */

/*-------------------------------------------------------------------------
 *
 * Macro:	H5PB__UPDATE_RP_FOR_ACCESS
 *
 * Purpose:     Update the replacement policy data structures for an
 *		access of the specified page buffer entry.
 *
 *		At present, we only support the modified LRU policy, so
 *		this function deals with that case unconditionally.  If
 *		we ever support other replacement policies, the function
 *		should switch on the current policy and act accordingly.
 *
 * Return:      N/A
 *
 * Programmer:  John Mainzer, 10/09/18
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

#define H5PB__UPDATE_RP_FOR_ACCESS(page_buf, entry_ptr, fail_val)                                            \
    {                                                                                                        \
        HDassert((page_buf));                                                                                \
        HDassert((page_buf)->magic == H5PB__H5PB_T_MAGIC);                                                   \
        HDassert((entry_ptr));                                                                               \
        HDassert((entry_ptr)->magic == H5PB__H5PB_ENTRY_T_MAGIC);                                            \
        HDassert((entry_ptr)->size >= page_buf->page_size);                                                  \
                                                                                                             \
        /* modified LRU specific code */                                                                     \
                                                                                                             \
        /* Move entry to the head of the LRU */                                                              \
                                                                                                             \
        H5PB__DLL_REMOVE((entry_ptr), (page_buf)->LRU_head_ptr, (page_buf)->LRU_tail_ptr,                    \
                         (page_buf)->LRU_len, (page_buf)->LRU_size, (fail_val))                              \
                                                                                                             \
        H5PB__DLL_PREPEND((entry_ptr), (page_buf)->LRU_head_ptr, (page_buf)->LRU_tail_ptr,                   \
                          (page_buf)->LRU_len, (page_buf)->LRU_size, (fail_val))                             \
                                                                                                             \
        /* End modified LRU specific code. */                                                                \
                                                                                                             \
    } /* H5PB__UPDATE_RP_FOR_ACCESS */

/*-------------------------------------------------------------------------
 *
 * Macro:	H5PB__UPDATE_RP_FOR_FLUSH
 *
 * Purpose:     Update the replacement policy data structures for a flush
 *		of the specified page buffer entry.
 *
 *		At present, we only support the modified LRU policy, so
 *		this function deals with that case unconditionally.  If
 *		we ever support other replacement policies, the function
 *		should switch on the current policy and act accordingly.
 *
 * Return:      N/A
 *
 * Programmer:  John Mainzer, 10/09/18
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

#define H5PB__UPDATE_RP_FOR_FLUSH(page_buf, entry_ptr, fail_val)                                             \
    {                                                                                                        \
        H5PB__UPDATE_RP_FOR_ACCESS(page_buf, entry_ptr, fail_val)                                            \
                                                                                                             \
    } /* H5PB__UPDATE_RP_FOR_FLUSH */

/*-------------------------------------------------------------------------
 *
 * Macro:	H5PB__UPDATE_RP_FOR_INSERT_APPEND
 *
 * Purpose:     Update the replacement policy data structures for an
 *		insertion of the specified cache entry.
 *
 *		Unlike H5PB__UPDATE_RP_FOR_INSERTION below, mark the
 *		new entry as the LEAST recently used entry, not the
 *		most recently used.
 *
 *		At present, we only support the modified LRU policy, so
 *		this function deals with that case unconditionally.  If
 *		we ever support other replacement policies, the function
 *		should switch on the current policy and act accordingly.
 *
 * Return:      N/A
 *
 * Programmer:  John Mainzer, 10/10/18
 *
 * Modifications:
 *
 *              None.
 *
 *-------------------------------------------------------------------------
 */

#define H5PB__UPDATE_RP_FOR_INSERT_APPEND(page_buf, entry_ptr, fail_val)                                     \
    {                                                                                                        \
        HDassert((page_buf));                                                                                \
        HDassert((page_buf)->magic == H5PB__H5PB_T_MAGIC);                                                   \
        HDassert((entry_ptr));                                                                               \
        HDassert((entry_ptr)->magic == H5PB__H5PB_ENTRY_T_MAGIC);                                            \
        HDassert((entry_ptr)->size == page_buf->page_size);                                                  \
                                                                                                             \
        /* modified LRU specific code */                                                                     \
                                                                                                             \
        /* insert the entry at the tail of the LRU list. */                                                  \
                                                                                                             \
        H5PB__DLL_APPEND((entry_ptr), (page_buf)->LRU_head_ptr, (page_buf)->LRU_tail_ptr,                    \
                         (page_buf)->LRU_len, (page_buf)->LRU_size, (fail_val))                              \
                                                                                                             \
        H5PB__UPDATE_LRU_SIZE_STATS(page_buf)                                                                \
                                                                                                             \
        /* End modified LRU specific code. */                                                                \
    }

/*-------------------------------------------------------------------------
 *
 * Macro:	H5PB__UPDATE_RP_FOR_INSERTION
 *
 * Purpose:     Update the replacement policy data structures for an
 *		insertion of the specified cache entry.
 *
 *		At present, we only support the modified LRU policy, so
 *		this function deals with that case unconditionally.  If
 *		we ever support other replacement policies, the function
 *		should switch on the current policy and act accordingly.
 *
 * Return:      N/A
 *
 * Programmer:  John Mainzer, 10/10/18
 *
 * Modifications:
 *
 *              None.
 *
 *-------------------------------------------------------------------------
 */

#define H5PB__UPDATE_RP_FOR_INSERTION(page_buf, entry_ptr, fail_val)                                         \
    {                                                                                                        \
        HDassert((page_buf));                                                                                \
        HDassert((page_buf)->magic == H5PB__H5PB_T_MAGIC);                                                   \
        HDassert((entry_ptr));                                                                               \
        HDassert((entry_ptr)->magic == H5PB__H5PB_ENTRY_T_MAGIC);                                            \
        HDassert((entry_ptr)->size >= page_buf->page_size);                                                  \
                                                                                                             \
        /* modified LRU specific code */                                                                     \
                                                                                                             \
        /* insert the entry at the head of the LRU list. */                                                  \
                                                                                                             \
        H5PB__DLL_PREPEND((entry_ptr), (page_buf)->LRU_head_ptr, (page_buf)->LRU_tail_ptr,                   \
                          (page_buf)->LRU_len, (page_buf)->LRU_size, (fail_val))                             \
                                                                                                             \
        H5PB__UPDATE_LRU_SIZE_STATS(page_buf)                                                                \
                                                                                                             \
        /* End modified LRU specific code. */                                                                \
    }

/***********************************************************************
 *
 * Tick list management macros
 *
 * When the target file is opened in VFD SWMR writer mode, the page
 * buffer must retain copies of all metadata writes during each tick so
 * that the metadata file can be updated correctly in end of tick
 * processing.
 *
 * Once tick processing is complete, all entries are removed from the
 * tick list, to leave it empty for the next tick.  Metadata pages from
 * the tick list are already in the replacement policy, and thus require
 * no further action.
 *
 * Multi-page metadata entries are evicted from the page buffer if they
 * are not subject to delayed write, or left in the delayed write list
 * for later flush and eviction if they are.
 *
 * The macros required to support this are defined below.
 *
 *                                              JRM -- 10/09/18
 *
 ***********************************************************************/

/*-------------------------------------------------------------------------
 *
 * Macro:	H5PB__INSERT_IN_TL
 *
 * Purpose:     Insert the specified page buffer entry at the head of the
 *              tick list.
 *
 * Return:      N/A
 *
 * Programmer:  John Mainzer, 10/10/18
 *
 * Modifications:
 *
 *              None.
 *
 *-------------------------------------------------------------------------
 */

#define H5PB__INSERT_IN_TL(page_buf, entry_ptr, fail_val)                                                    \
    {                                                                                                        \
        HDassert((page_buf));                                                                                \
        HDassert((page_buf)->magic == H5PB__H5PB_T_MAGIC);                                                   \
        HDassert((page_buf)->vfd_swmr_writer);                                                               \
        HDassert((entry_ptr));                                                                               \
        HDassert((entry_ptr)->magic == H5PB__H5PB_ENTRY_T_MAGIC);                                            \
        HDassert((entry_ptr)->modified_this_tick);                                                           \
        HDassert((entry_ptr)->size >= page_buf->page_size);                                                  \
                                                                                                             \
        /* insert the entry at the head of the tick list. */                                                 \
                                                                                                             \
        H5PB__TL_DLL_PREPEND((entry_ptr), (page_buf)->tl_head_ptr, (page_buf)->tl_tail_ptr,                  \
                             (page_buf)->tl_len, (page_buf)->tl_size, (fail_val))                            \
                                                                                                             \
        H5PB__UPDATE_TL_SIZE_STATS(page_buf)                                                                 \
                                                                                                             \
    } /* H5PB__INSERT_IN_TL */

/*-------------------------------------------------------------------------
 *
 * Macro:	H5PB__REMOVE_FROM_TL
 *
 * Purpose:     Remove the specified page buffer entry from the tick list.
 *
 * Return:      N/A
 *
 * Programmer:  John Mainzer, 10/10/18
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

#define H5PB__REMOVE_FROM_TL(page_buf, entry_ptr, fail_val)                                                  \
    {                                                                                                        \
        HDassert((page_buf));                                                                                \
        HDassert((page_buf)->magic == H5PB__H5PB_T_MAGIC);                                                   \
        HDassert((page_buf)->vfd_swmr_writer);                                                               \
        HDassert((entry_ptr));                                                                               \
        HDassert((entry_ptr)->magic == H5PB__H5PB_ENTRY_T_MAGIC);                                            \
        HDassert((entry_ptr)->modified_this_tick);                                                           \
        HDassert((entry_ptr)->size >= page_buf->page_size);                                                  \
                                                                                                             \
        /* remove the entry from the tick list. */                                                           \
                                                                                                             \
        H5PB__TL_DLL_REMOVE((entry_ptr), (page_buf)->tl_head_ptr, (page_buf)->tl_tail_ptr,                   \
                            (page_buf)->tl_len, (page_buf)->tl_size, (fail_val))                             \
                                                                                                             \
    } /* H5PB__REMOVE_FROM_TL */

/***********************************************************************
 *
 * Delayed write list management macros
 *
 * When the target file is opened in VFD SWMR writer mode, the page
 * buffer must delay flush of all metadata pages and multi-page metadata
 * entries that:
 *
 * 1) have not appeared in the metadata file index for at least max_lag
 *    ticks, and
 *
 * 2) a previous version of the metadata page or multi-page metadata
 *    cache entry exists in the file.
 *
 * Failure to do so can result in VFD SWMR readers to receive messages
 * from the future.
 *
 * To minimize overhead, the delayed write list is sorted in decreasing
 * values of the constituent delay_write_until fields.
 *
 * Entries are removed from the delayed write list when their
 * delay_write_until fields are satisfied.  Metadata pages are inserted
 * at the bottom of the replacement policy, and multi-page metadata
 * entries are immediately flushed and evicted.
 *
 * The macros required to support this are defined below.
 *
 *                                              JRM -- 10/09/18
 *
 ***********************************************************************/

/*-------------------------------------------------------------------------
 *
 * Macro:	H5PB__INSERT_IN_DWL
 *
 * Insert the supplied page buffer entry in the delayed write list
 * maintaining the invarient:
 *
 *    entry_ptr->next == NULL ||
 *    entry_ptr->delay_write_until >= entry_ptr->next->delay_write_until
 *
 * In passing update page_buf->max_delay if appropriate.
 *
 * Return:      N/A
 *
 * Programmer:  John Mainzer, 10/10/18
 *
 * Modifications:
 *
 *              None.
 *
 *-------------------------------------------------------------------------
 */

#define H5PB__INSERT_IN_DWL(page_buf, entry_ptr, fail_val)                                                   \
    {                                                                                                        \
        int           insertion_depth = 0;                                                                   \
        uint64_t      delay;                                                                                 \
        H5PB_entry_t *suc_ptr;                                                                               \
                                                                                                             \
        HDassert((page_buf));                                                                                \
        HDassert((page_buf)->magic == H5PB__H5PB_T_MAGIC);                                                   \
        HDassert((page_buf)->vfd_swmr_writer);                                                               \
        HDassert((entry_ptr));                                                                               \
        HDassert((entry_ptr)->magic == H5PB__H5PB_ENTRY_T_MAGIC);                                            \
        HDassert((entry_ptr)->size >= page_buf->page_size);                                                  \
        HDassert((entry_ptr)->delay_write_until > (page_buf)->cur_tick);                                     \
                                                                                                             \
        delay   = (entry_ptr)->delay_write_until - (page_buf)->cur_tick;                                     \
        suc_ptr = page_buf->dwl_head_ptr;                                                                    \
                                                                                                             \
        while ((suc_ptr) && ((suc_ptr)->delay_write_until > (entry_ptr)->delay_write_until)) {               \
            insertion_depth++;                                                                               \
            suc_ptr = suc_ptr->next;                                                                         \
        }                                                                                                    \
                                                                                                             \
        H5PB__DLL_INSERT_BEFORE((entry_ptr), (suc_ptr), (page_buf)->dwl_head_ptr, (page_buf)->dwl_tail_ptr,  \
                                (page_buf)->dwl_len, (page_buf)->dwl_size, (fail_val))                       \
                                                                                                             \
        if (entry_ptr->delay_write_until > page_buf->max_delay)                                              \
            page_buf->max_delay = entry_ptr->delay_write_until;                                              \
                                                                                                             \
        H5PB__UPDATE_DWL_SIZE_STATS(page_buf)                                                                \
        H5PB__UPDATE_DWL_DELAYED_WRITES(page_buf, insertion_depth, delay)                                    \
                                                                                                             \
    } /* H5PB__INSERT_IN_DWL */

/*-------------------------------------------------------------------------
 *
 * Macro:	H5PB__REMOVE_FROM_DWL
 *
 * Purpose:     Remove the specified page buffer entry from the delayed
 *              write list.
 *
 * Return:      N/A
 *
 * Programmer:  John Mainzer, 10/10/18
 *
 * Modifications:
 *
 *              None.
 *
 *-------------------------------------------------------------------------
 */

#define H5PB__REMOVE_FROM_DWL(page_buf, entry_ptr, fail_val)                                                 \
    {                                                                                                        \
        HDassert((page_buf));                                                                                \
        HDassert((page_buf)->magic == H5PB__H5PB_T_MAGIC);                                                   \
        HDassert((page_buf)->vfd_swmr_writer);                                                               \
        HDassert((entry_ptr));                                                                               \
        HDassert((entry_ptr)->magic == H5PB__H5PB_ENTRY_T_MAGIC);                                            \
        HDassert((entry_ptr)->size >= page_buf->page_size);                                                  \
        HDassert((entry_ptr)->delay_write_until == 0);                                                       \
                                                                                                             \
        /* remove the entry from the delayed write list. */                                                  \
                                                                                                             \
        H5PB__DLL_REMOVE((entry_ptr), (page_buf)->dwl_head_ptr, (page_buf)->dwl_tail_ptr,                    \
                         (page_buf)->dwl_len, (page_buf)->dwl_size, (fail_val))                              \
                                                                                                             \
    } /* H5PB__REMOVE_FROM_DWLL */

/****************************/
/* Package Private Typedefs */
/****************************/

/****************************************************************************
 *
 * structure H5PB_entry_t
 *
 * Individual instances of the H5PB_entry_t structure are used to manage
 * individual pages in the page buffer.  In the case of a VFD SWMR writer,
 * they are also used to manage multi-page metadata entries.
 *
 * The fields of this structure are discussed below:
 *
 *                                              JRM - 9/27/18
 *
 * magic:       Unsigned 32 bit integer that must always be set to
 *              H5PB__H5PB_ENTRY_T_MAGIC when the entry is valid.
 *
 * page_buf:      Pointer to the page buffer that contains this entry.
 *
 * addr:        Base address of the page in the file.
 *
 * page:        Page offset of the page -- i.e. addr / page_buf->page_size.
 *              Note that addr must always equal page * page_buf->page_size.
 *
 * size:        Size of the page buffer entry in bytes.  Under normal
 *              circumstance, this will always be equal to page_buf->page_size.
 *              However, in the context of a VFD SWMR writer, the page
 *              buffer may be used to store multi-page metadata entries
 *              until the end of tick, or to delay writes of such entries
 *              for up to max_lag ticks.
 *
 *              In such cases, size must be greater than page_buf->page_size.
 *
 * image_ptr:   Pointer to void.  When not NULL, this field points to a
 *              dynamically allocated block of size bytes in which the
 *              on disk image of the page.  In the context of VFD SWMR,
 *              it points to the image of the multi-page metadata entry.
 *
 * mem_type:    Type (H5F_mem_t) of the page buffer entry.  This value
 *              is needed when reading or writing the entry from/to file.
 *
 * is_metadata:  Boolean flag that is set to TRUE iff the associated
 *              entry is a page of metadata (or, in the context of VFD
 *              SWMR, a multi-page metadata entry).
 *
 * is_dirty:    Boolean flag indicating whether the contents of the page
 *              buffer entry has been modified since the last time it
 *              was written to disk.
 *
 *
 * Fields supporting the hash table:
 *
 * Entries in the page buffer are indexed by a more or less conventional
 * hash table with chaining (see header comment on H5PB_t for futher details).
 * If there are multiple entries in any hash bin, they are stored in a doubly
 * linked list.
 *
 * To facilitate flushing the page buffer, we also maintain a doubly linked
 * list of all entries in the page buffer.
 *
 * ht_next:     Next pointer used by the hash table to store multiple
 *              entries in a single hash bin.  This field points to the
 *              next entry in the doubly linked list of entries in the
 *              hash bin, or NULL if there is no next entry.
 *
 * ht_prev:     Prev pointer used by the hash table to store multiple
 *              entries in a single hash bin.  This field points to the
 *              previous entry in the doubly linked list of entries in
 *              the hash bin, or NULL if there is no previuos entry.
 *
 * il_next:     Next pointer used by the index to maintain a doubly linked
 *              list of all entries in the index (and thus in the page buffer).
 *              This field contains a pointer to the next entry in the
 *              index list, or NULL if there is no next entry.
 *
 * il_prev:     Prev pointer used by the index to maintain a doubly linked
 *              list of all entries in the index (and thus in the page buffer).
 *              This field contains a pointer to the previous entry in the
 *              index list, or NULL if there is no previous entry.
 *
 *
 * Fields supporting replacement policies:
 *
 * The page buffer must have a replacement policy, and it will usually be
 * necessary for this structure to contain fields supporting that policy.
 *
 * At present, only a modified LRU replacement policy is contemplated,
 * (see header comment for H5PB_t for details), for which the following
 * fields are adequate.
 *
 * next:        Next pointer in either the LRU, or (in the context of
 *              VFD SWMR) the delayed write list.  If there is no next entry
 *              on the list, this field should be set to NULL.
 *
 * prev:        Prev pointer in either the LRU, or (in the context of
 *              VFD SWMR) the delayed write list.  If there is no previous
 *              entry on the list, this field should be set to NULL.
 *
 * Fields supporting VFD SWMR:
 *
 * is_mpmde:    Boolean flag that is set to TRUE iff the entry
 *              is a multi-page metadata entry.  In the absense of VFD
 *              SWMR, the field should always be set to FALSE.
 *
 *              Observe that:
 *
 *              is_mpmde <==> is_metadata && size > page_buf->page_size
 *
 * loaded:      Boolean flag that is set to TRUE iff the entry was loaded
 *              from file.  This is a necessary input in determining
 *              whether the write of the entry must be delayed.
 *
 *              This field is only maintained in the VFD SWMR case
 *              and should be false otherwise.
 *
 * modified_this_tick:  This field is set to TRUE iff page_buf->vfd_swrm_write
 *              and the entry has been modified in the current tick.  If
 *              modified_this_tick is TRUE, the entry must also be in the
 *              tick list.
 *
 * delay_write_until: Unsigned 64 bit integer containing the first tick
 *              in which the entry may be written to file, or 0 if there
 *              is no such constraint.  It should be set ot 0 when VFD
 *              is not enabled.
 *
 * tl_next:     Next pointer on the list of entries modified in the current
 *              tick,  If the enty is not on the tick list, or if there is
 *              no next entry on the list, this field should be set to NULL.
 *
 * tl_prev:     Prev pointer on the list of entries modified in the current
 *              tick,  If the enty is not on the tick list, or if there is
 *              no previous entry on the list, this field should be set to
 *              NULL.
 *
 ****************************************************************************/

#define H5PB__H5PB_ENTRY_T_MAGIC 0x02030405

struct H5PB_entry_t {

    uint32_t   magic;
    H5PB_t *   page_buf;
    haddr_t    addr;
    uint64_t   page;
    size_t     size;
    void *     image_ptr;
    H5FD_mem_t mem_type;
    hbool_t    is_metadata;
    hbool_t    is_dirty;

    /* fields supporting the hash table: */
    struct H5PB_entry_t *ht_next;
    struct H5PB_entry_t *ht_prev;
    struct H5PB_entry_t *il_next;
    struct H5PB_entry_t *il_prev;

    /* fields supporting replacement policies: */
    struct H5PB_entry_t *next;
    struct H5PB_entry_t *prev;

    /* fields supporting VFD SWMR */
    hbool_t              is_mpmde;
    hbool_t              loaded;
    hbool_t              modified_this_tick;
    uint64_t             delay_write_until;
    struct H5PB_entry_t *tl_next;
    struct H5PB_entry_t *tl_prev;

}; /* H5PB_entry_t */

/*****************************/
/* Package Private Variables */
/*****************************/

/******************************/
/* Package Private Prototypes */
/******************************/

#endif /* H5PBpkg_H */
