/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:             H5AClog_json.c
 *
 * Purpose:             Functions for metadata cache logging in JSON format
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/
#define H5AC_PACKAGE            /* suppress error about including H5ACpkg  */
#define H5C_PACKAGE             /* suppress error about including H5Cpkg   */

/***********/
/* Headers */
/***********/
#include "H5private.h"          /* Generic Functions                    */
#include "H5ACpkg.h"            /* Metadata cache                       */
#include "H5Cpkg.h"             /* Cache                                */
#include "H5Eprivate.h"         /* Error handling                       */

/****************/
/* Local Macros */
/****************/

#define MSG_SIZE 128

/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/


/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/


/*-------------------------------------------------------------------------
 * Function:    H5AC__write_create_cache_log_msg
 *
 * Purpose:     Write a log message for cache creation.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 *-------------------------------------------------------------------------
 */



/*-------------------------------------------------------------------------
 * Function:    H5AC__write_create_cache_log_msg
 *
 * Purpose:     Write a log message for cache creation.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC__write_create_cache_log_msg(H5AC_t *cache) 
{
    char msg[MSG_SIZE];
    hbool_t orig_state;            /* saved "current logging" flag state */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(cache);
    HDassert(cache->magic == H5C__H5C_T_MAGIC);

    /* Create the log message string */
    snprintf(msg, MSG_SIZE, 
"\
{\n\
\"create_time\":%lld,\n\
\"messages\":\n\
[\n\
"
    , (long long)time(NULL));

    /* Since we're about to override the current logging flag,
     * check the "log enabled" flag to see if we didn't get here
     * by mistake.
     */
    if(!(cache->logging_enabled))
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "attempt to write opening log message when logging is disabled")

    /* Write the log message to the file
     * Have to temporarily enable logging for this.
     */
    orig_state = cache->currently_logging;
    cache->currently_logging = TRUE;
    if(H5C_write_log_message(cache, msg) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to emit log message")
    cache->currently_logging = orig_state;

done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__write_create_cache_log_msg() */


/*-------------------------------------------------------------------------
 * Function:    H5AC__write_destroy_cache_log_msg
 *
 * Purpose:     Write a log message for cache destruction.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC__write_destroy_cache_log_msg(H5AC_t *cache)
{
    char msg[MSG_SIZE];
    hbool_t orig_state;            /* saved "current logging" flag state */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(cache);
    HDassert(cache->magic == H5C__H5C_T_MAGIC);

    /* Create the log message string */
    snprintf(msg, MSG_SIZE, 
"\
],\n\
\"close_time\":%lld,\n\
}\n\
"
    , (long long)time(NULL));

    /* Since we're about to override the current logging flag,
     * check the "log enabled" flag to see if we didn't get here
     * by mistake.
     */
    if(!(cache->logging_enabled))
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "attempt to write closing log message when logging is disabled")

    /* Write the log message to the file
     * Have to temporarily enable logging for this.
     */
    orig_state = cache->currently_logging;
    cache->currently_logging = TRUE;
    if(H5C_write_log_message(cache, msg) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to emit log message")
    cache->currently_logging = orig_state;

done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__write_destroy_cache_log_msg() */


/*-------------------------------------------------------------------------
 * Function:    H5AC__write_evict_cache_log_msg
 *
 * Purpose:     Write a log message for eviction of cache entries.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC__write_evict_cache_log_msg(const H5AC_t *cache,
                                herr_t fxn_ret_value)
{
    char msg[MSG_SIZE];
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(cache);
    HDassert(cache->magic == H5C__H5C_T_MAGIC);

    /* Create the log message string */
    snprintf(msg, MSG_SIZE, 
"\
{\
\"timestamp\":%lld,\
\"action\":\"evict\",\
\"returned\":%d\
},\n\
"
    , (long long)time(NULL), (int)fxn_ret_value);

    /* Write the log message to the file */
    if(H5C_write_log_message(cache, msg) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to emit log message")

done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__write_evict_cache_log_msg() */


/*-------------------------------------------------------------------------
 * Function:    H5AC__write_expunge_entry_log_msg
 *
 * Purpose:     Write a log message for expunge of cache entries.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC__write_expunge_entry_log_msg(const H5AC_t *cache,
                                  haddr_t address,
                                  int type_id,
                                  herr_t fxn_ret_value)
{
    char msg[MSG_SIZE];
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(cache);
    HDassert(cache->magic == H5C__H5C_T_MAGIC);

    /* Create the log message string */
    snprintf(msg, MSG_SIZE, 
"\
{\
\"timestamp\":%lld,\
\"action\":\"expunge\",\
\"address\":0x%lx,\
\"type_id\":%d,\
\"returned\":%d\
},\n\
"
    , (long long)time(NULL), (unsigned long)address, (int)type_id, (int)fxn_ret_value);


    /* Write the log message to the file */
    if(H5C_write_log_message(cache, msg) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to emit log message")

done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__write_expunge_entry_log_msg() */


/*-------------------------------------------------------------------------
 * Function:    H5AC__write_flush_cache_log_msg
 *
 * Purpose:     Write a log message for cache flushes.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC__write_flush_cache_log_msg(const H5AC_t *cache,
                                herr_t fxn_ret_value)
{
    char msg[MSG_SIZE];
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(cache);
    HDassert(cache->magic == H5C__H5C_T_MAGIC);

    /* Create the log message string */
    snprintf(msg, MSG_SIZE, 
"\
{\
\"timestamp\":%lld,\
\"action\":\"flush\",\
\"returned\":%d\
},\n\
"
    , (long long)time(NULL), (int)fxn_ret_value);

    /* Write the log message to the file */
    if(H5C_write_log_message(cache, msg) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to emit log message")

done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__write_flush_cache_log_msg() */


/*-------------------------------------------------------------------------
 * Function:    H5AC__write_insert_entry_log_msg
 *
 * Purpose:     Write a log message for insertion of cache entries.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC__write_insert_entry_log_msg(const H5AC_t *cache,
                                 haddr_t address,
                                 int type_id,
                                 unsigned flags,
                                 size_t size,
                                 herr_t fxn_ret_value)
{
    char msg[MSG_SIZE];
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(cache);
    HDassert(cache->magic == H5C__H5C_T_MAGIC);


    /* Create the log message string */
    snprintf(msg, MSG_SIZE, 
"\
{\
\"timestamp\":%lld,\
\"action\":\"insert\",\
\"address\":0x%lx,\
\"flags\":0x%x,\
\"type_id\":%d,\
\"size\":%d,\
\"returned\":%d\
},\n\
"
    , (long long)time(NULL), (unsigned long)address, flags, type_id, 
      (int)size, (int)fxn_ret_value);

    /* Write the log message to the file */
    if(H5C_write_log_message(cache, msg) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to emit log message")

done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__write_insert_entry_log_msg() */


/*-------------------------------------------------------------------------
 * Function:    H5AC__write_mark_dirty_entry_log_msg
 *
 * Purpose:     Write a log message for marking cache entries as dirty.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC__write_mark_dirty_entry_log_msg(const H5AC_t *cache,
                                     const H5AC_info_t *entry,
                                     herr_t fxn_ret_value)
{
    char msg[MSG_SIZE];
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(cache);
    HDassert(cache->magic == H5C__H5C_T_MAGIC);
    HDassert(entry);

    /* Create the log message string */
    snprintf(msg, MSG_SIZE, 
"\
{\
\"timestamp\":%lld,\
\"action\":\"dirty\",\
\"address\":0x%lx,\
\"returned\":%d\
},\n\
"
    , (long long)time(NULL), (unsigned long)entry->addr, (int)fxn_ret_value);

    /* Write the log message to the file */
    if(H5C_write_log_message(cache, msg) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to emit log message")

done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__write_mark_dirty_entry_log_msg() */


/*-------------------------------------------------------------------------
 * Function:    H5AC__write_move_entry_log_msg
 *
 * Purpose:     Write a log message for moving a cache entry.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC__write_move_entry_log_msg(const H5AC_t *cache,
                               haddr_t old_addr,
                               haddr_t new_addr,
                               int type_id,
                               herr_t fxn_ret_value)
{
    char msg[MSG_SIZE];
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(cache);
    HDassert(cache->magic == H5C__H5C_T_MAGIC);

    /* Create the log message string */
    snprintf(msg, MSG_SIZE, 
"\
{\
\"timestamp\":%lld,\
\"action\":\"move\",\
\"old_address\":0x%lx,\
\"new_address\":0x%lx,\
\"type_id\":%d,\
\"returned\":%d\
},\n\
"
    , (long long)time(NULL), (unsigned long)old_addr, 
      (unsigned long)new_addr, type_id, (int)fxn_ret_value);

    /* Write the log message to the file */
    if(H5C_write_log_message(cache, msg) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to emit log message")

done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__write_move_entry_log_msg() */


/*-------------------------------------------------------------------------
 * Function:    H5AC__write_pin_entry_log_msg
 *
 * Purpose:     Write a log message for pinning a cache entry.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC__write_pin_entry_log_msg(const H5AC_t *cache,
                              const H5AC_info_t *entry,
                              herr_t fxn_ret_value)
{
    char msg[MSG_SIZE];
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(cache);
    HDassert(cache->magic == H5C__H5C_T_MAGIC);
    HDassert(entry);

    /* Create the log message string */
    snprintf(msg, MSG_SIZE, 
"\
{\
\"timestamp\":%lld,\
\"action\":\"pin\",\
\"address\":0x%lx,\
\"returned\":%d\
},\n\
"
    , (long long)time(NULL), (unsigned long)entry->addr, 
      (int)fxn_ret_value);

    /* Write the log message to the file */
    if(H5C_write_log_message(cache, msg) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to emit log message")

done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__write_pin_entry_log_msg() */


/*-------------------------------------------------------------------------
 * Function:    H5AC__write_create_fd_log_msg
 *
 * Purpose:     Write a log message for creating a flush dependency between
 *              two cache entries.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC__write_create_fd_log_msg(const H5AC_t *cache,
                              const H5AC_info_t *parent,
                              const H5AC_info_t *child,
                              herr_t fxn_ret_value)
{
    char msg[MSG_SIZE];
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(cache);
    HDassert(cache->magic == H5C__H5C_T_MAGIC);
    HDassert(parent);
    HDassert(child);

    /* Create the log message string */
    snprintf(msg, MSG_SIZE, 
"\
{\
\"timestamp\":%lld,\
\"action\":\"create_fd\",\
\"parent_addr\":0x%lx,\
\"child_addr\":0x%lx,\
\"returned\":%d\
},\n\
"
    , (long long)time(NULL), (unsigned long)parent->addr, 
      (unsigned long)child->addr, (int)fxn_ret_value);

    /* Write the log message to the file */
    if(H5C_write_log_message(cache, msg) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to emit log message")

done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__write_create_fd_log_msg() */


/*-------------------------------------------------------------------------
 * Function:    H5AC__write_protect_entry_log_msg
 *
 * Purpose:     Write a log message for protecting a cache entry.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC__write_protect_entry_log_msg(const H5AC_t *cache,
                                  const H5AC_info_t *entry,
                                  H5AC_protect_t rw,
                                  herr_t fxn_ret_value)
{
    char msg[MSG_SIZE];
    char rw_s[16];
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(cache);
    HDassert(cache->magic == H5C__H5C_T_MAGIC);
    HDassert(entry);

    if(H5AC_READ == rw)
        HDstrcpy(rw_s, "READ");
    else if(H5AC_WRITE == rw)
        HDstrcpy(rw_s, "WRITE");
    else
        HDstrcpy(rw_s, "UNKNOWN");

    /* Create the log message string */
    snprintf(msg, MSG_SIZE, 
"\
{\
\"timestamp\":%lld,\
\"action\":\"protect\",\
\"address\":0x%lx,\
\"readwrite\":\"%s\",\
\"size\":%d,\
\"returned\":%d\
},\n\
"
    , (long long)time(NULL), (unsigned long)entry->addr, 
      rw_s, (int)entry->size, (int)fxn_ret_value);

    /* Write the log message to the file */
    if(H5C_write_log_message(cache, msg) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to emit log message")

done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__write_protect_entry_log_msg() */


/*-------------------------------------------------------------------------
 * Function:    H5AC__write_resize_entry_log_msg
 *
 * Purpose:     Write a log message for resizing a cache entry.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC__write_resize_entry_log_msg(const H5AC_t *cache,
                                 const H5AC_info_t *entry,
                                 size_t new_size,
                                 herr_t fxn_ret_value)
{
    char msg[MSG_SIZE];
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(cache);
    HDassert(cache->magic == H5C__H5C_T_MAGIC);
    HDassert(entry);

    /* Create the log message string */
    snprintf(msg, MSG_SIZE, 
"\
{\
\"timestamp\":%lld,\
\"action\":\"resize\",\
\"address\":0x%lx,\
\"new_size\":%d,\
\"returned\":%d\
},\n\
"
    , (long long)time(NULL), (unsigned long)entry->addr, 
      (int)new_size, (int)fxn_ret_value);

    /* Write the log message to the file */
    if(H5C_write_log_message(cache, msg) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to emit log message")

done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__write_resize_entry_log_msg() */


/*-------------------------------------------------------------------------
 * Function:    H5AC__write_unpin_entry_log_msg
 *
 * Purpose:     Write a log message for unpinning a cache entry.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC__write_unpin_entry_log_msg(const H5AC_t *cache,
                                const H5AC_info_t *entry,
                                herr_t fxn_ret_value)
{
    char msg[MSG_SIZE];
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(cache);
    HDassert(cache->magic == H5C__H5C_T_MAGIC);
    HDassert(entry);

    /* Create the log message string */
    snprintf(msg, MSG_SIZE, 
"\
{\
\"timestamp\":%lld,\
\"action\":\"unpin\",\
\"address\":0x%lx,\
\"returned\":%d\
},\n\
"
    , (long long)time(NULL), (unsigned long)entry->addr, 
      (int)fxn_ret_value);

    /* Write the log message to the file */
    if(H5C_write_log_message(cache, msg) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to emit log message")

done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__write_unpin_entry_log_msg() */


/*-------------------------------------------------------------------------
 * Function:    H5AC__write_destroy_fd_log_msg
 *
 * Purpose:     Write a log message for destroying a flush dependency
 *              between two cache entries.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC__write_destroy_fd_log_msg(const H5AC_t *cache,
                               const H5AC_info_t *parent,
                               const H5AC_info_t *child,
                               herr_t fxn_ret_value)
{
    char msg[MSG_SIZE];
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(cache);
    HDassert(cache->magic == H5C__H5C_T_MAGIC);
    HDassert(parent);
    HDassert(child);

    /* Create the log message string */
    snprintf(msg, MSG_SIZE, 
"\
{\
\"timestamp\":%lld,\
\"action\":\"destroy_fd\",\
\"parent_addr\":0x%lx,\
\"child_addr\":0x%lx,\
\"returned\":%d\
},\n\
"
    , (long long)time(NULL), (unsigned long)parent->addr, 
      (unsigned long)child->addr, (int)fxn_ret_value);

    /* Write the log message to the file */
    if(H5C_write_log_message(cache, msg) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to emit log message")

done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__write_destroy_fd_log_msg() */


/*-------------------------------------------------------------------------
 * Function:    H5AC__write_unprotect_entry_log_msg
 *
 * Purpose:     Write a log message for unprotecting a cache entry.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC__write_unprotect_entry_log_msg(const H5AC_t *cache,
                                    const H5AC_info_t *entry,
                                    int type_id,
                                    unsigned flags,
                                    herr_t fxn_ret_value)
{
    char msg[MSG_SIZE];
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(cache);
    HDassert(cache->magic == H5C__H5C_T_MAGIC);
    HDassert(entry);

    /* Create the log message string */
    snprintf(msg, MSG_SIZE, 
"\
{\
\"timestamp\":%lld,\
\"action\":\"unprotect\",\
\"address\":0x%lx,\
\"id\":%d,\
\"flags\":%x,\
\"returned\":%d\
},\n\
"
    , (long long)time(NULL), (unsigned long)entry->addr, 
      type_id, flags, (int)fxn_ret_value);

    snprintf(msg, MSG_SIZE, " ");

    /* Write the log message to the file */
    if(H5C_write_log_message(cache, msg) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to emit log message")

done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__write_unprotect_entry_log_msg() */


/*-------------------------------------------------------------------------
 * Function:    H5AC__write_set_cache_config_log_msg
 *
 * Purpose:     Write a log message for setting the cache configuration.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5AC__write_set_cache_config_log_msg(const H5AC_t *cache,
                                     const H5AC_cache_config_t *config,
                                     herr_t fxn_ret_value)
{
    char msg[MSG_SIZE];
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(cache);
    HDassert(cache->magic == H5C__H5C_T_MAGIC);
    HDassert(config);

    /* Create the log message string */
    snprintf(msg, MSG_SIZE, 
"\
{\
\"timestamp\":%lld,\
\"action\":\"set_config\",\
\"returned\":%d\
},\n\
"
    , (long long)time(NULL), (int)fxn_ret_value);


    /* Write the log message to the file */
    if(H5C_write_log_message(cache, msg) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "unable to emit log message")

done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5AC__write_set_cache_config_log_msg() */

