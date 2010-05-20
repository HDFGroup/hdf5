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
 * Created:     H5Cjournal.c
 *              Dec 6 2007
 *              John Mainzer
 *
 * Purpose:     This file is a general catchall for functions supporting
 *              metadata journaling.  Note that journaling must be tighly
 *              integrated with the metadata cache, and thus this file only
 *              contains only that code that can be easily separated from 
 *              the rest of the cache code.
 *
 *              Observe also that to minimize overhead, it is quite possible
 *              that many of the functions in this file will be converted
 *              into macros at some point in the future.  
 *
 * Modifications:
 *
 *              None.
 *
 *-------------------------------------------------------------------------
 */

#define H5F_PACKAGE             /* suppress error about including H5Fpkg  */
#define H5C_PACKAGE             /* suppress error about including H5Cpkg */

#include "H5private.h"          /* Generic Functions                    */
#include "H5Cpkg.h"             /* Cache                                */
#include "H5Eprivate.h"         /* Error handling                       */
#include "H5Fpkg.h"		/* File access                          */
#include "H5MFprivate.h"        /* File memory management               */
#include "H5MMprivate.h"        /* Memory management                    */
#include "H5Pprivate.h"		/* Property lists			*/

/**************************************************************************/
/***************************** global variables ***************************/
/**************************************************************************/

/* In the test code, it is sometimes useful to skip the check for journaling
 * in progress on open.  The check_for_journaling global is used to support
 * this.  Note that we can't tuck this variable into H5C_t, as the test
 * takes place before H5Fopen() returns.
 */

hbool_t H5C__check_for_journaling = TRUE;

/**************************************************************************/
/******************************* local macros *****************************/
/**************************************************************************/
#define H5C__TRANS_NUM_SIZE 8
#define H5C__CHECKSUM_SIZE  4

/**************************************************************************/
/***************************** local prototypes ***************************/
/**************************************************************************/

static herr_t H5C_call_mdjsc_callbacks(H5C_t * cache_ptr,
                                        hid_t dxpl_id,
                                        H5C_mdj_config_t * config_ptr);

static herr_t H5C_get_journaling_in_progress(const H5F_t * f,
					      H5C_t * cache_ptr);

static herr_t H5C_grow_mdjsc_callback_table(H5C_t * cache_ptr);

static herr_t H5C_jb_aio__await_buffer_write_completion(
						H5C_jbrb_t * struct_ptr,
                                               int buf_num);

static herr_t H5C_jb_aio__await_async_fsync_completion(
						H5C_jbrb_t * struct_ptr);

static herr_t H5C_jb_aio__flush(H5C_jbrb_t * struct_ptr);

static herr_t H5C_jb_aio__get_last_transaction_on_disk(
					H5C_jbrb_t * struct_ptr,
                                        uint64_t * trans_num_ptr);

static herr_t H5C_jb_aio__make_space_in_ring_buffer(H5C_jbrb_t * struct_ptr);

static herr_t H5C_jb_aio__note_completed_async_buffer_writes(
						H5C_jbrb_t * struct_ptr);

static herr_t H5C_jb_aio__note_completed_async_fsyncs(
						H5C_jbrb_t * struct_ptr);

static herr_t H5C_jb_aio__prep_next_buf_for_use(H5C_jbrb_t * struct_ptr,
                                            uint64_t last_trans_in_ring_buffer);

static herr_t H5C_jb_aio__queue_async_fsync(H5C_jbrb_t * struct_ptr);

static herr_t H5C_jb_aio__queue_buffer_write(H5C_jbrb_t * struct_ptr,
                                              int buf_num,
                                              hbool_t partial_write_ok);

static herr_t H5C_jb_aio__sync_file(H5C_jbrb_t * struct_ptr);

static herr_t H5C_jb_aio__sync_q__append(H5C_jbrb_t * struct_ptr,
                               struct H5C_jbrb_sync_q_entry_t * entry_ptr);

static herr_t H5C_jb_aio__sync_q__discard_head(H5C_jbrb_t * struct_ptr);

static herr_t H5C_jb_aio__test_buffer_write_complete(H5C_jbrb_t * struct_ptr,
                                                      int buf_num,
                                                      hbool_t *complete_ptr);

static herr_t H5C_jb_aio__test_next_async_fsync_complete(
						H5C_jbrb_t * struct_ptr,
                                                hbool_t *sync_complete_ptr);

herr_t H5C_jb_aio__write_to_buffer(H5C_jbrb_t * struct_ptr,	
                                    size_t size,			
                                    const char * data,
                                    hbool_t is_end_trans,
                                    uint64_t trans_num);

static herr_t H5C_jb_bjf__comment(H5C_jbrb_t * struct_ptr,
                                   const char * comment_ptr);

static herr_t H5C_jb_bjf__end_transaction(H5C_jbrb_t * struct_ptr,
                                           uint64_t trans_num);

static herr_t H5C_jb_bjf__eoa(H5C_jbrb_t * struct_ptr,
                               haddr_t eoa);

static herr_t H5C_jb_bjf__journal_entry(H5C_jbrb_t * struct_ptr,
                                         uint64_t trans_num,
                                         haddr_t base_addr,
                                         size_t length,
                                         const uint8_t * body);

static herr_t H5C_jb_bjf__start_transaction(H5C_jbrb_t * struct_ptr,
                                             uint64_t trans_num);

static herr_t H5C_jb_bjf__write_buffer(H5C_jbrb_t * struct_ptr,
                                        size_t buf_size,
                                        const char * buf_ptr,
                                        hbool_t is_end_trans,
                                        uint64_t trans_num);

static herr_t H5C_jb_bjf__write_chksum(H5C_jbrb_t * struct_ptr,
                                        hbool_t is_end_trans,
                                        uint64_t trans_num);

static herr_t H5C_jb_bjf__write_length(H5C_jbrb_t * struct_ptr,
                                        size_t length,
                                        hbool_t is_end_trans, 
                                        uint64_t trans_num);

static herr_t H5C_jb_bjf__write_offset(H5C_jbrb_t * struct_ptr,
                                        haddr_t offset,
                                        hbool_t is_end_trans,
                                        uint64_t trans_num);

static herr_t H5C_jb_bjf__write_sig_and_ver(H5C_jbrb_t * struct_ptr,
                                             const char * sig_ptr,
                                             const uint8_t version,
                                             hbool_t keep_chksum,
                                             hbool_t is_end_trans,
                                             uint64_t trans_num);

static herr_t H5C_jb_bjf__write_trans_num(H5C_jbrb_t * struct_ptr,
                                           hbool_t is_end_trans,
                                           uint64_t trans_num);

static herr_t H5C_jb_hrjf__comment(H5C_jbrb_t * struct_ptr,
 		                    const char * comment_ptr);

static herr_t H5C_jb_hrjf__end_transaction(H5C_jbrb_t * struct_ptr,
                                            uint64_t trans_num);

static herr_t H5C_jb_hrjf__eoa(H5C_jbrb_t * struct_ptr,
                                haddr_t eoa);

static herr_t H5C_jb_hrjf__journal_entry(H5C_jbrb_t * struct_ptr,
                                          uint64_t trans_num,
                                          haddr_t base_addr,
                                          size_t length,
                                          const uint8_t * body);

static herr_t H5C_jb_sio__flush(H5C_jbrb_t * struct_ptr);

static herr_t H5C_jb_sio__flush_full_buffers(H5C_jbrb_t * struct_ptr);

static herr_t H5C_jb_sio__get_last_transaction_on_disk(
					H5C_jbrb_t * struct_ptr,
				        uint64_t * trans_num_ptr);

static herr_t H5C_jb_sio__write_to_buffer(H5C_jbrb_t * struct_ptr,	
                                           size_t size,			
                                           const char * data,
                                           hbool_t is_end_trans,
                                           uint64_t trans_num);

herr_t H5C_jb_stats__dump(H5C_jbrb_t * struct_ptr);

herr_t H5C_jb_stats__reset(H5C_jbrb_t * struct_ptr);

static herr_t H5C_shrink_mdjsc_callback_table(H5C_t * cache_ptr);



/**************************************************************************/
/************************* journaling code proper *************************/
/**************************************************************************/

/*-------------------------------------------------------------------------
 * Function:    H5C_begin_journaling
 *
 * Purpose:     Setup the metadata cache to begin journaling.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              March 26, 2008
 *
 * Changes:	JRM -- 8/14/08
 *              Reworked the function to use the H5C_mdj_config_t
 *              structure.
 *
 *		JRM -- 8/18/08
 *		Added code to flush the cache before journaling 
 *		starts, and to call the metadata journaling status
 *		change callbacks after journaling has been started.
 *
 *		JRM -- 2/10/09
 *		Added journal_magic variable and supporting code.  
 *
 *		The idea is to assign a random magic number to both the 
 *		journal file, and to the journal configuration information
 *		information in the super block so that it will be hard to
 *		apply the wrong journal file to a corrupted hdf5 file.
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C_begin_journaling(H5F_t * f,
		      hid_t dxpl_id,
		      H5C_t * cache_ptr,
		      H5C_mdj_config_t * config_ptr)
{
    herr_t result;
    herr_t ret_value = SUCCEED;      /* Return value */
    int32_t journal_magic;
    H5C_mdj_config_t config;

    FUNC_ENTER_NOAPI(H5C_begin_journaling, FAIL)

    HDassert( f != NULL );
    HDassert( f->name != NULL );
    HDassert( f->shared != NULL );
    HDassert( f->shared->sizeof_addr > 0 );
    HDassert( f->shared->sizeof_size > 0 );
    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( cache_ptr->mdj_enabled == FALSE );
    HDassert( cache_ptr->trans_in_progress == FALSE );
    HDassert( cache_ptr->trans_num == 0 );
    HDassert( cache_ptr->last_trans_on_disk == 0 );
    HDassert( cache_ptr->tl_len == 0 );
    HDassert( cache_ptr->tl_size == 0 );
    HDassert( cache_ptr->tl_head_ptr == NULL );
    HDassert( cache_ptr->tl_tail_ptr == NULL );
    HDassert( cache_ptr->jwipl_len == 0 );
    HDassert( cache_ptr->jwipl_size == 0 );
    HDassert( cache_ptr->jwipl_head_ptr == NULL );
    HDassert( cache_ptr->jwipl_tail_ptr == NULL );
    HDassert( config_ptr != NULL );
    HDassert( config_ptr->jbrb_buf_size > 0 );
    HDassert( config_ptr->jbrb_num_bufs > 0 );
    HDassert( HDstrlen(config_ptr->journal_file_path) > 0 );

    if ( cache_ptr->mdj_enabled ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "metadata journaling already enabled on entry.")
    }

    result = H5C_flush_cache(f, dxpl_id, H5C__NO_FLAGS_SET);

    if ( result < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C_flush_cache() failed.") 
    }

    journal_magic = (int32_t)HDrand();

    result = H5C_jb__init(&(cache_ptr->mdj_jbrb),
                           journal_magic,
		           f->name,
			   config_ptr->journal_file_path,
			   config_ptr->jbrb_buf_size,
			   config_ptr->jbrb_num_bufs,
			   config_ptr->jbrb_use_aio,
			   config_ptr->jbrb_human_readable,
                           f->shared->sizeof_addr,
                           f->shared->sizeof_size);

    if ( result != SUCCEED ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C_jb__init() failed.")
    }

    /* Note that this call flushes the HDF5 file in passing */
    result = H5C_mark_journaling_in_progress(f, dxpl_id, journal_magic,
		                              config_ptr->journal_file_path);

    if ( result != SUCCEED ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C_mark_journaling_in_progress() failed.")
    }

    cache_ptr->mdj_enabled = TRUE;

    result = H5C_get_journal_config(cache_ptr, &config);

    if ( result < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C_get_journal_config() failed.")
    }

    result = H5C_call_mdjsc_callbacks(cache_ptr, dxpl_id, &config);

    if ( result < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C_call_mdjsc_callbacks() failed.")
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_begin_journaling() */


/*-------------------------------------------------------------------------
 * Function:    H5C_begin_transaction
 *
 * Purpose:     Handle book keeping for the beginning of a transaction, and
 *              return the transaction ID assigned to the transaction in 
 *              *trans_num_ptr.  
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              March 18, 2008
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C_begin_transaction(H5C_t * cache_ptr,
                       uint64_t * trans_num_ptr,
                       const char * api_call_name)
{
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5C_begin_transaction, FAIL)

    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( cache_ptr->tl_len == 0 );
    HDassert( cache_ptr->tl_size == 0 );
    HDassert( cache_ptr->tl_head_ptr == NULL );
    HDassert( cache_ptr->tl_tail_ptr == NULL );
    HDassert( trans_num_ptr != NULL );
    HDassert( api_call_name != NULL );
    HDassert( HDstrlen(api_call_name) <= H5C__MAX_API_NAME_LEN );

    if ( cache_ptr->mdj_enabled ) {

        if ( cache_ptr->trans_in_progress ) {

	    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                        "transaction already in progress?.")
        }

	HDstrncpy(cache_ptr->trans_api_name, api_call_name,
                  (size_t)H5C__MAX_API_NAME_LEN);

        (cache_ptr->trans_num)++;

        *trans_num_ptr = cache_ptr->trans_num;

        cache_ptr->trans_in_progress = TRUE;
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_begin_transaction() */


/*-------------------------------------------------------------------------
 * Function:    H5C_end_journaling
 *
 * Purpose:     Shutdown metadata journaling.
 *
 *              To do this we must:
 *
 *              1) Flush the cache.  This will also flush and truncate the
 *                 journal file.
 *
 *              2) Mark the superblock to indicate that we are no longer
 *                 journaling.  Note that this will flush the HDF5 file 
 *                 again in passing.
 *
 *              3) Tell the journal file write code to shutdown.  This will
 *                 also cause the journal file to be deleted.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              April 12, 2008
 *
 * Changes:	Added code to call the metadata journaling status change
 *		callback function.
 *						JR -- 8/18/08
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C_end_journaling(H5F_t * f,
                    hid_t dxpl_id,
		    H5C_t * cache_ptr)
{
    herr_t result;
    herr_t ret_value = SUCCEED;      /* Return value */
    H5C_mdj_config_t config;

    FUNC_ENTER_NOAPI(H5C_end_journaling, FAIL)

    HDassert( f != NULL );
    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );

    if ( cache_ptr->mdj_enabled ) {

	HDassert( cache_ptr->mdj_enabled );
        HDassert( cache_ptr->trans_in_progress == FALSE );
        HDassert( cache_ptr->tl_len == 0 );
        HDassert( cache_ptr->tl_size == 0 );
        HDassert( cache_ptr->tl_head_ptr == NULL );
        HDassert( cache_ptr->tl_tail_ptr == NULL );

        result = H5C_flush_cache(f, dxpl_id, H5C__NO_FLAGS_SET);

        if ( result < 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                             "H5C_flush_cache() failed.") 
	}

	HDassert( cache_ptr->mdj_enabled );

        /* Turn off journaling now, before attempting to modify the superblock
         *      extension (which is really an object header) and having the
         *      object header code call into the cache, which gets confused
         *      because there's no transaction in progress. -QAK
         */
        cache_ptr->mdj_enabled = FALSE;

        /* Remove the journal configuration information from the superblock
         * extension.  In passing, also discard the cache's copies of the 
         * metadata journaling magic, and the journal file name.
         */
        result = H5C_unmark_journaling_in_progress(f, dxpl_id, cache_ptr);

        if ( result < 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C_unmark_journaling_in_progress() failed.")
        }

        result = H5C_jb__takedown(&(cache_ptr->mdj_jbrb));

        if ( result < 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C_jb__takedown() failed.")
        }

        result = H5C_get_journal_config(cache_ptr, &config);

        if ( result < 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C_get_journal_config() failed.")
        }

        result = H5C_call_mdjsc_callbacks(cache_ptr, dxpl_id, &config);

        if ( result < 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C_call_mdjsc_callbacks() failed.")
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_end_journaling() */



/*-------------------------------------------------------------------------
 * Function:    H5C_end_transaction
 *
 * Purpose:     Handle book keeping for the end of a transaction.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              March 18, 2008
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C_end_transaction(H5F_t * f,
		     hid_t dxpl_id,
                     H5C_t * cache_ptr,
                     uint64_t trans_num,
                     const char * api_call_name)
{
    uint64_t new_last_trans_on_disk = 0;
    herr_t result;
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5C_end_transaction, FAIL)

    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( api_call_name != NULL );
    HDassert( HDstrlen(api_call_name) <= H5C__MAX_API_NAME_LEN );
    HDassert( ( ! ( cache_ptr->mdj_enabled ) ) ||
              ( HDstrcmp(api_call_name, cache_ptr->trans_api_name) == 0 ) );

    if ( cache_ptr->mdj_enabled ) {

        if ( ! ( cache_ptr->trans_in_progress ) ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                        "transaction not in progress?!?!")
        }

        if ( cache_ptr->trans_num != trans_num ) {

	    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "trans_num mis-match?!?!")
        }

        /* if the transaction list is not empty, generate journal messages,
         * and remove all entries from the transaction list.
         */
        if ( cache_ptr->tl_len > 0 ) {

            result = H5C_journal_transaction(f, dxpl_id, cache_ptr);

            if ( result != SUCCEED ) {

                HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                            "H5C_journal_transaction() failed.")
            }
        }

        cache_ptr->trans_in_progress = FALSE;

        /* Get the last transaction on disk.  If it has changed, remove
         * all entries with completed journal writes from the journal write
         * in progress list.
         */

        result = H5C_jb__get_last_transaction_on_disk(&(cache_ptr->mdj_jbrb),
                                                       &new_last_trans_on_disk);

        if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C_jb__get_last_transaction_on_disk() failed.")
        }

        if ( cache_ptr->last_trans_on_disk < new_last_trans_on_disk ) {

            result = H5C_update_for_new_last_trans_on_disk(cache_ptr,
		                                        new_last_trans_on_disk);

	    if ( result != SUCCEED ) {

                HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                            "H5C_update_for_new_last_trans_on_disk() failed.")
            }
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_end_transaction() */


/*-------------------------------------------------------------------------
 * Function:    H5C_get_journal_config
 *
 * Purpose:     Return the current metadata journaling status in an
 *              instance of H5C_mdj_config_t.
 *
 *              If journaling is enabled, config_ptr->enable_journaling 
 *              is set to TRUE, and the remaining fields in *config_ptr
 *              will be set to reflect current journaling status.
 *
 *              If journaling is disabled, config_ptr->enable_journaling
 *              is set to FALSE, and the remaining fields of *config_ptr
 *              are undefined.
 * 		
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              April 13, 2008
 *
 * Changes:
 *
 *              JRM -- 8/14/08
 *              Reworked function to use H5C_mdj_config_t.
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C_get_journal_config(H5C_t * cache_ptr,
		        H5C_mdj_config_t * config_ptr)
{
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5C_get_journal_config, FAIL)

    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );

    if ( config_ptr == NULL ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "config_ptr NULL on entry!?!.")
    }

    if ( cache_ptr->mdj_enabled ) {

        config_ptr->enable_journaling = TRUE;

	HDstrncpy(&(config_ptr->journal_file_path[0]), 
		  cache_ptr->jnl_file_name,
		  H5C__MAX_JOURNAL_FILE_NAME_LEN);

	config_ptr->journal_file_path[H5C__MAX_JOURNAL_FILE_NAME_LEN] = '\0';

	config_ptr->jbrb_buf_size = (cache_ptr->mdj_jbrb).buf_size;

	config_ptr->jbrb_num_bufs = (cache_ptr->mdj_jbrb).num_bufs;

	config_ptr->jbrb_use_aio = (cache_ptr->mdj_jbrb).use_aio;

	config_ptr->jbrb_human_readable = (cache_ptr->mdj_jbrb).human_readable;
	
    } else {

        config_ptr->enable_journaling = FALSE;
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_get_journal_config() */


/*-------------------------------------------------------------------------
 * Function:    H5C_journal_post_flush()
 *
 * Purpose:     Handle any journaling activities that are necessary
 * 		after we flush the metadata cache.
 *
 * 		At present this means:
 *
 * 		1) Verify that a transaction is still not in progress.
 *
 * 		2) Verify that the journal write in progress list
 * 		   is still empty.
 *
 * 		3) If the cache_is_clean parameter is true:
 *
 * 		   a) Flush the HDF5 file
 *
 * 		   b) Truncate the journal file
 *
 * 		   c) Reset cache_ptr->trans_num and 
 * 		      cache_ptr->last_trans_on_disk to zero.
 * 		
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              April 10, 2008
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C_journal_post_flush(H5F_t * f,
                        hid_t dxpl_id,
                        H5C_t * cache_ptr,
		        hbool_t cache_is_clean)
{
    herr_t result;
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5C_journal_post_flush, FAIL)

    HDassert( f != NULL );
    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( cache_ptr->mdj_enabled );

    if ( cache_ptr->trans_in_progress ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "Transaction in progress during flush?!?!?.")
    }

    if ( cache_ptr->jwipl_len != 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "journal write in progress list isn't empty?!?!.")
    }

    if ( cache_is_clean ) {

        /* Write the superblock to disk */

        result = H5F_super_write(f, dxpl_id);

        if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_WRITEERROR, FAIL, \
                        "unable to write superblock to file")
        }

	result = H5FD_flush(f->shared->lf, dxpl_id, (unsigned)0);

	if ( result > 0 ) {

	    HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
		        "low level flush failed")
	}

        result = H5C_jb__trunc(&(cache_ptr->mdj_jbrb));

        if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C_jb__trunc() failed.")
        }
        
	cache_ptr->trans_num = (uint64_t)0;
	cache_ptr->last_trans_on_disk = (uint64_t)0;
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_journal_post_flush() */


/*-------------------------------------------------------------------------
 * Function:    H5C_journal_pre_flush()
 *
 * Purpose:     Handle any journaling activities that are necessary
 * 		before we flush the metadata cache.
 *
 * 		At present this means:
 *
 * 		1) Verify that a transaction is not in progress.
 *
 * 		2) Flush the journal to disk.
 *
 * 		3) Get the ID of the last transaction on disk.
 *
 * 		4) If the value obtained in 3) above has changed,
 * 		   remove all entries whose last transaction has 
 * 		   made it to disk from the journal write in progress
 * 		   list.
 *
 * 		5) Verify that the journal write in progress list is
 * 		   empty.
 * 		
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              April 10, 2008
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C_journal_pre_flush(H5C_t * cache_ptr)
{
    herr_t result;
    uint64_t new_last_trans_on_disk;
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5C_journal_pre_flush, FAIL)

    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( cache_ptr->mdj_enabled );

    if ( cache_ptr->trans_in_progress ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "Transaction in progress during flush?!?!?.")
    }

    result = H5C_jb__flush(&(cache_ptr->mdj_jbrb));

    if ( result != SUCCEED ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C_jb__flush() failed.")
    }

    result = H5C_jb__get_last_transaction_on_disk(&(cache_ptr->mdj_jbrb),
		                                   &new_last_trans_on_disk);
    if ( result != SUCCEED ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C_jb__get_last_transaction_on_disk() failed.")
    }

    if ( cache_ptr->last_trans_on_disk < new_last_trans_on_disk ) {

        result = H5C_update_for_new_last_trans_on_disk(cache_ptr,
		                                        new_last_trans_on_disk);

	if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C_update_for_new_last_trans_on_disk() failed.")
        }
    }    

    if ( cache_ptr->jwipl_len != 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "journal write in progress list isn't empty?!?!.")
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_journal_pre_flush() */


/*-------------------------------------------------------------------------
 * Function:    H5C_journal_transaction()
 *
 * Purpose:     Generate journal messages for the current transaction.
 * 		In passing, remove all entries from the transaction list.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              April 3, 2008
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C_journal_transaction(H5F_t * f,
		         hid_t dxpl_id,
		         H5C_t * cache_ptr)

{
    char buf[H5C__MAX_API_NAME_LEN + 128];
    hbool_t resized;
    hbool_t renamed;
    H5C_cache_entry_t * entry_ptr = NULL;
    unsigned serialize_flags = 0;
    haddr_t new_addr;
    size_t new_len;
    void * new_image_ptr;
    void * thing;
    herr_t result;    
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5C_journal_transaction, FAIL)
    
    HDassert( f != NULL );
    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( cache_ptr->trans_in_progress );
    HDassert( cache_ptr->tl_len > 0 );

    HDsnprintf(buf, H5C__MAX_API_NAME_LEN + 128, "Begin transaction on %s.",
               cache_ptr->trans_api_name);

    result = H5C_jb__comment(&(cache_ptr->mdj_jbrb), buf);

    if ( result != SUCCEED ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C_jb__comment() failed.")
    }

    result = H5C_jb__start_transaction(&(cache_ptr->mdj_jbrb), 
		                        cache_ptr->trans_num);

    if ( result != SUCCEED ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C_jb__start_transaction() failed.")
    }

    entry_ptr = cache_ptr->tl_tail_ptr;
    while ( entry_ptr != NULL )
    {
        HDassert( entry_ptr->is_dirty );
	HDassert( entry_ptr->last_trans == cache_ptr->trans_num );

	resized = FALSE;
	renamed = FALSE;

	if ( entry_ptr->is_protected ) 
        {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "Protected entry in TL at transaction close.")
	}

        if ( entry_ptr->image_ptr == NULL )
        {
            entry_ptr->image_ptr = H5MM_malloc(entry_ptr->size);

            if ( entry_ptr->image_ptr == NULL )
            {

                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                           "memory allocation failed for on disk image buffer.")
            }
        }

	/* This should always be true, unless the entry has already been 
	 * serialized in this function, and that serialization caused the
	 * entry to be resized (and possibly renamed as well).
	 */
	if ( ! ( entry_ptr->image_up_to_date ) ) {

            result = entry_ptr->type->serialize(f,
			                        dxpl_id,
                                                entry_ptr->addr,
                                                entry_ptr->size,
                                                entry_ptr->image_ptr,
                                                (void *)entry_ptr,
                                                &serialize_flags,
                                                &new_addr,
                                                &new_len,
                                                &new_image_ptr);

            if ( result != SUCCEED ) {

                HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                            "unable to serialize entry")
            }

            if ( serialize_flags != 0 ) {

	        /* if the serialize_flags are not zero, the entry has been 
	         * modified as a result of the serialize.  Pass these changes
	         * on to the cache, and don't bother to write a journal entry 
	         * at this time -- the protect/unprotect/rename will move the 
	         * entry to the head of the transaction list, where we will 
	         * handle it later.
	         */

	        resized = 
		    (hbool_t)((serialize_flags & H5C__SERIALIZE_RESIZED_FLAG) != 0);
	        renamed = 
                    (hbool_t)((serialize_flags & H5C__SERIALIZE_MOVED_FLAG) != 0);

	        if ( ( renamed ) && ( ! resized ) ) {

                    HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                                "entry renamed but not resized?!?!")
                }

	        if ( resized ) 
                {
                    /* in the following protect/unprotect, the dxpl_id & udata
		     * are irrelement, as we know that the entry is in cache,
	             * and thus no I/O will take place.
	             */
	            thing = H5C_protect(f, dxpl_id,
	                                 entry_ptr->type, entry_ptr->addr,
				         NULL, H5C__NO_FLAGS_SET);

                    if ( thing == NULL ) {

                        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                                    "H5C_protect() failed.")
                    }

                    result = H5C_resize_entry(thing, new_len);

                    if ( result < 0 ) {

                        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                                    "H5C_resize_entry() failed.")
                    }

                    result = H5C_unprotect(f, dxpl_id,
                                            entry_ptr->type, entry_ptr->addr,
                                            thing, H5C__NO_FLAGS_SET);

                    if ( result < 0 ) {

                        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                                    "H5C_unprotect() failed.")
                    }

		    entry_ptr->image_ptr = new_image_ptr;
                }

	        if ( renamed ) {

                    result = H5C_move_entry(cache_ptr, entry_ptr->type,
				               entry_ptr->addr, new_addr);

                    if ( result < 0 ) {

                        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                                    "H5C_move_entry() failed.")
                    }
                }
            }

	    entry_ptr->image_up_to_date = TRUE;
        }

	/* if the entry hasn't been either resized or renamed, generate
	 * the journal entry, & remove from the transaction list.
	 */
	if ( ( ! resized ) && ( ! renamed ) ) {
                
            result = H5C_jb__journal_entry(&(cache_ptr->mdj_jbrb),
                                            cache_ptr->trans_num,
					    entry_ptr->addr,
					    entry_ptr->size,
					    (uint8_t *)(entry_ptr->image_ptr));

            if ( result != SUCCEED ) {

                HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                            "H5C_jb__journal_entry() failed.")
            }

	    H5C__TRANS_DLL_REMOVE(entry_ptr, cache_ptr->tl_head_ptr, \
                                   cache_ptr->tl_tail_ptr, cache_ptr->tl_len, \
				   cache_ptr->tl_size, FAIL);
        }
        entry_ptr = cache_ptr->tl_tail_ptr;
    }

    result = H5C_jb__end_transaction(&(cache_ptr->mdj_jbrb),
		                      cache_ptr->trans_num);

    if ( result != SUCCEED ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C_jb__end_transaction() failed.")
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_journal_transaction() */


/*-------------------------------------------------------------------------
 * Function:    H5C_update_for_new_last_trans_on_disk()
 *
 * Purpose:     Update the journal write in progress list for a change in
 * 		the last transaction on disk.
 *
 * 		Specifically, update the last_trans_on_disk field of 
 * 		*cache_ptr, and then scan the journal write in progress
 * 		list for entries whose last_trans field is now less than
 * 		or equal to cache_ptr->last_trans_on_disk.  Remove all
 * 		these entries from the journal write in progress list,
 * 		set their last_trans fields to zero, and insert then into
 * 		the eviction policy data structures.
 *
 * 		Similarly, scan the pinned entry list for entries whose
 * 		last_trans field is now less than or equal to 
 * 		cache_ptr->last_trans_on_disk.  In this case, just set
 * 		the last trans field to 0.  Note that here we assume that
 * 		the pinned entry list will always be small -- if this
 * 		ceases to be the case, we will have re-visit this case.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              April 3, 2008
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C_update_for_new_last_trans_on_disk(H5C_t * cache_ptr,
		                       uint64_t new_last_trans_on_disk)
{
    H5C_cache_entry_t * entry_ptr = NULL;
    H5C_cache_entry_t * prev_entry_ptr = NULL;
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5C_update_for_new_last_trans_on_disk, FAIL)

    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( cache_ptr->mdj_enabled );
    HDassert( cache_ptr->last_trans_on_disk <= new_last_trans_on_disk );

    if ( cache_ptr->last_trans_on_disk < new_last_trans_on_disk ) {

        cache_ptr->last_trans_on_disk = new_last_trans_on_disk;

	entry_ptr = cache_ptr->jwipl_tail_ptr;

	while ( entry_ptr != NULL )
        {
            prev_entry_ptr = entry_ptr->prev;

	    HDassert( entry_ptr->last_trans > 0 );
	    HDassert( entry_ptr->is_dirty );

	    if ( entry_ptr->last_trans <= cache_ptr->last_trans_on_disk ) {

                entry_ptr->last_trans = 0;
                H5C__UPDATE_RP_FOR_JOURNAL_WRITE_COMPLETE(cache_ptr, \
				                           entry_ptr, \
		                                           FAIL)
            }

	    entry_ptr = prev_entry_ptr;
        }

	/* now scan the pinned entry list */

	entry_ptr = cache_ptr->pel_head_ptr;

	while ( entry_ptr != NULL ) {

	    if ( entry_ptr->last_trans > 0 ) {

	        HDassert( entry_ptr->is_dirty );

		if ( entry_ptr->last_trans <= cache_ptr->last_trans_on_disk ) {

		    entry_ptr->last_trans = 0;
		}
	    }
	    entry_ptr = entry_ptr->next;
	}
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_update_for_new_last_trans_on_disk() */


/**************************************************************************/
/************* superblock journaling message management code **************/
/**************************************************************************/

/*-------------------------------------------------------------------------
 * Function:	H5C_check_for_journaling()
 *
 * Purpose:	If the superblock extension of a newly opened HDF5 file
 * 		indicates that journaling is in progress, the process
 * 		that created the file failed to close it properly, and 
 * 		thus the file is almost certainly corrupted.
 *
 * 		The purpose of this function is to detect this condition,
 * 		and either throw an error telling the user to run the 
 * 		recovery tool, or if so directed (presumably by the 
 * 		recovery tool) simply delete the metadata journaling 
 * 		configuration block and any reference to journaling in the 
 * 		superblock extension.
 *
 * 							JRM -- 3/26/08
 *
 * Return:	Success:	SUCCEED
 * 		Failure:	FAIL
 *
 * Programmer:	John Mainzer
 * 		March 26, 2008
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C_check_for_journaling(H5F_t * f,
                          hid_t dxpl_id,
			  H5C_t * cache_ptr,
		          hbool_t journal_recovered)
{
    const char * l0 =
        "This file was last written with metadata journaling enabled and was \n";
    const char * l1 =
        "not closed cleanly.  To allow HDF5 to read this file, please run the \n";
    const char * l2 = 
	"journal recovery tool on this file.  The journal was written \n";
    const char * l3 = "to \"";
    const char * l4 = "\".\n";
    herr_t result;
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5C_check_for_journaling, FAIL)

    HDassert( f );
    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( cache_ptr->jnl_magic == 0 );
    HDassert( cache_ptr->jnl_file_name_len == 0 );

    if ( H5C__check_for_journaling ) {

        result = H5C_get_journaling_in_progress(f, cache_ptr);

        if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C_get_journaling_in_progress() failed.")
        }

        if ( cache_ptr->jnl_file_name_len > 0 ) { /* journaling was in */
					          /* progress          */

            if ( journal_recovered ) {

	        /* Just forget that we were journaling.  Do this by
                 * deleting the superblock extension message that says
                 * we were.
	         */

                result = H5C_unmark_journaling_in_progress(f, 
                                                            dxpl_id, 
                                                            cache_ptr);

	        if ( result != SUCCEED ) {

                    HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                                "H5C_unmark_journaling_in_progress() failed.")
                }
	    } else {

                /* we have to play some games here to set up an error message 
		 * that contains the journal file path.  In essence, what 
		 * follows is a somewhat modified version of the HGOTO_ERROR() 
		 * macro.
	         */
                (void)H5Epush2(H5E_DEFAULT, __FILE__, FUNC, __LINE__, 
			       H5E_ERR_CLS_g, H5E_CACHE, H5E_CANTJOURNAL, 
			       "%s%s%s%s%s%s", l0, l1, l2, l3, 
			       cache_ptr->jnl_file_name, l4);
	        (void)H5E_dump_api_stack((int)H5_IS_API(FUNC));
	        HGOTO_DONE(FAIL)

	    }
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_check_for_journaling() */


/*-------------------------------------------------------------------------
 * Function:    H5C_get_journaling_in_progress()
 *
 * Purpose:     Query the HDF5 file to see if it is marked as having
 * 		journaling in progress.  Update the journaling 
 * 		configuration fields in the cache structure accordingly.
 *
 * 		At least initially, the purpose of this function is
 * 		to examine a newly opened HDF5 file, and determine
 * 		whether journaling was enabled.  If so, we can presume
 * 		that the application crashed while journaling, and that
 * 		we must refuse to open the file until the user runs the
 * 		recovery utility on it.
 *
 * 		Hwever, this logic will be handled at a higher level.
 * 		In this function, we just get the journaling configuration
 * 		(if any) that has been saved in the file, and load it
 * 		into *cache_ptr.
 *
 * 		Note that this function assumes that *cache_ptr has
 * 		no journaling configuration set before the function
 * 		is called.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              March 11, 2008
 *
 * Changes:	JRM -- 2/20/09
 *		Reworked to reflect the move of the journal file name 
 *		and magic from the journaling configuration block to 
 *		the metadata journaling superblock extension message.
 *		Note that the journaling configuration block no longer
 *		exists.
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C_get_journaling_in_progress(const H5F_t * f,
				H5C_t * cache_ptr)
{
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5C_get_journaling_in_progress)

    HDassert( f );
    HDassert( f->shared != NULL );
    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( cache_ptr->jnl_file_name_len == 0 );

    if ( f->shared->mdc_jnl_enabled == TRUE ) {

        if ( f->shared->mdc_jnl_file_name_len <= 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "journaling enabled but jnl file name empty?!?.")
        }

        if ( f->shared->mdc_jnl_file_name_len > 
             H5C__MAX_JOURNAL_FILE_NAME_LEN ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "journal file name too long?!?.")
        }

        cache_ptr->jnl_magic         = f->shared->mdc_jnl_magic;
        cache_ptr->jnl_file_name_len = (int32_t)f->shared->mdc_jnl_file_name_len;
        HDstrncpy(cache_ptr->jnl_file_name,
                  f->shared->mdc_jnl_file_name,
                  f->shared->mdc_jnl_file_name_len + 1);

        if ( ( (cache_ptr->jnl_file_name)[cache_ptr->jnl_file_name_len]
               != '\0' ) ||
             ( HDstrlen(cache_ptr->jnl_file_name) != 
               (size_t)(cache_ptr->jnl_file_name_len) ) ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "bad jnl file name or name len?!?.")
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_get_journaling_in_progress() */


/*-------------------------------------------------------------------------
 * Function:    H5C_mark_journaling_in_progress()
 *
 * Purpose:     Modify the HDF5 file to indicate that journaling is 
 * 		in progress, and flush the file to disk.  
 *
 * 		The objective here is to allow us to detect the fact 
 * 		the file was being journaled if we crash before we 
 * 		close the file properly.
 *
 * 		Note that the function assumes that the file is not 
 * 		currently marked as having journaling in progress.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              March 11, 2008
 *
 * Changes:	JRM -- 2/10/09
 *		Added the journal_magic parameter and related code.
 *
 *		JRM -- 2/20/09
 *		Reworked function to reflect the move of the journal
 *		file name and magic to the super block extension message
 *		and out of the metadata journaling configuration block
 *		which no longer exists.
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C_mark_journaling_in_progress(H5F_t * f,
                                 hid_t dxpl_id,
				 const int32_t journal_magic,
                                 const char * journal_file_name_ptr)
{
    H5C_t * cache_ptr;
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5C_mark_journaling_in_progress, FAIL)

    HDassert( f != NULL );
    HDassert( f->shared != NULL );
    HDassert( ! f->shared->mdc_jnl_enabled );

    cache_ptr = f->shared->cache;

    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( cache_ptr->jnl_file_name_len == 0 );
    HDassert( journal_file_name_ptr != NULL );

    /* Can't journal a read only file, so verify that we are
     * opened read/write and fail if we are not.
     */
    if ( (f->shared->flags & H5F_ACC_RDWR) == 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "File is opened read only.")
    }

    cache_ptr->jnl_magic = journal_magic;
    cache_ptr->jnl_file_name_len = (int32_t)HDstrlen(journal_file_name_ptr);

    if ( cache_ptr->jnl_file_name_len <= 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "length of journal file name is zero.")
    }

    if ( cache_ptr->jnl_file_name_len > H5C__MAX_JOURNAL_FILE_NAME_LEN ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "journal file name too long.")
    }

    HDstrncpy(cache_ptr->jnl_file_name,
              journal_file_name_ptr,
              (size_t)(cache_ptr->jnl_file_name_len + 1));

    /* now, load the journaling information into shared, and then call
     * H5F_super_write_mdj_msg() to write the metadata journaling 
     * superblock extension message to file.  
     */
    f->shared->mdc_jnl_enabled       = TRUE;
    f->shared->mdc_jnl_magic         = journal_magic;
    f->shared->mdc_jnl_file_name_len = (size_t)(cache_ptr->jnl_file_name_len);
    HDstrncpy(f->shared->mdc_jnl_file_name,
              journal_file_name_ptr,
              (size_t)(cache_ptr->jnl_file_name_len + 1));

    if ( H5F_super_write_mdj_msg(f, dxpl_id) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5F_super_write_mdj_msg() failed.")
    }

    /* Finally, flush the file to ensure that changes made it to disk. */

    if ( H5F_flush(f, dxpl_id, H5F_SCOPE_GLOBAL, H5F_FLUSH_NONE) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, "H5F_flush() failed.")
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_mark_journaling_in_progress() */


/*-------------------------------------------------------------------------
 * Function:    H5C_unmark_journaling_in_progress()
 *
 * Purpose:     Modify the HDF5 file to indicate that journaling is 
 * 		not in progress, and flush the file to disk.  
 *
 * 		The objective here is to remove the messages indicating
 * 		that the file is being journaled.  We will typically do 
 * 		this either on file close, or if directed to cease 
 * 		journaling.  Once these messages are removed, we will
 * 		be able to open the file without triggering a "journaling
 * 		in progress" failure.
 *
 * 		Note that the function assumes that the file is
 * 		currently marked as having journaling in progress.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              March 11, 2008
 *
 * Changes:	JRM -- 2/20/09
 *		Reworked function to reflect the move of the journal 
 *		file name and magic from the metadata journaling config
 *		block and into a superblock extension message.  Note that 
 *		the metadata journaling configuration block no longer 
 *		exists.
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C_unmark_journaling_in_progress(H5F_t * f,
                                   hid_t dxpl_id,
#ifndef NDEBUG
				   H5C_t * cache_ptr)
#else /* NDEBUG */
				   H5C_t UNUSED * cache_ptr)
#endif /* NDEBUG */
{
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5C_unmark_journaling_in_progress, FAIL)

    HDassert( f != NULL );
    HDassert( f->shared != NULL );
    HDassert( f->shared->mdc_jnl_enabled );
    HDassert( f->shared->cache == cache_ptr );
    HDassert( cache_ptr );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( cache_ptr->jnl_file_name_len > 0 );


    /* Can't journal a read only file, so verify that we are
     * opened read/write and fail if we are not.
     */
    if ( (f->shared->flags & H5F_ACC_RDWR) == 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "File is opened read only.")
    }

    /* Discard the journal file name and magic in *cache_ptr */
    cache_ptr->jnl_magic          = 0;
    cache_ptr->jnl_file_name_len  = 0;
    (cache_ptr->jnl_file_name)[0] = '\0';

    /* now, mark f->shared to indicate that journaling is not in 
     * progress, and then call H5F_super_write_mdj_msg() to write
     * the changes to disk.
     */
    f->shared->mdc_jnl_enabled        = FALSE;
    f->shared->mdc_jnl_magic          = 0;
    f->shared->mdc_jnl_file_name_len  = 0;
    (f->shared->mdc_jnl_file_name)[0] = '\0';

    if ( H5F_super_write_mdj_msg(f, dxpl_id) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5F_super_write_mdj_msg() failed.")
    }

    /* Finally, flush the file to ensure that changes made it to disk. */

    if ( H5F_flush(f, dxpl_id, H5F_SCOPE_GLOBAL, H5F_FLUSH_NONE) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, "H5F_flush() failed.")
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_unmark_journaling_in_progress() */


/**************************************************************************/
/****** metadata journaling status change callback management code ********/
/**************************************************************************/

/*-------------------------------------------------------------------------
 * Function:    H5C_call_mdjsc_callbacks()
 *
 * Purpose:     Call the metadata journaling status change callbacks.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              August 15, 2008
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5C_call_mdjsc_callbacks(H5C_t * cache_ptr, 
		          hid_t dxpl_id,
		          H5C_mdj_config_t * config_ptr)
{
    herr_t ret_value = SUCCEED;      /* Return value */
    int32_t i;
    int32_t funcs_called = 0;
    H5C_mdj_status_change_func_t func_ptr;
    void * data_ptr;

    FUNC_ENTER_NOAPI_NOINIT(H5C_call_mdjsc_callbacks)

    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( cache_ptr->mdjsc_cb_tbl != NULL );
    HDassert( cache_ptr->mdjsc_cb_tbl_len >= H5C__MIN_MDJSC_CB_TBL_LEN );
    HDassert( ( cache_ptr->mdjsc_cb_tbl_fl_head == -1 ) ||
	      ( cache_ptr->num_mdjsc_cbs < cache_ptr->mdjsc_cb_tbl_len ) );

    if ( ( cache_ptr->num_mdjsc_cbs < 0 ) 
         ||
	 ( cache_ptr->num_mdjsc_cbs > cache_ptr->mdjsc_cb_tbl_len ) 
	 ||
	 ( cache_ptr->mdjsc_cb_tbl_fl_head < -1 ) 
	 ||
	 ( cache_ptr->mdjsc_cb_tbl_fl_head > cache_ptr->mdjsc_cb_tbl_len ) 
	 ||
	 ( cache_ptr->mdjsc_cb_tbl_max_idx_in_use < -1 ) 
	 ||
	 ( cache_ptr->mdjsc_cb_tbl_max_idx_in_use >= 
	   cache_ptr->mdjsc_cb_tbl_len ) 
	 ||
	 ( cache_ptr->mdjsc_cb_tbl_len < H5C__MIN_MDJSC_CB_TBL_LEN ) 
	 ||
         ( ( cache_ptr->num_mdjsc_cbs == cache_ptr->mdjsc_cb_tbl_len )
	   &&
	   ( ( cache_ptr->mdjsc_cb_tbl_fl_head != -1 ) 
	     ||
	     ( cache_ptr->mdjsc_cb_tbl_max_idx_in_use !=
	       cache_ptr->mdjsc_cb_tbl_len - 1 ) 
	   )
	 )
	 ||
         ( ( cache_ptr->num_mdjsc_cbs < cache_ptr->mdjsc_cb_tbl_len )
	   &&
	   ( cache_ptr->mdjsc_cb_tbl_fl_head < 0 )
	 )
       ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "mdjsc_cb_tbl corrupt(1)?!?!");
    }

    for ( i = 0; i <= cache_ptr->mdjsc_cb_tbl_max_idx_in_use; i++ )
    {
        if ( ((cache_ptr->mdjsc_cb_tbl)[i]).fcn_ptr != NULL ) {

	    func_ptr = ((cache_ptr->mdjsc_cb_tbl)[i]).fcn_ptr;
	    data_ptr = ((cache_ptr->mdjsc_cb_tbl)[i]).data_ptr;

            /* Try the callback */
	    if(func_ptr(config_ptr, dxpl_id, data_ptr) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "metadata journaling status change callback failed!");

	    funcs_called++;
	}
    }

    if ( funcs_called != cache_ptr->num_mdjsc_cbs ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
		    "funcs_called != cache_ptr->num_mdjsc_cbs.");
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_call_mdjsc_callbacks() */


/*-------------------------------------------------------------------------
 * Function:    H5C_deregister_mdjsc_callback()
 *
 * Purpose:     Deregister a metadata journaling status change callback,
 * 		shrinking the metadata journaling status callback table 
 * 		as necessary.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              August 15, 2008
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C_deregister_mdjsc_callback(H5C_t * cache_ptr,
			       int32_t idx)
{
    herr_t ret_value = SUCCEED;      /* Return value */
    int32_t i;
    double fraction_in_use;

    FUNC_ENTER_NOAPI(H5C_deregister_mdjsc_callback, FAIL)

    if ( ( cache_ptr == NULL ) ||
         ( cache_ptr->magic != H5C__H5C_T_MAGIC ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
		    "cache_ptr corrupt?!?");
    }

    if ( ( cache_ptr->mdjsc_cb_tbl == NULL ) ||
         ( ( cache_ptr->num_mdjsc_cbs == cache_ptr->mdjsc_cb_tbl_len ) 
	   &&
	   ( cache_ptr->mdjsc_cb_tbl_fl_head != -1 ) ) ||
	 ( ( cache_ptr->mdjsc_cb_tbl_fl_head < 0 ) 
	   &&
	   ( cache_ptr->num_mdjsc_cbs != cache_ptr->mdjsc_cb_tbl_len ) ) ||
         ( cache_ptr->mdjsc_cb_tbl_len < H5C__MIN_MDJSC_CB_TBL_LEN ) ||
         ( cache_ptr->mdjsc_cb_tbl_fl_head >= cache_ptr->mdjsc_cb_tbl_len ) ||
	 ( cache_ptr->num_mdjsc_cbs > cache_ptr->mdjsc_cb_tbl_len ) ||
	 ( cache_ptr->num_mdjsc_cbs < 0 ) ||
	 ( ( cache_ptr->mdjsc_cb_tbl_max_idx_in_use < 0 ) &&
	   ( cache_ptr->num_mdjsc_cbs > 0 ) ) ) {
	    
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "mdjsc_cb_tbl corrupt(1)?!?!");
    }

    if ( cache_ptr->num_mdjsc_cbs <= 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "mdjsc_cb_tbl empty(1)!?!");
    }

    if ( ( idx < 0 ) ||
	 ( idx >= cache_ptr->mdjsc_cb_tbl_len ) ||
	 ( idx > cache_ptr->mdjsc_cb_tbl_max_idx_in_use ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "bad idx?!?");

    } else if ( ((cache_ptr->mdjsc_cb_tbl)[idx]).fcn_ptr == NULL ) {
	
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
		    "callback already deregistered");
   
    } else if ( ((cache_ptr->mdjsc_cb_tbl)[idx]).fl_next != -1 ) {
	
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "free list corrupted.");

    }

    ((cache_ptr->mdjsc_cb_tbl)[idx]).fcn_ptr = NULL;
    ((cache_ptr->mdjsc_cb_tbl)[idx]).data_ptr = NULL;
    ((cache_ptr->mdjsc_cb_tbl)[idx]).fl_next = 
	    cache_ptr->mdjsc_cb_tbl_fl_head;
    cache_ptr->mdjsc_cb_tbl_fl_head = idx;
    (cache_ptr->num_mdjsc_cbs)--;

    if ( cache_ptr->num_mdjsc_cbs == 0 ) {

        cache_ptr->mdjsc_cb_tbl_max_idx_in_use = -1;

    } else if ( idx == cache_ptr->mdjsc_cb_tbl_max_idx_in_use ) {

        i = idx;

        while ( ( i >= 0 ) &&
		( ((cache_ptr->mdjsc_cb_tbl)[i]).fcn_ptr == NULL ) ) {

	    i--;
	}

	if ( i < 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
			"mdjsc_cb_tbl empty(2)!?!");
	}

	cache_ptr->mdjsc_cb_tbl_max_idx_in_use = i;
    }

    if ( ( cache_ptr->num_mdjsc_cbs >= cache_ptr->mdjsc_cb_tbl_len )
	 ||
	 ( ( cache_ptr->num_mdjsc_cbs < cache_ptr->mdjsc_cb_tbl_len ) 
	   &&
	   ( cache_ptr->num_mdjsc_cbs > 0 ) 
	   &&
	   ( ( cache_ptr->mdjsc_cb_tbl_fl_head < 0 ) 
	     ||
	     ( cache_ptr->mdjsc_cb_tbl_fl_head >= cache_ptr->mdjsc_cb_tbl_len )
	   ) 
	 ) 
	 ||
	 ( ( cache_ptr->num_mdjsc_cbs == 0 ) 
	   &&
	   ( cache_ptr->mdjsc_cb_tbl_max_idx_in_use != -1 ) 
	 )
       ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "mdjsc_cb_tbl corrupt(2)?!?!");
    }

    fraction_in_use = ((double)(cache_ptr->num_mdjsc_cbs)) /
	              ((double)(cache_ptr->mdjsc_cb_tbl_len));

    if ( ( fraction_in_use < H5C__MDJSC_CB_TBL_MIN_ACTIVE_RATIO ) &&
         ( cache_ptr->mdjsc_cb_tbl_len > H5C__MIN_MDJSC_CB_TBL_LEN ) &&
         ( cache_ptr->mdjsc_cb_tbl_max_idx_in_use < 
	   (cache_ptr->mdjsc_cb_tbl_len / 2) ) ) {
        herr_t result;

        result = H5C_shrink_mdjsc_callback_table(cache_ptr);

	if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
			"H5C_shrink_mdjsc_callback_table() failed.");
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_deregister_mdjsc_callback() */


/*-------------------------------------------------------------------------
 * Function:    H5C_grow_mdjsc_callback_table()
 *
 * Purpose:     Double the size of the the metadata journaling status
 * 		change callback table.  Note that the table is assumed
 * 		to be full on entry.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              August 15, 2008
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5C_grow_mdjsc_callback_table(H5C_t * cache_ptr)
{
    herr_t ret_value = SUCCEED;      /* Return value */
    int32_t i;
    int32_t old_mdjsc_cb_tbl_len;
    int64_t new_mdjsc_cb_tbl_len;
    H5C_mdjsc_record_t * old_mdjsc_cb_tbl = NULL;
    H5C_mdjsc_record_t * new_mdjsc_cb_tbl = NULL;

    FUNC_ENTER_NOAPI_NOINIT(H5C_grow_mdjsc_callback_table)

    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( cache_ptr->mdjsc_cb_tbl != NULL );
    HDassert( cache_ptr->mdjsc_cb_tbl_len >= H5C__MIN_MDJSC_CB_TBL_LEN );
    HDassert( cache_ptr->mdjsc_cb_tbl_fl_head == -1 );
    HDassert( cache_ptr->num_mdjsc_cbs == cache_ptr->mdjsc_cb_tbl_len );

    if ( ( cache_ptr->num_mdjsc_cbs != cache_ptr->mdjsc_cb_tbl_len ) ||
	 ( cache_ptr->mdjsc_cb_tbl_fl_head != -1 ) ||
	 ( cache_ptr->mdjsc_cb_tbl_max_idx_in_use != 
	   cache_ptr->mdjsc_cb_tbl_len - 1 ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
	            "unexpected mdjsc_cb_tbl status.");
    }

    old_mdjsc_cb_tbl = cache_ptr->mdjsc_cb_tbl;
    old_mdjsc_cb_tbl_len = cache_ptr->mdjsc_cb_tbl_len;

    new_mdjsc_cb_tbl_len = 2 * old_mdjsc_cb_tbl_len;
    new_mdjsc_cb_tbl = (H5C_mdjsc_record_t *)
	H5MM_malloc((size_t)new_mdjsc_cb_tbl_len * sizeof(H5C_mdjsc_record_t));
    if ( new_mdjsc_cb_tbl == NULL ) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                    "can't alloc new mdjsc_cb_tbl.")
    }

    for ( i = 0; i < old_mdjsc_cb_tbl_len; i++ )
    {
        new_mdjsc_cb_tbl[i] = old_mdjsc_cb_tbl[i];
    }

    for ( i = old_mdjsc_cb_tbl_len; i < new_mdjsc_cb_tbl_len; i++ )
    {
	new_mdjsc_cb_tbl[i].fcn_ptr = NULL;
	new_mdjsc_cb_tbl[i].data_ptr = NULL;
	new_mdjsc_cb_tbl[i].fl_next = i + 1;
    }
    new_mdjsc_cb_tbl[new_mdjsc_cb_tbl_len - 1].fl_next = -1;

    cache_ptr->mdjsc_cb_tbl = new_mdjsc_cb_tbl;
    cache_ptr->mdjsc_cb_tbl_len = new_mdjsc_cb_tbl_len;
    cache_ptr->mdjsc_cb_tbl_fl_head = old_mdjsc_cb_tbl_len;

    old_mdjsc_cb_tbl = (H5C_mdjsc_record_t *)H5MM_xfree(old_mdjsc_cb_tbl);

    if ( old_mdjsc_cb_tbl != NULL ) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                    "free of old_mdjsc_cb_tbl failed.");
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_grow_mdjsc_callback_table() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_register_mdjsc_callback()
 *
 * Purpose:     Register a metadata journaling status change callback,
 * 		growing the metadata journaling status callback table 
 * 		as necessary.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              August 15, 2008
 *
 *-------------------------------------------------------------------------
 */

herr_t
H5C_register_mdjsc_callback(H5C_t * cache_ptr,
		             H5C_mdj_status_change_func_t fcn_ptr,
			     void * data_ptr,
			     int32_t * idx_ptr)
{
    herr_t result;
    herr_t ret_value = SUCCEED;      /* Return value */
    int32_t i;

    FUNC_ENTER_NOAPI(H5C_register_mdjsc_callback, FAIL)

    if ( ( cache_ptr == NULL ) ||
         ( cache_ptr->magic != H5C__H5C_T_MAGIC ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "bad cache_ptr on entry");
    }

    if ( cache_ptr->mdjsc_cb_tbl == NULL ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
		    "cache_ptr->mdjsc_cb_tbl == NULL")
    }

    if ( cache_ptr->mdjsc_cb_tbl_len < H5C__MIN_MDJSC_CB_TBL_LEN ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
		    "cache_ptr->mdjsc_cb_tbl_len too small")
    }

    if ( ( cache_ptr->mdjsc_cb_tbl_fl_head == -1 ) &&
	 ( cache_ptr->num_mdjsc_cbs < cache_ptr->mdjsc_cb_tbl_len ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
		    "mdjsc callback table corrupt?")
    }

    if ( fcn_ptr == NULL ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "fcn_ptr NULL on entry")
    }

    if ( idx_ptr == NULL ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "idx_ptr NULL on entry")
    }

    if ( cache_ptr->mdjsc_cb_tbl_len <= cache_ptr->num_mdjsc_cbs ) {

        result = H5C_grow_mdjsc_callback_table(cache_ptr);

	if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
			"H5C_grow_mdjsc_callback_table() failed.");
        }
    }

    if ( ( cache_ptr->mdjsc_cb_tbl_fl_head < 0 ) ||
         ( cache_ptr->mdjsc_cb_tbl_fl_head >= cache_ptr->mdjsc_cb_tbl_len ) ||
	 ( cache_ptr->num_mdjsc_cbs >= cache_ptr->mdjsc_cb_tbl_len ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "mdjsc_cb_tbl corrupt(1)?!?!");
    }

    i = cache_ptr->mdjsc_cb_tbl_fl_head;

    cache_ptr->mdjsc_cb_tbl_fl_head = ((cache_ptr->mdjsc_cb_tbl)[i]).fl_next;
    (cache_ptr->num_mdjsc_cbs)++;

    if ( ( ( cache_ptr->num_mdjsc_cbs == cache_ptr->mdjsc_cb_tbl_len ) &&
	   ( cache_ptr->mdjsc_cb_tbl_fl_head != -1 ) 
	 ) 
         ||
	 ( cache_ptr->num_mdjsc_cbs > cache_ptr->mdjsc_cb_tbl_len ) 
	 ||
	 ( ( cache_ptr->num_mdjsc_cbs < cache_ptr->mdjsc_cb_tbl_len ) 
	   &&
	   ( ( cache_ptr->mdjsc_cb_tbl_fl_head < 0 ) 
	     ||
	     ( cache_ptr->mdjsc_cb_tbl_fl_head >= cache_ptr->mdjsc_cb_tbl_len )
	   ) 
	 ) 
       ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "mdjsc_cb_tbl corrupt(2)?!?!");
    }

    ((cache_ptr->mdjsc_cb_tbl)[i]).fcn_ptr  = fcn_ptr;
    ((cache_ptr->mdjsc_cb_tbl)[i]).data_ptr = data_ptr;
    ((cache_ptr->mdjsc_cb_tbl)[i]).fl_next  = -1;

    if ( i > cache_ptr->mdjsc_cb_tbl_max_idx_in_use ) {

        cache_ptr->mdjsc_cb_tbl_max_idx_in_use = i;
    }

    *idx_ptr = i;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_register_mdjsc_callback() */


/*-------------------------------------------------------------------------
 * Function:    H5C_shrink_mdjsc_callback_table()
 *
 * Purpose:     Half the size of the the metadata journaling status
 * 		change callback table.  Note that the table is assumed
 * 		to be:
 *
 * 		1) Not more than H5C__MDJSC_CB_TBL_MIN_ACTIVE_RATIO * 100
 *                 percent full.
 *
 *              2) Of size H5C__MIN_MDJSC_CB_TBL_LEN * 2 ** n, where
 *                 n is a positive integer.
 *
 *              3) Contain no entries at index greater than or equal to
 *                 cache_ptr->mdjsc_cb_tbl_len / 2.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              August 15, 2008
 *
 *-------------------------------------------------------------------------
 */

static herr_t
H5C_shrink_mdjsc_callback_table(H5C_t * cache_ptr)
{
    herr_t ret_value = SUCCEED;      /* Return value */
    int32_t i;
    int32_t old_mdjsc_cb_tbl_len;
    int32_t new_mdjsc_cb_tbl_len;
    int32_t new_fl_head = -1;
    int32_t last_free_entry = -1;
    double fraction_in_use;
    H5C_mdjsc_record_t * old_mdjsc_cb_tbl = NULL;
    H5C_mdjsc_record_t * new_mdjsc_cb_tbl = NULL;

    FUNC_ENTER_NOAPI_NOINIT(H5C_shrink_mdjsc_callback_table)

    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( cache_ptr->mdjsc_cb_tbl != NULL );
    HDassert( cache_ptr->mdjsc_cb_tbl_len > H5C__MIN_MDJSC_CB_TBL_LEN );
    HDassert( cache_ptr->mdjsc_cb_tbl_fl_head >= 0);
    HDassert( cache_ptr->num_mdjsc_cbs < cache_ptr->mdjsc_cb_tbl_len / 2 );

    fraction_in_use = ((double)(cache_ptr->num_mdjsc_cbs)) /
	              ((double)(cache_ptr->mdjsc_cb_tbl_len));

    if ( ( cache_ptr->num_mdjsc_cbs >= cache_ptr->mdjsc_cb_tbl_len ) ||
	 ( (cache_ptr->mdjsc_cb_tbl_len / 2) < H5C__MIN_MDJSC_CB_TBL_LEN ) ||
	 ( cache_ptr->mdjsc_cb_tbl_fl_head == -1 ) ||
	 ( cache_ptr->mdjsc_cb_tbl_max_idx_in_use >= 
	   cache_ptr->mdjsc_cb_tbl_len / 2 ) ||
	 ( fraction_in_use >= H5C__MDJSC_CB_TBL_MIN_ACTIVE_RATIO ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
	            "unexpected mdjsc_cb_tbl status.");
    }

    old_mdjsc_cb_tbl = cache_ptr->mdjsc_cb_tbl;
    old_mdjsc_cb_tbl_len = cache_ptr->mdjsc_cb_tbl_len;

    new_mdjsc_cb_tbl_len = old_mdjsc_cb_tbl_len / 2;

    while ( ( (new_mdjsc_cb_tbl_len / 2) >= H5C__MIN_MDJSC_CB_TBL_LEN ) &&
	    ( (((double)(cache_ptr->num_mdjsc_cbs)) / 
	       ((double)new_mdjsc_cb_tbl_len)) <= 
	      H5C__MDJSC_CB_TBL_MIN_ACTIVE_RATIO ) &&
	    ( (new_mdjsc_cb_tbl_len / 2) > 
	      cache_ptr->mdjsc_cb_tbl_max_idx_in_use ) )
    {
	new_mdjsc_cb_tbl_len /= 2;
    }

    if ( ( new_mdjsc_cb_tbl_len < H5C__MIN_MDJSC_CB_TBL_LEN ) ||
         ( new_mdjsc_cb_tbl_len < cache_ptr->mdjsc_cb_tbl_max_idx_in_use ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
	            "error in computation of new_mdjsc_cb_tbl_len?!?!");
    }

    new_mdjsc_cb_tbl = (H5C_mdjsc_record_t *)
	H5MM_malloc((size_t)new_mdjsc_cb_tbl_len * sizeof(H5C_mdjsc_record_t));
    if ( new_mdjsc_cb_tbl == NULL ) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                    "can't alloc new mdjsc_cb_tbl.")
    }

    /* now copy over the table, constructing the free list as we go */

    for ( i = 0; i < new_mdjsc_cb_tbl_len; i++ )
    {
        if ( old_mdjsc_cb_tbl[i].fcn_ptr == NULL ) {

	    new_mdjsc_cb_tbl[i].fcn_ptr = NULL;
	    new_mdjsc_cb_tbl[i].data_ptr = NULL;
	    new_mdjsc_cb_tbl[i].fl_next = -1;
	
	    if ( new_fl_head == -1 ) {

	        new_fl_head = i;
		last_free_entry = i;

	    } else {

		new_mdjsc_cb_tbl[last_free_entry].fl_next = i;
	        last_free_entry = i;
	    }
	} else {

            new_mdjsc_cb_tbl[i] = old_mdjsc_cb_tbl[i];

	}
    }

    if ( new_fl_head == -1 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "table full after shrink!?!.");

    }

    cache_ptr->mdjsc_cb_tbl = new_mdjsc_cb_tbl;
    cache_ptr->mdjsc_cb_tbl_fl_head = new_fl_head;
    cache_ptr->mdjsc_cb_tbl_len = new_mdjsc_cb_tbl_len;

    old_mdjsc_cb_tbl = ( H5C_mdjsc_record_t *)H5MM_xfree(old_mdjsc_cb_tbl);

    if ( old_mdjsc_cb_tbl != NULL ) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                    "free of old_mdjsc_cb_tbl failed.");
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_shrink_mdjsc_callback_table() */


/**************************************************************************/
/********************** journal file management code **********************/
/**************************************************************************/

/* The following macros are wrappers for the low level binary journal file
 * functions.  They exist, as it is likely that these function will be 
 * converted into macros once we have asynchronous journal file writes 
 * running, and by setting up these wrappers now, we will make this 
 * conversion easier.
 */

#if 1 /* JRM */
    /* remove print statements from these macros. -- JRM */
#endif /* JRM */

#define H5C_JB_BJF__WRITE_BUFFER(struct_ptr,                             \
                                  buf_size,                               \
                                  buf_ptr,                                \
                                  is_end_trans,                           \
                                  trans_num,                              \
                                  fail_return)				  \
if ( H5C_jb_bjf__write_buffer((struct_ptr), (buf_size), (buf_ptr),       \
                              (is_end_trans), (trans_num)) != SUCCEED ) { \
    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, (fail_return),                 \
                "H5C_jb_bjf__write_buffer() failed.");                   \
}

#define H5C_jb_BJF__WRITE_CHKSUM(struct_ptr,                             \
                                  is_end_trans,                           \
                                  trans_num,                              \
                                  fail_return)                            \
if ( H5C_jb_bjf__write_chksum((struct_ptr), (is_end_trans), (trans_num)) \
     != SUCCEED ) {                                                       \
    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, (fail_return),                 \
                "H5C_jb_bjf__write_chksum() failed.");                   \
}

#define H5C_JB_BJF__WRITE_LENGTH(struct_ptr,                             \
                                  length,                                 \
                                  is_end_trans,                           \
                                  trans_num,                              \
                                  fail_return)                            \
if ( H5C_jb_bjf__write_length((struct_ptr), (length), (is_end_trans),    \
                               (trans_num)) != SUCCEED ) {                \
    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, (fail_return),                 \
                "H5C_jb_bjf__write_length() failed.");                   \
}

#define H5C_JB_BJF__WRITE_OFFSET(struct_ptr,                             \
                                  offset,                                 \
                                  is_end_trans,                           \
                                  trans_num,                              \
                                  fail_return)                            \
if ( H5C_jb_bjf__write_offset((struct_ptr), (offset), (is_end_trans),    \
                               (trans_num)) != SUCCEED ) {                \
    HDfprintf(stdout, "%s: H5C_jb_bjf__write_offset() failed.\n", FUNC); \
    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, (fail_return),                 \
                "H5C_jb_bjf__write_offset() failed.");                   \
}

#define H5C_JB_BJF__WRITE_SIG_AND_VER(struct_ptr,                        \
                                       sig_ptr,                           \
                                       version,                           \
                                       keep_chksum,                       \
                                       is_end_trans,                      \
                                       trans_num,                         \
                                       fail_return)                       \
if(H5C_jb_bjf__write_sig_and_ver((struct_ptr), (sig_ptr), (version),     \
        (keep_chksum), (is_end_trans), (trans_num)) < 0) {                \
    HDfprintf(stdout, "%s: H5C_jb_bjf__write_sig_and_ver() failed.\n", FUNC); \
    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, (fail_return), "H5C_jb_bjf__write_sig_and_ver() failed.") \
}

#define H5C_JB_BJF__WRITE_TRANS_NUM(struct_ptr,                          \
                                     is_end_trans,                        \
                                     trans_num,                           \
                                     fail_return)                         \
if ( H5C_jb_bjf__write_trans_num((struct_ptr), (is_end_trans),           \
                                  (trans_num)) != SUCCEED ) {             \
    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, (fail_return),                 \
                "H5C_jb_bjf__write_trans_num() failed.");                \
}


/******************************************************************************
 *
 * Function:		H5C_jb_aio__await_buffer_write_completion()
 *
 * Programmer:		John Mainzer
 *
 * Purpose:		Await completion of the asynchronous write of the
 *			specified buffer.
 *
 *			Verify that AIO is enabled, that the specified 
 *			buffer exists and has been queued for an asynchronous 
 *			write which has not been logged as complete.
 *
 *			Test to see if the write has completed -- if it has
 *			not, await its completion.
 *
 *			Then mark the write as complete and return.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

static herr_t 
H5C_jb_aio__await_buffer_write_completion(H5C_jbrb_t * struct_ptr,
				           int buf_num)
{
    int result;
    herr_t ret_value = SUCCEED;
    struct aiocb * aiocb_ptr = NULL;
    const struct aiocb * aiocb_list[1] = { NULL };

    FUNC_ENTER_NOAPI(H5C_jb_aio__await_buffer_write_completion, FAIL)

    HDassert( struct_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );
    HDassert( struct_ptr->use_aio );
    HDassert( struct_ptr->aio_ctl_blks != NULL );
    HDassert( buf_num >= 0 );
    HDassert( buf_num < struct_ptr->num_bufs );

    aiocb_ptr = &((*(struct_ptr->aio_ctl_blks))[buf_num]);

    if ( aiocb_ptr->aio_fildes != struct_ptr->journal_file_fd ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "unexpected file descriptor in target buffer aio ctl blk")
    }

    /* wait until the request completes */
    aiocb_list[0] = aiocb_ptr;
    result = aio_suspend(aiocb_list, 1, NULL);

    if ( result != 0 ) {

        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
		    "call to aio_suspend() failed.")
    }
#if 0 /* JRM */
    /* check to see if there were any errors */
    result = aio_error(aiocb_ptr);
    if ( result != 0 ) {

        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
		    "aio_error() reports something other than success.")
    }
#else /* JRM */
    /* verify that aio_error() returns something other than EINPROGRESS */
    result = aio_error(aiocb_ptr);

    if ( result != 0 ) {

        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
                    "aio_error() reports error after aio_suspend() returns")
    }

    /* call aio_return() to complete the write */
    result = aio_return(aiocb_ptr);
    if ( result == -1 ) {

        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
		    "aio_error() reports something other than success.")

    }
    /* TODO: Verify the number of bytes written? */
#endif /* JRM */

    H5C__JBRB__UPDATE_STATS_FOR_BUF_WRITE_COMPLETE(struct_ptr, TRUE)

    /* mark the aio control block to indicate no write in progress */
    aiocb_ptr->aio_fildes = -1;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb_aio__await_buffer_write_completion() */


/******************************************************************************
 *
 * Function:		H5C_jb_aio__await_async_fsync_completion()
 *
 * Programmer:		John Mainzer
 *
 * Purpose:		Verify that AIO is enabled, taht struct_ptr->
 *			use_aio_fsync is TRUE, and that the sync queue
 *			is not empty.
 *
 *			Then await completion of the asynchronous fsync 
 *			at the head of the sync queue, update struct_ptr->
 *			last_trans_on_disk, remove and discard the instance
 *			of H5C_jbrb_sync_q_entry_t at the head of the sync
 *			queue, and return.
 *
 *							JRM -- 2/10/10
 *
 * Returns:		SUCCEED on success.
 *			FAIL otherwise.
 *
 ******************************************************************************/

#define H5C_JB_AIO__AWAIT_ASYNC_FSYNC_COMPLETION__DEBUG 0

#if 1
static herr_t 
H5C_jb_aio__await_async_fsync_completion(H5C_jbrb_t * struct_ptr)
{
    int result;
    herr_t ret_value = SUCCEED;
    struct H5C_jbrb_sync_q_entry_t * head_ptr = NULL;
    struct aiocb * aiocb_ptr = NULL;

    FUNC_ENTER_NOAPI(H5C_jb_aio__await_async_fsync_completion, FAIL)

    HDassert( struct_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );
    HDassert( struct_ptr->use_aio );
    HDassert( struct_ptr->use_aio_fsync );
    HDassert( struct_ptr->aio_sync_q_len > 0 );

    head_ptr = struct_ptr->aio_sync_q_head;

    HDassert( head_ptr != NULL );
    HDassert( head_ptr->magic == H5C__H5C_JBRB_SYNC_Q_T_MAGIC );

    aiocb_ptr = &(head_ptr->ctl_blk);

    if ( aiocb_ptr->aio_fildes != struct_ptr->journal_file_fd ) {

#if H5C_JB_AIO__AWAIT_ASYNC_FSYNC_COMPLETION__DEBUG
        HDfprintf(stdout, "%s: bad fd in ctl blk?!?\n", FUNC);
#endif /* H5C_JB_AIO__AWAIT_ASYNC_FSYNC_COMPLETION__DEBUG */

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "bad fd in ctl blk?!?")
    }

    /* can't use aio_suspend() with an aiocb from a call to aio_fsync()
     * hence loop until aio_error() returns either 0 or something other
     * than EINPROGRESS.
     */
    do 
    {
        result = aio_error(aiocb_ptr);

        if ( ( result != 0 ) && ( result != EINPROGRESS ) ) {

#if H5C_JB_AIO__AWAIT_ASYNC_FSYNC_COMPLETION__DEBUG
            HDfprintf(stdout, "%s: call to aio_error() reports error.\n", FUNC);
            HDfprintf(stdout, "%s: errno = %d (%s).\n", FUNC, errno, 
                      strerror(errno));
#endif /* H5C_JB_AIO__AWAIT_ASYNC_FSYNC_COMPLETION__DEBUG */

            HGOTO_ERROR(H5E_IO, H5E_SYNCFAIL, FAIL, \
                        "aio_error() reports error.")
        }

	H5C__JBRB__UPDATE_STATS_FOR_AIO_ERROR_CALL_AWAITING_SYNC(struct_ptr)

    } while ( result != 0 );

    /* call aio_return() to complete the aio_fsync() */
    result = aio_return(aiocb_ptr);

    if ( result == -1 ) {

#if H5C_JB_AIO__AWAIT_ASYNC_FSYNC_COMPLETION__DEBUG
        HDfprintf(stdout, 
               "%s: aio_return() reports something other than success.\n",
               FUNC);
#endif /* H5C_JB_AIO__AWAIT_ASYNC_FSYNC_COMPLETION__DEBUG */

        HGOTO_ERROR(H5E_IO, H5E_SYNCFAIL, FAIL, \
		    "aio_return() reports something other than success.")

    }

    /* the aio_fsync() completed successfully -- update last trans on disk,
     * and discard the head of the sync queue.
     */

    HDassert( struct_ptr->last_trans_on_disk <= head_ptr->last_trans_in_sync );
    HDassert( head_ptr->last_trans_in_sync <= struct_ptr->last_trans_written );

#if H5C_JB_AIO__AWAIT_ASYNC_FSYNC_COMPLETION__DEBUG
    HDfprintf(stdout, "%s: changing last trans on disk from %lld to %lld.\n",
              FUNC, struct_ptr->last_trans_on_disk, 
              head_ptr->last_trans_in_sync);
#endif /* H5C_JB_AIO__AWAIT_ASYNC_FSYNC_COMPLETION__DEBUG */

    H5C__JBRB__UPDATE_STATS_FOR_ASYNC_SYNC_COMPLETED(struct_ptr, TRUE)

    struct_ptr->last_trans_on_disk = head_ptr->last_trans_in_sync;
    aiocb_ptr->aio_fildes = -1;

    if ( H5C_jb_aio__sync_q__discard_head(struct_ptr) != SUCCEED ) {

#if H5C_JB_AIO__AWAIT_ASYNC_FSYNC_COMPLETION__DEBUG
        HDfprintf(stdout, 
                 "%s: H5C_jb_aio__sync_q__discard_head() failed.\n", FUNC);
#endif /* H5C_JB_AIO__AWAIT_ASYNC_FSYNC_COMPLETION__DEBUG */

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "H5C_jb_aio__sync_q__discard_head() failed.")
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb_aio__await_async_fsync_completion() */
#else 
static herr_t 
H5C_jb_aio__await_async_fsync_completion(H5C_jbrb_t * struct_ptr)
{
    int result;
    herr_t ret_value = SUCCEED;
    struct H5C_jbrb_sync_q_entry_t * head_ptr = NULL;
    struct aiocb * aiocb_ptr = NULL;
    const struct aiocb * aiocb_list[1] = { NULL };

    FUNC_ENTER_NOAPI(H5C_jb_aio__await_async_fsync_completion, FAIL)

    HDassert( struct_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );
    HDassert( struct_ptr->use_aio );
    HDassert( struct_ptr->use_aio_fsync );
    HDassert( struct_ptr->aio_sync_q_len > 0 );

    head_ptr = struct_ptr->aio_sync_q_head;

    HDassert( head_ptr != NULL );
    HDassert( head_ptr->magic == H5C__H5C_JBRB_SYNC_Q_T_MAGIC );

    aiocb_ptr = &(head_ptr->ctl_blk);

    if ( aiocb_ptr->aio_fildes != struct_ptr->journal_file_fd ) {

#if H5C_JB_AIO__AWAIT_ASYNC_FSYNC_COMPLETION__DEBUG
        HDfprintf(stdout, "%s: bad fd in ctl blk?!?\n", FUNC);
#endif /* H5C_JB_AIO__AWAIT_ASYNC_FSYNC_COMPLETION__DEBUG */

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "bad fd in ctl blk?!?")
    }

    /* wait until the fsync request completes */
    aiocb_list[0] = aiocb_ptr;
    result = aio_suspend(aiocb_list, 1, NULL);

    if ( result != 0 ) {

#if H5C_JB_AIO__AWAIT_ASYNC_FSYNC_COMPLETION__DEBUG
        HDfprintf(stdout, "%s: call to aio_suspend() failed.\n", FUNC);
        HDfprintf(stdout, "%s: errno = %d (%s).\n", FUNC, errno, 
                  strerror(errno));
#endif /* H5C_JB_AIO__AWAIT_ASYNC_FSYNC_COMPLETION__DEBUG */

        HGOTO_ERROR(H5E_IO, H5E_SYNCFAIL, FAIL, \
		    "call to aio_suspend() failed.")
    }

    /* verify that aio_error() returns good status */
    result = aio_error(aiocb_ptr);

    if ( result != 0 ) {

#if H5C_JB_AIO__AWAIT_ASYNC_FSYNC_COMPLETION__DEBUG
        HDfprintf(stdout, 
               "%s: aio_error() reports error after aio_suspend() returns.\n",
               FUNC);
#endif /* H5C_JB_AIO__AWAIT_ASYNC_FSYNC_COMPLETION__DEBUG */

        HGOTO_ERROR(H5E_IO, H5E_SYNCFAIL, FAIL, \
                    "aio_error() reports error after aio_suspend() returns")
    }

    /* call aio_return() to complete the aio_fsync() */
    result = aio_return(aiocb_ptr);
    if ( result == -1 ) {

#if H5C_JB_AIO__AWAIT_ASYNC_FSYNC_COMPLETION__DEBUG
        HDfprintf(stdout, 
               "%s: aio_return() reports something other than success.\n",
               FUNC);
#endif /* H5C_JB_AIO__AWAIT_ASYNC_FSYNC_COMPLETION__DEBUG */

        HGOTO_ERROR(H5E_IO, H5E_SYNCFAIL, FAIL, \
		    "aio_return() reports something other than success.")

    }

    /* the aio_fsync() completed successfully -- update last trans on disk,
     * and discard the head of the sync queue.
     */

    HDassert( struct_ptr->last_trans_on_disk <= head_ptr->last_trans_in_sync );
    HDassert( head_ptr->last_trans_in_sync <= struct_ptr->last_trans_written );

#if H5C_JB_AIO__AWAIT_ASYNC_FSYNC_COMPLETION__DEBUG
    HDfprintf(stdout, "%s: changing last trans on disk from %lld to %lld.\n",
              FUNC, struct_ptr->last_trans_on_disk, 
              head_ptr->last_trans_in_sync);
#endif /* H5C_JB_AIO__AWAIT_ASYNC_FSYNC_COMPLETION__DEBUG */

    struct_ptr->last_trans_on_disk = head_ptr->last_trans_in_sync;
    aiocb_ptr->aio_fildes = -1;

    if ( H5C_jb_aio__sync_q__discard_head(struct_ptr) != SUCCEED ) {

#if H5C_JB_AIO__AWAIT_ASYNC_FSYNC_COMPLETION__DEBUG
        HDfprintf(stdout, 
                 "%s: H5C_jb_aio__sync_q__discard_head() failed.\n", FUNC);
#endif /* H5C_JB_AIO__AWAIT_ASYNC_FSYNC_COMPLETION__DEBUG */

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "H5C_jb_aio__sync_q__discard_head() failed.")
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb_aio__await_async_fsync_completion() */
#endif


/******************************************************************************
 *
 * Function:		H5C_jb_aio__await_completion_of_all_async_fsyncs
 *
 * Programmer:		John Mainzer
 *			2/10/10
 *
 * Purpose:		Verify that AIO is enabled, and that struct_ptr->
 *			use_aio_fsync is TRUE.
 *
 * 			Then await completion of all asynchronous fsyncs 
 *			currently in progress -- if any.  As each fsync 
 *			completes, update struct_ptr->last_trans_on_disk,
 *			and discard the head of the sync queue.
 *
 * Returns:		SUCCEED on success.
 *			FAIL otherwise.
 *
 ******************************************************************************/

#define H5C_JB_AIO__AWAIT_COMPLETION_OF_ALL_ASYNC_FSYNCS__DEBUG 0

herr_t 
H5C_jb_aio__await_completion_of_all_async_fsyncs(H5C_jbrb_t * struct_ptr)
{
    herr_t result;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C_jb_aio__await_completion_of_all_async_fsyncs, FAIL)

    HDassert( struct_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );
    HDassert( struct_ptr->use_aio );
    HDassert( struct_ptr->use_aio_fsync );

#if H5C_JB_AIO__AWAIT_COMPLETION_OF_ALL_ASYNC_FSYNCS__DEBUG 
    HDfprintf(stdout, "%s: entering -- aio_sync_q_len = %d.\n",
              FUNC, (int)(struct_ptr->aio_sync_q_len));
#endif /* H5C_JB_AIO__AWAIT_COMPLETION_OF_ALL_ASYNC_FSYNCS__DEBUG */

    while ( struct_ptr->aio_sync_q_len > 0 ) {

        result = H5C_jb_aio__await_async_fsync_completion(struct_ptr);

        if ( result != SUCCEED ) {

#if H5C_JB_AIO__AWAIT_COMPLETION_OF_ALL_ASYNC_FSYNCS__DEBUG 
            HDfprintf(stdout, 
                      "H5C_jb_aio__await_async_fsync_completion() failed.\n",
                      FUNC);
#endif /* H5C_JB_AIO__AWAIT_COMPLETION_OF_ALL_ASYNC_FSYNCS__DEBUG */

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                        "H5C_jb_aio__await_async_fsync_completion() failed.")
        }
    } /* while */

    HDassert( struct_ptr->aio_sync_q_len == 0 );

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb_aio__await_completion_of_all_async_fsyncs() */


/******************************************************************************
 *
 * Function:		H5C_jb_aio__await_completion_of_all_pending_writes
 *
 * Programmer:		John Mainzer
 *			1/12/10
 *
 * Purpose:		Await completion of all asynchronous writes currently
 *			in progress -- if any.  As each write completes, mark
 *			the associated buffer as free by updating 
 *			struct_ptr->get and decrementing 
 *			struct_ptr->bufs_in_use.  
 *
 *			Note that the buffer indicated by struct_ptr->put
 *			may or may not be involved in a write when this 
 *			function is called.  Infer whether it is via the 
 *			aio contol blocks array.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

#define H5C_JB_AIO__AWAIT_COMPLETION_OF_ALL_PENDING_WRITES__DEBUG 0

herr_t 
H5C_jb_aio__await_completion_of_all_pending_writes(H5C_jbrb_t * struct_ptr)
{
    hbool_t done = FALSE;
    int result;
    herr_t ret_value = SUCCEED;
    struct aiocb * aiocb_ptr = NULL;

    FUNC_ENTER_NOAPI(H5C_jb_aio__await_completion_of_all_pending_writes, FAIL)

    HDassert( struct_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );
    HDassert( struct_ptr->use_aio );

    if ( struct_ptr->bufs_in_use <= 0 ) {

        done = TRUE;
    }

    while ( ! done ) {

        aiocb_ptr = &((*(struct_ptr->aio_ctl_blks))[struct_ptr->get]);

        if ( aiocb_ptr->aio_fildes != -1 ) {

	    result = H5C_jb_aio__await_buffer_write_completion(struct_ptr,
				                              struct_ptr->get);

            if ( result != SUCCEED ) {

                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
			"H5C_jb_aio__await_buffer_write_completion() failed.");
            }

            /* Update the last transaction written, and then set the 
             * transaction tracking array entry of the buffer whose 
             * write just completed to zero.
             */
            HDassert( struct_ptr->last_trans_written <= 
                      (*struct_ptr->trans_tracking)[struct_ptr->get] );

            struct_ptr->last_trans_written = (uint64_t)
	        (*struct_ptr->trans_tracking)[struct_ptr->get];

            (*struct_ptr->trans_tracking)[struct_ptr->get] = 0;

#if H5C_JB_AIO__AWAIT_COMPLETION_OF_ALL_PENDING_WRITES__DEBUG
            HDfprintf(stdout, 
                      "%s: last_trans_written = %lld, get/put = %d/%d\n", 
                      FUNC, (long long)(struct_ptr->last_trans_written),
                      struct_ptr->get, struct_ptr->put);
#endif /* H5C_JB_AIO__AWAIT_COMPLETION_OF_ALL_PENDING_WRITES__DEBUG */

            /* decrement writes in progress */
            struct_ptr->writes_in_progress--;

            /* decrement bufs in use */
            struct_ptr->bufs_in_use--;

            HDassert( 
	      ( struct_ptr->writes_in_progress == struct_ptr->bufs_in_use ) ||
              ( struct_ptr->writes_in_progress == struct_ptr->bufs_in_use - 1 ) );

            /* Update get to reference the next entry in the ring buffer
             * if get and put are not the same.
             *
             * If they are the same, verify that bufs_in_use and 
             * writes_in_progress are both zero, and then set done to TRUE
             */
            if ( struct_ptr->get != struct_ptr->put ) {

                struct_ptr->get = 
                    (struct_ptr->get + 1) % (struct_ptr->num_bufs);

            } else {

                HDassert( struct_ptr->bufs_in_use == 0 );
                HDassert( struct_ptr->writes_in_progress == 0 );

                done = TRUE;

            }
        } else {

            HDassert( struct_ptr->get == struct_ptr->put );

            done = TRUE;
        }
    } /* while */

    HDassert( struct_ptr->bufs_in_use <= 1 );

    HDassert( struct_ptr->writes_in_progress == 0 );

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb_aio__await_completion_of_all_pending_writes() */



/******************************************************************************
 *
 * Function:		H5C_jb_aio__flush
 *
 * Programmer:		John Mainzer
 *			1/11/10
 *
 * Purpose:		Verify that there is no transaction in progress and
 *			that aio is enabled. 
 *			
 *			Test to see if the current buffer (indicated by
 *			struct_ptr->put) is dirty.  If it is, queue a write
 *			of the current buffer.
 *
 *			Await completion of all outstanding writes.
 *
 * 			Sync the file.
 *
 *			Update struct_ptr->last_trans_on_disk.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

#define H5C_JB_AIO__FLUSH__DEBUG 0

static herr_t 
H5C_jb_aio__flush(H5C_jbrb_t * struct_ptr)
{
    hbool_t cur_buf_was_dirty = FALSE;
    herr_t result;
    herr_t ret_value = SUCCEED;
    uint64_t last_trans_in_ring_buffer;

    FUNC_ENTER_NOAPI(H5C_jb_aio__flush, FAIL)

    /* Check Arguments and status */
    HDassert( struct_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );
    HDassert( struct_ptr->use_aio );
    HDassert( struct_ptr->trans_in_prog == FALSE );
    HDassert( ( ( struct_ptr->writes_in_progress == struct_ptr->bufs_in_use ) 
                &&
                ( struct_ptr->bufs_in_use < struct_ptr->num_bufs ) 
                &&
                ( struct_ptr->cur_buf_free_space == struct_ptr->buf_size )
              )
              ||
              ( ( struct_ptr->writes_in_progress + 1 == struct_ptr->bufs_in_use )
                &&
                ( struct_ptr->cur_buf_free_space < struct_ptr->buf_size )
                &&
                ( struct_ptr->cur_buf_free_space > 0 )
              )
              ||
              ( ( struct_ptr->writes_in_progress == struct_ptr->bufs_in_use )
                &&
                ( struct_ptr->bufs_in_use == struct_ptr->num_bufs ) 
                &&
                ( struct_ptr->cur_buf_free_space == 0 )
              )
            );


    /* make note of the last transaction in the ring buffer */
    last_trans_in_ring_buffer = (*struct_ptr->trans_tracking)[struct_ptr->put];

#if H5C_JB_AIO__FLUSH__DEBUG
    HDfprintf(stdout, "%s: trans_tracking[%d] = %lld\n", FUNC, struct_ptr->get,
              (*struct_ptr->trans_tracking)[struct_ptr->get]);
    HDfprintf(stdout, "%s: trans_tracking[%d] = %lld\n", FUNC, struct_ptr->put,
              (*struct_ptr->trans_tracking)[struct_ptr->put]);
#endif /* H5C_JB_AIO__FLUSH__DEBUG */

    /* if the current buffer (indicated by struct_ptr->put) is dirty,
     * but not full, queue a write of the buffer.  The dirty part should
     * be obvious.  The not full part is required, as 
     * H5C_jb_aio__write_to_buffer() will have already queued the write
     * if the buffer is full.
     *
     * In passing, make note of whether the current buffer is dirty -- 
     * need to know this so we can setup the buffer properly after 
     * the flush.
     */
    if ( struct_ptr->cur_buf_free_space < struct_ptr->buf_size ) {

        cur_buf_was_dirty = TRUE;

        if ( struct_ptr->cur_buf_free_space > 0 ) {

            /* kick off an asynchronous write of the current buffer */
	    result = H5C_jb_aio__queue_buffer_write(struct_ptr,
				                     struct_ptr->put,
                                                     TRUE);

            if ( result != SUCCEED ) {

#if H5C_JB_AIO__FLUSH__DEBUG
		HDfprintf(stdout, 
                          "%s: H5C_jb_aio__queue_buffer_write() failed.\n", 
                          FUNC);
#endif /* H5C_JB_AIO__FLUSH__DEBUG */

                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
			    "H5C_jb_aio__queue_buffer_write() failed.");
            }
        }
    }

    /* await completion of all outstanding writes */

    result = H5C_jb_aio__await_completion_of_all_pending_writes(struct_ptr);

    if ( result != SUCCEED ) {

#if H5C_JB_AIO__FLUSH__DEBUG
	HDfprintf(stdout, 
         "%s: H5C_jb_aio__await_completion_of_all_pending_writes() failed.\n", 
          FUNC);
#endif /* H5C_JB_AIO__FLUSH__DEBUG */

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
	    "H5C_jb_aio__await_completion_of_all_pending_writes() failed.");
    }

    HDassert( struct_ptr->bufs_in_use == 0 );
    HDassert( struct_ptr->writes_in_progress == 0 );
    HDassert( struct_ptr->put == struct_ptr->get );

    /* sync out the file */

    result = H5C_jb_aio__sync_file(struct_ptr);

    if ( result != SUCCEED ) {

#if H5C_JB_AIO__FLUSH__DEBUG
	HDfprintf(stdout, 
                  "%s: H5C_jb_aio__sync_file() failed.\n", 
                  FUNC);
#endif /* H5C_JB_AIO__FLUSH__DEBUG */


        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "H5C_jb_aio__sync_file() failed.");
    }

    /* If the current buffer was dirty on entry, it was flushed and must 
     * be prepared for use.
     *
     * Don't call H5C_jb_aio__prep_next_buf_for_use() for this, as 
     * it assumes that the current buffer is full and dirty -- neither
     * of which is the case at present.
     *
     * further, H5C_jb_aio__prep_next_buf_for_use() will also 
     * increment put, which will cause problems if we don't increment
     * get as well.
     */
    if ( cur_buf_was_dirty ) {

        /* the following assignment is redundant if struct_ptr->use_aio_fsync
         * is TRUE, as struct_ptr->last_trans_on_disk will already be 
         * set correctly.  Verify this with an assert.
         */
#if 1 /* JRM */
        if ( ( struct_ptr->use_aio_fsync ) &&
             ( struct_ptr->last_trans_on_disk != last_trans_in_ring_buffer ) ) {

            HDfprintf(stdout, "%s: ltod = %lld, ltirb = %lld\n", 
                      FUNC, struct_ptr->last_trans_on_disk,
                      last_trans_in_ring_buffer);
        }
#endif /* JRM */
        HDassert( ( ! struct_ptr->use_aio_fsync ) ||
                  ( struct_ptr->last_trans_on_disk == 
                    last_trans_in_ring_buffer ) );
        struct_ptr->last_trans_on_disk = last_trans_in_ring_buffer;

        /* set the cur_buf_free_space */
        struct_ptr->cur_buf_free_space = struct_ptr->buf_size;

        /* set the head pointer to point to the beginning of the 
         * current buffer 
         */
        struct_ptr->head = (*struct_ptr->buf)[struct_ptr->put];

        /* load the transaction tracking array for the current buffer with the 
         * id of the last transaction fully written to the ring buffer.
         */
        HDassert( (*struct_ptr->trans_tracking)[struct_ptr->put] == 0 );

        (*struct_ptr->trans_tracking)[struct_ptr->put] = 
		last_trans_in_ring_buffer;

    } else {

	HDassert( struct_ptr->cur_buf_free_space == struct_ptr->buf_size );
        HDassert( struct_ptr->head == (*struct_ptr->buf)[struct_ptr->put] );
    }

    HDassert( struct_ptr->bufs_in_use == 0 );
    HDassert( struct_ptr->writes_in_progress == 0 );
    HDassert( struct_ptr->put == struct_ptr->get );

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb_aio__flush() */


/******************************************************************************
 *
 * Function:		H5C_jb_aio__get_last_transaction_on_disk
 *
 * Programmer:		John Mainzer
 *			1/19/10
 *
 * Purpose:		Determine the last transaction fully on disk,
 *			and place its transaction number in *trans_num_ptr. 
 *			If no transaction has made it to disk, load zero 
 *			into *trans_num_ptr.
 *
 *			If aio_fsync() is not available, stall pending 
 *			completion of all writes in progress, and then
 *			sync the file out using fsync().
 *
 *			If aio_fsync() is available, note any asynchronous
 *			syncs that have completed since the last check.
 *			This will update struct_ptr->last_trans_on_disk
 *			as appropriate.
 *
 *			In either case, return the ID of the last 
 *			transaction known to be on disk.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

static herr_t 
H5C_jb_aio__get_last_transaction_on_disk(H5C_jbrb_t * struct_ptr,
                                          uint64_t * trans_num_ptr)
{
    hbool_t ring_buffer_was_full = FALSE;
    herr_t result;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C_jb_aio__get_last_transaction_on_disk, FAIL)
	
    /* Check Arguments */
    HDassert( struct_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );
    HDassert( struct_ptr->use_aio );
    HDassert( trans_num_ptr != NULL );
    HDassert( ( ( struct_ptr->writes_in_progress == struct_ptr->bufs_in_use ) 
                &&
                ( struct_ptr->bufs_in_use < struct_ptr->num_bufs ) 
                &&
                ( struct_ptr->cur_buf_free_space == struct_ptr->buf_size )
              )
              ||
              ( ( struct_ptr->writes_in_progress + 1 == struct_ptr->bufs_in_use )
                &&
                ( struct_ptr->cur_buf_free_space < struct_ptr->buf_size )
                &&
                ( struct_ptr->cur_buf_free_space > 0 )
              )
              ||
              ( ( struct_ptr->writes_in_progress == struct_ptr->bufs_in_use )
                &&
                ( struct_ptr->bufs_in_use == struct_ptr->num_bufs ) 
                &&
                ( struct_ptr->cur_buf_free_space == 0 )
              )
            );

    if ( struct_ptr->use_aio_fsync ) {

        result = H5C_jb_aio__note_completed_async_buffer_writes(struct_ptr);

        if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "H5C_jb_aio__note_completed_async_buffer_writes() failed.")
        }

        result = H5C_jb_aio__note_completed_async_fsyncs(struct_ptr);

        if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                        "H5C_jb_aio__note_completed_async_fsyncs() failed.")
        }
    } else {

        /* aio_fsync() is not available */

        if ( struct_ptr->writes_in_progress == struct_ptr->num_bufs )  {

            ring_buffer_was_full = TRUE;
        }

        /* await completion of all outstanding writes */

        result = 
	    H5C_jb_aio__await_completion_of_all_pending_writes(struct_ptr);

        if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
	       "H5C_jb_aio__await_completion_of_all_pending_writes() failed.");
        }

        HDassert( struct_ptr->bufs_in_use <= 1 );
        HDassert( ( struct_ptr->get == struct_ptr->put ) ||
                  ( ((struct_ptr->get + 1) % struct_ptr->num_bufs) == 
                    struct_ptr->put ) );

        /* sync out the file */

        result = H5C_jb_aio__sync_file(struct_ptr);

        if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                        "H5C_jb_aio__sync_file() failed.");
        }


        /* update last transaction on disk */

        struct_ptr->last_trans_on_disk = struct_ptr->last_trans_written;


        /* if the ring buffer was full, put was not advanced to the next 
         * buffer in the ring buffer, nor were the associated fields updated
         * to refer to the next buffer.  
         *
         * In this case, we must prepare the next buffer for use.
         *
         * Don't call H5C_jb_aio__prep_next_buf_for_use() for this, as 
         * it assumes that the current buffer is full and dirty -- neither
         * of which is the case at present.
         *
         * further, H5C_jb_aio__prep_next_buf_for_use() will also 
         * increment put, which will cause problems if we don't increment
         * get as well.
         */
        if ( ring_buffer_was_full ) {

            HDassert( struct_ptr->bufs_in_use == 0 );
            HDassert( struct_ptr->get == struct_ptr->put );

            /* set the cur_buf_free_space */
            struct_ptr->cur_buf_free_space = struct_ptr->buf_size;

            /* set the head pointer to point to the beginning of the 
             * current buffer 
             */
            struct_ptr->head = (*struct_ptr->buf)[struct_ptr->put];

            /* load the transaction tracking array for the current buffer 
             * with the id of the last transaction fully written to the 
             * ring buffer.
             */
            HDassert( (*struct_ptr->trans_tracking)[struct_ptr->put] == 0 );

            (*struct_ptr->trans_tracking)[struct_ptr->put] = 
		    struct_ptr->last_trans_written;

        }
    }

    /* report last trans on disk */

    *trans_num_ptr = struct_ptr->last_trans_on_disk;

    /* closing sanity check */

    HDassert( ( ( struct_ptr->writes_in_progress == struct_ptr->bufs_in_use ) 
                &&
                ( struct_ptr->bufs_in_use < struct_ptr->num_bufs ) 
                &&
                ( struct_ptr->cur_buf_free_space == struct_ptr->buf_size )
              )
              ||
              ( ( struct_ptr->writes_in_progress + 1 == struct_ptr->bufs_in_use )
                &&
                ( struct_ptr->cur_buf_free_space < struct_ptr->buf_size )
                &&
                ( struct_ptr->cur_buf_free_space > 0 )
              )
              ||
              ( ( struct_ptr->writes_in_progress == struct_ptr->bufs_in_use )
                &&
                ( struct_ptr->bufs_in_use == struct_ptr->num_bufs ) 
                &&
                ( struct_ptr->cur_buf_free_space == 0 )
              )
            );

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb_aio__get_last_transaction_on_disk */


/******************************************************************************
 *
 * Function:		H5C_jb_aio__make_space_in_ring_buffer()
 *
 * Programmer:		John Mainzer
 *
 * Purpose:		Make at least one buffer available for writing.
 *			
 *			Do this by awaiting completion of the oldest 
 *			asynchronous write, and then marking this buffer
 *			as available on the ring buffer.
 *
 *			Then repeatedly test the remaining oldest write
 *			to see if it has completed, and mark its buffer
 *			available if it has until either a write that 
 *			has not completed is encountered, or we run out
 *			of writes in progress.
 *
 *			Note that this function presumes that it will not
 *			be called unless the ring buffer is completely full.
 *			The function will fail if this is not the case.
 *
 * Returns:		SUCCEED if no errors are encountered.
 *
 ******************************************************************************/

static herr_t 
H5C_jb_aio__make_space_in_ring_buffer(H5C_jbrb_t * struct_ptr)
{
    herr_t ret_value = SUCCEED;
    herr_t result;
    uint64_t last_trans_in_ring_buffer;

    FUNC_ENTER_NOAPI(H5C_jb_aio__make_space_in_ring_buffer, FAIL)

    HDassert( struct_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );
    HDassert( struct_ptr->use_aio == TRUE );
    HDassert( struct_ptr->bufs_in_use == struct_ptr->num_bufs );
    HDassert( struct_ptr->bufs_in_use == struct_ptr->writes_in_progress );
    HDassert( ((struct_ptr->put + 1) % struct_ptr->num_bufs) == 
              struct_ptr->get );
    HDassert( struct_ptr->cur_buf_free_space == 0 );

    /* free up the oldest (or least recently dirtied) buffer */
    result = H5C_jb_aio__await_buffer_write_completion(struct_ptr,
				                        struct_ptr->get);

    if ( result != SUCCEED ) {
#if 1 /* JRM */
        HDassert(FALSE);
#endif /* JRM */
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "H5C_jb_aio__await_buffer_write_completion() failed.")
    }

    /* Update the last transaction written, and then set the transaction 
     * tracking array entry of the buffer whose write just completed to zero.
     */
    HDassert( struct_ptr->last_trans_written <= 
              (*struct_ptr->trans_tracking)[struct_ptr->get] );
    struct_ptr->last_trans_written = 
	(*struct_ptr->trans_tracking)[struct_ptr->get];
    (*struct_ptr->trans_tracking)[struct_ptr->get] = 0;

    /* decrements writes in progress */
    struct_ptr->writes_in_progress--;

    /* decrement bufs in use */
    struct_ptr->bufs_in_use--;

    HDassert( ( struct_ptr->writes_in_progress == struct_ptr->bufs_in_use ) ||
              ( struct_ptr->writes_in_progress == struct_ptr->bufs_in_use - 1 ) );

    /* update get to reference the next entry in the ring buffer */
    struct_ptr->get = (struct_ptr->get + 1) % (struct_ptr->num_bufs);

    /* Since we now have a free buffer, prepare it for use */

    last_trans_in_ring_buffer = (*struct_ptr->trans_tracking)[struct_ptr->put];

    result = H5C_jb_aio__prep_next_buf_for_use(struct_ptr, 
                                                last_trans_in_ring_buffer);

    if ( result != SUCCEED ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "H5C_jb_aio__prep_next_buf_for_use() failed.")
    }

    HDassert( ((struct_ptr->put + 1) % struct_ptr->num_bufs) == 
              struct_ptr->get );
    HDassert( struct_ptr->bufs_in_use + 1 == struct_ptr->num_bufs );

    /* now scan through the ring buffer marking buffers as available
     * until we either hit a buffer whose write is still in progress,
     * or all buffers are available for re-use.
     */

#if 1 /* JRM */

    result = H5C_jb_aio__note_completed_async_buffer_writes(struct_ptr);

    if ( result != SUCCEED ) {
#if 1 /* JRM */
        HDassert(FALSE);
#endif /* JRM */
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "H5C_jb_aio__note_completed_async_buffer_writes() failed.")
    }

#else /* delete this branch if all goes well -- JRM */
{
    hbool_t done = FALSE;
    hbool_t buf_write_complete;

    while ( ! done ) {

        buf_write_complete = FALSE;

        result = H5C_jb_aio__test_buffer_write_complete(struct_ptr,
				                         struct_ptr->get,
                                                         &buf_write_complete);

        if ( result != SUCCEED ) {
#if 1 /* JRM */
            HDassert(FALSE);
#endif /* JRM */
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                        "H5C_jb_aio__test_buffer_write_complete() failed.")
        }

        if ( buf_write_complete ) {

            /* decrements writes in progress */
            struct_ptr->writes_in_progress--;

            /* decrement bufs in use */
            struct_ptr->bufs_in_use--;

            /* Update the last transaction written, and then set the 
             * transaction tracking array entry of the buffer whose 
             * write just completed to zero.
             */
            HDassert( struct_ptr->last_trans_written <= 
                      (*struct_ptr->trans_tracking)[struct_ptr->get] );

            struct_ptr->last_trans_written = 
	        (*struct_ptr->trans_tracking)[struct_ptr->get];

            (*struct_ptr->trans_tracking)[struct_ptr->get] = 0;

            HDassert( struct_ptr->writes_in_progress == 
                      struct_ptr->bufs_in_use );

            /* mark the buffer as available */

            struct_ptr->get = (struct_ptr->get + 1) % (struct_ptr->num_bufs);

            if ( struct_ptr->bufs_in_use == 0 ) {

                /* all buffer writes are complete */

                HDassert( struct_ptr->put == struct_ptr->get );
                done = TRUE;

            } else { 

                HDassert( struct_ptr->put != struct_ptr->get );

            }
        } else { 

            /* we have hit a buffer whose write is still in progress */
            done = TRUE;
        }
    }

}
#endif /* JRM */

    HDassert( struct_ptr->writes_in_progress == struct_ptr->bufs_in_use );

    if ( struct_ptr->use_aio_fsync ) {

        result = H5C_jb_aio__note_completed_async_fsyncs(struct_ptr);

        if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                        "H5C_jb_aio__note_completed_async_fsyncs() failed.")
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb_aio__make_space_in_ring_buffer() */


/******************************************************************************
 *
 * Function:		H5C_jb_aio__note_completed_async_buffer_writes
 *
 * Programmer:		John Mainzer
 *			2/10/10
 *
 * Purpose:		Verify that AIO is enabled.
 *
 * 			Then, if there are any writes in progress, check to
 *			see if the oldest one has completed.  If it has,
 *			update *struct_ptr to reflect this.  Specifically
 *			update the last_trans_written, put, and 
 *			writes_in_progress fields to reflect the completion
 *			of the write.
 *
 *			Repeat until there are no writes in progress, or 
 *			the oldest write is still in progress.
 *
 * Returns:		SUCCEED on success.
 *			FAIL otherwise.
 *
 ******************************************************************************/

#define H5C_JB_AIO__NOTE_COMPLETED_ASYNC_BUFFER_WRITES__DEBUG 0

static herr_t 
H5C_jb_aio__note_completed_async_buffer_writes(H5C_jbrb_t * struct_ptr)
{
    herr_t result;
    herr_t ret_value = SUCCEED;
    hbool_t write_completed = TRUE;

    FUNC_ENTER_NOAPI(H5C_jb_aio__note_completed_async_buffer_writes, FAIL)

    HDassert( struct_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );
    HDassert( struct_ptr->use_aio );

    while ( ( struct_ptr->writes_in_progress > 0 ) &&
            ( write_completed ) ) {

        result = H5C_jb_aio__test_buffer_write_complete(struct_ptr, 
                                                         struct_ptr->get,
                                                         &write_completed);

        if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                        "H5C_jb_aio__test_buffer_write_complete() failed.")
        }

        if ( write_completed ) {

	    H5C__JBRB__UPDATE_STATS_FOR_BUF_WRITE_COMPLETE(struct_ptr, FALSE)

            /* Update the last transaction written, and then set the
             * transaction tracking array entry of the buffer whose
             * write just completed to zero.
             */
            HDassert( struct_ptr->last_trans_written <=
                      (*struct_ptr->trans_tracking)[struct_ptr->get] );

            struct_ptr->last_trans_written =
                (*struct_ptr->trans_tracking)[struct_ptr->get];

            (*struct_ptr->trans_tracking)[struct_ptr->get] = 0;


            /* decrements writes in progress */
            struct_ptr->writes_in_progress--;

            /* decrement bufs in use */
            struct_ptr->bufs_in_use--;

            HDassert( ( struct_ptr->writes_in_progress == 
                        struct_ptr->bufs_in_use ) 
                      ||
                      (  struct_ptr->writes_in_progress + 1 ==
                         struct_ptr->bufs_in_use ) );

            /* mark the buffer as available */

            struct_ptr->get = (struct_ptr->get + 1) % (struct_ptr->num_bufs);

#if H5C_JB_AIO__NOTE_COMPLETED_ASYNC_BUFFER_WRITES__DEBUG 
            if ( ! ( ( ( struct_ptr->bufs_in_use == 0 ) 
                       &&
                       ( struct_ptr->put == struct_ptr->get ) 
                     )
                     ||
                     ( ( struct_ptr->bufs_in_use > 0 ) 
                       &&
                       ( struct_ptr->put != struct_ptr->get ) 
                     )
                   )
               ) {
                   HDfprintf(stdout, "%s: biu/put/get = %d/%d/%d.\n",
                             FUNC, 
                             (int)(struct_ptr->bufs_in_use),
                             (int)(struct_ptr->put),
                             (int)(struct_ptr->get));
               } 
#endif /* H5C_JB_AIO__NOTE_COMPLETED_ASYNC_BUFFER_WRITES__DEBUG */

            HDassert( ( ( struct_ptr->bufs_in_use == 0 ) 
                        &&
                        ( struct_ptr->put == struct_ptr->get ) 
                      )
                      ||
                      ( ( struct_ptr->bufs_in_use == 1 )
                        &&
                        ( struct_ptr->cur_buf_free_space > 0 )
                        &&
                        ( struct_ptr->put == struct_ptr->get )
                      )
                      ||
                      ( ( struct_ptr->bufs_in_use > 0 ) 
                        &&
                        ( struct_ptr->put != struct_ptr->get ) 
                      )
                    );
        }
    } /* while */

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb_aio__note_completed_async_buffer_writes() */


/******************************************************************************
 *
 * Function:		H5C_jb_aio__note_completed_async_fsyncs
 *
 * Programmer:		John Mainzer
 *			2/10/10
 *
 * Purpose:		Verify that AIO is enabled, and that struct_ptr->
 *			use_aio_fsync is TRUE.
 *
 * 			Then, if the sync queue is not empty, test to see 
 *			if the asynchronous fsync associated with the 
 *			instance of struct H5C_jbrb_sync_q_entry_t at the 
 *			head of the sync queue has completed.  
 *
 *			If it hasn't, return.
 *
 *			If it has, update struct_ptr->last_trans_on_disk,
 *			discard the head of the sync queue, and repeat if
 *			the sync queue is not empty..
 *
 * Returns:		SUCCEED on success.
 *			FAIL otherwise.
 *
 * Changes:		Modified the function to never let the sync queue
 *			contain fewer entryies than 
 *			struct_ptr->writes_in_progress.
 *
 *			This forces us to complete each buffer write before
 *			we complete the associated aio_fsync() -- and 
 *			maintains the expected relationship between 
 *			last_trans_queued, last_trans_written, and 
 *			last_trans_on_disk.
 *
 ******************************************************************************/

static herr_t 
H5C_jb_aio__note_completed_async_fsyncs(H5C_jbrb_t * struct_ptr)
{
    herr_t result;
    herr_t ret_value = SUCCEED;
    hbool_t sync_completed = TRUE;

    FUNC_ENTER_NOAPI(H5C_jb_aio__note_completed_async_fsyncs, FAIL)

    HDassert( struct_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );
    HDassert( struct_ptr->use_aio );
    HDassert( struct_ptr->use_aio_fsync );

    while ( ( struct_ptr->aio_sync_q_len > 0 ) &&
            ( struct_ptr->aio_sync_q_len > struct_ptr->writes_in_progress ) &&
            ( sync_completed ) ) {

        result = H5C_jb_aio__test_next_async_fsync_complete(struct_ptr, 
                                                             &sync_completed);

        if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                        "H5C_jb_aio__test_next_async_fsync_complete() failed.")
        }
    } /* while */

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb_aio__note_completed_async_fsyncs() */


/******************************************************************************
 *
 * Function:		H5C_jb_aio__prep_next_buf_for_use()
 *
 * Programmer:		John Mainzer
 *
 * Purpose:		Prepare the next free buffer in the ring buffer
 *			for use.  
 *
 *			Note that this function assumes that there
 *			is a next free buffer, and will fail if there is 
 *			not.
 *
 *			Note also that this function is for use with AIO
 *			only.  In the SIO case, we do some extra book keeping
 *			which is not managed here.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

static herr_t 
H5C_jb_aio__prep_next_buf_for_use(H5C_jbrb_t * struct_ptr,
                                   uint64_t last_trans_in_ring_buffer)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C_jb_aio__prep_next_buf_for_use, FAIL)

    HDassert( struct_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );
    HDassert( struct_ptr->use_aio == TRUE );

    if ( ( struct_ptr->bufs_in_use >= struct_ptr->num_bufs ) ||
         ( struct_ptr->cur_buf_free_space != 0 ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "no free buffer or space left in current buffer.")
    }

    /* increment put to reference the next buffer in the ring buffer */
    struct_ptr->put = (struct_ptr->put + 1) % (struct_ptr->num_bufs);

    /* set the cur_buf_free_space */
    struct_ptr->cur_buf_free_space = struct_ptr->buf_size;

    /* set the head pointer to point to the beginning of the newly 
     * available buffer.
     */
    struct_ptr->head = (*struct_ptr->buf)[struct_ptr->put];

    /* load the transaction tracking array for the current buffer with the 
     * id of the last transaction fully written to the ring buffer.
     */
    HDassert( (*struct_ptr->trans_tracking)[struct_ptr->put] == 0 );

    (*struct_ptr->trans_tracking)[struct_ptr->put] = last_trans_in_ring_buffer;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb_aio__prep_next_buf_for_use() */


/******************************************************************************
 *
 * Function:		H5C_jb_aio__queue_async_fsync()
 *
 * Programmer:		John Mainzer
 *
 * Purpose:		Queue an asynchronous fsync -- if possible.
 *
 *			Verify that AIO is enabled, and that use_aio_fsync
 *			is TRUE.
 *
 *			Then allocate an instance of H5C_jbrb_sync_q_entry_t,
 *			load it with the last transaction queued and a 
 *			correctly configured aio control block, and attempt
 *			to queue an asynchronous fsync via aio_fsync().
 *
 *			If aio_fsync() is not supported, (i.e. it fails 
 *			with ENOSYS or EINVAL), set struct_ptr->use_aio_fsync 
 *			to FALSE, discard the instance of 
 *			H5C_jbrb_sync_q_entry_t and return.
 *
 *			if aio_fsync() fails with EAGAIN, retry until either
 *			success, failure with some other error, or the retry
 *			limit is exceeded.  In the latter two cases, flag
 *			an error and quit.
 *
 *			If aio_fsync() fails with any error other than 
 *			EAGAIN, EINVAL, or ENOSYS, flag an error and quit.
 *
 *			If the aio_fsync() is queued successfully, add the
 *			instance of H5C_jbrb_sync_q_entry_t to the tail of 
 *			the aio sync queue, and then return.  
 *
 *			If the instance	of H5C_jbrb_sync_q_entry_t is 
 *			allocated, but the call to aio_fsync() fails for 
 *			any reason, discard the instance of 
 *			H5C_jbrb_sync_q_entry_t before exiting.
 *
 *						JRM -- 2/8/10
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

#define H5C_JB_AIO__QUEUE_ASYNC_FSYNC__DEBUG 0
#define AIO_FSYNC_MAX_RETRIES	120

static herr_t 
H5C_jb_aio__queue_async_fsync(H5C_jbrb_t * struct_ptr)
{
    herr_t ret_value = SUCCEED;
    herr_t herr_result;
    hbool_t sync_complete;
    hbool_t sync_queued = FALSE;
    int result;
    int retries = -1;
    struct H5C_jbrb_sync_q_entry_t * entry_ptr = NULL;

    HDassert( struct_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );
    HDassert( struct_ptr->journal_file_fd >= 0 );
    HDassert( struct_ptr->use_aio );
    HDassert( struct_ptr->use_aio_fsync );

    FUNC_ENTER_NOAPI(H5C_jb_aio__queue_async_fsync, FAIL)

    entry_ptr = (struct H5C_jbrb_sync_q_entry_t *) 
	H5MM_malloc(sizeof(struct H5C_jbrb_sync_q_entry_t));

    if ( entry_ptr == NULL ) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                    "memory allocation failed for aio sync queue entry.")
    }

    entry_ptr->magic = H5C__H5C_JBRB_SYNC_Q_T_MAGIC;
    entry_ptr->last_trans_in_sync = struct_ptr->last_trans_queued;
    bzero((void *)(&(entry_ptr->ctl_blk)), sizeof(struct aiocb));
    entry_ptr->ctl_blk.aio_fildes = struct_ptr->journal_file_fd;
    entry_ptr->next = NULL;

    do {

        result = aio_fsync(O_SYNC, &(entry_ptr->ctl_blk));

        if ( result != 0 ) {

            if ( ( errno == EINVAL ) ||
                 ( errno == ENOSYS ) ) {

#if H5C_JB_AIO__QUEUE_ASYNC_FSYNC__DEBUG
	        HDfprintf(stdout, 
                      "%s: aio_fsync() not supported. errno = %d (%s)\n",
                      FUNC, errno, strerror(errno));
#endif /* H5C_JB_AIO__QUEUE_ASYNC_FSYNC__DEBUG */
                HDassert( struct_ptr->aio_sync_q_len == 0 );
                struct_ptr->use_aio_fsync = FALSE;

	    } else if ( errno == EAGAIN ) {

		retries++;

                if ( retries > AIO_FSYNC_MAX_RETRIES ) {

#if H5C_JB_AIO__QUEUE_ASYNC_FSYNC__DEBUG
                    HDfprintf(stdout, 
			  "%s: retry limit on calls to aio_fsync() exceeded\n",
                          FUNC);
#endif /* H5C_JB_AIO__QUEUE_ASYNC_FSYNC__DEBUG */

                    HGOTO_ERROR(H5E_IO, H5E_SYNCFAIL, FAIL,  \
                        "retry limit on calls to aio_fsync() exceeded.")
                }

                /* if we get errno == EAGAIN, it is possible that 
                 * the problem is too many aio operations in progress.
                 * Thus, if the sync queue is not empty, check to 
                 * see if an asynchronous sync has completed, and 
                 * retire it if it has.
                 */
                if ( struct_ptr->aio_sync_q_len > 0 ) {

                    herr_result =  
			H5C_jb_aio__test_next_async_fsync_complete(struct_ptr,
                                                               &sync_complete);
                }
	    } else {

#if H5C_JB_AIO__QUEUE_ASYNC_FSYNC__DEBUG
	        HDfprintf(stdout, 
                      "%s: aio_fsync() failed. errno = %d (%s)\n",
                      FUNC, errno, strerror(errno));
#endif /* H5C_JB_AIO__QUEUE_ASYNC_FSYNC__DEBUG */

                HGOTO_ERROR(H5E_IO, H5E_SYNCFAIL, FAIL, \
		        "call to aio_fsync() failed.")
            }
        }
    } while ( ( result != 0 ) && 
              ( struct_ptr->use_aio_fsync ) );

    if ( result == 0 ) {

	herr_result = H5C_jb_aio__sync_q__append(struct_ptr, entry_ptr);

        if ( herr_result != SUCCEED ) {

             HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                         "H5C_jb_aio__sync_q__append() failed.")

        }

        sync_queued = TRUE;

	H5C__JBRB__UPDATE_STATS_FOR_ASYNC_SYNCS_QUEUED(struct_ptr);

#if H5C_JB_AIO__QUEUE_ASYNC_FSYNC__DEBUG
        HDfprintf(stdout, 
                  "%s: queued async fsync. last trans = %lld, q_len = %lld.\n",
                  FUNC, 
                  (long long)(entry_ptr->last_trans_in_sync), 
                  (long long)(struct_ptr->aio_sync_q_len));
#endif /* H5C_JB_AIO__QUEUE_ASYNC_FSYNC__DEBUG */
		
    }
done:

    if ( ( entry_ptr != NULL ) && 
         ( ! sync_queued ) ) { /* discard *entry_ptr */

        entry_ptr->magic = 0;
        entry_ptr = (struct H5C_jbrb_sync_q_entry_t *)H5MM_xfree(entry_ptr);

        if ( entry_ptr != NULL ) {

            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "free of entry_ptr failed.");
        }
    }

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb_aio__queue_async_fsync() */


/******************************************************************************
 *
 * Function:		H5C_jb_aio__queue_buffer_write
 *
 * Programmer:		John Mainzer
 *
 * Purpose:		Queue an asynchronous write of the specified buffer,
 *			and update struct_ptr->last_trans_queue if appropriate.
 *			If struct_ptr->use_aio_fsync is TRUE, queue and 
 *			asynchronous fsync after the buffer write has
 *			been queued.
 *
 *			Verify that AIO is enabled, that buffer specified
 *			exists, and is not already involved in an 
 *			asynchronous write. 
 *
 *			Further verify that the buffer is not empty, and that
 *			either partial_write_ok is TRUE, or the buffer is full.
 *
 * Returns:		SUCCEED on success.
 *			FAIL otherwise
 *
 ******************************************************************************/

#define H5C_JB_AIO__QUEUE_BUFFER_WRITE__DEBUG 0
#define AIO_WRITE_MAX_RETRIES	120

static herr_t 
H5C_jb_aio__queue_buffer_write(H5C_jbrb_t * struct_ptr,
		 	        int buf_num,
                                hbool_t partial_write_ok)
{
    int result;
    int retries = -1;
#if H5C_JB_AIO__QUEUE_BUFFER_WRITE__DEBUG
    static int writes_queued = 0;
#endif /* H5C_JB_AIO__QUEUE_BUFFER_WRITE__DEBUG */
    uint64_t last_trans_in_buf;
    herr_t herr_result;
    herr_t ret_value = SUCCEED;
    size_t bytes_to_write;
    void * buf_ptr = NULL;
    struct aiocb * aiocb_ptr = NULL;

    HDassert( struct_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );
    HDassert( struct_ptr->buf_size > 0 );
    HDassert( struct_ptr->journal_file_fd >= 0 );
    HDassert( struct_ptr->use_aio );
    HDassert( struct_ptr->trans_tracking != NULL );
    HDassert( struct_ptr->aio_ctl_blks != NULL );
    HDassert( buf_num >= 0 );
    HDassert( buf_num < struct_ptr->num_bufs );

    FUNC_ENTER_NOAPI(H5C_jb_aio__queue_buffer_write, FAIL)

    if ( struct_ptr->cur_buf_free_space >= struct_ptr->buf_size ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "attempt to queue write of an empty buffer.")
    }

    if ( ( struct_ptr->cur_buf_free_space > 0 ) && 
         ( ! partial_write_ok ) ) {

#if H5C_JB_AIO__QUEUE_BUFFER_WRITE__DEBUG
	HDfprintf(stdout, 
                  "%s: buffer not full and partial_write_ok == FALSE.\n",
                  FUNC);
#endif /* H5C_JB_AIO__QUEUE_BUFFER_WRITE__DEBUG */

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "buffer not full and partial_write_ok == FALSE.")
    }

    last_trans_in_buf = (*struct_ptr->trans_tracking)[struct_ptr->put];

    HDassert( last_trans_in_buf >= struct_ptr->last_trans_queued );
    HDassert( struct_ptr->last_trans_queued >= struct_ptr->last_trans_written );
    HDassert( struct_ptr->last_trans_written >= 
              struct_ptr->last_trans_on_disk );

    aiocb_ptr = &((*(struct_ptr->aio_ctl_blks))[buf_num]);

    if ( aiocb_ptr->aio_fildes != -1 ) {

#if H5C_JB_AIO__QUEUE_BUFFER_WRITE__DEBUG
	HDfprintf(stdout, 
                  "%s: AIO write alread in progress for target buffer?\n",
                  FUNC);
#endif /* H5C_JB_AIO__QUEUE_BUFFER_WRITE__DEBUG */

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "AIO write alread in progress for target buffer?")
    }

    buf_ptr = (void *)((*struct_ptr->buf)[buf_num]);

    if ( buf_ptr == NULL ) {

#if H5C_JB_AIO__QUEUE_BUFFER_WRITE__DEBUG
	HDfprintf(stdout, 
                  "%s: ((*struct_ptr->buf)[buf_num]) == NULL?!?\n",
                  FUNC);
#endif /* H5C_JB_AIO__QUEUE_BUFFER_WRITE__DEBUG */

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
		    "((*struct_ptr->buf)[buf_num]) == NULL?!?")
    }

    HDassert( struct_ptr->buf_size > struct_ptr->cur_buf_free_space);

    bytes_to_write = struct_ptr->buf_size - struct_ptr->cur_buf_free_space;

    /* all looks good -- setup to initiate the write ... */
    
    bzero((void *)aiocb_ptr, sizeof(struct aiocb));

    aiocb_ptr->aio_fildes = struct_ptr->journal_file_fd;
    aiocb_ptr->aio_offset = struct_ptr->aio_next_buf_offset;
    aiocb_ptr->aio_buf    = buf_ptr;
    aiocb_ptr->aio_nbytes = bytes_to_write;

    /* ... and kick it off */
    do {

        result = aio_write(aiocb_ptr);

        if ( result != 0 ) {

	    if ( errno == EAGAIN ) {

                sleep(1);
		retries++;

	    } else {

#if H5C_JB_AIO__QUEUE_BUFFER_WRITE__DEBUG
	        HDfprintf(stdout, 
                      "%s: aio_write(aiocb_ptr) failed. errno = %d (%s)\n",
                      FUNC, errno, strerror(errno));
                HDfprintf(stdout, "%s: offset/size = %lld/%d\n", 
                          FUNC,
                          (long long)struct_ptr->aio_next_buf_offset,
                          (int)bytes_to_write);
#endif /* H5C_JB_AIO__QUEUE_BUFFER_WRITE__DEBUG */

                HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
		        "call to aio_write() failed.")
            }
        }
    } while ( ( result != 0 ) && ( retries <= AIO_WRITE_MAX_RETRIES ) );

#if H5C_JB_AIO__QUEUE_BUFFER_WRITE__DEBUG
    if ( retries > 0 ) {

        HDfprintf(stdout, 
                  "%s: aio_write() retries = %d, writes queued = %d\n", 
                  FUNC, retries, writes_queued);
        HDfprintf(stdout, "%s: offset/size = %lld/%d\n", FUNC,
                  (long long)struct_ptr->aio_next_buf_offset,
                  (int)bytes_to_write);
    } else {
        HDfprintf(stdout, "%s: aio_write(): offset/size = %lld/%d\n", FUNC,
                  (long long)struct_ptr->aio_next_buf_offset,
                  (int)bytes_to_write);
    }
#endif /* H5C_JB_AIO__QUEUE_BUFFER_WRITE__DEBUG */


    if ( ( result != 0 ) && ( retries > AIO_WRITE_MAX_RETRIES ) ) {

#if H5C_JB_AIO__QUEUE_BUFFER_WRITE__DEBUG
        HDfprintf(stdout, "%s: retry limit on calls to aio_write() exceeded\n",
                   FUNC);
#endif /* H5C_JB_AIO__QUEUE_BUFFER_WRITE__DEBUG */

        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
                    "retry limit on calls to aio_write() exceeded.")
    }

#if H5C_JB_AIO__QUEUE_BUFFER_WRITE__DEBUG
        writes_queued++;
#endif /* H5C_JB_AIO__QUEUE_BUFFER_WRITE__DEBUG */

    H5C__JBRB__UPDATE_STATS_FOR_BUF_WRITE_QUEUED(struct_ptr, \
				(struct_ptr->cur_buf_free_space > 0 ))

    /* note that another write is in progress */
    struct_ptr->writes_in_progress++;
#if H5C_JB_AIO__QUEUE_BUFFER_WRITE__DEBUG
    if ( struct_ptr->writes_in_progress != struct_ptr->bufs_in_use ) {
        HDfprintf(stdout, "%s: wip = %d, biu = %d.\n", FUNC,
                  struct_ptr->writes_in_progress,
                  struct_ptr->bufs_in_use);
    } 
#endif /* H5C_JB_AIO__QUEUE_BUFFER_WRITE__DEBUG */
    HDassert( struct_ptr->writes_in_progress == struct_ptr->bufs_in_use );

    /* update struct_ptr->last_trans_queued */
    struct_ptr->last_trans_queued = last_trans_in_buf;

    /* update struct_ptr->aio_next_buf_offset */
    struct_ptr->aio_next_buf_offset += bytes_to_write;

    /* if aio_fsync() is available, queue a sync to force the 
     * data just written to disk.
     */
    if ( struct_ptr->use_aio_fsync ) {

        herr_result = H5C_jb_aio__queue_async_fsync(struct_ptr);

        if ( herr_result != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                        "H5C_jb_aio__queue_async_fsync() failed.")
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb_aio__queue_buffer_write() */


/******************************************************************************
 *
 * Function:		H5C_jb_aio__sync_file()
 *
 * Programmer:		John Mainzer
 *			1/13/10
 *
 * Purpose:		Sync out the journal file.
 *
 *			Verify that AIO is enabled, and that all pending 
 *			writes are complete.  
 *
 *			Note that this last precondition should not be 
 *			necessary.  However, given the incomplete state
 *			of many AIO implementation, it seems wise to 
 *			allow all writes to complete before calling fsync().
 *
 *			If struct_ptr->use_aio_fsync is TRUE, await 
 *			completion of all asynchronous fsyncs on the sync
 *			queue.  When this is done, the file should be 
 *			synced.
 *
 *			If struct_ptr->use_aio_fsync is FALSE, call 
 *			fsync().
 *
 * Returns:		SUCCEED if no errors are detected, 
 *			FAIL otherwise.
 *
 ******************************************************************************/

#define H5C_JB_AIO__SYNC_FILE__DEBUG 0

static herr_t 
H5C_jb_aio__sync_file(H5C_jbrb_t * struct_ptr)
{
    int result;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C_jb_aio__sync_file, FAIL)

    HDassert( struct_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );
    HDassert( struct_ptr->journal_file_fd >= 0 );
    HDassert( struct_ptr->use_aio );

    if ( struct_ptr->writes_in_progress != 0 ) {

#if H5C_JB_AIO__SYNC_FILE__DEBUG 
        HDfprintf(stdout, "%s: async write in progress on entry.\n", FUNC);
#endif /* H5C_JB_AIO__SYNC_FILE__DEBUG */

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "async write in progress on entry")
    }

    if ( struct_ptr->use_aio_fsync ) {

        result = H5C_jb_aio__await_completion_of_all_async_fsyncs(struct_ptr);

        if ( result != 0 ) {

#if H5C_JB_AIO__SYNC_FILE__DEBUG 
            HDfprintf(stdout, 
	    "%s: H5C_jb_aio__await_completion_of_all_async_fsyncs() failed.\n",
            FUNC);
#endif /* H5C_JB_AIO__SYNC_FILE__DEBUG */

            HGOTO_ERROR(H5E_IO, H5E_SYNCFAIL, FAIL,  \
                "H5C_jb_aio__await_completion_of_all_async_fsyncs() failed.");
        }
    } else {

        result = fsync(struct_ptr->journal_file_fd);

        if ( result != 0 ) {

#if H5C_JB_AIO__SYNC_FILE__DEBUG 
            HDfprintf(stdout, "%s: fsync() failed.\n", FUNC);
#endif /* H5C_JB_AIO__SYNC_FILE__DEBUG */

            HGOTO_ERROR(H5E_IO, H5E_SYNCFAIL, FAIL,  "fsync() failed.");
        }

	H5C__JBRB__UPDATE_STATS_FOR_CALL_TO_FSYNC(struct_ptr)
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb_aio__sync_file() */


/******************************************************************************
 *
 * Function:		H5C_jb_aio__sync_q__append()
 *
 * Purpose:		Append an instance of H5C_jbrb_sync_q_entry_t to
 *			the sync queue.
 *
 *			Verify that AIO is enabled, and that the supplied
 *			instance of H5C_jbrb_sync_q_entry_t has the correct
 *			magic value.  Then append to the end of the sync 
 *			queue.
 *							JRM -- 2/9/10
 *
 * Returns:		SUCCEED if no errors are detected, 
 *			FAIL otherwise.
 *
 ******************************************************************************/

static herr_t 
H5C_jb_aio__sync_q__append(H5C_jbrb_t * struct_ptr,
                           struct H5C_jbrb_sync_q_entry_t * entry_ptr)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C_jb_aio__sync_q__append, FAIL)

    HDassert( struct_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );
    HDassert( struct_ptr->use_aio );
    HDassert( entry_ptr != NULL );
    HDassert( entry_ptr->magic == H5C__H5C_JBRB_SYNC_Q_T_MAGIC );
    HDassert( entry_ptr->next == NULL );

    /* this should be an assert, but we need to include one call to
     * HGOTO_ERROR() to keep the compiler happy
     */
    if ( ! struct_ptr->use_aio_fsync ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "struct_ptr->use_aio_fsync FALSE on entry")
    }

    /* add the *entry_ptr to the sync queue */

    if ( struct_ptr->aio_sync_q_len == 0 ) {

        HDassert( struct_ptr->aio_sync_q_head == NULL );
        HDassert( struct_ptr->aio_sync_q_tail == NULL );

        struct_ptr->aio_sync_q_head = entry_ptr;
        struct_ptr->aio_sync_q_tail = entry_ptr;
        struct_ptr->aio_sync_q_len = 1;

    } else {

        HDassert( struct_ptr->aio_sync_q_head != NULL );
        HDassert( struct_ptr->aio_sync_q_tail != NULL );
        HDassert( struct_ptr->aio_sync_q_len > 0 );
        HDassert( ( ( struct_ptr->aio_sync_q_len == 1 ) &&
                    ( struct_ptr->aio_sync_q_head == 
                      struct_ptr->aio_sync_q_tail ) 
                  )
                  ||
                  ( ( struct_ptr->aio_sync_q_len > 1 ) &&
                    ( struct_ptr->aio_sync_q_head !=
                      struct_ptr->aio_sync_q_tail )
                  )
                );
        HDassert( struct_ptr->aio_sync_q_tail->magic == 
                  H5C__H5C_JBRB_SYNC_Q_T_MAGIC );
        HDassert( struct_ptr->aio_sync_q_tail->next == NULL );

        struct_ptr->aio_sync_q_tail->next = entry_ptr;
        struct_ptr->aio_sync_q_tail = entry_ptr;
        struct_ptr->aio_sync_q_len++;
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb_aio__sync_q__append() */


/******************************************************************************
 *
 * Function:		H5C_jb_aio__sync_q__append()
 *
 * Purpose:		Append an instance of H5C_jbrb_sync_q_entry_t to
 *			the sync queue.
 *
 *			Verify that AIO is enabled, and that the supplied
 *			instance of H5C_jbrb_sync_q_entry_t has the correct
 *			magic value.  Then append to the end of the sync 
 *			queue.
 *							JRM -- 2/9/10
 *
 * Returns:		SUCCEED if no errors are detected, 
 *			FAIL otherwise.
 *
 ******************************************************************************/

static herr_t 
H5C_jb_aio__sync_q__discard_head(H5C_jbrb_t * struct_ptr)
{
    herr_t ret_value = SUCCEED;
    struct H5C_jbrb_sync_q_entry_t * head_ptr = NULL;

    FUNC_ENTER_NOAPI(H5C_jb_aio__sync_q__discard_head, FAIL)

    HDassert( struct_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );
    HDassert( struct_ptr->use_aio );
    HDassert( struct_ptr->use_aio_fsync );

    /* this should be an assert, but we need to include one call to
     * HGOTO_ERROR() to keep the compiler happy
     */
    if ( struct_ptr->aio_sync_q_len <= 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "sync queue empty on entry?!?")
    }

    head_ptr = struct_ptr->aio_sync_q_head;
    HDassert( head_ptr != NULL );
    HDassert( head_ptr->magic == H5C__H5C_JBRB_SYNC_Q_T_MAGIC );

    /* unlink *head_ptr from the queue */

    /* add the *entry_ptr to the sync queue */

    if ( struct_ptr->aio_sync_q_len == 1 ) {

        HDassert( struct_ptr->aio_sync_q_head == head_ptr );
        HDassert( struct_ptr->aio_sync_q_tail == head_ptr );
        HDassert( head_ptr->next == NULL );

        struct_ptr->aio_sync_q_head = NULL;
        struct_ptr->aio_sync_q_tail = NULL;
        struct_ptr->aio_sync_q_len = 0;

    } else {

        HDassert( struct_ptr->aio_sync_q_len >= 2 );
        HDassert( struct_ptr->aio_sync_q_head == head_ptr );
        HDassert( struct_ptr->aio_sync_q_tail != NULL );
        HDassert( struct_ptr->aio_sync_q_tail != head_ptr );
        HDassert( head_ptr->next != NULL );
        HDassert( head_ptr->next->magic == H5C__H5C_JBRB_SYNC_Q_T_MAGIC );
        HDassert( struct_ptr->aio_sync_q_tail->next == NULL );

        struct_ptr->aio_sync_q_head = head_ptr->next;
        head_ptr->next = NULL;
        struct_ptr->aio_sync_q_len--;
    }

    /* and then discard it */

    head_ptr->magic = 0;
    head_ptr = (struct H5C_jbrb_sync_q_entry_t *)H5MM_xfree(head_ptr);

    if ( head_ptr != NULL ) {

        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "free of *head_ptr failed.");
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb_aio__sync_q__discard_head() */


/******************************************************************************
 *
 * Function:		H5C_jb_aio__test_buffer_write_complete()
 *
 * Programmer:		John Mainzer
 *
 * Purpose:		Test to see if an asynchronous write has completed.
 *
 *			Verify that AIO is enabled, that buffer specified
 *			exists, and that an asynchronous write of the buffer
 *			has been queued.
 *
 *			If it is, mark it complete, set *complete_ptr
 *			to TRUE, mark the associate AIO control block as
 *			having no write in progress, and return.
 *
 *			It it isn't, set *complete_ptr to FALSE and return
 *
 * Returns:		SUCCEED if no errors are detected, 
 *			FAIL otherwise.
 *
 ******************************************************************************/

static herr_t 
H5C_jb_aio__test_buffer_write_complete(H5C_jbrb_t * struct_ptr,
				        int buf_num,
                                        hbool_t *complete_ptr)
{
    int result;
    herr_t ret_value = SUCCEED;
    struct aiocb * aiocb_ptr = NULL;

    FUNC_ENTER_NOAPI(H5C_jb_aio__test_buffer_write_complete, FAIL)

    HDassert( struct_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );
    HDassert( struct_ptr->buf_size > 0 );
    HDassert( struct_ptr->journal_file_fd >= 0 );
    HDassert( struct_ptr->use_aio );
    HDassert( struct_ptr->aio_ctl_blks != NULL );
    HDassert( buf_num >= 0 );
    HDassert( buf_num < struct_ptr->num_bufs );
    HDassert( complete_ptr != NULL );

    aiocb_ptr = &((*(struct_ptr->aio_ctl_blks))[buf_num]);

    if ( aiocb_ptr->aio_fildes < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "AIO write not in progress for target buffer?")
    }

    result = aio_error(aiocb_ptr);

    if ( result == EINPROGRESS ) {

        /* the write is still in progress -- set *complete_ptr to 
         * FALSE and do nothing.
         */
        *complete_ptr = FALSE;

    } else if ( result == 0 ) {

        /* call aio_return() to complete the write */
        result = aio_return(aiocb_ptr);
        if ( result == -1 ) {

            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
		        "aio_error() reports something other than success.")

        }
        /* TODO: Verify the number of bytes written? */

        /* the write completed successfully -- set *complete_ptr 
         * to TRUE, mark the aio control block as having no write 
         * in progress.
         */
        
        aiocb_ptr->aio_fildes = -1;

        *complete_ptr = TRUE;

    } else {

        /* the write failed -- scream and die. */

        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "target async write failed.")

    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb_aio__test_buffer_write_complete() */


/******************************************************************************
 *
 * Function:		H5C_jb_aio__test_next_async_fsync_complete()
 *
 * Purpose:		Test to see if the asynchronous fsync at the head
 *			of the sync queue is complete.  
 *
 *			If it is, finish up the call to aio_fsync(), update 
 *			last_trans_on_disk, remove the associated instance of 
 *			H5C_jbrb_sync_q_entry_t from the sync queue, set
 *			*sync_complete_ptr to TRUE, and return.
 *
 *			If it isn't, set *sync_complete_ptr to FALSE, and 
 *			return.
 *
 *			In either case, verify that AIO is enabled, that 
 *			struct_ptr->use_aio_fsync is TRUE, and that the 
 *			sync queue is not empty.
 *							JRM -- 2/10/10
 *
 * Returns:		SUCCEED if no errors are detected, 
 *			FAIL otherwise.
 *
 ******************************************************************************/

#define H5C_JB_AIO__TEST_NEXT_ASYNC_FSYNC_COMPLETE__DEBUG 0

static herr_t 
H5C_jb_aio__test_next_async_fsync_complete(H5C_jbrb_t * struct_ptr,
                                            hbool_t *sync_complete_ptr)
{
    int result;
    herr_t ret_value = SUCCEED;
    struct H5C_jbrb_sync_q_entry_t * head_ptr = NULL;
    struct aiocb * aiocb_ptr = NULL;

    FUNC_ENTER_NOAPI(H5C_jb_aio__test_next_async_fsync_complete, FAIL)

    HDassert( struct_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );
    HDassert( struct_ptr->use_aio );
    HDassert( struct_ptr->use_aio_fsync );
    HDassert( struct_ptr->aio_sync_q_len > 0 );
    HDassert( sync_complete_ptr != NULL );

    head_ptr = struct_ptr->aio_sync_q_head;

    HDassert( head_ptr != NULL );
    HDassert( head_ptr->magic == H5C__H5C_JBRB_SYNC_Q_T_MAGIC );

    aiocb_ptr = &(head_ptr->ctl_blk);

    if ( aiocb_ptr->aio_fildes != struct_ptr->journal_file_fd ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "bad fd in ctl blk?!?")
    }

    result = aio_error(aiocb_ptr);

    if ( result == EINPROGRESS ) {

        /* the write is still in progress -- set *sync_complete_ptr to 
         * FALSE and do nothing.
         */
        *sync_complete_ptr = FALSE;

    } else if ( result == 0 ) {

        /* call aio_return() to complete the aio_fsync() */
        result = aio_return(aiocb_ptr);

        if ( result == -1 ) {

            HGOTO_ERROR(H5E_IO, H5E_SYNCFAIL, FAIL, \
		        "aio_error() reports something other than success.")

        }

        /* the aio_fsync() completed successfully -- update last trans on disk,
         * discard the head of the sync queue, and set *sync_complete_ptr to
         * TRUE.
         */

#if H5C_JB_AIO__TEST_NEXT_ASYNC_FSYNC_COMPLETE__DEBUG 
    HDfprintf(stdout, "%s: ltod/ltw/ltis = %lld/%lld/%lld.\n",
              FUNC, 
              (long long)(struct_ptr->last_trans_on_disk), 
              (long long)(struct_ptr->last_trans_written), 
              (long long)(head_ptr->last_trans_in_sync));
#endif /* H5C_JB_AIO__TEST_NEXT_ASYNC_FSYNC_COMPLETE__DEBUG */
        
        HDassert( (uint64_t)(struct_ptr->last_trans_on_disk) <= 
                      (uint64_t)(head_ptr->last_trans_in_sync) );
        HDassert( (uint64_t)(head_ptr->last_trans_in_sync) <= 
                      (uint64_t)(struct_ptr->last_trans_written) );

#if H5C_JB_AIO__TEST_NEXT_ASYNC_FSYNC_COMPLETE__DEBUG 
    HDfprintf(stdout, "%s: changing last trans on disk from %lld to %lld.\n",
              FUNC, struct_ptr->last_trans_on_disk, 
              head_ptr->last_trans_in_sync);
#endif /* H5C_JB_AIO__TEST_NEXT_ASYNC_FSYNC_COMPLETE__DEBUG */

        H5C__JBRB__UPDATE_STATS_FOR_ASYNC_SYNC_COMPLETED(struct_ptr, FALSE)

        aiocb_ptr->aio_fildes = -1;

        struct_ptr->last_trans_on_disk = head_ptr->last_trans_in_sync;

        if ( H5C_jb_aio__sync_q__discard_head(struct_ptr) != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                        "H5C_jb_aio__sync_q__discard_head() failed.")
        }

        *sync_complete_ptr = TRUE;

    } else {

        /* the sync failed -- scream and die. */

        HGOTO_ERROR(H5E_IO, H5E_SYNCFAIL, FAIL, "target async fsync failed.")

    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb_aio__test_next_async_fsync_complete() */


/******************************************************************************
 *
 * Function:		H5C_jb_aio__write_to_buffer
 *
 * Programmer:		John Mainzer
 *			0/09/10
 *
 * Purpose:		Copy the contents of the supplied data buffer into 
 *			the ring buffers, kicking off asynchronous writes 
 *			as the buffers fill, and stalling on completion of 
 *			writes as needed when the ring buffer fills.  
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

#define H5C_JB_AIO__WRITE_TO_BUFFER__DEBUG 0

herr_t 
H5C_jb_aio__write_to_buffer(H5C_jbrb_t * struct_ptr,	
			     size_t size,			
			     const char * data,
                             hbool_t is_end_trans,
                             uint64_t trans_num)
{
    hbool_t just_called_make_space_in_ring_buffer = FALSE;
    herr_t ret_value = SUCCEED;
    herr_t result;
    size_t size_remaining;
    const char * data_remaining;
    uint64_t last_trans_in_ring_buffer;
	
    FUNC_ENTER_NOAPI(H5C_jb_aio__write_to_buffer, FAIL)

    /* Check Arguments */
    HDassert( struct_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );
    HDassert( struct_ptr->use_aio == TRUE );
    HDassert( size > 0 );
    HDassert( data != 0 );
    HDassert( ( struct_ptr->human_readable == FALSE ) || 
              ( HDstrlen(data) == size ) );
    HDassert( ( ( struct_ptr->writes_in_progress == struct_ptr->bufs_in_use ) 
                &&
                ( struct_ptr->bufs_in_use < struct_ptr->num_bufs ) 
                &&
                ( struct_ptr->cur_buf_free_space == struct_ptr->buf_size )
              )
              ||
              ( ( struct_ptr->writes_in_progress + 1 == struct_ptr->bufs_in_use )
                &&
                ( struct_ptr->cur_buf_free_space < struct_ptr->buf_size )
                &&
                ( struct_ptr->cur_buf_free_space > 0 )
              )
              ||
              ( ( struct_ptr->writes_in_progress == struct_ptr->bufs_in_use )
                &&
                ( struct_ptr->bufs_in_use == struct_ptr->num_bufs ) 
                &&
                ( struct_ptr->cur_buf_free_space == 0 )
              )
            );

    /* make space in the ring buffer if necessary.  As 
     * struct_ptr->cur_buf_free_space will always be greater 
     * than zero if there is any space in the ring buffer,
     * it is sufficient to check that value and call 
     * H5C_jb_aio__make_space_in_ring_buffer() if it is zero.
     */
    if ( struct_ptr->cur_buf_free_space <= 0 ) {

        HDassert( struct_ptr->bufs_in_use == struct_ptr->num_bufs );
        HDassert( struct_ptr->writes_in_progress == struct_ptr->bufs_in_use );

        result = H5C_jb_aio__make_space_in_ring_buffer(struct_ptr);

        if ( result != SUCCEED ) {

#if H5C_JB_AIO__WRITE_TO_BUFFER__DEBUG
            HDfprintf(stdout, 
		      "%s: H5C_jb_aio__make_space_in_ring_buffer(1) failed.\n",
                      FUNC);
#endif /* H5C_JB_AIO__WRITE_TO_BUFFER__DEBUG */

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                        "H5C_jb_aio__make_space_in_ring_buffer(1) failed.")
        }

        just_called_make_space_in_ring_buffer = TRUE;

        HDassert( struct_ptr->writes_in_progress == struct_ptr->bufs_in_use );
    }

    data_remaining = data;
    size_remaining = size;

    while ( ( size_remaining >= struct_ptr->cur_buf_free_space ) &&
            ( struct_ptr->cur_buf_free_space > 0 ) ) {

        HDassert( struct_ptr->cur_buf_free_space > 0 );

        if ( struct_ptr->cur_buf_free_space == struct_ptr->buf_size ) {

            struct_ptr->bufs_in_use += 1;

            HDassert( struct_ptr->bufs_in_use <= struct_ptr->num_bufs );
        }

        HDassert( (struct_ptr->writes_in_progress + 1) == 
                  struct_ptr->bufs_in_use );

        /* fill the remainder of the current buffer with data */
        HDmemcpy(struct_ptr->head, 
                 (const void *)data_remaining, 
                 struct_ptr->cur_buf_free_space);

        data_remaining = data_remaining + struct_ptr->cur_buf_free_space;
        size_remaining = size_remaining - struct_ptr->cur_buf_free_space;

        struct_ptr->cur_buf_free_space = 0;

        if ( ( is_end_trans ) && ( size_remaining == 0 ) ) {

            (*struct_ptr->trans_tracking)[struct_ptr->put] = trans_num;

#if H5C_JB_AIO__WRITE_TO_BUFFER__DEBUG
	    HDfprintf(stdout, "%s: set trans_tracking[%d] to %lld (1).\n",
                      FUNC, struct_ptr->put, trans_num);
#endif /* H5C_JB_AIO__WRITE_TO_BUFFER__DEBUG */
        }

        HDassert( struct_ptr->bufs_in_use == 
                  (struct_ptr->writes_in_progress + 1 ) );
     
        /* kick off an asynchronous write of the current buffer */
	result = H5C_jb_aio__queue_buffer_write(struct_ptr,
				                 struct_ptr->put,
                                                 FALSE);

        if ( result != SUCCEED ) {

#if H5C_JB_AIO__WRITE_TO_BUFFER__DEBUG
            HDfprintf(stdout, 
		      "%s: H5C_jb_aio__queue_buffer_write() failed.\n",
                      FUNC);
#endif /* H5C_JB_AIO__WRITE_TO_BUFFER__DEBUG */

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
			"H5C_jb_aio__queue_buffer_write() failed.");
        }

        just_called_make_space_in_ring_buffer = FALSE;

        HDassert( struct_ptr->bufs_in_use == struct_ptr->writes_in_progress );

        /* if there is another free buffer, call 
         * H5C_jb_aio__prep_next_buf_for_use().
         *
         * otherwise, if we still have data to write, call 
         * H5C_jb_aio__make_space_in_ring_buffer() to free up 
         * space to continue the write.
         */
        if ( struct_ptr->bufs_in_use < struct_ptr->num_bufs ) {

            last_trans_in_ring_buffer = 
		(*struct_ptr->trans_tracking)[struct_ptr->put];

#if H5C_JB_AIO__WRITE_TO_BUFFER__DEBUG
	    HDfprintf(stdout, "%s: set trans_tracking[%d] to %lld (2).\n",
                      FUNC, struct_ptr->put, trans_num);
#endif /* H5C_JB_AIO__WRITE_TO_BUFFER__DEBUG */

            result = H5C_jb_aio__prep_next_buf_for_use(struct_ptr,
						last_trans_in_ring_buffer);

            if ( result != SUCCEED ) {

#if H5C_JB_AIO__WRITE_TO_BUFFER__DEBUG
            HDfprintf(stdout, 
		      "%s: H5C_jb_aio__prep_next_buf_for_use() failed.\n",
                      FUNC);
#endif /* H5C_JB_AIO__WRITE_TO_BUFFER__DEBUG */

                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                            "H5C_jb_aio__prep_next_buf_for_use() failed.")
            }
        } else if ( size_remaining > 0 ) { 

            result = H5C_jb_aio__make_space_in_ring_buffer(struct_ptr);

            if ( result != SUCCEED ) {

#if H5C_JB_AIO__WRITE_TO_BUFFER__DEBUG
                HDfprintf(stdout, 
		       "%s: H5C_jb_aio__make_space_in_ring_buffer() failed.\n",
                       FUNC);
#endif /* H5C_JB_AIO__WRITE_TO_BUFFER__DEBUG */

                HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                           "H5C_jb_aio__make_space_in_ring_buffer(2) failed.")
            }
            just_called_make_space_in_ring_buffer = TRUE;
        } 

        HDassert( ( struct_ptr->writes_in_progress == 
			struct_ptr->bufs_in_use ) ||
                  ( (struct_ptr->writes_in_progress + 1) ==
			struct_ptr->bufs_in_use ) );

    } /* while */

    HDassert( ( size_remaining < struct_ptr->cur_buf_free_space ) ||
              ( size_remaining == 0 ) );

    if ( size_remaining > 0 ) {

        /* increment bufs_in_use if we are about to write to an empty buffer */
        if ( struct_ptr->cur_buf_free_space == struct_ptr->buf_size ) {

            HDassert( struct_ptr->bufs_in_use == 
                      struct_ptr->writes_in_progress );

            struct_ptr->bufs_in_use += 1;

            HDassert( struct_ptr->bufs_in_use <= struct_ptr->num_bufs );
        }

        /* copy data into the current buffer */
        HDmemcpy(struct_ptr->head, 
                 (const void *)data_remaining, 
                 size_remaining);

        struct_ptr->head += size_remaining;
        struct_ptr->cur_buf_free_space -= size_remaining;

        if ( is_end_trans ) {

            (*struct_ptr->trans_tracking)[struct_ptr->put] = trans_num;

#if H5C_JB_AIO__WRITE_TO_BUFFER__DEBUG
	    HDfprintf(stdout, "%s: set trans_tracking[%d] to %lld (3).\n",
                      FUNC, struct_ptr->put, trans_num);
#endif /* H5C_JB_AIO__WRITE_TO_BUFFER__DEBUG */
        }
    }

    HDassert( ( ( struct_ptr->writes_in_progress == struct_ptr->bufs_in_use ) 
                &&
                ( struct_ptr->bufs_in_use < struct_ptr->num_bufs ) 
                &&
                ( struct_ptr->cur_buf_free_space == struct_ptr->buf_size )
              )
              ||
              ( ( struct_ptr->writes_in_progress + 1 == struct_ptr->bufs_in_use )
                &&
                ( struct_ptr->cur_buf_free_space < struct_ptr->buf_size )
                &&
                ( struct_ptr->cur_buf_free_space > 0 )
              )
              ||
              ( ( struct_ptr->writes_in_progress == struct_ptr->bufs_in_use )
                &&
                ( struct_ptr->bufs_in_use == struct_ptr->num_bufs ) 
                &&
                ( struct_ptr->cur_buf_free_space == 0 )
              )
            );

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb_aio__write_to_buffer */



/******************************************************************************
 *
 * Function:		H5C_jb_bjf__comment
 *
 * Programmer:		John Mainzer
 *
 * Purpose:		In the binary journal file format, a comment is 
 *			a no-op.  Thus in this function, we simply verify
 *			that we are in fact writing a binary journal file,
 *			and then return.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

static herr_t 
H5C_jb_bjf__comment(H5C_jbrb_t * struct_ptr,
                     const char * comment_ptr)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C_jb_bjf__comment, FAIL)
	
    /* Check Arguments */
    HDassert( struct_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );
    HDassert( comment_ptr != NULL );

    /* the following really should be an assert, but the FUNC ENTER/LEAVE
     * macros are happier if we have at least one call to HGOTO_ERROR().
     */

    if ( struct_ptr->human_readable ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "human_readable is TRUE?!?!\n")
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb_bjf__comment() */


/*****************************************************************************
 *
 * Function:		H5C_jb_bjf__end_transaction
 *
 * Programmer:		John Mainzer
 *
 * Purpose:		Verify that the supplied transaction is in progress,
 *			and that at least one journal entry has been written 
 *			under it. 
 *
 *                      Then write an end transaction message to the ring 
 *			buffer.
 *
 *			Make note that the supplied transaction is closed, 
 *			and that no transaction is in progress.
 *
 * Returns:		SUCCEED on success.
 *
 * Changes:		None.
 *
 *****************************************************************************/

static herr_t
H5C_jb_bjf__end_transaction(H5C_jbrb_t * struct_ptr,
			     uint64_t trans_num)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C_jb_bjf__end_transaction, FAIL)

    /* Check Arguments */
    HDassert( struct_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );
    HDassert( struct_ptr->human_readable == FALSE );
	
    /* Verify that the supplied transaction is in progress */
    if ( ( struct_ptr->trans_in_prog != TRUE ) ||
         ( struct_ptr->cur_trans != trans_num ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                   "Transaction not in progress or bad transaction number.")
    } /* end if */
	
    /* Verify that at least one journal entry has been written under 
     * the current transaction 
     */
    if ( struct_ptr->jentry_written != TRUE ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "Empty transaction -- at least one journal entry required.")
    } /* end if */

    /* Write end transaction message */
    H5C_JB_BJF__WRITE_SIG_AND_VER(struct_ptr,               \
                                   H5C_BJNL__END_TRANS_SIG, \
                                   H5C_BJNL__END_TRANS_VER, \
                                   /* keep_chksum */ FALSE,  \
                                   /* is_end_trans */ FALSE, \
                                   trans_num,                \
                                   /* fail_return */ FAIL)

    H5C_JB_BJF__WRITE_TRANS_NUM(struct_ptr,                 \
                                 /* is_end_trans */ TRUE,    \
                                 trans_num,                  \
                                 /* fail_return */ FAIL)

    /* reset boolean flag indicating that at least one journal entry has 
     * been written under transaction 
     */
    struct_ptr->jentry_written = FALSE;

    /* Close current transaction */
    struct_ptr->trans_in_prog = FALSE;

done:
	
    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C_jb_bjf__end_transaction */


/******************************************************************************
 *
 * Function:		H5C_jb_bjf__eoa
 *
 * Programmer:		John Mainzer
 *
 * Purpose:		Write an end of address space message with the 
 *			supplied EOA in binary format to the journal file.
 *
 *			Note that EOA messages are not generated by the 
 *			metadata cache, and thus are not associated with 
 *			transactions.  Since H5C_jb__write_to_buffer()
 *			expects a transaction number, we use 
 *			struct_ptr->cur_trans and pass is_end_trans
 *			as FALSE.  However, this is just a cluge to 
 *			keep pre-existing code happy.
 *
 * Returns:		SUCCEED on success.
 *
 * Changes:		None.
 *
 ******************************************************************************/

static herr_t 
H5C_jb_bjf__eoa(H5C_jbrb_t * struct_ptr,
		 haddr_t eoa)
{
    herr_t ret_value = SUCCEED;
    FUNC_ENTER_NOAPI(H5C_jb_bjf__eoa, FAIL)
	
    /* Check Arguments */
    HDassert( struct_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );
    HDassert( struct_ptr->human_readable == FALSE );
    HDassert( struct_ptr->hdf5_file_name != NULL );

    /* Verify that header message is present in journal file or ring buffer. 
     * If not, write it. 
     */
    if ( struct_ptr->header_present == FALSE ) {

        if ( H5C_jb__write_header_entry(struct_ptr) != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C_jb__write_header_entry() failed.\n")
        }
    } /* end if */

    /* Note that EOA messages are not generated by the metadata cache, and 
     * thus are not associated with transactions.  Since 
     * H5C_jb__write_to_buffer() expects a transaction number, we use 
     * struct_ptr->cur_trans and pass is_end_trans as FALSE.  However, 
     * this is just a cluge to keep pre-existing code happy.
     */

    /* Write EOA message */
    H5C_JB_BJF__WRITE_SIG_AND_VER(struct_ptr,                    \
                                   H5C_BJNL__END_ADDR_SPACE_SIG, \
                                   H5C_BJNL__END_ADDR_SPACE_VER, \
                                   /* keep_chksum */ FALSE,       \
                                   /* is_end_trans */ FALSE,      \
                                   struct_ptr->cur_trans,         \
                                   /* fail_return */ FAIL)

    H5C_JB_BJF__WRITE_OFFSET(struct_ptr,               \
                              eoa,                      \
                              /* is_end_trans */ FALSE, \
                              struct_ptr->cur_trans,    \
                              /* fail_return */ FAIL)

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C_jb_bjf__eoa */


/******************************************************************************
 *
 * Function:		H5C_jb_bjf__journal_entry
 *
 * Programmer:		John Mainzer
 *
 * Purpose:		Verify that the specified transaction is open. Then
 *			write a binary journal file message to the ring buffer.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

static herr_t 
H5C_jb_bjf__journal_entry(H5C_jbrb_t * struct_ptr,
			   uint64_t trans_num,
			   haddr_t base_addr,
			   size_t length,
			   const uint8_t * body)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C_jb_bjf__journal_entry, FAIL)

    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC);
    HDassert(struct_ptr->human_readable == FALSE );

    /* Verify that the supplied transaction is in progress */
    if ( ( struct_ptr->trans_in_prog != TRUE ) ||
         ( struct_ptr->cur_trans != trans_num ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "Transaction not in progress or bad transaction number.")
    } /* end if */

    H5C_JB_BJF__WRITE_SIG_AND_VER(struct_ptr,                   \
                                   H5C_BJNL__JOURNAL_ENTRY_SIG, \
                                   H5C_BJNL__JOURNAL_ENTRY_VER, \
                                   /* keep_chksum */ TRUE,       \
                                   /* is_end_trans */ FALSE,     \
                                   trans_num,                    \
                                   /* fail_return */ FAIL)

    H5C_JB_BJF__WRITE_TRANS_NUM(struct_ptr,                     \
                                 /* is_end_trans */ FALSE,       \
                                 trans_num,                      \
                                 /* fail_return */ FAIL)

    H5C_JB_BJF__WRITE_OFFSET(struct_ptr,                        \
                              base_addr,                         \
                              /* is_end_trans */ FALSE,          \
                              trans_num,                         \
                              /* fail_return */ FAIL)

    H5C_JB_BJF__WRITE_LENGTH(struct_ptr,                        \
                              length,                            \
                              /* is_end_trans */ FALSE,          \
                              trans_num,                         \
                              /* fail_return */ FAIL)

    H5C_JB_BJF__WRITE_BUFFER(struct_ptr,                        \
                              /* buf_size */ length,             \
                              /* buf_ptr */ (const char *)body,  \
                              /* is_end_trans */ FALSE,          \
                              trans_num,                         \
                              /* fail_return */ FAIL)

    H5C_jb_BJF__WRITE_CHKSUM(struct_ptr,                        \
                              /* is_end_trans */ FALSE,          \
                              trans_num,                         \
                              /* fail_return */ FAIL)

    /* Indicate that at least one journal entry has been written under 
     * this transaction 
     */
    struct_ptr->jentry_written = TRUE;


done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb_bjf__journal_entry() */


/******************************************************************************
 *
 * Function:		H5C_jb_bjf__start_transaction
 *
 * Programmer:		John Mainzer
 *
 * Purpose:		Verify that there is no transaction in progress, and
 *			that the supplied transaction number greater than 
 *			the last.  Then write a binary start transaction 
 *			message to the ring buffer.  Make note of the fact 
 *			that the supplied transaction is in progress.
 *
 * Returns:		SUCCEED on success.
 *
 * Changes:		None
 *
 ******************************************************************************/

static herr_t 
H5C_jb_bjf__start_transaction(H5C_jbrb_t * struct_ptr,
			       uint64_t trans_num)

{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C_jb_bjf__start_transaction, FAIL)

    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC);
    HDassert(struct_ptr->human_readable == FALSE );
	
    /* Verify that there is no transaction in progress */
    if ( struct_ptr->trans_in_prog != FALSE ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "Transaction already in progress.")
    } /* end if */

    /* Verify that the supplied transaction number greater than the last */
    if ( (struct_ptr->cur_trans) >= trans_num ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "New transaction out of sequence.")
    } /* end if */

    /* Verify that header message is present in journal file or ring buffer. 
     * If not, write it. 
     */
    if ( struct_ptr->header_present == FALSE ) {

        if ( H5C_jb__write_header_entry(struct_ptr) != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C_jb__write_header_entry() failed.\n")
        }

    } /* end if */


    /* Write start transaction message */
    H5C_JB_BJF__WRITE_SIG_AND_VER(struct_ptr,                 \
                                   H5C_BJNL__BEGIN_TRANS_SIG, \
                                   H5C_BJNL__BEGIN_TRANS_VER, \
                                   /* keep_chksum */ FALSE,    \
                                   /* is_end_trans */ FALSE,   \
                                   trans_num,                  \
                                   /* fail_return */ FAIL)

    H5C_JB_BJF__WRITE_TRANS_NUM(struct_ptr,                   \
                                 /* is_end_trans */ FALSE,     \
                                 trans_num,                    \
                                 /* fail_return */ FAIL)
		
    /* Make note of the fact that supplied transaction is in progress */
    struct_ptr->trans_in_prog = TRUE;
    struct_ptr->cur_trans = trans_num;

done:
	
    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C_jb_bjf__start_transaction */


/******************************************************************************
 *
 * Function:		H5C_jb_bjf__write_buf
 *
 * Programmer:		John Mainzer
 *			4/24/09
 *
 * Purpose:		Copy the supplied buffer to the ring buffer as 
 *			efficiently as possible.
 *
 *			If there is space available in the current buffer in 
 *			the ring buffer is big enough, just memcpy the 
 *                      supplied buffer directly into the ring buffer buffer
 *                      and update its fields accordingly.  
 *
 *			If the supplied buffer will cross ring buffer buffer 
 *			boundaries, for now just call 
 *			H5C_jb__write_to_buffer().
 *
 *			In either case, if struct_ptr->chksum_cur_msg is TRUE,
 *			update struct_ptr->msg_chksum.
 *
 *			Note that this function will probably prove to be
 *			a hot spot in profiling, so we should more or less
 *			plan on converting it into a macro at some point.
 *
 * Returns:		SUCCEED on success.
 *			FAIL on failure.
 *
 * Changes:		Updated to updated fields used only in the SIO case
 *			only when SIO is selected.
 *							JRM -- 1/14/10
 *
 ******************************************************************************/

static herr_t 
H5C_jb_bjf__write_buffer(H5C_jbrb_t * struct_ptr,
                          size_t buf_size,
                          const char * buf_ptr,
                          hbool_t is_end_trans,
                          uint64_t trans_num)
{
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5C_jb_bjf__write_buffer, FAIL)

    HDassert( struct_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );
    HDassert( buf_size > 0 );
    HDassert( buf_ptr != NULL );
    HDassert( trans_num > 0 ); 
    HDassert( ( ! struct_ptr->use_aio )
              ||
              ( ( struct_ptr->writes_in_progress == struct_ptr->bufs_in_use ) 
                &&
                ( struct_ptr->bufs_in_use < struct_ptr->num_bufs ) 
                &&
                ( struct_ptr->cur_buf_free_space == struct_ptr->buf_size )
              )
              ||
              ( ( struct_ptr->writes_in_progress + 1 == struct_ptr->bufs_in_use )
                &&
                ( struct_ptr->cur_buf_free_space < struct_ptr->buf_size )
                &&
                ( struct_ptr->cur_buf_free_space > 0 )
              )
              ||
              ( ( struct_ptr->writes_in_progress == struct_ptr->bufs_in_use )
                &&
                ( struct_ptr->bufs_in_use == struct_ptr->num_bufs ) 
                &&
                ( struct_ptr->cur_buf_free_space == 0 )
              )
            );

    /* is_end_trans must be FALSE if struct_ptr->chksum_cur_msg is TRUE.
     * Throw an error if this invarient doesn't hold.
     */

    if ( ( is_end_trans ) && ( struct_ptr->chksum_cur_msg ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "is_end_trans and struct_ptr->chksum_cur_msg both true.")
    }

    /* Update the check sum if required */
    if ( struct_ptr->chksum_cur_msg ) {

        struct_ptr->msg_chksum = H5_checksum_metadata((const void *)(buf_ptr), 
                                                      buf_size, 
                                                      struct_ptr->msg_chksum);
    }

    if ( buf_size < struct_ptr->cur_buf_free_space ) {

        /* If the buffer will fit in the current ring buffer buffer with space
         * left over, just memcpy() it in and touch up the ring buffer
         * fields accordingly.  
         *
         * This is the common case, so when we convert this function into 
         * a macro, this will allow us to avoid a function call in the vast 
         * majority of cases.
         */

	/* write data into journal buffer */
        HDmemcpy(struct_ptr->head, (const void *)buf_ptr, buf_size);

        /* increment bufs_in_use as necessary -- do this differently 
         * for aio and sio.
         */
        if ( ( ( struct_ptr->bufs_in_use == 0 ) 
               &&
               ( ! struct_ptr->use_aio ) 
             )
             ||
             ( ( struct_ptr->cur_buf_free_space == struct_ptr->buf_size ) 
               &&
               ( struct_ptr->use_aio )
             )
           ) {

            struct_ptr->bufs_in_use++;
        }

        /* update head pointer */
        struct_ptr->head = &(struct_ptr->head[buf_size]);

        /* update current buffer usage */
        struct_ptr->cur_buf_free_space -= buf_size;

        if ( ! struct_ptr->use_aio ) {

            /* update fields used only with SIO: */

            /* update rb_free_space */
            struct_ptr->rb_free_space -= buf_size;

            /* update end of buffer space */
            struct_ptr->rb_space_to_rollover -= buf_size;
        }

        if ( is_end_trans == TRUE ) {

            (*struct_ptr->trans_tracking)[struct_ptr->put] = trans_num;
        } 

        HDassert( struct_ptr->cur_buf_free_space > 0 );

    } else {

        /* Here, handle the case where the write will reach the edge
         * of a ring buffer buffer.  This gets a bit more complex, so 
         * for now at least, we will call H5C_jb__write_to_buffer().  
         * If this proves too costly, further optimizations will be necessary.
         */

        if ( H5C_jb__write_to_buffer(struct_ptr, buf_size, buf_ptr,
                                      is_end_trans, trans_num) != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C_jb__write_to_buffer() failed.")
        }
    }

    HDassert( ( ! struct_ptr->use_aio )
              ||
              ( ( struct_ptr->writes_in_progress == struct_ptr->bufs_in_use ) 
                &&
                ( struct_ptr->bufs_in_use < struct_ptr->num_bufs ) 
                &&
                ( struct_ptr->cur_buf_free_space == struct_ptr->buf_size )
              )
              ||
              ( ( struct_ptr->writes_in_progress + 1 == struct_ptr->bufs_in_use )
                &&
                ( struct_ptr->cur_buf_free_space < struct_ptr->buf_size )
                &&
                ( struct_ptr->cur_buf_free_space > 0 )
              )
              ||
              ( ( struct_ptr->writes_in_progress == struct_ptr->bufs_in_use )
                &&
                ( struct_ptr->bufs_in_use == struct_ptr->num_bufs ) 
                &&
                ( struct_ptr->cur_buf_free_space == 0 )
              )
            );
done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb_bjf__write_buffer() */
 

/******************************************************************************
 *
 * Function:		H5C_jb_bjf__write_chksum
 *
 * Programmer:		John Mainzer
 *			4/24/09
 *
 * Purpose:		Write the checksum of a binary journal file message
 *			to the ring buffer as eficiently as possible.  Note
 *			that this checksum is computed only on the body of 
 *			the message -- not the signature and version.
 *
 *			If there is space available in the current buffer in 
 *			the ring buffer is big enough, just write the chksum
 *                      directly into the ring buffer buffer and update its 
 *			fields accordingly.  
 *
 *			If the chksum will cross ring buffer buffer boundaries, 
 *			for now just call H5C_jb__write_to_buffer().
 *
 *			Note that this function will probably prove to be
 *			a hot spot in profiling, so we should more or less
 *			plan on converting it into a macro at some point.
 *
 * Returns:		SUCCEED on success.
 *			FAIL on failure.
 *
 ******************************************************************************/

static herr_t 
H5C_jb_bjf__write_chksum(H5C_jbrb_t * struct_ptr, 
                          hbool_t is_end_trans,
                          uint64_t trans_num)
{
    uint8_t *p;
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5C_jb_bjf__write_chksum, FAIL)

    /* Sanity check */
    HDassert(struct_ptr != NULL);
    HDassert(struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC);
    HDassert(trans_num > 0); 

    if ( ! struct_ptr->chksum_cur_msg ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "struct_ptr->chksum_cur_msg is false?!?!.")
    }

    if ( H5C__CHECKSUM_SIZE < struct_ptr->cur_buf_free_space ) {

        /* If the checksum will fit in the current buffer with space
         * left over, just write it directly into the buffer, and 
         * touch up the ring buffer fields accordingly.  
         *
         * This is the common case, so when we convert this function into 
         * a macro, this will allow us to avoid a function call in the vast 
         * majority of cases.
         */

	/* write data into journal buffer */

        p = (uint8_t *)(struct_ptr->head);

        UINT32ENCODE(p, struct_ptr->msg_chksum);

        HDassert( p == ((uint8_t *)(struct_ptr->head + H5C__CHECKSUM_SIZE)) );

        /* increment bufs_in_use as necessary -- do this differently
         * for aio and sio.
         */
        if ( ( ( struct_ptr->bufs_in_use == 0 )
               &&
               ( ! struct_ptr->use_aio )
             )
             ||
             ( ( struct_ptr->cur_buf_free_space == struct_ptr->buf_size )
               &&
               ( struct_ptr->use_aio )
             )
           ) {

            struct_ptr->bufs_in_use++;
        }

        /* update head pointer */
        struct_ptr->head = &(struct_ptr->head[H5C__CHECKSUM_SIZE]);

        /* update current buffer usage */
        struct_ptr->cur_buf_free_space -= H5C__CHECKSUM_SIZE;

        /* update fields used only with SIO: */
        if( ! struct_ptr->use_aio ) {

            /* update rb_free_space */
            struct_ptr->rb_free_space -= H5C__CHECKSUM_SIZE;

            /* update end of buffer space */
            struct_ptr->rb_space_to_rollover -= H5C__CHECKSUM_SIZE;
        }

        if(is_end_trans)
            (*struct_ptr->trans_tracking)[struct_ptr->put] = trans_num;

        HDassert( struct_ptr->cur_buf_free_space > 0 );

    } /* end if */
    else {

        uint8_t buf[H5C__CHECKSUM_SIZE + 1];

        /* Here, handle the case where the write will reach the edge
         * of a buffer.  This gets a bit more complex, so for now at 
         * least, we will construct a buffer containing a binary 
         * representation of the checksum, and then call 
         * H5C_jb__write_to_buffer().  If this proves too costly, 
         * further optimizations will be necessary.
         */
        p = buf;
        UINT32ENCODE(p, struct_ptr->msg_chksum);
        HDassert( p == &(buf[H5C__CHECKSUM_SIZE]) );

        if ( H5C_jb__write_to_buffer(struct_ptr, 
                                      H5C__CHECKSUM_SIZE, 
                                      (const char *)buf, 
                                      is_end_trans, 
                                      trans_num) < 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C_jb__write_to_buffer() failed.")
        }
    } /* end else */

    /* re-set the checksum computation fields */
    struct_ptr->chksum_cur_msg = FALSE;
    struct_ptr->msg_chksum = 0;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb_bjf__write_chksum() */
 

/******************************************************************************
 *
 * Function:		H5C_jb_bjf__write_length
 *
 * Programmer:		John Mainzer
 *			4/24/09
 *
 * Purpose:		Write a HDF5 file length to the ring buffer as 
 *			efficiently as possible.
 *
 *			If the space available in the current buffer in 
 *			the ring buffer is big enough, just encode the 
 *                      lenght directly into the buffer and update its 
 *			fields accordingly.  
 *
 *			If the binary representation of the length will 
 *			touch buffer boundaries, create a buffer containing
 *			the binary representation of the length, and then
 *			call H5C_jb__write_to_buffer() to handle the write.
 *
 *			In either case, if struct_ptr->chksum_cur_msg is TRUE,
 *			update struct_ptr->msg_chksum.
 *
 *			Note that this function will probably prove to be
 *			a hot spot in profiling, so we should more or less
 *			plan on converting it into a macro at some point.
 *
 * Returns:		SUCCEED on success.
 *			FAIL on failure.
 *
 * Changes:		None.
 *
 ******************************************************************************/

static herr_t 
H5C_jb_bjf__write_length(H5C_jbrb_t * struct_ptr,
                          size_t length,
                          hbool_t is_end_trans,
                          uint64_t trans_num)
{
    herr_t ret_value = SUCCEED;      /* Return value */
    uint8_t * p;
    size_t length_width;

    FUNC_ENTER_NOAPI(H5C_jb_bjf__write_length, FAIL)

    HDassert( struct_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );
    HDassert( trans_num > 0 ); 

    /* is_end_trans must be FALSE if struct_ptr->chksum_cur_msg is TRUE.
     * Throw an error if this invarient doesn't hold.
     */

    if ( ( is_end_trans ) && ( struct_ptr->chksum_cur_msg ) ) {

       HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                   "is_end_trans and struct_ptr->chksum_cur_msg both true.")
    }

    length_width = (size_t)(struct_ptr->length_width);

    HDassert( ( length_width == 2 ) || 
              ( length_width == 4 ) || 
              ( length_width == 8 ) );

    if ( length_width < struct_ptr->cur_buf_free_space ) {

        /* If the offset will fit in the current buffer with space
         * left over, just write it directly into the buffer, and 
         * touch up the ring buffer fields accordingly.  
         *
         * This is the common case, so when we convert this function into 
         * a macro, this will allow us to avoid a function call in the vast 
         * majority of cases.
         */

	/* write data into journal buffer */

        p = (uint8_t *)(struct_ptr->head);

        switch ( length_width )
        {
	    case 2:
                UINT16ENCODE(p, length);
                break;

	    case 4:
                UINT32ENCODE(p, length);
                break;

	    case 8:
                UINT64ENCODE(p, length);
                break;

            default:
               HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                           "length_width out of range (1).")
               break;
        }

        HDassert( p == ((uint8_t *)(struct_ptr->head + length_width)) );

        /* increment bufs_in_use as necessary -- do this differently
         * for aio and sio.
         */
        if ( ( ( struct_ptr->bufs_in_use == 0 )
               &&
               ( ! struct_ptr->use_aio )
             )
             ||
             ( ( struct_ptr->cur_buf_free_space == struct_ptr->buf_size )
               &&
               ( struct_ptr->use_aio )
             )
           ) {

            struct_ptr->bufs_in_use++;
        }

        /* Update the check sum if required */
        if ( struct_ptr->chksum_cur_msg ) {

            struct_ptr->msg_chksum = 
		H5_checksum_metadata((const void *)(struct_ptr->head), 
                                     length_width, 
                                     struct_ptr->msg_chksum);
        }

        /* update head pointer */
        struct_ptr->head = &(struct_ptr->head[length_width]);

        /* update current buffer usage */
        struct_ptr->cur_buf_free_space -= length_width;

        /* update fields used only with SIO: */
        if( ! struct_ptr->use_aio ) {

            /* update rb_free_space */
            struct_ptr->rb_free_space -= length_width;

            /* update end of buffer space */
            struct_ptr->rb_space_to_rollover -= length_width;
        }

        if ( is_end_trans == TRUE ) {

            (*struct_ptr->trans_tracking)[struct_ptr->put] = trans_num;
        } 

        HDassert( struct_ptr->cur_buf_free_space > 0 );

    } else {

        /* Here, handle the case where the write will reach the edge
         * of a buffer.  This gets a bit more complex, so for now at 
         * least, we will construct a buffer containing a binary 
         * representation of the offset, and then call 
         * H5C_jb__write_to_buffer().  If this proves too costly, 
         * further optimizations will be necessary.
         */

        uint8_t buf[17]; /* should be big enough for a long time. */

        HDassert(length_width < sizeof(buf));
        p = buf;
        switch(length_width) {
	    case 2:
                UINT16ENCODE(p, length);
                break;

	    case 4:
                UINT32ENCODE(p, length);
                break;

	    case 8:
                UINT64ENCODE(p, length);
                break;

            default:
               HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                           "length_width out of range (2).")
               break;
        } /* end switch */

        HDassert( p == &(buf[length_width]) );

        if ( H5C_jb__write_to_buffer(struct_ptr, length_width, 
                                      (const char *)buf,
                                      is_end_trans, 
                                      trans_num) != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C_jb__write_to_buffer() failed.")
        }

        /* Update the check sum if required */
        if ( struct_ptr->chksum_cur_msg ) {

            struct_ptr->msg_chksum = 
			H5_checksum_metadata((const void *)(buf), 
                                             length_width, 
                                             struct_ptr->msg_chksum);
        }
    } /* end else */

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb_bjf__write_length() */
 

/******************************************************************************
 *
 * Function:		H5C_jb_bjf__write_offset
 *
 * Programmer:		John Mainzer
 *			4/24/09
 *
 * Purpose:		Write a HDF5 file offset to the ring buffer as 
 *			efficiently as possible.
 *
 *			If the space available in the current buffer in 
 *			the ring buffer is big enough, just encode the 
 *                      offset directly into the buffer and update its 
 *			fields accordingly.  
 *
 *			If the binary representation of the offset will 
 *			touch buffer boundaries, create a buffer containing
 *			the binary representation of the offset, and then
 *			call H5C_jb__write_to_buffer() to handle the write.
 *
 *			In either case, if struct_ptr->chksum_cur_msg is TRUE,
 *			update struct_ptr->msg_chksum.
 *
 *			Note that this function will probably prove to be
 *			a hot spot in profiling, so we should more or less
 *			plan on converting it into a macro at some point.
 *
 * Returns:		SUCCEED on success.
 *			FAIL on failure.
 *
 * Changes:		None.
 *
 ******************************************************************************/

static herr_t 
H5C_jb_bjf__write_offset(H5C_jbrb_t * struct_ptr,
                          haddr_t offset,
                          hbool_t is_end_trans,
                          uint64_t trans_num)
{
    herr_t ret_value = SUCCEED;      /* Return value */
    uint8_t * p;
    size_t offset_width;

    FUNC_ENTER_NOAPI(H5C_jb_bjf__write_offset, FAIL)

    HDassert( struct_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );
    /* eoa messages can be written outside transactions -- so it is
     * possible that the trans_num will be 0.  Since the trans_num is 
     * not used unless is_end_trans is TRUE, we make an exception for
     * the eoa message.
     */
    HDassert( ( ! is_end_trans ) || ( trans_num > 0 ) ); 

    /* is_end_trans must be FALSE if struct_ptr->chksum_cur_msg is TRUE.
     * Throw an error if this invarient doesn't hold.
     */

    if ( ( is_end_trans ) && ( struct_ptr->chksum_cur_msg ) ) {

       HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                   "is_end_trans and struct_ptr->chksum_cur_msg both true.")
    }

    offset_width = (size_t)(struct_ptr->offset_width);

    HDassert( ( offset_width == 2 ) || 
              ( offset_width == 4 ) || 
              ( offset_width == 8 ) );

    if ( offset_width < struct_ptr->cur_buf_free_space ) {

        /* If the offset will fit in the current buffer with space
         * left over, just write it directly into the buffer, and 
         * touch up the ring buffer fields accordingly.  
         *
         * This is the common case, so when we convert this function into 
         * a macro, this will allow us to avoid a function call in the vast 
         * majority of cases.
         */

	/* write data into journal buffer */

        p = (uint8_t *)(struct_ptr->head);

        switch ( offset_width )
        {
	    case 2:
                UINT16ENCODE(p, offset);
                break;

	    case 4:
                UINT32ENCODE(p, offset);
                break;

	    case 8:
                UINT64ENCODE(p, offset);
                break;

            default:
               HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                           "offset_width out of range (1).")
               break;
        }

        HDassert( p == ((uint8_t *)(struct_ptr->head + offset_width)) );

        /* increment bufs_in_use as necessary -- do this differently
         * for aio and sio.
         */
        if ( ( ( struct_ptr->bufs_in_use == 0 )
               &&
               ( ! struct_ptr->use_aio )
             )
             ||
             ( ( struct_ptr->cur_buf_free_space == struct_ptr->buf_size )
               &&
               ( struct_ptr->use_aio )
             )
           ) {

            struct_ptr->bufs_in_use++;
        }

        /* Update the check sum if required */
        if ( struct_ptr->chksum_cur_msg ) {

            struct_ptr->msg_chksum = 
		H5_checksum_metadata((const void *)(struct_ptr->head), 
                                     offset_width, 
                                     struct_ptr->msg_chksum);
        }

        /* update head pointer */
        struct_ptr->head = &(struct_ptr->head[offset_width]);

        /* update current buffer usage */
        struct_ptr->cur_buf_free_space -= offset_width;

        /* update fields used only with SIO: */
        if( ! struct_ptr->use_aio ) {

            /* update rb_free_space */
            struct_ptr->rb_free_space -= offset_width;

            /* update end of buffer space */
            struct_ptr->rb_space_to_rollover -= offset_width;
        }

        if ( is_end_trans == TRUE ) {

            (*struct_ptr->trans_tracking)[struct_ptr->put] = trans_num;
        } 

        HDassert( struct_ptr->cur_buf_free_space > 0 );

    } else {

        /* Here, handle the case where the write will reach the edge
         * of a buffer.  This gets a bit more complex, so for now at 
         * least, we will construct a buffer containing a binary 
         * representation of the offset, and then call 
         * H5C_jb__write_to_buffer().  If this proves too costly, 
         * further optimizations will be necessary.
         */

        uint8_t buf[17]; /* should be big enough for a long time. */

        HDassert( offset_width < 17 ) ;

        p = buf;

        switch ( offset_width )
        {
	    case 2:
                UINT16ENCODE(p, offset);
                break;

	    case 4:
                UINT32ENCODE(p, offset);
                break;

	    case 8:
                UINT64ENCODE(p, offset);
                break;

            default:
               HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                           "offset_width out of range (2).")
               break;
        }

        HDassert( p == &(buf[offset_width]) );

        if ( H5C_jb__write_to_buffer(struct_ptr, offset_width, 
                                      (const char *)buf,
                                      is_end_trans, trans_num) != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C_jb__write_to_buffer() failed.")
        }

        /* Update the check sum if required */
        if ( struct_ptr->chksum_cur_msg ) {

            struct_ptr->msg_chksum = H5_checksum_metadata((const void *)(buf), 
                                                        offset_width, 
                                                        struct_ptr->msg_chksum);
        }

    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb_bjf__write_offset() */
 

/******************************************************************************
 *
 * Function:		H5C_jb_bjf__write_sig_and_ver
 *
 * Programmer:		John Mainzer
 *			4/24/09
 *
 * Purpose:		Write the signature and version of a binary journal 
 *			file message to the ring buffer as efficiently as 
 *			possible.
 *
 *			If there is space available in the current buffer in 
 *			the ring buffer is big enough, just memcpy the 
 *                      signature and write the version directly into the 
 *			buffer and update its fields accordingly.  
 *
 *			If the signature and version will cross buffer 
 *			boundaries, for now just call 
 *			H5C_jb__write_to_buffer().
 *
 *			In either case, if keep_chksum is TRUE, initialize
 *			struct_ptr->msg_chksum to 0, and set struct_ptr->
 *			chksum_cur_msg to TRUE.
 *
 *			Observe that the checksum does not include the 
 *			signature and version.
 *
 *			Note that this function will probably prove to be
 *			a hot spot in profiling, so we should more or less
 *			plan on converting it into a macro at some point.
 *
 * Returns:		SUCCEED on success.
 *			FAIL on failure.
 *
 * Changes:		Updated function for slight differences in buffer
 *			management when aio is enabled.
 *							JRM -- 1/27/09
 *
 ******************************************************************************/

static herr_t 
H5C_jb_bjf__write_sig_and_ver(H5C_jbrb_t *struct_ptr, 
                               const char *sig_ptr,
                               const uint8_t version, 
                               hbool_t keep_chksum, 
                               hbool_t is_end_trans,
                               uint64_t trans_num)
{
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5C_jb_bjf__write_sig_and_ver, FAIL)

    HDassert( struct_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );
    HDassert( sig_ptr != NULL );
    HDassert( H5C_BJNL__SIG_LEN == HDstrlen(sig_ptr) );
    HDassert( ! is_end_trans );

    /* eoa messages can occur outside of transactions -- and thus it is 
     * possible that we will have to process one before any transaction 
     * has started -- in which case trans_num will be 0.  Since the trans_num
     * isn't used unless is_end_trans is TRUE, we carve a small exception 
     * for the eoa message.
     */
    HDassert((!is_end_trans) || (trans_num > 0));

    if ( (H5C_BJNL__SIG_LEN + 1) < struct_ptr->cur_buf_free_space ) {

        /* If the signature and version will fit in the current buffer 
         * with space left over, just memcpy()/write it in and touch up 
         * the ring bufferfields accordingly.  
         *
         * This is the common case, so when we convert this function into 
         * a macro, this will allow us to avoid a function call in the vast 
         * majority of cases.
         */

	/* write the signature into journal buffer */
        HDmemcpy(struct_ptr->head, (const void *)sig_ptr, H5C_BJNL__SIG_LEN);

        struct_ptr->head[H5C_BJNL__SIG_LEN] = (char)version;

        /* update head pointer */
        struct_ptr->head = &(struct_ptr->head[H5C_BJNL__SIG_LEN + 1]);

        /* increment bufs_in_use as necessary -- do this differently
         * for aio and sio.
         */
        if ( ( ( struct_ptr->bufs_in_use == 0 )
               &&
               ( ! struct_ptr->use_aio )
             )
             ||
             ( ( struct_ptr->cur_buf_free_space == struct_ptr->buf_size )
               &&
               ( struct_ptr->use_aio )
             )
           ) {

            struct_ptr->bufs_in_use++;
        }

        /* update current buffer usage */
        struct_ptr->cur_buf_free_space -= H5C_BJNL__SIG_LEN + 1;

        /* update fields used only with SIO: */
        if( ! struct_ptr->use_aio ) {

            /* update rb_free_space */
            struct_ptr->rb_free_space -= H5C_BJNL__SIG_LEN + 1;

            /* update end of buffer space */
            struct_ptr->rb_space_to_rollover -= H5C_BJNL__SIG_LEN + 1;
        }

        /* is_end_trans must be false in this call, so just throw an 
         * error if it is TRUE.
         */
        if ( is_end_trans ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "is_end_trans TRUE when writing signiture.")
        }

        HDassert( struct_ptr->cur_buf_free_space > 0 );

    } /* end if */
    else {

        uint8_t buf[H5C_BJNL__SIG_LEN + 2];

        /* Here, handle the case where the write will reach the edge
         * of a buffer.  This gets a bit more complex, so for now at 
         * least, we will call H5C_jb__write_to_buffer().  If this 
         * proves too costly, further optimizations will be necessary.
         */

        HDmemcpy(buf, (const void *)sig_ptr, H5C_BJNL__SIG_LEN);

        buf[H5C_BJNL__SIG_LEN] = version;

        if ( H5C_jb__write_to_buffer(struct_ptr, 
                                      H5C_BJNL__SIG_LEN + 1, 
                                      (const char *)buf, 
                                      is_end_trans, 
                                      trans_num) < 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C_jb__write_to_buffer() failed.")
        }
    } /* end else */

    if ( struct_ptr->chksum_cur_msg ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "struct_ptr->chksum_cur_msg is already TRUE")
    }

    if ( keep_chksum ) {

        struct_ptr->chksum_cur_msg = TRUE;
        struct_ptr->msg_chksum = 0;

    } /* end if */

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C_jb_bjf__write_sig_and_ver() */
 

/******************************************************************************
 *
 * Function:		H5C_jb_bjf__write_trans_num()
 *
 * Programmer:		John Mainzer
 *			4/24/09
 *
 * Purpose:		Write the transaction number in a binary journal file 
 *			message to the ring buffer as efficiently as possible.
 *
 *			If the space available in the current buffer in 
 *			the ring buffer is big enough, just write the 
 *                      transaction number directly into the buffer and 
 *			update its fields accordingly.  
 *
 *			If the transaction will cross or touch buffer 
 *			boundaries, construct binary representation of the 
 *			transaction number in a buffer, and pass it to 
 *			H5C_jb__write_to_buffer().
 *
 *			In either case, if struct_ptr->chksum_cur_msg is TRUE,
 *			update struct_ptr->msg_chksum.
 *
 *			Note that this function will probably prove to be
 *			a hot spot in profiling, so we should more or less
 *			plan on converting it into a macro at some point.
 *
 * Returns:		SUCCEED on success.
 *			FAIL on failure.
 *
 * Changes:		None.
 *
 ******************************************************************************/

static herr_t 
H5C_jb_bjf__write_trans_num(H5C_jbrb_t * struct_ptr,
                             hbool_t is_end_trans,
                             uint64_t trans_num)
{
    herr_t ret_value = SUCCEED;      /* Return value */
    uint8_t * p;

    FUNC_ENTER_NOAPI(H5C_jb_bjf__write_trans_num, FAIL)

    HDassert( struct_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );
    HDassert( trans_num > 0 ); 

    /* is_end_trans must be FALSE if struct_ptr->chksum_cur_msg is TRUE.
     * Throw an error if this invarient doesn't hold.
     */

    if ( ( is_end_trans ) && ( struct_ptr->chksum_cur_msg ) ) {

       HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                   "is_end_trans and struct_ptr->chksum_cur_msg both true.")
    }

    if ( H5C__TRANS_NUM_SIZE < struct_ptr->cur_buf_free_space ) {

        /* If the transaction number will fit in the current buffer with space
         * left over, just write it directly into the buffer, and touch up the 
         * ring buffer fields accordingly.  
         *
         * This is the common case, so when we convert this function into 
         * a macro, this will allow us to avoid a function call in the vast 
         * majority of cases.
         */

	/* write data into journal buffer */
        p = (uint8_t *)(struct_ptr->head);
        UINT64ENCODE(p, trans_num);

        HDassert( p == ((uint8_t *)(struct_ptr->head + H5C__TRANS_NUM_SIZE)) );

        /* increment bufs_in_use as necessary -- do this differently
         * for aio and sio.
         */
        if ( ( ( struct_ptr->bufs_in_use == 0 )
               &&
               ( ! struct_ptr->use_aio )
             )
             ||
             ( ( struct_ptr->cur_buf_free_space == struct_ptr->buf_size )
               &&
               ( struct_ptr->use_aio )
             )
           ) {

            struct_ptr->bufs_in_use++;
        }

        /* Update the check sum if required */
        if ( struct_ptr->chksum_cur_msg ) {

            struct_ptr->msg_chksum = 
		H5_checksum_metadata((const void *)(struct_ptr->head), 
                                     H5C__TRANS_NUM_SIZE, 
                                     struct_ptr->msg_chksum);
        }

        /* update head pointer */
        struct_ptr->head = &(struct_ptr->head[H5C__TRANS_NUM_SIZE]);

        /* update current buffer usage */
        struct_ptr->cur_buf_free_space -= H5C__TRANS_NUM_SIZE;

        /* update fields used only with SIO: */
        if( ! struct_ptr->use_aio ) {

            /* update rb_free_space */
            struct_ptr->rb_free_space -= H5C__TRANS_NUM_SIZE;

            /* update end of buffer space */
            struct_ptr->rb_space_to_rollover -= H5C__TRANS_NUM_SIZE;
        }


        if ( is_end_trans == TRUE ) {

            (*struct_ptr->trans_tracking)[struct_ptr->put] = trans_num;
        } 

        HDassert( struct_ptr->cur_buf_free_space > 0 );

    } else {

        /* Here, handle the case where the write will reach the edge
         * of a buffer.  This gets a bit more complex, so for now at 
         * least, we will construct a buffer containing a binary representation
         * of the transaction number, and then call H5C_jb__write_to_buffer().
         * If this proves too costly, further optimizations will be necessary.
         */

        uint8_t buf[H5C__TRANS_NUM_SIZE + 1];

        p = buf;

        UINT64ENCODE(p, trans_num);

        HDassert( p == &(buf[H5C__TRANS_NUM_SIZE]) );

        if ( H5C_jb__write_to_buffer(struct_ptr, 
                                      H5C__TRANS_NUM_SIZE, 
                                      (const char *)buf,
                                      is_end_trans, 
                                      trans_num) != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C_jb__write_to_buffer() failed.")
        }

        /* Update the check sum if required */
        if ( struct_ptr->chksum_cur_msg ) {

            struct_ptr->msg_chksum = 
 			H5_checksum_metadata((const void *)(buf), 
                                             H5C__TRANS_NUM_SIZE, 
                                             struct_ptr->msg_chksum);
        }

    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb_bjf__write_trans_num() */


/******************************************************************************
 *
 * Function:		H5C_jb__bin2hex
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Tuesday, March 4, 2008
 *
 * Purpose:		Convert binary data into hexadecimal.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

herr_t 
H5C_jb__bin2hex(const uint8_t * buf, 
                 char * hexdata,
                 size_t * hexlength,
                 size_t buf_size)

{
    size_t         v;                   /* Local index variable */
    uint8_t        c;
    char *         t;
	
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5C_jb__bin2hex)

    t = hexdata;
    t[0] = ' ';
    for (v = 0; v < buf_size; v++) {

        t = &hexdata[v * 3 + 1];
        c = buf[v];
        HDsnprintf(t, (size_t)3, "%02x ", c);
        t[2] = ' ';

    } /* end for */

    t[3] = '\n';
    t[4] = '\0';

    * hexlength = v * 3 + 2;

    FUNC_LEAVE_NOAPI(SUCCEED)

} /* end H5C_jb__bin2hex*/


/******************************************************************************
 *
 * Function:		H5C_jb__comment
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Wednesday, February 6, 2008
 *
 * Purpose:		Insert the supplied comment in the journal file. This 
 * 			call may be ignored if the journal file is machine 
 *			readable.
 *
 * Returns:		SUCCEED on success.
 *
 * Changes:		Turned this function into a switch board function,
 *			calling either the human readable or the binary 
 *			journal file version of the function as indicated 
 *			by struct_ptr->human_readable.  
 *
 *			The original version of this file has been renamed
 *			to H5C_jb_hrjf__comment().
 *
 *							JRM -- 4/2/09
 *
 ******************************************************************************/

herr_t 
H5C_jb__comment(H5C_jbrb_t * struct_ptr,
		 const char * comment_ptr)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C_jb__comment, FAIL)
	
    /* Check Arguments */
    HDassert( struct_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );
    HDassert( comment_ptr != NULL );

    if ( struct_ptr->human_readable ) {

        if ( H5C_jb_hrjf__comment(struct_ptr, comment_ptr) != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C_jb_hrjf__comment() failed.")
        }
    } else {

        if ( H5C_jb_bjf__comment(struct_ptr, comment_ptr) != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C_jb_bjf__comment() failed.")
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C_jb__comment */


/*****************************************************************************
 *
 * Function:		H5C_jb__end_transaction
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Wednesday, February 6, 2008
 *
 * Purpose:		Verify that the supplied transaction is in progress,
 *			and that at least one journal entry has been written 
 *			under it. Then construct an end transaction message,
 *			and write it to the current journal buffer. Make note
 *			that the supplied transaction is closed, and that no
 *			transaction is in progress.
 *
 * Returns:		SUCCEED on success.
 *
 * Changes:		Turned this function into a switch board function,
 *			calling either the human readable or the binary 
 *			journal file version of the function as indicated 
 *			by struct_ptr->human_readable.  
 *
 *			The original version of this file has been renamed
 *			to H5C_jb_hrjf__end_transaction().
 *
 *							JRM -- 4/2/09
 *
 *****************************************************************************/

herr_t
H5C_jb__end_transaction(H5C_jbrb_t * struct_ptr,
			 uint64_t trans_num)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C_jb__end_transaction, FAIL)

    /* Check Arguments */
    HDassert( struct_ptr );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );

    if ( struct_ptr->human_readable ) {

        if ( H5C_jb_hrjf__end_transaction(struct_ptr, trans_num) != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C_jb_hrjf__end_transaction() failed.")
        }
    } else {

        if ( H5C_jb_bjf__end_transaction(struct_ptr, trans_num) != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C_jb_bjf__end_transaction() failed.")
        }
    }

    H5C__JBRB__UPDATE_STATS_FOR_TRANS_COMPLETED(struct_ptr);
	
done:
	
    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C_jb__end_transaction */


/******************************************************************************
 *
 * Function:		H5C_jb__eoa
 *
 * Programmer:		Mike McGreevy <mamcgree@hdfgroup.org>
 *			July 29, 2008
 *
 * Purpose:		Insert the supplied EOA into the journal file.
 *
 * Returns:		SUCCEED on success.
 *
 * Changes:		Turned this function into a switch board function,
 *			calling either the human readable or the binary 
 *			journal file version of the function as indicated 
 *			by struct_ptr->human_readable.  
 *
 *			The original version of this file has been renamed
 *			to H5C_jb_hrjf__eoa().
 *
 *							JRM -- 4/2/09
 *
 ******************************************************************************/

herr_t 
H5C_jb__eoa(H5C_jbrb_t * struct_ptr,
             haddr_t eoa)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C_jb__eoa, FAIL)
	
    /* Check Arguments */
    HDassert( struct_ptr );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );
    HDassert( struct_ptr->hdf5_file_name );

    if ( struct_ptr->human_readable ) {

        if ( H5C_jb_hrjf__eoa(struct_ptr, eoa) != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C_jb_hrjf__eoa() failed.")
        }
    } else {

        if ( H5C_jb_bjf__eoa(struct_ptr, eoa) != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C_jb_bjf__eoa() failed.")
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb__eoa() */


/******************************************************************************
 *
 * Function:		H5C_jb__flush
 *
 * Programmer:		John Mainzer -- 1/14/10
 *
 * Purpose:		Determine whether AIO is enabled, and then call the 
 *			appropriate flush routine.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

herr_t 
H5C_jb__flush(H5C_jbrb_t * struct_ptr)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C_jb__flush, FAIL)

    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC);
	
    /* Check if transaction is in progress */

    if (struct_ptr->trans_in_prog != FALSE) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "Attempt to flush buffers with transaction in progress.")
    } /* end if */

    if ( struct_ptr->use_aio ) {

        ret_value = H5C_jb_aio__flush(struct_ptr);

    } else {

        ret_value = H5C_jb_sio__flush(struct_ptr);
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb__flush() */


/******************************************************************************
 *
 * Function:		H5C_jb__get_last_transaction_on_disk
 *
 * Programmer:		JRM -- 1/20/10
 *
 * Purpose:		Determine whether we are using aio for journal 
 *			entry writes, and then call the appropriate
 *			function.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

herr_t 
H5C_jb__get_last_transaction_on_disk(H5C_jbrb_t * struct_ptr,
				      uint64_t * trans_num_ptr)
{
    herr_t result;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C_jb__get_last_transaction_on_disk, FAIL)
	
    /* Check Arguments */
    HDassert( struct_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );
    HDassert( trans_num_ptr != NULL );

    if ( struct_ptr->use_aio ) {

        result = H5C_jb_aio__get_last_transaction_on_disk(struct_ptr,
                                                           trans_num_ptr);

        if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                        "H5C_jb_aio__get_last_transaction_on_disk failed")
        }
        
    } else {

        result = H5C_jb_sio__get_last_transaction_on_disk(struct_ptr,
                                                           trans_num_ptr);

        if ( result != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                        "H5C_jb_sio__get_last_transaction_on_disk failed")
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C_jb__get_last_transaction_on_disk */


/******************************************************************************
 *
 * Function:		H5C_jb_hrjf__comment
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Wednesday, February 6, 2008
 *
 * Purpose:		Insert the supplied comment in the journal file. This 
 * 			call may be ignored if the journal file is machine 
 *			readable.
 *
 * Returns:		SUCCEED on success.
 *
 * Changes:		Renamed H5C_jb__comment() to H5C_jb_hrjf__comment().  
 *
 *							JRM -- 5/2/09
 *
 ******************************************************************************/

static herr_t 
H5C_jb_hrjf__comment(H5C_jbrb_t * struct_ptr,
 		      const char * comment_ptr)
{
    char * temp = NULL;
    size_t temp_len;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C_jb_hrjf__comment, FAIL)
	
    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(comment_ptr);
    HDassert(struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC);
    HDassert(struct_ptr->hdf5_file_name);

    /* Verify that header message is present in journal file or ring buffer. 
     * If not, write it. 
     */
    if ( struct_ptr->header_present == FALSE ) {

        if ( H5C_jb__write_header_entry(struct_ptr) != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C_jb__write_header_entry() failed.\n")
        }

    } /* end if */

    temp_len = HDstrlen(comment_ptr) + 11;

    if ( NULL == (temp = (char *)H5MM_malloc(temp_len + 1)) ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                    "allocation of temp buffer failed.")
    }

    /* Write comment message */
    HDsnprintf(temp, (temp_len + 1), "C comment %s\n", comment_ptr);

    HDassert ( temp_len == HDstrlen(temp) );

    if ( H5C_jb__write_to_buffer(struct_ptr, 
                                  temp_len, 
                                  temp, 
                                  FALSE, 
                                  struct_ptr->cur_trans) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C_jb__write_to_buffer() failed.\n")
    }

done:

    if ( NULL != temp ) {

        temp = (char *)H5MM_xfree(temp);

        if ( NULL != temp ) {

            HDONE_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "free of assembly buffer failed.")
        }
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb hrjf__comment() */


/*****************************************************************************
 *
 * Function:		H5C_jb_hrjf__end_transaction
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Wednesday, February 6, 2008
 *
 * Purpose:		Verify that the supplied transaction is in progress,
 *			and that at least one journal entry has been written 
 *			under it. Then construct an end transaction message,
 *			and write it to the current journal buffer. Make note
 *			that the supplied transaction is closed, and that no
 *			transaction is in progress.
 *
 * Returns:		SUCCEED on success.
 *
 * Changes:		Renamed H5C_jb__end_transaction() to
 *			H5C_jb_hrjf__end_transaction().  
 *							JRM -- 5/2/09
 *
 *****************************************************************************/

static herr_t
H5C_jb_hrjf__end_transaction(H5C_jbrb_t * struct_ptr,
			      uint64_t trans_num)
{
    char temp[25];
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C_jb_hrjf__end_transaction, FAIL)

    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC);
	
    /* Verify that the supplied transaction is in progress */
    if ( ( struct_ptr->trans_in_prog != TRUE ) ||
         ( struct_ptr->cur_trans != trans_num ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "Transaction not in progress or bad transaction number.")
    } /* end if */
	
    /* Verify that at least one journal entry has been written under 
     * the current transaction 
     */
    if ( struct_ptr->jentry_written != TRUE ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "Empty transaction -- at least one journal entry required.")
    } /* end if */


    /* Prepare end transaction message */
    HDsnprintf(temp, (size_t)25, "3 end_trans %llu\n", trans_num);

    /* Write end transaction message */
    if ( H5C_jb__write_to_buffer(struct_ptr, HDstrlen(temp), temp, 
			          TRUE, trans_num ) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C_jb__write_to_buffer() failed.\n")
    } /* end if */

    /* reset boolean flag indicating if at least one journal entry has 
     * been written under transaction 
     */
    struct_ptr->jentry_written = FALSE;

    /* Close current transaction */
    struct_ptr->trans_in_prog = FALSE;

done:
	
    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C_jb_hrjf__end_transaction */


/******************************************************************************
 *
 * Function:		H5C_jb_hrjf__eoa
 *
 * Programmer:		Mike McGreevy <mamcgree@hdfgroup.org>
 *			July 29, 2008
 *
 * Purpose:		Insert the supplied EOA into the journal file.
 *
 * Returns:		SUCCEED on success.
 *
 * Changes:		Renamed H5C_jb__eoa() to H5C_jb_hrjf__eoa().  
 *							JRM -- 5/2/09
 *
 ******************************************************************************/

static herr_t 
H5C_jb_hrjf__eoa(H5C_jbrb_t * struct_ptr,
		  haddr_t eoa)
{
    char temp[41];
    size_t temp_len = 41;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C_jb_hrjf__eoa, FAIL)
	
    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC);
    HDassert(struct_ptr->hdf5_file_name);

    /* Verify that header message is present in journal file or ring buffer. 
     * If not, write it. 
     */
    if ( struct_ptr->header_present == FALSE ) {

        if ( H5C_jb__write_header_entry(struct_ptr) != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C_jb__write_header_entry() failed.\n")
        }
    } /* end if */

    /* Write EOA message */
    HDsnprintf(temp, temp_len, "E eoa_value 0x%llx\n", eoa);

    HDassert ( HDstrlen(temp) < temp_len );

    if ( H5C_jb__write_to_buffer(struct_ptr,  
                                  HDstrlen(temp), 
                                  temp, 
                                  FALSE, 
                                  struct_ptr->cur_trans) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C_jb__write_to_buffer() failed.\n")
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb_hrjf__eoa() */


/******************************************************************************
 *
 * Function:		H5C_jb_hrjf__journal_entry
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Wednesday, February 6, 2008
 *
 * Purpose:		Verify that the specified transaction is open. Then
 *			construct a journal entry recording the supplied base
 *			address, length, and body, and write it to the current
 *			journal buffer.
 *
 * Returns:		SUCCEED on success.
 *
 * Changes:		Renamed H5C_jb__journal_entry() to 
 *			H5C_jb_hrjf__journal_entry().  
 *							JRM -- 5/2/09
 *
 ******************************************************************************/

static herr_t 
H5C_jb_hrjf__journal_entry(H5C_jbrb_t * struct_ptr,
	   		    uint64_t trans_num,
			    haddr_t base_addr,
			    size_t length,
			    const uint8_t * body)
{

    char * temp = NULL;
    char * hexdata = NULL;
    size_t hexlength;
    herr_t ret_value = SUCCEED;
    uint8_t * bodydata;

    FUNC_ENTER_NOAPI(H5C_jb_hrjf__journal_entry, FAIL)

    /* Check Arguments */
    HDassert(struct_ptr != NULL);
    HDassert(struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC);

    /* Make a copy of body data */
    if ( (bodydata = (uint8_t *)H5MM_malloc(length)) == NULL ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                   "allocation of assembly buffer failed.");
    }

    HDmemcpy(bodydata, body, length);
	
    /* Verify that the supplied transaction is in progress */
    if ( ( struct_ptr->trans_in_prog != TRUE ) ||
         ( struct_ptr->cur_trans != trans_num ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "Transaction not in progress or bad transaction number.")
    } /* end if */

    if ( (temp = (char *)H5MM_malloc(length + 100)) == NULL ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                   "allocation of assembly buffer failed.");
    }

    if ( (hexdata = (char *)H5MM_malloc(length * 40)) == NULL ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                   "allocation of assembly buffer failed.");
    }

    /* Write journal entry */
    HDsnprintf(temp, 
               (size_t)(length + 100),
               "2 trans_num %llu length %zu base_addr 0x%lx body ", 
 	       trans_num, 
	       length, 
	       (unsigned long)base_addr);

    if ( H5C_jb__write_to_buffer(struct_ptr, 
                                  HDstrlen(temp), 
                                  temp, 
                                  FALSE, 
                                  trans_num) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C_jb__write_to_buffer() failed.\n")
    } /* end if */

    /* Convert data from binary to hex */
    H5C_jb__bin2hex(bodydata, hexdata, &hexlength, length);

    if ( H5C_jb__write_to_buffer(struct_ptr, 
                                  hexlength, 
                                  hexdata, 
                                  FALSE, 
                                  trans_num) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C_jb__write_to_buffer() failed.\n")
    } /* end if */

    /* Indicate that at least one journal entry has been written under 
     * this transaction 
     */
    if ( struct_ptr->jentry_written == FALSE ) {

	struct_ptr->jentry_written = TRUE;
    }

done:

    if ( bodydata != NULL ) {

        bodydata = (uint8_t *)H5MM_xfree(bodydata);

        if ( bodydata != NULL ) {

            HDONE_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "free of assembly buffer failed.")
        }
    } /* end if */

    if ( temp != NULL ) {

        temp = (char *)H5MM_xfree(temp);

        if ( temp != NULL ) {

            HDONE_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "free of assembly buffer failed.")
        }
    } /* end if */

    if ( hexdata != NULL ) {

        hexdata = (char *)H5MM_xfree(hexdata);

        if ( hexdata != NULL ) {

            HDONE_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "free of assembly buffer failed.")
        }
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb_hrjf__journal_entry() */


/******************************************************************************
 *
 * Function:		H5C_jb_hrjf__start_transaction
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Wednesday, February 6, 2008
 *
 * Purpose:		Verify that there is no transaction in progress, and
 *			that the supplied transaction number greater than 
 *			the last.  Then construct a start transaction message, 
 *			and write it to the current journal buffer. Make note
 *			of the fact that the supplied transaction is in
 *			progress.
 *
 * Returns:		SUCCEED on success.
 *
 * Changes:		Renamed H5C_jb__start_transaction() to 
 *			H5C_jb_hrjf__start_transaction().  
 *							JRM -- 5/2/09
 *
 ******************************************************************************/

static herr_t 
H5C_jb_hrjf__start_transaction(H5C_jbrb_t * struct_ptr,
			   uint64_t trans_num)

{
    char temp[150];
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C_jb_hrjf__start_transaction, FAIL)

    /* Check Arguments */
    HDassert( struct_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );
	
    /* Verify that there is no transaction in progress */
    if ( struct_ptr->trans_in_prog != FALSE ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "Transaction already in progress.")
    } /* end if */

    /* Verify that the supplied transaction number greater than the last */
    if ( (struct_ptr->cur_trans) >= trans_num ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "New transaction out of sequence.")
    } /* end if */

    /* Verify that header message is present in journal file or ring buffer. 
     * If not, write it. 
     */
    if ( struct_ptr->header_present == FALSE ) {

        if ( H5C_jb__write_header_entry(struct_ptr) != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C_jb__write_header_entry() failed.\n")
        }

    } /* end if */

    /* Write start transaction message */
    HDsnprintf(temp, (size_t)150, "1 bgn_trans %llu\n", trans_num);

    if ( H5C_jb__write_to_buffer(struct_ptr, 
                                  HDstrlen(temp), 
                                  temp, 
			          FALSE, 
                                  trans_num) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C_jb__write_to_buffer() failed.\n")
    } /* end if */
		
    /* Make note of the fact that supplied transaction is in progress */
    struct_ptr->trans_in_prog = TRUE;
    struct_ptr->cur_trans = trans_num;

done:
	
    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb_hrjf__start_transaction() */


/******************************************************************************
 *
 * Function:		H5C_jb__init
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Tuesday, February 5, 2008
 *
 * Purpose:		Initialize the supplied instance of H5C_jbrb_t as
 *			specified by the buf_size and num_bufs fields. Open the
 *			journal file whose name is supplied in journal_file_name
 *			for either synchronous or asynchronous I/O as specified
 * 			by use_aio.
 *
 * Returns:		SUCCEED on success.
 *
 * Changes:		JRM -- 2/10/09
 *			Added the journal_magic parameter and related code.
 *
 *			Also deleted code to write the header message.
 *			Since the base address of the journal magic in 
 *			the HDF5 file isn't available at this time, wait
 *			until our first real entry to write the header.
 *
 *			JRM -- 4/16/09
 *			Added the sizeof_addr and sizeof_size parameters, and
 *			associated code.  These parameters must contain the 
 *			values of the same name in the instance of H5F_file_t
 *			associated with the target file.
 *
 *			JRM -- 12/7/09
 *			Added initialization for posix aio fields in 
 *			H5C_jbrb_t.
 *
 *			JRM -- 2/21/10
 *			Added call to H5C_jb_stats__reset();
 *
 ******************************************************************************/

herr_t 
H5C_jb__init(H5C_jbrb_t * struct_ptr,  	
              const int32_t journal_magic,
	      const char * HDF5_file_name,	 	
	      const char * journal_file_name, 	
	      size_t buf_size,		
	      int num_bufs,		 	
	      hbool_t use_aio,		
 	      hbool_t human_readable,
              size_t sizeof_addr,
              size_t sizeof_size)
{
    int 	i;
    herr_t 	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C_jb__init, FAIL)

    /* Check Arguments */
    HDassert( struct_ptr );
    HDassert( HDF5_file_name );
    HDassert( journal_file_name );
    HDassert( buf_size > 0 );
    HDassert( num_bufs > 0 );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );

    /* Initialize Fields of H5C_jbrb_t structure.  Note that we will
     * overwrite some of these initializations almost immediately.
     */
    struct_ptr->journal_magic = journal_magic;
    struct_ptr->journal_file_fd = -1;
    struct_ptr->num_bufs = num_bufs;
    struct_ptr->buf_size = buf_size;
    struct_ptr->bufs_in_use = 0;
    struct_ptr->writes_in_progress = 0;
    struct_ptr->jvers = H5C__JOURNAL_VERSION;
    struct_ptr->get = 0;
    struct_ptr->put = 0;
    struct_ptr->jentry_written = FALSE;
    struct_ptr->use_aio = use_aio;
    struct_ptr->human_readable = human_readable;
    struct_ptr->offset_width = (int)sizeof_addr;
    struct_ptr->length_width = (int)sizeof_size;
    struct_ptr->chksum_cur_msg = FALSE;
    struct_ptr->msg_chksum = 0;
    struct_ptr->journal_is_empty = TRUE;
    struct_ptr->cur_trans = 0;
    struct_ptr->last_trans_queued = 0;
    struct_ptr->last_trans_written = 0;
    struct_ptr->last_trans_on_disk = 0;
    struct_ptr->trans_in_prog = FALSE;
    struct_ptr->jname = HDstrdup(journal_file_name);

    if ( struct_ptr->jname == NULL ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                "allocation of space for copy of journal_file_name failed.");
    } 

    struct_ptr->hdf5_file_name = HDstrdup(HDF5_file_name);

    if ( struct_ptr->jname == NULL ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                "allocation of space for copy of HDF5_file_name failed.");
    } 

    struct_ptr->header_present = FALSE;
    struct_ptr->cur_buf_free_space = buf_size;
    struct_ptr->rb_space_to_rollover = (size_t)num_bufs * buf_size;
    struct_ptr->rb_free_space = (size_t)num_bufs * buf_size;
    struct_ptr->head = NULL;
    struct_ptr->trans_tracking = NULL;
    struct_ptr->aio_ctl_blks = NULL;
    struct_ptr->aio_next_buf_offset = (off_t)0;
/* Comment this out to work on the Mac, currently */
#if 1
    struct_ptr->use_aio_fsync = use_aio;
#else
    struct_ptr->use_aio_fsync = FALSE;
#endif
    struct_ptr->aio_sync_q_head = NULL;
    struct_ptr->aio_sync_q_tail = NULL;
    struct_ptr->aio_sync_q_len = 0;
    struct_ptr->buf = NULL;
	
    /* Open journal file */
#if 0 /* JRM */
    HDfprintf(stdout, "%s: journal file name = %s.\n", FUNC, journal_file_name);
#endif /* JRM */
    struct_ptr->journal_file_fd = 
	    HDopen(journal_file_name, O_WRONLY|O_CREAT|O_EXCL, 0777);

    if ( struct_ptr->journal_file_fd  == -1) {

#if 0 /* JRM */
        HDfprintf(stdout, "%s: errno = %d (%s).\n", 
                  FUNC, errno, strerror(errno));
#endif /* JRM */
        HGOTO_ERROR(H5E_FILE, H5E_CANTCREATE, FAIL, \
                    "Can't create journal file.  Does it already exist?")
    } /* end if */

	
    /* Allocate space for the ring buffer's journal buffer pointers */
    struct_ptr->buf = (char *((*)[]))
		H5MM_malloc((size_t)(struct_ptr->num_bufs) * sizeof(char *));

    if ( struct_ptr->buf == NULL ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                    "allocation of buf pointer array failed.");
    } /* end if */
	
    /* Allocate space for journal buffers */
    (*struct_ptr->buf)[0] = (char *)
            H5MM_malloc(struct_ptr->buf_size * (size_t)(struct_ptr->num_bufs));

    if ( (*struct_ptr->buf)[0] == NULL ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                    "allocation of buffers failed.");
    } /* end if */

    /* Allocate space for the purposes of tracking the last 
     * transaction on disk 
     */
    struct_ptr->trans_tracking = (uint64_t (*)[])
    	H5MM_malloc((size_t)(struct_ptr->num_bufs) * sizeof(uint64_t));

    if ( struct_ptr->trans_tracking == NULL ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                    "allocation of trans_tracking failed.");
    } /* end if */
	
    /* Initialize the transaction tracking array */
    for (i=0; i<struct_ptr->num_bufs; i++)
    {
	(*struct_ptr->trans_tracking)[i] = 0;
    }
	
    /* Make journal buffer pointers point to the right location in 
     * chunk of allocated memory above 
     */
    for ( i = 1; i < struct_ptr->num_bufs; i++ )
    {
	(*struct_ptr->buf)[i] = 
		&((*struct_ptr->buf)[0])[i * (int)(struct_ptr->buf_size)];
    }

    /* Define head pointer to point at where we are writing to in the buffer */
    struct_ptr->head = (*struct_ptr->buf)[struct_ptr->put];

    /* if we are using aio, allocate the array of struct aiocb used to manage
     * and track the asychronous writes.
     */
    if ( struct_ptr->use_aio ) {

        struct_ptr->aio_ctl_blks = (struct aiocb (*)[])
		H5MM_malloc(sizeof(struct aiocb) * 
                            (size_t)(struct_ptr->num_bufs));

        if ( struct_ptr->aio_ctl_blks == NULL ) {

	    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                        "allocation of aio_ctl_blks failed.");
        }

        /* Set the aio_fildes field of each aio control block to -1.  We
         * use this value to indicate that no asynchronous write is currently
         * in progress.  
         *
	 * Don't bother with zeroing out the blocks now -- we will do this 
	 * to each block just prior to use.
	 */
        for ( i = 0; i < struct_ptr->num_bufs; i++ )
        {
            ((*(struct_ptr->aio_ctl_blks))[i]).aio_fildes = -1;
        }
    }

    if ( H5C_jb_stats__reset(struct_ptr) != SUCCEED ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "H5C_jb_stats__reset() failed.")
    }

done:
	
    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C_jb__init */


/******************************************************************************
 *
 * Function:		H5C_jb__journal_entry
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Wednesday, February 6, 2008
 *
 * Purpose:		Verify that the specified transaction is open. Then
 *			construct a journal entry recording the supplied base
 *			address, length, and body, and write it to the current
 *			journal buffer.
 *
 * Returns:		SUCCEED on success.
 *
 * Changes:		Turned this function into a switch board function,
 *			calling either the human readable or the binary 
 *			journal file version of the function as indicated 
 *			by struct_ptr->human_readable.  
 *
 *			The original version of this file has been renamed
 *			to H5C_jb_hrjf__journal_entry().
 *
 *							JRM -- 4/2/09
 *
 ******************************************************************************/

herr_t 
H5C_jb__journal_entry(H5C_jbrb_t * struct_ptr,
			uint64_t trans_num,
			haddr_t base_addr,
			size_t length,
			const uint8_t * body)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C_jb__journal_entry, FAIL)

    /* Check Arguments */
    HDassert( struct_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );

    if ( struct_ptr->human_readable ) {

        if ( H5C_jb_hrjf__journal_entry(struct_ptr, trans_num, base_addr, 
                                         length, body) != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C_jb_hrjf__journal_entry() failed.")
        }
    } else {

        if ( H5C_jb_bjf__journal_entry(struct_ptr, trans_num, base_addr,
                                        length, body) != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C_jb_bjf__journal_entry() failed.")
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb__journal_entry() */


/******************************************************************************
 *
 * Function:		H5C_jb_sio__flush
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Wednesday, February 6, 2008
 *
 * Purpose:		Verify that there is no transaction in progress. Then
 * 			flush all journal entries in the journal buffers to the
 * 			journal file. Do not return until all entries are on
 * 			disk.
 *
 * Returns:		SUCCEED on success.
 *
 * Changes:		Renamed function from H5C_jb__flush() to
 *			H5C_jb_sio__flush().  Added code to verify that 
 *			SIO is selected.
 *
 *						JRM -- 1/14/10
 *
 ******************************************************************************/

static herr_t 
H5C_jb_sio__flush(H5C_jbrb_t * struct_ptr)
{
    int result;
    int i;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C_jb_sio__flush, FAIL)

    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC);
    HDassert(struct_ptr->use_aio == FALSE);
	
    /* Check if transaction is in progress */

    if (struct_ptr->trans_in_prog != FALSE) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "Attempt to flush buffers with transaction in progress.")
    } /* end if */

    if (struct_ptr->get > struct_ptr->put) {

	/* write from get through end of buffer */
	result = HDwrite(struct_ptr->journal_file_fd, 
	      (*struct_ptr->buf)[struct_ptr->get], 
	      (size_t)(struct_ptr->num_bufs - struct_ptr->get) * 
			struct_ptr->buf_size);

	if ( result == -1 ) {

            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
		        "Journal file write failed(1).")
        }
        
	struct_ptr->bufs_in_use -= (struct_ptr->num_bufs - struct_ptr->get);
        struct_ptr->rb_free_space += 
	    (size_t)(struct_ptr->num_bufs - struct_ptr->get) * struct_ptr->buf_size;
        struct_ptr->get = 0;
        
    } /* end if */

    if (struct_ptr->get < struct_ptr->put) {

	/* write from get up to, but not including, put */
	result = HDwrite(struct_ptr->journal_file_fd, 
	            (*struct_ptr->buf)[struct_ptr->get], 
	            (size_t)(struct_ptr->put - struct_ptr->get) * 
			struct_ptr->buf_size);

	if ( result == -1 ) {

            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
		        "Journal file write failed (2).")
        }

	struct_ptr->bufs_in_use -= (struct_ptr->put - struct_ptr->get);
        struct_ptr->rb_free_space += 
		(size_t)(struct_ptr->put - struct_ptr->get) * 
                struct_ptr->buf_size;
        struct_ptr->get = struct_ptr->put;

    } /* end if */

    if ( struct_ptr->cur_buf_free_space != struct_ptr->buf_size ) {

        /* flush partially filled portion of current journal buffer to disk */
	result = HDwrite(struct_ptr->journal_file_fd, 
	               (*struct_ptr->buf)[struct_ptr->put], 
	               struct_ptr->buf_size - struct_ptr->cur_buf_free_space);

	if ( result == -1 ) {

            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
		        "Journal file write failed (3).")
        }

	struct_ptr->bufs_in_use--;
        struct_ptr->rb_free_space += 
            (struct_ptr->buf_size - struct_ptr->cur_buf_free_space);

    } /* end if */

    HDassert(struct_ptr->bufs_in_use == 0);
    HDassert(struct_ptr->rb_free_space == 
 	     (size_t)(struct_ptr->num_bufs) * struct_ptr->buf_size);

    /* perform sync to ensure everything gets to disk before returning
     *
     * Note: there is no HDfsync function, so for now, the standard
     * fsync is being used. 
     */
    if ( fsync(struct_ptr->journal_file_fd) < 0 ) {

        HGOTO_ERROR(H5E_IO, H5E_SYNCFAIL, FAIL, "Journal file sync failed.")
    }

    H5C__JBRB__UPDATE_STATS_FOR_CALL_TO_FSYNC(struct_ptr)

    /* record last transaction number that made it to disk */
    struct_ptr->last_trans_on_disk = 
		(*struct_ptr->trans_tracking)[struct_ptr->put];

    /* MIKE: optimization note: don't reset to top of ring buffer. 
     * instead, keep filling out current buffer so we can keep writes 
     * on block boundaries. 
     */
    struct_ptr->cur_buf_free_space = struct_ptr->buf_size;
    struct_ptr->rb_space_to_rollover = 
		(size_t)(struct_ptr->num_bufs) * struct_ptr->buf_size;
    struct_ptr->head = (*struct_ptr->buf)[0];
    struct_ptr->put = 0;

    /* Propogate the last transaction on in the buffers throughout the 
     * transaction tracking array. */
    for ( i = 0; i < struct_ptr->num_bufs; i++ ) {

	(*struct_ptr->trans_tracking)[i] = struct_ptr->last_trans_on_disk;
    }

    /* update get index */
    struct_ptr->get = struct_ptr->put;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C_jb_sio__flush */


/******************************************************************************
 *
 * Function:		H5C_jb_sio__flush_full_buffers
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Wednesday, February 6, 2008
 *
 * Purpose:		Flush all the dirtied buffers in the ring buffer 
 *                      starting with the buffer referenced by struct_ptr->get 
 *                      and ending with the buffer right before the one
 *                      referenced by struct_ptr->put. 
 *
 * Returns:		SUCCEED on success.
 *
 * Changes:		Changed name from H5C_jb__flush_full_buffers() to
 *			H5C_jb_sio__flush_full_buffers().
 *
 *							JRM -- 1/14/10
 *
 ******************************************************************************/

static herr_t 
H5C_jb_sio__flush_full_buffers(H5C_jbrb_t * struct_ptr)	
{
    int result;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C_jb_sio__flush_full_buffers, FAIL)

    /* this asserts that at least one buffer is in use */
    HDassert( struct_ptr->bufs_in_use > 0 );

    /* write an assert to verify that at least one buffer is full */
    HDassert( (struct_ptr->put != struct_ptr->get) ||
              (struct_ptr->rb_free_space == 0) );

    /* flush all full, dirtied journal buffers to disk */
    if ( struct_ptr->get < struct_ptr->put ) {

	/* can write solid chunk from get up to, but not 
	 * including, put 
	 */
	result = HDwrite(struct_ptr->journal_file_fd, 
	                 (*struct_ptr->buf)[struct_ptr->get], 
	                 (size_t)(struct_ptr->put - struct_ptr->get) * 
                                  struct_ptr->buf_size);

	if ( result == -1 ) {

            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
		        "Journal file write failed (1).")
        }

	struct_ptr->bufs_in_use -= (struct_ptr->put - struct_ptr->get);
        struct_ptr->rb_free_space += 
		(size_t)(struct_ptr->put - struct_ptr->get) * 
                struct_ptr->buf_size;

    } /* end if */

    else {

	/* write from get through end of buffer */
	result = HDwrite(struct_ptr->journal_file_fd, 
	                 (*struct_ptr->buf)[struct_ptr->get], 
	                 (size_t)(struct_ptr->num_bufs - struct_ptr->get) * 
			          struct_ptr->buf_size);

	if ( result == -1 ) {

            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
		        "Journal file write failed (2).")
        }

	struct_ptr->bufs_in_use -= (struct_ptr->num_bufs - struct_ptr->get);
        struct_ptr->rb_free_space += 
		(size_t)(struct_ptr->num_bufs - struct_ptr->get) * 
		struct_ptr->buf_size;

        /* if put = 0, then everything that needs to be flushed will have been
         * flushed, so we can stop here. Otherwise, need to flush all buffers
         * from the start of the ring buffer's allocated space up to, but not
         * including, the buffer indexed by put. 
         */
        if (struct_ptr->put != 0) {

            result = HDwrite(struct_ptr->journal_file_fd, 
                             (*struct_ptr->buf)[0], 
                             (size_t)(struct_ptr->put) * struct_ptr->buf_size);

	    if ( result == -1 ) {

                HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
		            "Journal file write failed(3).")
            } /* end if */

            struct_ptr->rb_free_space += 
		((size_t)(struct_ptr->put) * struct_ptr->buf_size);

        } /* end if */

	struct_ptr->bufs_in_use -= struct_ptr->put;

    } /* end else */
	
    HDassert(struct_ptr->bufs_in_use <= 1);

    /* update get index */
    struct_ptr->get = struct_ptr->put;
	
    /* record last transaction number that made it to disk */
    if (struct_ptr->put == 0) {

	struct_ptr->last_trans_on_disk = 
		(*struct_ptr->trans_tracking)[struct_ptr->num_bufs - 1];

    } else {

	struct_ptr->last_trans_on_disk = 
		(*struct_ptr->trans_tracking)[struct_ptr->put - 1];
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C_jb_sio__flush_full_buffers */


/******************************************************************************
 *
 * Function:		H5C_jb_sio__get_last_transaction_on_disk
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Wednesday, February 6, 2008
 *
 * Purpose:		Lookup the number of the last transaction to have been
 *			fully written to disk, and place its transaction
 *			number in *trans_num_ptr. If no transaction has made
 *			it to disk, load zero into *trans_num_ptr.
 *
 * Returns:		SUCCEED on success.
 *
 * Changes:		Renamed the function from 
 *
 *			H5C_jb__get_last_transaction_on_disk()
 *
 *			to 
 *
 * 			H5C_jb_sio__get_last_transaction_on_disk()
 *
 *			and added some additional sanity checks.
 *
 *						JRM -- 1/20/10
 *
 ******************************************************************************/

static herr_t 
H5C_jb_sio__get_last_transaction_on_disk(H5C_jbrb_t * struct_ptr,
				          uint64_t * trans_num_ptr)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C_jb_sio__get_last_transaction_on_disk, FAIL)
	
    /* Check Arguments */
    HDassert( struct_ptr != NULL );
    HDassert( trans_num_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );

    /* JRM: In machine readable version, lets check to see if a sync is 
     *      necessary, and call it only if it is.
     */
    /* perform a sync to ensure everything gets to disk before continuing */
    /* Note: there is no HDfsync function, so for now, the standard
       fsync is being used. */
    if(fsync(struct_ptr->journal_file_fd) < 0 ) {

        HGOTO_ERROR(H5E_IO, H5E_SYNCFAIL, FAIL, "Jounal file sync failed.")
    }

    H5C__JBRB__UPDATE_STATS_FOR_CALL_TO_FSYNC(struct_ptr)

    * trans_num_ptr = struct_ptr->last_trans_on_disk;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C_jb_sio__get_last_transaction_on_disk */


/******************************************************************************
 *
 * Function:		H5C_jb_sio__write_to_buffer
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Wednesday, February 6, 2008
 *
 * Purpose:		Put the contents of data into the journal buffers. This
 * 			is done as follows: While the data to be written is 
 * 			larger than the amount of space left in the ring buffer,
 * 			the ring buffer is filled to capacity with data and
 *			flushed. This repeats until the unwritten data remaining
 * 			can fit in the ring buffer without having to loop around
 *			to the top.
 *
 *			At this point, the rest of the data can just be written
 *			without having to break it up further. In the event
 *			the data covers more than one journal buffer, the get 
 *			and put indices are updated to state this fact. Any 
 *			journal buffers that were filled during the write are 
 *			flushed.
 *
 * Returns:		SUCCEED on success.
 *
 * Changes:		Changed the name of the function from 
 *			H5C_jb__write_to_buffer() to 
 *			H5C_jb_sio__write_to_buffer().  Added assert to
 *			verify that struct_ptr->use_aio is FALSE.
 *
 *						JRM -- 1/14/10
 *
 ******************************************************************************/

static herr_t 
H5C_jb_sio__write_to_buffer(H5C_jbrb_t * struct_ptr,	
			     size_t size,			
			     const char * data,
                             hbool_t is_end_trans,
                             uint64_t trans_num)
{
    herr_t ret_value = SUCCEED;
    uint64_t track_last_trans = 0;
    int oldput = 0;
    int i;
	
    FUNC_ENTER_NOAPI(H5C_jb_sio__write_to_buffer, FAIL)

    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(data);
    HDassert(struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC);
    HDassert(struct_ptr->use_aio == FALSE);
    HDassert( ( struct_ptr->human_readable == FALSE ) || 
              ( HDstrlen(data) == size ) );
    HDassert(struct_ptr->rb_space_to_rollover <= 
	     ((size_t)(struct_ptr->num_bufs)) * struct_ptr->buf_size);
    HDassert(struct_ptr->rb_space_to_rollover > 0); 

    /* If the data size exceeds the bounds of the ring buffer's allocated 
     * memory, loop around to top 
     */
    if (size >= struct_ptr->rb_space_to_rollover) {

	while (size >= struct_ptr->rb_space_to_rollover) {
			
	    /* Assertions */
	    HDassert(size != 0);
	    HDassert( ( struct_ptr->human_readable == FALSE ) ||
                      ( HDstrlen(data) >= struct_ptr->rb_space_to_rollover ) );

	    /* fill up remaining space in the ring buffer */
	    HDmemcpy(struct_ptr->head, data, struct_ptr->rb_space_to_rollover);
			
	    /* move head to point to start of ring buffer */
	    struct_ptr->head = (*struct_ptr->buf)[0];

            /* make note of last transaction on disk */
            track_last_trans = (*struct_ptr->trans_tracking)[struct_ptr->put];
            
            /* update rb_free_space */
            struct_ptr->rb_free_space -= struct_ptr->rb_space_to_rollover;

            /* Fill out the remainder of the trans_tracking array with
               the most recent transaction in the array.*/
	    (*struct_ptr->trans_tracking)[0] = track_last_trans;
            for (i=struct_ptr->put; i<struct_ptr->num_bufs; i++)
            {
	        (*struct_ptr->trans_tracking)[i] = track_last_trans;
            }

	    /* reset put index */
	    struct_ptr->put = 0;

	    /* update bufs_in_use as necessary */
	    struct_ptr->bufs_in_use = struct_ptr->num_bufs - struct_ptr->get;

            /* check to see if trans_tracking needs to be updated. If so,
             * then update it 
             */
            if ((size == struct_ptr->rb_space_to_rollover) &&
                (is_end_trans == TRUE)) {
                
                (*struct_ptr->trans_tracking)[struct_ptr->num_bufs - 1] 
                                                    = trans_num;
                (*struct_ptr->trans_tracking)[0] = trans_num;
            }

	    /* flush buffers */
	    if ( H5C_jb_sio__flush_full_buffers(struct_ptr) < 0 ) {

		 HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                             "H5C_jb_sio__flush_full_buffers() failed.\n")
            }

	    /* update remaining size of data to be written */
	    size = size - struct_ptr->rb_space_to_rollover;

	    /* update the data pointer to point to the remaining data to be 
	     * written 
	     */
	    data = &data[struct_ptr->rb_space_to_rollover];

	    /* update the amount of space left at end of ring buffer */
	    struct_ptr->rb_space_to_rollover = 
		    struct_ptr->buf_size * (size_t)(struct_ptr->num_bufs);

	    /* update the amount of space in the current buffer */
	    struct_ptr->cur_buf_free_space = struct_ptr->buf_size;

	} /* end while */
    } /* end if */
	
    /* If the size of the data exceeds the bounds of a single journal 
     * buffer, will write into multiple 
     */
    if (size > struct_ptr->cur_buf_free_space) {

	HDassert(struct_ptr->cur_buf_free_space != 0);

	/* write data into journal buffers */
	HDmemcpy(struct_ptr->head, data, size);

	/* update head pointer */
	struct_ptr->head = &struct_ptr->head[size];

        /* make note of last transaction on disk */
        track_last_trans = (*struct_ptr->trans_tracking)[struct_ptr->put];
        oldput = struct_ptr->put;

        /* update rb_free_space */
        struct_ptr->rb_free_space -= size;

	/* update put index */
	struct_ptr->put += (int)
            ((size - struct_ptr->cur_buf_free_space)/(struct_ptr->buf_size) + 1); 

        /* Drag the last transaction in a filled buffer value residing in the 
           old put location through the trans_tracking array to the new 
           corresponding put position. */
        for (i=oldput; i<struct_ptr->put+1; i++)
        {
            (*struct_ptr->trans_tracking)[i] = track_last_trans;
        }

	/* update current buffer usage */
	struct_ptr->cur_buf_free_space = 
            struct_ptr->rb_space_to_rollover - size - 
            (((size_t)(struct_ptr->num_bufs)) - ((size_t)(struct_ptr->put + 1))) * 
            (struct_ptr->buf_size );

	/* update bufs_in_use as necessary */
	struct_ptr->bufs_in_use = struct_ptr->put - struct_ptr->get;
	if (struct_ptr->cur_buf_free_space < struct_ptr->buf_size) {

	    struct_ptr->bufs_in_use++;
        }

        /* check to see if trans_tracking needs to be updated. If so,
         * then update it 
         */
        if ( is_end_trans == TRUE ) {
                
            if ( struct_ptr->cur_buf_free_space == struct_ptr->buf_size ) {

                (*struct_ptr->trans_tracking)[struct_ptr->put - 1] = trans_num;

            } else {

                (*struct_ptr->trans_tracking)[struct_ptr->put] = trans_num;
            }

        } /* end if */

	/* flush buffers */
	if ( H5C_jb_sio__flush_full_buffers(struct_ptr) < 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C_jb_siok__flush_full_buffers() failed.\n")
        }

	/* update space left at end of ring buffer */
	struct_ptr->rb_space_to_rollover -= size;

    } /* end if */

    /* if the data can fit in the remaining space in the current journal 
     * buffer indexed by put 
     */
    else if (size > 0)  {

	HDassert(size <= struct_ptr->cur_buf_free_space);
		
	/* write data into journal buffer */
	HDmemcpy(struct_ptr->head, data, size);

	/* increment bufs_in_use as necessary */
	if ( ( struct_ptr->bufs_in_use == 0 ) ) {

	    struct_ptr->bufs_in_use++;
        }

	/* update head pointer */
	struct_ptr->head = &struct_ptr->head[size];

        /* update rb_free_space */
        struct_ptr->rb_free_space -= size;

	/* update current buffer usage */
	struct_ptr->cur_buf_free_space -= size;

	/* update end of buffer space */
	struct_ptr->rb_space_to_rollover -= size;
		
        /* check to see if trans_tracking needs to be updated. If so,
           then update it */
        if (is_end_trans == TRUE) {
                
            (*struct_ptr->trans_tracking)[struct_ptr->put] 
                                            = trans_num;
        } /* end if */

	/* if buffer is full, flush it, and loop to the top of the 
	 * ring buffer if at the end. 
	 */
	if (struct_ptr->cur_buf_free_space == 0) {

	    if ( struct_ptr->put != (struct_ptr->num_bufs - 1) ) {
		struct_ptr->put += 1;

                /* Drag trans_tracking value into next buffer */
                (*struct_ptr->trans_tracking)[struct_ptr->put] =
                        (*struct_ptr->trans_tracking)[struct_ptr->put - 1];

	    } /* end if */
                
            else {

		struct_ptr->put = 0;

                /* Drag trans_tracking value into next buffer */
                (*struct_ptr->trans_tracking)[0] 
                 = (*struct_ptr->trans_tracking)[struct_ptr->num_bufs - 1];

                /* reset head pointer and free space values */
		struct_ptr->head = (*struct_ptr->buf)[0];
		struct_ptr->rb_space_to_rollover = 
			struct_ptr->buf_size * (size_t)(struct_ptr->num_bufs);

	    } /* end else */

	    if ( H5C_jb_sio__flush_full_buffers(struct_ptr) < 0 ) {

                HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                            "H5C_jb_sio__flush_full_buffers() failed.\n")
            } /* end if */

	    struct_ptr->cur_buf_free_space = struct_ptr->buf_size;

	} /* end if */

    } /* end else */
	
    HDassert(struct_ptr->bufs_in_use <= struct_ptr->num_bufs);

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C_jb_sio__write_to_buffer */


/******************************************************************************
 *
 * Function:		H5C_jb_stats__dump
 *
 * Programmer:		JRM -- 2/21/20
 *
 * Purpose:		If H5C__JBRB__COLLECT_STATS is TRUE, dump the 
 *			contents of the journal buffer ring buffer stats
 *			fields to stdout.
 *
 *			If H5C__JBRB__COLLECT_STATS is FALSE, do nothing. 
 *
 * Returns:		void
 *
 * Changes:		None.
 *
 ******************************************************************************/

herr_t
H5C_jb_stats__dump(H5C_jbrb_t * struct_ptr)
{
    herr_t      ret_value = SUCCEED;   /* Return value */
#if H5C__JBRB__COLLECT_STATS
    double	calls_to_aio_error_per_async_sync_await = 0.0;
#endif /* H5C__JBRB__COLLECT_STATS */

    FUNC_ENTER_NOAPI(H5C_jb_stats__dump, FAIL)

    /* This would normally be an assert, but we need to use an HGOTO_ERROR
     * call to shut up the compiler.
     */
    if ( ( struct_ptr == NULL ) ||
         ( struct_ptr->magic != H5C__H5C_JBRB_T_MAGIC ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad struct_ptr on entry")
    }

#if H5C__JBRB__COLLECT_STATS
    if ( struct_ptr->async_syncs_completed_by_await > 0 ) {

        calls_to_aio_error_per_async_sync_await = 
            (double)(struct_ptr->calls_to_aio_error_awaiting_sync) /
            (double)(struct_ptr->async_syncs_completed_by_await);
    }

    HDfprintf(stdout, 
              "buf count/size = %lld/%lld, trans completed = %lld\n",
              (long long)(struct_ptr->num_bufs),
              (long long)(struct_ptr->buf_size),
              (long long)(struct_ptr->transactions_completed));
    HDfprintf(stdout, 
              "buf writes queued full/part/total = %lld/%lld/%lld\n",
              (long long)(struct_ptr->full_buf_writes_queued),
              (long long)(struct_ptr->partial_buf_writes_queued),
              (long long)(struct_ptr->buf_writes_queued));

    HDfprintf(stdout, 
	      "buf writes completed by test/await/total = %lld/%lld/%lld\n",
              (long long)(struct_ptr->buf_writes_completed_by_test),
              (long long)(struct_ptr->buf_writes_completed_by_await),
              (long long)(struct_ptr->buf_writes_completed));

    HDfprintf(stdout,
              "async syncs queued = %lld, max sync q len = %lld.\n",
              (long long)(struct_ptr->async_syncs_queued),
              (long long)(struct_ptr->max_sync_q_len));

    HDfprintf(stdout, 
	      "async syncs completed by test/await/total = %lld/%lld/%lld.\n",
              (long long)(struct_ptr->async_syncs_completed_by_test),
              (long long)(struct_ptr->async_syncs_completed_by_await),
    	      (long long)(struct_ptr->async_syncs_completed));

    HDfprintf(stdout, 
              "ave calls to aio_error() per aio_fsync() await = %f.\n",
              calls_to_aio_error_per_async_sync_await);

     HDfprintf(stdout, "calls to fsync() = %lld.\n",
	       (long long)(struct_ptr->calls_to_fsync));
#endif /* H5C__JBRB__COLLECT_STATS */

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb_stats__dump() */


/******************************************************************************
 *
 * Function:		H5C_jb_stats__reset
 *
 * Programmer:		JRM -- 2/21/20
 *
 * Purpose:		If H5C__JBRB__COLLECT_STATS is TRUE, reset the 
 *			stats fields in the instance of H5C_jbrb_t 
 *			pointed to by struct_ptr.  
 *
 *			If H5C__JBRB__COLLECT_STATS is FALSE, do nothing. 
 *
 * Returns:		void
 *
 * Changes:		None.
 *
 ******************************************************************************/

herr_t
H5C_jb_stats__reset(H5C_jbrb_t * struct_ptr)
{
    herr_t      ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5C_jb_stats__reset, FAIL)

    /* This would normally be an assert, but we need to use an HGOTO_ERROR
     * call to shut up the compiler.
     */
    if ( ( struct_ptr == NULL ) ||
         ( struct_ptr->magic != H5C__H5C_JBRB_T_MAGIC ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad struct_ptr")
    }

#if H5C__JBRB__COLLECT_STATS
    struct_ptr->transactions_completed           = 0;
    struct_ptr->buf_writes_queued                = 0;
    struct_ptr->full_buf_writes_queued           = 0;
    struct_ptr->partial_buf_writes_queued        = 0;
    struct_ptr->buf_writes_completed             = 0;
    struct_ptr->buf_writes_completed_by_test     = 0;
    struct_ptr->buf_writes_completed_by_await    = 0;
    struct_ptr->async_syncs_queued               = 0;
    struct_ptr->async_syncs_completed            = 0;
    struct_ptr->async_syncs_completed_by_test    = 0;
    struct_ptr->async_syncs_completed_by_await   = 0;
    struct_ptr->calls_to_aio_error_awaiting_sync = 0;
    struct_ptr->max_sync_q_len                   = 0;
    struct_ptr->calls_to_fsync                   = 0;
#endif /* H5C__JBRB__COLLECT_STATS */

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb_stats__reset() */


/******************************************************************************
 *
 * Function:		H5C_jb__start_transaction
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Wednesday, February 6, 2008
 *
 * Purpose:		Verify that there is no transaction in progress, and
 *			that the supplied transaction number greater than 
 *			the last.  Then construct a start transaction message, 
 *			and write it to the current journal buffer. Make note
 *			of the fact that the supplied transaction is in
 *			progress.
 *
 * Returns:		SUCCEED on success.
 *
 * Changes:		Turned this function into a switch board function,
 *			calling either the human readable or the binary 
 *			journal file version of the function as indicated 
 *			by struct_ptr->human_readable.  
 *
 *			The original version of this file has been renamed
 *			to H5C_jb_hrjf__start_transaction().
 *
 *							JRM -- 4/2/09
 *
 ******************************************************************************/

herr_t 
H5C_jb__start_transaction(H5C_jbrb_t * struct_ptr,
			   uint64_t trans_num)

{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C_jb__start_transaction, FAIL)

    /* Check Arguments */
    HDassert( struct_ptr != NULL );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );

    if ( struct_ptr->human_readable ) {

        if ( H5C_jb_hrjf__start_transaction(struct_ptr, trans_num) 
             != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C_jb_hrjf__start_transaction() failed.")
        }
    } else {

        if ( H5C_jb_bjf__start_transaction(struct_ptr, trans_num) 
             != SUCCEED ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C_jb_bjf__start_transaction() failed.")
        }
    }
	
done:
	
    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_jb__start_transaction() */


/******************************************************************************
 *
 * Function:		H5C_jb__takedown
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Thursday, February 7, 2008
 *
 * Purpose:		Verify that the journal buffers are empty, and that the
 *			journal file has been truncated. Then close and delete
 *			the journal file associated with *struct_ptr, and free
 *			all dynamically allocated memory associated with 
 *			*struct_ptr.
 *
 * Returns:		SUCCEED on success.
 *
 * Changes:		JRM -- 12/7/09
 *			Added code to free the aio control blocks if necessary.
 *
 *			JRM -- 2/21/10
 *			Added call to H5C_jb_stats__dump().
 *
 ******************************************************************************/

herr_t 
H5C_jb__takedown(H5C_jbrb_t * struct_ptr)

{
    herr_t ret_value = SUCCEED;
	
    FUNC_ENTER_NOAPI(H5C_jb__takedown, FAIL)

    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC);

    /* dump the stats before we start the takedown, as the dump
     * routine may want to look at some of *struct_ptr's regular 
     * fields, as well as the stats fields.
     */
#if H5C__JBRB__DUMP_STATS_ON_TAKEDOWN
    if ( H5C_jb_stats__dump(struct_ptr) != SUCCEED ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
	            "H5C_jb_stats__dump() failed.")
    }
#endif /* H5C__JBRB__DUMP_STATS_ON_TAKEDOWN */
	
    /* Verify that the journal buffers are empty */
    if ( struct_ptr->bufs_in_use != 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
	            "Attempt to takedown with non-empty buffers.")
    } /* end if */	

    /* Verify that the journal file has been truncated */
    if (struct_ptr->journal_is_empty != TRUE) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
	            "Attempt to takedown with journal file not truncated.")
    } /* end if */

    /* Close and delete the journal file associated with struct_ptr */
    if ( HDclose(struct_ptr->journal_file_fd) < 0 ) {

        HGOTO_ERROR(H5E_IO, H5E_CLOSEERROR, FAIL, "Journal file close failed.")
    } /* end if */

    if ( HDremove(struct_ptr->jname) < 0) {

        HGOTO_ERROR(H5E_IO, H5E_REMOVEFAIL, FAIL, \
		    "Journal file close failed.")
    } /* end if */

    /* Free all memory associated with struct_ptr */

    if ( struct_ptr->jname != NULL ) {

        struct_ptr->jname = (char *)H5MM_xfree(struct_ptr->jname);

        if ( struct_ptr->jname != NULL ) {

            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "free of jname failed.");
        }
    }

    if ( struct_ptr->hdf5_file_name != NULL ) {

        struct_ptr->hdf5_file_name = 
		(char *)H5MM_xfree(struct_ptr->hdf5_file_name);

        if ( struct_ptr->hdf5_file_name != NULL ) {

            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "free of hdf5_file_name failed.");
        }
    }

    if ( (*struct_ptr->buf)[0] != NULL ) {

        (*struct_ptr->buf)[0] = (char *)H5MM_xfree((*struct_ptr->buf)[0]);
        if ( (*struct_ptr->buf)[0] != NULL ) {

            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "free of buffers failed.");
        }
    }

    if ( struct_ptr->buf != NULL ) {

        struct_ptr->buf = (char *((*)[]))H5MM_xfree(struct_ptr->buf);
        if ( struct_ptr->buf != NULL ) {

            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "free of buffer pointer array failed.");
        }
    }

    if ( struct_ptr->trans_tracking != NULL ) {

        struct_ptr->trans_tracking = 
		(uint64_t (*)[])H5MM_xfree(struct_ptr->trans_tracking);

        if ( struct_ptr->trans_tracking != NULL ) {

            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "free of transaction tracking array failed.");
        }
    }

    if ( struct_ptr->aio_ctl_blks != NULL ) {

        struct_ptr->aio_ctl_blks = 
		(struct aiocb (*)[])H5MM_xfree(struct_ptr->aio_ctl_blks);

        if ( struct_ptr->aio_ctl_blks != NULL ) {

            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "free of AIO control blocks array failed.");
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C_jb__takedown */


/******************************************************************************
 *
 * Function:		H5C_jb__trunc
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Thursday, February 7, 2008
 *
 * Purpose:		Verify that there is no transaction in progress, and 
 *			that the journal entry buffers are empty. Truncate
 *			the journal file. Does not return until the file
 *			is truncated on disk.
 *
 * Returns:		SUCCEED on success.
 *
 * Changes:		Added code to reset fields used exclusively by 
 *			AIO.
 *						JRM -- 1/12/10
 *
 ******************************************************************************/

herr_t 
H5C_jb__trunc(H5C_jbrb_t * struct_ptr)

{
    int i;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C_jb__trunc, FAIL)

    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC);
	
    /* Verify that there is no transaction in progress */
    if ( struct_ptr->trans_in_prog != FALSE ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
	     "Attempt to truncate journal file while transaction in progress.")
    } /* end if */

    /* Verify that the journal buffers are empty */
    if ( struct_ptr->bufs_in_use != 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
	            "Attempt to truncate with non-empty buffers.")
    } /* end if */	

    /* Truncate the journal file */
    if ( HDftruncate(struct_ptr->journal_file_fd, (off_t)0) < 0 ) {

        HGOTO_ERROR(H5E_IO, H5E_SYNCFAIL, FAIL, "Jounal file truncate failed.")
    } /* end if */

    /* Start back to top of journal buffer and journal file */
    struct_ptr->header_present = FALSE;
    struct_ptr->journal_is_empty = TRUE;
    struct_ptr->aio_next_buf_offset = 0;

    /* reset the transaction number fields */
    struct_ptr->cur_trans = 0;
    struct_ptr->last_trans_queued = 0;
    struct_ptr->last_trans_written = 0;
    struct_ptr->last_trans_on_disk = 0;
	
    /* reset the transaction tracking array */
    for (i=0; i<struct_ptr->num_bufs; i++)
    {
	(*struct_ptr->trans_tracking)[i] = 0;
    }

    if ( HDlseek(struct_ptr->journal_file_fd, (off_t)0, SEEK_SET) == (off_t)-1 )
    {
        HGOTO_ERROR(H5E_IO, H5E_SEEKERROR, FAIL, "Jounal file seek failed.")
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C_jb__trunc */


/******************************************************************************
 *
 * Function:		H5C_jb__write_header_entry
 *
 * Programmer:		John Mainzer
 *			2/12/09
 *
 * Purpose:		Write the header message to the journal file.
 * 
 *			This message appear exactly once in every journal
 *			file, and is always the first message in the file.
 *			It identifies the journal file, and contains 
 *			information required to run the journal, should 
 *			that be necessary.
 *
 *			It is always in human readable format.
 *			
 * Returns:		SUCCEED on success.
 *			FAIL on failure.
 *
 * Changes:		JRM -- 3/21/09
 *                      Moved the entry tag strings into #defines.  
 *			Replaced all white space in the creation date 
 *			string with underscores.
 *
 *			JRM -- 4/16/09
 *			Updated function to include the offset and length
 *			sizes in the header if human_readable is FALSE.
 *
 ******************************************************************************/

herr_t 
H5C_jb__write_header_entry(H5C_jbrb_t * struct_ptr)

{
    herr_t      ret_value = SUCCEED;
    char 	*buf;
    char      * p;
    char	time_buf[32];
    int		chars_written;
    int         i;
    size_t      file_name_len;
    size_t	buf_len;
    time_t      current_date;
	
    FUNC_ENTER_NOAPI(H5C_jb__write_header_entry, FAIL)

    /* Check Arguments */
    HDassert( struct_ptr );
    HDassert( struct_ptr->magic == H5C__H5C_JBRB_T_MAGIC );
    HDassert( struct_ptr->hdf5_file_name != NULL );
    HDassert( struct_ptr->header_present == FALSE );
    HDassert( struct_ptr->journal_is_empty == TRUE );

    file_name_len = HDstrlen(struct_ptr->hdf5_file_name);

    HDassert( file_name_len > 0 );

    buf_len = file_name_len + 256;
	
    /* Allocate space for journal buffers */
    buf = (char *)H5MM_malloc(buf_len);

    if ( buf == NULL ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                    "buffer allocation failed.");
    } /* end if */
	
    /* Get the current date */
    current_date = HDtime(NULL);

    /* load ascii representation of current_date into time_buf[],
     * replacing white space with underscores.
     */
    time_buf[31] = '\0'; /* just to be safe */

    if ( (p = HDctime(&current_date)) == NULL ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "Can't get ascii representation of current date.")

    } else {

        /* copy the string into time_buf, replacing white space with 
         * underscores.
         *
         * Do this to make parsing the header easier.
         */
        i = 0;

        while ( ( i < 31 ) && ( *p != '\0' ) ) {

            if ( isspace(*p) ) {

                time_buf[i] = '_';

            } else {

                time_buf[i] = *p;
            }

            i++;
            p++;
        }

        time_buf[i] = '\0';
    }

    /* Format the header message in the temporary buffer */

    if ( struct_ptr->human_readable ) {

        chars_written = 
            HDsnprintf(buf, 
                       buf_len - 1,
                       "0 %s %ld %s %s %s %d %s %10.10s %s %d\n",
                       H5C_JNL__VER_NUM_TAG,
	               struct_ptr->jvers, 
                       H5C_JNL__TGT_FILE_NAME_TAG,
	               struct_ptr->hdf5_file_name, 
                       H5C_JNL__JNL_MAGIC_TAG,
                       (int)(struct_ptr->journal_magic),
                       H5C_JNL__CREATION_DATE_TAG,
	               time_buf,
                       H5C_JNL__HUMAN_READABLE_TAG,
	               struct_ptr->human_readable);

    } else {

        /* Only include the offset and length widths in header for a binary 
         * journal file.  Leave this data out of the human readable journal 
         * file header because:
         *
         *	1) Everything is in ASCII so it isn't needed, and 
         *
         *	2) If we included it anyway, we would have to update the 
         *	   tests for the human readable journal file code.
         */

        chars_written = 
            HDsnprintf(buf, 
                       buf_len - 1,
                       "0 %s %ld %s %s %s %d %s %10.10s %s %d %s %d %s %d\n",
                       H5C_JNL__VER_NUM_TAG,
	               struct_ptr->jvers, 
                       H5C_JNL__TGT_FILE_NAME_TAG,
	               struct_ptr->hdf5_file_name, 
                       H5C_JNL__JNL_MAGIC_TAG,
                       (int)(struct_ptr->journal_magic),
                       H5C_JNL__CREATION_DATE_TAG,
	               time_buf,
                       H5C_JNL__HUMAN_READABLE_TAG,
	               struct_ptr->human_readable,
                       H5C_JNL__OFFSET_WIDTH_TAG,
                       struct_ptr->offset_width,
                       H5C_JNL__LENGTH_WIDTH_TAG,
                       struct_ptr->length_width);

    }

    if ( chars_written >= (int)(buf_len - 1) ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_CANTCOPY, FAIL, \
                    "tried to overwrite buffer.");
    }

    HDassert( HDstrlen(buf) < buf_len );

    /* Write the header message into the ring buffer */
    if ( H5C_jb__write_to_buffer(struct_ptr, HDstrlen(buf), buf, FALSE, 
			          (uint64_t)0) < 0) {
#if 1 /* JRM */
    HDfprintf(stdout, "%s: H5C_jb__write_to_buffer() failed.\n", FUNC);
#endif /* JRM */
        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C_jb__write_to_buffer() failed.\n")
    } /* end if */

    /* Update boolean flags */
    struct_ptr->header_present = TRUE;
    struct_ptr->journal_is_empty = FALSE;

done:

    if ( buf != NULL ) {

        buf = (char *)H5MM_xfree(buf);

        if ( buf != NULL ) {

            HDONE_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "free of buf failed.")
        }
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C_jb__write_header_entry() */


/******************************************************************************
 *
 * Function:		H5C_jb__write_to_buffer
 *
 * Programmer:		John Mainzer
 *			1/14/10
 *
 * Purpose:		Test to see whether AIO is enabled, and the 
 *			call the appropriate version of the function.
 *
 *			At some point we may wish to replace this switch
 *			function with a function pointer in struct H5C_jbrb_t.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

herr_t 
H5C_jb__write_to_buffer(H5C_jbrb_t * struct_ptr,	
			 size_t size,			
			 const char * data,
                         hbool_t is_end_trans,
                         uint64_t trans_num)
{
    herr_t ret_value = SUCCEED;
	
    FUNC_ENTER_NOAPI(H5C_jb__write_to_buffer, FAIL)

    /* Check Arguments */
    if ( ( struct_ptr == NULL ) ||
         ( struct_ptr->magic != H5C__H5C_JBRB_T_MAGIC ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
	     "bad struct_ptr on entry.")
    }

    if ( struct_ptr->use_aio ) {

        ret_value = H5C_jb_aio__write_to_buffer(struct_ptr,
                                                 size,
                                                 data,
                                                 is_end_trans,
                                                 trans_num);

    } else {

        ret_value = H5C_jb_sio__write_to_buffer(struct_ptr,
                                                 size,
                                                 data,
                                                 is_end_trans,
                                                 trans_num);

    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C_jb__write_to_buffer */

