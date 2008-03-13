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
 * Created:     H5C2journal.c
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

#define H5C2_PACKAGE            /*suppress error about including H5C2pkg  */

#include "H5private.h"          /* Generic Functions                    */
#include "H5Eprivate.h"         /* Error handling                       */
#include "H5MMprivate.h"        /* Memory management                    */
#include "H5C2pkg.h"            /* Cache                                */


/**************************************************************************/
/********************** journal file management code **********************/
/**************************************************************************/

/******************************************************************************
 *
 * Function:		H5C2_jb__flush_full_buffers
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
 ******************************************************************************/

herr_t 
H5C2_jb__flush_full_buffers(H5C2_jbrb_t * struct_ptr)	
{
    int result;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C2_jb__flush_full_buffers, FAIL)
	
    /* this asserts that at least one buffer is in use */
    HDassert(struct_ptr->bufs_in_use > 0);
    /* write an assert to verify that at least one buffer is full */
    HDassert( (struct_ptr->put != struct_ptr->get) ||
              (struct_ptr->rb_free_space == 0)      );

    /* flush all full, dirtied journal buffers to disk */
    if (struct_ptr->get < struct_ptr->put) {

	/* can write solid chunk from get up to, but not 
	 * including, put 
	 */
	HDwrite(struct_ptr->journal_file_fd, 
	        (*struct_ptr->buf)[struct_ptr->get], 
	        (struct_ptr->put - struct_ptr->get) * struct_ptr->buf_size);

	struct_ptr->bufs_in_use -= (struct_ptr->put - struct_ptr->get);
        struct_ptr->rb_free_space += (struct_ptr->put - struct_ptr->get) * struct_ptr->buf_size;

    } /* end if */

    else {

	/* write from get through end of buffer */
	result = HDwrite(struct_ptr->journal_file_fd, 
	      (*struct_ptr->buf)[struct_ptr->get], 
	      (struct_ptr->num_bufs - struct_ptr->get) * struct_ptr->buf_size);

	if ( result == -1 ) {

            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
		        "Journal file write failed.")
        }

	struct_ptr->bufs_in_use -= (struct_ptr->num_bufs - struct_ptr->get);
        struct_ptr->rb_free_space += (struct_ptr->num_bufs - struct_ptr->get) * struct_ptr->buf_size;

        /* if put = 0, then everything that needs to be flushed will have been
           flushed, so we can stop here. Otherwise, need to flush all buffers
           from the start of the ring buffer's allocated space up to, but not
           including, the buffer indexed by put. */
        if (struct_ptr->put != 0) {

            result = HDwrite(struct_ptr->journal_file_fd, 
                             (*struct_ptr->buf)[0], 
                             (struct_ptr->put) * struct_ptr->buf_size);

	    if ( result == -1 ) {

                HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
		            "Journal file write failed.")
            } /* end if */

        struct_ptr->rb_free_space += (struct_ptr->put * struct_ptr->buf_size);

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

} /* end H5C2_jb__flush_full_buffers */


/******************************************************************************
 *
 * Function:		H5C2_jb__flush
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
 ******************************************************************************/

herr_t 
H5C2_jb__flush(H5C2_jbrb_t * struct_ptr)
{
    int result;
    int i;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C2_jb__flush, FAIL)

    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(struct_ptr->magic == H5C2__H5C2_JBRB_T_MAGIC);
	
    /* Check if transaction is in progress */

    if (struct_ptr->trans_in_prog != FALSE) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "Attempt to flush buffers with transaction in progress.")
    } /* end if */

    if (struct_ptr->get > struct_ptr->put) {

	/* write from get through end of buffer */
	result = HDwrite(struct_ptr->journal_file_fd, 
	      (*struct_ptr->buf)[struct_ptr->get], 
	      (struct_ptr->num_bufs - struct_ptr->get) * struct_ptr->buf_size);

	if ( result == -1 ) {

            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
		        "Journal file write failed.")
        }
        
	struct_ptr->bufs_in_use -= (struct_ptr->num_bufs - struct_ptr->get);
        struct_ptr->rb_free_space += (struct_ptr->num_bufs - struct_ptr->get) * struct_ptr->buf_size;
        struct_ptr->get = 0;
        
    } /* end if */

    if (struct_ptr->get < struct_ptr->put) {

	/* write from get up to, but not including, put */
	result = HDwrite(struct_ptr->journal_file_fd, 
	            (*struct_ptr->buf)[struct_ptr->get], 
	            (struct_ptr->put - struct_ptr->get) * struct_ptr->buf_size);

	if ( result == -1 ) {

            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
		        "Journal file write failed.")
        }

	struct_ptr->bufs_in_use -= (struct_ptr->put - struct_ptr->get);
        struct_ptr->rb_free_space += (struct_ptr->put - struct_ptr->get) * struct_ptr->buf_size;
        struct_ptr->get = struct_ptr->put;

    } /* end if */

    if ( struct_ptr->cur_buf_free_space != struct_ptr->buf_size ) {

        /* flush partially filled portion of current journal buffer to disk */
	result = HDwrite(struct_ptr->journal_file_fd, 
	               (*struct_ptr->buf)[struct_ptr->put], 
	               struct_ptr->buf_size - struct_ptr->cur_buf_free_space);

	if ( result == -1 ) {

            HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, \
		        "Journal file write failed.")
        }

	struct_ptr->bufs_in_use--;
        struct_ptr->rb_free_space += (struct_ptr->buf_size - struct_ptr->cur_buf_free_space);

    } /* end if */

    HDassert(struct_ptr->bufs_in_use == 0);
    HDassert(struct_ptr->rb_free_space == struct_ptr->num_bufs * struct_ptr->buf_size);

    /* perform sync to ensure everything gets to disk before returning */
    /* Note: there is no HDfsync function, so for now, the standard
       fsync is being used. */
    if ( fsync(struct_ptr->journal_file_fd) < 0 ) {

        HGOTO_ERROR(H5E_IO, H5E_SYNCFAIL, FAIL, "Journal file sync failed.")
    } /* end if */

    /* record last transaction number that made it to disk */
	struct_ptr->last_trans_on_disk = 
		(*struct_ptr->trans_tracking)[struct_ptr->put];

    /* MIKE: optimization note: don't reset to top of ring buffer. 
     * instead, keep filling out current buffer so we can keep writes 
     * on block boundaries. 
     */
    struct_ptr->cur_buf_free_space = struct_ptr->buf_size;
    struct_ptr->rb_space_to_rollover = struct_ptr->num_bufs * struct_ptr->buf_size;
    struct_ptr->head = (*struct_ptr->buf)[0];
    struct_ptr->put = 0;

    /* Propogate the last transaction on in the buffers throughout the 
     * transaction tracking array. */
    for (i=0; i<struct_ptr->num_bufs; i++)
    {
	(*struct_ptr->trans_tracking)[i] = struct_ptr->last_trans_on_disk;
    }

    /* update get index */
    struct_ptr->get = struct_ptr->put;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C2_jb__flush */


/******************************************************************************
 *
 * Function:		H5C2_jb__write_to_buffer
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
 ******************************************************************************/

herr_t 
H5C2_jb__write_to_buffer(H5C2_jbrb_t * struct_ptr,	
			size_t size,			
			const char * data,
                        hbool_t is_end_trans,
                        unsigned long trans_num)
{
    herr_t ret_value = SUCCEED;
    unsigned long track_last_trans = 0;
    int oldput = 0;
    int i;
	
    FUNC_ENTER_NOAPI(H5C2_jb__write_to_buffer, FAIL)

    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(data);
    HDassert(struct_ptr->magic == H5C2__H5C2_JBRB_T_MAGIC);
    HDassert(HDstrlen(data) == size);
    HDassert(struct_ptr->rb_space_to_rollover <= 
		    struct_ptr->num_bufs * struct_ptr->buf_size);
    HDassert(struct_ptr->rb_space_to_rollover > 0); 

    /* If the data size exceeds the bounds of the ring buffer's allocated 
     * memory, loop around to top 
     */
    if (size >= struct_ptr->rb_space_to_rollover) {

	while (size >= struct_ptr->rb_space_to_rollover) {
			
	    /* Assertions */
	    HDassert(size != 0);
	    HDassert(HDstrlen(data) >= struct_ptr->rb_space_to_rollover);

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
               then update it */
            if ((size == struct_ptr->rb_space_to_rollover) &&
                (is_end_trans == TRUE)) {
                
                (*struct_ptr->trans_tracking)[struct_ptr->num_bufs - 1] 
                                                    = trans_num;
                (*struct_ptr->trans_tracking)[0] = trans_num;
            }

	    /* flush buffers */
	    if ( H5C2_jb__flush_full_buffers(struct_ptr) < 0 ) {

		 HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                             "H5C2_jb__flush_full_buffers() failed.\n")
            }

	    /* update remaining size of data to be written */
	    size = size - struct_ptr->rb_space_to_rollover;

	    /* update the data pointer to point to the remaining data to be 
	     * written 
	     */
	    data = &data[struct_ptr->rb_space_to_rollover];

	    /* update the amount of space left at end of ring buffer */
	    struct_ptr->rb_space_to_rollover = 
		    struct_ptr->buf_size * struct_ptr->num_bufs;

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
	struct_ptr->put += 
            (size-struct_ptr->cur_buf_free_space)/(struct_ptr->buf_size) + 1; 

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
            (struct_ptr->num_bufs - (struct_ptr->put + 1)) * 
            (struct_ptr->buf_size );

	/* update bufs_in_use as necessary */
	struct_ptr->bufs_in_use = struct_ptr->put - struct_ptr->get;
	if (struct_ptr->cur_buf_free_space < struct_ptr->buf_size) {

	    struct_ptr->bufs_in_use++;
        }

        /* check to see if trans_tracking needs to be updated. If so,
           then update it */
        if (is_end_trans == TRUE) {
                
            if (struct_ptr->cur_buf_free_space == struct_ptr->buf_size) {
                (*struct_ptr->trans_tracking)[struct_ptr->put - 1] = trans_num;
            }
            else {
                (*struct_ptr->trans_tracking)[struct_ptr->put] = trans_num;
            }

        } /* end if */

	/* flush buffers */
	if ( H5C2_jb__flush_full_buffers(struct_ptr) < 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C2_jb__flush_full_buffers() failed.\n")
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
                (*struct_ptr->trans_tracking)[struct_ptr->put] 
                 = (*struct_ptr->trans_tracking)[struct_ptr->put - 1];

	    } /* end if */
                
            else {

		struct_ptr->put = 0;

                /* Drag trans_tracking value into next buffer */
                (*struct_ptr->trans_tracking)[0] 
                 = (*struct_ptr->trans_tracking)[struct_ptr->num_bufs - 1];

                /* reset head pointer and free space values */
		struct_ptr->head = (*struct_ptr->buf)[0];
		struct_ptr->rb_space_to_rollover = 
			struct_ptr->buf_size * struct_ptr->num_bufs;

	    } /* end else */

	    if ( H5C2_jb__flush_full_buffers(struct_ptr) < 0 ) {

                HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                            "H5C2_jb__flush_full_buffers() failed.\n")
            } /* end if */

	    struct_ptr->cur_buf_free_space = struct_ptr->buf_size;

	} /* end if */

    } /* end else */
	
    HDassert(struct_ptr->bufs_in_use <= struct_ptr->num_bufs);

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C2_jb__write_to_buffer */


/******************************************************************************
 *
 * Function:		H5C2_jb__init
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Tuesday, February 5, 2008
 *
 * Purpose:		Initialize the supplied instance of H5C2_jbrb_t as
 *			specified by the buf_size and num_bufs fields. Open the
 *			journal file whose name is supplied in journal_file_name
 *			for either synchronous or asynchronous I/O as specified
 * 			by use_aio.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

herr_t 
H5C2_jb__init(H5C2_jbrb_t * struct_ptr,  	
	      char * HDF5_file_name,	 	
	      char * journal_file_name, 	
	      size_t buf_size,		
	      int num_bufs,		 	
	      hbool_t use_aio,		
 	      hbool_t human_readable)
{
    char 	temp[150];
    int 	i;
    herr_t 	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C2_jb__init, FAIL)

    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(HDF5_file_name);
    HDassert(journal_file_name);
    HDassert(buf_size > 0);
    HDassert(num_bufs > 0);
    HDassert(struct_ptr->magic == H5C2__H5C2_JBRB_T_MAGIC);
	
    /* Open journal file */
    struct_ptr->journal_file_fd = 
	    open(journal_file_name, O_WRONLY|O_CREAT|O_EXCL, 0777);

    if ( struct_ptr->journal_file_fd  == -1) {

        HGOTO_ERROR(H5E_FILE, H5E_CANTCREATE, FAIL, \
                    "Can't create journal file.  Does it already exist?")
    } /* end if */

    /* Initialize Fields of H5C2_jbrb_t structure */
    struct_ptr->jname = journal_file_name;
    struct_ptr->hdf5_file_name = HDF5_file_name;
    struct_ptr->buf_size = buf_size;
    struct_ptr->num_bufs = num_bufs;
    struct_ptr->use_aio = use_aio;
    struct_ptr->human_readable = human_readable;
    struct_ptr->bufs_in_use = 0;
    struct_ptr->trans_in_prog = FALSE;
    struct_ptr->get = 0;
    struct_ptr->put = 0;
    struct_ptr->jvers = H5C2__JOURNAL_VERSION;
    struct_ptr->cur_trans = 0;
    struct_ptr->last_trans_on_disk = 0;
    struct_ptr->cur_buf_free_space = struct_ptr->buf_size;
    struct_ptr->rb_free_space = struct_ptr->num_bufs * struct_ptr->buf_size;
    struct_ptr->rb_space_to_rollover = struct_ptr->num_bufs * struct_ptr->buf_size;
    struct_ptr->jentry_written = FALSE;
    struct_ptr->journal_is_empty = TRUE;
	
    /* Allocate space for the ring buffer's journal buffer pointers */
    struct_ptr->buf = H5MM_malloc(struct_ptr->num_bufs * sizeof(char *));

    if ( struct_ptr->buf == NULL ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                    "allocation of buf pointer array failed.");
    } /* end if */
	
    /* Allocate space for journal buffers */
    (*struct_ptr->buf)[0] = 
            H5MM_malloc(struct_ptr->buf_size * struct_ptr->num_bufs);

    if ( (*struct_ptr->buf)[0] == NULL ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                    "allocation of buffers failed.");
    } /* end if */

    /* Allocate space for the purposes of tracking the last 
     * transaction on disk 
     */
    struct_ptr->trans_tracking = 
    	H5MM_malloc(struct_ptr->num_bufs * sizeof(unsigned long));

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
    for (i=1; i<struct_ptr->num_bufs; i++)
    {
	(*struct_ptr->buf)[i] = 
		&((*struct_ptr->buf)[0])[i * struct_ptr->buf_size];
    }

    /* Define head pointer to point at where we are writing to in the buffer */
    struct_ptr->head = (*struct_ptr->buf)[struct_ptr->put];
	
    /* Format the header message into a temporary buffer */
    HDsnprintf(temp, 
        (size_t)150,
	"0 ver_num %ld target_file_name %s creation_date %s human_readable %d\n",
	struct_ptr->jvers, 
	struct_ptr->hdf5_file_name, 
	__DATE__, 
	struct_ptr->human_readable);

    /* Write the header message into the ring buffer */
    if ( H5C2_jb__write_to_buffer(struct_ptr, HDstrlen(temp), temp, FALSE, 0) 
		    < 0) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C2_jb__write_to_buffer() failed.\n")
    } /* end if */

    /* Update boolean flags */
    struct_ptr->header_present = TRUE;
    struct_ptr->journal_is_empty = FALSE;

done:
	
    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C2_jb__init */


/******************************************************************************
 *
 * Function:		H5C2_jb__start_transaction
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Wednesday, February 6, 2008
 *
 * Purpose:		Verify that there is no transaction in progress, and
 *			that the supplied transaction number is next in
 *			sequence. Then construct a start transaction message, 
 *			and write it to the current journal buffer. Make note
 *			of the fact that the supplied transaction is in
 *			progress.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

herr_t 
H5C2_jb__start_transaction(H5C2_jbrb_t * struct_ptr,
			   unsigned long trans_num)

{
    char temp[150];
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C2_jb__start_transaction, FAIL)
	
    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(struct_ptr->magic == H5C2__H5C2_JBRB_T_MAGIC);
	
    /* Verify that there is no transaction in progress */
    if ( struct_ptr->trans_in_prog != FALSE ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "Transaction already in progress.")
    } /* end if */

    /* JRM: Heads up:  we may relax this constraint to rquire that the 
     *      new transaction number is greater than the old, but possibly
     *      not the next integer in sequence.  Will this cause problems
     *      with testing?
     */

    /* Verify that the supplied transaction number is next in sequence */
    if ( (struct_ptr->cur_trans + 1) != trans_num ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "New transaction out of sequence.")
    } /* end if */

    /* Verify that header message is present in journal file or ring buffer. 
     * If not, write it. 
     */
    if ( struct_ptr->header_present == FALSE ) {

	HDsnprintf(temp, 
	(size_t)150,
	"0 ver_num %ld target_file_name %s creation_date %s human_readable %d\n",
	    struct_ptr->jvers, 
	    struct_ptr->hdf5_file_name, 
	    __DATE__, 
	    struct_ptr->human_readable);

        if ( H5C2_jb__write_to_buffer(struct_ptr, HDstrlen(temp), temp, 
				      FALSE, trans_num) < 0 ) {

            HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                        "H5C2_jb__write_to_buffer() failed.\n")
        } /* end if */

	struct_ptr->header_present = 1;
	struct_ptr->journal_is_empty = 0;
    } /* end if */

    /* Write start transaction message */
    HDsnprintf(temp, (size_t)150, "1 bgn_trans %ld\n", trans_num);
    if ( H5C2_jb__write_to_buffer(struct_ptr, HDstrlen(temp), temp, 
			          FALSE, trans_num) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C2_jb__write_to_buffer() failed.\n")
    } /* end if */
		
    /* Make note of the fact that supplied transaction is in progress */
    struct_ptr->trans_in_prog = TRUE;
    struct_ptr->cur_trans = trans_num;

done:
	
    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C2_jb__start_transaction */


/******************************************************************************
 *
 * Function:		H5C2_jb__journal_entry
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
 ******************************************************************************/

herr_t 
H5C2_jb__journal_entry(H5C2_jbrb_t * struct_ptr,
			unsigned long trans_num,
			haddr_t base_addr,
			size_t length,
			const char * body)
{

    char * temp = NULL;
    char * hexdata = NULL;
    size_t hexlength;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C2_jb__journal_entry, FAIL)
	
    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(body);
    HDassert(struct_ptr->magic == H5C2__H5C2_JBRB_T_MAGIC);
	
    /* Verify that the supplied transaction is in progress */
    if ( ( struct_ptr->trans_in_prog != TRUE ) ||
         ( struct_ptr->cur_trans != trans_num ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, \
                    "Transaction not in progress or bad transaction number.")
    } /* end if */

    if ( (temp = H5MM_malloc(length + 100)) == NULL ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                   "allocation of assembly buffer failed.");
    }

    if ( (hexdata = H5MM_malloc(length * 40)) == NULL ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                   "allocation of assembly buffer failed.");
    }

    /* Write journal entry */
    HDsnprintf(temp, 
               (size_t)(length + 100),
               "2 trans_num %ld length %d base_addr 0x%lx body ", 
 	       trans_num, 
	       length, 
	       (unsigned long)base_addr); /* <== fix this */

    if ( H5C2_jb__write_to_buffer(struct_ptr, HDstrlen(temp), temp, FALSE, trans_num) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C2_jb__write_to_buffer() failed.\n")
    } /* end if */

    /* Convert data from binary to hex */
    H5C2_jb__bin2hex(body, hexdata, &hexlength, 0, length);

    if ( H5C2_jb__write_to_buffer(struct_ptr, hexlength, hexdata, FALSE, trans_num) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C2_jb__write_to_buffer() failed.\n")
    } /* end if */

    /* Indicate that at least one journal entry has been written under 
     * this transaction 
     */
    if ( struct_ptr->jentry_written == FALSE ) {

	struct_ptr->jentry_written = TRUE;
    }

done:

    if ( temp != NULL )
    {
        temp = H5MM_xfree(temp);
        if ( temp != NULL ) {

            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "free of assembly buffer failed.");
        }
    }

    if ( hexdata != NULL )
    {
        hexdata = H5MM_xfree(hexdata);
        if ( hexdata != NULL ) {

            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "free of assembly buffer failed.");
        }
    }

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C2_jb__journal_entry */


/*****************************************************************************
 *
 * Function:		H5C2_jb__end_transaction
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
 *****************************************************************************/
herr_t
H5C2_jb__end_transaction(H5C2_jbrb_t * struct_ptr,
			 unsigned long trans_num)
{
    char temp[25];
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C2_jb__end_transaction, FAIL)
	
    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(struct_ptr->magic == H5C2__H5C2_JBRB_T_MAGIC);
	
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
    HDsnprintf(temp, (size_t)25, "3 end_trans %ld\n", trans_num);

    /* Write end transaction message */
    if ( H5C2_jb__write_to_buffer(struct_ptr, HDstrlen(temp), temp, 
			          TRUE, trans_num ) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C2_jb__write_to_buffer() failed.\n")
    } /* end if */

    /* reset boolean flag indicating if at least one journal entry has 
     * been written under transaction 
     */
    struct_ptr->jentry_written = FALSE;

    /* Close current transaction */
    struct_ptr->trans_in_prog = FALSE;

done:
	
    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C2_jb__end_transaction */


/******************************************************************************
 *
 * Function:		H5C2_jb__comment
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
 ******************************************************************************/

herr_t 
H5C2_jb__comment(H5C2_jbrb_t * struct_ptr,
		 char * comment_ptr)
{
    char * temp = NULL;
    size_t temp_len;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C2_jb__comment, FAIL)
	
    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(comment_ptr);
    HDassert(struct_ptr->magic == H5C2__H5C2_JBRB_T_MAGIC);

    temp_len = HDstrlen(comment_ptr) + 11;
    if ( ( temp = H5MM_malloc(HDstrlen(comment_ptr) + 11) ) == NULL ) {

	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, \
                   "allocation of temp buffer failed.");
    } /* end if */

    /* Write comment message */
    HDsnprintf(temp, temp_len, "C comment %s", comment_ptr);

    if ( H5C2_jb__write_to_buffer(struct_ptr, HDstrlen(temp), temp, FALSE, struct_ptr->cur_trans ) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C2_jb__write_to_buffer() failed.\n")
    } /* end if */

    if ( H5C2_jb__write_to_buffer(struct_ptr, 1, "\n", FALSE, struct_ptr->cur_trans ) < 0 ) {

        HGOTO_ERROR(H5E_CACHE, H5E_CANTJOURNAL, FAIL, \
                    "H5C2_jb__write_to_buffer() failed.\n")
    } /* end if */

done:

    if ( temp != NULL ) {

        temp = H5MM_xfree(temp);
        if ( temp != NULL ) {

            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "free of assembly buffer failed.");
        }
    }

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C2_jb__comment */


/******************************************************************************
 *
 * Function:		H5C2_jb__get_last_transaction_on_disk
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
 ******************************************************************************/

herr_t 
H5C2_jb__get_last_transaction_on_disk(H5C2_jbrb_t * struct_ptr,
				      unsigned long * trans_num_ptr)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C2_jb__get_last_transaction_on_disk, FAIL)
	
    /* Check Arguments */
    HDassert( trans_num_ptr != NULL );

    /* This should really be an assert, but the func enter/exit 
     * macros get testy if there isn't at least one goto error 
     * macro call in the funtion.
     */
    if ( ( struct_ptr == NULL ) ||
         ( struct_ptr->magic != H5C2__H5C2_JBRB_T_MAGIC ) ) {

        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "bad struct_ptr.")
    }

    /* JRM: In machine readable version, lets check to see if a sync is 
     *      necessary, and call it only if it is.
     */
    /* perform a sync to ensure everything gets to disk before continuing */
    /* Note: there is no HDfsync function, so for now, the standard
       fsync is being used. */
    if ( fsync(struct_ptr->journal_file_fd) < 0 ) {

        HGOTO_ERROR(H5E_IO, H5E_SYNCFAIL, FAIL, "Jounal file sync failed.")

    } /* end if */

    * trans_num_ptr = struct_ptr->last_trans_on_disk;

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C2_jb__get_last_transaction_on_disk */


/******************************************************************************
 *
 * Function:		H5C2_jb__trunc
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
 ******************************************************************************/

herr_t 
H5C2_jb__trunc(H5C2_jbrb_t * struct_ptr)

{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5C2_jb__trunc, FAIL)

    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(struct_ptr->magic == H5C2__H5C2_JBRB_T_MAGIC);
	
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

    if ( HDlseek(struct_ptr->journal_file_fd, (off_t)0, SEEK_SET) == (off_t)-1 )
    {
        HGOTO_ERROR(H5E_IO, H5E_SEEKERROR, FAIL, "Jounal file seek failed.")
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C2_jb__trunc */


/******************************************************************************
 *
 * Function:		H5C2_jb__takedown
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
 ******************************************************************************/

herr_t 
H5C2_jb__takedown(H5C2_jbrb_t * struct_ptr)

{
    herr_t ret_value = SUCCEED;
	
    FUNC_ENTER_NOAPI(H5C2_jb__takedown, FAIL)

    /* Check Arguments */
    HDassert(struct_ptr);
    HDassert(struct_ptr->magic == H5C2__H5C2_JBRB_T_MAGIC);
	
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

        HGOTO_ERROR(H5E_IO, H5E_CLOSEERROR, FAIL, "Jounal file close failed.")
    } /* end if */

    if (remove(struct_ptr->jname) < 0) {

        HGOTO_ERROR(H5E_IO, H5E_REMOVEFAIL, FAIL, "Jounal file close failed.")
    } /* end if */

    /* Free all memory associated with struct_ptr */

    if ( (*struct_ptr->buf)[0] != NULL ) {

        (*struct_ptr->buf)[0] = H5MM_xfree((*struct_ptr->buf)[0]);
        if ( (*struct_ptr->buf)[0] != NULL ) {

            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "free of buffers failed.");
        }
    }

    if ( struct_ptr->buf != NULL ) {

        struct_ptr->buf = H5MM_xfree(struct_ptr->buf);
        if ( struct_ptr->buf != NULL ) {

            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "free of buffer pointer array failed.");
        }
    }

    if ( struct_ptr->trans_tracking != NULL ) {

        struct_ptr->trans_tracking = H5MM_xfree(struct_ptr->trans_tracking);
        if ( struct_ptr->trans_tracking != NULL ) {

            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, \
                        "free of transaction tracking array failed.");
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C2_jb__takedown */


/******************************************************************************
 *
 * Function:		H5C2_jb__reconfigure
 *
 * Programmer:		Mike McGreevy <mcgreevy@hdfgroup.org>
 *			Wednesday, February 6, 2008
 *
 * Purpose:		Re-configure the specified journal buffer ring buffer
 *			to use the supplied parameters.
 *
 * Returns:		SUCCEED on success.
 *
 ******************************************************************************/

herr_t 
H5C2_jb__reconfigure(H5C2_jbrb_t * struct_ptr,
	             size_t new_buf_size,
                     int new_num_bufs,
                     hbool_t new_use_aio)
{
#if 0 /* body commented out pending implementation */
    herr_t ret_value = SUCCEED;
	
    FUNC_ENTER_NOAPI(H5C2_jb__reconfigure, FAIL)

		/* code */
		/* code */
		/* code */

done:

    FUNC_LEAVE_NOAPI(ret_value)
#else
    return FAIL;
#endif
} /* end H5C2_jb__reconfigure */


/******************************************************************************
 *
 * Function:		H5C2_jb__bin2hex
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
H5C2_jb__bin2hex(uint8_t * buf, 
                 uint8_t * hexdata,
                 size_t * hexlength,
                 size_t buf_offset, 
                 size_t buf_size)

{

    herr_t ret_value = SUCCEED;
    size_t      u, v;                   /* Local index variable */
    uint8_t        c;
	
    FUNC_ENTER_NOAPI(H5C2_jb__bin2hex, FAIL)

    HDsnprintf(hexdata, (size_t)2, " ");

    for(u = 0; u < buf_size; u += 16) {

        /* Print the hex values */
        for(v = 0; v < 16; v++) {

            if(u + v < buf_size) {

                c = buf[buf_offset + u + v];
                sprintf(hexdata, "%s%02x ", hexdata, c);

            } /* end if */

        } /* end for */

    } /* end for */
    
    sprintf(hexdata, "%s\n", hexdata);

    * hexlength = HDstrlen(hexdata);

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5C2_jb__bin2hex*/

