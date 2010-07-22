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
 * Created:		H5FDint.c
 *			Jan 17 2008
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Internal routine for VFD operations
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5FD_PACKAGE		/*suppress error about including H5FDpkg  */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5FD_int_init_interface


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fprivate.h"         /* File access				*/
#include "H5FDpkg.h"		/* File Drivers				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Pprivate.h"		/* Property lists			*/


/****************/
/* Local Macros */
/****************/


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

char * dummy_aio_ctlblk = "Dummy AIO Control Block";



/*--------------------------------------------------------------------------
NAME
   H5FD_int_init_interface -- Initialize interface-specific information
USAGE
    herr_t H5FD_int_init_interface()

RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.  (Just calls
    H5FD_init_iterface currently).

--------------------------------------------------------------------------*/
static herr_t
H5FD_int_init_interface(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FD_int_init_interface)

    FUNC_LEAVE_NOAPI(H5FD_init())
} /* H5FD_int_init_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_read
 *
 * Purpose:	Private version of H5FDread()
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_read(H5FD_t *file, hid_t dxpl_id, H5FD_mem_t type, haddr_t addr,
    size_t size, void *buf/*out*/)
{
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5FD_read, FAIL)

    HDassert(file && file->cls);
    HDassert(H5I_GENPROP_LST == H5I_get_type(dxpl_id));
    HDassert(TRUE == H5P_isa_class(dxpl_id, H5P_DATASET_XFER));
    HDassert(buf);

#ifndef H5_HAVE_PARALLEL
    /* Do not return early for Parallel mode since the I/O could be a */
    /* collective transfer. */
    /* The no-op case */
    if(0 == size)
        HGOTO_DONE(SUCCEED)
#endif /* H5_HAVE_PARALLEL */

    /* Dispatch to driver */
    if((file->cls->read)(file, type, dxpl_id, addr + file->base_addr, size, buf) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, "driver read request failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_read() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_write
 *
 * Purpose:	Private version of H5FDwrite()
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_write(H5FD_t *file, hid_t dxpl_id, H5FD_mem_t type, haddr_t addr,
    size_t size, const void *buf)
{
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5FD_write, FAIL)

    HDassert(file && file->cls);
    HDassert(H5I_GENPROP_LST == H5I_get_type(dxpl_id));
    HDassert(TRUE == H5P_isa_class(dxpl_id, H5P_DATASET_XFER));
    HDassert(buf);

#ifndef H5_HAVE_PARALLEL
    /* Do not return early for Parallel mode since the I/O could be a */
    /* collective transfer. */
    /* The no-op case */
    if(0 == size)
        HGOTO_DONE(SUCCEED)
#endif /* H5_HAVE_PARALLEL */

    /* Dispatch to driver */
    if((file->cls->write)(file, type, dxpl_id, addr + file->base_addr, size, buf) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, "driver write request failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_write() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_set_eoa
 *
 * Purpose:	Private version of H5FDset_eoa()
 *
 *              This function expects the EOA is a RELATIVE address, i.e.
 *              relative to the base address.  This is NOT the same as the
 *              EOA stored in the superblock, which is an absolute
 *              address.  Object addresses are relative.
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative, no side effect
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FD_set_eoa(H5FD_t *file, H5FD_mem_t type, haddr_t addr)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5FD_set_eoa, FAIL)

    HDassert(file && file->cls);
    HDassert(H5F_addr_defined(addr) && addr <= file->maxaddr);

    /* Dispatch to driver, convert to absolute address */
    if((file->cls->set_eoa)(file, type, addr + file->base_addr) < 0)
	HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "driver set_eoa request failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_set_eoa() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_get_eoa
 *
 * Purpose:	Private version of H5FDget_eoa()
 *
 *              This function returns the EOA as a RELATIVE address, i.e.
 *              relative to the base address.  This is NOT the same as the
 *              EOA stored in the superblock, which is an absolute
 *              address.  Object addresses are relative.
 *
 * Return:	Success:	First byte after allocated memory.
 *		Failure:	HADDR_UNDEF
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 *-------------------------------------------------------------------------
 */
haddr_t
H5FD_get_eoa(const H5FD_t *file, H5FD_mem_t type)
{
    haddr_t	ret_value;

    FUNC_ENTER_NOAPI(H5FD_get_eoa, HADDR_UNDEF)

    HDassert(file && file->cls);

    /* Dispatch to driver */
    if(HADDR_UNDEF == (ret_value = (file->cls->get_eoa)(file, type)))
	HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, HADDR_UNDEF, "driver get_eoa request failed")

    /* Adjust for base address in file (convert to relative address) */
    ret_value -= file->base_addr;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_get_eoa() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_get_eof
 *
 * Purpose:	Private version of H5FDget_eof()
 *
 *              This function returns the EOF as a RELATIVE address, i.e.
 *              relative to the base address.  This will be different
 *              from  the end of the physical file if there is a user
 *              block.
 *
 * Return:	Success:	The EOF address.
 *
 *		Failure:	HADDR_UNDEF
 *
 * Programmer:	Robb Matzke
 *              Wednesday, August  4, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
haddr_t
H5FD_get_eof(const H5FD_t *file)
{
    haddr_t	ret_value;

    FUNC_ENTER_NOAPI(H5FD_get_eof, HADDR_UNDEF)

    HDassert(file && file->cls);

    /* Dispatch to driver */
    if(file->cls->get_eof) {
	if(HADDR_UNDEF == (ret_value = (file->cls->get_eof)(file)))
	    HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, HADDR_UNDEF, "driver get_eof request failed")
    } /* end if */
    else
	ret_value = file->maxaddr;

    /* Adjust for base address in file (convert to relative address)  */
    ret_value -= file->base_addr;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FD_get_eof() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_aio_read
 *
 * Purpose:     Initiate an asynchronous read from the indicated file of
 *              the specified number of bytes starting at the specified
 *              offset and loading the data  into the provided buffer.
 *
 *              The buffer must be large enough to contain the requested
 *              data, and is undefined and must not be read or modified
 *              until the read completes successfully.  Completion is
 *              determined via either a call to H5FD_aio_test() or a
 *              call to H5FD_aio_wait(), and success via a call to
 *              H5FD_aio_finish().
 *
 *              If successful, the H5FD_aio_read routine will return a 
 *		pointer to an internal control block in *ctlblk_ptr_ptr.  
 *		This pointer must be used in all subsequent 
 *		H5FD_aio_test() / H5FD_aio_wait() / H5FD_aio_finish() 
 *		calls referring to this request.
 *
 *              Note that a successful return from this function does not
 *              imply a successful read -- simply that no errors were
 *              immediately evident.  
 *
 * 		If AIO is supported by the underlying file driver, 
 *		simply pass the call on, and report the results.
 *
 *		If AIO is not supported by the underlying driver, set
 *		*ctlblk_ptr_ptr the address of a shared dummy control
 *		block, and do a synchronous read instead.  Return 
 *		SUCCEED or FAIL depending on the success of the 
 *		read.  
 *
 *		The pointer to the shared dummy control block 
 *		many be used in subsequent calls to H5FD_aio_test(),
 *		H5FD_aio_wait(), and H5FD_aio_finish(), which calls will
 *		report completion without error as appropriate as long
 *		as the underlying file driver does not support AIO.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	John Mainzer
 *              6/12/10
 *
 * Changes:	None.
 *
 *-------------------------------------------------------------------------
 */

herr_t 
H5FD_aio_read(H5FD_t *file, 
              H5FD_mem_t type, 
              hid_t dxpl,
              haddr_t addr, 
              size_t size, 
              void *buffer,
              void **ctlblk_ptr_ptr)
{
    herr_t      	ret_value = SUCCEED;       /* Return value */
    herr_t		result;

    FUNC_ENTER_NOAPI(H5FD_aio_read, FAIL)

    if ( ( file == NULL ) ||
         ( file->cls == NULL ) ||
         ( ! H5F_addr_defined(addr) ) ||
         ( size <= 0 ) ||
         ( buffer == NULL ) ||
         ( ctlblk_ptr_ptr == NULL ) ||
         ( *ctlblk_ptr_ptr != NULL ) ) {

        HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, "bad arg(s) on entry.")
    }

    if ( file->cls->aio_read != NULL ) { /* aio_read supported */

        /* if we have aio_read, verify that we also have aio_test, 
         * aio_wait, and aio_finish as well.
         */
        HDassert( file->cls->aio_test != NULL );
        HDassert( file->cls->aio_wait != NULL );
        HDassert( file->cls->aio_finish != NULL );

	result = (file->cls->aio_read)(file, type, dxpl, addr + file->base_addr,
                                       size, buffer, ctlblk_ptr_ptr);

	if ( result < 0 ) {

            HGOTO_ERROR(H5E_VFL, H5E_AIOREADERROR, FAIL, \
			"driver aio read request failed")
        }
    } else { /* aio_read not supported -- do synchronous read instead */

	HDassert( file->cls->read != NULL );

	result = (file->cls->read)(file, type, dxpl, addr + file->base_addr, 
                                   size, buffer);

        if ( result < 0 ) {

            HGOTO_ERROR(H5E_VFL, H5E_READERROR, FAIL, \
                        "driver fallback sio read request failed")

	} else { /* setup the dummy control block pointer */

            *ctlblk_ptr_ptr = (void *)dummy_aio_ctlblk;
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD_aio_read() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_aio_write
 *
 * Purpose:     Initiate an asynchronous write to the indicated file of
 *              the specified number of bytes from the supplied  buffer
 *              to the indicated location.
 *
 *              The buffer must not be discarded or modified until the
 *              write completes successfully.  Completion is determined
 *              via either H5FD_aio_test() or H5FD_aio_wait(), and 
 *		success via H5FD_aio_finish().
 *
 *              If successful, the H5FD_aio_write routine will return a 
 *		pointer to an internal control block in *ctlblk_ptr_ptr.  
 *		This pointer must be used in all subsequent H5FD_aio_test() 
 *		/ H5FD_aio_wait() / H5FD_aio_finish() calls referring to 
 *		this request.
 *
 *              Note that a successful return from this function does not
 *              imply a successful write -- simply that no errors were
 *              immediately evident.
 *
 * 		If AIO is supported by the underlying file driver, 
 *		simply pass the call on and report the results.
 *
 *		If AIO is not supported by the underlying driver, set
 *		*ctlblk_ptr_ptr the address of a shared dummy control
 *		block, and do a synchronous write instead.  Return 
 *		SUCCEED or FAIL depending on the success of the 
 *		write.  
 *
 *		The pointer to the shared dummy control block 
 *		pointer many be used in subsequent calls to H5FD_aio_test(),
 *		H5FD_aio_wait(), and H5FD_aio_finish(), which calls will
 *		report completion without error as appropriate as long
 *		as the underlying file driver does not support AIO.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	John Mainzer
 *              6/12/10
 *
 * Changes: 	None.
 *
 *-------------------------------------------------------------------------
 */

herr_t 
H5FD_aio_write(H5FD_t *file, 
               H5FD_mem_t type, 
               hid_t dxpl,
               haddr_t addr, 
               size_t size, 
               void *buffer,
               void **ctlblk_ptr_ptr)
{
    herr_t              ret_value = SUCCEED;       /* Return value */
    herr_t		result;

    FUNC_ENTER_NOAPI(H5FD_aio_write, FAIL)

    if ( ( file == NULL ) ||
         ( file->cls == NULL ) ||
         ( ! H5F_addr_defined(addr) ) ||
         ( size <= 0 ) ||
         ( buffer == NULL ) ||
         ( ctlblk_ptr_ptr == NULL ) ||
         ( *ctlblk_ptr_ptr != NULL ) ) {

        HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, "bad arg(s) on entry.")
    }

    if ( file->cls->aio_write != NULL ) { /* aio_write supported */

        /* if we have aio_write, verify that we also have aio_test, 
         * aio_wait, and aio_finish as well.
         */
        HDassert( file->cls->aio_test != NULL );
        HDassert( file->cls->aio_wait != NULL );
        HDassert( file->cls->aio_finish != NULL );

	result = (file->cls->aio_write)(file, type, dxpl, 
                                        addr + file->base_addr,
                                        size, buffer, ctlblk_ptr_ptr);

	if ( result < 0 ) {

            HGOTO_ERROR(H5E_VFL, H5E_AIOWRITEERROR, FAIL, \
			"driver aio write request failed")
        }
    } else { /* aio_write not supported -- do synchronous write instead */

	HDassert( file->cls->write != NULL );

	result = (file->cls->write)(file, type, dxpl, addr + file->base_addr, 
                                    size, buffer);

        if ( result < 0 ) {

            HGOTO_ERROR(H5E_VFL, H5E_WRITEERROR, FAIL, \
                        "driver fallback sio write request failed")

	} else { /* setup the dummy control block pointer */

            *ctlblk_ptr_ptr = (void *)dummy_aio_ctlblk;
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD_aio_write() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_aio_test
 *
 * Purpose:	This function is used to determine if the asynchronous
 *		operation associated with the supplied control block 
 *		pointer is complete.  If it is, *done_ptr should be set
 *		to TRUE, if it isn't, *done_ptr should be set to FALSE.
 *		In all cases, there function should return immediately.
 *
 *		Note that the return value only reflects errors in the 
 *		process of testing whether the operation is complete.
 *
 *		After the operation is complete, a call to 
 *		H5FD_aio_finish() must be made to determine whether 
 *		the operation completed successfully and to allow the 
 *		driver to tidy its data structures.
 *
 *		If the underlying file driver supports AIO, pass the 
 *		call on to the underlying file driver, and relay the 
 *		results back.
 *
 *		If the underlying file driver doesn't support AIO, 
 *		the operation being tested for was run synchronously,
 *		and is already complete.  Verify that the supplied 
 *		control block pointer points to the shared dummy 
 *		control block, set *done_ptr to TRUE and return.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	John Mainzer
 *              6/12/10
 *
 * Changes:	None.
 *
 *-------------------------------------------------------------------------
 */

herr_t 
H5FD_aio_test(H5FD_t *file, 
              hbool_t *done_ptr, 
              void *ctlblk_ptr)
{
    herr_t		ret_value = SUCCEED;  /* Return value */
    herr_t		result;

    FUNC_ENTER_NOAPI(H5FD_aio_test, FAIL)

    if ( ( file == NULL ) ||
         ( file->cls == NULL ) ||
         ( done_ptr == NULL ) ||
         ( ctlblk_ptr == NULL ) ) {

        HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, "bad arg(s) on entry.")
    }

    if ( file->cls->aio_test != NULL ) { /* aio_test supported */

	result = (file->cls->aio_test)(done_ptr, ctlblk_ptr);

	if ( result < 0 ) {

            HGOTO_ERROR(H5E_VFL, H5E_AIOTESTFAIL, FAIL, \
			"driver aio test request failed")
        }
    } else { /* aio_test not supported -- just fake it */

        if ( ctlblk_ptr != (void *)dummy_aio_ctlblk ) {

            HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                        "unexpected control block pointer.")
        }

        *done_ptr = TRUE;
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD_aio_test() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_aio_wait
 *
 * Purpose:	Wait until the asynchronous read, write, or fsync operation 
 *		indicated by *ctlblk_ptr has completed (successfully or 
 *		otherwise).
 *
 *		Note that the error code returned refers only to the 
 *		operation of waiting until read/write/fsync is
 *		complete -- Success does not imply that the read, write,
 *		or fsync operation completed successfully, only that
 *		no error was encountered while waiting for the operation 
 *		to finish.
 *
 *		If the underlying file driver supports AIO, pass the 
 *		call on to the underlying file driver, and relay the 
 *		results back.
 *
 *		If the underlying file driver doesn't support AIO, 
 *		the operation being tested for was run synchronously,
 *		and is already complete.  Verify that the supplied 
 *		control block pointer points to the shared dummy 
 *		control block, and return.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	John Mainzer
 *              6/12/10
 *
 * Changes:	None.
 *
 *-------------------------------------------------------------------------
 */

herr_t 
H5FD_aio_wait(H5FD_t *file,
              void *ctlblk_ptr)
{
    herr_t    		ret_value = SUCCEED;       /* Return value */
    herr_t		result;

    FUNC_ENTER_NOAPI(H5FD_aio_wait, FAIL)

    if ( ( file == NULL ) ||
         ( file->cls == NULL ) ||
         ( ctlblk_ptr == NULL ) ) {

        HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, "NULL ctlblk ptr on entry.")
    }

    if ( ctlblk_ptr == NULL ) {

        HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, "bad arg(s) on entry.")
    }

    if ( file->cls->aio_wait != NULL ) { /* aio_wait supported */

	result = (file->cls->aio_wait)(ctlblk_ptr);

	if ( result < 0 ) {

            HGOTO_ERROR(H5E_VFL, H5E_AIOWAITFAIL, FAIL, \
			"driver aio wait request failed")
        }
    } else { /* aio_wait not supported -- just fake it */

        if ( ctlblk_ptr != (void *)dummy_aio_ctlblk ) {

            HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                        "unexpected control block pointer.")
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD_aio_wait() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_aio_finish
 *
 * Purpose:	Determine whether the read, write, or fsync operation 
 *		indicated by *ctlblk_ptr completed successfully.  If it 
 *		did, set *errno_ptr to 0.  If if didn't, set *errno_ptr 
 *		to the appropriate error code.
 *
 *		Return SUCCEED if successful, and the appropriate error 
 *		code if not.
 *
 *		Note that the returned error code only refers to the 
 *		success or failure of the finish operation.  The caller 
 *		must examine *errno_ptr to determine if the underlying 
 *		asynchronous operation succeeded.
 *
 *		If the underlying file driver supports AIO, pass the 
 *		call on to the underlying file driver, and relay the 
 *		results back.
 *
 *		If the underlying file driver doesn't support AIO, 
 *		the operation being tested for was run synchronously,
 *		and has already completed successfully.  Verify that 
 *		the supplied control block pointer points to the 
 *		shared dummy control block, set *errno_ptr to zero,
 *		and return.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	John Mainzer
 *              6/12/10
 *
 *-------------------------------------------------------------------------
 */

herr_t 
H5FD_aio_finish(H5FD_t *file,
                int *errno_ptr, 
                void *ctlblk_ptr)
{
    herr_t              ret_value = SUCCEED;       /* Return value */
    herr_t		result;

    FUNC_ENTER_NOAPI(H5FD_aio_finish, FAIL)

    if ( ( file == NULL ) ||
         ( file->cls == NULL ) ||
         ( errno_ptr == NULL ) ||
         ( ctlblk_ptr == NULL ) ) {

        HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, "NULL param(s) on entry.")
    }

    if ( file->cls->aio_finish != NULL ) { /* aio_finish supported */

	result = (file->cls->aio_finish)(errno_ptr, ctlblk_ptr);

	if ( result < 0 ) {

            HGOTO_ERROR(H5E_VFL, H5E_AIOFINISHFAIL, FAIL, \
			"driver aio finish request failed")
        }
    } else { /* aio_finish not supported -- just fake it */

        if ( ctlblk_ptr != (void *)dummy_aio_ctlblk ) {

            HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                        "unexpected control block pointer.")
        }

	*errno_ptr = 0;
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD_aio_finish() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_aio_fsync
 *
 * Purpose:	Queue a sync of all asynchronous writes outstanding as of 
 *		the time this function is called.  Return SUCCEED if no 
 *		errors are encountered, but note that a good error return 
 *		from H5FD_aio_fsync() does not imply a successful 
 *		operation, only that no immediate errors were detected.
 *
 *		The sync is not known to be successful until reported 
 *		complete by either H5FD_aio_test or H5FD_aio_wait, 
 *		and reported successful by H5FD_aio_finish.
 *
 *		If the underlying file driver supports AIO, pass the the 
 *		call on to the underlying file driver, and relay the 
 *		results back.
 *
 *		If the underlying file driver doesn't support AIO, 
 *		call H5FD_fsync() do do a synchronous fsync, 
 *		set *ctlblk_ptr_ptr to point to the shared dummy 
 *		control block, and return.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	Negative
 *
 * Programmer:	John Mainzer
 *              6/12/10
 *
 *-------------------------------------------------------------------------
 */

herr_t 
H5FD_aio_fsync(H5FD_t *file, 
               void **ctlblk_ptr_ptr)
{
    herr_t     		ret_value = SUCCEED;       /* Return value */
    herr_t		result;

    FUNC_ENTER_NOAPI(H5FD_aio_fsync, FAIL)

    if ( ( file == NULL ) ||
         ( file->cls == NULL ) ||
         ( ctlblk_ptr_ptr == NULL ) || 
         ( *ctlblk_ptr_ptr != NULL ) ) {

        HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, "NULL param(s) on entry.")
    }

    if ( file->cls->aio_fsync != NULL ) { /* aio_fsync supported */

        /* if we have aio_fsync, verify that we also have aio_test, 
         * aio_wait, and aio_finish as well.
         */
        HDassert( file->cls->aio_test != NULL );
        HDassert( file->cls->aio_wait != NULL );
        HDassert( file->cls->aio_finish != NULL );

	result = (file->cls->aio_fsync)(file, ctlblk_ptr_ptr);

	if ( result < 0 ) {

            HGOTO_ERROR(H5E_VFL, H5E_AIOSYNCFAIL, FAIL, \
			"driver aio fsync request failed")
        }
    } else { /* aio_fsync not supported -- do synchronous fsync instead */

        result = H5FD_fsync(file, H5P_DATASET_XFER_DEFAULT);

        if ( result < 0 ) {

            HGOTO_ERROR(H5E_VFL, H5E_SYNCFAIL, FAIL, \
                        "driver fallback sio fsync request failed")

	} else { /* setup the dummy control block pointer */

            *ctlblk_ptr_ptr = (void *)dummy_aio_ctlblk;
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD_aio_fsync() */


/*-------------------------------------------------------------------------
 * Function:	H5FD_aio_cancel
 *
 * Purpose:	Attempt to cancel the asynchronous operation associated 
 *		with the control block pointed to by ctlblk_ptr.  
 *
 *		Note that this operation may have completed, but it is 
 *		an error if H5FD_aio_finish() has been called on it.
 *
 *		As part of the cancel, free the associated control blocks.
 *
 *		Return SUCCEED if successful, and the appropriate error 
 *		code otherwise.
 *
 *		If the underlying file driver supports AIO, pass the 
 *		call on to the underlying file driver, and relay the 
 *		results back.
 *
 *		If the underlying file driver doesn't support AIO, 
 *		the operation being tested for was run synchronously,
 *		and has already completed successfully.  Verify that 
 *		the supplied control block pointer points to the 
 *		shared dummy control block, and return.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	John Mainzer
 *              6/12/10
 *
 *-------------------------------------------------------------------------
 */

herr_t 
H5FD_aio_cancel(H5FD_t *file,
                void *ctlblk_ptr)
{
    herr_t   		ret_value = SUCCEED;       /* Return value */
    herr_t		result;

    FUNC_ENTER_NOAPI(H5FD_aio_cancel, FAIL)

    if ( ( file == NULL ) ||
         ( file->cls == NULL ) ||
         ( ctlblk_ptr == NULL ) ) {

        HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, "NULL ctlblk_ptr on entry.")
    }

    if ( file->cls->aio_cancel != NULL ) { /* aio_cancel supported */

	result = (file->cls->aio_cancel)(ctlblk_ptr);

	if ( result < 0 ) {

            HGOTO_ERROR(H5E_VFL, H5E_AIOCANCELFAIL, FAIL, \
			"driver aio cancel request failed")
        }
    } else { /* aio_cancel not supported -- just fake it */

        if ( ctlblk_ptr != (void *)dummy_aio_ctlblk ) {

            HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, \
                        "unexpected control block pointer.")
        }
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD_aio_cancel() */



/*-------------------------------------------------------------------------
 * Function:	H5FD_fsync
 *
 * Purpose:     Do an fsync on the indicated file.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	John Mainzer
 *              7/7/10
 *
 * Changes: 	None.
 *
 *-------------------------------------------------------------------------
 */

herr_t 
H5FD_fsync(H5FD_t *file, 
           hid_t dxpl)
{
    herr_t              ret_value = SUCCEED;       /* Return value */
    herr_t		result;

    FUNC_ENTER_NOAPI(H5FD_fsync, FAIL)

    if ( ( file == NULL ) ||
         ( file->cls == NULL ) ) {

        HGOTO_ERROR(H5E_ARGS, H5E_SYSTEM, FAIL, "bad file param on entry.")
    }

    if ( file->cls->fsync != NULL ) { /* fsync supported */

	result = (file->cls->fsync)(file, dxpl);

	if ( result < 0 ) {

            HGOTO_ERROR(H5E_VFL, H5E_SYNCFAIL, FAIL, \
                        "sio fsync request failed")
        }
    } else { /* fsync not supported */

	/* Quincey:
         *
 	 * We need to make a decision here, and its a bit above my pay
	 * grade.
	 *
 	 * On the one hand, there will be file drivers such as the core,
	 * and direct file drivers for which fsync makes no sense.
	 *
	 * On the other hand, in general I would think we want fsync 
	 * supported for the vast majority of cases.
	 *
	 * Thus the question is, do we want to require that the fsync
	 * call be supported in all cases -- albeit by an empty function 
	 * in cases like the  core file driver.
	 *
	 * If so, we should throw an error at this point.
	 * 
	 * If not, we should just do nothing quietly.
	 *
	 * Let me know what you think.
	 *
 	 *				JRM -- 7/7/10
	 */
    }

done:

    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5FD_fsync() */

