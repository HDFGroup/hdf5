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

/*
 * Programmer:  Raymond Lu<slu@ncsa.uiuc.edu>
 *              Aug 27, 2007
 */

#define H5Z_PACKAGE		/*suppress error about including H5Zpkg	  */

#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Dprivate.h"		/* Dataset                              */
#include "H5Fprivate.h"         /* File access                          */
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5SMprivate.h"	/* Shared Object Header Messages	*/
#include "H5Zpkg.h"		/* Data filters				*/

#ifdef H5_HAVE_FILTER_DTYPE_MODIFY

/* Local function prototypes */
static herr_t H5Z_set_local_dtype_modify(hid_t dcpl_id, hid_t dtype_id, hid_t space_id,
    hid_t file_id);
static herr_t H5Z_reset_local_dtype_modify(hid_t dcpl_id, hid_t dtype_id, hid_t space_id,
    hid_t file_id);
static size_t H5Z_filter_dtype_modify (unsigned flags, size_t cd_nelmts,
    const unsigned cd_values[], size_t nbytes, size_t *buf_size, void **buf);

/* This message derives from H5Z */
H5Z_class_t H5Z_DTYPE_MODIFY[1] = {{
    H5Z_CLASS_T_VERS,           /* H5Z_class_t version          */
    H5Z_FILTER_DTYPE_MODIFY,	/* Filter id number		*/
    1,                    /* encoder_present flag (set to true) */
    1,                    /* decoder_present flag (set to true) */
    "dtype_modify",		/* Filter name for debugging	*/
    NULL,                       /* The "can apply" callback     */
    H5Z_set_local_dtype_modify, /* The "set local" callback     */
    H5Z_reset_local_dtype_modify, /* The "reset local" callback     */
    H5Z_filter_dtype_modify,	/* The actual filter function	*/
}};


/*-------------------------------------------------------------------------
 * Function:	H5Z_set_local_dtype_modify
 *
 * Purpose:	Set the "local" dataset parameters for datatype modification
 *
 * Return:	Success: Non-negative
 *		Failure: Negative
 *
 * Programmer:	Raymond Lu
 *              30 Aug 2007
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5Z_set_local_dtype_modify(hid_t dcpl_id, hid_t dtype_id, hid_t UNUSED space_id, 
    hid_t file_id)
{
    H5F_t      *file=NULL;         /* File object for file ID */
    size_t cd_nelmts = 1;          /* Number of filter parameters */
    unsigned cd_values[8]={0,0,0,0,0,0,0,0}; /* Filter parameters */
    uint8_t *pp = NULL;
    herr_t ret_value=SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5Z_set_local_dtype_modify, FAIL)

    if(NULL == (file = H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file ID")

    /* The first element of the client data is the ID of dataset's datatype */
    cd_values[0] = dtype_id;

    pp = (uint8_t*)((unsigned *)cd_values + 1);

    /*
     * The second is the file pointer copied as unsigned integer.  Depending 
     * on the size of pointer, it may take the space of 1 - 2 elements.
     */  
    HDmemcpy(pp, &file, sizeof(H5F_t*));
    if(4 == sizeof(H5F_t*))
        cd_nelmts += 1;
    else if(8 == sizeof(H5F_t*))
        cd_nelmts += 2;
    else
	HGOTO_ERROR(H5E_PLINE, H5E_CANTENCODE, FAIL, "not implemented yet")

    /* Modify the filter's parameters for this dataset */
    if(H5Pmodify_filter(dcpl_id, H5Z_FILTER_DTYPE_MODIFY, H5Z_FLAG_MANDATORY, (size_t)cd_nelmts, cd_values) < 0)
	HGOTO_ERROR(H5E_PLINE, H5E_CANTSET, FAIL, "can't set local parameters")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Z_set_local_dtype_modify() */


/*-------------------------------------------------------------------------
 * Function:	H5Z_reset_local_dtype_modify
 *
 * Purpose:	Reset the "local" dataset parameters for datatype 
 *              modification.  This function is the same as 
 *              H5Z_set_local_dtype_modify.  But it's called after the 
 *              dataset has been created.
 *
 * Return:	Success: Non-negative
 *		Failure: Negative
 *
 * Programmer:	Raymond Lu
 *              16 Sept 2007
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5Z_reset_local_dtype_modify(hid_t dcpl_id, hid_t dtype_id, hid_t UNUSED space_id, 
    hid_t file_id)
{
    herr_t ret_value=SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(H5Z_reset_local_dtype_modify, FAIL)

    if((ret_value = H5Z_set_local_dtype_modify(dcpl_id, dtype_id, space_id, file_id))<0)
	HGOTO_ERROR(H5E_PLINE, H5E_CANTSET, FAIL, "can't reset local parameters")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Z_reset_local_dtype_modify() */


/*-------------------------------------------------------------------------
 * Function:	H5Z_filter_dtype_modify
 *
 * Purpose:	Implement the I/O filter of the datatype modification for 
 *              the dataset.  During the write, prepends the encoded current
 *              datatype info of the dataset to the chunked data.  During
 *              the read, decodes the datatype for the chunked data.  If 
 *              it's different from the current datatype of the dataset, 
 *              convert the data.
 *
 * Return:	Success: Size of buffer filtered
 *		Failure: 0
 *
 * Programmer:	Raymond Lu
 *              Aug 28, 2007
 *
 * Modifications:
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
static size_t
H5Z_filter_dtype_modify (unsigned flags, size_t cd_nelmts, const unsigned cd_values[],
                     size_t nbytes, size_t *buf_size, void **buf)
{
    unsigned char    *outbuf = NULL;     /* Pointer to new buffer */
    hid_t    dtype_id;
    H5T_t    *dtype = NULL, *orig_type = NULL;
    size_t   dtype_size, orig_size;
    size_t   type_msg_size;
    size_t   nelmts, dbuf_size;
    H5F_t    *file = NULL;
    uint8_t  *pp = NULL;
    unsigned char *dst=NULL;        /* Temporary pointer to destination buffer */
    size_t   ret_value;             /* Return value */

    FUNC_ENTER_NOAPI(H5Z_filter_dtype_modify, 0)

    assert(cd_nelmts==2 || cd_nelmts==3);

    /* Get the dataset's datatype */
    dtype_id = cd_values[0];
    pp = (uint8_t*)((unsigned *)cd_values + 1);

    /* Depending on the size of pointer, retrieve the value of file pointer */
    if(2 == cd_nelmts) {
        HDmemcpy(&file, pp, 4);
    } else if(3 == cd_nelmts) {
        HDmemcpy(&file, pp, 8);
    } else
        HGOTO_ERROR(H5E_PLINE, H5E_CANTENCODE, 0, "not implemented yet")

    /* The current datatype of the dataset */
    if(NULL==(dtype=(H5T_t *)H5I_object_verify(dtype_id, H5I_DATATYPE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, 0, "not a datatype")

    if (flags & H5Z_FLAG_REVERSE) { /* Read */
        /*
         * Decode the original datatype info from the chunk of data which might 
         * be shared or not shared.  Try to treat it as shared then non-shared.
         */
        H5E_BEGIN_TRY {
            orig_type=(H5T_t *)H5O_msg_decode(file, H5AC_dxpl_id, H5O_DTYPE_ID,
                (unsigned char *)*buf, H5O_MSG_FLAG_SHARED);
        } H5E_END_TRY;

        if(!orig_type) {
            if((orig_type=(H5T_t *)H5O_msg_decode(file, H5AC_dxpl_id, H5O_DTYPE_ID, (unsigned char *)*buf, H5O_MSG_FLAG_WAS_UNKNOWN))==NULL)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDECODE, 0, "can't decode object")
        }

        /* Get the encoding size of the original datatype */
        if((type_msg_size = H5O_msg_raw_size(file, H5O_DTYPE_ID, FALSE, orig_type))==0)
	    HGOTO_ERROR(H5E_DATATYPE, H5E_BADSIZE, 0, "can't find datatype size")

        /* Calculate the size of buffer containing the new data */
        if(0 != H5T_cmp(dtype, orig_type, FALSE)) {
            if((dtype_size = H5T_get_size(dtype))==0)
	        HGOTO_ERROR(H5E_DATATYPE, H5E_BADSIZE, 0, "can't get datatype size")
            if((orig_size = H5T_get_size(orig_type))==0)
	        HGOTO_ERROR(H5E_DATATYPE, H5E_BADSIZE, 0, "can't get datatype size")

            nelmts = (nbytes-type_msg_size)/orig_size;
            if(dtype_size <= orig_size)
                dbuf_size = nbytes-type_msg_size;
            else
                dbuf_size = nelmts*dtype_size;
        } else
            dbuf_size = nbytes-type_msg_size;

        /* Allocate the buffer for the data */ 
	if (NULL==(dst=(unsigned char*)H5MM_malloc(dbuf_size)))
	    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "unable to allocate buffer")

        /* Copy the raw data */
        outbuf = (unsigned char*)*buf + type_msg_size;
        HDmemcpy((void*)dst, (void*)outbuf, nbytes-type_msg_size);

        /* Free input buffer */
 	H5MM_xfree(*buf);

        /* 
         * Convert the data if the current datatype is different from the 
         * original datatype.
         */
        if(0 != H5T_cmp(dtype, orig_type, FALSE)) {
            H5P_genplist_t *plist;              /* Property list pointer */
            hbool_t     is_vlen = FALSE;        /* Flag to indicate VL type */
            hbool_t     vlen_conv = TRUE;       /* Transfer property to indicate no conversion for vlen */
            H5T_path_t		*tpath=NULL;	/*type conversion info	*/
            unsigned char       *bkg=NULL;      /* Pointer to background buffer */
            hid_t               orig_type_id;

	    /* If the datatype is or contains vlen, set the property to indicate no conversion 
	     * is needed for vlen. The file to memory conversion will take place at a 
             * higher level. */
	    if((is_vlen = H5T_detect_class(orig_type, H5T_VLEN)) < 0)
		HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to detect dtatypes")

	    if(is_vlen ) {
		vlen_conv = FALSE;
		if(NULL == (plist = H5P_object_verify(H5AC_dxpl_id, H5P_DATASET_XFER)))
		    HGOTO_ERROR(H5E_PLIST, H5E_BADATOM, FAIL, "can't find object for ID")

		if(H5P_set(plist, H5D_XFER_VLEN_CONV_NAME, &vlen_conv) < 0)
		    HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "Error setting vlen conv flag")
	    }

            /* Find the conversion function */
            if (NULL==(tpath=H5T_path_find(orig_type, dtype, NULL, NULL, /*H5P_DEFAULT*/H5AC_dxpl_id, FALSE)))
	        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, 0, "unable to convert between src and dst data types");

            /* Register the original type and return the ID */
            /*if((orig_type_copy = H5T_copy(orig_type,H5T_COPY_ALL)) == NULL)
	        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, 0, "unable to copy data type")*/

            if((orig_type_id = H5I_register(H5I_DATATYPE, orig_type)) < 0)
	        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, 0, "unable to register data type")

            if (H5T_path_bkg(tpath)) {
	        if (NULL==(bkg=(unsigned char*)H5MM_malloc(dbuf_size)))
	            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "unable to allocate buffer")
            }

            /* Convert the data */
            if (H5T_convert(tpath, orig_type_id, dtype_id, nelmts, (size_t)0, (size_t)0, dst, bkg, /*H5P_DATASET_XFER_DEFAULT*/H5AC_dxpl_id)<0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, 0, "data type conversion failed");

	    /* Set the property of vlen conversion back to normal */
	    if(is_vlen ) {
		vlen_conv = TRUE;

		if(H5P_set(plist, H5D_XFER_VLEN_CONV_NAME, &vlen_conv) < 0)
		    HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "Error setting vlen conv flag")
	    }

            if(H5Tclose(orig_type_id)<0)
	        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCLOSEOBJ, 0, "unable to close data type ID")

            if(bkg)
                H5MM_xfree(bkg);
        } 

        /* Set return values */
        if(0 != H5T_cmp(dtype, orig_type, FALSE))
            *buf_size = nelmts*dtype_size;
        else
            *buf_size = nbytes - type_msg_size;

	*buf = (void*)dst;
	dst = NULL;
        outbuf = NULL;
	ret_value = *buf_size;

    } else { /* Write */
        /* Mark the new datatype as being on disk now */
        if(H5T_set_loc(dtype, file, H5T_LOC_DISK) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, 0, "invalid datatype location")

        /* Check if the datatype should be (or are already) shared in the SOHM table */
        if(H5SM_try_share(file, H5AC_dxpl_id, NULL, H5O_DTYPE_ID, dtype, NULL) < 0)
	    HGOTO_ERROR(H5E_DATATYPE, H5E_BADMESG, 0, "trying to share datatype failed")

        /* Check whether datatype is committed & increment ref count
         * (to maintain ref. count incr/decr similarity with "shared message"
         * type of datatype sharing)
         */
        if(H5T_committed(dtype)) {
            /* Increment the reference count on the shared datatype */
            if(H5T_link(dtype, 1, H5AC_dxpl_id) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_LINKCOUNT, 0, "unable to adjust shared datatype link count")
        } /* end if */

        /* Get the encoding size of the dataset's datatype */
        if((type_msg_size = H5O_msg_raw_size(file, H5O_DTYPE_ID, FALSE, dtype))==0)
	    HGOTO_ERROR(H5E_DATATYPE, H5E_BADSIZE, 0, "can't find datatype size")

        /* Allocate the buffer for the datatype info and the data */
	if (NULL==(dst=outbuf=(unsigned char*)H5MM_malloc(type_msg_size+nbytes)))
	    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, 0, "unable to allocate buffer")

        /* prepend datatype info to raw data for storage */
        if(H5O_msg_encode(file, H5O_DTYPE_ID, FALSE, dst, dtype)<0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, 0, "can't encode object")

        /* Copy raw data */
        dst += type_msg_size;
        HDmemcpy((void*)dst, (void*)(*buf), nbytes);

        /* Free input buffer */
 	H5MM_xfree(*buf);

        /* Set return values */
        *buf_size = type_msg_size + nbytes;
	*buf = (void*)outbuf;
	outbuf = NULL;
	ret_value = *buf_size;
    }

done:
    /*if(outbuf)
        H5MM_xfree(outbuf);*/

    FUNC_LEAVE_NOAPI(ret_value)
}
#endif /* H5_HAVE_FILTER_DTYPE_MODIFY */
