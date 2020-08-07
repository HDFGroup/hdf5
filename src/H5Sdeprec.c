/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:	H5Sdeprec.c
 *
 * Purpose:	Deprecated functions from the H5S interface.  These
 *          functions are here for compatibility purposes and may be
 *          removed in the future.  Applications should switch to the
 *          newer APIs.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#include "H5Smodule.h"          /* This source code file is part of the H5S module */


/***********/
/* Headers */
/***********/
#include "H5private.h"      /* Generic Functions        */
#include "H5CXprivate.h"    /* API Contexts             */
#include "H5Spkg.h"		    /* Dataspaces               */
#include "H5Eprivate.h"     /* Error handling           */
#include "H5Iprivate.h"     /* IDs                      */


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


#ifndef H5_NO_DEPRECATED_SYMBOLS

/* --------------------------------------------------------------------------*/
/**\ingroup H5S
 *
 * \brief Encodes a data space object description into a binary buffer
 *
 * \space_id{obj_id}
 * \param[in,out] buf     Buffer for the object to be encoded into;
 *                        If the provided buffer is NULL, only the size of 
 *                        buffer needed is returned through \p nalloc.   
 * \param[in,out] nalloc  The size of the allocated buffer
 *
 * \return \herr_t
 *
 * \deprecated Deprecated in favor of H5Sencode2()
 *
 * \details Given the data space identifier \p obj_id, H5Sencode1() converts 
 *          a data space description into binary form in a buffer. Using 
 *          this binary form in the buffer, a data space object can be 
 *          reconstructed using H5Sdecode() to return a new object handle 
 *          (\p hid_t) for this data space.
 *
 *          A preliminary H5Sencode1() call can be made to find out the size 
 *          of the buffer needed. This value is returned as \p nalloc. That 
 *          value can then be assigned to \p nalloc for a second H5Sencode1() 
 *          call, which will retrieve the actual encoded object.
 *
 *          If the library finds out \p nalloc is not big enough for the 
 *          object, it simply returns the size of the buffer needed through 
 *          \p nalloc without encoding the provided buffer.
 *
 *          The types of data space addressed in this function are null, 
 *          scalar, and simple space. For a simple data space, the information 
 *          on the selection, for example, hyperslab selection, is also 
 *          encoded and decoded. A complex data space has not been 
 *          implemented in the library.
 *
 * \version 1.12.0 The function H5Sencode was renamed H5Sencode1() and
 *                 deprecated.
 * \since 1.8.0
 */

/*-------------------------------------------------------------------------
 * Function:	H5Sencode1
 *
 * Purpose:	Given a dataspace ID, converts the object description
 *          (including selection) into binary in a buffer.
 *
 * Return:	Success:	non-negative
 *		    Failure:	negative
 *
 * Programmer:	Raymond Lu
 *              slu@ncsa.uiuc.edu
 *              July 14, 2004
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Sencode1(hid_t obj_id, void *buf, size_t *nalloc)
{
    H5S_t       *dspace;
    hid_t       temp_fapl_id = H5P_DEFAULT;
    herr_t      ret_value=SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "i*x*z", obj_id, buf, nalloc);

    /* Check argument and retrieve object */
    if (NULL == (dspace = (H5S_t *)H5I_object_verify(obj_id, H5I_DATASPACE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataspace")

    /* Verify access property list and set up collective metadata if appropriate */
    if(H5CX_set_apl(&temp_fapl_id, H5P_CLS_FACC, H5I_INVALID_HID, TRUE) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, H5I_INVALID_HID, "can't set access property list info")

    /* Use (earliest, latest) i.e. not latest format */
    if(H5S_encode(dspace, (unsigned char **)&buf, nalloc)<0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "can't encode dataspace")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Sencode1() */
#endif /* H5_NO_DEPRECATED_SYMBOLS */

