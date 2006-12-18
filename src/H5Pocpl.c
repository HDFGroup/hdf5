/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:		H5Pocpl.c
 *			Nov 28 2006
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Object creation property list class routines
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/
#define H5P_PACKAGE		/*suppress error about including H5Ppkg	  */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Ppkg.h"		/* Property lists		  	*/


/****************/
/* Local Macros */
/****************/

/* ========= Object Creation properties ============ */
/* Definitions for the max. # of attributes to store compactly */
#define H5O_CRT_ATTR_MAX_COMPACT_SIZE   sizeof(unsigned)
#define H5O_CRT_ATTR_MAX_COMPACT_DEF    8
/* Definitions for the min. # of attributes to store densely */
#define H5O_CRT_ATTR_MIN_DENSE_SIZE     sizeof(unsigned)
#define H5O_CRT_ATTR_MIN_DENSE_DEF      6


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

/* Property class callbacks */
static herr_t H5P_ocrt_reg_prop(H5P_genclass_t *pclass);


/*********************/
/* Package Variables */
/*********************/

/* Object creation property list class library initialization object */
const H5P_libclass_t H5P_CLS_OCRT[1] = {{
    "object create",		/* Class name for debugging     */
    &H5P_CLS_ROOT_g,		/* Parent class ID              */
    &H5P_CLS_OBJECT_CREATE_g,	/* Pointer to class ID          */
    NULL,			/* Pointer to default property list ID */
    H5P_ocrt_reg_prop,		/* Default property registration routine */
    NULL,		        /* Class creation callback      */
    NULL,		        /* Class creation callback info */
    NULL,			/* Class copy callback          */
    NULL,		        /* Class copy callback info     */
    NULL,			/* Class close callback         */
    NULL 		        /* Class close callback info    */
}};



/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:    H5P_ocrt_reg_prop
 *
 * Purpose:     Initialize the object creation property list class
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              November 28, 2006
 *-------------------------------------------------------------------------
 */
static herr_t
H5P_ocrt_reg_prop(H5P_genclass_t *pclass)
{
    unsigned attr_max_compact = H5O_CRT_ATTR_MAX_COMPACT_DEF;   /* Default max. compact attribute storage settings */
    unsigned attr_min_dense = H5O_CRT_ATTR_MIN_DENSE_DEF;       /* Default min. dense attribute storage settings */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5P_ocrt_reg_prop)

    /* Register max. compact attribute storage property */
    if(H5P_register(pclass, H5O_CRT_ATTR_MAX_COMPACT_NAME, H5O_CRT_ATTR_MAX_COMPACT_SIZE,
             &attr_max_compact, NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
         HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register min. dense attribute storage property */
    if(H5P_register(pclass, H5O_CRT_ATTR_MIN_DENSE_NAME, H5O_CRT_ATTR_MIN_DENSE_SIZE,
             &attr_min_dense, NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
         HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_ocrt_reg_prop() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_attr_phase_change
 *
 * Purpose:	Sets the cutoff values for indexes storing attributes
 *              in object headers for this file.  If more than max_compact
 *              attributes are in an object header, the attributes will be
 *              moved to a heap and indexed with a B-tree.
 *              Likewise, an object header containing fewer than min_dense
 *              attributes will be converted back to storing the attributes
 *              directly in the object header.
 *
 *              If the max_compact is zero then attributes for this object will
 *              never be stored in the object header but will be always be
 *              stored in a heap.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Tuesday, November 28, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_attr_phase_change(hid_t plist_id, unsigned max_compact, unsigned min_dense)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(H5Pset_attr_phase_change, FAIL)
    H5TRACE3("e", "iIuIu", plist_id, max_compact, min_dense);

    /* Range check values */
    if(max_compact < min_dense)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "max compact value must be >= min dense value")
    if(max_compact > 65535)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "max compact value must be < 65536")
    if(min_dense > 65535)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "min dense value must be < 65536")

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Set property values */
    if(H5P_set(plist, H5O_CRT_ATTR_MAX_COMPACT_NAME, &max_compact) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set max. # of compact attributes in property list")
    if(H5P_set(plist, H5O_CRT_ATTR_MIN_DENSE_NAME, &min_dense) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set min. # of dense attributes in property list")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_attr_phase_change */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_attr_phase_change
 *
 * Purpose:	Gets the phase change values for attribute storage
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Tuesday, November 28, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_attr_phase_change(hid_t plist_id, unsigned *max_compact, unsigned *min_dense)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(H5Pget_attr_phase_change, FAIL)
    H5TRACE3("e", "i*Iu*Iu", plist_id, max_compact, min_dense);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get values */
    if(max_compact) {
        if(H5P_get(plist, H5O_CRT_ATTR_MAX_COMPACT_NAME, max_compact) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get max. # of compact attributes")
    } /* end if */
    if(min_dense) {
        if(H5P_get(plist, H5O_CRT_ATTR_MIN_DENSE_NAME, min_dense) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get min. # of dense attributes")
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_attr_phase_change() */

