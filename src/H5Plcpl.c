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
 * Created:		H5Plcpl.c
 *			May  8 2006
 *			Peter Cao <xcao@ncsa.uiuc.edu>
 *
 * Purpose:		Link creation property list class routines
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
#include "H5Lprivate.h"		/* Links        		  	*/
#include "H5Ppkg.h"		/* Property lists		  	*/


/****************/
/* Local Macros */
/****************/

/* ========  Link creation properties ======== */
/* Definitions for create intermediate groups flag */
#define H5L_CRT_INTERMEDIATE_GROUP_SIZE         sizeof(unsigned)
#define H5L_CRT_INTERMEDIATE_GROUP_DEF          0

/* Definitions for target object ID */
#define H5L_CRT_TARGET_ID_SIZE     sizeof(hid_t)
#define H5L_CRT_TARGET_ID_DEF      0

/* Definitions for target object NAME */
#define H5L_CRT_TARGET_NAME_SIZE   sizeof(char *)
#define H5L_CRT_TARGET_NAME_DEF    NULL

/* Definitions for link type */
#define H5L_CRT_LINK_TYPE_SIZE     sizeof(link_type)
#define H5L_CRT_LINK_TYPE_DEF      H5L_TYPE_ERROR

/* Definitions for UDATA */
#define H5L_CRT_UDATA_SIZE   sizeof(void *)
#define H5L_CRT_UDATA_DEF    NULL

/* Definitions for UDATA_SIZE */
#define H5L_CRT_UDATA_SIZE_SIZE   sizeof(size_t)
#define H5L_CRT_UDATA_SIZE_DEF    0

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
static herr_t H5P_lcrt_reg_prop(H5P_genclass_t *pclass);


/*********************/
/* Package Variables */
/*********************/

/* Link creation property list class library initialization object */
const H5P_libclass_t H5P_CLS_LCRT[1] = {{
    "link create",		/* Class name for debugging     */
    &H5P_CLS_STRING_CREATE_g,	/* Parent class ID              */
    &H5P_CLS_LINK_CREATE_g,	/* Pointer to class ID          */
    &H5P_LST_LINK_CREATE_g,	/* Pointer to default property list ID */
    H5P_lcrt_reg_prop,		/* Default property registration routine */
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
 * Function:    H5P_lcrt_reg_prop
 *
 * Purpose:     Register the dataset creation property list class's properties
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              October 31, 2006
 *-------------------------------------------------------------------------
 */
herr_t
H5P_lcrt_reg_prop(H5P_genclass_t *pclass)
{
    unsigned intmd_group = H5L_CRT_INTERMEDIATE_GROUP_DEF;      /* Default setting for creating intermediate groups */
    hid_t target_id = H5L_CRT_TARGET_ID_DEF;
    char *target_name = H5L_CRT_TARGET_NAME_DEF;
    H5L_type_t link_type = H5L_CRT_LINK_TYPE_DEF;
    void *udata = H5L_CRT_UDATA_DEF;
    size_t udata_size = H5L_CRT_UDATA_SIZE_DEF;
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Register create intermediate groups property */
    if(H5P_register_real(pclass, H5L_CRT_INTERMEDIATE_GROUP_NAME, H5L_CRT_INTERMEDIATE_GROUP_SIZE, &intmd_group, NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
         HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    if(H5P_register_real(pclass, H5VL_LINK_TARGET_ID, H5L_CRT_TARGET_ID_SIZE, &target_id,
                         NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    if(H5P_register_real(pclass, H5VL_LINK_TARGET_NAME, H5L_CRT_TARGET_NAME_SIZE, &target_name,
                         NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    if(H5P_register_real(pclass, H5VL_LINK_TYPE, H5L_CRT_LINK_TYPE_SIZE, &link_type,
                         NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    if(H5P_register_real(pclass, H5VL_LINK_UDATA, H5L_CRT_UDATA_SIZE, &udata,
                         NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    if(H5P_register_real(pclass, H5VL_LINK_UDATA_SIZE, H5L_CRT_UDATA_SIZE_SIZE, &udata_size,
                         NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_lcrt_reg_prop() */


/*-------------------------------------------------------------------------
 * Function:    H5Pset_create_intermediate_group
 *
 * Purpose:     set crt_intmd_group so that H5Lcreate_*, H5Olink, etc.
 *              will create missing groups along the given path "name"
 *
 * Note:        XXX: This property should really be an access property. -QAK
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Peter Cao
 *              May 08, 2005
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_create_intermediate_group(hid_t plist_id, unsigned crt_intmd_group)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "iIu", plist_id, crt_intmd_group);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_LINK_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Set value */
    crt_intmd_group = (unsigned)(crt_intmd_group > 0 ? 1 : 0);
    if(H5P_set(plist, H5L_CRT_INTERMEDIATE_GROUP_NAME, &crt_intmd_group) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set intermediate group creation flag")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_create_intermediate_group() */


/*-------------------------------------------------------------------------
 * Function:    H5Pget_create_intermediate_group
 *
 * Purpose:     Returns the crt_intmd_group, which is set to create missing
 *              groups during H5Olink, etc.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Peter Cao
 *              May 08, 2005
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_create_intermediate_group(hid_t plist_id, unsigned *crt_intmd_group /*out*/)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ix", plist_id, crt_intmd_group);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_LINK_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get values */
    if(crt_intmd_group)
        if(H5P_get(plist, H5L_CRT_INTERMEDIATE_GROUP_NAME, crt_intmd_group) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get intermediate group creation flag")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_create_intermediate_group() */

