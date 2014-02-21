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
 * Created:		H5Pvcpl.c
 *			February 2014
 *			Mohamad Chaarawi <chaarawi@hdfgroup.org>
 *
 * Purpose:		View creation property list class routines
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
#include "H5Sprivate.h"		/* Dataspaces        		  	*/
#include "H5Vprivate.h"		/* Views        		  	*/
#include "H5Ppkg.h"		/* Property lists		  	*/


/****************/
/* Local Macros */
/****************/

/* ========  View creation properties ======== */
/* Definitions for create intermediate groups flag */
#define H5V_CRT_ELMT_SCOPE_SIZE         sizeof(unsigned)
#define H5V_CRT_ELMT_SCOPE_DEF          -1
#define H5V_CRT_ELMT_SCOPE_ENC          H5P_vcrt_elmt_scope_enc
#define H5V_CRT_ELMT_SCOPE_DEC          H5P_vcrt_elmt_scope_dec
#define H5V_CRT_ELMT_SCOPE_DEL		H5P_vcrt_elmt_scope_del
#define H5V_CRT_ELMT_SCOPE_COPY        	H5P_vcrt_elmt_scope_copy
#define H5V_CRT_ELMT_SCOPE_CMP        	H5P_vcrt_elmt_scope_cmp
#define H5V_CRT_ELMT_SCOPE_CLOSE       	H5P_vcrt_elmt_scope_close

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
static herr_t H5P_vcrt_reg_prop(H5P_genclass_t *pclass);

static herr_t H5P_vcrt_elmt_scope_enc(const void *value, void **_pp, size_t *size);
static herr_t H5P_vcrt_elmt_scope_dec(const void **_pp, void *value);
static herr_t H5P_vcrt_elmt_scope_del(hid_t prop_id, const char* name, size_t size, void* value);
static herr_t H5P_vcrt_elmt_scope_copy(const char* name, size_t size, void* value);
static int H5P_vcrt_elmt_scope_cmp(const void *value1, const void *value2, size_t size);
static herr_t H5P_vcrt_elmt_scope_close(const char* name, size_t size, void* value);

/*********************/
/* Package Variables */
/*********************/

/* View creation property list class library initialization object */
const H5P_libclass_t H5P_CLS_VCRT[1] = {{
    "view create",		/* Class name for debugging     */
    H5P_TYPE_VIEW_CREATE,       /* Class type                   */
    &H5P_CLS_STRING_CREATE_g,	/* Parent class ID              */
    &H5P_CLS_VIEW_CREATE_g,	/* Pointer to class ID          */
    &H5P_LST_VIEW_CREATE_g,	/* Pointer to default property list ID */
    H5P_vcrt_reg_prop,		/* Default property registration routine */
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

/* Property value defaults */
static const hid_t H5V_def_elmt_scope_g = H5V_CRT_ELMT_SCOPE_DEF;      /* Default setting for creating intermediate groups */



/*-------------------------------------------------------------------------
 * Function:    H5P_vcrt_reg_prop
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
H5P_vcrt_reg_prop(H5P_genclass_t *pclass)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Register elmt region dataspace scope */
    if(H5P_register_real(pclass, H5V_CRT_ELMT_SCOPE_NAME, H5V_CRT_ELMT_SCOPE_SIZE, &H5V_def_elmt_scope_g, 
                         NULL, NULL, NULL, H5V_CRT_ELMT_SCOPE_ENC, H5V_CRT_ELMT_SCOPE_DEC,
                         H5V_CRT_ELMT_SCOPE_DEL, H5V_CRT_ELMT_SCOPE_COPY, H5V_CRT_ELMT_SCOPE_CMP, H5V_CRT_ELMT_SCOPE_CLOSE) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_vcrt_reg_prop() */


/*--------------------------------------------------------------------------
 * Function:	H5P_vcrt_elmt_scope_del
 *
 * Purpose:	Close the dataspace for vcpl
 *
 * Return:	Success:	Non-negative
 * 		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              February 2014
 *
 *--------------------------------------------------------------------------
 */
/* ARGSUSED */
static herr_t
H5P_vcrt_elmt_scope_del(hid_t UNUSED prop_id, const char UNUSED *name, size_t UNUSED size, void *value)
{
    hid_t          space_id;
    herr_t         ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(value);

    space_id = (*(const hid_t *)value);

    if((space_id >= 0) && (H5I_dec_ref(space_id) < 0))
	HGOTO_ERROR(H5E_PLIST, H5E_CANTRELEASE, FAIL, "unable to close atom for dataspace scope")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_vcrt_elmt_scope_del() */


/*--------------------------------------------------------------------------
 * Function:	H5P_vcrt_elmt_scope_copy
 *
 * Purpose:	Copy the dataspace
 *
 * Return:	Success:	Non-negative
 * 		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              February 2014
 *
 *--------------------------------------------------------------------------
 */
/* ARGSUSED */
static herr_t
H5P_vcrt_elmt_scope_copy(const char UNUSED *name, size_t UNUSED size, void *value)
{
    hid_t          space_id;
    herr_t         ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(value);

    space_id = (*(const hid_t *)value);

    if(space_id > H5P_DEFAULT) {
        H5S_t *space = NULL, *new_space = NULL;

        if(NULL == (space = (H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE)))
            HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dataspace")

        /* Copy */
        if(NULL == (new_space = H5S_copy(space, FALSE, TRUE)))
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to copy dataspace")

        /* Atomize */
        if(((*(hid_t *)value) = H5I_register (H5I_DATASPACE, new_space, TRUE)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register dataspace atom")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_vcrt_elmt_scope_copy() */


/*-------------------------------------------------------------------------
 * Function:    H5P_vcrt_elmt_scope_cmp
 *
 * Purpose:     Callback routine which is called whenever the elmt dataspace 
 *              region property in the vcpl
 *
 * Return:      zero if VALUE1 and VALUE2 are equal, non zero otherwise.
 *
 * Programmer:	Mohamad Chaarawi
 *              February 2014
 *
 *-------------------------------------------------------------------------
 */
static int
H5P_vcrt_elmt_scope_cmp(const void *value1, const void *value2, size_t UNUSED size)
{
    const hid_t *space1_id = (const hid_t *)value1;
    const hid_t *space2_id = (const hid_t *)value2;
    H5S_t *ds1 = NULL, *ds2 = NULL;
    htri_t result;
    int ret_value = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* check args */
    if(NULL == (ds1 = (H5S_t *)H5I_object_verify(*space1_id, H5I_DATASPACE)) ||
       NULL == (ds2 = (H5S_t *)H5I_object_verify(*space2_id, H5I_DATASPACE))) 
        HGOTO_DONE(-1);

    /* Check dataspaces for extent's equality */
    if((result = H5S_extent_equal(ds1, ds2)) < 0)
        HGOTO_DONE(-1);

    if(TRUE == result)
        ret_value = 0;
    else
        ret_value = -1;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_vcrt_elmt_scope_cmp() */


/*--------------------------------------------------------------------------
 * Function:	H5P_vcrt_elmt_scope_close
 *
 * Purpose:	Close the dataspace for vcpl
 *
 * Return:	Success:	Non-negative
 * 		Failure:	Negative
 *
 * Programmer:	Mohamad Chaarawi
 *              February 2014
 *
 *--------------------------------------------------------------------------
 */
/* ARGSUSED */
static herr_t
H5P_vcrt_elmt_scope_close(const char UNUSED *name, size_t UNUSED size, void *value)
{
    hid_t          space_id;
    herr_t         ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(value);

    space_id = (*(const hid_t *)value);

    if((space_id >= 0) && (H5I_dec_ref(space_id) < 0))
	HGOTO_ERROR(H5E_PLIST, H5E_CANTRELEASE, FAIL, "unable to close atom for dataspace scope")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_vcrt_elmt_scope_close() */


/*-------------------------------------------------------------------------
 * Function:       H5P_vcrt_elmt_scope_enc
 *
 * Purpose:        Callback routine which is called whenever the dataspace scope
 *                 property in the vcpl is encoded
 *
 * Return:	   Success:	Non-negative
 *		   Failure:	Negative
 *
 * Programmer:	   Mohamad Chaarawi
 *                 February 2014
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P_vcrt_elmt_scope_enc(const void *value, void **_pp, size_t *size)
{
    const hid_t *space_id = (const hid_t *)value;     /* Property to encode */
    uint8_t **pp = (uint8_t **)_pp;
    H5S_t *space;
    hbool_t non_default_space = FALSE;   /* Whether the FAPL is non-default */
    size_t enc_size = 0;                /* FAPL's encoded size */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check for non-default FAPL */
    if(*space_id != -1) {
        if(NULL == (space = (H5S_t *)H5I_object_verify(*space_id, H5I_DATASPACE)))
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get dataspace")
        non_default_space = TRUE;
    } /* end if */

    if(NULL != *pp) {
        /* Store whether the FAPL is non-default */
        *(*pp)++ = (uint8_t)non_default_space;
    } /* end if */

    /* Encode the property list, if non-default */
    /* (if *pp == NULL, will only compute the size) */
    if(non_default_space) {
        if(H5S_encode(space, *pp, &enc_size) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "can't encode dataspace")
        if(*pp)
            *pp += enc_size;
    } /* end if */

    *size += (1 + enc_size);      /* Non-default flag, plus encoded property list size */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_vcrt_elmt_scope_enc() */


/*-------------------------------------------------------------------------
 * Function:       H5P_vcrt_elmt_scope_dec
 *
 * Purpose:        Callback routine which is called whenever the dataspace scope
 *                 property in the vcpl is decoded
 *
 * Return:	   Success:	Non-negative
 *		   Failure:	Negative
 *
 * Programmer:	   Mohamad Chaarawi
 *                 February 2014
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P_vcrt_elmt_scope_dec(const void **_pp, void *_value)
{
    hid_t *space_id = (hid_t *)_value;        /* The elink FAPL value */
    const uint8_t **pp = (const uint8_t **)_pp;
    hbool_t non_default_space;           /* Whether the FAPL is non-default */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(pp);
    HDassert(*pp);
    HDassert(space_id);
    HDcompile_assert(sizeof(size_t) <= sizeof(uint64_t));

    /* Determine if the FAPL is non-default */
    non_default_space = (hbool_t)*(*pp)++;

    if(non_default_space) {
        H5S_t *space;         /* Pointer to property list */
        size_t enc_size = 0;  /* Encoded size of property list */

        /* Decode the property list */
        if(NULL == (space = H5S_decode(*pp)))
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "can't decode dataspace")

        /* Register the type and return the ID */
        if((*space_id = H5I_register(H5I_DATASPACE, space, TRUE)) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTREGISTER, FAIL, "unable to register dataspace")

        /* Compute the encoded size of the property list */
        if(H5S_encode(space, NULL, &enc_size) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "can't compute encoded property list size")

        *pp += enc_size;
    } /* end if */
    else
        *space_id = -1;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_vcrt_elmt_scope_dec() */
