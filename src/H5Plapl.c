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
 * Created:		H5Plapl.c
 *			July 14 2006
 *			James Laird <jlaird@ncsa.uiuc.edu>
 *
 * Purpose:		Link access property list class routines
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
#include "H5Lprivate.h"		/* Links		  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Ppkg.h"		/* Property lists		  	*/


/****************/
/* Local Macros */
/****************/

/* ========  Link access properties ======== */
/* Definitions for number of soft links to traverse */
#define H5L_ACS_NLINKS_SIZE        sizeof(size_t)
#define H5L_ACS_NLINKS_DEF         H5L_NUM_LINKS /*max symlinks to follow per lookup  */
#define H5L_ACS_NLINKS_ENC         H5P__encode_size_t
#define H5L_ACS_NLINKS_DEC         H5P__decode_size_t


/* Definitions for external link prefix */
#define H5L_ACS_ELINK_PREFIX_SIZE        sizeof(char *)
#define H5L_ACS_ELINK_PREFIX_DEF         NULL /*default is no prefix */
#define H5L_ACS_ELINK_PREFIX_ENC         H5P_lacc_elink_pref_enc
#define H5L_ACS_ELINK_PREFIX_DEC         H5P_lacc_elink_pref_dec
#define H5L_ACS_ELINK_PREFIX_DEL         H5P_lacc_elink_pref_del
#define H5L_ACS_ELINK_PREFIX_COPY        H5P_lacc_elink_pref_copy
#define H5L_ACS_ELINK_PREFIX_CMP         H5P_lacc_elink_pref_cmp
#define H5L_ACS_ELINK_PREFIX_CLOSE       H5P_lacc_elink_pref_close

/* Definitions for setting fapl of external link access */
#define H5L_ACS_ELINK_FAPL_SIZE        	sizeof(hid_t)
#define H5L_ACS_ELINK_FAPL_DEF         	H5P_DEFAULT
#define H5L_ACS_ELINK_FAPL_ENC		H5P_lacc_elink_fapl_enc
#define H5L_ACS_ELINK_FAPL_DEC         	H5P_lacc_elink_fapl_dec
#define H5L_ACS_ELINK_FAPL_DEL		H5P_lacc_elink_fapl_del
#define H5L_ACS_ELINK_FAPL_COPY        	H5P_lacc_elink_fapl_copy
#define H5L_ACS_ELINK_FAPL_CMP        	H5P_lacc_elink_fapl_cmp
#define H5L_ACS_ELINK_FAPL_CLOSE       	H5P_lacc_elink_fapl_close

/* Definitions for file access flags for external link traversal */
#define H5L_ACS_ELINK_FLAGS_SIZE        sizeof(unsigned)
#define H5L_ACS_ELINK_FLAGS_DEF         H5F_ACC_DEFAULT
#define H5L_ACS_ELINK_FLAGS_ENC         H5P__encode_unsigned
#define H5L_ACS_ELINK_FLAGS_DEC         H5P__decode_unsigned

/* Definitions for callback function for external link traversal */
#define H5L_ACS_ELINK_CB_SIZE           sizeof(H5L_elink_cb_t)
#define H5L_ACS_ELINK_CB_DEF            {NULL,NULL}


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
static herr_t H5P_lacc_reg_prop(H5P_genclass_t *pclass);

/* Property list callbacks */
static herr_t H5P_lacc_elink_pref_enc(const void *value, void **_pp, size_t *size);
static herr_t H5P_lacc_elink_pref_dec(const void **_pp, void *value);
static herr_t H5P_lacc_elink_pref_del(hid_t prop_id, const char* name, size_t size, void* value);
static herr_t H5P_lacc_elink_pref_copy(const char* name, size_t size, void* value);
static int H5P_lacc_elink_pref_cmp(const void *value1, const void *value2, size_t size);
static herr_t H5P_lacc_elink_pref_close(const char* name, size_t size, void* value);
static herr_t H5P_lacc_elink_fapl_enc(const void *value, void **_pp, size_t *size);
static herr_t H5P_lacc_elink_fapl_dec(const void **_pp, void *value);
static herr_t H5P_lacc_elink_fapl_del(hid_t prop_id, const char* name, size_t size, void* value);
static herr_t H5P_lacc_elink_fapl_copy(const char* name, size_t size, void* value);
static int H5P_lacc_elink_fapl_cmp(const void *value1, const void *value2, size_t size);
static herr_t H5P_lacc_elink_fapl_close(const char* name, size_t size, void* value);


/*********************/
/* Package Variables */
/*********************/

/* Dataset creation property list class library initialization object */
const H5P_libclass_t H5P_CLS_LACC[1] = {{
    "link access",		/* Class name for debugging     */
    H5P_TYPE_LINK_ACCESS,       /* Class type                   */
    &H5P_CLS_ROOT_g,		/* Parent class ID              */
    &H5P_CLS_LINK_ACCESS_g,	/* Pointer to class ID          */
    &H5P_LST_LINK_ACCESS_g,	/* Pointer to default property list ID */
    H5P_lacc_reg_prop,		/* Default property registration routine */
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
static const size_t H5L_def_nlinks_g = H5L_ACS_NLINKS_DEF; 	   /* Default number of soft links to traverse */
static const char *H5L_def_elink_prefix_g = H5L_ACS_ELINK_PREFIX_DEF; /* Default external link prefix string */
static const hid_t H5L_def_fapl_id_g = H5L_ACS_ELINK_FAPL_DEF;    /* Default fapl for external link access */
static const unsigned H5L_def_elink_flags_g = H5L_ACS_ELINK_FLAGS_DEF; /* Default file access flags for external link traversal */
static const H5L_elink_cb_t H5L_def_elink_cb_g = H5L_ACS_ELINK_CB_DEF; /* Default external link traversal callback */



/*-------------------------------------------------------------------------
 * Function:    H5P_lacc_reg_prop
 *
 * Purpose:     Register the dataset creation property list class's properties
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              October 31, 2006
 *
 * Modifications:
 *	Vailin Choi, Sept. 12th 2008
 *	Register the setting of file access property list for link access
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P_lacc_reg_prop(H5P_genclass_t *pclass)
{
    herr_t ret_value = SUCCEED;         	   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Register property for number of links traversed */
    if(H5P_register_real(pclass, H5L_ACS_NLINKS_NAME, H5L_ACS_NLINKS_SIZE, &H5L_def_nlinks_g, 
            NULL, NULL, NULL, H5L_ACS_NLINKS_ENC, H5L_ACS_NLINKS_DEC,
            NULL, NULL, NULL, NULL) < 0)
         HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register property for external link prefix */
    if(H5P_register_real(pclass, H5L_ACS_ELINK_PREFIX_NAME, H5L_ACS_ELINK_PREFIX_SIZE, &H5L_def_elink_prefix_g, 
            NULL, NULL, NULL, H5L_ACS_ELINK_PREFIX_ENC, H5L_ACS_ELINK_PREFIX_DEC,
            H5L_ACS_ELINK_PREFIX_DEL, H5L_ACS_ELINK_PREFIX_COPY, H5L_ACS_ELINK_PREFIX_CMP, H5L_ACS_ELINK_PREFIX_CLOSE) < 0)
         HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register fapl for link access */
    if(H5P_register_real(pclass, H5L_ACS_ELINK_FAPL_NAME, H5L_ACS_ELINK_FAPL_SIZE, &H5L_def_fapl_id_g, 
             NULL, NULL, NULL, H5L_ACS_ELINK_FAPL_ENC, H5L_ACS_ELINK_FAPL_DEC,
             H5L_ACS_ELINK_FAPL_DEL, H5L_ACS_ELINK_FAPL_COPY, H5L_ACS_ELINK_FAPL_CMP, H5L_ACS_ELINK_FAPL_CLOSE) < 0)
         HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register property for external link file access flags */
    if(H5P_register_real(pclass, H5L_ACS_ELINK_FLAGS_NAME, H5L_ACS_ELINK_FLAGS_SIZE, &H5L_def_elink_flags_g, 
             NULL, NULL, NULL, H5L_ACS_ELINK_FLAGS_ENC, H5L_ACS_ELINK_FLAGS_DEC,
             NULL, NULL, NULL, NULL) < 0)
         HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register property for external link file traversal callback */
    /* (Note: this property should not have an encode/decode callback -QAK) */
    if(H5P_register_real(pclass, H5L_ACS_ELINK_CB_NAME, H5L_ACS_ELINK_CB_SIZE, &H5L_def_elink_cb_g, 
             NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
         HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_lacc_reg_prop() */


/*-------------------------------------------------------------------------
 * Function:       H5P_lacc_elink_fapl_enc
 *
 * Purpose:        Callback routine which is called whenever the elink FAPL
 *                 property in the dataset access property list is
 *                 encoded.
 *
 * Return:	   Success:	Non-negative
 *		   Failure:	Negative
 *
 * Programmer:     Quincey Koziol
 *                 Wednesday, August 15, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P_lacc_elink_fapl_enc(const void *value, void **_pp, size_t *size)
{
    const hid_t *elink_fapl = (const hid_t *)value;     /* Property to encode */
    uint8_t **pp = (uint8_t **)_pp;
    H5P_genplist_t *fapl_plist;         /* Pointer to property list */
    hbool_t non_default_fapl = FALSE;   /* Whether the FAPL is non-default */
    size_t enc_size = 0;                /* FAPL's encoded size */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check for non-default FAPL */
    if(*elink_fapl != H5P_DEFAULT) {
        if(NULL == (fapl_plist = (H5P_genplist_t *)H5P_object_verify(*elink_fapl, H5P_FILE_ACCESS)))
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property list")
        non_default_fapl = TRUE;
    } /* end if */

    if(NULL != *pp) {
        /* Store whether the FAPL is non-default */
        *(*pp)++ = (uint8_t)non_default_fapl;
    } /* end if */

    /* Encode the property list, if non-default */
    /* (if *pp == NULL, will only compute the size) */
    if(non_default_fapl) {
        if(H5P__encode(fapl_plist, TRUE, *pp, &enc_size) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "can't encode property list")
        if(*pp)
            *pp += enc_size;
    } /* end if */

    *size += (1 + enc_size);      /* Non-default flag, plus encoded property list size */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_lacc_elink_fapl_enc() */


/*-------------------------------------------------------------------------
 * Function:       H5P_lacc_elink_fapl_dec
 *
 * Purpose:        Callback routine which is called whenever the elink FAPL
 *                 property in the dataset access property list is
 *                 decoded.
 *
 * Return:	   Success:	Non-negative
 *		   Failure:	Negative
 *
 * Programmer:     Quincey Koziol
 *                 Wednesday, August 15, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P_lacc_elink_fapl_dec(const void **_pp, void *_value)
{
    hid_t *elink_fapl = (hid_t *)_value;        /* The elink FAPL value */
    const uint8_t **pp = (const uint8_t **)_pp;
    hbool_t non_default_fapl;           /* Whether the FAPL is non-default */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(pp);
    HDassert(*pp);
    HDassert(elink_fapl);
    HDcompile_assert(sizeof(size_t) <= sizeof(uint64_t));

    /* Determine if the FAPL is non-default */
    non_default_fapl = (hbool_t)*(*pp)++;

    if(non_default_fapl) {
        H5P_genplist_t *fapl_plist;         /* Pointer to property list */
        size_t enc_size = 0;                /* Encoded size of property list */

        /* Decode the property list */
        if((*elink_fapl = H5P__decode(*pp)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "can't decode property")

        /* Get the property list object */
        if(NULL == (fapl_plist = (H5P_genplist_t *)H5P_object_verify(*elink_fapl, H5P_FILE_ACCESS)))
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property list")

        /* Compute the encoded size of the property list */
        if(H5P__encode(fapl_plist, TRUE, NULL, &enc_size) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "can't compute encoded property list size")

        *pp += enc_size;
    } /* end if */
    else
        *elink_fapl = H5P_DEFAULT;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_lacc_elink_fapl_dec() */


/*--------------------------------------------------------------------------
 * Function:	H5P_lacc_elink_fapl_del
 *
 * Purpose:	Close the FAPL for link access
 *
 * Return:	Success:	Non-negative
 * 		Failure:	Negative
 *
 * Programmer:	Vailin Choi
 *		Tuesday, Sept 23, 2008
 *
 *--------------------------------------------------------------------------
 */
/* ARGSUSED */
static herr_t
H5P_lacc_elink_fapl_del(hid_t UNUSED prop_id, const char UNUSED *name, size_t UNUSED size, void *value)
{
    hid_t          l_fapl_id;
    herr_t         ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(value);

    l_fapl_id = (*(const hid_t *)value);

    if((l_fapl_id > H5P_DEFAULT) && (H5I_dec_ref(l_fapl_id) < 0))
	HGOTO_ERROR(H5E_PLIST, H5E_CANTRELEASE, FAIL, "unable to close atom for file access property list")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_lacc_elink_fapl_del() */


/*--------------------------------------------------------------------------
 * Function:	H5P_lacc_elink_fapl_copy
 *
 * Purpose:	Copy the FAPL for link access
 *
 * Return:	Success:	Non-negative
 * 		Failure:	Negative
 *
 * Programmer:	Vailin Choi
 *		Tuesday, Sept 23, 2008
 *
 *--------------------------------------------------------------------------
 */
/* ARGSUSED */
static herr_t
H5P_lacc_elink_fapl_copy(const char UNUSED *name, size_t UNUSED size, void *value)
{
    hid_t          l_fapl_id;
    herr_t         ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(value);

    l_fapl_id = (*(const hid_t *)value);

    if(l_fapl_id > H5P_DEFAULT) {
        H5P_genplist_t *l_fapl_plist;

        if(NULL == (l_fapl_plist = (H5P_genplist_t *)H5P_object_verify(l_fapl_id, H5P_FILE_ACCESS)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "can't get property list")

	if(((*(hid_t *)value) = H5P_copy_plist(l_fapl_plist, FALSE)) < 0)
	    HGOTO_ERROR(H5E_INTERNAL, H5E_CANTINIT, FAIL, "unable to copy file access properties")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_lacc_elink_fapl_copy() */


/*-------------------------------------------------------------------------
 * Function:       H5P_lacc_elink_fapl_cmp
 *
 * Purpose:        Callback routine which is called whenever the elink FAPL
 *                 property in the link access property list is
 *                 compared.
 *
 * Return:         zero if VALUE1 and VALUE2 are equal, non zero otherwise.
 *
 * Programmer:     Quincey Koziol
 *                 Wednesday, August 15, 2012
 *
 *-------------------------------------------------------------------------
 */
static int
H5P_lacc_elink_fapl_cmp(const void *value1, const void *value2, size_t UNUSED size)
{
    const hid_t *fapl1 = (const hid_t *)value1;
    const hid_t *fapl2 = (const hid_t *)value2;
    H5P_genplist_t *obj1, *obj2;          /* Property lists to compare */
    int ret_value = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Check for comparison with default value */
    if(*fapl1 == 0 && *fapl2 > 0) HGOTO_DONE(1);
    if(*fapl1 > 0 && *fapl2 == 0) HGOTO_DONE(-1);

    /* Get the property list objects */
    obj1 = (H5P_genplist_t *)H5I_object(*fapl1);
    obj2 = (H5P_genplist_t *)H5I_object(*fapl2);

    /* Check for NULL property lists */
    if(obj1 == NULL && obj2 != NULL) HGOTO_DONE(1);
    if(obj1 != NULL && obj2 == NULL) HGOTO_DONE(-1);
    if(obj1 && obj2) {
        herr_t status;

        status = H5P_cmp_plist(obj1, obj2, &ret_value);
        HDassert(status >= 0);
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_lacc_elink_fapl_cmp() */


/*--------------------------------------------------------------------------
 * Function:	H5P_lacc_elink_fapl_close
 *
 * Purpose:	Close the FAPL for link access
 *
 * Return:	Success:	Non-negative
 * 		Failure:	Negative
 *
 * Programmer:	Vailin Choi
 *		Tuesday, Sept 23, 2008
 *
 *---------------------------------------------------------------------------
 */
/* ARGSUSED */
static herr_t
H5P_lacc_elink_fapl_close(const char UNUSED *name, size_t UNUSED size, void *value)
{
    hid_t		l_fapl_id;
    herr_t     		ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(value);

    l_fapl_id = (*(const hid_t *)value);
    if((l_fapl_id > H5P_DEFAULT) && (H5I_dec_ref(l_fapl_id) < 0))
	HGOTO_ERROR(H5E_PLIST, H5E_CANTRELEASE, FAIL, "unable to close atom for file access property list")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_lacc_elink_fapl_close() */


/*-------------------------------------------------------------------------
 * Function:       H5P_lacc_elink_pref_enc
 *
 * Purpose:        Callback routine which is called whenever the elink flags
 *                 property in the dataset access property list is
 *                 encoded.
 *
 * Return:	   Success:	Non-negative
 *		   Failure:	Negative
 *
 * Programmer:     Mohamad Chaarawi
 *                 Monday, October 10, 2011
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P_lacc_elink_pref_enc(const void *value, void **_pp, size_t *size)
{
    const char *elink_pref = *(const char * const *)value;
    uint8_t **pp = (uint8_t **)_pp;
    size_t len = 0;
    uint64_t enc_value;
    unsigned enc_size;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDcompile_assert(sizeof(size_t) <= sizeof(uint64_t));

    /* calculate prefix length */
    if(NULL != elink_pref)
        len = HDstrlen(elink_pref);

    enc_value = (uint64_t)len;
    enc_size = H5V_limit_enc_size(enc_value);
    HDassert(enc_size < 256);

    if(NULL != *pp) {
        /* encode the length of the prefix */
        *(*pp)++ = (uint8_t)enc_size;
        UINT64ENCODE_VAR(*pp, enc_value, enc_size);

        /* encode the prefix */
        if(NULL != elink_pref) {
            HDmemcpy(*(char **)pp, elink_pref, len);
            *pp += len;
        } /* end if */
    } /* end if */

    *size += (1 + enc_size);
    if(NULL != elink_pref)
        *size += len;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5P_lacc_elink_pref_enc() */


/*-------------------------------------------------------------------------
 * Function:       H5P_lacc_elink_pref_dec
 *
 * Purpose:        Callback routine which is called whenever the elink prefix
 *                 property in the dataset access property list is
 *                 decoded.
 *
 * Return:	   Success:	Non-negative
 *		   Failure:	Negative
 *
 * Programmer:     Mohamad Chaarawi
 *                 Monday, October 10, 2011
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P_lacc_elink_pref_dec(const void **_pp, void *_value)
{
    char **elink_pref = (char **)_value;
    const uint8_t **pp = (const uint8_t **)_pp;
    size_t len;
    uint64_t enc_value;                 /* Decoded property value */
    unsigned enc_size;                  /* Size of encoded property */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(pp);
    HDassert(*pp);
    HDassert(elink_pref);
    HDcompile_assert(sizeof(size_t) <= sizeof(uint64_t));

    /* Decode the size */
    enc_size = *(*pp)++;
    HDassert(enc_size < 256);

    /* Decode the value */
    UINT64DECODE_VAR(*pp, enc_value, enc_size);
    len = enc_value;

    if(0 != len) {
        /* Make a copy of the user's prefix string */
        if(NULL == (*elink_pref = (char *)H5MM_malloc(len + 1)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL, "memory allocation failed for prefix")
        HDstrncpy(*elink_pref, *(const char **)pp, len);
        (*elink_pref)[len] = '\0';

        *pp += len;
    } /* end if */
    else
        *elink_pref = NULL;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_lacc_elink_pref_dec() */


/*-------------------------------------------------------------------------
 * Function:    H5P_lacc_elink_pref_del
 *
 * Purpose:     Frees memory used to store the external link prefix string
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              November 2, 2006
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
static herr_t
H5P_lacc_elink_pref_del(hid_t UNUSED prop_id, const char UNUSED *name, size_t UNUSED size, void *value)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(value);

    H5MM_xfree(*(void **)value);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5P_lacc_elink_pref_del() */


/*-------------------------------------------------------------------------
 * Function:    H5P_lacc_elink_pref_copy
 *
 * Purpose:     Creates a copy of the external link prefix string
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              November 2, 2006
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
static herr_t
H5P_lacc_elink_pref_copy(const char UNUSED *name, size_t UNUSED size, void *value)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(value);

    *(char **)value = H5MM_xstrdup(*(const char **)value);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5P_lacc_elink_pref_copy() */


/*-------------------------------------------------------------------------
 * Function:       H5P_lacc_elink_pref_cmp
 *
 * Purpose:        Callback routine which is called whenever the elink prefix
 *                 property in the dataset creation property list is
 *                 compared.
 *
 * Return:         zero if VALUE1 and VALUE2 are equal, non zero otherwise.
 *
 * Programmer:     Mohamad Chaarawi
 *                 Thursday, November 3, 2011
 *
 *-------------------------------------------------------------------------
 */
static int
H5P_lacc_elink_pref_cmp(const void *value1, const void *value2, size_t UNUSED size)
{
    const char *pref1 = *(const char * const *)value1;
    const char *pref2 = *(const char * const *)value2;
    int ret_value = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(NULL == pref1 && NULL != pref2)
        HGOTO_DONE(1);
    if(NULL != pref1 && NULL == pref2)
        HGOTO_DONE(-1);
    if(NULL != pref1 && NULL != pref2)
        ret_value = HDstrcmp(pref1, pref2);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P_lacc_elink_pref_cmp() */


/*-------------------------------------------------------------------------
 * Function:    H5P_lacc_elink_pref_close
 *
 * Purpose:     Frees memory used to store the external link prefix string
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              November 2, 2006
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
static herr_t
H5P_lacc_elink_pref_close(const char UNUSED *name, size_t UNUSED size, void *value)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(value);

    H5MM_xfree(*(void **)value);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5P_lacc_elink_pref_close() */


/*-------------------------------------------------------------------------
 * Function:    H5Pset_nlinks
 *
 * Purpose:     Set the number of soft or UD link traversals allowed before
 *              the library assumes it has found a cycle and aborts the
 *              traversal.
 *
 *              The limit on soft or UD link traversals is designed to
 *              terminate link traversal if one or more links form a cycle.
 *              However, users may have a file with a legitimate path
 *              formed of a large number of soft or user-defined links.
 *              This property can be used to allow traversal of as many
 *              links as desired.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Friday, July 14, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_nlinks(hid_t plist_id, size_t nlinks)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "iz", plist_id, nlinks);

    if(nlinks <= 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "number of links must be positive");

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_LINK_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Set number of links */
    if(H5P_set(plist, H5L_ACS_NLINKS_NAME, &nlinks) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set nlink info")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_nlinks() */


/*-------------------------------------------------------------------------
 * Function:    H5Pget_nlinks
 *
 * Purpose:	Gets the number of soft or user-defined links that can be
 *              traversed before a failure occurs.
 *
 *              Retrieves the current setting for the nlinks property on
 *              the given property list.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Friday, July 14, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_nlinks(hid_t plist_id, size_t *nlinks)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*z", plist_id, nlinks);

    if(!nlinks)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid pointer passed in");

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_LINK_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get the current number of links */
    if(H5P_get(plist, H5L_ACS_NLINKS_NAME, nlinks) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get number of links")

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:    H5Pset_elink_prefix
 *
 * Purpose:     Set a prefix to be applied to the path of any external links
 *              traversed.  The prefix is appended to the filename stored
 *              in the external link.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Thursday, August 3, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_elink_prefix(hid_t plist_id, const char *prefix)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    char *my_prefix;                    /* Copy of prefix string */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*s", plist_id, prefix);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_LINK_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get current prefix value */
    if(H5P_get(plist, H5L_ACS_ELINK_PREFIX_NAME, &my_prefix) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get prefix info")

    /* Free existing prefix, if there is one */
    H5MM_xfree(my_prefix);

    /* Make a copy of the user's prefix string */
    if(NULL == (my_prefix = H5MM_xstrdup(prefix)))
        HGOTO_ERROR(H5E_PLIST, H5E_CANTCOPY, FAIL, "can't copy prefix")

    /* Set prefix */
    if(H5P_set(plist, H5L_ACS_ELINK_PREFIX_NAME, &my_prefix) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set prefix info")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_elink_prefix() */


/*-------------------------------------------------------------------------
 * Function:    H5Pget_elink_prefix
 *
 * Purpose:	Gets the prefix to be applied to any external link
 *              traversals made using this property list.
 *
 *              If the pointer is not NULL, it points to a user-allocated
 *              buffer.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Thursday, August 3, 2006
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5Pget_elink_prefix(hid_t plist_id, char *prefix, size_t size)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    char *my_prefix;                    /* Library's copy of the prefix */
    size_t	len;                    /* Length of prefix string */
    ssize_t 	ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("Zs", "i*sz", plist_id, prefix, size);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_LINK_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get the current prefix */
    if(H5P_get(plist, H5L_ACS_ELINK_PREFIX_NAME, &my_prefix) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get external link prefix")

    /* Check for prefix being set */
    if(my_prefix) {
        /* Copy to user's buffer, if given */
        len = HDstrlen(my_prefix);
        if(prefix) {
            HDstrncpy(prefix, my_prefix, MIN(len + 1, size));
            if(len >= size)
                prefix[size - 1] = '\0';
        } /* end if */
    } /* end if */
    else
        len = 0;

    /* Set return value */
    ret_value = (ssize_t)len;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_elink_prefix() */

/*-------------------------------------------------------------------------
 * Function:    H5Pset_elink_fapl
 *
 * Purpose:     Sets the file access property list for link access
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:
 *              Vailin Choi; Tuesday, September 12th, 2008
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_elink_fapl(hid_t lapl_id, hid_t fapl_id)
{
    H5P_genplist_t 	*plist, *fapl_plist;	/* Property list pointer */
    hid_t		l_fapl_id, new_fapl_id;
    herr_t 		ret_value = SUCCEED;         		/* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ii", lapl_id, fapl_id);

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(lapl_id, H5P_LINK_ACCESS)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a link access property list");

    /* Get the current file access property list for the link access */
    if(H5P_get(plist, H5L_ACS_ELINK_FAPL_NAME, &l_fapl_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get fapl")

    /* Close the current file access property list if set */
    if((l_fapl_id > H5P_DEFAULT) && (H5I_dec_ref(l_fapl_id) < 0))
	HGOTO_ERROR(H5E_PLIST, H5E_CANTRELEASE, FAIL, "unable to close atom for file access property list")

    if(NULL == (fapl_plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
	HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a file access property list");

    /* Make a copy of the property list for FAPL_ID */
    if((new_fapl_id = H5P_copy_plist(fapl_plist, FALSE)) < 0)
	HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, FAIL, "unable to copy file access properties")

    /* Set the file access property list for the link access */
    if(H5P_set(plist, H5L_ACS_ELINK_FAPL_NAME, &new_fapl_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set fapl for link")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_elink_fapl() */


/*-------------------------------------------------------------------------
 * Function:    H5Pget_elink_fapl
 *
 * Purpose:	Gets the file access property list identifier that is
 *		set for link access property.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:
 *              Vailin Choi; Tuesday, September 12th, 2008
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Pget_elink_fapl(hid_t lapl_id)
{
    H5P_genplist_t 	*plist, *fapl_plist; 	/* Property list pointer */
    hid_t		l_fapl_id;
    hid_t		ret_value=FAIL;		/* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("i", "i", lapl_id);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(lapl_id, H5P_LINK_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    if(H5P_get(plist, H5L_ACS_ELINK_FAPL_NAME, &l_fapl_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get fapl for links")

    if(l_fapl_id > H5P_DEFAULT) {
	if(NULL==(fapl_plist = H5P_object_verify(l_fapl_id, H5P_FILE_ACCESS)))
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list");

	if((ret_value = H5P_copy_plist(fapl_plist, TRUE)) < 0)
	    HGOTO_ERROR(H5E_INTERNAL, H5E_CANTINIT, FAIL, "unable to copy file access properties")
    } else
	ret_value = l_fapl_id;

done:
    FUNC_LEAVE_API(ret_value);
} /* end H5Pget_elink_fapl() */


/*-------------------------------------------------------------------------
 * Function:    H5Pset_elink_acc_flags
 *
 * Purpose:     Sets the file access flags to be used when traversing an
 *              external link.  This should be either H5F_ACC_RDONLY or
 *              H5F_ACC_RDWR, or H5F_ACC_DEFAULT to unset the value.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              Tuesday, December 9, 2008
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_elink_acc_flags(hid_t lapl_id, unsigned flags)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "iIu", lapl_id, flags);

    /* Check that flags are valid */
    if((flags != H5F_ACC_RDWR) && (flags != H5F_ACC_RDONLY) && (flags != H5F_ACC_DEFAULT))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid file open flags")

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(lapl_id, H5P_LINK_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Set flags */
    if(H5P_set(plist, H5L_ACS_ELINK_FLAGS_NAME, &flags) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set access flags")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_elink_acc_flags() */


/*-------------------------------------------------------------------------
 * Function:    H5Pget_elink_acc_flags
 *
 * Purpose:     Gets the file access flags to be used when traversing an
 *              external link.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              Tuesday, December 9, 2008
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_elink_acc_flags(hid_t lapl_id, unsigned *flags)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*Iu", lapl_id, flags);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(lapl_id, H5P_LINK_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get flags */
    if (flags)
        if(H5P_get(plist, H5L_ACS_ELINK_FLAGS_NAME, flags)<0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, 0, "can't get access flags")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_elink_acc_flags() */


/*-------------------------------------------------------------------------
 * Function:    H5Pset_elink_cb
 *
 * Purpose:     Sets the file access flags to be used when traversing an
 *              external link.  This should be either H5F_ACC_RDONLY or
 *              H5F_ACC_RDWR.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              Tuesday, December 15, 2008
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_elink_cb(hid_t lapl_id, H5L_elink_traverse_t func, void *op_data)
{
    H5P_genplist_t  *plist;                 /* Property list pointer */
    H5L_elink_cb_t  cb_info;                /* Callback info struct */
    herr_t          ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "ix*x", lapl_id, func, op_data);

    /* Check if the callback function is NULL and the user data is non-NULL.
     * This is almost certainly an error as the user data will not be used. */
    if(!func && op_data)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "callback is NULL while user data is not")

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(lapl_id, H5P_LINK_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Populate the callback info struct */
    cb_info.func = func;
    cb_info.user_data = op_data;

    /* Set callback info */
    if(H5P_set(plist, H5L_ACS_ELINK_CB_NAME, &cb_info) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set callback info")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_elink_acc_flags() */


/*-------------------------------------------------------------------------
 * Function:    H5Pget_elink_cb
 *
 * Purpose:     Gets the file access flags to be used when traversing an
 *              external link.
 *
 * Return:  Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              Tuesday, December 15, 2008
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_elink_cb(hid_t lapl_id, H5L_elink_traverse_t *func, void **op_data)
{
    H5P_genplist_t  *plist;                 /* Property list pointer */
    H5L_elink_cb_t  cb_info;                /* Callback info struct */
    herr_t          ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "i*x**x", lapl_id, func, op_data);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(lapl_id, H5P_LINK_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get callback_info */
    if(H5P_get(plist, H5L_ACS_ELINK_CB_NAME, &cb_info)<0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get callback info")

    if(func)
        *func = cb_info.func;

    if(op_data)
        *op_data = cb_info.user_data;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_elink_cb() */


