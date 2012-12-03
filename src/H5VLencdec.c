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

/* Programmer:  Mohamad Chaarawi <chaarawi@hdfgroup.org>
 *
 * Purpose:	Metadata server code
 */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5VL_init_encdec_interface

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Dprivate.h"		/* Datasets				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fprivate.h"		/* File access				*/
#include "H5FDmds.h"            /* MDS file driver      		*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5Sprivate.h" 	/* Dataspaces                      	*/
#include "H5Tprivate.h"		/* Datatypes				*/
#include "H5VLprivate.h"	/* VOL plugins				*/


/*--------------------------------------------------------------------------
NAME
   H5VL_init_encdec_interface -- Initialize interface-specific information
USAGE
    herr_t H5VL_init_encdec_interface()
RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.  (Just calls
    H5VL_init() currently).

--------------------------------------------------------------------------*/
static herr_t
H5VL_init_encdec_interface(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5VL_init_encdec_interface() */


/*-------------------------------------------------------------------------
 * Function:       H5VL__encode_loc_params
 *
 * Purpose:        Generic encoding callback routine for 'H5VL_loc_params_t'
 *
 * Return:	   Success:	Non-negative
 *		   Failure:	Negative
 *
 * Programmer:     Mohamad Chaarawi
 *                 August, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL__encode_loc_params(H5VL_loc_params_t loc_params, void *buf, size_t *nalloc)
{
    size_t plist_size = 0;
    H5P_genplist_t *plist = NULL;
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t len = 0;
    size_t size = 0;
    herr_t ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(nalloc);

    size += 2;
    switch(loc_params.type) {
        case H5VL_OBJECT_BY_SELF:
            break;
        case H5VL_OBJECT_BY_NAME:
            /* get length of name */
            len = HDstrlen(loc_params.loc_data.loc_by_name.name) + 1;

            /* get size of property list */
            if(H5P_DEFAULT != loc_params.loc_data.loc_by_name.plist_id) {
                if(NULL == (plist = (H5P_genplist_t *)H5I_object_verify(loc_params.loc_data.loc_by_name.plist_id, H5I_GENPROP_LST)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
                if(H5P__encode(plist, FALSE, NULL, &plist_size) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
            }
            size += (1 + H5V_limit_enc_size((uint64_t)len) + len + 
                     1 + H5V_limit_enc_size((uint64_t)plist_size) + plist_size);
            break;
        case H5VL_OBJECT_BY_IDX:
            /* get length of name */
            len = HDstrlen(loc_params.loc_data.loc_by_idx.name) + 1;

            /* get size of property list */
            if(H5P_DEFAULT != loc_params.loc_data.loc_by_idx.plist_id) {
                if(NULL == (plist = (H5P_genplist_t *)H5I_object_verify(loc_params.loc_data.loc_by_idx.plist_id, H5I_GENPROP_LST)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
                if(H5P__encode(plist, FALSE, NULL, &plist_size) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
            }
            size += (1 + H5V_limit_enc_size((uint64_t)len) + len + 2 + 
                     1 + H5V_limit_enc_size((uint64_t)plist_size) + plist_size + 
                     1 + H5V_limit_enc_size((uint64_t)loc_params.loc_data.loc_by_idx.n));
            break;
        case H5VL_OBJECT_BY_ADDR:
            size += 1 + H5V_limit_enc_size((uint64_t)loc_params.loc_data.loc_by_addr.addr);
            break;
        case H5VL_OBJECT_BY_REF:
            if(loc_params.loc_data.loc_by_ref.ref_type == H5R_DATASET_REGION)
                len = sizeof(hdset_reg_ref_t);
            else if (loc_params.loc_data.loc_by_ref.ref_type == H5R_OBJECT)
                len = sizeof(hobj_ref_t);

            /* get size of property list */
            if(H5P_DEFAULT != loc_params.loc_data.loc_by_ref.plist_id) {
                if(NULL == (plist = (H5P_genplist_t *)H5I_object_verify(loc_params.loc_data.loc_by_ref.plist_id, H5I_GENPROP_LST)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
                if(H5P__encode(plist, FALSE, NULL, &plist_size) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
            }

            size += 1 + 
                (1 + H5V_limit_enc_size((uint64_t)len) + len) + 
                (1 + H5V_limit_enc_size((uint64_t)plist_size) + plist_size);
            break;
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "invalid location type");
    } /* end switch */

    if(NULL != p) {
        /* Encode the size */
        *p++ = (uint8_t)loc_params.obj_type;
        *p++ = (uint8_t)loc_params.type;

        switch(loc_params.type) {
            case H5VL_OBJECT_BY_SELF:
                break;
            case H5VL_OBJECT_BY_NAME:
                {
                    /* encode length of name and name */
                    UINT64ENCODE_VARLEN(p, len);
                    HDstrcpy((char *)p, loc_params.loc_data.loc_by_name.name);
                    p += len;

                    /* encode the plist size */
                    UINT64ENCODE_VARLEN(p, plist_size);
                    /* encode property lists if they are not default*/
                    if(plist_size) {
                        if(H5P__encode(plist, FALSE, p, &plist_size) < 0)
                            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
                        p += plist_size;
                    }
                    break;
                }
            case H5VL_OBJECT_BY_IDX:
                {
                    /* encode length of name and name */
                    UINT64ENCODE_VARLEN(p, len);
                    HDstrcpy((char *)p, loc_params.loc_data.loc_by_idx.name);
                    p += len;

                    *p++ = (uint8_t)loc_params.loc_data.loc_by_idx.idx_type;
                    *p++ = (uint8_t)loc_params.loc_data.loc_by_idx.order;
                    UINT64ENCODE_VARLEN(p, loc_params.loc_data.loc_by_idx.n);

                    /* encode the plist size */
                    UINT64ENCODE_VARLEN(p, plist_size);
                    /* encode property lists if they are not default*/
                    if(plist_size) {
                        if(H5P__encode(plist, FALSE, p, &plist_size) < 0)
                            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
                        p += plist_size;
                    }
                    break;
                }
            case H5VL_OBJECT_BY_ADDR:
                {
                    UINT64ENCODE_VARLEN(p, loc_params.loc_data.loc_by_addr.addr);
                    break;
                }
            case H5VL_OBJECT_BY_REF:
                {
                    *p++ = (uint8_t)loc_params.loc_data.loc_by_ref.ref_type;

                    UINT64ENCODE_VARLEN(p, len);
                    if(len)
                        HDmemcpy(p, (const uint8_t *)loc_params.loc_data.loc_by_ref._ref, len);
                    p += len;

                    /* encode the plist size */
                    UINT64ENCODE_VARLEN(p, plist_size);
                    /* encode property lists if they are not default*/
                    if(plist_size) {
                        if(H5P__encode(plist, FALSE, p, &plist_size) < 0)
                            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
                        p += plist_size;
                    }
                    break;
                }
            default:
                HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "invalid location type");
        } /* end switch */
    } /* end if */

    *nalloc = size;
done:
    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL__encode_loc_params() */


/*-------------------------------------------------------------------------
 * Function:       H5VL__decode_loc_params
 *
 * Purpose:        Generic decoding callback routine for 'H5VL_loc_params_t'
 *
 * Return:	   Success:	Non-negative
 *		   Failure:	Negative
 *
 * Programmer:     Mohamad Chaarawi
 *                 August, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5VL__decode_loc_params(const void *buf, H5VL_loc_params_t *loc_params)
{
    size_t len = 0;
    const uint8_t *p = (const uint8_t *)buf;     /* Current pointer into buffer */
    size_t plist_size = 0;
    herr_t ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if(NULL == p)
        HGOTO_ERROR(H5E_VOL, H5E_BADVALUE, FAIL, "decode buffer is NULL")

    loc_params->obj_type = (H5I_type_t)*p++;
    loc_params->type = (H5VL_loc_type_t)*p++;

    switch(loc_params->type) {
        case H5VL_OBJECT_BY_SELF:
            /* nothing else to decode */
            break;

        case H5VL_OBJECT_BY_NAME:
            /* get length of name */
            UINT64DECODE_VARLEN(p, len);
            loc_params->loc_data.loc_by_name.name = H5MM_xstrdup((const char *)(p));
            p += len;

            /* decode the plist size */
            UINT64DECODE_VARLEN(p, plist_size);
            /* decode property lists if they are not default*/
            if(plist_size) {
                if((loc_params->loc_data.loc_by_name.plist_id = H5P__decode(p)) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
                p += plist_size;
            }
            else
                loc_params->loc_data.loc_by_name.plist_id = H5P_DEFAULT;
            break;

        case H5VL_OBJECT_BY_IDX:
            /* get length of name */
            UINT64DECODE_VARLEN(p, len);
            loc_params->loc_data.loc_by_idx.name = H5MM_xstrdup((const char *)(p));
            p += len;

            loc_params->loc_data.loc_by_idx.idx_type = (H5_index_t)*p++;
            loc_params->loc_data.loc_by_idx.order = (H5_iter_order_t)*p++;

            UINT64DECODE_VARLEN(p, loc_params->loc_data.loc_by_idx.n);

            /* decode the plist size */
            UINT64DECODE_VARLEN(p, plist_size);
            /* decode property lists if they are not default*/
            if(plist_size) {
                if((loc_params->loc_data.loc_by_idx.plist_id = H5P__decode(p)) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
                p += plist_size;
            }
            else
                loc_params->loc_data.loc_by_idx.plist_id = H5P_DEFAULT;
            break;

        case H5VL_OBJECT_BY_ADDR:
            UINT64DECODE_VARLEN(p, loc_params->loc_data.loc_by_addr.addr);
            break;

        case H5VL_OBJECT_BY_REF:
            loc_params->loc_data.loc_by_ref.ref_type = (H5R_type_t)*p++;

            UINT64DECODE_VARLEN(p, len);

            loc_params->loc_data.loc_by_ref._ref = HDmalloc(len);
            HDmemcpy(loc_params->loc_data.loc_by_ref._ref, p, len);
            p += len;

            /* decode the plist size */
            UINT64DECODE_VARLEN(p, plist_size);
            /* decode property lists if they are not default*/
            if(plist_size) {
                if((loc_params->loc_data.loc_by_ref.plist_id = H5P__decode(p)) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
                p += plist_size;
            }
            else
                loc_params->loc_data.loc_by_ref.plist_id = H5P_DEFAULT;
            break;

        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTDECODE, FAIL, "invalid location type");
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL__decode_loc_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__encode_file_create_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__encode_file_create_params(void *buf, size_t *nalloc, const char *name, unsigned flags, 
                                hid_t fcpl_id, hid_t fapl_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0;
    size_t len = 0, fcpl_size = 0, fapl_size = 0;
    H5P_genplist_t *fcpl = NULL, *fapl = NULL;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(nalloc);

    /* get property list sizes */
    if(H5P_FILE_CREATE_DEFAULT != fcpl_id) {
        if(NULL == (fcpl = (H5P_genplist_t *)H5I_object_verify(fcpl_id, H5I_GENPROP_LST)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
        if((ret_value = H5P__encode(fcpl, FALSE, NULL, &fcpl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }
    if(H5P_FILE_ACCESS_DEFAULT != fapl_id) {
        if(NULL == (fapl = (H5P_genplist_t *)H5I_object_verify(fapl_id, H5I_GENPROP_LST)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
        if((ret_value = H5P__encode(fapl, FALSE, NULL, &fapl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }

    len = HDstrlen(name) + 1;

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_FILE_CREATE;

        /* encode length of name and name */
        UINT64ENCODE_VARLEN(p, len);

        HDstrcpy((char *)p, name);
        p += len;

        /* encode create flags */
        H5_ENCODE_UNSIGNED(p, flags);

        /* encode the plist size */
        UINT64ENCODE_VARLEN(p, fcpl_size);
        /* encode property lists if they are not default*/
        if(fcpl_size) {
            if((ret_value = H5P__encode(fcpl, FALSE, p, &fcpl_size)) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
            p += fcpl_size;
        }

        /* encode the plist size */
        UINT64ENCODE_VARLEN(p, fapl_size);
        /* encode property lists if they are not default*/
        if(fapl_size) {
            if((ret_value = H5P__encode(fapl, FALSE, p, &fapl_size)) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
            p += fapl_size;
        }
    }
    size += (1 + 1 + H5V_limit_enc_size((uint64_t)len) + len + sizeof(unsigned) + 
             1 + H5V_limit_enc_size((uint64_t)fapl_size) + fapl_size + 
             1 + H5V_limit_enc_size((uint64_t)fcpl_size) + fcpl_size);

    *nalloc = size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__encode_file_create_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_file_create_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__decode_file_create_params(void *buf, char **name, unsigned *flags, 
                                hid_t *fcpl_id, hid_t *fapl_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t len = 0; /* length of name (decoded) */
    size_t fcpl_size = 0, fapl_size = 0; /* plist sizes */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* decode length of name and name */
    UINT64DECODE_VARLEN(p, len);
    *name = H5MM_xstrdup((const char *)(p));
    p += len;

    /* deocde create flags */
    H5_DECODE_UNSIGNED(p, *flags);

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, fcpl_size);
    /* decode property lists if they are not default*/
    if(fcpl_size) {
        if((*fcpl_id = H5P__decode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += fcpl_size;
    }
    else {
        *fcpl_id = H5P_FILE_CREATE_DEFAULT;
    }

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, fapl_size);
    /* decode property lists if they are not default*/
    if(fapl_size) {
        if((*fapl_id = H5P__decode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += fapl_size;
    }
    else {
        *fapl_id = H5P_FILE_ACCESS_DEFAULT;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__decode_file_create_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__encode_file_open_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__encode_file_open_params(void *buf, size_t *nalloc, const char *name, unsigned flags, hid_t fapl_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0;
    size_t len = 0, fapl_size = 0;
    H5P_genplist_t *fapl = NULL;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(nalloc);

    /* get property list sizes */
    if(H5P_FILE_ACCESS_DEFAULT != fapl_id) {
        if(NULL == (fapl = (H5P_genplist_t *)H5I_object_verify(fapl_id, H5I_GENPROP_LST)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
        if((ret_value = H5P__encode(fapl, FALSE, NULL, &fapl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }

    len = HDstrlen(name) + 1;

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_FILE_OPEN;

        /* encode length of name and name */
        UINT64ENCODE_VARLEN(p, len);
        HDstrcpy((char *)p, name);
        p += len;

        /* encode open flags */
        H5_ENCODE_UNSIGNED(p, flags);

        /* encode the plist size */
        UINT64ENCODE_VARLEN(p, fapl_size);
        /* encode property lists if they are not default*/
        if(fapl_size) {
            if((ret_value = H5P__encode(fapl, FALSE, p, &fapl_size)) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
            p += fapl_size;
        }
    }
    size += (1 + sizeof(unsigned) +
             1 + H5V_limit_enc_size((uint64_t)len) + len +  
             1 + H5V_limit_enc_size((uint64_t)fapl_size) + fapl_size);

    *nalloc = size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__encode_file_open_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_file_open_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__decode_file_open_params(void *buf, char **name, unsigned *flags, hid_t *fapl_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t len = 0; /* length of name (decoded) */
    size_t fapl_size = 0; /* plist sizes */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* decode length of name and name */
    UINT64DECODE_VARLEN(p, len);
    *name = H5MM_xstrdup((const char *)(p));
    p += len;

    /* deocde open flags */
    H5_DECODE_UNSIGNED(p, *flags);

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, fapl_size);
    /* decode property lists if they are not default*/
    if(fapl_size) {
        if((*fapl_id = H5P__decode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += fapl_size;
    }
    else {
        *fapl_id = H5P_FILE_ACCESS_DEFAULT;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__decode_file_open_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__encode_file_flush_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__encode_file_flush_params(void *buf, size_t *nalloc, hid_t obj_id, H5VL_loc_params_t loc_params,
                               H5F_scope_t scope)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t loc_size = 0;
    size_t size = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(nalloc);

    /* get loc params size to encode */
    if((ret_value = H5VL__encode_loc_params(loc_params, NULL, &loc_size)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_FILE_FLUSH;

        /* encode the object id */
        INT32ENCODE(p, obj_id);

        UINT64ENCODE_VARLEN(p, loc_size);
        /* encode the location parameters */
        if((ret_value = H5VL__encode_loc_params(loc_params, p, &loc_size)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");                    
        p += loc_size;

        /* encode scope */
        *p++ = (uint8_t)scope;
    }

    size = 2 + sizeof(int) + 
        1 + H5V_limit_enc_size((uint64_t)loc_size) + loc_size;
    *nalloc = size;

done:
    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL__encode_file_flush_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_file_flush_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__decode_file_flush_params(void *buf, hid_t *obj_id, H5VL_loc_params_t *loc_params,
                               H5F_scope_t *scope)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t loc_size = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* the metadata file id */
    INT32DECODE(p, *obj_id);

    UINT64DECODE_VARLEN(p, loc_size);
    /* decode the location parameters */
    if((ret_value = H5VL__decode_loc_params(p, loc_params)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTDECODE, FAIL, "unable to decode VOL location param");
    p += loc_size;

    /* decode the scope */
    *scope = (H5F_scope_t)*p++;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__decode_file_flush_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_file_misc_encode__params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__encode_file_misc_params(void *buf, size_t *nalloc, H5VL_file_misc_t misc_type, ...)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0;
    va_list arguments;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(nalloc);

    va_start (arguments, misc_type);
    switch (misc_type) {
        case H5VL_FILE_MOUNT:
            {
                hid_t obj_id           = va_arg (arguments, hid_t);
                H5I_type_t type        = va_arg (arguments, H5I_type_t);
                const char *name       = va_arg (arguments, const char *);
                hid_t child            = va_arg (arguments, hid_t);
                hid_t plist_id         = va_arg (arguments, hid_t);
                H5P_genplist_t *plist = NULL;
                size_t len = 0, plist_size = 0;

                /* get name size to encode */
                if(NULL != name)
                    len = HDstrlen(name) + 1;

                /* get size for property lists to encode */
                if(H5P_FILE_MOUNT_DEFAULT != plist_id) {
                    if(NULL == (plist = (H5P_genplist_t *)H5I_object_verify(plist_id, H5I_GENPROP_LST)))
                        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
                    if((ret_value = H5P__encode(plist, FALSE, NULL, &plist_size)) < 0)
                        HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
                }

                size += 3 + 2*sizeof(int32_t) + 
                    1 + H5V_limit_enc_size((uint64_t)plist_size) + plist_size + 
                    1 + H5V_limit_enc_size((uint64_t)len) + len;

                if(NULL != p) {
                    /* encode request type */
                    *p++ = (uint8_t)H5VL_FILE_MISC;
                    /* encode get type */
                    *p++ = (uint8_t)misc_type;

                    /* encode the object id */
                    INT32ENCODE(p, obj_id);

                    /* encode object type */
                    *p++ = (uint8_t)type;

                    /* encode length of the name and the actual name */
                    UINT64ENCODE_VARLEN(p, len);
                    if(NULL != name)
                        HDstrcpy((char *)p, name);
                    p += len;

                    /* encode the child id */
                    INT32ENCODE(p, child);

                    /* encode the plist size */
                    UINT64ENCODE_VARLEN(p, plist_size);
                    /* encode property lists if they are not default*/
                    if(plist_size) {
                        if((ret_value = H5P__encode(plist, FALSE, p, &plist_size)) < 0)
                            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
                        p += plist_size;
                    }
                }
                break;
            }
        case H5VL_FILE_UNMOUNT:
            {
                hid_t obj_id           = va_arg (arguments, hid_t);
                H5I_type_t type        = va_arg (arguments, H5I_type_t);
                const char *name       = va_arg (arguments, const char *);
                size_t len = 0;

                /* get name size to encode */
                if(NULL != name)
                    len = HDstrlen(name) + 1;

                size += 3 + sizeof(int32_t) + 
                    1 + H5V_limit_enc_size((uint64_t)len) + len;

                if(NULL != p) {
                    /* encode request type */
                    *p++ = (uint8_t)H5VL_FILE_MISC;
                    /* encode get type */
                    *p++ = (uint8_t)misc_type;

                    /* encode the object id */
                    INT32ENCODE(p, obj_id);

                    /* encode object type */
                    *p++ = (uint8_t)type;

                    /* encode length of the name and the actual name */
                    UINT64ENCODE_VARLEN(p, len);
                    if(NULL != name)
                        HDstrcpy((char *)p, name);
                    p += len;
                }
                break;
            }
        case H5VL_FILE_IS_ACCESSIBLE:
            {
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from attr");
    }
    va_end (arguments);

    *nalloc = size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__encode_file_misc_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_file_misc_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__decode_file_misc_params(void *buf, H5VL_file_misc_t misc_type, ...)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    va_list arguments;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    va_start (arguments, misc_type);
    switch (misc_type) {
        case H5VL_FILE_MOUNT:
            {
                hid_t *obj_id = va_arg (arguments, hid_t *);
                H5I_type_t *type = va_arg (arguments, H5I_type_t *);
                char **name = va_arg (arguments, char **);
                hid_t *child = va_arg (arguments, hid_t *);
                hid_t *plist_id = va_arg (arguments, hid_t *);
                size_t len = 0, plist_size = 0;

                /* decode the object id */
                INT32DECODE(p, *obj_id);

                *type = (H5I_type_t)*p++;

                /* decode length of the name and the actual name */
                UINT64DECODE_VARLEN(p, len);
                if(0 != len) {
                    *name = H5MM_xstrdup((const char *)(p));
                    p += len;
                }

                /* decode the child id */
                INT32DECODE(p, *child);

                /* decode the plist size */
                UINT64DECODE_VARLEN(p, plist_size);
                /* decode property list if not default*/
                if(plist_size) {
                    if((*plist_id = H5P__decode(p)) < 0)
                        HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
                    p += plist_size;
                }
                else {
                    *plist_id = H5P_FILE_MOUNT_DEFAULT;
                }
                break;
            }
        case H5VL_FILE_UNMOUNT:
            {
                hid_t *obj_id = va_arg (arguments, hid_t *);
                H5I_type_t *type = va_arg (arguments, H5I_type_t *);
                char **name = va_arg (arguments, char **);
                size_t len = 0;

                /* decode the object id */
                INT32DECODE(p, *obj_id);

                *type = (H5I_type_t)*p++;

                /* decode length of the name and the actual name */
                UINT64DECODE_VARLEN(p, len);
                if(0 != len) {
                    *name = H5MM_xstrdup((const char *)(p));
                    p += len;
                }
                break;
            }
        case H5VL_FILE_IS_ACCESSIBLE:
            {
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from attr");
    }
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__decode_file_misc_params() */
#if 0
/*-------------------------------------------------------------------------
 * Function:	H5VL_file_optional_encode__params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__encode_file_optional_params(void *buf, size_t *nalloc, H5VL_file_optional_t optional_type, ...)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0;
    va_list arguments;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(nalloc);

    va_start (arguments, optional_type);
    switch (optional_type) {
        case H5VL_FILE_GET_FREE_SPACE:
        case H5VL_FILE_GET_MDC_CONF:
        case H5VL_FILE_GET_MDC_HR:
        case H5VL_FILE_GET_MDC_SIZE:
        case H5VL_FILE_CLEAR_ELINK_CACHE:
        case H5VL_FILE_RESET_MDC_HIT_RATE:
            {
                hid_t *obj_id = va_arg (arguments, hid_t *);

                INT32DECODE(p, *obj_id);

                break;
            }
        case H5VL_FILE_GET_FREE_SECTIONS:
            {
                hid_t *obj_id = va_arg (arguments, hid_t *);
                H5F_mem_t *type = va_arg (arguments, H5F_mem_t *);
                size_t *nsects = va_arg (arguments, size_t *);
                uint8_t *flag = va_arg (arguments, uint8_t *);

                INT32DECODE(p, *obj_id);
                *type = (H5F_mem_t)*p++;
                UINT64DECODE_VARLEN(p, *nsects);
                *flag = *p++;

                break;
            }
        case H5VL_FILE_GET_INFO:
            {
                hid_t *obj_id = va_arg (arguments, hid_t *);
                H5I_type_t *type = va_arg (arguments, H5I_type_t *);

                INT32DECODE(p, *obj_id);
                *type = (H5I_type_t)*p++;

                break;
            }
        /* H5Freopen */
        case H5VL_FILE_REOPEN:
            {
                break;
            }
        case H5VL_FILE_SET_MDC_CONFIG:
            {
                H5AC_cache_config_t config_ptr;

                break;
            }
        case H5VL_FILE_GET_VFD_HANDLE:
        case H5VL_FILE_GET_SIZE:
        case H5VL_FILE_GET_FILE_IMAGE:

        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't encode this routine");
    }
    va_end (arguments);

    *nalloc = size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__encode_file_optional_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_file_optional_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__decode_file_optional_params(void *buf, H5VL_file_optional_t optional_type, ...)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    va_list arguments;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    va_start (arguments, optional_type);
    switch (optional_type) {
        case H5VL_FILE_GET_FREE_SPACE:
        case H5VL_FILE_GET_MDC_CONF:
        case H5VL_FILE_GET_MDC_HR:
        case H5VL_FILE_GET_MDC_SIZE:
        case H5VL_FILE_CLEAR_ELINK_CACHE:
        case H5VL_FILE_RESET_MDC_HIT_RATE:
            {
                hid_t *obj_id = va_arg (arguments, hid_t *);

                INT32DECODE(p, *obj_id);

                break;
            }
        case H5VL_FILE_GET_FREE_SECTIONS:
            {
                hid_t *obj_id = va_arg (arguments, hid_t *);
                H5F_mem_t *type = va_arg (arguments, H5F_mem_t *);
                size_t *nsects = va_arg (arguments, size_t *);
                uint8_t *flag = va_arg (arguments, uint8_t *);

                INT32DECODE(p, *obj_id);
                *type = (H5F_mem_t)*p++;
                UINT64DECODE_VARLEN(p, *nsects);
                *flag = *p++;

                break;
            }
        case H5VL_FILE_GET_INFO:
            {
                hid_t *obj_id = va_arg (arguments, hid_t *);
                H5I_type_t *type = va_arg (arguments, H5I_type_t *);

                INT32DECODE(p, *obj_id);
                *type = (H5I_type_t)*p++;

                break;
            }
        /* H5Freopen */
        case H5VL_FILE_REOPEN:
            {
                break;
            }
        case H5VL_FILE_SET_MDC_CONFIG:
            {
                H5AC_cache_config_t config_ptr;

                break;
            }
        case H5VL_FILE_GET_VFD_HANDLE:
        case H5VL_FILE_GET_SIZE:
        case H5VL_FILE_GET_FILE_IMAGE:
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't decode params for this routine");
    }
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__decode_file_optional_params() */
#endif

/*-------------------------------------------------------------------------
 * Function:	H5VL__encode_file_close_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__encode_file_close_params(void *buf, size_t *nalloc, hid_t obj_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(nalloc);

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_FILE_CLOSE;

        /* encode the object id */
        INT32ENCODE(p, obj_id);
    }

    size = 1 + sizeof(int);
    *nalloc = size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL__encode_file_close_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_file_close_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__decode_file_close_params(void *buf, hid_t *file_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* the metadata file id */
    INT32DECODE(p, *file_id);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL__decode_file_close_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__encode_attr_create_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__encode_attr_create_params(void *buf, size_t *nalloc, hid_t obj_id, 
                                H5VL_loc_params_t loc_params, const char *name, hid_t acpl_id,
                                hid_t UNUSED aapl_id, hid_t type_id, hid_t space_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0;
    size_t len = 0, acpl_size = 0;
    size_t type_size = 0, space_size = 0, loc_size = 0;
    H5P_genplist_t *acpl = NULL;
    H5S_t *dspace = NULL;
    H5T_t *dtype = NULL;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(nalloc);

    /* get size for property lists to encode */
    if(H5P_ATTRIBUTE_CREATE_DEFAULT != acpl_id) {
        if(NULL == (acpl = (H5P_genplist_t *)H5I_object_verify(acpl_id, H5I_GENPROP_LST)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
        if((ret_value = H5P__encode(acpl, FALSE, NULL, &acpl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }

    /* get Type size to encode */
    if(NULL == (dtype = (H5T_t *)H5I_object_verify(type_id, H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype");
    if(H5T_encode(dtype, NULL, &type_size) < 0)
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "can't encode datatype");

    /* get Dataspace size to encode */
    if (NULL==(dspace=(H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataspace")
    if(H5S_encode(dspace, NULL, &space_size)<0)
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "can't encode datatype")

    /* get name size to encode */
    if(NULL != name)
        len = HDstrlen(name) + 1;

    /* get loc params size to encode */
    if((ret_value = H5VL__encode_loc_params(loc_params, NULL, &loc_size)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_ATTR_CREATE;

        /* encode the object id */
        INT32ENCODE(p, obj_id);

        UINT64ENCODE_VARLEN(p, loc_size);
        /* encode the location parameters */
        if((ret_value = H5VL__encode_loc_params(loc_params, p, &loc_size)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");                    
        p += loc_size;

        /* encode length of the attr name and the actual attr name */
        UINT64ENCODE_VARLEN(p, len);
        if(NULL != name)
            HDstrcpy((char *)p, name);
        p += len;

        /* encode the plist size */
        UINT64ENCODE_VARLEN(p, acpl_size);
        /* encode property lists if they are not default*/
        if(acpl_size) {
            if((ret_value = H5P__encode(acpl, FALSE, p, &acpl_size)) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
            p += acpl_size;
        }

        /* encode the datatype size */
        UINT64ENCODE_VARLEN(p, type_size);
        /* encode datatype */
        if((ret_value = H5T_encode(dtype, p, &type_size)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "unable to encode datatype");
        p += type_size;

        /* encode the dataspace size */
        UINT64ENCODE_VARLEN(p, space_size);
        /* encode datatspace */
        if((ret_value = H5S_encode(dspace, p, &space_size)) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "unable to encode datatspace");
        p += space_size;
    }
    size += (1 + sizeof(int32_t) + 
             1 + H5V_limit_enc_size((uint64_t)loc_size) + loc_size + 
             1 + H5V_limit_enc_size((uint64_t)len) + len + 
             1 + H5V_limit_enc_size((uint64_t)acpl_size) + acpl_size + 
             1 + H5V_limit_enc_size((uint64_t)type_size) + type_size + 
             1 + H5V_limit_enc_size((uint64_t)space_size) + space_size);


    *nalloc = size;

 done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__encode_attr_create_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_attr_create_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__decode_attr_create_params(void *buf, hid_t *obj_id, H5VL_loc_params_t *loc_params, 
                                char **name, hid_t *acpl_id, hid_t UNUSED *aapl_id, 
                                hid_t *type_id, hid_t *space_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t len = 0; /* len of attr name */
    size_t acpl_size = 0, type_size = 0, space_size = 0, loc_size = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* decode the object id */
    INT32DECODE(p, *obj_id);

    UINT64DECODE_VARLEN(p, loc_size);
    /* decode the location parameters */
    if((ret_value = H5VL__decode_loc_params(p, loc_params)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTDECODE, FAIL, "unable to decode VOL location param");
    p += loc_size;

    /* decode length of the attr name and the actual attr name */
    UINT64DECODE_VARLEN(p, len);
    if(0 != len) {
        *name = H5MM_xstrdup((const char *)(p));
        p += len;
    }

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, acpl_size);
    /* decode property lists if they are not default*/
    if(acpl_size) {
        if((*acpl_id = H5P__decode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += acpl_size;
    }
    else {
        *acpl_id = H5P_ATTRIBUTE_CREATE_DEFAULT;
    }

    /* decode the type size */
    UINT64DECODE_VARLEN(p, type_size);
    {
        H5T_t *dt;
        /* Create datatype by decoding buffer */
        if(NULL == (dt = H5T_decode((const unsigned char *)p)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDECODE, FAIL, "can't decode object");
        /* Register the type and return the ID */
        if((*type_id = H5I_register(H5I_DATATYPE, dt, FALSE)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, FAIL, "unable to register data type");
    }
    p += type_size;

    /* decode the space size */
    UINT64DECODE_VARLEN(p, space_size);
    /* decode the dataspace */
    {
        H5S_t *ds = NULL;
        if((ds = H5S_decode((const unsigned char *)p)) == NULL)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDECODE, FAIL, "can't decode object");
        /* Register the type and return the ID */
        if((*space_id = H5I_register(H5I_DATASPACE, ds, FALSE)) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTREGISTER, FAIL, "unable to register dataspace");
    }
    p += space_size;

 done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__decode_attr_create_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_attr_open_encode__params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__encode_attr_open_params(void *buf, size_t *nalloc, hid_t obj_id, H5VL_loc_params_t loc_params,
                              const char *name, hid_t UNUSED aapl_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0;
    size_t len = 0, loc_size = 0;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(nalloc);

    /* get name size to encode */
    if(NULL != name)
        len = HDstrlen(name) + 1;

    /* get loc params size to encode */
    if((ret_value = H5VL__encode_loc_params(loc_params, NULL, &loc_size)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_ATTR_OPEN;

        /* encode the object id */
        INT32ENCODE(p, obj_id);

        UINT64ENCODE_VARLEN(p, loc_size);
        /* encode the location parameters */
        if((ret_value = H5VL__encode_loc_params(loc_params, p, &loc_size)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");                    
        p += loc_size;

        /* encode length of the attr name and the actual attr name */
        UINT64ENCODE_VARLEN(p, len);
        if(NULL != name && len)
            HDstrcpy((char *)p, name);
        p += len;
    }
    size += (1 + sizeof(int32_t) + 
             1 + H5V_limit_enc_size((uint64_t)loc_size) + loc_size + 
             1 + H5V_limit_enc_size((uint64_t)len) + len);

    *nalloc = size;
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__encode_attr_open_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_attr_open_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__decode_attr_open_params(void *buf, hid_t *obj_id, H5VL_loc_params_t *loc_params, 
                              char **name, hid_t UNUSED *aapl_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t len = 0; /* len of attr name */
    size_t loc_size = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* decode the object id */
    INT32DECODE(p, *obj_id);

    UINT64DECODE_VARLEN(p, loc_size);

    /* decode the location parameters */
    if((ret_value = H5VL__decode_loc_params(p, loc_params)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTDECODE, FAIL, "unable to decode VOL location param");
    p += loc_size;

    /* decode length of the attr name and the actual attr name */
    UINT64DECODE_VARLEN(p, len);

    if(0 != len) {
        *name = H5MM_strdup((const char *)(p));
        p += len;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__decode_attr_open_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__encode_attr_read_params
 *------------------------------------------------------------------------- */
H5_DLL herr_t 
H5VL__encode_attr_read_params(void *buf, size_t *nalloc, hid_t obj_id, hid_t type_id, size_t buf_size)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0, type_size;
    H5T_t *dtype = NULL;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(nalloc);

    /* get Type size to encode */
    if(NULL == (dtype = (H5T_t *)H5I_object_verify(type_id, H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype");
    if(H5T_encode(dtype, NULL, &type_size) < 0)
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "can't encode datatype");

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_ATTR_READ;

        /* encode the object id */
        INT32ENCODE(p, obj_id);

        /* encode the datatype size */
        UINT64ENCODE_VARLEN(p, type_size);
        /* encode datatype */
        if((ret_value = H5T_encode(dtype, p, &type_size)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "unable to encode datatype");
        p += type_size;

        UINT64ENCODE_VARLEN(p, buf_size);
    }
    size += (1 + sizeof(int32_t) + 
             1 + H5V_limit_enc_size((uint64_t)type_size) + type_size + 
             1 + H5V_limit_enc_size((uint64_t)buf_size));

    *nalloc = size;
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__encode_attr_read_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_attr_read_params
 *------------------------------------------------------------------------- */
H5_DLL herr_t 
H5VL__decode_attr_read_params(void *buf, hid_t *obj_id, hid_t *type_id, size_t *buf_size)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t type_size = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* decode the object id */
    INT32DECODE(p, *obj_id);

    /* decode the type size */
    UINT64DECODE_VARLEN(p, type_size);
    /* decode the datatype */
    {
        H5T_t *dt;
        /* Create datatype by decoding buffer */
        if(NULL == (dt = H5T_decode((const unsigned char *)p)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDECODE, FAIL, "can't decode object");
        /* Register the type and return the ID */
        if((*type_id = H5I_register(H5I_DATATYPE, dt, FALSE)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, FAIL, "unable to register data type");
    }
    p += type_size;

    /* decode the read buffer size */
    UINT64DECODE_VARLEN(p, *buf_size);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__decode_attr_read_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__encode_attr_write_params
 *------------------------------------------------------------------------- */
H5_DLL herr_t 
H5VL__encode_attr_write_params(void *buf, size_t *nalloc, hid_t obj_id, hid_t type_id, 
                               const void *attr_buf, size_t buf_size)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0, type_size;
    H5T_t *dtype = NULL;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(nalloc);

    /* get Type size to encode */
    if(NULL == (dtype = (H5T_t *)H5I_object_verify(type_id, H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype");
    if(H5T_encode(dtype, NULL, &type_size) < 0)
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "can't encode datatype");

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_ATTR_WRITE;

        /* encode the object id */
        INT32ENCODE(p, obj_id);

        /* encode the datatype size */
        UINT64ENCODE_VARLEN(p, type_size);
        /* encode datatype */
        if((ret_value = H5T_encode(dtype, p, &type_size)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "unable to encode datatype");
        p += type_size;

        /* encode the size of the data */
        UINT64ENCODE_VARLEN(p, buf_size);
        /* encode the data */
        HDmemcpy(p, (const uint8_t *)attr_buf, buf_size);
    }
    size += (1 + sizeof(int32_t) + 
             1 + H5V_limit_enc_size((uint64_t)type_size) + type_size + 
             1 + H5V_limit_enc_size((uint64_t)buf_size) + buf_size);

    *nalloc = size;
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__encode_attr_read_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_attr_write_params
 *------------------------------------------------------------------------- */
H5_DLL herr_t 
H5VL__decode_attr_write_params(void *buf, hid_t *obj_id, hid_t *type_id, 
                               void **attr_buf, size_t *buf_size)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t type_size = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* decode the object id */
    INT32DECODE(p, *obj_id);

    /* decode the type size */
    UINT64DECODE_VARLEN(p, type_size);
    /* decode the datatype */
    {
        H5T_t *dt;
        /* Create datatype by decoding buffer */
        if(NULL == (dt = H5T_decode((const unsigned char *)p)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDECODE, FAIL, "can't decode object");
        /* Register the type and return the ID */
        if((*type_id = H5I_register(H5I_DATATYPE, dt, FALSE)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, FAIL, "unable to register data type");
    }
    p += type_size;

    /* decode the write buffer size */
    UINT64DECODE_VARLEN(p, *buf_size);
    *attr_buf = p;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__decode_attr_write_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_attr_get_encode__params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__encode_attr_get_params(void *buf, size_t *nalloc, H5VL_attr_get_t get_type, ...)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0;
    va_list arguments;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(nalloc);

    va_start (arguments, get_type);
    switch (get_type) {
        case H5VL_ATTR_EXISTS:
            {
                hid_t obj_id = va_arg (arguments, hid_t);
                H5VL_loc_params_t loc_params = va_arg (arguments, H5VL_loc_params_t);
                char *name = va_arg (arguments, char *);
                size_t len = 0, loc_size = 0;

                /* get name size to encode */
                if(NULL != name)
                    len = HDstrlen(name) + 1;

                /* get loc params size to encode */
                if((ret_value = H5VL__encode_loc_params(loc_params, NULL, &loc_size)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");

                size += 2 + sizeof(int32_t) + 
                    1 + H5V_limit_enc_size((uint64_t)loc_size) + loc_size + 
                    1 + H5V_limit_enc_size((uint64_t)len) + len;

                if(NULL != p) {
                    /* encode request type */
                    *p++ = (uint8_t)H5VL_ATTR_GET;
                    /* encode get type */
                    *p++ = (uint8_t)get_type;

                    /* encode the object id */
                    INT32ENCODE(p, obj_id);

                    UINT64ENCODE_VARLEN(p, loc_size);
                    /* encode the location parameters */
                    if((ret_value = H5VL__encode_loc_params(loc_params, p, &loc_size)) < 0)
                        HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");
                    p += loc_size;

                    /* encode length of the attr name and the actual attr name */
                    UINT64ENCODE_VARLEN(p, len);
                    if(NULL != name)
                        HDstrcpy((char *)p, name);
                    p += len;
                }
                break;
            }
        case H5VL_ATTR_GET_NAME:
            {
                hid_t obj_id = va_arg (arguments, hid_t);
                H5VL_loc_params_t loc_params = va_arg (arguments, H5VL_loc_params_t);
                size_t buf_size = va_arg (arguments, size_t);
                size_t loc_size = 0;

                /* get loc params size to encode */
                if((ret_value = H5VL__encode_loc_params(loc_params, NULL, &loc_size)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");

                size += 2 + sizeof(int32_t) + 
                    1 + H5V_limit_enc_size((uint64_t)loc_size) + loc_size + 
                    1 + H5V_limit_enc_size((uint64_t)buf_size);

                if(NULL != p) {
                    /* encode request type */
                    *p++ = (uint8_t)H5VL_ATTR_GET;
                    /* encode get type */
                    *p++ = (uint8_t)get_type;

                    /* encode the object id */
                    INT32ENCODE(p, obj_id);

                    UINT64ENCODE_VARLEN(p, loc_size);
                    /* encode the location parameters */
                    if((ret_value = H5VL__encode_loc_params(loc_params, p, &loc_size)) < 0)
                        HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");
                    p += loc_size;

                    /* encode length of the attr name and the actual attr name */
                    UINT64ENCODE_VARLEN(p, buf_size);
                }
                break;
            }
        case H5VL_ATTR_GET_INFO:
            {
                hid_t obj_id = va_arg (arguments, hid_t);
                H5VL_loc_params_t loc_params = va_arg (arguments, H5VL_loc_params_t);
                size_t loc_size = 0, len = 0;
                char *name = NULL;

                /* get loc params size to encode */
                if((ret_value = H5VL__encode_loc_params(loc_params, NULL, &loc_size)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");

                size += 2 + sizeof(int32_t) + 
                    1 + H5V_limit_enc_size((uint64_t)loc_size) + loc_size;

                if(H5VL_OBJECT_BY_NAME == loc_params.type) {
                    name = va_arg (arguments, char *);
                    /* get name size to encode */
                    if(NULL != name)
                        len = HDstrlen(name) + 1;
                    size += 1 + H5V_limit_enc_size((uint64_t)len) + len;
                }
                if(NULL != p) {
                    /* encode request type */
                    *p++ = (uint8_t)H5VL_ATTR_GET;
                    /* encode get type */
                    *p++ = (uint8_t)get_type;

                    /* encode the object id */
                    INT32ENCODE(p, obj_id);

                    UINT64ENCODE_VARLEN(p, loc_size);
                    /* encode the location parameters */
                    if((ret_value = H5VL__encode_loc_params(loc_params, p, &loc_size)) < 0)
                        HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");
                    p += loc_size;

                    if(H5VL_OBJECT_BY_NAME == loc_params.type) {
                        /* encode length of the attr name and the actual attr name */
                        UINT64ENCODE_VARLEN(p, len);
                        if(NULL != name)
                            HDstrcpy((char *)p, name);
                        p += len;
                    }
                }
                break;
            }
        case H5VL_ATTR_GET_SPACE:
        case H5VL_ATTR_GET_TYPE:
        case H5VL_ATTR_GET_ACPL:
        case H5VL_ATTR_GET_STORAGE_SIZE:
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from attr");
    }
    va_end (arguments);

    *nalloc = size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__encode_attr_get_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_attr_get_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__decode_attr_get_params(void *buf, H5VL_attr_get_t get_type, ...)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    va_list arguments;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    va_start (arguments, get_type);
    switch (get_type) {
        case H5VL_ATTR_EXISTS:
            {
                hid_t *obj_id = va_arg (arguments, hid_t *);
                H5VL_loc_params_t *loc_params = va_arg (arguments, H5VL_loc_params_t *);
                char **name = va_arg (arguments, char **);
                size_t len = 0, loc_size = 0;

                /* decode the object id */
                INT32DECODE(p, *obj_id);

                UINT64DECODE_VARLEN(p, loc_size);
                /* decode the location parameters */
                if((ret_value = H5VL__decode_loc_params(p, loc_params)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTDECODE, FAIL, "unable to decode VOL location param");
                p += loc_size;

                /* decode length of the attr name and the actual attr name */
                UINT64DECODE_VARLEN(p, len);
                if(0 != len) {
                    *name = H5MM_xstrdup((const char *)(p));
                    p += len;
                }
                break;
            }
        case H5VL_ATTR_GET_NAME:
            {
                hid_t *obj_id = va_arg (arguments, hid_t *);
                H5VL_loc_params_t *loc_params = va_arg (arguments, H5VL_loc_params_t *);
                size_t *size =  va_arg (arguments, size_t*);
                size_t loc_size = 0;

                /* decode the object id */
                INT32DECODE(p, *obj_id);

                UINT64DECODE_VARLEN(p, loc_size);
                /* decode the location parameters */
                if((ret_value = H5VL__decode_loc_params(p, loc_params)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTDECODE, FAIL, "unable to decode VOL location param");
                p += loc_size;

                UINT64DECODE_VARLEN(p, *size);

                break;
            }
        case H5VL_ATTR_GET_INFO:
            {
                hid_t *obj_id = va_arg (arguments, hid_t *);
                H5VL_loc_params_t *loc_params = va_arg (arguments, H5VL_loc_params_t *);
                size_t loc_size = 0;

                /* decode the object id */
                INT32DECODE(p, *obj_id);

                UINT64DECODE_VARLEN(p, loc_size);
                /* decode the location parameters */
                if((ret_value = H5VL__decode_loc_params(p, loc_params)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTDECODE, FAIL, "unable to decode VOL location param");
                p += loc_size;

                if(H5VL_OBJECT_BY_NAME == loc_params->type) {
                    char **name = va_arg (arguments, char **);
                    size_t len = 0;

                    /* decode length of the attr name and the actual attr name */
                    UINT64DECODE_VARLEN(p, len);
                    if(0 != len) {
                        *name = H5MM_xstrdup((const char *)(p));
                        p += len;
                    }
                }
                break;
            }
        case H5VL_ATTR_GET_SPACE:
        case H5VL_ATTR_GET_TYPE:
        case H5VL_ATTR_GET_ACPL:
        case H5VL_ATTR_GET_STORAGE_SIZE:
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from attr");
    }
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__decode_attr_get_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_attr_remove_encode__params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__encode_attr_remove_params(void *buf, size_t *nalloc, hid_t obj_id, H5VL_loc_params_t loc_params,
                                const char *name)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0;
    size_t len = 0, loc_size = 0;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(nalloc);

    /* get name size to encode */
    if(NULL != name)
        len = HDstrlen(name) + 1;

    /* get loc params size to encode */
    if((ret_value = H5VL__encode_loc_params(loc_params, NULL, &loc_size)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_ATTR_REMOVE;

        /* encode the object id */
        INT32ENCODE(p, obj_id);

        UINT64ENCODE_VARLEN(p, loc_size);
        /* encode the location parameters */
        if((ret_value = H5VL__encode_loc_params(loc_params, p, &loc_size)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");                    
        p += loc_size;

        /* encode length of the attr name and the actual attr name */
        UINT64ENCODE_VARLEN(p, len);
        if(NULL != name)
            HDstrcpy((char *)p, name);
        p += len;
    }
    size += (1 + sizeof(int32_t) + 
             1 + H5V_limit_enc_size((uint64_t)loc_size) + loc_size + 
             1 + H5V_limit_enc_size((uint64_t)len) + len);

    *nalloc = size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__encode_attr_remove_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_attr_remove_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__decode_attr_remove_params(void *buf, hid_t *obj_id, H5VL_loc_params_t *loc_params, char **name)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t len = 0; /* len of attr name */
    size_t loc_size = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* decode the object id */
    INT32DECODE(p, *obj_id);

    UINT64DECODE_VARLEN(p, loc_size);
    /* decode the location parameters */
    if((ret_value = H5VL__decode_loc_params(p, loc_params)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTDECODE, FAIL, "unable to decode VOL location param");
    p += loc_size;

    /* decode length of the attr name and the actual attr name */
    UINT64DECODE_VARLEN(p, len);
    if(0 != len) {
        *name = H5MM_xstrdup((const char *)(p));
        p += len;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__decode_attr_remove_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__encode_attr_close_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__encode_attr_close_params(void *buf, size_t *nalloc, hid_t attr_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(nalloc);

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_ATTR_CLOSE;

        /* encode the object id */
        INT32ENCODE(p, attr_id);
    }

    size = 1 + sizeof(int);
    *nalloc = size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL__encode_attr_close_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_attr_close_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__decode_attr_close_params(void *buf, hid_t *attr_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* the metadata attr id */
    INT32DECODE(p, *attr_id);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL__decode_attr_close_params() */


/*-------------------------------------------------------------------------
 * Function:	H5VL__encode_dataset_create_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__encode_dataset_create_params(void *buf, size_t *nalloc, hid_t obj_id, 
                                   H5VL_loc_params_t loc_params, const char *name, hid_t dcpl_id,
                                   hid_t dapl_id, hid_t type_id, hid_t space_id, hid_t lcpl_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0;
    size_t len = 0, dcpl_size = 0, dapl_size = 0, lcpl_size = 0;
    size_t type_size = 0, space_size = 0, loc_size = 0;
    H5P_genplist_t *dcpl = NULL, *dapl = NULL, *lcpl = NULL;
    H5S_t *dspace = NULL;
    H5T_t *dtype = NULL;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(nalloc);

    /* get size for property lists to encode */
    if(H5P_DATASET_CREATE_DEFAULT != dcpl_id) {
        if(NULL == (dcpl = (H5P_genplist_t *)H5I_object_verify(dcpl_id, H5I_GENPROP_LST)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
        if((ret_value = H5P__encode(dcpl, FALSE, NULL, &dcpl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }
    if(H5P_DATASET_ACCESS_DEFAULT != dapl_id) {
        if(NULL == (dapl = (H5P_genplist_t *)H5I_object_verify(dapl_id, H5I_GENPROP_LST)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
        if((ret_value = H5P__encode(dapl, FALSE, NULL, &dapl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }
    if(H5P_LINK_CREATE_DEFAULT != lcpl_id) {
        if(NULL == (lcpl = (H5P_genplist_t *)H5I_object_verify(lcpl_id, H5I_GENPROP_LST)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
        if((ret_value = H5P__encode(lcpl, FALSE, NULL, &lcpl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }

    /* get Type size to encode */
    if(NULL == (dtype = (H5T_t *)H5I_object_verify(type_id, H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype");
    if(H5T_encode(dtype, NULL, &type_size) < 0)
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "can't encode datatype");

    /* get Dataspace size to encode */
    if (NULL==(dspace=(H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataspace")
    if(H5S_encode(dspace, NULL, &space_size)<0)
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "can't encode datatype")

    /* get name size to encode */
    if(NULL != name)
        len = HDstrlen(name) + 1;

    /* get loc params size to encode */
    if((ret_value = H5VL__encode_loc_params(loc_params, NULL, &loc_size)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_DSET_CREATE;

        /* encode the object id */
        INT32ENCODE(p, obj_id);

        UINT64ENCODE_VARLEN(p, loc_size);
        /* encode the location parameters */
        if((ret_value = H5VL__encode_loc_params(loc_params, p, &loc_size)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");
        p += loc_size;

        /* encode length of the dataset name and the actual dataset name */
        UINT64ENCODE_VARLEN(p, len);
        if(NULL != name)
            HDstrcpy((char *)p, name);
        p += len;

        /* encode the plist size */
        UINT64ENCODE_VARLEN(p, dcpl_size);
        /* encode property lists if they are not default*/
        if(dcpl_size) {
            if((ret_value = H5P__encode(dcpl, FALSE, p, &dcpl_size)) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
            p += dcpl_size;
        }

        /* encode the plist size */
        UINT64ENCODE_VARLEN(p, dapl_size);
        /* encode property lists if they are not default*/
        if(dapl_size) {
            if((ret_value = H5P__encode(dapl, FALSE, p, &dapl_size)) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
            p += dapl_size;
        }

        /* encode the plist size */
        UINT64ENCODE_VARLEN(p, lcpl_size);
        /* encode property lists if they are not default*/
        if(H5P_LINK_CREATE_DEFAULT != lcpl_id) {
            if((ret_value = H5P__encode(lcpl, FALSE, p, &lcpl_size)) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
            p += lcpl_size;
        }

        /* encode the datatype size */
        UINT64ENCODE_VARLEN(p, type_size);
        /* encode datatype */
        if((ret_value = H5T_encode(dtype, p, &type_size)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "unable to encode datatype");
        p += type_size;

        /* encode the dataspace size */
        UINT64ENCODE_VARLEN(p, space_size);
        /* encode datatspace */
        if((ret_value = H5S_encode(dspace, p, &space_size)) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "unable to encode datatspace");
        p += space_size;
    }
    size += (1 + sizeof(int32_t) + 
             1 + H5V_limit_enc_size((uint64_t)loc_size) + loc_size + 
             1 + H5V_limit_enc_size((uint64_t)len) + len + 
             1 + H5V_limit_enc_size((uint64_t)dapl_size) + dapl_size + 
             1 + H5V_limit_enc_size((uint64_t)dcpl_size) + dcpl_size + 
             1 + H5V_limit_enc_size((uint64_t)lcpl_size) + lcpl_size + 
             1 + H5V_limit_enc_size((uint64_t)type_size) + type_size + 
             1 + H5V_limit_enc_size((uint64_t)space_size) + space_size);


    *nalloc = size;

 done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__encode_dataset_create_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_dataset_create_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__decode_dataset_create_params(void *buf, hid_t *obj_id, H5VL_loc_params_t *loc_params, 
                                   char **name, hid_t *dcpl_id, hid_t *dapl_id, 
                                   hid_t *type_id, hid_t *space_id, hid_t *lcpl_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t len = 0; /* len of dataset name */
    size_t dcpl_size = 0, dapl_size = 0, lcpl_size = 0, type_size = 0, space_size = 0, loc_size = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* decode the object id */
    INT32DECODE(p, *obj_id);

    UINT64DECODE_VARLEN(p, loc_size);
    /* decode the location parameters */
    if((ret_value = H5VL__decode_loc_params(p, loc_params)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTDECODE, FAIL, "unable to decode VOL location param");
    p += loc_size;

    /* decode length of the dataset name and the actual dataset name */
    UINT64DECODE_VARLEN(p, len);
    if(0 != len) {
        *name = H5MM_xstrdup((const char *)(p));
        p += len;
    }

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, dcpl_size);
    /* decode property lists if they are not default*/
    if(dcpl_size) {
        if((*dcpl_id = H5P__decode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += dcpl_size;
    }
    else {
        *dcpl_id = H5P_DATASET_CREATE_DEFAULT;
    }

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, dapl_size);
    /* decode property lists if they are not default*/
    if(dapl_size) {
        if((*dapl_id = H5P__decode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += dapl_size;
    }
    else {
        *dapl_id = H5P_DATASET_ACCESS_DEFAULT;
    }

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, lcpl_size);
    /* decode property lists if they are not default*/
    if(lcpl_size) {
        if((*lcpl_id = H5P__decode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += lcpl_size;
    }
    else {
        *lcpl_id = H5P_LINK_CREATE_DEFAULT;
    }

    /* decode the type size */
    UINT64DECODE_VARLEN(p, type_size);
    /* decode the datatype */
    {
        H5T_t *dt;
        /* Create datatype by decoding buffer */
        if(NULL == (dt = H5T_decode((const unsigned char *)p)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDECODE, FAIL, "can't decode object");
        /* Register the type and return the ID */
        if((*type_id = H5I_register(H5I_DATATYPE, dt, FALSE)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, FAIL, "unable to register data type");
    }
    p += type_size;

    /* decode the space size */
    UINT64DECODE_VARLEN(p, space_size);
    /* decode the dataspace */
    {
        H5S_t *ds = NULL;
        if((ds = H5S_decode((const unsigned char *)p)) == NULL)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDECODE, FAIL, "can't decode object");
        /* Register the type and return the ID */
        if((*space_id = H5I_register(H5I_DATASPACE, ds, FALSE)) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTREGISTER, FAIL, "unable to register dataspace");
    }
    p += space_size;

 done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__decode_dataset_create_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_dataset_open_encode__params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__encode_dataset_open_params(void *buf, size_t *nalloc, hid_t obj_id, H5VL_loc_params_t loc_params,
                                 const char *name, hid_t dapl_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0;
    size_t len = 0, dapl_size = 0, loc_size = 0;
    H5P_genplist_t *dapl = NULL;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(nalloc);

    /* get size for property lists to encode */
    if(H5P_DATASET_ACCESS_DEFAULT != dapl_id) {
        if(NULL == (dapl = (H5P_genplist_t *)H5I_object_verify(dapl_id, H5I_GENPROP_LST)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
        if((ret_value = H5P__encode(dapl, FALSE, NULL, &dapl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }

    /* get name size to encode */
    if(NULL != name)
        len = HDstrlen(name) + 1;

    /* get loc params size to encode */
    if((ret_value = H5VL__encode_loc_params(loc_params, NULL, &loc_size)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_DSET_OPEN;

        /* encode the object id */
        INT32ENCODE(p, obj_id);

        UINT64ENCODE_VARLEN(p, loc_size);
        /* encode the location parameters */
        if((ret_value = H5VL__encode_loc_params(loc_params, p, &loc_size)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");                    
        p += loc_size;

        /* encode length of the dataset name and the actual dataset name */
        UINT64ENCODE_VARLEN(p, len);
        if(NULL != name)
            HDstrcpy((char *)p, name);
        p += len;

        /* encode the plist size */
        UINT64ENCODE_VARLEN(p, dapl_size);
        /* encode property lists if they are not default*/
        if(dapl_size) {
            if((ret_value = H5P__encode(dapl, FALSE, p, &dapl_size)) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
            p += dapl_size;
        }
    }
    size += (1 + sizeof(int32_t) + 
             1 + H5V_limit_enc_size((uint64_t)loc_size) + loc_size + 
             1 + H5V_limit_enc_size((uint64_t)len) + len + 
             1 + H5V_limit_enc_size((uint64_t)dapl_size) + dapl_size);

    *nalloc = size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__encode_dataset_open_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_dataset_open_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__decode_dataset_open_params(void *buf, hid_t *obj_id, H5VL_loc_params_t *loc_params, 
                                 char **name, hid_t *dapl_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t len = 0; /* len of dataset name */
    size_t dapl_size = 0, loc_size = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* decode the object id */
    INT32DECODE(p, *obj_id);

    UINT64DECODE_VARLEN(p, loc_size);
    /* decode the location parameters */
    if((ret_value = H5VL__decode_loc_params(p, loc_params)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTDECODE, FAIL, "unable to decode VOL location param");
    p += loc_size;

    /* decode length of the dataset name and the actual dataset name */
    UINT64DECODE_VARLEN(p, len);
    if(0 != len) {
        *name = H5MM_xstrdup((const char *)(p));
        p += len;
    }

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, dapl_size);
    /* decode property lists if they are not default*/
    if(dapl_size) {
        if((*dapl_id = H5P__decode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += dapl_size;
    }
    else {
        *dapl_id = H5P_DATASET_ACCESS_DEFAULT;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__decode_dataset_open_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_dataset_set_extent_encode__params
 *------------------------------------------------------------------------- */
H5_DLL herr_t 
H5VL__encode_dataset_set_extent_params(void *buf, size_t *nalloc, hid_t obj_id, 
                                       int rank, const hsize_t *esize)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0;
    int i;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(nalloc);

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_DSET_SET_EXTENT;

        /* encode the object id */
        INT32ENCODE(p, obj_id);

        /* encode the rank */
        INT32ENCODE(p, rank);

        for(i=0 ; i<rank ; i++)
            UINT64ENCODE_VARLEN(p, esize[i])
    }
    size += 1 + sizeof(int32_t) * 2;
    for(i=0 ; i<rank ; i++)
        size += 1 + H5V_limit_enc_size((uint64_t)esize[i]);

    *nalloc = size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL__decode_dataset_set_extent_params() */
/*-------------------------------------------------------------------------
 * Function:	H5VL_dataset_set_extent_encode__params
 *------------------------------------------------------------------------- */
H5_DLL herr_t 
H5VL__decode_dataset_set_extent_params(void *buf, hid_t *obj_id, int *rank, hsize_t **size)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    int i;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* decode the object id */
    INT32DECODE(p, *obj_id);

    /* decode the rank */
    INT32DECODE(p, *rank);

    if(NULL == (*size = (hsize_t *)H5MM_malloc(sizeof(hsize_t) * (*rank))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    for(i=0 ; i<*rank ; i++)
        UINT64DECODE_VARLEN(p, (*size)[i])

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__decode_dataset_set_extent_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__encode_dataset_close_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__encode_dataset_close_params(void *buf, size_t *nalloc, hid_t dataset_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(nalloc);

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_DSET_CLOSE;

        /* encode the object id */
        INT32ENCODE(p, dataset_id);
    }

    size = 1 + sizeof(int);
    *nalloc = size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL__encode_dataset_close_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_dataset_close_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__decode_dataset_close_params(void *buf, hid_t *dataset_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* the metadata dataset id */
    INT32DECODE(p, *dataset_id);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL__decode_dataset_close_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__encode_datatype_commit_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__encode_datatype_commit_params(void *buf, size_t *nalloc, hid_t obj_id, 
                                    H5VL_loc_params_t loc_params, const char *name, hid_t type_id,
                                    hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0;
    size_t len = 0, tcpl_size = 0, tapl_size = 0, lcpl_size = 0;
    size_t type_size = 0, loc_size = 0;
    H5P_genplist_t *tcpl = NULL, *tapl = NULL, *lcpl = NULL;
    H5T_t *dtype = NULL;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(nalloc);

    /* get size for property lists to encode */
    if(H5P_DATATYPE_CREATE_DEFAULT != tcpl_id) {
        if(NULL == (tcpl = (H5P_genplist_t *)H5I_object_verify(tcpl_id, H5I_GENPROP_LST)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
        if((ret_value = H5P__encode(tcpl, FALSE, NULL, &tcpl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }
    if(H5P_DATATYPE_ACCESS_DEFAULT != tapl_id) {
        if(NULL == (tapl = (H5P_genplist_t *)H5I_object_verify(tapl_id, H5I_GENPROP_LST)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
        if((ret_value = H5P__encode(tapl, FALSE, NULL, &tapl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }
    if(H5P_LINK_CREATE_DEFAULT != lcpl_id) {
        if(NULL == (lcpl = (H5P_genplist_t *)H5I_object_verify(lcpl_id, H5I_GENPROP_LST)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
        if((ret_value = H5P__encode(lcpl, FALSE, NULL, &lcpl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }

    /* get Type size to encode */
    if(NULL == (dtype = (H5T_t *)H5I_object_verify(type_id, H5I_DATATYPE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype");
    if(H5T_encode(dtype, NULL, &type_size) < 0)
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "can't encode datatype");

    /* get name size to encode */
    if(NULL != name)
        len = HDstrlen(name) + 1;

    /* get loc params size to encode */
    if((ret_value = H5VL__encode_loc_params(loc_params, NULL, &loc_size)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_DTYPE_COMMIT;

        /* encode the object id */
        INT32ENCODE(p, obj_id);

        UINT64ENCODE_VARLEN(p, loc_size);
        /* encode the location parameters */
        if((ret_value = H5VL__encode_loc_params(loc_params, p, &loc_size)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");                    
        p += loc_size;

        /* encode length of the datatype name and the actual datatype name */
        UINT64ENCODE_VARLEN(p, len);
        if(NULL != name)
            HDstrcpy((char *)p, name);
        p += len;

        /* encode the datatype size */
        UINT64ENCODE_VARLEN(p, type_size);
        /* encode datatype */
        if((ret_value = H5T_encode(dtype, p, &type_size)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "unable to encode datatype");
        p += type_size;

        /* encode the plist size */
        UINT64ENCODE_VARLEN(p, lcpl_size);
        /* encode property lists if they are not default*/
        if(lcpl_size) {
            if((ret_value = H5P__encode(lcpl, FALSE, p, &lcpl_size)) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
            p += lcpl_size;
        }

        /* encode the plist size */
        UINT64ENCODE_VARLEN(p, tcpl_size);
        /* encode property lists if they are not default*/
        if(tcpl_size) {
            if((ret_value = H5P__encode(tcpl, FALSE, p, &tcpl_size)) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
            p += tcpl_size;
        }

        /* encode the plist size */
        UINT64ENCODE_VARLEN(p, tapl_size);
        /* encode property lists if they are not default*/
        if(tapl_size) {
            if((ret_value = H5P__encode(tapl, FALSE, p, &tapl_size)) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
            p += tapl_size;
        }
    }
    size += (1 + sizeof(int32_t) + 
             1 + H5V_limit_enc_size((uint64_t)loc_size) + loc_size + 
             1 + H5V_limit_enc_size((uint64_t)len) + len + 
             1 + H5V_limit_enc_size((uint64_t)tapl_size) + tapl_size + 
             1 + H5V_limit_enc_size((uint64_t)tcpl_size) + tcpl_size + 
             1 + H5V_limit_enc_size((uint64_t)lcpl_size) + lcpl_size + 
             1 + H5V_limit_enc_size((uint64_t)type_size) + type_size);

    *nalloc = size;

 done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__encode_datatype_commit_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_datatype_commit_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__decode_datatype_commit_params(void *buf, hid_t *obj_id, H5VL_loc_params_t *loc_params, 
                                    char **name, hid_t *type_id, hid_t *lcpl_id, 
                                    hid_t *tcpl_id, hid_t *tapl_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t len = 0; /* len of dataset name */
    size_t tcpl_size = 0, tapl_size = 0, lcpl_size = 0, type_size = 0, loc_size = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* decode the object id */
    INT32DECODE(p, *obj_id);

    UINT64DECODE_VARLEN(p, loc_size);
    /* decode the location parameters */
    if((ret_value = H5VL__decode_loc_params(p, loc_params)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTDECODE, FAIL, "unable to decode VOL location param");
    p += loc_size;

    /* decode length of the dataset name and the actual dataset name */
    UINT64DECODE_VARLEN(p, len);
    if(0 != len) {
        *name = H5MM_xstrdup((const char *)(p));
        p += len;
    }

    /* decode the type size */
    UINT64DECODE_VARLEN(p, type_size);
    /* decode the datatype */
    {
        H5T_t *dt;
        /* Create datatype by decoding buffer */
        if(NULL == (dt = H5T_decode((const unsigned char *)p)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDECODE, FAIL, "can't decode object");
        /* Register the type and return the ID */
        if((*type_id = H5I_register(H5I_DATATYPE, dt, FALSE)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, FAIL, "unable to register data type");
    }
    p += type_size;

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, lcpl_size);
    /* decode property lists if they are not default*/
    if(lcpl_size) {
        if((*lcpl_id = H5P__decode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += lcpl_size;
    }
    else {
        *lcpl_id = H5P_LINK_CREATE_DEFAULT;
    }

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, tcpl_size);
    /* decode property lists if they are not default*/
    if(tcpl_size) {
        if((*tcpl_id = H5P__decode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += tcpl_size;
    }
    else {
        *tcpl_id = H5P_DATATYPE_CREATE_DEFAULT;
    }

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, tapl_size);
    /* decode property lists if they are not default*/
    if(tapl_size) {
        if((*tapl_id = H5P__decode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += tapl_size;
    }
    else {
        *tapl_id = H5P_DATATYPE_ACCESS_DEFAULT;
    }

 done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__decode_datatype_commit_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__encode_datatype_open_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__encode_datatype_open_params(void *buf, size_t *nalloc, hid_t obj_id, 
                                  H5VL_loc_params_t loc_params, const char *name, hid_t tapl_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0;
    size_t len = 0, tapl_size = 0, loc_size = 0;
    H5P_genplist_t *tapl = NULL;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(nalloc);

    /* get size for property lists to encode */
    if(H5P_DATATYPE_ACCESS_DEFAULT != tapl_id) {
        if(NULL == (tapl = (H5P_genplist_t *)H5I_object_verify(tapl_id, H5I_GENPROP_LST)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
        if((ret_value = H5P__encode(tapl, FALSE, NULL, &tapl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }

    /* get name size to encode */
    if(NULL != name)
        len = HDstrlen(name) + 1;

    /* get loc params size to encode */
    if((ret_value = H5VL__encode_loc_params(loc_params, NULL, &loc_size)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_DTYPE_OPEN;

        /* encode the object id */
        INT32ENCODE(p, obj_id);

        UINT64ENCODE_VARLEN(p, loc_size);
        /* encode the location parameters */
        if((ret_value = H5VL__encode_loc_params(loc_params, p, &loc_size)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");                    
        p += loc_size;

        /* encode length of the datatype name and the actual datatype name */
        UINT64ENCODE_VARLEN(p, len);
        if(NULL != name)
            HDstrcpy((char *)p, name);
        p += len;

        /* encode the plist size */
        UINT64ENCODE_VARLEN(p, tapl_size);
        /* encode property lists if they are not default*/
        if(H5P_DATATYPE_ACCESS_DEFAULT != tapl_id) {
            if((ret_value = H5P__encode(tapl, FALSE, p, &tapl_size)) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
            p += tapl_size;
        }
    }
    size += (1 + sizeof(int32_t) + 
             1 + H5V_limit_enc_size((uint64_t)loc_size) + loc_size + 
             1 + H5V_limit_enc_size((uint64_t)len) + len + 
             1 + H5V_limit_enc_size((uint64_t)tapl_size) + tapl_size);

    *nalloc = size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__encode_datatype_open_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_datatype_open_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__decode_datatype_open_params(void *buf, hid_t *obj_id, H5VL_loc_params_t *loc_params, 
                                  char **name, hid_t *tapl_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t len = 0; /* len of dataset name */
    size_t tapl_size = 0, loc_size = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* decode the object id */
    INT32DECODE(p, *obj_id);

    UINT64DECODE_VARLEN(p, loc_size);
    /* decode the location parameters */
    if((ret_value = H5VL__decode_loc_params(p, loc_params)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTDECODE, FAIL, "unable to decode VOL location param");
    p += loc_size;

    /* decode length of the dataset name and the actual dataset name */
    UINT64DECODE_VARLEN(p, len);
    if(0 != len) {
        *name = H5MM_xstrdup((const char *)(p));
        p += len;
    }

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, tapl_size);
    /* decode property lists if they are not default*/
    if(tapl_size) {
        if((*tapl_id = H5P__decode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += tapl_size;
    }
    else {
        *tapl_id = H5P_DATATYPE_ACCESS_DEFAULT;
    }

 done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__decode_datatype_open_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__encode_datatype_close_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__encode_datatype_close_params(void *buf, size_t *nalloc, hid_t datatype_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(nalloc);

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_DTYPE_CLOSE;

        /* encode the object id */
        INT32ENCODE(p, datatype_id);
    }

    size = 1 + sizeof(int);
    *nalloc = size;

    FUNC_LEAVE_NOAPI(SUCCEED);
} /* end H5VL__encode_datatype_close_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_datatype_close_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__decode_datatype_close_params(void *buf, hid_t *datatype_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* the metadata datatype id */
    INT32DECODE(p, *datatype_id);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL__decode_datatype_close_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__encode_group_create_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__encode_group_create_params(void *buf, size_t *nalloc, hid_t obj_id, 
                                 H5VL_loc_params_t loc_params, const char *name, hid_t gcpl_id,
                                 hid_t gapl_id, hid_t lcpl_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0;
    size_t len = 0, gcpl_size = 0, gapl_size = 0, lcpl_size = 0, loc_size = 0;
    H5P_genplist_t *gcpl = NULL, *gapl = NULL, *lcpl = NULL;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(nalloc);

    /* get size for property lists to encode */
    if(H5P_GROUP_CREATE_DEFAULT != gcpl_id) {
        if(NULL == (gcpl = (H5P_genplist_t *)H5I_object_verify(gcpl_id, H5I_GENPROP_LST)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
        if((ret_value = H5P__encode(gcpl, FALSE, NULL, &gcpl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }
    if(H5P_GROUP_ACCESS_DEFAULT != gapl_id) {
        if(NULL == (gapl = (H5P_genplist_t *)H5I_object_verify(gapl_id, H5I_GENPROP_LST)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
        if((ret_value = H5P__encode(gapl, FALSE, NULL, &gapl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }
    if(H5P_LINK_CREATE_DEFAULT != lcpl_id) {
        if(NULL == (lcpl = (H5P_genplist_t *)H5I_object_verify(lcpl_id, H5I_GENPROP_LST)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
        if((ret_value = H5P__encode(lcpl, FALSE, NULL, &lcpl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }

    /* get name size to encode */
    if(NULL != name)
        len = HDstrlen(name) + 1;

    /* get loc params size to encode */
    if((ret_value = H5VL__encode_loc_params(loc_params, NULL, &loc_size)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_GROUP_CREATE;

        /* encode the object id */
        INT32ENCODE(p, obj_id);

        UINT64ENCODE_VARLEN(p, loc_size);
        /* encode the location parameters */
        if((ret_value = H5VL__encode_loc_params(loc_params, p, &loc_size)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");                    
        p += loc_size;

        /* encode length of the group name and the actual group name */
        UINT64ENCODE_VARLEN(p, len);
        if(NULL != name)
            HDstrcpy((char *)p, name);
        p += len;

        /* encode the plist size */
        UINT64ENCODE_VARLEN(p, gcpl_size);
        /* encode property lists if they are not default*/
        if(gcpl_size) {
            if((ret_value = H5P__encode(gcpl, FALSE, p, &gcpl_size)) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
            p += gcpl_size;
        }

        /* encode the plist size */
        UINT64ENCODE_VARLEN(p, gapl_size);
        /* encode property lists if they are not default*/
        if(gapl_size) {
            if((ret_value = H5P__encode(gapl, FALSE, p, &gapl_size)) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
            p += gapl_size;
        }

        /* encode the plist size */
        UINT64ENCODE_VARLEN(p, lcpl_size);
        /* encode property lists if they are not default*/
        if(H5P_LINK_CREATE_DEFAULT != lcpl_id) {
            if((ret_value = H5P__encode(lcpl, FALSE, p, &lcpl_size)) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
            p += lcpl_size;
        }
    }
    size += (1 + sizeof(int32_t) + 
             1 + H5V_limit_enc_size((uint64_t)loc_size) + loc_size + 
             1 + H5V_limit_enc_size((uint64_t)len) + len + 
             1 + H5V_limit_enc_size((uint64_t)gapl_size) + gapl_size + 
             1 + H5V_limit_enc_size((uint64_t)gcpl_size) + gcpl_size + 
             1 + H5V_limit_enc_size((uint64_t)lcpl_size) + lcpl_size);

    *nalloc = size;

 done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__encode_group_create_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_group_create_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__decode_group_create_params(void *buf, hid_t *obj_id, H5VL_loc_params_t *loc_params, 
                                 char **name, hid_t *gcpl_id, hid_t *gapl_id, hid_t *lcpl_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t len = 0; /* len of group name */
    size_t gcpl_size = 0, gapl_size = 0, lcpl_size = 0, loc_size = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* decode the object id */
    INT32DECODE(p, *obj_id);

    UINT64DECODE_VARLEN(p, loc_size);
    /* decode the location parameters */
    if((ret_value = H5VL__decode_loc_params(p, loc_params)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTDECODE, FAIL, "unable to decode VOL location param");
    p += loc_size;

    /* decode length of the group name and the actual group name */
    UINT64DECODE_VARLEN(p, len);
    if(0 != len) {
        *name = H5MM_xstrdup((const char *)(p));
        p += len;
    }

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, gcpl_size);
    /* decode property lists if they are not default*/
    if(gcpl_size) {
        if((*gcpl_id = H5P__decode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += gcpl_size;
    }
    else {
        *gcpl_id = H5P_GROUP_CREATE_DEFAULT;
    }

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, gapl_size);
    /* decode property lists if they are not default*/
    if(gapl_size) {
        if((*gapl_id = H5P__decode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += gapl_size;
    }
    else {
        *gapl_id = H5P_GROUP_ACCESS_DEFAULT;
    }

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, lcpl_size);
    /* decode property lists if they are not default*/
    if(lcpl_size) {
        if((*lcpl_id = H5P__decode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += lcpl_size;
    }
    else {
        *lcpl_id = H5P_LINK_CREATE_DEFAULT;
    }
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__decode_group_create_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__encode_group_open_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__encode_group_open_params(void *buf, size_t *nalloc, hid_t obj_id, 
                               H5VL_loc_params_t loc_params, const char *name, hid_t gapl_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0;
    size_t len = 0, gapl_size = 0, loc_size = 0;
    H5P_genplist_t *gapl = NULL;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(nalloc);

    /* get size for property lists to encode */
    if(H5P_GROUP_ACCESS_DEFAULT != gapl_id) {
        if(NULL == (gapl = (H5P_genplist_t *)H5I_object_verify(gapl_id, H5I_GENPROP_LST)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
        if((ret_value = H5P__encode(gapl, FALSE, NULL, &gapl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }

    /* get name size to encode */
    if(NULL != name)
        len = HDstrlen(name) + 1;

    /* get loc params size to encode */
    if((ret_value = H5VL__encode_loc_params(loc_params, NULL, &loc_size)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_GROUP_OPEN;

        /* encode the object id */
        INT32ENCODE(p, obj_id);

        UINT64ENCODE_VARLEN(p, loc_size);
        /* encode the location parameters */
        if((ret_value = H5VL__encode_loc_params(loc_params, p, &loc_size)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");                    
        p += loc_size;

        /* encode length of the group name and the actual group name */
        UINT64ENCODE_VARLEN(p, len);
        if(NULL != name)
            HDstrcpy((char *)p, name);
        p += len;

        /* encode the plist size */
        UINT64ENCODE_VARLEN(p, gapl_size);
        /* encode property lists if they are not default*/
        if(H5P_GROUP_ACCESS_DEFAULT != gapl_id) {
            if((ret_value = H5P__encode(gapl, FALSE, p, &gapl_size)) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
            p += gapl_size;
        }
    }
    size += (1 + sizeof(int32_t) + 
             1 + H5V_limit_enc_size((uint64_t)loc_size) + loc_size + 
             1 + H5V_limit_enc_size((uint64_t)len) + len + 
             1 + H5V_limit_enc_size((uint64_t)gapl_size) + gapl_size);

    *nalloc = size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__encode_group_open_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_group_open_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__decode_group_open_params(void *buf, hid_t *obj_id, H5VL_loc_params_t *loc_params, 
                                  char **name, hid_t *gapl_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t len = 0; /* len of dataset name */
    size_t gapl_size = 0, loc_size = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* decode the object id */
    INT32DECODE(p, *obj_id);

    UINT64DECODE_VARLEN(p, loc_size);
    /* decode the location parameters */
    if((ret_value = H5VL__decode_loc_params(p, loc_params)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTDECODE, FAIL, "unable to decode VOL location param");
    p += loc_size;

    /* decode length of the dataset name and the actual dataset name */
    UINT64DECODE_VARLEN(p, len);
    if(0 != len) {
        *name = H5MM_xstrdup((const char *)(p));
        p += len;
    }

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, gapl_size);
    /* decode property lists if they are not default*/
    if(gapl_size) {
        if((*gapl_id = H5P__decode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += gapl_size;
    }
    else {
        *gapl_id = H5P_GROUP_ACCESS_DEFAULT;
    }

 done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__decode_group_open_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__encode_group_get_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__encode_group_get_params(void *buf, size_t *nalloc, H5VL_group_get_t get_type, ...)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0;
    va_list arguments;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(nalloc);

    va_start (arguments, get_type);
    switch (get_type) {
        case H5VL_GROUP_GET_GCPL:
            {
                hid_t obj_id = va_arg (arguments, hid_t);

                size += 2 + sizeof(int32_t);

                if(NULL != p) {
                    /* encode request type */
                    *p++ = (uint8_t)H5VL_GROUP_GET;
                    /* encode get type */
                    *p++ = (uint8_t)get_type;
                    /* encode the object id */
                    INT32ENCODE(p, obj_id);
                }
                break;
            }
        case H5VL_GROUP_GET_INFO:
            {
                hid_t obj_id = va_arg (arguments, hid_t);
                H5VL_loc_params_t loc_params = va_arg (arguments, H5VL_loc_params_t);
                size_t loc_size = 0;

                /* get loc params size to encode */
                if((ret_value = H5VL__encode_loc_params(loc_params, NULL, &loc_size)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");

                size += 2 + sizeof(int32_t) + 
                    1 + H5V_limit_enc_size((uint64_t)loc_size) + loc_size;

                if(NULL != p) {
                    /* encode request type */
                    *p++ = (uint8_t)H5VL_GROUP_GET;
                    /* encode get type */
                    *p++ = (uint8_t)get_type;
                    /* encode the object id */
                    INT32ENCODE(p, obj_id);
                    UINT64ENCODE_VARLEN(p, loc_size);
                    /* encode the location parameters */
                    if((ret_value = H5VL__encode_loc_params(loc_params, p, &loc_size)) < 0)
                        HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");
                    p += loc_size;
                }
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from group");
    }
    va_end (arguments);

    *nalloc = size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__encode_group_get_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_group_get_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__decode_group_get_params(void *buf, H5VL_group_get_t get_type, ...)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    va_list arguments;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    va_start (arguments, get_type);
    switch (get_type) {
        case H5VL_GROUP_GET_GCPL:
            {
                hid_t *obj_id = va_arg (arguments, hid_t *);
                /* decode the object id */
                INT32DECODE(p, *obj_id);
                break;
            }
        case H5VL_GROUP_GET_INFO:
            {
                hid_t *obj_id = va_arg (arguments, hid_t *);
                H5VL_loc_params_t *loc_params = va_arg (arguments, H5VL_loc_params_t *);
                size_t loc_size = 0;

                /* decode the object id */
                INT32DECODE(p, *obj_id);

                UINT64DECODE_VARLEN(p, loc_size);
                /* decode the location parameters */
                if((ret_value = H5VL__decode_loc_params(p, loc_params)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTDECODE, FAIL, "unable to decode VOL location param");
                p += loc_size;
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from group");
    }
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__decode_group_get_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__encode_group_close_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__encode_group_close_params(void *buf, size_t *nalloc, hid_t group_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(nalloc);

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_GROUP_CLOSE;

        /* encode the object id */
        INT32ENCODE(p, group_id);
    }

    size = 1 + sizeof(int);
    *nalloc = size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL__encode_group_close_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_group_close_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__decode_group_close_params(void *buf, hid_t *group_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* the metadata group id */
    INT32DECODE(p, *group_id);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL__decode_group_close_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__encode_link_create_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__encode_link_create_params(void *buf, size_t *nalloc, H5VL_link_create_type_t create_type,
                                hid_t obj_id, H5VL_loc_params_t loc_params, hid_t lcpl_id, 
                                hid_t lapl_id, ...)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0;
    va_list arguments;             /* argument list passed from the API call */
    size_t lcpl_size = 0, lapl_size = 0, loc_size = 0;
    H5P_genplist_t *lcpl = NULL, *lapl = NULL;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(nalloc);

    /* get loc params size to encode */
    if((ret_value = H5VL__encode_loc_params(loc_params, NULL, &loc_size)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");

    /* get size for property lists to encode */
    if(H5P_LINK_CREATE_DEFAULT != lcpl_id) {
        if(NULL == (lcpl = (H5P_genplist_t *)H5I_object_verify(lcpl_id, H5I_GENPROP_LST)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
        if((ret_value = H5P__encode(lcpl, FALSE, NULL, &lcpl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }
    if(H5P_LINK_ACCESS_DEFAULT != lapl_id) {
        if(NULL == (lapl = (H5P_genplist_t *)H5I_object_verify(lapl_id, H5I_GENPROP_LST)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
        if((ret_value = H5P__encode(lapl, FALSE, NULL, &lapl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }
    /* common stuff between all link create types */
    size += 2 + sizeof(int32_t) +
        1 + H5V_limit_enc_size((uint64_t)loc_size) + loc_size + 
        1 + H5V_limit_enc_size((uint64_t)lapl_size) + lapl_size + 
        1 + H5V_limit_enc_size((uint64_t)lcpl_size) + lcpl_size;

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_LINK_CREATE;

        /* encode the link create type */
        *p++ = (uint8_t)create_type;

        /* encode the object id */
        INT32ENCODE(p, obj_id);

        UINT64ENCODE_VARLEN(p, loc_size);
        /* encode the location parameters */
        if((ret_value = H5VL__encode_loc_params(loc_params, p, &loc_size)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");
        p += loc_size;

        /* encode the plist size */
        UINT64ENCODE_VARLEN(p, lcpl_size);
        /* encode property lists if they are not default*/
        if(H5P_LINK_CREATE_DEFAULT != lcpl_id) {
            if((ret_value = H5P__encode(lcpl, FALSE, p, &lcpl_size)) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
            p += lcpl_size;
        }

        /* encode the plist size */
        UINT64ENCODE_VARLEN(p, lapl_size);
        /* encode property lists if they are not default*/
        if(lapl_size) {
            if((ret_value = H5P__encode(lapl, FALSE, p, &lapl_size)) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
            p += lapl_size;
        }
    }

    va_start (arguments, lapl_id);
    switch (create_type) {
        case H5VL_LINK_CREATE_HARD:
            {
                hid_t cur_id = va_arg (arguments, hid_t);
                H5VL_loc_params_t cur_params = va_arg (arguments, H5VL_loc_params_t);
                size_t cur_loc_size = 0;

                /* get loc params size to encode */
                if((ret_value = H5VL__encode_loc_params(cur_params, NULL, &cur_loc_size)) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");

                size += sizeof(int32_t) + 1 + H5V_limit_enc_size((uint64_t)cur_loc_size) + cur_loc_size;

                if (NULL != p) {
                    /* encode the object id */
                    INT32ENCODE(p, cur_id);

                    UINT64ENCODE_VARLEN(p, loc_size);
                    /* encode the location parameters */
                    if((ret_value = H5VL__encode_loc_params(cur_params, p, &cur_loc_size)) < 0)
                        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");                    
                    p += cur_loc_size;
                }

                break;
            }
        case H5VL_LINK_CREATE_SOFT:
            {
                char *name = va_arg (arguments, char *);
                size_t len = 0;

                /* get name size to encode */
                if(NULL != name)
                    len = HDstrlen(name) + 1;

                size += 1 + H5V_limit_enc_size((uint64_t)len) + len;

                if (NULL != p) {
                    /* encode length of the link name and the actual link name */
                    UINT64ENCODE_VARLEN(p, len);
                    if(NULL != name)
                        HDstrcpy((char *)p, name);
                    p += len;
                }
                break;
            }
        case H5VL_LINK_CREATE_UD:
            {
                H5L_type_t link_type = va_arg (arguments, H5L_type_t);
                void *udata = va_arg (arguments, void *);
                size_t udata_size = va_arg (arguments, size_t);

                size += 2 + H5V_limit_enc_size((uint64_t)udata_size) + udata_size;

                if (NULL != p) {
                    *p++ = (uint8_t)link_type;
                    UINT64ENCODE_VARLEN(p, udata_size);
                    if(udata)
                        HDmemcpy(p, (const uint8_t *)udata, udata_size);
                    p += udata_size;
                }
                break;
            }
        default:
            HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "invalid link creation call");
    }
    va_end (arguments);

    *nalloc = size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__encode_link_create_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_link_create_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__decode_link_create_params(void *buf, H5VL_link_create_type_t *create_type, hid_t *obj_id, 
                                H5VL_loc_params_t *loc_params, hid_t *lcpl_id, hid_t *lapl_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t lapl_size = 0, lcpl_size = 0, loc_size = 0;
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    *create_type = (H5VL_link_create_type_t)*p++;

    /* decode the object id */
    INT32DECODE(p, *obj_id);

    UINT64DECODE_VARLEN(p, loc_size);
    /* decode the location parameters */
    if((ret_value = H5VL__decode_loc_params(p, loc_params)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "unable to decode VOL location param");
    p += loc_size;

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, lcpl_size);
    /* decode property lists if they are not default*/
    if(lcpl_size) {
        if((*lcpl_id = H5P__decode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += lcpl_size;
    }
    else {
        *lcpl_id = H5P_LINK_CREATE_DEFAULT;
    }

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, lapl_size);
    /* decode property lists if they are not default*/
    if(lapl_size) {
        if((*lapl_id = H5P__decode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += lapl_size;
    }
    else {
        *lapl_id = H5P_LINK_ACCESS_DEFAULT;
    }

    /* Get the plist structure */
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(*lcpl_id)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    switch (*create_type) {
        case H5VL_LINK_CREATE_HARD:
            {
                hid_t cur_id;
                H5VL_loc_params_t cur_loc_params;
                void *cur_obj = NULL; /* pointer to the target location object */
                size_t cur_loc_size = 0;

                /* decode the object id */
                INT32DECODE(p, cur_id);

                UINT64DECODE_VARLEN(p, cur_loc_size);
                /* decode the location parameters */
                if((ret_value = H5VL__decode_loc_params(p, &cur_loc_params)) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "unable to decode VOL location param");
                p += cur_loc_size;

                if(H5L_SAME_LOC != cur_id) {
                    if(NULL == (cur_obj = (void *)H5I_object(cur_id)))
                        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier");
                }

                /* set creation properties */
                if(H5P_set(plist, H5VL_LINK_TARGET, &cur_obj) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for target id");
                if(H5P_set(plist, H5VL_LINK_TARGET_LOC_PARAMS, &cur_loc_params) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't set property value for target name");

                break;
            }
        case H5VL_LINK_CREATE_SOFT:
            {
                char *name;
                size_t len = 0;

                /* decode length of the link target name and the actual link target_name */
                UINT64DECODE_VARLEN(p, len);
                if(0 != len) {
                    name = H5MM_xstrdup((const char *)(p));
                    p += len;
                }

                /* set creation properties */
                if(H5P_set(plist, H5VL_LINK_TARGET_NAME, &name) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for target name")

                break;
            }
        case H5VL_LINK_CREATE_UD:
            {
                H5L_type_t link_type;
                void *udata;
                size_t udata_size;

                link_type = (H5L_type_t)*p++;

                /* decode the length of udata */
                UINT64DECODE_VARLEN(p, udata_size);

                /* allocate udata buffer and copy data into it */
                if(0 != udata_size) {
                    if(NULL == (udata = H5MM_malloc(udata_size)))
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
                    HDmemcpy(udata, (const uint8_t *)p, udata_size);                    
                    p += udata_size;
                }

                /* set creation properties */
                if(H5P_set(plist, H5VL_LINK_TYPE, &link_type) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value from plist");
                if(H5P_set(plist, H5VL_LINK_UDATA, &udata) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value from plist");
                if(H5P_set(plist, H5VL_LINK_UDATA_SIZE, &udata_size) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value from plist");

                break;
            }
        default:
            HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "invalid link creation call")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__decode_link_create_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__encode_link_move_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__encode_link_move_params(void *buf, size_t *nalloc, hid_t src_id, H5VL_loc_params_t loc_params1, 
                              hid_t dst_id, H5VL_loc_params_t loc_params2, hbool_t copy_flag,
                              hid_t lcpl_id, hid_t lapl_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0;
    size_t lcpl_size = 0, lapl_size = 0, loc_size1 = 0, loc_size2 = 0;
    H5P_genplist_t *lcpl = NULL, *lapl = NULL;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(nalloc);

    /* get loc params size to encode */
    if((ret_value = H5VL__encode_loc_params(loc_params1, NULL, &loc_size1)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");
    /* get loc params size to encode */
    if((ret_value = H5VL__encode_loc_params(loc_params2, NULL, &loc_size2)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");

    /* get size for property lists to encode */
    if(H5P_LINK_CREATE_DEFAULT != lcpl_id) {
        if(NULL == (lcpl = (H5P_genplist_t *)H5I_object_verify(lcpl_id, H5I_GENPROP_LST)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
        if((ret_value = H5P__encode(lcpl, FALSE, NULL, &lcpl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }
    if(H5P_LINK_ACCESS_DEFAULT != lapl_id) {
        if(NULL == (lapl = (H5P_genplist_t *)H5I_object_verify(lapl_id, H5I_GENPROP_LST)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
        if((ret_value = H5P__encode(lapl, FALSE, NULL, &lapl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }

    /* common stuff between all link move types */
    size += 1 + 2*sizeof(int32_t) + sizeof(unsigned) + 
        1 + H5V_limit_enc_size((uint64_t)loc_size1) + loc_size1 + 
        1 + H5V_limit_enc_size((uint64_t)loc_size2) + loc_size2 + 
        1 + H5V_limit_enc_size((uint64_t)lapl_size) + lapl_size + 
        1 + H5V_limit_enc_size((uint64_t)lcpl_size) + lcpl_size;

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_LINK_MOVE;

        /* encode the object id */
        INT32ENCODE(p, src_id);

        UINT64ENCODE_VARLEN(p, loc_size1);
        /* encode the location parameters */
        if((ret_value = H5VL__encode_loc_params(loc_params1, p, &loc_size1)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");
        p += loc_size1;

        /* encode the object id */
        INT32ENCODE(p, dst_id);

        UINT64ENCODE_VARLEN(p, loc_size2);
        /* encode the location parameters */
        if((ret_value = H5VL__encode_loc_params(loc_params2, p, &loc_size2)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");
        p += loc_size2;

        /* encode the copy flag */
        H5_ENCODE_UNSIGNED(p, copy_flag);

        /* encode the plist size */
        UINT64ENCODE_VARLEN(p, lcpl_size);
        /* encode property lists if they are not default*/
        if(lcpl_size) {
            if((ret_value = H5P__encode(lcpl, FALSE, p, &lcpl_size)) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
            p += lcpl_size;
        }

        /* encode the plist size */
        UINT64ENCODE_VARLEN(p, lapl_size);
        /* encode property lists if they are not default*/
        if(lapl_size) {
            if((ret_value = H5P__encode(lapl, FALSE, p, &lapl_size)) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
            p += lapl_size;
        }
    }

    *nalloc = size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__encode_link_move_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_link_move_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__decode_link_move_params(void *buf, hid_t *src_id, H5VL_loc_params_t *loc_params1, 
                              hid_t *dst_id, H5VL_loc_params_t *loc_params2, hbool_t *copy_flag,
                              hid_t *lcpl_id, hid_t *lapl_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t lapl_size = 0, lcpl_size = 0, loc_size1 = 0, loc_size2 = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* decode the object id */
    INT32DECODE(p, *src_id);

    UINT64DECODE_VARLEN(p, loc_size1);
    /* decode the location parameters */
    if((ret_value = H5VL__decode_loc_params(p, loc_params1)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "unable to decode VOL location param");
    p += loc_size1;

    /* decode the object id */
    INT32DECODE(p, *dst_id);

    UINT64DECODE_VARLEN(p, loc_size2);
    /* decode the location parameters */
    if((ret_value = H5VL__decode_loc_params(p, loc_params2)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "unable to decode VOL location param");
    p += loc_size2;

    /* deocde open flag */
    H5_DECODE_UNSIGNED(p, *copy_flag);

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, lcpl_size);
    /* decode property lists if they are not default*/
    if(lcpl_size) {
        if((*lcpl_id = H5P__decode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += lcpl_size;
    }
    else {
        *lcpl_id = H5P_LINK_CREATE_DEFAULT;
    }

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, lapl_size);
    /* decode property lists if they are not default*/
    if(lapl_size) {
        if((*lapl_id = H5P__decode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += lapl_size;
    }
    else {
        *lapl_id = H5P_LINK_ACCESS_DEFAULT;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__decode_link_move_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__encode_link_iterate_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__encode_link_iterate_params(void *buf, size_t *nalloc, hid_t obj_id, H5VL_loc_params_t loc_params, 
                                 hbool_t recursive, H5_index_t idx_type, H5_iter_order_t order, 
                                 hsize_t *idx)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0;
    size_t loc_size = 0;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(nalloc);

    /* get loc params size to encode */
    if((ret_value = H5VL__encode_loc_params(loc_params, NULL, &loc_size)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");

    /* common stuff between all link iterate types */
    size += 4 + sizeof(int32_t) + sizeof(unsigned) + 
        1 + H5V_limit_enc_size((uint64_t)loc_size) + loc_size;
    if(NULL != idx)
        size += 1 + H5V_limit_enc_size((uint64_t)(*idx));

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_LINK_ITERATE;

        /* encode the object id */
        INT32ENCODE(p, obj_id);

        UINT64ENCODE_VARLEN(p, loc_size);
        /* encode the location parameters */
        if((ret_value = H5VL__encode_loc_params(loc_params, p, &loc_size)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");
        p += loc_size;

        H5_ENCODE_UNSIGNED(p, recursive);
        *p++ = (uint8_t)idx_type;
        *p++ = (uint8_t)order;

        if(NULL != idx) {
            *p++ = (uint8_t)1;
            UINT64ENCODE_VARLEN(p, *idx);
        }
        else {
            *p++ = (uint8_t)0;
        }
    }

    *nalloc = size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__encode_link_iterate_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_link_iterate_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__decode_link_iterate_params(void *buf, hid_t *obj_id, H5VL_loc_params_t *loc_params, 
                                 hbool_t *recursive, H5_index_t *idx_type, H5_iter_order_t *order, 
                                 hsize_t **idx)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t loc_size = 0;
    uint8_t flag;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* decode the object id */
    INT32DECODE(p, *obj_id);

    UINT64DECODE_VARLEN(p, loc_size);
    /* decode the location parameters */
    if((ret_value = H5VL__decode_loc_params(p, loc_params)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "unable to decode VOL location param");
    p += loc_size;

    H5_DECODE_UNSIGNED(p, *recursive);
    *idx_type = (H5_index_t)*p++;
    *order = (H5_iter_order_t)*p++;

    flag = (uint8_t)*p++;
    if(1 == flag) {
        *idx = (hsize_t *)H5MM_malloc(sizeof(hsize_t));
        UINT64DECODE_VARLEN(p, **idx);
    }
    else if (0 == flag)
        *idx = NULL;
    else
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "unable to decode idx flag");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__decode_link_iterate_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__encode_link_get_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__encode_link_get_params(void *buf, size_t *nalloc, H5VL_link_get_t get_type, ...)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0;
    va_list arguments;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(nalloc);

    va_start (arguments, get_type);
    switch (get_type) {
        case H5VL_LINK_EXISTS:
        case H5VL_LINK_GET_INFO:
            {
                hid_t obj_id = va_arg (arguments, hid_t);
                H5VL_loc_params_t loc_params = va_arg (arguments, H5VL_loc_params_t);
                size_t loc_size = 0;

                /* get loc params size to encode */
                if((ret_value = H5VL__encode_loc_params(loc_params, NULL, &loc_size)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");

                size += 2 + sizeof(int32_t) + 
                    1 + H5V_limit_enc_size((uint64_t)loc_size) + loc_size;

                if(NULL != p) {
                    /* encode request type */
                    *p++ = (uint8_t)H5VL_LINK_GET;
                    /* encode get type */
                    *p++ = (uint8_t)get_type;
                    /* encode the object id */
                    INT32ENCODE(p, obj_id);
                    UINT64ENCODE_VARLEN(p, loc_size);
                    /* encode the location parameters */
                    if((ret_value = H5VL__encode_loc_params(loc_params, p, &loc_size)) < 0)
                        HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");
                    p += loc_size;
                }
                break;
            }
        case H5VL_LINK_GET_NAME:
        case H5VL_LINK_GET_VAL:
            {
                hid_t obj_id = va_arg (arguments, hid_t);
                H5VL_loc_params_t loc_params = va_arg (arguments, H5VL_loc_params_t);
                size_t buf_size = va_arg (arguments, size_t);
                size_t loc_size = 0;

                /* get loc params size to encode */
                if((ret_value = H5VL__encode_loc_params(loc_params, NULL, &loc_size)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");

                size += 2 + sizeof(int32_t) + 
                    1 + H5V_limit_enc_size((uint64_t)loc_size) + loc_size + 
                    1 + H5V_limit_enc_size((uint64_t)buf_size);

                if(NULL != p) {
                    /* encode request type */
                    *p++ = (uint8_t)H5VL_ATTR_GET;
                    /* encode get type */
                    *p++ = (uint8_t)get_type;

                    /* encode the object id */
                    INT32ENCODE(p, obj_id);

                    UINT64ENCODE_VARLEN(p, loc_size);
                    /* encode the location parameters */
                    if((ret_value = H5VL__encode_loc_params(loc_params, p, &loc_size)) < 0)
                        HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");
                    p += loc_size;

                    /* encode length of the attr name and the actual attr name */
                    UINT64ENCODE_VARLEN(p, buf_size);
                }
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from link");
    }
    va_end (arguments);

    *nalloc = size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__encode_link_get_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_link_get_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__decode_link_get_params(void *buf, H5VL_link_get_t get_type, ...)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    va_list arguments;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    va_start (arguments, get_type);
    switch (get_type) {
        case H5VL_LINK_EXISTS:
        case H5VL_LINK_GET_INFO:
            {
                hid_t *obj_id = va_arg (arguments, hid_t *);
                H5VL_loc_params_t *loc_params = va_arg (arguments, H5VL_loc_params_t *);
                size_t loc_size = 0;

                /* decode the object id */
                INT32DECODE(p, *obj_id);

                UINT64DECODE_VARLEN(p, loc_size);
                /* decode the location parameters */
                if((ret_value = H5VL__decode_loc_params(p, loc_params)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTDECODE, FAIL, "unable to decode VOL location param");
                p += loc_size;
                break;
            }
        case H5VL_LINK_GET_NAME:
        case H5VL_LINK_GET_VAL:
            {
                hid_t *obj_id = va_arg (arguments, hid_t *);
                H5VL_loc_params_t *loc_params = va_arg (arguments, H5VL_loc_params_t *);
                size_t *size =  va_arg (arguments, size_t*);
                size_t loc_size = 0;

                /* decode the object id */
                INT32DECODE(p, *obj_id);

                UINT64DECODE_VARLEN(p, loc_size);
                /* decode the location parameters */
                if((ret_value = H5VL__decode_loc_params(p, loc_params)) < 0)
                    HGOTO_ERROR(H5E_VOL, H5E_CANTDECODE, FAIL, "unable to decode VOL location param");
                p += loc_size;

                UINT64DECODE_VARLEN(p, *size);

                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't get this type of information from link");
    }
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__decode_link_get_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__encode_link_remove_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__encode_link_remove_params(void *buf, size_t *nalloc, hid_t obj_id, H5VL_loc_params_t loc_params)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0;
    size_t loc_size = 0;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(nalloc);

    /* get loc params size to encode */
    if((ret_value = H5VL__encode_loc_params(loc_params, NULL, &loc_size)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");

    /* common stuff between all link remove types */
    size += 1 + sizeof(int32_t) +
        1 + H5V_limit_enc_size((uint64_t)loc_size) + loc_size;

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_LINK_REMOVE;

        /* encode the object id */
        INT32ENCODE(p, obj_id);

        UINT64ENCODE_VARLEN(p, loc_size);
        /* encode the location parameters */
        if((ret_value = H5VL__encode_loc_params(loc_params, p, &loc_size)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");
        p += loc_size;
    }

    *nalloc = size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__encode_link_remove_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_link_remove_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__decode_link_remove_params(void *buf, hid_t *obj_id, H5VL_loc_params_t *loc_params)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t loc_size = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* decode the object id */
    INT32DECODE(p, *obj_id);

    UINT64DECODE_VARLEN(p, loc_size);
    /* decode the location parameters */
    if((ret_value = H5VL__decode_loc_params(p, loc_params)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "unable to decode VOL location param");
    p += loc_size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__decode_link_remove_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__encode_object_open_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__encode_object_open_params(void *buf, size_t *nalloc, hid_t obj_id, H5VL_loc_params_t loc_params)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0;
    size_t loc_size = 0;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(nalloc);

    /* get loc params size to encode */
    if((ret_value = H5VL__encode_loc_params(loc_params, NULL, &loc_size)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_OBJECT_OPEN;

        /* encode the object id */
        INT32ENCODE(p, obj_id);

        UINT64ENCODE_VARLEN(p, loc_size);
        /* encode the location parameters */
        if((ret_value = H5VL__encode_loc_params(loc_params, p, &loc_size)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");                    
        p += loc_size;
    }
    size += (1 + sizeof(int32_t) + 
             1 + H5V_limit_enc_size((uint64_t)loc_size) + loc_size);

    *nalloc = size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__encode_object_open_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_object_open_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__decode_object_open_params(void *buf, hid_t *obj_id, H5VL_loc_params_t *loc_params)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t loc_size = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* decode the object id */
    INT32DECODE(p, *obj_id);

    UINT64DECODE_VARLEN(p, loc_size);
    /* decode the location parameters */
    if((ret_value = H5VL__decode_loc_params(p, loc_params)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTDECODE, FAIL, "unable to decode VOL location param");
    p += loc_size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__decode_object_open_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__encode_object_copy_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__encode_object_copy_params(void *buf, size_t *nalloc, 
                                hid_t src_id, H5VL_loc_params_t loc_params1, const char *src_name,
                                hid_t dst_id, H5VL_loc_params_t loc_params2, const char *dst_name,
                                hid_t ocpypl_id, hid_t lcpl_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0;
    size_t lcpl_size = 0, ocpypl_size = 0, loc_size1 = 0, loc_size2 = 0, len1 = 0, len2 = 0;
    H5P_genplist_t *lcpl = NULL, *ocpypl = NULL;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(nalloc);

    /* get source name size to encode */
    if(NULL != src_name)
        len1 = HDstrlen(src_name) + 1;

    /* get dest name size to encode */
    if(NULL != dst_name)
        len2 = HDstrlen(dst_name) + 1;

    /* get loc params size to encode */
    if((ret_value = H5VL__encode_loc_params(loc_params1, NULL, &loc_size1)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");
    /* get loc params size to encode */
    if((ret_value = H5VL__encode_loc_params(loc_params2, NULL, &loc_size2)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");

    /* get size for property lists to encode */
    if(H5P_LINK_CREATE_DEFAULT != lcpl_id) {
        if(NULL == (lcpl = (H5P_genplist_t *)H5I_object_verify(lcpl_id, H5I_GENPROP_LST)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
        if((ret_value = H5P__encode(lcpl, FALSE, NULL, &lcpl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }
    if(H5P_OBJECT_COPY_DEFAULT != ocpypl_id) {
        if(NULL == (ocpypl = (H5P_genplist_t *)H5I_object_verify(ocpypl_id, H5I_GENPROP_LST)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
        if((ret_value = H5P__encode(ocpypl, FALSE, NULL, &ocpypl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }

    /* common stuff between all link move types */
    size += 1 + 2*sizeof(int32_t) + 
        1 + H5V_limit_enc_size((uint64_t)len1) + len1 + 
        1 + H5V_limit_enc_size((uint64_t)len2) + len2 + 
        1 + H5V_limit_enc_size((uint64_t)loc_size1) + loc_size1 + 
        1 + H5V_limit_enc_size((uint64_t)loc_size2) + loc_size2 + 
        1 + H5V_limit_enc_size((uint64_t)ocpypl_size) + ocpypl_size + 
        1 + H5V_limit_enc_size((uint64_t)lcpl_size) + lcpl_size;

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_OBJECT_COPY;

        /* encode the object id */
        INT32ENCODE(p, src_id);

        UINT64ENCODE_VARLEN(p, loc_size1);
        /* encode the location parameters */
        if((ret_value = H5VL__encode_loc_params(loc_params1, p, &loc_size1)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");
        p += loc_size1;

        UINT64ENCODE_VARLEN(p, len1);
        if(NULL != src_name)
            HDstrcpy((char *)p, src_name);
        p += len1;

        /* encode the object id */
        INT32ENCODE(p, dst_id);

        UINT64ENCODE_VARLEN(p, loc_size2);
        /* encode the location parameters */
        if((ret_value = H5VL__encode_loc_params(loc_params2, p, &loc_size2)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");
        p += loc_size2;

        UINT64ENCODE_VARLEN(p, len2);
        if(NULL != dst_name)
            HDstrcpy((char *)p, dst_name);
        p += len2;

        /* encode the plist size */
        UINT64ENCODE_VARLEN(p, lcpl_size);
        /* encode property lists if they are not default*/
        if(lcpl_size) {
            if((ret_value = H5P__encode(lcpl, FALSE, p, &lcpl_size)) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
            p += lcpl_size;
        }

        /* encode the plist size */
        UINT64ENCODE_VARLEN(p, ocpypl_size);
        /* encode property lists if they are not default*/
        if(ocpypl_size) {
            if((ret_value = H5P__encode(ocpypl, FALSE, p, &ocpypl_size)) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
            p += ocpypl_size;
        }
    }

    *nalloc = size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__encode_object_copy_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_object_copy_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__decode_object_copy_params(void *buf, hid_t *src_id, H5VL_loc_params_t *loc_params1, char **src_name,
                                hid_t *dst_id, H5VL_loc_params_t *loc_params2, char **dst_name,
                                hid_t *ocpypl_id, hid_t *lcpl_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t ocpypl_size = 0, lcpl_size = 0, loc_size1 = 0, loc_size2 = 0, len1 = 0, len2 = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* decode the object id */
    INT32DECODE(p, *src_id);

    UINT64DECODE_VARLEN(p, loc_size1);
    /* decode the location parameters */
    if((ret_value = H5VL__decode_loc_params(p, loc_params1)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "unable to decode VOL location param");
    p += loc_size1;

    /* decode length of the src name and the actual src_name */
    UINT64DECODE_VARLEN(p, len1);
    if(len1) {
        *src_name = H5MM_xstrdup((const char *)(p));
        p += len1;
    }

    /* decode the object id */
    INT32DECODE(p, *dst_id);

    UINT64DECODE_VARLEN(p, loc_size2);
    /* decode the location parameters */
    if((ret_value = H5VL__decode_loc_params(p, loc_params2)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "unable to decode VOL location param");
    p += loc_size2;

    /* decode length of the dst name and the actual dst_name */
    UINT64DECODE_VARLEN(p, len2);
    if(len2) {
        *dst_name = H5MM_xstrdup((const char *)(p));
        p += len2;
    }

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, lcpl_size);
    /* decode property lists if they are not default*/
    if(lcpl_size) {
        if((*lcpl_id = H5P__decode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += lcpl_size;
    }
    else {
        *lcpl_id = H5P_LINK_CREATE_DEFAULT;
    }

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, ocpypl_size);
    /* decode property lists if they are not default*/
    if(ocpypl_size) {
        if((*ocpypl_id = H5P__decode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += ocpypl_size;
    }
    else {
        *ocpypl_id = H5P_OBJECT_COPY_DEFAULT;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__decode_object_copy_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__encode_object_visit_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__encode_object_visit_params(void *buf, size_t *nalloc, hid_t obj_id, H5VL_loc_params_t loc_params, 
                                 H5_index_t idx_type, H5_iter_order_t order)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0;
    size_t loc_size = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(nalloc);

    /* get loc params size to encode */
    if((ret_value = H5VL__encode_loc_params(loc_params, NULL, &loc_size)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");

    /* common stuff between all link iterate types */
    size += 3 + sizeof(int32_t) + 
        1 + H5V_limit_enc_size((uint64_t)loc_size) + loc_size;

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_OBJECT_VISIT;

        /* encode the object id */
        INT32ENCODE(p, obj_id);

        UINT64ENCODE_VARLEN(p, loc_size);
        /* encode the location parameters */
        if((ret_value = H5VL__encode_loc_params(loc_params, p, &loc_size)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");
        p += loc_size;

        *p++ = (uint8_t)idx_type;
        *p++ = (uint8_t)order;
    }

    *nalloc = size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__encode_object_visit_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_object_visit_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__decode_object_visit_params(void *buf, hid_t *obj_id, H5VL_loc_params_t *loc_params, 
                                 H5_index_t *idx_type, H5_iter_order_t *order)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t loc_size = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* decode the object id */
    INT32DECODE(p, *obj_id);

    UINT64DECODE_VARLEN(p, loc_size);
    /* decode the location parameters */
    if((ret_value = H5VL__decode_loc_params(p, loc_params)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "unable to decode VOL location param");
    p += loc_size;

    *idx_type = (H5_index_t)*p++;
    *order = (H5_iter_order_t)*p++;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__decode_object_visit_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__encode_object_misc_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__encode_object_misc_params(void *buf, size_t *nalloc, H5VL_object_misc_t misc_type, 
                                hid_t obj_id, H5VL_loc_params_t loc_params, ...)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0, loc_size = 0;
    va_list arguments;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(nalloc);

    /* get loc params size to encode */
    if((ret_value = H5VL__encode_loc_params(loc_params, NULL, &loc_size)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");

    size += 2 + sizeof(int32_t) +
        1 + H5V_limit_enc_size((uint64_t)loc_size) + loc_size;

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_OBJECT_MISC;
        /* encode misc type */
        *p++ = (uint8_t)misc_type;
        /* encode the object id */
        INT32ENCODE(p, obj_id);

        UINT64ENCODE_VARLEN(p, loc_size);
        /* encode the location parameters */
        if((ret_value = H5VL__encode_loc_params(loc_params, p, &loc_size)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");
        p += loc_size;
    }

    va_start (arguments, loc_params);
    switch (misc_type) {
        case H5VL_ATTR_RENAME:
            {
                char *old_name = va_arg (arguments, char *);
                char *new_name = va_arg (arguments, char *);
                size_t len1 = 0, len2 = 0;

                /* get old name size to encode */
                if(NULL != old_name)
                    len1 = HDstrlen(old_name) + 1;

                /* get new name size to encode */
                if(NULL != new_name)
                    len2 = HDstrlen(new_name) + 1;

                size += 1 + H5V_limit_enc_size((uint64_t)len1) + len1 +
                    1 + H5V_limit_enc_size((uint64_t)len2) + len2;

                if(NULL != p) {
                    UINT64ENCODE_VARLEN(p, len1);
                    if(NULL != old_name)
                        HDstrcpy((char *)p, old_name);
                    p += len1;

                    UINT64ENCODE_VARLEN(p, len2);
                    if(NULL != new_name)
                        HDstrcpy((char *)p, new_name);
                    p += len2;
                }
                break;
            }
        case H5VL_OBJECT_CHANGE_REF_COUNT:
            {
                int update_ref  = va_arg (arguments, int);

                size += sizeof(int32_t);
                if(NULL != p) {
                    INT32ENCODE(p, update_ref);
                }
                break;
            }
        case H5VL_OBJECT_SET_COMMENT:
            {
                char *comment = va_arg (arguments, char *);
                size_t len = 0;

                /* get comment size to encode */
                if(NULL != comment)
                    len = HDstrlen(comment) + 1;

                size += 1 + H5V_limit_enc_size((uint64_t)len) + len;

                if(NULL != p) {
                    UINT64ENCODE_VARLEN(p, len);
                    if(NULL != comment)
                        HDstrcpy((char *)p, comment);
                    p += len;
                }
                break;
            }
        case H5VL_REF_CREATE:
            {
                char *name = va_arg (arguments, char *);
                H5R_type_t ref_type = va_arg (arguments, H5R_type_t);
                hid_t space_id = va_arg (arguments, hid_t);
                size_t len = 0, space_size = 0;
                H5S_t *dspace = NULL;

                /* get name size to encode */
                if(NULL != name)
                    len = HDstrlen(name) + 1;

                /* get Dataspace size to encode */
                if(space_id >= 0) {
                    if (NULL==(dspace=(H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE)))
                        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataspace");
                    if(H5S_encode(dspace, NULL, &space_size)<0)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "can't encode datatype");
                }

                size += 1 + 
                    1+ H5V_limit_enc_size((uint64_t)len) + len + 
                    1+ H5V_limit_enc_size((uint64_t)space_size) + space_size;

                if(NULL != p) {
                    /* encode the name size */
                    UINT64ENCODE_VARLEN(p, len);
                    /* encode the name */
                    if(NULL != name)
                        HDstrcpy((char *)p, name);
                    p += len;

                    /* encode ref_type */
                    *p++ = (uint8_t)ref_type;

                    /* encode the dataspace size */
                    UINT64ENCODE_VARLEN(p, space_size);
                    if (space_size) {
                        /* encode datatspace */
                        if((ret_value = H5S_encode(dspace, p, &space_size)) < 0)
                            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "unable to encode datatspace");
                        p += space_size;
                    }
                }
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't perform this type of operation on object");
    }
    va_end (arguments);

    *nalloc = size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__encode_object_misc_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_object_misc_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__decode_object_misc_params(void *buf, H5VL_object_misc_t misc_type, hid_t *obj_id,
                                H5VL_loc_params_t *loc_params, ...)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    va_list arguments;
    size_t loc_size = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* decode the object id */
    INT32DECODE(p, *obj_id);

    UINT64DECODE_VARLEN(p, loc_size);
    /* decode the location parameters */
    if((ret_value = H5VL__decode_loc_params(p, loc_params)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTDECODE, FAIL, "unable to decode VOL location param");
    p += loc_size;

    va_start (arguments, loc_params);
    switch (misc_type) {
        case H5VL_ATTR_RENAME:
            {
                char **old_name = va_arg (arguments, char **);
                char **new_name = va_arg (arguments, char **);
                size_t len1 = 0, len2 = 0;

                /* decode length of the old name and the actual old_name */
                UINT64DECODE_VARLEN(p, len1);
                if(len1) {
                    *old_name = H5MM_xstrdup((const char *)(p));
                    p += len1;
                }

                /* decode length of the new name and the actual new_name */
                UINT64DECODE_VARLEN(p, len2);
                if(len2) {
                    *new_name = H5MM_xstrdup((const char *)(p));
                    p += len2;
                }

                break;
            }
        case H5VL_OBJECT_CHANGE_REF_COUNT:
            {
                int *update_ref  = va_arg (arguments, int *);

                INT32DECODE(p, *update_ref);
                break;
            }
        case H5VL_OBJECT_SET_COMMENT:
            {
                char **comment = va_arg (arguments, char **);
                size_t len = 0;

                /* decode length of the new name and the actual comment */
                UINT64DECODE_VARLEN(p, len);
                if(len) {
                    *comment = H5MM_xstrdup((const char *)(p));
                    p += len;
                }

                break;
            }
        case H5VL_REF_CREATE:
            {
                char **name = va_arg (arguments, char **);
                H5R_type_t *ref_type  = va_arg (arguments, H5R_type_t *);
                hid_t *space_id  = va_arg (arguments, hid_t *);
                size_t len = 0, space_size = 0;

                /* decode length of the new name and the actual name */
                UINT64DECODE_VARLEN(p, len);
                if(len) {
                    *name = H5MM_xstrdup((const char *)(p));
                    p += len;
                }

                /* decode reference type */
                *ref_type = (H5R_type_t)*p++;

                /* decode the space size */
                UINT64DECODE_VARLEN(p, space_size);
                if(space_size) {
                    H5S_t *ds = NULL;
                    /* decode the dataspace */
                    if((ds = H5S_decode((const unsigned char *)p)) == NULL)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDECODE, FAIL, "can't decode object");
                    /* Register the type and return the ID */
                    if((*space_id = H5I_register(H5I_DATASPACE, ds, FALSE)) < 0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTREGISTER, FAIL, "unable to register dataspace");
                    p += space_size;
                }

                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't perform this type of operation on object");
    }
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__decode_object_misc_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__encode_object_get_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__encode_object_get_params(void *buf, size_t *nalloc, H5VL_object_get_t get_type, 
                               hid_t obj_id, H5VL_loc_params_t loc_params, ...)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0, loc_size = 0;
    va_list arguments;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(nalloc);

    /* get loc params size to encode */
    if((ret_value = H5VL__encode_loc_params(loc_params, NULL, &loc_size)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");

    size += 2 + sizeof(int32_t) +
        1 + H5V_limit_enc_size((uint64_t)loc_size) + loc_size;

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_OBJECT_GET;
        /* encode get type */
        *p++ = (uint8_t)get_type;
        /* encode the object id */
        INT32ENCODE(p, obj_id);

        UINT64ENCODE_VARLEN(p, loc_size);
        /* encode the location parameters */
        if((ret_value = H5VL__encode_loc_params(loc_params, p, &loc_size)) < 0)
            HGOTO_ERROR(H5E_VOL, H5E_CANTENCODE, FAIL, "unable to encode VOL location param");
        p += loc_size;
    }

    va_start (arguments, loc_params);
    switch (get_type) {
        case H5VL_OBJECT_EXISTS:
        case H5VL_OBJECT_GET_INFO:
            break;
        case H5VL_OBJECT_GET_COMMENT:
            {
                size_t len = va_arg (arguments, size_t);

                size += 1 + H5V_limit_enc_size((uint64_t)len);

                if(NULL != p)
                    UINT64ENCODE_VARLEN(p, len);
                break;
            }
        case H5VL_REF_GET_REGION:
        case H5VL_REF_GET_TYPE:
            {
                H5R_type_t  ref_type =  va_arg (arguments, H5R_type_t);
                const void  *ref     =  va_arg (arguments, const void *);
                size_t ref_size = 0;

                if(ref_type == H5R_DATASET_REGION)
                    ref_size = sizeof(hdset_reg_ref_t);
                else if (ref_type == H5R_OBJECT)
                    ref_size = sizeof(hobj_ref_t);

                size += 1 + 
                    1 + H5V_limit_enc_size((uint64_t)ref_size) + ref_size;

                if(NULL != p) {
                    *p++ = (uint8_t)ref_type;
                    UINT64ENCODE_VARLEN(p, ref_size);
                    if(ref_size)
                        HDmemcpy(p, (const uint8_t *)ref, ref_size);
                    p += ref_size;
                }

                break;
            }
        case H5VL_REF_GET_NAME:
            {
                H5R_type_t  ref_type =  va_arg (arguments, H5R_type_t);
                void        *ref     =  va_arg (arguments, void *);
                size_t      buf_size = va_arg (arguments, size_t);
                size_t ref_size = 0;

                if(ref_type == H5R_DATASET_REGION)
                    ref_size = sizeof(hdset_reg_ref_t);
                else if (ref_type == H5R_OBJECT)
                    ref_size = sizeof(hobj_ref_t);

                size += 1 + 
                    1 + H5V_limit_enc_size((uint64_t)ref_size) + ref_size +
                    1 + H5V_limit_enc_size((uint64_t)buf_size);

                if(NULL != p) {
                    *p++ = (uint8_t)ref_type;
                    UINT64ENCODE_VARLEN(p, ref_size);
                    if(ref_size)
                        HDmemcpy(p, (const uint8_t *)ref, ref_size);
                    p += ref_size;
                    UINT64ENCODE_VARLEN(p, buf_size);
                }

                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't perform this type of operation on object");
    }
    va_end (arguments);

    *nalloc = size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__encode_object_get_params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode_object_get_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__decode_object_get_params(void *buf, H5VL_object_get_t get_type, hid_t *obj_id,
                               H5VL_loc_params_t *loc_params, ...)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    va_list arguments;
    size_t loc_size = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* decode the object id */
    INT32DECODE(p, *obj_id);

    UINT64DECODE_VARLEN(p, loc_size);
    /* decode the location parameters */
    if((ret_value = H5VL__decode_loc_params(p, loc_params)) < 0)
        HGOTO_ERROR(H5E_VOL, H5E_CANTDECODE, FAIL, "unable to decode VOL location param");
    p += loc_size;

    va_start (arguments, loc_params);
    switch (get_type) {
        case H5VL_OBJECT_EXISTS:
        case H5VL_OBJECT_GET_INFO:
            break;
        case H5VL_OBJECT_GET_COMMENT:
            {
                size_t *len = va_arg (arguments, size_t *);

                UINT64DECODE_VARLEN(p, *len);
                break;
            }
        case H5VL_REF_GET_REGION:
        case H5VL_REF_GET_TYPE:
            {
                H5R_type_t  *ref_type =  va_arg (arguments, H5R_type_t *);
                void        **ref     =  va_arg (arguments, void **);
                size_t ref_size = 0;

                *ref_type = (H5R_type_t)*p++;

                UINT64DECODE_VARLEN(p, ref_size);

                *ref = HDmalloc(ref_size);
                HDmemcpy(*ref, (const uint8_t *)p, ref_size);
                p += ref_size;
                break;
            }
        case H5VL_REF_GET_NAME:
            {
                H5R_type_t  *ref_type = va_arg (arguments, H5R_type_t *);
                void        **ref     = va_arg (arguments, void **);
                size_t      *buf_size = va_arg (arguments, size_t *);
                size_t ref_size = 0;

                *ref_type = (H5R_type_t)*p++;

                UINT64DECODE_VARLEN(p, ref_size);

                *ref = HDmalloc(ref_size);
                HDmemcpy(*ref, (const uint8_t *)p, ref_size);
                p += ref_size;

                UINT64DECODE_VARLEN(p, *buf_size);
                break;
            }
        default:
            HGOTO_ERROR(H5E_VOL, H5E_CANTGET, FAIL, "can't perform this type of operation on object");
    }
    va_end (arguments);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__decode_object_get_params() */

#if 0
/*-------------------------------------------------------------------------
 * Function:	H5VL__encode__params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__encode__params(void *buf, size_t *nalloc, )
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t size = 0;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT


    *nalloc = size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__encode__params() */

/*-------------------------------------------------------------------------
 * Function:	H5VL__decode__params
 *------------------------------------------------------------------------- */
herr_t 
H5VL__decode__params(void *buf, )
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */

    FUNC_ENTER_NOAPI_NOINIT


 done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__decode__params() */
#endif
