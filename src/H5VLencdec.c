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
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5Sprivate.h" 	/* Dataspaces                      	*/
#include "H5Tprivate.h"		/* Datatypes				*/
#include "H5VLprivate.h"	/* VOL plugins				*/
#include "H5VLmdserver.h"       /* MDS helper routines			*/


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
            len = HDstrlen(loc_params.loc_data.loc_by_name.name);

            /* get size of property list */
            if(H5P_DEFAULT != loc_params.loc_data.loc_by_name.plist_id)
                if((ret_value = H5Pencode(loc_params.loc_data.loc_by_name.plist_id, 
                                          NULL, &plist_size)) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");

            size += (1 + H5V_limit_enc_size((uint64_t)len) + len + 
                      1 + H5V_limit_enc_size((uint64_t)plist_size) + plist_size);
            break;
        case H5VL_OBJECT_BY_IDX:
            /* get length of name */
            len = HDstrlen(loc_params.loc_data.loc_by_idx.name);

            /* get size of property list */
            if(H5P_DEFAULT != loc_params.loc_data.loc_by_idx.plist_id)
                if((ret_value = H5Pencode(loc_params.loc_data.loc_by_idx.plist_id,
                                          NULL, &plist_size)) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");

            size += (1 + H5V_limit_enc_size((uint64_t)len) + len + 2 + 
                      1 + H5V_limit_enc_size((uint64_t)plist_size) + plist_size + 
                      1 + H5V_limit_enc_size((uint64_t)loc_params.loc_data.loc_by_idx.n));
            break;
        case H5VL_OBJECT_BY_ADDR:
            size += 1 + H5V_limit_enc_size((uint64_t)loc_params.loc_data.loc_by_addr.addr);
            break;
        case H5VL_OBJECT_BY_REF:
            /* get size of property list */
            if(H5P_DEFAULT != loc_params.loc_data.loc_by_ref.plist_id)
                if((ret_value = H5Pencode(loc_params.loc_data.loc_by_ref.plist_id,
                                          NULL, &plist_size)) < 0)
                    HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");

            size += (1 + H5V_limit_enc_size((uint64_t)plist_size) + plist_size);
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
                    /* get length of name */
                    len = HDstrlen(loc_params.loc_data.loc_by_name.name);

                    /* encode length of name and name */
                    UINT64ENCODE_VARLEN(p, len);
                    HDmemcpy(p, (const uint8_t *)loc_params.loc_data.loc_by_name.name, len);
                    p += len;

                    /* encode the plist size */
                    UINT64ENCODE_VARLEN(p, plist_size);
                    /* encode property lists if they are not default*/
                    if(H5P_DEFAULT != loc_params.loc_data.loc_by_name.plist_id) {
                        if((ret_value = H5Pencode(loc_params.loc_data.loc_by_name.plist_id, p, 
                                                  &plist_size)) < 0)
                            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
                        p += plist_size;
                    }
                    break;
                }
            case H5VL_OBJECT_BY_IDX:
                {
                    /* get length of name */
                    len = HDstrlen(loc_params.loc_data.loc_by_idx.name);

                    /* encode length of name and name */
                    UINT64ENCODE_VARLEN(p, len);
                    HDmemcpy(p, (const uint8_t *)loc_params.loc_data.loc_by_idx.name, len);
                    p += len;

                    *p++ = (uint8_t)loc_params.loc_data.loc_by_idx.idx_type;
                    *p++ = (uint8_t)loc_params.loc_data.loc_by_idx.order;
                    UINT64ENCODE_VARLEN(p, loc_params.loc_data.loc_by_idx.n);

                    /* encode the plist size */
                    UINT64ENCODE_VARLEN(p, plist_size);
                    /* encode property lists if they are not default*/
                    if(H5P_DEFAULT != loc_params.loc_data.loc_by_idx.plist_id) {
                        if((ret_value = H5Pencode(loc_params.loc_data.loc_by_idx.plist_id, p, 
                                                  &plist_size)) < 0)
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

                    /* encode the plist size */
                    UINT64ENCODE_VARLEN(p, plist_size);
                    /* encode property lists if they are not default*/
                    if(H5P_DEFAULT != loc_params.loc_data.loc_by_ref.plist_id) {
                        if((ret_value = H5Pencode(loc_params.loc_data.loc_by_ref.plist_id, p, 
                                                  &plist_size)) < 0)
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
            //HDmemcpy(p, (uint8_t *)(loc_params->loc_data.loc_by_idx.name), len);
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

    len = HDstrlen(name);

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_FILE_CREATE;

        /* encode length of name and name */
        UINT64ENCODE_VARLEN(p, len);

        HDmemcpy(p, (const uint8_t *)name, len);
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
H5VL__decode_file_create_params(void *buf, char **mds_filename, unsigned *flags, 
                                hid_t *fcpl_id, hid_t *fapl_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t len = 0; /* length of name (decoded) */
    char *name = NULL;
    size_t fcpl_size = 0, fapl_size = 0; /* plist sizes */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* decode length of name and name */
    UINT64DECODE_VARLEN(p, len);
    name = H5MM_xstrdup((const char *)(p));
    name[len] = '\0';
    p += len;

    /* generate the MDS file name by adding a .md extension to the file name */
    *mds_filename = (char *)H5MM_malloc (sizeof(char) * (len + 4));
    sprintf(*mds_filename, "%s.md", name);

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

    if(name)
        H5MM_xfree(name);
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

    len = HDstrlen(name);

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_FILE_OPEN;

        /* encode length of name and name */
        UINT64ENCODE_VARLEN(p, len);
        HDmemcpy(p, (const uint8_t *)name, len);
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
H5VL__decode_file_open_params(void *buf, char **mds_filename, unsigned *flags, hid_t *fapl_id)
{
    uint8_t *p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */
    size_t len = 0; /* length of name (decoded) */
    char *name = NULL;
    size_t fapl_size = 0; /* plist sizes */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* decode length of name and name */
    UINT64DECODE_VARLEN(p, len);
    name = H5MM_xstrdup((const char *)(p));
    name[len] = '\0';
    p += len;

    /* generate the MDS file name by adding a .md extension to the file name */
    *mds_filename = (char *)H5MM_malloc (sizeof(char) * (len + 4));
    sprintf(*mds_filename, "%s.md", name);

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

    if(name)
        H5MM_xfree(name);
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
    if((ret_value = H5Tencode(type_id, NULL, &type_size)) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "unable to encode datatype");

    /* get Dataspace size to encode */
    if((ret_value = H5Sencode(space_id, NULL, &space_size)) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "unable to encode dataspace");

    /* get name size to encode */
    if(NULL != name)
        len = HDstrlen(name);

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
            HDmemcpy(p, (const uint8_t *)name, len);
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
        if((ret_value = H5Tencode(type_id, p, &type_size)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "unable to encode datatype");
        p += type_size;

        /* encode the dataspace size */
        UINT64ENCODE_VARLEN(p, space_size);
        /* encode datatspace */
        if((ret_value = H5Sencode(space_id, p, &space_size)) < 0)
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
        (*name)[len] = '\0';
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
    /* decode the datatype */
    if((*type_id = H5Tdecode(p)) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDECODE, FAIL, "unable to decode datatype");
    p += type_size;

    /* decode the space size */
    UINT64DECODE_VARLEN(p, space_size);
    /* decode the dataspace */
    if((*space_id = H5Sdecode((const void *)p)) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDECODE, FAIL, "unable to decode dataspace");
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
        len = HDstrlen(name);

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
        if(NULL != name)
            HDmemcpy(p, (const uint8_t *)name, len);
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
        *name = H5MM_xstrdup((const char *)(p));
        (*name)[len] = '\0';
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
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(nalloc);

    /* get Type size to encode */
    if((ret_value = H5Tencode(type_id, NULL, &type_size)) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "unable to encode datatype");

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_ATTR_READ;

        /* encode the object id */
        INT32ENCODE(p, obj_id);

        /* encode the datatype size */
        UINT64ENCODE_VARLEN(p, type_size);
        /* encode datatype */
        if((ret_value = H5Tencode(type_id, p, &type_size)) < 0)
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
    if((*type_id = H5Tdecode(p)) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDECODE, FAIL, "unable to decode datatype");
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
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(nalloc);

    /* get Type size to encode */
    if((ret_value = H5Tencode(type_id, NULL, &type_size)) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "unable to encode datatype");

    if(NULL != p) {
        /* encode request type */
        *p++ = (uint8_t)H5VL_ATTR_WRITE;

        /* encode the object id */
        INT32ENCODE(p, obj_id);

        /* encode the datatype size */
        UINT64ENCODE_VARLEN(p, type_size);
        /* encode datatype */
        if((ret_value = H5Tencode(type_id, p, &type_size)) < 0)
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
    if((*type_id = H5Tdecode(p)) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDECODE, FAIL, "unable to decode datatype");
    p += type_size;

    /* decode the write buffer size */
    UINT64DECODE_VARLEN(p, *buf_size);
    *attr_buf = p;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL__decode_attr_write_params() */

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
        len = HDstrlen(name);

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
            HDmemcpy(p, (const uint8_t *)name, len);
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
        (*name)[len] = '\0';
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
    if((ret_value = H5Tencode(type_id, NULL, &type_size)) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "unable to encode datatype");

    /* get Dataspace size to encode */
    if((ret_value = H5Sencode(space_id, NULL, &space_size)) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "unable to encode dataspace");

    /* get name size to encode */
    if(NULL != name)
        len = HDstrlen(name);

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
            HDmemcpy(p, (const uint8_t *)name, len);
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
        if((ret_value = H5Tencode(type_id, p, &type_size)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "unable to encode datatype");
        p += type_size;

        /* encode the dataspace size */
        UINT64ENCODE_VARLEN(p, space_size);
        /* encode datatspace */
        if((ret_value = H5Sencode(space_id, p, &space_size)) < 0)
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
        (*name)[len] = '\0';
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
    if((*type_id = H5Tdecode(p)) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDECODE, FAIL, "unable to decode datatype");
    p += type_size;

    /* decode the space size */
    UINT64DECODE_VARLEN(p, space_size);
    /* decode the dataspace */
    if((*space_id = H5Sdecode((const void *)p)) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDECODE, FAIL, "unable to decode dataspace");
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
        len = HDstrlen(name);

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
            HDmemcpy(p, (const uint8_t *)name, len);
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
        (*name)[len] = '\0';
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
    if((ret_value = H5Tencode(type_id, NULL, &type_size)) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "unable to encode datatype");

    /* get name size to encode */
    if(NULL != name)
        len = HDstrlen(name);

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
            HDmemcpy(p, (const uint8_t *)name, len);
        p += len;

        /* encode the datatype size */
        UINT64ENCODE_VARLEN(p, type_size);
        /* encode datatype */
        if((ret_value = H5Tencode(type_id, p, &type_size)) < 0)
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
        (*name)[len] = '\0';
        p += len;
    }

    /* decode the type size */
    UINT64DECODE_VARLEN(p, type_size);
    /* decode the datatype */
    if((*type_id = H5Tdecode(p)) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDECODE, FAIL, "unable to decode datatype");
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
        len = HDstrlen(name);

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
            HDmemcpy(p, (const uint8_t *)name, len);
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
        (*name)[len] = '\0';
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
        len = HDstrlen(name);

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
            HDmemcpy(p, (const uint8_t *)name, len);
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
        (*name)[len] = '\0';
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
        len = HDstrlen(name);

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
            HDmemcpy(p, (const uint8_t *)name, len);
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
        (*name)[len] = '\0';
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
