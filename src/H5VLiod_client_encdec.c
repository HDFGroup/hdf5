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
 * Purpose:	IOD plugin client encode/decode code
 */

#define H5P_PACKAGE		/*suppress error about including H5Ppkg	  */

#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Oprivate.h"         /* Object headers			*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5Ppkg.h"		/* Property lists			*/
#include "H5Sprivate.h"		/* Dataspaces				*/
#include "H5Tprivate.h"		/* Datatypes				*/
#include "H5VLprivate.h"	/* VOL plugins				*/
#include "H5VLiod.h"         /* Iod VOL plugin			*/
#include "H5VLiod_common.h"
#include "H5VLiod_client.h"

/*-------------------------------------------------------------------------
 * Function:	H5VL_client_encode_eff_init
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_client_encode_eff_init(fs_proc_t proc, void *_input)
{
    int *input = (int *)_input;
    size_t size, nalloc;
    void *buf;
    uint8_t *p;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    size = sizeof(int);

    nalloc = fs_proc_get_size(proc);

    if(nalloc < size)
        fs_proc_set_size(proc, size);

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;

    INT32ENCODE(p, *input);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_client_encode_eff_init() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_client_decode_eff_init
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_client_decode_eff_init(fs_proc_t proc, void *_output)
{
    herr_t *output = (herr_t *)_output;
    void *buf=NULL;
    uint8_t *p;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to decode from does not exist");

    p = (uint8_t *)buf;
    INT32DECODE(p, *output);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_client_decode_eff_init() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_client_encode_file_create
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_client_encode_file_create(fs_proc_t proc, void *_input)
{
    H5VL_iod_file_create_input_t *input = (H5VL_iod_file_create_input_t *)_input;
    size_t size, nalloc;
    size_t len = 0, fcpl_size = 0, fapl_size = 0;
    H5P_genplist_t *fcpl = NULL, *fapl = NULL;
    hid_t fcpl_id = input->fcpl_id;
    hid_t fapl_id = input->fapl_id;
    unsigned flags = input->flags;
    const char *name = input->name;
    void *buf;
    uint8_t *p;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

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

    size = sizeof(unsigned) + 
        1 + H5V_limit_enc_size((uint64_t)len) + len +
        1 + H5V_limit_enc_size((uint64_t)fapl_size) + fapl_size + 
        1 + H5V_limit_enc_size((uint64_t)fcpl_size) + fcpl_size;

    nalloc = fs_proc_get_size(proc);

    if(nalloc < size)
        fs_proc_set_size(proc, size);

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */

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

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_client_encode_file_create() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_client_decode_file_create
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_client_decode_file_create(fs_proc_t proc, void *_output)
{
    H5VL_iod_remote_file_t *output = (H5VL_iod_remote_file_t *)_output;
    void *buf=NULL;
    uint8_t *p;

    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to decode from does not exist");

    p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */

    /* decode the location with the container handle & iod object IDs and opened handles */
    UINT64DECODE_VARLEN(p, output->coh.cookie);
    UINT64DECODE_VARLEN(p, output->root_id.oid_hi);
    UINT64DECODE_VARLEN(p, output->root_id.oid_lo);
    UINT64DECODE_VARLEN(p, output->root_oh.cookie);
    UINT64DECODE_VARLEN(p, output->scratch_id.oid_hi);
    UINT64DECODE_VARLEN(p, output->scratch_id.oid_lo);
    UINT64DECODE_VARLEN(p, output->scratch_oh.cookie);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_client_decode_file_create() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_client_encode_file_open
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_client_encode_file_open(fs_proc_t proc, void *_input)
{
    H5VL_iod_file_open_input_t *input = (H5VL_iod_file_open_input_t *)_input;
    size_t size, nalloc;
    size_t len = 0, fapl_size = 0;
    H5P_genplist_t *fapl = NULL;
    hid_t fapl_id = input->fapl_id;
    unsigned flags = input->flags;
    const char *name = input->name;
    void *buf;
    uint8_t *p;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* get property list sizes */
    if(H5P_FILE_ACCESS_DEFAULT != fapl_id) {
        if(NULL == (fapl = (H5P_genplist_t *)H5I_object_verify(fapl_id, H5I_GENPROP_LST)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list");
        if((ret_value = H5P__encode(fapl, FALSE, NULL, &fapl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }
    len = HDstrlen(name) + 1;
    size = sizeof(unsigned) +
        1 + H5V_limit_enc_size((uint64_t)len) + len +  
        1 + H5V_limit_enc_size((uint64_t)fapl_size) + fapl_size;

    nalloc = fs_proc_get_size(proc);

    if(nalloc < size)
        fs_proc_set_size(proc, size);

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */

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

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_client_encode_file_open() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_client_decode_file_open
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_client_decode_file_open(fs_proc_t proc, void *_output)
{
    H5VL_iod_remote_file_t *output = (H5VL_iod_remote_file_t *)_output;
    void *buf=NULL;
    size_t fcpl_size = 0;
    uint8_t *p;

    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to decode from does not exist");

    p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */

    /* decode the location with the container handle & iod object IDs and opened handles */
    UINT64DECODE_VARLEN(p, output->coh.cookie);
    UINT64DECODE_VARLEN(p, output->root_id.oid_hi);
    UINT64DECODE_VARLEN(p, output->root_id.oid_lo);
    UINT64DECODE_VARLEN(p, output->root_oh.cookie);
    UINT64DECODE_VARLEN(p, output->scratch_id.oid_hi);
    UINT64DECODE_VARLEN(p, output->scratch_id.oid_lo);
    UINT64DECODE_VARLEN(p, output->scratch_oh.cookie);

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, fcpl_size);
    /* decode property lists if they are not default*/
    if(fcpl_size) {
        if((output->fcpl_id = H5P__decode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += fcpl_size;
    }
    else {
        output->fcpl_id = H5P_FILE_CREATE_DEFAULT;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_client_decode_file_open() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_client_encode_file_close
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_client_encode_file_close(fs_proc_t proc, void *_input)
{
    H5VL_iod_remote_file_t *input = (H5VL_iod_remote_file_t *)_input;
    size_t size, nalloc;
    void *buf;
    uint8_t *p;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    size = 1 + H5V_limit_enc_size((uint64_t)input->coh.cookie) + 
        1 + H5V_limit_enc_size((uint64_t)input->root_id.oid_hi) +
        1 + H5V_limit_enc_size((uint64_t)input->root_id.oid_lo) +
        1 + H5V_limit_enc_size((uint64_t)input->root_oh.cookie) + 
        1 + H5V_limit_enc_size((uint64_t)input->scratch_id.oid_hi) +
        1 + H5V_limit_enc_size((uint64_t)input->scratch_id.oid_lo) +
        1 + H5V_limit_enc_size((uint64_t)input->scratch_oh.cookie);

    nalloc = fs_proc_get_size(proc);

    if(nalloc < size)
        fs_proc_set_size(proc, size);

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;

    /* encode the location with the container handle & iod object IDs and opened handles */
    UINT64ENCODE_VARLEN(p, input->coh.cookie);
    UINT64ENCODE_VARLEN(p, input->root_id.oid_hi);
    UINT64ENCODE_VARLEN(p, input->root_id.oid_lo);
    UINT64ENCODE_VARLEN(p, input->root_oh.cookie);
    UINT64ENCODE_VARLEN(p, input->scratch_id.oid_hi);
    UINT64ENCODE_VARLEN(p, input->scratch_id.oid_lo);
    UINT64ENCODE_VARLEN(p, input->scratch_oh.cookie);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_client_encode_file_close() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_client_decode_file_close
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_client_decode_file_close(fs_proc_t proc, void *_output)
{
    int *output = (int *)_output;
    void *buf=NULL;
    uint8_t *p;

    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to decode from does not exist");

    p = (uint8_t *)buf;
    INT32DECODE(p, *output);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_client_decode_file_close() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_client_encode_group_create
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_client_encode_group_create(fs_proc_t proc, void *_input)
{
    H5VL_iod_group_create_input_t *input = (H5VL_iod_group_create_input_t *)_input;
    size_t size, nalloc;
    size_t len = 0, gcpl_size = 0, gapl_size = 0, lcpl_size = 0;
    H5P_genplist_t *gcpl = NULL, *gapl = NULL, *lcpl = NULL;
    const char *name = input->name;
    hid_t gcpl_id = input->gcpl_id;
    hid_t gapl_id = input->gapl_id;
    hid_t lcpl_id = input->lcpl_id;
    void *buf;
    uint8_t *p;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

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

    size = 1 + H5V_limit_enc_size((uint64_t)input->coh.cookie) + 
        1 + H5V_limit_enc_size((uint64_t)input->loc_id.oid_hi) +
        1 + H5V_limit_enc_size((uint64_t)input->loc_id.oid_lo) +
        1 + H5V_limit_enc_size((uint64_t)input->loc_oh.cookie) + 
        1 + H5V_limit_enc_size((uint64_t)len) + len + 
        1 + H5V_limit_enc_size((uint64_t)gapl_size) + gapl_size + 
        1 + H5V_limit_enc_size((uint64_t)gcpl_size) + gcpl_size + 
        1 + H5V_limit_enc_size((uint64_t)lcpl_size) + lcpl_size;

    nalloc = fs_proc_get_size(proc);

    if(nalloc < size)
        fs_proc_set_size(proc, size);

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */

    /* encode the location with the container handle & iod object IDs and opened handles */
    UINT64ENCODE_VARLEN(p, input->coh.cookie);
    UINT64ENCODE_VARLEN(p, input->loc_id.oid_hi);
    UINT64ENCODE_VARLEN(p, input->loc_id.oid_lo);
    UINT64ENCODE_VARLEN(p, input->loc_oh.cookie);

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

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_client_encode_group_create() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_client_decode_group_create
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_client_decode_group_create(fs_proc_t proc, void *_output)
{
    H5VL_iod_remote_group_t *output = (H5VL_iod_remote_group_t *)_output;
    void *buf=NULL;
    uint8_t *p;

    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to decode from does not exist");

    p = (uint8_t *)buf;

    /* decode the location with the container handle & iod object IDs and opened handles */
    UINT64DECODE_VARLEN(p, output->iod_id.oid_hi);
    UINT64DECODE_VARLEN(p, output->iod_id.oid_lo);
    UINT64DECODE_VARLEN(p, output->iod_oh.cookie);
    UINT64DECODE_VARLEN(p, output->scratch_id.oid_hi);
    UINT64DECODE_VARLEN(p, output->scratch_id.oid_lo);
    UINT64DECODE_VARLEN(p, output->scratch_oh.cookie);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_client_decode_group_create() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_client_encode_group_open
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_client_encode_group_open(fs_proc_t proc, void *_input)
{
    H5VL_iod_group_open_input_t *input = (H5VL_iod_group_open_input_t *)_input;
    size_t size, nalloc;
    size_t len = 0, gapl_size = 0;
    H5P_genplist_t *gapl = NULL;
    const char *name = input->name;
    hid_t gapl_id = input->gapl_id;
    void *buf;
    uint8_t *p;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

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

    size = 1 + H5V_limit_enc_size((uint64_t)input->coh.cookie) + 
        1 + H5V_limit_enc_size((uint64_t)input->loc_id.oid_hi) +
        1 + H5V_limit_enc_size((uint64_t)input->loc_id.oid_lo) +
        1 + H5V_limit_enc_size((uint64_t)input->loc_oh.cookie) +
        1 + H5V_limit_enc_size((uint64_t)len) + len + 
        1 + H5V_limit_enc_size((uint64_t)gapl_size) + gapl_size;

    nalloc = fs_proc_get_size(proc);

    if(nalloc < size)
        fs_proc_set_size(proc, size);

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */

    /* encode the location with the container handle & iod object IDs and opened handles */
    UINT64ENCODE_VARLEN(p, input->coh.cookie);
    UINT64ENCODE_VARLEN(p, input->loc_id.oid_hi);
    UINT64ENCODE_VARLEN(p, input->loc_id.oid_lo);
    UINT64ENCODE_VARLEN(p, input->loc_oh.cookie);

    /* encode length of the group name and the actual group name */
    UINT64ENCODE_VARLEN(p, len);
    if(NULL != name)
        HDstrcpy((char *)p, name);
    p += len;

    /* encode the plist size */
    UINT64ENCODE_VARLEN(p, gapl_size);
    /* encode property lists if they are not default*/
    if(gapl_size) {
        if((ret_value = H5P__encode(gapl, FALSE, p, &gapl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
        p += gapl_size;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_client_encode_group_open() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_client_decode_group_open
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_client_decode_group_open(fs_proc_t proc, void *_output)
{
    H5VL_iod_remote_group_t *output = (H5VL_iod_remote_group_t *)_output;
    void *buf=NULL;
    size_t gcpl_size = 0;
    uint8_t *p;

    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to decode from does not exist");

    p = (uint8_t *)buf;

    /* decode the location with the container handle & iod object IDs and opened handles */
    UINT64DECODE_VARLEN(p, output->iod_id.oid_hi);
    UINT64DECODE_VARLEN(p, output->iod_id.oid_lo);
    UINT64DECODE_VARLEN(p, output->iod_oh.cookie);
    UINT64DECODE_VARLEN(p, output->scratch_id.oid_hi);
    UINT64DECODE_VARLEN(p, output->scratch_id.oid_lo);
    UINT64DECODE_VARLEN(p, output->scratch_oh.cookie);

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, gcpl_size);
    /* decode property lists if they are not default*/
    if(gcpl_size) {
        if((output->gcpl_id = H5P__decode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += gcpl_size;
    }
    else {
        output->gcpl_id = H5P_FILE_CREATE_DEFAULT;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_client_decode_group_open() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_client_encode_group_close
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_client_encode_group_close(fs_proc_t proc, void *_input)
{
    H5VL_iod_remote_group_t *input = (H5VL_iod_remote_group_t *)_input;
    size_t size, nalloc;
    void *buf;
    uint8_t *p;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    size = 1 + H5V_limit_enc_size((uint64_t)input->iod_id.oid_hi) +
        1 + H5V_limit_enc_size((uint64_t)input->iod_id.oid_lo) +
        1 + H5V_limit_enc_size((uint64_t)input->iod_oh.cookie) +
        1 + H5V_limit_enc_size((uint64_t)input->scratch_id.oid_hi) +
        1 + H5V_limit_enc_size((uint64_t)input->scratch_id.oid_lo) +
        1 + H5V_limit_enc_size((uint64_t)input->scratch_oh.cookie);

    nalloc = fs_proc_get_size(proc);

    if(nalloc < size)
        fs_proc_set_size(proc, size);

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */

    /* encode the location with the container handle & iod object IDs and opened handles */
    UINT64ENCODE_VARLEN(p, input->iod_id.oid_hi);
    UINT64ENCODE_VARLEN(p, input->iod_id.oid_lo)
    UINT64ENCODE_VARLEN(p, input->iod_oh.cookie);
    UINT64ENCODE_VARLEN(p, input->scratch_id.oid_hi);
    UINT64ENCODE_VARLEN(p, input->scratch_id.oid_lo);
    UINT64ENCODE_VARLEN(p, input->scratch_oh.cookie);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_client_encode_group_close() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_client_decode_group_close
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_client_decode_group_close(fs_proc_t proc, void *_output)
{
    int *output = (int *)_output;
    void *buf=NULL;
    uint8_t *p;

    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to decode from does not exist");

    p = (uint8_t *)buf;
    INT32DECODE(p, *output);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_client_decode_group_close() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_client_encode_dset_create
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_client_encode_dset_create(fs_proc_t proc, void *_input)
{
    H5VL_iod_dset_create_input_t *input = (H5VL_iod_dset_create_input_t *)_input;
    size_t size, nalloc;
    size_t len = 0, dcpl_size = 0, dapl_size = 0, lcpl_size = 0;
    size_t type_size = 0, space_size = 0;
    H5P_genplist_t *dcpl = NULL, *dapl = NULL, *lcpl = NULL;
    H5T_t *dtype = NULL;
    H5S_t *dspace = NULL;
    const char *name = input->name;
    hid_t dcpl_id = input->dcpl_id;
    hid_t dapl_id = input->dapl_id;
    hid_t lcpl_id = input->lcpl_id;
    hid_t type_id = input->type_id;
    hid_t space_id = input->space_id;
    void *buf;
    uint8_t *p;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

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

    size = 1 + H5V_limit_enc_size((uint64_t)input->coh.cookie) + 
        1 + H5V_limit_enc_size((uint64_t)input->loc_id.oid_hi) +
        1 + H5V_limit_enc_size((uint64_t)input->loc_id.oid_lo) +
        1 + H5V_limit_enc_size((uint64_t)input->loc_oh.cookie) + 
        1 + H5V_limit_enc_size((uint64_t)len) + len + 
        1 + H5V_limit_enc_size((uint64_t)dapl_size) + dapl_size + 
        1 + H5V_limit_enc_size((uint64_t)dcpl_size) + dcpl_size + 
        1 + H5V_limit_enc_size((uint64_t)lcpl_size) + lcpl_size + 
        1 + H5V_limit_enc_size((uint64_t)type_size) + type_size + 
        1 + H5V_limit_enc_size((uint64_t)space_size) + space_size;

    nalloc = fs_proc_get_size(proc);

    if(nalloc < size)
        fs_proc_set_size(proc, size);

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */

    /* encode the location with the container handle & iod object IDs and opened handles */
    UINT64ENCODE_VARLEN(p, input->coh.cookie);
    UINT64ENCODE_VARLEN(p, input->loc_id.oid_hi);
    UINT64ENCODE_VARLEN(p, input->loc_id.oid_lo);
    UINT64ENCODE_VARLEN(p, input->loc_oh.cookie);

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

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_client_encode_dset_create() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_client_decode_dset_create
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_client_decode_dset_create(fs_proc_t proc, void *_output)
{
    H5VL_iod_remote_dset_t *output = (H5VL_iod_remote_dset_t *)_output;
    void *buf=NULL;
    uint8_t *p;

    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to decode from does not exist");

    p = (uint8_t *)buf;

    /* decode the location with the container handle & iod object IDs and opened handles */
    UINT64DECODE_VARLEN(p, output->iod_id.oid_hi);
    UINT64DECODE_VARLEN(p, output->iod_id.oid_lo);
    UINT64DECODE_VARLEN(p, output->iod_oh.cookie);
    UINT64DECODE_VARLEN(p, output->scratch_id.oid_hi);
    UINT64DECODE_VARLEN(p, output->scratch_id.oid_lo);
    UINT64DECODE_VARLEN(p, output->scratch_oh.cookie);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_client_decode_dset_create() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_client_encode_dset_open
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_client_encode_dset_open(fs_proc_t proc, void *_input)
{
    H5VL_iod_dset_open_input_t *input = (H5VL_iod_dset_open_input_t *)_input;
    size_t size, nalloc;
    size_t len = 0, dapl_size = 0;
    H5P_genplist_t *dapl = NULL;
    const char *name = input->name;
    hid_t dapl_id = input->dapl_id;
    void *buf;
    uint8_t *p;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

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

    size = 1 + H5V_limit_enc_size((uint64_t)input->coh.cookie) + 
        1 + H5V_limit_enc_size((uint64_t)input->loc_id.oid_hi) +
        1 + H5V_limit_enc_size((uint64_t)input->loc_id.oid_lo) +
        1 + H5V_limit_enc_size((uint64_t)input->loc_oh.cookie) + 
        1 + H5V_limit_enc_size((uint64_t)len) + len + 
        1 + H5V_limit_enc_size((uint64_t)dapl_size) + dapl_size;

    nalloc = fs_proc_get_size(proc);

    if(nalloc < size)
        fs_proc_set_size(proc, size);

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */

    /* encode the location with the container handle & iod object IDs and opened handles */
    UINT64ENCODE_VARLEN(p, input->coh.cookie);
    UINT64ENCODE_VARLEN(p, input->loc_id.oid_hi);
    UINT64ENCODE_VARLEN(p, input->loc_id.oid_lo);
    UINT64ENCODE_VARLEN(p, input->loc_oh.cookie);

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

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_client_encode_dset_open() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_client_decode_dset_open
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_client_decode_dset_open(fs_proc_t proc, void *_output)
{
    H5VL_iod_remote_dset_t *output = (H5VL_iod_remote_dset_t *)_output;
    void *buf=NULL;
    size_t dcpl_size = 0;
    uint8_t *p;

    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to decode from does not exist");

    p = (uint8_t *)buf;

    /* decode the location with the container handle & iod object IDs and opened handles */
    UINT64DECODE_VARLEN(p, output->iod_id.oid_hi);
    UINT64DECODE_VARLEN(p, output->iod_id.oid_lo);
    UINT64DECODE_VARLEN(p, output->iod_oh.cookie);
    UINT64DECODE_VARLEN(p, output->scratch_id.oid_hi);
    UINT64DECODE_VARLEN(p, output->scratch_id.oid_lo);
    UINT64DECODE_VARLEN(p, output->scratch_oh.cookie);

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, dcpl_size);
    /* decode property lists if they are not default*/
    if(dcpl_size) {
        if((output->dcpl_id = H5P__decode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += dcpl_size;
    }
    else {
        output->dcpl_id = H5P_FILE_CREATE_DEFAULT;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_client_decode_dset_open() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_client_encode_dset_close
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_client_encode_dset_close(fs_proc_t proc, void *_input)
{
    H5VL_iod_remote_dset_t *input = (H5VL_iod_remote_dset_t *)_input;
    size_t size, nalloc;
    void *buf;
    uint8_t *p;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    size = 1 + H5V_limit_enc_size((uint64_t)input->iod_id.oid_hi) +
        1 + H5V_limit_enc_size((uint64_t)input->iod_id.oid_lo) +
        1 + H5V_limit_enc_size((uint64_t)input->iod_oh.cookie) +
        1 + H5V_limit_enc_size((uint64_t)input->scratch_id.oid_hi) +
        1 + H5V_limit_enc_size((uint64_t)input->scratch_id.oid_lo) +
        1 + H5V_limit_enc_size((uint64_t)input->scratch_oh.cookie);

    nalloc = fs_proc_get_size(proc);

    if(nalloc < size)
        fs_proc_set_size(proc, size);

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */

    /* encode the location with the container handle & iod object IDs and opened handles */
    UINT64ENCODE_VARLEN(p, input->iod_id.oid_hi);
    UINT64ENCODE_VARLEN(p, input->iod_id.oid_lo);
    UINT64ENCODE_VARLEN(p, input->iod_oh.cookie);
    UINT64ENCODE_VARLEN(p, input->scratch_id.oid_hi);
    UINT64ENCODE_VARLEN(p, input->scratch_id.oid_lo);
    UINT64ENCODE_VARLEN(p, input->scratch_oh.cookie);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_client_encode_dset_close() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_client_decode_dset_close
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_client_decode_dset_close(fs_proc_t proc, void *_output)
{
    int *output = (int *)_output;
    void *buf=NULL;
    uint8_t *p;

    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to decode from does not exist");

    p = (uint8_t *)buf;
    INT32DECODE(p, *output);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_client_decode_dset_close() */
