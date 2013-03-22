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

#define H5P_PACKAGE		/*suppress error about including H5Ppkg	  */

#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5VLprivate.h"	/* VOL plugins				*/
#include "H5VLiod.h"            /* Iod VOL plugin			*/
#include "H5VLiod_common.h"
#include "H5VLiod_server.h"

/*
 * Programmer:  Mohamad Chaarawi <chaarawi@hdfgroup.gov>
 *              February, 2012
 *
 * Purpose:	The IOD plugin server encode/decode code
 */

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_decode_eff_init_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_server_decode_eff_init(fs_proc_t proc, void *_input)
{
    int *input = (int *)_input;
    void *buf=NULL;
    uint8_t *p;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;

    INT32DECODE(p, *input);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_decode_eff_init() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_encode_eff_init_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_server_encode_eff_init(fs_proc_t proc, void *_output)
{
    herr_t *output = (herr_t *)_output;
    void *buf = NULL;
    uint8_t *p;
    size_t size, nalloc;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    size = sizeof(herr_t);

    nalloc = fs_proc_get_size(proc);

    if(nalloc < size)
        fs_proc_set_size(proc, size);

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;

    INT32ENCODE(p, output);

done:
    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_iod_server_encode_eff_init() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_decode_file_create_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_server_decode_file_create(fs_proc_t proc, void *_input)
{
    H5VL_iod_file_create_input_t *input = (H5VL_iod_file_create_input_t *)_input;
    void *buf=NULL;
    uint8_t *p;
    size_t len = 0; /* length of name (decoded) */
    size_t fcpl_size = 0, fapl_size = 0; /* plist sizes */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;

    /* decode length of name and name */
    UINT64DECODE_VARLEN(p, len);
    input->name = H5MM_xstrdup((const char *)(p));
    p += len;

    /* deocde create flags */
    H5_DECODE_UNSIGNED(p, input->flags);

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, fcpl_size);
    /* decode property lists if they are not default*/
    if(fcpl_size) {
        if((input->fcpl_id = H5Pdecode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += fcpl_size;
    }
    else {
        input->fcpl_id = H5P_FILE_CREATE_DEFAULT;
    }

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, fapl_size);
    /* decode property lists if they are not default*/
    if(fapl_size) {
        if((input->fapl_id = H5Pdecode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += fapl_size;
    }
    else {
        input->fapl_id = H5P_FILE_ACCESS_DEFAULT;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_decode_file_create() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_encode_file_create_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_server_encode_file_create(fs_proc_t proc, void *_output)
{
    H5VL_iod_remote_file_t *output = (H5VL_iod_remote_file_t *)_output;
    void *buf = NULL;
    uint8_t *p;
    size_t size, nalloc;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    size = 1 + H5V_limit_enc_size((uint64_t)output->coh.cookie) + 
        1 + H5V_limit_enc_size((uint64_t)output->root_id.oid_hi) +
        1 + H5V_limit_enc_size((uint64_t)output->root_id.oid_lo) +
        1 + H5V_limit_enc_size((uint64_t)output->root_oh.cookie) + 
        1 + H5V_limit_enc_size((uint64_t)output->scratch_id.oid_hi) +
        1 + H5V_limit_enc_size((uint64_t)output->scratch_id.oid_lo) +
        1 + H5V_limit_enc_size((uint64_t)output->scratch_oh.cookie);

    nalloc = fs_proc_get_size(proc);

    if(nalloc < size)
        fs_proc_set_size(proc, size);

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;

    UINT64ENCODE_VARLEN(p, output->coh.cookie);
    UINT64ENCODE_VARLEN(p, output->root_id.oid_hi);
    UINT64ENCODE_VARLEN(p, output->root_id.oid_lo);
    UINT64ENCODE_VARLEN(p, output->root_oh.cookie);
    UINT64ENCODE_VARLEN(p, output->scratch_id.oid_hi);
    UINT64ENCODE_VARLEN(p, output->scratch_id.oid_lo);
    UINT64ENCODE_VARLEN(p, output->scratch_oh.cookie);

done:
    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_iod_server_encode_file_create() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_decode_file_open_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_server_decode_file_open(fs_proc_t proc, void *_input)
{
    H5VL_iod_file_open_input_t *input = (H5VL_iod_file_open_input_t *)_input;
    void *buf=NULL;
    uint8_t *p;
    size_t len = 0; /* length of name (decoded) */
    size_t fapl_size = 0; /* plist sizes */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;

    /* decode length of name and name */
    UINT64DECODE_VARLEN(p, len);
    input->name = H5MM_xstrdup((const char *)(p));
    p += len;

    /* deocde open flags */
    H5_DECODE_UNSIGNED(p, input->flags);

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, fapl_size);
    /* decode property lists if they are not default*/
    if(fapl_size) {
        if((input->fapl_id = H5Pdecode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += fapl_size;
    }
    else {
        input->fapl_id = H5P_FILE_ACCESS_DEFAULT;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_decode_file_open() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_encode_file_open_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_server_encode_file_open(fs_proc_t proc, void *_output)
{
    H5VL_iod_server_remote_file_t *output = (H5VL_iod_server_remote_file_t *)_output;
    void *buf = NULL;
    size_t fcpl_size = output->fcpl_size;
    uint8_t *p;
    size_t size, nalloc;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    size = 1 + H5V_limit_enc_size((uint64_t)output->coh.cookie) + 
        1 + H5V_limit_enc_size((uint64_t)output->root_id.oid_hi) +
        1 + H5V_limit_enc_size((uint64_t)output->root_id.oid_lo) +
        1 + H5V_limit_enc_size((uint64_t)output->root_oh.cookie) + 
        1 + H5V_limit_enc_size((uint64_t)output->scratch_id.oid_hi) +
        1 + H5V_limit_enc_size((uint64_t)output->scratch_id.oid_lo) +
        1 + H5V_limit_enc_size((uint64_t)output->scratch_oh.cookie) + 
        1 + H5V_limit_enc_size((uint64_t)fcpl_size) + fcpl_size;

    nalloc = fs_proc_get_size(proc);

    if(nalloc < size)
        fs_proc_set_size(proc, size);

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;

    UINT64ENCODE_VARLEN(p, output->coh.cookie);
    UINT64ENCODE_VARLEN(p, output->root_id.oid_hi);
    UINT64ENCODE_VARLEN(p, output->root_id.oid_lo);
    UINT64ENCODE_VARLEN(p, output->root_oh.cookie);
    UINT64ENCODE_VARLEN(p, output->scratch_id.oid_hi);
    UINT64ENCODE_VARLEN(p, output->scratch_id.oid_lo);
    UINT64ENCODE_VARLEN(p, output->scratch_oh.cookie);

    /* encode the length of plist and the plist buffer (already encoded) */
    UINT64ENCODE_VARLEN(p, fcpl_size);
    if(0 != fcpl_size) {
        HDmemcpy(p, output->fcpl, fcpl_size);
        p += fcpl_size;
    }

done:
    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_iod_server_encode_file_open() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_decode_file_flush_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_server_decode_file_flush(fs_proc_t proc, void *_input)
{
    H5VL_iod_file_flush_input_t *input = (H5VL_iod_file_flush_input_t *)_input;
    void *buf=NULL;
    uint8_t *p;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;

    input->scope = (H5F_scope_t)*p++;
    UINT64DECODE_VARLEN(p, input->coh.cookie);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_decode_file_flush() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_encode_file_flush_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_server_encode_file_flush(fs_proc_t proc, void *_output)
{
    int output = *((int *)_output);
    void *buf = NULL;
    uint8_t *p;
    size_t size, nalloc;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    size = sizeof(int32_t);

    nalloc = fs_proc_get_size(proc);

    if(nalloc < size)
        fs_proc_set_size(proc, size);

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;

    INT32ENCODE(p, output);

done:
    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_iod_server_encode_file_flush() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_decode_file_close_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_server_decode_file_close(fs_proc_t proc, void *_input)
{
    H5VL_iod_remote_file_t *input = (H5VL_iod_remote_file_t *)_input;
    void *buf=NULL;
    uint8_t *p;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;

    /* decode the location with the container handle & iod object IDs and opened handles */
    UINT64DECODE_VARLEN(p, input->coh.cookie);
    UINT64DECODE_VARLEN(p, input->root_id.oid_hi);
    UINT64DECODE_VARLEN(p, input->root_id.oid_lo);
    UINT64DECODE_VARLEN(p, input->root_oh.cookie);
    UINT64DECODE_VARLEN(p, input->scratch_id.oid_hi);
    UINT64DECODE_VARLEN(p, input->scratch_id.oid_lo);
    UINT64DECODE_VARLEN(p, input->scratch_oh.cookie);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_decode_file_close() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_encode_file_close_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_server_encode_file_close(fs_proc_t proc, void *_output)
{
    int output = *((int *)_output);
    void *buf = NULL;
    uint8_t *p;
    size_t size, nalloc;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    size = sizeof(int32_t);

    nalloc = fs_proc_get_size(proc);

    if(nalloc < size)
        fs_proc_set_size(proc, size);

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;

    INT32ENCODE(p, output);

done:
    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_iod_server_encode_file_close() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_decode_group_create_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_server_decode_group_create(fs_proc_t proc, void *_input)
{
    H5VL_iod_group_create_input_t *input = (H5VL_iod_group_create_input_t *)_input;
    void *buf=NULL;
    uint8_t *p;
    size_t len = 0; /* len of group name */
    size_t gcpl_size = 0, gapl_size = 0, lcpl_size = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;

    /* decode the location with the container handle & iod object IDs and opened handles */
    UINT64DECODE_VARLEN(p, input->coh.cookie);
    UINT64DECODE_VARLEN(p, input->loc_id.oid_hi);
    UINT64DECODE_VARLEN(p, input->loc_id.oid_lo);
    UINT64DECODE_VARLEN(p, input->loc_oh.cookie);

    /* decode length of the group name and the actual group name */
    UINT64DECODE_VARLEN(p, len);
    if(0 != len) {
        input->name = H5MM_xstrdup((const char *)(p));
        p += len;
    }

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, gcpl_size);
    /* decode property lists if they are not default*/
    if(gcpl_size) {
        if((input->gcpl_id = H5Pdecode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += gcpl_size;
    }
    else {
        input->gcpl_id = H5P_GROUP_CREATE_DEFAULT;
    }

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, gapl_size);
    /* decode property lists if they are not default*/
    if(gapl_size) {
        if((input->gapl_id = H5Pdecode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += gapl_size;
    }
    else {
        input->gapl_id = H5P_GROUP_ACCESS_DEFAULT;
    }

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, lcpl_size);
    /* decode property lists if they are not default*/
    if(lcpl_size) {
        if((input->lcpl_id = H5Pdecode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += lcpl_size;
    }
    else {
        input->lcpl_id = H5P_LINK_CREATE_DEFAULT;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_decode_group_create() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_encode_group_create_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_server_encode_group_create(fs_proc_t proc, void *_output)
{
    H5VL_iod_remote_group_t *output = (H5VL_iod_remote_group_t *)_output;
    void *buf = NULL;
    uint8_t *p;
    size_t size, nalloc;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    size = 1 + H5V_limit_enc_size((uint64_t)output->iod_id.oid_hi) +
        1 + H5V_limit_enc_size((uint64_t)output->iod_id.oid_lo) +
        1 + H5V_limit_enc_size((uint64_t)output->iod_oh.cookie) + 
        1 + H5V_limit_enc_size((uint64_t)output->scratch_id.oid_hi) +
        1 + H5V_limit_enc_size((uint64_t)output->scratch_id.oid_lo) +
        1 + H5V_limit_enc_size((uint64_t)output->scratch_oh.cookie);

    nalloc = fs_proc_get_size(proc);

    if(nalloc < size)
        fs_proc_set_size(proc, size);

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;

    UINT64ENCODE_VARLEN(p, output->iod_id.oid_hi);
    UINT64ENCODE_VARLEN(p, output->iod_id.oid_lo);
    UINT64ENCODE_VARLEN(p, output->iod_oh.cookie);
    UINT64ENCODE_VARLEN(p, output->scratch_id.oid_hi);
    UINT64ENCODE_VARLEN(p, output->scratch_id.oid_lo);
    UINT64ENCODE_VARLEN(p, output->scratch_oh.cookie);

done:
    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_iod_server_encode_group_create() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_decode_group_open_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_server_decode_group_open(fs_proc_t proc, void *_input)
{
    H5VL_iod_group_open_input_t *input = (H5VL_iod_group_open_input_t *)_input;
    void *buf=NULL;
    uint8_t *p;
    size_t len = 0; /* len of group name */
    size_t gapl_size = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;

    /* decode the location with the container handle & iod object IDs and opened handles */
    UINT64DECODE_VARLEN(p, input->coh.cookie);
    UINT64DECODE_VARLEN(p, input->loc_id.oid_hi);
    UINT64DECODE_VARLEN(p, input->loc_id.oid_lo);
    UINT64DECODE_VARLEN(p, input->loc_oh.cookie);

    /* decode length of the group name and the actual group name */
    UINT64DECODE_VARLEN(p, len);
    if(0 != len) {
        input->name = H5MM_xstrdup((const char *)(p));
        p += len;
    }

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, gapl_size);
    /* decode property lists if they are not default*/
    if(gapl_size) {
        if((input->gapl_id = H5Pdecode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += gapl_size;
    }
    else {
        input->gapl_id = H5P_GROUP_ACCESS_DEFAULT;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_decode_group_open() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_encode_group_open_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_server_encode_group_open(fs_proc_t proc, void *_output)
{
    H5VL_iod_server_remote_group_t *output = (H5VL_iod_server_remote_group_t *)_output;
    size_t gcpl_size = output->gcpl_size;
    void *buf = NULL;
    uint8_t *p;
    size_t size, nalloc;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    size = 1 + H5V_limit_enc_size((uint64_t)output->iod_id.oid_hi) +
        1 + H5V_limit_enc_size((uint64_t)output->iod_id.oid_lo) +
        1 + H5V_limit_enc_size((uint64_t)output->iod_oh.cookie) + 
        1 + H5V_limit_enc_size((uint64_t)output->scratch_id.oid_hi) +
        1 + H5V_limit_enc_size((uint64_t)output->scratch_id.oid_lo) +
        1 + H5V_limit_enc_size((uint64_t)output->scratch_oh.cookie) +
        1 + H5V_limit_enc_size((uint64_t)gcpl_size) + gcpl_size;;

    nalloc = fs_proc_get_size(proc);

    if(nalloc < size)
        fs_proc_set_size(proc, size);

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;

    UINT64ENCODE_VARLEN(p, output->iod_id.oid_hi);
    UINT64ENCODE_VARLEN(p, output->iod_id.oid_lo);
    UINT64ENCODE_VARLEN(p, output->iod_oh.cookie);
    UINT64ENCODE_VARLEN(p, output->scratch_id.oid_hi);
    UINT64ENCODE_VARLEN(p, output->scratch_id.oid_lo);
    UINT64ENCODE_VARLEN(p, output->scratch_oh.cookie);

    /* encode the length of plist and the plist buffer (already encoded) */
    UINT64ENCODE_VARLEN(p, gcpl_size);
    if(0 != gcpl_size) {
        HDmemcpy(p, output->gcpl, gcpl_size);
        p += gcpl_size;
    }

done:
    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_iod_server_encode_group_open() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_decode_group_close_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_server_decode_group_close(fs_proc_t proc, void *_input)
{
    H5VL_iod_remote_group_t *input = (H5VL_iod_remote_group_t *)_input;
    void *buf=NULL;
    uint8_t *p;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;

    /* decode the location with the container handle & iod object IDs and opened handles */
    UINT64DECODE_VARLEN(p, input->iod_id.oid_hi);
    UINT64DECODE_VARLEN(p, input->iod_id.oid_lo);
    UINT64DECODE_VARLEN(p, input->iod_oh.cookie);
    UINT64DECODE_VARLEN(p, input->scratch_id.oid_hi);
    UINT64DECODE_VARLEN(p, input->scratch_id.oid_lo);
    UINT64DECODE_VARLEN(p, input->scratch_oh.cookie);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_decode_group_close() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_encode_group_close_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_server_encode_group_close(fs_proc_t proc, void *_output)
{
    int output = *((int *)_output);
    void *buf = NULL;
    uint8_t *p;
    size_t size, nalloc;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    size = sizeof(int32_t);

    nalloc = fs_proc_get_size(proc);

    if(nalloc < size)
        fs_proc_set_size(proc, size);

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;

    INT32ENCODE(p, output);

done:
    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_iod_server_encode_group_close() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_decode_dset_create_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_server_decode_dset_create(fs_proc_t proc, void *_input)
{
    H5VL_iod_dset_create_input_t *input = (H5VL_iod_dset_create_input_t *)_input;
    void *buf=NULL;
    uint8_t *p;
    size_t len = 0; /* len of dset name */
    size_t dcpl_size = 0, dapl_size = 0, lcpl_size = 0;
    size_t space_size = 0, type_size = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;

    /* decode the location with the container handle & iod object IDs and opened handles */
    UINT64DECODE_VARLEN(p, input->coh.cookie);
    UINT64DECODE_VARLEN(p, input->loc_id.oid_hi);
    UINT64DECODE_VARLEN(p, input->loc_id.oid_lo);
    UINT64DECODE_VARLEN(p, input->loc_oh.cookie);

    /* decode length of the dset name and the actual dset name */
    UINT64DECODE_VARLEN(p, len);
    if(0 != len) {
        input->name = H5MM_xstrdup((const char *)(p));
        p += len;
    }

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, dcpl_size);
    /* decode property lists if they are not default*/
    if(dcpl_size) {
        if((input->dcpl_id = H5Pdecode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += dcpl_size;
    }
    else {
        input->dcpl_id = H5P_DATASET_CREATE_DEFAULT;
    }

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, dapl_size);
    /* decode property lists if they are not default*/
    if(dapl_size) {
        if((input->dapl_id = H5Pdecode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += dapl_size;
    }
    else {
        input->dapl_id = H5P_DATASET_ACCESS_DEFAULT;
    }

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, lcpl_size);
    /* decode property lists if they are not default*/
    if(lcpl_size) {
        if((input->lcpl_id = H5Pdecode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += lcpl_size;
    }
    else {
        input->lcpl_id = H5P_LINK_CREATE_DEFAULT;
    }

    /* decode the type size */
    UINT64DECODE_VARLEN(p, type_size);
    if((input->type_id = H5Tdecode((const void *)p)) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDECODE, FAIL, "can't decode object");
    p += type_size;

    /* decode the space size */
    UINT64DECODE_VARLEN(p, space_size);
    if((input->space_id = H5Sdecode((const void *)p)) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDECODE, FAIL, "can't decode object");
    p += space_size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_decode_dset_create() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_encode_dset_create_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_server_encode_dset_create(fs_proc_t proc, void *_output)
{
    H5VL_iod_remote_dset_t *output = (H5VL_iod_remote_dset_t *)_output;
    void *buf = NULL;
    uint8_t *p;
    size_t size, nalloc;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    size = 1 + H5V_limit_enc_size((uint64_t)output->iod_id.oid_hi) +
        1 + H5V_limit_enc_size((uint64_t)output->iod_id.oid_lo) +
        1 + H5V_limit_enc_size((uint64_t)output->iod_oh.cookie) + 
        1 + H5V_limit_enc_size((uint64_t)output->scratch_id.oid_hi) +
        1 + H5V_limit_enc_size((uint64_t)output->scratch_id.oid_lo) +
        1 + H5V_limit_enc_size((uint64_t)output->scratch_oh.cookie);

    nalloc = fs_proc_get_size(proc);

    if(nalloc < size)
        fs_proc_set_size(proc, size);

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;

    UINT64ENCODE_VARLEN(p, output->iod_id.oid_hi);
    UINT64ENCODE_VARLEN(p, output->iod_id.oid_lo);
    UINT64ENCODE_VARLEN(p, output->iod_oh.cookie);
    UINT64ENCODE_VARLEN(p, output->scratch_id.oid_hi);
    UINT64ENCODE_VARLEN(p, output->scratch_id.oid_lo);
    UINT64ENCODE_VARLEN(p, output->scratch_oh.cookie);

done:
    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_iod_server_encode_dset_create() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_decode_dset_open_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_server_decode_dset_open(fs_proc_t proc, void *_input)
{
    H5VL_iod_dset_open_input_t *input = (H5VL_iod_dset_open_input_t *)_input;
    void *buf=NULL;
    uint8_t *p;
    size_t len = 0; /* len of dset name */
    size_t dapl_size = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;

    /* decode the location with the container handle & iod object IDs and opened handles */
    UINT64DECODE_VARLEN(p, input->coh.cookie);
    UINT64DECODE_VARLEN(p, input->loc_id.oid_hi);
    UINT64DECODE_VARLEN(p, input->loc_id.oid_lo);
    UINT64DECODE_VARLEN(p, input->loc_oh.cookie);

    /* decode length of the dset name and the actual dset name */
    UINT64DECODE_VARLEN(p, len);
    if(0 != len) {
        input->name = H5MM_xstrdup((const char *)(p));
        p += len;
    }

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, dapl_size);
    /* decode property lists if they are not default*/
    if(dapl_size) {
        if((input->dapl_id = H5Pdecode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += dapl_size;
    }
    else {
        input->dapl_id = H5P_DATASET_ACCESS_DEFAULT;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_decode_dset_open() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_encode_dset_open_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_server_encode_dset_open(fs_proc_t proc, void *_output)
{
    H5VL_iod_server_remote_dset_t *output = (H5VL_iod_server_remote_dset_t *)_output;
    void *buf = NULL;
    uint8_t *p;
    size_t size, nalloc;
    size_t type_size = output->dtype_size;
    size_t space_size = output->dspace_size;
    size_t dcpl_size = output->dcpl_size;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    size = 1 + H5V_limit_enc_size((uint64_t)output->iod_id.oid_hi) +
        1 + H5V_limit_enc_size((uint64_t)output->iod_id.oid_lo) +
        1 + H5V_limit_enc_size((uint64_t)output->iod_oh.cookie) + 
        1 + H5V_limit_enc_size((uint64_t)output->scratch_id.oid_hi) +
        1 + H5V_limit_enc_size((uint64_t)output->scratch_id.oid_lo) +
        1 + H5V_limit_enc_size((uint64_t)output->scratch_oh.cookie) +
        1 + H5V_limit_enc_size((uint64_t)type_size) + type_size +
        1 + H5V_limit_enc_size((uint64_t)space_size) + space_size +
        1 + H5V_limit_enc_size((uint64_t)dcpl_size) + dcpl_size;

    nalloc = fs_proc_get_size(proc);

    if(nalloc < size)
        fs_proc_set_size(proc, size);

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;

    UINT64ENCODE_VARLEN(p, output->iod_id.oid_hi);
    UINT64ENCODE_VARLEN(p, output->iod_id.oid_lo);
    UINT64ENCODE_VARLEN(p, output->iod_oh.cookie);
    UINT64ENCODE_VARLEN(p, output->scratch_id.oid_hi);
    UINT64ENCODE_VARLEN(p, output->scratch_id.oid_lo);
    UINT64ENCODE_VARLEN(p, output->scratch_oh.cookie);

    /* encode the length of plist and the plist buffer (already encoded) */
    UINT64ENCODE_VARLEN(p, dcpl_size);
    if(0 != dcpl_size) {
        HDmemcpy(p, output->dcpl, dcpl_size);
        p += dcpl_size;
    }

    /* encode the length of the type and the type buffer (already encoded) */
    UINT64ENCODE_VARLEN(p, type_size);
    if(0 != type_size) {
        HDmemcpy(p, output->dtype, type_size);
        p += type_size;
    }

    /* encode the length of the spacs and the space buffer (already encoded) */
    UINT64ENCODE_VARLEN(p, space_size);
    if(0 != space_size) {
        HDmemcpy(p, output->dspace, space_size);
        p += space_size;
    }

done:
    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_iod_server_encode_dset_open() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_decode_dset_io_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_server_decode_dset_io(fs_proc_t proc, void *_input)
{
    H5VL_iod_dset_io_input_t *input = (H5VL_iod_dset_io_input_t *)_input;
    void *buf=NULL;
    uint8_t *p;
    size_t dxpl_size = 0;
    size_t space_size = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;

    /* decode the location with the container handle & iod object IDs and opened handles */
    UINT64DECODE_VARLEN(p, input->iod_oh.cookie);
    UINT64DECODE_VARLEN(p, input->scratch_oh.cookie);
    UINT32DECODE(p, input->checksum);

    /* decode the plist size */
    UINT64DECODE_VARLEN(p, dxpl_size);
    /* decode property lists if they are not default*/
    if(dxpl_size) {
        if((input->dxpl_id = H5Pdecode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += dxpl_size;
    }
    else {
        input->dxpl_id = H5P_DATASET_XFER_DEFAULT;
    }

    /* decode the space size & dataspace */
    UINT64DECODE_VARLEN(p, space_size);
    if((input->space_id = H5Sdecode((const void *)p)) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDECODE, FAIL, "can't decode object");
    p += space_size;

    if(S_FAIL == bds_handle_deserialize(&input->bds_handle, p, BDS_MAX_HANDLE_SIZE))
        HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to desrialize bds handle");
    p += BDS_MAX_HANDLE_SIZE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_decode_dset_io() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_encode_dset_read
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_server_encode_dset_read(fs_proc_t proc, void *_output)
{
    H5VL_iod_read_status_t *output = (H5VL_iod_read_status_t *)_output;
    void *buf = NULL;
    uint8_t *p;
    size_t size, nalloc;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    size = sizeof(uint32_t) + sizeof(int32_t);

    nalloc = fs_proc_get_size(proc);

    if(nalloc < size)
        fs_proc_set_size(proc, size);

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;
    INT32ENCODE(p, output->ret);
    UINT32ENCODE(p, output->cs);

done:
    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_iod_server_encode_dset_read() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_encode_dset_write
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_server_encode_dset_write(fs_proc_t proc, void *_output)
{
    int output = *((int *)_output);
    void *buf = NULL;
    uint8_t *p;
    size_t size, nalloc;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    size = sizeof(int);

    nalloc = fs_proc_get_size(proc);

    if(nalloc < size)
        fs_proc_set_size(proc, size);

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;
    INT32ENCODE(p, output);

done:
    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_iod_server_encode_dset_io() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_decode_dset_set_extent_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_server_decode_dset_set_extent(fs_proc_t proc, void *_input)
{
    H5VL_iod_dset_set_extent_input_t *input = (H5VL_iod_dset_set_extent_input_t *)_input;
    void *buf=NULL;
    uint8_t *p;
    int i;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;

    /* decode the location with the container handle & iod object IDs and opened handles */
    UINT64DECODE_VARLEN(p, input->iod_oh.cookie);

    /* decode the rank */
    INT32DECODE(p, input->rank);

    input->size = malloc (sizeof(hsize_t) * input->rank);
    for(i=0 ; i<input->rank ; i++)
        UINT64DECODE_VARLEN(p, input->size[i])

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_decode_dset_set_extent() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_encode_dset_set_extent_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_server_encode_dset_set_extent(fs_proc_t proc, void *_output)
{
    int output = *((int *)_output);
    void *buf = NULL;
    uint8_t *p;
    size_t size, nalloc;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    size = sizeof(int32_t);

    nalloc = fs_proc_get_size(proc);

    if(nalloc < size)
        fs_proc_set_size(proc, size);

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;

    INT32ENCODE(p, output);

done:
    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_iod_server_encode_dset_set_extent() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_decode_dset_close_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_server_decode_dset_close(fs_proc_t proc, void *_input)
{
    H5VL_iod_remote_dset_t *input = (H5VL_iod_remote_dset_t *)_input;
    void *buf=NULL;
    uint8_t *p;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;

    /* decode the location with the container handle & iod object IDs and opened handles */
    UINT64DECODE_VARLEN(p, input->iod_id.oid_hi);
    UINT64DECODE_VARLEN(p, input->iod_id.oid_lo);
    UINT64DECODE_VARLEN(p, input->iod_oh.cookie);
    UINT64DECODE_VARLEN(p, input->scratch_id.oid_hi);
    UINT64DECODE_VARLEN(p, input->scratch_id.oid_lo);
    UINT64DECODE_VARLEN(p, input->scratch_oh.cookie);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_iod_server_decode_dset_close() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_iod_server_encode_dset_close_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_server_encode_dset_close(fs_proc_t proc, void *_output)
{
    int output = *((int *)_output);
    void *buf = NULL;
    uint8_t *p;
    size_t size, nalloc;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    size = sizeof(int32_t);

    nalloc = fs_proc_get_size(proc);

    if(nalloc < size)
        fs_proc_set_size(proc, size);

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;

    INT32ENCODE(p, output);

done:
    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_iod_server_encode_dset_close() */
