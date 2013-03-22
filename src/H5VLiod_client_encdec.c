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

#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"		/* Property lists			*/
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
        if((ret_value = H5Pencode(fcpl_id,  NULL, &fcpl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }
    if(H5P_FILE_ACCESS_DEFAULT != fapl_id) {
        if((ret_value = H5Pencode(fapl_id, NULL, &fapl_size)) < 0)
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
        if((ret_value = H5Pencode(fcpl_id, p, &fcpl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
        p += fcpl_size;
    }

    /* encode the plist size */
    UINT64ENCODE_VARLEN(p, fapl_size);
    /* encode property lists if they are not default*/
    if(fapl_size) {
        if((ret_value = H5Pencode(fapl_id, p, &fapl_size)) < 0)
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
    hid_t fapl_id = input->fapl_id;
    unsigned flags = input->flags;
    const char *name = input->name;
    void *buf;
    uint8_t *p;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* get property list sizes */
    if(H5P_FILE_ACCESS_DEFAULT != fapl_id) {
        if((ret_value = H5Pencode(fapl_id, NULL, &fapl_size)) < 0)
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
        if((ret_value = H5Pencode(fapl_id, p, &fapl_size)) < 0)
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
        if((output->fcpl_id = H5Pdecode(p)) < 0)
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
 * Function:	H5VL_client_encode_file_flush
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_client_encode_file_flush(fs_proc_t proc, void *_input)
{
    H5VL_iod_file_flush_input_t *input = (H5VL_iod_file_flush_input_t *)_input;
    size_t size, nalloc;
    void *buf;
    uint8_t *p;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    size = 1 + 1 + H5V_limit_enc_size((uint64_t)input->coh.cookie);

    nalloc = fs_proc_get_size(proc);

    if(nalloc < size)
        fs_proc_set_size(proc, size);

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;

    /* encode scope */
    *p++ = (uint8_t)input->scope;
    UINT64ENCODE_VARLEN(p, input->coh.cookie);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_client_encode_file_flush() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_client_decode_file_flush
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_client_decode_file_flush(fs_proc_t proc, void *_output)
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
} /* end H5VL_client_decode_file_flush() */

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
        if((ret_value = H5Pencode(gcpl_id, NULL, &gcpl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }
    if(H5P_GROUP_ACCESS_DEFAULT != gapl_id) {
        if((ret_value = H5Pencode(gapl_id, NULL, &gapl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }
    if(H5P_LINK_CREATE_DEFAULT != lcpl_id) {
        if((ret_value = H5Pencode(lcpl_id, NULL, &lcpl_size)) < 0)
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
        if((ret_value = H5Pencode(gcpl_id, p, &gcpl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
        p += gcpl_size;
    }

    /* encode the plist size */
    UINT64ENCODE_VARLEN(p, gapl_size);
    /* encode property lists if they are not default*/
    if(gapl_size) {
        if((ret_value = H5Pencode(gapl_id, p, &gapl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
        p += gapl_size;
    }

    /* encode the plist size */
    UINT64ENCODE_VARLEN(p, lcpl_size);
    /* encode property lists if they are not default*/
    if(H5P_LINK_CREATE_DEFAULT != lcpl_id) {
        if((ret_value = H5Pencode(lcpl_id, p, &lcpl_size)) < 0)
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
    const char *name = input->name;
    hid_t gapl_id = input->gapl_id;
    void *buf;
    uint8_t *p;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* get size for property lists to encode */
    if(H5P_GROUP_ACCESS_DEFAULT != gapl_id) {
        if((ret_value = H5Pencode(gapl_id, NULL, &gapl_size)) < 0)
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
        if((ret_value = H5Pencode(gapl_id, p, &gapl_size)) < 0)
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
        if((output->gcpl_id = H5Pdecode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += gcpl_size;
    }
    else {
        output->gcpl_id = H5P_GROUP_CREATE_DEFAULT;
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
        if((ret_value = H5Pencode(dcpl_id, NULL, &dcpl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }
    if(H5P_DATASET_ACCESS_DEFAULT != dapl_id) {
        if((ret_value = H5Pencode(dapl_id, NULL, &dapl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }
    if(H5P_LINK_CREATE_DEFAULT != lcpl_id) {
        if((ret_value = H5Pencode(lcpl_id, NULL, &lcpl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }

    /* get Type size to encode */
    if(H5Tencode(type_id, NULL, &type_size) < 0)
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "can't encode datatype");

    /* get Dataspace size to encode */
    if(H5Sencode(space_id, NULL, &space_size)<0)
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
        if((ret_value = H5Pencode(dcpl_id, p, &dcpl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
        p += dcpl_size;
    }

    /* encode the plist size */
    UINT64ENCODE_VARLEN(p, dapl_size);
    /* encode property lists if they are not default*/
    if(dapl_size) {
        if((ret_value = H5Pencode(dapl_id, p, &dapl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
        p += dapl_size;
    }

    /* encode the plist size */
    UINT64ENCODE_VARLEN(p, lcpl_size);
    /* encode property lists if they are not default*/
    if(lcpl_size) {
        if((ret_value = H5Pencode(lcpl_id, p, &lcpl_size)) < 0)
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
    const char *name = input->name;
    hid_t dapl_id = input->dapl_id;
    void *buf;
    uint8_t *p;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* get size for property lists to encode */
    if(H5P_DATASET_ACCESS_DEFAULT != dapl_id) {
        if((ret_value = H5Pencode(dapl_id, NULL, &dapl_size)) < 0)
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
        if((ret_value = H5Pencode(dapl_id, p, &dapl_size)) < 0)
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
    size_t dcpl_size = 0, type_size = 0, space_size = 0;
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
        if((output->dcpl_id = H5Pdecode(p)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTDECODE, FAIL, "unable to decode property list");
        p += dcpl_size;
    }
    else {
        output->dcpl_id = H5P_FILE_CREATE_DEFAULT;
    }

    /* decode the type size */
    UINT64DECODE_VARLEN(p, type_size);
    /* decode the datatype */
    /* Create datatype by decoding buffer */
    if(FAIL == (output->type_id = H5Tdecode((const void *)p)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTDECODE, FAIL, "can't decode object");
    p += type_size;

    /* decode the space size */
    UINT64DECODE_VARLEN(p, space_size);
    /* decode the dataspace */
    if((output->space_id = H5Sdecode((const void *)p)) == FAIL)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDECODE, FAIL, "can't decode object");
    p += space_size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_client_decode_dset_open() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_client_encode_dset_io
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_client_encode_dset_io(fs_proc_t proc, void *_input)
{
    H5VL_iod_dset_io_input_t *input = (H5VL_iod_dset_io_input_t *)_input;
    size_t size, nalloc;
    void *buf;
    uint8_t *p;
    hid_t dxpl_id = input->dxpl_id;
    hid_t space_id = input->space_id;
    size_t space_size = 0, dxpl_size = 0;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* get size for property lists to encode */
    if(H5P_DATASET_XFER_DEFAULT != dxpl_id) {
        if((ret_value = H5Pencode(dxpl_id, NULL, &dxpl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
    }

    /* get Dataspace size to encode */
    if(H5Sencode(space_id, NULL, &space_size)<0)
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "can't encode datatype");

    size = BDS_MAX_HANDLE_SIZE + sizeof(uint32_t) + 
        1 + H5V_limit_enc_size((uint64_t)input->iod_oh.cookie) +
        1 + H5V_limit_enc_size((uint64_t)input->scratch_oh.cookie) + 
        1 + H5V_limit_enc_size((uint64_t)dxpl_size) + dxpl_size + 
        1 + H5V_limit_enc_size((uint64_t)space_size) + space_size;

    nalloc = fs_proc_get_size(proc);

    if(nalloc < size)
        fs_proc_set_size(proc, size);

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */

    /* encode the location with the container handle & iod object IDs and opened handles */
    UINT64ENCODE_VARLEN(p, input->iod_oh.cookie);
    UINT64ENCODE_VARLEN(p, input->scratch_oh.cookie);
    UINT32ENCODE(p, input->checksum);

    /* encode the plist size */
    UINT64ENCODE_VARLEN(p, dxpl_size);
    /* encode property lists if they are not default*/
    if(dxpl_size) {
        if((ret_value = H5Pencode(dxpl_id, p, &dxpl_size)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTENCODE, FAIL, "unable to encode property list");
        p += dxpl_size;
    }

    /* encode the dataspace size */
    UINT64ENCODE_VARLEN(p, space_size);
    /* encode datatspace */
    if(H5Sencode(space_id, p, &space_size) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "unable to encode datatspace");
    p += space_size;

    if(S_FAIL == bds_handle_serialize(p, BDS_MAX_HANDLE_SIZE, input->bds_handle))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "unable to serialize bds handle");

    p += BDS_MAX_HANDLE_SIZE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_client_encode_dset_io() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_client_decode_dset_read
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_client_decode_dset_read(fs_proc_t proc, void *_output)
{
    H5VL_iod_read_status_t *output = (H5VL_iod_read_status_t *)_output;
    void *buf=NULL;
    uint8_t *p;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to decode from does not exist");

    p = (uint8_t *)buf;
    INT32DECODE(p, output->ret);
    UINT32DECODE(p, output->cs);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_client_decode_dset_read() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_client_decode_dset_write
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_client_decode_dset_write(fs_proc_t proc, void *_output)
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
} /* end H5VL_client_decode_dset_write() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_client_encode_dset_set_extent
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_client_encode_dset_set_extent(fs_proc_t proc, void *_input)
{
    H5VL_iod_dset_set_extent_input_t *input = (H5VL_iod_dset_set_extent_input_t *)_input;
    size_t size, nalloc;
    void *buf;
    uint8_t *p;
    int i;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    size = 1 + H5V_limit_enc_size((uint64_t)input->iod_oh.cookie) + sizeof(int32_t);
    for(i=0 ; i<input->rank ; i++)
        size += 1 + H5V_limit_enc_size((uint64_t)(input->size[i]));

    nalloc = fs_proc_get_size(proc);

    if(nalloc < size)
        fs_proc_set_size(proc, size);

    if(NULL == (buf = fs_proc_get_buf_ptr(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    p = (uint8_t *)buf;    /* Temporary pointer to encoding buffer */

    /* encode the location with the container handle & iod object IDs and opened handles */
    UINT64ENCODE_VARLEN(p, input->iod_oh.cookie);
    /* encode the rank */
    INT32ENCODE(p, input->rank);
    for(i=0 ; i<input->rank ; i++)
        UINT64ENCODE_VARLEN(p, input->size[i])

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_client_encode_dset_set_extent() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_client_decode_dset_set_extent
 *------------------------------------------------------------------------- */
herr_t 
H5VL_iod_client_decode_dset_set_extent(fs_proc_t proc, void *_output)
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
} /* end H5VL_client_decode_dset_set_extent() */

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
