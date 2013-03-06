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

#include "H5VLiod_common.h"

/*
 * Programmer:  Mohamad Chaarawi <chaarawi@hdfgroup.gov>
 *              February, 2012
 *
 * Purpose:	The IOD plugin server encode/decode code
 */

/*-------------------------------------------------------------------------
 * Function:	H5VL_server_encode_file_create_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL_server_encode_file_create(fs_proc_t proc, void *_output)
{
    H5VL_iod_file_create_output_t *output = (H5VL_iod_file_create_output_t *)_output;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* determine the size of the buffer needed to encode the output */
    if(H5VL__encode_file_create_ouput(NULL, &buf_size, input->name, input->flags, 
                                      input->fcpl_id, input->fapl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to determine buffer size needed");

    if(NULL == (buf = fs_proc_get_buf(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "buffer to encode in does not exist");

    nalloc = fs_proc_get_size(proc);

    if(nalloc < buf_size)
        fs_proc_set_size(nalloc);

    buf = fs_proc_get_buf(proc);

    /* encode the parameters */
    if(H5VL__encode_file_create_params(buf, &buf_size, input->name, input->flags,
                                       input->fcpl_id, input->fapl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NU, "unable to encode file create parameters");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5VL_server_encode_file_create() */

/*-------------------------------------------------------------------------
 * Function:	H5VL_server_decode_file_create_params
 *------------------------------------------------------------------------- */
herr_t 
H5VL_server_decode_file_create(fs_proc_t proc, void *_input)
{
    H5VL_iod_file_create_input_t input = *((H5VL_iod_file_create_input_t *)_input);
    void *buf = NULL;
    uint8_t *p;

    FUNC_ENTER_NOAPI_NOINIT

    if(NULL == (buf = fs_proc_get_buf(proc)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "encoded buffer does not exist");

    p = (uint8_t *)buf;
    /* decode the file creation parameters */
    if(H5VL__decode_file_create_params(p, &input.name, &input.flags, 
                                       &input.fcpl_id, &input.fapl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDECODE, FAIL, "can't decode file create params");

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5VL_server_decode_file_create() */
