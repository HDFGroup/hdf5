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

/*
 * Programmer:	Quincey Koziol <koziol@hdfgroup.org>
 *		Friday, January 19, 2007
 *
 * Purpose:	This file contains inline definitions for "generic" routines
 *		supporting a "shared message interface" (ala Java) for object
 *		header messages that can be shared.  This interface is
 *              dependent on a bunch of macros being defined which define
 *              the name of the interface and "real" methods which need to
 *              be implemented for each message class that supports the
 *              shared message interface.
 */

#ifndef H5Oshared_H
#define H5Oshared_H


/*-------------------------------------------------------------------------
 * Function:    H5O_SHARED_DECODE
 *
 * Purpose:     Decode an object header message that may be shared.
 *
 * Note:	The actual name of this routine can be different in each source
 *		file that this header file is included in, and must be defined
 *		prior to including this header file.
 *
 * Return:      Success:        Pointer to the new message in native form
 *              Failure:        NULL
 *
 * Programmer:  Quincey Koziol
 *              Friday, January 19, 2007
 *
 *-------------------------------------------------------------------------
 */
static H5_inline void *
H5O_SHARED_DECODE(H5F_t *f, hid_t dxpl_id, unsigned mesg_flags, const uint8_t *p)
{
    void *ret_value;            /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_SHARED_DECODE)

#ifndef H5O_SHARED_TYPE
#error "Need to define H5O_SHARED_TYPE macro!"
#endif /* H5O_SHARED_TYPE */
#ifndef H5O_SHARED_DECODE
#error "Need to define H5O_SHARED_DECODE macro!"
#endif /* H5O_SHARED_DECODE */
#ifndef H5O_SHARED_DECODE_REAL
#error "Need to define H5O_SHARED_DECODE_REAL macro!"
#endif /* H5O_SHARED_DECODE_REAL */

    /* Check for shared message */
    if(mesg_flags & H5O_MSG_FLAG_SHARED) {
        H5O_shared_t sh_mesg;           /* Shared message info */

        /* Retrieve shared message info by decoding info in buffer */
        if(H5O_shared_decode_new(f, p, &sh_mesg) < 0)
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTDECODE, NULL, "unable to decode shared message")

        /* Retrieve actual native message by reading it through shared info */
        if(NULL == (ret_value = H5O_shared_read(f, dxpl_id, &sh_mesg, H5O_SHARED_TYPE, NULL)))
	    HGOTO_ERROR(H5E_OHDR, H5E_READERROR, NULL, "unable to retrieve native message")
    } /* end if */
    else {
        /* Decode native message directly */
        if(NULL == (ret_value = H5O_SHARED_DECODE_REAL(f, dxpl_id, p)))
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTDECODE, NULL, "unable to decode native message")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_SHARED_DECODE() */


/*-------------------------------------------------------------------------
 * Function:    H5O_SHARED_ENCODE
 *
 * Purpose:     Encode an object header message that may be shared.
 *
 * Note:	The actual name of this routine can be different in each source
 *		file that this header file is included in, and must be defined
 *		prior to including this header file.
 *
 * Return:      Success:        Non-negative
 *              Failure:        Negative
 *
 * Programmer:  Quincey Koziol
 *              Friday, January 19, 2007
 *
 *-------------------------------------------------------------------------
 */
static H5_inline herr_t
H5O_SHARED_ENCODE(H5F_t *f, uint8_t *p, const void *_mesg)
{
    const H5O_shared_t *sh_mesg = (const H5O_shared_t *)_mesg;     /* Pointer to shared message portion of actual message */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_SHARED_ENCODE)

#ifndef H5O_SHARED_TYPE
#error "Need to define H5O_SHARED_TYPE macro!"
#endif /* H5O_SHARED_TYPE */
#ifndef H5O_SHARED_ENCODE
#error "Need to define H5O_SHARED_ENCODE macro!"
#endif /* H5O_SHARED_ENCODE */
#ifndef H5O_SHARED_ENCODE_REAL
#error "Need to define H5O_SHARED_ENCODE_REAL macro!"
#endif /* H5O_SHARED_ENCODE_REAL */

    /* Check for shared message */
    if(H5O_IS_SHARED(sh_mesg->flags)) {
        /* Encode shared message into buffer */
        if(H5O_shared_encode_new(f, p, sh_mesg) < 0)
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTENCODE, FAIL, "unable to encode shared message")
    } /* end if */
    else {
        /* Encode native message directly */
        if(H5O_SHARED_ENCODE_REAL(f, p, _mesg) < 0)
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTENCODE, FAIL, "unable to encode native message")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_SHARED_ENCODE() */


/*-------------------------------------------------------------------------
 * Function:    H5O_SHARED_SIZE
 *
 * Purpose:	Returns the length of an encoded message.
 *
 * Note:	The actual name of this routine can be different in each source
 *		file that this header file is included in, and must be defined
 *		prior to including this header file.
 *
 * Return:	Success:	Length
 *		Failure:	0
 *
 * Programmer:  Quincey Koziol
 *              Friday, January 19, 2007
 *
 *-------------------------------------------------------------------------
 */
static H5_inline size_t
H5O_SHARED_SIZE(const H5F_t *f, const void *_mesg)
{
    const H5O_shared_t *sh_mesg = (const H5O_shared_t *)_mesg;     /* Pointer to shared message portion of actual message */
    size_t ret_value;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_SHARED_SIZE)

#ifndef H5O_SHARED_TYPE
#error "Need to define H5O_SHARED_TYPE macro!"
#endif /* H5O_SHARED_TYPE */
#ifndef H5O_SHARED_SIZE
#error "Need to define H5O_SHARED_SIZE macro!"
#endif /* H5O_SHARED_SIZE */
#ifndef H5O_SHARED_SIZE_REAL
#error "Need to define H5O_SHARED_SIZE_REAL macro!"
#endif /* H5O_SHARED_SIZE_REAL */

    /* Check for shared message */
    if(H5O_IS_SHARED(sh_mesg->flags)) {
        /* Retrieve encoded size of shared message */
        if(0 == (ret_value = H5O_shared_size_new(f, sh_mesg)))
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, 0, "unable to retrieve encoded size of shared message")
    } /* end if */
    else {
        /* Retrieve size of native message directly */
        if(0 == (ret_value = H5O_SHARED_SIZE_REAL(f, _mesg)))
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, 0, "unable to retrieve encoded size of native message")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_SHARED_SIZE() */


/*-------------------------------------------------------------------------
 * Function:    H5O_SHARED_DELETE
 *
 * Purpose:     Decrement reference count on any objects referenced by
 *              message
 *
 * Note:	The actual name of this routine can be different in each source
 *		file that this header file is included in, and must be defined
 *		prior to including this header file.
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:  Quincey Koziol
 *              Friday, January 19, 2007
 *
 *-------------------------------------------------------------------------
 */
static H5_inline herr_t
H5O_SHARED_DELETE(H5F_t *f, hid_t dxpl_id, const void *_mesg, hbool_t adj_link)
{
    const H5O_shared_t *sh_mesg = (const H5O_shared_t *)_mesg;     /* Pointer to shared message portion of actual message */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_SHARED_DELETE)

#ifndef H5O_SHARED_TYPE
#error "Need to define H5O_SHARED_TYPE macro!"
#endif /* H5O_SHARED_TYPE */
#ifndef H5O_SHARED_DELETE
#error "Need to define H5O_SHARED_DELETE macro!"
#endif /* H5O_SHARED_DELETE */
#ifndef H5O_SHARED_DELETE_REAL
#error "Need to define H5O_SHARED_DELETE_REAL macro!"
#endif /* H5O_SHARED_DELETE_REAL */

    /* Check for shared message */
    if(H5O_IS_SHARED(sh_mesg->flags)) {
        /* Decrement the reference count on the shared message/object */
        if(H5O_shared_delete_new(f, dxpl_id, sh_mesg, adj_link) < 0)
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTDEC, FAIL, "unable to decrement ref count for shared message")
    } /* end if */
    else {
        /* Decrement the reference count on the native message directly */
        if(H5O_SHARED_DELETE_REAL(f, dxpl_id, _mesg, adj_link) < 0)
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTDEC, FAIL, "unable to decrement ref count for native message")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_SHARED_DELETE() */


/*-------------------------------------------------------------------------
 * Function:    H5O_SHARED_LINK
 *
 * Purpose:     Increment reference count on any objects referenced by
 *              message
 *
 * Note:	The actual name of this routine can be different in each source
 *		file that this header file is included in, and must be defined
 *		prior to including this header file.
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:  Quincey Koziol
 *              Friday, January 19, 2007
 *
 *-------------------------------------------------------------------------
 */
static H5_inline herr_t
H5O_SHARED_LINK(H5F_t *f, hid_t dxpl_id, const void *_mesg)
{
    const H5O_shared_t *sh_mesg = (const H5O_shared_t *)_mesg;     /* Pointer to shared message portion of actual message */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_SHARED_LINK)

#ifndef H5O_SHARED_TYPE
#error "Need to define H5O_SHARED_TYPE macro!"
#endif /* H5O_SHARED_TYPE */
#ifndef H5O_SHARED_LINK
#error "Need to define H5O_SHARED_LINK macro!"
#endif /* H5O_SHARED_LINK */
#ifndef H5O_SHARED_LINK_REAL
#error "Need to define H5O_SHARED_LINK_REAL macro!"
#endif /* H5O_SHARED_LINK_REAL */

    /* Check for shared message */
    if(H5O_IS_SHARED(sh_mesg->flags)) {
        /* Increment the reference count on the shared message/object */
        if(H5O_shared_link_new(f, dxpl_id, sh_mesg) < 0)
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTINC, FAIL, "unable to increment ref count for shared message")
    } /* end if */
    else {
        /* Increment the reference count on the native message directly */
        if(H5O_SHARED_LINK_REAL(f, dxpl_id, _mesg) < 0)
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTINC, FAIL, "unable to increment ref count for native message")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_SHARED_LINK() */


/*-------------------------------------------------------------------------
 * Function:    H5O_SHARED_COPY_FILE
 *
 * Purpose:     Copies a message from _SRC to _DEST in file
 *
 * Note:	The actual name of this routine can be different in each source
 *		file that this header file is included in, and must be defined
 *		prior to including this header file.
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:  Quincey Koziol
 *              Friday, January 19, 2007
 *
 *-------------------------------------------------------------------------
 */
static H5_inline void *
H5O_SHARED_COPY_FILE(H5F_t *file_src, const H5O_msg_class_t *mesg_type,
    void *_native_src, H5F_t *file_dst, hid_t dxpl_id, H5O_copy_t *cpy_info,
    void *udata)
{
    const H5O_shared_t *sh_mesg = (const H5O_shared_t *)_native_src;     /* Pointer to shared message portion of actual message */
    void *ret_value;            /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_SHARED_COPY_FILE)

#ifndef H5O_SHARED_TYPE
#error "Need to define H5O_SHARED_TYPE macro!"
#endif /* H5O_SHARED_TYPE */
#ifndef H5O_SHARED_COPY_FILE
#error "Need to define H5O_SHARED_COPY_FILE macro!"
#endif /* H5O_SHARED_COPY_FILE */
#ifndef H5O_SHARED_COPY_FILE_REAL
#error "Need to define H5O_SHARED_COPY_FILE_REAL macro!"
#endif /* H5O_SHARED_COPY_FILE_REAL */

    /* Check for shared message */
    if(H5O_IS_SHARED(sh_mesg->flags)) {
        /* Copy the shared message to another file */
        if(NULL == (ret_value = H5O_shared_copy_file_new(file_src, mesg_type, _native_src, file_dst, dxpl_id, cpy_info, udata)))
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, NULL, "unable to copy shared message to another file")
    } /* end if */
    else {
        /* Decrement the reference count on the native message directly */
        /* Copy the native message directly to another file */
        if(NULL == (ret_value = H5O_SHARED_COPY_FILE_REAL(file_src, mesg_type, _native_src, file_dst, dxpl_id, cpy_info, udata)))
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, NULL, "unable to copy native message to another file")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_SHARED_COPY_FILE() */

#endif /* H5Oshared_H */

