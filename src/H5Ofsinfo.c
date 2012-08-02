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
 * Created:             H5Ofsinfo.c
 *                      Feb 2009
 *			Vailin Choi
 *
 * Purpose:             Free space manager info message.
 *
 *-------------------------------------------------------------------------
 */

#define H5O_PACKAGE		/* suppress error about including H5Opkg	*/

#include "H5private.h"		/* Generic Functions	*/
#include "H5Eprivate.h"		/* Error handling	*/
#include "H5FLprivate.h"	/* Free lists          	*/
#include "H5Opkg.h"             /* Object headers	*/

/* PRIVATE PROTOTYPES */
static void *H5O_fsinfo_decode(H5F_t *f, hid_t dxpl_id, H5O_t *open_oh, unsigned mesg_flags, unsigned *ioflags, const uint8_t *p);
static herr_t H5O_fsinfo_encode(H5F_t *f, hbool_t disable_shared, uint8_t *p, const void *_mesg);
static void *H5O_fsinfo_copy(const void *_mesg, void *_dest);
static size_t H5O_fsinfo_size(const H5F_t *f, hbool_t disable_shared, const void *_mesg);
static herr_t H5O_fsinfo_free(void *mesg);
static herr_t H5O_fsinfo_debug(H5F_t *f, hid_t dxpl_id, const void *_mesg,
    FILE * stream, int indent, int fwidth);

/* This message derives from H5O message class */
const H5O_msg_class_t H5O_MSG_FSINFO[1] = {{
    H5O_FSINFO_ID,            	/* message id number             	*/
    "fsinfo",                 	/* message name for debugging    	*/
    sizeof(H5O_fsinfo_t),     	/* native message size           	*/
    0,				/* messages are sharable?        	*/
    H5O_fsinfo_decode,        	/* decode message                	*/
    H5O_fsinfo_encode,        	/* encode message                	*/
    H5O_fsinfo_copy,          	/* copy the native value         	*/
    H5O_fsinfo_size,          	/* size of free-space manager info message */
    NULL,                   	/* default reset method         	*/
    H5O_fsinfo_free,	        /* free method				*/
    NULL,        		/* file delete method			*/
    NULL,			/* link method				*/
    NULL,			/* set share method			*/
    NULL,		    	/* can share method			*/
    NULL,			/* pre copy native value to file 	*/
    NULL,			/* copy native value to file    	*/
    NULL,			/* post copy native value to file	*/
    NULL,			/* get creation index			*/
    NULL,			/* set creation index			*/
    H5O_fsinfo_debug          	/* debug the message            	*/
}};

/* Current version of free-space manager info information */
#define H5O_FSINFO_VERSION 	0

/* Declare a free list to manage the H5O_fsinfo_t struct */
H5FL_DEFINE_STATIC(H5O_fsinfo_t);


/*-------------------------------------------------------------------------
 * Function:    H5O_fsinfo_decode
 *
 * Purpose:     Decode a message and return a pointer to a newly allocated one.
 *
 * Return:      Success:        Ptr to new message in native form.
 *              Failure:        NULL
 *
 * Programmer:  Vailin Choi; Feb 2009
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_fsinfo_decode(H5F_t *f, hid_t UNUSED dxpl_id, H5O_t UNUSED *open_oh,
    unsigned UNUSED mesg_flags, unsigned UNUSED *ioflags, const uint8_t *p)
{
    H5O_fsinfo_t	*fsinfo = NULL; /* free-space manager info */
    H5FD_mem_t 		type;		/* Memory type for iteration */
    void        	*ret_value;  	/* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* check args */
    HDassert(f);
    HDassert(p);

    /* Version of message */
    if(*p++ != H5O_FSINFO_VERSION)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "bad version number for message")

    /* Allocate space for message */
    if(NULL == (fsinfo = H5FL_CALLOC(H5O_fsinfo_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    fsinfo->strategy = (H5F_file_space_type_t)*p++;	/* file space strategy */
    H5F_DECODE_LENGTH(f, p, fsinfo->threshold);	/* free space section size threshold */

    /* Addresses of free space managers: only exist for H5F_FILE_SPACE_ALL_PERSIST */
    if(fsinfo->strategy == H5F_FILE_SPACE_ALL_PERSIST) {
	for(type = H5FD_MEM_SUPER; type < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, type))
	    H5F_addr_decode(f, &p, &(fsinfo->fs_addr[type-1]));
    } /* end if */
    else {
	for(type = H5FD_MEM_SUPER; type < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, type))
	    fsinfo->fs_addr[type-1] = HADDR_UNDEF;
    } /* end else */

    /* Set return value */
    ret_value = fsinfo;

done:
    if(ret_value == NULL && fsinfo != NULL)
        fsinfo = H5FL_FREE(H5O_fsinfo_t, fsinfo);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_fsinfo_decode() */


/*-------------------------------------------------------------------------
 * Function:    H5O_fsinfo_encode
 *
 * Purpose:     Encodes a message.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Vailin Choi; Feb 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_fsinfo_encode(H5F_t *f, hbool_t UNUSED disable_shared, uint8_t *p, const void *_mesg)
{
    const H5O_fsinfo_t  *fsinfo = (const H5O_fsinfo_t *)_mesg;
    H5FD_mem_t 		type;	/* Memory type for iteration */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* check args */
    HDassert(f);
    HDassert(p);
    HDassert(fsinfo);

    *p++ = H5O_FSINFO_VERSION;	/* message version */
    *p++ = fsinfo->strategy;	/* file space strategy */
    H5F_ENCODE_LENGTH(f, p, fsinfo->threshold); /* free-space section size threshold */

    /* Addresses of free space managers: only exist for H5F_FILE_SPACE_ALL_PERSIST */
    if(fsinfo->strategy == H5F_FILE_SPACE_ALL_PERSIST) {
	for(type = H5FD_MEM_SUPER; type < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, type))
	    H5F_addr_encode(f, &p, fsinfo->fs_addr[type-1]);
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_fsinfo_encode() */


/*-------------------------------------------------------------------------
 * Function:    H5O_fsinfo_copy
 *
 * Purpose:     Copies a message from _MESG to _DEST, allocating _DEST if
 *              necessary.
 *
 * Return:      Success:        Ptr to _DEST
 *              Failure:        NULL
 *
 * Programmer:  Vailin Choi; Feb 2009
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_fsinfo_copy(const void *_mesg, void *_dest)
{
    const H5O_fsinfo_t   *fsinfo = (const H5O_fsinfo_t *)_mesg;
    H5O_fsinfo_t         *dest = (H5O_fsinfo_t *) _dest;
    void                *ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* check args */
    HDassert(fsinfo);
    if(!dest && NULL == (dest = H5FL_CALLOC(H5O_fsinfo_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* copy */
    *dest = *fsinfo;

    /* Set return value */
    ret_value = dest;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_fsinfo_copy() */


/*-------------------------------------------------------------------------
 * Function:    H5O_fsinfo_size
 *
 * Purpose:     Returns the size of the raw message in bytes not counting
 *              the message type or size fields, but only the data fields.
 *              This function doesn't take into account alignment.
 *
 * Return:      Success:        Message data size in bytes without alignment.
 *              Failure:        zero
 *
 * Programmer:  Vailin Choi; Feb 2009
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5O_fsinfo_size(const H5F_t *f, hbool_t UNUSED disable_shared, const void *_mesg)
{
    const H5O_fsinfo_t   *fsinfo = (const H5O_fsinfo_t *)_mesg;
    size_t fs_addr_size = 0;
    size_t ret_value;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR


    /* Addresses of free-space managers exist only for H5F_FILE_SPACE_ALL_PERSIST type */
    if(H5F_FILE_SPACE_ALL_PERSIST == fsinfo->strategy)
	fs_addr_size = (H5FD_MEM_NTYPES - 1) * (size_t)H5F_SIZEOF_ADDR(f);

    ret_value = 2                       /* Version & strategy */
		+ (size_t)H5F_SIZEOF_SIZE(f)	/* Threshold */
                + fs_addr_size;		/* Addresses of free-space managers */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_fsinfo_size() */


/*-------------------------------------------------------------------------
 * Function:    H5O_fsinfo_free
 *
 * Purpose:     Free's the message
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Vailin Choi; Feb 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_fsinfo_free(void *mesg)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(mesg);

    mesg = H5FL_FREE(H5O_fsinfo_t, mesg);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_fsinfo_free() */


/*-------------------------------------------------------------------------
 * Function:    H5O_fsinfo_debug
 *
 * Purpose:     Prints debugging info for a message.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Vailin Choi; Feb 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_fsinfo_debug(H5F_t UNUSED *f, hid_t UNUSED dxpl_id, const void *_mesg, FILE * stream,
	       int indent, int fwidth)
{
    const H5O_fsinfo_t	*fsinfo = (const H5O_fsinfo_t *) _mesg;
    H5FD_mem_t 		type;	/* Memory type for iteration */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* check args */
    HDassert(f);
    HDassert(fsinfo);
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);

    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
              "File space strategy:", fsinfo->strategy);

    HDfprintf(stream, "%*s%-*s %Hu\n", indent, "", fwidth,
              "Free space section threshold:", fsinfo->threshold);

    if(fsinfo->strategy == H5F_FILE_SPACE_ALL_PERSIST) {
	for(type = H5FD_MEM_SUPER; type < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, type))
	    HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
		"Free space manager address:", fsinfo->fs_addr[type-1]);
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_fsinfo_debug() */

