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

#define H5A_PACKAGE             /*prevent warning from including H5Apkg   */
#define H5O_PACKAGE		/*suppress error about including H5Opkg	  */
#define H5S_PACKAGE	        /*suppress error about including H5Spkg	  */


#include "H5private.h"		/* Generic Functions			*/
#include "H5Apkg.h"		/* Attributes				*/
#include "H5Dprivate.h"		/* Datasets				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Opkg.h"             /* Object headers			*/
#include "H5Spkg.h"		/* Dataspaces				*/
#include "H5SMprivate.h"	/* Shared Object Header Messages	*/

/* PRIVATE PROTOTYPES */
static herr_t H5O_attr_encode(H5F_t *f, uint8_t *p, const void *mesg);
static void *H5O_attr_decode(H5F_t *f, hid_t dxpl_id, const uint8_t *p);
static void *H5O_attr_copy(const void *_mesg, void *_dest);
static size_t H5O_attr_size(const H5F_t *f, const void *_mesg);
static herr_t H5O_attr_free(void *mesg);
static herr_t H5O_attr_link(H5F_t *f, hid_t dxpl_id, const void *_mesg);
static herr_t H5O_attr_pre_copy_file(H5F_t *file_src, const H5O_msg_class_t *type,
    const void *mesg_src, hbool_t *deleted, const H5O_copy_t *cpy_info, void *udata);
static void *H5O_attr_copy_file(H5F_t *file_src, const H5O_msg_class_t *mesg_type,
    void *native_src, H5F_t *file_dst, hid_t dxpl_id, H5O_copy_t *cpy_info,
    void *udata);
static herr_t H5O_attr_set_share(void *_mesg, const H5O_shared_t *sh);
static htri_t H5O_attr_is_shared(const void *_mesg);
static herr_t H5O_attr_debug(H5F_t *f, hid_t dxpl_id, const void *_mesg,
			      FILE * stream, int indent, int fwidth);

/* This message derives from H5O message class */
const H5O_msg_class_t H5O_MSG_ATTR[1] = {{
    H5O_ATTR_ID,		/* message id number            */
    "attribute",		/* message name for debugging   */
    sizeof(H5A_t),		/* native message size          */
    H5O_attr_decode,		/* decode message               */
    H5O_attr_encode,		/* encode message               */
    H5O_attr_copy,		/* copy the native value        */
    H5O_attr_size,		/* size of raw message          */
    H5O_attr_reset,		/* reset method                 */
    H5O_attr_free,	        /* free method			*/
    H5O_attr_delete,		/* file delete method		*/
    H5O_attr_link,		/* link method			*/
    H5O_attr_get_share,		/* get share method		*/
    H5O_attr_set_share,		/* set share method		*/
    H5O_attr_is_shared,		/*is shared method		*/
    H5O_attr_pre_copy_file,	/* pre copy native value to file */
    H5O_attr_copy_file,		/* copy native value to file    */
    NULL,			/* post copy native value to file    */
    H5O_attr_debug		/* debug the message            */
}};

/* This is the initial version, which does not have support for shared datatypes */
#define H5O_ATTR_VERSION_1	1

/* This version allows support for shared datatypes */
#define H5O_ATTR_VERSION_2	2

/* Add support for different character encodings of attribute names */
#define H5O_ATTR_VERSION_3      3

/* Add support for shared dataspaces */
#define H5O_ATTR_VERSION_4      4

/* The latest version of the format.  Look through the 'encode' 
 *      and 'size' callback for places to change when updating this. */
#define H5O_ATTR_VERSION_LATEST H5O_ATTR_VERSION_4

/* Flags for attribute flag encoding */
#define H5O_ATTR_FLAG_TYPE_SHARED       0x01
#define H5O_ATTR_FLAG_SPACE_SHARED      0x02

/* Declare external the free list for H5S_t's */
H5FL_EXTERN(H5S_t);

/* Declare external the free list for H5S_extent_t's */
H5FL_EXTERN(H5S_extent_t);


/*--------------------------------------------------------------------------
 NAME
    H5O_attr_decode
 PURPOSE
    Decode a attribute message and return a pointer to a memory struct
        with the decoded information
 USAGE
    void *H5O_attr_decode(f, raw_size, p)
        H5F_t *f;               IN: pointer to the HDF5 file struct
        size_t raw_size;        IN: size of the raw information buffer
        const uint8_t *p;         IN: the raw information buffer
 RETURNS
    Pointer to the new message in native order on success, NULL on failure
 DESCRIPTION
        This function decodes the "raw" disk form of a attribute message
    into a struct in memory native format.  The struct is allocated within this
    function using malloc() and is returned to the caller.
 *
 * Modifications:
 * 	Robb Matzke, 17 Jul 1998
 *	Added padding for alignment.
 *
 * 	Robb Matzke, 20 Jul 1998
 *	Added a version number at the beginning.
 *
 *	Raymond Lu, 8 April 2004
 *	Changed Dataspace operation on H5S_simple_t to H5S_extent_t.
 *
 *      James Laird, 15 November 2005
 *      Added character encoding (version 3)
 *
--------------------------------------------------------------------------*/
static void *
H5O_attr_decode(H5F_t *f, hid_t dxpl_id, const uint8_t *p)
{
    H5A_t		*attr = NULL;
    H5S_extent_t	*extent;	/*extent dimensionality information  */
    size_t		name_len;   	/*attribute name length */
    int		        version;	/*message version number*/
    unsigned            flags = 0;      /* Attribute flags */
    H5A_t		*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_attr_decode)

    /* check args */
    HDassert(f);
    HDassert(p);

    if(NULL == (attr = H5FL_CALLOC(H5A_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Version number */
    version = *p++;
    if(version < H5O_ATTR_VERSION_1 || version > H5O_ATTR_VERSION_4)
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "bad version number for attribute message")

    /* Get the flags byte if we have a later version of the attribute */
    if(version > H5O_ATTR_VERSION_1)
        flags = *p++;
    else
        p++;    /* Byte is unused when version<2 */

    /*
     * Decode the sizes of the parts of the attribute.  The sizes stored in
     * the file are exact but the parts are aligned on 8-byte boundaries.
     */
    UINT16DECODE(p, name_len); /*including null*/
    UINT16DECODE(p, attr->dt_size);
    UINT16DECODE(p, attr->ds_size);

    /*
     * Decode the character encoding for the name for versions 3 or later,
     * as well as some reserved bytes.
     */
    if(version >= H5O_ATTR_VERSION_3)
        attr->encoding = *p++;

    /* Decode and store the name */
    if(NULL == (attr->name = H5MM_strdup((const char *)p)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    if(version < H5O_ATTR_VERSION_2)
        p += H5O_ALIGN_OLD(name_len);    /* advance the memory pointer */
    else
        p += name_len;    /* advance the memory pointer */

    /* decode the attribute datatype */
    if(flags & H5O_ATTR_FLAG_TYPE_SHARED) {
	H5O_shared_t *shared;   /* Shared information */

        /* Get the shared information */
	if (NULL == (shared = (H5O_MSG_SHARED->decode)(f, dxpl_id, p)))
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTDECODE, NULL, "unable to decode shared message")

        /* Get the actual datatype information */
        if((attr->dt = H5O_shared_read(f, dxpl_id, shared, H5O_MSG_DTYPE, NULL)) == NULL)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTDECODE, NULL, "can't decode attribute datatype")

        /* Free the shared information */
        H5O_msg_free_real(H5O_MSG_SHARED, shared);
    } /* end if */
    else {
        if((attr->dt = (H5O_MSG_DTYPE->decode)(f, dxpl_id, p)) == NULL)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTDECODE, NULL, "can't decode attribute datatype")
    } /* end else */
    if(version < H5O_ATTR_VERSION_2)
        p += H5O_ALIGN_OLD(attr->dt_size);
    else
        p += attr->dt_size;

    /* decode the attribute dataspace.  It can be shared in versions >= 4
     * What's actually shared, though, is only the extent.
     */
    if(NULL == (attr->ds = H5FL_CALLOC(H5S_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    if (flags & H5O_ATTR_FLAG_SPACE_SHARED) {
	H5O_shared_t *shared;   /* Shared information */

        /* Get the shared information */
	if (NULL == (shared = (H5O_MSG_SHARED->decode) (f, dxpl_id, p)))
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTDECODE, NULL, "unable to decode shared message")

        /* Get the actual datatype information */
        if((extent= H5O_shared_read(f, dxpl_id, shared, H5O_MSG_SDSPACE, NULL))==NULL)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTDECODE, NULL, "can't decode attribute dataspace")

        /* Free the shared information */
        H5O_msg_free_real(H5O_MSG_SHARED, shared);
    } /* end if */
    else {
        if((extent = (H5O_MSG_SDSPACE->decode)(f, dxpl_id, p)) == NULL)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTDECODE, NULL, "can't decode attribute dataspace")
    } /* end else */

    /* Copy the extent information to the dataspace */
    HDmemcpy(&(attr->ds->extent), extent, sizeof(H5S_extent_t));

    /* Release temporary extent information */
    H5FL_FREE(H5S_extent_t, extent);

    /* Default to entire dataspace being selected */
    if(H5S_select_all(attr->ds, 0) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSET, NULL, "unable to set all selection")

    if(version < H5O_ATTR_VERSION_2)
        p += H5O_ALIGN_OLD(attr->ds_size);
    else
        p += attr->ds_size;

    /* Compute the size of the data */
    H5_ASSIGN_OVERFLOW(attr->data_size, H5S_GET_EXTENT_NPOINTS(attr->ds) * H5T_get_size(attr->dt), hsize_t, size_t);

    /* Go get the data */
    if(attr->data_size) {
        if(NULL == (attr->data = H5FL_BLK_MALLOC(attr_buf, attr->data_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
        HDmemcpy(attr->data, p, attr->data_size);
    } /* end if */

    /* Indicate that the fill values aren't to be written out */
    attr->initialized = 1;

    /* Set return value */
    ret_value = attr;

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5O_attr_decode() */


/*--------------------------------------------------------------------------
 NAME
    H5O_attr_encode
 PURPOSE
    Encode a simple attribute message
 USAGE
    herr_t H5O_attr_encode(f, raw_size, p, mesg)
        H5F_t *f;         IN: pointer to the HDF5 file struct
        const uint8 *p;         IN: the raw information buffer
        const void *mesg;       IN: Pointer to the simple datatype struct
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
        This function encodes the native memory form of the attribute
    message in the "raw" disk form.
 *
 * Modifications:
 * 	Robb Matzke, 17 Jul 1998
 *	Added padding for alignment.
 *
 * 	Robb Matzke, 20 Jul 1998
 *	Added a version number at the beginning.
 *
 *	Raymond Lu, 8 April 2004
 *	For data space, changed the operation on H5S_simple_t to
 *	H5S_extent_t
 *
 *      James Laird, 15 November 2005
 *      Added character encoding (version 3)
 *
--------------------------------------------------------------------------*/
static herr_t
H5O_attr_encode(H5F_t *f, uint8_t *p, const void *mesg)
{
    const H5A_t *attr = (const H5A_t *) mesg;
    size_t      name_len;   /* Attribute name length */
    htri_t      is_shared_ret;  /* Return value from H5O_msg_is_shared */
    unsigned    version;        /* Attribute version */
    hbool_t     type_shared;    /* Flag to indicate that a shared datatype is used for this attribute */
    hbool_t     space_shared;   /* Flag to indicate that a shared dataspace is used for this attribute */
    H5O_shared_t sh_mesg;       /* Shared message location */
    hbool_t     use_latest_format;      /* Flag indicating the newest file format should be used */
    unsigned     flags=0;        /* Attribute flags */
    herr_t      ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_attr_encode)

    /* check args */
    HDassert(f);
    HDassert(p);
    HDassert(attr);

    /* Get the file's 'use the latest version of the format' flag */
    use_latest_format = H5F_USE_LATEST_FORMAT(f);

    /* Check whether datatype and dataspace are shared */
    if((is_shared_ret = H5O_msg_is_shared(H5O_DTYPE_ID, attr->dt)) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_BADMESG, FAIL, "can't determine if datatype is shared")
    else if(is_shared_ret)
        type_shared = TRUE;
    else
        type_shared = FALSE;

    if((is_shared_ret = H5O_msg_is_shared(H5O_SDSPACE_ID, attr->ds)) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_BADMESG, FAIL, "can't determine if dataspace is shared")
    else if(is_shared_ret)
        space_shared = TRUE;
    else
        space_shared = FALSE;

    /* Check which version to write out */
    if(use_latest_format)
        version = H5O_ATTR_VERSION_LATEST;      /* Write out latest version of format */
    else if(space_shared)
      version = H5O_ATTR_VERSION_4;   /* Write version with shared dataspaces */
    else if(attr->encoding != H5T_CSET_ASCII)
        version = H5O_ATTR_VERSION_3;   /* Write version which includes the character encoding */
    else if(type_shared)
        version = H5O_ATTR_VERSION_2;   /* Write out version with shared datatype */
    else
        version = H5O_ATTR_VERSION_1;   /* Write out basic version */

    /* Encode Version */
    *p++ = version;

    /* Set attribute flags if version >1 */
    if(version > H5O_ATTR_VERSION_1)
    {
        flags = (type_shared ? H5O_ATTR_FLAG_TYPE_SHARED : 0 );
        flags |= (space_shared ? H5O_ATTR_FLAG_SPACE_SHARED : 0);
        *p++ = flags;    /* Set flags for attribute */
    }
    else
        *p++ = 0; /* Reserved, for version <2 */

    /*
     * Encode the lengths of the various parts of the attribute message. The
     * encoded lengths are exact but we pad each part except the data to be a
     * multiple of eight bytes (in the first version).
     */
    name_len = HDstrlen(attr->name) + 1;
    UINT16ENCODE(p, name_len);
    UINT16ENCODE(p, attr->dt_size);
    UINT16ENCODE(p, attr->ds_size);

    /*
     * Encode the character encoding used for the attribute's name
     * Also add several "reserved" fields to pad to 16 bytes.
     */
    /* JAMES: only do this if flag says to? */
    if(version >= H5O_ATTR_VERSION_3)
        *p++ = attr->encoding;

    /* Write the name including null terminator */
    HDmemcpy(p, attr->name, name_len);
    if(version < H5O_ATTR_VERSION_2) {
        /* Pad to the correct number of bytes */
        HDmemset(p + name_len, 0, H5O_ALIGN_OLD(name_len) - name_len);
        p += H5O_ALIGN_OLD(name_len);
    } /* end if */
    else
        p += name_len;

    /* encode the attribute datatype */
    if(type_shared) {
        /* Reset shared message information */
        HDmemset(&sh_mesg, 0, sizeof(H5O_shared_t));

        /* Get shared message information from datatype */
        if(NULL == (H5O_MSG_DTYPE->get_share)(attr->dt, &sh_mesg/*out*/))
            HGOTO_ERROR(H5E_ATTR, H5E_CANTENCODE, FAIL, "can't encode shared attribute datatype")

        /* Encode shared message information for datatype */
        if((H5O_MSG_SHARED->encode)(f, p, &sh_mesg) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTENCODE, FAIL, "can't encode shared attribute datatype")
    } /* end if */
    else {
        /* Encode datatype information */
        if((H5O_MSG_DTYPE->encode)(f, p, attr->dt) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTENCODE, FAIL, "can't encode attribute datatype")
    } /* end else */

    if(version < H5O_ATTR_VERSION_2) {
        HDmemset(p + attr->dt_size, 0, H5O_ALIGN_OLD(attr->dt_size) - attr->dt_size);
        p += H5O_ALIGN_OLD(attr->dt_size);
    } /* end if */
    else
        p += attr->dt_size;

    /* encode the attribute dataspace */
    if(space_shared) {
        /* Reset shared message information */
        HDmemset(&sh_mesg, 0, sizeof(H5O_shared_t));

        /* Get shared message information from dataspace */
        if(NULL == (H5O_MSG_SDSPACE->get_share)(attr->ds, &sh_mesg/*out*/))
            HGOTO_ERROR(H5E_ATTR, H5E_CANTENCODE, FAIL, "can't encode shared attribute dataspace")

        /* Encode shared message information for dataspace */
        if((H5O_MSG_SHARED->encode)(f, p, &sh_mesg) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTENCODE, FAIL, "can't encode shared attribute dataspace")
    } /* end if */
    else {
        /* Encode non-shared dataspace information */
        if((H5O_MSG_SDSPACE->encode)(f, p, &(attr->ds->extent)) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTENCODE, FAIL, "can't encode attribute dataspace")
    } /* end else */

    if(version < H5O_ATTR_VERSION_2) {
        HDmemset(p + attr->ds_size, 0, H5O_ALIGN_OLD(attr->ds_size) - attr->ds_size);
        p += H5O_ALIGN_OLD(attr->ds_size);
    } /* end if */
    else
        p += attr->ds_size;

    /* Store attribute data */
    if(attr->data)
        HDmemcpy(p, attr->data, attr->data_size);
    else
        HDmemset(p, 0, attr->data_size);

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5O_attr_encode() */


/*--------------------------------------------------------------------------
 NAME
    H5O_attr_copy
 PURPOSE
    Copies a message from MESG to DEST, allocating DEST if necessary.
 USAGE
    void *H5O_attr_copy(mesg, dest)
        const void *mesg;       IN: Pointer to the source attribute struct
        const void *dest;       IN: Pointer to the destination attribute struct
 RETURNS
    Pointer to DEST on success, NULL on failure
 DESCRIPTION
        This function copies a native (memory) attribute message,
    allocating the destination structure if necessary.
--------------------------------------------------------------------------*/
static void *
H5O_attr_copy(const void *_src, void *_dst)
{
    void *ret_value;            /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_attr_copy)

    /* check args */
    HDassert(_src);

    /* copy */
    if(NULL == (ret_value = H5A_copy(_dst, _src)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, NULL, "can't copy attribute")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_attr_copy() */


/*--------------------------------------------------------------------------
 NAME
    H5O_attr_size
 PURPOSE
    Return the raw message size in bytes
 USAGE
    size_t H5O_attr_size(f, mesg)
        H5F_t *f;         IN: pointer to the HDF5 file struct
        const void *mesg;     IN: Pointer to the source attribute struct
 RETURNS
    Size of message on success, 0 on failure
 DESCRIPTION
        This function returns the size of the raw attribute message on
    success.  (Not counting the message type or size fields, only the data
    portion of the message).  It doesn't take into account alignment.
 *
 * Modified:
 * 	Robb Matzke, 17 Jul 1998
 *	Added padding between message parts for alignment.
--------------------------------------------------------------------------*/
static size_t
H5O_attr_size(const H5F_t *f, const void *_mesg)
{
    const H5A_t         *attr = (const H5A_t *)_mesg;
    size_t		name_len;
    unsigned            version;         /* Attribute version */
    hbool_t             type_shared, space_shared;    /* Flags to indicate that shared messages are used for this attribute */
    hbool_t             use_latest_format;      /* Flag indicating the newest file format should be used */
    size_t		ret_value = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_attr_size)

    HDassert(attr);

    name_len = HDstrlen(attr->name) + 1;

    /* Get the file's 'use the latest version of the format' flag */
    use_latest_format = H5F_USE_LATEST_FORMAT(f);

    /* Check whether datatype and dataspace are shared */
    if(H5O_msg_is_shared(H5O_DTYPE_ID, attr->dt) > 0)
        type_shared = TRUE;
    else
        type_shared = FALSE;

    if(H5O_msg_is_shared(H5O_SDSPACE_ID, attr->ds) > 0)
        space_shared = TRUE;
    else
        space_shared = FALSE;

    /* Check which version to write out */
    if(use_latest_format)
        version = H5O_ATTR_VERSION_LATEST;      /* Write out latest version of format */
    else if(space_shared)
      version = H5O_ATTR_VERSION_4;   /* Write version with shared dataspaces */
    else if(attr->encoding != H5T_CSET_ASCII)
        version = H5O_ATTR_VERSION_3;   /* Write version which includes the character encoding */
    else if(type_shared)
        version = H5O_ATTR_VERSION_2;   /* Write out version with shared datatype */
    else
        version = H5O_ATTR_VERSION_1;   /* Write out basic version */

    /* Common size information */
    ret_value = 1 +				/*version               */
                1 +				/*reserved		*/
                2 +				/*name size inc. null	*/
                2 +				/*type size		*/
                2; 				/*space size		*/

    /* Version-specific size information */
    if(version == H5O_ATTR_VERSION_1)
        ret_value += H5O_ALIGN_OLD(name_len) +	/*attribute name	*/
                    H5O_ALIGN_OLD(attr->dt_size) +	/*data type		*/
                    H5O_ALIGN_OLD(attr->ds_size) +	/*data space		*/
                    attr->data_size;		/*the data itself	*/
    else if(version == H5O_ATTR_VERSION_2)
        ret_value += name_len	+		/*attribute name	*/
                    attr->dt_size +		/*data type		*/
                    attr->ds_size +		/*data space		*/
                    attr->data_size;		/*the data itself	*/
    else if(version == H5O_ATTR_VERSION_3 || version == H5O_ATTR_VERSION_4)
        ret_value += 1 +                        /*character encoding    */
                    name_len	+		/*attribute name	*/
                    attr->dt_size +		/*data type		*/
                    attr->ds_size +		/*data space		*/
                    attr->data_size;		/*the data itself	*/
    else
        HDassert(0 && "Bad attribute version");

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_attr_size() */


/*-------------------------------------------------------------------------
 * Function:    H5O_attr_reset
 *
 * Purpose:     Frees resources within a attribute message, but doesn't free
 *              the message itself.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Robb Matzke
 *              Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_attr_reset(void *_mesg)
{
    H5A_t                  *attr = (H5A_t *)_mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_attr_reset)

    if(attr)
        H5A_free(attr);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_attr_reset() */


/*-------------------------------------------------------------------------
 * Function:	H5O_attr_free
 *
 * Purpose:	Free's the message
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Thursday, November 18, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_attr_free(void *mesg)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_attr_free)

    HDassert(mesg);

    H5FL_FREE(H5A_t, mesg);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_attr_free() */


/*-------------------------------------------------------------------------
 * Function:    H5O_attr_delete
 *
 * Purpose:     Free file space referenced by message
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Friday, September 26, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_attr_delete(H5F_t *f, hid_t dxpl_id, const void *_mesg, hbool_t adj_link)
{
    const H5A_t            *attr = (const H5A_t *) _mesg;
    htri_t                  tri_ret; 
    H5O_shared_t            sh_mesg;
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_attr_delete)

    /* check args */
    HDassert(f);
    HDassert(attr);

    /* Remove both the datatype and dataspace from the SOHM heap if they're
     * shared there.
     */
    if((tri_ret = H5O_msg_is_shared(H5O_DTYPE_ID, attr->dt)) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_BADMESG, FAIL, "can't tell if datatype is shared")
    if(tri_ret > 0)
    {
        if(NULL == H5O_msg_get_share(H5O_DTYPE_ID, attr->dt, &sh_mesg))
            HGOTO_ERROR(H5E_OHDR, H5E_BADMESG, FAIL, "can't get shared message from datatype")
        if(H5SM_try_delete(f, H5AC_dxpl_id, H5O_DTYPE_ID, &sh_mesg) < 0)
            HGOTO_ERROR(H5E_SOHM, H5E_CANTREMOVE, FAIL, "can't remove datatype from SOHM heap")
    } /* end if */

    if((tri_ret = H5O_msg_is_shared(H5O_SDSPACE_ID, attr->ds)) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_BADMESG, FAIL, "can't tell if dataspace is shared")
    if(tri_ret > 0)
    {
        if(NULL == H5O_msg_get_share(H5O_SDSPACE_ID, attr->ds, &sh_mesg))
            HGOTO_ERROR(H5E_OHDR, H5E_BADMESG, FAIL, "can't get shared message from dataspace")
        if(H5SM_try_delete(f, H5AC_dxpl_id, H5O_SDSPACE_ID, &sh_mesg) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_SOHM, FAIL, "can't remove dataspace from SOHM heap")
    } /* end if */

    /* Check whether datatype is shared */
    if(H5T_committed(attr->dt)) {
        /* Decrement the reference count on the shared datatype, if requested */
        if(adj_link)
            if(H5T_link(attr->dt, -1, dxpl_id) < 0)
                HGOTO_ERROR(H5E_OHDR, H5E_LINKCOUNT, FAIL, "unable to adjust shared datatype link count")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_attr_delete() */


/*-------------------------------------------------------------------------
 * Function:    H5O_attr_link
 *
 * Purpose:     Increment reference count on any objects referenced by
 *              message
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Friday, September 26, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_attr_link(H5F_t UNUSED *f, hid_t dxpl_id, const void *_mesg)
{
    const H5A_t            *attr = (const H5A_t *) _mesg;
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_attr_link)

    /* check args */
    HDassert(f);
    HDassert(attr);

    /* Check whether datatype is shared */
    if(H5T_committed(attr->dt)) {
        /* Increment the reference count on the shared datatype */
        if(H5T_link(attr->dt,1,dxpl_id) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_LINKCOUNT, FAIL, "unable to adjust shared datatype link count")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_attr_link() */


/*-------------------------------------------------------------------------
 * Function:    H5O_attr_pre_copy_file
 *
 * Purpose:     Perform any necessary actions before copying message between
 *              files for attribute messages.
 *
 * Return:      Success:        Non-negative
 *
 *              Failure:        Negative
 *
 * Programmer:  Quincey Koziol
 *              Monday, June 26, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_attr_pre_copy_file(H5F_t UNUSED *file_src, const H5O_msg_class_t UNUSED *type,
    const void UNUSED *native_src, hbool_t *deleted, const H5O_copy_t *cpy_info,
    void UNUSED *udata)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_attr_pre_copy_file)

    /* check args */
    HDassert(deleted);
    HDassert(cpy_info);

    /* If we are not copying attributes into the destination file, indicate
     *  that this message should be deleted.
     */
    if(cpy_info->copy_without_attr)
        *deleted = TRUE;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_attr_pre_copy_file() */


/*-------------------------------------------------------------------------
 * Function:    H5O_attr_copy_file
 *
 * Purpose:     Copies a message from _MESG to _DEST in file
 *
 * Return:      Success:        Ptr to _DEST
 *
 *              Failure:        NULL
 *
 * Programmer:  Quincey Koziol
 *              November 1, 2005
 *
 * Modifications: Peter Cao
 *              December 17, 2005: datatype conversion for variable length datatype
 *              August 1, 2006: fix reference datatype
 *              
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_attr_copy_file(H5F_t *file_src, const H5O_msg_class_t UNUSED *mesg_type,
    void *native_src, H5F_t *file_dst, hid_t dxpl_id,
    H5O_copy_t *cpy_info, void UNUSED *udata)
{
    H5A_t        *attr_src = (H5A_t *)native_src;
    H5A_t        *attr_dst = NULL;

    /* for dataype conversion */
    hid_t       tid_src = -1;           /* Datatype ID for source datatype */
    hid_t       tid_dst = -1;           /* Datatype ID for destination datatype */
    hid_t       tid_mem = -1;           /* Datatype ID for memory datatype */
    void       *buf = NULL;             /* Buffer for copying data */
    void       *reclaim_buf = NULL;     /* Buffer for reclaiming data */
    hid_t       buf_sid = -1;           /* ID for buffer dataspace */

    H5O_shared_t sh_mesg;				/* Shared message information */
    htri_t      tri_ret;                /* htri_t return value */

    void        *ret_value;             /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_attr_copy_file)

    /* check args */
    HDassert(attr_src);
    HDassert(file_dst);
    HDassert(cpy_info);
    HDassert(!cpy_info->copy_without_attr);

    /* Allocate space for the destination message */
    if(NULL == (attr_dst = H5FL_CALLOC(H5A_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Copy the top level of the attribute */
    *attr_dst = *attr_src;

    /* Don't have an opened group location for copy */
    H5O_loc_reset(&(attr_dst->oloc));
    H5G_name_reset(&(attr_dst->path));
    attr_dst->obj_opened = 0;

    /* Copy attribute's name */
    attr_dst->name = H5MM_strdup(attr_src->name);
    HDassert(attr_dst->name);

    /* Copy attribute's datatype */
    /* (Start destination datatype as transient, even if source is named) */
    attr_dst->dt = H5T_copy(attr_src->dt, H5T_COPY_ALL);
    HDassert(attr_dst->dt);

    /* Set the location of the destination datatype */
    if(H5T_set_loc(attr_dst->dt, file_dst, H5T_LOC_DISK) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "cannot mark datatype on disk")

    /* Check for named datatype being copied */
    if(H5T_committed(attr_src->dt)) {
        H5O_loc_t         *src_oloc;           /* Pointer to source datatype's object location */
        H5O_loc_t         *dst_oloc;           /* Pointer to dest. datatype's object location */

        /* Get group entries for source & destination */
        src_oloc = H5T_oloc(attr_src->dt);
        HDassert(src_oloc);
        dst_oloc = H5T_oloc(attr_dst->dt);
        HDassert(dst_oloc);

        /* Reset object location for new object */
        H5O_loc_reset(dst_oloc);
        dst_oloc->file = file_dst;

        /* Copy the shared object from source to destination */
        if(H5O_copy_header_map(src_oloc, dst_oloc, dxpl_id, cpy_info, FALSE) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, NULL, "unable to copy object")
    } /* end if */

    /* Copy the dataspace for the attribute */
	/* JAMES: does this need to be copy_file? */
    attr_dst->ds = H5S_copy(attr_src->ds, FALSE);
    HDassert(attr_dst->ds);

    /* Try to share both the datatype and dataset.  This does nothing if the
     * datatype is committed or sharing is disabled.
     */
    if(H5SM_try_share(file_dst, dxpl_id, H5O_DTYPE_ID, attr_dst->dt) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, NULL, "can't share attribute datatype")
    if(H5SM_try_share(file_dst, dxpl_id, H5O_SDSPACE_ID, attr_dst->ds) <0)
	HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, NULL, "can't share attribute dataspace")

    /* Compute the sizes of the datatype and dataspace. This is their raw
     * size unless they're shared.
     */
    if((tri_ret = H5O_msg_is_shared(H5O_DTYPE_ID, attr_dst->dt)) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_BADMESG, NULL, "unable to determine if datatype is shared")
    if(tri_ret == TRUE) {
        /* Reset shared message information */
        HDmemset(&sh_mesg, 0, sizeof(H5O_shared_t));

        /* Get shared message information for datatype */
        if(NULL == H5O_msg_get_share(H5O_DTYPE_ID, attr_dst->dt, &sh_mesg/*out*/))
            HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, NULL, "unable to get shared message")

        /* Compute shared message size for datatype */
        attr_dst->dt_size = H5O_msg_raw_size(file_dst, H5O_SHARED_ID, &sh_mesg);
    } /* end if */
    else
        attr_dst->dt_size = H5O_msg_raw_size(file_dst, H5O_DTYPE_ID, attr_src->dt);
    HDassert(attr_dst->dt_size > 0);

    if((tri_ret = H5O_msg_is_shared(H5O_SDSPACE_ID, attr_dst->ds)) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_BADMESG, NULL, "unable to determine if dataspace is shared")
    if(tri_ret == TRUE) {
        /* Reset shared message information */
        HDmemset(&sh_mesg, 0, sizeof(H5O_shared_t));

        /* Get shared message information for dataspace */
        if(NULL == H5O_msg_get_share(H5O_SDSPACE_ID, attr_dst->ds, &sh_mesg/*out*/))
            HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, NULL, "unable to get shared message")

        /* Compute shared message size for dataspace */
        attr_dst->ds_size = H5O_msg_raw_size(file_dst, H5O_SHARED_ID, &sh_mesg);
    }
    else {
        attr_dst->ds_size = H5O_msg_raw_size(file_dst, H5O_SDSPACE_ID, attr_src->ds);
    }
    HDassert(attr_dst->ds_size > 0);

    /* Compute the size of the data */
    H5_ASSIGN_OVERFLOW(attr_dst->data_size, H5S_GET_EXTENT_NPOINTS(attr_dst->ds) * H5T_get_size(attr_dst->dt), hsize_t, size_t);

    /* Copy (& convert) the data, if necessary */
    if(attr_src->data) {
        if(NULL == (attr_dst->data = H5FL_BLK_MALLOC(attr_buf, attr_dst->data_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

        /* Check if we need to convert data */
        if(H5T_detect_class(attr_src->dt, H5T_VLEN) > 0) {
            H5T_path_t  *tpath_src_mem, *tpath_mem_dst;   /* Datatype conversion paths */
            H5T_t *dt_mem;              /* Memory datatype */
            size_t src_dt_size;         /* Source datatype size */
            size_t tmp_dt_size;         /* Temp. datatype size */
            size_t max_dt_size;         /* Max atatype size */
            H5S_t *buf_space;           /* Dataspace describing buffer */
            hsize_t buf_dim;            /* Dimension for buffer */
            size_t nelmts;              /* Number of elements in buffer */
            size_t buf_size;            /* Size of copy buffer */

            /* Create datatype ID for src datatype */
            if((tid_src = H5I_register(H5I_DATATYPE, attr_src->dt)) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, NULL, "unable to register source file datatype")

            /* create a memory copy of the variable-length datatype */
            if(NULL == (dt_mem = H5T_copy(attr_src->dt, H5T_COPY_TRANSIENT)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "unable to copy")
            if((tid_mem = H5I_register(H5I_DATATYPE, dt_mem)) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, NULL, "unable to register memory datatype")

            /* create variable-length datatype at the destinaton file */
            if((tid_dst = H5I_register(H5I_DATATYPE, attr_dst->dt)) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, NULL, "unable to register destination file datatype")

            /* Set up the conversion functions */
            if(NULL == (tpath_src_mem = H5T_path_find(attr_src->dt, dt_mem, NULL, NULL, dxpl_id, FALSE)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "unable to convert between src and mem datatypes")
            if(NULL == (tpath_mem_dst = H5T_path_find(dt_mem, attr_dst->dt, NULL, NULL, dxpl_id, FALSE)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "unable to convert between mem and dst datatypes")

            /* Determine largest datatype size */
            if(0 == (src_dt_size = H5T_get_size(attr_src->dt)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "unable to determine datatype size")
            if(0 == (tmp_dt_size = H5T_get_size(dt_mem)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "unable to determine datatype size")
            max_dt_size = MAX(src_dt_size, tmp_dt_size);
            if(0 == (tmp_dt_size = H5T_get_size(attr_dst->dt)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "unable to determine datatype size")
            max_dt_size = MAX(max_dt_size, tmp_dt_size);

            /* Set number of whole elements that fit in buffer */
            if(0 == (nelmts = attr_src->data_size / src_dt_size))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "element size too large")

            /* Set up number of bytes to copy, and initial buffer size */
            buf_size = nelmts * max_dt_size;

            /* Create dataspace for number of elements in buffer */
            buf_dim = nelmts;

            /* Create the space and set the initial extent */
            if(NULL == (buf_space = H5S_create_simple((unsigned)1, &buf_dim, NULL)))
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCREATE, NULL, "can't create simple dataspace")

            /* Atomize */
            if((buf_sid = H5I_register(H5I_DATASPACE, buf_space)) < 0) {
                H5S_close(buf_space);
                HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, NULL, "unable to register dataspace ID")
            } /* end if */

            /* Allocate memory for recclaim buf */
            if(NULL == (reclaim_buf = H5FL_BLK_MALLOC(attr_buf, buf_size)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation NULLed for raw data chunk")

            /* Allocate memory for copying the chunk */
            if(NULL == (buf = H5FL_BLK_MALLOC(attr_buf, buf_size)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation NULLed for raw data chunk")

            HDmemcpy(buf, attr_src->data, attr_src->data_size);

            /* Convert from source file to memory */
            if(H5T_convert(tpath_src_mem, tid_src, tid_mem, nelmts, (size_t)0, (size_t)0, buf, NULL, dxpl_id) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "datatype conversion NULLed")

            HDmemcpy(reclaim_buf, buf, buf_size);

            /* Convert from memory to destination file */
            if(H5T_convert(tpath_mem_dst, tid_mem, tid_dst, nelmts, (size_t)0, (size_t)0, buf, NULL, dxpl_id) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "datatype conversion NULLed")

            HDmemcpy(attr_dst->data, buf, attr_dst->data_size);

            if(H5D_vlen_reclaim(tid_mem, buf_space, H5P_DATASET_XFER_DEFAULT, reclaim_buf) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_BADITER, NULL, "unable to reclaim variable-length data")
        }  /* end if */
        else if((H5T_get_class(attr_src->dt, FALSE) == H5T_REFERENCE) && (file_src != file_dst)) {
            /* copy object pointed by reference. The current implementation does not deal with
               nested reference such as reference in a compound structure */

            /* Check for expanding references */
            if(cpy_info->expand_ref) {
                size_t ref_count;

                /* Determine # of reference elements to copy */
                ref_count = attr_dst->data_size / attr_dst->dt_size;

                /* Copy objects referenced in source buffer to destination file and set destination elements */
                if(H5O_copy_expand_ref(file_src, attr_src->data, dxpl_id,
                        file_dst, attr_dst->data, ref_count, H5T_get_ref_type(attr_src->dt), cpy_info) < 0)
                    HGOTO_ERROR(H5E_ATTR, H5E_CANTCOPY, NULL, "unable to copy reference attribute")
            } /* end if */
            else
                /* Reset value to zero */
                HDmemset(attr_dst->data, 0, attr_dst->data_size);
	} /* end if */
        else {
            HDassert(attr_dst->data_size == attr_src->data_size);
            HDmemcpy(attr_dst->data, attr_src->data, attr_src->data_size);
        } /* end else */
    } /* end if(attr_src->data) */

    /* Indicate that the fill values aren't to be written out */
    attr_dst->initialized = TRUE;

    /* Set return value */
    ret_value = attr_dst;

done:
    if(buf_sid > 0)
        if(H5I_dec_ref(buf_sid) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, NULL, "Can't decrement temporary dataspace ID")
    if(tid_src > 0)
        /* Don't decrement ID, we want to keep underlying datatype */
        if(H5I_remove(tid_src) == NULL)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, NULL, "Can't decrement temporary datatype ID")
    if(tid_dst > 0)
        /* Don't decrement ID, we want to keep underlying datatype */
        if(H5I_remove(tid_dst) == NULL)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, NULL, "Can't decrement temporary datatype ID")
    if(tid_mem > 0)
        /* Decrement the memory datatype ID, it's transient */
        if(H5I_dec_ref(tid_mem) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, NULL, "Can't decrement temporary datatype ID")
    if(buf)
        H5FL_BLK_FREE(attr_buf, buf);
    if(reclaim_buf)
        H5FL_BLK_FREE(attr_buf, reclaim_buf);

    /* Release destination attribute information on failure */
    if(!ret_value)
        if(attr_dst)
            (void)H5A_free(attr_dst);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O_attr_copy_file() */


/*-------------------------------------------------------------------------
 * Function:	H5O_attr_get_share
 *
 * Purpose:	Gets sharing information from the message
 *
 * Return:	Shared message on success/NULL on failure
 *
 * Programmer:	James Laird
 *              Tuesday, October 17, 2006
 *
 *-------------------------------------------------------------------------
 */
void *
H5O_attr_get_share(const void *_mesg, H5O_shared_t *sh /*out*/)
{
    const H5A_t  *mesg = (const H5A_t *)_mesg;
    void         *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_attr_get_share)

    HDassert (mesg);

    ret_value = H5O_msg_copy(H5O_SHARED_ID, &(mesg->sh_loc), sh);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_attr_get_share() */


/*-------------------------------------------------------------------------
 * Function:	H5O_attr_set_share
 *
 * Purpose:	Sets sharing information for the message
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Tuesday, October 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_attr_set_share(void *_mesg/*in,out*/, const H5O_shared_t *sh)
{
    H5A_t  *mesg = (H5A_t *)_mesg;
    herr_t       ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_attr_set_share)

    HDassert (mesg);
    HDassert (sh);

    if(NULL == H5O_msg_copy(H5O_SHARED_ID, sh, &(mesg->sh_loc)))
        ret_value = FAIL;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_attr_set_share() */


/*-------------------------------------------------------------------------
 * Function:	H5O_attr_is_shared
 *
 * Purpose:	Determines if this attribute is shared (committed or a SOHM)
 *              or not.
 *
 * Return:	TRUE if attribute is shared
 *              FALSE if attribute is not shared
 *              Negative on failure
 *
 * Programmer:	James Laird
 *		Tuesday, October 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5O_attr_is_shared(const void *_mesg)
{
    const H5A_t  *mesg = (const H5A_t *)_mesg;
    htri_t       ret_value;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_attr_is_shared)

    HDassert(mesg);

    /* Attributes can't currently be committed, but this should let the
     * library read a "committed attribute" if we ever create one in
     * the future.
     */
    if(mesg->sh_loc.flags & (H5O_COMMITTED_FLAG | H5O_SHARED_IN_HEAP_FLAG))
        ret_value = TRUE;
    else
        ret_value = FALSE;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_attr_is_shared */


/*--------------------------------------------------------------------------
 NAME
    H5O_attr_debug
 PURPOSE
    Prints debugging information for an attribute message
 USAGE
    void *H5O_attr_debug(f, mesg, stream, indent, fwidth)
        H5F_t *f;               IN: pointer to the HDF5 file struct
        const void *mesg;       IN: Pointer to the source attribute struct
        FILE *stream;           IN: Pointer to the stream for output data
        int indent;            IN: Amount to indent information by
        int fwidth;            IN: Field width (?)
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
        This function prints debugging output to the stream passed as a
    parameter.
--------------------------------------------------------------------------*/
static herr_t
H5O_attr_debug(H5F_t *f, hid_t dxpl_id, const void *_mesg, FILE * stream, int indent,
	       int fwidth)
{
    const H5A_t *mesg = (const H5A_t *)_mesg;
    H5O_shared_t	sh_mesg;        /* Shared message information */
    void *dt_mesg;                      /* Pointer to datatype message to dump */
    herr_t      (*debug)(H5F_t*, hid_t, const void*, FILE*, int, int)=NULL;
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_attr_debug)

    /* check args */
    HDassert(f);
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);

    fprintf(stream, "%*s%-*s \"%s\"\n", indent, "", fwidth,
	    "Name:",
	    mesg->name);
    fprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	    "Initialized:",
	    (unsigned int)mesg->initialized);
    fprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	    "Object opened:",
	    (unsigned int)mesg->obj_opened);
    HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
	    "Object:",
	    mesg->oloc.addr);

    fprintf(stream, "%*sData type...\n", indent, "");
    fprintf(stream, "%*s%-*s %lu\n", indent+3, "", MAX(0,fwidth-3),
	    "Size:",
	    (unsigned long)(mesg->dt_size));
    fprintf (stream, "%*s%-*s %s\n", indent+3, "", MAX(0,fwidth-3),
               "Shared:",
               (H5T_committed(mesg->dt) ? "Yes" : "No")
               );
    if(H5T_committed(mesg->dt)) {
        /* Reset shared message information */
        HDmemset(&sh_mesg, 0, sizeof(H5O_shared_t));

        /* Get shared message information from datatype */
        if(NULL == (H5O_MSG_DTYPE->get_share)(mesg->dt, &sh_mesg/*out*/))
            HGOTO_ERROR(H5E_ATTR, H5E_CANTENCODE, FAIL, "can't retrieve shared message information")

        debug = H5O_MSG_SHARED->debug;
        dt_mesg = &sh_mesg;
    } /* end if */
    else {
        debug = H5O_MSG_DTYPE->debug;
        dt_mesg = mesg->dt;
    } /* end else */
    if(debug)
        (debug)(f, dxpl_id, dt_mesg, stream, indent+3, MAX(0, fwidth-3));
    else
        fprintf(stream, "%*s<No info for this message>\n", indent + 6, "");

    fprintf(stream, "%*sData space...\n", indent, "");
    fprintf(stream, "%*s%-*s %lu\n", indent+3, "", MAX(0, fwidth - 3),
	    "Size:",
	    (unsigned long)(mesg->ds_size));
    H5S_debug(f, dxpl_id, mesg->ds, stream, indent+3, MAX(0, fwidth - 3));

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_attr_debug() */

