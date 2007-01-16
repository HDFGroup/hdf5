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

#define H5O_PACKAGE	/*suppress error about including H5Opkg	  */
#define H5S_PACKAGE		/*prevent warning from including H5Spkg.h */

#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FLprivate.h"	/* Free lists                           */
#include "H5Gprivate.h"		/* Groups			  	*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Opkg.h"		/* Object headers		  	*/
#include "H5Spkg.h"		/* Dataspaces 				*/


/* PRIVATE PROTOTYPES */
static void *H5O_sdspace_decode(H5F_t *f, hid_t dxpl_id, const uint8_t *p);
static herr_t H5O_sdspace_encode(H5F_t *f, uint8_t *p, const void *_mesg);
static void *H5O_sdspace_copy(const void *_mesg, void *_dest);
static size_t H5O_sdspace_size(const H5F_t *f, const void *_mesg);
static herr_t H5O_sdspace_reset(void *_mesg);
static herr_t H5O_sdspace_free (void *_mesg);
static void *H5O_sdspace_get_share(const void *_mesg, H5O_shared_t *sh);
static herr_t H5O_sdspace_set_share(void *_mesg, const H5O_shared_t *sh);
static htri_t H5O_sdspace_is_shared(const void *_mesg);
static herr_t H5O_sdspace_pre_copy_file(H5F_t *file_src, const H5O_msg_class_t *type,
    const void *mesg_src, hbool_t *deleted, const H5O_copy_t *cpy_info, void *_udata);
static herr_t H5O_sdspace_debug(H5F_t *f, hid_t dxpl_id, const void *_mesg,
				FILE * stream, int indent, int fwidth);

/* This message derives from H5O message class */
const H5O_msg_class_t H5O_MSG_SDSPACE[1] = {{
    H5O_SDSPACE_ID,	    	/* message id number		    	*/
    "dataspace",	    	/* message name for debugging	   	*/
    sizeof(H5S_extent_t),   	/* native message size		    	*/
    H5O_sdspace_decode,	    	/* decode message			*/
    H5O_sdspace_encode,	    	/* encode message			*/
    H5O_sdspace_copy,	    	/* copy the native value		*/
    H5O_sdspace_size,	    	/* size of symbol table entry	    	*/
    H5O_sdspace_reset,	    	/* default reset method		    	*/
    H5O_sdspace_free,		/* free method				*/
    NULL,		        /* file delete method		*/
    NULL,			/* link method			*/
    H5O_sdspace_get_share,    	/* get share method			*/
    H5O_sdspace_set_share,	/* set share method			*/
    NULL,		    	/*can share method		*/
    H5O_sdspace_is_shared,	/* is shared method			*/
    H5O_sdspace_pre_copy_file,	/* pre copy native value to file */
    NULL,			/* copy native value to file    */
    NULL,			/* post copy native value to file    */
    H5O_sdspace_debug	        /* debug the message		    	*/
}};

/* Initial version of the dataspace information */
#define H5O_SDSPACE_VERSION_1	1

/* This version adds support for "null" dataspaces, encodes the type of the
 *      dataspace in the message and eliminated the rest of the "reserved"
 *      bytes.
 */
#define H5O_SDSPACE_VERSION_2	2

/* The latest version of the format.  Look through the 'encode' 
 *      and 'size' callbacks for places to change when updating this. */
#define H5O_SDSPACE_VERSION_LATEST H5O_SDSPACE_VERSION_2

/* Declare external the free list for H5S_extent_t's */
H5FL_EXTERN(H5S_extent_t);

/* Declare external the free list for hsize_t arrays */
H5FL_ARR_EXTERN(hsize_t);


/*--------------------------------------------------------------------------
 NAME
    H5O_sdspace_decode
 PURPOSE
    Decode a simple dimensionality message and return a pointer to a memory
	struct with the decoded information
 USAGE
    void *H5O_sdspace_decode(f, raw_size, p)
	H5F_t *f;	  IN: pointer to the HDF5 file struct
	size_t raw_size;	IN: size of the raw information buffer
	const uint8 *p;		IN: the raw information buffer
 RETURNS
    Pointer to the new message in native order on success, NULL on failure
 DESCRIPTION
	This function decodes the "raw" disk form of a simple dimensionality
    message into a struct in memory native format.  The struct is allocated
    within this function using malloc() and is returned to the caller.

 MODIFICATIONS
	Robb Matzke, 1998-04-09
	The current and maximum dimensions are now H5F_SIZEOF_SIZET bytes
	instead of just four bytes.

  	Robb Matzke, 1998-07-20
        Added a version number and reformatted the message for aligment.

        Raymond Lu
        April 8, 2004
        Added the type of dataspace into this header message using a reserved
        byte.

--------------------------------------------------------------------------*/
static void *
H5O_sdspace_decode(H5F_t *f, hid_t UNUSED dxpl_id, const uint8_t *p)
{
    H5S_extent_t	*sdim = NULL;/* New extent dimensionality structure */
    void		*ret_value;
    unsigned		i;		/* local counting variable */
    unsigned		flags, version;

    FUNC_ENTER_NOAPI_NOINIT(H5O_sdspace_decode)

    /* check args */
    HDassert(f);
    HDassert(p);

    /* decode */
    if(NULL == (sdim = H5FL_CALLOC(H5S_extent_t)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_NOSPACE, NULL, "dataspace structure allocation failed")

    /* Check version */
    version = *p++;
    if(version < H5O_SDSPACE_VERSION_1 || version > H5O_SDSPACE_VERSION_2)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, NULL, "wrong version number in dataspace message")

    /* Get rank */
    sdim->rank = *p++;
    if(sdim->rank > H5S_MAX_RANK)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, NULL, "simple dataspace dimensionality is too large")

    /* Get dataspace flags for later */
    flags = *p++;

    /* Get or determine the type of the extent */
    if(version >= H5O_SDSPACE_VERSION_2)
        sdim->type = (H5S_class_t)*p++;
    else {
        /* Set the dataspace type to be simple or scalar as appropriate */
        if(sdim->rank > 0)
            sdim->type = H5S_SIMPLE;
        else
            sdim->type = H5S_SCALAR;

        /* Increment past reserved byte */
        p++;
    } /* end else */

    /* Only Version 1 has these reserved bytes */
    if(version == H5O_SDSPACE_VERSION_1)
        p += 4; /*reserved*/

    /* Decode dimension sizes */
    if(sdim->rank > 0) {
        if(NULL == (sdim->size = H5FL_ARR_MALLOC(hsize_t, (size_t)sdim->rank)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
        for(i = 0; i < sdim->rank; i++) {
            H5F_DECODE_LENGTH(f, p, sdim->size[i]);
#ifndef H5_HAVE_LARGE_HSIZET
            /* Rudimentary check for overflow of the dimension size */
            if(sdim->size[i] == 0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_BADSIZE, NULL, "invalid size detected")
#endif /* H5_HAVE_LARGE_HSIZET */
        } /* end for */

        if(flags & H5S_VALID_MAX) {
            if(NULL == (sdim->max = H5FL_ARR_MALLOC(hsize_t, (size_t)sdim->rank)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
            for(i = 0; i < sdim->rank; i++)
                H5F_DECODE_LENGTH (f, p, sdim->max[i]);
        } /* end if */
    } /* end if */

    /* Compute the number of elements in the extent */
    if(sdim->type == H5S_NULL)
        sdim->nelem = 0;
    else {
        for(i = 0, sdim->nelem = 1; i < sdim->rank; i++)
            sdim->nelem *= sdim->size[i];
    } /* end else */

    /* Set return value */
    ret_value = (void*)sdim;	/*success*/

done:
    if(!ret_value && sdim) {
        H5S_extent_release(sdim);
        H5FL_FREE(H5S_extent_t, sdim);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_sdspace_decode() */


/*--------------------------------------------------------------------------
 NAME
    H5O_sdspace_encode
 PURPOSE
    Encode a simple dimensionality message
 USAGE
    herr_t H5O_sdspace_encode(f, raw_size, p, mesg)
	H5F_t *f;	        IN: pointer to the HDF5 file struct
	size_t raw_size;	IN: size of the raw information buffer
	const uint8 *p;		IN: the raw information buffer
	const void *mesg;	IN: Pointer to the extent dimensionality struct
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
	This function encodes the native memory form of the simple
    dimensionality message in the "raw" disk form.

 MODIFICATIONS
	Robb Matzke, 1998-04-09
	The current and maximum dimensions are now H5F_SIZEOF_SIZET bytes
	instead of just four bytes.

  	Robb Matzke, 1998-07-20
        Added a version number and reformatted the message for aligment.

        Raymond Lu
        April 8, 2004
        Added the type of dataspace into this header message using a reserved
        byte.

--------------------------------------------------------------------------*/
static herr_t
H5O_sdspace_encode(H5F_t *f, uint8_t *p, const void *_mesg)
{
    const H5S_extent_t	*sdim = (const H5S_extent_t *)_mesg;
    unsigned		flags = 0;
    unsigned		version;
    hbool_t             use_latest_format;      /* Flag indicating the newest file format should be used */
    unsigned		u;  /* Local counting variable */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_sdspace_encode)

    /* check args */
    HDassert(f);
    HDassert(p);
    HDassert(sdim);

    /* Get the file's 'use the latest version of the format' flag */
    use_latest_format = H5F_USE_LATEST_FORMAT(f);

    /* Version */
    if(use_latest_format)
        version = H5O_SDSPACE_VERSION_LATEST;
    else if(sdim->type == H5S_NULL)
        version = H5O_SDSPACE_VERSION_2;
    else
        version = H5O_SDSPACE_VERSION_1;
    *p++ = version;

    /* Rank */
    *p++ = sdim->rank;

    /* Flags */
    if(sdim->max)
        flags |= H5S_VALID_MAX;
    *p++ = flags;

    /* Dataspace type */
    if(version > H5O_SDSPACE_VERSION_1)
        *p++ = sdim->type;
    else {
        *p++ = 0; /*reserved*/
        *p++ = 0; /*reserved*/
        *p++ = 0; /*reserved*/
        *p++ = 0; /*reserved*/
        *p++ = 0; /*reserved*/
    } /* end else */

    /* Current & maximum dimensions */
    if(sdim->rank > 0) {
        for(u = 0; u < sdim->rank; u++)
            H5F_ENCODE_LENGTH(f, p, sdim->size[u]);
        if(flags & H5S_VALID_MAX) {
            for(u = 0; u < sdim->rank; u++)
                H5F_ENCODE_LENGTH(f, p, sdim->max[u]);
        } /* end if */
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_sdspace_encode() */


/*--------------------------------------------------------------------------
 NAME
    H5O_sdspace_copy
 PURPOSE
    Copies a message from MESG to DEST, allocating DEST if necessary.
 USAGE
    void *H5O_sdspace_copy(mesg, dest)
	const void *mesg;	IN: Pointer to the source extent dimensionality struct
	const void *dest;	IN: Pointer to the destination extent dimensionality struct
 RETURNS
    Pointer to DEST on success, NULL on failure
 DESCRIPTION
	This function copies a native (memory) simple dimensionality message,
    allocating the destination structure if necessary.
 MODIFICATIONS
    Raymond Lu
    April 8, 2004
    Changed operation on H5S_simple_t to H5S_extent_t.

--------------------------------------------------------------------------*/
static void *
H5O_sdspace_copy(const void *mesg, void *dest)
{
    const H5S_extent_t	   *src = (const H5S_extent_t *) mesg;
    H5S_extent_t	   *dst = (H5S_extent_t *) dest;
    void                   *ret_value;          /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_sdspace_copy)

    /* check args */
    assert(src);
    if (!dst && NULL==(dst = H5FL_MALLOC(H5S_extent_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Copy extent information */
    if(H5S_extent_copy(dst,src)<0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOPY, NULL, "can't copy extent")

    /* Set return value */
    ret_value=dst;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*--------------------------------------------------------------------------
 NAME
    H5O_sdspace_size
 PURPOSE
    Return the raw message size in bytes
 USAGE
    void *H5O_sdspace_size(f, mesg)
	H5F_t *f;	  IN: pointer to the HDF5 file struct
	const void *mesg;	IN: Pointer to the source extent dimensionality struct
 RETURNS
    Size of message on success, zero on failure
 DESCRIPTION
	This function returns the size of the raw simple dimensionality message on
    success.  (Not counting the message type or size fields, only the data
    portion of the message).  It doesn't take into account alignment.

 MODIFICATIONS
	Robb Matzke, 1998-04-09
	The current and maximum dimensions are now H5F_SIZEOF_SIZET bytes
	instead of just four bytes.
--------------------------------------------------------------------------*/
static size_t
H5O_sdspace_size(const H5F_t *f, const void *_mesg)
{
    const H5S_extent_t	*space = (const H5S_extent_t *)_mesg;
    hbool_t             use_latest_format;      /* Flag indicating the newest file format should be used */
    size_t		ret_value;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_sdspace_size)

    /* Get the file's 'use the latest version of the format' flag */
    use_latest_format = H5F_USE_LATEST_FORMAT(f);

    /* Basic information for all dataspace messages */
    ret_value = 1 +             /* Version */
            1 +                 /* Rank */
            1 +                 /* Flags */
            1 +                 /* Dataspace type/reserved */
            (use_latest_format ? 0 : 4); /* Eliminated/reserved */

    /* Add in the dimension sizes */
    ret_value += space->rank * H5F_SIZEOF_SIZE(f);

    /* Add in the space for the maximum dimensions, if they are present */
    ret_value += space->max ? (space->rank * H5F_SIZEOF_SIZE(f)) : 0;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_sdspace_size() */


/*-------------------------------------------------------------------------
 * Function:	H5O_sdspace_reset
 *
 * Purpose:	Frees the inside of a dataspace message and resets it to some
 *		initial value.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Thursday, April 30, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_sdspace_reset(void *_mesg)
{
    H5S_extent_t	*mesg = (H5S_extent_t*)_mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_sdspace_reset)

    H5S_extent_release(mesg);

    FUNC_LEAVE_NOAPI(SUCCEED)
}


/*-------------------------------------------------------------------------
 * Function:	H5O_sdsdpace_free
 *
 * Purpose:	Free's the message
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Thursday, March 30, 2000
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_sdspace_free (void *mesg)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_sdspace_free)

    assert (mesg);

    H5FL_FREE(H5S_extent_t,mesg);

    FUNC_LEAVE_NOAPI(SUCCEED)
}


/*-------------------------------------------------------------------------
 * Function:	H5O_sdspace_get_share
 *
 * Purpose:	Gets sharing information from the message
 *
 * Return:	Shared message on success/NULL on failure
 *
 * Programmer:	James Laird
 *              Tuesday, October 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_sdspace_get_share(const void *_mesg, H5O_shared_t *sh /*out*/)
{
    const H5S_extent_t  *mesg = (const H5S_extent_t *)_mesg;
    void       *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_sdspace_get_share)

    HDassert (mesg);

    ret_value = H5O_msg_copy(H5O_SHARED_ID, &(mesg->sh_loc), sh);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_sdspace_get_share() */


/*-------------------------------------------------------------------------
 * Function:	H5O_sdspace_set_share
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
H5O_sdspace_set_share(void *_mesg/*in,out*/, const H5O_shared_t *sh)
{
    H5S_extent_t  *mesg = (H5S_extent_t *)_mesg;
    herr_t       ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_sdspace_set_share)

    HDassert (mesg);
    HDassert (sh);

    if(NULL == H5O_msg_copy(H5O_SHARED_ID, sh, &(mesg->sh_loc)))
        ret_value = FAIL;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_sdspace_set_share() */


/*-------------------------------------------------------------------------
 * Function:	H5O_sdspace_is_shared
 *
 * Purpose:	Determines if this dataspace is shared (committed or a SOHM)
 *              or not.
 *
 * Return:	TRUE if dataspace is shared
 *              FALSE if dataspace is not shared
 *              Negative on failure
 *
 * Programmer:	James Laird
 *		Monday, October 16, 2006
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5O_sdspace_is_shared(const void *_mesg)
{
    const H5S_extent_t  *mesg = (const H5S_extent_t *)_mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_sdspace_is_shared)

    HDassert(mesg);

    /* Dataspaces can't currently be committed, but this should let the
     * library read a "committed dataspace" if we ever create one in
     * the future.
     */
    FUNC_LEAVE_NOAPI(H5O_IS_SHARED(mesg->sh_loc.flags))
} /* end H5O_sdspace_is_shared() */


/*-------------------------------------------------------------------------
 * Function:    H5O_sdspace_pre_copy_file
 *
 * Purpose:     Perform any necessary actions before copying message between
 *              files
 *
 * Return:      Success:        Non-negative
 *
 *              Failure:        Negative
 *
 * Programmer:  Quincey Koziol
 *              November 30, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_sdspace_pre_copy_file(H5F_t *file_src, const H5O_msg_class_t UNUSED *type,
    const void *mesg_src, hbool_t UNUSED *deleted, const H5O_copy_t UNUSED *cpy_info,
    void *_udata)
{
    const H5S_extent_t *src_space_extent = (const H5S_extent_t *)mesg_src;  /* Source dataspace extent */
    H5D_copy_file_ud_t *udata = (H5D_copy_file_ud_t *)_udata;   /* Dataset copying user data */
    herr_t         ret_value = SUCCEED;          /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5O_sdspace_pre_copy_file)

    /* check args */
    HDassert(file_src);
    HDassert(src_space_extent);

    /* If the user data is non-NULL, assume we are copying a dataset
     * and make a copy of the dataspace extent for later in the object copying
     * process.  (We currently only need to make a copy of the dataspace extent
     * if the layout is an early version, but that information isn't
     * available here, so we just make a copy of it in all cases)
     */
    if(udata) {
        /* Allocate copy of dataspace extent */
        if(NULL == (udata->src_space_extent = H5FL_MALLOC(H5S_extent_t)))
            HGOTO_ERROR(H5E_DATASPACE, H5E_NOSPACE, FAIL, "dataspace extent allocation failed")

        /* Create a copy of the dataspace extent */
        if(H5S_extent_copy(udata->src_space_extent, src_space_extent) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOPY, FAIL, "can't copy extent")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_dspace_pre_copy_file() */


/*--------------------------------------------------------------------------
 NAME
    H5O_sdspace_debug
 PURPOSE
    Prints debugging information for a simple dimensionality message
 USAGE
    void *H5O_sdspace_debug(f, mesg, stream, indent, fwidth)
	H5F_t *f;	        IN: pointer to the HDF5 file struct
	const void *mesg;	IN: Pointer to the source extent dimensionality struct
	FILE *stream;		IN: Pointer to the stream for output data
	int indent;		IN: Amount to indent information by
	int fwidth;		IN: Field width (?)
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
	This function prints debugging output to the stream passed as a
    parameter.
--------------------------------------------------------------------------*/
static herr_t
H5O_sdspace_debug(H5F_t UNUSED *f, hid_t UNUSED dxpl_id, const void *mesg,
		  FILE * stream, int indent, int fwidth)
{
    const H5S_extent_t	   *sdim = (const H5S_extent_t *) mesg;
    unsigned		    u;	/* local counting variable */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5O_sdspace_debug)

    /* check args */
    assert(f);
    assert(sdim);
    assert(stream);
    assert(indent >= 0);
    assert(fwidth >= 0);

    HDfprintf(stream, "%*s%-*s %lu\n", indent, "", fwidth,
	    "Rank:",
	    (unsigned long) (sdim->rank));

    if(sdim->rank>0) {
        HDfprintf(stream, "%*s%-*s {", indent, "", fwidth, "Dim Size:");
        for (u = 0; u < sdim->rank; u++)
            HDfprintf (stream, "%s%Hu", u?", ":"", sdim->size[u]);
        HDfprintf (stream, "}\n");

        HDfprintf(stream, "%*s%-*s ", indent, "", fwidth, "Dim Max:");
        if (sdim->max) {
            HDfprintf (stream, "{");
            for (u = 0; u < sdim->rank; u++) {
                if (H5S_UNLIMITED==sdim->max[u]) {
                    HDfprintf (stream, "%sINF", u?", ":"");
                } else {
                    HDfprintf (stream, "%s%Hu", u?", ":"", sdim->max[u]);
                }
            }
            HDfprintf (stream, "}\n");
        } else {
            HDfprintf (stream, "CONSTANT\n");
        }
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
}

