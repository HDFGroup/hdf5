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

/* $Id$ */

#define H5P_PACKAGE     /* prevent warning from including H5Ppkg.h */
#define H5S_PACKAGE     /* prevent warning from including H5Spkg.h */

#include "H5private.h"          /* Generic functions                        */
#include "H5Eprivate.h"         /* Error handling                           */
#include "H5FLprivate.h"        /* Free lists                               */
#include "H5MMprivate.h"        /* Memory management                        */
#include "H5Oprivate.h"         /* Object headers                           */
#include "H5Ppkg.h"		/* Property lists		  	    */
#include "H5Rprivate.h"         /* References                               */
#include "H5Spkg.h"		/* Dataspace functions			    */

#if defined (WIN32) && !defined (__MWERKS__) 
#include <sys/types.h>
#include <sys/timeb.h>
#endif

#ifdef H5_HAVE_FPHDF5

/* Pablo mask */
#define PABLO_MASK  H5O_fphdf5_mask

/* local prototypes */
static void     *H5O_fphdf5_decode(H5F_t *f, hid_t dxpl_id, const uint8_t *p, H5O_shared_t *sh);
static herr_t    H5O_fphdf5_encode(H5F_t *f, uint8_t *p, const void *_mesg);
static size_t    H5O_fphdf5_size(H5F_t *f, const void *_mesg);
static herr_t    H5O_fphdf5_reset(void *_mesg);
static void     *H5O_fphdf5_copy(const void *mesg, void *dest);
static herr_t    H5O_fphdf5_free(void *_mesg);
static herr_t    H5O_fphdf5_debug(H5F_t *f, hid_t dxpl_id, const void *_mesg,
                                  FILE *stream, int indent, int fwidth);

/* This message derives from H5O */
const H5O_class_t H5O_FPHDF5[1] = {{
    H5O_FPHDF5_ID,              /* message id number                    */
    "fphdf5",                   /* message name for debugging           */
    sizeof(H5O_fphdf5_t),       /* native message size                  */
    H5O_fphdf5_decode,          /* decode message                       */
    H5O_fphdf5_encode,          /* encode message                       */
    H5O_fphdf5_copy,            /* copy the native value                */
    H5O_fphdf5_size,            /* size of symbol table entry           */
    H5O_fphdf5_reset,           /* default reset method                 */
    H5O_fphdf5_free,            /* free method                          */
    NULL,                       /* get share method                     */
    NULL,                       /* set share method                     */
    H5O_fphdf5_debug,           /* debug the message                    */
}};

#define H5O_FPHDF5_VERSION      1

/* Is the interface initialized? */
static int interface_initialize_g = 0;
#define INTERFACE_INIT          NULL

/* Define the free list for H5O_fphdf5_t's */
H5FL_DEFINE_STATIC(H5O_fphdf5_t);

/* Declare external the free list for hsize_t arrays */
H5FL_ARR_EXTERN(hsize_t);

/*
 * Function:    H5O_fphdf5_decode
 * Purpose:     Decode a metadata message from the SAP and return a pointer to
 *              a memory struct with the decoded information.
 *
 *              This function decodes the "raw" form of a metadata message
 *              sent from the SAP into a struct in memory native format. The
 *              struct is allocated within this function using malloc() and is
 *              returned to the caller.
 *
 *          H5F_t *f            IN: pointer to the HDF5 file struct
 *          uint8 *p            OUT: the raw information buffer
 *          H5O_shared_t *sh    IN: not used; must be NULL
 *
 * Return:      Success:    Pointer to the new message in native order
 *              Failure:    NULL
 * Programmer:  Bill Wendling, 20. August 2002
 * Modifications:
 */
static void *
H5O_fphdf5_decode(H5F_t *f, hid_t dxpl_id, const uint8_t *p, H5O_shared_t UNUSED *sh)
{
    H5O_fphdf5_t *fmeta = NULL; /* New FPHDF5 metadata structure */
    void *ret_value;
    
    FUNC_ENTER_NOAPI(H5O_fphdf5_decode, NULL);

    /* check args */
    assert(f);
    assert(p);
    assert(!sh);

    /* decode */
    fmeta = H5FL_CALLOC(H5O_fphdf5_t);

    /* decode the OID first */
    NBYTEDECODE(p, fmeta->oid, sizeof(fmeta->oid));

    /* decode the header address next */
    NBYTEDECODE(p, &fmeta->header, sizeof(fmeta->header));

    /* decode the dataspace dimensions next */
    fmeta->sdim = H5O_SDSPACE[0].decode(f, p, NULL);

    if (!fmeta->sdim)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* jump past the dataspace dimensions part */
    p += H5O_SDSPACE[0].raw_size(f, fmeta->sdim);

    /* decode the datatype next */
    fmeta->dtype = H5O_DTYPE[0].decode(f, p, NULL);

    if (!fmeta->dtype)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* jump past the datatype part */
    p += H5O_DTYPE[0].raw_size(f, fmeta->dtype);

    /* decode the modification time next */
    fmeta->mtime = H5O_MTIME[0].decode(f, p, NULL);

    if (!fmeta->mtime)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* jump past the modification time part */
    p += H5O_MTIME[0].raw_size(f, fmeta->mtime);

    /* decode the dataset layout next */
    fmeta->layout = H5O_LAYOUT[0].decode(f, p, NULL);

    if (!fmeta->layout)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* jump past the dataset layout part */
    p += H5O_LAYOUT[0].raw_size(f, fmeta->layout);

    /* decode the group the modification took place in */
    fmeta->group = H5O_NAME[0].decode(f, p, NULL);

    if (!fmeta->group)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* jump past the group name part */
    p += H5O_NAME[0].raw_size(f, fmeta->group);

    /* decode the dataset the modification took place in */
    fmeta->dset = H5O_NAME[0].decode(f, p, NULL);

    if (!fmeta->dset)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* jump past the dataset name part */
    p += H5O_NAME[0].raw_size(f, fmeta->dset);

    /* decode the property list last */
    fmeta->plist = H5O_PLIST[0].decode(f, p, NULL);

    if (!fmeta->plist)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* Set return value */
    ret_value = (void *)fmeta;  /*success*/
    
done:
    if (!ret_value && fmeta) {
        /* free up fmeta */
        if (H5O_SDSPACE[0].free && fmeta->sdim)
            H5O_SDSPACE[0].free(fmeta->sdim);

        if (H5O_DTYPE[0].free && fmeta->dtype)
            H5O_DTYPE[0].free(fmeta->dtype);

        if (H5O_MTIME[0].free && fmeta->mtime)
            H5O_MTIME[0].free(fmeta->mtime);

        if (H5O_LAYOUT[0].free && fmeta->layout)
            H5O_LAYOUT[0].free(fmeta->layout);

        if (H5O_NAME[0].free && fmeta->group)
            H5O_NAME[0].free(fmeta->dset);

        if (H5O_NAME[0].free && fmeta->dset)
            H5O_NAME[0].free(fmeta->dset);

        if (H5O_PLIST[0].free && fmeta->plist)
            H5O_PLIST[0].free(fmeta->plist);

        H5FL_FREE(H5O_fphdf5_t, fmeta);
    }

    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5O_fphdf5_encode
 * Purpose:     Encode a metadata message for the SAP and return a pointer to
 *              a memory struct with the encoded information.
 *
 *              This function encodes the "raw" form of a metadata message
 *              sent from the SAP into a struct in memory native format.
 *
 *          H5F_t *f            IN: pointer to the HDF5 file struct
 *          uint8 *p            OUT: the raw information buffer
 *          const void *mesg    IN: pointer to the metadata to encode
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 * Programmer:  Bill Wendling, 21. August 2002
 * Modifications:
 */
static herr_t
H5O_fphdf5_encode(H5F_t *f, uint8_t *p, const void *mesg)
{
    const H5O_fphdf5_t *fmeta = (const H5O_fphdf5_t *)mesg;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5O_fphdf5_encode, FAIL);

    /* check args */
    assert(f);
    assert(p);
    assert(fmeta);

    /* encode the OID first */
    NBYTEENCODE(p, fmeta->oid, sizeof(fmeta->oid));

    /* encode the header address info next */
    NBYTEENCODE(p, &fmeta->header, sizeof(fmeta->header));

    /* encode the dataspace dimensions next */
    ret_value = H5O_SDSPACE[0].encode(f, p, fmeta->sdim);

    if (ret_value < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    /* jump past the dataspace dimensions part */
    p += H5O_SDSPACE[0].raw_size(f, fmeta->sdim);

    /* encode the datatype next */
    ret_value = H5O_DTYPE[0].encode(f, p, fmeta->dtype);

    if (ret_value < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    /* jump past the datatype part */
    p += H5O_DTYPE[0].raw_size(f, fmeta->dtype);

    /* encode the modification time next */
    ret_value = H5O_MTIME[0].encode(f, p, fmeta->mtime);

    if (ret_value < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    /* jump past the modification time part */
    p += H5O_MTIME[0].raw_size(f, fmeta->mtime);

    /* encode the dataset layout next */
    ret_value = H5O_LAYOUT[0].encode(f, p, fmeta->layout);

    if (ret_value < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    /* jump past the dataset layout part */
    p += H5O_LAYOUT[0].raw_size(f, fmeta->layout);

    /* encode the group name next */
    ret_value = H5O_NAME[0].encode(f, p, fmeta->group);

    if (ret_value < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    /* jump past the group name part */
    p += H5O_NAME[0].raw_size(f, fmeta->group);

    /* encode the dataset name next */
    ret_value = H5O_NAME[0].encode(f, p, fmeta->dset);

    if (ret_value < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    /* jump past the dataset name part */
    p += H5O_NAME[0].raw_size(f, fmeta->dset);

    /* decode the property list last */
    ret_value = H5O_PLIST[0].encode(f, p, fmeta->plist);

    if (ret_value < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

done:
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5O_fphdf5_copy
 * Purpose:     Copies a metadata object from MESG to DEST allocating DEST if
 *              necessary.
 *
 *          const void *mesg    IN: pointer to the metadata to copy
 *          const void *dest 	OUT: pointer to the destination metadata struct
 *
 * Return:      Success:    Pointer to DEST
 *              Failure:    NULL
 * Programmer:  Bill Wendling, 21. August 2002
 * Modifications:
 */
static void *
H5O_fphdf5_copy(const void *mesg, void *dest)
{
    const H5O_fphdf5_t *src = (const H5O_fphdf5_t *)mesg;
    H5O_fphdf5_t *dst = (H5O_fphdf5_t *)dest;
    void *ret_value;

    FUNC_ENTER_NOAPI(H5O_fphdf5_copy, NULL);

    /* check args */
    assert(src);

    if (!dst && NULL == (dst = H5FL_MALLOC(H5O_fphdf5_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* copy the individual metadata*/
    HDmemcpy(dst->oid,src->oid,sizeof(src->oid));

    H5O_SDSPACE[0].copy(src->sdim, dst->sdim);
    dst->header = src->header;
    H5O_DTYPE[0].copy(src->dtype, dst->dtype);
    H5O_MTIME[0].copy(src->mtime, dst->mtime);
    H5O_LAYOUT[0].copy(src->layout, dst->layout);
    H5O_NAME[0].copy(src->group, dst->group);
    H5O_NAME[0].copy(src->dset, dst->dset);

    if (H5O_PLIST[0].copy)
        H5O_PLIST[0].copy(src->plist, dst->plist);

    /* Set return value */
    ret_value = dst;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5O_fphdf5_size
 * Purpose:     Return the raw message size in bytes.
 *
 *              This function returns the size of hte raw simple
 *              dimensionality, datatype, and object path message on succes.
 *              (Not counting the message type or size fields, only the data
 *              portion of the message). It doesn't take into account
 *              alignment.
 *
 *          H5F_t *f            IN: pointer to the HDF5 file struct
 *          const void *mesg    IN: pointer to the metadata structure
 *
 * Return:      Success:    Size of message
 *              Failure:    0
 * Programmer:  Bill Wendling, 21. August 2002
 * Modifications:
 */
static size_t
H5O_fphdf5_size(H5F_t *f, const void *mesg)
{
    const H5O_fphdf5_t *fmeta = (const H5O_fphdf5_t *)mesg;
    size_t ret_value;

    FUNC_ENTER_NOAPI(H5O_fphdf5_size, 0);

    /* add in the metadata sizes */
    ret_value = sizeof(fmeta->oid);
    ret_value += H5O_SDSPACE[0].raw_size(f, fmeta->sdim);
    ret_value += sizeof(fmeta->header);
    ret_value += H5O_DTYPE[0].raw_size(f, fmeta->dtype);
    ret_value += H5O_MTIME[0].raw_size(f, fmeta->mtime);
    ret_value += H5O_LAYOUT[0].raw_size(f, fmeta->layout);
    ret_value += H5O_NAME[0].raw_size(f, fmeta->group);
    ret_value += H5O_NAME[0].raw_size(f, fmeta->dset);
    ret_value += H5O_PLIST[0].raw_size(f, fmeta->plist);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5O_fphdf5_reset
 * Purpose:     Frees the inside of a metadata message and resets it to some
 *              initial value.
 *
 *          const void *mesg    IN: pointer to the metadata to reset
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 * Programmer:  Bill Wendling, 21. August 2002
 * Modifications:
 */
static herr_t
H5O_fphdf5_reset(void *mesg)
{
    H5O_fphdf5_t *fmeta = (H5O_fphdf5_t *)mesg;
    herr_t ret_value = SUCCEED;
    
    FUNC_ENTER_NOAPI(H5O_fphdf5_reset, FAIL);

    /* reset the metadata */
    HDmemset(fmeta->oid,0,sizeof(fmeta->oid));

    if (H5O_SDSPACE[0].reset)
        ret_value = H5O_SDSPACE[0].reset(fmeta->sdim);

    fmeta->header = 0;

    if (H5O_DTYPE[0].reset)
        ret_value = H5O_DTYPE[0].reset(fmeta->dtype);

    if (H5O_MTIME[0].reset)
        ret_value = H5O_MTIME[0].reset(fmeta->mtime);

    if (H5O_LAYOUT[0].reset)
        ret_value = H5O_LAYOUT[0].reset(fmeta->layout);

    if (H5O_NAME[0].reset)
        ret_value = H5O_NAME[0].reset(fmeta->group);

    if (H5O_NAME[0].reset)
        ret_value = H5O_NAME[0].reset(fmeta->dset);

    if (H5O_PLIST[0].reset)
        ret_value = H5O_PLIST[0].reset(fmeta->plist);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5O_fphdf5_free
 * Purpose:     Free's the message
 *
 *          const void *mesg    IN: pointer to the metadata to free
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 * Programmer:  Bill Wendling, 21. August 2002
 * Modifications:
 */
static herr_t
H5O_fphdf5_free(void *mesg)
{
    H5O_fphdf5_t *fmeta = (H5O_fphdf5_t *)mesg;
    herr_t ret_value = SUCCEED;
    
    FUNC_ENTER_NOAPI(H5O_fphdf5_free, FAIL);
    assert(fmeta);

    if (H5O_SDSPACE[0].free && fmeta->sdim)
        ret_value = H5O_SDSPACE[0].free(fmeta->sdim);

    if (H5O_DTYPE[0].free && fmeta->dtype)
        ret_value = H5O_DTYPE[0].free(fmeta->dtype);

    if (H5O_MTIME[0].free && fmeta->mtime)
        ret_value = H5O_MTIME[0].free(fmeta->mtime);

    if (H5O_LAYOUT[0].free && fmeta->layout)
        ret_value = H5O_MTIME[0].free(fmeta->layout);

    if (H5O_NAME[0].free && fmeta->group)
        ret_value = H5O_NAME[0].free(fmeta->group);

    if (H5O_NAME[0].free && fmeta->dset)
        ret_value = H5O_NAME[0].free(fmeta->dset);

    if (H5O_PLIST[0].free && fmeta->plist)
        ret_value = H5O_PLIST[0].free(fmeta->plist);

    H5FL_FREE(H5O_fphdf5_t, fmeta);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}

/*
 * Function:    H5O_fphdf5_debug
 * Purpose:     Prints debugging information for the metadata message.
 *
 *          H5F_t *f            IN: pointer to the HDF5 file struct
 *          const void *mesg    IN: Pointer to the source metadata struct
 *          FILE *stream        IN: Pointer to the stream for output data
 *          int indent          IN: Amount to indent information by
 *          int fwidth          IN: Field width (?)
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 * Programmer:  Bill Wendling, 22. August 2002
 * Modifications:
 */
static herr_t
H5O_fphdf5_debug(H5F_t UNUSED *f, hid_t dxpl_id, const void *mesg,
                 FILE * stream, int indent, int fwidth)
{
    const H5O_fphdf5_t *fmeta = (const H5O_fphdf5_t *) mesg;
    herr_t ret_value = SUCCEED;
    unsigned int i;

    FUNC_ENTER_NOAPI(H5O_fphdf5_debug, FAIL);

    /* check args */
    assert(f);
    assert(fmeta);
    assert(stream);
    assert(indent >= 0);
    assert(fwidth >= 0);

    HDfprintf(stream, "%*s%-*s\n", indent, "", fwidth, "Metadata:");
    HDfprintf(stream, "%*sOID: 0x", indent + 1, "");

    for (i = 0; i < sizeof(fmeta->oid); ++i)
        HDfprintf(stream, "%02x", fmeta->oid[i]);

    HDfprintf(stream, "\n");
    ret_value = H5O_SDSPACE[0].debug(f, fmeta->sdim, stream, indent + 1, fwidth);
    HDfprintf(stream, "%*sHeader Address: %" H5_PRINTF_LL_WIDTH "u\n",
              indent, "", (unsigned long_long)fmeta->header);
    ret_value = H5O_DTYPE[0].debug(f, fmeta->dtype, stream, indent + 1, fwidth);
    ret_value = H5O_MTIME[0].debug(f, fmeta->mtime, stream, indent + 1, fwidth);
    ret_value = H5O_LAYOUT[0].debug(f, fmeta->layout, stream, indent + 1, fwidth);
    ret_value = H5O_NAME[0].debug(f, fmeta->group, stream, indent + 1, fwidth);
    ret_value = H5O_NAME[0].debug(f, fmeta->dset, stream, indent + 1, fwidth);
    ret_value = H5O_PLIST[0].debug(f, fmeta->plist, stream, indent + 1, fwidth);

    HDfprintf(stream, "}\n");

done:
    FUNC_LEAVE_NOAPI(ret_value);
}

#endif  /* H5_HAVE_FPHDF5 */
