/****************************************************************************
* NCSA HDF                                                                 *
* Software Development Group                                               *
* National Center for Supercomputing Applications                          *
* University of Illinois at Urbana-Champaign                               *
* 605 E. Springfield, Champaign IL 61820                                   *
*                                                                          *
* For conditions of distribution and use, see the accompanying             *
* hdf/COPYING file.                                                        *
*                                                                          *
****************************************************************************/

#ifdef RCSID
static char             RcsId[] = "@(#)$Revision$";
#endif

/* $Id$ */

#define H5A_PACKAGE             /*prevent warning from including H5Tpkg.h */

#include <H5private.h>
#include <H5Eprivate.h>
#include <H5Gprivate.h>
#include <H5MMprivate.h>
#include <H5Oprivate.h>
#include <H5Apkg.h>

#define PABLO_MASK      H5O_attr_mask

/* PRIVATE PROTOTYPES */
static herr_t H5O_attr_encode (H5F_t *f, uint8 *p, const void *mesg);
static void *H5O_attr_decode (H5F_t *f, const uint8 *p, H5O_shared_t *sh);
static void *H5O_attr_copy (const void *_mesg, void *_dest);
static size_t H5O_attr_size (H5F_t *f, const void *_mesg);
static herr_t H5O_attr_reset (void *_mesg);
static herr_t H5O_attr_debug (H5F_t *f, const void *_mesg,
			      FILE * stream, intn indent, intn fwidth);

/* This message derives from H5O */
const H5O_class_t H5O_ATTR[1] = {{
    H5O_ATTR_ID,		/* message id number            */
    "attribute",		/* message name for debugging   */
    sizeof(H5A_t),		/* native message size          */
    H5O_attr_decode,		/* decode message               */
    H5O_attr_encode,		/* encode message               */
    H5O_attr_copy,		/* copy the native value        */
    H5O_attr_size,		/* size of raw message          */
    H5O_attr_reset,		/* reset method                 */
    NULL,			/* get share method		*/
    NULL,			/* set share method		*/
    H5O_attr_debug,		/* debug the message            */
}};

/* Interface initialization */
static hbool_t          interface_initialize_g = FALSE;
#define INTERFACE_INIT  NULL

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
        const uint8 *p;         IN: the raw information buffer
 RETURNS
    Pointer to the new message in native order on success, NULL on failure
 DESCRIPTION
        This function decodes the "raw" disk form of a attribute message
    into a struct in memory native format.  The struct is allocated within this
    function using malloc() and is returned to the caller.
--------------------------------------------------------------------------*/
static void *
H5O_attr_decode(H5F_t *f, const uint8 *p, H5O_shared_t __unused__ *sh)
{
    H5A_t		*attr = NULL;
    H5S_simple_t	*simple;	/*simple dimensionality information  */
    size_t		name_len;   	/*attribute name length */

    FUNC_ENTER(H5O_attr_decode, NULL);

    /* check args */
    assert(f);
    assert(p);

    attr = H5MM_xcalloc(1, sizeof(H5A_t));

    /* Decode and store the name */
    UINT16DECODE(p, name_len);
    attr->name=H5MM_xmalloc(name_len+1);
    HDmemcpy(attr->name,p,name_len);
    attr->name[name_len]='\0';
    p+=name_len;    /* advance the memory pointer */

    /* decode the attribute datatype */
    if((attr->dt=(H5O_DTYPE->decode)(f,p,NULL))==NULL) {
        HRETURN_ERROR(H5E_ATTR, H5E_CANTDECODE, NULL,
                      "can't decode attribute datatype");
    }
    attr->dt_size=(H5O_DTYPE->raw_size)(f,attr->dt);
    p+=attr->dt_size;

    /* decode the attribute dataspace */
    attr->ds = H5MM_xcalloc(1, sizeof(H5S_t));
    if((simple=(H5O_SDSPACE->decode)(f,p,NULL))!=NULL) {
        attr->ds->type = H5S_SIMPLE;
        HDmemcpy(&(attr->ds->u.simple),simple, sizeof(H5S_simple_t));
        H5MM_xfree(simple);
    } else {
        attr->ds->type = H5S_SCALAR;
    } /* end else */
    attr->ds_size=(H5O_SDSPACE->raw_size)(f,&(attr->ds->u.simple));
    p+=attr->ds_size;

    /* Compute the size of the data */
    attr->data_size=H5S_get_npoints(attr->ds)*H5T_get_size(attr->dt);

    /* Go get the data */
    attr->data = H5MM_xmalloc(attr->data_size);
    HDmemcpy(attr->data,p,attr->data_size);

    /* Indicate that the fill values aren't to be written out */
    attr->initialized=1;
    
#ifdef LOTSLATER
    if (hobj) {
        attr->sh_heap = *hobj;
        attr->sh_file = f;
    }
#endif 

    FUNC_LEAVE(attr);
}

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
    SUCCEED/FAIL
 DESCRIPTION
        This function encodes the native memory form of the attribute
    message in the "raw" disk form.
--------------------------------------------------------------------------*/
static herr_t
H5O_attr_encode(H5F_t *f, uint8 *p, const void *mesg)
{
    const H5A_t    *attr = (const H5A_t *) mesg;
    size_t          name_len;   /* Attribute name length */

    FUNC_ENTER(H5O_attr_encode, FAIL);

    /* check args */
    assert(f);
    assert(p);
    assert(attr);

    /* encode the attribute name */
    name_len=HDstrlen(attr->name);
    UINT16ENCODE(p, name_len);
    HDmemcpy(p,attr->name,name_len);
    p+=name_len;

    /* encode the attribute datatype */
    if((H5O_DTYPE->encode)(f,p,attr->dt)<0) {
        HRETURN_ERROR(H5E_ATTR, H5E_CANTENCODE, FAIL,
                      "can't encode attribute datatype");
    }
    p+=attr->dt_size;

    /* encode the attribute dataspace */
    if((H5O_SDSPACE->encode)(f,p,&(attr->ds->u.simple))<0) {
        HRETURN_ERROR(H5E_ATTR, H5E_CANTENCODE, FAIL,
                      "can't encode attribute dataspace");
    }
    p+=attr->ds_size;
    
    /* Store attribute data */
    HDmemcpy(p,attr->data,attr->data_size);

    FUNC_LEAVE(SUCCEED);
}

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
static void            *
H5O_attr_copy(const void *_src, void *_dst)
{
    const H5A_t            *src = (const H5A_t *) _src;
    H5A_t                  *dst = NULL;

    FUNC_ENTER(H5O_attr_copy, NULL);

    /* check args */
    assert(src);

    /* copy */
    if (NULL == (dst = H5A_copy(src))) {
        HRETURN_ERROR(H5E_ATTR, H5E_CANTINIT, NULL, "can't copy attribute");
    }
    /* was result already allocated? */
    if (_dst) {
        *((H5A_t *) _dst) = *dst;
        H5MM_xfree(dst);
        dst = (H5A_t *) _dst;
    }
    FUNC_LEAVE((void *) dst);
}

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
--------------------------------------------------------------------------*/
static size_t
H5O_attr_size(H5F_t __unused__ *f, const void *mesg)
{
    size_t                  ret_value = 0;
    const H5A_t            *attr = (const H5A_t *) mesg;

    FUNC_ENTER(H5O_attr_size, 0);

    assert(attr);

    /* Get size of name */
    ret_value=2;    /* Size to store length of name */
    ret_value+=HDstrlen(attr->name); /* Add length of name (non-zero terminated) */

    /* Get size of datatype information */
    ret_value+=attr->dt_size;

    /* Get size of [simple] dataspace information */
    ret_value+=attr->ds_size;

    /* Get size of attribute data */
    ret_value+=attr->data_size;

    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:    H5O_attr_reset
 *
 * Purpose:     Frees resources within a attribute message, but doesn't free
 *              the message itself.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_attr_reset(void *_mesg)
{
    H5A_t                  *attr = (H5A_t *) _mesg;
    H5A_t                  *tmp = NULL;

    FUNC_ENTER(H5O_attr_reset, FAIL);

    if (attr) {
        tmp = H5MM_xmalloc(sizeof(H5A_t));
        HDmemcpy(tmp,attr,sizeof(H5A_t));
        H5A_close(tmp);
        HDmemset(attr, 0, sizeof(H5A_t));
    }
    FUNC_LEAVE(SUCCEED);
}

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
        intn indent;            IN: Amount to indent information by
        intn fwidth;            IN: Field width (?)
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
        This function prints debugging output to the stream passed as a 
    parameter.
--------------------------------------------------------------------------*/
static herr_t
H5O_attr_debug(H5F_t __unused__ *f, const void __unused__ *mesg,
	       FILE __unused__ * stream, intn __unused__ indent,
	       intn __unused__ fwidth)
{
#ifdef LATER
    const char             *s;
    char                    buf[256];
    intn                    i, j;
#endif /* LATER */

    FUNC_ENTER(H5O_attr_debug, FAIL);

    /* check args */
    assert(f);
    assert(stream);
    assert(indent >= 0);
    assert(fwidth >= 0);

#ifdef LATER
    switch (dt->type) {
    case H5T_INTEGER:
        s = "integer";
        break;
    case H5T_FLOAT:
        s = "floating-point";
        break;
    case H5T_TIME:
        s = "date and time";
        break;
    case H5T_STRING:
        s = "text string";
        break;
    case H5T_BITFIELD:
        s = "bit field";
        break;
    case H5T_OPAQUE:
        s = "opaque";
        break;
    case H5T_COMPOUND:
        s = "compound";
        break;
    default:
        sprintf(buf, "H5T_CLASS_%d", (int) (dt->type));
        s = buf;
        break;
    }
    fprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
            "Type class:",
            s);

    fprintf(stream, "%*s%-*s %lu byte%s\n", indent, "", fwidth,
            "Size:",
            (unsigned long) (dt->size), 1 == dt->size ? "" : "s");

    if (H5T_COMPOUND == dt->type) {
        fprintf(stream, "%*s%-*s %d\n", indent, "", fwidth,
                "Number of members:",
                dt->u.compnd.nmembs);
        for (i = 0; i < dt->u.compnd.nmembs; i++) {
            sprintf(buf, "Member %d:", i);
            fprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
                    buf,
                    dt->u.compnd.memb[i].name);
            fprintf(stream, "%*s%-*s %lu\n", indent + 3, "", MAX(0, fwidth-3),
                    "Byte offset:",
                    (unsigned long) (dt->u.compnd.memb[i].offset));
            fprintf(stream, "%*s%-*s %d%s\n", indent + 3, "", MAX(0, fwidth-3),
                    "Dimensionality:",
                    dt->u.compnd.memb[i].ndims,
                    0 == dt->u.compnd.memb[i].ndims ? " (scalar)" : "");
            if (dt->u.compnd.memb[i].ndims > 0) {
                fprintf(stream, "%*s%-*s {", indent + 3, "", MAX(0, fwidth-3),
                        "Size:");
                for (j = 0; j < dt->u.compnd.memb[i].ndims; j++) {
                    fprintf(stream, "%s%lu", j ? ", " : "",
                            (unsigned long) (dt->u.compnd.memb[i].dim[j]));
                }
                fprintf(stream, "}\n");
                fprintf(stream, "%*s%-*s {", indent + 3, "", MAX(0, fwidth-3),
                        "Permutation:");
                for (j = 0; j < dt->u.compnd.memb[i].ndims; j++) {
                    fprintf(stream, "%s%lu", j ? ", " : "",
                            (unsigned long) (dt->u.compnd.memb[i].perm[j]));
                }
                fprintf(stream, "}\n");
            }
            H5O_dtype_debug(f, dt->u.compnd.memb[i].type, stream,
                            indent + 3, MAX(0, fwidth - 3));
        }
    } else {
        switch (dt->u.atomic.order) {
        case H5T_ORDER_LE:
            s = "little endian";
            break;
        case H5T_ORDER_BE:
            s = "big endian";
            break;
        case H5T_ORDER_VAX:
            s = "VAX";
            break;
        case H5T_ORDER_NONE:
            s = "none";
            break;
        default:
            sprintf(buf, "H5T_ORDER_%d", dt->u.atomic.order);
            s = buf;
            break;
        }
        fprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
                "Byte order:",
                s);

        fprintf(stream, "%*s%-*s %lu bit%s\n", indent, "", fwidth,
                "Precision:",
                (unsigned long) (dt->u.atomic.prec),
                1 == dt->u.atomic.prec ? "" : "s");

        fprintf(stream, "%*s%-*s %lu bit%s\n", indent, "", fwidth,
                "Offset:",
                (unsigned long) (dt->u.atomic.offset),
                1 == dt->u.atomic.offset ? "" : "s");

        switch (dt->u.atomic.lsb_pad) {
        case H5T_PAD_ZERO:
            s = "zero";
            break;
        case H5T_PAD_ONE:
            s = "one";
            break;
        default:
            s = "pad?";
            break;
        }
        fprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
                "Low pad type:", s);

        switch (dt->u.atomic.msb_pad) {
        case H5T_PAD_ZERO:
            s = "zero";
            break;
        case H5T_PAD_ONE:
            s = "one";
            break;
        default:
            s = "pad?";
            break;
        }
        fprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
                "High pad type:", s);

        if (H5T_FLOAT == dt->type) {
            switch (dt->u.atomic.u.f.pad) {
            case H5T_PAD_ZERO:
                s = "zero";
                break;
            case H5T_PAD_ONE:
                s = "one";
                break;
            default:
                if (dt->u.atomic.u.f.pad < 0) {
                    sprintf(buf, "H5T_PAD_%d", -(dt->u.atomic.u.f.pad));
                } else {
                    sprintf(buf, "bit-%d", dt->u.atomic.u.f.pad);
                }
                s = buf;
                break;
            }
            fprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
                    "Internal pad type:", s);

            switch (dt->u.atomic.u.f.norm) {
            case H5T_NORM_IMPLIED:
                s = "implied";
                break;
            case H5T_NORM_MSBSET:
                s = "msb set";
                break;
            case H5T_NORM_NONE:
                s = "none";
                break;
            default:
                sprintf(buf, "H5T_NORM_%d", (int) (dt->u.atomic.u.f.norm));
                s = buf;
            }
            fprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
                    "Normalization:", s);

            fprintf(stream, "%*s%-*s %lu\n", indent, "", fwidth,
                    "Sign bit location:",
                    (unsigned long) (dt->u.atomic.u.f.sign));

            fprintf(stream, "%*s%-*s %lu\n", indent, "", fwidth,
                    "Exponent location:",
                    (unsigned long) (dt->u.atomic.u.f.epos));

            fprintf(stream, "%*s%-*s 0x%08lx\n", indent, "", fwidth,
                    "Exponent bias:",
                    (unsigned long) (dt->u.atomic.u.f.ebias));

            fprintf(stream, "%*s%-*s %lu\n", indent, "", fwidth,
                    "Exponent size:",
                    (unsigned long) (dt->u.atomic.u.f.esize));

            fprintf(stream, "%*s%-*s %lu\n", indent, "", fwidth,
                    "Mantissa location:",
                    (unsigned long) (dt->u.atomic.u.f.mpos));

            fprintf(stream, "%*s%-*s %lu\n", indent, "", fwidth,
                    "Mantissa size:",
                    (unsigned long) (dt->u.atomic.u.f.msize));

        } else if (H5T_INTEGER == dt->type) {
            switch (dt->u.atomic.u.i.sign) {
            case H5T_SGN_NONE:
                s = "none";
                break;
            case H5T_SGN_2:
                s = "2's comp";
                break;
            default:
                sprintf(buf, "H5T_SGN_%d", (int) (dt->u.atomic.u.i.sign));
                s = buf;
                break;
            }
            fprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
                    "Sign scheme:", s);

        }
    }
#endif /* LATER */

    FUNC_LEAVE(SUCCEED);
}

