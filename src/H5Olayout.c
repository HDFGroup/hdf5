/*
 * Copyright (C) 1997 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Wednesday, October  8, 1997
 *
 * Purpose:     Messages related to data layout.
 */
#include <H5private.h>
#include <H5Dprivate.h>
#include <H5Eprivate.h>
#include <H5MMprivate.h>
#include <H5Oprivate.h>

/* PRIVATE PROTOTYPES */
static void            *H5O_layout_decode(H5F_t *f, size_t raw_size, const uint8 *p);
static herr_t           H5O_layout_encode(H5F_t *f, size_t size, uint8 *p,
                                          const void *_mesg);
static void            *H5O_layout_copy(const void *_mesg, void *_dest);
static size_t           H5O_layout_size(H5F_t *f, const void *_mesg);
static herr_t           H5O_layout_debug(H5F_t *f, const void *_mesg, FILE * stream,
                                         intn indent, intn fwidth);

/* This message derives from H5O */
const H5O_class_t       H5O_LAYOUT[1] =
{
    {
        H5O_LAYOUT_ID,          /*message id number             */
        "layout",               /*message name for debugging    */
        sizeof(H5O_layout_t),   /*native message size           */
        H5O_layout_decode,      /*decode message                */
        H5O_layout_encode,      /*encode message                */
        H5O_layout_copy,        /*copy the native value         */
        H5O_layout_size,        /*size of message on disk       */
        NULL,                   /*reset method                  */
        H5O_layout_debug,       /*debug the message             */
    }};

/* Interface initialization */
#define PABLO_MASK      H5O_layout_mask
static hbool_t          interface_initialize_g = FALSE;
#define INTERFACE_INIT  NULL

/*-------------------------------------------------------------------------
 * Function:    H5O_layout_decode
 *
 * Purpose:     Decode an data layout message and return a pointer to a
 *              new one created with malloc().
 *
 * Return:      Success:        Ptr to new message in native order.
 *
 *              Failure:        NULL
 *
 * Programmer:  Robb Matzke
 *              Wednesday, October  8, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void            *
H5O_layout_decode(H5F_t *f, size_t raw_size, const uint8 *p)
{
    H5O_layout_t           *mesg = NULL;
    intn                    i;

    FUNC_ENTER(H5O_layout_decode, NULL);

    /* check args */
    assert(f);
    assert(p);

    /* decode */
    mesg = H5MM_xcalloc(1, sizeof(H5O_layout_t));
    H5F_addr_decode(f, &p, &(mesg->addr));
    mesg->ndims = *p++;
    assert(raw_size == H5O_layout_size(f, mesg));

    /* Layout class */
    mesg->type = *p++;
    assert(H5D_CONTIGUOUS == mesg->type || H5D_CHUNKED == mesg->type);

    /* Reserved bytes */
    p += 6;

    /* Read the size */
    for (i = 0; i < mesg->ndims; i++) {
        UINT32DECODE(p, mesg->dim[i]);
    }

    FUNC_LEAVE(mesg);
}

/*-------------------------------------------------------------------------
 * Function:    H5O_layout_encode
 *
 * Purpose:     Encodes a message.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Wednesday, October  8, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_layout_encode(H5F_t *f, size_t raw_size, uint8 *p, const void *_mesg)
{
    const H5O_layout_t     *mesg = (const H5O_layout_t *) _mesg;
    int                     i;

    FUNC_ENTER(H5O_layout_encode, FAIL);

    /* check args */
    assert(f);
    assert(mesg);
    assert(mesg->ndims > 0 && mesg->ndims <= H5O_LAYOUT_NDIMS);
    assert(raw_size == H5O_layout_size(f, _mesg));
    assert(p);

    /* data or B-tree address */
    H5F_addr_encode(f, &p, &(mesg->addr));

    /* number of dimensions */
    *p++ = mesg->ndims;

    /* layout class */
    *p++ = mesg->type;

    /* reserved bytes should be zero */
    for (i = 0; i < 6; i++)
        *p++ = 0;

    /* dimension size */
    for (i = 0; i < mesg->ndims; i++) {
        UINT32ENCODE(p, mesg->dim[i]);
    }

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5O_layout_copy
 *
 * Purpose:     Copies a message from _MESG to _DEST, allocating _DEST if
 *              necessary.
 *
 * Return:      Success:        Ptr to _DEST
 *
 *              Failure:        NULL
 *
 * Programmer:  Robb Matzke
 *              Wednesday, October  8, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void            *
H5O_layout_copy(const void *_mesg, void *_dest)
{
    const H5O_layout_t     *mesg = (const H5O_layout_t *) _mesg;
    H5O_layout_t           *dest = (H5O_layout_t *) _dest;

    FUNC_ENTER(H5O_layout_copy, NULL);

    /* check args */
    assert(mesg);
    if (!dest)
        dest = H5MM_xcalloc(1, sizeof(H5O_layout_t));

    /* copy */
    *dest = *mesg;

    FUNC_LEAVE((void *) dest);
}

/*-------------------------------------------------------------------------
 * Function:    H5O_layout_size
 *
 * Purpose:     Returns the size of the raw message in bytes not counting the
 *              message type or size fields, but only the data fields.  This
 *              function doesn't take into account message alignment.
 *
 * Return:      Success:        Message data size in bytes
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Wednesday, October  8, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5O_layout_size(H5F_t *f, const void *_mesg)
{
    const H5O_layout_t     *mesg = (const H5O_layout_t *) _mesg;
    size_t                  ret_value = FAIL;

    FUNC_ENTER(H5O_layout_size, FAIL);

    /* check args */
    assert(f);
    assert(mesg);
    assert(mesg->ndims > 0 && mesg->ndims <= H5O_LAYOUT_NDIMS);

    ret_value = H5F_SIZEOF_ADDR(f) +    /* B-tree address       */
        1 +                     /* max dimension index  */
        1 +                     /* layout class number  */
        6 +                     /* reserved bytes       */
        mesg->ndims * 4;        /* alignment            */

    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:    H5O_layout_debug
 *
 * Purpose:     Prints debugging info for a message.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Wednesday, October  8, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_layout_debug(H5F_t *f, const void *_mesg, FILE * stream, intn indent,
                 intn fwidth)
{
    const H5O_layout_t     *mesg = (const H5O_layout_t *) _mesg;
    intn                    i;

    FUNC_ENTER(H5O_layout_debug, FAIL);

    /* check args */
    assert(f);
    assert(mesg);
    assert(stream);
    assert(indent >= 0);
    assert(fwidth >= 0);

    fprintf(stream, "%*s%-*s ", indent, "", fwidth,
            H5D_CHUNKED == mesg->type ? "B-tree address:" : "Data address:");
    H5F_addr_print(stream, &(mesg->addr));
    fprintf(stream, "\n");

    fprintf(stream, "%*s%-*s %lu\n", indent, "", fwidth,
            "Number of dimensions:",
            (unsigned long) (mesg->ndims));

    /* Size */
    fprintf(stream, "%*s%-*s {", indent, "", fwidth,
            "Size:");
    for (i = 0; i < mesg->ndims; i++) {
        fprintf(stream, "%s%lu", i ? ", " : "",
                (unsigned long) (mesg->dim[i]));
    }
    fprintf(stream, "}\n");

    FUNC_LEAVE(SUCCEED);
}
