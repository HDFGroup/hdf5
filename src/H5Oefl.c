/*
 * Copyright (C) 1997 NCSA
 *                    All rights reserved.
 *
 * Programmer: Robb Matzke <matzke@llnl.gov>
 *             Tuesday, November 25, 1997
 */
#include <H5private.h>
#include <H5Eprivate.h>
#include <H5MMprivate.h>
#include <H5Oprivate.h>

#define PABLO_MASK      H5O_efl_mask

/* PRIVATE PROTOTYPES */
static void *H5O_efl_decode(H5F_t *f, size_t raw_size, const uint8 *p);
static herr_t H5O_efl_encode(H5F_t *f, size_t size, uint8 *p,
			     const void *_mesg);
static void *H5O_efl_copy(const void *_mesg, void *_dest);
static size_t H5O_efl_size(H5F_t *f, const void *_mesg);
static herr_t H5O_efl_reset(void *_mesg);
static herr_t H5O_efl_debug(H5F_t *f, const void *_mesg, FILE * stream,
			    intn indent, intn fwidth);

/* This message derives from H5O */
const H5O_class_t       H5O_EFL[1] = {{
    H5O_EFL_ID,             /*message id number             */
    "external file list",   /*message name for debugging    */
    sizeof(H5O_efl_t),      /*native message size           */
    H5O_efl_decode,         /*decode message                */
    H5O_efl_encode,         /*encode message                */
    H5O_efl_copy,           /*copy native value             */
    H5O_efl_size,           /*size of message on disk       */
    H5O_efl_reset,          /*reset method                  */
    H5O_efl_debug,          /*debug the message             */
}};

/* Interface initialization */
static hbool_t interface_initialize_g = FALSE;
#define INTERFACE_INIT  NULL

/*-------------------------------------------------------------------------
 * Function:    H5O_efl_decode
 *
 * Purpose:     Decode an external file list message and return a pointer to
 *              the message (and some other data).
 *
 * Return:      Success:        Ptr to a new message struct.
 *
 *              Failure:        NULL
 *
 * Programmer:  Robb Matzke
 *              Tuesday, November 25, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_efl_decode(H5F_t *f, size_t raw_size, const uint8 *p)
{
    H5O_efl_t              *mesg = NULL;
    int                     i;

    FUNC_ENTER(H5O_efl_decode, NULL);

    /* check args */
    assert(f);
    assert(p);

    /* decode */
    mesg = H5MM_xcalloc(1, sizeof(H5O_efl_t));
    H5F_addr_decode(f, &p, &(mesg->heap_addr));
    UINT32DECODE(p, mesg->nalloc);
    assert(mesg->nalloc > 0);
    UINT32DECODE(p, mesg->nused);
    assert(mesg->nused <= mesg->nalloc);

    mesg->offset = H5MM_xmalloc(mesg->nalloc * sizeof(size_t));
    for (i = 0; i < mesg->nused; i++) {
        UINT32DECODE(p, mesg->offset[i]);
    }

    FUNC_LEAVE(mesg);
}

/*-------------------------------------------------------------------------
 * Function:    H5O_efl_encode
 *
 * Purpose:     Encodes a message
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Tuesday, November 25, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_efl_encode(H5F_t *f, size_t raw_size, uint8 *p, const void *_mesg)
{
    const H5O_efl_t        *mesg = (const H5O_efl_t *) _mesg;
    int                     i;

    FUNC_ENTER(H5O_efl_encode, FAIL);

    /* check args */
    assert(f);
    assert(mesg);
    assert(raw_size == H5O_ALIGN (H5O_efl_size(f, _mesg)));
    assert(p);

    /* encode */
    H5F_addr_encode(f, &p, &(mesg->heap_addr));
    UINT32ENCODE(p, mesg->nalloc);
    UINT32ENCODE(p, mesg->nused);
    for (i = 0; i < mesg->nused; i++) {
        UINT32ENCODE(p, mesg->offset[i]);
    }

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5O_efl_copy
 *
 * Purpose:     Copies a message from _MESG to _DEST, allocating _DEST if
 *              necessary.
 *
 * Return:      Success:        Ptr to _DEST
 *
 *              Failure:        NULL
 *
 * Programmer:  Robb Matzke
 *              Tuesday, November 25, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void            *
H5O_efl_copy(const void *_mesg, void *_dest)
{
    const H5O_efl_t        *mesg = (const H5O_efl_t *) _mesg;
    H5O_efl_t              *dest = (H5O_efl_t *) _dest;
    int                     i;

    FUNC_ENTER(H5O_efl_copy, NULL);

    /* check args */
    assert(mesg);
    if (!dest) {
        dest = H5MM_xcalloc(1, sizeof(H5O_efl_t));
        dest->offset = H5MM_xmalloc(mesg->nalloc * sizeof(size_t));
    } else if (dest->nalloc < mesg->nalloc) {
        H5MM_xfree(dest->offset);
        dest->offset = H5MM_xmalloc(mesg->nalloc * sizeof(size_t));
    }
    dest->heap_addr = mesg->heap_addr;
    dest->nalloc = mesg->nalloc;
    dest->nused = mesg->nused;

    for (i = 0; i < mesg->nused; i++) {
        dest->offset[i] = mesg->offset[i];
    }

    FUNC_LEAVE((void *) dest);
}

/*-------------------------------------------------------------------------
 * Function:    H5O_efl_size
 *
 * Purpose:     Returns the size of the raw message in bytes not counting the
 *              message type or size fields, but only the data fields.  This
 *              function doesn't take into account message alignment.
 *
 * Return:      Success:        Message data size in bytes.
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Tuesday, November 25, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5O_efl_size(H5F_t *f, const void *_mesg)
{
    const H5O_efl_t        *mesg = (const H5O_efl_t *) _mesg;
    size_t                  ret_value = FAIL;

    FUNC_ENTER(H5O_efl_size, FAIL);

    /* check args */
    assert(f);
    assert(mesg);

    ret_value = H5F_SIZEOF_ADDR(f) +    /*heap address          */
        4 +                     /*num slots allocated   */
        4 +                     /*num slots used        */
        mesg->nalloc * 4;       /*name offsets in heap  */

    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:    H5O_efl_reset
 *
 * Purpose:     Frees internal pointers and resets the message to an
 *              initialial state.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Tuesday, November 25, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_efl_reset(void *_mesg)
{
    H5O_efl_t              *mesg = (H5O_efl_t *) _mesg;

    FUNC_ENTER(H5O_efl_reset, FAIL);

    /* check args */
    assert(mesg);

    /* reset */
    mesg->nused = mesg->nalloc = 0;
    mesg->offset = H5MM_xfree(mesg->offset);

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5O_efl_debug
 *
 * Purpose:     Prints debugging info for a message.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Tuesday, November 25, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_efl_debug(H5F_t *f, const void *_mesg, FILE * stream, intn indent,
              intn fwidth)
{
    const H5O_efl_t        *mesg = (const H5O_efl_t *) _mesg;
    char                    buf[64];
    intn                    i;

    FUNC_ENTER(H5O_efl_debug, FAIL);

    /* check args */
    assert(f);
    assert(mesg);
    assert(stream);
    assert(indent >= 0);
    assert(fwidth >= 0);

    fprintf(stream, "%*s%-*s ", indent, "", fwidth,
            "Heap address:");
    H5F_addr_print(stream, &(mesg->heap_addr));
    fprintf(stream, "\n");

    fprintf(stream, "%*s%-*s %u/%u\n", indent, "", fwidth,
            "Slots used/allocated:",
            mesg->nused, mesg->nalloc);

    for (i = 0; i < mesg->nused; i++) {
        sprintf(buf, "Name %d:", i + 1);
        fprintf(stream, "%*s%-*s %lu\n", indent, "", fwidth,
                buf,
                (unsigned long) (mesg->offset[i]));
    }

    FUNC_LEAVE(SUCCEED);
}
