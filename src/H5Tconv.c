/*
 * Copyright (C) 1998 Spizella Software
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <robb@arborea.spizella.com>
 *              Tuesday, January 13, 1998
 *
 * Purpose:     Data type conversions.
 */
#define H5T_PACKAGE             /*suppress error about including H5Tpkg   */

#include <H5Aprivate.h>
#include <H5Eprivate.h>
#include <H5MMprivate.h>
#include <H5Tpkg.h>

/* Interface initialization */
static intn interface_initialize_g = FALSE;
#define INTERFACE_INIT NULL

/*-------------------------------------------------------------------------
 * Function:    H5T_conv_noop
 *
 * Purpose:     The no-op conversion.  The library knows about this
 *              conversion without it being registered.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        never fails
 *
 * Programmer:  Robb Matzke
 *              Wednesday, January 14, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_noop(hid_t src_id, hid_t dst_id, size_t nelmts,
              void *buf, const void *background)
{
    FUNC_ENTER(H5T_conv_noop, FAIL);
    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5T_conv_order
 *
 * Purpose:     Convert one type to another when byte order is the only
 *              difference.
 *
 * Note:        This is a soft conversion function.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Tuesday, January 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_order(hid_t src_id, hid_t dst_id, size_t nelmts,
               void *_buf, const void *background)
{
    uint8                  *buf = (uint8 *) _buf;
    uint8                   tmp;
    H5T_t                  *src = NULL;
    H5T_t                  *dst = NULL;
    intn                    i, j, md;

    FUNC_ENTER(H5T_conv_order, FAIL);

    /* Check args */
    if (H5_DATATYPE != H5A_group(src_id) ||
        NULL == (src = H5A_object(src_id)) ||
        H5_DATATYPE != H5A_group(dst_id) ||
        NULL == (dst = H5A_object(dst_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
    }
    if (background) {
        HRETURN_ERROR(H5E_ARGS, H5E_UNSUPPORTED, FAIL,
                      "background values not supported in this conv path");
    }

    if (!buf) {
        /* Capability query */
        if (src->size != dst->size ||
            0 != src->u.atomic.offset ||
            0 != dst->u.atomic.offset ||
            !((H5T_ORDER_BE == src->u.atomic.order &&
               H5T_ORDER_LE == dst->u.atomic.order) ||
              (H5T_ORDER_LE == src->u.atomic.order &&
               H5T_ORDER_BE == dst->u.atomic.order))) {
            HRETURN_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
                          "conversion not supported");
        }
        switch (src->type) {
        case H5T_INTEGER:
            /* nothing to check */
            break;

        case H5T_FLOAT:
            if (src->u.atomic.u.f.sign != dst->u.atomic.u.f.sign ||
                src->u.atomic.u.f.epos != dst->u.atomic.u.f.epos ||
                src->u.atomic.u.f.esize != dst->u.atomic.u.f.esize ||
                src->u.atomic.u.f.ebias != dst->u.atomic.u.f.ebias ||
                src->u.atomic.u.f.mpos != dst->u.atomic.u.f.mpos ||
                src->u.atomic.u.f.msize != dst->u.atomic.u.f.msize ||
                src->u.atomic.u.f.norm != dst->u.atomic.u.f.norm ||
                src->u.atomic.u.f.pad != dst->u.atomic.u.f.pad) {
                HRETURN_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
                              "conversion not supported");
            }
            break;

        default:
            HRETURN_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
                          "conversion not supported");
        }
        HRETURN(SUCCEED);
    }
    /* The conversion */
    md = src->size / 2;
    for (i = 0; i < nelmts; i++, buf += src->size) {
        for (j = 0; j < md; j++) {
            tmp = buf[j];
            buf[j] = buf[src->size - (j + 1)];
            buf[src->size - (j + 1)] = tmp;
        }
    }

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5T_conv_struct
 *
 * Purpose:	Converts between compound data types.  This is a soft
 *		conversion function.  The algorithm is basically:
 *
 * 		For I=1..NUM_MEMBERS do
 *		  If sizeof detination type <= sizeof source type then
 *		    Convert member to destination type;
 *		  Move member as far left as possible;
 *		  
 *		For I=NUM_MEMBERS..1 do
 *		  If not destination type then
 *		    Convert member to destination type;
 *		  Move member to correct position in BACKGROUND
 *
 * 		Copy BACKGROUND to BUF
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Thursday, January 22, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_struct(hid_t src_id, hid_t dst_id, size_t nelmts,
		void *_buf, const void *background)
{
    uint8	*buf = (uint8 *)_buf;	/*cast for pointer arithmetic	*/
    H5T_t	*src = NULL;		/*source data type		*/
    H5T_t	*dst = NULL;		/*destination data type		*/
    intn	*dst2src_map = NULL;	/*maps dst member to src member	*/
    intn	i;
    
    FUNC_ENTER (H5T_conv_struct, FAIL);

    /* Check args */
    if (H5_DATATYPE != H5A_group(src_id) ||
        NULL == (src = H5A_object(src_id)) ||
        H5_DATATYPE != H5A_group(dst_id) ||
        NULL == (dst = H5A_object(dst_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
    }
    if (background) {
        HRETURN_ERROR(H5E_ARGS, H5E_UNSUPPORTED, FAIL,
                      "background values not supported in this conv path");
    }

    /* Capability query? */
    if (!buf) {
	assert (H5T_COMPOUND==src->type);
	assert (H5T_COMPOUND==dst->type);
	HRETURN (SUCCEED);
    }

    /*
     * Build a mapping from destination member number to source member number
     */
    H5T_sort_by_offset (src);
    H5T_sort_by_offset (dst);
    dst2src_map = H5MM_xmalloc (dst->u.compnd.nmembs * sizeof(intn));
    for (i=0; i<src->u.compnd.nmembs; i++) {
    }



    /*
     * For each source member which will be present in the destination,
     * convert the member to the destination type unless it is larger than
     * the source type.  Then move the member to the left-most unoccupied
     * position in the buffer.  This makes the data point as small as
     * possible with all the free space on the right side.
     */
    for (i=0; i<src->u.compnd.nmembs; i++) {
    }


    /*
     * For each source member which will be present in the destination,
     * convert the member to the destination type if it is larger than the
     * source type (that is, has not been converted yet).  Then copy the
     * member to the destination offset in the background buffer.
     */
    for (i=src->u.compnd.nmembs-1; i>=0; --i) {
    }

    
    /*
     * Copy the background buffer back into the in-place conversion buffer.
     */
    HDmemcpy (buf, background, dst->size);








    
    HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL, "not implemented yet");
    FUNC_LEAVE (SUCCEED);
}

