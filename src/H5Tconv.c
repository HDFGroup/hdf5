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

/* Conversion data for H5T_conv_struct() */
typedef struct H5T_conv_struct_t {
    intn	*src2dst;		/*mapping from src to dst memb ID    */
    hid_t	*src_memb_id;		/*source member type ID's	     */
    hid_t	*dst_memb_id;		/*destination member type ID's	     */
    H5T_conv_t	*memb_conv;		/*array of membr conversion functions*/
    H5T_cdata_t	**memb_cdata;		/*array of member cdata pointers     */
} H5T_conv_struct_t;

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
H5T_conv_noop(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata, size_t nelmts,
              void *buf, void *background)
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
H5T_conv_order(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata, size_t nelmts,
               void *_buf, void *background)
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
 * Function:	H5T_conv_struct_init
 *
 * Purpose:	Initialize the `priv' field of `cdata' with conversion
 *		information that is relatively constant.  If `priv' is
 *		already initialized then the member conversion functions
 *		are recalculated.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Monday, January 26, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5T_conv_struct_init (H5T_t *src, H5T_t *dst, H5T_cdata_t *cdata)
{
    H5T_conv_struct_t	*priv = (H5T_conv_struct_t*)(cdata->priv);
    intn		i, j, *src2dst = NULL;
    H5T_t		*type = NULL;
    hid_t		tid;
    
    FUNC_ENTER (H5T_conv_struct_init, FAIL);
    
    if (!priv) {
	/*
	 * Notice: the thing marked with `!' below really is `dst' and not
	 *	   `src' because we're only interested in the members of the
	 * 	   source type that are also in the destination type.
	 */
	cdata->priv = priv = H5MM_xcalloc (1, sizeof(H5T_conv_struct_t));
	priv->src2dst = H5MM_xmalloc (src->u.compnd.nmembs * sizeof(intn));
	priv->src_memb_id = H5MM_xmalloc (/*!*/dst->u.compnd.nmembs *
					  sizeof(hid_t));
	priv->dst_memb_id = H5MM_xmalloc (dst->u.compnd.nmembs *
					  sizeof(hid_t));

	/*
	 * Insure that members are sorted.
	 */
	H5T_sort_by_offset (src);
	H5T_sort_by_offset (dst);

	/*
	 * Build a mapping from source member number to destination member
	 * number. If some source member is not a destination member then that
	 * mapping element will be negative.  Also create atoms for each
	 * source and destination member data type so we can look up the
	 * member data type conversion functions later.
	 */
	for (i=0; i<src->u.compnd.nmembs; i++) {
	    priv->src2dst[i] = -1;
	    for (j=0; j<dst->u.compnd.nmembs; j++) {
		if (!HDstrcmp (src->u.compnd.memb[i].name,
			   dst->u.compnd.memb[j].name)) {
		    priv->src2dst[i] = j;
		    break;
		}
	    }
	    if (priv->src2dst[i]>=0) {
		type = &(src->u.compnd.memb[i].type);
		tid = H5A_register (H5_DATATYPE, type);
		assert (tid>=0);
		priv->src_memb_id[priv->src2dst[i]] = tid;

		type = &(dst->u.compnd.memb[priv->src2dst[i]].type);
		tid = H5A_register (H5_DATATYPE, type);
		assert (tid>=0);
		priv->dst_memb_id[priv->src2dst[i]] = tid;
	    }
	}
    }

    /*
     * (Re)build the cache of member conversion functions and pointers to
     * their cdata entries.
     */
    priv->memb_conv = H5MM_xfree (priv->memb_conv);
    priv->memb_cdata = H5MM_xfree (priv->memb_cdata);
    priv->memb_conv = H5MM_xmalloc (dst->u.compnd.nmembs *
				    sizeof(H5T_conv_t));
    priv->memb_cdata = H5MM_xcalloc (dst->u.compnd.nmembs,
				     sizeof(H5T_cdata_t*));
    src2dst = priv->src2dst;

    for (i=0; i<src->u.compnd.nmembs; i++) {
	if (priv->src2dst[i]>=0) {
	    H5T_conv_t tconv_func = H5Tfind (priv->src_memb_id[src2dst[i]],
					     priv->dst_memb_id[src2dst[i]],
					     priv->memb_cdata+src2dst[i]);
	    if (!tconv_func) {
		H5MM_xfree (priv->src2dst);
		H5MM_xfree (priv->src_memb_id);
		H5MM_xfree (priv->dst_memb_id);
		H5MM_xfree (priv->memb_conv);
		cdata->priv = priv = H5MM_xfree (priv);
		HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			       "unable to convert member data type");
	    }
	    priv->memb_conv[src2dst[i]] = tconv_func;
	}
    }

#ifndef LATER
    /*
     * Always use an initialized background buffer.  Actually, we usually
     * won't need it initialized but we'll almost always need a buffer to
     * move the members around.
     */
    cdata->need_bkg = H5T_BKG_YES;
#endif

    cdata->recalc = FALSE;
    FUNC_LEAVE (SUCCEED);
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
H5T_conv_struct(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata, size_t nelmts,
		void *_buf, void *_bkg)
{
    uint8	*buf = (uint8 *)_buf;	/*cast for pointer arithmetic	*/
    uint8	*bkg = (uint8 *)_bkg;	/*background pointer arithmetic	*/
    H5T_t	*src = NULL;		/*source data type		*/
    H5T_t	*dst = NULL;		/*destination data type		*/
    intn	*src2dst = NULL;	/*maps src member to dst member	*/
    H5T_member_t *src_memb = NULL;	/*source struct member descript.*/
    H5T_member_t *dst_memb = NULL;	/*destination struct memb desc.	*/
    size_t	offset;			/*byte offset wrt struct	*/
    size_t	src_delta, dst_delta;	/*source & destination stride	*/
    intn	elmtno, i;		/*counters			*/
    herr_t	ret_value = FAIL;
    H5T_conv_struct_t *priv = (H5T_conv_struct_t *)(cdata->priv);

    FUNC_ENTER (H5T_conv_struct, FAIL);

    if (!buf && H5T_CONV_INIT==nelmts) {
	/*
	 * First, determine if this conversion function applies to the
	 * conversion path SRC_ID-->DST_ID.  If not, return failure;
	 * otherwise initialize the `priv' field of `cdata' with information
	 * that remains (almost) constant for this conversion path.
	 */
	if (H5_DATATYPE != H5A_group(src_id) ||
	    NULL == (src = H5A_object(src_id)) ||
	    H5_DATATYPE != H5A_group(dst_id) ||
	    NULL == (dst = H5A_object(dst_id))) {
	    HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
	}
	assert (H5T_COMPOUND==src->type);
	assert (H5T_COMPOUND==dst->type);

#ifndef LATER
	/*
	 * Struct members must be scalar for now.
	 */
	for (i=0; i<src->u.compnd.nmembs; i++) {
	    if (src->u.compnd.memb[i].ndims>0) {
		HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			       "array members are not supported yet");
	    }
	}
	for (i=0; i<dst->u.compnd.nmembs; i++) {
	    if (dst->u.compnd.memb[i].ndims>0) {
		HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			       "array members are not supported yet");
	    }
	}
#endif

	if (H5T_conv_struct_init (src, dst, cdata)<0) {
	    HRETURN_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,
			   "unable to initialize conversion data");
	}
	HRETURN (SUCCEED);
	
    } else if (!buf && H5T_CONV_FREE==nelmts) {
	/*
	 * Free the private conversion data.
	 */
	H5MM_xfree (priv->src2dst);
	H5MM_xfree (priv->src_memb_id);
	H5MM_xfree (priv->dst_memb_id);
	H5MM_xfree (priv->memb_conv);
	cdata->priv = priv = H5MM_xfree (priv);
	HRETURN (SUCCEED);
	
    } else if (!buf) {
	/* Some other command we don't know about yet.*/
	HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
		       "unknown conversion command");
    }
    
	
    /* Check args */
    if (H5_DATATYPE != H5A_group(src_id) ||
        NULL == (src = H5A_object(src_id)) ||
        H5_DATATYPE != H5A_group(dst_id) ||
        NULL == (dst = H5A_object(dst_id))) {
        HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
    }
    assert (priv);
    assert (bkg && cdata->need_bkg>=H5T_BKG_TEMP);

    if (cdata->recalc &&
	H5T_conv_struct_init (src, dst, cdata)<0) {
	HRETURN_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,
		       "unable to initialize conversion data");
    }
    

    /*
     * Insure that members are sorted.
     */
    H5T_sort_by_offset (src);
    H5T_sort_by_offset (dst);
    src2dst = priv->src2dst;

    /*
     * Direction of conversion.
     */
    if (dst->size <= src->size) {
	src_delta = src->size;
	dst_delta = dst->size;
    } else {
	src_delta = -(src->size);
	dst_delta = -(dst->size);
	buf += (nelmts-1) * src->size;
	bkg += (nelmts-1) * dst->size;
    }

    for (elmtno=0; elmtno<nelmts; elmtno++) {
	/*
	 * For each source member which will be present in the destination,
	 * convert the member to the destination type unless it is larger than
	 * the source type.  Then move the member to the left-most unoccupied
	 * position in the buffer.  This makes the data point as small as
	 * possible with all the free space on the right side.
	 */
	for (i=0, offset=0; i<src->u.compnd.nmembs; i++) {
	    if (src2dst[i]<0) continue;
	    src_memb = src->u.compnd.memb + i;
	    dst_memb = dst->u.compnd.memb + src2dst[i];
	    
	    if (dst_memb->type.size <= src_memb->type.size) {
		H5T_conv_t tconv_func = priv->memb_conv[src2dst[i]];
		H5T_cdata_t *memb_cdata = priv->memb_cdata[src2dst[i]];
		(tconv_func)(priv->src_memb_id[src2dst[i]],
			     priv->dst_memb_id[src2dst[i]], memb_cdata, 1,
			     buf + src_memb->offset, bkg + dst_memb->offset);

		HDmemmove (buf + offset, buf + src_memb->offset,
			   dst_memb->type.size);
		offset += dst_memb->type.size;
	    } else {
		HDmemmove (buf + offset, buf + src_memb->offset,
			   src_memb->type.size);
		offset += src_memb->type.size;
	    }
	}

	/*
	 * For each source member which will be present in the destination,
	 * convert the member to the destination type if it is larger than the
	 * source type (that is, has not been converted yet).  Then copy the
	 * member to the destination offset in the background buffer.
	 */
	for (i=src->u.compnd.nmembs-1; i>=0; --i) {
	    if (src2dst[i]<0) continue;
	    src_memb = src->u.compnd.memb + i;
	    dst_memb = dst->u.compnd.memb + src2dst[i];
	    offset -= dst_memb->type.size;

	    if (dst_memb->type.size > src_memb->type.size) {
		H5T_conv_t tconv_func = priv->memb_conv[src2dst[i]];
		H5T_cdata_t *memb_cdata = priv->memb_cdata[src2dst[i]];
		(tconv_func)(priv->src_memb_id[src2dst[i]],
			     priv->dst_memb_id[src2dst[i]], memb_cdata, 1,
			     buf + offset, bkg + dst_memb->offset);
	    }
	    HDmemmove (bkg+dst_memb->offset, buf+offset, dst_memb->type.size);
	}
	assert (0==offset);

	/*
	 * Update buf and background.
	 */
	buf += src_delta;
	bkg += dst_delta;
    }
    
    /*
     * Copy the background buffer back into the in-place conversion buffer.
     */
    HDmemcpy (_buf, _bkg, nelmts*dst->size);
    ret_value = SUCCEED;

    FUNC_LEAVE (ret_value);
}

