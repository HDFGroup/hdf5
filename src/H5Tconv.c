/*
 * Copyright (C) 1998 Spizella Software
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <robb@arborea.spizella.com>
 *              Tuesday, January 13, 1998
 *
 * Purpose:     Data type conversions.
 */
#define H5T_PACKAGE             /*suppress error about including H5Tpkg      */

#include <H5Iprivate.h>
#include <H5Eprivate.h>
#include <H5MMprivate.h>
#include <H5Tpkg.h>
#include <math.h>		/*for ceil()				     */

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
H5T_conv_noop(hid_t __unused__ src_id, hid_t __unused__ dst_id,
	      H5T_cdata_t *cdata, size_t __unused__ nelmts,
              void __unused__ *buf, void __unused__ *background)
{
    FUNC_ENTER(H5T_conv_noop, FAIL);

    switch (cdata->command) {
    case H5T_CONV_INIT:
	/* Nothing to initialize */
	break;

    case H5T_CONV_CONV:
	/* Nothing to convert */
	break;

    case H5T_CONV_FREE:
	/* Nothing to free */
	cdata->stats = H5MM_xfree (cdata->stats);
	break;
	
    default:
	HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
		       "unknown conversion command");
    }

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
               void *_buf, void __unused__ *background)
{
    uint8       *buf = (uint8 *) _buf;
    uint8       tmp;
    H5T_t       *src = NULL;
    H5T_t       *dst = NULL;
    size_t	i, j, md;

    FUNC_ENTER(H5T_conv_order, FAIL);

    switch (cdata->command) {
    case H5T_CONV_INIT:
        /* Capability query */
	if (H5_DATATYPE != H5I_group(src_id) ||
	    NULL == (src = H5I_object(src_id)) ||
	    H5_DATATYPE != H5I_group(dst_id) ||
	    NULL == (dst = H5I_object(dst_id))) {
	    HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
	}
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
	break;

    case H5T_CONV_CONV:
	/* The conversion */
	if (H5_DATATYPE != H5I_group(src_id) ||
	    NULL == (src = H5I_object(src_id)) ||
	    H5_DATATYPE != H5I_group(dst_id) ||
	    NULL == (dst = H5I_object(dst_id))) {
	    HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
	}
	md = src->size / 2;
	for (i=0; i<nelmts; i++, buf+=src->size) {
	    for (j=0; j<md; j++) {
		tmp = buf[j];
		buf[j] = buf[src->size-(j+1)];
		buf[src->size-(j+1)] = tmp;
	    }
	}
	break;

    case H5T_CONV_FREE:
	/* Free private data */
	break;

    default:
	HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
		       "unknown conversion command");
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
		type = H5T_copy (src->u.compnd.memb[i].type, H5T_COPY_ALL);
		tid = H5I_register (H5_DATATYPE, type);
		assert (tid>=0);
		priv->src_memb_id[priv->src2dst[i]] = tid;

		type = H5T_copy (dst->u.compnd.memb[priv->src2dst[i]].type,
				 H5T_COPY_ALL);
		tid = H5I_register (H5_DATATYPE, type);
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

    cdata->need_bkg = H5T_BKG_TEMP;
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
    uintn	elmtno;
    intn	i;			/*counters			*/
    H5T_conv_struct_t *priv = (H5T_conv_struct_t *)(cdata->priv);

    FUNC_ENTER (H5T_conv_struct, FAIL);

    switch (cdata->command) {
    case H5T_CONV_INIT:
	/*
	 * First, determine if this conversion function applies to the
	 * conversion path SRC_ID-->DST_ID.  If not, return failure;
	 * otherwise initialize the `priv' field of `cdata' with information
	 * that remains (almost) constant for this conversion path.
	 */
	if (H5_DATATYPE != H5I_group(src_id) ||
	    NULL == (src = H5I_object(src_id)) ||
	    H5_DATATYPE != H5I_group(dst_id) ||
	    NULL == (dst = H5I_object(dst_id))) {
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
	break;

    case H5T_CONV_FREE:
	/*
	 * Free the private conversion data.
	 */
	H5MM_xfree (priv->src2dst);
	H5MM_xfree (priv->src_memb_id);
	H5MM_xfree (priv->dst_memb_id);
	H5MM_xfree (priv->memb_conv);
	H5MM_xfree (priv->memb_cdata);
	cdata->priv = priv = H5MM_xfree (priv);
	break;

    case H5T_CONV_CONV:
	/*
	 * Conversion.
	 */
	if (H5_DATATYPE != H5I_group(src_id) ||
	    NULL == (src = H5I_object(src_id)) ||
	    H5_DATATYPE != H5I_group(dst_id) ||
	    NULL == (dst = H5I_object(dst_id))) {
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
	     * For each source member which will be present in the
	     * destination, convert the member to the destination type unless
	     * it is larger than the source type.  Then move the member to the
	     * left-most unoccupied position in the buffer.  This makes the
	     * data point as small as possible with all the free space on the
	     * right side.
	     */
	    for (i=0, offset=0; i<src->u.compnd.nmembs; i++) {
		if (src2dst[i]<0) continue;
		src_memb = src->u.compnd.memb + i;
		dst_memb = dst->u.compnd.memb + src2dst[i];

		if (H5T_get_size (dst_memb->type) <=
		    H5T_get_size (src_memb->type)) {
		    H5T_conv_t tconv_func = priv->memb_conv[src2dst[i]];
		    H5T_cdata_t *memb_cdata = priv->memb_cdata[src2dst[i]];
		    memb_cdata->command = H5T_CONV_CONV;
		    (tconv_func)(priv->src_memb_id[src2dst[i]],
				 priv->dst_memb_id[src2dst[i]],
				 memb_cdata,
				 1,
				 buf + src_memb->offset,
				 bkg + dst_memb->offset);

		    HDmemmove (buf + offset, buf + src_memb->offset,
			       H5T_get_size (dst_memb->type));
		    offset += H5T_get_size (dst_memb->type);
		} else {
		    HDmemmove (buf + offset, buf + src_memb->offset,
			       H5T_get_size (src_memb->type));
		    offset += H5T_get_size (src_memb->type);
		}
	    }

	    /*
	     * For each source member which will be present in the
	     * destination, convert the member to the destination type if it
	     * is larger than the source type (that is, has not been converted
	     * yet).  Then copy the member to the destination offset in the
	     * background buffer.
	     */
	    for (i=src->u.compnd.nmembs-1; i>=0; --i) {
		if (src2dst[i]<0) continue;
		src_memb = src->u.compnd.memb + i;
		dst_memb = dst->u.compnd.memb + src2dst[i];
		offset -= H5T_get_size (dst_memb->type);

		if (H5T_get_size (dst_memb->type) >
		    H5T_get_size (src_memb->type)) {
		    H5T_conv_t tconv_func = priv->memb_conv[src2dst[i]];
		    H5T_cdata_t *memb_cdata = priv->memb_cdata[src2dst[i]];
		    memb_cdata->command = H5T_CONV_CONV;
		    (tconv_func)(priv->src_memb_id[src2dst[i]],
				 priv->dst_memb_id[src2dst[i]], memb_cdata, 1,
				 buf + offset, bkg + dst_memb->offset);
		}
		HDmemmove (bkg+dst_memb->offset, buf+offset,
			   H5T_get_size (dst_memb->type));
	    }
	    assert (0==offset);

	    /*
	     * Update buf and background.
	     */
	    buf += src_delta;
	    bkg += dst_delta;
	}

	/*
	 * Copy the background buffer back into the in-place conversion
	 * buffer.
	 */
	HDmemcpy (_buf, _bkg, nelmts*dst->size);
	break;

    default:
	/* Some other command we don't know about yet.*/
	HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
		       "unknown conversion command");
    }
    
    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_i_i
 *
 * Purpose:	Convert one integer type to another.  This is the catch-all
 *		function for integer conversions and is probably not
 *		particularly fast.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, June 10, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_i_i (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
	      size_t nelmts, void *buf, void __unused__ *bkg)
{
    H5T_t	*src = NULL;		/*source data type		*/
    H5T_t	*dst = NULL;		/*destination data type		*/
    intn	direction;		/*direction of traversal	*/
    size_t	elmtno;			/*element number		*/
    size_t	half_size;		/*half the type size		*/
    size_t	olap;			/*num overlapping elements	*/
    uint8	*s, *sp, *d, *dp;	/*source and dest traversal ptrs*/
    uint8	dbuf[64];		/*temp destination buffer	*/
    size_t	first;
    ssize_t	sfirst;			/*a signed version of `first'	*/
    size_t	i;
    
    FUNC_ENTER (H5T_conv_i_i, FAIL);

    switch (cdata->command) {
    case H5T_CONV_INIT:
	if (H5_DATATYPE!=H5I_group (src_id) ||
	    NULL==(src=H5I_object (src_id)) ||
	    H5_DATATYPE!=H5I_group (dst_id) ||
	    NULL==(dst=H5I_object (dst_id))) {
	    HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
	}
	if (H5T_ORDER_LE!=src->u.atomic.order &&
	    H5T_ORDER_BE!=src->u.atomic.order) {
	    HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			   "unsupported byte order");
	}
	if (H5T_ORDER_LE!=dst->u.atomic.order &&
	    H5T_ORDER_BE!=dst->u.atomic.order) {
	    HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			   "unsupported byte order");
	}
	if (dst->size>sizeof dbuf) {
	    HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			   "destination size is too large");
	}
	break;
	
    case H5T_CONV_FREE:
	break;

    case H5T_CONV_CONV:
	/* Get the data types */
	if (H5_DATATYPE!=H5I_group (src_id) ||
	    NULL==(src=H5I_object (src_id)) ||
	    H5_DATATYPE!=H5I_group (dst_id) ||
	    NULL==(dst=H5I_object (dst_id))) {
	    HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
	}

	/*
	 * Do we process the values from beginning to end or vice versa? Also,
	 * how many of the elements have the source and destination areas
	 * overlapping?
	 */
	if (src->size==dst->size) {
	    sp = dp = (uint8*)buf;
	    direction = 1;
	    olap = nelmts;
	} else if (src->size>=dst->size) {
	    sp = dp = (uint8*)buf;
	    direction = 1;
	    olap = ceil((double)(src->size)/(src->size-dst->size))-1;
	} else {
	    sp = (uint8*)buf + (nelmts-1) * src->size;
	    dp = (uint8*)buf + (nelmts-1) * dst->size;
	    direction = -1;
	    olap = ceil((double)(dst->size)/(dst->size-src->size))-1;
	}

	/* The conversion loop */
	for (elmtno=0; elmtno<nelmts; elmtno++) {

	    /*
	     * If the source and destination buffers overlap then use a
	     * temporary buffer for the destination.
	     */
	    if (direction>0) {
		s = sp;
		d = elmtno<olap ? dbuf : dp;
	    } else {
		s = sp;
		d = elmtno >= nelmts-olap ? dbuf : dp;
	    }
#ifndef NDEBUG
	    /* I don't quite trust the overlap calculations yet --rpm */
	    if (d==dbuf) {
		assert ((dp>=sp && dp<sp+src->size) ||
			(sp>=dp && sp<dp+dst->size));
	    } else {
		assert ((dp<sp && dp+dst->size<=sp) ||
			(sp<dp && sp+src->size<=dp));
	    }
#endif
	    
	    /*
	     * Put the data in little endian order so our loops aren't so
	     * complicated.  We'll do all the conversion stuff assuming
	     * little endian and then we'll fix the order at the end.
	     */
	    if (H5T_ORDER_BE==src->u.atomic.order) {
		half_size = src->size/2;
		for (i=0; i<half_size; i++) {
		    uint8 tmp = s[src->size-(i+1)];
		    s[src->size-(i+1)] = s[i];
		    s[i] = tmp;
		}
	    }

	    /*
	     * What is the bit number for the msb bit of S which is set? The
	     * bit number is relative to the significant part of the number.
	     */
	    sfirst = H5T_bit_find (s, src->u.atomic.offset, src->u.atomic.prec,
				   H5T_BIT_MSB, TRUE);
	    first = (size_t)sfirst;

	    if (sfirst<0) {
		/*
		 * The source has no bits set and must therefore be zero.
		 * Set the destination to zero.
		 */
		H5T_bit_set (d, dst->u.atomic.offset, dst->u.atomic.prec,
			     FALSE);
		
	    } else if (H5T_SGN_NONE==src->u.atomic.u.i.sign &&
		       H5T_SGN_NONE==dst->u.atomic.u.i.sign) {
		/*
		 * Source and destination are both unsigned, but if the
		 * source has more precision bits than the destination then
		 * it's possible to overflow.  When overflow occurs the
		 * destination will be set to the maximum possible value.
		 */
		if (src->u.atomic.prec <= dst->u.atomic.prec) {
		    H5T_bit_copy (d, dst->u.atomic.offset,
				  s, src->u.atomic.offset,
				  src->u.atomic.prec);
		    H5T_bit_set (d, dst->u.atomic.offset+src->u.atomic.prec,
				 dst->u.atomic.prec-src->u.atomic.prec, FALSE);
		} else if (first>=dst->u.atomic.prec) {
		    /*overflow*/
		    H5T_bit_set (d, dst->u.atomic.offset, dst->u.atomic.prec,
				 TRUE);
		} else {
		    H5T_bit_copy (d, dst->u.atomic.offset,
				  s, src->u.atomic.offset,
				  dst->u.atomic.prec);
		}
		
	    } else if (H5T_SGN_2==src->u.atomic.u.i.sign &&
		       H5T_SGN_NONE==dst->u.atomic.u.i.sign) {
		/*
		 * If the source is signed and the destination isn't then we
		 * can have overflow if the source contains more bits than
		 * the destination (destination is set to the maximum
		 * possible value) or underflow if the source is negative
		 * (destination is set to zero).
		 */
		if (first+1 == src->u.atomic.prec) {
		    /*underflow*/
		    H5T_bit_set (d, dst->u.atomic.offset, dst->u.atomic.prec,
				 FALSE);
		} else if (src->u.atomic.prec < dst->u.atomic.prec) {
		    H5T_bit_copy (d, dst->u.atomic.offset,
				  s, src->u.atomic.offset,
				  src->u.atomic.prec-1);
		    H5T_bit_set (d, dst->u.atomic.offset+src->u.atomic.prec-1,
				 (dst->u.atomic.prec-src->u.atomic.prec)+1,
				 FALSE);
		} else if (first>=dst->u.atomic.prec) {
		    /*overflow*/
		    H5T_bit_set (d, dst->u.atomic.offset, dst->u.atomic.prec,
				 TRUE);
		} else {
		    H5T_bit_copy (d, dst->u.atomic.offset,
				  s, src->u.atomic.offset,
				  dst->u.atomic.prec);
		}
		
	    } else if (H5T_SGN_NONE==src->u.atomic.u.i.sign &&
		       H5T_SGN_2==dst->u.atomic.u.i.sign) {
		/*
		 * If the source is not signed but the destination is then
		 * overflow can occur in which case the destination is set to
		 * the largest possible value (all bits set except the msb).
		 */
		if (first+1 >= dst->u.atomic.prec) {
		    /*overflow*/
		    H5T_bit_set (d, dst->u.atomic.offset,
				 dst->u.atomic.prec-1, TRUE);
		    H5T_bit_set (d, dst->u.atomic.offset+dst->u.atomic.prec-1,
				 1, FALSE);
		} else if (src->u.atomic.prec<dst->u.atomic.prec) {
		    H5T_bit_copy (d, dst->u.atomic.offset,
				  s, src->u.atomic.offset,
				  src->u.atomic.prec);
		    H5T_bit_set (d, dst->u.atomic.offset+src->u.atomic.prec,
				 dst->u.atomic.prec-src->u.atomic.prec, FALSE);
		} else {
		    H5T_bit_copy (d, dst->u.atomic.offset,
				  s, src->u.atomic.offset,
				  dst->u.atomic.prec);
		}
		
	    } else if (first+1 == src->u.atomic.prec) {
		/*
		 * Both the source and the destination are signed and the
		 * source value is negative.  We could experience underflow
		 * if the destination isn't wide enough in which case the
		 * destination is set to a negative number with the largest
		 * possible magnitude.
		 */
		ssize_t sfz = H5T_bit_find (s, src->u.atomic.offset,
					    src->u.atomic.prec-1, H5T_BIT_MSB,
					    FALSE);
		size_t fz = (size_t)sfz;
		
		if (sfz>=0 && fz+1>=dst->u.atomic.prec) {
		    /*underflow*/
		    H5T_bit_set (d, dst->u.atomic.offset, dst->u.atomic.prec-1,
				 FALSE);
		    H5T_bit_set (d, dst->u.atomic.offset+dst->u.atomic.prec-1,
				 1, TRUE);
		} else if (src->u.atomic.prec<dst->u.atomic.prec) {
		    H5T_bit_copy (d, dst->u.atomic.offset,
				  s, src->u.atomic.offset,
				  src->u.atomic.prec);
		    H5T_bit_set (d, dst->u.atomic.offset+src->u.atomic.prec,
				 dst->u.atomic.prec-src->u.atomic.prec, TRUE);
		} else {
		    H5T_bit_copy (d, dst->u.atomic.offset,
				  s, src->u.atomic.offset,
				  dst->u.atomic.prec);
		}
		
	    } else {
		/*
		 * Source and destination are both signed but the source
		 * value is positive.  We could have an overflow in which
		 * case the destination is set to the largest possible
		 * positive value.
		 */
		if (first+1>=dst->u.atomic.prec) {
		    /*overflow*/
		    H5T_bit_set (d, dst->u.atomic.offset, dst->u.atomic.prec-1,
				 TRUE);
		    H5T_bit_set (d, dst->u.atomic.offset+dst->u.atomic.prec-1,
				 1, FALSE);
		} else if (src->u.atomic.prec<dst->u.atomic.prec) {
		    H5T_bit_copy (d, dst->u.atomic.offset,
				  s, src->u.atomic.offset,
				  src->u.atomic.prec);
		    H5T_bit_set (d, dst->u.atomic.offset+src->u.atomic.prec,
				 dst->u.atomic.prec-src->u.atomic.prec, FALSE);
		} else {
		    H5T_bit_copy (d, dst->u.atomic.offset,
				  s, src->u.atomic.offset,
				  dst->u.atomic.prec);
		}
	    }

	    /*
	     * Set padding areas in destination.
	     */
	    if (dst->u.atomic.offset>0) {
		assert (H5T_PAD_ZERO==dst->u.atomic.lsb_pad ||
			H5T_PAD_ONE==dst->u.atomic.lsb_pad);
		H5T_bit_set (d, 0, dst->u.atomic.offset,
			     H5T_PAD_ONE==dst->u.atomic.lsb_pad);
	    }
	    if (dst->u.atomic.offset+dst->u.atomic.prec!=8*dst->size) {
		assert (H5T_PAD_ZERO==dst->u.atomic.msb_pad ||
			H5T_PAD_ONE==dst->u.atomic.msb_pad);
		H5T_bit_set (d, dst->u.atomic.offset+dst->u.atomic.prec,
			     8*dst->size - (dst->u.atomic.offset+
					    dst->u.atomic.prec),
			     H5T_PAD_ONE==dst->u.atomic.msb_pad);
	    }

	    /*
	     * Put the destination in the correct byte order.  See note at
	     * beginning of loop.
	     */
	    if (H5T_ORDER_BE==dst->u.atomic.order) {
		half_size = dst->size/2;
		for (i=0; i<half_size; i++) {
		    uint8 tmp = d[dst->size-(i+1)];
		    d[dst->size-(i+1)] = d[i];
		    d[i] = tmp;
		}
	    }

	    /*
	     * If we had used a temporary buffer for the destination then we
	     * should copy the value to the true destination buffer.
	     */
	    if (d==dbuf) HDmemcpy (dp, d, dst->size);
		
	next:
	    sp += direction * src->size;
	    dp += direction * dst->size;
	}
	
	break;

    default:
	HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
		       "unknown conversion command");
    }

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_i32le_r64le
 *
 * Purpose:	Converts 4-byte little-endian integers (signed or unsigned)
 *		to 8-byte litte-endian IEEE floating point.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, June 10, 1998
 *
 * Modifications:
 * 
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_i32le_r64le (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, void *buf, void __unused__ *bkg)
{
    uint8	*s=NULL, *d=NULL;	/*src and dst buf pointers	*/
    uint8	tmp[8];			/*temporary destination buffer	*/
    H5T_t	*src = NULL;		/*source data type		*/
    H5T_t	*dst = NULL;		/*destination data type		*/
    size_t	elmtno;			/*element counter		*/
    uintn	sign;			/*sign bit			*/
    uintn	cin, cout;		/*carry in/out			*/
    uintn	mbits=0;		/*mantissa bits			*/
    uintn	exponent;		/*exponent			*/
    intn	i;			/*counter			*/

    FUNC_ENTER (H5T_conv_i32le_r64le, FAIL);

    switch (cdata->command) {
    case H5T_CONV_INIT:
	assert (sizeof(intn)>=4);
	break;

    case H5T_CONV_FREE:
	/* Free private data */
	break;

    case H5T_CONV_CONV:
	/* The conversion */
	if (H5_DATATYPE!=H5I_group (src_id) ||
	    NULL==(src=H5I_object (src_id)) ||
	    H5_DATATYPE!=H5I_group (dst_id) ||
	    NULL==(dst=H5I_object (dst_id))) {
	    HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
	}
	
	s = (uint8*)buf + 4*(nelmts-1);
	d = (uint8*)buf + 8*(nelmts-1);
	for (elmtno=0; elmtno<nelmts; elmtno++, s-=4, d-=8) {

	    /*
	     * If this is the last element to convert (that is, the first
	     * element of the buffer) then the source and destination areas
	     * overlap so we need to use a temp buf for the destination.
	     */
	    if (s==buf) d = tmp;

	    /* Convert the integer to a sign and magnitude */
	    switch (src->u.atomic.u.i.sign) {
	    case H5T_SGN_NONE:
		sign = 0;
		break;
	    case H5T_SGN_2:
		if (s[3] & 0x80) {
		    sign = 1 ;
		    for (i=0,cin=1; i<4; i++,cin=cout) {
			s[i] = ~s[i] ;
			cout = ((unsigned)(s[i])+cin > 0xff) ? 1 : 0 ;
			s[i] += cin ;
		    }
		} else {
		    sign = 0;
		}
		break;
	    default:
		HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			       "unsupported integer sign method");
	    }
	    
	    /*
	     * Where is the most significant bit that is set?  We could do
	     * this in a loop, but testing it this way might be faster.
	     */
	    if (s[3]) {
		if (s[3] & 0x80) mbits = 32 ;
		else if (s[3] & 0x40) mbits = 31 ;
		else if (s[3] & 0x20) mbits = 30 ;
		else if (s[3] & 0x10) mbits = 29 ;
		else if (s[3] & 0x08) mbits = 28 ;
		else if (s[3] & 0x04) mbits = 27 ;
		else if (s[3] & 0x02) mbits = 26 ;
		else if (s[3] & 0x01) mbits = 25 ;
	    } else if (s[2]) {
		if (s[2] & 0x80) mbits = 24 ;
		else if (s[2] & 0x40) mbits = 23 ;
		else if (s[2] & 0x20) mbits = 22 ;
		else if (s[2] & 0x10) mbits = 21 ;
		else if (s[2] & 0x08) mbits = 20 ;
		else if (s[2] & 0x04) mbits = 19 ;
		else if (s[2] & 0x02) mbits = 18 ;
		else if (s[2] & 0x01) mbits = 17 ;
	    } else if (s[1]) {
		if (s[1] & 0x80) mbits = 16 ;
		else if (s[1] & 0x40) mbits = 15 ;
		else if (s[1] & 0x20) mbits = 14 ;
		else if (s[1] & 0x10) mbits = 13 ;
		else if (s[1] & 0x08) mbits = 12 ;
		else if (s[1] & 0x04) mbits = 11 ;
		else if (s[1] & 0x02) mbits = 10 ;
		else if (s[1] & 0x01) mbits =  9 ;
	    } else if (s[0]) {
		if (s[0] & 0x80) mbits = 8 ;
		else if (s[0] & 0x40) mbits =  7 ;
		else if (s[0] & 0x20) mbits =  6 ;
		else if (s[0] & 0x10) mbits =  5 ;
		else if (s[0] & 0x08) mbits =  4 ;
		else if (s[0] & 0x04) mbits =  3 ;
		else if (s[0] & 0x02) mbits =  2 ;
		else if (s[0] & 0x01) mbits =  1 ;
	    } else {
		/*zero*/
		d[7] = d[6] = d[5] = d[4] = d[3] = d[2] = d[1] = d[0] = 0 ;
		continue ;
	    }

	    /*
	     * The sign and exponent.
	     */
	    exponent = (mbits - 1) + 1023 ;
	    d[7] = (sign<<7) | ((exponent>>4) & 0x7f) ;
	    d[6] = (exponent & 0x0f) << 4 ;
      
	    /*
	     * The mantissa.
	     */
	    switch (mbits) {
	    case 32:
		d[5] = d[4] = d[3] = d[1] = d[0] = 0 ;
		break ;
	    case 31:
		d[6] |=  0x0f    & (s[3]>>2) ;
		d[5] = (s[3]<<6) | (s[2]>>2) ;
		d[4] = (s[2]<<6) | (s[1]>>2) ;
		d[3] = (s[1]<<6) | (s[0]>>2) ;
		d[2] = (s[0]<<6) ;
		d[1] = d[0] = 0 ;
		break ;
	    case 30:
		d[6] |=  0x0f    & (s[3]>>1) ;
		d[5] = (s[3]<<7) | (s[2]>>1) ;
		d[4] = (s[2]<<7) | (s[1]>>1) ;
		d[3] = (s[1]<<7) | (s[0]>>1) ;
		d[2] = (s[0]<<7) ;
		d[1] = d[0] = 0 ;
		break ;
	    case 29:
		d[6] |=  0x0f    & s[3] ;
		d[5] = s[2] ;
		d[4] = s[1] ;
		d[3] = s[0] ;
		d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 28:
		d[6] |= ((s[3]<<1) | (s[2]>>7)) & 0x0f ;
		d[5] =   (s[2]<<1) | (s[1]>>7) ;
		d[4] =   (s[1]<<1) | (s[0]>>7) ;
		d[3] =   (s[0]<<1) ;
		d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 27:
		d[6] |= ((s[3]<<2) | (s[2]>>6)) & 0x0f ;
		d[5] =   (s[2]<<2) | (s[1]>>6) ;
		d[4] =   (s[1]<<2) | (s[0]>>6) ;
		d[3] =   (s[0]<<2) ;
		d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 26:
		d[6] |= ((s[3]<<3) | (s[2]>>5)) & 0x0f ;
		d[5] =   (s[2]<<3) | (s[1]>>5) ;
		d[4] =   (s[1]<<3) | (s[0]>>5) ;
		d[3] =   (s[0]<<3) ;
		d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 25:
		d[6] |=   0x0f   & (s[2]>>4) ;
		d[5] = (s[2]<<4) | (s[1]>>4) ;
		d[4] = (s[1]<<4) | (s[0]>>4) ;
		d[3] = (s[0]<<4) ;
		d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 24:
		d[6] |=   0x0f   & (s[2]>>3) ;
		d[5] = (s[2]<<5) | (s[1]>>3) ;
		d[4] = (s[1]<<5) | (s[0]>>3) ;
		d[3] = (s[0]<<5) ;
		d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 23:
		d[6] |=   0x0f   & (s[2]>>2) ;
		d[5] = (s[2]<<6) | (s[1]>>2) ;
		d[4] = (s[1]<<6) | (s[0]>>2) ;
		d[3] = (s[0]<<6) ;
		d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 22:
		d[6] |=   0x0f   & (s[2]>>1) ;
		d[5] = (s[2]<<7) | (s[1]>>1) ;
		d[4] = (s[1]<<7) | (s[0]>>1) ;
		d[3] = (s[0]<<7) ;
		d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 21:
		d[6] |= 0x0f & s[2] ;
		d[5] = s[1] ;
		d[4] = s[0] ;
		d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 20:
		d[6] |= ((s[2]<<1) | (s[1]>>7)) & 0x0f ;
		d[5] =   (s[1]<<1) | (s[0]>>7) ;
		d[4] =   (s[0]<<1) ;
		d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 19:
		d[6] |= ((s[2]<<2) | (s[1]>>6)) & 0x0f ;
		d[5] =   (s[1]<<2) | (s[0]>>6) ;
		d[4] =   (s[0]<<2) ;
		d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 18:
		d[6] |= ((s[2]<<3) | (s[1]>>5)) & 0x0f ;
		d[5] =   (s[1]<<3) | (s[0]>>5) ;
		d[4] =   (s[0]<<3) ;
		d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 17:
		d[6] |=   0x0f   & (s[1]>>4) ;
		d[5] = (s[1]<<4) | (s[0]>>4) ;
		d[4] = (s[0]<<4) ;
		d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 16:
		d[6] |=   0x0f   & (s[1]>>3) ;
		d[5] = (s[1]<<5) | (s[0]>>3) ;
		d[4] = (s[0]<<5) ;
		d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 15:
		d[6] |=   0x0f   & (s[1]>>2) ;
		d[5] = (s[1]<<6) | (s[0]>>2) ;
		d[4] = (s[0]<<6) ;
		d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 14:
		d[6] |=   0x0f   & (s[1]>>1) ;
		d[5] = (s[1]<<7) | (s[0]>>1) ;
		d[4] = (s[0]<<7) ;
		d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 13:
		d[6] |= 0x0f & s[1] ;
		d[5] = s[0] ;
		d[4] = d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 12:
		d[6] |= ((s[1]<<1) | (s[0]>>7)) & 0x0f ;
		d[5] =   (s[0]<<1) ;
		d[4] = d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 11:
		d[6] |= ((s[1]<<2) | (s[0]>>6)) & 0x0f ;
		d[5] =   (s[0]<<2) ;
		d[4] = d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 10:
		d[6] |= ((s[1]<<3) | (s[0]>>5)) & 0x0f ;
		d[5] =   (s[0]<<3) ;
		d[4] = d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 9:
		d[6] |=   0x0f   & (s[0]>>4) ;
		d[5] = (s[0]<<4) ;
		d[4] = d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 8:
		d[6] |=   0x0f   & (s[0]>>3) ;
		d[5] = (s[0]<<5) ;
		d[4] = d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 7:
		d[6] |=   0x0f   & (s[0]>>2) ;
		d[5] = (s[0]<<6) ;
		d[4] = d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 6:
		d[6] |=   0x0f   & (s[0]>>1) ;
		d[5] = (s[0]<<7) ;
		d[4] = d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 5:
		d[6] |= 0x0f & s[0] ;
		d[5] = d[4] = d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 4:
		d[6] |= (s[0]<<1) & 0x0f ;
		d[5] = d[4] = d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 3:
		d[6] |= (s[0]<<2) & 0x0f ;
		d[5] = d[4] = d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 2:
		d[6] |= (s[0]<<3) & 0x0f ;
		d[5] = d[4] = d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 1:
		d[5] = d[4] = d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    }

	    /*
	     * Copy temp buffer to the destination.  This only happens for
	     * the first value in the array, the last value processed. See
	     * beginning of loop.
	     */
	    if (d==tmp) HDmemcpy (s, d, 8);
	}
	break;

    default:
	/* Some other command we don't know about yet.*/
	HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
		       "unknown conversion command");
    }
    
    FUNC_LEAVE (SUCCEED);
}
