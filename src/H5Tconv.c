/*
 * Copyright (C) 1998 Spizella Software
 *		      All rights reserved.
 *
 * Programmer:	Robb Matzke <robb@arborea.spizella.com>
 *		Tuesday, January 13, 1998
 *
 * Purpose:	Data type conversions.
 */
#define H5T_PACKAGE		/*suppress error about including H5Tpkg	     */

#define PABLO_MASK    H5T_conv_mask
#include <H5Iprivate.h>
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
    size_t	*memb_nelmts;		/*member element count		     */
} H5T_conv_struct_t;

/* Interface initialization */
static intn interface_initialize_g = 0;
#define INTERFACE_INIT NULL

/*
 * These macros are for the bodies of functions that convert buffers of one
 * integer type to another using hardware.  They all start with `H5T_CONV_'
 * and end with two letters that represent the source and destination types,
 * respectively. The letters `s' and `S' refer to signed values while the
 * letters `u' and `U' refer to unsigned values. The letter which is
 * capitalized indicates that the corresponding type (source or destination)
 * is at least as large as the other type.  Certain conversions may
 * experience overflow conditions which arise when the source value has a
 * magnitude that cannot be represented by the destination type.
 *
 * Suffix	Description
 * ------	-----------
 * sS:		Signed integers to signed integers where the destination is
 *		at least as wide as the source.	 This case cannot generate
 *		overflows.
 *
 * sU:		Signed integers to unsigned integers where the destination is
 *		at least as wide as the source.	 This case experiences
 *		overflows when the source value is negative.
 *
 * uS:		Unsigned integers to signed integers where the destination is
 *		at least as wide as the source.	 This case can experience
 *		overflows when the source and destination are the same size.
 *
 * uU:		Unsigned integers to unsigned integers where the destination
 *		is at least as wide as the source.  Overflows are not
 *		possible in this case.
 *
 * Ss:		Signed integers to signed integers where the source is at
 *		least as large as the destination.  Overflows can occur when
 *		the destination is narrower than the source.
 *
 * Su:		Signed integers to unsigned integers where the source is at
 *		least as large as the destination.  Overflows occur when the
 *		source value is negative and can also occur if the
 *		destination is narrower than the source.
 *
 * Us:		Unsigned integers to signed integers where the source is at
 *		least as large as the destination.  Overflows can occur for
 *		all sizes.
 *
 * Uu:		Unsigned integers to unsigned integers where the source is at
 *		least as large as the destination. Overflows can occur if the
 *		destination is narrower than the source.
 *
 * su:		Conversion from signed integers to unsigned integers where
 *		the source and destination are the same size. Overflow occurs
 *		when the source value is negative.
 *
 * us:		Conversion from unsigned integers to signed integers where
 *		the source and destination are the same size.  Overflow
 *		occurs when the source magnitude is too large for the
 *		destination.
 *
 * The macros take a subset of these arguments in the order listed here:
 *
 * CDATA:	A pointer to the H5T_cdata_t structure that was passed to the
 *		conversion function.
 *
 * S_ID:	The hid_t value for the source data type.
 *
 * D_ID:	The hid_t value for the destination data type.
 *
 * BUF:		A pointer to the conversion buffer.
 *
 * NELMTS:	The number of values to be converted.
 *
 * ST:		The C name for source data type (e.g., int)
 *
 * DT:		The C name for the destination data type (e.g., signed char)
 *
 * D_MIN:	The minimum possible destination value.	 For unsigned
 *		destination types this should be zero.	For signed
 *		destination types it's a negative value with a magnitude that
 *		is usually one greater than D_MAX.  Source values which are
 *		smaller than D_MIN generate overflows.
 *
 * D_MAX:	The maximum possible destination value. Source values which
 *		are larger than D_MAX generate overflows.
 * 
 */
#define H5T_CONV_sS(S_ALIGN,D_ALIGN,ST,DT) {				      \
    assert(sizeof(ST)<=sizeof(DT));					      \
    CI_BEGIN(S_ALIGN, D_ALIGN, ST, DT, nelmts-1, --) {			      \
	*d = *s;							      \
    } CI_END;								      \
}

#define H5T_CONV_sU(STYPE,DTYPE,ST,DT) {				      \
    assert(sizeof(ST)<=sizeof(DT));					      \
    CI_BEGIN(STYPE, DTYPE, ST, DT, nelmts-1, --) {			      \
	if (*s<0) {							      \
	    if (!H5T_overflow_g ||					      \
		(H5T_overflow_g)(src_id, dst_id, s, d)<0) {		      \
		*d = 0;							      \
	    }								      \
	} else {							      \
	    *d = *s;							      \
	}								      \
    } CI_END;								      \
}

#define H5T_CONV_uS(STYPE,DTYPE,ST,DT,D_MAX) {				      \
    assert(sizeof(ST)<=sizeof(DT));					      \
    CI_BEGIN(STYPE, DTYPE, ST, DT, nelmts-1, --) {			      \
	if (*s > (D_MAX)) {						      \
	    if (!H5T_overflow_g ||					      \
		(H5T_overflow_g)(src_id, dst_id, s, d)<0) {		      \
		*d = (D_MAX);						      \
	    }								      \
	} else {							      \
	    *d = *s;							      \
	}								      \
    } CI_END;								      \
}

#define H5T_CONV_uU(STYPE,DTYPE,ST,DT) {				      \
    assert(sizeof(ST)<=sizeof(DT));					      \
    CI_BEGIN(STYPE, DTYPE, ST, DT, nelmts-1, --) {			      \
	*d = *s;							      \
    } CI_END;								      \
}

#define H5T_CONV_Ss(STYPE,DTYPE,ST,DT,D_MIN,D_MAX) {			      \
    assert(sizeof(ST)>=sizeof(DT));					      \
    CI_BEGIN(STYPE, DTYPE, ST, DT, 0, ++) {				      \
	if (*s > (DT)(D_MAX)) {						      \
	    if (!H5T_overflow_g ||					      \
		(H5T_overflow_g)(src_id, dst_id, s, d)<0) {		      \
		*d = (D_MAX);						      \
	    }								      \
	} else if (*s < (D_MIN)) {					      \
	    if (!H5T_overflow_g ||					      \
		(H5T_overflow_g)(src_id, dst_id, s, d)<0) {		      \
		*d = (D_MIN);						      \
	    }								      \
	} else {							      \
	    *d = *s;							      \
	}								      \
    } CI_END;								      \
}

#define H5T_CONV_Su(STYPE,DTYPE,ST,DT,D_MAX) {				      \
    assert(sizeof(ST)>=sizeof(DT));					      \
    CI_BEGIN(STYPE, DTYPE, ST, DT, 0, ++) {				      \
	if (*s < 0) {							      \
	    if (!H5T_overflow_g ||					      \
		(H5T_overflow_g)(src_id, dst_id, s, d)<0) {		      \
		*d = 0;							      \
	    }								      \
	} else if (sizeof(ST)>sizeof(DT) && *s>(D_MAX)) {		      \
	    /*sign vs. unsign ok in previous line*/			      \
	    if (!H5T_overflow_g ||					      \
		(H5T_overflow_g)(src_id, dst_id, s, d)<0) {		      \
		*d = (D_MAX);						      \
	    }								      \
	} else {							      \
	    *d = *s;							      \
	}								      \
    } CI_END;								      \
}

#define H5T_CONV_Us(STYPE,DTYPE,ST,DT,D_MAX) {				      \
    assert(sizeof(ST)>=sizeof(DT));					      \
    CI_BEGIN(STYPE, DTYPE, ST, DT, 0, ++) {				      \
	if (*s > (D_MAX)) {						      \
	    if (!H5T_overflow_g ||					      \
		(H5T_overflow_g)(src_id, dst_id, s, d)<0) {		      \
		*d = (D_MAX);						      \
	    }								      \
	} else {							      \
	    *d = *s;							      \
	}								      \
    } CI_END;								      \
}

#define H5T_CONV_Uu(STYPE,DTYPE,ST,DT,D_MAX) {				      \
    assert(sizeof(ST)>=sizeof(DT));					      \
    CI_BEGIN(STYPE, DTYPE, ST, DT, 0, ++) {				      \
	if (*s > (D_MAX)) {						      \
	    if (!H5T_overflow_g ||					      \
		(H5T_overflow_g)(src_id, dst_id, s, d)<0) {		      \
		*d = (D_MAX);						      \
	    }								      \
	} else {							      \
	    *d = *s;							      \
	}								      \
    } CI_END;								      \
}

#define H5T_CONV_su(STYPE,DTYPE,ST,DT) {				      \
    assert(sizeof(ST)==sizeof(DT));					      \
    CI_BEGIN(STYPE, DTYPE, ST, DT, 0, ++) {				      \
	if (*s < 0) {							      \
	    if (!H5T_overflow_g ||					      \
		(H5T_overflow_g)(src_id, dst_id, s, d)<0) {		      \
		*d = 0;							      \
	    }								      \
	} else {							      \
	    *d = *s;							      \
	}								      \
    } CI_END;								      \
}

#define H5T_CONV_us(STYPE,DTYPE,ST,DT,D_MAX) {				      \
    assert(sizeof(ST)==sizeof(DT));					      \
    CI_BEGIN(STYPE, DTYPE, ST, DT, 0, ++) {				      \
	if (*s > (D_MAX)) {						      \
	    if (!H5T_overflow_g ||					      \
		(H5T_overflow_g)(src_id, dst_id, s, d)<0) {		      \
		*d = (D_MAX);						      \
	    }								      \
	} else {							      \
	    *d = *s;							      \
	}								      \
    } CI_END;								      \
}

/* The first part of every integer hardware conversion macro */
#define CI_BEGIN(STYPE,DTYPE,ST,DT,STRT,DIR) {				      \
    size_t	elmtno;			/*element number		*/    \
    ST		*src, *s;		/*source buffer			*/    \
    DT		*dst, *d;		/*destination buffer		*/    \
    H5T_t	*st, *dt;		/*src and dest data types	*/    \
    long_long	aligned;		/*largest integer type, aligned	*/    \
    hbool_t	s_mv, d_mv;		/*move data to align it?	*/    \
									      \
    switch (cdata->command) {						      \
    case H5T_CONV_INIT:							      \
	cdata->need_bkg = H5T_BKG_NO;					      \
	break;								      \
    case H5T_CONV_FREE:							      \
	break;								      \
    case H5T_CONV_CONV:							      \
	src = (ST*)buf+(STRT);						      \
	dst = (DT*)buf+(STRT);						      \
	st = H5I_object(src_id);					      \
	dt = H5I_object(dst_id);					      \
	assert(st && dt);						      \
	s_mv = H5T_NATIVE_##STYPE##_ALIGN_g>1 &&			      \
               ((size_t)buf%H5T_NATIVE_##STYPE##_ALIGN_g ||		      \
		st->size%H5T_NATIVE_##STYPE##_ALIGN_g);			      \
	d_mv = H5T_NATIVE_##DTYPE##_ALIGN_g>1 &&			      \
               ((size_t)buf%H5T_NATIVE_##DTYPE##_ALIGN_g ||		      \
                dt->size%H5T_NATIVE_##DTYPE##_ALIGN_g);			      \
        CI_DEBUG(s_mv, STYPE, ST);					      \
        CI_DEBUG(d_mv, DTYPE, DT);					      \
	for (elmtno=0; elmtno<nelmts; elmtno++, DIR src,  DIR dst) {	      \
	    if (s_mv) {							      \
		memcpy(&aligned, src, st->size);			      \
		s = (ST*)&aligned;					      \
	    } else {							      \
		s = src;						      \
	    }								      \
	    if (d_mv) {							      \
		d = (DT*)&aligned;					      \
	    } else {							      \
		d = dst;						      \
	    }
	    /* ... user-defined stuff here ... */
#define CI_END								      \
            if (d_mv) memcpy(dst, &aligned, dt->size);			      \
        }								      \
        break;								      \
    default:								      \
	HRETURN_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,		      \
		      "unknown conversion command");			      \
    }									      \
}

/* Print alignment information */
#ifdef H5T_DEBUG
#   define CI_DEBUG(MV,HDF_TYPE,C_TYPE) {				      \
    if (MV && H5DEBUG(T)) {						      \
	fprintf(H5DEBUG(T), "<%d-byte alignment for %s>",		      \
		H5T_NATIVE_##HDF_TYPE##_ALIGN_g, #C_TYPE);		      \
    }									      \
}
#else
#   define CI_DEBUG(MV,HDF_TYPE,C_TYPE) /*void*/
#endif

/*-------------------------------------------------------------------------
 * Function:	H5T_conv_noop
 *
 * Purpose:	The no-op conversion.  The library knows about this
 *		conversion without it being registered.
 *
 * Return: Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January 14, 1998
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
	cdata->need_bkg = H5T_BKG_NO;
	break;

    case H5T_CONV_CONV:
	/* Nothing to convert */
	break;

    case H5T_CONV_FREE:
	cdata->stats = H5MM_xfree (cdata->stats);
	break;
	
    default:
	HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
		       "unknown conversion command");
    }

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:	H5T_conv_order
 *
 * Purpose:	Convert one type to another when byte order is the only
 *		difference.
 *
 * Note:	This is a soft conversion function.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Tuesday, January 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_order(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata, size_t nelmts,
	       void *_buf, void __unused__ *background)
{
    uint8_t	*buf = (uint8_t*)_buf;
    uint8_t	tmp;
    H5T_t	*src = NULL;
    H5T_t	*dst = NULL;
    size_t	i, j, md;

    FUNC_ENTER(H5T_conv_order, FAIL);

    switch (cdata->command) {
    case H5T_CONV_INIT:
	/* Capability query */
	if (H5I_DATATYPE != H5I_get_type(src_id) ||
	    NULL == (src = H5I_object(src_id)) ||
	    H5I_DATATYPE != H5I_get_type(dst_id) ||
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
	cdata->need_bkg = H5T_BKG_NO;
	break;

    case H5T_CONV_CONV:
	/* The conversion */
	if (H5I_DATATYPE != H5I_get_type(src_id) ||
	    NULL == (src = H5I_object(src_id)) ||
	    H5I_DATATYPE != H5I_get_type(dst_id) ||
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
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Monday, January 26, 1998
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
	 *	   source type that are also in the destination type.
	 */
	cdata->priv = priv = H5MM_calloc (sizeof(H5T_conv_struct_t));
	if (NULL==priv) {
	    HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,
			   "memory allocation failed");
	}
	priv->src2dst = H5MM_malloc (src->u.compnd.nmembs * sizeof(intn));
	priv->src_memb_id = H5MM_malloc (/*!*/dst->u.compnd.nmembs *
					 sizeof(hid_t));
	priv->dst_memb_id = H5MM_malloc (dst->u.compnd.nmembs *
					 sizeof(hid_t));
	if (NULL==priv->src2dst ||
	    NULL==priv->src_memb_id ||
	    NULL==priv->dst_memb_id) {
	    HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,
			   "memory allocation failed");
	}

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
		tid = H5I_register (H5I_DATATYPE, type);
		assert (tid>=0);
		priv->src_memb_id[priv->src2dst[i]] = tid;

		type = H5T_copy (dst->u.compnd.memb[priv->src2dst[i]].type,
				 H5T_COPY_ALL);
		tid = H5I_register (H5I_DATATYPE, type);
		assert (tid>=0);
		priv->dst_memb_id[priv->src2dst[i]] = tid;
	    }
	}

	/*
	 * Those members which are in both the source and destination must be
	 * the same size and shape arrays.
	 */
	for (i=0; i<src->u.compnd.nmembs; i++) {
	    if (priv->src2dst[i]>=0) {
		H5T_member_t *src_memb = src->u.compnd.memb + i;
		H5T_member_t *dst_memb = dst->u.compnd.memb + priv->src2dst[i];
		if (src_memb->ndims != dst_memb->ndims) {
		    HRETURN_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
				  "source and dest members have incompatible "
				  "size or shape");
		}
		for (j=0; j<src_memb->ndims; j++) {
		    if (src_memb->dim[j] != dst_memb->dim[j]) {
			HRETURN_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
				      "source and dest members have "
				      "incompatible size or shape");
		    }
#ifndef LATER
		    /* Their permutation vectors must be equal */
		    if (src_memb->perm[j]!=dst_memb->perm[j]) {
			HRETURN_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
				      "member permutations must be equal");
		    }
#endif
		}
	    }
	}

	/* Calculate number of elements of each member */
	priv->memb_nelmts = H5MM_malloc(src->u.compnd.nmembs*sizeof(size_t));
	for (i=0; i<src->u.compnd.nmembs; i++) {
	    priv->memb_nelmts[i] = 1;
	    for (j=0; j<src->u.compnd.memb[i].ndims; j++) {
		priv->memb_nelmts[i] *= src->u.compnd.memb[i].dim[j];
	    }
	}
    }

    /*
     * (Re)build the cache of member conversion functions and pointers to
     * their cdata entries.
     */
    priv->memb_conv = H5MM_xfree (priv->memb_conv);
    priv->memb_cdata = H5MM_xfree (priv->memb_cdata);
    priv->memb_conv = H5MM_malloc (dst->u.compnd.nmembs *
				   sizeof(H5T_conv_t));
    priv->memb_cdata = H5MM_calloc (dst->u.compnd.nmembs *
				     sizeof(H5T_cdata_t*));
    if (NULL==priv->memb_conv || NULL==priv->memb_cdata) {
	HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,
		       "memory allocation failed");
    }
    src2dst = priv->src2dst;

    for (i=0; i<src->u.compnd.nmembs; i++) {
	if (priv->src2dst[i]>=0) {
	    H5T_conv_t tconv_func;
	    tconv_func = H5T_find(src->u.compnd.memb[i].type,
				  dst->u.compnd.memb[src2dst[i]].type,
				  H5T_BKG_NO, priv->memb_cdata+src2dst[i]);
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
 *		For I=1..NUM_MEMBERS do
 *		  If sizeof detination type <= sizeof source type then
 *		    Convert member to destination type;
 *		  Move member as far left as possible;
 *		  
 *		For I=NUM_MEMBERS..1 do
 *		  If not destination type then
 *		    Convert member to destination type;
 *		  Move member to correct position in BACKGROUND
 *
 *		Copy BACKGROUND to BUF
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Thursday, January 22, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_struct(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata, size_t nelmts,
		void *_buf, void *_bkg)
{
    uint8_t	*buf = (uint8_t *)_buf;	/*cast for pointer arithmetic	*/
    uint8_t	*bkg = (uint8_t *)_bkg;	/*background pointer arithmetic	*/
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
	if (H5I_DATATYPE != H5I_get_type(src_id) ||
	    NULL == (src = H5I_object(src_id)) ||
	    H5I_DATATYPE != H5I_get_type(dst_id) ||
	    NULL == (dst = H5I_object(dst_id))) {
	    HRETURN_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
	}
	assert (H5T_COMPOUND==src->type);
	assert (H5T_COMPOUND==dst->type);

	if (H5T_conv_struct_init (src, dst, cdata)<0) {
	    HRETURN_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,
			   "unable to initialize conversion data");
	}
	break;

    case H5T_CONV_FREE:
	/*
	 * Free the private conversion data.
	 */
	H5MM_xfree(priv->src2dst);
	H5MM_xfree(priv->src_memb_id);
	H5MM_xfree(priv->dst_memb_id);
	H5MM_xfree(priv->memb_conv);
	H5MM_xfree(priv->memb_cdata);
	H5MM_xfree(priv->memb_nelmts);
	cdata->priv = priv = H5MM_xfree (priv);
	break;

    case H5T_CONV_CONV:
	/*
	 * Conversion.
	 */
	if (H5I_DATATYPE != H5I_get_type(src_id) ||
	    NULL == (src = H5I_object(src_id)) ||
	    H5I_DATATYPE != H5I_get_type(dst_id) ||
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

		if (dst_memb->size <= src_memb->size) {
		    H5T_conv_t tconv_func = priv->memb_conv[src2dst[i]];
		    H5T_cdata_t *memb_cdata = priv->memb_cdata[src2dst[i]];
		    memb_cdata->command = H5T_CONV_CONV;
		    (tconv_func)(priv->src_memb_id[src2dst[i]],
				 priv->dst_memb_id[src2dst[i]],
				 memb_cdata, priv->memb_nelmts[i],
				 buf + src_memb->offset,
				 bkg + dst_memb->offset);

		    HDmemmove (buf + offset, buf + src_memb->offset,
			       dst_memb->size);
		    offset += dst_memb->size;
		} else {
		    HDmemmove (buf + offset, buf + src_memb->offset,
			       src_memb->size);
		    offset += src_memb->size;
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

		if (dst_memb->size > src_memb->size) {
		    H5T_conv_t tconv_func = priv->memb_conv[src2dst[i]];
		    H5T_cdata_t *memb_cdata = priv->memb_cdata[src2dst[i]];
		    offset -= src_memb->size;
		    memb_cdata->command = H5T_CONV_CONV;
		    (tconv_func)(priv->src_memb_id[src2dst[i]],
				 priv->dst_memb_id[src2dst[i]],
				 memb_cdata, priv->memb_nelmts[i],
				 buf+offset, bkg+dst_memb->offset);
		} else {
		    offset -= dst_memb->size;
		}
		HDmemmove (bkg+dst_memb->offset, buf+offset, dst_memb->size);
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
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Wednesday, June 10, 1998
 *
 * Modifications:
 *
 *	Robb Matzke, 7 Jul 1998
 *	Added overflow handling.
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
    uint8_t	*s, *sp, *d, *dp;	/*source and dest traversal ptrs*/
    uint8_t	dbuf[64];		/*temp destination buffer	*/
    size_t	first;
    ssize_t	sfirst;			/*a signed version of `first'	*/
    size_t	i;
    
    FUNC_ENTER (H5T_conv_i_i, FAIL);

    switch (cdata->command) {
    case H5T_CONV_INIT:
	if (H5I_DATATYPE!=H5I_get_type (src_id) ||
	    NULL==(src=H5I_object (src_id)) ||
	    H5I_DATATYPE!=H5I_get_type (dst_id) ||
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
	cdata->need_bkg = H5T_BKG_NO;
	break;
	
    case H5T_CONV_FREE:
	break;

    case H5T_CONV_CONV:
	/* Get the data types */
	if (H5I_DATATYPE!=H5I_get_type (src_id) ||
	    NULL==(src=H5I_object (src_id)) ||
	    H5I_DATATYPE!=H5I_get_type (dst_id) ||
	    NULL==(dst=H5I_object (dst_id))) {
	    HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
	}

	/*
	 * Do we process the values from beginning to end or vice versa? Also,
	 * how many of the elements have the source and destination areas
	 * overlapping?
	 */
	if (src->size==dst->size) {
	    sp = dp = (uint8_t*)buf;
	    direction = 1;
	    olap = nelmts;
	} else if (src->size>=dst->size) {
	    sp = dp = (uint8_t*)buf;
	    direction = 1;
	    olap = (size_t)(HDceil((double)(src->size)/
				   (double)(src->size-dst->size))-1);
	} else {
	    sp = (uint8_t*)buf + (nelmts-1) * src->size;
	    dp = (uint8_t*)buf + (nelmts-1) * dst->size;
	    direction = -1;
	    olap = (size_t)(HDceil((double)(dst->size)/
				   (double)(dst->size-src->size))-1);
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
		    uint8_t tmp = s[src->size-(i+1)];
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
		    if (!H5T_overflow_g ||
			(H5T_overflow_g)(src_id, dst_id, s, d)<0) {
			H5T_bit_set (d, dst->u.atomic.offset,
				     dst->u.atomic.prec, TRUE);
		    }
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
		 * possible value) or overflow if the source is negative
		 * (destination is set to zero).
		 */
		if (first+1 == src->u.atomic.prec) {
		    /*overflow*/
		    if (!H5T_overflow_g ||
			(H5T_overflow_g)(src_id, dst_id, s, d)<0) {
			H5T_bit_set (d, dst->u.atomic.offset,
				     dst->u.atomic.prec, FALSE);
		    }
		} else if (src->u.atomic.prec < dst->u.atomic.prec) {
		    H5T_bit_copy (d, dst->u.atomic.offset,
				  s, src->u.atomic.offset,
				  src->u.atomic.prec-1);
		    H5T_bit_set (d, dst->u.atomic.offset+src->u.atomic.prec-1,
				 (dst->u.atomic.prec-src->u.atomic.prec)+1,
				 FALSE);
		} else if (first>=dst->u.atomic.prec) {
		    /*overflow*/
		    if (!H5T_overflow_g ||
			(H5T_overflow_g)(src_id, dst_id, s, d)<0) {
			H5T_bit_set (d, dst->u.atomic.offset,
				     dst->u.atomic.prec, TRUE);
		    }
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
		    if (!H5T_overflow_g ||
			(H5T_overflow_g)(src_id, dst_id, s, d)<0) {
			H5T_bit_set (d, dst->u.atomic.offset,
				     dst->u.atomic.prec-1, TRUE);
			H5T_bit_set (d, (dst->u.atomic.offset +
					 dst->u.atomic.prec-1), 1, FALSE);
		    }
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
		 * source value is negative.  We could experience overflow
		 * if the destination isn't wide enough in which case the
		 * destination is set to a negative number with the largest
		 * possible magnitude.
		 */
		ssize_t sfz = H5T_bit_find (s, src->u.atomic.offset,
					    src->u.atomic.prec-1, H5T_BIT_MSB,
					    FALSE);
		size_t fz = (size_t)sfz;
		
		if (sfz>=0 && fz+1>=dst->u.atomic.prec) {
		    /*overflow*/
		    if (!H5T_overflow_g ||
			(H5T_overflow_g)(src_id, dst_id, s, d)<0) {
			H5T_bit_set (d, dst->u.atomic.offset,
				     dst->u.atomic.prec-1, FALSE);
			H5T_bit_set (d, (dst->u.atomic.offset +
					 dst->u.atomic.prec-1), 1, TRUE);
		    }
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
		    if (!H5T_overflow_g ||
			(H5T_overflow_g)(src_id, dst_id, s, d)<0) {
			H5T_bit_set (d, dst->u.atomic.offset,
				     dst->u.atomic.prec-1, TRUE);
			H5T_bit_set (d, (dst->u.atomic.offset +
					 dst->u.atomic.prec-1), 1, FALSE);
		    }
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
		    uint8_t tmp = d[dst->size-(i+1)];
		    d[dst->size-(i+1)] = d[i];
		    d[i] = tmp;
		}
	    }

	    /*
	     * If we had used a temporary buffer for the destination then we
	     * should copy the value to the true destination buffer.
	     */
	    if (d==dbuf) HDmemcpy (dp, d, dst->size);
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
 * Function:	H5T_conv_f_f
 *
 * Purpose:	Convert one floating point type to another.  This is a catch
 *		all for floating point conversions and is probably not
 *		particularly fast!
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Tuesday, June 23, 1998
 *
 * Modifications:
 *
 *	Robb Matzke, 7 Jul 1998
 *	Added overflow handling.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_f_f (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
	      size_t nelmts, void *buf, void __unused__ *bkg)
{
    /* Traversal-related variables */
    H5T_t	*src_p;			/*source data type		*/
    H5T_t	*dst_p;			/*destination data type		*/
    H5T_atomic_t src;			/*atomic source info		*/
    H5T_atomic_t dst;			/*atomic destination info	*/
    intn	direction;		/*forward or backward traversal	*/
    size_t	elmtno;			/*element number		*/
    size_t	half_size;		/*half the type size		*/
    size_t	olap;			/*num overlapping elements	*/
    ssize_t	bitno;			/*bit number			*/
    uint8_t	*s, *sp, *d, *dp;	/*source and dest traversal ptrs*/
    uint8_t	dbuf[64];		/*temp destination buffer	*/

    /* Conversion-related variables */
    hssize_t	expo;			/*exponent			*/
    hssize_t	expo_max;		/*maximum possible dst exponent	*/
    size_t	msize=0;		/*useful size of mantissa in src*/
    size_t	mpos;			/*offset to useful mant is src	*/
    size_t	mrsh;			/*amount to right shift mantissa*/
    hbool_t	carry;			/*carry after rounding mantissa	*/
    size_t	i;			/*miscellaneous counters	*/
    size_t	implied;		/*destination implied bits	*/
    
    FUNC_ENTER (H5T_conv_f_f, FAIL);

    switch (cdata->command) {
    case H5T_CONV_INIT:
	if (H5I_DATATYPE!=H5I_get_type (src_id) ||
	    NULL==(src_p=H5I_object (src_id)) ||
	    H5I_DATATYPE!=H5I_get_type (dst_id) ||
	    NULL==(dst_p=H5I_object (dst_id))) {
	    HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
	}
	src = src_p->u.atomic;
	dst = dst_p->u.atomic;
	if (H5T_ORDER_LE!=src.order &&
	    H5T_ORDER_BE!=src.order) {
	    HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			   "unsupported byte order");
	}
	if (H5T_ORDER_LE!=dst.order &&
	    H5T_ORDER_BE!=dst.order) {
	    HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			   "unsupported byte order");
	}
	if (dst_p->size>sizeof(dbuf)) {
	    HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			   "destination size is too large");
	}
	if (8*sizeof(expo)-1<src.u.f.esize ||
	    8*sizeof(expo)-1<dst.u.f.esize) {
	    HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			   "exponent field is too large");
	}
	cdata->need_bkg = H5T_BKG_NO;
	break;

    case H5T_CONV_FREE:
	break;

    case H5T_CONV_CONV:
	/* Get the data types */
	if (H5I_DATATYPE!=H5I_get_type (src_id) ||
	    NULL==(src_p=H5I_object (src_id)) ||
	    H5I_DATATYPE!=H5I_get_type (dst_id) ||
	    NULL==(dst_p=H5I_object (dst_id))) {
	    HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
	}
	src = src_p->u.atomic;
	dst = dst_p->u.atomic;
	expo_max = ((hssize_t)1 << dst.u.f.esize) - 1;

	/*
	 * Do we process the values from beginning to end or vice versa? Also,
	 * how many of the elements have the source and destination areas
	 * overlapping?
	 */
	if (src_p->size==dst_p->size) {
	    sp = dp = (uint8_t*)buf;
	    direction = 1;
	    olap = nelmts;
	} else if (src_p->size>=dst_p->size) {
	    sp = dp = (uint8_t*)buf;
	    direction = 1;
	    olap = (size_t)(HDceil((double)(src_p->size)/
				   (double)(src_p->size-dst_p->size))-1);
	} else {
	    sp = (uint8_t*)buf + (nelmts-1) * src_p->size;
	    dp = (uint8_t*)buf + (nelmts-1) * dst_p->size;
	    direction = -1;
	    olap = (size_t)(HDceil((double)(dst_p->size)/
				   (double)(dst_p->size-src_p->size))-1);
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
		assert ((dp>=sp && dp<sp+src_p->size) ||
			(sp>=dp && sp<dp+dst_p->size));
	    } else {
		assert ((dp<sp && dp+dst_p->size<=sp) ||
			(sp<dp && sp+src_p->size<=dp));
	    }
#endif
	    
	    /*
	     * Put the data in little endian order so our loops aren't so
	     * complicated.  We'll do all the conversion stuff assuming
	     * little endian and then we'll fix the order at the end.
	     */
	    if (H5T_ORDER_BE==src.order) {
		half_size = src_p->size/2;
		for (i=0; i<half_size; i++) {
		    uint8_t tmp = s[src_p->size-(i+1)];
		    s[src_p->size-(i+1)] = s[i];
		    s[i] = tmp;
		}
	    }

	    /*
	     * Check for special cases: +0, -0, +Inf, -Inf, NaN
	     */
	    if (H5T_bit_find (s, src.u.f.mpos, src.u.f.msize,
			      H5T_BIT_LSB, TRUE)<0) {
		if (H5T_bit_find (s, src.u.f.epos, src.u.f.esize,
				  H5T_BIT_LSB, TRUE)<0) {
		    /* +0 or -0 */
		    H5T_bit_copy (d, dst.u.f.sign, s, src.u.f.sign, 1);
		    H5T_bit_set (d, dst.u.f.epos, dst.u.f.esize, FALSE);
		    H5T_bit_set (d, dst.u.f.mpos, dst.u.f.esize, FALSE);
		    goto padding;
		} else if (H5T_bit_find (s, src.u.f.epos, src.u.f.esize,
					 H5T_BIT_LSB, FALSE)<0) {
		    /* +Inf or -Inf */
		    H5T_bit_copy (d, dst.u.f.sign, s, src.u.f.sign, 1);
		    H5T_bit_set (d, dst.u.f.epos, dst.u.f.esize, TRUE);
		    H5T_bit_set (d, dst.u.f.mpos, dst.u.f.msize, FALSE);
		    goto padding;
		}
	    } else if (H5T_bit_find (s, src.u.f.epos, src.u.f.esize,
				     H5T_BIT_LSB, FALSE)<0) {
		/*
		 * NaN. There are many NaN values, so we just set all bits of
		 * the significand.
		 */
		H5T_bit_copy (d, dst.u.f.sign, s, src.u.f.sign, 1);
		H5T_bit_set (d, dst.u.f.epos, dst.u.f.esize, TRUE);
		H5T_bit_set(d, dst.u.f.mpos, dst.u.f.msize, TRUE);
		goto padding;
	    }

	    /*
	     * Get the exponent as an unsigned quantity from the section of
	     * the source bit field where it's located.	 Don't worry about
	     * the exponent bias yet.
	     */
	    expo = H5T_bit_get_d(s, src.u.f.epos, src.u.f.esize);
	    
	    /*
	     * Set markers for the source mantissa, excluding the leading `1'
	     * (might be implied).
	     */
	    implied = 1;
	    mpos = src.u.f.mpos;
	    mrsh = 0;
	    if (0==expo || H5T_NORM_NONE==src.u.f.norm) {
		if ((bitno=H5T_bit_find(s, src.u.f.mpos, src.u.f.msize,
					H5T_BIT_MSB, TRUE))>0) {
		    msize = bitno;
		} else if (0==bitno) {
		    msize = 1;
		    H5T_bit_set(s, src.u.f.mpos, 1, FALSE);
		}
	    } else if (H5T_NORM_IMPLIED==src.u.f.norm) {
		msize = src.u.f.msize;
	    } else {
		assert("normalization method not implemented yet" && 0);
		HDabort();
	    }
	    
	    /*
	     * The sign for the destination is the same as the sign for the
	     * source in all cases.
	     */
	    H5T_bit_copy (d, dst.u.f.sign, s, src.u.f.sign, 1);

	    /*
	     * Calculate the true source exponent by adjusting according to
	     * the source exponent bias.
	     */
	    if (0==expo || H5T_NORM_NONE==src.u.f.norm) {
		bitno = H5T_bit_find(s, src.u.f.mpos, src.u.f.msize,
				     H5T_BIT_MSB, TRUE);
		assert(bitno>=0);
		expo -= (src.u.f.ebias-1) + (src.u.f.msize-bitno);
	    } else if (H5T_NORM_IMPLIED==src.u.f.norm) {
		expo -= src.u.f.ebias;
	    } else {
		assert("normalization method not implemented yet" && 0);
		HDabort();
	    }

	    /*
	     * If the destination is not normalized then right shift the
	     * mantissa by one.
	     */
	    if (H5T_NORM_NONE==dst.u.f.norm) {
		mrsh++;
	    }

	    /*
	     * Calculate the destination exponent by adding the destination
	     * bias and clipping by the minimum and maximum possible
	     * destination exponent values.
	     */
	    expo += dst.u.f.ebias;
	    if (expo < -(hssize_t)(dst.u.f.msize)) {
		/* The exponent is way too small.  Result is zero. */
		expo = 0;
		H5T_bit_set(d, dst.u.f.mpos, dst.u.f.msize, FALSE);
		msize = 0;

	    } else if (expo<=0) {
		/*
		 * The exponent is too small to fit in the exponent field,
		 * but by shifting the mantissa to the right we can
		 * accomodate that value.  The mantissa of course is no
		 * longer normalized.
		 */
		mrsh += 1-expo;
		expo = 0;
		
	    } else if (expo>=expo_max) {
		/*
		 * The exponent is too large to fit in the available region
		 * or it results in the maximum possible value.	 Use positive
		 * or negative infinity instead unless the application
		 * specifies something else.  Before calling the overflow
		 * handler make sure the source buffer we hand it is in the
		 * original byte order.
		 */
		if (H5T_overflow_g) {
		    uint8_t over_src[256];
		    assert(src_p->size<=sizeof over_src);
		    if (H5T_ORDER_BE==src.order) {
			for (i=0; i<src_p->size; i++) {
			    over_src[src_p->size-(i+1)] = s[i];
			}
		    } else {
			for (i=0; i<src_p->size; i++) {
			    over_src[i] = s[i];
			}
		    }
		    if ((H5T_overflow_g)(src_id, dst_id, over_src, d)>=0) {
			goto next;
		    }
		}
		expo = expo_max;
		H5T_bit_set(d, dst.u.f.mpos, dst.u.f.msize, FALSE);
		msize = 0;
	    }

	    /*
	     * If the destination mantissa is smaller than the source
	     * mantissa then round the source mantissa.	 Rounding may cause a
	     * carry in which case the exponent has to be re-evaluated for
	     * overflow.  That is, if `carry' is clear then the implied
	     * mantissa bit is `1', else it is `10' binary.
	     */
	    if (msize>0 && mrsh<=dst.u.f.msize && mrsh+msize>dst.u.f.msize) {
		bitno = (ssize_t)(mrsh+msize - dst.u.f.msize);
		assert(bitno>=0 && (size_t)bitno<=msize);
		carry = H5T_bit_inc(s, mpos+bitno-1, 1+msize-bitno);
		if (carry) implied = 2;
	    }

	    /*
	     * Write the mantissa to the destination
	     */
	    if (mrsh>dst.u.f.msize+1) {
		H5T_bit_set(d, dst.u.f.mpos, dst.u.f.msize, FALSE);
	    } else if (mrsh==dst.u.f.msize+1) {
		H5T_bit_set(d, dst.u.f.mpos+1, dst.u.f.msize-1, FALSE);
		H5T_bit_set(d, dst.u.f.mpos, 1, TRUE);
	    } else if (mrsh==dst.u.f.msize) {
		H5T_bit_set(d, dst.u.f.mpos, dst.u.f.msize, FALSE);
		H5T_bit_set_d(d, dst.u.f.mpos, MIN(2, dst.u.f.msize), implied);
	    } else {
		if (mrsh>0) {
		    H5T_bit_set(d, dst.u.f.mpos+dst.u.f.msize-mrsh, mrsh,
				FALSE);
		    H5T_bit_set_d(d, dst.u.f.mpos+dst.u.f.msize-mrsh, 2,
				  implied);
		}
		if (mrsh+msize>=dst.u.f.msize) {
		    H5T_bit_copy(d, dst.u.f.mpos,
				 s, (mpos+msize+mrsh-dst.u.f.msize), 
				 dst.u.f.msize-mrsh);
		} else {
		    H5T_bit_copy(d, dst.u.f.mpos+dst.u.f.msize-(mrsh+msize),
				 s, mpos, msize);
		    H5T_bit_set(d, dst.u.f.mpos, dst.u.f.msize-(mrsh+msize),
				FALSE);
		}
	    }
		
	    /* Write the exponent */
	    H5T_bit_set_d(d, dst.u.f.epos, dst.u.f.esize, expo);

	padding:
#ifndef LATER
	    /*
	     * Set internal padding areas
	     */
#endif

	    /*
	     * Set external padding areas
	     */
	    if (dst.offset>0) {
		assert (H5T_PAD_ZERO==dst.lsb_pad ||
			H5T_PAD_ONE==dst.lsb_pad);
		H5T_bit_set (d, 0, dst.offset,
			     H5T_PAD_ONE==dst.lsb_pad);
	    }
	    if (dst.offset+dst.prec!=8*dst_p->size) {
		assert (H5T_PAD_ZERO==dst.msb_pad ||
			H5T_PAD_ONE==dst.msb_pad);
		H5T_bit_set (d, dst.offset+dst.prec,
			     8*dst_p->size - (dst.offset+dst.prec),
			     H5T_PAD_ONE==dst.msb_pad);
	    }

	    /*
	     * Put the destination in the correct byte order.  See note at
	     * beginning of loop.
	     */
	    if (H5T_ORDER_BE==dst.order) {
		half_size = dst_p->size/2;
		for (i=0; i<half_size; i++) {
		    uint8_t tmp = d[dst_p->size-(i+1)];
		    d[dst_p->size-(i+1)] = d[i];
		    d[i] = tmp;
		}
	    }

	    /*
	     * If we had used a temporary buffer for the destination then we
	     * should copy the value to the true destination buffer.
	     */
	next:
	    if (d==dbuf) HDmemcpy (dp, d, dst_p->size);
	    sp += direction * src_p->size;
	    dp += direction * dst_p->size;
	}
	
	break;
	    
    default:
	HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
		       "unknown conversion command");
    }

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_s_s
 *
 * Purpose:	Convert one fixed-length string type to another.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Friday, August	7, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_s_s (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata, size_t nelmts,
	      void *buf, void __unused__ *bkg)
{
    H5T_t	*src=NULL;		/*source data type		*/
    H5T_t	*dst=NULL;		/*destination data type		*/
    intn	direction;		/*direction of traversal	*/
    size_t	elmtno;			/*element number		*/
    size_t	olap;			/*num overlapping elements	*/
    size_t	nchars=0;		/*number of characters copied	*/
    uint8_t	*s, *sp, *d, *dp;	/*src and dst traversal pointers*/
    uint8_t	*dbuf=NULL;		/*temp buf for overlap convers.	*/
    herr_t	ret_value=FAIL;		/*return value			*/
    
    FUNC_ENTER(H5T_conv_s_s, FAIL);

    switch (cdata->command) {
    case H5T_CONV_INIT:
	if (H5I_DATATYPE!=H5I_get_type(src_id) ||
	    NULL==(src=H5I_object(src_id)) ||
	    H5I_DATATYPE!=H5I_get_type(dst_id) ||
	    NULL==(dst=H5I_object(dst_id))) {
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
	}
	if (8*src->size != src->u.atomic.prec ||
	    8*dst->size != dst->u.atomic.prec) {
	    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bad precision");
	}
	if (0 != src->u.atomic.offset ||
	    0 != dst->u.atomic.offset) {
	    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bad offset");
	}
	if (H5T_CSET_ASCII != src->u.atomic.u.s.cset ||
	    H5T_CSET_ASCII != dst->u.atomic.u.s.cset) {
	    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bad character set");
	}
	if (src->u.atomic.u.s.pad<0 || src->u.atomic.u.s.pad>=H5T_NPAD ||
	    dst->u.atomic.u.s.pad<0 || dst->u.atomic.u.s.pad>=H5T_NPAD) {
	    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bad character padding");
	}
	cdata->need_bkg = H5T_BKG_NO;
	break;

    case H5T_CONV_FREE:
	break;

    case H5T_CONV_CONV:
	/* Get the data types */
	if (H5I_DATATYPE!=H5I_get_type(src_id) ||
	    NULL==(src=H5I_object(src_id)) ||
	    H5I_DATATYPE!=H5I_get_type(dst_id) ||
	    NULL==(dst=H5I_object(dst_id))) {
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
	}

	/*
	 * Do we process the values from beginning to end or vice versa? Also,
	 * how many of the elements have the source and destination areas
	 * overlapping?
	 */
	if (src->size==dst->size) {
	    /*
	     * When the source and destination are the same size we can do
	     * all the conversions in place.
	     */
	    sp = dp = (uint8_t*)buf;
	    direction = 1;
	    olap = 0;
	} else if (src->size>=dst->size) {
	    sp = dp = (uint8_t*)buf;
	    direction = 1;
	    olap = (size_t)(HDceil((double)(src->size)/
				   (double)(src->size-dst->size))-1);
	} else {
	    sp = (uint8_t*)buf + (nelmts-1) * src->size;
	    dp = (uint8_t*)buf + (nelmts-1) * dst->size;
	    direction = -1;
	    olap = (size_t)(HDceil((double)(dst->size)/
				   (double)(dst->size-src->size))-1);
	}

	/* Allocate the overlap buffer */
	if (NULL==(dbuf=H5MM_malloc(dst->size))) {
	    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
			"memory allocation failed for string conversion");
	}
	
	/* The conversion loop. */
	for (elmtno=0; elmtno<nelmts; elmtno++) {

	    /*
	     * If the source and destination buffers overlap then use a
	     * temporary buffer fot eh destination.
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
	    if (src->size==dst->size) {
		assert(s==d);
	    } else if (d==dbuf) {
		assert((dp>=sp && dp<sp+src->size) ||
		       (sp>=dp && sp<dp+dst->size));
	    } else {
		assert((dp<sp && dp+dst->size<=sp) ||
		       (sp<dp && sp+src->size<=dp));
	    }
#endif
	    
	    /* Copy characters from source to destination */
	    switch (src->u.atomic.u.s.pad) {
	    case H5T_STR_NULLTERM:
		for (nchars=0;
		     nchars<dst->size && nchars<src->size && s[nchars];
		     nchars++) {
		    d[nchars] = s[nchars];
		}
		break;

	    case H5T_STR_NULLPAD:
		for (nchars=0;
		     nchars<dst->size && nchars<src->size && s[nchars];
		     nchars++) {
		    d[nchars] = s[nchars];
		}
		break;

	    case H5T_STR_SPACEPAD:
		nchars = src->size;
		while (nchars>0 && ' '==s[nchars-1]) --nchars;
		nchars = MIN(dst->size, nchars);
		HDmemcpy(d, s, nchars);
		break;

	    case H5T_STR_RESERVED_3:
	    case H5T_STR_RESERVED_4:
	    case H5T_STR_RESERVED_5:
	    case H5T_STR_RESERVED_6:
	    case H5T_STR_RESERVED_7:
	    case H5T_STR_RESERVED_8:
	    case H5T_STR_RESERVED_9:
	    case H5T_STR_RESERVED_10:
	    case H5T_STR_RESERVED_11:
	    case H5T_STR_RESERVED_12:
	    case H5T_STR_RESERVED_13:
	    case H5T_STR_RESERVED_14:
	    case H5T_STR_RESERVED_15:
	    case H5T_STR_ERROR:
		HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			    "source string padding method not supported");
	    }

	    /* Terminate or pad the destination */
	    switch (dst->u.atomic.u.s.pad) {
	    case H5T_STR_NULLTERM:
		while (nchars<dst->size) d[nchars++] = '\0';
		d[dst->size-1] = '\0';
		break;
		
	    case H5T_STR_NULLPAD:
		while (nchars<dst->size) d[nchars++] = '\0';
		break;
		
	    case H5T_STR_SPACEPAD:
		while (nchars<dst->size) d[nchars++] = ' ';
		break;

	    case H5T_STR_RESERVED_3:
	    case H5T_STR_RESERVED_4:
	    case H5T_STR_RESERVED_5:
	    case H5T_STR_RESERVED_6:
	    case H5T_STR_RESERVED_7:
	    case H5T_STR_RESERVED_8:
	    case H5T_STR_RESERVED_9:
	    case H5T_STR_RESERVED_10:
	    case H5T_STR_RESERVED_11:
	    case H5T_STR_RESERVED_12:
	    case H5T_STR_RESERVED_13:
	    case H5T_STR_RESERVED_14:
	    case H5T_STR_RESERVED_15:
	    case H5T_STR_ERROR:
		HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			    "destination string padding method not supported");
	    }

	    /*
	     * If we used a temporary buffer for the destination then we
	     * should copy the value to the true destination buffer.
	     */
	    if (d==dbuf) HDmemcpy(dp, d, dst->size);
	    sp += direction * src->size;
	    dp += direction * dst->size;
	}
	break;

    default:
	HGOTO_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
		      "unknown converson command");
    }
    ret_value = SUCCEED;

 done:
    H5MM_xfree(dbuf);
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_schar_uchar
 *
 * Purpose:	Converts `signed char' to `unsigned char'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Monday, November 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_schar_uchar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_schar_uchar, FAIL);
    H5T_CONV_su(SCHAR, UCHAR,
		signed char, unsigned char);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uchar_schar
 *
 * Purpose:	Converts `unsigned char' to `signed char'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Monday, November 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uchar_schar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_uchar_schar, FAIL);
    H5T_CONV_us(UCHAR, SCHAR,
		unsigned char, signed char,
		SCHAR_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_schar_short
 *
 * Purpose:	Converts `signed char' to `short'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_schar_short(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_schar_short, FAIL);
    H5T_CONV_sS(SCHAR, SHORT,
		signed char, short);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_schar_ushort
 *
 * Purpose:	Converts `signed char' to `unsigned short'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_schar_ushort(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_schar_ushort, FAIL);
    H5T_CONV_sU(SCHAR, USHORT,
		signed char, unsigned short);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uchar_short
 *
 * Purpose:	Converts `unsigned char' to `short'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uchar_short(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_uchar_short, FAIL);
    H5T_CONV_uS(UCHAR, SHORT,
		unsigned char, short,
		SHRT_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uchar_ushort
 *
 * Purpose:	Converts `unsigned char' to `unsigned short'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uchar_ushort(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_uchar_ushort, FAIL);
    H5T_CONV_uU(UCHAR, USHORT,
		unsigned char, unsigned short);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_schar_int
 *
 * Purpose:	Converts `signed char' to `int'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_schar_int(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		   size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_schar_int, FAIL);
    H5T_CONV_sS(SCHAR, INT,
		signed char, int);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_schar_uint
 *
 * Purpose:	Converts `signed char' to `unsigned int'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_schar_uint(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_schar_uint, FAIL);
    H5T_CONV_sU(SCHAR, UINT,
		signed char, unsigned);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uchar_int
 *
 * Purpose:	Converts `unsigned char' to `int'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uchar_int(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		   size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_uchar_int, FAIL);
    H5T_CONV_uS(UCHAR, INT,
		unsigned char, int,
		INT_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uchar_uint
 *
 * Purpose:	Converts `unsigned char' to `unsigned int'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uchar_uint(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_uchar_uint, FAIL);
    H5T_CONV_uU(UCHAR, UINT,
		unsigned char, unsigned);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_schar_long
 *
 * Purpose:	Converts `signed char' to `long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_schar_long(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_schar_long, FAIL);
    H5T_CONV_sS(SCHAR, LONG,
		signed char, long);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_schar_ulong
 *
 * Purpose:	Converts `signed char' to `unsigned long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_schar_ulong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_schar_ulong, FAIL);
    H5T_CONV_sU(SCHAR, ULONG,
		signed char, unsigned long);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uchar_long
 *
 * Purpose:	Converts `unsigned char' to `long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uchar_long(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_uchar_long, FAIL);
    H5T_CONV_uS(UCHAR, LONG,
		unsigned char, long,
		LONG_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uchar_ulong
 *
 * Purpose:	Converts `unsigned char' to `unsigned long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uchar_ulong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_uchar_ulong, FAIL);
    H5T_CONV_uU(UCHAR, ULONG,
		unsigned char, unsigned long);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_schar_llong
 *
 * Purpose:	Converts `signed char' to `long_long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_schar_llong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_schar_llong, FAIL);
    H5T_CONV_sS(SCHAR, LLONG,
		signed char, long_long);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_schar_ullong
 *
 * Purpose:	Converts `signed char' to `unsigned long_long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_schar_ullong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_schar_ullong, FAIL);
    H5T_CONV_sU(SCHAR, ULLONG,
		signed char, unsigned long_long);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uchar_llong
 *
 * Purpose:	Converts `unsigned char' to `long_long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uchar_llong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_uchar_llong, FAIL);
    H5T_CONV_uS(UCHAR, LLONG,
		unsigned char, long_long,
		LLONG_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uchar_ullong
 *
 * Purpose:	Converts `unsigned char' to `unsigned long_long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uchar_ullong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_uchar_ullong, FAIL);
    H5T_CONV_uU(UCHAR, ULLONG,
		unsigned char, unsigned long_long);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_short_schar
 *
 * Purpose:	Converts `short' to `signed char'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_short_schar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_short_schar, FAIL);
    H5T_CONV_Ss(SHORT, SCHAR,
		short, signed char,
		SCHAR_MIN, SCHAR_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_short_uchar
 *
 * Purpose:	Converts `short' to `unsigned char'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_short_uchar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_short_uchar, FAIL);
    H5T_CONV_Su(SHORT, UCHAR,
		short, unsigned char,
		UCHAR_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ushort_schar
 *
 * Purpose:	Converts `unsigned short' to `signed char'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ushort_schar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_ushort_schar, FAIL);
    H5T_CONV_Us(USHORT, SCHAR,
		unsigned short, signed char,
		SCHAR_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ushort_uchar
 *
 * Purpose:	Converts `unsigned short' to `unsigned char'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ushort_uchar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_ushort_uchar, FAIL);
    H5T_CONV_Uu(USHORT, UCHAR,
		unsigned short, unsigned char,
		UCHAR_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_short_ushort
 *
 * Purpose:	Converts `short' to `unsigned short'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Monday, November 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_short_ushort(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_short_ushort, FAIL);
    H5T_CONV_su(SHORT, USHORT,
		short, unsigned short);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ushort_short
 *
 * Purpose:	Converts `unsigned short' to `short'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Monday, November 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ushort_short(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_ushort_short, FAIL);
    H5T_CONV_us(USHORT, SHORT,
		unsigned short, short,
		SHRT_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_short_int
 *
 * Purpose:	Converts `short' to `int'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_short_int(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		   size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_short_int, FAIL);
    H5T_CONV_sS(SHORT, INT,
		short, int);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_short_uint
 *
 * Purpose:	Converts `short' to `unsigned int'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_short_uint(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_short_uint, FAIL);
    H5T_CONV_sU(SHORT, UINT,
		short, unsigned);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ushort_int
 *
 * Purpose:	Converts `unsigned short' to `int'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ushort_int(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_ushort_int, FAIL);
    H5T_CONV_uS(USHORT, INT,
		unsigned short, int,
		INT_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ushort_uint
 *
 * Purpose:	Converts `unsigned short' to `unsigned int'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ushort_uint(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_ushort_uint, FAIL);
    H5T_CONV_uU(USHORT, UINT,
		unsigned short, unsigned);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_short_long
 *
 * Purpose:	Converts `short' to `long'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_short_long(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_short_long, FAIL);
    H5T_CONV_sS(SHORT, LONG,
		short, long);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_short_ulong
 *
 * Purpose:	Converts `short' to `unsigned long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_short_ulong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_short_ulong, FAIL);
    H5T_CONV_sU(SHORT, ULONG,
		short, unsigned long);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ushort_long
 *
 * Purpose:	Converts `unsigned short' to `long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ushort_long(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_ushort_long, FAIL);
    H5T_CONV_uS(USHORT, LONG,
		unsigned short, long,
		LONG_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ushort_ulong
 *
 * Purpose:	Converts `unsigned short' to `unsigned long'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ushort_ulong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_ushort_ulong, FAIL);
    H5T_CONV_uU(USHORT, ULONG,
		unsigned short, unsigned long);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_short_llong
 *
 * Purpose:	Converts `short' to `long_long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_short_llong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_short_llong, FAIL);
    H5T_CONV_sS(SHORT, LLONG,
		short, long_long);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_short_ullong
 *
 * Purpose:	Converts `short' to `unsigned long_long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_short_ullong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_short_ullong, FAIL);
    H5T_CONV_sU(SHORT, ULLONG,
		short, unsigned long_long);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ushort_llong
 *
 * Purpose:	Converts `unsigned short' to `long_long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ushort_llong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_ushort_llong, FAIL);
    H5T_CONV_uS(USHORT, LLONG,
		unsigned short, long_long,
		LLONG_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ushort_ullong
 *
 * Purpose:	Converts `unsigned short' to `unsigned long_long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ushort_ullong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_ushort_ullong, FAIL);
    H5T_CONV_uU(USHORT, ULLONG,
		unsigned short, unsigned long_long);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_int_schar
 *
 * Purpose:	Converts `int' to `signed char'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_int_schar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		   size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_int_schar, FAIL);
    H5T_CONV_Ss(INT, SCHAR,
		int, signed char,
		SCHAR_MIN, SCHAR_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_int_uchar
 *
 * Purpose:	Converts `int' to `unsigned char'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_int_uchar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		   size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_int_uchar, FAIL);
    H5T_CONV_Su(INT, UCHAR,
		int, unsigned char,
		UCHAR_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uint_schar
 *
 * Purpose:	Converts `unsigned int' to `signed char'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uint_schar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_uint_schar, FAIL);
    H5T_CONV_Us(UINT, SCHAR,
		unsigned, signed char,
		SCHAR_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uint_uchar
 *
 * Purpose:	Converts `unsigned int' to `unsigned char'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uint_uchar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_uint_uchar, FAIL);
    H5T_CONV_Uu(UINT, UCHAR,
		unsigned, unsigned char,
		UCHAR_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_int_short
 *
 * Purpose:	Converts `int' to `short'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_int_short(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		   size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_int_short, FAIL);
    H5T_CONV_Ss(INT, SHORT,
		int, short,
		SHRT_MIN, SHRT_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_int_ushort
 *
 * Purpose:	Converts `int' to `unsigned short'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_int_ushort(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_int_ushort, FAIL);
    H5T_CONV_Su(INT, USHORT,
		int, unsigned short,
		USHRT_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uint_short
 *
 * Purpose:	Converts `unsigned int' to `short'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uint_short(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_uint_short, FAIL);
    H5T_CONV_Us(UINT, SHORT,
		unsigned, short,
		SHRT_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uint_ushort
 *
 * Purpose:	Converts `unsigned int' to `unsigned short'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uint_ushort(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_uint_ushort, FAIL);
    H5T_CONV_Uu(UINT, USHORT,
		unsigned, unsigned short,
		USHRT_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_int_uint
 *
 * Purpose:	Converts `int' to `unsigned int'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Monday, November 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_int_uint(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		  size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_int_uint, FAIL);
    H5T_CONV_su(INT, UINT,
		int, unsigned);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uint_int
 *
 * Purpose:	Converts `unsigned int' to `int'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Monday, November 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uint_int(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		  size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_uint_int, FAIL);
    H5T_CONV_us(UINT, INT,
		unsigned, int,
		INT_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_int_long
 *
 * Purpose:	Converts `int' to `long'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_int_long(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		  size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_int_long, FAIL);
    H5T_CONV_sS(INT, LONG,
		int, long);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_int_ulong
 *
 * Purpose:	Converts `int' to `unsigned long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_int_ulong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		   size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_int_ulong, FAIL);
    H5T_CONV_sU(INT, LONG,
		int, unsigned long);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uint_long
 *
 * Purpose:	Converts `unsigned int' to `long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uint_long(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		   size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_uint_long, FAIL);
    H5T_CONV_uS(UINT, LONG,
		unsigned, long,
		LONG_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uint_ulong
 *
 * Purpose:	Converts `unsigned int' to `unsigned long'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uint_ulong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_uint_ulong, FAIL);
    H5T_CONV_uU(UINT, ULONG,
		unsigned, unsigned long);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_int_llong
 *
 * Purpose:	Converts `int' to `long_long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_int_llong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		   size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_int_llong, FAIL);
    H5T_CONV_sS(INT, LLONG,
		int, long_long);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_int_ullong
 *
 * Purpose:	Converts `int' to `unsigned long_long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_int_ullong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_int_ullong, FAIL);
    H5T_CONV_sU(INT, ULLONG,
		int, unsigned long_long);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uint_llong
 *
 * Purpose:	Converts `unsigned int' to `long_long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uint_llong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_uint_llong, FAIL);
    H5T_CONV_uS(UINT, LLONG,
		unsigned, long_long,
		LLONG_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_uint_ullong
 *
 * Purpose:	Converts `unsigned int' to `unsigned long_long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_uint_ullong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_uint_ullong, FAIL);
    H5T_CONV_uU(UINT, ULLONG,
		unsigned, unsigned long_long);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_long_schar
 *
 * Purpose:	Converts `long' to `signed char'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_long_schar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_long_schar, FAIL);
    H5T_CONV_Ss(LONG, SCHAR,
		long, signed char,
		SCHAR_MIN, SCHAR_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_long_uchar
 *
 * Purpose:	Converts `long' to `unsigned char'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_long_uchar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_long_uchar, FAIL);
    H5T_CONV_Su(LONG, UCHAR,
		long, unsigned char,
		UCHAR_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ulong_schar
 *
 * Purpose:	Converts `unsigned long' to `signed char'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ulong_schar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_ulong_schar, FAIL);
    H5T_CONV_Us(ULONG, SCHAR,
		unsigned long, signed char,
		SCHAR_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ulong_uchar
 *
 * Purpose:	Converts `unsigned long' to `unsigned char'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ulong_uchar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_ulong_uchar, FAIL);
    H5T_CONV_Uu(ULONG, UCHAR,
		unsigned long, unsigned char,
		UCHAR_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_long_short
 *
 * Purpose:	Converts `long' to `short'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_long_short(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_long_short, FAIL);
    H5T_CONV_Ss(LONG, SHORT,
		long, short,
		SHRT_MIN, SHRT_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_long_ushort
 *
 * Purpose:	Converts `long' to `unsigned short'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_long_ushort(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_long_ushort, FAIL);
    H5T_CONV_Su(LONG, USHORT,
		long, unsigned short,
		USHRT_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ulong_short
 *
 * Purpose:	Converts `unsigned long' to `short'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ulong_short(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_ulong_short, FAIL);
    H5T_CONV_Us(ULONG, SHORT,
		unsigned long, short,
		SHRT_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ulong_ushort
 *
 * Purpose:	Converts `unsigned long' to `unsigned short'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ulong_ushort(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_ulong_ushort, FAIL);
    H5T_CONV_Uu(ULONG, USHORT,
		unsigned long, unsigned short,
		USHRT_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_long_int
 *
 * Purpose:	Converts `long' to `int'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_long_int(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		  size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_long_int, FAIL);
    H5T_CONV_Ss(LONG, INT,
		long, int,
		INT_MIN, INT_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_long_uint
 *
 * Purpose:	Converts `long' to `unsigned int'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_long_uint(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		   size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_long_uint, FAIL);
    H5T_CONV_Su(LONG, UINT,
		long, unsigned,
		UINT_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ulong_int
 *
 * Purpose:	Converts `unsigned long' to `int'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ulong_int(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		   size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_ulong_int, FAIL);
    H5T_CONV_Us(ULONG, INT,
		unsigned long, int,
		INT_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ulong_uint
 *
 * Purpose:	Converts `unsigned long' to `unsigned int'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ulong_uint(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_ulong_uint, FAIL);
    H5T_CONV_Uu(ULONG, UINT,
		unsigned long, unsigned,
		UINT_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_long_ulong
 *
 * Purpose:	Converts `long' to `unsigned long'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Monday, November 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_long_ulong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_long_ulong, FAIL);
    H5T_CONV_su(LONG, ULONG,
		long, unsigned long);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ulong_long
 *
 * Purpose:	Converts `unsigned long' to `long'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Monday, November 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ulong_long(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_ulong_long, FAIL);
    H5T_CONV_us(ULONG, LONG,
		unsigned long, long,
		LONG_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_long_llong
 *
 * Purpose:	Converts `long' to `long_long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_long_llong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_long_llong, FAIL);
    H5T_CONV_sS(LONG, LLONG,
		long, long_long);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_long_ullong
 *
 * Purpose:	Converts `long' to `unsigned long_long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_long_ullong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_long_ullong, FAIL);
    H5T_CONV_sU(LONG, ULLONG,
		long, unsigned long_long);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ulong_llong
 *
 * Purpose:	Converts `unsigned long' to `long_long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ulong_llong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_long_llong, FAIL);
    H5T_CONV_uS(ULONG, LLONG,
		unsigned long, long_long,
		LLONG_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ulong_ullong
 *
 * Purpose:	Converts `unsigned long' to `unsigned long_long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ulong_ullong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_ulong_ullong, FAIL);
    H5T_CONV_uU(ULONG, ULLONG,
		unsigned long, unsigned long_long);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_llong_schar
 *
 * Purpose:	Converts `long_long' to `signed char'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_llong_schar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_llong_schar, FAIL);
    H5T_CONV_Ss(LLONG, SCHAR,
		long_long, signed char,
		SCHAR_MIN, SCHAR_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_llong_uchar
 *
 * Purpose:	Converts `long_long' to `unsigned char'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_llong_uchar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_llong_uchar, FAIL);
    H5T_CONV_Su(LLONG, UCHAR,
		long_long, unsigned char,
		UCHAR_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ullong_schar
 *
 * Purpose:	Converts `unsigned long_long' to `signed char'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ullong_schar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_ullong_schar, FAIL);
    H5T_CONV_Us(ULLONG, SCHAR,
		unsigned long_long, signed char,
		SCHAR_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ullong_uchar
 *
 * Purpose:	Converts `unsigned long_long' to `unsigned char'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ullong_uchar(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_ullong_uchar, FAIL);
    H5T_CONV_Uu(ULLONG, UCHAR,
		unsigned long_long, unsigned char,
		UCHAR_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_llong_short
 *
 * Purpose:	Converts `long_long' to `short'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_llong_short(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_llong_short, FAIL);
    H5T_CONV_Ss(LLONG, SHORT,
		long_long, short,
		SHRT_MIN, SHRT_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_llong_ushort
 *
 * Purpose:	Converts `long_long' to `unsigned short'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_llong_ushort(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_llong_ushort, FAIL);
    H5T_CONV_Su(LLONG, USHORT,
		long_long, unsigned short,
		USHRT_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ullong_short
 *
 * Purpose:	Converts `unsigned long_long' to `short'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ullong_short(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_ullong_short, FAIL);
    H5T_CONV_Us(ULLONG, SHORT,
		unsigned long_long, short,
		SHRT_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ullong_ushort
 *
 * Purpose:	Converts `unsigned long_long' to `unsigned short'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ullong_ushort(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_ullong_ushort, FAIL);
    H5T_CONV_Uu(ULLONG, USHORT,
		unsigned long_long, unsigned short,
		USHRT_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_llong_int
 *
 * Purpose:	Converts `long_long' to `int'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_llong_int(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		   size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_llong_int, FAIL);
    H5T_CONV_Ss(LLONG, INT,
		long_long, int,
		INT_MIN, INT_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_llong_uint
 *
 * Purpose:	Converts `long_long' to `unsigned int'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_llong_uint(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_llong_uint, FAIL);
    H5T_CONV_Su(LLONG, UINT,
		long_long, unsigned,
		UINT_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ullong_int
 *
 * Purpose:	Converts `unsigned long_long' to `int'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ullong_int(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_ullong_int, FAIL);
    H5T_CONV_Us(ULLONG, INT,
		unsigned long_long, int,
		INT_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ullong_uint
 *
 * Purpose:	Converts `unsigned long_long' to `unsigned int'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ullong_uint(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_ullong_uint, FAIL);
    H5T_CONV_Uu(ULLONG, UINT,
		unsigned long_long, unsigned,
		UINT_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_llong_long
 *
 * Purpose:	Converts `long_long' to `long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_llong_long(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		    size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_llong_long, FAIL);
    H5T_CONV_Ss(LLONG, LONG,
		long_long, long,
		LONG_MIN, LONG_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_llong_ulong
 *
 * Purpose:	Converts `long_long' to `unsigned long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_llong_ulong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_llong_ulong, FAIL);
    H5T_CONV_Su(LLONG, ULONG,
		long_long, unsigned long,
		ULONG_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ullong_long
 *
 * Purpose:	Converts `unsigned long_long' to `long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ullong_long(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		     size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_ullong_long, FAIL);
    H5T_CONV_Us(ULLONG, LONG,
		unsigned long_long, long,
		LONG_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ullong_ulong
 *
 * Purpose:	Converts `unsigned long_long' to `unsigned long'
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, November 13, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ullong_ulong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_ullong_ulong, FAIL);
    H5T_CONV_Uu(ULLONG, ULONG,
		unsigned long_long, unsigned long,
		ULONG_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_llong_ullong
 *
 * Purpose:	Converts `long_long' to `unsigned long_long'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Monday, November 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_llong_ullong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_llong_ullong, FAIL);
    H5T_CONV_su(LLONG, ULLONG,
		long_long, unsigned long_long);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_ullong_llong
 *
 * Purpose:	Converts `unsigned long_long' to `long_long'
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Monday, November 16, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_ullong_llong(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, void *buf, void __unused__ *bkg)
{
    FUNC_ENTER(H5T_conv_ullong_llong, FAIL);
    H5T_CONV_us(ULLONG, LLONG,
		unsigned long_long, long_long,
		LLONG_MAX);
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_float_double
 *
 * Purpose:	Convert native `float' to native `double' using hardware.
 *		This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Tuesday, June 23, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_float_double (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, void *buf, void __unused__ *bkg)
{
    size_t	elmtno;			/*element number		*/
    float	*src, *s;		/*source buffer			*/
    double	*dst, *d;		/*destination buffer		*/
    H5T_t	*st, *dt;		/*type descriptors		*/
    hbool_t	src_mv, dst_mv;		/*align data?			*/
    double	aligned;		/*aligned data			*/
    
    FUNC_ENTER (H5T_conv_float_double, FAIL);

    switch (cdata->command) {
    case H5T_CONV_INIT:
	cdata->need_bkg = H5T_BKG_NO;
	break;

    case H5T_CONV_FREE:
	break;

    case H5T_CONV_CONV:
	src = (float*)buf + nelmts-1;
	dst = (double*)buf + nelmts-1;
	st = H5I_object(src_id);
	dt = H5I_object(dst_id);
	assert(st && dt);

	/* Need alignment? */
	if (H5T_NATIVE_FLOAT_ALIGN_g>1) {
	    src_mv = ((size_t)buf % H5T_NATIVE_FLOAT_ALIGN_g) ||
		     (st->size % H5T_NATIVE_FLOAT_ALIGN_g);
	} else {
	    src_mv = FALSE;
	}
	if (H5T_NATIVE_DOUBLE_ALIGN_g>1) {
	    dst_mv = ((size_t)buf % H5T_NATIVE_DOUBLE_ALIGN_g) ||
		     (dt->size % H5T_NATIVE_DOUBLE_ALIGN_g);
	} else {
	    dst_mv = FALSE;
	}
	CI_DEBUG(src_mv, FLOAT, float);
	CI_DEBUG(dst_mv, DOUBLE, double);
	
	for (elmtno=0; elmtno<nelmts; elmtno++, --src, --dst) {
	    /* Align source and/or destination */
	    if (src_mv) {
		memcpy(&aligned, src, st->size);
		s = (float*)&aligned;
	    } else {
		s = src;
	    }
	    if (dst_mv) d = (double*)&aligned;
	    else d = dst;

	    /* Conversion */
	    *d = *s;

	    /* Unalign destination */
	    if (dst_mv) memcpy(dst, &aligned, dt->size);
	}
	break;
	    
    default:
	HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
		       "unknown conversion command");
    }

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_double_float
 *
 * Purpose:	Convert native `double' to native `float' using hardware.
 *		This is a fast special case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Tuesday, June 23, 1998
 *
 * Modifications:
 *
 *	Robb Matzke, 7 Jul 1998
 *	Added overflow handling.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_double_float (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		       size_t nelmts, void *buf, void __unused__ *bkg)
{
    size_t	elmtno;			/*element number		*/
    double	*src, *s;		/*source buffer			*/
    float	*dst, *d;		/*destination buffer		*/
    H5T_t	*st, *dt;		/*type descriptors		*/
    hbool_t	src_mv, dst_mv;		/*align data?			*/
    double	aligned;		/*aligned data			*/
    
    FUNC_ENTER (H5T_conv_double_float, FAIL);

    switch (cdata->command) {
    case H5T_CONV_INIT:
	cdata->need_bkg = H5T_BKG_NO;
	break;

    case H5T_CONV_FREE:
	break;

    case H5T_CONV_CONV:
	src = (double*)buf;
	dst = (float*)buf;
	st = H5I_object(src_id);
	dt = H5I_object(dst_id);
	assert(st && dt);

	/* Need alignment? */
	if (H5T_NATIVE_DOUBLE_ALIGN_g>1) {
	    src_mv = ((size_t)buf % H5T_NATIVE_DOUBLE_ALIGN_g) ||
		     (st->size % H5T_NATIVE_DOUBLE_ALIGN_g);
	} else {
	    src_mv = FALSE;
	}
	if (H5T_NATIVE_FLOAT_ALIGN_g>1) {
	    dst_mv = ((size_t)buf % H5T_NATIVE_FLOAT_ALIGN_g) ||
		     (dt->size % H5T_NATIVE_FLOAT_ALIGN_g);
	} else {
	    dst_mv = FALSE;
	}
	CI_DEBUG(src_mv, DOUBLE, double);
	CI_DEBUG(dst_mv, FLOAT, float);
	
	for (elmtno=0; elmtno<nelmts; elmtno++, src++, dst++) {
	    /* Align source and/or destination */
	    if (src_mv) {
		memcpy(&aligned, src, st->size);
		s = (double*)&aligned;
	    } else {
		s = src;
	    }
	    if (dst_mv) d = (float*)&aligned;
	    else d = dst;

	    /* Conversion */
	    if (*s > FLT_MAX) {
		if (!H5T_overflow_g ||
		    (H5T_overflow_g)(src_id, dst_id, s, d)<0) {
		    *d = HUGE_VAL;
		}
	    } else if (*s < -FLT_MAX) {
		if (!H5T_overflow_g ||
		    (H5T_overflow_g)(src_id, dst_id, s, d)<0) {
		    *d = -HUGE_VAL;
		}
	    } else {
		*d = *s;
	    }

	    /* Unalign destination */
	    if (dst_mv) memcpy(dst, &aligned, dt->size);
	}
	break;
	    
    default:
	HRETURN_ERROR (H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
		       "unknown conversion command");
    }

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5T_conv_i32le_f64le
 *
 * Purpose:	Converts 4-byte little-endian integers (signed or unsigned)
 *		to 8-byte litte-endian IEEE floating point.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *
 * Programmer:	Robb Matzke
 *		Wednesday, June 10, 1998
 *
 * Modifications:
 * 
 *-------------------------------------------------------------------------
 */
herr_t
H5T_conv_i32le_f64le (hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata,
		      size_t nelmts, void *buf, void __unused__ *bkg)
{
    uint8_t	*s=NULL, *d=NULL;	/*src and dst buf pointers	*/
    uint8_t	tmp[8];			/*temporary destination buffer	*/
    H5T_t	*src = NULL;		/*source data type		*/
    size_t	elmtno;			/*element counter		*/
    uintn	sign;			/*sign bit			*/
    uintn	cin, cout;		/*carry in/out			*/
    uintn	mbits=0;		/*mantissa bits			*/
    uintn	exponent;		/*exponent			*/
    intn	i;			/*counter			*/

    FUNC_ENTER (H5T_conv_i32le_f64le, FAIL);

    switch (cdata->command) {
    case H5T_CONV_INIT:
	assert (sizeof(intn)>=4);
	cdata->need_bkg = H5T_BKG_NO;
	break;

    case H5T_CONV_FREE:
	/* Free private data */
	break;

    case H5T_CONV_CONV:
	/* The conversion */
	if (H5I_DATATYPE!=H5I_get_type (src_id) ||
	    NULL==(src=H5I_object (src_id)) ||
	    H5I_DATATYPE!=H5I_get_type (dst_id) ||
	    NULL==H5I_object (dst_id)) {
	    HRETURN_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a data type");
	}
	
	s = (uint8_t*)buf + 4*(nelmts-1);
	d = (uint8_t*)buf + 8*(nelmts-1);
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
		d[6] |=	 0x0f	 & (s[3]>>2) ;
		d[5] = (s[3]<<6) | (s[2]>>2) ;
		d[4] = (s[2]<<6) | (s[1]>>2) ;
		d[3] = (s[1]<<6) | (s[0]>>2) ;
		d[2] = (s[0]<<6) ;
		d[1] = d[0] = 0 ;
		break ;
	    case 30:
		d[6] |=	 0x0f	 & (s[3]>>1) ;
		d[5] = (s[3]<<7) | (s[2]>>1) ;
		d[4] = (s[2]<<7) | (s[1]>>1) ;
		d[3] = (s[1]<<7) | (s[0]>>1) ;
		d[2] = (s[0]<<7) ;
		d[1] = d[0] = 0 ;
		break ;
	    case 29:
		d[6] |=	 0x0f	 & s[3] ;
		d[5] = s[2] ;
		d[4] = s[1] ;
		d[3] = s[0] ;
		d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 28:
		d[6] |= ((s[3]<<1) | (s[2]>>7)) & 0x0f ;
		d[5] =	 (s[2]<<1) | (s[1]>>7) ;
		d[4] =	 (s[1]<<1) | (s[0]>>7) ;
		d[3] =	 (s[0]<<1) ;
		d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 27:
		d[6] |= ((s[3]<<2) | (s[2]>>6)) & 0x0f ;
		d[5] =	 (s[2]<<2) | (s[1]>>6) ;
		d[4] =	 (s[1]<<2) | (s[0]>>6) ;
		d[3] =	 (s[0]<<2) ;
		d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 26:
		d[6] |= ((s[3]<<3) | (s[2]>>5)) & 0x0f ;
		d[5] =	 (s[2]<<3) | (s[1]>>5) ;
		d[4] =	 (s[1]<<3) | (s[0]>>5) ;
		d[3] =	 (s[0]<<3) ;
		d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 25:
		d[6] |=	  0x0f	 & (s[2]>>4) ;
		d[5] = (s[2]<<4) | (s[1]>>4) ;
		d[4] = (s[1]<<4) | (s[0]>>4) ;
		d[3] = (s[0]<<4) ;
		d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 24:
		d[6] |=	  0x0f	 & (s[2]>>3) ;
		d[5] = (s[2]<<5) | (s[1]>>3) ;
		d[4] = (s[1]<<5) | (s[0]>>3) ;
		d[3] = (s[0]<<5) ;
		d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 23:
		d[6] |=	  0x0f	 & (s[2]>>2) ;
		d[5] = (s[2]<<6) | (s[1]>>2) ;
		d[4] = (s[1]<<6) | (s[0]>>2) ;
		d[3] = (s[0]<<6) ;
		d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 22:
		d[6] |=	  0x0f	 & (s[2]>>1) ;
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
		d[5] =	 (s[1]<<1) | (s[0]>>7) ;
		d[4] =	 (s[0]<<1) ;
		d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 19:
		d[6] |= ((s[2]<<2) | (s[1]>>6)) & 0x0f ;
		d[5] =	 (s[1]<<2) | (s[0]>>6) ;
		d[4] =	 (s[0]<<2) ;
		d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 18:
		d[6] |= ((s[2]<<3) | (s[1]>>5)) & 0x0f ;
		d[5] =	 (s[1]<<3) | (s[0]>>5) ;
		d[4] =	 (s[0]<<3) ;
		d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 17:
		d[6] |=	  0x0f	 & (s[1]>>4) ;
		d[5] = (s[1]<<4) | (s[0]>>4) ;
		d[4] = (s[0]<<4) ;
		d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 16:
		d[6] |=	  0x0f	 & (s[1]>>3) ;
		d[5] = (s[1]<<5) | (s[0]>>3) ;
		d[4] = (s[0]<<5) ;
		d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 15:
		d[6] |=	  0x0f	 & (s[1]>>2) ;
		d[5] = (s[1]<<6) | (s[0]>>2) ;
		d[4] = (s[0]<<6) ;
		d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 14:
		d[6] |=	  0x0f	 & (s[1]>>1) ;
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
		d[5] =	 (s[0]<<1) ;
		d[4] = d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 11:
		d[6] |= ((s[1]<<2) | (s[0]>>6)) & 0x0f ;
		d[5] =	 (s[0]<<2) ;
		d[4] = d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 10:
		d[6] |= ((s[1]<<3) | (s[0]>>5)) & 0x0f ;
		d[5] =	 (s[0]<<3) ;
		d[4] = d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 9:
		d[6] |=	  0x0f	 & (s[0]>>4) ;
		d[5] = (s[0]<<4) ;
		d[4] = d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 8:
		d[6] |=	  0x0f	 & (s[0]>>3) ;
		d[5] = (s[0]<<5) ;
		d[4] = d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 7:
		d[6] |=	  0x0f	 & (s[0]>>2) ;
		d[5] = (s[0]<<6) ;
		d[4] = d[3] = d[2] = d[1] = d[0] = 0 ;
		break ;
	    case 6:
		d[6] |=	  0x0f	 & (s[0]>>1) ;
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
