/****************************************************************************
* NCSA HDF								   *
* Software Development Group						   *
* National Center for Supercomputing Applications			   *
* University of Illinois at Urbana-Champaign				   *
* 605 E. Springfield, Champaign IL 61820				   *
*									   *
* For conditions of distribution and use, see the accompanying		   *
* hdf/COPYING file.							   *
*									   *
****************************************************************************/

#ifdef RCSID
static char		RcsId[] = "@(#)$Revision$";
#endif

/* $Id$ */

#define H5T_PACKAGE		/*prevent warning from including H5Tpkg.h */

#include <H5private.h>
#include <H5Eprivate.h>
#include <H5Gprivate.h>
#include <H5MMprivate.h>
#include <H5Oprivate.h>
#include <H5Tpkg.h>

#define PABLO_MASK	H5O_dtype_mask

/* PRIVATE PROTOTYPES */
static herr_t H5O_dtype_encode (H5F_t *f, uint8_t *p, const void *mesg);
static void *H5O_dtype_decode (H5F_t *f, const uint8_t *p, H5O_shared_t *sh);
static void *H5O_dtype_copy (const void *_mesg, void *_dest);
static size_t H5O_dtype_size (H5F_t *f, const void *_mesg);
static herr_t H5O_dtype_reset (void *_mesg);
static herr_t H5O_dtype_get_share (H5F_t *f, const void *_mesg,
				   H5O_shared_t *sh);
static herr_t H5O_dtype_set_share (H5F_t *f, void *_mesg,
				   const H5O_shared_t *sh);
static herr_t H5O_dtype_debug (H5F_t *f, const void *_mesg,
			       FILE * stream, intn indent, intn fwidth);

/* This message derives from H5O */
const H5O_class_t H5O_DTYPE[1] = {{
    H5O_DTYPE_ID,		/* message id number		*/
    "data_type",		/* message name for debugging	*/
    sizeof(H5T_t),		/* native message size		*/
    H5O_dtype_decode,		/* decode message		*/
    H5O_dtype_encode,		/* encode message		*/
    H5O_dtype_copy,		/* copy the native value	*/
    H5O_dtype_size,		/* size of raw message		*/
    H5O_dtype_reset,		/* reset method			*/
    H5O_dtype_get_share,	/* get share method		*/
    H5O_dtype_set_share,	/* set share method		*/
    H5O_dtype_debug,		/* debug the message		*/
}};

#define H5O_DTYPE_VERSION	1

/* Interface initialization */
static intn		interface_initialize_g = 0;
#define INTERFACE_INIT	NULL

/*-------------------------------------------------------------------------
 * Function:	H5O_dtype_decode_helper
 *
 * Purpose:	Decodes a data type
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Monday, December  8, 1997
 *
 * Modifications:
 *		Robb Matzke, Thursday, May 20, 1999
 *		Added support for bitfields and opaque data types.
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_dtype_decode_helper(H5F_t *f, const uint8_t **pp, H5T_t *dt)
{
    uintn		flags, perm_word, version;
    intn		i, j;
    size_t		z;

    FUNC_ENTER(H5O_dtype_decode_helper, FAIL);

    /* check args */
    assert(pp && *pp);
    assert(dt);

    /* decode */
    UINT32DECODE(*pp, flags);
    version = (flags>>4) & 0x0f;
    if (version!=H5O_DTYPE_VERSION) {
	HRETURN_ERROR(H5E_DATATYPE, H5E_CANTLOAD, FAIL,
		      "bad version number for data type message");
    }
    dt->type = (H5T_class_t)(flags & 0x0f);
    flags >>= 8;
    UINT32DECODE(*pp, dt->size);

    switch (dt->type) {
    case H5T_INTEGER:
	/*
	 * Integer types...
	 */
	dt->u.atomic.order = (flags & 0x1) ? H5T_ORDER_BE : H5T_ORDER_LE;
	dt->u.atomic.lsb_pad = (flags & 0x2) ? H5T_PAD_ONE : H5T_PAD_ZERO;
	dt->u.atomic.msb_pad = (flags & 0x4) ? H5T_PAD_ONE : H5T_PAD_ZERO;
	dt->u.atomic.u.i.sign = (flags & 0x8) ? H5T_SGN_2 : H5T_SGN_NONE;
	UINT16DECODE(*pp, dt->u.atomic.offset);
	UINT16DECODE(*pp, dt->u.atomic.prec);
	break;

    case H5T_BITFIELD:
	/*
         * Bit fields...
         */
	dt->u.atomic.order = (flags & 0x1) ? H5T_ORDER_BE : H5T_ORDER_LE;
	dt->u.atomic.lsb_pad = (flags & 0x2) ? H5T_PAD_ONE : H5T_PAD_ZERO;
	dt->u.atomic.msb_pad = (flags & 0x4) ? H5T_PAD_ONE : H5T_PAD_ZERO;
	UINT16DECODE(*pp, dt->u.atomic.offset);
	UINT16DECODE(*pp, dt->u.atomic.prec);
	break;

    case H5T_OPAQUE:
	/*
         * Opaque types...
         */
	z = flags & 0xff;
	assert(0==(z&0x7)); /*must be aligned*/
	if (NULL==(dt->u.opaque.tag=H5MM_malloc(z+1))) {
	    HRETURN_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
			  "memory allocation failed");
	}
	HDmemcpy(dt->u.opaque.tag, *pp, z);
	dt->u.opaque.tag[z] = '\0';
	*pp += z;
	break;

    case H5T_STRING:
	/*
	 * Character string types...
	 */
	dt->u.atomic.order = H5T_ORDER_NONE;
	dt->u.atomic.prec = 8 * dt->size;
	dt->u.atomic.offset = 0;
	dt->u.atomic.lsb_pad = H5T_PAD_ZERO;
	dt->u.atomic.msb_pad = H5T_PAD_ZERO;

	dt->u.atomic.u.s.pad = (H5T_str_t)(flags & 0x0f);
	dt->u.atomic.u.s.cset = (H5T_cset_t)((flags>>4) & 0x0f);
	break;

    case H5T_FLOAT:
	/*
	 * Floating-point types...
	 */
	dt->u.atomic.order = (flags & 0x1) ? H5T_ORDER_BE : H5T_ORDER_LE;
	dt->u.atomic.lsb_pad = (flags & 0x2) ? H5T_PAD_ONE : H5T_PAD_ZERO;
	dt->u.atomic.msb_pad = (flags & 0x4) ? H5T_PAD_ONE : H5T_PAD_ZERO;
	dt->u.atomic.u.f.pad = (flags & 0x8) ? H5T_PAD_ONE : H5T_PAD_ZERO;
	switch ((flags >> 4) & 0x03) {
	case 0:
	    dt->u.atomic.u.f.norm = H5T_NORM_NONE;
	    break;
	case 1:
	    dt->u.atomic.u.f.norm = H5T_NORM_MSBSET;
	    break;
	case 2:
	    dt->u.atomic.u.f.norm = H5T_NORM_IMPLIED;
	    break;
	default:
	    HRETURN_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			  "unknown floating-point normalization");
	}
	dt->u.atomic.u.f.sign = (flags >> 8) & 0xff;
	UINT16DECODE(*pp, dt->u.atomic.offset);
	UINT16DECODE(*pp, dt->u.atomic.prec);
	dt->u.atomic.u.f.epos = *(*pp)++;
	dt->u.atomic.u.f.esize = *(*pp)++;
	assert(dt->u.atomic.u.f.esize > 0);
	dt->u.atomic.u.f.mpos = *(*pp)++;
	dt->u.atomic.u.f.msize = *(*pp)++;
	assert(dt->u.atomic.u.f.msize > 0);
	UINT32DECODE(*pp, dt->u.atomic.u.f.ebias);
	break;

    case H5T_COMPOUND:
	/*
	 * Compound data types...
	 */
	dt->u.compnd.nmembs = flags & 0xffff;
	assert(dt->u.compnd.nmembs > 0);
	dt->u.compnd.nalloc = dt->u.compnd.nmembs;
	dt->u.compnd.memb = H5MM_calloc(dt->u.compnd.nalloc*
					sizeof(H5T_cmemb_t));
	if (NULL==dt->u.compnd.memb) {
	    HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,
			   "memory allocation failed");
	}
	for (i = 0; i < dt->u.compnd.nmembs; i++) {
	    dt->u.compnd.memb[i].name = H5MM_xstrdup((const char *)*pp);
	    /*multiple of 8 w/ null terminator */
	    *pp += ((HDstrlen((const char *)*pp) + 8) / 8) * 8;
	    UINT32DECODE(*pp, dt->u.compnd.memb[i].offset);
	    dt->u.compnd.memb[i].ndims = *(*pp)++;
	    assert(dt->u.compnd.memb[i].ndims <= 4);
	    *pp += 3;		/*reserved bytes */

	    /* Dimension permutation */
	    UINT32DECODE(*pp, perm_word);
	    dt->u.compnd.memb[i].perm[0] = (perm_word >> 0) & 0xff;
	    dt->u.compnd.memb[i].perm[1] = (perm_word >> 8) & 0xff;
	    dt->u.compnd.memb[i].perm[2] = (perm_word >> 16) & 0xff;
	    dt->u.compnd.memb[i].perm[3] = (perm_word >> 24) & 0xff;
	    dt->u.compnd.memb[i].type = H5MM_calloc (sizeof(H5T_t));

	    /* Reserved */
	    *pp += 4;
	    
	    /* Dimension sizes */
	    for (j=0; j<4; j++) {
		UINT32DECODE(*pp, dt->u.compnd.memb[i].dim[j]);
	    }
	    if (NULL==dt->u.compnd.memb[i].type) {
		HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,
			       "memory allocation failed");
	    }
	    H5F_addr_undef (&(dt->u.compnd.memb[i].type->ent.header));
	    if (H5O_dtype_decode_helper(f, pp, dt->u.compnd.memb[i].type)<0) {
		for (j=0; j<=i; j++) H5MM_xfree(dt->u.compnd.memb[j].name);
		H5MM_xfree(dt->u.compnd.memb);
		HRETURN_ERROR(H5E_DATATYPE, H5E_CANTDECODE, FAIL,
			      "unable to decode member type");
	    }

        /*
         * Set the "force conversion" flag if VL datatype fields exist in this
         * type or any component types
         */
        if(dt->u.compnd.memb[i].type->type==H5T_VLEN || dt->u.compnd.memb[i].type->force_conv==TRUE)
            dt->force_conv=TRUE;
	    
	    /* Total member size */
	    dt->u.compnd.memb[i].size = dt->u.compnd.memb[i].type->size;
	    for (j=0; j<dt->u.compnd.memb[i].ndims; j++) {
		dt->u.compnd.memb[i].size *= dt->u.compnd.memb[i].dim[j];
	    }
	}
	break;

    case H5T_ENUM:
	/*
	 * Enumeration data types...
	 */
	dt->u.enumer.nmembs = dt->u.enumer.nalloc = flags & 0xffff;
	assert(dt->u.enumer.nmembs>=0);
	if (NULL==(dt->parent=H5MM_calloc(sizeof(H5T_t)))) {
	    HRETURN_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
			  "memory allocation failed");
	}
	H5F_addr_undef(&(dt->parent->ent.header));
	if (H5O_dtype_decode_helper(f, pp, dt->parent)<0) {
	    HRETURN_ERROR(H5E_DATATYPE, H5E_CANTDECODE, FAIL,
			  "unable to decode parent data type");
	}
	if (NULL==(dt->u.enumer.name=H5MM_calloc(dt->u.enumer.nalloc *
						 sizeof(char*))) ||
	    NULL==(dt->u.enumer.value=H5MM_calloc(dt->u.enumer.nalloc *
						  dt->parent->size))) {
	    HRETURN_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
			  "memory allocation failed");
	}

	/* Names, each a multiple of 8 with null termination */
	for (i=0; i<dt->u.enumer.nmembs; i++) {
	    dt->u.enumer.name[i] = H5MM_xstrdup((const char*)*pp);
	    *pp += ((HDstrlen((const char*)*pp)+8)/8)*8;
	}

	/* Values */
	HDmemcpy(dt->u.enumer.value, *pp,
		 dt->u.enumer.nmembs * dt->parent->size);
	*pp += dt->u.enumer.nmembs * dt->parent->size;
	break;

    case H5T_REFERENCE:
	dt->u.atomic.order = H5T_ORDER_NONE;
	dt->u.atomic.prec = 8 * dt->size;
	dt->u.atomic.offset = 0;
	dt->u.atomic.lsb_pad = H5T_PAD_ZERO;
	dt->u.atomic.msb_pad = H5T_PAD_ZERO;
	dt->u.atomic.u.r.rtype = (H5R_type_t)(flags & 0x0f);
	break;

    case H5T_VLEN:  /* Variable length datatypes...  */
        /* Decode base type of VL information */
        H5F_addr_undef(&(dt->parent->ent.header));
	    if (H5O_dtype_decode_helper(f, pp, dt->parent)<0) {
            HRETURN_ERROR(H5E_DATATYPE, H5E_CANTDECODE, FAIL, "unable to decode VL parent type");
        }

        dt->force_conv=TRUE;
        /* Mark this type as on disk */
	    if (H5T_vlen_mark(dt, f, H5T_VLEN_DISK)<0) {
            HRETURN_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "invalid VL location");
        }
        break;

    default:
	if (flags) {
	    HRETURN_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			  "class flags are non-zero");
	}
	break;
    }

    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_dtype_encode_helper
 *
 * Purpose:	Encodes a data type.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Monday, December  8, 1997
 *
 * Modifications:
 *		Robb Matzke, Thursday, May 20, 1999
 *		Added support for bitfields and opaque types.
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_dtype_encode_helper(uint8_t **pp, const H5T_t *dt)
{
    uintn		flags = 0;
    uintn		perm_word;
    char		*hdr = (char *)*pp;
    intn		i, j;
    size_t		n, z, aligned;

    FUNC_ENTER(H5O_dtype_encode_helper, FAIL);

    /* check args */
    assert(pp && *pp);
    assert(dt);

    /* skip the type and class bit field for now */
    *pp += 4;
    UINT32ENCODE(*pp, dt->size);

    switch (dt->type) {
    case H5T_INTEGER:
	/*
	 * Integer data types...
	 */
	switch (dt->u.atomic.order) {
	case H5T_ORDER_LE:
	    break;		/*nothing */
	case H5T_ORDER_BE:
	    flags |= 0x01;
	    break;
	default:
	    HRETURN_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			  "byte order is not supported in file format yet");
	}

	switch (dt->u.atomic.lsb_pad) {
	case H5T_PAD_ZERO:
	    break;		/*nothing */
	case H5T_PAD_ONE:
	    flags |= 0x02;
	    break;
	default:
	    HRETURN_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			  "bit padding is not supported in file format yet");
	}

	switch (dt->u.atomic.msb_pad) {
	case H5T_PAD_ZERO:
	    break;		/*nothing */
	case H5T_PAD_ONE:
	    flags |= 0x04;
	    break;
	default:
	    HRETURN_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			  "bit padding is not supported in file format yet");
	}

	switch (dt->u.atomic.u.i.sign) {
	case H5T_SGN_NONE:
	    break;		/*nothing */
	case H5T_SGN_2:
	    flags |= 0x08;
	    break;
	default:
	    HRETURN_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			  "sign scheme is not supported in file format yet");
	}

	UINT16ENCODE(*pp, dt->u.atomic.offset);
	UINT16ENCODE(*pp, dt->u.atomic.prec);
	break;

    case H5T_BITFIELD:
	/*
	 * Bitfield data types...
	 */
	switch (dt->u.atomic.order) {
	case H5T_ORDER_LE:
	    break;		/*nothing */
	case H5T_ORDER_BE:
	    flags |= 0x01;
	    break;
	default:
	    HRETURN_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			  "byte order is not supported in file format yet");
	}

	switch (dt->u.atomic.lsb_pad) {
	case H5T_PAD_ZERO:
	    break;		/*nothing */
	case H5T_PAD_ONE:
	    flags |= 0x02;
	    break;
	default:
	    HRETURN_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			  "bit padding is not supported in file format yet");
	}

	switch (dt->u.atomic.msb_pad) {
	case H5T_PAD_ZERO:
	    break;		/*nothing */
	case H5T_PAD_ONE:
	    flags |= 0x04;
	    break;
	default:
	    HRETURN_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			  "bit padding is not supported in file format yet");
	}

	UINT16ENCODE(*pp, dt->u.atomic.offset);
	UINT16ENCODE(*pp, dt->u.atomic.prec);
	break;

    case H5T_OPAQUE:
	/*
         * Opaque data types...  The tag is stored in a field which is a
         * multiple of eight characters and null padded (not necessarily
         * null terminated).
         */
	z = HDstrlen(dt->u.opaque.tag);
	aligned = (z+7) & 0xf8;
	flags |= aligned;
	HDmemcpy(*pp, dt->u.opaque.tag, MIN(z,aligned));
	for (n=MIN(z,aligned); n<aligned; n++) (*pp)[n] = 0;
	*pp += aligned;
	break;

    case H5T_STRING:
	/*
	 * Character string types... (not fully implemented)
	 */
	assert (dt->u.atomic.order == H5T_ORDER_NONE);
	assert (dt->u.atomic.prec == 8 * dt->size);
	assert (dt->u.atomic.offset == 0);
	assert (dt->u.atomic.lsb_pad == H5T_PAD_ZERO);
	assert (dt->u.atomic.msb_pad == H5T_PAD_ZERO);

	flags |= (dt->u.atomic.u.s.pad & 0x0f);
	flags |= (dt->u.atomic.u.s.cset & 0x0f) << 4;
	break;

    case H5T_FLOAT:
	/*
	 * Floating-point types...
	 */
	switch (dt->u.atomic.order) {
	case H5T_ORDER_LE:
	    break;		/*nothing */
	case H5T_ORDER_BE:
	    flags |= 0x01;
	    break;
	default:
	    HRETURN_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			  "byte order is not supported in file format yet");
	}

	switch (dt->u.atomic.lsb_pad) {
	case H5T_PAD_ZERO:
	    break;		/*nothing */
	case H5T_PAD_ONE:
	    flags |= 0x02;
	    break;
	default:
	    HRETURN_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			  "bit padding is not supported in file format yet");
	}

	switch (dt->u.atomic.msb_pad) {
	case H5T_PAD_ZERO:
	    break;		/*nothing */
	case H5T_PAD_ONE:
	    flags |= 0x04;
	    break;
	default:
	    HRETURN_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			  "bit padding is not supported in file format yet");
	}

	switch (dt->u.atomic.u.f.pad) {
	case H5T_PAD_ZERO:
	    break;		/*nothing */
	case H5T_PAD_ONE:
	    flags |= 0x08;
	    break;
	default:
	    HRETURN_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			  "bit padding is not supported in file format yet");
	}

	switch (dt->u.atomic.u.f.norm) {
	case H5T_NORM_NONE:
	    break;		/*nothing */
	case H5T_NORM_MSBSET:
	    flags |= 0x10;
	    break;
	case H5T_NORM_IMPLIED:
	    flags |= 0x20;
	    break;
	default:
	    HRETURN_ERROR(H5E_DATATYPE, H5E_UNSUPPORTED, FAIL,
			  "normalization scheme is not supported in file "
			  "format yet");
	}

	flags |= (dt->u.atomic.u.f.sign << 8) & 0xff00;
	UINT16ENCODE(*pp, dt->u.atomic.offset);
	UINT16ENCODE(*pp, dt->u.atomic.prec);
	assert (dt->u.atomic.u.f.epos<=255);
	*(*pp)++ = (uint8_t)(dt->u.atomic.u.f.epos);
	assert (dt->u.atomic.u.f.esize<=255);
	*(*pp)++ = (uint8_t)(dt->u.atomic.u.f.esize);
	assert (dt->u.atomic.u.f.mpos<=255);
	*(*pp)++ = (uint8_t)(dt->u.atomic.u.f.mpos);
	assert (dt->u.atomic.u.f.msize<=255);
	*(*pp)++ = (uint8_t)(dt->u.atomic.u.f.msize);
	UINT32ENCODE(*pp, dt->u.atomic.u.f.ebias);
	break;

    case H5T_COMPOUND:
	/*
	 * Compound data types...
	 */
	flags = dt->u.compnd.nmembs & 0xffff;
	for (i=0; i<dt->u.compnd.nmembs; i++) {
	    /* Name, multiple of eight bytes */
	    HDstrcpy((char*)(*pp), dt->u.compnd.memb[i].name);
	    n = HDstrlen(dt->u.compnd.memb[i].name);
	    for (z=n+1; z%8; z++) (*pp)[z] = '\0';
	    *pp += z;

	    /* Member offset */
	    UINT32ENCODE(*pp, dt->u.compnd.memb[i].offset);

	    /* Dimensionality */
	    *(*pp)++ = dt->u.compnd.memb[i].ndims;
	    assert(dt->u.compnd.memb[i].ndims <= 4);

	    /* Reserved */
	    *(*pp)++ = '\0';
	    *(*pp)++ = '\0';
	    *(*pp)++ = '\0';

	    /* Dimension permutation */
	    for (j = 0, perm_word = 0; j < dt->u.compnd.memb[i].ndims; j++) {
		perm_word |= dt->u.compnd.memb[i].perm[j] << (8 * j);
	    }
	    UINT32ENCODE(*pp, perm_word);

	    /* Reserved */
	    UINT32ENCODE(*pp, 0);

	    /* Dimensions */
	    for (j=0; j<dt->u.compnd.memb[i].ndims; j++) {
		UINT32ENCODE(*pp, dt->u.compnd.memb[i].dim[j]);
	    }
	    for (/*void*/; j<4; j++) {
		UINT32ENCODE(*pp, 0);
	    }

	    /* Subtype */
	    if (H5O_dtype_encode_helper(pp, dt->u.compnd.memb[i].type)<0) {
		HRETURN_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL,
			      "unable to encode member type");
	    }
	}
	break;

    case H5T_ENUM:
        /*
         * Enumeration data types...
         */
        flags = dt->u.enumer.nmembs & 0xffff;

        /* Parent type */
        if (H5O_dtype_encode_helper(pp, dt->parent)<0) {
            HRETURN_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL,
                  "unable to encode parent data type");
        }
        
        /* Names, each a multiple of eight bytes */
        for (i=0; i<dt->u.enumer.nmembs; i++) {
            HDstrcpy((char*)(*pp), dt->u.enumer.name[i]);
            n = HDstrlen(dt->u.enumer.name[i]);
            for (z=n+1; z%8; z++) (*pp)[z] = '\0';
            *pp += z;
        }

        /* Values */
        HDmemcpy(*pp, dt->u.enumer.value, dt->u.enumer.nmembs * dt->parent->size);
        *pp += dt->u.enumer.nmembs * dt->parent->size;
        break;
	
    case H5T_REFERENCE:
        flags |= (dt->u.atomic.u.r.rtype & 0x0f);
        break;
        
    case H5T_VLEN:  /* Variable length datatypes...  */
        /* Encode base type of VL information */
        if (H5O_dtype_encode_helper(pp, dt->parent)<0) {
            HRETURN_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL, "unable to encode VL parent type");
        }
        break;

    default:
        /*nothing */
        break;
    }

    *hdr++ = ((uintn)(dt->type) & 0x0f) | (H5O_DTYPE_VERSION<<4);
    *hdr++ = (flags >> 0) & 0xff;
    *hdr++ = (flags >> 8) & 0xff;
    *hdr++ = (flags >> 16) & 0xff;

    FUNC_LEAVE(SUCCEED);
}

/*--------------------------------------------------------------------------
 NAME
    H5O_dtype_decode
 PURPOSE
    Decode a datatype message and return a pointer to a memory struct
	with the decoded information
 USAGE
    void *H5O_dtype_decode(f, raw_size, p)
	H5F_t *f;		IN: pointer to the HDF5 file struct
	size_t raw_size;	IN: size of the raw information buffer
	const uint8 *p;		IN: the raw information buffer
 RETURNS
    Pointer to the new message in native order on success, NULL on failure
 DESCRIPTION
	This function decodes the "raw" disk form of a simple datatype message
    into a struct in memory native format.  The struct is allocated within this
    function using malloc() and is returned to the caller.
--------------------------------------------------------------------------*/
static void *
H5O_dtype_decode(H5F_t *f, const uint8_t *p,
		 H5O_shared_t UNUSED *sh)
{
    H5T_t		   *dt = NULL;

    FUNC_ENTER(H5O_dtype_decode, NULL);

    /* check args */
    assert(p);

    if (NULL==(dt = H5MM_calloc(sizeof(H5T_t)))) {
	HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL,
		       "memory allocation failed");
    }
    H5F_addr_undef (&(dt->ent.header));

    if (H5O_dtype_decode_helper(f, &p, dt) < 0) {
	H5MM_xfree(dt);
	HRETURN_ERROR(H5E_DATATYPE, H5E_CANTDECODE, NULL,
		      "can't decode type");
    }
    FUNC_LEAVE(dt);
}

/*--------------------------------------------------------------------------
 NAME
    H5O_dtype_encode
 PURPOSE
    Encode a simple datatype message 
 USAGE
    herr_t H5O_dtype_encode(f, raw_size, p, mesg)
	H5F_t *f;	  IN: pointer to the HDF5 file struct
	size_t raw_size;	IN: size of the raw information buffer
	const uint8 *p;		IN: the raw information buffer
	const void *mesg;	IN: Pointer to the simple datatype struct
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
	This function encodes the native memory form of the simple datatype
    message in the "raw" disk form.
--------------------------------------------------------------------------*/
static herr_t
H5O_dtype_encode(H5F_t UNUSED *f, uint8_t *p, const void *mesg)
{
    const H5T_t		   *dt = (const H5T_t *) mesg;

    FUNC_ENTER(H5O_dtype_encode, FAIL);

    /* check args */
    assert(f);
    assert(p);
    assert(dt);

    /* encode */
    if (H5O_dtype_encode_helper(&p, dt) < 0) {
	HRETURN_ERROR(H5E_DATATYPE, H5E_CANTENCODE, FAIL,
		      "can't encode type");
    }
    FUNC_LEAVE(SUCCEED);
}

/*--------------------------------------------------------------------------
 NAME
    H5O_dtype_copy
 PURPOSE
    Copies a message from MESG to DEST, allocating DEST if necessary.
 USAGE
    void *H5O_dtype_copy(mesg, dest)
	const void *mesg;	IN: Pointer to the source simple datatype
				    struct 
	const void *dest;	IN: Pointer to the destination simple
				    datatype struct 
 RETURNS
    Pointer to DEST on success, NULL on failure
 DESCRIPTION
	This function copies a native (memory) simple datatype message,
    allocating the destination structure if necessary.
--------------------------------------------------------------------------*/
static void *
H5O_dtype_copy(const void *_src, void *_dst)
{
    const H5T_t		   *src = (const H5T_t *) _src;
    H5T_t		   *dst = NULL;

    FUNC_ENTER(H5O_dtype_copy, NULL);

    /* check args */
    assert(src);

    /* copy */
    if (NULL == (dst = H5T_copy(src, H5T_COPY_ALL))) {
	HRETURN_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "can't copy type");
    }
    /* was result already allocated? */
    if (_dst) {
	*((H5T_t *) _dst) = *dst;
	H5MM_xfree(dst);
	dst = (H5T_t *) _dst;
    }
    FUNC_LEAVE((void *) dst);
}

/*--------------------------------------------------------------------------
 NAME
    H5O_dtype_size
 PURPOSE
    Return the raw message size in bytes
 USAGE
    void *H5O_dtype_size(f, mesg)
	H5F_t *f;	  IN: pointer to the HDF5 file struct
	const void *mesg;     IN: Pointer to the source simple datatype struct
 RETURNS
    Size of message on success, 0 on failure
 DESCRIPTION
	This function returns the size of the raw simple datatype message on
    success.  (Not counting the message type or size fields, only the data
    portion of the message).  It doesn't take into account alignment.
--------------------------------------------------------------------------*/
static size_t
H5O_dtype_size(H5F_t *f, const void *mesg)
{
    intn		    i;
    size_t		    ret_value = 8;
    const H5T_t		   *dt = (const H5T_t *) mesg;

    FUNC_ENTER(H5O_dtype_size, 0);

    assert(mesg);

    switch (dt->type) {
    case H5T_INTEGER:
	ret_value += 4;
	break;

    case H5T_BITFIELD:
	ret_value += 4;
	break;

    case H5T_OPAQUE:
	ret_value += (HDstrlen(dt->u.opaque.tag)+7) & 0xf8;
	break;

    case H5T_FLOAT:
	ret_value += 12;
	break;

    case H5T_COMPOUND:
	for (i=0; i<dt->u.compnd.nmembs; i++) {
	    ret_value += ((HDstrlen(dt->u.compnd.memb[i].name) + 8) / 8) * 8;
	    ret_value += 4 +		/*member offset*/
			 1 +		/*dimensionality*/
			 3 +		/*reserved*/
			 4 +		/*permutation*/
			 4 +		/*reserved*/
			 16;		/*dimensions*/
	    ret_value += H5O_dtype_size(f, dt->u.compnd.memb[i].type);
	}
	break;

    case H5T_ENUM:
	ret_value += H5O_dtype_size(f, dt->parent);
	for (i=0; i<dt->u.enumer.nmembs; i++) {
	    ret_value += ((HDstrlen(dt->u.enumer.name[i])+8)/8)*8;
	}
	ret_value += dt->u.enumer.nmembs * dt->parent->size;
	break;

    default:
	/*no properties */
	break;
    }

    FUNC_LEAVE(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:	H5O_dtype_reset
 *
 * Purpose:	Frees resources within a data type message, but doesn't free
 *		the message itself.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_dtype_reset(void *_mesg)
{
    H5T_t		   *dt = (H5T_t *) _mesg;
    H5T_t		   *tmp = NULL;

    FUNC_ENTER(H5O_dtype_reset, FAIL);

    if (dt) {
	if (NULL==(tmp = H5MM_malloc(sizeof(H5T_t)))) {
	    HRETURN_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL,
			   "memory allocation failed");
	}
	*tmp = *dt;
	H5T_close(tmp);
	HDmemset(dt, 0, sizeof(H5T_t));
    }
    FUNC_LEAVE(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_dtype_get_share
 *
 * Purpose:	Returns information about where the shared message is located
 *		by filling in the SH shared message struct.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Monday, June  1, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_dtype_get_share (H5F_t UNUSED *f, const void *_mesg,
		     H5O_shared_t *sh/*out*/)
{
    const H5T_t	*dt = (const H5T_t *)_mesg;
    
    FUNC_ENTER (H5O_dtype_get_share, FAIL);
    assert (dt);
    assert (sh);

    if (H5F_addr_defined (&(dt->ent.header))) {
	assert (H5T_STATE_NAMED==dt->state || H5T_STATE_OPEN==dt->state);
	sh->in_gh = FALSE;
	sh->u.ent = dt->ent;
    } else {
	HRETURN_ERROR (H5E_DATATYPE, H5E_CANTINIT, FAIL,
		       "data type is not sharable");
    }

    FUNC_LEAVE (SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5O_dtype_set_share
 *
 * Purpose:	Copies sharing information from SH into the message.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Thursday, June	4, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_dtype_set_share (H5F_t UNUSED *f, void *_mesg/*in,out*/,
		     const H5O_shared_t *sh)
{
    H5T_t	*dt = (H5T_t *)_mesg;
    
    FUNC_ENTER (H5O_dtype_set_share, FAIL);
    assert (dt);
    assert (sh);
    assert (!sh->in_gh);

    dt->ent = sh->u.ent;
    dt->state = H5T_STATE_NAMED;

    FUNC_LEAVE (SUCCEED);
}

/*--------------------------------------------------------------------------
 NAME
    H5O_dtype_debug
 PURPOSE
    Prints debugging information for a data type message
 USAGE
    void *H5O_dtype_debug(f, mesg, stream, indent, fwidth)
	H5F_t *f;		IN: pointer to the HDF5 file struct
	const void *mesg;	IN: Pointer to the source simple datatype
				    struct
	FILE *stream;		IN: Pointer to the stream for output data
	intn indent;		IN: Amount to indent information by
	intn fwidth;		IN: Field width (?)
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
	This function prints debugging output to the stream passed as a 
    parameter.
--------------------------------------------------------------------------*/
static herr_t
H5O_dtype_debug(H5F_t *f, const void *mesg, FILE *stream,
		intn indent, intn fwidth)
{
    const H5T_t		*dt = (const H5T_t*)mesg;
    const char		*s;
    char		buf[256];
    intn		i, j;
    size_t		k;
    

    FUNC_ENTER(H5O_dtype_debug, FAIL);

    /* check args */
    assert(f);
    assert(dt);
    assert(stream);
    assert(indent >= 0);
    assert(fwidth >= 0);

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
    case H5T_ENUM:
	s = "enum";
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
	    (unsigned long)(dt->size), 1==dt->size?"":"s");

    if (H5T_COMPOUND == dt->type) {
	fprintf(stream, "%*s%-*s %d\n", indent, "", fwidth,
		"Number of members:",
		dt->u.compnd.nmembs);
	for (i=0; i<dt->u.compnd.nmembs; i++) {
	    sprintf(buf, "Member %d:", i);
	    fprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
		    buf,
		    dt->u.compnd.memb[i].name);
	    fprintf(stream, "%*s%-*s %lu\n", indent+3, "", MAX(0, fwidth-3),
		    "Byte offset:",
		    (unsigned long) (dt->u.compnd.memb[i].offset));
	    fprintf(stream, "%*s%-*s %d%s\n", indent+3, "", MAX(0, fwidth-3),
		    "Dimensionality:",
		    dt->u.compnd.memb[i].ndims,
		    0==dt->u.compnd.memb[i].ndims?" (scalar)":"");
	    if (dt->u.compnd.memb[i].ndims>0) {
		fprintf(stream, "%*s%-*s {", indent+3, "", MAX(0, fwidth-3),
			"Size:");
		for (j=0; j<dt->u.compnd.memb[i].ndims; j++) {
		    fprintf(stream, "%s%lu", j?", ":"",
			    (unsigned long)(dt->u.compnd.memb[i].dim[j]));
		}
		fprintf(stream, "}\n");
		fprintf(stream, "%*s%-*s {", indent+3, "", MAX(0, fwidth-3),
			"Permutation:");
		for (j=0; j<dt->u.compnd.memb[i].ndims; j++) {
		    fprintf(stream, "%s%lu", j?", ":"",
			    (unsigned long)(dt->u.compnd.memb[i].perm[j]));
		}
		fprintf(stream, "}\n");
	    }
	    H5O_dtype_debug(f, dt->u.compnd.memb[i].type, stream,
			    indent+3, MAX(0, fwidth - 3));
	}
    } else if (H5T_ENUM==dt->type) {
	fprintf(stream, "%*s%s\n", indent, "", "Base type:");
	H5O_dtype_debug(f, dt->parent, stream, indent+3, MAX(0, fwidth-3));
	fprintf(stream, "%*s%-*s %d\n", indent, "", fwidth,
		"Number of members:",
		dt->u.enumer.nmembs);
	for (i=0; i<dt->u.enumer.nmembs; i++) {
	    sprintf(buf, "Member %d:", i);
	    fprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
		    buf,
		    dt->u.enumer.name[i]);
	    fprintf(stream, "%*s%-*s 0x", indent, "", fwidth,
		    "Raw bytes of value:");
	    for (k=0; k<dt->parent->size; k++) {
		fprintf(stream, "%02x",
			dt->u.enumer.value[i*dt->parent->size+k]);
	    }
	    fprintf(stream, "\n");
	}
	
    } else if (H5T_OPAQUE==dt->type) {
	fprintf(stream, "%*s%-*s \"%s\"\n", indent, "", fwidth,
		"Tag:", dt->u.opaque.tag);

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
		(unsigned long)(dt->u.atomic.prec),
		1==dt->u.atomic.prec?"":"s");

	fprintf(stream, "%*s%-*s %lu bit%s\n", indent, "", fwidth,
		"Offset:",
		(unsigned long)(dt->u.atomic.offset),
		1==dt->u.atomic.offset?"":"s");

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

    FUNC_LEAVE(SUCCEED);
}
