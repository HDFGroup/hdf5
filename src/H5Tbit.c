/*
 * Copyright (C) 1998 NCSA
 *                    All rights reserved.
 *
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Wednesday, June 10, 1998
 *
 * Purpose:	Operations on bit vectors.  A bit vector is an array of bytes
 *		with the least-significant bits in the first byte.  That is,
 *		the bytes are in little-endian order.
 */
#define H5T_PACKAGE
#include <H5private.h>
#include <H5Tpkg.h>


/*-------------------------------------------------------------------------
 * Function:	H5T_bit_copy
 *
 * Purpose:	Copies bits from one vector to another.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Wednesday, June 10, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
H5T_bit_copy (uint8 *dst, size_t dst_offset, const uint8 *src,
	      size_t src_offset, size_t size)
{
    intn	shift;
    uintn	mask_lo, mask_hi;
    intn	s_idx, d_idx;

    /*
     * Normalize the offset to be a byte number and a bit offset within that
     * byte.
     */
    s_idx = src_offset / 8;
    d_idx = dst_offset / 8;
    src_offset %= 8;
    dst_offset %= 8;
    
    /*
     * Get things rolling. This means copying bits until we're aligned on a
     * source byte.  This the following example, five bits are copied to the
     * destination.
     *
     *                      src[s_idx]
     *   +---------------+---------------+
     *   |7 6 5 4 3 2 1 0|7 6 5 4 3 2 1 0|
     *   +---------------+---------------+
     *      ... : : : : : | | | | |
     *      ... v v v v v V V V V V
     *      ...+---------------+---------------+
     *      ...|7 6 5 4 3 2 1 0|7 6 5 4 3 2 1 0|
     *      ...+---------------+---------------+
     *           dst[d_idx+1]      dst[d_idx]
     */
    while (src_offset && size>0) {
	unsigned nbits = MIN3 (size, 8-dst_offset, 8-src_offset);
	unsigned mask = (1<<nbits) - 1;

	dst[d_idx] &= ~(mask<<dst_offset);
	dst[d_idx] |= ((src[s_idx]>>src_offset)&mask) << dst_offset;

	src_offset += nbits;
	if (src_offset>=8) {
	    s_idx++;
	    src_offset %= 8;
	}
	dst_offset += nbits;
	if (dst_offset>=8) {
	    d_idx++;
	    dst_offset %= 8;
	}
	size -= nbits;
    }
	
    /*
     * The middle bits. We are aligned on a source byte which needs to be
     * copied to two (or one in the degenerate case) destination bytes.
     *
     * 		      src[s_idx]
     * 		   +---------------+
     *  	   |7 6 5 4 3 2 1 0|
     * 		   +---------------+
     *              | | | | | | | |
     * 		    V V V V V V V V
     *   +---------------+---------------+
     *   |7 6 5 4 3 2 1 0|7 6 5 4 3 2 1 0|
     *   +---------------+---------------+
     *     dst[d_idx+1]      dst[d_idx]
     *
     *		 
     * Calculate shifts and masks.  See diagrams below.  MASK_LO in this
     * example is 0x1f (the low five bits) and MASK_HI is 0xe0 (the high three
     * bits). SHIFT is three since the source must be shifted right three bits
     * to line up with the destination.
     */
    shift = dst_offset;
    mask_lo = (1<<(8-shift))-1;
    mask_hi = ~mask_lo;
    
    for (/*void*/; size>8; size-=8, d_idx++, s_idx++) {
	if (shift) {
	    dst[d_idx+0] &= ~(mask_lo<<shift);
	    dst[d_idx+0] |= (src[s_idx] & mask_lo) << shift;
	    dst[d_idx+1] &= ~(mask_hi>>(8-shift));
	    dst[d_idx+1] |= (src[s_idx] & mask_hi) >> (8-shift);
	} else {
	    dst[d_idx] = src[s_idx];
	}
    }

    /* Finish up */
    while (size>0) {
	unsigned nbits = MIN3 (size, 8-dst_offset, 8-src_offset);
	unsigned mask = (1<<nbits) - 1;

	dst[d_idx] &= ~(mask<<dst_offset);
	dst[d_idx] |= ((src[s_idx]>>src_offset)&mask) << dst_offset;

	src_offset += nbits;
	if (src_offset>=8) {
	    s_idx++;
	    src_offset %= 8;
	}
	dst_offset += nbits;
	if (dst_offset>=8) {
	    d_idx++;
	    dst_offset %= 8;
	}
	size -= nbits;
    }
}


/*-------------------------------------------------------------------------
 * Function:	H5T_bit_set
 *
 * Purpose:	Sets or clears bits in a contiguous region of a vector
 *		beginning at bit OFFSET and continuing for SIZE bits.
 *
 * Return:	void
 *
 * Programmer:	Robb Matzke
 *              Wednesday, June 10, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void
H5T_bit_set (uint8 *buf, size_t offset, size_t size, hbool_t value)
{
    intn	idx;

    /* Normalize */
    idx = offset / 8;
    offset %= 8;

    /* The first partial byte */
    if (size && offset%8) {
	size_t nbits = MIN (size, 8-offset);
	unsigned mask = (1<<nbits)-1;
	if (value) {
	    buf[idx++] |= mask << offset;
	} else {
	    buf[idx++] &= ~(mask << offset);
	}
	size -= nbits;
    }
    
    /* The middle bytes */
    while (size>=8) {
	buf[idx++] = value ? 0xff : 0x00;
	size -= 8;
    }

    /* The last partial byte */
    if (size) {
	if (value) {
	    buf[idx] |= (1<<size)-1;
	} else {
	    buf[idx] &= ~((1<<size)-1);
	}
    }
}


/*-------------------------------------------------------------------------
 * Function:	H5T_bit_find
 *
 * Purpose:	Finds the first bit with the specified VALUE within a region
 *		of a bit vector.  The region begins at OFFSET and continues
 *		for SIZE bits, but the region can be searched from the least
 *		significat end toward the most significant end with 
 *
 * Return:	Success:	The position of the bit found, relative to
 *				the offset.
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Wednesday, June 10, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5T_bit_find (uint8 *buf, size_t offset, size_t size, H5T_sdir_t direction,
	      hbool_t value)
{
    size_t	base=offset;
    ssize_t	idx, i;

    /* Some functions call this with value=TRUE */
    assert (TRUE==1);


    switch (direction) {
    case H5T_BIT_LSB:
	/* Calculate index */
	idx = offset / 8;
	offset %= 8;
	
	/* Beginning */
	if (offset) {
	    for (i=offset; i<8 && size>0; i++, size--) {
		if (value==((buf[idx]>>i) & 0x01)) {
		    return 8*idx+i - base;
		}
	    }
	    offset = 0;
	    idx++;
	}
	/* Middle */
	while (size>=8) {
	    if ((value?0x00:0xff)!=buf[idx]) {
		for (i=0; i<8; i++) {
		    if (value==((buf[idx]>>i) & 0x01)) {
			return 8*idx+i - base;
		    }
		}
	    }
	    size -= 8;
	    idx++;
	}
	/* End */
	for (i=0; i<(ssize_t)size; i++) {
	    if (value==((buf[idx]>>i) & 0x01)) {
		return 8*idx+i - base;
	    }
	}
	break;

    case H5T_BIT_MSB:
	/* Calculate index */
	idx = (offset+size-1) / 8;
	offset %= 8;
	
	/* Beginning */
	if (size>8-offset && (offset+size)%8) {
	    for (i=(offset+size)%8-1; i>=0; --i, --size) {
		if (value==((buf[idx]>>i) & 0x01)) {
		    return 8*idx+i - base;
		}
	    }
	    --idx;
	}
	/* Middle */
	while (size>=8) {
	    if ((value?0x00:0xff)!=buf[idx]) {
		for (i=7; i>=0; --i) {
		    if (value==((buf[idx]>>i) & 0x01)) {
			return 8*idx+i - base;
		    }
		}
	    }
	    size -= 8;
	    --idx;
	}
	/* End */
	if (size>0) {
	    for (i=offset+size-1; i>=(ssize_t)offset; --i) {
		if (value==((buf[idx]>>i) & 0x01)) {
		    return 8*idx+i - base;
		}
	    }
	}
	break;
    }

    
    return -1;
}
