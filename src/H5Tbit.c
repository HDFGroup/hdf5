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
    uintn	shift;
    uintn	mask_lo, mask_hi;
    intn	s_idx, d_idx;
    
    /*
     * Calculate shifts and masks.  See diagrams below.  MASK_LO in this
     * example is 0x1f (the low five bits) and MASK_HI is 0xe0 (the high three
     * bits). SHIFT is three since the source must be shifted right three bits
     * to line up with the destination.
     */
    shift = (dst_offset%8)-(src_offset%8);
    mask_lo = (1<<(8-shift))-1;
    mask_hi = ((1<<shift)-1) << (8-shift);
    s_idx = src_offset / 8;
    d_idx = dst_offset / 8;
    
    /*
     * Get things rolling. This means copying bits until we're aligned on a
     * source byte.  This the following example, four bits are copied to the
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
    if (src_offset%8 && size>0) {
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
     */
    for (/*void*/; size>8; size-=8, d_idx++, s_idx++) {
	if (shift) {
	    dst[d_idx+0] &= mask_lo;
	    dst[d_idx+0] |= (src[s_idx] << shift) & mask_hi;
	    dst[d_idx+1] &= mask_hi;
	    dst[d_idx+1] |= (src[s_idx] >> (8-shift)) & mask_lo;
	} else {
	    dst[d_idx] = src[s_idx];
	}
    }
    
	


    /* Finish up */
    
	
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
    return -1;
}
