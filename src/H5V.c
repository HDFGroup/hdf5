/*
 * Copyright (C) 1997 NCSA
 *                    All rights reserved.
 *
 * Programmer: Robb Matzke <matzke@llnl.gov>
 *             Friday, October 10, 1997
 */

#include <H5private.h>
#include <H5Eprivate.h>
#include <H5Oprivate.h>
#include <H5Vprivate.h>

#define H5V_HYPER_NDIMS H5O_LAYOUT_NDIMS
#define PABLO_MASK      H5V_mask
static hbool_t          interface_initialize_g = TRUE;
#define INTERFACE_INIT  NULL

static herr_t           H5V_stride_optimize1(size_t *np, size_t *elmt_size,
                                             size_t *size, intn *stride1);
static herr_t           H5V_stride_optimize2(size_t *np, size_t *elmt_size,
                                             size_t *size, intn *stride1,
                                             intn *stride2);

/*-------------------------------------------------------------------------
 * Function:    H5V_stride_optimize1
 *
 * Purpose:     Given a stride vector which references elements of the
 *              specified size, optimize the dimensionality, the stride
 *              vector, and the element size to minimize the dimensionality
 *              and the number of memory accesses.
 *
 *              All arguments are passed by reference and their values may be
 *              modified by this function.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Saturday, October 11, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5V_stride_optimize1(size_t *np, size_t *elmt_size, size_t *size,
                     intn *stride1)
{
    FUNC_ENTER(H5V_stride_optimize1, FAIL);

    /*
     * This has to be true because if we optimize the dimensionality down to
     * zero we still must make one reference.
     */
    assert(1 == H5V_vector_reduce_product(0, (void *) 1));

    /*
     * Combine adjacent memory accesses
     */
    while (*np && stride1[*np - 1] == *elmt_size) {

        *elmt_size *= size[*np - 1];
        if (--*np) {
            stride1[*np - 1] += size[*np] * stride1[*np];
        }
    }

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5V_stride_optimize2
 *
 * Purpose:     Given two stride vectors which reference elements of the
 *              specified size, optimize the dimensionality, the stride
 *              vectors, and the element size to minimize the dimensionality
 *              and the number of memory accesses.
 *
 *              All arguments are passed by reference and their values may be
 *              modified by this function.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Saturday, October 11, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5V_stride_optimize2(size_t *np, size_t *elmt_size, size_t *size,
                     intn *stride1, intn *stride2)
{
    FUNC_ENTER(H5V_stride_optimize2, FAIL);

    /*
     * This has to be true because if we optimize the dimensionality down to
     * zero we still must make one reference.
     */
    assert(1 == H5V_vector_reduce_product(0, (void *) 1));

    /*
     * Combine adjacent memory accesses
     */
    while (*np && stride1[*np - 1] == *elmt_size && stride2[*np - 1] == *elmt_size) {

        *elmt_size *= size[*np - 1];
        if (--*np) {
            stride1[*np - 1] += size[*np] * stride1[*np];
            stride2[*np - 1] += size[*np] * stride2[*np];
        }
    }

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5V_hyper_stride
 *
 * Purpose:     Given a description of a hyperslab, this function returns
 *              (through STRIDE[]) the byte strides appropriate for accessing
 *              all bytes of the hyperslab and the byte offset where the
 *              striding will begin.  The SIZE can be passed to the various
 *              stride functions.
 *
 *              The stride and starting point returned will cause the
 *              hyperslab elements to be referenced in C order.
 *
 * Return:      Success:        Byte offset from beginning of array to start
 *                              of striding.
 *
 *              Failure:        abort() -- should never fail
 *
 * Programmer:  Robb Matzke
 *              Saturday, October 11, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
size_t
H5V_hyper_stride(size_t n, const size_t *size,
                 const size_t *total_size, const size_t *offset,
                 intn *stride /*output arg */ )
{
    size_t                  skip;       /*starting point byte offset            */
    size_t                  acc;        /*accumulator                           */
    int                     i;  /*counter                               */

    FUNC_ENTER(H5V_hyper_stride, (abort(), 0));

    assert(n >= 0 && n < H5V_HYPER_NDIMS);
    assert(size);
    assert(total_size);
    assert(stride);

    /* init */
    stride[n - 1] = 1;
    skip = offset ? offset[n - 1] : 0;

    /* others */
    for (i = n - 2, acc = 1; i >= 0; --i) {
        stride[i] = acc * (total_size[i + 1] - size[i + 1]);
        acc *= total_size[i + 1];
        skip += acc * (offset ? offset[i] : 0);
    }

    FUNC_LEAVE(skip);
}

/*-------------------------------------------------------------------------
 * Function:    H5V_hyper_eq
 *
 * Purpose:     Determines whether two hyperslabs are equal.  This function
 *              assumes that both hyperslabs are relative to the same array,
 *              for if not, they could not possibly be equal.
 *
 * Return:      Success:        TRUE if the hyperslabs are equal (that is,
 *                              both refer to exactly the same elements of an
 *                              array)
 *
 *                              FALSE otherwise.
 *
 *              Failure:        TRUE the rank is zero or if both hyperslabs
 *                              are of zero size.
 *
 * Programmer:  Robb Matzke
 *              Friday, October 17, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hbool_t
H5V_hyper_eq(size_t n,
             const size_t *offset1, const size_t *size1,
             const size_t *offset2, const size_t *size2)
{
    size_t                  nelmts1 = 1, nelmts2 = 1;
    intn                    i;

    if (n <= 0)
        return TRUE;

    for (i = 0; i < n; i++) {
        if ((offset1 ? offset1[i] : 0) != (offset2 ? offset2[i] : 0)) {
            return FALSE;
        }
        if ((size1 ? size1[i] : 0) != (size2 ? size2[i] : 0)) {
            return FALSE;
        }
        if (0 == (nelmts1 *= (size1 ? size1[i] : 0)))
            return FALSE;
        if (0 == (nelmts2 *= (size2 ? size2[i] : 0)))
            return FALSE;
    }
    return TRUE;
}

/*-------------------------------------------------------------------------
 * Function:    H5V_hyper_disjointp
 *
 * Purpose:     Determines if two hyperslabs are disjoint.
 *
 * Return:      Success:        FALSE if they are not disjoint.
 *                              TRUE if they are disjoint.
 *
 *              Failure:        A hyperslab of zero size is disjoint from all
 *                              other hyperslabs.
 *
 * Programmer:  Robb Matzke
 *              Thursday, October 16, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hbool_t
H5V_hyper_disjointp(size_t n,
                    const size_t *offset1, const size_t *size1,
                    const size_t *offset2, const size_t *size2)
{
    intn                    i;

    if (!n || !size1 || !size2)
        return TRUE;

    for (i = 0; i < n; i++) {
        if (0 == size1[i] || 0 == size2[i])
            return TRUE;
        if (((offset1 ? offset1[i] : 0) < (offset2 ? offset2[i] : 0) &&
             (offset1 ? offset1[i] : 0) + size1[i] <= (offset2 ? offset2[i] : 0)) ||
            ((offset2 ? offset2[i] : 0) < (offset1 ? offset1[i] : 0) &&
             (offset2 ? offset2[i] : 0) + size2[i] <= (offset1 ? offset1[i] : 0))) {
            return TRUE;
        }
    }
    return FALSE;
}

/*-------------------------------------------------------------------------
 * Function:    H5V_hyper_fill
 *
 * Purpose:     Similar to memset() except it operates on hyperslabs...
 *
 *              Fills a hyperslab of array BUF with some value VAL.  BUF
 *              is treated like a C-order array with N dimensions where the
 *              size of each dimension is TOTAL_SIZE[].  The hyperslab which
 *              will be filled with VAL begins at byte offset OFFSET[] from
 *              the minimum corner of BUF and continues for SIZE[] bytes in
 *              each dimension.
 *              
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Friday, October 10, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5V_hyper_fill(size_t n, const size_t *_size,
               const size_t *total_size, const size_t *offset, void *_dst,
               uint8 fill_value)
{
    uint8                  *dst = (uint8 *) _dst;       /*cast for ptr arithmetic       */
    size_t                  size[H5V_HYPER_NDIMS];      /*a modifiable copy of _size    */
    intn                    dst_stride[H5V_HYPER_NDIMS];        /*destination stride info  */
    size_t                  dst_start;  /*byte offset to start of stride */
    size_t                  elmt_size = 1;      /*bytes per element             */
    herr_t                  status;     /*function return status        */
#ifndef NDEBUG
    int                     i;
#endif

    FUNC_ENTER(H5V_hyper_fill, FAIL);

    /* check args */
    assert(n > 0 && n <= H5V_HYPER_NDIMS);
    assert(_size);
    assert(total_size);
    assert(dst);
#ifndef NDEBUG
    for (i = 0; i < n; i++) {
        assert(_size[i] > 0);
        assert(total_size[i] > 0);
    }
#endif

    /* Copy the size vector so we can modify it */
    H5V_vector_cpy(n, size, _size);

    /* Compute an optimal destination stride vector */
    dst_start = H5V_hyper_stride(n, size, total_size, offset, dst_stride);
    H5V_stride_optimize1(&n, &elmt_size, size, dst_stride);

    /* Copy */
    status = H5V_stride_fill(n, elmt_size, size, dst_stride, dst + dst_start,
                             fill_value);

    FUNC_LEAVE(status);
}

/*-------------------------------------------------------------------------
 * Function:    H5V_hyper_copy
 *
 * Purpose:     Copies a hyperslab from the source to the destination.
 *
 *              A hyperslab is a logically contiguous region of
 *              multi-dimensional size SIZE of an array whose dimensionality
 *              is N and whose total size is DST_TOTAL_SIZE or SRC_TOTAL_SIZE.
 *              The minimum corner of the hyperslab begins at a
 *              multi-dimensional offset from the minimum corner of the DST
 *              (destination) or SRC (source) array.  The sizes and offsets
 *              are assumed to be in C order, that is, the first size/offset
 *              varies the slowest while the last varies the fastest in the
 *              mapping from N-dimensional space to linear space.  This
 *              function assumes that the array elements are single bytes (if
 *              your array has multi-byte elements then add an additional
 *              dimension whose size is that of your element).
 *
 *              The SRC and DST array may be the same array, but the results
 *              are undefined if the source hyperslab overlaps the
 *              destination hyperslab.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Friday, October 10, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5V_hyper_copy(size_t n, const size_t *_size,

                /*destination */
               const size_t *dst_size, const size_t *dst_offset,
               void *_dst,

                /*source */
               const size_t *src_size, const size_t *src_offset,
               const void *_src)
{
    const uint8            *src = (const uint8 *) _src;         /*cast for ptr arithmtc */
    uint8                  *dst = (uint8 *) _dst;       /*cast for ptr arithmtc */
    size_t                  size[H5V_HYPER_NDIMS];      /*a modifiable _size    */
    intn                    src_stride[H5V_HYPER_NDIMS];        /*source stride info    */
    intn                    dst_stride[H5V_HYPER_NDIMS];        /*dest stride info      */
    size_t                  dst_start, src_start;       /*offset to start at    */
    size_t                  elmt_size = 1;      /*element size in bytes */
    herr_t                  status;     /*return status         */
#ifndef NDEBUG
    intn                    i;
#endif

    FUNC_ENTER(H5V_hyper_copy, FAIL);

    /* check args */
    assert(n > 0 && n <= H5V_HYPER_NDIMS);
    assert(_size);
    assert(dst_size);
    assert(src_size);
    assert(dst);
    assert(src);
#ifndef NDEBUG
    for (i = 0; i < n; i++) {
        assert(_size[i] > 0);
        assert(dst_size[i] > 0);
        assert(src_size[i] > 0);
    }
#endif

    /* Copy the size vector so we can modify it */
    H5V_vector_cpy(n, size, _size);

    /* Compute stride vectors for source and destination */
    dst_start = H5V_hyper_stride(n, size, dst_size, dst_offset, dst_stride);
    src_start = H5V_hyper_stride(n, size, src_size, src_offset, src_stride);

    /* Optimize the strides as a pair */
    H5V_stride_optimize2(&n, &elmt_size, size, dst_stride, src_stride);

    /* Perform the copy in terms of stride */
    status = H5V_stride_copy(n, elmt_size, size,
                             dst_stride, dst + dst_start,
                             src_stride, src + src_start);

    FUNC_LEAVE(status);
}

/*-------------------------------------------------------------------------
 * Function:    H5V_stride_fill
 *
 * Purpose:     Fills all bytes of a hyperslab with the same value using
 *              memset().
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Saturday, October 11, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5V_stride_fill(size_t n, size_t elmt_size, const size_t *size,
                const intn *stride, void *_dst, uint8 fill_value)
{
    uint8                  *dst = (uint8 *) _dst;       /*cast for ptr arithmetic       */
    size_t                  idx[H5V_HYPER_NDIMS];       /*1-origin indices              */
    size_t                  nelmts;     /*number of elements to fill    */
    intn                    i, j;       /*counters                      */
    hbool_t                 carry;      /*subtraction carray value      */

    FUNC_ENTER(H5V_stride_fill, FAIL);

    H5V_vector_cpy(n, idx, size);
    nelmts = H5V_vector_reduce_product(n, size);
    for (i = 0; i < nelmts; i++) {

        /* Copy an element */
        HDmemset(dst, fill_value, elmt_size);

        /* Decrement indices and advance pointer */
        for (j = n - 1, carry = TRUE; j >= 0 && carry; --j) {
            dst += stride[j];

            if (--idx[j])
                carry = FALSE;
            else
                idx[j] = size[j];
        }
    }

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5V_stride_copy
 *
 * Purpose:     Uses DST_STRIDE and SRC_STRIDE to advance through the arrays
 *              DST and SRC while copying bytes from SRC to DST.  This
 *              function minimizes the number of calls to memcpy() by
 *              combining various strides, but it will never touch memory
 *              outside the hyperslab defined by the strides.
 *
 * Note:        If the src_stride is all zero and elmt_size is one, then it's
 *              probably more efficient to use H5V_stride_fill() instead.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Saturday, October 11, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5V_stride_copy(size_t n, size_t elmt_size, const size_t *size,
                const intn *dst_stride, void *_dst,
                const intn *src_stride, const void *_src)
{

    uint8                  *dst = (uint8 *) _dst;       /*cast for ptr arithmetic */
    const uint8            *src = (const uint8 *) _src;         /*cast for ptr arithmetic */
    size_t                  idx[H5V_HYPER_NDIMS];       /*1-origin indices      */
    size_t                  nelmts;     /*num elements to copy  */
    intn                    i, j;       /*counters              */
    hbool_t                 carry;      /*carray for subtraction */

    FUNC_ENTER(H5V_stride_copy, FAIL);

    H5V_vector_cpy(n, idx, size);
    nelmts = H5V_vector_reduce_product(n, size);
    for (i = 0; i < nelmts; i++) {

        /* Copy an element */
        HDmemcpy(dst, src, elmt_size);

        /* Decrement indices and advance pointers */
        for (j = n - 1, carry = TRUE; j >= 0 && carry; --j) {
            src += src_stride[j];
            dst += dst_stride[j];

            if (--idx[j])
                carry = FALSE;
            else
                idx[j] = size[j];
        }
    }

    FUNC_LEAVE(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    H5V_stride_copy2
 *
 * Purpose:     Similar to H5V_stride_copy() except the source and
 *              destination each have their own dimensionality and size and
 *              we copy exactly NELMTS elements each of size ELMT_SIZE.  The
 *              size counters wrap if NELMTS is more than a size counter.
 *
 * Return:      Success:        SUCCEED
 *
 *              Failure:        FAIL
 *
 * Programmer:  Robb Matzke
 *              Saturday, October 11, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5V_stride_copy2(size_t nelmts, size_t elmt_size,

                  /* destination */
               size_t dst_n, const size_t *dst_size, const intn *dst_stride,
                 void *_dst,

                  /* source */
               size_t src_n, const size_t *src_size, const intn *src_stride,
                 const void *_src)
{
    uint8                  *dst = (uint8 *) _dst;
    const uint8            *src = (const uint8 *) _src;
    size_t                  dst_idx[H5V_HYPER_NDIMS];
    size_t                  src_idx[H5V_HYPER_NDIMS];
    intn                    i, j;
    hbool_t                 carry;

    FUNC_ENTER(H5V_stride_copy2, FAIL);

    H5V_vector_cpy(dst_n, dst_idx, dst_size);
    H5V_vector_cpy(src_n, src_idx, src_size);

    for (i = 0; i < nelmts; i++) {

        /* Copy an element */
        HDmemcpy(dst, src, elmt_size);

        /* Decrement indices and advance pointers */
        for (j = dst_n - 1, carry = TRUE; j >= 0 && carry; --j) {
            dst += dst_stride[j];
            if (--dst_idx[j])
                carry = FALSE;
            else
                dst_idx[j] = dst_size[j];
        }
        for (j = src_n - 1, carry = TRUE; j >= 0 && carry; --j) {
            src += src_stride[j];
            if (--src_idx[j])
                carry = FALSE;
            else
                src_idx[j] = src_size[j];
        }
    }

    FUNC_LEAVE(SUCCEED);
}
