/*
 * Copyright (C) 1997 NCSA
 *                    All rights reserved.
 *
 * Programmer: Robb Matzke <matzke@llnl.gov>
 *             Friday, October 10, 1997
 */
#ifndef H5Vprivate_H
#define H5Vprivate_H

#include <H5private.h>

/* Vector comparison functions like Fortran66 comparison operators */
#define H5V_vector_eq(N,V1,V2) (H5V_vector_cmp (N, V1, V2)==0)
#define H5V_vector_lt(N,V1,V2) (H5V_vector_cmp (N, V1, V2)<0)
#define H5V_vector_gt(N,V1,V2) (H5V_vector_cmp (N, V1, V2)>0)
#define H5V_vector_le(N,V1,V2) (H5V_vector_cmp (N, V1, V2)<=0)
#define H5V_vector_ge(N,V1,V2) (H5V_vector_cmp (N, V1, V2)>=0)

/* Other functions */
#define H5V_vector_cpy(N,DST,SRC) {                                           \
   if (SRC) HDmemcpy (DST, SRC, (N)*sizeof(size_t));                          \
   else HDmemset (DST, 0, (N)*sizeof(size_t));                                \
}

#define H5V_vector_zero(N,DST) HDmemset(DST,0,(N)*sizeof(size_t))

/* A null pointer is equivalent to a zero vector */
#define H5V_ZERO        NULL

size_t                  H5V_hyper_stride(size_t n, const size_t *size,
                             const size_t *total_size, const size_t *offset,
                                         intn *stride);
hbool_t                 H5V_hyper_disjointp(size_t n,
                                 const size_t *offset1, const size_t *size1,
                                const size_t *offset2, const size_t *size2);
hbool_t                 H5V_hyper_eq(size_t n, const size_t *offset1, const size_t *size1,
                                const size_t *offset2, const size_t *size2);
herr_t                  H5V_hyper_fill(size_t n, const size_t *total_size,
                                   const size_t *offset, const size_t *size,
                                       void *buf, uint8 val);
herr_t                  H5V_hyper_copy(size_t n, const size_t *size,
                     const size_t *dst_total_size, const size_t *dst_offset,
                                   void *_dst, const size_t *src_total_size,
                                const size_t *src_offset, const void *_src);
herr_t                  H5V_stride_fill(size_t n, size_t elmt_size, const size_t *size,
                          const intn *stride, void *_dst, uint8 fill_value);
herr_t                  H5V_stride_copy(size_t n, size_t elmt_size, const size_t *_size,
                                        const intn *dst_stride, void *_dst,
                                  const intn *src_stride, const void *_src);
herr_t                  H5V_stride_copy2(size_t nelmts, size_t elmt_size, size_t dst_n,
                             const size_t *dst_size, const intn *dst_stride,
                           void *_dst, size_t src_n, const size_t *src_size,
                                  const intn *src_stride, const void *_src);

/*-------------------------------------------------------------------------
 * Function:    H5V_vector_reduce_product
 *
 * Purpose:     Product reduction of a vector.  Vector elements and return
 *              value are size_t because we usually want the number of
 *              elements in an array and array dimensions are always of type
 *              size_t.
 *
 * Return:      Success:        Product of elements
 *
 *              Failure:        1 if N is zero
 *
 * Programmer:  Robb Matzke
 *              Friday, October 10, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static inline size_t 
__attribute__((unused))
H5V_vector_reduce_product(size_t n, const size_t *v)
{
    size_t                  ans = 1;

    if (n && !v)
        return 0;
    while (n--)
        ans *= *v++;
    return ans;
}

/*-------------------------------------------------------------------------
 * Function:    H5V_vector_zerop
 *
 * Purpose:     Determines if all elements of a vector are zero.
 *
 * Return:      Success:        TRUE if all elements are zero,
 *                              FALSE otherwise
 *
 *              Failure:        TRUE if N is zero
 *
 * Programmer:  Robb Matzke
 *              Friday, October 10, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static inline hbool_t 
__attribute__((unused))
H5V_vector_zerop(size_t n, const size_t *v)
{
    if (!v)
        return TRUE;
    while (n--)
        if (*v++)
            return FALSE;
    return TRUE;
}

/*-------------------------------------------------------------------------
 * Function:    H5V_vector_cmp
 *
 * Purpose:     Compares two vectors of the same size and determines if V1 is
 *              lexicographically less than, equal, or greater than V2.
 *
 * Return:      Success:        -1 if V1 is less than V2
 *                              0 if they are equal
 *                              1 if V1 is greater than V2
 *
 *              Failure:        0 if N is zero
 *
 * Programmer:  Robb Matzke
 *              Friday, October 10, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static inline intn 
__attribute__((unused))
H5V_vector_cmp(size_t n, const size_t *v1, const size_t *v2)
{
    if (v1 == v2)
        return 0;
    while (n--) {
        if ((v1 ? *v1 : 0) < (v2 ? *v2 : 0))
            return -1;
        if ((v1 ? *v1 : 0) > (v2 ? *v2 : 0))
            return 1;
        if (v1)
            v1++;
        if (v2)
            v2++;
    }
    return 0;
}

/*-------------------------------------------------------------------------
 * Function:    H5V_vector_inc
 *
 * Purpose:     Increments V1 by V2
 *
 * Return:      void
 *
 * Programmer:  Robb Matzke
 *              Monday, October 13, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static inline void 
__attribute__((unused))
H5V_vector_inc(size_t n, size_t *v1, const size_t *v2)
{
    while (n--)
        *v1++ += *v2++;
}

#endif
