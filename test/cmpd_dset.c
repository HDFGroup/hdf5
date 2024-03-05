/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* See H5private.h for how to include headers */
#undef NDEBUG

#define H5T_FRIEND  /*suppress error about including H5Tpkg      */
#include "H5Tpkg.h" /*to turn off hardware conversions*/
#include "H5Iprivate.h"

#include "h5test.h"

static const char *FILENAME[] = {"cmpd_dset", "src_subset", "dst_subset", "select_cmpd_dset", NULL};

const char *DSET_NAME[] = {"contig_src_subset", "chunk_src_subset", "contig_dst_subset", "chunk_dst_subset",
                           NULL};

/* The first dataset */
typedef struct s1_t {
    unsigned int a;
    unsigned int b;
    unsigned int c[4];
    unsigned int d;
    unsigned int e;
} s1_t;

/* The second dataset (same as first) */
typedef s1_t s2_t;

/* The third dataset (reversed fields of s1) */
typedef struct s3_t {
    unsigned int e;
    unsigned int d;
    unsigned int c[4];
    unsigned int b;
    unsigned int a;
} s3_t;

/* The fourth dataset (a subset of s1) */
typedef struct s4_t {
    unsigned int b;
    unsigned int d;
} s4_t;

/* The fifth dataset (a superset of s1) */
typedef struct s5_t {
    unsigned int pre;
    unsigned int a;
    unsigned int b;
    unsigned int mid1;
    unsigned int c[4];
    unsigned int mid2;
    unsigned int d;
    unsigned int e;
    unsigned int post;
} s5_t;

/* The sixth dataset (a superset of s1).  This is for
 * testing the optimization for the Chicago company. */
typedef struct s6_t {
    unsigned int a;
    unsigned int b;
    unsigned int c[4];
    unsigned int d;
    unsigned int e;
    unsigned int pre;
    unsigned int mid1;
    unsigned int mid2;
    unsigned int post;
} s6_t;

typedef struct s7_t {
    int32_t a;
    int32_t d;
} s7_t;

typedef struct s8_t {
    int64_t a;
    int64_t b;
    int64_t c;
} s8_t;

/* Structures for testing the optimization for the Chicago company. */
typedef struct {
    int    a, b, c[8], d, e;
    float  f, g, h[16], i, j;
    double k, l, m, n;
} stype1;

typedef struct {
    int    a, b, c[8], d, e;
    float  f, g, h[16], i, j;
    double k, l, m, n;
    long   o, p, q;
} stype2;

typedef struct {
    int a, b, c[8], d, e;
} stype3;

typedef struct {
    int       a, b, c[8], d, e;
    float     f, g, h[16], i, j;
    double    k, l, m, n;
    long      o, p, q;
    long long r, s, t;
} stype4;

#define NX          100U
#define NY          2000U
#define PACK_NMEMBS 100

static void  initialize_stype1(unsigned char *buf, size_t num);
static void  initialize_stype2(unsigned char *buf, size_t num);
static void  initialize_stype3(unsigned char *buf, size_t num);
static void  initialize_stype4(unsigned char *buf, size_t num);
static hid_t create_stype1(void);
static hid_t create_stype2(void);
static hid_t create_stype3(void);
static hid_t create_stype4(void);
static int   compare_data(void *src_data, void *dst_data, hbool_t src_subset);
static int   compare_stype4_data(void *expect_buf, void *rbuf);
static int   compare_s1_data(void *expect_buf, void *rbuf);
static int   compare_s1_s3_data(void *expect_buf, void *rbuf);
static int   compare_s7_data(void *expect_buf, void *rbuf);
static int   compare_a_d_data(void *exp1_buf, void *exp2_buf, void *rbuf);
static int   compare_a_b_c_data(void *exp1_buf, void *exp2_buf, void *rbuf);

/*-------------------------------------------------------------------------
 * Function:    compare_stype4_data
 *
 * Purpose:     Compare data (the common fields in stype4/stype2) read in rbuf with expected data
 *              in expect_buf.
 *
 * Return:      Success:        0
 *
 *              Failure:        negative
 *
 *-------------------------------------------------------------------------
 */
static int
compare_stype4_data(void *expect_buf, void *rbuf)
{
    int i;

    for (i = 0; i < (int)(NX * NY); i++) {
        stype4 *s1_ptr;
        stype4 *s2_ptr;
        s1_ptr = ((stype4 *)expect_buf) + i;
        s2_ptr = ((stype4 *)rbuf) + i;

        if (s1_ptr->a != s2_ptr->a || s1_ptr->b != s2_ptr->b || s1_ptr->c[0] != s2_ptr->c[0] ||
            s1_ptr->c[1] != s2_ptr->c[1] || s1_ptr->c[2] != s2_ptr->c[2] || s1_ptr->c[3] != s2_ptr->c[3] ||
            s1_ptr->c[4] != s2_ptr->c[4] || s1_ptr->c[5] != s2_ptr->c[5] || s1_ptr->c[6] != s2_ptr->c[6] ||
            s1_ptr->c[7] != s2_ptr->c[7] || s1_ptr->d != s2_ptr->d || s1_ptr->e != s2_ptr->e ||
            !H5_FLT_ABS_EQUAL(s1_ptr->f, s2_ptr->f) || !H5_FLT_ABS_EQUAL(s1_ptr->g, s2_ptr->g) ||
            !H5_FLT_ABS_EQUAL(s1_ptr->h[0], s2_ptr->h[0]) || !H5_FLT_ABS_EQUAL(s1_ptr->h[1], s2_ptr->h[1]) ||
            !H5_FLT_ABS_EQUAL(s1_ptr->h[2], s2_ptr->h[2]) || !H5_FLT_ABS_EQUAL(s1_ptr->h[3], s2_ptr->h[3]) ||
            !H5_FLT_ABS_EQUAL(s1_ptr->h[4], s2_ptr->h[4]) || !H5_FLT_ABS_EQUAL(s1_ptr->h[5], s2_ptr->h[5]) ||
            !H5_FLT_ABS_EQUAL(s1_ptr->h[6], s2_ptr->h[6]) || !H5_FLT_ABS_EQUAL(s1_ptr->h[7], s2_ptr->h[7]) ||
            !H5_FLT_ABS_EQUAL(s1_ptr->h[8], s2_ptr->h[8]) || !H5_FLT_ABS_EQUAL(s1_ptr->h[9], s2_ptr->h[9]) ||
            !H5_FLT_ABS_EQUAL(s1_ptr->h[10], s2_ptr->h[10]) ||
            !H5_FLT_ABS_EQUAL(s1_ptr->h[11], s2_ptr->h[11]) ||
            !H5_FLT_ABS_EQUAL(s1_ptr->h[12], s2_ptr->h[12]) ||
            !H5_FLT_ABS_EQUAL(s1_ptr->h[13], s2_ptr->h[13]) ||
            !H5_FLT_ABS_EQUAL(s1_ptr->h[14], s2_ptr->h[14]) ||
            !H5_FLT_ABS_EQUAL(s1_ptr->h[15], s2_ptr->h[15]) || !H5_FLT_ABS_EQUAL(s1_ptr->i, s2_ptr->i) ||
            !H5_FLT_ABS_EQUAL(s1_ptr->j, s2_ptr->j) || !H5_DBL_ABS_EQUAL(s1_ptr->k, s2_ptr->k) ||
            !H5_DBL_ABS_EQUAL(s1_ptr->l, s2_ptr->l) || !H5_DBL_ABS_EQUAL(s1_ptr->m, s2_ptr->m) ||
            !H5_DBL_ABS_EQUAL(s1_ptr->n, s2_ptr->n) || s1_ptr->o != s2_ptr->o || s1_ptr->p != s2_ptr->p ||
            s1_ptr->q != s2_ptr->q) {
            H5_FAILED();
            printf("    i=%d\n", i);
            printf("    exp_buf={a=%d, b=%d, c=[%d,%d,%d,%d,%d,%d,%d,%d], d=%d, e=%d, f=%f, g=%f, "
                   "h=[%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f], i=%f, j=%f, k=%f, l=%f, m=%f, n=%f, "
                   "o=%ld, p=%ld, q=%ld}\n",
                   s1_ptr->a, s1_ptr->b, s1_ptr->c[0], s1_ptr->c[1], s1_ptr->c[2], s1_ptr->c[3], s1_ptr->c[4],
                   s1_ptr->c[5], s1_ptr->c[6], s1_ptr->c[7], s1_ptr->d, s1_ptr->e, (double)s1_ptr->f,
                   (double)s1_ptr->g, (double)s1_ptr->h[0], (double)s1_ptr->h[1], (double)s1_ptr->h[2],
                   (double)s1_ptr->h[3], (double)s1_ptr->h[4], (double)s1_ptr->h[5], (double)s1_ptr->h[6],
                   (double)s1_ptr->h[7], (double)s1_ptr->h[8], (double)s1_ptr->h[9], (double)s1_ptr->h[10],
                   (double)s1_ptr->h[11], (double)s1_ptr->h[12], (double)s1_ptr->h[13], (double)s1_ptr->h[14],
                   (double)s1_ptr->h[15], (double)s1_ptr->i, (double)s1_ptr->j, s1_ptr->k, s1_ptr->l,
                   s1_ptr->m, s1_ptr->n, s1_ptr->o, s1_ptr->p, s1_ptr->q);
            printf("    rbuf={a=%d, b=%d, c=[%d,%d,%d,%d,%d,%d,%d,%d], d=%d, e=%d, f=%f, g=%f, "
                   "h=[%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f], i=%f, j=%f, k=%f, l=%f, m=%f, n=%f, "
                   "o=%ld, p=%ld, q=%ld}\n",
                   s2_ptr->a, s2_ptr->b, s2_ptr->c[0], s2_ptr->c[1], s2_ptr->c[2], s2_ptr->c[3], s2_ptr->c[4],
                   s2_ptr->c[5], s2_ptr->c[6], s2_ptr->c[7], s2_ptr->d, s2_ptr->e, (double)s2_ptr->f,
                   (double)s2_ptr->g, (double)s2_ptr->h[0], (double)s2_ptr->h[1], (double)s2_ptr->h[2],
                   (double)s2_ptr->h[3], (double)s2_ptr->h[4], (double)s2_ptr->h[5], (double)s2_ptr->h[6],
                   (double)s2_ptr->h[7], (double)s2_ptr->h[8], (double)s2_ptr->h[9], (double)s2_ptr->h[10],
                   (double)s2_ptr->h[11], (double)s2_ptr->h[12], (double)s2_ptr->h[13], (double)s2_ptr->h[14],
                   (double)s2_ptr->h[15], (double)s2_ptr->i, (double)s2_ptr->j, s2_ptr->k, s2_ptr->l,
                   s2_ptr->m, s2_ptr->n, s1_ptr->o, s1_ptr->p, s1_ptr->q);

            goto error;
        }
    } /* end for */

    return SUCCEED;

error:
    return FAIL;

} /* compare_stype4_data() */

/*-------------------------------------------------------------------------
 * Function:    compare_s1_data
 *
 * Purpose:     Compare data (s1_t) read in rbuf with expected data in expect_buf.
 *
 * Return:      Success:        0
 *
 *              Failure:        negative
 *
 *-------------------------------------------------------------------------
 */
static int
compare_s1_data(void *expect_buf, void *rbuf)
{
    int   i;
    s1_t *s1_ptr;
    s1_t *s2_ptr;

    /* Compare save_s1 with rbuf1.  They should be the same */
    for (i = 0; i < (int)(NX * NY); i++) {
        s1_ptr = ((s1_t *)expect_buf) + i;
        s2_ptr = ((s1_t *)rbuf) + i;

        if (s1_ptr->a != s2_ptr->a || s1_ptr->b != s2_ptr->b || s1_ptr->c[0] != s2_ptr->c[0] ||
            s1_ptr->c[1] != s2_ptr->c[1] || s1_ptr->c[2] != s2_ptr->c[2] || s1_ptr->c[3] != s2_ptr->c[3] ||
            s1_ptr->d != s2_ptr->d || s1_ptr->e != s2_ptr->e) {
            H5_FAILED();
            printf("    i=%d\n", i);
            puts("    Incorrect values read from the file");
            goto error;
        }
    }

    return SUCCEED;

error:
    return FAIL;

} /* compare_s1_data() */

/*-------------------------------------------------------------------------
 * Function:    compare_s1_s3_data
 *
 * Purpose:     Compare data (s1_t/s3_t) read in rbuf with expected data in expect_buf.
 *
 * Return:      Success:        0
 *
 *              Failure:        negative
 *
 *-------------------------------------------------------------------------
 */
static int
compare_s1_s3_data(void *expect_buf, void *rbuf)
{
    int   i;
    s1_t *s1_ptr;
    s3_t *s2_ptr;

    for (i = 0; i < (int)(NX * NY); i++) {
        s1_ptr = ((s1_t *)expect_buf) + i;
        s2_ptr = ((s3_t *)rbuf) + i;

        if (s1_ptr->a != s2_ptr->a || s1_ptr->b != s2_ptr->b || s1_ptr->c[0] != s2_ptr->c[0] ||
            s1_ptr->c[1] != s2_ptr->c[1] || s1_ptr->c[2] != s2_ptr->c[2] || s1_ptr->c[3] != s2_ptr->c[3] ||
            s1_ptr->d != s2_ptr->d || s1_ptr->e != s2_ptr->e) {
            H5_FAILED();
            printf("    i=%d\n", i);
            puts("    Incorrect values read from the file");
            goto error;
        }
    }

    return SUCCEED;

error:
    return FAIL;

} /* compare_s1_s3_data() */

/*-------------------------------------------------------------------------
 * Function:    compare_s7_data
 *
 * Purpose:     Compare data (s7_t) read in rbuf with expected data in expect_buf.
 *
 * Return:      Success:        0
 *
 *              Failure:        negative
 *
 *-------------------------------------------------------------------------
 */
static int
compare_s7_data(void *expect_buf, void *rbuf)
{
    int   i;
    s7_t *s1_ptr;
    s7_t *s2_ptr;

    for (i = 0; i < (int)(NX * NY); i++) {
        s1_ptr = ((s7_t *)expect_buf) + i;
        s2_ptr = ((s7_t *)rbuf) + i;

        /* Compare only the data */
        if (s1_ptr->a != s2_ptr->a || s1_ptr->d != s2_ptr->d) {
            H5_FAILED();
            printf("    i=%d\n", i);
            printf("    expect_buf:a=%d, d=%d\n", s1_ptr->a, s1_ptr->d);
            printf("    rbuf:a=%d, d=%d", s2_ptr->a, s2_ptr->d);
            goto error;
        }
    } /* end for */

    return SUCCEED;

error:
    return FAIL;

} /* compare_s7_data() */

/*-------------------------------------------------------------------------
 * Function:    compare_s7_s8_data
 *
 * Purpose:     Compare data read in rbuf with expected data
 *              in expect_buf: save_s7, save_s8.
 *
 * Return:      Success:        0
 *
 *              Failure:        negative
 *
 *-------------------------------------------------------------------------
 */
static int
compare_a_d_data(void *exp1_buf, void *exp2_buf, void *rbuf)
{
    int   i;
    s7_t *s1_ptr;
    s8_t *s2_ptr;
    s7_t *rbuf_ptr;

    for (i = 0; i < (int)(NX * NY); i++) {
        s1_ptr   = ((s7_t *)exp1_buf) + i;
        s2_ptr   = ((s8_t *)exp2_buf) + i;
        rbuf_ptr = ((s7_t *)rbuf) + i;

        if (s2_ptr->a != rbuf_ptr->a || s1_ptr->d != rbuf_ptr->d) {
            H5_FAILED();
            printf("    i=%d\n", i);
            printf("    expect_buf:a=%d, d=%d\n", (int32_t)s2_ptr->a, s1_ptr->d);
            printf("    rbuf: a=%d, d=%d", rbuf_ptr->a, rbuf_ptr->d);
            goto error;
        }
    } /* end for */

    return SUCCEED;

error:
    return FAIL;

} /* compare_a_d_data() */

/*-------------------------------------------------------------------------
 * Function:    compare_a_b_c_data
 *
 * Purpose:     Compare data read in rbuf with expected data
 *              in expect_buf: save_s8, save_rbuf8.
 *
 * Return:      Success:        0
 *
 *              Failure:        negative
 *
 *-------------------------------------------------------------------------
 */
static int
compare_a_b_c_data(void *exp1_buf, void *exp2_buf, void *rbuf)
{
    int   i;
    s8_t *s1_ptr;
    s8_t *s2_ptr;
    s8_t *rbuf_ptr;

    for (i = 0; i < (int)(NX * NY); i++) {
        s1_ptr   = ((s8_t *)exp1_buf) + i;
        s2_ptr   = ((s8_t *)exp2_buf) + i;
        rbuf_ptr = ((s8_t *)rbuf) + i;

        if (s1_ptr->a != rbuf_ptr->a || s2_ptr->b != rbuf_ptr->b || s2_ptr->c != rbuf_ptr->c) {
            H5_FAILED();
            printf("    i=%d\n", i);
            printf("    expect_buf:a=%" PRId64 ", b=%" PRId64 ", c=%" PRId64 "\n", s1_ptr->a, s2_ptr->b,
                   s2_ptr->c);
            printf("    rbuf: a=%" PRId64 ", b=%" PRId64 ", c=%" PRId64 "\n", rbuf_ptr->a, rbuf_ptr->b,
                   rbuf_ptr->c);
            goto error;
        }
    } /* end for */

    return SUCCEED;

error:
    return FAIL;

} /* compare_a_b_c_data() */

/*-------------------------------------------------------------------------
 * Function:    test_select_src_subset
 *
 * Purpose:     This is derived from test_hdf5_src_subset() for selection
 *              I/O testing:
 *
 *              Test the optimization of compound data writing, rewriting,
 *              and reading when the source type is a subset of the destination
 *              type.  For example:
 *                  struct source {            struct destination {
 *                      TYPE1 A;      -->          TYPE1 A;
 *                      TYPE2 B;      -->          TYPE2 B;
 *                      TYPE3 C;      -->          TYPE3 C;
 *                  };                             TYPE4 D;
 *                                                 TYPE5 E;
 *                                             };
 *
 * Return:    Success:    0
 *            Failure:    1
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_select_src_subset(char *fname, hid_t fapl, hid_t in_dxpl, unsigned set_fillvalue, unsigned set_buf)
{
    hid_t          fid     = H5I_INVALID_HID;
    hid_t          rew_tid = H5I_INVALID_HID, src_tid = H5I_INVALID_HID;
    hid_t          did           = H5I_INVALID_HID;
    hid_t          sid           = H5I_INVALID_HID;
    hid_t          dcpl          = H5I_INVALID_HID;
    hid_t          dxpl          = H5I_INVALID_HID;
    hsize_t        dims[2]       = {NX, NY};
    hsize_t        chunk_dims[2] = {NX / 10, NY / 10};
    unsigned char *rew_buf = NULL, *save_rew_buf = NULL, *rbuf = NULL;
    stype1         fillvalue;
    size_t         ss, ss1, ss2;

    /* Initialize the fill value */
    memset(&fillvalue, 0, sizeof(stype1));

    /* Create the file for this test */
    if ((fid = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        goto error;

    /* Build hdf5 datatypes */
    if ((src_tid = create_stype1()) < 0)
        goto error;

    if ((rew_tid = create_stype3()) < 0)
        goto error;

    /* Create the data space */
    if ((sid = H5Screate_simple(2, dims, NULL)) < 0)
        goto error;

    /* Allocate space and initialize data */
    rbuf = (unsigned char *)calloc(NX * NY, sizeof(stype3));

    rew_buf = (unsigned char *)calloc(NX * NY, sizeof(stype3));
    initialize_stype3(rew_buf, (size_t)NX * NY);

    /* Save a copy as the buffer may be clobbered due to H5Pset_modify_write_buf() */
    save_rew_buf = (unsigned char *)calloc(NX * NY, sizeof(stype3));
    initialize_stype3(save_rew_buf, (size_t)NX * NY);

    /* Create dataset creation property list */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;

    /*
     * Create contiguous and chunked datasets.
     * Write to the datasets in a different compound subset order
     */
    printf("    test_select_src_subset(): writing data to contiguous and chunked datasets");

    if (set_fillvalue) {
        if (H5Pset_fill_value(dcpl, src_tid, &fillvalue) < 0)
            goto error;
    }

    dxpl = H5Pcopy(in_dxpl);
    if (set_buf) {
        ss1 = H5Tget_size(rew_tid);
        ss2 = H5Tget_size(src_tid);
        ss  = MAX(ss1, ss2) * NX * NY;

        if (H5Pset_buffer(dxpl, ss, NULL, NULL) < 0)
            goto error;
    }

    /* Create contiguous data set */
    if ((did = H5Dcreate2(fid, DSET_NAME[0], src_tid, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    /* Write to the dataset with rew_tid */
    if (H5Dwrite(did, rew_tid, H5S_ALL, H5S_ALL, dxpl, rew_buf) < 0)
        goto error;

    if (H5Dread(did, rew_tid, H5S_ALL, H5S_ALL, dxpl, rbuf) < 0)
        goto error;

    if (memcmp(save_rew_buf, rbuf, sizeof(stype3) * NX * NY) != 0)
        goto error;

    if (H5Dclose(did) < 0)
        goto error;

    /* Set chunking */
    if (H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        goto error;

    /* Create chunked data set */
    if ((did = H5Dcreate2(fid, DSET_NAME[1], src_tid, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    /* Write to the dataset with rew_tid */
    if (H5Dwrite(did, rew_tid, H5S_ALL, H5S_ALL, dxpl, rew_buf) < 0)
        FAIL_STACK_ERROR;

    if (H5Dread(did, rew_tid, H5S_ALL, H5S_ALL, dxpl, rbuf) < 0)
        goto error;

    if (memcmp(save_rew_buf, rbuf, sizeof(stype3) * NX * NY) != 0)
        goto error;

    if (H5Dclose(did) < 0)
        goto error;

    /* Finishing test and release resources */
    if (H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;

    if (H5Pclose(dcpl) < 0)
        FAIL_STACK_ERROR;

    if (H5Pclose(dxpl) < 0)
        FAIL_STACK_ERROR;

    if (H5Tclose(src_tid) < 0)
        FAIL_STACK_ERROR;

    if (H5Tclose(rew_tid) < 0)
        FAIL_STACK_ERROR;

    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    free(rbuf);
    free(rew_buf);
    free(save_rew_buf);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(sid);
        H5Pclose(dcpl);
        H5Pclose(dxpl);
        H5Dclose(did);
        H5Fclose(fid);
        H5Tclose(src_tid);
        H5Tclose(rew_tid);
    }
    H5E_END_TRY

    if (rbuf)
        free(rbuf);
    if (rew_buf)
        free(rew_buf);
    if (save_rew_buf)
        free(save_rew_buf);

    printf("\n*** SELECT SRC SUBSET TEST FAILED ***\n");
    return 1;
} /* test_select_src_subset() */

/*-------------------------------------------------------------------------
 * Function:    test_select_dst_subset
 *
 * Purpose:     This is derived from test_hdf5_dst_subset() for selection
 *              I/O testing:

 *              Test the optimization of compound data writing, rewriting,
 *              and reading when the destination type is a subset of the
 *              source type.  For example:
 *                  struct source {            struct destination {
 *                      TYPE1 A;      -->          TYPE1 A;
 *                      TYPE2 B;      -->          TYPE2 B;
 *                      TYPE3 C;      -->          TYPE3 C;
 *                      TYPE4 D;               }
 *                      TYPE5 E;
 *                  };
 *
 * Return:    Success:    0
 *            Failure:    1
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_select_dst_subset(char *fname, hid_t fapl, hid_t in_dxpl, unsigned set_fillvalue, unsigned set_buf)
{
    hid_t          fid     = H5I_INVALID_HID;
    hid_t          rew_tid = H5I_INVALID_HID, src_tid = H5I_INVALID_HID;
    hid_t          did           = H5I_INVALID_HID;
    hid_t          sid           = H5I_INVALID_HID;
    hid_t          dcpl          = H5I_INVALID_HID;
    hid_t          dxpl          = H5I_INVALID_HID;
    hsize_t        dims[2]       = {NX, NY};
    hsize_t        chunk_dims[2] = {NX / 10, NY / 10};
    unsigned char *rew_buf = NULL, *save_rew_buf = NULL, *rbuf = NULL;
    stype2         fillvalue;
    size_t         ss, ss1, ss2;

    /* Initialize the fill value */
    memset(&fillvalue, 0, sizeof(stype2));

    /* Create the file for this test */
    if ((fid = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        goto error;

    /* Build hdf5 datatypes */
    if ((src_tid = create_stype2()) < 0)
        goto error;

    if ((rew_tid = create_stype4()) < 0)
        goto error;

    /* Create the data space */
    if ((sid = H5Screate_simple(2, dims, NULL)) < 0)
        goto error;

    rbuf = (unsigned char *)calloc(NX * NY, sizeof(stype4));

    rew_buf = (unsigned char *)calloc(NX * NY, sizeof(stype4));
    initialize_stype4(rew_buf, (size_t)NX * NY);

    /* Save a copy as the buffer may be clobbered due to H5Pset_modify_write_buf() */
    save_rew_buf = (unsigned char *)calloc(NX * NY, sizeof(stype4));
    initialize_stype4(save_rew_buf, (size_t)NX * NY);

    /* Create dataset creation property list */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;

    /*
     * Write data to contiguous and chunked datasets.
     */
    printf("    test_select_dst_subset(): writing data to contiguous and chunked datasets");

    if (set_fillvalue) {
        if (H5Pset_fill_value(dcpl, src_tid, &fillvalue) < 0)
            goto error;
    }

    dxpl = H5Pcopy(in_dxpl);
    if (set_buf) {
        ss1 = H5Tget_size(rew_tid);
        ss2 = H5Tget_size(src_tid);
        ss  = MAX(ss1, ss2) * NX * NY;

        if (H5Pset_buffer(dxpl, ss, NULL, NULL) < 0)
            goto error;
    }

    /* Create contiguous data set */
    if ((did = H5Dcreate2(fid, DSET_NAME[2], src_tid, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    /* Write to the dataset with rew_tid */
    if (H5Dwrite(did, rew_tid, H5S_ALL, H5S_ALL, dxpl, rew_buf) < 0)
        goto error;

    /* Read from the dataset with rew_tid */
    if (H5Dread(did, rew_tid, H5S_ALL, H5S_ALL, dxpl, rbuf) < 0)
        goto error;

    if (compare_stype4_data(save_rew_buf, rbuf) < 0)
        goto error;

    if (H5Dclose(did) < 0)
        goto error;

    /* Set chunking */
    if (H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        goto error;

    /* Create chunked data set */
    if ((did = H5Dcreate2(fid, DSET_NAME[3], src_tid, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    initialize_stype4(rew_buf, (size_t)NX * NY);

    /* Write data to the dataset with rew_tid */
    if (H5Dwrite(did, rew_tid, H5S_ALL, H5S_ALL, dxpl, rew_buf) < 0)
        goto error;

    /* Read frm the dataset with rew_tid */
    if (H5Dread(did, rew_tid, H5S_ALL, H5S_ALL, dxpl, rbuf) < 0)
        goto error;

    if (compare_stype4_data(save_rew_buf, rbuf) < 0)
        goto error;

    if (H5Dclose(did) < 0)
        goto error;

    /* Finishing test and release resources */
    if (H5Sclose(sid) < 0)
        goto error;

    if (H5Pclose(dcpl) < 0)
        goto error;

    if (H5Pclose(dxpl) < 0)
        FAIL_STACK_ERROR;

    if (H5Tclose(src_tid) < 0)
        goto error;

    if (H5Tclose(rew_tid) < 0)
        goto error;
    if (H5Fclose(fid) < 0)
        goto error;

    free(rbuf);
    free(rew_buf);
    free(save_rew_buf);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(sid);
        H5Pclose(dcpl);
        H5Pclose(dxpl);
        H5Dclose(did);
        H5Fclose(fid);
        H5Tclose(src_tid);
        H5Tclose(rew_tid);
    }
    H5E_END_TRY

    if (rbuf)
        free(rbuf);
    if (rew_buf)
        free(rew_buf);
    if (save_rew_buf)
        free(save_rew_buf);

    printf("\n*** SELECT DST SUBSET TEST FAILED ***\n");
    return 1;
} /* test_select_dst_subset */

/*-------------------------------------------------------------------------
 * Function:    test_select_compound
 *
 * Purpose:     This is derived from test_comppound() for selection I/O
 *              testing:
 *
 *              --Creates a simple dataset of a compound type and then
 *              writes it in original and reverse order.
 *              --Creates another dataset to verify the CI window
 *              is fixed.
 *
 * Return:      Success:    0
 *              Failure:    1
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_select_compound(char *fname, hid_t fapl, hid_t in_dxpl, unsigned set_fillvalue, unsigned set_buf)
{
    hid_t s1_tid = H5I_INVALID_HID;
    hid_t s3_tid = H5I_INVALID_HID;
    hid_t s7_tid = H5I_INVALID_HID;
    hid_t s8_tid = H5I_INVALID_HID;

    /* Buffers */
    s1_t *s1      = NULL;
    s1_t *save_s1 = NULL;
    s3_t *s3      = NULL;
    s3_t *save_s3 = NULL;
    s1_t *rbuf1   = NULL;
    s3_t *rbuf3   = NULL;

    s7_t *s7      = NULL;
    s7_t *save_s7 = NULL;
    s7_t *rbuf7   = NULL;

    s8_t *s8         = NULL;
    s8_t *save_s8    = NULL;
    s8_t *rbuf8      = NULL;
    s8_t *save_rbuf8 = NULL;

    /* Other variables */
    unsigned int   i;
    hid_t          fid      = H5I_INVALID_HID;
    hid_t          did      = H5I_INVALID_HID;
    hid_t          sid      = H5I_INVALID_HID;
    hid_t          dcpl     = H5I_INVALID_HID;
    hid_t          dxpl     = H5I_INVALID_HID;
    hid_t          array_dt = H5I_INVALID_HID;
    static hsize_t dim[]    = {NX, NY};
    s1_t           fillvalue1;
    s7_t           fillvalue7;
    size_t         ss = 0, ss1 = 0, ss2 = 0;
    hsize_t        memb_size[1] = {4};

    /* Initialize the fill values */
    memset(&fillvalue1, 0, sizeof(s1_t));
    memset(&fillvalue7, 0, sizeof(s7_t));

    /* Allocate buffers */
    if (NULL == (s1 = (s1_t *)calloc(NX * NY, sizeof(s1_t))))
        goto error;
    if (NULL == (save_s1 = (s1_t *)calloc(NX * NY, sizeof(s1_t))))
        goto error;
    if (NULL == (rbuf1 = (s1_t *)calloc(NX * NY, sizeof(s1_t))))
        goto error;
    if (NULL == (s3 = (s3_t *)calloc(NX * NY, sizeof(s3_t))))
        goto error;
    if (NULL == (save_s3 = (s3_t *)calloc(NX * NY, sizeof(s3_t))))
        goto error;
    if (NULL == (rbuf3 = (s3_t *)calloc(NX * NY, sizeof(s3_t))))
        goto error;

    if (NULL == (s7 = (s7_t *)calloc(NX * NY, sizeof(s7_t))))
        goto error;
    if (NULL == (save_s7 = (s7_t *)calloc(NX * NY, sizeof(s7_t))))
        goto error;
    if (NULL == (rbuf7 = (s7_t *)calloc(NX * NY, sizeof(s7_t))))
        goto error;

    if (NULL == (s8 = (s8_t *)calloc(NX * NY, sizeof(s8_t))))
        goto error;
    if (NULL == (save_s8 = (s8_t *)calloc(NX * NY, sizeof(s8_t))))
        goto error;
    if (NULL == (rbuf8 = (s8_t *)calloc(NX * NY, sizeof(s8_t))))
        goto error;
    if (NULL == (save_rbuf8 = (s8_t *)calloc(NX * NY, sizeof(s8_t))))
        goto error;

    /* Create the file */
    if ((fid = H5Fcreate(fname, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) {
        goto error;
    }

    /* Create the data space */
    if ((sid = H5Screate_simple(2, dim, NULL)) < 0)
        goto error;

    /* Create dataset creation property list */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;

    /* Create a copy of the incoming dataset transfer property list */
    if ((dxpl = H5Pcopy(in_dxpl)) < 0)
        goto error;

    /*
     * Create and write to the dataset in original compound struct members order
     */
    printf("    test_select_compound(): basic compound write");

    /* Initialize buffer with s1_t */
    for (i = 0; i < NX * NY; i++) {
        s1[i].a    = 8 * i + 0;
        s1[i].b    = 2000 + 2 * i;
        s1[i].c[0] = 8 * i + 2;
        s1[i].c[1] = 8 * i + 3;
        s1[i].c[2] = 8 * i + 4;
        s1[i].c[3] = 8 * i + 5;
        s1[i].d    = 2001 + 2 * i;
        s1[i].e    = 8 * i + 7;
    }
    memcpy(save_s1, s1, sizeof(s1_t) * NX * NY);

    /* Create file type s1_t */
    if ((s1_tid = H5Tcreate(H5T_COMPOUND, sizeof(s1_t))) < 0)
        goto error;
    array_dt = H5Tarray_create2(H5T_NATIVE_INT, 1, memb_size);
    if (H5Tinsert(s1_tid, "a", HOFFSET(s1_t, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s1_tid, "b", HOFFSET(s1_t, b), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s1_tid, "c", HOFFSET(s1_t, c), array_dt) < 0 ||
        H5Tinsert(s1_tid, "d", HOFFSET(s1_t, d), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s1_tid, "e", HOFFSET(s1_t, e), H5T_NATIVE_INT) < 0)
        goto error;
    H5Tclose(array_dt);

    /* Set fill value accordingly */
    if (set_fillvalue) {
        if (H5Pset_fill_value(dcpl, s1_tid, &fillvalue1) < 0)
            goto error;
    }

    /* Create the dataset with file type s1_tid */
    if ((did = H5Dcreate2(fid, "s1", s1_tid, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    /* Set buffer size accordingly */
    if (set_buf) {
        ss1 = H5Tget_size(s1_tid);

        if (H5Pset_buffer(dxpl, ss1, NULL, NULL) < 0)
            goto error;
    }

    /* Write to the dataset with file type s1_tid */
    if (H5Dwrite(did, s1_tid, H5S_ALL, H5S_ALL, dxpl, s1) < 0)
        goto error;

    /* Read from the dataset with file type s1_tid */
    if (H5Dread(did, s1_tid, H5S_ALL, H5S_ALL, dxpl, rbuf1) < 0)
        goto error;

    /* Verify data is correct */
    if (compare_s1_data(save_s1, rbuf1) < 0)
        goto error;

    PASSED();

    /*
     * Write to the dataset with s3 memory buffer. This buffer
     * has the same data space but the data type is different: the
     * data type is a struct whose members are in the opposite order.
     */
    printf("    test_select_compound(): reversal of struct members");

    /* Create mem type s3_tid */
    if ((s3_tid = H5Tcreate(H5T_COMPOUND, sizeof(s3_t))) < 0)
        goto error;
    array_dt = H5Tarray_create2(H5T_NATIVE_INT, 1, memb_size);
    if (H5Tinsert(s3_tid, "a", HOFFSET(s3_t, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s3_tid, "b", HOFFSET(s3_t, b), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s3_tid, "c", HOFFSET(s3_t, c), array_dt) < 0 ||
        H5Tinsert(s3_tid, "d", HOFFSET(s3_t, d), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s3_tid, "e", HOFFSET(s3_t, e), H5T_NATIVE_INT) < 0)
        goto error;
    H5Tclose(array_dt);

    /* Initialize buffer with s3_t */
    for (i = 0; i < NX * NY; i++) {
        s3[i].a    = 8 * i + 0;
        s3[i].b    = 2000 + 2 * i;
        s3[i].c[0] = 8 * i + 2;
        s3[i].c[1] = 8 * i + 3;
        s3[i].c[2] = 8 * i + 4;
        s3[i].c[3] = 8 * i + 5;
        s3[i].d    = 2001 + 2 * i;
        s3[i].e    = 8 * i + 7;
    }

    memcpy(save_s3, s3, sizeof(s3_t) * NX * NY);

    /* Set buffer size accordingly */
    if (set_buf) {
        /* ss1 is set already previously */
        ss2 = H5Tget_size(s3_tid);
        ss  = MAX(ss1, ss2) * NX * NY;

        if (H5Pset_buffer(dxpl, ss, NULL, NULL) < 0)
            goto error;
    }

    /* Read from the dataset with mem type s3_tid */
    if (H5Dread(did, s3_tid, H5S_ALL, H5S_ALL, dxpl, rbuf3) < 0)
        goto error;

    if (compare_s1_s3_data(save_s1, rbuf3) < 0)
        goto error;

    if (H5Dclose(did) < 0)
        goto error;

    PASSED();

    printf("    test_select_compound(): verify fix for non-optimized compound conversions with memory type "
           "larger than file ");

    /* Create file type s7_tid */
    if ((s7_tid = H5Tcreate(H5T_COMPOUND, sizeof(s7_t))) < 0)
        goto error;

    if (H5Tinsert(s7_tid, "a", HOFFSET(s7_t, a), H5T_NATIVE_INT32) < 0 ||
        H5Tinsert(s7_tid, "d", HOFFSET(s7_t, d), H5T_NATIVE_INT32) < 0)
        goto error;

    /* Initialize buffer with s7_t */
    for (i = 0; i < NX * NY; i++) {
        s7[i].a = (int32_t)(2 * i);
        s7[i].d = (int32_t)(2 * i + 1);
    }
    memcpy(save_s7, s7, sizeof(s7_t) * NX * NY);

    /* Create mem type s8_tid */
    if ((s8_tid = H5Tcreate(H5T_COMPOUND, sizeof(s8_t))) < 0)
        goto error;

    if (H5Tinsert(s8_tid, "a", HOFFSET(s8_t, a), H5T_NATIVE_INT64) < 0 ||
        H5Tinsert(s8_tid, "b", HOFFSET(s8_t, b), H5T_NATIVE_INT64) < 0 ||
        H5Tinsert(s8_tid, "c", HOFFSET(s8_t, c), H5T_NATIVE_INT64) < 0)
        goto error;

    /* Initialize buffer with s8_t */
    for (i = 0; i < NX * NY; i++) {
        s8[i].a = (int64_t)(2 * NX * NY + 3 * i);
        s8[i].b = (int64_t)(2 * NX * NY + 3 * i + 1);
        s8[i].c = (int64_t)(2 * NX * NY + 3 * i + 2);
    }
    memcpy(save_s8, s8, sizeof(s8_t) * NX * NY);

    /* Set fill value accordingly */
    if (set_fillvalue) {
        if (H5Pset_fill_value(dcpl, s7_tid, &fillvalue7) < 0)
            goto error;
    }

    /* Set buffer size accordingly */
    if (set_buf) {
        ss1 = H5Tget_size(s7_tid);
        ss2 = H5Tget_size(s8_tid);
        ss  = MAX(ss1, ss2) * NX * NY;

        if (H5Pset_buffer(dxpl, ss, NULL, NULL) < 0)
            goto error;
    }

    /* Create dataset with file type s7_tid */
    if ((did = H5Dcreate2(fid, "ss", s7_tid, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    /* Write to the dataset with mem type s7_tid */
    if (H5Dwrite(did, s7_tid, H5S_ALL, H5S_ALL, dxpl, s7) < 0)
        goto error;

    /* Read from the dataset with mem type s7_tid */
    if (H5Dread(did, s7_tid, H5S_ALL, H5S_ALL, dxpl, rbuf7) < 0)
        goto error;

    /* Verify data read is correct */
    if (compare_s7_data(save_s7, rbuf7) < 0)
        goto error;

    /* Write to the dataset with mem type s8_tid */
    if (H5Dwrite(did, s8_tid, H5S_ALL, H5S_ALL, dxpl, s8) < 0)
        goto error;

    /* Read from the dataset with mem type s7_tid */
    memset(rbuf7, 0, NX * NY * sizeof(s7_t));
    if (H5Dread(did, s7_tid, H5S_ALL, H5S_ALL, dxpl, rbuf7) < 0)
        goto error;

    /* Verify a: save_s8, d: save_s7 */
    if (compare_a_d_data(save_s7, save_s8, rbuf7) < 0)
        goto error;

    /* Initialize read buffer of s8_t with unique values */
    for (i = 0; i < NX * NY; i++) {
        rbuf8[i].a = (int64_t)(5 * NX * NY + 3 * i);
        rbuf8[i].b = (int64_t)(5 * NX * NY + 3 * i + 1);
        rbuf8[i].c = (int64_t)(5 * NX * NY + 3 * i + 2);
    }
    memcpy(save_rbuf8, rbuf8, sizeof(s8_t) * NX * NY);

    /* Read from the dataset with mem type s8_tid */
    if (H5Dread(did, s8_tid, H5S_ALL, H5S_ALL, dxpl, rbuf8) < 0)
        goto error;

    /* Verify a: save_s8; b, c: save_rbuf8 */
    if (compare_a_b_c_data(save_s8, save_rbuf8, rbuf8) < 0)
        goto error;

    if (H5Dclose(did) < 0)
        goto error;

    PASSED();

    /*
     * Release resources.
     */
    if (H5Sclose(sid) < 0)
        goto error;

    if (H5Pclose(dcpl) < 0)
        goto error;

    if (H5Pclose(dxpl) < 0)
        goto error;

    if (H5Tclose(s1_tid) < 0)
        goto error;

    if (H5Tclose(s3_tid) < 0)
        goto error;

    if (H5Tclose(s7_tid) < 0)
        goto error;

    if (H5Tclose(s8_tid) < 0)
        goto error;

    if (H5Fclose(fid) < 0)
        goto error;

    /* Release buffers */
    free(s1);
    free(save_s1);
    free(s3);
    free(save_s3);
    free(rbuf1);
    free(rbuf3);
    free(s7);
    free(save_s7);
    free(s8);
    free(save_s8);
    free(rbuf7);
    free(rbuf8);
    free(save_rbuf8);

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(sid);
        H5Pclose(dcpl);
        H5Pclose(dxpl);
        H5Dclose(did);
        H5Fclose(fid);
        H5Tclose(s1_tid);
        H5Tclose(s3_tid);
        H5Tclose(s7_tid);
        H5Tclose(s8_tid);
    }
    H5E_END_TRY

    /* Release resources */
    free(s1);
    free(save_s1);
    free(s3);
    free(save_s3);
    free(rbuf1);
    free(rbuf3);
    free(s7);
    free(save_s7);
    free(s8);
    free(save_s8);
    free(rbuf7);
    free(rbuf8);
    free(save_rbuf8);

    printf("\n*** SELECT COMPOUND DATASET TESTS FAILED ***\n");
    return 1;
} /* test_select_compound() */

/*
 * Purpose:     Tests for selection I/O with compound types:
 *              --set_cache: set chunk cache to 0 or not
 *                via H5Pset_cache(fapl...)
 *              --set_fillvalue: set fill value or not
 *                via H5Pset_fill_value(dcpl...)
 *              --select_io: enable selection I/O or not
 *                via H5Pset_selection_io(dxpl...)
 *              --mwbuf: with or without modifying write buffers
 *                via H5Pset_modify_write_buf(dxpl...)
 *              --set_buf: with or without setting the maximum size
 *                for the type conversion buffer and background buffer
 *                via H5Pset_buffer(dxpl...)
 *
 *              These tests will test the selection I/O pipeline in particular
 *              triggering H5D__scatgath_read()/write(),
 *              H5D__scatgath_write_select_read()/write(),
 *              and with/without the optimized compound read/write.
 */
static unsigned
test_compounds_selection_io(void)
{
    unsigned nerrs = 0;
    unsigned set_cache;     /* Set cache to 0 or not */
    unsigned set_fillvalue; /* Set fill value or not */
    unsigned select_io;     /* Enable selection I/O or not */
    unsigned mwbuf;         /* With or without modifying write buffers */
    unsigned set_buf;       /* With or without H5Pset_buffer */
    hid_t    fapl = -1;
    hid_t    dxpl = -1;
    char     fname[256];

    fapl = h5_fileaccess();
    h5_fixname(FILENAME[3], fapl, fname, sizeof(fname));

    for (set_cache = FALSE; set_cache <= TRUE; set_cache++) {
        for (set_fillvalue = FALSE; set_fillvalue <= TRUE; set_fillvalue++) {
            for (select_io = FALSE; select_io <= TRUE; select_io++) {
                for (mwbuf = FALSE; mwbuf <= TRUE; mwbuf++) {
                    for (set_buf = FALSE; set_buf <= TRUE; set_buf++) {

                        if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
                            goto error;

                        if (set_cache) {
                            printf("  With chunk cache set 0, ");
                            if (H5Pset_cache(fapl, 0, (size_t)0, (size_t)0, 0.0) < 0)
                                goto error;
                        }
                        else
                            printf("  With default chunk cache, ");

                        if (set_fillvalue)
                            printf("set fill value, ");
                        else
                            printf("not set fill value, ");

                        if (select_io) {
                            printf("selection I/O ON, ");
                            if (H5Pset_selection_io(dxpl, H5D_SELECTION_IO_MODE_ON) < 0)
                                goto error;
                        }
                        else
                            printf("selection I/O OFF, ");

                        if (mwbuf) {
                            printf("with modify write buf, ");
                            if (H5Pset_modify_write_buf(dxpl, TRUE) < 0)
                                goto error;
                        }
                        else
                            printf("without modify write buf, ");

                        if (set_buf)
                            printf("with H5Pset_buffer:\n");
                        else
                            printf("without H5Pset_buffer:\n");

                        nerrs += test_select_compound(fname, fapl, dxpl, set_fillvalue, set_buf);
                        nerrs += test_select_src_subset(fname, fapl, dxpl, set_fillvalue, set_buf);
                        nerrs += test_select_dst_subset(fname, fapl, dxpl, set_fillvalue, set_buf);

                        if (H5Pclose(dxpl) < 0)
                            goto error;

                    } /* set_buf */
                }     /* mwbuf */
            }         /* select_io */
        }             /* set_fillvalue */
    }                 /* set_cache */

    if (H5Pclose(fapl) < 0)
        goto error;

    if (nerrs)
        goto error;

    return 0;

error:
    printf("*** COMPOUNDS TESTS FOR SELECTION I/O FAILED ***");

    return 1;
} /* test_compounds_selection_io() */

/*-------------------------------------------------------------------------
 * Function:    test_compound
 *
 * Purpose:    Creates a simple dataset of a compound type and then reads
 *        it back.  The dataset is read back in various ways to
 *        exercise the I/O pipeline and compound type conversion.
 *
 * Return:    Success:    0
 *
 *        Failure:    1
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_compound(char *filename, hid_t fapl)
{
    /* First dataset */
    s1_t *s1 = NULL;
    hid_t s1_tid;

    /* Second dataset */
    s2_t *s2 = NULL;
    hid_t s2_tid;

    /* Third dataset */
    s3_t *s3 = NULL;
    hid_t s3_tid;

    /* Fourth dataset */
    s4_t *s4 = NULL;
    hid_t s4_tid;

    /* Fifth dataset */
    s5_t *s5 = NULL;
    hid_t s5_tid;

    /* Sixth dataset */
    s6_t *s6 = NULL;
    hid_t s6_tid;

    /* Seventh dataset */
    hid_t s7_sid;

    /* Eighth dataset */
    s1_t *s8 = NULL;
    hid_t s8_f_sid; /*file data space        */
    hid_t s8_m_sid; /*memory data space        */

    /* Ninth dataset */

    /* Tenth dataset */

    /* Eleventh dataset */
    s4_t *s11 = NULL;

    /* Other variables */
    unsigned int   i, j;
    hid_t          file, dataset, space, PRESERVE;
    hid_t          array_dt;
    static hsize_t dim[] = {NX, NY};
    hsize_t        f_offset[2]; /*offset of hyperslab in file    */
    hsize_t        h_size[2];   /*size of hyperslab        */
    hsize_t        memb_size[1] = {4};
    int            ret_code;

    /* Allocate buffers for datasets */
    if (NULL == (s1 = (s1_t *)malloc(sizeof(s1_t) * NX * NY)))
        goto error;
    if (NULL == (s2 = (s2_t *)malloc(sizeof(s2_t) * NX * NY)))
        goto error;
    if (NULL == (s3 = (s3_t *)malloc(sizeof(s3_t) * NX * NY)))
        goto error;
    if (NULL == (s4 = (s4_t *)malloc(sizeof(s4_t) * NX * NY)))
        goto error;
    if (NULL == (s5 = (s5_t *)malloc(sizeof(s5_t) * NX * NY)))
        goto error;
    if (NULL == (s6 = (s6_t *)malloc(sizeof(s6_t) * NX * NY)))
        goto error;

    /* Create the file */
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) {
        goto error;
    }

    /* Create the data space */
    if ((space = H5Screate_simple(2, dim, NULL)) < 0)
        goto error;

    /* Create xfer properties to preserve initialized data */
    /* Also verify H5Pset_preserve is initially 0 and then is set to 1. */
    if ((PRESERVE = H5Pcreate(H5P_DATASET_XFER)) < 0)
        goto error;
    if ((ret_code = H5Pget_preserve(PRESERVE)) != 0) {
        printf("Preserve status of dataset transfer property list should be"
               " 0 (false), got %d\n",
               ret_code);
        goto error;
    }
    if (H5Pset_preserve(PRESERVE, 1) < 0)
        goto error;
    if ((ret_code = H5Pget_preserve(PRESERVE)) != 1) {
        printf("Preserve status of dataset transfer property list should be"
               " 1 (true), got %d\n",
               ret_code);
        goto error;
    }

    /*
     *######################################################################
     * STEP 1: Save the original dataset natively.
     */
    TESTING("basic compound write");

    /* Initialize the dataset */
    for (i = 0; i < NX * NY; i++) {
        s1[i].a    = 8 * i + 0;
        s1[i].b    = 2000 + 2 * i;
        s1[i].c[0] = 8 * i + 2;
        s1[i].c[1] = 8 * i + 3;
        s1[i].c[2] = 8 * i + 4;
        s1[i].c[3] = 8 * i + 5;
        s1[i].d    = 2001 + 2 * i;
        s1[i].e    = 8 * i + 7;
    }

    /* Create the memory data type */
    if ((s1_tid = H5Tcreate(H5T_COMPOUND, sizeof(s1_t))) < 0)
        goto error;
    array_dt = H5Tarray_create2(H5T_NATIVE_INT, 1, memb_size);
    if (H5Tinsert(s1_tid, "a", HOFFSET(s1_t, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s1_tid, "b", HOFFSET(s1_t, b), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s1_tid, "c", HOFFSET(s1_t, c), array_dt) < 0 ||
        H5Tinsert(s1_tid, "d", HOFFSET(s1_t, d), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s1_tid, "e", HOFFSET(s1_t, e), H5T_NATIVE_INT) < 0)
        goto error;
    H5Tclose(array_dt);

    /* Create the dataset */
    if ((dataset = H5Dcreate2(file, "s1", s1_tid, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    /* Write the data */
    if (H5Dwrite(dataset, s1_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s1) < 0)
        goto error;

    PASSED();

    /*
     *######################################################################
     * STEP 2: We create a new type ID for the second dataset even though
     *            it's the same as the first just to test things better, but
     *           in fact, we could have used s1_tid.
     */
    TESTING("basic compound read");

    /* Create a data type for s2 */
    if ((s2_tid = H5Tcreate(H5T_COMPOUND, sizeof(s2_t))) < 0)
        goto error;
    array_dt = H5Tarray_create2(H5T_NATIVE_INT, 1, memb_size);
    if (H5Tinsert(s2_tid, "a", HOFFSET(s2_t, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s2_tid, "b", HOFFSET(s2_t, b), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s2_tid, "c", HOFFSET(s2_t, c), array_dt) < 0 ||
        H5Tinsert(s2_tid, "d", HOFFSET(s2_t, d), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s2_tid, "e", HOFFSET(s2_t, e), H5T_NATIVE_INT) < 0)
        goto error;
    H5Tclose(array_dt);

    /* Read the data */
    if (H5Dread(dataset, s2_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s2) < 0) {
        goto error;
    }

    /* Compare s2 with s1.  They should be the same */
    for (i = 0; i < NX * NY; i++) {
        if (s1[i].a != s2[i].a || s1[i].b != s2[i].b || s1[i].c[0] != s2[i].c[0] ||
            s1[i].c[1] != s2[i].c[1] || s1[i].c[2] != s2[i].c[2] || s1[i].c[3] != s2[i].c[3] ||
            s1[i].d != s2[i].d || s1[i].e != s2[i].e) {
            H5_FAILED();
            puts("    Incorrect values read from the file");
            goto error;
        }
    }
    PASSED();

    /*
     *######################################################################
     * STEP 3: Read the dataset back into a third memory buffer. This buffer
     *            has the same data space but the data type is different: the
     *           data type is a struct whose members are in the opposite order.
     */
    TESTING("reversal of struct members");

    /* Create a data type for s3 */
    if ((s3_tid = H5Tcreate(H5T_COMPOUND, sizeof(s3_t))) < 0)
        goto error;
    array_dt = H5Tarray_create2(H5T_NATIVE_INT, 1, memb_size);
    if (H5Tinsert(s3_tid, "a", HOFFSET(s3_t, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s3_tid, "b", HOFFSET(s3_t, b), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s3_tid, "c", HOFFSET(s3_t, c), array_dt) < 0 ||
        H5Tinsert(s3_tid, "d", HOFFSET(s3_t, d), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s3_tid, "e", HOFFSET(s3_t, e), H5T_NATIVE_INT) < 0)
        goto error;
    H5Tclose(array_dt);

    /* Read the data */
    if (H5Dread(dataset, s3_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s3) < 0) {
        goto error;
    }

    /* Compare s3 with s1.  They should be the same */
    for (i = 0; i < NX * NY; i++) {
        if (s1[i].a != s3[i].a || s1[i].b != s3[i].b || s1[i].c[0] != s3[i].c[0] ||
            s1[i].c[1] != s3[i].c[1] || s1[i].c[2] != s3[i].c[2] || s1[i].c[3] != s3[i].c[3] ||
            s1[i].d != s3[i].d || s1[i].e != s3[i].e) {
            H5_FAILED();
            puts("    Incorrect values read from the file");
            goto error;
        }
    }
    PASSED();

    /*
     *######################################################################
     * STEP 4: Read a subset of the members.  Of the <a,b,c,d,e> members
     *         stored on disk we'll read <b,d>.
     */
    TESTING("subset struct read");

    /* Create a datatype for s4 */
    if ((s4_tid = H5Tcreate(H5T_COMPOUND, sizeof(s4_t))) < 0)
        goto error;
    if (H5Tinsert(s4_tid, "b", HOFFSET(s4_t, b), H5T_NATIVE_INT) < 0)
        goto error;
    if (H5Tinsert(s4_tid, "d", HOFFSET(s4_t, d), H5T_NATIVE_INT) < 0)
        goto error;

    /* Read the data */
    if (H5Dread(dataset, s4_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s4) < 0) {
        goto error;
    }

    /* Compare s4 with s1 */
    for (i = 0; i < NX * NY; i++) {
        if (s1[i].b != s4[i].b || s1[i].d != s4[i].d) {
            H5_FAILED();
            puts("    Incorrect values read from the file");
            goto error;
        }
    }
    PASSED();

    /*
     *######################################################################
     * STEP 5: Read all the members into a struct which has other members
     *            which have already been initialized.
     */
    TESTING("partially initialized superset read");

    /* Initialize some members */
    for (i = 0; i < NX * NY; i++) {
        s5[i].pre  = 1000 + 4 * i;
        s5[i].mid1 = 1001 + 4 * i;
        s5[i].mid2 = 1002 + 4 * i;
        s5[i].post = 1003 + 4 * i;
    }

    /* Create a data type for s5 */
    if ((s5_tid = H5Tcreate(H5T_COMPOUND, sizeof(s5_t))) < 0)
        goto error;
    array_dt = H5Tarray_create2(H5T_NATIVE_INT, 1, memb_size);
    if (H5Tinsert(s5_tid, "a", HOFFSET(s5_t, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s5_tid, "b", HOFFSET(s5_t, b), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s5_tid, "c", HOFFSET(s5_t, c), array_dt) < 0 ||
        H5Tinsert(s5_tid, "d", HOFFSET(s5_t, d), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s5_tid, "e", HOFFSET(s5_t, e), H5T_NATIVE_INT))
        goto error;
    H5Tclose(array_dt);

    /* Read the data */
    if (H5Dread(dataset, s5_tid, H5S_ALL, H5S_ALL, PRESERVE, s5) < 0) {
        goto error;
    }

    /* Check that the data was read properly */
    for (i = 0; i < NX * NY; i++) {
        if (s1[i].a != s5[i].a || s1[i].b != s5[i].b || s1[i].c[0] != s5[i].c[0] ||
            s1[i].c[1] != s5[i].c[1] || s1[i].c[2] != s5[i].c[2] || s1[i].c[3] != s5[i].c[3] ||
            s1[i].d != s5[i].d || s1[i].e != s5[i].e) {
            H5_FAILED();
            puts("    Incorrect values read from the file");
            goto error;
        }
    }

    /* Check that no previous values were clobbered */
    for (i = 0; i < NX * NY; i++) {
        if (s5[i].pre != 1000 + 4 * i || s5[i].mid1 != 1001 + 4 * i || s5[i].mid2 != 1002 + 4 * i ||
            s5[i].post != 1003 + 4 * i) {
            H5_FAILED();
            puts("    Memory values were clobbered");
            goto error;
        }
    }
    PASSED();

    /*
     *######################################################################
     * STEP 6: Read all the members into a struct which has other members
     *            which have already been initialized.  This is to test the
     *         optimization for the Chicago company.  The optimization is
     *         for the special case when the source members are a subset of
     *         destination, and the order is the same, and no conversion
     *         is needed.  For example:
     *              struct source {            struct destination {
     *                  TYPE1 A;      -->          TYPE1 A;
     *                  TYPE2 B;      -->          TYPE2 B;
     *                  TYPE3 C;      -->          TYPE3 C;
     *              };                             TYPE4 D;
     *                                             TYPE5 E;
     *                                         };
     */
    TESTING("partially initialized superset optimized read");

    /* Initialize some members */
    for (i = 0; i < NX * NY; i++) {
        s6[i].pre  = 1000 + 4 * i;
        s6[i].mid1 = 1001 + 4 * i;
        s6[i].mid2 = 1002 + 4 * i;
        s6[i].post = 1003 + 4 * i;
    }

    /* Create a data type for s6 */
    if ((s6_tid = H5Tcreate(H5T_COMPOUND, sizeof(s6_t))) < 0)
        goto error;
    array_dt = H5Tarray_create2(H5T_NATIVE_INT, 1, memb_size);
    if (H5Tinsert(s6_tid, "a", HOFFSET(s6_t, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s6_tid, "b", HOFFSET(s6_t, b), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s6_tid, "c", HOFFSET(s6_t, c), array_dt) < 0 ||
        H5Tinsert(s6_tid, "d", HOFFSET(s6_t, d), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s6_tid, "e", HOFFSET(s6_t, e), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s6_tid, "pre", HOFFSET(s6_t, pre), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s6_tid, "mid1", HOFFSET(s6_t, mid1), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s6_tid, "mid2", HOFFSET(s6_t, mid2), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s6_tid, "post", HOFFSET(s6_t, post), H5T_NATIVE_INT) < 0)
        goto error;
    H5Tclose(array_dt);

    /* Read the data */
    if (H5Dread(dataset, s6_tid, H5S_ALL, H5S_ALL, PRESERVE, s6) < 0) {
        goto error;
    }

    /* Check that the data was read properly */
    for (i = 0; i < NX * NY; i++) {
        if (s1[i].a != s6[i].a || s1[i].b != s6[i].b || s1[i].c[0] != s6[i].c[0] ||
            s1[i].c[1] != s6[i].c[1] || s1[i].c[2] != s6[i].c[2] || s1[i].c[3] != s6[i].c[3] ||
            s1[i].d != s6[i].d || s1[i].e != s6[i].e) {
            H5_FAILED();
            puts("    Incorrect values read from the file");
            goto error;
        }
    }

    /* Check that no previous values were clobbered */
    for (i = 0; i < NX * NY; i++) {
        if (s6[i].pre != 1000 + 4 * i || s6[i].mid1 != 1001 + 4 * i || s6[i].mid2 != 1002 + 4 * i ||
            s6[i].post != 1003 + 4 * i) {
            H5_FAILED();
            puts("    Memory values were clobbered");
            goto error;
        }
    }
    PASSED();

    /*
     *######################################################################
     * STEP 7: Update fields `b' and `d' on the file leaving the other
     *         fields unchanged.  This tests member alignment and background
     *           buffers.
     */
    TESTING("partially initialized superset write");

    /* Initialize `s4' with new values */
    for (i = 0; i < NX * NY; i++) {
        s4[i].b = 8 * i + 1;
        s4[i].d = 8 * i + 6;
    }

    /* Write the data to file */
    if (H5Dwrite(dataset, s4_tid, H5S_ALL, H5S_ALL, PRESERVE, s4) < 0) {
        goto error;
    }

    /* Read the data back */
    if (H5Dread(dataset, s1_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s1) < 0) {
        goto error;
    }

    /* Compare */
    for (i = 0; i < NX * NY; i++) {
        if (s1[i].a != 8 * i + 0 || s1[i].b != 8 * i + 1 || s1[i].c[0] != 8 * i + 2 ||
            s1[i].c[1] != 8 * i + 3 || s1[i].c[2] != 8 * i + 4 || s1[i].c[3] != 8 * i + 5 ||
            s1[i].d != 8 * i + 6 || s1[i].e != 8 * i + 7) {
            H5_FAILED();
            printf("    i==%u, row=%u, col=%u\n", i, i / NY, i % NY);
            printf("    got: {%7d,%7d,[%7d,%7d,%7d,%7d],%7d,%7d}\n", s1[i].a, s1[i].b, s1[i].c[0], s1[i].c[1],
                   s1[i].c[2], s1[i].c[3], s1[i].d, s1[i].e);
            printf("    ans: {%7d,%7d,[%7d,%7d,%7d,%7d],%7d,%7d}\n", 8 * i + 0, 8 * i + 1, 8 * i + 2,
                   8 * i + 3, 8 * i + 4, 8 * i + 5, 8 * i + 6, 8 * i + 7);
            goto error;
        }
    }
    PASSED();

    /*
     *######################################################################
     * STEP 8. Read the original dataset with an explicit data space.  Even
     * though these data spaces are equal it tests a different part of the
     * library.
     */
    TESTING("explicit data space");

    /* Create the data space */
    if ((s7_sid = H5Screate_simple(2, dim, NULL)) < 0)
        goto error;

    /* Read the dataset */
    if (H5Dread(dataset, s2_tid, s7_sid, H5S_ALL, H5P_DEFAULT, s2) < 0) {
        goto error;
    }

    /* Compare */
    for (i = 0; i < NX * NY; i++) {
        if (s2[i].a != s1[i].a || s2[i].b != s1[i].b || s2[i].c[0] != s1[i].c[0] ||
            s2[i].c[1] != s1[i].c[1] || s2[i].c[2] != s1[i].c[2] || s2[i].c[3] != s1[i].c[3] ||
            s2[i].d != s1[i].d || s2[i].e != s1[i].e) {
            H5_FAILED();
            puts("    Incorrect values read from file");
            goto error;
        }
    }
    PASSED();

    /*
     *######################################################################
     * STEP 9. Read a hyperslab of the file into a complete array in memory.
     * The hyperslab is the middle third of the array.
     */
    TESTING("hyperslab partial read to array");

    /* Create the file data space */
    if ((s8_f_sid = H5Dget_space(dataset)) < 0)
        goto error;
    f_offset[0] = NX / 3;
    f_offset[1] = NY / 3;
    h_size[0]   = 2 * NX / 3 - f_offset[0];
    h_size[1]   = 2 * NY / 3 - f_offset[1];
    if (H5Sselect_hyperslab(s8_f_sid, H5S_SELECT_SET, f_offset, NULL, h_size, NULL) < 0)
        goto error;

    /* Create memory data space */
    if ((s8_m_sid = H5Screate_simple(2, h_size, NULL)) < 0)
        goto error;

    /* Read the dataset */
    s8 = (s1_t *)calloc((size_t)(h_size[0] * h_size[1]), sizeof(s1_t));
    assert(s8);
    if (H5Dread(dataset, s1_tid, s8_m_sid, s8_f_sid, H5P_DEFAULT, s8) < 0) {
        goto error;
    }

    /* Compare */
    for (i = 0; i < h_size[0]; i++) {
        for (j = 0; j < h_size[1]; j++) {
            s1_t *ps1 = s1 + (f_offset[0] + i) * NY + f_offset[1] + j;
            s1_t *ps8 = s8 + i * h_size[1] + j;

            if (ps8->a != ps1->a || ps8->b != ps1->b || ps8->c[0] != ps1->c[0] || ps8->c[1] != ps1->c[1] ||
                ps8->c[2] != ps1->c[2] || ps8->c[3] != ps1->c[3] || ps8->d != ps1->d || ps8->e != ps1->e) {
                H5_FAILED();
                puts("    Incorrect values read from file");
                goto error;
            }
        }
    }

    free(s8);
    s8 = NULL;
    PASSED();

    /*
     *######################################################################
     * STEP 10.  Read a hyperslab of the file into a hyperslab of memory.  The
     * part of memory not read is already initialized and must not change.
     */
    TESTING("hyperslab partial read to another hyperslab");

    /* Initialize */
    for (i = 0; i < NX * NY; i++) {
        s2[i].a = s2[i].b = s2[i].d = s2[i].e = (unsigned)(-1);
        s2[i].c[0] = s2[i].c[1] = s2[i].c[2] = s2[i].c[3] = (unsigned)(-1);
    }

    /* Read the hyperslab */
    if (H5Dread(dataset, s2_tid, s8_f_sid, s8_f_sid, H5P_DEFAULT, s2) < 0) {
        goto error;
    }

    /* Compare */
    for (i = 0; i < NX; i++) {
        for (j = 0; j < NY; j++) {
            s1_t *ps1 = s1 + i * NY + j;
            s2_t *ps2 = s2 + i * NY + j;
            if (i >= f_offset[0] && i < f_offset[0] + h_size[0] && j >= f_offset[1] &&
                j < f_offset[1] + h_size[1]) {
                if (ps2->a != ps1->a || ps2->b != ps1->b || ps2->c[0] != ps1->c[0] ||
                    ps2->c[1] != ps1->c[1] || ps2->c[2] != ps1->c[2] || ps2->c[3] != ps1->c[3] ||
                    ps2->d != ps1->d || ps2->e != ps1->e) {
                    H5_FAILED();
                    puts("    Memory values clobbered");
                    goto error;
                }
            }
            else {
                if (ps2->a != (unsigned)(-1) || ps2->b != (unsigned)(-1) || ps2->c[0] != (unsigned)(-1) ||
                    ps2->c[1] != (unsigned)(-1) || ps2->c[2] != (unsigned)(-1) ||
                    ps2->c[3] != (unsigned)(-1) || ps2->d != (unsigned)(-1) || ps2->e != (unsigned)(-1)) {
                    H5_FAILED();
                    puts("    Incorrect values read from file");
                    goto error;
                }
            }
        }
    }
    PASSED();

    /*
     *######################################################################
     * STEP 11. Same as step 9 except the memory array contains some members
     * which are already initialized, like step 5.
     */
    TESTING("hyperslab to hyperslab part initialized read");

    /* Initialize */
    for (i = 0; i < NX * NY; i++) {
        s5[i].a = s5[i].b = s5[i].d = s5[i].e = (unsigned)(-1);
        s5[i].c[0] = s5[i].c[1] = s5[i].c[2] = s5[i].c[3] = (unsigned)(-1);
        s5[i].pre = s5[i].mid1 = s5[i].mid2 = s5[i].post = (unsigned)(-1);
    }

    /* Read the hyperslab */
    if (H5Dread(dataset, s5_tid, s8_f_sid, s8_f_sid, PRESERVE, s5) < 0) {
        goto error;
    }

    /* Compare */
    for (i = 0; i < NX; i++) {
        for (j = 0; j < NY; j++) {
            s1_t *ps1 = s1 + i * NY + j;
            s5_t *ps5 = s5 + i * NY + j;
            if (i >= f_offset[0] && i < f_offset[0] + h_size[0] && j >= f_offset[1] &&
                j < f_offset[1] + h_size[1]) {
                if (ps5->pre != (unsigned)(-1) || ps5->a != ps1->a || ps5->b != ps1->b ||
                    ps5->mid1 != (unsigned)(-1) || ps5->c[0] != ps1->c[0] || ps5->c[1] != ps1->c[1] ||
                    ps5->c[2] != ps1->c[2] || ps5->c[3] != ps1->c[3] || ps5->mid2 != (unsigned)(-1) ||
                    ps5->d != ps1->d || ps5->e != ps1->e || ps5->post != (unsigned)(-1)) {
                    H5_FAILED();
                    puts("    Memory values clobbered");
                    goto error;
                }
            }
            else {
                if (ps5->pre != (unsigned)(-1) || ps5->a != (unsigned)(-1) || ps5->b != (unsigned)(-1) ||
                    ps5->mid1 != (unsigned)(-1) || ps5->c[0] != (unsigned)(-1) ||
                    ps5->c[1] != (unsigned)(-1) || ps5->c[2] != (unsigned)(-1) ||
                    ps5->c[3] != (unsigned)(-1) || ps5->mid2 != (unsigned)(-1) || ps5->d != (unsigned)(-1) ||
                    ps5->e != (unsigned)(-1) || ps5->post != (unsigned)(-1)) {
                    H5_FAILED();
                    puts("    Incorrect values read from file");
                    goto error;
                }
            }
        }
    }
    PASSED();

    /*
     *######################################################################
     * Step 12: Write an array into the middle third of the dataset
     * initializing only members `b' and `d' to -1.
     */
    TESTING("hyperslab part initialized write");

    /* Create the memory array and initialize all fields to zero */
    f_offset[0] = NX / 3;
    f_offset[1] = NY / 3;
    h_size[0]   = 2 * NX / 3 - f_offset[0];
    h_size[1]   = 2 * NY / 3 - f_offset[1];
    s11         = (s4_t *)malloc((size_t)h_size[0] * (size_t)h_size[1] * sizeof(s4_t));
    assert(s11);

    /* Initialize */
    for (i = 0; i < h_size[0] * h_size[1]; i++) {
        s11[i].b = s11[i].d = (unsigned)(-1);
    }

    /* Write to disk */
    if (H5Dwrite(dataset, s4_tid, s8_m_sid, s8_f_sid, PRESERVE, s11) < 0) {
        goto error;
    }
    free(s11);
    s11 = NULL;

    /* Read the whole thing */
    if (H5Dread(dataset, s1_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s1) < 0) {
        goto error;
    }

    /* Compare */
    for (i = 0; i < NX; i++) {
        for (j = 0; j < NY; j++) {
            s1_t *ps1 = s1 + i * NY + j;

            if (ps1->a != 8 * (i * NY + j) + 0 || ps1->c[0] != 8 * (i * NY + j) + 2 ||
                ps1->c[1] != 8 * (i * NY + j) + 3 || ps1->c[2] != 8 * (i * NY + j) + 4 ||
                ps1->c[3] != 8 * (i * NY + j) + 5 || ps1->e != 8 * (i * NY + j) + 7) {
                H5_FAILED();
                puts("    Write clobbered values");
                goto error;
            }

            if (i >= f_offset[0] && i < f_offset[0] + h_size[0] && j >= f_offset[1] &&
                j < f_offset[1] + h_size[1]) {
                if (ps1->b != (unsigned)(-1) || ps1->d != (unsigned)(-1)) {
                    H5_FAILED();
                    puts("    Wrong values written or read");
                    goto error;
                }
            }
            else {
                if (ps1->b != 8 * (i * NY + j) + 1 || ps1->d != 8 * (i * NY + j) + 6) {
                    H5_FAILED();
                    puts("    Write clobbered values");
                    goto error;
                }
            }
        }
    }

    /*
     * Release resources.
     */
    H5Pclose(PRESERVE);
    H5Dclose(dataset);
    H5Fclose(file);

    /* Release buffers */
    free(s1);
    free(s2);
    free(s3);
    free(s4);
    free(s5);
    free(s6);

    PASSED();
    return 0;

error:
    puts("*** DATASET TESTS FAILED ***");

    /* Release resources */
    if (s1)
        free(s1);
    if (s2)
        free(s2);
    if (s3)
        free(s3);
    if (s4)
        free(s4);
    if (s5)
        free(s5);
    if (s6)
        free(s6);

    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    initialize_stype1
 *
 * Purpose:    Initialize data buffer.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */
static void
initialize_stype1(unsigned char *buf, size_t num)
{
    int     i, j;
    stype1 *s_ptr;

    for (i = 0; i < (int)num; i++) {
        s_ptr    = (stype1 *)((void *)buf) + i;
        s_ptr->a = i * 8 + 0;
        s_ptr->b = i * 8 + 1;
        for (j = 0; j < 8; j++)
            s_ptr->c[j] = i * 8 + j;
        s_ptr->d = i * 8 + 6;
        s_ptr->e = i * 8 + 7;

        s_ptr->f = (float)(i * 2 / 3);
        s_ptr->g = (float)(i * 2 / 3 + 1);
        for (j = 0; j < 16; j++)
            s_ptr->h[j] = (float)(i * j / 5 + j);
        s_ptr->i = (float)(i * 2 / 3 + 2);
        s_ptr->j = (float)(i * 2 / 3 + 3);

        s_ptr->k = i / 7 + 1;
        s_ptr->l = i / 7 + 2;
        s_ptr->m = i / 7 + 3;
        s_ptr->n = i / 7 + 4;
    }
}

/*-------------------------------------------------------------------------
 * Function:    initialize_stype2
 *
 * Purpose:    Initialize data buffer.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */
static void
initialize_stype2(unsigned char *buf, size_t num)
{
    size_t  i, j;
    stype2 *s_ptr;

    for (i = 0; i < num; i++) {
        s_ptr    = (stype2 *)((void *)buf) + i;
        s_ptr->a = (int)(i * 8 + 0);
        s_ptr->b = (int)(i * 8 + 1);
        for (j = 0; j < 8; j++)
            s_ptr->c[j] = (int)(i * 8 + j);
        s_ptr->d = (int)(i * 8 + 6);
        s_ptr->e = (int)(i * 8 + 7);

        s_ptr->f = (float)(i * 2 / 3);
        s_ptr->g = (float)(i * 2 / 3 + 1);
        for (j = 0; j < 16; j++)
            s_ptr->h[j] = (float)(i * j / 5 + j);
        s_ptr->i = (float)(i * 2 / 3 + 2);
        s_ptr->j = (float)(i * 2 / 3 + 3);

        s_ptr->k = (double)(i / 7 + 1);
        s_ptr->l = (double)(i / 7 + 2);
        s_ptr->m = (double)(i / 7 + 3);
        s_ptr->n = (double)(i / 7 + 4);

        s_ptr->o = (long)(i * 3 + 0);
        s_ptr->p = (long)(i * 3 + 1);
        s_ptr->q = (long)(i * 3 + 2);
    }
}

/*-------------------------------------------------------------------------
 * Function:    initialize_stype3
 *
 * Purpose:    Initialize data buffer.
 *
 * Return:    Success:
 *
 *-------------------------------------------------------------------------
 */
static void
initialize_stype3(unsigned char *buf, size_t num)
{
    int     i, j;
    stype3 *s_ptr;

    for (i = 0; i < (int)num; i++) {
        s_ptr    = (stype3 *)((void *)buf) + i;
        s_ptr->a = i * 8 + 0;
        s_ptr->b = i * 8 + 1;
        for (j = 0; j < 8; j++)
            s_ptr->c[j] = i * 8 + j;
        s_ptr->d = i * 8 + 6;
        s_ptr->e = i * 8 + 7;
    }
}

/*-------------------------------------------------------------------------
 * Function:    initialize_stype4
 *
 * Purpose:    Initialize data buffer.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */
static void
initialize_stype4(unsigned char *buf, size_t num)
{
    size_t  i, j;
    stype4 *s_ptr;

    for (i = 0; i < num; i++) {
        s_ptr    = (stype4 *)((void *)buf) + i;
        s_ptr->a = (int)(i * 8 + 0);
        s_ptr->b = (int)(i * 8 + 1);
        for (j = 0; j < 8; j++)
            s_ptr->c[j] = (int)(i * 8 + j);
        s_ptr->d = (int)(i * 8 + 6);
        s_ptr->e = (int)(i * 8 + 7);

        s_ptr->f = (float)(i * 2 / 3);
        s_ptr->g = (float)(i * 2 / 3 + 1);
        for (j = 0; j < 16; j++)
            s_ptr->h[j] = (float)(i * j / 5 + j);
        s_ptr->i = (float)(i * 2 / 3 + 2);
        s_ptr->j = (float)(i * 2 / 3 + 3);

        s_ptr->k = (double)(i / 7 + 1);
        s_ptr->l = (double)(i / 7 + 2);
        s_ptr->m = (double)(i / 7 + 3);
        s_ptr->n = (double)(i / 7 + 4);

        s_ptr->o = (long)(i * 3 + 0);
        s_ptr->p = (long)(i * 3 + 1);
        s_ptr->q = (long)(i * 3 + 2);

        s_ptr->r = (long long)(i * 5 + 1);
        s_ptr->s = (long long)(i * 5 + 2);
        s_ptr->t = (long long)(i * 5 + 3);
    }
}

/*-------------------------------------------------------------------------
 * Function:    create_stype1
 *
 * Purpose:    Create HDF5 compound datatype for stype1.
 *
 * Return:    Success:        datatype ID
 *
 *              Failure:        negative
 *
 *-------------------------------------------------------------------------
 */
static hid_t
create_stype1(void)
{
    hid_t         array_dt1, array_dt2, tid;
    const hsize_t eight = 8, sixteen = 16;

    /* Build hdf5 datatypes */
    if ((array_dt1 = H5Tarray_create2(H5T_NATIVE_INT, 1, &eight)) < 0)
        goto error;
    if ((array_dt2 = H5Tarray_create2(H5T_NATIVE_FLOAT, 1, &sixteen)) < 0)
        goto error;

    if ((tid = H5Tcreate(H5T_COMPOUND, sizeof(stype1))) < 0 ||
        H5Tinsert(tid, "a", HOFFSET(stype1, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(tid, "b", HOFFSET(stype1, b), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(tid, "c", HOFFSET(stype1, c), array_dt1) < 0 ||
        H5Tinsert(tid, "d", HOFFSET(stype1, d), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(tid, "e", HOFFSET(stype1, e), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(tid, "f", HOFFSET(stype1, f), H5T_NATIVE_FLOAT) < 0 ||
        H5Tinsert(tid, "g", HOFFSET(stype1, g), H5T_NATIVE_FLOAT) < 0 ||
        H5Tinsert(tid, "h", HOFFSET(stype1, h), array_dt2) < 0 ||
        H5Tinsert(tid, "i", HOFFSET(stype1, i), H5T_NATIVE_FLOAT) < 0 ||
        H5Tinsert(tid, "j", HOFFSET(stype1, j), H5T_NATIVE_FLOAT) < 0 ||
        H5Tinsert(tid, "k", HOFFSET(stype1, k), H5T_NATIVE_DOUBLE) < 0 ||
        H5Tinsert(tid, "l", HOFFSET(stype1, l), H5T_NATIVE_DOUBLE) < 0 ||
        H5Tinsert(tid, "m", HOFFSET(stype1, m), H5T_NATIVE_DOUBLE) < 0 ||
        H5Tinsert(tid, "n", HOFFSET(stype1, n), H5T_NATIVE_DOUBLE) < 0)
        goto error;

    if (H5Tclose(array_dt1) < 0)
        goto error;
    if (H5Tclose(array_dt2) < 0)
        goto error;

    return tid;

error:
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    create_stype2
 *
 * Purpose:    Create HDF5 compound datatype for stype2.
 *
 * Return:    Success:        datatype ID
 *
 *              Failure:        negative
 *
 *-------------------------------------------------------------------------
 */
static hid_t
create_stype2(void)
{
    hid_t         array_dt1, array_dt2, tid;
    const hsize_t eight = 8, sixteen = 16;

    /* Build hdf5 datatypes */
    if ((array_dt1 = H5Tarray_create2(H5T_NATIVE_INT, 1, &eight)) < 0)
        goto error;
    if ((array_dt2 = H5Tarray_create2(H5T_NATIVE_FLOAT, 1, &sixteen)) < 0)
        goto error;

    if ((tid = H5Tcreate(H5T_COMPOUND, sizeof(stype2))) < 0 ||
        H5Tinsert(tid, "a", HOFFSET(stype2, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(tid, "b", HOFFSET(stype2, b), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(tid, "c", HOFFSET(stype2, c), array_dt1) < 0 ||
        H5Tinsert(tid, "d", HOFFSET(stype2, d), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(tid, "e", HOFFSET(stype2, e), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(tid, "f", HOFFSET(stype2, f), H5T_NATIVE_FLOAT) < 0 ||
        H5Tinsert(tid, "g", HOFFSET(stype2, g), H5T_NATIVE_FLOAT) < 0 ||
        H5Tinsert(tid, "h", HOFFSET(stype2, h), array_dt2) < 0 ||
        H5Tinsert(tid, "i", HOFFSET(stype2, i), H5T_NATIVE_FLOAT) < 0 ||
        H5Tinsert(tid, "j", HOFFSET(stype2, j), H5T_NATIVE_FLOAT) < 0 ||
        H5Tinsert(tid, "k", HOFFSET(stype2, k), H5T_NATIVE_DOUBLE) < 0 ||
        H5Tinsert(tid, "l", HOFFSET(stype2, l), H5T_NATIVE_DOUBLE) < 0 ||
        H5Tinsert(tid, "m", HOFFSET(stype2, m), H5T_NATIVE_DOUBLE) < 0 ||
        H5Tinsert(tid, "n", HOFFSET(stype2, n), H5T_NATIVE_DOUBLE) < 0 ||
        H5Tinsert(tid, "o", HOFFSET(stype2, o), H5T_NATIVE_LONG) < 0 ||
        H5Tinsert(tid, "p", HOFFSET(stype2, p), H5T_NATIVE_LONG) < 0 ||
        H5Tinsert(tid, "q", HOFFSET(stype2, q), H5T_NATIVE_LONG) < 0)
        goto error;

    if (H5Tclose(array_dt1) < 0)
        goto error;
    if (H5Tclose(array_dt2) < 0)
        goto error;

    return tid;

error:
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    create_stype3
 *
 * Purpose:    Create HDF5 compound datatype for stype3.
 *
 * Return:    Success:        datatype ID
 *
 *              Failure:        negative
 *
 *-------------------------------------------------------------------------
 */
static hid_t
create_stype3(void)
{
    hid_t         array_dt1, tid;
    const hsize_t eight = 8;

    /* Build hdf5 datatypes */
    if ((array_dt1 = H5Tarray_create2(H5T_NATIVE_INT, 1, &eight)) < 0)
        goto error;

    if ((tid = H5Tcreate(H5T_COMPOUND, sizeof(stype3))) < 0 ||
        H5Tinsert(tid, "a", HOFFSET(stype3, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(tid, "b", HOFFSET(stype3, b), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(tid, "c", HOFFSET(stype3, c), array_dt1) < 0 ||
        H5Tinsert(tid, "d", HOFFSET(stype3, d), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(tid, "e", HOFFSET(stype3, e), H5T_NATIVE_INT) < 0)
        goto error;

    if (H5Tclose(array_dt1) < 0)
        goto error;

    return tid;

error:
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    create_stype4
 *
 * Purpose:    Create HDF5 compound datatype for stype4.
 *
 * Return:    Success:        datatype ID
 *
 *              Failure:        negative
 *
 *-------------------------------------------------------------------------
 */
static hid_t
create_stype4(void)
{
    hid_t         array_dt1, array_dt2, tid;
    const hsize_t eight = 8, sixteen = 16;

    /* Build hdf5 datatypes */
    if ((array_dt1 = H5Tarray_create2(H5T_NATIVE_INT, 1, &eight)) < 0)
        goto error;
    if ((array_dt2 = H5Tarray_create2(H5T_NATIVE_FLOAT, 1, &sixteen)) < 0)
        goto error;

    if ((tid = H5Tcreate(H5T_COMPOUND, sizeof(stype4))) < 0 ||
        H5Tinsert(tid, "a", HOFFSET(stype4, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(tid, "b", HOFFSET(stype4, b), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(tid, "c", HOFFSET(stype4, c), array_dt1) < 0 ||
        H5Tinsert(tid, "d", HOFFSET(stype4, d), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(tid, "e", HOFFSET(stype4, e), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(tid, "f", HOFFSET(stype4, f), H5T_NATIVE_FLOAT) < 0 ||
        H5Tinsert(tid, "g", HOFFSET(stype4, g), H5T_NATIVE_FLOAT) < 0 ||
        H5Tinsert(tid, "h", HOFFSET(stype4, h), array_dt2) < 0 ||
        H5Tinsert(tid, "i", HOFFSET(stype4, i), H5T_NATIVE_FLOAT) < 0 ||
        H5Tinsert(tid, "j", HOFFSET(stype4, j), H5T_NATIVE_FLOAT) < 0 ||
        H5Tinsert(tid, "k", HOFFSET(stype4, k), H5T_NATIVE_DOUBLE) < 0 ||
        H5Tinsert(tid, "l", HOFFSET(stype4, l), H5T_NATIVE_DOUBLE) < 0 ||
        H5Tinsert(tid, "m", HOFFSET(stype4, m), H5T_NATIVE_DOUBLE) < 0 ||
        H5Tinsert(tid, "n", HOFFSET(stype4, n), H5T_NATIVE_DOUBLE) < 0 ||
        H5Tinsert(tid, "o", HOFFSET(stype4, o), H5T_NATIVE_LONG) < 0 ||
        H5Tinsert(tid, "p", HOFFSET(stype4, p), H5T_NATIVE_LONG) < 0 ||
        H5Tinsert(tid, "q", HOFFSET(stype4, q), H5T_NATIVE_LONG) < 0 ||
        H5Tinsert(tid, "r", HOFFSET(stype4, r), H5T_NATIVE_LLONG) < 0 ||
        H5Tinsert(tid, "s", HOFFSET(stype4, s), H5T_NATIVE_LLONG) < 0 ||
        H5Tinsert(tid, "t", HOFFSET(stype4, t), H5T_NATIVE_LLONG) < 0)
        goto error;

    if (H5Tclose(array_dt1) < 0)
        goto error;
    if (H5Tclose(array_dt2) < 0)
        goto error;

    return tid;

error:
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    compare_data
 *
 * Purpose:    Compare data of stype1 and stype2.
 *
 * Return:    Success:        0
 *
 *              Failure:        negative
 *
 *-------------------------------------------------------------------------
 */
static int
compare_data(void *src_data, void *dst_data, bool src_subset)
{
    stype1 *s_ptr;
    stype2 *d_ptr;
    int     i;

    for (i = 0; i < (int)(NX * NY); i++) {
        if (src_subset) {
            s_ptr = ((stype1 *)src_data) + i;
            d_ptr = ((stype2 *)dst_data) + i;
        }
        else {
            s_ptr = (stype1 *)(((stype2 *)src_data) + i);
            d_ptr = (stype2 *)(((stype1 *)dst_data) + i);
        }

        if (s_ptr->a != d_ptr->a || s_ptr->b != d_ptr->b || s_ptr->c[0] != d_ptr->c[0] ||
            s_ptr->c[1] != d_ptr->c[1] || s_ptr->c[2] != d_ptr->c[2] || s_ptr->c[3] != d_ptr->c[3] ||
            s_ptr->d != d_ptr->d || s_ptr->e != d_ptr->e || !H5_FLT_ABS_EQUAL(s_ptr->f, d_ptr->f) ||
            !H5_FLT_ABS_EQUAL(s_ptr->g, d_ptr->g) || !H5_FLT_ABS_EQUAL(s_ptr->h[0], d_ptr->h[0]) ||
            !H5_FLT_ABS_EQUAL(s_ptr->h[1], d_ptr->h[1]) || !H5_FLT_ABS_EQUAL(s_ptr->i, d_ptr->i) ||
            !H5_FLT_ABS_EQUAL(s_ptr->j, d_ptr->j) || !H5_DBL_ABS_EQUAL(s_ptr->k, d_ptr->k) ||
            !H5_DBL_ABS_EQUAL(s_ptr->l, d_ptr->l) || !H5_DBL_ABS_EQUAL(s_ptr->m, d_ptr->m) ||
            !H5_DBL_ABS_EQUAL(s_ptr->n, d_ptr->n)) {

            H5_FAILED();
            printf("    i=%d\n", i);
            printf(
                "    src={a=%d, b=%d, c=[%d,%d,%d,%d,%d,%d,%d,%d], d=%d, e=%d, f=%f, g=%f, "
                "h=[%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f], i=%f, j=%f, k=%f, l=%f, m=%f, n=%f}\n",
                s_ptr->a, s_ptr->b, s_ptr->c[0], s_ptr->c[1], s_ptr->c[2], s_ptr->c[3], s_ptr->c[4],
                s_ptr->c[5], s_ptr->c[6], s_ptr->c[7], s_ptr->d, s_ptr->e, (double)s_ptr->f, (double)s_ptr->g,
                (double)s_ptr->h[0], (double)s_ptr->h[1], (double)s_ptr->h[2], (double)s_ptr->h[3],
                (double)s_ptr->h[4], (double)s_ptr->h[5], (double)s_ptr->h[6], (double)s_ptr->h[7],
                (double)s_ptr->h[8], (double)s_ptr->h[9], (double)s_ptr->h[10], (double)s_ptr->h[11],
                (double)s_ptr->h[12], (double)s_ptr->h[13], (double)s_ptr->h[14], (double)s_ptr->h[15],
                (double)s_ptr->i, (double)s_ptr->j, s_ptr->k, s_ptr->l, s_ptr->m, s_ptr->n);
            printf(
                "    dst={a=%d, b=%d, c=[%d,%d,%d,%d,%d,%d,%d,%d], d=%d, e=%d, f=%f, g=%f, "
                "h=[%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f], i=%f, j=%f, k=%f, l=%f, m=%f, n=%f}\n",
                d_ptr->a, d_ptr->b, d_ptr->c[0], d_ptr->c[1], d_ptr->c[2], d_ptr->c[3], d_ptr->c[4],
                d_ptr->c[5], d_ptr->c[6], d_ptr->c[7], d_ptr->d, d_ptr->e, (double)d_ptr->f, (double)d_ptr->g,
                (double)d_ptr->h[0], (double)d_ptr->h[1], (double)d_ptr->h[2], (double)d_ptr->h[3],
                (double)d_ptr->h[4], (double)d_ptr->h[5], (double)d_ptr->h[6], (double)d_ptr->h[7],
                (double)d_ptr->h[8], (double)d_ptr->h[9], (double)d_ptr->h[10], (double)d_ptr->h[11],
                (double)d_ptr->h[12], (double)d_ptr->h[13], (double)d_ptr->h[14], (double)d_ptr->h[15],
                (double)d_ptr->i, (double)d_ptr->j, d_ptr->k, d_ptr->l, d_ptr->m, d_ptr->n);
            goto error;
        }
    }

    return SUCCEED;

error:
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    test_hdf5_src_subset
 *
 * Purpose:    Test the optimization of compound data writing, rewriting,
 *              and reading when the source type is a subset of the destination
 *              type.  For example:
 *                  struct source {            struct destination {
 *                      TYPE1 A;      -->          TYPE1 A;
 *                      TYPE2 B;      -->          TYPE2 B;
 *                      TYPE3 C;      -->          TYPE3 C;
 *                  };                             TYPE4 D;
 *                                                 TYPE5 E;
 *                                             };
 *              This optimization is for the Chicago company.
 *
 * Return:    Success:    0
 *
 *        Failure:    1
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_hdf5_src_subset(char *filename, hid_t fapl)
{
    hid_t          file;
    hid_t          rew_tid, src_tid, dst_tid;
    hid_t          dataset;
    hid_t          space;
    hid_t          dcpl, dxpl;
    hsize_t        dims[2]       = {NX, NY};
    hsize_t        chunk_dims[2] = {NX / 10, NY / 10};
    unsigned char *orig = NULL, *rew_buf = NULL, *rbuf = NULL;

    /* Create the file for this test */
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        goto error;

    /* Build hdf5 datatypes */
    if ((src_tid = create_stype1()) < 0)
        goto error;

    if ((dst_tid = create_stype2()) < 0)
        goto error;

    if ((rew_tid = create_stype3()) < 0)
        goto error;

    /* Create the data space */
    if ((space = H5Screate_simple(2, dims, NULL)) < 0)
        goto error;

    /* Allocate space and initialize data */
    orig = (unsigned char *)malloc(NX * NY * sizeof(stype1));
    initialize_stype1(orig, (size_t)NX * NY);

    rbuf = (unsigned char *)malloc(NX * NY * sizeof(stype2));

    rew_buf = (unsigned char *)malloc(NX * NY * sizeof(stype3));
    initialize_stype3(rew_buf, (size_t)NX * NY);

    /* Create dataset creation property list */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;

    /*
     *######################################################################
     * STEP 1. Write data to contiguous and chunked datasets.
     */
    TESTING("writing data to contiguous and chunked datasets");

    /* Create contiguous data set */
    if ((dataset = H5Dcreate2(file, DSET_NAME[0], src_tid, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    /* Write the data to the dataset */
    if (H5Dwrite(dataset, src_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, orig) < 0)
        goto error;

    if (H5Dclose(dataset) < 0)
        goto error;

    /* Set chunking */
    if (H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        goto error;

    /* Create chunked data set */
    if ((dataset = H5Dcreate2(file, DSET_NAME[1], src_tid, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    /* Write the data to the dataset */
    if (H5Dwrite(dataset, src_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, orig) < 0)
        goto error;

    if (H5Dclose(dataset) < 0)
        goto error;

    PASSED();

    /*
     *######################################################################
     * STEP 2. Rewrite the data with a subset of original data type.
     */
    TESTING("rewriting data with a subset of original data type");

    /* Create xfer properties to preserve initialized data */
    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        FAIL_STACK_ERROR;

    if (H5Pset_preserve(dxpl, true) < 0)
        FAIL_STACK_ERROR;

    /* Rewrite contiguous data set */
    if ((dataset = H5Dopen2(file, DSET_NAME[0], H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Write the data to the dataset */
    if (H5Dwrite(dataset, rew_tid, H5S_ALL, H5S_ALL, dxpl, rew_buf) < 0)
        FAIL_STACK_ERROR;

    if (H5Dclose(dataset) < 0)
        FAIL_STACK_ERROR;

    /* Rewrite chunked data set */
    if ((dataset = H5Dopen2(file, DSET_NAME[1], H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Write the data to the dataset */
    if (H5Dwrite(dataset, rew_tid, H5S_ALL, H5S_ALL, dxpl, rew_buf) < 0)
        FAIL_STACK_ERROR;

    if (H5Dclose(dataset) < 0)
        FAIL_STACK_ERROR;

    PASSED();

    /*
     *######################################################################
     * STEP 3. Read the data into a subset of the original compound type.
     */
    TESTING("reading data with a subset of original data type");

    /* Check contiguous data set */
    if ((dataset = H5Dopen2(file, DSET_NAME[0], H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    if (H5Dread(dataset, dst_tid, H5S_ALL, H5S_ALL, dxpl, rbuf) < 0)
        FAIL_STACK_ERROR;

    if (compare_data(orig, rbuf, true) < 0)
        TEST_ERROR;

    if (H5Dclose(dataset) < 0)
        FAIL_STACK_ERROR;

    /* Check chunked data set */
    if ((dataset = H5Dopen2(file, DSET_NAME[1], H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    if (H5Dread(dataset, dst_tid, H5S_ALL, H5S_ALL, dxpl, rbuf) < 0)
        FAIL_STACK_ERROR;

    if (compare_data(orig, rbuf, true) < 0)
        TEST_ERROR;

    if (H5Dclose(dataset) < 0)
        FAIL_STACK_ERROR;

    /* Finishing test and release resources */
    if (H5Sclose(space) < 0)
        FAIL_STACK_ERROR;

    if (H5Pclose(dcpl) < 0)
        FAIL_STACK_ERROR;

    if (H5Pclose(dxpl) < 0)
        FAIL_STACK_ERROR;

    if (H5Tclose(src_tid) < 0)
        FAIL_STACK_ERROR;
    if (H5Tclose(dst_tid) < 0)
        FAIL_STACK_ERROR;
    if (H5Tclose(rew_tid) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(file) < 0)
        FAIL_STACK_ERROR;

    free(orig);
    free(rbuf);
    free(rew_buf);

    PASSED();
    return 0;

error:
    free(orig);
    free(rbuf);
    free(rew_buf);

    puts("*** DATASET TESTS FAILED ***");
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test_hdf5_dst_subset
 *
 * Purpose:    Test the optimization of compound data writing, rewriting,
 *              and reading when the destination type is a subset of the
 *              source type.  For example:
 *                  struct source {            struct destination {
 *                      TYPE1 A;      -->          TYPE1 A;
 *                      TYPE2 B;      -->          TYPE2 B;
 *                      TYPE3 C;      -->          TYPE3 C;
 *                      TYPE4 D;               }
 *                      TYPE5 E;
 *                  };
 *              This optimization is for the Chicago company.  This test
 *              is the opposite of test_hdf5_src_subset.
 *
 * Return:    Success:    0
 *
 *        Failure:    1
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_hdf5_dst_subset(char *filename, hid_t fapl)
{
    hid_t          file;
    hid_t          rew_tid, src_tid, dst_tid;
    hid_t          dataset;
    hid_t          space;
    hid_t          dcpl, dxpl;
    hsize_t        dims[2]       = {NX, NY};
    hsize_t        chunk_dims[2] = {NX / 10, NY / 10};
    unsigned char *orig = NULL, *rew_buf = NULL, *rbuf = NULL;

    /* Create the file for this test */
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        goto error;

    /* Build hdf5 datatypes */
    if ((src_tid = create_stype2()) < 0)
        goto error;

    if ((dst_tid = create_stype1()) < 0)
        goto error;

    if ((rew_tid = create_stype4()) < 0)
        goto error;

    /* Create the data space */
    if ((space = H5Screate_simple(2, dims, NULL)) < 0)
        goto error;

    /* Allocate space and initialize data */
    orig = (unsigned char *)malloc(NX * NY * sizeof(stype2));
    initialize_stype2(orig, (size_t)NX * NY);

    rbuf = (unsigned char *)malloc(NX * NY * sizeof(stype1));

    rew_buf = (unsigned char *)malloc(NX * NY * sizeof(stype4));
    initialize_stype4(rew_buf, (size_t)NX * NY);

    /* Create dataset creation property list */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;

    /*
     *######################################################################
     * STEP 1. Write data to contiguous and chunked datasets.
     */
    TESTING("writing data to contiguous and chunked datasets");

    /* Create contiguous data set */
    if ((dataset = H5Dcreate2(file, DSET_NAME[2], src_tid, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    /* Write the data to the dataset */
    if (H5Dwrite(dataset, src_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, orig) < 0)
        goto error;

    if (H5Dclose(dataset) < 0)
        goto error;

    /* Set chunking */
    if (H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        goto error;

    /* Create chunked data set */
    if ((dataset = H5Dcreate2(file, DSET_NAME[3], src_tid, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    /* Write the data to the dataset */
    if (H5Dwrite(dataset, src_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, orig) < 0)
        goto error;

    if (H5Dclose(dataset) < 0)
        goto error;

    PASSED();

    /*
     *######################################################################
     * STEP 2. Rewrite the data with a subset of original data type.
     */
    TESTING("rewriting data with a subset of original data type");

    /* Create xfer properties to preserve initialized data */
    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        goto error;

    if (H5Pset_preserve(dxpl, true) < 0)
        goto error;

    /* Rewrite contiguous data set */
    if ((dataset = H5Dopen2(file, DSET_NAME[2], H5P_DEFAULT)) < 0)
        goto error;

    /* Write the data to the dataset */
    if (H5Dwrite(dataset, rew_tid, H5S_ALL, H5S_ALL, dxpl, rew_buf) < 0)
        goto error;

    if (H5Dclose(dataset) < 0)
        goto error;

    /* Rewrite chunked data set */
    if ((dataset = H5Dopen2(file, DSET_NAME[3], H5P_DEFAULT)) < 0)
        goto error;

    /* Write the data to the dataset */
    if (H5Dwrite(dataset, rew_tid, H5S_ALL, H5S_ALL, dxpl, rew_buf) < 0)
        goto error;

    if (H5Dclose(dataset) < 0)
        goto error;

    PASSED();

    /*
     *######################################################################
     * STEP 3. Read the data into a subset of the original compound type.
     */
    TESTING("reading data with a subset of original data type");

    /* Check contiguous data set */
    if ((dataset = H5Dopen2(file, DSET_NAME[2], H5P_DEFAULT)) < 0)
        goto error;

    if (H5Dread(dataset, dst_tid, H5S_ALL, H5S_ALL, dxpl, rbuf) < 0)
        goto error;

    if (compare_data(orig, rbuf, false) < 0)
        goto error;

    if (H5Dclose(dataset) < 0)
        goto error;

    /* Check chunked data set */
    if ((dataset = H5Dopen2(file, DSET_NAME[3], H5P_DEFAULT)) < 0)
        goto error;

    if (H5Dread(dataset, dst_tid, H5S_ALL, H5S_ALL, dxpl, rbuf) < 0)
        goto error;

    if (compare_data(orig, rbuf, false) < 0)
        goto error;

    if (H5Dclose(dataset) < 0)
        goto error;

    /* Finishing test and release resources */
    if (H5Sclose(space) < 0)
        goto error;

    if (H5Pclose(dcpl) < 0)
        goto error;

    if (H5Pclose(dxpl) < 0)
        goto error;

    if (H5Tclose(src_tid) < 0)
        goto error;
    if (H5Tclose(dst_tid) < 0)
        goto error;
    if (H5Tclose(rew_tid) < 0)
        goto error;
    if (H5Fclose(file) < 0)
        goto error;

    free(orig);
    free(rbuf);
    free(rew_buf);

    PASSED();
    return 0;

error:
    puts("*** DATASET TESTS FAILED ***");
    return 1;
}

/* Error macro that outputs the state of the randomly generated variables so the
 * failure can be reproduced */
#define PACK_OOO_ERROR                                                                                       \
    {                                                                                                        \
        int _i;                                                                                              \
        H5_FAILED();                                                                                         \
        AT();                                                                                                \
        printf("    Insertion order =");                                                                     \
        for (_i = 0; _i < PACK_NMEMBS; _i++)                                                                 \
            printf(" %d", order[_i]);                                                                        \
        printf("\n    Inner compound order = %d, location = %d\n", sub_cmpd_order, order[sub_cmpd_order]);   \
        fflush(stdout);                                                                                      \
        goto error;                                                                                          \
    }

/*-------------------------------------------------------------------------
 * Function:    test_pack_ooo
 *
 * Purpose:    Test inserting fields into a compound out of offset order.
 *              Verifies that the compound is correctly marked as packed
 *              or non-packed.
 *
 * Return:    Success:    0
 *
 *        Failure:    1
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_pack_ooo(void)
{
    hid_t    cmpd, sub_cmpd;          /* Datatype IDs */
    H5T_t   *dt;                      /* Datatype pointer */
    unsigned order[PACK_NMEMBS];      /* Order of insertion */
    unsigned free_order[PACK_NMEMBS]; /* Index of remaining free slots in order */
    unsigned num_free;                /* Number of free slots in order */
    unsigned sub_cmpd_order;          /* Order to insert the inner compound */
    char     name[16];                /* Member name */
    unsigned extra_space;             /* Whether to add extra space to the end of
                                       * the compound */
    unsigned i, j;                    /* Indices */

    HDsrand((unsigned)HDtime(NULL));

    /* Initialize "free_order" array to indicate that all slots in order are
     * free */
    for (i = 0; i < PACK_NMEMBS; i++)
        free_order[i] = i;

    /* Create "order" array */
    for (i = 0; i < PACK_NMEMBS; i++) {
        /* Generate index into free_order array */
        num_free = PACK_NMEMBS - i;
        j        = (unsigned)HDrandom() % num_free;

        /* Update order array at the randomly generated (but guaranteed to be
         * free) location */
        order[free_order[j]] = i;

        /* Reshape free_order to remove j (which is no longer free) */
        if (j < (num_free - 1))
            memmove(&free_order[j], &free_order[j + 1], (num_free - j - 1) * sizeof(free_order[0]));
    } /* end for */

    /* Generate order to insert inner compound type */
    sub_cmpd_order = (unsigned)HDrandom() % PACK_NMEMBS;

    for (extra_space = 0; extra_space < 2; extra_space++) {
        if (extra_space)
            puts("With extra space at the end of compound...");
        else
            puts("Without extra space at the end of compound...");

        TESTING("random member insertion with empty compound subtype");

        /* Create inner compound type.  It will be empty for the first run */
        if ((sub_cmpd = H5Tcreate(H5T_COMPOUND, (size_t)4)) < 0)
            PACK_OOO_ERROR

        /* Create main compound type, with extra space at the end */
        if ((cmpd = H5Tcreate(H5T_COMPOUND, (size_t)((4 * PACK_NMEMBS) + extra_space))) < 0)
            PACK_OOO_ERROR

        /* Insert the compound members in the random order previously generated */
        for (i = 0; i < PACK_NMEMBS; i++) {
            snprintf(name, sizeof(name), "%05d", i);
            if (i == sub_cmpd_order) {
                if (H5Tinsert(cmpd, name, (size_t)(4 * order[i]), sub_cmpd) < 0)
                    PACK_OOO_ERROR
            }
            else if (H5Tinsert(cmpd, name, (size_t)(4 * order[i]), H5T_STD_I32BE) < 0)
                PACK_OOO_ERROR
        } /* end for */

        /* Verify that the compound is not packed */
        if (NULL == (dt = (H5T_t *)H5I_object_verify(cmpd, H5I_DATATYPE)))
            PACK_OOO_ERROR
        if (dt->shared->u.compnd.packed)
            PACK_OOO_ERROR

        /* Close the main compound */
        if (H5Tclose(cmpd) < 0)
            PACK_OOO_ERROR

        PASSED();

        TESTING("random member insertion with full compound subtype");

        /* Complete the inner compound type */
        if (H5Tinsert(sub_cmpd, "int", (size_t)0, H5T_STD_I32LE) < 0)
            PACK_OOO_ERROR

        /* Recreate main compound type */
        if ((cmpd = H5Tcreate(H5T_COMPOUND, (size_t)((4 * PACK_NMEMBS) + extra_space))) < 0)
            PACK_OOO_ERROR

        /* Insert the compound members in the random order previously generated */
        for (i = 0; i < PACK_NMEMBS; i++) {
            snprintf(name, sizeof(name), "%05d", i);
            if (i == sub_cmpd_order) {
                if (H5Tinsert(cmpd, name, (size_t)(4 * order[i]), sub_cmpd) < 0)
                    PACK_OOO_ERROR
            }
            else if (H5Tinsert(cmpd, name, (size_t)(4 * order[i]), H5T_STD_I32BE) < 0)
                PACK_OOO_ERROR
        } /* end for */

        /* Verify that the compound is not packed */
        if (NULL == (dt = (H5T_t *)H5I_object_verify(cmpd, H5I_DATATYPE)))
            PACK_OOO_ERROR
        if (dt->shared->u.compnd.packed != !extra_space)
            PACK_OOO_ERROR

        /* Close */
        if (H5Tclose(cmpd) < 0)
            PACK_OOO_ERROR
        if (H5Tclose(sub_cmpd) < 0)
            PACK_OOO_ERROR

        PASSED();

        TESTING("reverse member insertion with empty compound subtype");

        /* Create inner compound type.  It will be empty for the first run */
        if ((sub_cmpd = H5Tcreate(H5T_COMPOUND, (size_t)4)) < 0)
            PACK_OOO_ERROR

        /* Create main compound type, with extra space at the end */
        if ((cmpd = H5Tcreate(H5T_COMPOUND, (size_t)((4 * PACK_NMEMBS) + extra_space))) < 0)
            PACK_OOO_ERROR

        /* Insert the compound members in reverse order, with compound last */
        for (i = 0; i < PACK_NMEMBS; i++) {
            snprintf(name, sizeof(name), "%05d", i);
            if (i == PACK_NMEMBS - 1) {
                if (H5Tinsert(cmpd, name, (size_t)(4 * (PACK_NMEMBS - i - 1)), sub_cmpd) < 0)
                    PACK_OOO_ERROR
            }
            else if (H5Tinsert(cmpd, name, (size_t)(4 * (PACK_NMEMBS - i - 1)), H5T_STD_I32BE) < 0)
                PACK_OOO_ERROR
        } /* end for */

        /* Verify that the compound is not packed */
        if (NULL == (dt = (H5T_t *)H5I_object_verify(cmpd, H5I_DATATYPE)))
            PACK_OOO_ERROR
        if (dt->shared->u.compnd.packed)
            PACK_OOO_ERROR

        /* Close the main compound */
        if (H5Tclose(cmpd) < 0)
            PACK_OOO_ERROR

        PASSED();

        TESTING("reverse member insertion with full compound subtype");

        /* Complete the inner compound type */
        if (H5Tinsert(sub_cmpd, "int", (size_t)0, H5T_STD_I32LE) < 0)
            PACK_OOO_ERROR

        /* Recreate main compound type */
        if ((cmpd = H5Tcreate(H5T_COMPOUND, (size_t)((4 * PACK_NMEMBS) + extra_space))) < 0)
            PACK_OOO_ERROR

        /* Insert the compound members in reverse order, with compound last */
        for (i = 0; i < PACK_NMEMBS; i++) {
            snprintf(name, sizeof(name), "%05d", i);
            if (i == PACK_NMEMBS - 1) {
                if (H5Tinsert(cmpd, name, (size_t)(4 * (PACK_NMEMBS - i - 1)), sub_cmpd) < 0)
                    PACK_OOO_ERROR
            }
            else if (H5Tinsert(cmpd, name, (size_t)(4 * (PACK_NMEMBS - i - 1)), H5T_STD_I32BE) < 0)
                PACK_OOO_ERROR
        } /* end for */

        /* Verify that the compound is packed */
        if (NULL == (dt = (H5T_t *)H5I_object_verify(cmpd, H5I_DATATYPE)))
            PACK_OOO_ERROR
        if (dt->shared->u.compnd.packed != !extra_space)
            PACK_OOO_ERROR

        /* Close */
        if (H5Tclose(cmpd) < 0)
            PACK_OOO_ERROR
        if (H5Tclose(sub_cmpd) < 0)
            PACK_OOO_ERROR

        PASSED();

        TESTING("forward member insertion with empty compound subtype");

        /* Create inner compound type.  It will be empty for the first run */
        if ((sub_cmpd = H5Tcreate(H5T_COMPOUND, (size_t)4)) < 0)
            PACK_OOO_ERROR

        /* Create main compound type, with extra space at the end */
        if ((cmpd = H5Tcreate(H5T_COMPOUND, (size_t)((4 * PACK_NMEMBS) + extra_space))) < 0)
            PACK_OOO_ERROR

        /* Insert the compound members in forward order, with compound first */
        for (i = 0; i < PACK_NMEMBS; i++) {
            snprintf(name, sizeof(name), "%05d", i);
            if (i == 0) {
                if (H5Tinsert(cmpd, name, (size_t)(4 * i), sub_cmpd) < 0)
                    PACK_OOO_ERROR
            }
            else if (H5Tinsert(cmpd, name, (size_t)(4 * i), H5T_STD_I32BE) < 0)
                PACK_OOO_ERROR
        } /* end for */

        /* Verify that the compound is not packed */
        if (NULL == (dt = (H5T_t *)H5I_object_verify(cmpd, H5I_DATATYPE)))
            PACK_OOO_ERROR
        if (dt->shared->u.compnd.packed)
            PACK_OOO_ERROR

        /* Close the main compound */
        if (H5Tclose(cmpd) < 0)
            PACK_OOO_ERROR

        PASSED();

        TESTING("forward member insertion with full compound subtype");

        /* Complete the inner compound type */
        if (H5Tinsert(sub_cmpd, "int", (size_t)0, H5T_STD_I32LE) < 0)
            PACK_OOO_ERROR

        /* Recreate main compound type */
        if ((cmpd = H5Tcreate(H5T_COMPOUND, (size_t)((4 * PACK_NMEMBS) + extra_space))) < 0)
            PACK_OOO_ERROR

        /* Insert the compound members in forward order */
        for (i = 0; i < PACK_NMEMBS; i++) {
            snprintf(name, sizeof(name), "%05d", i);
            if (i == 0) {
                if (H5Tinsert(cmpd, name, (size_t)(4 * i), sub_cmpd) < 0)
                    PACK_OOO_ERROR
            }
            else if (H5Tinsert(cmpd, name, (size_t)(4 * i), H5T_STD_I32BE) < 0)
                PACK_OOO_ERROR
        } /* end for */

        /* Verify that the compound is packed */
        if (NULL == (dt = (H5T_t *)H5I_object_verify(cmpd, H5I_DATATYPE)))
            PACK_OOO_ERROR
        if (dt->shared->u.compnd.packed != !extra_space)
            PACK_OOO_ERROR

        /* Close */
        if (H5Tclose(cmpd) < 0)
            PACK_OOO_ERROR
        if (H5Tclose(sub_cmpd) < 0)
            PACK_OOO_ERROR

        PASSED();
    } /* end for */

    return 0;

error:
    puts("*** DATASET TESTS FAILED ***");
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test_ooo_order
 *
 * Purpose:    Test inserting fields into a compound out of offset order.
 *              Verifies that the order of compound members is the same as
 *              the order in which they were inserted.  While this is
 *              explicitly not guaranteed by the documentation, the H5TB
 *              API currently makes this assumption.
 *
 * Return:    Success:    0
 *
 *        Failure:    1
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_ooo_order(char *filename, hid_t fapl_id)
{
    hid_t  file      = H5I_INVALID_HID; /* File ID */
    hid_t  dtype     = H5I_INVALID_HID; /* Datatype IDs */
    hid_t  dtype_tmp = H5I_INVALID_HID; /* Temp Datatype ID */
    H5T_t *dt        = NULL;            /* Datatype pointer */

    TESTING("that compound member insertion order is preserved");

    /* Create the file */
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
        TEST_ERROR;

    /* Create the compound */
    if ((dtype = H5Tcreate(H5T_COMPOUND, (size_t)20)) < 0)
        TEST_ERROR;
    if (H5Tinsert(dtype, "A", (size_t)8, H5T_STD_I32LE) < 0)
        TEST_ERROR;
    if (H5Tinsert(dtype, "B", (size_t)12, H5T_STD_I32LE) < 0)
        TEST_ERROR;
    if (H5Tinsert(dtype, "C", (size_t)0, H5T_STD_I32LE) < 0)
        TEST_ERROR;
    if (H5Tinsert(dtype, "D", (size_t)16, H5T_STD_I32LE) < 0)
        TEST_ERROR;

    /* Verify that the compound is not packed */
    if (NULL == (dt = (H5T_t *)H5I_object_verify(dtype, H5I_DATATYPE)))
        TEST_ERROR;
    if (dt->shared->u.compnd.packed)
        TEST_ERROR;

    /* Verify that the order is the same as the insertion order */
    if (H5Tget_member_offset(dtype, 0) != 8)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype, 1) != 12)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype, 2) != 0)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype, 3) != 16)
        TEST_ERROR;

    /* Commit the datatype */
    if (H5Tcommit2(file, "dtype", dtype, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR;

    /* Close and reopen the file */
    if (H5Tclose(dtype))
        TEST_ERROR;
    if (H5Fclose(file))
        TEST_ERROR;
    if ((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    /* Open the type */
    if ((dtype_tmp = H5Topen2(file, "dtype", H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Verify that the compound is not packed */
    if (NULL == (dt = (H5T_t *)H5I_object_verify(dtype_tmp, H5I_DATATYPE)))
        TEST_ERROR;
    if (dt->shared->u.compnd.packed)
        TEST_ERROR;

    /* Verify that the order is the same as the insertion order */
    if (H5Tget_member_offset(dtype_tmp, 0) != 8)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype_tmp, 1) != 12)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype_tmp, 2) != 0)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype_tmp, 3) != 16)
        TEST_ERROR;

    /* Copy the datatype */
    if ((dtype = H5Tcopy(dtype_tmp)) < 0)
        TEST_ERROR;

    /* Verify that the compound is not packed */
    if (NULL == (dt = (H5T_t *)H5I_object_verify(dtype, H5I_DATATYPE)))
        TEST_ERROR;
    if (dt->shared->u.compnd.packed)
        TEST_ERROR;

    /* Verify that the order is the same as the insertion order */
    if (H5Tget_member_offset(dtype, 0) != 8)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype, 1) != 12)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype, 2) != 0)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype, 3) != 16)
        TEST_ERROR;

    /* Insert the last member */
    if (H5Tinsert(dtype, "E", (size_t)4, H5T_STD_I32LE) < 0)
        TEST_ERROR;

    /* Verify that the compound is packed */
    if (NULL == (dt = (H5T_t *)H5I_object_verify(dtype, H5I_DATATYPE)))
        TEST_ERROR;
    if (!dt->shared->u.compnd.packed)
        TEST_ERROR;

    /* Verify that the order is the same as the insertion order */
    if (H5Tget_member_offset(dtype, 0) != 8)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype, 1) != 12)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype, 2) != 0)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype, 3) != 16)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype, 4) != 4)
        TEST_ERROR;

    /* Commit the modified datatype */
    if (H5Tcommit2(file, "dtype2", dtype, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR;

    /* Close and reopen the file */
    if (H5Tclose(dtype_tmp))
        TEST_ERROR;
    if (H5Tclose(dtype))
        TEST_ERROR;
    if (H5Fclose(file))
        TEST_ERROR;
    if ((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl_id)) < 0)
        TEST_ERROR;

    /* Open the type, and verify status */
    if ((dtype_tmp = H5Topen2(file, "dtype2", H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (NULL == (dt = (H5T_t *)H5I_object_verify(dtype_tmp, H5I_DATATYPE)))
        TEST_ERROR;
    if (!dt->shared->u.compnd.packed)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype_tmp, 0) != 8)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype_tmp, 1) != 12)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype_tmp, 2) != 0)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype_tmp, 3) != 16)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype_tmp, 4) != 4)
        TEST_ERROR;

    /* Copy the datatype, and verify status */
    if ((dtype = H5Tcopy(dtype_tmp)) < 0)
        TEST_ERROR;
    if (NULL == (dt = (H5T_t *)H5I_object_verify(dtype, H5I_DATATYPE)))
        TEST_ERROR;
    if (!dt->shared->u.compnd.packed)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype, 0) != 8)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype, 1) != 12)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype, 2) != 0)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype, 3) != 16)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype, 4) != 4)
        TEST_ERROR;

    /* Expand the type, and verify that it became unpacked */
    if (H5Tset_size(dtype, (size_t)21) < 0)
        TEST_ERROR;
    if (NULL == (dt = (H5T_t *)H5I_object_verify(dtype, H5I_DATATYPE)))
        TEST_ERROR;
    if (dt->shared->u.compnd.packed)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype, 0) != 8)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype, 1) != 12)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype, 2) != 0)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype, 3) != 16)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype, 4) != 4)
        TEST_ERROR;

    /* Shrink the type, and verify that it became packed */
    if (H5Tset_size(dtype, (size_t)20) < 0)
        TEST_ERROR;
    if (NULL == (dt = (H5T_t *)H5I_object_verify(dtype, H5I_DATATYPE)))
        TEST_ERROR;
    if (!dt->shared->u.compnd.packed)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype, 0) != 8)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype, 1) != 12)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype, 2) != 0)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype, 3) != 16)
        TEST_ERROR;
    if (H5Tget_member_offset(dtype, 4) != 4)
        TEST_ERROR;

    /* Close */
    if (H5Tclose(dtype_tmp))
        TEST_ERROR;
    if (H5Tclose(dtype))
        TEST_ERROR;
    if (H5Fclose(file))
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(dtype_tmp);
        H5Tclose(dtype);
        H5Fclose(file);
    }
    H5E_END_TRY
    puts("*** DATASET TESTS FAILED ***");
    return 1;
} /* test_ooo_order */

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:    Test different cases of I/O for compound data and the
 *              compound optimization for the Chicago company.
 *
 * Return:    Success:         0
 *
 *              Failure:         1
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, char *argv[])
{
    hid_t    fapl_id;
    char     fname[256];
    unsigned nerrors = 0;

    h5_reset();

    /* Turn off optimized compound converter? */
    if (argc > 1) {
        if (argc > 2 || strcmp("--noopt", argv[1]) != 0) {
            fprintf(stderr, "usage: %s [--noopt]\n", argv[0]);
            exit(EXIT_FAILURE);
        }
        H5Tunregister(H5T_PERS_DONTCARE, NULL, (hid_t)-1, (hid_t)-1,
                      (H5T_conv_t)((void (*)(void))H5T__conv_struct_opt));
    }

    printf("Testing compound dataset for selection I/O cases----\n");
    nerrors += test_compounds_selection_io();

    /* Create the file */
    fapl_id = h5_fileaccess();

    h5_fixname(FILENAME[0], fapl_id, fname, sizeof(fname));

    puts("Testing compound dataset:");
    nerrors += test_compound(fname, fapl_id);

    puts("Testing the optimization of when the source type is a subset of the dest:");
    h5_fixname(FILENAME[1], fapl_id, fname, sizeof(fname));
    nerrors += test_hdf5_src_subset(fname, fapl_id);

    puts("Testing the optimization of when the dest type is a subset of the source:");
    h5_fixname(FILENAME[2], fapl_id, fname, sizeof(fname));
    nerrors += test_hdf5_dst_subset(fname, fapl_id);

    puts("Testing that compound types can be packed out of order:");
    nerrors += test_pack_ooo();

    puts("Testing compound member ordering:");
    nerrors += test_ooo_order(fname, fapl_id);

    /* Verify symbol table messages are cached */
    nerrors += (h5_verify_cached_stabs(FILENAME, fapl_id) < 0 ? 1 : 0);

    if (nerrors) {
        printf("***** %u FAILURE%s! *****\n", nerrors, 1 == nerrors ? "" : "S");
        exit(EXIT_FAILURE);
    }

    h5_cleanup(FILENAME, fapl_id);
    puts("All compound dataset tests passed.");
    return 0;
}
