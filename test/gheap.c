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

/*
 * Purpose:     Tests the global heap.  The global heap is the set of all
 *              collections but the collections are not related to one
 *              another by anything that appears in the file format.
 */
#include "h5test.h"
#include "H5ACprivate.h"
#include "H5CXprivate.h" /* API Contexts                         */
#include "H5Eprivate.h"
#include "H5Fprivate.h"
#include "H5Gprivate.h"
#include "H5HGprivate.h"
#include "H5Iprivate.h"
#include "H5Pprivate.h"
#include "H5VLprivate.h"

/* Macros for printing error messages in loops.  These print up to
 * GHEAP_REPEATED_ERR_LIM errors, and suppress the rest */
#define GHEAP_REPEATED_ERR_LIM 20

/* Number of heap objects to test */
#define GHEAP_TEST_NOBJS 1024

#define GHEAP_REPEATED_ERR(MSG)                                                                              \
    do {                                                                                                     \
        nerrors++;                                                                                           \
        if (nerrors <= GHEAP_REPEATED_ERR_LIM) {                                                             \
            H5_FAILED();                                                                                     \
            puts(MSG);                                                                                       \
            if (nerrors == GHEAP_REPEATED_ERR_LIM)                                                           \
                puts("    Suppressing further errors...");                                                   \
        }       /* end if */                                                                                 \
    } while (0) /* end GHEAP_REPEATED_ERR */

static const char *FILENAME[] = {"gheap1", "gheap2", "gheap3", "gheap4", "gheapooo", NULL};

/*-------------------------------------------------------------------------
 * Function:    test_1
 *
 * Purpose:     Writes a sequence of objects to the global heap where each
 *              object is larger than the one before.
 *
 * Return:      Success:    0
 *
 *              Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_1(hid_t fapl)
{
    hid_t   file = H5I_INVALID_HID;
    H5F_t  *f    = NULL;
    H5HG_t *obj  = NULL;
    uint8_t out[GHEAP_TEST_NOBJS];
    uint8_t in[GHEAP_TEST_NOBJS];
    size_t  u;
    size_t  size;
    herr_t  status;
    int     nerrors = 0;
    char    filename[1024];

    TESTING("monotonically increasing lengths");

    /* Allocate buffer for H5HG_t */
    if (NULL == (obj = (H5HG_t *)malloc(sizeof(H5HG_t) * GHEAP_TEST_NOBJS)))
        goto error;

    /* Open a clean file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        goto error;
    if (NULL == (f = (H5F_t *)H5VL_object(file))) {
        H5_FAILED();
        puts("    Unable to create file");
        goto error;
    }

    /*
     * Write the objects, monotonically increasing in length.  Since this is
     * a clean file, the addresses allocated for the collections should also
     * be monotonically increasing.
     */
    for (u = 0; u < GHEAP_TEST_NOBJS; u++) {
        size = u + 1;
        memset(out, (int)('A' + u % 26), size);
        H5Eclear2(H5E_DEFAULT);
        status = H5HG_insert(f, size, out, obj + u);
        if (status < 0) {
            H5_FAILED();
            puts("    Unable to insert object into global heap");
            nerrors++;
        }
        else if (u && H5_addr_gt(obj[u - 1].addr, obj[u].addr)) {
            H5_FAILED();
            puts("    Collection addresses are not monotonically increasing");
            nerrors++;
        }
    }

    /*
     * Now try to read each object back.
     */
    for (u = 0; u < GHEAP_TEST_NOBJS; u++) {
        size = u + 1;
        memset(out, (int)('A' + u % 26), size);
        H5Eclear2(H5E_DEFAULT);
        if (NULL == H5HG_read(f, obj + u, in, NULL)) {
            H5_FAILED();
            puts("    Unable to read object");
            nerrors++;
        }
        else if (memcmp(in, out, size) != 0) {
            H5_FAILED();
            puts("    Value read doesn't match value written");
            nerrors++;
        }
    }

    /* Release buffer */
    free(obj);
    obj = NULL;

    if (H5Fclose(file) < 0)
        goto error;
    if (nerrors)
        goto error;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Fclose(file);
    }
    H5E_END_TRY
    if (obj)
        free(obj);
    return MAX(1, nerrors);
}

/*-------------------------------------------------------------------------
 * Function:    test_2
 *
 * Purpose:     Writes a sequence of objects to the global heap where each
 *              object is smaller than the one before.
 *
 * Return:      Success:    0
 *
 *              Failure:     number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_2(hid_t fapl)
{
    hid_t   file = H5I_INVALID_HID;
    H5F_t  *f    = NULL;
    H5HG_t *obj  = NULL;
    uint8_t out[GHEAP_TEST_NOBJS];
    uint8_t in[GHEAP_TEST_NOBJS];
    size_t  u;
    size_t  size;
    int     nerrors = 0;
    char    filename[1024];

    TESTING("monotonically decreasing lengths");

    /* Allocate buffer for H5HG_t */
    if (NULL == (obj = (H5HG_t *)malloc(sizeof(H5HG_t) * GHEAP_TEST_NOBJS)))
        goto error;

    /* Open a clean file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        goto error;
    if (NULL == (f = (H5F_t *)H5VL_object(file))) {
        H5_FAILED();
        puts("    Unable to create file");
        goto error;
    }

    /*
     * Write the objects, monotonically decreasing in length.
     */
    for (u = 0; u < GHEAP_TEST_NOBJS; u++) {
        size = GHEAP_TEST_NOBJS - u;
        memset(out, (int)('A' + u % 26), size);
        H5Eclear2(H5E_DEFAULT);
        if (H5HG_insert(f, size, out, obj + u) < 0) {
            H5_FAILED();
            puts("    Unable to insert object into global heap");
            nerrors++;
        }
    }

    /*
     * Now try to read each object back.
     */
    for (u = 0; u < GHEAP_TEST_NOBJS; u++) {
        size = GHEAP_TEST_NOBJS - u;
        memset(out, (int)('A' + u % 26), size);
        H5Eclear2(H5E_DEFAULT);
        if (NULL == H5HG_read(f, obj + u, in, NULL)) {
            H5_FAILED();
            puts("    Unable to read object");
            nerrors++;
        }
        else if (memcmp(in, out, size) != 0) {
            H5_FAILED();
            puts("    Value read doesn't match value written");
            nerrors++;
        }
    }

    /* Release buffer */
    free(obj);
    obj = NULL;

    if (H5Fclose(file) < 0)
        goto error;
    if (nerrors)
        goto error;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Fclose(file);
    }
    H5E_END_TRY
    if (obj)
        free(obj);
    return MAX(1, nerrors);
}

/*-------------------------------------------------------------------------
 * Function:    test_3
 *
 * Purpose:     Creates a few global heap objects and then removes them all.
 *              The collection should also be removed.
 *
 * Return:      Success:    0
 *
 *              Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_3(hid_t fapl)
{
    hid_t   file = H5I_INVALID_HID;
    H5F_t  *f    = NULL;
    H5HG_t *obj  = NULL;
    uint8_t out[GHEAP_TEST_NOBJS];
    size_t  u;
    size_t  size;
    herr_t  status;
    int     nerrors = 0;
    char    filename[1024];

    TESTING("complete object removal");

    /* Allocate buffer for H5HG_t */
    if (NULL == (obj = (H5HG_t *)malloc(sizeof(H5HG_t) * GHEAP_TEST_NOBJS)))
        goto error;

    /* Open a clean file */
    h5_fixname(FILENAME[2], fapl, filename, sizeof filename);
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        goto error;
    if (NULL == (f = (H5F_t *)H5VL_object(file))) {
        H5_FAILED();
        puts("    Unable to create file");
        goto error;
    }

    /* Create some stuff */
    for (u = 0; u < GHEAP_TEST_NOBJS; u++) {
        size = u % 30 + 100;
        memset(out, (int)('A' + u % 26), size);
        H5Eclear2(H5E_DEFAULT);
        status = H5HG_insert(f, size, out, obj + u);
        if (status < 0) {
            H5_FAILED();
            puts("    Unable to insert object into global heap");
            nerrors++;
        }
    }

    /* Remove everything */
    for (u = 0; u < GHEAP_TEST_NOBJS; u++) {
        status = H5HG_remove(f, obj + u);
        if (status < 0) {
            H5_FAILED();
            puts("    Unable to remove object");
            nerrors++;
        }
    }

    /* Release buffer */
    free(obj);
    obj = NULL;

    if (H5Fclose(file) < 0)
        goto error;
    if (nerrors)
        goto error;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Fclose(file);
    }
    H5E_END_TRY
    if (obj)
        free(obj);
    return MAX(1, nerrors);
}

/*-------------------------------------------------------------------------
 * Function:    test_4
 *
 * Purpose:     Tests the H5HG_remove() feature by writing lots of objects
 *              and occasionally removing some.  When we're done they're all
 *              removed.
 *
 * Return:      Success:    0
 *
 *              Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_4(hid_t fapl)
{
    hid_t   file = H5I_INVALID_HID;
    H5F_t  *f    = NULL;
    H5HG_t *obj  = NULL;
    uint8_t out[GHEAP_TEST_NOBJS];
    size_t  u;
    size_t  size;
    herr_t  status;
    int     nerrors = 0;
    char    filename[1024];

    TESTING("partial object removal");

    /* Allocate buffer for H5HG_t */
    if (NULL == (obj = (H5HG_t *)malloc(sizeof(H5HG_t) * GHEAP_TEST_NOBJS)))
        goto error;

    /* Open a clean file */
    h5_fixname(FILENAME[3], fapl, filename, sizeof filename);
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        goto error;
    if (NULL == (f = (H5F_t *)H5VL_object(file))) {
        H5_FAILED();
        puts("    Unable to create file");
        goto error;
    }

    for (u = 0; u < GHEAP_TEST_NOBJS; u++) {
        /* Insert */
        size = u % 30 + 100;
        memset(out, (int)('A' + u % 26), size);
        H5Eclear2(H5E_DEFAULT);
        status = H5HG_insert(f, size, out, obj + u);
        if (status < 0) {
            H5_FAILED();
            puts("    Unable to insert object into global heap");
            nerrors++;
        }

        /* Remove every third one beginning with the second, but after the
         * next one has already been inserted.  That is, insert A, B, C;
         * remove B, insert D, E, F; remove E; etc.
         */
        if (1 == (u % 3)) {
            H5Eclear2(H5E_DEFAULT);
            status = H5HG_remove(f, obj + u - 1);
            if (status < 0) {
                H5_FAILED();
                puts("    Unable to remove object");
                nerrors++;
            }
            memset(obj + u - 1, 0, sizeof *obj);
        }
    }

    /* Release buffer */
    free(obj);
    obj = NULL;

    if (H5Fclose(file) < 0)
        goto error;
    if (nerrors)
        goto error;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Fclose(file);
    }
    H5E_END_TRY
    if (obj)
        free(obj);
    return MAX(1, nerrors);
}

/*-------------------------------------------------------------------------
 * Function:    test_ooo_indices
 *
 * Purpose:     Tests that indices can be stored out of order.  This can
 *              happen when the indices "wrap around" due to many
 *              insertions and deletions (for example, from rewriting a
 *              VL dataset).
 *
 * Return:      Success:    0
 *
 *              Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_ooo_indices(hid_t fapl)
{
    hid_t    file = H5I_INVALID_HID;
    H5F_t   *f    = NULL;
    unsigned i, j;
    H5HG_t  *obj = NULL;
    herr_t   status;
    int      nerrors = 0;
    char     filename[1024];

    TESTING("out of order indices");

    if (NULL == (obj = (H5HG_t *)malloc(2000 * sizeof(*obj))))
        goto error;

    /* Open a clean file */
    h5_fixname(FILENAME[4], fapl, filename, sizeof filename);
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        goto error;
    if (NULL == (f = (H5F_t *)H5VL_object(file))) {
        H5_FAILED();
        puts("    Unable to create file");
        goto error;
    }

    /* Alternately insert 1000 entries and remove the previous group of 1000
     * entries, until the indices wrap around.
     */
    for (i = 0; i < 66; i++) {
        /* Insert 1000 entries.  The index into the obj array will alternate up
         * and down by 1000 so the previous set of insertions is preserved and
         * can be deleted.
         */
        for (j = 1000 * ((~i & 1)); j < 1000 * ((~i & 1) + 1); j++) {
            H5Eclear2(H5E_DEFAULT);
            status = H5HG_insert(f, sizeof(j), &j, &obj[j]);
            if (status < 0)
                GHEAP_REPEATED_ERR("    Unable to insert object into global heap");

            /* Check that the index is as expected */
            if (obj[j].idx != ((1000 * i) + j - (1000 * ((~i & 1)))) % ((1U << 16) - 1) + 1)
                GHEAP_REPEATED_ERR("    Unexpected global heap index");
        }

        /* Remove the previous 1000 entries */
        if (i > 0)
            for (j = 1000 * (i & 1); j < 1000 * ((i & 1) + 1); j++) {
                H5Eclear2(H5E_DEFAULT);
                status = H5HG_remove(f, &obj[j]);
                if (status < 0)
                    GHEAP_REPEATED_ERR("    Unable to remove object from global heap");
            }
    }

    /* The indices should have "wrapped around" on the last iteration */
    assert(obj[534].idx == 65535);
    assert(obj[535].idx == 1);

    /* Reopen the file */
    if (H5Fclose(file) < 0)
        goto error;
    if ((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        goto error;
    if (NULL == (f = (H5F_t *)H5VL_object(file))) {
        H5_FAILED();
        puts("    Unable to open file");
        goto error;
    } /* end if */

    /* Read the objects to make sure the heap is still readable */
    for (i = 0; i < 1000; i++) {
        if (NULL == H5HG_read(f, &obj[i], &j, NULL))
            goto error;
        if (i != j) {
            H5_FAILED();
            puts("    Incorrect read value");
            goto error;
        }
    }

    if (H5Fclose(file) < 0)
        goto error;
    if (nerrors)
        goto error;
    free(obj);
    obj = NULL;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Fclose(file);
    }
    H5E_END_TRY
    if (obj)
        free(obj);
    return MAX(1, nerrors);
} /* end test_ooo_indices */

/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:     Tests global heap.
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    int   nerrors        = 0;
    hid_t fapl_id        = H5I_INVALID_HID;
    bool  api_ctx_pushed = false; /* Whether API context pushed */

    h5_reset();
    if ((fapl_id = h5_fileaccess()) < 0)
        goto error;

    /* Push API context */
    if (H5CX_push() < 0)
        FAIL_STACK_ERROR;
    api_ctx_pushed = true;

    nerrors += test_1(fapl_id);
    nerrors += test_2(fapl_id);
    nerrors += test_3(fapl_id);
    nerrors += test_4(fapl_id);
    nerrors += test_ooo_indices(fapl_id);

    /* Verify symbol table messages are cached */
    nerrors += (h5_verify_cached_stabs(FILENAME, fapl_id) < 0 ? 1 : 0);

    if (nerrors)
        goto error;

    puts("All global heap tests passed.");

    /* Pop API context */
    if (api_ctx_pushed && H5CX_pop(false) < 0)
        FAIL_STACK_ERROR;
    api_ctx_pushed = false;

    h5_cleanup(FILENAME, fapl_id);
    exit(EXIT_SUCCESS);

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl_id);
    }
    H5E_END_TRY

    if (api_ctx_pushed)
        H5CX_pop(false);

    puts("*** TESTS FAILED ***");
    exit(EXIT_FAILURE);
} /* end main() */
