/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Tuesday, March 31, 1998
 *
 * Purpose:	Tests the global heap.  The global heap is the set of all
 *		collections but the collections are not related to one
 *		another by anything that appears in the file format.
 */
#include "h5test.h"
#include "H5private.h"
#include "H5ACprivate.h"
#include "H5Eprivate.h"
#include "H5Fprivate.h"
#include "H5Gprivate.h"
#include "H5HGprivate.h"
#include "H5Iprivate.h"
#include "H5Pprivate.h"

/* Macros for printing error messages in loops.  These print up to
 * GHEAP_REPEATED_ERR_LIM errors, and suppress the rest */
#define GHEAP_REPEATED_ERR_LIM 20

#define GHEAP_REPEATED_ERR(MSG)                                                \
{                                                                              \
    nerrors++;                                                                 \
    if(nerrors <= GHEAP_REPEATED_ERR_LIM) {                                    \
        H5_FAILED();                                                           \
        puts(MSG);                                                             \
        if(nerrors == GHEAP_REPEATED_ERR_LIM)                                  \
            puts("    Suppressing further errors...");                         \
    } /* end if */                                                             \
} /* end GHEAP_REPEATED_ERR */

const char *FILENAME[] = {
    "gheap1",
    "gheap2",
    "gheap3",
    "gheap4",
    "gheap5",
    NULL
};

/* Number of heap objects in tests */
#define N_GHEAP_OBJS    1024

/* Size of heap object buffers */
#define OBJ_BUF_SIZE    N_GHEAP_OBJS

/* Size of filename */
#define FILENAME_SIZE   1024


/*-------------------------------------------------------------------------
 * Function:    test_monotonic_increasing
 *
 * Purpose:     Writes a sequence of objects to the global heap where each
 *              object is larger than the one before.
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *
 * Programmer:  Robb Matzke
 *              Tuesday, March 31, 1998
 *
 *-------------------------------------------------------------------------
 */
static int
test_monotonic_increasing(hid_t fapl)
{
    char        *filename = NULL;   /* VFD-dependent filename           */
    hid_t       fid = -1;           /* HDF5 file ID                     */
    H5F_t       *f = NULL;          /* file object pointer              */
    H5HG_t      *obj = NULL;        /* global heap objects              */
    uint8_t     *in = NULL;         /* global heap data sent            */
    uint8_t     *out = NULL;        /* global heap data received        */
    unsigned    i;                  /* iterator                         */
    size_t      size;               /* heap object size                 */
    int         nerrors = 0;        /* # of errors encountered          */

    TESTING("monotonically increasing lengths");

    /* allocate memory */
    if(NULL == (filename = (char *)HDcalloc(FILENAME_SIZE, sizeof(char))))
        goto error;
    if(NULL == (obj = (H5HG_t *)HDcalloc(N_GHEAP_OBJS, sizeof(H5HG_t))))
        goto error;
    if(NULL == (in = (uint8_t *)HDcalloc(OBJ_BUF_SIZE, sizeof(uint8_t))))
        goto error;
    if(NULL == (out = (uint8_t *)HDcalloc(OBJ_BUF_SIZE, sizeof(uint8_t))))
        goto error;

    /* Open a clean file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        goto error;
    if(NULL == (f = (H5F_t *)H5I_object(fid))) {
        H5_FAILED();
        HDputs("    Unable to create file");
        goto error;
    } /* end if */

    /* Write the objects, monotonically increasing in length.  Since this is
     * a clean file, the addresses allocated for the collections should also
     * be monotonically increasing.
     */
    for(i = 0; i < N_GHEAP_OBJS; i++) {
        size = i + 1;
        HDmemset(out, 'A' + (int)i % 26, size);
        H5Eclear2(H5E_DEFAULT);
        if(H5HG_insert(f, H5AC_dxpl_id, size, out, obj + i) < 0) {
            H5_FAILED();
            HDputs("    Unable to insert object into global heap");
	        nerrors++;
        } else if(i && H5F_addr_gt(obj[i - 1].addr, obj[i].addr)) {
            H5_FAILED();
            HDputs("    Collection addresses are not monotonically increasing");
            nerrors++;
        } /* end if */
    } /* end for */

    /* Now try to read each object back. */
    for(i = 0; i < N_GHEAP_OBJS; i++) {
        size = i + 1;
        HDmemset(out, 'A' + (int)i % 26, size);
        H5Eclear2(H5E_DEFAULT);
        if(NULL == H5HG_read(f, H5AC_dxpl_id, obj + i, in, NULL)) {
            H5_FAILED();
            HDputs("    Unable to read object");
            nerrors++;
	    } else if(HDmemcmp(in, out, size)) {
            H5_FAILED();
            HDputs("    Value read doesn't match value written");
            nerrors++;
        } /* end if */
    } /* end for */

    if(H5Fclose(fid) < 0) goto error;
    if(nerrors) goto error;

    HDfree(filename);
    HDfree(obj);
    HDfree(in);
    HDfree(out);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Fclose(fid);
    } H5E_END_TRY;
    if(filename)
        HDfree(filename);
    if(obj)
        HDfree(obj);
    if(in)
        HDfree(in);
    if(out)
        HDfree(out);
    return MAX(1, nerrors);
} /* end test_monotonic_increasing() */


/*-------------------------------------------------------------------------
 * Function:    test_monotonic_decreasing
 *
 * Purpose:     Writes a sequence of objects to the global heap where each
 *              object is smaller than the one before.
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *
 * Programmer:  Robb Matzke
 *              Tuesday, March 31, 1998
 *
 *-------------------------------------------------------------------------
 */
static int
test_monotonic_decreasing(hid_t fapl)
{
    char        *filename = NULL;   /* VFD-dependent filename           */
    hid_t       fid = -1;           /* HDF5 file ID                     */
    H5F_t       *f = NULL;          /* file object pointer              */
    H5HG_t      *obj = NULL;        /* global heap objects              */
    uint8_t     *in = NULL;         /* global heap data sent            */
    uint8_t     *out = NULL;        /* global heap data received        */
    unsigned    i;                  /* iterator                         */
    size_t      size;               /* heap object size                 */
    int         nerrors = 0;        /* # of errors encountered          */

    TESTING("monotonically decreasing lengths");

    /* allocate memory */
    if(NULL == (filename = (char *)HDcalloc(FILENAME_SIZE, sizeof(char))))
        goto error;
    if(NULL == (obj = (H5HG_t *)HDcalloc(N_GHEAP_OBJS, sizeof(H5HG_t))))
        goto error;
    if(NULL == (in = (uint8_t *)HDcalloc(OBJ_BUF_SIZE, sizeof(uint8_t))))
        goto error;
    if(NULL == (out = (uint8_t *)HDcalloc(OBJ_BUF_SIZE, sizeof(uint8_t))))
        goto error;

    /* Open a clean file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof(filename));
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        goto error;
    if(NULL == (f = (H5F_t *)H5I_object(fid))) {
        H5_FAILED();
        HDputs("    Unable to create file");
        goto error;
    } /* end if */

    /* Write the objects, monotonically decreasing in length. */
    for(i = 0; i < N_GHEAP_OBJS; i++) {
        size = N_GHEAP_OBJS - i;
        HDmemset(out, 'A' + (int)i % 26, size);
        H5Eclear2(H5E_DEFAULT);
        if(H5HG_insert(f, H5AC_dxpl_id, size, out, obj+i) < 0) {
            H5_FAILED();
            HDputs("    Unable to insert object into global heap");
            nerrors++;
        } /* end if */
    } /* end for */

    /* Now try to read each object back. */
    for(i = 0; i < N_GHEAP_OBJS; i++) {
        size = N_GHEAP_OBJS - i;
        HDmemset(out, 'A' + (int)i % 26, size);
        H5Eclear2(H5E_DEFAULT);
        if(NULL == H5HG_read(f, H5AC_dxpl_id, obj+i, in, NULL)) {
            H5_FAILED();
            HDputs("    Unable to read object");
            nerrors++;
        } else if (HDmemcmp(in, out, size)) {
            H5_FAILED();
            HDputs("    Value read doesn't match value written");
            nerrors++;
        } /* end if */
    } /* end for */

    if(H5Fclose(fid) < 0) goto error;
    if(nerrors) goto error;

    HDfree(filename);
    HDfree(obj);
    HDfree(in);
    HDfree(out);

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
        H5Fclose(fid);
    } H5E_END_TRY;
    if(filename)
        HDfree(filename);
    if(obj)
        HDfree(obj);
    if(in)
        HDfree(in);
    if(out)
        HDfree(out);
    return MAX(1, nerrors);
} /* end test_monotonic_decreasing() */


/*-------------------------------------------------------------------------
 * Function:	test_complete_removal
 *
 * Purpose:     Creates a few global heap objects and then removes them all.
 *              The collection should also be removed.
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *
 * Programmer:  Robb Matzke
 *              Tuesday, March 31, 1998
 *
 *-------------------------------------------------------------------------
 */
static int
test_complete_removal(hid_t fapl)
{
    char        *filename = NULL;   /* VFD-dependent filename           */
    hid_t       fid = -1;           /* HDF5 file ID                     */
    H5F_t       *f = NULL;          /* file object pointer              */
    H5HG_t      *obj = NULL;        /* global heap objects              */
    uint8_t     *out = NULL;        /* global heap data received        */
    unsigned    i;                  /* iterator                         */
    size_t      size;               /* heap object size                 */
    int         nerrors = 0;        /* # of errors encountered          */

    TESTING("complete object removal");

    /* allocate memory */
    if(NULL == (filename = (char *)HDcalloc(FILENAME_SIZE, sizeof(char))))
        goto error;
    if(NULL == (obj = (H5HG_t *)HDcalloc(N_GHEAP_OBJS, sizeof(H5HG_t))))
        goto error;
    if(NULL == (out = (uint8_t *)HDcalloc(OBJ_BUF_SIZE, sizeof(uint8_t))))
        goto error;

    /* Open a clean file */
    h5_fixname(FILENAME[2], fapl, filename, sizeof(filename));
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        goto error;
    if(NULL == (f = (H5F_t *)H5I_object(fid))) {
        H5_FAILED();
        HDputs("    Unable to create file");
        goto error;
    } /* end if */

    /* Create some stuff */
    for(i = 0; i < N_GHEAP_OBJS; i++) {
        size = i % 30 + 100;
        HDmemset(out, 'A' + (int)i % 26, size);
        H5Eclear2(H5E_DEFAULT);
        if(H5HG_insert(f, H5AC_dxpl_id, size, out, obj+i) < 0) {
            H5_FAILED();
            HDputs("    Unable to insert object into global heap");
            nerrors++;
        } /* end if */
    } /* end for */

    /* Remove everything */
    for(i = 0; i < N_GHEAP_OBJS; i++) {
        if(H5HG_remove(f, H5AC_dxpl_id, obj+i) < 0) {
            H5_FAILED();
            HDputs("    Unable to remove object");
            nerrors++;
        } /* end if */
    } /* end for */

    if(H5Fclose(fid) < 0) goto error;
    if(nerrors) goto error;

    HDfree(filename);
    HDfree(obj);
    HDfree(out);

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
        H5Fclose(fid);
    } H5E_END_TRY;
    if(filename)
        HDfree(filename);
    if(obj)
        HDfree(obj);
    if(out)
        HDfree(out);
    return MAX(1, nerrors);
} /* end test_complete_removal() */


/*-------------------------------------------------------------------------
 * Function:    test_partial_removal
 *
 * Purpose:     Tests the H5HG_remove() feature by writing lots of objects
 *              and occassionally removing some.  When we're done they're all
 *              removed.
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *
 * Programmer:  Robb Matzke
 *              Tuesday, March 31, 1998
 *
 *-------------------------------------------------------------------------
 */
static int
test_partial_removal(hid_t fapl)
{
    char        *filename = NULL;   /* VFD-dependent filename           */
    hid_t       fid = -1;           /* HDF5 file ID                     */
    H5F_t       *f = NULL;          /* file object pointer              */
    H5HG_t      *obj = NULL;        /* global heap objects              */
    uint8_t     *out = NULL;        /* global heap data received        */
    unsigned    i;                  /* iterator                         */
    size_t      size;               /* heap object size                 */
    int         nerrors = 0;        /* # of errors encountered          */

    TESTING("partial object removal");

    /* allocate memory */
    if(NULL == (filename = (char *)HDcalloc(FILENAME_SIZE, sizeof(char))))
        goto error;
    if(NULL == (obj = (H5HG_t *)HDcalloc(N_GHEAP_OBJS, sizeof(H5HG_t))))
        goto error;
    if(NULL == (out = (uint8_t *)HDcalloc(OBJ_BUF_SIZE, sizeof(uint8_t))))
        goto error;

    /* Open a clean file */
    h5_fixname(FILENAME[3], fapl, filename, sizeof(filename));
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        goto error;
    if(NULL == (f = (H5F_t *)H5I_object(fid))) {
        H5_FAILED();
        HDputs("    Unable to create file");
        goto error;
    } /* end if */

    for(i = 0; i < N_GHEAP_OBJS; i++) {

        /* Insert */
        size = i % 30 + 100;
        HDmemset(out, 'A' + (int)i % 26, size);
        H5Eclear2(H5E_DEFAULT);
        if(H5HG_insert(f, H5AC_dxpl_id, size, out, obj+i) < 0) {
            H5_FAILED();
            HDputs("    Unable to insert object into global heap");
            nerrors++;
        } /* end if */

        /* Remove every third one beginning with the second, but after the
         * next one has already been inserted.  That is, insert A, B, C;
         * remove B, insert D, E, F; remove E; etc.
         */
        if(1 == i % 3) {
            H5Eclear2(H5E_DEFAULT);
            if(H5HG_remove(f, H5AC_dxpl_id, obj+i-1) < 0) {
                H5_FAILED();
                HDputs("    Unable to remove object");
                nerrors++;
            } /* end if */
            HDmemset(obj+i-1, 0, sizeof(*obj));
        } /* end if */
    } /* end for */

    if(H5Fclose(fid) < 0) goto error;
    if(nerrors) goto error;

    HDfree(filename);
    HDfree(obj);
    HDfree(out);

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
        H5Fclose(fid);
    } H5E_END_TRY;
    if(filename)
        HDfree(filename);
    if(obj)
        HDfree(obj);
    if(out)
        HDfree(out);
    return MAX(1, nerrors);
} /* end test_partial_removal() */


/*-------------------------------------------------------------------------
 * Function:    test_ooo_indices
 *
 * Purpose:     Tests that indices can be stored out of order.  This can
 *              happen when the indices "wrap around" due to many
 *              insertions and deletions (for example, from rewriting a
 *              VL dataset).
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *
 * Programmer:  Neil Fortner
 *              Monday, October 26, 2009
 *
 *-------------------------------------------------------------------------
 */
static int
test_ooo_indices(hid_t fapl)
{
    char        *filename = NULL;   /* VFD-dependent filename           */
    hid_t       fid = -1;           /* HDF5 file ID                     */
    H5F_t       *f = NULL;          /* file object pointer              */
    H5HG_t      *obj = NULL;        /* global heap objects              */
    unsigned    i, j;               /* iterators                        */
    int         nerrors = 0;        /* # of errors encountered          */

    TESTING("out of order indices");

    /* allocate memory */
    if(NULL == (filename = (char *)HDcalloc(FILENAME_SIZE, sizeof(char))))
        goto error;
    if(NULL == (obj = (H5HG_t *)HDmalloc(2000 * sizeof(*obj))))
        goto error;

    /* Open a clean file */
    h5_fixname(FILENAME[4], fapl, filename, sizeof(filename));
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        goto error;
    if(NULL == (f = (H5F_t *)H5I_object(fid))) {
        H5_FAILED();
        HDputs("    Unable to create file");
        goto error;
    } /* end if */

    /* Alternately insert 1000 entries and remove the previous group of 1000
     * entries, until the indices wrap around.
     */
    for(i = 0; i < 66; i++) {

        /* Insert 1000 entries.  The index into the obj array will alternate up
         * and down by 1000 so the previous set of insertions is preserved and
         * can be deleted.
         */
        for(j = 1000 * ((~i & 1)); j < 1000 * ((~i & 1) + 1); j++) {
            H5Eclear2(H5E_DEFAULT);
            if(H5HG_insert(f, H5AC_dxpl_id, sizeof(j), &j, &obj[j]) < 0)
                GHEAP_REPEATED_ERR("    Unable to insert object into global heap")

            /* Check that the index is as expected */
            if(obj[j].idx != ((1000 * i) + j - (1000 * ((~i & 1)))) % ((1u << 16) - 1) + 1)
                GHEAP_REPEATED_ERR("    Unexpected global heap index");
        } /* end for */

        /* Remove the previous 1000 entries */
        if(i > 0)
            for(j = 1000 * (i & 1); j < 1000 * ((i & 1) + 1); j++) {
                H5Eclear2(H5E_DEFAULT);
                if(H5HG_remove(f, H5AC_dxpl_id, &obj[j]) < 0)
                    GHEAP_REPEATED_ERR("    Unable to remove object from global heap");
            } /* end for */
    } /* end for */

    /* The indices should have "wrapped around" on the last iteration */
    HDassert(obj[534].idx == 65535);
    HDassert(obj[535].idx == 1);

    /* Reopen the file */
    if(H5Fclose(fid) < 0) goto error;
    if((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        goto error;
    if(NULL == (f = (H5F_t *)H5I_object(fid))) {
        H5_FAILED();
        HDputs("    Unable to open file");
        goto error;
    } /* end if */

    /* Read the objects to make sure the heap is still readable */
    for(i = 0; i < 1000; i++) {
        if(NULL == H5HG_read(f, H5AC_dxpl_id, &obj[i], &j, NULL))
            goto error;
        if(i != j) {
            H5_FAILED();
            HDputs("    Incorrect read value");
            goto error;
        } /* end if */
    } /* end for */

    if(H5Fclose(fid) < 0) goto error;
    if(nerrors) goto error;

    HDfree(filename);
    HDfree(obj);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Fclose(fid);
    } H5E_END_TRY;
    if(filename)
        HDfree(filename);
    if(obj)
        HDfree(obj);
    return MAX(1, nerrors);
} /* end test_ooo_indices */


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests global heap
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 * Programmer:  Robb Matzke
 *              Tuesday, March 31, 1998
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    int		nerrors = 0;        /* # of errors                          */
    hid_t	fapl = -1;          /* VFD-dependent fapl ID                */

    h5_reset();
    fapl = h5_fileaccess();

    nerrors += test_monotonic_increasing(fapl);
    nerrors += test_monotonic_decreasing(fapl);
    nerrors += test_complete_removal(fapl);
    nerrors += test_partial_removal(fapl);
    nerrors += test_ooo_indices(fapl);

    /* Verify symbol table messages are cached */
    nerrors += (h5_verify_cached_stabs(FILENAME, fapl) < 0 ? 1 : 0);

    if(nerrors) goto error;

    HDputs("All global heap tests passed.");
    h5_cleanup(FILENAME, fapl);
    return EXIT_SUCCESS;

error:
    HDputs("*** TESTS FAILED ***");
    return EXIT_FAILURE;
} /* end main() */

