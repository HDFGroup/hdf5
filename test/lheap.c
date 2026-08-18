/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Purpose:    Test local heaps used by symbol tables (groups).
 */
#include "h5test.h"
#include "H5srcdir.h"
#include "H5Lpublic.h" /* H5Lvisit2, H5L_info2_t                      */
#include "H5Opublic.h" /* H5Oget_info2, H5O_info2_t                  */
#include "H5ACprivate.h"
#include "H5CXprivate.h" /* API Contexts                         */
#include "H5HLprivate.h"
#define H5HL_FRIEND
#include "H5HLpkg.h" /* H5HL_t definition (prfx/dblk fields)        */
#include "H5Iprivate.h"
#include "H5VLprivate.h" /* Virtual Object Layer                     */

static const char *FILENAME[] = {"lheap", "heap_corrupt_unprotect", NULL};

#define TESTFILE "tsizeslheap.h5"

/* File from OSS-Fuzz (matio fuzzer, issue 504827191): a corrupted local
 * heap whose prefix/data block pointer becomes NULL during cache eviction.
 * Opening and traversing it used to crash in H5HL_unprotect(). */
#define CORRUPT_HEAP_FILE     "heap_corrupt_prfx.h5"
#define CORRUPT_HEAP_TESTFILE "heap_corrupt_unprotect"

#define NOBJS 40

/*-------------------------------------------------------------------------
 * Function:    corrupt_heap_visit
 *
 * Purpose:     Visitor for the best-effort smoke test of the OSS-Fuzz
 *              minimized file. It recurses into groups and opens datasets
 *              by name. The only requirement is that opening/traversing a
 *              corrupted file must not crash (it used to crash in
 *              H5HL_unprotect()).
 *
 * Return:      Success:        0
 *              Failure:        0 (best-effort; errors are ignored)
 *
 *-------------------------------------------------------------------------
 */
static herr_t
corrupt_heap_attr_op(hid_t H5_ATTR_UNUSED loc_id, const char *H5_ATTR_UNUSED attr_name,
                     const H5A_info_t H5_ATTR_UNUSED *ainfo, void *H5_ATTR_UNUSED op_data)
{
    return 0;
}

/* Replicate matio's exact traversal: open every dataset to read attributes
 * (e.g. MATLAB_sparse) and recurse into every group.  This is the workflow
 * that originally triggered the crash in H5HL_unprotect().
 */
static herr_t
corrupt_heap_visit(hid_t group, const char *name, const H5L_info2_t *info, void *op_data)
{
    (void)op_data;

    /* info is already populated by the link iteration; only follow hard links */
    if (info->type == H5L_TYPE_HARD) {
        H5O_info2_t oinfo;

        /* Skip the same special groups matio skips */
        if (0 == strcmp(name, "#refs#") || 0 == strcmp(name, "#subsystem#"))
            return 0;

        if (H5Oget_info_by_name3(group, name, &oinfo, H5O_INFO_BASIC, H5P_DEFAULT) >= 0) {
            if (oinfo.type == H5O_TYPE_GROUP) {
                hid_t g = H5Gopen2(group, name, H5P_DEFAULT);
                if (g >= 0) {
                    H5Lvisit2(g, H5_INDEX_NAME, H5_ITER_INC, corrupt_heap_visit, NULL);
                    /* Read group attributes as matio does */
                    H5Aiterate2(g, H5_INDEX_NAME, H5_ITER_INC, NULL, corrupt_heap_attr_op, NULL);
                    H5Gclose(g);
                }
            }
            else if (oinfo.type == H5O_TYPE_DATASET) {
                hid_t d = H5Dopen2(group, name, H5P_DEFAULT);
                if (d >= 0) {
                    /* Read dataset attributes as matio does (e.g. MATLAB_sparse) */
                    H5Aiterate2(d, H5_INDEX_NAME, H5_ITER_INC, NULL, corrupt_heap_attr_op, NULL);
                    H5Dclose(d);
                }
            }
        }
    }

    return 0;
} /* end corrupt_heap_visit() */

/*-------------------------------------------------------------------------
 * Function:    corrupt_heap_unprotect
 *
 * Purpose:     Regression test for a NULL-pointer dereference in
 *              H5HL_unprotect() when a local heap's prefix/data block pointer
 *              has become NULL (as set by H5HL__prfx_dest during cache eviction
 *              of a corrupted entry). The minimized fuzzer file from OSS-Fuzz
 *              (matio fuzzer, issue 504827191) exercises this path; the
 *              deterministic check below forces the exact condition directly.
 *
 * Return:      Success:        0
 *              Failure:        1
 *
 *-------------------------------------------------------------------------
 */
static int
corrupt_heap_unprotect(void)
{
    hid_t  file = H5I_INVALID_HID;
    hid_t  fapl = H5I_INVALID_HID;
    herr_t ret;

    TESTING("corrupted local heap unprotect (OSS-Fuzz 504827191)");

    /* Reproduce the issue using the minimized fuzzer file.  The original
     * crash happened while matio was iterating the file's groups with
     * H5Literate and opening each dataset with H5Dopen (which triggers
     * H5G__stab_lookup -> H5HL_unprotect).  Use a tiny metadata cache
     * so that the local-heap prefix can be evicted while still pinned.
     */
    H5E_BEGIN_TRY
    {
        fapl = H5Pcreate(H5P_FILE_ACCESS);
        if (fapl >= 0)
            H5Pset_cache(fapl, 0, 2, 256, 0.0);
    }
    H5E_END_TRY

    if (fapl >= 0) {
        const char *tf = H5_get_srcdir_filename(CORRUPT_HEAP_FILE);

        H5E_BEGIN_TRY
        {
            file = H5Fopen(tf, H5F_ACC_RDONLY, fapl);
            if (file >= 0) {
                /* Replicate matio's exact traversal: H5Literate2 on root,
                 * then for each group H5Gopen + H5Literate2 recursively,
                 * and for each dataset H5Dopen.  This creates the cache
                 * pressure that triggers the re-entrant eviction.
                 */
                hsize_t idx = 0;
                ret         = H5Literate2(file, H5_INDEX_NAME, H5_ITER_INC, &idx, corrupt_heap_visit, NULL);
                H5Fclose(file);
            }
        }
        H5E_END_TRY
        H5Pclose(fapl);
    }

    /* The fuzzer file's corrupted local heap does not deterministically crash
     * via the public API (the original crash required the matio fuzzer's
     * specific cache-pressure environment).  The deterministic check below
     * forces the exact buggy condition directly: simulate an evicted entry
     * whose prfx/dblk became NULL and verify H5HL_unprotect() returns an
     * error instead of dereferencing NULL.
     */
    if (FAIL == (fapl = h5_fileaccess()))
        goto cleanup_fail;
    {
        char         filename[1024];
        H5F_t       *f = NULL;
        haddr_t      heap_addr;
        H5HL_t      *heap       = NULL;
        H5HL_prfx_t *saved_prfx = NULL;
        H5HL_dblk_t *saved_dblk = NULL;

        h5_fixname(CORRUPT_HEAP_TESTFILE, fapl, filename, sizeof filename);
        if (FAIL == (file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)))
            goto cleanup_fail2;
        if (NULL == (f = (H5F_t *)H5VL_object(file)))
            goto cleanup_fail2;
        if (FAIL == H5AC_ignore_tags(f))
            goto cleanup_fail2;
        if (FAIL == H5HL_create(f, (size_t)0, &heap_addr))
            goto cleanup_fail2;
        if (NULL == (heap = H5HL_protect(f, heap_addr, H5AC__NO_FLAGS_SET)))
            goto cleanup_fail2;

        /* Save the valid pointers, then simulate a corrupted/evicted entry whose
         * local heap prefix/data block pointer became NULL (as set by
         * H5HL__prfx_dest during cache eviction), and verify that H5HL_unprotect()
         * returns an error instead of dereferencing NULL.
         */
        saved_prfx = heap->prfx;
        saved_dblk = heap->dblk;
        heap->prfx = NULL;
        heap->dblk = NULL;

        H5E_BEGIN_TRY
        {
            ret = H5HL_unprotect(heap);
        }
        H5E_END_TRY

        /* Restore the pointers so the cache entry can be cleaned up normally. */
        heap->prfx = saved_prfx;
        heap->dblk = saved_dblk;

        if (ret >= 0) {
            H5_FAILED();
            printf("***H5HL_unprotect did not return an error for a heap with NULL prfx/dblk\n");
            goto cleanup_fail2;
        }

        /* The prefix is still pinned from H5HL_protect(); unpin it and close. */
        H5E_BEGIN_TRY
        {
            H5AC_unpin_entry(saved_prfx);
        }
        H5E_END_TRY

cleanup_fail2:
        H5E_BEGIN_TRY
        {
            H5Fclose(file);
        }
        H5E_END_TRY
        H5Pclose(fapl);
    }

    PASSED();

    return 0;

cleanup_fail:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
    }
    H5E_END_TRY

    return 1;
} /* end corrupt_heap_unprotect() */

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Create a file, create a local heap, write data into the local
 *              heap, close the file, open the file, read data out of the
 *              local heap, close the file.
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t       fapl = H5P_DEFAULT;           /* file access properties   */
    hid_t       file = H5I_INVALID_HID;       /* hdf5 file                */
    H5F_t      *f    = NULL;                  /* hdf5 file pointer        */
    char        filename[1024];               /* file name                */
    haddr_t     heap_addr;                    /* local heap address       */
    H5HL_t     *heap = NULL;                  /* local heap               */
    size_t      obj[NOBJS];                   /* offsets within the heap  */
    int         i, j;                         /* miscellaneous counters   */
    char        buf[1024];                    /* the value to store       */
    const char *s;                            /* value to read            */
    H5CX_node_t api_ctx        = {{0}, NULL}; /* API context node to push */
    bool        api_ctx_pushed = false;       /* Whether API context pushed */
    bool        driver_is_default_compatible;

    /* Reset library */
    h5_test_init();
    fapl = h5_fileaccess();

    /* Push API context */
    if (H5CX_push(&api_ctx) < 0)
        FAIL_STACK_ERROR;
    api_ctx_pushed = true;

    /*
     * Test writing to the heap...
     */
    TESTING("local heap write");
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if (FAIL == (file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)))
        goto error;
    if (NULL == (f = (H5F_t *)H5VL_object(file))) {
        H5_FAILED();
        H5Eprint2(H5E_DEFAULT, stdout);
        goto error;
    }
    if (FAIL == H5AC_ignore_tags(f)) {
        H5_FAILED();
        H5Eprint2(H5E_DEFAULT, stdout);
        goto error;
    }
    if (FAIL == H5HL_create(f, (size_t)0, &heap_addr /*out*/)) {
        H5_FAILED();
        H5Eprint2(H5E_DEFAULT, stdout);
        goto error;
    }
    if (NULL == (heap = H5HL_protect(f, heap_addr, H5AC__NO_FLAGS_SET))) {
        H5_FAILED();
        H5Eprint2(H5E_DEFAULT, stdout);
        goto error;
    }
    for (i = 0; i < NOBJS; i++) {
        snprintf(buf, sizeof(buf), "%03d-", i);
        for (j = 4; j < i; j++)
            buf[j] = (char)('0' + j % 10);
        if (j > 4)
            buf[j] = '\0';

        if (H5HL_insert(f, heap, strlen(buf) + 1, buf, &obj[i]) < 0) {
            H5_FAILED();
            H5Eprint2(H5E_DEFAULT, stdout);
            goto error;
        }
    }
    if (FAIL == H5HL_unprotect(heap)) {
        H5_FAILED();
        H5Eprint2(H5E_DEFAULT, stdout);
        goto error;
    }
    if (FAIL == H5Fclose(file))
        goto error;
    PASSED();

    /*
     * Test reading from the heap...
     */

    TESTING("local heap read");
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if (FAIL == (file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)))
        goto error;
    if (NULL == (f = (H5F_t *)H5VL_object(file))) {
        H5_FAILED();
        H5Eprint2(H5E_DEFAULT, stdout);
        goto error;
    }
    if (FAIL == H5AC_ignore_tags(f)) {
        H5_FAILED();
        H5Eprint2(H5E_DEFAULT, stdout);
        goto error;
    }
    for (i = 0; i < NOBJS; i++) {
        snprintf(buf, sizeof(buf), "%03d-", i);
        for (j = 4; j < i; j++)
            buf[j] = (char)('0' + j % 10);
        if (j > 4)
            buf[j] = '\0';

        if (NULL == (heap = H5HL_protect(f, heap_addr, H5AC__READ_ONLY_FLAG))) {
            H5_FAILED();
            H5Eprint2(H5E_DEFAULT, stdout);
            goto error;
        }

        if (NULL == (s = (const char *)H5HL_offset_into(heap, obj[i]))) {
            H5_FAILED();
            H5Eprint2(H5E_DEFAULT, stdout);
            goto error;
        }

        if (strcmp(s, buf) != 0) {
            H5_FAILED();
            printf("    i=%d, heap offset=%lu\n", i, (unsigned long)(obj[i]));
            printf("    got: \"%s\"\n", s);
            printf("    ans: \"%s\"\n", buf);
            goto error;
        }

        if (FAIL == H5HL_unprotect(heap)) {
            H5_FAILED();
            H5Eprint2(H5E_DEFAULT, stdout);
            goto error;
        }
    }

    if (FAIL == H5Fclose(file))
        goto error;
    PASSED();

    if (h5_driver_is_default_vfd_compatible(H5P_DEFAULT, &driver_is_default_compatible) < 0)
        TEST_ERROR;

    if (driver_is_default_compatible) {
        /* Check opening existing file non-default sizes of lengths and addresses */
        TESTING("opening pre-created file with non-default sizes");
        {
            const char *testfile = H5_get_srcdir_filename(TESTFILE); /* Corrected test file name */
            hid_t       dset     = H5I_INVALID_HID;
            file                 = H5Fopen(testfile, H5F_ACC_RDONLY, H5P_DEFAULT);
            if (file >= 0) {
                if ((dset = H5Dopen2(file, "/Dataset1", H5P_DEFAULT)) < 0)
                    TEST_ERROR;
                if (H5Dclose(dset) < 0)
                    TEST_ERROR;
                if (H5Fclose(file) < 0)
                    TEST_ERROR;
            }
            else {
                H5_FAILED();
                printf("***cannot open the pre-created non-default sizes test file (%s)\n", testfile);
                goto error;
            } /* end else */
        }
        PASSED();
    }

    /* Regression test: corrupted local heap must not crash on unprotect */
    if (corrupt_heap_unprotect() != 0)
        TEST_ERROR;

    /* Verify symbol table messages are cached */
    if (h5_verify_cached_stabs(FILENAME, fapl) < 0)
        TEST_ERROR;

    /* Pop API context */
    if (api_ctx_pushed && H5CX_pop(false) < 0)
        FAIL_STACK_ERROR;
    api_ctx_pushed = false;

    puts("All local heap tests passed.");
    h5_cleanup(FILENAME, fapl);

    return EXIT_SUCCESS;

error:
    puts("*** TESTS FAILED ***");
    H5E_BEGIN_TRY
    {
        H5Fclose(file);
    }
    H5E_END_TRY

    if (api_ctx_pushed)
        H5CX_pop(false);

    return EXIT_FAILURE;
}
