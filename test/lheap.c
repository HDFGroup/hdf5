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
#include "H5ACprivate.h"
#include "H5CXprivate.h" /* API Contexts                             */
#include "H5HLprivate.h"
#define H5HL_FRIEND
#include "H5HLpkg.h" /* Local heaps (H5HL_t fields)              */
#include "H5Iprivate.h"
#include "H5VLprivate.h" /* Virtual Object Layer                     */

static const char *FILENAME[] = {"lheap", "lheap_unprotect", NULL};

#define TESTFILE "tsizeslheap.h5"

/* A file with a damaged local heap */
#define CORRUPT_HEAP_FILE "heap_corrupt_prfx.h5"

/* Bytes inserted into a heap to force its data block to be relocated, which
 * moves the data block into a cache entry of its own
 */
#define HEAP_GROW_SIZE 512

#define NOBJS 40

/* Attribute visitor.  Iterating is enough to decode the attributes. */
static herr_t
ignore_attr_cb(hid_t H5_ATTR_UNUSED loc_id, const char H5_ATTR_UNUSED *attr_name,
               const H5A_info_t H5_ATTR_UNUSED *ainfo, void H5_ATTR_UNUSED *op_data)
{
    return 0;
}

/* Link visitor that opens each object and reads its attributes.  Opening an
 * object by name protects and unprotects the local heap of its group.
 *
 * Errors are ignored: the file is damaged, so individual opens are allowed to
 * fail.  Do not iterate the opened group here -- the caller's H5Lvisit2()
 * already recurses over the whole file with cycle detection, and a nested
 * traversal restarts that detection and can recurse without bound.
 */
static herr_t
open_object_cb(hid_t group, const char *name, const H5L_info2_t *info, void H5_ATTR_UNUSED *op_data)
{
    H5O_info2_t oinfo;

    /* Only hard links refer to an object in this file */
    if (info->type != H5L_TYPE_HARD)
        return 0;

    H5E_BEGIN_TRY
    {
        if (H5Oget_info_by_name3(group, name, &oinfo, H5O_INFO_BASIC, H5P_DEFAULT) >= 0) {
            if (oinfo.type == H5O_TYPE_GROUP) {
                hid_t gid = H5Gopen2(group, name, H5P_DEFAULT);

                if (gid >= 0) {
                    H5Aiterate2(gid, H5_INDEX_NAME, H5_ITER_INC, NULL, ignore_attr_cb, NULL);
                    H5Gclose(gid);
                }
            }
            else if (oinfo.type == H5O_TYPE_DATASET) {
                hid_t did = H5Dopen2(group, name, H5P_DEFAULT);

                if (did >= 0) {
                    H5Aiterate2(did, H5_INDEX_NAME, H5_ITER_INC, NULL, ignore_attr_cb, NULL);
                    H5Dclose(did);
                }
            }
        }
    }
    H5E_END_TRY

    return 0;
}

/*-------------------------------------------------------------------------
 * Function:    test_corrupt_heap_traversal
 *
 * Purpose:     Walk every object in a file with a damaged local heap, using a
 *              metadata cache small enough that entries are evicted along the
 *              way.  The walk may fail, but must not crash the library or
 *              leave the file unclosable.
 *
 * Return:      Success:        0
 *              Failure:        1
 *
 *-------------------------------------------------------------------------
 */
static int
test_corrupt_heap_traversal(void)
{
    H5AC_cache_config_t mdc_config;
    const char         *testfile = NULL;
    hid_t               fapl     = H5I_INVALID_HID;
    hid_t               file     = H5I_INVALID_HID;

    TESTING("traversal of a file with a damaged local heap");

    if (NULL == (testfile = H5_get_srcdir_filename(CORRUPT_HEAP_FILE)))
        FAIL_PUTS_ERROR("unable to build the path of the damaged test file");

    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        FAIL_STACK_ERROR;

    /* Shrink the metadata cache */
    memset(&mdc_config, 0, sizeof(mdc_config));
    mdc_config.version = H5AC__CURR_CACHE_CONFIG_VERSION;
    if (H5Pget_mdc_config(fapl, &mdc_config) < 0)
        FAIL_STACK_ERROR;
    mdc_config.set_initial_size = true;
    mdc_config.initial_size     = (size_t)(16 * 1024);
    mdc_config.min_size         = (size_t)(16 * 1024);
    mdc_config.max_size         = (size_t)(64 * 1024);
    mdc_config.incr_mode        = H5C_incr__off;
    mdc_config.flash_incr_mode  = H5C_flash_incr__off;
    mdc_config.decr_mode        = H5C_decr__off;
    if (H5Pset_mdc_config(fapl, &mdc_config) < 0)
        FAIL_STACK_ERROR;

    /* The file is damaged, so the open and the walk are allowed to fail */
    H5E_BEGIN_TRY
    {
        file = H5Fopen(testfile, H5F_ACC_RDONLY, fapl);
    }
    H5E_END_TRY

    if (file >= 0) {
        H5E_BEGIN_TRY
        {
            H5Lvisit2(file, H5_INDEX_NAME, H5_ITER_INC, open_object_cb, NULL);
        }
        H5E_END_TRY

        if (H5Fclose(file) < 0)
            FAIL_STACK_ERROR;
        file = H5I_INVALID_HID;
    }

    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Fclose(file);
        H5Pclose(fapl);
    }
    H5E_END_TRY

    return 1;
} /* end test_corrupt_heap_traversal() */

/*-------------------------------------------------------------------------
 * Function:    test_unprotect_detached_entry
 *
 * Purpose:     H5HL_protect() pins one cache entry per heap -- the prefix for
 *              a single cache object, otherwise the data block -- which
 *              H5HL_unprotect() unpins again.  The cache unlinks that entry
 *              from the heap when it destroys it, leaving nothing to unpin.
 *
 *              Force that state for both kinds of heap and check that
 *              H5HL_unprotect() reports an error rather than dereferencing
 *              NULL, and that it leaves the heap untouched: the protect count
 *              is not consumed and the entry is not left pinned.
 *
 * Return:      Success:        0
 *              Failure:        1
 *
 *-------------------------------------------------------------------------
 */
static int
test_unprotect_detached_entry(hid_t fapl)
{
    char         filename[1024];
    hid_t        file       = H5I_INVALID_HID;
    H5F_t       *f          = NULL;
    haddr_t      heap_addr  = HADDR_UNDEF;
    haddr_t      other_addr = HADDR_UNDEF;
    H5HL_t      *heap       = NULL;
    H5HL_prfx_t *saved_prfx = NULL;
    H5HL_dblk_t *saved_dblk = NULL;
    char        *buf        = NULL;
    size_t       offset;
    herr_t       ret;

    TESTING("local heap unprotect with a detached cache entry");

    if (NULL == h5_fixname(FILENAME[1], fapl, filename, sizeof(filename)))
        FAIL_PUTS_ERROR("unable to build the test file name");
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR;
    if (NULL == (f = (H5F_t *)H5VL_object(file)))
        FAIL_STACK_ERROR;
    if (H5AC_ignore_tags(f) < 0)
        FAIL_STACK_ERROR;
    if (H5HL_create(f, (size_t)0, &heap_addr) < 0)
        FAIL_STACK_ERROR;

    /* A second heap right behind the first one keeps the first heap's data
     * block from being extended in place later on
     */
    if (H5HL_create(f, (size_t)HEAP_GROW_SIZE, &other_addr) < 0)
        FAIL_STACK_ERROR;

    /* A newly created heap is a single cache object: the prefix is pinned */
    if (NULL == (heap = H5HL_protect(f, heap_addr, H5AC__NO_FLAGS_SET)))
        FAIL_STACK_ERROR;
    if (!heap->single_cache_obj)
        FAIL_PUTS_ERROR("a newly created heap should be a single cache object");

    saved_prfx = heap->prfx;
    heap->prfx = NULL;
    H5E_BEGIN_TRY
    {
        ret = H5HL_unprotect(heap);
    }
    H5E_END_TRY
    heap->prfx = saved_prfx;

    if (ret >= 0)
        FAIL_PUTS_ERROR("H5HL_unprotect() succeeded for a heap without a prefix");

    /* The rejected call must not have consumed the protect count, so this is
     * the unprotect that balances the protect above
     */
    if (H5HL_unprotect(heap) < 0)
        FAIL_STACK_ERROR;
    heap = NULL;

    /* Grow the heap until its data block has a cache entry of its own, which
     * makes the data block the pinned entry
     */
    if (NULL == (heap = H5HL_protect(f, heap_addr, H5AC__NO_FLAGS_SET)))
        FAIL_STACK_ERROR;
    if (NULL == (buf = (char *)calloc((size_t)HEAP_GROW_SIZE, sizeof(char))))
        FAIL_PUTS_ERROR("memory allocation failed");
    if (H5HL_insert(f, heap, (size_t)HEAP_GROW_SIZE, buf, &offset) < 0)
        FAIL_STACK_ERROR;
    free(buf);
    buf = NULL;
    if (H5HL_unprotect(heap) < 0)
        FAIL_STACK_ERROR;
    heap = NULL;

    if (NULL == (heap = H5HL_protect(f, heap_addr, H5AC__NO_FLAGS_SET)))
        FAIL_STACK_ERROR;
    if (heap->single_cache_obj)
        FAIL_PUTS_ERROR("heap data block did not move into a cache entry of its own");

    saved_dblk = heap->dblk;
    heap->dblk = NULL;
    H5E_BEGIN_TRY
    {
        ret = H5HL_unprotect(heap);
    }
    H5E_END_TRY
    heap->dblk = saved_dblk;

    if (ret >= 0)
        FAIL_PUTS_ERROR("H5HL_unprotect() succeeded for a heap without a data block");

    if (H5HL_unprotect(heap) < 0)
        FAIL_STACK_ERROR;
    heap = NULL;

    /* Closing the file also checks that no entry was left pinned */
    if (H5Fclose(file) < 0)
        FAIL_STACK_ERROR;

    PASSED();

    return 0;

error:
    free(buf);
    H5E_BEGIN_TRY
    {
        if (heap)
            H5HL_unprotect(heap);
        H5Fclose(file);
    }
    H5E_END_TRY

    return 1;
} /* end test_unprotect_detached_entry() */

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

    if (driver_is_default_compatible) {
        /* A damaged local heap must not crash the library when it is walked */
        if (test_corrupt_heap_traversal() != 0)
            TEST_ERROR;
    }

    /* A heap whose pinned cache entry has been detached must be rejected */
    if (test_unprotect_detached_entry(fapl) != 0)
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
