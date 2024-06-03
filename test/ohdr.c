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

#include "h5test.h"

#include "H5CXprivate.h" /* API Contexts                             */
#include "H5Iprivate.h"  /* Identifiers                              */
#include "H5VLprivate.h" /* Virtual Object Layer                     */

/*
 * This file needs to access private datatypes from the H5O package.
 * This file also needs to access the object header testing code.
 */
#define H5O_FRIEND /* suppress error about including H5Opkg */
#define H5O_TESTING
#include "H5Opkg.h"

/*
 * This file needs to access private datatypes from the H5G package.
 */
#define H5G_FRIEND /* suppress error about including H5Gpkg */
#include "H5Gpkg.h"

/*
 * This file needs to access private routines from the H5FD package.
 */
#define H5FD_FRIEND /* suppress error about including H5FDpkg */
#define H5FD_TESTING
#include "H5FDpkg.h"

static const char *FILENAME[] = {"ohdr", "ohdr_min_a", "ohdr_min_b", NULL};

/* used for object header size comparison */
#define EQ 1
#define LT 2
#define GT 3

/* The tbogus.h5 is generated from gen_bogus.c in HDF5 'test' directory.
 * To get this data file, define H5O_ENABLE_BOGUS in src/H5Oprivate, rebuild
 * the library and simply compile gen_bogus.c with that HDF5 library and run it.
 */
#define FILE_BOGUS   "tbogus.h5"
#define TESTFILE_LEN 256

/*  */
#define FILE_OHDR_SWMR     "ohdr_swmr.h5"
#define DSET_NAME          "COMPACT_DSET"
#define OBJ_VERSION_LATEST 2

/*
 *  Verify that messages are moved forward into a "continuation message":
 *    Create an object header with several continuation chunks
 *    Remove a message in the last chunk
 *    The remaining message(s) in the last chunk should be moved forward into the continuation message
 *    The process will repeat when the continuation message is big enough to hold all the
 *        messages in the last chunk.
 *    Result: the number of chunks should be reduced
 */
static herr_t
test_cont(char *filename, hid_t fapl)
{
    hid_t          file = H5I_INVALID_HID;
    H5F_t         *f    = NULL;
    H5O_hdr_info_t hdr_info;
    H5O_loc_t      oh_locA, oh_locB;
    time_t         time_new;
    const char    *short_name = "T";
    const char    *long_name  = "This is the message";
    size_t         nchunks;

    TESTING("object header continuation block");

    memset(&oh_locA, 0, sizeof(oh_locA));
    memset(&oh_locB, 0, sizeof(oh_locB));

    /* Create the file to operate on */
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR;
    if (NULL == (f = (H5F_t *)H5VL_object(file)))
        FAIL_STACK_ERROR;
    if (H5AC_ignore_tags(f) < 0) {
        H5_FAILED();
        H5Eprint2(H5E_DEFAULT, stdout);
        goto error;
    } /* end if */

    if (H5O_create(f, (size_t)H5O_MIN_SIZE, (size_t)0, H5P_GROUP_CREATE_DEFAULT, &oh_locA /*out*/) < 0)
        FAIL_STACK_ERROR;

    if (H5O_create(f, (size_t)H5O_MIN_SIZE, (size_t)0, H5P_GROUP_CREATE_DEFAULT, &oh_locB /*out*/) < 0)
        FAIL_STACK_ERROR;

    time_new = 11111111;

    if (H5O_msg_create(&oh_locA, H5O_NAME_ID, 0, 0, &long_name) < 0)
        FAIL_STACK_ERROR;

    if (H5O_msg_create(&oh_locB, H5O_MTIME_ID, 0, 0, &time_new) < 0)
        FAIL_STACK_ERROR;
    if (H5O_msg_create(&oh_locB, H5O_MTIME_ID, 0, 0, &time_new) < 0)
        FAIL_STACK_ERROR;
    if (H5O_msg_create(&oh_locB, H5O_MTIME_ID, 0, 0, &time_new) < 0)
        FAIL_STACK_ERROR;

    if (H5O_msg_create(&oh_locA, H5O_MTIME_NEW_ID, 0, 0, &time_new) < 0)
        FAIL_STACK_ERROR;

    if (H5O_msg_create(&oh_locB, H5O_MTIME_ID, 0, 0, &time_new) < 0)
        FAIL_STACK_ERROR;

    if (H5O_msg_create(&oh_locA, H5O_NAME_ID, 0, 0, &short_name) < 0)
        FAIL_STACK_ERROR;

    if (1 != H5O_link(&oh_locA, 1))
        FAIL_STACK_ERROR;
    if (1 != H5O_link(&oh_locB, 1))
        FAIL_STACK_ERROR;
    if (H5AC_prep_for_file_flush(f) < 0)
        FAIL_STACK_ERROR;
    if (H5AC_flush(f) < 0)
        FAIL_STACK_ERROR;
    if (H5AC_secure_from_file_flush(f) < 0)
        FAIL_STACK_ERROR;
    if (H5O__expunge_chunks_test(&oh_locA) < 0)
        FAIL_STACK_ERROR;

    if (H5O_get_hdr_info(&oh_locA, &hdr_info) < 0)
        FAIL_STACK_ERROR;
    nchunks = hdr_info.nchunks;

    /* remove the 1st H5O_NAME_ID message */
    if (H5O_msg_remove(&oh_locA, H5O_NAME_ID, 0, false) < 0)
        FAIL_STACK_ERROR;

    if (H5O_get_hdr_info(&oh_locA, &hdr_info) < 0)
        FAIL_STACK_ERROR;

    if (hdr_info.nchunks >= nchunks)
        TEST_ERROR;

    if (H5O_close(&oh_locA, NULL) < 0)
        FAIL_STACK_ERROR;
    if (H5O_close(&oh_locB, NULL) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(file) < 0)
        FAIL_STACK_ERROR;

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5O_close(&oh_locA, NULL);
        H5O_close(&oh_locB, NULL);
        H5Fclose(file);
    }
    H5E_END_TRY

    return FAIL;
} /* end test_cont() */

/*
 *  Verify that object headers are held in the cache until they are linked
 *      to a location in the graph, or assigned an ID.  This is done by
 *      creating an object header, then forcing it out of the cache by creating
 *      local heaps until the object header is evicted from the cache, then
 *      modifying the object header.  The refcount on the object header is
 *      checked as verifying that the object header has remained in the cache.
 */
static herr_t
test_ohdr_cache(char *filename, hid_t fapl)
{
    hid_t               file = H5I_INVALID_HID;               /* File ID */
    hid_t               my_fapl;                              /* FAPL ID */
    H5AC_cache_config_t mdc_config;                           /* Metadata cache configuration info */
    H5F_t              *f = NULL;                             /* File handle */
    H5HL_t             *lheap, *lheap2, *lheap3;              /* Pointer to local heaps */
    haddr_t             lheap_addr, lheap_addr2, lheap_addr3; /* Local heap addresses */
    H5O_loc_t           oh_loc;                               /* Object header location */
    time_t              time_new;                             /* Time value for modification time message */
    unsigned            rc;                                   /* Refcount for object */

    TESTING("object header creation in cache");

    /* Make a copy of the FAPL */
    if ((my_fapl = H5Pcopy(fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Tweak down the size of the metadata cache to only 64K */
    mdc_config.version = H5AC__CURR_CACHE_CONFIG_VERSION;
    if (H5Pget_mdc_config(my_fapl, &mdc_config) < 0)
        FAIL_STACK_ERROR;
    mdc_config.set_initial_size = true;
    mdc_config.initial_size     = 32 * 1024;
    mdc_config.max_size         = 64 * 1024;
    mdc_config.min_size         = 8 * 1024;
    if (H5Pset_mdc_config(my_fapl, &mdc_config) < 0)
        FAIL_STACK_ERROR;

    /* Create the file to operate on */
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(my_fapl) < 0)
        FAIL_STACK_ERROR;
    if (NULL == (f = (H5F_t *)H5VL_object(file)))
        FAIL_STACK_ERROR;
    if (H5AC_ignore_tags(f) < 0)
        FAIL_STACK_ERROR;

    /* Create object (local heap) that occupies most of cache */
    if (H5HL_create(f, (31 * 1024), &lheap_addr) < 0)
        FAIL_STACK_ERROR;

    /* Protect local heap (which actually pins it in the cache) */
    if (NULL == (lheap = H5HL_protect(f, lheap_addr, H5AC__READ_ONLY_FLAG)))
        FAIL_STACK_ERROR;

    /* Create an object header */
    memset(&oh_loc, 0, sizeof(oh_loc));
    if (H5O_create(f, (size_t)2048, (size_t)1, H5P_GROUP_CREATE_DEFAULT, &oh_loc /*out*/) < 0)
        FAIL_STACK_ERROR;

    /* Query object header information */
    rc = 0;
    if (H5O__get_rc_test(&oh_loc, &rc) < 0)
        FAIL_STACK_ERROR;
    if (0 != rc)
        TEST_ERROR;

    /* Create object (local heap) that occupies most of cache */
    if (H5HL_create(f, (31 * 1024), &lheap_addr2) < 0)
        FAIL_STACK_ERROR;

    /* Protect local heap (which actually pins it in the cache) */
    if (NULL == (lheap2 = H5HL_protect(f, lheap_addr2, H5AC__READ_ONLY_FLAG)))
        FAIL_STACK_ERROR;

    /* Unprotect local heap (which actually unpins it from the cache) */
    if (H5HL_unprotect(lheap2) < 0)
        FAIL_STACK_ERROR;

    /* Create object header message in new object header */
    time_new = 11111111;
    if (H5O_msg_create(&oh_loc, H5O_MTIME_NEW_ID, 0, 0, &time_new) < 0)
        FAIL_STACK_ERROR;

    /* Create object (local heap) that occupies most of cache */
    if (H5HL_create(f, (31 * 1024), &lheap_addr3) < 0)
        FAIL_STACK_ERROR;

    /* Protect local heap (which actually pins it in the cache) */
    if (NULL == (lheap3 = H5HL_protect(f, lheap_addr3, H5AC__READ_ONLY_FLAG)))
        FAIL_STACK_ERROR;

    /* Unprotect local heap (which actually unpins it from the cache) */
    if (H5HL_unprotect(lheap3) < 0)
        FAIL_STACK_ERROR;

    /* Query object header information */
    /* (Note that this is somewhat of a weak test, since it doesn't actually
     *  verify that the object header was evicted from the cache, but it's
     *  very difficult to verify when an entry is evicted from the cache in
     *  a non-invasive way -QAK)
     */
    rc = 0;
    if (H5O__get_rc_test(&oh_loc, &rc) < 0)
        FAIL_STACK_ERROR;
    if (0 != rc)
        TEST_ERROR;

    /* Decrement reference count o object header */
    if (H5O_dec_rc_by_loc(&oh_loc) < 0)
        FAIL_STACK_ERROR;

    /* Close object header created */
    if (H5O_close(&oh_loc, NULL) < 0)
        FAIL_STACK_ERROR;

    /* Unprotect local heap (which actually unpins it from the cache) */
    if (H5HL_unprotect(lheap) < 0)
        FAIL_STACK_ERROR;

    if (H5Fclose(file) < 0)
        FAIL_STACK_ERROR;

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Fclose(file);
    }
    H5E_END_TRY

    return FAIL;
} /* test_ohdr_cache() */

/*
 *  To exercise the coding for the re-read of the object header for SWMR access.
 *  When the object header is read in H5O_load() of H5Ocache.c, the library initially reads
 *  H5O_SPEC_READ_SIZE (512, currently)  bytes for decoding, then reads the
 *  remaining bytes later if the object header is greater than H5O_SPEC_READ_SIZE
 *  bytes.  For SWMR access, the read should be done all at one time.
 */
static herr_t
test_ohdr_swmr(bool new_format)
{
    hid_t             fid          = H5I_INVALID_HID; /* File ID */
    hid_t             fapl         = H5I_INVALID_HID; /* File access property list */
    hid_t             did          = H5I_INVALID_HID; /* Dataset ID */
    hid_t             sid          = H5I_INVALID_HID; /* Dataspace ID */
    hid_t             plist        = H5I_INVALID_HID; /* Dataset creation property list */
    size_t            compact_size = 1024;            /* The size of compact dataset */
    int              *wbuf         = NULL;            /* Buffer for writing */
    hsize_t           dims[1];                        /* Dimension sizes */
    size_t            u;                              /* Iterator */
    int               n;                              /* Data variable */
    H5O_native_info_t ninfo;                          /* Information for the object */

    if (new_format) {
        TESTING("exercise the coding for the re-read of the object header for SWMR access: latest-format");
    }
    else {
        TESTING(
            "exercise the coding for the re-read of the object header for SWMR access: non-latest-format");
    } /* end if */

    /* File access property list */
    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        FAIL_STACK_ERROR;

    /* Create the file with/without latest format: ensure version 2 object header for SWMR */
    if (new_format) {
        /* Set to use latest library format */
        if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
            FAIL_STACK_ERROR;

        if ((fid = H5Fcreate(FILE_OHDR_SWMR, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            TEST_ERROR;
    } /* end if */
    else {
        if ((fid = H5Fcreate(FILE_OHDR_SWMR, H5F_ACC_TRUNC | H5F_ACC_SWMR_WRITE, H5P_DEFAULT, fapl)) < 0)
            TEST_ERROR;
    } /* end else */

    /* Initialize data */
    wbuf = (int *)calloc(compact_size, sizeof(int));
    n    = 0;
    for (u = 0; u < compact_size; u++)
        wbuf[u] = n++;

    /* Create a small data space for compact dataset */
    dims[0] = (hsize_t)compact_size;
    if ((sid = H5Screate_simple(1, dims, NULL)) < 0)
        FAIL_STACK_ERROR;

    /* Create property list for compact dataset creation */
    if ((plist = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR;

    /* Set the layout for the compact dataset */
    if (H5Pset_layout(plist, H5D_COMPACT) < 0)
        FAIL_STACK_ERROR;

    /* Create a compact dataset */
    if ((did = H5Dcreate2(fid, DSET_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, plist, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Write to the compact dataset */
    if (H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0)
        FAIL_STACK_ERROR;

    /* Close the dataset */
    if (H5Dclose(did) < 0)
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Open the file for SWMR write with/without latest format */
    if ((fid = H5Fopen(FILE_OHDR_SWMR, H5F_ACC_RDWR | H5F_ACC_SWMR_WRITE, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Open the compact dataset */
    if ((did = H5Dopen2(fid, DSET_NAME, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Get the object information */
    if (H5Oget_native_info(did, &ninfo, H5O_NATIVE_INFO_HDR) < 0)
        FAIL_STACK_ERROR;

    if (new_format)
        if (ninfo.hdr.version != OBJ_VERSION_LATEST)
            FAIL_STACK_ERROR;

    /* The size of object header should be greater than the speculative read size of H5O_SPEC_READ_SIZE */
    /* This will exercise the coding for the re-read of the object header for SWMR access */
    if (ninfo.hdr.space.total < H5O_SPEC_READ_SIZE)
        TEST_ERROR;

    /* Close the dataset */
    if (H5Dclose(did) < 0)
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Close the dataspace */
    if (H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;

    /* Close the dataset creation property list */
    if (H5Pclose(plist) < 0)
        FAIL_STACK_ERROR;

    /* Close the file access property list */
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    /* Remove the test file */
    if (HDremove(FILE_OHDR_SWMR) < 0)
        FAIL_STACK_ERROR;

    /* Free the buffer */
    free(wbuf);

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Fclose(fid);
        H5Dclose(did);
        H5Sclose(sid);
        H5Pclose(plist);
        H5Pclose(fapl);
        HDremove(FILE_OHDR_SWMR);
        free(wbuf);
    }
    H5E_END_TRY

    return FAIL;
} /* test_ohdr_swmr() */

/*
 * Tests bad object header messages.
 *
 * Currently tests for CVE-2020-10810 fixes but can be expanded to handle
 * other CVE badness.
 */

/* This is a generated file that can be obtained from:
 *
 * https://nvd.nist.gov/vuln/detail/CVE-2020-10810
 *
 * It was formerly named H5AC_unpin_entry_POC
 */
#define CVE_2020_10810_FILENAME "cve_2020_10810.h5"

static herr_t
test_ohdr_badness(hid_t fapl)
{
    hid_t fid = H5I_INVALID_HID;

    /* CVE-2020-10810 involved a malformed fsinfo message
     * This test ensures the fundamental problem is fixed. Running it under
     * valgrind et al. will ensure that the memory leaks and invalid access
     * are fixed.
     */
    TESTING("Fix for CVE-2020-10810");

    H5E_BEGIN_TRY
    {
        /* This should fail due to the malformed fsinfo message. It should
         * fail gracefully and not segfault.
         */
        fid = H5Fopen(CVE_2020_10810_FILENAME, H5F_ACC_RDWR, fapl);
    }
    H5E_END_TRY

    if (fid >= 0)
        FAIL_PUTS_ERROR("should not have been able to open malformed file");

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Fclose(fid);
    }
    H5E_END_TRY

    return FAIL;
}

/*
 *  To test objects with unknown messages in a file with:
 *      a) H5O_BOGUS_VALID_ID:
 *       --the bogus_id is within the range of H5O_msg_class_g[]
 *      b) H5O_BOGUS_INVALID_ID:
 *         --the bogus_id is outside the range of H5O_msg_class_g[]
 *
 *   The test file is FILE_BOGUS: "tbogus.h5" generated with gen_bogus.c
 *   --objects that have unknown header messages with H5O_BOGUS_VALID_ID in "/"
 *   --objects that have unknown header messages with H5O_BOGUS_INVALID_ID in "/group"
 *
 *   The test also uses the test file FILENAME[0] (ohdr.h5): the parameter "filename"
 */
static herr_t
test_unknown(unsigned bogus_id, char *filename, hid_t fapl)
{
    hid_t fid       = H5I_INVALID_HID; /* file ID */
    hid_t gid       = H5I_INVALID_HID; /* group ID */
    hid_t did       = H5I_INVALID_HID; /* Dataset ID */
    hid_t sid       = H5I_INVALID_HID; /* Dataspace ID */
    hid_t aid       = H5I_INVALID_HID; /* Attribute ID */
    hid_t loc       = H5I_INVALID_HID; /* location: file or group ID */
    hid_t fid_bogus = H5I_INVALID_HID; /* bogus file ID */
    hid_t gid_bogus = H5I_INVALID_HID; /* bogus group ID */
    hid_t loc_bogus = H5I_INVALID_HID; /* location: bogus file or group ID */
    char  testfile[TESTFILE_LEN];

    /* create a different name for a local copy of the data file to be
       opened with rd/wr file permissions in case build and test are
       done in the source directory. */
    strncpy(testfile, FILE_BOGUS, TESTFILE_LEN);
    testfile[TESTFILE_LEN - 1] = '\0';
    strncat(testfile, ".copy", sizeof(testfile) - strlen(testfile) - 1);

    /* Make a copy of the data file from svn. */
    if (h5_make_local_copy(FILE_BOGUS, testfile) < 0)
        FAIL_STACK_ERROR;

    TESTING("object with unknown header message and no flags set");

    /* Open filename */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Open FILE_BOGUS */
    if ((fid_bogus = H5Fopen(testfile, H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Set up location ID depending on bogus_id */
    if (bogus_id == H5O_BOGUS_INVALID_ID) {
        /* Open "group" in FILE_BOGUS */
        if ((gid_bogus = H5Gopen2(fid_bogus, "group", H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;
        loc_bogus = gid_bogus;

        /* Create "group" in filename */
        if ((gid = H5Gcreate2(fid, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;
        loc = gid;
    }
    else { /* H5O_BOGUS_VALID_ID */
        loc_bogus = fid_bogus;
        loc       = fid;
    } /* end else */

    /* Open the dataset with the unknown header message, but no extra flags */
    if ((did = H5Dopen2(loc_bogus, "Dataset1", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if (H5Dclose(did) < 0)
        FAIL_STACK_ERROR;

    PASSED();

    TESTING("object with unknown header message & 'shareable' flag set");

    /* Open the dataset with the unknown header message, and "shareable" flag */
    if ((did = H5Dopen2(loc_bogus, "Dataset5", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if (H5Dclose(did) < 0)
        FAIL_STACK_ERROR;

    PASSED();

    TESTING("object in r/o file with unknown header message & 'fail if unknown and open for write' flag set");

    /* Open the dataset with the unknown header message, and "fail if unknown and open for write" flag */
    if ((did = H5Dopen2(loc_bogus, "Dataset2", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if (H5Dclose(did) < 0)
        FAIL_STACK_ERROR;

    PASSED();

    TESTING("object in r/o file with unknown header message & 'fail if unknown always' flag set");

    /* Attempt to open the dataset with the unknown header message, and "fail if unknown always" flag */
    H5E_BEGIN_TRY
    {
        did = H5Dopen2(loc_bogus, "Dataset3", H5P_DEFAULT);
    }
    H5E_END_TRY
    if (did >= 0) {
        H5Dclose(did);
        TEST_ERROR;
    } /* end if */

    PASSED();

    TESTING("object with unknown header message & 'mark if unknown' flag set");

    /* Copy object with "mark if unknown" flag on message into file (FILENAME[0]) that can be modified */
    if (H5Ocopy(loc_bogus, "Dataset4", loc, "Dataset4", H5P_DEFAULT, H5P_DEFAULT) < 0)
        FAIL_STACK_ERROR;

    /* Closing: filename */
    if (bogus_id == H5O_BOGUS_INVALID_ID)
        if (H5Gclose(gid) < 0)
            FAIL_STACK_ERROR;
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Re-open filename, with read-only permissions */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Set up location ID depending on bogus_id */
    if (bogus_id == H5O_BOGUS_INVALID_ID) {
        /* Open "group" in filename */
        if ((gid = H5Gopen2(fid, "group", H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;
        loc = gid;
    }
    else
        loc = fid;

    /* Open the dataset with the "mark if unknown" message */
    if ((did = H5Dopen2(loc, "Dataset4", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Check that the "unknown" message was _NOT_ marked */
    if (H5O__check_msg_marked_test(did, false) < 0)
        FAIL_STACK_ERROR;

    /* Close the dataset */
    if (H5Dclose(did) < 0)
        FAIL_STACK_ERROR;

    /* Close "group" in filename depending on bogus_id */
    if (bogus_id == H5O_BOGUS_INVALID_ID)
        if (H5Gclose(gid) < 0)
            FAIL_STACK_ERROR;

    /* Close filename (to flush change to object header) */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Re-open filename */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Set up location ID depending on bogus_id */
    if (bogus_id == H5O_BOGUS_INVALID_ID) {
        /* Open "group" in filename */
        if ((gid = H5Gopen2(fid, "group", H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;
        loc = gid;
    }
    else
        loc = fid;

    /* Open the dataset with the "mark if unknown" message */
    if ((did = H5Dopen2(loc, "Dataset4", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Create data space */
    if ((sid = H5Screate(H5S_SCALAR)) < 0)
        FAIL_STACK_ERROR;

    /* Create an attribute, to get the object header into write access */
    if ((aid = H5Acreate2(did, "Attr", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Close dataspace */
    if (H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;

    /* Close attribute */
    if (H5Aclose(aid) < 0)
        FAIL_STACK_ERROR;

    /* Close the dataset */
    if (H5Dclose(did) < 0)
        FAIL_STACK_ERROR;

    /* Close "group" in filename depending on bogus_id */
    if (bogus_id == H5O_BOGUS_INVALID_ID)
        if (H5Gclose(gid) < 0)
            FAIL_STACK_ERROR;

    /* Close filename (to flush change to object header) */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Re-open filename */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Set up location ID depending on bogus_id */
    if (bogus_id == H5O_BOGUS_INVALID_ID) {
        /* Open "group" in filename */
        if ((gid = H5Gopen2(fid, "group", H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;
        loc = gid;
    }
    else
        loc = fid;

    /* Re-open the dataset with the "mark if unknown" message */
    if ((did = H5Dopen2(loc, "Dataset4", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Check that the "unknown" message was marked */
    if (H5O__check_msg_marked_test(did, true) < 0)
        FAIL_STACK_ERROR;

    /* Close the dataset */
    if (H5Dclose(did) < 0)
        FAIL_STACK_ERROR;

    /* Closing: filename */
    if (bogus_id == H5O_BOGUS_INVALID_ID)
        if (H5Gclose(gid) < 0)
            FAIL_STACK_ERROR;
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    PASSED();

    /* Closing: FILE_BOGUS */
    if (bogus_id == H5O_BOGUS_INVALID_ID)
        if (H5Gclose(gid_bogus) < 0)
            FAIL_STACK_ERROR;
    if (H5Fclose(fid_bogus) < 0)
        FAIL_STACK_ERROR;

    TESTING("object in r/w file with unknown header message & 'fail if unknown and open for write' flag set");

    /* Open FILE_BOGUS with RW intent this time */
    if ((fid_bogus = H5Fopen(testfile, H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Set up location ID */
    if (bogus_id == H5O_BOGUS_INVALID_ID) {
        /* Open "group" in FILE_BOGUS */
        if ((gid_bogus = H5Gopen2(fid_bogus, "group", H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;
        loc_bogus = gid_bogus;
    }
    else
        loc_bogus = fid_bogus;

    /* Attempt to open the dataset with the unknown header message, and "fail if unknown and open for write"
     * flag */
    H5E_BEGIN_TRY
    {
        did = H5Dopen2(loc_bogus, "Dataset2", H5P_DEFAULT);
    }
    H5E_END_TRY
    if (did >= 0) {
        H5Dclose(did);
        TEST_ERROR;
    } /* end if */

    PASSED();

    TESTING("object in r/w file with unknown header message & 'fail if unknown always' flag set");

    /* Attempt to open the dataset with the unknown header message, and "fail if unknown always" flag */
    H5E_BEGIN_TRY
    {
        did = H5Dopen2(loc_bogus, "Dataset3", H5P_DEFAULT);
    }
    H5E_END_TRY
    if (did >= 0) {
        H5Dclose(did);
        TEST_ERROR;
    } /* end if */

    /* Closing: FILE_BOGUS */
    if (bogus_id == H5O_BOGUS_INVALID_ID)
        if (H5Gclose(gid_bogus) < 0)
            FAIL_STACK_ERROR;
    if (H5Fclose(fid_bogus) < 0)
        FAIL_STACK_ERROR;

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Fclose(fid);
        H5Gclose(gid);
        H5Fclose(fid_bogus);
        H5Gclose(gid_bogus);
        H5Dclose(did);
        H5Sclose(sid);
        H5Aclose(aid);
    }
    H5E_END_TRY

    return FAIL;
} /* test_unknown() */

/*
 * Count the number of attributes attached to an object.
 * Returns negative in event of error.
 */
static int
count_attributes(hid_t dset_id)
{
    H5O_info2_t info;

    if (H5Oget_info3(dset_id, &info, H5O_INFO_NUM_ATTRS) < 0)
        return -1;
    else
        return (int)info.num_attrs; /* should never exceed int bounds */
} /* count_attributes */

/*
 * Get the total space used by the object header.
 * Used by oh_compare()
 * On success, stores size in `size_out` pointer.
 */
static herr_t
oh_getsize(hid_t did, hsize_t *size_out)
{
    H5O_native_info_t ninfo;

    if (FAIL == H5Oget_native_info(did, &ninfo, H5O_NATIVE_INFO_HDR))
        return FAIL;

    *size_out = ninfo.hdr.space.total;

    return SUCCEED;
} /* _oh_getsize */

/*
 * Compare the TOTAL space used by datasets' object headers.
 * Returns negative value if an error occurred,
 * else positive #defined indicator value EQ, LT, GT.
 */
static int
oh_compare(hid_t did1, hid_t did2)
{
    hsize_t space1 = 0;
    hsize_t space2 = 0;

    if (FAIL == oh_getsize(did1, &space1))
        return -1;
    if (FAIL == oh_getsize(did2, &space2))
        return -2;

    if (space1 < space2)
        return LT;
    else if (space1 > space2)
        return GT;
    else
        return EQ;
} /* oh_compare() */

/*
 * Demonstrate attribute addition to datasets.
 * Conduct additions side-by-side with a standard datataset and one with
 * minimized dataset object headers.
 */
#define ATTR_NAME_MAX 64
#define ATTR_SHORT    "first"
#define ATTR_LONG     "second"
#define N_ATTRS       64
static herr_t
test_minimized_dset_ohdr_attribute_addition(hid_t fapl_id)
{
    hsize_t dims[1]                  = {0}; /* dataspace extent */
    char    filename[512]            = "";
    char    attr_name[ATTR_NAME_MAX] = "";
    hid_t   fid                      = H5I_INVALID_HID;
    hid_t   dcpl_id                  = H5I_INVALID_HID;
    hid_t   sid                      = H5I_INVALID_HID;
    hid_t   did                      = H5I_INVALID_HID;
    hid_t   aid                      = H5I_INVALID_HID;
    char   *in_buf                   = NULL;
    char   *out_buf                  = NULL;
    size_t  buf_size                 = 0;
    int     out_val                  = 0;
    int     in_val                   = 0;
    int     i;

    TESTING("adding attributes to datasets created with H5Pset_dset_no_attrs_hint()");

    /* Create the test file */
    if (NULL == h5_fixname(FILENAME[1], fapl_id, filename, sizeof(filename)))
        TEST_ERROR;
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) == H5I_INVALID_HID)
        TEST_ERROR;

    /* Set the 'no attrs' hint on the dcpl */
    if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) == H5I_INVALID_HID)
        TEST_ERROR;
    if (H5Pset_dset_no_attrs_hint(dcpl_id, true) < 0)
        TEST_ERROR;

    /* The dataset doesn't need to contain data */
    dims[0] = 0;
    if ((sid = H5Screate_simple(1, dims, NULL)) == H5I_INVALID_HID)
        TEST_ERROR;

    /* Create the dataset */
    if ((did = H5Dcreate2(fid, "H5Pset_dset_no_attrs_hint", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl_id,
                          H5P_DEFAULT)) == H5I_INVALID_HID)
        TEST_ERROR;

    /* Close */
    if (H5Pclose(dcpl_id) < 0)
        TEST_ERROR;
    if (H5Sclose(sid) < 0)
        TEST_ERROR;

    /**********************************************
     * ADD A (STRING) ATTRIBUTE AND MANIPULATE IT *
     **********************************************/

    buf_size = strlen(ATTR_LONG) + 1;
    if (NULL == (in_buf = (char *)calloc(buf_size, sizeof(char))))
        TEST_ERROR;
    if (NULL == (out_buf = (char *)calloc(buf_size, sizeof(char))))
        TEST_ERROR;

    /* Create a string attribute on the dataset
     *
     * It has to be long enough to hold the longest string we're going to write
     * to it.
     */
    dims[0] = buf_size;
    if ((sid = H5Screate_simple(1, dims, NULL)) == H5I_INVALID_HID)
        TEST_ERROR;
    if ((aid = H5Acreate2(did, "string_attr", H5T_NATIVE_CHAR, sid, H5P_DEFAULT, H5P_DEFAULT)) ==
        H5I_INVALID_HID)
        TEST_ERROR;

    /* Write attribute data */
    strcpy(in_buf, ATTR_SHORT);
    if (H5Awrite(aid, H5T_NATIVE_CHAR, in_buf) < 0)
        TEST_ERROR;

    /* Make sure the count is correct */
    if (count_attributes(did) != 1)
        TEST_ERROR;

    /* Read the data back and verify */
    if (H5Aread(aid, H5T_NATIVE_CHAR, out_buf) < 0)
        TEST_ERROR;
    if (strcmp(in_buf, out_buf) != 0)
        TEST_ERROR;

    /* modify the string attribute */
    memset(in_buf, 0, buf_size);
    strcpy(in_buf, ATTR_LONG);
    if (H5Awrite(aid, H5T_NATIVE_CHAR, in_buf) < 0)
        TEST_ERROR;

    if (count_attributes(did) != 1)
        TEST_ERROR;

    /* Read the data back and verify */
    if (H5Aread(aid, H5T_NATIVE_CHAR, out_buf) < 0)
        TEST_ERROR;
    if (strcmp(in_buf, out_buf) != 0)
        TEST_ERROR;

    /* Close */
    if (H5Sclose(sid) < 0)
        TEST_ERROR;
    if (H5Aclose(aid) < 0)
        TEST_ERROR;

    /***************************************
     * ADD A BUNCH OF (INTEGER) ATTRIBUTES *
     ***************************************/

    if ((sid = H5Screate(H5S_SCALAR)) == H5I_INVALID_HID)
        TEST_ERROR;

    /* Loop over a reasonable number of attributes */
    for (i = 0; i < N_ATTRS; i++) {

        /* Set the attribute's name */
        if (snprintf(attr_name, ATTR_NAME_MAX, "int_attr_%d", i) < 0)
            TEST_ERROR;

        /* Create an integer attribute on the dataset */
        if ((aid = H5Acreate2(did, attr_name, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT)) ==
            H5I_INVALID_HID)
            TEST_ERROR;

        /* Write attribute data */
        in_val = i;
        if (H5Awrite(aid, H5T_NATIVE_INT, &in_val) < 0)
            TEST_ERROR;

        /* Make sure the count is correct (already has one attribute) */
        if (count_attributes(did) != i + 2)
            TEST_ERROR;

        /* Read the data back and verify */
        if (H5Aread(aid, H5T_NATIVE_INT, &out_val) < 0)
            TEST_ERROR;
        if (in_val != out_val)
            TEST_ERROR;

        /* Close */
        if (H5Aclose(aid) < 0)
            TEST_ERROR;
    }

    /* Close */
    if (H5Sclose(sid) < 0)
        TEST_ERROR;

    /* Close the remaining IDs */
    if (H5Dclose(did) < 0)
        TEST_ERROR;
    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    /* Free memory */
    free(in_buf);
    free(out_buf);

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        (void)H5Pclose(dcpl_id);
        (void)H5Sclose(sid);
        (void)H5Dclose(did);
        (void)H5Aclose(aid);
        (void)H5Fclose(fid);
    }
    H5E_END_TRY

    free(in_buf);
    free(out_buf);

    return FAIL;
} /* test_minimized_dset_ohdr_attribute_addition */

/*
 * Compare header sizes against when headers have been minimized.
 * Repeats tests with headers "compact" and normal.
 */
static herr_t
test_minimized_dset_ohdr_size_comparisons(hid_t fapl_id)
{
    hsize_t  array_10[1] = {10}; /* dataspace extents */
    unsigned compact     = 0;

    /* IDs that are file-agnostic */
    hid_t dspace_id     = H5I_INVALID_HID;
    hid_t int_type_id   = H5I_INVALID_HID;
    hid_t dcpl_minimize = H5I_INVALID_HID;
    hid_t dcpl_dontmin  = H5I_INVALID_HID;
    hid_t dcpl_default  = H5I_INVALID_HID;

    /* IDs for non-minimized file open */
    hid_t file_f_id   = H5I_INVALID_HID; /* lower 'f' for standard file setting */
    hid_t dset_f_x_id = H5I_INVALID_HID; /* 'x' for default */
    hid_t dset_f_N_id = H5I_INVALID_HID; /* 'N' for explicit non-minimized dset */
    hid_t dset_f_Y_id = H5I_INVALID_HID; /* 'Y' for minimized dset */

    /* IDs for minimized file open */
    hid_t file_F_id   = H5I_INVALID_HID; /* upper 'F' for minimized file setting */
    hid_t dset_F_x_id = H5I_INVALID_HID; /* 'x' for default */
    hid_t dset_F_N_id = H5I_INVALID_HID; /* 'N' for explicit non-minimized dset */
    hid_t dset_F_Y_id = H5I_INVALID_HID; /* 'Y' for minimized dset */

    char filename_a[512] = "";
    char filename_b[512] = "";

    herr_t ret;

    /* dataset suffixes:
     *                | default | minimize | don't minimize (dcpl-set)
     * ---------------+---------+----------+---------------
     * file-default   |   f_x   |   f_Y    |   f_N
     * ---------------+---------+----------+---------------
     * file-minimized |   F_x   |   F_Y    |   F_N
     */

    /*********
     * SETUP *
     *********/

    /* Set filenames (not in a test, can't use TEST_ERROR) */
    if (h5_fixname(FILENAME[1], fapl_id, filename_a, sizeof(filename_a)) == NULL)
        return FAIL;
    if (h5_fixname(FILENAME[2], fapl_id, filename_b, sizeof(filename_b)) == NULL)
        return FAIL;

    for (compact = 0; compact < 2; compact++) { /* 0 or 1 */

        if (compact)
            TESTING("minimized dset object headers size comparisons (compact)");
        else
            TESTING("minimized dset object headers size comparisons");

        dcpl_default = H5Pcreate(H5P_DATASET_CREATE);
        if (dcpl_default < 0)
            TEST_ERROR;

        dcpl_minimize = H5Pcreate(H5P_DATASET_CREATE);
        if (dcpl_minimize < 0)
            TEST_ERROR;
        ret = H5Pset_dset_no_attrs_hint(dcpl_minimize, true);
        if (ret < 0)
            TEST_ERROR;

        dcpl_dontmin = H5Pcreate(H5P_DATASET_CREATE);
        if (dcpl_dontmin < 0)
            TEST_ERROR;
        ret = H5Pset_dset_no_attrs_hint(dcpl_dontmin, false);
        if (ret < 0)
            TEST_ERROR;

        if (compact) {
            ret = H5Pset_layout(dcpl_default, H5D_COMPACT);
            if (ret < 0)
                TEST_ERROR;
            ret = H5Pset_layout(dcpl_minimize, H5D_COMPACT);
            if (ret < 0)
                TEST_ERROR;
            ret = H5Pset_layout(dcpl_dontmin, H5D_COMPACT);
            if (ret < 0)
                TEST_ERROR;
        }

        dspace_id = H5Screate_simple(1, array_10, NULL);
        if (dspace_id < 0)
            TEST_ERROR;

        int_type_id = H5Tcopy(H5T_NATIVE_INT);
        if (int_type_id < 0)
            TEST_ERROR;

        file_f_id = H5Fcreate(filename_a, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
        if (file_f_id < 0)
            TEST_ERROR;

        dset_f_x_id =
            H5Dcreate2(file_f_id, "default", int_type_id, dspace_id, H5P_DEFAULT, dcpl_default, H5P_DEFAULT);
        if (dset_f_x_id < 0)
            TEST_ERROR;

        dset_f_N_id =
            H5Dcreate2(file_f_id, "dsetNOT", int_type_id, dspace_id, H5P_DEFAULT, dcpl_dontmin, H5P_DEFAULT);
        if (dset_f_N_id < 0)
            TEST_ERROR;

        dset_f_Y_id =
            H5Dcreate2(file_f_id, "dsetMIN", int_type_id, dspace_id, H5P_DEFAULT, dcpl_minimize, H5P_DEFAULT);
        if (dset_f_x_id < 0)
            TEST_ERROR;

        file_F_id = H5Fcreate(filename_b, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
        if (file_F_id < 0)
            TEST_ERROR;
        ret = H5Fset_dset_no_attrs_hint(file_F_id, true);
        if (ret < 0)
            TEST_ERROR;

        dset_F_x_id =
            H5Dcreate2(file_F_id, "default", int_type_id, dspace_id, H5P_DEFAULT, dcpl_default, H5P_DEFAULT);
        if (dset_F_x_id < 0)
            TEST_ERROR;

        dset_F_N_id =
            H5Dcreate2(file_F_id, "dsetNOT", int_type_id, dspace_id, H5P_DEFAULT, dcpl_dontmin, H5P_DEFAULT);
        if (dset_F_N_id < 0)
            TEST_ERROR;

        dset_F_Y_id =
            H5Dcreate2(file_F_id, "dsetMIN", int_type_id, dspace_id, H5P_DEFAULT, dcpl_minimize, H5P_DEFAULT);
        if (dset_F_Y_id < 0)
            TEST_ERROR;

        /*********
         * TESTS *
         *********/

        /* Identity */
        if (oh_compare(dset_f_x_id, dset_f_x_id) != EQ)
            TEST_ERROR;

        if (oh_compare(dset_f_x_id, dset_f_N_id) != EQ)
            TEST_ERROR;
        if (oh_compare(dset_f_x_id, dset_f_Y_id) != GT)
            TEST_ERROR;
        if (oh_compare(dset_f_N_id, dset_f_Y_id) != GT)
            TEST_ERROR;

        if (oh_compare(dset_F_x_id, dset_F_N_id) != EQ)
            TEST_ERROR;
        if (oh_compare(dset_F_x_id, dset_F_Y_id) != EQ)
            TEST_ERROR;
        if (oh_compare(dset_F_N_id, dset_F_Y_id) != EQ)
            TEST_ERROR;

        if (oh_compare(dset_F_x_id, dset_f_Y_id) != EQ)
            TEST_ERROR;
        if (oh_compare(dset_F_x_id, dset_f_x_id) != LT)
            TEST_ERROR;

        /************
         * TEARDOWN *
         ************/

        if (H5Sclose(dspace_id) < 0)
            TEST_ERROR;
        if (H5Tclose(int_type_id) < 0)
            TEST_ERROR;
        if (H5Pclose(dcpl_default) < 0)
            TEST_ERROR;
        if (H5Pclose(dcpl_minimize) < 0)
            TEST_ERROR;
        if (H5Pclose(dcpl_dontmin) < 0)
            TEST_ERROR;

        if (H5Fclose(file_f_id) < 0)
            TEST_ERROR;
        if (H5Dclose(dset_f_x_id) < 0)
            TEST_ERROR;
        if (H5Dclose(dset_f_N_id) < 0)
            TEST_ERROR;
        if (H5Dclose(dset_f_Y_id) < 0)
            TEST_ERROR;

        if (H5Fclose(file_F_id) < 0)
            TEST_ERROR;
        if (H5Dclose(dset_F_x_id) < 0)
            TEST_ERROR;
        if (H5Dclose(dset_F_N_id) < 0)
            TEST_ERROR;
        if (H5Dclose(dset_F_Y_id) < 0)
            TEST_ERROR;

        PASSED();

    } /* compact and non-compact */

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        (void)H5Pclose(dcpl_default);
        (void)H5Pclose(dcpl_minimize);
        (void)H5Pclose(dcpl_dontmin);
        (void)H5Sclose(dspace_id);
        (void)H5Tclose(int_type_id);

        (void)H5Fclose(file_f_id);
        (void)H5Dclose(dset_f_x_id);
        (void)H5Dclose(dset_f_N_id);
        (void)H5Dclose(dset_f_Y_id);

        (void)H5Fclose(file_F_id);
        (void)H5Dclose(dset_F_x_id);
        (void)H5Dclose(dset_F_N_id);
        (void)H5Dclose(dset_F_Y_id);
    }
    H5E_END_TRY
    return FAIL;
} /* test_minimized_dset_ohdr_size_comparisons */

/*
 * Test minimized dataset object header with filter/pipeline message
 */
static herr_t
test_minimized_dset_ohdr_with_filter(hid_t fapl_id)
{
    char           filename[512]   = "";
    const hsize_t  extents[1]      = {1024}; /* extents of dataspace */
    const unsigned filter_values[] = {0};    /* TBD */
    const hsize_t  chunk_dim[]     = {32};   /* needed for filter */
    const int      ndims           = 1;
    hid_t          dspace_id       = H5I_INVALID_HID;
    hid_t          dtype_id        = H5I_INVALID_HID;
    hid_t          dcpl_xZ_id      = H5I_INVALID_HID;
    hid_t          dcpl_mx_id      = H5I_INVALID_HID;
    hid_t          dcpl_mZ_id      = H5I_INVALID_HID;
    hid_t          dset_xx_id      = H5I_INVALID_HID;
    hid_t          dset_xZ_id      = H5I_INVALID_HID;
    hid_t          dset_mx_id      = H5I_INVALID_HID;
    hid_t          dset_mZ_id      = H5I_INVALID_HID;
    hid_t          file_id         = H5I_INVALID_HID;
    herr_t         ret;

    /* dcpl suffixes:
     *           | default | minimize
     * ----------+---------+---------
     * no filter |    xx   |   mx
     * ----------+---------+---------
     * filter    |    xZ   |   mZ
     */

    TESTING("minimized dset object headers with filter message");

    /*********
     * SETUP *
     *********/

    if (h5_fixname(FILENAME[1], fapl_id, filename, sizeof(filename)) == NULL)
        TEST_ERROR;

    dcpl_mx_id = H5Pcreate(H5P_DATASET_CREATE);
    if (dcpl_mx_id < 0)
        TEST_ERROR;
    ret = H5Pset_dset_no_attrs_hint(dcpl_mx_id, true);
    if (ret < 0)
        TEST_ERROR;

    dcpl_xZ_id = H5Pcreate(H5P_DATASET_CREATE);
    if (dcpl_xZ_id < 0)
        TEST_ERROR;
    ret = H5Pset_chunk(dcpl_xZ_id, ndims, chunk_dim);
    if (ret < 0)
        TEST_ERROR;
    ret = H5Pset_filter(dcpl_xZ_id, H5Z_FILTER_DEFLATE, H5Z_FLAG_OPTIONAL, 0, filter_values);
    if (ret < 0)
        TEST_ERROR;
    dcpl_mZ_id = H5Pcreate(H5P_DATASET_CREATE);
    if (dcpl_mZ_id < 0)
        TEST_ERROR;
    ret = H5Pset_dset_no_attrs_hint(dcpl_mZ_id, true);
    if (ret < 0)
        TEST_ERROR;
    ret = H5Pset_chunk(dcpl_mZ_id, ndims, chunk_dim);
    if (ret < 0)
        TEST_ERROR;
    ret = H5Pset_filter(dcpl_mZ_id, H5Z_FILTER_DEFLATE, H5Z_FLAG_OPTIONAL, 0, filter_values);
    if (ret < 0)
        TEST_ERROR;

    dspace_id = H5Screate_simple(1, extents, extents);
    if (dspace_id < 0)
        TEST_ERROR;

    dtype_id = H5Tcopy(H5T_NATIVE_INT);
    if (dtype_id < 0)
        TEST_ERROR;

    file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    if (file_id < 0)
        TEST_ERROR;

    dset_xx_id = H5Dcreate2(file_id, "xx", dtype_id, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (dset_xx_id < 0)
        TEST_ERROR;

    dset_mx_id = H5Dcreate2(file_id, "Mx", dtype_id, dspace_id, H5P_DEFAULT, dcpl_mx_id, H5P_DEFAULT);
    if (dset_mx_id < 0)
        TEST_ERROR;

    dset_xZ_id = H5Dcreate2(file_id, "xZ", dtype_id, dspace_id, H5P_DEFAULT, dcpl_xZ_id, H5P_DEFAULT);
    if (dset_xZ_id < 0)
        TEST_ERROR;

    dset_mZ_id = H5Dcreate2(file_id, "MZ", dtype_id, dspace_id, H5P_DEFAULT, dcpl_mZ_id, H5P_DEFAULT);
    if (dset_mZ_id < 0)
        TEST_ERROR;

    /*********
     * TESTS *
     *********/

    if (oh_compare(dset_mx_id, dset_xx_id) != LT)
        TEST_ERROR;
    if (oh_compare(dset_mx_id, dset_xZ_id) != LT)
        TEST_ERROR;
    if (oh_compare(dset_mZ_id, dset_mx_id) != GT)
        TEST_ERROR;
    if (oh_compare(dset_mZ_id, dset_xZ_id) != LT)
        TEST_ERROR;

    /************
     * TEARDOWN *
     ************/

    if (H5Sclose(dspace_id) < 0)
        TEST_ERROR;
    if (H5Tclose(dtype_id) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl_xZ_id) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl_mx_id) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl_mZ_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_xx_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_xZ_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_mx_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_mZ_id) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        (void)H5Sclose(dspace_id);
        (void)H5Tclose(dtype_id);
        (void)H5Pclose(dcpl_xZ_id);
        (void)H5Pclose(dcpl_mx_id);
        (void)H5Pclose(dcpl_mZ_id);
        (void)H5Dclose(dset_xx_id);
        (void)H5Dclose(dset_xZ_id);
        (void)H5Dclose(dset_mx_id);
        (void)H5Dclose(dset_mZ_id);
        (void)H5Fclose(file_id);
    }
    H5E_END_TRY
    return FAIL;
} /* test_minimized_dset_ohdr_with_filter */

/*
 * Test minimized dataset object header and recording modification times.
 */
static herr_t
test_minimized_dset_ohdr_modification_times(hid_t _fapl_id)
{
    /* test-local structure for parameterized testing
     */
    struct testcase {
        unsigned oh_version;
    };

    char          filename[512] = "";
    const hsize_t extents[1]    = {128}; /* extents of dataspace */
    hid_t         dspace_id     = H5I_INVALID_HID;
    hid_t         dtype_id      = H5I_INVALID_HID;
    hid_t         dcpl_xT_id    = H5I_INVALID_HID; /* Track modtime */
    hid_t         dcpl_mx_id    = H5I_INVALID_HID; /* minimized */
    hid_t         dcpl_mT_id    = H5I_INVALID_HID; /* minimized, Track */
    hid_t         dcpl_mN_id    = H5I_INVALID_HID; /* minimized, do Not track */
    hid_t         dset_xx_id    = H5I_INVALID_HID;
    hid_t         dset_xT_id    = H5I_INVALID_HID;
    hid_t         dset_mx_id    = H5I_INVALID_HID;
    hid_t         dset_mT_id    = H5I_INVALID_HID;
    hid_t         dset_mN_id    = H5I_INVALID_HID;
    hid_t         file_id       = H5I_INVALID_HID;
    hid_t         fapl_id       = H5I_INVALID_HID;
    herr_t        ret;

    unsigned        i        = 0; /* for testcase loop */
    unsigned        n_cases  = 2; /* must match `cases` array size below */
    struct testcase cases[2] = {
        {
            1,
        }, /* version 1 object header */
        {
            2,
        }, /* version 2 object header */
    };

    /* dcpl suffixes:
     *             | default | minimize
     * ------------+---------+---------
     * default     |    xx   |   mx
     * ------------+---------+---------
     * don't track |    xN   |   mN
     * ------------+---------+---------
     * track       |    xT   |   mT
     */

    TESTING("minimized dset object headers with modification times");

    /*********
     * SETUP *
     *********/

    if (h5_fixname(FILENAME[1], _fapl_id, filename, sizeof(filename)) == NULL)
        TEST_ERROR;

    dcpl_mx_id = H5Pcreate(H5P_DATASET_CREATE);
    if (dcpl_mx_id < 0)
        TEST_ERROR;
    ret = H5Pset_dset_no_attrs_hint(dcpl_mx_id, true);
    if (ret < 0)
        TEST_ERROR;

    dcpl_xT_id = H5Pcreate(H5P_DATASET_CREATE);
    if (dcpl_xT_id < 0)
        TEST_ERROR;
    ret = H5Pset_obj_track_times(dcpl_xT_id, true);
    if (ret < 0)
        TEST_ERROR;

    dcpl_mT_id = H5Pcreate(H5P_DATASET_CREATE);
    if (dcpl_mT_id < 0)
        TEST_ERROR;
    ret = H5Pset_dset_no_attrs_hint(dcpl_mT_id, true);
    if (ret < 0)
        TEST_ERROR;
    ret = H5Pset_obj_track_times(dcpl_mT_id, true);
    if (ret < 0)
        TEST_ERROR;

    dcpl_mN_id = H5Pcreate(H5P_DATASET_CREATE);
    if (dcpl_mN_id < 0)
        TEST_ERROR;
    ret = H5Pset_dset_no_attrs_hint(dcpl_mN_id, true);
    if (ret < 0)
        TEST_ERROR;
    ret = H5Pset_obj_track_times(dcpl_mN_id, false);
    if (ret < 0)
        TEST_ERROR;

    dspace_id = H5Screate_simple(1, extents, extents);
    if (dspace_id < 0)
        TEST_ERROR;

    dtype_id = H5Tcopy(H5T_NATIVE_INT);
    if (dtype_id < 0)
        TEST_ERROR;

    for (i = 0; i < n_cases; i++) {

        /* -------------- *
         * per-case setup *
         * -------------- */

        fapl_id = H5Pcopy(_fapl_id);
        if (fapl_id < 0)
            TEST_ERROR;

        if (cases[i].oh_version > 1) {
            ret = H5Pset_libver_bounds(fapl_id, H5F_LIBVER_V18, H5F_LIBVER_V110);
            if (ret < 0)
                TEST_ERROR;
        }

        file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
        if (file_id < 0)
            TEST_ERROR;

        dset_xx_id = H5Dcreate2(file_id, "xx", dtype_id, dspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        if (dset_xx_id < 0)
            TEST_ERROR;

        dset_mx_id = H5Dcreate2(file_id, "mx", dtype_id, dspace_id, H5P_DEFAULT, dcpl_mx_id, H5P_DEFAULT);
        if (dset_mx_id < 0)
            TEST_ERROR;

        dset_xT_id = H5Dcreate2(file_id, "xT", dtype_id, dspace_id, H5P_DEFAULT, dcpl_xT_id, H5P_DEFAULT);
        if (dset_xT_id < 0)
            TEST_ERROR;
        dset_mT_id = H5Dcreate2(file_id, "mT", dtype_id, dspace_id, H5P_DEFAULT, dcpl_mT_id, H5P_DEFAULT);
        if (dset_mT_id < 0)
            TEST_ERROR;

        dset_mN_id = H5Dcreate2(file_id, "mN", dtype_id, dspace_id, H5P_DEFAULT, dcpl_mN_id, H5P_DEFAULT);
        if (dset_mN_id < 0)
            TEST_ERROR;

        /* ----- *
         * TESTS *
         * ----- */

        /* sanity check */
        if (oh_compare(dset_mx_id, dset_xx_id) != LT)
            TEST_ERROR;
        if (oh_compare(dset_mx_id, dset_xT_id) != LT)
            TEST_ERROR;

        if (oh_compare(dset_xx_id, dset_xT_id) != EQ)
            TEST_ERROR;
        if (oh_compare(dset_mx_id, dset_mT_id) != EQ)
            TEST_ERROR;
        if (oh_compare(dset_mN_id, dset_mT_id) != LT)
            TEST_ERROR;

        if (oh_compare(dset_mT_id, dset_xT_id) != LT)
            TEST_ERROR;

        /* ----------------- *
         * per-case teardown *
         * ----------------- */

        if (H5Dclose(dset_xx_id) < 0)
            TEST_ERROR;
        if (H5Dclose(dset_xT_id) < 0)
            TEST_ERROR;
        if (H5Dclose(dset_mx_id) < 0)
            TEST_ERROR;
        if (H5Dclose(dset_mT_id) < 0)
            TEST_ERROR;
        if (H5Dclose(dset_mN_id) < 0)
            TEST_ERROR;
        if (H5Fclose(file_id) < 0)
            TEST_ERROR;
        if (H5Pclose(fapl_id) < 0)
            TEST_ERROR;

    } /* for each version tested */

    /************
     * TEARDOWN *
     ************/

    if (H5Sclose(dspace_id) < 0)
        TEST_ERROR;
    if (H5Tclose(dtype_id) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl_xT_id) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl_mx_id) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl_mT_id) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl_mN_id) < 0)
        TEST_ERROR;

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        (void)H5Sclose(dspace_id);
        (void)H5Tclose(dtype_id);
        (void)H5Pclose(dcpl_xT_id);
        (void)H5Pclose(dcpl_mx_id);
        (void)H5Pclose(dcpl_mT_id);
        (void)H5Pclose(dcpl_mN_id);
        (void)H5Dclose(dset_xx_id);
        (void)H5Dclose(dset_xT_id);
        (void)H5Dclose(dset_mx_id);
        (void)H5Dclose(dset_mT_id);
        (void)H5Dclose(dset_mN_id);
        (void)H5Fclose(file_id);
        (void)H5Pclose(fapl_id);
    }
    H5E_END_TRY
    return FAIL;
} /* test_minimized_dset_ohdr_modification_times */

/*
 * Test minimized dataset object header with a fill value set.
 */
static herr_t
test_minimized_dset_ohdr_fillvalue_backwards_compatability(hid_t _fapl_id)
{
    char          filename[512] = "";
    const hsize_t extents[1]    = {64};  /* extents of dataspace */
    const int     fill[1]       = {343}; /* fill value of dataset */
    hid_t         file_id       = H5I_INVALID_HID;
    hid_t         dtype_id      = H5I_INVALID_HID;
    hid_t         dspace_id     = H5I_INVALID_HID;
    hid_t         dcpl_id       = H5I_INVALID_HID;
    hid_t         fapl_id       = H5I_INVALID_HID;
    hid_t         dset_0_id     = H5I_INVALID_HID;
    hid_t         dset_1_id     = H5I_INVALID_HID;
    herr_t        ret;

    /*********
     * SETUP *
     *********/

    TESTING("minimized dset object headers with fill values and different libver support");

    fapl_id = H5Pcopy(_fapl_id);
    if (fapl_id < 0)
        TEST_ERROR;

    if (h5_fixname(FILENAME[1], fapl_id, filename, sizeof(filename)) == NULL)
        TEST_ERROR;

    dspace_id = H5Screate_simple(1, extents, extents);
    if (dspace_id < 0)
        TEST_ERROR;

    dtype_id = H5Tcopy(H5T_NATIVE_INT);
    if (dtype_id < 0)
        TEST_ERROR;

    dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
    if (dcpl_id < 0)
        TEST_ERROR;

    ret = H5Pset_dset_no_attrs_hint(dcpl_id, true);
    if (ret == FAIL)
        TEST_ERROR;

    ret = H5Pset_fill_value(dcpl_id, dtype_id, fill);
    if (ret == FAIL)
        TEST_ERROR;

    ret = H5Pset_libver_bounds(fapl_id, H5F_LIBVER_EARLIEST, H5F_LIBVER_LATEST);
    if (ret == FAIL)
        TEST_ERROR;

    file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);
    if (file_id < 0)
        TEST_ERROR;

    dset_0_id = H5Dcreate2(file_id, "fullrange", dtype_id, dspace_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
    if (dset_0_id < 0)
        TEST_ERROR;

    /* Close file and re-open with different libver bounds.
     * Dataset "fullrange" must also be closed for expected reopen behavior.
     */
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_0_id) < 0)
        TEST_ERROR;

    ret = H5Pset_libver_bounds(fapl_id, H5F_LIBVER_V18, H5F_LIBVER_LATEST);
    if (ret == FAIL)
        TEST_ERROR;

    file_id = H5Fopen(filename, H5F_ACC_RDWR, fapl_id);
    if (file_id < 0)
        TEST_ERROR;

    dset_1_id = H5Dcreate2(file_id, "upperrange", dtype_id, dspace_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
    if (dset_1_id < 0)
        TEST_ERROR;

    /* re-open "fullrange" dataset
     */
    dset_0_id = H5Dopen2(file_id, "fullrange", H5P_DEFAULT);
    if (dset_0_id < 0)
        TEST_ERROR;

    /*********
     * TESTS *
     *********/

    /* dset not supporting pre-1.08 should be smaller? */
    if (oh_compare(dset_1_id, dset_0_id) != LT)
        TEST_ERROR;

    /************
     * TEARDOWN *
     ************/

    if (H5Sclose(dspace_id) < 0)
        TEST_ERROR;
    if (H5Tclose(dtype_id) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl_id) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_0_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_1_id) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        (void)H5Sclose(dspace_id);
        (void)H5Tclose(dtype_id);
        (void)H5Pclose(dcpl_id);
        (void)H5Pclose(fapl_id);
        (void)H5Dclose(dset_0_id);
        (void)H5Dclose(dset_1_id);
        (void)H5Fclose(file_id);
    }
    H5E_END_TRY
    return FAIL;
} /* test_minimized_dset_ohdr_fillvalue_backwards_compatability */

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Exercise private object header behavior and routines
 *
 * Return:      Success: 0
 *              Failure: 1
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t          fapl = H5I_INVALID_HID;
    hid_t          file = H5I_INVALID_HID;
    H5F_t         *f    = NULL;
    const char    *driver_name;     /* File driver value from environment */
    bool           single_file_vfd; /* Whether VFD used stores data in a single file */
    char           filename[1024];
    H5O_hdr_info_t hdr_info;  /* Object info */
    H5O_loc_t      oh_loc;    /* Object header locations */
    H5F_libver_t   low, high; /* File format bounds */
    time_t         time_new, ro;
    int            i;                      /* Local index variable */
    bool           api_ctx_pushed = false; /* Whether API context pushed */
    herr_t         ret;                    /* Generic return value */

    /* Get the VFD to use */
    driver_name = h5_get_test_driver_name();

    /* Check for VFD which stores data in multiple files */
    single_file_vfd = !h5_driver_uses_multiple_files(driver_name, 0);

    /* Reset library */
    h5_reset();
    fapl = h5_fileaccess();
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Push API context */
    if (H5CX_push() < 0)
        FAIL_STACK_ERROR;
    api_ctx_pushed = true;

    /* Loop through all the combinations of low/high library format bounds */
    for (low = H5F_LIBVER_EARLIEST; low < H5F_LIBVER_NBOUNDS; low++) {
        for (high = H5F_LIBVER_EARLIEST; high < H5F_LIBVER_NBOUNDS; high++) {
            const char *low_string;  /* Message for library version low bound */
            const char *high_string; /* Message for library version high bound */
            char        msg[80];     /* Message for file format version */

            /* Set version bounds before opening the file */
            H5E_BEGIN_TRY
            {
                ret = H5Pset_libver_bounds(fapl, low, high);
            }
            H5E_END_TRY

            if (ret < 0) /* Invalid low/high combinations */
                continue;

            /* Display info about testing */
            low_string  = h5_get_version_string(low);
            high_string = h5_get_version_string(high);
            snprintf(msg, sizeof(msg), "Using file format version: (%s, %s)", low_string, high_string);
            puts(msg);

            /* test on object continuation block */
            if (test_cont(filename, fapl) < 0)
                TEST_ERROR;

            /* Create the file to operate on */
            if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
                FAIL_STACK_ERROR;
            if (NULL == (f = (H5F_t *)H5VL_object(file)))
                FAIL_STACK_ERROR;
            if (H5AC_ignore_tags(f) < 0) {
                H5_FAILED();
                H5Eprint2(H5E_DEFAULT, stdout);
                goto error;
            } /* end if */

            /*
             * Test object header creation
             * (using default group creation property list only because it's convenient)
             */
            TESTING("object header creation");
            memset(&oh_loc, 0, sizeof(oh_loc));
            if (H5O_create(f, (size_t)64, (size_t)0, H5P_GROUP_CREATE_DEFAULT, &oh_loc /*out*/) < 0)
                FAIL_STACK_ERROR;
            PASSED();

            /* create a new message */
            TESTING("message creation");
            time_new = 11111111;
            if (H5O_msg_create(&oh_loc, H5O_MTIME_NEW_ID, 0, 0, &time_new) < 0)
                FAIL_STACK_ERROR;
            if (1 != H5O_link(&oh_loc, 1))
                FAIL_STACK_ERROR;
            if (H5AC_prep_for_file_flush(f) < 0)
                FAIL_STACK_ERROR;
            if (H5AC_flush(f) < 0)
                FAIL_STACK_ERROR;
            if (H5AC_secure_from_file_flush(f) < 0)
                FAIL_STACK_ERROR;
            if (H5AC_expunge_entry(f, H5AC_OHDR, oh_loc.addr, H5AC__NO_FLAGS_SET) < 0)
                FAIL_STACK_ERROR;
            if (NULL == H5O_msg_read(&oh_loc, H5O_MTIME_NEW_ID, &ro))
                FAIL_STACK_ERROR;
            if (ro != time_new)
                TEST_ERROR;
            PASSED();

            /*
             * Test modification of an existing message.
             */
            TESTING("message modification");
            time_new = 33333333;
            if (H5O_msg_write(&oh_loc, H5O_MTIME_NEW_ID, 0, 0, &time_new) < 0)
                FAIL_STACK_ERROR;
            if (H5AC_prep_for_file_flush(f) < 0)
                FAIL_STACK_ERROR;
            if (H5AC_flush(f) < 0)
                FAIL_STACK_ERROR;
            if (H5AC_secure_from_file_flush(f) < 0)
                FAIL_STACK_ERROR;
            if (H5AC_expunge_entry(f, H5AC_OHDR, oh_loc.addr, H5AC__NO_FLAGS_SET) < 0)
                FAIL_STACK_ERROR;
            if (NULL == H5O_msg_read(&oh_loc, H5O_MTIME_NEW_ID, &ro))
                FAIL_STACK_ERROR;
            if (ro != time_new)
                TEST_ERROR;

            /* Make certain that chunk #0 in the object header can be encoded with a 1-byte size */
            if (H5O_get_hdr_info(&oh_loc, &hdr_info) < 0)
                FAIL_STACK_ERROR;
            if (hdr_info.space.total >= 256)
                TEST_ERROR;

            PASSED();

            /*
             * Test creation of a bunch of messages one after another to see
             * what happens when the object header overflows in core.
             * (Use 'old' MTIME message here, because it is large enough to be
             *  replaced with a continuation message (the new one is too small)
             *  and the library doesn't understand how to migrate more than one
             *  message from an object header currently - QAK - 10/8/03)
             */
            TESTING("object header overflow in memory");
            for (i = 0; i < 40; i++) {
                time_new = (i + 1) * 1000 + 1000000;
                if (H5O_msg_create(&oh_loc, H5O_MTIME_ID, 0, 0, &time_new) < 0)
                    FAIL_STACK_ERROR;
            } /* end for */
            if (H5AC_prep_for_file_flush(f) < 0)
                FAIL_STACK_ERROR;
            if (H5AC_flush(f) < 0)
                FAIL_STACK_ERROR;
            if (H5AC_secure_from_file_flush(f) < 0)
                FAIL_STACK_ERROR;
            if (H5AC_expunge_entry(f, H5AC_OHDR, oh_loc.addr, H5AC__NO_FLAGS_SET) < 0)
                FAIL_STACK_ERROR;

            /* Make certain that chunk #0 in the object header will be encoded with a 2-byte size */
            if (H5O_get_hdr_info(&oh_loc, &hdr_info) < 0)
                FAIL_STACK_ERROR;
            if (hdr_info.space.total < 256)
                TEST_ERROR;

            PASSED();

            /* Close & re-open file & object header */
            /* (makes certain that an object header in the new format that transitions
             *  between 1-byte chunk #0 size encoding and 2-byte chunk #0 size encoding
             *  works correctly - QAK)
             */
            TESTING("close & re-open object header");
            if (H5O_close(&oh_loc, NULL) < 0)
                FAIL_STACK_ERROR;
            if (H5Fclose(file) < 0)
                FAIL_STACK_ERROR;
            if ((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
                FAIL_STACK_ERROR;
            if (NULL == (f = (H5F_t *)H5VL_object(file)))
                FAIL_STACK_ERROR;
            if (H5AC_ignore_tags(f) < 0)
                FAIL_STACK_ERROR;
            oh_loc.file = f;
            if (H5O_open(&oh_loc) < 0)
                FAIL_STACK_ERROR;
            PASSED();

            /*
             * Test creation of a bunch of messages one after another to see
             * what happens when the object header overflows on disk.
             */
            TESTING("object header overflow on disk");
            for (i = 0; i < 10; i++) {
                time_new = (i + 1) * 1000 + 10;
                if (H5O_msg_create(&oh_loc, H5O_MTIME_NEW_ID, 0, 0, &time_new) < 0)
                    FAIL_STACK_ERROR;
                if (H5AC_prep_for_file_flush(f) < 0)
                    FAIL_STACK_ERROR;
                if (H5AC_flush(f) < 0)
                    FAIL_STACK_ERROR;
                if (H5AC_secure_from_file_flush(f) < 0)
                    FAIL_STACK_ERROR;
                if (H5AC_expunge_entry(f, H5AC_OHDR, oh_loc.addr, H5AC__NO_FLAGS_SET) < 0)
                    FAIL_STACK_ERROR;
            } /* end for */
            PASSED();

            /*
             * Delete all time messages.
             */
            TESTING("message deletion");
            if (H5O_msg_remove(&oh_loc, H5O_MTIME_NEW_ID, H5O_ALL, true) < 0)
                FAIL_STACK_ERROR;
            if (H5O_msg_remove(&oh_loc, H5O_MTIME_ID, H5O_ALL, true) < 0)
                FAIL_STACK_ERROR;
            if (H5O_msg_read(&oh_loc, H5O_MTIME_NEW_ID, &ro))
                FAIL_STACK_ERROR;
            if (H5O_msg_read(&oh_loc, H5O_MTIME_ID, &ro))
                FAIL_STACK_ERROR;
            PASSED();

            /*
             * Constant message handling.
             * (can't write to them, but should be able to remove them)
             */
            TESTING("constant message handling");
            time_new = 22222222;
            if (H5O_msg_create(&oh_loc, H5O_MTIME_NEW_ID, H5O_MSG_FLAG_CONSTANT, 0, &time_new) < 0)
                FAIL_STACK_ERROR;
            if (H5AC_prep_for_file_flush(f) < 0)
                FAIL_STACK_ERROR;
            if (H5AC_flush(f) < 0)
                FAIL_STACK_ERROR;
            if (H5AC_secure_from_file_flush(f) < 0)
                FAIL_STACK_ERROR;
            if (H5AC_expunge_entry(f, H5AC_OHDR, oh_loc.addr, H5AC__NO_FLAGS_SET) < 0)
                FAIL_STACK_ERROR;
            if (NULL == H5O_msg_read(&oh_loc, H5O_MTIME_NEW_ID, &ro))
                FAIL_STACK_ERROR;
            if (ro != time_new)
                TEST_ERROR;
            time_new = 33333333;
            H5E_BEGIN_TRY
            {
                ret = H5O_msg_write(&oh_loc, H5O_MTIME_NEW_ID, 0, 0, &time_new);
            }
            H5E_END_TRY
            if (ret >= 0)
                TEST_ERROR;
            if (H5O_msg_remove(&oh_loc, H5O_MTIME_NEW_ID, H5O_ALL, true) < 0)
                FAIL_STACK_ERROR;
            PASSED();

            /* release resources */
            TESTING("object header closing");
            if (H5O_close(&oh_loc, NULL) < 0)
                FAIL_STACK_ERROR;
            PASSED();

            /* Close the file we created */
            if (H5Fclose(file) < 0)
                TEST_ERROR;

            /* Test reading datasets with undefined object header messages
             * and the various "fail/mark if unknown" object header message flags
             */
            puts("Accessing objects with unknown header messages: H5O_BOGUS_VALID_ID");
            if (single_file_vfd) {
                if (test_unknown(H5O_BOGUS_VALID_ID, filename, fapl) < 0)
                    TEST_ERROR;
            } /* end if */
            else {
                SKIPPED();
                puts("    Unknown header message test not supported with the current VFD.");
            } /* end else */
            puts("Accessing objects with unknown header messages: H5O_BOGUS_INVALID_ID");
            if (single_file_vfd) {
                if (test_unknown(H5O_BOGUS_INVALID_ID, filename, fapl) < 0)
                    TEST_ERROR;
            } /* end if */
            else {
                SKIPPED();
                puts("    Unknown header message test not supported with the current VFD.");
            } /* end else */

            /* Test object header creation metadata cache issues */
            if (test_ohdr_cache(filename, fapl) < 0)
                TEST_ERROR;

            if (test_minimized_dset_ohdr_attribute_addition(fapl) < 0)
                TEST_ERROR;

            if (test_minimized_dset_ohdr_size_comparisons(fapl) < 0)
                TEST_ERROR;

            if (test_minimized_dset_ohdr_with_filter(fapl) < 0)
                TEST_ERROR;

            if (test_minimized_dset_ohdr_modification_times(fapl) < 0)
                TEST_ERROR;

            if (test_minimized_dset_ohdr_fillvalue_backwards_compatability(fapl) < 0)
                TEST_ERROR;

        } /* high */
    }     /* low */

    /* Verify bad ohdr message fixes work */
    test_ohdr_badness(fapl);

    /* Verify symbol table messages are cached */
    if (h5_verify_cached_stabs(FILENAME, fapl) < 0)
        TEST_ERROR;

    if (H5FD__supports_swmr_test(driver_name)) {
        /* A test to exercise the re-read of the object header for SWMR access */
        if (test_ohdr_swmr(true) < 0)
            TEST_ERROR;
        if (test_ohdr_swmr(false) < 0)
            TEST_ERROR;
    }
    else
        puts("Skipped SWMR tests for SWMR-incompatible VFD");

    /* Pop API context */
    if (api_ctx_pushed && H5CX_pop(false) < 0)
        FAIL_STACK_ERROR;
    api_ctx_pushed = false;

    puts("All object header tests passed.");
    h5_cleanup(FILENAME, fapl);
    return 0;

error:
    puts("*** TESTS FAILED ***");
    H5E_BEGIN_TRY
    {
        H5Fclose(file);
    }
    H5E_END_TRY

    if (api_ctx_pushed)
        H5CX_pop(false);

    return 1;
} /* end main() */
