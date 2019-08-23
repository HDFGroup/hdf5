/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* Programmer:  Mike McGreevy
 *              January 25, 2010
 *
 *              This file contains tests for metadata tagging.
 */
#define H5F_FRIEND        /*suppress error about including H5Fpkg      */
#define H5F_TESTING
#include "H5Fpkg.h"

#include "testhdf5.h"
#include "cache_common.h"

#include "H5CXprivate.h"        /* API Contexts                         */
#include "H5HLprivate.h"

/* ============ */
/* Test Defines */
/* ============ */

#define FILENAME      "tagging_test.h5"
#define FILENAME2     "tagging_ext_test.h5"
#define GROUPNAME     "Group"
#define GROUPNAMEPATH "/Group"
#define GROUPNAMECOPY "GroupCopy"
#define ATTRNAME      "Attribute 1"
#define ATTRNAME3     "Attribute 3"
#define DATASETNAME   "Dataset"
#define DATASETNAME2  "Dataset2"
#define LINKNAME      "Link"
#define RANK 2
#define DIMS 32

#define MULTIGROUPS 10

#define TEST_DEFAULT 0
#define TEST_SHMESG 1
#define NUM_TEST_TYPES 2

/* ===================== */
/* Function Declarations */
/* ===================== */

/* Helper Functions */
#ifndef NDEBUG
static int dump_cache(hid_t fid);
#endif /* NDEBUG */ /* end debugging functions */
static int verify_no_unknown_tags(hid_t fid);
static int mark_all_entries_investigated(hid_t fid);
static int reset_all_entries_investigated(hid_t fid);
static int verify_tag(hid_t fid, int id, haddr_t tag);
static int get_object_header_tag(hid_t loc_id, haddr_t *tag);
static int get_sbe_tag(hid_t fid, haddr_t *tag);

/* Tests */
static unsigned check_file_creation_tags(hid_t fcpl_id, int type);
static unsigned check_file_open_tags(hid_t fcpl, int type);
static unsigned check_attribute_creation_tags(hid_t fcpl, int type);
static unsigned check_attribute_open_tags(hid_t fcpl, int type);
static unsigned check_attribute_write_tags(hid_t fcpl, int type);
static unsigned check_attribute_delete_tags(hid_t fcpl, int type);
static unsigned check_attribute_rename_tags(hid_t fcpl, int type);
static unsigned check_dataset_creation_tags(hid_t fcpl, int type);
static unsigned check_dataset_creation_earlyalloc_tags(hid_t fcpl, int type);
static unsigned check_link_removal_tags(hid_t fcpl, int type);
static unsigned check_group_creation_tags(void);
static unsigned check_multi_group_creation_tags(void);
static unsigned check_group_open_tags(void);
static unsigned check_dataset_open_tags(void);
static unsigned check_dataset_write_tags(void);
static unsigned check_dataset_read_tags(void);
static unsigned check_dataset_size_retrieval(void);
static unsigned check_dataset_extend_tags(void);
static unsigned check_object_info_tags(void);
static unsigned check_object_copy_tags(void);
static unsigned check_link_getname_tags(void);
static unsigned check_external_link_creation_tags(void);
static unsigned check_external_link_open_tags(void);
static unsigned check_dense_attribute_tags(void);
static unsigned check_link_iteration_tags(void);
static unsigned check_invalid_tag_application(void);


/* ================ */
/* Helper Functions */
/* ================ */


#ifndef NDEBUG

/*-------------------------------------------------------------------------
 * Function:    dump_cache()
 *
 * Purpose:     DEBUG CODE (for when verbose is set).
 *
 *              Prints cache index to screen, including address of entries,
 *              tag values of entries, and entry types.
 *
 * Return:      void
 *
 * Programmer:  Mike McGreevy
 *              January 25, 2010
 *
 *-------------------------------------------------------------------------
 */
static int dump_cache(hid_t fid)
{
    H5F_t *f;           /* File Pointer */

    /* Get Internal File / Cache Pointers */
    if(NULL == (f = (H5F_t *)H5I_object(fid)))
        TEST_ERROR;

    /* Dump the cache */
    if(H5AC_dump_cache(f) < 0)
        TEST_ERROR;

    return 0;

error:
    return -1;
} /* dump_cache */
#endif /* NDEBUG */ /* end debugging functions */


/*-------------------------------------------------------------------------
 * Function:    verify_no_unknown_tags()
 *
 * Purpose:     Verifies that all tags in the provided cache have the
 *        'dirtied' flag set.  Other verification functions in this
 *        test file set this flag after checking them, so
 *              this is handy to verify that tests have checked all entries
 *              in the cache.
 *
 * Return:      0 on Success, -1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              January 25, 2010
 *
 *-------------------------------------------------------------------------
 */
static int
verify_no_unknown_tags(hid_t fid)
{

    H5F_t *f;           /* File Pointer */
    H5C_t *cache_ptr;   /* Cache Pointer */
    int i;              /* Iterator */

    /* Get Internal File / Cache Pointers */
    if(NULL == (f = (H5F_t *)H5I_object(fid)))
        TEST_ERROR;
    cache_ptr = f->shared->cache;

    for(i = 0; i < H5C__HASH_TABLE_LEN; i++) {
        H5C_cache_entry_t *entry_ptr;      /* entry pointer */

        entry_ptr = cache_ptr->index[i];
        while(entry_ptr != NULL) {
            if(!entry_ptr->dirtied)
                TEST_ERROR;

            entry_ptr = entry_ptr->ht_next;
        } /* end if */
    } /* end for */

    return 0;

error:
    return -1;
} /* verify_no_unknown_tags */


/*-------------------------------------------------------------------------
 * Function:    mark_all_entries_investigated()
 *
 * Purpose:     Marks all entries in the cache with the 'dirtied' flag,
 *              which is a convention in this test file that indicates that
 *              a tag has been checked and is valid. This may come in handy
 *              for tests that have a lot of setup that has been checked
 *              for correctness elsewhere, so should save time in not having
 *              to check the same sort of tag application in many places.
 *
 * Return:      0 on Success, -1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              February 3, 2010
 *
 *-------------------------------------------------------------------------
 */
static int
mark_all_entries_investigated(hid_t fid)
{
    H5F_t *f;           /* File Pointer */
    H5C_t *cache_ptr;   /* Cache Pointer */
    int i;              /* Iterator */

    /* Get Internal File / Cache Pointers */
    if(NULL == (f = (H5F_t *)H5I_object(fid)))
        TEST_ERROR;
    cache_ptr = f->shared->cache;

    for(i = 0; i < H5C__HASH_TABLE_LEN; i++) {
        H5C_cache_entry_t *entry_ptr;      /* entry pointer */

        entry_ptr = cache_ptr->index[i];
        while(entry_ptr != NULL) {
            if(!entry_ptr->dirtied)
                entry_ptr->dirtied = TRUE;

            entry_ptr = entry_ptr->ht_next;
        } /* end if */
    } /* end for */

    return 0;

error:
    return -1;
} /* mark_all_entries_investigated */


/*-------------------------------------------------------------------------
 * Function:    reset_all_entries_investigated()
 *
 * Purpose:     Resets all entries in the cache with the 'dirtied' flag,
 *              which is a convention in this test file that indicates that
 *              a tag has been checked and is valid. This resets the cache back
 *              to the same state as just after a flush call.
 *
 * Return:      0 on Success, -1 on Failure
 *
 * Programmer:  Quincey Koziol
 *              July 13, 2016
 *
 *-------------------------------------------------------------------------
 */
static int
reset_all_entries_investigated(hid_t fid)
{
    H5F_t *f;           /* File Pointer */
    H5C_t *cache_ptr;   /* Cache Pointer */
    int i;              /* Iterator */

    /* Get Internal File / Cache Pointers */
    if(NULL == (f = (H5F_t *)H5I_object(fid)))
        TEST_ERROR;
    cache_ptr = f->shared->cache;

    for(i = 0; i < H5C__HASH_TABLE_LEN; i++) {
        H5C_cache_entry_t *entry_ptr;      /* entry pointer */

        entry_ptr = cache_ptr->index[i];
        while(entry_ptr != NULL) {
            if(entry_ptr->dirtied)
                entry_ptr->dirtied = FALSE;

            entry_ptr = entry_ptr->ht_next;
        } /* end if */
    } /* end for */

    return 0;

error:
    return -1;
} /* reset_all_entries_investigated */


/*-------------------------------------------------------------------------
 * Function:    verify_tag()
 *
 * Purpose:     Asserts that there is an entry in the specified cache with
 *              the provided entry id and provided tag. The function will
 *              fail if this is not the case. If found, this function will
 *              set the entry's flush_marker flag, so future verification
 *              attempts can skip over this entry, knowing it has already been
 *              checked.
 *
 * Return:      0 on Success, -1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              January 25, 2010
 *
 *-------------------------------------------------------------------------
 */
static int
verify_tag(hid_t fid, int id, haddr_t tag)
{
    H5F_t *f;                   /* File Pointer */
    H5C_t *cache_ptr;           /* Cache Pointer */
    int i;                      /* Iterator */

    /* Get Internal File / Cache Pointers */
    if(NULL == (f = (H5F_t *)H5I_object(fid)))
        TEST_ERROR;
    cache_ptr = f->shared->cache;

    for(i = 0; i < H5C__HASH_TABLE_LEN; i++) {
        H5C_cache_entry_t *entry_ptr; /* entry pointer */

        entry_ptr = cache_ptr->index[i];
        while(entry_ptr != NULL) {
            if(entry_ptr->type->id == id && !entry_ptr->dirtied) {
                if(entry_ptr->tag_info->tag != tag)
                    TEST_ERROR;

                /* Mark the entry/tag pair as found */
                entry_ptr->dirtied = TRUE;

                /* leave now that we've found the entry */
                goto done;
            } /* end if */

            entry_ptr = entry_ptr->ht_next;
        } /* end if */
    } /* end for */

    /* Didn't find the tagged entry, throw an error */
    TEST_ERROR;

done:
    return 0;

error:
    return -1;
} /* verify_tag */

static int
evict_entries(hid_t fid)
{
    H5F_t *f;         /* File Pointer */

    /* Get Internal File / Cache Pointers */
    if(NULL == (f = (H5F_t *)H5I_object(fid)))
        TEST_ERROR;

    /* Mark all entries investigated */
    mark_all_entries_investigated(fid);

    /* Evict all we can from the cache to examine full tag creation tree */
        /* This function will likely return failure since the root group
         * is still protected. Thus, don't check its return value. */
    H5C_flush_cache(f, H5C__FLUSH_INVALIDATE_FLAG);

    return 0;

error:
    return -1;
} /* evict entries */


/*-------------------------------------------------------------------------
 * Function:    get_object_header_tag()
 *
 * Purpose:     This function retrieves the tag associated with an object.
 *
 * Return:      0 on Success; 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              January 25, 2010
 *
 *-------------------------------------------------------------------------
 */
static int
get_object_header_tag(hid_t loc_id, haddr_t *tag)
{
    H5O_info_t oinfo;           /* Object info */

    /* Retrieve the info for the object */
    if(H5Oget_info2(loc_id, &oinfo, H5O_INFO_ALL) < 0)
        TEST_ERROR;

    /* Set the tag to return */
    *tag = oinfo.addr;

    return 0;

error:
    return -1;
} /* get_object_header_tag */


/*-------------------------------------------------------------------------
 * Function:    get_sbe_tag()
 *
 * Purpose:     This function retrieves the tag associated with the superblock
 *              extension (the object header address stored in the superblock)
 *
 * Return:      0 on Success; 1 on Failure
 *
 * Programmer:  Quincey Koziol
 *              July 10, 2016
 *
 *-------------------------------------------------------------------------
 */
static int
get_sbe_tag(hid_t fid, haddr_t *tag)
{
    /* Retrieve the superblock extension's object header address for the file */
    if(H5F_get_sbe_addr_test(fid, tag) < 0)
        TEST_ERROR;

    return 0;

error:
    return -1;
} /* get_sbe_tag */

/* ============== */
/* Test Functions */
/* ============== */


/*-------------------------------------------------------------------------
 * Function:    check_file_creation_tags
 *
 * Purpose:     This function verifies the correct application of tags
 *              during file creation.
 *
 * Return:      0 on Success; 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              January 25, 2010
 *
 *-------------------------------------------------------------------------
 */
static unsigned
check_file_creation_tags(hid_t fcpl_id, int type)
{
    /* Variable Declarations */
    hid_t fid = -1;             /* File Identifier */
    int verbose = FALSE;        /* verbose test outout */
    haddr_t root_tag = 0;
    haddr_t sbe_tag = 0;

    /* Testing Macro */
    TESTING("tag application during file creation");

    /* Create a test file with provided fcpl_t */
    if ( (fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl_id, H5P_DEFAULT)) < 0 ) TEST_ERROR;

#ifndef NDEBUG
    /* if verbose, print cache index to screen before verification . */
    if ( verbose ) dump_cache(fid);
#endif /* NDEBUG */ /* end debugging functions */

    /* verify there is a superblock entry with superblock tag */
    if ( verify_tag(fid, H5AC_SUPERBLOCK_ID, H5AC__SUPERBLOCK_TAG) < 0 ) TEST_ERROR;

    if ( type == TEST_SHMESG ) {

        /* determine tag value of superblock extension object header */
        if ( get_sbe_tag(fid, &sbe_tag) < 0 ) TEST_ERROR;

        /* verify object header belonging to superblock extension */
        if ( verify_tag(fid, H5AC_OHDR_ID, sbe_tag) < 0 ) TEST_ERROR;

        /* verify sohm master table with sohm tag */
        if ( verify_tag(fid, H5AC_SOHM_TABLE_ID, H5AC__SOHM_TAG) < 0 ) TEST_ERROR;

        /* verify object header chunk belonging to superblock extension */
        if ( verify_tag(fid, H5AC_OHDR_CHK_ID, sbe_tag) < 0 ) TEST_ERROR;

    }  /* end if */

    /* determine tag value of root group's object header */
    if ( get_object_header_tag(fid, &root_tag) < 0 ) TEST_ERROR;

    /* verify object header belonging to superblock extension */
    if ( verify_tag(fid, H5AC_OHDR_ID, root_tag) < 0 ) TEST_ERROR;

    /* verify local heap prefix belonging to root group */
    if ( verify_tag(fid, H5AC_LHEAP_PRFX_ID, root_tag) < 0 ) TEST_ERROR;

    /* verify b-tree node belonging to root group */
    if ( verify_tag(fid, H5AC_BT_ID, root_tag) < 0 ) TEST_ERROR;

    /* verify no other cache entries present */
    if ( verify_no_unknown_tags(fid) < 0 ) TEST_ERROR;

    /* Reset the changes we've made to the cache's data structures */
    if(reset_all_entries_investigated(fid) < 0) TEST_ERROR;

    /* Close the file */
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;

    PASSED();
    return 0;

error:
    return 1;
} /* check_file_creation_tags */


/*-------------------------------------------------------------------------
 * Function:    check_file_open_tags
 *
 * Purpose:     This function verifies the correct application of tags
 *              during file open.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              January 25, 2010
 *
 *-------------------------------------------------------------------------
 */
static unsigned
check_file_open_tags(hid_t fcpl, int type)
{
    /* Variable Declarations */
    hid_t fid = -1;          /* File Identifier */
    int verbose = FALSE;     /* verbose file outout */
    haddr_t root_tag;       /* Root Group Tag */
    haddr_t sbe_tag;        /* Sblock Extension Tag */

    /* Testing Macro */
    TESTING("tag application during file open");

    /* ===== */
    /* Setup */
    /* ===== */

    /* Create a test file with provided fcpl_t */
    if ( (fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* determine tag value of root group's object header */
    if ( get_object_header_tag(fid, &root_tag) < 0 ) TEST_ERROR;

    /* Retrieve various tags */
    if ( type == TEST_SHMESG ) {

        /* determine tag value of superblock extension object header */
        if ( get_sbe_tag(fid, &sbe_tag) < 0 ) TEST_ERROR;

    } /* end if */

    /* Close the file */
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;

    /* =================== */
    /* TEST: Open The File */
    /* =================== */
    if ( (fid = H5Fopen(FILENAME, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* =================================== */
    /* Verification of Metadata Tag Values */
    /* =================================== */

#ifndef NDEBUG
    /* if verbose, print cache index to screen before verification . */
    if ( verbose ) dump_cache(fid);
#endif /* NDEBUG */ /* end debugging functions */

    /* verify there is a superblock entry with superblock tag. */
    if ( verify_tag(fid, H5AC_SUPERBLOCK_ID, H5AC__SUPERBLOCK_TAG) < 0 ) TEST_ERROR;

    /* Verify test-type-dependent tags */
    if ( type == TEST_DEFAULT ) {

        /* verify there is an object header belonging to the root group. */
        if ( verify_tag(fid, H5AC_OHDR_ID, root_tag) < 0 ) TEST_ERROR;

    } else if ( type == TEST_SHMESG ) {

        /* verify there is a superblock extension object header. */
        if ( verify_tag(fid, H5AC_OHDR_ID, sbe_tag) < 0 ) TEST_ERROR;

        /* verify sohm master table with sohm tag */
        if ( verify_tag(fid, H5AC_SOHM_TABLE_ID, H5AC__SOHM_TAG) < 0 ) TEST_ERROR;

        /* verify object header chunk belonging to superblock extension */
        if ( verify_tag(fid, H5AC_OHDR_CHK_ID, sbe_tag) < 0 ) TEST_ERROR;

    } /* end if */

    /* verify no other entries present */
    if ( verify_no_unknown_tags(fid) < 0 ) TEST_ERROR;

    /* Reset the changes we've made to the cache's data structures */
    if(reset_all_entries_investigated(fid) < 0) TEST_ERROR;

    /* ========== */
    /* Close file */
    /* ========== */

    if ( H5Fclose(fid) < 0 ) TEST_ERROR;

    /* ========================================== */
    /* Finished Test. Print status and return. */
    /* ========================================== */

    PASSED();
    return 0;

error:
    return 1;
} /* check_file_open_tags */


/*-------------------------------------------------------------------------
 * Function:    check_group_creation_tags
 *
 * Purpose:     This function verifies the correct application of tags
 *              during group creation.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              January 27, 2010
 *
 *-------------------------------------------------------------------------
 */
static unsigned
check_group_creation_tags(void)
{
    /* Variable Declarations */
    hid_t fid = -1;          /* File Identifier */
    hid_t gid = -1;          /* Group Identifier */
    int verbose = FALSE;     /* verbose file outout */
    haddr_t root_tag = HADDR_UNDEF;   /* Root Group Tag */
    haddr_t g_tag;          /* Group Tag */

    /* Testing Macro */
    TESTING("tag application during group creation");

    /* ===== */
    /* Setup */
    /* ===== */

    /* Create a test file with provided fcpl_t */
    if ( (fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* determine tag value of root group's object header */
    if ( get_object_header_tag(fid, &root_tag) < 0 ) TEST_ERROR;

    /* Close and Reopen the file */
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;
    if ( (fid = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Evict as much as we can from the cache so we can track full tag path */
    if ( evict_entries(fid) < 0 ) TEST_ERROR;

    /* ==================== */
    /* TEST: Create a Group */
    /* ==================== */

    if ( (gid = H5Gcreate2(fid, GROUPNAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* =================================== */
    /* Verification of Metadata Tag Values */
    /* =================================== */

#ifndef NDEBUG
    /* if verbose, print cache index to screen for visual verification */
    if ( verbose ) dump_cache(fid);
#endif /* NDEBUG */ /* end debugging functions */

    /* Verify root group's tagged metadata */
    if ( verify_tag(fid, H5AC_OHDR_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_SNODE_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_LHEAP_PRFX_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_BT_ID, root_tag) < 0 ) TEST_ERROR;

    /* Verify new group's tagged metadata */
    if ( get_object_header_tag(gid, &g_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_OHDR_ID, g_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_BT_ID, g_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_LHEAP_PRFX_ID, g_tag) < 0 ) TEST_ERROR;

    /* verify no other cache entries present */
    if ( verify_no_unknown_tags(fid) < 0 ) TEST_ERROR;

    /* Reset the changes we've made to the cache's data structures */
    if(reset_all_entries_investigated(fid) < 0) TEST_ERROR;

    /* =========================== */
    /* Close open objects and file */
    /* =========================== */

    if ( H5Gclose(gid) < 0 ) TEST_ERROR;
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;

    /* ========================================== */
    /* Finished Test. Print status and return. */
    /* ========================================== */

    PASSED();
    return 0;

error:
    return 1;
} /* check_group_creation_tags */


/*-------------------------------------------------------------------------
 * Function:    check_multi_group_creation_tags
 *
 * Purpose:     This function verifies the correct application of tags
 *              during multiple group creation.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              March 2, 2010
 *
 *-------------------------------------------------------------------------
 */
static unsigned
check_multi_group_creation_tags(void)
{
    /* Variable Declarations */
    hid_t fid = -1;          /* File Identifier */
    hid_t gid = -1;          /* Group Identifier */
    int verbose = FALSE;     /* verbose file outout */
    char gname[16];          /* group name buffer */
    int i = 0;               /* iterator */
    hid_t fapl = -1;         /* File access prop list */
    haddr_t g_tag = 0;       /* Group tag value */
    haddr_t root_tag = 0;    /* Root group tag value */

    /* Testing Macro */
    TESTING("tag application during multiple group creation");

    /* Create Fapl */
    if ( (fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0 ) TEST_ERROR;

    /* Set latest version of library */
    if ( H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0 ) TEST_ERROR;

    /* =========== */
    /* Create File */
    /* =========== */

    if ( (fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0 ) TEST_ERROR;

    /* determine tag value of root group's object header */
    if ( get_object_header_tag(fid, &root_tag) < 0 ) TEST_ERROR;

    /* Clear Metadata Tags (don't care about them for this test) */
    mark_all_entries_investigated(fid);

    /* ============= */
    /* Create Groups */
    /* ============= */

    for (i = 0; i < MULTIGROUPS; i++) {

        HDsprintf(gname, "%d", i);
        if ( (gid = H5Gcreate2(fid, gname, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;
        if ( H5Gclose(gid) < 0 ) TEST_ERROR;

    } /* end for */

    /* =================================== */
    /* Verification of Metadata Tag Values */
    /* =================================== */

#ifndef NDEBUG
    /* if verbose, print cache index to screen for visual verification */
    if ( verbose ) dump_cache(fid);
#endif /* NDEBUG */ /* end debugging functions */

    /* Verify there is an object header for each group */
    for (i = 0; i < MULTIGROUPS; i++) {

        /* Re-open the group */
        HDsprintf(gname, "%d", i);
        if ( (gid = H5Gopen2(fid, gname, H5P_DEFAULT)) < 0 ) TEST_ERROR;

        /* Verify object header for root group */
        /* ('dirtied' flag on entry gets cleared with each open operation) */
        if ( verify_tag(fid, H5AC_OHDR_ID, root_tag) < 0 ) TEST_ERROR;

        /* Retrieve the object address for the group */
        if ( get_object_header_tag(gid, &g_tag) < 0 ) TEST_ERROR;

        /* Verify object header for group */
        if ( verify_tag(fid, H5AC_OHDR_ID, g_tag) < 0 ) TEST_ERROR;

        /* Close the group */
        if ( H5Gclose(gid) < 0 ) TEST_ERROR;
    } /* end for */

    /* Verify free space header and section info */
    if ( verify_tag(fid, H5AC_FSPACE_SINFO_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_FSPACE_HDR_ID, root_tag) < 0 ) TEST_ERROR;

    /* verify fractal heap header belonging to root group */
    if ( verify_tag(fid, H5AC_FHEAP_HDR_ID, root_tag) < 0 ) TEST_ERROR;

    /* verify fractal heap direct block belonging to root group */
    if ( verify_tag(fid, H5AC_FHEAP_DBLOCK_ID, root_tag) < 0 ) TEST_ERROR;

    /* verify btree header and leaf node belonging to root group */
    if ( verify_tag(fid, H5AC_BT2_HDR_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_BT2_LEAF_ID, root_tag) < 0 ) TEST_ERROR;

    /* verify no other entries present */
    if ( verify_no_unknown_tags(fid) < 0 ) TEST_ERROR;

    /* Reset the changes we've made to the cache's data structures */
    if(reset_all_entries_investigated(fid) < 0) TEST_ERROR;

    /* =========================== */
    /* Close open objects and file */
    /* =========================== */

    if ( H5Fclose(fid) < 0 ) TEST_ERROR;

    /* ========================================== */
    /* Finished Test. Print status and return. */
    /* ========================================== */

    PASSED();
    return 0;

error:
    return 1;
} /* check_multi_group_creation_tags */


/*-------------------------------------------------------------------------
 * Function:    check_link_iteration_tags
 *
 * Purpose:     This function verifies the correct application of tags
 *              during iteration over links in a group.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              March 2, 2010
 *
 *-------------------------------------------------------------------------
 */
static unsigned
check_link_iteration_tags(void)
{
    /* Variable Declarations */
    hid_t fid = -1;          /* File Identifier */
    hid_t sid = -1;          /* Group Identifier */
    hid_t did = -1;          /* Group Identifier */
    int verbose = FALSE;     /* verbose file outout */
    int i = 0;               /* iterator */
    haddr_t root_tag = 0;   /* Root Group Tag Value */
    char dsetname[500];      /* Name of dataset */
    H5G_info_t ginfo;        /* Group Info Struct */
    hid_t root_group = -1;   /* Root Group Identifier */

    /* Testing Macro */
    TESTING("tag application during iteration over links in a group");

    /* =========== */
    /* Create File */
    /* =========== */
    if ( (fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Get root group tag */
    if ( get_object_header_tag(fid, &root_tag) < 0 ) TEST_ERROR;

    /* Create dataspace */
    if ( (sid = H5Screate(H5S_SCALAR)) < 0 ) TEST_ERROR;

    /* Create many datasets in root group */
    for (i=0;i<500;i++) {

        HDsprintf(dsetname, "Dset %d", i);
        if ( (did = H5Dcreate2(fid, dsetname, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;
        if ( H5Dclose(did) < 0 ) TEST_ERROR;
    }

    /* Close and Reopen the file (to clear cache) */
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;
    if ( (fid = H5Fopen(FILENAME, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* clear remaining metadata tags */
    mark_all_entries_investigated(fid);

    /* ================================ */
    /* Iterate over links in root group */
    /* ================================ */

    /* Open root group */
    if ( (root_group = H5Gopen2(fid, "/", H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Get root group info (will iterate over all links in group) */
    if ( H5Gget_info(root_group, &ginfo) < 0 ) TEST_ERROR;

    /* =================================== */
    /* Verification of Metadata Tag Values */
    /* =================================== */

#ifndef NDEBUG
    /* if verbose, print cache index to screen for visual verification */
    if ( verbose ) dump_cache(fid);
#endif /* NDEBUG */ /* end debugging functions */

    /* Verify root group's tagged metadata */
    if ( verify_tag(fid, H5AC_OHDR_ID, root_tag) < 0 ) TEST_ERROR;

    /* Verify 112 symbol table nodes belonging to the root group */
    for (i = 0; i < 112; i++)
        if ( verify_tag(fid, H5AC_SNODE_ID, root_tag) < 0 ) TEST_ERROR;

    /* Verify 9 b-tree nodes belonging to the root group */
    for (i = 0; i < 9; i++)
        if ( verify_tag(fid, H5AC_BT_ID, root_tag) < 0 ) TEST_ERROR;

    /* verify no other entries present */
    if ( verify_no_unknown_tags(fid) < 0 ) TEST_ERROR;

    /* Reset the changes we've made to the cache's data structures */
    if(reset_all_entries_investigated(fid) < 0) TEST_ERROR;

    /* =========================== */
    /* Close open objects and file */
    /* =========================== */

    if ( H5Sclose(sid) < 0 ) TEST_ERROR;
    if ( H5Gclose(root_group) < 0 ) TEST_ERROR;
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;

    /* ========================================== */
    /* Finished Test. Print status and return. */
    /* ========================================== */

    PASSED();
    return 0;

error:
    return 1;
} /* check_link_iteration_tags */


/*-------------------------------------------------------------------------
 * Function:    check_dense_attribute_tags
 *
 * Purpose:     This function verifies the correct application of tags
 *              during various dense attribute manipulations.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              March 2, 2010
 *
 *-------------------------------------------------------------------------
 */
static unsigned
check_dense_attribute_tags(void)
{
    /* Variable Declarations */
    hid_t fid = -1;          /* File Identifier */
    hid_t aid = -1;          /* File Identifier */
    hid_t sid = -1;          /* Group Identifier */
    hid_t did = -1;          /* Group Identifier */
    hid_t dcpl = -1;         /* Group Identifier */
    int verbose = FALSE;     /* verbose file outout */
    int i = 0;               /* iterator */
    hid_t fapl = -1;         /* File access property list */
    haddr_t d_tag = 0;       /* Dataset tag value */
    haddr_t root_tag = 0;    /* Root group tag value */
    char attrname[500];         /* Name of attribute */

    /* Testing Macro */
    TESTING("tag application during dense attribute manipulation");

    /* Create Fapl */
    if ( (fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0 ) TEST_ERROR;
    if ( H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0 ) TEST_ERROR;

    /* Create Dcpl */
    if ( (dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0 ) TEST_ERROR;

    /* =========== */
    /* Create File */
    /* =========== */
    if ( (fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0 ) TEST_ERROR;

    /* determine tag value of root group's object header */
    if ( get_object_header_tag(fid, &root_tag) < 0 ) TEST_ERROR;

    /* Create dataspace */
    if ( (sid = H5Screate(H5S_SCALAR)) < 0 ) TEST_ERROR;

    /* Create dataset */
    if ( (did = H5Dcreate2(fid, DATASETNAME, H5T_NATIVE_UCHAR, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0 ) TEST_ERROR;
    if ( H5Pclose(dcpl) < 0 ) TEST_ERROR;

    /* get dataset object header */
    if ( get_object_header_tag(did, &d_tag) < 0 ) TEST_ERROR;

    /* Clear Metadata Tags (don't care about them for this test */
    mark_all_entries_investigated(fid);

    /* ================================================ */
    /* Create Many attributes, triggering dense storage */
    /* ================================================ */

    for (i=0;i<50;i++) {

        HDsprintf(attrname, "attr %d", i);
        if ( (aid = H5Acreate2(did, attrname, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;
        if ( H5Awrite(aid, H5T_NATIVE_UINT, &i) < 0 ) TEST_ERROR;
        if ( H5Aclose(aid) < 0 ) TEST_ERROR;

    } /* end for */

    /* =================================== */
    /* Verification of Metadata Tag Values */
    /* =================================== */

#ifndef NDEBUG
    /* if verbose, print cache index to screen for visual verification */
    if ( verbose ) dump_cache(fid);
#endif /* NDEBUG */ /* end debugging functions */

    /* Verify free space header and section info */
    if ( verify_tag(fid, H5AC_FSPACE_SINFO_ID, d_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_FSPACE_HDR_ID, d_tag) < 0 ) TEST_ERROR;

    /* verify object header belonging to dataset */
    if ( verify_tag(fid, H5AC_OHDR_ID, d_tag) < 0 ) TEST_ERROR;

    /* verify fractal heap header belonging to dataset */
    if ( verify_tag(fid, H5AC_FHEAP_HDR_ID, d_tag) < 0 ) TEST_ERROR;

    /* verify fractal heap direct block belonging to root group */
    if ( verify_tag(fid, H5AC_FHEAP_DBLOCK_ID, d_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_FHEAP_DBLOCK_ID, d_tag) < 0 ) TEST_ERROR;

    /* verify btree header and leaf node belonging to dataset */
    if ( verify_tag(fid, H5AC_BT2_HDR_ID, d_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_BT2_LEAF_ID, d_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_BT2_LEAF_ID, d_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_BT2_INT_ID, d_tag) < 0 ) TEST_ERROR;

    /* verify no other entries present */
    if ( verify_no_unknown_tags(fid) < 0 ) TEST_ERROR;

    /* Reset the changes we've made to the cache's data structures */
    if(reset_all_entries_investigated(fid) < 0) TEST_ERROR;

    /* =========================== */
    /* Close open objects and file */
    /* =========================== */

    if ( H5Dclose(did) < 0 ) TEST_ERROR;
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;

    /* ======================= */
    /* Reopen file and dataset */
    /* ======================= */

    if ( (fid = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT)) < 0 ) TEST_ERROR;
    if ( (did = H5Dopen2(fid, DATASETNAME, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Clear Metadata Tags (don't care about them for this test */
    mark_all_entries_investigated(fid);

    /* ======================= */
    /* Open attribute by index */
    /* ======================= */

    if ( (aid = H5Aopen_by_idx(did, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)4, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* =================================== */
    /* Verification of Metadata Tag Values */
    /* =================================== */

#ifndef NDEBUG
    /* if verbose, print cache index to screen for visual verification */
    if ( verbose ) dump_cache(fid);
#endif /* NDEBUG */ /* end debugging functions */

    /* verify object header belonging to dataset */
    if ( verify_tag(fid, H5AC_OHDR_ID, d_tag) < 0 ) TEST_ERROR;

    /* verify fractal heap header belonging to dataset */
    if ( verify_tag(fid, H5AC_FHEAP_HDR_ID, d_tag) < 0 ) TEST_ERROR;

    /* verify fractal heap direct block belonging to root group */
    if ( verify_tag(fid, H5AC_FHEAP_DBLOCK_ID, d_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_FHEAP_DBLOCK_ID, d_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_FHEAP_IBLOCK_ID, d_tag) < 0 ) TEST_ERROR;

    /* verify btree header and leaf node belonging to dataset */
    if ( verify_tag(fid, H5AC_BT2_HDR_ID, d_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_BT2_LEAF_ID, d_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_BT2_LEAF_ID, d_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_BT2_INT_ID, d_tag) < 0 ) TEST_ERROR;

    /* verify no other entries present */
    if ( verify_no_unknown_tags(fid) < 0 ) TEST_ERROR;

    /* Reset the changes we've made to the cache's data structures */
    if(reset_all_entries_investigated(fid) < 0) TEST_ERROR;

    /* =========================== */
    /* Close open objects and file */
    /* =========================== */

    if ( H5Aclose(aid) < 0 ) TEST_ERROR;
    if ( H5Dclose(did) < 0 ) TEST_ERROR;
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;

    /* ========================================== */
    /* Finished Test. Print status and return. */
    /* ========================================== */

    PASSED();
    return 0;

error:
    return 1;
} /* check_dense_attribute_tags */


/*-------------------------------------------------------------------------
 * Function:    check_group_open_tags
 *
 * Purpose:     This function verifies the correct application of tags
 *              during group open.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              January 27, 2010
 *
 *-------------------------------------------------------------------------
 */
static unsigned
check_group_open_tags(void)
{
    /* Variable Declarations */
    hid_t fid = -1;          /* File Identifier */
    hid_t gid = -1;          /* Group Identifier */
    int verbose = FALSE;     /* verbose file output */
    haddr_t root_tag = HADDR_UNDEF;
    haddr_t g_tag;

    /* Testing Macro */
    TESTING("tag application during group open");

    /* ===== */
    /* Setup */
    /* ===== */

    /* Create a test file with provided fcpl_t */
    if ( (fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* determine tag value of root group's object header */
    if ( get_object_header_tag(fid, &root_tag) < 0 ) TEST_ERROR;

    /* Create group */
    if ( (gid = H5Gcreate2(fid, GROUPNAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Retrieve group tag */
    if ( get_object_header_tag(gid, &g_tag) < 0 ) TEST_ERROR;

    /* Close Group */
    if (H5Gclose(gid) < 0) TEST_ERROR;

    /* Close and Reopen the file */
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;
    if ( (fid = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Evict as much as we can from the cache so we can track full tag path */
    if ( evict_entries(fid) < 0 ) TEST_ERROR;

    /* ================ */
    /* TEST: Open Group */
    /* ================ */

    if ( (gid = H5Gopen2(fid, GROUPNAME, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* =================================== */
    /* Verification of Metadata Tag Values */
    /* =================================== */

#ifndef NDEBUG
    /* if verbose, print cache index to screen for visual verification */
    if ( verbose ) dump_cache(fid);
#endif /* NDEBUG */ /* end debugging functions */

    /* Verify root group metadata */
    if ( verify_tag(fid, H5AC_OHDR_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_SNODE_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_LHEAP_PRFX_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_BT_ID, root_tag) < 0 ) TEST_ERROR;

    /* Verify opened group metadata */
    if ( verify_tag(fid, H5AC_OHDR_ID, g_tag) < 0 ) TEST_ERROR;

    /* verify no other entries present */
    if ( verify_no_unknown_tags(fid) < 0 ) TEST_ERROR;

    /* Reset the changes we've made to the cache's data structures */
    if(reset_all_entries_investigated(fid) < 0) TEST_ERROR;

    /* =========================== */
    /* Close open objects and file */
    /* =========================== */

    if ( H5Gclose(gid) < 0 ) TEST_ERROR;
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;

    /* ========================================== */
    /* Finished Test. Print status and return. */
    /* ========================================== */

    PASSED();
    return 0;

error:
    return 1;
} /* check_group_open_tags */


/*-------------------------------------------------------------------------
 * Function:    check_attribute_creation_tags
 *
 * Purpose:     This function verifies the correct application of tags
 *              during attribute creation.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              February 24, 2010
 *
 *-------------------------------------------------------------------------
 */
static unsigned
check_attribute_creation_tags(hid_t fcpl, int type)
{
    /* Variable Declarations */
    hid_t fid = -1;          /* File Identifier */
    hid_t aid = -1;          /* Attribute Identifier */
    hid_t gid = -1;          /* Group Identifier */
    hid_t sid = -1;          /* Dataspace Identifier */
    int verbose = FALSE;     /* verbose file outout */
    haddr_t root_tag = 0;   /* Root group tag */
    haddr_t g_tag = 0;
    hsize_t dims1[2] = {DIMS, DIMS}; /* dimensions */
    hsize_t maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED}; /* dimensions */

    /* Testing Macro */
    TESTING("tag application during attribute creation");

    /* ===== */
    /* Setup */
    /* ===== */

    /* Create a test file with provided fcpl_t */
    if ( (fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* determine tag value of root group's object header */
    if ( get_object_header_tag(fid, &root_tag) < 0 ) TEST_ERROR;

    /* Create group */
    if ( (gid = H5Gcreate2(fid, GROUPNAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Retrieve group tag */
    if ( get_object_header_tag(gid, &g_tag) < 0 ) TEST_ERROR;

    /* Close and Reopen the file and group */
    if ( H5Gclose(gid) < 0 ) TEST_ERROR;
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;
    if ( (fid = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT)) < 0 ) TEST_ERROR;
    if ( (gid = H5Gopen2(fid, GROUPNAME, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Evict as much as we can from the cache so we can track full tag path */
    if ( evict_entries(fid) < 0 ) TEST_ERROR;

    /* ========================= */
    /* Create Attribute on Group */
    /* ========================= */

    if ( (sid = H5Screate_simple(2, dims1, maxdims)) < 0 ) TEST_ERROR;

    if ( (aid = H5Acreate2(gid, ATTRNAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* =================================== */
    /* Verification of Metadata Tag Values */
    /* =================================== */

#ifndef NDEBUG
    /* if verbose, print cache index to screen for visual verification */
    if ( verbose ) dump_cache(fid);
#endif /* NDEBUG */ /* end debugging functions */

    /* verify object header belonging to group */
    if ( verify_tag(fid, H5AC_OHDR_ID, g_tag) < 0 ) TEST_ERROR;

    /* verify object header chunk belonging to group */
    if ( verify_tag(fid, H5AC_OHDR_CHK_ID, g_tag) < 0 ) TEST_ERROR;

    if ( type == TEST_SHMESG ) {

        /* verify (another) object header chunk belonging to group */
        if ( verify_tag(fid, H5AC_OHDR_CHK_ID, g_tag) < 0 ) TEST_ERROR;

        /* verify shared message index tagged with sohm */
        if ( verify_tag(fid, H5AC_SOHM_LIST_ID, H5AC__SOHM_TAG) < 0 ) TEST_ERROR;
        if ( verify_tag(fid, H5AC_SOHM_TABLE_ID, H5AC__SOHM_TAG) < 0 ) TEST_ERROR;

        /* verify fractal heap header belonging to group */
        if ( verify_tag(fid, H5AC_FHEAP_HDR_ID, H5AC__SOHM_TAG) < 0 ) TEST_ERROR;

        /* verify fractal heap direct block belonging to group */
        if ( verify_tag(fid, H5AC_FHEAP_DBLOCK_ID, H5AC__SOHM_TAG) < 0 ) TEST_ERROR;

        /* Verify free space header and free space section */
        if ( verify_tag(fid, H5AC_FSPACE_HDR_ID, H5AC__SOHM_TAG) < 0 ) TEST_ERROR;
        if ( verify_tag(fid, H5AC_FSPACE_SINFO_ID, H5AC__SOHM_TAG) < 0 ) TEST_ERROR;

        /* verify btree header and leaf node belonging to group */
        if ( verify_tag(fid, H5AC_BT2_HDR_ID, H5AC__SOHM_TAG) < 0 ) TEST_ERROR;
        if ( verify_tag(fid, H5AC_BT2_LEAF_ID, H5AC__SOHM_TAG) < 0 ) TEST_ERROR;

    } /* end if */

    /* verify no other entries present */
    if ( verify_no_unknown_tags(fid) < 0 ) TEST_ERROR;

    /* Reset the changes we've made to the cache's data structures */
    if(reset_all_entries_investigated(fid) < 0) TEST_ERROR;

    /* =========================== */
    /* Close open objects and file */
    /* =========================== */

    if ( H5Aclose(aid) < 0 ) TEST_ERROR;
    if ( H5Gclose(gid) < 0 ) TEST_ERROR;
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;

    /* ========================================== */
    /* Finished Test. Print status and return. */
    /* ========================================== */

    PASSED();
    return 0;

error:
    return 1;
} /* check_attribute_creation_tags */


/*-------------------------------------------------------------------------
 * Function:    check_attribute_open_tags
 *
 * Purpose:     This function verifies the correct application of tags
 *              during attribute open.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              February 24, 2010
 *
 *-------------------------------------------------------------------------
 */
static unsigned
check_attribute_open_tags(hid_t fcpl, int type)
{
    /* Variable Declarations */
    hid_t fid = -1;          /* File Identifier */
    hid_t aid = -1;          /* Attribute Identifier */
    hid_t gid = -1;          /* Group Identifier */
    hid_t sid = -1;          /* Dataspace Identifier */
    int verbose = FALSE;     /* verbose file outout */
    haddr_t root_tag = 0;
    haddr_t g_tag = 0;
    hsize_t dims1[2] = {DIMS, DIMS}; /* dimensions */
    hsize_t maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED}; /* dimensions */

    /* Testing Macro */
    TESTING("tag application during attribute open");

    /* ===== */
    /* Setup */
    /* ===== */

    /* Create a test file with provided fcpl_t */
    if ( (fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* determine tag value of root group's object header */
    if ( get_object_header_tag(fid, &root_tag) < 0 ) TEST_ERROR;

    /* Create group */
    if ( (gid = H5Gcreate2(fid, GROUPNAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Retrieve group tag */
    if ( get_object_header_tag(gid, &g_tag) < 0 ) TEST_ERROR;

    /* Create attribute dataspace */
    if ( (sid = H5Screate_simple(2, dims1, maxdims)) < 0 ) TEST_ERROR;

    /* Create attribute on group */
    if ( (aid = H5Acreate2(gid, ATTRNAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Close attribute */
    if ( H5Aclose(aid) < 0 ) TEST_ERROR;

    /* Close and Reopen the file and group */
    if ( H5Gclose(gid) < 0 ) TEST_ERROR;
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;
    if ( (fid = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT)) < 0 ) TEST_ERROR;
    if ( (gid = H5Gopen2(fid, GROUPNAME, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Evict as much as we can from the cache so we can track full tag path */
    if ( evict_entries(fid) < 0 ) TEST_ERROR;

    /* ========================= */
    /* Open Attribute of Group */
    /* ========================= */

    if ( (aid = H5Aopen(gid, ATTRNAME, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* =================================== */
    /* Verification of Metadata Tag Values */
    /* =================================== */

#ifndef NDEBUG
    /* if verbose, print cache index to screen for visual verification */
    if ( verbose ) dump_cache(fid);
#endif /* NDEBUG */ /* end debugging functions */

    /* verify object header belonging to group */
    if ( verify_tag(fid, H5AC_OHDR_ID, g_tag) < 0 ) TEST_ERROR;

    /* verify object header chunk belonging to group */
    if ( verify_tag(fid, H5AC_OHDR_CHK_ID, g_tag) < 0 ) TEST_ERROR;

    if ( type == TEST_SHMESG ) {

        /* verify (another) object header chunk belonging to group */
        if ( verify_tag(fid, H5AC_OHDR_CHK_ID, g_tag) < 0 ) TEST_ERROR;

        /* verify shared header message master table */
        if ( verify_tag(fid, H5AC_SOHM_TABLE_ID, H5AC__SOHM_TAG) < 0 ) TEST_ERROR;

        /* verify fractal heap header belonging to group */
        if ( verify_tag(fid, H5AC_FHEAP_HDR_ID, g_tag) < 0 ) TEST_ERROR;

        /* verify fractal heap direct block belonging to group */
        if ( verify_tag(fid, H5AC_FHEAP_DBLOCK_ID, g_tag) < 0 ) TEST_ERROR;

        /* verify btree header and leaf node belonging to group */
        if ( verify_tag(fid, H5AC_BT2_HDR_ID, g_tag) < 0 ) TEST_ERROR;
        if ( verify_tag(fid, H5AC_BT2_LEAF_ID, g_tag) < 0 ) TEST_ERROR;

    } /* end if */

    /* verify no other entries present */
    if ( verify_no_unknown_tags(fid) < 0 ) TEST_ERROR;

    /* Reset the changes we've made to the cache's data structures */
    if(reset_all_entries_investigated(fid) < 0) TEST_ERROR;

    /* =========================== */
    /* Close open objects and file */
    /* =========================== */

    if ( H5Aclose(aid) < 0 ) TEST_ERROR;
    if ( H5Gclose(gid) < 0 ) TEST_ERROR;
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;

    /* ========================================== */
    /* Finished Test. Print status and return. */
    /* ========================================== */

    PASSED();
    return 0;

error:
    return 1;
} /* check_attribute_open_tags */


/*-------------------------------------------------------------------------
 * Function:    check_attribute_rename_tags
 *
 * Purpose:     This function verifies the correct application of tags
 *              during attribute renaming.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              March 3, 2010
 *
 *-------------------------------------------------------------------------
 */
static unsigned
check_attribute_rename_tags(hid_t fcpl, int type)
{
    /* Variable declarations */
    hid_t fid = -1;                         /* File Identifier */
    hid_t gid = -1;                         /* Group Identifier */
    hid_t aid = -1;                         /* Attribute Identifier */
    hid_t sid = -1;                         /* Dataset Identifier */
    int verbose = FALSE;                    /* verbose file outout */
    int *data = NULL;                       /* data buffer */
    int i,j,k = 0;                          /* iterators */
    haddr_t root_tag = 0;
    haddr_t g_tag = 0;
    hsize_t dims1[2] = {DIMS, DIMS}; /* dimensions */
    hsize_t maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED}; /* dimensions */
    hbool_t persistent_fsms = FALSE;

    /* Testing Macro */
    TESTING("tag application during attribute renaming");

    /* ===== */
    /* Setup */
    /* ===== */

    /* check to see if the FCPL specified persistent free space managers */
    if(H5Pget_file_space_strategy(fcpl, NULL, &persistent_fsms, NULL) < 0)
        TEST_ERROR;

    /* Allocate array */
    if ( (NULL == (data = (int *)HDcalloc(DIMS * DIMS, sizeof(int)))) ) TEST_ERROR;

    /* Create a test file with provided fcpl_t */
    if ( (fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* determine tag value of root group's object header */
    if ( get_object_header_tag(fid, &root_tag) < 0 ) TEST_ERROR;

    /* Create group */
    if ( (gid = H5Gcreate2(fid, GROUPNAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Retrieve group tag */
    if ( get_object_header_tag(gid, &g_tag) < 0 ) TEST_ERROR;

    /* Set up attribute dataspace */
    if ( (sid = H5Screate_simple(2, dims1, maxdims)) < 0 ) TEST_ERROR;

    /* Create attribute */
    if ( (aid = H5Acreate2(gid, ATTRNAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* fill out data buffer */
    for(i = 0; i < DIMS; i++)
        for(j = 0; j < DIMS; j++)
            data[(DIMS * i) + j] = k++;

    /* Write to attribute */
    if ( H5Awrite(aid, H5T_NATIVE_INT, data) < 0 ) TEST_ERROR;

    /* Close Attribute */
    if ( H5Aclose(aid) < 0 ) TEST_ERROR;

    /* Close and Reopen the file and group */
    if ( H5Gclose(gid) < 0 ) TEST_ERROR;
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;

    if ( (fid = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT)) < 0 ) TEST_ERROR;
    if ( (gid = H5Gopen2(fid, GROUPNAME, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Evict as much as we can from the cache so we can track full tag path */
    if ( evict_entries(fid) < 0 ) TEST_ERROR;

    /* ========================= */
    /* Rename Attribute of Group */
    /* ========================= */

    if ( H5Arename_by_name(fid, GROUPNAME, ATTRNAME, ATTRNAME3, H5P_DEFAULT) < 0 ) TEST_ERROR;

    /* =================================== */
    /* Verification of Metadata Tag Values */
    /* =================================== */

#ifndef NDEBUG
    /* if verbose, print cache index to screen for visual verification */
    if ( verbose ) dump_cache(fid);
#endif /* NDEBUG */ /* end debugging functions */

    /* Verify root group metadata */
    if ( verify_tag(fid, H5AC_OHDR_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_SNODE_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_LHEAP_PRFX_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_BT_ID, root_tag) < 0 ) TEST_ERROR;

    /* verify object header belonging to group */
    if ( verify_tag(fid, H5AC_OHDR_ID, g_tag) < 0 ) TEST_ERROR;

    /* verify object header chunk belonging to group */
    if ( verify_tag(fid, H5AC_OHDR_CHK_ID, g_tag) < 0 ) TEST_ERROR;

    if ( type == TEST_SHMESG ) {

        /* verify (another) object header chunk belonging to group */
        if ( verify_tag(fid, H5AC_OHDR_CHK_ID, g_tag) < 0 ) TEST_ERROR;

        /* verify shared header message master table */
        if ( verify_tag(fid, H5AC_SOHM_TABLE_ID, H5AC__SOHM_TAG) < 0 ) TEST_ERROR;

        /* verify fractal heap header belonging to group */
        if ( verify_tag(fid, H5AC_FHEAP_HDR_ID, g_tag) < 0 ) TEST_ERROR;

        /* verify fractal heap direct block belonging to group */
        if ( verify_tag(fid, H5AC_FHEAP_DBLOCK_ID, g_tag) < 0 ) TEST_ERROR;

        /* verify shared header message stored as a list */
        if ( verify_tag(fid, H5AC_SOHM_LIST_ID, H5AC__SOHM_TAG) < 0 ) TEST_ERROR;

        /*
    * 3 calls to verify_tag() for verifying free space:
    *   one freespace header tag for H5FD_MEM_DRAW manager,
    *   one freespace header tag for H5FD_MEM_SUPER manager
    *   one freespace section info tag for H5FD_MEM_SUPER manager
         */
        if ( verify_tag(fid, H5AC_FSPACE_HDR_ID, H5AC__FREESPACE_TAG) < 0 ) TEST_ERROR;

        /* If the free space managers are persistent, the
         * H5MF_tidy_self_referential_fsm_hack() must have been run.
         * Since this function floats all self referential free space
         * managers, the H5FD_MEM_SUPER FSM will not be in the metadata
         * cache.
         */
        if(!persistent_fsms && verify_tag(fid, H5AC_FSPACE_HDR_ID, H5AC__FREESPACE_TAG) < 0) TEST_ERROR;
        if(!persistent_fsms && verify_tag(fid, H5AC_FSPACE_SINFO_ID, H5AC__FREESPACE_TAG) < 0) TEST_ERROR;

        /* verify btree header and leaf node belonging to group */
        if ( verify_tag(fid, H5AC_BT2_HDR_ID, g_tag) < 0 ) TEST_ERROR;
        if ( verify_tag(fid, H5AC_BT2_LEAF_ID, g_tag) < 0 ) TEST_ERROR;

    } /* end if */

    /* verify no other entries present */
    if ( verify_no_unknown_tags(fid) < 0 ) TEST_ERROR;

    /* Reset the changes we've made to the cache's data structures */
    if(reset_all_entries_investigated(fid) < 0) TEST_ERROR;

    /* =========================== */
    /* Close open objects and file */
    /* =========================== */

    if ( H5Gclose(gid) < 0 ) TEST_ERROR;
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;

    /* ========================================== */
    /* Finished Test. Print status and return. */
    /* ========================================== */

    HDfree(data);

    PASSED();
    return 0;

error:
    if(data)
        HDfree(data);
    return 1;
} /* check_attribute_rename_tags */


/*-------------------------------------------------------------------------
 * Function:    check_attribute_delete_tags
 *
 * Purpose:     This function verifies the correct application of tags
 *              during attribute deletion.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              March 3, 2010
 *
 *-------------------------------------------------------------------------
 */
static unsigned
check_attribute_delete_tags(hid_t fcpl, int type)
{
    /* Variable Declarations */
    hid_t fid = -1;                         /* File Identifier */
    hid_t gid = -1;                         /* Group Identifier */
    hid_t aid = -1;                         /* Attribute Identifier */
    hid_t sid = -1;                         /* Dataset Identifier */
    int verbose = FALSE;                    /* verbose file outout */
    int *data = NULL;                       /* data buffer */
    int i,j,k = 0;                          /* iterators */
    haddr_t root_tag = 0;
    haddr_t g_tag = 0;
    hsize_t dims1[2] = {DIMS, DIMS}; /* dimensions */
    hsize_t maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED}; /* dimensions */
    hbool_t persistent_fsms = FALSE;

    /* Testing Macro */
    TESTING("tag application during attribute delete");

    /* ===== */
    /* Setup */
    /* ===== */

    /* check to see if the FCPL specified persistent free space managers */
    if ( H5Pget_file_space_strategy(fcpl, NULL, &persistent_fsms, NULL) < 0 )
        TEST_ERROR;

    /* Allocate array */
    if ( (NULL == (data = (int *)HDcalloc(DIMS * DIMS, sizeof(int)))) ) TEST_ERROR;

    /* Create a test file with provided fcpl_t */
    if ( (fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* determine tag value of root group's object header */
    if ( get_object_header_tag(fid, &root_tag) < 0 ) TEST_ERROR;

    /* Create group */
    if ( (gid = H5Gcreate2(fid, GROUPNAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Retrieve group tag */
    if ( get_object_header_tag(gid, &g_tag) < 0 ) TEST_ERROR;

    /* Set up attribute dataspace */
    if ( (sid = H5Screate_simple(2, dims1, maxdims)) < 0 ) TEST_ERROR;

    /* Create attribute */
    if ( (aid = H5Acreate2(gid, ATTRNAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* fill out data buffer */
    for(i = 0;i < DIMS; i++)
        for(j = 0;j < DIMS; j++)
            data[(DIMS * i) + j] = k++;

    /* Write to attribute */
    if ( (H5Awrite(aid, H5T_NATIVE_INT, data)) < 0 ) TEST_ERROR;

    /* Close Attribute */
    if ( H5Aclose(aid) < 0 ) TEST_ERROR;

    /* Close and Reopen the file and group */
    if ( H5Gclose(gid) < 0 ) TEST_ERROR;
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;
    if ( (fid = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT)) < 0 ) TEST_ERROR;
    if ( (gid = H5Gopen2(fid, GROUPNAME, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Evict as much as we can from the cache so we can track full tag path */
    if ( evict_entries(fid) < 0 ) TEST_ERROR;

    /* ========================= */
    /* Delete Attribute of Group */
    /* ========================= */

    if ( (H5Adelete(gid, ATTRNAME)) < 0 ) TEST_ERROR;

    /* =================================== */
    /* Verification of Metadata Tag Values */
    /* =================================== */

#ifndef NDEBUG
    /* if verbose, print cache index to screen for visual verification */
    if ( verbose ) dump_cache(fid);
#endif /* NDEBUG */ /* end debugging functions */

    /* verify object header belonging to group */
    if ( verify_tag(fid, H5AC_OHDR_ID, g_tag) < 0 ) TEST_ERROR;

    if ( type == TEST_SHMESG ) {

        /* verify shared header message master table */
        if ( verify_tag(fid, H5AC_SOHM_TABLE_ID, H5AC__SOHM_TAG) < 0 ) TEST_ERROR;

        /*
         * 2 calls to verify_tag() for verifying free space:
         *   one freespace header tag for free-space header,
         *   one freespace header tag for free-space section info
         */
        if ( verify_tag(fid, H5AC_FSPACE_HDR_ID, H5AC__FREESPACE_TAG) < 0 )
            TEST_ERROR;
        if ( verify_tag(fid, H5AC_FSPACE_SINFO_ID, H5AC__FREESPACE_TAG) < 0 )
            TEST_ERROR;

#if 0
        /* If the free space managers are persistent, the
         * H5MF_tidy_self_referential_fsm_hack() must have been run.
         * Since this function floats all self referential free space
         * managers, the H5FD_MEM_SUPER FSM will not be in the metadata
         * cache.
         */
        if ( ( ! persistent_fsms ) &&
             ( verify_tag(fid, H5AC_FSPACE_HDR_ID, H5AC__FREESPACE_TAG) < 0 ) )
            TEST_ERROR;
#endif

    } /* end if */

    /* verify no other entries present */
    if ( verify_no_unknown_tags(fid) < 0 ) TEST_ERROR;

    /* Reset the changes we've made to the cache's data structures */
    if(reset_all_entries_investigated(fid) < 0) TEST_ERROR;

    /* =========================== */
    /* Close open objects and file */
    /* =========================== */

    if ( H5Gclose(gid) < 0 ) TEST_ERROR;
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;

    /* ========================================== */
    /* Finished Test. Print status and return. */
    /* ========================================== */

    HDfree(data);

    PASSED();
    return 0;

error:
    if(data)
        HDfree(data);
    return 1;
} /* check_attribute_delete_tags */


/*-------------------------------------------------------------------------
 * Function:    check_dataset_creation_tags
 *
 * Purpose:     This function verifies the correct application of tags
 *              during dataset creation.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              February 10, 2010
 *
 *-------------------------------------------------------------------------
 */
static unsigned
check_dataset_creation_tags(hid_t fcpl, int type)
{
    /* Variable Declarations */
    hid_t fid = -1;                         /* File Identifier */
    hid_t did = -1;                         /* Dataset Identifier */
    hid_t sid = -1;                         /* Dataspace Identifier */
    int verbose = FALSE;                    /* verbose file outout */
    hid_t dcpl = -1;                        /* dataset creation pl */
    hsize_t cdims[2] = {1,1};               /* chunk dimensions */
    int fillval = 0;
    haddr_t root_tag = 0;
    haddr_t d_tag = 0;
    hsize_t dims1[2] = {DIMS, DIMS}; /* dimensions */
    hsize_t maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED}; /* dimensions */

    /* Testing Macro */
    TESTING("tag application during dataset creation");

    /* ===== */
    /* Setup */
    /* ===== */

    if ( (fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* determine tag value of root group's object header */
    if ( get_object_header_tag(fid, &root_tag) < 0 ) TEST_ERROR;

    /* Close and Reopen the file */
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;
    if ( (fid = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Evict as much as we can from the cache so we can track full tag path */
    if ( evict_entries(fid) < 0 ) TEST_ERROR;

    /* ============================ */
    /* Create Dataset in Root Group */
    /* ============================ */

    /* Set up creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);

    /* Enable chunking */
    if ( H5Pset_chunk(dcpl, RANK, cdims) < 0 ) TEST_ERROR;

    /* Set up a fill value */
    if ( H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval) < 0 ) TEST_ERROR;

    /* Set up dataset dataspace */
    if ( (sid = H5Screate_simple(2, dims1, maxdims)) < 0 ) TEST_ERROR;

    /* Create Dataset */
    if (( did = H5Dcreate2(fid, DATASETNAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0 ) TEST_ERROR;
    if ( H5Pclose(dcpl) < 0 ) TEST_ERROR;

    /* =================================== */
    /* Verification of Metadata Tag Values */
    /* =================================== */

#ifndef NDEBUG
    /* if verbose, print cache index to screen for visual verification */
    if ( verbose ) dump_cache(fid);
#endif /* NDEBUG */ /* end debugging functions */

    /* Verify root group metadata */
    if ( verify_tag(fid, H5AC_OHDR_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_SNODE_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_LHEAP_PRFX_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_BT_ID, root_tag) < 0 ) TEST_ERROR;

    /* Get dataset's object header address */
    if ( get_object_header_tag(did, &d_tag) < 0 ) TEST_ERROR;

    /* Verify object header for group */
    if ( verify_tag(fid, H5AC_OHDR_ID, d_tag) < 0 ) TEST_ERROR;

    if ( type == TEST_SHMESG ) {

        /* verify shared header message master table */
        if ( verify_tag(fid, H5AC_SOHM_TABLE_ID, H5AC__SOHM_TAG) < 0 ) TEST_ERROR;

        /* Verify dataset's tagged metadata */
        if ( verify_tag(fid, H5AC_FHEAP_HDR_ID, H5AC__SOHM_TAG) < 0 ) TEST_ERROR;

        /* Verify shared object header message tags */
        if ( verify_tag(fid, H5AC_SOHM_LIST_ID, H5AC__SOHM_TAG) < 0 ) TEST_ERROR;

    } /* end if */

    /* verify no other entries present */
    if ( verify_no_unknown_tags(fid) < 0 ) TEST_ERROR;

    /* Reset the changes we've made to the cache's data structures */
    if(reset_all_entries_investigated(fid) < 0) TEST_ERROR;

    /* =========================== */
    /* Close open objects and file */
    /* =========================== */

    if ( H5Dclose(did) < 0 ) TEST_ERROR;
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;

    /* ========================================== */
    /* Finished Test. Print status and return. */
    /* ========================================== */

    PASSED();
    return 0;

error:
    return 1;
} /* check_dataset_creation_tags */


/*-------------------------------------------------------------------------
 * Function:    check_dataset_creation_earlyalloc_tags
 *
 * Purpose:     This function verifies the correct application of tags
 *              during dataset creation.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              March 1, 2010
 *
 *-------------------------------------------------------------------------
 */
static unsigned
check_dataset_creation_earlyalloc_tags(hid_t fcpl, int type)
{
    /* Variable Declarations */
    hid_t fid = -1;                         /* File Identifier */
    hid_t did = -1;                         /* Dataset Identifier */
    hid_t sid = -1;                         /* Dataspace Identifier */
    int verbose = FALSE;                    /* verbose file outout */
    hid_t dcpl = -1;                        /* dataset creation pl */
    hsize_t cdims[2] = {1,1};               /* chunk dimensions */
    int fillval = 0;
    haddr_t root_tag = 0;
    haddr_t d_tag = 0;
    hsize_t dims1[2] = {DIMS, DIMS}; /* dimensions */
    hsize_t maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED}; /* dimensions */
    int i = 0;           /* iterator */

    /* Testing Macro */
    TESTING("tag application during dataset creation with early allocation");

    /* ===== */
    /* Setup */
    /* ===== */

    if ( (fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* determine tag value of root group's object header */
    if ( get_object_header_tag(fid, &root_tag) < 0 ) TEST_ERROR;

    /* Close and Reopen the file */
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;
    if ( (fid = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Evict as much as we can from the cache so we can track full tag path */
    if ( evict_entries(fid) < 0 ) TEST_ERROR;

    /* ============================ */
    /* Create Dataset in Root Group */
    /* ============================ */

    dcpl = H5Pcreate(H5P_DATASET_CREATE);

    if ( H5Pset_chunk(dcpl, RANK, cdims) < 0 ) TEST_ERROR;

    if ( H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval) < 0 ) TEST_ERROR;

    /* Set early allocation time */
    if ( H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0 ) TEST_ERROR;

    /* Set up dataset dataspace */
    if ( (sid = H5Screate_simple(2, dims1, maxdims)) < 0 ) TEST_ERROR;

    if (( did = H5Dcreate2(fid, DATASETNAME2, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0 ) TEST_ERROR;
    if ( H5Pclose(dcpl) < 0 ) TEST_ERROR;

    /* =================================== */
    /* Verification of Metadata Tag Values */
    /* =================================== */

#ifndef NDEBUG
    /* if verbose, print cache index to screen for visual verification */
    if ( verbose ) dump_cache(fid);
#endif /* NDEBUG */ /* end debugging functions */

    /* Verify root group metadata */
    if ( verify_tag(fid, H5AC_OHDR_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_SNODE_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_LHEAP_PRFX_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_BT_ID, root_tag) < 0 ) TEST_ERROR;

    /* Get dataset's object header address */
    if ( get_object_header_tag(did, &d_tag) < 0 ) TEST_ERROR;

    /* Verify object header for group */
    if ( verify_tag(fid, H5AC_OHDR_ID, d_tag) < 0 ) TEST_ERROR;

    if ( type == TEST_SHMESG ) {

        /* verify shared header message master table */
        if ( verify_tag(fid, H5AC_SOHM_TABLE_ID, H5AC__SOHM_TAG) < 0 ) TEST_ERROR;

        /* Verify dataset's tagged metadata */
        if ( verify_tag(fid, H5AC_FHEAP_HDR_ID, H5AC__SOHM_TAG) < 0 ) TEST_ERROR;

        /* Verify shared object header message tags */
        if ( verify_tag(fid, H5AC_SOHM_LIST_ID, H5AC__SOHM_TAG) < 0 ) TEST_ERROR;

    } /* end if */

    /* Verify 19 b-tree nodes belonging to dataset  */
    for (i=0; i<19; i++)
        if ( verify_tag(fid, H5AC_BT_ID, d_tag) < 0 ) TEST_ERROR;

    /* verify no other entries present */
    if ( verify_no_unknown_tags(fid) < 0 ) TEST_ERROR;

    /* Reset the changes we've made to the cache's data structures */
    if(reset_all_entries_investigated(fid) < 0) TEST_ERROR;

    /* =========================== */
    /* Close open objects and file */
    /* =========================== */

    if ( H5Dclose(did) < 0 ) TEST_ERROR;
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;

    /* ========================================== */
    /* Finished Test. Print status and return. */
    /* ========================================== */

    PASSED();
    return 0;

error:
    return 1;
} /* check_dataset_creation_earlyalloc_tags */


/*-------------------------------------------------------------------------
 * Function:    check_dataset_open_tags
 *
 * Purpose:     This function verifies the correct application of tags
 *              during dataset open.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              February 10, 2010
 *
 *-------------------------------------------------------------------------
 */
static unsigned
check_dataset_open_tags(void)
{
    /* Variable Declarations */
    hid_t fid = -1;                         /* File Identifier */
    hid_t did = -1;                         /* Dataset Identifier */
    hid_t sid = -1;                         /* Dataspace Identifier */
    int verbose = FALSE;                    /* verbose file outout */
    hid_t dcpl = -1;                        /* dataset creation pl */
    hsize_t cdims[2] = {1,1};               /* chunk dimensions */
    int fillval = 0;
    haddr_t root_tag = 0;
    haddr_t d_tag = 0;
    hsize_t dims1[2] = {DIMS, DIMS}; /* dimensions */
    hsize_t maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED}; /* dimensions */

    /* Testing Macro */
    TESTING("tag application during dataset open");

    /* ========= */
    /* Open File */
    /* ========= */

    /* Create file */
    if ( (fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* determine tag value of root group's object header */
    if ( get_object_header_tag(fid, &root_tag) < 0 ) TEST_ERROR;

    /* Set up creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);

    /* Enable chunking */
    if ( H5Pset_chunk(dcpl, RANK, cdims) < 0 ) TEST_ERROR;

    /* Set up a fill value */
    if ( H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval) < 0 ) TEST_ERROR;

    /* Set up dataset dataspace */
    if ( (sid = H5Screate_simple(2, dims1, maxdims)) < 0 ) TEST_ERROR;

    /* Create Dataset */
    if (( did = H5Dcreate2(fid, DATASETNAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0 ) TEST_ERROR;
    if ( H5Pclose(dcpl) < 0 ) TEST_ERROR;

    /* Retrieve tag associated with this dataset */
    if ( get_object_header_tag(did, &d_tag) < 0 ) TEST_ERROR;

    /* Close Dataset */
    if (H5Dclose(did) < 0 ) TEST_ERROR;

    /* Close and Reopen the file */
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;
    if ( (fid = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Evict as much as we can from the cache so we can track full tag path */
    if ( evict_entries(fid) < 0 ) TEST_ERROR;

    /* ========================== */
    /* Open Dataset in Root Group */
    /* ========================== */

    if (( did = H5Dopen2(fid, DATASETNAME, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* =================================== */
    /* Verification of Metadata Tag Values */
    /* =================================== */

#ifndef NDEBUG
    /* if verbose, print cache index to screen for visual verification */
    if ( verbose ) dump_cache(fid);
#endif /* NDEBUG */ /* end debugging functions */

    /* Verify root group metadata */
    if ( verify_tag(fid, H5AC_OHDR_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_SNODE_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_LHEAP_PRFX_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_BT_ID, root_tag) < 0 ) TEST_ERROR;

    /* Verify dataset's object header */
    if ( verify_tag(fid, H5AC_OHDR_ID, d_tag) < 0 ) TEST_ERROR;

    /* verify no other entries present */
    if ( verify_no_unknown_tags(fid) < 0 ) TEST_ERROR;

    /* Reset the changes we've made to the cache's data structures */
    if(reset_all_entries_investigated(fid) < 0) TEST_ERROR;

    /* =========================== */
    /* Close open objects and file */
    /* =========================== */

    if ( H5Dclose(did) < 0 ) TEST_ERROR;
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;

    /* ========================================== */
    /* Finished Test. Print status and return. */
    /* ========================================== */

    PASSED();
    return 0;

error:
    return 1;
} /* check_dataset_open_tags */


/*-------------------------------------------------------------------------
 * Function:    check_dataset_write_tags
 *
 * Purpose:     This function verifies the correct application of tags
 *              during dataset write.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              February 10, 2010
 *
 *-------------------------------------------------------------------------
 */
static unsigned
check_dataset_write_tags(void)
{
    /* Variable Declarations */
    hid_t fid = -1;                         /* File Identifier */
    hid_t did = -1;                         /* Dataset Identifier */
    hid_t sid = -1;                         /* Dataspace Identifier */
    int verbose = FALSE;                    /* verbose file outout */
    hid_t dcpl = -1;                        /* dataset creation pl */
    hsize_t cdims[2] = {1,1};               /* chunk dimensions */
    int fillval = 0;
    haddr_t root_tag = 0;
    haddr_t d_tag = 0;
    hsize_t dims1[2] = {DIMS, DIMS};        /* dimensions */
    hsize_t maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED}; /* dimensions */
    int i,j,k = 0;                          /* iterators */
    int *data = NULL;                       /* data buffer */

    /* Testing Macro */
    TESTING("tag application during dataset write");

    /* ===== */
    /* Setup */
    /* ===== */

    /* Allocate array */
    if ( (NULL == (data = (int *)HDcalloc(DIMS * DIMS, sizeof(int)))) ) TEST_ERROR;

    /* Create file */
    if ( (fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* determine tag value of root group's object header */
    if ( get_object_header_tag(fid, &root_tag) < 0 ) TEST_ERROR;

    /* Set up creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);

    /* Enable chunking */
    if ( H5Pset_chunk(dcpl, RANK, cdims) < 0 ) TEST_ERROR;

    /* Set up a fill value */
    if ( H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval) < 0 ) TEST_ERROR;

    /* Set up dataset dataspace */
    if ( (sid = H5Screate_simple(2, dims1, maxdims)) < 0 ) TEST_ERROR;

    /* Create Dataset */
    if (( did = H5Dcreate2(fid, DATASETNAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0 ) TEST_ERROR;
    if ( H5Pclose(dcpl) < 0 ) TEST_ERROR;

    /* Retrieve tag associated with this dataset */
    if ( get_object_header_tag(did, &d_tag) < 0 ) TEST_ERROR;

    /* Close and Reopen the file and dataset */
    if ( H5Dclose(did) < 0 ) TEST_ERROR;
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;
    if ( (fid = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT)) < 0 ) TEST_ERROR;
    if (( did = H5Dopen2(fid, DATASETNAME, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Evict as much as we can from the cache so we can track full tag path */
    if ( evict_entries(fid) < 0 ) TEST_ERROR;

    /* ============================== */
    /* Write to Dataset in Root Group */
    /* ============================== */

    /* fill out data buffer */
    for(i = 0;i < DIMS; i++) {
        for(j = 0;j < DIMS; j++) {

            data[(DIMS *i) + j] = k++;
        } /* end for */
    } /* end for */

    /* Write to dataset */
    if( (H5Dwrite(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, data)) < 0 ) TEST_ERROR;

    /* =================================== */
    /* Verification of Metadata Tag Values */
    /* =================================== */

#ifndef NDEBUG
    /* if verbose, print cache index to screen for visual verification */
    if ( verbose ) dump_cache(fid);
#endif /* NDEBUG */ /* end debugging functions */

    /* Verify 10 b-tree nodes belonging to dataset  */
    for (i=0; i<10; i++)
        if ( verify_tag(fid, H5AC_BT_ID, d_tag) < 0 ) TEST_ERROR;

    /* Verify object header for dataset */
    if ( verify_tag(fid, H5AC_OHDR_ID, d_tag) < 0 ) TEST_ERROR;

    /* verify no other entries present */
    if ( verify_no_unknown_tags(fid) < 0 ) TEST_ERROR;

    /* Reset the changes we've made to the cache's data structures */
    if(reset_all_entries_investigated(fid) < 0) TEST_ERROR;

    /* =========================== */
    /* Close open objects and file */
    /* =========================== */

    if ( H5Dclose(did) < 0 ) TEST_ERROR;
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;

    /* ========================================== */
    /* Finished Test. Print status and return. */
    /* ========================================== */

    HDfree(data);

    PASSED();
    return 0;

error:
    if(data)
        HDfree(data);
    return 1;
} /* check_dataset_write_tags */


/*-------------------------------------------------------------------------
 * Function:    check_attribute_write_tags
 *
 * Purpose:     This function verifies the correct application of tags
 *              during attribute write.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              March 3, 2010
 *
 *-------------------------------------------------------------------------
 */
static unsigned
check_attribute_write_tags(hid_t fcpl, int type)
{
    /* Variable Declarations */
    hid_t fid = -1;                         /* File Identifier */
    hid_t gid = -1;                         /* Group Identifier */
    hid_t aid = -1;                         /* Attribute Identifier */
    hid_t sid = -1;                         /* Dataset Identifier */
    int verbose = FALSE;                    /* verbose file outout */
    int *data = NULL;                       /* data buffer */
    int i,j,k = 0;                          /* iterators */
    haddr_t root_tag = 0;
    haddr_t g_tag = 0;
    hsize_t dims1[2] = {DIMS, DIMS}; /* dimensions */
    hsize_t maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED}; /* dimensions */

    /* Testing Macro */
    TESTING("tag application during attribute write");

    /* ===== */
    /* Setup */
    /* ===== */

    /* Allocate array */
    if ( (NULL == (data = (int *)HDcalloc(DIMS * DIMS, sizeof(int)))) ) TEST_ERROR;

    /* Create a test file with provided fcpl_t */
    if ( (fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* determine tag value of root group's object header */
    if ( get_object_header_tag(fid, &root_tag) < 0 ) TEST_ERROR;

    /* Create group */
    if ( (gid = H5Gcreate2(fid, GROUPNAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Retrieve group tag */
    if ( get_object_header_tag(gid, &g_tag) < 0 ) TEST_ERROR;

    /* Create attribute dataspace */
    if ( (sid = H5Screate_simple(2, dims1, maxdims)) < 0 ) TEST_ERROR;

    /* Create attribute on group */
    if ( (aid = H5Acreate2(gid, ATTRNAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Close and Reopen the file, group, and attribute */
    if ( H5Aclose(aid) < 0 ) TEST_ERROR;
    if ( H5Gclose(gid) < 0 ) TEST_ERROR;
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;
    if ( (fid = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT)) < 0 ) TEST_ERROR;
    if ( (gid = H5Gopen2(fid, GROUPNAME, H5P_DEFAULT)) < 0 ) TEST_ERROR;
    if ( (aid = H5Aopen(gid, ATTRNAME, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Evict as much as we can from the cache so we can track full tag path */
    if ( evict_entries(fid) < 0 ) TEST_ERROR;

    /* =========================== */
    /* Write to Attribute in Group */
    /* =========================== */

    /* fill out data buffer */
    for(i = 0; i < DIMS; i++)
        for(j = 0;j < DIMS; j++)
            data[(DIMS * i) + j] = k++;

    /* Write attribute */
    if ( (H5Awrite(aid, H5T_NATIVE_INT, data)) < 0 ) TEST_ERROR;

    /* =================================== */
    /* Verification of Metadata Tag Values */
    /* =================================== */

#ifndef NDEBUG
    /* if verbose, print cache index to screen for visual verification */
    if ( verbose ) dump_cache(fid);
#endif /* NDEBUG */ /* end debugging functions */

    /* Verify object header of group */
    if ( verify_tag(fid, H5AC_OHDR_ID, g_tag) < 0 ) TEST_ERROR;

    /* verify object header chunk belonging to group */
    if ( verify_tag(fid, H5AC_OHDR_CHK_ID, g_tag) < 0 ) TEST_ERROR;

    if ( type == TEST_SHMESG ) {

        /* verify (another) object header chunk belonging to group */
        if ( verify_tag(fid, H5AC_OHDR_CHK_ID, g_tag) < 0 ) TEST_ERROR;

        /* verify shared header message master table and list */
        if ( verify_tag(fid, H5AC_SOHM_TABLE_ID, H5AC__SOHM_TAG) < 0 ) TEST_ERROR;

        /* verify fractal heap header belonging to group */
        if ( verify_tag(fid, H5AC_FHEAP_HDR_ID, g_tag) < 0 ) TEST_ERROR;

        /* verify fractal heap direct block belonging to group */
        if ( verify_tag(fid, H5AC_FHEAP_DBLOCK_ID, g_tag) < 0 ) TEST_ERROR;

        /* Verify SOHM list */
        if ( verify_tag(fid, H5AC_SOHM_LIST_ID, H5AC__SOHM_TAG) < 0 ) TEST_ERROR;

        /* verify btree header and leaf node belonging to group */
        if ( verify_tag(fid, H5AC_BT2_HDR_ID, g_tag) < 0 ) TEST_ERROR;
        if ( verify_tag(fid, H5AC_BT2_LEAF_ID, g_tag) < 0 ) TEST_ERROR;

    } /* end if */

    /* verify no other entries present */
    if ( verify_no_unknown_tags(fid) < 0 ) TEST_ERROR;

    /* Reset the changes we've made to the cache's data structures */
    if(reset_all_entries_investigated(fid) < 0) TEST_ERROR;

    /* =========================== */
    /* Close open objects and file */
    /* =========================== */

    if ( H5Aclose(aid) < 0 ) TEST_ERROR;
    if ( H5Gclose(gid) < 0 ) TEST_ERROR;
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;

    /* ========================================== */
    /* Finished Test. Print status and return. */
    /* ========================================== */

    HDfree(data);

    PASSED();
    return 0;

error:
    if(data)
        HDfree(data);
    return 1;
} /* check_attribute_write_tags */


/*-------------------------------------------------------------------------
 * Function:    check_dataset_read_tags
 *
 * Purpose:     This function verifies the correct application of tags
 *              during dataset read.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              February 10, 2010
 *
 *-------------------------------------------------------------------------
 */
static unsigned
check_dataset_read_tags(void)
{
    /* Variable Declarations */
    hid_t fid = -1;                         /* File Identifier */
    hid_t did = -1;                         /* Dataset Identifier */
    hid_t sid = -1;                         /* Dataspace Identifier */
    int verbose = FALSE;                    /* verbose file outout */
    hid_t dcpl = -1;                        /* dataset creation pl */
    hsize_t cdims[2] = {1,1};               /* chunk dimensions */
    int fillval = 0;
    haddr_t root_tag = 0;
    haddr_t d_tag = 0;
    hsize_t dims1[2] = {DIMS, DIMS};        /* dimensions */
    hsize_t maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED}; /* dimensions */
    int i,j,k = 0;                          /* iterators */
    int *data = NULL;                       /* data buffer */

    /* Testing Macro */
    TESTING("tag application during dataset read");

    /* ===== */
    /* Setup */
    /* ===== */

    /* Allocate array */
    if ( (NULL == (data = (int *)HDcalloc(DIMS * DIMS, sizeof(int)))) ) TEST_ERROR;

    /* Create file */
    if ( (fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* determine tag value of root group's object header */
    if ( get_object_header_tag(fid, &root_tag) < 0 ) TEST_ERROR;

    /* Set up creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);

    /* Enable chunking */
    if ( H5Pset_chunk(dcpl, RANK, cdims) < 0 ) TEST_ERROR;

    /* Set up a fill value */
    if ( H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval) < 0 ) TEST_ERROR;

    /* Set up dataset dataspace */
    if ( (sid = H5Screate_simple(2, dims1, maxdims)) < 0 ) TEST_ERROR;

    /* Create Dataset */
    if (( did = H5Dcreate2(fid, DATASETNAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0 ) TEST_ERROR;
    if ( H5Pclose(dcpl) < 0 ) TEST_ERROR;

    /* Retrieve tag associated with this dataset */
    if ( get_object_header_tag(did, &d_tag) < 0 ) TEST_ERROR;

    /* fill out data buffer */
    for(i = 0; i < DIMS; i++)
        for(j = 0; j < DIMS; j++)
            data[(DIMS * i) + j] = k++;

    /* Write to dataset */
    if( (H5Dwrite(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, data)) < 0 ) TEST_ERROR;

    /* Close and Reopen the file and dataset */
    if ( H5Dclose(did) < 0 ) TEST_ERROR;
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;
    if ( (fid = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT)) < 0 ) TEST_ERROR;
    if (( did = H5Dopen2(fid, DATASETNAME, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Evict as much as we can from the cache so we can track full tag path */
    if ( evict_entries(fid) < 0 ) TEST_ERROR;

    /* ===================================== */
    /* TEST: Read from Dataset in Root Group */
    /* ===================================== */

    if( (H5Dread(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, data)) < 0 ) TEST_ERROR;

    /* =================================== */
    /* Verification of Metadata Tag Values */
    /* =================================== */

#ifndef NDEBUG
    /* if verbose, print cache index to screen for visual verification */
    if ( verbose ) dump_cache(fid);
#endif /* NDEBUG */ /* end debugging functions */

    /* Verify 19 b-tree nodes belonging to dataset  */
    for (i=0; i<19; i++)
        if ( verify_tag(fid, H5AC_BT_ID, d_tag) < 0 ) TEST_ERROR;

    /* verify no other entries present */
    if ( verify_no_unknown_tags(fid) < 0 ) TEST_ERROR;

    /* Reset the changes we've made to the cache's data structures */
    if(reset_all_entries_investigated(fid) < 0) TEST_ERROR;

    /* =========================== */
    /* Close open objects and file */
    /* =========================== */

    if ( H5Dclose(did) < 0 ) TEST_ERROR;
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;

    /* ========================================== */
    /* Finished Test. Print status and return. */
    /* ========================================== */

    HDfree(data);

    PASSED();
    return 0;

error:
    if(data)
        HDfree(data);
    return 1;
} /* check_dataset_read_tags */


/*-------------------------------------------------------------------------
 * Function:    check_dataset_size_retrieval
 *
 * Purpose:     This function verifies the correct application of tags
 *              during dataset size retrieval.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              February 24, 2010
 *
 *-------------------------------------------------------------------------
 */
static unsigned
check_dataset_size_retrieval(void)
{
    /* Variable Declarations */
    hid_t fid = -1;                         /* File Identifier */
    hid_t did = -1;                         /* Dataset Identifier */
    hid_t sid = -1;                         /* Dataspace Identifier */
    int verbose = FALSE;                    /* verbose file outout */
    hid_t dcpl = -1;                        /* dataset creation pl */
    hsize_t cdims[2] = {1,1};               /* chunk dimensions */
    int fillval = 0;
    haddr_t root_tag = 0;
    haddr_t d_tag = 0;
    hsize_t dims1[2] = {DIMS, DIMS};        /* dimensions */
    hsize_t maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED}; /* dimensions */
    int i,j,k = 0;                          /* iterators */
    int *data = NULL;                       /* data buffer */
    hsize_t dsize = 0;

    /* Testing Macro */
    TESTING("tag application during dataset storage size retrieval");

    /* ===== */
    /* Setup */
    /* ===== */

    /* Allocate array */
    if ( (NULL == (data = (int *)HDcalloc(DIMS * DIMS, sizeof(int)))) ) TEST_ERROR;

    /* Create file */
    if ( (fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* determine tag value of root group's object header */
    if ( get_object_header_tag(fid, &root_tag) < 0 ) TEST_ERROR;

    /* Set up creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);

    /* Enable chunking */
    if ( H5Pset_chunk(dcpl, RANK, cdims) < 0 ) TEST_ERROR;

    /* Set up a fill value */
    if ( H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval) < 0 ) TEST_ERROR;

    /* Set up dataset dataspace */
    if ( (sid = H5Screate_simple(2, dims1, maxdims)) < 0 ) TEST_ERROR;

    /* Create Dataset */
    if (( did = H5Dcreate2(fid, DATASETNAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0 ) TEST_ERROR;
    if ( H5Pclose(dcpl) < 0 ) TEST_ERROR;

    /* Retrieve tag associated with this dataset */
    if ( get_object_header_tag(did, &d_tag) < 0 ) TEST_ERROR;

    /* fill out data buffer */
    for(i = 0; i < DIMS; i++)
        for(j = 0; j < DIMS; j++)
            data[(DIMS * i) + j] = k++;

    /* Write to dataset */
    if( (H5Dwrite(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, data)) < 0 ) TEST_ERROR;

    /* Close and Reopen the file and dataset */
    if ( H5Dclose(did) < 0 ) TEST_ERROR;
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;
    if ( (fid = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT)) < 0 ) TEST_ERROR;
    if (( did = H5Dopen2(fid, DATASETNAME, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Evict as much as we can from the cache so we can track full tag path */
    if ( evict_entries(fid) < 0 ) TEST_ERROR;

    /* ========================================= */
    /* Get storage size of dataset in Root Group */
    /* ========================================= */

    if ( (dsize = H5Dget_storage_size(did)) == 0 ) TEST_ERROR;

    /* =================================== */
    /* Verification of Metadata Tag Values */
    /* =================================== */

#ifndef NDEBUG
    /* if verbose, print cache index to screen for visual verification */
    if ( verbose ) dump_cache(fid);
#endif /* NDEBUG */ /* end debugging functions */

    /* Verify 19 b-tree nodes belonging to dataset  */
    for (i=0; i<19; i++)
        if ( verify_tag(fid, H5AC_BT_ID, d_tag) < 0 ) TEST_ERROR;

    /* verify no other entries present */
    if ( verify_no_unknown_tags(fid) < 0 ) TEST_ERROR;

    /* Reset the changes we've made to the cache's data structures */
    if(reset_all_entries_investigated(fid) < 0) TEST_ERROR;

    /* =========================== */
    /* Close open objects and file */
    /* =========================== */

    if ( H5Dclose(did) < 0 ) TEST_ERROR;
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;

    /* ========================================== */
    /* Finished Test. Print status and return. */
    /* ========================================== */

    HDfree(data);

    PASSED();
    return 0;

error:
    if(data)
        HDfree(data);
    return 1;
} /* check_dataset_size_retrieval */


/*-------------------------------------------------------------------------
 * Function:    check_dataset_extend_tags
 *
 * Purpose:     This function verifies the correct application of tags
 *              during dataset extension.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              February 24, 2010
 *
 *-------------------------------------------------------------------------
 */
static unsigned
check_dataset_extend_tags(void)
{

    /* Variable Declarations */
    hid_t fid = -1;                         /* File Identifier */
    hid_t did = -1;                         /* Dataset Identifier */
    hid_t sid = -1;                         /* Dataspace Identifier */
    int verbose = FALSE;                    /* verbose file outout */
    hid_t dcpl = -1;                        /* dataset creation pl */
    hsize_t cdims[2] = {1,1};               /* chunk dimensions */
    int fillval = 0;
    haddr_t root_tag = 0;
    haddr_t d_tag = 0;
    hsize_t dims1[2] = {DIMS, DIMS};        /* dimensions */
    hsize_t maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED}; /* dimensions */
    int i,j,k = 0;                          /* iterators */
    int *data = NULL;                       /* data buffer */
    hsize_t newdims[2] = {DIMS*2, DIMS};    /* dimensions */

    /* Testing Macro */
    TESTING("tag application during dataset extend");

    /* ===== */
    /* Setup */
    /* ===== */

    /* Allocate array */
    if ( (NULL == (data = (int *)HDcalloc(DIMS * DIMS, sizeof(int)))) ) TEST_ERROR;

    /* Create file */
    if ( (fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* determine tag value of root group's object header */
    if ( get_object_header_tag(fid, &root_tag) < 0 ) TEST_ERROR;

    /* Set up creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);

    /* Enable chunking */
    if ( H5Pset_chunk(dcpl, RANK, cdims) < 0 ) TEST_ERROR;

    /* Set up a fill value */
    if ( H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval) < 0 ) TEST_ERROR;

    /* Set up dataset dataspace */
    if ( (sid = H5Screate_simple(2, dims1, maxdims)) < 0 ) TEST_ERROR;

    /* Create Dataset */
    if (( did = H5Dcreate2(fid, DATASETNAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0 ) TEST_ERROR;
    if ( H5Pclose(dcpl) < 0 ) TEST_ERROR;

    /* Retrieve tag associated with this dataset */
    if ( get_object_header_tag(did, &d_tag) < 0 ) TEST_ERROR;

    /* fill out data buffer */
    for(i = 0; i < DIMS; i++)
        for(j = 0; j < DIMS; j++)
            data[(DIMS * i) + j] = k++;

    /* Write to dataset */
    if( (H5Dwrite(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, data)) < 0 ) TEST_ERROR;

    /* Close and Reopen the file and dataset */
    if ( H5Dclose(did) < 0 ) TEST_ERROR;
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;
    if ( (fid = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT)) < 0 ) TEST_ERROR;
    if (( did = H5Dopen2(fid, DATASETNAME, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Evict as much as we can from the cache so we can track full tag path */
    if ( evict_entries(fid) < 0 ) TEST_ERROR;

    /* ================== */
    /* Set Dataset extent */
    /* ================== */

    if ( H5Dset_extent(did, newdims) < 0 ) TEST_ERROR;

    if ( H5Dclose(did) < 0 ) TEST_ERROR;

    /* =================================== */
    /* Verification of Metadata Tag Values */
    /* =================================== */

#ifndef NDEBUG
    /* if verbose, print cache index to screen for visual verification */
    if ( verbose ) dump_cache(fid);
#endif /* NDEBUG */ /* end debugging functions */

    /* Verify root group metadata */
    if ( verify_tag(fid, H5AC_OHDR_ID, d_tag) < 0 ) TEST_ERROR;

    /* verify no other entries present */
    if ( verify_no_unknown_tags(fid) < 0 ) TEST_ERROR;

    /* Reset the changes we've made to the cache's data structures */
    if(reset_all_entries_investigated(fid) < 0) TEST_ERROR;

    /* =========================== */
    /* Close open objects and file */
    /* =========================== */

    if ( H5Fclose(fid) < 0 ) TEST_ERROR;

    /* ========================================== */
    /* Finished Test. Print status and return. */
    /* ========================================== */

    HDfree(data);

    PASSED();
    return 0;

error:
    if(data)
        HDfree(data);
    return 1;
} /* check_dataset_extend_tags */


/*-------------------------------------------------------------------------
 * Function:    check_object_info_tags
 *
 * Purpose:     This function verifies the correct application of tags
 *              during object information retrieval.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              March 1, 2010
 *
 *-------------------------------------------------------------------------
 */
static unsigned
check_object_info_tags(void)
{
    /* Variable Declarations */
    hid_t fid = -1;          /* File Identifier */
    hid_t gid = -1;          /* Group Identifier */
    int verbose = FALSE;     /* verbose file output */
    haddr_t root_tag = HADDR_UNDEF;
    haddr_t g_tag;
    H5O_info_t oinfo;                       /* Object info struct */

    /* Testing Macro */
    TESTING("tag application during object info retrieval");

    /* ===== */
    /* Setup */
    /* ===== */

    /* Create a test file */
    if ( (fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* determine tag value of root group's object header */
    if ( get_object_header_tag(fid, &root_tag) < 0 ) TEST_ERROR;

    /* Create group */
    if ( (gid = H5Gcreate2(fid, GROUPNAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Retrieve group tag */
    if ( get_object_header_tag(gid, &g_tag) < 0 ) TEST_ERROR;

    /* Close Group */
    if (H5Gclose(gid) < 0) TEST_ERROR;

    /* Close and Reopen the file */
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;
    if ( (fid = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Evict as much as we can from the cache so we can track full tag path */
    if ( evict_entries(fid) < 0 ) TEST_ERROR;

    /* ===================================== */
    /* Get information on an object by name  */
    /* ===================================== */

    if ( H5Oget_info_by_name2(fid, GROUPNAME, &oinfo, H5O_INFO_ALL, H5P_DEFAULT) < 0 ) TEST_ERROR;

    /* =================================== */
    /* Verification of Metadata Tag Values */
    /* =================================== */

#ifndef NDEBUG
    /* if verbose, print cache index to screen for visual verification */
    if ( verbose ) dump_cache(fid);
#endif /* NDEBUG */ /* end debugging functions */

    /* Verify root group's tagged metadata */
    if ( verify_tag(fid, H5AC_OHDR_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_SNODE_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_LHEAP_PRFX_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_BT_ID, root_tag) < 0 ) TEST_ERROR;

    /* Verify dataset's tagged metadata */
    if ( verify_tag(fid, H5AC_OHDR_ID, g_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_BT_ID, g_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_LHEAP_PRFX_ID, g_tag) < 0 ) TEST_ERROR;

    /* verify no other entries present */
    if ( verify_no_unknown_tags(fid) < 0 ) TEST_ERROR;

    /* Reset the changes we've made to the cache's data structures */
    if(reset_all_entries_investigated(fid) < 0) TEST_ERROR;

    /* =========================== */
    /* Close open objects and file */
    /* =========================== */

    if ( H5Fclose(fid) < 0 ) TEST_ERROR;

    /* ========================================== */
    /* Finished Test. Print status and return. */
    /* ========================================== */

    PASSED();
    return 0;

error:
    return 1;
} /* check_object_info_tags */


/*-------------------------------------------------------------------------
 * Function:    check_object_copy_tags
 *
 * Purpose:     This function verifies the correct application of tags
 *              during object copy.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              March 3, 2010
 *
 *-------------------------------------------------------------------------
 */
static unsigned
check_object_copy_tags(void)
{
    /* Variable Declarations */
    hid_t fid = -1;                         /* File Identifier */
    hid_t gid = -1;                         /* Group Identifier */
    int verbose = FALSE;     /* verbose file output */
    haddr_t root_tag = HADDR_UNDEF;
    haddr_t g_tag;
    haddr_t copy_tag;

    /* Testing Macro */
    TESTING("tag application during object copy");

    /* ===== */
    /* Setup */
    /* ===== */

    /* Create a test file */
    if ( (fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* determine tag value of root group's object header */
    if ( get_object_header_tag(fid, &root_tag) < 0 ) TEST_ERROR;

    /* Create group */
    if ( (gid = H5Gcreate2(fid, GROUPNAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Retrieve group tag */
    if ( get_object_header_tag(gid, &g_tag) < 0 ) TEST_ERROR;

    /* Close Group */
    if (H5Gclose(gid) < 0) TEST_ERROR;

    /* Close and Reopen the file */
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;
    if ( (fid = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Evict as much as we can from the cache so we can track full tag path */
    if ( evict_entries(fid) < 0 ) TEST_ERROR;

    /* =========== */
    /* Copy Group */
    /* =========== */

    if ( H5Ocopy(fid, GROUPNAME, fid, GROUPNAMECOPY, H5P_DEFAULT, H5P_DEFAULT) < 0) TEST_ERROR;

    /* Get tag for copied group */
    if ( (gid = H5Gopen2(fid, GROUPNAMECOPY, H5P_DEFAULT)) < 0 ) TEST_ERROR;
    if ( get_object_header_tag(gid, &copy_tag) < 0 ) TEST_ERROR;
    if (H5Gclose(gid) < 0) TEST_ERROR;

    /* =================================== */
    /* Verification of Metadata Tag Values */
    /* =================================== */

#ifndef NDEBUG
    /* if verbose, print cache index to screen for visual verification */
    if ( verbose ) dump_cache(fid);
#endif /* NDEBUG */ /* end debugging functions */

    /* Verify root group's tagged metadata */
    if ( verify_tag(fid, H5AC_OHDR_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_SNODE_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_LHEAP_PRFX_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_BT_ID, root_tag) < 0 ) TEST_ERROR;

    /* Verify group's tagged metadata */
    if ( verify_tag(fid, H5AC_OHDR_ID, g_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_BT_ID, g_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_LHEAP_PRFX_ID, g_tag) < 0 ) TEST_ERROR;

    /* Verify copied group's tagged metadata */
    if ( verify_tag(fid, H5AC_OHDR_ID, copy_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_BT_ID, copy_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_LHEAP_PRFX_ID, copy_tag) < 0 ) TEST_ERROR;

    /* verify no other entries present */
    if ( verify_no_unknown_tags(fid) < 0 ) TEST_ERROR;

    /* Reset the changes we've made to the cache's data structures */
    if(reset_all_entries_investigated(fid) < 0) TEST_ERROR;

    /* =========================== */
    /* Close open objects and file */
    /* =========================== */

    if ( H5Fclose(fid) < 0 ) TEST_ERROR;

    /* ========================================== */
    /* Finished Test. Print status and return. */
    /* ========================================== */

    PASSED();
    return 0;

error:
    return 1;
} /* check_object_copy_tags */


/*-------------------------------------------------------------------------
 * Function:    check_link_removal_tags
 *
 * Purpose:     This function verifies the correct application of tags
 *              during link removal.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              March 1, 2010
 *
 *-------------------------------------------------------------------------
 */
static unsigned
check_link_removal_tags(hid_t fcpl, int type)
{
    /* Variable Declarations */
    hid_t fid = -1;                         /* File Identifier */
    hid_t did = -1;                         /* Dataset Identifier */
    hid_t sid = -1;                         /* Dataspace Identifier */
    hid_t gid = -1;                         /* Dataspace Identifier */
    int verbose = FALSE;                    /* verbose file outout */
    hid_t dcpl = -1;                        /* dataset creation pl */
    hsize_t cdims[2] = {1,1};               /* chunk dimensions */
    int fillval = 0;
    haddr_t root_tag = 0;
    haddr_t d_tag = 0;
    haddr_t g_tag = 0;
    hsize_t dims1[2] = {DIMS, DIMS};        /* dimensions */
    hsize_t maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED}; /* dimensions */
    int i,j,k = 0;                          /* iterators */
    int *data = NULL;                       /* data buffer */

    /* Testing Macro */
    TESTING("tag application during link removal");

    /* ===== */
    /* Setup */
    /* ===== */

    /* Allocate array */
    if ( (NULL == (data = (int *)HDcalloc(DIMS * DIMS, sizeof(int)))) ) TEST_ERROR;

    /* Create file */
    if ( (fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, fcpl, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* determine tag value of root group's object header */
    if ( get_object_header_tag(fid, &root_tag) < 0 ) TEST_ERROR;

    /* Create group */
    if ( (gid = H5Gcreate2(fid, GROUPNAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Retrieve group tag */
    if ( get_object_header_tag(gid, &g_tag) < 0 ) TEST_ERROR;

    /* Close Group */
    if (H5Gclose(gid) < 0) TEST_ERROR;

    /* Set up creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);

    /* Enable chunking */
    if ( H5Pset_chunk(dcpl, RANK, cdims) < 0 ) TEST_ERROR;

    /* Set up a fill value */
    if ( H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval) < 0 ) TEST_ERROR;

    /* Set up dataset dataspace */
    if ( (sid = H5Screate_simple(2, dims1, maxdims)) < 0 ) TEST_ERROR;

    /* Create Dataset */
    if (( did = H5Dcreate2(fid, DATASETNAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0 ) TEST_ERROR;
    if ( H5Pclose(dcpl) < 0 ) TEST_ERROR;

    /* Retrieve tag associated with this dataset */
    if ( get_object_header_tag(did, &d_tag) < 0 ) TEST_ERROR;

    /* fill out data buffer */
    for(i = 0; i < DIMS; i++)
        for(j = 0; j < DIMS; j++)
            data[(DIMS * i) + j] = k++;

    /* Write to dataset */
    if( (H5Dwrite(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, data)) < 0 ) TEST_ERROR;

    /* Close Dataset */
    if ( H5Dclose(did) < 0 ) TEST_ERROR;

    /* Close and Reopen the file and dataset */
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;
    if ( (fid = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Evict as much as we can from the cache so we can track full tag path */
    if ( evict_entries(fid) < 0 ) TEST_ERROR;

    /* ================================ */
    /* Remove link to group and dataset */
    /* ================================ */

    if ( (H5Ldelete(fid, GROUPNAME, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    if ( (H5Ldelete(fid, DATASETNAME, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* =================================== */
    /* Verification of Metadata Tag Values */
    /* =================================== */

#ifndef NDEBUG
    /* if verbose, print cache index to screen for visual verification */
    if ( verbose ) dump_cache(fid);
#endif /* NDEBUG */ /* end debugging functions */

    /* Verify root group's tagged metadata */
    if ( verify_tag(fid, H5AC_OHDR_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_LHEAP_PRFX_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_BT_ID, root_tag) < 0 ) TEST_ERROR;

    if ( type == TEST_SHMESG ) {

        /* verify shared header message master table */
        if ( verify_tag(fid, H5AC_SOHM_TABLE_ID, H5AC__SOHM_TAG) < 0 ) TEST_ERROR;

    } /* end if */

    /* verify no other entries present */
    if ( verify_no_unknown_tags(fid) < 0 ) TEST_ERROR;

    /* Reset the changes we've made to the cache's data structures */
    if(reset_all_entries_investigated(fid) < 0) TEST_ERROR;

    /* =========================== */
    /* Close open objects and file */
    /* =========================== */

    if ( H5Fclose(fid) < 0 ) TEST_ERROR;

    /* ========================================== */
    /* Finished Test. Print status and return. */
    /* ========================================== */

    HDfree(data);

    PASSED();
    return 0;

error:
    if(data)
        HDfree(data);
    return 1;
} /* check_link_removal_tags */


/*-------------------------------------------------------------------------
 * Function:    check_link_getname_tags
 *
 * Purpose:     This function verifies the correct application of tags
 *              during link name retrieval.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              March 2, 2010
 *
 *-------------------------------------------------------------------------
 */
static unsigned
check_link_getname_tags(void)
{
    /* Variable Declarations */
    char name[500];
    hid_t fid = -1;                         /* File Identifier */
    hid_t did = -1;                         /* Dataset Identifier */
    hid_t sid = -1;                         /* Dataspace Identifier */
    hid_t gid = -1;                         /* Dataspace Identifier */
    int verbose = FALSE;                    /* verbose file outout */
    hid_t dcpl = -1;                        /* dataset creation pl */
    hsize_t cdims[2] = {1,1};               /* chunk dimensions */
    int fillval = 0;
    haddr_t root_tag = 0;
    haddr_t d_tag = 0;
    haddr_t g_tag = 0;
    hsize_t dims1[2] = {DIMS, DIMS};        /* dimensions */
    hsize_t maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED}; /* dimensions */
    int i,j,k = 0;                          /* iterators */
    int *data = NULL;                       /* data buffer */

    /* Testing Macro */
    TESTING("tag application during link name retrieval");

    /* ===== */
    /* Setup */
    /* ===== */

    /* Allocate array */
    if ( (NULL == (data = (int *)HDcalloc(DIMS * DIMS, sizeof(int)))) ) TEST_ERROR;

    /* Create file */
    if ( (fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* determine tag value of root group's object header */
    if ( get_object_header_tag(fid, &root_tag) < 0 ) TEST_ERROR;

    /* Create group */
    if ( (gid = H5Gcreate2(fid, GROUPNAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Retrieve group tag */
    if ( get_object_header_tag(gid, &g_tag) < 0 ) TEST_ERROR;

    /* Close Group */
    if (H5Gclose(gid) < 0) TEST_ERROR;

    /* Set up creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);

    /* Enable chunking */
    if ( H5Pset_chunk(dcpl, RANK, cdims) < 0 ) TEST_ERROR;

    /* Set up a fill value */
    if ( H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval) < 0 ) TEST_ERROR;

    /* Set up dataset dataspace */
    if ( (sid = H5Screate_simple(2, dims1, maxdims)) < 0 ) TEST_ERROR;

    /* Create Dataset */
    if (( did = H5Dcreate2(fid, DATASETNAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0 ) TEST_ERROR;
    if ( H5Pclose(dcpl) < 0 ) TEST_ERROR;

    /* Retrieve tag associated with this dataset */
    if ( get_object_header_tag(did, &d_tag) < 0 ) TEST_ERROR;

    /* fill out data buffer */
    for(i = 0; i < DIMS; i++)
        for(j = 0; j < DIMS; j++)
            data[(DIMS * i) + j] = k++;

    /* Write to dataset */
    if( (H5Dwrite(did, H5T_NATIVE_INT, sid, sid, H5P_DEFAULT, data)) < 0 ) TEST_ERROR;

    /* Close Dataset */
    if ( H5Dclose(did) < 0 ) TEST_ERROR;

    /* Close and Reopen the file and dataset */
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;
    if ( (fid = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Evict as much as we can from the cache so we can track full tag path */
    if ( evict_entries(fid) < 0 ) TEST_ERROR;

    /* =========================== */
    /* Get name by index location. */
    /* =========================== */

    H5Lget_name_by_idx(fid, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)1, name, (size_t)500, H5P_DEFAULT);

    /* =================================== */
    /* Verification of Metadata Tag Values */
    /* =================================== */

#ifndef NDEBUG
    /* if verbose, print cache index to screen for visual verification */
    if ( verbose ) dump_cache(fid);
#endif /* NDEBUG */ /* end debugging functions */

    /* Verify root group's tagged metadata */
    if ( verify_tag(fid, H5AC_OHDR_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_SNODE_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_LHEAP_PRFX_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_BT_ID, root_tag) < 0 ) TEST_ERROR;

    /* verify no other entries present */
    if ( verify_no_unknown_tags(fid) < 0 ) TEST_ERROR;

    /* Reset the changes we've made to the cache's data structures */
    if(reset_all_entries_investigated(fid) < 0) TEST_ERROR;

    /* =========================== */
    /* Close open objects and file */
    /* =========================== */

    if ( H5Fclose(fid) < 0 ) TEST_ERROR;

    /* ========================================== */
    /* Finished Test. Print status and return. */
    /* ========================================== */

    HDfree(data);

    PASSED();
    return 0;

error:
    if(data)
        HDfree(data);
    return 1;
} /* check_link_getname_tags */


/*-------------------------------------------------------------------------
 * Function:    check_external_link_creation_tags
 *
 * Purpose:     This function verifies the correct application of tags
 *              during external link creation.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              February 24, 2010
 *
 *-------------------------------------------------------------------------
 */
static unsigned
check_external_link_creation_tags(void)
{
    /* Variable Declarations */
    hid_t fid = -1;                         /* File Identifier */
    hid_t fid2 = -1;                         /* File Identifier */
    hid_t gid = -1;                         /* Dataspace Identifier */
    int verbose = FALSE;                    /* verbose file outout */
    haddr_t root_tag = 0;

    /* Testing Macro */
    TESTING("tag application during external link creation");

    /* ===== */
    /* Setup */
    /* ===== */

    /* Create a test file */
    if ( (fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* determine tag value of root group's object header */
    if ( get_object_header_tag(fid, &root_tag) < 0 ) TEST_ERROR;

    /* Close and Reopen the file */
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;
    if ( (fid = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Evict as much as we can from the cache so we can track full tag path */
    if ( evict_entries(fid) < 0 ) TEST_ERROR;

    /* Create a second file */
    if ( (fid2 = H5Fcreate(FILENAME2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Create group in second file */
    if ( (gid = H5Gcreate2(fid2, GROUPNAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Close out second file */
    if ( (H5Gclose(gid)) < 0 ) TEST_ERROR;
    if ( (H5Fclose(fid2)) < 0 ) TEST_ERROR;

    /* ==================== */
    /* Create External Link */
    /* ==================== */

    if (H5Lcreate_external(FILENAME2, GROUPNAMEPATH, fid, LINKNAME, H5P_DEFAULT, H5P_DEFAULT) < 0 ) TEST_ERROR;

    /* =================================== */
    /* Verification of Metadata Tag Values */
    /* =================================== */

#ifndef NDEBUG
    /* if verbose, print cache index to screen for visual verification */
    if ( verbose ) dump_cache(fid);
#endif /* NDEBUG */ /* end debugging functions */

    /* Verify root group metadata */
    if ( verify_tag(fid, H5AC_OHDR_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_OHDR_CHK_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_LHEAP_PRFX_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_BT_ID, root_tag) < 0 ) TEST_ERROR;

    /* verify no other entries present */
    if ( verify_no_unknown_tags(fid) < 0 ) TEST_ERROR;

    /* Reset the changes we've made to the cache's data structures */
    if(reset_all_entries_investigated(fid) < 0) TEST_ERROR;

    /* =========================== */
    /* Close open objects and file */
    /* =========================== */

    if ( H5Fclose(fid) < 0 ) TEST_ERROR;

    /* ========================================== */
    /* Finished Test. Print status and return. */
    /* ========================================== */

    PASSED();
    return 0;

error:
    return 1;
} /* check_external_link_creation_tags */


/*-------------------------------------------------------------------------
 * Function:    check_external_link_open_tags
 *
 * Purpose:     This function verifies the correct application of tags
 *              during external link open.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              February 24, 2010
 *
 *-------------------------------------------------------------------------
 */
static unsigned
check_external_link_open_tags(void)
{
    /* Variable Declarations */
    haddr_t link_tag = 0;                  /* link tag */
    hid_t fid = -1;                         /* File Identifier */
    hid_t fid2 = -1;                        /* File Identifier */
    hid_t gid = -1;                         /* Dataspace Identifier */
    hid_t xid = -1;                         /* Dataspace Identifier */
    int verbose = FALSE;                    /* verbose file outout */
    haddr_t root_tag = 0;
    haddr_t root2_tag = 0;

    /* Testing Macro */
    TESTING("tag application during external link open");

    /* ===== */
    /* Setup */
    /* ===== */

    /* Create a test file */
    if ( (fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* determine tag value of root group's object header */
    if ( get_object_header_tag(fid, &root_tag) < 0 ) TEST_ERROR;

    /* Create a second file */
    if ( (fid2 = H5Fcreate(FILENAME2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* determine tag value of root group's object header */
    if ( get_object_header_tag(fid2, &root2_tag) < 0 ) TEST_ERROR;

    /* Create group in second file */
    if ( (gid = H5Gcreate2(fid2, GROUPNAME, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Close out second file */
    if ( (H5Gclose(gid)) < 0 ) TEST_ERROR;
    if ( (H5Fclose(fid2)) < 0 ) TEST_ERROR;

    /* Create external link to second file */
    if ( H5Lcreate_external(FILENAME2, GROUPNAMEPATH, fid, LINKNAME, H5P_DEFAULT, H5P_DEFAULT) < 0 ) TEST_ERROR;

    /* Close and Reopen the file */
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;
    if ( (fid = H5Fopen(FILENAME, H5F_ACC_RDWR, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Evict as much as we can from the cache so we can track full tag path */
    if ( evict_entries(fid) < 0 ) TEST_ERROR;

    /* ================== */
    /* Open External Link */
    /* ================== */

    if ( (xid = H5Gopen2(fid, LINKNAME, H5P_DEFAULT)) < 0 ) TEST_ERROR;
    if ( (fid2 = H5Iget_file_id(xid)) < 0) TEST_ERROR;
    if ( get_object_header_tag(xid, &link_tag) < 0 ) TEST_ERROR;

    /* =================================== */
    /* Verification of Metadata Tag Values */
    /* =================================== */

#ifndef NDEBUG
    /* if verbose, print cache index to screen for visual verification */
    if ( verbose ) dump_cache(fid);
#endif /* NDEBUG */ /* end debugging functions */

    /* verify tag value of first file's root group */
    if ( verify_tag(fid, H5AC_OHDR_ID, root_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid, H5AC_OHDR_CHK_ID, root_tag) < 0 ) TEST_ERROR;

    /* verify there is a superblock entry with superblock tag. */
    if ( verify_tag(fid2, H5AC_SUPERBLOCK_ID, H5AC__SUPERBLOCK_TAG) < 0 ) TEST_ERROR;

    /* verify tag value of linked file's root group */
    if ( verify_tag(fid2, H5AC_OHDR_ID, root2_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid2, H5AC_LHEAP_PRFX_ID, root2_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid2, H5AC_BT_ID, root2_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid2, H5AC_SNODE_ID, root2_tag) < 0 ) TEST_ERROR;

    /* verify tag value of linked group's object header */
    if ( verify_tag(fid2, H5AC_OHDR_ID, link_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid2, H5AC_LHEAP_PRFX_ID, link_tag) < 0 ) TEST_ERROR;
    if ( verify_tag(fid2, H5AC_BT_ID, link_tag) < 0 ) TEST_ERROR;

    /* verify no other entries present */
    if ( verify_no_unknown_tags(fid) < 0 ) TEST_ERROR;
    if ( verify_no_unknown_tags(fid2) < 0 ) TEST_ERROR;

    /* Reset the changes we've made to the cache's data structures */
    if(reset_all_entries_investigated(fid) < 0) TEST_ERROR;
    if(reset_all_entries_investigated(fid2) < 0) TEST_ERROR;

    /* =========================== */
    /* Close open objects and file */
    /* =========================== */
    if ( (H5Gclose(xid)) < 0 ) TEST_ERROR;
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;
    if ( H5Fclose(fid2) < 0 ) TEST_ERROR;

    /* ========================================== */
    /* Finished Test. Print status and return. */
    /* ========================================== */

    PASSED();
    return 0;

error:
    return 1;
} /* check_external_link_open_tags */


/*-------------------------------------------------------------------------
 * Function:    check_invalid_tag_application
 *
 * Purpose:     This function verifies that an error occurs if a tag
 *              has not been set up during a protect or set of
 *              a new piece of metadata.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              May 27, 2010
 *
 *-------------------------------------------------------------------------
 */
static unsigned
check_invalid_tag_application(void)
{
#if H5C_DO_TAGGING_SANITY_CHECKS
    /* Variables */
    H5F_t * f = NULL;
    hid_t fid = -1;
    haddr_t addr;
    H5HL_t * lheap = NULL;
    hbool_t     api_ctx_pushed = FALSE;             /* Whether API context pushed */
#endif /* H5C_DO_TAGGING_SANITY_CHECKS */

    /* Testing Macro */
    TESTING("failure on invalid tag application");

#if H5C_DO_TAGGING_SANITY_CHECKS
    /* Create a test file */
    if ( (fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0 ) TEST_ERROR;

    /* Push API context */
    if(H5CX_push() < 0) TEST_ERROR
    api_ctx_pushed = TRUE;

    /* Get internal file pointer*/
    if ( NULL == (f = (H5F_t *)H5I_object(fid)) ) TEST_ERROR;

    /* Call H5HL_create, an internal function that calls H5AC_insert_entry without setting up a tag */
    /* Ensure this returns FAILURE, as a tag has not been set up. */
    if ( H5HL_create(f, (size_t)1024, &addr) >= 0) TEST_ERROR;

    /* Now set up a tag in the API context */
    H5AC_tag((haddr_t)25, NULL);

    /* Verify the same call to H5HL_create now works as intended, with a tag set up. */
    if ( H5HL_create(f, (size_t)1024, &addr) < 0) TEST_ERROR;

    /* Reset API context to use invalid tag. */
    H5AC_tag(H5AC__INVALID_TAG, NULL);

    /* Call H5HL_protect to protect the local heap created above. */
    /* This should fail as no tag is set up during the protect call */
    if (( lheap = H5HL_protect(f, addr, H5AC__NO_FLAGS_SET)) != NULL ) TEST_ERROR;

    /* Again, set up a valid tag in the DXPL */
    H5AC_tag((haddr_t)25, NULL);

    /* Call H5HL_protect again to protect the local heap. This should succeed. */
    if (( lheap = H5HL_protect(f, addr, H5AC__NO_FLAGS_SET)) == NULL ) TEST_ERROR;

    /* Now unprotect the heap, as we're done with the test. */
    if ( H5HL_unprotect(lheap) < 0 ) TEST_ERROR;

    /* Pop API context */
    if(api_ctx_pushed && H5CX_pop() < 0) TEST_ERROR
    api_ctx_pushed = FALSE;

    /* Close open objects and file */
    if ( H5Fclose(fid) < 0 ) TEST_ERROR;

    /* Finished Test. Print status and return. */
    PASSED();
#else
    SKIPPED();
    HDprintf("    test skipped because sanity checking on tag value is disabled.\n");
#endif /* H5C_DO_TAGGING_SANITY_CHECKS */

    return 0;

error:
#if H5C_DO_TAGGING_SANITY_CHECKS
    if(api_ctx_pushed) H5CX_pop();
#endif /* H5C_DO_TAGGING_SANITY_CHECKS */

    return 1;
} /* check_invalid_tag_application */


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Run tests on library's ability to tag metadata entries.
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 * Programmer:  Mike McGreevy
 *              January 15, 2009
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    /* Variable Declarations */
    hid_t fcpl_default = -1;        /* file creation prop list */
    hid_t fcpl_shmesg_all = -1;     /* file creation prop list */
    hid_t fcpl = -1;                /* file creation prop list */
    unsigned nerrs = 0;         /* Error Encountered */
    int test_type = 0;          /* test type iterator */

    /* Open the HDF5 Library */
    H5open();

    /* ========== */
    /* Test Setup */
    /* ========== */

    /* Create a standard file creation property list */
    fcpl_default = H5Pcreate(H5P_FILE_CREATE);

    /* Create an fcpl with shared messages and file space managment enabled */
    fcpl_shmesg_all = H5Pcreate(H5P_FILE_CREATE);
    H5Pset_shared_mesg_nindexes(fcpl_shmesg_all, 1);
    H5Pset_shared_mesg_index(fcpl_shmesg_all, 0, H5O_SHMESG_ALL_FLAG, 20);
    H5Pset_file_space_strategy(fcpl_shmesg_all, H5F_FSPACE_STRATEGY_FSM_AGGR, TRUE, (hsize_t)0);

    /* ========= */
    /* Run Tests */
    /* ========= */

    for (test_type=0; test_type<NUM_TEST_TYPES; test_type++) {

        /* Run tests on each fcpl set up above. */
        if (test_type == TEST_DEFAULT) {

            if (!nerrs) HDprintf("Testing standard tag application cases w/ default fcpl:\n");
            fcpl = fcpl_default;

        } else if (test_type == TEST_SHMESG) {

            if (!nerrs) HDprintf("Testing standard tag application cases w/ shared messages:\n");
            fcpl = fcpl_shmesg_all;

        } else {
            TEST_ERROR;
        }

        /* Check tag application under different circumstances */
        if (!nerrs) nerrs += check_file_creation_tags(fcpl, test_type);
        if (!nerrs) nerrs += check_file_open_tags(fcpl, test_type);
        if (!nerrs) nerrs += check_attribute_creation_tags(fcpl, test_type);
        if (!nerrs) nerrs += check_attribute_open_tags(fcpl, test_type);
        if (!nerrs) nerrs += check_attribute_write_tags(fcpl, test_type);
        if (!nerrs) nerrs += check_attribute_delete_tags(fcpl, test_type);
        if (!nerrs) nerrs += check_attribute_rename_tags(fcpl, test_type);
        if (!nerrs) nerrs += check_dataset_creation_tags(fcpl, test_type);
        if (!nerrs) nerrs += check_dataset_creation_earlyalloc_tags(fcpl, test_type);
        if (!nerrs) nerrs += check_link_removal_tags(fcpl, test_type);
    } /* end for */

    if (!nerrs) HDprintf("Testing other specific tag application cases:\n");
    if (!nerrs) nerrs += check_group_creation_tags();
    if (!nerrs) nerrs += check_multi_group_creation_tags();
    if (!nerrs) nerrs += check_group_open_tags();
    if (!nerrs) nerrs += check_dataset_open_tags();
    if (!nerrs) nerrs += check_dataset_write_tags();
    if (!nerrs) nerrs += check_dataset_read_tags();
    if (!nerrs) nerrs += check_dataset_size_retrieval();
    if (!nerrs) nerrs += check_dataset_extend_tags();
    if (!nerrs) nerrs += check_object_info_tags();
    if (!nerrs) nerrs += check_object_copy_tags();
    if (!nerrs) nerrs += check_link_getname_tags();
    if (!nerrs) nerrs += check_external_link_creation_tags();
    if (!nerrs) nerrs += check_external_link_open_tags();

    if (!nerrs) nerrs += check_dense_attribute_tags();
    if (!nerrs) nerrs += check_link_iteration_tags();
    if (!nerrs) nerrs += check_invalid_tag_application();

    /* Delete test files */
    HDremove(FILENAME);
    HDremove(FILENAME2);

    /* Return Errors */
    if (nerrs > 0)
        return EXIT_FAILURE;
    else
        return EXIT_SUCCESS;

error:
    /* Return with Error */
    return EXIT_FAILURE;

} /* main */
