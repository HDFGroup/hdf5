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
 * Onion Virtual File Driver (VFD)
 *
 * Purpose:
 *
 *      Verify Onion VFD behavior that is not involved with operations on the
 *      backing store.
 */

#include "h5test.h"

#include "H5Fprivate.h"     /* encode/decode macros         */
#include "H5FDonion.h"      /* This file driver's utilities */
#include "H5FDonion_priv.h" /* Onion file driver internals  */

/* The Onion VFD uses H5MM calls internally, so any tests that allocate
 * or free memory for said internal structures (e.g., the revision lists)
 * will need to allocate memory using H5MM calls.
 */
#include "H5MMprivate.h" /* Memory management */

/* 2^n for uint64_t types -- H5_EXP2 unsafe past 32 bits */
#define U64_EXP2(n) ((uint64_t)1 << (n))

#define ONION_TEST_PAGE_SIZE_1                    4
#define ONION_TEST_PAGE_SIZE_5                    32
#define ONION_TEST_FIXNAME_SIZE                   1024
#define ONION_TEST_EXPECTED_HISTORY_REVISIONS_MAX 16
#define ONION_TEST_REV_REV_WRITES_MAX             8
#define ONE_DIM_SIZE                              1024

/* Structure to collect the onion filepaths in one place. */
struct onion_filepaths {
    char *canon;
    char *onion;
    char *recovery;
};

struct expected_revision {
    uint64_t    revision_num;
    uint64_t    parent_revision_num;
    uint64_t    logical_eof;
    uint64_t    n_index_entries;
    const char *comment;
};
struct expected_history {
    uint64_t                 page_size;
    uint64_t                 n_revisions;
    uint64_t                 origin_eof;
    struct expected_revision revisions[ONION_TEST_EXPECTED_HISTORY_REVISIONS_MAX];
};

struct write_info {
    haddr_t              offset;
    haddr_t              size;
    const unsigned char *buf;
};
struct revise_revision {
    bool              truncate; /* onion-create, truncating any existing data */
    uint64_t          revision_num;
    size_t            n_writes;
    struct write_info writes[ONION_TEST_REV_REV_WRITES_MAX];
    const char       *comment;
};

static int  compare_file_bytes_exactly(const char *filepath, hid_t fapl_id, size_t nbytes,
                                       const unsigned char *exp);
static int  do_onion_open_and_writes(const char *filename, H5FD_onion_fapl_info_t *onion_info_p, size_t n_ops,
                                     struct revise_revision *about);
static void onion_filepaths_destroy(struct onion_filepaths *paths);
static struct onion_filepaths *onion_filepaths_init(const char *basename);

/* set at runtime in main() */
static unsigned int flags_create_s = 0;

/* NOTE: b_list must be longer than a_list.
 * Sizes must match respective buffer lengths.
 */

/* twenty-six four-character words beginning with 'a' -> 104 bytes */
static const unsigned char *a_list_s =
    (const unsigned char *)"abetableacedacesacheacidacneadzeafaragedagesaidsairsajarallyalum"
                           "amokantsapesarcsareaartsasksaspsavidaxes";
uint64_t a_list_size_s = 104;

/* fifty-three four-character words beginning with 'b' -> 212 bytes */
static const unsigned char *b_list_s =
    (const unsigned char *)"badebailbaitbalebanebarebaskbeambeanbearbeenbeerbeltbentbestbide"
                           "bikebilebindbirdbiteblipblueboarboatbobsbodyboilboldbollboltbond"
                           "boneboobboorboosbootbradbragbratbraybrewbritbrowbuckbudsbunkbunt"
                           "buoyburnburybustbuys";
static uint64_t b_list_size_s = 212;

/* Allocate and populate filepaths with h5_fixname'd strings as appropriate.
 * Should be released with onion_filepaths_destroy() when done.
 */
static struct onion_filepaths *
onion_filepaths_init(const char *basename)
{
    struct onion_filepaths *paths = NULL;

    if (NULL == (paths = malloc(sizeof(struct onion_filepaths))))
        TEST_ERROR;
    paths->canon    = NULL;
    paths->onion    = NULL;
    paths->recovery = NULL;

    if (NULL == (paths->canon = malloc(sizeof(char) * ONION_TEST_FIXNAME_SIZE)))
        TEST_ERROR;
    snprintf(paths->canon, ONION_TEST_FIXNAME_SIZE, "%s", basename);

    if (NULL == (paths->onion = malloc(sizeof(char) * ONION_TEST_FIXNAME_SIZE)))
        TEST_ERROR;
    snprintf(paths->onion, ONION_TEST_FIXNAME_SIZE, "%s.onion", paths->canon);

    if (NULL == (paths->recovery = malloc(sizeof(char) * ONION_TEST_FIXNAME_SIZE)))
        TEST_ERROR;
    snprintf(paths->recovery, ONION_TEST_FIXNAME_SIZE, "%s.onion.recovery", paths->canon);

    return paths;

error:
    if (paths != NULL) {
        free(paths->canon);
        free(paths->onion);
        free(paths->recovery);
    }
    free(paths);

    return NULL;
}

/* Free onion file paths */
static void
onion_filepaths_destroy(struct onion_filepaths *paths)
{
    free(paths->canon);
    free(paths->onion);
    free(paths->recovery);
    free(paths);
}

/*-----------------------------------------------------------------------------
 * Function:    test_archival_index()
 *
 * Purpose:     Unit-test mechanisms for the onion archival index.
 *              Specifies and verifies index-validation and -search routines.
 *
 * Return:      PASSED : 0
 *              FAILED : -1
 *-----------------------------------------------------------------------------
 */
static int
test_archival_index(void)
{
    /* We can ignore each entry's physical address and checksum values */
    H5FD_onion_index_entry_t e0                   = {1, 474};
    H5FD_onion_index_entry_t e1                   = {4, 558};
    H5FD_onion_index_entry_t e2                   = {5, 306};
    H5FD_onion_index_entry_t e3                   = {9, 515};
    H5FD_onion_index_entry_t e4                   = {14, 386};
    H5FD_onion_index_entry_t e5                   = {18, 90};
    H5FD_onion_index_entry_t e6                   = {19, 94};
    H5FD_onion_index_entry_t e7                   = {20, 509};
    H5FD_onion_index_entry_t sorted[8]            = {e0, e1, e2, e3, e4, e5, e6, e7};
    H5FD_onion_index_entry_t sorted_duplicates[8] = {e0, e1, e2, e2, e4, e5, e6, e7};
    H5FD_onion_index_entry_t sorted_incomplete[8] = {e1, e3, e4, e5};
    /* Partially-sorted list also aligned to 2 * page-size */
    H5FD_onion_index_entry_t    sorted_partial[8] = {e1, e4, e5, e7, e0, e6, e2, e3}; /* 0..3 sorted */
    H5FD_onion_index_entry_t    unsorted[8]       = {e3, e1, e4, e5, e0, e6, e2, e7};
    H5FD_onion_archival_index_t aix               = {
        H5FD_ONION_ARCHIVAL_INDEX_VERSION_CURR, 1, /* page_size_log2 */
        8,      /* list must be populated and sorted through 0 .. (count-1) */
        sorted, /* list */
    };
    const H5FD_onion_index_entry_t *entry_out_p = NULL;

    TESTING("archival index");

    /*
     * Failing validity checks
     */

    /* Invalid version should fail */
    aix.version++;
    if (H5FD__onion_archival_index_is_valid(&aix))
        TEST_ERROR;

    /* Invalid version should fail */
    aix.version = 0;
    if (H5FD__onion_archival_index_is_valid(&aix))
        TEST_ERROR;
    aix.version = H5FD_ONION_ARCHIVAL_INDEX_VERSION_CURR;

    /* NULL list should fail */
    aix.list = NULL;
    if (H5FD__onion_archival_index_is_valid(&aix))
        TEST_ERROR;

    /* List not full should fail */
    aix.list = sorted_incomplete;
    if (H5FD__onion_archival_index_is_valid(&aix))
        TEST_ERROR;

    /* Unsorted list should fail */
    aix.list = unsorted;
    if (H5FD__onion_archival_index_is_valid(&aix))
        TEST_ERROR;

    /* List with duplicates should fail */
    aix.list = sorted_duplicates;
    if (H5FD__onion_archival_index_is_valid(&aix))
        TEST_ERROR;

    /*
     * Passing validity checks
     */

    /* Sorted list should pass */
    aix.list = sorted;
    if (!H5FD__onion_archival_index_is_valid(&aix))
        TEST_ERROR;

    /* Extra elements ignored (should pass) */
    aix.list      = sorted_partial;
    aix.n_entries = 4;
    if (!H5FD__onion_archival_index_is_valid(&aix))
        TEST_ERROR;

    /*
     * Archival index search routine
     */

    aix.list      = sorted;
    aix.n_entries = 8;

    /* Check that address not in array returns zero */
    if (H5FD__onion_archival_index_find(&aix, 3, &entry_out_p) != 0)
        TEST_ERROR;
    /* Pointer should remain unset */
    if (entry_out_p != NULL)
        TEST_ERROR;

    /* Address found should return 1 */
    if (H5FD__onion_archival_index_find(&aix, 4, &entry_out_p) != 1)
        TEST_ERROR;
    /* Pointer should be set */
    if (NULL == entry_out_p)
        TEST_ERROR;
    /* Incorrect address recorded */
    if (558 != entry_out_p->phys_addr)
        TEST_ERROR;

    /*
     * Test search edge cases
     */

    aix.list      = sorted_incomplete;
    aix.n_entries = 4;

    /* Address not in array should return 0 */
    if (H5FD__onion_archival_index_find(&aix, 1, &entry_out_p) != 0)
        TEST_ERROR;

    /* Address not in array should return 0 */
    if (H5FD__onion_archival_index_find(&aix, 101, &entry_out_p) != 0)
        TEST_ERROR;

    /*
     * Empty archival index
     */

    entry_out_p   = NULL;
    aix.n_entries = 0; /* actually populated list is irrelevant */
    /* Address not in array should return 0 */
    if (H5FD__onion_archival_index_find(&aix, 3, &entry_out_p) != 0)
        TEST_ERROR;
    /* Pointer should remain unset */
    if (entry_out_p != NULL)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    return -1;
} /* end test_archival_index() */

/*-----------------------------------------------------------------------------
 * Function:    test_revision_index()
 *
 * Purpose:     Test revision index functionality
 *
 * Return:      PASSED : 0
 *              FAILED : -1
 *-----------------------------------------------------------------------------
 */
static int
test_revision_index(void)
{
    H5FD_onion_revision_index_t *rix_p = NULL;
    H5FD_onion_index_entry_t     entry = {
        42,     /* logical_page */
        111112, /* phys_addr */
    };
    const H5FD_onion_index_entry_t *entry_out_p = NULL;

    TESTING("revision index");

    /* Test index creation */

    if (NULL == (rix_p = H5FD__onion_revision_index_init(ONION_TEST_PAGE_SIZE_5)))
        TEST_ERROR;
    if (H5FD_ONION_REVISION_INDEX_VERSION_CURR != rix_p->version)
        TEST_ERROR;
    if (0 != rix_p->n_entries)
        TEST_ERROR;

    /* Test missed search */

    if (H5FD__onion_revision_index_find(rix_p, entry.logical_page, &entry_out_p) != 0)
        TEST_ERROR;

    /* Test successful insertion and lookup */

    /* Insertion failed */
    if (H5FD__onion_revision_index_insert(rix_p, &entry) < 0)
        TEST_ERROR;
    if (1 != rix_p->n_entries)
        TEST_ERROR;
    /* Lookup failed */
    if (H5FD__onion_revision_index_find(rix_p, entry.logical_page, &entry_out_p) < 0)
        TEST_ERROR;
    /* Failure to set output parameter */
    if (NULL == entry_out_p)
        TEST_ERROR;
    if (entry.logical_page != entry_out_p->logical_page)
        TEST_ERROR;
    /* Seeking missing page should miss */
    if (H5FD__onion_revision_index_find(rix_p, entry.logical_page + 1, &entry_out_p) != 0)
        TEST_ERROR;

    /* Test / demonstrate stored entry independent of user object */

    entry.logical_page = 100;
    entry.phys_addr    = 101;
    if (H5FD__onion_revision_index_insert(rix_p, &entry) < 0)
        TEST_ERROR;
    if (2 != rix_p->n_entries)
        TEST_ERROR;
    entry.logical_page = 500;
    entry.phys_addr    = 501;
    if (H5FD__onion_revision_index_find(rix_p, 100, &entry_out_p) < 0)
        TEST_ERROR;
    if (100 != entry_out_p->logical_page || 101 != entry_out_p->phys_addr)
        TEST_ERROR;

    /* Demonstrate updating an entry */

    /* Error cases */

    entry.logical_page = 100; /* phys_addr still 501, checksum bbbbbbbb */
    if (H5FD__onion_revision_index_insert(rix_p, &entry) >= 0)
        TEST_ERROR; /* all components but sum must match */
    entry.phys_addr = 101;

    /* Successful update */

    entry.logical_page = 100;
    entry.phys_addr    = 101;
    if (H5FD__onion_revision_index_insert(rix_p, &entry) < 0)
        TEST_ERROR;

    /* Should still be two unique entries, not three */
    if (2 != rix_p->n_entries)
        TEST_ERROR;
    if (H5FD__onion_revision_index_find(rix_p, 100, &entry_out_p) < 0)
        TEST_ERROR;
    if (100 != entry_out_p->logical_page || 101 != entry_out_p->phys_addr)
        TEST_ERROR;

    if (H5FD__onion_revision_index_destroy(rix_p) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    if (rix_p != NULL)
        (void)H5FD__onion_revision_index_destroy(rix_p);

    return -1;
} /* end test_revision_index() */

/*-----------------------------------------------------------------------------
 * Function:    test_revision_index_collisions()
 *
 * Purpose:     With knowledge of the revision index implementation, test
 *              hash key collisions.
 *
 * Return:      PASSED : 0
 *              FAILED : -1
 *-----------------------------------------------------------------------------
 */
static int
test_revision_index_collisions(void)
{
    H5FD_onion_revision_index_t *rix_p = NULL;
    H5FD_onion_index_entry_t     entry = {
        0, /* logical_page */
        0, /* phys_addr */
    };
    const H5FD_onion_index_entry_t *entry_out_p       = NULL;
    const uint64_t                  n_insert          = 40;
    const uint64_t                  offset_from_power = 5;

    TESTING("revision index collisions");

    if (NULL == (rix_p = H5FD__onion_revision_index_init(ONION_TEST_PAGE_SIZE_5)))
        TEST_ERROR;

    for (uint64_t i = 0; i < n_insert; i++) {
        entry.phys_addr    = i;
        entry.logical_page = U64_EXP2(i) + offset_from_power;
        if (H5FD__onion_revision_index_insert(rix_p, &entry) < 0)
            TEST_ERROR;
    }

    if (n_insert != rix_p->n_entries)
        TEST_ERROR;

    for (uint64_t i = 0; i < n_insert; i++) {
        uint64_t page_id = U64_EXP2(i) + offset_from_power;

        if (H5FD__onion_revision_index_find(rix_p, page_id, &entry_out_p) != 1)
            TEST_ERROR;
        if (entry_out_p->phys_addr != i)
            TEST_ERROR;
    }

    if (H5FD__onion_revision_index_destroy(rix_p) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    if (rix_p != NULL)
        (void)H5FD__onion_revision_index_destroy(rix_p);

    return -1;
} /* end test_revision_index_collisions() */

/*-----------------------------------------------------------------------------
 *
 * Function:    test_revision_index_resizing()
 *
 * Purpose:     With knowledge of the revision index implementation, test
 *              one or more resizig of the index.
 *
 * Return:      PASSED : 0
 *              FAILED : -1
 *
 *-----------------------------------------------------------------------------
 */
static int
test_revision_index_resizing(void)
{
    H5FD_onion_revision_index_t *rix_p = NULL;
    H5FD_onion_index_entry_t     entry = {
        0, /* logical_page */
        0, /* phys_addr */
    };
    const H5FD_onion_index_entry_t *entry_out_p = NULL;
    const uint64_t                  n_insert = U64_EXP2((H5FD_ONION_REVISION_INDEX_STARTING_SIZE_LOG2 + 3));

    TESTING("revision index resizing");

    if (NULL == (rix_p = H5FD__onion_revision_index_init(ONION_TEST_PAGE_SIZE_5)))
        TEST_ERROR;

    for (uint64_t i = 0; i < n_insert; i++) {
        entry.logical_page = i;
        entry.phys_addr    = ((uint64_t)(-1) - i);
        if (H5FD__onion_revision_index_insert(rix_p, &entry) < 0)
            TEST_ERROR;
    }

    if (n_insert != rix_p->n_entries)
        TEST_ERROR;

    for (uint64_t i = 0; i < n_insert; i++) {
        uint64_t page_id = i;

        if (H5FD__onion_revision_index_find(rix_p, page_id, &entry_out_p) != 1)
            TEST_ERROR;
        if (entry_out_p->phys_addr != ((uint64_t)(-1) - i))
            TEST_ERROR;
    }

    if (H5FD__onion_revision_index_destroy(rix_p) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    if (rix_p != NULL)
        (void)H5FD__onion_revision_index_destroy(rix_p);

    return -1;
} /* end test_revision_index_resizing() */

/*-----------------------------------------------------------------------------
 * Function:    test_revision_index_to_archival_index()
 *
 * Purpose:     Verify to_archival_index().
 *
 * Return:      PASSED : 0
 *              FAILED : -1
 *-----------------------------------------------------------------------------
 */
static int
test_revision_index_to_archival_index(void)
{
    H5FD_onion_revision_index_t *rix_p     = NULL;
    H5FD_onion_index_entry_t     rix_entry = {
        0, /* logical_page */
        0, /* phys_addr */
    };
    H5FD_onion_archival_index_t aix = {
        H5FD_ONION_ARCHIVAL_INDEX_VERSION_CURR,
        5, /* page_size_log2 */
        0, /* n_entries to be set */
        NULL,
    };
    const uint64_t n_insert = 10;

    TESTING("revision index to archival index");

    /*
     * SETUP
     */

    if (NULL == (rix_p = H5FD__onion_revision_index_init(ONION_TEST_PAGE_SIZE_5)))
        TEST_ERROR;

    /* Add scattered entries in reverse order. */
    for (uint64_t i = 0; i < n_insert; i++) {
        uint64_t n = 2003 * (n_insert - i) + 47;

        rix_entry.logical_page = n;
        rix_entry.phys_addr    = n * 13;
        if (H5FD__onion_revision_index_insert(rix_p, &rix_entry) < 0)
            TEST_ERROR;
    }

    if (n_insert != rix_p->n_entries)
        TEST_ERROR;

    aix.list      = NULL;
    aix.n_entries = 0;

    /* Successful merge into empty archival index */

    if (H5FD__onion_merge_revision_index_into_archival_index(rix_p, &aix) < 0)
        TEST_ERROR;

    if (!H5FD__onion_archival_index_is_valid(&aix))
        TEST_ERROR;

    if (n_insert != aix.n_entries)
        TEST_ERROR;
    for (uint64_t i = 0; i < n_insert; i++) {
        const H5FD_onion_index_entry_t *aix_entry_p = NULL;

        uint64_t n = 2003 * (i + 1) + 47;

        aix_entry_p = &aix.list[i];

        if (aix_entry_p->logical_page != n)
            TEST_ERROR;
        if (aix_entry_p->phys_addr != (n * 13))
            TEST_ERROR;
    }

    /* Successful merge into populated archival index */

    H5MM_xfree(aix.list);
    aix.list = NULL;
    if (NULL == (aix.list = H5MM_malloc(sizeof(H5FD_onion_index_entry_t) * 2)))
        TEST_ERROR;
    aix.list[0].logical_page = 47;
    aix.list[0].phys_addr    = 47 * 13;
    aix.list[1].logical_page = (2003 * (n_insert + 1) + 47);
    aix.list[1].phys_addr    = (2003 * (n_insert + 1) + 47) * 13;
    aix.n_entries            = 2;

    if (!H5FD__onion_archival_index_is_valid(&aix))
        TEST_ERROR;

    if (H5FD__onion_merge_revision_index_into_archival_index(rix_p, &aix) < 0)
        TEST_ERROR;
    if (!H5FD__onion_archival_index_is_valid(&aix))
        TEST_ERROR;

    if (n_insert + 2 != aix.n_entries)
        TEST_ERROR;

    for (uint64_t i = 0; i < (n_insert + 2); i++) {
        const H5FD_onion_index_entry_t *aix_entry_p = NULL;

        uint64_t n = 2003 * i + 47;

        aix_entry_p = &aix.list[i];

        if (aix_entry_p->logical_page != n)
            TEST_ERROR;
        if (aix_entry_p->phys_addr != (n * 13))
            TEST_ERROR;
    }

    /* Merged enties from revision index replace existing entries */

    H5MM_xfree(aix.list);
    aix.list = NULL;
    if (NULL == (aix.list = H5MM_malloc(sizeof(H5FD_onion_index_entry_t) * 2)))
        TEST_ERROR;
    aix.list[0].logical_page = 2003 * (n_insert / 2) + 47;
    aix.list[0].phys_addr    = 103;
    aix.list[1].logical_page = 2003 * (n_insert / 2 + 1) + 47;
    aix.list[1].phys_addr    = 101;
    aix.n_entries            = 2;

    if (!H5FD__onion_archival_index_is_valid(&aix))
        TEST_ERROR;

    if (H5FD__onion_merge_revision_index_into_archival_index(rix_p, &aix) < 0)
        TEST_ERROR;

    if (!H5FD__onion_archival_index_is_valid(&aix))
        TEST_ERROR;

    if (n_insert != aix.n_entries)
        TEST_ERROR;

    for (uint64_t i = 0; i < n_insert; i++) {
        const H5FD_onion_index_entry_t *aix_entry_p = NULL;
        uint64_t                        n           = 2003 * (i + 1) + 47;

        aix_entry_p = &aix.list[i];

        if (aix_entry_p->logical_page != n)
            TEST_ERROR;
        if (aix_entry_p->phys_addr != (n * 13))
            TEST_ERROR;
    }

    /* CLEANUP */

    if (H5FD__onion_revision_index_destroy(rix_p) < 0)
        TEST_ERROR;
    H5MM_xfree(aix.list);

    PASSED();
    return 0;

error:
    if (rix_p)
        (void)H5FD__onion_revision_index_destroy(rix_p);
    H5MM_xfree(aix.list);

    return -1;
} /* end test_revision_index_to_archival_index() */

/*-----------------------------------------------------------------------------
 * Function:    test_fapl()
 *
 * Purpose:     Verify H5Pget and set behavior, and data-consistency checks.
 *
 * Return:      PASSED : 0
 *              FAILED : -1
 *-----------------------------------------------------------------------------
 */
static int
test_fapl(void)
{
    H5FD_onion_fapl_info_t info_in = {
        H5FD_ONION_FAPL_INFO_VERSION_CURR,
        H5P_DEFAULT,                   /* backing_fapl_id                */
        ONION_TEST_PAGE_SIZE_1,        /* page_size                      */
        H5FD_ONION_STORE_TARGET_ONION, /* store_target                   */
        H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST,
        0,                        /* force_write_open               */
        0,                        /* creation_flags                 */
        "indoor speaking voices", /* comment                        */
    };
    H5FD_onion_fapl_info_t info_out;
    hid_t                  dxpl_id      = H5I_INVALID_HID;
    hid_t                  fapl_id      = H5I_INVALID_HID;
    hid_t                  fapl_id_sec2 = H5I_INVALID_HID;
    herr_t                 ret          = FAIL;

    TESTING("file access property list");

    if ((dxpl_id = H5Pcreate(H5P_DATASET_XFER)) < 0)
        TEST_ERROR;

    if ((fapl_id_sec2 = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_fapl_sec2(fapl_id_sec2))
        TEST_ERROR;

    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    /* Set FAPL */

    /* Invalid fapl should fail */
    H5E_BEGIN_TRY
    {
        ret = H5Pset_fapl_onion(H5I_INVALID_HID, &info_in);
    }
    H5E_END_TRY
    if (SUCCEED == ret)
        TEST_ERROR;

    /* NULL info pointer should fail */
    H5E_BEGIN_TRY
    {
        ret = H5Pset_fapl_onion(fapl_id, NULL);
    }
    H5E_END_TRY
    if (SUCCEED == ret)
        TEST_ERROR;

    /* Invalid version should fail */
    info_in.version++;
    H5E_BEGIN_TRY
    {
        ret = H5Pset_fapl_onion(fapl_id, &info_in);
    }
    H5E_END_TRY
    if (SUCCEED == ret)
        TEST_ERROR;
    info_in.version--;

    /* Page size not a power of 2 should fail */
    info_in.page_size = 7;
    H5E_BEGIN_TRY
    {
        ret = H5Pset_fapl_onion(fapl_id, &info_in);
    }
    H5E_END_TRY
    if (SUCCEED == ret)
        TEST_ERROR;

    /* Page size <=0 should fail */
    info_in.page_size = 0;
    H5E_BEGIN_TRY
    {
        ret = H5Pset_fapl_onion(fapl_id, &info_in);
    }
    H5E_END_TRY
    if (SUCCEED == ret)
        TEST_ERROR;
    info_in.page_size = ONION_TEST_PAGE_SIZE_1;

    /* Invalid backing fapl should fail */
    info_in.backing_fapl_id = H5I_INVALID_HID;
    H5E_BEGIN_TRY
    {
        ret = H5Pset_fapl_onion(fapl_id, &info_in);
    }
    H5E_END_TRY
    if (SUCCEED == ret)
        TEST_ERROR;

    /* Backing fapl not a fapl should fail */
    info_in.backing_fapl_id = dxpl_id;
    H5E_BEGIN_TRY
    {
        ret = H5Pset_fapl_onion(fapl_id, &info_in);
    }
    H5E_END_TRY
    if (SUCCEED == ret)
        TEST_ERROR;
    info_in.backing_fapl_id = H5P_DEFAULT;

    if (H5Pset_fapl_onion(fapl_id, &info_in) < 0)
        TEST_ERROR;

    /* Get onion fapl info */

    /* NULL info_out pointer should fail */
    H5E_BEGIN_TRY
    {
        ret = H5Pget_fapl_onion(fapl_id, NULL);
    }
    H5E_END_TRY
    if (SUCCEED == ret)
        TEST_ERROR;

    /* Invalid fapl should fail */
    H5E_BEGIN_TRY
    {
        ret = H5Pget_fapl_onion(H5I_INVALID_HID, &info_out);
    }
    H5E_END_TRY
    if (SUCCEED == ret)
        TEST_ERROR;

    /* Non-onion fapl ID should fail */
    H5E_BEGIN_TRY
    {
        ret = H5Pget_fapl_onion(fapl_id_sec2, &info_out);
    }
    H5E_END_TRY
    if (SUCCEED == ret)
        TEST_ERROR;

    /* Normal case */
    if (H5Pget_fapl_onion(fapl_id, &info_out) < 0)
        TEST_ERROR;
    if (H5FD_ONION_FAPL_INFO_VERSION_CURR != info_out.version)
        TEST_ERROR;
    if (H5P_DEFAULT != info_out.backing_fapl_id)
        TEST_ERROR;
    if (ONION_TEST_PAGE_SIZE_1 != info_out.page_size)
        TEST_ERROR;
    if (H5FD_ONION_STORE_TARGET_ONION != info_out.store_target)
        TEST_ERROR;
    if (H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST != info_out.revision_num)
        TEST_ERROR;
    if (0 != info_out.creation_flags)
        TEST_ERROR;
    if (0 != info_out.force_write_open)
        TEST_ERROR;
    if (strcmp(info_in.comment, info_out.comment))
        TEST_ERROR;

    /* Cleanup */

    if (H5Pclose(dxpl_id) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id_sec2) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(dxpl_id);
        H5Pclose(fapl_id);
        H5Pclose(fapl_id_sec2);
    }
    H5E_END_TRY

    return -1;
} /* end test_fapl() */

/*-----------------------------------------------------------------------------
 * Function:    test_header_encode_decode()
 *
 * Purpose:     Verify onion header encoding and decoding behavior.
 *
 * Return:      PASSED : 0
 *              FAILED : -1
 *-----------------------------------------------------------------------------
 */
static int
test_header_encode_decode(void)
{
    unsigned char buf[64];
    unsigned char exp[64] = {
        /* bogus but unique values */
        'O',  'H',  'D',  'H', /* NOTE: match signature define in onion_priv.h */
        1,    12,   0,    0,   /* NOTE: update version w/ "current" as needed */
        0,    16,   0,    0,   0x11, 0x00, 0, 0, 0x02, 0, 0, 0, /* origin_eof         */
        0x40, 0xe2, 0x01, 0,   0,    0,    0, 0,                /* history_addr */
        88,   0,    0,    0,   0,    0,    0, 0,                /* history_size */
        0,    0,    0,    0                                     /* sum populated below */
    };
    unsigned char      *ptr          = NULL;
    uint32_t            checksum     = 0;
    uint32_t            checksum_out = 0;
    size_t              i            = 0;
    uint64_t            size_ret     = 0;
    H5FD_onion_header_t hdr;
    H5FD_onion_header_t hdr_out;

    TESTING("encode/decode history header");

    checksum = H5_checksum_fletcher32(exp, H5FD_ONION_ENCODED_SIZE_HEADER - 4);
    ptr      = exp + H5FD_ONION_ENCODED_SIZE_HEADER - 4;
    UINT32ENCODE(ptr, checksum);

    hdr.version      = H5FD_ONION_HEADER_VERSION_CURR;
    hdr.flags        = 12;
    hdr.origin_eof   = 8589934609ull;
    hdr.page_size    = 4096;
    hdr.history_addr = 123456;
    hdr.history_size = 88;

    if (H5FD__onion_header_encode(&hdr, buf, &checksum_out) != H5FD_ONION_ENCODED_SIZE_HEADER)
        TEST_ERROR;

    if (checksum != checksum_out)
        TEST_ERROR;

    for (i = 0; i < H5FD_ONION_ENCODED_SIZE_HEADER; i++) {
        if (exp[i] != buf[i]) {
            printf("first mismatched byte at %zu: %02x %02x\n", i, exp[i], buf[i]);
            TEST_ERROR;
        }
    }

    hdr_out.version      = H5FD_ONION_HEADER_VERSION_CURR;
    hdr_out.flags        = 0;
    hdr_out.page_size    = 0;
    hdr_out.history_addr = 0;
    hdr_out.history_size = 0;

    /* Invalid header signature prevents decoding.
     */

    exp[3] = 'X'; /* invalidate encoded signature */
    H5E_BEGIN_TRY
    {
        size_ret = H5FD__onion_header_decode(exp, &hdr_out);
    }
    H5E_END_TRY
    if (0 != size_ret)
        TEST_ERROR;

    exp[3] = 'H'; /* reset */

    /* Invalid header version prevents decoding.
     */

    exp[4] = 0; /* encoded version 0?!? */
    H5E_BEGIN_TRY
    {
        size_ret = H5FD__onion_header_decode(exp, &hdr_out);
    }
    H5E_END_TRY
    if (0 != size_ret)
        TEST_ERROR;

    exp[4] = H5FD_ONION_HEADER_VERSION_CURR + 1; /* encoded super-version?! */
    H5E_BEGIN_TRY
    {
        size_ret = H5FD__onion_header_decode(exp, &hdr_out);
    }
    H5E_END_TRY
    if (0 != size_ret)
        TEST_ERROR;

    /* Reset */
    exp[4] = H5FD_ONION_HEADER_VERSION_CURR;

    /* Valid header can be decoded */

    if (H5FD__onion_header_decode(buf, &hdr_out) != H5FD_ONION_ENCODED_SIZE_HEADER)
        TEST_ERROR;
    if (H5FD_ONION_HEADER_VERSION_CURR != hdr_out.version)
        TEST_ERROR;
    if (hdr.flags != hdr_out.flags)
        TEST_ERROR;
    if (hdr.page_size != hdr_out.page_size)
        TEST_ERROR;
    if (hdr.history_addr != hdr_out.history_addr)
        TEST_ERROR;
    if (hdr.history_size != hdr_out.history_size)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    return -1;
} /* end test_header_encode_decode() */

/*-----------------------------------------------------------------------------
 * Function:    test_history_encode_decode_empty()
 *
 * Purpose:     Verify onion history encoding and decoding behavior.
 *              Tests the case of the "empty" history.
 *              Verifies behavior in standard error cases.
 *
 * Return:      PASSED : 0
 *              FAILED : -1
 *-----------------------------------------------------------------------------
 */
static int
test_history_encode_decode_empty(void)
{
    unsigned char buf[32];
    unsigned char exp[32] = {
        'O', 'W', 'H', 'S',                        /* NOTE: match signature define in onion_priv.h */
        1,   0,   0,   0,                          /* NOTE: update version w/ "current" as needed */
        0,   0,   0,   0,   0, 0, 0, 0, 0, 0, 0, 0 /* sum populated below */
    };
    unsigned char       *ptr          = NULL;
    uint32_t             checksum     = 0;
    uint32_t             checksum_out = 0;
    size_t               i            = 0;
    uint64_t             size_ret     = 0;
    H5FD_onion_history_t history      = {
        H5FD_ONION_HISTORY_VERSION_CURR, 0, /* n_revisions */
        NULL,                               /* list */
        0,                                  /* checksum */
    };
    H5FD_onion_history_t history_out = {
        H5FD_ONION_HISTORY_VERSION_CURR, 0, /* n_revisions */
        NULL,                               /* list */
        0,                                  /* checksum */
    };

    TESTING("encode/decode history (empty and failures)");

    /* Generate checksum but don't store it yet */
    checksum = H5_checksum_fletcher32(exp, H5FD_ONION_ENCODED_SIZE_HISTORY - 4);
    ptr      = exp + H5FD_ONION_ENCODED_SIZE_HISTORY - 4;
    UINT32ENCODE(ptr, checksum);

    if (H5FD__onion_history_encode(&history, buf, &checksum_out) != H5FD_ONION_ENCODED_SIZE_HISTORY)
        TEST_ERROR;
    for (i = 0; i < 20; i++) {
        if (exp[i] != buf[i]) {
            printf("first mismatched byte at %zu: %02x %02x\n", i, exp[i], buf[i]);
            TEST_ERROR;
        }
    }
    if (checksum != checksum_out)
        TEST_ERROR;
    history.checksum = checksum; /* set to compare later */

    /* Invalid signature prevents decoding */

    exp[3] = 'X'; /* invalidate encoded signature */
    H5E_BEGIN_TRY
    {
        size_ret = H5FD__onion_history_decode(exp, &history_out);
    }
    H5E_END_TRY
    if (0 != size_ret)
        TEST_ERROR;

    exp[3] = 'H'; /* reset */

    /* Invalid version prevents decoding */

    exp[4] = 0; /* encoded version 0?!? */
    H5E_BEGIN_TRY
    {
        size_ret = H5FD__onion_history_decode(exp, &history_out);
    }
    H5E_END_TRY
    if (0 != size_ret)
        TEST_ERROR;

    exp[4] = H5FD_ONION_HISTORY_VERSION_CURR + 1;
    H5E_BEGIN_TRY
    {
        size_ret = H5FD__onion_history_decode(exp, &history_out);
    }
    H5E_END_TRY
    if (0 != size_ret)
        TEST_ERROR;

    exp[4] = H5FD_ONION_HISTORY_VERSION_CURR; /* reset */

    /* Valid summary can be decoded */

    if (H5FD__onion_history_decode(buf, &history_out) != H5FD_ONION_ENCODED_SIZE_HISTORY)
        TEST_ERROR;
    if (H5FD_ONION_HISTORY_VERSION_CURR != history_out.version)
        TEST_ERROR;
    if (history.n_revisions != history_out.n_revisions)
        TEST_ERROR;
    if (history.checksum != history_out.checksum)
        TEST_ERROR;
    if (NULL != history_out.record_locs)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    return -1;
} /* end test_history_encode_decode_empty() */

/*-----------------------------------------------------------------------------
 * Function:    test_history_encode_decode()
 *
 * Purpose:     Verify onion history encoding and decoding behavior.
 *              Encode/decode with some set of revision record pointers.
 *
 * Return:      PASSED : 0
 *              FAILED : -1
 *-----------------------------------------------------------------------------
 */
static int
test_history_encode_decode(void)
{
    unsigned char *buf     = NULL;
    unsigned char  exp[80] = {
        'O', 'W', 'H', 'S', /* NOTE: match signature define in onion_priv.h */
        1, 0, 0, 0,         /* NOTE: update version w/ "current" as needed */
        3, 0, 0, 0, 0, 0, 0, 0,
        /* rev0 pointer */
        56, 2, 0, 0, 0, 0, 0, 0, 238, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* sum populated below */
        /* rev1 pointer */
        121, 173, 3, 0, 0, 0, 0, 0, 203, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* sum populated below */
        /* rev2 pointer */
        96, 158, 52, 198, 213, 0, 0, 0, 240, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* sum populated below */
        /* final checksum */
        0, 0, 0, 0 /* sum populated below */
    };
    unsigned char       *buf_p        = NULL;
    uint32_t             checksum_out = 0;
    size_t               i            = 0;
    H5FD_onion_history_t history      = {
        H5FD_ONION_HISTORY_VERSION_CURR, 3, /* n_revisions */
        NULL,                               /* list set below */
        0,                                  /* checksum  not set by us */
    };
    H5FD_onion_history_t history_out = {
        H5FD_ONION_HISTORY_VERSION_CURR, 0, /* n_revisions must start as zero */
        NULL,                               /* list */
        0,                                  /* checksum */
    };
    size_t exp_size =
        H5FD_ONION_ENCODED_SIZE_HISTORY + H5FD_ONION_ENCODED_SIZE_RECORD_POINTER * history.n_revisions;

    TESTING("encode/decode history");

    if (80 != exp_size)
        TEST_ERROR;

    history.record_locs = calloc(history.n_revisions, sizeof(H5FD_onion_record_loc_t));
    if (NULL == history.record_locs)
        TEST_ERROR;

    /* Must match values in exp */
    history.record_locs[0].phys_addr   = 568ull;
    history.record_locs[0].record_size = 238ull;
    history.record_locs[1].phys_addr   = 241017ull;
    history.record_locs[1].record_size = 4555ull;
    history.record_locs[2].phys_addr   = 918153371232ull;
    history.record_locs[2].record_size = 240ull;

    /* Populate revision pointer sums in exp */
    for (i = 0; i < history.n_revisions; i++) {
        uint64_t history_pre = H5FD_ONION_ENCODED_SIZE_HISTORY - 4;
        uint64_t ptr_pre     = H5FD_ONION_ENCODED_SIZE_RECORD_POINTER - 4;
        uint64_t ptr_size    = H5FD_ONION_ENCODED_SIZE_RECORD_POINTER;

        buf_p                           = exp + history_pre + ptr_size * i;
        history.record_locs[i].checksum = H5_checksum_fletcher32(buf_p, ptr_pre);
        buf_p += ptr_pre;
        UINT32ENCODE(buf_p, history.record_locs[i].checksum);
    }

    /* Compute, populate, and store exp final sum */
    history.checksum = H5_checksum_fletcher32(exp, exp_size - 4);
    buf_p            = exp + exp_size - 4;
    UINT32ENCODE(buf_p, history.checksum);

    if (NULL == (buf = malloc(exp_size)))
        TEST_ERROR;

    if (H5FD__onion_history_encode(&history, buf, &checksum_out) != exp_size)
        TEST_ERROR;
    for (i = 0; i < exp_size; i++) {
        if (exp[i] != buf[i])
            TEST_ERROR;
    }
    if (history.checksum != checksum_out)
        TEST_ERROR;

    /* Initial decode, gets always-present components */

    history_out.n_revisions = 0; /* must be initialized to 0 */
    if (H5FD__onion_history_decode(exp, &history_out) != exp_size)
        TEST_ERROR;
    if (H5FD_ONION_HISTORY_VERSION_CURR != history_out.version)
        TEST_ERROR;
    if (history.n_revisions != history_out.n_revisions)
        TEST_ERROR;
    /* Must be created by us */
    if (NULL != history_out.record_locs)
        TEST_ERROR;

    /* True decode requires allocating space for record pointers */

    history_out.record_locs = calloc(history_out.n_revisions, sizeof(H5FD_onion_record_loc_t));
    if (NULL == history_out.record_locs)
        TEST_ERROR;

    if (H5FD__onion_history_decode(exp, &history_out) != exp_size)
        TEST_ERROR;
    if (H5FD_ONION_HISTORY_VERSION_CURR != history_out.version)
        TEST_ERROR;
    if (history.n_revisions != history_out.n_revisions)
        TEST_ERROR;
    if (history.checksum != history_out.checksum)
        TEST_ERROR;
    if (NULL == history_out.record_locs)
        TEST_ERROR;
    for (i = 0; i < history.n_revisions; i++) {
        H5FD_onion_record_loc_t exp_rp = history.record_locs[i];
        H5FD_onion_record_loc_t act_rp = history_out.record_locs[i];

        if (exp_rp.phys_addr != act_rp.phys_addr)
            TEST_ERROR;
        if (exp_rp.record_size != act_rp.record_size)
            TEST_ERROR;
        if (exp_rp.checksum != act_rp.checksum)
            TEST_ERROR;
    }

    free(history_out.record_locs);
    free(buf);
    free(history.record_locs);

    PASSED();
    return 0;

error:
    free(history_out.record_locs);
    free(buf);
    free(history.record_locs);

    return -1;
} /* end test_history_encode_decode() */

/*-----------------------------------------------------------------------------
 *
 * Function:    test_revision_record_encode_decode()
 *
 * Purpose:     Verify onion revision-record encoding and decoding behavior.
 *
 * Return:      PASSED : 0
 *              FAILED : -1
 *
 *-----------------------------------------------------------------------------
 */
static int
test_revision_record_encode_decode(void)
{
    /* clang-format off */
    /* Byte array of expected values (FRAGILE!) */
    unsigned char exp[173] = {
        'O', 'R', 'R', 'S',                     /* Bytes 000-003:   signature */
        1, 0, 0, 0,                             /* Bytes 004-007:   version */
        5, 0, 0, 0, 0, 0, 0, 0,                 /* Bytes 008-015:   revision ID */
        2, 0, 0, 0, 0, 0, 0, 0,                 /* Bytes 016-023:   parent revision ID */
        '1', '9', '4', '1', '1', '2', '0', '7', /* Bytes 024-039:   time of creation */
        'T', '1', '9', '0', '6', '4', '3', 'Z', /*                  ... */
        0x11, 0x00, 0, 0, 0x02, 0, 0, 0,        /* Bytes 040-047:   logical file size */
        0, 16, 0, 0,                            /* Bytes 048-051:   page size */
        4, 0, 0, 0, 0, 0, 0, 0,                 /* Bytes 052-059:   # entries */
        25, 0, 0, 0,                            /* Bytes 060-063:   comment size */
        /* ENTRY 0 */
        0, 0xB0, 0x1E, 0, 0, 0, 0, 0,           /* Bytes 064-071:   entry 0: logical offset */
        0x4B, 0x02, 0, 0, 0, 0, 0, 0,           /* Bytes 072-079:   entry 0: physical address */
        0, 0, 0, 0,                             /* Bytes 080-083:   checksum (populated below) */
        /* ENTRY 1 */
        0, 0xF0, 0x2E, 0, 0, 0, 0, 0,           /* Bytes 084-091:   entry 1: logical offset */
        0xA7, 0, 0, 0, 0, 0, 0, 0,              /* Bytes 092-099:   entry 1: physical address */
        0, 0, 0, 0,                             /* Bytes 100-103:   checksum (populated below) */
        /* ENTRY 2 */
        0, 0x50, 0x15, 0, 0, 0x20, 0, 0,        /* Bytes 104-111:   entry 2: logical offset */
        0x11, 0, 0, 0, 0x02, 0, 0, 0,           /* Bytes 112-119:   entry 2: physical address */
        0, 0, 0, 0,                             /* Bytes 120-123:   checksum (populated below) */
        /* ENTRY 3 */
        0, 0xE0, 0x24, 0, 0, 0, 0, 0,           /* Bytes 124-131:   entry 3: logical offset */
        0xB1, 0x01, 0, 0, 0, 0, 0, 0,           /* Bytes 132-139:   entry 3: physical address */
        0, 0, 0, 0,                             /* Bytes 140-143:   checksum (populated below) */
        'E', 'x', 'a', 'm', 'p', 'l', 'e', ' ', /* Bytes 144-168:   comment */
        'c', 'o', 'm', 'm', 'e', 'n', 't', ' ', /*                  ... */
        'm', 'e', 's', 's', 'a', 'g', 'e', '.', /*                  ... */
        '\0',                                   /*                  ... */
        0, 0, 0, 0                              /* Bytes 169-172:   final checksum (populated below) */
    };
    /* clang-format on */
    unsigned char               *buf   = NULL;
    unsigned char               *buf_p = NULL;
    size_t                       i     = 0;
    uint64_t                     size_ret;
    H5FD_onion_revision_record_t r_out;
    uint32_t                     checksum    = 0;
    bool                         badness     = false;
    char                         comment[25] = "Example comment message.";
    H5FD_onion_revision_record_t record      = {
        H5FD_ONION_REVISION_RECORD_VERSION_CURR,
        5,             /* revision ID */
        2,             /* parent revision ID */
        {'\0'},        /* time of creation - populated below */
        8589934609ull, /* logical file size */
        {
            H5FD_ONION_ARCHIVAL_INDEX_VERSION_CURR, /* version */
            12,                                     /* page_size_log2 */
            4,                                      /* n_entries */
            NULL,                                   /* list - populated below */
        },                                          /* archival index struct */
        25,                                         /* comment size */
        comment,                                    /* comment */
        0,                                          /* checksum (computed later) */
    };
    size_t exp_size = H5FD_ONION_ENCODED_SIZE_REVISION_RECORD +
                      (H5FD_ONION_ENCODED_SIZE_INDEX_ENTRY * record.archival_index.n_entries) +
                      strlen("Example comment message.") + 1;

    r_out.archival_index.list = NULL;
    r_out.comment             = NULL;

    TESTING("encode/decode revision record");

    memcpy(record.time_of_creation, "19411207T190643Z", 16);
    record.archival_index.list = calloc(record.archival_index.n_entries, sizeof(H5FD_onion_index_entry_t));
    if (NULL == record.archival_index.list)
        TEST_ERROR;

    /* Convert logical_page and should match address in expected buffer */
    record.archival_index.list[0].logical_page = 491ull;
    record.archival_index.list[0].phys_addr    = 587ull;
    record.archival_index.list[1].logical_page = 751ull;
    record.archival_index.list[1].phys_addr    = 167ull;
    record.archival_index.list[2].logical_page = 8589934933ull;
    record.archival_index.list[2].phys_addr    = 8589934609ull;
    record.archival_index.list[3].logical_page = 590ull;
    record.archival_index.list[3].phys_addr    = 433ull;

    /* Set expected checksum for each archival index entry in buffer */
    for (i = 0; i < record.archival_index.n_entries; i++) {
        uint64_t rec_pre  = H5FD_ONION_ENCODED_SIZE_REVISION_RECORD - 4;
        uint64_t idx_pre  = H5FD_ONION_ENCODED_SIZE_INDEX_ENTRY - 4;
        uint64_t idx_size = H5FD_ONION_ENCODED_SIZE_INDEX_ENTRY;

        buf_p    = exp + rec_pre + idx_size * i;
        checksum = H5_checksum_fletcher32(buf_p, idx_pre);
        buf_p += idx_pre;
        UINT32ENCODE(buf_p, checksum);
    }

    checksum = 0;

    record.checksum = H5_checksum_fletcher32(exp, exp_size - 4);
    buf_p           = exp + exp_size - 4;
    UINT32ENCODE(buf_p, record.checksum);

    /* Required initialization for record-out structure */
    r_out.version                  = H5FD_ONION_REVISION_RECORD_VERSION_CURR;
    r_out.comment_size             = 0;
    r_out.comment                  = NULL;
    r_out.archival_index.version   = H5FD_ONION_ARCHIVAL_INDEX_VERSION_CURR;
    r_out.archival_index.n_entries = 0;
    r_out.archival_index.list      = NULL;

    if (NULL == (buf = malloc(sizeof(unsigned char) * exp_size)))
        TEST_ERROR;

    /* Test encode */

    if (H5FD__onion_revision_record_encode(&record, buf, &checksum) != exp_size)
        TEST_ERROR;

    for (i = 0; i < exp_size; i++) {
        if (exp[i] != buf[i]) {
            badness = true;
            printf("Bad encoded record - Index %zu: expected 0x%02X but got 0x%02X\n", i,
                   (unsigned int)exp[i], (unsigned int)buf[i]);
        }
    }
    if (badness) {
        /* If this fragile test breaks, this information is helpful... */
        printf("INDEX\n");
        for (i = 0; i < exp_size; i++)
            printf("%4zu ", i);
        printf("\n");

        printf("EXPECTED\n");
        for (i = 0; i < exp_size; i++)
            printf("0x%02X ", (unsigned int)exp[i]);
        printf("\n");

        printf("ACTUAL\n");
        for (i = 0; i < exp_size; i++)
            printf("0x%02X ", (unsigned int)buf[i]);
        printf("\n");
    }
    if (badness)
        TEST_ERROR;
    if (record.checksum != checksum)
        TEST_ERROR;

    /* Test decode (malformed encoding) */

    /* Invalid signature */
    exp[2] = 'Y';
    H5E_BEGIN_TRY
    {
        size_ret = H5FD__onion_revision_record_decode(exp, &r_out);
    }
    H5E_END_TRY
    if (0 != size_ret)
        TEST_ERROR;
    exp[2] = 'R'; /* reset */

    /* Zero version */
    exp[4] = 0;
    H5E_BEGIN_TRY
    {
        size_ret = H5FD__onion_revision_record_decode(exp, &r_out);
    }
    H5E_END_TRY
    if (0 != size_ret)
        TEST_ERROR;

    /* Advance version */
    exp[4] = H5FD_ONION_REVISION_RECORD_VERSION_CURR + 1;
    H5E_BEGIN_TRY
    {
        size_ret = H5FD__onion_revision_record_decode(exp, &r_out);
    }
    H5E_END_TRY
    if (0 != size_ret)
        TEST_ERROR;
    exp[4] = H5FD_ONION_REVISION_RECORD_VERSION_CURR; /* reset */

    /* Test successful decode */

    /* Initial decode; get variable-length component sizes */
    if (H5FD__onion_revision_record_decode(exp, &r_out) != exp_size)
        TEST_ERROR;
    if (record.comment_size != r_out.comment_size)
        TEST_ERROR;
    if (record.archival_index.n_entries != r_out.archival_index.n_entries)
        TEST_ERROR;

    /* Allocate variable-length components */
    r_out.comment = calloc(r_out.comment_size, sizeof(char));
    if (NULL == r_out.comment)
        TEST_ERROR;
    r_out.archival_index.list = calloc(r_out.archival_index.n_entries, sizeof(H5FD_onion_index_entry_t));
    if (NULL == r_out.archival_index.list)
        TEST_ERROR;

    /* Decode into all components */
    if (H5FD__onion_revision_record_decode(exp, &r_out) != exp_size)
        TEST_ERROR;
    if (H5FD_ONION_REVISION_RECORD_VERSION_CURR != r_out.version)
        TEST_ERROR;
    if (record.revision_num != r_out.revision_num)
        TEST_ERROR;
    if (record.parent_revision_num != r_out.parent_revision_num)
        TEST_ERROR;
    if (record.parent_revision_num != r_out.parent_revision_num)
        TEST_ERROR;
    if (record.checksum != r_out.checksum)
        TEST_ERROR;
    if (strncmp(record.time_of_creation, r_out.time_of_creation, 16) != 0)
        TEST_ERROR;
    if (record.comment_size != r_out.comment_size)
        TEST_ERROR;
    if (record.comment_size != strlen(r_out.comment) + 1)
        TEST_ERROR;
    if (strlen(record.comment) != strlen(r_out.comment))
        TEST_ERROR;
    if (strcmp(record.comment, r_out.comment) != 0)
        TEST_ERROR;

    if (H5FD_ONION_ARCHIVAL_INDEX_VERSION_CURR != r_out.archival_index.version)
        TEST_ERROR;
    if (record.archival_index.page_size_log2 != r_out.archival_index.page_size_log2)
        TEST_ERROR;
    if (record.archival_index.n_entries != r_out.archival_index.n_entries)
        TEST_ERROR;
    for (i = 0; i < record.archival_index.n_entries; i++) {
        H5FD_onion_index_entry_t *ep = &record.archival_index.list[i];
        H5FD_onion_index_entry_t *ap = &r_out.archival_index.list[i];

        if (ep->phys_addr != ap->phys_addr)
            TEST_ERROR;
        if (ep->logical_page != ap->logical_page)
            TEST_ERROR;
    }

    /* Cleanup */

    free(r_out.archival_index.list);
    free(r_out.comment);
    free(buf);
    free(record.archival_index.list);

    PASSED();
    return 0;

error:
    free(r_out.archival_index.list);
    free(r_out.comment);
    free(buf);
    free(record.archival_index.list);

    return -1;
} /* end test_revision_record_encode_decode() */

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Use VFL to open target file and check that its bytes exactly match those
 * of given buffer 'exp'[ected].
 *
 * Returns 0 if successful, -1 if error or mismatch.
 *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */
static int
compare_file_bytes_exactly(const char *filepath, hid_t fapl_id, size_t nbytes, const unsigned char *exp)
{
    H5FD_t        *raw_vfile = NULL; /* virtual file to look at raw file contents */
    unsigned char *act_buf   = NULL; /* allocated area for actual file bytes */
    uint64_t       filesize  = 0;

    if (NULL == (raw_vfile = H5FDopen(filepath, H5F_ACC_RDONLY, fapl_id, HADDR_UNDEF)))
        TEST_ERROR;

    /* filesize is wrong w/ stdio - it's zero instead of 40 or whatnot */
    filesize = (uint64_t)H5FDget_eof(raw_vfile, H5FD_MEM_DRAW);
    if ((uint64_t)nbytes != filesize) {
        fprintf(stderr, "\nSizes not the same - nbytes: %zu, filesize: %" PRIu64 "\n", nbytes, filesize);
        TEST_ERROR;
    }

    if (NULL == (act_buf = malloc(nbytes)))
        TEST_ERROR;
    /* Fill buffer with bogus UCHAR_MAX values */
    for (size_t i = 0; i < nbytes; i++)
        act_buf[i] = UCHAR_MAX;
    if (H5FDset_eoa(raw_vfile, H5FD_MEM_DRAW, nbytes) < 0)
        TEST_ERROR;
    if (H5FDread(raw_vfile, H5FD_MEM_DRAW, H5P_DEFAULT, 0, nbytes, act_buf) < 0)
        TEST_ERROR;

    /* Compare raw bytes data */
    for (size_t i = 0; i < nbytes; i++) {
        if (exp[i] != act_buf[i]) {
            printf("first mismatched byte %zu: expected 0x%02X was 0x%02X\n", i, exp[i], act_buf[i]);
            TEST_ERROR;
        }
    }

    if (H5FDclose(raw_vfile) < 0)
        TEST_ERROR;
    free(act_buf);

    return 0;

error:
    if (raw_vfile != NULL)
        H5FDclose(raw_vfile);
    free(act_buf);

    return -1;
} /* end compare_file_bytes_exactly() */

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Do a manual read of the onion history (separate, single "Onion" file).
 * Verify that the history data is well-formed and matches the expected state.
 *
 * Inspect file contents on backing store.
 * Return -1 on problem, 0 if okay.
 *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */
static int
verify_history_as_expected_onion(H5FD_t *raw_file, struct expected_history *filter)
{
    unsigned char               *buf = NULL; /* allocated area for actual file bytes */
    H5FD_onion_header_t          hdr_out;
    H5FD_onion_history_t         history_out;
    H5FD_onion_revision_record_t rev_out;
    uint64_t                     filesize     = 0;
    uint64_t                     readsize     = 0;
    uint8_t                     *ui8p         = NULL;
    uint32_t                     buf_checksum = 0;

    /* memset to avoid bad frees on errors */
    memset(&rev_out, 0, sizeof(H5FD_onion_revision_record_t));
    memset(&history_out, 0, sizeof(H5FD_onion_history_t));

    hdr_out.version = H5FD_ONION_HEADER_VERSION_CURR;

    history_out.version     = H5FD_ONION_HISTORY_VERSION_CURR;
    history_out.n_revisions = 0;
    history_out.record_locs = NULL;

    rev_out.version                = H5FD_ONION_REVISION_RECORD_VERSION_CURR;
    rev_out.archival_index.version = H5FD_ONION_ARCHIVAL_INDEX_VERSION_CURR;

    filesize = (uint64_t)H5FDget_eof(raw_file, H5FD_MEM_DRAW);
    if (H5FDset_eoa(raw_file, H5FD_MEM_DRAW, filesize) < 0)
        TEST_ERROR;

    /* Ingest onion header */

    readsize = MIN(filesize, H5FD_ONION_ENCODED_SIZE_HEADER);
    if (NULL == (buf = malloc(readsize * sizeof(unsigned char))))
        TEST_ERROR;
    if (H5FDread(raw_file, H5FD_MEM_DRAW, H5P_DEFAULT, 0, readsize, buf) < 0)
        TEST_ERROR;

    readsize = H5FD__onion_header_decode(buf, &hdr_out);
    if (0 == readsize)
        TEST_ERROR;
    if (H5FD_ONION_HEADER_VERSION_CURR != hdr_out.version)
        TEST_ERROR;
    /* Decode from the buffer to we can compare on BE systems */
    ui8p = (uint8_t *)(&buf[readsize - 4]);
    UINT32DECODE(ui8p, buf_checksum);
    if (hdr_out.checksum != buf_checksum)
        TEST_ERROR;
    if (hdr_out.checksum != H5_checksum_fletcher32(buf, readsize - 4))
        TEST_ERROR;
    if (filter->page_size != hdr_out.page_size)
        TEST_ERROR;
    if (hdr_out.history_addr + hdr_out.history_size != filesize)
        TEST_ERROR;
    if (filter->origin_eof != hdr_out.origin_eof)
        TEST_ERROR;

    free(buf);
    buf = NULL;

    /* Ingest history */

    readsize = hdr_out.history_size;
    if (NULL == (buf = malloc(readsize * sizeof(unsigned char))))
        TEST_ERROR;
    if (H5FDread(raw_file, H5FD_MEM_DRAW, H5P_DEFAULT, hdr_out.history_addr, readsize, buf) < 0)
        TEST_ERROR;

    /* Initial read, get count of revisions */
    readsize = H5FD__onion_history_decode(buf, &history_out);
    if (0 == readsize)
        TEST_ERROR;
    if (H5FD_ONION_HISTORY_VERSION_CURR != history_out.version)
        TEST_ERROR;
    /* Decode from the buffer to we can compare on BE systems */
    ui8p = (uint8_t *)(&buf[readsize - 4]);
    UINT32DECODE(ui8p, buf_checksum);
    if (history_out.checksum != buf_checksum)
        TEST_ERROR;
    if (history_out.checksum != H5_checksum_fletcher32(buf, readsize - 4))
        TEST_ERROR;
    if (filter->n_revisions != history_out.n_revisions)
        TEST_ERROR;

    /* Final read, populate pointers to revision records */
    history_out.record_locs = calloc(history_out.n_revisions, sizeof(H5FD_onion_record_loc_t));
    if (NULL == history_out.record_locs)
        TEST_ERROR;
    if (H5FD__onion_history_decode(buf, &history_out) != readsize)
        TEST_ERROR;

    /* Reuse buffer space to sanity-check checksum for record pointer(s). */
    assert(readsize >= sizeof(H5FD_onion_record_loc_t));
    for (size_t i = 0; i < history_out.n_revisions; i++) {

        uint64_t phys_addr;
        uint64_t record_size;

        /* Do a checked assignment from the struct value into appropriately
         * sized types. We don't have access to the H5F_t struct for this
         * file, so we can't use the offset/length macros in H5Fprivate.h.
         *
         * Have to do an encode to get these values so the test passes on BE
         * systems.
         */
        H5_CHECKED_ASSIGN(phys_addr, uint64_t, history_out.record_locs[i].phys_addr, haddr_t);
        H5_CHECKED_ASSIGN(record_size, uint64_t, history_out.record_locs[i].record_size, hsize_t);

        ui8p = (uint8_t *)buf;
        UINT64ENCODE(ui8p, phys_addr);

        ui8p = (uint8_t *)(buf + 8);
        UINT64ENCODE(ui8p, record_size);

        if (history_out.record_locs[i].checksum != H5_checksum_fletcher32(buf, 16))
            TEST_ERROR;
    }

    free(buf);
    buf = NULL;

    /* Ingest revision(s) */

    for (size_t i = 0; i < history_out.n_revisions; i++) {
        H5FD_onion_record_loc_t  *rpp = &history_out.record_locs[i];
        struct expected_revision *erp = &filter->revisions[i];

        rev_out.archival_index.list           = NULL;
        rev_out.archival_index.n_entries      = 0;
        rev_out.archival_index.page_size_log2 = 0;
        rev_out.comment_size                  = 0;
        rev_out.comment                       = NULL;

        readsize = rpp->record_size;
        if (NULL == (buf = malloc((size_t)rpp->record_size)))
            TEST_ERROR;
        if (H5FDread(raw_file, H5FD_MEM_DRAW, H5P_DEFAULT, rpp->phys_addr, rpp->record_size, buf) < 0)
            TEST_ERROR;

        /* Initial revision read -- get fixed components */
        readsize = H5FD__onion_revision_record_decode(buf, &rev_out);
        if (0 == readsize)
            TEST_ERROR;
        if (rpp->record_size != readsize)
            TEST_ERROR;
        if (H5FD_ONION_REVISION_RECORD_VERSION_CURR != rev_out.version)
            TEST_ERROR;
        /* Decode from the buffer to we can compare on BE systems */
        ui8p = (uint8_t *)(&buf[readsize - 4]);
        UINT32DECODE(ui8p, buf_checksum);
        if (rev_out.checksum != buf_checksum)
            TEST_ERROR;
        if (rev_out.checksum != H5_checksum_fletcher32(buf, readsize - 4))
            TEST_ERROR;
        if (erp->revision_num != rev_out.revision_num)
            TEST_ERROR;
        if (erp->parent_revision_num != rev_out.parent_revision_num)
            TEST_ERROR;
        if (erp->logical_eof != rev_out.logical_eof)
            TEST_ERROR;

        /* Final read, get variable-length data */
        if (NULL == (rev_out.comment = malloc((size_t)rev_out.comment_size)))
            TEST_ERROR;
        rev_out.archival_index.list =
            calloc(rev_out.archival_index.n_entries, sizeof(H5FD_onion_index_entry_t));
        if (NULL == rev_out.archival_index.list)
            TEST_ERROR;

        readsize = H5FD__onion_revision_record_decode(buf, &rev_out);
        if (rpp->record_size != readsize)
            TEST_ERROR;

        /* Compare revision info with expected filter */
        if (erp->comment == NULL) {
            if (rev_out.comment_size != 0)
                TEST_ERROR;
        }
        else {
            if (strlen(rev_out.comment) != strlen(erp->comment))
                TEST_ERROR;
            if (strcmp(rev_out.comment, erp->comment) != 0)
                TEST_ERROR;
        }
        if (erp->n_index_entries != (uint64_t)(-1) &&
            erp->n_index_entries != rev_out.archival_index.n_entries)
            TEST_ERROR;

        free(buf);
        free(rev_out.comment);
        free(rev_out.archival_index.list);
    }

    free(history_out.record_locs);
    history_out.record_locs = NULL;

    return 0;

error:
    free(buf);
    free(rev_out.comment);
    free(rev_out.archival_index.list);
    free(history_out.record_locs);

    return -1;

} /* end verify_history_as_expected_onion() */

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 * Verify file bytes on the backing store
 * + onion storage target
 * + create from nothing
 * + stage 0 (initializing)
 * + open (not yet written)
 *     + "Empty" .h5 file created
 *     + .onion file created w/ only header (0 whole-hist addr)
 *     + .onion.recovery created w/ "empty" history
 *     + Cannot open onionized canonical file (incomplete history, no rev)
 *
 * Inspect file contents on backing store.
 * Return -1 on problem, 0 if okay.
 *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */
static int
verify_stored_onion_create_0_open(struct onion_filepaths *paths, H5FD_onion_fapl_info_t *onion_info)
{
    H5FD_t        *file            = NULL; /* virtual file to look at raw file contents */
    unsigned char *act_buf         = NULL; /* allocated area for actual file bytes */
    hid_t          fapl_id         = onion_info->backing_fapl_id;
    herr_t         err_ret         = FAIL;
    unsigned char  hdr_exp_bytes[] = {
        'O', 'H', 'D', 'H', 1, 1, 0, 0, 0, 0, 0, 0, /* page-size encoded below */
        0,   0,   0,   0,   0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0,   0,   20,  0,   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* checksum encoded below */
    };
    size_t        history_exp_bytes_size = 20;
    unsigned char history_exp_bytes[]    = {
        'O', 'W', 'H', 'S', 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /* checksum encoded below */
    };
    unsigned char *ptr      = NULL;
    uint32_t       checksum = 0;

    /* Finish populating expected header bytes */
    ptr = hdr_exp_bytes + 8; /* WARNING: must match format */
    UINT32ENCODE(ptr, onion_info->page_size);
    checksum = H5_checksum_fletcher32(hdr_exp_bytes, H5FD_ONION_ENCODED_SIZE_HEADER - 4);
    ptr      = hdr_exp_bytes + H5FD_ONION_ENCODED_SIZE_HEADER - 4;
    UINT32ENCODE(ptr, checksum);
    ptr = NULL;

    /* Finish populating expected history bytes */
    checksum = H5_checksum_fletcher32(history_exp_bytes, H5FD_ONION_ENCODED_SIZE_HISTORY - 4);
    ptr      = history_exp_bytes + H5FD_ONION_ENCODED_SIZE_HISTORY - 4;
    UINT32ENCODE(ptr, checksum);
    ptr = NULL;

    /* Look at h5 file: should have zero bytes */

    file = H5FDopen(paths->canon, H5F_ACC_RDONLY, fapl_id, HADDR_UNDEF);
    if (NULL == file)
        TEST_ERROR;

    /* Size here is arbitrary */
    if (NULL == (act_buf = calloc(1, 8)))
        TEST_ERROR;

    /* Should fail when reading from an empty file */
    H5E_BEGIN_TRY
    {
        err_ret = H5FDread(file, H5FD_MEM_DRAW, H5P_DEFAULT, 0, 1, act_buf);
    }
    H5E_END_TRY
    if (err_ret != FAIL)
        TEST_ERROR;

    free(act_buf);
    act_buf = NULL;

    if (H5FDclose(file) < 0)
        TEST_ERROR;
    file = NULL;

    /* Look at onion file: should have header */
    if (compare_file_bytes_exactly(paths->onion, fapl_id, H5FD_ONION_ENCODED_SIZE_HEADER, hdr_exp_bytes) < 0)
        TEST_ERROR;

    /* Look at history backing file: should have nascent history */
    if (compare_file_bytes_exactly(paths->recovery, fapl_id, history_exp_bytes_size, history_exp_bytes) < 0)
        TEST_ERROR;

    /* Inspect .h5 file contents */
    if (compare_file_bytes_exactly(paths->canon, fapl_id, 8, (const unsigned char *)"ONIONEOF") < 0)
        TEST_ERROR;

    return 0;

error:
    if (file != NULL)
        (void)H5FDclose(file);
    free(act_buf);

    return -1;
} /* end verify_stored_onion_create_0_open() */

/*-----------------------------------------------------------------------------
 * Function:    test_create_oniontarget()
 *
 * Purpose:     Test the ability of the Onion VFD to create a valid
 *              'onionized' file.
 *
 *              When `truncate_canonical` is false, the canonical file is
 *              nonexistent on the backing store on onion-creation.
 *              When `truncate_canonical` is true, a canonical file is created
 *              on the backing store with known contents, which are to be
 *              truncated on onion-creation.
 *
 * Return:      PASSED : 0
 *              FAILED : -1
 *-----------------------------------------------------------------------------
 */
static int
test_create_oniontarget(bool truncate_canonical, bool with_initial_data)
{
    const char             *basename   = "somesuch";
    hid_t                   fapl_id    = H5I_INVALID_HID;
    struct onion_filepaths *paths      = NULL;
    H5FD_onion_fapl_info_t  onion_info = {
        H5FD_ONION_FAPL_INFO_VERSION_CURR,
        H5P_DEFAULT,                   /* backing_fapl_id  */
        ONION_TEST_PAGE_SIZE_5,        /* page_size        */
        H5FD_ONION_STORE_TARGET_ONION, /* store_target     */
        H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST,
        0,               /* force_write_open */
        0,               /* creation_flags   */
        "initial commit" /* comment          */
    };
    H5FD_t                 *vfile_raw = NULL; /* virtual file to look at raw file contents */
    H5FD_t                 *vfile_rw  = NULL; /* Onion virtual file for read/write */
    H5FD_t                 *vfile_ro  = NULL; /* Onion virtual file for read-only */
    struct expected_history filter;
    char                   *buf = NULL;

    if (true == truncate_canonical && true == with_initial_data)
        TESTING("onion creation; truncate extant canonical; w/ initial data");
    else if (true == truncate_canonical)
        TESTING("onion creation; truncate extant canonical; no initial data");
    else if (true == with_initial_data)
        TESTING("onion creation; no extant canonical; w/ initial data");
    else
        TESTING("onion creation; no extant canonical; no initial data");

    /*********
     * SETUP *
     *********/

    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;

    if (NULL == (paths = onion_filepaths_init(basename)))
        TEST_ERROR;

    HDremove(paths->canon);
    HDremove(paths->onion);
    HDremove(paths->recovery);

    /* Create canonical file to be truncated */
    if (true == truncate_canonical) {
        /* Create canonical file. */
        vfile_raw = H5FDopen(paths->canon, flags_create_s, onion_info.backing_fapl_id, HADDR_UNDEF);
        if (NULL == vfile_raw)
            TEST_ERROR;
        if (H5FDset_eoa(vfile_raw, H5FD_MEM_DRAW, b_list_size_s) < 0)
            TEST_ERROR;
        if (H5FDwrite(vfile_raw, H5FD_MEM_DRAW, H5P_DEFAULT, 0, b_list_size_s, b_list_s) < 0)
            TEST_ERROR;
        if (H5FDclose(vfile_raw) < 0)
            TEST_ERROR;

        vfile_raw = NULL;
        H5E_BEGIN_TRY
        {
            vfile_raw = H5FDopen(paths->canon, H5F_ACC_RDONLY, fapl_id, HADDR_UNDEF);
        }
        H5E_END_TRY

        /* Check if onion history for onion-open created file */
        if (NULL != vfile_raw)
            TEST_ERROR;

        /* Create "existing onion file". */
        vfile_raw = H5FDopen(paths->onion, flags_create_s, onion_info.backing_fapl_id, HADDR_UNDEF);
        if (NULL == vfile_raw)
            TEST_ERROR;
        if (H5FDset_eoa(vfile_raw, H5FD_MEM_DRAW, b_list_size_s) < 0)
            TEST_ERROR;
        if (H5FDwrite(vfile_raw, H5FD_MEM_DRAW, H5P_DEFAULT, 0, 23, "prior history stand-in") < 0)
            TEST_ERROR;
        if (H5FDclose(vfile_raw) < 0)
            TEST_ERROR;
        vfile_raw = NULL;
    } /* end if to create canonical file for truncation */

    /*
     * OPENED
     */

    /* Begin creation of onionized file from nothing */

    vfile_rw = H5FDopen(paths->canon, flags_create_s, fapl_id, HADDR_UNDEF);
    if (NULL == vfile_rw)
        TEST_ERROR;

    if (verify_stored_onion_create_0_open(paths, &onion_info) < 0)
        TEST_ERROR;

    H5E_BEGIN_TRY
    {
        vfile_ro = H5FDopen(paths->canon, H5F_ACC_RDONLY, fapl_id, HADDR_UNDEF);
    }
    H5E_END_TRY
    /* check if onionization (creation) not complete; nothing to open */
    if (vfile_ro != NULL)
        TEST_ERROR;

    /*
     * WRITING
     */

    if (true == with_initial_data) {
        haddr_t half_size = 0;
        haddr_t buf_size  = 0;

        /* Write the sub-page entry at addr 0 */
        if (4 >= onion_info.page_size)
            TEST_ERROR;
        if (H5FDset_eoa(vfile_rw, H5FD_MEM_DRAW, 4) < 0)
            TEST_ERROR;
        if (H5FDwrite(vfile_rw, H5FD_MEM_DRAW, H5P_DEFAULT, 0, 4, a_list_s) < 0) {
            TEST_ERROR;
        }

        /* Verify logical file contents. */
        if (NULL == (buf = malloc(4 * sizeof(char))))
            TEST_ERROR;
        if (H5FDread(vfile_rw, H5FD_MEM_DRAW, H5P_DEFAULT, 0, 4, buf) < 0)
            TEST_ERROR;
        if (memcmp(a_list_s, buf, 4) != 0)
            TEST_ERROR;
        free(buf);
        buf = NULL;

        /* Write the latter half of buffer at addr 0 (more than one page) */
        half_size = a_list_size_s / 2;
        buf_size  = a_list_size_s - half_size;
        if (buf_size <= onion_info.page_size)
            TEST_ERROR;
        if (H5FDset_eoa(vfile_rw, H5FD_MEM_DRAW, buf_size) < 0)
            TEST_ERROR;
        if (H5FDwrite(vfile_rw, H5FD_MEM_DRAW, H5P_DEFAULT, 0, buf_size, a_list_s + half_size) < 0)
            TEST_ERROR;

        /* Verify logical file contents. */
        if (NULL == (buf = malloc(buf_size * sizeof(char))))
            TEST_ERROR;
        if (H5FDread(vfile_rw, H5FD_MEM_DRAW, H5P_DEFAULT, 0, buf_size, buf) < 0)
            TEST_ERROR;
        if (memcmp(a_list_s + half_size, buf, buf_size) != 0)
            TEST_ERROR;
        free(buf);
        buf = NULL;

        /* Overwrite existing data with entire buffer at addr 0 */
        if (H5FDset_eoa(vfile_rw, H5FD_MEM_DRAW, a_list_size_s) < 0)
            TEST_ERROR;
        if (H5FDwrite(vfile_rw, H5FD_MEM_DRAW, H5P_DEFAULT, 0, a_list_size_s, a_list_s) < 0)
            TEST_ERROR;

        /* Verify logical file contents. */
        if (NULL == (buf = malloc(a_list_size_s * sizeof(char))))
            TEST_ERROR;
        if (H5FDread(vfile_rw, H5FD_MEM_DRAW, H5P_DEFAULT, 0, a_list_size_s, buf) < 0)
            TEST_ERROR;
        if (memcmp(a_list_s, buf, a_list_size_s) != 0)
            TEST_ERROR;
        free(buf);
        buf = NULL;
    } /* end if writing data to logical file */

    /*
     * CLOSED
     */

    if (H5FDclose(vfile_rw) < 0)
        TEST_ERROR;
    vfile_rw = NULL;

    /* Look at h5 file: should be known-empty */
    if (compare_file_bytes_exactly(paths->canon, onion_info.backing_fapl_id, 8,
                                   (const unsigned char *)"ONIONEOF") < 0)
        TEST_ERROR;

    /* Look at recovery file: should be gone */
    H5E_BEGIN_TRY
    {
        vfile_raw = H5FDopen(paths->recovery, H5F_ACC_RDONLY, onion_info.backing_fapl_id, HADDR_UNDEF);
    }
    H5E_END_TRY
    if (NULL != vfile_raw)
        TEST_ERROR;

    /* Inspect onion file */
    vfile_raw = H5FDopen(paths->onion, H5F_ACC_RDONLY, onion_info.backing_fapl_id, HADDR_UNDEF);
    if (NULL == vfile_raw)
        TEST_ERROR;

    filter.page_size                        = onion_info.page_size;
    filter.n_revisions                      = 1;
    filter.origin_eof                       = 0;
    filter.revisions[0].comment             = onion_info.comment;
    filter.revisions[0].n_index_entries     = (uint64_t)(-1); /* don't care */
    filter.revisions[0].revision_num        = 0;
    filter.revisions[0].parent_revision_num = 0;
    filter.revisions[0].logical_eof         = (true == with_initial_data) ? a_list_size_s : 0;

    if (verify_history_as_expected_onion(vfile_raw, &filter) < 0)
        TEST_ERROR;

    if (H5FDclose(vfile_raw) < 0)
        TEST_ERROR;
    vfile_raw = NULL;

    /* R/O open the file with Onion VFD; inspect logical file */

    vfile_ro = H5FDopen(paths->canon, H5F_ACC_RDONLY, fapl_id, HADDR_UNDEF);
    if (NULL == vfile_ro)
        TEST_ERROR;

    if (true == with_initial_data) {
        /* Initial revision contains data */
        if (H5FDget_eof(vfile_ro, H5FD_MEM_DRAW) != a_list_size_s)
            TEST_ERROR;
        if (H5FDget_eoa(vfile_ro, H5FD_MEM_DRAW) != 0)
            TEST_ERROR;
        if (H5FDset_eoa(vfile_ro, H5FD_MEM_DRAW, a_list_size_s) < 0)
            TEST_ERROR;
        if (NULL == (buf = malloc(a_list_size_s * 64 * sizeof(char))))
            TEST_ERROR;
        if (H5FDread(vfile_ro, H5FD_MEM_DRAW, H5P_DEFAULT, 0, a_list_size_s, buf) < 0)
            TEST_ERROR;
        if (memcmp(a_list_s, buf, a_list_size_s) != 0)
            TEST_ERROR;
        free(buf);
        buf = NULL;
    }
    else {
        /* Initial revision has no data */
        if (H5FDget_eoa(vfile_ro, H5FD_MEM_DRAW) != 0)
            TEST_ERROR;
        if (H5FDget_eof(vfile_ro, H5FD_MEM_DRAW) != 0)
            TEST_ERROR;
    }

    if (H5FDclose(vfile_ro) < 0)
        TEST_ERROR;
    vfile_ro = NULL;

    /*
     * CLEANUP
     */

    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    HDremove(paths->canon);
    HDremove(paths->onion);
    HDremove(paths->recovery);
    onion_filepaths_destroy(paths);

    PASSED();
    return 0;

error:

    if (paths != NULL) {
        HDremove(paths->canon);
        HDremove(paths->onion);
        HDremove(paths->recovery);
        onion_filepaths_destroy(paths);
    }

    free(buf);

    if (vfile_raw != NULL)
        (void)H5FDclose(vfile_raw);
    if (vfile_rw != NULL)
        (void)H5FDclose(vfile_rw);
    if (vfile_ro != NULL)
        (void)H5FDclose(vfile_ro);

    H5E_BEGIN_TRY
    {
        H5Pclose(fapl_id);
    }
    H5E_END_TRY

    return -1;
} /* end test_create_oniontarget() */

/*-----------------------------------------------------------------------------
 *
 * Function:    test_several_revisions_with_logical_gaps()
 *
 * Purpose:     Test the ability of the Onion VFD to create a valid
 *              'onionized' file.
 *
 *              When `truncate_canonical` is false, the canonical file is
 *              nonexistent on the backing store on onion-creation.
 *              When `truncate_canonical` is true, a canonical file is created
 *              on the backing store with known contents, which are to be
 *              truncated on onion-creation.
 *
 * Return:      PASSED : 0
 *              FAILED : -1
 *
 *-----------------------------------------------------------------------------
 */
static int
test_several_revisions_with_logical_gaps(void)
{
    const char             *basename   = "somesuch";
    hid_t                   fapl_id    = H5I_INVALID_HID;
    struct onion_filepaths *paths      = NULL;
    H5FD_onion_fapl_info_t  onion_info = {
        H5FD_ONION_FAPL_INFO_VERSION_CURR,
        H5I_INVALID_HID,               /* backing_fapl_id  */
        ONION_TEST_PAGE_SIZE_5,        /* page_size        */
        H5FD_ONION_STORE_TARGET_ONION, /* store_target     */
        H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST,
        0,      /* force_write_open */
        0,      /* flags */
        "first" /* comment          */
    };
    H5FD_t                 *file = NULL; /* Onion virtual file for read/write */
    struct expected_history filter;
    unsigned char          *buf = NULL;
    struct revise_revision  about[4];
    H5FD_onion_history_t    history_out;
    size_t                  i     = 0;
    haddr_t                 size  = 0;
    uint64_t                a_off = ONION_TEST_PAGE_SIZE_5 + 7; /* 39 */
    uint64_t                b_off = (((a_off + a_list_size_s + ONION_TEST_PAGE_SIZE_5 - 1) >> 5) << 5) +
                     ONION_TEST_PAGE_SIZE_5 + 7; /* full page between */

    TESTING("multiple revisions with gaps and overlap");

    /*********
     * SETUP *
     *********/

    history_out.version     = H5FD_ONION_HISTORY_VERSION_CURR;
    history_out.n_revisions = 0;
    history_out.record_locs = NULL;

    if (NULL == (paths = onion_filepaths_init(basename)))
        TEST_ERROR;

    if ((onion_info.backing_fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    HDremove(paths->canon);
    HDremove(paths->onion);
    HDremove(paths->recovery);

    /* Empty first revision */
    about[0].truncate     = true;
    about[0].revision_num = H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST;
    about[0].comment      = "first";
    about[0].n_writes     = 0;

    about[1].truncate         = false;
    about[1].revision_num     = H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST;
    about[1].comment          = "second";
    about[1].n_writes         = 1;
    about[1].writes[0].offset = a_off;
    about[1].writes[0].size   = a_list_size_s;
    about[1].writes[0].buf    = a_list_s;

    about[2].truncate         = false;
    about[2].revision_num     = H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST;
    about[2].comment          = "third";
    about[2].n_writes         = 1; /* TODO: several writes */
    about[2].writes[0].offset = b_off;
    about[2].writes[0].size   = b_list_size_s;
    about[2].writes[0].buf    = b_list_s;

    about[3].truncate         = false;
    about[3].revision_num     = H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST;
    about[3].comment          = "fourth";
    about[3].n_writes         = 1;
    about[3].writes[0].offset = 0;
    about[3].writes[0].size   = a_list_size_s;
    about[3].writes[0].buf    = a_list_s;

    if (do_onion_open_and_writes(paths->canon, &onion_info, 4, about) < 0)
        TEST_ERROR;

    /* Inspect logical file */

    /* THIS IS THE INITIAL FILE, SHOULD ONLY HAVE 8 BYTES */
    onion_info.revision_num = 0;
    fapl_id                 = H5Pcreate(H5P_FILE_ACCESS);
    if (H5I_INVALID_HID == fapl_id)
        TEST_ERROR;
    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;
    file = H5FDopen(paths->canon, H5F_ACC_RDONLY, fapl_id, HADDR_UNDEF);
    if (NULL == file)
        TEST_ERROR;
    if (8 != H5FDget_eof(file, H5FD_MEM_DRAW)) {
        printf("\nEOF is not zero, it is: %" PRIuHADDR "\n", H5FDget_eof(file, H5FD_MEM_DRAW));
        TEST_ERROR;
    }
    if (H5FDclose(file) < 0)
        TEST_ERROR;
    file = NULL;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    fapl_id = H5I_INVALID_HID;

    /* Empty first revision */
    onion_info.revision_num = 1;
    fapl_id                 = H5Pcreate(H5P_FILE_ACCESS);
    if (H5I_INVALID_HID == fapl_id)
        TEST_ERROR;
    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;
    file = H5FDopen(paths->canon, H5F_ACC_RDONLY, fapl_id, HADDR_UNDEF);
    if (NULL == file)
        TEST_ERROR;
    if (0 != H5FDget_eof(file, H5FD_MEM_DRAW)) {
        printf("\nEOF is not zero, it is: %" PRIuHADDR "\n", H5FDget_eof(file, H5FD_MEM_DRAW));
        TEST_ERROR;
    }
    if (H5FDclose(file) < 0)
        TEST_ERROR;
    file = NULL;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    fapl_id = H5I_INVALID_HID;

    /* One offset block in second revision */
    onion_info.revision_num = 2;
    fapl_id                 = H5Pcreate(H5P_FILE_ACCESS);
    if (H5I_INVALID_HID == fapl_id)
        TEST_ERROR;
    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;
    file = H5FDopen(paths->canon, H5F_ACC_RDONLY, fapl_id, HADDR_UNDEF);
    if (NULL == file)
        TEST_ERROR;
    size = a_off + a_list_size_s;
    if (size != H5FDget_eof(file, H5FD_MEM_DRAW)) {
        printf("\nEOF is not %" PRIuHADDR ", it is: %" PRIuHADDR "\n", size,
               H5FDget_eof(file, H5FD_MEM_DRAW));
        TEST_ERROR;
    }
    if (NULL == (buf = malloc(size * sizeof(unsigned char))))
        TEST_ERROR;
    if (H5FDset_eoa(file, H5FD_MEM_DRAW, size) < 0)
        TEST_ERROR;
    if (H5FDread(file, H5FD_MEM_DRAW, H5P_DEFAULT, 0, size, buf) < 0)
        TEST_ERROR;
    for (i = 0; i < a_off; i++) {
        if (0 != buf[i])
            TEST_ERROR;
    }
    if (memcmp(buf + a_off, a_list_s, a_list_size_s) != 0)
        TEST_ERROR;
    free(buf);
    buf = NULL;
    /* Repeat read at page offset; test possible read offset error */
    if (NULL == (buf = malloc(ONION_TEST_PAGE_SIZE_5 * sizeof(unsigned char))))
        TEST_ERROR;
    if (H5FDread(file, H5FD_MEM_DRAW, H5P_DEFAULT, ONION_TEST_PAGE_SIZE_5, ONION_TEST_PAGE_SIZE_5, buf) < 0)
        TEST_ERROR;
    size = a_off - ONION_TEST_PAGE_SIZE_5;
    for (i = 0; i < size; i++) {
        if (0 != buf[i])
            TEST_ERROR;
    }
    if (memcmp(buf + size, a_list_s, ONION_TEST_PAGE_SIZE_5 - size) != 0)
        TEST_ERROR;
    free(buf);
    buf = NULL;
    if (H5FDclose(file) < 0)
        TEST_ERROR;
    file = NULL;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    fapl_id = H5I_INVALID_HID;

    /* Two offset blocks in third revision */
    onion_info.revision_num = 3;
    fapl_id                 = H5Pcreate(H5P_FILE_ACCESS);
    if (H5I_INVALID_HID == fapl_id)
        TEST_ERROR;
    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;
    file = H5FDopen(paths->canon, H5F_ACC_RDONLY, fapl_id, HADDR_UNDEF);
    if (NULL == file)
        TEST_ERROR;
    size = b_off + b_list_size_s;
    if (size != H5FDget_eof(file, H5FD_MEM_DRAW))
        TEST_ERROR;
    if (NULL == (buf = malloc(size * sizeof(unsigned char))))
        TEST_ERROR;
    if (H5FDset_eoa(file, H5FD_MEM_DRAW, size) < 0)
        TEST_ERROR;
    if (H5FDread(file, H5FD_MEM_DRAW, H5P_DEFAULT, 0, size, buf) < 0)
        TEST_ERROR;
    for (i = 0; i < a_off; i++) {
        if (0 != buf[i])
            TEST_ERROR;
    }
    if (memcmp(buf + a_off, a_list_s, a_list_size_s) != 0)
        TEST_ERROR;
    for (i = a_off + a_list_size_s; i < b_off; i++) {
        if (0 != buf[i])
            TEST_ERROR;
    }
    if (memcmp(buf + b_off, b_list_s, b_list_size_s) != 0)
        TEST_ERROR;
    free(buf);
    buf = NULL;
    if (H5FDclose(file) < 0)
        TEST_ERROR;
    file = NULL;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    fapl_id = H5I_INVALID_HID;

    /* From start and partial overwrite in fourth revision */
    onion_info.revision_num = 4;
    fapl_id                 = H5Pcreate(H5P_FILE_ACCESS);
    if (H5I_INVALID_HID == fapl_id)
        TEST_ERROR;
    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;
    file = H5FDopen(paths->canon, H5F_ACC_RDONLY, fapl_id, HADDR_UNDEF);
    if (NULL == file)
        TEST_ERROR;
    size = b_off + b_list_size_s;
    if (size != H5FDget_eof(file, H5FD_MEM_DRAW))
        TEST_ERROR;
    buf = (unsigned char *)malloc(sizeof(unsigned char) * size);
    if (NULL == buf)
        TEST_ERROR;
    if (H5FDset_eoa(file, H5FD_MEM_DRAW, size) < 0)
        TEST_ERROR;
    if (H5FDread(file, H5FD_MEM_DRAW, H5P_DEFAULT, 0, size, buf) < 0)
        TEST_ERROR;
    if (memcmp(buf, a_list_s, a_list_size_s) != 0)
        TEST_ERROR;
    if (memcmp(buf + a_list_size_s, a_list_s + a_list_size_s - a_off, a_off) != 0)
        TEST_ERROR;
    for (i = a_off + a_list_size_s; i < b_off; i++) {
        if (0 != buf[i])
            TEST_ERROR;
    }
    if (memcmp(buf + b_off, b_list_s, b_list_size_s) != 0)
        TEST_ERROR;
    free(buf);
    buf = NULL;
    if (H5FDclose(file) < 0)
        TEST_ERROR;
    file = NULL;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    fapl_id = H5I_INVALID_HID;

    /* No fifth revision */

    /* Inspect history construction */

    file = H5FDopen(paths->onion, H5F_ACC_RDONLY, onion_info.backing_fapl_id, HADDR_UNDEF);
    if (NULL == file)
        TEST_ERROR;

    filter.page_size   = onion_info.page_size;
    filter.n_revisions = 4;
    filter.origin_eof  = 0;

    filter.revisions[0].comment             = "first";
    filter.revisions[0].n_index_entries     = 0;
    filter.revisions[0].revision_num        = 0;
    filter.revisions[0].parent_revision_num = 0;
    filter.revisions[0].logical_eof         = 0;

    filter.revisions[1].comment             = "second";
    filter.revisions[1].n_index_entries     = (a_list_size_s + ONION_TEST_PAGE_SIZE_5 - 1) >> 5;
    filter.revisions[1].revision_num        = 1;
    filter.revisions[1].parent_revision_num = 0;
    filter.revisions[1].logical_eof         = a_off + a_list_size_s;

    filter.revisions[2].comment = "third";
    filter.revisions[2].n_index_entries =
        filter.revisions[1].n_index_entries + ((b_list_size_s + ONION_TEST_PAGE_SIZE_5 - 1) >> 5);
    filter.revisions[2].revision_num        = 2;
    filter.revisions[2].parent_revision_num = 1;
    filter.revisions[2].logical_eof         = b_off + b_list_size_s;

    filter.revisions[3].comment             = "fourth";
    filter.revisions[3].n_index_entries     = filter.revisions[2].n_index_entries + 1;
    filter.revisions[3].revision_num        = 3;
    filter.revisions[3].parent_revision_num = 2;
    filter.revisions[3].logical_eof         = b_off + b_list_size_s;

    if (verify_history_as_expected_onion(file, &filter) < 0)
        TEST_ERROR;

    if (H5FDclose(file) < 0)
        TEST_ERROR;
    file = NULL;

    /* CLEANUP */

    if (H5Pclose(onion_info.backing_fapl_id) < 0)
        TEST_ERROR;

    HDremove(paths->canon);
    HDremove(paths->onion);
    HDremove(paths->recovery);
    onion_filepaths_destroy(paths);

    PASSED();
    return 0;

error:

    if (paths != NULL) {
        HDremove(paths->canon);
        HDremove(paths->onion);
        HDremove(paths->recovery);
        onion_filepaths_destroy(paths);
    }

    free(history_out.record_locs);
    free(buf);

    if (file != NULL)
        (void)H5FDclose(file);

    H5E_BEGIN_TRY
    {
        H5Pclose(fapl_id);
        H5Pclose(onion_info.backing_fapl_id);
    }
    H5E_END_TRY

    return -1;
} /* end test_several_revisions_with_logical_gaps() */

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 *
 * Function:    do_onion_open_and_writes
 *
 * Purpose:     Automate the process of creating/opening a file and performing
 *              a series of writes.
 *
 * Return:      Success : 0
 *              Failure : -1
 *
 *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */
// TODO: Modify to create initial file without onion
static int
do_onion_open_and_writes(const char *filename, H5FD_onion_fapl_info_t *onion_info_p, size_t n_ops,
                         struct revise_revision *about)
{
    hid_t          fapl_id = H5I_INVALID_HID;
    H5FD_t        *file    = NULL; /* Onion virtual file for read/write */
    unsigned char *buf_vfy = NULL;
    size_t         i       = 0;

    for (i = 0; i < n_ops; i++) {
        size_t       j     = 0;
        unsigned int flags = H5F_ACC_RDWR;

        if (i != 0 && about[i].truncate == true)
            goto error;

        if (true == about[i].truncate)
            flags |= H5F_ACC_CREAT | H5F_ACC_TRUNC;

        onion_info_p->revision_num = about[i].revision_num;
        if (about[i].comment != NULL) {
            j = MIN(strlen(about[i].comment), H5FD_ONION_FAPL_INFO_COMMENT_MAX_LEN);
            memcpy(onion_info_p->comment, about[i].comment, j);
        }
        onion_info_p->comment[j] = '\0';
        fapl_id                  = H5Pcreate(H5P_FILE_ACCESS);
        if (H5I_INVALID_HID == fapl_id)
            goto error;
        if (H5Pset_fapl_onion(fapl_id, onion_info_p) < 0)
            goto error;
        file = H5FDopen(filename, flags, fapl_id, HADDR_UNDEF);
        if (NULL == file)
            goto error;

        for (j = 0; j < about[i].n_writes; j++) {
            struct write_info *wi = &about[i].writes[j];

            /* Write to file */
            if (H5FDget_eoa(file, H5FD_MEM_DRAW) < wi->offset + wi->size &&
                H5FDset_eoa(file, H5FD_MEM_DRAW, wi->offset + wi->size) < 0)
                TEST_ERROR;
            if (H5FDwrite(file, H5FD_MEM_DRAW, H5P_DEFAULT, wi->offset, wi->size, wi->buf) < 0)
                TEST_ERROR;
            /* Verify write as expected */
            if (NULL == (buf_vfy = malloc(wi->size * sizeof(unsigned char))))
                TEST_ERROR;
            if (H5FDread(file, H5FD_MEM_DRAW, H5P_DEFAULT, wi->offset, wi->size, buf_vfy) < 0)
                TEST_ERROR;
            if (memcmp(buf_vfy, wi->buf, wi->size) != 0) {
                const unsigned char *_buf = wi->buf;
                size_t               z    = 0;
                puts("i  exp  act");
                for (z = 0; z < wi->size; z++)
                    printf("%02zx %c %c\n", z, _buf[z], buf_vfy[z]);
                fflush(stdout);
                TEST_ERROR;
            }
            free(buf_vfy);
            buf_vfy = NULL;
        } /* end for each write */

        if (H5FDclose(file) < 0)
            goto error;
        file = NULL;
        if (H5Pclose(fapl_id) < 0)
            goto error;
        fapl_id = H5I_INVALID_HID;
    } /* end for each open-close cycle */

    return 0;

error:
    if (file != NULL)
        (void)H5FDclose(file);

    H5E_BEGIN_TRY
    {
        H5Pclose(fapl_id);
    }
    H5E_END_TRY

    free(buf_vfy);

    return -1;
} /* end do_onion_open_and_writes() */

/*-----------------------------------------------------------------------------
 *
 * Function:    test_page_aligned_history_create()
 *
 * Purpose:     Verify that, when specified in FAPL on onionization/creation,
 *              All history writes are aligned to page-size boundaries.
 *
 * Return:      PASSED : 0
 *              FAILED : -1
 *
 *-----------------------------------------------------------------------------
 */
static int
test_page_aligned_history_create(void)
{
    const char             *basename   = "somesuch";
    hid_t                   fapl_id    = H5I_INVALID_HID;
    struct onion_filepaths *paths      = NULL;
    H5FD_onion_fapl_info_t  onion_info = {
        H5FD_ONION_FAPL_INFO_VERSION_CURR,
        H5I_INVALID_HID,               /* backing_fapl_id  */
        ONION_TEST_PAGE_SIZE_5,        /* page_size        */
        H5FD_ONION_STORE_TARGET_ONION, /* store_target     */
        H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST,
        0, /* force_write_open */
        H5FD_ONION_FAPL_INFO_CREATE_FLAG_ENABLE_PAGE_ALIGNMENT,
        "initial commit" /* comment          */
    };
    H5FD_t                *file = NULL; /* Onion virtual file for read/write */
    unsigned char         *buf  = NULL;
    struct revise_revision about[2];
    H5FD_onion_header_t    hdr_out;
    H5FD_onion_history_t   history_out;
    size_t                 i     = 0;
    uint64_t               a_off = b_list_size_s - a_list_size_s;

    TESTING("page-aligned history on onion-created file");

    /*********
     * SETUP *
     *********/

    hdr_out.version         = H5FD_ONION_HEADER_VERSION_CURR;
    history_out.version     = H5FD_ONION_HISTORY_VERSION_CURR;
    history_out.n_revisions = 0;
    history_out.record_locs = NULL;

    if ((onion_info.backing_fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;

    if (NULL == (paths = onion_filepaths_init(basename)))
        TEST_ERROR;

    HDremove(paths->canon);
    HDremove(paths->onion);
    HDremove(paths->recovery);

    about[0].truncate         = true;
    about[0].revision_num     = H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST;
    about[0].comment          = "initial_commit";
    about[0].n_writes         = 1;
    about[0].writes[0].offset = 0;
    about[0].writes[0].size   = b_list_size_s;
    about[0].writes[0].buf    = b_list_s;

    about[1].truncate         = false;
    about[1].revision_num     = H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST;
    about[1].comment          = "second";
    about[1].n_writes         = 1;
    about[1].writes[0].offset = a_off;
    about[1].writes[0].size   = a_list_size_s;
    about[1].writes[0].buf    = a_list_s;

    if (do_onion_open_and_writes(paths->canon, &onion_info, 2, about) < 0)
        TEST_ERROR;

    /* Inspect logical file */
    if (NULL == (buf = malloc(b_list_size_s * sizeof(unsigned char))))
        TEST_ERROR;
    file = H5FDopen(paths->canon, H5F_ACC_RDONLY, fapl_id, HADDR_UNDEF);
    if (NULL == file)
        TEST_ERROR;
    if (b_list_size_s != H5FDget_eof(file, H5FD_MEM_DRAW))
        TEST_ERROR;
    if (H5FDset_eoa(file, H5FD_MEM_DRAW, b_list_size_s) < 0)
        TEST_ERROR;
    if (H5FDread(file, H5FD_MEM_DRAW, H5P_DEFAULT, 0, b_list_size_s, buf) < 0)
        TEST_ERROR;
    if (memcmp(a_list_s, buf + a_off, a_list_size_s) != 0) {
        size_t k;
        printf("aoff: %" PRIu64 "\n", a_off);
        puts("i exp act");
        for (k = 0; k < b_list_size_s; k++) {
            printf("%3zu:: %c : %c\n", k, (k < a_off) ? ' ' : a_list_s[k - a_off], buf[k]);
        }
        fflush(stdout);
        TEST_ERROR;
    }
    if (memcmp(b_list_s, buf, a_off) != 0) {
        size_t k;
        printf("aoff: %" PRIu64 "\n", a_off);
        puts("i exp act");
        for (k = 0; k < b_list_size_s; k++) {
            printf("%3zu:: %c : %c\n", k, (k < a_off) ? b_list_s[k] : ' ', buf[k]);
        }
        fflush(stdout);
        TEST_ERROR;
    }
    if (H5FDclose(file) < 0)
        TEST_ERROR;
    file = NULL;
    free(buf);
    buf = NULL;

    /* Inspect history construction */

    if (NULL == (file = H5FDopen(paths->onion, H5F_ACC_RDONLY, onion_info.backing_fapl_id, HADDR_UNDEF)))
        TEST_ERROR;
    if (H5FDset_eoa(file, H5FD_MEM_DRAW, H5FDget_eof(file, H5FD_MEM_DRAW)) < 0)
        TEST_ERROR;

    if (NULL == (buf = malloc(H5FD_ONION_ENCODED_SIZE_HEADER)))
        TEST_ERROR;
    if (H5FDread(file, H5FD_MEM_DRAW, H5P_DEFAULT, 0, H5FD_ONION_ENCODED_SIZE_HEADER, buf) < 0)
        TEST_ERROR;
    if (H5FD__onion_header_decode(buf, &hdr_out) != H5FD_ONION_ENCODED_SIZE_HEADER)
        TEST_ERROR;
    if (hdr_out.history_addr & ((1 << 5) - 1)) /* 5::PAGE_SIZE_5 */
        TEST_ERROR;
    free(buf);
    buf = NULL;

    if (NULL == (buf = malloc(hdr_out.history_size)))
        TEST_ERROR;
    if (H5FDread(file, H5FD_MEM_DRAW, H5P_DEFAULT, hdr_out.history_addr, hdr_out.history_size, buf) < 0)
        TEST_ERROR;
    if (H5FD__onion_history_decode(buf, &history_out) != hdr_out.history_size)
        TEST_ERROR;
    if (history_out.n_revisions != 2)
        TEST_ERROR;
    history_out.record_locs = calloc(history_out.n_revisions, sizeof(H5FD_onion_record_loc_t));
    if (NULL == history_out.record_locs)
        TEST_ERROR;
    if (H5FD__onion_history_decode(buf, &history_out) != hdr_out.history_size)
        TEST_ERROR;
    free(buf);
    buf = NULL;

    for (i = 0; i < history_out.n_revisions; i++) {
        H5FD_onion_record_loc_t *rloc = &history_out.record_locs[i];
        if (rloc->phys_addr & ((1 << 5) - 1)) /* 5::PAGE_SIZE_5 */
            TEST_ERROR;
        /* TODO: check phys_addr of each page entry? */
    }

    free(history_out.record_locs);
    history_out.record_locs = NULL;

    if (H5FDclose(file) < 0)
        TEST_ERROR;
    file = NULL;

    /* CLEANUP */

    if (H5Pclose(onion_info.backing_fapl_id) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    HDremove(paths->canon);
    HDremove(paths->onion);
    HDremove(paths->recovery);
    onion_filepaths_destroy(paths);

    free(buf);

    PASSED();
    return 0;

error:

    if (paths != NULL) {
        HDremove(paths->canon);
        HDremove(paths->onion);
        HDremove(paths->recovery);
        onion_filepaths_destroy(paths);
    }

    free(history_out.record_locs);
    free(buf);

    if (file != NULL)
        (void)H5FDclose(file);

    H5E_BEGIN_TRY
    {
        H5Pclose(fapl_id);
        H5Pclose(onion_info.backing_fapl_id);
    }
    H5E_END_TRY

    return -1;
} /* end test_page_aligned_history_create() */

/*-----------------------------------------------------------------------------
 * Function:    test_integration_create()
 *
 * Purpose:     Create and make multiple revisions in an HDF5 file.
 *
 * Return:      PASSED : 0
 *              FAILED : -1
 *-----------------------------------------------------------------------------
 */
static int
test_integration_create(void)
{
    const char             *basename   = "integration_2d.h5";
    hid_t                   fapl_id    = H5I_INVALID_HID;
    hid_t                   file_id    = H5I_INVALID_HID;
    hid_t                   file       = H5I_INVALID_HID;
    hid_t                   space      = H5I_INVALID_HID;
    hid_t                   dset       = H5I_INVALID_HID;
    hid_t                   dcpl       = H5I_INVALID_HID;
    struct onion_filepaths *paths      = NULL;
    H5FD_onion_fapl_info_t  onion_info = {
        H5FD_ONION_FAPL_INFO_VERSION_CURR,
        H5I_INVALID_HID,               /* backing_fapl_id  */
        ONION_TEST_PAGE_SIZE_5,        /* page_size        */
        H5FD_ONION_STORE_TARGET_ONION, /* store_target     */
        H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST,
        0,               /* force_write_open */
        0,               /* creation flags, was H5FD_ONION_FAPL_INFO_CREATE_FLAG_ENABLE_PAGE_ALIGNMENT */
        "initial commit" /* comment          */
    };
    hsize_t dims[2]    = {128, 256};
    hsize_t maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED};
    hsize_t chunk[2]   = {4, 4};
    int     fillval;
    struct {
        int arr[128][256];
    } *wdata = NULL;
    struct {
        int arr[128][256];
    } *rdata = NULL;
    struct {
        int arr[128][256];
    } *dset_data = NULL;

    TESTING("onion-created two dimensional HDF5 file with revisions");

    /* SETUP */

    if (NULL == (wdata = calloc(1, sizeof(*wdata))))
        TEST_ERROR;
    if (NULL == (rdata = calloc(1, sizeof(*rdata))))
        TEST_ERROR;
    if (NULL == (dset_data = calloc(1, sizeof(*dset_data))))
        TEST_ERROR;

    if ((onion_info.backing_fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;

    if (NULL == (paths = onion_filepaths_init(basename)))
        TEST_ERROR;

    HDremove(paths->canon);
    HDremove(paths->onion);
    HDremove(paths->recovery);

    /*----------------------------------------------------------------------
     * Create the skeleton file (create the file without Onion VFD)
     *----------------------------------------------------------------------
     */

    /* Initialize data */
    for (int i = 0; i < 128; i++)
        for (int j = 0; j < 256; j++)
            wdata->arr[i][j] = i * j - j;

    /* Create a new file using the default properties */
    if ((file = H5Fcreate(paths->canon, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create dataspace with unlimited dimensions */
    if ((space = H5Screate_simple(2, dims, maxdims)) < 0)
        TEST_ERROR;

    /* Create the dataset creation property list, and set the chunk
     * size
     */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_chunk(dcpl, 2, chunk) < 0)
        TEST_ERROR;

    /* Set the fill value for the dataset */
    fillval = 99;
    if (H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval) < 0)
        TEST_ERROR;

    /* Set the allocation time to "early".  This way we can be sure
     * that reading from the dataset immediately after creation will
     * return the fill value.
     */
    if (H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0)
        TEST_ERROR;

    /* Create the dataset using the dataset creation property list */
    if ((dset = H5Dcreate2(file, "DS1", H5T_STD_I32LE, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Write the data to the dataset */
    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata) < 0)
        TEST_ERROR;

    /* Close and release resources */
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    if (H5Sclose(space) < 0)
        TEST_ERROR;
    if (H5Fclose(file) < 0)
        TEST_ERROR;

    /*----------------------------------------------------------------------
     * First revision: open the file with Onion VFD and change the data
     *----------------------------------------------------------------------
     */
    if ((file_id = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    if ((dset = H5Dopen2(file_id, "DS1", H5P_DEFAULT)) < 0)
        TEST_ERROR;

    for (int i = 0; i < 128; i++)
        for (int j = 0; j < 256; j++)
            dset_data->arr[i][j] = i * 6 + j + 1;

    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset_data) < 0)
        TEST_ERROR;

    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;
    file_id = H5I_INVALID_HID;

    /*----------------------------------------------------------------------
     * Second revision: open the file with Onion VFD and change the data
     *----------------------------------------------------------------------
     */
    if ((file_id = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    if ((dset = H5Dopen2(file_id, "DS1", H5P_DEFAULT)) < 0)
        TEST_ERROR;

    for (int i = 0; i < 128; i++)
        for (int j = 0; j < 256; j++)
            dset_data->arr[i][j] = i * 3 + j + 5;

    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset_data) < 0)
        TEST_ERROR;

    /* CLEANUP */
    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;
    file_id = H5I_INVALID_HID;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    fapl_id = H5I_INVALID_HID;

    /*----------------------------------------------------------------------
     *  Start to verify the revision
     *----------------------------------------------------------------------
     */
    /*----------------------------------------------------------------------
     * Verify the original file
     *----------------------------------------------------------------------
     */
    onion_info.revision_num = 0;

    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;

    if ((file_id = H5Fopen(paths->canon, H5F_ACC_RDONLY, fapl_id)) < 0)
        TEST_ERROR;

    if ((dset = H5Dopen2(file_id, "DS1", H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if (H5Dread(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata) < 0)
        TEST_ERROR;

    for (int i = 0; i < 128; i++) {
        for (int j = 0; j < 256; j++) {
            int expected = i * j - j;
            if (rdata->arr[i][j] != expected) {
                printf("ERROR!!! Expected: %d, Got: %d\n", expected, rdata->arr[i][j]);
                fflush(stdout);
                TEST_ERROR;
            }
        }
    }

    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;
    file_id = H5I_INVALID_HID;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    fapl_id = H5I_INVALID_HID;

    /*----------------------------------------------------------------------
     * Verify the first revision
     *----------------------------------------------------------------------
     */
    onion_info.revision_num = 1;
    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;

    if ((file_id = H5Fopen(paths->canon, H5F_ACC_RDONLY, fapl_id)) < 0)
        TEST_ERROR;
    if ((dset = H5Dopen2(file_id, "DS1", H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dread(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata) < 0)
        TEST_ERROR;

    for (int i = 0; i < 128; i++) {
        for (int j = 0; j < 256; j++) {
            int expected = i * 6 + j + 1;
            if (rdata->arr[i][j] != expected) {
                printf("ERROR!!! Expected: %d, Got: %d\n", expected, rdata->arr[i][j]);
                fflush(stdout);
                TEST_ERROR;
            }
        }
    }

    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;
    file_id = H5I_INVALID_HID;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    fapl_id = H5I_INVALID_HID;

    /*----------------------------------------------------------------------
     * Verify the second revision
     *----------------------------------------------------------------------
     */
    onion_info.revision_num = 2;

    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;

    if ((file_id = H5Fopen(paths->canon, H5F_ACC_RDONLY, fapl_id)) < 0)
        TEST_ERROR;
    if ((dset = H5Dopen2(file_id, "DS1", H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dread(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata) < 0)
        TEST_ERROR;

    for (int i = 0; i < 128; i++) {
        for (int j = 0; j < 256; j++) {
            int expected = i * 3 + j + 5;
            if (rdata->arr[i][j] != expected) {
                printf("ERROR!!! Expected: %d, Got: %d\n", expected, rdata->arr[i][j]);
                fflush(stdout);
                TEST_ERROR;
            }
            else {
                fflush(stdout);
            }
        }
    }

    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    if (H5Pclose(onion_info.backing_fapl_id) < 0)
        TEST_ERROR;

    HDremove(paths->canon);
    HDremove(paths->onion);
    HDremove(paths->recovery);
    onion_filepaths_destroy(paths);

    free(wdata);
    free(rdata);
    free(dset_data);

    PASSED();
    return 0;

error:

    if (paths != NULL) {
        HDremove(paths->canon);
        HDremove(paths->onion);
        HDremove(paths->recovery);
        onion_filepaths_destroy(paths);
    }

    H5E_BEGIN_TRY
    {
        H5Dclose(dset);
        H5Fclose(file_id);
        H5Pclose(fapl_id);
        H5Pclose(onion_info.backing_fapl_id);
    }
    H5E_END_TRY

    free(wdata);
    free(rdata);
    free(dset_data);

    return -1;
} /* end test_integration_create() */

static int
test_integration_create_simple(void)
{
    const char             *basename   = "integration_1d.h5";
    hid_t                   fapl_id    = H5I_INVALID_HID;
    struct onion_filepaths *paths      = NULL;
    H5FD_onion_fapl_info_t  onion_info = {
        H5FD_ONION_FAPL_INFO_VERSION_CURR,
        H5I_INVALID_HID,               /* backing_fapl_id  */
        ONION_TEST_PAGE_SIZE_5,        /* page_size        */
        H5FD_ONION_STORE_TARGET_ONION, /* store_target     */
        H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST,
        0,               /* force_write_open */
        0,               /* creation flags, was H5FD_ONION_FAPL_INFO_CREATE_FLAG_ENABLE_PAGE_ALIGNMENT */
        "initial commit" /* comment          */
    };
    hid_t   file_id    = H5I_INVALID_HID;
    hid_t   file       = H5I_INVALID_HID;
    hid_t   space      = H5I_INVALID_HID;
    hid_t   dset       = H5I_INVALID_HID;
    hid_t   dcpl       = H5I_INVALID_HID;
    hsize_t dims[2]    = {1, ONE_DIM_SIZE};
    hsize_t maxdims[2] = {1, ONE_DIM_SIZE};
    int     fillval;
    struct {
        int arr[ONE_DIM_SIZE];
    } *wdata = NULL;
    struct {
        int arr[ONE_DIM_SIZE];
    } *rdata = NULL;
    struct {
        int arr[ONE_DIM_SIZE];
    } *dset_data = NULL;

    TESTING("onion-created one-dimensional HDF5 file with revisions");

    /* Setup */
    if (NULL == (wdata = calloc(1, sizeof(*wdata))))
        TEST_ERROR;
    if (NULL == (rdata = calloc(1, sizeof(*rdata))))
        TEST_ERROR;
    if (NULL == (dset_data = calloc(1, sizeof(*dset_data))))
        TEST_ERROR;

    if ((onion_info.backing_fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;

    if (NULL == (paths = onion_filepaths_init(basename)))
        TEST_ERROR;

    HDremove(paths->canon);
    HDremove(paths->onion);
    HDremove(paths->recovery);

    /*----------------------------------------------------------------------
     * Create the skeleton file (create the file without Onion VFD)
     *----------------------------------------------------------------------
     */
    /* Initialize data */
    for (int i = 0; i < ONE_DIM_SIZE; i++)
        wdata->arr[i] = i;

    /* Create a new file using the default properties */
    if ((file = H5Fcreate(paths->canon, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create dataspace with unlimited dimensions*/
    if ((space = H5Screate_simple(2, dims, maxdims)) < 0)
        TEST_ERROR;

    /* Create the dataset creation property list */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    /* Set the fill value for the dataset */
    fillval = 99;
    if (H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval) < 0)
        TEST_ERROR;

    /* Set the allocation time to "early".  This way we can be sure
     * that reading from the dataset immediately after creation will
     * return the fill value.
     */
    if (H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0)
        TEST_ERROR;

    /* Create the dataset using the dataset creation property list */
    if ((dset = H5Dcreate2(file, "DS1", H5T_STD_I32LE, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Write the data to the dataset */
    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata) < 0)
        TEST_ERROR;

    /* Close everything */
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    if (H5Sclose(space) < 0)
        TEST_ERROR;
    if (H5Fclose(file) < 0)
        TEST_ERROR;

    /*----------------------------------------------------------------------
     * First revision: open the file with Onion VFD and change the data
     *----------------------------------------------------------------------
     */
    if ((file_id = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    if ((dset = H5Dopen2(file_id, "DS1", H5P_DEFAULT)) < 0)
        TEST_ERROR;

    for (int i = 0; i < ONE_DIM_SIZE; i++)
        dset_data->arr[i] = i + ONE_DIM_SIZE;

    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset_data) < 0)
        TEST_ERROR;

    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;
    file_id = H5I_INVALID_HID;

    /*----------------------------------------------------------------------
     * Second revision: open the file with Onion VFD and change the data
     *----------------------------------------------------------------------
     */
    if ((file_id = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;
    if ((dset = H5Dopen2(file_id, "DS1", H5P_DEFAULT)) < 0)
        TEST_ERROR;

    for (int i = 0; i < ONE_DIM_SIZE; i++)
        dset_data->arr[i] = i + 2048;

    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset_data) < 0)
        TEST_ERROR;

    /* CLEANUP */
    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;
    file_id = H5I_INVALID_HID;

    /*----------------------------------------------------------------------
     * Third revision: open the file with Onion VFD and change the data
     *----------------------------------------------------------------------
     */
    if ((file_id = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;
    if ((dset = H5Dopen2(file_id, "DS1", H5P_DEFAULT)) < 0)
        TEST_ERROR;

    for (int i = 0; i < ONE_DIM_SIZE; i += 20)
        dset_data->arr[i] = i + 3072;

    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset_data) < 0)
        TEST_ERROR;

    /* CLEANUP */
    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;
    file_id = H5I_INVALID_HID;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    fapl_id = H5I_INVALID_HID;

    /*----------------------------------------------------------------------
     *  Start to verify the revision
     *----------------------------------------------------------------------
     */
    /*----------------------------------------------------------------------
     * Verify the second revision
     *----------------------------------------------------------------------
     */
    onion_info.revision_num = 2;

    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;

    if ((file_id = H5Fopen(paths->canon, H5F_ACC_RDONLY, fapl_id)) < 0)
        TEST_ERROR;
    if ((dset = H5Dopen2(file_id, "DS1", H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if (H5Dread(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata) < 0)
        TEST_ERROR;

    for (int i = 0; i < ONE_DIM_SIZE; i += 20) {
        int expected = i + 2048;
        if (rdata->arr[i] != expected) {
            printf("ERROR!!! Expected: %d, Got: %d\n", expected, rdata->arr[i]);
            TEST_ERROR;
        }
    }

    /* Close everything */
    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    if (H5Pclose(onion_info.backing_fapl_id) < 0)
        TEST_ERROR;

    HDremove(paths->canon);
    HDremove(paths->onion);
    HDremove(paths->recovery);
    onion_filepaths_destroy(paths);

    free(wdata);
    free(rdata);
    free(dset_data);

    PASSED();
    return 0;

error:

    if (paths != NULL) {
        HDremove(paths->canon);
        HDremove(paths->onion);
        HDremove(paths->recovery);
        onion_filepaths_destroy(paths);
    }

    H5E_BEGIN_TRY
    {
        H5Dclose(dset);
        H5Fclose(file_id);
        H5Pclose(fapl_id);
        H5Pclose(onion_info.backing_fapl_id);
    }
    H5E_END_TRY

    free(wdata);
    free(rdata);
    free(dset_data);

    return -1;
} /* end test_integration_create_simple() */

static int
test_integration_create_delete_objects(void)
{
    const char             *basename   = "integration_objs.h5";
    hid_t                   fapl_id    = H5I_INVALID_HID;
    struct onion_filepaths *paths      = NULL;
    H5FD_onion_fapl_info_t  onion_info = {
        H5FD_ONION_FAPL_INFO_VERSION_CURR,
        H5I_INVALID_HID,               /* backing_fapl_id  */
        ONION_TEST_PAGE_SIZE_5,        /* page_size        */
        H5FD_ONION_STORE_TARGET_ONION, /* store_target     */
        H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST,
        0,               /* force_write_open */
        0,               /* creation flags, was H5FD_ONION_FAPL_INFO_CREATE_FLAG_ENABLE_PAGE_ALIGNMENT */
        "initial commit" /* comment          */
    };
    hid_t   group_id      = H5I_INVALID_HID;
    hid_t   attr_space_id = H5I_INVALID_HID;
    hid_t   attr_id       = H5I_INVALID_HID;
    hsize_t attr_dim[1]   = {4};

    hid_t   file    = H5I_INVALID_HID;
    hid_t   space   = H5I_INVALID_HID;
    hid_t   dset    = H5I_INVALID_HID;
    hid_t   dcpl    = H5I_INVALID_HID;
    hsize_t dims[2] = {4, 4}, maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED}, chunk[2] = {4, 4};
    int     wdata[4][4], /* Write buffer */
        fillval, i, j;

    TESTING("onion-created HDF5 file with revisions testing addition and deletion of objects");

    /* Set up */
    if ((onion_info.backing_fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;

    if (NULL == (paths = onion_filepaths_init(basename)))
        TEST_ERROR;

    HDremove(paths->canon);
    HDremove(paths->onion);
    HDremove(paths->recovery);

    /*----------------------------------------------------------------------
     * Create the skeleton file (create the file without Onion VFD)
     *----------------------------------------------------------------------
     */

    /* Initialize data */
    for (i = 0; i < 4; i++)
        for (j = 0; j < 4; j++)
            wdata[i][j] = i + j;

    /* Create a new file using the default properties */
    if ((file = H5Fcreate(paths->canon, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create dataspace with unlimited dimensions */
    if ((space = H5Screate_simple(2, dims, maxdims)) < 0)
        TEST_ERROR;

    /* Create the dataset creation property list, and set the chunk
     * size
     */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_chunk(dcpl, 2, chunk) < 0)
        TEST_ERROR;

    /* Set the fill value for the dataset */
    fillval = 99;
    if (H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval) < 0)
        TEST_ERROR;

    /* Set the allocation time to "early".  This way we can be sure
     * that reading from the dataset immediately after creation will
     * return the fill value.
     */
    if (H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0)
        TEST_ERROR;

    /* Create the dataset using the dataset creation property list */
    if ((dset = H5Dcreate2(file, "DS1", H5T_STD_I32LE, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Write the data to the dataset */
    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata) < 0)
        TEST_ERROR;

    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    if (H5Fclose(file) < 0)
        TEST_ERROR;

    /*----------------------------------------------------------------------
     * First revision: open the file with Onion VFD and add a dataset (DS2) to the file
     *----------------------------------------------------------------------
     */
    if ((file = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    /* Create the dataset using the dataset creation property list */
    if ((dset = H5Dcreate2(file, "DS2", H5T_STD_I32LE, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Write the data to the dataset */
    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata) < 0)
        TEST_ERROR;

    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;
    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;

    /*----------------------------------------------------------------------
     * Second revision: open the file with Onion VFD and remove the dataset (DS2),
     * which was added during the first revision.
     *----------------------------------------------------------------------
     */
    if ((file = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    if (H5Ldelete(file, "DS2", H5P_DEFAULT) < 0)
        TEST_ERROR;

    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;

    /*----------------------------------------------------------------------
     * Third revision: open the file with Onion VFD and add an attribute to the file
     *----------------------------------------------------------------------
     */
    if ((file = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    /* Create dataspace for attribute */
    if ((attr_space_id = H5Screate_simple(1, attr_dim, NULL)) < 0)
        TEST_ERROR;

    if ((attr_id =
             H5Acreate2(file, "file_attribute", H5T_STD_I32LE, attr_space_id, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if (H5Sclose(attr_space_id) < 0)
        TEST_ERROR;
    if (H5Aclose(attr_id) < 0)
        TEST_ERROR;
    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;

    /*----------------------------------------------------------------------
     * Fourth revision: open the file with Onion VFD and delete the attribute
     *----------------------------------------------------------------------
     */
    if ((file = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    if (H5Adelete(file, "file_attribute") < 0)
        TEST_ERROR;

    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;

    /*----------------------------------------------------------------------
     * Fifth revision: open the file with Onion VFD and add a group to the file
     *----------------------------------------------------------------------
     */
    if ((file = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    if ((group_id = H5Gcreate2(file, "new_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;

    /*----------------------------------------------------------------------
     * Sixth revision: open the file with Onion VFD and delete the newly added group
     *----------------------------------------------------------------------
     */
    if ((file = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    if (H5Ldelete(file, "new_group", H5P_DEFAULT) < 0)
        TEST_ERROR;

    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;

    /*----------------------------------------------------------------------
     *  Start to verify the revision
     *----------------------------------------------------------------------
     */
    /*----------------------------------------------------------------------
     * Verify the first revision: it should have the second dataset (DS2)
     *----------------------------------------------------------------------
     */
    onion_info.revision_num = 1;

    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;
    if ((file = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    /* The second dataset (DS2) should exist */
    if (H5Lexists(file, "DS2", H5P_DEFAULT) <= 0)
        TEST_ERROR;

    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;

    /*------------------------------------------------------------------------
     * Verify the second revision: the second dataset (DS2) should be removed
     *------------------------------------------------------------------------
     */
    onion_info.revision_num = 2;

    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;
    if ((file = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    /* The second dataset (DS2) shouldn't exist */
    if (H5Lexists(file, "DS2", H5P_DEFAULT) > 0)
        TEST_ERROR;

    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;

    /*-------------------------------------------------------------------------
     * Verify the third revision: the file attribute (file_attribute) should exist
     *-------------------------------------------------------------------------
     */
    onion_info.revision_num = 3;

    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;
    if ((file = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    /* The file attribute should exist */
    if (H5Aexists(file, "file_attribute") <= 0)
        TEST_ERROR;

    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;

    /*-------------------------------------------------------------------------
     * Verify the fourth revision: the file attribute (file_attribute) should be removed
     *-------------------------------------------------------------------------
     */
    onion_info.revision_num = 4;

    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;
    if ((file = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    /* The file attribute should be removed */
    if (H5Aexists(file, "file_attribute") > 0)
        TEST_ERROR;

    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;

    /*-------------------------------------------------------------------------
     * Verify the fifth revision: the group (new_group) should exist
     *-------------------------------------------------------------------------
     */
    onion_info.revision_num = 5;

    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;
    if ((file = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    /* The new group should exist */
    if (H5Lexists(file, "new_group", H5P_DEFAULT) <= 0)
        TEST_ERROR;

    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;

    /*-------------------------------------------------------------------------
     * Verify the sixth revision: the group (new_group) should be removed
     *-------------------------------------------------------------------------
     */
    onion_info.revision_num = 6;

    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;
    if ((file = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    /* The new group should exist */
    if (H5Lexists(file, "new_group", H5P_DEFAULT) > 0)
        TEST_ERROR;

    /* Close everything */
    if (H5Fclose(file) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    if (H5Sclose(space) < 0)
        TEST_ERROR;
    if (H5Pclose(onion_info.backing_fapl_id) < 0)
        TEST_ERROR;

    HDremove(paths->canon);
    HDremove(paths->onion);
    HDremove(paths->recovery);
    onion_filepaths_destroy(paths);

    PASSED();
    return 0;

error:

    if (paths != NULL) {
        HDremove(paths->canon);
        HDremove(paths->onion);
        HDremove(paths->recovery);
        onion_filepaths_destroy(paths);
    }

    H5E_BEGIN_TRY
    {
        H5Dclose(dset);
        H5Fclose(file);
        H5Pclose(fapl_id);
        H5Sclose(space);
        H5Pclose(onion_info.backing_fapl_id);
    }
    H5E_END_TRY

    return -1;
} /* end test_integration_create_delete_objects */

static int
test_integration_dset_extension(void)
{
    const char             *basename   = "integration_dset_ext.h5";
    hid_t                   fapl_id    = H5I_INVALID_HID;
    struct onion_filepaths *paths      = NULL;
    H5FD_onion_fapl_info_t  onion_info = {
        H5FD_ONION_FAPL_INFO_VERSION_CURR,
        H5I_INVALID_HID,               /* backing_fapl_id  */
        ONION_TEST_PAGE_SIZE_5,        /* page_size        */
        H5FD_ONION_STORE_TARGET_ONION, /* store_target     */
        H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST,
        0,               /* force_write_open */
        0,               /* creation flags, was H5FD_ONION_FAPL_INFO_CREATE_FLAG_ENABLE_PAGE_ALIGNMENT */
        "initial commit" /* comment          */
    };
    hid_t   file       = H5I_INVALID_HID;
    hid_t   space      = H5I_INVALID_HID;
    hid_t   dset_space = H5I_INVALID_HID;
    hid_t   dset       = H5I_INVALID_HID;
    hid_t   dcpl       = H5I_INVALID_HID;
    hsize_t dims[2]    = {4, 4};
    hsize_t maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED};
    hsize_t chunk[2]   = {4, 4};
    hsize_t size[2];
    hsize_t offset[2];
    int     wdata[4][4]; /* Write buffer */
    int     fillval;
    int     rdata[4][4]; /* Read buffer */

    TESTING("onion-created HDF5 file with revisions testing dataset extension");

    /* Setup */
    if ((onion_info.backing_fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;

    if (NULL == (paths = onion_filepaths_init(basename)))
        TEST_ERROR;

    HDremove(paths->canon);
    HDremove(paths->onion);
    HDremove(paths->recovery);

    /*----------------------------------------------------------------------
     * Create the skeleton file (create the file without Onion VFD)
     *----------------------------------------------------------------------
     */

    /* Initialize data */
    for (int i = 0; i < 4; i++)
        for (int j = 0; j < 4; j++)
            wdata[i][j] = i + j;

    /* Create a new file using the default properties */
    if ((file = H5Fcreate(paths->canon, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create dataspace with unlimited dimensions*/
    if ((space = H5Screate_simple(2, dims, maxdims)) < 0)
        TEST_ERROR;

    /* Create the dataset creation property list, and set the chunk
     * size
     */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_chunk(dcpl, 2, chunk) < 0)
        TEST_ERROR;

    /* Set the fill value for the dataset */
    fillval = 99;
    if (H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval) < 0)
        TEST_ERROR;

    /* Set the allocation time to "early".  This way we can be sure
     * that reading from the dataset immediately after creation will
     * return the fill value.
     */
    if (H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0)
        TEST_ERROR;

    /* Create the dataset using the dataset creation property list */
    if ((dset = H5Dcreate2(file, "DS1", H5T_STD_I32LE, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Write the data to the dataset */
    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata) < 0)
        TEST_ERROR;

    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    if (H5Fclose(file) < 0)
        TEST_ERROR;

    /*----------------------------------------------------------------------
     * First revision: open the file with Onion VFD and extend the dataset
     *----------------------------------------------------------------------
     */
    if ((file = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    /* Open the dataset */
    if ((dset = H5Dopen2(file, "DS1", H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Extend the dataset and double the rows */
    size[0] = 2 * dims[0];
    size[1] = dims[1];
    if (H5Dset_extent(dset, size) < 0)
        TEST_ERROR;

    if ((dset_space = H5Dget_space(dset)) < 0)
        TEST_ERROR;

    offset[0] = dims[0];
    offset[1] = 0;
    if (H5Sselect_hyperslab(dset_space, H5S_SELECT_SET, offset, NULL, dims, NULL) < 0)
        TEST_ERROR;

    /* Write the data to the dataset. */
    if (H5Dwrite(dset, H5T_NATIVE_INT, space, dset_space, H5P_DEFAULT, wdata) < 0)
        TEST_ERROR;

    if (H5Sclose(dset_space) < 0)
        TEST_ERROR;

    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;
    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;

    /*----------------------------------------------------------------------
     * Second revision: open the file with Onion VFD and shrink the dataset
     *----------------------------------------------------------------------
     */
    if ((file = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    /* Open the dataset */
    if ((dset = H5Dopen2(file, "DS1", H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Extend the dataset and shrink back the size */
    if (H5Dset_extent(dset, dims) < 0)
        TEST_ERROR;

    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;
    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;

    /*----------------------------------------------------------------------
     *  Start to verify the revision
     *----------------------------------------------------------------------
     */
    /*----------------------------------------------------------------------
     * Verify the first revision: it should have the extended data
     *----------------------------------------------------------------------
     */
    onion_info.revision_num = 1;

    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;
    if ((file = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    /* Open the dataset */
    if ((dset = H5Dopen2(file, "DS1", H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if ((dset_space = H5Dget_space(dset)) < 0)
        TEST_ERROR;

    offset[0] = dims[0];
    offset[1] = 0;
    if (H5Sselect_hyperslab(dset_space, H5S_SELECT_SET, offset, NULL, dims, NULL) < 0)
        TEST_ERROR;

    if (H5Dread(dset, H5T_NATIVE_INT, space, dset_space, H5P_DEFAULT, rdata) < 0)
        TEST_ERROR;

    for (int i = 0; i < 4; i++)
        for (int j = 0; j < 4; j++)
            if (rdata[i][j] != wdata[i][j])
                TEST_ERROR;

    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;

    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;

    /*----------------------------------------------------------------------
     * Verify the second revision: it should have the original data
     *----------------------------------------------------------------------
     */
    onion_info.revision_num = 2;

    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;
    if ((file = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    /* Open the dataset */
    dset = H5Dopen2(file, "DS1", H5P_DEFAULT);

    if ((dset_space = H5Dget_space(dset)) < 0)
        TEST_ERROR;

    if (H5Dread(dset, H5T_NATIVE_INT, space, dset_space, H5P_DEFAULT, rdata) < 0)
        TEST_ERROR;

    for (int i = 0; i < 4; i++)
        for (int j = 0; j < 4; j++)
            if (rdata[i][j] != wdata[i][j])
                TEST_ERROR;

    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;

    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;

    /* Close and release resources. */
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    if (H5Sclose(space) < 0)
        TEST_ERROR;
    if (H5Pclose(onion_info.backing_fapl_id) < 0)
        TEST_ERROR;

    HDremove(paths->canon);
    HDremove(paths->onion);
    HDremove(paths->recovery);
    onion_filepaths_destroy(paths);

    PASSED();
    return 0;

error:

    if (paths != NULL) {
        HDremove(paths->canon);
        HDremove(paths->onion);
        HDremove(paths->recovery);
        onion_filepaths_destroy(paths);
    }

    H5E_BEGIN_TRY
    {
        H5Dclose(dset);
        H5Fclose(file);
        H5Pclose(fapl_id);
        H5Pclose(onion_info.backing_fapl_id);
    }
    H5E_END_TRY

    return -1;
} /* end test_integration_dset_extension */

static int
test_integration_ctl(void)
{
    const char             *basename   = "integration_ctl.h5";
    hid_t                   file       = H5I_INVALID_HID;
    hid_t                   space      = H5I_INVALID_HID;
    hid_t                   dset       = H5I_INVALID_HID;
    hid_t                   dcpl       = H5I_INVALID_HID;
    hid_t                   fapl_id    = H5I_INVALID_HID;
    hsize_t                 dims[2]    = {4, 4};
    hsize_t                 maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED};
    hsize_t                 chunk[2]   = {4, 4};
    int                     wdata[4][4]; /* Write buffer */
    int                     fillval;
    struct onion_filepaths *paths      = NULL;
    H5FD_onion_fapl_info_t  onion_info = {
        H5FD_ONION_FAPL_INFO_VERSION_CURR,
        H5I_INVALID_HID,               /* backing_fapl_id  */
        ONION_TEST_PAGE_SIZE_5,        /* page_size        */
        H5FD_ONION_STORE_TARGET_ONION, /* store_target     */
        H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST,
        0,               /* force_write_open */
        0,               /* creation flags, was H5FD_ONION_FAPL_INFO_CREATE_FLAG_ENABLE_PAGE_ALIGNMENT */
        "initial commit" /* comment */
    };
    uint64_t revision_count;

    TESTING("onion-created HDF5 file with revisions testing H5FDctl");

    /* Set up */
    if ((onion_info.backing_fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;

    if (NULL == (paths = onion_filepaths_init(basename)))
        TEST_ERROR;

    HDremove(paths->canon);
    HDremove(paths->onion);
    HDremove(paths->recovery);

    /*----------------------------------------------------------------------
     * Create the skeleton file (create the file without Onion VFD)
     *----------------------------------------------------------------------
     */

    /* Initialize data */
    for (int i = 0; i < 4; i++)
        for (int j = 0; j < 4; j++)
            wdata[i][j] = i + j;

    /* Create a new file using the default properties */
    if ((file = H5Fcreate(paths->canon, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create dataspace with unlimited dimensions */
    if ((space = H5Screate_simple(2, dims, maxdims)) < 0)
        TEST_ERROR;

    /* Create the dataset creation property list, and set the chunk size */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_chunk(dcpl, 2, chunk) < 0)
        TEST_ERROR;

    /* Set the fill value for the dataset */
    fillval = 99;
    if (H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval) < 0)
        TEST_ERROR;

    /* Set the allocation time to "early".  This way we can be sure
     * that reading from the dataset immediately after creation will
     * return the fill value.
     */
    if (H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0)
        TEST_ERROR;

    /* Create the dataset using the dataset creation property list */
    if ((dset = H5Dcreate2(file, "DS1", H5T_STD_I32LE, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Write the data to the dataset */
    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata) < 0)
        TEST_ERROR;

    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    if (H5Fclose(file) < 0)
        TEST_ERROR;

    /*----------------------------------------------------------------------
     * First revision: open the file with Onion VFD and add a dataset (DS2) to the file
     *----------------------------------------------------------------------
     */
    if ((file = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    /* Create the dataset using the dataset creation property list */
    if ((dset = H5Dcreate2(file, "DS2", H5T_STD_I32LE, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Write the data to the dataset */
    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata) < 0)
        TEST_ERROR;

    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;
    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;

    /*----------------------------------------------------------------------
     * Second revision: open the file with Onion VFD and remove the dataset (DS2),
     * which was added during the first revision.
     *----------------------------------------------------------------------
     */
    if ((file = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    if (H5Ldelete(file, "DS2", H5P_DEFAULT) < 0)
        TEST_ERROR;

    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;

    /*----------------------------------------------------------------------
     *  Start to verify the number of revisions
     *----------------------------------------------------------------------
     */
    if (H5FDonion_get_revision_count(basename, fapl_id, &revision_count) < 0)
        TEST_ERROR;

    if (2 != revision_count)
        TEST_ERROR;

    /* Close and release resources */
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    if (H5Sclose(space) < 0)
        TEST_ERROR;
    if (H5Pclose(onion_info.backing_fapl_id) < 0)
        TEST_ERROR;

    HDremove(paths->canon);
    HDremove(paths->onion);
    HDremove(paths->recovery);
    onion_filepaths_destroy(paths);

    PASSED();
    return 0;

error:

    if (paths != NULL) {
        HDremove(paths->canon);
        HDremove(paths->onion);
        HDremove(paths->recovery);
        onion_filepaths_destroy(paths);
    }

    H5E_BEGIN_TRY
    {
        H5Dclose(dset);
        H5Fclose(file);
        H5Pclose(fapl_id);
        H5Pclose(onion_info.backing_fapl_id);
    }
    H5E_END_TRY

    return -1;
}

static int
test_integration_reference(void)
{
    const char *basename   = "integration_refer.h5";
    hid_t       file       = H5I_INVALID_HID;
    hid_t       group      = H5I_INVALID_HID;
    hid_t       space      = H5I_INVALID_HID;
    hid_t       space2     = H5I_INVALID_HID;
    hid_t       space_ref  = H5I_INVALID_HID;
    hid_t       dset       = H5I_INVALID_HID;
    hid_t       dset2      = H5I_INVALID_HID;
    hid_t       fapl_id    = H5I_INVALID_HID;
    hsize_t     dims[2]    = {4, 4};
    hsize_t     dim_ref[1] = {2};
    int         wdata[4][4]; /* Write buffer */
    int         rdata[4][4]; /* Read buffer  */
    H5R_ref_t   wbuf[2];
    H5R_ref_t   rbuf[2];
    H5O_type_t  obj_type;
    hsize_t     start[2];
    hsize_t     stride[2];
    hsize_t     count[2];
    hsize_t     block[2];
    hsize_t     coord1[4][2]; /* Coordinates for point selection */
    hssize_t    nelmts;

    struct onion_filepaths *paths      = NULL;
    H5FD_onion_fapl_info_t  onion_info = {
        H5FD_ONION_FAPL_INFO_VERSION_CURR,
        H5I_INVALID_HID,               /* backing_fapl_id  */
        ONION_TEST_PAGE_SIZE_5,        /* page_size        */
        H5FD_ONION_STORE_TARGET_ONION, /* store_target     */
        H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST,
        0,               /* force_write_open */
        0,               /* creation flags, was H5FD_ONION_FAPL_INFO_CREATE_FLAG_ENABLE_PAGE_ALIGNMENT */
        "initial commit" /* comment */
    };

    TESTING("onion-created HDF5 file with revisions testing references");

    /* Set up */
    if ((onion_info.backing_fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;

    if (NULL == (paths = onion_filepaths_init(basename)))
        TEST_ERROR;

    HDremove(paths->canon);
    HDremove(paths->onion);
    HDremove(paths->recovery);

    /*----------------------------------------------------------------------
     * Create the skeleton file (create the file without Onion VFD)
     *----------------------------------------------------------------------
     */

    /* Initialize data */
    for (int i = 0; i < 4; i++)
        for (int j = 0; j < 4; j++)
            wdata[i][j] = i + j;

    /* Create a new file using the default properties */
    if ((file = H5Fcreate(paths->canon, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create dataspace */
    if ((space = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Create the dataset using the dataset creation property list */
    if ((dset = H5Dcreate2(file, "DS1", H5T_STD_I32LE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Write the data to the dataset */
    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata) < 0)
        TEST_ERROR;

    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    if (H5Fclose(file) < 0)
        TEST_ERROR;

    /*----------------------------------------------------------------------
     * First revision: open the file with Onion VFD and add a dataset (DS2)
     * of object references
     *----------------------------------------------------------------------
     */
    if ((file = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    /* Create dataspace with unlimited dimensions */
    if ((space_ref = H5Screate_simple(1, dim_ref, NULL)) < 0)
        TEST_ERROR;

    /* Create the dataset of object references */
    if ((dset = H5Dcreate2(file, "DS2", H5T_STD_REF, space_ref, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create reference to dataset */
    if (H5Rcreate_object(file, "DS1", H5P_DEFAULT, &wbuf[0]) < 0)
        TEST_ERROR;

    if (H5Rget_obj_type3(&wbuf[0], H5P_DEFAULT, &obj_type) < 0)
        TEST_ERROR;

    if (obj_type != H5O_TYPE_DATASET)
        TEST_ERROR;

    /* Create reference to the root group */
    if (H5Rcreate_object(file, "/", H5P_DEFAULT, &wbuf[1]) < 0)
        TEST_ERROR;

    if (H5Rget_obj_type3(&wbuf[1], H5P_DEFAULT, &obj_type) < 0)
        TEST_ERROR;

    if (obj_type != H5O_TYPE_GROUP)
        TEST_ERROR;

    /* Write the object reference data to the dataset */
    if (H5Dwrite(dset, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0)
        TEST_ERROR;

    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;

    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;

    for (int i = 0; i < 2; i++)
        if (H5Rdestroy(&wbuf[i]) < 0)
            TEST_ERROR;

    /*----------------------------------------------------------------------
     * Second revision: open the file with Onion VFD and add a dataset (DS3)
     * of region references
     *----------------------------------------------------------------------
     */
    if ((file = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    /* Create the dataset of region references */
    if ((dset = H5Dcreate2(file, "DS3", H5T_STD_REF, space_ref, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Select 2x4 hyperslab for first reference */
    start[0]  = 0;
    start[1]  = 0;
    stride[0] = 1;
    stride[1] = 1;
    count[0]  = 1;
    count[1]  = 1;
    block[0]  = 2;
    block[1]  = 4;

    /* Make a hyperslab selection of 2x4 elements */
    if (H5Sselect_hyperslab(space, H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR;

    /* Verify the number of selection */
    if ((nelmts = H5Sget_select_npoints(space)) != 8) {
        printf("Number of selected elements is supposed to be 8, but got %" PRIuHSIZE "\n", nelmts);
        TEST_ERROR;
    }

    /* Store first data region */
    if (H5Rcreate_region(file, "/DS1", space, H5P_DEFAULT, &wbuf[0]) < 0)
        TEST_ERROR;

    if (H5Rget_obj_type3(&wbuf[0], H5P_DEFAULT, &obj_type) < 0)
        TEST_ERROR;

    if (obj_type != H5O_TYPE_DATASET)
        TEST_ERROR;

    /* Select the sequence of four points for the second reference */
    coord1[0][0] = 0;
    coord1[0][1] = 0;
    coord1[1][0] = 1;
    coord1[1][1] = 1;
    coord1[2][0] = 2;
    coord1[2][1] = 2;
    coord1[3][0] = 3;
    coord1[3][1] = 3;

    if (H5Sselect_elements(space, H5S_SELECT_SET, 4, (const hsize_t *)coord1) < 0)
        TEST_ERROR;

    /* Store the second data region */
    if (H5Rcreate_region(file, "/DS1", space, H5P_DEFAULT, &wbuf[1]) < 0)
        TEST_ERROR;

    if (H5Rget_obj_type3(&wbuf[1], H5P_DEFAULT, &obj_type) < 0)
        TEST_ERROR;

    if (obj_type != H5O_TYPE_DATASET)
        TEST_ERROR;

    /* Write the region reference data to the dataset */
    if (H5Dwrite(dset, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0)
        TEST_ERROR;

    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;

    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;

    for (int i = 0; i < 2; i++)
        if (H5Rdestroy(&wbuf[i]) < 0)
            TEST_ERROR;

    /*----------------------------------------------------------------------
     *  Start to verify the revisions
     *----------------------------------------------------------------------
     */
    /*----------------------------------------------------------------------
     * Verify the first revision: it should have the object references
     *----------------------------------------------------------------------
     */
    onion_info.revision_num = 1;

    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;

    if ((file = H5Fopen(paths->canon, H5F_ACC_RDONLY, fapl_id)) < 0)
        TEST_ERROR;

    /* Open the dataset of the object references */
    if ((dset = H5Dopen2(file, "DS2", H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if (H5Dread(dset, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
        TEST_ERROR;

    /* Open the referenced dataset and check the data */
    if ((dset2 = H5Ropen_object(&rbuf[0], H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if (H5Dread(dset2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata) < 0)
        TEST_ERROR;

    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            int expected = i + j;
            if (rdata[i][j] != expected) {
                printf("ERROR!!! Expected: %d, Got: %d\n", expected, rdata[i][j]);
                TEST_ERROR;
            }
        }
    }

    /* Open the referenced group and make sure it's a group object */
    if ((group = H5Ropen_object(&rbuf[1], H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if (H5I_GROUP != H5Iget_type(group))
        TEST_ERROR;

    if (H5Gclose(group) < 0)
        TEST_ERROR;

    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;

    if (H5Dclose(dset2) < 0)
        TEST_ERROR;
    dset2 = H5I_INVALID_HID;

    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;

    for (int i = 0; i < 2; i++)
        if (H5Rdestroy(&rbuf[i]) < 0)
            TEST_ERROR;

    /*----------------------------------------------------------------------
     * Verify the second revision: it should have the region references
     *----------------------------------------------------------------------
     */
    onion_info.revision_num = 2;

    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;

    if ((file = H5Fopen(paths->canon, H5F_ACC_RDONLY, fapl_id)) < 0)
        TEST_ERROR;

    /* Open the dataset of the region reference */
    if ((dset = H5Dopen2(file, "DS3", H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if (H5Dread(dset, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
        TEST_ERROR;

    /* Get the hyperslab selection and check the referenced region of the dataset */
    if ((space2 = H5Ropen_region(&rbuf[0], H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if ((nelmts = H5Sget_select_npoints(space2)) != 8) {
        printf("Number of selected elements is supposed to be 8, but got %" PRIuHSIZE "\n", nelmts);
        TEST_ERROR;
    }

    if (H5Sclose(space2) < 0)
        TEST_ERROR;
    space2 = H5I_INVALID_HID;

    /* Get the element selection and check the referenced region of the dataset */
    if ((space2 = H5Ropen_region(&rbuf[1], H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if ((nelmts = H5Sget_select_npoints(space2)) != 4) {
        printf("Number of selected elements is supposed to be 4, but got %" PRIuHSIZE "\n", nelmts);
        TEST_ERROR;
    }

    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;

    if (H5Sclose(space2) < 0)
        TEST_ERROR;
    space2 = H5I_INVALID_HID;

    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;

    for (int i = 0; i < 2; i++)
        if (H5Rdestroy(&rbuf[i]) < 0)
            TEST_ERROR;

    /* Close and release resources */
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    if (H5Sclose(space) < 0)
        TEST_ERROR;
    if (H5Sclose(space_ref) < 0)
        TEST_ERROR;
    if (H5Pclose(onion_info.backing_fapl_id) < 0)
        TEST_ERROR;

    HDremove(paths->canon);
    HDremove(paths->onion);
    HDremove(paths->recovery);
    onion_filepaths_destroy(paths);

    PASSED();
    return 0;

error:

    if (paths != NULL) {
        HDremove(paths->canon);
        HDremove(paths->onion);
        HDremove(paths->recovery);
        onion_filepaths_destroy(paths);
    }

    H5E_BEGIN_TRY
    {
        H5Dclose(dset);
        H5Fclose(file);
        H5Pclose(fapl_id);
        H5Pclose(onion_info.backing_fapl_id);
    }
    H5E_END_TRY

    return -1;
}

static int
test_integration_create_by_name(void)
{
    const char             *basename   = "integration_by_name.h5";
    hid_t                   fapl_id    = H5I_INVALID_HID;
    struct onion_filepaths *paths      = NULL;
    hid_t                   file_id    = H5I_INVALID_HID;
    hid_t                   file       = H5I_INVALID_HID;
    hid_t                   space      = H5I_INVALID_HID;
    hid_t                   dset       = H5I_INVALID_HID;
    hid_t                   dcpl       = H5I_INVALID_HID;
    hsize_t                 dims[2]    = {1, ONE_DIM_SIZE};
    hsize_t                 maxdims[2] = {1, ONE_DIM_SIZE};
    int                     fillval;
    struct {
        int arr[ONE_DIM_SIZE];
    } *wdata = NULL;
    struct {
        int arr[ONE_DIM_SIZE];
    } *rdata = NULL;
    struct {
        int arr[ONE_DIM_SIZE];
    } *dset_data = NULL;

    TESTING("H5Pset_driver_by_name");

    /* Setup */
    if (NULL == (wdata = calloc(1, sizeof(*wdata))))
        TEST_ERROR;
    if (NULL == (rdata = calloc(1, sizeof(*rdata))))
        TEST_ERROR;
    if (NULL == (dset_data = calloc(1, sizeof(*dset_data))))
        TEST_ERROR;

    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    /* Use H5Pset_driver_by_name to enable the Onion VFD */
    if (H5Pset_driver_by_name(fapl_id, "onion", "{revision_num: H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST}") <
        0)
        TEST_ERROR;

    if (NULL == (paths = onion_filepaths_init(basename)))
        TEST_ERROR;

    HDremove(paths->canon);
    HDremove(paths->onion);
    HDremove(paths->recovery);

    /*----------------------------------------------------------------------
     * Create the skeleton file (create the file without Onion VFD)
     *----------------------------------------------------------------------
     */
    /* Initialize data */
    for (int i = 0; i < ONE_DIM_SIZE; i++)
        wdata->arr[i] = i;

    /* Create a new file using the default properties */
    if ((file = H5Fcreate(paths->canon, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create dataspace with unlimited dimensions*/
    if ((space = H5Screate_simple(2, dims, maxdims)) < 0)
        TEST_ERROR;

    /* Create the dataset creation property list */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    /* Set the fill value for the dataset */
    fillval = 99;
    if (H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval) < 0)
        TEST_ERROR;

    /* Set the allocation time to "early".  This way we can be sure
     * that reading from the dataset immediately after creation will
     * return the fill value.
     */
    if (H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0)
        TEST_ERROR;

    /* Create the dataset using the dataset creation property list */
    if ((dset = H5Dcreate2(file, "DS1", H5T_STD_I32LE, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Write the data to the dataset */
    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata) < 0)
        TEST_ERROR;

    /* Close everything */
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    if (H5Sclose(space) < 0)
        TEST_ERROR;
    if (H5Fclose(file) < 0)
        TEST_ERROR;

    /*----------------------------------------------------------------------
     * First revision: open the file with Onion VFD and change the data
     *----------------------------------------------------------------------
     */
    if ((file_id = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    if ((dset = H5Dopen2(file_id, "DS1", H5P_DEFAULT)) < 0)
        TEST_ERROR;

    for (int i = 0; i < ONE_DIM_SIZE; i++)
        dset_data->arr[i] = i + ONE_DIM_SIZE;

    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset_data) < 0)
        TEST_ERROR;

    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;
    file_id = H5I_INVALID_HID;

    /*----------------------------------------------------------------------
     * Second revision: open the file with Onion VFD and change the data
     *----------------------------------------------------------------------
     */
    if ((file_id = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;
    if ((dset = H5Dopen2(file_id, "DS1", H5P_DEFAULT)) < 0)
        TEST_ERROR;

    for (int i = 0; i < ONE_DIM_SIZE; i++)
        dset_data->arr[i] = i + 2048;

    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset_data) < 0)
        TEST_ERROR;

    /* CLEANUP */
    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;
    file_id = H5I_INVALID_HID;

    /*----------------------------------------------------------------------
     * Third revision: open the file with Onion VFD and change the data
     *----------------------------------------------------------------------
     */
    if ((file_id = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;
    if ((dset = H5Dopen2(file_id, "DS1", H5P_DEFAULT)) < 0)
        TEST_ERROR;

    for (int i = 0; i < ONE_DIM_SIZE; i += 20)
        dset_data->arr[i] = i + 3072;

    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset_data) < 0)
        TEST_ERROR;

    /* CLEANUP */
    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;
    file_id = H5I_INVALID_HID;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    fapl_id = H5I_INVALID_HID;

    /*----------------------------------------------------------------------
     *  Start to verify the revision with H5Pset_driver_by_name
     *----------------------------------------------------------------------
     */
    /*----------------------------------------------------------------------
     * Verify the second revision
     *----------------------------------------------------------------------
     */
    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_driver_by_name(fapl_id, "onion", "{revision_num: 2; page_size: 4; }") < 0)
        TEST_ERROR;

    if ((file_id = H5Fopen(paths->canon, H5F_ACC_RDONLY, fapl_id)) < 0)
        TEST_ERROR;
    if ((dset = H5Dopen2(file_id, "DS1", H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if (H5Dread(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata) < 0)
        TEST_ERROR;

    for (int i = 0; i < ONE_DIM_SIZE; i += 20) {
        int expected = i + 2048;
        if (rdata->arr[i] != expected) {
            printf("ERROR!!! Expected: %d, Got: %d\n", expected, rdata->arr[i]);
            TEST_ERROR;
        }
    }

    /* Close everything */
    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    HDremove(paths->canon);
    HDremove(paths->onion);
    HDremove(paths->recovery);
    onion_filepaths_destroy(paths);

    free(wdata);
    free(rdata);
    free(dset_data);

    PASSED();
    return 0;

error:

    if (paths != NULL) {
        HDremove(paths->canon);
        HDremove(paths->onion);
        HDremove(paths->recovery);
        onion_filepaths_destroy(paths);
    }

    H5E_BEGIN_TRY
    {
        H5Dclose(dset);
        H5Fclose(file_id);
        H5Pclose(fapl_id);
    }
    H5E_END_TRY

    free(wdata);
    free(rdata);
    free(dset_data);

    return -1;
} /* end test_integration_create_simple() */

/*-----------------------------------------------------------------------------
 *
 * Function:     main()
 *
 * Purpose:      Perform unit tests on for the Onion VFD.
 *
 *-----------------------------------------------------------------------------
 */
int
main(void)
{
    const char *driver_name; /* VFD value from environment */
    int         nerrors = 0;

    printf("Testing Onion VFD functionality.\n");

    h5_reset();

    /* The onion VFD only supports the sec2 VFD under the hood, so skip this
     * test when the environment variable has been set to something else
     */
    driver_name = h5_get_test_driver_name();
    if (0 != strcmp(driver_name, "sec2")) {
        SKIPPED();
        puts("Onion VFD test skipped due to non-sec2 default VFD");
        exit(EXIT_SUCCESS);
    }

    /* Initialize */
    flags_create_s = H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC;

    /* Run tests. Return values on error are negative. */
    nerrors -= test_archival_index();
    nerrors -= test_revision_index();
    nerrors -= test_revision_index_collisions();
    nerrors -= test_revision_index_resizing();
    nerrors -= test_revision_index_to_archival_index();
    nerrors -= test_fapl();
    nerrors -= test_header_encode_decode();
    nerrors -= test_history_encode_decode_empty();
    nerrors -= test_history_encode_decode();
    nerrors -= test_revision_record_encode_decode();
    nerrors -= test_create_oniontarget(false, false);
    nerrors -= test_create_oniontarget(true, false);
    nerrors -= test_create_oniontarget(false, true);
    nerrors -= test_create_oniontarget(true, true);
    nerrors -= test_several_revisions_with_logical_gaps();
    nerrors -= test_page_aligned_history_create();
    nerrors -= test_integration_create();
    nerrors -= test_integration_create_simple();
    nerrors -= test_integration_create_delete_objects();
    nerrors -= test_integration_dset_extension();
    nerrors -= test_integration_ctl();
    nerrors -= test_integration_reference();
    nerrors -= test_integration_create_by_name();

    if (nerrors > 0) {
        printf("***** %d Onion TEST%s FAILED! *****\n", nerrors, nerrors > 1 ? "S" : "");
        return EXIT_FAILURE;
    }

    printf("All Onion tests passed.\n");
    return EXIT_SUCCESS;

} /* end main() */
