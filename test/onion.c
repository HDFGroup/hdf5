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

#define ONION_TEST_PAGE_SIZE_1                    (uint32_t)4
#define ONION_TEST_PAGE_SIZE_5                    (uint32_t)32
#define ONION_TEST_BASENAME_SIZE                  32
#define ONION_TEST_FIXNAME_SIZE                   1024
#define ONION_TEST_EXPECTED_HISTORY_REVISIONS_MAX 16
#define ONION_TEST_REV_REV_WRITES_MAX             8

#define WIP 1 /* development toggle */

/* Structure to collect the onion filepaths in one place. */
struct onion_filepaths {
    char *canon;
    char *onion;
    char *recovery;
};

struct expected_revision {
    uint64_t    revision_id;
    uint64_t    parent_revision_id;
    uint64_t    logi_eof;
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
    hbool_t           truncate; /* onion-create, truncating any existing data */
    uint64_t          revision_id;
    size_t            n_writes;
    struct write_info writes[ONION_TEST_REV_REV_WRITES_MAX];
    const char *      comment;
};

static int  compare_file_bytes_exactly(const char *, hid_t, size_t, const unsigned char *);
static int  do_onion_open_and_writes(const char *filename, H5FD_onion_fapl_info_t *onion_info_p, size_t n_ops,
                                     struct revise_revision *about);
static void onion_filepaths_destroy(struct onion_filepaths *);
static struct onion_filepaths *onion_filepaths_init(const char *, H5FD_onion_fapl_info_t *);
#if 0
static int is_onion_data_page_aligned(
        const H5FD_onion_history_header_t *);
static uint32_t up_size_to_page_boundary(uint64_t, uint32_t);
#endif

/* set at runtime in main() */
static unsigned int flags_create_s = 0;

/* NOTE: b_list must be longer than a_list.
 * Sizes must match respective buffer lengths.
 */

/* twenty-six four-character words beginning with 'a' -> 104 bytes */
const unsigned char *a_list_s =
    (const unsigned char *)"abetableacedacesacheacidacneadzeafaragedagesaidsairsajarallyalum"
                           "amokantsapesarcsareaartsasksaspsavidaxes";
uint64_t a_list_size_s = 104;

/* fifty-three four-character words beginning with 'b' -> 212 bytes */
const unsigned char *b_list_s =
    (const unsigned char *)"badebailbaitbalebanebarebaskbeambeanbearbeenbeerbeltbentbestbide"
                           "bikebilebindbirdbiteblipblueboarboatbobsbodyboilboldbollboltbond"
                           "boneboobboorboosbootbradbragbratbraybrewbritbrowbuckbudsbunkbunt"
                           "buoyburnburybustbuys";
uint64_t b_list_size_s = 212;

/* Allocate and populate filepaths with h5_fixname'd strings as appropriate.
 * Should be released with onion_filepaths_destroy() when done.
 */
static struct onion_filepaths *
onion_filepaths_init(const char *basename, H5FD_onion_fapl_info_t *fa_info)
{
    struct onion_filepaths *paths = NULL;

    paths = (struct onion_filepaths *)HDmalloc(sizeof(struct onion_filepaths));
    if (NULL == paths)
        TEST_ERROR;
    paths->canon    = NULL;
    paths->onion    = NULL;
    paths->recovery = NULL;

    paths->canon = (char *)HDmalloc(sizeof(char) * ONION_TEST_FIXNAME_SIZE);
    if (NULL == paths->canon)
        TEST_ERROR;
    if (!h5_fixname_no_suffix(basename, fa_info->backing_fapl_id, paths->canon, ONION_TEST_FIXNAME_SIZE))
        TEST_ERROR;

    paths->onion = (char *)HDmalloc(sizeof(char) * ONION_TEST_FIXNAME_SIZE);
    HDsnprintf(paths->onion, ONION_TEST_FIXNAME_SIZE, "%s.onion", paths->canon);

    paths->recovery = (char *)HDmalloc(sizeof(char) * ONION_TEST_FIXNAME_SIZE);
    HDsnprintf(paths->recovery, ONION_TEST_FIXNAME_SIZE, "%s.onion.recovery", paths->canon);

    return paths;

error:
    if (paths != NULL) {
        if (paths->canon != NULL)
            HDfree(paths->canon);
        if (paths->onion != NULL)
            HDfree(paths->onion);
        if (paths->recovery != NULL)
            HDfree(paths->recovery);
    }
    return NULL;
}

static void
onion_filepaths_destroy(struct onion_filepaths *s)
{
    HDfree(s->canon);
    HDfree(s->onion);
    HDfree(s->recovery);
    HDfree(s);
}

#if 0
static int
is_onion_data_page_aligned(const H5FD_onion_history_header_t *header)
{
    return header->flags & H5FD__ONION_HEADER_FLAG_PAGE_ALIGNMENT;
}

static uint32_t
up_size_to_page_boundary(uint64_t size_in, uint32_t page_size)
{
    uint32_t _i = 0; /* number of pages occupied */
    for (_i = 1; (page_size * _i) < size_in; _i++);
    return page_size * _i;
}
#endif

/*-----------------------------------------------------------------------------
 *
 * Function:    test_archival_index()
 *
 * Purpose:     Unit-test mechanisms for the onion archival index.
 *              Specifies and verifies index-validation and -search routines.
 *
 * Return:      PASSED : 0
 *              FAILED : -1
 *
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
    /* partially-sorted list also aligned to 2 * page-size */
    H5FD_onion_index_entry_t    sorted_partial[8] = {e1, e4, e5, e7, e0, e6, e2, e3}; /* 0..3 sorted */
    H5FD_onion_index_entry_t    unsorted[8]       = {e3, e1, e4, e5, e0, e6, e2, e7};
    H5FD_onion_archival_index_t aix               = {
        H5FD__ONION_ARCHIVAL_INDEX_VERSION_CURR, 1, /* page_size_log2 */
        8,      /* list must be populated and sorted through 0 .. (count-1) */
        sorted, /* list */
    };
    const H5FD_onion_index_entry_t *entry_out_p = NULL;

    TESTING("archival index");

    /*
     * Failing validity checks
     */

    aix.version++;
    if (H5FD_onion_archival_index_is_valid(&aix))
        TEST_ERROR; /* invalid version should fail */
    aix.version = 0;
    if (H5FD_onion_archival_index_is_valid(&aix))
        TEST_ERROR; /* invalid version should fail */
    aix.version = H5FD__ONION_ARCHIVAL_INDEX_VERSION_CURR;

    aix.list = NULL;
    if (H5FD_onion_archival_index_is_valid(&aix))
        TEST_ERROR; /* list cannot be NULL */

    aix.list = sorted_incomplete;
    if (H5FD_onion_archival_index_is_valid(&aix))
        TEST_ERROR; /* list must be full */

    aix.list = unsorted;
    if (H5FD_onion_archival_index_is_valid(&aix))
        TEST_ERROR; /* list must be sorted */

    aix.list = sorted_duplicates;
    if (H5FD_onion_archival_index_is_valid(&aix))
        TEST_ERROR; /* list cannot have duplicates */

    /*
     * Passing validity checks
     */

    aix.list = sorted;
    if (!H5FD_onion_archival_index_is_valid(&aix))
        TEST_ERROR; /* everything in order should report valid */

    aix.list      = sorted_partial;
    aix.n_entries = 4;
    if (!H5FD_onion_archival_index_is_valid(&aix))
        TEST_ERROR; /* elements after n_entries are ignored */

    /*
     * Archival index search routine
     */

    aix.list      = sorted;
    aix.n_entries = 8;

    if (H5FD_onion_archival_index_find(&aix, 3, &entry_out_p) != 0)
        TEST_ERROR; /* address not in array -> returns 0 */
    if (entry_out_p != NULL)
        TEST_ERROR; /* pointer should remain unset */

    if (H5FD_onion_archival_index_find(&aix, 4, &entry_out_p) != 1)
        TEST_ERROR; /* address found -> should return 1 */
    if (NULL == entry_out_p)
        TEST_ERROR; /* pointer should be set */
    if (558 != entry_out_p->phys_addr)
        TEST_ERROR; /* incorrect address recorded */

    /*
     * Test search edge cases
     */

    aix.list      = sorted_incomplete;
    aix.n_entries = 4;

    if (H5FD_onion_archival_index_find(&aix, 1, &entry_out_p) != 0)
        TEST_ERROR; /* address not in array -> returns 0 */

    if (H5FD_onion_archival_index_find(&aix, 101, &entry_out_p) != 0)
        TEST_ERROR; /* address not in array -> returns 0 */

    /*
     * Empty archival index
     */

    entry_out_p   = NULL;
    aix.n_entries = 0; /* actually populated list is irrelevant */
    if (H5FD_onion_archival_index_find(&aix, 3, &entry_out_p) != 0)
        TEST_ERROR; /* address not in array -> returns 0 */
    if (entry_out_p != NULL)
        TEST_ERROR; /* pointer should remain unset */

    PASSED();
    return 0;

error:
    return -1;
} /* end test_archival_index() */

/*-----------------------------------------------------------------------------
 *
 * Function:    test_revision_index()
 *
 * Purpose:     TBD
 *
 * Return:      PASSED : 0
 *              FAILED : -1
 *
 *-----------------------------------------------------------------------------
 */
static int
test_revision_index(void)
{
    H5FD__onion_revision_index_t *rix_p = NULL;
    H5FD_onion_index_entry_t      entry = {
        42,     /* logi_page */
        111112, /* phys_addr */
    };
    const H5FD_onion_index_entry_t *entry_out_p = NULL;

    TESTING("revision index");

    /* Test index creation */

    if (NULL == (rix_p = H5FD_onion_revision_index_init(ONION_TEST_PAGE_SIZE_5)))
        TEST_ERROR;
    if (H5FD__ONION_REVISION_INDEX_VERSION_CURR != rix_p->version)
        TEST_ERROR;
    if (0 != rix_p->n_entries)
        TEST_ERROR;

    /* Test missed search */

    if (H5FD_onion_revision_index_find(rix_p, entry.logi_page, &entry_out_p) != 0)
        TEST_ERROR;

    /* Test successful insertion and lookup */

    if (H5FD_onion_revision_index_insert(rix_p, &entry) < 0)
        TEST_ERROR; /* insertion failed */
    if (1 != rix_p->n_entries)
        TEST_ERROR;
    if (H5FD_onion_revision_index_find(rix_p, entry.logi_page, &entry_out_p) < 0)
        TEST_ERROR; /* lookup failed */
    if (NULL == entry_out_p)
        TEST_ERROR; /* failure to set output parameter */
    if (entry.logi_page != entry_out_p->logi_page)
        TEST_ERROR;
    if (H5FD_onion_revision_index_find(rix_p, entry.logi_page + 1, &entry_out_p) != 0)
        TEST_ERROR; /* seeking other, absent page should miss */

    /* Test / demonstrate stored entry independent of user object */

    entry.logi_page = 100;
    entry.phys_addr = 101;
    if (H5FD_onion_revision_index_insert(rix_p, &entry) < 0)
        TEST_ERROR;
    if (2 != rix_p->n_entries)
        TEST_ERROR;
    entry.logi_page = 500;
    entry.phys_addr = 501;
    if (H5FD_onion_revision_index_find(rix_p, 100, &entry_out_p) < 0)
        TEST_ERROR;
    if (100 != entry_out_p->logi_page || 101 != entry_out_p->phys_addr)
        TEST_ERROR;

    /* Demonstrate updating an entry
     */

    /* Error cases */

    entry.logi_page = 100; /* phys_addr still 501, checksum bbbbbbbb */
    if (H5FD_onion_revision_index_insert(rix_p, &entry) >= 0)
        TEST_ERROR; /* all components but sum must match */
    entry.phys_addr = 101;

    /* Successful update */

    entry.logi_page = 100;
    entry.phys_addr = 101;
    if (H5FD_onion_revision_index_insert(rix_p, &entry) < 0)
        TEST_ERROR;
    if (2 != rix_p->n_entries)
        TEST_ERROR; /* Should still be two unique entries, not three */
    if (H5FD_onion_revision_index_find(rix_p, 100, &entry_out_p) < 0)
        TEST_ERROR;
    if (100 != entry_out_p->logi_page || 101 != entry_out_p->phys_addr)
        TEST_ERROR;

    if (H5FD_onion_revision_index_destroy(rix_p) < 0)
        TEST_ERROR;
    rix_p = NULL;

    PASSED();
    return 0;

error:
    if (rix_p != NULL) {
        (void)H5FD_onion_revision_index_destroy(rix_p);
    }
    return -1;
} /* end test_revision_index() */

/*-----------------------------------------------------------------------------
 *
 * Function:    test_revision_index_collisions()
 *
 * Purpose:     With knowledge of the revision index implementation, test
 *              hash key collisions.
 *
 * Return:      PASSED : 0
 *              FAILED : -1
 *
 *-----------------------------------------------------------------------------
 */
static int
test_revision_index_collisions(void)
{
    H5FD__onion_revision_index_t *rix_p = NULL;
    H5FD_onion_index_entry_t      entry = {
        0, /* logi_page */
        0, /* phys_addr */
    };
    const H5FD_onion_index_entry_t *entry_out_p       = NULL;
    uint64_t                        i                 = 0;
    const uint64_t                  n_insert          = 40;
    const uint64_t                  offset_from_power = 5;

    TESTING("revision index collisions");

    rix_p = H5FD_onion_revision_index_init(ONION_TEST_PAGE_SIZE_5);
    if (NULL == rix_p)
        TEST_ERROR; /* unable to initialize working index */

    for (i = 0; i < n_insert; i++) {
        entry.phys_addr = i;
        entry.logi_page = U64_EXP2(i) + offset_from_power;
        if (H5FD_onion_revision_index_insert(rix_p, &entry) < 0)
            TEST_ERROR;
    }

    if (n_insert != rix_p->n_entries)
        TEST_ERROR;

    for (i = 0; i < n_insert; i++) {
        uint64_t page_id = U64_EXP2(i) + offset_from_power;

        if (H5FD_onion_revision_index_find(rix_p, page_id, &entry_out_p) != 1)
            TEST_ERROR;
        if (entry_out_p->phys_addr != i)
            TEST_ERROR;
    }

    if (H5FD_onion_revision_index_destroy(rix_p) < 0)
        TEST_ERROR;
    rix_p = NULL;

    PASSED();
    return 0;

error:
    if (rix_p != NULL) {
        (void)H5FD_onion_revision_index_destroy(rix_p);
    }
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
    H5FD__onion_revision_index_t *rix_p = NULL;
    H5FD_onion_index_entry_t      entry = {
        0, /* logi_page */
        0, /* phys_addr */
    };
    const H5FD_onion_index_entry_t *entry_out_p = NULL;
    uint64_t                        i           = 0;
    const uint64_t                  n_insert = U64_EXP2((H5FD__ONION_REVISION_INDEX_STARTING_SIZE_LOG2 + 3));

    TESTING("revision index resizing");

    rix_p = H5FD_onion_revision_index_init(ONION_TEST_PAGE_SIZE_5);
    if (NULL == rix_p)
        TEST_ERROR; /* unable to initialize working index */

    for (i = 0; i < n_insert; i++) {
        entry.logi_page = i;
        entry.phys_addr = ((uint64_t)(-1) - i);
        if (H5FD_onion_revision_index_insert(rix_p, &entry) < 0)
            TEST_ERROR;
    }

    if (n_insert != rix_p->n_entries)
        TEST_ERROR;

    for (i = 0; i < n_insert; i++) {
        uint64_t page_id = i;

        if (H5FD_onion_revision_index_find(rix_p, page_id, &entry_out_p) != 1)
            TEST_ERROR;
        if (entry_out_p->phys_addr != ((uint64_t)(-1) - i))
            TEST_ERROR;
    }

    if (H5FD_onion_revision_index_destroy(rix_p) < 0)
        TEST_ERROR;
    rix_p = NULL;

    PASSED();
    return 0;

error:
    if (rix_p != NULL) {
        (void)H5FD_onion_revision_index_destroy(rix_p);
    }
    return -1;
} /* end test_revision_index_resizing() */

/*-----------------------------------------------------------------------------
 *
 * Function:    test_revision_index_to_archival_index()
 *
 * Purpose:     Verify to_archival_index().
 *
 * Return:      PASSED : 0
 *              FAILED : -1
 *
 *-----------------------------------------------------------------------------
 */
static int
test_revision_index_to_archival_index(void)
{
    H5FD__onion_revision_index_t *rix_p     = NULL;
    H5FD_onion_index_entry_t      rix_entry = {
        0, /* logi_page */
        0, /* phys_addr */
    };
    H5FD_onion_archival_index_t aix = {
        H5FD__ONION_ARCHIVAL_INDEX_VERSION_CURR,
        5, /* page_size_log2 */
        0, /* n_entries to be set */
        NULL,
    };
    const uint64_t n_insert = 10;
    uint64_t       i        = 0;

    TESTING("revision index to archival index");

    /*
     * SETUP
     */

    if (NULL == (rix_p = H5FD_onion_revision_index_init(ONION_TEST_PAGE_SIZE_5)))
        TEST_ERROR; /* Unable to initialize working index */

    /* Add scattered entries in reverse order. */
    for (i = 0; i < n_insert; i++) {
        uint64_t n = 2003 * (n_insert - i) + 47;

        rix_entry.logi_page = n;
        rix_entry.phys_addr = n * 13;
        if (H5FD_onion_revision_index_insert(rix_p, &rix_entry) < 0)
            TEST_ERROR;
    }

    if (n_insert != rix_p->n_entries)
        TEST_ERROR;

    aix.list      = NULL;
    aix.n_entries = 0;

    /* Successful merge into empty archival index */

    if (H5FD_onion_merge_revision_index_into_archival_index(rix_p, &aix) < 0)
        TEST_ERROR;

    if (!H5FD_onion_archival_index_is_valid(&aix))
        TEST_ERROR; /* Entries not sorted, or other obscure issue */

    if (n_insert != aix.n_entries)
        TEST_ERROR; /* Failed to resize and/or update archival index info */

    for (i = 0; i < n_insert; i++) {
        const H5FD_onion_index_entry_t *aix_entry_p = NULL;
        uint64_t                        n           = 2003 * (i + 1) + 47;

        aix_entry_p = &aix.list[i];

        if (aix_entry_p->logi_page != n)
            TEST_ERROR;
        if (aix_entry_p->phys_addr != (n * 13))
            TEST_ERROR;
    }

    /* Successful merge into populated archival index */

    H5MM_xfree(aix.list);
    aix.list = NULL;
    if (NULL == (aix.list = H5MM_malloc(sizeof(H5FD_onion_index_entry_t) * 2)))
        TEST_ERROR;
    aix.list[0].logi_page = 47;
    aix.list[0].phys_addr = 47 * 13;
    aix.list[1].logi_page = (2003 * (n_insert + 1) + 47);
    aix.list[1].phys_addr = (2003 * (n_insert + 1) + 47) * 13;
    aix.n_entries         = 2;

    if (!H5FD_onion_archival_index_is_valid(&aix))
        TEST_ERROR; /* entries not sorted, or other obscure issue */

    if (H5FD_onion_merge_revision_index_into_archival_index(rix_p, &aix) < 0)
        TEST_ERROR;

    if (!H5FD_onion_archival_index_is_valid(&aix))
        TEST_ERROR; /* entries not sorted, or other obscure issue */

    if (n_insert + 2 != aix.n_entries)
        TEST_ERROR;

    for (i = 0; i < (n_insert + 2); i++) {
        const H5FD_onion_index_entry_t *aix_entry_p = NULL;
        uint64_t                        n           = 2003 * i + 47;

        aix_entry_p = &aix.list[i];

        if (aix_entry_p->logi_page != n)
            TEST_ERROR;
        if (aix_entry_p->phys_addr != (n * 13))
            TEST_ERROR;
    }

    /* Merged enties from revision index replace existing entries */

    H5MM_xfree(aix.list);
    aix.list = NULL;
    if (NULL == (aix.list = H5MM_malloc(sizeof(H5FD_onion_index_entry_t) * 2)))
        TEST_ERROR;
    aix.list[0].logi_page = 2003 * (n_insert / 2) + 47;
    aix.list[0].phys_addr = 103;
    aix.list[1].logi_page = 2003 * (n_insert / 2 + 1) + 47;
    aix.list[1].phys_addr = 101;
    aix.n_entries         = 2;

    if (!H5FD_onion_archival_index_is_valid(&aix))
        TEST_ERROR; /* entries not sorted, or other obscure issue */

    if (H5FD_onion_merge_revision_index_into_archival_index(rix_p, &aix) < 0)
        TEST_ERROR;

    if (!H5FD_onion_archival_index_is_valid(&aix))
        TEST_ERROR; /* entries not sorted, or other obscure issue */

    if (n_insert != aix.n_entries)
        TEST_ERROR;

    for (i = 0; i < n_insert; i++) {
        const H5FD_onion_index_entry_t *aix_entry_p = NULL;
        uint64_t                        n           = 2003 * (i + 1) + 47;

        aix_entry_p = &aix.list[i];

        if (aix_entry_p->logi_page != n)
            TEST_ERROR;
        if (aix_entry_p->phys_addr != (n * 13))
            TEST_ERROR;
    }

    /*
     * CLEANUP
     */

    if (H5FD_onion_revision_index_destroy(rix_p) < 0)
        TEST_ERROR;

    H5MM_xfree(aix.list);

    PASSED();
    return 0;

error:
    if (rix_p)
        (void)H5FD_onion_revision_index_destroy(rix_p);
    if (aix.list)
        H5MM_xfree(aix.list);
    return -1;
} /* end test_revision_index_to_archival_index() */

/*-----------------------------------------------------------------------------
 *
 * Function:    test_fapl()
 *
 * Purpose:     Verify H5Pget and set behavior, and data-consistency checks.
 *
 * Return:      PASSED : 0
 *              FAILED : -1
 *
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

    dxpl_id = H5Pcreate(H5P_DATASET_XFER);
    if (H5I_INVALID_HID == dxpl_id)
        TEST_ERROR;

    fapl_id_sec2 = H5Pcreate(H5P_FILE_ACCESS);
    if (H5I_INVALID_HID == fapl_id_sec2)
        TEST_ERROR;
    if (H5Pset_fapl_sec2(fapl_id_sec2))
        TEST_ERROR;

    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    if (H5I_INVALID_HID == fapl_id)
        TEST_ERROR;

    /*
     * Set FAPL
     */

    H5E_BEGIN_TRY
    {
        ret = H5Pset_fapl_onion(H5I_INVALID_HID, &info_in);
    }
    H5E_END_TRY;
    if (SUCCEED == ret)
        TEST_ERROR; /* fapl_id must be valid */

    H5E_BEGIN_TRY
    {
        ret = H5Pset_fapl_onion(fapl_id, NULL);
    }
    H5E_END_TRY;
    if (SUCCEED == ret)
        TEST_ERROR; /* info pointer cannot be NULL */

    info_in.version++;
    H5E_BEGIN_TRY
    {
        ret = H5Pset_fapl_onion(fapl_id, &info_in);
    }
    H5E_END_TRY;
    if (SUCCEED == ret)
        TEST_ERROR; /* info version must be valid */
    info_in.version--;

    info_in.page_size = 7;
    H5E_BEGIN_TRY
    {
        ret = H5Pset_fapl_onion(fapl_id, &info_in);
    }
    H5E_END_TRY;
    if (SUCCEED == ret)
        TEST_ERROR; /* page size must be valid power of 2 */

    info_in.page_size = 0;
    H5E_BEGIN_TRY
    {
        ret = H5Pset_fapl_onion(fapl_id, &info_in);
    }
    H5E_END_TRY;
    if (SUCCEED == ret)
        TEST_ERROR; /* page size must be greater than zero */
    info_in.page_size = ONION_TEST_PAGE_SIZE_1;

    info_in.backing_fapl_id = H5I_INVALID_HID;
    H5E_BEGIN_TRY
    {
        ret = H5Pset_fapl_onion(fapl_id, &info_in);
    }
    H5E_END_TRY;
    if (SUCCEED == ret)
        TEST_ERROR; /* backing fapl ID cannot be invalid */

    info_in.backing_fapl_id = dxpl_id;
    H5E_BEGIN_TRY
    {
        ret = H5Pset_fapl_onion(fapl_id, &info_in);
    }
    H5E_END_TRY;
    if (SUCCEED == ret)
        TEST_ERROR; /* backing fapl ID must be file access proptery list ID */
    info_in.backing_fapl_id = H5P_DEFAULT;

    if (H5Pset_fapl_onion(fapl_id, &info_in) < 0)
        TEST_ERROR;

    /*
     * Get onion fapl info
     */

    H5E_BEGIN_TRY
    {
        ret = H5Pget_fapl_onion(fapl_id, NULL);
    }
    H5E_END_TRY;
    if (SUCCEED == ret)
        TEST_ERROR; /* info_out pointer cannot be NULL */

    H5E_BEGIN_TRY
    {
        ret = H5Pget_fapl_onion(H5I_INVALID_HID, &info_out);
    }
    H5E_END_TRY;
    if (SUCCEED == ret)
        TEST_ERROR; /* fapl_id must be valid */

    H5E_BEGIN_TRY
    {
        ret = H5Pget_fapl_onion(fapl_id_sec2, &info_out);
    }
    H5E_END_TRY;
    if (SUCCEED == ret)
        TEST_ERROR; /* wrong fapl_id in */

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
    if (H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST != info_out.revision_id)
        TEST_ERROR;
    if (0 != info_out.creation_flags)
        TEST_ERROR;
    if (0 != info_out.force_write_open)
        TEST_ERROR;
    if (HDstrcmp(info_in.comment, info_out.comment))
        TEST_ERROR;

    /*
     * Cleanup
     */

    if (H5Pclose(dxpl_id) < 0)
        TEST_ERROR;
    dxpl_id = H5I_INVALID_HID;

    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    fapl_id = H5I_INVALID_HID;

    if (H5Pclose(fapl_id_sec2) < 0)
        TEST_ERROR;
    fapl_id_sec2 = H5I_INVALID_HID;

    PASSED();
    return 0;

error:
    if (H5I_INVALID_HID != dxpl_id)
        (void)H5Pclose(dxpl_id);
    if (H5I_INVALID_HID != fapl_id)
        (void)H5Pclose(fapl_id);
    if (H5I_INVALID_HID != fapl_id_sec2)
        (void)H5Pclose(fapl_id_sec2);

    return -1;
} /* end test_fapl() */

/*-----------------------------------------------------------------------------
 *
 * Function:    test_header_encode_decode()
 *
 * Purpose:     Verify onion header encoding and decoding behavior.
 *
 * Return:      PASSED : 0
 *              FAILED : -1
 *
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
        0x40, 0xe2, 0x01, 0,   0,    0,    0, 0,                /* whole_history_addr */
        88,   0,    0,    0,   0,    0,    0, 0,                /* whole_history_size */
        0,    0,    0,    0                                     /* sum populated below */
    };
    unsigned char *             ptr      = NULL;
    uint32_t                    sum      = 0;
    uint32_t                    sum_out  = 0;
    size_t                      i        = 0;
    uint64_t                    size_ret = 0;
    H5FD_onion_history_header_t hdr;
    H5FD_onion_history_header_t hdr_out;

    TESTING("encode/decode history header");

    sum = H5_checksum_fletcher32(exp, H5FD__ONION_ENCODED_SIZE_HEADER - 4);
    ptr = exp + H5FD__ONION_ENCODED_SIZE_HEADER - 4;
    UINT32ENCODE(ptr, sum);

    hdr.version    = H5FD__ONION_HEADER_VERSION_CURR;
    hdr.flags      = 12;
    hdr.origin_eof = 8589934609ull, hdr.page_size = 4096;
    hdr.whole_history_addr = 123456;
    hdr.whole_history_size = 88;

    if (H5FD_onion_history_header_encode(&hdr, buf, &sum_out) != H5FD__ONION_ENCODED_SIZE_HEADER)
        TEST_ERROR;

    if (sum != sum_out)
        TEST_ERROR;

    for (i = 0; i < H5FD__ONION_ENCODED_SIZE_HEADER; i++) {
        if (exp[i] != buf[i]) {
            HDprintf("first mismatched byte at %zu: %02x %02x\n", i, exp[i], buf[i]);
            TEST_ERROR;
        }
    }

    hdr_out.version            = H5FD__ONION_HEADER_VERSION_CURR;
    hdr_out.flags              = 0;
    hdr_out.page_size          = 0;
    hdr_out.whole_history_addr = 0;
    hdr_out.whole_history_size = 0;

    /* Invalid header signature prevents decoding.
     */

    exp[3] = 'X'; /* invalidate encoded signature */
    H5E_BEGIN_TRY
    {
        size_ret = H5FD_onion_history_header_decode(exp, &hdr_out);
    }
    H5E_END_TRY;
    if (0 != size_ret)
        TEST_ERROR;

    exp[3] = 'H'; /* reset */

    /* Invalid header version prevents decoding.
     */

    exp[4] = 0; /* encoded version 0?!? */
    H5E_BEGIN_TRY
    {
        size_ret = H5FD_onion_history_header_decode(exp, &hdr_out);
    }
    H5E_END_TRY;
    if (0 != size_ret)
        TEST_ERROR;

    exp[4] = H5FD__ONION_HEADER_VERSION_CURR + 1; /* encoded super-version?! */
    H5E_BEGIN_TRY
    {
        size_ret = H5FD_onion_history_header_decode(exp, &hdr_out);
    }
    H5E_END_TRY;
    if (0 != size_ret)
        TEST_ERROR;

    exp[4] = H5FD__ONION_HEADER_VERSION_CURR; /* reset */

    /* Valid header can be decoded.
     */

    if (H5FD_onion_history_header_decode(buf, &hdr_out) != H5FD__ONION_ENCODED_SIZE_HEADER)
        TEST_ERROR;
    if (H5FD__ONION_HEADER_VERSION_CURR != hdr_out.version)
        TEST_ERROR;
    if (hdr.flags != hdr_out.flags)
        TEST_ERROR;
    if (hdr.page_size != hdr_out.page_size)
        TEST_ERROR;
    if (hdr.whole_history_addr != hdr_out.whole_history_addr)
        TEST_ERROR;
    if (hdr.whole_history_size != hdr_out.whole_history_size)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    return -1;
} /* end test_header_encode_decode() */

/*-----------------------------------------------------------------------------
 *
 * Function:    test_whole_history_encode_decode_empty()
 *
 * Purpose:     Verify onion whole-history encoding and decoding behavior.
 *              Tests the case of the "empty" whole-history.
 *              Verifies behavior in standard error cases.
 *
 * Return:      PASSED : 0
 *              FAILED : -1
 *
 *-----------------------------------------------------------------------------
 */
static int
test_whole_history_encode_decode_empty(void)
{
    unsigned char buf[32];
    unsigned char exp[32] = {
        'O', 'W', 'H', 'S',                        /* NOTE: match signature define in onion_priv.h */
        1,   0,   0,   0,                          /* NOTE: update version w/ "current" as needed */
        0,   0,   0,   0,   0, 0, 0, 0, 0, 0, 0, 0 /* sum populated below */
    };
    unsigned char *            ptr      = NULL;
    uint32_t                   sum      = 0;
    uint32_t                   sum_out  = 0;
    size_t                     i        = 0;
    uint64_t                   size_ret = 0;
    H5FD_onion_whole_history_t whs      = {
        H5FD__ONION_WHOLE_HISTORY_VERSION_CURR, 0, /* n_revisions */
        NULL,                                      /* list */
        0,                                         /* checksum */
    };
    H5FD_onion_whole_history_t whs_out = {
        H5FD__ONION_WHOLE_HISTORY_VERSION_CURR, 0, /* n_revisions */
        NULL,                                      /* list */
        0,                                         /* checksum */
    };

    TESTING("encode/decode whole-history (empty and failures)");

    /* Generage checksum but don't store it yet */
    sum = H5_checksum_fletcher32(exp, H5FD__ONION_ENCODED_SIZE_WHOLE_HISTORY - 4);
    ptr = exp + H5FD__ONION_ENCODED_SIZE_WHOLE_HISTORY - 4;
    UINT32ENCODE(ptr, sum);

    if (H5FD_onion_whole_history_encode(&whs, buf, &sum_out) != H5FD__ONION_ENCODED_SIZE_WHOLE_HISTORY)
        TEST_ERROR;
    for (i = 0; i < 20; i++) {
        if (exp[i] != buf[i]) {
            HDprintf("first mismatched byte at %zu: %02x %02x\n", i, exp[i], buf[i]);
            TEST_ERROR;
        }
    }
    if (sum != sum_out)
        TEST_ERROR;
    whs.checksum = sum; /* set to compare later */

    /* Invalid signature prevents decoding.
     */

    exp[3] = 'X'; /* invalidate encoded signature */
    H5E_BEGIN_TRY
    {
        size_ret = H5FD_onion_whole_history_decode(exp, &whs_out);
    }
    H5E_END_TRY;
    if (0 != size_ret)
        TEST_ERROR;

    exp[3] = 'H'; /* reset */

    /* Invalid version prevents decoding.
     */

    exp[4] = 0; /* encoded version 0?!? */
    H5E_BEGIN_TRY
    {
        size_ret = H5FD_onion_whole_history_decode(exp, &whs_out);
    }
    H5E_END_TRY;
    if (0 != size_ret)
        TEST_ERROR;

    exp[4] = H5FD__ONION_WHOLE_HISTORY_VERSION_CURR + 1;
    H5E_BEGIN_TRY
    {
        size_ret = H5FD_onion_whole_history_decode(exp, &whs_out);
    }
    H5E_END_TRY;
    if (0 != size_ret)
        TEST_ERROR;

    exp[4] = H5FD__ONION_WHOLE_HISTORY_VERSION_CURR; /* reset */

    /* Valid summary can be decoded.
     */

    if (H5FD_onion_whole_history_decode(buf, &whs_out) != H5FD__ONION_ENCODED_SIZE_WHOLE_HISTORY)
        TEST_ERROR;
    if (H5FD__ONION_WHOLE_HISTORY_VERSION_CURR != whs_out.version)
        TEST_ERROR;
    if (whs.n_revisions != whs_out.n_revisions)
        TEST_ERROR;
    if (whs.checksum != whs_out.checksum)
        TEST_ERROR;
    if (NULL != whs_out.record_pointer_list)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    return -1;
} /* end test_whole_history_encode_decode_empty() */

/*-----------------------------------------------------------------------------
 *
 * Function:    test_whole_history_encode_decode()
 *
 * Purpose:     Verify onion whole-history encoding and decoding behavior.
 *              Encode/decode with some set of revision record pointers.
 *
 * Return:      PASSED : 0
 *              FAILED : -1
 *
 *-----------------------------------------------------------------------------
 */
static int
test_whole_history_encode_decode(void)
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
    unsigned char *            buf_p   = NULL;
    uint32_t                   sum_out = 0;
    size_t                     i       = 0;
    H5FD_onion_whole_history_t whs     = {
        H5FD__ONION_WHOLE_HISTORY_VERSION_CURR, 3, /* n_revisions */
        NULL,                                      /* list set below */
        0,                                         /* checksum  not set by us */
    };
    H5FD_onion_whole_history_t whs_out = {
        H5FD__ONION_WHOLE_HISTORY_VERSION_CURR, 0, /* n_revisions must start as zero */
        NULL,                                      /* list */
        0,                                         /* checksum */
    };
    uint64_t exp_size =
        H5FD__ONION_ENCODED_SIZE_WHOLE_HISTORY + H5FD__ONION_ENCODED_SIZE_RECORD_POINTER * whs.n_revisions;

    TESTING("encode/decode whole-history");

    if (80 != exp_size)
        TEST_ERROR;

    whs.record_pointer_list =
        (H5FD_onion_record_pointer_t *)HDcalloc(whs.n_revisions, sizeof(H5FD_onion_record_pointer_t));
    if (NULL == whs.record_pointer_list)
        TEST_ERROR;

    /* must match values in exp */
    whs.record_pointer_list[0].phys_addr   = 568ull;
    whs.record_pointer_list[0].record_size = 238ull;
    whs.record_pointer_list[1].phys_addr   = 241017ull;
    whs.record_pointer_list[1].record_size = 4555ull;
    whs.record_pointer_list[2].phys_addr   = 918153371232ull;
    whs.record_pointer_list[2].record_size = 240ull;

    /* populate revision pointer sums in exp */
    for (i = 0; i < whs.n_revisions; i++) {
        uint64_t whs_pre  = H5FD__ONION_ENCODED_SIZE_WHOLE_HISTORY - 4;
        uint64_t ptr_pre  = H5FD__ONION_ENCODED_SIZE_RECORD_POINTER - 4;
        uint64_t ptr_size = H5FD__ONION_ENCODED_SIZE_RECORD_POINTER;

        buf_p                               = exp + whs_pre + ptr_size * i;
        whs.record_pointer_list[i].checksum = H5_checksum_fletcher32(buf_p, ptr_pre);
        buf_p += ptr_pre;
        UINT32ENCODE(buf_p, whs.record_pointer_list[i].checksum);
    }

    /* compute, populate, and store exp final sum */
    whs.checksum = H5_checksum_fletcher32(exp, exp_size - 4);
    buf_p        = exp + exp_size - 4;
    UINT32ENCODE(buf_p, whs.checksum);

    buf = (unsigned char *)HDmalloc(exp_size);
    if (NULL == buf)
        TEST_ERROR;

    if (H5FD_onion_whole_history_encode(&whs, buf, &sum_out) != exp_size)
        TEST_ERROR;
    for (i = 0; i < exp_size; i++) {
        if (exp[i] != buf[i])
            TEST_ERROR;
    }
    if (whs.checksum != sum_out)
        TEST_ERROR;

    /* Initial decode, gets always-present components.
     */

    whs_out.n_revisions = 0; /* must be initialized to 0 */
    if (H5FD_onion_whole_history_decode(exp, &whs_out) != exp_size)
        TEST_ERROR;
    if (H5FD__ONION_WHOLE_HISTORY_VERSION_CURR != whs_out.version)
        TEST_ERROR;
    if (whs.n_revisions != whs_out.n_revisions)
        TEST_ERROR;
    if (NULL != whs_out.record_pointer_list)
        TEST_ERROR; /* Must be created by us */

    /* True decode requires allocating space for record pointers
     */

    whs_out.record_pointer_list =
        (H5FD_onion_record_pointer_t *)HDcalloc(whs_out.n_revisions, sizeof(H5FD_onion_record_pointer_t));
    if (NULL == whs_out.record_pointer_list)
        TEST_ERROR;

    if (H5FD_onion_whole_history_decode(exp, &whs_out) != exp_size)
        TEST_ERROR;
    if (H5FD__ONION_WHOLE_HISTORY_VERSION_CURR != whs_out.version)
        TEST_ERROR;
    if (whs.n_revisions != whs_out.n_revisions)
        TEST_ERROR;
    if (whs.checksum != whs_out.checksum)
        TEST_ERROR;
    if (NULL == whs_out.record_pointer_list)
        TEST_ERROR;
    for (i = 0; i < whs.n_revisions; i++) {
        H5FD_onion_record_pointer_t exp_rp = whs.record_pointer_list[i];
        H5FD_onion_record_pointer_t act_rp = whs_out.record_pointer_list[i];

        if (exp_rp.phys_addr != act_rp.phys_addr)
            TEST_ERROR;
        if (exp_rp.record_size != act_rp.record_size)
            TEST_ERROR;
        if (exp_rp.checksum != act_rp.checksum)
            TEST_ERROR;
    }

    HDfree(whs_out.record_pointer_list);
    whs_out.record_pointer_list = NULL;

    HDfree(buf);
    buf = NULL;

    HDfree(whs.record_pointer_list);
    whs.record_pointer_list = NULL;

    PASSED();
    return 0;

error:
    if (whs_out.record_pointer_list != NULL)
        HDfree(whs_out.record_pointer_list);
    if (buf != NULL)
        HDfree(buf);
    if (whs.record_pointer_list != NULL)
        HDfree(whs.record_pointer_list);
    return -1;
} /* end test_whole_history_encode_decode() */

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
    unsigned char *buf      = NULL;
    unsigned char  exp[189] = {
        'O', 'R', 'R', 'S',                     /* signature                 */
        1, 0, 0, 0,                             /* NOTE: update version w/ "current" as needed */
        5, 0, 0, 0, 0, 0, 0, 0,                 /* revision ID               */
        2, 0, 0, 0, 0, 0, 0, 0,                 /* parent revision ID        */
        '1', '9', '4', '1', '1', '2', '0', '7', /* Time of Creation          */
        'T', '1', '9', '0', '6', '4', '3', 'Z', /* ...                       */
        0x11, 0x00, 0, 0, 0x02, 0, 0, 0,        /* logical file size         */
        0, 16, 0, 0,                            /* page size                 */
        143, 25, 0, 0,                          /* user ID                   */
        4, 0, 0, 0, 0, 0, 0, 0,                 /* n_entries                 */
        8, 0, 0, 0,                             /* username size             */
        25, 0, 0, 0,                            /* comment size              */
        /* entry0 pointer */
        0, 0xB0, 0x1E, 0, 0, 0, 0, 0,         /* logical offset            */
        0x4B, 0x02, 0, 0, 0, 0, 0, 0,         /* physical address          */
        0, 0, 0, 0, /* sum populated below */ /* checksum                  */
        /* entry1 pointer */
        0, 0xF0, 0x2E, 0, 0, 0, 0, 0, 0xA7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* sum populated below */
        /* entry2 pointer */
        0, 0x50, 0x15, 0, 0, 0x20, 0, 0, 0x11, 0, 0, 0, 0x02, 0, 0, 0, 0, 0, 0, 0, /* sum populated below */
        /* entry3 pointer */
        0, 0xE0, 0x24, 0, 0, 0, 0, 0, 0xB1, 0x01, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* sum populated below */
        'J', 'o', 'h', 'n', 'D', 'o', 'e', '\0', /* username                  */
        'E', 'x', 'a', 'm', 'p', 'l', 'e', ' ',  /* comment                   */
        'c', 'o', 'm', 'm', 'e', 'n', 't', ' ',  /* ...                       */
        'm', 'e', 's', 's', 'a', 'g', 'e', '.',  /* ...                       */
        '\0',                                    /* ...                       */
        /* final checksum */
        0, 0, 0, 0 /* sum populated below */ /* checksum                  */
    };
    unsigned char *              buf_p = NULL;
    size_t                       i     = 0;
    uint64_t                     size_ret;
    H5FD_onion_revision_record_t r_out;
    uint32_t                     sum_out     = 0;
    char                         username[8] = "JohnDoe";
    char                         comment[25] = "Example comment message.";
    H5FD_onion_revision_record_t record      = {
        H5FD__ONION_REVISION_RECORD_VERSION_CURR,
        5,             /* revision ID */
        2,             /* parent revision ID */
        {'\0'},        /* time of creation - populated below */
        8589934609ull, /* logical file size */
        6543,          /* user ID */
        8,             /* username size */
        25,            /* comment size */
        {
            H5FD__ONION_ARCHIVAL_INDEX_VERSION_CURR, 12, /* page_size_log2 */
            4,                                           /* n_entries */
            NULL,                                        /* list - populated below */
        },                                               /* archival index struct */
        username,                                        /* username */
        comment,                                         /* comment */
        0,                                               /* checksum - computed for us */
    };
    uint64_t exp_size = H5FD__ONION_ENCODED_SIZE_REVISION_RECORD +
                        (H5FD__ONION_ENCODED_SIZE_INDEX_ENTRY * record.archival_index.n_entries) +
                        strlen("JohnDoe") + 1 + strlen("Example comment message.") + 1;

    r_out.archival_index.list = NULL;
    r_out.comment             = NULL;
    r_out.username            = NULL;

    TESTING("encode/decode revision record");

    if (189 != exp_size)
        TEST_ERROR;

    HDmemcpy(record.time_of_creation, "19411207T190643Z", 16);
    record.archival_index.list = (H5FD_onion_index_entry_t *)HDcalloc(record.archival_index.n_entries,
                                                                      sizeof(H5FD_onion_index_entry_t));
    if (NULL == record.archival_index.list)
        TEST_ERROR;
    /* convert logi_page and should match address in expected buffer */
    record.archival_index.list[0].logi_page = 491ull;
    record.archival_index.list[0].phys_addr = 587ull;
    record.archival_index.list[1].logi_page = 751ull;
    record.archival_index.list[1].phys_addr = 167ull;
    record.archival_index.list[2].logi_page = 8589934933ull;
    record.archival_index.list[2].phys_addr = 8589934609ull;
    record.archival_index.list[3].logi_page = 590ull;
    record.archival_index.list[3].phys_addr = 433ull;

    /* Set expected checksum for each archival index entry in buffer */
    for (i = 0; i < record.archival_index.n_entries; i++) {
        uint64_t rec_pre  = H5FD__ONION_ENCODED_SIZE_REVISION_RECORD - 4;
        uint64_t idx_pre  = H5FD__ONION_ENCODED_SIZE_INDEX_ENTRY - 4;
        uint64_t idx_size = H5FD__ONION_ENCODED_SIZE_INDEX_ENTRY;

        buf_p   = exp + rec_pre + idx_size * i;
        sum_out = H5_checksum_fletcher32(buf_p, idx_pre);
        buf_p += idx_pre;
        UINT32ENCODE(buf_p, sum_out);
    }

    sum_out = 0;

    record.checksum = H5_checksum_fletcher32(exp, exp_size - 4);
    buf_p           = exp + exp_size - 4;
    UINT32ENCODE(buf_p, record.checksum);

    /* required initialization for record-out structure */
    r_out.version                  = H5FD__ONION_REVISION_RECORD_VERSION_CURR;
    r_out.username_size            = 0;
    r_out.comment_size             = 0;
    r_out.username                 = NULL;
    r_out.comment                  = NULL;
    r_out.archival_index.version   = H5FD__ONION_ARCHIVAL_INDEX_VERSION_CURR;
    r_out.archival_index.n_entries = 0;
    r_out.archival_index.list      = NULL;

    buf = (unsigned char *)HDmalloc(sizeof(unsigned char) * exp_size);
    if (NULL == buf)
        TEST_ERROR;

    /* Test encode
     */

    if (H5FD_onion_revision_record_encode(&record, buf, &sum_out) != exp_size)
        TEST_ERROR;
    for (i = 0; i < exp_size; i++) {
        if (exp[i] != buf[i])
            TEST_ERROR;
    }
    if (record.checksum != sum_out)
        TEST_ERROR;

    HDfree(buf);
    buf = NULL;

    /* Test decode (malformed encoding)
     */

    /* invalid signature */
    exp[2] = 'Y';
    H5E_BEGIN_TRY
    {
        size_ret = H5FD_onion_revision_record_decode(exp, &r_out);
    }
    H5E_END_TRY;
    if (0 != size_ret)
        TEST_ERROR;
    exp[2] = 'R'; /* reset */

    /* zero version */
    exp[4] = 0;
    H5E_BEGIN_TRY
    {
        size_ret = H5FD_onion_revision_record_decode(exp, &r_out);
    }
    H5E_END_TRY;
    if (0 != size_ret)
        TEST_ERROR;

    /* advance version */
    exp[4] = H5FD__ONION_REVISION_RECORD_VERSION_CURR + 1;
    H5E_BEGIN_TRY
    {
        size_ret = H5FD_onion_revision_record_decode(exp, &r_out);
    }
    H5E_END_TRY;
    if (0 != size_ret)
        TEST_ERROR;
    exp[4] = H5FD__ONION_REVISION_RECORD_VERSION_CURR; /* reset */

    /* Test successful decode
     */

    /* Initial decode; get variable-length component sizes */
    if (H5FD_onion_revision_record_decode(exp, &r_out) != exp_size)
        TEST_ERROR;
    if (record.username_size != r_out.username_size)
        TEST_ERROR;
    if (record.comment_size != r_out.comment_size)
        TEST_ERROR;
    if (record.archival_index.n_entries != r_out.archival_index.n_entries)
        TEST_ERROR;

    /* Allocate variable-length components */
    r_out.username = (char *)HDcalloc(r_out.username_size, sizeof(char));
    if (NULL == r_out.username)
        TEST_ERROR;
    r_out.comment = (char *)HDcalloc(r_out.comment_size, sizeof(char));
    if (NULL == r_out.comment)
        TEST_ERROR;
    r_out.archival_index.list = (H5FD_onion_index_entry_t *)HDcalloc(r_out.archival_index.n_entries,
                                                                     sizeof(H5FD_onion_index_entry_t));
    if (NULL == r_out.archival_index.list)
        TEST_ERROR;

    /* Decode into all components */
    if (H5FD_onion_revision_record_decode(exp, &r_out) != exp_size)
        TEST_ERROR;
    if (H5FD__ONION_REVISION_RECORD_VERSION_CURR != r_out.version)
        TEST_ERROR;
    if (record.user_id != r_out.user_id)
        TEST_ERROR;
    if (record.revision_id != r_out.revision_id)
        TEST_ERROR;
    if (record.parent_revision_id != r_out.parent_revision_id)
        TEST_ERROR;
    if (record.parent_revision_id != r_out.parent_revision_id)
        TEST_ERROR;
    if (record.checksum != r_out.checksum)
        TEST_ERROR;
    if (HDstrncmp(record.time_of_creation, r_out.time_of_creation, 16) != 0)
        TEST_ERROR;
    if (record.username_size != r_out.username_size)
        TEST_ERROR;
    if (record.username_size != HDstrlen(r_out.username) + 1)
        TEST_ERROR;
    if (HDstrlen(record.username) != HDstrlen(r_out.username))
        TEST_ERROR;
    if (HDstrcmp(record.username, r_out.username) != 0)
        TEST_ERROR;
    if (record.comment_size != r_out.comment_size)
        TEST_ERROR;
    if (record.comment_size != HDstrlen(r_out.comment) + 1)
        TEST_ERROR;
    if (HDstrlen(record.comment) != HDstrlen(r_out.comment))
        TEST_ERROR;
    if (HDstrcmp(record.comment, r_out.comment) != 0)
        TEST_ERROR;

    if (H5FD__ONION_ARCHIVAL_INDEX_VERSION_CURR != r_out.archival_index.version)
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
        if (ep->logi_page != ap->logi_page)
            TEST_ERROR;
    }

    /* Cleanup
     */

    HDfree(r_out.archival_index.list);
    r_out.archival_index.list = NULL;

    HDfree(r_out.comment);
    r_out.comment = NULL;

    HDfree(r_out.username);
    r_out.username = NULL;

    HDfree(record.archival_index.list);
    record.archival_index.list = NULL;

    PASSED();
    return 0;

error:
    if (r_out.archival_index.list != NULL)
        HDfree(r_out.archival_index.list);
    if (r_out.comment != NULL)
        HDfree(r_out.comment);
    if (r_out.username != NULL)
        HDfree(r_out.username);
    if (buf != NULL)
        HDfree(buf);
    if (record.archival_index.list != NULL)
        HDfree(record.archival_index.list);
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
    H5FD_t *       raw_vfile = NULL; /* virtual file to look at raw file contents */
    unsigned char *act_buf   = NULL; /* allocated area for actual file bytes */
    size_t         i         = 0;
    uint64_t       filesize  = 0;

    raw_vfile = H5FDopen(filepath, H5F_ACC_RDONLY, fapl_id, HADDR_UNDEF);
    if (NULL == raw_vfile)
        TEST_ERROR;

    filesize = (uint64_t)H5FDget_eof(raw_vfile, H5FD_MEM_DRAW);
    if ((uint64_t)nbytes != filesize)
        TEST_ERROR;

    act_buf = (unsigned char *)HDmalloc(nbytes);
    if (NULL == act_buf)
        TEST_ERROR;
    for (i = 0; i < nbytes; i++)
        act_buf[i] = (unsigned char)(-1); /* fill with bogus all-1s */
    if (H5FDset_eoa(raw_vfile, H5FD_MEM_DRAW, nbytes) < 0)
        TEST_ERROR;
    if (H5FDread(raw_vfile, H5FD_MEM_DRAW, H5P_DEFAULT, 0, nbytes, act_buf) < 0)
        TEST_ERROR;

    /* compare raw bytes data */
    for (i = 0; i < nbytes; i++) {
        if (exp[i] != act_buf[i]) {
            HDprintf("first mismatched byte %zu: expected 0x%02X was 0x%02X\n", i, exp[i], act_buf[i]);
            TEST_ERROR;
        }
    }

    if (H5FDclose(raw_vfile) < 0)
        TEST_ERROR;
    raw_vfile = NULL;

    HDfree(act_buf);
    act_buf = NULL;

    return 0;

error:
    if (act_buf != NULL)
        HDfree(act_buf);
    if (raw_vfile != NULL)
        H5FDclose(raw_vfile);
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
    unsigned char *              buf = NULL; /* allocated area for actual file bytes */
    H5FD_onion_history_header_t  hdr_out;
    H5FD_onion_whole_history_t   whs_out;
    H5FD_onion_revision_record_t rev_out;
    uint64_t                     filesize = 0;
    uint64_t                     readsize = 0;
    size_t                       i        = 0;

    hdr_out.version = H5FD__ONION_HEADER_VERSION_CURR;

    whs_out.version             = H5FD__ONION_WHOLE_HISTORY_VERSION_CURR;
    whs_out.n_revisions         = 0;
    whs_out.record_pointer_list = NULL;

    rev_out.version                = H5FD__ONION_REVISION_RECORD_VERSION_CURR;
    rev_out.archival_index.version = H5FD__ONION_ARCHIVAL_INDEX_VERSION_CURR;

    filesize = (uint64_t)H5FDget_eof(raw_file, H5FD_MEM_DRAW);
    if (H5FDset_eoa(raw_file, H5FD_MEM_DRAW, filesize) < 0)
        TEST_ERROR;

    /* Injest onion header.
     */

    readsize = MIN(filesize, H5FD__ONION_ENCODED_SIZE_HEADER);
    buf      = (unsigned char *)HDmalloc(sizeof(unsigned char) * readsize);
    if (NULL == buf)
        TEST_ERROR;
    if (H5FDread(raw_file, H5FD_MEM_DRAW, H5P_DEFAULT, 0, readsize, buf) < 0)
        TEST_ERROR;

    readsize = H5FD_onion_history_header_decode(buf, &hdr_out);
    if (0 == readsize)
        TEST_ERROR;
    if (H5FD__ONION_HEADER_VERSION_CURR != hdr_out.version)
        TEST_ERROR;
    if (HDmemcmp(&hdr_out.checksum, &buf[readsize - 4], 4) != 0)
        TEST_ERROR;
    if (hdr_out.checksum != H5_checksum_fletcher32(buf, readsize - 4))
        TEST_ERROR;
    if (filter->page_size != hdr_out.page_size)
        TEST_ERROR;
    if (hdr_out.whole_history_addr + hdr_out.whole_history_size != filesize)
        TEST_ERROR;
    if (filter->origin_eof != hdr_out.origin_eof)
        TEST_ERROR;

    HDfree(buf);
    buf = NULL;

    /* Injest whole-history.
     */

    readsize = hdr_out.whole_history_size;
    buf      = (unsigned char *)HDmalloc(sizeof(unsigned char) * readsize);
    if (NULL == buf)
        TEST_ERROR;
    if (H5FDread(raw_file, H5FD_MEM_DRAW, H5P_DEFAULT, hdr_out.whole_history_addr, readsize, buf) < 0)
        TEST_ERROR;

    /* Initial read, get count of revisions */
    readsize = H5FD_onion_whole_history_decode(buf, &whs_out);
    if (0 == readsize)
        TEST_ERROR;
    if (H5FD__ONION_WHOLE_HISTORY_VERSION_CURR != whs_out.version)
        TEST_ERROR;
    if (HDmemcmp(&whs_out.checksum, &buf[readsize - 4], 4) != 0)
        TEST_ERROR;
    if (whs_out.checksum != H5_checksum_fletcher32(buf, readsize - 4))
        TEST_ERROR;
    if (filter->n_revisions != whs_out.n_revisions)
        TEST_ERROR;

    /* Final read, populate pointers to revision records */
    whs_out.record_pointer_list =
        (H5FD_onion_record_pointer_t *)HDcalloc(whs_out.n_revisions, sizeof(H5FD_onion_record_pointer_t));
    if (NULL == whs_out.record_pointer_list)
        TEST_ERROR;
    if (H5FD_onion_whole_history_decode(buf, &whs_out) != readsize)
        TEST_ERROR;

    /* Re-use buffer space to sanity-check checksum for record pointer(s). */
    HDassert(readsize >= sizeof(H5FD_onion_record_pointer_t));
    for (i = 0; i < whs_out.n_revisions; i++) {
#if 0
        uint32_t sum = 0;
#endif

        HDmemcpy(buf, &whs_out.record_pointer_list[i].phys_addr, 8);
        HDmemcpy(buf + 8, &whs_out.record_pointer_list[i].record_size, 8);
#if 0
        sum = H5_checksum_fletcher32(buf, 16);
#endif
        if (whs_out.record_pointer_list[i].checksum != H5_checksum_fletcher32(buf, 16))
            TEST_ERROR;
    }

    HDfree(buf);
    buf = NULL;

    /* Injest revision(s).
     */

    for (i = 0; i < whs_out.n_revisions; i++) {
        H5FD_onion_record_pointer_t *rpp = &whs_out.record_pointer_list[i];
        struct expected_revision *   erp = &filter->revisions[i];

        rev_out.archival_index.list           = NULL;
        rev_out.archival_index.n_entries      = 0;
        rev_out.archival_index.page_size_log2 = 0;
        rev_out.comment_size                  = 0;
        rev_out.comment                       = NULL;
        rev_out.username_size                 = 0;
        rev_out.username                      = NULL;

        readsize = rpp->record_size;
        buf      = (unsigned char *)HDmalloc((size_t)rpp->record_size);
        if (NULL == buf)
            TEST_ERROR;
        if (H5FDread(raw_file, H5FD_MEM_DRAW, H5P_DEFAULT, rpp->phys_addr, rpp->record_size, buf) < 0)
            TEST_ERROR;

        /* Initial revision read -- get fixed components */
        readsize = H5FD_onion_revision_record_decode(buf, &rev_out);
        if (0 == readsize)
            TEST_ERROR;
        if (rpp->record_size != readsize)
            TEST_ERROR;
        if (H5FD__ONION_REVISION_RECORD_VERSION_CURR != rev_out.version)
            TEST_ERROR;
        if (HDmemcmp(&rev_out.checksum, &buf[readsize - 4], 4) != 0)
            TEST_ERROR;
        if (rev_out.checksum != H5_checksum_fletcher32(buf, readsize - 4))
            TEST_ERROR;
        if (erp->revision_id != rev_out.revision_id)
            TEST_ERROR;
        if (erp->parent_revision_id != rev_out.parent_revision_id)
            TEST_ERROR;
        if (erp->logi_eof != rev_out.logi_eof)
            TEST_ERROR;

        /* Final read, get variable-length data */
        rev_out.comment = (char *)HDmalloc((size_t)rev_out.comment_size);
        if (NULL == rev_out.comment)
            TEST_ERROR;
        rev_out.archival_index.list = (H5FD_onion_index_entry_t *)HDcalloc(rev_out.archival_index.n_entries,
                                                                           sizeof(H5FD_onion_index_entry_t));
        if (NULL == rev_out.archival_index.list)
            TEST_ERROR;
        rev_out.username = (char *)HDmalloc((size_t)rev_out.username_size);
        if (NULL == rev_out.username)
            TEST_ERROR;

        readsize = H5FD_onion_revision_record_decode(buf, &rev_out);
        if (rpp->record_size != readsize)
            TEST_ERROR;

        /* Compare revision info with expected filter */
        if (erp->comment == NULL) {
            if (rev_out.comment_size != 0)
                TEST_ERROR;
        }
        else {
            if (HDstrlen(rev_out.comment) != HDstrlen(erp->comment))
                TEST_ERROR;
            if (HDstrcmp(rev_out.comment, erp->comment) != 0)
                TEST_ERROR;
        }
        if (erp->n_index_entries != (uint64_t)(-1) &&
            erp->n_index_entries != rev_out.archival_index.n_entries)
            TEST_ERROR;

        HDfree(buf);
        buf = NULL;

        HDfree(rev_out.comment);
        rev_out.comment = NULL;

        HDfree(rev_out.archival_index.list);
        rev_out.archival_index.list = NULL;

        HDfree(rev_out.username);
        rev_out.username = NULL;
    }

    HDfree(whs_out.record_pointer_list);
    whs_out.record_pointer_list = NULL;

    return 0;

error:
    if (buf != NULL)
        HDfree(buf);
    if (rev_out.comment != NULL)
        HDfree(rev_out.comment);
    if (rev_out.archival_index.list != NULL)
        HDfree(rev_out.archival_index.list);
    if (rev_out.username != NULL)
        HDfree(rev_out.username);
    if (whs_out.record_pointer_list != NULL)
        HDfree(whs_out.record_pointer_list);

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
 *     + .onion.recovery created w/ "empty" whole-history
 *     + Cannot open onionized canonical file (incomplete history, no rev)
 *
 * Inspect file contents on backing store.
 * Return -1 on problem, 0 if okay.
 *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 */
static int
verify_stored_onion_create_0_open(struct onion_filepaths *paths, H5FD_onion_fapl_info_t *onion_info)
{
    H5FD_t *       file            = NULL; /* virtual file to look at raw file contents */
    unsigned char *act_buf         = NULL; /* allocated area for actual file bytes */
    hid_t          fapl_id         = onion_info->backing_fapl_id;
    herr_t         err_ret         = FAIL;
    unsigned char  hdr_exp_bytes[] = {
        'O', 'H', 'D', 'H', 1, 1, 0, 0, 0, 0, 0, 0, /* page-size encoded below */
        0,   0,   0,   0,   0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0,   0,   20,  0,   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* checksum encoded below */
    };
    size_t        whs_exp_bytes_size = 20;
    unsigned char whs_exp_bytes[]    = {
        'O', 'W', 'H', 'S', 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /* checksum encoded below */
    };
    unsigned char *ptr           = NULL;
    uint32_t       sum           = 0;
    hid_t          onion_fapl_id = H5I_INVALID_HID;

    onion_fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    if (H5I_INVALID_HID == onion_fapl_id)
        TEST_ERROR
    if (H5Pset_fapl_onion(onion_fapl_id, onion_info) < 0)
        TEST_ERROR

    /* Finish populating expected header bytes
     */
    ptr = hdr_exp_bytes + 8; /* WARNING: must match format */
    UINT32ENCODE(ptr, onion_info->page_size);
    sum = H5_checksum_fletcher32(hdr_exp_bytes, H5FD__ONION_ENCODED_SIZE_HEADER - 4);
    ptr = hdr_exp_bytes + H5FD__ONION_ENCODED_SIZE_HEADER - 4;
    UINT32ENCODE(ptr, sum);
    ptr = NULL;

    /* Finish populating expected whole-history bytes
     */
    sum = H5_checksum_fletcher32(whs_exp_bytes, H5FD__ONION_ENCODED_SIZE_WHOLE_HISTORY - 4);
    ptr = whs_exp_bytes + H5FD__ONION_ENCODED_SIZE_WHOLE_HISTORY - 4;
    UINT32ENCODE(ptr, sum);
    ptr = NULL;

    /* Look at h5 file: should have zero bytes.
     */

    file = H5FDopen(paths->canon, H5F_ACC_RDONLY, fapl_id, HADDR_UNDEF);
    if (NULL == file)
        TEST_ERROR;

    act_buf = (unsigned char *)HDcalloc(1, 8); /* any size would do */
    if (NULL == act_buf)
        TEST_ERROR;

    H5E_BEGIN_TRY
    {
        err_ret = H5FDread(file, H5FD_MEM_DRAW, H5P_DEFAULT, 0, 1, act_buf);
    }
    H5E_END_TRY;
    if (err_ret != FAIL)
        TEST_ERROR; /* cannot read from empty file */

    HDfree(act_buf);
    act_buf = NULL;

    if (H5FDclose(file) < 0)
        TEST_ERROR;
    file = NULL;

    /* Look at onion file: should have header.
     */

    if (compare_file_bytes_exactly(paths->onion, fapl_id, H5FD__ONION_ENCODED_SIZE_HEADER, hdr_exp_bytes) < 0)
        TEST_ERROR;

    /* Look at history backing file: should have nascent whole-history.
     */

    if (compare_file_bytes_exactly(paths->recovery, fapl_id, whs_exp_bytes_size, whs_exp_bytes) < 0)
        TEST_ERROR;

    /* Inspect .h5 file contents
     */

    if (compare_file_bytes_exactly(paths->canon, fapl_id, 8, (const unsigned char *)"ONIONEOF") < 0) {
        TEST_ERROR;
    }

    if (H5Pclose(onion_fapl_id) < 0)
        TEST_ERROR;
    onion_fapl_id = H5I_INVALID_HID;

    return 0;

error:
    if (file != NULL)
        (void)H5FDclose(file);
    if (act_buf != NULL)
        HDfree(act_buf);
    if (onion_fapl_id != H5I_INVALID_HID)
        (void)H5Pclose(onion_fapl_id);

    return -1;
} /* end verify_stored_onion_create_0_open() */

/*-----------------------------------------------------------------------------
 *
 * Function:    test_create_oniontarget()
 *
 * Purpose:     Test the ability of the Onion VFD to create a valid
 *              'onionized' file.
 *
 *              When `truncate_canonical` is FALSE, the canonical file is
 *              nonexistent on the backing store on onion-creation.
 *              When `truncate_canonical` is TRUE, a canonical file is created
 *              on the backing store with known contents, which are to be
 *              truncated on onion-creation.
 *
 * Return:      PASSED : 0
 *              FAILED : -1
 *
 *-----------------------------------------------------------------------------
 */
static int
test_create_oniontarget(hbool_t truncate_canonical, hbool_t with_initial_data)
{
    const char *            basename   = "somesuch";
    hid_t                   fapl_id    = H5I_INVALID_HID;
    struct onion_filepaths *paths      = NULL;
    H5FD_onion_fapl_info_t  onion_info = {
        H5FD_ONION_FAPL_INFO_VERSION_CURR,
        H5I_INVALID_HID,               /* backing_fapl_id  */
        ONION_TEST_PAGE_SIZE_5,        /* page_size        */
        H5FD_ONION_STORE_TARGET_ONION, /* store_target     */
        H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST,
        0,               /* force_write_open */
        0,               /* creation_flags   */
        "initial commit" /* comment          */
    };
    H5FD_t *                vfile_raw = NULL; /* virtual file to look at raw file contents */
    H5FD_t *                vfile_rw  = NULL; /* Onion virtual file for read/write */
    H5FD_t *                vfile_ro  = NULL; /* Onion virtual file for read-only */
    struct expected_history filter;
    char *                  buf = NULL;

    if (TRUE == truncate_canonical && TRUE == with_initial_data) {
        TESTING("onion creation; truncate extant canonical; w/ initial data");
    }
    else if (TRUE == truncate_canonical) {
        TESTING("onion creation; truncate extant canonical; no initial data");
    }
    else if (TRUE == with_initial_data) {
        TESTING("onion creation; no extant canonical; w/ initial data");
    }
    else {
        TESTING("onion creation; no extant canonical; no initial data");
    }

    /*********
     * SETUP *
     *********/

    onion_info.backing_fapl_id = h5_fileaccess();
    fapl_id                    = H5Pcreate(H5P_FILE_ACCESS);
    if (H5I_INVALID_HID == fapl_id)
        TEST_ERROR;
    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;

    paths = onion_filepaths_init(basename, &onion_info);
    if (NULL == paths)
        TEST_ERROR;

    HDremove(paths->canon);
    HDremove(paths->onion);
    HDremove(paths->recovery);

    /* create canonical file to be truncated */
    if (TRUE == truncate_canonical) {
        /* Create canonical file. */
        vfile_raw = H5FDopen(paths->canon, flags_create_s, onion_info.backing_fapl_id, HADDR_UNDEF);
        if (NULL == vfile_raw)
            TEST_ERROR;
        if (H5FDset_eoa(vfile_raw, H5FD_MEM_DRAW, b_list_size_s) < 0)
            TEST_ERROR;
        if (H5FDwrite(vfile_raw, H5FD_MEM_DRAW, H5P_DEFAULT, 0, b_list_size_s, b_list_s) < 0) {
            TEST_ERROR;
        }
        if (H5FDclose(vfile_raw) < 0)
            TEST_ERROR;
        vfile_raw = NULL;
        H5E_BEGIN_TRY
        {
            vfile_raw = H5FDopen(paths->canon, H5F_ACC_RDONLY, fapl_id, HADDR_UNDEF);
        }
        H5E_END_TRY;
        if (NULL != vfile_raw)
            TEST_ERROR; /* no onion history to onion-open created file */

        /* Create "existing onion file". */
        vfile_raw = H5FDopen(paths->onion, flags_create_s, onion_info.backing_fapl_id, HADDR_UNDEF);
        if (NULL == vfile_raw)
            TEST_ERROR;
        if (H5FDset_eoa(vfile_raw, H5FD_MEM_DRAW, b_list_size_s) < 0)
            TEST_ERROR;
        if (H5FDwrite(vfile_raw, H5FD_MEM_DRAW, H5P_DEFAULT, 0, 23, "prior history stand-in") < 0) {
            TEST_ERROR;
        }
        if (H5FDclose(vfile_raw) < 0)
            TEST_ERROR;
        vfile_raw = NULL;
    } /* end if to create canonical file for truncation */

    /*
     * OPENED
     */

    /* Begin creation of onionized file from nothing.
     */

    vfile_rw = H5FDopen(paths->canon, flags_create_s, fapl_id, HADDR_UNDEF);
    if (NULL == vfile_rw)
        TEST_ERROR;

    if (verify_stored_onion_create_0_open(paths, &onion_info) < 0)
        TEST_ERROR

    H5E_BEGIN_TRY
    {
        vfile_ro = H5FDopen(paths->canon, H5F_ACC_RDONLY, fapl_id, HADDR_UNDEF);
    }
    H5E_END_TRY;
    if (vfile_ro != NULL)
        TEST_ERROR; /* onionization (creation) not complete; nothing to open */

    /*
     * WRITING
     */

    if (TRUE == with_initial_data) {
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
        buf = (char *)HDmalloc(sizeof(char) * 4);
        if (NULL == buf)
            TEST_ERROR;
        if (H5FDread(vfile_rw, H5FD_MEM_DRAW, H5P_DEFAULT, 0, 4, buf) < 0) {
            TEST_ERROR;
        }
        if (HDmemcmp(a_list_s, buf, 4) != 0)
            TEST_ERROR;
        HDfree(buf);
        buf = NULL;

        /* Write the latter half of buffer at addr 0 (more than one page) */
        half_size = a_list_size_s / 2;
        buf_size  = a_list_size_s - half_size;
        if (buf_size <= onion_info.page_size)
            TEST_ERROR;
        if (H5FDset_eoa(vfile_rw, H5FD_MEM_DRAW, buf_size) < 0)
            TEST_ERROR;
        if (H5FDwrite(vfile_rw, H5FD_MEM_DRAW, H5P_DEFAULT, 0, buf_size, a_list_s + half_size) < 0) {
            TEST_ERROR;
        }

        /* Verify logical file contents. */
        buf = (char *)HDmalloc(sizeof(char) * buf_size);
        if (NULL == buf)
            TEST_ERROR;
        if (H5FDread(vfile_rw, H5FD_MEM_DRAW, H5P_DEFAULT, 0, buf_size, buf) < 0) {
            TEST_ERROR;
        }
        if (HDmemcmp(a_list_s + half_size, buf, buf_size) != 0)
            TEST_ERROR;
        HDfree(buf);
        buf = NULL;

        /* Overwrite existing data with entire buffer at addr 0 */
        if (H5FDset_eoa(vfile_rw, H5FD_MEM_DRAW, a_list_size_s) < 0)
            TEST_ERROR;
        if (H5FDwrite(vfile_rw, H5FD_MEM_DRAW, H5P_DEFAULT, 0, a_list_size_s, a_list_s) < 0) {
            TEST_ERROR;
        }

        /* Verify logical file contents. */
        buf = (char *)HDmalloc(sizeof(char) * a_list_size_s);
        if (NULL == buf)
            TEST_ERROR;
        if (H5FDread(vfile_rw, H5FD_MEM_DRAW, H5P_DEFAULT, 0, a_list_size_s, buf) < 0) {
            TEST_ERROR;
        }
        if (HDmemcmp(a_list_s, buf, a_list_size_s) != 0)
            TEST_ERROR;
        HDfree(buf);
        buf = NULL;
    } /* end if writing data to logical file */

    /*
     * CLOSED
     */

    if (H5FDclose(vfile_rw) < 0)
        TEST_ERROR;
    vfile_rw = NULL;

    /* Look at h5 file: should be known-empty
     */

    if (compare_file_bytes_exactly(paths->canon, onion_info.backing_fapl_id, 8,
                                   (const unsigned char *)"ONIONEOF") < 0) {
        TEST_ERROR;
    }

    /* Look at recovery file: should be gone.
     */

    H5E_BEGIN_TRY
    {
        vfile_raw = H5FDopen(paths->recovery, H5F_ACC_RDONLY, onion_info.backing_fapl_id, HADDR_UNDEF);
    }
    H5E_END_TRY;
    if (NULL != vfile_raw)
        TEST_ERROR;

    /* Inspect onion file.
     */

    vfile_raw = H5FDopen(paths->onion, H5F_ACC_RDONLY, onion_info.backing_fapl_id, HADDR_UNDEF);
    if (NULL == vfile_raw)
        TEST_ERROR;

    filter.page_size                       = onion_info.page_size;
    filter.n_revisions                     = 1;
    filter.origin_eof                      = 0;
    filter.revisions[0].comment            = onion_info.comment;
    filter.revisions[0].n_index_entries    = (uint64_t)(-1); /* don't care */
    filter.revisions[0].revision_id        = 0;
    filter.revisions[0].parent_revision_id = 0;
    filter.revisions[0].logi_eof           = (TRUE == with_initial_data) ? a_list_size_s : 0;

    if (verify_history_as_expected_onion(vfile_raw, &filter) < 0)
        TEST_ERROR;

    if (H5FDclose(vfile_raw) < 0)
        TEST_ERROR;
    vfile_raw = NULL;

    /* R/O open the file with Onion VFD; inspect logial file.
     */

    vfile_ro = H5FDopen(paths->canon, H5F_ACC_RDONLY, fapl_id, HADDR_UNDEF);
    if (NULL == vfile_ro)
        TEST_ERROR;

    if (TRUE == with_initial_data) {
        if (H5FDget_eof(vfile_ro, H5FD_MEM_DRAW) != a_list_size_s)
            TEST_ERROR;
        if (H5FDget_eoa(vfile_ro, H5FD_MEM_DRAW) != 0)
            TEST_ERROR;
        if (H5FDset_eoa(vfile_ro, H5FD_MEM_DRAW, a_list_size_s) < 0)
            TEST_ERROR;
        buf = (char *)HDmalloc(sizeof(char) * a_list_size_s * 64);
        if (NULL == buf)
            TEST_ERROR;
        if (H5FDread(vfile_ro, H5FD_MEM_DRAW, H5P_DEFAULT, 0, a_list_size_s, buf) < 0) {
            TEST_ERROR;
        }
        if (HDmemcmp(a_list_s, buf, a_list_size_s) != 0)
            TEST_ERROR;
        HDfree(buf);
        buf = NULL;
    } /* end if data was written to initial revision */
    else {
        if (H5FDget_eoa(vfile_ro, H5FD_MEM_DRAW) != 0)
            TEST_ERROR;
        if (H5FDget_eof(vfile_ro, H5FD_MEM_DRAW) != 0)
            TEST_ERROR;
    } /* end if initial revision has no data */

    if (H5FDclose(vfile_ro) < 0)
        TEST_ERROR;
    vfile_ro = NULL;

    /*
     * CLEANUP
     */

    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    fapl_id = H5I_INVALID_HID;

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

    if (buf != NULL)
        HDfree(buf);

    if (vfile_raw != NULL)
        (void)H5FDclose(vfile_raw);
    if (vfile_rw != NULL)
        (void)H5FDclose(vfile_rw);
    if (vfile_ro != NULL)
        (void)H5FDclose(vfile_ro);

    if (fapl_id != H5I_INVALID_HID)
        (void)H5Pclose(fapl_id);

    return -1;
} /* end test_create_oniontarget() */

/*-----------------------------------------------------------------------------
 *
 * Function:    test_several_revisions_with_logical_gaps()
 *
 * Purpose:     Test the ability of the Onion VFD to create a valid
 *              'onionized' file.
 *
 *              When `truncate_canonical` is FALSE, the canonical file is
 *              nonexistent on the backing store on onion-creation.
 *              When `truncate_canonical` is TRUE, a canonical file is created
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
    const char *            basename   = "somesuch";
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
    H5FD_t *                file = NULL; /* Onion virtual file for read/write */
    struct expected_history filter;
    unsigned char *         buf = NULL;
    struct revise_revision  about[4];
#if 0
    H5FD_onion_history_header_t hdr_out;
    H5FD_onion_revision_record_t rev_out;
#endif
    H5FD_onion_whole_history_t whs_out;
    size_t                     i     = 0;
    haddr_t                    size  = 0;
    uint64_t                   a_off = ONION_TEST_PAGE_SIZE_5 + 7; /* 39 */
    uint64_t                   b_off = (((a_off + a_list_size_s + ONION_TEST_PAGE_SIZE_5 - 1) >> 5) << 5) +
                     ONION_TEST_PAGE_SIZE_5 + 7; /* full page between */

    TESTING("multiple revisions with gaps and overlap");

    /*********
     * SETUP *
     *********/

#if 0
    hdr_out.version = H5FD__ONION_HEADER_VERSION_CURR;
    rev_out.version = H5FD__ONION_REVISION_RECORD_VERSION_CURR;
#endif
    whs_out.version             = H5FD__ONION_WHOLE_HISTORY_VERSION_CURR;
    whs_out.n_revisions         = 0;
    whs_out.record_pointer_list = NULL;

    onion_info.backing_fapl_id = h5_fileaccess();

    paths = onion_filepaths_init(basename, &onion_info);
    if (NULL == paths)
        TEST_ERROR;

    HDremove(paths->canon);
    HDremove(paths->onion);
    HDremove(paths->recovery);

    /* Empty first revision */
    about[0].truncate    = TRUE;
    about[0].revision_id = H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST;
    about[0].comment     = "first";
    about[0].n_writes    = 0;

    about[1].truncate         = FALSE;
    about[1].revision_id      = H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST;
    about[1].comment          = "second";
    about[1].n_writes         = 1;
    about[1].writes[0].offset = a_off;
    about[1].writes[0].size   = a_list_size_s;
    about[1].writes[0].buf    = a_list_s;

    about[2].truncate         = FALSE;
    about[2].revision_id      = H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST;
    about[2].comment          = "third";
    about[2].n_writes         = 1; /* TODO: several writes */
    about[2].writes[0].offset = b_off;
    about[2].writes[0].size   = b_list_size_s;
    about[2].writes[0].buf    = b_list_s;

    about[3].truncate         = FALSE;
    about[3].revision_id      = H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST;
    about[3].comment          = "fourth";
    about[3].n_writes         = 1;
    about[3].writes[0].offset = 0;
    about[3].writes[0].size   = a_list_size_s;
    about[3].writes[0].buf    = a_list_s;

    if (do_onion_open_and_writes(paths->canon, &onion_info, 4, about) < 0)
        TEST_ERROR;

    /* Inspect logical file */

    /* THIS IS THE INITIAL FILE, SHOULD ONLY HAVE 8 BYTES */
    onion_info.revision_id = 0;
    fapl_id                = H5Pcreate(H5P_FILE_ACCESS);
    if (H5I_INVALID_HID == fapl_id)
        TEST_ERROR;
    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;
    file = H5FDopen(paths->canon, H5F_ACC_RDONLY, fapl_id, HADDR_UNDEF);
    if (NULL == file)
        TEST_ERROR;
    if (8 != H5FDget_eof(file, H5FD_MEM_DRAW)) {
        HDprintf("\nEOF is not zero, it is: %" PRIuHADDR "\n", H5FDget_eof(file, H5FD_MEM_DRAW));
        TEST_ERROR;
    }
    if (H5FDclose(file) < 0)
        TEST_ERROR;
    file = NULL;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    fapl_id = H5I_INVALID_HID;

    /* Empty first revision */
    onion_info.revision_id = 1;
    fapl_id                = H5Pcreate(H5P_FILE_ACCESS);
    if (H5I_INVALID_HID == fapl_id)
        TEST_ERROR;
    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;
    file = H5FDopen(paths->canon, H5F_ACC_RDONLY, fapl_id, HADDR_UNDEF);
    if (NULL == file)
        TEST_ERROR;
    if (0 != H5FDget_eof(file, H5FD_MEM_DRAW)) {
        HDprintf("\nEOF is not zero, it is: %" PRIuHADDR "\n", H5FDget_eof(file, H5FD_MEM_DRAW));
        TEST_ERROR;
    }
    if (H5FDclose(file) < 0)
        TEST_ERROR;
    file = NULL;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    fapl_id = H5I_INVALID_HID;

    /* One offset block in second revision */
    onion_info.revision_id = 2;
    // onion_info.revision_id = 1;
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    if (H5I_INVALID_HID == fapl_id)
        TEST_ERROR;
    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;
    file = H5FDopen(paths->canon, H5F_ACC_RDONLY, fapl_id, HADDR_UNDEF);
    if (NULL == file)
        TEST_ERROR;
    size = a_off + a_list_size_s;
    if (size != H5FDget_eof(file, H5FD_MEM_DRAW)) {
        HDprintf("\nEOF is not %" PRIuHADDR ", it is: %" PRIuHADDR "\n", size,
                 H5FDget_eof(file, H5FD_MEM_DRAW));
        TEST_ERROR;
    }
    buf = (unsigned char *)HDmalloc(sizeof(unsigned char) * size);
    if (NULL == buf)
        TEST_ERROR;
    if (H5FDset_eoa(file, H5FD_MEM_DRAW, size) < 0)
        TEST_ERROR;
    if (H5FDread(file, H5FD_MEM_DRAW, H5P_DEFAULT, 0, size, buf) < 0)
        TEST_ERROR;
    for (i = 0; i < a_off; i++) {
        if (0 != buf[i])
            TEST_ERROR;
    }
    if (HDmemcmp(buf + a_off, a_list_s, a_list_size_s) != 0)
        TEST_ERROR;
    HDfree(buf);
    buf = NULL;
    /* Repeat read at page offset; test possible read offset error */
    buf = (unsigned char *)HDmalloc(sizeof(unsigned char) * ONION_TEST_PAGE_SIZE_5);
    if (NULL == buf)
        TEST_ERROR;
    if (H5FDread(file, H5FD_MEM_DRAW, H5P_DEFAULT, ONION_TEST_PAGE_SIZE_5, ONION_TEST_PAGE_SIZE_5, buf) < 0)
        TEST_ERROR;
    size = a_off - ONION_TEST_PAGE_SIZE_5;
    for (i = 0; i < size; i++) {
        if (0 != buf[i])
            TEST_ERROR;
    }
    if (HDmemcmp(buf + size, a_list_s, ONION_TEST_PAGE_SIZE_5 - size) != 0)
        TEST_ERROR;
    HDfree(buf);
    buf = NULL;
    if (H5FDclose(file) < 0)
        TEST_ERROR;
    file = NULL;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    fapl_id = H5I_INVALID_HID;

    /* Two offset blocks in third revision */
    onion_info.revision_id = 3;
    // onion_info.revision_id = 2;
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
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
    buf = (unsigned char *)HDmalloc(sizeof(unsigned char) * size);
    if (NULL == buf)
        TEST_ERROR;
    if (H5FDset_eoa(file, H5FD_MEM_DRAW, size) < 0)
        TEST_ERROR;
    if (H5FDread(file, H5FD_MEM_DRAW, H5P_DEFAULT, 0, size, buf) < 0)
        TEST_ERROR;
    for (i = 0; i < a_off; i++) {
        if (0 != buf[i])
            TEST_ERROR;
    }
    if (HDmemcmp(buf + a_off, a_list_s, a_list_size_s) != 0)
        TEST_ERROR;
    for (i = a_off + a_list_size_s; i < b_off; i++) {
        if (0 != buf[i])
            TEST_ERROR;
    }
    if (HDmemcmp(buf + b_off, b_list_s, b_list_size_s) != 0)
        TEST_ERROR;
    HDfree(buf);
    buf = NULL;
    if (H5FDclose(file) < 0)
        TEST_ERROR;
    file = NULL;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    fapl_id = H5I_INVALID_HID;

    /* From start and partial overwrite in fourth revision */
    onion_info.revision_id = 4;
    // onion_info.revision_id = 3;
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
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
    buf = (unsigned char *)HDmalloc(sizeof(unsigned char) * size);
    if (NULL == buf)
        TEST_ERROR;
    if (H5FDset_eoa(file, H5FD_MEM_DRAW, size) < 0)
        TEST_ERROR;
    if (H5FDread(file, H5FD_MEM_DRAW, H5P_DEFAULT, 0, size, buf) < 0)
        TEST_ERROR;
    if (HDmemcmp(buf, a_list_s, a_list_size_s) != 0)
        TEST_ERROR;
    if (HDmemcmp(buf + a_list_size_s, a_list_s + a_list_size_s - a_off, a_off) != 0)
        TEST_ERROR;
    for (i = a_off + a_list_size_s; i < b_off; i++) {
        if (0 != buf[i])
            TEST_ERROR;
    }
    if (HDmemcmp(buf + b_off, b_list_s, b_list_size_s) != 0)
        TEST_ERROR;
    HDfree(buf);
    buf = NULL;
    if (H5FDclose(file) < 0)
        TEST_ERROR;
    file = NULL;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    fapl_id = H5I_INVALID_HID;

    /* No fifth revision */
    // TODO: Can this be done without triggering an error?
    /*    onion_info.revision_id = 5;
        //onion_info.revision_id = 4;
        fapl_id                = H5Pcreate(H5P_FILE_ACCESS);
        if (H5I_INVALID_HID == fapl_id)
            TEST_ERROR;
        if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
            TEST_ERROR;
        file = H5FDopen(paths->canon, H5F_ACC_RDONLY, fapl_id, HADDR_UNDEF);
        if (NULL != file)
            TEST_ERROR;

        if (H5FDclose(file) < 0)
            TEST_ERROR;
        file = NULL;
        if (H5Pclose(fapl_id) < 0)
            TEST_ERROR;
        fapl_id = H5I_INVALID_HID;
    */
    /* Inspect history construction */

    file = H5FDopen(paths->onion, H5F_ACC_RDONLY, onion_info.backing_fapl_id, HADDR_UNDEF);
    if (NULL == file)
        TEST_ERROR;

    filter.page_size   = onion_info.page_size;
    filter.n_revisions = 4;
    filter.origin_eof  = 0;

    filter.revisions[0].comment            = "first";
    filter.revisions[0].n_index_entries    = 0;
    filter.revisions[0].revision_id        = 0;
    filter.revisions[0].parent_revision_id = 0;
    filter.revisions[0].logi_eof           = 0;

    filter.revisions[1].comment            = "second";
    filter.revisions[1].n_index_entries    = (a_list_size_s + ONION_TEST_PAGE_SIZE_5 - 1) >> 5;
    filter.revisions[1].revision_id        = 1;
    filter.revisions[1].parent_revision_id = 0;
    filter.revisions[1].logi_eof           = a_off + a_list_size_s;

    filter.revisions[2].comment = "third";
    filter.revisions[2].n_index_entries =
        filter.revisions[1].n_index_entries + ((b_list_size_s + ONION_TEST_PAGE_SIZE_5 - 1) >> 5);
    filter.revisions[2].revision_id        = 2;
    filter.revisions[2].parent_revision_id = 1;
    filter.revisions[2].logi_eof           = b_off + b_list_size_s;

    filter.revisions[3].comment            = "fourth";
    filter.revisions[3].n_index_entries    = filter.revisions[2].n_index_entries + 1;
    filter.revisions[3].revision_id        = 3;
    filter.revisions[3].parent_revision_id = 2;
    filter.revisions[3].logi_eof           = b_off + b_list_size_s;

    if (verify_history_as_expected_onion(file, &filter) < 0)
        TEST_ERROR;

    if (H5FDclose(file) < 0)
        TEST_ERROR;
    file = NULL;

    /*
     * CLEANUP
     */

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

    if (whs_out.record_pointer_list != NULL)
        HDfree(whs_out.record_pointer_list);
    if (buf != NULL)
        HDfree(buf);
    if (file != NULL)
        (void)H5FDclose(file);
    if (fapl_id != H5I_INVALID_HID)
        (void)H5Pclose(fapl_id);

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
    H5FD_t *       file    = NULL; /* Onion virtual file for read/write */
    unsigned char *buf_vfy = NULL;
    size_t         i       = 0;

    for (i = 0; i < n_ops; i++) {
        size_t       j     = 0;
        unsigned int flags = H5F_ACC_RDWR;

        if (i != 0 && about[i].truncate == TRUE)
            goto error;

        if (TRUE == about[i].truncate)
            flags |= H5F_ACC_CREAT | H5F_ACC_TRUNC;

        onion_info_p->revision_id = about[i].revision_id;
        if (about[i].comment != NULL) {
            j = MIN(HDstrlen(about[i].comment), H5FD_ONION_FAPL_INFO_COMMENT_MAX_LEN);
            HDmemcpy(onion_info_p->comment, about[i].comment, j);
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
            buf_vfy = (unsigned char *)HDmalloc(sizeof(unsigned char) * wi->size);
            if (NULL == buf_vfy)
                TEST_ERROR;
            if (H5FDread(file, H5FD_MEM_DRAW, H5P_DEFAULT, wi->offset, wi->size, buf_vfy) < 0)
                TEST_ERROR;
            if (HDmemcmp(buf_vfy, wi->buf, wi->size) != 0) {
                const unsigned char *_buf = wi->buf;
                size_t               z    = 0;
                HDputs("i  exp  act");
                for (z = 0; z < wi->size; z++)
                    HDprintf("%02zx %c %c\n", z, _buf[z], buf_vfy[z]);
                HDfflush(stdout);
                TEST_ERROR;
            }
            HDfree(buf_vfy);
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
    if (fapl_id != H5I_INVALID_HID)
        (void)H5Pclose(fapl_id);
    if (buf_vfy != NULL)
        HDfree(buf_vfy);

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
    const char *            basename   = "somesuch";
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
    H5FD_t *                    file = NULL; /* Onion virtual file for read/write */
    unsigned char *             buf  = NULL;
    struct revise_revision      about[2];
    H5FD_onion_history_header_t hdr_out;
#if 0
    H5FD_onion_revision_record_t rev_out;
#endif
    H5FD_onion_whole_history_t whs_out;
    size_t                     i     = 0;
    uint64_t                   a_off = b_list_size_s - a_list_size_s;

    TESTING("page-aligned history on onion-created file");

    /*********
     * SETUP *
     *********/

    hdr_out.version = H5FD__ONION_HEADER_VERSION_CURR;
#if 0
    rev_out.version = H5FD__ONION_REVISION_RECORD_VERSION_CURR;
#endif
    whs_out.version             = H5FD__ONION_WHOLE_HISTORY_VERSION_CURR;
    whs_out.n_revisions         = 0;
    whs_out.record_pointer_list = NULL;

    onion_info.backing_fapl_id = h5_fileaccess();
    fapl_id                    = H5Pcreate(H5P_FILE_ACCESS);
    if (H5I_INVALID_HID == fapl_id)
        TEST_ERROR;
    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;

    paths = onion_filepaths_init(basename, &onion_info);
    if (NULL == paths)
        TEST_ERROR;

    HDremove(paths->canon);
    HDremove(paths->onion);
    HDremove(paths->recovery);

    about[0].truncate         = TRUE;
    about[0].revision_id      = H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST;
    about[0].comment          = "initial_commit";
    about[0].n_writes         = 1;
    about[0].writes[0].offset = 0;
    about[0].writes[0].size   = b_list_size_s;
    about[0].writes[0].buf    = b_list_s;

    about[1].truncate         = FALSE;
    about[1].revision_id      = H5FD_ONION_FAPL_INFO_REVISION_ID_LATEST;
    about[1].comment          = "second";
    about[1].n_writes         = 1;
    about[1].writes[0].offset = a_off;
    about[1].writes[0].size   = a_list_size_s;
    about[1].writes[0].buf    = a_list_s;

    if (do_onion_open_and_writes(paths->canon, &onion_info, 2, about) < 0)
        TEST_ERROR;

    /* Inspect logical file */
    buf = (unsigned char *)HDmalloc(sizeof(unsigned char) * b_list_size_s);
    if (NULL == buf)
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
    if (HDmemcmp(a_list_s, buf + a_off, a_list_size_s) != 0) {
        size_t k;
        HDprintf("aoff: %" PRIu64 "\n", a_off);
        HDputs("i exp act");
        for (k = 0; k < b_list_size_s; k++) {
            HDprintf("%3zu:: %c : %c\n", k, (k < a_off) ? ' ' : a_list_s[k - a_off], buf[k]);
        }
        fflush(stdout);
        TEST_ERROR;
    }
    if (HDmemcmp(b_list_s, buf, a_off) != 0) {
        size_t k;
        HDprintf("aoff: %" PRIu64 "\n", a_off);
        HDputs("i exp act");
        for (k = 0; k < b_list_size_s; k++) {
            HDprintf("%3zu:: %c : %c\n", k, (k < a_off) ? b_list_s[k] : ' ', buf[k]);
        }
        fflush(stdout);
        TEST_ERROR;
    }
    if (H5FDclose(file) < 0)
        TEST_ERROR;
    file = NULL;
    HDfree(buf);
    buf = NULL;

    /* Inspect history construction */

    file = H5FDopen(paths->onion, H5F_ACC_RDONLY, onion_info.backing_fapl_id, HADDR_UNDEF);
    if (NULL == file)
        TEST_ERROR;
    if (H5FDset_eoa(file, H5FD_MEM_DRAW, H5FDget_eof(file, H5FD_MEM_DRAW)) < 0)
        TEST_ERROR;

    buf = (unsigned char *)HDmalloc(H5FD__ONION_ENCODED_SIZE_HEADER);
    if (NULL == buf)
        TEST_ERROR;
    if (H5FDread(file, H5FD_MEM_DRAW, H5P_DEFAULT, 0, H5FD__ONION_ENCODED_SIZE_HEADER, buf) < 0)
        TEST_ERROR;
    if (H5FD_onion_history_header_decode(buf, &hdr_out) != H5FD__ONION_ENCODED_SIZE_HEADER)
        TEST_ERROR;
    if (hdr_out.whole_history_addr & ((1 << 5) - 1)) /* 5::PAGE_SIZE_5 */
        TEST_ERROR;
    HDfree(buf);
    buf = NULL;

    buf = (unsigned char *)HDmalloc(hdr_out.whole_history_size);
    if (H5FDread(file, H5FD_MEM_DRAW, H5P_DEFAULT, hdr_out.whole_history_addr, hdr_out.whole_history_size,
                 buf) < 0)
        TEST_ERROR;
    if (H5FD_onion_whole_history_decode(buf, &whs_out) != hdr_out.whole_history_size)
        TEST_ERROR;
    if (whs_out.n_revisions != 2)
        TEST_ERROR;
    whs_out.record_pointer_list =
        (H5FD_onion_record_pointer_t *)HDcalloc(whs_out.n_revisions, sizeof(H5FD_onion_record_pointer_t));
    if (NULL == whs_out.record_pointer_list)
        TEST_ERROR;
    if (H5FD_onion_whole_history_decode(buf, &whs_out) != hdr_out.whole_history_size)
        TEST_ERROR;
    HDfree(buf);
    buf = NULL;

    for (i = 0; i < whs_out.n_revisions; i++) {
        H5FD_onion_record_pointer_t *rr_p = &whs_out.record_pointer_list[i];
        if (rr_p->phys_addr & ((1 << 5) - 1)) /* 5::PAGE_SIZE_5 */
            TEST_ERROR;
        /* TODO: check phys_addr of each page entry? */
    }

    HDfree(whs_out.record_pointer_list);
    whs_out.record_pointer_list = NULL;

    if (H5FDclose(file) < 0)
        TEST_ERROR;
    file = NULL;
    HDfree(buf);
    buf = NULL;

    /*
     * CLEANUP
     */

    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    fapl_id = H5I_INVALID_HID;

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

    if (whs_out.record_pointer_list != NULL)
        HDfree(whs_out.record_pointer_list);
    if (buf != NULL)
        HDfree(buf);
    if (file != NULL)
        (void)H5FDclose(file);
    if (fapl_id != H5I_INVALID_HID)
        (void)H5Pclose(fapl_id);

    return -1;
} /* end test_page_aligned_history_create() */

/*-----------------------------------------------------------------------------
 *
 * Function:    test_integration_create()
 *
 * Purpose:     Create and make multiple revisions in an HDF5 file.
 *
 * Return:      PASSED : 0
 *              FAILED : -1
 *
 *-----------------------------------------------------------------------------
 */
static int
test_integration_create(void)
{
    const char *basename = "integration.h5";
    // const char *basename = "somesuch.h5";
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
    hid_t file_id = H5I_INVALID_HID;

    TESTING("onion-created two dimensional HDF5 file with revisions");

    /*********
     * SETUP *
     *********/

    onion_info.backing_fapl_id = h5_fileaccess();
    fapl_id                    = H5Pcreate(H5P_FILE_ACCESS);
    if (H5I_INVALID_HID == fapl_id)
        TEST_ERROR;
    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;

    paths = onion_filepaths_init(basename, &onion_info);
    if (NULL == paths)
        TEST_ERROR;

    HDremove(paths->canon);
    HDremove(paths->onion);
    HDremove(paths->recovery);

    /* Create skeleton file without onion */
    hid_t   file, space, dset, dcpl; /* Handles */
    hsize_t dims[2] = {128, 256}, maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED}, chunk[2] = {4, 4};
    int     wdata[128][256], /* Write buffer */
        fillval, i, j;

    /*
     * Initialize data.
     */
    for (i = 0; i < 128; i++)
        for (j = 0; j < 256; j++)
            wdata[i][j] = i * j - j;

    /*
     * Create a new file using the default properties.
     */
    // file = H5Fcreate ("example.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    file = H5Fcreate(paths->canon, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create dataspace with unlimited dimensions.
     */
    space = H5Screate_simple(2, dims, maxdims);

    /*
     * Create the dataset creation property list, and set the chunk
     * size.
     */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    if (H5Pset_chunk(dcpl, 2, chunk) < 0)
        TEST_ERROR

    /*
     * Set the fill value for the dataset.
     */
    fillval = 99;
    if (H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval) < 0)
        TEST_ERROR

    /*
     * Set the allocation time to "early".  This way we can be sure
     * that reading from the dataset immediately after creation will
     * return the fill value.
     */
    if (H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0)
        TEST_ERROR

    /*
     * Create the dataset using the dataset creation property list.
     */
    dset = H5Dcreate(file, "DS1", H5T_STD_I32LE, space, H5P_DEFAULT, dcpl, H5P_DEFAULT);

    /*
     * Write the data to the dataset.
     */
    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata[0]) < 0)
        TEST_ERROR

    /*
     * Close and release resources.
     */
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR
    if (H5Dclose(dset) < 0)
        TEST_ERROR
    if (H5Sclose(space) < 0)
        TEST_ERROR
    if (H5Fclose(file) < 0)
        TEST_ERROR

    /*
     * Create first revision
     */
    // HDputs(".");
    fflush(stdout);

    file_id = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id);

    // HDputs(".");
    fflush(stdout);
    if (H5I_INVALID_HID == file_id) {
        TEST_ERROR;
    }
    // HDputs(".");
    HDfflush(stdout);

    ///
    dset = H5Dopen(file_id, "DS1", H5P_DEFAULT);
    if (dset < 0) {
        TEST_ERROR
    }

    // HDputs(".");
    HDfflush(stdout);
    int dset_data[128][256];
    for (i = 0; i < 128; i++)
        for (j = 0; j < 256; j++)
            dset_data[i][j] = i * 6 + j + 1;

#if 0
    printf("First revision\n");
    for (i = 0; i < 4; i++) {
        printf(" [");
        for (j = 0; j < 7; j++)
            printf(" %3d", dset_data[i][j]);
        printf("]\n");
    }
#endif

    // HDputs(".");
    HDfflush(stdout);
    if (H5Dwrite(dset, H5T_STD_I32LE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset_data) < 0)
        TEST_ERROR;
    // HDputs(".");
    HDfflush(stdout);

    // HDputs(".");
    HDfflush(stdout);

    if (H5Dclose(dset) < 0)
        TEST_ERROR
    dset = H5I_INVALID_HID;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR
    file_id = H5I_INVALID_HID;

    /*
     * Create second revision
     */
    // HDputs(".");
    fflush(stdout);

    file_id = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id);

    // HDputs(".");
    fflush(stdout);
    if (H5I_INVALID_HID == file_id) {
        TEST_ERROR;
    }
    // HDputs(".");
    HDfflush(stdout);

    dset = H5Dopen(file_id, "DS1", H5P_DEFAULT);
    if (dset < 0) {
        TEST_ERROR
    }

    // HDputs(".");
    HDfflush(stdout);
    for (i = 0; i < 128; i++)
        for (j = 0; j < 256; j++)
            dset_data[i][j] = i * 3 + j + 5;

#if 0
    printf("Second revision\n");
    for (i = 0; i < 128; i++) {
        printf(" [");
        for (j = 0; j < 256; j++)
            printf(" %3d", dset_data[i][j]);
        printf("]\n");
    }
#endif

    // HDputs(".");
    HDfflush(stdout);
    if (H5Dwrite(dset, H5T_STD_I32LE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset_data) < 0)
        TEST_ERROR;
    // HDputs(".");
    HDfflush(stdout);

    // HDputs(".");
    HDfflush(stdout);

    /*
     * CLEANUP
     */

    if (H5Dclose(dset) < 0)
        TEST_ERROR
    dset = H5I_INVALID_HID;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR
    file_id = H5I_INVALID_HID;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    fapl_id = H5I_INVALID_HID;

    // Read back data to check for validtiy
    onion_info.revision_id = 0;
    fapl_id                = H5Pcreate(H5P_FILE_ACCESS);
    if (H5I_INVALID_HID == fapl_id)
        TEST_ERROR;
    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;

    HDputs(".");
    fflush(stdout);

    file_id = H5Fopen(paths->canon, H5F_ACC_RDONLY, fapl_id);

    HDputs(".");
    fflush(stdout);
    if (H5I_INVALID_HID == file_id) {
        TEST_ERROR;
    }
    HDputs(".");
    HDfflush(stdout);

    ///
    dset = H5Dopen(file_id, "DS1", H5P_DEFAULT);
    if (dset < 0) {
        TEST_ERROR
    }

    HDputs(".");
#if 0
    HDputs("\n\nREADING\n\n");
#endif
    HDfflush(stdout);
    int rdata[128][256];
    if (H5Dread(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata[0]) < 0)
        TEST_ERROR
#if 0
    HDputs("\n\nDONE READING\n\n");
#endif
    HDfflush(stdout);

#if 0
    for (i = 0; i < 128; i++) {
        printf(" [");
        for (j = 0; j < 256; j++)
            printf(" %3d", rdata[i][j]);
        printf("]\n");
    }
#endif

    for (i = 0; i < 128; i++) {
        for (j = 0; j < 256; j++) {
            // printf("i: %d, j: %d\n", i, j);
            // int expected = i * 3 + j + 5;
            // int expected = i * 6 + j + 1;
            int expected = i * j - j;
            if (rdata[i][j] != expected) {
                printf("ERROR!!! Expected: %d, Got: %d\n", expected, rdata[i][j]);
                HDfflush(stdout);
                TEST_ERROR
            }
            else {
                // printf("Expected: %d, Got: %d\n", expected, rdata[i][j]);
                HDfflush(stdout);
            }
        }
    }

    if (H5Dclose(dset) < 0)
        TEST_ERROR
    dset = H5I_INVALID_HID;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR
    file_id = H5I_INVALID_HID;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    fapl_id = H5I_INVALID_HID;
    /////
    // onion_info.revision_id = 2;
    onion_info.revision_id = 1;
    fapl_id                = H5Pcreate(H5P_FILE_ACCESS);
    if (H5I_INVALID_HID == fapl_id)
        TEST_ERROR;
    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;

    HDputs(".");
    fflush(stdout);

    file_id = H5Fopen(paths->canon, H5F_ACC_RDONLY, fapl_id);

    HDputs(".");
    fflush(stdout);
    if (H5I_INVALID_HID == file_id) {
        TEST_ERROR;
    }
    HDputs(".");
    HDfflush(stdout);

    ///
    dset = H5Dopen(file_id, "DS1", H5P_DEFAULT);
    if (dset < 0) {
        TEST_ERROR
    }

    HDputs(".");
#if 0
    HDputs("\n\nREADING\n\n");
#endif
    HDfflush(stdout);
    if (H5Dread(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata[0]) < 0)
        TEST_ERROR
#if 0
    HDputs("\n\nDONE READING\n\n");
#endif
    HDfflush(stdout);

#if 0
    for (i = 0; i < 128; i++) {
        printf(" [");
        for (j = 0; j < 256; j++)
            printf(" %3d", rdata[i][j]);
        printf("]\n");
    }
#endif

    for (i = 0; i < 128; i++) {
        for (j = 0; j < 256; j++) {
            // printf("i: %d, j: %d\n", i, j);
            // int expected = i * 3 + j + 5;
            int expected = i * 6 + j + 1;
            if (rdata[i][j] != expected) {
                printf("ERROR!!! Expected: %d, Got: %d\n", expected, rdata[i][j]);
                HDfflush(stdout);
                TEST_ERROR
            }
            else {
                // printf("Expected: %d, Got: %d\n", expected, rdata[i][j]);
                HDfflush(stdout);
            }
        }
    }

    if (H5Dclose(dset) < 0)
        TEST_ERROR
    dset = H5I_INVALID_HID;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR
    file_id = H5I_INVALID_HID;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    fapl_id = H5I_INVALID_HID;

    /////
    onion_info.revision_id = 2;
    // onion_info.revision_id = 1;
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    if (H5I_INVALID_HID == fapl_id)
        TEST_ERROR;
    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;

    // HDputs(".");
    fflush(stdout);

    file_id = H5Fopen(paths->canon, H5F_ACC_RDONLY, fapl_id);

    // HDputs(".");
    fflush(stdout);
    if (H5I_INVALID_HID == file_id) {
        TEST_ERROR;
    }
    // HDputs(".");
    HDfflush(stdout);

    ///
    dset = H5Dopen(file_id, "DS1", H5P_DEFAULT);
    if (dset < 0) {
        TEST_ERROR
    }

    // HDputs(".");
#if 0
    HDputs("\n\nREADING\n\n");
#endif
    HDfflush(stdout);
    if (H5Dread(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata[0]) < 0)
        TEST_ERROR
#if 0
    HDputs("\n\nDONE READING\n\n");
#endif
    HDfflush(stdout);

#if 0
    for (i = 0; i < 128; i++) {
        printf(" [");
        for (j = 0; j < 256; j++)
            printf(" %3d", rdata[i][j]);
        printf("]\n");
    }
#endif

    for (i = 0; i < 128; i++) {
        for (j = 0; j < 256; j++) {
            // printf("i: %d, j: %d\n", i, j);
            int expected = i * 3 + j + 5;
            // int expected = i * 6 + j + 1;
            if (rdata[i][j] != expected) {
                printf("ERROR!!! Expected: %d, Got: %d\n", expected, rdata[i][j]);
                HDfflush(stdout);
                TEST_ERROR
            }
            else {
                // printf("Expected: %d, Got: %d\n", expected, rdata[i][j]);
                HDfflush(stdout);
            }
        }
    }

    if (H5Dclose(dset) < 0)
        TEST_ERROR
    dset = H5I_INVALID_HID;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR
    file_id = H5I_INVALID_HID;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    fapl_id = H5I_INVALID_HID;

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

    if (dset != H5I_INVALID_HID)
        (void)H5Dclose(dset);
    if (file_id != H5I_INVALID_HID)
        (void)H5Fclose(file_id);
    if (fapl_id != H5I_INVALID_HID)
        (void)H5Pclose(fapl_id);

    return -1;
} /* end test_integration_create() */

static int
test_integration_create_simple(void)
{
    const char *            basename   = "integration.h5";
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
    hid_t file_id = H5I_INVALID_HID;

    TESTING("onion-created one-dimensional HDF5 file with revisions");

    /*********
     * SETUP *
     *********/

    onion_info.backing_fapl_id = h5_fileaccess();
    fapl_id                    = H5Pcreate(H5P_FILE_ACCESS);
    if (H5I_INVALID_HID == fapl_id)
        TEST_ERROR;
    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;

    paths = onion_filepaths_init(basename, &onion_info);
    if (NULL == paths)
        TEST_ERROR;

    HDremove(paths->canon);
    HDremove(paths->onion);
    HDremove(paths->recovery);

    /* Create skeleton file */

    // CREATE FILE WITHOUT ONION

    hid_t   file, space, dset, dcpl; /* Handles */
    hsize_t dims[2] = {1, 1024}, maxdims[2] = {1, 1024};
    int     wdata[1][1024], /* Write buffer */
        fillval, i, j;

    /*
     * Initialize data.
     */
    for (i = 0; i < 1024; i++)
        wdata[0][i] = i;

    /*
     * Create a new file using the default properties.
     */
    // file = H5Fcreate ("example.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    file = H5Fcreate(paths->canon, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create dataspace with unlimited dimensions.
     */
    space = H5Screate_simple(2, dims, maxdims);

    /*
     * Create the dataset creation property list
     */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);

    /*
     * Set the fill value for the dataset.
     */
    fillval = 99;
    if (H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval) < 0)
        TEST_ERROR

    /*
     * Set the allocation time to "early".  This way we can be sure
     * that reading from the dataset immediately after creation will
     * return the fill value.
     */
    if (H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0)
        TEST_ERROR

    /*
     * Create the dataset using the dataset creation property list.
     */
    dset = H5Dcreate(file, "DS1", H5T_STD_I32LE, space, H5P_DEFAULT, dcpl, H5P_DEFAULT);

    /*
     * Write the data to the dataset.
     */
    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata[0]) < 0)
        TEST_ERROR

    /*
     * Close and release resources.
     */
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR
    if (H5Dclose(dset) < 0)
        TEST_ERROR
    if (H5Sclose(space) < 0)
        TEST_ERROR
    if (H5Fclose(file) < 0)
        TEST_ERROR

    ////////////////////////////

    // HDputs(".");
    fflush(stdout);

    file_id = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id);
    if (H5I_INVALID_HID == file_id) {
        TEST_ERROR;
    }

    // HDputs(".");
    HDfflush(stdout);

    dset = H5Dopen(file_id, "DS1", H5P_DEFAULT);
    if (dset < 0) {
        TEST_ERROR
    }

    // HDputs(".");
    HDfflush(stdout);
    int dset_data[1][1024];
    for (i = 0; i < 1024; i++)
        dset_data[0][i] = i + 1024;

#if 0
    printf("First revision\n");
    for (i = 0; i < 1; i++) {
        printf(" [");
        for (j = 0; j < 1024; j++)
            printf(" %3d", dset_data[i][j]);
        printf("]\n");
    }
#endif

    // HDputs(".");
    HDfflush(stdout);
    if (H5Dwrite(dset, H5T_STD_I32LE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset_data) < 0)
        TEST_ERROR;
    // HDputs(".");
    HDfflush(stdout);

    // HDputs(".");
    HDfflush(stdout);

    if (H5Dclose(dset) < 0)
        TEST_ERROR
    dset = H5I_INVALID_HID;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR
    file_id = H5I_INVALID_HID;

    /*
     * Create second revision
     */
    // HDputs(".");
    fflush(stdout);

    file_id = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id);
    if (H5I_INVALID_HID == file_id) {
        printf("\n\n\n\nERROR OPENING\n\n\n\n");
        TEST_ERROR;
    }

    // HDputs(".");
    HDfflush(stdout);

    dset = H5Dopen(file_id, "DS1", H5P_DEFAULT);
    if (dset < 0) {
        printf("\n\n\n\nERROR OPENING DSET\n\n\n\n");
        TEST_ERROR
    }

    // HDputs(".");
    HDfflush(stdout);
    for (i = 0; i < 1024; i++)
        dset_data[0][i] = i + 2048;

#if 0
    printf("Second revision\n");
    for (i = 0; i < 1; i++) {
        printf(" [");
        for (j = 0; j < 1024; j++)
            printf(" %3d", dset_data[i][j]);
        printf("]\n");
    }
#endif

    // HDputs(".");
    HDfflush(stdout);
    if (H5Dwrite(dset, H5T_STD_I32LE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset_data) < 0)
        TEST_ERROR;
    // HDputs(".");
    HDfflush(stdout);

    // HDputs(".");
    HDfflush(stdout);

    /*
     * CLEANUP
     */

    if (H5Dclose(dset) < 0)
        TEST_ERROR
    dset = H5I_INVALID_HID;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR
    file_id = H5I_INVALID_HID;

    /*
     * Create third revision
     */

    // HDputs(".");
    fflush(stdout);

    file_id = H5Fopen(paths->canon, H5F_ACC_RDWR, fapl_id);
    if (H5I_INVALID_HID == file_id) {
        printf("\n\n\n\nERROR OPENING\n\n\n\n");
        TEST_ERROR;
    }

    // HDputs(".");
    HDfflush(stdout);

    dset = H5Dopen(file_id, "DS1", H5P_DEFAULT);
    if (dset < 0) {
        printf("\n\n\n\nERROR OPENING DSET\n\n\n\n");
        TEST_ERROR
    }

    // HDputs(".");
    HDfflush(stdout);
    for (i = 0; i < 1024; i += 20)
        dset_data[0][i] = i + 3072;

#if 0
    printf("Third revision\n");
    for (i = 0; i < 1; i++) {
        printf(" [");
        for (j = 0; j < 1024; j++)
            printf(" %3d", dset_data[i][j]);
        printf("]\n");
    }
#endif

    // HDputs(".");
    HDfflush(stdout);
    if (H5Dwrite(dset, H5T_STD_I32LE, H5S_ALL, H5S_ALL, H5P_DEFAULT, dset_data) < 0)
        TEST_ERROR;
    // HDputs(".");
    HDfflush(stdout);

    // HDputs(".");
    HDfflush(stdout);

    /*
     * CLEANUP
     */

    if (H5Dclose(dset) < 0)
        TEST_ERROR
    dset = H5I_INVALID_HID;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR
    file_id = H5I_INVALID_HID;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    fapl_id = H5I_INVALID_HID;

    /////////////////////////

    // Read back data to check for validtiy
    // onion_info.revision_id = 3;
    onion_info.revision_id = 2;
    // onion_info.revision_id = 1;
    // onion_info.revision_id = 0;
    fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    if (H5I_INVALID_HID == fapl_id)
        TEST_ERROR;
    if (H5Pset_fapl_onion(fapl_id, &onion_info) < 0)
        TEST_ERROR;

    // HDputs(".");
    fflush(stdout);

    file_id = H5Fopen(paths->canon, H5F_ACC_RDONLY, fapl_id);

    // HDputs(".");
    fflush(stdout);
    if (H5I_INVALID_HID == file_id) {
        printf("\n\n\n\nERROR OPENING\n\n\n\n");
        TEST_ERROR;
    }
    // HDputs(".");
    HDfflush(stdout);

    dset = H5Dopen(file_id, "DS1", H5P_DEFAULT);
    if (dset < 0) {
        printf("\n\n\n\nERROR OPENING DSET\n\n\n\n");
        TEST_ERROR
    }

    // HDputs(".");
#if 0
    HDputs("\n\nREADING\n\n");
#endif
    HDfflush(stdout);
    int rdata[1][1024];
    if (H5Dread(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata[0]) < 0)
        TEST_ERROR
#if 0
    HDputs("\n\nDONE READING\n\n");
#endif
    HDfflush(stdout);

#if 0
    for (i = 0; i < 1; i++) {
        printf(" [");
        for (j = 0; j < 1024; j++)
            printf(" %3d", rdata[i][j]);
        printf("]\n");
    }
#endif

    for (i = 0; i < 1; i++) {
        // for (j = 0; j < 1024; j++) {
        for (j = 0; j < 1024; j += 20) {
            // printf("i: %d, j: %d\n", i, j);
            // int expected = j;
            // int expected = j + 1024;
            int expected = j + 2048;
            // int expected = j + 3072;
            if (rdata[i][j] != expected) {
                printf("ERROR!!! Expected: %d, Got: %d\n", expected, rdata[i][j]);
                HDfflush(stdout);
                TEST_ERROR
            }
            else {
                // printf("Expected: %d, Got: %d\n", expected, rdata[i][j]);
                HDfflush(stdout);
            }
        }
    }

    if (H5Dclose(dset) < 0)
        TEST_ERROR
    dset = H5I_INVALID_HID;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR
    file_id = H5I_INVALID_HID;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    fapl_id = H5I_INVALID_HID;

    // HDremove(paths->canon);
    // HDremove(paths->onion);
    // HDremove(paths->recovery);
    // onion_filepaths_destroy(paths);

    PASSED();
    return 0;

error:

    if (paths != NULL) {
        // HDremove(paths->canon);
        // HDremove(paths->onion);
        // HDremove(paths->recovery);
        // onion_filepaths_destroy(paths);
    }

    if (dset != H5I_INVALID_HID)
        (void)H5Dclose(dset);
    if (file_id != H5I_INVALID_HID)
        (void)H5Fclose(file_id);
    if (fapl_id != H5I_INVALID_HID)
        (void)H5Pclose(fapl_id);

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
    int nerrors = 0;

    HDprintf("Testing Onion VFD functionality.\n");

    h5_reset();

    /* initialize */
    flags_create_s = H5F_ACC_RDWR | H5F_ACC_CREAT | H5F_ACC_TRUNC;

    /* Run tests. Return values on error are negative. */
    nerrors -= test_archival_index();
    nerrors -= test_revision_index();
    nerrors -= test_revision_index_collisions();
    nerrors -= test_revision_index_resizing();
    nerrors -= test_revision_index_to_archival_index();
    nerrors -= test_fapl();
    nerrors -= test_header_encode_decode();
    nerrors -= test_whole_history_encode_decode_empty();
    nerrors -= test_whole_history_encode_decode();
    nerrors -= test_revision_record_encode_decode();
    nerrors -= test_create_oniontarget(FALSE, FALSE);
    nerrors -= test_create_oniontarget(TRUE, FALSE);
    nerrors -= test_create_oniontarget(FALSE, TRUE);
    nerrors -= test_create_oniontarget(TRUE, TRUE);
    nerrors -= test_several_revisions_with_logical_gaps();
    nerrors -= test_page_aligned_history_create();
    nerrors -= test_integration_create();
    nerrors -= test_integration_create_simple();

#if H5FD_ONION_ENABLE_INDEX_STATS
    nerrors -= test_working_index_stats(); /* TODO */
#endif                                     /* H5FD_ONION_ENABLE_INDEX_STATS */

    if (nerrors > 0) {
        HDprintf("***** %d Onion TEST%s FAILED! *****\n", nerrors, nerrors > 1 ? "S" : "");
        nerrors = 1;
    }
    else {
        HDprintf("All Onion tests passed.\n");
    }
    return nerrors; /* 0 if no errors, 1 if any errors */

} /* end main() */
