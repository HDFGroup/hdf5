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

#include "h5repack.h"
#include "h5test.h"
#include "h5diff.h"
#include "h5tools.h"
#include "h5tools_utils.h"
#include "h5repackgentest.h"

/* Name of tool */
#define PROGRAMNAME "h5repacktst"

#define GOERROR                                                                                              \
    do {                                                                                                     \
        H5_FAILED();                                                                                         \
        goto error;                                                                                          \
    } while (0)

/*-------------------------------------------------------------------------
 * blob-05: h5repack cross-file copy of a blob-configured dataset must
 * produce an output file whose blob lives at a new, independent on-disk
 * locator, not the source file's locator (RFC-HDFG-2026-003).  The
 * H5Z_blob_loc_t a filter receives is opaque to plugin authors, so a
 * custom filter's write_blob/read_blob callbacks are the only way to
 * observe it; they log each call's locator to these static variables.
 *-------------------------------------------------------------------------
 */
#define REPACK_BLOB_FILTER_ID 530
#define REPACK_BLOB_STORE_MAX 256
#define REPACK_BLOB_LOG_MAX   8

static unsigned char  repack_blob_store[REPACK_BLOB_STORE_MAX];
static size_t         repack_blob_store_size = 0;
static H5Z_blob_loc_t repack_blob_write_log[REPACK_BLOB_LOG_MAX];
static int            repack_blob_write_count = 0;
static int            repack_blob_read_count  = 0;

static size_t
repack_blob_filter(unsigned int H5_ATTR_UNUSED flags, size_t H5_ATTR_UNUSED cd_nelmts,
                   const unsigned int H5_ATTR_UNUSED *cd_values, hid_t H5_ATTR_UNUSED dxpl_id,
                   const hsize_t H5_ATTR_UNUSED *scaled, size_t H5_ATTR_UNUSED ndims, size_t nbytes,
                   size_t H5_ATTR_UNUSED *buf_size, void H5_ATTR_UNUSED **buf)
{
    return nbytes; /* pass-through */
}

static herr_t
repack_blob_write(hid_t file_id, const void *buf, size_t size, H5Z_blob_loc_t *loc_out)
{
    if (H5Iget_type(file_id) != H5I_FILE)
        return FAIL;
    if (size > sizeof(repack_blob_store))
        return FAIL;
    memcpy(repack_blob_store, buf, size);
    repack_blob_store_size = size;

    /* Each call gets a distinct token, so source-create vs. h5repack's
     * destination-create are distinguishable in the log below. */
    loc_out->addr = (haddr_t)(0x9000 + repack_blob_write_count);
    loc_out->idx  = (size_t)repack_blob_write_count;
    if (repack_blob_write_count < REPACK_BLOB_LOG_MAX)
        repack_blob_write_log[repack_blob_write_count] = *loc_out;
    repack_blob_write_count++;
    return SUCCEED;
}

static herr_t
repack_blob_read(hid_t file_id, H5Z_blob_loc_t H5_ATTR_UNUSED loc, void **buf_out, size_t *size_out)
{
    if (H5Iget_type(file_id) != H5I_FILE)
        return FAIL;
    if (NULL == (*buf_out = malloc(repack_blob_store_size)))
        return FAIL;
    memcpy(*buf_out, repack_blob_store, repack_blob_store_size);
    *size_out = repack_blob_store_size;
    repack_blob_read_count++;
    return SUCCEED;
}

static herr_t
repack_blob_close(void *buf, size_t H5_ATTR_UNUSED size)
{
    free(buf);
    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function: main
 *
 * Purpose:  Executes h5repack tests
 *
 * Return:   Success: zero
 *           Failure: 1
 *-------------------------------------------------------------------------
 */

int
main(void)
{
    pack_opt_t pack_options;
    diff_opt_t diff_options;

    unsigned    j     = 0;    /* Local index variable for testing file space */
    const char *fname = NULL; /* File name for testing file space */

    h5_stat_t      file_stat;
    h5_stat_size_t fsize1, fsize2; /* file sizes */
    bool           driver_is_parallel;
    hid_t          fapl_id = H5I_INVALID_HID;

#if defined(H5_HAVE_FILTER_SZIP)
    int szip_can_encode = 0;
#endif

    h5tools_setprogname(PROGRAMNAME);
    h5tools_setstatus(EXIT_SUCCESS);

    /* Initialize h5tools lib */
    h5tools_init();

    /* initialize */
    memset(&diff_options, 0, sizeof(diff_opt_t));
    memset(&pack_options, 0, sizeof(pack_opt_t));

    /* run tests  */
    puts("Testing h5repack:");

    /* make the test files */
    TESTING("    generating files for testing");
    if (make_h5repack_testfiles() < 0)
        GOERROR;
    PASSED();

    if (h5_using_parallel_driver(H5P_DEFAULT, &driver_is_parallel) < 0)
        GOERROR;

    /*-------------------------------------------------------------------------
     * Format of the tests:
     *
     * 1) make a copy of the file with h5repack
     * 2) use the h5diff function to compare the input and output file
     *-------------------------------------------------------------------------
     */
    /*-------------------------------------------------------------------------
     * Testing file space info setting
     *-------------------------------------------------------------------------
     */
    TESTING("    files with file space info setting--no options (-S, -P, -T, -G) are set");
    j = 0; /* #0 */
    assert(j < NELMTS(H5REPACK_FSPACE_FNAMES));
    fname = H5REPACK_FSPACE_FNAMES[j];
    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;

    if (h5repack(fname, FSPACE_OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(fname, FSPACE_OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(fname, FSPACE_OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;
    PASSED();

    TESTING("    files with file space info setting--all options -S, -P, -T, -G are set");
    ++j; /* #1 */
    assert(j < NELMTS(H5REPACK_FSPACE_FNAMES));
    fname = H5REPACK_FSPACE_FNAMES[j];
    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    pack_options.fs_strategy  = H5F_FSPACE_STRATEGY_NONE;
    pack_options.fs_persist   = -1; /* "false" is set via -P 0 */
    pack_options.fs_threshold = 1;
    pack_options.fs_pagesize  = 8192;
    if (h5repack(fname, FSPACE_OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(fname, FSPACE_OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(fname, FSPACE_OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;
    PASSED();

    TESTING("    files with file space info setting--options -S and -T are set");
    ++j; /* #2 */
    assert(j < NELMTS(H5REPACK_FSPACE_FNAMES));
    fname = H5REPACK_FSPACE_FNAMES[j];
    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    pack_options.fs_strategy  = (H5F_fspace_strategy_t)-1; /* "FSM_AGGR" specified via -S FSM_AGGR */
    pack_options.fs_threshold = -1;                        /* "0" specified via -T 0 */
    if (h5repack(fname, FSPACE_OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(fname, FSPACE_OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(fname, FSPACE_OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;
    PASSED();

    if (h5_using_default_driver(NULL)) {
        TESTING("    files with file space info setting-- options -S and -P are set & -L");
        ++j; /* #3 */
        assert(j < NELMTS(H5REPACK_FSPACE_FNAMES));
        fname = H5REPACK_FSPACE_FNAMES[j];
        if (h5repack_init(&pack_options, 0, true) < 0)
            GOERROR;
        pack_options.fs_strategy = H5F_FSPACE_STRATEGY_PAGE; /* "PAGE" specified via -S */
        pack_options.fs_persist  = true;
        if (h5repack(fname, FSPACE_OUT, &pack_options) < 0)
            GOERROR;
        if (h5diff(fname, FSPACE_OUT, NULL, NULL, &diff_options) > 0)
            GOERROR;
        if (h5repack_verify(fname, FSPACE_OUT, &pack_options) <= 0)
            GOERROR;
        if (h5repack_end(&pack_options) < 0)
            GOERROR;
        PASSED();

        TESTING("    files with file space info setting-- options -P and -T are set & -L");
        ++j; /* #4 */
        assert(j < NELMTS(H5REPACK_FSPACE_FNAMES));
        fname = H5REPACK_FSPACE_FNAMES[j];
        if (h5repack_init(&pack_options, 0, true) < 0)
            GOERROR;
        pack_options.fs_persist   = -1; /* "false" is set via -P 0 */
        pack_options.fs_threshold = 2;
        if (h5repack(fname, FSPACE_OUT, &pack_options) < 0)
            GOERROR;
        if (h5diff(fname, FSPACE_OUT, NULL, NULL, &diff_options) > 0)
            GOERROR;
        if (h5repack_verify(fname, FSPACE_OUT, &pack_options) <= 0)
            GOERROR;
        if (h5repack_end(&pack_options) < 0)
            GOERROR;
        PASSED();

        TESTING("    files with file space info setting-- options -S and -G are set & -L");
        ++j; /* #5 */
        assert(j < NELMTS(H5REPACK_FSPACE_FNAMES));
        fname = H5REPACK_FSPACE_FNAMES[j];
        if (h5repack_init(&pack_options, 0, true) < 0)
            GOERROR;
        pack_options.fs_strategy = H5F_FSPACE_STRATEGY_PAGE;
        pack_options.fs_pagesize = 8192;
        if (h5repack(fname, FSPACE_OUT, &pack_options) < 0)
            GOERROR;
        if (h5diff(fname, FSPACE_OUT, NULL, NULL, &diff_options) > 0)
            GOERROR;
        if (h5repack_verify(fname, FSPACE_OUT, &pack_options) <= 0)
            GOERROR;
        if (h5repack_end(&pack_options) < 0)
            GOERROR;
        PASSED();

        TESTING("    files with file space info setting-- options -S, -P, -T, -G are set");
        ++j; /* #6 */
        assert(j < NELMTS(H5REPACK_FSPACE_FNAMES));
        fname = H5REPACK_FSPACE_FNAMES[j];
        if (h5repack_init(&pack_options, 0, false) < 0)
            GOERROR;
        pack_options.fs_strategy  = H5F_FSPACE_STRATEGY_NONE;
        pack_options.fs_persist   = -1; /* "false" is set via -P 0 */
        pack_options.fs_threshold = 1;
        pack_options.fs_pagesize  = 8192;
        if (h5repack(fname, FSPACE_OUT, &pack_options) < 0)
            GOERROR;
        if (h5diff(fname, FSPACE_OUT, NULL, NULL, &diff_options) > 0)
            GOERROR;
        if (h5repack_verify(fname, FSPACE_OUT, &pack_options) <= 0)
            GOERROR;
        if (h5repack_end(&pack_options) < 0)
            GOERROR;
        PASSED();

        TESTING("    files with file space info setting-- options -S, -T, -G are set & -L");
        ++j; /* #7 */
        assert(j < NELMTS(H5REPACK_FSPACE_FNAMES));
        fname = H5REPACK_FSPACE_FNAMES[j];
        if (h5repack_init(&pack_options, 0, true) < 0)
            GOERROR;
        pack_options.fs_strategy  = H5F_FSPACE_STRATEGY_AGGR;
        pack_options.fs_threshold = 1;
        pack_options.fs_pagesize  = 4096;
        if (h5repack(fname, FSPACE_OUT, &pack_options) < 0)
            GOERROR;
        if (h5diff(fname, FSPACE_OUT, NULL, NULL, &diff_options) > 0)
            GOERROR;
        if (h5repack_verify(fname, FSPACE_OUT, &pack_options) <= 0)
            GOERROR;
        if (h5repack_end(&pack_options) < 0)
            GOERROR;
        PASSED();
    }

    /*-------------------------------------------------------------------------
     * file with fill values
     *-------------------------------------------------------------------------
     */
    TESTING("    copy of datasets (fill values)");

    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME0, H5REPACK_FNAME0OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME0, H5REPACK_FNAME0OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME0, H5REPACK_FNAME0OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_cmp_pl(H5REPACK_FNAME0, H5REPACK_FNAME0OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;
    PASSED();

    /*-------------------------------------------------------------------------
     * file with all kinds of dataset datatypes
     *-------------------------------------------------------------------------
     */
    if (!driver_is_parallel) {
        TESTING("    copy of datasets (all datatypes)");
        if (h5repack_init(&pack_options, 0, false) < 0)
            GOERROR;
        if (h5repack(H5REPACK_FNAME1, H5REPACK_FNAME1OUT, &pack_options) < 0)
            GOERROR;
        if (h5diff(H5REPACK_FNAME1, H5REPACK_FNAME1OUT, NULL, NULL, &diff_options) > 0)
            GOERROR;
        if (h5repack_verify(H5REPACK_FNAME1, H5REPACK_FNAME1OUT, &pack_options) <= 0)
            GOERROR;
        if (h5repack_cmp_pl(H5REPACK_FNAME1, H5REPACK_FNAME1OUT, &pack_options) <= 0)
            GOERROR;
        if (h5repack_end(&pack_options) < 0)
            GOERROR;
        PASSED();
    }

    /*-------------------------------------------------------------------------
     * file with attributes
     *-------------------------------------------------------------------------
     */
    TESTING("    copy of datasets (attributes)");
    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME2, H5REPACK_FNAME2OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME2, H5REPACK_FNAME2OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME2, H5REPACK_FNAME2OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_cmp_pl(H5REPACK_FNAME2, H5REPACK_FNAME2OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;
    PASSED();

    /*-------------------------------------------------------------------------
     * file with hardlinks
     *-------------------------------------------------------------------------
     */
    TESTING("    copy of datasets (hardlinks)");
    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME3, H5REPACK_FNAME3OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME3, H5REPACK_FNAME3OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME3, H5REPACK_FNAME3OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_cmp_pl(H5REPACK_FNAME3, H5REPACK_FNAME3OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;

    PASSED();

    /*-------------------------------------------------------------------------
     * alloc early test
     *-------------------------------------------------------------------------
     */
    TESTING("    copy of allocation early file");
    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME5, H5REPACK_FNAME5OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME5, H5REPACK_FNAME5OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME5, H5REPACK_FNAME5OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;
    PASSED();

    /*-------------------------------------------------------------------------
     * the remaining files differ in the dcpl's
     *-------------------------------------------------------------------------
     */

    /*-------------------------------------------------------------------------
     * deflate
     *-------------------------------------------------------------------------
     */
    TESTING("    adding deflate filter (old_format)");

#ifdef H5_HAVE_FILTER_DEFLATE

    /*-------------------------------------------------------------------------
     * test an individual object option
     *-------------------------------------------------------------------------
     */

    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack_addfilter("dset1:GZIP=9", &pack_options) < 0)
        GOERROR;
    if (h5repack_addlayout("dset1:CHUNK=20x10", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;
    PASSED();
#else
    SKIPPED();
#endif

    TESTING("    adding deflate filter (new format)");
#ifdef H5_HAVE_FILTER_DEFLATE
    /*-------------------------------------------------------------------------
     * test an individual object option
     *     For new format, "dset1" should be using Fixed Array indexing
     *-------------------------------------------------------------------------
     */

    if (h5repack_init(&pack_options, 0, true) < 0)
        GOERROR;
    if (h5repack_addfilter("dset1:GZIP=9", &pack_options) < 0)
        GOERROR;
    if (h5repack_addlayout("dset1:CHUNK=20x10", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;
    PASSED();
#else
    SKIPPED();
#endif

    /*-------------------------------------------------------------------------
     * test all objects option
     *-------------------------------------------------------------------------
     */

    TESTING("    adding deflate filter to all");

#ifdef H5_HAVE_FILTER_DEFLATE

    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack_addfilter("GZIP=1", &pack_options) < 0)
        GOERROR;
    if (h5repack_addlayout("CHUNK=20x10", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;

    PASSED();
#else
    SKIPPED();
#endif

    /*-------------------------------------------------------------------------
     * SZIP
     *-------------------------------------------------------------------------
     */

    TESTING("    adding szip filter");

#if defined(H5_HAVE_FILTER_SZIP)
    if (h5tools_can_encode(H5Z_FILTER_SZIP) > 0)
        szip_can_encode = 1;

    /*-------------------------------------------------------------------------
     * test an individual object option
     *-------------------------------------------------------------------------
     */

    if (szip_can_encode) {
        if (h5repack_init(&pack_options, 0, false) < 0)
            GOERROR;
        if (h5repack_addfilter("dset2:SZIP=8,EC", &pack_options) < 0)
            GOERROR;
        if (h5repack_addlayout("dset2:CHUNK=20x10", &pack_options) < 0)
            GOERROR;
        if (h5repack(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) < 0)
            GOERROR;
        if (h5diff(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, NULL, NULL, &diff_options) > 0)
            GOERROR;
        if (h5repack_verify(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) <= 0)
            GOERROR;
        if (h5repack_end(&pack_options) < 0)
            GOERROR;

        PASSED();
    }
    else {
        SKIPPED();
    }
#else
    SKIPPED();
#endif

    /*-------------------------------------------------------------------------
     * test all objects option
     *-------------------------------------------------------------------------
     */
    TESTING("    adding szip filter to all");

#if defined(H5_HAVE_FILTER_SZIP)
    if (szip_can_encode) {
        if (h5repack_init(&pack_options, 0, false) < 0)
            GOERROR;
        if (h5repack_addfilter("SZIP=8,NN", &pack_options) < 0)
            GOERROR;
        if (h5repack(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) < 0)
            GOERROR;
        if (h5diff(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, NULL, NULL, &diff_options) > 0)
            GOERROR;
        if (h5repack_verify(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) <= 0)
            GOERROR;
        if (h5repack_end(&pack_options) < 0)
            GOERROR;

        PASSED();
    }
    else {
        SKIPPED();
    }
#else
    SKIPPED();
#endif

    TESTING("    adding shuffle filter");

    /*-------------------------------------------------------------------------
     * test an individual object option
     *-------------------------------------------------------------------------
     */

    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack_addfilter("dset1:SHUF", &pack_options) < 0)
        GOERROR;
    if (h5repack_addlayout("dset1:CHUNK=20x10", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;

    PASSED();

    /*-------------------------------------------------------------------------
     * test all objects option
     *-------------------------------------------------------------------------
     */

    TESTING("    adding shuffle filter to all");

    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack_addfilter("SHUF", &pack_options) < 0)
        GOERROR;
    if (h5repack_addlayout("CHUNK=20x10", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;

    PASSED();

    TESTING("    adding checksum filter");

    /*-------------------------------------------------------------------------
     * test an individual object option
     *-------------------------------------------------------------------------
     */
    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack_addfilter("dset1:FLET", &pack_options) < 0)
        GOERROR;
    if (h5repack_addlayout("dset1:CHUNK=20x10", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;

    PASSED();

    /*-------------------------------------------------------------------------
     * test all objects option
     *-------------------------------------------------------------------------
     */
    TESTING("    adding checksum filter to all");

    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack_addfilter("FLET", &pack_options) < 0)
        GOERROR;
    if (h5repack_addlayout("CHUNK=20x10", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;

    PASSED();

    TESTING("    filter queue fletcher, shuffle, deflate, szip");

    /*-------------------------------------------------------------------------
     * add some filters
     *-------------------------------------------------------------------------
     */

    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack_addlayout("dset1:CHUNK 20x10", &pack_options) < 0)
        GOERROR;
    if (h5repack_addfilter("dset1:FLET", &pack_options) < 0)
        GOERROR;
    if (h5repack_addfilter("dset1:SHUF", &pack_options) < 0)
        GOERROR;

#if defined(H5_HAVE_FILTER_SZIP)
    if (szip_can_encode) {
        if (h5repack_addfilter("dset1:SZIP=8,NN", &pack_options) < 0)
            GOERROR;
    }
#endif

#ifdef H5_HAVE_FILTER_DEFLATE
    if (h5repack_addfilter("dset1:GZIP=1", &pack_options) < 0)
        GOERROR;
#endif

    if (h5repack(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;

    PASSED();

    TESTING("    adding layout chunked (old format)");

    /*-------------------------------------------------------------------------
     * test an individual object option
     *-------------------------------------------------------------------------
     */

    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack_addlayout("dset1:CHUNK=20x10", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;
    PASSED();

    TESTING("    adding layout chunked (new format)");

    /*-------------------------------------------------------------------------
     * test an individual object option
     *     For new format, "dset1" should be using Fixed Array indexing
     *-------------------------------------------------------------------------
     */

    if (h5repack_init(&pack_options, 0, true) < 0)
        GOERROR;
    if (h5repack_addlayout("dset1:CHUNK=20x10", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;
    PASSED();

    /*-------------------------------------------------------------------------
     * test all objects option
     *-------------------------------------------------------------------------
     */
    TESTING("    adding layout chunked to all");

    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack_addlayout("CHUNK=20x10", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;

    PASSED();

    TESTING("    adding layout contiguous");

    /*-------------------------------------------------------------------------
     * test an individual object option
     *-------------------------------------------------------------------------
     */
    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack_addlayout("dset1:CONTI", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;

    PASSED();

    TESTING("    adding layout contiguous to all");

    /*-------------------------------------------------------------------------
     * test all objects option
     *-------------------------------------------------------------------------
     */
    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack_addlayout("CONTI", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;

    /*-------------------------------------------------------------------------
     * do the same test for a file with filters (chunked)
     *-------------------------------------------------------------------------
     */
    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack_addlayout("CONTI", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME8, H5REPACK_FNAME8OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME8, H5REPACK_FNAME8OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME8, H5REPACK_FNAME8OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;

    PASSED();

    TESTING("    adding layout compact");

    /*-------------------------------------------------------------------------
     * test an individual object option
     *-------------------------------------------------------------------------
     */

    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack_addlayout("dset1:COMPA", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;
    PASSED();

    TESTING("    adding layout compact to all");

    /*-------------------------------------------------------------------------
     * test all objects option
     *-------------------------------------------------------------------------
     */

    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack_addlayout("COMPA", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;

    PASSED();

    TESTING("    layout compact to contiguous conversion");

    /*-------------------------------------------------------------------------
     * layout compact to contiguous conversion
     *-------------------------------------------------------------------------
     */
    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack_addlayout("dset_compact:CONTI", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;
    PASSED();

    TESTING("    layout compact to chunk conversion");

    /*-------------------------------------------------------------------------
     * layout compact to chunk conversion
     *-------------------------------------------------------------------------
     */
    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack_addlayout("dset_compact:CHUNK=2x5", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;
    PASSED();

    TESTING("    layout compact to compact conversion");

    /*-------------------------------------------------------------------------
     * layout compact to compact conversion
     *-------------------------------------------------------------------------
     */
    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack_addlayout("dset_compact:COMPA", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;
    PASSED();

    TESTING("    layout contiguous to compact conversion");
    /*-------------------------------------------------------------------------
     * layout contiguous to compact conversion
     *-------------------------------------------------------------------------
     */
    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack_addlayout("dset_contiguous:COMPA", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;
    PASSED();

    TESTING("    layout contiguous to chunk conversion");
    /*-------------------------------------------------------------------------
     * layout contiguous to chunk conversion
     *-------------------------------------------------------------------------
     */
    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack_addlayout("dset_contiguous:CHUNK=3x6", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;
    PASSED();

    TESTING("    layout contiguous to contiguous conversion");

    /*-------------------------------------------------------------------------
     * layout contiguous to contiguous conversion
     *-------------------------------------------------------------------------
     */
    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack_addlayout("dset_contiguous:CONTI", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;
    PASSED();

    TESTING("    layout chunked to compact conversion");
    /*-------------------------------------------------------------------------
     * layout chunked to compact conversion
     *-------------------------------------------------------------------------
     */
    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack_addlayout("dset_chunk:COMPA", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;
    PASSED();

    TESTING("    layout chunked to contiguous conversion");

    /*-------------------------------------------------------------------------
     * layout chunked to contiguous conversion
     *-------------------------------------------------------------------------
     */
    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack_addlayout("dset_chunk:CONTI", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;
    PASSED();

    TESTING("    layout chunked to chunk conversion");
    /*-------------------------------------------------------------------------
     * layout chunked to chunked conversion
     *-------------------------------------------------------------------------
     */
    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack_addlayout("dset_chunk:CHUNK=18x13", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;
    PASSED();

    /*-------------------------------------------------------------------------
     * the following tests assume the input files have filters
     * H5REPACK_FNAME7
     * H5REPACK_FNAME8
     * H5REPACK_FNAME9
     * H5REPACK_FNAME10
     * H5REPACK_FNAME11
     *-------------------------------------------------------------------------
     */
    TESTING("    copy of szip filter");

#if defined(H5_HAVE_FILTER_SZIP)
    if (szip_can_encode) {
        if (h5repack_init(&pack_options, 0, false) < 0)
            GOERROR;
        if (h5repack(H5REPACK_FNAME7, H5REPACK_FNAME7OUT, &pack_options) < 0)
            GOERROR;
        if (h5diff(H5REPACK_FNAME7, H5REPACK_FNAME7OUT, NULL, NULL, &diff_options) > 0)
            GOERROR;
        if (h5repack_verify(H5REPACK_FNAME7, H5REPACK_FNAME7OUT, &pack_options) <= 0)
            GOERROR;
        if (h5repack_cmp_pl(H5REPACK_FNAME7, H5REPACK_FNAME7OUT, &pack_options) <= 0)
            GOERROR;
        if (h5repack_end(&pack_options) < 0)
            GOERROR;

        PASSED();
    }
    else {
        SKIPPED();
    }
#else
    SKIPPED();
#endif

    TESTING("    removing szip filter");

#if defined(H5_HAVE_FILTER_SZIP)
    if (szip_can_encode) {
        if (h5repack_init(&pack_options, 0, false) < 0)
            GOERROR;
        if (h5repack_addfilter("dset_szip:NONE", &pack_options) < 0)
            GOERROR;
        if (h5repack(H5REPACK_FNAME7, H5REPACK_FNAME7OUT, &pack_options) < 0)
            GOERROR;
        if (h5diff(H5REPACK_FNAME7, H5REPACK_FNAME7OUT, NULL, NULL, &diff_options) > 0)
            GOERROR;
        if (h5repack_verify(H5REPACK_FNAME7, H5REPACK_FNAME7OUT, &pack_options) <= 0)
            GOERROR;
        if (h5repack_end(&pack_options) < 0)
            GOERROR;

        PASSED();
    }
    else {
        SKIPPED();
    }
#else
    SKIPPED();
#endif

    TESTING("    copy of deflate filter");

#ifdef H5_HAVE_FILTER_DEFLATE
    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME8, H5REPACK_FNAME8OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME8, H5REPACK_FNAME8OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME8, H5REPACK_FNAME8OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;

    PASSED();
#else
    SKIPPED();
#endif

    TESTING("    removing deflate filter");

#ifdef H5_HAVE_FILTER_DEFLATE
    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack_addfilter("dset_deflate:NONE", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME8, H5REPACK_FNAME8OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME8, H5REPACK_FNAME8OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME8, H5REPACK_FNAME8OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;

    PASSED();
#else
    SKIPPED();
#endif

    TESTING("    copy of shuffle filter");

    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME9, H5REPACK_FNAME9OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME9, H5REPACK_FNAME9OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME9, H5REPACK_FNAME9OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;

    PASSED();

    TESTING("    removing shuffle filter");

    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack_addfilter("dset_shuffle:NONE", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME9, H5REPACK_FNAME9OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME9, H5REPACK_FNAME9OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME9, H5REPACK_FNAME9OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;

    PASSED();

    TESTING("    copy of fletcher filter");

    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME10, H5REPACK_FNAME10OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME10, H5REPACK_FNAME10OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME10, H5REPACK_FNAME10OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;

    PASSED();

    TESTING("    removing fletcher filter");

    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack_addfilter("dset_fletcher32:NONE", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME10, H5REPACK_FNAME10OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME10, H5REPACK_FNAME10OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME10, H5REPACK_FNAME10OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;

    PASSED();

    TESTING("    copy of nbit filter");

    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME12, H5REPACK_FNAME12OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME12, H5REPACK_FNAME12OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME12, H5REPACK_FNAME12OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;

    PASSED();

    TESTING("    removing nbit filter");

    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack_addfilter("dset_nbit:NONE", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME12, H5REPACK_FNAME12OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME12, H5REPACK_FNAME12OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME12, H5REPACK_FNAME12OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;

    PASSED();

    TESTING("    adding nbit filter");

    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack_addfilter("dset_int31:NBIT", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME12, H5REPACK_FNAME12OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME12, H5REPACK_FNAME12OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME12, H5REPACK_FNAME12OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;

    PASSED();

    TESTING("    copy of scaleoffset filter");

    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME13, H5REPACK_FNAME13OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME13, H5REPACK_FNAME13OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME13, H5REPACK_FNAME13OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;

    PASSED();

    TESTING("    removing scaleoffset filter");

    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack_addfilter("dset_scaleoffset:NONE", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME13, H5REPACK_FNAME13OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME13, H5REPACK_FNAME13OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME13, H5REPACK_FNAME13OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;

    PASSED();

    TESTING("    adding scaleoffset filter");

    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack_addfilter("dset_none:SOFF=31,IN", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME13, H5REPACK_FNAME13OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME13, H5REPACK_FNAME13OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME13, H5REPACK_FNAME13OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;

    PASSED();

    /*-------------------------------------------------------------------------
     * file with all filters
     *  dset_all
     *  dset_deflate
     *  dset_szip
     *  dset_shuffle
     *  dset_fletcher32
     *-------------------------------------------------------------------------
     */
    TESTING("    filter conversion from deflate to szip");

#if defined(H5_HAVE_FILTER_SZIP) && defined(H5_HAVE_FILTER_DEFLATE)

    if (szip_can_encode) {
        if (h5repack_init(&pack_options, 0, false) < 0)
            GOERROR;
        if (h5repack_addfilter("dset_deflate:SZIP=8,NN", &pack_options) < 0)
            GOERROR;
        if (h5repack(H5REPACK_FNAME11, H5REPACK_FNAME11OUT, &pack_options) < 0)
            GOERROR;
        if (h5diff(H5REPACK_FNAME11, H5REPACK_FNAME11OUT, NULL, NULL, &diff_options) > 0)
            GOERROR;
        if (h5repack_verify(H5REPACK_FNAME11, H5REPACK_FNAME11OUT, &pack_options) <= 0)
            GOERROR;
        if (h5repack_end(&pack_options) < 0)
            GOERROR;

        PASSED();
    }
    else {
        SKIPPED();
    }
#else
    SKIPPED();
#endif

    TESTING("    filter conversion from szip to deflate");

#if defined(H5_HAVE_FILTER_SZIP) && defined(H5_HAVE_FILTER_DEFLATE)

    if (szip_can_encode) {
        if (h5repack_init(&pack_options, 0, false) < 0)
            GOERROR;
        if (h5repack_addfilter("dset_szip:GZIP=1", &pack_options) < 0)
            GOERROR;
        if (h5repack(H5REPACK_FNAME11, H5REPACK_FNAME11OUT, &pack_options) < 0)
            GOERROR;
        if (h5diff(H5REPACK_FNAME11, H5REPACK_FNAME11OUT, NULL, NULL, &diff_options) > 0)
            GOERROR;
        if (h5repack_verify(H5REPACK_FNAME11, H5REPACK_FNAME11OUT, &pack_options) <= 0)
            GOERROR;
        if (h5repack_end(&pack_options) < 0)
            GOERROR;

        PASSED();
    }
    else {
        SKIPPED();
    }
#else
    SKIPPED();
#endif

    /*-------------------------------------------------------------------------
     * test the NONE global option
     *-------------------------------------------------------------------------
     */

    TESTING("    removing all filters");

#if defined(H5_HAVE_FILTER_SZIP) && defined(H5_HAVE_FILTER_DEFLATE)

    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack_addfilter("NONE", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME11, H5REPACK_FNAME11OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME11, H5REPACK_FNAME11OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME11, H5REPACK_FNAME11OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;

    PASSED();
#else
    SKIPPED();
#endif

    /*-------------------------------------------------------------------------
     * test a big file
     *-------------------------------------------------------------------------
     */
    TESTING("    big file");

    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME14, H5REPACK_FNAME14OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME14, H5REPACK_FNAME14OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME14, H5REPACK_FNAME14OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;
    PASSED();

    /*-------------------------------------------------------------------------
     * test external dataset
     *-------------------------------------------------------------------------
     */
    TESTING("    external datasets");
    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME15, H5REPACK_FNAME15OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME15, H5REPACK_FNAME15OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME15, H5REPACK_FNAME15OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;
    PASSED();

    if (h5_using_default_driver(NULL)) {
        /*-------------------------------------------------------------------------
         * test file with userblock
         *-------------------------------------------------------------------------
         */
        TESTING("    file with userblock");
        if (h5repack_init(&pack_options, 0, false) < 0)
            GOERROR;
        if (h5repack(H5REPACK_FNAME16, H5REPACK_FNAME16OUT, &pack_options) < 0)
            GOERROR;
        if (h5diff(H5REPACK_FNAME16, H5REPACK_FNAME16OUT, NULL, NULL, &diff_options) > 0)
            GOERROR;
        if (h5repack_verify(H5REPACK_FNAME16, H5REPACK_FNAME16OUT, &pack_options) <= 0)
            GOERROR;
        if (verify_userblock(H5REPACK_FNAME16OUT) < 0)
            GOERROR;
        if (h5repack_end(&pack_options) < 0)
            GOERROR;
        PASSED();
    }

    /*-------------------------------------------------------------------------
     * test --latest options
     *-------------------------------------------------------------------------
     */
    if (!driver_is_parallel) {
        TESTING("    latest file format options");
        if (h5repack_init(&pack_options, 0, false) < 0)
            GOERROR;
        pack_options.latest      = 1;
        pack_options.grp_compact = 10;
        pack_options.grp_indexed = 5;
        pack_options.msg_size[0] = 10;
        pack_options.msg_size[1] = 20;
        pack_options.msg_size[2] = 30;
        pack_options.msg_size[3] = 40;
        pack_options.msg_size[4] = 50;
        if (h5repack(H5REPACK_FNAME1, H5REPACK_FNAME1OUT, &pack_options) < 0)
            GOERROR;
        if (h5diff(H5REPACK_FNAME1, H5REPACK_FNAME1OUT, NULL, NULL, &diff_options) > 0)
            GOERROR;
        if (h5repack_verify(H5REPACK_FNAME1, H5REPACK_FNAME1OUT, &pack_options) <= 0)
            GOERROR;
        if (h5repack_end(&pack_options) < 0)
            GOERROR;
        PASSED();
    }

    /*-------------------------------------------------------------------------
     * test several global filters
     *-------------------------------------------------------------------------
     */

    TESTING("    several global filters");

#if defined(H5_HAVE_FILTER_DEFLATE)

    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;
    if (h5repack_addfilter("GZIP=1", &pack_options) < 0)
        GOERROR;
    if (h5repack_addfilter("SHUF", &pack_options) < 0)
        GOERROR;
    if (h5repack(H5REPACK_FNAME11, H5REPACK_FNAME11OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME11, H5REPACK_FNAME11OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME11, H5REPACK_FNAME11OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;

    PASSED();
#else
    SKIPPED();
#endif

    if (h5_using_default_driver(NULL)) {
        /*-------------------------------------------------------------------------
         * test file with userblock
         *-------------------------------------------------------------------------
         */
        TESTING("    file with added userblock");

#ifdef H5_HAVE_FILTER_DEFLATE

        if (h5repack_init(&pack_options, 0, false) < 0)
            GOERROR;

        /* add the options for a user block size and user block filename */
        pack_options.ublock_size     = USERBLOCK_SIZE;
        pack_options.ublock_filename = H5REPACK_FNAME_UB;

        if (h5repack(H5REPACK_FNAME8, H5REPACK_FNAME8OUT, &pack_options) < 0)
            GOERROR;
        if (h5diff(H5REPACK_FNAME8, H5REPACK_FNAME8OUT, NULL, NULL, &diff_options) > 0)
            GOERROR;
        if (h5repack_verify(H5REPACK_FNAME8, H5REPACK_FNAME8OUT, &pack_options) <= 0)
            GOERROR;
        if (verify_userblock(H5REPACK_FNAME8OUT) < 0)
            GOERROR;
        if (h5repack_end(&pack_options) < 0)
            GOERROR;

        PASSED();
#else
        SKIPPED();
#endif
    }

    /*-------------------------------------------------------------------------
     * test file with alignment
     *-------------------------------------------------------------------------
     */
    TESTING("    file with alignment");

#ifdef H5_HAVE_FILTER_DEFLATE

    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;

    /* add the options for alignment */
    pack_options.alignment = 1;
    pack_options.threshold = 1;

    if (h5repack(H5REPACK_FNAME8, H5REPACK_FNAME8OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME8, H5REPACK_FNAME8OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME8, H5REPACK_FNAME8OUT, &pack_options) <= 0)
        GOERROR;

    /* verify alignment */
    {
        hsize_t threshold;
        hsize_t alignment;
        hid_t   fapl;
        hid_t   fid;

        if ((fid = H5Fopen(H5REPACK_FNAME8OUT, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
            GOERROR;
        if ((fapl = H5Fget_access_plist(fid)) < 0)
            GOERROR;
        if (H5Pget_alignment(fapl, &threshold, &alignment) < 0)
            GOERROR;
        if (threshold != 1)
            GOERROR;
        if (alignment != 1)
            GOERROR;
        if (H5Pclose(fapl) < 0)
            GOERROR;
        if (H5Fclose(fid) < 0)
            GOERROR;
    }

    if (h5repack_end(&pack_options) < 0)
        GOERROR;

    PASSED();
#else
    SKIPPED();
#endif

    /*-------------------------------------------------------------------------
     * test file with userblock
     *-------------------------------------------------------------------------
     */
    TESTING("    file with committed datatypes");

    if (h5repack_init(&pack_options, 0, false) < 0)
        GOERROR;

    if (h5repack(H5REPACK_FNAME17, H5REPACK_FNAME17OUT, &pack_options) < 0)
        GOERROR;
    if (h5diff(H5REPACK_FNAME17, H5REPACK_FNAME17OUT, NULL, NULL, &diff_options) > 0)
        GOERROR;
    if (h5repack_verify(H5REPACK_FNAME17, H5REPACK_FNAME17OUT, &pack_options) <= 0)
        GOERROR;
    if (h5repack_end(&pack_options) < 0)
        GOERROR;

    PASSED();

    if (h5_using_default_driver(NULL)) {
        /*-------------------------------------------------------------------------
         * test --metadata_block_size option
         * Verify the option's effect on output file size across every valid
         * low library version bound. H5REPACK_FNAME4 is used because it is
         * the same as the test file used for the shell script version of
         * this test (h5repack.sh).
         *
         * The relationship between meta_block_size and the output file size
         * depends on the file's low library version bound:
         *   - H5F_LIBVER_EARLIEST: a larger meta_block_size grows the
         *     output file (more pre-allocated metadata space).
         *   - H5F_LIBVER_V18 and later: a larger meta_block_size shrinks
         *     the output file (better metadata aggregation reduces
         *     unaccounted padding).
         *-------------------------------------------------------------------------
         */
        TESTING("    metadata block size option");
        for (H5F_libver_t lb = H5F_LIBVER_EARLIEST; lb < H5F_LIBVER_NBOUNDS; lb++) {
            /* First run without metadata option. No need to verify the
             * correctness since this has been verified by earlier tests;
             * just record the output file size. */
            if (h5repack_init(&pack_options, 0, false) < 0)
                GOERROR;
            pack_options.low_bound  = lb;
            pack_options.high_bound = H5F_LIBVER_LATEST;
            if (h5repack(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) < 0)
                GOERROR;
            memset(&file_stat, 0, sizeof(h5_stat_t));
            if (HDstat(H5REPACK_FNAME4OUT, &file_stat) < 0)
                GOERROR;
            fsize1 = file_stat.st_size;
            if (h5repack_end(&pack_options) < 0)
                GOERROR;

            /* Second run with metadata option. */
            if (h5repack_init(&pack_options, 0, false) < 0)
                GOERROR;
            pack_options.low_bound       = lb;
            pack_options.high_bound      = H5F_LIBVER_LATEST;
            pack_options.meta_block_size = 8192;
            if (h5repack(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) < 0)
                GOERROR;
            if (h5diff(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, NULL, NULL, &diff_options) > 0)
                GOERROR;
            if (h5repack_verify(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) <= 0)
                GOERROR;
            /* record the file size of the output file */
            memset(&file_stat, 0, sizeof(h5_stat_t));
            if (HDstat(H5REPACK_FNAME4OUT, &file_stat) < 0)
                GOERROR;
            fsize2 = file_stat.st_size;
            /* Verify file-size ordering according to the low library
             * version bound. */
            if (lb == H5F_LIBVER_EARLIEST) {
                if (fsize2 <= fsize1)
                    GOERROR;
            }
            else {
                if (fsize2 >= fsize1)
                    GOERROR;
            }
            if (h5repack_end(&pack_options) < 0)
                GOERROR;
        }
        PASSED();
    }

    /*-------------------------------------------------------------------------
     * blob-05: h5repack cross-file copy of a blob-configured dataset
     *-------------------------------------------------------------------------
     */
    TESTING("    blob-configured dataset gets a new, independent locator");
    {
        static const H5Z_class3_t blob_cls = {
            2,                     /* version         */
            REPACK_BLOB_FILTER_ID, /* id              */
            1,                     /* encoder_present */
            1,                     /* decoder_present */
            "repack_blob_filter",  /* canonical_name  */
            NULL,                  /* can_apply       */
            NULL,                  /* set_local       */
            repack_blob_filter,    /* filter          */
            NULL,                  /* set_config      */
            NULL,                  /* get_config      */
            repack_blob_write,     /* write_blob      */
            repack_blob_read,      /* read_blob       */
            repack_blob_close,     /* close_blob      */
            NULL,                  /* description     */
        };
        const char   *blob_src = "h5repack_blob_src.h5";
        const char   *blob_out = "h5repack_blob_OUT.h5";
        unsigned char blob[64];
        hid_t         src = H5I_INVALID_HID, sid = H5I_INVALID_HID;
        hid_t         dcpl = H5I_INVALID_HID, dset = H5I_INVALID_HID;
        hid_t         dst = H5I_INVALID_HID, dcpl_out = H5I_INVALID_HID;
        hsize_t       dims[2] = {8, 8}, chunk[2] = {4, 4};
        int           wdata[8][8], rdata[8][8];
        unsigned char blob_out_buf[sizeof(blob)];
        size_t        blob_out_size = 0;

        repack_blob_write_count = 0;
        repack_blob_read_count  = 0;

        if (H5Zregister(&blob_cls) < 0)
            GOERROR;
        for (size_t i = 0; i < sizeof(blob); i++)
            blob[i] = (unsigned char)(i * 7 + 3);
        for (int r = 0; r < 8; r++)
            for (int c = 0; c < 8; c++)
                wdata[r][c] = r * 8 + c;

        if ((src = H5Fcreate(blob_src, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            GOERROR;
        if ((sid = H5Screate_simple(2, dims, NULL)) < 0)
            GOERROR;
        if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
            GOERROR;
        if (H5Pset_chunk(dcpl, 2, chunk) < 0)
            GOERROR;
        if (H5Pappend_filter_blob(dcpl, REPACK_BLOB_FILTER_ID, 0, blob, sizeof(blob)) < 0)
            GOERROR;
        if ((dset = H5Dcreate2(src, "dset", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            GOERROR;
        if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata) < 0)
            GOERROR;
        if (H5Dclose(dset) < 0)
            GOERROR;
        if (H5Sclose(sid) < 0)
            GOERROR;
        if (H5Pclose(dcpl) < 0)
            GOERROR;
        if (H5Fclose(src) < 0)
            GOERROR;

        /* Exactly one write_blob call so far: the source dataset's create. */
        if (repack_blob_write_count != 1)
            GOERROR;

        /* Force the rebuild path (H5Dget_create_plist -> H5Pcopy ->
         * H5Dcreate2) rather than h5repack's H5Ocopy fast path, so
         * write_blob fires again for the destination. An unfiltered
         * layout directive on the object is enough: apply_filters()
         * leaves an untouched filter pipeline (and its blob) alone via
         * H5Pcopy(dcpl_in). h5repack_verify() is skipped here: it only
         * knows how to check filters explicitly registered via
         * h5repack_addfilter(), not a user-defined blob filter, so it
         * would reject the (correctly) unmodified pipeline as a mismatch;
         * the checks below verify the actually interesting properties
         * directly instead. */
        if (h5repack_init(&pack_options, 0, false) < 0)
            GOERROR;
        if (h5repack_addlayout("dset:CHUNK=4x4", &pack_options) < 0)
            GOERROR;
        if (h5repack(blob_src, blob_out, &pack_options) < 0)
            GOERROR;
        if (h5repack_end(&pack_options) < 0)
            GOERROR;

        /* write_blob fired again for the destination, and read_blob fired
         * at least once (h5repack reading the source's blob to carry it
         * over). The destination's locator must differ from the source's:
         * a new, independent on-disk object, not a leaked reference to
         * the source file's heap address. */
        if (repack_blob_write_count != 2)
            GOERROR;
        if (repack_blob_read_count < 1)
            GOERROR;
        if (repack_blob_write_log[0].addr == repack_blob_write_log[1].addr &&
            repack_blob_write_log[0].idx == repack_blob_write_log[1].idx)
            GOERROR;

        /* The output file reads back correctly: data and blob both. */
        if ((dst = H5Fopen(blob_out, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
            GOERROR;
        if ((dset = H5Dopen2(dst, "dset", H5P_DEFAULT)) < 0)
            GOERROR;
        memset(rdata, 0, sizeof(rdata));
        if (H5Dread(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata) < 0)
            GOERROR;
        if (memcmp(wdata, rdata, sizeof(wdata)) != 0)
            GOERROR;
        if ((dcpl_out = H5Dget_create_plist(dset)) < 0)
            GOERROR;
        blob_out_size = sizeof(blob_out_buf);
        if (H5Pget_filter_blob(dcpl_out, 0, 0, blob_out_buf, &blob_out_size) < 0)
            GOERROR;
        if (blob_out_size != sizeof(blob) || memcmp(blob_out_buf, blob, sizeof(blob)) != 0)
            GOERROR;
        if (H5Pclose(dcpl_out) < 0)
            GOERROR;
        if (H5Dclose(dset) < 0)
            GOERROR;
        if (H5Fclose(dst) < 0)
            GOERROR;

        if (H5Zunregister(REPACK_BLOB_FILTER_ID) < 0)
            GOERROR;
        if (remove(blob_src) < 0 || remove(blob_out) < 0)
            GOERROR;
    }
    PASSED();

    /* Remove test files */
    TESTING("    test file cleanup");

    if ((fapl_id = h5_fileaccess()) < 0) {
        printf(" Failed to generate FAPL");
        GOERROR;
    }

    h5_delete_all_test_files(H5REPACK_TEST_H5_FILES, fapl_id);

    for (size_t i = 0; i < NELMTS(H5REPACK_FSPACE_FNAMES); i++) {
        h5_delete_test_file(H5REPACK_FSPACE_FNAMES[i], fapl_id);
    }

    /* Clean up default-driver exclusive files */
    if (h5_using_default_driver(NULL)) {
        for (size_t i = 0; i < NELMTS(H5REPACK_DEFAULT_DRIVER_FILES); i++) {
            h5_delete_test_file(H5REPACK_DEFAULT_DRIVER_FILES[i], fapl_id);
        }

        for (size_t i = 0; i < NELMTS(H5REPACK_DEFAULT_DRIVER_MISC_FILES); i++) {
            if (remove(H5REPACK_DEFAULT_DRIVER_MISC_FILES[i]) < 0) {
                printf(" Failed to delete %s", H5REPACK_DEFAULT_DRIVER_MISC_FILES[i]);
                GOERROR;
            }
        }
    }

    for (size_t i = 0; i < NELMTS(H5REPACK_TEST_MISC_FILES); i++) {
        if (remove(H5REPACK_TEST_MISC_FILES[i]) < 0) {
            printf(" Failed to delete %s", H5REPACK_TEST_MISC_FILES[i]);
            GOERROR;
        }
    }

    if (H5Pclose(fapl_id) < 0) {
        printf(" Failed to close FAPL");
        GOERROR;
    }

    PASSED();

    puts("All h5repack tests passed.");

    h5tools_close();

    return 0;

error:
    h5tools_close();

    if (fapl_id > 0)
        H5Pclose(fapl_id);

    puts("***** H5REPACK TESTS FAILED *****");

    return 1;
}
