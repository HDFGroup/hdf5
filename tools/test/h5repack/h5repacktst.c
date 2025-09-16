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
         * Also verify that output file using the metadata_block_size option is
         * larger than the output file one not using it.
         * H5REPACK_FNAME4 is used because it is the same as the test file used for the
         * shell script version of this test (h5repack.sh).
         *-------------------------------------------------------------------------
         */
        TESTING("    metadata block size option");
        /* First run without metadata option. No need to verify the correctness */
        /* since this has been verified by earlier tests. Just record the file */
        /* size of the output file. */
        if (h5repack_init(&pack_options, 0, false) < 0)
            GOERROR;
        if (h5repack(H5REPACK_FNAME4, H5REPACK_FNAME4OUT, &pack_options) < 0)
            GOERROR;
        memset(&file_stat, 0, sizeof(h5_stat_t));
        if (HDstat(H5REPACK_FNAME4OUT, &file_stat) < 0)
            GOERROR;
        fsize1 = file_stat.st_size;
        if (h5repack_end(&pack_options) < 0)
            GOERROR;

        /* run it again with metadata option */
        if (h5repack_init(&pack_options, 0, false) < 0)
            GOERROR;
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
        /* verify second file size is larger than the first one */
        if (fsize2 <= fsize1)
            GOERROR;
        if (h5repack_end(&pack_options) < 0)
            GOERROR;
        PASSED();
    }

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
