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
 * Tests for the string-based filter configuration API:
 *   - H5Pappend_filter / H5Pget_filter_params_by_idx
 *   - Typed TOML accessor functions (H5Zconfig_get_int, _get_str, etc.)
 *   - Built-in filter set_config / get_config round-trips
 *   - Name registry (H5Z_filter_id_by_name)
 *   - Regression: existing H5Pset_filter still works
 */

#include "h5test.h"

static const char *FILENAME[] = {"tfilter2", "tfilter2_cfg", "tfilter2_cfg_copy", NULL};

/* -----------------------------------------------------------------------
 * Parser tests - typed TOML accessor functions
 * ---------------------------------------------------------------------- */
static int
test_parser(void)
{
    char    vbuf[256];
    size_t  vsz;
    int64_t ival;
    double  dval;
    bool    bval;
    htri_t  ret;

    TESTING("H5Zconfig_get_int: basic integer lookup");
    ret = H5Zconfig_get_int("level = 6, mode = 2", "level", &ival);
    if (ret <= 0 || ival != 6)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: key not found");
    ret = H5Zconfig_get_int("level = 6", "mode", &ival);
    if (ret != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_has_key: key present");
    ret = H5Zconfig_has_key("level = 6, compress = true", "compress");
    if (ret <= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_has_key: key absent");
    ret = H5Zconfig_has_key("level = 6", "mode");
    if (ret != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_str: double-quoted value");
    vsz = sizeof(vbuf);
    ret = H5Zconfig_get_str("name = \"hello world\"", "name", vbuf, &vsz);
    if (ret <= 0 || strcmp(vbuf, "hello world") != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_str: single-quoted value");
    vsz = sizeof(vbuf);
    ret = H5Zconfig_get_str("name = 'hello world'", "name", vbuf, &vsz);
    if (ret <= 0 || strcmp(vbuf, "hello world") != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_bool: boolean true");
    ret = H5Zconfig_get_bool("compress = true", "compress", &bval);
    if (ret <= 0 || !bval)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_bool: boolean false");
    ret = H5Zconfig_get_bool("compress = false", "compress", &bval);
    if (ret <= 0 || bval)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_double: float value");
    ret = H5Zconfig_get_double("tol = 1.5", "tol", &dval);
    if (ret <= 0 || dval != 1.5)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: NULL params error");
    H5E_BEGIN_TRY
    {
        ret = H5Zconfig_get_int(NULL, "key", &ival);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: NULL key error");
    H5E_BEGIN_TRY
    {
        ret = H5Zconfig_get_int("level = 6", NULL, &ival);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: duplicate key error");
    H5E_BEGIN_TRY
    {
        ret = H5Zconfig_get_int("level = 6, level = 9", "level", &ival);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: whitespace around equals");
    ret = H5Zconfig_get_int("  level = 6 , mode = 2 ", "level", &ival);
    if (ret <= 0 || ival != 6)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: braced inline-table form");
    ret = H5Zconfig_get_int("{level = 6, mode = 2}", "level", &ival);
    if (ret <= 0 || ival != 6)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_str: braced inline-table form");
    vsz = sizeof(vbuf);
    ret = H5Zconfig_get_str("{ coding = \"entropy\" }", "coding", vbuf, &vsz);
    if (ret <= 0 || strcmp(vbuf, "entropy") != 0)
        TEST_ERROR;
    PASSED();

    /* Dotted-key form: nested inline tables addressed through a single
     * dotted accessor call. Both surface forms below must resolve
     * identically. */
    TESTING("H5Zconfig_get_str: dotted-key into nested table (dotted form)");
    vsz = sizeof(vbuf);
    ret = H5Zconfig_get_str("compressor.name = \"zlib\", shuffle = 1", "compressor.name", vbuf, &vsz);
    if (ret <= 0 || strcmp(vbuf, "zlib") != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: dotted-key into nested table (inline-table form)");
    ret = H5Zconfig_get_int("compressor = {name = \"zlib\", level = 6}", "compressor.level", &ival);
    if (ret <= 0 || ival != 6)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: top-level sibling alongside nested table");
    ret = H5Zconfig_get_int("compressor = {name = \"zlib\", level = 6}, shuffle = 1", "shuffle", &ival);
    if (ret <= 0 || ival != 1)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_str: missing dotted key returns 0");
    vsz = sizeof(vbuf);
    ret = H5Zconfig_get_str("compressor.name = \"zlib\"", "compressor.missing", vbuf, &vsz);
    if (ret != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_str: type mismatch error (integer key)");
    H5E_BEGIN_TRY
    {
        vsz = sizeof(vbuf);
        ret = H5Zconfig_get_str("level = 6", "level", vbuf, &vsz);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: negative integer");
    ret = H5Zconfig_get_int("offset = -4", "offset", &ival);
    if (ret <= 0 || ival != -4)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_double: scientific notation");
    ret = H5Zconfig_get_double("tol = 1.0e-6", "tol", &dval);
    if (ret <= 0 || dval < 9.9e-7 || dval > 1.1e-6)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_str: comma inside quoted value");
    vsz = sizeof(vbuf);
    ret = H5Zconfig_get_str("path = \"/data/run_1,v2/dict.bin\"", "path", vbuf, &vsz);
    if (ret <= 0 || strcmp(vbuf, "/data/run_1,v2/dict.bin") != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_str: backslash-quote escape in double-quoted value");
    vsz = sizeof(vbuf);
    ret = H5Zconfig_get_str("msg = \"say \\\"hi\\\"\"", "msg", vbuf, &vsz);
    if (ret <= 0 || strcmp(vbuf, "say \"hi\"") != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_has_key: empty string is valid (no params)");
    ret = H5Zconfig_has_key("", "level");
    if (ret != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_double: inf rejected");
    H5E_BEGIN_TRY
    {
        ret = H5Zconfig_get_double("tol = inf", "tol", &dval);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_double: nan rejected");
    H5E_BEGIN_TRY
    {
        ret = H5Zconfig_get_double("tol = nan", "tol", &dval);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: semicolon outside quotes rejected");
    H5E_BEGIN_TRY
    {
        ret = H5Zconfig_get_int("level = 6; mode = 2", "level", &ival);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: underscore digit separator");
    ret = H5Zconfig_get_int("count = 1_000_000", "count", &ival);
    if (ret <= 0 || ival != 1000000)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: hex prefix 0x");
    ret = H5Zconfig_get_int("flags = 0xff", "flags", &ival);
    if (ret <= 0 || ival != 255)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_double: hex-float 0x1.8p+1 == 3.0");
    ret = H5Zconfig_get_double("rate = 0x1.8p+1", "rate", &dval);
    if (ret <= 0 || dval != 3.0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_double: hex-float 0x1.cp+1 == 3.5");
    ret = H5Zconfig_get_double("rate = 0x1.cp+1", "rate", &dval);
    if (ret <= 0 || dval != 3.5)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_double: negative hex-float -0x1p-1 == -0.5");
    ret = H5Zconfig_get_double("offset = -0x1p-1", "offset", &dval);
    if (ret <= 0 || dval != -0.5)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_double: hex-float without fraction 0xAp0 == 10.0");
    ret = H5Zconfig_get_double("val = 0xAp0", "val", &dval);
    if (ret <= 0 || dval != 10.0)
        TEST_ERROR;
    PASSED();

    /* Verify that %a output round-trips exactly for a value that is not
     * representable exactly in decimal (0.1 requires hex-float to preserve
     * the exact IEEE 754 bit pattern through a serialize/parse cycle). */
    TESTING("H5Zconfig_get_double: %%a round-trip for non-decimal-exact value");
    {
        char   pstr[64];
        double orig = 0.1, rt;
        snprintf(pstr, sizeof(pstr), "rate = %a", orig);
        ret = H5Zconfig_get_double(pstr, "rate", &rt);
        if (ret <= 0 || orig != rt)
            TEST_ERROR;
    }
    PASSED();

    /* --- Malformed input ------------------------------------------------- */

    TESTING("H5Zconfig_get_int: missing '=' is a parse error");
    H5E_BEGIN_TRY
    {
        ret = H5Zconfig_get_int("level6", "level", &ival);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: empty value after '=' is a parse error");
    H5E_BEGIN_TRY
    {
        ret = H5Zconfig_get_int("level =", "level", &ival);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_str: unterminated double-quote is a parse error");
    H5E_BEGIN_TRY
    {
        vsz = sizeof(vbuf);
        ret = H5Zconfig_get_str("name = \"hello", "name", vbuf, &vsz);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_str: unterminated single-quote is a parse error");
    H5E_BEGIN_TRY
    {
        vsz = sizeof(vbuf);
        ret = H5Zconfig_get_str("name = 'hello", "name", vbuf, &vsz);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    /* --- Boolean edge cases ---------------------------------------------- */

    TESTING("H5Zconfig_get_bool: uppercase TRUE is rejected (TOML case-sensitive)");
    H5E_BEGIN_TRY
    {
        ret = H5Zconfig_get_bool("flag = TRUE", "flag", &bval);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_bool: integer 1 is a type error (not a boolean)");
    H5E_BEGIN_TRY
    {
        ret = H5Zconfig_get_bool("flag = 1", "flag", &bval);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    /* --- Miscellaneous value content ------------------------------------- */

    TESTING("H5Zconfig_get_int: single-character key");
    ret = H5Zconfig_get_int("x = 7", "x", &ival);
    if (ret <= 0 || ival != 7)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_str: equals sign inside quoted value");
    vsz = sizeof(vbuf);
    ret = H5Zconfig_get_str("expr = \"a=b\"", "expr", vbuf, &vsz);
    if (ret <= 0 || strcmp(vbuf, "a=b") != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_str: backslash-n escape in double-quoted value");
    vsz = sizeof(vbuf);
    ret = H5Zconfig_get_str("msg = \"line1\\nline2\"", "msg", vbuf, &vsz);
    if (ret <= 0 || strcmp(vbuf, "line1\nline2") != 0)
        TEST_ERROR;
    PASSED();

    /* --- H5Zconfig_get_str buffer-size edge cases ------------------------ */

    TESTING("H5Zconfig_get_str: size-query (buf=NULL sets *buf_size)");
    vsz = 0;
    ret = H5Zconfig_get_str("name = \"hello\"", "name", NULL, &vsz);
    if (ret <= 0 || vsz != 5) /* "hello" is 5 chars */
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_str: buf_size=1 returns overflow error but sets *buf_size");
    {
        char   tiny[1];
        size_t tsz = 1;
        H5E_BEGIN_TRY
        {
            ret = H5Zconfig_get_str("name = \"hello\"", "name", tiny, &tsz);
        }
        H5E_END_TRY
        if (ret >= 0)
            TEST_ERROR;
        if (tsz != 5) /* *buf_size still updated to required length */
            TEST_ERROR;
    }
    PASSED();

    return 0;

error:
    return -1;
}

/* -----------------------------------------------------------------------
 * H5Pappend_filter / H5Pget_filter_params_by_idx callback contract tests
 * ---------------------------------------------------------------------- */
static int
test_callback_contracts(void)
{
    hid_t  dcpl = H5I_INVALID_HID;
    char   pbuf[256];
    size_t plen;
    htri_t deflate_avail;

    if ((deflate_avail = H5Zfilter_avail(H5Z_FILTER_DEFLATE)) < 0)
        TEST_ERROR;

    TESTING("H5Pappend_filter: deflate with level=6");
    if (deflate_avail) {
        if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
            TEST_ERROR;
        {
            H5Z_params_t _p = H5Z_PARAMS_STR("level=6");
            if (H5Pappend_filter(dcpl, H5Z_FILTER_DEFLATE, 0, &_p) < 0)
                TEST_ERROR;
        }
        if (H5Pget_nfilters(dcpl) != 1)
            TEST_ERROR;
        H5Pclose(dcpl);
        dcpl = H5I_INVALID_HID;
        PASSED();
    }
    else
        SKIPPED();

    TESTING("H5Pappend_filter: deflate default (no params)");
    if (deflate_avail) {
        if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
            TEST_ERROR;
        if (H5Pappend_filter(dcpl, H5Z_FILTER_DEFLATE, 0, NULL) < 0)
            TEST_ERROR;
        H5Pclose(dcpl);
        dcpl = H5I_INVALID_HID;
        PASSED();
    }
    else
        SKIPPED();

    TESTING("H5Pappend_filter: shuffle (no params)");
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pappend_filter(dcpl, H5Z_FILTER_SHUFFLE, 0, NULL) < 0)
        TEST_ERROR;
    H5Pclose(dcpl);
    dcpl = H5I_INVALID_HID;
    PASSED();

    TESTING("H5Pappend_filter: shuffle rejects params");
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    H5E_BEGIN_TRY
    {
        H5Z_params_t _p  = H5Z_PARAMS_STR("blocksize=8");
        herr_t       ret = H5Pappend_filter(dcpl, H5Z_FILTER_SHUFFLE, 0, &_p);
        if (ret >= 0)
            TEST_ERROR;
    }
    H5E_END_TRY
    H5Pclose(dcpl);
    dcpl = H5I_INVALID_HID;
    PASSED();

    TESTING("H5Pget_filter_params_by_idx: deflate level=9");
    if (deflate_avail) {
        if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
            TEST_ERROR;
        {
            H5Z_params_t _p = H5Z_PARAMS_STR("level=9");
            if (H5Pappend_filter(dcpl, H5Z_FILTER_DEFLATE, 0, &_p) < 0)
                TEST_ERROR;
        }
        plen = 0;
        if (H5Pget_filter_params_by_idx(dcpl, 0, pbuf, sizeof(pbuf), &plen) < 0)
            TEST_ERROR;
        if (plen == 0)
            TEST_ERROR;
        /* The verbatim configuration string is retained and returned as-is
         * ("level=9"), taking precedence over the get_config reconstruction. */
        if (strcmp(pbuf, "level=9") != 0)
            TEST_ERROR;
        H5Pclose(dcpl);
        dcpl = H5I_INVALID_HID;
        PASSED();
    }
    else
        SKIPPED();

    TESTING("H5Pget_filter_params_by_idx: fallback for filter without get_config");
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pappend_filter(dcpl, H5Z_FILTER_SHUFFLE, 0, NULL) < 0)
        TEST_ERROR;
    plen = 0;
    /* Fletcher32 has no get_config, should fall back to cd_values= format */
    if (H5Pget_filter_params_by_idx(dcpl, 0, pbuf, sizeof(pbuf), &plen) < 0)
        TEST_ERROR;
    H5Pclose(dcpl);
    dcpl = H5I_INVALID_HID;
    PASSED();

    TESTING("H5Pappend_filter: invalid level rejects");
    if (deflate_avail) {
        if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
            TEST_ERROR;
        H5E_BEGIN_TRY
        {
            H5Z_params_t _p  = H5Z_PARAMS_STR("level=99");
            herr_t       ret = H5Pappend_filter(dcpl, H5Z_FILTER_DEFLATE, 0, &_p);
            if (ret >= 0)
                TEST_ERROR;
        }
        H5E_END_TRY
        H5Pclose(dcpl);
        dcpl = H5I_INVALID_HID;
        PASSED();
    }
    else
        SKIPPED();

    return 0;

error:
    if (dcpl != H5I_INVALID_HID)
        H5Pclose(dcpl);
    return -1;
}

/* -----------------------------------------------------------------------
 * Modify-filter pattern test
 *
 * There is no H5Pmodify_filter2 (string-based).  The documented pattern for
 * updating a filter's parameters on a copied DCPL is:
 *   1. H5Pget_filter_by_id2 -> retrieve current cd_values
 *   2. Mutate cd_values in place
 *   3. H5Pmodify_filter -> write back
 *
 * This test verifies that a filter appended via the string API produces
 * cd_values that round-trip correctly through this pattern.
 * ---------------------------------------------------------------------- */

/* H5Pmodify_filter on a filter that is not in the pipeline must fail.
 *
 * H5Z_modify locates the filter with a loop that leaves idx == nused when
 * the filter is absent, so its not-found check has to be >= rather than >.
 * With a strict >, the absent case fell through and wrote to
 * filter[nused]: past the used entries, and past the end of the array
 * altogether once nused reached nalloc (H5Z_MAX_NFILTERS), leaking the
 * cd_values allocation it stored there. */
static int
test_modify_filter_absent(void)
{
    hid_t    dcpl = H5I_INVALID_HID;
    unsigned cd[8];
    herr_t   status;

    TESTING("H5Pmodify_filter: absent filter is rejected");

    for (size_t i = 0; i < 8; i++)
        cd[i] = 0xAAAAAAAAu;

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_deflate(dcpl, 6) < 0)
        TEST_ERROR;

    /* Shuffle is not in the pipeline */
    H5E_BEGIN_TRY
    {
        status = H5Pmodify_filter(dcpl, H5Z_FILTER_SHUFFLE, 0, 1, cd);
    }
    H5E_END_TRY
    if (status >= 0)
        TEST_ERROR;
    if (H5Pget_nfilters(dcpl) != 1)
        TEST_ERROR;
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    dcpl = H5I_INVALID_HID;

    /* Same check with the pipeline filled to H5Z_MAX_NFILTERS, where
     * nused == nalloc and the stray write ran off the end of the array.
     * cd_nelmts > H5Z_COMMON_CD_VALUES so the entry would own a heap
     * allocation that nothing could ever free. */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    for (int i = 0; i < H5Z_MAX_NFILTERS; i++)
        if (H5Pset_filter(dcpl, H5Z_FILTER_SHUFFLE, H5Z_FLAG_OPTIONAL, 0, NULL) < 0)
            TEST_ERROR;
    if (H5Pget_nfilters(dcpl) != H5Z_MAX_NFILTERS)
        TEST_ERROR;
    H5E_BEGIN_TRY
    {
        status = H5Pmodify_filter(dcpl, H5Z_FILTER_SZIP, 0, 8, cd);
    }
    H5E_END_TRY
    if (status >= 0)
        TEST_ERROR;
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(dcpl);
    }
    H5E_END_TRY
    return -1;
}

static int
test_modify_filter_pattern(void)
{
    hid_t    dcpl_orig = H5I_INVALID_HID;
    hid_t    dcpl      = H5I_INVALID_HID;
    unsigned flags;
    size_t   cd_nelmts;
    unsigned cd_values[8];
    char     name[64];
    unsigned config;

    TESTING("modify filter params: H5Pget_filter_by_id2 + H5Pmodify_filter");

    if (H5Zfilter_avail(H5Z_FILTER_DEFLATE) <= 0) {
        SKIPPED();
        puts("    deflate filter not available");
        return 0;
    }

    /* Build original DCPL with deflate level=6 via string API */
    if ((dcpl_orig = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    {
        H5Z_params_t _p = H5Z_PARAMS_STR("level=6");
        if (H5Pappend_filter(dcpl_orig, H5Z_FILTER_DEFLATE, 0, &_p) < 0)
            TEST_ERROR;
    }

    /* Copy it - simulates a caller receiving a DCPL they did not create */
    if ((dcpl = H5Pcopy(dcpl_orig)) < 0)
        TEST_ERROR;

    /* Retrieve current cd_values */
    cd_nelmts = 8;
    if (H5Pget_filter_by_id2(dcpl, H5Z_FILTER_DEFLATE, &flags, &cd_nelmts, cd_values, sizeof(name), name,
                             &config) < 0)
        TEST_ERROR;
    if (cd_nelmts < 1)
        TEST_ERROR;

    /* Verify level=6 is present before modification */
    if (cd_values[0] != 6)
        TEST_ERROR;

    /* Bump level to 9 and write back */
    cd_values[0] = 9;
    if (H5Pmodify_filter(dcpl, H5Z_FILTER_DEFLATE, flags, cd_nelmts, cd_values) < 0)
        TEST_ERROR;

    /* Read back and confirm level=9 */
    cd_nelmts = 8;
    if (H5Pget_filter_by_id2(dcpl, H5Z_FILTER_DEFLATE, &flags, &cd_nelmts, cd_values, sizeof(name), name,
                             &config) < 0)
        TEST_ERROR;
    if (cd_values[0] != 9)
        TEST_ERROR;

    H5Pclose(dcpl_orig);
    H5Pclose(dcpl);
    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(dcpl_orig);
        H5Pclose(dcpl);
    }
    H5E_END_TRY
    return -1;
}

/* -----------------------------------------------------------------------
 * Round-trip tests: write and read a chunked dataset through the new API
 * ---------------------------------------------------------------------- */

/* Shared helper: create dataset with H5Pappend_filter, write wbuf, read back
 * into rbuf, verify every element matches.  Returns SUCCEED or FAIL. */
static herr_t
h5_run_filter_roundtrip(hid_t file, const char *dset_name, hsize_t *dims, hsize_t *chunks, int ndims,
                        H5Z_filter_t filter_id, const H5Z_params_t *params, int *wbuf, int *rbuf,
                        size_t total_elements)
{
    hid_t  sid  = H5I_INVALID_HID;
    hid_t  dcpl = H5I_INVALID_HID;
    hid_t  dset = H5I_INVALID_HID;
    size_t i;
    herr_t ret = FAIL;

    if ((sid = H5Screate_simple(ndims, dims, NULL)) < 0)
        goto done;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto done;
    if (H5Pset_chunk(dcpl, ndims, chunks) < 0)
        goto done;
    if (H5Pappend_filter(dcpl, filter_id, 0, params) < 0)
        goto done;

    if ((dset = H5Dcreate2(file, dset_name, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto done;
    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0)
        goto done;
    H5Dclose(dset);
    dset = H5I_INVALID_HID;

    if ((dset = H5Dopen2(file, dset_name, H5P_DEFAULT)) < 0)
        goto done;
    if (H5Dread(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
        goto done;
    for (i = 0; i < total_elements; i++)
        if (rbuf[i] != wbuf[i])
            goto done;
    ret = SUCCEED;

done:
    if (dset != H5I_INVALID_HID)
        H5Dclose(dset);
    if (dcpl != H5I_INVALID_HID)
        H5Pclose(dcpl);
    if (sid != H5I_INVALID_HID)
        H5Sclose(sid);
    return ret;
}

static int
test_roundtrip_deflate(hid_t file)
{
    hsize_t dims[2]   = {32, 32};
    hsize_t chunks[2] = {8, 8};
    int     wbuf[32 * 32], rbuf[32 * 32];
    int     i;

    TESTING("Round-trip: deflate=level=6 write/read");
    if (H5Zfilter_avail(H5Z_FILTER_DEFLATE) <= 0) {
        SKIPPED();
        puts("    deflate filter not available");
        return 0;
    }
    for (i = 0; i < 32 * 32; i++)
        wbuf[i] = i;
    {
        H5Z_params_t _p = H5Z_PARAMS_STR("level=6");
        if (h5_run_filter_roundtrip(file, "deflate_rt", dims, chunks, 2, H5Z_FILTER_DEFLATE, &_p, wbuf, rbuf,
                                    32 * 32) < 0)
            TEST_ERROR;
    }
    PASSED();
    return 0;
error:
    return -1;
}

static int
test_roundtrip_shuffle(hid_t file)
{
    hsize_t dims[1]   = {64};
    hsize_t chunks[1] = {16};
    int     wbuf[64], rbuf[64];
    int     i;

    TESTING("Round-trip: shuffle write/read");
    for (i = 0; i < 64; i++)
        wbuf[i] = i;
    if (h5_run_filter_roundtrip(file, "shuffle_rt", dims, chunks, 1, H5Z_FILTER_SHUFFLE, NULL, wbuf, rbuf,
                                64) < 0)
        TEST_ERROR;
    PASSED();
    return 0;
error:
    return -1;
}

static int
test_roundtrip_fletcher32(hid_t file)
{
    hsize_t dims[1]   = {32};
    hsize_t chunks[1] = {8};
    int     wbuf[32], rbuf[32];
    int     i;

    TESTING("Round-trip: fletcher32 write/read");
    for (i = 0; i < 32; i++)
        wbuf[i] = i * 3;
    if (h5_run_filter_roundtrip(file, "fletcher32_rt", dims, chunks, 1, H5Z_FILTER_FLETCHER32, NULL, wbuf,
                                rbuf, 32) < 0)
        TEST_ERROR;
    PASSED();
    return 0;
error:
    return -1;
}

/* -----------------------------------------------------------------------
 * Regression tests: existing H5Pset_filter still works correctly
 * ---------------------------------------------------------------------- */
static int
test_regression_old_api(hid_t file)
{
    hid_t    dset = H5I_INVALID_HID, dcpl = H5I_INVALID_HID;
    hid_t    sid        = H5I_INVALID_HID;
    hsize_t  dims[1]    = {32};
    hsize_t  chunks[1]  = {8};
    unsigned cd_vals[1] = {5}; /* deflate level 5 */
    int      wbuf[32], rbuf[32];
    int      i;

    TESTING("Regression: H5Pset_filter (old API) still works");

    if (H5Zfilter_avail(H5Z_FILTER_DEFLATE) <= 0) {
        SKIPPED();
        puts("    deflate filter not available");
        return 0;
    }

    for (i = 0; i < 32; i++)
        wbuf[i] = i + 100;

    if ((sid = H5Screate_simple(1, dims, NULL)) < 0)
        TEST_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_chunk(dcpl, 1, chunks) < 0)
        TEST_ERROR;
    if (H5Pset_filter(dcpl, H5Z_FILTER_DEFLATE, 0, 1, cd_vals) < 0)
        TEST_ERROR;

    if ((dset = H5Dcreate2(file, "old_api_rt", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0)
        TEST_ERROR;

    H5Dclose(dset);
    dset = H5I_INVALID_HID;

    if ((dset = H5Dopen2(file, "old_api_rt", H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dread(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
        TEST_ERROR;
    for (i = 0; i < 32; i++)
        if (rbuf[i] != wbuf[i])
            TEST_ERROR;

    H5Dclose(dset);
    H5Sclose(sid);
    H5Pclose(dcpl);
    PASSED();
    return 0;

error:
    if (dset != H5I_INVALID_HID)
        H5Dclose(dset);
    if (sid != H5I_INVALID_HID)
        H5Sclose(sid);
    if (dcpl != H5I_INVALID_HID)
        H5Pclose(dcpl);
    return -1;
}

static int
test_regression_filter2_appends(void)
{
    hid_t dcpl = H5I_INVALID_HID;
    int   nfilters;

    TESTING("Regression: H5Pappend_filter appends (matches H5Pset_filter behavior)");

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pappend_filter(dcpl, H5Z_FILTER_SHUFFLE, 0, NULL) < 0)
        TEST_ERROR;
    {
        int expected = 1;
        if (H5Zfilter_avail(H5Z_FILTER_DEFLATE) > 0) {
            H5Z_params_t _p = H5Z_PARAMS_STR("level=3");
            if (H5Pappend_filter(dcpl, H5Z_FILTER_DEFLATE, 0, &_p) < 0)
                TEST_ERROR;
            expected = 2;
        }
        if ((nfilters = H5Pget_nfilters(dcpl)) != expected)
            TEST_ERROR;
    }

    H5Pclose(dcpl);
    PASSED();
    return 0;

error:
    if (dcpl != H5I_INVALID_HID)
        H5Pclose(dcpl);
    return -1;
}

/* -----------------------------------------------------------------------
 * ScaleOffset set_config / get_config round-trip
 * ---------------------------------------------------------------------- */
static int
test_scaleoffset_params(hid_t file)
{
    hid_t   dcpl      = H5I_INVALID_HID;
    hsize_t dims[1]   = {32};
    hsize_t chunks[1] = {8};
    int     wbuf[32], rbuf[32];
    char    pbuf[256];
    size_t  plen;
    int     i;

    TESTING("Round-trip: scaleoffset scale_type = \"int\", scale_factor = 0");

    /* Verify get_config round-trip on the dcpl before writing */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_chunk(dcpl, 1, chunks) < 0)
        TEST_ERROR;
    {
        H5Z_params_t _p = H5Z_PARAMS_STR("scale_type = \"int\", scale_factor = 0");
        if (H5Pappend_filter(dcpl, H5Z_FILTER_SCALEOFFSET, 0, &_p) < 0)
            TEST_ERROR;
    }
    plen = 0;
    if (H5Pget_filter_params_by_idx(dcpl, 0, pbuf, sizeof(pbuf), &plen) < 0)
        TEST_ERROR;
    if (plen == 0)
        TEST_ERROR;
    H5Pclose(dcpl);
    dcpl = H5I_INVALID_HID;

    for (i = 0; i < 32; i++)
        wbuf[i] = i * 2;
    {
        H5Z_params_t _p = H5Z_PARAMS_STR("scale_type = \"int\", scale_factor = 0");
        if (h5_run_filter_roundtrip(file, "scaleoffset_rt", dims, chunks, 1, H5Z_FILTER_SCALEOFFSET, &_p,
                                    wbuf, rbuf, 32) < 0)
            TEST_ERROR;
    }
    PASSED();
    return 0;

error:
    if (dcpl != H5I_INVALID_HID)
        H5Pclose(dcpl);
    return -1;
}

/* -----------------------------------------------------------------------
 * canonical_name display tests
 *
 * Registers a minimal class3 filter and verifies that H5Pget_filter_by_id2
 * returns the canonical_name as the filter name.
 * ---------------------------------------------------------------------- */

#define TITLE_FILTER_ID 512

static size_t
title_filter_func(unsigned int flags, size_t cd_nelmts, const unsigned int *cd_values,
                  hid_t H5_ATTR_UNUSED dxpl_id, const hsize_t H5_ATTR_UNUSED *scaled,
                  size_t H5_ATTR_UNUSED ndims, size_t nbytes, size_t *buf_size, void **buf)
{
    (void)flags;
    (void)cd_nelmts;
    (void)cd_values;
    (void)buf_size;
    (void)buf;
    return nbytes; /* pass-through */
}

static int
test_canonical_name_display(void)
{
    static const H5Z_class3_t title_cls = {
        2,                   /* version        */
        TITLE_FILTER_ID,     /* id             */
        1,                   /* encoder_present */
        1,                   /* decoder_present */
        "test_title_filter", /* canonical_name */
        NULL,                /* description    */
        NULL,                /* can_apply      */
        NULL,                /* set_local      */
        title_filter_func,   /* filter         */
        NULL,                /* set_config     */
        NULL,                /* get_config     */
    };
    hid_t    dcpl = H5I_INVALID_HID;
    unsigned flags;
    unsigned cd_values[8];
    size_t   cd_nelmts;
    char     name[64];
    unsigned config;

    TESTING("canonical_name: returned by H5Pget_filter_by_id2 as filter name");

    if (H5Zregister(&title_cls) < 0)
        TEST_ERROR;

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pappend_filter(dcpl, TITLE_FILTER_ID, 0, NULL) < 0)
        TEST_ERROR;

    cd_nelmts = 8;
    if (H5Pget_filter_by_id2(dcpl, TITLE_FILTER_ID, &flags, &cd_nelmts, cd_values, sizeof(name), name,
                             &config) < 0)
        TEST_ERROR;
    if (strcmp(name, "test_title_filter") != 0)
        TEST_ERROR;

    H5Pclose(dcpl);
    H5Zunregister(TITLE_FILTER_ID);
    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (dcpl != H5I_INVALID_HID)
            H5Pclose(dcpl);
        H5Zunregister(TITLE_FILTER_ID);
    }
    H5E_END_TRY
    return -1;
}

/* -----------------------------------------------------------------------
 * H5Z_class3_t name field tests
 * ---------------------------------------------------------------------- */

#define NAME_FILTER_ID 513

static size_t
name_filter_func(unsigned int flags, size_t cd_nelmts, const unsigned int *cd_values,
                 hid_t H5_ATTR_UNUSED dxpl_id, const hsize_t H5_ATTR_UNUSED *scaled,
                 size_t H5_ATTR_UNUSED ndims, size_t nbytes, size_t *buf_size, void **buf)
{
    (void)flags;
    (void)cd_nelmts;
    (void)cd_values;
    (void)buf_size;
    (void)buf;
    return nbytes;
}

static int
test_class3_name(void)
{
    herr_t ret;

    TESTING("H5Z_class3_t: NULL name rejected by H5Zregister");
    {
        static const H5Z_class3_t null_name_cls = {
            2,                /* version        */
            NAME_FILTER_ID,   /* id             */
            1,                /* encoder_present */
            1,                /* decoder_present */
            NULL,             /* canonical_name - intentionally NULL to trigger error */
            NULL,             /* description    */
            NULL,             /* can_apply      */
            NULL,             /* set_local      */
            name_filter_func, /* filter         */
            NULL,             /* set_config     */
            NULL,             /* get_config     */
        };
        H5E_BEGIN_TRY
        {
            ret = H5Zregister(&null_name_cls);
        }
        H5E_END_TRY
        if (ret >= 0)
            TEST_ERROR;
    }
    PASSED();

    TESTING("H5Z_class3_t: valid name accepted by H5Zregister");
    {
        static const H5Z_class3_t valid_cls = {
            2,                  /* version        */
            NAME_FILTER_ID,     /* id             */
            1,                  /* encoder_present */
            1,                  /* decoder_present */
            "test_name_filter", /* canonical_name */
            NULL,               /* description    */
            NULL,               /* can_apply      */
            NULL,               /* set_local      */
            name_filter_func,   /* filter         */
            NULL,               /* set_config     */
            NULL,               /* get_config     */
        };
        if (H5Zregister(&valid_cls) < 0)
            TEST_ERROR;
        H5Zunregister(NAME_FILTER_ID);
    }
    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Zunregister(NAME_FILTER_ID);
    }
    H5E_END_TRY
    return -1;
}

/* -----------------------------------------------------------------------
 * Additional coverage tests
 * ---------------------------------------------------------------------- */

/* 1. Empty-input handling: when set_config is present it is invoked with
 *    params=NULL so the plugin can fail-fast. When set_config is absent,
 *    the filter is appended with cd_nelmts=0 and no callback runs. */
#define FASTPATH_FILTER_ID 514
#define NOCFG_FILTER_ID    518

static int fastpath_set_config_called   = 0;
static int fastpath_set_config_was_null = 0;

static herr_t
fastpath_set_config(const char *params, unsigned *flags, size_t *cd_nelmts, unsigned cd_values[],
                    size_t cd_values_size)
{
    (void)flags;
    (void)cd_values;
    (void)cd_values_size;
    fastpath_set_config_called   = 1;
    fastpath_set_config_was_null = (params == NULL);
    *cd_nelmts                   = 0;
    return 0;
}

static size_t
fastpath_filter_func(unsigned int flags, size_t cd_nelmts, const unsigned int *cd_values,
                     hid_t H5_ATTR_UNUSED dxpl_id, const hsize_t H5_ATTR_UNUSED *scaled,
                     size_t H5_ATTR_UNUSED ndims, size_t nbytes, size_t *buf_size, void **buf)
{
    (void)flags;
    (void)cd_nelmts;
    (void)cd_values;
    (void)buf_size;
    (void)buf;
    return nbytes;
}

static int
test_empty_string_fast_path(void)
{
    static const H5Z_class3_t fp_cls = {
        2,                    /* version         */
        FASTPATH_FILTER_ID,   /* id              */
        1,                    /* encoder_present */
        1,                    /* decoder_present */
        "fastpath_filter",    /* canonical_name  */
        NULL,                 /* description     */
        NULL,                 /* can_apply       */
        NULL,                 /* set_local       */
        fastpath_filter_func, /* filter          */
        fastpath_set_config,  /* set_config      */
        NULL,                 /* get_config      */
    };
    static const H5Z_class3_t nocfg_cls = {
        2,                    /* version         */
        NOCFG_FILTER_ID,      /* id              */
        1,                    /* encoder_present */
        1,                    /* decoder_present */
        "nocfg_filter",       /* canonical_name  */
        NULL,                 /* description     */
        NULL,                 /* can_apply       */
        NULL,                 /* set_local       */
        fastpath_filter_func, /* filter          */
        NULL,                 /* set_config (intentionally absent) */
        NULL,                 /* get_config      */
    };
    hid_t  dcpl = H5I_INVALID_HID;
    herr_t ret;

    /* NULL passed as the 4th argument means "no params" via the CDVALUES
     * path; the filter is appended with cd_nelmts = 0 and set_config is
     * not consulted (the STRING path is only entered when the caller
     * supplies an H5Z_params_t with type == H5Z_PARAMS_STRING). */
    TESTING("H5Pappend_filter: NULL params (4th arg) takes CDVALUES path; set_config not called");
    if (H5Zregister(&fp_cls) < 0)
        TEST_ERROR;
    fastpath_set_config_called   = 0;
    fastpath_set_config_was_null = 0;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pappend_filter(dcpl, FASTPATH_FILTER_ID, 0, NULL) < 0)
        TEST_ERROR;
    if (fastpath_set_config_called != 0)
        TEST_ERROR;
    H5Pclose(dcpl);
    dcpl = H5I_INVALID_HID;
    PASSED();

    TESTING("H5Pappend_filter: H5Z_PARAMS_STRING with NULL str invokes set_config with params=NULL");
    fastpath_set_config_called   = 0;
    fastpath_set_config_was_null = 0;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    {
        H5Z_params_t _p = H5Z_PARAMS_STR(NULL);
        if (H5Pappend_filter(dcpl, FASTPATH_FILTER_ID, 0, &_p) < 0)
            TEST_ERROR;
    }
    if (fastpath_set_config_called == 0)
        TEST_ERROR;
    if (fastpath_set_config_was_null == 0)
        TEST_ERROR;
    H5Pclose(dcpl);
    dcpl = H5I_INVALID_HID;
    PASSED();

    TESTING("H5Pappend_filter: empty string params invokes set_config with params=NULL");
    fastpath_set_config_called   = 0;
    fastpath_set_config_was_null = 0;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    {
        H5Z_params_t _p = H5Z_PARAMS_STR("");
        if (H5Pappend_filter(dcpl, FASTPATH_FILTER_ID, 0, &_p) < 0)
            TEST_ERROR;
    }
    if (fastpath_set_config_called == 0)
        TEST_ERROR;
    if (fastpath_set_config_was_null == 0)
        TEST_ERROR;
    H5Pclose(dcpl);
    dcpl = H5I_INVALID_HID;
    H5Zunregister(FASTPATH_FILTER_ID);
    PASSED();

    TESTING("H5Pappend_filter: filter without set_config accepts NULL/empty params");
    if (H5Zregister(&nocfg_cls) < 0)
        TEST_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pappend_filter(dcpl, NOCFG_FILTER_ID, 0, NULL) < 0)
        TEST_ERROR;
    {
        H5Z_params_t _p = H5Z_PARAMS_STR("");
        if (H5Pappend_filter(dcpl, NOCFG_FILTER_ID, 0, &_p) < 0)
            TEST_ERROR;
    }
    H5Pclose(dcpl);
    dcpl = H5I_INVALID_HID;
    PASSED();

    TESTING("H5Pappend_filter: filter without set_config rejects non-empty params");
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    H5E_BEGIN_TRY
    {
        H5Z_params_t _p = H5Z_PARAMS_STR("level=6");
        ret             = H5Pappend_filter(dcpl, NOCFG_FILTER_ID, 0, &_p);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    H5Pclose(dcpl);
    dcpl = H5I_INVALID_HID;
    H5Zunregister(NOCFG_FILTER_ID);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (dcpl != H5I_INVALID_HID)
            H5Pclose(dcpl);
        H5Zunregister(FASTPATH_FILTER_ID);
        H5Zunregister(NOCFG_FILTER_ID);
    }
    H5E_END_TRY
    return -1;
}

/* 2. CDVALUES path: H5Z_PARAMS_CDVALUES is passed through correctly */
#define CDVALS_FILTER_ID 515

static size_t
cdvals_filter_func(unsigned int flags, size_t cd_nelmts, const unsigned int *cd_values,
                   hid_t H5_ATTR_UNUSED dxpl_id, const hsize_t H5_ATTR_UNUSED *scaled,
                   size_t H5_ATTR_UNUSED ndims, size_t nbytes, size_t *buf_size, void **buf)
{
    (void)flags;
    (void)cd_nelmts;
    (void)cd_values;
    (void)buf_size;
    (void)buf;
    return nbytes;
}

static int
test_cdvalues_path(void)
{
    static const H5Z_class3_t cdv_cls = {
        2,                  /* version         */
        CDVALS_FILTER_ID,   /* id              */
        1,                  /* encoder_present */
        1,                  /* decoder_present */
        "cdvals_filter",    /* canonical_name  */
        NULL,               /* description     */
        NULL,               /* can_apply       */
        NULL,               /* set_local       */
        cdvals_filter_func, /* filter          */
        NULL,               /* set_config      */
        NULL,               /* get_config      */
    };
    hid_t        dcpl   = H5I_INVALID_HID;
    unsigned     vals[] = {42, 99};
    H5Z_params_t p      = H5Z_PARAMS_RAW(2, vals);
    unsigned     flags2;
    unsigned     cd_out[8];
    size_t       cd_nelmts = 8;
    char         name[64];
    unsigned     config;

    TESTING("H5Pappend_filter: CDVALUES path stores raw cd_values");
    if (H5Zregister(&cdv_cls) < 0)
        TEST_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pappend_filter(dcpl, CDVALS_FILTER_ID, 0, &p) < 0)
        TEST_ERROR;
    if (H5Pget_filter_by_id2(dcpl, CDVALS_FILTER_ID, &flags2, &cd_nelmts, cd_out, sizeof(name), name,
                             &config) < 0)
        TEST_ERROR;
    if (cd_nelmts < 2 || cd_out[0] != 42 || cd_out[1] != 99)
        TEST_ERROR;
    H5Pclose(dcpl);
    dcpl = H5I_INVALID_HID;
    H5Zunregister(CDVALS_FILTER_ID);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (dcpl != H5I_INVALID_HID)
            H5Pclose(dcpl);
        H5Zunregister(CDVALS_FILTER_ID);
    }
    H5E_END_TRY
    return -1;
}

/* 3. CDVALUES path: cd_values=NULL with cd_nelmts>0 is rejected */
static int
test_cdvalues_null_check(void)
{
    hid_t        dcpl = H5I_INVALID_HID;
    H5Z_params_t p;
    herr_t       ret;

    TESTING("H5Pappend_filter: CDVALUES with NULL pointer and nelmts>0 is rejected");
    p.type            = H5Z_PARAMS_CDVALUES;
    p.u.raw.cd_nelmts = 3;
    p.u.raw.cd_values = NULL;

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    H5E_BEGIN_TRY
    {
        ret = H5Pappend_filter(dcpl, H5Z_FILTER_SHUFFLE, 0, &p);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    H5Pclose(dcpl);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (dcpl != H5I_INVALID_HID)
            H5Pclose(dcpl);
    }
    H5E_END_TRY
    return -1;
}

/* 4. canonical_name does NOT pollute cd_values; H5Pappend_filter via
 *    H5Z_PARAMS_STRING writes only set_config's cd_values to the pipeline. */
#define CDVALS_CLEAN_FILTER_ID 516

static int64_t cdvals_clean_param_value = 0;

static herr_t
cdvals_clean_set_config(const char *params, unsigned *flags, size_t *cd_nelmts, unsigned cd_values[],
                        size_t cd_values_size)
{
    int64_t val = 0;
    (void)flags;
    (void)cd_values_size;
    if (params && *params)
        H5Zconfig_get_int(params, "alpha", &val);
    cdvals_clean_param_value = val;
    *cd_nelmts               = 1;
    if (cd_values)
        cd_values[0] = (unsigned)val;
    return 0;
}

static size_t
cdvals_clean_filter_func(unsigned int flags, size_t cd_nelmts, const unsigned int *cd_values,
                         hid_t H5_ATTR_UNUSED dxpl_id, const hsize_t H5_ATTR_UNUSED *scaled,
                         size_t H5_ATTR_UNUSED ndims, size_t nbytes, size_t *buf_size, void **buf)
{
    (void)flags;
    (void)cd_nelmts;
    (void)cd_values;
    (void)buf_size;
    (void)buf;
    return nbytes;
}

static int
test_cdvalues_no_name_pollution(void)
{
    static const H5Z_class3_t tc_cls = {
        2,                        /* version         */
        CDVALS_CLEAN_FILTER_ID,   /* id              */
        1,                        /* encoder_present */
        1,                        /* decoder_present */
        "cdvals_clean_filter",    /* canonical_name  */
        NULL,                     /* description     */
        NULL,                     /* can_apply       */
        NULL,                     /* set_local       */
        cdvals_clean_filter_func, /* filter          */
        cdvals_clean_set_config,  /* set_config      */
        NULL,                     /* get_config      */
    };
    hid_t    dcpl = H5I_INVALID_HID;
    unsigned flags2;
    unsigned cd_out[32];
    size_t   cd_nelmts = 32;
    char     name[64];
    unsigned config;

    TESTING("canonical_name: not packed into cd_values");
    if (H5Zregister(&tc_cls) < 0)
        TEST_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    {
        H5Z_params_t _p = H5Z_PARAMS_STR("alpha=7");
        if (H5Pappend_filter(dcpl, CDVALS_CLEAN_FILTER_ID, 0, &_p) < 0)
            TEST_ERROR;
    }
    if (H5Pget_filter_by_id2(dcpl, CDVALS_CLEAN_FILTER_ID, &flags2, &cd_nelmts, cd_out, sizeof(name), name,
                             &config) < 0)
        TEST_ERROR;
    /* cd_nelmts should be exactly what set_config wrote (1), with no
     * trailing canonical_name slots appended by the library. */
    if (cd_nelmts != 1)
        TEST_ERROR;
    if (cd_out[0] != 7)
        TEST_ERROR;
    H5Pclose(dcpl);
    dcpl = H5I_INVALID_HID;
    H5Zunregister(CDVALS_CLEAN_FILTER_ID);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (dcpl != H5I_INVALID_HID)
            H5Pclose(dcpl);
        H5Zunregister(CDVALS_CLEAN_FILTER_ID);
    }
    H5E_END_TRY
    return -1;
}

/* 5. canonical_name persists as the pipeline name field after H5Zunregister */
#define PERSIST_FILTER_ID 517

static size_t
persist_filter_func(unsigned int flags, size_t cd_nelmts, const unsigned int *cd_values,
                    hid_t H5_ATTR_UNUSED dxpl_id, const hsize_t H5_ATTR_UNUSED *scaled,
                    size_t H5_ATTR_UNUSED ndims, size_t nbytes, size_t *buf_size, void **buf)
{
    (void)flags;
    (void)cd_nelmts;
    (void)cd_values;
    (void)buf_size;
    (void)buf;
    return nbytes;
}

static int
test_canonical_name_persistence(void)
{
    static const H5Z_class3_t persist_cls = {
        2,                   /* version         */
        PERSIST_FILTER_ID,   /* id              */
        1,                   /* encoder_present */
        1,                   /* decoder_present */
        "persist_filter",    /* canonical_name  */
        NULL,                /* description     */
        NULL,                /* can_apply       */
        NULL,                /* set_local       */
        persist_filter_func, /* filter         */
        NULL,                /* set_config      */
        NULL,                /* get_config      */
    };
    hid_t    dcpl = H5I_INVALID_HID;
    unsigned flags2;
    unsigned cd_out[8];
    size_t   cd_nelmts = 8;
    char     name[64];
    unsigned config;

    TESTING("canonical_name: persists as pipeline name after H5Zunregister");
    if (H5Zregister(&persist_cls) < 0)
        TEST_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pappend_filter(dcpl, PERSIST_FILTER_ID, 0, NULL) < 0)
        TEST_ERROR;
    /* Unregister so the only name source is the pipeline name field */
    H5Zunregister(PERSIST_FILTER_ID);
    /* Name should still be "persist_filter" (canonical_name) from the stored pipeline entry */
    if (H5Pget_filter_by_id2(dcpl, PERSIST_FILTER_ID, &flags2, &cd_nelmts, cd_out, sizeof(name), name,
                             &config) < 0)
        TEST_ERROR;
    if (strcmp(name, "persist_filter") != 0)
        TEST_ERROR;
    H5Pclose(dcpl);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (dcpl != H5I_INVALID_HID)
            H5Pclose(dcpl);
        H5Zunregister(PERSIST_FILTER_ID);
    }
    H5E_END_TRY
    return -1;
}

/* 6. When canonical_name is absent and plugin is not loaded, name falls back to decimal ID */
static int
test_name_id_fallback(void)
{
    /* Use a filter ID that is not registered and has no built-in entry */
    H5Z_filter_t unregistered_id = 800;
    hid_t        dcpl            = H5I_INVALID_HID;
    unsigned     flags2;
    unsigned     cd_out[8];
    size_t       cd_nelmts = 8;
    char         name[64];
    unsigned     config;
    char         expected[32];

    TESTING("name fallback: unregistered filter returns decimal ID string");
    /* Build a dcpl with the unregistered filter via the old raw API */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    /* H5Pset_filter does not load plugins or validate existence at property-set time */
    if (H5Pset_filter(dcpl, unregistered_id, H5Z_FLAG_OPTIONAL, 0, NULL) < 0)
        TEST_ERROR;
    cd_nelmts = 8;
    /* H5Pget_filter_by_id2: with no registered entry, name should be "800" */
    if (H5Pget_filter_by_id2(dcpl, unregistered_id, &flags2, &cd_nelmts, cd_out, sizeof(name), name,
                             &config) < 0)
        TEST_ERROR;
    snprintf(expected, sizeof(expected), "%d", (int)unregistered_id);
    if (strcmp(name, expected) != 0)
        TEST_ERROR;
    H5Pclose(dcpl);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (dcpl != H5I_INVALID_HID)
            H5Pclose(dcpl);
    }
    H5E_END_TRY
    return -1;
}

/* 7. H5Zregister rejects a canonical_name longer than 255 bytes */
#define LONGTITLE_FILTER_ID 518

static size_t
longtitle_filter_func(unsigned int flags, size_t cd_nelmts, const unsigned int *cd_values,
                      hid_t H5_ATTR_UNUSED dxpl_id, const hsize_t H5_ATTR_UNUSED *scaled,
                      size_t H5_ATTR_UNUSED ndims, size_t nbytes, size_t *buf_size, void **buf)
{
    (void)flags;
    (void)cd_nelmts;
    (void)cd_values;
    (void)buf_size;
    (void)buf;
    return nbytes;
}

static int
test_canonical_name_length_limit(void)
{
    /* A 256-byte canonical_name (one byte over the 255-byte limit) */
    static const char long_title[257] =
        /* 100 */ "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
                  "AAAAAAAAAA"
                  /* 100 */ "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
                  "AAAAAAAAAAAAAAAAAAAA"
                  /*  56 */ "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"; /* 256 'A's + NUL */
    H5Z_class3_t long_cls = {
        2,                     /* version         */
        LONGTITLE_FILTER_ID,   /* id              */
        1,                     /* encoder_present */
        1,                     /* decoder_present */
        long_title,            /* canonical_name  */
        NULL,                  /* description     */
        NULL,                  /* can_apply       */
        NULL,                  /* set_local       */
        longtitle_filter_func, /* filter          */
        NULL,                  /* set_config      */
        NULL,                  /* get_config      */
    };
    herr_t ret;

    TESTING("H5Zregister: canonical_name > 255 bytes is rejected");
    H5E_BEGIN_TRY
    {
        ret = H5Zregister(&long_cls);
    }
    H5E_END_TRY
    if (ret >= 0) {
        H5Zunregister(LONGTITLE_FILTER_ID);
        TEST_ERROR;
    }
    PASSED();

    /* The canonical name is written into the filter-pipeline object header
     * message, so it reaches disk and flows back out through h5dump and
     * h5repack (which resolves names to filter IDs).  H5Zregister must
     * therefore hold it to [A-Za-z0-9_.-], non-empty, rather than accepting
     * arbitrary bytes.  RFC-HDFG-2026-001 sec:name-registry. */
    TESTING("H5Zregister: canonical_name syntax is enforced");
    {
        /* Each must be rejected, and for the stated reason. */
        static const char *const bad[] = {
            "",              /* empty                                  */
            "has space",     /* whitespace                             */
            "semi;colon",    /* the reserved pipeline separator        */
            "quote\"mark",   /* would need escaping in tool output     */
            "brace{}",       /* TOML inline-table delimiters           */
            "comma,sep",     /* the parameter-string separator         */
            "new\nline",     /* would corrupt line-oriented tool output*/
            "tab\there",     /* likewise                               */
            "caf\xc3\xa9",    /* non-ASCII: outside the declared class  */
            "slash/path",    /* path-like, unsafe as an identifier     */
            "equals=sign",   /* the key/value separator                */
        };
        /* Each must be accepted: the full declared character class. */
        static const char *const good[] = {
            "zfp", "deflate", "blosc2.lz4", "my_filter-2", "A", "0",
            "aA0_.-",
        };
        size_t i;

        for (i = 0; i < sizeof(bad) / sizeof(bad[0]); i++) {
            H5Z_class3_t c = {2,   LONGTITLE_FILTER_ID, 1,    1,   bad[i], NULL,
                              NULL, NULL,               longtitle_filter_func, NULL, NULL};
            H5E_BEGIN_TRY
            {
                ret = H5Zregister(&c);
            }
            H5E_END_TRY
            if (ret >= 0) {
                fprintf(stderr, "\n   accepted invalid name \"%s\"\n", bad[i]);
                H5Zunregister(LONGTITLE_FILTER_ID);
                TEST_ERROR;
            }
        }

        for (i = 0; i < sizeof(good) / sizeof(good[0]); i++) {
            H5Z_class3_t c = {2,   LONGTITLE_FILTER_ID, 1,    1,   good[i], NULL,
                              NULL, NULL,               longtitle_filter_func, NULL, NULL};
            if (H5Zregister(&c) < 0) {
                fprintf(stderr, "\n   rejected valid name \"%s\"\n", good[i]);
                TEST_ERROR;
            }
            if (H5Zunregister(LONGTITLE_FILTER_ID) < 0)
                TEST_ERROR;
        }
    }
    PASSED();
    return 0;

error:
    return -1;
}

/* 8. H5Pappend_filter rejects a param string longer than H5Z_CONFIG_STRING_MAX */
static int
test_config_string_max(void)
{
    hid_t  dcpl    = H5I_INVALID_HID;
    char  *big_str = NULL;
    herr_t ret;

    TESTING("H5Pappend_filter: param string > H5Z_CONFIG_STRING_MAX is rejected");
    /* Build a string one byte over the limit */
    if (NULL == (big_str = (char *)malloc(H5Z_CONFIG_STRING_MAX + 2)))
        TEST_ERROR;
    memset(big_str, 'x', H5Z_CONFIG_STRING_MAX + 1);
    big_str[H5Z_CONFIG_STRING_MAX + 1] = '\0';

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    H5E_BEGIN_TRY
    {
        H5Z_params_t _p;
        _p.type  = H5Z_PARAMS_STRING;
        _p.u.str = big_str;
        ret      = H5Pappend_filter(dcpl, H5Z_FILTER_DEFLATE, 0, &_p);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    H5Pclose(dcpl);
    dcpl = H5I_INVALID_HID;
    free(big_str);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (dcpl != H5I_INVALID_HID)
            H5Pclose(dcpl);
    }
    H5E_END_TRY
    free(big_str);
    return -1;
}

/* 8b. H5Pappend_filter accepts a param string of exactly H5Z_CONFIG_STRING_MAX bytes */
static int
test_config_string_max_boundary(void)
{
    hid_t      dcpl       = H5I_INVALID_HID;
    char      *ok_str     = NULL;
    const char prefix[]   = "level = 6";
    size_t     prefix_len = sizeof(prefix) - 1;
    herr_t     ret;
    htri_t     deflate_avail;

    TESTING("H5Pappend_filter: param string == H5Z_CONFIG_STRING_MAX is accepted");
    /* Deflate (zlib) required as the test filter - skip if not compiled in */
    if ((deflate_avail = H5Zfilter_avail(H5Z_FILTER_DEFLATE)) < 0)
        TEST_ERROR;
    if (!deflate_avail) {
        SKIPPED();
        return 0;
    }
    if (NULL == (ok_str = (char *)malloc(H5Z_CONFIG_STRING_MAX + 1)))
        TEST_ERROR;
    /* Valid TOML string of exactly H5Z_CONFIG_STRING_MAX bytes:
     * "level = 6" followed by trailing spaces (valid in TOML). */
    memcpy(ok_str, prefix, prefix_len);
    memset(ok_str + prefix_len, ' ', H5Z_CONFIG_STRING_MAX - prefix_len);
    ok_str[H5Z_CONFIG_STRING_MAX] = '\0';

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    {
        H5Z_params_t _p;
        _p.type  = H5Z_PARAMS_STRING;
        _p.u.str = ok_str;
        ret      = H5Pappend_filter(dcpl, H5Z_FILTER_DEFLATE, 0, &_p);
    }
    if (ret < 0)
        TEST_ERROR;
    H5Pclose(dcpl);
    dcpl = H5I_INVALID_HID;
    free(ok_str);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (dcpl != H5I_INVALID_HID)
            H5Pclose(dcpl);
    }
    H5E_END_TRY
    free(ok_str);
    return -1;
}

/* 9. H5Zconfig_get_str: buf != NULL but buf_size == NULL is rejected */
static int
test_config_get_str_null_buf_size(void)
{
    const char *params = "key = \"value\"";
    char        buf[32];
    htri_t      ret;

    TESTING("H5Zconfig_get_str: buf != NULL, buf_size == NULL is rejected");
    H5E_BEGIN_TRY
    {
        ret = H5Zconfig_get_str(params, "key", buf, NULL);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();
    return 0;

error:
    return -1;
}

/* 10. set_config and get_config are invoked and their output is consistent */
#define CALLBACK_FILTER_ID 519

static int     callback_set_called = 0;
static int64_t callback_stored_val = 0;

static herr_t
callback_set_config(const char *params, unsigned *flags, size_t *cd_nelmts, unsigned cd_values[],
                    size_t cd_values_size)
{
    int64_t v = 0;
    (void)flags;
    (void)cd_nelmts;
    (void)cd_values;
    (void)cd_values_size;
    callback_set_called = 1;
    if (params && *params)
        H5Zconfig_get_int(params, "beta", &v);
    callback_stored_val = v;
    return 0;
}

static herr_t
callback_get_config(unsigned flags, size_t cd_nelmts, const unsigned cd_values[], char *buf, size_t *buf_size)
{
    size_t needed;
    (void)flags;
    (void)cd_nelmts;
    (void)cd_values;
    needed = (size_t)snprintf(NULL, 0, "beta = %" PRId64, callback_stored_val) + 1;
    if (buf_size)
        *buf_size = needed;
    if (buf)
        snprintf(buf, needed, "beta = %" PRId64, callback_stored_val);
    return 0;
}

static size_t
callback_filter_func(unsigned int flags, size_t cd_nelmts, const unsigned int *cd_values,
                     hid_t H5_ATTR_UNUSED dxpl_id, const hsize_t H5_ATTR_UNUSED *scaled,
                     size_t H5_ATTR_UNUSED ndims, size_t nbytes, size_t *buf_size, void **buf)
{
    (void)flags;
    (void)cd_nelmts;
    (void)cd_values;
    (void)buf_size;
    (void)buf;
    return nbytes;
}

static int
test_set_get_config_callbacks(void)
{
    static const H5Z_class3_t cb_cls = {
        2,                    /* version         */
        CALLBACK_FILTER_ID,   /* id              */
        1,                    /* encoder_present */
        1,                    /* decoder_present */
        "callback_filter",    /* canonical_name  */
        NULL,                 /* description     */
        NULL,                 /* can_apply       */
        NULL,                 /* set_local       */
        callback_filter_func, /* filter          */
        callback_set_config,  /* set_config      */
        callback_get_config,  /* get_config      */
    };
    hid_t  dcpl = H5I_INVALID_HID;
    char   pbuf[256];
    size_t plen = 0;

    TESTING("set_config and get_config callbacks are invoked and output is consistent");
    if (H5Zregister(&cb_cls) < 0)
        TEST_ERROR;
    callback_set_called = 0;
    callback_stored_val = 0;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    {
        H5Z_params_t _p = H5Z_PARAMS_STR("beta=42");
        if (H5Pappend_filter(dcpl, CALLBACK_FILTER_ID, 0, &_p) < 0)
            TEST_ERROR;
    }
    if (callback_set_called == 0)
        TEST_ERROR;
    if (callback_stored_val != 42)
        TEST_ERROR;
    /* get_config should produce "beta = 42" */
    if (H5Pget_filter_params_by_idx(dcpl, 0, pbuf, sizeof(pbuf), &plen) < 0)
        TEST_ERROR;
    if (strstr(pbuf, "beta") == NULL)
        TEST_ERROR;
    if (strstr(pbuf, "42") == NULL)
        TEST_ERROR;
    H5Pclose(dcpl);
    dcpl = H5I_INVALID_HID;
    H5Zunregister(CALLBACK_FILTER_ID);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (dcpl != H5I_INVALID_HID)
            H5Pclose(dcpl);
        H5Zunregister(CALLBACK_FILTER_ID);
    }
    H5E_END_TRY
    return -1;
}

/* -----------------------------------------------------------------------
 * H5Zget_filter_class_info: registry-level info incl. name + description
 * ---------------------------------------------------------------------- */
static int
test_get_filter_info2_builtin(void)
{
    H5Z_class_info_t info;
    herr_t           ret;

    TESTING("H5Zget_filter_class_info: deflate built-in (canonical name + description)");
#ifdef H5_HAVE_FILTER_DEFLATE
    memset(&info, 0xAA, sizeof(info));
    if (H5Zget_filter_class_info(H5Z_FILTER_DEFLATE, &info) < 0)
        TEST_ERROR;
    if (info.id != H5Z_FILTER_DEFLATE)
        TEST_ERROR;
    if ((info.config_flags & H5Z_FILTER_CONFIG_ENCODE_ENABLED) == 0)
        TEST_ERROR;
    if ((info.config_flags & H5Z_FILTER_CONFIG_DECODE_ENABLED) == 0)
        TEST_ERROR;
    if (info.name == NULL || strcmp(info.name, "deflate") != 0)
        TEST_ERROR;
    if (info.description == NULL || strstr(info.description, "Deflate") == NULL)
        TEST_ERROR;
    if (!info.has_set_config)
        TEST_ERROR;
    if (!info.has_get_config)
        TEST_ERROR;
    PASSED();
#else
    SKIPPED();
    puts("    deflate filter not built");
    (void)info;
    (void)ret;
#endif

    TESTING("H5Zget_filter_class_info: scaleoffset (no description in built-in registration)");
    memset(&info, 0xAA, sizeof(info));
    if (H5Zget_filter_class_info(H5Z_FILTER_SCALEOFFSET, &info) < 0)
        TEST_ERROR;
    if (info.id != H5Z_FILTER_SCALEOFFSET)
        TEST_ERROR;
    if (info.name == NULL || strcmp(info.name, "scaleoffset") != 0)
        TEST_ERROR;
    /* description for scaleoffset is set ("Scale+offset lossy compression...") */
    if (info.description == NULL)
        TEST_ERROR;
    if (!info.has_set_config)
        TEST_ERROR;
    if (!info.has_get_config)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zget_filter_class_info: shuffle (set_config but no get_config)");
    memset(&info, 0xAA, sizeof(info));
    if (H5Zget_filter_class_info(H5Z_FILTER_SHUFFLE, &info) < 0)
        TEST_ERROR;
    if (info.id != H5Z_FILTER_SHUFFLE)
        TEST_ERROR;
    if (info.name == NULL || strcmp(info.name, "shuffle") != 0)
        TEST_ERROR;
    if (!info.has_set_config)
        TEST_ERROR;
    if (info.has_get_config) /* shuffle has NULL get_config */
        TEST_ERROR;
    PASSED();

    TESTING("H5Zget_filter_class_info: NULL info pointer rejected");
    H5E_BEGIN_TRY
    {
        ret = H5Zget_filter_class_info(H5Z_FILTER_SHUFFLE, NULL);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zget_filter_class_info: unknown filter ID fails with H5E_NOFILTER");
    H5E_BEGIN_TRY
    {
        ret = H5Zget_filter_class_info((H5Z_filter_t)0x6FFE /*unregistered*/, &info);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    return 0;

error:
    return -1;
}

/* -----------------------------------------------------------------------
 * filter2 context passthrough: verify dxpl_id, scaled[], ndims arrive
 * at the H5Z_func2_t callback with correct values during chunk I/O.
 * ---------------------------------------------------------------------- */

#define CTXPASS_FILTER_ID 520
#define CTXPASS_NCHUNKS   4 /* 2x2 chunk grid in an 8x8/4x4 dataset */

typedef struct {
    hid_t  expected_dxpl;
    size_t count;
    bool   dxpl_ok;
    bool   ndims_ok;
    bool   scaled_ok;
    bool   scaled_seen[2][2]; /* [row_chunk][col_chunk] */
} ctxpass_state_t;

static ctxpass_state_t g_ctxpass;

static size_t
ctxpass_filter_cb(unsigned int flags, size_t cd_nelmts, const unsigned int *cd_values, hid_t dxpl_id,
                  const hsize_t *scaled, size_t ndims, size_t nbytes, size_t *buf_size, void **buf)
{
    (void)flags;
    (void)cd_nelmts;
    (void)cd_values;
    (void)buf_size;
    (void)buf;

    g_ctxpass.count++;

    if (dxpl_id != g_ctxpass.expected_dxpl)
        g_ctxpass.dxpl_ok = false;

    if (ndims != 2)
        g_ctxpass.ndims_ok = false;

    if (scaled == NULL || scaled[0] > 1 || scaled[1] > 1)
        g_ctxpass.scaled_ok = false;
    else
        g_ctxpass.scaled_seen[scaled[0]][scaled[1]] = true;

    return nbytes; /* pass-through */
}

static const H5Z_class3_t ctxpass_cls = {
    2, CTXPASS_FILTER_ID, 1, 1, "test_ctxpass_filter", NULL, NULL, NULL, ctxpass_filter_cb, NULL, NULL,
};

static int
check_ctxpass_state(void)
{
    if (g_ctxpass.count != CTXPASS_NCHUNKS)
        return -1;
    if (!g_ctxpass.dxpl_ok)
        return -1;
    if (!g_ctxpass.ndims_ok)
        return -1;
    if (!g_ctxpass.scaled_ok)
        return -1;
    if (!g_ctxpass.scaled_seen[0][0] || !g_ctxpass.scaled_seen[0][1] || !g_ctxpass.scaled_seen[1][0] ||
        !g_ctxpass.scaled_seen[1][1])
        return -1;
    return 0;
}

static int
test_filter2_context_passthrough(hid_t file)
{
    /* 8x8 dataset with 4x4 chunks -> 2x2 chunk grid, 4 total chunks.
     * Chunk cache is disabled (nslots=0) so the filter fires during
     * H5Dwrite / H5Dread rather than at a later flush, letting us verify
     * the dxpl_id, scaled[], and ndims values that arrive at filter2.
     */
    static const hsize_t dims[2]   = {8, 8};
    static const hsize_t chunks[2] = {4, 4};
    hid_t                dxpl      = H5I_INVALID_HID;
    hid_t                dapl      = H5I_INVALID_HID;
    hid_t                dcpl      = H5I_INVALID_HID;
    hid_t                sid       = H5I_INVALID_HID;
    hid_t                dset      = H5I_INVALID_HID;
    int                  wbuf[8 * 8], rbuf[8 * 8];

    for (int i = 0; i < 64; i++)
        wbuf[i] = i;

    if (H5Zregister(&ctxpass_cls) < 0)
        TEST_ERROR;

    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        TEST_ERROR;

    /* nslots=0 disables the chunk cache; each chunk is encoded/decoded
     * immediately during the I/O call rather than deferred to flush. */
    if ((dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_chunk_cache(dapl, 0, H5D_CHUNK_CACHE_NBYTES_DEFAULT, H5D_CHUNK_CACHE_W0_DEFAULT) < 0)
        TEST_ERROR;

    if ((sid = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_chunk(dcpl, 2, chunks) < 0)
        TEST_ERROR;
    if (H5Pset_filter(dcpl, CTXPASS_FILTER_ID, H5Z_FLAG_MANDATORY, 0, NULL) < 0)
        TEST_ERROR;
    if ((dset = H5Dcreate2(file, "ctxpass_dset", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, dapl)) < 0)
        TEST_ERROR;

    TESTING("filter2 context passthrough: dxpl_id/scaled/ndims on write");

    memset(&g_ctxpass, 0, sizeof(g_ctxpass));
    g_ctxpass.expected_dxpl = dxpl;
    g_ctxpass.dxpl_ok       = true;
    g_ctxpass.ndims_ok      = true;
    g_ctxpass.scaled_ok     = true;

    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, wbuf) < 0)
        TEST_ERROR;
    if (check_ctxpass_state() < 0)
        TEST_ERROR;

    PASSED();

    TESTING("filter2 context passthrough: dxpl_id/scaled/ndims on read");

    memset(&g_ctxpass, 0, sizeof(g_ctxpass));
    g_ctxpass.expected_dxpl = dxpl;
    g_ctxpass.dxpl_ok       = true;
    g_ctxpass.ndims_ok      = true;
    g_ctxpass.scaled_ok     = true;

    if (H5Dread(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, rbuf) < 0)
        TEST_ERROR;

    for (int i = 0; i < 64; i++)
        if (rbuf[i] != wbuf[i])
            TEST_ERROR;
    if (check_ctxpass_state() < 0)
        TEST_ERROR;

    PASSED();

    H5Dclose(dset);
    H5Pclose(dcpl);
    H5Pclose(dapl);
    H5Sclose(sid);
    H5Pclose(dxpl);
    H5Zunregister(CTXPASS_FILTER_ID);
    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (dset != H5I_INVALID_HID)
            H5Dclose(dset);
        if (dcpl != H5I_INVALID_HID)
            H5Pclose(dcpl);
        if (dapl != H5I_INVALID_HID)
            H5Pclose(dapl);
        if (sid != H5I_INVALID_HID)
            H5Sclose(sid);
        if (dxpl != H5I_INVALID_HID)
            H5Pclose(dxpl);
        H5Zunregister(CTXPASS_FILTER_ID);
    }
    H5E_END_TRY
    return -1;
}

/* -----------------------------------------------------------------------
 * On-disk configuration-string storage (pipeline v3, RFC-HDFG-2026-001)
 *
 * This filter's stored parameter string ("level=N", no spaces) differs
 * from its get_config reconstruction ("level = N", with spaces) so tests
 * can tell whether a returned string came from the persisted verbatim
 * string or from the get_config fallback.
 * ---------------------------------------------------------------------- */

#define CFG_ONDISK_FILTER_ID 531

static herr_t
cfg_ondisk_set_config(const char *params, unsigned H5_ATTR_UNUSED *flags, size_t *cd_nelmts,
                      unsigned cd_values[], size_t cd_values_size)
{
    int64_t level = 0;

    *cd_nelmts = 1;
    if (cd_values && cd_values_size >= 1) {
        if (params && *params)
            H5Zconfig_get_int(params, "level", &level);
        cd_values[0] = (unsigned)level;
    }
    return SUCCEED;
}

static herr_t
cfg_ondisk_get_config(unsigned H5_ATTR_UNUSED flags, size_t cd_nelmts, const unsigned cd_values[], char *buf,
                      size_t *buf_size)
{
    unsigned level  = (cd_nelmts >= 1) ? cd_values[0] : 0;
    size_t   needed = (size_t)snprintf(NULL, 0, "level = %u", level) + 1;

    if (buf_size)
        *buf_size = needed;
    if (buf)
        snprintf(buf, needed, "level = %u", level);
    return SUCCEED;
}

static size_t
cfg_ondisk_filter_func(unsigned int flags, size_t cd_nelmts, const unsigned int *cd_values,
                       hid_t H5_ATTR_UNUSED dxpl_id, const hsize_t H5_ATTR_UNUSED *scaled,
                       size_t H5_ATTR_UNUSED ndims, size_t nbytes, size_t *buf_size, void **buf)
{
    (void)flags;
    (void)cd_nelmts;
    (void)cd_values;
    (void)buf_size;
    (void)buf;
    return nbytes; /* pass-through */
}

static const H5Z_class3_t cfg_ondisk_cls = {
    2,                      /* version         */
    CFG_ONDISK_FILTER_ID,   /* id              */
    1,                      /* encoder_present */
    1,                      /* decoder_present */
    "cfg_ondisk_filter",    /* name            */
    NULL,                   /* description     */
    NULL,                   /* can_apply       */
    NULL,                   /* set_local       */
    cfg_ondisk_filter_func, /* filter         */
    cfg_ondisk_set_config,  /* set_config      */
    cfg_ondisk_get_config,  /* get_config      */
};

/* Build a chunked, filter-configured DCPL from a parameter string */
static hid_t
cfg_ondisk_make_dcpl(const char *params)
{
    hid_t        dcpl     = H5I_INVALID_HID;
    hsize_t      chunk[2] = {4, 4};
    H5Z_params_t p;

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        return H5I_INVALID_HID;
    if (H5Pset_chunk(dcpl, 2, chunk) < 0)
        goto error;
    p.type  = H5Z_PARAMS_STRING;
    p.u.str = params;
    if (H5Pappend_filter(dcpl, CFG_ONDISK_FILTER_ID, 0, &p) < 0)
        goto error;
    return dcpl;
error:
    H5E_BEGIN_TRY
    {
        H5Pclose(dcpl);
    }
    H5E_END_TRY
    return H5I_INVALID_HID;
}

/* Fetch filter 0's parameter string from a DCPL into buf */
static herr_t
cfg_ondisk_get_params(hid_t dcpl, char *buf, size_t buf_size)
{
    size_t len = 0;

    if (H5Pget_filter_params_by_idx(dcpl, 0, buf, buf_size, &len) < 0)
        return FAIL;
    return SUCCEED;
}

static int
test_config_string_ondisk(hid_t fapl)
{
    hid_t   file = H5I_INVALID_HID, sid = H5I_INVALID_HID, dcpl = H5I_INVALID_HID, dset = H5I_INVALID_HID;
    hid_t   dcpl_out = H5I_INVALID_HID, fapl_dg = H5I_INVALID_HID, dcpl_dec = H5I_INVALID_HID;
    hid_t   file2   = H5I_INVALID_HID;
    hsize_t dims[2] = {8, 8};
    char    filename[1024], filename2[1024];
    char    pbuf[H5Z_CONFIG_STRING_MAX + 1];
    void   *enc_buf  = NULL;
    size_t  enc_size = 0;

    if (H5Zregister(&cfg_ondisk_cls) < 0)
        TEST_ERROR;
    if ((sid = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;
    h5_fixname(FILENAME[1], fapl, filename, sizeof(filename));
    h5_fixname(FILENAME[2], fapl, filename2, sizeof(filename2));

    /* --- fmt-01/02: verbatim round-trip, recovered without the plugin --- */
    TESTING("config string: verbatim on-disk round-trip without plugin");
    if ((dcpl = cfg_ondisk_make_dcpl("level=7")) < 0)
        TEST_ERROR;
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;
    if ((dset = H5Dcreate2(file, "dset", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0 || H5Pclose(dcpl) < 0 || H5Fclose(file) < 0)
        TEST_ERROR;
    dset = dcpl = file = H5I_INVALID_HID;

    /* Drop the plugin: the only remaining source is the persisted string */
    if (H5Zunregister(CFG_ONDISK_FILTER_ID) < 0)
        TEST_ERROR;
    if ((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        TEST_ERROR;
    if ((dset = H5Dopen2(file, "dset", H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if ((dcpl_out = H5Dget_create_plist(dset)) < 0)
        TEST_ERROR;
    if (cfg_ondisk_get_params(dcpl_out, pbuf, sizeof(pbuf)) < 0)
        TEST_ERROR;
    if (strcmp(pbuf, "level=7") != 0) /* verbatim, not the "level = 7" get_config form */
        TEST_ERROR;
    if (H5Pclose(dcpl_out) < 0 || H5Dclose(dset) < 0 || H5Fclose(file) < 0)
        TEST_ERROR;
    dcpl_out = dset = file = H5I_INVALID_HID;
    if (H5Zregister(&cfg_ondisk_cls) < 0) /* restore for later cases */
        TEST_ERROR;
    PASSED();

    /* --- fmt-05: libver high bound below V300 silently omits the string --- */
    TESTING("config string: silent v2 downgrade when libver bound too low");
    if ((fapl_dg = H5Pcopy(fapl)) < 0)
        TEST_ERROR;
    if (H5Pset_libver_bounds(fapl_dg, H5F_LIBVER_EARLIEST, H5F_LIBVER_V200) < 0)
        TEST_ERROR;
    if ((dcpl = cfg_ondisk_make_dcpl("level=5")) < 0)
        TEST_ERROR;
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_dg)) < 0)
        TEST_ERROR;
    if ((dset = H5Dcreate2(file, "dset", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0 || H5Pclose(dcpl) < 0 || H5Fclose(file) < 0)
        TEST_ERROR;
    dset = dcpl = file = H5I_INVALID_HID;
    /* Plugin still registered: getter falls back to get_config ("level = 5"),
     * proving the verbatim string was not persisted at v2. */
    if ((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        TEST_ERROR;
    if ((dset = H5Dopen2(file, "dset", H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if ((dcpl_out = H5Dget_create_plist(dset)) < 0)
        TEST_ERROR;
    if (cfg_ondisk_get_params(dcpl_out, pbuf, sizeof(pbuf)) < 0)
        TEST_ERROR;
    if (strcmp(pbuf, "level = 5") != 0) /* get_config form, not the stored "level=5" */
        TEST_ERROR;
    if (H5Pclose(dcpl_out) < 0 || H5Dclose(dset) < 0 || H5Fclose(file) < 0 || H5Pclose(fapl_dg) < 0)
        TEST_ERROR;
    dcpl_out = dset = file = fapl_dg = H5I_INVALID_HID;
    PASSED();

    /* --- fmt-07: H5Pmodify_filter clears the stored string --- */
    TESTING("config string: H5Pmodify_filter clears the stored string");
    if ((dcpl = cfg_ondisk_make_dcpl("level=3")) < 0)
        TEST_ERROR;
    {
        unsigned cd[1] = {8};
        if (H5Pmodify_filter(dcpl, CFG_ONDISK_FILTER_ID, 0, 1, cd) < 0)
            TEST_ERROR;
    }
    if (cfg_ondisk_get_params(dcpl, pbuf, sizeof(pbuf)) < 0)
        TEST_ERROR;
    if (strcmp(pbuf, "level = 8") != 0) /* get_config of new cd_values, not "level=3" */
        TEST_ERROR;
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    dcpl = H5I_INVALID_HID;
    PASSED();

    /* --- reg-07: stored string survives H5Pencode/H5Pdecode --- */
    TESTING("config string: survives H5Pencode/H5Pdecode");
    if ((dcpl = cfg_ondisk_make_dcpl("level=9")) < 0)
        TEST_ERROR;
    if (H5Pencode2(dcpl, NULL, &enc_size, H5P_DEFAULT) < 0)
        TEST_ERROR;
    if (NULL == (enc_buf = malloc(enc_size)))
        TEST_ERROR;
    if (H5Pencode2(dcpl, enc_buf, &enc_size, H5P_DEFAULT) < 0)
        TEST_ERROR;
    if ((dcpl_dec = H5Pdecode(enc_buf)) < 0)
        TEST_ERROR;
    if (cfg_ondisk_get_params(dcpl_dec, pbuf, sizeof(pbuf)) < 0)
        TEST_ERROR;
    if (strcmp(pbuf, "level=9") != 0)
        TEST_ERROR;
    free(enc_buf);
    enc_buf = NULL;
    if (H5Pclose(dcpl_dec) < 0 || H5Pclose(dcpl) < 0)
        TEST_ERROR;
    dcpl_dec = dcpl = H5I_INVALID_HID;
    PASSED();

    /* --- fmt-06: H5Ocopy deep-copies the stored string to a new file --- */
    TESTING("config string: survives H5Ocopy to another file");
    if ((dcpl = cfg_ondisk_make_dcpl("level=9")) < 0)
        TEST_ERROR;
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;
    if ((dset = H5Dcreate2(file, "dset", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;
    if ((file2 = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;
    if (H5Ocopy(file, "dset", file2, "dset_copy", H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR;
    if ((dset = H5Dopen2(file2, "dset_copy", H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if ((dcpl_out = H5Dget_create_plist(dset)) < 0)
        TEST_ERROR;
    if (cfg_ondisk_get_params(dcpl_out, pbuf, sizeof(pbuf)) < 0)
        TEST_ERROR;
    if (strcmp(pbuf, "level=9") != 0)
        TEST_ERROR;
    if (H5Pclose(dcpl_out) < 0 || H5Dclose(dset) < 0 || H5Pclose(dcpl) < 0 || H5Fclose(file) < 0 ||
        H5Fclose(file2) < 0)
        TEST_ERROR;
    dcpl_out = dset = dcpl = file = file2 = H5I_INVALID_HID;
    PASSED();

    if (H5Sclose(sid) < 0)
        TEST_ERROR;
    if (H5Zunregister(CFG_ONDISK_FILTER_ID) < 0)
        TEST_ERROR;
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(dcpl);
        H5Pclose(dcpl_out);
        H5Pclose(dcpl_dec);
        H5Pclose(fapl_dg);
        H5Dclose(dset);
        H5Sclose(sid);
        H5Fclose(file);
        H5Fclose(file2);
        H5Zunregister(CFG_ONDISK_FILTER_ID);
    }
    H5E_END_TRY
    free(enc_buf);
    return -1;
}

/* -----------------------------------------------------------------------
 * Canonicalization of the persisted configuration string
 * (RFC-HDFG-2026-001 sec:pline-v3)
 *
 * The stored string is normalised so the bytes on disk are a valid TOML
 * v1.0.0 document: optional outer braces are stripped, and C99 hex-float
 * literals are rewritten to %.16e decimal.  Neither the braced form nor a
 * hex-float literal is accepted by a stock TOML parser, and the persisted
 * string is meant to be readable by tools that are not the HDF5 library
 * (pure-reimplementation readers such as jHDF and pyfive parse the object
 * header directly).  Both normalisations preserve the value exactly.
 *
 * This filter carries a double so bit-exactness can be asserted: the
 * value is memcpy'd into cd_values rather than quantised.
 * ---------------------------------------------------------------------- */

#define CANON_FILTER_ID 532

static herr_t
canon_set_config(const char *params, unsigned H5_ATTR_UNUSED *flags, size_t *cd_nelmts, unsigned cd_values[],
                 size_t cd_values_size)
{
    double rate = 0.0;

    /* A double occupies two unsigned slots; cd_values_size is an element
     * count, matching the value H5Pappend_filter passes. */
    *cd_nelmts = 2;
    if (cd_values) {
        if (cd_values_size < 2)
            return FAIL;
        if (params && *params) {
            if (H5Zconfig_get_double(params, "rate", &rate) < 0)
                return FAIL;
        }
        memcpy(cd_values, &rate, sizeof(rate));
    }
    return SUCCEED;
}

static herr_t
canon_get_config(unsigned H5_ATTR_UNUSED flags, size_t cd_nelmts, const unsigned cd_values[], char *buf,
                 size_t *buf_size)
{
    double rate = 0.0;
    size_t needed;

    if (cd_nelmts >= 2)
        memcpy(&rate, cd_values, sizeof(rate));
    needed = (size_t)snprintf(NULL, 0, "rate = %.16e", rate) + 1;
    if (buf_size)
        *buf_size = needed;
    if (buf)
        snprintf(buf, needed, "rate = %.16e", rate);
    return SUCCEED;
}

static size_t
canon_filter_func(unsigned int H5_ATTR_UNUSED flags, size_t H5_ATTR_UNUSED cd_nelmts,
                  const unsigned int H5_ATTR_UNUSED *cd_values, hid_t H5_ATTR_UNUSED dxpl_id,
                  const hsize_t H5_ATTR_UNUSED *scaled, size_t H5_ATTR_UNUSED ndims, size_t nbytes,
                  size_t H5_ATTR_UNUSED *buf_size, void H5_ATTR_UNUSED **buf)
{
    return nbytes; /* pass-through */
}

static const H5Z_class3_t canon_cls = {
    2,                 /* version         */
    CANON_FILTER_ID,   /* id              */
    1,                 /* encoder_present */
    1,                 /* decoder_present */
    "canon_filter",    /* name            */
    NULL,              /* description     */
    NULL,              /* can_apply       */
    NULL,              /* set_local       */
    canon_filter_func, /* filter          */
    canon_set_config,  /* set_config      */
    canon_get_config,  /* get_config      */
};

/* Append CANON_FILTER_ID configured with PARAMS and return the DCPL */
static hid_t
canon_make_dcpl(const char *params)
{
    hid_t        dcpl     = H5I_INVALID_HID;
    hsize_t      chunk[2] = {4, 4};
    H5Z_params_t p;

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        return H5I_INVALID_HID;
    if (H5Pset_chunk(dcpl, 2, chunk) < 0)
        goto error;
    p.type  = H5Z_PARAMS_STRING;
    p.u.str = params;
    if (H5Pappend_filter(dcpl, CANON_FILTER_ID, 0, &p) < 0)
        goto error;
    return dcpl;
error:
    H5E_BEGIN_TRY
    {
        H5Pclose(dcpl);
    }
    H5E_END_TRY
    return H5I_INVALID_HID;
}

/* Assert that appending INPUT stores exactly EXPECT */
static int
canon_check(const char *input, const char *expect)
{
    hid_t  dcpl = H5I_INVALID_HID;
    char   pbuf[H5Z_CONFIG_STRING_MAX + 1];
    size_t plen = 0;

    if ((dcpl = canon_make_dcpl(input)) < 0)
        return -1;
    if (H5Pget_filter_params_by_idx(dcpl, 0, pbuf, sizeof(pbuf), &plen) < 0)
        goto error;
    if (strcmp(pbuf, expect) != 0) {
        fprintf(stderr, "\n   input  \"%s\"\n   stored \"%s\"\n   expect \"%s\"\n", input, pbuf, expect);
        goto error;
    }
    if (H5Pclose(dcpl) < 0)
        return -1;
    return 0;
error:
    H5E_BEGIN_TRY
    {
        H5Pclose(dcpl);
    }
    H5E_END_TRY
    return -1;
}

/* Recover the double that set_config packed into cd_values */
static int
canon_stored_double(hid_t dcpl, double *out)
{
    unsigned     cd[8];
    size_t       cd_nelmts = 8;
    unsigned     flags     = 0;
    char         name[64];
    unsigned     cfg = 0;
    H5Z_filter_t id;

    id = H5Pget_filter2(dcpl, 0, &flags, &cd_nelmts, cd, sizeof(name), name, &cfg);
    if (id < 0 || cd_nelmts < 2)
        return -1;
    memcpy(out, cd, sizeof(*out));
    return 0;
}


/* -----------------------------------------------------------------------
 * A version-2 plugin and a version-3 plugin in the SAME pipeline
 *
 * The regression tests (reg-01..reg-07) cover v2 and v3 filters separately.
 * They do not cover a pipeline that carries both at once, which is the shape
 * a real file takes while an ecosystem is mid-migration: an old third-party
 * compressor stacked with a newly ported one, plus a built-in.  The pipeline
 * message must then hold entries of both kinds -- one with a stored
 * configuration string, one without -- and still round-trip data, preserve
 * order, and introspect correctly per entry.
 * ---------------------------------------------------------------------- */
#define MIXV2_FILTER_ID 533
#define MIXV3_FILTER_ID 534

/* An XOR transform is its own inverse, so a successful data round-trip is
 * equally consistent with both filters running and with neither running.
 * Count invocations so the test can tell those apart. */
static int mixv2_calls = 0;
static int mixv3_calls = 0;

/* Both filters are byte-reversible transforms so a data round-trip actually
 * proves each ran, rather than passing vacuously as a no-op would. */
static size_t
mixv2_filter_func(unsigned int flags, size_t H5_ATTR_UNUSED cd_nelmts,
                  const unsigned int H5_ATTR_UNUSED cd_values[], size_t nbytes,
                  size_t H5_ATTR_UNUSED *buf_size, void **buf)
{
    unsigned char *p = (unsigned char *)*buf;
    size_t         i;

    (void)flags; /* XOR is its own inverse, so encode and decode are identical */
    mixv2_calls++;
    for (i = 0; i < nbytes; i++)
        p[i] ^= 0x5AU;
    return nbytes;
}

static size_t
mixv3_filter_func(unsigned int flags, size_t H5_ATTR_UNUSED cd_nelmts,
                  const unsigned int H5_ATTR_UNUSED *cd_values, hid_t H5_ATTR_UNUSED dxpl_id,
                  const hsize_t H5_ATTR_UNUSED *scaled, size_t H5_ATTR_UNUSED ndims, size_t nbytes,
                  size_t H5_ATTR_UNUSED *buf_size, void **buf)
{
    unsigned char *p = (unsigned char *)*buf;
    size_t         i;

    (void)flags;
    mixv3_calls++;
    for (i = 0; i < nbytes; i++)
        p[i] ^= 0xA5U;
    return nbytes;
}

static herr_t
mixv3_set_config(const char *params, unsigned H5_ATTR_UNUSED *flags, size_t *cd_nelmts,
                 unsigned cd_values[], size_t cd_values_size)
{
    int64_t lvl = 0;

    if (H5Zconfig_get_int(params, "level", &lvl) < 0)
        return FAIL;

    /* Pass 1: size query.  Pass 2: populate.  Both must report the same
     * count, and the second pass must respect the caller's capacity. */
    *cd_nelmts = 1;
    if (cd_values == NULL)
        return SUCCEED;
    if (cd_values_size < 1)
        return FAIL;
    cd_values[0] = (unsigned)lvl;
    return SUCCEED;
}

static const H5Z_class2_t mixv2_cls = {
    H5Z_CLASS_T_VERS,  /* version -- a genuine v2 class, not a v3 in disguise */
    MIXV2_FILTER_ID,   /* id              */
    1,                 /* encoder_present */
    1,                 /* decoder_present */
    "legacy v2 filter (free-form comment)", /* name: v2 permits arbitrary text */
    NULL,              /* can_apply       */
    NULL,              /* set_local       */
    mixv2_filter_func, /* filter          */
};

static const H5Z_class3_t mixv3_cls = {
    2,                 /* version -- H5Z_class3_t  */
    MIXV3_FILTER_ID,   /* id              */
    1,                 /* encoder_present */
    1,                 /* decoder_present */
    "mixv3",           /* canonical name  */
    NULL,              /* description     */
    NULL,              /* can_apply       */
    NULL,              /* set_local       */
    mixv3_filter_func, /* filter          */
    mixv3_set_config,  /* set_config      */
    NULL,              /* get_config      */
};

static int
test_mixed_v2_v3_pipeline(hid_t fapl)
{
    hid_t   file = H5I_INVALID_HID, dcpl = H5I_INVALID_HID, sid = H5I_INVALID_HID;
    hid_t   dset = H5I_INVALID_HID, dcpl_out = H5I_INVALID_HID;
    hsize_t dims[2] = {8, 8}, chunk[2] = {4, 4};
    char    filename[1024];
    int     wbuf[8][8], rbuf[8][8];
    int     i, j;
    char    pbuf[256];
    size_t  plen = 0;
    unsigned nfilt;

    if (H5Zregister(&mixv2_cls) < 0)
        TEST_ERROR;
    if (H5Zregister(&mixv3_cls) < 0)
        TEST_ERROR;

    TESTING("mixed pipeline: v2 and v3 filters in one dataset");

    for (i = 0; i < 8; i++)
        for (j = 0; j < 8; j++)
            wbuf[i][j] = i * 8 + j;

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_chunk(dcpl, 2, chunk) < 0)
        TEST_ERROR;

    /* Order matters and must survive: v2 (raw cd_values), then a built-in,
     * then v3 (configuration string). */
    if (H5Pset_filter(dcpl, MIXV2_FILTER_ID, H5Z_FLAG_MANDATORY, 0, NULL) < 0)
        TEST_ERROR;
    if (H5Pset_shuffle(dcpl) < 0)
        TEST_ERROR;
    {
        H5Z_params_t p;
        p.type  = H5Z_PARAMS_STRING;
        p.u.str = "level = 7";
        if (H5Pappend_filter(dcpl, MIXV3_FILTER_ID, H5Z_FLAG_MANDATORY, &p) < 0)
            TEST_ERROR;
    }

    h5_fixname(FILENAME[2], fapl, filename, sizeof(filename));
    if ((sid = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;
    if ((dset = H5Dcreate2(file, "mixed", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0 || H5Fclose(file) < 0 || H5Pclose(dcpl) < 0)
        TEST_ERROR;
    dset = file = dcpl = H5I_INVALID_HID;

    /* Both must have run on the write path. */
    if (mixv2_calls == 0 || mixv3_calls == 0)
        TEST_ERROR;
    mixv2_calls = mixv3_calls = 0;

    /* Reopen: both filters must run in reverse and reproduce the data. */
    if ((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        TEST_ERROR;
    if ((dset = H5Dopen2(file, "mixed", H5P_DEFAULT)) < 0)
        TEST_ERROR;
    memset(rbuf, 0, sizeof(rbuf));
    if (H5Dread(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
        TEST_ERROR;
    if (memcmp(wbuf, rbuf, sizeof(wbuf)) != 0)
        TEST_ERROR;
    /* ... and on the read path, which is what makes the compare meaningful. */
    if (mixv2_calls == 0 || mixv3_calls == 0)
        TEST_ERROR;

    /* Pipeline shape and per-entry introspection. */
    if ((dcpl_out = H5Dget_create_plist(dset)) < 0)
        TEST_ERROR;
    if ((nfilt = (unsigned)H5Pget_nfilters(dcpl_out)) != 3)
        TEST_ERROR;

    /* Entry 0: the v2 filter, in its original position, with no stored
     * string -- it was configured through the raw cd_values path. */
    if (H5Pget_filter2(dcpl_out, 0, NULL, NULL, NULL, 0, NULL, NULL) != MIXV2_FILTER_ID)
        TEST_ERROR;
    plen = 0;
    H5E_BEGIN_TRY
    {
        (void)H5Pget_filter_params_by_idx(dcpl_out, 0, NULL, 0, &plen);
    }
    H5E_END_TRY
    if (plen != 0)
        TEST_ERROR;

    /* Entry 1: the built-in, undisturbed between the two plugins. */
    if (H5Pget_filter2(dcpl_out, 1, NULL, NULL, NULL, 0, NULL, NULL) != H5Z_FILTER_SHUFFLE)
        TEST_ERROR;

    /* Entry 2: the v3 filter, still carrying its configuration string. */
    if (H5Pget_filter2(dcpl_out, 2, NULL, NULL, NULL, 0, NULL, NULL) != MIXV3_FILTER_ID)
        TEST_ERROR;
    if (H5Pget_filter_params_by_idx(dcpl_out, 2, pbuf, sizeof(pbuf), &plen) < 0)
        TEST_ERROR;
    if (strcmp(pbuf, "level = 7") != 0) {
        fprintf(stderr, "\n   entry 2 stored \"%s\", expected \"level = 7\"\n", pbuf);
        TEST_ERROR;
    }

    if (H5Pclose(dcpl_out) < 0 || H5Dclose(dset) < 0 || H5Fclose(file) < 0 || H5Sclose(sid) < 0)
        TEST_ERROR;
    dcpl_out = dset = file = sid = H5I_INVALID_HID;

    if (H5Zunregister(MIXV2_FILTER_ID) < 0 || H5Zunregister(MIXV3_FILTER_ID) < 0)
        TEST_ERROR;
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(dcpl);
        H5Pclose(dcpl_out);
        H5Dclose(dset);
        H5Fclose(file);
        H5Sclose(sid);
        H5Zunregister(MIXV2_FILTER_ID);
        H5Zunregister(MIXV3_FILTER_ID);
    }
    H5E_END_TRY
    return -1;
}

static int
test_config_canonicalization(hid_t fapl)
{
    hid_t   dcpl = H5I_INVALID_HID, sid = H5I_INVALID_HID, file = H5I_INVALID_HID, dset = H5I_INVALID_HID;
    hid_t   dcpl_out = H5I_INVALID_HID;
    hsize_t dims[2]  = {8, 8};
    char    filename[1024];
    char    pbuf[H5Z_CONFIG_STRING_MAX + 1];
    size_t  plen = 0;
    double  got  = 0.0;

    if (H5Zregister(&canon_cls) < 0)
        TEST_ERROR;

    /* --- canon-01: a plain bare string is stored unchanged --- */
    TESTING("canonicalization: bare string stored unchanged");
    if (canon_check("rate = 1.5", "rate = 1.5") < 0)
        TEST_ERROR;
    PASSED();

    /* --- canon-02: outer braces are stripped --- */
    TESTING("canonicalization: outer braces stripped");
    if (canon_check("{rate = 1.5}", "rate = 1.5") < 0)
        TEST_ERROR;
    if (canon_check("{ rate = 1.5 }", "rate = 1.5") < 0)
        TEST_ERROR;
    if (canon_check("  {rate = 1.5}  ", "rate = 1.5") < 0)
        TEST_ERROR;
    PASSED();

    /* --- canon-03: hex-float rewritten to %.16e decimal --- */
    TESTING("canonicalization: hex-float rewritten to decimal");
    if (canon_check("rate = 0x1.8p+1", "rate = 3.0000000000000000e+00") < 0)
        TEST_ERROR;
    if (canon_check("rate = 0x1.cp+1", "rate = 3.5000000000000000e+00") < 0)
        TEST_ERROR;
    PASSED();

    /* --- canon-04: both normalisations at once --- */
    TESTING("canonicalization: braces and hex-float together");
    if (canon_check("{ rate = 0x1.8p+1 }", "rate = 3.0000000000000000e+00") < 0)
        TEST_ERROR;
    PASSED();

    /* --- canon-05: hex-float text inside a quoted string is preserved --- */
    TESTING("canonicalization: hex-float inside a string is not rewritten");
    if (canon_check("rate = 1.5, note = \"0x1.8p+1\"", "rate = 1.5, note = \"0x1.8p+1\"") < 0)
        TEST_ERROR;
    if (canon_check("rate = 1.5, note = '0x1.8p+1'", "rate = 1.5, note = '0x1.8p+1'") < 0)
        TEST_ERROR;
    PASSED();

    /* --- canon-06: rewriting a hex-float loses no precision --- */
    TESTING("canonicalization: hex-float value is bit-exact after rewrite");
    if ((dcpl = canon_make_dcpl("rate = 0x1.5555555555555p-2")) < 0)
        TEST_ERROR;
    if (canon_stored_double(dcpl, &got) < 0)
        TEST_ERROR;
    if (memcmp(&got, &(double){0x1.5555555555555p-2}, sizeof(got)) != 0)
        TEST_ERROR;
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    dcpl = H5I_INVALID_HID;
    PASSED();

    /* --- canon-07: the canonical form is itself valid set_config input --- */
    TESTING("canonicalization: stored form round-trips through set_config");
    if ((dcpl = canon_make_dcpl("{rate = 0x1.cp+1}")) < 0)
        TEST_ERROR;
    if (H5Pget_filter_params_by_idx(dcpl, 0, pbuf, sizeof(pbuf), &plen) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    /* Feed the stored string back in; it must parse and yield the same value */
    if ((dcpl = canon_make_dcpl(pbuf)) < 0)
        TEST_ERROR;
    if (canon_stored_double(dcpl, &got) < 0)
        TEST_ERROR;
    if (memcmp(&got, &(double){0x1.cp+1}, sizeof(got)) != 0)
        TEST_ERROR;
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    dcpl = H5I_INVALID_HID;
    PASSED();

    /* --- canon-08: the canonical form survives to disk and back with no
     *               plugin loaded (the case a non-HDF5 reader faces) --- */
    TESTING("canonicalization: canonical form persists and reads back plugin-free");
    if ((sid = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;
    h5_fixname(FILENAME[1], fapl, filename, sizeof(filename));
    if ((dcpl = canon_make_dcpl("{ rate = 0x1.8p+1 }")) < 0)
        TEST_ERROR;
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;
    if ((dset = H5Dcreate2(file, "dset", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0 || H5Pclose(dcpl) < 0 || H5Fclose(file) < 0)
        TEST_ERROR;
    dset = dcpl = file = H5I_INVALID_HID;

    if (H5Zunregister(CANON_FILTER_ID) < 0) /* only the stored bytes remain */
        TEST_ERROR;
    if ((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        TEST_ERROR;
    if ((dset = H5Dopen2(file, "dset", H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if ((dcpl_out = H5Dget_create_plist(dset)) < 0)
        TEST_ERROR;
    if (H5Pget_filter_params_by_idx(dcpl_out, 0, pbuf, sizeof(pbuf), &plen) < 0)
        TEST_ERROR;
    /* Canonical: no outer brace, no hex-float -- parseable as plain TOML */
    if (strcmp(pbuf, "rate = 3.0000000000000000e+00") != 0)
        TEST_ERROR;
    if (pbuf[0] == '{' || strstr(pbuf, "0x") != NULL)
        TEST_ERROR;
    if (H5Pclose(dcpl_out) < 0 || H5Dclose(dset) < 0 || H5Fclose(file) < 0 || H5Sclose(sid) < 0)
        TEST_ERROR;
    dcpl_out = dset = file = sid = H5I_INVALID_HID;
    if (H5Zregister(&canon_cls) < 0)
        TEST_ERROR;
    PASSED();

    /* --- canon-09: the canonical form is a fixed point.
     *
     * The stored string is an INPUT to set_config, not only a display
     * artefact: the RFC guarantees the stored bytes are always valid
     * set_config input, so a caller may retrieve a configuration and re-apply
     * it, and h5repack -f accepts a parameter string on the command line.
     * Re-canonicalising an already canonical string must therefore reproduce
     * the identical bytes -- it has no outer braces and no hex-float tokens
     * left to rewrite -- so re-application cannot drift.  (Note a plain
     * h5repack copy does NOT exercise this: it duplicates the source DCPL
     * with H5Pcopy, so the bytes travel as data and are never re-parsed.)
     * (RFC-HDFG-2026-001 fmt-01b)                                        --- */
    TESTING("canonicalization: stored form is a fixed point");
    {
        /* The getter truncates silently into an undersized buffer but always
         * reports the full length in plen, so every read below is checked
         * against plen -- a truncated compare must not be able to pass. */
        char s1[128];
        char s2[128];
        char s3[128];

        /* 2^-36, the case raised on HDFGroup/hdf5#6153: zfp's fixed-accuracy
         * mode derives minexp via frexp and so rounds the requested tolerance
         * DOWN to the next integer power of two, which is why a user writes
         * the exponent exactly rather than in decimal. */
        if ((dcpl = canon_make_dcpl("{ rate = 0x1p-36 }")) < 0)
            TEST_ERROR;
        if (H5Pget_filter_params_by_idx(dcpl, 0, s1, sizeof(s1), &plen) < 0)
            TEST_ERROR;
        if (plen >= sizeof(s1))
            TEST_ERROR;
        if (H5Pclose(dcpl) < 0)
            TEST_ERROR;
        dcpl = H5I_INVALID_HID;
        if (strcmp(s1, "rate = 1.4551915228366852e-11") != 0) {
            fprintf(stderr, "\n   stored \"%s\"\n", s1);
            TEST_ERROR;
        }

        /* Cycle 1: feed the stored bytes back in */
        if ((dcpl = canon_make_dcpl(s1)) < 0)
            TEST_ERROR;
        if (H5Pget_filter_params_by_idx(dcpl, 0, s2, sizeof(s2), &plen) < 0)
            TEST_ERROR;
        if (plen >= sizeof(s2))
            TEST_ERROR;
        if (H5Pclose(dcpl) < 0)
            TEST_ERROR;
        dcpl = H5I_INVALID_HID;
        if (strcmp(s1, s2) != 0)
            TEST_ERROR;

        /* Cycle 2: still byte-identical, and the value survived both hops */
        if ((dcpl = canon_make_dcpl(s2)) < 0)
            TEST_ERROR;
        if (H5Pget_filter_params_by_idx(dcpl, 0, s3, sizeof(s3), &plen) < 0)
            TEST_ERROR;
        if (plen >= sizeof(s3))
            TEST_ERROR;
        if (canon_stored_double(dcpl, &got) < 0)
            TEST_ERROR;
        if (H5Pclose(dcpl) < 0)
            TEST_ERROR;
        dcpl = H5I_INVALID_HID;
        if (strcmp(s2, s3) != 0)
            TEST_ERROR;
        if (memcmp(&got, &(double){0x1p-36}, sizeof(got)) != 0)
            TEST_ERROR;
    }
    PASSED();

    /* --- canon-10: the hex-float rewrite is value-transparent at the
     *               boundaries where it matters.
     *
     * A hex literal whose mantissa fits in 53 bits converts with no rounding
     * step at all; the canonical decimal has to come back through a
     * decimal-to-binary conversion, which C11 7.22.1.3 permits to be 1 ulp
     * off.  That ulp is only observable where a filter quantises on a binary
     * boundary -- and exact powers of two are both where such a boundary sits
     * and what a user writes in hex in the first place.  Appending the hex
     * form and appending its canonical decimal must pack the identical
     * double.  (RFC-HDFG-2026-001 fmt-01c)                               --- */
    TESTING("canonicalization: hex rewrite is value-transparent at powers of two");
    {
        /* smallest subnormal and smallest normal at the bottom, the 2^-36
         * case from the issue thread, and a spread out to the top of the
         * exponent range */
        static const int exps[] = {-1074, -1073, -1022, -1021, -100, -37, -36, -35,
                                   -1,    0,     1,     52,    53,   100, 512, 1023};
        size_t           i;

        for (i = 0; i < sizeof(exps) / sizeof(exps[0]); i++) {
            char   hexstr[64];
            char   canon[128];
            double want     = ldexp(1.0, exps[i]);
            double from_hex = 0.0;
            double from_dec = 0.0;

            snprintf(hexstr, sizeof(hexstr), "rate = 0x1p%+d", exps[i]);

            if ((dcpl = canon_make_dcpl(hexstr)) < 0)
                TEST_ERROR;
            if (canon_stored_double(dcpl, &from_hex) < 0)
                TEST_ERROR;
            if (H5Pget_filter_params_by_idx(dcpl, 0, canon, sizeof(canon), &plen) < 0)
                TEST_ERROR;
            if (plen >= sizeof(canon)) /* never compare a truncated string */
                TEST_ERROR;
            if (H5Pclose(dcpl) < 0)
                TEST_ERROR;
            dcpl = H5I_INVALID_HID;

            /* nothing a stock TOML parser would choke on may remain */
            if (strstr(canon, "0x") != NULL)
                TEST_ERROR;

            if ((dcpl = canon_make_dcpl(canon)) < 0)
                TEST_ERROR;
            if (canon_stored_double(dcpl, &from_dec) < 0)
                TEST_ERROR;
            if (H5Pclose(dcpl) < 0)
                TEST_ERROR;
            dcpl = H5I_INVALID_HID;

            if (memcmp(&from_hex, &want, sizeof(want)) != 0 ||
                memcmp(&from_dec, &want, sizeof(want)) != 0) {
                fprintf(stderr, "\n   2^%d: hex -> %a, canonical \"%s\" -> %a, want %a\n", exps[i], from_hex,
                        canon, from_dec, want);
                TEST_ERROR;
            }
        }
    }
    PASSED();

    if (H5Zunregister(CANON_FILTER_ID) < 0)
        TEST_ERROR;
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(dcpl);
        H5Pclose(dcpl_out);
        H5Dclose(dset);
        H5Fclose(file);
        H5Sclose(sid);
        H5Zunregister(CANON_FILTER_ID);
    }
    H5E_END_TRY
    return -1;
}

/* -----------------------------------------------------------------------
 * set_local must not discard the stored configuration string
 * (RFC-HDFG-2026-001 sec:dcpl-retention)
 *
 * scaleoffset, szip, nbit and shuffle each call H5P_modify_filter from
 * their set_local callback to specialise cd_values for the dataset's
 * datatype/dataspace. That is a library refinement, not a change to what
 * the user asked for, so the entry's stored string must survive it -- it
 * has to still be there for H5Dcreate to encode.
 *
 * The input below uses compact spacing ("a=1,b=2") while scaleoffset's
 * get_config emits spaced output ("a = 1, b = 2"), so the two sources are
 * distinguishable: recovering the compact form proves the stored string
 * was used, not a reconstruction.
 * ---------------------------------------------------------------------- */
static int
test_set_local_keeps_config(hid_t fapl)
{
    hid_t        dcpl = H5I_INVALID_HID, sid = H5I_INVALID_HID, file = H5I_INVALID_HID;
    hid_t        dset = H5I_INVALID_HID, dcpl_out = H5I_INVALID_HID;
    hsize_t      dims[2] = {8, 8}, chunk[2] = {4, 4};
    char         filename[1024];
    char         pbuf[H5Z_CONFIG_STRING_MAX + 1];
    size_t       plen    = 0;
    const char  *compact = "scale_type=\"int\",scale_factor=8";
    H5Z_params_t p;

    TESTING("set_local preserves the stored configuration string");

    h5_fixname(FILENAME[1], fapl, filename, sizeof(filename));

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_chunk(dcpl, 2, chunk) < 0)
        TEST_ERROR;
    p.type  = H5Z_PARAMS_STRING;
    p.u.str = compact;
    if (H5Pappend_filter(dcpl, H5Z_FILTER_SCALEOFFSET, H5Z_FLAG_MANDATORY, &p) < 0)
        TEST_ERROR;

    /* Present in the DCPL before H5Dcreate runs set_local */
    if (H5Pget_filter_params_by_idx(dcpl, 0, pbuf, sizeof(pbuf), &plen) < 0)
        TEST_ERROR;
    if (strcmp(pbuf, compact) != 0)
        TEST_ERROR;

    if ((sid = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;
    /* H5Dcreate invokes scaleoffset's set_local -> H5P_modify_filter */
    if ((dset = H5Dcreate2(file, "dset", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0 || H5Pclose(dcpl) < 0 || H5Fclose(file) < 0)
        TEST_ERROR;
    dset = dcpl = file = H5I_INVALID_HID;

    /* Still the compact form on disk -> the stored string was encoded */
    if ((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        TEST_ERROR;
    if ((dset = H5Dopen2(file, "dset", H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if ((dcpl_out = H5Dget_create_plist(dset)) < 0)
        TEST_ERROR;
    if (H5Pget_filter_params_by_idx(dcpl_out, 0, pbuf, sizeof(pbuf), &plen) < 0)
        TEST_ERROR;
    if (strcmp(pbuf, compact) != 0) {
        fprintf(stderr, "\n   expected stored \"%s\"\n   got             \"%s\"\n", compact, pbuf);
        TEST_ERROR;
    }
    if (H5Pclose(dcpl_out) < 0 || H5Dclose(dset) < 0 || H5Fclose(file) < 0 || H5Sclose(sid) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(dcpl);
        H5Pclose(dcpl_out);
        H5Dclose(dset);
        H5Fclose(file);
        H5Sclose(sid);
    }
    H5E_END_TRY
    return -1;
}

/* -----------------------------------------------------------------------
 * H5Pmodify_filter_by_idx (RFC-HDFG-2026-001 sec:modify-filter)
 *
 * Reuses the canon filter (CANON_FILTER_ID, one double "rate") so the
 * stored string and the recovered value can both be checked.
 * ---------------------------------------------------------------------- */
static int
test_modify_filter_by_idx(hid_t fapl)
{
    hid_t        dcpl = H5I_INVALID_HID, sid = H5I_INVALID_HID, file = H5I_INVALID_HID;
    hid_t        dset = H5I_INVALID_HID, dcpl_out = H5I_INVALID_HID;
    hsize_t      dims[2] = {8, 8};
    char         filename[1024];
    char         pbuf[H5Z_CONFIG_STRING_MAX + 1];
    size_t       plen      = 0;
    double       got       = 0.0;
    unsigned     flags_out = 0;
    unsigned     cd[8];
    size_t       cd_nelmts = 8;
    char         nm[64];
    unsigned     cfg = 0;
    H5Z_params_t p;

    if (H5Zregister(&canon_cls) < 0)
        TEST_ERROR;

    /* --- mod-01: STRING replaces cd_values and keeps a stored string --- */
    TESTING("H5Pmodify_filter_by_idx: string form replaces config in place");
    if ((dcpl = canon_make_dcpl("rate = 1.5")) < 0)
        TEST_ERROR;
    p.type  = H5Z_PARAMS_STRING;
    p.u.str = "rate = 2.5";
    if (H5Pmodify_filter_by_idx(dcpl, 0, H5Z_FLAG_MANDATORY, &p) < 0)
        TEST_ERROR;
    if (H5Pget_filter_params_by_idx(dcpl, 0, pbuf, sizeof(pbuf), &plen) < 0)
        TEST_ERROR;
    if (strcmp(pbuf, "rate = 2.5") != 0)
        TEST_ERROR;
    if (canon_stored_double(dcpl, &got) < 0)
        TEST_ERROR;
    if (memcmp(&got, &(double){2.5}, sizeof(got)) != 0)
        TEST_ERROR;
    /* Position and filter ID unchanged, and still exactly one entry */
    if (H5Pget_nfilters(dcpl) != 1)
        TEST_ERROR;
    cd_nelmts = 8;
    if (H5Pget_filter2(dcpl, 0, &flags_out, &cd_nelmts, cd, sizeof(nm), nm, &cfg) != CANON_FILTER_ID)
        TEST_ERROR;
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    dcpl = H5I_INVALID_HID;
    PASSED();

    /* --- mod-02: canonicalization applies on the modify path too --- */
    TESTING("H5Pmodify_filter_by_idx: replacement string is canonicalized");
    if ((dcpl = canon_make_dcpl("rate = 1.5")) < 0)
        TEST_ERROR;
    p.type  = H5Z_PARAMS_STRING;
    p.u.str = "{rate = 0x1.8p+1}";
    if (H5Pmodify_filter_by_idx(dcpl, 0, H5Z_FLAG_MANDATORY, &p) < 0)
        TEST_ERROR;
    if (H5Pget_filter_params_by_idx(dcpl, 0, pbuf, sizeof(pbuf), &plen) < 0)
        TEST_ERROR;
    if (strcmp(pbuf, "rate = 3.0000000000000000e+00") != 0)
        TEST_ERROR;
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    dcpl = H5I_INVALID_HID;
    PASSED();

    /* --- mod-03: CDVALUES form clears the stored string --- */
    TESTING("H5Pmodify_filter_by_idx: cd_values form clears the stored string");
    if ((dcpl = canon_make_dcpl("rate = 1.5")) < 0)
        TEST_ERROR;
    {
        double   v      = 4.5;
        unsigned raw[2] = {0, 0};

        memcpy(raw, &v, sizeof(v));
        p.type            = H5Z_PARAMS_CDVALUES;
        p.u.raw.cd_nelmts = 2;
        p.u.raw.cd_values = raw;
        if (H5Pmodify_filter_by_idx(dcpl, 0, H5Z_FLAG_MANDATORY, &p) < 0)
            TEST_ERROR;
    }
    if (H5Pget_filter_params_by_idx(dcpl, 0, pbuf, sizeof(pbuf), &plen) < 0)
        TEST_ERROR;
    /* Stored string gone -> get_config reconstruction, which uses %.16e */
    if (strcmp(pbuf, "rate = 4.5000000000000000e+00") != 0) {
        fprintf(stderr, "\n   got \"%s\"\n", pbuf);
        TEST_ERROR;
    }
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    dcpl = H5I_INVALID_HID;
    PASSED();

    /* --- mod-04: index addressing distinguishes duplicate filter IDs --- */
    TESTING("H5Pmodify_filter_by_idx: duplicate filter IDs addressed by index");
    if ((dcpl = canon_make_dcpl("rate = 1.5")) < 0)
        TEST_ERROR;
    p.type  = H5Z_PARAMS_STRING;
    p.u.str = "rate = 2.5";
    if (H5Pappend_filter(dcpl, CANON_FILTER_ID, H5Z_FLAG_MANDATORY, &p) < 0)
        TEST_ERROR;
    if (H5Pget_nfilters(dcpl) != 2)
        TEST_ERROR;
    /* Edit the second entry only */
    p.u.str = "rate = 9.5";
    if (H5Pmodify_filter_by_idx(dcpl, 1, H5Z_FLAG_MANDATORY, &p) < 0)
        TEST_ERROR;
    if (H5Pget_filter_params_by_idx(dcpl, 0, pbuf, sizeof(pbuf), &plen) < 0)
        TEST_ERROR;
    if (strcmp(pbuf, "rate = 1.5") != 0) /* entry 0 untouched */
        TEST_ERROR;
    if (H5Pget_filter_params_by_idx(dcpl, 1, pbuf, sizeof(pbuf), &plen) < 0)
        TEST_ERROR;
    if (strcmp(pbuf, "rate = 9.5") != 0)
        TEST_ERROR;
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    dcpl = H5I_INVALID_HID;
    PASSED();

    /* --- mod-05: errors, and the entry survives a rejected edit --- */
    TESTING("H5Pmodify_filter_by_idx: errors leave the entry unchanged");
    if ((dcpl = canon_make_dcpl("rate = 1.5")) < 0)
        TEST_ERROR;

    /* index out of range */
    p.type  = H5Z_PARAMS_STRING;
    p.u.str = "rate = 2.5";
    H5E_BEGIN_TRY
    {
        if (H5Pmodify_filter_by_idx(dcpl, 1, H5Z_FLAG_MANDATORY, &p) >= 0)
            TEST_ERROR;
    }
    H5E_END_TRY

    /* invalid flags */
    H5E_BEGIN_TRY
    {
        if (H5Pmodify_filter_by_idx(dcpl, 0, 0xFFFFFFFFu, &p) >= 0)
            TEST_ERROR;
    }
    H5E_END_TRY

    /* set_config rejects the string (canon_set_config fails a type mismatch) */
    p.u.str = "rate = \"not a number\"";
    H5E_BEGIN_TRY
    {
        if (H5Pmodify_filter_by_idx(dcpl, 0, H5Z_FLAG_MANDATORY, &p) >= 0)
            TEST_ERROR;
    }
    H5E_END_TRY

    /* After every rejected edit the original configuration is intact */
    if (H5Pget_filter_params_by_idx(dcpl, 0, pbuf, sizeof(pbuf), &plen) < 0)
        TEST_ERROR;
    if (strcmp(pbuf, "rate = 1.5") != 0) {
        fprintf(stderr, "\n   entry was disturbed: \"%s\"\n", pbuf);
        TEST_ERROR;
    }
    if (canon_stored_double(dcpl, &got) < 0)
        TEST_ERROR;
    if (memcmp(&got, &(double){1.5}, sizeof(got)) != 0)
        TEST_ERROR;
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    dcpl = H5I_INVALID_HID;
    PASSED();

    /* --- mod-06 (fmt-07): the replacement string is what reaches disk --- */
    TESTING("H5Pmodify_filter_by_idx: replacement string persists, not the original");
    h5_fixname(FILENAME[1], fapl, filename, sizeof(filename));
    if ((dcpl = canon_make_dcpl("rate = 1.5")) < 0)
        TEST_ERROR;
    p.type  = H5Z_PARAMS_STRING;
    p.u.str = "rate = 7.25";
    if (H5Pmodify_filter_by_idx(dcpl, 0, H5Z_FLAG_MANDATORY, &p) < 0)
        TEST_ERROR;
    if ((sid = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;
    if ((dset = H5Dcreate2(file, "dset", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0 || H5Pclose(dcpl) < 0 || H5Fclose(file) < 0)
        TEST_ERROR;
    dset = dcpl = file = H5I_INVALID_HID;

    /* Drop the plugin so only the persisted string can answer */
    if (H5Zunregister(CANON_FILTER_ID) < 0)
        TEST_ERROR;
    if ((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        TEST_ERROR;
    if ((dset = H5Dopen2(file, "dset", H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if ((dcpl_out = H5Dget_create_plist(dset)) < 0)
        TEST_ERROR;
    if (H5Pget_filter_params_by_idx(dcpl_out, 0, pbuf, sizeof(pbuf), &plen) < 0)
        TEST_ERROR;
    if (strcmp(pbuf, "rate = 7.25") != 0) {
        fprintf(stderr, "\n   expected \"rate = 7.25\" on disk, got \"%s\"\n", pbuf);
        TEST_ERROR;
    }
    if (H5Pclose(dcpl_out) < 0 || H5Dclose(dset) < 0 || H5Fclose(file) < 0 || H5Sclose(sid) < 0)
        TEST_ERROR;
    dcpl_out = dset = file = sid = H5I_INVALID_HID;
    if (H5Zregister(&canon_cls) < 0)
        TEST_ERROR;
    PASSED();

    if (H5Zunregister(CANON_FILTER_ID) < 0)
        TEST_ERROR;
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(dcpl);
        H5Pclose(dcpl_out);
        H5Dclose(dset);
        H5Fclose(file);
        H5Sclose(sid);
        H5Zunregister(CANON_FILTER_ID);
    }
    H5E_END_TRY
    return -1;
}

/* -----------------------------------------------------------------------
 * main
 * ---------------------------------------------------------------------- */
int
main(void)
{
    hid_t fapl    = H5I_INVALID_HID;
    hid_t file    = H5I_INVALID_HID;
    int   nerrors = 0;
    char  filename[1024];

    h5_test_init();
    fapl = h5_fileaccess();

    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        goto error;

    /* Parser tests */
    nerrors += test_parser() < 0 ? 1 : 0;

    /* canonical_name display test */
    nerrors += test_canonical_name_display() < 0 ? 1 : 0;

    /* H5Z_class3_t name field tests */
    nerrors += test_class3_name() < 0 ? 1 : 0;

    /* H5Pappend_filter callback contract tests */
    nerrors += test_callback_contracts() < 0 ? 1 : 0;

    /* Modify-filter pattern (H5Pget_filter_by_id2 + H5Pmodify_filter) */
    nerrors += test_modify_filter_absent() < 0 ? 1 : 0;
    nerrors += test_modify_filter_pattern() < 0 ? 1 : 0;

    /* Round-trip tests */
    nerrors += test_roundtrip_deflate(file) < 0 ? 1 : 0;
    nerrors += test_roundtrip_shuffle(file) < 0 ? 1 : 0;
    nerrors += test_roundtrip_fletcher32(file) < 0 ? 1 : 0;
    nerrors += test_scaleoffset_params(file) < 0 ? 1 : 0;

    /* Regression tests */
    nerrors += test_regression_old_api(file) < 0 ? 1 : 0;
    nerrors += test_regression_filter2_appends() < 0 ? 1 : 0;

    /* Additional coverage tests */
    nerrors += test_empty_string_fast_path() < 0 ? 1 : 0;
    nerrors += test_cdvalues_path() < 0 ? 1 : 0;
    nerrors += test_cdvalues_null_check() < 0 ? 1 : 0;
    nerrors += test_cdvalues_no_name_pollution() < 0 ? 1 : 0;
    nerrors += test_canonical_name_persistence() < 0 ? 1 : 0;
    nerrors += test_name_id_fallback() < 0 ? 1 : 0;
    nerrors += test_canonical_name_length_limit() < 0 ? 1 : 0;
    nerrors += test_config_string_max() < 0 ? 1 : 0;
    nerrors += test_config_string_max_boundary() < 0 ? 1 : 0;
    nerrors += test_config_get_str_null_buf_size() < 0 ? 1 : 0;
    nerrors += test_set_get_config_callbacks() < 0 ? 1 : 0;
    nerrors += test_get_filter_info2_builtin() < 0 ? 1 : 0;

    /* filter2 context passthrough: dxpl_id, scaled, ndims */
    nerrors += test_filter2_context_passthrough(file) < 0 ? 1 : 0;

    /* On-disk configuration-string storage (pipeline v3) */
    nerrors += test_config_string_ondisk(fapl) < 0 ? 1 : 0;

    /* set_local must not discard the stored configuration string */
    nerrors += test_set_local_keeps_config(fapl) < 0 ? 1 : 0;

    /* Canonicalization of the persisted configuration string */
    nerrors += test_config_canonicalization(fapl) < 0 ? 1 : 0;

    /* A v2 plugin and a v3 plugin in the same pipeline */
    nerrors += test_mixed_v2_v3_pipeline(fapl) < 0 ? 1 : 0;

    /* H5Pmodify_filter_by_idx */
    nerrors += test_modify_filter_by_idx(fapl) < 0 ? 1 : 0;

    if (H5Fclose(file) < 0)
        goto error;

    h5_cleanup(FILENAME, fapl);

    if (nerrors)
        goto error;

    printf("All tfilter2 tests passed.\n");
    return EXIT_SUCCESS;

error:
    puts("***** TFILTER2 TESTS FAILED *****");
    H5E_BEGIN_TRY
    {
        H5Fclose(file);
        H5Pclose(fapl);
    }
    H5E_END_TRY
    return EXIT_FAILURE;
}
