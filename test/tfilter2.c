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

static const char *FILENAME[] = {"tfilter2",
                                 "tfilter2_blob",
                                 "tfilter2_blob_custom",
                                 "tfilter2_cfg",
                                 "tfilter2_cfg_copy",
                                 "tfilter2_blob_delete",
                                 "tfilter2_blob_dup",
                                 "tfilter2_blob_percopy",
                                 "tfilter2_blob_usecaseb",
                                 "tfilter2_blob_oversized",
                                 NULL};

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
        NULL,                /* write_blob: use default global-heap storage */
        NULL,                /* read_blob  */
        NULL,                /* close_blob */
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
            NULL,             /* write_blob: use default global-heap storage */
            NULL,             /* read_blob  */
            NULL,             /* close_blob */
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
            NULL,               /* write_blob: use default global-heap storage */
            NULL,               /* read_blob  */
            NULL,               /* close_blob */
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
        NULL,                 /* write_blob: use default global-heap storage */
        NULL,                 /* read_blob  */
        NULL,                 /* close_blob */
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
        NULL,                 /* write_blob: use default global-heap storage */
        NULL,                 /* read_blob  */
        NULL,                 /* close_blob */
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
        NULL,               /* write_blob: use default global-heap storage */
        NULL,               /* read_blob  */
        NULL,               /* close_blob */
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
        NULL,                     /* write_blob: use default global-heap storage */
        NULL,                     /* read_blob  */
        NULL,                     /* close_blob */
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
        NULL,                /* write_blob: use default global-heap storage */
        NULL,                /* read_blob  */
        NULL,                /* close_blob */
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
        NULL,                  /* write_blob: use default global-heap storage */
        NULL,                  /* read_blob  */
        NULL,                  /* close_blob */
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
        NULL,                 /* write_blob: use default global-heap storage */
        NULL,                 /* read_blob  */
        NULL,                 /* close_blob */
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
    2,
    CTXPASS_FILTER_ID,
    1,
    1,
    "test_ctxpass_filter",
    NULL,
    NULL,
    NULL,
    ctxpass_filter_cb,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
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
    NULL,                   /* write_blob      */
    NULL,                   /* read_blob       */
    NULL,                   /* close_blob      */
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
    h5_fixname(FILENAME[3], fapl, filename, sizeof(filename));
    h5_fixname(FILENAME[4], fapl, filename2, sizeof(filename2));

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
 * In-file blob configuration storage (H5Pappend_filter_blob)
 * ---------------------------------------------------------------------- */

#define BLOB_DEFAULT_FILTER_ID 522
#define BLOB_CUSTOM_FILTER_ID  523
#define BLOB_TEST_SIZE         (64 * 1024)
#define BLOB_MAGIC             "TFILTER2BLOBMAGIC"
#define BLOB_MAGIC_LEN         (sizeof(BLOB_MAGIC) - 1)

static size_t
blob_passthrough_func(unsigned int flags, size_t cd_nelmts, const unsigned int *cd_values,
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

/* Fill a blob buffer with a leading magic marker and a deterministic pattern */
static void
blob_fill_pattern(unsigned char *buf, size_t size)
{
    memcpy(buf, BLOB_MAGIC, BLOB_MAGIC_LEN);
    for (size_t i = BLOB_MAGIC_LEN; i < size; i++)
        buf[i] = (unsigned char)(i * 7 + 3);
}

/* Locate NEEDLE in HAYSTACK; returns true when found */
static bool
blob_find_bytes(const unsigned char *haystack, size_t hay_len, const unsigned char *needle, size_t nlen)
{
    if (nlen == 0 || hay_len < nlen)
        return false;
    for (size_t i = 0; i + nlen <= hay_len; i++)
        if (haystack[i] == needle[0] && 0 == memcmp(haystack + i, needle, nlen))
            return true;
    return false;
}

/* Verify that PLIST's encoded form contains the blob pattern bytes */
static int
blob_check_encoded_plist(hid_t plist, const unsigned char *blob, size_t blob_size)
{
    void  *enc_buf  = NULL;
    size_t enc_size = 0;

    if (H5Pencode2(plist, NULL, &enc_size, H5P_DEFAULT) < 0)
        goto error;
    if (NULL == (enc_buf = malloc(enc_size)))
        goto error;
    if (H5Pencode2(plist, enc_buf, &enc_size, H5P_DEFAULT) < 0)
        goto error;
    if (!blob_find_bytes((unsigned char *)enc_buf, enc_size, blob, blob_size))
        goto error;
    free(enc_buf);
    return 0;

error:
    free(enc_buf);
    return -1;
}

/* Verify H5Pget_filter_blob(plist, idx, ...) returns exactly EXPECTED via
 * both the size-query form (buf == NULL) and the fill form. */
static int
blob_check_getter(hid_t plist, unsigned idx, const unsigned char *expected, size_t expected_size)
{
    unsigned char *buf  = NULL;
    size_t         size = 0;

    if (H5Pget_filter_blob(plist, idx, 0, NULL, &size) < 0)
        goto error;
    if (size != expected_size)
        goto error;

    if (expected_size > 0) {
        if (NULL == (buf = (unsigned char *)malloc(expected_size)))
            goto error;
        size = expected_size;
        if (H5Pget_filter_blob(plist, idx, 0, buf, &size) < 0)
            goto error;
        if (size != expected_size)
            goto error;
        if (memcmp(buf, expected, expected_size) != 0)
            goto error;
        free(buf);
    }
    return 0;

error:
    free(buf);
    return -1;
}

/* Default (global-heap) blob storage: create/write/reopen/read round-trip,
 * H5Pcopy and H5Pencode/H5Pdecode propagation, and dataset delete. */
static int
test_blob_default_storage(hid_t fapl)
{
    static const H5Z_class3_t blob_cls = {
        2,                      /* version         */
        BLOB_DEFAULT_FILTER_ID, /* id              */
        1,                      /* encoder_present */
        1,                      /* decoder_present */
        "blob_default_filter",  /* canonical_name  */
        NULL,                   /* description     */
        NULL,                   /* can_apply       */
        NULL,                   /* set_local       */
        blob_passthrough_func,  /* filter          */
        NULL,                   /* set_config      */
        NULL,                   /* get_config      */
        NULL,                   /* write_blob: use default global-heap storage */
        NULL,                   /* read_blob       */
        NULL,                   /* close_blob      */
    };
    char           filename[1024];
    unsigned char *blob     = NULL;
    void          *enc_buf  = NULL;
    size_t         enc_size = 0;
    hid_t          file = H5I_INVALID_HID, sid = H5I_INVALID_HID;
    hid_t          dcpl = H5I_INVALID_HID, dcpl_copy = H5I_INVALID_HID, dcpl_dec = H5I_INVALID_HID;
    hid_t          dcpl_out = H5I_INVALID_HID;
    hid_t          dset = H5I_INVALID_HID, dset2 = H5I_INVALID_HID, dset3 = H5I_INVALID_HID;
    hsize_t        dims[2] = {8, 8}, chunk[2] = {4, 4};
    int            wdata[8][8], rdata[8][8];
    H5Z_filter_t   filt_id;
    unsigned       flags;
    size_t         cd_nelmts = 0;
    char           fname[64];

    TESTING("H5Pappend_filter_blob: default global-heap storage round-trip");

    if (H5Zregister(&blob_cls) < 0)
        TEST_ERROR;

    if (NULL == (blob = (unsigned char *)malloc(BLOB_TEST_SIZE)))
        TEST_ERROR;
    blob_fill_pattern(blob, BLOB_TEST_SIZE);

    for (int i = 0; i < 8; i++)
        for (int j = 0; j < 8; j++)
            wdata[i][j] = i * 8 + j;

    /* Build a blob-bearing DCPL */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_chunk(dcpl, 2, chunk) < 0)
        TEST_ERROR;
    if (H5Pappend_filter_blob(dcpl, BLOB_DEFAULT_FILTER_ID, 0, blob, BLOB_TEST_SIZE) < 0)
        TEST_ERROR;

    /* H5Pget_filter_blob works immediately, before any file I/O */
    if (blob_check_getter(dcpl, 0, blob, BLOB_TEST_SIZE) < 0)
        TEST_ERROR;

    /* The caller's buffer must be copied, so scribbling on it now must not
     * affect what reaches the file */
    memset(blob, 0xEE, BLOB_TEST_SIZE);
    blob_fill_pattern(blob, BLOB_TEST_SIZE); /* restore for later comparisons */

    /* Create three datasets: from the original DCPL, from an H5Pcopy of it,
     * and from an H5Pencode/H5Pdecode round-trip of it */
    h5_fixname(FILENAME[1], fapl, filename, sizeof(filename));
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;
    if ((sid = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    if ((dset = H5Dcreate2(file, "blob_dset1", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata) < 0)
        TEST_ERROR;

    if ((dcpl_copy = H5Pcopy(dcpl)) < 0)
        TEST_ERROR;
    if ((dset2 = H5Dcreate2(file, "blob_dset2", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl_copy, H5P_DEFAULT)) <
        0)
        TEST_ERROR;
    if (H5Dwrite(dset2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata) < 0)
        TEST_ERROR;

    if (H5Pencode2(dcpl, NULL, &enc_size, H5P_DEFAULT) < 0)
        TEST_ERROR;
    if (NULL == (enc_buf = malloc(enc_size)))
        TEST_ERROR;
    if (H5Pencode2(dcpl, enc_buf, &enc_size, H5P_DEFAULT) < 0)
        TEST_ERROR;
    if ((dcpl_dec = H5Pdecode(enc_buf)) < 0)
        TEST_ERROR;
    if ((dset3 = H5Dcreate2(file, "blob_dset3", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl_dec, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dwrite(dset3, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata) < 0)
        TEST_ERROR;

    if (H5Dclose(dset) < 0 || H5Dclose(dset2) < 0 || H5Dclose(dset3) < 0)
        TEST_ERROR;
    dset = dset2 = dset3 = H5I_INVALID_HID;
    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;

    /* Reopen and verify each dataset: data reads back, the pipeline carries
     * the filter, and the recovered DCPL carries the blob bytes */
    if ((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;
    for (int d = 1; d <= 3; d++) {
        snprintf(fname, sizeof(fname), "blob_dset%d", d);
        if ((dset = H5Dopen2(file, fname, H5P_DEFAULT)) < 0)
            TEST_ERROR;
        memset(rdata, 0, sizeof(rdata));
        if (H5Dread(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata) < 0)
            TEST_ERROR;
        if (memcmp(wdata, rdata, sizeof(wdata)) != 0)
            TEST_ERROR;

        if ((dcpl_out = H5Dget_create_plist(dset)) < 0)
            TEST_ERROR;
        if (H5Pget_nfilters(dcpl_out) != 1)
            TEST_ERROR;
        cd_nelmts = 0;
        if ((filt_id = H5Pget_filter2(dcpl_out, 0, &flags, &cd_nelmts, NULL, 0, NULL, NULL)) < 0)
            TEST_ERROR;
        if (filt_id != BLOB_DEFAULT_FILTER_ID)
            TEST_ERROR;
        if (blob_check_encoded_plist(dcpl_out, blob, BLOB_TEST_SIZE) < 0)
            TEST_ERROR;
        if (blob_check_getter(dcpl_out, 0, blob, BLOB_TEST_SIZE) < 0)
            TEST_ERROR;
        if (H5Pclose(dcpl_out) < 0)
            TEST_ERROR;
        dcpl_out = H5I_INVALID_HID;
        if (H5Dclose(dset) < 0)
            TEST_ERROR;
        dset = H5I_INVALID_HID;
    }

    /* Deleting a blob-bearing dataset reclaims its heap object */
    if (H5Ldelete(file, "blob_dset1", H5P_DEFAULT) < 0)
        TEST_ERROR;

    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;

    if (H5Sclose(sid) < 0 || H5Pclose(dcpl) < 0 || H5Pclose(dcpl_copy) < 0 || H5Pclose(dcpl_dec) < 0)
        TEST_ERROR;
    sid = dcpl = dcpl_copy = dcpl_dec = H5I_INVALID_HID;
    if (H5Zunregister(BLOB_DEFAULT_FILTER_ID) < 0)
        TEST_ERROR;

    free(blob);
    free(enc_buf);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(dset);
        H5Dclose(dset2);
        H5Dclose(dset3);
        H5Pclose(dcpl);
        H5Pclose(dcpl_copy);
        H5Pclose(dcpl_dec);
        H5Pclose(dcpl_out);
        H5Sclose(sid);
        H5Fclose(file);
        H5Zunregister(BLOB_DEFAULT_FILTER_ID);
    }
    H5E_END_TRY
    free(blob);
    free(enc_buf);
    return -1;
}

/* Custom blob callback state */
#define BLOB_CUSTOM_STORE_MAX 1024
static unsigned char blob_custom_store[BLOB_CUSTOM_STORE_MAX]; /* stands in for filter-managed storage */
static size_t        blob_custom_store_size = 0;
static int           blob_write_count       = 0;
static int           blob_read_count        = 0;
static int           blob_close_count       = 0;

static herr_t
blob_custom_write(hid_t file_id, const void *buf, size_t size, H5Z_blob_loc_t *loc_out)
{
    if (H5Iget_type(file_id) != H5I_FILE)
        return FAIL;
    if (size > sizeof(blob_custom_store))
        return FAIL;
    memcpy(blob_custom_store, buf, size);
    blob_custom_store_size = size;
    blob_write_count++;
    /* Arbitrary token the library must hand back unchanged at read time */
    loc_out->addr = (haddr_t)0x1234;
    loc_out->idx  = 42;
    return SUCCEED;
}

static herr_t
blob_custom_read(hid_t file_id, H5Z_blob_loc_t loc, void **buf_out, size_t *size_out)
{
    if (H5Iget_type(file_id) != H5I_FILE)
        return FAIL;
    if (loc.addr != (haddr_t)0x1234 || loc.idx != 42)
        return FAIL;
    if (NULL == (*buf_out = malloc(blob_custom_store_size)))
        return FAIL;
    memcpy(*buf_out, blob_custom_store, blob_custom_store_size);
    *size_out = blob_custom_store_size;
    blob_read_count++;
    return SUCCEED;
}

static herr_t
blob_custom_close(void *buf, size_t H5_ATTR_UNUSED size)
{
    free(buf);
    blob_close_count++;
    return SUCCEED;
}

/* Custom write_blob/read_blob/close_blob callbacks: invocation points,
 * locator round-trip, allocator symmetry, and class-info reporting. */
static int
test_blob_custom_callbacks(hid_t fapl)
{
    static const H5Z_class3_t blob_cls = {
        2,                     /* version         */
        BLOB_CUSTOM_FILTER_ID, /* id              */
        1,                     /* encoder_present */
        1,                     /* decoder_present */
        "blob_custom_filter",  /* canonical_name  */
        NULL,                  /* description     */
        NULL,                  /* can_apply       */
        NULL,                  /* set_local       */
        blob_passthrough_func, /* filter          */
        NULL,                  /* set_config      */
        NULL,                  /* get_config      */
        blob_custom_write,     /* write_blob      */
        blob_custom_read,      /* read_blob       */
        blob_custom_close,     /* close_blob      */
    };
    char             filename[1024];
    unsigned char    small_blob[1024];
    H5Z_class_info_t info;
    hid_t            file = H5I_INVALID_HID, sid = H5I_INVALID_HID;
    hid_t            dcpl = H5I_INVALID_HID, dset = H5I_INVALID_HID;
    hsize_t          dims[2] = {8, 8}, chunk[2] = {4, 4};
    int              wdata[8][8], rdata[8][8];

    TESTING("H5Pappend_filter_blob: custom write/read/close callbacks");

    blob_custom_store_size = 0;
    blob_write_count = blob_read_count = blob_close_count = 0;

    if (H5Zregister(&blob_cls) < 0)
        TEST_ERROR;

    /* Registry reports the custom callbacks */
    if (H5Zget_filter_class_info(BLOB_CUSTOM_FILTER_ID, &info) < 0)
        TEST_ERROR;
    if (!info.has_blob_callbacks)
        TEST_ERROR;

    blob_fill_pattern(small_blob, sizeof(small_blob));
    for (int i = 0; i < 8; i++)
        for (int j = 0; j < 8; j++)
            wdata[i][j] = i - j;

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_chunk(dcpl, 2, chunk) < 0)
        TEST_ERROR;
    if (H5Pappend_filter_blob(dcpl, BLOB_CUSTOM_FILTER_ID, 0, small_blob, sizeof(small_blob)) < 0)
        TEST_ERROR;

    h5_fixname(FILENAME[2], fapl, filename, sizeof(filename));
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;
    if ((sid = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;
    if ((dset = H5Dcreate2(file, "blob_dset", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* write_blob fires once at create; the callback saw the exact bytes */
    if (blob_write_count != 1 || blob_read_count != 0)
        TEST_ERROR;
    if (blob_custom_store_size != sizeof(small_blob))
        TEST_ERROR;
    if (memcmp(blob_custom_store, small_blob, sizeof(small_blob)) != 0)
        TEST_ERROR;

    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;
    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;

    /* read_blob fires once at open with the locator write_blob produced */
    if ((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        TEST_ERROR;
    if ((dset = H5Dopen2(file, "blob_dset", H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (blob_read_count != 1)
        TEST_ERROR;

    memset(rdata, 0, sizeof(rdata));
    if (H5Dread(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata) < 0)
        TEST_ERROR;
    if (memcmp(wdata, rdata, sizeof(wdata)) != 0)
        TEST_ERROR;

    /* H5Pget_filter_blob works uniformly regardless of custom vs default
     * storage: the bytes came back via read_blob, but the getter doesn't
     * care how they got into the property list. */
    {
        hid_t dcpl_out = H5Dget_create_plist(dset);
        int   getter_ret;

        if (dcpl_out < 0)
            TEST_ERROR;
        getter_ret = blob_check_getter(dcpl_out, 0, small_blob, sizeof(small_blob));
        H5Pclose(dcpl_out);
        if (getter_ret < 0)
            TEST_ERROR;
    }

    /* close_blob releases the callback-allocated buffer at dataset close */
    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;
    if (blob_close_count != 1)
        TEST_ERROR;

    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;
    if (H5Sclose(sid) < 0 || H5Pclose(dcpl) < 0)
        TEST_ERROR;
    sid = dcpl = H5I_INVALID_HID;
    if (H5Zunregister(BLOB_CUSTOM_FILTER_ID) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(dset);
        H5Pclose(dcpl);
        H5Sclose(sid);
        H5Fclose(file);
        H5Zunregister(BLOB_CUSTOM_FILTER_ID);
    }
    H5E_END_TRY
    return -1;
}

/* Argument validation and the no-blob degenerate case */
static int
test_blob_errors(void)
{
    static const H5Z_class3_t blob_cls = {
        2,                      /* version         */
        BLOB_DEFAULT_FILTER_ID, /* id              */
        1,                      /* encoder_present */
        1,                      /* decoder_present */
        "blob_default_filter",  /* canonical_name  */
        NULL,                   /* description     */
        NULL,                   /* can_apply       */
        NULL,                   /* set_local       */
        blob_passthrough_func,  /* filter          */
        NULL,                   /* set_config      */
        NULL,                   /* get_config      */
        NULL,                   /* write_blob      */
        NULL,                   /* read_blob       */
        NULL,                   /* close_blob      */
    };
    unsigned char    bytes[16] = {1, 2, 3, 4};
    H5Z_class_info_t info;
    hid_t            dcpl = H5I_INVALID_HID;
    herr_t           ret;

    TESTING("H5Pappend_filter_blob: argument validation");

    if (H5Zregister(&blob_cls) < 0)
        TEST_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    /* NULL buf with nonzero size */
    H5E_BEGIN_TRY
    {
        ret = H5Pappend_filter_blob(dcpl, BLOB_DEFAULT_FILTER_ID, 0, NULL, 16);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;

    /* non-NULL buf with zero size */
    H5E_BEGIN_TRY
    {
        ret = H5Pappend_filter_blob(dcpl, BLOB_DEFAULT_FILTER_ID, 0, bytes, 0);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;

    /* unregistered, unloadable filter */
    H5E_BEGIN_TRY
    {
        ret = H5Pappend_filter_blob(dcpl, 801, 0, bytes, sizeof(bytes));
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;

    /* NULL buf with zero size appends the filter with no blob attached */
    if (H5Pappend_filter_blob(dcpl, BLOB_DEFAULT_FILTER_ID, 0, NULL, 0) < 0)
        TEST_ERROR;
    if (H5Pget_nfilters(dcpl) != 1)
        TEST_ERROR;

    /* Default-storage filters report no custom blob callbacks */
    if (H5Zget_filter_class_info(BLOB_DEFAULT_FILTER_ID, &info) < 0)
        TEST_ERROR;
    if (info.has_blob_callbacks)
        TEST_ERROR;

    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    if (H5Zunregister(BLOB_DEFAULT_FILTER_ID) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(dcpl);
        H5Zunregister(BLOB_DEFAULT_FILTER_ID);
    }
    H5E_END_TRY
    return -1;
}

/* H5Pget_filter_blob: no-blob-attached, out-of-range index, NULL size
 * pointer, truncated-buffer reporting, and offset-based partial/streaming
 * reads. */
static int
test_blob_getter(void)
{
    static const H5Z_class3_t blob_cls = {
        2,                      /* version         */
        BLOB_DEFAULT_FILTER_ID, /* id              */
        1,                      /* encoder_present */
        1,                      /* decoder_present */
        "blob_default_filter",  /* canonical_name  */
        NULL,                   /* description     */
        NULL,                   /* can_apply       */
        NULL,                   /* set_local       */
        blob_passthrough_func,  /* filter          */
        NULL,                   /* set_config      */
        NULL,                   /* get_config      */
        NULL,                   /* write_blob      */
        NULL,                   /* read_blob       */
        NULL,                   /* close_blob      */
    };
    unsigned char bytes[64];
    unsigned char half[32];
    hid_t         dcpl = H5I_INVALID_HID;
    size_t        size;
    herr_t        ret;

    TESTING("H5Pget_filter_blob: no blob, bad index, truncation, offset streaming");

    if (H5Zregister(&blob_cls) < 0)
        TEST_ERROR;
    blob_fill_pattern(bytes, sizeof(bytes));

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    /* A filter with no blob attached: size query reports 0, not an error */
    if (H5Pappend_filter_blob(dcpl, BLOB_DEFAULT_FILTER_ID, 0, NULL, 0) < 0)
        TEST_ERROR;
    size = 999;
    if (H5Pget_filter_blob(dcpl, 0, 0, NULL, &size) < 0)
        TEST_ERROR;
    if (size != 0)
        TEST_ERROR;

    /* Out-of-range index */
    H5E_BEGIN_TRY
    {
        size = 0;
        ret  = H5Pget_filter_blob(dcpl, 1, 0, NULL, &size);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;

    /* NULL size pointer is rejected regardless of buf */
    H5E_BEGIN_TRY
    {
        ret = H5Pget_filter_blob(dcpl, 0, 0, NULL, NULL);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;

    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    dcpl = H5I_INVALID_HID;

    /* Truncation: buffer smaller than the blob copies only what fits, but
     * *size still reports the blob's full (untruncated) length. */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pappend_filter_blob(dcpl, BLOB_DEFAULT_FILTER_ID, 0, bytes, sizeof(bytes)) < 0)
        TEST_ERROR;
    size = sizeof(half);
    if (H5Pget_filter_blob(dcpl, 0, 0, half, &size) < 0)
        TEST_ERROR;
    if (size != sizeof(bytes)) /* full size reported despite truncated copy */
        TEST_ERROR;
    if (memcmp(half, bytes, sizeof(half)) != 0)
        TEST_ERROR;

    /* Nonzero offset: remaining count and bytes both start from offset.
     * half's capacity (32) is smaller than what remains (64-16=48), so
     * only the first sizeof(half) bytes from the offset were copied. */
    size = sizeof(half);
    if (H5Pget_filter_blob(dcpl, 0, 16, half, &size) < 0)
        TEST_ERROR;
    if (size != sizeof(bytes) - 16)
        TEST_ERROR;
    if (memcmp(half, bytes + 16, sizeof(half)) != 0)
        TEST_ERROR;

    /* Offset at exactly the blob's end, and past it: both report 0
     * remaining, neither is an error */
    size = sizeof(half);
    if (H5Pget_filter_blob(dcpl, 0, sizeof(bytes), half, &size) < 0)
        TEST_ERROR;
    if (size != 0)
        TEST_ERROR;
    size = sizeof(half);
    if (H5Pget_filter_blob(dcpl, 0, sizeof(bytes) + 1000, half, &size) < 0)
        TEST_ERROR;
    if (size != 0)
        TEST_ERROR;

    /* Streaming: read the whole blob back in small chunks via repeated
     * calls with a growing offset, and verify the reassembled bytes match. */
    {
        unsigned char reassembled[sizeof(bytes)];
        size_t        off  = 0;
        size_t        step = 7; /* deliberately does not evenly divide sizeof(bytes) */

        while (off < sizeof(bytes)) {
            unsigned char chunk[7];
            size_t        chunk_size = sizeof(chunk);

            if (H5Pget_filter_blob(dcpl, 0, off, chunk, &chunk_size) < 0)
                TEST_ERROR;
            /* chunk_size is bytes REMAINING from off, not bytes copied;
             * the copy itself is capped at sizeof(chunk) by the callee. */
            {
                size_t copied = (chunk_size < step) ? chunk_size : step;
                if (off + copied > sizeof(bytes))
                    copied = sizeof(bytes) - off;
                memcpy(reassembled + off, chunk, copied);
                off += copied;
            }
        }
        if (memcmp(reassembled, bytes, sizeof(bytes)) != 0)
            TEST_ERROR;
    }

    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    dcpl = H5I_INVALID_HID;
    if (H5Zunregister(BLOB_DEFAULT_FILTER_ID) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(dcpl);
        H5Zunregister(BLOB_DEFAULT_FILTER_ID);
    }
    H5E_END_TRY
    return -1;
}

/* Deleting a blob-bearing dataset must reclaim the global-heap object
 * holding its blob, not just the dataset's own storage. Compare the free
 * space reclaimed by deleting a blob-bearing dataset against an
 * otherwise-identical plain dataset; the blob-bearing delete should
 * reclaim substantially more (the difference is the freed blob, not a
 * leak). */
static int
test_blob_delete_reclaims_heap(hid_t fapl)
{
    static const H5Z_class3_t blob_cls = {
        2,                      /* version         */
        BLOB_DEFAULT_FILTER_ID, /* id              */
        1,                      /* encoder_present */
        1,                      /* decoder_present */
        "blob_default_filter",  /* canonical_name  */
        NULL,                   /* description     */
        NULL,                   /* can_apply       */
        NULL,                   /* set_local       */
        blob_passthrough_func,  /* filter          */
        NULL,                   /* set_config      */
        NULL,                   /* get_config      */
        NULL,                   /* write_blob      */
        NULL,                   /* read_blob       */
        NULL,                   /* close_blob      */
    };
    char           filename[1024];
    unsigned char *blob = NULL;
    hid_t          file = H5I_INVALID_HID, sid = H5I_INVALID_HID;
    hid_t          dcpl_plain = H5I_INVALID_HID, dcpl_blob = H5I_INVALID_HID;
    hid_t          dset    = H5I_INVALID_HID;
    hsize_t        dims[2] = {8, 8}, chunk[2] = {4, 4};
    int            wdata[8][8];
    hssize_t       fs_before, fs_after;
    hssize_t       delta_plain, delta_blob;

    TESTING("H5Pappend_filter_blob: delete reclaims heap space");

    if (H5Zregister(&blob_cls) < 0)
        TEST_ERROR;
    if (NULL == (blob = (unsigned char *)malloc(BLOB_TEST_SIZE)))
        TEST_ERROR;
    blob_fill_pattern(blob, BLOB_TEST_SIZE);
    for (int i = 0; i < 8; i++)
        for (int j = 0; j < 8; j++)
            wdata[i][j] = i * 8 + j;

    h5_fixname(FILENAME[5], fapl, filename, sizeof(filename));
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;
    if ((sid = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Plain dataset: same shape/chunking, no filter, no blob */
    if ((dcpl_plain = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_chunk(dcpl_plain, 2, chunk) < 0)
        TEST_ERROR;
    if ((dset = H5Dcreate2(file, "plain", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl_plain, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;

    /* Blob-bearing dataset: same shape/chunking plus a blob-carrying filter */
    if ((dcpl_blob = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_chunk(dcpl_blob, 2, chunk) < 0)
        TEST_ERROR;
    if (H5Pappend_filter_blob(dcpl_blob, BLOB_DEFAULT_FILTER_ID, 0, blob, BLOB_TEST_SIZE) < 0)
        TEST_ERROR;
    if ((dset = H5Dcreate2(file, "blobby", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl_blob, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;

    /* Measure within one continuous open session: the default file space
     * strategy does not persist free-space manager state across close, so
     * a close/reopen between measurements would lose track of space freed
     * in the prior session instead of reflecting it. */
    if ((fs_before = H5Fget_freespace(file)) < 0)
        TEST_ERROR;
    if (H5Ldelete(file, "plain", H5P_DEFAULT) < 0)
        TEST_ERROR;
    if ((fs_after = H5Fget_freespace(file)) < 0)
        TEST_ERROR;
    delta_plain = fs_after - fs_before;

    if ((fs_before = H5Fget_freespace(file)) < 0)
        TEST_ERROR;
    if (H5Ldelete(file, "blobby", H5P_DEFAULT) < 0)
        TEST_ERROR;
    if ((fs_after = H5Fget_freespace(file)) < 0)
        TEST_ERROR;
    delta_blob = fs_after - fs_before;

    /* The extra space reclaimed by deleting the blob-bearing dataset,
     * beyond what deleting an equivalent plain dataset reclaims, must
     * account for most of the blob -- otherwise the heap object leaked. */
    if (delta_blob < delta_plain + (hssize_t)(BLOB_TEST_SIZE / 2))
        TEST_ERROR;

    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;
    if (H5Sclose(sid) < 0 || H5Pclose(dcpl_plain) < 0 || H5Pclose(dcpl_blob) < 0)
        TEST_ERROR;
    sid = dcpl_plain = dcpl_blob = H5I_INVALID_HID;
    if (H5Zunregister(BLOB_DEFAULT_FILTER_ID) < 0)
        TEST_ERROR;

    free(blob);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(dset);
        H5Pclose(dcpl_plain);
        H5Pclose(dcpl_blob);
        H5Sclose(sid);
        H5Fclose(file);
        H5Zunregister(BLOB_DEFAULT_FILTER_ID);
    }
    H5E_END_TRY
    free(blob);
    return -1;
}

/* Appending the same filter ID twice to one pipeline, each with a
 * different blob, must recover each entry's own blob independently after
 * create/reopen -- blob association is per pipeline-entry, not per filter
 * ID. */
static int
test_blob_duplicate_filter_ids(hid_t fapl)
{
    static const H5Z_class3_t blob_cls = {
        2,                      /* version         */
        BLOB_DEFAULT_FILTER_ID, /* id              */
        1,                      /* encoder_present */
        1,                      /* decoder_present */
        "blob_default_filter",  /* canonical_name  */
        NULL,                   /* description     */
        NULL,                   /* can_apply       */
        NULL,                   /* set_local       */
        blob_passthrough_func,  /* filter          */
        NULL,                   /* set_config      */
        NULL,                   /* get_config      */
        NULL,                   /* write_blob      */
        NULL,                   /* read_blob       */
        NULL,                   /* close_blob      */
    };
    char          filename[1024];
    unsigned char blob_a[256], blob_b[256];
    hid_t         file = H5I_INVALID_HID, sid = H5I_INVALID_HID;
    hid_t         dcpl = H5I_INVALID_HID, dset = H5I_INVALID_HID, dcpl_out = H5I_INVALID_HID;
    hsize_t       dims[2] = {8, 8}, chunk[2] = {4, 4};

    TESTING("H5Pappend_filter_blob: duplicate filter IDs, distinct blobs");

    if (H5Zregister(&blob_cls) < 0)
        TEST_ERROR;
    blob_fill_pattern(blob_a, sizeof(blob_a));
    blob_fill_pattern(blob_b, sizeof(blob_b));
    memset(blob_b, 0x5A, BLOB_MAGIC_LEN); /* distinguish from blob_a's leading magic */

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_chunk(dcpl, 2, chunk) < 0)
        TEST_ERROR;
    if (H5Pappend_filter_blob(dcpl, BLOB_DEFAULT_FILTER_ID, 0, blob_a, sizeof(blob_a)) < 0)
        TEST_ERROR;
    if (H5Pappend_filter_blob(dcpl, BLOB_DEFAULT_FILTER_ID, 0, blob_b, sizeof(blob_b)) < 0)
        TEST_ERROR;
    if (H5Pget_nfilters(dcpl) != 2)
        TEST_ERROR;

    /* Both blobs are already independently recoverable before any file I/O */
    if (blob_check_getter(dcpl, 0, blob_a, sizeof(blob_a)) < 0)
        TEST_ERROR;
    if (blob_check_getter(dcpl, 1, blob_b, sizeof(blob_b)) < 0)
        TEST_ERROR;

    h5_fixname(FILENAME[6], fapl, filename, sizeof(filename));
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;
    if ((sid = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;
    if ((dset = H5Dcreate2(file, "dset", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;
    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;

    if ((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        TEST_ERROR;
    if ((dset = H5Dopen2(file, "dset", H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if ((dcpl_out = H5Dget_create_plist(dset)) < 0)
        TEST_ERROR;
    if (H5Pget_nfilters(dcpl_out) != 2)
        TEST_ERROR;
    if (blob_check_getter(dcpl_out, 0, blob_a, sizeof(blob_a)) < 0)
        TEST_ERROR;
    if (blob_check_getter(dcpl_out, 1, blob_b, sizeof(blob_b)) < 0)
        TEST_ERROR;

    if (H5Pclose(dcpl_out) < 0)
        TEST_ERROR;
    dcpl_out = H5I_INVALID_HID;
    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;
    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;
    if (H5Sclose(sid) < 0 || H5Pclose(dcpl) < 0)
        TEST_ERROR;
    sid = dcpl = H5I_INVALID_HID;
    if (H5Zunregister(BLOB_DEFAULT_FILTER_ID) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(dcpl_out);
        H5Dclose(dset);
        H5Pclose(dcpl);
        H5Sclose(sid);
        H5Fclose(file);
        H5Zunregister(BLOB_DEFAULT_FILTER_ID);
    }
    H5E_END_TRY
    return -1;
}

/* Per-dataset blobs via the established H5Pcopy-then-tweak idiom: build a
 * base DCPL, H5Pcopy it, append the same filter with a different blob in
 * each copy, create two datasets, and verify each reads back its own
 * blob. */
static int
test_blob_per_dataset_copy_then_tweak(hid_t fapl)
{
    static const H5Z_class3_t blob_cls = {
        2,                      /* version         */
        BLOB_DEFAULT_FILTER_ID, /* id              */
        1,                      /* encoder_present */
        1,                      /* decoder_present */
        "blob_default_filter",  /* canonical_name  */
        NULL,                   /* description     */
        NULL,                   /* can_apply       */
        NULL,                   /* set_local       */
        blob_passthrough_func,  /* filter          */
        NULL,                   /* set_config      */
        NULL,                   /* get_config      */
        NULL,                   /* write_blob      */
        NULL,                   /* read_blob       */
        NULL,                   /* close_blob      */
    };
    char          filename[1024];
    unsigned char blob_1[128], blob_2[128];
    hid_t         file = H5I_INVALID_HID, sid = H5I_INVALID_HID;
    hid_t         dcpl_base = H5I_INVALID_HID, dcpl_1 = H5I_INVALID_HID, dcpl_2 = H5I_INVALID_HID;
    hid_t         dset1 = H5I_INVALID_HID, dset2 = H5I_INVALID_HID, dcpl_out = H5I_INVALID_HID;
    hsize_t       dims[2] = {8, 8}, chunk[2] = {4, 4};

    TESTING("H5Pcopy-then-tweak: per-dataset blobs from one base DCPL");

    if (H5Zregister(&blob_cls) < 0)
        TEST_ERROR;
    blob_fill_pattern(blob_1, sizeof(blob_1));
    blob_fill_pattern(blob_2, sizeof(blob_2));
    memset(blob_2, 0xA5, BLOB_MAGIC_LEN);

    /* Base DCPL carries no blob yet */
    if ((dcpl_base = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_chunk(dcpl_base, 2, chunk) < 0)
        TEST_ERROR;

    if ((dcpl_1 = H5Pcopy(dcpl_base)) < 0)
        TEST_ERROR;
    if (H5Pappend_filter_blob(dcpl_1, BLOB_DEFAULT_FILTER_ID, 0, blob_1, sizeof(blob_1)) < 0)
        TEST_ERROR;

    if ((dcpl_2 = H5Pcopy(dcpl_base)) < 0)
        TEST_ERROR;
    if (H5Pappend_filter_blob(dcpl_2, BLOB_DEFAULT_FILTER_ID, 0, blob_2, sizeof(blob_2)) < 0)
        TEST_ERROR;

    /* Tweaking dcpl_2 must not have disturbed dcpl_1's independently-owned
     * blob (this is exactly the copy-on-write sharing path: dcpl_1 and
     * dcpl_2 started from the same base but diverge here). */
    if (blob_check_getter(dcpl_1, 0, blob_1, sizeof(blob_1)) < 0)
        TEST_ERROR;
    if (blob_check_getter(dcpl_2, 0, blob_2, sizeof(blob_2)) < 0)
        TEST_ERROR;

    h5_fixname(FILENAME[7], fapl, filename, sizeof(filename));
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;
    if ((sid = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;
    if ((dset1 = H5Dcreate2(file, "dset1", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl_1, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if ((dset2 = H5Dcreate2(file, "dset2", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl_2, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dclose(dset1) < 0 || H5Dclose(dset2) < 0)
        TEST_ERROR;
    dset1 = dset2 = H5I_INVALID_HID;
    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;

    if ((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        TEST_ERROR;
    if ((dset1 = H5Dopen2(file, "dset1", H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if ((dcpl_out = H5Dget_create_plist(dset1)) < 0)
        TEST_ERROR;
    if (blob_check_getter(dcpl_out, 0, blob_1, sizeof(blob_1)) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl_out) < 0)
        TEST_ERROR;
    dcpl_out = H5I_INVALID_HID;
    if (H5Dclose(dset1) < 0)
        TEST_ERROR;
    dset1 = H5I_INVALID_HID;

    if ((dset2 = H5Dopen2(file, "dset2", H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if ((dcpl_out = H5Dget_create_plist(dset2)) < 0)
        TEST_ERROR;
    if (blob_check_getter(dcpl_out, 0, blob_2, sizeof(blob_2)) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl_out) < 0)
        TEST_ERROR;
    dcpl_out = H5I_INVALID_HID;
    if (H5Dclose(dset2) < 0)
        TEST_ERROR;
    dset2 = H5I_INVALID_HID;

    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;
    if (H5Sclose(sid) < 0 || H5Pclose(dcpl_base) < 0 || H5Pclose(dcpl_1) < 0 || H5Pclose(dcpl_2) < 0)
        TEST_ERROR;
    sid = dcpl_base = dcpl_1 = dcpl_2 = H5I_INVALID_HID;
    if (H5Zunregister(BLOB_DEFAULT_FILTER_ID) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(dcpl_out);
        H5Dclose(dset1);
        H5Dclose(dset2);
        H5Pclose(dcpl_base);
        H5Pclose(dcpl_1);
        H5Pclose(dcpl_2);
        H5Sclose(sid);
        H5Fclose(file);
        H5Zunregister(BLOB_DEFAULT_FILTER_ID);
    }
    H5E_END_TRY
    return -1;
}

/* Use Case B pattern: a filter stores a companion mask dataset's path in
 * its blob. On reopen, the filter's read_blob callback opens the mask via
 * H5Dopen2(file_id, path, ...) using the file_id it's handed and reads
 * correct contents -- exercising blob-as-cross-reference, not just
 * blob-as-opaque-bytes. */
#define USECASEB_MASK_PATH "/mask"
static int
usecaseb_read_blob(hid_t file_id, H5Z_blob_loc_t loc, void **buf_out, size_t *size_out)
{
    hid_t   mask_dset = H5I_INVALID_HID, mask_sid = H5I_INVALID_HID;
    hsize_t mask_dims[1] = {4};
    int     mask_data[4];
    int    *result = NULL;

    if (H5Iget_type(file_id) != H5I_FILE)
        return FAIL;
    if (loc.addr != (haddr_t)0xB100B || loc.idx != 7)
        return FAIL;

    if ((mask_dset = H5Dopen2(file_id, USECASEB_MASK_PATH, H5P_DEFAULT)) < 0)
        return FAIL;
    if ((mask_sid = H5Dget_space(mask_dset)) < 0) {
        H5Dclose(mask_dset);
        return FAIL;
    }
    if (H5Sget_simple_extent_dims(mask_sid, mask_dims, NULL) < 0) {
        H5Sclose(mask_sid);
        H5Dclose(mask_dset);
        return FAIL;
    }
    H5Sclose(mask_sid);
    if (H5Dread(mask_dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, mask_data) < 0) {
        H5Dclose(mask_dset);
        return FAIL;
    }
    if (H5Dclose(mask_dset) < 0)
        return FAIL;

    if (NULL == (result = (int *)malloc(sizeof(mask_data))))
        return FAIL;
    memcpy(result, mask_data, sizeof(mask_data));
    *buf_out  = result;
    *size_out = sizeof(mask_data);
    return SUCCEED;
}

static herr_t
usecaseb_write_blob(hid_t file_id, const void *buf, size_t size, H5Z_blob_loc_t *loc_out)
{
    (void)buf;
    (void)size;
    if (H5Iget_type(file_id) != H5I_FILE)
        return FAIL;
    /* The blob is the path string itself; this filter's on-disk locator
     * is a fixed token since it never varies -- the path is recovered
     * from the blob bytes the library already stores, not from loc. */
    loc_out->addr = (haddr_t)0xB100B;
    loc_out->idx  = 7;
    return SUCCEED;
}

static herr_t
usecaseb_close_blob(void *buf, size_t H5_ATTR_UNUSED size)
{
    free(buf);
    return SUCCEED;
}

static int
test_blob_usecaseb_path_association(hid_t fapl)
{
    static const H5Z_class3_t blob_cls = {
        2,                      /* version         */
        BLOB_CUSTOM_FILTER_ID,  /* id              */
        1,                      /* encoder_present */
        1,                      /* decoder_present */
        "blob_usecaseb_filter", /* canonical_name  */
        NULL,                   /* description     */
        NULL,                   /* can_apply       */
        NULL,                   /* set_local       */
        blob_passthrough_func,  /* filter          */
        NULL,                   /* set_config      */
        NULL,                   /* get_config      */
        usecaseb_write_blob,    /* write_blob      */
        usecaseb_read_blob,     /* read_blob       */
        usecaseb_close_blob,    /* close_blob      */
    };
    char    filename[1024];
    hid_t   file = H5I_INVALID_HID, sid = H5I_INVALID_HID, mask_sid = H5I_INVALID_HID;
    hid_t   dcpl = H5I_INVALID_HID, dset = H5I_INVALID_HID, mask_dset = H5I_INVALID_HID;
    hsize_t dims[2] = {4, 4}, chunk[2] = {2, 2}, mask_dims[1] = {4};
    int     mask_data[4] = {10, 20, 30, 40};

    TESTING("H5Pappend_filter_blob: path-string dataset association (Use Case B)");

    if (H5Zregister(&blob_cls) < 0)
        TEST_ERROR;

    h5_fixname(FILENAME[8], fapl, filename, sizeof(filename));
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    /* The mask dataset the filter's blob will reference by path */
    if ((mask_sid = H5Screate_simple(1, mask_dims, NULL)) < 0)
        TEST_ERROR;
    if ((mask_dset = H5Dcreate2(file, USECASEB_MASK_PATH + 1, H5T_NATIVE_INT, mask_sid, H5P_DEFAULT,
                                H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dwrite(mask_dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, mask_data) < 0)
        TEST_ERROR;
    if (H5Dclose(mask_dset) < 0)
        TEST_ERROR;
    mask_dset = H5I_INVALID_HID;

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_chunk(dcpl, 2, chunk) < 0)
        TEST_ERROR;
    if (H5Pappend_filter_blob(dcpl, BLOB_CUSTOM_FILTER_ID, 0, (const void *)USECASEB_MASK_PATH,
                              strlen(USECASEB_MASK_PATH) + 1) < 0)
        TEST_ERROR;

    if ((sid = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;
    if ((dset = H5Dcreate2(file, "filtered", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;
    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;

    /* Reopen: the filter's read_blob callback must open the mask dataset
     * by the path recovered from its blob and read back correct data. */
    if ((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        TEST_ERROR;
    if ((dset = H5Dopen2(file, "filtered", H5P_DEFAULT)) < 0)
        TEST_ERROR;
    {
        hid_t dcpl_out = H5Dget_create_plist(dset);
        int   ret      = 0;

        if (dcpl_out < 0)
            TEST_ERROR;
        ret = blob_check_getter(dcpl_out, 0, (const unsigned char *)mask_data, sizeof(mask_data));
        H5Pclose(dcpl_out);
        if (ret < 0)
            TEST_ERROR;
    }
    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;
    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;
    if (H5Sclose(sid) < 0 || H5Sclose(mask_sid) < 0 || H5Pclose(dcpl) < 0)
        TEST_ERROR;
    sid = mask_sid = dcpl = H5I_INVALID_HID;
    if (H5Zunregister(BLOB_CUSTOM_FILTER_ID) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(mask_dset);
        H5Dclose(dset);
        H5Pclose(dcpl);
        H5Sclose(mask_sid);
        H5Sclose(sid);
        H5Fclose(file);
        H5Zunregister(BLOB_CUSTOM_FILTER_ID);
    }
    H5E_END_TRY
    return -1;
}

/* Oversized default-storage blob: a multi-megabyte buffer stored via the
 * default H5HG-based path must not affect create/open or the runtime
 * chunk I/O path -- the blob is configuration data loaded once at open
 * time, not something the per-chunk filter pipeline touches on every
 * I/O call. */
#define OVERSIZED_BLOB_SIZE (4 * 1024 * 1024) /* 4 MiB */
static int
test_blob_oversized_default_storage(hid_t fapl)
{
    static const H5Z_class3_t blob_cls = {
        2,                      /* version         */
        BLOB_DEFAULT_FILTER_ID, /* id              */
        1,                      /* encoder_present */
        1,                      /* decoder_present */
        "blob_default_filter",  /* canonical_name  */
        NULL,                   /* description     */
        NULL,                   /* can_apply       */
        NULL,                   /* set_local       */
        blob_passthrough_func,  /* filter          */
        NULL,                   /* set_config      */
        NULL,                   /* get_config      */
        NULL,                   /* write_blob      */
        NULL,                   /* read_blob       */
        NULL,                   /* close_blob      */
    };
    char           filename[1024];
    unsigned char *blob = NULL;
    hid_t          file = H5I_INVALID_HID, sid = H5I_INVALID_HID;
    hid_t          dcpl = H5I_INVALID_HID, dset = H5I_INVALID_HID;
    hsize_t        dims[2] = {64, 64}, chunk[2] = {8, 8};
    int           *wdata = NULL, *rdata = NULL;

    TESTING("H5Pappend_filter_blob: oversized (4 MiB) default-storage blob");

    if (H5Zregister(&blob_cls) < 0)
        TEST_ERROR;
    if (NULL == (blob = (unsigned char *)malloc(OVERSIZED_BLOB_SIZE)))
        TEST_ERROR;
    blob_fill_pattern(blob, OVERSIZED_BLOB_SIZE);
    if (NULL == (wdata = (int *)malloc(64 * 64 * sizeof(int))))
        TEST_ERROR;
    if (NULL == (rdata = (int *)malloc(64 * 64 * sizeof(int))))
        TEST_ERROR;
    for (int i = 0; i < 64 * 64; i++)
        wdata[i] = i;

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_chunk(dcpl, 2, chunk) < 0)
        TEST_ERROR;
    if (H5Pappend_filter_blob(dcpl, BLOB_DEFAULT_FILTER_ID, 0, blob, OVERSIZED_BLOB_SIZE) < 0)
        TEST_ERROR;

    h5_fixname(FILENAME[9], fapl, filename, sizeof(filename));
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;
    if ((sid = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;
    if ((dset = H5Dcreate2(file, "dset", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;
    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;

    if ((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        TEST_ERROR;
    if ((dset = H5Dopen2(file, "dset", H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Dread(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata) < 0)
        TEST_ERROR;
    if (memcmp(wdata, rdata, 64 * 64 * sizeof(int)) != 0)
        TEST_ERROR;
    {
        hid_t dcpl_out = H5Dget_create_plist(dset);
        int   ret      = 0;

        if (dcpl_out < 0)
            TEST_ERROR;
        ret = blob_check_getter(dcpl_out, 0, blob, OVERSIZED_BLOB_SIZE);
        H5Pclose(dcpl_out);
        if (ret < 0)
            TEST_ERROR;
    }
    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;
    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;
    if (H5Sclose(sid) < 0 || H5Pclose(dcpl) < 0)
        TEST_ERROR;
    sid = dcpl = H5I_INVALID_HID;
    if (H5Zunregister(BLOB_DEFAULT_FILTER_ID) < 0)
        TEST_ERROR;

    free(blob);
    free(wdata);
    free(rdata);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(dset);
        H5Pclose(dcpl);
        H5Sclose(sid);
        H5Fclose(file);
        H5Zunregister(BLOB_DEFAULT_FILTER_ID);
    }
    H5E_END_TRY
    free(blob);
    free(wdata);
    free(rdata);
    return -1;
}

/* -----------------------------------------------------------------------
 * Reference example: migrating a real-world filter's oversized-config
 * pattern to H5Pappend_filter_blob.
 *
 * Modeled directly on LibPressio's actual production HDF5 filter --
 * https://github.com/robertu94/libpressio/blob/master/tools/hdf5_filter/
 * src/libpressio_hdf5_filter.cc, functions H5Z_libpressio_set_local(),
 * get_cd_values_from_options(), and get_options_from_cd_values() -- which
 * hand-packs a compressor's serialized options (msgpack, via nlohmann
 * json) directly into cd_values with no size bound. LibPressio's author
 * reported this "cause[s] segfaults when attempting to store in CD
 * values over a few KB" (HDFGroup/hdf5#6153, comment from @robertu94),
 * citing two concrete cases: ROIBIN-SZ's binary spatial mask, and SZ4's
 * (still unreleased) JIT compiler needing to store pre-processed source.
 * Both are exactly the multi-megabyte-blob problem this RFC's mechanism
 * targets.
 *
 * This models the migrated pattern: set_local still packs only the
 * small, fixed-size stuff (datatype class, ndims, dims) into cd_values
 * exactly as the real filter does -- that part was never the problem.
 * The arbitrarily large "compressor options" travel via
 * H5Pappend_filter_blob instead of being hand-serialized into cd_values,
 * and set_local recovers them with H5Pget_filter_blob. Unlike this
 * file's other blob tests (which pre-build the DCPL's blob and never
 * touch cd_values), this one exercises the two mechanisms *together* --
 * set_local computing cd_values dynamically per chunk shape while a
 * blob is also attached -- which is the actual shape a filter migration
 * would take. */
#define LIBPRESSIO_PATTERN_FILTER_ID 550

static size_t
libpressio_pattern_filter_func(unsigned int H5_ATTR_UNUSED flags, size_t H5_ATTR_UNUSED cd_nelmts,
                               const unsigned int H5_ATTR_UNUSED *cd_values, hid_t H5_ATTR_UNUSED dxpl_id,
                               const hsize_t H5_ATTR_UNUSED *scaled, size_t H5_ATTR_UNUSED ndims,
                               size_t nbytes, size_t H5_ATTR_UNUSED *buf_size, void H5_ATTR_UNUSED **buf)
{
    /* Pass-through: this example is about the configuration path (the
     * part LibPressio's author flagged as broken), not about actually
     * invoking a compressor. */
    return nbytes;
}

/* cd_values-packing filters like the real LibPressio one know their own
 * filter ID at compile time but not their position in a (possibly
 * multi-filter) pipeline -- H5Pget_filter_blob() is index-based, so
 * set_local has to find itself first, same as any real filter author
 * integrating this API would. */
static herr_t
libpressio_pattern_find_self(hid_t dcpl_id, unsigned *idx_out)
{
    int nfilters = H5Pget_nfilters(dcpl_id);

    if (nfilters < 0)
        return FAIL;
    for (unsigned i = 0; i < (unsigned)nfilters; i++) {
        unsigned     flags;
        size_t       cd_nelmts = 0;
        H5Z_filter_t id        = H5Pget_filter2(dcpl_id, i, &flags, &cd_nelmts, NULL, 0, NULL, NULL);

        if (id == LIBPRESSIO_PATTERN_FILTER_ID) {
            *idx_out = i;
            return SUCCEED;
        }
    }
    return FAIL;
}

/* Modeled on H5Z_libpressio_set_local(): computes dtype class and chunk
 * dims from the chunk dataspace exactly as the real filter does. Where
 * the real filter would msgpack-serialize the full compressor options
 * and hand-pack them into cd_values here, this instead just confirms
 * the options blob attached via H5Pappend_filter_blob is present and
 * readable -- a real filter would parse it at this point (msgpack-decode
 * it, or for a hypothetical SZ4-shaped filter, JIT-compile the embedded
 * source once per dataset here rather than per chunk). */
static herr_t
libpressio_pattern_set_local(hid_t dcpl_id, hid_t type_id, hid_t chunk_space_id)
{
    unsigned    idx;
    unsigned    flags;
    size_t      cd_nelmts_cur = 0;
    int         chunk_ndims;
    hsize_t     dims[32];
    unsigned    cd_values[34]; /* dtype class + ndims + up to 32 dims */
    size_t      n         = 0;
    size_t      blob_size = 0;
    H5T_class_t dclass;

    if (libpressio_pattern_find_self(dcpl_id, &idx) < 0)
        return FAIL;

    /* The compressor-options blob attached at H5Pappend_filter_blob
     * time -- the multi-megabyte piece the real filter would have tried
     * to jam into cd_values. */
    if (H5Pget_filter_blob(dcpl_id, idx, 0, NULL, &blob_size) < 0)
        return FAIL;
    if (blob_size == 0)
        return FAIL; /* this filter requires options */

    if ((chunk_ndims = H5Sget_simple_extent_ndims(chunk_space_id)) < 0)
        return FAIL;
    if ((size_t)chunk_ndims > sizeof(dims) / sizeof(dims[0]))
        return FAIL;
    if (H5Sget_simple_extent_dims(chunk_space_id, dims, NULL) < 0)
        return FAIL;

    if ((dclass = H5Tget_class(type_id)) == H5T_NO_CLASS)
        return FAIL;

    cd_values[n++] = (unsigned)dclass;
    cd_values[n++] = (unsigned)chunk_ndims;
    for (int i = 0; i < chunk_ndims; i++)
        cd_values[n++] = (unsigned)dims[i];

    if (H5Pget_filter_by_id2(dcpl_id, LIBPRESSIO_PATTERN_FILTER_ID, &flags, &cd_nelmts_cur, NULL, 0, NULL,
                             NULL) < 0)
        return FAIL;
    if (H5Pmodify_filter(dcpl_id, LIBPRESSIO_PATTERN_FILTER_ID, flags, n, cd_values) < 0)
        return FAIL;

    return SUCCEED;
}

/* Stands in for a msgpack-serialized pressio_options bag (or, for the
 * SZ4-shaped variant, pre-processed JIT source). Sized well past any
 * cd_values-array-based scheme's practical ceiling to make the point the
 * real filter's approach can't handle. */
#define LIBPRESSIO_OPTIONS_BLOB_SIZE (256 * 1024)

static int
test_blob_libpressio_migration_pattern(hid_t fapl)
{
    static const H5Z_class3_t libpressio_pattern_cls = {
        2,                              /* version         */
        LIBPRESSIO_PATTERN_FILTER_ID,   /* id              */
        1,                              /* encoder_present */
        1,                              /* decoder_present */
        "libpressio_pattern_filter",    /* canonical_name  */
        NULL,                           /* description     */
        NULL,                           /* can_apply       */
        libpressio_pattern_set_local,   /* set_local       */
        libpressio_pattern_filter_func, /* filter        */
        NULL,                           /* set_config      */
        NULL,                           /* get_config      */
        NULL,                           /* write_blob: default global-heap storage */
        NULL,                           /* read_blob       */
        NULL,                           /* close_blob      */
    };
    char           filename[1024];
    unsigned char *options_blob = NULL;
    hid_t          file = H5I_INVALID_HID, sid = H5I_INVALID_HID;
    hid_t          dcpl = H5I_INVALID_HID, dset = H5I_INVALID_HID, dcpl_out = H5I_INVALID_HID;
    hsize_t        dims[2] = {8, 8}, chunk[2] = {4, 4};
    int            wdata[8][8], rdata[8][8];
    unsigned       flags;
    size_t         cd_nelmts = 0;
    unsigned       cd_values[34];

    TESTING("H5Pappend_filter_blob: LibPressio set_local + oversized-options migration pattern");

    if (H5Zregister(&libpressio_pattern_cls) < 0)
        TEST_ERROR;
    if (NULL == (options_blob = (unsigned char *)malloc(LIBPRESSIO_OPTIONS_BLOB_SIZE)))
        TEST_ERROR;
    blob_fill_pattern(options_blob, LIBPRESSIO_OPTIONS_BLOB_SIZE);
    for (int r = 0; r < 8; r++)
        for (int c = 0; c < 8; c++)
            wdata[r][c] = r * 8 + c;

    /* User workflow: attach the (oversized) options as a blob, then
     * create the dataset. H5Dcreate2 triggers set_local, which pulls the
     * blob back out and derives the small cd_values summary -- the real
     * filter's H5Z_libpressio_set_local() does the dims/dtype half of
     * this already; only the options-into-cd_values half needed to
     * change. */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_chunk(dcpl, 2, chunk) < 0)
        TEST_ERROR;
    if (H5Pappend_filter_blob(dcpl, LIBPRESSIO_PATTERN_FILTER_ID, 0, options_blob,
                              LIBPRESSIO_OPTIONS_BLOB_SIZE) < 0)
        TEST_ERROR;

    h5_fixname(FILENAME[1], fapl, filename, sizeof(filename));
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;
    if ((sid = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;
    if ((dset = H5Dcreate2(file, "dset", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* set_local ran against the dataset's own private DCPL copy, not the
     * "dcpl" template handle above (H5Dcreate2 never mutates the
     * caller's original) -- H5Dget_create_plist(dset) is the only way to
     * see what set_local actually wrote. cd_values now holds
     * dclass/ndims/dims, not the options blob -- confirming the split
     * actually took effect. */
    if ((dcpl_out = H5Dget_create_plist(dset)) < 0)
        TEST_ERROR;
    cd_nelmts = NELMTS(cd_values);
    if (H5Pget_filter_by_id2(dcpl_out, LIBPRESSIO_PATTERN_FILTER_ID, &flags, &cd_nelmts, cd_values, 0, NULL,
                             NULL) < 0)
        TEST_ERROR;
    if (cd_nelmts != 4) /* dclass + ndims(2) + dims[4,4] */
        TEST_ERROR;
    if (cd_values[0] != (unsigned)H5T_INTEGER)
        TEST_ERROR;
    if (cd_values[1] != 2 || cd_values[2] != chunk[0] || cd_values[3] != chunk[1])
        TEST_ERROR;
    if (H5Pclose(dcpl_out) < 0)
        TEST_ERROR;
    dcpl_out = H5I_INVALID_HID;

    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;
    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;

    /* Reopen: data reads back, the small cd_values summary survived, and
     * the full oversized options blob -- the part that would have
     * segfaulted the real filter -- round-trips byte-for-byte. */
    if ((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        TEST_ERROR;
    if ((dset = H5Dopen2(file, "dset", H5P_DEFAULT)) < 0)
        TEST_ERROR;
    memset(rdata, 0, sizeof(rdata));
    if (H5Dread(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata) < 0)
        TEST_ERROR;
    if (memcmp(wdata, rdata, sizeof(wdata)) != 0)
        TEST_ERROR;

    if ((dcpl_out = H5Dget_create_plist(dset)) < 0)
        TEST_ERROR;
    if (blob_check_getter(dcpl_out, 0, options_blob, LIBPRESSIO_OPTIONS_BLOB_SIZE) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl_out) < 0)
        TEST_ERROR;
    dcpl_out = H5I_INVALID_HID;

    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    dset = H5I_INVALID_HID;
    if (H5Fclose(file) < 0)
        TEST_ERROR;
    file = H5I_INVALID_HID;
    if (H5Sclose(sid) < 0 || H5Pclose(dcpl) < 0)
        TEST_ERROR;
    sid = dcpl = H5I_INVALID_HID;
    if (H5Zunregister(LIBPRESSIO_PATTERN_FILTER_ID) < 0)
        TEST_ERROR;

    free(options_blob);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(dcpl_out);
        H5Dclose(dset);
        H5Pclose(dcpl);
        H5Sclose(sid);
        H5Fclose(file);
        H5Zunregister(LIBPRESSIO_PATTERN_FILTER_ID);
    }
    H5E_END_TRY
    free(options_blob);
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

    /* In-file blob configuration storage (H5Pappend_filter_blob) */
    nerrors += test_blob_default_storage(fapl) < 0 ? 1 : 0;
    nerrors += test_blob_custom_callbacks(fapl) < 0 ? 1 : 0;
    nerrors += test_blob_errors() < 0 ? 1 : 0;
    nerrors += test_blob_getter() < 0 ? 1 : 0;
    nerrors += test_blob_delete_reclaims_heap(fapl) < 0 ? 1 : 0;
    nerrors += test_blob_duplicate_filter_ids(fapl) < 0 ? 1 : 0;
    nerrors += test_blob_per_dataset_copy_then_tweak(fapl) < 0 ? 1 : 0;
    nerrors += test_blob_usecaseb_path_association(fapl) < 0 ? 1 : 0;
    nerrors += test_blob_oversized_default_storage(fapl) < 0 ? 1 : 0;
    nerrors += test_blob_libpressio_migration_pattern(fapl) < 0 ? 1 : 0;

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
