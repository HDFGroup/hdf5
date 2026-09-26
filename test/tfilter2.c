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
 *   - Typed TOML accessor functions (H5Zconfig_get_int, _get_str, etc.)
 *   - H5Z_class3_t registration, canonical names and H5Zget_filter_class_info
 *   - The H5Z_func2_t extended filter callback
 *   - Regression: existing H5Pset_filter still works
 */

#include "h5test.h"

static const char *FILENAME[] = {"tfilter2", NULL};

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

    /* RFC-HDFG-2026-001 test parse-09: key lookup is case-sensitive, per TOML
     * v1.0.0 bare-key semantics -- LEVEL and level are distinct keys.
     * Folding key case would be the only case-insensitive comparison in an
     * otherwise case-sensitive grammar, silently masking caller typos. */
    TESTING("H5Zconfig_get_int: key lookup is case-sensitive (LEVEL != level)");
    ret = H5Zconfig_get_int("LEVEL = 6", "level", &ival);
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

    /* The two cases above use TOML's inf/nan keywords.  These two arrive as
     * ordinary decimal literals that strtod() cannot represent, and are the
     * regressions found reviewing https://github.com/cktan/tomlc17/pull/50:
     * a literal overflowing to infinity was accepted wherever isfinite() had
     * been folded away by a fast-math build, and one underflowing to -0.0 was
     * accepted everywhere, because the sign bit made the bit pattern nonzero.
     * Neither needs a special build to assert on -- both must be rejected. */
    TESTING("H5Zconfig_get_double: literal overflowing to inf rejected");
    H5E_BEGIN_TRY
    {
        ret = H5Zconfig_get_double("tol = 1e400", "tol", &dval);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_double: literal underflowing to -0.0 rejected");
    H5E_BEGIN_TRY
    {
        ret = H5Zconfig_get_double("tol = -1e-400", "tol", &dval);
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

/* H5Zconfig_get_str: buf != NULL but buf_size == NULL is rejected */
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

/* -----------------------------------------------------------------------
 * Round-trip tests: write and read a chunked dataset through the built-in
 * filters, which are registered as H5Z_class3_t and run through filter2
 * ---------------------------------------------------------------------- */

/* Shared helper: create dataset with H5Pset_filter, write wbuf, read back
 * into rbuf, verify every element matches.  Returns SUCCEED or FAIL. */
static herr_t
h5_run_filter_roundtrip(hid_t file, const char *dset_name, hsize_t *dims, hsize_t *chunks, int ndims,
                        H5Z_filter_t filter_id, size_t cd_nelmts, const unsigned cd_values[], int *wbuf,
                        int *rbuf, size_t total_elements)
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
    if (H5Pset_filter(dcpl, filter_id, 0, cd_nelmts, cd_values) < 0)
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
    hsize_t  dims[2]    = {32, 32};
    hsize_t  chunks[2]  = {8, 8};
    unsigned cd_vals[1] = {6}; /* deflate level 6 */
    int      wbuf[32 * 32], rbuf[32 * 32];
    int      i;

    TESTING("Round-trip: deflate level 6 write/read");
    if (H5Zfilter_avail(H5Z_FILTER_DEFLATE) <= 0) {
        SKIPPED();
        puts("    deflate filter not available");
        return 0;
    }
    for (i = 0; i < 32 * 32; i++)
        wbuf[i] = i;
    if (h5_run_filter_roundtrip(file, "deflate_rt", dims, chunks, 2, H5Z_FILTER_DEFLATE, 1, cd_vals, wbuf,
                                rbuf, 32 * 32) < 0)
        TEST_ERROR;
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
    if (h5_run_filter_roundtrip(file, "shuffle_rt", dims, chunks, 1, H5Z_FILTER_SHUFFLE, 0, NULL, wbuf, rbuf,
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
    if (h5_run_filter_roundtrip(file, "fletcher32_rt", dims, chunks, 1, H5Z_FILTER_FLETCHER32, 0, NULL, wbuf,
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

/* -----------------------------------------------------------------------
 * canonical_name display tests
 *
 * Registers a minimal class3 filter and verifies that H5Pget_filter_by_id2
 * returns the registered canonical name as the filter name.
 * ---------------------------------------------------------------------- */

#define TITLE_FILTER_ID 512

static size_t
title_filter_func(unsigned int flags, size_t cd_nelmts, const unsigned int *cd_values,
                  hid_t H5_ATTR_UNUSED dxpl_id, const hsize_t H5_ATTR_UNUSED *scaled,
                  size_t H5_ATTR_UNUSED ndims, void H5_ATTR_UNUSED *state, size_t nbytes, size_t *buf_size,
                  void **buf)
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
        NULL,                /* can_apply      */
        NULL,                /* set_local      */
        title_filter_func,   /* filter         */
        NULL,                /* set_config     */
        NULL,                /* get_config     */
        NULL,                /* description    */
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
    if (H5Pset_filter(dcpl, TITLE_FILTER_ID, 0, 0, NULL) < 0)
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
                 size_t H5_ATTR_UNUSED ndims, void H5_ATTR_UNUSED *state, size_t nbytes, size_t *buf_size,
                 void **buf)
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
            NULL,             /* can_apply      */
            NULL,             /* set_local      */
            name_filter_func, /* filter         */
            NULL,             /* set_config     */
            NULL,             /* get_config     */
            NULL,             /* description    */
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
            NULL,               /* can_apply      */
            NULL,               /* set_local      */
            name_filter_func,   /* filter         */
            NULL,               /* set_config     */
            NULL,               /* get_config     */
            NULL,               /* description    */
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
    /* Build a dcpl with the unregistered filter via H5Pset_filter */
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
                      size_t H5_ATTR_UNUSED ndims, void H5_ATTR_UNUSED *state, size_t nbytes,
                      size_t *buf_size, void **buf)
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
        NULL,                  /* can_apply       */
        NULL,                  /* set_local       */
        longtitle_filter_func, /* filter          */
        NULL,                  /* set_config      */
        NULL,                  /* get_config      */
        NULL,                  /* description     */
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

    /* The canonical name is the filter's stable identifier and flows out
     * through H5Pget_filter2() and the command-line tools.  H5Zregister
     * must therefore hold it to [A-Za-z0-9_.-], non-empty, rather than
     * accepting arbitrary bytes. */
    TESTING("H5Zregister: canonical_name syntax is enforced");
    {
        /* Each must be rejected, and for the stated reason. */
        static const char *const bad[] = {
            "",                 /* empty                                  */
            "has space",        /* whitespace                             */
            "semi;colon",       /* the reserved pipeline separator        */
            "quote\"mark",      /* would need escaping in tool output     */
            "brace{}",          /* TOML inline-table delimiters           */
            "comma,sep",        /* the parameter-string separator         */
            "new\nline",        /* would corrupt line-oriented tool output*/
            "tab\there",        /* likewise                               */
            "nonascii\xc3\xa9", /* UTF-8 e-acute: outside the declared class */
            "slash/path",       /* path-like, unsafe as an identifier     */
            "equals=sign",      /* the key/value separator                */
        };
        /* Each must be accepted: the full declared character class.
         * "deflate" deliberately avoided -- it collides with the built-in
         * deflate filter's own canonical name, rejected by the separate
         * uniqueness check in test_canonical_name_uniqueness() below, a
         * different concern from the syntax check this loop covers. */
        static const char *const good[] = {
            "zfp", "not-deflate", "blosc2.lz4", "my_filter-2", "A", "0", "aA0_.-",
        };
        size_t i;

        for (i = 0; i < sizeof(bad) / sizeof(bad[0]); i++) {
            H5Z_class3_t c = {2,    LONGTITLE_FILTER_ID,   1,    1,    bad[i], NULL,
                              NULL, longtitle_filter_func, NULL, NULL, NULL};
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
            H5Z_class3_t c = {2,    LONGTITLE_FILTER_ID,   1,    1,    good[i], NULL,
                              NULL, longtitle_filter_func, NULL, NULL, NULL};
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

/* -----------------------------------------------------------------------
 * A canonical name must be unique among v3-registered filters: it
 * identifies the filter, so two different filters sharing one name would
 * make resolving a name to a filter ID ambiguous.
 * ---------------------------------------------------------------------- */
#define UNIQUENAME_FILTER_ID_A 536
#define UNIQUENAME_FILTER_ID_B 537

static int
test_canonical_name_uniqueness(void)
{
    H5Z_class3_t cls_a = {
        2,   UNIQUENAME_FILTER_ID_A, 1, 1, "test-unique-name", NULL, NULL, longtitle_filter_func, NULL, NULL,
        NULL};
    H5Z_class3_t cls_b = {
        2,   UNIQUENAME_FILTER_ID_B, 1, 1, "test-unique-name", NULL, NULL, longtitle_filter_func, NULL, NULL,
        NULL};
    herr_t ret;

    TESTING("H5Zregister: canonical_name collision across different filter ids is rejected");

    if (H5Zregister(&cls_a) < 0)
        TEST_ERROR;

    /* A different id claiming the same name must fail. */
    H5E_BEGIN_TRY
    {
        ret = H5Zregister(&cls_b);
    }
    H5E_END_TRY
    if (ret >= 0) {
        H5Zunregister(UNIQUENAME_FILTER_ID_A);
        H5Zunregister(UNIQUENAME_FILTER_ID_B);
        TEST_ERROR;
    }

    /* Re-registering the SAME id under its own unchanged name is not a
     * collision -- H5Z__insert_entry replaces the entry in place. */
    if (H5Zregister(&cls_a) < 0) {
        H5Zunregister(UNIQUENAME_FILTER_ID_A);
        TEST_ERROR;
    }

    /* Once A is gone, B may claim the name that's no longer in use. */
    if (H5Zunregister(UNIQUENAME_FILTER_ID_A) < 0)
        TEST_ERROR;
    if (H5Zregister(&cls_b) < 0)
        TEST_ERROR;
    if (H5Zunregister(UNIQUENAME_FILTER_ID_B) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
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
    if (info.has_set_config || info.has_get_config)
        TEST_ERROR;
    PASSED();
#else
    SKIPPED();
    puts("    deflate filter not built");
    (void)info;
    (void)ret;
#endif

    TESTING("H5Zget_filter_class_info: scaleoffset built-in (canonical name + description)");
    memset(&info, 0xAA, sizeof(info));
    if (H5Zget_filter_class_info(H5Z_FILTER_SCALEOFFSET, &info) < 0)
        TEST_ERROR;
    if (info.id != H5Z_FILTER_SCALEOFFSET)
        TEST_ERROR;
    if (info.name == NULL || strcmp(info.name, "scaleoffset") != 0)
        TEST_ERROR;
    if (info.description == NULL || strstr(info.description, "Scale+offset") == NULL)
        TEST_ERROR;
    if (info.has_set_config || info.has_get_config)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zget_filter_class_info: shuffle built-in (canonical name)");
    memset(&info, 0xAA, sizeof(info));
    if (H5Zget_filter_class_info(H5Z_FILTER_SHUFFLE, &info) < 0)
        TEST_ERROR;
    if (info.id != H5Z_FILTER_SHUFFLE)
        TEST_ERROR;
    if (info.name == NULL || strcmp(info.name, "shuffle") != 0)
        TEST_ERROR;
    if (info.description == NULL)
        TEST_ERROR;
    if (info.has_set_config || info.has_get_config)
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
 * at the H5Z_func2_t callback with correct values during chunk I/O, and
 * that the reserved state argument is NULL.
 * ---------------------------------------------------------------------- */

#define CTXPASS_FILTER_ID 520
#define CTXPASS_NCHUNKS   4 /* 2x2 chunk grid in an 8x8/4x4 dataset */

typedef struct {
    hid_t  expected_dxpl;
    size_t count;
    bool   dxpl_ok;
    bool   ndims_ok;
    bool   scaled_ok;
    bool   state_ok;
    bool   scaled_seen[2][2]; /* [row_chunk][col_chunk] */
} ctxpass_state_t;

static ctxpass_state_t g_ctxpass;

static size_t
ctxpass_filter_cb(unsigned int flags, size_t cd_nelmts, const unsigned int *cd_values, hid_t dxpl_id,
                  const hsize_t *scaled, size_t ndims, void *state, size_t nbytes, size_t *buf_size,
                  void **buf)
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

    /* state is reserved and always NULL */
    if (state != NULL)
        g_ctxpass.state_ok = false;

    if (scaled == NULL || scaled[0] > 1 || scaled[1] > 1)
        g_ctxpass.scaled_ok = false;
    else
        g_ctxpass.scaled_seen[scaled[0]][scaled[1]] = true;

    return nbytes; /* pass-through */
}

static const H5Z_class3_t ctxpass_cls = {
    2, CTXPASS_FILTER_ID, 1, 1, "test_ctxpass_filter", NULL, NULL, ctxpass_filter_cb, NULL, NULL, NULL,
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
    if (!g_ctxpass.state_ok)
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
     * H5Dwrite / H5Dread rather than at a later flush, exposing the
     * dxpl_id, scaled[], and ndims values that arrive at filter2. */
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

    TESTING("filter2 context passthrough: dxpl_id/scaled/ndims/state on write");

    memset(&g_ctxpass, 0, sizeof(g_ctxpass));
    g_ctxpass.expected_dxpl = dxpl;
    g_ctxpass.dxpl_ok       = true;
    g_ctxpass.ndims_ok      = true;
    g_ctxpass.scaled_ok     = true;
    g_ctxpass.state_ok      = true;

    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, wbuf) < 0)
        TEST_ERROR;
    if (check_ctxpass_state() < 0)
        TEST_ERROR;

    PASSED();

    TESTING("filter2 context passthrough: dxpl_id/scaled/ndims/state on read");

    memset(&g_ctxpass, 0, sizeof(g_ctxpass));
    g_ctxpass.expected_dxpl = dxpl;
    g_ctxpass.dxpl_ok       = true;
    g_ctxpass.ndims_ok      = true;
    g_ctxpass.scaled_ok     = true;
    g_ctxpass.state_ok      = true;

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
    nerrors += test_config_get_str_null_buf_size() < 0 ? 1 : 0;

    /* H5Z_class3_t registration and canonical names */
    nerrors += test_canonical_name_display() < 0 ? 1 : 0;
    nerrors += test_class3_name() < 0 ? 1 : 0;
    nerrors += test_name_id_fallback() < 0 ? 1 : 0;
    nerrors += test_canonical_name_length_limit() < 0 ? 1 : 0;
    nerrors += test_canonical_name_uniqueness() < 0 ? 1 : 0;
    nerrors += test_get_filter_info2_builtin() < 0 ? 1 : 0;

    /* Round-trip tests through the built-in filters */
    nerrors += test_roundtrip_deflate(file) < 0 ? 1 : 0;
    nerrors += test_roundtrip_shuffle(file) < 0 ? 1 : 0;
    nerrors += test_roundtrip_fletcher32(file) < 0 ? 1 : 0;

    /* Regression tests */
    nerrors += test_regression_old_api(file) < 0 ? 1 : 0;

    /* filter2 context passthrough: dxpl_id, scaled, ndims */
    nerrors += test_filter2_context_passthrough(file) < 0 ? 1 : 0;

    if (H5Fclose(file) < 0)
        goto error;
    file = H5I_INVALID_HID;

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
