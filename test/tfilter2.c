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
 */

#include "h5test.h"

/* Bit-for-bit double comparison; the accessors must round-trip exactly */
static bool
dbl_same(double a, double b)
{
    return memcmp(&a, &b, sizeof(double)) == 0;
}

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

    /* RFC-HDFG-2026-001 parse-09: keys are case-sensitive */
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
    if (ret <= 0 || !dbl_same(dval, 1.5))
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

    /* "a = {b = ...}" and "a.b = ..." must resolve identically */
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

    /* Decimal literals that overflow or underflow a double (tomlc17 PR #50) */
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

    TESTING("H5Zconfig_get_double: hex-float literal is rejected (not TOML)");
    H5E_BEGIN_TRY
    {
        ret = H5Zconfig_get_double("rate = 0x1.8p+1", "rate", &dval);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_double: 17-digit decimal round-trips exactly");
    {
        const double origs[] = {0.1, 1.0 / 3.0, 3.0, -0.5, 6.02214076e23, 2.2250738585072014e-308};
        char         pstr[64];
        double       rt;
        size_t       i;

        for (i = 0; i < sizeof(origs) / sizeof(origs[0]); i++) {
            snprintf(pstr, sizeof(pstr), "rate = %.17e", origs[i]);
            ret = H5Zconfig_get_double(pstr, "rate", &rt);
            if (ret <= 0 || !dbl_same(origs[i], rt))
                TEST_ERROR;
        }
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

int
main(void)
{
    int nerrors = 0;

    h5_test_init();

    /* Parser tests */
    nerrors += test_parser() < 0 ? 1 : 0;
    nerrors += test_config_get_str_null_buf_size() < 0 ? 1 : 0;

    if (nerrors)
        goto error;

    printf("All tfilter2 tests passed.\n");
    return EXIT_SUCCESS;

error:
    puts("***** TFILTER2 TESTS FAILED *****");
    return EXIT_FAILURE;
}
