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
 *   - The parsed-config handle, key validation, input limits and locale
 */

#include "h5test.h"

#include <locale.h>

/* Parse PARAMS, call an accessor on the result, and free it.  A NULL PARAMS
 * passes a NULL config so that the accessor's own argument check runs. */
#define CFG_CALL(params, call)                                                                               \
    do {                                                                                                     \
        H5Z_config_t *cfg_ = NULL;                                                                           \
        htri_t        r_   = FAIL;                                                                           \
                                                                                                             \
        if ((params) && NULL == (cfg_ = H5Zconfig_parse(params)))                                            \
            return FAIL;                                                                                     \
        r_ = (call);                                                                                         \
        H5Zconfig_close(cfg_);                                                                               \
        return r_;                                                                                           \
    } while (0)

static htri_t
cfg_has_key(const char *params, const char *key)
{
    CFG_CALL(params, H5Zconfig_has_key(cfg_, key));
}
static htri_t
cfg_get_int(const char *params, const char *key, int64_t *out)
{
    CFG_CALL(params, H5Zconfig_get_int(cfg_, key, out));
}
static htri_t
cfg_get_double(const char *params, const char *key, double *out)
{
    CFG_CALL(params, H5Zconfig_get_double(cfg_, key, out));
}
static htri_t
cfg_get_bool(const char *params, const char *key, bool *out)
{
    CFG_CALL(params, H5Zconfig_get_bool(cfg_, key, out));
}
static htri_t
cfg_get_str(const char *params, const char *key, char *buf, size_t *buf_size)
{
    CFG_CALL(params, H5Zconfig_get_str(cfg_, key, buf, buf_size));
}

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
    ret = cfg_get_int("level = 6, mode = 2", "level", &ival);
    if (ret <= 0 || ival != 6)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: key not found");
    ret = cfg_get_int("level = 6", "mode", &ival);
    if (ret != 0)
        TEST_ERROR;
    PASSED();

    /* RFC-HDFG-2026-001 parse-09: keys are case-sensitive */
    TESTING("H5Zconfig_get_int: key lookup is case-sensitive (LEVEL != level)");
    ret = cfg_get_int("LEVEL = 6", "level", &ival);
    if (ret != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_has_key: key present");
    ret = cfg_has_key("level = 6, compress = true", "compress");
    if (ret <= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_has_key: key absent");
    ret = cfg_has_key("level = 6", "mode");
    if (ret != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_str: double-quoted value");
    vsz = sizeof(vbuf);
    ret = cfg_get_str("name = \"hello world\"", "name", vbuf, &vsz);
    if (ret <= 0 || strcmp(vbuf, "hello world") != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_str: single-quoted value");
    vsz = sizeof(vbuf);
    ret = cfg_get_str("name = 'hello world'", "name", vbuf, &vsz);
    if (ret <= 0 || strcmp(vbuf, "hello world") != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_bool: boolean true");
    ret = cfg_get_bool("compress = true", "compress", &bval);
    if (ret <= 0 || !bval)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_bool: boolean false");
    ret = cfg_get_bool("compress = false", "compress", &bval);
    if (ret <= 0 || bval)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_double: float value");
    ret = cfg_get_double("tol = 1.5", "tol", &dval);
    if (ret <= 0 || !dbl_same(dval, 1.5))
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: NULL config error");
    H5E_BEGIN_TRY
    {
        ret = cfg_get_int(NULL, "key", &ival);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: NULL key error");
    H5E_BEGIN_TRY
    {
        ret = cfg_get_int("level = 6", NULL, &ival);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: duplicate key error");
    H5E_BEGIN_TRY
    {
        ret = cfg_get_int("level = 6, level = 9", "level", &ival);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: whitespace around equals");
    ret = cfg_get_int("  level = 6 , mode = 2 ", "level", &ival);
    if (ret <= 0 || ival != 6)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: braced inline-table form");
    ret = cfg_get_int("{level = 6, mode = 2}", "level", &ival);
    if (ret <= 0 || ival != 6)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_str: braced inline-table form");
    vsz = sizeof(vbuf);
    ret = cfg_get_str("{ coding = \"entropy\" }", "coding", vbuf, &vsz);
    if (ret <= 0 || strcmp(vbuf, "entropy") != 0)
        TEST_ERROR;
    PASSED();

    /* "a = {b = ...}" and "a.b = ..." must resolve identically */
    TESTING("H5Zconfig_get_str: dotted-key into nested table (dotted form)");
    vsz = sizeof(vbuf);
    ret = cfg_get_str("compressor.name = \"zlib\", shuffle = 1", "compressor.name", vbuf, &vsz);
    if (ret <= 0 || strcmp(vbuf, "zlib") != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: dotted-key into nested table (inline-table form)");
    ret = cfg_get_int("compressor = {name = \"zlib\", level = 6}", "compressor.level", &ival);
    if (ret <= 0 || ival != 6)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: top-level sibling alongside nested table");
    ret = cfg_get_int("compressor = {name = \"zlib\", level = 6}, shuffle = 1", "shuffle", &ival);
    if (ret <= 0 || ival != 1)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_str: missing dotted key returns 0");
    vsz = sizeof(vbuf);
    ret = cfg_get_str("compressor.name = \"zlib\"", "compressor.missing", vbuf, &vsz);
    if (ret != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_str: type mismatch error (integer key)");
    H5E_BEGIN_TRY
    {
        vsz = sizeof(vbuf);
        ret = cfg_get_str("level = 6", "level", vbuf, &vsz);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: negative integer");
    ret = cfg_get_int("offset = -4", "offset", &ival);
    if (ret <= 0 || ival != -4)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_double: scientific notation");
    ret = cfg_get_double("tol = 1.0e-6", "tol", &dval);
    if (ret <= 0 || dval < 9.9e-7 || dval > 1.1e-6)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_str: comma inside quoted value");
    vsz = sizeof(vbuf);
    ret = cfg_get_str("path = \"/data/run_1,v2/dict.bin\"", "path", vbuf, &vsz);
    if (ret <= 0 || strcmp(vbuf, "/data/run_1,v2/dict.bin") != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_str: backslash-quote escape in double-quoted value");
    vsz = sizeof(vbuf);
    ret = cfg_get_str("msg = \"say \\\"hi\\\"\"", "msg", vbuf, &vsz);
    if (ret <= 0 || strcmp(vbuf, "say \"hi\"") != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_has_key: empty string is valid (no params)");
    ret = cfg_has_key("", "level");
    if (ret != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_double: inf rejected");
    H5E_BEGIN_TRY
    {
        ret = cfg_get_double("tol = inf", "tol", &dval);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_double: nan rejected");
    H5E_BEGIN_TRY
    {
        ret = cfg_get_double("tol = nan", "tol", &dval);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    /* Decimal literals that overflow or underflow a double (tomlc17 PR #50) */
    TESTING("H5Zconfig_get_double: literal overflowing to inf rejected");
    H5E_BEGIN_TRY
    {
        ret = cfg_get_double("tol = 1e400", "tol", &dval);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_double: literal underflowing to -0.0 rejected");
    H5E_BEGIN_TRY
    {
        ret = cfg_get_double("tol = -1e-400", "tol", &dval);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: semicolon outside quotes rejected");
    H5E_BEGIN_TRY
    {
        ret = cfg_get_int("level = 6; mode = 2", "level", &ival);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: underscore digit separator");
    ret = cfg_get_int("count = 1_000_000", "count", &ival);
    if (ret <= 0 || ival != 1000000)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: hex prefix 0x");
    ret = cfg_get_int("flags = 0xff", "flags", &ival);
    if (ret <= 0 || ival != 255)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_double: hex-float literal is rejected (not TOML)");
    H5E_BEGIN_TRY
    {
        ret = cfg_get_double("rate = 0x1.8p+1", "rate", &dval);
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
            ret = cfg_get_double(pstr, "rate", &rt);
            if (ret <= 0 || !dbl_same(origs[i], rt))
                TEST_ERROR;
        }
    }
    PASSED();

    /* --- Malformed input ------------------------------------------------- */

    TESTING("H5Zconfig_get_int: missing '=' is a parse error");
    H5E_BEGIN_TRY
    {
        ret = cfg_get_int("level6", "level", &ival);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_int: empty value after '=' is a parse error");
    H5E_BEGIN_TRY
    {
        ret = cfg_get_int("level =", "level", &ival);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_str: unterminated double-quote is a parse error");
    H5E_BEGIN_TRY
    {
        vsz = sizeof(vbuf);
        ret = cfg_get_str("name = \"hello", "name", vbuf, &vsz);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_str: unterminated single-quote is a parse error");
    H5E_BEGIN_TRY
    {
        vsz = sizeof(vbuf);
        ret = cfg_get_str("name = 'hello", "name", vbuf, &vsz);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    /* --- Boolean edge cases ---------------------------------------------- */

    TESTING("H5Zconfig_get_bool: uppercase TRUE is rejected (TOML case-sensitive)");
    H5E_BEGIN_TRY
    {
        ret = cfg_get_bool("flag = TRUE", "flag", &bval);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_bool: integer 1 is a type error (not a boolean)");
    H5E_BEGIN_TRY
    {
        ret = cfg_get_bool("flag = 1", "flag", &bval);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();

    /* --- Miscellaneous value content ------------------------------------- */

    TESTING("H5Zconfig_get_int: single-character key");
    ret = cfg_get_int("x = 7", "x", &ival);
    if (ret <= 0 || ival != 7)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_str: equals sign inside quoted value");
    vsz = sizeof(vbuf);
    ret = cfg_get_str("expr = \"a=b\"", "expr", vbuf, &vsz);
    if (ret <= 0 || strcmp(vbuf, "a=b") != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_str: backslash-n escape in double-quoted value");
    vsz = sizeof(vbuf);
    ret = cfg_get_str("msg = \"line1\\nline2\"", "msg", vbuf, &vsz);
    if (ret <= 0 || strcmp(vbuf, "line1\nline2") != 0)
        TEST_ERROR;
    PASSED();

    /* --- H5Zconfig_get_str buffer-size edge cases ------------------------ */

    TESTING("H5Zconfig_get_str: size-query (buf=NULL sets *buf_size)");
    vsz = 0;
    ret = cfg_get_str("name = \"hello\"", "name", NULL, &vsz);
    if (ret <= 0 || vsz != 5) /* "hello" is 5 chars */
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_str: buf_size=1 returns overflow error but sets *buf_size");
    {
        char   tiny[1];
        size_t tsz = 1;
        H5E_BEGIN_TRY
        {
            ret = cfg_get_str("name = \"hello\"", "name", tiny, &tsz);
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
        ret = cfg_get_str(params, "key", buf, NULL);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    PASSED();
    return 0;

error:
    return -1;
}

/* One parsed handle serves many lookups; empty strings and key validation */
static int
test_config_handle(void)
{
    const char   *known[]       = {"level", "mode", "opt.fast", NULL};
    const char   *short_known[] = {"level", NULL};
    H5Z_config_t *cfg           = NULL;
    int64_t       ival          = 0;
    bool          bval          = false;
    herr_t        ret;

    TESTING("H5Z_config_t: one handle, several lookups");
    if (NULL == (cfg = H5Zconfig_parse("level = 6, mode = 2, opt = {fast = true}")))
        TEST_ERROR;
    if (H5Zconfig_get_int(cfg, "level", &ival) <= 0 || ival != 6)
        TEST_ERROR;
    if (H5Zconfig_get_int(cfg, "mode", &ival) <= 0 || ival != 2)
        TEST_ERROR;
    if (H5Zconfig_get_bool(cfg, "opt.fast", &bval) <= 0 || !bval)
        TEST_ERROR;
    if (H5Zconfig_has_key(cfg, "missing") != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_validate_keys: known, nested and unknown keys");
    if (H5Zconfig_validate_keys(cfg, known) < 0)
        TEST_ERROR;
    H5E_BEGIN_TRY
    {
        ret = H5Zconfig_validate_keys(cfg, short_known);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    H5Zconfig_close(cfg);
    cfg = NULL;
    PASSED();

    TESTING("H5Z_config_t: empty string has no keys and validates");
    if (NULL == (cfg = H5Zconfig_parse("")))
        TEST_ERROR;
    if (H5Zconfig_has_key(cfg, "level") != 0)
        TEST_ERROR;
    if (H5Zconfig_validate_keys(cfg, short_known) < 0)
        TEST_ERROR;
    H5Zconfig_close(cfg);
    cfg = NULL;
    PASSED();

    TESTING("H5Z_config_t: malformed string fails to parse");
    H5E_BEGIN_TRY
    {
        cfg = H5Zconfig_parse("level = ");
    }
    H5E_END_TRY
    if (cfg != NULL)
        TEST_ERROR;
    PASSED();

    return 0;

error:
    H5Zconfig_close(cfg);
    return -1;
}

/* True if PARAMS fails to parse */
static bool
parse_fails(const char *params)
{
    H5Z_config_t *cfg = NULL;

    H5E_BEGIN_TRY
    {
        cfg = H5Zconfig_parse(params);
    }
    H5E_END_TRY
    if (cfg) {
        H5Zconfig_close(cfg);
        return false;
    }
    return true;
}

/* Input the wrapper must reject, and edge cases it must accept */
static int
test_config_strict_input(void)
{
    H5Z_config_t *cfg  = NULL;
    int64_t       ival = 0;

    TESTING("H5Zconfig_parse: '}' cannot close the table early");
    if (!parse_fails("a = 1}\nevil = 2\n#"))
        TEST_ERROR;
    if (!parse_fails("a = 1}\n[evil]\nx = 1\n#"))
        TEST_ERROR;
    if (!parse_fails("a = 1} # junk"))
        TEST_ERROR;
    if (!parse_fails("a = 1}, b = {"))
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_parse: '{' without a closing '}' is rejected");
    if (!parse_fails("{a = 1"))
        TEST_ERROR;
    if (!parse_fails("{"))
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_parse: trailing comment is allowed");
    if (cfg_get_int("a = 1 # comment", "a", &ival) <= 0 || ival != 1)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_parse: quoted key with '.' or NUL is rejected");
    if (!parse_fails("\"a.b\" = 1"))
        TEST_ERROR;
    if (!parse_fails("t = {\"a.b\" = 1}"))
        TEST_ERROR;
    if (!parse_fails("\"a\\u0000b\" = 1"))
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_parse: string value with NUL is rejected");
    if (!parse_fails("s = \"ok\\u0000hidden\""))
        TEST_ERROR;
    if (!parse_fails("s = [\"ok\", \"x\\u0000y\"]"))
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_parse: NULL, braces only and blanks give no keys");
    {
        const char *empties[] = {"{}", "{ }", "   ", " { } "};
        size_t      i;

        if (NULL == (cfg = H5Zconfig_parse(NULL)))
            TEST_ERROR;
        if (H5Zconfig_has_key(cfg, "a") != 0)
            TEST_ERROR;
        H5Zconfig_close(cfg);
        cfg = NULL;
        for (i = 0; i < sizeof(empties) / sizeof(empties[0]); i++) {
            if (NULL == (cfg = H5Zconfig_parse(empties[i])))
                TEST_ERROR;
            if (H5Zconfig_has_key(cfg, "a") != 0)
                TEST_ERROR;
            H5Zconfig_close(cfg);
            cfg = NULL;
        }
        if (H5Zconfig_close(NULL) < 0)
            TEST_ERROR;
    }
    PASSED();

    return 0;

error:
    H5Zconfig_close(cfg);
    return -1;
}

/* H5Z_CONFIG_STRING_MAX, H5Z_CONFIG_MAX_PARAMS and the 255-character key limit */
static int
test_config_limits(void)
{
    const char   *known[] = {"k", NULL};
    char         *buf     = NULL;
    char          key[300];
    H5Z_config_t *cfg = NULL;
    size_t        len;
    int           i;
    herr_t        ret;

    if (NULL == (buf = (char *)malloc(H5Z_CONFIG_STRING_MAX + 2)))
        TEST_ERROR;

    TESTING("H5Zconfig_parse: H5Z_CONFIG_STRING_MAX bytes parse, one more fails");
    /* k = "xxx...x" with the total length exactly H5Z_CONFIG_STRING_MAX */
    strcpy(buf, "k = \"");
    len = strlen(buf);
    memset(buf + len, 'x', H5Z_CONFIG_STRING_MAX - len - 1);
    buf[H5Z_CONFIG_STRING_MAX - 1] = '"';
    buf[H5Z_CONFIG_STRING_MAX]     = '\0';
    if (strlen(buf) != H5Z_CONFIG_STRING_MAX || NULL == (cfg = H5Zconfig_parse(buf)))
        TEST_ERROR;
    H5Zconfig_close(cfg);
    cfg                            = NULL;
    buf[H5Z_CONFIG_STRING_MAX - 1] = 'x';
    buf[H5Z_CONFIG_STRING_MAX]     = '"';
    buf[H5Z_CONFIG_STRING_MAX + 1] = '\0';
    if (!parse_fails(buf))
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_parse: H5Z_CONFIG_MAX_PARAMS keys parse, one more fails");
    buf[0] = '\0';
    for (i = 0; i < H5Z_CONFIG_MAX_PARAMS; i++)
        snprintf(buf + strlen(buf), H5Z_CONFIG_STRING_MAX - strlen(buf), "%sk%d = %d", i ? ", " : "", i, i);
    if (NULL == (cfg = H5Zconfig_parse(buf)))
        TEST_ERROR;
    H5Zconfig_close(cfg);
    cfg = NULL;
    snprintf(buf + strlen(buf), H5Z_CONFIG_STRING_MAX - strlen(buf), ", extra = 1");
    if (!parse_fails(buf))
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_parse: keys in nested tables count toward the limit");
    strcpy(buf, "t = {");
    for (i = 0; i < H5Z_CONFIG_MAX_PARAMS; i++)
        snprintf(buf + strlen(buf), H5Z_CONFIG_STRING_MAX - strlen(buf), "%sk%d = %d", i ? ", " : "", i, i);
    strcat(buf, "}");
    if (NULL == (cfg = H5Zconfig_parse(buf)))
        TEST_ERROR;
    H5Zconfig_close(cfg);
    cfg = NULL;
    strcat(buf, ", u = {v = 1}");
    if (!parse_fails(buf))
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_*: keys longer than 255 characters are errors");
    if (NULL == (cfg = H5Zconfig_parse("k = 1")))
        TEST_ERROR;
    memset(key, 'k', 255);
    key[255] = '\0';
    if (H5Zconfig_has_key(cfg, key) != 0)
        TEST_ERROR;
    key[255] = 'k';
    key[256] = '\0';
    H5E_BEGIN_TRY
    {
        ret = H5Zconfig_has_key(cfg, key);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    H5Zconfig_close(cfg);
    cfg = NULL;

    /* A 300-character key parses but cannot be validated */
    memset(key, 'k', 299);
    key[299] = '\0';
    snprintf(buf, H5Z_CONFIG_STRING_MAX, "%s = 1", key);
    if (NULL == (cfg = H5Zconfig_parse(buf)))
        TEST_ERROR;
    H5E_BEGIN_TRY
    {
        ret = H5Zconfig_validate_keys(cfg, known);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    H5Zconfig_close(cfg);
    cfg = NULL;
    PASSED();

    free(buf);
    return 0;

error:
    H5Zconfig_close(cfg);
    free(buf);
    return -1;
}

/* H5Zconfig_validate_keys with dotted keys and empty tables */
static int
test_config_validate_more(void)
{
    const char   *dotted[] = {"opt.fast", NULL};
    const char   *level[]  = {"level", NULL};
    const char   *opt[]    = {"opt", NULL};
    H5Z_config_t *cfg      = NULL;
    herr_t        ret;

    TESTING("H5Zconfig_validate_keys: dotted form matches a dotted path");
    if (NULL == (cfg = H5Zconfig_parse("opt.fast = true")))
        TEST_ERROR;
    if (H5Zconfig_validate_keys(cfg, dotted) < 0)
        TEST_ERROR;
    H5Zconfig_close(cfg);
    cfg = NULL;
    PASSED();

    TESTING("H5Zconfig_validate_keys: unknown empty table is rejected");
    if (NULL == (cfg = H5Zconfig_parse("level = 1, unknown = {}")))
        TEST_ERROR;
    H5E_BEGIN_TRY
    {
        ret = H5Zconfig_validate_keys(cfg, level);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    H5Zconfig_close(cfg);
    cfg = NULL;
    PASSED();

    TESTING("H5Zconfig_validate_keys: known empty table is accepted");
    if (NULL == (cfg = H5Zconfig_parse("opt = {}")))
        TEST_ERROR;
    if (H5Zconfig_validate_keys(cfg, opt) < 0)
        TEST_ERROR;
    H5Zconfig_close(cfg);
    cfg = NULL;
    PASSED();

    return 0;

error:
    H5Zconfig_close(cfg);
    return -1;
}

/* H5Zconfig_get_str at and around the exact buffer size */
static int
test_config_get_str_sizes(void)
{
    const char *params = "name = \"hello\"";
    char        buf[8];
    size_t      sz;
    htri_t      ret;

    TESTING("H5Zconfig_get_str: buffer of exactly length+1 succeeds");
    sz = 6;
    memset(buf, 'X', sizeof(buf));
    if (cfg_get_str(params, "name", buf, &sz) <= 0 || sz != 5 || strcmp(buf, "hello") != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_str: buffer of length fails and truncates");
    sz = 5;
    memset(buf, 'X', sizeof(buf));
    H5E_BEGIN_TRY
    {
        ret = cfg_get_str(params, "name", buf, &sz);
    }
    H5E_END_TRY
    if (ret >= 0 || sz != 5 || strcmp(buf, "hell") != 0)
        TEST_ERROR;
    PASSED();

    TESTING("H5Zconfig_get_str: buf and buf_size both NULL checks the type");
    if (cfg_get_str(params, "name", NULL, NULL) <= 0)
        TEST_ERROR;
    PASSED();

    return 0;

error:
    return -1;
}

/* Floats parse with '.' under a comma-decimal locale, and the caller's locale
 * is left as it was.  Set HDF5_REQUIRE_LOCALE_TEST to fail rather than skip
 * when no comma-decimal locale is installed. */
static int
test_config_locale(void)
{
    const char *names[] = {"de_DE.UTF-8", "de_DE.utf8", "de_DE",          "fr_FR.UTF-8",
                           "fr_FR.utf8",  "fr_FR",      "German_Germany", "French_France"};
    const char *cur;
    char       *saved = NULL;
    double      dval  = 0.0;
    bool        found = false;
    size_t      i;

    TESTING("H5Zconfig_get_double: '.' parses under a comma-decimal locale");

    if (NULL == (cur = setlocale(LC_NUMERIC, NULL)) || NULL == (saved = strdup(cur)))
        TEST_ERROR;
    for (i = 0; i < sizeof(names) / sizeof(names[0]) && !found; i++)
        if (setlocale(LC_NUMERIC, names[i]) && strcmp(localeconv()->decimal_point, ",") == 0)
            found = true;
    if (!found) {
        setlocale(LC_NUMERIC, saved);
        free(saved);
        if (getenv("HDF5_REQUIRE_LOCALE_TEST")) {
            H5_FAILED();
            puts("    no comma-decimal locale installed (HDF5_REQUIRE_LOCALE_TEST is set)");
            return -1;
        }
        SKIPPED();
        puts("    no comma-decimal locale installed");
        return 0;
    }

    if (cfg_get_double("rate = 1.5", "rate", &dval) <= 0 || !dbl_same(dval, 1.5))
        TEST_ERROR;
    if (strcmp(localeconv()->decimal_point, ",") != 0)
        TEST_ERROR;
    if (!parse_fails("rate = 1,5"))
        TEST_ERROR;
    if (strcmp(localeconv()->decimal_point, ",") != 0)
        TEST_ERROR;

    setlocale(LC_NUMERIC, saved);
    free(saved);
    PASSED();
    return 0;

error:
    if (saved) {
        setlocale(LC_NUMERIC, saved);
        free(saved);
    }
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
    nerrors += test_config_handle() < 0 ? 1 : 0;
    nerrors += test_config_strict_input() < 0 ? 1 : 0;
    nerrors += test_config_limits() < 0 ? 1 : 0;
    nerrors += test_config_validate_more() < 0 ? 1 : 0;
    nerrors += test_config_get_str_sizes() < 0 ? 1 : 0;
    nerrors += test_config_locale() < 0 ? 1 : 0;

    if (nerrors)
        goto error;

    printf("All tfilter2 tests passed.\n");
    return EXIT_SUCCESS;

error:
    puts("***** TFILTER2 TESTS FAILED *****");
    return EXIT_FAILURE;
}
