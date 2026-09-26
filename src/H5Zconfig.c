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
 * Parsing and typed lookup of filter parameter strings, built on the vendored
 * tomlc17 parser.
 */

#define H5Z_FRIEND /* suppress error on H5Zpkg.h include */

#include "H5Zmodule.h"

#include "H5private.h"   /* Generic Functions   */
#include "H5Eprivate.h"  /* Error handling      */
#include "H5MMprivate.h" /* Memory management   */
#include "H5Zpkg.h"      /* Filter internals    */

/* The prefix headers rename the vendored symbols and must come first */
#include "tomlc17/h5_toml_prefix.h"
#include "tomlc17/tomlc17.h"
#include "ryu/h5_ryu_prefix.h"
#include "ryu/ryu.h"

/* Append one source character to the output buffer, or skip it if full. */
static inline void
H5Z__copy_char(char *out, size_t cap, size_t *pos, const char **p)
{
    if (*pos + 1 < cap)
        out[(*pos)++] = **p;
    (*p)++;
}

/* Append two source characters (a backslash escape), or skip both if full. */
static inline void
H5Z__copy_chars2(char *out, size_t cap, size_t *pos, const char **p)
{
    if (*pos + 2 < cap) {
        out[(*pos)++] = (*p)[0];
        out[(*pos)++] = (*p)[1];
    }
    (*p) += 2;
}

/* Tests the bit pattern because fast-math builds (icx's default, or
 * -ffinite-math-only) may fold isnan()/isinf() to 0. */
static inline bool
H5Z__fp64_is_inf_or_nan(double v)
{
#if H5_SIZEOF_DOUBLE == 8
    uint64_t bits;

    memcpy(&bits, &v, sizeof(v));
    return (bits & 0x7ff0000000000000ULL) == 0x7ff0000000000000ULL;
#else
    return isnan(v) || isinf(v);
#endif
}

/* TOML's spelling of a non-finite double, or NULL if V is finite.  Ryu's
 * "NaN"/"Infinity" are not valid TOML. */
static inline const char *
H5Z__fp64_nonfinite_toml(double v)
{
    if (!H5Z__fp64_is_inf_or_nan(v))
        return NULL;

#if H5_SIZEOF_DOUBLE == 8
    {
        uint64_t bits;

        memcpy(&bits, &v, sizeof(v));
        if (bits & 0x000fffffffffffffULL)
            return "nan";
        return (bits & 0x8000000000000000ULL) ? "-inf" : "inf";
    }
#else
    if (isnan(v))
        return "nan";
    return (v < 0.0) ? "-inf" : "inf";
#endif
}

/*
 * Format VAL as the shortest decimal that round-trips to the same double and
 * that TOML lexes as a float (never a bare integer like "8").  Ryu supplies
 * the digits (see issue #6153); they are re-laid out with printf("%g")'s
 * fixed/scientific rule, which moves the decimal point but never changes a
 * digit.  Locale-independent.
 *
 * Returns the length written (excluding the NUL), or -1 if BUFSIZE is too
 * small.
 */
static int
H5Z__format_double_canonical(char *buf, size_t bufsize, double val)
{
    const char *nonfinite = H5Z__fp64_nonfinite_toml(val);
    char        ryu[32];       /* d2s_buffered_n() writes at most 24 chars */
    char        dig[24] = {0}; /* at most 17 significant digits */
    char        tmp[40];       /* longest result is 24 chars + NUL */
    int         ryu_len, i, ndigits = 0, e10 = 0, n = 0;
    bool        neg, exp_neg;

    if (nonfinite) {
        n = (int)strlen(nonfinite);
        if ((size_t)n >= bufsize)
            return -1;
        memcpy(buf, nonfinite, (size_t)n + 1);
        return n;
    }

    /* Split Ryu's "[-]d[.ddd]E[-]ddd" (not NUL-terminated) into sign, digits
     * and exponent */
    ryu_len = d2s_buffered_n(val, ryu);

    i   = 0;
    neg = (ryu[0] == '-');
    if (neg)
        i++;
    while (i < ryu_len && ryu[i] != 'E') {
        if (ryu[i] != '.')
            dig[ndigits++] = ryu[i];
        i++;
    }
    i++; /* skip 'E' */
    exp_neg = (ryu[i] == '-');
    if (exp_neg)
        i++;
    while (i < ryu_len)
        e10 = e10 * 10 + (ryu[i++] - '0');
    if (exp_neg)
        e10 = -e10;

    if (neg)
        tmp[n++] = '-';

    if (e10 >= -4 && e10 < ndigits) {
        /* Fixed-point; e10 < ndigits means no right-hand zero padding */
        if (e10 >= 0) {
            for (i = 0; i <= e10; i++)
                tmp[n++] = dig[i];
            tmp[n++] = '.';
            if (e10 + 1 == ndigits)
                tmp[n++] = '0'; /* force float lexical class */
            else
                for (i = e10 + 1; i < ndigits; i++)
                    tmp[n++] = dig[i];
        }
        else {
            tmp[n++] = '0';
            tmp[n++] = '.';
            for (i = 0; i < -e10 - 1; i++)
                tmp[n++] = '0';
            for (i = 0; i < ndigits; i++)
                tmp[n++] = dig[i];
        }
    }
    else {
        /* Scientific, with printf("%e")'s two-digit signed exponent */
        int abs_e10 = (e10 < 0) ? -e10 : e10;

        tmp[n++] = dig[0];
        if (ndigits > 1) {
            tmp[n++] = '.';
            for (i = 1; i < ndigits; i++)
                tmp[n++] = dig[i];
        }
        tmp[n++] = 'e';
        tmp[n++] = (e10 < 0) ? '-' : '+';
        if (abs_e10 >= 100)
            tmp[n++] = (char)('0' + abs_e10 / 100);
        tmp[n++] = (char)('0' + (abs_e10 / 10) % 10);
        tmp[n++] = (char)('0' + abs_e10 % 10);
    }
    tmp[n] = '\0';

    if ((size_t)n >= bufsize)
        return -1;
    memcpy(buf, tmp, (size_t)n + 1);
    return n;
}

/*
 * Return a copy of SRC with C99 hex-float literals ("0x1.8p+1") replaced by
 * exact decimals, since TOML has no hex-float syntax.  Quoted strings and
 * comments are left alone.  Caller frees with H5MM_xfree().
 */
static char *
H5Z__rewrite_hexfloats(const char *src)
{
    const char *p   = src;
    size_t      len = strlen(src);
    size_t      cap;
    char       *out;
    size_t      pos = 0;

    /* A rewrite grows a token by under 4x ("0x1p99" -> "6.338253001141147e+29") */
    if (len > (SIZE_MAX - 1) / 8)
        return NULL;
    cap = len * 8 + 1;
    out = (char *)H5MM_malloc(cap);

    if (!out)
        return NULL;

    while (*p) {
        if (*p == '"') {
            H5Z__copy_char(out, cap, &pos, &p);
            while (*p && *p != '"') {
                if (*p == '\\' && *(p + 1))
                    H5Z__copy_chars2(out, cap, &pos, &p);
                else
                    H5Z__copy_char(out, cap, &pos, &p);
            }
            if (*p == '"')
                H5Z__copy_char(out, cap, &pos, &p);
            continue;
        }

        if (*p == '\'') {
            H5Z__copy_char(out, cap, &pos, &p);
            while (*p && *p != '\'')
                H5Z__copy_char(out, cap, &pos, &p);
            if (*p == '\'')
                H5Z__copy_char(out, cap, &pos, &p);
            continue;
        }

        if (*p == '#') {
            while (*p && *p != '\n')
                H5Z__copy_char(out, cap, &pos, &p);
            continue;
        }

        const char *tok_start = p;
        if (*p == '+' || *p == '-')
            p++;

        if (p[0] == '0' && (p[1] == 'x' || p[1] == 'X')) {
            const char *q = p + 2;
            while (isxdigit((unsigned char)*q) || *q == '_')
                q++;
            if (*q == '.' || *q == 'p' || *q == 'P') {
                if (*q == '.')
                    q++;
                while (isxdigit((unsigned char)*q) || *q == '_')
                    q++;
                if (*q == 'p' || *q == 'P') {
                    q++;
                    if (*q == '+' || *q == '-')
                        q++;
                    while (isdigit((unsigned char)*q))
                        q++;
                    size_t tok_len = (size_t)(q - tok_start);
                    char   tmp[64];
                    if (tok_len < sizeof(tmp)) {
                        memcpy(tmp, tok_start, tok_len);
                        tmp[tok_len] = '\0';
                        char  *end;
                        double val = strtod(tmp, &end);
                        if (end == tmp + tok_len) {
                            char dec[32];
                            int  n = H5Z__format_double_canonical(dec, sizeof(dec), val);
                            if (n >= 0 && pos + (size_t)n < cap) {
                                memcpy(out + pos, dec, (size_t)n);
                                pos += (size_t)n;
                                p = q;
                                continue;
                            }
                        }
                    }
                }
            }
        }

        p = tok_start;
        H5Z__copy_char(out, cap, &pos, &p);
    }
    out[pos] = '\0';
    return out;
}

/*
 * Wrap PARAMS, with or without its outer braces, as the TOML document
 * "__p__ = {...}".  User keys live inside the inline table, so they cannot
 * collide with "__p__".  Caller frees with H5MM_xfree().
 */
static char *
H5Z__toml_wrap(const char *params)
{
    const char *p = params ? params : "";
    const char *e;
    size_t      content_len;
    size_t      wlen;
    char       *buf;

    while (*p == ' ' || *p == '\t')
        p++;

    if (*p == '{') {
        p++;
        e = p + strlen(p);
        while (e > p && (*(e - 1) == ' ' || *(e - 1) == '\t'))
            e--;
        if (e > p && *(e - 1) == '}')
            e--;
    }
    else {
        e = p + strlen(p);
    }
    content_len = (size_t)(e - p);

    wlen = content_len + sizeof("__p__ = {}");
    buf  = (char *)H5MM_malloc(wlen);
    if (buf)
        snprintf(buf, wlen, "__p__ = {%.*s}", (int)content_len, p);
    return buf;
}

/*-------------------------------------------------------------------------
 * Function:    H5Z_canonicalize_params
 *
 * Purpose:     Return a copy of PARAMS in the form stored in pipeline v3:
 *              outer braces and surrounding whitespace removed, hex-floats
 *              rewritten as exact decimals, everything else unchanged.  The
 *              result is valid TOML, so other readers can use a stock TOML
 *              parser.
 *
 * Return:      Success:    Heap-allocated NUL-terminated string, freed by
 *                          the caller with H5MM_xfree().
 *              Failure:    NULL
 *-------------------------------------------------------------------------
 */
char *
H5Z_canonicalize_params(const char *params)
{
    char       *expanded  = NULL;
    char       *ret_value = NULL;
    const char *p;
    const char *e;
    size_t      len;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if (params == NULL)
        HGOTO_DONE(NULL);

    if (NULL == (expanded = H5Z__rewrite_hexfloats(params)))
        HGOTO_DONE(NULL);

    p = expanded;
    while (*p == ' ' || *p == '\t')
        p++;
    if (*p == '{') {
        p++;
        e = p + strlen(p);
        while (e > p && (*(e - 1) == ' ' || *(e - 1) == '\t'))
            e--;
        if (e > p && *(e - 1) == '}')
            e--;
    }
    else
        e = p + strlen(p);

    while (p < e && (*p == ' ' || *p == '\t'))
        p++;
    while (e > p && (*(e - 1) == ' ' || *(e - 1) == '\t'))
        e--;

    len = (size_t)(e - p);
    if (NULL != (ret_value = (char *)H5MM_malloc(len + 1))) {
        H5MM_memcpy(ret_value, p, len);
        ret_value[len] = '\0';
    }

done:
    H5MM_xfree(expanded);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Z_canonicalize_params() */

/* Count leaf keys, including those in nested tables */
static size_t
H5Z__count_table_keys(toml_datum_t tab)
{
    size_t  count = 0;
    int32_t i;

    for (i = 0; i < tab.u.tab.size; i++) {
        toml_datum_t v = tab.u.tab.value[i];

        if (v.type == TOML_TABLE)
            count += H5Z__count_table_keys(v);
        else
            count++;
    }

    return count;
}

/*
 * Parse PARAMS.  On success *ptab_out is the parameter table and the caller
 * must toml_free(*tr_out); on failure *tr_out is zeroed.
 */
static htri_t
H5Z__toml_parse_params(const char *params, toml_result_t *tr_out, toml_datum_t *ptab_out)
{
    char  *expanded  = NULL;
    char  *wrapped   = NULL;
    htri_t ret_value = true;

    FUNC_ENTER_PACKAGE

    if (params && strlen(params) > H5Z_CONFIG_STRING_MAX)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                    "filter parameter string exceeds H5Z_CONFIG_STRING_MAX (%d bytes)",
                    H5Z_CONFIG_STRING_MAX);

    if (params && *params) {
        if (NULL == (expanded = H5Z__rewrite_hexfloats(params)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "out of memory rewriting hex-float literals");
        params = expanded;
    }

    if (NULL == (wrapped = H5Z__toml_wrap(params)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "out of memory for TOML wrapper buffer");

    *tr_out = toml_parse(wrapped, (int)strlen(wrapped));

    if (!tr_out->ok) {
        /* errbuf below is sized with sizeof */
        _Static_assert(sizeof(tr_out->errmsg) > sizeof(void *),
                       "toml_result_t.errmsg must be a fixed-size char array, not a pointer");
        char errbuf[sizeof(tr_out->errmsg)];
        memcpy(errbuf, tr_out->errmsg, sizeof(errbuf));
        toml_free(*tr_out);
        memset(tr_out, 0, sizeof(*tr_out));
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "TOML parse error in filter parameter string: %s", errbuf);
    }

    *ptab_out = toml_get(tr_out->toptab, "__p__");
    if (ptab_out->type != TOML_TABLE) {
        toml_free(*tr_out);
        memset(tr_out, 0, sizeof(*tr_out));
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                    "malformed filter parameter string (not a valid TOML inline table)");
    }

    if (H5Z__count_table_keys(*ptab_out) > H5Z_CONFIG_MAX_PARAMS) {
        toml_free(*tr_out);
        memset(tr_out, 0, sizeof(*tr_out));
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                    "filter parameter string exceeds H5Z_CONFIG_MAX_PARAMS (%d key-value pairs)",
                    H5Z_CONFIG_MAX_PARAMS);
    }

done:
    H5MM_xfree(wrapped);
    H5MM_xfree(expanded);
    FUNC_LEAVE_NOAPI(ret_value)
}

/* Check each leaf key's dotted path against KNOWN_KEYS */
static herr_t
H5Z__validate_table_keys(toml_datum_t tab, const char *prefix, const char *const *known_keys)
{
    int32_t i;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    for (i = 0; i < tab.u.tab.size; i++) {
        const char  *k = tab.u.tab.key[i];
        toml_datum_t v = tab.u.tab.value[i];
        char         full[H5Z_CONFIG_MAX_KEY_PATH];
        size_t       ki;
        bool         found = false;

        if (prefix && *prefix) {
            if (snprintf(full, sizeof(full), "%s.%s", prefix, k) >= (int)sizeof(full))
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "filter parameter key path too long: %s.%s", prefix,
                            k);
        }
        else {
            if (snprintf(full, sizeof(full), "%s", k) >= (int)sizeof(full))
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "filter parameter key too long: %s", k);
        }

        if (v.type == TOML_TABLE) {
            if (H5Z__validate_table_keys(v, full, known_keys) < 0)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unknown parameter key in nested table");
            continue;
        }

        for (ki = 0; known_keys[ki] != NULL; ki++) {
            if (strcmp(full, known_keys[ki]) == 0) {
                found = true;
                break;
            }
        }
        if (!found)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unknown parameter key '%s' in filter configuration",
                        full);
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*
 * Fail if PARAMS contains a key not in KNOWN_KEYS.  Nested keys are matched
 * by dotted path, so "a = {b = 1}" and "a.b = 1" are equivalent.
 */
herr_t
H5Z__config_validate_keys(const char *params, const char *const *known_keys)
{
    toml_result_t tr;
    toml_datum_t  ptab;
    bool          tr_valid  = false;
    herr_t        ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    if (!params || *params == '\0')
        HGOTO_DONE(SUCCEED);

    if (strlen(params) > H5Z_CONFIG_STRING_MAX)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                    "filter parameter string exceeds H5Z_CONFIG_STRING_MAX (%d bytes)",
                    H5Z_CONFIG_STRING_MAX);

    if (H5Z__toml_parse_params(params, &tr, &ptab) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "failed to parse filter parameter string");
    tr_valid = true;

    if (known_keys) {
        if (H5Z__validate_table_keys(ptab, NULL, known_keys) < 0)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unknown parameter key in filter configuration");
    }

done:
    if (tr_valid)
        toml_free(tr);
    FUNC_LEAVE_NOAPI(ret_value)
}

/*
 * Parse PARAMS and look up KEY (a dotted path is allowed).  The caller must
 * toml_free(*tr) only when this returns > 0.
 */
static htri_t
H5Z__config_get_datum(const char *params, const char *key, toml_result_t *tr, toml_datum_t *d)
{
    toml_datum_t ptab;
    bool         tr_valid  = false;
    htri_t       ret_value = FAIL;

    FUNC_ENTER_PACKAGE

    if (!params)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "params must not be NULL");
    if (!key || !*key)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "key must be a non-empty string");
    if (!params[0])
        HGOTO_DONE(false);

    if (H5Z__toml_parse_params(params, tr, &ptab) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "failed to parse parameter string");
    tr_valid = true;

    *d = toml_seek(ptab, key);
    if (d->type == TOML_UNKNOWN)
        HGOTO_DONE(false);

    ret_value = true;

done:
    if (tr_valid && ret_value <= 0)
        toml_free(*tr);
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5Zconfig_has_key
 *
 * Purpose:     Check whether a key exists in a TOML parameter string.
 *
 * Return:      > 0 present, 0 absent, < 0 error.
 *
 * Since:  3.0.0
 *-------------------------------------------------------------------------
 */
htri_t
H5Zconfig_has_key(const char *params, const char *key)
{
    toml_result_t tr;
    toml_datum_t  d;
    htri_t        ret_value = FAIL;

    /* No API lock: filter set_config callbacks call this while
     * H5Pappend_filter holds it */
    FUNC_ENTER_API_NOINIT_NOLOCK

    ret_value = H5Z__config_get_datum(params, key, &tr, &d);

    if (ret_value > 0)
        toml_free(tr);
    FUNC_LEAVE_API_NOINIT_NOLOCK(ret_value)
}

/* Internal version of H5Zconfig_get_int() */
htri_t
H5Z__config_get_int(const char *params, const char *key, int64_t *out)
{
    toml_result_t tr;
    toml_datum_t  d;
    bool          tr_valid = false;
    htri_t        found;
    htri_t        ret_value = FAIL;

    FUNC_ENTER_PACKAGE

    if (!out)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "out must not be NULL");
    if ((found = H5Z__config_get_datum(params, key, &tr, &d)) < 0)
        HGOTO_DONE(FAIL);
    if (found == 0)
        HGOTO_DONE(false);
    tr_valid = true;

    if (d.type != TOML_INT64)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "type mismatch: key '%s' is not a TOML integer", key);
    *out      = d.u.int64;
    ret_value = true;

done:
    if (tr_valid)
        toml_free(tr);
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5Z__no_params_set_config
 *
 * Purpose:     Shared set_config implementation for filters that accept no
 *              user parameters (shuffle, fletcher32, nbit).  Sets
 *              *cd_nelmts = 0 and rejects any non-empty params.
 *
 * Return:      Non-negative on success / Negative on failure
 *-------------------------------------------------------------------------
 */
herr_t
H5Z__no_params_set_config(const char *params, unsigned H5_ATTR_UNUSED *flags, size_t *cd_nelmts,
                          unsigned H5_ATTR_UNUSED cd_values[], size_t H5_ATTR_UNUSED cd_values_size)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    *cd_nelmts = 0;

    if (params && *params != '\0')
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "filter takes no parameters");

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5Zconfig_get_int
 *
 * Purpose:     Look up a key and return its TOML integer value (int64_t).
 *
 * Return:      > 0 found and converted, 0 not found, < 0 error (includes
 *              type mismatch and parse error).
 *
 * Since:  3.0.0
 *-------------------------------------------------------------------------
 */
htri_t
H5Zconfig_get_int(const char *params, const char *key, int64_t *out)
{
    htri_t ret_value = FAIL;

    /* No API lock: see comment on H5Zconfig_has_key. */
    FUNC_ENTER_API_NOINIT_NOLOCK

    ret_value = H5Z__config_get_int(params, key, out);

    FUNC_LEAVE_API_NOINIT_NOLOCK(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5Zconfig_get_double
 *
 * Purpose:     Look up a key and return its TOML float value (double).
 *              inf and nan are rejected with H5E_BADVALUE.
 *
 * Return:      > 0 found and converted, 0 not found, < 0 error.
 *
 * Since:  3.0.0
 *-------------------------------------------------------------------------
 */
htri_t
H5Zconfig_get_double(const char *params, const char *key, double *out)
{
    toml_result_t tr;
    toml_datum_t  d;
    bool          tr_valid = false;
    htri_t        found;
    htri_t        ret_value = FAIL;

    /* No API lock: see comment on H5Zconfig_has_key. */
    FUNC_ENTER_API_NOINIT_NOLOCK

    if (!out)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "out must not be NULL");
    if ((found = H5Z__config_get_datum(params, key, &tr, &d)) < 0)
        HGOTO_DONE(FAIL);
    if (found == 0)
        HGOTO_DONE(false);
    tr_valid = true;

    if (d.type != TOML_FP64)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "type mismatch: key '%s' is not a TOML float", key);
    if (H5Z__fp64_is_inf_or_nan(d.u.fp64))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                    "inf/nan float values are not supported for filter parameters (key '%s')", key);
    *out      = d.u.fp64;
    ret_value = true;

done:
    if (tr_valid)
        toml_free(tr);
    FUNC_LEAVE_API_NOINIT_NOLOCK(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5Zconfig_get_bool
 *
 * Purpose:     Look up a key and return its TOML boolean value (hbool_t).
 *
 * Return:      > 0 found, 0 not found, < 0 error.
 *
 * Since:  3.0.0
 *-------------------------------------------------------------------------
 */
htri_t
H5Zconfig_get_bool(const char *params, const char *key, bool *out)
{
    toml_result_t tr;
    toml_datum_t  d;
    bool          tr_valid = false;
    htri_t        found;
    htri_t        ret_value = FAIL;

    /* No API lock: see comment on H5Zconfig_has_key. */
    FUNC_ENTER_API_NOINIT_NOLOCK

    if (!out)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "out must not be NULL");
    if ((found = H5Z__config_get_datum(params, key, &tr, &d)) < 0)
        HGOTO_DONE(FAIL);
    if (found == 0)
        HGOTO_DONE(false);
    tr_valid = true;

    if (d.type != TOML_BOOLEAN)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "type mismatch: key '%s' is not a TOML boolean", key);
    *out      = d.u.boolean ? true : false;
    ret_value = true;

done:
    if (tr_valid)
        toml_free(tr);
    FUNC_LEAVE_API_NOINIT_NOLOCK(ret_value)
}

/* Internal version of H5Zconfig_get_str() */
htri_t
H5Z__config_get_str(const char *params, const char *key, char *buf, size_t *buf_size)
{
    toml_result_t tr;
    toml_datum_t  d;
    bool          tr_valid = false;
    htri_t        found;
    size_t        vlen;
    htri_t        ret_value = FAIL;

    FUNC_ENTER_PACKAGE

    if ((found = H5Z__config_get_datum(params, key, &tr, &d)) < 0)
        HGOTO_DONE(FAIL);
    if (found == 0)
        HGOTO_DONE(false);
    tr_valid = true;

    if (d.type != TOML_STRING)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                    "type mismatch: key '%s' is not a TOML string (value must be quoted)", key);

    vlen = (size_t)d.u.str.len;

    {
        size_t cap;

        if (buf && !buf_size)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "buf_size must not be NULL when buf is non-NULL");

        cap = buf_size ? *buf_size : 0;

        if (buf_size)
            *buf_size = vlen;

        if (buf) {
            if (cap > vlen) {
                memcpy(buf, d.u.s, vlen + 1);
            }
            else {
                if (cap > 0) {
                    memcpy(buf, d.u.s, cap - 1);
                    buf[cap - 1] = '\0';
                }
                HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL,
                            "output buffer too small for string value of key '%s'", key);
            }
        }
    }

    ret_value = true;

done:
    if (tr_valid)
        toml_free(tr);
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5Zconfig_get_str
 *
 * Purpose:     Look up a key and return its TOML string value, unquoted.
 *              With BUF NULL, only *BUF_SIZE is set (length excluding NUL).
 *
 * Return:      > 0 found, 0 not found, < 0 error.
 *
 * Since:  3.0.0
 *-------------------------------------------------------------------------
 */
htri_t
H5Zconfig_get_str(const char *params, const char *key, char *buf, size_t *buf_size)
{
    htri_t ret_value = FAIL;

    /* No API lock: see comment on H5Zconfig_has_key. */
    FUNC_ENTER_API_NOINIT_NOLOCK

    ret_value = H5Z__config_get_str(params, key, buf, buf_size);

    FUNC_LEAVE_API_NOINIT_NOLOCK(ret_value)
}
