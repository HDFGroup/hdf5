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
 * H5Zconfig.c - TOML parameter string parser for the string-based filter
 *               configuration API.
 *
 * Uses the vendored tomlc17 library for all TOML parsing.
 *
 * Public typed accessor functions:
 *   H5Zconfig_has_key    - key presence check
 *   H5Zconfig_get_int    - TOML integer  -> int64_t
 *   H5Zconfig_get_double - TOML float    -> double
 *   H5Zconfig_get_bool   - TOML boolean  -> bool
 *   H5Zconfig_get_str    - TOML string   -> char buffer
 *
 * Package-internal:
 *   H5Z__config_validate_keys - validate all keys in params against a
 *                               known-key list; called by built-in filter
 *                               set_config callbacks.
 */

#define H5Z_FRIEND /* suppress error on H5Zpkg.h include */

#include "H5Zmodule.h"

#include "H5private.h"   /* Generic Functions   */
#include "H5Eprivate.h"  /* Error handling      */
#include "H5MMprivate.h" /* Memory management   */
#include "H5Zpkg.h"      /* Filter internals    */

/* Renames every public tomlc17 symbol to an H5Z__toml_c17_-prefixed name so
 * a statically-linked libhdf5.a cannot collide with an application's own
 * copy of tomlc17 (see h5_toml_prefix.h for the full rationale). Must be
 * included before tomlc17.h so every call site below picks up the renamed
 * declarations. */
#include "tomlc17/h5_toml_prefix.h"
#include "tomlc17/tomlc17.h"

/* Same treatment for the vendored Ryu shortest-round-trip float formatter
 * (see ryu/h5_ryu_prefix.h); must precede ryu/ryu.h for the same reason. */
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

/* Append two source characters (e.g. a backslash escape) to the output
 * buffer, or skip both if there is insufficient space. */
static inline void
H5Z__copy_chars2(char *out, size_t cap, size_t *pos, const char **p)
{
    if (*pos + 2 < cap) {
        out[(*pos)++] = (*p)[0];
        out[(*pos)++] = (*p)[1];
    }
    (*p) += 2;
}

/* Is v an inf or a nan?
 *
 * Decided on the bit pattern rather than with isnan()/isinf(): a fast-math
 * build -- Intel icx's -fp-model=fast (its default at -O2 and above), or
 * gcc/clang -ffast-math or -ffinite-math-only -- may assume no operand is
 * ever inf or nan and fold both classifiers to 0, letting through exactly
 * the values the caller means to reject.  An exponent field of all ones is
 * inf (zero mantissa) or nan (nonzero mantissa). */
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

/* TOML's spelling of a non-finite double ("nan", "inf", "-inf"), or NULL if V
 * is finite.  Ryu spells these "NaN", "Infinity" and "-Infinity", none of
 * which any TOML scanner accepts.  Discriminates on the bit pattern for the
 * same fast-math reason as H5Z__fp64_is_inf_or_nan() above. */
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
 * H5Z__format_double_canonical - format VAL into BUF (capacity BUFSIZE) as
 * the shortest decimal literal that round-trips back to the identical IEEE
 * 754 double, and that a TOML scanner types as a float rather than an
 * integer.
 *
 * The digits come from the vendored Ryu library (src/ryu), which computes the
 * shortest round-tripping decimal directly from the bit pattern.  HDF5 does
 * not attempt that conversion itself: getting it right in every rounding
 * corner is the whole subject of a PLDI paper, and the result is written into
 * the file format, where a wrong digit is permanent.  See
 * <https://github.com/HDFGroup/hdf5/issues/6153>.
 *
 * Ryu emits scientific notation unconditionally and with no padding -- 3.0 is
 * "3E0", 0.1 is "1E-1" -- which would make the canonical form of an ordinary
 * compression level read `rate = 3.5E0`.  Its output is therefore taken apart
 * into the digit string and decimal exponent it really represents and laid
 * out again below, using printf("%g")'s rule: fixed-point while the exponent
 * stays in human-scale range, scientific outside it.  That re-layout only
 * moves the decimal point -- it never rounds, drops or adds a significant
 * digit -- so Ryu's round-trip guarantee carries over verbatim, and no
 * strtod() readback is needed to confirm it.
 *
 * Nothing here is locale-sensitive.  Ryu writes ASCII digits directly instead
 * of going through snprintf(), so LC_NUMERIC cannot substitute ',' for the
 * '.' that TOML requires.
 *
 * TOML types a bare "8" as an integer, not a float, which would fail the
 * typed getters; the fixed-point branch appends ".0" whenever the digits run
 * out at the decimal point, and the scientific branch always carries an
 * exponent, so every result lexes as a float.
 *
 * Returns the length written (excluding the NUL), or -1 if BUFSIZE was too
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

    /* Ryu's output is "[-]d[.ddd]E[-]ddd", and d2s_buffered_n() returns its
     * length without NUL-terminating it.  Split it back into sign, the
     * significant digits with the '.' removed, and the exponent of the
     * leading digit. */
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
        /* Fixed-point.  e10 < ndigits keeps the decimal point inside the
         * digits, so no zero padding is ever needed on the right. */
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
        /* Scientific, spelled the way printf("%e") would: a signed exponent
         * of at least two digits. */
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
 * H5Z__rewrite_hexfloats - return a copy of `src` with every C99 hex-float
 * literal (e.g. "0x1.8p+1", "-0x1p-1") replaced by an equivalent decimal
 * string, via H5Z__format_double_canonical() above.
 *
 * Lets callers write `%a` hex-float literals for exact float encoding
 * without requiring hex-float support in the vendored tomlc17 scanner.
 *
 * Caller frees the returned buffer with H5MM_xfree(); NULL on allocation
 * failure.
 */
static char *
H5Z__rewrite_hexfloats(const char *src)
{
    const char *p   = src;
    size_t      len = strlen(src);
    size_t      cap;
    char       *out;
    size_t      pos = 0;

    /* Worst case: a short token whose value needs the full 17 significant
     * digits, e.g. "0x1p99" (6 chars) -> "6.338253001141147e+29" (21), just
     * under 4x; 8x leaves ample headroom.
     * Guard against size_t overflow in the multiplication; callers normally
     * cap input at H5Z_CONFIG_STRING_MAX, but enforce the bound here too so
     * this static helper is safe for any future caller. */
    if (len > (SIZE_MAX - 1) / 8)
        return NULL;
    cap = len * 8 + 1;
    out = (char *)H5MM_malloc(cap);

    if (!out)
        return NULL;

    while (*p) {
        /* Skip TOML double-quoted strings verbatim (honors backslash
         * escapes so \" does not end the string early). */
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

        /* Skip TOML single-quoted (literal) strings verbatim (no escapes). */
        if (*p == '\'') {
            H5Z__copy_char(out, cap, &pos, &p);
            while (*p && *p != '\'')
                H5Z__copy_char(out, cap, &pos, &p);
            if (*p == '\'')
                H5Z__copy_char(out, cap, &pos, &p);
            continue;
        }

        /* Skip TOML comments (# to end of line) verbatim. */
        if (*p == '#') {
            while (*p && *p != '\n')
                H5Z__copy_char(out, cap, &pos, &p);
            continue;
        }

        /* Detect optional sign followed by "0x" or "0X" */
        const char *tok_start = p;
        if (*p == '+' || *p == '-')
            p++;

        if (p[0] == '0' && (p[1] == 'x' || p[1] == 'X')) {
            const char *q = p + 2;
            while (isxdigit((unsigned char)*q) || *q == '_')
                q++;
            /* Is this a hex-float? Requires '.' or 'p'/'P' after hex digits */
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
                    /* q now points past the hex-float token; convert it */
                    size_t tok_len = (size_t)(q - tok_start);
                    char   tmp[64];
                    if (tok_len < sizeof(tmp)) {
                        memcpy(tmp, tok_start, tok_len);
                        tmp[tok_len] = '\0';
                        char  *end;
                        double val = strtod(tmp, &end);
                        if (end == tmp + tok_len) {
                            /* Decimal form of the hex-float literal; see
                             * H5Z__format_double_canonical() above.
                             * localeconv() returns thread-shared static
                             * storage inside that call; HDF5_ENABLE_THREADSAFE
                             * builds serialize concurrent setlocale() calls
                             * via the global library lock, making this
                             * safe. */
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

        /* Not a hex-float: copy one character verbatim */
        p = tok_start;
        H5Z__copy_char(out, cap, &pos, &p);
    }
    out[pos] = '\0';
    return out;
}

/*
 * H5Z__toml_wrap - allocate a NUL-terminated TOML document that wraps the
 * inline-table content in params.  Returns a heap buffer that the caller
 * must free with H5MM_xfree().
 *
 * The wrapper key "__p__" is chosen specifically because TOML bare keys cannot
 * contain two consecutive underscores - "__p__" is therefore impossible to
 * produce in user-supplied content.  User keys become *values* inside the
 * inline table, so a user key named "__p__" would still not collide.
 *
 * Accepts both bare content and an already-braced inline table:
 *   "level = 6"        ->  "__p__ = {level = 6}"
 *   "{level = 6}"      ->  "__p__ = {level = 6}"
 *   "{ level = 6 }"   ->  "__p__ = {level = 6}"  (whitespace trimmed inside braces)
 */
static char *
H5Z__toml_wrap(const char *params)
{
    const char *p = params ? params : "";
    const char *e;
    size_t      content_len;
    size_t      wlen;
    char       *buf;

    /* skip leading whitespace */
    while (*p == ' ' || *p == '\t')
        p++;

    /* strip optional outer { } */
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

    wlen = content_len + 12; /* "__p__ = {" (9) + content + "}" (1) + NUL */
    buf  = (char *)H5MM_malloc(wlen);
    if (buf)
        snprintf(buf, wlen, "__p__ = {%.*s}", (int)content_len, p);
    return buf;
}

/*-------------------------------------------------------------------------
 * Function:    H5Z_canonicalize_params
 *
 * Purpose:     Return a heap copy of PARAMS in the canonical form persisted
 *              on disk (filter pipeline v3): optional outer braces and
 *              surrounding whitespace stripped, and C99 hex-float literals
 *              rewritten to the shortest bit-exact decimal (up to
 *              DBL_DECIMAL_DIG == 17 significant digits; see
 *              H5Z__format_double_canonical()).  Both normalizations
 *              exist because the stored bytes must be valid TOML v1.0.0 --
 *              pure-reimplementation readers (e.g. jHDF, pyfive) parse the
 *              object header directly with a stock TOML parser, for which a
 *              braced or hex-float payload is a hard error.  See
 *              RFC-HDFG-2026-001 sec:pline-v3.
 *
 *              Everything else -- interior spacing, quote style, key case,
 *              key order -- is preserved byte-for-byte; values are never
 *              re-serialized, so no decimal-precision rounding is
 *              introduced.
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

    /* Rewrite hex-float literals first.  The rewriter skips quoted strings
     * and comments, so it cannot disturb the brace characters examined
     * below, nor rewrite hex-float-looking text inside a string value. */
    if (NULL == (expanded = H5Z__rewrite_hexfloats(params)))
        HGOTO_DONE(NULL);

    /* Strip optional outer braces, then trim whitespace at both ends. */
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

/*
 * H5Z__count_table_keys - recursively count leaf key=value assignments in a
 * parsed TOML table, including keys nested inside inline tables / dotted-key
 * groups (a nested table itself is not counted, only its own leaves are).
 * Used to enforce H5Z_CONFIG_MAX_PARAMS.
 */
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
 * H5Z__toml_parse_params - wrap params as a TOML document and parse it.
 *
 * On success: *tr_out holds a valid result; *ptab_out is the inline-table
 *             datum.  The caller MUST call toml_free(*tr_out) when done.
 * On failure: *tr_out is zeroed; an HDF5 error is pushed; returns FAIL.
 */
static htri_t
H5Z__toml_parse_params(const char *params, toml_result_t *tr_out, toml_datum_t *ptab_out)
{
    char  *expanded  = NULL;
    char  *wrapped   = NULL;
    htri_t ret_value = true;

    FUNC_ENTER_PACKAGE

    /* Defence-in-depth length check: callers SHOULD enforce
     * H5Z_CONFIG_STRING_MAX, but enforce it here too so that the
     * downstream `len * 8` worst-case allocation in H5Z__rewrite_hexfloats
     * cannot overflow size_t. */
    if (params && strlen(params) > H5Z_CONFIG_STRING_MAX)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                    "filter parameter string exceeds H5Z_CONFIG_STRING_MAX (%d bytes)",
                    H5Z_CONFIG_STRING_MAX);

    /* Replace hex-float literals (e.g. 0x1.8p+1) with decimal equivalents
     * so the tomlc17 scanner, which does not support C99 hex-float syntax,
     * can parse the resulting string without modification. */
    if (params && *params) {
        if (NULL == (expanded = H5Z__rewrite_hexfloats(params)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "out of memory rewriting hex-float literals");
        params = expanded;
    }

    if (NULL == (wrapped = H5Z__toml_wrap(params)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "out of memory for TOML wrapper buffer");

    *tr_out = toml_parse(wrapped, (int)strlen(wrapped));

    if (!tr_out->ok) {
        /* Guard: errmsg must be a fixed-size char array so sizeof gives the
         * full capacity.  If a future tomlc17 update changes it to a pointer,
         * sizeof would equal sizeof(char *) (<=8) and the memcpy below would
         * silently truncate.  The assert catches that at compile time. */
        _Static_assert(sizeof(tr_out->errmsg) > sizeof(void *),
                       "toml_result_t.errmsg must be a fixed-size char array, not a pointer");
        /* Copy errmsg before toml_free invalidates it */
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

/*
 * H5Z__validate_table_keys - walk one TOML table level, checking each leaf
 * key against known_keys.  When a value is a nested table, recurse with the
 * dotted prefix accumulated so far.  Returns FAIL on first unknown leaf.
 */
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

        /* Nested inline table: recurse rather than checking against known_keys.
         * (The dotted-key form "compressor.name = ..." also parses to a nested
         * table, so this is the single canonical traversal path.) */
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
 * H5Z__config_validate_keys - verify every leaf key in params is in known_keys.
 * Nested inline tables are walked recursively so the dotted-key form
 * ("compressor.name") and the inline-table form ("compressor = {name = ...}")
 * are validated identically.
 * Package-internal; called by built-in filter set_config callbacks.
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

/*-------------------------------------------------------------------------
 * H5Z__config_get_datum - shared lookup core for all public accessors.
 *
 * Parses params, looks up key, and returns the raw toml_datum_t.
 *
 * Return:  > 0  key found;   *tr is valid - caller MUST toml_free(*tr)
 *           0   key absent;  helper already called toml_free(*tr)
 *         < 0   error;       error pushed; helper already cleaned up *tr
 *-------------------------------------------------------------------------
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

    /* toml_seek traverses dotted paths (e.g. "compressor.name") through
     * nested inline tables; for flat keys it behaves identically to toml_get.
     * Both surface forms - "compressor = {name = ...}" and the dotted form
     * "compressor.name = ..." - parse to the same nested layout, so callers
     * see one canonical lookup convention. */
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

    /* No API lock: this is a pure parser over caller-provided buffers and
     * may be called from inside an H5Z_set_config_func_t callback that is
     * already running under the API lock held by H5Pappend_filter. */
    FUNC_ENTER_API_NOINIT_NOLOCK

    ret_value = H5Z__config_get_datum(params, key, &tr, &d);

    if (ret_value > 0)
        toml_free(tr);
    FUNC_LEAVE_API_NOINIT_NOLOCK(ret_value)
}

/*-------------------------------------------------------------------------
 * H5Z__config_get_int - package-level integer lookup (no API lock).
 * Called by set_config callbacks which already run inside an API context.
 *-------------------------------------------------------------------------
 */
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

/*-------------------------------------------------------------------------
 * H5Z__config_get_str - package-level string lookup (no API lock).
 * Called by set_config callbacks which already run inside an API context.
 *-------------------------------------------------------------------------
 */
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

        /* Reject ambiguous (buf != NULL, buf_size == NULL): the caller's
         * buffer size is unknown, and an unbounded memcpy would be unsafe. */
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
 * Purpose:     Look up a key and return its TOML string value (decoded,
 *              without surrounding quotes).  Only TOML_STRING values are
 *              accepted; bare integers, floats, and booleans are type errors.
 *
 *              Size-query pattern:
 *                - buf == NULL: only *buf_size is set to the required length
 *                  (excluding NUL), returns > 0.
 *                - buf != NULL, *buf_size > 0: copies up to *buf_size - 1
 *                  bytes plus NUL; always sets *buf_size to required length.
 *                  Returns H5E_OVERFLOW if the buffer is too small.
 *                - buf != NULL, buf_size == NULL: rejected with H5E_BADVALUE
 *                  (the function has no way to know the buffer capacity).
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
