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

#include <locale.h> /* localeconv() for decimal_point */

#include "H5private.h"   /* Generic Functions   */
#include "H5Eprivate.h"  /* Error handling      */
#include "H5MMprivate.h" /* Memory management   */
#include "H5Zpkg.h"      /* Filter internals    */

#include "tomlc17/tomlc17.h"

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

/*
 * H5Z__rewrite_hexfloats - return a copy of `src` with every C99 hex-float
 * literal (e.g. "0x1.8p+1", "-0x1p-1") replaced by an equivalent decimal
 * string.  Uses %.17g which guarantees IEEE 754 double round-trip fidelity.
 *
 * This pre-processing step lets callers produce parameter strings with `%a`
 * for exact float encoding without
 * requiring changes to the vendored tomlc17 scanner, which does not support
 * hex-float syntax natively.
 *
 * Returns a heap-allocated NUL-terminated string; caller frees with
 * H5MM_xfree().  Returns NULL on allocation failure.
 */
static char *
H5Z__rewrite_hexfloats(const char *src)
{
    const char *p   = src;
    size_t      len = strlen(src);
    size_t      cap;
    char       *out;
    size_t      pos = 0;

    /* Worst case: every 3-char token "0x1" expands to ~24 chars "%.17e" -> 8x.
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
        /* ----------------------------------------------------------------
         * Skip TOML double-quoted strings verbatim - do not rewrite content
         * inside "...".  Honours backslash escapes so that \" does not end
         * the string prematurely.
         * ---------------------------------------------------------------- */
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

        /* ----------------------------------------------------------------
         * Skip TOML single-quoted (literal) strings verbatim - no escapes.
         * ---------------------------------------------------------------- */
        if (*p == '\'') {
            H5Z__copy_char(out, cap, &pos, &p);
            while (*p && *p != '\'')
                H5Z__copy_char(out, cap, &pos, &p);
            if (*p == '\'')
                H5Z__copy_char(out, cap, &pos, &p);
            continue;
        }

        /* ----------------------------------------------------------------
         * Skip TOML comments (# to end of line) verbatim.
         * ---------------------------------------------------------------- */
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
            /* Scan hex digits */
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
                            /* Emit decimal equivalent as a TOML float.
                             * Use %e (scientific notation) which always
                             * contains a decimal point and exponent, so
                             * tomlc17 parses it as TOML_FP64 not TOML_INTEGER.
                             * 17 significant digits guarantee IEEE 754
                             * double round-trip fidelity (C99 DBL_DECIMAL_DIG). */
                            char dec[32];
                            int  n = snprintf(dec, sizeof(dec), "%.17e", val);
                            /* LC_NUMERIC may replace '.' with the locale decimal
                             * separator (e.g. ',' in de_DE).  TOML requires '.'.
                             * Use localeconv() to find the actual separator rather
                             * than hardcoding ','. */
                            if (n > 0 && n < (int)sizeof(dec)) {
                                /* localeconv() returns a pointer to thread-shared
                                 * static storage; we read decimal_point[0] exactly
                                 * once.  In HDF5_ENABLE_THREADSAFE builds the global
                                 * library lock serializes concurrent setlocale() calls,
                                 * so this is safe. */
                                const char *locale_sep = localeconv()->decimal_point; /* always non-NULL */
                                if (locale_sep[0] != '.' && locale_sep[0] != '\0') {
                                    char *dp;
                                    for (dp = dec; *dp; dp++) {
                                        if (*dp == locale_sep[0]) {
                                            *dp = '.';
                                            break; /* one separator per number */
                                        }
                                    }
                                }
                                if (pos + (size_t)n < cap) {
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
 * Since:  2.2.0
 *-------------------------------------------------------------------------
 */
htri_t
H5Zconfig_has_key(const char *params, const char *key)
{
    toml_result_t tr;
    toml_datum_t  d;
    htri_t        ret_value = FAIL;

    /* The API lock is recursive, so this is safe to call from inside an
     * H5Z_set_config_func_t callback that is already running under the API
     * lock held by H5Pappend_filter. */
    FUNC_ENTER_API_NOINIT

    ret_value = H5Z__config_get_datum(params, key, &tr, &d);

    if (ret_value > 0)
        toml_free(tr);
    FUNC_LEAVE_API_NOINIT(ret_value)
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
 * H5Z__no_params_set_config - shared set_config implementation for
 * filters that accept no user parameters (e.g. shuffle, fletcher32).
 * Sets *cd_nelmts = 0 and rejects any non-empty params.
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
 * Since:  2.2.0
 *-------------------------------------------------------------------------
 */
htri_t
H5Zconfig_get_int(const char *params, const char *key, int64_t *out)
{
    htri_t ret_value = FAIL;

    /* See comment on H5Zconfig_has_key about recursive re-entry. */
    FUNC_ENTER_API_NOINIT

    ret_value = H5Z__config_get_int(params, key, out);

    FUNC_LEAVE_API_NOINIT(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5Zconfig_get_double
 *
 * Purpose:     Look up a key and return its TOML float value (double).
 *              inf and nan are rejected with H5E_BADVALUE.
 *
 * Return:      > 0 found and converted, 0 not found, < 0 error.
 *
 * Since:  2.2.0
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

    /* See comment on H5Zconfig_has_key about recursive re-entry. */
    FUNC_ENTER_API_NOINIT

    if (!out)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "out must not be NULL");
    if ((found = H5Z__config_get_datum(params, key, &tr, &d)) < 0)
        HGOTO_DONE(FAIL);
    if (found == 0)
        HGOTO_DONE(false);
    tr_valid = true;

    if (d.type != TOML_FP64)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "type mismatch: key '%s' is not a TOML float", key);
    if (isnan(d.u.fp64) || isinf(d.u.fp64))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                    "inf/nan float values are not supported for filter parameters (key '%s')", key);
    *out      = d.u.fp64;
    ret_value = true;

done:
    if (tr_valid)
        toml_free(tr);
    FUNC_LEAVE_API_NOINIT(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5Zconfig_get_bool
 *
 * Purpose:     Look up a key and return its TOML boolean value (hbool_t).
 *
 * Return:      > 0 found, 0 not found, < 0 error.
 *
 * Since:  2.2.0
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

    /* See comment on H5Zconfig_has_key about recursive re-entry. */
    FUNC_ENTER_API_NOINIT

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
    FUNC_LEAVE_API_NOINIT(ret_value)
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

        /* Reject ambiguous (buf != NULL, buf_size == NULL): we have no way to
         * know the caller's buffer size, and an unbounded memcpy is unsafe. */
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
 * Since:  2.2.0
 *-------------------------------------------------------------------------
 */
htri_t
H5Zconfig_get_str(const char *params, const char *key, char *buf, size_t *buf_size)
{
    htri_t ret_value = FAIL;

    /* See comment on H5Zconfig_has_key about recursive re-entry. */
    FUNC_ENTER_API_NOINIT

    ret_value = H5Z__config_get_str(params, key, buf, buf_size);

    FUNC_LEAVE_API_NOINIT(ret_value)
}
