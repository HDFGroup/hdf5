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
 *              outer braces and surrounding whitespace removed, everything
 *              else unchanged.
 *
 * Return:      Success:    Heap-allocated NUL-terminated string, freed by
 *                          the caller with H5MM_xfree().
 *              Failure:    NULL
 *-------------------------------------------------------------------------
 */
char *
H5Z_canonicalize_params(const char *params)
{
    const char *p;
    const char *e;
    size_t      len;
    char       *ret_value = NULL;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if (params == NULL)
        HGOTO_DONE(NULL);

    p = params;
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
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Z_canonicalize_params() */

/* Count leaf keys, including those in nested tables */
static H5_ATTR_PURE size_t
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
    char  *wrapped   = NULL;
    htri_t ret_value = true;

    FUNC_ENTER_PACKAGE

    if (params && strlen(params) > H5Z_CONFIG_STRING_MAX)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                    "filter parameter string exceeds H5Z_CONFIG_STRING_MAX (%d bytes)",
                    H5Z_CONFIG_STRING_MAX);

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

/* Internal version of H5Zconfig_has_key() */
htri_t
H5Z__config_has_key(const char *params, const char *key)
{
    toml_result_t tr;
    toml_datum_t  d;
    htri_t        ret_value = FAIL;

    FUNC_ENTER_PACKAGE

    if ((ret_value = H5Z__config_get_datum(params, key, &tr, &d)) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "failed to look up key");
    if (ret_value > 0)
        toml_free(tr);

done:
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
    htri_t ret_value = FAIL;

    FUNC_ENTER_API(FAIL)

    if ((ret_value = H5Z__config_has_key(params, key)) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTGET, FAIL, "unable to look up filter parameter key");

done:
    FUNC_LEAVE_API(ret_value)
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
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "failed to look up key");
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

    FUNC_ENTER_API(FAIL)

    if ((ret_value = H5Z__config_get_int(params, key, out)) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTGET, FAIL, "unable to get integer filter parameter");

done:
    FUNC_LEAVE_API(ret_value)
}

/* Internal version of H5Zconfig_get_double() */
htri_t
H5Z__config_get_double(const char *params, const char *key, double *out)
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
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "failed to look up key");
    if (found == 0)
        HGOTO_DONE(false);
    tr_valid = true;

    if (d.type != TOML_FP64)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "type mismatch: key '%s' is not a TOML float", key);
    if (!isfinite(d.u.fp64))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                    "inf/nan float values are not supported for filter parameters (key '%s')", key);
    *out      = d.u.fp64;
    ret_value = true;

done:
    if (tr_valid)
        toml_free(tr);
    FUNC_LEAVE_NOAPI(ret_value)
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
    htri_t ret_value = FAIL;

    FUNC_ENTER_API(FAIL)

    if ((ret_value = H5Z__config_get_double(params, key, out)) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTGET, FAIL, "unable to get float filter parameter");

done:
    FUNC_LEAVE_API(ret_value)
}

/* Internal version of H5Zconfig_get_bool() */
htri_t
H5Z__config_get_bool(const char *params, const char *key, bool *out)
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
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "failed to look up key");
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
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5Zconfig_get_bool
 *
 * Purpose:     Look up a key and return its TOML boolean value (bool).
 *
 * Return:      > 0 found, 0 not found, < 0 error.
 *
 * Since:  3.0.0
 *-------------------------------------------------------------------------
 */
htri_t
H5Zconfig_get_bool(const char *params, const char *key, bool *out)
{
    htri_t ret_value = FAIL;

    FUNC_ENTER_API(FAIL)

    if ((ret_value = H5Z__config_get_bool(params, key, out)) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTGET, FAIL, "unable to get boolean filter parameter");

done:
    FUNC_LEAVE_API(ret_value)
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
    size_t        cap;
    htri_t        ret_value = FAIL;

    FUNC_ENTER_PACKAGE

    if (buf && !buf_size)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "buf_size must not be NULL when buf is non-NULL");
    if ((found = H5Z__config_get_datum(params, key, &tr, &d)) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "failed to look up key");
    if (found == 0)
        HGOTO_DONE(false);
    tr_valid = true;

    if (d.type != TOML_STRING)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                    "type mismatch: key '%s' is not a TOML string (value must be quoted)", key);

    vlen = (size_t)d.u.str.len;
    cap  = buf_size ? *buf_size : 0;
    if (buf_size)
        *buf_size = vlen;

    if (buf) {
        if (cap > vlen)
            memcpy(buf, d.u.s, vlen + 1);
        else {
            if (cap > 0) {
                memcpy(buf, d.u.s, cap - 1);
                buf[cap - 1] = '\0';
            }
            HGOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, "output buffer too small for string value of key '%s'",
                        key);
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

    FUNC_ENTER_API(FAIL)

    if ((ret_value = H5Z__config_get_str(params, key, buf, buf_size)) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTGET, FAIL, "unable to get string filter parameter");

done:
    FUNC_LEAVE_API(ret_value)
}
