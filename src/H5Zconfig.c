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
#include "H5FLprivate.h" /* Free lists          */
#include "H5MMprivate.h" /* Memory management   */
#include "H5Zpkg.h"      /* Filter internals    */

#include <locale.h>

/* The prefix header renames the vendored symbols and must come first */
#include "tomlc17/h5_toml_prefix.h"
#include "tomlc17/tomlc17.h"

/*
 * Find the body of PARAMS: surrounding spaces and tabs and one pair of outer
 * braces removed.  Returns false if PARAMS opens with '{' but does not close
 * with '}'.
 */
static bool
H5Z__params_body(const char *params, const char **begin, const char **end)
{
    const char *p = params ? params : "";
    const char *e;

    while (*p == ' ' || *p == '\t')
        p++;
    e = p + strlen(p);
    while (e > p && (*(e - 1) == ' ' || *(e - 1) == '\t'))
        e--;

    if (*p == '{') {
        if (e - p < 2 || *(e - 1) != '}')
            return false;
        p++;
        e--;
    }

    *begin = p;
    *end   = e;
    return true;
}

/*
 * Wrap the body [BEGIN, END) as the TOML document "__p__ = {...\n}".  User
 * keys live inside the inline table, so they cannot collide with "__p__".
 * The closing brace goes on its own line so that neither a '#' comment nor a
 * '}' in the body can end the table early without a parse error.  Caller
 * frees with H5MM_xfree().
 */
static char *
H5Z__toml_wrap(const char *begin, const char *end)
{
    size_t content_len = (size_t)(end - begin);
    size_t wlen        = content_len + sizeof("__p__ = {\n}");
    char  *buf;

    if (NULL != (buf = (char *)H5MM_malloc(wlen)))
        snprintf(buf, wlen, "__p__ = {%.*s\n}", (int)content_len, begin);
    return buf;
}

/*-------------------------------------------------------------------------
 * Function:    H5Z_canonicalize_params
 *
 * Purpose:     Return a copy of PARAMS with outer braces and surrounding
 *              whitespace removed, everything else unchanged.  This is the
 *              form stored with the filter.  A string with an unbalanced
 *              opening brace is only trimmed; H5Z_config_parse() rejects it.
 *
 * Return:      Success:    Heap-allocated NUL-terminated string, freed by
 *                          the caller with H5MM_xfree().  NULL PARAMS
 *                          gives NULL without an error.
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

    FUNC_ENTER_NOAPI(NULL)

    if (params == NULL)
        HGOTO_DONE(NULL);

    if (!H5Z__params_body(params, &p, &e)) {
        p = params;
        e = params + strlen(params);
    }
    while (p < e && (*p == ' ' || *p == '\t'))
        p++;
    while (e > p && (*(e - 1) == ' ' || *(e - 1) == '\t'))
        e--;

    len = (size_t)(e - p);
    if (NULL == (ret_value = (char *)H5MM_malloc(len + 1)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate filter parameter string");
    H5MM_memcpy(ret_value, p, len);
    ret_value[len] = '\0';

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Z_canonicalize_params() */

/*
 * Check a parsed value and count its leaf parameters into *NLEAVES.
 *
 * H5Zconfig_*() lookups split the key on '.' and compare NUL-terminated
 * strings, so a quoted key containing '.' or NUL could be validated but
 * never read; such keys are rejected.  String values may not contain NUL.
 * An empty table counts as a leaf, so H5Zconfig_validate_keys() checks its
 * key.  An array counts as one leaf; its elements are checked but not
 * counted.
 */
static herr_t
H5Z__check_datum(toml_datum_t d, size_t *nleaves)
{
    size_t  ignored = 0;
    int32_t i;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    if (d.type == TOML_TABLE) {
        if (d.u.tab.size == 0)
            (*nleaves)++;
        for (i = 0; i < d.u.tab.size; i++) {
            const char *k    = d.u.tab.key[i];
            size_t      klen = (size_t)d.u.tab.len[i];

            if (memchr(k, '\0', klen) || memchr(k, '.', klen))
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                            "filter parameter key '%s' contains '.' or NUL; use a nested table instead", k);
            if (H5Z__check_datum(d.u.tab.value[i], nleaves) < 0)
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid value for filter parameter '%s'", k);
        }
    }
    else {
        (*nleaves)++;
        if (d.type == TOML_STRING && memchr(d.u.str.ptr, '\0', (size_t)d.u.str.len))
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "filter parameter string value contains NUL");
        if (d.type == TOML_ARRAY)
            for (i = 0; i < d.u.arr.size; i++)
                if (H5Z__check_datum(d.u.arr.elem[i], &ignored) < 0)
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid filter parameter array element");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*
 * toml_parse() in the "C" numeric locale for the calling thread.  tomlc17
 * converts floats with strtod(), which follows LC_NUMERIC; since R261109 it
 * substitutes the decimal point from localeconv() first (cktan/tomlc17#57).
 * Parsing in the "C" locale makes that substitution a no-op, so the result
 * does not depend on the global locale, which another thread can change.
 * Platforms with neither uselocale() nor per-thread locales rely on the
 * upstream substitution alone.
 */
static herr_t
H5Z__toml_parse_c_locale(const char *src, int len, toml_result_t *tr)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

#if defined(H5_HAVE_WIN32_API)
    {
        const char *cur;
        char       *saved = NULL;
        int         prev_cfg;

        if (-1 == (prev_cfg = _configthreadlocale(_ENABLE_PER_THREAD_LOCALE)))
            HGOTO_ERROR(H5E_INTERNAL, H5E_CANTSET, FAIL, "can't enable a per-thread locale");
        if (NULL == (cur = setlocale(LC_NUMERIC, NULL)) || NULL == (saved = H5MM_strdup(cur))) {
            _configthreadlocale(prev_cfg);
            HGOTO_ERROR(H5E_INTERNAL, H5E_CANTGET, FAIL, "can't save the numeric locale");
        }
        if (NULL == setlocale(LC_NUMERIC, "C")) {
            H5MM_xfree(saved);
            _configthreadlocale(prev_cfg);
            HGOTO_ERROR(H5E_INTERNAL, H5E_CANTSET, FAIL, "can't switch to the C numeric locale");
        }

        *tr = toml_parse(src, len);

        setlocale(LC_NUMERIC, saved);
        H5MM_xfree(saved);
        _configthreadlocale(prev_cfg);
    }
#elif defined(H5_HAVE_USELOCALE)
    {
        locale_t c_loc;
        locale_t prev;

        if ((locale_t)0 == (c_loc = newlocale(LC_NUMERIC_MASK, "C", (locale_t)0)))
            HGOTO_ERROR(H5E_INTERNAL, H5E_CANTCREATE, FAIL, "can't create the C numeric locale");
        if ((locale_t)0 == (prev = uselocale(c_loc))) {
            freelocale(c_loc);
            HGOTO_ERROR(H5E_INTERNAL, H5E_CANTSET, FAIL, "can't switch to the C numeric locale");
        }

        *tr = toml_parse(src, len);

        uselocale(prev);
        freelocale(c_loc);
    }
#else
    *tr = toml_parse(src, len);
#endif

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*
 * Parse PARAMS.  On success *ptab_out is the parameter table and the caller
 * must toml_free(*tr_out); on failure *tr_out is zeroed.
 */
static herr_t
H5Z__toml_parse_params(const char *params, toml_result_t *tr_out, toml_datum_t *ptab_out)
{
    const char *begin;
    const char *end;
    char       *wrapped   = NULL;
    size_t      nleaves   = 0;
    bool        have_tr   = false;
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    memset(tr_out, 0, sizeof(*tr_out));

    if (params && strlen(params) > H5Z_CONFIG_STRING_MAX)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                    "filter parameter string exceeds H5Z_CONFIG_STRING_MAX (%d bytes)",
                    H5Z_CONFIG_STRING_MAX);

    if (!H5Z__params_body(params, &begin, &end))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                    "filter parameter string opens with '{' but does not end with '}'");

    if (NULL == (wrapped = H5Z__toml_wrap(begin, end)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "out of memory for TOML wrapper buffer");

    if (H5Z__toml_parse_c_locale(wrapped, (int)strlen(wrapped), tr_out) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_CANTINIT, FAIL, "can't parse filter parameter string");
    have_tr = true;

    if (!tr_out->ok) {
        char   errbuf[sizeof(tr_out->errmsg)];
        size_t n;

        /* errbuf is sized with sizeof, so errmsg must be an array */
        HDcompile_assert(sizeof(tr_out->errmsg) > sizeof(void *));

        memcpy(errbuf, tr_out->errmsg, sizeof(errbuf));
        errbuf[sizeof(errbuf) - 1] = '\0';
        n                          = strlen(errbuf);
        while (n > 0 && (errbuf[n - 1] == '\n' || errbuf[n - 1] == '\r'))
            errbuf[--n] = '\0';
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "TOML parse error in filter parameter string: %s", errbuf);
    }

    /* The wrapper makes "__p__" the only top-level key */
    *ptab_out = toml_get(tr_out->toptab, "__p__");
    if (tr_out->toptab.u.tab.size != 1 || ptab_out->type != TOML_TABLE)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                    "malformed filter parameter string (not a single TOML inline table)");

    if (H5Z__check_datum(*ptab_out, &nleaves) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid filter parameter string");

    if (nleaves > H5Z_CONFIG_MAX_PARAMS)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                    "filter parameter string exceeds H5Z_CONFIG_MAX_PARAMS (%d key-value pairs)",
                    H5Z_CONFIG_MAX_PARAMS);

done:
    if (ret_value < 0 && have_tr) {
        toml_free(*tr_out);
        memset(tr_out, 0, sizeof(*tr_out));
    }
    H5MM_xfree(wrapped);
    FUNC_LEAVE_NOAPI(ret_value)
}

/*
 * Check each leaf key's dotted path against KNOWN_KEYS.  A nested table with
 * no keys is a leaf.
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

        if (v.type == TOML_TABLE && v.u.tab.size > 0) {
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

/* A parsed filter parameter string */
struct H5Z_config_t {
    toml_result_t tr;     /* Parse result; owns the memory behind tab */
    toml_datum_t  tab;    /* The parameter table */
    bool          parsed; /* false for an empty string (no keys, nothing to free) */
};

/* Declare a free list to manage H5Z_config_t objects */
H5FL_DEFINE_STATIC(H5Z_config_t);

/*-------------------------------------------------------------------------
 * Function:    H5Z_config_parse
 *
 * Purpose:     Parse PARAMS once.  NULL or "" yields an empty config.
 *
 * Return:      Success:    Config, freed with H5Z_config_close()
 *              Failure:    NULL
 *-------------------------------------------------------------------------
 */
H5Z_config_t *
H5Z_config_parse(const char *params)
{
    H5Z_config_t *config    = NULL;
    H5Z_config_t *ret_value = NULL;

    FUNC_ENTER_NOAPI(NULL)

    if (NULL == (config = H5FL_CALLOC(H5Z_config_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate filter config");

    if (params && *params) {
        if (H5Z__toml_parse_params(params, &config->tr, &config->tab) < 0)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "failed to parse filter parameter string");
        config->parsed = true;
    }

    ret_value = config;

done:
    if (!ret_value && config)
        config = H5FL_FREE(H5Z_config_t, config);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5Z_config_parse() */

/*-------------------------------------------------------------------------
 * Function:    H5Z_config_close
 *
 * Purpose:     Free a config from H5Z_config_parse().  NULL is a no-op.
 *
 * Return:      SUCCEED
 *-------------------------------------------------------------------------
 */
herr_t
H5Z_config_close(H5Z_config_t *config)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if (config) {
        if (config->parsed)
            toml_free(config->tr);
        config = H5FL_FREE(H5Z_config_t, config);
    }

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5Z_config_close() */

/*
 * Fail if CONFIG contains a key not in KNOWN_KEYS (a NULL-terminated list).
 * Nested keys are matched by dotted path, so "a = {b = 1}" and "a.b = 1" are
 * equivalent.
 */
herr_t
H5Z__config_validate_keys(const H5Z_config_t *config, const char *const *known_keys)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    if (!config)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "config must not be NULL");
    if (!known_keys)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "known_keys must not be NULL");

    if (config->parsed && H5Z__validate_table_keys(config->tab, NULL, known_keys) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unknown parameter key in filter configuration");

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/* Look up KEY (a dotted path is allowed) in CONFIG */
static htri_t
H5Z__config_get_datum(const H5Z_config_t *config, const char *key, toml_datum_t *d)
{
    htri_t ret_value = true;

    FUNC_ENTER_PACKAGE

    if (!config)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "config must not be NULL");
    if (!key || !*key)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "key must be a non-empty string");
    if (strlen(key) >= H5Z_CONFIG_MAX_KEY_PATH)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "key is longer than %d characters",
                    H5Z_CONFIG_MAX_KEY_PATH - 1);
    if (!config->parsed)
        HGOTO_DONE(false);

    *d = toml_seek(config->tab, key);
    if (d->type == TOML_UNKNOWN)
        HGOTO_DONE(false);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/* Internal version of H5Zconfig_has_key() */
htri_t
H5Z__config_has_key(const H5Z_config_t *config, const char *key)
{
    toml_datum_t d;
    htri_t       ret_value = FAIL;

    FUNC_ENTER_PACKAGE

    if ((ret_value = H5Z__config_get_datum(config, key, &d)) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "failed to look up key");

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/* Internal version of H5Zconfig_get_int() */
htri_t
H5Z__config_get_int(const H5Z_config_t *config, const char *key, int64_t *out)
{
    toml_datum_t d;
    htri_t       ret_value = FAIL;

    FUNC_ENTER_PACKAGE

    if (!out)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "out must not be NULL");
    if ((ret_value = H5Z__config_get_datum(config, key, &d)) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "failed to look up key");
    if (ret_value == 0)
        HGOTO_DONE(false);

    if (d.type != TOML_INT64)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "type mismatch: key '%s' is not a TOML integer", key);
    *out = d.u.int64;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/* Internal version of H5Zconfig_get_double() */
htri_t
H5Z__config_get_double(const H5Z_config_t *config, const char *key, double *out)
{
    toml_datum_t d;
    htri_t       ret_value = FAIL;

    FUNC_ENTER_PACKAGE

    if (!out)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "out must not be NULL");
    if ((ret_value = H5Z__config_get_datum(config, key, &d)) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "failed to look up key");
    if (ret_value == 0)
        HGOTO_DONE(false);

    if (d.type != TOML_FP64)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "type mismatch: key '%s' is not a TOML float", key);
    if (!isfinite(d.u.fp64))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                    "inf/nan float values are not supported for filter parameters (key '%s')", key);
    *out = d.u.fp64;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/* Internal version of H5Zconfig_get_bool() */
htri_t
H5Z__config_get_bool(const H5Z_config_t *config, const char *key, bool *out)
{
    toml_datum_t d;
    htri_t       ret_value = FAIL;

    FUNC_ENTER_PACKAGE

    if (!out)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "out must not be NULL");
    if ((ret_value = H5Z__config_get_datum(config, key, &d)) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "failed to look up key");
    if (ret_value == 0)
        HGOTO_DONE(false);

    if (d.type != TOML_BOOLEAN)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "type mismatch: key '%s' is not a TOML boolean", key);
    *out = d.u.boolean ? true : false;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/* Internal version of H5Zconfig_get_str() */
htri_t
H5Z__config_get_str(const H5Z_config_t *config, const char *key, char *buf, size_t *buf_size)
{
    toml_datum_t d;
    size_t       vlen;
    size_t       cap;
    htri_t       ret_value = FAIL;

    FUNC_ENTER_PACKAGE

    if (buf && !buf_size)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "buf_size must not be NULL when buf is non-NULL");
    if ((ret_value = H5Z__config_get_datum(config, key, &d)) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "failed to look up key");
    if (ret_value == 0)
        HGOTO_DONE(false);

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

done:
    FUNC_LEAVE_NOAPI(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5Zconfig_parse
 *
 * Purpose:     Parse a filter parameter string.  NULL or "" yields a config
 *              with no keys.
 *
 * Return:      Success:    Config, freed with H5Zconfig_close()
 *              Failure:    NULL
 *
 * Since:  3.0.0
 *-------------------------------------------------------------------------
 */
H5Z_config_t *
H5Zconfig_parse(const char *params)
{
    H5Z_config_t *ret_value = NULL;

    FUNC_ENTER_API(NULL)

    if (NULL == (ret_value = H5Z_config_parse(params)))
        HGOTO_ERROR(H5E_PLINE, H5E_CANTINIT, NULL, "unable to parse filter parameter string");

done:
    FUNC_LEAVE_API(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5Zconfig_close
 *
 * Purpose:     Free a config from H5Zconfig_parse().  NULL is a no-op.
 *
 * Return:      Non-negative on success, negative on failure
 *
 * Since:  3.0.0
 *-------------------------------------------------------------------------
 */
herr_t
H5Zconfig_close(H5Z_config_t *config)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if (H5Z_config_close(config) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTFREE, FAIL, "unable to free filter config");

done:
    FUNC_LEAVE_API(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5Zconfig_validate_keys
 *
 * Purpose:     Fail if CONFIG contains a key not in KNOWN_KEYS, a
 *              NULL-terminated list of dotted key paths.
 *
 * Return:      Non-negative on success, negative on failure
 *
 * Since:  3.0.0
 *-------------------------------------------------------------------------
 */
herr_t
H5Zconfig_validate_keys(const H5Z_config_t *config, const char *const known_keys[])
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)

    if (H5Z__config_validate_keys(config, known_keys) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_BADVALUE, FAIL, "filter parameter validation failed");

done:
    FUNC_LEAVE_API(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5Zconfig_has_key
 *
 * Purpose:     Check whether a key exists in a filter config.
 *
 * Return:      > 0 present, 0 absent, < 0 error.
 *
 * Since:  3.0.0
 *-------------------------------------------------------------------------
 */
htri_t
H5Zconfig_has_key(const H5Z_config_t *config, const char *key)
{
    htri_t ret_value = FAIL;

    FUNC_ENTER_API(FAIL)

    if ((ret_value = H5Z__config_has_key(config, key)) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTGET, FAIL, "unable to look up filter parameter key");

done:
    FUNC_LEAVE_API(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5Zconfig_get_int
 *
 * Purpose:     Look up a key and return its TOML integer value (int64_t).
 *
 * Return:      > 0 found, 0 not found, < 0 error (including type mismatch).
 *
 * Since:  3.0.0
 *-------------------------------------------------------------------------
 */
htri_t
H5Zconfig_get_int(const H5Z_config_t *config, const char *key, int64_t *out)
{
    htri_t ret_value = FAIL;

    FUNC_ENTER_API(FAIL)

    if ((ret_value = H5Z__config_get_int(config, key, out)) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTGET, FAIL, "unable to get integer filter parameter");

done:
    FUNC_LEAVE_API(ret_value)
}

/*-------------------------------------------------------------------------
 * Function:    H5Zconfig_get_double
 *
 * Purpose:     Look up a key and return its TOML float value (double).
 *              inf and nan are rejected with H5E_BADVALUE.
 *
 * Return:      > 0 found, 0 not found, < 0 error.
 *
 * Since:  3.0.0
 *-------------------------------------------------------------------------
 */
htri_t
H5Zconfig_get_double(const H5Z_config_t *config, const char *key, double *out)
{
    htri_t ret_value = FAIL;

    FUNC_ENTER_API(FAIL)

    if ((ret_value = H5Z__config_get_double(config, key, out)) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTGET, FAIL, "unable to get float filter parameter");

done:
    FUNC_LEAVE_API(ret_value)
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
H5Zconfig_get_bool(const H5Z_config_t *config, const char *key, bool *out)
{
    htri_t ret_value = FAIL;

    FUNC_ENTER_API(FAIL)

    if ((ret_value = H5Z__config_get_bool(config, key, out)) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTGET, FAIL, "unable to get boolean filter parameter");

done:
    FUNC_LEAVE_API(ret_value)
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
H5Zconfig_get_str(const H5Z_config_t *config, const char *key, char *buf, size_t *buf_size)
{
    htri_t ret_value = FAIL;

    FUNC_ENTER_API(FAIL)

    if ((ret_value = H5Z__config_get_str(config, key, buf, buf_size)) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTGET, FAIL, "unable to get string filter parameter");

done:
    FUNC_LEAVE_API(ret_value)
}
