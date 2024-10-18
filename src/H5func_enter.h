/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 * H5func_enter.h
 *
 * FUNC_ENTER/LEAVE macros used in HDF5 library API calls.
 *
 * The macros in this file set up error handling, handle thread-safety
 * lock acquisition, and perform other per-API-call housekeeping tasks.
 *-------------------------------------------------------------------------
 */

#ifndef H5func_enter_H
#define H5func_enter_H

/* ----------------------------------------------------------------------------
 * Macros to check function names for appropriate form. Called from the
 * FUNC_ENTER macros, below.
 *
 * - public:         H5X(Y)foo()
 *
 * - private:        H5X(Y)_foo()
 *
 * - package/static: H5X(Y)__foo()
 *
 * NOTE: These will generate somewhat cryptic errors when APIs with incorrectly
 *       formed names are called at runtime. The H5_CHECK_FUNCTION_NAME()
 *       macro will emit a helpful error message if it detects badness.
 * ----------------------------------------------------------------------------
 */

/* Macro to check if a function call is a public HDF5 API call
 *
 * `S' is the name of a function which is being tested to check if it's
 * an API function.
 *
 *  UNDERSCORE CHECKS:
 *      - Underscore at positions 2 or 3 (0-indexed string). Handles
 *        H5_ and H5X_.
 *      - Underscore at position 4 if position 3 is uppercase or a digit.
 *        Handles H5XY_.
 */
#define H5_IS_PUBLIC(S)                                                                                      \
    ('_' != ((const char *)S)[2]                        /* underscore at position 2     */                   \
     && '_' != ((const char *)S)[3]                     /* underscore at position 3     */                   \
     && !(                                              /* NOT              */                               \
          ((const char *)S)[4]                          /* pos 4 exists     */                               \
          && (isupper((int)S[3]) || isdigit((int)S[3])) /* pos 3 dig | uc   */                               \
          && '_' == ((const char *)S)[4]                /* pos 4 underscore */                               \
          ))

/* Macro to check if a function call is a library private HDF5 API call
 *
 * These have the form H5X(Y)_foo() <--- single underscore
 *
 * `S' is the name of a function which is being tested to check if it's a private function
 */
#define H5_IS_PRIVATE(S)                                                                                     \
    (((isdigit((int)S[1]) || isupper((int)S[1])) && '_' == S[2] && islower((int)S[3])) ||                    \
     ((isdigit((int)S[2]) || isupper((int)S[2])) && '_' == S[3] && islower((int)S[4])) ||                    \
     ((isdigit((int)S[3]) || isupper((int)S[3])) && '_' == S[4] && islower((int)S[5])))

/* Macro to check if a function call is a package internal HDF5 API call
 *
 * These have the form H5X(Y)__foo() <--- two underscores
 *
 * `S' is the name of a function which is being tested to check if it's a package function
 */
#define H5_IS_PKG(S)                                                                                         \
    (((isdigit((int)S[1]) || isupper((int)S[1])) && '_' == S[2] && '_' == S[3] && islower((int)S[4])) ||     \
     ((isdigit((int)S[2]) || isupper((int)S[2])) && '_' == S[3] && '_' == S[4] && islower((int)S[5])) ||     \
     ((isdigit((int)S[3]) || isupper((int)S[3])) && '_' == S[4] && '_' == S[5] && islower((int)S[6])))

/* Macro to check that an API call is using a correctly formed name.
 *
 * The name checks only occur in debug builds to avoid performance degradation and only take
 * place once per API call per library initialization.
 */
#ifndef NDEBUG
#define H5_CHECK_FUNCTION_NAME(asrt)                                                                         \
    {                                                                                                        \
        static bool func_check = false;                                                                      \
                                                                                                             \
        if (H5_UNLIKELY(!func_check)) {                                                                      \
            /* Check function naming status */                                                               \
            assert(asrt &&                                                                                   \
                   "Function naming conventions are incorrect (see H5func_enter.h)"                          \
                   "(this is usually due to an incorrect number of underscores in the function name)");      \
                                                                                                             \
            /* Don't check again */                                                                          \
            func_check = true;                                                                               \
        }                                                                                                    \
    }
#else
#define H5_CHECK_FUNCTION_NAME(asrt)
#endif

/* ----------------------------------------------------------------------------
 * Macros that things up upon entering an HDF5 API call
 *
 * These are all of the form `H5_API_SETUP_<thing>`
 * ----------------------------------------------------------------------------
 */

/* clang-format off */

/* Error setup for FUNC_ENTER macros that report an error */
#define H5_API_SETUP_ERROR_HANDLING                                                                          \
    bool err_occurred = false;

/* Entry setup for public API call variables */
#define H5_API_SETUP_PUBLIC_API_VARS                                                                         \
    H5CANCEL_DECL /* thread cancellation */

/* clang-format on */

/* Macro to initialize the library, if some other package hasn't already done that */
#define H5_API_SETUP_INIT_LIBRARY(err)                                                                       \
    do {                                                                                                     \
        if (H5_UNLIKELY(!H5_INIT_GLOBAL && !H5_TERM_GLOBAL)) {                                               \
            if (H5_UNLIKELY(H5_init_library() < 0))                                                          \
                HGOTO_ERROR(H5E_FUNC, H5E_CANTINIT, err, "library initialization failed");                   \
        }                                                                                                    \
    } while (0)

/* Macro to push the API context */
#define H5_API_SETUP_PUSH_CONTEXT(err)                                                                       \
    /* The library context variable can't go in this macro since then it might                               \
     * be uninitialized if the library init fails.                                                           \
     */                                                                                                      \
    do {                                                                                                     \
        if (H5_UNLIKELY(H5CX_push() < 0))                                                                    \
            HGOTO_ERROR(H5E_FUNC, H5E_CANTSET, err, "can't set API context");                                \
        else                                                                                                 \
            api_ctx_pushed = true;                                                                           \
    } while (0)

/* ----------------------------------------------------------------------------
 * HDF5 API call entry macros
 *
 * These are all of the form `FUNC_ENTER_*`. Every HDF5 API call will begin
 * with one of these macros immediately after the variable declarations.
 * ----------------------------------------------------------------------------
 */

/* Use this macro for all "normal" public API functions */
#define FUNC_ENTER_API(err)                                                                                  \
    {                                                                                                        \
        {                                                                                                    \
            bool api_ctx_pushed = false;                                                                     \
                                                                                                             \
            H5_CHECK_FUNCTION_NAME(H5_IS_PUBLIC(__func__));                                                  \
                                                                                                             \
            H5_API_SETUP_PUBLIC_API_VARS                                                                     \
            H5_API_SETUP_ERROR_HANDLING                                                                      \
            H5_API_LOCK                                                                                      \
            H5_API_SETUP_INIT_LIBRARY(err);                                                                  \
            H5_API_SETUP_PUSH_CONTEXT(err);                                                                  \
                                                                                                             \
            /* Clear thread error stack entering public functions */                                         \
            H5E_clear_stack();                                                                               \
            {

/*
 * Use this macro for public API functions that shouldn't clear the error stack
 * like H5Eprint and H5Ewalk.
 */
#define FUNC_ENTER_API_NOCLEAR(err)                                                                          \
    {                                                                                                        \
        {                                                                                                    \
            bool api_ctx_pushed = false;                                                                     \
                                                                                                             \
            H5_CHECK_FUNCTION_NAME(H5_IS_PUBLIC(__func__));                                                  \
                                                                                                             \
            H5_API_SETUP_PUBLIC_API_VARS                                                                     \
            H5_API_SETUP_ERROR_HANDLING                                                                      \
            H5_API_LOCK                                                                                      \
            H5_API_SETUP_INIT_LIBRARY(err);                                                                  \
            H5_API_SETUP_PUSH_CONTEXT(err);                                                                  \
            {

/*
 * Use this macro for public API functions that shouldn't perform _any_
 * initialization of the library or an interface, just perform tracing, etc.
 * Examples are: H5is_library_threadsafe, H5VLretrieve_lib_state, etc.
 */
#define FUNC_ENTER_API_NOINIT                                                                                \
    {                                                                                                        \
        {                                                                                                    \
            {                                                                                                \
                H5_CHECK_FUNCTION_NAME(H5_IS_PUBLIC(__func__));                                              \
                                                                                                             \
                H5_API_SETUP_PUBLIC_API_VARS                                                                 \
                H5_API_SETUP_ERROR_HANDLING                                                                  \
                H5_API_LOCK                                                                                  \
                {

/*
 * Use this macro for public API functions that shouldn't perform _any_
 * initialization of the library or an interface or push themselves on the
 * function stack, just perform tracing, etc. Examples are: H5close,
 * H5check_version, etc.
 */
#define FUNC_ENTER_API_NOINIT_NOERR_NOFS                                                                     \
    {                                                                                                        \
        {                                                                                                    \
            {                                                                                                \
                {                                                                                            \
                    H5_CHECK_FUNCTION_NAME(H5_IS_PUBLIC(__func__));                                          \
                                                                                                             \
                    H5_API_SETUP_PUBLIC_API_VARS                                                             \
                    H5_API_LOCK                                                                              \
                    {

/*
 * Use this macro for public API functions that should only perform
 * initialization of the library or an interface, but not push any state (API
 * context, function name, etc.). Examples are: H5open.
 */
#define FUNC_ENTER_API_NOPUSH(err)                                                                           \
    {                                                                                                        \
        {                                                                                                    \
            {                                                                                                \
                {                                                                                            \
                    {                                                                                        \
                        H5_CHECK_FUNCTION_NAME(H5_IS_PUBLIC(__func__));                                      \
                                                                                                             \
                        H5_API_SETUP_PUBLIC_API_VARS                                                         \
                        H5_API_SETUP_ERROR_HANDLING                                                          \
                        H5_API_LOCK                                                                          \
                        H5_API_SETUP_INIT_LIBRARY(err);                                                      \
                        {

/*
 * Use this macro for public API functions that shouldn't perform _any_
 * initialization of the library or an interface, or push themselves on the
 * function stack, or perform tracing, etc.  This macro _only_ sanity checks
 * the API name itself. Examples are: H5TSmutex_acquire
 */
#define FUNC_ENTER_API_NAMECHECK_ONLY                                                                        \
    {                                                                                                        \
        {                                                                                                    \
            {                                                                                                \
                {                                                                                            \
                    {                                                                                        \
                        {                                                                                    \
                            H5_CHECK_FUNCTION_NAME(H5_IS_PUBLIC(__func__));                                  \
                            {

/* Use this macro for all "normal" non-API functions */
#define FUNC_ENTER_NOAPI(err)                                                                                \
    {                                                                                                        \
        H5_CHECK_FUNCTION_NAME(H5_IS_PRIVATE(__func__));                                                     \
                                                                                                             \
        H5_API_SETUP_ERROR_HANDLING                                                                          \
        {

/* Use this macro for all non-API functions, which propagate errors, but don't issue them */
#define FUNC_ENTER_NOAPI_NOERR                                                                               \
    {                                                                                                        \
        {

/*
 * Use this macro for non-API functions which are called during library
 * shutdown, since we don't want to re-initialize the library.
 */
#define FUNC_ENTER_NOAPI_NOINIT                                                                              \
    {                                                                                                        \
        H5_CHECK_FUNCTION_NAME(H5_IS_PRIVATE(__func__));                                                     \
                                                                                                             \
        H5_API_SETUP_ERROR_HANDLING                                                                          \
        {

/*
 * Use this macro for non-API functions that propagate, but do not isssue
 * errors, which are called during library shutdown, since we don't want to
 * re-initialize the library.
 */
#define FUNC_ENTER_NOAPI_NOINIT_NOERR                                                                        \
    {                                                                                                        \
        {

/*
 * Use this macro for non-API functions that shouldn't perform _any_
 * initialization of the library or an interface, or push themselves on the
 * function stack, or perform tracing, etc.  This macro _only_ sanity checks
 * the API name itself. Examples are private routines in the H5TS package.
 */
#define FUNC_ENTER_NOAPI_NAMECHECK_ONLY {

/* Use the following two macros as replacements for the FUNC_ENTER_NOAPI
 * and FUNC_ENTER_NOAPI_NOINIT macros when the function needs to set
 * up a metadata tag.
 */
#define FUNC_ENTER_NOAPI_TAG(tag, err)                                                                       \
    {                                                                                                        \
        haddr_t prev_tag = HADDR_UNDEF;                                                                      \
                                                                                                             \
        H5_CHECK_FUNCTION_NAME(H5_IS_PRIVATE(__func__));                                                     \
                                                                                                             \
        H5_API_SETUP_ERROR_HANDLING                                                                          \
                                                                                                             \
        H5AC_tag(tag, &prev_tag);                                                                            \
        {

#define FUNC_ENTER_NOAPI_NOINIT_TAG(tag)                                                                     \
    {                                                                                                        \
        haddr_t prev_tag = HADDR_UNDEF;                                                                      \
                                                                                                             \
        H5_CHECK_FUNCTION_NAME(H5_IS_PRIVATE(__func__));                                                     \
                                                                                                             \
        H5_API_SETUP_ERROR_HANDLING                                                                          \
                                                                                                             \
        H5AC_tag(tag, &prev_tag);                                                                            \
        {

/* Use this macro for all "normal" package-level and static functions */
#define FUNC_ENTER_PACKAGE                                                                                   \
    {                                                                                                        \
        H5_CHECK_FUNCTION_NAME(H5_IS_PKG(__func__));                                                         \
                                                                                                             \
        H5_API_SETUP_ERROR_HANDLING                                                                          \
        {

/* Use this macro for package-level and static functions which propagate
 * errors, but don't issue them
 */
#define FUNC_ENTER_PACKAGE_NOERR                                                                             \
    {                                                                                                        \
        H5_CHECK_FUNCTION_NAME(H5_IS_PKG(__func__));                                                         \
        {

/* Use the following macro as replacement for the FUNC_ENTER_PACKAGE
 * macro when the function needs to set up a metadata tag.
 */
#define FUNC_ENTER_PACKAGE_TAG(tag)                                                                          \
    {                                                                                                        \
        haddr_t prev_tag = HADDR_UNDEF;                                                                      \
                                                                                                             \
        H5_CHECK_FUNCTION_NAME(H5_IS_PKG(__func__));                                                         \
                                                                                                             \
        H5_API_SETUP_ERROR_HANDLING                                                                          \
                                                                                                             \
        H5AC_tag(tag, &prev_tag);                                                                            \
        {

/* Use this macro for package-level and static functions which propagate
 * errors, but don't issue them and that shouldn't push their name on
 * the function stack
 */
#define FUNC_ENTER_PACKAGE_NOERR_NOFS                                                                        \
    {                                                                                                        \
        H5_CHECK_FUNCTION_NAME(H5_IS_PKG(__func__));                                                         \
        {

/*
 * Use this macro for package-level or static functions that shouldn't perform
 * _any_ initialization of the library or an interface, or push themselves on
 * the function stack, or perform tracing, etc.  This macro _only_ sanity
 * checks the API name itself. Examples are static routines in the H5TS package.
 */
#define FUNC_ENTER_PACKAGE_NAMECHECK_ONLY                                                                    \
    {                                                                                                        \
        H5_CHECK_FUNCTION_NAME(H5_IS_PKG(__func__));

/* ----------------------------------------------------------------------------
 * HDF5 API call leave macros
 *
 * These are all of the form `FUNC_LEAVE_*` and need to match the FUNC_ENTER
 * macro used when entering. The PACKAGE FUNC_ENTER macros use the NOAPI
 * FUNC_LEAVE macros.
 *
 * The FUNC_LEAVE macro will be the last statement in an HDF5 API call.
 *
 * NOTE: All FUNC_LEAVE macros begin with a semicolon to prevent compiler
 *       warnings from having the done: target right before the scope-closing
 *       bracket. Labels at the end of compound statements is a C23 extension.
 * ----------------------------------------------------------------------------
 */

#define FUNC_LEAVE_API(ret_value)                                                                            \
    ;                                                                                                        \
    } /* end scope from end of FUNC_ENTER */                                                                 \
    if (H5_LIKELY(api_ctx_pushed)) {                                                                         \
        (void)H5CX_pop(true);                                                                                \
        api_ctx_pushed = false;                                                                              \
    }                                                                                                        \
    if (H5_UNLIKELY(err_occurred))                                                                           \
        (void)H5E_dump_api_stack();                                                                          \
    H5_API_UNLOCK                                                                                            \
    return (ret_value);                                                                                      \
    }                                                                                                        \
    } /* end scope from beginning of FUNC_ENTER */

/* Use this macro to match the FUNC_ENTER_API_NOINIT macro */
#define FUNC_LEAVE_API_NOINIT(ret_value)                                                                     \
    ;                                                                                                        \
    } /* end scope from end of FUNC_ENTER */                                                                 \
    if (H5_UNLIKELY(err_occurred))                                                                           \
        (void)H5E_dump_api_stack();                                                                          \
    H5_API_UNLOCK                                                                                            \
    return (ret_value);                                                                                      \
    }                                                                                                        \
    }                                                                                                        \
    } /* end scope from beginning of FUNC_ENTER */

/* Use this macro to match the FUNC_ENTER_API_NOINIT_NOERR_NOFS macro */
#define FUNC_LEAVE_API_NOFS(ret_value)                                                                       \
    ;                                                                                                        \
    } /* end scope from end of FUNC_ENTER */                                                                 \
    H5_API_UNLOCK                                                                                            \
    return (ret_value);                                                                                      \
    }                                                                                                        \
    }                                                                                                        \
    }                                                                                                        \
    } /* end scope from beginning of FUNC_ENTER */

/* Use this macro to match the FUNC_ENTER_API_NOPUSH macro */
#define FUNC_LEAVE_API_NOPUSH(ret_value)                                                                     \
    ;                                                                                                        \
    } /* end scope from end of FUNC_ENTER */                                                                 \
    if (H5_UNLIKELY(err_occurred))                                                                           \
        (void)H5E_dump_api_stack();                                                                          \
    H5_API_UNLOCK                                                                                            \
    return (ret_value);                                                                                      \
    }                                                                                                        \
    }                                                                                                        \
    }                                                                                                        \
    }                                                                                                        \
    } /* end scope from beginning of FUNC_ENTER */

/* Use this macro to match the FUNC_ENTER_API_NAMECHECK_ONLY macro */
#define FUNC_LEAVE_API_NAMECHECK_ONLY(ret_value)                                                             \
    ;                                                                                                        \
    } /* end scope from end of FUNC_ENTER */                                                                 \
    return (ret_value);                                                                                      \
    }                                                                                                        \
    }                                                                                                        \
    }                                                                                                        \
    }                                                                                                        \
    }                                                                                                        \
    } /* end scope from beginning of FUNC_ENTER */

/* Use this macro to match NOAPI and PACKAGE macros which return a value */
#define FUNC_LEAVE_NOAPI(ret_value)                                                                          \
    ;                                                                                                        \
    } /* end scope from end of FUNC_ENTER */                                                                 \
    return (ret_value);                                                                                      \
    } /* end scope from beginning of FUNC_ENTER */

/* Use this macro to match NOAPI and PACKAGE macros which do not return a value */
#define FUNC_LEAVE_NOAPI_VOID                                                                                \
    ;                                                                                                        \
    } /* end scope from end of FUNC_ENTER */                                                                 \
    return;                                                                                                  \
    } /* end scope from beginning of FUNC_ENTER */

/* Use these macros to match the FUNC_ENTER_NOAPI_NAMECHECK_ONLY macro */
#define FUNC_LEAVE_NOAPI_NAMECHECK_ONLY(ret_value)                                                           \
    return (ret_value);                                                                                      \
    } /* end scope from beginning of FUNC_ENTER */
#define FUNC_LEAVE_NOAPI_VOID_NAMECHECK_ONLY                                                                 \
    return;                                                                                                  \
    } /* end scope from beginning of FUNC_ENTER */

/* Use this macro when exiting a function that sets up a metadata tag */
#define FUNC_LEAVE_NOAPI_TAG(ret_value)                                                                      \
    ;                                                                                                        \
    } /* end scope from end of FUNC_ENTER */                                                                 \
    H5AC_tag(prev_tag, NULL);                                                                                \
    return (ret_value);                                                                                      \
    } /* end scope from beginning of FUNC_ENTER */

/* ----------------------------------------------------------------------------
 * Metadata cache tagging macros (when FUNC_ENTER_*TAG macros are insufficient)
 *
 * Make sure to use HGOTO_ERROR_TAG and HGOTO_DONE_TAG between these macros!
 * ----------------------------------------------------------------------------
 */

/* Macro to begin tagging */
#define H5_BEGIN_TAG(tag)                                                                                    \
    {                                                                                                        \
        haddr_t prv_tag = HADDR_UNDEF;                                                                       \
        H5AC_tag(tag, &prv_tag);

/* Macro to end tagging */
#define H5_END_TAG                                                                                           \
    H5AC_tag(prv_tag, NULL);                                                                                 \
    }

#endif /* H5func_enter_H */
