/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 *  Header file for error values, etc.
 */
#ifndef H5TOOLS_ERROR_H_
#define H5TOOLS_ERROR_H_

#include "H5Epublic.h"
#include "H5Eprivate.h" /* Error handling       */

/* tools-HDF5 Error variables */
H5TOOLS_DLLVAR hid_t H5tools_ERR_STACK_g;
H5TOOLS_DLLVAR hid_t H5tools_DBG_ERR_STACK_g;
H5TOOLS_DLLVAR hid_t H5tools_ERR_CLS_g;
H5TOOLS_DLLVAR hid_t H5tools_DBG_ERR_CLS_g;
H5TOOLS_DLLVAR hid_t H5E_tools_g;
H5TOOLS_DLLVAR hid_t H5E_tools_min_id_g;
H5TOOLS_DLLVAR hid_t H5E_tools_min_info_id_g;
H5TOOLS_DLLVAR hid_t H5E_tools_min_dbg_id_g;

/* Use FUNC to safely handle variations of C99 __func__ keyword handling */
#ifdef H5_HAVE_C99_FUNC
#define FUNC __func__
#elif defined(H5_HAVE_FUNCTION)
#define FUNC __FUNCTION__
#else
#error "We need __func__ or __FUNCTION__ to test function names!"
#endif

/*
 * H5TOOLS_INIT_ERROR macro, used to initialize error reporting.
 */
#define H5TOOLS_INIT_ERROR()                                                                            \
do {                                                                                                    \
    char lib_str[256];                                                                                  \
                                                                                                        \
    /* Initialize library version string for error class */                                             \
    HDsnprintf(lib_str, sizeof(lib_str), "%d.%d.%d", H5_VERS_MAJOR, H5_VERS_MINOR, H5_VERS_RELEASE);    \
                                                                                                        \
    /* Create new HDF5 error stack for the tools to use */                                              \
    if ((H5tools_ERR_STACK_g = H5Ecreate_stack()) < 0)                                                  \
        HDfprintf(stderr, "Failed to create HDF5 tools error stack\n");                                 \
                                                                                                        \
    /* Create new HDF5 error stack for debugging, if tools debugging is enabled */                      \
    H5TOOLS_CREATE_DEBUG_STACK();                                                                       \
                                                                                                        \
    /* Register errors from the HDF5 tools as a new error class */                                      \
    if ((H5tools_ERR_CLS_g = H5Eregister_class("H5tools", "HDF5:tools", lib_str)) < 0)                  \
        HDfprintf(stderr, "Failed to register HDF5 tools error class\n");                               \
                                                                                                        \
    /* Create a new HDF5 major error message for errors from the tools library */                       \
    if ((H5E_tools_g = H5Ecreate_msg(H5tools_ERR_CLS_g, H5E_MAJOR, "Failure in tools library")) < 0)    \
        HDfprintf(stderr, "Failed to register major error message for tools library errors\n");         \
                                                                                                        \
    /* Create a new HDF5 minor error message for errors from the tools library */                       \
    if ((H5E_tools_min_id_g = H5Ecreate_msg(H5tools_ERR_CLS_g, H5E_MINOR, "error in function")) < 0)    \
        HDfprintf(stderr, "Failed to register minor error message for tools library errors\n");         \
                                                                                                        \
    /* Create a new HDF5 minor error message for info messages from the tools library */                \
    if ((H5E_tools_min_info_id_g = H5Ecreate_msg(H5tools_ERR_CLS_g, H5E_MINOR, "function info")) < 0)   \
        HDfprintf(stderr, "Failed to register minor error message for tools library info messages\n");  \
                                                                                                        \
    /* Create a new HDF5 minor error message for debug messages from the tools library */               \
    if ((H5E_tools_min_dbg_id_g = H5Ecreate_msg(H5tools_ERR_CLS_g, H5E_MINOR, "function debug")) < 0)   \
        HDfprintf(stderr, "Failed to register minor error message for tools library debug messages\n"); \
} while(0)

/*
 * H5TOOLS_CLOSE_ERROR macro, used to terminate error reporting.
 */
#define H5TOOLS_CLOSE_ERROR()                                                                        \
do {                                                                                                 \
    /* Close all error messages created by H5TOOLS_INIT_ERROR() */                                   \
    if (H5Eclose_msg(H5E_tools_min_dbg_id_g) < 0)                                                    \
        HDfprintf(stderr, "Failed to close minor error message for tools library debug messages\n"); \
    if (H5Eclose_msg(H5E_tools_min_info_id_g) < 0)                                                   \
        HDfprintf(stderr, "Failed to close minor error message for tools library info messages\n");  \
    if (H5Eclose_msg(H5E_tools_min_id_g) < 0)                                                        \
        HDfprintf(stderr, "Failed to close minor error message for tools library errors\n");         \
    if (H5Eclose_msg(H5E_tools_g) < 0)                                                               \
        HDfprintf(stderr, "Failed to close major error message for tools library errors\n");         \
                                                                                                     \
    /* Unregister the HDF5 tools error class */                                                      \
    if (H5Eunregister_class(H5tools_ERR_CLS_g) < 0)                                                  \
        HDfprintf(stderr, "Failed to unregister the HDF5 tools error class\n");                      \
                                                                                                     \
    /* Close the tools debugging error stack, if it was created */                                   \
    H5TOOLS_CLOSE_DEBUG_STACK();                                                                     \
                                                                                                     \
    /* Close the tools error stack */                                                                \
    if (H5Eclose_stack(H5tools_ERR_STACK_g) < 0)                                                     \
        HDfprintf(stderr, "Failed to close HDF5 tools error stack\n");                               \
} while(0)

/*
 * H5TOOLS_PUSH_ERROR macro, used to push an error to an error stack. Not meant to
 * be called directly.
 */
#define H5TOOLS_PUSH_ERROR(estack_id, err_cls, maj_err_id, min_err_id, ...)                          \
do {                                                                                                 \
    if (estack_id >= 0 && err_cls >= 0)                                                              \
        H5Epush2(estack_id, __FILE__, FUNC, __LINE__, err_cls, maj_err_id, min_err_id, __VA_ARGS__); \
    else {                                                                                           \
        HDfprintf(stderr, __VA_ARGS__);                                                              \
        HDfprintf(stderr, "\n");                                                                     \
    }                                                                                                \
} while(0)

/*
 * H5TOOLS_ERROR macro, used to facilitate error reporting within a function body.
 * The arguments are the return value and an error string. The return value is assigned
 * to a variable `ret_value'. This macro is meant to be used for reporting an error without
 * having control branch to the `done' label. This is often used when an error occurs
 * after the `done' label, in which case an infinite loop would ensue if control branched
 * backwards.
 */
#define H5TOOLS_ERROR(ret_val, ...)                                                                           \
do {                                                                                                          \
    H5TOOLS_PUSH_ERROR(H5tools_ERR_STACK_g, H5tools_ERR_CLS_g, H5E_tools_g, H5E_tools_min_id_g, __VA_ARGS__); \
    ret_value = ret_val;                                                                                      \
} while(0)

/*
 * H5TOOLS_GOTO_ERROR macro, used to facilitate error reporting within a function body.
 * The arguments are the return value and an error string. The return value is assigned
 * to a variable `ret_value' and control branches to the `done' label.
 */
#define H5TOOLS_GOTO_ERROR(ret_val, ...)                                                                      \
do {                                                                                                          \
    H5TOOLS_PUSH_ERROR(H5tools_ERR_STACK_g, H5tools_ERR_CLS_g, H5E_tools_g, H5E_tools_min_id_g, __VA_ARGS__); \
    H5TOOLS_GOTO_DONE(ret_val);                                                                               \
} while(0)

/*
 * H5TOOLS_GOTO_DONE macro, used to facilitate normal return within a function body.
 * The argument is the return value which is assigned to the `ret_value'
 * variable. Control branches to the `done' label.
 */
#define H5TOOLS_GOTO_DONE(ret_val) \
do {                               \
    ret_value = ret_val;           \
    goto done;                     \
} while(0)

/*
 * H5TOOLS_GOTO_DONE_NO_RET macro, used to facilitate normal return within a function body.
 * Control simply branches to the `done' label without setting any return value.
 */
#define H5TOOLS_GOTO_DONE_NO_RET() \
do {                               \
    goto done;                     \
} while(0)

/*
 * H5TOOLS_INFO macro, used to facilitate error reporting. The arguments are
 * a description of the error.
 */
#define H5TOOLS_INFO(...)                                                                                          \
do {                                                                                                               \
    H5TOOLS_PUSH_ERROR(H5tools_ERR_STACK_g, H5tools_ERR_CLS_g, H5E_tools_g, H5E_tools_min_info_id_g, __VA_ARGS__); \
} while(0)

#ifdef H5_TOOLS_DEBUG

#define H5TOOLS_CREATE_DEBUG_STACK()                                                             \
do {                                                                                             \
    /* Create new HDF5 error stack for debugging */                                              \
    if ((H5tools_DBG_ERR_STACK_g = H5Ecreate_stack()) < 0)                                       \
        HDfprintf(stderr, "Failed to create HDF5 tools debugging error stack\n");                \
                                                                                                 \
    /* Register debugging output from the HDF5 tools as a new error class */                     \
    if ((H5tools_DBG_ERR_CLS_g = H5Eregister_class("H5tools-debug", "HDF5:tools", lib_str)) < 0) \
        HDfprintf(stderr, "Failed to register HDF5 tools debugging error class\n");              \
} while(0)

#define H5TOOLS_CLOSE_DEBUG_STACK()                                                       \
do {                                                                                      \
    /* Unregister the HDF5 tools debugging error class */                                 \
    if (H5Eunregister_class(H5tools_DBG_ERR_CLS_g) < 0)                                   \
        HDfprintf(stderr, "Failed to unregister the HDF5 tools debugging error class\n"); \
                                                                                          \
    /* Close the tools debugging error stack */                                           \
    if (H5Eclose_stack(H5tools_DBG_ERR_STACK_g) < 0)                                      \
        HDfprintf(stderr, "Failed to close HDF5 tools debugging error stack\n");          \
} while(0)

#define H5TOOLS_DEBUG(...)                                                                                                \
do {                                                                                                                      \
    H5TOOLS_PUSH_ERROR(H5tools_DBG_ERR_STACK_g, H5tools_DBG_ERR_CLS_g, H5E_tools_g, H5E_tools_min_dbg_id_g, __VA_ARGS__); \
} while(0)

#define H5TOOLS_ENDDEBUG(...)                                                                                             \
do {                                                                                                                      \
    H5TOOLS_PUSH_ERROR(H5tools_DBG_ERR_STACK_g, H5tools_DBG_ERR_CLS_g, H5E_tools_g, H5E_tools_min_dbg_id_g, __VA_ARGS__); \
    H5Eprint2(H5tools_DBG_ERR_STACK_g, stderr);                                                                           \
} while(0)

#else

#define H5TOOLS_CREATE_DEBUG_STACK() \
do {                                 \
    ;                                \
} while(0)

#define H5TOOLS_CLOSE_DEBUG_STACK() \
do {                                \
    ;                               \
} while(0)

#define H5TOOLS_DEBUG(...) \
do {                       \
    ;                      \
} while(0)

#define H5TOOLS_ENDDEBUG(...) \
do {                          \
    ;                         \
} while(0)

#endif


/* Macro for "catching" flow of control when an error occurs.  Note that the
 *      H5_LEAVE macro won't jump back here once it's past this point.
 */
/* #define CATCH catch_except:; past_catch = TRUE; defined in H5Eprivate.h */

/*
 * H5_LEAVE macro, used to facilitate control flow between a
 * BEGIN_FUNC() and an END_FUNC() within a function body.  The argument is
 * the return value.
 * The return value is assigned to a variable `ret_value' and control branches
 * to the `catch_except' label, if we're not already past it.
 */
/*
 *  #define H5_LEAVE(v) {                           \
 *   ret_value = v;                              \
 *   if(!past_catch)                             \
 *        goto catch_except;                      \
 * }
 * defined in H5Eprivate.h */

/*
 * H5TOOLS_THROW macro, used to facilitate error reporting within a function body.
 * The arguments are the minor error number, and an error string.
 * The return value is assigned to a variable `ret_value' and control branches
 * to the `catch_except' label, if we're not already past it.
 */
#define H5TOOLS_THROW(ret_val, ...)                                                                           \
do {                                                                                                          \
    H5TOOLS_PUSH_ERROR(H5tools_ERR_STACK_g, H5tools_ERR_CLS_g, H5E_tools_g, H5E_tools_min_id_g, __VA_ARGS__); \
    H5_LEAVE(ret_val)                                                                                         \
} while(0)

#endif /* H5TOOLS_ERROR_H_ */

