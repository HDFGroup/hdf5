/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef _H5HLprivate2_H
#define _H5HLprivate2_H

/* Standard C Headers */
#include <assert.h>
#include <ctype.h>

/* Public HDF5 header */
#include "hdf5.h"

/* Public High-Level header */
#include "hdf5_hl.h"

/* Private header files needed */
#include "H5HLerror.h"          /* High-Level errors */

/* The following is copied from src/H5private.h */

/*
 * Status return values for the `herr_t' type.
 * Since some unix/c routines use 0 and -1 (or more precisely, non-negative
 * vs. negative) as their return code, and some assumption had been made in
 * the code about that, it is important to keep these constants the same
 * values.  When checking the success or failure of an integer-valued
 * function, remember to compare against zero and not one of these two
 * values.
 */
#define SUCCEED		0
#define FAIL		(-1)
#define UFAIL		(unsigned)(-1)

/* minimum of two, three, or four values */
#undef MIN
#define MIN(a,b)		(((a)<(b)) ? (a) : (b))
#define MIN2(a,b)		MIN(a,b)
#define MIN3(a,b,c)		MIN(a,MIN(b,c))
#define MIN4(a,b,c,d)		MIN(MIN(a,b),MIN(c,d))

/* maximum of two, three, or four values */
#undef MAX
#define MAX(a,b)		(((a)>(b)) ? (a) : (b))
#define MAX2(a,b)		MAX(a,b)
#define MAX3(a,b,c)		MAX(a,MAX(b,c))
#define MAX4(a,b,c,d)		MAX(MAX(a,b),MAX(c,d))

/*
 * HDF Boolean type.
 */
#ifndef FALSE
#   define FALSE 0
#endif
#ifndef TRUE
#   define TRUE 1
#endif

/* Macro for "glueing" together items, for re-scanning macros */
#define H5_GLUE(x,y)       x##y
#define H5_GLUE3(x,y,z)    x##y##z
#define H5_GLUE4(w,x,y,z)  w##x##y##z

/************************************************/
/* Revisions to FUNC_ENTER/LEAVE & Error Macros */
/************************************************/

/* `S' is the name of a function which is being tested to check if it's */
/*      a public API function */
#define H5_IS_PUB(S) (((isdigit(S[1]) || isupper(S[1])) && islower(S[2])) || \
    ((isdigit(S[2]) || isupper(S[2])) && islower(S[3])) || \
    (!S[4] || ((isdigit(S[3]) || isupper(S[3])) && islower(S[4]))))

/* `S' is the name of a function which is being tested to check if it's */
/*      a private library function */
#define H5_IS_PRIV(S) (((isdigit(S[1]) || isupper(S[1])) && '_' == S[2] && islower(S[3])) || \
    ((isdigit(S[2]) || isupper(S[2])) && '_' == S[3] && islower(S[4])) || \
    ((isdigit(S[3]) || isupper(S[3])) && '_' == S[4] && islower(S[5])))

/* `S' is the name of a function which is being tested to check if it's */
/*      a package private function */
#define H5_IS_PKG(S) (((isdigit(S[1]) || isupper(S[1])) && '_' == S[2] && '_' == S[3] && islower(S[4])) || \
    ((isdigit(S[2]) || isupper(S[2])) && '_' == S[3] && '_' == S[4] && islower(S[5])) || \
    ((isdigit(S[3]) || isupper(S[3])) && '_' == S[4] && '_' == S[5] && islower(S[6])))

#ifndef NDEBUG
#define FUNC_ENTER_NAME_CHECK(asrt)					      \
    {					          			      \
        static hbool_t func_check = FALSE;          			      \
                                                                              \
        if(!func_check) {		   				      \
            /* Check function naming status */				      \
            assert(asrt);			                              \
                                                                              \
            /* Don't check again */                 			      \
            func_check = TRUE;						      \
        } /* end if */							      \
    } /* end scope */
#else /* NDEBUG */
#define FUNC_ENTER_NAME_CHECK(asrt)
#endif /* NDEBUG */

/* Macros for referencing package initialization symbols */
#define H5_PACKAGE_INIT_VAR(x) H5_GLUE3(H5_, x, _init_g)
#define H5_PACKAGE_INIT_FUNC(x) H5_GLUE3( ,x, __pkg_init)

/* Macros to check if a package is initialized */
#define H5_CHECK_PACKAGE_INIT_REG_YES(asrt)       assert(H5_PACKAGE_INIT_VAR(pkg));
#define H5_CHECK_PACKAGE_INIT_REG_NO(asrt)
#define H5_CHECK_PACKAGE_INIT_INIT_YES(asrt)
#define H5_CHECK_PACKAGE_INIT_INIT_NO(asrt)

/* Macros to initialize package, if a package initialization routine is defined */
#define H5_PKG_YES_INIT(pkg)						      \
    if(!H5_PACKAGE_INIT_VAR(pkg)) {					      \
        if(H5_PACKAGE_INIT_FUNC(pkg)() < 0) {				      \
            /* (Can't use H5E_THROW here) */				      \
            H5E_PRINTF(H5E_CANTINIT, "interface initialization failed");      \
            ret_value = fail_value;					      \
            goto func_init_failed;					      \
        } /* end if */						              \
    } /* end if */
#define H5_PKG_NO_INIT(pkg)

/* Macros to declare package initialization symbols, if a package initialization routine is defined */
#define H5_PKG_YES_INIT_VAR(pkg) extern hbool_t H5_PACKAGE_INIT_VAR(pkg);
#define H5_PKG_NO_INIT_VAR(pkg)
#define H5_PKG_YES_INIT_FUNC(pkg) extern herr_t H5_PACKAGE_INIT_FUNC(pkg)(void);
#define H5_PKG_NO_INIT_FUNC(pkg)

/* Declare package initialization symbols (if in a package) */
#define H5_DECLARE_PKG_VAR(pkg_init, pkg) H5_GLUE3(H5_PKG_, pkg_init, _INIT_VAR)(pkg)
#define H5_DECLARE_PKG_FUNC(pkg_init, pkg) H5_GLUE3(H5_PKG_, pkg_init, _INIT_FUNC)(pkg)
#ifdef H5_MY_PKG
H5_DECLARE_PKG_VAR(H5_MY_PKG_INIT, H5_MY_PKG)
H5_DECLARE_PKG_FUNC(H5_MY_PKG_INIT, H5_MY_PKG)
#endif /* H5_MY_PKG */

/* API re-entrance variable */
extern hbool_t H5HL_api_entered_g;    /* Has library already been entered through API? */

/* extern global variables */
extern hbool_t H5HL_libinit_g;    /* Has the library been initialized? */

/* Use FUNC to safely handle variations of C99 __func__ keyword handling */
#ifdef H5_HAVE_C99_FUNC
#define FUNC __func__
#elif defined(H5_HAVE_FUNCTION)
#define FUNC __FUNCTION__
#else
#error "We need __func__ or __FUNCTION__ to test function names!"
#endif

/* Macros for entering different scopes of routines */
#define H5_PACKAGE_ENTER(pkg, pkg_init, init)				      \
    FUNC_ENTER_NAME_CHECK(H5_IS_PKG(FUNC))				      \
                                                                              \
    /* The library should be initialized already */			      \
    assert(H5HL_libinit_g);						      \
                                                                              \
    /* This interface should be initialized already */			      \
    /* (except for package initialization routines :-) */		      \
    H5_GLUE4(H5_CHECK_PACKAGE_INIT_, init, _, pkg_init)(pkg)		      \
                                                                              \
    /* Enter scope for this type of function */				      \
    {

#define H5_PRIVATE_ENTER(pkg, pkg_init)					      \
    FUNC_ENTER_NAME_CHECK(H5_IS_PRIV(FUNC))				      \
                                                                              \
    /* The library should be initialized already */			      \
    assert(H5HL_libinit_g);						      \
                                                                              \
    /* Initialize this interface if desired */				      \
    H5_GLUE3(H5_PKG_, pkg_init, _INIT)(pkg)				      \
                                                                              \
    /* Enter scope for this type of function */				      \
    {{

#define H5_PUBLIC_ENTER(pkg, pkg_init)					      \
    FUNC_ENTER_NAME_CHECK(H5_IS_PUB(FUNC))				      \
                                                                              \
    /* Clear thread error stack when entering public functions */	      \
    H5Eclear2(H5E_DEFAULT);				                      \
                                                                              \
    /* Initialize the library or bust */				      \
    if(!(H5HL_libinit_g)) {						      \
        H5HL_libinit_g = TRUE;                                                \
        if(H5HL_init_library() < 0) {  					      \
            /* (Can't use H5E_THROW here) */				      \
            H5E_PRINTF(H5E_CANTINIT, "interface initialization failed");      \
            ret_value = fail_value;					      \
            goto func_init_failed;					      \
        } /* end if */						              \
    } /* end if */						              \
                                                                              \
    /* Initialize this interface if desired */				      \
    H5_GLUE3(H5_PKG_, pkg_init, _INIT)(pkg)				      \
                                                                              \
    /* Check for re-entering API routine */				      \
    assert(!H5HL_api_entered_g);					      \
    H5HL_api_entered_g = TRUE;						      \
                                                                              \
    /* Enter scope for this type of function */				      \
    {{{

/* Macros for substituting the package name */
#define FUNC_ENTER_STATIC	H5_PACKAGE_ENTER(H5_MY_PKG, H5_MY_PKG_INIT, REG)
#define FUNC_ENTER_PKGINIT	H5_PACKAGE_ENTER(H5_MY_PKG, H5_MY_PKG_INIT, INIT)
#define FUNC_ENTER_PKG		H5_PACKAGE_ENTER(H5_MY_PKG, H5_MY_PKG_INIT, REG)
#define FUNC_ENTER_PRIV		H5_PRIVATE_ENTER(H5_MY_PKG, H5_MY_PKG_INIT)
#define FUNC_ENTER_PUB		H5_PUBLIC_ENTER(H5_MY_PKG, H5_MY_PKG_INIT)

/* Macros for substituting a function prefix */
#define FUNC_PREFIX_STATIC	static
#define FUNC_PREFIX_PKGINIT
#define FUNC_PREFIX_PKG
#define FUNC_PREFIX_PRIV
#define FUNC_PREFIX_PUB

/* Macros for declaring error variables */
#define FUNC_ERR_VAR_ERR(ret_typ, err)					      \
    hbool_t past_catch = FALSE;						      \
    ret_typ fail_value = err;
#define FUNC_ERR_VAR_ERRCATCH(ret_typ, err)					      \
    hbool_t past_catch = FALSE;
#define FUNC_ERR_VAR_NOERR(ret_typ, err)

/* Use this macro when entering all functions */
#define BEGIN_FUNC(scope, use_err, ret_typ, ret_init, err, func)	      \
H5_GLUE(FUNC_PREFIX_, scope)						      \
ret_typ									      \
func									      \
/* Open function */							      \
{									      \
    ret_typ ret_value = ret_init;					      \
    H5_GLUE(FUNC_ERR_VAR_, use_err)(ret_typ, err)			      \
    H5_GLUE(FUNC_ENTER_, scope)

/* Macros for label when a function initialization can fail */
#define H5_PRIV_YES_FUNC_INIT_FAILED func_init_failed:
#define H5_PRIV_NO_FUNC_INIT_FAILED
#define H5_PRIV_FUNC_INIT_FAILED(pkg_init) H5_GLUE3(H5_PRIV_, pkg_init, _FUNC_INIT_FAILED)

/* Macros for leaving different scopes of routines */
#define FUNC_LEAVE_PKGINIT					       	      \
    /* Leave scope for this type of function */				      \
    }

#define FUNC_LEAVE_STATIC					       	      \
    /* Leave scope for this type of function */				      \
    }

#define FUNC_LEAVE_PKG						       	      \
    /* Leave scope for this type of function */				      \
    }

#define FUNC_LEAVE_PRIV						       	      \
    /* Leave scope for this type of function */				      \
    }}									      \
                                                                              \
    /* Label for errors during FUNC_ENTER */				      \
    H5_PRIV_FUNC_INIT_FAILED(H5_MY_PKG_INIT)

#define FUNC_LEAVE_PUB						       	      \
    /* Leave scope for this type of function */				      \
    }}}									      \
                                                                              \
    /* Label for errors during FUNC_ENTER */				      \
func_init_failed:							      \
                                                                              \
    /* Dump error stack if an error occurred during API routine */	      \
    if(ret_value == fail_value)						      \
        (void)H5HLE_dump_api_stack(TRUE);				      \
                                                                              \
    /* Check for leaving API routine */					      \
    assert(H5HL_api_entered_g);						      \
    H5HL_api_entered_g = FALSE;						      \
                                                                              \

/* Use this macro when leaving all functions */
#define END_FUNC(scope)							      \
    /* Scope-specific function conclusion */				      \
    H5_GLUE(FUNC_LEAVE_, scope)						      \
                                                                              \
    /* Leave routine */							      \
    return(ret_value);							      \
                                                                              \
    /* Close Function */						      \
}


/*
 * H5E_PRINTF macro, used to facilitate error reporting between a BEGIN_FUNC()
 * and an END_FUNC() within a function body.  The arguments are the minor
 * error number, a description of the error (as a printf-like format string),
 * and an optional set of arguments for the printf format arguments.
 */
#define H5E_PRINTF(...) H5Epush2(H5E_DEFAULT, __FILE__, FUNC, __LINE__, H5HL_ERR_CLS_g, H5_MY_PKG_ERR,  __VA_ARGS__)

/*
 * H5_LEAVE macro, used to facilitate control flow between a
 * BEGIN_FUNC() and an END_FUNC() within a function body.  The argument is
 * the return value.
 * The return value is assigned to a variable `ret_value' and control branches
 * to the `catch_except' label, if we're not already past it.
 */
#define H5_LEAVE(v) {							      \
    ret_value = v;							      \
    if(!past_catch)							      \
        goto catch_except;						      \
}

/*
 * H5E_THROW macro, used to facilitate error reporting between a
 * FUNC_ENTER() and a FUNC_LEAVE() within a function body.  The arguments are
 * the minor error number, and an error string.
 * The return value is assigned to a variable `ret_value' and control branches
 * to the `catch_except' label, if we're not already past it.
 */
#define H5E_THROW(...) {						      \
    H5E_PRINTF(__VA_ARGS__);						      \
    H5_LEAVE(fail_value)						      \
}

/* Macro for "catching" flow of control when an error occurs.  Note that the
 *      H5_LEAVE macro won't jump back here once it's past this point.
 */
#define CATCH past_catch = TRUE; catch_except:;

/* Private functions */
herr_t H5HL_init_library(void);

#endif /* _H5HLprivate2_H */

