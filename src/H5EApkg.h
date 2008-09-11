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

/*
 * Programmer:	Quincey Koziol <koziol@hdfgroup.org>
 *		Tuesday, June 17, 2008
 *
 * Purpose:	This file contains declarations which are visible only within
 *		the H5EA package.  Source files outside the H5EA package should
 *		include H5EAprivate.h instead.
 */
#if !(defined(H5EA_PACKAGE) | defined(H5EA_MODULE))
#error "Do not include this file outside the H5EA package!"
#endif

#ifndef _H5EApkg_H
#define _H5EApkg_H

/* Get package's private header */
#include "H5EAprivate.h"

/* Other private headers needed by this file */
#include "H5ACprivate.h"	/* Metadata cache			*/
#include "H5FLprivate.h"	/* Free Lists                           */

/************************************************/
/* Revisions to FUNC_ENTER/LEAVE & Error Macros */
/************************************************/

#ifndef NDEBUG
/* `S' is the name of a function which is being tested to check if it's */
/*      a public API function */
#define H5_IS_PUB(S) (((HDisdigit(S[1]) || HDisupper(S[1])) && HDislower(S[2])) || \
    ((HDisdigit(S[2]) || HDisupper(S[2])) && HDislower(S[3])) || \
    (!S[4] || ((HDisdigit(S[3]) || HDisupper(S[3])) && HDislower(S[4]))))

/* `S' is the name of a function which is being tested to check if it's */
/*      a private library function */
#define H5_IS_PRIV(S) (((HDisdigit(S[1]) || HDisupper(S[1])) && '_' == S[2] && HDislower(S[3])) || \
    ((HDisdigit(S[2]) || HDisupper(S[2])) && '_' == S[3] && HDislower(S[4])) || \
    ((HDisdigit(S[3]) || HDisupper(S[3])) && '_' == S[4] && HDislower(S[5])))

/* `S' is the name of a function which is being tested to check if it's */
/*      a package private function */
#define H5_IS_PKG(S) (((HDisdigit(S[1]) || HDisupper(S[1])) && '_' == S[2] && '_' == S[3] && HDislower(S[4])) || \
    ((HDisdigit(S[2]) || HDisupper(S[2])) && '_' == S[3] && '_' == S[4] && HDislower(S[5])) || \
    ((HDisdigit(S[3]) || HDisupper(S[3])) && '_' == S[4] && '_' == S[5] && HDislower(S[6])))

#define FUNC_ENTER_NAME_CHECK(asrt)					      \
    {					          			      \
        static hbool_t func_check = FALSE;          			      \
                                                                              \
        if(!func_check) {		   				      \
            /* Check function naming status */				      \
            HDassert(asrt);			                              \
                                                                              \
            /* Don't check again */                 			      \
            func_check = TRUE;						      \
        } /* end if */							      \
    } /* end scope */
#else /* NDEBUG */
#define FUNC_ENTER_NAME_CHECK(asrt)
#define H5_IS_PUB(S)
#define H5_IS_PRIV(S)
#define H5_IS_PKG(S)
#endif /* NDEBUG */

/* Macro for referencing package initialization variables */
#define H5_PACKAGE_INIT_VAR(x) H5_GLUE3(H5_, x, _init_g)

/* Macros to check if a package is initialized */
#define H5_CHECK_PACKAGE_INIT_REG_YES(asrt)       HDassert(H5_PACKAGE_INIT_VAR(pkg));
#define H5_CHECK_PACKAGE_INIT_REG_NO(asrt)
#define H5_CHECK_PACKAGE_INIT_INIT_YES(asrt)
#define H5_CHECK_PACKAGE_INIT_INIT_NO(asrt)

/* Macros to initialize package, if a package initialization routine is defined */
#define H5_PKG_YES_INIT(pkg)						      \
    if(!H5_PACKAGE_INIT_VAR(pkg)) {					      \
        if(H5_GLUE(pkg, _pkg_init)() < 0) {				      \
            /* (Can't use H5E_THROW here) */				      \
            H5E_PRINTF(H5E_CANTINIT, "interface initialization failed");      \
            ret_value = fail_value;					      \
            goto func_init_failed;					      \
        } /* end if */						              \
    } /* end if */
#define H5_PKG_NO_INIT(pkg)

/* Macros to declare package initialization variable, if a package initialization routine is defined */
#define H5_PKG_YES_INIT_VAR(pkg) extern hbool_t H5_PACKAGE_INIT_VAR(H5_MY_PKG);
#define H5_PKG_NO_INIT_VAR(pkg)

/* Declare package initialization variable (if in a package) */
#define H5_DECLARE_PKG_VAR(pkg_init, pkg) H5_GLUE3(H5_PKG_, pkg_init, _INIT_VAR)(pkg)
#ifdef H5_MY_PKG
H5_DECLARE_PKG_VAR(H5_MY_PKG_INIT, H5_MY_PKG)
#endif /* H5_MY_PKG */

/* API re-entrance variable */
extern hbool_t H5_api_entered_g;    /* Has library already been entered through API? */

/* Macros for entering different scopes of routines */
#define H5_PACKAGE_ENTER(pkg, pkg_init, init)				      \
    FUNC_ENTER_NAME_CHECK(H5_IS_PKG(__func__))				      \
                                                                              \
    /* The library should be initialized already */			      \
    HDassert(H5_INIT_GLOBAL);						      \
                                                                              \
    /* This interface should be initialized already */			      \
    /* (except for package initialization routines :-) */		      \
    H5_GLUE4(H5_CHECK_PACKAGE_INIT_, init, _, pkg_init)(pkg)		      \
                                                                              \
    /* Push the name of this function on the function stack */		      \
    H5_PUSH_FUNC(__func__)						      \
                                                                              \
    /* Enter scope for this type of function */				      \
    {

#define H5_PRIVATE_ENTER(pkg, pkg_init)					      \
    FUNC_ENTER_NAME_CHECK(H5_IS_PRIV(__func__))				      \
                                                                              \
    /* The library should be initialized already */			      \
    HDassert(H5_INIT_GLOBAL);						      \
                                                                              \
    /* Initialize this interface if desired */				      \
    H5_GLUE3(H5_PKG_, pkg_init, _INIT)(pkg)				      \
                                                                              \
    /* Push the name of this function on the function stack */		      \
    H5_PUSH_FUNC(__func__)						      \
                                                                              \
    /* Enter scope for this type of function */				      \
    {{

/* Remove this shim and change H5TRACE* macros when this change is permanent -QAK */
#ifdef H5_DEBUG_API
#define FUNC __func__
#endif

#define H5_PUBLIC_ENTER(pkg, pkg_init)					      \
    FUNC_ENTER_API_VARS(__func__)                                             \
    FUNC_ENTER_API_THREADSAFE;                                                \
    FUNC_ENTER_NAME_CHECK(H5_IS_PUB(__func__))				      \
                                                                              \
    /* Clear thread error stack when entering public functions */	      \
    H5E_clear_stack(NULL);				                      \
                                                                              \
    /* Initialize the library or bust */				      \
    if(!(H5_INIT_GLOBAL)) {						      \
        H5_INIT_GLOBAL = TRUE;                                                \
        if(H5_init_library() < 0) {  					      \
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
    HDassert(!H5_api_entered_g);					      \
    H5_api_entered_g = TRUE;						      \
                                                                              \
    /* Start logging MPI's MPE information */				      \
    BEGIN_MPE_LOG(__func__)						      \
                                                                              \
    /* Push the name of this function on the function stack */		      \
    H5_PUSH_FUNC(__func__)						      \
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
#define FUNC_LEAVE_STATIC					       	      \
    /* Leave scope for this type of function */				      \
    }									      \
                                                                              \
    /* Pop the name of this function off the function stack */		      \
    H5_POP_FUNC

#define FUNC_LEAVE_PKG						       	      \
    /* Leave scope for this type of function */				      \
    }									      \
                                                                              \
    /* Pop the name of this function off the function stack */		      \
    H5_POP_FUNC

#define FUNC_LEAVE_PRIV						       	      \
    /* Leave scope for this type of function */				      \
    }}									      \
                                                                              \
    /* Label for errors during FUNC_ENTER */				      \
    H5_PRIV_FUNC_INIT_FAILED(H5_MY_PKG_INIT)				      \
                                                                              \
    /* Pop the name of this function off the function stack */		      \
    H5_POP_FUNC

#define FUNC_LEAVE_PUB						       	      \
    /* Leave scope for this type of function */				      \
    }}}									      \
                                                                              \
    /* Label for errors during FUNC_ENTER */				      \
func_init_failed:							      \
                                                                              \
    /* Dump error stack if an error occurred during API routine */	      \
    if(ret_value == fail_value)						      \
        (void)H5E_dump_api_stack(TRUE);					      \
                                                                              \
    /* Finish the API tracing info */					      \
    H5TRACE_RETURN(ret_value);						      \
                                                                              \
    /* Pop the name of this function off the function stack */		      \
    H5_POP_FUNC							      \
                                                                              \
    /* Finish the MPE tracing info */					      \
    FINISH_MPE_LOG;							      \
                                                                              \
    /* Check for leaving API routine */					      \
    HDassert(H5_api_entered_g);						      \
    H5_api_entered_g = FALSE;						      \
                                                                              \
    /* Release thread-safety semaphore */				      \
    FUNC_LEAVE_API_THREADSAFE

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
#define H5E_PRINTF(...) H5E_printf_stack(NULL, __FILE__, __func__, __LINE__, H5E_ERR_CLS_g, H5_MY_PKG_ERR,  __VA_ARGS__)

/*
 * H5_LEAVE macro, used to facilitate control flow between a
 * BEGIN_FUNC() and an END_FUNC() within a function body.  The argument is
 * the return value.
 * The return value is assigned to a variable `ret_value' and control branches
 * to the `catch' label, if we're not already past it.
 */
#define H5_LEAVE(v) {							      \
    ret_value = v;							      \
    if(!past_catch)							      \
        goto catch;							      \
}

/*
 * H5E_THROW macro, used to facilitate error reporting between a
 * FUNC_ENTER() and a FUNC_LEAVE() within a function body.  The arguments are
 * the minor error number, and an error string.
 * The return value is assigned to a variable `ret_value' and control branches
 * to the `catch' label, if we're not already past it.
 */
#define H5E_THROW(...) {						      \
    H5E_PRINTF(__VA_ARGS__);						      \
    H5_LEAVE(fail_value)						      \
}

/* Macro for "catching" flow of control when an error occurs.  Note that the
 *      H5_LEAVE macro won't jump back here once it's past this point.
 */
#define CATCH past_catch = TRUE; catch:;


/**************************/
/* Package Private Macros */
/**************************/

/* If this package header is being included in one of the H5EA modules, define
 *      the proper control macros for the generic FUNC_ENTER/LEAVE and error
 *      reporting macros.
 */
#ifdef H5EA_MODULE
#define H5_MY_PKG       H5EA
#define H5_MY_PKG_ERR   H5E_EARRAY
#define H5_MY_PKG_INIT  NO
#endif /* H5EA_MODULE */

/* Size of signature information (on disk) */
#define H5EA_SIZEOF_MAGIC               4

/* Fill value for extensible array test class */
#ifdef H5EA_TESTING
#define H5EA_TEST_FILL          ((uint64_t)ULLONG_MAX)
#endif /* H5EA_TESTING */

/* Extensible array signatures */
#define H5EA_HDR_MAGIC                  "EAHD"          /* Header */
#define H5EA_IBLOCK_MAGIC               "EAIB"          /* Index block */

/* Size of checksum information (on disk) */
#define H5EA_SIZEOF_CHKSUM      4

/* "Standard" size of prefix information for extensible array metadata */
#define H5EA_METADATA_PREFIX_SIZE(c) (                                        \
    H5EA_SIZEOF_MAGIC   /* Signature */                                       \
    + 1 /* Version */                                                         \
    + 1 /* Array type */                                                      \
    + ((c) ? H5EA_SIZEOF_CHKSUM : 0) /* Metadata checksum */                  \
    )

/* Size of the extensible array header on disk */
#define H5EA_HEADER_SIZE(h)     (                                             \
    /* General metadata fields */                                             \
    H5EA_METADATA_PREFIX_SIZE(TRUE)                                           \
                                                                              \
    /* General heap information */                                            \
    + 1 /* Element Size */                                                    \
    + 1 /* Max. # of elements bits */                                         \
    + 1 /* # of elements to store in index block */                           \
    + 1 /* Min. # elements per data block */                                  \
    + 1 /* Min. # of data block pointers for a super block */                 \
                                                                              \
    /* Extensible Array Header specific fields */                             \
    + (h)->sizeof_addr /* File address of index block  */		      \
    + (h)->sizeof_size /* Max. index set  */				      \
    )

/* Size of the extensible array index block on disk */
#define H5EA_IBLOCK_SIZE(h)     (                                             \
    /* General metadata fields */                                             \
    H5EA_METADATA_PREFIX_SIZE(TRUE)                                           \
                                                                              \
    /* Extensible Array Index Block specific fields */			      \
    + (size_t)((h)->idx_blk_elmts * (h)->raw_elmt_size) /* Elements in index block  */ \
    )


/****************************/
/* Package Private Typedefs */
/****************************/

/* The extensible array header information */
/* (Each extensible array header has certain information that is shared across
 * all the blocks in that extensible array)
 */
typedef struct H5EA_hdr_t {
    /* Information for H5AC cache functions, _must_ be first field in structure */
    H5AC_info_t cache_info;

    /* Extensible array configuration/creation parameters (stored) */
    uint8_t raw_elmt_size;              /* Element size in file (in bytes) */
    uint8_t max_nelmts_bits;            /* Log2(Max. # of elements in array) - i.e. # of bits needed to store max. # of elements */
    uint8_t idx_blk_elmts;              /* # of elements to store in index block */
    uint8_t data_blk_min_elmts;         /* Min. # of elements per data block */
    uint8_t sup_blk_min_data_ptrs;      /* Min. # of data block pointers for a super block */

    /* Index block information (stored in header) */
    haddr_t idx_blk_addr;               /* Address of index block in header */

    /* Statistics for array (stored in header) */
    hsize_t max_idx_set;                /* Highest element index stored (+1 - i.e. if element 0 has been set, this value with be '1', if no elements have been stored, this value will be '0') */

    /* Computed/cached values */
    size_t rc;                          /* Reference count of heap's components using heap header */
    haddr_t addr;                       /* Address of header in file */
    size_t size;                        /* Size of header in file */
    H5F_t *f;                           /* Pointer to file for extensible array */
    size_t file_rc;                     /* Reference count of files using array header */
    hbool_t pending_delete;             /* Array is pending deletion */
    size_t sizeof_addr;                 /* Size of file addresses */
    size_t sizeof_size;                 /* Size of file sizes */

    /* Memory data structures (not stored directly) */
    const H5EA_class_t *cls;            /* Pointer to class for array */
} H5EA_hdr_t;

/* The extensible array index block information */
typedef struct H5EA_iblock_t {
    /* Information for H5AC cache functions, _must_ be first field in structure */
    H5AC_info_t cache_info;

    /* Extensible array information (stored) */
    void        *elmts;         /* Buffer for elements stored in index block  */

    /* Internal array information (not stored) */
    size_t      rc;             /* Reference count of objects using this block */
    H5EA_hdr_t	*hdr;	        /* Shared array header info	              */
    haddr_t     addr;           /* Address of this index block on disk	      */
    size_t      size;           /* Size of index block on disk		      */
} H5EA_iblock_t;

/* Extensible array */
struct H5EA_t {
    H5EA_hdr_t  *hdr;           /* Pointer to internal extensible array header info */
    H5F_t      *f;              /* Pointer to file for extensible array */
};


/*****************************/
/* Package Private Variables */
/*****************************/

/* H5EA header inherits cache-like properties from H5AC */
H5_DLLVAR const H5AC_class_t H5AC_EARRAY_HDR[1];

/* H5EA index block inherits cache-like properties from H5AC */
H5_DLLVAR const H5AC_class_t H5AC_EARRAY_IBLOCK[1];

/* Declare a free list to manage the H5EA_hdr_t struct */
H5FL_EXTERN(H5EA_hdr_t);

/* Declare a free list to manage the H5EA_iblock_t struct */
H5FL_EXTERN(H5EA_iblock_t);

/* Declare a free list to manage the index block elements */
H5FL_BLK_EXTERN(elmt_buf);

/* Internal extensible array testing class */
#ifdef H5EA_TESTING
H5_DLLVAR const H5EA_class_t H5EA_CLS_TEST[1];
#endif /* H5EA_TESTING */


/******************************/
/* Package Private Prototypes */
/******************************/

/* Header routines */
H5_DLL H5EA_hdr_t *H5EA__hdr_alloc(H5F_t *f, const H5EA_class_t *cls);
H5_DLL haddr_t H5EA__hdr_create(H5F_t *f, hid_t dxpl_id, const H5EA_create_t *cparam);
H5_DLL herr_t H5EA__hdr_incr(H5EA_hdr_t *hdr);
H5_DLL herr_t H5EA__hdr_decr(H5EA_hdr_t *hdr);
H5_DLL herr_t H5EA__hdr_fuse_incr(H5EA_hdr_t *hdr);
H5_DLL size_t H5EA__hdr_fuse_decr(H5EA_hdr_t *hdr);
H5_DLL herr_t H5EA__hdr_modified(H5EA_hdr_t *hdr);
H5_DLL herr_t H5EA__hdr_delete(H5EA_hdr_t *hdr, hid_t dxpl_id);

/* Index block routines */
H5_DLL H5EA_iblock_t *H5EA__iblock_alloc(H5EA_hdr_t *hdr);
H5_DLL haddr_t H5EA__iblock_create(H5EA_hdr_t *hdr, hid_t dxpl_id);
H5_DLL H5EA_iblock_t *H5EA__iblock_protect(H5EA_hdr_t *hdr, hid_t dxpl_id,
    H5AC_protect_t rw);
H5_DLL herr_t H5EA__iblock_unprotect(H5EA_iblock_t *iblock, hid_t dxpl_id,
    unsigned cache_flags);
H5_DLL herr_t H5EA__iblock_delete(H5EA_hdr_t *hdr, hid_t dxpl_id);

/* Metadata cache callbacks */
H5_DLL herr_t H5EA__cache_hdr_dest(H5F_t *f, H5EA_hdr_t *hdr);
H5_DLL herr_t H5EA__cache_iblock_dest(H5F_t *f, H5EA_iblock_t *iblock);

/* Debugging routines for dumping file structures */
H5_DLL herr_t H5EA__hdr_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr,
    FILE *stream, int indent, int fwidth, const H5EA_class_t *cls);
H5_DLL herr_t H5EA__iblock_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr,
    FILE *stream, int indent, int fwidth, const H5EA_class_t *cls,
    haddr_t hdr_addr);

/* Testing routines */
#ifdef H5EA_TESTING
H5_DLL herr_t H5EA_get_cparam_test(const H5EA_t *ea, H5EA_create_t *cparam);
H5_DLL int H5EA_cmp_cparam_test(const H5EA_create_t *cparam1, const H5EA_create_t *cparam2);
#endif /* H5EA_TESTING */

#endif /* _H5EApkg_H */

