/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Purpose:	Provides error handling in the form of a stack.  The
 *		FUNC_ENTER() macro clears the error stack whenever an API
 *		function is entered.  When an error is detected, an entry is
 *		pushed onto the stack.  As the functions unwind additional
 *		entries are pushed onto the stack. The API function will
 *		return some indication that an error occurred and the
 *		application can print the error stack.
 *
 *		Certain API functions in the H5E package (such as H5Eprint())
 *		do not clear the error stack.  Otherwise, any function which
 *		doesn't have an underscore immediately after the package name
 *		will clear the error stack.  For instance, H5Fopen() clears
 *		the error stack while H5F_open() does not.
 *
 *		An error stack has a fixed maximum size.  If this size is
 *		exceeded then the stack will be truncated and only the
 *		inner-most functions will have entries on the stack. This is
 *		expected to be a rare condition.
 *
 *		Each thread has its own error stack, but since
 *		multi-threading has not been added to the library yet, this
 *		package maintains a single error stack. The error stack is
 *		statically allocated to reduce the complexity of handling
 *		errors within the H5E package.
 *
 */
#include "H5private.h"		/* Generic Functions			  */
#include "H5Iprivate.h"		/* IDs                                    */
#include "H5Eprivate.h"		/* Private error routines		  */
#include "H5MMprivate.h"	/* Memory management			  */

#define PABLO_MASK	H5E_mask

/* Major error IDs */
hid_t    H5E_NONE_MAJOR_g   = FAIL;                /*special zero, no error                     */
hid_t    H5E_ARGS_g         = FAIL;                      /*invalid arguments to routine               */
hid_t    H5E_RESOURCE_g     = FAIL;                  /*resource unavailable                       */
hid_t    H5E_INTERNAL_g     = FAIL;                  /*Internal error (too specific to document in detail) */
hid_t    H5E_FILE_g         = FAIL;                      /*file Accessability                         */
hid_t    H5E_IO_g           = FAIL;                        /*Low-level I/O                              */
hid_t    H5E_FUNC_g         = FAIL;                      /*function Entry/Exit                        */
hid_t    H5E_ATOM_g         = FAIL;                      /*object Atom                                */
hid_t    H5E_CACHE_g        = FAIL;                     /*object Cache                               */
hid_t    H5E_BTREE_g        = FAIL;                     /*B-Tree Node                                */
hid_t    H5E_SYM_g          = FAIL;                       /*symbol Table                               */
hid_t    H5E_HEAP_g         = FAIL;                      /*Heap                                       */
hid_t    H5E_OHDR_g         = FAIL;                      /*object Header                              */
hid_t    H5E_DATATYPE_g     = FAIL;                  /*Datatype                                   */
hid_t    H5E_DATASPACE_g    = FAIL;                 /*Dataspace                                  */
hid_t    H5E_DATASET_g      = FAIL;                   /*Dataset                                    */
hid_t    H5E_STORAGE_g      = FAIL;                   /*data storage                               */
hid_t    H5E_PLIST_g        = FAIL;                     /*Property lists                             */
hid_t    H5E_ATTR_g         = FAIL;                      /*Attribute                                  */
hid_t    H5E_PLINE_g        = FAIL;                     /*Data filters                               */
hid_t    H5E_EFL_g          = FAIL;                       /*External file list                         */
hid_t    H5E_REFERENCE_g    = FAIL;                 /*References                                 */
hid_t    H5E_VFL_g          = FAIL;		        /*Virtual File Layer			     */
hid_t    H5E_TBBT_g         = FAIL; 		        /*Threaded, Balanced, Binary Trees           */
hid_t    H5E_FPHDF5_g       = FAIL;		        /*Flexible Parallel HDF5                     */
hid_t    H5E_TST_g          = FAIL; 		        /*Ternary Search Trees                       */
hid_t    H5E_RS_g           = FAIL;  		        /*Reference Counted Strings                  */
hid_t    H5E_ERROR_g        = FAIL;  		        /*Error API				     */

/* Minor error IDs */
hid_t    H5E_NONE_MINOR_g         = FAIL;                /*special zero, no error                     */
hid_t    H5E_UNINITIALIZED_g              = FAIL;             /*information is unitialized                 */
hid_t    H5E_UNSUPPORTED_g                = FAIL;               /*feature is unsupported                     */
hid_t    H5E_BADTYPE_g            = FAIL;                   /*incorrect type found                       */
hid_t    H5E_BADRANGE_g           = FAIL;                  /*argument out of range                      */
hid_t    H5E_BADVALUE_g           = FAIL;                  /*bad value for argument                     */

hid_t    H5E_NOSPACE_g            = FAIL;                   /*no space available for allocation          */
hid_t    H5E_CANTCOPY_g           = FAIL;                  /*unable to copy object                      */
hid_t    H5E_CANTFREE_g           = FAIL;                  /*unable to free object                      */
hid_t    H5E_ALREADYEXISTS_g              = FAIL;             /*Object already exists                      */
hid_t    H5E_CANTLOCK_g           = FAIL;                  /*Unable to lock object                      */
hid_t    H5E_CANTUNLOCK_g         = FAIL;                /*Unable to unlock object                    */
hid_t    H5E_CANTGC_g             = FAIL;		        /*Unable to garbage collect                  */

hid_t    H5E_FILEEXISTS_g         = FAIL;                /*file already exists                        */
hid_t    H5E_FILEOPEN_g           = FAIL;                  /*file already open                          */
hid_t    H5E_CANTCREATE_g         = FAIL;                /*Can't create file                          */
hid_t    H5E_CANTOPENFILE_g               = FAIL;              /*Can't open file                            */
hid_t    H5E_CANTCLOSEFILE_g             = FAIL;             /*Can't close file			     */
hid_t    H5E_NOTHDF5_g            = FAIL;                   /*not an HDF5 format file                    */
hid_t    H5E_BADFILE_g            = FAIL;                   /*bad file ID accessed                       */
hid_t    H5E_TRUNCATED_g          = FAIL;                 /*file has been truncated                    */
hid_t    H5E_MOUNT_g              = FAIL;			/*file mount error			     */

hid_t    H5E_SEEKERROR_g          = FAIL;                 /*seek failed                                */
hid_t    H5E_READERROR_g          = FAIL;                 /*read failed                                */
hid_t    H5E_WRITEERROR_g         = FAIL;                /*write failed                               */
hid_t    H5E_CLOSEERROR_g         = FAIL;                /*close failed                               */
hid_t    H5E_OVERFLOW_g           = FAIL;		        /*address overflowed			     */
hid_t    H5E_FCNTL_g              = FAIL;                     /*file fcntl failed                          */

hid_t    H5E_CANTINIT_g           = FAIL;                  /*Can't initialize object                    */
hid_t    H5E_ALREADYINIT_g                = FAIL;               /*object already initialized                 */
hid_t    H5E_CANTRELEASE_g                = FAIL;               /*Can't release object                       */

hid_t    H5E_BADATOM_g            = FAIL;                   /*Can't find atom information                */
hid_t    H5E_BADGROUP_g           = FAIL;                  /*Can't find group information               */
hid_t    H5E_CANTREGISTER_g               = FAIL;              /*Can't register new atom                    */
hid_t    H5E_CANTINC_g            = FAIL;                   /*Can't increment reference count            */
hid_t    H5E_CANTDEC_g            = FAIL;                   /*Can't decrement reference count            */
hid_t    H5E_NOIDS_g              = FAIL;                     /*Out of IDs for group                       */

hid_t    H5E_CANTFLUSH_g          = FAIL;                 /*Can't flush object from cache              */
hid_t    H5E_CANTLOAD_g           = FAIL;                  /*Can't load object into cache               */
hid_t    H5E_PROTECT_g            = FAIL;                   /*protected object error                     */
hid_t    H5E_NOTCACHED_g          = FAIL;                 /*object not currently cached                */

hid_t    H5E_NOTFOUND_g           = FAIL;                  /*object not found                           */
hid_t    H5E_EXISTS_g             = FAIL;                    /*object already exists                      */
hid_t    H5E_CANTENCODE_g         = FAIL;                /*Can't encode value                         */
hid_t    H5E_CANTDECODE_g         = FAIL;                /*Can't decode value                         */
hid_t    H5E_CANTSPLIT_g          = FAIL;                 /*Can't split node                           */
hid_t    H5E_CANTINSERT_g         = FAIL;                /*Can't insert object                        */
hid_t    H5E_CANTLIST_g           = FAIL;                  /*Can't list node                            */

hid_t    H5E_LINKCOUNT_g          = FAIL;                 /*bad object header link count               */
hid_t    H5E_VERSION_g            = FAIL;                   /*wrong version number                       */
hid_t    H5E_ALIGNMENT_g          = FAIL;                 /*alignment error                            */
hid_t    H5E_BADMESG_g            = FAIL;                   /*unrecognized message                       */
hid_t    H5E_CANTDELETE_g         = FAIL;                /* Can't delete message                      */

hid_t    H5E_CANTOPENOBJ_g                = FAIL;               /*Can't open object                          */
hid_t    H5E_COMPLEN_g            = FAIL;                   /*name component is too long                 */
hid_t    H5E_CWG_g                = FAIL;                       /*problem with current working group         */
hid_t    H5E_LINK_g               = FAIL;                      /*link count failure                         */
hid_t    H5E_SLINK_g              = FAIL;			/*symbolic link error			     */

hid_t    H5E_CANTCONVERT_g                = FAIL;               /*Can't convert datatypes                    */
hid_t    H5E_BADSIZE_g            = FAIL;                   /*Bad size for object                        */

hid_t    H5E_CANTCLIP_g           = FAIL;                  /*Can't clip hyperslab region                */
hid_t    H5E_CANTCOUNT_g          = FAIL;                 /*Can't count elements                       */
hid_t    H5E_CANTSELECT_g         = FAIL;                /*Can't select hyperslab                     */
hid_t    H5E_CANTNEXT_g           = FAIL;                  /*Can't move to next iterator location       */
hid_t    H5E_BADSELECT_g          = FAIL;                 /*Invalid selection                          */
hid_t    H5E_CANTCOMPARE_g                = FAIL;               /*Can't compare objects                      */

hid_t    H5E_CANTGET_g            = FAIL;                   /*Can't get value                            */
hid_t    H5E_CANTSET_g            = FAIL;                   /*Can't set value                            */
hid_t    H5E_DUPCLASS_g           = FAIL;                  /*Duplicate class name in parent class */

hid_t    H5E_MPI_g                = FAIL;			/*some MPI function failed		     */
hid_t    H5E_MPIERRSTR_g          = FAIL;		        /*MPI Error String 			     */

hid_t    H5E_CANTMAKETREE_g               = FAIL;              /*can't make a TBBT tree                     */
hid_t    H5E_CANTRECV_g           = FAIL;                  /*can't receive messages from processes      */
hid_t    H5E_CANTSENDMDATA_g              = FAIL;             /*can't send metadata message                */
hid_t    H5E_CANTCHANGE_g         = FAIL;                /*can't register change on server            */
hid_t    H5E_CANTALLOC_g          = FAIL;                 /*can't allocate from file                   */

hid_t    H5E_NOFILTER_g           = FAIL;                  /*requested filter is not available          */
hid_t    H5E_CALLBACK_g           = FAIL;                  /*callback failed                            */
hid_t    H5E_CANAPPLY_g           = FAIL;                  /*error from filter "can apply" callback     */
hid_t    H5E_SETLOCAL_g           = FAIL;                  /*error from filter "set local" callback     */

/* Interface initialization? */
static int interface_initialize_g = 0;
#define INTERFACE_INIT H5E_init_interface

/*
 * Predefined errors. These are initialized at runtime in H5E_init_interface()
 * in this source file.
 *
 * If more of these are added, the new ones must be added to the list of
 * types to reset in H5E_term_interface().
 */
hid_t H5E_ERR_CLS_g			= FAIL;
#ifndef H5_HAVE_THREADSAFE
H5E_t		H5E_stack_g[1];
#endif /* H5_HAVE_THREADSAFE */

#ifdef H5_HAVE_PARALLEL
/*
 * variables used for MPI error reporting
 */
char	H5E_mpi_error_str[MPI_MAX_ERROR_STRING];
int	H5E_mpi_error_str_len;
#endif

/* Static function declarations */
static herr_t H5E_init_interface (void);
static int H5E_close_msg_cb(void *obj_ptr, hid_t obj_id, void *key);
static herr_t H5E_walk_cb(int n, H5E_error_t *err_desc, void *client_data);


#ifdef OLD_ERR /* Old codes, commented out */

#ifdef H5_HAVE_THREADSAFE
/*-------------------------------------------------------------------------
 * Function:	H5E_get_stack
 *
 * Purpose:	Support function for H5E_get_my_stack() to initialize and
 *              acquire per-thread error stack.
 *
 * Return:	Success:	error stack (H5E_t *)
 *
 *		Failure:	NULL
 *
 * Programmer:	Chee Wai LEE
 *              April 24, 2000
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5E_t *
H5E_get_stack(void)
{
    H5E_t *estack;
    H5E_t *ret_value;   /* Return value */

    FUNC_ENTER_NOAPI(H5E_get_stack,NULL);

    estack = pthread_getspecific(H5TS_errstk_key_g);
    if (!estack) {
        /* no associated value with current thread - create one */
        estack = (H5E_t *)H5MM_malloc(sizeof(H5E_t));
        pthread_setspecific(H5TS_errstk_key_g, (void *)estack);
    }

    /* Set return value */
    ret_value=estack;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}
#endif  /* H5_HAVE_THREADSAFE */
#endif /* OLD_ERR */

/*--------------------------------------------------------------------------
 * Function:    H5E_init_interface
 *
 * Purpose:     Initialize interface-specific information
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Raymond Lu
 *              Friday, July 11, 2003
 *               
 *--------------------------------------------------------------------------
 */
static herr_t
H5E_init_interface(void)
{
    herr_t          ret_value                = SUCCEED;   /* Return value */

    FUNC_ENTER_NOINIT(H5E_init_interface);

    /* Initialize the atom group for the error class IDs */
    H5I_init_group(H5I_ERROR_CLASS, H5I_ERRCLS_HASHSIZE, H5E_RESERVED_ATOMS, 
                    (H5I_free_t)H5E_unregister_class);
    /* Initialize the atom group for the major error IDs */
    H5I_init_group(H5I_ERROR_MSG, H5I_ERRMSG_HASHSIZE, H5E_RESERVED_ATOMS, 
                    (H5I_free_t)H5E_close_msg);
    /* Initialize the atom group for the error stacks */
    H5I_init_group(H5I_ERROR_STACK, H5I_ERRSTK_HASHSIZE, H5E_RESERVED_ATOMS, 
                    (H5I_free_t)H5E_close_stack);

#ifndef H5_HAVE_THREADSAFE
    H5E_stack_g[0].func = (H5E_auto_t)H5Eprint;
    H5E_stack_g[0].auto_data = stderr;
#endif /* H5_HAVE_THREADSAFE */

    H5E_ERR_CLS_g = H5E_register_class(H5E_CLS_NAME, H5E_CLS_LIB_NAME, H5E_CLS_LIB_VERS);

    H5E_NONE_MAJOR_g  = H5E_create_msg(H5E_ERR_CLS_g, H5E_MAJOR, H5E_NONE_MAJOR_MSG);
    H5E_ARGS_g        = H5E_create_msg(H5E_ERR_CLS_g, H5E_MAJOR, H5E_MAJ_ARGS_MSG);
    H5E_RESOURCE_g    = H5E_create_msg(H5E_ERR_CLS_g, H5E_MAJOR, H5E_MAJ_RESOURCE_MSG);
    H5E_INTERNAL_g    = H5E_create_msg(H5E_ERR_CLS_g, H5E_MAJOR, H5E_MAJ_INTERNAL_MSG);
    H5E_FILE_g        = H5E_create_msg(H5E_ERR_CLS_g, H5E_MAJOR, H5E_MAJ_FILE_MSG);

    H5E_IO_g          = H5E_create_msg(H5E_ERR_CLS_g, H5E_MAJOR, H5E_MAJ_IO_MSG);
    H5E_FUNC_g        = H5E_create_msg(H5E_ERR_CLS_g, H5E_MAJOR, H5E_MAJ_FUNC_MSG);
    H5E_ATOM_g        = H5E_create_msg(H5E_ERR_CLS_g, H5E_MAJOR, H5E_MAJ_ATOM_MSG);
    H5E_CACHE_g       = H5E_create_msg(H5E_ERR_CLS_g, H5E_MAJOR, H5E_MAJ_CACHE_MSG);
    H5E_BTREE_g       = H5E_create_msg(H5E_ERR_CLS_g, H5E_MAJOR, H5E_MAJ_BTREE_MSG);

    H5E_SYM_g         = H5E_create_msg(H5E_ERR_CLS_g, H5E_MAJOR, H5E_MAJ_SYM_MSG);
    H5E_HEAP_g        = H5E_create_msg(H5E_ERR_CLS_g, H5E_MAJOR, H5E_MAJ_HEAP_MSG);
    H5E_OHDR_g        = H5E_create_msg(H5E_ERR_CLS_g, H5E_MAJOR, H5E_MAJ_HEAP_MSG);
    H5E_DATATYPE_g    = H5E_create_msg(H5E_ERR_CLS_g, H5E_MAJOR, H5E_MAJ_DATATYPE_MSG);
    H5E_DATASPACE_g   = H5E_create_msg(H5E_ERR_CLS_g, H5E_MAJOR, H5E_MAJ_DATASPACE_MSG);

    H5E_DATASET_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MAJOR, H5E_MAJ_DATASET_MSG);
    H5E_STORAGE_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MAJOR, H5E_MAJ_STORAGE_MSG);
    H5E_PLIST_g       = H5E_create_msg(H5E_ERR_CLS_g, H5E_MAJOR, H5E_MAJ_PLIST_MSG);
    H5E_ATTR_g        = H5E_create_msg(H5E_ERR_CLS_g, H5E_MAJOR, H5E_MAJ_ATTR_MSG);
    H5E_PLINE_g       = H5E_create_msg(H5E_ERR_CLS_g, H5E_MAJOR, H5E_MAJ_PLINE_MSG);
    
    H5E_EFL_g         = H5E_create_msg(H5E_ERR_CLS_g, H5E_MAJOR, H5E_MAJ_EFL_MSG);
    H5E_REFERENCE_g   = H5E_create_msg(H5E_ERR_CLS_g, H5E_MAJOR, H5E_MAJ_REFERENCE_MSG);
    H5E_VFL_g         = H5E_create_msg(H5E_ERR_CLS_g, H5E_MAJOR, H5E_MAJ_VFL_MSG);
    H5E_TBBT_g        = H5E_create_msg(H5E_ERR_CLS_g, H5E_MAJOR, H5E_MAJ_TBBT_MSG);
    H5E_FPHDF5_g      = H5E_create_msg(H5E_ERR_CLS_g, H5E_MAJOR, H5E_MAJ_FPHDF5_MSG);

    H5E_TST_g         = H5E_create_msg(H5E_ERR_CLS_g, H5E_MAJOR, H5E_MAJ_TST_MSG);
    H5E_RS_g          = H5E_create_msg(H5E_ERR_CLS_g, H5E_MAJOR, H5E_MAJ_RS_MSG);
    H5E_ERROR_g       = H5E_create_msg(H5E_ERR_CLS_g, H5E_MAJOR, H5E_MAJ_ERROR_MSG);

    /* Minor errors */
    H5E_NONE_MINOR_g  = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_NONE_MINOR_MSG);
    H5E_UNINITIALIZED_g      = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_UNINITIALIZED_MSG);
    H5E_UNSUPPORTED_g        = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_UNSUPPORTED_MSG);
    H5E_BADTYPE_g      = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_BADTYPE_MSG);
    H5E_BADRANGE_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_BADRANGE_MSG);
    H5E_BADVALUE_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_BADVALUE_MSG);

    H5E_NOSPACE_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_NOSPACE_MSG);
    H5E_CANTCOPY_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTCOPY_MSG);
    H5E_CANTFREE_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTFREE_MSG);
    H5E_ALREADYEXISTS_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_ALREADYEXISTS_MSG);
    H5E_CANTLOCK_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTLOCK_MSG);
    H5E_CANTUNLOCK_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTUNLOCK_MSG);
    H5E_CANTGC_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTGC_MSG);
    
    H5E_FILEEXISTS_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_FILEEXISTS_MSG);
    H5E_FILEOPEN_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_FILEOPEN_MSG);
    H5E_CANTCREATE_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTCREATE_MSG);
    H5E_CANTOPENFILE_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTOPENFILE_MSG);
    H5E_CANTCLOSEFILE_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTCLOSEFILE_MSG);
    H5E_NOTHDF5_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_NOTHDF5_MSG);
    H5E_BADFILE_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_BADFILE_MSG);
    H5E_TRUNCATED_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_TRUNCATED_MSG);
    H5E_MOUNT_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_MOUNT_MSG);

    H5E_SEEKERROR_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_SEEKERROR_MSG);
    H5E_READERROR_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_READERROR_MSG);
    H5E_WRITEERROR_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_WRITEERROR_MSG);
    H5E_CLOSEERROR_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CLOSEERROR_MSG);

    H5E_CANTINIT_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTINIT_MSG);
    H5E_ALREADYINIT_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_ALREADYINIT_MSG);
    H5E_CANTRELEASE_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTRELEASE_MSG);
    
    H5E_BADATOM_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_BADATOM_MSG);
    H5E_BADGROUP_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_BADGROUP_MSG);
    H5E_CANTREGISTER_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTREGISTER_MSG);
    H5E_CANTINC_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTINC_MSG);
    H5E_CANTDEC_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTDEC_MSG);
    H5E_NOIDS_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_NOIDS_MSG);
    
    H5E_CANTFLUSH_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTFLUSH_MSG);
    H5E_CANTLOAD_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTLOAD_MSG);
    H5E_PROTECT_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_PROTECT_MSG);
    H5E_NOTCACHED_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_NOTCACHED_MSG);
    
    H5E_NOTFOUND_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_NOTFOUND_MSG);
    H5E_EXISTS_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_EXISTS_MSG);
    H5E_CANTENCODE_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTENCODE_MSG);
    H5E_CANTDECODE_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTDECODE_MSG);
    H5E_CANTSPLIT_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTSPLIT_MSG);
    H5E_CANTINSERT_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTINSERT_MSG);
    H5E_CANTLIST_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTLIST_MSG);
    
    H5E_LINKCOUNT_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_LINKCOUNT_MSG);
    H5E_VERSION_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_VERSION_MSG);
    H5E_ALIGNMENT_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_ALIGNMENT_MSG);
    H5E_BADMESG_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_BADMESG_MSG);
    H5E_CANTDELETE_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTDELETE_MSG);
    
    H5E_CANTOPENOBJ_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTOPENOBJ_MSG);
    H5E_COMPLEN_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_COMPLEN_MSG);
    H5E_CWG_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CWG_MSG);
    H5E_LINK_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_LINK_MSG);
    H5E_SLINK_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_SLINK_MSG);
    
    H5E_CANTCONVERT_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTCONVERT_MSG);
    H5E_BADSIZE_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_BADSIZE_MSG);
    
    H5E_CANTCLIP_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTCLIP_MSG);
    H5E_CANTCOUNT_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTCOUNT_MSG);
    H5E_CANTSELECT_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTSELECT_MSG);
    H5E_CANTNEXT_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTNEXT_MSG);
    H5E_BADSELECT_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_BADSELECT_MSG);
    H5E_CANTCOMPARE_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTCOMPARE_MSG);
    
    H5E_CANTGET_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTGET_MSG);
    H5E_CANTSET_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTSET_MSG);
    H5E_DUPCLASS_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_DUPCLASS_MSG);

    H5E_MPI_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_MPI_MSG);
    H5E_MPIERRSTR_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_MPIERRSTR_MSG);
    
    H5E_CANTMAKETREE_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTMAKETREE_MSG);
    H5E_CANTRECV_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTRECV_MSG);
    H5E_CANTSENDMDATA_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTSENDMDATA_MSG);
    H5E_CANTCHANGE_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTCHANGE_MSG);
    H5E_CANTALLOC_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANTALLOC_MSG);

    H5E_NOFILTER_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_NOFILTER_MSG);
    H5E_CALLBACK_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CALLBACK_MSG);
    H5E_CANAPPLY_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_CANAPPLY_MSG);
    H5E_SETLOCAL_g     = H5E_create_msg(H5E_ERR_CLS_g, H5E_MINOR, H5E_MIN_SETLOCAL_MSG);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5E_term_interface
 *
 * Purpose:	Terminates the H5E interface
 *
 * Return:	Success:	Positive if anything is done that might
 *				affect other interfaces; zero otherwise.
 *
 * 		Failure:	Negative.
 *
 * Programmer:	Raymond Lu
 *	        Tuesday, July 22, 2003	
 *
 * Modifications:
 *              
 *-------------------------------------------------------------------------
 */
int
H5E_term_interface(void)
{
    int	        ncls, nmsg, nstk, n=0;
    H5E_t       *estack;

    FUNC_ENTER_NOINIT(H5E_term_interface);

    if (interface_initialize_g) {
#ifdef H5_HAVE_THREADSAFE
        /* Free default error stack here? */ 
        estack = pthread_getspecific(H5TS_errstk_key_g);
        if (estack) {
            H5MM_free(estack);
        }
#endif
    
        /* Check if there are any open property list classes or lists */
        ncls = H5I_nmembers(H5I_ERROR_CLASS);
        nmsg = H5I_nmembers(H5I_ERROR_MSG);
        nstk = H5I_nmembers(H5I_ERROR_STACK);

        n = ncls + nmsg + nstk;
        if(n>0) {
	    if (nmsg>0)
	        H5I_clear_group(H5I_ERROR_MSG, FALSE);

	    if (ncls>0) {
	        H5I_clear_group(H5I_ERROR_CLASS, FALSE);
                
                /* Reset the error class, if they've been closed */
                if(H5I_nmembers(H5I_ERROR_CLASS)==0)
                        H5E_ERR_CLS_g = -1;
            }
            
            if (nstk>0)
	        H5I_clear_group(H5I_ERROR_STACK, FALSE);
        
	} else {
	    /* Destroy the error class, message, and stack id groups */
	    H5I_destroy_group(H5I_ERROR_CLASS);
	    H5I_destroy_group(H5I_ERROR_MSG);
	    H5I_destroy_group(H5I_ERROR_STACK);

	    /* Mark closed */
	    interface_initialize_g = 0;
	    n = 1; /*H5I*/
	}
    }

    FUNC_LEAVE_NOAPI(n);
}


/*-------------------------------------------------------------------------
 * Function:	H5Eregister_class
 *
 * Purpose:	Registers an error class.
 *
 * Return:	Non-negative value as class ID on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Eregister_class(const char *cls_name, const char *lib_name, const char *version)
{
    hid_t       ret_value;   /* Return value */

    FUNC_ENTER_API(H5Eregister_class, FAIL);
    H5TRACE3("i","sss",cls_name,lib_name,version);
    
    /* Add HGOTO_ERROR later */
    ret_value=H5E_register_class(cls_name, lib_name, version);

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5E_register_class
 *
 * Purpose:	Private function to register an error class.
 *
 * Return:	Non-negative value as class ID on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5E_register_class(const char *cls_name, const char *lib_name, const char *version)
{
    hid_t       ret_value;   /* Return value */
    H5E_cls_t   *cls;
    
    FUNC_ENTER_NOAPI(H5E_register_class, FAIL);
    
    /* Check arguments */
    assert(cls_name);
    assert(lib_name);
    assert(version);

    /* Need to check for failures from malloc & strdup */
    cls = H5MM_malloc(sizeof(H5E_cls_t));

    cls->cls_name = HDstrdup(cls_name);
    cls->lib_name = HDstrdup(lib_name);
    cls->lib_vers = HDstrdup(version);

    /* Register the new error class to get an ID for it */
    /* Need to check for error */
    ret_value = H5I_register(H5I_ERROR_CLASS, cls);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Eunregister_class
 *
 * Purpose:	Closes an error class.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Eunregister_class(hid_t class_id)
{
    herr_t       ret_value = SUCCEED;   /* Return value */
    H5E_cls_t    *cls;

    FUNC_ENTER_API(H5Eunregister_class, FAIL);
    H5TRACE1("e","i",class_id);
    
    /* Need to check for errors */
    cls = H5I_object_verify(class_id, H5I_ERROR_CLASS);

    /*
     * Decrement the counter on the dataset.  It will be freed if the count
     * reaches zero.
     */
    /* Need to check for errors */
    H5I_dec_ref(class_id);

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5E_unregister_class
 *
 * Purpose:	Private function to close an error class.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5E_unregister_class(H5E_cls_t *cls)
{
    herr_t       ret_value = SUCCEED;   /* Return value */
    
    FUNC_ENTER_NOAPI(H5E_unregister_class, FAIL);

    H5I_search(H5I_ERROR_MSG, H5E_close_msg_cb, cls);

    if(cls) {
        if(cls->cls_name)    
            H5MM_xfree((void*)cls->cls_name);
        if(cls->lib_name)
            H5MM_xfree((void*)cls->lib_name);
        if(cls->lib_vers)
            H5MM_xfree((void*)cls->lib_vers);
        H5MM_xfree((void*)cls);
    }
    
done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Eget_class_name
 *
 * Purpose:	Retrieves error class name.
 *
 * Return:      Non-negative for name length if succeeds(zero means no name);
 *              otherwise returns negative value.	
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5Eget_class_name(hid_t class_id, char *name, size_t size)
{
    ssize_t      ret_value;   /* Return value */
    H5E_cls_t    *cls;

    FUNC_ENTER_API(H5Eget_class_name, FAIL);
    H5TRACE3("Zs","isz",class_id,name,size);
    
    /* Need to check for errors */
    cls = H5I_object_verify(class_id, H5I_ERROR_CLASS);

    ret_value = H5E_get_class_name(cls, name, size);

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5E_get_class_name
 *
 * Purpose:	Private function to retrieve error class name.
 * 
 * Return:      Non-negative for name length if succeeds(zero means no name);
 *              otherwise returns negative value.	
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5E_get_class_name(H5E_cls_t *cls, char *name, size_t size)
{
    ssize_t       ret_value;   /* Return value */
    ssize_t       len = FAIL;
    
    FUNC_ENTER_NOAPI(H5E_get_class_name, FAIL);

    if(cls->cls_name)
        len = (ssize_t)HDstrlen(cls->cls_name);
    else
        len = 0;

    if(name) {
       HDstrncpy(name, cls->cls_name, MIN(len+1, (ssize_t)size));
       if(len >= (ssize_t)size)
          name[size-1]='\0';
    } 
    
    ret_value = len;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}



/*-------------------------------------------------------------------------
 * Function:    H5E_close_msg_cb
 *
 * Purpose:     H5I_search callback function to close error messages in the
 *              error class.
 *
 * Programmer:  Raymond Lu
 *              July 14, 2003
 *              
 * Return:	Non-negative value on success/Negative on failure
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
H5E_close_msg_cb(void *obj_ptr, hid_t obj_id, void *key)
{
    herr_t      ret_value = SUCCEED;       /* Return value */
    H5E_msg_t   *err_msg = (H5E_msg_t*)obj_ptr;
    H5E_cls_t   *cls = (H5E_cls_t*)key;
        
    FUNC_ENTER_NOAPI(H5_close_msg_cb, FAIL);
  
    assert(obj_ptr);

    if(err_msg->cls == cls)
        H5E_close_msg(err_msg);
    
 done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Eclose_msg
 *
 * Purpose:	Closes a major or minor error.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Eclose_msg(hid_t err_id)
{
    herr_t       ret_value = SUCCEED;   /* Return value */
    H5E_msg_t    *err;
    
    FUNC_ENTER_API(H5Eclose_msg, FAIL);
    H5TRACE1("e","i",err_id);

    /* Need to check for errors */
    err = H5I_object_verify(err_id, H5I_ERROR_MSG);
    
    /* Decrement the counter.  It will be freed if the count reaches zero. */
    H5I_dec_ref(err_id);

done:
    FUNC_LEAVE_API(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:	H5E_close_msg
 *
 * Purpose:	Private function to close an error messge.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5E_close_msg(H5E_msg_t *err)
{
    herr_t       ret_value = SUCCEED;   /* Return value */
    
    FUNC_ENTER_NOAPI(H5E_close_msg, FAIL);

    /* Doesn't free err->cls here */
    if(err) {
        if(err->msg)    
            H5MM_xfree((void*)err->msg);
        H5MM_xfree((void*)err);
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:	H5Ecreate_msg
 *
 * Purpose:	Creates a major or minor error, returns an ID.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Ecreate_msg(hid_t cls, H5E_type_t msg_type, const char *msg)
{
    hid_t       ret_value;   /* Return value */
    
    FUNC_ENTER_API(H5Ecreate_msg, FAIL);
    
    ret_value = H5E_create_msg(cls, msg_type, msg);

done:
    FUNC_LEAVE_API(ret_value);
}

/*-------------------------------------------------------------------------
 * Function:	H5E_create_msg
 *
 * Purpose:	Private function to create a major or minor error.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5E_create_msg(hid_t cls_id, H5E_type_t msg_type, const char *msg)
{
    hid_t       ret_value;   /* Return value */
    H5E_msg_t   *msg_ptr;
    
    FUNC_ENTER_NOAPI(H5E_create_msg, FAIL);
   
    /* Check arguments */
    assert(msg);

    /* Need to check for failures from malloc & strdup */
    msg_ptr = H5MM_malloc(sizeof(H5E_msg_t));

    msg_ptr->cls = H5I_object_verify(cls_id, H5I_ERROR_CLASS);
    msg_ptr->type = msg_type;
    msg_ptr->msg = HDstrdup(msg);

    /* Register the new error message to get an ID for it */
    /* Need to check for error */
    ret_value = H5I_register(H5I_ERROR_MSG, msg_ptr);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Eget_msg
 *
 * Purpose:	Retrieves an error message.
 *
 * Return:      Non-negative for message length if succeeds(zero means no message);
 *              otherwise returns negative value.	
 *
 * Programmer:	Raymond Lu
 *              Friday, July 14, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5Eget_msg(hid_t msg_id, H5E_type_t *type, char *msg, size_t size)
{
    ssize_t      ret_value;   /* Return value */
    H5E_msg_t    *msg_ptr;

    FUNC_ENTER_API(H5Eget_msg, FAIL);
    
    /* Need to check for errors */
    msg_ptr = H5I_object_verify(msg_id, H5I_ERROR_MSG);

    ret_value = H5E_get_msg(msg_ptr, type, msg, size);

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5E_get_msg
 *
 * Purpose:	Private function to retrieve an error message.
 * 
 * Return:      Non-negative for name length if succeeds(zero means no name);
 *              otherwise returns negative value.	
 *
 * Programmer:	Raymond Lu
 *              Friday, July 14, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5E_get_msg(H5E_msg_t *msg_ptr, H5E_type_t *type, char *msg, size_t size)
{
    ssize_t       ret_value;   /* Return value */
    ssize_t       len;
    
    FUNC_ENTER_NOAPI(H5E_get_msg, FAIL);

    len = HDstrlen(msg_ptr->msg);

    if(msg) {
       HDstrncpy(msg, msg_ptr->msg, MIN(len+1, size));
       if(len >= (ssize_t)size)
          msg[size-1]='\0';
    } 
    
    if(type)
        *type = msg_ptr->type;

    ret_value = len;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


#ifdef H5_HAVE_THREADSAFE
/*-------------------------------------------------------------------------
 * Function:	H5E_get_stack
 *
 * Purpose:	Support function for H5E_get_my_stack() to initialize and
 *              acquire per-thread error stack.
 *
 * Return:	Success:	error stack (H5E_t *)
 *
 *		Failure:	NULL
 *
 * Programmer:	Chee Wai LEE
 *              April 24, 2000
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5E_t *
H5E_get_stack(void)
{
    H5E_t *estack, *tmp;
    H5E_t *ret_value;   /* Return value */

    FUNC_ENTER_NOAPI(H5E_get_stack,NULL);

    estack = pthread_getspecific(H5TS_errstk_key_g);

    if (!estack) {
        /* no associated value with current thread - create one */
        /* Where is it freed? */
        estack = (H5E_t *)H5MM_calloc(sizeof(H5E_t));
        estack->func = H5Eprint;
        estack->auto_data = stderr;
        pthread_setspecific(H5TS_errstk_key_g, (void *)estack);
    }

    /* Set return value */
    ret_value=estack;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}
#endif  /* H5_HAVE_THREADSAFE */


/*-------------------------------------------------------------------------
 * Function:	H5Eget_current_stack
 *
 * Purpose:	Registers current error stack, returns object handle for it,
 *              clears it.
 *
 * Return:	Non-negative value as stack ID on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 14, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Eget_current_stack(void)
{
    hid_t       ret_value;   /* Return value */

    FUNC_ENTER_API(H5Eget_current_stack, FAIL);
    H5TRACE0("i","");
    
    /* Add HGOTO_ERROR later */
    ret_value=H5E_get_current_stack();

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5E_get_current_stack
 *
 * Purpose:	Private function to register an error stack.
 *
 * Return:	Non-negative value as class ID on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5E_get_current_stack(void)
{
    hid_t       ret_value;   /* Return value */
    H5E_t	*current_stack = H5E_get_my_stack ();
    H5E_t   *estack_copy = H5MM_malloc(sizeof(H5E_t));
    H5E_error_t     *current_error, *new_error;
    int         i;
    
    FUNC_ENTER_NOAPI(H5E_get_current_stack, FAIL);

    /* Make a copy of current error stack */
    estack_copy->nused = current_stack->nused; 
    for(i=0; i<current_stack->nused; i++) {
        current_error = &(current_stack->slot[i]);
        new_error = &(estack_copy->slot[i]);
       
        /* Should we make copies of these IDs? */ 
        new_error->cls_id = current_error->cls_id;       
        new_error->maj_id = current_error->maj_id;       
        new_error->min_id = current_error->min_id;       
        new_error->func_name = HDstrdup(current_error->func_name);       
        new_error->file_name = HDstrdup(current_error->file_name);       
        new_error->line = current_error->line;
        new_error->desc = HDstrdup(current_error->desc);       
    }
   
    /* Empty current error stack */ 
    for(i=0; i<current_stack->nused; i++) {
        current_error = &(current_stack->slot[i]);
        if(current_error->func_name)
            H5MM_xfree((void*)current_error->func_name);
        if(current_error->file_name)
            H5MM_xfree((void*)current_error->file_name);
        if(current_error->desc)
            H5MM_xfree((void*)current_error->desc);
    }
    HDmemset(current_stack->slot, 0, sizeof(H5E_error_t)*current_stack->nused);
    current_stack->nused = 0;
   
    /* Register the error stack to get an ID for it */
    /* Need to check for error */
    ret_value = H5I_register(H5I_ERROR_STACK, estack_copy);

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Eset_current_stack
 *
 * Purpose:     Replaces current stack with specified stack.	
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 15, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Eset_current_stack(hid_t err_stack_id)
{
    herr_t         ret_value = SUCCEED;   /* Return value */
    H5E_t      *estack;
    
    FUNC_ENTER_API(H5Eset_current_stack, FAIL);
    H5TRACE1("e","i",err_stack_id);
    
    /* Need to check for errors */
    if(err_stack_id == H5E_DEFAULT)
        goto done; /*HGOTO_DONE(SUCCEED);*/
    else
        estack = H5I_object_verify(err_stack_id, H5I_ERROR_STACK);

    /* Add HGOTO_ERROR later */
    ret_value=H5E_set_current_stack(estack);

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5E_set_current_stack
 *
 * Purpose:	Private function to replace an error stack.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 15, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5E_set_current_stack(H5E_t *estack)
{
    herr_t       ret_value = SUCCEED;   /* Return value */
    H5E_t	*current_stack = H5E_get_my_stack();
    H5E_error_t     *current_error, *new_error;
    int         i;
    
    FUNC_ENTER_NOAPI(H5E_get_current_stack, FAIL);

    /* Empty current error stack */ 
    for(i=0; i<current_stack->nused; i++) {
        current_error = &(current_stack->slot[i]);
        if(current_error->func_name)
            H5MM_xfree((void*)current_error->func_name);
        if(current_error->file_name)
            H5MM_xfree((void*)current_error->file_name);
        if(current_error->desc)
            H5MM_xfree((void*)current_error->desc);
    }
    HDmemset(current_stack->slot, 0, sizeof(H5E_error_t)*current_stack->nused);
    current_stack->nused = 0;

    /* Copy new stack to current error stack */
    current_stack->nused = estack->nused; 
    for(i=0; i<current_stack->nused; i++) {
        current_error = &(current_stack->slot[i]);
        new_error = &(estack->slot[i]);
       
        /* Should we make copies of these IDs? */ 
        current_error->cls_id = new_error->cls_id;
        current_error->maj_id = new_error->maj_id;
        current_error->min_id = new_error->min_id;
        current_error->func_name = HDstrdup(new_error->func_name);       
        current_error->file_name = HDstrdup(new_error->file_name);       
        current_error->line = new_error->line;
        current_error->desc = HDstrdup(new_error->desc);       
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Eclose_stack
 *
 * Purpose:	Closes an error stack.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 14, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Eclose_stack(hid_t stack_id)
{
    herr_t       ret_value = SUCCEED;   /* Return value */
    H5E_t    *err_stack;

    FUNC_ENTER_API(H5Eclose_stack, FAIL);
    H5TRACE1("e","i",stack_id);
/*HDfprintf(stderr, "H5Eclose_stack is called\n");*/
    /* Add HGOTO_ERROR later */
    if(H5E_DEFAULT == stack_id)
        ;
    
    /* Need to check for errors */
    err_stack = H5I_object_verify(stack_id, H5I_ERROR_STACK);

    /*
     * Decrement the counter on the dataset.  It will be freed if the count
     * reaches zero.
     */
    /* Need to check for errors */
    H5I_dec_ref(stack_id);

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5E_close_stack
 *
 * Purpose:	Private function to close an error stack.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 14, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5E_close_stack(H5E_t *err_stack)
{
    herr_t       ret_value = SUCCEED;   /* Return value */
    H5E_error_t     *error;
    int          i;

    FUNC_ENTER_NOAPI(H5E_close_stack, FAIL);
/*HDfprintf(stderr, "H5E_close_stack is called\n");*/
    
    if(err_stack) {
        for(i=0; i<err_stack->nused; i++) {
            error = &(err_stack->slot[i]);

            if(error->func_name)
                H5MM_xfree((void*)error->func_name);
            if(error->file_name)
                H5MM_xfree((void*)error->file_name);
            if(error->desc)
                H5MM_xfree((void*)error->desc);
        }
        H5MM_xfree((void*)err_stack);
    }
    
done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Eget_num
 *
 * Purpose:	Retrieves the number of error message.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 15, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5Eget_num(hid_t error_stack_id)
{
    int       ret_value;   /* Return value */
    H5E_t *estack;

    FUNC_ENTER_API(H5Eget_num, FAIL);
    H5TRACE1("Is","i",error_stack_id);
   
    /* Need to check for errors */
    if(error_stack_id == H5E_DEFAULT)
    	estack = H5E_get_my_stack();
    else
        estack = H5I_object_verify(error_stack_id, H5I_ERROR_STACK);
    
    /* Add HGOTO_ERROR later */
    ret_value=H5E_get_num(estack);

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5E_get_num
 *
 * Purpose:	Private function to retrieve number of errors in error stack.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 15, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5E_get_num(H5E_t *err_stack)
{
    int      ret_value;   /* Return value */

    FUNC_ENTER_NOAPI(H5E_get_num, FAIL);
 
    assert(err_stack);   
    ret_value = err_stack->nused;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Epop
 *
 * Purpose:	Deletes some error messages from the top of error stack.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 16, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Epop(hid_t err_stack, size_t count)
{
    herr_t       ret_value = SUCCEED;   /* Return value */
    H5E_t *estack;

    FUNC_ENTER_API(H5Epop, FAIL);
    H5TRACE2("e","iz",err_stack,count);
   
    /* Need to check for errors */
    if(err_stack == H5E_DEFAULT)
    	estack = H5E_get_my_stack();
    else
        estack = H5I_object_verify(err_stack, H5I_ERROR_STACK);

    if(count > estack->nused)
        count = estack->nused;

    /* Add HGOTO_ERROR later */
    ret_value=H5E_pop(estack, count);

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5E_pop
 *
 * Purpose:	Private function to delete some error messages from the top
 *              of error stack.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 16, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5E_pop(H5E_t *err_stack, size_t count)
{
    herr_t      ret_value = SUCCEED;   /* Return value */
    H5E_error_t     *old_head, *new_head, *delete_head;
    size_t      delete_count;    
    int         i;
    
    FUNC_ENTER_NOAPI(H5E_pop, FAIL);

    assert(err_stack);  

    /* Do an in-place move.  Shift the remaining errors to the top of stack. */
    if(count != err_stack->nused) { 
        old_head = &(err_stack->slot[0]); 
        new_head = &(err_stack->slot[count]);

        /* Free memory for the errors to be deleted */
        for(i=0; i<count; i++) {
            H5E_error_t *error = &(err_stack->slot[i]);
            if(error->func_name)
                H5MM_xfree((void*)error->func_name);
            if(error->file_name)
                H5MM_xfree((void*)error->file_name);
            if(error->desc)
                H5MM_xfree((void*)error->desc);
        }
       
        /* Move the rest errors to the top of stack. Watch out: func_name, file_name, desc in new slot 
         * each points to the same location as the old slot.  Do not free them when deleting the old
         * slot. */ 
        HDmemmove(old_head, new_head, (err_stack->nused-count)*sizeof(H5E_error_t));

        /* Point to the beginning of errors to be removed, delete the old moved slots. */
        delete_head = &(err_stack->slot[err_stack->nused-count]);
        delete_count = count;
        HDmemset(delete_head, 0, delete_count*sizeof(H5E_error_t));

        err_stack->nused = err_stack->nused - count;   
    } else  {
        H5E_clear(err_stack);
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Epush
 *
 * Purpose:	Pushes a new error record onto error stack for the current
 *		thread.  The error has major and minor IDs MAJ_ID and
 *		MIN_ID, the name of a function where the error was detected,
 *		the name of the file where the error was detected, the
 *		line within that file, and an error description string.  The
 *		function name, file name, and error description strings must
 *		be statically allocated.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Monday, October 18, 1999
 *
 * Notes: 	Basically a public API wrapper around the H5E_push function.
 *
 * Modifications:
 *              Raymond Lu
 *              Tuesday, July 15, 2003

 *              Added the ID of the error stack to which the error is pushed
 *              on.  The error message can be appended more message in the
 *              same control format as printf and fprintf.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Epush(hid_t err_stack, const char *file, const char *func, unsigned line, 
        hid_t maj_id, hid_t min_id, const char *fmt, ...)
{
    herr_t	ret_value;
    H5E_t   *estack_ptr;
    H5E_msg_t   *maj_ptr, *min_ptr;
    va_list     ap;
    hid_t       cls_id;
    char        tmp[128];  /* What's the maximal length? */

    FUNC_ENTER_API(H5Epush, FAIL);
    H5TRACE7("e","issIuiis",err_stack,file,func,line,maj_id,min_id,fmt);
    
    /* Need to check for errors */
    if(err_stack == H5E_DEFAULT)
    	estack_ptr = H5E_get_my_stack();
    else
        estack_ptr = H5I_object_verify(err_stack, H5I_ERROR_STACK);
    
    maj_ptr = H5I_object_verify(maj_id, H5I_ERROR_MSG);
    min_ptr = H5I_object_verify(min_id, H5I_ERROR_MSG);
    /* Error check later */
    if(maj_ptr->cls != min_ptr->cls)
        ;
    cls_id = H5I_register(H5I_ERROR_CLASS, maj_ptr->cls);

    va_start(ap, fmt);
    HDvsnprintf(tmp, H5E_LEN, fmt, ap);
    va_end(ap);

    /* Should we make copies of maj_idm and min_id? */
    ret_value = H5E_push(estack_ptr, file, func, line, cls_id, maj_id, min_id, tmp);

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5E_push
 *
 * Purpose:	Pushes a new error record onto error stack for the current
 *		thread.  The error has major and minor IDs MAJ_ID and
 *		MIN_ID, the name of a function where the error was detected,
 *		the name of the file where the error was detected, the
 *		line within that file, and an error description string.  The
 *		function name, file name, and error description strings must
 *		be statically allocated (the FUNC_ENTER() macro takes care of
 *		the function name and file name automatically, but the
 *		programmer is responsible for the description string).
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Friday, December 12, 1997
 *
 * Modifications: 
 *              Raymond Lu
 *              Tuesday, July 15, 2003
 *               
 *              Added the ID of the error stack to which the error is pushed
 *              on.  The error message can be appended more message in the
 *              same control format as printf and fprintf.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5E_push(H5E_t *estack, const char *file, const char *func, unsigned line, 
        hid_t cls_id, hid_t maj_id, hid_t min_id, const char *desc)
{
    /*
     * WARNING: We cannot call HERROR() from within this function or else we
     *		could enter infinite recursion.  Furthermore, we also cannot
     *		call any other HDF5 macro or function which might call
     *		HERROR().  HERROR() is called by HRETURN_ERROR() which could
     *		be called by FUNC_ENTER().
     */
    FUNC_ENTER_NOINIT(H5E_push);

    /*
     * Don't fail if arguments are bad.  Instead, substitute some default
     * value.
     */
    if (!func) func = "Unknown_Function";
    if (!file) file = "Unknown_File";
    if (!desc) desc = "No description given";

    /*
     * Push the error if there's room.  Otherwise just forget it.
     */
    assert (estack);

    if (estack->nused<H5E_NSLOTS) {
	estack->slot[estack->nused].cls_id = cls_id;
	estack->slot[estack->nused].maj_id = maj_id;
	estack->slot[estack->nused].min_id = min_id;
	estack->slot[estack->nused].func_name = HDstrdup(func);
	estack->slot[estack->nused].file_name = HDstrdup(file);
	estack->slot[estack->nused].line = line;
	estack->slot[estack->nused].desc = HDstrdup(desc);
	estack->nused++;
    }

    FUNC_LEAVE_NOAPI(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Eclear
 *
 * Purpose:	Clears the error stack for the specified error stack.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Wednesday, July 16, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Eclear(hid_t err_stack)
{
    herr_t ret_value=SUCCEED;   /* Return value */
    H5E_t   *estack_ptr;
    
    FUNC_ENTER_API(H5Eclear, FAIL);
    H5TRACE1("e","i",err_stack);
    /* FUNC_ENTER() does all the work */

    /* Need to check for errors */
    if(err_stack == H5E_DEFAULT)
    	estack_ptr = H5E_get_my_stack();
    else
        estack_ptr = H5I_object_verify(err_stack, H5I_ERROR_STACK);
 
    ret_value = H5E_clear(estack_ptr);

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5E_clear
 *
 * Purpose:	Private function to clear the error stack for the
 *              specified error stack.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Wednesday, July 16, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5E_clear(H5E_t *estack)
{
    herr_t ret_value=SUCCEED;   /* Return value */
    H5E_error_t         *error;
    int                 i;

    FUNC_ENTER_NOAPI(H5E_clear, FAIL);

    /* Empty the error stack */ 
    if (estack) {
        for(i=0; i<estack->nused; i++) {
            error = &(estack->slot[i]);
            if(error->func_name)
                H5MM_xfree((void*)error->func_name);
            if(error->file_name)
                H5MM_xfree((void*)error->file_name);
            if(error->desc)
                H5MM_xfree((void*)error->desc);
        }
        HDmemset(estack->slot, 0, sizeof(H5E_error_t)*estack->nused);
        estack->nused = 0;
    }
    
done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Eprint
 *
 * Purpose:	Prints the error stack in some default way.  This is just a
 *		convenience function for H5Ewalk() with a function that
 *		prints error messages.  Users are encouraged to write there
 *		own more specific error handlers.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Friday, February 27, 1998
 *
 * Modifications:
 *	Albert Cheng, 2000/12/02
 *	Show MPI process rank id if applicable.
 *	Albert Cheng, 2001/07/14
 *	Show HDF5 library version information string too.
 *
 *      Raymond Lu, 2003/7/16
 *      Print for specified error stack.  A line will appear before the error
 *      messages of each error class.  It states the information of library
 *      name, version number and thread ID.
 *      
 *-------------------------------------------------------------------------
 */
herr_t
H5Eprint(hid_t err_stack, FILE *stream)
{
    herr_t ret_value=SUCCEED;   /* Return value */
    H5E_t   *estack_ptr;
    
    /* Don't clear the error stack! :-) */
    FUNC_ENTER_API_NOCLEAR(H5Eprint, FAIL);
    /*NO TRACE*/

    /* Need to check for errors */
    if(err_stack == H5E_DEFAULT)
    	estack_ptr = H5E_get_my_stack();
    else
        estack_ptr = H5I_object_verify(err_stack, H5I_ERROR_STACK);
 
    ret_value = H5E_print(estack_ptr, stream);

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5E_print
 *
 * Purpose:	Private function to print the error stack in some default 
 *              way.  This is just a convenience function for H5Ewalk() 
 *              with a function that prints error messages.  Users are 
 *              encouraged to write there own more specific error handlers.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Friday, February 27, 1998
 *
 * Modifications:
 *	Albert Cheng, 2000/12/02
 *	Show MPI process rank id if applicable.
 *	Albert Cheng, 2001/07/14
 *	Show HDF5 library version information string too.
 *
 *      Raymond Lu, 2003/7/16
 *      Print for specified error stack.  A line will appear before the error
 *      messages of each error class.  It states the information of library
 *      name, version number and thread ID.
 *      
 *-------------------------------------------------------------------------
 */
herr_t
H5E_print(H5E_t *estack, FILE *stream)
{
    herr_t	ret_value = SUCCEED;
    H5E_print_t eprint;
    H5E_cls_t   origin_cls={"Unknown class", "Unknown library", "Unknown library version"};

    /* Don't clear the error stack! :-) */
    FUNC_ENTER_NOAPI(H5E_print, FAIL);
    /*NO TRACE*/
    
    assert(estack);
    if (!stream) 
        eprint.stream = stderr;
    else
        eprint.stream = stream;

    eprint.cls.cls_name = origin_cls.cls_name;
    eprint.cls.lib_name = origin_cls.lib_name;
    eprint.cls.lib_vers = origin_cls.lib_vers; 
    
    ret_value = H5E_walk (estack, H5E_WALK_DOWNWARD, H5E_walk_cb, (void*)&eprint);
  
done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Ewalk
 *
 * Purpose:	Walks the error stack for the current thread and calls some
 *		function for each error along the way.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Friday, February 27, 1998
 *
 * Modifications:
 *
 *              Raymond Lu
 *              Wednesday, July 16, 2003
 *              Let it walk through specified error stack.
 *              
 *-------------------------------------------------------------------------
 */
herr_t
H5Ewalk(hid_t err_stack, H5E_direction_t direction, H5E_walk_t func, void *client_data)
{
    H5E_t   *estack_ptr;
    herr_t	ret_value;

    /* Don't clear the error stack! :-) */
    FUNC_ENTER_API_NOCLEAR(H5Ewalk, FAIL);
    H5TRACE4("e","iEdxx",err_stack,direction,func,client_data);

    /* Need to check for errors */
    if(err_stack == H5E_DEFAULT)
    	estack_ptr = H5E_get_my_stack();
    else
        estack_ptr = H5I_object_verify(err_stack, H5I_ERROR_STACK);

    ret_value = H5E_walk (estack_ptr, direction, func, client_data);

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5E_walk
 *
 * Purpose:	Private function for H5Ewalk.
 *              Walks the error stack, calling the specified function for
 *		each error on the stack.  The DIRECTION argument determines
 *		whether the stack is walked from the inside out or the
 *		outside in.  The value H5E_WALK_UPWARD means begin with the
 *		most specific error and end at the API; H5E_WALK_DOWNWARD
 *		means to start at the API and end at the inner-most function
 *		where the error was first detected.
 *
 *		The function pointed to by FUNC will be called for each error
 *		in the error stack. It's arguments will include an index
 *		number (beginning at zero regardless of stack traversal
 *		direction), an error stack entry, and the CLIENT_DATA pointer
 *		passed to H5E_print.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Friday, December 12, 1997
 *
 * Modifications:
 *
 *              Raymond Lu
 *              Wednesday, July 16, 2003
 *              Let it walk through specified error stack.
 *              
 *-------------------------------------------------------------------------
 */
herr_t
H5E_walk (H5E_t *estack, H5E_direction_t direction, H5E_walk_t func, void *client_data)
{
    int		i;
    herr_t	status;
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5E_walk, FAIL);

    /* check args, but rather than failing use some default value */
    if (direction!=H5E_WALK_UPWARD && direction!=H5E_WALK_DOWNWARD) {
	direction = H5E_WALK_UPWARD;
    }

    /* walk the stack */
    assert (estack);
    
/*for(i=0; i<estack->nused; i++)
    HDfprintf(stderr, "%s %d: i=%d, maj_id=%d, min_id=%d, nused=%d\n", FUNC, __LINE__, i, estack->slot[i].maj_id, 
            estack->slot[i].min_id, estack->nused);
*/

    if (func && H5E_WALK_UPWARD==direction) {
	for (i=0, status=SUCCEED; i<estack->nused && status>=0; i++) {
	    status = (func)(i, estack->slot+i, client_data);
	}
    } else if (func && H5E_WALK_DOWNWARD==direction) {
	for (i=estack->nused-1, status=SUCCEED; i>=0 && status>=0; --i) {
	    status = (func)(estack->nused-(i+1), estack->slot+i, client_data);
	}
    }
    
done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5E_walk_cb
 *
 * Purpose:	This is a default error stack traversal callback function
 *		that prints error messages to the specified output stream.
 *		It is not meant to be called directly but rather as an
 *		argument to the H5Ewalk() function.  This function is called
 *		also by H5Eprint().  Application writers are encouraged to
 *		use this function as a model for their own error stack
 *		walking functions.
 *
 *		N is a counter for how many times this function has been
 *		called for this particular traversal of the stack.  It always
 *		begins at zero for the first error on the stack (either the
 *		top or bottom error, or even both, depending on the traversal
 *		direction and the size of the stack).
 *
 *		ERR_DESC is an error description.  It contains all the
 *		information about a particular error.
 *
 *		CLIENT_DATA is the same pointer that was passed as the
 *		CLIENT_DATA argument of H5Ewalk().  It is expected to be a
 *		file pointer (or stderr if null).
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Friday, December 12, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5E_walk_cb(int n, H5E_error_t *err_desc, void *client_data)
{
    H5E_print_t         *eprint  = (H5E_print_t *)client_data;
    FILE		*stream  = NULL;
    H5E_cls_t           *cls_ptr = NULL;
    H5E_msg_t           *maj_ptr = NULL;
    H5E_msg_t           *min_ptr = NULL;
    const char		*maj_str = NULL;
    const char		*min_str = NULL;
    const char		*cls_str = NULL;
    const int		indent = 2;

    FUNC_ENTER_NOINIT(H5E_walk_cb);
    /*NO TRACE*/

    /* Check arguments */
    assert (err_desc);
    if (!client_data) stream = stderr;
    else stream = eprint->stream;
    
    /* Get descriptions for the major and minor error numbers */
    /* Need to check for errors */
    maj_ptr = H5I_object_verify(err_desc->maj_id, H5I_ERROR_MSG);
    min_ptr = H5I_object_verify(err_desc->min_id, H5I_ERROR_MSG);
    assert(maj_ptr && min_ptr);
    if(maj_ptr->msg)
        maj_str = maj_ptr->msg;
    if(min_ptr)
        min_str = min_ptr->msg;

    /* Get error class info */
    /* add error checking later */
    if(HDstrcmp(maj_ptr->cls->cls_name, min_ptr->cls->cls_name))
        ;
    cls_ptr = maj_ptr->cls;
    cls_str = maj_ptr->cls->cls_name;
    
    /* Print error message */
    if(HDstrcmp(cls_ptr->lib_name, eprint->cls.lib_name)) {
        /* update to the new class information */
        if(cls_ptr->cls_name) eprint->cls.cls_name = cls_ptr->cls_name;
        if(cls_ptr->lib_name) eprint->cls.lib_name = cls_ptr->lib_name;
        if(cls_ptr->lib_vers) eprint->cls.lib_vers = cls_ptr->lib_vers;

        fprintf (stream, "%s-DIAG: Error detected in %s ", cls_ptr->lib_name, cls_ptr->lib_vers);
        
        /* try show the process or thread id in multiple processes cases*/
#ifdef H5_HAVE_PARALLEL
        {   int mpi_rank, mpi_initialized;
	    MPI_Initialized(&mpi_initialized);
	    if (mpi_initialized){
	        MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);
	        fprintf (stream, "MPI-process %d.", mpi_rank);
	    }else
	        fprintf (stream, "thread 0.");
        }
#elif defined(H5_HAVE_THREADSAFE)
        fprintf (stream, "thread %d.", (int)pthread_self());
#else
        fprintf (stream, "thread 0.");
#endif
        /* Don't know what this is for */
        /*if (estack && estack->nused>0) fprintf (stream, "  Back trace follows.");*/
        HDfputc ('\n', stream);
    }

    fprintf (stream, "%*s#%03d: %s line %u in %s(): %s\n",
	     indent, "", n, err_desc->file_name, err_desc->line,
	     err_desc->func_name, err_desc->desc);
    fprintf (stream, "%*sclass: %s\n", indent*2, "", cls_str);
    fprintf (stream, "%*smajor: %s\n", indent*2, "", maj_str);
    fprintf (stream, "%*sminor: %s\n", indent*2, "", min_str);

    FUNC_LEAVE_NOAPI(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Eget_auto
 *
 * Purpose:	Returns the current settings for the automatic error stack
 *		traversal function and its data for specific error stack.  
 *		Either (or both) arguments may be null in which case the 
 *		value is not returned.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Saturday, February 28, 1998
 *
 * Modifications:
 *              Raymond Lu
 *              July 18, 2003
 *              Added error stack in the parameters.  It returns the 
 *              traversal function and data for that error stack.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Eget_auto(hid_t estack_id, H5E_auto_t *func, void **client_data)
{
    herr_t ret_value=SUCCEED;   /* Return value */
    H5E_t *estack_ptr = NULL;
    
    FUNC_ENTER_API(H5Eget_auto, FAIL);
    H5TRACE3("e","i*x*x",estack_id,func,client_data);
    
    if(estack_id == H5E_DEFAULT)
    	estack_ptr = H5E_get_my_stack();
    else
        estack_ptr = H5I_object_verify(estack_id, H5I_ERROR_STACK);

    ret_value = H5E_get_auto(estack_ptr, func, client_data);

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5E_get_auto
 *
 * Purpose:	Private function to return the current settings for the 
 *              automatic error stack traversal function and its data 
 *              for specific error stack. Either (or both) arguments may 
 *              be null in which case the value is not returned.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              July 18, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5E_get_auto(H5E_t *estack, H5E_auto_t *func, void **client_data)
{
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5E_get_auto, FAIL);

    assert (estack);
        
    if(func) *func = estack->func;
    if(client_data) *client_data = estack->auto_data;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Eset_auto
 *
 * Purpose:	Turns on or off automatic printing of errors for certain 
 *              error stack.  When turned on (non-null FUNC pointer) any 
 *              API function which returns an error indication will first
 *              call FUNC passing it CLIENT_DATA as an argument.
 *
 *		The default values before this function is called are
 *		H5Eprint() with client data being the standard error stream,
 *		stderr.
 *
 *		Automatic stack traversal is always in the H5E_WALK_DOWNWARD
 *		direction.
 *		
 * See Also:	H5Ewalk()
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Friday, February 27, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Eset_auto(hid_t estack_id, H5E_auto_t func, void *client_data)
{
    herr_t ret_value=SUCCEED;   /* Return value */
    H5E_t   *estack_ptr = NULL;
    
    FUNC_ENTER_API(H5Eset_auto, FAIL);
    H5TRACE3("e","ixx",estack_id,func,client_data);

    if(estack_id == H5E_DEFAULT)
    	estack_ptr = H5E_get_my_stack();
    else
        estack_ptr = H5I_object_verify(estack_id, H5I_ERROR_STACK);
    
    ret_value = H5E_set_auto(estack_ptr, func, client_data);   

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5E_set_auto
 *
 * Purpose:	Private function to turn on or off automatic printing of 
 *              errors for certain error stack.  When turned on (non-null 
 *              FUNC pointer) any API function which returns an error 
 *              indication will first call FUNC passing it CLIENT_DATA 
 *              as an argument.
 *
 *		The default values before this function is called are
 *		H5Eprint() with client data being the standard error stream,
 *		stderr.
 *
 *		Automatic stack traversal is always in the H5E_WALK_DOWNWARD
 *		direction.
 *		
 * See Also:	H5Ewalk()
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Friday, February 27, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5E_set_auto(H5E_t *estack, H5E_auto_t func, void *client_data)
{
    herr_t ret_value=SUCCEED;   /* Return value */
    H5E_t *tmp;

    FUNC_ENTER_NOAPI(H5E_set_auto, FAIL);

    assert(estack);

    estack->func = func;
    estack->auto_data = client_data;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}
