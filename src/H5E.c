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

/* Interface initialization? */
static int interface_initialize_g = 0;
#define INTERFACE_INIT H5E_init_interface

/* HDF5 error class ID */
hid_t H5E_ERR_CLS_g			= FAIL;

/*
 * Predefined errors. These are initialized at runtime in H5E_init_interface()
 * in this source file.
 *
 * If more of these are added, the new ones must be added to the list of
 * types to reset in H5E_term_interface().
 */

/* Major error IDs */
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

/* Amount to indent each error */
#define H5E_INDENT              2

#ifdef H5_HAVE_THREADSAFE
/*
 * The per-thread error stack. pthread_once() initializes a special
 * key that will be used by all threads to create a stack specific to
 * each thread individually. The association of stacks to threads will
 * be handled by the pthread library.
 *
 * In order for this macro to work, H5E_get_my_stack() must be preceeded
 * by "H5E_t *estack =".
 */
static H5E_t *    H5E_get_stack(void);
#define H5E_get_my_stack()  H5E_get_stack()
#else /* H5_HAVE_THREADSAFE */
/*
 * The current error stack.
 */
H5E_t		H5E_stack_g[1];
#define H5E_get_my_stack() (H5E_stack_g+0)
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
static H5E_cls_t *H5E_register_class(const char *cls_name, const char *lib_name, 
                                const char *version);
static herr_t  H5E_unregister_class(H5E_cls_t *cls);
static ssize_t H5E_get_class_name(H5E_cls_t *cls, char *name, size_t size);
static int H5E_close_msg_cb(void *obj_ptr, hid_t obj_id, void *key);
static herr_t  H5E_close_msg(H5E_msg_t *err);
static H5E_msg_t *H5E_create_msg(H5E_cls_t *cls, H5E_type_t msg_type, const char *msg);
static ssize_t H5E_get_msg(H5E_msg_t *msg_ptr, H5E_type_t *type, char *msg, size_t size);
static H5E_t  *H5E_get_current_stack(void);
static herr_t  H5E_set_current_stack(H5E_t *estack);
static herr_t  H5E_close_stack(H5E_t *err_stack);
static int     H5E_get_num(H5E_t *err_stack);
static herr_t  H5E_pop(H5E_t *err_stack, size_t count);
static herr_t  H5E_clear_entries(H5E_t *estack, unsigned nentries);
static herr_t  H5E_print(H5E_t *estack, FILE *stream);
static herr_t  H5E_walk (H5E_t *estack, H5E_direction_t direction, H5E_walk_t func, 
                             void *client_data);
static herr_t  H5E_walk_cb(unsigned n, H5E_error_t *err_desc, void *client_data);
static herr_t  H5E_get_auto(H5E_t *estack, H5E_auto_t *func, void **client_data);
static herr_t  H5E_set_auto(H5E_t *estack, H5E_auto_t func, void *client_data);

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
    H5E_cls_t   *cls;           /* Pointer to error class */
    H5E_msg_t   *msg;           /* Pointer to new error message */
    char lib_vers[128];         /* Buffer to constructu library version within */
    herr_t      ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOINIT(H5E_init_interface);

    /* Initialize the atom group for the error class IDs */
    if(H5I_init_group(H5I_ERROR_CLASS, H5I_ERRCLS_HASHSIZE, H5E_RESERVED_ATOMS, 
                    (H5I_free_t)H5E_unregister_class)<0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTINIT, FAIL, "unable to initialize ID group");
    /* Initialize the atom group for the major error IDs */
    if(H5I_init_group(H5I_ERROR_MSG, H5I_ERRMSG_HASHSIZE, H5E_RESERVED_ATOMS, 
                    (H5I_free_t)H5E_close_msg)<0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTINIT, FAIL, "unable to initialize ID group");
    /* Initialize the atom group for the error stacks */
    if(H5I_init_group(H5I_ERROR_STACK, H5I_ERRSTK_HASHSIZE, H5E_RESERVED_ATOMS, 
                    (H5I_free_t)H5E_close_stack)<0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTINIT, FAIL, "unable to initialize ID group");

#ifndef H5_HAVE_THREADSAFE
    H5E_stack_g[0].nused = 0;
    H5E_stack_g[0].func = (H5E_auto_t)H5Eprint;
    H5E_stack_g[0].auto_data = NULL;
#endif /* H5_HAVE_THREADSAFE */

    /* Allocate the HDF5 error class */
    assert(H5E_ERR_CLS_g==(-1));
    HDsnprintf(lib_vers,sizeof(lib_vers),"%u.%u.%u%s",H5_VERS_MAJOR,H5_VERS_MINOR,H5_VERS_RELEASE,(HDstrlen(H5_VERS_SUBRELEASE)>0 ? "-"H5_VERS_SUBRELEASE : ""));
    if((cls = H5E_register_class(H5E_CLS_NAME, H5E_CLS_LIB_NAME, lib_vers))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "class initialization failed");
    if((H5E_ERR_CLS_g = H5I_register(H5I_ERROR_CLASS, cls))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error class")

    /* Allocate the HDF5 major errors */

    assert(H5E_ARGS_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MAJOR, H5E_MAJ_ARGS_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_ARGS_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_RESOURCE_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MAJOR, H5E_MAJ_RESOURCE_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_RESOURCE_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_INTERNAL_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MAJOR, H5E_MAJ_INTERNAL_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_INTERNAL_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_FILE_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MAJOR, H5E_MAJ_FILE_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_FILE_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_IO_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MAJOR, H5E_MAJ_IO_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_IO_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_FUNC_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MAJOR, H5E_MAJ_FUNC_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_FUNC_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_ATOM_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MAJOR, H5E_MAJ_ATOM_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_ATOM_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CACHE_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MAJOR, H5E_MAJ_CACHE_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CACHE_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_BTREE_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MAJOR, H5E_MAJ_BTREE_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_BTREE_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_SYM_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MAJOR, H5E_MAJ_SYM_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_SYM_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_HEAP_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MAJOR, H5E_MAJ_HEAP_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_HEAP_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_OHDR_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MAJOR, H5E_MAJ_HEAP_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_OHDR_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_DATATYPE_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MAJOR, H5E_MAJ_DATATYPE_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_DATATYPE_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_DATASPACE_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MAJOR, H5E_MAJ_DATASPACE_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_DATASPACE_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_DATASET_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MAJOR, H5E_MAJ_DATASET_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_DATASET_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_STORAGE_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MAJOR, H5E_MAJ_STORAGE_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_STORAGE_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_PLIST_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MAJOR, H5E_MAJ_PLIST_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_PLIST_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_ATTR_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MAJOR, H5E_MAJ_ATTR_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_ATTR_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_PLINE_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MAJOR, H5E_MAJ_PLINE_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_PLINE_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_EFL_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MAJOR, H5E_MAJ_EFL_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_EFL_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_REFERENCE_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MAJOR, H5E_MAJ_REFERENCE_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_REFERENCE_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_VFL_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MAJOR, H5E_MAJ_VFL_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_VFL_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_TBBT_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MAJOR, H5E_MAJ_TBBT_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_TBBT_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_FPHDF5_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MAJOR, H5E_MAJ_FPHDF5_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_FPHDF5_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_TST_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MAJOR, H5E_MAJ_TST_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_TST_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_RS_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MAJOR, H5E_MAJ_RS_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_RS_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_ERROR_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MAJOR, H5E_MAJ_ERROR_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_ERROR_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")

    /* Allocate the HDF5 minor errors */

    assert(H5E_UNINITIALIZED_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_UNINITIALIZED_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_UNINITIALIZED_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_UNSUPPORTED_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_UNSUPPORTED_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_UNSUPPORTED_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_BADTYPE_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_BADTYPE_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_BADTYPE_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_BADRANGE_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_BADRANGE_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_BADRANGE_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_BADVALUE_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_BADVALUE_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_BADVALUE_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")

    assert(H5E_NOSPACE_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_NOSPACE_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_NOSPACE_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CANTCOPY_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTCOPY_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTCOPY_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CANTFREE_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTFREE_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTFREE_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_ALREADYEXISTS_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_ALREADYEXISTS_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_ALREADYEXISTS_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CANTLOCK_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTLOCK_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTLOCK_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CANTUNLOCK_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTUNLOCK_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTUNLOCK_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CANTGC_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTGC_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTGC_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    
    assert(H5E_FILEEXISTS_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_FILEEXISTS_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_FILEEXISTS_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_FILEOPEN_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_FILEOPEN_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_FILEOPEN_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CANTCREATE_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTCREATE_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTCREATE_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CANTOPENFILE_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTOPENFILE_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTOPENFILE_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CANTCLOSEFILE_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTCLOSEFILE_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTCLOSEFILE_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_NOTHDF5_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_NOTHDF5_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_NOTHDF5_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_BADFILE_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_BADFILE_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_BADFILE_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_TRUNCATED_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_TRUNCATED_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_TRUNCATED_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_MOUNT_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_MOUNT_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_MOUNT_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")

    assert(H5E_SEEKERROR_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_SEEKERROR_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_SEEKERROR_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_READERROR_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_READERROR_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_READERROR_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_WRITEERROR_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_WRITEERROR_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_WRITEERROR_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CLOSEERROR_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CLOSEERROR_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CLOSEERROR_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_OVERFLOW_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_OVERFLOW_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_OVERFLOW_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_FCNTL_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_FCNTL_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_FCNTL_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")

    assert(H5E_CANTINIT_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTINIT_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTINIT_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_ALREADYINIT_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_ALREADYINIT_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_ALREADYINIT_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CANTRELEASE_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTRELEASE_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTRELEASE_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    
    assert(H5E_BADATOM_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_BADATOM_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_BADATOM_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_BADGROUP_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_BADGROUP_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_BADGROUP_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CANTREGISTER_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTREGISTER_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTREGISTER_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CANTINC_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTINC_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTINC_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CANTDEC_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTDEC_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTDEC_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_NOIDS_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_NOIDS_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_NOIDS_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    
    assert(H5E_CANTFLUSH_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTFLUSH_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTFLUSH_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CANTLOAD_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTLOAD_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTLOAD_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_PROTECT_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_PROTECT_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_PROTECT_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_NOTCACHED_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_NOTCACHED_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_NOTCACHED_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    
    assert(H5E_NOTFOUND_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_NOTFOUND_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_NOTFOUND_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_EXISTS_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_EXISTS_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_EXISTS_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CANTENCODE_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTENCODE_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTENCODE_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CANTDECODE_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTDECODE_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTDECODE_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CANTSPLIT_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTSPLIT_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTSPLIT_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CANTINSERT_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTINSERT_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTINSERT_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CANTLIST_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTLIST_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTLIST_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    
    assert(H5E_LINKCOUNT_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_LINKCOUNT_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_LINKCOUNT_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_VERSION_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_VERSION_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_VERSION_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_ALIGNMENT_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_ALIGNMENT_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_ALIGNMENT_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_BADMESG_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_BADMESG_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_BADMESG_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CANTDELETE_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTDELETE_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTDELETE_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    
    assert(H5E_CANTOPENOBJ_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTOPENOBJ_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTOPENOBJ_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_COMPLEN_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_COMPLEN_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_COMPLEN_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CWG_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CWG_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CWG_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_LINK_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_LINK_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_LINK_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_SLINK_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_SLINK_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_SLINK_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    
    assert(H5E_CANTCONVERT_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTCONVERT_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTCONVERT_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_BADSIZE_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_BADSIZE_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_BADSIZE_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    
    assert(H5E_CANTCLIP_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTCLIP_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTCLIP_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CANTCOUNT_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTCOUNT_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTCOUNT_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CANTSELECT_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTSELECT_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTSELECT_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CANTNEXT_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTNEXT_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTNEXT_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_BADSELECT_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_BADSELECT_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_BADSELECT_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CANTCOMPARE_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTCOMPARE_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTCOMPARE_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    
    assert(H5E_CANTGET_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTGET_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTGET_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CANTSET_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTSET_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTSET_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_DUPCLASS_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_DUPCLASS_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_DUPCLASS_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")

    assert(H5E_MPI_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_MPI_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_MPI_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_MPIERRSTR_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_MPIERRSTR_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_MPIERRSTR_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    
    assert(H5E_CANTMAKETREE_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTMAKETREE_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTMAKETREE_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CANTRECV_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTRECV_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTRECV_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CANTSENDMDATA_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTSENDMDATA_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTSENDMDATA_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CANTCHANGE_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTCHANGE_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTCHANGE_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CANTALLOC_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANTALLOC_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANTALLOC_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")

    assert(H5E_NOFILTER_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_NOFILTER_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_NOFILTER_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CALLBACK_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CALLBACK_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CALLBACK_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_CANAPPLY_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_CANAPPLY_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_CANAPPLY_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")
    assert(H5E_SETLOCAL_g==(-1));
    if((msg = H5E_create_msg(cls, H5E_MINOR, H5E_MIN_SETLOCAL_MSG))==NULL)
        HGOTO_ERROR (H5E_ERROR, H5E_CANTINIT, FAIL, "error message initialization failed");
    if((H5E_SETLOCAL_g = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")

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

    FUNC_ENTER_NOINIT(H5E_term_interface);

    if (interface_initialize_g) {
        /* Check if there are any open error stacks, classes or messages */
        ncls = H5I_nmembers(H5I_ERROR_CLASS);
        nmsg = H5I_nmembers(H5I_ERROR_MSG);
        nstk = H5I_nmembers(H5I_ERROR_STACK);

        n = ncls + nmsg + nstk;
        if(n>0) {
            /* Clear any outstanding error stacks */
            if (nstk>0)
	        H5I_clear_group(H5I_ERROR_STACK, FALSE);
        
            /* Clear all the error classes */
	    if (ncls>0) {
	        H5I_clear_group(H5I_ERROR_CLASS, FALSE);
                
                /* Reset the HDF5 error class, if its been closed */
                if(H5I_nmembers(H5I_ERROR_CLASS)==0)
                    H5E_ERR_CLS_g = -1;
            }
            
            /* Clear all the error messages */
	    if (nmsg>0) {
	        H5I_clear_group(H5I_ERROR_MSG, FALSE);
                
                /* Reset the HDF5 error messages, if they've been closed */
                if(H5I_nmembers(H5I_ERROR_MSG)==0) {
                    /* Reset major error IDs */
                    H5E_ARGS_g=
                    H5E_RESOURCE_g=
                    H5E_INTERNAL_g=
                    H5E_FILE_g=
                    H5E_IO_g=
                    H5E_FUNC_g=
                    H5E_ATOM_g=
                    H5E_CACHE_g=
                    H5E_BTREE_g=
                    H5E_SYM_g=
                    H5E_HEAP_g=
                    H5E_OHDR_g=
                    H5E_DATATYPE_g=
                    H5E_DATASPACE_g=
                    H5E_DATASET_g=
                    H5E_STORAGE_g=
                    H5E_PLIST_g=
                    H5E_ATTR_g=
                    H5E_PLINE_g=
                    H5E_EFL_g=
                    H5E_REFERENCE_g=
                    H5E_VFL_g=
                    H5E_TBBT_g=
                    H5E_FPHDF5_g=
                    H5E_TST_g=
                    H5E_RS_g=
                    H5E_ERROR_g= (-1);

                    /* Reset minor error IDs */
                    H5E_UNINITIALIZED_g=
                    H5E_UNSUPPORTED_g=
                    H5E_BADTYPE_g=
                    H5E_BADRANGE_g=
                    H5E_BADVALUE_g=
                    H5E_NOSPACE_g=
                    H5E_CANTCOPY_g=
                    H5E_CANTFREE_g=
                    H5E_ALREADYEXISTS_g=
                    H5E_CANTLOCK_g=
                    H5E_CANTUNLOCK_g=
                    H5E_CANTGC_g=
                    H5E_FILEEXISTS_g=
                    H5E_FILEOPEN_g=
                    H5E_CANTCREATE_g=
                    H5E_CANTOPENFILE_g=
                    H5E_CANTCLOSEFILE_g=
                    H5E_NOTHDF5_g=
                    H5E_BADFILE_g=
                    H5E_TRUNCATED_g=
                    H5E_MOUNT_g=
                    H5E_SEEKERROR_g=
                    H5E_READERROR_g=
                    H5E_WRITEERROR_g=
                    H5E_CLOSEERROR_g=
                    H5E_OVERFLOW_g=
                    H5E_FCNTL_g=
                    H5E_CANTINIT_g=
                    H5E_ALREADYINIT_g=
                    H5E_CANTRELEASE_g=
                    H5E_BADATOM_g=
                    H5E_BADGROUP_g=
                    H5E_CANTREGISTER_g=
                    H5E_CANTINC_g=
                    H5E_CANTDEC_g=
                    H5E_NOIDS_g=
                    H5E_CANTFLUSH_g=
                    H5E_CANTLOAD_g=
                    H5E_PROTECT_g=
                    H5E_NOTCACHED_g=
                    H5E_NOTFOUND_g=
                    H5E_EXISTS_g=
                    H5E_CANTENCODE_g=
                    H5E_CANTDECODE_g=
                    H5E_CANTSPLIT_g=
                    H5E_CANTINSERT_g=
                    H5E_CANTLIST_g=
                    H5E_LINKCOUNT_g=
                    H5E_VERSION_g=
                    H5E_ALIGNMENT_g=
                    H5E_BADMESG_g=
                    H5E_CANTDELETE_g=
                    H5E_CANTOPENOBJ_g=
                    H5E_COMPLEN_g=
                    H5E_CWG_g=
                    H5E_LINK_g=
                    H5E_SLINK_g=
                    H5E_CANTCONVERT_g=
                    H5E_BADSIZE_g=
                    H5E_CANTCLIP_g=
                    H5E_CANTCOUNT_g=
                    H5E_CANTSELECT_g=
                    H5E_CANTNEXT_g=
                    H5E_BADSELECT_g=
                    H5E_CANTCOMPARE_g=
                    H5E_CANTGET_g=
                    H5E_CANTSET_g=
                    H5E_DUPCLASS_g=
                    H5E_MPI_g=
                    H5E_MPIERRSTR_g=
                    H5E_CANTMAKETREE_g=
                    H5E_CANTRECV_g=
                    H5E_CANTSENDMDATA_g=
                    H5E_CANTCHANGE_g=
                    H5E_CANTALLOC_g=
                    H5E_NOFILTER_g=
                    H5E_CALLBACK_g=
                    H5E_CANAPPLY_g=
                    H5E_SETLOCAL_g= (-1);
                } /* end if */
            } /* end if */

	} else {
	    /* Destroy the error class, message, and stack id groups */
	    H5I_destroy_group(H5I_ERROR_STACK);
	    H5I_destroy_group(H5I_ERROR_CLASS);
	    H5I_destroy_group(H5I_ERROR_MSG);

	    /* Mark closed */
	    interface_initialize_g = 0;
	    n = 1; /*H5I*/
	}
    }

    FUNC_LEAVE_NOAPI(n);
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
static H5E_t *
H5E_get_stack(void)
{
    H5E_t *estack;
    H5E_t *ret_value;   /* Return value */

    FUNC_ENTER_NOAPI(H5E_get_stack,NULL);

    estack = pthread_getspecific(H5TS_errstk_key_g);

    if (!estack) {
        /* no associated value with current thread - create one */
        estack = (H5E_t *)H5MM_malloc(sizeof(H5E_t));
        estack->nused = 0;
        estack->func = H5Eprint;
        estack->auto_data = NULL;
        pthread_setspecific(H5TS_errstk_key_g, (void *)estack);
    }

    /* Set return value */
    ret_value=estack;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}
#endif  /* H5_HAVE_THREADSAFE */


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
    H5E_cls_t   *cls;        /* Pointer to error class */
    hid_t       ret_value;   /* Return value */

    FUNC_ENTER_API(H5Eregister_class, FAIL);
    H5TRACE3("i","sss",cls_name,lib_name,version);
    
    /* Check arguments */
    if (cls_name==NULL || lib_name==NULL || version==NULL)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid string")

    /* Create the new error class object */
    if((cls=H5E_register_class(cls_name, lib_name, version))==NULL)
	HGOTO_ERROR(H5E_ERROR, H5E_CANTCREATE, FAIL, "can't create error class")

    /* Register the new error class to get an ID for it */
    if((ret_value = H5I_register(H5I_ERROR_CLASS, cls))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error class")

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
static H5E_cls_t *
H5E_register_class(const char *cls_name, const char *lib_name, const char *version)
{
    H5E_cls_t   *cls;        /* Pointer to error class */
    H5E_cls_t   *ret_value;  /* Return value */
    
    FUNC_ENTER_NOAPI(H5E_register_class, NULL);
    
    /* Check arguments */
    assert(cls_name);
    assert(lib_name);
    assert(version);

    /* Allocate space for new error class */
    if((cls = H5MM_malloc(sizeof(H5E_cls_t)))==NULL)
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Duplicate string information */
    if((cls->cls_name = HDstrdup(cls_name))==NULL)
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    if((cls->lib_name = HDstrdup(lib_name))==NULL)
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    if((cls->lib_vers = HDstrdup(version))==NULL)
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Set the return value */
    ret_value=cls;

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

    FUNC_ENTER_API(H5Eunregister_class, FAIL);
    H5TRACE1("e","i",class_id);
    
    /* Check arguments */
    if (H5I_ERROR_CLASS != H5I_get_type(class_id))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an error class")

    /*
     * Decrement the counter on the dataset.  It will be freed if the count
     * reaches zero.
     */
    if(H5I_dec_ref(class_id)<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTDEC, FAIL, "unable to decrement ref count on error class");

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
static herr_t
H5E_unregister_class(H5E_cls_t *cls)
{
    herr_t       ret_value = SUCCEED;   /* Return value */
    
    FUNC_ENTER_NOAPI(H5E_unregister_class, FAIL);

    /* Check arguments */
    assert(cls);

    /* Iterate over all the messages and delete those in this error class */
    H5I_search(H5I_ERROR_MSG, H5E_close_msg_cb, cls);

    /* Free error class structure */
    if(cls->cls_name)    
        H5MM_xfree((void*)cls->cls_name);
    if(cls->lib_name)
        H5MM_xfree((void*)cls->lib_name);
    if(cls->lib_vers)
        H5MM_xfree((void*)cls->lib_vers);
    H5MM_xfree((void*)cls);
    
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
    H5E_cls_t   *cls;        /* Pointer to error class */
    ssize_t     ret_value;   /* Return value */

    FUNC_ENTER_API(H5Eget_class_name, FAIL);
    H5TRACE3("Zs","isz",class_id,name,size);
    
    /* Get the error class */
    if(NULL==(cls = H5I_object_verify(class_id, H5I_ERROR_CLASS)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error class ID")

    /* Retrieve the class name */
    if((ret_value = H5E_get_class_name(cls, name, size))<0)
	HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get error class name")

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
    ssize_t       len;          /* Length of rror class's name */
    ssize_t       ret_value;    /* Return value */
    
    FUNC_ENTER_NOAPI(H5E_get_class_name, FAIL);

    /* Check arguments */
    assert(cls);

    /* Get the class's name */
    len = (ssize_t)HDstrlen(cls->cls_name);

    /* Set the user's buffer, if provided */
    if(name) {
       HDstrncpy(name, cls->cls_name, MIN((size_t)(len+1), size));
       if((size_t)len >= size)
          name[size-1]='\0';
    } 
    
    /* Return the full length */
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
    H5E_msg_t   *err_msg = (H5E_msg_t*)obj_ptr;
    H5E_cls_t   *cls = (H5E_cls_t*)key;
    herr_t      ret_value = SUCCEED;       /* Return value */
        
    FUNC_ENTER_NOAPI(H5_close_msg_cb, FAIL);
  
    /* Check arguments */
    assert(err_msg);

    /* Close the message if it is in the class being closed */
    if(err_msg->cls == cls)
        if(H5I_dec_ref(obj_id)<0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTDEC, FAIL, "unable to decrement ref count on error message");
    
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
    
    FUNC_ENTER_API(H5Eclose_msg, FAIL);
    H5TRACE1("e","i",err_id);

    /* Check arguments */
    if (H5I_ERROR_MSG != H5I_get_type(err_id))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an error class")

    /* Decrement the counter.  It will be freed if the count reaches zero. */
    if(H5I_dec_ref(err_id)<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTDEC, FAIL, "unable to decrement ref count on error message");

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
static herr_t
H5E_close_msg(H5E_msg_t *err)
{
    herr_t       ret_value = SUCCEED;   /* Return value */
    
    FUNC_ENTER_NOAPI(H5E_close_msg, FAIL);

    /* Check arguments */
    assert(err);

    if(err->msg)    
        H5MM_xfree((void*)err->msg);
    /* Don't free err->cls here */

    H5MM_xfree((void*)err);

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
H5Ecreate_msg(hid_t class_id, H5E_type_t msg_type, const char *msg_str)
{
    H5E_cls_t   *cls;           /* Pointer to error class */
    H5E_msg_t   *msg;           /* Pointer to new error message */
    hid_t       ret_value;      /* Return value */
    
    FUNC_ENTER_API(H5Ecreate_msg, FAIL);
    
    /* Check arguments */
    if(msg_type!=H5E_MAJOR && msg_type!=H5E_MINOR)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a valid message type")
    if(msg_str==NULL)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "message is NULL")

    /* Get the error class */
    if(NULL==(cls = H5I_object_verify(class_id, H5I_ERROR_CLASS)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error class ID")

    /* Create the new error message object */
    if((msg = H5E_create_msg(cls, msg_type, msg_str))==NULL)
	HGOTO_ERROR(H5E_ERROR, H5E_CANTCREATE, FAIL, "can't create error message")

    /* Register the new error class to get an ID for it */
    if((ret_value = H5I_register(H5I_ERROR_MSG, msg))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")

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
static H5E_msg_t *
H5E_create_msg(H5E_cls_t *cls, H5E_type_t msg_type, const char *msg_str)
{
    H5E_msg_t   *msg;           /* Pointer to new error message */
    H5E_msg_t   *ret_value;     /* Return value */
    
    FUNC_ENTER_NOAPI(H5E_create_msg, NULL);
   
    /* Check arguments */
    assert(cls);
    assert(msg_type==H5E_MAJOR || msg_type==H5E_MINOR);
    assert(msg_str);

    /* Allocate new message object */
    if((msg = H5MM_malloc(sizeof(H5E_msg_t)))==NULL)
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Fill new message object */
    msg->cls = cls;
    msg->type = msg_type;
    if((msg->msg = HDstrdup(msg_str))==NULL)
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Set return value */
    ret_value = msg;

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
H5Eget_msg(hid_t msg_id, H5E_type_t *type, char *msg_str, size_t size)
{
    H5E_msg_t   *msg;           /* Pointer to error message */
    ssize_t      ret_value;     /* Return value */

    FUNC_ENTER_API(H5Eget_msg, FAIL);
    
    /* Get the message object */
    if((msg = H5I_object_verify(msg_id, H5I_ERROR_MSG))==NULL)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error message ID")

    /* Get the message's text */
    if((ret_value = H5E_get_msg(msg, type, msg_str, size))<0)
	HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get error message text")

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
static ssize_t
H5E_get_msg(H5E_msg_t *msg, H5E_type_t *type, char *msg_str, size_t size)
{
    ssize_t       len;          /* Length of rror class's name */
    ssize_t       ret_value;   /* Return value */
    
    FUNC_ENTER_NOAPI(H5E_get_msg, FAIL);

    /* Check arguments */
    assert(msg);

    /* Get the length of the message string */
    len = HDstrlen(msg->msg);

    /* Copy the message into the user's buffer, if given */
    if(msg_str) {
       HDstrncpy(msg_str, msg->msg, MIN((size_t)(len+1), size));
       if((size_t)len >= size)
          msg_str[size-1]='\0';
    } 
    
    /* Give the message type, if asked */
    if(type)
        *type = msg->type;

    /* Set the return value to the full length of the message */
    ret_value = len;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


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
    H5E_t	*stk;           /* Error stack */
    hid_t       ret_value;   /* Return value */

    /* Don't clear the error stack! :-) */
    FUNC_ENTER_API_NOCLEAR(H5Eget_current_stack, FAIL);
    H5TRACE0("i","");
    
    /* Get the current stack */
    if((stk=H5E_get_current_stack())==NULL)
	HGOTO_ERROR(H5E_ERROR, H5E_CANTCREATE, FAIL, "can't create error stack")

    /* Register the stack */
    if((ret_value = H5I_register(H5I_ERROR_STACK, stk))<0)
	HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't create error stack")

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
static H5E_t *
H5E_get_current_stack(void)
{
    H5E_t	*current_stack; /* Pointer to the current error stack */
    H5E_t	*estack_copy=NULL;   /* Pointer to new error stack to return */
    unsigned    u;              /* Local index variable */
    H5E_t      *ret_value;   /* Return value */
    
    FUNC_ENTER_NOAPI(H5E_get_current_stack, NULL);

    /* Get a pointer to the current error stack */
    if((current_stack = H5E_get_my_stack ())==NULL)
	HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, NULL, "can't get current error stack")

    /* Allocate a new error stack */
    if((estack_copy = H5MM_malloc(sizeof(H5E_t)))==NULL)
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Make a copy of current error stack */
    estack_copy->nused = current_stack->nused; 
    for(u=0; u<current_stack->nused; u++) {
        H5E_error_t *current_error, *new_error; /* Pointers to errors on each stack */

        /* Get pointers into the current error stack location */
        current_error = &(current_stack->slot[u]);
        new_error = &(estack_copy->slot[u]);
       
        /* Increment the IDs to indicate that they are used in this stack */
        if(H5I_inc_ref(current_error->cls_id)<0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTINC, NULL, "unable to decrement ref count on error class");
        new_error->cls_id = current_error->cls_id;       
        if(H5I_inc_ref(current_error->maj_id)<0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTINC, NULL, "unable to decrement ref count on error message");
        new_error->maj_id = current_error->maj_id;       
        if(H5I_inc_ref(current_error->min_id)<0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTINC, NULL, "unable to decrement ref count on error message");
        new_error->min_id = current_error->min_id;       
        if((new_error->func_name = HDstrdup(current_error->func_name))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
        if((new_error->file_name = HDstrdup(current_error->file_name))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
        new_error->line = current_error->line;
        if((new_error->desc = HDstrdup(current_error->desc))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    } /* end for */
   
    /* Empty current error stack */ 
    H5E_clear(current_stack);
   
    /* Set the return value */
    ret_value = estack_copy;

done:
    if(ret_value==NULL) {
        if(estack_copy!=NULL)
            H5MM_xfree((void*)estack_copy);
    } /* end if */

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
H5Eset_current_stack(hid_t err_stack)
{
    H5E_t      *estack;
    herr_t         ret_value = SUCCEED;   /* Return value */
    
    FUNC_ENTER_API(H5Eset_current_stack, FAIL);
    H5TRACE1("e","i",err_stack);
    
    if(err_stack != H5E_DEFAULT) {
        if((estack = H5I_object_verify(err_stack, H5I_ERROR_STACK))==NULL)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error stack ID")

        /* Set the current error stack */
        if(H5E_set_current_stack(estack)<0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTSET, FAIL, "unable to set error stack");
    } /* end if */

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
static herr_t
H5E_set_current_stack(H5E_t *estack)
{
    H5E_t	*current_stack;         /* Default error stack */
    unsigned     u;                     /* Local index variable */
    herr_t       ret_value = SUCCEED;   /* Return value */
    
    FUNC_ENTER_NOAPI(H5E_set_current_stack, FAIL);

    /* Sanity check */
    assert(estack);

    /* Get a pointer to the current error stack */
    if((current_stack = H5E_get_my_stack ())==NULL)
	HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get current error stack")

    /* Empty current error stack */ 
    H5E_clear(current_stack);

    /* Copy new stack to current error stack */
    current_stack->nused = estack->nused; 
    for(u=0; u<current_stack->nused; u++) {
        H5E_error_t *current_error, *new_error; /* Pointers to errors on each stack */

        /* Get pointers into the current error stack location */
        current_error = &(current_stack->slot[u]);
        new_error = &(estack->slot[u]);
       
        /* Increment the IDs to indicate that they are used in this stack */
        if(H5I_inc_ref(new_error->cls_id)<0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTINC, FAIL, "unable to decrement ref count on error class");
        current_error->cls_id = new_error->cls_id;
        if(H5I_inc_ref(new_error->maj_id)<0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTINC, FAIL, "unable to decrement ref count on error class");
        current_error->maj_id = new_error->maj_id;
        if(H5I_inc_ref(new_error->min_id)<0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTINC, FAIL, "unable to decrement ref count on error class");
        current_error->min_id = new_error->min_id;
        if((current_error->func_name = HDstrdup(new_error->func_name))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
        if((current_error->file_name = HDstrdup(new_error->file_name))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
        current_error->line = new_error->line;
        if((current_error->desc = HDstrdup(new_error->desc))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
    } /* end for */

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

    FUNC_ENTER_API(H5Eclose_stack, FAIL);
    H5TRACE1("e","i",stack_id);

    if(H5E_DEFAULT != stack_id) {
        /*
         * Decrement the counter on the error stack.  It will be freed if the count
         * reaches zero.
         */
        if(H5I_dec_ref(stack_id)<0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTDEC, FAIL, "unable to decrement ref count on error stack");
    } /* end if */

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
static herr_t
H5E_close_stack(H5E_t *estack)
{
    herr_t       ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5E_close_stack, FAIL);
    
    /* Sanity check */
    assert(estack);

    /* Release the stack's error information */
    H5E_clear(estack);

    /* Free the stack structure */
    H5MM_xfree((void*)estack);
    
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
    H5E_t *estack;      /* Error stack to operate on */
    int ret_value;      /* Return value */

    /* Don't clear the error stack! :-) */
    FUNC_ENTER_API_NOCLEAR(H5Eget_num, FAIL);
    H5TRACE1("Is","i",error_stack_id);
   
    /* Need to check for errors */
    if(error_stack_id == H5E_DEFAULT) {
    	if((estack = H5E_get_my_stack())==NULL)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get current error stack")
    } /* end if */
    else {
        /* Only clear the error stack if it's not the default stack */
        H5E_clear(NULL);

        /* Get the error stack to operate on */
        if((estack = H5I_object_verify(error_stack_id, H5I_ERROR_STACK))==NULL)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error stack ID")
    } /* end else */
    
    /* Get the number of errors on stack */
    if((ret_value=H5E_get_num(estack))<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get number of errors")

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
static int
H5E_get_num(H5E_t *estack)
{
    int      ret_value;   /* Return value */

    FUNC_ENTER_NOAPI(H5E_get_num, FAIL);
 
    assert(estack);   

    ret_value = estack->nused;

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
    H5E_t *estack;
    herr_t       ret_value = SUCCEED;   /* Return value */

    /* Don't clear the error stack! :-) */
    FUNC_ENTER_API_NOCLEAR(H5Epop, FAIL);
    H5TRACE2("e","iz",err_stack,count);
   
    /* Need to check for errors */
    if(err_stack == H5E_DEFAULT) {
    	if((estack = H5E_get_my_stack())==NULL)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get current error stack")
    } /* end if */
    else {
        /* Only clear the error stack if it's not the default stack */
        H5E_clear(NULL);

        /* Get the error stack to operate on */
        if((estack = H5I_object_verify(err_stack, H5I_ERROR_STACK))==NULL)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error stack ID")
    } /* end else */

    /* Range limit the number of errors to pop off stack */
    if(count > estack->nused)
        count = estack->nused;

    /* Pop the errors off the stack */
    if(H5E_pop(estack, count)<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTRELEASE, FAIL, "can't pop errors from stack")

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
H5E_pop(H5E_t *estack, size_t count)
{
    herr_t      ret_value = SUCCEED;   /* Return value */
    
    FUNC_ENTER_NOAPI(H5E_pop, FAIL);

    /* Sanity check */
    assert(estack);
    assert(estack->nused>=count);

    /* Remove the entries from the error stack */
    if(H5E_clear_entries(estack, count)<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTRELEASE, FAIL, "can't remove errors from stack")

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
 *
 *              Added the ID of the error stack to which the error is pushed
 *              on.  The error message can be appended more message in the
 *              same control format as printf and fprintf.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Epush(hid_t err_stack, const char *file, const char *func, unsigned line, 
        hid_t cls_id, hid_t maj_id, hid_t min_id, const char *fmt, ...)
{
    va_list     ap;             /* Varargs info */
    H5E_t       *estack;        /* Pointer to error stack to modify */
    H5E_msg_t   *maj_ptr, *min_ptr;     /* Pointer to major and minor error info */
    char        tmp[H5E_LEN];   /* Buffer to place formatted description in */
    herr_t	ret_value=SUCCEED;      /* Return value */

    /* Don't clear the error stack! :-) */
    FUNC_ENTER_API_NOCLEAR(H5Epush, FAIL);
    H5TRACE7("e","issIuiis",err_stack,file,func,line,maj_id,min_id,fmt);
    
    /* Need to check for errors */
    if(err_stack == H5E_DEFAULT)
    	estack = NULL;
    else {
        /* Only clear the error stack if it's not the default stack */
        H5E_clear(NULL);

        /* Get the error stack to operate on */
        if((estack = H5I_object_verify(err_stack, H5I_ERROR_STACK))==NULL)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error stack ID")
    } /* end else */
    
    /* Check for mis-matches in major & minor error classes */
    if((maj_ptr = H5I_object_verify(maj_id, H5I_ERROR_MSG))==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error message ID")
    if((min_ptr = H5I_object_verify(min_id, H5I_ERROR_MSG))==NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error message ID")
    if(maj_ptr->cls != min_ptr->cls)
        HGOTO_ERROR(H5E_ARGS, H5E_UNSUPPORTED, FAIL, "major and minor errors not from same error class")

    /* Format the description */
    va_start(ap, fmt);
    HDvsnprintf(tmp, H5E_LEN, fmt, ap);
    va_end(ap);

    /* Push the error on the stack */
    if(H5E_push(estack, file, func, line, cls_id, maj_id, min_id, tmp)<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTSET, FAIL, "can't push error on stack")

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
    herr_t	ret_value=SUCCEED;      /* Return value */

    /*
     * WARNING: We cannot call HERROR() from within this function or else we
     *		could enter infinite recursion.  Furthermore, we also cannot
     *		call any other HDF5 macro or function which might call
     *		HERROR().  HERROR() is called by HRETURN_ERROR() which could
     *		be called by FUNC_ENTER().
     */
    FUNC_ENTER_NOINIT(H5E_push);

    /* Sanity check */
    assert(cls_id>0);
    assert(maj_id>0);
    assert(min_id>0);

    /* Check for 'default' error stack */
    if(estack==NULL)
        if((estack=H5E_get_my_stack())==NULL)
            HGOTO_DONE(FAIL);

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
        /* Increment the IDs to indicate that they are used in this stack */
        if(H5I_inc_ref(cls_id)<0)
            HGOTO_DONE(FAIL);
	estack->slot[estack->nused].cls_id = cls_id;
        if(H5I_inc_ref(maj_id)<0)
            HGOTO_DONE(FAIL);
	estack->slot[estack->nused].maj_id = maj_id;
        if(H5I_inc_ref(min_id)<0)
            HGOTO_DONE(FAIL);
	estack->slot[estack->nused].min_id = min_id;
	if((estack->slot[estack->nused].func_name = HDstrdup(func))==NULL)
            HGOTO_DONE(FAIL);
	if((estack->slot[estack->nused].file_name = HDstrdup(file))==NULL)
            HGOTO_DONE(FAIL);
	estack->slot[estack->nused].line = line;
	if((estack->slot[estack->nused].desc = HDstrdup(desc))==NULL)
            HGOTO_DONE(FAIL);
	estack->nused++;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
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
    H5E_t   *estack;            /* Error stack to operate on */
    herr_t ret_value=SUCCEED;   /* Return value */
    
    /* Don't clear the error stack! :-) */
    FUNC_ENTER_API_NOCLEAR(H5Eclear, FAIL);
    H5TRACE1("e","i",err_stack);

    /* Need to check for errors */
    if(err_stack == H5E_DEFAULT)
    	estack = NULL;
    else {
        /* Only clear the error stack if it's not the default stack */
        H5E_clear(NULL);

        if((estack = H5I_object_verify(err_stack, H5I_ERROR_STACK))==NULL)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error stack ID")
    } /* end else */
 
    /* Clear the error stack */
    if(H5E_clear(estack)<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTSET, FAIL, "can't clear error stack")

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5E_clear_entries
 *
 * Purpose:	Private function to clear the error stack entries for the
 *              specified error stack.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, August 6, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5E_clear_entries(H5E_t *estack, unsigned nentries)
{
    H5E_error_t         *error; /* Pointer to error stack entry to clear */
    unsigned u;                 /* Local index variable */
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5E_clear_entries, FAIL);

    /* Sanity check */
    assert(estack);
    assert(estack->nused>=nentries);

    /* Empty the error stack from the top down */ 
    for(u=0; nentries>0; nentries--,u++) {
        error = &(estack->slot[estack->nused-(u+1)]);

        /* Decrement the IDs to indicate that they are no longer used by this stack */
        /* (In reverse order that they were incremented, so that reference counts work well) */
        if(H5I_dec_ref(error->min_id)<0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTDEC, FAIL, "unable to decrement ref count on error message");
        if(H5I_dec_ref(error->maj_id)<0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTDEC, FAIL, "unable to decrement ref count on error message");
        if(H5I_dec_ref(error->cls_id)<0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTDEC, FAIL, "unable to decrement ref count on error class");

        /* Release strings */
        if(error->func_name)
            H5MM_xfree((void*)error->func_name);
        if(error->file_name)
            H5MM_xfree((void*)error->file_name);
        if(error->desc)
            H5MM_xfree((void*)error->desc);
    } /* end for */

    /* Decrement number of errors on stack */
    estack->nused-=u;
    
done:
    FUNC_LEAVE_NOAPI(ret_value);
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

    FUNC_ENTER_NOAPI(H5E_clear, FAIL);

    /* Check for 'default' error stack */
    if(estack==NULL)
        if((estack=H5E_get_my_stack())==NULL)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get current error stack")

    /* Empty the error stack */ 
    assert(estack);
    if(estack->nused)
        if(H5E_clear_entries(estack, estack->nused)<0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTSET, FAIL, "can't clear error stack")
    
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
    H5E_t   *estack;            /* Error stack to operate on */
    herr_t ret_value=SUCCEED;   /* Return value */
    
    /* Don't clear the error stack! :-) */
    FUNC_ENTER_API_NOCLEAR(H5Eprint, FAIL);
    /*NO TRACE*/

    /* Need to check for errors */
    if(err_stack == H5E_DEFAULT) {
    	if((estack = H5E_get_my_stack())==NULL)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get current error stack")
    } /* end if */
    else {
        /* Only clear the error stack if it's not the default stack */
        H5E_clear(NULL);

        if((estack = H5I_object_verify(err_stack, H5I_ERROR_STACK))==NULL)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error stack ID")
    } /* end else */
 
    /* Print error stack */
    if(H5E_print(estack, stream)<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTLIST, FAIL, "can't display error stack")

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
static herr_t
H5E_print(H5E_t *estack, FILE *stream)
{
    H5E_print_t eprint;         /* Callback information to pass to H5E_walk_cb() */
    herr_t	ret_value = SUCCEED;

    /* Don't clear the error stack! :-) */
    FUNC_ENTER_NOAPI(H5E_print, FAIL);
    
    /* Sanity check */
    assert(estack);

    /* If no stream was given, use stderr */
    if (!stream) 
        eprint.stream = stderr;
    else
        eprint.stream = stream;

    /* Reset the original error class information */
    HDmemset(&eprint.cls,0,sizeof(H5E_cls_t));
    
    /* Walk the error stack */
    if(H5E_walk (estack, H5E_WALK_DOWNWARD, H5E_walk_cb, (void*)&eprint)<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTLIST, FAIL, "can't walk error stack")
  
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
    H5E_t   *estack;            /* Error stack to operate on */
    herr_t	ret_value=SUCCEED;      /* Return value */

    /* Don't clear the error stack! :-) */
    FUNC_ENTER_API_NOCLEAR(H5Ewalk, FAIL);
    /*NO TRACE*/

    /* Need to check for errors */
    if(err_stack == H5E_DEFAULT) {
    	if((estack = H5E_get_my_stack())==NULL)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get current error stack")
    } /* end if */
    else {
        /* Only clear the error stack if it's not the default stack */
        H5E_clear(NULL);

        if((estack = H5I_object_verify(err_stack, H5I_ERROR_STACK))==NULL)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error stack ID")
    } /* end else */

    /* Walk the error stack */
    if(H5E_walk (estack, direction, func, client_data)<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTLIST, FAIL, "can't walk error stack")

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
static herr_t
H5E_walk (H5E_t *estack, H5E_direction_t direction, H5E_walk_t func, void *client_data)
{
    int		i;              /* Local index variable */
    herr_t	status;         /* Status from callback function */
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5E_walk, FAIL);

    /* Sanity check */
    assert (estack);

    /* check args, but rather than failing use some default value */
    if (direction!=H5E_WALK_UPWARD && direction!=H5E_WALK_DOWNWARD)
	direction = H5E_WALK_UPWARD;

    /* Walk the stack if a callback function was given */
    if(func) {
        status=SUCCEED;
        if (H5E_WALK_UPWARD==direction) {
            for (i=0; i<(int)estack->nused && status>=0; i++)
                status = (func)((unsigned)i, estack->slot+i, client_data);
        } else {
            for (i=estack->nused-1; i>=0 && status>=0; i--)
                status = (func)(estack->nused-(i+1), estack->slot+i, client_data);
        }
        if(status<0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTLIST, FAIL, "can't walk error stack")
    } /* end if */
    
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
H5E_walk_cb(unsigned n, H5E_error_t *err_desc, void *client_data)
{
    H5E_print_t         *eprint  = (H5E_print_t *)client_data;
    FILE		*stream;        /* I/O stream to print output to */
    H5E_cls_t           *cls_ptr;       /* Pointer to error class */
    H5E_msg_t           *maj_ptr;       /* Pointer to major error info */
    H5E_msg_t           *min_ptr;       /* Pointer to minor error info */
    const char		*cls_str;       /* Class description */
    const char		*maj_str = "No major description";      /* Major error description */
    const char		*min_str = "No major description";      /* Minor error description */
    unsigned            have_desc=1;    /* Flag to indicate whether the error has a "real" description */

    FUNC_ENTER_NOINIT(H5E_walk_cb);

    /* Check arguments */
    assert (err_desc);

    /* If no client data was passed, output to stderr */
    if (!client_data) stream = stderr;
    else stream = eprint->stream;
    
    /* Get descriptions for the major and minor error numbers */
    maj_ptr = H5I_object_verify(err_desc->maj_id, H5I_ERROR_MSG);
    min_ptr = H5I_object_verify(err_desc->min_id, H5I_ERROR_MSG);
    assert(maj_ptr && min_ptr);
    if(maj_ptr->msg)
        maj_str = maj_ptr->msg;
    if(min_ptr)
        min_str = min_ptr->msg;

    /* Get error class info */
    cls_ptr = maj_ptr->cls;
    cls_str = maj_ptr->cls->cls_name;
    
    /* Print error class header if new class */
    if(eprint->cls.lib_name==NULL || HDstrcmp(cls_ptr->lib_name, eprint->cls.lib_name)) {
        /* update to the new class information */
        if(cls_ptr->cls_name) eprint->cls.cls_name = cls_ptr->cls_name;
        if(cls_ptr->lib_name) eprint->cls.lib_name = cls_ptr->lib_name;
        if(cls_ptr->lib_vers) eprint->cls.lib_vers = cls_ptr->lib_vers;

        fprintf (stream, "%s-DIAG: Error detected in %s (%s) ", cls_ptr->cls_name, cls_ptr->lib_name, cls_ptr->lib_vers);
        
        /* try show the process or thread id in multiple processes cases*/
#ifdef H5_HAVE_PARALLEL
        {   int mpi_rank, mpi_initialized;
	    MPI_Initialized(&mpi_initialized);
	    if (mpi_initialized){
	        MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);
	        fprintf (stream, "MPI-process %d", mpi_rank);
	    }else
	        fprintf (stream, "thread 0");
        }
#elif defined(H5_HAVE_THREADSAFE)
        fprintf (stream, "thread %d", (int)pthread_self());
#else
        fprintf (stream, "thread 0");
#endif
        fprintf (stream, ":\n");
    }

    /* Check for "real" error description - used to format output more nicely */
    if(err_desc->desc==NULL || HDstrlen(err_desc->desc)==0)
        have_desc=0;

    /* Print error message */
    fprintf (stream, "%*s#%03u: %s line %u in %s()%s%s\n",
	     H5E_INDENT, "", n, err_desc->file_name, err_desc->line,
	     err_desc->func_name, (have_desc ? ": " : ""),
             (err_desc->desc ? err_desc->desc : ""));
    fprintf (stream, "%*smajor: %s\n", H5E_INDENT*2, "", maj_str);
    fprintf (stream, "%*sminor: %s\n", H5E_INDENT*2, "", min_str);

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
    H5E_t   *estack;            /* Error stack to operate on */
    herr_t ret_value=SUCCEED;   /* Return value */
    
    FUNC_ENTER_API(H5Eget_auto, FAIL);
    H5TRACE3("e","i*x*x",estack_id,func,client_data);
    
    if(estack_id == H5E_DEFAULT) {
    	if((estack = H5E_get_my_stack())==NULL)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get current error stack")
    } /* end if */
    else
        if((estack = H5I_object_verify(estack_id, H5I_ERROR_STACK))==NULL)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error stack ID")

    /* Get the automatic error reporting information */
    if(H5E_get_auto(estack, func, client_data)<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get automatic error info")

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
static herr_t
H5E_get_auto(H5E_t *estack, H5E_auto_t *func, void **client_data)
{
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5E_get_auto, FAIL);

    assert (estack);
        
    /* Retrieve the requested information */
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
    H5E_t   *estack;            /* Error stack to operate on */
    herr_t ret_value=SUCCEED;   /* Return value */
    
    FUNC_ENTER_API(H5Eset_auto, FAIL);
    H5TRACE3("e","ixx",estack_id,func,client_data);

    if(estack_id == H5E_DEFAULT) {
    	if((estack = H5E_get_my_stack())==NULL)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get current error stack")
    } /* end if */
    else
        if((estack = H5I_object_verify(estack_id, H5I_ERROR_STACK))==NULL)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error stack ID")
    
    /* Set the automatic error reporting information */
    if(H5E_set_auto(estack, func, client_data)<0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTSET, FAIL, "can't set automatic error info")

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
static herr_t
H5E_set_auto(H5E_t *estack, H5E_auto_t func, void *client_data)
{
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5E_set_auto, FAIL);

    assert(estack);

    /* Set the automatic error reporting info */
    estack->func = func;
    estack->auto_data = client_data;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5E_dump_api_stack
 *
 * Purpose:	Private function to dump the error stack during an error in
 *              an API function if a callback function is defined for the
 *              current error stack.
 *		
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, August 6, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5E_dump_api_stack(int is_api)
{
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5E_dump_api_stack, FAIL);

    /* Only dump the error stack during an API call */
    if(is_api) {
        H5E_t *estack = H5E_get_my_stack();

        assert(estack);
        if (estack->func)
            (void)((estack->func)(H5E_DEFAULT, estack->auto_data));
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value);
}
