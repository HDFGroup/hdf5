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

static const H5E_major_mesg_t H5E_major_mesg_g[] = {
    {H5E_NONE_MAJOR,	"No error"},
    {H5E_ARGS,		"Function arguments"},
    {H5E_RESOURCE,	"Resource unavailable"},
    {H5E_INTERNAL,	"Internal HDF5 error"},
    {H5E_FILE,		"File interface"},
    {H5E_IO,		"Low-level I/O layer"},
    {H5E_FUNC,		"Function entry/exit"},
    {H5E_ATOM,		"Atom layer"},
    {H5E_CACHE,		"Meta data cache layer"},
    {H5E_BTREE,		"B-tree layer"},
    {H5E_SYM,		"Symbol table layer"},
    {H5E_HEAP,		"Heap layer"},
    {H5E_OHDR,		"Object header layer"},
    {H5E_DATATYPE,	"Datatype interface"},
    {H5E_DATASPACE,	"Dataspace interface"},
    {H5E_DATASET,	"Dataset interface"},
    {H5E_STORAGE,	"Data storage layer"},
    {H5E_PLIST,		"Property list interface"},
    {H5E_ATTR, 		"Attribute layer"},
    {H5E_PLINE,		"Data filters layer"},
    {H5E_EFL, 		"External file list"},
    {H5E_REFERENCE,	"References layer"},
    {H5E_VFL,		"Virtual File Layer"},
    {H5E_TBBT,		"Threaded, Balanced, Binary Trees"},
    {H5E_FPHDF5,	"Flexible Parallel HDF5"},
    {H5E_TST,		"Ternary Search Trees"},
    {H5E_RS,		"Reference Counted Strings"},
    {H5E_ERROR,		"Error API"},
};

static const H5E_minor_mesg_t H5E_minor_mesg_g[] = {
    {H5E_NONE_MINOR, 	"No error"},

    /* Argument errors */
    {H5E_UNINITIALIZED, "Information is uninitialized"},
    {H5E_UNSUPPORTED, 	"Feature is unsupported"},
    {H5E_BADTYPE, 	"Inappropriate type"},
    {H5E_BADRANGE, 	"Out of range"},
    {H5E_BADVALUE, 	"Bad value"},

    /* Resource errors */
    {H5E_NOSPACE, 	"No space available for allocation"},
    {H5E_CANTCOPY, 	"Unable to copy object"},
    {H5E_CANTFREE, 	"Unable to free object"},
    {H5E_ALREADYEXISTS, "Object already exists"},
    {H5E_CANTLOCK, 	"Unable to lock object"},
    {H5E_CANTUNLOCK, 	"Unable to unlock object"},
    {H5E_CANTGC, 	"Unable to garbage collect"},

    /* File accessability errors */
    {H5E_FILEEXISTS, 	"File already exists"},
    {H5E_FILEOPEN, 	"File already open"},
    {H5E_CANTCREATE, 	"Unable to create file"},
    {H5E_CANTOPENFILE, 	"Unable to open file"},
    {H5E_CANTCLOSEFILE, 	"Unable to close file"},
    {H5E_NOTHDF5, 	"Not an HDF5 file"},
    {H5E_BADFILE, 	"Bad file ID accessed"},
    {H5E_TRUNCATED, 	"File has been truncated"},
    {H5E_MOUNT,		"File mount error"},

    /* Generic low-level file I/O errors */
    {H5E_SEEKERROR,	"Seek failed"},
    {H5E_READERROR, 	"Read failed"},
    {H5E_WRITEERROR, 	"Write failed"},
    {H5E_CLOSEERROR, 	"Close failed"},
    {H5E_OVERFLOW, 	"Address overflowed"},
    {H5E_FCNTL,         "File control (fcntl) failed"},

    /* Function entry/exit interface errors */
    {H5E_CANTINIT, 	"Unable to initialize object"},
    {H5E_ALREADYINIT, 	"Object already initialized"},
    {H5E_CANTRELEASE, 	"Unable to release object"},

    /* Object atom related errors */
    {H5E_BADATOM, 	"Unable to find atom information (already closed?)"},
    {H5E_BADGROUP, 	"Unable to find ID group information"},
    {H5E_CANTREGISTER, 	"Unable to register new atom"},
    {H5E_CANTINC,      	"Unable to increment reference count"},
    {H5E_CANTDEC,      	"Unable to decrement reference count"},
    {H5E_NOIDS,      	"Out of IDs for group"},

    /* Cache related errors */
    {H5E_CANTFLUSH, 	"Unable to flush data from cache"},
    {H5E_CANTLOAD, 	"Unable to load meta data into cache"},
    {H5E_PROTECT, 	"Protected meta data error"},
    {H5E_NOTCACHED, 	"Meta data not currently cached"},

    /* B-tree related errors */
    {H5E_NOTFOUND, 	"Object not found"},
    {H5E_EXISTS, 	"Object already exists"},
    {H5E_CANTENCODE, 	"Unable to encode value"},
    {H5E_CANTDECODE, 	"Unable to decode value"},
    {H5E_CANTSPLIT, 	"Unable to split node"},
    {H5E_CANTINSERT, 	"Unable to insert object"},
    {H5E_CANTLIST, 	"Unable to list node"},

    /* Object header related errors */
    {H5E_LINKCOUNT, 	"Bad object header link count"},
    {H5E_VERSION, 	"Wrong version number"},
    {H5E_ALIGNMENT, 	"Alignment error"},
    {H5E_BADMESG, 	"Unrecognized message"},
    {H5E_CANTDELETE, 	"Can't delete message"},

    /* Group related errors */
    {H5E_CANTOPENOBJ, 	"Can't open object"},
    {H5E_COMPLEN, 	"Name component is too long"},
    {H5E_CWG, 		"Problem with current working group"},
    {H5E_LINK, 		"Link count failure"},
    {H5E_SLINK,		"Symbolic link error"},

    /* Datatype conversion errors */
    {H5E_CANTCONVERT,	"Can't convert datatypes"},
    {H5E_BADSIZE,	"Bad size for object"},

    /* Dataspace errors */
    {H5E_CANTCLIP,	"Can't clip hyperslab region"},
    {H5E_CANTCOUNT,	"Can't count elements"},
    {H5E_CANTSELECT,    "Can't select hyperslab"},
    {H5E_CANTNEXT,      "Can't move to next iterator location"},
    {H5E_BADSELECT,     "Invalid selection"},
    {H5E_CANTCOMPARE,   "Can't compare objects"},

    /* Property list errors */
    {H5E_CANTGET,	"Can't get value"},
    {H5E_CANTSET,	"Can't set value"},
    {H5E_DUPCLASS,	"Duplicate class name in parent class"},

    /* Parallel MPI errors */
    {H5E_MPI,		"Some MPI function failed"},
    {H5E_MPIERRSTR,     "MPI Error String"},

    /* FPHDF5 errors */
    {H5E_CANTMAKETREE,  "Can't create a binary tree node"},
    {H5E_CANTRECV,      "Can't receive messages from processes"},
    {H5E_CANTSENDMDATA, "Can't send metadata message"},
    {H5E_CANTCHANGE,    "Can't register change with server"},
    {H5E_CANTALLOC,     "Can't allocate from file"},

    /* I/O pipeline errors */
    {H5E_NOFILTER,      "Requested filter is not available"},
    {H5E_CALLBACK,      "Callback failed"},
    {H5E_CANAPPLY,      "Error from filter \"can apply\" callback"},
    {H5E_SETLOCAL,      "Error from filter \"set local\" callback"}

};

/* Interface initialization? */
static int interface_initialize_g = 0;
#define INTERFACE_INIT H5E_init_interface

#ifndef NEW_ERR
/*
 * Predefined errors. These are initialized at runtime in H5E_init_interface()
 * in this source file.
 *
 * If more of these are added, the new ones must be added to the list of
 * types to reset in H5E_term_interface().
 */
hid_t H5E_ERR_CLS_g			= FAIL;

#endif /* NEW_ERR */

#ifndef NEW_ERR
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
H5E_t_new *H5E_get_stack_new(void);
#define H5E_get_my_stack_new()  H5E_get_stack_new()
#else
/*
 * The error stack.  Eventually we'll have some sort of global table so each
 * thread has it's own stack.  The stacks will be created on demand when the
 * thread first calls H5E_push().  */
H5E_t_new		H5E_stack_g_new[1];
#define H5E_get_my_stack_new()	(H5E_stack_g_new+0)
#endif

#endif

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
H5E_t *H5E_get_stack(void);
#define H5E_get_my_stack()  H5E_get_stack()
#else
/*
 * The error stack.  Eventually we'll have some sort of global table so each
 * thread has it's own stack.  The stacks will be created on demand when the
 * thread first calls H5E_push().  */
H5E_t		H5E_stack_g[1];
#define H5E_get_my_stack()	(H5E_stack_g+0)
#endif

#ifdef H5_HAVE_PARALLEL
/*
 * variables used for MPI error reporting
 */
char	H5E_mpi_error_str[MPI_MAX_ERROR_STRING];
int	H5E_mpi_error_str_len;
#endif

/*
 * Automatic error stack traversal occurs if the traversal callback function
 * is non null and an API function is about to return an error.  These should
 * probably be part of the error stack so they're local to a thread.
 */
herr_t (*H5E_auto_g)(void*) = (herr_t(*)(void*))H5Eprint;
void *H5E_auto_data_g = NULL;


/* Static function declarations */
static herr_t H5E_init_interface (void);
#ifndef NEW_ERR
static int H5E_close_msg_cb(void *obj_ptr, hid_t obj_id, void *key);
#endif /* NEW_ERR */
static herr_t H5E_walk_cb (int n, H5E_error_t *err_desc, void *client_data);


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

#ifndef NEW_ERR

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

    /* From the old function; take out later */
    H5E_auto_data_g = stderr;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}

#else /* NEW_ERR */

/*-------------------------------------------------------------------------
 * Function:	H5E_init_interface
 *
 * Purpose:	Initialize the H5E interface. `stderr' is an extern or
 *		function on some systems so we can't initialize
 *		H5E_auto_data_g statically.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Friday, June 11, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5E_init_interface (void)
{
    FUNC_ENTER_NOINIT(H5E_init_interface);

    H5E_auto_data_g = stderr;

    FUNC_LEAVE_NOAPI(SUCCEED);
}
#endif /* NEW_ERR */

#ifndef NEW_ERR

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
            H5MM_xfree(cls->cls_name);
        if(cls->lib_name)
            H5MM_xfree(cls->lib_name);
        if(cls->lib_vers)
            H5MM_xfree(cls->lib_vers);
        H5MM_xfree(cls);
    }
    
done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Eget_class
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
H5Eget_class(hid_t class_id, char *name, size_t size)
{
    ssize_t      ret_value;   /* Return value */
    H5E_cls_t    *cls;

    FUNC_ENTER_API(H5Eget_class, FAIL);
    H5TRACE3("Zs","isz",class_id,name,size);
    
    /* Need to check for errors */
    cls = H5I_object_verify(class_id, H5I_ERROR_CLASS);

    ret_value = H5E_get_class(cls, name, size);

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5E_get_class
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
H5E_get_class(H5E_cls_t *cls, char *name, size_t size)
{
    ssize_t       ret_value;   /* Return value */
    ssize_t       len;
    
    FUNC_ENTER_NOAPI(H5E_get_class, FAIL);

    len = HDstrlen(cls->cls_name);

    if(name) {
       HDstrncpy(name, cls->cls_name, MIN(len+1, size));
       if(len >= size)
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
            H5MM_xfree(err->msg);
        H5MM_xfree(err);
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
       if(len >= size)
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
 * Function:	H5E_get_stack_new
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
H5E_t_new *
H5E_get_stack_new(void)
{
    H5E_t_new *estack;
    H5E_t *ret_value;   /* Return value */

    FUNC_ENTER_NOAPI(H5E_get_stack_new,NULL);

    estack = pthread_getspecific(H5TS_errstk_key_g);
    if (!estack) {
        /* no associated value with current thread - create one */
        estack = (H5E_t_new *)H5MM_malloc(sizeof(H5E_t_new));
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
    H5E_t_new	*current_stack = H5E_get_my_stack_new ();
    H5E_t_new   *estack_copy = H5MM_malloc(sizeof(H5E_t_new));
    H5E_error_t_new     *current_error, *new_error;
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
            H5MM_xfree(current_error->func_name);
        if(current_error->file_name)
            H5MM_xfree(current_error->file_name);
        if(current_error->desc)
            H5MM_xfree(current_error->desc);
    }
    HDmemset(current_stack->slot, 0, sizeof(H5E_error_t_new)*current_stack->nused);
    current_stack->nused = 0;
   
    /* Register the error stack to get an ID for it */
    /* Need to check for error */
    ret_value = H5I_register(H5I_ERROR_STACK, estack_copy);

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
    H5E_t_new    *err_stack;

    FUNC_ENTER_API(H5Eclose_stack, FAIL);
    H5TRACE1("e","i",stack_id);

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
H5E_close_stack(H5E_t_new *err_stack)
{
    herr_t       ret_value = SUCCEED;   /* Return value */
    H5E_error_t_new     *error;
    int          i;

    FUNC_ENTER_NOAPI(H5E_close_stack, FAIL);
    
    if(err_stack) {
        for(i=0; i<err_stack->nused; i++) {
            error = &(err_stack->slot[i]);

            if(error->func_name)
                H5MM_xfree(error->func_name);
            if(error->file_name)
                H5MM_xfree(error->file_name);
            if(error->desc)
                H5MM_xfree(error->desc);
        }
        H5MM_xfree(err_stack);
    }
    
done:
    FUNC_LEAVE_NOAPI(ret_value);
}

#endif /* NEW_ERR */


/*-------------------------------------------------------------------------
 * Function:	H5Eset_auto
 *
 * Purpose:	Turns on or off automatic printing of errors.  When turned on
 *		(non-null FUNC pointer) any API function which returns an
 *		error indication will first call FUNC passing it CLIENT_DATA
 *		as an argument.
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
H5Eset_auto(H5E_auto_t func, void *client_data)
{
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_API(H5Eset_auto, FAIL);
    H5TRACE2("e","xx",func,client_data);
    
    H5E_auto_g = func;
    H5E_auto_data_g = client_data;

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Eget_auto
 *
 * Purpose:	Returns the current settings for the automatic error stack
 *		traversal function and its data.  Either (or both) arguments
 *		may be null in which case the value is not returned.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Saturday, February 28, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Eget_auto(H5E_auto_t *func, void **client_data)
{
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_API(H5Eget_auto, FAIL);
    H5TRACE2("e","*x*x",func,client_data);

    if (func) *func = H5E_auto_g;
    if (client_data) *client_data = H5E_auto_data_g;

done:
    FUNC_LEAVE_API(ret_value);
}



/*-------------------------------------------------------------------------
 * Function:	H5Eclear
 *
 * Purpose:	Clears the error stack for the current thread.
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
H5Eclear(void)
{
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_API(H5Eclear, FAIL);
    H5TRACE0("e","");
    /* FUNC_ENTER() does all the work */

done:
    FUNC_LEAVE_API(ret_value);
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
 *-------------------------------------------------------------------------
 */
herr_t
H5Eprint(FILE *stream)
{
    H5E_t	*estack = H5E_get_my_stack ();
    herr_t	ret_value = FAIL;
    
    /* Don't clear the error stack! :-) */
    FUNC_ENTER_API_NOCLEAR(H5Eprint, FAIL);
    /*NO TRACE*/
    
    if (!stream) stream = stderr;
    fprintf (stream, "HDF5-DIAG: Error detected in %s ", H5_lib_vers_info_g);
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
    if (estack && estack->nused>0) fprintf (stream, "  Back trace follows.");
    HDfputc ('\n', stream);

    ret_value = H5E_walk (H5E_WALK_DOWNWARD, H5E_walk_cb, (void*)stream);
    
done:
    FUNC_LEAVE_API(ret_value);
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
 *-------------------------------------------------------------------------
 */
herr_t
H5Ewalk(H5E_direction_t direction, H5E_walk_t func, void *client_data)
{
    herr_t	ret_value;

    /* Don't clear the error stack! :-) */
    FUNC_ENTER_API_NOCLEAR(H5Ewalk, FAIL);
    H5TRACE3("e","Edxx",direction,func,client_data);

    ret_value = H5E_walk (direction, func, client_data);

done:
    FUNC_LEAVE_API(ret_value);
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
    FILE		*stream = (FILE *)client_data;
    const char		*maj_str = NULL;
    const char		*min_str = NULL;
    const int		indent = 2;

    FUNC_ENTER_NOINIT(H5E_walk_cb);
    /*NO TRACE*/

    /* Check arguments */
    assert (err_desc);
    if (!client_data) client_data = stderr;

    /* Get descriptions for the major and minor error numbers */
    maj_str = H5Eget_major (err_desc->maj_num);
    min_str = H5Eget_minor (err_desc->min_num);

    /* Print error message */
    fprintf (stream, "%*s#%03d: %s line %u in %s(): %s\n",
	     indent, "", n, err_desc->file_name, err_desc->line,
	     err_desc->func_name, err_desc->desc);
    fprintf (stream, "%*smajor(%02d): %s\n",
	     indent*2, "", err_desc->maj_num, maj_str);
    fprintf (stream, "%*sminor(%02d): %s\n",
	     indent*2, "", err_desc->min_num, min_str);

    FUNC_LEAVE_NOAPI(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Eget_major
 *
 * Purpose:	Given a major error number return a constant character string
 *		that describes the error.
 *
 * Return:	Success:	Ptr to a character string.
 *
 *		Failure:	Ptr to "Invalid major error number"
 *
 * Programmer:	Robb Matzke
 *              Friday, February 27, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
const char *
H5Eget_major (H5E_major_t n)
{
    unsigned	i;
    const char *ret_value="Invalid major error number";
    
    /*
     * WARNING: Do not call the FUNC_ENTER() or FUNC_LEAVE() macros since
     *		they might interact badly with the error stack.  We are
     *		probably calling this function during an error stack
     *		traversal and adding/removing entries as the result of an
     *		error would most likely mess things up.
     */
    FUNC_ENTER_API_NOINIT(H5Eget_major);

    for (i=0; i<NELMTS (H5E_major_mesg_g); i++) {
	if (H5E_major_mesg_g[i].error_code==n)
	    HGOTO_DONE(H5E_major_mesg_g[i].str);
    }

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Eget_minor
 *
 * Purpose:	Given a minor error number return a constant character string
 *		that describes the error.
 *
 * Return:	Success:	Ptr to a character string.
 *
 *		Failure:	Ptr to "Invalid minor error number"
 *
 * Programmer:	Robb Matzke
 *              Friday, February 27, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
const char *
H5Eget_minor (H5E_minor_t n)
{
    unsigned	i;
    const char *ret_value="Invalid minor error number";
    
    /*
     * WARNING: Do not call the FUNC_ENTER() or FUNC_LEAVE() macros since
     *		they might interact badly with the error stack.  We are
     *		probably calling this function during an error stack
     *		traversal and adding/removing entries as the result of an
     *		error would most likely mess things up.
     */
    FUNC_ENTER_API_NOINIT(H5Eget_minor);

    for (i=0; i<NELMTS (H5E_minor_mesg_g); i++) {
	if (H5E_minor_mesg_g[i].error_code==n)
	    HGOTO_DONE(H5E_minor_mesg_g[i].str);
    }

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5E_push
 *
 * Purpose:	Pushes a new error record onto error stack for the current
 *		thread.  The error has major and minor numbers MAJ_NUM and
 *		MIN_NUM, the name of a function where the error was detected,
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
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5E_push(H5E_major_t maj_num, H5E_minor_t min_num, const char *function_name,
	 const char *file_name, unsigned line, const char *desc)
{
    H5E_t	*estack = H5E_get_my_stack ();
    
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
    if (!function_name) function_name = "Unknown_Function";
    if (!file_name) file_name = "Unknown_File";
    if (!desc) desc = "No description given";

    /*
     * Push the error if there's room.  Otherwise just forget it.
     */
    assert (estack);
    if (estack->nused<H5E_NSLOTS) {
	estack->slot[estack->nused].maj_num = maj_num;
	estack->slot[estack->nused].min_num = min_num;
	estack->slot[estack->nused].func_name = function_name;
	estack->slot[estack->nused].file_name = file_name;
	estack->slot[estack->nused].line = line;
	estack->slot[estack->nused].desc = desc;
	estack->nused++;
    }
    
    FUNC_LEAVE_NOAPI(SUCCEED);
}


/*-------------------------------------------------------------------------
 * Function:	H5Epush
 *
 * Purpose:	Pushes a new error record onto error stack for the current
 *		thread.  The error has major and minor numbers MAJ_NUM and
 *		MIN_NUM, the name of a function where the error was detected,
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
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Epush(const char *file, const char *func, unsigned line, H5E_major_t maj,
	H5E_minor_t min, const char *str)
{
    herr_t	ret_value;
    
    FUNC_ENTER_API(H5Epush, FAIL);
    H5TRACE6("e","ssIuEjEns",file,func,line,maj,min,str);

    ret_value = H5E_push(maj, min, func, file, line, str);

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5E_clear
 *
 * Purpose:	Clears the error stack for the current thread.
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
H5E_clear(void)
{
    H5E_t	*estack = H5E_get_my_stack ();
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5E_clear, FAIL);

    if (estack) estack->nused = 0;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5E_walk
 *
 * Purpose:	Walks the error stack, calling the specified function for
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
 *-------------------------------------------------------------------------
 */
herr_t
H5E_walk (H5E_direction_t direction, H5E_walk_t func, void *client_data)
{
    H5E_t	*estack = H5E_get_my_stack ();
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
