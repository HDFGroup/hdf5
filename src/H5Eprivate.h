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
 *  Header file for error values, etc.
 */
#ifndef _H5Eprivate_H
#define _H5Eprivate_H

#include "H5Epublic.h"

/* Private headers needed by this file */
#include "H5private.h"

#define H5E_NSLOTS	32	/*number of slots in an error stack	     */

#ifndef NEW_ERR

#define H5E_RESERVED_ATOMS  0

typedef struct H5E_cls_t {
    char *cls_name;
    char *lib_name;
    char *lib_vers;
} H5E_cls_t;

typedef struct H5E_msg_t {
    char        *msg;
    H5E_type_t   type;
    H5E_cls_t   *cls;
} H5E_msg_t;

/* Printing information */
typedef struct H5E_print_t {
    FILE        *stream;
    H5E_cls_t   cls;
} H5E_print_t;

/*H5_DLLVAR herr_t (*H5E_auto_g_new)(hid_t estack, void *client_data);
H5_DLLVAR void *H5E_auto_data_g_new;
*/

/* An error stack */
typedef struct H5E_t_new {
    int	nused;			        /*num slots currently used in stack  */
    H5E_error_t_new slot[H5E_NSLOTS];	/*array of error records	     */
    H5E_auto_t  func;
    void *auto_data;
} H5E_t_new;

/* HDF5 error class */
#define    H5E_CLS_NAME         "HDF5"
#define    H5E_CLS_LIB_NAME     "HDF5"
#define    H5E_CLS_LIB_VERS     ""              /* How to find out version number? */

/* HDF5 error class: major errors */
#define    H5E_NONE_MAJOR_MSG_new	        "No error"
#define    H5E_MAJ_ARGS_MSG_new		"Function arguments"
#define    H5E_MAJ_RESOURCE_MSG_new        "Resource unavailable"
#define    H5E_MAJ_INTERNAL_MSG_new        "Internal HDF5 error"
#define    H5E_MAJ_FILE_MSG_new		"File interface"
#define    H5E_MAJ_IO_MSG_new		        "Low-level I/O layer"
#define    H5E_MAJ_FUNC_MSG_new		"Function entry/exit"
#define    H5E_MAJ_ATOM_MSG_new		"Atom layer"
#define    H5E_MAJ_CACHE_MSG_new		"Meta data cache layer"
#define    H5E_MAJ_BTREE_MSG_new		"B-tree layer"
#define    H5E_MAJ_SYM_MSG_new		        "Symbol table layer"
#define    H5E_MAJ_HEAP_MSG_new		"Heap layer"
#define    H5E_MAJ_OHDR_MSG_new		"Object header layer"
#define    H5E_MAJ_DATATYPE_MSG_new	        "Datatype interface"
#define    H5E_MAJ_DATASPACE_MSG_new	        "Dataspace interface"
#define    H5E_MAJ_DATASET_MSG_new	        "Dataset interface"
#define    H5E_MAJ_STORAGE_MSG_new	        "Data storage layer"
#define    H5E_MAJ_PLIST_MSG_new		"Property list interface"
#define    H5E_MAJ_ATTR_MSG_new		"Attribute layer"
#define    H5E_MAJ_PLINE_MSG_new		"Data filters layer"
#define    H5E_MAJ_EFL_MSG_new 		"External file list"
#define    H5E_MAJ_REFERENCE_MSG_new	        "References layer"
#define    H5E_MAJ_VFL_MSG_new		        "Virtual File Layer"
#define    H5E_MAJ_TBBT_MSG_new		"Threaded, Balanced, Binary Trees"
#define    H5E_MAJ_FPHDF5_MSG_new	        "Flexible Parallel HDF5"
#define    H5E_MAJ_TST_MSG_new		        "Ternary Search Trees"
#define    H5E_MAJ_RS_MSG_new		        "Reference Counted Strings"
#define    H5E_MAJ_ERROR_MSG_new		        "Error API"

/* HDF5 error class: minor errors */
    /* Argument errors */
#define    H5E_NONE_MINOR_MSG_new	"No error"
#define    H5E_MIN_UNINITIALIZED_MSG_new "Information is uninitialized"
#define    H5E_MIN_UNSUPPORTED_MSG_new 	"Feature is unsupported"
#define    H5E_MIN_BADTYPE_MSG_new 	"Inappropriate type"
#define    H5E_MIN_BADRANGE_MSG_new 	"Out of range"
#define    H5E_MIN_BADVALUE_MSG_new 	"Bad value"

    /* Resource errors */
#define    H5E_MIN_NOSPACE_MSG_new 	"No space available for allocation"
#define    H5E_MIN_CANTCOPY_MSG_new 	"Unable to copy object"
#define    H5E_MIN_CANTFREE_MSG_new 	"Unable to free object"
#define    H5E_MIN_ALREADYEXISTS_MSG_new "Object already exists"
#define    H5E_MIN_CANTLOCK_MSG_new 	"Unable to lock object"
#define    H5E_MIN_CANTUNLOCK_MSG_new 	"Unable to unlock object"
#define    H5E_MIN_CANTGC_MSG_new 	"Unable to garbage collect"

    /* File accessability errors */
#define    H5E_MIN_FILEEXISTS_MSG_new 	"File already exists"
#define    H5E_MIN_FILEOPEN_MSG_new 	"File already open"
#define    H5E_MIN_CANTCREATE_MSG_new 	"Unable to create file"
#define    H5E_MIN_CANTOPENFILE_MSG_new 	"Unable to open file"
#define    H5E_MIN_CANTCLOSEFILE_MSG_new 	"Unable to close file"
#define    H5E_MIN_NOTHDF5_MSG_new 	"Not an HDF5 file"
#define    H5E_MIN_BADFILE_MSG_new 	"Bad file ID accessed"
#define    H5E_MIN_TRUNCATED_MSG_new 	"File has been truncated"
#define    H5E_MIN_MOUNT_MSG_new		"File mount error"

    /* Generic low-level file I/O errors */
#define    H5E_MIN_SEEKERROR_MSG_new	"Seek failed"
#define    H5E_MIN_READERROR_MSG_new	"Read failed"
#define    H5E_MIN_WRITEERROR_MSG_new 	"Write failed"
#define    H5E_MIN_CLOSEERROR_MSG_new 	"Close failed"
#define    H5E_MIN_OVERFLOW_MSG_new 	"Address overflowed"
#define    H5E_MIN_FCNTL_MSG_new         "File control (fcntl) failed"

    /* Function entry/exit interface errors */
#define    H5E_MIN_CANTINIT_MSG_new 	"Unable to initialize object"
#define    H5E_MIN_ALREADYINIT_MSG_new 	"Object already initialized"
#define    H5E_MIN_CANTRELEASE_MSG_new 	"Unable to release object"

    /* Object atom related errors */
#define    H5E_MIN_BADATOM_MSG_new 	"Unable to find atom information (already closed?)"
#define    H5E_MIN_BADGROUP_MSG_new 	"Unable to find ID group information"
#define    H5E_MIN_CANTREGISTER_MSG_new 	"Unable to register new atom"
#define    H5E_MIN_CANTINC_MSG_new      	"Unable to increment reference count"
#define    H5E_MIN_CANTDEC_MSG_new      	"Unable to decrement reference count"
#define    H5E_MIN_NOIDS_MSG_new      	"Out of IDs for group"

    /* Cache related errors */
#define    H5E_MIN_CANTFLUSH_MSG_new 	"Unable to flush data from cache"
#define    H5E_MIN_CANTLOAD_MSG_new 	"Unable to load meta data into cache"
#define    H5E_MIN_PROTECT_MSG_new 	"Protected meta data error"
#define    H5E_MIN_NOTCACHED_MSG_new 	"Meta data not currently cached"

    /* B-tree related errors */
#define    H5E_MIN_NOTFOUND_MSG_new 	"Object not found"
#define    H5E_MIN_EXISTS_MSG_new 	"Object already exists"
#define    H5E_MIN_CANTENCODE_MSG_new 	"Unable to encode value"
#define    H5E_MIN_CANTDECODE_MSG_new 	"Unable to decode value"
#define    H5E_MIN_CANTSPLIT_MSG_new 	"Unable to split node"
#define    H5E_MIN_CANTINSERT_MSG_new 	"Unable to insert object"
#define    H5E_MIN_CANTLIST_MSG_new 	"Unable to list node"

    /* Object header related errors */
#define    H5E_MIN_LINKCOUNT_MSG_new 	"Bad object header link count"
#define    H5E_MIN_VERSION_MSG_new 	"Wrong version number"
#define    H5E_MIN_ALIGNMENT_MSG_new 	"Alignment error"
#define    H5E_MIN_BADMESG_MSG_new 	"Unrecognized message"
#define    H5E_MIN_CANTDELETE_MSG_new 	"Can't delete message"

    /* Group related errors */
#define    H5E_MIN_CANTOPENOBJ_MSG_new	"Can't open object"
#define    H5E_MIN_COMPLEN_MSG_new 	"Name component is too long"
#define    H5E_MIN_CWG_MSG_new 		"Problem with current working group"
#define    H5E_MIN_LINK_MSG_new 		"Link count failure"
#define    H5E_MIN_SLINK_MSG_new		"Symbolic link error"

    /* Datatype conversion errors */
#define    H5E_MIN_CANTCONVERT_MSG_new	"Can't convert datatypes"
#define    H5E_MIN_BADSIZE_MSG_new	"Bad size for object"

    /* Dataspace errors */
#define    H5E_MIN_CANTCLIP_MSG_new	"Can't clip hyperslab region"
#define    H5E_MIN_CANTCOUNT_MSG_new	"Can't count elements"
#define    H5E_MIN_CANTSELECT_MSG_new    "Can't select hyperslab"
#define    H5E_MIN_CANTNEXT_MSG_new      "Can't move to next iterator location"
#define    H5E_MIN_BADSELECT_MSG_new     "Invalid selection"
#define    H5E_MIN_CANTCOMPARE_MSG_new   "Can't compare objects"

    /* Property list errors */
#define    H5E_MIN_CANTGET_MSG_new	"Can't get value"
#define    H5E_MIN_CANTSET_MSG_new	"Can't set value"
#define    H5E_MIN_DUPCLASS_MSG_new	"Duplicate class name in parent class"

    /* Parallel MPI errors */
#define    H5E_MIN_MPI_MSG_new		"Some MPI function failed"
#define    H5E_MIN_MPIERRSTR_MSG_new     "MPI Error String"

    /* FPHDF5 errors */
#define    H5E_MIN_CANTMAKETREE_MSG_new "Can't create a binary tree node"
#define    H5E_MIN_CANTRECV_MSG_new      "Can't receive messages from processes"
#define    H5E_MIN_CANTSENDMDATA_MSG_new "Can't send metadata message"
#define    H5E_MIN_CANTCHANGE_MSG_new   "Can't register change with server"
#define    H5E_MIN_CANTALLOC_MSG_new     "Can't allocate from file"

    /* I/O pipeline errors */
#define    H5E_MIN_NOFILTER_MSG_new      "Requested filter is not available"
#define    H5E_MIN_CALLBACK_MSG_new      "Callback failed"
#define    H5E_MIN_CANAPPLY_MSG_new      "Error from filter \"can apply\" callback"
#define    H5E_MIN_SETLOCAL_MSG_new      "Error from filter \"set local\" callback"
#endif /* NEW_ERR */

/*
 * HERROR macro, used to facilitate error reporting between a FUNC_ENTER()
 * and a FUNC_LEAVE() within a function body.  The arguments are the major
 * error number, the minor error number, and a description of the error.
 */
#ifdef NEW_ERR
#define HERROR(maj_id, min_id, str) {                                         \
   H5E_msg_t    *maj_ptr, *min_ptr;                                           \
   hid_t        cls_id;                                                       \
   maj_ptr = H5I_object_verify(maj_id, H5I_ERROR_MSG);                        \
   min_ptr = H5I_object_verify(min_id, H5I_ERROR_MSG);                        \
   /*check error: cls of maj and min should be same*/                         \
   cls_id = H5I_register(H5I_ERROR_CLASS, maj_ptr->cls);                      \
   H5E_push_new(H5E_DEFAULT, __FILE__, FUNC, __LINE__, cls_id, maj_id, min_id, str); \
}
#else
#define HERROR(maj, min, str) H5E_push(maj, min, FUNC, __FILE__, __LINE__, str)
#endif /* NEW_ERR */

/*
 * HCOMMON_ERROR macro, used by HDONE_ERROR and HGOTO_ERROR
 * (Shouldn't need to be used outside this header file)
 */
#ifdef NEW_ERR
#define HCOMMON_ERROR(maj, min, str)  				              \
   H5E_t_new *estack = H5E_get_my_stack_new();                                \
   HERROR (maj, min, str);						      \
   if (H5_IS_API(FUNC) && estack->auto_data)  				      \
       (void)((estack->func)(H5E_DEFAULT, estack->auto_data))
#else
#define HCOMMON_ERROR(maj, min, str)  				              \
   HERROR (maj, min, str);						      \
   if (H5_IS_API(FUNC) && H5E_auto_g)  					      \
       (void)((H5E_auto_g)(H5E_auto_data_g))
#endif /* NEW_ERR */

/*
 * HDONE_ERROR macro, used to facilitate error reporting between a
 * FUNC_ENTER() and a FUNC_LEAVE() within a function body, but _AFTER_ the
 * "done:" label.  The arguments are
 * the major error number, the minor error number, a return value, and a
 * description of the error.
 */
#define HDONE_ERROR(maj, min, ret_val, str) {				      \
   HCOMMON_ERROR (maj, min, str);					      \
   ret_value = ret_val;                                                       \
}

/*
 * HGOTO_ERROR macro, used to facilitate error reporting between a
 * FUNC_ENTER() and a FUNC_LEAVE() within a function body.  The arguments are
 * the major error number, the minor error number, the return value, and an
 * error string.  The return value is assigned to a variable `ret_value' and
 * control branches to the `done' label.
 */
#define HGOTO_ERROR(maj, min, ret_val, str) {				      \
   HCOMMON_ERROR (maj, min, str);					      \
   HGOTO_DONE (ret_val)						              \
}

/*
 * HGOTO_DONE macro, used to facilitate normal return between a FUNC_ENTER()
 * and a FUNC_LEAVE() within a function body. The argument is the return
 * value which is assigned to the `ret_value' variable.	 Control branches to
 * the `done' label.
 */
#define HGOTO_DONE(ret_val) {ret_value = ret_val; goto done;}

/*
 * The list of error messages in the system is kept as an array of
 * error_code/message pairs, one for major error numbers and another for
 * minor error numbers.
 */
typedef struct H5E_major_mesg_t {
    H5E_major_t error_code;
    const char	*str;
} H5E_major_mesg_t;

typedef struct H5E_minor_mesg_t {
    H5E_minor_t error_code;
    const char	*str;
} H5E_minor_mesg_t;

/* An error stack */
typedef struct H5E_t {
    int	nused;			/*num slots currently used in stack  */
    H5E_error_t slot[H5E_NSLOTS];	/*array of error records	     */
} H5E_t;

H5_DLLVAR const hbool_t H5E_clearable_g;/*safe to call H5E_clear() on enter?*/
H5_DLLVAR herr_t (*H5E_auto_g)(void *client_data);
H5_DLLVAR void *H5E_auto_data_g;

H5_DLL herr_t H5E_push (H5E_major_t maj_num, H5E_minor_t min_num,
			 const char *func_name, const char *file_name,
			 unsigned line, const char *desc);
H5_DLL herr_t H5E_clear (void);
H5_DLL herr_t H5E_walk (H5E_direction_t dir, H5E_walk_t func,
			 void *client_data);

#ifndef NEW_ERR
/* New error API */
H5E_t_new *    H5E_get_stack_new(void);
H5_DLL hid_t   H5E_register_class(const char *cls_name, const char *lib_name, 
                                const char *version);
H5_DLL herr_t  H5E_unregister_class(H5E_cls_t *cls);
H5_DLL herr_t  H5E_close_msg(H5E_msg_t *err);
H5_DLL hid_t   H5E_create_msg(hid_t cls_id, H5E_type_t msg_type, const char *msg);
H5_DLL hid_t   H5E_get_current_stack(void);
H5_DLL herr_t  H5E_close_stack(H5E_t_new *err_stack);
H5_DLL ssize_t H5E_get_class_name(H5E_cls_t *cls, char *name, size_t size);
H5_DLL ssize_t H5E_get_msg(H5E_msg_t *msg_ptr, H5E_type_t *type, char *msg, size_t size);
H5_DLL int     H5E_get_num(H5E_t_new *err_stack);
H5_DLL herr_t  H5E_set_current_stack(H5E_t_new *estack);
H5_DLL herr_t  H5E_push_new(H5E_t_new *estack, const char *file, const char *func, unsigned line, 
                            hid_t cls_id, hid_t maj_id, hid_t min_id, const char *desc);
H5_DLL herr_t  H5E_pop(H5E_t_new *err_stack, size_t count);
H5_DLL herr_t  H5E_clear_new(H5E_t_new *estack);
H5_DLL herr_t  H5E_print_new(H5E_t_new *estack, FILE *stream);
H5_DLL herr_t  H5E_walk_new (H5E_t_new *estack, H5E_direction_t direction, H5E_walk_t_new func, 
                             void *client_data);
H5_DLL herr_t  H5E_get_auto_new(H5E_t_new *estack, H5E_auto_t *func, void **client_data);
H5_DLL herr_t  H5E_set_auto_new(H5E_t_new *estack, H5E_auto_t func, void *client_data);

#endif /* NEW_ERR */

#ifdef H5_HAVE_PARALLEL
/*
 * MPI error handling macros.
 */

extern	char	H5E_mpi_error_str[MPI_MAX_ERROR_STRING];
extern	int	H5E_mpi_error_str_len;

#define	HMPI_ERROR(mpierr){						      \
    MPI_Error_string(mpierr, H5E_mpi_error_str, &H5E_mpi_error_str_len);      \
    HERROR(H5E_INTERNAL, H5E_MPIERRSTR, H5E_mpi_error_str);                   \
}
#define	HMPI_DONE_ERROR(retcode, str, mpierr){				      \
    HMPI_ERROR(mpierr);							      \
    HDONE_ERROR(H5E_INTERNAL, H5E_MPI, retcode, str);			      \
}
#define	HMPI_GOTO_ERROR(retcode, str, mpierr){				      \
    HMPI_ERROR(mpierr);							      \
    HGOTO_ERROR(H5E_INTERNAL, H5E_MPI, retcode, str);			      \
}
#endif

#endif
