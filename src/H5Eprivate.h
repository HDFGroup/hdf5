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
#ifdef TMP
/* HDF5 error class */
#define    H5E_CLS_NAME         "HDF5"
#define    H5E_CLS_LIB_NAME     "HDF5"
#define    H5E_CLS_LIB_VERS     ""              /* How to find out version number? */

/* HDF5 error class: major errors */
#define    H5E_NONE_MAJOR_MSG	        "No error"
#define    H5E_ARGS_MSG		"Function arguments"
#define    H5E_RESOURCE_MSG	        "Resource unavailable"
#define    H5E_INTERNAL_MSG	        "Internal HDF5 error"
#define    H5E_FILE_MSG		"File interface"
#define    H5E_IO_MSG		        "Low-level I/O layer"
#define    H5E_FUNC_MSG		"Function entry/exit"
#define    H5E_ATOM_MSG		"Atom layer"
#define    H5E_CACHE_MSG		"Meta data cache layer"
#define    H5E_BTREE_MSG		"B-tree layer"
#define    H5E_SYM_MSG		        "Symbol table layer"
#define    H5E_HEAP_MSG		"Heap layer"
#define    H5E_OHDR_MSG		"Object header layer"
#define    H5E_DATATYPE_MSG	        "Datatype interface"
#define    H5E_DATASPACE_MSG	        "Dataspace interface"
#define    H5E_DATASET_MSG	        "Dataset interface"
#define    H5E_STORAGE_MSG	        "Data storage layer"
#define    H5E_PLIST_MSG		"Property list interface"
#define    H5E_ATTR_MSG 		"Attribute layer"
#define    H5E_PLINE_MSG		"Data filters layer"
#define    H5E_EFL_MSG 		"External file list"
#define    H5E_REFERENCE_MSG	        "References layer"
#define    H5E_VFL_MSG		        "Virtual File Layer"
#define    H5E_TBBT_MSG		"Threaded, Balanced, Binary Trees"
#define    H5E_FPHDF5_MSG	        "Flexible Parallel HDF5"
#define    H5E_TST_MSG		        "Ternary Search Trees"
#define    H5E_RS_MSG		        "Reference Counted Strings"

/* HDF5 error class: minor errors */
#define    H5E_NONE_MINOR 	"No error"

    /* Argument errors */
#define    H5E_UNINITIALIZED_MSG "Information is uninitialized"
#define    H5E_UNSUPPORTED_MSG 	"Feature is unsupported"
#define    H5E_BADTYPE_MSG 	"Inappropriate type"
#define    H5E_BADRANGE_MSG 	"Out of range"
#define    H5E_BADVALUE_MSG 	"Bad value"

    /* Resource errors */
#define    H5E_NOSPACE_MSG 	"No space available for allocation"
#define    H5E_CANTCOPY_MSG 	"Unable to copy object"
#define    H5E_CANTFREE_MSG 	"Unable to free object"
#define    H5E_ALREADYEXISTS_MSG "Object already exists"
#define    H5E_CANTLOCK_MSG 	"Unable to lock object"
#define    H5E_CANTUNLOCK_MSG 	"Unable to unlock object"
#define    H5E_CANTGC_MSG 	"Unable to garbage collect"

    /* File accessability errors */
#define    H5E_FILEEXISTS_MSG 	"File already exists"
#define    H5E_FILEOPEN_MSG 	"File already open"
#define    H5E_CANTCREATE_MSG 	"Unable to create file"
#define    H5E_CANTOPENFILE_MSG 	"Unable to open file"
#define    H5E_CANTCLOSEFILE_MSG 	"Unable to close file"
#define    H5E_NOTHDF5_MSG 	"Not an HDF5 file"
#define    H5E_BADFILE_MSG 	"Bad file ID accessed"
#define    H5E_TRUNCATED_MSG 	"File has been truncated"
#define    H5E_MOUNT_MSG		"File mount error"

    /* Generic low-level file I/O errors */
#define    H5E_SEEKERROR_MSG	"Seek failed"
#define    H5E_READERROR_MSG 	"Read failed"
#define    H5E_WRITEERROR_MSG 	"Write failed"
#define    H5E_CLOSEERROR_MSG 	"Close failed"
#define    H5E_OVERFLOW_MSG 	"Address overflowed"
#define    H5E_FCNTL_MSG         "File control (fcntl) failed"

    /* Function entry/exit interface errors */
#define    H5E_CANTINIT_MSG 	"Unable to initialize object"
#define    H5E_ALREADYINIT_MSG 	"Object already initialized"
#define    H5E_CANTRELEASE_MSG 	"Unable to release object"

    /* Object atom related errors */
#define    H5E_BADATOM_MSG 	"Unable to find atom information (already closed?)"
#define    H5E_BADGROUP_MSG 	"Unable to find ID group information"
#define    H5E_CANTREGISTER_MSG 	"Unable to register new atom"
#define    H5E_CANTINC_MSG      	"Unable to increment reference count"
#define    H5E_CANTDEC_MSG      	"Unable to decrement reference count"
#define    H5E_NOIDS_MSG      	"Out of IDs for group"

    /* Cache related errors */
#define    H5E_CANTFLUSH_MSG 	"Unable to flush data from cache"
#define    H5E_CANTLOAD_MSG 	"Unable to load meta data into cache"
#define    H5E_PROTECT_MSG 	"Protected meta data error"
#define    H5E_NOTCACHED_MSG 	"Meta data not currently cached"

    /* B-tree related errors */
#define    H5E_NOTFOUND_MSG 	"Object not found"
#define    H5E_EXISTS_MSG 	"Object already exists"
#define    H5E_CANTENCODE_MSG 	"Unable to encode value"
#define    H5E_CANTDECODE_MSG 	"Unable to decode value"
#define    H5E_CANTSPLIT_MSG 	"Unable to split node"
#define    H5E_CANTINSERT_MSG 	"Unable to insert object"
#define    H5E_CANTLIST_MSG 	"Unable to list node"

    /* Object header related errors */
#define    H5E_LINKCOUNT_MSG 	"Bad object header link count"
#define    H5E_VERSION_MSG 	"Wrong version number"
#define    H5E_ALIGNMENT_MSG 	"Alignment error"
#define    H5E_BADMESG_MSG 	"Unrecognized message"
#define    H5E_CANTDELETE_MSG 	"Can't delete message"

    /* Group related errors */
#define    H5E_CANTOPENOBJ_MSG 	"Can't open object"
#define    H5E_COMPLEN_MSG 	"Name component is too long"
#define    H5E_CWG_MSG 		"Problem with current working group"
#define    H5E_LINK_MSG 		"Link count failure"
#define    H5E_SLINK_MSG		"Symbolic link error"

    /* Datatype conversion errors */
#define    H5E_CANTCONVERT_MSG	"Can't convert datatypes"
#define    H5E_BADSIZE_MSG	"Bad size for object"

    /* Dataspace errors */
#define    H5E_CANTCLIP_MSG	"Can't clip hyperslab region"
#define    H5E_CANTCOUNT_MSG	"Can't count elements"
#define    H5E_CANTSELECT_MSG    "Can't select hyperslab"
#define    H5E_CANTNEXT_MSG      "Can't move to next iterator location"
#define    H5E_BADSELECT_MSG     "Invalid selection"
#define    H5E_CANTCOMPARE_MSG   "Can't compare objects"

    /* Property list errors */
#define    H5E_CANTGET_MSG	"Can't get value"
#define    H5E_CANTSET_MSG	"Can't set value"
#define    H5E_DUPCLASS_MSG	"Duplicate class name in parent class"

    /* Parallel MPI errors */
#define    H5E_MPI_MSG		"Some MPI function failed"
#define    H5E_MPIERRSTR_MSG     "MPI Error String"

    /* FPHDF5 errors */
#define    H5E_CANTMAKETREE_MSG  "Can't create a binary tree node"
#define    H5E_CANTRECV_MSG      "Can't receive messages from processes"
#define    H5E_CANTSENDMDATA_MSG "Can't send metadata message"
#define    H5E_CANTCHANGE_MSG    "Can't register change with server"
#define    H5E_CANTALLOC_MSG     "Can't allocate from file"

    /* I/O pipeline errors */
#define    H5E_NOFILTER_MSG      "Requested filter is not available"
#define    H5E_CALLBACK_MSG      "Callback failed"
#define    H5E_CANAPPLY_MSG      "Error from filter \"can apply\" callback"
#define    H5E_SETLOCAL_MSG      "Error from filter \"set local\" callback"
#endif /* TMP */
#endif /* NEW_ERR */

/*
 * HERROR macro, used to facilitate error reporting between a FUNC_ENTER()
 * and a FUNC_LEAVE() within a function body.  The arguments are the major
 * error number, the minor error number, and a description of the error.
 */
#define HERROR(maj, min, str) H5E_push(maj, min, FUNC, __FILE__, __LINE__, str)

/*
 * HCOMMON_ERROR macro, used by HDONE_ERROR and HGOTO_ERROR
 * (Shouldn't need to be used outside this header file)
 */
#define HCOMMON_ERROR(maj, min, str)  				              \
   HERROR (maj, min, str);						      \
   if (H5_IS_API(FUNC) && H5E_auto_g)  					      \
       (void)((H5E_auto_g)(H5E_auto_data_g))

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

H5_DLL hid_t  H5E_register_class(const char *cls_name, const char *lib_name, 
                                const char *version);
H5_DLL hid_t  H5E_unregister_class(H5E_cls_t *cls);
H5_DLL herr_t H5E_push (H5E_major_t maj_num, H5E_minor_t min_num,
			 const char *func_name, const char *file_name,
			 unsigned line, const char *desc);
H5_DLL herr_t H5E_clear (void);
H5_DLL herr_t H5E_walk (H5E_direction_t dir, H5E_walk_t func,
			 void *client_data);

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
