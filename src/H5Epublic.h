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
 * This file contains public declarations for the H5E module.
 */
#ifndef _H5Epublic_H
#define _H5Epublic_H

#include <stdio.h>              /*FILE arg of H5Eprint()                     */

/* Public headers needed by this file */
#include "H5public.h"
#include "H5Ipublic.h"

/* Value for the default error stack */
#define H5E_DEFAULT             0

/* Limit of error strings recorded */
#define H5E_LEN                 128

/* Take out _new later */
typedef enum H5E_type_t {
    H5E_MAJOR,
    H5E_MINOR
} H5E_type_t;

/* Information about an error; element of error stack */
typedef struct H5E_error_t {
    hid_t       cls_id;         /*class ID                           */
    hid_t       maj_id;		/*major error ID		     */
    hid_t       min_id;		/*minor error number		     */
    const char	*func_name;   	/*function in which error occurred   */
    const char	*file_name;	/*file in which error occurred       */
    unsigned	line;		/*line in file where error occurs    */
    const char	*desc;		/*optional supplied description      */
} H5E_error_t;

/* When this header is included from H5Eprivate.h, don't make calls to H5open() */
#undef H5OPEN
#ifndef _H5Eprivate_H
#define H5OPEN          H5open(),
#else   /* _H5Eprivate_H */
#define H5OPEN
#endif  /* _H5Eprivate_H */

/* HDF5 error class */
#define H5E_ERR_CLS		(H5OPEN H5E_ERR_CLS_g)
H5_DLLVAR hid_t H5E_ERR_CLS_g;

/* HDF5 error class: major errors. */
#define H5E_ARGS                            (H5OPEN H5E_ARGS_g)
#define H5E_RESOURCE                        (H5OPEN H5E_RESOURCE_g)
#define H5E_INTERNAL                        (H5OPEN H5E_INTERNAL_g)
#define H5E_FILE                            (H5OPEN H5E_FILE_g)
#define H5E_IO                              (H5OPEN H5E_IO_g)
#define H5E_FUNC                            (H5OPEN H5E_FUNC_g)
#define H5E_ATOM                            (H5OPEN H5E_ATOM_g)
#define H5E_CACHE                           (H5OPEN H5E_CACHE_g)
#define H5E_BTREE                           (H5OPEN H5E_BTREE_g)
#define H5E_SYM                             (H5OPEN H5E_SYM_g)
#define H5E_HEAP                            (H5OPEN H5E_HEAP_g)
#define H5E_OHDR                            (H5OPEN H5E_OHDR_g)
#define H5E_DATATYPE                        (H5OPEN H5E_DATATYPE_g)
#define H5E_DATASPACE                       (H5OPEN H5E_DATASPACE_g)
#define H5E_DATASET                         (H5OPEN H5E_DATASET_g)
#define H5E_STORAGE                         (H5OPEN H5E_STORAGE_g)
#define H5E_PLIST                           (H5OPEN H5E_PLIST_g)
#define H5E_ATTR                            (H5OPEN H5E_ATTR_g)
#define H5E_PLINE                           (H5OPEN H5E_PLINE_g)
#define H5E_EFL                             (H5OPEN H5E_EFL_g)
#define H5E_REFERENCE                       (H5OPEN H5E_REFERENCE_g)
#define H5E_VFL                             (H5OPEN H5E_VFL_g)
#define H5E_TBBT                            (H5OPEN H5E_TBBT_g)
#define H5E_FPHDF5                          (H5OPEN H5E_FPHDF5_g)
#define H5E_TST                             (H5OPEN H5E_TST_g)
#define H5E_RS                              (H5OPEN H5E_RS_g)
#define H5E_ERROR                           (H5OPEN H5E_ERROR_g)

H5_DLLVAR hid_t    H5E_ARGS_g;                      /*invalid arguments to routine               */
H5_DLLVAR hid_t    H5E_RESOURCE_g;                  /*resource unavailable                       */
H5_DLLVAR hid_t    H5E_INTERNAL_g;                  /*Internal error (too specific to document in detail) */
H5_DLLVAR hid_t    H5E_FILE_g;                      /*file Accessability                         */
H5_DLLVAR hid_t    H5E_IO_g;                        /*Low-level I/O                              */
H5_DLLVAR hid_t    H5E_FUNC_g;                      /*function Entry/Exit                        */
H5_DLLVAR hid_t    H5E_ATOM_g;                      /*object Atom                                */
H5_DLLVAR hid_t    H5E_CACHE_g;                     /*object Cache                               */
H5_DLLVAR hid_t    H5E_BTREE_g;                     /*B-Tree Node                                */
H5_DLLVAR hid_t    H5E_SYM_g;                       /*symbol Table                               */
H5_DLLVAR hid_t    H5E_HEAP_g;                      /*Heap                                       */
H5_DLLVAR hid_t    H5E_OHDR_g;                      /*object Header                              */
H5_DLLVAR hid_t    H5E_DATATYPE_g;                  /*Datatype                                   */
H5_DLLVAR hid_t    H5E_DATASPACE_g;                 /*Dataspace                                  */
H5_DLLVAR hid_t    H5E_DATASET_g;                   /*Dataset                                    */
H5_DLLVAR hid_t    H5E_STORAGE_g;                   /*data storage                               */
H5_DLLVAR hid_t    H5E_PLIST_g;                     /*Property lists                             */
H5_DLLVAR hid_t    H5E_ATTR_g;                      /*Attribute                                  */
H5_DLLVAR hid_t    H5E_PLINE_g;                     /*Data filters                               */
H5_DLLVAR hid_t    H5E_EFL_g;                       /*External file list                         */
H5_DLLVAR hid_t    H5E_REFERENCE_g;                 /*References                                 */
H5_DLLVAR hid_t    H5E_VFL_g;		        /*Virtual File Layer			     */
H5_DLLVAR hid_t    H5E_TBBT_g; 		        /*Threaded, Balanced, Binary Trees           */
H5_DLLVAR hid_t    H5E_FPHDF5_g;		        /*Flexible Parallel HDF5                     */
H5_DLLVAR hid_t    H5E_TST_g; 		        /*Ternary Search Trees                       */
H5_DLLVAR hid_t    H5E_RS_g;  		        /*Reference Counted Strings                  */
H5_DLLVAR hid_t    H5E_ERROR_g;  		        /*Error API				     */

/* HDF5 error class: minor errors. */
         /* Argument errors */
#define H5E_UNINITIALIZED                   (H5OPEN H5E_UNINITIALIZED_g)
#define H5E_UNSUPPORTED                     (H5OPEN H5E_UNSUPPORTED_g)
#define H5E_BADTYPE                         (H5OPEN H5E_BADTYPE_g)
#define H5E_BADRANGE                        (H5OPEN H5E_BADRANGE_g)
#define H5E_BADVALUE                        (H5OPEN H5E_BADVALUE_g)

H5_DLLVAR hid_t    H5E_UNINITIALIZED_g;             /*information is unitialized                 */
H5_DLLVAR hid_t    H5E_UNSUPPORTED_g;               /*feature is unsupported                     */
H5_DLLVAR hid_t    H5E_BADTYPE_g;                   /*incorrect type found                       */
H5_DLLVAR hid_t    H5E_BADRANGE_g;                  /*argument out of range                      */
H5_DLLVAR hid_t    H5E_BADVALUE_g;                  /*bad value for argument                     */

         /* Resource errors */
#define H5E_NOSPACE                         (H5OPEN H5E_NOSPACE_g)
#define H5E_CANTCOPY                        (H5OPEN H5E_CANTCOPY_g)
#define H5E_CANTFREE                        (H5OPEN H5E_CANTFREE_g)
#define H5E_ALREADYEXISTS                   (H5OPEN H5E_ALREADYEXISTS_g)
#define H5E_CANTLOCK                        (H5OPEN H5E_CANTLOCK_g)
#define H5E_CANTUNLOCK                      (H5OPEN H5E_CANTUNLOCK_g)
#define H5E_CANTGC                          (H5OPEN H5E_CANTGC_g)

H5_DLLVAR hid_t    H5E_NOSPACE_g;                   /*no space available for allocation          */
H5_DLLVAR hid_t    H5E_CANTCOPY_g;                  /*unable to copy object                      */
H5_DLLVAR hid_t    H5E_CANTFREE_g;                  /*unable to free object                      */
H5_DLLVAR hid_t    H5E_ALREADYEXISTS_g;             /*Object already exists                      */
H5_DLLVAR hid_t    H5E_CANTLOCK_g;                  /*Unable to lock object                      */
H5_DLLVAR hid_t    H5E_CANTUNLOCK_g;                /*Unable to unlock object                    */
H5_DLLVAR hid_t    H5E_CANTGC_g;		        /*Unable to garbage collect                  */

    /* File accessability errors */
#define H5E_FILEEXISTS                      (H5OPEN H5E_FILEEXISTS_g)
#define H5E_FILEOPEN                        (H5OPEN H5E_FILEOPEN_g)
#define H5E_CANTCREATE                      (H5OPEN H5E_CANTCREATE_g)
#define H5E_CANTOPENFILE                    (H5OPEN H5E_CANTOPENFILE_g)
#define H5E_CANTCLOSEFILE                   (H5OPEN H5E_CANTCLOSEFILE_g)
#define H5E_NOTHDF5                         (H5OPEN H5E_NOTHDF5_g)
#define H5E_BADFILE                         (H5OPEN H5E_BADFILE_g)
#define H5E_TRUNCATED                       (H5OPEN H5E_TRUNCATED_g)
#define H5E_MOUNT                           (H5OPEN H5E_MOUNT_g)

H5_DLLVAR hid_t    H5E_FILEEXISTS_g;                /*file already exists                        */
H5_DLLVAR hid_t    H5E_FILEOPEN_g;                  /*file already open                          */
H5_DLLVAR hid_t    H5E_CANTCREATE_g;                /*Can't create file                          */
H5_DLLVAR hid_t    H5E_CANTOPENFILE_g;              /*Can't open file                            */
H5_DLLVAR hid_t    H5E_CANTCLOSEFILE_g;             /*Can't close file			     */
H5_DLLVAR hid_t    H5E_NOTHDF5_g;                   /*not an HDF5 format file                    */
H5_DLLVAR hid_t    H5E_BADFILE_g;                   /*bad file ID accessed                       */
H5_DLLVAR hid_t    H5E_TRUNCATED_g;                 /*file has been truncated                    */
H5_DLLVAR hid_t    H5E_MOUNT_g;			/*file mount error			     */

    /* Generic low-level file I/O errors */
#define H5E_SEEKERROR                       (H5OPEN H5E_SEEKERROR_g)
#define H5E_READERROR                       (H5OPEN H5E_READERROR_g)
#define H5E_WRITEERROR                      (H5OPEN H5E_WRITEERROR_g)
#define H5E_CLOSEERROR                      (H5OPEN H5E_CLOSEERROR_g)
#define H5E_OVERFLOW                        (H5OPEN H5E_OVERFLOW_g)
#define H5E_FCNTL                           (H5OPEN H5E_FCNTL_g)

H5_DLLVAR hid_t    H5E_SEEKERROR_g;                 /*seek failed                                */
H5_DLLVAR hid_t    H5E_READERROR_g;                 /*read failed                                */
H5_DLLVAR hid_t    H5E_WRITEERROR_g;                /*write failed                               */
H5_DLLVAR hid_t    H5E_CLOSEERROR_g;                /*close failed                               */
H5_DLLVAR hid_t    H5E_OVERFLOW_g;		        /*address overflowed			     */
H5_DLLVAR hid_t    H5E_FCNTL_g;                     /*file fcntl failed                          */

    /* Function entry/exit interface errors */
#define H5E_CANTINIT                        (H5OPEN H5E_CANTINIT_g)
#define H5E_ALREADYINIT                     (H5OPEN H5E_ALREADYINIT_g)
#define H5E_CANTRELEASE                     (H5OPEN H5E_CANTRELEASE_g)

H5_DLLVAR hid_t    H5E_CANTINIT_g;                  /*Can't initialize object                    */
H5_DLLVAR hid_t    H5E_ALREADYINIT_g;               /*object already initialized                 */
H5_DLLVAR hid_t    H5E_CANTRELEASE_g;               /*Can't release object                       */

    /* Object atom related errors */
#define H5E_BADATOM                         (H5OPEN H5E_BADATOM_g)
#define H5E_BADGROUP                        (H5OPEN H5E_BADGROUP_g)
#define H5E_CANTREGISTER                    (H5OPEN H5E_CANTREGISTER_g)
#define H5E_CANTINC                         (H5OPEN H5E_CANTINC_g)
#define H5E_CANTDEC                         (H5OPEN H5E_CANTDEC_g)
#define H5E_NOIDS                           (H5OPEN H5E_NOIDS_g)

H5_DLLVAR hid_t    H5E_BADATOM_g;                   /*Can't find atom information                */
H5_DLLVAR hid_t    H5E_BADGROUP_g;                  /*Can't find group information               */
H5_DLLVAR hid_t    H5E_CANTREGISTER_g;              /*Can't register new atom                    */
H5_DLLVAR hid_t    H5E_CANTINC_g;                   /*Can't increment reference count            */
H5_DLLVAR hid_t    H5E_CANTDEC_g;                   /*Can't decrement reference count            */
H5_DLLVAR hid_t    H5E_NOIDS_g;                     /*Out of IDs for group                       */

    /* Cache related errors */
#define H5E_CANTFLUSH                       (H5OPEN H5E_CANTFLUSH_g)
#define H5E_CANTLOAD                        (H5OPEN H5E_CANTLOAD_g)
#define H5E_PROTECT                         (H5OPEN H5E_PROTECT_g)
#define H5E_NOTCACHED                       (H5OPEN H5E_NOTCACHED_g)

H5_DLLVAR hid_t    H5E_CANTFLUSH_g;                 /*Can't flush object from cache              */
H5_DLLVAR hid_t    H5E_CANTLOAD_g;                  /*Can't load object into cache               */
H5_DLLVAR hid_t    H5E_PROTECT_g;                   /*protected object error                     */
H5_DLLVAR hid_t    H5E_NOTCACHED_g;                 /*object not currently cached                */

    /* B-tree related errors */
#define H5E_NOTFOUND                        (H5OPEN H5E_NOTFOUND_g)
#define H5E_EXISTS                          (H5OPEN H5E_EXISTS_g)
#define H5E_CANTENCODE                      (H5OPEN H5E_CANTENCODE_g)
#define H5E_CANTDECODE                      (H5OPEN H5E_CANTDECODE_g)
#define H5E_CANTSPLIT                       (H5OPEN H5E_CANTSPLIT_g)
#define H5E_CANTINSERT                      (H5OPEN H5E_CANTINSERT_g)
#define H5E_CANTLIST                        (H5OPEN H5E_CANTLIST_g)

H5_DLLVAR hid_t    H5E_NOTFOUND_g;                  /*object not found                           */
H5_DLLVAR hid_t    H5E_EXISTS_g;                    /*object already exists                      */
H5_DLLVAR hid_t    H5E_CANTENCODE_g;                /*Can't encode value                         */
H5_DLLVAR hid_t    H5E_CANTDECODE_g;                /*Can't decode value                         */
H5_DLLVAR hid_t    H5E_CANTSPLIT_g;                 /*Can't split node                           */
H5_DLLVAR hid_t    H5E_CANTINSERT_g;                /*Can't insert object                        */
H5_DLLVAR hid_t    H5E_CANTLIST_g;                  /*Can't list node                            */

    /* Object header related errors */
#define H5E_LINKCOUNT                       (H5OPEN H5E_LINKCOUNT_g)
#define H5E_VERSION                         (H5OPEN H5E_VERSION_g)
#define H5E_ALIGNMENT                       (H5OPEN H5E_ALIGNMENT_g)
#define H5E_BADMESG                         (H5OPEN H5E_BADMESG_g)
#define H5E_CANTDELETE                      (H5OPEN H5E_CANTDELETE_g)

H5_DLLVAR hid_t    H5E_LINKCOUNT_g;                 /*bad object header link count               */
H5_DLLVAR hid_t    H5E_VERSION_g;                   /*wrong version number                       */
H5_DLLVAR hid_t    H5E_ALIGNMENT_g;                 /*alignment error                            */
H5_DLLVAR hid_t    H5E_BADMESG_g;                   /*unrecognized message                       */
H5_DLLVAR hid_t    H5E_CANTDELETE_g;                /* Can't delete message                      */

    /* Group related errors */
#define H5E_CANTOPENOBJ                     (H5OPEN H5E_CANTOPENOBJ_g)
#define H5E_COMPLEN                         (H5OPEN H5E_COMPLEN_g)
#define H5E_CWG                             (H5OPEN H5E_CWG_g)
#define H5E_LINK                            (H5OPEN H5E_LINK_g)
#define H5E_SLINK                           (H5OPEN H5E_SLINK_g)

H5_DLLVAR hid_t    H5E_CANTOPENOBJ_g;               /*Can't open object                          */
H5_DLLVAR hid_t    H5E_COMPLEN_g;                   /*name component is too long                 */
H5_DLLVAR hid_t    H5E_CWG_g;                       /*problem with current working group         */
H5_DLLVAR hid_t    H5E_LINK_g;                      /*link count failure                         */
H5_DLLVAR hid_t    H5E_SLINK_g;			/*symbolic link error			     */

    /* Datatype conversion errors */
#define H5E_CANTCONVERT                     (H5OPEN H5E_CANTCONVERT_g)
#define H5E_BADSIZE                         (H5OPEN H5E_BADSIZE_g)

H5_DLLVAR hid_t    H5E_CANTCONVERT_g;               /*Can't convert datatypes                    */
H5_DLLVAR hid_t    H5E_BADSIZE_g;                   /*Bad size for object                        */

    /* Dataspace errors */
#define H5E_CANTCLIP                        (H5OPEN H5E_CANTCLIP_g)
#define H5E_CANTCOUNT                       (H5OPEN H5E_CANTCOUNT_g)
#define H5E_CANTSELECT                      (H5OPEN H5E_CANTSELECT_g)
#define H5E_CANTNEXT                        (H5OPEN H5E_CANTNEXT_g)
#define H5E_BADSELECT                       (H5OPEN H5E_BADSELECT_g)
#define H5E_CANTCOMPARE                     (H5OPEN H5E_CANTCOMPARE_g)

H5_DLLVAR hid_t    H5E_CANTCLIP_g;                  /*Can't clip hyperslab region                */
H5_DLLVAR hid_t    H5E_CANTCOUNT_g;                 /*Can't count elements                       */
H5_DLLVAR hid_t    H5E_CANTSELECT_g;                /*Can't select hyperslab                     */
H5_DLLVAR hid_t    H5E_CANTNEXT_g;                  /*Can't move to next iterator location       */
H5_DLLVAR hid_t    H5E_BADSELECT_g;                 /*Invalid selection                          */
H5_DLLVAR hid_t    H5E_CANTCOMPARE_g;               /*Can't compare objects                      */

    /* Property list errors */
#define H5E_CANTGET                         (H5OPEN H5E_CANTGET_g)
#define H5E_CANTSET                         (H5OPEN H5E_CANTSET_g)
#define H5E_DUPCLASS                        (H5OPEN H5E_DUPCLASS_g)

H5_DLLVAR hid_t    H5E_CANTGET_g;                   /*Can't get value                            */
H5_DLLVAR hid_t    H5E_CANTSET_g;                   /*Can't set value                            */
H5_DLLVAR hid_t    H5E_DUPCLASS_g;                  /*Duplicate class name in parent class */

    /* Parallel errors */
#define H5E_MPI                             (H5OPEN H5E_MPI_g)
#define H5E_MPIERRSTR                       (H5OPEN H5E_MPIERRSTR_g)

H5_DLLVAR hid_t    H5E_MPI_g;			/*some MPI function failed		     */
H5_DLLVAR hid_t    H5E_MPIERRSTR_g;		        /*MPI Error String 			     */

    /* FPHDF5 errors */
#define H5E_CANTMAKETREE                    (H5OPEN H5E_CANTMAKETREE_g)
#define H5E_CANTRECV                        (H5OPEN H5E_CANTRECV_g)
#define H5E_CANTSENDMDATA                   (H5OPEN H5E_CANTSENDMDATA_g)
#define H5E_CANTCHANGE                      (H5OPEN H5E_CANTCHANGE_g)
#define H5E_CANTALLOC                       (H5OPEN H5E_CANTALLOC_g)

H5_DLLVAR hid_t    H5E_CANTMAKETREE_g;              /*can't make a TBBT tree                     */
H5_DLLVAR hid_t    H5E_CANTRECV_g;                  /*can't receive messages from processes      */
H5_DLLVAR hid_t    H5E_CANTSENDMDATA_g;             /*can't send metadata message                */
H5_DLLVAR hid_t    H5E_CANTCHANGE_g;                /*can't register change on server            */
H5_DLLVAR hid_t    H5E_CANTALLOC_g;                 /*can't allocate from file                   */

    /* I/O pipeline errors */
#define H5E_NOFILTER                        (H5OPEN H5E_NOFILTER_g)
#define H5E_CALLBACK                        (H5OPEN H5E_CALLBACK_g)
#define H5E_CANAPPLY                        (H5OPEN H5E_CANAPPLY_g)
#define H5E_SETLOCAL                        (H5OPEN H5E_SETLOCAL_g)

H5_DLLVAR hid_t    H5E_NOFILTER_g;                  /*requested filter is not available          */
H5_DLLVAR hid_t    H5E_CALLBACK_g;                  /*callback failed                            */
H5_DLLVAR hid_t    H5E_CANAPPLY_g;                  /*error from filter "can apply" callback     */
H5_DLLVAR hid_t    H5E_SETLOCAL_g;                  /*error from filter "set local" callback     */

/*
 * One often needs to temporarily disable automatic error reporting when
 * trying something that's likely or expected to fail.  For instance, to
 * determine if an object exists one can call H5Gget_objinfo() which will fail if
 * the object doesn't exist.  The code to try can be nested between calls to
 * H5Eget_auto() and H5Eset_auto(), but it's easier just to use this macro
 * like:
 * 	H5E_BEGIN_TRY {
 *	    ...stuff here that's likely to fail...
 *      } H5E_END_TRY;
 *
 * Warning: don't break, return, or longjmp() from the body of the loop or
 *	    the error reporting won't be properly restored!
 */
#define H5E_BEGIN_TRY {							      \
    H5E_auto_t H5E_saved_efunc;						      \
    void *H5E_saved_edata;						      \
    H5Eget_auto(H5E_DEFAULT, &H5E_saved_efunc, &H5E_saved_edata);			      \
    H5Eset_auto(H5E_DEFAULT, NULL, NULL);

#define H5E_END_TRY							      \
    H5Eset_auto (H5E_DEFAULT, H5E_saved_efunc, H5E_saved_edata);			      \
}

/*
 * Public API Convenience Macros for Error reporting - Documented
 */
/* Use the Standard C __FILE__ & __LINE__ macros instead of typing them in */
#define H5Epush_sim(func,cls,maj,min,str) H5Epush(H5E_DEFAULT,__FILE__,func,__LINE__,cls,maj,min,str)

/*
 * Public API Convenience Macros for Error reporting - Undocumented
 */
/* Use the Standard C __FILE__ & __LINE__ macros instead of typing them in */
/*  And return after pushing error onto stack */
#define H5Epush_ret(func,cls,maj,min,str,ret) {         \
    H5Epush(H5E_DEFAULT,__FILE__,func,__LINE__,cls,maj,min,str);    \
    return(ret);                                    \
}

/* Use the Standard C __FILE__ & __LINE__ macros instead of typing them in */
/*  And goto a label after pushing error onto stack */
#define H5Epush_goto(func,cls,maj,min,str,label) {      \
    H5Epush(H5E_DEFAULT,__FILE__,func,__LINE__,cls,maj,min,str);    \
    goto label;                                   \
}

/* Error stack traversal direction */
typedef enum H5E_direction_t {
    H5E_WALK_UPWARD	= 0,		/*begin deep, end at API function    */
    H5E_WALK_DOWNWARD	= 1		/*begin at API function, end deep    */
} H5E_direction_t;


#ifdef __cplusplus
extern "C" {
#endif

/* Error stack traversal callback function pointers */
typedef herr_t (*H5E_walk_t)(unsigned n, H5E_error_t *err_desc, void *client_data);
typedef herr_t (*H5E_auto_t)(hid_t estack, void *client_data);

/* Public API functions */
H5_DLL hid_t  H5Eregister_class(const char *cls_name, const char *lib_name, const char *version);
H5_DLL herr_t H5Eunregister_class(hid_t class_id);
H5_DLL herr_t H5Eclose_msg(hid_t err_id);
H5_DLL hid_t  H5Ecreate_msg(hid_t cls, H5E_type_t msg_type, const char *msg);
H5_DLL hid_t  H5Eget_current_stack(void);
H5_DLL herr_t H5Eclose_stack(hid_t stack_id);
H5_DLL ssize_t H5Eget_class_name(hid_t class_id, char *name, size_t size);
H5_DLL ssize_t H5Eget_msg(hid_t msg_id, H5E_type_t *type, char *msg, size_t size);
H5_DLL int     H5Eget_num(hid_t error_stack_id);
H5_DLL herr_t  H5Eset_current_stack(hid_t err_stack_id);
H5_DLL herr_t  H5Epush(hid_t err_stack, const char *file, const char *func, unsigned line, 
                           hid_t cls_id, hid_t maj_id, hid_t min_id, const char *msg, ...);
H5_DLL herr_t  H5Epop(hid_t err_stack, size_t count);
H5_DLL herr_t  H5Eclear(hid_t err_stack);
H5_DLL herr_t  H5Eprint(hid_t err_stack, FILE *stream);
H5_DLL herr_t  H5Ewalk(hid_t err_stack, H5E_direction_t direction, H5E_walk_t func, 
                            void *client_data);
H5_DLL herr_t  H5Eget_auto(hid_t estack_id, H5E_auto_t *func, void **client_data);
H5_DLL herr_t  H5Eset_auto(hid_t estack_id, H5E_auto_t func, void *client_data);

#ifdef __cplusplus
}
#endif
#endif
