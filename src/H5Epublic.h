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

#ifndef NEW_ERR

#define H5E_DEFAULT             0

/* Take out _new later */
typedef enum H5E_type_t {
    H5E_LLIMIT_new   =-1,
    H5E_MAJOR_new,
    H5E_MINOR_new,
    H5E_ULIMIT_new
} H5E_type_t;

/* Information about an error; element of error stack */
typedef struct H5E_error_t_new {
    hid_t       cls_id;         /*class ID                           */
    hid_t       maj_id;		/*major error ID		     */
    hid_t       min_id;		/*minor error number		     */
    const char	*func_name;   	/*function in which error occurred   */
    const char	*file_name;	/*file in which error occurred       */
    unsigned	line;		/*line in file where error occurs    */
    const char	*desc;		/*optional supplied description      */
} H5E_error_t_new;

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
#define H5E_NONE_MAJOR_new                      (H5OPEN H5E_NONE_MAJOR_g)
#define H5E_ARGS_new                            (H5OPEN H5E_ARGS_g)
#define H5E_RESOURCE_new                        (H5OPEN H5E_RESOURCE_g)
#define H5E_INTERNAL_new                        (H5OPEN H5E_INTERNAL_g)
#define H5E_FILE_new                            (H5OPEN H5E_FILE_g)
#define H5E_IO_new                              (H5OPEN H5E_IO_g)
#define H5E_FUNC_new                            (H5OPEN H5E_FUNC_g)
#define H5E_ATOM_new                            (H5OPEN H5E_ATOM_g)
#define H5E_CACHE_new                           (H5OPEN H5E_CACHE_g)
#define H5E_BTREE_new                           (H5OPEN H5E_BTREE_g)
#define H5E_SYM_new                             (H5OPEN H5E_SYM_g)
#define H5E_HEAP_new                            (H5OPEN H5E_HEAP_g)
#define H5E_OHDR_new                            (H5OPEN H5E_OHDR_g)
#define H5E_DATATYPE_new                        (H5OPEN H5E_DATATYPE_g)
#define H5E_DATASPACE_new                       (H5OPEN H5E_DATASPACE_g)
#define H5E_DATASET_new                         (H5OPEN H5E_DATASET_g)
#define H5E_STORAGE_new                         (H5OPEN H5E_STORAGE_g)
#define H5E_PLIST_new                           (H5OPEN H5E_PLIST_g)
#define H5E_ATTR_new                            (H5OPEN H5E_ATTR_g)
#define H5E_PLINE_new                           (H5OPEN H5E_PLINE_g)
#define H5E_EFL_new                             (H5OPEN H5E_EFL_g)
#define H5E_REFERENCE_new                       (H5OPEN H5E_REFERENCE_g)
#define H5E_VFL_new                             (H5OPEN H5E_VFL_g)
#define H5E_TBBT_new                            (H5OPEN H5E_TBBT_g)
#define H5E_FPHDF5_new                          (H5OPEN H5E_FPHDF5_g)
#define H5E_TST_new                             (H5OPEN H5E_TST_g)
#define H5E_RS_new                              (H5OPEN H5E_RS_g)
#define H5E_ERROR_new                           (H5OPEN H5E_ERROR_g)

H5_DLLVAR hid_t    H5E_NONE_MAJOR_g;                /*special zero, no error                     */
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
#define H5E_NONE_MINOR_new                      (H5OPEN H5E_NONE_MINOR_g)
#define H5E_UNINITIALIZED_new                   (H5OPEN H5E_UNINITIALIZED_g)
#define H5E_UNSUPPORTED_new                     (H5OPEN H5E_UNSUPPORTED_g)
#define H5E_BADTYPE_new                         (H5OPEN H5E_BADTYPE_g)
#define H5E_BADRANGE_new                        (H5OPEN H5E_BADRANGE_g)
#define H5E_BADVALUE_new                        (H5OPEN H5E_BADVALUE_g)

H5_DLLVAR hid_t    H5E_NONE_MINOR_g;                /*special zero, no error                     */
H5_DLLVAR hid_t    H5E_UNINITIALIZED_g;             /*information is unitialized                 */
H5_DLLVAR hid_t    H5E_UNSUPPORTED_g;               /*feature is unsupported                     */
H5_DLLVAR hid_t    H5E_BADTYPE_g;                   /*incorrect type found                       */
H5_DLLVAR hid_t    H5E_BADRANGE_g;                  /*argument out of range                      */
H5_DLLVAR hid_t    H5E_BADVALUE_g;                  /*bad value for argument                     */

         /* Resource errors */
#define H5E_NOSPACE_new                         (H5OPEN H5E_NOSPACE_g)
#define H5E_CANTCOPY_new                        (H5OPEN H5E_CANTCOPY_g)
#define H5E_CANTFREE_new                        (H5OPEN H5E_CANTFREE_g)
#define H5E_ALREADYEXISTS_new                   (H5OPEN H5E_ALREADYEXISTS_g)
#define H5E_CANTLOCK_new                        (H5OPEN H5E_CANTLOCK_g)
#define H5E_CANTUNLOCK_new                      (H5OPEN H5E_CANTUNLOCK_g)
#define H5E_CANTGC_new                          (H5OPEN H5E_CANTGC_g)

H5_DLLVAR hid_t    H5E_NOSPACE_g;                   /*no space available for allocation          */
H5_DLLVAR hid_t    H5E_CANTCOPY_g;                  /*unable to copy object                      */
H5_DLLVAR hid_t    H5E_CANTFREE_g;                  /*unable to free object                      */
H5_DLLVAR hid_t    H5E_ALREADYEXISTS_g;             /*Object already exists                      */
H5_DLLVAR hid_t    H5E_CANTLOCK_g;                  /*Unable to lock object                      */
H5_DLLVAR hid_t    H5E_CANTUNLOCK_g;                /*Unable to unlock object                    */
H5_DLLVAR hid_t    H5E_CANTGC_g;		        /*Unable to garbage collect                  */

    /* File accessability errors */
#define H5E_FILEEXISTS_new                      (H5OPEN H5E_FILEEXISTS_g)
#define H5E_FILEOPEN_new                        (H5OPEN H5E_FILEOPEN_g)
#define H5E_CANTCREATE_new                      (H5OPEN H5E_CANTCREATE_g)
#define H5E_CANTOPENFILE_new                    (H5OPEN H5E_CANTOPENFILE_g)
#define H5E_CANTCLOSEFILE_new                   (H5OPEN H5E_CANTCLOSEFILE_g)
#define H5E_NOTHDF5_new                         (H5OPEN H5E_NOTHDF5_g)
#define H5E_BADFILE_new                         (H5OPEN H5E_BADFILE_g)
#define H5E_TRUNCATED_new                       (H5OPEN H5E_TRUNCATED_g)
#define H5E_MOUNT_new                           (H5OPEN H5E_MOUNT_g)

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
#define H5E_SEEKERROR_new                       (H5OPEN H5E_SEEKERROR_g)
#define H5E_READERROR_new                       (H5OPEN H5E_READERROR_g)
#define H5E_WRITEERROR_new                      (H5OPEN H5E_WRITEERROR_g)
#define H5E_CLOSEERROR_new                      (H5OPEN H5E_CLOSEERROR_g)
#define H5E_OVERFLOW_new                        (H5OPEN H5E_OVERFLOW_g)
#define H5E_FCNTL_new                           (H5OPEN H5E_FCNTL_g)

H5_DLLVAR hid_t    H5E_SEEKERROR_g;                 /*seek failed                                */
H5_DLLVAR hid_t    H5E_READERROR_g;                 /*read failed                                */
H5_DLLVAR hid_t    H5E_WRITEERROR_g;                /*write failed                               */
H5_DLLVAR hid_t    H5E_CLOSEERROR_g;                /*close failed                               */
H5_DLLVAR hid_t    H5E_OVERFLOW_g;		        /*address overflowed			     */
H5_DLLVAR hid_t    H5E_FCNTL_g;                     /*file fcntl failed                          */

    /* Function entry/exit interface errors */
#define H5E_CANTINIT_new                        (H5OPEN H5E_CANTINIT_g)
#define H5E_ALREADYINIT_new                     (H5OPEN H5E_ALREADYINIT_g)
#define H5E_CANTRELEASE_new                     (H5OPEN H5E_CANTRELEASE_g)

H5_DLLVAR hid_t    H5E_CANTINIT_g;                  /*Can't initialize object                    */
H5_DLLVAR hid_t    H5E_ALREADYINIT_g;               /*object already initialized                 */
H5_DLLVAR hid_t    H5E_CANTRELEASE_g;               /*Can't release object                       */

    /* Object atom related errors */
#define H5E_BADATOM_new                         (H5OPEN H5E_BADATOM_g)
#define H5E_BADGROUP_new                        (H5OPEN H5E_BADGROUP_g)
#define H5E_CANTREGISTER_new                    (H5OPEN H5E_CANTREGISTER_g)
#define H5E_CANTINC_new                         (H5OPEN H5E_CANTINC_g)
#define H5E_CANTDEC_new                         (H5OPEN H5E_CANTDEC_g)
#define H5E_NOIDS_new                           (H5OPEN H5E_NOIDS_g)

H5_DLLVAR hid_t    H5E_BADATOM_g;                   /*Can't find atom information                */
H5_DLLVAR hid_t    H5E_BADGROUP_g;                  /*Can't find group information               */
H5_DLLVAR hid_t    H5E_CANTREGISTER_g;              /*Can't register new atom                    */
H5_DLLVAR hid_t    H5E_CANTINC_g;                   /*Can't increment reference count            */
H5_DLLVAR hid_t    H5E_CANTDEC_g;                   /*Can't decrement reference count            */
H5_DLLVAR hid_t    H5E_NOIDS_g;                     /*Out of IDs for group                       */

    /* Cache related errors */
#define H5E_CANTFLUSH_new                       (H5OPEN H5E_CANTFLUSH_g)
#define H5E_CANTLOAD_new                        (H5OPEN H5E_CANTLOAD_g)
#define H5E_PROTECT_new                         (H5OPEN H5E_PROTECT_g)
#define H5E_NOTCACHED_new                       (H5OPEN H5E_NOTCACHED_g)

H5_DLLVAR hid_t    H5E_CANTFLUSH_g;                 /*Can't flush object from cache              */
H5_DLLVAR hid_t    H5E_CANTLOAD_g;                  /*Can't load object into cache               */
H5_DLLVAR hid_t    H5E_PROTECT_g;                   /*protected object error                     */
H5_DLLVAR hid_t    H5E_NOTCACHED_g;                 /*object not currently cached                */

    /* B-tree related errors */
#define H5E_NOTFOUND_new                        (H5OPEN H5E_NOTFOUND_g)
#define H5E_EXISTS_new                          (H5OPEN H5E_EXISTS_g)
#define H5E_CANTENCODE_new                      (H5OPEN H5E_CANTENCODE_g)
#define H5E_CANTDECODE_new                      (H5OPEN H5E_CANTDECODE_g)
#define H5E_CANTSPLIT_new                       (H5OPEN H5E_CANTSPLIT_g)
#define H5E_CANTINSERT_new                      (H5OPEN H5E_CANTINSERT_g)
#define H5E_CANTLIST_new                        (H5OPEN H5E_CANTLIST_g)

H5_DLLVAR hid_t    H5E_NOTFOUND_g;                  /*object not found                           */
H5_DLLVAR hid_t    H5E_EXISTS_g;                    /*object already exists                      */
H5_DLLVAR hid_t    H5E_CANTENCODE_g;                /*Can't encode value                         */
H5_DLLVAR hid_t    H5E_CANTDECODE_g;                /*Can't decode value                         */
H5_DLLVAR hid_t    H5E_CANTSPLIT_g;                 /*Can't split node                           */
H5_DLLVAR hid_t    H5E_CANTINSERT_g;                /*Can't insert object                        */
H5_DLLVAR hid_t    H5E_CANTLIST_g;                  /*Can't list node                            */

    /* Object header related errors */
#define H5E_LINKCOUNT_new                       (H5OPEN H5E_LINKCOUNT_g)
#define H5E_VERSION_new                         (H5OPEN H5E_VERSION_g)
#define H5E_ALIGNMENT_new                       (H5OPEN H5E_ALIGNMENT_g)
#define H5E_BADMESG_new                         (H5OPEN H5E_BADMESG_g)
#define H5E_CANTDELETE_new                      (H5OPEN H5E_CANTDELETE_g)

H5_DLLVAR hid_t    H5E_LINKCOUNT_g;                 /*bad object header link count               */
H5_DLLVAR hid_t    H5E_VERSION_g;                   /*wrong version number                       */
H5_DLLVAR hid_t    H5E_ALIGNMENT_g;                 /*alignment error                            */
H5_DLLVAR hid_t    H5E_BADMESG_g;                   /*unrecognized message                       */
H5_DLLVAR hid_t    H5E_CANTDELETE_g;                /* Can't delete message                      */

    /* Group related errors */
#define H5E_CANTOPENOBJ_new                     (H5OPEN H5E_CANTOPENOBJ_g)
#define H5E_COMPLEN_new                         (H5OPEN H5E_COMPLEN_g)
#define H5E_CWG_new                             (H5OPEN H5E_CWG_g)
#define H5E_LINK_new                            (H5OPEN H5E_LINK_g)
#define H5E_SLINK_new                           (H5OPEN H5E_SLINK_g)

H5_DLLVAR hid_t    H5E_CANTOPENOBJ_g;               /*Can't open object                          */
H5_DLLVAR hid_t    H5E_COMPLEN_g;                   /*name component is too long                 */
H5_DLLVAR hid_t    H5E_CWG_g;                       /*problem with current working group         */
H5_DLLVAR hid_t    H5E_LINK_g;                      /*link count failure                         */
H5_DLLVAR hid_t    H5E_SLINK_g;			/*symbolic link error			     */

    /* Datatype conversion errors */
#define H5E_CANTCONVERT_new                     (H5OPEN H5E_CANTCONVERT_g)
#define H5E_BADSIZE_new                         (H5OPEN H5E_BADSIZE_g)

H5_DLLVAR hid_t    H5E_CANTCONVERT_g;               /*Can't convert datatypes                    */
H5_DLLVAR hid_t    H5E_BADSIZE_g;                   /*Bad size for object                        */

    /* Dataspace errors */
#define H5E_CANTCLIP_new                        (H5OPEN H5E_CANTCLIP_g)
#define H5E_CANTCOUNT_new                       (H5OPEN H5E_CANTCOUNT_g)
#define H5E_CANTSELECT_new                      (H5OPEN H5E_CANTSELECT_g)
#define H5E_CANTNEXT_new                        (H5OPEN H5E_CANTNEXT_g)
#define H5E_BADSELECT_new                       (H5OPEN H5E_BADSELECT_g)
#define H5E_CANTCOMPARE_new                     (H5OPEN H5E_CANTCOMPARE_g)

H5_DLLVAR hid_t    H5E_CANTCLIP_g;                  /*Can't clip hyperslab region                */
H5_DLLVAR hid_t    H5E_CANTCOUNT_g;                 /*Can't count elements                       */
H5_DLLVAR hid_t    H5E_CANTSELECT_g;                /*Can't select hyperslab                     */
H5_DLLVAR hid_t    H5E_CANTNEXT_g;                  /*Can't move to next iterator location       */
H5_DLLVAR hid_t    H5E_BADSELECT_g;                 /*Invalid selection                          */
H5_DLLVAR hid_t    H5E_CANTCOMPARE_g;               /*Can't compare objects                      */

    /* Property list errors */
#define H5E_CANTGET_new                         (H5OPEN H5E_CANTGET_g)
#define H5E_CANTSET_new                         (H5OPEN H5E_CANTSET_g)
#define H5E_DUPCLASS_new                        (H5OPEN H5E_DUPCLASS_g)

H5_DLLVAR hid_t    H5E_CANTGET_g;                   /*Can't get value                            */
H5_DLLVAR hid_t    H5E_CANTSET_g;                   /*Can't set value                            */
H5_DLLVAR hid_t    H5E_DUPCLASS_g;                  /*Duplicate class name in parent class */

    /* Parallel errors */
#define H5E_MPI_new                             (H5OPEN H5E_MPI_g)
#define H5E_MPIERRSTR_new                       (H5OPEN H5E_MPIERRSTR_g)

H5_DLLVAR hid_t    H5E_MPI_g;			/*some MPI function failed		     */
H5_DLLVAR hid_t    H5E_MPIERRSTR_g;		        /*MPI Error String 			     */

    /* FPHDF5 errors */
#define H5E_CANTMAKETREE_new                    (H5OPEN H5E_CANTMAKETREE_g)
#define H5E_CANTRECV_new                        (H5OPEN H5E_CANTRECV_g)
#define H5E_CANTSENDMDATA_new                   (H5OPEN H5E_CANTSENDMDATA_g)
#define H5E_CANTCHANGE_new                      (H5OPEN H5E_CANTCHANGE_g)
#define H5E_CANTALLOC_new                       (H5OPEN H5E_CANTALLOC_g)

H5_DLLVAR hid_t    H5E_CANTMAKETREE_g;              /*can't make a TBBT tree                     */
H5_DLLVAR hid_t    H5E_CANTRECV_g;                  /*can't receive messages from processes      */
H5_DLLVAR hid_t    H5E_CANTSENDMDATA_g;             /*can't send metadata message                */
H5_DLLVAR hid_t    H5E_CANTCHANGE_g;                /*can't register change on server            */
H5_DLLVAR hid_t    H5E_CANTALLOC_g;                 /*can't allocate from file                   */

    /* I/O pipeline errors */
#define H5E_NOFILTER_new                        (H5OPEN H5E_NOFILTER_g)
#define H5E_CALLBACK_new                        (H5OPEN H5E_CALLBACK_g)
#define H5E_CANAPPLY_new                        (H5OPEN H5E_CANAPPLY_g)
#define H5E_SETLOCAL_new                        (H5OPEN H5E_SETLOCAL_g)

H5_DLLVAR hid_t    H5E_NOFILTER_g;                  /*requested filter is not available          */
H5_DLLVAR hid_t    H5E_CALLBACK_g;                  /*callback failed                            */
H5_DLLVAR hid_t    H5E_CANAPPLY_g;                  /*error from filter "can apply" callback     */
H5_DLLVAR hid_t    H5E_SETLOCAL_g;                  /*error from filter "set local" callback     */

#endif /* NEW_ERR */

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
    H5Eget_auto (&H5E_saved_efunc, &H5E_saved_edata);			      \
    H5Eset_auto (NULL, NULL);

#define H5E_END_TRY							      \
    H5Eset_auto (H5E_saved_efunc, H5E_saved_edata);			      \
}

/*
 * Public API Convenience Macros for Error reporting - Documented
 */
/* Use the Standard C __FILE__ & __LINE__ macros instead of typing them in */
#define H5Epush_sim(func,maj,min,str) H5Epush(__FILE__,func,__LINE__,maj,min,str)

/*
 * Public API Convenience Macros for Error reporting - Undocumented
 */
/* Use the Standard C __FILE__ & __LINE__ macros instead of typing them in */
/*  And return after pushing error onto stack */
#define H5Epush_ret(func,maj,min,str,ret) {         \
    H5Epush(__FILE__,func,__LINE__,maj,min,str);    \
    return(ret);                                    \
}

/* Use the Standard C __FILE__ & __LINE__ macros instead of typing them in */
/*  And goto a label after pushing error onto stack */
#define H5Epush_goto(func,maj,min,str,label) {      \
    H5Epush(__FILE__,func,__LINE__,maj,min,str);    \
    goto label;                                   \
}

/*
 * Declare an enumerated type which holds all the valid major HDF error codes.
 */
typedef enum H5E_major_t {
    H5E_NONE_MAJOR       = 0,   /*special zero, no error                     */
    H5E_ARGS,                   /*invalid arguments to routine               */
    H5E_RESOURCE,               /*resource unavailable                       */
    H5E_INTERNAL,               /* Internal error (too specific to document
                                 * in detail)
                                 */
    H5E_FILE,                   /*file Accessability                         */
    H5E_IO,                     /*Low-level I/O                              */
    H5E_FUNC,                   /*function Entry/Exit                        */
    H5E_ATOM,                   /*object Atom                                */
    H5E_CACHE,                  /*object Cache                               */
    H5E_BTREE,                  /*B-Tree Node                                */
    H5E_SYM,                    /*symbol Table                               */
    H5E_HEAP,                   /*Heap                                       */
    H5E_OHDR,                   /*object Header                              */
    H5E_DATATYPE,               /*Datatype                                   */
    H5E_DATASPACE,              /*Dataspace                                  */
    H5E_DATASET,                /*Dataset                                    */
    H5E_STORAGE,                /*data storage                               */
    H5E_PLIST,                  /*Property lists                             */
    H5E_ATTR,                   /*Attribute                                  */
    H5E_PLINE,                  /*Data filters                               */
    H5E_EFL,                    /*External file list                         */
    H5E_REFERENCE,              /*References                                 */
    H5E_VFL,			/*Virtual File Layer			     */
    H5E_TBBT, 		        /*Threaded, Balanced, Binary Trees           */
    H5E_FPHDF5,		        /*Flexible Parallel HDF5                     */
    H5E_TST, 		        /*Ternary Search Trees                       */
    H5E_RS, 		        /*Reference Counted Strings                  */
    H5E_ERROR 		        /*Error API				     */
} H5E_major_t;

/* Declare an enumerated type which holds all the valid minor HDF error codes */
typedef enum H5E_minor_t {
    H5E_NONE_MINOR       = 0,   /*special zero, no error                     */

    /* Argument errors */
    H5E_UNINITIALIZED,          /*information is unitialized                 */
    H5E_UNSUPPORTED,            /*feature is unsupported                     */
    H5E_BADTYPE,                /*incorrect type found                       */
    H5E_BADRANGE,               /*argument out of range                      */
    H5E_BADVALUE,               /*bad value for argument                     */

    /* Resource errors */
    H5E_NOSPACE,                /*no space available for allocation          */
    H5E_CANTCOPY,               /*unable to copy object                      */
    H5E_CANTFREE,               /*unable to free object                      */
    H5E_ALREADYEXISTS,          /*Object already exists                      */
    H5E_CANTLOCK,               /*Unable to lock object                      */
    H5E_CANTUNLOCK,             /*Unable to unlock object                    */
    H5E_CANTGC,			/*Unable to garbage collect                  */

    /* File accessability errors */
    H5E_FILEEXISTS,             /*file already exists                        */
    H5E_FILEOPEN,               /*file already open                          */
    H5E_CANTCREATE,             /*Can't create file                          */
    H5E_CANTOPENFILE,           /*Can't open file                            */
    H5E_CANTCLOSEFILE,          /*Can't close file			     */
    H5E_NOTHDF5,                /*not an HDF5 format file                    */
    H5E_BADFILE,                /*bad file ID accessed                       */
    H5E_TRUNCATED,              /*file has been truncated                    */
    H5E_MOUNT,			/*file mount error			     */

    /* Generic low-level file I/O errors */
    H5E_SEEKERROR,              /*seek failed                                */
    H5E_READERROR,              /*read failed                                */
    H5E_WRITEERROR,             /*write failed                               */
    H5E_CLOSEERROR,             /*close failed                               */
    H5E_OVERFLOW,		/*address overflowed			     */
    H5E_FCNTL,                  /*file fcntl failed                          */

    /* Function entry/exit interface errors */
    H5E_CANTINIT,               /*Can't initialize object                    */
    H5E_ALREADYINIT,            /*object already initialized                 */
    H5E_CANTRELEASE,            /*Can't release object                       */

    /* Object atom related errors */
    H5E_BADATOM,                /*Can't find atom information                */
    H5E_BADGROUP,               /*Can't find group information               */
    H5E_CANTREGISTER,           /*Can't register new atom                    */
    H5E_CANTINC,                /*Can't increment reference count            */
    H5E_CANTDEC,                /*Can't decrement reference count            */
    H5E_NOIDS,                  /*Out of IDs for group                       */

    /* Cache related errors */
    H5E_CANTFLUSH,              /*Can't flush object from cache              */
    H5E_CANTLOAD,               /*Can't load object into cache               */
    H5E_PROTECT,                /*protected object error                     */
    H5E_NOTCACHED,              /*object not currently cached                */

    /* B-tree related errors */
    H5E_NOTFOUND,               /*object not found                           */
    H5E_EXISTS,                 /*object already exists                      */
    H5E_CANTENCODE,             /*Can't encode value                         */
    H5E_CANTDECODE,             /*Can't decode value                         */
    H5E_CANTSPLIT,              /*Can't split node                           */
    H5E_CANTINSERT,             /*Can't insert object                        */
    H5E_CANTLIST,               /*Can't list node                            */

    /* Object header related errors */
    H5E_LINKCOUNT,              /*bad object header link count               */
    H5E_VERSION,                /*wrong version number                       */
    H5E_ALIGNMENT,              /*alignment error                            */
    H5E_BADMESG,                /*unrecognized message                       */
    H5E_CANTDELETE,             /* Can't delete message                      */

    /* Group related errors */
    H5E_CANTOPENOBJ,            /*Can't open object                          */
    H5E_COMPLEN,                /*name component is too long                 */
    H5E_CWG,                    /*problem with current working group         */
    H5E_LINK,                   /*link count failure                         */
    H5E_SLINK,			/*symbolic link error			     */

    /* Datatype conversion errors */
    H5E_CANTCONVERT,            /*Can't convert datatypes                    */
    H5E_BADSIZE,                /*Bad size for object                        */

    /* Dataspace errors */
    H5E_CANTCLIP,               /*Can't clip hyperslab region                */
    H5E_CANTCOUNT,              /*Can't count elements                       */
    H5E_CANTSELECT,             /*Can't select hyperslab                     */
    H5E_CANTNEXT,               /*Can't move to next iterator location       */
    H5E_BADSELECT,              /*Invalid selection                          */
    H5E_CANTCOMPARE,            /*Can't compare objects                      */

    /* Property list errors */
    H5E_CANTGET,                /*Can't get value                            */
    H5E_CANTSET,                /*Can't set value                            */
    H5E_DUPCLASS,               /*Duplicate class name in parent class */

    /* Parallel errors */
    H5E_MPI,			/*some MPI function failed		     */
    H5E_MPIERRSTR,		/*MPI Error String 			     */

    /* FPHDF5 errors */
    H5E_CANTMAKETREE,           /*can't make a TBBT tree                     */
    H5E_CANTRECV,               /*can't receive messages from processes      */
    H5E_CANTSENDMDATA,          /*can't send metadata message                */
    H5E_CANTCHANGE,             /*can't register change on server            */
    H5E_CANTALLOC,              /*can't allocate from file                   */

    /* I/O pipeline errors */
    H5E_NOFILTER,               /*requested filter is not available          */
    H5E_CALLBACK,               /*callback failed                            */
    H5E_CANAPPLY,               /*error from filter "can apply" callback     */
    H5E_SETLOCAL                /*error from filter "set local" callback     */

} H5E_minor_t;

/* Information about an error */
typedef struct H5E_error_t {
    H5E_major_t maj_num;		/*major error number		     */
    H5E_minor_t min_num;		/*minor error number		     */
    const char	*func_name;   		/*function in which error occurred   */
    const char	*file_name;		/*file in which error occurred       */
    unsigned	line;			/*line in file where error occurs    */
    const char	*desc;			/*optional supplied description      */
} H5E_error_t;

/* Error stack traversal direction */
typedef enum H5E_direction_t {
    H5E_WALK_UPWARD	= 0,		/*begin deep, end at API function    */
    H5E_WALK_DOWNWARD	= 1		/*begin at API function, end deep    */
} H5E_direction_t;


#ifdef __cplusplus
extern "C" {
#endif

/* Error stack traversal callback function */
typedef herr_t (*H5E_walk_t)(int n, H5E_error_t *err_desc, void *client_data);
typedef herr_t (*H5E_auto_t)(hid_t estack, void *client_data);

H5_DLL herr_t H5Eset_auto (H5E_auto_t func, void *client_data);
H5_DLL herr_t H5Eget_auto (H5E_auto_t *func, void **client_data);
H5_DLL herr_t H5Eclear (void);
H5_DLL herr_t H5Eprint (FILE *stream);
H5_DLL herr_t H5Ewalk (H5E_direction_t direction, H5E_walk_t func,
			void *client_data);
H5_DLL const char *H5Eget_major (H5E_major_t major_number);
H5_DLL const char *H5Eget_minor (H5E_minor_t minor_number);
H5_DLL herr_t H5Epush(const char *file, const char *func,
            unsigned line, H5E_major_t maj, H5E_minor_t min, const char *str);

/* New error API */
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
H5_DLL herr_t  H5Epush_new(hid_t err_stack, const char *file, const char *func, unsigned line, 
                           hid_t maj_id, hid_t min_id, const char *msg, ...);
H5_DLL herr_t  H5Epop(hid_t err_stack, size_t count);
H5_DLL herr_t  H5Eclear_new(hid_t err_stack);
H5_DLL herr_t  H5Eprint_new(hid_t err_stack, FILE *stream);
typedef herr_t (*H5E_walk_t_new)(int n, H5E_error_t_new *err_desc, void *client_data);
H5_DLL herr_t  H5Ewalk_new(hid_t err_stack, H5E_direction_t direction, H5E_walk_t_new func, 
                            void *client_data);
H5_DLL herr_t  H5Eget_auto_new(hid_t estack_id, H5E_auto_t *func, void **client_data);
H5_DLL herr_t  H5Eset_auto_new(hid_t estack_id, H5E_auto_t func, void *client_data);

#ifdef __cplusplus
}
#endif
#endif
