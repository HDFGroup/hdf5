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

/* Generated automatically by bin/make_err -- do not edit */
/* Add new errors to H5err.txt file */


#ifndef _H5Eterm_H
#define _H5Eterm_H

/* Reset major error IDs */
    
H5E_DATASET_g=    
H5E_FUNC_g=    
H5E_STORAGE_g=    
H5E_FILE_g=    
H5E_FPHDF5_g=    
H5E_SYM_g=    
H5E_VFL_g=    
H5E_INTERNAL_g=    
H5E_BTREE_g=    
H5E_REFERENCE_g=    
H5E_DATASPACE_g=    
H5E_RESOURCE_g=    
H5E_PLIST_g=    
H5E_DATATYPE_g=    
H5E_RS_g=    
H5E_HEAP_g=    
H5E_OHDR_g=    
H5E_ATOM_g=    
H5E_ATTR_g=    
H5E_IO_g=    
H5E_SLIST_g=    
H5E_EFL_g=    
H5E_TST_g=    
H5E_ARGS_g=    
H5E_ERROR_g=    
H5E_PLINE_g=    
H5E_CACHE_g=
H5E_NONE_MAJOR_g=(-1);

/* Reset minor error IDs */


/* Generic low-level file I/O errors */    
H5E_SEEKERROR_g=    
H5E_READERROR_g=    
H5E_WRITEERROR_g=    
H5E_CLOSEERROR_g=    
H5E_OVERFLOW_g=    
H5E_FCNTL_g=

/* Resource errors */    
H5E_NOSPACE_g=    
H5E_CANTCOPY_g=    
H5E_CANTFREE_g=    
H5E_ALREADYEXISTS_g=    
H5E_CANTLOCK_g=    
H5E_CANTUNLOCK_g=    
H5E_CANTGC_g=    
H5E_CANTGETSIZE_g=

/* Heap errors */    
H5E_CANTRESTORE_g=    
H5E_CANTCOMPUTE_g=

/* Function entry/exit interface errors */    
H5E_CANTINIT_g=    
H5E_ALREADYINIT_g=    
H5E_CANTRELEASE_g=

/* Property list errors */    
H5E_CANTGET_g=    
H5E_CANTSET_g=    
H5E_DUPCLASS_g=

/* Object header related errors */    
H5E_LINKCOUNT_g=    
H5E_VERSION_g=    
H5E_ALIGNMENT_g=    
H5E_BADMESG_g=    
H5E_CANTDELETE_g=    
H5E_BADITER_g=    
H5E_CANTPACK_g=

/* FPHDF5 errors */    
H5E_CANTRECV_g=    
H5E_CANTSENDMDATA_g=    
H5E_CANTCHANGE_g=    
H5E_CANTALLOC_g=

/* System level errors */    
H5E_SYSERRSTR_g=

/* I/O pipeline errors */    
H5E_NOFILTER_g=    
H5E_CALLBACK_g=    
H5E_CANAPPLY_g=    
H5E_SETLOCAL_g=    
H5E_NOENCODER_g=

/* Group related errors */    
H5E_CANTOPENOBJ_g=    
H5E_CANTCLOSEOBJ_g=    
H5E_COMPLEN_g=    
H5E_LINK_g=    
H5E_SLINK_g=    
H5E_PATH_g=

/* File accessability errors */    
H5E_FILEEXISTS_g=    
H5E_FILEOPEN_g=    
H5E_CANTCREATE_g=    
H5E_CANTOPENFILE_g=    
H5E_CANTCLOSEFILE_g=    
H5E_NOTHDF5_g=    
H5E_BADFILE_g=    
H5E_TRUNCATED_g=    
H5E_MOUNT_g=

/* Object atom related errors */    
H5E_BADATOM_g=    
H5E_BADGROUP_g=    
H5E_CANTREGISTER_g=    
H5E_CANTINC_g=    
H5E_CANTDEC_g=    
H5E_NOIDS_g=

/* Cache related errors */    
H5E_CANTFLUSH_g=    
H5E_CANTSERIALIZE_g=    
H5E_CANTLOAD_g=    
H5E_PROTECT_g=    
H5E_NOTCACHED_g=    
H5E_SYSTEM_g=    
H5E_CANTINS_g=    
H5E_CANTRENAME_g=    
H5E_CANTPROTECT_g=    
H5E_CANTUNPROTECT_g=

/* Parallel MPI errors */    
H5E_MPI_g=    
H5E_MPIERRSTR_g=

/* Dataspace errors */    
H5E_CANTCLIP_g=    
H5E_CANTCOUNT_g=    
H5E_CANTSELECT_g=    
H5E_CANTNEXT_g=    
H5E_BADSELECT_g=    
H5E_CANTCOMPARE_g=

/* B-tree related errors */    
H5E_NOTFOUND_g=    
H5E_EXISTS_g=    
H5E_CANTENCODE_g=    
H5E_CANTDECODE_g=    
H5E_CANTSPLIT_g=    
H5E_CANTREDISTRIBUTE_g=    
H5E_CANTSWAP_g=    
H5E_CANTINSERT_g=    
H5E_CANTLIST_g=    
H5E_CANTMODIFY_g=

/* Argument errors */    
H5E_UNINITIALIZED_g=    
H5E_UNSUPPORTED_g=    
H5E_BADTYPE_g=    
H5E_BADRANGE_g=    
H5E_BADVALUE_g=

/* Datatype conversion errors */    
H5E_CANTCONVERT_g=    
H5E_BADSIZE_g=

/* No error, only for backward compatibility */
H5E_NONE_MINOR_g= (-1);

#endif /* H5Eterm_H */
