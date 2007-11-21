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
 * Programmer:	Quincey Koziol <koziol@ncsa.uiuc.edu>
 *		Thursday, September 28, 2000
 *
 * Purpose:	This file contains declarations which are visible only within
 *		the H5F package.  Source files outside the H5F package should
 *		include H5Fprivate.h instead.
 */
#ifndef H5F_PACKAGE
#error "Do not include this file outside the H5F package!"
#endif

#ifndef _H5Fpkg_H
#define _H5Fpkg_H

/* Get package's private header */
#include "H5Fprivate.h"

/* Other public headers needed by this file */
#include "H5Bpublic.h"          /* B-tree header, for H5B_NUM_BTREE_ID */

/* Other private headers needed by this file */
#include "H5private.h"		/* Generic Functions			*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5FOprivate.h"        /* File objects                         */
#include "H5Gprivate.h"		/* Groups 			  	*/
#include "H5Oprivate.h"         /* Object header messages               */
#include "H5RCprivate.h"	/* Reference counted object functions	*/
#include "H5AC2private.h"	/* cache                                */

/*
 * Feature: Define this constant on the compiler command-line if you want to
 *	    see some debugging messages on the debug stream.
 */
#ifdef NDEBUG
#  undef H5F_DEBUG
#endif

/* Define the HDF5 file signature */
#define H5F_SIGNATURE	  "\211HDF\r\n\032\n"
#define H5F_SIGNATURE_LEN 8

/* Superblock status flags */
#define H5F_SUPER_WRITE_ACCESS          0x01
#define H5F_SUPER_FILE_OK               0x02
#define H5F_SUPER_ALL_FLAGS             (H5F_SUPER_WRITE_ACCESS | H5F_SUPER_FILE_OK)

/* Mask for removing private file access flags */
#define H5F_ACC_PUBLIC_FLAGS 	0x00ffu

/*
 * Define the structure to store the file information for HDF5 files. One of
 * these structures is allocated per file, not per H5Fopen(). That is, set of
 * H5F_t structs can all point to the same H5F_file_t struct. The `nrefs'
 * count in this struct indicates the number of H5F_t structs which are
 * pointing to this struct.
 */
typedef struct H5F_file_t {
    H5FD_t	*lf; 		/* Lower level file handle for I/O	*/
    unsigned	nrefs;		/* Ref count for times file is opened	*/
    uint8_t	status_flags;	/* File status flags			*/
    unsigned	flags;		/* Access Permissions for file		*/

    /* Cached values from FCPL/superblock */
    unsigned	sym_leaf_k;	/* Size of leaves in symbol tables      */
    unsigned    btree_k[H5B_NUM_BTREE_ID];  /* B-tree key values for each type  */
    size_t	sizeof_addr;	/* Size of addresses in file            */
    size_t	sizeof_size;	/* Size of offsets in file              */
    haddr_t	super_addr;	/* Absolute address of super block	*/
    haddr_t	base_addr;	/* Absolute base address for rel.addrs. */
    haddr_t	extension_addr;	/* Relative address of superblock extension	*/
    haddr_t	sohm_addr;	/* Relative address of shared object header message table */
    unsigned	sohm_vers;	/* Version of shared message table on disk */
    unsigned	sohm_nindexes;	/* Number of shared messages indexes in the table */
    haddr_t	driver_addr;	/* File driver information block address*/
    hbool_t     fam_to_sec2;    /* Is h5repart changing driver from family to sec2 */

    H5AC_t      *cache;		/* The object cache			*/
    H5AC2_t     *cache2;	/* test cache		 		*/
    H5AC_cache_config_t
		mdc_initCacheCfg; /* initial configuration for the      */
                                /* metadata cache.  This structure is   */
                                /* fixed at creation time and should    */
                                /* not change thereafter.               */
    hid_t       fcpl_id;	/* File creation property list ID 	*/
    H5F_close_degree_t fc_degree;   /* File close behavior degree	*/
    size_t	rdcc_nelmts;	/* Size of raw data chunk cache (elmts)	*/
    size_t	rdcc_nbytes;	/* Size of raw data chunk cache	(bytes)	*/
    double	rdcc_w0;	/* Preempt read chunks first? [0.0..1.0]*/
    size_t      sieve_buf_size; /* Size of the data sieve buffer allocated (in bytes) */
    hsize_t	threshold;	/* Threshold for alignment		*/
    hsize_t	alignment;	/* Alignment				*/
    unsigned	gc_ref;		/* Garbage-collect references?		*/
    hbool_t	latest_format;	/* Always use the latest format?	*/
    hbool_t	store_msg_crt_idx;	/* Store creation index for object header messages?	*/
    int	ncwfs;			/* Num entries on cwfs list		*/
    struct H5HG_heap_t **cwfs;	/* Global heap cache			*/
    struct H5G_t *root_grp;	/* Open root group			*/
    H5FO_t *open_objs;          /* Open objects in file                 */
    H5RC_t *grp_btree_shared;   /* Ref-counted group B-tree node info   */
} H5F_file_t;

/* A record of the mount table */
typedef struct H5F_mount_t {
    struct H5G_t	*group;	/* Mount point group held open		*/
    struct H5F_t	*file;	/* File mounted at that point		*/
} H5F_mount_t;

/*
 * The mount table describes what files are attached to (mounted on) the file
 * to which this table belongs.
 */
typedef struct H5F_mtab_t {
    struct H5F_t	*parent;/* Parent file				*/
    unsigned		nmounts;/* Number of children which are mounted	*/
    unsigned		nalloc;	/* Number of mount slots allocated	*/
    H5F_mount_t		*child;	/* An array of mount records		*/
} H5F_mtab_t;

/*
 * This is the top-level file descriptor.  One of these structures is
 * allocated every time H5Fopen() is called although they may contain pointers
 * to shared H5F_file_t structs. The reference count (nrefs) indicates the
 * number of times the file has been opened (the application can only open a
 * file once explicitly, but the library can open the file a second time to
 * indicate that the file is mounted on some other file).
 */
struct H5F_t {
    unsigned		intent;		/* The flags passed to H5F_open()*/
    char		*name;		/* Name used to open file	*/
    H5F_file_t		*shared;	/* The shared file info		*/
    unsigned		nopen_objs;	/* Number of open object headers*/
    H5FO_t              *obj_count;     /* # of time each object is opened through top file structure */
    hid_t               file_id;        /* ID of this file              */
    hbool_t             closing;        /* File is in the process of being closed */
    H5F_mtab_t		mtab;		/* File mount table		*/
};

/*****************************/
/* Package Private Variables */
/*****************************/

/* Declare a free list to manage the H5F_t struct */
H5FL_EXTERN(H5F_t);

/* Declare a free list to manage the H5F_file_t struct */
H5FL_EXTERN(H5F_file_t);


/******************************/
/* Package Private Prototypes */
/******************************/

/* General routines */
H5_DLL herr_t H5F_init(void);
H5_DLL haddr_t H5F_locate_signature(H5FD_t *file, hid_t dxpl_id);

/* File mount related routines */
H5_DLL herr_t H5F_close_mounts(H5F_t *f);
H5_DLL int H5F_term_unmount_cb(void *obj_ptr, hid_t obj_id, void *key);
H5_DLL herr_t H5F_mount_count_ids(H5F_t *f, unsigned *nopen_files, unsigned *nopen_objs);

/* Superblock related routines */
H5_DLL herr_t H5F_super_init(H5F_t *f, hid_t dxpl_id);
H5_DLL herr_t H5F_super_write(H5F_t *f, hid_t dxpl_id);
H5_DLL herr_t H5F_super_read(H5F_t *f, hid_t dxpl_id, H5G_loc_t *root_loc);
H5_DLL herr_t H5F_super_ext_size(H5F_t *f, hid_t dxpl_id, hsize_t *super_ext_info);


/* Shared file list related routines */
H5_DLL herr_t H5F_sfile_add(H5F_file_t *shared);
H5_DLL H5F_file_t * H5F_sfile_search(H5FD_t *lf);
H5_DLL herr_t H5F_sfile_remove(H5F_file_t *shared);

/* Testing functions */
#ifdef H5F_TESTING
H5_DLL herr_t H5F_get_sohm_mesg_count_test(hid_t fid, unsigned type_id,
    size_t *mesg_count);
#endif /* H5F_TESTING */

#endif /* _H5Fpkg_H */

