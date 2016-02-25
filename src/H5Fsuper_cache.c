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

/*-------------------------------------------------------------------------
 *
 * Created:		H5Fsuper_cache.c
 *			Aug 15 2009
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Implement file superblock & driver info metadata cache methods.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#include "H5Fmodule.h"          /* This source code file is part of the H5F module */
#define H5G_FRIEND		/*suppress error about including H5Gpkg	  */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"             /* File access				*/
#include "H5FDprivate.h"	/* File drivers				*/
#include "H5FLprivate.h"        /* Free Lists                           */
#include "H5Gpkg.h"		/* Groups		  		*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"        /* Memory management                    */
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5SMprivate.h"        /* Shared Object Header Messages        */


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

/* Metadata cache (H5AC) callbacks */
static herr_t H5F__cache_superblock_get_load_size(const void *image_ptr, void *udata, 
    size_t *image_len, size_t *actual_len,
    hbool_t *compressed_ptr, size_t *compressed_image_len_ptr);
static htri_t H5F__cache_superblock_verify_chksum(const void *image_ptr, size_t len, void *udata_ptr);
static void *H5F__cache_superblock_deserialize(const void *image, size_t len,
    void *udata, hbool_t *dirty);
static herr_t H5F__cache_superblock_image_len(const void *thing, 
    size_t *image_len, hbool_t *compressed_ptr, 
    size_t *compressed_image_len_ptr);
static herr_t H5F__cache_superblock_pre_serialize(const H5F_t *f, 
    hid_t dxpl_id, void *thing, haddr_t addr, size_t len, 
    size_t compressed_len, haddr_t *new_addr, size_t *new_len, 
    size_t *new_compressed_len, unsigned *flags);
static herr_t H5F__cache_superblock_serialize(const H5F_t *f, void *image, size_t len,
    void *thing);
static herr_t H5F__cache_superblock_free_icr(void *thing);

static herr_t H5F__cache_drvrinfo_get_load_size(const void *image_ptr, void *udata, 
    size_t *image_len, size_t *actual_len,
    hbool_t *compressed_ptr, size_t *compressed_image_len_ptr);
static void *H5F__cache_drvrinfo_deserialize(const void *image, size_t len,
    void *udata, hbool_t *dirty);
static herr_t H5F__cache_drvrinfo_image_len(const void *thing, 
    size_t *image_len, hbool_t *compressed_ptr, 
    size_t *compressed_image_len_ptr);
static herr_t H5F__cache_drvrinfo_serialize(const H5F_t *f, void *image, size_t len,
    void *thing);
static herr_t H5F__cache_drvrinfo_free_icr(void *thing);


/*********************/
/* Package Variables */
/*********************/

/* H5F superblock inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_SUPERBLOCK[1] = {{
    H5AC_SUPERBLOCK_ID,                 /* Metadata client ID */
    "Superblock",                       /* Metadata client name (for debugging) */
    H5FD_MEM_SUPER,                     /* File space memory type for client */
    H5AC__CLASS_SPECULATIVE_LOAD_FLAG,  /* Client class behavior flags */
    H5F__cache_superblock_get_load_size,/* 'get_load_size' callback */
    H5F__cache_superblock_verify_chksum, /* 'verify_chksum' callback */
    H5F__cache_superblock_deserialize,  /* 'deserialize' callback */
    H5F__cache_superblock_image_len,    /* 'image_len' callback */
    H5F__cache_superblock_pre_serialize,/* 'pre_serialize' callback */
    H5F__cache_superblock_serialize,    /* 'serialize' callback */
    NULL,       			/* 'notify' callback */
    H5F__cache_superblock_free_icr,     /* 'free_icr' callback */
    NULL,				/* 'clear' callback */
    NULL,                               /* 'fsf_size' callback */
}};

/* H5F driver info block inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_DRVRINFO[1] = {{
    H5AC_DRVRINFO_ID,                   /* Metadata client ID */
    "Driver info block",                /* Metadata client name (for debugging) */
    H5FD_MEM_SUPER,                     /* File space memory type for client */
    H5AC__CLASS_SPECULATIVE_LOAD_FLAG,  /* Client class behavior flags */
    H5F__cache_drvrinfo_get_load_size,  /* 'get_load_size' callback */
    NULL,				/* 'verify_chksum' callback */
    H5F__cache_drvrinfo_deserialize,    /* 'deserialize' callback */
    H5F__cache_drvrinfo_image_len,      /* 'image_len' callback */
    NULL,                               /* 'pre_serialize' callback */
    H5F__cache_drvrinfo_serialize,      /* 'serialize' callback */
    NULL,                               /* 'notify' callback */
    H5F__cache_drvrinfo_free_icr,       /* 'free_icr' callback */
    NULL,				/* 'clear' callback */
    NULL,                               /* 'fsf_size' callback */
}};


/*****************************/
/* Library Private Variables */
/*****************************/

/* Declare extern the free list to manage the H5F_super_t struct */
H5FL_EXTERN(H5F_super_t);


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:    H5F__cache_superblock_get_load_size
 *
 * Purpose:     Compute the size of the data structure on disk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              koziol@hdfgroup.org
 *              July 17, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F__cache_superblock_get_load_size(const void *_image, void *_udata, size_t *image_len, size_t *actual_len,
    hbool_t H5_ATTR_UNUSED *compressed_ptr, size_t H5_ATTR_UNUSED *compressed_image_len_ptr)
{
    const uint8_t *image = (const uint8_t *)_image;   			    /* Pointer into raw data buffer */
    H5F_superblock_cache_ud_t *udata = (H5F_superblock_cache_ud_t *)_udata; /* User data */
    unsigned super_vers;                /* Superblock version */
    uint8_t sizeof_addr;                /* Size of offsets in the file (in bytes) */
    uint8_t sizeof_size;                /* Size of lengths in the file (in bytes) */
    size_t variable_size;               /* Variable size of superblock */
    htri_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check arguments */
    HDassert(image_len);

    if(image == NULL) {
	HDassert(actual_len == NULL);

	/* Set the initial image length size */
	*image_len = H5F_SUPERBLOCK_FIXED_SIZE +    /* Fixed size of superblock */
	    	     H5F_SUPERBLOCK_MINIMAL_VARLEN_SIZE;
    } else { /* compute actual_len */
	HDassert(udata);
	HDassert(udata->f);
	HDassert(actual_len);
	HDassert(*actual_len == *image_len);

	image += H5F_SIGNATURE_LEN;

	/* Superblock version */
	super_vers = *image++;
	if(super_vers > HDF5_SUPERBLOCK_VERSION_LATEST)
	    HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "bad superblock version number")

	/* Save the version to be used in verify_chksum callback */
	udata->super_vers = super_vers;

	/* Sanity check */
	HDassert(((size_t)(image - (const uint8_t *)_image)) == H5F_SUPERBLOCK_FIXED_SIZE);
	HDassert(*image_len >= H5F_SUPERBLOCK_FIXED_SIZE + 6);

	/* Determine the size of addresses & size of offsets, for computing the
	 * variable-sized portion of the superblock.
	 */
	if(super_vers < HDF5_SUPERBLOCK_VERSION_2) {
	    sizeof_addr = image[4];
	    sizeof_size = image[5];
	} /* end if */
	else {
	    sizeof_addr = image[0];
	    sizeof_size = image[1];
	} /* end else */
	if(sizeof_addr != 2 && sizeof_addr != 4 &&
           sizeof_addr != 8 && sizeof_addr != 16 && sizeof_addr != 32)
	    HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "bad byte number in an address")
	if(sizeof_size != 2 && sizeof_size != 4 &&
           sizeof_size != 8 && sizeof_size != 16 && sizeof_size != 32)
	    HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "bad byte number for object size")

	/* Determine the size of the variable-length part of the superblock */
	variable_size = (size_t)H5F_SUPERBLOCK_VARLEN_SIZE(super_vers, sizeof_addr, sizeof_size);
	HDassert(variable_size > 0);

	/* Handle metadata cache retry for variable-sized portion of the superblock */
	if(*image_len != (H5F_SUPERBLOCK_FIXED_SIZE + variable_size)) {

	    /* Sanity check */
	    HDassert(*image_len == (H5F_SUPERBLOCK_FIXED_SIZE + H5F_SUPERBLOCK_MINIMAL_VARLEN_SIZE));

	    /* Make certain we can read the variabled-sized portion of the superblock */
	    if(H5F__set_eoa(udata->f, H5FD_MEM_SUPER, (haddr_t)(H5F_SUPERBLOCK_FIXED_SIZE + variable_size)) < 0)
		HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "set end of space allocation request failed")

	    *actual_len = H5F_SUPERBLOCK_FIXED_SIZE + variable_size;

	} /* end if */
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__cache_superblock_get_load_size() */


/*-------------------------------------------------------------------------
 * Function:    H5F__cache_superblock_verify_chksum
 *
 * Purpose:	Verify the computed checksum of the data structure is the 
 *		same as the stored chksum.
 *
 * Return:      Success:        TRUE/FALSE
 *              Failure:        Negative
 *
 * Programmer:  Vailin Choi; Aug 2015
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5F__cache_superblock_verify_chksum(const void *_image, size_t len, void *_udata)
{
    const uint8_t *image = (const uint8_t *)_image;    	/* Pointer into raw data buffer */
    H5F_superblock_cache_ud_t *udata = (H5F_superblock_cache_ud_t *)_udata; /* User data */
    uint32_t stored_chksum;     /* Stored metadata checksum value */
    uint32_t computed_chksum;   /* Computed metadata checksum value */
    htri_t ret_value = TRUE;	/* Return value */

    FUNC_ENTER_STATIC_NOERR

    /* Check arguments */
    HDassert(image);
    HDassert(udata);

    /* No checksum for version 0 & 1 */
    if(udata->super_vers >= HDF5_SUPERBLOCK_VERSION_2) {

	/* Get stored and computed checksums */
	H5F_get_checksums(image, len, &stored_chksum, &computed_chksum);

	if(stored_chksum != computed_chksum)
	    ret_value = FALSE;
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__cache_superblock_verify_chksum() */


/*-------------------------------------------------------------------------
 * Function:	H5F__cache_superblock_deserialize
 *
 * Purpose:	Loads an object from the disk.
 *
 * Return:	Success:	Pointer to new object
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		July 18 2013
 *
 *-------------------------------------------------------------------------
 */
static void *
H5F__cache_superblock_deserialize(const void *_image, size_t len, void *_udata,
    hbool_t H5_ATTR_UNUSED *dirty)
{
    H5F_super_t         *sblock = NULL; /* File's superblock */
    H5F_superblock_cache_ud_t *udata = (H5F_superblock_cache_ud_t *)_udata; /* User data */
    const uint8_t	*image = (const uint8_t *)_image;       /* Pointer into raw data buffer */
    size_t              variable_size;  /* Variable size of superblock */
    unsigned            super_vers;     /* Superblock version */
    uint8_t             sizeof_addr;    /* Size of offsets in the file (in bytes) */
    uint8_t             sizeof_size;    /* Size of lengths in the file (in bytes) */
    H5F_super_t         *ret_value = NULL;      /* Return value */

    FUNC_ENTER_STATIC

    /* Check arguments */
    HDassert(image);
    HDassert(udata);
    HDassert(udata->f);

    /* Allocate space for the superblock */
    if(NULL == (sblock = H5FL_CALLOC(H5F_super_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    image += H5F_SIGNATURE_LEN;

    /* Superblock version */
    super_vers = *image++;
    if(super_vers > HDF5_SUPERBLOCK_VERSION_LATEST)
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, NULL, "bad superblock version number")

    /* Record the superblock version */
    sblock->super_vers = super_vers;

    /* Sanity check */
    HDassert(((size_t)(image - (const uint8_t *)_image)) == H5F_SUPERBLOCK_FIXED_SIZE);
    HDassert(len >= H5F_SUPERBLOCK_FIXED_SIZE + 6);

    /* Determine the size of addresses & size of offsets, for computing the
     * variable-sized portion of the superblock.
     */
    if(super_vers < HDF5_SUPERBLOCK_VERSION_2) {
        sizeof_addr = image[4];
        sizeof_size = image[5];
    } /* end if */
    else {
        sizeof_addr = image[0];
        sizeof_size = image[1];
    } /* end else */
    if(sizeof_addr != 2 && sizeof_addr != 4 &&
            sizeof_addr != 8 && sizeof_addr != 16 && sizeof_addr != 32)
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, NULL, "bad byte number in an address")
    if(sizeof_size != 2 && sizeof_size != 4 &&
            sizeof_size != 8 && sizeof_size != 16 && sizeof_size != 32)
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, NULL, "bad byte number for object size")
    sblock->sizeof_addr = sizeof_addr;
    sblock->sizeof_size = sizeof_size;

    /* Determine the size of the variable-length part of the superblock */
    variable_size = (size_t)H5F_SUPERBLOCK_VARLEN_SIZE(super_vers, sizeof_addr, sizeof_size);
    HDassert(variable_size > 0);

    HDassert(len == (H5F_SUPERBLOCK_FIXED_SIZE + variable_size));

    /* Check for older version of superblock format */
    if(super_vers < HDF5_SUPERBLOCK_VERSION_2) {
	uint32_t    status_flags;	    /* File status flags	   */
	unsigned    sym_leaf_k;         /* Symbol table leaf node's 'K' value */
	unsigned    snode_btree_k;      /* B-tree symbol table internal node 'K' value */
	unsigned    chunk_btree_k;      /* B-tree chunk internal node 'K' value */

	/* Freespace version (hard-wired) */
	if(HDF5_FREESPACE_VERSION != *image++)
	    HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, NULL, "bad free space version number")

	/* Root group version number (hard-wired) */
	if(HDF5_OBJECTDIR_VERSION != *image++)
	    HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, NULL, "bad object directory version number")

	/* Skip over reserved byte */
	image++;

	/* Shared header version number (hard-wired) */
	if(HDF5_SHAREDHEADER_VERSION != *image++)
	    HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, NULL, "bad shared-header format version number")

	/* Size of file addresses */
	sizeof_addr = *image++;
	if(sizeof_addr != 2 && sizeof_addr != 4 &&
		sizeof_addr != 8 && sizeof_addr != 16 && sizeof_addr != 32)
	    HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, NULL, "bad byte number in an address")
	sblock->sizeof_addr = sizeof_addr;
	udata->f->shared->sizeof_addr = sizeof_addr;  /* Keep a local copy also */

	/* Size of file sizes */
	sizeof_size = *image++;
	if(sizeof_size != 2 && sizeof_size != 4 &&
		sizeof_size != 8 && sizeof_size != 16 && sizeof_size != 32)
	    HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, NULL, "bad byte number for object size")
	sblock->sizeof_size = sizeof_size;
	udata->f->shared->sizeof_size = sizeof_size;  /* Keep a local copy also */

	/* Skip over reserved byte */
	image++;

	/* Various B-tree sizes */
	UINT16DECODE(image, sym_leaf_k);
	if(sym_leaf_k == 0)
	    HGOTO_ERROR(H5E_FILE, H5E_BADRANGE, NULL, "bad symbol table leaf node 1/2 rank")
        udata->sym_leaf_k = sym_leaf_k;    /* Keep a local copy also */

        /* Need 'get' call to set other array values */
        UINT16DECODE(image, snode_btree_k);
        if(snode_btree_k == 0)
	    HGOTO_ERROR(H5E_FILE, H5E_BADRANGE, NULL, "bad 1/2 rank for btree internal nodes")
	udata->btree_k[H5B_SNODE_ID] = snode_btree_k;

	/*
         * Delay setting the value in the property list until we've checked
         * for the indexed storage B-tree internal 'K' value later.
         */

        /* File status flags (not really used yet) */
        UINT32DECODE(image, status_flags);
        HDassert(status_flags <= 255);
        sblock->status_flags = (uint8_t)status_flags;
        if(sblock->status_flags & ~H5F_SUPER_ALL_FLAGS)
	    HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, NULL, "bad flag value for superblock")

	/*
         * If the superblock version # is greater than 0, read in the indexed
         * storage B-tree internal 'K' value
         */
        if(super_vers > HDF5_SUPERBLOCK_VERSION_DEF) {
	    UINT16DECODE(image, chunk_btree_k);

	    /* Reserved bytes are present only in version 1 */
	    if(super_vers == HDF5_SUPERBLOCK_VERSION_1)
		image += 2;   /* reserved */
	} /* end if */
	else
	    chunk_btree_k = HDF5_BTREE_CHUNK_IK_DEF;
	udata->btree_k[H5B_CHUNK_ID] = chunk_btree_k;

	/* Remainder of "variable-sized" portion of superblock */
	H5F_addr_decode(udata->f, (const uint8_t **)&image, &sblock->base_addr/*out*/);
	H5F_addr_decode(udata->f, (const uint8_t **)&image, &sblock->ext_addr/*out*/);
	H5F_addr_decode(udata->f, (const uint8_t **)&image, &udata->stored_eof/*out*/);
	H5F_addr_decode(udata->f, (const uint8_t **)&image, &sblock->driver_addr/*out*/);

	/* Allocate space for the root group symbol table entry */
	HDassert(!sblock->root_ent);
	if(NULL == (sblock->root_ent = (H5G_entry_t *)H5MM_calloc(sizeof(H5G_entry_t))))
	    HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, NULL, "can't allocate space for root group symbol table entry")

	/* decode the root group symbol table entry */
	if(H5G_ent_decode(udata->f, (const uint8_t **)&image, sblock->root_ent) < 0)
	    HGOTO_ERROR(H5E_FILE, H5E_CANTDECODE, NULL, "can't decode root group symbol table entry")

	/* Set the root group address to the correct value */
	sblock->root_addr = sblock->root_ent->header;

	/* This step is for h5repart tool only. If user wants to change file driver
         *  from family to sec2 while using h5repart, set the driver address to
         *  undefined to let the library ignore the family driver information saved
         *  in the superblock.
         */
	if(udata->ignore_drvrinfo && H5F_addr_defined(sblock->driver_addr)) {
	    /* Eliminate the driver info */
	    sblock->driver_addr = HADDR_UNDEF;
            udata->drvrinfo_removed = TRUE;
	} /* end if */

	/* NOTE: Driver info block is decoded separately, later */

    } /* end if */
    else {
	uint32_t read_chksum;           /* Checksum read from file  */

       /* Size of file addresses */
       sizeof_addr = *image++;
       if(sizeof_addr != 2 && sizeof_addr != 4 &&
		sizeof_addr != 8 && sizeof_addr != 16 && sizeof_addr != 32)
	    HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, NULL, "bad byte number in an address")
	sblock->sizeof_addr = sizeof_addr;
	udata->f->shared->sizeof_addr = sizeof_addr;  /* Keep a local copy also */

	/* Size of file sizes */
	sizeof_size = *image++;
	if(sizeof_size != 2 && sizeof_size != 4 &&
		sizeof_size != 8 && sizeof_size != 16 && sizeof_size != 32)
	    HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, NULL, "bad byte number for object size")
	sblock->sizeof_size = sizeof_size;
	udata->f->shared->sizeof_size = sizeof_size;  /* Keep a local copy also */

	/* File status flags (not really used yet) */
	sblock->status_flags = *image++;
	if(sblock->status_flags & ~H5F_SUPER_ALL_FLAGS)
	    HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, NULL, "bad flag value for superblock")

	/* Base, superblock extension, end of file & root group object header addresses */
	H5F_addr_decode(udata->f, (const uint8_t **)&image, &sblock->base_addr/*out*/);
	H5F_addr_decode(udata->f, (const uint8_t **)&image, &sblock->ext_addr/*out*/);
	H5F_addr_decode(udata->f, (const uint8_t **)&image, &udata->stored_eof/*out*/);
	H5F_addr_decode(udata->f, (const uint8_t **)&image, &sblock->root_addr/*out*/);

	/* checksum verification already done in verify_chksum cb */		

	/* Decode checksum */
	UINT32DECODE(image, read_chksum);

	/* The Driver Information Block may not appear with the version
	 * 2 super block.  Thus we set the driver_addr field of the in 
         * core representation of the super block HADDR_UNDEF to prevent 
         * any attempt to load the Driver Information Block.
	 */
	sblock->driver_addr = HADDR_UNDEF;
    } /* end else */

    /* Sanity check */
    HDassert((size_t)(image - (const uint8_t *)_image) <= len);

    /* Set return value */
    ret_value = sblock;

done:
    /* Release the [possibly partially initialized] superblock on error */
    if(!ret_value && sblock)
        if(H5F__super_free(sblock) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTFREE, NULL, "unable to destroy superblock data")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__cache_superblock_deserialize() */


/*-------------------------------------------------------------------------
 * Function:    H5F__cache_superblock_image_len
 *
 * Purpose:     Compute the size of the data structure on disk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              koziol@hdfgroup.org
 *              July 19, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F__cache_superblock_image_len(const void *_thing, size_t *image_len, hbool_t *compressed_ptr, size_t H5_ATTR_UNUSED *compressed_image_len_ptr)
{
    const H5F_super_t *sblock = (const H5F_super_t *)_thing;    /* Pointer to the object */

    FUNC_ENTER_STATIC_NOERR

    /* Check arguments */
    HDassert(sblock);
    HDassert(sblock->cache_info.magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
    HDassert(sblock->cache_info.type == H5AC_SUPERBLOCK);
    HDassert(image_len);
    HDassert(compressed_ptr);

    /* Set the image length size */
    *image_len = (size_t)H5F_SUPERBLOCK_SIZE(sblock);

    /* Set *compressed_ptr to FALSE unconditionally */
    *compressed_ptr = FALSE;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5F__cache_superblock_image_len() */


/*-------------------------------------------------------------------------
 * Function:    H5FS__cache_hdf_pre_serialize
 *
 * Purpose:	The current use of this function is a cludge to repair an 
 *		oversight in the conversion of the superblock code to use the 
 *		version 3 cache.  
 *
 *		In the V2 metadata cache callbacks, the superblock dirver info 
 *     		message was updated in the flush routine.  Note that this 
 *		operation only applies to version 2 or later superblocks.
 *
 *     		Somehow, this functionality was lost in the conversion to use 
 *     		the V3 cache, causing failures with the multi file driver 
 *     		(and possibly the family file driver as well).
 *
 *     		Performing this operation is impossible in the current 
 *		serialize routine, as the dxpl_id is not available.  While 
 *		I am pretty sure that this is not the correct place for this 
 *		functionality, as I can see it causing problems with both 
 *		journaling and possibly parallel HDF5 as well, I am placing 
 *		code for the necessary update in the pre_serialize call for 
 *		now for testing purposes.  We will almost certainly want to 
 *		change this.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              10/82/14
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F__cache_superblock_pre_serialize(const H5F_t *f, hid_t dxpl_id, 
    void *_thing, haddr_t H5_ATTR_UNUSED addr, size_t H5_ATTR_UNUSED len, 
    size_t H5_ATTR_UNUSED compressed_len, haddr_t H5_ATTR_UNUSED *new_addr, 
    size_t H5_ATTR_UNUSED *new_len, size_t H5_ATTR_UNUSED *new_compressed_len, 
    unsigned H5_ATTR_UNUSED *flags)
{
    H5P_genplist_t *dxpl = NULL;        /* DXPL for setting ring */
    H5AC_ring_t orig_ring = H5AC_RING_INV;      /* Original ring value */
    H5F_super_t *sblock = (H5F_super_t *)_thing; /* Pointer to the super block */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Sanity check */
    HDassert(f);
    HDassert(sblock);
    HDassert(sblock->cache_info.magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
    HDassert(sblock->cache_info.type == H5AC_SUPERBLOCK);
    HDassert(flags);

    if(sblock->super_vers >= HDF5_SUPERBLOCK_VERSION_2) {
        /* WARNING: This code almost certainly doesn't belong here.  Must
         *          discuss with Quincey where to put it.  Note issues
         *          for journaling and possibly parallel.
         *
         *                                            -- JRM
         */
        /* Update the driver information message in the superblock extension
         * if appropriate.
         */
        if(H5F_addr_defined(sblock->ext_addr)) {
            size_t     driver_size;    /* Size of driver info block (bytes)*/
            H5O_loc_t  ext_loc;        /* "Object location" for superblock extension */

            HDassert(sblock->super_vers >= HDF5_SUPERBLOCK_VERSION_2);

            /* Open the superblock extension's object header */
            if(H5F_super_ext_open((H5F_t *)f, sblock->ext_addr, &ext_loc) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTOPENOBJ, FAIL, "unable to open file's superblock extension")

            /* Check for ignoring the driver info for this file */
            if(!H5F_HAS_FEATURE(f, H5FD_FEAT_IGNORE_DRVRINFO)) {
                /* Check for driver info message */
                H5_CHECKED_ASSIGN(driver_size, size_t, H5FD_sb_size(f->shared->lf), hsize_t);
                if(driver_size > 0) {
                    H5O_drvinfo_t drvinfo;      /* Driver info */
                    uint8_t dbuf[H5F_MAX_DRVINFOBLOCK_SIZE];  /* Driver info block encoding buffer */

                    /* Sanity check */
                    HDassert(driver_size <= H5F_MAX_DRVINFOBLOCK_SIZE);

                    /* Encode driver-specific data */
                    if(H5FD_sb_encode(f->shared->lf, drvinfo.name, dbuf) < 0)
                        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "unable to encode driver information")

                    /* Set the ring type in the DXPL */
                    if(H5AC_set_ring(dxpl_id, H5AC_RING_SBE, &dxpl, &orig_ring) < 0)
                        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "unable to set ring value")

                    /* Write driver info information to the superblock extension */
                    drvinfo.len = driver_size;
                    drvinfo.buf = dbuf;
                    if(H5O_msg_write(&ext_loc, H5O_DRVINFO_ID, H5O_MSG_FLAG_DONTSHARE, H5O_UPDATE_TIME, &drvinfo, dxpl_id) < 0)
                        HGOTO_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL, "unable to update driver info header message")
                } /* end if */
            } /* end if */

            /* Close the superblock extension object header */
            if(H5F_super_ext_close((H5F_t *)f, &ext_loc, dxpl_id, FALSE) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTCLOSEOBJ, FAIL, "unable to close file's superblock extension")
        } /* end if */
    } /* end if */

done:
    /* Reset the ring in the DXPL */
    if(H5AC_reset_ring(dxpl, orig_ring) < 0)
        HDONE_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "unable to set property value")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FS_cache_superblock_pre_serialize() */


/*-------------------------------------------------------------------------
 * Function:	H5F__cache_superblock_serialize
 *
 * Purpose:	Flushes a dirty object to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		July 19 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F__cache_superblock_serialize(const H5F_t *f, void *_image, size_t H5_ATTR_UNUSED len,
    void *_thing)
{
    H5F_super_t *sblock = (H5F_super_t *)_thing;      /* Pointer to the object */
    uint8_t *image = (uint8_t *)_image;         /* Pointer into raw data buffer */
    haddr_t rel_eof;            /* Relative EOF for file */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity check */
    HDassert(f);
    HDassert(image);
    HDassert(sblock);

    /* Assert that the superblock is marked as being flushed last (and 
       collectively in parallel) */
    /* (We'll rely on the cache to make sure it actually *is* flushed
       last (and collectively in parallel), but this check doesn't hurt) */
    HDassert(sblock->cache_info.flush_me_last);    

    /* Encode the common portion of the file superblock for all versions */
    HDmemcpy(image, H5F_SIGNATURE, (size_t)H5F_SIGNATURE_LEN);
    image += H5F_SIGNATURE_LEN;
    *image++ = (uint8_t)sblock->super_vers;

    /* Check for older version of superblock format */
    if(sblock->super_vers < HDF5_SUPERBLOCK_VERSION_2) {
        *image++ = (uint8_t)HDF5_FREESPACE_VERSION;     /* (hard-wired) */
        *image++ = (uint8_t)HDF5_OBJECTDIR_VERSION;     /* (hard-wired) */
        *image++ = 0;   /* reserved*/

        *image++ = (uint8_t)HDF5_SHAREDHEADER_VERSION;  /* (hard-wired) */
        *image++ = sblock->sizeof_addr;
        *image++ = sblock->sizeof_size;
        *image++ = 0;   /* reserved */

        UINT16ENCODE(image, sblock->sym_leaf_k);
        UINT16ENCODE(image, sblock->btree_k[H5B_SNODE_ID]);
        UINT32ENCODE(image, (uint32_t)sblock->status_flags);

        /*
         * Versions of the superblock >0 have the indexed storage B-tree
         * internal 'K' value stored
         */
        if(sblock->super_vers > HDF5_SUPERBLOCK_VERSION_DEF) {
            UINT16ENCODE(image, sblock->btree_k[H5B_CHUNK_ID]);
            *image++ = 0;   /*reserved */
            *image++ = 0;   /*reserved */
        } /* end if */

        /* Encode the base address */
        H5F_addr_encode(f, &image, sblock->base_addr);

        /* Encode the address of global free-space index */
        H5F_addr_encode(f, &image, sblock->ext_addr);

        /* Encode the end-of-file address. Note that at this point in time,
         * the EOF value itself may not be reflective of the file's size, as
         * we will eventually truncate the file to match the EOA value. As
         * such, use the EOA value in its place, knowing that the current EOF
         * value will ultimately match it. */
        if ((rel_eof = H5FD_get_eoa(f->shared->lf, H5FD_MEM_SUPER)) == HADDR_UNDEF)
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTGET, FAIL, "driver get_eoa request failed")
        H5F_addr_encode(f, &image, (rel_eof + sblock->base_addr));

        /* Encode the driver informaton block address */
        H5F_addr_encode(f, &image, sblock->driver_addr);

        /* Encode the root group object entry, including the cached stab info */
        if(H5G_ent_encode(f, &image, sblock->root_ent) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTENCODE, FAIL, "can't encode root group symbol table entry")

        /* NOTE: Driver info block is handled separately */

    } /* end if */
    else { /* sblock->super_vers >= HDF5_SUPERBLOCK_VERSION_2 */
        uint32_t        chksum;                 /* Checksum temporary variable      */
        H5O_loc_t       *root_oloc;             /* Pointer to root group's object location */

        /* Size of file addresses & offsets, and status flags */
        *image++ = sblock->sizeof_addr;
        *image++ = sblock->sizeof_size;
        *image++ = sblock->status_flags;

        /* Encode the base address */
        H5F_addr_encode(f, &image, sblock->base_addr);

        /* Encode the address of the superblock extension */
        H5F_addr_encode(f, &image, sblock->ext_addr);

        /* At this point in time, the EOF value itself may
         * not be reflective of the file's size, since we'll eventually
         * truncate it to match the EOA value. As such, use the EOA value
         * in its place, knowing that the current EOF value will
         * ultimately match it. */
        if ((rel_eof = H5FD_get_eoa(f->shared->lf, H5FD_MEM_SUPER)) == HADDR_UNDEF)
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTGET, FAIL, "driver get_eoa request failed")
        H5F_addr_encode(f, &image, (rel_eof + sblock->base_addr));

        /* Retrieve information for root group */
        if(NULL == (root_oloc = H5G_oloc(f->shared->root_grp)))
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "unable to retrieve root group information")

        /* Encode address of root group's object header */
        H5F_addr_encode(f, &image, root_oloc->addr);

        /* Compute superblock checksum */
        chksum = H5_checksum_metadata(_image, ((size_t)H5F_SUPERBLOCK_SIZE(sblock) - H5F_SIZEOF_CHKSUM), 0);

        /* Superblock checksum */
        UINT32ENCODE(image, chksum);

        /* Sanity check */
        HDassert((size_t)(image - (uint8_t *)_image) == (size_t)H5F_SUPERBLOCK_SIZE(sblock));
    } /* end else */

    /* Sanity check */
    HDassert((size_t)(image - (uint8_t *)_image) == len);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5F__cache_superblock_serialize() */


/*-------------------------------------------------------------------------
 * Function:	H5F__cache_superblock_free_icr
 *
 * Purpose:	Destroy/release an "in core representation" of a data
 *              structure
 *
 * Note:	The metadata cache sets the object's cache_info.magic to
 *		H5C__H5C_CACHE_ENTRY_T_BAD_MAGIC before calling a free_icr
 *		callback (checked in assert).
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              koziol@hdfgroup.org
 *              July 20, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F__cache_superblock_free_icr(void *_thing)
{
    H5F_super_t *sblock = (H5F_super_t *)_thing;        /* Pointer to the object */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity check */
    HDassert(sblock);
    HDassert(sblock->cache_info.magic == H5C__H5C_CACHE_ENTRY_T_BAD_MAGIC);
    HDassert(sblock->cache_info.type == H5AC_SUPERBLOCK);

    /* Destroy superblock */
    if(H5F__super_free(sblock) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTFREE, FAIL, "unable to free superblock")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5F__cache_superblock_free_icr() */


/*-------------------------------------------------------------------------
 * Function:    H5F__cache_drvrinfo_get_load_size
 *
 * Purpose:     Compute the size of the data structure on disk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              koziol@hdfgroup.org
 *              July 20, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F__cache_drvrinfo_get_load_size(const void *_image, void *_udata, size_t *image_len, size_t *actual_len,
    hbool_t H5_ATTR_UNUSED *compressed_ptr, size_t H5_ATTR_UNUSED *compressed_image_len_ptr)
{
    const uint8_t *image = (const uint8_t *)_image;   			/* Pointer into raw data buffer */
    H5F_drvrinfo_cache_ud_t *udata = (H5F_drvrinfo_cache_ud_t *)_udata;	/* User data */
    unsigned drv_vers;          /* Version of driver info block */
    size_t drvinfo_len;         /* Length of encoded buffer */
    htri_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_STATIC

    /* Check arguments */
    HDassert(image_len);

    if(image == NULL) {
	HDassert(actual_len == NULL);

	/* Set the initial image length size */
	*image_len = H5F_DRVINFOBLOCK_HDR_SIZE;     /* Fixed size portion of driver info block */

    } else { /* compute actual_len */

	HDassert(udata);
	HDassert(udata->f);
	HDassert(actual_len);
	HDassert(*actual_len == *image_len);

	/* Version number */
	drv_vers = *image++;
	if(drv_vers != HDF5_DRIVERINFO_VERSION_0)
	    HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "bad driver information block version number")

	image += 3; /* reserved bytes */

	/* Driver info size */
	UINT32DECODE(image, drvinfo_len);

	/* Handle metadata cache retry for variable-sized portion of the driver info block */
	if(*image_len != (H5F_DRVINFOBLOCK_HDR_SIZE + drvinfo_len)) {
	    /* Sanity check */
	    HDassert(*image_len == H5F_DRVINFOBLOCK_HDR_SIZE);

	    /* extend the eoa if required so that we can read the complete driver info block */
	    {
		haddr_t eoa;
		haddr_t min_eoa;

		/* get current eoa... */
		if ((eoa = H5FD_get_eoa(udata->f->shared->lf, H5FD_MEM_SUPER)) == HADDR_UNDEF)
		    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTGET, FAIL, "driver get_eoa request failed")

		/* ... if it is too small, extend it. */
		min_eoa = udata->driver_addr + H5F_DRVINFOBLOCK_HDR_SIZE + drvinfo_len;

		if ( H5F_addr_gt(min_eoa, eoa) )
		    if(H5FD_set_eoa(udata->f->shared->lf, H5FD_MEM_SUPER, min_eoa) < 0)
			HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, \
			    "set end of space allocation request failed")
	    }        
	    *actual_len = H5F_DRVINFOBLOCK_HDR_SIZE + drvinfo_len;
	}
    } /* compute actual_len */

done:

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__cache_drvrinfo_get_load_size() */


/*-------------------------------------------------------------------------
 * Function:	H5F__cache_drvrinfo_deserialize
 *
 * Purpose:	Loads an object from the disk.
 *
 * Return:	Success:	Pointer to a new B-tree.
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		July 20 2013
 *
 *-------------------------------------------------------------------------
 */
static void *
H5F__cache_drvrinfo_deserialize(const void *_image, size_t len, void *_udata,
    hbool_t H5_ATTR_UNUSED *dirty)
{
    H5O_drvinfo_t       *drvinfo = NULL; /* Driver info */
    H5F_drvrinfo_cache_ud_t *udata = (H5F_drvrinfo_cache_ud_t *)_udata;     /* User data */
    const uint8_t	*image = (const uint8_t *)_image;       /* Pointer into raw data buffer */
    char                drv_name[9];    /* Name of driver */
    unsigned            drv_vers;       /* Version of driver info block */
    H5O_drvinfo_t       *ret_value = NULL;      /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity check */
    HDassert(image);
    HDassert(len >= H5F_DRVINFOBLOCK_HDR_SIZE);
    HDassert(udata);
    HDassert(udata->f);

    /* Allocate space for the driver info */
    if(NULL == (drvinfo = (H5O_drvinfo_t *)H5MM_calloc(sizeof(H5O_drvinfo_t))))
	HGOTO_ERROR(H5E_FILE, H5E_CANTALLOC, NULL, "memory allocation failed for driver info message")

    /* Version number */
    drv_vers = *image++;
    if(drv_vers != HDF5_DRIVERINFO_VERSION_0)
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, NULL, "bad driver information block version number")

    image += 3; /* reserved bytes */

    /* Driver info size */
    UINT32DECODE(image, drvinfo->len);

    /* Driver name and/or version */
    HDstrncpy(drv_name, (const char *)image, (size_t)8);
    drv_name[8] = '\0';
    image += 8; /* advance past name/version */

    HDassert(len == (H5F_DRVINFOBLOCK_HDR_SIZE + drvinfo->len));

    /* Validate and decode driver information */
    if(H5FD_sb_load(udata->f->shared->lf, drv_name, image) < 0)
	HGOTO_ERROR(H5E_FILE, H5E_CANTDECODE, NULL, "unable to decode driver information")

    /* Sanity check */
    HDassert((size_t)(image - (const uint8_t *)_image) <= len);

    /* Set return value */
    ret_value = drvinfo;

done:
    /* Release the [possibly partially initialized] driver info message on error */
    if(!ret_value && drvinfo)
        H5MM_xfree(drvinfo);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F__cache_drvrinfo_deserialize() */


/*-------------------------------------------------------------------------
 * Function:    H5F__cache_drvrinfo_image_len
 *
 * Purpose:     Compute the size of the data structure on disk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              koziol@hdfgroup.org
 *              July 20, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F__cache_drvrinfo_image_len(const void *_thing, size_t *image_len,
    hbool_t H5_ATTR_UNUSED *compressed_ptr, size_t H5_ATTR_UNUSED *compressed_image_len_ptr)
{
    const H5O_drvinfo_t *drvinfo = (const H5O_drvinfo_t *)_thing;       /* Pointer to the object */

    FUNC_ENTER_STATIC_NOERR

    /* Check arguments */
    HDassert(drvinfo);
    HDassert(drvinfo->cache_info.magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
    HDassert(drvinfo->cache_info.type == H5AC_DRVRINFO);
    HDassert(image_len);
    HDassert(compressed_ptr);

    /* Set the image length size */
    *image_len = (size_t)(H5F_DRVINFOBLOCK_HDR_SIZE +   /* Fixed-size portion of driver info block */
        drvinfo->len);                                  /* Variable-size portion of driver info block */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5F__cache_drvrinfo_image_len() */


/*-------------------------------------------------------------------------
 * Function:	H5F__cache_drvrinfo_serialize
 *
 * Purpose:	Flushes a dirty object to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		July 20 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F__cache_drvrinfo_serialize(const H5F_t *f, void *_image, size_t len,
    void *_thing)
{
    H5O_drvinfo_t *drvinfo = (H5O_drvinfo_t *)_thing;   /* Pointer to the object */
    uint8_t *image = (uint8_t *)_image;         /* Pointer into raw data buffer */
    uint8_t *dbuf;              /* Pointer to beginning of driver info */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_STATIC

    /* check arguments */
    HDassert(f);
    HDassert(image);
    HDassert(drvinfo);
    HDassert(drvinfo->cache_info.magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
    HDassert(drvinfo->cache_info.type == H5AC_DRVRINFO);
    HDassert(len == (size_t)(H5F_DRVINFOBLOCK_HDR_SIZE + drvinfo->len));

    /* Save pointer to beginning of driver info */
    dbuf = image;

    /* Encode the driver information block */
    *image++ = HDF5_DRIVERINFO_VERSION_0; /* Version */
    *image++ = 0; /* reserved */
    *image++ = 0; /* reserved */
    *image++ = 0; /* reserved */

    /* Driver info size, excluding header */
    UINT32ENCODE(image, drvinfo->len);

    /* Encode driver-specific data */
    if(H5FD_sb_encode(f->shared->lf, (char *)image, dbuf + H5F_DRVINFOBLOCK_HDR_SIZE) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "unable to encode driver information")

    /* Advance buffer pointer past name & variable-sized portion of driver info */
    image += 8 + drvinfo->len;

    /* Sanity check */
    HDassert((size_t)(image - (uint8_t *)_image) == len);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5F__cache_drvrinfo_serialize() */


/*-------------------------------------------------------------------------
 * Function:	H5F__cache_drvrinfo_free_icr
 *
 * Purpose:	Destroy/release an "in core representation" of a data
 *              structure
 *
 * Note:	The metadata cache sets the object's cache_info.magic to
 *		H5C__H5C_CACHE_ENTRY_T_BAD_MAGIC before calling a free_icr
 *		callback (checked in assert).
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              koziol@hdfgroup.org
 *              July 20, 2013
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5F__cache_drvrinfo_free_icr(void *_thing)
{
    H5O_drvinfo_t *drvinfo = (H5O_drvinfo_t *)_thing;    /* Pointer to the object */

    FUNC_ENTER_STATIC_NOERR

    /* Check arguments */
    HDassert(drvinfo);
    HDassert(drvinfo->cache_info.magic == H5C__H5C_CACHE_ENTRY_T_BAD_MAGIC);
    HDassert(drvinfo->cache_info.type == H5AC_DRVRINFO);

    /* Destroy driver info message */
    H5MM_xfree(drvinfo);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5F__cache_drvrinfo_free_icr() */

