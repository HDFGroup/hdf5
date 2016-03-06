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
 * Created:		H5Cprivate.h
 *			6/3/04
 *			John Mainzer
 *
 * Purpose:		Constants and typedefs available to the rest of the
 *			library.
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

#ifndef _H5Cprivate_H
#define _H5Cprivate_H

#include "H5Cpublic.h"		/* public prototypes		        */

/* Private headers needed by this header */
#include "H5private.h"		/* Generic Functions			*/
#include "H5Fprivate.h"		/* File access				*/

/**************************/
/* Library Private Macros */
/**************************/

/* Cache configuration settings */
#define H5C__MAX_NUM_TYPE_IDS   29
#define H5C__PREFIX_LEN         32

/* This sanity checking constant was picked out of the air.  Increase
 * or decrease it if appropriate.  Its purposes is to detect corrupt
 * object sizes, so it probably doesn't matter if it is a bit big.
 *
 *					JRM - 5/17/04
 */
#define H5C_MAX_ENTRY_SIZE		((size_t)(32 * 1024 * 1024))

#ifdef H5_HAVE_PARALLEL
/* we must maintain the clean and dirty LRU lists when we are compiled
 * with parallel support.
 */
#define H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS  1
#else /* H5_HAVE_PARALLEL */
/* The clean and dirty LRU lists don't buy us anything here -- we may
 * want them on for testing on occasion, but in general they should be
 * off.
 */
#define H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS  0
#endif /* H5_HAVE_PARALLEL */

/* Flags for cache client class behavior */
#define H5C__CLASS_NO_FLAGS_SET			((unsigned)0x0)
#define H5C__CLASS_SPECULATIVE_LOAD_FLAG	((unsigned)0x1)
#define H5C__CLASS_COMPRESSED_FLAG		((unsigned)0x2)
/* The following flags may only appear in test code */
/* The H5C__CLASS_SKIP_READS & H5C__CLASS_SKIP_WRITES flags are used in H5Oproxy.c */
#define H5C__CLASS_NO_IO_FLAG			((unsigned)0x4)
#define H5C__CLASS_SKIP_READS			((unsigned)0x8)
#define H5C__CLASS_SKIP_WRITES			((unsigned)0x10)

/* Flags for pre-serialize callback */
#define H5C__SERIALIZE_NO_FLAGS_SET	((unsigned)0)
#define H5C__SERIALIZE_RESIZED_FLAG	((unsigned)0x1)
#define H5C__SERIALIZE_MOVED_FLAG	((unsigned)0x2)
#define H5C__SERIALIZE_COMPRESSED_FLAG  ((unsigned)0x4)

/* Upper and lower limits on cache size.  These limits are picked
 * out of a hat -- you should be able to change them as necessary.
 *
 * However, if you need a very big cache, you should also increase the
 * size of the hash table (H5C__HASH_TABLE_LEN in H5Cpkg.h).  The current
 * upper bound on cache size is rather large for the current hash table
 * size.
 */
#define H5C__MAX_MAX_CACHE_SIZE		((size_t)(128 * 1024 * 1024))
#define H5C__MIN_MAX_CACHE_SIZE		((size_t)(1024))

/* Default max cache size and min clean size are give here to make
 * them generally accessable.
 */
#define H5C__DEFAULT_MAX_CACHE_SIZE     ((size_t)(4 * 1024 * 1024))
#define H5C__DEFAULT_MIN_CLEAN_SIZE     ((size_t)(2 * 1024 * 1024))

/* Values for cache entry magic field */
#define H5C__H5C_CACHE_ENTRY_T_MAGIC		0x005CAC0A
#define H5C__H5C_CACHE_ENTRY_T_BAD_MAGIC	0xDeadBeef

/* Cache configuration validation definitions */
#define H5C_RESIZE_CFG__VALIDATE_GENERAL        0x1
#define H5C_RESIZE_CFG__VALIDATE_INCREMENT      0x2
#define H5C_RESIZE_CFG__VALIDATE_DECREMENT      0x4
#define H5C_RESIZE_CFG__VALIDATE_INTERACTIONS   0x8
#define H5C_RESIZE_CFG__VALIDATE_ALL      \
(                                         \
    H5C_RESIZE_CFG__VALIDATE_GENERAL |    \
    H5C_RESIZE_CFG__VALIDATE_INCREMENT |  \
    H5C_RESIZE_CFG__VALIDATE_DECREMENT |  \
    H5C_RESIZE_CFG__VALIDATE_INTERACTIONS \
)

/* Cache configuration versions */
#define H5C__CURR_AUTO_SIZE_CTL_VER		1
#define H5C__CURR_AUTO_RESIZE_RPT_FCN_VER	1

/* Number of epoch markers active */
#define H5C__MAX_EPOCH_MARKERS  		10

/* Default configuration settings */
#define H5C__DEF_AR_UPPER_THRESHHOLD		0.9999f
#define H5C__DEF_AR_LOWER_THRESHHOLD		0.9f
#define H5C__DEF_AR_MAX_SIZE			((size_t)(16 * 1024 * 1024))
#define H5C__DEF_AR_INIT_SIZE			((size_t)( 1 * 1024 * 1024))
#define H5C__DEF_AR_MIN_SIZE			((size_t)( 1 * 1024 * 1024))
#define H5C__DEF_AR_MIN_CLEAN_FRAC		0.5f
#define H5C__DEF_AR_INCREMENT			2.0f
#define H5C__DEF_AR_MAX_INCREMENT		((size_t)( 2 * 1024 * 1024))
#define H5C__DEF_AR_FLASH_MULTIPLE              1.0f
#define H5C__DEV_AR_FLASH_THRESHOLD             0.25f
#define H5C__DEF_AR_DECREMENT			0.9f
#define H5C__DEF_AR_MAX_DECREMENT		((size_t)( 1 * 1024 * 1024))
#define H5C__DEF_AR_EPCHS_B4_EVICT		3
#define H5C__DEF_AR_EMPTY_RESERVE		0.05f
#define H5C__MIN_AR_EPOCH_LENGTH		100
#define H5C__DEF_AR_EPOCH_LENGTH		50000
#define H5C__MAX_AR_EPOCH_LENGTH		1000000

/* #defines of flags used in the flags parameters in some of the
 * following function calls.  Note that not all flags are applicable
 * to all function calls.  Flags that don't apply to a particular
 * function are ignored in that function.
 *
 * These flags apply to all function calls:
 * 	H5C__NO_FLAGS_SET (generic "no flags set" for all fcn calls)
 *
 *
 * These flags apply to H5C_insert_entry():
 * 	H5C__SET_FLUSH_MARKER_FLAG
 * 	H5C__PIN_ENTRY_FLAG
 *	H5C__FLUSH_LAST_FLAG		; super block only
 *	H5C__FLUSH_COLLECTIVELY_FLAG	; super block only
 *
 * These flags apply to H5C_protect()
 * 	H5C__READ_ONLY_FLAG
 *	H5C__FLUSH_LAST_FLAG		; super block only 
 *	H5C__FLUSH_COLLECTIVELY_FLAG	; super block only
 *
 * These flags apply to H5C_unprotect():
 * 	H5C__SET_FLUSH_MARKER_FLAG
 * 	H5C__DELETED_FLAG
 * 	H5C__DIRTIED_FLAG
 * 	H5C__PIN_ENTRY_FLAG
 * 	H5C__UNPIN_ENTRY_FLAG
 * 	H5C__FREE_FILE_SPACE_FLAG
 *      H5C__TAKE_OWNERSHIP_FLAG
 *
 * These flags apply to H5C_expunge_entry():
 * 	H5C__FREE_FILE_SPACE_FLAG
 *
 * These flags apply to H5C_flush_cache():
 * 	H5C__FLUSH_INVALIDATE_FLAG
 * 	H5C__FLUSH_CLEAR_ONLY_FLAG
 * 	H5C__FLUSH_MARKED_ENTRIES_FLAG
 *	H5C__FLUSH_IGNORE_PROTECTED_FLAG (can't use this flag in combination
 *					  with H5C__FLUSH_INVALIDATE_FLAG)
 *
 * These flags apply to H5C_flush_single_entry():
 * 	H5C__FLUSH_INVALIDATE_FLAG
 * 	H5C__FLUSH_CLEAR_ONLY_FLAG
 * 	H5C__FLUSH_MARKED_ENTRIES_FLAG
 *      H5C__TAKE_OWNERSHIP_FLAG
 *      H5C__DEL_FROM_SLIST_ON_DESTROY_FLAG
 */
#define H5C__NO_FLAGS_SET			0x0000
#define H5C__SET_FLUSH_MARKER_FLAG		0x0001
#define H5C__DELETED_FLAG			0x0002
#define H5C__DIRTIED_FLAG			0x0004
#define H5C__PIN_ENTRY_FLAG			0x0008
#define H5C__UNPIN_ENTRY_FLAG			0x0010
#define H5C__FLUSH_INVALIDATE_FLAG		0x0020
#define H5C__FLUSH_CLEAR_ONLY_FLAG		0x0040
#define H5C__FLUSH_MARKED_ENTRIES_FLAG		0x0080
#define H5C__FLUSH_IGNORE_PROTECTED_FLAG	0x0100
#define H5C__READ_ONLY_FLAG			0x0200
#define H5C__FREE_FILE_SPACE_FLAG		0x0400
#define H5C__TAKE_OWNERSHIP_FLAG		0x0800
#define H5C__FLUSH_LAST_FLAG			0x1000
#define H5C__FLUSH_COLLECTIVELY_FLAG		0x2000
#define H5C__EVICT_ALLOW_LAST_PINS_FLAG         0x4000
#define H5C__DEL_FROM_SLIST_ON_DESTROY_FLAG     0x8000

/* Definitions for cache "tag" property */
#define H5C_TAG_NAME           "H5C_tag"
#define H5C_TAG_SIZE           sizeof(H5C_tag_t)
#define H5C_TAG_DEF            {(haddr_t)0, H5C_GLOBALITY_NONE}

/* Debugging/sanity checking/statistics settings */
#ifndef NDEBUG
#define H5C_DO_SANITY_CHECKS		1
#define H5C_DO_SLIST_SANITY_CHECKS	0
#define H5C_DO_TAGGING_SANITY_CHECKS	1
#define H5C_DO_EXTREME_SANITY_CHECKS	0
#else /* NDEBUG */
/* With rare execptions, the following defines should be set 
 * to 0 if NDEBUG is defined 
 */
#define H5C_DO_SANITY_CHECKS		0
#define H5C_DO_SLIST_SANITY_CHECKS	0
#define H5C_DO_TAGGING_SANITY_CHECKS	0
#define H5C_DO_EXTREME_SANITY_CHECKS	0
#endif /* NDEBUG */

/* Cork actions: cork/uncork/get cork status of an object */
#define H5C__SET_CORK                  0x1
#define H5C__UNCORK                    0x2
#define H5C__GET_CORKED                0x4

/* Note: The memory sanity checks aren't going to work until I/O filters are
 *      changed to call a particular alloc/free routine for their buffers,
 *      because the H5AC__SERIALIZE_RESIZED_FLAG set by the fractal heap
 *      direct block serialize callback calls H5Z_pipeline().  When the I/O
 *      filters are changed, then we should implement "cache image alloc/free"
 *      routines that the fractal heap direct block (and global heap) serialize
 *      calls can use when resizing (and re-allocating) their image in the
 *      cache. -QAK */
#define H5C_DO_MEMORY_SANITY_CHECKS	0

/* H5C_COLLECT_CACHE_STATS controls overall collection of statistics
 * on cache activity.  In general, this #define should be set to 1 in
 * debug mode, and 0 in production mode..
 */

#ifndef NDEBUG
#define H5C_COLLECT_CACHE_STATS	1
#else /* NDEBUG */
#define H5C_COLLECT_CACHE_STATS	0
#endif /* NDEBUG */

/* H5C_COLLECT_CACHE_ENTRY_STATS controls collection of statistics
 * in individual cache entries.
 *
 * H5C_COLLECT_CACHE_ENTRY_STATS should only be defined to true if
 * H5C_COLLECT_CACHE_STATS is also defined to true.
 */
#if H5C_COLLECT_CACHE_STATS
#define H5C_COLLECT_CACHE_ENTRY_STATS	1
#else
#define H5C_COLLECT_CACHE_ENTRY_STATS	0
#endif /* H5C_COLLECT_CACHE_STATS */


/****************************/
/* Library Private Typedefs */
/****************************/

/* Typedef for the main structure for the cache (defined in H5Cpkg.h) */
typedef struct H5C_t H5C_t;

/* Cache entry tag structure */
typedef struct H5C_tag_t {
    haddr_t value;
    int globality;
} H5C_tag_t;

/* Define enum for cache entry tag 'globality' value */
typedef enum {
    H5C_GLOBALITY_NONE=0, /* Non-global tag */
    H5C_GLOBALITY_MINOR,  /* global, not flushed during single object flush */
    H5C_GLOBALITY_MAJOR   /* global, needs flushed during single obect flush */
} H5C_tag_globality_t;

/*
 *
 * Struct H5C_class_t
 *
 * Instances of H5C_class_t are used to specify the callback functions
 * used by the metadata cache for each class of metadata cache entry.
 * The fields of the structure are discussed below:
 *
 * id:	Integer field containing the unique ID of the class of metadata
 * 	cache entries.
 *
 * name: Pointer to a string containing the name of the class of metadata
 * 	cache entries.
 *
 * mem_type:  Instance of H5FD_mem_t, that is used to supply the
 * 	mem type passed into H5F_block_read().
 *
 * flags:  Flags indicating class-specific behavior.
 *
 *	Whoever created the flags field neglected to document the meanings 
 *      of the flags he created.  Hence the following discussion of the 
 *	H5C__CLASS_SPECULATIVE_LOAD_FLAG and (to a lesser extent) 
 *	H5C__CLASS_COMPRESSED_FLAG should be viewed with suspicion, 
 *	as the meanings are divined from the source code, and thus may be 
 *	inaccurate.  Please correct any errors you find.
 *
 *	Possible flags are:
 *
 *	H5C__CLASS_NO_FLAGS_SET: No special processing.
 *
 *	H5C__CLASS_SPECULATIVE_LOAD_FLAG: This flag appears to be used
 *		only in H5C_load_entry().  When it is set, entries are 
 *		permitted to change their sizes on the first attempt 
 *		to load.  
 *
 *		If the new size is larger than the old, the read buffer
 *		is reallocated to the new size, loaded from file, and the 
 *		deserialize routine is called a second time on the 
 *		new buffer.  The entry returned by the first call to 
 *		the deserialize routine is discarded (via the free_icr
 *		call) after the new size is retrieved (via the image_len
 *		call).  Note that the new size is used as the size of the 
 *		entry in the cache.
 *
 *		If the new size is smaller than the old, no new loads 
 *		or desearializes are performed, but the new size becomes
 *		the size of the entry in the cache.
 *
 *		When this flag is set, an attempt to read past the 
 *		end of file is pemitted.  In this case, if the size 
 *		returned get_load_size callback would result in a 
 *		read past the end of file, the size is trunkated to 
 *		avoid this, and processing proceeds as normal.
 *
 *	H5C__CLASS_COMPRESSED_FLAG: This flags indicates that the entry 
 *		may be compressed -- i.e. its on disk image is run through 
 *		filters on the way to and from disk.  Thus the uncompressed 
 *		(or unfiltered) size of the entry may be different from the 
 *		size of the entry in file.  
 *
 *		This has the following implications:
 *
 *		On load, uncompressed size and load size may be different.  
 *		Presumably, load size will be smaller than uncompressed 
 *		size, but there is no requirement for this in the code 
 *		(but note that I have inserted an assertion to this effect, 
 *		which has not been triggered to date).
 *
 *		On insertion, compressed (AKA filtered, AKA on disk) size 
 *		is unknown, as the entry has yet to be run through filters.
 *		Compressed size is computed whenever the entry is 
 *		written (or the image is updated -- not relevant until 
 *		journaling is brought back).
 *
 *		On dirty (of a clean entry), compressed (AKA filtered, 
 *		AKA on disk) size becomes unknown.  Thus, compressed size
 *		must be computed by the pre-serialize callback before the 
 *		entry may be written.  
 *
 *		Once the compressed size becomes unknown, it remains so 
 *		until the on disk image is constructed.  
 *
 *		Observe that the cache needs to know the size of the entry
 *		for space allocation purposes.  Since the compressed size 
 *		can change or become unknown, it uses the uncompressed
 *		size which may change, but which should always be known.  
 *		The compressed size is used only for journaling and disk I/O.
 *
 *		While there is no logical reason why they could not be 
 *		combined, due to absence of need and for simplicity of code, 
 *		the cache does not permit both the the 
 *		H5C__CLASS_COMPRESSED_FLAG and the 
 *		H5C__CLASS_SPECULATIVE_LOAD_FLAG to be set in the same 
 *		instance of H5C_class_t.
 *
 *      The following flags may only appear in test code.
 *
 *	H5C__CLASS_NO_IO_FLAG:  This flag is intended only for use in test 
 *		code.  When it is set, any attempt to load an entry of 
 *		the type with this flag set will trigger an assertion 
 *		failure, and any flush of an entry with this flag set 
 *		will not result in any write to file.
 *
 *	H5C__CLASS_SKIP_READS: This flags is intended only for use in test
 *		code.  When it is set, reads on load will be skipped,
 *		and an uninitialize buffer will be passed to the 
 *		deserialize function.
 *		This flag is used in H5Oproxy.c because this client does not
 *		exist on disk.  See description in that file.
 *
 *	H5C__CLASS_SKIP_WRITES: This flags is intended only for use in test
 *		code.  When it is set, writes of buffers prepared by the 
 *		serialize callback will be skipped.
 *		This flag is used in H5Oproxy.c because this client does not
 *		exist on disk.  See description in that file.
 *
 * GET_LOAD_SIZE: Pointer to the 'get load size' function.
 *
 * 	This function determines the size of a metadata cache entry based
 *      on the parameter "image":
 *	(a) When the piece of metadata has not been read, "image" is null.  
 *	    This function determines the size based on the information in the 
 *	    parameter "udata" or an initial speculative guess.  The size is
 *	    returned in the parameter "image_len_ptr".
 *	(b) When the piece of metadata has been read, "image" is non-null.
 *	    This function might deserialize the needed metadata information
 *	    to determine the actual size.  The size is returned in the
 *	    parameter "actual_len_ptr".  The calculation of actual size is
 *	    needed for checksum verification.
 *
 * 	For an entry with H5C__CLASS_NO_FLAGS_SET:
 *	  This function computes either one of the following:
 *		(1) When "image" is null, it returns in "image_len_ptr" 
 *		    the on disk size of the entry.
 *		(2) When "image" is non-null, it does nothing.
 *		    The values stored in "image_len_ptr" and "actual_len_ptr" 
 *		    should be the same.
 *	
 * 	For an entry with H5C__CLASS_SPECULATIVE_LOAD_FLAG:
 *	  This function computes either one of the following:
 *		(1) When "image" is null, it returns in "image_len_ptr" 
 *		    the initial guess of the entry's on disk size.
 *		(2) When "image" is non-null, it returns in "actual_len_ptr" 
 *		    the actual size of the entry on disk by 
 *		    deserializing the needed metadata information.
 *
 * 	For an entry with H5C__CLASS_COMPRESSED_FLAG:
 *	  This function computes either one of the following:
 *		(1) When "image" is null, it returns in "image_len_ptr" 
 *		    the entry's on disk size:
 *		    --for a filtered entry: the compressed size
 *		    --for a non-filtered entry: the uncompressed size
 *		(2) When "image" is non-null, it returns in "actual_len_ptr:
 *		    --for a filtered entry: the de-compressed size based on 
 *		      "udata" information
 *		    --for a non-filtered entry: the uncompressed size
 *
 *	The typedef for the get_load_size callback is as follows:
 *
 * 	   typedef herr_t (*H5C_get_load_size_func_t)(const void *image_ptr,
 *						      void *udata_ptr,
 * 	                                              size_t *image_len_ptr,
 *						      size_t *actual_len_ptr,
 *						      size_t *compressed_ptr,
 *						      size_t *compressed_image_len_ptr);
 *
 *	The parameters of the get_load_size callback are as follows:
 *
 *	image_ptr: Pointer to a buffer containing the metadata read in.
 *
 *	udata_ptr: Pointer to user data provided in the protect call, which
 *         	will also be passed through to the deserialize callback.
 *
 *	image_len_ptr: Pointer to the location in which the length in bytes 
 *		of the in file image to be deserialized is to be returned.
 *
 *              This value is used by the cache to determine the size of
 *              the disk image for the metadata, in order to read the disk
 *              image from the file.
 *	
 *	actual_len_ptr: Pointer to the location containing the actual length
 *			of the metadata entry on disk.
 *
 *	compressed_ptr: See description in image_len callback.
 *	
 *	compressed_image_len_ptr: See description in image_len callback.
 *
 *	Processing in the get_load_size function should proceed as follows:
 *
 *	If successful, the function will place the length in either the
 *	*image_len_ptr or *actual_le_ptr associated with supplied user data
 *	and then return SUCCEED.
 *
 *	On failure, the function must return FAIL and push error information
 *	onto the error stack with the error API routines, without modifying
 *      the value pointed to by image_len_ptr or actual_len_ptr.
 *
 *
 * VERIFY_CHKSUM: Pointer to the verify_chksum function.
 *
 *	This function verifies the checksum computed for the metadata is
 *	the same as the checksum stored in the metadata.
 *
 *	It computes the checksum based on the metadata stored in the
 *	parameter "image_ptr" and the actual length of the metadata in the 
 * 	parameter "len"  which is obtained from the "get_load_size" callback.
 *
 *	For a filtered metadata entry with H5C__CLASS_COMPRESSED_FLAG, 
 *	"len" is the decompressed size.  This function will decompress 
 *	the metadata and compute the checksum for the metadata with 
 *	length "len".
 *
 *	The typedef for the verify_chksum callback is as follows:
 *
 *	   typedef htri_t (*H5C_verify_chksum_func_t)(const void *image_ptr, 
 *						      size_t len, 
 *						      void *udata_ptr);
 *
 *	The parameters of the verify_chksum callback are as follows:
 *	
 *	image_ptr: Pointer to a buffer containing the metadata read in.
 *
 *	len: The actual length of the metadata.
 *
 *	udata_ptr: Pointer to user data.
 *
 *
 * DESERIALIZE: Pointer to the deserialize function.
 *
 * 	This function must be able to read a buffer containing the on disk 
 *	image of a metadata cache entry, allocate and load the equivalent 
 *	in core representation, and return a pointer to that representation.
 *
 *	The typedef for the deserialize callback is as follows:
 *
 * 	   typedef void *(*H5C_deserialize_func_t)(const void * image_ptr,
 * 	                                           size_t len,
 *                                                 void * udata_ptr,
 *                                                 boolean * dirty_ptr);
 *
 *	The parameters of the deserialize callback are as follows:
 *
 *	image_ptr: Pointer to a buffer of length len containing the
 *		contents of the file starting at addr and continuing
 *		for len bytes.
 *
 *	len:    Length in bytes of the in file image to be deserialized.
 *
 *              This parameter is supplied mainly for sanity checking.
 *              Sanity checks should be performed when compiled in debug
 *              mode, but the parameter may be unused when compiled in
 *              production mode.
 *
 *	udata_ptr: Pointer to user data provided in the protect call, which
 *         	must be passed through to the deserialize callback.
 *
 *      dirty_ptr:  Pointer to boolean which the deserialize function
 *      	must use to mark the entry dirty if it has to modify
 *      	the entry to clean up file corruption left over from
 *      	an old bug in the HDF5 library.
 *
 *	Processing in the deserialize function should proceed as follows:
 *
 *	NOTE: With the modification to get_load_size callback (see above 
 *	description) to compute the actual length of a metadata entry for 
 *	checksum verification, the value in the parameter "len" will be the 
 *	actual length of the metadata and a subsequent call to image_len
 * 	callback is not needed.
 *
 *      If the image contains valid data, and is of the correct length,
 *      the deserialize function must allocate space for an in core
 *      representation of that data, load the contents of the image into
 *      the space allocated for the in core representation, and return
 *      a pointer to the in core representation.  Observe that an
 *      instance of H5C_cache_entry_t must be the first item in this
 *      representation.  The cache will initialize it after the callback
 *      returns.
 *
 *      Note that the structure of the in core representation is otherwise
 *      up to the cache client.  All that is required is that the pointer
 *      returned be sufficient for the clients purposes when it is returned
 *      on a protect call.
 *
 *      If the deserialize function has to clean up file corruption
 *      left over from an old bug in the HDF5 library, it must set
 *      *dirty_ptr to TRUE.  If it doesn't, no action is needed as
 *      *dirty_ptr will be set to FALSE before the deserialize call.
 *
 *      If the operation fails for any reason (i.e. bad data in buffer, bad
 *      buffer length, malloc failure, etc.) the function must return NULL and
 *      push error information on the error stack with the error API routines.
 *
 *	Exceptions to the above:
 *
 *	If the H5C__CLASS_SPECULATIVE_LOAD_FLAG is set, the buffer supplied
 *	to the function need not be currect on the first invocation of the 
 *	callback in any single attempt to load the entry.
 *
 *	In this case, if the buffer is larger than necessary, the function
 *	should load the entry as described above and not flag an error due
 *	to the oversized buffer.  The cache will correct its mis-apprehension
 *	of the entry size with a subsequent call to the image_len callback.
 *
 *	If the buffer is too small, and this is the first deserialize call
 *	in the entry load operation, the function should not flag an error.
 *	Instead, it must compute the correct size of the entry, allocate an
 *	in core representation and initialize it to the extent that an 
 *	immediate call to the image len callback will return the correct 
 *	image size.
 *
 *	In this case, when the deserialize callback returns, the cache will
 *	call the image length callback, realize that the supplied buffer was
 *	too small, discard the returned in core representation, allocate
 *	and load a new buffer of the correct size from file, and then call
 *	the deserialize callback again.
 *
 *	If the H5C__CLASS_COMPRESSED_FLAG is set, exceptions are as per the
 *	H5C__CLASS_SPECULATIVE_LOAD_FLAG, save that only oversized buffers
 *	are permitted.
 *
 *
 * IMAGE_LEN: Pointer to the image length callback.

 *	NOTE: With the modification to the get_load_size callback (see above
 *	description) due to checksum verification, the image_len callback 
 *	is mainly used for newly inserted entries and assert verification.
 *
 *	This callback exists primarily to support 
 *	H5C__CLASS_SPECULATIVE_LOAD_FLAG and H5C__CLASS_COMPRESSED_FLAG 
 *	discussed above, although it is also used to obtain the size of 
 *	newly inserted entries.
 *
 *	In the case of the H5C__CLASS_SPECULATIVE_LOAD_FLAG, it is used to
 *	allow the client to change the size of an entry in the deserialize
 *	callback.
 *
 *	For the H5C__CLASS_COMPRESSED_FLAG, it is used to allow the client
 *	to indicate whether the entry is compressed (i.e. whether entries
 *	are run through filters) and if so, to report both the uncompressed 
 *	and the compressed entry size (i.e. the actual on disk size after
 *	the entry has been run through filters) if that value is known.
 *
 *	The callback is also used in H5C_insert_entry() to obtain the 
 *	size of the newly inserted entry.
 *
 *      The typedef for the image_len callback is as follows:
 *
 *      typedef herr_t (*H5C_image_len_func_t)(void *thing,
 *                                           size_t *image_len_ptr,
 *                                           hbool_t *compressed_ptr,
 *                                           size_t *compressed_image_len_ptr);
 *
 * 	The parameters of the image_len callback are as follows:
 *
 *	thing:  Pointer to the in core representation of the entry.
 *
 *	image_len_ptr: Pointer to size_t in which the callback will return
 *		the length (in bytes) of the cache entry.
 *
 *		If the H5C__CLASS_COMPRESSED_FLAG is not set in the 
 *		associated instance of H5C_class_t, or if the flag is 
 *		set, and the callback sets *compressed_ptr to FALSE,
 *		this size is the actual size of the entry on disk.
 *
 *		Otherwise, this size is the uncompressed size of the 
 *		entry -- which the cache will use for all purposes OTHER
 *		than journal writes and disk I/O.
 *
 *	compressed_ptr: Pointer to a boolean flag indicating whether 
 *		the cache entry will be compressed / uncompressed on 
 *		disk writes / reads.
 *
 *		If the H5C__CLASS_COMPRESSED_FLAG is not set in the
 *              associated instance of H5C_class_t, *compressed_ptr 
 *		must be set to FALSE.
 *
 *		If the H5C__CLASS_COMPRESSED_FLAG is set in the
 *              associated instance of H5C_class_t, and filters are 
 *		not enabled, *compressed_ptr must be set to FALSE.
 *
 *		If the H5C__CLASS_COMPRESSED_FLAG is set in the
 *              associated instance of H5C_class_t, and filters are
 *		enabled, the callback must set *compressed_ptr to TRUE.
 *
 *		Note that *compressed_ptr will always be set to FALSE 
 *		by the caller prior to invocation of the callback.  Thus 
 *		callbacks for clients that don't set the 
 *		H5C__CLASS_COMPRESSED_FLAG can ignore this parameter.
 *
 *	compressed_image_len_ptr: Pointer to size_t in which the callback 
 *		may return the length (in bytes) of the compressed on 
 *		disk image of the entry, or the uncompressed size if the
 *		compressed size has not yet been calculated.
 *
 *		Since computing the compressed image len is expensive, 
 *		the callback should only report the most recently computed
 *		value -- which will typically be incorrect if the entry
 *		is dirty.
 *
 *		If *compressed_ptr is set to FALSE, *compressed_image_len_ptr
 *		should be set to zero.  However, as *compressed_image_len_ptr
 *		will be initialize to zero prior to the call, the callback 
 *		need not modify it if the H5C__CLASS_COMPRESSED_FLAG is 
 *		not set.
 *
 *	If the H5C__CLASS_COMPRESSED_FLAG is not set in the associated 
 *	instance of H5C_class_t, processing in the image_len function 
 *	should proceed as follows:
 *
 *	If successful, the function will place the length of the on disk
 *	image associated with the in core representation provided in the
 *	thing parameter in *image_len_ptr, and then return SUCCEED.  Since
 *	*compressed_ptr and *compressed_image_len_ptr will be initialized to 
 *	FALSE and zero respectively before the call, the callback need not
 *	modify these values, and may declare the associated parameters as 
 *	UNUSED.
 *
 *	If the H5C__CLASS_COMPRESSED_FLAG is set in the associated 
 *      instance of H5C_class_t, processing in the image_len function 
 *      should proceed as follows:
 *
 *	If successful, the function will place the uncompressed length of 
 *	the on disk image associated with the in core representation 
 *	provided in the thing parameter in *image_len_ptr.  If filters 
 *	are not enabled for the entry, it will set *compressed_ptr to FALSE,
 *      and *compressed_image_len_ptr to zero.  If filters are enabled, 
 *	it will set *compressed_ptr to TRUE.  In this case, it must set 
 *	*compressed_image_len_ptr equal to the last computed compressed
 *	if the compressed size, or to the uncompressed size if that value
 *	is yet to be computed.  In all cases, it will return SUCCEED if 
 *	successful.
 *
 *	In either case, if the function fails, it must return FAIL and 
 *	push error information onto the error stack with the error API 
 *	routines, and return without modifying the values pointed to by 
 *	the image_len_ptr, compressed_ptr, and compressed_image_len_ptr
 *	parameters.
 *
 * PRE_SERIALIZE: Pointer to the pre-serialize callback.
 *
 *	The pre-serialize callback is invoked by the metadata cache before
 *	it needs a current on-disk image of the metadata entry for purposes
 *	either constructing a journal or flushing the entry to disk.
 *
 *      If the client needs to change the address or compressed or 
 *	uncompressed length of the entry prior to flush, the pre-serialize 
 *	callback is responsible for these actions, so that the actual 
 *	serialize callback (described below) is only responsible for 
 *	serializing the data structure, not moving it on disk or resizing it.
 *
 *	In addition, the client may use the pre-serialize callback to 
 *	ensure that the entry is ready to be flushed -- in particular, 
 *	if the entry contains references to other entries that are in 
 *	temporary file space, the pre-serialize callback must move those
 *	entries into real file space so that the serialzed entry will 
 *	contain no invalid data.
 *
 *	One would think that the base address and length of
 *	the length of the entry's image on disk would be well known. 
 *	However, that need not be the case as free space section info
 *	entries will change size (and possibly location) depending on the 
 *	number of blocks of free space being manages, and fractal heap 
 *	direct blocks can change compressed size (and  possibly location) 
 *	on serialization if compression is enabled.  Similarly, it may
 *	be necessary to move entries from temporary to real file space.
 *
 *	The pre-serialize callback must report any such changes to the 
 *	cache, which must then update its internal structures as needed.
 *
 *	The typedef for the pre-serialize callback is as follows:
 *
 *	typedef herr_t (*H5C_pre_serialize_func_t)(const H5F_t *f,
 *                                             hid_t dxpl_id,
 *                                             void * thing,
 *                                             haddr_t addr,
 *                                             size_t len,
 *					       size_t compressed_len,
 *                                             haddr_t * new_addr_ptr,
 *                                             size_t * new_len_ptr,
 *					       size_t * new_compressed_len_ptr,
 *                                             unsigned * flags_ptr);
 *
 *	The parameters of the pre-serialize callback are as follows:
 *
 *	f:	File pointer -- needed if other metadata cache entries
 *		must be modified in the process of serializing the
 *		target entry.
 *
 *	dxpl_id: dxpl_id passed with the file pointer to the cache, and
 *	        passed on to the callback.  Necessary as some callbacks
 *	        revise the size and location of the target entry, or
 *	        possibly other entries on pre-serialize.
 *
 *	thing:  Pointer to void containing the address of the in core
 *		representation of the target metadata cache entry. 
 *		This is the same pointer returned by a protect of the
 *		addr and len given above.
 *
 *	addr:   Base address in file of the entry to be serialized.
 *
 *		This parameter is supplied mainly for sanity checking.
 *		Sanity checks should be performed when compiled in debug
 *		mode, but the parameter may be unused when compiled in
 *		production mode.
 *
 *	len:    Length in bytes of the in file image of the entry to be
 *		serialized.  Also the size the image passed to the 
 *		serialize callback (discussed below) unless either that 
 *		value is altered by this function, or the entry will be
 *		compressed.  In the latter case, the compressed size
 *		of the entry will be reported in *new_compressed_len_ptr.
 *
 *		This parameter is supplied mainly for sanity checking.
 *		Sanity checks should be performed when compiled in debug
 *		mode, but the parameter may be unused when compiled in
 *		production mode.
 *
 *	compressed_len: If the entry is to be compressed (i.e. run through 
 *		filters) prior to flush, Length in bytes of the last know 
 *		compressed size of the entry -- or the uncompressed size 
 *		if no such value exists (i.e. the entry has been inserted, 
 *		but never flushed).  This parameter should be set to zero
 *		in all other cases.
 *
 *		This parameter is supplied mainly for sanity checking.
 *		Sanity checks should be performed when compiled in debug
 *		mode, but the parameter may be unused when compiled in
 *		production mode.
 *
 *	new_addr_ptr:  Pointer to haddr_t.  If the entry is moved by
 *		the serialize function, the new on disk base address must
 *		be stored in *new_addr_ptr, and the appropriate flag set
 *		in *flags_ptr.  
 *
 *		If the entry is not moved by the serialize function, 
 *		*new_addr_ptr is undefined on pre-serialize callback 
 *		return.
 *
 *	new_len_ptr:  Pointer to size_t.  If the entry is resized by the
 *		serialize function, the new length of the on disk image
 *		must be stored in *new_len_ptr, and the appropriate flag set
 *              in *flags_ptr.
 *
 *		If the entry is not resized by the pre-serialize function, 
 *		*new_len_ptr is undefined on pre-serialize callback 
 *		return.
 *
 *	new_compressed_len_ptr: Pointer to size_t.  If the image will be
 *		compressed (i.e. run through filters) prior to being 
 *		written to disk, the compressed size (in bytes) of the
 *		on disk image must be stored in *new_compressed_len_ptr,
 *		and the appropriate flag set in *flags_ptr.  
 *
 *	flags_ptr:  Pointer to an unsigned integer used to return flags
 *		indicating whether the preserialize function resized or moved
 *		the entry, or computed its compressed size.  If the entry was 
 *		neither resized or moved, nor will be compressed,
 *		the serialize function must set *flags_ptr to zero. 
 *		H5C__SERIALIZE_RESIZED_FLAG, H5C__SERIALIZE_MOVED_FLAG
 *		and H5C__SERIALIZE_COMPRESSED_FLAG must be set to indicate 
 *		a resize, a move, or compression respectively.
 *
 *	        If the H5C__SERIALIZE_RESIZED_FLAG is set, the new length
 *	        must be stored in *new_len_ptr.
 *
 *	        If the H5C__SERIALIZE_MOVED_FLAG flag is set, the
 *	        new image base address must be stored in *new_addr_ptr. 
 *
 *		If the H5C__SERIALIZE_COMPRESSED_FLAG is set, the 
 *		compressed size of the new image must be stored in 
 *		*new_compressed_len_ptr. 
 *
 *	Processing in the pre-serialize function should proceed as follows:
 *
 *	The pre-serialize function must examine the in core representation
 *	indicated by the thing parameter, if the pre-serialize function does
 *      not need to change the size or location of the on-disk image, or
 *	compute its compress size, it must set *flags_ptr to zero. 
 *
 *	If the (uncompressed) size of the on-disk image must be changed, 
 *	the pre-serialize function must load the length of the new image 
 *	into *new_len_ptr, and set the H5C__SERIALIZE_RESIZED_FLAG in 
 *	*flags_ptr. 
 *
 *	If the base address of the on disk image must be changed, the
 *      pre-serialize function must set *new_addr_ptr to the new base address,
 *      and set the H5C__SERIALIZE_MOVED_FLAG in *flags_ptr.
 *
 *	If the H5C__CLASS_COMPRESSED_FLAG is set in the assocated instance
 *	of H5C_class_t, and filters (i.e. compression) are enabled, the 
 *	pre-serialize function must compute the compressed size of the 
 *	on disk image, and if it has changed, load this value into 
 *	*new_compressed_len_ptr, and set H5C__SERIALIZE_COMPRESSED_FLAG in 
 *	*flags_ptr.
 *
 *	Note that to do this, the preserialize function will typically have 
 *	to serialize the entry, and run it through the filters to obtain 
 *	the compressed size.  For efficiency, the compressed image may
 *	be stored to be copied into the supplied buffer by the 
 *	serialize callback.  Needless to say this is awkward.  We may
 *	want to re-work the API for cache clients to simplify this.
 *
 *	In addition, the pre-serialize callback may perform any other 
 *	processing required before the entry is written to disk
 *
 *	If it is successful, the function must return SUCCEED. 
 *
 *	If it fails for any reason, the function must return FAIL and
 *	push error information on the error stack with the error API
 *	routines.
 *
 *
 * SERIALIZE: Pointer to the serialize callback.
 *
 *	The serialize callback is invoked by the metadata cache whenever
 *	it needs a current on disk image of the metadata entry for purposes
 *	either constructing a journal entry or flushing the entry to disk.
 *
 *	At this point, the base address and length of the entry's image on
 *      disk must be well known and not change during the serialization
 *      process. 
 *
 *	While any size and/or location changes must have been handled 
 *	by a pre-serialize call, the client may elect to handle any other 
 *	changes to the entry required to place it in correct form for 
 *	writing to disk in this call.
 *
 *	The typedef for the serialize callback is as follows:
 *
 *	typedef herr_t (*H5C_serialize_func_t)(const H5F_t *f,
 *                                             void * image_ptr,
 *                                             size_t len,
 *                                             void * thing);
 *
 *	The parameters of the serialize callback are as follows:
 *
 *	f:	File pointer -- needed if other metadata cache entries
 *		must be modified in the process of serializing the
 *		target entry.
 *
 *	image_ptr: Pointer to a buffer of length len bytes into which a
 *		serialized image of the target metadata cache entry is
 *		to be written.
 *
 * 		Note that this buffer will not in general be initialized
 * 		to any particular value.  Thus the serialize function may
 * 		not assume any initial value and must set each byte in
 * 		the buffer.
 *
 *	len:    Length in bytes of the in file image of the entry to be
 *		serialized.  Also the size of *image_ptr (below).  If 
 *		compression is not enabled, this value is simply the 
 *		uncompressed size of the entry's image on disk.  If 
 *		compression is enabled, this value is the size of the 
 *		compressed image.
 *
 *		This parameter is supplied mainly for sanity checking.
 *		Sanity checks should be performed when compiled in debug
 *		mode, but the parameter may be unused when compiled in
 *		production mode.
 *
 *	thing:  Pointer to void containing the address of the in core
 *		representation of the target metadata cache entry. 
 *		This is the same pointer returned by a protect of the
 *		addr and len given above.
 *
 *	Processing in the serialize function should proceed as follows:
 *
 *	If there are any remaining changes to the entry required before 
 *	write to disk, they must be dealt with first.
 *
 *	The serialize function must then examine the in core 
 *	representation indicated by the thing parameter, and write a 
 *	serialized (and possibly compressed) image of its contents into 
 *	the provided buffer. 
 *
 *	If it is successful, the function must return SUCCEED. 
 *
 *	If it fails for any reason, the function must return FAIL and
 *	push error information on the error stack with the error API
 *	routines.
 *
 *
 * NOTIFY: Pointer to the notify callback.
 *
 *      The notify callback is invoked by the metadata cache when a cache
 *      action on an entry has taken/will take place and the client indicates
 *      it wishes to be notified about the action.
 *
 *	The typedef for the notify callback is as follows:
 *
 *	typedef herr_t (*H5C_notify_func_t)(H5C_notify_action_t action,
 *                                          void *thing);
 *
 * 	The parameters of the notify callback are as follows:
 *
 *	action: An enum indicating the metadata cache action that has taken/
 *              will take place.
 *
 *	thing:  Pointer to void containing the address of the in core
 *		representation of the target metadata cache entry.  This
 *		is the same pointer that would be returned by a protect
 *		of the addr and len of the entry. 
 *
 *	Processing in the notify function should proceed as follows:
 *
 *	The notify function may perform any action it would like, including
 *      metadata cache calls.
 *
 *	If the function is successful, it must return SUCCEED. 
 *
 *	If it fails for any reason, the function must return FAIL and
 *	push error information on the error stack with the error API
 *	routines. 
 *
 *
 * FREE_ICR: Pointer to the free ICR callback.
 *
 *	The free ICR callback is invoked by the metadata cache when it
 *	wishes to evict an entry, and needs the client to free the memory
 *	allocated for the in core representation. 
 *
 *	The typedef for the free ICR callback is as follows:
 *
 *	typedef herr_t (*H5C_free_icr_func_t)(void * thing));
 *
 * 	The parameters of the free ICR callback are as follows:
 *
 *	thing:  Pointer to void containing the address of the in core
 *		representation of the target metadata cache entry.  This
 *		is the same pointer that would be returned by a protect
 *		of the addr and len of the entry. 
 *
 *	Processing in the free ICR function should proceed as follows:
 *
 *	The free ICR function must free all memory allocated to the
 *	in core representation. 
 *
 *	If the function is successful, it must return SUCCEED. 
 *
 *	If it fails for any reason, the function must return FAIL and
 *	push error information on the error stack with the error API
 *	routines. 
 *
 *	At least when compiled with debug, it would be useful if the
 *	free ICR call would fail if the in core representation has been
 *	modified since the last serialize of clear callback.
 *
 * CLEAR: Pointer to the clear callback.
 *
 *	In principle, there should be no need for the clear callback,
 *	as the dirty flag should be maintained by the metadata cache.
 *.  
 *	However, some clients maintain dirty bits on internal data, 
 *	and we need some way of keeping these dirty bits in sync with 
 *	those maintained by the metadata cache.  This callback exists
 *	to serve this purpose.  If defined, it is called whenever the 
 *	cache marks dirty entry clean, or when the cache is about to 
 *	discard a dirty entry without writing it to disk (This 
 *	happens as the result of an unprotect call with the 
 *	H5AC__DELETED_FLAG set, and the H5C__TAKE_OWNERSHIP_FLAG not
 *	set.)
 *
 *	Arguably, this functionality should be in the NOTIFY callback.
 *	However, this callback is specific to only a few clients, and 
 *	it will be called relatively frequently.  Hence it is made its
 *	own callback to minimize overhead.
 *
 *	The typedef for the clear callback is as follows:
 *
 *	typedef herr_t (*H5C_clear_func_t)(const H5F_t *f,
 *                                         void * thing, 
 *                                         hbool_t about_to_destroy);
 *
 *	The parameters of the clear callback are as follows:
 *
 *	f:	File pointer.
 *
 *	thing:  Pointer to void containing the address of the in core
 *		representation of the target metadata cache entry.  This
 *		is the same pointer that would be returned by a protect()
 *		call of the associated addr and len.
 *
 *	about_to_destroy: Boolean flag used to indicate whether the 
 *		metadata cache is about to destroy the target metadata
 *		cache entry.  The callback may use this flag to omit
 *		operations that are irrelevant it the entry is about 
 *		to be destroyed.
 *
 *	Processing in the clear function should proceed as follows:
 *
 *	Reset all internal dirty bits in the target metadata cache entry.
 *
 *	If the about_to_destroy flag is TRUE, the clear function may 
 *	ommit any dirty bit that will not trigger a sanity check failure 
 *	or otherwise cause problems in the subsequent free icr call.
 *	In particular, the call must ensure that the free icr call will
 *	not fail due to changes prior to this call, and after the 
 *	last serialize or clear call.
 *
 *	If the function is successful, it must return SUCCEED. 
 *
 *	If it fails for any reason, the function must return FAIL and
 *	push error information on the error stack with the error API
 *	routines. 
 *
 * GET_FSF_SIZE: Pointer to the get file space free size callback.
 *
 *	In principle, there is no need for the get file space free size
 *	callback.  However, as an optimization, it is sometimes convenient
 *	to allocate and free file space for a number of cache entries 
 *	simultaneously in a single contiguous block of file space.
 *
 *	File space allocation is done by the client, so the metadata cache
 *	need not be involved.  However, since the metadata cache typically
 *      handles file space release when an entry is destroyed, some 
 *	adjustment on the part of the metadata cache is required for this
 *	operation.
 *
 *      The get file space free size callback exists to support this 
 *	operation.
 *
 *	If a group of cache entries that were allocated as a group are to 
 *	be discarded and their file space released, the type of the first
 *	(i.e. lowest address) entry in the group must implement the 
 *	get free file space size callback.  
 *
 *	To free the file space of all entries in the group in a single 
 *	operation, first expunge all entries other than the first without 
 *	the free file space flag.  
 *
 *	Then, to complete the operation, unprotect or expunge the first
 *	entry in the block with the free file space flag set.  Since 
 *	the get free file space callback is implemented, the metadata 
 *	cache will use this callback to get the size of the block to be
 *	freed, instead of using the size of the entry as is done otherwise.
 *
 *	At present this callback is used only by the H5FA and H5EA dblock
 *	and dblock page client classes.
 *
 *      The typedef for the clear callback is as follows:
 *
 *      typedef herr_t (*H5C_get_fsf_size_t)(const void * thing,
 *                                                size_t *fsf_size_ptr);
 *
 *      The parameters of the clear callback are as follows:
 *
 *      thing:  Pointer to void containing the address of the in core
 *              representation of the target metadata cache entry.  This
 *              is the same pointer that would be returned by a protect()
 *              call of the associated addr and len.
 *
 *	fs_size_ptr: Pointer to size_t in which the callback will return
 *              the size of the piece of file space to be freed.  Note 
 *		that the space to be freed is presumed to have the same 
 *		base address as the cache entry.
 *
 *      The function simply returns the size of the block of file space
 *	to be freed in *fsf_size_ptr.  
 *
 *	If the function is successful, it must return SUCCEED.
 *
 *      If it fails for any reason, the function must return FAIL and
 *      push error information on the error stack with the error API
 *      routines.
 *
 ***************************************************************************/

/* Actions that can be reported to 'notify' client callback */
typedef enum H5C_notify_action_t {
    H5C_NOTIFY_ACTION_AFTER_INSERT,     /* Entry has been added to the cache 
                                         * via the insert call
                                         */
    H5C_NOTIFY_ACTION_AFTER_LOAD,	/* Entry has been loaded into the 
                                         * from file via the protect call
                                         */
    H5C_NOTIFY_ACTION_AFTER_FLUSH,	/* Entry has just been flushed to
 					 * file.
                                         */
    H5C_NOTIFY_ACTION_BEFORE_EVICT      /* Entry is about to be evicted 
                                         * from cache.
                                         */
} H5C_notify_action_t;

/* Cache client callback function pointers */
typedef herr_t (*H5C_get_load_size_func_t)(const void *image_ptr, void *udata_ptr,
    size_t *image_len_ptr, size_t *actual_len_ptr, 
    hbool_t *compressed_ptr, size_t *compressed_image_len_ptr);
typedef htri_t (*H5C_verify_chksum_func_t)(const void *image_ptr, size_t len, void *udata_ptr);
typedef void *(*H5C_deserialize_func_t)(const void *image_ptr,
    size_t len, void *udata_ptr, hbool_t *dirty_ptr);
typedef herr_t (*H5C_image_len_func_t)(const void *thing,
    size_t *image_len_ptr, hbool_t *compressed_ptr, size_t *compressed_image_len_ptr);
typedef herr_t (*H5C_pre_serialize_func_t)(const H5F_t *f, hid_t dxpl_id,
    void *thing, haddr_t addr, size_t len, size_t compressed_len,
    haddr_t *new_addr_ptr, size_t *new_len_ptr, size_t *new_compressed_len_ptr,
    unsigned *flags_ptr);
typedef herr_t (*H5C_serialize_func_t)(const H5F_t *f, void *image_ptr,
    size_t len, void *thing);
typedef herr_t (*H5C_notify_func_t)(H5C_notify_action_t action, void *thing);
typedef herr_t (*H5C_free_icr_func_t)(void *thing);
typedef herr_t (*H5C_clear_func_t)(const H5F_t *f, void * thing, 
    hbool_t about_to_destroy);
typedef herr_t (*H5C_get_fsf_size_t)(const void * thing, size_t *fsf_size_ptr);

/* Metadata cache client class definition */
typedef struct H5C_class_t {
    int				id;
    const char *		name;
    H5FD_mem_t			mem_type;
    unsigned			flags;
    H5C_get_load_size_func_t 	get_load_size;
    H5C_verify_chksum_func_t	verify_chksum;
    H5C_deserialize_func_t 	deserialize;
    H5C_image_len_func_t	image_len;
    H5C_pre_serialize_func_t	pre_serialize;
    H5C_serialize_func_t	serialize;
    H5C_notify_func_t		notify;
    H5C_free_icr_func_t	        free_icr;
    H5C_clear_func_t		clear;
    H5C_get_fsf_size_t		fsf_size;
} H5C_class_t;

/* Type defintions of callback functions used by the cache as a whole */
typedef herr_t (*H5C_write_permitted_func_t)(const H5F_t *f,
    hbool_t *write_permitted_ptr);
typedef herr_t (*H5C_log_flush_func_t)(H5C_t *cache_ptr, haddr_t addr,
    hbool_t was_dirty, unsigned flags);

/****************************************************************************
 *
 * H5C_ring_t & associated #defines
 *
 * The metadata cache uses the concept of rings to order the flushes of 
 * classes of entries.  In this arrangement, each entry in the cache is 
 * assigned to a ring, and on flush, the members of the outermost ring 
 * are flushed first, followed by the next outermost, and so on with the
 * members of the innermost ring being flushed last.  
 *
 * Note that flush dependencies are used to order flushes within rings.  
 *
 * Note also that at the conceptual level, rings are argueably superfluous,
 * as a similar effect could be obtained via the flush dependency mechanism.  
 * However, this would require all entries in the cache to participate in a 
 * flush dependency -- with the implied setup and takedown overhead and 
 * added complexity.  Further, the flush ordering between rings need only 
 * be enforced on flush operations, and thus the use of flush dependencies 
 * instead would apply unecessary constraints on flushes under normal 
 * operating circumstances.
 *
 * As of this writing, all metadata entries pretaining to data sets and 
 * groups must be flushed first, and are thus assigned to the outermost 
 * ring.  
 *
 * Free space managers managing file space must be flushed next,
 * and are assigned to the second outermost ring.
 *
 * The object header and associated chunks used to implement superblock 
 * extension messages must be flushed next, and are thus assigned to 
 * the third outermost ring.
 *
 * The superblock proper must be flushed last, and is thus assigned to 
 * the innermost ring.
 *
 * The H5C_ring_t and the associated #defines below are used to define
 * the rings.  Each entry must be assigned to the appropriate ring on 
 * insertion or protect.
 *
 * Note that H5C_ring_t was originally an enumerated type.  It was 
 * converted to an integer and a set of #defines for convenience in 
 * debugging.
 */

#define H5C_RING_UNDEFINED	0 /* shouldn't appear in the cache */
#define H5C_RING_USER		1 /* outermost ring */
#define H5C_RING_FSM		2
#define H5C_RING_SBE		4 /* temporarily merged with H5C_RING_SB */
#define H5C_RING_SB		4 /* innermost ring */
#define H5C_RING_NTYPES		5 

typedef int H5C_ring_t;


/****************************************************************************
 *
 * structure H5C_cache_entry_t
 *
 * Instances of the H5C_cache_entry_t structure are used to store cache
 * entries in a hash table and sometimes in a skip list.
 * See H5SL.c for the particulars of the skip list.
 *
 * In typical application, this structure is the first field in a
 * structure to be cached.  For historical reasons, the external module
 * is responsible for managing the is_dirty field (this is no longer
 * completely true.  See the comment on the is_dirty field for details).
 * All other fields are managed by the cache.
 *
 * The fields of this structure are discussed individually below:
 *
 *						JRM - 4/26/04
 *
 * magic:	Unsigned 32 bit integer that must always be set to
 *              H5C__H5C_CACHE_ENTRY_T_MAGIC when the entry is valid.
 *              The field must be set to H5C__H5C_CACHE_ENTRY_T_BAD_MAGIC
 *              just before the entry is freed.
 *
 *              This is necessary, as the LRU list can be changed out
 *              from under H5C_make_space_in_cache() by the serialize
 *              callback which may change the size of an existing entry,
 *              and/or load a new entry while serializing the target entry.
 *
 *              This in turn can cause a recursive call to
 *              H5C_make_space_in_cache() which may either flush or evict
 *              the next entry that the first invocation of that function
 *              was about to examine.
 *
 *              The magic field allows H5C_make_space_in_cache() to
 *              detect this case, and re-start its scan from the bottom
 *              of the LRU when this situation occurs.
 *
 * cache_ptr:	Pointer to the cache that this entry is contained within.
 *
 * addr:	Base address of the cache entry on disk.
 *
 * size:	Length of the cache entry on disk in bytes(exception: if 
 *		the entry is compressed on disk, this field contains the 
 *		uncompressed size of the entry -- see discussion of 
 *		compressed entries below).  Note that unlike normal 
 *		caches, the entries in this cache are of arbitrary size.
 *
 *		With the exception of compressed entries, the file space
 *		allocations for cache entries implied by the addr and size
 *		fields must be disjoint.  For compressed entries,
 *		the size field contains the uncompressed size -- thus in
 *		in this case, substitution of compressed size for size 
 *		must result in disjoint file space allocations.  However, 
 *		as discussed below, the compressed size may not be know.
 *
 *		Any entry whose associated instance of H5C_class_t has the
 *		H5C__CLASS_COMPRESSED_FLAG set may be compressed.  When
 *		an entry is compressed (that is, when filters are enabled
 *		on it), the compressed flag (see below) must be set, and 
 *		the compressed size (if known), must be stored in 
 *		the compressed_size field.
 *
 *		Since the compressed size will be unknown unless the 
 *		entry is clean, or has an up to date image (see the 
 *		image_ptr and image_up_to_date fields below), we use the 
 *		uncompressed size for all purposes other than disk I/O.  
 *		
 * compressed:	Boolean flag that is set iff the instance of H5C_class_t
 *		associated with the entry has the H5C__CLASS_COMPRESSED_FLAG
 *		set, and filters are enabled on the entry.
 *
 * compressed_size: If compressed is TRUE, this field contains the actual
 *		compressed size of the entry in bytes, which is also its 
 *		true size on disk -- or the uncompressed size if the 
 *		compressed size is unknown (i.e. the entry has been 
 *		inserted in the cache, but it has not been compressed yet).
 *              Note that this value will usually be incorrect if the 
 *		entry is dirty.
 *
 *		Since this value is frequently out of date and expensive to 
 *		compute, it is used only for disk I/O.  The uncompressed 
 *		size of the entry (stored in the size field above) is used 
 *		for all other purposes (i.e. computing the sum of the sizes 
 *		of all entries in the cache, etc.).
 *
 *		If compressed is FALSE, this field should contain 0.
 *
 * image_ptr:	Pointer to void.  When not NULL, this field points to a
 * 		dynamically allocated block of size bytes in which the
 * 		on disk image of the metadata cache entry is stored.
 *
 * 		If the entry is dirty, the pre-serialize and serialize 
 *		callbacks must be used to update this image before it is 
 *		written to disk
 *
 * image_up_to_date:  Boolean flag that is set to TRUE when *image_ptr
 * 		is up to date, and set to false when the entry is dirtied.
 *
 * type:	Pointer to the instance of H5C_class_t containing pointers
 *		to the methods for cache entries of the current type.  This
 *		field should be NULL when the instance of H5C_cache_entry_t
 *		is not in use.
 *
 *		The name is not particularly descriptive, but is retained
 *		to avoid changes in existing code.
 *
 * is_corked:	Boolean flag indicating whether the cache entry associated
 *		with an object is corked or not corked.
 *
 * is_dirty:	Boolean flag indicating whether the contents of the cache
 *		entry has been modified since the last time it was written
 *		to disk.
 *
 *		NOTE: For historical reasons, this field is not maintained
 *		      by the cache.  Instead, the module using the cache
 *		      sets this flag when it modifies the entry, and the
 *		      flush and clear functions supplied by that module
 *		      reset the dirty when appropriate.
 *
 *		      This is a bit quirky, so we may want to change this
 *		      someday.  However it will require a change in the
 *		      cache interface.
 *
 *		Update: Management of the is_dirty field has been largely
 *		      moved into the cache.  The only remaining exceptions
 *		      are the flush and clear functions supplied by the
 *		      modules using the cache.  These still clear the
 *		      is_dirty field as before.  -- JRM 7/5/05
 *
 *		Update: Management of the is_dirty field is now entirely
 *		      in the cache.		 -- JRM 7/5/07
 *
 * dirtied:	Boolean flag used to indicate that the entry has been
 * 		dirtied while protected.
 *
 * 		This field is set to FALSE in the protect call, and may
 * 		be set to TRUE by the H5C_mark_entry_dirty() call at any 
 *		time prior to the unprotect call.
 *
 * 		The H5C_mark_entry_dirty() call exists as a convenience 
 *		function for the fractal heap code which may not know if 
 *		an entry is protected or pinned, but knows that is either 
 *		protected or pinned.  The dirtied field was added as in 
 *		the parallel case, it is necessary to know whether a 
 *		protected entry is dirty prior to the protect call.
 *
 * is_protected: Boolean flag indicating whether this entry is protected
 *		(or locked, to use more conventional terms).  When it is
 *		protected, the entry cannot be flushed or accessed until
 *		it is unprotected (or unlocked -- again to use more
 *		conventional terms).
 *
 *		Note that protected entries are removed from the LRU lists
 *		and inserted on the protected list.
 *
 * is_read_only: Boolean flag that is only meaningful if is_protected is
 * 		TRUE.  In this circumstance, it indicates whether the
 * 		entry has been protected read only, or read/write.
 *
 * 		If the entry has been protected read only (i.e. is_protected
 * 		and is_read_only are both TRUE), we allow the entry to be
 * 		protected more than once.
 *
 *		In this case, the number of readers is maintained in the
 *		ro_ref_count field (see below), and unprotect calls simply
 *		decrement that field until it drops to zero, at which point
 *		the entry is actually unprotected.
 *
 * ro_ref_count: Integer field used to maintain a count of the number of
 * 		outstanding read only protects on this entry.  This field
 * 		must be zero whenever either is_protected or is_read_only
 * 		are TRUE.
 *
 * is_pinned:	Boolean flag indicating whether the entry has been pinned
 * 		in the cache.
 *
 * 		For very hot entries, the protect / unprotect overhead
 * 		can become excessive.  Thus the cache has been extended
 * 		to allow an entry to be "pinned" in the cache.
 *
 * 		Pinning an entry in the cache has several implications:
 *
 * 		1) A pinned entry cannot be evicted.  Thus unprotected
 * 		   pinned entries must be stored in the pinned entry
 * 		   list, instead of being managed by the replacement
 * 		   policy code (LRU at present).
 *
 * 		2) A pinned entry can be accessed or modified at any time.
 * 		   This places an extra burden on the pre-serialize and 
 *		   serialize callbacks, which must ensure that a pinned 
 *		   entry is consistant and ready to write to disk before 
 *		   generating an image.
 *
 * 		3) A pinned entry can be marked as dirty (and possibly
 *		   change size) while it is unprotected.
 *
 *		4) The flush-destroy code must allow pinned entries to
 *		   be unpinned (and possibly unprotected) during the
 *		   flush.
 *
 *		   					JRM -- 3/16/06
 *
 * in_slist:	Boolean flag indicating whether the entry is in the skip list
 *		As a general rule, entries are placed in the list when they
 *              are marked dirty.  However they may remain in the list after
 *              being flushed.
 *
 *              Update: Dirty entries are now removed from the skip list
 *			when they are flushed.
 *
 * flush_marker:  Boolean flag indicating that the entry is to be flushed
 *		the next time H5C_flush_cache() is called with the
 *		H5C__FLUSH_MARKED_ENTRIES_FLAG.  The flag is reset when
 *		the entry is flushed for whatever reason.
 *
 * flush_me_last:  Boolean flag indicating that this entry should not be
 *		flushed from the cache until all other entries without
 *              the flush_me_last flag set have been flushed.
 *
 *		Note: 
 *		
 *		At this time, the flush_me_last 
 *              flag will only be applied to one entry, the superblock,
 *              and the code utilizing these flags is protected with HDasserts
 *              to enforce this. This restraint can certainly be relaxed in
 *              the future if the the need for multiple entries getting flushed
 *              last or collectively arises, though the code allowing for that
 *              will need to be expanded and tested appropriately if that
 *              functionality is desired.
 *
 *		Update: There are now two possible last entries
 *                   	(superblock and file driver info message).  This
 *                   	number will probably increase as we add superblock
 *                   	messages.   JRM -- 11/18/14
 *
 * clear_on_unprotect:  Boolean flag used only in PHDF5.  When H5C is used
 *		to implement the metadata cache In the parallel case, only
 *		the cache with mpi rank 0 is allowed to actually write to
 *		file -- all other caches must retain dirty entries until they
 *		are advised that the entry is clean.
 *
 *		This flag is used in the case that such an advisory is
 *		received when the entry is protected.  If it is set when an
 *		entry is unprotected, and the dirtied flag is not set in
 *		the unprotect, the entry's is_dirty flag is reset by flushing
 *		it with the H5C__FLUSH_CLEAR_ONLY_FLAG.
 *
 * flush_immediately:  Boolean flag used only in Phdf5 -- and then only 
 *		for H5AC_METADATA_WRITE_STRATEGY__DISTRIBUTED.
 *
 *		When a destributed metadata write is triggered at a 
 *		sync point, this field is used to mark entries that 
 *		must be flushed before leaving the sync point.  At all
 *		other times, this field should be set to FALSE.
 *
 * flush_in_progress:  Boolean flag that is set to true iff the entry
 * 		is in the process of being flushed.  This allows the cache
 * 		to detect when a call is the result of a flush callback.
 *
 * destroy_in_progress:  Boolean flag that is set to true iff the entry
 * 		is in the process of being flushed and destroyed.
 *
 *
 * Fields supporting rings for flush ordering:
 *
 * All entries in the metadata cache are assigned to a ring.  On cache 
 * flush, all entries in the outermost ring are flushed first, followed
 * by all members of the next outermost ring, and so on until the 
 * innermost ring is flushed.  Note that this ordering is ONLY applied 
 * in flush and serialize calls.  Rings are ignored during normal operations
 * in which entries are flushed as directed by the replacement policy.
 *
 * See the header comment on H5C_ring_t above for further details.
 *
 * Note that flush dependencies (see below) are used to order flushes
 * within rings.  Unlike rings, flush dependencies are applied to ALL
 * writes, not just those triggered by flush or serialize calls.
 *
 * ring:	Instance of H5C_ring_t indicating the ring to which this
 *		entry is assigned.
 *
 *
 * Fields supporting the 'flush dependency' feature:
 *
 * Entries in the cache may have a 'flush dependencies' on other entries in the
 * cache.  A flush dependency requires that all dirty child entries be flushed
 * to the file before a dirty parent entry (of those child entries) can be
 * flushed to the file.  This can be used by cache clients to create data
 * structures that allow Single-Writer/Multiple-Reader (SWMR) access for the
 * data structure.
 *
 * flush_dep_parent:    Pointer to the array of flush dependency parent entries
 *              for this entry.
 *
 * flush_dep_nparents:  Number of flush dependency parent entries for this
 *              entry, i.e. the number of valid elements in flush_dep_parent.
 *
 * flush_dep_parent_nalloc: The number of allocated elements in
 *              flush_dep_parent_nalloc.
 *
 * flush_dep_nchildren: Number of flush dependency children for this entry.  If
 *              this field is nonzero, then this entry must be pinned and
 *              therefore cannot be evicted.
 *
 * flush_dep_ndirty_children: Number of flush dependency children that are
 *              either dirty or have a nonzero flush_dep_ndirty_children.  If
 *              this field is nonzero, then this entry cannot be flushed.
 *
 *
 * Fields supporting the hash table:
 *
 * Fields in the cache are indexed by a more or less conventional hash table.
 * If there are multiple entries in any hash bin, they are stored in a doubly
 * linked list.
 *
 * ht_next:	Next pointer used by the hash table to store multiple
 *		entries in a single hash bin.  This field points to the
 *		next entry in the doubly linked list of entries in the
 *		hash bin, or NULL if there is no next entry.
 *
 * ht_prev:     Prev pointer used by the hash table to store multiple
 *              entries in a single hash bin.  This field points to the
 *              previous entry in the doubly linked list of entries in
 *		the hash bin, or NULL if there is no previuos entry.
 *
 *
 * Fields supporting replacement policies:
 *
 * The cache must have a replacement policy, and it will usually be
 * necessary for this structure to contain fields supporting that policy.
 *
 * While there has been interest in several replacement policies for
 * this cache, the initial development schedule is tight.  Thus I have
 * elected to support only a modified LRU policy for the first cut.
 *
 * When additional replacement policies are added, the fields in this
 * section will be used in different ways or not at all.  Thus the
 * documentation of these fields is repeated for each replacement policy.
 *
 * Modified LRU:
 *
 * When operating in parallel mode, we must ensure that a read does not
 * cause a write.  If it does, the process will hang, as the write will
 * be collective and the other processes will not know to participate.
 *
 * To deal with this issue, I have modified the usual LRU policy by adding
 * clean and dirty LRU lists to the usual LRU list.  When reading in
 * parallel mode, we evict from the clean LRU list only.  This implies
 * that we must try to ensure that the clean LRU list is reasonably well
 * stocked.  See the comments on H5C_t in H5Cpkg.h for more details.
 *
 * Note that even if we start with a completely clean cache, a sequence
 * of protects without unprotects can empty the clean LRU list.  In this
 * case, the cache must grow temporarily.  At the next write, we will
 * attempt to evict enough entries to get the cache down to its nominal
 * maximum size.
 *
 * The use of the replacement policy fields under the Modified LRU policy
 * is discussed below:
 *
 * next:	Next pointer in either the LRU, the protected list, or 
 *		the pinned list depending on the current values of 
 *		is_protected and is_pinned.  If there is no next entry 
 *		on the list, this field should be set to NULL.
 *
 * prev:	Prev pointer in either the LRU, the protected list,
 *		or the pinned list depending on the current values of 
 *		is_protected and is_pinned.  If there is no previous 
 *		entry on the list, this field should be set to NULL.
 *
 * aux_next:	Next pointer on either the clean or dirty LRU lists.
 *		This entry should be NULL when either is_protected or 
 *		is_pinned is true.  
 *
 *		When is_protected and is_pinned are false, and is_dirty is 
 *		true, it should point to the next item on the dirty LRU 
 *		list.  
 *
 *		When is_protected and is_pinned are false, and is_dirty is 
 *		false, it should point to the next item on the clean LRU 
 *		list.  In either case, when there is no next item, it 
 *		should be NULL.
 *
 * aux_prev:	Previous pointer on either the clean or dirty LRU lists.
 *		This entry should be NULL when either is_protected or 
 *		is_pinned is true.  
 *
 *		When is_protected and is_pinned are false, and is_dirty is 
 *		true, it should point to the previous item on the dirty 
 *		LRU list.  
 *	
 *		When is_protected and is_pinned are false, and is_dirty 
 *		is false, it should point to the previous item on the 
 *		clean LRU list.  
 *
 *		In either case, when there is no previous item, it should 
 *		be NULL.
 *
 * Cache entry stats collection fields:
 *
 * These fields should only be compiled in when both H5C_COLLECT_CACHE_STATS
 * and H5C_COLLECT_CACHE_ENTRY_STATS are true.  When present, they allow
 * collection of statistics on individual cache entries.
 *
 * accesses:	int32_t containing the number of times this cache entry has
 *		been referenced in its lifetime.
 *
 * clears:	int32_t containing the number of times this cache entry has
 *              been cleared in its life time.
 *
 * flushes:	int32_t containing the number of times this cache entry has
 *              been flushed to file in its life time.
 *
 * pins:	int32_t containing the number of times this cache entry has
 * 		been pinned in cache in its life time.
 *
 ****************************************************************************/
typedef struct H5C_cache_entry_t {
    uint32_t			magic;
    H5C_t                     * cache_ptr;
    haddr_t			addr;
    size_t			size;
    hbool_t			compressed;
    size_t			compressed_size;
    void  		      *	image_ptr;
    hbool_t			image_up_to_date;
    const H5C_class_t	      *	type;
    haddr_t		        tag;
    int				globality;
    hbool_t			is_corked;
    hbool_t			is_dirty;
    hbool_t			dirtied;
    hbool_t			is_protected;
    hbool_t			is_read_only;
    int				ro_ref_count;
    hbool_t			is_pinned;
    hbool_t			in_slist;
    hbool_t			flush_marker;
    hbool_t                     flush_me_last;
#ifdef H5_HAVE_PARALLEL
    hbool_t			clear_on_unprotect;
    hbool_t			flush_immediately;
    hbool_t			coll_access;
#endif /* H5_HAVE_PARALLEL */
    hbool_t			flush_in_progress;
    hbool_t			destroy_in_progress;

    /* fields supporting rings for purposes of flush ordering */
    H5C_ring_t                  ring;

    /* fields supporting the 'flush dependency' feature: */
    struct H5C_cache_entry_t ** flush_dep_parent;
    unsigned                    flush_dep_nparents;
    unsigned                    flush_dep_parent_nalloc;
    unsigned                    flush_dep_nchildren;
    unsigned                    flush_dep_ndirty_children;
    hbool_t			pinned_from_client;
    hbool_t			pinned_from_cache;

    /* fields supporting the hash table: */
    struct H5C_cache_entry_t  *	ht_next;
    struct H5C_cache_entry_t  *	ht_prev;

    /* fields supporting replacement policies: */
    struct H5C_cache_entry_t  *	next;
    struct H5C_cache_entry_t  *	prev;
    struct H5C_cache_entry_t  *	aux_next;
    struct H5C_cache_entry_t  *	aux_prev;
#ifdef H5_HAVE_PARALLEL
    struct H5C_cache_entry_t  *	coll_next;
    struct H5C_cache_entry_t  *	coll_prev;
#endif /* H5_HAVE_PARALLEL */

#if H5C_COLLECT_CACHE_ENTRY_STATS
    /* cache entry stats fields */
    int32_t			accesses;
    int32_t			clears;
    int32_t			flushes;
    int32_t			pins;
#endif /* H5C_COLLECT_CACHE_ENTRY_STATS */
} H5C_cache_entry_t;

/****************************************************************************
 *
 * structure H5C_auto_size_ctl_t
 *
 * Instances of H5C_auto_size_ctl_t are used to get and set the control
 * fields for automatic cache re-sizing.
 *
 * The fields of the structure are discussed individually below:
 *
 * version: Integer field containing the version number of this version
 *	of the H5C_auto_size_ctl_t structure.  Any instance of
 *	H5C_auto_size_ctl_t passed to the cache must have a known
 *	version number, or an error will be flagged.
 *
 * report_fcn:  Pointer to the function that is to be called to report
 *      activities each time the auto cache resize code is executed.  If the
 *	field is NULL, no call is made.
 *
 *	If the field is not NULL, it must contain the address of a function
 *	of type H5C_auto_resize_report_fcn.
 *
 * set_initial_size: Boolean flag indicating whether the size of the
 *	initial size of the cache is to be set to the value given in
 *	the initial_size field.  If set_initial_size is FALSE, the
 *	initial_size field is ignored.
 *
 * initial_size: If enabled, this field contain the size the cache is
 *	to be set to upon receipt of this structure.  Needless to say,
 *	initial_size must lie in the closed interval [min_size, max_size].
 *
 * min_clean_fraction: double in the range 0 to 1 indicating the fraction
 *	of the cache that is to be kept clean.  This field is only used
 *	in parallel mode.  Typical values are 0.1 to 0.5.
 *
 * max_size: Maximum size to which the cache can be adjusted.  The
 *	supplied value must fall in the closed interval
 *	[MIN_MAX_CACHE_SIZE, MAX_MAX_CACHE_SIZE].  Also, max_size must
 *	be greater than or equal to min_size.
 *
 * min_size: Minimum size to which the cache can be adjusted.  The
 *      supplied value must fall in the closed interval
 *      [MIN_MAX_CACHE_SIZE, MAX_MAX_CACHE_SIZE].  Also, min_size must
 *	be less than or equal to max_size.
 *
 * epoch_length: Number of accesses on the cache over which to collect
 *	hit rate stats before running the automatic cache resize code,
 *      if it is enabled.
 *
 *	At the end of an epoch, we discard prior hit rate data and start
 * 	collecting afresh.  The epoch_length must lie in the closed
 *	interval [H5C__MIN_AR_EPOCH_LENGTH, H5C__MAX_AR_EPOCH_LENGTH].
 *
 *
 * Cache size increase control fields:
 *
 * incr_mode: Instance of the H5C_cache_incr_mode enumerated type whose
 *	value indicates how we determine whether the cache size should be
 *	increased.  At present there are two possible values:
 *
 *	H5C_incr__off:	Don't attempt to increase the size of the cache
 *		automatically.
 *
 *		When this increment mode is selected, the remaining fields
 *		in the cache size increase section ar ignored.
 *
 *	H5C_incr__threshold: Attempt to increase the size of the cache
 *		whenever the average hit rate over the last epoch drops
 *		below the value supplied in the lower_hr_threshold
 *		field.
 *
 *		Note that this attempt will fail if the cache is already
 *		at its maximum size, or if the cache is not already using
 *		all available space.
 *
 * lower_hr_threshold: Lower hit rate threshold.  If the increment mode
 *	(incr_mode) is H5C_incr__threshold and the hit rate drops below the
 *	value supplied in this field in an epoch, increment the cache size by
 *	size_increment.  Note that cache size may not be incremented above
 *	max_size, and that the increment may be further restricted by the
 *	max_increment field if it is enabled.
 *
 *	When enabled, this field must contain a value in the range [0.0, 1.0].
 *	Depending on the incr_mode selected, it may also have to be less than
 *	upper_hr_threshold.
 *
 * increment:  Double containing the multiplier used to derive the new
 *	cache size from the old if a cache size increment is triggered.
 *      The increment must be greater than 1.0, and should not exceed 2.0.
 *
 *	The new cache size is obtained by multiplying the current max cache
 *	size by the increment, and then clamping to max_size and to stay
 *	within the max_increment as necessary.
 *
 * apply_max_increment:  Boolean flag indicating whether the max_increment
 *	field should be used to limit the maximum cache size increment.
 *
 * max_increment: If enabled by the apply_max_increment field described
 *	above, this field contains the maximum number of bytes by which the
 *	cache size can be increased in a single re-size.
 *
 * flash_incr_mode:  Instance of the H5C_cache_flash_incr_mode enumerated
 *      type whose value indicates whether and by what algorithm we should
 *      make flash increases in the size of the cache to accomodate insertion
 *      of large entries and large increases in the size of a single entry.
 *
 *      The addition of the flash increment mode was occasioned by performance
 *      problems that appear when a local heap is increased to a size in excess
 *      of the current cache size.  While the existing re-size code dealt with
 *      this eventually, performance was very bad for the remainder of the
 *      epoch.
 *
 *      At present, there are two possible values for the flash_incr_mode:
 *
 *      H5C_flash_incr__off:  Don't perform flash increases in the size of
 *              the cache.
 *
 *      H5C_flash_incr__add_space:  Let x be either the size of a newly
 *              newly inserted entry, or the number of bytes by which the
 *              size of an existing entry has been increased.
 *
 *              If
 *                   x > flash_threshold * current max cache size,
 *
 *              increase the current maximum cache size by x * flash_multiple
 *              less any free space in the cache, and start a new epoch.  For
 *              now at least, pay no attention to the maximum increment.
 *
 *
 *      With a little thought, it should be obvious that the above flash
 *      cache size increase algorithm is not sufficient for all
 *      circumstances -- for example, suppose the user round robins through
 *      (1/flash_threshold) +1 groups, adding one data set to each on each
 *      pass.  Then all will increase in size at about the same time, requiring
 *      the max cache size to at least double to maintain acceptable
 *      performance, however the above flash increment algorithm will not be
 *      triggered.
 *
 *      Hopefully, the add space algorithm detailed above will be sufficient
 *      for the performance problems encountered to date.  However, we should
 *      expect to revisit the issue.
 *
 * flash_multiple: Double containing the multiple described above in the
 *      H5C_flash_incr__add_space section of the discussion of the
 *      flash_incr_mode section.  This field is ignored unless flash_incr_mode
 *      is H5C_flash_incr__add_space.
 *
 * flash_threshold: Double containing the factor by which current max cache
 * 	size is multiplied to obtain the size threshold for the add_space
 * 	flash increment algorithm.  The field is ignored unless
 * 	flash_incr_mode is H5C_flash_incr__add_space.
 *
 *
 * Cache size decrease control fields:
 *
 * decr_mode: Instance of the H5C_cache_decr_mode enumerated type whose
 *	value indicates how we determine whether the cache size should be
 *	decreased.  At present there are four possibilities.
 *
 *	H5C_decr__off:	Don't attempt to decrease the size of the cache
 *		automatically.
 *
 *		When this increment mode is selected, the remaining fields
 *		in the cache size decrease section are ignored.
 *
 *	H5C_decr__threshold: Attempt to decrease the size of the cache
 *		whenever the average hit rate over the last epoch rises
 *		above the value	supplied in the upper_hr_threshold
 *		field.
 *
 *	H5C_decr__age_out:  At the end of each epoch, search the cache for
 *		entries that have not been accessed for at least the number
 *		of epochs specified in the epochs_before_eviction field, and
 *		evict these entries.  Conceptually, the maximum cache size
 *		is then decreased to match the new actual cache size.  However,
 *		this reduction may be modified by the min_size, the
 *		max_decrement, and/or the empty_reserve.
 *
 *	H5C_decr__age_out_with_threshold:  Same as age_out, but we only
 *		attempt to reduce the cache size when the hit rate observed
 *		over the last epoch exceeds the value provided in the
 *		upper_hr_threshold field.
 *
 * upper_hr_threshold: Upper hit rate threshold.  The use of this field
 *	varies according to the current decr_mode:
 *
 *	H5C_decr__off or H5C_decr__age_out:  The value of this field is
 *		ignored.
 *
 *	H5C_decr__threshold:  If the hit rate exceeds this threshold in any
 *		epoch, attempt to decrement the cache size by size_decrement.
 *
 *		Note that cache size may not be decremented below min_size.
 *
 *		Note also that if the upper_threshold is 1.0, the cache size
 *		will never be reduced.
 *
 *	H5C_decr__age_out_with_threshold:  If the hit rate exceeds this
 *		threshold in any epoch, attempt to reduce the cache size
 *		by evicting entries that have not been accessed for more
 *		than the specified number of epochs.
 *
 * decrement: This field is only used when the decr_mode is
 *	H5C_decr__threshold.
 *
 *	The field is a double containing the multiplier used to derive the
 *	new cache size from the old if a cache size decrement is triggered.
 *	The decrement must be in the range 0.0 (in which case the cache will
 *      try to contract to its minimum size) to 1.0 (in which case the
 *      cache will never shrink).
 *
 * apply_max_decrement:  Boolean flag used to determine whether decrements
 *	in cache size are to be limited by the max_decrement field.
 *
 * max_decrement: Maximum number of bytes by which the cache size can be
 *	decreased in a single re-size.  Note that decrements may also be
 *	restricted by the min_size of the cache, and (in age out modes) by
 *	the empty_reserve field.
 *
 * epochs_before_eviction:  Integer field used in H5C_decr__age_out and
 *	H5C_decr__age_out_with_threshold decrement modes.
 *
 *	This field contains the number of epochs an entry must remain
 *	unaccessed before it is evicted in an attempt to reduce the
 *	cache size.  If applicable, this field must lie in the range
 *	[1, H5C__MAX_EPOCH_MARKERS].
 *
 * apply_empty_reserve:  Boolean field controlling whether the empty_reserve
 *	field is to be used in computing the new cache size when the
 *	decr_mode is H5C_decr__age_out or H5C_decr__age_out_with_threshold.
 *
 * empty_reserve:  To avoid a constant racheting down of cache size by small
 *	amounts in the H5C_decr__age_out and H5C_decr__age_out_with_threshold
 *	modes, this field allows one to require that any cache size
 *	reductions leave the specified fraction of unused space in the cache.
 *
 *	The value of this field must be in the range [0.0, 1.0].  I would
 *	expect typical values to be in the range of 0.01 to 0.1.
 *
 ****************************************************************************/

enum H5C_resize_status
{
    in_spec,
    increase,
    flash_increase,
    decrease,
    at_max_size,
    at_min_size,
    increase_disabled,
    decrease_disabled,
    not_full
}; /* enum H5C_resize_conditions */

typedef void (*H5C_auto_resize_rpt_fcn)(H5C_t * cache_ptr, int32_t version,
    double hit_rate, enum H5C_resize_status status, size_t old_max_cache_size,
    size_t new_max_cache_size, size_t old_min_clean_size, size_t new_min_clean_size);

typedef struct H5C_auto_size_ctl_t {
    /* general configuration fields: */
    int32_t				version;
    H5C_auto_resize_rpt_fcn		rpt_fcn;
    hbool_t				set_initial_size;
    size_t				initial_size;
    double				min_clean_fraction;
    size_t				max_size;
    size_t				min_size;
    int64_t				epoch_length;

    /* size increase control fields: */
    enum H5C_cache_incr_mode		incr_mode;
    double				lower_hr_threshold;
    double				increment;
    hbool_t				apply_max_increment;
    size_t				max_increment;
    enum H5C_cache_flash_incr_mode      flash_incr_mode;
    double                              flash_multiple;
    double                              flash_threshold;

    /* size decrease control fields: */
    enum H5C_cache_decr_mode		decr_mode;
    double				upper_hr_threshold;
    double				decrement;
    hbool_t				apply_max_decrement;
    size_t				max_decrement;
    int32_t				epochs_before_eviction;
    hbool_t				apply_empty_reserve;
    double				empty_reserve;
} H5C_auto_size_ctl_t;

/***************************************/
/* Library-private Function Prototypes */
/***************************************/

H5_DLL H5C_t *H5C_create(size_t max_cache_size, size_t min_clean_size,
    int max_type_id, const char *(*type_name_table_ptr),
    H5C_write_permitted_func_t check_write_permitted, hbool_t write_permitted,
    H5C_log_flush_func_t log_flush, void *aux_ptr);
H5_DLL herr_t H5C_set_up_logging(H5C_t *cache_ptr, const char log_location[], hbool_t start_immediately);
H5_DLL herr_t H5C_tear_down_logging(H5C_t *cache_ptr);
H5_DLL herr_t H5C_start_logging(H5C_t *cache_ptr);
H5_DLL herr_t H5C_stop_logging(H5C_t *cache_ptr);
H5_DLL herr_t H5C_get_logging_status(const H5C_t *cache_ptr, /*OUT*/ hbool_t *is_enabled,
    /*OUT*/ hbool_t *is_currently_logging);
H5_DLL herr_t H5C_write_log_message(const H5C_t *cache_ptr, const char message[]);

H5_DLL void H5C_def_auto_resize_rpt_fcn(H5C_t *cache_ptr, int32_t version,
    double hit_rate, enum H5C_resize_status status,
    size_t old_max_cache_size, size_t new_max_cache_size,
    size_t old_min_clean_size, size_t new_min_clean_size);
H5_DLL herr_t H5C_dest(H5F_t *f, hid_t dxpl_id);
H5_DLL herr_t H5C_evict(H5F_t *f, hid_t dxpl_id);
H5_DLL herr_t H5C_expunge_entry(H5F_t *f, hid_t dxpl_id, const H5C_class_t *type, haddr_t addr, unsigned flags);


H5_DLL herr_t H5C_flush_cache(H5F_t *f, hid_t dxpl_id, unsigned flags);
H5_DLL herr_t H5C_flush_tagged_entries(H5F_t * f, hid_t dxpl_id, haddr_t tag); 
H5_DLL herr_t H5C_evict_tagged_entries(H5F_t * f, hid_t dxpl_id, haddr_t tag);
H5_DLL herr_t H5C_flush_to_min_clean(H5F_t *f, hid_t dxpl_id);
H5_DLL herr_t H5C_get_cache_auto_resize_config(const H5C_t *cache_ptr,
    H5C_auto_size_ctl_t *config_ptr);
H5_DLL herr_t H5C_get_cache_size(H5C_t *cache_ptr, size_t *max_size_ptr,
    size_t *min_clean_size_ptr, size_t *cur_size_ptr,
    int32_t *cur_num_entries_ptr);
H5_DLL herr_t H5C_get_cache_hit_rate(H5C_t *cache_ptr, double *hit_rate_ptr);
H5_DLL herr_t H5C_get_entry_status(const H5F_t *f, haddr_t addr,
    size_t *size_ptr, hbool_t *in_cache_ptr, hbool_t *is_dirty_ptr,
    hbool_t *is_protected_ptr, hbool_t *is_pinned_ptr, hbool_t *is_corked_ptr,
    hbool_t *is_flush_dep_parent_ptr, hbool_t *is_flush_dep_child_ptr);
H5_DLL herr_t H5C_get_evictions_enabled(const H5C_t *cache_ptr, hbool_t *evictions_enabled_ptr);
H5_DLL void * H5C_get_aux_ptr(const H5C_t *cache_ptr);
H5_DLL FILE *H5C_get_trace_file_ptr(const H5C_t *cache_ptr);
H5_DLL FILE *H5C_get_trace_file_ptr_from_entry(const H5C_cache_entry_t *entry_ptr);
H5_DLL herr_t H5C_insert_entry(H5F_t *f, hid_t dxpl_id, const H5C_class_t *type,
    haddr_t addr, void *thing, unsigned int flags);
H5_DLL herr_t H5C_mark_entry_dirty(void *thing);
H5_DLL herr_t H5C_move_entry(H5C_t *cache_ptr, const H5C_class_t *type,
    haddr_t old_addr, haddr_t new_addr);
H5_DLL herr_t H5C_pin_protected_entry(void *thing);
H5_DLL herr_t H5C_create_flush_dependency(void *parent_thing, void *child_thing);
H5_DLL void * H5C_protect(H5F_t *f, hid_t dxpl_id, const H5C_class_t *type,
    haddr_t addr, void *udata, unsigned flags);
H5_DLL herr_t H5C_reset_cache_hit_rate_stats(H5C_t *cache_ptr);
H5_DLL herr_t H5C_resize_entry(void *thing, size_t new_size);
H5_DLL herr_t H5C_set_cache_auto_resize_config(H5C_t *cache_ptr, H5C_auto_size_ctl_t *config_ptr);
H5_DLL herr_t H5C_set_evictions_enabled(H5C_t *cache_ptr, hbool_t evictions_enabled);
H5_DLL herr_t H5C_set_prefix(H5C_t *cache_ptr, char *prefix);
H5_DLL herr_t H5C_set_trace_file_ptr(H5C_t *cache_ptr, FILE *trace_file_ptr);
H5_DLL herr_t H5C_stats(H5C_t *cache_ptr, const char *cache_name,
    hbool_t display_detailed_stats);
H5_DLL void H5C_stats__reset(H5C_t *cache_ptr);
H5_DLL herr_t H5C_dump_cache(H5C_t *cache_ptr, const char *cache_name);
H5_DLL herr_t H5C_unpin_entry(void *thing);
H5_DLL herr_t H5C_destroy_flush_dependency(void *parent_thing, void *child_thing);
H5_DLL herr_t H5C_unprotect(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *thing,
    unsigned int flags);
H5_DLL herr_t H5C_validate_resize_config(H5C_auto_size_ctl_t *config_ptr,
    unsigned int tests);
H5_DLL herr_t H5C_ignore_tags(H5C_t *cache_ptr);
H5_DLL void H5C_retag_entries(H5C_t * cache_ptr, haddr_t src_tag, haddr_t dest_tag);
H5_DLL herr_t H5C_cork(H5C_t *cache_ptr, haddr_t obj_addr, unsigned action, hbool_t *corked);
H5_DLL herr_t H5C_get_entry_ring(const H5F_t *f, haddr_t addr, H5C_ring_t *ring);

#ifdef H5_HAVE_PARALLEL
H5_DLL herr_t H5C_apply_candidate_list(H5F_t *f, hid_t dxpl_id,
    H5C_t *cache_ptr, int num_candidates, haddr_t *candidates_list_ptr,
    int mpi_rank, int mpi_size);
H5_DLL herr_t H5C_construct_candidate_list__clean_cache(H5C_t *cache_ptr);
H5_DLL herr_t H5C_construct_candidate_list__min_clean(H5C_t *cache_ptr);
H5_DLL herr_t H5C_clear_coll_entries(H5C_t * cache_ptr, hbool_t partial);
H5_DLL herr_t H5C_mark_entries_as_clean(H5F_t *f, hid_t dxpl_id, int32_t ce_array_len,
    haddr_t *ce_array_ptr);
#endif /* H5_HAVE_PARALLEL */

#ifndef NDEBUG	/* debugging functions */
H5_DLL herr_t H5C_get_entry_ptr_from_addr(const H5F_t *f, haddr_t addr,
    void **entry_ptr_ptr);
H5_DLL herr_t H5C_verify_entry_type(const H5F_t *f, haddr_t addr,
    const H5C_class_t *expected_type, hbool_t *in_cache_ptr,
    hbool_t *type_ok_ptr);
#endif /* NDEBUG */

#endif /* !_H5Cprivate_H */

