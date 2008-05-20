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

/* Programmer:  John Mainzer
 *              10/27/05
 *
 *		This file contains common #defines, type definitions, and
 *		externs for tests of the cache implemented in H5C2.c
 */
#include "h5test.h"
#include "H5Iprivate.h"
#include "H5ACprivate.h"

#define H5C2_PACKAGE             /*suppress error about including H5Cpkg   */

#include "H5C2pkg.h"

#define H5F_PACKAGE             /*suppress error about including H5Fpkg   */

#include "H5Fpkg.h"

#define NO_CHANGE       -1

#define USE_CORE_DRIVER 0

/* with apologies for the abuse of terminology... */

#define PICO_ENTRY_TYPE		0
#define NANO_ENTRY_TYPE		1
#define MICRO_ENTRY_TYPE	2
#define TINY_ENTRY_TYPE		3
#define SMALL_ENTRY_TYPE	4
#define MEDIUM_ENTRY_TYPE	5
#define LARGE_ENTRY_TYPE	6
#define HUGE_ENTRY_TYPE		7
#define MONSTER_ENTRY_TYPE	8
#define VARIABLE_ENTRY_TYPE	9

#define NUMBER_OF_ENTRY_TYPES   10	

#define PICO_ENTRY_SIZE		(size_t)1
#define NANO_ENTRY_SIZE		(size_t)4
#define MICRO_ENTRY_SIZE	(size_t)16
#define TINY_ENTRY_SIZE		(size_t)64
#define SMALL_ENTRY_SIZE	(size_t)256
#define MEDIUM_ENTRY_SIZE	(size_t)1024
#define LARGE_ENTRY_SIZE	(size_t)(4 * 1024)
#define HUGE_ENTRY_SIZE		(size_t)(16 * 1024)
#define MONSTER_ENTRY_SIZE	(size_t)(64 * 1024)
#define VARIABLE_ENTRY_SIZE	(size_t)(10 * 1024)

#define NUM_PICO_ENTRIES	(10 * 1024)
#define NUM_NANO_ENTRIES	(10 * 1024)
#define NUM_MICRO_ENTRIES	(10 * 1024)
#define NUM_TINY_ENTRIES	(10 * 1024)
#define NUM_SMALL_ENTRIES	(10 * 1024)
#define NUM_MEDIUM_ENTRIES	(10 * 1024)
#define NUM_LARGE_ENTRIES	(10 * 1024)
#define NUM_HUGE_ENTRIES	(10 * 1024)
#define NUM_MONSTER_ENTRIES	(10 * 1024)
#define NUM_VARIABLE_ENTRIES	(10 * 1024)

#define MAX_ENTRIES		(10 * 1024)


/* The choice of the BASE_ADDR below is arbitrary -- it just has to be 
 * larger than the superblock.
 */
#define BASE_ADDR		(haddr_t)1024 
#define PICO_BASE_ADDR		BASE_ADDR
#define NANO_BASE_ADDR		(haddr_t)(PICO_BASE_ADDR + \
                                      (PICO_ENTRY_SIZE * NUM_PICO_ENTRIES))
#define MICRO_BASE_ADDR		(haddr_t)(NANO_BASE_ADDR + \
                                      (NANO_ENTRY_SIZE * NUM_NANO_ENTRIES))
#define TINY_BASE_ADDR		(haddr_t)(MICRO_BASE_ADDR + \
			              (MICRO_ENTRY_SIZE * NUM_MICRO_ENTRIES))
#define SMALL_BASE_ADDR		(haddr_t)(TINY_BASE_ADDR + \
                                      (TINY_ENTRY_SIZE * NUM_TINY_ENTRIES))
#define MEDIUM_BASE_ADDR	(haddr_t)(SMALL_BASE_ADDR + \
                                      (SMALL_ENTRY_SIZE * NUM_SMALL_ENTRIES))
#define LARGE_BASE_ADDR		(haddr_t)(MEDIUM_BASE_ADDR + \
                                      (MEDIUM_ENTRY_SIZE * NUM_MEDIUM_ENTRIES))
#define HUGE_BASE_ADDR		(haddr_t)(LARGE_BASE_ADDR + \
                                      (LARGE_ENTRY_SIZE * NUM_LARGE_ENTRIES))
#define MONSTER_BASE_ADDR	(haddr_t)(HUGE_BASE_ADDR + \
                                      (HUGE_ENTRY_SIZE * NUM_HUGE_ENTRIES))
#define VARIABLE_BASE_ADDR	(haddr_t)(MONSTER_BASE_ADDR + \
				     (MONSTER_ENTRY_SIZE * NUM_MONSTER_ENTRIES))

#define PICO_ALT_BASE_ADDR	(haddr_t)(VARIABLE_BASE_ADDR + \
			           (VARIABLE_ENTRY_SIZE * NUM_VARIABLE_ENTRIES))
#define NANO_ALT_BASE_ADDR	(haddr_t)(PICO_ALT_BASE_ADDR + \
                                      (PICO_ENTRY_SIZE * NUM_PICO_ENTRIES))
#define MICRO_ALT_BASE_ADDR	(haddr_t)(NANO_ALT_BASE_ADDR + \
                                      (NANO_ENTRY_SIZE * NUM_NANO_ENTRIES))
#define TINY_ALT_BASE_ADDR	(haddr_t)(MICRO_ALT_BASE_ADDR + \
			              (MICRO_ENTRY_SIZE * NUM_MICRO_ENTRIES))
#define SMALL_ALT_BASE_ADDR	(haddr_t)(TINY_ALT_BASE_ADDR + \
                                      (TINY_ENTRY_SIZE * NUM_TINY_ENTRIES))
#define MEDIUM_ALT_BASE_ADDR	(haddr_t)(SMALL_ALT_BASE_ADDR + \
                                      (SMALL_ENTRY_SIZE * NUM_SMALL_ENTRIES))
#define LARGE_ALT_BASE_ADDR	(haddr_t)(MEDIUM_ALT_BASE_ADDR + \
                                      (MEDIUM_ENTRY_SIZE * NUM_MEDIUM_ENTRIES))
#define HUGE_ALT_BASE_ADDR	(haddr_t)(LARGE_ALT_BASE_ADDR + \
                                      (LARGE_ENTRY_SIZE * NUM_LARGE_ENTRIES))
#define MONSTER_ALT_BASE_ADDR	(haddr_t)(HUGE_ALT_BASE_ADDR + \
                                      (HUGE_ENTRY_SIZE * NUM_HUGE_ENTRIES))
#define VARIABLE_ALT_BASE_ADDR	(haddr_t)(MONSTER_ALT_BASE_ADDR + \
                                     (MONSTER_ENTRY_SIZE * NUM_MONSTER_ENTRIES))
#define MAX_ADDR		(haddr_t)(VARIABLE_ALT_BASE_ADDR + \
				   (VARIABLE_ENTRY_SIZE * NUM_VARIABLE_ENTRIES))
#define ADDR_SPACE_SIZE		(haddr_t)(MAX_ADDR - BASE_ADDR)

#define MAX_PINS	8	/* Maximum number of entries that can be
				 * directly pinned by a single entry.
				 */

#define FLUSH_OP__NO_OP		0
#define FLUSH_OP__DIRTY		1
#define FLUSH_OP__RESIZE	2
#define FLUSH_OP__RENAME	3
#define FLUSH_OP__MAX_OP	3

#define MAX_FLUSH_OPS		10	/* Maximum number of flush operations
					 * that can be associated with a 
					 * cache entry.
					 */

typedef struct flush_op
{
    int			op_code;	/* integer op code indicating the
					 * operation to be performed.  At
					 * present it must be one of:
					 *
					 *   FLUSH_OP__NO_OP
					 *   FLUSH_OP__DIRTY
					 *   FLUSH_OP__RESIZE
					 *   FLUSH_OP__RENAME
					 */
    int			type;		/* type code of the cache entry that
					 * is the target of the operation.
					 * This value is passed into the
					 * function implementing the flush
					 * operation.
					 */
    int			idx;		/* index of the cache entry that
					 * is the target of the operation.
					 * This value is passed into the
                                         * function implementing the flush
                                         * operation.
					 */
    hbool_t		flag;		/* boolean flag passed into the 
					 * function implementing the flush
					 * operation.  The meaning of the
					 * flag is dependant upon the flush
					 * operation:
					 *
					 * FLUSH_OP__DIRTY: TRUE iff the 
					 *   target is pinned, and is to 
					 *   be dirtied via the 
					 *   H5C_mark_pinned_entry_dirty()
					 *   call.
					 *
					 * FLUSH_OP__RESIZE: TRUE iff the
					 *   target is pinned, and is to 
					 *   be resized via the 
					 *   H5C_mark_pinned_entry_dirty()
					 *   call.
					 *
					 * FLUSH_OP__RENAME: TRUE iff the
					 *    target is to be renamed to 
					 *    its main address.
					 */
    size_t		size;		/* New target size in the 
					 * FLUSH_OP__RENAME operation.
					 * Unused elsewhere.
					 */
} flush_op;

typedef struct test_entry_t
{
    H5C2_cache_entry_t	  header;	/* entry data used by the cache
					 * -- must be first
                               		 */
    struct test_entry_t * self; 	/* pointer to this entry -- used for
					 * sanity checking.
                                         */
    H5F_t               * file_ptr;     /* pointer to the file in which the
                                         * entry resides, or NULL if the entry
                                         * is not in a file.
                                         */
    H5C2_t              * cache_ptr;	/* pointer to the cache in which
					 * the entry resides, or NULL if the
					 * entry is not in cache.
					 */
    hbool_t		  written_to_main_addr;
    					/* Flag indicating whether an image
					 * of the entry has been written to
					 * its main address.  Since we no 
					 * longer have a flush callback, we
					 * set this field to true whenever the
					 * entry is serialized while at its 
					 * main address.
					 */
    hbool_t		  written_to_alt_addr;
                                        /* Flag indicating whether an image 
					 * of the entry has been written to 
					 * its alternate address.  Since we no
					 * longer have a flush callback, we 
					 * set this field to true whenever the
					 * entry is serialized while at its
					 * alternate address.
					 */
    haddr_t		  addr;         /* where the cache thinks this entry
                                         * is located
                                         */
    hbool_t		  at_main_addr;	/* boolean flag indicating whether
					 * the entry is supposed to be at
					 * either its main or alternate
					 * address.
     					 */
    haddr_t		  main_addr;    /* initial location of the entry
                                         */
    haddr_t		  alt_addr;	/* location to which the entry
					 * can be relocated or "renamed"
                                         */
    size_t		  size;         /* how big the cache thinks this
                                         * entry is
                                         */
    int32_t		  type;		/* indicates which entry array this
					 * entry is in
                                         */
    int32_t		  index;	/* index in its entry array
                                         */
    int32_t		  serializes;	/* number of times this entry has
					 * been serialized.
                                         */
    int32_t		  deserializes;	/* number of times this entry has
                                         * been deserialized
                                         */
    hbool_t		  is_dirty;	/* entry has been modified since
                                         * last write
                                         */
    hbool_t		  is_protected;	/* entry should currently be on
					 * the cache's protected list.
                                         */
    hbool_t		  is_read_only; /* TRUE iff the entry should be 
					 * protected read only.
					 */
    int			  ro_ref_count; /* Number of outstanding read only
					 * protects on the entry.
					 */
    hbool_t		  is_pinned;	/* entry is currently pinned in
					 * the cache.
                                         */
    int			  pinning_ref_count; /* Number of entries that
					 * pin this entry in the cache.
					 * When this count drops to zero,
					 * this entry should be unpinned.
					 */
    int			  num_pins;     /* Number of entries that this
					 * entry pins in the cache.  This
					 * value must be in the range
					 * [0, MAX_PINS].
					 */
    int			  pin_type[MAX_PINS]; /* array of the types of entries
					 * pinned by this entry.
					 */
    int			  pin_idx[MAX_PINS]; /* array of the indicies of
					 * entries pinned by this entry.
					 */
    int			  num_flush_ops; /* integer field containing the
					 * number of flush operations to 
					 * be executed when the entry is 
					 * flushed.  This value must lie in
					 * the closed interval 
					 * [0, MAX_FLUSH_OPS].
					 */
    struct flush_op	  flush_ops[MAX_FLUSH_OPS]; /* Array of instances
					 * of struct flush_op detailing the 
					 * flush operations (if any) that
					 * are to be executed when the entry
					 * is flushed from the cache.
					 *
					 * num_flush_ops contains the number
					 * of valid entries in this array.
					 */
    hbool_t		  flush_op_self_resize_in_progress; /* Boolean flag 
					 * that is set to TRUE iff this 
					 * entry is being flushed, it has
					 * been resized by a resize flush
					 * op, and the flush function has
					 * not yet returned,  This field is
					 * used to turn off overactive santity 
					 * checking code that would otherwise 
					 * cause a false test failure.
					 */
    hbool_t		  deserialized; /* entry has been deserialized since 
					 * the last time it was reset.
                                         */
    hbool_t		  cleared;      /* entry has been cleared since the
                                         * last time it was reset.
                                         */
    hbool_t		  serialized;   /* entry has been serialized since the
                                         * last time it was reset.
                                         */
    hbool_t               destroyed;    /* entry has been destroyed since the
                                         * last time it was reset.
                                         */
} test_entry_t;

/* The following are cut down test versions of the hash table manipulation
 * macros from H5C2pkg.c, which have been further modified to avoid references
 * to the error reporting macros.  Needless to say, these macros must be
 * updated as necessary.
 */

#define H5C2__HASH_MASK          ((size_t)(H5C2__HASH_TABLE_LEN - 1) << 3)
#define H5C2__HASH_FCN(x)        (int)(((x) & H5C2__HASH_MASK) >> 3)

#define H5C2_TEST__PRE_HT_SEARCH_SC(cache_ptr, Addr)      \
if ( ( (cache_ptr) == NULL ) ||                           \
     ( (cache_ptr)->magic != H5C2__H5C2_T_MAGIC ) ||      \
     ( ! H5F_addr_defined(Addr) ) ||                      \
     ( H5C2__HASH_FCN(Addr) < 0 ) ||                      \
     ( H5C2__HASH_FCN(Addr) >= H5C2__HASH_TABLE_LEN ) ) { \
    HDfprintf(stdout, "Pre HT search SC failed.\n");      \
}

#define H5C2_TEST__POST_SUC_HT_SEARCH_SC(cache_ptr, entry_ptr, Addr, k) \
if ( ( (cache_ptr) == NULL ) ||                                         \
     ( (cache_ptr)->magic != H5C2__H5C2_T_MAGIC ) ||                    \
     ( (cache_ptr)->index_len < 1 ) ||                                  \
     ( (entry_ptr) == NULL ) ||                                         \
     ( (cache_ptr)->index_size < (entry_ptr)->size ) ||                 \
     ( H5F_addr_ne((entry_ptr)->addr, (Addr)) ) ||                      \
     ( (entry_ptr)->size <= 0 ) ||                                      \
     ( ((cache_ptr)->index)[k] == NULL ) ||                             \
     ( ( ((cache_ptr)->index)[k] != (entry_ptr) ) &&                    \
       ( (entry_ptr)->ht_prev == NULL ) ) ||                            \
     ( ( ((cache_ptr)->index)[k] == (entry_ptr) ) &&                    \
       ( (entry_ptr)->ht_prev != NULL ) ) ||                            \
     ( ( (entry_ptr)->ht_prev != NULL ) &&                              \
       ( (entry_ptr)->ht_prev->ht_next != (entry_ptr) ) ) ||            \
     ( ( (entry_ptr)->ht_next != NULL ) &&                              \
       ( (entry_ptr)->ht_next->ht_prev != (entry_ptr) ) ) ) {           \
    HDfprintf(stdout, "Post successful HT search SC failed.\n");        \
}


#define H5C2_TEST__SEARCH_INDEX(cache_ptr, Addr, entry_ptr)             \
{                                                                       \
    int k;                                                              \
    int depth = 0;                                                      \
    H5C2_TEST__PRE_HT_SEARCH_SC(cache_ptr, Addr)                        \
    k = H5C2__HASH_FCN(Addr);                                           \
    entry_ptr = ((cache_ptr)->index)[k];                                \
    while ( ( entry_ptr ) && ( H5F_addr_ne(Addr, (entry_ptr)->addr) ) ) \
    {                                                                   \
        (entry_ptr) = (entry_ptr)->ht_next;                             \
        (depth)++;                                                      \
    }                                                                   \
    if ( entry_ptr )                                                    \
    {                                                                   \
        H5C2_TEST__POST_SUC_HT_SEARCH_SC(cache_ptr, entry_ptr, Addr, k) \
        if ( entry_ptr != ((cache_ptr)->index)[k] )                     \
        {                                                               \
            if ( (entry_ptr)->ht_next )                                 \
            {                                                           \
                (entry_ptr)->ht_next->ht_prev = (entry_ptr)->ht_prev;   \
            }                                                           \
            HDassert( (entry_ptr)->ht_prev != NULL );                   \
            (entry_ptr)->ht_prev->ht_next = (entry_ptr)->ht_next;       \
            ((cache_ptr)->index)[k]->ht_prev = (entry_ptr);             \
            (entry_ptr)->ht_next = ((cache_ptr)->index)[k];             \
            (entry_ptr)->ht_prev = NULL;                                \
            ((cache_ptr)->index)[k] = (entry_ptr);                      \
        }                                                               \
    }                                                                   \
}


/* misc type definitions */

struct flush_cache_test_spec
{
    int			entry_num;
    int			entry_type;
    int			entry_index;
    hbool_t		insert_flag;
    hbool_t		dirty_flag;
    unsigned int	flags;
    hbool_t		expected_deserialized;
    hbool_t		expected_cleared;
    hbool_t		expected_serialized;
    hbool_t		expected_destroyed;
};

struct pe_flush_cache_test_spec
{
    int			entry_num;
    int			entry_type;
    int			entry_index;
    hbool_t		insert_flag;
    hbool_t		dirty_flag;
    unsigned int	flags;
    int			num_pins;
    int			pin_type[MAX_PINS];
    int			pin_idx[MAX_PINS];
    hbool_t		expected_deserialized;
    hbool_t		expected_cleared;
    hbool_t		expected_serialized;
    hbool_t		expected_destroyed;
};

struct fo_flush_entry_check
{
    int			entry_num;
    int			entry_type;
    int			entry_index;
    size_t		expected_size;
    hbool_t		in_cache;
    hbool_t		at_main_addr;
    hbool_t		is_dirty;
    hbool_t		is_protected;
    hbool_t		is_pinned;
    hbool_t		expected_deserialized;
    hbool_t		expected_cleared;
    hbool_t		expected_serialized;
    hbool_t		expected_destroyed;
};

struct fo_flush_cache_test_spec
{
    int				entry_num;
    int				entry_type;
    int				entry_index;
    hbool_t			insert_flag;
    unsigned int		flags;
    size_t			new_size;
    int				num_pins;
    int				pin_type[MAX_PINS];
    int				pin_idx[MAX_PINS];
    int				num_flush_ops;
    struct flush_op		flush_ops[MAX_FLUSH_OPS];
    hbool_t			expected_deserialized;
    hbool_t			expected_cleared;
    hbool_t			expected_serialized;
    hbool_t			expected_destroyed;
};

struct rename_entry_test_spec
{
    int			entry_type;
    int			entry_index;
    hbool_t		is_dirty;
    hbool_t		is_pinned;
};

struct expected_entry_status
{
    int			entry_type;
    int                 entry_index;
    size_t              size;
    hbool_t		in_cache;
    hbool_t             at_main_addr;
    hbool_t		is_dirty;
    hbool_t		is_protected;
    hbool_t		is_pinned;
    hbool_t		deserialized;
    hbool_t		cleared;
    hbool_t		serialized;
    hbool_t		destroyed;
};




/* global variable externs: */

extern hbool_t write_permitted2;
extern hbool_t pass2; /* set to false on error */
extern hbool_t skip_long_tests2;
extern hbool_t run_full_test2;
extern const char *failure_mssg2;
extern int express_test2;
extern int failures2;

extern test_entry_t pico_entries2[NUM_PICO_ENTRIES];
extern test_entry_t nano_entries2[NUM_NANO_ENTRIES];
extern test_entry_t micro_entries2[NUM_MICRO_ENTRIES];
extern test_entry_t tiny_entries2[NUM_TINY_ENTRIES];
extern test_entry_t small_entries2[NUM_SMALL_ENTRIES];
extern test_entry_t medium_entries2[NUM_MEDIUM_ENTRIES];
extern test_entry_t large_entries2[NUM_LARGE_ENTRIES];
extern test_entry_t huge_entries2[NUM_HUGE_ENTRIES];
extern test_entry_t monster_entries2[NUM_MONSTER_ENTRIES];

extern test_entry_t * entries2[NUMBER_OF_ENTRY_TYPES];
extern const int32_t max_indices2[NUMBER_OF_ENTRY_TYPES];
extern const size_t entry_sizes2[NUMBER_OF_ENTRY_TYPES];
extern const haddr_t base_addrs2[NUMBER_OF_ENTRY_TYPES];
extern const haddr_t alt_base_addrs2[NUMBER_OF_ENTRY_TYPES];
extern const char * entry_type_names2[NUMBER_OF_ENTRY_TYPES];


/* call back function declarations: */

herr_t check_write_permitted2(const H5F_t UNUSED * f,
                              hid_t UNUSED dxpl_id,
                              hbool_t * write_permitted_ptr);

herr_t pico_clear_dirty_bits(haddr_t addr, size_t len, void * thing);
herr_t nano_clear_dirty_bits(haddr_t addr, size_t len, void * thing);
herr_t micro_clear_dirty_bits(haddr_t addr, size_t len, void * thing);
herr_t tiny_clear_dirty_bits(haddr_t addr, size_t len, void * thing);
herr_t small_clear_dirty_bits(haddr_t addr, size_t len, void * thing);
herr_t medium_clear_dirty_bits(haddr_t addr, size_t len, void * thing);
herr_t large_clear_dirty_bits(haddr_t addr, size_t len, void * thing);
herr_t huge_clear_dirty_bits(haddr_t addr, size_t len, void * thing);
herr_t monster_clear_dirty_bits(haddr_t addr, size_t len, void * thing);
herr_t variable_clear_dirty_bits(haddr_t addr, size_t len, void * thing);


void * pico_deserialize(haddr_t addr, size_t len, const void * image_ptr, 
  		        const void * udata_ptr, hbool_t * dirty_ptr);
void * nano_deserialize(haddr_t addr, size_t len, const void * image_ptr, 
		        const void * udata_ptr, hbool_t * dirty_ptr);
void * micro_deserialize(haddr_t addr, size_t len, const void * image_ptr, 
		         const void * udata_ptr, hbool_t * dirty_ptr);
void * tiny_deserialize(haddr_t addr, size_t len, const void * image_ptr, 
		        const void * udata_ptr, hbool_t * dirty_ptr);
void * small_deserialize(haddr_t addr, size_t len, const void * image_ptr, 
		         const void * udata_ptr, hbool_t * dirty_ptr);
void * medium_deserialize(haddr_t addr, size_t len, const void * image_ptr, 
		          const void * udata_ptr, hbool_t * dirty_ptr);
void * large_deserialize(haddr_t addr, size_t len, const void * image_ptr, 
		         const void * udata_ptr, hbool_t * dirty_ptr);
void * huge_deserialize(haddr_t addr, size_t len, const void * image_ptr, 
		        const void * udata_ptr, hbool_t * dirty_ptr);
void * monster_deserialize(haddr_t addr, size_t len, const void * image_ptr, 
		           const void * udata_ptr, hbool_t * dirty_ptr);
void * variable_deserialize(haddr_t addr, size_t len, const void * image_ptr, 
		            const void * udata_ptr, hbool_t * dirty_ptr);

herr_t pico_image_len(void *thing, size_t *image_len_ptr);
herr_t nano_image_len(void *thing, size_t *image_len_ptr);
herr_t micro_image_len(void *thing, size_t *image_len_ptr);
herr_t tiny_image_len(void *thing, size_t *image_len_ptr);
herr_t small_image_len(void *thing, size_t *image_len_ptr);
herr_t medium_image_len(void *thing, size_t *image_len_ptr);
herr_t large_image_len(void *thing, size_t *image_len_ptr);
herr_t huge_image_len(void *thing, size_t *image_len_ptr);
herr_t monster_image_len(void *thing, size_t *image_len_ptr);
herr_t variable_image_len(void *thing, size_t *image_len_ptr);

herr_t pico_serialize(haddr_t addr, size_t len, void * image_ptr, 
		      void * thing, unsigned * flags_ptr, 
		      haddr_t * new_addr_ptr, size_t * new_len_ptr, 
		      void ** new_image_ptr_ptr);
herr_t nano_serialize(haddr_t addr, size_t len, void * image_ptr, 
		      void * thing, unsigned * flags_ptr, 
		      haddr_t * new_addr_ptr, size_t * new_len_ptr, 
		      void ** new_image_ptr_ptr);
herr_t micro_serialize(haddr_t addr, size_t len, void * image_ptr, 
		       void * thing, unsigned * flags_ptr, 
		       haddr_t * new_addr_ptr, size_t * new_len_ptr, 
		       void ** new_image_ptr_ptr);
herr_t tiny_serialize(haddr_t addr, size_t len, void * image_ptr, 
		      void * thing, unsigned * flags_ptr, 
		      haddr_t * new_addr_ptr, size_t * new_len_ptr, 
		      void ** new_image_ptr_ptr);
herr_t small_serialize(haddr_t addr, size_t len, void * image_ptr, 
		       void * thing, unsigned * flags_ptr, 
		       haddr_t * new_addr_ptr, size_t * new_len_ptr, 
		       void ** new_image_ptr_ptr);
herr_t medium_serialize(haddr_t addr, size_t len, void * image_ptr, 
	  	        void * thing, unsigned * flags_ptr, 
		        haddr_t * new_addr_ptr, size_t * new_len_ptr, 
		        void ** new_image_ptr_ptr);
herr_t large_serialize(haddr_t addr, size_t len, void * image_ptr, 
		       void * thing, unsigned * flags_ptr, 
		       haddr_t * new_addr_ptr, size_t * new_len_ptr, 
		       void ** new_image_ptr_ptr);
herr_t huge_serialize(haddr_t addr, size_t len, void * image_ptr, 
	              void * thing, unsigned * flags_ptr, 
	              haddr_t * new_addr_ptr, size_t * new_len_ptr, 
	              void ** new_image_ptr_ptr);
herr_t monster_serialize(haddr_t addr, size_t len, void * image_ptr, 
	                 void * thing, unsigned * flags_ptr, 
	                 haddr_t * new_addr_ptr, size_t * new_len_ptr, 
	                 void ** new_image_ptr_ptr);
herr_t variable_serialize(haddr_t addr, size_t len, void * image_ptr, 
	                  void * thing, unsigned * flags_ptr, 
	                  haddr_t * new_addr_ptr, size_t * new_len_ptr, 
	                  void ** new_image_ptr_ptr);

herr_t pico_free_icr(haddr_t addr, size_t len, void * thing);
herr_t nano_free_icr(haddr_t addr, size_t len, void * thing);
herr_t micro_free_icr(haddr_t addr, size_t len, void * thing);
herr_t tiny_free_icr(haddr_t addr, size_t len, void * thing);
herr_t small_free_icr(haddr_t addr, size_t len, void * thing);
herr_t medium_free_icr(haddr_t addr, size_t len, void * thing);
herr_t large_free_icr(haddr_t addr, size_t len, void * thing);
herr_t huge_free_icr(haddr_t addr, size_t len, void * thing);
herr_t monster_free_icr(haddr_t addr, size_t len, void * thing);
herr_t variable_free_icr(haddr_t addr, size_t len, void * thing);


/* callback table extern */

extern const H5C2_class_t types2[NUMBER_OF_ENTRY_TYPES];


/* function declarations: */

void add_flush_op2(int target_type,
                   int target_idx,
                   int op_code,
                   int type,
                   int idx,
                   hbool_t flag,
                   size_t size);


void addr_to_type_and_index2(haddr_t addr,
                             int32_t * type_ptr,
                             int32_t * index_ptr);

#if 0 /* keep this for a while -- it may be useful */
haddr_t type_and_index_to_addr2(int32_t type,
                                int32_t idx);
#endif

void dirty_entry2(H5F_t * file_ptr,
                  int32_t type,
                  int32_t idx,
                  hbool_t dirty_pin);

void expunge_entry2(H5F_t * file_ptr,
                    int32_t type,
                    int32_t idx);

void insert_entry2(H5F_t * file_ptr,
                   int32_t type,
                   int32_t idx,
                   hbool_t dirty,
                   unsigned int flags);

void mark_pinned_entry_dirty2(H5F_t * file_ptr,
	                      int32_t type,
		              int32_t idx,
		              hbool_t size_changed,
		              size_t  new_size);

void mark_pinned_or_protected_entry_dirty2(H5F_t * file_ptr,
                                           int32_t type,
                                           int32_t idx);

void rename_entry2(H5C2_t * cache_ptr,
                   int32_t type,
                   int32_t idx,
                   hbool_t main_addr);

void pin_protected_entry2(H5F_t * file_ptr,
                          int32_t type,
                          int32_t idx);

void protect_entry2(H5F_t * file_ptr,
                    int32_t type,
                    int32_t idx);

void protect_entry_ro2(H5F_t * file_ptr,
                       int32_t type,
                       int32_t idx);

hbool_t entry_in_cache2(H5C2_t * cache_ptr,
                        int32_t type,
                        int32_t idx);

void create_pinned_entry_dependency2(H5F_t * file_ptr,
		                     int pinning_type,
		                     int pinning_idx,
		                     int pinned_type,
		                     int pinned_idx);

void execute_flush_op2(H5F_t * file_ptr,
		      struct test_entry_t * entry_ptr,
                      struct flush_op * op_ptr,
		      unsigned * flags_ptr);

void reset_entries2(void);

void resize_entry2(H5F_t * file_ptr,
                   int32_t type,
                   int32_t idx,
                   size_t new_size,
                   hbool_t resize_pin);

void resize_pinned_entry2(H5F_t * file_ptr,
                          int32_t type,
                          int32_t idx,
                          size_t new_size);

H5F_t * setup_cache2(size_t max_cache_size, size_t min_clean_size);

void row_major_scan_forward2(H5F_t * file_ptr,
                             int32_t max_index,
                             int32_t lag,
                             hbool_t verbose,
                             hbool_t reset_stats,
                             hbool_t display_stats,
                             hbool_t display_detailed_stats,
                             hbool_t do_inserts,
                             hbool_t dirty_inserts,
                             hbool_t do_renames,
                             hbool_t rename_to_main_addr,
                             hbool_t do_destroys,
                             hbool_t do_mult_ro_protects,
                             int dirty_destroys,
                             int dirty_unprotects);

void hl_row_major_scan_forward2(H5F_t * file_ptr,
                                int32_t max_index,
                                hbool_t verbose,
                                hbool_t reset_stats,
                                hbool_t display_stats,
                                hbool_t display_detailed_stats,
                                hbool_t do_inserts,
                                hbool_t dirty_inserts);

void row_major_scan_backward2(H5F_t * file_ptr,
                              int32_t max_index,
                              int32_t lag,
                              hbool_t verbose,
                              hbool_t reset_stats,
                              hbool_t display_stats,
                              hbool_t display_detailed_stats,
                              hbool_t do_inserts,
                              hbool_t dirty_inserts,
                              hbool_t do_renames,
                              hbool_t rename_to_main_addr,
                              hbool_t do_destroys,
                              hbool_t do_mult_ro_protects,
                              int dirty_destroys,
                              int dirty_unprotects);

void hl_row_major_scan_backward2(H5F_t * file_ptr,
                                 int32_t max_index,
                                 hbool_t verbose,
                                 hbool_t reset_stats,
                                 hbool_t display_stats,
                                 hbool_t display_detailed_stats,
                                 hbool_t do_inserts,
                                 hbool_t dirty_inserts);

void col_major_scan_forward2(H5F_t * file_ptr,
                             int32_t max_index,
                             int32_t lag,
                             hbool_t verbose,
                             hbool_t reset_stats,
                             hbool_t display_stats,
                             hbool_t display_detailed_stats,
                             hbool_t do_inserts,
                             hbool_t dirty_inserts,
                             int dirty_unprotects);

void hl_col_major_scan_forward2(H5F_t * file_ptr,
                                int32_t max_index,
                                hbool_t verbose,
                                hbool_t reset_stats,
                                hbool_t display_stats,
                                hbool_t display_detailed_stats,
                                hbool_t do_inserts,
                                hbool_t dirty_inserts,
                                int dirty_unprotects);

void col_major_scan_backward2(H5F_t * file_ptr,
                              int32_t max_index,
                              int32_t lag,
                              hbool_t verbose,
                              hbool_t reset_stats,
                              hbool_t display_stats,
                              hbool_t display_detailed_stats,
                              hbool_t do_inserts,
                              hbool_t dirty_inserts,
                              int dirty_unprotects);

void hl_col_major_scan_backward2(H5F_t * file_ptr,
                                 int32_t max_index,
                                 hbool_t verbose,
                                 hbool_t reset_stats,
                                 hbool_t display_stats,
                                 hbool_t display_detailed_stats,
                                 hbool_t do_inserts,
                                 hbool_t dirty_inserts,
                                 int dirty_unprotects);

void takedown_cache2(H5F_t * file_ptr,
                     hbool_t dump_stats,
                     hbool_t dump_detailed_stats);

void flush_cache2(H5F_t * file_ptr,
                  hbool_t destroy_entries,
                  hbool_t dump_stats,
                  hbool_t dump_detailed_stats);

void unpin_entry2(H5F_t * file_ptr,
                  int32_t type,
                  int32_t idx);

void unprotect_entry2(H5F_t * file_ptr,
                      int32_t type,
                      int32_t idx,
                      int dirty,
                      unsigned int flags);

void unprotect_entry_with_size_change2(H5F_t * file_ptr,
                                       int32_t type,
                                       int32_t idx,
                                       unsigned int flags,
                                       size_t new_size);

void verify_clean2(void);

void verify_entry_status2(H5C2_t * cache_ptr,
		          int tag,
                          int num_entries,
                          struct expected_entry_status expected[]);

void verify_unprotected2(void);

