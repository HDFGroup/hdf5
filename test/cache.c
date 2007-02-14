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
 *              6/9/04
 *
 *		This file contains tests for the cache implemented in
 *		H5C.c
 */
#include "h5test.h"
#include "H5Iprivate.h"

const char *FILENAME[] = {
    "cache",
    NULL
};

#include "H5SLprivate.h"
#include "H5Cprivate.h"

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

#define NUMBER_OF_ENTRY_TYPES	9

#define PICO_ENTRY_SIZE		(size_t)1
#define NANO_ENTRY_SIZE		(size_t)4
#define MICRO_ENTRY_SIZE	(size_t)16
#define TINY_ENTRY_SIZE		(size_t)64
#define SMALL_ENTRY_SIZE	(size_t)256
#define MEDIUM_ENTRY_SIZE	(size_t)1024
#define LARGE_ENTRY_SIZE	(size_t)(4 * 1024)
#define HUGE_ENTRY_SIZE		(size_t)(16 * 1024)
#define MONSTER_ENTRY_SIZE	(size_t)(64 * 1024)

#define NUM_PICO_ENTRIES	(10 * 1024)
#define NUM_NANO_ENTRIES	(10 * 1024)
#define NUM_MICRO_ENTRIES	(10 * 1024)
#define NUM_TINY_ENTRIES	(10 * 1024)
#define NUM_SMALL_ENTRIES	(10 * 1024)
#define NUM_MEDIUM_ENTRIES	(10 * 1024)
#define NUM_LARGE_ENTRIES	(10 * 1024)
#define NUM_HUGE_ENTRIES	(10 * 1024)
#define NUM_MONSTER_ENTRIES	(10 * 1024)

#define MAX_ENTRIES		(10 * 1024)

#define PICO_BASE_ADDR		(haddr_t)0
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

#define PICO_ALT_BASE_ADDR	(haddr_t)(MONSTER_BASE_ADDR + \
				     (MONSTER_ENTRY_SIZE * NUM_MONSTER_ENTRIES))
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

typedef struct test_entry_t
{
    H5C_cache_entry_t	  header;	/* entry data used by the cache
					 * -- must be first
                               		 */
    struct test_entry_t * self; 	/* pointer to this entry -- used for
					 * sanity checking.
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
    int32_t		  reads;	/* number of times this entry has
					 * been loaded.
                                         */
    int32_t		  writes;	/* number of times this entry has
                                         * been written
                                         */
    hbool_t		  is_dirty;	/* entry has been modified since
                                         * last write
                                         */
    hbool_t		  is_protected;	/* entry should currently be on
					 * the cache's protected list.
                                         */
} test_entry_t;

/* The following is a cut down copy of the hash table manipulation
 * macros from H5C.c, which have been further modified to avoid references
 * to the error reporting macros.  Needless to say, these macros must be
 * updated as necessary.
 */

#define H5C__HASH_TABLE_LEN     (32 * 1024) /* must be a power of 2 */
#define H5C__HASH_MASK          ((size_t)(H5C__HASH_TABLE_LEN - 1) << 3)
#define H5C__HASH_FCN(x)        (int)(((x) & H5C__HASH_MASK) >> 3)

#define H5C__PRE_HT_SEARCH_SC(cache_ptr, Addr)          \
if ( ( (cache_ptr) == NULL ) ||                         \
     ( (cache_ptr)->magic != H5C__H5C_T_MAGIC ) ||      \
     ( ! H5F_addr_defined(Addr) ) ||                    \
     ( H5C__HASH_FCN(Addr) < 0 ) ||                     \
     ( H5C__HASH_FCN(Addr) >= H5C__HASH_TABLE_LEN ) ) { \
    HDfprintf(stdout, "Pre HT search SC failed.\n");    \
}

#define H5C__POST_SUC_HT_SEARCH_SC(cache_ptr, entry_ptr, Addr, k) \
if ( ( (cache_ptr) == NULL ) ||                                   \
     ( (cache_ptr)->magic != H5C__H5C_T_MAGIC ) ||                \
     ( (cache_ptr)->index_len < 1 ) ||                            \
     ( (entry_ptr) == NULL ) ||                                   \
     ( (cache_ptr)->index_size < (entry_ptr)->size ) ||           \
     ( H5F_addr_ne((entry_ptr)->addr, (Addr)) ) ||                \
     ( (entry_ptr)->size <= 0 ) ||                                \
     ( ((cache_ptr)->index)[k] == NULL ) ||                       \
     ( ( ((cache_ptr)->index)[k] != (entry_ptr) ) &&              \
       ( (entry_ptr)->ht_prev == NULL ) ) ||                      \
     ( ( ((cache_ptr)->index)[k] == (entry_ptr) ) &&              \
       ( (entry_ptr)->ht_prev != NULL ) ) ||                      \
     ( ( (entry_ptr)->ht_prev != NULL ) &&                        \
       ( (entry_ptr)->ht_prev->ht_next != (entry_ptr) ) ) ||      \
     ( ( (entry_ptr)->ht_next != NULL ) &&                        \
       ( (entry_ptr)->ht_next->ht_prev != (entry_ptr) ) ) ) {     \
    HDfprintf(stdout, "Post successful HT search SC failed.\n");  \
}


#define H5C__SEARCH_INDEX(cache_ptr, Addr, entry_ptr)                   \
{                                                                       \
    int k;                                                              \
    int depth = 0;                                                      \
    H5C__PRE_HT_SEARCH_SC(cache_ptr, Addr)                              \
    k = H5C__HASH_FCN(Addr);                                            \
    entry_ptr = ((cache_ptr)->index)[k];                                \
    while ( ( entry_ptr ) && ( H5F_addr_ne(Addr, (entry_ptr)->addr) ) ) \
    {                                                                   \
        (entry_ptr) = (entry_ptr)->ht_next;                             \
        (depth)++;                                                      \
    }                                                                   \
    if ( entry_ptr )                                                    \
    {                                                                   \
        H5C__POST_SUC_HT_SEARCH_SC(cache_ptr, entry_ptr, Addr, k)       \
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


/* The following is a local copy of the H5C_t structure -- any changes in
 * that structure must be reproduced here.  The typedef is used to allow
 * local access to the cache's private data.
 */

#define H5C__H5C_T_MAGIC                0x005CAC0E
#define H5C__MAX_NUM_TYPE_IDS           9

typedef struct local_H5C_t
{
    uint32_t                    magic;

    int32_t                     max_type_id;
    const char *                (* type_name_table_ptr);

    size_t                      max_cache_size;
    size_t                      min_clean_size;

    H5C_write_permitted_func_t  check_write_permitted;

    int32_t                     index_len;
    size_t                      index_size;
    H5C_cache_entry_t *         (index[H5C__HASH_TABLE_LEN]);


    int32_t                     slist_len;
    size_t                      slist_size;
    H5SL_t *                    slist_ptr;

    int32_t                     pl_len;
    size_t                      pl_size;
    H5C_cache_entry_t *         pl_head_ptr;
    H5C_cache_entry_t *         pl_tail_ptr;

    int32_t                     LRU_list_len;
    size_t                      LRU_list_size;
    H5C_cache_entry_t *         LRU_head_ptr;
    H5C_cache_entry_t *         LRU_tail_ptr;

    int32_t                     cLRU_list_len;
    size_t                      cLRU_list_size;
    H5C_cache_entry_t *         cLRU_head_ptr;
    H5C_cache_entry_t *         cLRU_tail_ptr;

    int32_t                     dLRU_list_len;
    size_t                      dLRU_list_size;
    H5C_cache_entry_t *         dLRU_head_ptr;
    H5C_cache_entry_t *         dLRU_tail_ptr;

#if H5C_COLLECT_CACHE_STATS

    /* stats fields */
    int64_t                     hits[H5C__MAX_NUM_TYPE_IDS];
    int64_t                     misses[H5C__MAX_NUM_TYPE_IDS];
    int64_t                     insertions[H5C__MAX_NUM_TYPE_IDS];
    int64_t                     clears[H5C__MAX_NUM_TYPE_IDS];
    int64_t                     flushes[H5C__MAX_NUM_TYPE_IDS];
    int64_t                     evictions[H5C__MAX_NUM_TYPE_IDS];
    int64_t                     renames[H5C__MAX_NUM_TYPE_IDS];

    int64_t                     total_ht_insertions;
    int64_t                     total_ht_deletions;
    int64_t                     successful_ht_searches;
    int64_t                     total_successful_ht_search_depth;
    int64_t                     failed_ht_searches;
    int64_t                     total_failed_ht_search_depth;

    int32_t                     max_index_len;
    size_t                      max_index_size;

    int32_t                     max_tree_len;
    size_t                      max_tree_size;

    int32_t                     max_pl_len;
    size_t                      max_pl_size;

#if H5C_COLLECT_CACHE_ENTRY_STATS

    int32_t                     max_accesses[H5C__MAX_NUM_TYPE_IDS];
    int32_t                     min_accesses[H5C__MAX_NUM_TYPE_IDS];
    int32_t                     max_clears[H5C__MAX_NUM_TYPE_IDS];
    int32_t                     max_flushes[H5C__MAX_NUM_TYPE_IDS];
    size_t                      max_size[H5C__MAX_NUM_TYPE_IDS];

#endif /* H5C_COLLECT_CACHE_ENTRY_STATS */

#endif /* H5C_COLLECT_CACHE_STATS */

    hbool_t                     skip_file_checks;
    hbool_t                     skip_dxpl_id_checks;

} local_H5C_t;


/* global variable declarations: */

static hbool_t write_permitted = TRUE;
static hbool_t pass = TRUE; /* set to false on error */
const char *failure_mssg = NULL;

test_entry_t pico_entries[NUM_PICO_ENTRIES];
test_entry_t nano_entries[NUM_NANO_ENTRIES];
test_entry_t micro_entries[NUM_MICRO_ENTRIES];
test_entry_t tiny_entries[NUM_TINY_ENTRIES];
test_entry_t small_entries[NUM_SMALL_ENTRIES];
test_entry_t medium_entries[NUM_MEDIUM_ENTRIES];
test_entry_t large_entries[NUM_LARGE_ENTRIES];
test_entry_t huge_entries[NUM_HUGE_ENTRIES];
test_entry_t monster_entries[NUM_MONSTER_ENTRIES];

test_entry_t * entries[NUMBER_OF_ENTRY_TYPES] =
{
    pico_entries,
    nano_entries,
    micro_entries,
    tiny_entries,
    small_entries,
    medium_entries,
    large_entries,
    huge_entries,
    monster_entries
};

const int32_t max_indices[NUMBER_OF_ENTRY_TYPES] =
{
    NUM_PICO_ENTRIES - 1,
    NUM_NANO_ENTRIES - 1,
    NUM_MICRO_ENTRIES - 1,
    NUM_TINY_ENTRIES - 1,
    NUM_SMALL_ENTRIES - 1,
    NUM_MEDIUM_ENTRIES - 1,
    NUM_LARGE_ENTRIES - 1,
    NUM_HUGE_ENTRIES - 1,
    NUM_MONSTER_ENTRIES - 1
};

const size_t entry_sizes[NUMBER_OF_ENTRY_TYPES] =
{
    PICO_ENTRY_SIZE,
    NANO_ENTRY_SIZE,
    MICRO_ENTRY_SIZE,
    TINY_ENTRY_SIZE,
    SMALL_ENTRY_SIZE,
    MEDIUM_ENTRY_SIZE,
    LARGE_ENTRY_SIZE,
    HUGE_ENTRY_SIZE,
    MONSTER_ENTRY_SIZE
};

const haddr_t base_addrs[NUMBER_OF_ENTRY_TYPES] =
{
    PICO_BASE_ADDR,
    NANO_BASE_ADDR,
    MICRO_BASE_ADDR,
    TINY_BASE_ADDR,
    SMALL_BASE_ADDR,
    MEDIUM_BASE_ADDR,
    LARGE_BASE_ADDR,
    HUGE_BASE_ADDR,
    MONSTER_BASE_ADDR
};

const haddr_t alt_base_addrs[NUMBER_OF_ENTRY_TYPES] =
{
    PICO_ALT_BASE_ADDR,
    NANO_ALT_BASE_ADDR,
    MICRO_ALT_BASE_ADDR,
    TINY_ALT_BASE_ADDR,
    SMALL_ALT_BASE_ADDR,
    MEDIUM_ALT_BASE_ADDR,
    LARGE_ALT_BASE_ADDR,
    HUGE_ALT_BASE_ADDR,
    MONSTER_ALT_BASE_ADDR
};

const char * entry_type_names[NUMBER_OF_ENTRY_TYPES] =
{
    "pico entries -- 1 B",
    "nano entries -- 4 B",
    "micro entries -- 16 B",
    "tiny entries -- 64 B",
    "small entries -- 256 B",
    "medium entries -- 1 KB",
    "large entries -- 4 KB",
    "huge entries -- 16 KB",
    "monster entries -- 64 KB"
};


/* call back function declarations: */

static herr_t check_write_permitted(const H5F_t UNUSED * f,
                                    hid_t UNUSED dxpl_id,
                                    hbool_t * write_permitted_ptr);

static herr_t clear(H5F_t * f, void * thing, hbool_t dest);

herr_t pico_clear(H5F_t * f, void *  thing, hbool_t dest);
herr_t nano_clear(H5F_t * f, void *  thing, hbool_t dest);
herr_t micro_clear(H5F_t * f, void *  thing, hbool_t dest);
herr_t tiny_clear(H5F_t * f, void *  thing, hbool_t dest);
herr_t small_clear(H5F_t * f, void *  thing, hbool_t dest);
herr_t medium_clear(H5F_t * f, void *  thing, hbool_t dest);
herr_t large_clear(H5F_t * f, void *  thing, hbool_t dest);
herr_t huge_clear(H5F_t * f, void *  thing, hbool_t dest);
herr_t monster_clear(H5F_t * f, void *  thing, hbool_t dest);


static herr_t destroy(H5F_t UNUSED * f, void * thing);

herr_t pico_dest(H5F_t * f, void * thing);
herr_t nano_dest(H5F_t * f, void * thing);
herr_t micro_dest(H5F_t * f, void * thing);
herr_t tiny_dest(H5F_t * f, void * thing);
herr_t small_dest(H5F_t * f, void * thing);
herr_t medium_dest(H5F_t * f, void * thing);
herr_t large_dest(H5F_t * f, void * thing);
herr_t huge_dest(H5F_t * f, void *  thing);
herr_t monster_dest(H5F_t * f, void *  thing);


static herr_t flush(H5F_t *f, hid_t UNUSED dxpl_id, hbool_t dest,
                    haddr_t addr, void *thing);

herr_t pico_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest,
                  haddr_t addr, void *thing);
herr_t nano_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest,
                  haddr_t addr, void *thing);
herr_t micro_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest,
                   haddr_t addr, void *thing);
herr_t tiny_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest,
                  haddr_t addr, void *thing);
herr_t small_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest,
                   haddr_t addr, void *thing);
herr_t medium_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest,
                    haddr_t addr, void *thing);
herr_t large_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest,
                   haddr_t addr, void *thing);
herr_t huge_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest,
                  haddr_t addr, void *thing);
herr_t monster_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest,
                     haddr_t addr, void *thing);


static void * load(H5F_t UNUSED *f, hid_t UNUSED dxpl_id, haddr_t addr,
                   const void UNUSED *udata1, void UNUSED *udata2);

void * pico_load(H5F_t *f, hid_t dxpl_id, haddr_t addr,
                 const void *udata1, void *udata2);
void * nano_load(H5F_t *f, hid_t dxpl_id, haddr_t addr,
                 const void *udata1, void *udata2);
void * micro_load(H5F_t *f, hid_t dxpl_id, haddr_t addr,
                  const void *udata1, void *udata2);
void * tiny_load(H5F_t *f, hid_t dxpl_id, haddr_t addr,
                 const void *udata1, void *udata2);
void * small_load(H5F_t *f, hid_t dxpl_id, haddr_t addr,
                  const void *udata1, void *udata2);
void * medium_load(H5F_t *f, hid_t dxpl_id, haddr_t addr,
                   const void *udata1, void *udata2);
void * large_load(H5F_t *f, hid_t dxpl_id, haddr_t addr,
                  const void *udata1, void *udata2);
void * huge_load(H5F_t *f, hid_t dxpl_id, haddr_t addr,
                 const void *udata1, void *udata2);
void * monster_load(H5F_t *f, hid_t dxpl_id, haddr_t addr,
                    const void *udata1, void *udata2);


static herr_t size(H5F_t UNUSED * f, void * thing, size_t * size_ptr);

herr_t pico_size(H5F_t * f, void * thing, size_t * size_ptr);
herr_t nano_size(H5F_t * f, void * thing, size_t * size_ptr);
herr_t micro_size(H5F_t * f, void * thing, size_t * size_ptr);
herr_t tiny_size(H5F_t * f, void * thing, size_t * size_ptr);
herr_t small_size(H5F_t * f, void * thing, size_t * size_ptr);
herr_t medium_size(H5F_t * f, void * thing, size_t * size_ptr);
herr_t large_size(H5F_t * f, void * thing, size_t * size_ptr);
herr_t huge_size(H5F_t * f, void * thing, size_t * size_ptr);
herr_t monster_size(H5F_t * f, void * thing, size_t * size_ptr);


/* callback table declaration */

static const H5C_class_t types[NUMBER_OF_ENTRY_TYPES] =
{
  {
    PICO_ENTRY_TYPE,
    (H5C_load_func_t)pico_load,
    (H5C_flush_func_t)pico_flush,
    (H5C_dest_func_t)pico_dest,
    (H5C_clear_func_t)pico_clear,
    (H5C_size_func_t)pico_size
  },
  {
    NANO_ENTRY_TYPE,
    (H5C_load_func_t)nano_load,
    (H5C_flush_func_t)nano_flush,
    (H5C_dest_func_t)nano_dest,
    (H5C_clear_func_t)nano_clear,
    (H5C_size_func_t)nano_size
  },
  {
    MICRO_ENTRY_TYPE,
    (H5C_load_func_t)micro_load,
    (H5C_flush_func_t)micro_flush,
    (H5C_dest_func_t)micro_dest,
    (H5C_clear_func_t)micro_clear,
    (H5C_size_func_t)micro_size
  },
  {
    TINY_ENTRY_TYPE,
    (H5C_load_func_t)tiny_load,
    (H5C_flush_func_t)tiny_flush,
    (H5C_dest_func_t)tiny_dest,
    (H5C_clear_func_t)tiny_clear,
    (H5C_size_func_t)tiny_size
  },
  {
    SMALL_ENTRY_TYPE,
    (H5C_load_func_t)small_load,
    (H5C_flush_func_t)small_flush,
    (H5C_dest_func_t)small_dest,
    (H5C_clear_func_t)small_clear,
    (H5C_size_func_t)small_size
  },
  {
    MEDIUM_ENTRY_TYPE,
    (H5C_load_func_t)medium_load,
    (H5C_flush_func_t)medium_flush,
    (H5C_dest_func_t)medium_dest,
    (H5C_clear_func_t)medium_clear,
    (H5C_size_func_t)medium_size
  },
  {
    LARGE_ENTRY_TYPE,
    (H5C_load_func_t)large_load,
    (H5C_flush_func_t)large_flush,
    (H5C_dest_func_t)large_dest,
    (H5C_clear_func_t)large_clear,
    (H5C_size_func_t)large_size
  },
  {
    HUGE_ENTRY_TYPE,
    (H5C_load_func_t)huge_load,
    (H5C_flush_func_t)huge_flush,
    (H5C_dest_func_t)huge_dest,
    (H5C_clear_func_t)huge_clear,
    (H5C_size_func_t)huge_size
  },
  {
    MONSTER_ENTRY_TYPE,
    (H5C_load_func_t)monster_load,
    (H5C_flush_func_t)monster_flush,
    (H5C_dest_func_t)monster_dest,
    (H5C_clear_func_t)monster_clear,
    (H5C_size_func_t)monster_size
  }
};


/* private function declarations: */

static void addr_to_type_and_index(haddr_t addr,
                                   int32_t * type_ptr,
                                   int32_t * index_ptr);

#if 0 /* keep this for a while -- it may be useful */
static haddr_t type_and_index_to_addr(int32_t type,
                                      int32_t idx);
#endif

static void insert_entry(H5C_t * cache_ptr,
                         int32_t type,
                         int32_t idx,
                         hbool_t dirty);

static void rename_entry(H5C_t * cache_ptr,
                         int32_t type,
                         int32_t idx,
                         hbool_t main_addr);

static void protect_entry(H5C_t * cache_ptr,
                          int32_t type,
                          int32_t idx);

hbool_t entry_in_cache(H5C_t * cache_ptr,
                       int32_t type,
                       int32_t idx);

static void reset_entries(void);

static H5C_t * setup_cache(size_t max_cache_size, size_t min_clean_size);

static void row_major_scan_forward(H5C_t * cache_ptr,
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
                                   int dirty_destroys,
                                   int dirty_unprotects);

static void row_major_scan_backward(H5C_t * cache_ptr,
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
                                    int dirty_destroys,
                                    int dirty_unprotects);

static void col_major_scan_forward(H5C_t * cache_ptr,
                                   int32_t lag,
                                   hbool_t verbose,
                                   hbool_t reset_stats,
                                   hbool_t display_stats,
                                   hbool_t display_detailed_stats,
                                   hbool_t do_inserts,
                                   hbool_t dirty_inserts,
                                   int dirty_unprotects);

static void col_major_scan_backward(H5C_t * cache_ptr,
                                    int32_t lag,
                                    hbool_t verbose,
                                    hbool_t reset_stats,
                                    hbool_t display_stats,
                                    hbool_t display_detailed_stats,
                                    hbool_t do_inserts,
                                    hbool_t dirty_inserts,
                                    int dirty_unprotects);

static void smoke_check_1(void);
static void smoke_check_2(void);
static void smoke_check_3(void);
static void smoke_check_4(void);
static void write_permitted_check(void);
static void check_flush_protected_err(void);
static void check_destroy_protected_err(void);
static void check_duplicate_insert_err(void);
static void check_rename_err(void);
static void check_double_protect_err(void);
static void check_double_unprotect_err(void);

static void takedown_cache(H5C_t * cache_ptr,
                           hbool_t dump_stats,
                           hbool_t dump_detailed_stats);

static void flush_cache(H5C_t * cache_ptr,
                        hbool_t destroy_entries,
                        hbool_t dump_stats,
                        hbool_t dump_detailed_stats);

static void unprotect_entry(H5C_t * cache_ptr,
                            int32_t type,
                            int32_t idx,
                            int dirty,
                            hbool_t deleted);

static void verify_clean(void);

static void verify_unprotected(void);



/* address translation funtions: */

/*-------------------------------------------------------------------------
 * Function:	addr_to_type_and_index
 *
 * Purpose:	Given an address, compute the type and index of the
 *		associated entry.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/10/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static void
addr_to_type_and_index(haddr_t addr,
                       int32_t * type_ptr,
                       int32_t * index_ptr)
{
    int i;
    int32_t type;
    int32_t idx;

    HDassert( type_ptr );
    HDassert( index_ptr );

    /* we only have a small number of entry types, so just do a
     * linear search.  If NUMBER_OF_ENTRY_TYPES grows, we may want
     * to do a binary search instead.
     */
    i = 1;
    if ( addr >= PICO_ALT_BASE_ADDR ) {

        while ( ( i < NUMBER_OF_ENTRY_TYPES ) &&
                ( addr >= alt_base_addrs[i] ) )
        {
            i++;
        }

    } else {

        while ( ( i < NUMBER_OF_ENTRY_TYPES ) &&
                ( addr >= base_addrs[i] ) )
        {
            i++;
        }
    }

    type = i - 1;

    HDassert( ( type >= 0 ) && ( type < NUMBER_OF_ENTRY_TYPES ) );

    if ( addr >= PICO_ALT_BASE_ADDR ) {

        idx = (addr - alt_base_addrs[type]) / entry_sizes[type];
        HDassert( !((entries[type])[idx].at_main_addr) );
        HDassert( addr == (entries[type])[idx].alt_addr );

    } else {

        idx = (addr - base_addrs[type]) / entry_sizes[type];
        HDassert( (entries[type])[idx].at_main_addr );
        HDassert( addr == (entries[type])[idx].main_addr );
    }

    HDassert( ( idx >= 0 ) && ( idx <= max_indices[type] ) );

    HDassert( addr == (entries[type])[idx].addr );

    *type_ptr = type;
    *index_ptr = idx;

    return;

} /* addr_to_type_and_index() */


#if 0 /* This function has never been used, but we may want it
       * some time.  Lets keep it for now.
       */
/*-------------------------------------------------------------------------
 * Function:	type_and_index_to_addr
 *
 * Purpose:	Given a type and index of an entry, compute the associated
 *		addr and return that value.
 *
 * Return:	computed addr
 *
 * Programmer:	John Mainzer
 *              6/10/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static haddr_t
type_and_index_to_addr(int32_t type,
                       int32_t idx)
{
    haddr_t addr;

    HDassert( ( type >= 0 ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
    HDassert( ( idx >= 0 ) && ( idx <= max_indices[type] ) );

    addr = base_addrs[type] + (((haddr_t)idx) * entry_sizes[type]);

    HDassert( addr == (entries[type])[idx].addr );

    if ( (entries[type])[idx].at_main_addr ) {

        HDassert( addr == (entries[type])[idx].main_addr );

    } else {

        HDassert( addr == (entries[type])[idx].alt_addr );
    }

    return(addr);

} /* type_and_index_to_addr() */

#endif


/* Call back functions: */

/*-------------------------------------------------------------------------
 *
 * Function:    H5AC_check_if_write_permitted
 *
 * Purpose:     Determine if a write is permitted under the current
 *              circumstances, and set *write_permitted_ptr accordingly.
 *              As a general rule it is, but when we are running in parallel
 *              mode with collective I/O, we must ensure that a read cannot
 *              cause a write.
 *
 *              In the event of failure, the value of *write_permitted_ptr
 *              is undefined.
 *
 * Return:      Non-negative on success/Negative on failure.
 *
 * Programmer:  John Mainzer, 5/15/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static herr_t
check_write_permitted(const H5F_t UNUSED * f,
                      hid_t UNUSED dxpl_id,
                      hbool_t * write_permitted_ptr)
{

    HDassert( write_permitted_ptr );
    *write_permitted_ptr = write_permitted;

    return(SUCCEED);

} /* check_write_permitted() */


/*-------------------------------------------------------------------------
 * Function:	clear & friends
 *
 * Purpose:	clear the entry.  The helper functions verify that the
 *		correct version of clear is being called, and then call
 *		clear proper.
 *
 * Return:	SUCCEED
 *
 * Programmer:	John Mainzer
 *              6/10/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static herr_t
clear(H5F_t * f,
      void *  thing,
      hbool_t dest)
{
    test_entry_t * entry_ptr;
    test_entry_t * base_addr;

    HDassert( thing );

    entry_ptr = (test_entry_t *)thing;
    base_addr = entries[entry_ptr->type];

    HDassert( entry_ptr->index >= 0 );
    HDassert( entry_ptr->index <= max_indices[entry_ptr->type] );
    HDassert( entry_ptr == &(base_addr[entry_ptr->index]) );
    HDassert( entry_ptr == entry_ptr->self );
    HDassert( entry_ptr->header.addr == entry_ptr->addr );
    HDassert( entry_ptr->header.size == entry_ptr->size );
    HDassert( entry_ptr->size == entry_sizes[entry_ptr->type] );

    entry_ptr->header.is_dirty = FALSE;
    entry_ptr->is_dirty = FALSE;

    if ( dest ) {

        destroy(f, thing);

    }

    return(SUCCEED);

} /* clear() */

herr_t
pico_clear(H5F_t * f, void *  thing, hbool_t dest)
{
    HDassert ( ((test_entry_t *)thing)->type == PICO_ENTRY_TYPE );
    return(clear(f, thing, dest));
}

herr_t
nano_clear(H5F_t * f, void *  thing, hbool_t dest)
{
    HDassert ( ((test_entry_t *)thing)->type == NANO_ENTRY_TYPE );
    return(clear(f, thing, dest));
}

herr_t
micro_clear(H5F_t * f, void *  thing, hbool_t dest)
{
    HDassert ( ((test_entry_t *)thing)->type == MICRO_ENTRY_TYPE );
    return(clear(f, thing, dest));
}

herr_t
tiny_clear(H5F_t * f, void *  thing, hbool_t dest)
{
    HDassert ( ((test_entry_t *)thing)->type == TINY_ENTRY_TYPE );
    return(clear(f, thing, dest));
}

herr_t
small_clear(H5F_t * f, void *  thing, hbool_t dest)
{
    HDassert ( ((test_entry_t *)thing)->type == SMALL_ENTRY_TYPE );
    return(clear(f, thing, dest));
}

herr_t
medium_clear(H5F_t * f, void *  thing, hbool_t dest)
{
    HDassert ( ((test_entry_t *)thing)->type == MEDIUM_ENTRY_TYPE );
    return(clear(f, thing, dest));
}

herr_t
large_clear(H5F_t * f, void *  thing, hbool_t dest)
{
    HDassert ( ((test_entry_t *)thing)->type == LARGE_ENTRY_TYPE );
    return(clear(f, thing, dest));
}

herr_t
huge_clear(H5F_t * f, void *  thing, hbool_t dest)
{
    HDassert ( ((test_entry_t *)thing)->type == HUGE_ENTRY_TYPE );
    return(clear(f, thing, dest));
}

herr_t
monster_clear(H5F_t * f, void *  thing, hbool_t dest)
{
    HDassert ( ((test_entry_t *)thing)->type == MONSTER_ENTRY_TYPE );
    return(clear(f, thing, dest));
}


/*-------------------------------------------------------------------------
 * Function:	dest & friends
 *
 * Purpose:	Destroy the entry.  The helper functions verify that the
 *		correct version of dest is being called, and then call
 *		dest proper.
 *
 * Return:	SUCCEED
 *
 * Programmer:	John Mainzer
 *              6/10/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static herr_t
destroy(H5F_t UNUSED * f,
        void *         thing)
{
    test_entry_t * entry_ptr;
    test_entry_t * base_addr;

    HDassert( thing );

    entry_ptr = (test_entry_t *)thing;
    base_addr = entries[entry_ptr->type];

    HDassert ( entry_ptr->index >= 0 );
    HDassert ( entry_ptr->index <= max_indices[entry_ptr->type] );
    HDassert( entry_ptr == &(base_addr[entry_ptr->index]) );
    HDassert( entry_ptr == entry_ptr->self );
    HDassert( entry_ptr->header.addr == entry_ptr->addr );
    HDassert( entry_ptr->header.size == entry_ptr->size );
    HDassert( entry_ptr->size == entry_sizes[entry_ptr->type] );

    HDassert( !(entry_ptr->is_dirty) );
    HDassert( !(entry_ptr->header.is_dirty) );

    return(SUCCEED);

} /* dest() */

herr_t
pico_dest(H5F_t * f, void * thing)
{
    HDassert ( ((test_entry_t *)thing)->type == PICO_ENTRY_TYPE );
    return(destroy(f, thing));
}

herr_t
nano_dest(H5F_t * f, void * thing)
{
    HDassert ( ((test_entry_t *)thing)->type == NANO_ENTRY_TYPE );
    return(destroy(f, thing));
}

herr_t
micro_dest(H5F_t * f, void * thing)
{
    HDassert ( ((test_entry_t *)thing)->type == MICRO_ENTRY_TYPE );
    return(destroy(f, thing));
}

herr_t
tiny_dest(H5F_t * f, void * thing)
{
    HDassert ( ((test_entry_t *)thing)->type == TINY_ENTRY_TYPE );
    return(destroy(f, thing));
}

herr_t
small_dest(H5F_t * f, void * thing)
{
    HDassert ( ((test_entry_t *)thing)->type == SMALL_ENTRY_TYPE );
    return(destroy(f, thing));
}

herr_t
medium_dest(H5F_t * f, void * thing)
{
    HDassert ( ((test_entry_t *)thing)->type == MEDIUM_ENTRY_TYPE );
    return(destroy(f, thing));
}

herr_t
large_dest(H5F_t * f, void * thing)
{
    HDassert ( ((test_entry_t *)thing)->type == LARGE_ENTRY_TYPE );
    return(destroy(f, thing));
}

herr_t
huge_dest(H5F_t * f, void *  thing)
{
    HDassert ( ((test_entry_t *)thing)->type == HUGE_ENTRY_TYPE );
    return(destroy(f, thing));
}

herr_t
monster_dest(H5F_t * f, void *  thing)
{
    HDassert ( ((test_entry_t *)thing)->type == MONSTER_ENTRY_TYPE );
    return(destroy(f, thing));
}


/*-------------------------------------------------------------------------
 * Function:	flush & friends
 *
 * Purpose:	flush the entry and mark it as clean.  The helper functions
 *              verify that the correct version of flush is being called,
 *		and then call flush proper.
 *
 * Return:	SUCCEED
 *
 * Programmer:	John Mainzer
 *              6/10/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static herr_t
flush(H5F_t *f,
      hid_t UNUSED dxpl_id,
      hbool_t dest,
      haddr_t addr,
      void *thing)
{
    test_entry_t * entry_ptr;
    test_entry_t * base_addr;

    HDassert( thing );

    entry_ptr = (test_entry_t *)thing;
    base_addr = entries[entry_ptr->type];

    HDassert( entry_ptr->index >= 0 );
    HDassert( entry_ptr->index <= max_indices[entry_ptr->type] );
    HDassert( entry_ptr == &(base_addr[entry_ptr->index]) );
    HDassert( entry_ptr == entry_ptr->self );
    HDassert( entry_ptr->header.addr == entry_ptr->addr );
    HDassert( entry_ptr->addr == addr );
    HDassert( entry_ptr->header.size == entry_ptr->size );
    HDassert( entry_ptr->size == entry_sizes[entry_ptr->type] );

    if ( ( ! write_permitted ) && ( entry_ptr->is_dirty ) ) {

        pass = FALSE;
        failure_mssg = "called flush when write_permitted is FALSE.";
    }

    if ( entry_ptr->is_dirty ) {

        (entry_ptr->writes)++;
        entry_ptr->is_dirty = FALSE;
        entry_ptr->header.is_dirty = FALSE;
    }

    if ( dest ) {

        destroy(f, thing);

    }

    return(SUCCEED);

} /* flush() */

herr_t
pico_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest, haddr_t addr, void *thing)
{
    HDassert ( ((test_entry_t *)thing)->type == PICO_ENTRY_TYPE );
    return(flush(f, dxpl_id, dest, addr, thing));
}

herr_t
nano_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest, haddr_t addr, void *thing)
{
    HDassert ( ((test_entry_t *)thing)->type == NANO_ENTRY_TYPE );
    return(flush(f, dxpl_id, dest, addr, thing));
}

herr_t
micro_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest, haddr_t addr, void *thing)
{
    HDassert ( ((test_entry_t *)thing)->type == MICRO_ENTRY_TYPE );
    return(flush(f, dxpl_id, dest, addr, thing));
}

herr_t
tiny_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest, haddr_t addr, void *thing)
{
    HDassert ( ((test_entry_t *)thing)->type == TINY_ENTRY_TYPE );
    return(flush(f, dxpl_id, dest, addr, thing));
}

herr_t
small_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest, haddr_t addr, void *thing)
{
    HDassert ( ((test_entry_t *)thing)->type == SMALL_ENTRY_TYPE );
    return(flush(f, dxpl_id, dest, addr, thing));
}

herr_t
medium_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest, haddr_t addr, void *thing)
{
    HDassert ( ((test_entry_t *)thing)->type == MEDIUM_ENTRY_TYPE );
    return(flush(f, dxpl_id, dest, addr, thing));
}

herr_t
large_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest, haddr_t addr, void *thing)
{
    HDassert ( ((test_entry_t *)thing)->type == LARGE_ENTRY_TYPE );
    return(flush(f, dxpl_id, dest, addr, thing));
}

herr_t
huge_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest, haddr_t addr, void *thing)
{
    HDassert ( ((test_entry_t *)thing)->type == HUGE_ENTRY_TYPE );
    return(flush(f, dxpl_id, dest, addr, thing));
}

herr_t
monster_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest, haddr_t addr, void *thing)
{
    HDassert ( ((test_entry_t *)thing)->type == MONSTER_ENTRY_TYPE );
    return(flush(f, dxpl_id, dest, addr, thing));
}


/*-------------------------------------------------------------------------
 * Function:	load & friends
 *
 * Purpose:	"load" the requested entry and mark it as clean.  The
 *		helper functions verify that the correct version of load
 *		 is being called, and then call load proper.
 *
 * Return:	SUCCEED
 *
 * Programmer:	John Mainzer
 *              6/10/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void *
load(H5F_t UNUSED *f,
     hid_t UNUSED dxpl_id,
     haddr_t addr,
     const void UNUSED *udata1,
     void UNUSED *udata2)
{
    int32_t type;
    int32_t idx;
    test_entry_t * entry_ptr;
    test_entry_t * base_addr;

    addr_to_type_and_index(addr, &type, &idx);

    base_addr = entries[type];
    entry_ptr = &(base_addr[idx]);

    HDassert( entry_ptr->type == type );
    HDassert( entry_ptr->type >= 0 );
    HDassert( entry_ptr->type < NUMBER_OF_ENTRY_TYPES );
    HDassert( entry_ptr->index == idx );
    HDassert( entry_ptr->index >= 0 );
    HDassert( entry_ptr->index <= max_indices[type] );
    HDassert( entry_ptr == entry_ptr->self );
    HDassert( entry_ptr->addr == addr );
    HDassert( entry_ptr->size == entry_sizes[type] );

    entry_ptr->is_dirty = FALSE;

    (entry_ptr->reads)++;

    return(entry_ptr);

} /* load() */

void *
pico_load(H5F_t *f, hid_t dxpl_id, haddr_t addr,
          const void *udata1, void *udata2)
{
    return(load(f, dxpl_id, addr, udata1, udata2));
}

void *
nano_load(H5F_t *f, hid_t dxpl_id, haddr_t addr,
          const void *udata1, void *udata2)
{
    return(load(f, dxpl_id, addr, udata1, udata2));
}

void *
micro_load(H5F_t *f, hid_t dxpl_id, haddr_t addr,
           const void *udata1, void *udata2)
{
    return(load(f, dxpl_id, addr, udata1, udata2));
}

void *
tiny_load(H5F_t *f, hid_t dxpl_id, haddr_t addr,
          const void *udata1, void *udata2)
{
    return(load(f, dxpl_id, addr, udata1, udata2));
}

void *
small_load(H5F_t *f, hid_t dxpl_id, haddr_t addr,
           const void *udata1, void *udata2)
{
    return(load(f, dxpl_id, addr, udata1, udata2));
}

void *
medium_load(H5F_t *f, hid_t dxpl_id, haddr_t addr,
            const void *udata1, void *udata2)
{
    return(load(f, dxpl_id, addr, udata1, udata2));
}

void *
large_load(H5F_t *f, hid_t dxpl_id, haddr_t addr,
           const void *udata1, void *udata2)
{
    return(load(f, dxpl_id, addr, udata1, udata2));
}

void *
huge_load(H5F_t *f, hid_t dxpl_id, haddr_t addr,
          const void *udata1, void *udata2)
{
    return(load(f, dxpl_id, addr, udata1, udata2));
}

void *
monster_load(H5F_t *f, hid_t dxpl_id, haddr_t addr,
             const void *udata1, void *udata2)
{
    return(load(f, dxpl_id, addr, udata1, udata2));
}


/*-------------------------------------------------------------------------
 * Function:	size & friends
 *
 * Purpose:	Get the size of the specified entry.  The helper functions
 *		verify that the correct version of size is being called,
 *		and then call size proper.
 *
 * Return:	SUCCEED
 *
 * Programmer:	John Mainzer
 *              6/10/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static herr_t
size(H5F_t UNUSED *  f,
     void *   thing,
     size_t * size_ptr)
{
    test_entry_t * entry_ptr;
    test_entry_t * base_addr;

    HDassert( size_ptr );
    HDassert( thing );

    entry_ptr = (test_entry_t *)thing;
    base_addr = entries[entry_ptr->type];

    HDassert( entry_ptr->index >= 0 );
    HDassert( entry_ptr->index <= max_indices[entry_ptr->type] );
    HDassert( entry_ptr == &(base_addr[entry_ptr->index]) );
    HDassert( entry_ptr == entry_ptr->self );
    HDassert( entry_ptr->header.addr == entry_ptr->addr );
    HDassert( entry_ptr->size == entry_sizes[entry_ptr->type] );

    *size_ptr = entry_ptr->size;

    return(SUCCEED);

} /* size() */

herr_t
pico_size(H5F_t * f, void * thing, size_t * size_ptr)
{
    HDassert ( ((test_entry_t *)thing)->type == PICO_ENTRY_TYPE );
    return(size(f, thing, size_ptr));
}

herr_t
nano_size(H5F_t * f, void * thing, size_t * size_ptr)
{
    HDassert ( ((test_entry_t *)thing)->type == NANO_ENTRY_TYPE );
    return(size(f, thing, size_ptr));
}

herr_t
micro_size(H5F_t * f, void * thing, size_t * size_ptr)
{
    HDassert ( ((test_entry_t *)thing)->type == MICRO_ENTRY_TYPE );
    return(size(f, thing, size_ptr));
}

herr_t
tiny_size(H5F_t * f, void * thing, size_t * size_ptr)
{
    HDassert ( ((test_entry_t *)thing)->type == TINY_ENTRY_TYPE );
    return(size(f, thing, size_ptr));
}

herr_t
small_size(H5F_t * f, void * thing, size_t * size_ptr)
{
    HDassert ( ((test_entry_t *)thing)->type == SMALL_ENTRY_TYPE );
    return(size(f, thing, size_ptr));
}

herr_t
medium_size(H5F_t * f, void * thing, size_t * size_ptr)
{
    HDassert ( ((test_entry_t *)thing)->type == MEDIUM_ENTRY_TYPE );
    return(size(f, thing, size_ptr));
}

herr_t
large_size(H5F_t * f, void * thing, size_t * size_ptr)
{
    HDassert ( ((test_entry_t *)thing)->type == LARGE_ENTRY_TYPE );
    return(size(f, thing, size_ptr));
}

herr_t
huge_size(H5F_t * f, void * thing, size_t * size_ptr)
{
    HDassert ( ((test_entry_t *)thing)->type == HUGE_ENTRY_TYPE );
    return(size(f, thing, size_ptr));
}

herr_t
monster_size(H5F_t * f, void * thing, size_t * size_ptr)
{
    HDassert ( ((test_entry_t *)thing)->type == MONSTER_ENTRY_TYPE );
    return(size(f, thing, size_ptr));
}


/**************************************************************************/
/**************************************************************************/
/************************** test utility functions: ***********************/
/**************************************************************************/
/**************************************************************************/

/*-------------------------------------------------------------------------
 * Function:	entry_in_cache
 *
 * Purpose:	Given a pointer to a cache, an entry type, and an index,
 *		determine if the entry is currently in the cache.
 *
 * Return:	TRUE if the entry is in the cache, and FALSE otherwise.
 *
 * Programmer:	John Mainzer
 *              6/10/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

hbool_t
entry_in_cache(H5C_t * cache_ptr,
               int32_t type,
               int32_t idx)
{
    hbool_t in_cache = FALSE; /* will set to TRUE if necessary */
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;
    H5C_cache_entry_t * test_ptr = NULL;

    HDassert( cache_ptr );
    HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
    HDassert( ( 0 <= idx ) && ( idx <= max_indices[type] ) );

    base_addr = entries[type];
    entry_ptr = &(base_addr[idx]);

    HDassert( entry_ptr->index == idx );
    HDassert( entry_ptr->type == type );
    HDassert( entry_ptr == entry_ptr->self );

    H5C__SEARCH_INDEX(((local_H5C_t *)cache_ptr), entry_ptr->addr, test_ptr)

    if ( test_ptr != NULL ) {

        in_cache = TRUE;
        HDassert( test_ptr == (H5C_cache_entry_t *)entry_ptr );
        HDassert( entry_ptr->addr == entry_ptr->header.addr );
    }

    return(in_cache);

} /* entry_in_cache() */


/*-------------------------------------------------------------------------
 * Function:	reset_entries
 *
 * Purpose:	reset the contents of the entries arrays to know values.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/10/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
reset_entries(void)

{
    int i;
    int j;
    int32_t max_index;
    haddr_t addr = 0;
    haddr_t alt_addr = PICO_ALT_BASE_ADDR;
    size_t entry_size;
    test_entry_t * base_addr;

    for ( i = 0; i < NUMBER_OF_ENTRY_TYPES; i++ )
    {
        entry_size = entry_sizes[i];
        max_index = max_indices[i];
        base_addr = entries[i];

        HDassert( base_addr );

        for ( j = 0; j <= max_index; j++ )
        {
            /* one can argue that we should fill the header with garbage.
             * If this is desired, we can simply comment out the header
             * initialization - the headers will be full of garbage soon
             * enough.
             */

            base_addr[j].header.addr = (haddr_t)0;
            base_addr[j].header.size = (size_t)0;
            base_addr[j].header.type = NULL;
            base_addr[j].header.is_dirty = FALSE;
            base_addr[j].header.is_protected = FALSE;
            base_addr[j].header.next = NULL;
            base_addr[j].header.prev = NULL;
            base_addr[j].header.aux_next = NULL;
            base_addr[j].header.aux_prev = NULL;

            base_addr[j].self = &(base_addr[j]);
            base_addr[j].addr = addr;
            base_addr[j].at_main_addr = TRUE;
            base_addr[j].main_addr = addr;
            base_addr[j].alt_addr = alt_addr;
            base_addr[j].size = entry_size;
            base_addr[j].type = i;
            base_addr[j].index = j;
            base_addr[j].reads = 0;
            base_addr[j].writes = 0;
            base_addr[j].is_dirty = FALSE;
            base_addr[j].is_protected = FALSE;

            addr += (haddr_t)entry_size;
            alt_addr += (haddr_t)entry_size;
        }
    }

    return;

} /* reset_entries() */


/*-------------------------------------------------------------------------
 * Function:	verify_clean
 *
 * Purpose:	Verify that all cache entries are marked as clean.  If any
 *		are not, set pass to FALSE.
 *
 *		Do nothing if pass is FALSE on entry.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/10/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
verify_clean(void)

{
    int i;
    int j;
    int dirty_count = 0;
    int32_t max_index;
    test_entry_t * base_addr;

    if ( pass ) {

        for ( i = 0; i < NUMBER_OF_ENTRY_TYPES; i++ )
        {
            max_index = max_indices[i];
            base_addr = entries[i];

            HDassert( base_addr );

            for ( j = 0; j <= max_index; j++ )
            {
                if ( ( base_addr[j].header.is_dirty ) || ( base_addr[j].is_dirty ) ) {

                    dirty_count++;
                }
            }
        }

        if ( dirty_count > 0 ) {

            pass = FALSE;
            failure_mssg = "verify_clean() found dirty entry(s).";
        }
    }

    return;

} /* verify_clean() */


/*-------------------------------------------------------------------------
 * Function:	verify_unprotected
 *
 * Purpose:	Verify that no cache entries are marked as protected.  If
 *		any are, set pass to FALSE.
 *
 *		Do nothing if pass is FALSE on entry.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/10/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
verify_unprotected(void)

{
    int i;
    int j;
    int protected_count = 0;
    int32_t max_index;
    test_entry_t * base_addr;

    if ( pass ) {

        for ( i = 0; i < NUMBER_OF_ENTRY_TYPES; i++ )
        {
            max_index = max_indices[i];
            base_addr = entries[i];

            HDassert( base_addr );

            for ( j = 0; j <= max_index; j++ )
            {
                HDassert( base_addr[j].header.is_protected ==
                          base_addr[j].is_protected );

                if ( ( base_addr[j].header.is_protected ) ||
                     ( base_addr[j].is_protected ) ) {

                    protected_count++;
                }
            }
        }

        if ( protected_count > 0 ) {

            pass = FALSE;
            failure_mssg = "verify_unprotected() found protected entry(s).";
        }
    }

    return;

} /* verify_unprotected() */


/*-------------------------------------------------------------------------
 * Function:	setup_cache()
 *
 * Purpose:	Allocate a cache of the desired size and configure it for
 *		use in the test bed.  Return a pointer to the new cache
 *		structure.
 *
 * Return:	Pointer to new cache, or NULL on failure.
 *
 * Programmer:	John Mainzer
 *              6/11/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static H5C_t *
setup_cache(size_t max_cache_size,
            size_t min_clean_size)
{
    H5C_t * cache_ptr = NULL;

    cache_ptr = H5C_create(max_cache_size,
                           min_clean_size,
                           (NUMBER_OF_ENTRY_TYPES - 1),
			   (const char **)entry_type_names,
                           check_write_permitted);

    if ( cache_ptr == NULL ) {

        pass = FALSE;
        failure_mssg = "H5C_create() returned NULL.";

    } else {

        H5C_set_skip_flags(cache_ptr, TRUE, TRUE);
    }

    return(cache_ptr);

} /* setup_cache() */


/*-------------------------------------------------------------------------
 * Function:	takedown_cache()
 *
 * Purpose:	Flush the specified cache and disable it.  If requested,
 *		dump stats first.  If pass is FALSE, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/11/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
takedown_cache(H5C_t * cache_ptr,
               hbool_t dump_stats,
               hbool_t dump_detailed_stats)
{
    HDassert(cache_ptr);

    if ( pass ) {

        if ( dump_stats ) {

            H5C_stats(cache_ptr, "test cache", dump_detailed_stats);
        }

        H5C_dest(NULL, -1, -1, cache_ptr);
    }

    return;

} /* takedown_cache() */


/*-------------------------------------------------------------------------
 * Function:	flush_cache()
 *
 * Purpose:	Flush the specified cache, destroying all entries if
                requested.  If requested, dump stats first.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/23/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
flush_cache(H5C_t * cache_ptr,
            hbool_t destroy_entries,
            hbool_t dump_stats,
            hbool_t dump_detailed_stats)
{
    herr_t result = 0;

    HDassert(cache_ptr);

    verify_unprotected();

    if ( pass ) {

        if ( destroy_entries ) {

            result = H5C_flush_cache(NULL, -1, -1, cache_ptr,
                                     H5F_FLUSH_INVALIDATE);

        } else {

            result = H5C_flush_cache(NULL, -1, -1, cache_ptr, 0);
        }
    }

    if ( dump_stats ) {

        H5C_stats(cache_ptr, "test cache", dump_detailed_stats);
    }

    if ( result < 0 ) {

        pass = FALSE;
        failure_mssg = "error in H5C_flush_cache().";
    }

    return;

} /* flush_cache() */


/*-------------------------------------------------------------------------
 * Function:	insert_entry()
 *
 * Purpose:	Insert the entry indicated by the type and index.  Mark
 *		it clean or dirty as indicated.
 *
 *		Note that I don't see much practical use for inserting
 *		a clean entry, but the interface permits it so we should
 *		test it.
 *
 *		Do nothing if pass is false.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/16/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
insert_entry(H5C_t * cache_ptr,
             int32_t type,
             int32_t idx,
             hbool_t dirty)
{
    herr_t result;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    if ( pass ) {

        HDassert( cache_ptr );
        HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
        HDassert( ( 0 <= idx ) && ( idx <= max_indices[type] ) );

        base_addr = entries[type];
        entry_ptr = &(base_addr[idx]);

        HDassert( entry_ptr->index == idx );
        HDassert( entry_ptr->type == type );
        HDassert( entry_ptr == entry_ptr->self );
        HDassert( !(entry_ptr->is_protected) );

        if ( dirty ) {

            (entry_ptr->header).is_dirty = dirty;
            entry_ptr->is_dirty = dirty;
        }

        result = H5C_insert_entry(NULL, -1, -1, cache_ptr, &(types[type]),
                                  entry_ptr->addr, (void *)entry_ptr);

        if ( ( result < 0 ) ||
             ( entry_ptr->header.is_protected ) ||
             ( entry_ptr->header.type != &(types[type]) ) ||
             ( entry_ptr->size != entry_ptr->header.size ) ||
             ( entry_ptr->addr != entry_ptr->header.addr ) ) {

            pass = FALSE;
            failure_mssg = "error in H5C_insert().";
        }

        HDassert( ((entry_ptr->header).type)->id == type );
    }

    return;

} /* insert_entry() */


/*-------------------------------------------------------------------------
 * Function:	rename_entry()
 *
 * Purpose:	Rename the entry indicated by the type and index to its
 *		main or alternate address as indicated.  If the entry is
 *		already at the desired entry, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/21/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
rename_entry(H5C_t * cache_ptr,
             int32_t type,
             int32_t idx,
             hbool_t main_addr)
{
    herr_t         result;
    hbool_t	   done = TRUE; /* will set to FALSE if we have work to do */
    haddr_t        old_addr = HADDR_UNDEF;
    haddr_t        new_addr = HADDR_UNDEF;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    HDassert( cache_ptr );
    HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
    HDassert( ( 0 <= idx ) && ( idx <= max_indices[type] ) );

    base_addr = entries[type];
    entry_ptr = &(base_addr[idx]);

    HDassert( entry_ptr->index == idx );
    HDassert( entry_ptr->type == type );
    HDassert( entry_ptr == entry_ptr->self );
    HDassert( !(entry_ptr->is_protected) );
    HDassert( !(entry_ptr->header.is_protected) );

    if ( entry_ptr->at_main_addr && !main_addr ) {

        /* rename to alt addr */

        HDassert( entry_ptr->addr == entry_ptr->main_addr );

        done = FALSE;
        old_addr = entry_ptr->addr;
        new_addr = entry_ptr->alt_addr;

    } else if ( !(entry_ptr->at_main_addr) && main_addr ) {

        /* rename to main addr */

        HDassert( entry_ptr->addr == entry_ptr->alt_addr );

        done = FALSE;
        old_addr = entry_ptr->addr;
        new_addr = entry_ptr->main_addr;
    }

    if ( ! done ) {

        result = H5C_rename_entry(cache_ptr, &(types[type]),
                                  old_addr, new_addr);
    }

    if ( ! done ) {

        if ( ( result < 0 ) || ( entry_ptr->header.addr != new_addr ) ) {

            pass = FALSE;
            failure_mssg = "error in H5C_rename_entry().";

        } else {

            entry_ptr->addr = new_addr;
            entry_ptr->at_main_addr = main_addr;
        }
    }

    HDassert( ((entry_ptr->header).type)->id == type );

    return;

} /* insert_entry() */


/*-------------------------------------------------------------------------
 * Function:	protect_entry()
 *
 * Purpose:	Protect the entry indicated by the type and index.
 *
 *		Do nothing if pass is FALSE on entry.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/11/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
protect_entry(H5C_t * cache_ptr,
              int32_t type,
              int32_t idx)
{
    /* const char * fcn_name = "protect_entry()"; */
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;
    H5C_cache_entry_t * cache_entry_ptr;

    if ( pass ) {

        HDassert( cache_ptr );
        HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
        HDassert( ( 0 <= idx ) && ( idx <= max_indices[type] ) );

        base_addr = entries[type];
        entry_ptr = &(base_addr[idx]);

        HDassert( entry_ptr->index == idx );
        HDassert( entry_ptr->type == type );
        HDassert( entry_ptr == entry_ptr->self );
        HDassert( !(entry_ptr->is_protected) );

        cache_entry_ptr = H5C_protect(NULL, -1, -1, cache_ptr, &(types[type]),
                                      entry_ptr->addr, NULL, NULL);

        if ( ( cache_entry_ptr != (void *)entry_ptr ) ||
             ( !(entry_ptr->header.is_protected) ) ||
             ( entry_ptr->header.type != &(types[type]) ) ||
             ( entry_ptr->size != entry_ptr->header.size ) ||
             ( entry_ptr->addr != entry_ptr->header.addr ) ) {

            pass = FALSE;
            failure_mssg = "error in H5C_protect().";

        } else {

            entry_ptr->is_protected = TRUE;

        }

        HDassert( ((entry_ptr->header).type)->id == type );
    }

    return;

} /* protect_entry() */


/*-------------------------------------------------------------------------
 * Function:	unprotect_entry()
 *
 * Purpose:	Unprotect the entry indicated by the type and index.
 *
 *		Do nothing if pass is FALSE on entry.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/12/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

#define NO_CHANGE	-1

static void
unprotect_entry(H5C_t * cache_ptr,
                int32_t type,
                int32_t idx,
                int dirty,
                hbool_t deleted)
{
    /* const char * fcn_name = "unprotect_entry()"; */
    herr_t result;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    if ( pass ) {

        HDassert( cache_ptr );
        HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
        HDassert( ( 0 <= idx ) && ( idx <= max_indices[type] ) );

        base_addr = entries[type];
        entry_ptr = &(base_addr[idx]);

        HDassert( entry_ptr->index == idx );
        HDassert( entry_ptr->type == type );
        HDassert( entry_ptr == entry_ptr->self );
        HDassert( entry_ptr->header.is_protected );
        HDassert( entry_ptr->is_protected );

        if ( ( dirty == TRUE ) || ( dirty == FALSE ) ) {

            entry_ptr->header.is_dirty = dirty;
            entry_ptr->is_dirty = dirty;
        }

        result = H5C_unprotect(NULL, -1, -1, cache_ptr, &(types[type]),
                               entry_ptr->addr, (void *)entry_ptr, deleted);

        if ( ( result < 0 ) ||
             ( entry_ptr->header.is_protected ) ||
             ( entry_ptr->header.type != &(types[type]) ) ||
             ( entry_ptr->size != entry_ptr->header.size ) ||
             ( entry_ptr->addr != entry_ptr->header.addr ) ) {

            pass = FALSE;
            failure_mssg = "error in H5C_unprotect().";

        }
        else
        {
            entry_ptr->is_protected = FALSE;
        }

        HDassert( ((entry_ptr->header).type)->id == type );
    }

    return;

} /* unprotect_entry() */


/*-------------------------------------------------------------------------
 * Function:	row_major_scan_forward()
 *
 * Purpose:	Do a sequence of inserts, protects, unprotects, renames,
 *		destroys while scanning through the set of entries.  If
 *		pass is false on entry, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/12/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
row_major_scan_forward(H5C_t * cache_ptr,
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
                       int dirty_destroys,
                       int dirty_unprotects)
{
    const char * fcn_name = "row_major_scan_forward";
    int32_t type;
    int32_t idx;

    if ( verbose )
        HDfprintf(stdout, "%s(): entering.\n", fcn_name);

    HDassert( lag > 5 );

    type = 0;

    if ( ( pass ) && ( reset_stats ) ) {

        H5C_stats__reset(cache_ptr);
    }

    while ( ( pass ) && ( type < NUMBER_OF_ENTRY_TYPES ) )
    {
        idx = -lag;

        while ( ( pass ) && ( idx <= (max_indices[type] + lag) ) )
        {
            if ( ( pass ) && ( do_inserts ) && ( (idx + lag) >= 0 ) &&
                 ( (idx + lag) <= max_indices[type] ) &&
                 ( ((idx + lag) % 2) == 0 ) &&
                 ( ! entry_in_cache(cache_ptr, type, (idx + lag)) ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(i, %d, %d) ", type, (idx + lag));

                insert_entry(cache_ptr, type, (idx + lag), dirty_inserts);
            }


            if ( ( pass ) && ( (idx + lag - 1) >= 0 ) &&
                 ( (idx + lag - 1) <= max_indices[type] ) &&
                 ( ( (idx + lag - 1) % 3 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, (idx + lag - 1));

                protect_entry(cache_ptr, type, (idx + lag - 1));
            }

            if ( ( pass ) && ( (idx + lag - 2) >= 0 ) &&
                 ( (idx + lag - 2) <= max_indices[type] ) &&
                 ( ( (idx + lag - 2) % 3 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(u, %d, %d) ", type, (idx + lag - 2));

                unprotect_entry(cache_ptr, type, idx+lag-2, NO_CHANGE, FALSE);
            }


            if ( ( pass ) && ( do_renames ) && ( (idx + lag - 2) >= 0 ) &&
                 ( (idx + lag - 2) <= max_indices[type] ) &&
                 ( ( (idx + lag - 2) % 3 ) == 0 ) ) {

                rename_entry(cache_ptr, type, (idx + lag - 2),
                             rename_to_main_addr);
            }


            if ( ( pass ) && ( (idx + lag - 3) >= 0 ) &&
                 ( (idx + lag - 3) <= max_indices[type] ) &&
                 ( ( (idx + lag - 3) % 5 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, (idx + lag - 3));

                protect_entry(cache_ptr, type, (idx + lag - 3));
            }

            if ( ( pass ) && ( (idx + lag - 5) >= 0 ) &&
                 ( (idx + lag - 5) <= max_indices[type] ) &&
                 ( ( (idx + lag - 5) % 5 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(u, %d, %d) ", type, (idx + lag - 5));

                unprotect_entry(cache_ptr, type, idx+lag-5, NO_CHANGE, FALSE);
            }

            if ( ( pass ) && ( idx >= 0 ) && ( idx <= max_indices[type] ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, idx);

                protect_entry(cache_ptr, type, idx);
            }


            if ( ( pass ) && ( (idx - lag + 2) >= 0 ) &&
                 ( (idx - lag + 2) <= max_indices[type] ) &&
                 ( ( (idx - lag + 2) % 7 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(u, %d, %d) ", type, (idx - lag + 2));

                unprotect_entry(cache_ptr, type, idx-lag+2, NO_CHANGE, FALSE);
            }

            if ( ( pass ) && ( (idx - lag + 1) >= 0 ) &&
                 ( (idx - lag + 1) <= max_indices[type] ) &&
                 ( ( (idx - lag + 1) % 7 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, (idx - lag + 1));

                protect_entry(cache_ptr, type, (idx - lag + 1));
            }


            if ( do_destroys ) {

                if ( ( pass ) && ( (idx - lag) >= 0 ) &&
                     ( ( idx - lag) <= max_indices[type] ) ) {

                    switch ( (idx - lag) %4 ) {

                        case 0: /* we just did an insert */
                            unprotect_entry(cache_ptr, type, idx - lag,
                                            NO_CHANGE, FALSE);
                            break;

                        case 1:
                            if ( (entries[type])[idx-lag].is_dirty ) {

                                unprotect_entry(cache_ptr, type, idx - lag,
                                                NO_CHANGE, FALSE);
                            } else {

                                unprotect_entry(cache_ptr, type, idx - lag,
                                                dirty_unprotects, FALSE);
                            }
                            break;

                        case 2: /* we just did an insrt */
                            unprotect_entry(cache_ptr, type, idx - lag,
                                            NO_CHANGE, TRUE);
                            break;

                        case 3:
                            if ( (entries[type])[idx-lag].is_dirty ) {

                                unprotect_entry(cache_ptr, type, idx - lag,
                                                NO_CHANGE, TRUE);
                            } else {

                                unprotect_entry(cache_ptr, type, idx - lag,
                                                dirty_destroys, TRUE);
                            }
                            break;

                        default:
                            HDassert(0); /* this can't happen... */
                            break;
                    }
                }

            } else {

                if ( ( pass ) && ( (idx - lag) >= 0 ) &&
                     ( ( idx - lag) <= max_indices[type] ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(u, %d, %d) ", type, (idx - lag));

                    unprotect_entry(cache_ptr, type, idx - lag,
                                    dirty_unprotects, FALSE);
                }
            }

            if ( verbose )
                HDfprintf(stdout, "\n");

            idx++;
        }
        type++;
    }

    if ( ( pass ) && ( display_stats ) ) {

        H5C_stats(cache_ptr, "test cache", display_detailed_stats);
    }

    return;

} /* row_major_scan_forward() */


/*-------------------------------------------------------------------------
 * Function:	row_major_scan_backward()
 *
 * Purpose:	Do a sequence of inserts, protects, unprotects, renames,
 *		destroys while scanning backwards through the set of
 *		entries.  If pass is false on entry, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/12/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
row_major_scan_backward(H5C_t * cache_ptr,
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
                        int dirty_destroys,
                        int dirty_unprotects)
{
    const char * fcn_name = "row_major_scan_backward";
    int32_t type;
    int32_t idx;

    if ( verbose )
        HDfprintf(stdout, "%s(): Entering.\n", fcn_name);

    HDassert( lag > 5 );

    type = NUMBER_OF_ENTRY_TYPES - 1;

    if ( ( pass ) && ( reset_stats ) ) {

        H5C_stats__reset(cache_ptr);
    }

    while ( ( pass ) && ( type >= 0 ) )
    {
        idx = max_indices[type] + lag;

        while ( ( pass ) && ( idx >= -lag ) )
        {
            if ( ( pass ) && ( do_inserts ) && ( (idx - lag) >= 0 ) &&
                 ( (idx - lag) <= max_indices[type] ) &&
                 ( ((idx - lag) % 2) == 1 ) &&
                 ( ! entry_in_cache(cache_ptr, type, (idx - lag)) ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(i, %d, %d) ", type, (idx - lag));

                insert_entry(cache_ptr, type, (idx - lag), dirty_inserts);
            }


            if ( ( pass ) && ( (idx - lag + 1) >= 0 ) &&
                 ( (idx - lag + 1) <= max_indices[type] ) &&
                 ( ( (idx - lag + 1) % 3 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, (idx - lag + 1));

                protect_entry(cache_ptr, type, (idx - lag + 1));
            }

            if ( ( pass ) && ( (idx - lag + 2) >= 0 ) &&
                 ( (idx - lag + 2) <= max_indices[type] ) &&
                 ( ( (idx - lag + 2) % 3 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(u, %d, %d) ", type, (idx - lag + 2));

                unprotect_entry(cache_ptr, type, idx-lag+2, NO_CHANGE, FALSE);
            }


            if ( ( pass ) && ( do_renames ) && ( (idx - lag + 2) >= 0 ) &&
                 ( (idx - lag + 2) <= max_indices[type] ) &&
                 ( ( (idx - lag + 2) % 3 ) == 0 ) ) {

                rename_entry(cache_ptr, type, (idx - lag + 2),
                             rename_to_main_addr);
            }


            if ( ( pass ) && ( (idx - lag + 3) >= 0 ) &&
                 ( (idx - lag + 3) <= max_indices[type] ) &&
                 ( ( (idx - lag + 3) % 5 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, (idx - lag + 3));

                protect_entry(cache_ptr, type, (idx - lag + 3));
            }

            if ( ( pass ) && ( (idx - lag + 5) >= 0 ) &&
                 ( (idx - lag + 5) <= max_indices[type] ) &&
                 ( ( (idx - lag + 5) % 5 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(u, %d, %d) ", type, (idx - lag + 5));

                unprotect_entry(cache_ptr, type, idx-lag+5, NO_CHANGE, FALSE);
            }

            if ( ( pass ) && ( idx >= 0 ) && ( idx <= max_indices[type] ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, idx);

                protect_entry(cache_ptr, type, idx);
            }


            if ( ( pass ) && ( (idx + lag - 2) >= 0 ) &&
                 ( (idx + lag - 2) <= max_indices[type] ) &&
                 ( ( (idx + lag - 2) % 7 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(u, %d, %d) ", type, (idx + lag - 2));

                unprotect_entry(cache_ptr, type, idx+lag-2, NO_CHANGE, FALSE);
            }

            if ( ( pass ) && ( (idx + lag - 1) >= 0 ) &&
                 ( (idx + lag - 1) <= max_indices[type] ) &&
                 ( ( (idx + lag - 1) % 7 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, (idx + lag - 1));

                protect_entry(cache_ptr, type, (idx + lag - 1));
            }


            if ( do_destroys ) {

                if ( ( pass ) && ( (idx + lag) >= 0 ) &&
                     ( ( idx + lag) <= max_indices[type] ) ) {

                    switch ( (idx + lag) %4 ) {

                        case 0:
                            if ( (entries[type])[idx+lag].is_dirty ) {

                                unprotect_entry(cache_ptr, type, idx + lag,
                                                NO_CHANGE, FALSE);
                            } else {

                                unprotect_entry(cache_ptr, type, idx + lag,
                                                dirty_unprotects, FALSE);
                            }
                            break;

                        case 1: /* we just did an insert */
                            unprotect_entry(cache_ptr, type, idx + lag,
                                            NO_CHANGE, FALSE);
                            break;

                        case 2:
                            if ( (entries[type])[idx + lag].is_dirty ) {

                                unprotect_entry(cache_ptr, type, idx + lag,
                                                NO_CHANGE, TRUE);
                            } else {

                                unprotect_entry(cache_ptr, type, idx + lag,
                                                dirty_destroys, TRUE);
                            }
                            break;

                        case 3: /* we just did an insrt */
                            unprotect_entry(cache_ptr, type, idx + lag,
                                            NO_CHANGE, TRUE);
                            break;

                        default:
                            HDassert(0); /* this can't happen... */
                            break;
                    }
                }
            } else {

                if ( ( pass ) && ( (idx + lag) >= 0 ) &&
                     ( ( idx + lag) <= max_indices[type] ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(u, %d, %d) ", type, (idx - lag));

                    unprotect_entry(cache_ptr, type, idx + lag,
                                    dirty_unprotects, FALSE);
                }
            }

            if ( verbose )
                HDfprintf(stdout, "\n");

            idx--;
        }
        type--;
    }

    if ( ( pass ) && ( display_stats ) ) {

        H5C_stats(cache_ptr, "test cache", display_detailed_stats);
    }

    return;

} /* row_major_scan_backward() */


/*-------------------------------------------------------------------------
 * Function:	col_major_scan_forward()
 *
 * Purpose:	Do a sequence of inserts, protects, and unprotects
 *		while scanning through the set of entries.  If
 *		pass is false on entry, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/23/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
col_major_scan_forward(H5C_t * cache_ptr,
                       int32_t lag,
                       hbool_t verbose,
                       hbool_t reset_stats,
                       hbool_t display_stats,
                       hbool_t display_detailed_stats,
                       hbool_t do_inserts,
                       hbool_t dirty_inserts,
                       int dirty_unprotects)
{
    const char * fcn_name = "col_major_scan_forward()";
    int32_t type;
    int32_t idx;

    if ( verbose )
        HDfprintf(stdout, "%s: entering.\n", fcn_name);

    HDassert( lag > 5 );

    type = 0;

    if ( ( pass ) && ( reset_stats ) ) {

        H5C_stats__reset(cache_ptr);
    }

    idx = -lag;

    while ( ( pass ) && ( (idx - lag) <= MAX_ENTRIES ) )
    {
        type = 0;

        while ( ( pass ) && ( type < NUMBER_OF_ENTRY_TYPES ) )
        {
            if ( ( pass ) && ( do_inserts ) && ( (idx + lag) >= 0 ) &&
                 ( (idx + lag) <= max_indices[type] ) &&
                 ( ((idx + lag) % 3) == 0 ) &&
                 ( ! entry_in_cache(cache_ptr, type, (idx + lag)) ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(i, %d, %d) ", type, (idx + lag));

                insert_entry(cache_ptr, type, (idx + lag), dirty_inserts);
            }

            if ( ( pass ) && ( idx >= 0 ) && ( idx <= max_indices[type] ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, idx);

                protect_entry(cache_ptr, type, idx);
            }

            if ( ( pass ) && ( (idx - lag) >= 0 ) &&
                 ( (idx - lag) <= max_indices[type] ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(u, %d, %d) ", type, (idx - lag));

                unprotect_entry(cache_ptr, type, idx - lag,
                                dirty_unprotects, FALSE);
            }

            if ( verbose )
                HDfprintf(stdout, "\n");

            type++;
        }

        idx++;
    }

    if ( ( pass ) && ( display_stats ) ) {

        H5C_stats(cache_ptr, "test cache", display_detailed_stats);
    }

    return;

} /* col_major_scan_forward() */


/*-------------------------------------------------------------------------
 * Function:	col_major_scan_backward()
 *
 * Purpose:	Do a sequence of inserts, protects, and unprotects
 *		while scanning backwards through the set of
 *		entries.  If pass is false on entry, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/23/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
col_major_scan_backward(H5C_t * cache_ptr,
                        int32_t lag,
                        hbool_t verbose,
                        hbool_t reset_stats,
                        hbool_t display_stats,
                        hbool_t display_detailed_stats,
                        hbool_t do_inserts,
                        hbool_t dirty_inserts,
                        int dirty_unprotects)
{
    const char * fcn_name = "col_major_scan_backward()";
    int mile_stone = 1;
    int32_t type;
    int32_t idx;

    if ( verbose )
        HDfprintf(stdout, "%s: entering.\n", fcn_name);

    HDassert( lag > 5 );

    if ( ( pass ) && ( reset_stats ) ) {

        H5C_stats__reset(cache_ptr);
    }

    idx = MAX_ENTRIES + lag;

    if ( verbose ) /* 1 */
        HDfprintf(stdout, "%s: point %d.\n", fcn_name, mile_stone++);


    while ( ( pass ) && ( (idx + lag) >= 0 ) )
    {
        type = NUMBER_OF_ENTRY_TYPES - 1;

        while ( ( pass ) && ( type >= 0 ) )
        {
            if ( ( pass ) && ( do_inserts) && ( (idx - lag) >= 0 ) &&
                 ( (idx - lag) <= max_indices[type] ) &&
                 ( ((idx - lag) % 3) == 0 ) &&
                 ( ! entry_in_cache(cache_ptr, type, (idx - lag)) ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(i, %d, %d) ", type, (idx - lag));

                insert_entry(cache_ptr, type, (idx - lag), dirty_inserts);
            }

            if ( ( pass ) && ( idx >= 0 ) && ( idx <= max_indices[type] ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, idx);

                protect_entry(cache_ptr, type, idx);
            }

            if ( ( pass ) && ( (idx + lag) >= 0 ) &&
                 ( (idx + lag) <= max_indices[type] ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(u, %d, %d) ", type, (idx + lag));

                unprotect_entry(cache_ptr, type, idx + lag,
                                dirty_unprotects, FALSE);
            }

            if ( verbose )
                HDfprintf(stdout, "\n");

            type--;
        }

        idx--;
    }

    if ( verbose ) /* 2 */
        HDfprintf(stdout, "%s: point %d.\n", fcn_name, mile_stone++);

    if ( ( pass ) && ( display_stats ) ) {

        H5C_stats(cache_ptr, "test cache", display_detailed_stats);
    }

    if ( verbose )
        HDfprintf(stdout, "%s: exiting.\n", fcn_name);

    return;

} /* col_major_scan_backward() */


/**************************************************************************/
/**************************************************************************/
/********************************* tests: *********************************/
/**************************************************************************/
/**************************************************************************/

/*-------------------------------------------------------------------------
 * Function:	smoke_check_1()
 *
 * Purpose:	A basic functional test, inserts, destroys, and renames in
 *              the mix, along with repeated protects and unprotects.
 *		All entries are marked as clean.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/16/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
smoke_check_1(void)
{
    const char * fcn_name = "smoke_check_1";
    hbool_t show_progress = FALSE;
    hbool_t dirty_inserts = FALSE;
    int dirty_unprotects = FALSE;
    int dirty_destroys = FALSE;
    hbool_t display_stats = FALSE;
    int32_t lag = 10;
    int mile_stone = 1;
    H5C_t * cache_ptr = NULL;

    TESTING("smoke check #1 -- all clean, ins, dest, ren, 4/2 MB cache");

    pass = TRUE;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    reset_entries();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    cache_ptr = setup_cache((size_t)(4 * 1024 * 1024),
                            (size_t)(2 * 1024 * 1024));

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ TRUE,
                           /* dirty_destroys         */ dirty_destroys,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_backward(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ FALSE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* do_renames             */ TRUE,
                            /* rename_to_main_addr    */ TRUE,
                            /* do_destroys            */ FALSE,
                            /* dirty_destroys         */ dirty_destroys,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ FALSE,
                           /* dirty_destroys         */ dirty_destroys,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    /* flush and destroy all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    col_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    /* flush all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ FALSE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    col_major_scan_backward(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ TRUE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    takedown_cache(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 11 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    verify_clean();
    verify_unprotected();

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass )
        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);

} /* smoke_check_1() */


/*-------------------------------------------------------------------------
 * Function:	smoke_check_2()
 *
 * Purpose:	A basic functional test, with inserts, destroys, and
 *		renames in the mix, along with some repeated protects
 *		and unprotects.  About half the entries are marked as
 *		dirty.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/24/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
smoke_check_2(void)
{
    const char * fcn_name = "smoke_check_2";
    hbool_t show_progress = FALSE;
    hbool_t dirty_inserts = TRUE;
    int dirty_unprotects = TRUE;
    int dirty_destroys = TRUE;
    hbool_t display_stats = FALSE;
    int32_t lag = 10;
    int mile_stone = 1;
    H5C_t * cache_ptr = NULL;

    TESTING("smoke check #2 -- ~1/2 dirty, ins, dest, ren, 4/2 MB cache");

    pass = TRUE;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    reset_entries();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    cache_ptr = setup_cache((size_t)(4 * 1024 * 1024),
                            (size_t)(2 * 1024 * 1024));

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ TRUE,
                           /* dirty_destroys         */ dirty_destroys,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_backward(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ FALSE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* do_renames             */ TRUE,
                            /* rename_to_main_addr    */ TRUE,
                            /* do_destroys            */ FALSE,
                            /* dirty_destroys         */ dirty_destroys,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ FALSE,
                           /* dirty_destroys         */ dirty_destroys,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    /* flush and destroy all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    col_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    /* flush all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ FALSE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    col_major_scan_backward(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ TRUE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    takedown_cache(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 11 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    verify_clean();
    verify_unprotected();

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass )
        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);

} /* smoke_check_2() */


/*-------------------------------------------------------------------------
 * Function:	smoke_check_3()
 *
 * Purpose:	A basic functional test on a tiny cache, with inserts,
 *		destroys, and renames in the mix, along with repeated
 *		protects and unprotects.  All entries are marked as clean.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/16/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
smoke_check_3(void)
{
    const char * fcn_name = "smoke_check_3";
    hbool_t show_progress = FALSE;
    hbool_t dirty_inserts = FALSE;
    int dirty_unprotects = FALSE;
    int dirty_destroys = FALSE;
    hbool_t display_stats = FALSE;
    int32_t lag = 10;
    int mile_stone = 1;
    H5C_t * cache_ptr = NULL;

    TESTING("smoke check #3 -- all clean, ins, dest, ren, 2/1 KB cache");

    pass = TRUE;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    reset_entries();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    cache_ptr = setup_cache((size_t)(2 * 1024),
                            (size_t)(1 * 1024));

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ TRUE,
                           /* dirty_destroys         */ dirty_destroys,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_backward(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ FALSE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* do_renames             */ TRUE,
                            /* rename_to_main_addr    */ TRUE,
                            /* do_destroys            */ FALSE,
                            /* dirty_destroys         */ dirty_destroys,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ FALSE,
                           /* dirty_destroys         */ dirty_destroys,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    /* flush and destroy all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    col_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    /* flush all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ FALSE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    col_major_scan_backward(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ TRUE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    takedown_cache(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 11 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    verify_clean();
    verify_unprotected();

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass )
        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);

} /* smoke_check_3() */


/*-------------------------------------------------------------------------
 * Function:	smoke_check_4()
 *
 * Purpose:	A basic functional test on a tiny cache, with inserts,
 *	 	destroys, and renames in the mix, along with repeated
 *		protects and unprotects.  About half the entries are
 *		marked as dirty.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/24/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
smoke_check_4(void)
{
    const char * fcn_name = "smoke_check_4";
    hbool_t show_progress = FALSE;
    hbool_t dirty_inserts = TRUE;
    int dirty_unprotects = TRUE;
    int dirty_destroys = TRUE;
    hbool_t display_stats = FALSE;
    int32_t lag = 10;
    int mile_stone = 1;
    H5C_t * cache_ptr = NULL;

    TESTING("smoke check #4 -- ~1/2 dirty, ins, dest, ren, 2/1 KB cache");

    pass = TRUE;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    reset_entries();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    cache_ptr = setup_cache((size_t)(2 * 1024),
                            (size_t)(1 * 1024));

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ TRUE,
                           /* dirty_destroys         */ dirty_destroys,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_backward(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ FALSE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* do_renames             */ TRUE,
                            /* rename_to_main_addr    */ TRUE,
                            /* do_destroys            */ FALSE,
                            /* dirty_destroys         */ dirty_destroys,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ FALSE,
                           /* dirty_destroys         */ dirty_destroys,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    /* flush and destroy all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    col_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ dirty_inserts,
                           /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    /* flush all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ FALSE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    col_major_scan_backward(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ TRUE,
                            /* dirty_inserts          */ dirty_inserts,
                            /* dirty_unprotects       */ dirty_unprotects);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    takedown_cache(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 11 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    verify_clean();
    verify_unprotected();

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass )
        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);

} /* smoke_check_4() */


/*-------------------------------------------------------------------------
 * Function:	write_permitted_check()
 *
 * Purpose:	A basic test of the write permitted function.  In essence,
 *		we load the cache up with dirty entryies, set
 *		write_permitted to FALSE, and then protect a bunch of
 *		entries.  If there are any writes while write_permitted is
 *		FALSE, the test will fail.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/24/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
write_permitted_check(void)
{

#if H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS

    const char * fcn_name = "write_permitted_check";
    hbool_t show_progress = FALSE;
    hbool_t display_stats = FALSE;
    int32_t lag = 10;
    int mile_stone = 1;
    H5C_t * cache_ptr = NULL;

#endif /* H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

    TESTING("write permitted check -- 1/0 MB cache");

#if H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS

    pass = TRUE;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    reset_entries();

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    cache_ptr = setup_cache((size_t)(1 * 1024 * 1024),
                            (size_t)(0));

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ TRUE,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ TRUE,
                           /* dirty_destroys         */ TRUE,
                           /* dirty_unprotects       */ TRUE);

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    write_permitted = FALSE;

    row_major_scan_backward(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ FALSE,
                            /* dirty_inserts          */ FALSE,
                            /* do_renames             */ TRUE,
                            /* rename_to_main_addr    */ TRUE,
                            /* do_destroys            */ FALSE,
                            /* dirty_destroys         */ FALSE,
                            /* dirty_unprotects       */ NO_CHANGE);

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    write_permitted = TRUE;

    row_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ TRUE,
                           /* do_renames             */ TRUE,
                           /* rename_to_main_addr    */ FALSE,
                           /* do_destroys            */ FALSE,
                           /* dirty_destroys         */ TRUE,
                           /* dirty_unprotects       */ TRUE);

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    /* flush and destroy all entries in the cache: */

    flush_cache(/* cache_ptr           */ cache_ptr,
                /* destroy_entries     */ TRUE,
                /* dump_stats          */ FALSE,
                /* dump_detailed_stats */ FALSE);

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    col_major_scan_forward(/* cache_ptr              */ cache_ptr,
                           /* lag                    */ lag,
                           /* verbose                */ FALSE,
                           /* reset_stats            */ TRUE,
                           /* display_stats          */ display_stats,
                           /* display_detailed_stats */ TRUE,
                           /* do_inserts             */ TRUE,
                           /* dirty_inserts          */ TRUE,
                           /* dirty_unprotects       */ TRUE);

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    write_permitted = FALSE;

    col_major_scan_backward(/* cache_ptr              */ cache_ptr,
                            /* lag                    */ lag,
                            /* verbose                */ FALSE,
                            /* reset_stats            */ TRUE,
                            /* display_stats          */ display_stats,
                            /* display_detailed_stats */ TRUE,
                            /* do_inserts             */ FALSE,
                            /* dirty_inserts          */ FALSE,
                            /* dirty_unprotects       */ NO_CHANGE);

    write_permitted = TRUE;

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    takedown_cache(cache_ptr, display_stats, TRUE);

    if ( show_progress ) /* 10 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    verify_clean();
    verify_unprotected();

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass )
        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);

#else /* H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

    SKIPPED();

    HDfprintf(stdout, "	Clean and dirty LRU lists disabled.\n");

#endif /* H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

} /* write_permitted_check() */


/*-------------------------------------------------------------------------
 * Function:	check_flush_protected_err()
 *
 * Purpose:	Verify that an attempt to flush the cache when it contains
 *		a protected entry will generate an error.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/24/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_flush_protected_err(void)
{
    const char * fcn_name = "check_flush_protected_err";
    H5C_t * cache_ptr = NULL;

    TESTING("flush cache with protected entry error");

    pass = TRUE;

    /* allocate a cache, protect an entry, and try to flush.  This
     * should fail.  Unprotect the entry and flush again -- should
     * succeed.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        protect_entry(cache_ptr, 0, 0);

        if ( H5C_flush_cache(NULL, -1, -1, cache_ptr, 0) >= 0 ) {

            pass = FALSE;
            failure_mssg = "flush succeeded on cache with protected entry.\n";

        } else {

            unprotect_entry(cache_ptr, 0, 0, TRUE, FALSE);

            if ( H5C_flush_cache(NULL, -1, -1, cache_ptr, 0) < 0 ) {

                pass = FALSE;
                failure_mssg = "flush failed after unprotect.\n";

            } else {

                takedown_cache(cache_ptr, FALSE, FALSE);
            }
        }
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass )
        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);

} /* check_flush_protected_err() */


/*-------------------------------------------------------------------------
 * Function:	check_destroy_protected_err()
 *
 * Purpose:	Verify that an attempt to destroy the cache when it contains
 *		a protected entry will generate an error.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/24/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_destroy_protected_err(void)
{
    const char * fcn_name = "check_destroy_protected_err";
    H5C_t * cache_ptr = NULL;

    TESTING("destroy cache with protected entry error");

    pass = TRUE;

    /* allocate a cache, protect an entry, and try to flush.  This
     * should fail.  Unprotect the entry and flush again -- should
     * succeed.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        protect_entry(cache_ptr, 0, 0);

        if ( H5C_dest(NULL, -1, -1, cache_ptr) >= 0 ) {

            pass = FALSE;
            failure_mssg = "destroy succeeded on cache with protected entry.\n";

        } else {

            unprotect_entry(cache_ptr, 0, 0, TRUE, FALSE);

            if ( H5C_dest(NULL, -1, -1, cache_ptr) < 0 ) {

                pass = FALSE;
                failure_mssg = "destroy failed after unprotect.\n";

            }
        }
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass )
        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);

} /* check_destroy_protected_err() */


/*-------------------------------------------------------------------------
 * Function:	check_duplicate_insert_err()
 *
 * Purpose:	Verify that an attempt to insert and entry that is
 *		alread in the cache will generate an error.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/24/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_duplicate_insert_err(void)
{
    const char * fcn_name = "check_duplicate_insert_err";
    herr_t result;
    H5C_t * cache_ptr = NULL;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    TESTING("duplicate entry insertion error");

    pass = TRUE;

    /* allocate a cache, protect an entry, and then try to insert
     * the entry again.  This should fail.  Unprotect the entry and
     * destroy the cache -- should succeed.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        protect_entry(cache_ptr, 0, 0);

        if ( pass ) {

            base_addr = entries[0];
            entry_ptr = &(base_addr[0]);

            result = H5C_insert_entry(NULL, -1, -1, cache_ptr,
                                      &(types[0]), entry_ptr->addr,
                                      (void *)entry_ptr);

            if ( result >= 0 ) {

                pass = FALSE;
                failure_mssg = "insert of duplicate entry succeeded.\n";

            } else {

                unprotect_entry(cache_ptr, 0, 0, TRUE, FALSE);

                takedown_cache(cache_ptr, FALSE, FALSE);
            }
        }
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass )
        HDfprintf(stdout, "%s(): failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);

} /* check_duplicate_insert_err() */


/*-------------------------------------------------------------------------
 * Function:	check_rename_err()
 *
 * Purpose:	Verify that an attempt to rename an entry to the address
 *		of an existing entry will generate an error.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/24/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_rename_err(void)
{
    const char * fcn_name = "check_rename_err()";
    herr_t result;
    H5C_t * cache_ptr = NULL;
    test_entry_t * entry_0_0_ptr;
    test_entry_t * entry_0_1_ptr;
    test_entry_t * entry_1_0_ptr;

    TESTING("rename to existing entry errors");

    pass = TRUE;

    /* allocate a cache, and insert several entries.  Try to rename
     * entries to other entries resident in the cache.  This should
     * fail.  Destroy the cache -- should succeed.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        insert_entry(cache_ptr, 0, 0, TRUE);
        insert_entry(cache_ptr, 0, 1, TRUE);
        insert_entry(cache_ptr, 1, 0, TRUE);

        entry_0_0_ptr = &((entries[0])[0]);
        entry_0_1_ptr = &((entries[0])[1]);
        entry_1_0_ptr = &((entries[1])[0]);
    }

    if ( pass ) {

        result = H5C_rename_entry(cache_ptr, &(types[0]),
                                  entry_0_0_ptr->addr, entry_0_1_ptr->addr);

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "rename to addr of same type succeeded.\n";
        }
    }

    if ( pass ) {

        result = H5C_rename_entry(cache_ptr, &(types[0]),
                                  entry_0_0_ptr->addr, entry_1_0_ptr->addr);

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "rename to addr of different type succeeded.\n";
        }
    }

    if ( pass ) {

        takedown_cache(cache_ptr, FALSE, FALSE);
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass )
        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);

} /* check_rename_err() */


/*-------------------------------------------------------------------------
 * Function:	check_double_protect_err()
 *
 * Purpose:	Verify that an attempt to protect an entry that is already
 *		protected will generate an error.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/24/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_double_protect_err(void)
{
    const char * fcn_name = "check_double_protect_err()";
    H5C_t * cache_ptr = NULL;
    test_entry_t * entry_ptr;
    H5C_cache_entry_t * cache_entry_ptr;

    TESTING("protect a protected entry error");

    pass = TRUE;

    /* allocate a cache, protect an entry, and then try to protect
     * the entry again.  This should fail.  Unprotect the entry and
     * destroy the cache -- should succeed.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        protect_entry(cache_ptr, 0, 0);

        entry_ptr = &((entries[0])[0]);
    }

    if ( pass ) {

        cache_entry_ptr = H5C_protect(NULL, -1, -1, cache_ptr, &(types[0]),
                                      entry_ptr->addr, NULL, NULL);

        if ( cache_entry_ptr != NULL ) {

            pass = FALSE;
            failure_mssg = "attempt to protect a protected entry succeeded.\n";
        }
    }

    if ( pass ) {

        unprotect_entry(cache_ptr, 0, 0, FALSE, FALSE);
    }

    if ( pass ) {

        takedown_cache(cache_ptr, FALSE, FALSE);
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass )
        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);

} /* check_double_protect_err() */


/*-------------------------------------------------------------------------
 * Function:	check_double_unprotect_err()
 *
 * Purpose:	Verify that an attempt to unprotect an entry that is already
 *		unprotected will generate an error.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/24/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

static void
check_double_unprotect_err(void)
{
    const char * fcn_name = "check_double_unprotect_err()";
    herr_t result;
    H5C_t * cache_ptr = NULL;
    test_entry_t * entry_ptr;

    TESTING("unprotect an unprotected entry error");

    pass = TRUE;

    /* allocate a cache, protect an entry, unprotect it, and then try to
     * unprotect the entry again.  This should fail.  Destroy the cache
     * -- should succeed.
     */

    if ( pass ) {

        reset_entries();

        cache_ptr = setup_cache((size_t)(2 * 1024),
                                (size_t)(1 * 1024));

        protect_entry(cache_ptr, 0, 0);

        unprotect_entry(cache_ptr, 0, 0, FALSE, FALSE);

        entry_ptr = &((entries[0])[0]);
    }

    if ( pass ) {

        result = H5C_unprotect(NULL, -1, -1, cache_ptr, &(types[0]),
                               entry_ptr->addr, (void *)entry_ptr, FALSE);

        if ( result > 0 ) {

            pass = FALSE;
            failure_mssg =
                "attempt to unprotect an unprotected entry succeeded.\n";
        }
    }

    if ( pass ) {

        takedown_cache(cache_ptr, FALSE, FALSE);
    }

    if ( pass ) { PASSED(); } else { H5_FAILED(); }

    if ( ! pass )
        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);

} /* check_double_unprotect_err() */


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Run tests on the cache code contained in H5C.c
 *
 * Return:	Success:
 *
 *		Failure:
 *
 * Programmer:	John Mainzer
 *              6/24/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    H5open();

    smoke_check_1();
    smoke_check_2();
    smoke_check_3();
    smoke_check_4();
    write_permitted_check();
    check_flush_protected_err();
    check_destroy_protected_err();
    check_duplicate_insert_err();
    check_rename_err();
    check_double_protect_err();
    check_double_unprotect_err();

    return(0);

} /* main() */
