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
 *              9/13/07
 *
 *		This file contains common code for tests of the cache
 *		implemented in H5C2.c
 */

#include "h5test.h"
#include "H5Iprivate.h"
#include "H5ACprivate.h"
#include "H5MFprivate.h"
#include "H5MMprivate.h"
#include "cache2_common.h"

#define USE_CORE_DRIVER	FALSE

/* global variable declarations: */

const char *FILENAME[] = {
	"cache2_test",
	"cache2_api_test",
	NULL
};

hid_t saved_fid = -1; /* store the file id here between cache setup 
		       * and takedown.
		       */

hbool_t write_permitted2 = TRUE;
hbool_t pass2 = TRUE; /* set to false on error */
hbool_t skip_long_tests2 = TRUE;
hbool_t run_full_test2 = TRUE;
const char *failure_mssg2 = NULL;

test_entry_t pico_entries2[NUM_PICO_ENTRIES];
test_entry_t nano_entries2[NUM_NANO_ENTRIES];
test_entry_t micro_entries2[NUM_MICRO_ENTRIES];
test_entry_t tiny_entries2[NUM_TINY_ENTRIES];
test_entry_t small_entries2[NUM_SMALL_ENTRIES];
test_entry_t medium_entries2[NUM_MEDIUM_ENTRIES];
test_entry_t large_entries2[NUM_LARGE_ENTRIES];
test_entry_t huge_entries2[NUM_HUGE_ENTRIES];
test_entry_t monster_entries2[NUM_MONSTER_ENTRIES];
test_entry_t variable_entries2[NUM_VARIABLE_ENTRIES];

test_entry_t * entries2[NUMBER_OF_ENTRY_TYPES] =
{
    pico_entries2,
    nano_entries2,
    micro_entries2,
    tiny_entries2,
    small_entries2,
    medium_entries2,
    large_entries2,
    huge_entries2,
    monster_entries2,
    variable_entries2
};

const int32_t max_indices2[NUMBER_OF_ENTRY_TYPES] =
{
    NUM_PICO_ENTRIES - 1,
    NUM_NANO_ENTRIES - 1,
    NUM_MICRO_ENTRIES - 1,
    NUM_TINY_ENTRIES - 1,
    NUM_SMALL_ENTRIES - 1,
    NUM_MEDIUM_ENTRIES - 1,
    NUM_LARGE_ENTRIES - 1,
    NUM_HUGE_ENTRIES - 1,
    NUM_MONSTER_ENTRIES - 1,
    NUM_VARIABLE_ENTRIES - 1
};

const size_t entry_sizes2[NUMBER_OF_ENTRY_TYPES] =
{
    PICO_ENTRY_SIZE,
    NANO_ENTRY_SIZE,
    MICRO_ENTRY_SIZE,
    TINY_ENTRY_SIZE,
    SMALL_ENTRY_SIZE,
    MEDIUM_ENTRY_SIZE,
    LARGE_ENTRY_SIZE,
    HUGE_ENTRY_SIZE,
    MONSTER_ENTRY_SIZE,
    VARIABLE_ENTRY_SIZE
};

const haddr_t base_addrs2[NUMBER_OF_ENTRY_TYPES] =
{
    PICO_BASE_ADDR,
    NANO_BASE_ADDR,
    MICRO_BASE_ADDR,
    TINY_BASE_ADDR,
    SMALL_BASE_ADDR,
    MEDIUM_BASE_ADDR,
    LARGE_BASE_ADDR,
    HUGE_BASE_ADDR,
    MONSTER_BASE_ADDR,
    VARIABLE_BASE_ADDR
};

const haddr_t alt_base_addrs2[NUMBER_OF_ENTRY_TYPES] =
{
    PICO_ALT_BASE_ADDR,
    NANO_ALT_BASE_ADDR,
    MICRO_ALT_BASE_ADDR,
    TINY_ALT_BASE_ADDR,
    SMALL_ALT_BASE_ADDR,
    MEDIUM_ALT_BASE_ADDR,
    LARGE_ALT_BASE_ADDR,
    HUGE_ALT_BASE_ADDR,
    MONSTER_ALT_BASE_ADDR,
    VARIABLE_ALT_BASE_ADDR
};

const char * entry_type_names2[NUMBER_OF_ENTRY_TYPES] =
{
    "pico entries -- 1 B",
    "nano entries -- 4 B",
    "micro entries -- 16 B",
    "tiny entries -- 64 B",
    "small entries -- 256 B",
    "medium entries -- 1 KB",
    "large entries -- 4 KB",
    "huge entries -- 16 KB",
    "monster entries -- 64 KB",
    "variable entries -- 1B - 10KB"
};


/* callback table declaration */

const H5C2_class_t types2[NUMBER_OF_ENTRY_TYPES] =
{
  {
    PICO_ENTRY_TYPE,
    "pico_entry",
    H5FD_MEM_DEFAULT,
    (H5C2_deserialize_func_t)pico_deserialize,
    (H5C2_image_len_func_t)pico_image_len,
    (H5C2_serialize_func_t)pico_serialize,
    (H5C2_free_icr_func_t)pico_free_icr,
    (H5C2_clear_dirty_bits_func_t)pico_clear_dirty_bits
  },
  {
    NANO_ENTRY_TYPE,
    "nano_entry",
    H5FD_MEM_DEFAULT,
    (H5C2_deserialize_func_t)nano_deserialize,
    (H5C2_image_len_func_t)nano_image_len,
    (H5C2_serialize_func_t)nano_serialize,
    (H5C2_free_icr_func_t)nano_free_icr,
    (H5C2_clear_dirty_bits_func_t)nano_clear_dirty_bits
  },
  {
    MICRO_ENTRY_TYPE,
    "micro_entry",
    H5FD_MEM_DEFAULT,
    (H5C2_deserialize_func_t)micro_deserialize,
    (H5C2_image_len_func_t)micro_image_len,
    (H5C2_serialize_func_t)micro_serialize,
    (H5C2_free_icr_func_t)micro_free_icr,
    (H5C2_clear_dirty_bits_func_t)micro_clear_dirty_bits
  },
  {
    TINY_ENTRY_TYPE,
    "tiny_entry",
    H5FD_MEM_DEFAULT,
    (H5C2_deserialize_func_t)tiny_deserialize,
    (H5C2_image_len_func_t)tiny_image_len,
    (H5C2_serialize_func_t)tiny_serialize,
    (H5C2_free_icr_func_t)tiny_free_icr,
    (H5C2_clear_dirty_bits_func_t)tiny_clear_dirty_bits
  },
  {
    SMALL_ENTRY_TYPE,
    "small_entry",
    H5FD_MEM_DEFAULT,
    (H5C2_deserialize_func_t)small_deserialize,
    (H5C2_image_len_func_t)small_image_len,
    (H5C2_serialize_func_t)small_serialize,
    (H5C2_free_icr_func_t)small_free_icr,
    (H5C2_clear_dirty_bits_func_t)small_clear_dirty_bits
  },
  {
    MEDIUM_ENTRY_TYPE,
    "medium_entry",
    H5FD_MEM_DEFAULT,
    (H5C2_deserialize_func_t)medium_deserialize,
    (H5C2_image_len_func_t)medium_image_len,
    (H5C2_serialize_func_t)medium_serialize,
    (H5C2_free_icr_func_t)medium_free_icr,
    (H5C2_clear_dirty_bits_func_t)medium_clear_dirty_bits
  },
  {
    LARGE_ENTRY_TYPE,
    "large_entry",
    H5FD_MEM_DEFAULT,
    (H5C2_deserialize_func_t)large_deserialize,
    (H5C2_image_len_func_t)large_image_len,
    (H5C2_serialize_func_t)large_serialize,
    (H5C2_free_icr_func_t)large_free_icr,
    (H5C2_clear_dirty_bits_func_t)large_clear_dirty_bits
  },
  {
    HUGE_ENTRY_TYPE,
    "huge_entry",
    H5FD_MEM_DEFAULT,
    (H5C2_deserialize_func_t)huge_deserialize,
    (H5C2_image_len_func_t)huge_image_len,
    (H5C2_serialize_func_t)huge_serialize,
    (H5C2_free_icr_func_t)huge_free_icr,
    (H5C2_clear_dirty_bits_func_t)huge_clear_dirty_bits
  },
  {
    MONSTER_ENTRY_TYPE,
    "monster_entry",
    H5FD_MEM_DEFAULT,
    (H5C2_deserialize_func_t)monster_deserialize,
    (H5C2_image_len_func_t)monster_image_len,
    (H5C2_serialize_func_t)monster_serialize,
    (H5C2_free_icr_func_t)monster_free_icr,
    (H5C2_clear_dirty_bits_func_t)monster_clear_dirty_bits
  },
  {
    VARIABLE_ENTRY_TYPE,
    "variable_entry",
    H5FD_MEM_DEFAULT,
    (H5C2_deserialize_func_t)variable_deserialize,
    (H5C2_image_len_func_t)variable_image_len,
    (H5C2_serialize_func_t)variable_serialize,
    (H5C2_free_icr_func_t)variable_free_icr,
    (H5C2_clear_dirty_bits_func_t)variable_clear_dirty_bits
  }
};

static herr_t clear_dirty_bits(haddr_t addr,
                               size_t len,
                               void * thing);

static void * deserialize(haddr_t addr,
                          size_t len,
                          const void * image_ptr,
                          const void * udata_ptr,
                          hbool_t * dirty_ptr);

static herr_t image_len(void *thing,
                        size_t *image_len_ptr);

static herr_t serialize(haddr_t addr,
		        size_t len,
		        void * image_ptr,
                        void * thing,
                        unsigned * flags_ptr,
                        haddr_t * new_addr_ptr,
                        size_t * new_len_ptr,
                        void ** new_image_ptr_ptr);

static herr_t free_icr(haddr_t addr,
                       size_t len,
                       void * thing);


/* address translation funtions: */

/*-------------------------------------------------------------------------
 * Function:	addr_to_type_and_index2
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
 * 		None.
 *
 *-------------------------------------------------------------------------
 */
void
addr_to_type_and_index2(haddr_t addr,
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
                ( addr >= alt_base_addrs2[i] ) )
        {
            i++;
        }

    } else {

        while ( ( i < NUMBER_OF_ENTRY_TYPES ) &&
                ( addr >= base_addrs2[i] ) )
        {
            i++;
        }
    }

    type = i - 1;

    HDassert( ( type >= 0 ) && ( type < NUMBER_OF_ENTRY_TYPES ) );

    if ( addr >= PICO_ALT_BASE_ADDR ) {

        idx = (int32_t)((addr - alt_base_addrs2[type]) / entry_sizes2[type]);
        HDassert( ( idx >= 0 ) && ( idx <= max_indices2[type] ) );
        HDassert( !((entries2[type])[idx].at_main_addr) );
        HDassert( addr == (entries2[type])[idx].alt_addr );

    } else {

        idx = (int32_t)((addr - base_addrs2[type]) / entry_sizes2[type]);
        HDassert( ( idx >= 0 ) && ( idx <= max_indices2[type] ) );
        HDassert( (entries2[type])[idx].at_main_addr );
        HDassert( addr == (entries2[type])[idx].main_addr );
    }

    HDassert( addr == (entries2[type])[idx].addr );

    *type_ptr = type;
    *index_ptr = idx;

    return;

} /* addr_to_type_and_index2() */


#if 0 /* This function has never been used, but we may want it
       * some time.  Lets keep it for now.
       */
/*-------------------------------------------------------------------------
 * Function:	type_and_index_to_addr2
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
haddr_t
type_and_index_to_addr2(int32_t type,
                        int32_t idx)
{
    haddr_t addr;

    HDassert( ( type >= 0 ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
    HDassert( ( idx >= 0 ) && ( idx <= max_indices2[type] ) );

    addr = base_addrs2[type] + (((haddr_t)idx) * entry_sizes2[type]);

    HDassert( addr == (entries2[type])[idx].addr );

    if ( (entries2[type])[idx].at_main_addr ) {

        HDassert( addr == (entries2[type])[idx].main_addr );

    } else {

        HDassert( addr == (entries2[type])[idx].alt_addr );
    }

    return(addr);

} /* type_and_index_to_addr2() */

#endif


/* Call back functions: */

/*-------------------------------------------------------------------------
 *
 * Function:    check_if_write_permitted2
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
 * 		None.
 *
 *-------------------------------------------------------------------------
 */

herr_t
check_write_permitted2(const H5F_t UNUSED * f,
                       hid_t UNUSED dxpl_id,
                       hbool_t * write_permitted_ptr)
{

    HDassert( write_permitted_ptr );
    *write_permitted_ptr = write_permitted2;

    return(SUCCEED);

} /* check_write_permitted2() */


/*-------------------------------------------------------------------------
 * Function:	clear_dirty_bits & friends
 *
 * Purpose:	Clear the dirty bits.  The helper functions verify that the
 *		correct version of clear_dirty_gits is being called, and 
 *		then call clear_dirty_bits() proper.
 *
 * Return:	SUCCEED
 *
 * Programmer:	John Mainzer
 *              9/20/07
 *
 * Modifications:
 *
 * 		None
 *
 *-------------------------------------------------------------------------
 */

herr_t 
clear_dirty_bits(haddr_t addr,
                 size_t len,
                 void * thing)
{
    test_entry_t * entry_ptr;
    test_entry_t * base_addr;

    HDassert( thing );

    entry_ptr = (test_entry_t *)thing;
    base_addr = entries2[entry_ptr->type];

    HDassert( entry_ptr->addr == addr );
    HDassert( entry_ptr->size == len );

    HDassert( entry_ptr->index >= 0 );
    HDassert( entry_ptr->index <= max_indices2[entry_ptr->type] );
    HDassert( entry_ptr == &(base_addr[entry_ptr->index]) );
    HDassert( entry_ptr == entry_ptr->self );
    HDassert( entry_ptr->header.addr == entry_ptr->addr );
    HDassert( entry_ptr->header.size == entry_ptr->size );
    HDassert( ( entry_ptr->type == VARIABLE_ENTRY_TYPE ) ||
	      ( entry_ptr->size == entry_sizes2[entry_ptr->type] ) );

    entry_ptr->is_dirty = FALSE;

    entry_ptr->cleared = TRUE;

    return(SUCCEED);

} /* clear_dirty_bits() */

herr_t 
pico_clear_dirty_bits(haddr_t addr,
                      size_t len,
                      void * thing)
{
    HDassert( ((test_entry_t *)thing)->type == PICO_ENTRY_TYPE );
    return(clear_dirty_bits(addr, len, thing));
}

herr_t 
nano_clear_dirty_bits(haddr_t addr,
                      size_t len,
                      void * thing)
{
    HDassert( ((test_entry_t *)thing)->type == NANO_ENTRY_TYPE );
    return(clear_dirty_bits(addr, len, thing));
}

herr_t 
micro_clear_dirty_bits(haddr_t addr,
                       size_t len,
                       void * thing)
{
    HDassert( ((test_entry_t *)thing)->type == MICRO_ENTRY_TYPE );
    return(clear_dirty_bits(addr, len, thing));
}

herr_t 
tiny_clear_dirty_bits(haddr_t addr,
                      size_t len,
                      void * thing)
{
    HDassert( ((test_entry_t *)thing)->type == TINY_ENTRY_TYPE );
    return(clear_dirty_bits(addr, len, thing));
}

herr_t 
small_clear_dirty_bits(haddr_t addr,
                       size_t len,
                       void * thing)
{
    HDassert( ((test_entry_t *)thing)->type == SMALL_ENTRY_TYPE );
    return(clear_dirty_bits(addr, len, thing));
}

herr_t 
medium_clear_dirty_bits(haddr_t addr,
                        size_t len,
                        void * thing)
{
    HDassert( ((test_entry_t *)thing)->type == MEDIUM_ENTRY_TYPE );
    return(clear_dirty_bits(addr, len, thing));
}

herr_t 
large_clear_dirty_bits(haddr_t addr,
                       size_t len,
                       void * thing)
{
    HDassert( ((test_entry_t *)thing)->type == LARGE_ENTRY_TYPE );
    return(clear_dirty_bits(addr, len, thing));
}

herr_t 
huge_clear_dirty_bits(haddr_t addr,
                      size_t len,
                      void * thing)
{
    HDassert( ((test_entry_t *)thing)->type == HUGE_ENTRY_TYPE );
    return(clear_dirty_bits(addr, len, thing));
}

herr_t 
monster_clear_dirty_bits(haddr_t addr,
                         size_t len,
                         void * thing)
{
    HDassert( ((test_entry_t *)thing)->type == MONSTER_ENTRY_TYPE );
    return(clear_dirty_bits(addr, len, thing));
}

herr_t 
variable_clear_dirty_bits(haddr_t addr,
                          size_t len,
                          void * thing)
{
    HDassert( ((test_entry_t *)thing)->type == VARIABLE_ENTRY_TYPE );
    return(clear_dirty_bits(addr, len, thing));
}


/*-------------------------------------------------------------------------
 * Function:	deserialize & friends
 *
 * Purpose:	deserialize the entry.  The helper functions verify that the
 *		correct version of deserialize is being called, and then call
 *		deserialize proper.
 *
 * Return:	void * (pointer to the in core representation of the entry)
 *
 * Programmer:	John Mainzer
 *              9/20/07
 *
 * Modifications:
 *
 * 		None
 *
 *-------------------------------------------------------------------------
 */

void *
deserialize(haddr_t addr,
            size_t len,
            const void * image_ptr,
            const UNUSED void * udata_ptr,
            hbool_t * dirty_ptr)
{
    int32_t type;
    int32_t idx;
    test_entry_t * entry_ptr;
    test_entry_t * base_addr;

    addr_to_type_and_index2(addr, &type, &idx);

    base_addr = entries2[type];
    entry_ptr = &(base_addr[idx]);

    HDassert( entry_ptr->type >= 0 );
    HDassert( entry_ptr->type < NUMBER_OF_ENTRY_TYPES );
    HDassert( entry_ptr->index == idx );
    HDassert( entry_ptr->index >= 0 );
    HDassert( entry_ptr->index <= max_indices2[type] );
    HDassert( entry_ptr == entry_ptr->self );
    HDassert( entry_ptr->addr == addr );
    HDassert( entry_ptr->size == len );
    HDassert( ( entry_ptr->type == VARIABLE_ENTRY_TYPE ) ||
	      ( entry_ptr->size == entry_sizes2[type] ) );
    HDassert( dirty_ptr != NULL );

    /* for now *dirty_ptr will always be FALSE */
    *dirty_ptr = FALSE;


    /* verify that the image contains the expected data. */
    HDassert( image_ptr != NULL );
    if ( ( ( entry_ptr->at_main_addr )
	   && 
	   ( entry_ptr->written_to_main_addr ) 
	 )
         ||
	 ( ( ! ( entry_ptr->at_main_addr ) ) 
	   &&
	   ( entry_ptr->written_to_alt_addr )
	 )
       ) {

        if ( ( type == PICO_ENTRY_TYPE ) || ( type == VARIABLE_ENTRY_TYPE ) ) {

            if ( (*((const char *)image_ptr)) != (char)(idx & 0xFF) ) {

                HDfprintf(stdout, "type = %d, idx = %d, addr = 0x%lx.\n",
                          type, idx, (long)addr);
                HDfprintf(stdout, "*image_ptr = 0x%x\n",
                          (int)(*((const char *)image_ptr)));
                HDfprintf(stdout, "expected *image_ptr = 0x%x\n",
                          (int)(idx & 0xFF));
            }
  	    HDassert( (*((const char *)image_ptr)) == (char)(idx & 0xFF) );

        } else {

            if ( (*(((const char *)image_ptr) + 2)) != (char)(idx & 0xFF) ) {

                HDfprintf(stdout, "type = %d, idx = %d, addr = 0x%lx.\n",
                          type, idx, (long)addr);
                HDfprintf(stdout, "*image_ptr = 0x%x 0x%x 0x%x\n",
                          (int)(*((const char *)image_ptr)),
                          (int)(*(((const char *)image_ptr) + 1)),
                          (int)(*(((const char *)image_ptr) + 2)));
                HDfprintf(stdout, "expected *image_ptr = 0x%x\n",
                          (int)(idx & 0xFF),
                          (int)((idx & 0xFF00)>>8),
                          (int)(idx & 0xFF));
            }
	    HDassert( (*((const char *)image_ptr)) == (char)(type & 0xFF) );
	    HDassert( (*(((const char *)image_ptr) + 1)) == 
                      (char)((idx & 0xFF00)>>8) );
	    HDassert( (*(((const char *)image_ptr) + 2)) == 
	              (char)(idx & 0xFF) );

        }
    }

    entry_ptr->deserialized = TRUE;

    entry_ptr->header.is_dirty = FALSE;
    entry_ptr->is_dirty = FALSE;

    (entry_ptr->deserializes)++;

    return((void *)entry_ptr);

} /* deserialize() */

void *
pico_deserialize(haddr_t addr,
                 size_t len,
                 const void * image_ptr,
                 const UNUSED void * udata_ptr,
                 hbool_t * dirty_ptr)
{
    return deserialize(addr, len, image_ptr, udata_ptr, dirty_ptr);
}

void *
nano_deserialize(haddr_t addr,
                 size_t len,
                 const void * image_ptr,
                 const UNUSED void * udata_ptr,
                 hbool_t * dirty_ptr)
{
    return deserialize(addr, len, image_ptr, udata_ptr, dirty_ptr);
}

void *
micro_deserialize(haddr_t addr,
                  size_t len,
                  const void * image_ptr,
                  const UNUSED void * udata_ptr,
                  hbool_t * dirty_ptr)
{
    return deserialize(addr, len, image_ptr, udata_ptr, dirty_ptr);
}

void *
tiny_deserialize(haddr_t addr,
                 size_t len,
                 const void * image_ptr,
                 const UNUSED void * udata_ptr,
                 hbool_t * dirty_ptr)
{
    return deserialize(addr, len, image_ptr, udata_ptr, dirty_ptr);
}

void *
small_deserialize(haddr_t addr,
                  size_t len,
                  const void * image_ptr,
                  const UNUSED void * udata_ptr,
                  hbool_t * dirty_ptr)
{
    return deserialize(addr, len, image_ptr, udata_ptr, dirty_ptr);
}

void *
medium_deserialize(haddr_t addr,
                   size_t len,
                   const void * image_ptr,
                   const UNUSED void * udata_ptr,
                   hbool_t * dirty_ptr)
{
    return deserialize(addr, len, image_ptr, udata_ptr, dirty_ptr);
}

void *
large_deserialize(haddr_t addr,
                  size_t len,
                  const void * image_ptr,
                  const UNUSED void * udata_ptr,
                  hbool_t * dirty_ptr)
{
    return deserialize(addr, len, image_ptr, udata_ptr, dirty_ptr);
}

void *
huge_deserialize(haddr_t addr,
                 size_t len,
                 const void * image_ptr,
                 const UNUSED void * udata_ptr,
                 hbool_t * dirty_ptr)
{
    return deserialize(addr, len, image_ptr, udata_ptr, dirty_ptr);
}

void *
monster_deserialize(haddr_t addr,
                    size_t len,
                    const void * image_ptr,
                    const UNUSED void * udata_ptr,
                    hbool_t * dirty_ptr)
{
    return deserialize(addr, len, image_ptr, udata_ptr, dirty_ptr);
}

void *
variable_deserialize(haddr_t addr,
                     size_t len,
                     const void * image_ptr,
                     const UNUSED void * udata_ptr,
                     hbool_t * dirty_ptr)
{
    return deserialize(addr, len, image_ptr, udata_ptr, dirty_ptr);
}


/*-------------------------------------------------------------------------
 * Function:	image_len & friends
 *
 * Purpose:	Return the real (and possibly reduced) length of the image.
 * 		The helper functions verify that the correct version of 
 * 		deserialize is being called, and then call deserialize 
 * 		proper.
 *
 * Return:	SUCCEED
 *
 * Programmer:	John Mainzer
 *              9/19/07
 *
 * Modifications:
 *
 * 		None.
 *
 *-------------------------------------------------------------------------
 */

herr_t 
image_len(void *thing,
          size_t *image_len_ptr)
{
    int32_t type;
    int32_t idx;
    test_entry_t * entry_ptr;
    test_entry_t * base_addr;

    HDassert( thing );
    HDassert( image_len_ptr );

    entry_ptr = (test_entry_t *)thing;

    HDassert( entry_ptr->self == entry_ptr );

    type = entry_ptr->type;
    idx = entry_ptr->index;

    HDassert( ( type >= 0 ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
    HDassert( ( idx >= 0 ) && ( idx <= max_indices2[type] ) );

    base_addr = entries2[type];
    HDassert( entry_ptr == &(base_addr[idx]) );

    if ( type != VARIABLE_ENTRY_TYPE ) {

	HDassert( entry_ptr->size == entry_sizes2[type] );

    } else {

	HDassert( entry_ptr->size <= entry_sizes2[type] );
	HDassert( entry_ptr->size > 0 );
    }

    *image_len_ptr = entry_ptr->size;

    return(SUCCEED);

} /* image_len() */


herr_t 
pico_image_len(void *thing,
               size_t *image_len_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == PICO_ENTRY_TYPE );
    return(image_len(thing, image_len_ptr));
}

herr_t 
nano_image_len(void *thing,
               size_t *image_len_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == NANO_ENTRY_TYPE );
    return(image_len(thing, image_len_ptr));
}

herr_t 
micro_image_len(void *thing,
                size_t *image_len_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == MICRO_ENTRY_TYPE );
    return(image_len(thing, image_len_ptr));
}

herr_t 
tiny_image_len(void *thing,
               size_t *image_len_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == TINY_ENTRY_TYPE );
    return(image_len(thing, image_len_ptr));
}


herr_t 
small_image_len(void *thing,
                size_t *image_len_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == SMALL_ENTRY_TYPE );
    return(image_len(thing, image_len_ptr));
}

herr_t 
medium_image_len(void *thing,
                 size_t *image_len_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == MEDIUM_ENTRY_TYPE );
    return(image_len(thing, image_len_ptr));
}

herr_t 
large_image_len(void *thing,
                size_t *image_len_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == LARGE_ENTRY_TYPE );
    return(image_len(thing, image_len_ptr));
}

herr_t 
huge_image_len(void *thing,
               size_t *image_len_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == HUGE_ENTRY_TYPE );
    return(image_len(thing, image_len_ptr));
}

herr_t 
monster_image_len(void *thing,
                  size_t *image_len_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == MONSTER_ENTRY_TYPE );
    return(image_len(thing, image_len_ptr));
}

herr_t 
variable_image_len(void *thing,
                   size_t *image_len_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == VARIABLE_ENTRY_TYPE );
    return(image_len(thing, image_len_ptr));
}


/*-------------------------------------------------------------------------
 * Function:	serialize & friends
 *
 * Purpose:	Serialize the supplied entry.  For now this consistes of 
 * 		loading the type and index of the entry into the first 
 * 		three bytes of the image (if it is long enough -- if not
 * 		just load the low order byte of the index into the first 
 * 		byte of the image).
 *
 * 		The helper functions verify that the correct version of 
 * 		serialize is being called, and then call serialize 
 * 		proper.
 *
 * Return:	SUCCEED if successful, FAIL otherwise.
 *
 * Programmer:	John Mainzer
 *              9/19/07
 *
 * Modifications:
 *
 * 		None.
 *
 *-------------------------------------------------------------------------
 */

herr_t 
serialize(haddr_t addr,
          size_t len,
          void * image_ptr,
          void * thing,
          unsigned * flags_ptr,
          haddr_t * new_addr_ptr,
          size_t * new_len_ptr,
          void ** new_image_ptr_ptr)
{
    const char * fcn_name = "serialize()";
    hbool_t verbose = FALSE;
    herr_t ret_val = SUCCEED;
    int32_t i;
    int32_t type;
    int32_t idx;
    test_entry_t * entry_ptr;
    test_entry_t * base_addr;

    if ( verbose ) {

        HDfprintf(stdout, "%s: addr = 0x%lx, len = %ld.\n", fcn_name,
                  (long)addr, (long)len);
    }

    HDassert( image_ptr );
    HDassert( thing );
    HDassert( flags_ptr );

    *flags_ptr = 0;

    HDassert( new_addr_ptr );
    HDassert( new_len_ptr );
    HDassert( new_image_ptr_ptr );

    entry_ptr = (test_entry_t *)thing;
    
    HDassert( entry_ptr->self == entry_ptr );
    HDassert( entry_ptr->addr == addr );
    HDassert( entry_ptr->size == len );
    
    /* shouldn't serialize the entry unless it is dirty */
    HDassert( entry_ptr->is_dirty );

    type = entry_ptr->type;
    idx = entry_ptr->index;

    HDassert( ( type >= 0 ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
    HDassert( ( idx >= 0 ) && ( idx <= max_indices2[type] ) );

    base_addr = entries2[type];

    HDassert( entry_ptr == &(base_addr[idx]) );

    HDassert( entry_ptr->num_flush_ops >= 0 );
    HDassert( entry_ptr->num_flush_ops < MAX_FLUSH_OPS );

    if ( entry_ptr->num_flush_ops > 0 ) {

        for ( i = 0; i < entry_ptr->num_flush_ops; i++ )
        {
            execute_flush_op2(entry_ptr->cache_ptr,
                              entry_ptr,
                              &((entry_ptr->flush_ops)[i]),
                              flags_ptr);
        }
        entry_ptr->num_flush_ops = 0;
        entry_ptr->flush_op_self_resize_in_progress = FALSE;

	if ( ( pass2 ) && 
             ( ((*flags_ptr) & H5C2__SERIALIZE_RESIZED_FLAG) != 0 ) ) {

	    /* re-allocate *image_ptr, and place the new pointer in 
	     * *new_image_ptr_ptr.
	     */
            image_ptr = H5MM_xfree(image_ptr);

            if ( image_ptr != NULL ) {

                ret_val = FAIL;
                pass2 = FALSE;
                failure_mssg2 = "couldn't free image_ptr.";
            }    

            if ( pass2 ) {

                HDassert( entry_ptr->type == VARIABLE_ENTRY_TYPE );
                HDassert( entry_ptr->size > 0 );
                HDassert( entry_ptr->size <= VARIABLE_ENTRY_SIZE );

                image_ptr = H5MM_malloc((size_t)(entry_ptr->size));

                if ( image_ptr == NULL ) {
 
                    ret_val = FAIL;
                    pass2 = FALSE;
                    failure_mssg2 = "couldn't allocate new image.";

                } else {

                    *new_image_ptr_ptr = image_ptr;
                    *new_len_ptr = entry_ptr->size;

                }
            }
	}

	if ( ((*flags_ptr) & H5C2__SERIALIZE_RENAMED_FLAG) != 0 ) {

            HDassert( ((*flags_ptr) | H5C2__SERIALIZE_RESIZED_FLAG) != 0 );

	    /* place the new address in *new_addr_ptr */

            *new_addr_ptr = entry_ptr->addr;
	}
    }

    if ( ( type == PICO_ENTRY_TYPE ) || ( type == VARIABLE_ENTRY_TYPE ) ) {

	HDassert( entry_ptr->size >= PICO_ENTRY_SIZE );
	*((char *)image_ptr) = (char)((entry_ptr->index) & 0xFF);

    } else {

	HDassert(entry_ptr->size >= NANO_ENTRY_SIZE );
	*((char *)image_ptr) = (char)((entry_ptr->type) & 0xFF);
	*(((char *)image_ptr) + 1) = 
            (char)(((entry_ptr->index) & 0xFF00) >> 8);
	*(((char *)image_ptr) + 2) = (char)((entry_ptr->index) & 0xFF);

    }

    /* We no longer do the actual write through an callback -- this is 
     * as close to that callback as we will get.  Hence mark the entry
     * clean here.  If all goes well, it will be flushed shortly.
     */

    entry_ptr->is_dirty = FALSE;

    /* since the entry is about to be written to disk, we can mark it
     * as initialized.
     */
    if ( entry_ptr->at_main_addr ) {
	entry_ptr->written_to_main_addr = TRUE;
    } else {
	entry_ptr->written_to_alt_addr = TRUE;
    }

    /* do book keeping */
    (entry_ptr->serializes)++;
    entry_ptr->serialized = TRUE;

    return(SUCCEED);

} /* serialize() */

herr_t 
pico_serialize(haddr_t addr,
               size_t len,
               void * image_ptr,
               void * thing,
               unsigned * flags_ptr,
               haddr_t * new_addr_ptr,
               size_t * new_len_ptr,
               void ** new_image_ptr_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == PICO_ENTRY_TYPE );
    return(serialize(addr, len, image_ptr, thing, flags_ptr, 
                     new_addr_ptr, new_len_ptr, new_image_ptr_ptr));
}

herr_t 
nano_serialize(haddr_t addr,
               size_t len,
               void * image_ptr,
               void * thing,
               unsigned * flags_ptr,
               haddr_t * new_addr_ptr,
               size_t * new_len_ptr,
               void ** new_image_ptr_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == NANO_ENTRY_TYPE );
    return(serialize(addr, len, image_ptr, thing, flags_ptr, 
                     new_addr_ptr, new_len_ptr, new_image_ptr_ptr));
}

herr_t 
micro_serialize(haddr_t addr,
                size_t len,
                void * image_ptr,
                void * thing,
                unsigned * flags_ptr,
                haddr_t * new_addr_ptr,
                size_t * new_len_ptr,
                void ** new_image_ptr_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == MICRO_ENTRY_TYPE );
    return(serialize(addr, len, image_ptr, thing, flags_ptr, 
                     new_addr_ptr, new_len_ptr, new_image_ptr_ptr));
}

herr_t 
tiny_serialize(haddr_t addr,
               size_t len,
               void * image_ptr,
               void * thing,
               unsigned * flags_ptr,
               haddr_t * new_addr_ptr,
               size_t * new_len_ptr,
               void ** new_image_ptr_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == TINY_ENTRY_TYPE );
    return(serialize(addr, len, image_ptr, thing, flags_ptr, 
                     new_addr_ptr, new_len_ptr, new_image_ptr_ptr));
}

herr_t 
small_serialize(haddr_t addr,
                size_t len,
                void * image_ptr,
                void * thing,
                unsigned * flags_ptr,
                haddr_t * new_addr_ptr,
                size_t * new_len_ptr,
                void ** new_image_ptr_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == SMALL_ENTRY_TYPE );
    return(serialize(addr, len, image_ptr, thing, flags_ptr, 
                     new_addr_ptr, new_len_ptr, new_image_ptr_ptr));
}

herr_t 
medium_serialize(haddr_t addr,
                 size_t len,
                 void * image_ptr,
                 void * thing,
                 unsigned * flags_ptr,
                 haddr_t * new_addr_ptr,
                 size_t * new_len_ptr,
                 void ** new_image_ptr_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == MEDIUM_ENTRY_TYPE );
    return(serialize(addr, len, image_ptr, thing, flags_ptr, 
                     new_addr_ptr, new_len_ptr, new_image_ptr_ptr));
}

herr_t 
large_serialize(haddr_t addr,
                size_t len,
                void * image_ptr,
                void * thing,
                unsigned * flags_ptr,
                haddr_t * new_addr_ptr,
                size_t * new_len_ptr,
                void ** new_image_ptr_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == LARGE_ENTRY_TYPE );
    return(serialize(addr, len, image_ptr, thing, flags_ptr, 
                     new_addr_ptr, new_len_ptr, new_image_ptr_ptr));
}

herr_t 
huge_serialize(haddr_t addr,
               size_t len,
               void * image_ptr,
               void * thing,
               unsigned * flags_ptr,
               haddr_t * new_addr_ptr,
               size_t * new_len_ptr,
               void ** new_image_ptr_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == HUGE_ENTRY_TYPE );
    return(serialize(addr, len, image_ptr, thing, flags_ptr, 
                     new_addr_ptr, new_len_ptr, new_image_ptr_ptr));
}

herr_t 
monster_serialize(haddr_t addr,
                  size_t len,
                  void * image_ptr,
                  void * thing,
                  unsigned * flags_ptr,
                  haddr_t * new_addr_ptr,
                  size_t * new_len_ptr,
                  void ** new_image_ptr_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == MONSTER_ENTRY_TYPE );
    return(serialize(addr, len, image_ptr, thing, flags_ptr, 
                     new_addr_ptr, new_len_ptr, new_image_ptr_ptr));
}

herr_t 
variable_serialize(haddr_t addr,
                   size_t len,
                   void * image_ptr,
                   void * thing,
                   unsigned * flags_ptr,
                   haddr_t * new_addr_ptr,
                   size_t * new_len_ptr,
                   void ** new_image_ptr_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == VARIABLE_ENTRY_TYPE );
    return(serialize(addr, len, image_ptr, thing, flags_ptr, 
                     new_addr_ptr, new_len_ptr, new_image_ptr_ptr));
}


/*-------------------------------------------------------------------------
 * Function:	free_icr & friends
 *
 * Purpose:	Nominally, this callback is supposed to free the 
 * 		in core representation of the entry.  
 *
 * 		In the context of this test bed, we use it to do 
 * 		do all the processing we used to do on a destroy.
 * 		In particular, we use it to release all the pins
 * 		that this entry may have on other entries.
 * 		
 * 		The helper functions verify that the correct version of 
 * 		serialize is being called, and then call free_icr
 * 		proper.
 *
 * Return:	SUCCEED
 *
 * Programmer:	John Mainzer
 *              9/19/07
 *
 * Modifications:
 *
 * 		None.
 *
 *-------------------------------------------------------------------------
 */

herr_t 
free_icr(haddr_t addr,
         size_t len,
         void * thing)
{
    int i;
    test_entry_t * entry_ptr;
    test_entry_t * base_addr;
    test_entry_t * pinned_entry_ptr;
    test_entry_t * pinned_base_addr;

    HDassert( thing );

    entry_ptr = (test_entry_t *)thing;
    base_addr = entries2[entry_ptr->type];

    HDassert( entry_ptr->addr == addr );
    HDassert( entry_ptr->size == len );

    HDassert( entry_ptr->index >= 0 );
    HDassert( entry_ptr->index <= max_indices2[entry_ptr->type] );
    HDassert( entry_ptr == &(base_addr[entry_ptr->index]) );
    HDassert( entry_ptr == entry_ptr->self );
    HDassert( entry_ptr->cache_ptr != NULL );
    HDassert( entry_ptr->cache_ptr->magic == H5C2__H5C2_T_MAGIC );
    HDassert( ( entry_ptr->header.destroy_in_progress ) ||
              ( entry_ptr->header.addr == entry_ptr->addr ) );
    HDassert( entry_ptr->header.size == entry_ptr->size );
    HDassert( ( entry_ptr->type == VARIABLE_ENTRY_TYPE ) ||
	      ( entry_ptr->size == entry_sizes2[entry_ptr->type] ) );

    HDassert( !(entry_ptr->is_dirty) );
    HDassert( !(entry_ptr->header.is_dirty) );

    if ( entry_ptr->num_pins > 0 ) {

	for ( i = 0; i < entry_ptr->num_pins; i++ )
        {
	    pinned_base_addr = entries2[entry_ptr->pin_type[i]];
	    pinned_entry_ptr = &(pinned_base_addr[entry_ptr->pin_idx[i]]);

	    HDassert( 0 <= pinned_entry_ptr->type );
            HDassert( pinned_entry_ptr->type < NUMBER_OF_ENTRY_TYPES );
	    HDassert( pinned_entry_ptr->type == entry_ptr->pin_type[i] );
	    HDassert( pinned_entry_ptr->index >= 0 );
	    HDassert( pinned_entry_ptr->index <=
		      max_indices2[pinned_entry_ptr->type] );
	    HDassert( pinned_entry_ptr->index == entry_ptr->pin_idx[i] );
	    HDassert( pinned_entry_ptr == pinned_entry_ptr->self );
	    HDassert( pinned_entry_ptr->header.is_pinned );
	    HDassert( pinned_entry_ptr->is_pinned );
	    HDassert( pinned_entry_ptr->pinning_ref_count > 0 );

	    pinned_entry_ptr->pinning_ref_count--;

	    if ( pinned_entry_ptr->pinning_ref_count <= 0 ) {

		unpin_entry2(pinned_entry_ptr->cache_ptr,
			     pinned_entry_ptr->type,
			     pinned_entry_ptr->index);
	    }

	    entry_ptr->pin_type[i] = -1;
	    entry_ptr->pin_idx[i] = -1;
	}
	entry_ptr->num_pins = 0;
    }

    entry_ptr->destroyed = TRUE;
    entry_ptr->cache_ptr = NULL;

    return(SUCCEED);

} /* free_icr() */

herr_t 
pico_free_icr(haddr_t addr,
              size_t len,
              void * thing)
{
    HDassert( ((test_entry_t *)thing)->type == PICO_ENTRY_TYPE );
    return(free_icr(addr, len, thing));
}

herr_t 
nano_free_icr(haddr_t addr,
              size_t len,
              void * thing)
{
    HDassert( ((test_entry_t *)thing)->type == NANO_ENTRY_TYPE );
    return(free_icr(addr, len, thing));
}

herr_t 
micro_free_icr(haddr_t addr,
               size_t len,
               void * thing)
{
    HDassert( ((test_entry_t *)thing)->type == MICRO_ENTRY_TYPE );
    return(free_icr(addr, len, thing));
}

herr_t 
tiny_free_icr(haddr_t addr,
              size_t len,
              void * thing)
{
    HDassert( ((test_entry_t *)thing)->type == TINY_ENTRY_TYPE );
    return(free_icr(addr, len, thing));
}

herr_t 
small_free_icr(haddr_t addr,
               size_t len,
               void * thing)
{
    HDassert( ((test_entry_t *)thing)->type == SMALL_ENTRY_TYPE );
    return(free_icr(addr, len, thing));
}

herr_t 
medium_free_icr(haddr_t addr,
                size_t len,
                void * thing)
{
    HDassert( ((test_entry_t *)thing)->type == MEDIUM_ENTRY_TYPE );
    return(free_icr(addr, len, thing));
}

herr_t 
large_free_icr(haddr_t addr,
               size_t len,
               void * thing)
{
    HDassert( ((test_entry_t *)thing)->type == LARGE_ENTRY_TYPE );
    return(free_icr(addr, len, thing));
}

herr_t 
huge_free_icr(haddr_t addr,
              size_t len,
              void * thing)
{
    HDassert( ((test_entry_t *)thing)->type == HUGE_ENTRY_TYPE );
    return(free_icr(addr, len, thing));
}

herr_t 
monster_free_icr(haddr_t addr,
                 size_t len,
                 void * thing)
{
    HDassert( ((test_entry_t *)thing)->type == MONSTER_ENTRY_TYPE );
    return(free_icr(addr, len, thing));
}

herr_t 
variable_free_icr(haddr_t addr,
                  size_t len,
                  void * thing)
{
    HDassert( ((test_entry_t *)thing)->type == VARIABLE_ENTRY_TYPE );
    return(free_icr(addr, len, thing));
}


/**************************************************************************/
/**************************************************************************/
/************************** test utility functions: ***********************/
/**************************************************************************/
/**************************************************************************/

/*-------------------------------------------------------------------------
 * Function:	add_flush_op2
 *
 * Purpose:	Do noting if pass2 is FALSE on entry.
 *
 *              Otherwise, add the specified flush operation to the 
 *              target instance of test_entry_t.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              9/1/06
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

void
add_flush_op2(int target_type,
	      int target_idx,
	      int op_code,
	      int type,
	      int idx,
	      hbool_t flag,
	      size_t new_size)
{
    int i;
    test_entry_t * target_base_addr;
    test_entry_t * target_entry_ptr;

    HDassert( ( 0 <= target_type ) && ( target_type < NUMBER_OF_ENTRY_TYPES ) );
    HDassert( ( 0 <= target_idx ) && 
	      ( target_idx <= max_indices2[target_type] ) );
    HDassert( ( 0 <= op_code ) && ( op_code <= FLUSH_OP__MAX_OP ) );
    HDassert( ( op_code != FLUSH_OP__RESIZE ) || 
	      ( type == VARIABLE_ENTRY_TYPE ) );
    HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
    HDassert( ( 0 <= idx ) && ( idx <= max_indices2[type] ) );
    HDassert( ( flag == TRUE ) || ( flag == FALSE ) );
    HDassert( new_size <= VARIABLE_ENTRY_SIZE );

    if ( pass2 ) {

        target_base_addr = entries2[target_type];
        target_entry_ptr = &(target_base_addr[target_idx]);

        HDassert( target_entry_ptr->index == target_idx );
        HDassert( target_entry_ptr->type == target_type );
        HDassert( target_entry_ptr == target_entry_ptr->self );
	HDassert( target_entry_ptr->num_flush_ops < MAX_FLUSH_OPS );

	i = (target_entry_ptr->num_flush_ops)++;
	(target_entry_ptr->flush_ops)[i].op_code = op_code;
	(target_entry_ptr->flush_ops)[i].type = type;
	(target_entry_ptr->flush_ops)[i].idx = idx;
	(target_entry_ptr->flush_ops)[i].flag = flag;
	(target_entry_ptr->flush_ops)[i].size = new_size;

    }
        
    return;

} /* add_flush_op2() */


/*-------------------------------------------------------------------------
 * Function:	create_pinned_entry_dependency2
 *
 * Purpose:	Do nothing if pass2 is FALSE on entry.
 *
 *              Otherwise, set up a pinned entry dependency so we can
 *              test the pinned entry modifications to the flush routine.
 *
 *		Given the types and indicies of the pinned and pinning
 *		entries, add the pinned entry to the list of pinned
 *		entries in the pinning entry, increment the
 *		pinning reference count of the pinned entry, and
 *		if that count was zero initially, pin the entry.
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

void
create_pinned_entry_dependency2(H5C2_t * cache_ptr,
		                int pinning_type,
                                int pinning_idx,
	                        int pinned_type,
	                        int pinned_idx)
{
    test_entry_t * pinning_base_addr;
    test_entry_t * pinning_entry_ptr;
    test_entry_t * pinned_base_addr;
    test_entry_t * pinned_entry_ptr;

    if ( pass2 ) {

        HDassert( ( 0 <= pinning_type ) &&
 	          ( pinning_type < NUMBER_OF_ENTRY_TYPES ) );
        HDassert( ( 0 <= pinning_idx ) &&
	          ( pinning_idx <= max_indices2[pinning_type] ) );
        HDassert( ( 0 <= pinned_type ) &&
	          ( pinned_type < NUMBER_OF_ENTRY_TYPES ) );
        HDassert( ( 0 <= pinned_idx ) &&
	          ( pinned_idx <= max_indices2[pinned_type] ) );

        pinning_base_addr = entries2[pinning_type];
        pinning_entry_ptr = &(pinning_base_addr[pinning_idx]);

        pinned_base_addr = entries2[pinned_type];
        pinned_entry_ptr = &(pinned_base_addr[pinned_idx]);

        HDassert( pinning_entry_ptr->index == pinning_idx );
        HDassert( pinning_entry_ptr->type == pinning_type );
        HDassert( pinning_entry_ptr == pinning_entry_ptr->self );
	HDassert( pinning_entry_ptr->num_pins < MAX_PINS );

        HDassert( pinning_entry_ptr->index == pinning_idx );
        HDassert( pinning_entry_ptr->type == pinning_type );
        HDassert( pinning_entry_ptr == pinning_entry_ptr->self );
	HDassert( ! ( pinning_entry_ptr->is_protected ) );

	pinning_entry_ptr->pin_type[pinning_entry_ptr->num_pins] = pinned_type;
	pinning_entry_ptr->pin_idx[pinning_entry_ptr->num_pins] = pinned_idx;
	(pinning_entry_ptr->num_pins)++;

        if ( pinned_entry_ptr->pinning_ref_count == 0 ) {

	    protect_entry2(cache_ptr, pinned_type, pinned_idx);
	    unprotect_entry2(cache_ptr, pinned_type, pinned_idx, FALSE,
		             H5C2__PIN_ENTRY_FLAG);
	}

	(pinned_entry_ptr->pinning_ref_count)++;
    }

    return;

} /* create_pinned_entry_dependency2() */


/*-------------------------------------------------------------------------
 * Function:	dirty_entry2
 *
 * Purpose:	Given a pointer to a cache, an entry type, and an index,
 *		dirty the target entry.  
 *
 *		If the dirty_pin parameter is true, verify that the
 *		target entry is in the cache and is pinned.  If it 
 *		isn't, scream and die.  If it is, use the 
 *		H5C2_mark_pinned_entry_dirty() call to dirty it.
 *
 *		Do nothing if pass2 is false on entry.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/10/04
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

void
dirty_entry2(H5C2_t * cache_ptr,
             int32_t type,
             int32_t idx,
	     hbool_t dirty_pin)
{
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    HDassert( cache_ptr );
    HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
    HDassert( ( 0 <= idx ) && ( idx <= max_indices2[type] ) );

    if ( pass2 ) {

        if ( dirty_pin ) {

	    if ( ! entry_in_cache2(cache_ptr, type, idx) ) {

		pass2 = FALSE;
                failure_mssg2 = "entry to be dirty pinned is not in cache.";

	    } else {

                base_addr = entries2[type];
                entry_ptr = &(base_addr[idx]);

	        HDassert( entry_ptr->index == idx );
	        HDassert( entry_ptr->type == type );
                HDassert( entry_ptr == entry_ptr->self );

		if ( ! ( (entry_ptr->header).is_pinned ) ) {

                    pass2 = FALSE;
                    failure_mssg2 = "entry to be dirty pinned is not pinned.";
		    
                } else {

		    mark_pinned_entry_dirty2(cache_ptr, type, idx, 
				             FALSE, (size_t)0);

		}
	    }
        } else {
        
	    protect_entry2(cache_ptr, type, idx);
            unprotect_entry2(cache_ptr, type, idx, TRUE, H5C2__NO_FLAGS_SET);
	}
    }

    return;

} /* dirty_entry2() */


/*-------------------------------------------------------------------------
 * Function:	execute_flush_op2
 *
 * Purpose:	Given a pointer to an instance of struct flush_op, execute
 * 		it.
 *
 *		Do nothing if pass2 is false on entry.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              9/1/06
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

void
execute_flush_op2(H5C2_t * cache_ptr,
		  struct test_entry_t * entry_ptr,
		  struct flush_op * op_ptr,
		  unsigned * flags_ptr)
{
    const char * fcn_name = "execute_flush_op2()";

    HDassert( cache_ptr != NULL );
    HDassert( cache_ptr->magic == H5C2__H5C2_T_MAGIC );
    HDassert( entry_ptr != NULL );
    HDassert( entry_ptr = entry_ptr->self );
    HDassert( entry_ptr->header.addr == entry_ptr->addr );
    HDassert( ( entry_ptr->flush_op_self_resize_in_progress ) ||
              ( entry_ptr->header.size == entry_ptr->size ) );
    HDassert( op_ptr != NULL );
    HDassert( ( 0 <= entry_ptr->type ) && 
	      ( entry_ptr->type < NUMBER_OF_ENTRY_TYPES ) );
    HDassert( ( 0 <= entry_ptr->index ) && 
              ( entry_ptr->index <= max_indices2[entry_ptr->type] ) );
    HDassert( ( 0 <= op_ptr->type ) && 
              ( op_ptr->type < NUMBER_OF_ENTRY_TYPES ) );
    HDassert( ( 0 <= op_ptr->idx ) && 
              ( op_ptr->idx <= max_indices2[op_ptr->type] ) );
    HDassert( ( op_ptr->flag == FALSE ) || ( op_ptr->flag == TRUE ) );
    HDassert( flags_ptr != NULL );

    if ( pass2 ) {

	switch ( op_ptr->op_code )
	{
	    case FLUSH_OP__NO_OP:
		break;

	    case FLUSH_OP__DIRTY:
		HDassert( ( entry_ptr->type != op_ptr->type ) || 
			  ( entry_ptr->index != op_ptr->idx ) );

		dirty_entry2(cache_ptr, op_ptr->type, op_ptr->idx, 
			     op_ptr->flag);
		break;

            case FLUSH_OP__RESIZE:
		if ( ( entry_ptr->type == op_ptr->type ) && 
                     ( entry_ptr->index == op_ptr->idx ) ) {

                    /* the flush operation is acting on the entry to 
		     * which it is attached.  Handle this here:
		     */
                    HDassert( entry_ptr->type == VARIABLE_ENTRY_TYPE );
		    HDassert( op_ptr->size > 0 );
		    HDassert( op_ptr->size <= VARIABLE_ENTRY_SIZE );

                    entry_ptr->size = op_ptr->size;

		    (*flags_ptr) |= H5C2__SERIALIZE_RESIZED_FLAG;

		    entry_ptr->flush_op_self_resize_in_progress = TRUE;

		} else {

		    /* change the size of some other entry */

		    resize_entry2(cache_ptr, op_ptr->type, op_ptr->idx, 
                                  op_ptr->size, op_ptr->flag);
		}
		break;

	    case FLUSH_OP__RENAME:
		if ( ( entry_ptr->type == op_ptr->type ) && 
                     ( entry_ptr->index == op_ptr->idx ) ) {

                    /* the flush operation is acting on the entry to 
		     * which it is attached.  Handle this here:
		     */

		    HDassert( ((*flags_ptr) & H5C2__SERIALIZE_RESIZED_FLAG) 
                              != 0 );

                    (*flags_ptr) |= H5C2__SERIALIZE_RENAMED_FLAG;

		    if ( op_ptr->flag ) {

                        HDassert( entry_ptr->addr == entry_ptr->alt_addr );
                        entry_ptr->addr = entry_ptr->main_addr;
                        entry_ptr->at_main_addr = TRUE;

                    } else {

                        HDassert( entry_ptr->addr == entry_ptr->main_addr );
                        entry_ptr->addr = entry_ptr->alt_addr;
                        entry_ptr->at_main_addr = FALSE;

                    }

		} else {

		    rename_entry2(cache_ptr, op_ptr->type, op_ptr->idx, 
			          op_ptr->flag);
                }
		break;

	    default:
                pass2 = FALSE;
                failure_mssg2 = "Undefined flush op code.";
		break;
	}
    }

    return;

} /* execute_flush_op2() */


/*-------------------------------------------------------------------------
 * Function:	entry_in_cache2
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
 *		JRM - 10/12/04
 *		Removed references to local_H5C2_t, as we now get direct
 *		access to the definition of H5C2_t via H5Cpkg.h.
 *
 *-------------------------------------------------------------------------
 */

hbool_t
entry_in_cache2(H5C2_t * cache_ptr,
                int32_t type,
                int32_t idx)
{
    hbool_t in_cache = FALSE; /* will set to TRUE if necessary */
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;
    H5C2_cache_entry_t * test_ptr = NULL;

    HDassert( cache_ptr );
    HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
    HDassert( ( 0 <= idx ) && ( idx <= max_indices2[type] ) );

    base_addr = entries2[type];
    entry_ptr = &(base_addr[idx]);

    HDassert( entry_ptr->index == idx );
    HDassert( entry_ptr->type == type );
    HDassert( entry_ptr == entry_ptr->self );

    H5C2__SEARCH_INDEX(cache_ptr, entry_ptr->addr, test_ptr)

    if ( test_ptr != NULL ) {

        in_cache = TRUE;
        HDassert( test_ptr == (H5C2_cache_entry_t *)entry_ptr );
        HDassert( entry_ptr->addr == entry_ptr->header.addr );
    }

    return(in_cache);

} /* entry_in_cache2() */


/*-------------------------------------------------------------------------
 * Function:	reset_entries2
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
 * 		JRM -- 3/31/06
 * 		Added initialization for new pinned entry test related
 * 		fields.
 *
 * 		JRM -- 4/1/07
 * 		Added initialization for the new is_read_only, and 
 * 		ro_ref_count fields.
 *
 * 		JRM -- 9/20/07
 * 		Re-worked function for the cache api mods needed to 
 * 		support journaling.
 *
 *-------------------------------------------------------------------------
 */

void
reset_entries2(void)

{
    int i;
    int j;
    int k;
    int32_t max_index;
    haddr_t addr = PICO_BASE_ADDR;
    haddr_t alt_addr = PICO_ALT_BASE_ADDR;
    size_t entry_size;
    test_entry_t * base_addr;

    for ( i = 0; i < NUMBER_OF_ENTRY_TYPES; i++ )
    {
        entry_size = entry_sizes2[i];
        max_index = max_indices2[i];
        base_addr = entries2[i];

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
            base_addr[j].header.is_read_only = FALSE;
            base_addr[j].header.ro_ref_count = FALSE;
            base_addr[j].header.next = NULL;
            base_addr[j].header.prev = NULL;
            base_addr[j].header.aux_next = NULL;
            base_addr[j].header.aux_prev = NULL;

            base_addr[j].self = &(base_addr[j]);
            base_addr[j].cache_ptr = NULL;
            base_addr[j].written_to_main_addr = FALSE;
            base_addr[j].written_to_alt_addr = FALSE;
            base_addr[j].addr = addr;
            base_addr[j].at_main_addr = TRUE;
            base_addr[j].main_addr = addr;
            base_addr[j].alt_addr = alt_addr;
            base_addr[j].size = entry_size;
            base_addr[j].type = i;
            base_addr[j].index = j;
            base_addr[j].serializes = 0;
            base_addr[j].deserializes = 0;
            base_addr[j].is_dirty = FALSE;
            base_addr[j].is_protected = FALSE;
            base_addr[j].is_read_only = FALSE;
            base_addr[j].ro_ref_count = FALSE;

            base_addr[j].is_pinned = FALSE;
	    base_addr[j].pinning_ref_count = 0;
	    base_addr[j].num_pins = 0;
	    for ( k = 0; k < MAX_PINS; k++ )
            {
	        base_addr[j].pin_type[k] = -1;
		base_addr[j].pin_idx[k] = -1;
	    }

	    base_addr[j].num_flush_ops = 0;
	    for ( k = 0; k < MAX_FLUSH_OPS; k++ )
	    {
		base_addr[j].flush_ops[k].op_code = FLUSH_OP__NO_OP;
		base_addr[j].flush_ops[k].type = -1;
		base_addr[j].flush_ops[k].idx = -1;
		base_addr[j].flush_ops[k].flag = FALSE;
		base_addr[j].flush_ops[k].size = 0;
            }
	    base_addr[j].flush_op_self_resize_in_progress = FALSE;

            base_addr[j].deserialized = FALSE;
            base_addr[j].cleared = FALSE;
            base_addr[j].serialized = FALSE;
            base_addr[j].destroyed = FALSE;

            addr += (haddr_t)entry_size;
            alt_addr += (haddr_t)entry_size;
        }
    }

    return;

} /* reset_entries2() */


/*-------------------------------------------------------------------------
 * Function:	resize_entry2
 *
 * Purpose:	Given a pointer to a cache, an entry type, an index, and
 * 		a size, set the size of the target entry to the size.  Note
 * 		that at present, the type of the entry must be 
 * 		VARIABLE_ENTRY_TYPE.
 *
 *		If the resize_pin parameter is true, verify that the
 *		target entry is in the cache and is pinned.  If it 
 *		isn't, scream and die.  If it is, use the 
 *		H5C2_mark_pinned_entry_dirty() call to resize it.
 *
 *		Do nothing if pass2 is false on entry.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/10/04
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

void
resize_entry2(H5C2_t * cache_ptr,
             int32_t type,
             int32_t idx,
	     size_t new_size,
	     hbool_t resize_pin)
{
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    HDassert( cache_ptr );
    HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
    HDassert( type == VARIABLE_ENTRY_TYPE );
    HDassert( ( 0 <= idx ) && ( idx <= max_indices2[type] ) );
    HDassert( ( 0 < new_size ) && ( new_size <= entry_sizes2[type] ) );

    if ( pass2 ) {

        base_addr = entries2[type];
        entry_ptr = &(base_addr[idx]);

        HDassert( entry_ptr->index == idx );
        HDassert( entry_ptr->type == type );
        HDassert( entry_ptr == entry_ptr->self );

        if ( resize_pin ) {

	    if ( ! entry_in_cache2(cache_ptr, type, idx) ) {

		pass2 = FALSE;
                failure_mssg2 = "entry to be resized pinned is not in cache.";

	    } else {

		if ( ! ( (entry_ptr->header).is_pinned ) ) {

                    pass2 = FALSE;
                    failure_mssg2 = "entry to be resized pinned is not pinned.";
		    
                } else {

		    mark_pinned_entry_dirty2(cache_ptr, type, idx, 
				             TRUE, new_size);
		}
	    }
        } else {
        
	    protect_entry2(cache_ptr, type, idx);
	    unprotect_entry_with_size_change2(cache_ptr, type, idx,
                                              H5C2__SIZE_CHANGED_FLAG, 
					      new_size);
	}
    }

    return;

} /* resize_entry2() */


  /*-------------------------------------------------------------------------
   * Function:    resize_pinned_entry2
   *
   * Purpose:     Given a pointer to a cache, an entry type, an index, and
   *              a new size, change the size of the target pinned entry
   *              to match the supplied new size.
   *
   *              Do nothing if pass is false on entry.
   *
   * Return:      void
   *
   * Programmer:  John Mainzer
   *              1/11/08
   *
   * Modifications:
   *
   *              None.
   *
   *-------------------------------------------------------------------------
   */

void
resize_pinned_entry2(H5C2_t * cache_ptr,
                     int32_t type,
                     int32_t idx,
                     size_t new_size)
{
    herr_t result;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    HDassert( cache_ptr );
    HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
    HDassert( ( 0 <= idx ) && ( idx <= max_indices2[type] ) );
    HDassert( type = VARIABLE_ENTRY_TYPE ) ;
    HDassert( ( 0 < new_size ) && ( new_size <= entry_sizes2[type] ) );

    if ( pass2 ) {

        if ( ! entry_in_cache2(cache_ptr, type, idx) ) {

            pass2 = FALSE;
            failure_mssg2 = "entry not in cache.";

        } else {

            base_addr = entries2[type];
            entry_ptr = &(base_addr[idx]);

            HDassert( entry_ptr->index == idx );
            HDassert( entry_ptr->type == type );
            HDassert( entry_ptr == entry_ptr->self );

            if ( ! ( (entry_ptr->header).is_pinned ) ) {

                pass2 = FALSE;
                failure_mssg2 = "entry to be resized is not pinned.";

            } else {

                entry_ptr->size = new_size;

                result = H5C2_resize_pinned_entry(cache_ptr,
                                                  (void *)entry_ptr,
                                                   new_size);

                if ( result != SUCCEED ) {

                    pass2 = FALSE;
                    failure_mssg2 = "error(s) in H5C2_resize_pinned_entry().";

                } else {

                    HDassert( entry_ptr->size = (entry_ptr->header).size );

                }
            }
        }
    }

    return;

} /* resize_pinned_entry() */


/*-------------------------------------------------------------------------
 * Function:	verify_clean2
 *
 * Purpose:	Verify that all cache entries are marked as clean.  If any
 *		are not, set pass2 to FALSE.
 *
 *		Do nothing if pass2 is FALSE on entry.
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

void
verify_clean2(void)

{
    int i;
    int j;
    int dirty_count = 0;
    int32_t max_index;
    test_entry_t * base_addr;

    if ( pass2 ) {

        for ( i = 0; i < NUMBER_OF_ENTRY_TYPES; i++ )
        {
            max_index = max_indices2[i];
            base_addr = entries2[i];

            HDassert( base_addr );

            for ( j = 0; j <= max_index; j++ )
            {
                if ( ( base_addr[j].header.is_dirty ) || 
		     ( base_addr[j].is_dirty ) ) {

                    dirty_count++;
                }
            }
        }

        if ( dirty_count > 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "verify_clean2() found dirty entry(s).";
        }
    }

    return;

} /* verify_clean2() */


/*-------------------------------------------------------------------------
 * Function:	verify_entry_status2
 *
 * Purpose:	Verify that a list of entries have the expected status.
 * 		If any discrepencies are found, set the failure message
 * 		and set pass2 to FALSE.
 *
 *		Do nothing if pass2 is FALSE on entry.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              10/8/04
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

void
verify_entry_status2(H5C2_t * cache_ptr,
		     int tag,
		     int num_entries,
		     struct expected_entry_status expected[])
{
    const char *   fcn_name = "verify_entry_status2()";
    static char    msg[128];
    hbool_t        in_cache = FALSE; /* will set to TRUE if necessary */
    int            i;
    test_entry_t * entry_ptr;
    test_entry_t * base_addr;

    i = 0;
    while ( ( pass2 ) && ( i < num_entries ) )
    {
        base_addr = entries2[expected[i].entry_type];
	entry_ptr = &(base_addr[expected[i].entry_index]);

	if ( ( ! expected[i].in_cache ) &&
	     ( ( expected[i].is_dirty ) ||
	       ( expected[i].is_protected ) ||
	       ( expected[i].is_pinned ) ) ) {

	    pass2 = FALSE;
	    sprintf(msg, "%d: Contradictory data in expected[%d].\n", tag, i);
	    failure_mssg2 = msg;
	}

        if ( pass2 ) {

	    in_cache = entry_in_cache2(cache_ptr, expected[i].entry_type, 
		                       expected[i].entry_index);

	    if ( in_cache != expected[i].in_cache ) {

	        pass2 = FALSE;
	        sprintf(msg,
		      "%d entry (%d, %d) in cache actual/expected = %d/%d.\n",
		      tag,
		      (int)expected[i].entry_type,
		      (int)expected[i].entry_index,
		      (int)in_cache,
		      (int)expected[i].in_cache);
	        failure_mssg2 = msg;
	    }
	}

        if ( pass2 ) {

	    if ( entry_ptr->size != expected[i].size ) {

	        pass2 = FALSE;
	        sprintf(msg, 
                        "%d entry (%d, %d) size actualexpected = %ld/%ld.\n",
			tag,
	                (int)expected[i].entry_type,
		        (int)expected[i].entry_index,
		        (long)(entry_ptr->size),
		        (long)expected[i].size);
	        failure_mssg2 = msg;
	    }
	}

        if ( ( pass2 ) && ( in_cache ) ) {

	    if ( entry_ptr->header.size != expected[i].size ) {

	        pass2 = FALSE;
	        sprintf(msg, 
                        "%d entry (%d, %d) header size actual/expected = %ld/%ld.\n",
			tag,
		        (int)expected[i].entry_type,
		        (int)expected[i].entry_index,
		        (long)(entry_ptr->header.size),
		        (long)expected[i].size);
	        failure_mssg2 = msg;
	    }
	}

	if ( pass2 ) {

	    if ( entry_ptr->at_main_addr != expected[i].at_main_addr ) {

	        pass2 = FALSE;
	        sprintf(msg, 
                      "%d entry (%d, %d) at main addr actual/expected = %d/%d.\n",
		      tag,
		      (int)expected[i].entry_type,
		      (int)expected[i].entry_index,
		      (int)(entry_ptr->at_main_addr),
		      (int)expected[i].at_main_addr);
	        failure_mssg2 = msg;
	    }
	}

	if ( pass2 ) {

	    if ( entry_ptr->is_dirty != expected[i].is_dirty ) {

	        pass2 = FALSE;
	        sprintf(msg, 
                      "%d entry (%d, %d) is_dirty actual/expected = %d/%d.\n",
		      tag,
		      (int)expected[i].entry_type,
		      (int)expected[i].entry_index,
		      (int)(entry_ptr->is_dirty),
		      (int)expected[i].is_dirty);
	        failure_mssg2 = msg;
	    }
	}

	if ( ( pass2 ) && ( in_cache ) ) {

	    if ( entry_ptr->header.is_dirty != expected[i].is_dirty ) {

	        pass2 = FALSE;
	        sprintf(msg, 
                      "%d entry (%d, %d) header is_dirty actual/expected = %d/%d.\n",
		      tag,
		      (int)expected[i].entry_type,
		      (int)expected[i].entry_index,
		      (int)(entry_ptr->header.is_dirty),
		      (int)expected[i].is_dirty);
	        failure_mssg2 = msg;
	    }
	}

	if ( pass2 ) {

	    if ( entry_ptr->is_protected != expected[i].is_protected ) {

	        pass2 = FALSE;
	        sprintf(msg, 
                      "%d entry (%d, %d) is_protected actual/expected = %d/%d.\n",
		      tag,
		      (int)expected[i].entry_type,
		      (int)expected[i].entry_index,
		      (int)(entry_ptr->is_protected),
		      (int)expected[i].is_protected);
	        failure_mssg2 = msg;
	    }
	}

	if ( ( pass2 ) && ( in_cache ) ) {

	    if ( entry_ptr->header.is_protected != expected[i].is_protected ) {

	        pass2 = FALSE;
	        sprintf(msg, 
                      "%d entry (%d, %d) header is_protected actual/expected = %d/%d.\n",
		      tag,
		      (int)expected[i].entry_type,
		      (int)expected[i].entry_index,
		      (int)(entry_ptr->header.is_protected),
		      (int)expected[i].is_protected);
	        failure_mssg2 = msg;
	    }
	}

	if ( pass2 ) {

	    if ( entry_ptr->is_pinned != expected[i].is_pinned ) {

	        pass2 = FALSE;
	        sprintf(msg, 
                      "%d entry (%d, %d) is_pinned actual/expected = %d/%d.\n",
		      tag,
		      (int)expected[i].entry_type,
		      (int)expected[i].entry_index,
		      (int)(entry_ptr->is_pinned),
		      (int)expected[i].is_pinned);
	        failure_mssg2 = msg;
	    }
	}

	if ( ( pass2 ) && ( in_cache ) ) {

	    if ( entry_ptr->header.is_pinned != expected[i].is_pinned ) {

	        pass2 = FALSE;
	        sprintf(msg, 
                  "%d entry (%d, %d) header is_pinned actual/expected = %d/%d.\n",
		  tag,
		  (int)expected[i].entry_type,
		  (int)expected[i].entry_index,
		  (int)(entry_ptr->header.is_pinned),
		  (int)expected[i].is_pinned);
	        failure_mssg2 = msg;
	    }
	}

	if ( pass2 ) {

            if ( ( entry_ptr->deserialized != expected[i].deserialized ) ||
	         ( entry_ptr->cleared != expected[i].cleared ) ||
	         ( entry_ptr->serialized != expected[i].serialized ) ||
	         ( entry_ptr->destroyed != expected[i].destroyed ) ) {

	        pass2 = FALSE;
                sprintf(msg,
                        "%d entry (%d,%d) deserialized = %d(%d), clrd = %d(%d), serialized = %d(%d), dest = %d(%d)\n",
			tag,
		        (int)expected[i].entry_type,
		        (int)expected[i].entry_index,
		        (int)(entry_ptr->deserialized),
		        (int)(expected[i].deserialized),
		        (int)(entry_ptr->cleared),
		        (int)(expected[i].cleared),
		        (int)(entry_ptr->serialized),
		        (int)(expected[i].serialized),
		        (int)(entry_ptr->destroyed),
		        (int)(expected[i].destroyed));
                failure_mssg2 = msg;
            }
        }
	i++;
    } /* while */

    return;

} /* verify_entry_status2() */


/*-------------------------------------------------------------------------
 * Function:	verify_unprotected2
 *
 * Purpose:	Verify that no cache entries are marked as protected.  If
 *		any are, set pass2 to FALSE.
 *
 *		Do nothing if pass2 is FALSE on entry.
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

void
verify_unprotected2(void)

{
    int i;
    int j;
    int protected_count = 0;
    int32_t max_index;
    test_entry_t * base_addr;

    if ( pass2 ) {

        for ( i = 0; i < NUMBER_OF_ENTRY_TYPES; i++ )
        {
            max_index = max_indices2[i];
            base_addr = entries2[i];

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

            pass2 = FALSE;
            failure_mssg2 = "verify_unprotected2() found protected entry(s).";
        }
    }

    return;

} /* verify_unprotected2() */


/*****************************************************************************
 *
 * Function:    setup_cache2()
 *
 * Purpose:     Open an HDF file.  This will allocate an instance and 
 * 		initialize an associated instance of H5C2_t.  However, 
 * 		we want to test an instance of H5C2_t, so allocate and 
 * 		initialize one with the file ID returned by the call to
 * 		H5Fcreate().  Return a pointer to this instance of H5C2_t.
 *
 *		Observe that we open a HDF file because the cache now
 *		writes directly to file, and we need the file I/O facilities
 *		associated with the file.
 *
 *		To avoid tripping on error check code, must allocate enough
 *		space in the file to hold all the test entries and their
 *		alternates.  This is a little sticky, as the addresses of 
 *		all the test entries are determined at compile time.
 *
 *		Deal with this by choosing BASE_ADDR large enough that 
 *		the base address of the allocate space will be less than
 *		or equal to BASE_ADDR, and then requesting an extra BASE_ADDR
 *		bytes, so we don't have to wory about exceeding the allocation.
 *
 * Return:      Success:        Ptr to H5C2_t
 *
 *              Failure:        NULL
 *
 * Programmer:  JRM -- 9/13/07
 *
 * Modifications:
 *
 *              None.
 *
 *****************************************************************************/

H5C2_t *
setup_cache2(size_t max_cache_size,
             size_t min_clean_size)
{
    const char * fcn_name = "setup_cache2()";
    char filename[512];
    hbool_t show_progress = FALSE;
    hbool_t verbose = TRUE;
    int mile_stone = 1;
    hid_t fid = -1;
    H5F_t * file_ptr = NULL;
    H5C2_t * cache_ptr = NULL;
    H5C2_t * ret_val = NULL;
    haddr_t actual_base_addr;
    hid_t fapl_id = H5P_DEFAULT;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

#if 0 /* This debugging code is useful from time to time -- keep it for now */
    HDfprintf(stdout, "PICO_BASE_ADDR = 0x%lx, PICO_ALT_BASE_ADDR = 0x%lx.\n",
              (long)PICO_BASE_ADDR, (long)PICO_ALT_BASE_ADDR);
    HDfprintf(stdout, "NANO_BASE_ADDR = 0x%lx, NANO_ALT_BASE_ADDR = 0x%lx.\n",
              (long)NANO_BASE_ADDR, (long)NANO_ALT_BASE_ADDR);
    HDfprintf(stdout, 
              "MICRO_BASE_ADDR = 0x%lx, MICRO_ALT_BASE_ADDR = 0x%lx.\n",
              (long)MICRO_BASE_ADDR, (long)MICRO_ALT_BASE_ADDR);
    HDfprintf(stdout, "TINY_BASE_ADDR = 0x%lx, TINY_ALT_BASE_ADDR = 0x%lx.\n",
              (long)TINY_BASE_ADDR, (long)TINY_ALT_BASE_ADDR);
    HDfprintf(stdout, 
	      "SMALL_BASE_ADDR = 0x%lx, SMALL_ALT_BASE_ADDR = 0x%lx.\n",
              (long)SMALL_BASE_ADDR, (long)SMALL_ALT_BASE_ADDR);
    HDfprintf(stdout, 
	      "MEDIUM_BASE_ADDR = 0x%lx, MEDIUM_ALT_BASE_ADDR = 0x%lx.\n",
              (long)MEDIUM_BASE_ADDR, (long)MEDIUM_ALT_BASE_ADDR);
    HDfprintf(stdout, 
	      "LARGE_BASE_ADDR = 0x%lx, LARGE_ALT_BASE_ADDR = 0x%lx.\n",
              (long)LARGE_BASE_ADDR, (long)LARGE_ALT_BASE_ADDR);
    HDfprintf(stdout, "HUGE_BASE_ADDR = 0x%lx, HUGE_ALT_BASE_ADDR = 0x%lx.\n",
              (long)HUGE_BASE_ADDR, (long)HUGE_ALT_BASE_ADDR);
    HDfprintf(stdout, 
	      "MONSTER_BASE_ADDR = 0x%lx, MONSTER_ALT_BASE_ADDR = 0x%lx.\n",
              (long)MONSTER_BASE_ADDR, (long)MONSTER_ALT_BASE_ADDR);
    HDfprintf(stdout, 
	      "VARIABLE_BASE_ADDR = 0x%lx, VARIABLE_ALT_BASE_ADDR = 0x%lx.\n",
              (long)VARIABLE_BASE_ADDR, (long)VARIABLE_ALT_BASE_ADDR);
#endif /* JRM */

    /* setup the file name */
    if ( pass2 ) {

        if ( h5_fixname(FILENAME[0], H5P_DEFAULT, filename, sizeof(filename))
            == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "h5_fixname() failed.\n";
        }
    }

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

#if USE_CORE_DRIVER
    if ( pass2 ) {

	if ( (fapl_id = H5Pcreate(H5P_FILE_ACCESS)) == FAIL ) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5Pcreate(H5P_FILE_ACCESS) failed.\n";
        }
	else if ( H5Pset_fapl_core(fapl_id, 64 * 1024 * 1024, FALSE) < 0 ) {

	    pass2 = FALSE;
	    failure_mssg2 = "H5P_set_fapl_core() failed.\n";
        }
    }
#endif /* USE_CORE_DRIVER */

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    if ( pass2 ) {

        fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);

        if ( fid < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Fcreate() failed.";
        
            if ( verbose ) {
                HDfprintf(stdout, "%s: H5Fcreate() failed.\n", fcn_name);
            }

        } else if ( H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "H5Fflush() failed.";
        
            if ( verbose ) {
                HDfprintf(stdout, "%s: H5Fflush() failed.\n", fcn_name);
            }

        } else {

	    saved_fid = fid;
            file_ptr = H5I_object_verify(fid, H5I_FILE);

	    if ( file_ptr == NULL ) {

                pass2 = FALSE;
                failure_mssg2 = "Can't get file_ptr.";
        
                if ( verbose ) {
                    HDfprintf(stdout, "%s: H5Fflush() failed.\n", fcn_name);
                }
	    }
        }
    }

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    if ( pass2 ) {

        cache_ptr = H5C2_create(file_ptr,
			        max_cache_size,
                                min_clean_size,
                                (NUMBER_OF_ENTRY_TYPES - 1),
				(const char **)entry_type_names2,
                                check_write_permitted2,
                                TRUE,
                                NULL,
                                NULL);
    }

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    if ( pass2 ) {
	    
	if ( cache_ptr == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "H5C2_create() failed.";

            if ( verbose ) {
                 HDfprintf(stdout, "%s: H5C2_create() failed.\n", fcn_name);
            }

        } else if ( cache_ptr->magic != H5C2__H5C2_T_MAGIC ) {

            pass2 = FALSE;
	    failure_mssg2 = "Bad cache_ptr magic.";

            if ( verbose ) {
                HDfprintf(stdout, "%s: Bad cache_ptr magic.\n", fcn_name);
            }
	}
    }

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    if ( pass2 ) { /* allocate space for test entries */

        actual_base_addr = H5MF_alloc(file_ptr, H5FD_MEM_DEFAULT, H5P_DEFAULT, 
			              (hsize_t)(ADDR_SPACE_SIZE + BASE_ADDR));

	if ( actual_base_addr == HADDR_UNDEF ) {

            pass2 = FALSE;
	    failure_mssg2 = "H5MF_alloc() failed.";
	    
	    if ( verbose ) {
                HDfprintf(stdout, "%s: H5MF_alloc() failed.\n", fcn_name);
            }

	} else if ( actual_base_addr > BASE_ADDR ) {

	    /* If this happens, must increase BASE_ADDR so that the
	     * actual_base_addr is <= BASE_ADDR.  This should only happen
	     * if the size of the superblock is increase.
	     */
            pass2 = FALSE;
	    failure_mssg2 = "actual_base_addr > BASE_ADDR";

	    if ( verbose ) {
                HDfprintf(stdout, "%s: actual_base_addr > BASE_ADDR.\n", 
			  fcn_name);
            }
        }
    }

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    if ( pass2 ) {

        H5C2_stats__reset(cache_ptr);
        ret_val = cache_ptr;
    }

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass2 = %d\n",
                  fcn_name, mile_stone++, (int)pass2);

    return(ret_val);

} /* setup_cache2() */


/*-------------------------------------------------------------------------
 * Function:	takedown_cache2()
 *
 * Purpose:	Flush the specified cache and destroy it.  If requested,
 *		dump stats first.  Then close and delete the associate
 *		file.
 *
 *		If pass2 is FALSE, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              9/14/07
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

void
takedown_cache2(H5C2_t * cache_ptr,
                hbool_t dump_stats,
                hbool_t dump_detailed_stats)
{
    char filename[512];

    if ( cache_ptr != NULL ) {

        if ( dump_stats ) {

            H5C2_stats(cache_ptr, "test cache", dump_detailed_stats);
        }
	
        flush_cache2(cache_ptr, TRUE, FALSE, FALSE);

        H5C2_dest(cache_ptr, H5P_DATASET_XFER_DEFAULT);

    }

    if ( saved_fid != -1 ) {

	if ( H5Fclose(saved_fid) < 0  ) {

            pass2 = FALSE;
	    failure_mssg2 = "couldn't close test file.";

	} else {

	    saved_fid = -1;

        }
#if ! USE_CORE_DRIVER
        if ( h5_fixname(FILENAME[0], H5P_DEFAULT, filename, sizeof(filename))
            == NULL ) {

            pass2 = FALSE;
            failure_mssg2 = "h5_fixname() failed.\n";
        }

        if ( HDremove(filename) < 0 ) {

            pass2 = FALSE;
	    failure_mssg2 = "couldn't delete test file.";

	}
#endif /* USE_CORE_CRIVER */
    }

    return;

} /* takedown_cache2() */


/*-------------------------------------------------------------------------
 * Function:	expunge_entry2()
 *
 * Purpose:	Expunge the entry indicated by the type and index.
 *
 *		Do nothing if pass2 is FALSE on entry.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              7/6/06
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

void
expunge_entry2(H5C2_t * cache_ptr,
              int32_t type,
              int32_t idx)
{
    /* const char * fcn_name = "expunge_entry2()"; */
    herr_t result;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    if ( pass2 ) {

        HDassert( cache_ptr );
        HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
        HDassert( ( 0 <= idx ) && ( idx <= max_indices2[type] ) );

        base_addr = entries2[type];
        entry_ptr = &(base_addr[idx]);

        HDassert( entry_ptr->index == idx );
        HDassert( entry_ptr->type == type );
        HDassert( entry_ptr == entry_ptr->self );
	HDassert( entry_ptr->cache_ptr == cache_ptr );
        HDassert( ! ( entry_ptr->header.is_protected ) );
        HDassert( ! ( entry_ptr->is_protected ) );
        HDassert( ! ( entry_ptr->header.is_pinned ) );
	HDassert( ! ( entry_ptr->is_pinned ) );

        result = H5C2_expunge_entry(cache_ptr, H5P_DATASET_XFER_DEFAULT, 
			            &(types2[type]),
                                    entry_ptr->addr);

        if ( result < 0 ) {

            pass2 = FALSE;
            failure_mssg2 = "error in H5C2_expunge_entry().";

        }
    }

    return;

} /* expunge_entry2() */


/*-------------------------------------------------------------------------
 * Function:	flush_cache2()
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

void
flush_cache2(H5C2_t * cache_ptr,
             hbool_t destroy_entries,
             hbool_t dump_stats,
             hbool_t dump_detailed_stats)
{
    const char * fcn_name = "flush_cache2()";
    hbool_t show_progress = FALSE;
    herr_t result = 0;
    int mile_post = 0;

    HDassert(cache_ptr);

    if ( show_progress ) {
	HDfprintf(stdout, "%s: mile_post = %d.\n", 
		  fcn_name, mile_post++); /* 0 */
    }

    verify_unprotected2();

    if ( show_progress ) {
	HDfprintf(stdout, "%s: mile_post = %d.\n", 
		  fcn_name, mile_post++); /* 1 */
    }

    if ( pass2 ) {

        if ( destroy_entries ) {

            result = H5C2_flush_cache(cache_ptr, H5P_DATASET_XFER_DEFAULT,
                                     H5C2__FLUSH_INVALIDATE_FLAG);

        } else {

            result = H5C2_flush_cache(cache_ptr, H5P_DATASET_XFER_DEFAULT,
                                      H5C2__NO_FLAGS_SET);
        }
    }

    if ( show_progress ) {
	HDfprintf(stdout, "%s: mile_post = %d.\n", 
		  fcn_name, mile_post++); /* 2 */
    }

    if ( dump_stats ) {

        H5C2_stats(cache_ptr, "test cache", dump_detailed_stats);
    }

    if ( show_progress ) {
	HDfprintf(stdout, "%s: mile_post = %d.\n", 
		  fcn_name, mile_post++); /* 3 */
    }

    if ( result < 0 ) {

        pass2 = FALSE;
        failure_mssg2 = "error in H5C2_flush_cache().";
    }

    if ( show_progress ) {
	HDfprintf(stdout, "%s: mile_post = %d.\n", 
		  fcn_name, mile_post++); /* 4 */
    }

    return;

} /* flush_cache2() */


/*-------------------------------------------------------------------------
 * Function:	insert_entry2()
 *
 * Purpose:	Insert the entry indicated by the type and index.  Mark
 *		it clean or dirty as indicated.
 *
 *		Note that I don't see much practical use for inserting
 *		a clean entry, but the interface permits it so we should
 *		test it.
 *
 *		Do nothing if pass2 is false.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/16/04
 *
 * Modifications:
 *
 *		JRM -- 1/13/05
 *		Updated function for the flags parameter in
 *		H5C2_insert_entry(), and to allow access to this parameter.
 *
 *		JRM -- 6/17/05
 *		The interface no longer permits clean inserts.
 *		Accordingly, the dirty parameter is no longer meaningfull.
 *
 *		JRM -- 4/5/06
 *		Added code to initialize the new cache_ptr field of the
 *		test_entry_t structure.
 *
 *		JRM -- 8/10/06
 *		Updated to reflect the fact that entries can now be
 *		inserted pinned.
 *
 *-------------------------------------------------------------------------
 */

void
insert_entry2(H5C2_t * cache_ptr,
             int32_t type,
             int32_t idx,
             hbool_t UNUSED dirty,
             unsigned int flags)
{
    herr_t result;
    hbool_t insert_pinned;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    if ( pass2 ) {

        HDassert( cache_ptr );
        HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
        HDassert( ( 0 <= idx ) && ( idx <= max_indices2[type] ) );

        base_addr = entries2[type];
        entry_ptr = &(base_addr[idx]);

        HDassert( entry_ptr->index == idx );
        HDassert( entry_ptr->type == type );
        HDassert( entry_ptr == entry_ptr->self );
        HDassert( !(entry_ptr->is_protected) );

	insert_pinned = ((flags & H5C2__PIN_ENTRY_FLAG) != 0 );

	entry_ptr->is_dirty = TRUE;

        result = H5C2_insert_entry(cache_ptr, H5P_DATASET_XFER_DEFAULT, 
			           &(types2[type]), entry_ptr->addr, 
				   entry_ptr->size, (void *)entry_ptr, flags);

        if ( ( result < 0 ) ||
             ( entry_ptr->header.is_protected ) ||
             ( entry_ptr->header.type != &(types2[type]) ) ||
             ( entry_ptr->size != entry_ptr->header.size ) ||
             ( entry_ptr->addr != entry_ptr->header.addr ) ) {

            pass2 = FALSE;
            failure_mssg2 = "error in H5C2_insert().";

#if 0 /* This is useful debugging code.  Lets keep it around. */

            HDfprintf(stdout, "result = %d\n", (int)result);
            HDfprintf(stdout, "entry_ptr->header.is_protected = %d\n",
                      (int)(entry_ptr->header.is_protected));
            HDfprintf(stdout,
		      "entry_ptr->header.type != &(types2[type]) = %d\n",
                      (int)(entry_ptr->header.type != &(types2[type])));
            HDfprintf(stdout,
                      "entry_ptr->size != entry_ptr->header.size = %d\n",
                      (int)(entry_ptr->size != entry_ptr->header.size));
            HDfprintf(stdout,
                      "entry_ptr->addr != entry_ptr->header.addr = %d\n",
                       (int)(entry_ptr->addr != entry_ptr->header.addr));
#endif
        }
	HDassert( entry_ptr->cache_ptr == NULL );

        entry_ptr->cache_ptr = cache_ptr;

	if ( insert_pinned ) {

	    HDassert( entry_ptr->header.is_pinned );
	    entry_ptr->is_pinned = TRUE;

	} else {

	    HDassert( ! ( entry_ptr->header.is_pinned ) );
	    entry_ptr->is_pinned = FALSE;

	}
        HDassert( entry_ptr->header.is_dirty );
        HDassert( ((entry_ptr->header).type)->id == type );
    }

    return;

} /* insert_entry2() */


/*-------------------------------------------------------------------------
 * Function:	mark_pinned_entry_dirty2()
 *
 * Purpose:	Mark the specified entry as dirty.
 *
 *		Do nothing if pass2 is FALSE on entry.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              3/28/06
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

void
mark_pinned_entry_dirty2(H5C2_t * cache_ptr,
                         int32_t type,
                         int32_t idx,
		 	 hbool_t size_changed,
			 size_t  new_size)
{
    /* const char * fcn_name = "mark_pinned_entry_dirty2()"; */
    herr_t result;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    if ( pass2 ) {

        HDassert( cache_ptr );
        HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
        HDassert( ( 0 <= idx ) && ( idx <= max_indices2[type] ) );

        base_addr = entries2[type];
        entry_ptr = &(base_addr[idx]);

        HDassert( entry_ptr->index == idx );
        HDassert( entry_ptr->type == type );
        HDassert( entry_ptr == entry_ptr->self );
	HDassert( entry_ptr->cache_ptr == cache_ptr );
        HDassert( ! (entry_ptr->header.is_protected) );
        HDassert( entry_ptr->header.is_pinned );
	HDassert( entry_ptr->is_pinned );

	entry_ptr->is_dirty = TRUE;

        if ( size_changed ) {

            /* update entry size now to keep the sanity checks happy */
            entry_ptr->size = new_size;
        }

        result = H5C2_mark_pinned_entry_dirty(cache_ptr,
		 	                      (void *)entry_ptr,
				 	      size_changed,
					      new_size);

        if ( ( result < 0 ) ||
             ( ! (entry_ptr->header.is_dirty) ) ||
             ( ! (entry_ptr->header.is_pinned) ) ||
             ( entry_ptr->header.type != &(types2[type]) ) ||
             ( entry_ptr->size != entry_ptr->header.size ) ||
             ( entry_ptr->addr != entry_ptr->header.addr ) ) {

#if 0 /* This is useful debugging code -- keep it around  */
            HDfprintf(stdout, "result = %ld.\n", (long)result);
            HDfprintf(stdout, "entry_ptr->header.is_dirty = %d.\n",
                      (int)(entry_ptr->header.is_dirty));
            HDfprintf(stdout, "entry_ptr->header.is_pinned = %d.\n",
                      (int)(entry_ptr->header.is_pinned));
            HDfprintf(stdout,
                      "(entry_ptr->header.type != &(types[type])) = %d.\n",
                      (int)(entry_ptr->header.type != &(types[type])));
            HDfprintf(stdout,
                      "entry_ptr->size = %ld, entry_ptr->header.size = %ld.\n",
                      (long)(entry_ptr->size), (long)(entry_ptr->header.size));
            HDfprintf(stdout,
                      "entry_ptr->addr = %ld, entry_ptr->header.addr = %ld.\n",
                      (long)(entry_ptr->addr), (long)(entry_ptr->header.addr));
#endif
            pass2 = FALSE;
            failure_mssg2 = "error in H5C2_mark_pinned_entry_dirty().";

        }

        HDassert( ((entry_ptr->header).type)->id == type );

    }

    return;

} /* mark_pinned_entry_dirty2() */


/*-------------------------------------------------------------------------
 * Function:	mark_pinned_or_protected_entry_dirty2()
 *
 * Purpose:	Mark the specified entry as dirty.
 *
 *		Do nothing if pass2 is FALSE on entry.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              5/17/06
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

void
mark_pinned_or_protected_entry_dirty2(H5C2_t * cache_ptr,
                                     int32_t type,
                                     int32_t idx)
{
    /* const char * fcn_name = "mark_pinned_or_protected_entry_dirty2()"; */
    herr_t result;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    if ( pass2 ) {

        HDassert( cache_ptr );
        HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
        HDassert( ( 0 <= idx ) && ( idx <= max_indices2[type] ) );

        base_addr = entries2[type];
        entry_ptr = &(base_addr[idx]);

        HDassert( entry_ptr->index == idx );
        HDassert( entry_ptr->type == type );
        HDassert( entry_ptr == entry_ptr->self );
	HDassert( entry_ptr->cache_ptr == cache_ptr );
        HDassert( entry_ptr->header.is_protected ||
		  entry_ptr->header.is_pinned );

	entry_ptr->is_dirty = TRUE;

        result = H5C2_mark_pinned_or_protected_entry_dirty(cache_ptr,
		 	                                   (void *)entry_ptr);

        if ( ( result < 0 )
	     ||
	     ( ( ! (entry_ptr->header.is_protected) )
	       &&
	       ( ! (entry_ptr->header.is_pinned) )
	     )
	     ||
             ( ( entry_ptr->header.is_protected )
	       &&
	       ( ! ( entry_ptr->header.dirtied ) )
	     )
	     ||
             ( ( ! ( entry_ptr->header.is_protected ) )
	       &&
	       ( ! ( entry_ptr->header.is_dirty ) )
	     )
	     ||
             ( entry_ptr->header.type != &(types2[type]) )
	     ||
             ( entry_ptr->size != entry_ptr->header.size )
	     ||
             ( entry_ptr->addr != entry_ptr->header.addr ) ) {

            pass2 = FALSE;
            failure_mssg2 =
                "error in H5C2_mark_pinned_or_protected_entry_dirty().";

        }

        HDassert( ((entry_ptr->header).type)->id == type );

    }

    return;

} /* mark_pinned_or_protected_entry_dirty2() */


/*-------------------------------------------------------------------------
 * Function:	rename_entry2()
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
 *		JRM -- 6/17/05
 *		Updated code to reflect the fact that renames automatically
 *		dirty entries.
 *
 *-------------------------------------------------------------------------
 */

void
rename_entry2(H5C2_t * cache_ptr,
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
    HDassert( ( 0 <= idx ) && ( idx <= max_indices2[type] ) );

    base_addr = entries2[type];
    entry_ptr = &(base_addr[idx]);

    HDassert( entry_ptr->index == idx );
    HDassert( entry_ptr->type == type );
    HDassert( entry_ptr == entry_ptr->self );
    HDassert( entry_ptr->cache_ptr == cache_ptr );
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

        entry_ptr->is_dirty = TRUE;

        result = H5C2_rename_entry(cache_ptr, &(types2[type]),
                                   old_addr, new_addr);
    }

    if ( ! done ) {

        if ( ( result < 0 ) || 
	     ( ( ! ( entry_ptr->header.destroy_in_progress ) ) &&
	       ( entry_ptr->header.addr != new_addr ) ) ) {

            pass2 = FALSE;
            failure_mssg2 = "error in H5C2_rename_entry().";

        } else {

            entry_ptr->addr = new_addr;
            entry_ptr->at_main_addr = main_addr;
        }
    }

    HDassert( ((entry_ptr->header).type)->id == type );

    HDassert( entry_ptr->header.is_dirty );
    HDassert( entry_ptr->is_dirty );

    return;

} /* rename_entry2() */


/*-------------------------------------------------------------------------
 * Function:	protect_entry2()
 *
 * Purpose:	Protect the entry indicated by the type and index.
 *
 *		Do nothing if pass2 is FALSE on entry.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/11/04
 *
 * Modifications:
 *	
 *    - Modified call to H5C2_protect to pass H5C2__NO_FLAGS_SET in the 
 *      new flags parameter.
 *    						JRM -- 3/28/07
 *
 *-------------------------------------------------------------------------
 */

void
protect_entry2(H5C2_t * cache_ptr,
               int32_t type,
               int32_t idx)
{
    const char * fcn_name = "protect_entry2()";
    hbool_t verbose = FALSE;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;
    H5C2_cache_entry_t * cache_entry_ptr;

    if ( verbose ) {
	HDfprintf(stdout, "\n%s: entering. type = %d, idx = %d.\n",
		  fcn_name, type, idx);
    }

    if ( pass2 ) {

        HDassert( cache_ptr );
        HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
        HDassert( ( 0 <= idx ) && ( idx <= max_indices2[type] ) );

        base_addr = entries2[type];
        entry_ptr = &(base_addr[idx]);

        HDassert( entry_ptr->index == idx );
        HDassert( entry_ptr->type == type );
        HDassert( entry_ptr == entry_ptr->self );
        HDassert( !(entry_ptr->is_protected) );

	if ( verbose ) {
	    HDfprintf(stdout, 
		      "%s: calling H5C2_protect(). addr = 0x%lx, len = %ld.\n",
		      fcn_name, (long)(entry_ptr->addr), 
		      (long)(entry_ptr->size));
	}

        cache_entry_ptr = H5C2_protect(cache_ptr, H5P_DATASET_XFER_DEFAULT, 
			               &(types2[type]), entry_ptr->addr, 
				       entry_ptr->size, NULL, 
				       H5C2__NO_FLAGS_SET);

	if ( verbose ) {
	    HDfprintf(stdout, 
		      "%s: H5C2_protect() returns. addr = 0x%lx, len = %ld.\n",
		      fcn_name, (long)(entry_ptr->addr), 
		      (long)(entry_ptr->size));
	}

        if ( ( cache_entry_ptr != (void *)entry_ptr ) ||
             ( !(entry_ptr->header.is_protected) ) ||
             ( entry_ptr->header.type != &(types2[type]) ) ||
             ( entry_ptr->size != entry_ptr->header.size ) ||
             ( entry_ptr->addr != entry_ptr->header.addr ) ) {

#if 0
            /* I've written the following debugging code several times
             * now.  Lets keep it around so I don't have to write it
             * again.
             *                              - JRM
             */
            HDfprintf(stdout, "( cache_entry_ptr != (void *)entry_ptr ) = %d\n",
                      (int)( cache_entry_ptr != (void *)entry_ptr ));
            HDfprintf(stdout, "cache_entry_ptr = 0x%lx, entry_ptr = 0x%lx\n",
                      (long)cache_entry_ptr, (long)entry_ptr);
            HDfprintf(stdout, "entry_ptr->header.is_protected = %d\n",
                      (int)(entry_ptr->header.is_protected));
            HDfprintf(stdout,
                      "( entry_ptr->header.type != &(types2[type]) ) = %d\n",
                      (int)( entry_ptr->header.type != &(types2[type]) ));
            HDfprintf(stdout,
                      "entry_ptr->size = %d, entry_ptr->header.size = %d\n",
                      (int)(entry_ptr->size), (int)(entry_ptr->header.size));
            HDfprintf(stdout,
                      "entry_ptr->addr = %d, entry_ptr->header.addr = %d\n",
                      (int)(entry_ptr->addr), (int)(entry_ptr->header.addr));
#endif
            pass2 = FALSE;
            failure_mssg2 = "error in H5C2_protect().";

        } else {

	    HDassert( ( entry_ptr->cache_ptr == NULL ) ||
		      ( entry_ptr->cache_ptr == cache_ptr ) );

	    entry_ptr->cache_ptr = cache_ptr;
            entry_ptr->is_protected = TRUE;

        }

        HDassert( ((entry_ptr->header).type)->id == type );
    }

    if ( verbose ) {
	HDfprintf(stdout, "%s: exiting.\n", fcn_name);
    }

    return;

} /* protect_entry2() */


/*-------------------------------------------------------------------------
 * Function:	protect_entry_ro2()
 *
 * Purpose:	Do a read only protect the entry indicated by the type 
 * 		and index.
 *
 *		Do nothing if pass2 is FALSE on entry.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              4/1/07
 *
 * Modifications:
 *	
 *    - None.
 *
 *-------------------------------------------------------------------------
 */

void
protect_entry_ro2(H5C2_t * cache_ptr,
                  int32_t type,
                  int32_t idx)
{
    /* const char * fcn_name = "protect_entry_ro2()"; */
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;
    H5C2_cache_entry_t * cache_entry_ptr;

    if ( pass2 ) {

        HDassert( cache_ptr );
        HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
        HDassert( ( 0 <= idx ) && ( idx <= max_indices2[type] ) );

        base_addr = entries2[type];
        entry_ptr = &(base_addr[idx]);

        HDassert( entry_ptr->index == idx );
        HDassert( entry_ptr->type == type );
        HDassert( entry_ptr == entry_ptr->self );
        HDassert( ( ! ( entry_ptr->is_protected ) ) || 
		  ( ( entry_ptr->is_read_only ) && 
		    ( entry_ptr->ro_ref_count > 0 ) ) );

        cache_entry_ptr = H5C2_protect(cache_ptr, H5P_DATASET_XFER_DEFAULT, 
			               &(types2[type]), entry_ptr->addr, 
				       entry_ptr->size, NULL,
				       H5C2__READ_ONLY_FLAG);

        if ( ( cache_entry_ptr != (void *)entry_ptr ) ||
             ( !(entry_ptr->header.is_protected) ) ||
             ( !(entry_ptr->header.is_read_only) ) ||
             ( entry_ptr->header.ro_ref_count <= 0 ) ||
             ( entry_ptr->header.type != &(types2[type]) ) ||
             ( entry_ptr->size != entry_ptr->header.size ) ||
             ( entry_ptr->addr != entry_ptr->header.addr ) ) {

            pass2 = FALSE;
            failure_mssg2 = "error in read only H5C2_protect().";

        } else {

	    HDassert( ( entry_ptr->cache_ptr == NULL ) ||
		      ( entry_ptr->cache_ptr == cache_ptr ) );

	    entry_ptr->cache_ptr = cache_ptr;
            entry_ptr->is_protected = TRUE;
	    entry_ptr->is_read_only = TRUE;
	    entry_ptr->ro_ref_count++;
        }

        HDassert( ((entry_ptr->header).type)->id == type );
    }

    return;

} /* protect_entry_ro2() */


/*-------------------------------------------------------------------------
 * Function:	unpin_entry2()
 *
 * Purpose:	Unpin the entry indicated by the type and index.
 *
 *		Do nothing if pass2 is FALSE on entry.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              3/28/06
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

void
unpin_entry2(H5C2_t * cache_ptr,
             int32_t type,
             int32_t idx)
{
    /* const char * fcn_name = "unpin_entry2()"; */
    herr_t result;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    if ( pass2 ) {

        HDassert( cache_ptr );
        HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
        HDassert( ( 0 <= idx ) && ( idx <= max_indices2[type] ) );

        base_addr = entries2[type];
        entry_ptr = &(base_addr[idx]);

        HDassert( entry_ptr->index == idx );
        HDassert( entry_ptr->type == type );
        HDassert( entry_ptr == entry_ptr->self );
	HDassert( entry_ptr->cache_ptr == cache_ptr );
        HDassert( ! (entry_ptr->header.is_protected) );
        HDassert( entry_ptr->header.is_pinned );
	HDassert( entry_ptr->is_pinned );

        result = H5C2_unpin_entry(cache_ptr, (void *)entry_ptr);

        if ( ( result < 0 ) ||
             ( entry_ptr->header.is_pinned ) ||
             ( entry_ptr->header.type != &(types2[type]) ) ||
             ( entry_ptr->size != entry_ptr->header.size ) ||
             ( entry_ptr->addr != entry_ptr->header.addr ) ) {

            pass2 = FALSE;
            failure_mssg2 = "error in H5C2_unpin().";

        }

	entry_ptr->is_pinned = FALSE;

        HDassert( ((entry_ptr->header).type)->id == type );

    }

    return;

} /* unpin_entry2() */


/*-------------------------------------------------------------------------
 * Function:	unprotect_entry2()
 *
 * Purpose:	Unprotect the entry indicated by the type and index.
 *
 *		Do nothing if pass2 is FALSE on entry.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/12/04
 *
 * Modifications:
 *
 *		JRM -- 1/7/05
 *		Updated for the replacement of the deleted parameter in
 *		H5C2_unprotect() with the new flags parameter.
 *
 *		JRM - 6/17/05
 *		Modified function to use the new dirtied parameter of
 *		H5C2_unprotect().
 *
 *		JRM -- 9/8/05
 *		Update for new entry size parameter in H5C2_unprotect().
 *		We don't use them here for now.
 *
 *		JRM -- 3/31/06
 *		Update for pinned entries.
 *
 *		JRM -- 4/1/07 
 *		Updated for new multiple read protects.
 *
 *-------------------------------------------------------------------------
 */

void
unprotect_entry2(H5C2_t * cache_ptr,
                int32_t type,
                int32_t idx,
                int dirty,
                unsigned int flags)
{
    const char * fcn_name = "unprotect_entry2()";
    herr_t result;
    hbool_t verbose = FALSE;
    hbool_t pin_flag_set;
    hbool_t unpin_flag_set;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    if ( verbose ) {
	HDfprintf(stdout, 
            "\n%s: entering. type = %d, idx = %d, dirty = %d, flags = %0x.\n",
	    fcn_name, type, idx, (int)dirty, (int)flags);
    }

    if ( pass2 ) {

        HDassert( cache_ptr );
        HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
        HDassert( ( 0 <= idx ) && ( idx <= max_indices2[type] ) );

        base_addr = entries2[type];
        entry_ptr = &(base_addr[idx]);

        HDassert( entry_ptr->index == idx );
        HDassert( entry_ptr->type == type );
        HDassert( entry_ptr == entry_ptr->self );
	HDassert( entry_ptr->cache_ptr == cache_ptr );
        HDassert( entry_ptr->header.is_protected );
        HDassert( entry_ptr->is_protected );

	pin_flag_set = ((flags & H5C2__PIN_ENTRY_FLAG) != 0 );
	unpin_flag_set = ((flags & H5C2__UNPIN_ENTRY_FLAG) != 0 );

	HDassert ( ! ( pin_flag_set && unpin_flag_set ) );
	HDassert ( ( ! pin_flag_set ) || ( ! (entry_ptr->is_pinned) ) );
	HDassert ( ( ! unpin_flag_set ) || ( entry_ptr->is_pinned ) );

        if ( ( dirty == TRUE ) || ( dirty == FALSE ) ) {

            flags |= (dirty ? H5C2__DIRTIED_FLAG : H5C2__NO_FLAGS_SET);
            entry_ptr->is_dirty = (entry_ptr->is_dirty || dirty);
        }

	if ( verbose ) {
	    HDfprintf(stdout, "%s: calling H5C2_unprotect(). addr = 0X%lx.\n",
	              fcn_name, (long)(entry_ptr->addr));
        }

        result = H5C2_unprotect(cache_ptr, H5P_DATASET_XFER_DEFAULT, 
			        &(types2[type]), entry_ptr->addr, 
				(void *)entry_ptr, flags, (size_t)0);

	if ( verbose ) {
	    HDfprintf(stdout, "%s: H5C2_unprotect() returns. addr = 0X%lx.\n",
	              fcn_name, (long)(entry_ptr->addr));
        }


        if ( ( result < 0 ) ||
             ( ( entry_ptr->header.is_protected ) &&
	       ( ( ! ( entry_ptr->is_read_only ) ) ||
		 ( entry_ptr->ro_ref_count <= 0 ) ) ) ||
             ( entry_ptr->header.type != &(types2[type]) ) ||
             ( entry_ptr->size != entry_ptr->header.size ) ||
             ( entry_ptr->addr != entry_ptr->header.addr ) ) {

            pass2 = FALSE;
            failure_mssg2 = "error in H5C2_unprotect().";

        }
        else
        {
	    if ( entry_ptr->ro_ref_count > 1 ) {

		entry_ptr->ro_ref_count--;

	    } else if ( entry_ptr->ro_ref_count == 1 ) {

		entry_ptr->is_protected = FALSE;
		entry_ptr->is_read_only = FALSE;
		entry_ptr->ro_ref_count = 0;

	    } else {
            
		entry_ptr->is_protected = FALSE;

	    }

	    if ( pin_flag_set ) {

	        HDassert ( entry_ptr->header.is_pinned );
		entry_ptr->is_pinned = TRUE;

	    } else if ( unpin_flag_set ) {

	        HDassert ( ! ( entry_ptr->header.is_pinned ) );
		entry_ptr->is_pinned = FALSE;

            }
        }

        HDassert( ((entry_ptr->header).type)->id == type );

        if ( ( flags & H5C2__DIRTIED_FLAG ) != 0
                && ( (flags & H5C2__DELETED_FLAG) == 0 ) ) {

            HDassert( entry_ptr->header.is_dirty );
            HDassert( entry_ptr->is_dirty );
        }

	HDassert( entry_ptr->header.is_protected == entry_ptr->is_protected );
	HDassert( entry_ptr->header.is_read_only == entry_ptr->is_read_only );
	HDassert( entry_ptr->header.ro_ref_count == entry_ptr->ro_ref_count );
    }

    if ( verbose ) {
	HDfprintf(stdout, "\n%s: exiting.\n", fcn_name);
    }

    return;

} /* unprotect_entry2() */


/*-------------------------------------------------------------------------
 * Function:	unprotect_entry_with_size_change2()
 *
 * Purpose:	Version of unprotect_entry() that allow access to the new
 * 		size change parameters in H5C2_unprotect_entry()
 *
 * 		At present, only the sizes of VARIABLE_ENTRY_TYPE entries
 * 		can be changed.  Thus this function will scream and die
 * 		if the H5C2__SIZE_CHANGED_FLAG is set and the type is not
 * 		VARIABLE_ENTRY_TYPE.
 *
 *		Do nothing if pass2 is FALSE on entry.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              8/31/06
 *
 * Modifications:
 *
 *		None.
 *
 *-------------------------------------------------------------------------
 */

void
unprotect_entry_with_size_change2(H5C2_t * cache_ptr,
                                  int32_t type,
                                  int32_t idx,
                                  unsigned int flags,
		                  size_t new_size)
{
    /* const char * fcn_name = "unprotect_entry_with_size_change2()"; */
    herr_t result;
    hbool_t dirty_flag_set;
    hbool_t pin_flag_set;
    hbool_t unpin_flag_set;
    hbool_t size_changed_flag_set;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    if ( pass2 ) {

        HDassert( cache_ptr );
        HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
        HDassert( ( 0 <= idx ) && ( idx <= max_indices2[type] ) );
	HDassert( new_size <= entry_sizes2[type] );

        base_addr = entries2[type];
        entry_ptr = &(base_addr[idx]);

        HDassert( entry_ptr->index == idx );
        HDassert( entry_ptr->type == type );
        HDassert( entry_ptr == entry_ptr->self );
	HDassert( entry_ptr->cache_ptr == cache_ptr );
        HDassert( entry_ptr->header.is_protected );
        HDassert( entry_ptr->is_protected );

	dirty_flag_set = ((flags & H5C2__DIRTIED_FLAG) != 0 );
	pin_flag_set = ((flags & H5C2__PIN_ENTRY_FLAG) != 0 );
	unpin_flag_set = ((flags & H5C2__UNPIN_ENTRY_FLAG) != 0 );
	size_changed_flag_set = ((flags & H5C2__SIZE_CHANGED_FLAG) != 0 );

	HDassert ( ! ( pin_flag_set && unpin_flag_set ) );
	HDassert ( ( ! pin_flag_set ) || ( ! (entry_ptr->is_pinned) ) );
	HDassert ( ( ! unpin_flag_set ) || ( entry_ptr->is_pinned ) );
	HDassert ( ( ! size_changed_flag_set ) || ( new_size > 0 ) );
	HDassert ( ( ! size_changed_flag_set ) || 
		   ( type == VARIABLE_ENTRY_TYPE ) );

        entry_ptr->is_dirty = (entry_ptr->is_dirty || dirty_flag_set);

	if ( size_changed_flag_set ) {

            entry_ptr->is_dirty = TRUE;  
	    entry_ptr->size = new_size;
        }

        result = H5C2_unprotect(cache_ptr, H5P_DATASET_XFER_DEFAULT, 
			        &(types2[type]), entry_ptr->addr, 
				(void *)entry_ptr, flags, new_size);

        if ( ( result < 0 ) ||
             ( entry_ptr->header.is_protected ) ||
             ( entry_ptr->header.type != &(types2[type]) ) ||
             ( entry_ptr->size != entry_ptr->header.size ) ||
             ( entry_ptr->addr != entry_ptr->header.addr ) ) {

            pass2 = FALSE;
            failure_mssg2 = "error in H5C2_unprotect().";

        }
        else
        {
            entry_ptr->is_protected = FALSE;

	    if ( pin_flag_set ) {

	        HDassert ( entry_ptr->header.is_pinned );
		entry_ptr->is_pinned = TRUE;

	    } else if ( unpin_flag_set ) {

	        HDassert ( ! ( entry_ptr->header.is_pinned ) );
		entry_ptr->is_pinned = FALSE;

            }
        }

        HDassert( ((entry_ptr->header).type)->id == type );

        if ( ( flags & H5C2__DIRTIED_FLAG ) != 0
                && ( (flags & H5C2__DELETED_FLAG) == 0 ) ) {

            HDassert( entry_ptr->header.is_dirty );
            HDassert( entry_ptr->is_dirty );
        }
    }

    return;

} /* unprotect_entry_with_size_change2() */


/*-------------------------------------------------------------------------
 * Function:	row_major_scan_forward2()
 *
 * Purpose:	Do a sequence of inserts, protects, unprotects, renames,
 *		destroys while scanning through the set of entries.  If
 *		pass2 is false on entry, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/12/04
 *
 * Modifications:
 *
 * 		JRM -- 4/4/07
 * 		Added code supporting multiple read only protects.
 * 		Note that this increased the minimum lag to 10.
 *
 *-------------------------------------------------------------------------
 */

void
row_major_scan_forward2(H5C2_t * cache_ptr,
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
                        int dirty_unprotects)
{
    const char * fcn_name = "row_major_scan_forward2";
    int32_t type;
    int32_t idx;

    if ( verbose )
        HDfprintf(stdout, "%s(): entering.\n", fcn_name);

    HDassert( lag >= 10 );

    type = 0;

    if ( ( pass2 ) && ( reset_stats ) ) {

        H5C2_stats__reset(cache_ptr);
    }

    while ( ( pass2 ) && ( type < NUMBER_OF_ENTRY_TYPES ) )
    {
        idx = -lag;

        while ( ( pass2 ) && ( idx <= (max_indices2[type] + lag) ) )
        {
	    if ( verbose ) {

                HDfprintf(stdout, "%d:%d: ", type, idx);
	    }

            if ( ( pass2 ) && ( do_inserts ) && ( (idx + lag) >= 0 ) &&
                 ( (idx + lag) <= max_indices2[type] ) &&
                 ( ((idx + lag) % 2) == 0 ) &&
                 ( ! entry_in_cache2(cache_ptr, type, (idx + lag)) ) ) {

                if ( verbose )
                    HDfprintf(stdout, "1(i, %d, %d) ", type, (idx + lag));

                insert_entry2(cache_ptr, type, (idx + lag), dirty_inserts,
                              H5C2__NO_FLAGS_SET);
            }


            if ( ( pass2 ) && ( (idx + lag - 1) >= 0 ) &&
                 ( (idx + lag - 1) <= max_indices2[type] ) &&
                 ( ( (idx + lag - 1) % 3 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "2(p, %d, %d) ", type, (idx + lag - 1));

                protect_entry2(cache_ptr, type, (idx + lag - 1));
            }

            if ( ( pass2 ) && ( (idx + lag - 2) >= 0 ) &&
                 ( (idx + lag - 2) <= max_indices2[type] ) &&
                 ( ( (idx + lag - 2) % 3 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "3(u, %d, %d) ", type, (idx + lag - 2));

                unprotect_entry2(cache_ptr, type, idx+lag-2, NO_CHANGE,
                                 H5C2__NO_FLAGS_SET);
            }


            if ( ( pass2 ) && ( do_renames ) && ( (idx + lag - 2) >= 0 ) &&
                 ( (idx + lag - 2) <= max_indices2[type] ) &&
                 ( ( (idx + lag - 2) % 3 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "4(r, %d, %d, %d) ", 
			      type, (idx + lag - 2), (int)rename_to_main_addr);

                rename_entry2(cache_ptr, type, (idx + lag - 2),
                             rename_to_main_addr);
            }


            if ( ( pass2 ) && ( (idx + lag - 3) >= 0 ) &&
                 ( (idx + lag - 3) <= max_indices2[type] ) &&
                 ( ( (idx + lag - 3) % 5 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "5(p, %d, %d) ", type, (idx + lag - 3));

                protect_entry2(cache_ptr, type, (idx + lag - 3));
            }

            if ( ( pass2 ) && ( (idx + lag - 5) >= 0 ) &&
                 ( (idx + lag - 5) <= max_indices2[type] ) &&
                 ( ( (idx + lag - 5) % 5 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "6(u, %d, %d) ", type, (idx + lag - 5));

                unprotect_entry2(cache_ptr, type, idx+lag-5, NO_CHANGE,
                                 H5C2__NO_FLAGS_SET);
            }

	    if ( do_mult_ro_protects )
	    {
		if ( ( pass2 ) && ( (idx + lag - 5) >= 0 ) &&
		     ( (idx + lag - 5) < max_indices2[type] ) &&
		     ( (idx + lag - 5) % 9 == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "7(p-ro, %d, %d) ", type, 
				  (idx + lag - 5));

		    protect_entry_ro2(cache_ptr, type, (idx + lag - 5));
		}

		if ( ( pass2 ) && ( (idx + lag - 6) >= 0 ) &&
		     ( (idx + lag - 6) < max_indices2[type] ) &&
		     ( (idx + lag - 6) % 11 == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "8(p-ro, %d, %d) ", type, 
				  (idx + lag - 6));

		    protect_entry_ro2(cache_ptr, type, (idx + lag - 6));
		}

		if ( ( pass2 ) && ( (idx + lag - 7) >= 0 ) &&
		     ( (idx + lag - 7) < max_indices2[type] ) &&
		     ( (idx + lag - 7) % 13 == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "9(p-ro, %d, %d) ", type, 
				  (idx + lag - 7));

		    protect_entry_ro2(cache_ptr, type, (idx + lag - 7));
		}

		if ( ( pass2 ) && ( (idx + lag - 7) >= 0 ) &&
		     ( (idx + lag - 7) < max_indices2[type] ) &&
		     ( (idx + lag - 7) % 9 == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "10(u-ro, %d, %d) ", type, 
				  (idx + lag - 7));

		    unprotect_entry2(cache_ptr, type, (idx + lag - 7),
				     FALSE, H5C2__NO_FLAGS_SET);
		}

		if ( ( pass2 ) && ( (idx + lag - 8) >= 0 ) &&
		     ( (idx + lag - 8) < max_indices2[type] ) &&
		     ( (idx + lag - 8) % 11 == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "11(u-ro, %d, %d) ", type, 
				  (idx + lag - 8));

		    unprotect_entry2(cache_ptr, type, (idx + lag - 8),
				    FALSE, H5C2__NO_FLAGS_SET);
		}

		if ( ( pass2 ) && ( (idx + lag - 9) >= 0 ) &&
		     ( (idx + lag - 9) < max_indices2[type] ) &&
		     ( (idx + lag - 9) % 13 == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "12(u-ro, %d, %d) ", type, 
				  (idx + lag - 9));

		    unprotect_entry2(cache_ptr, type, (idx + lag - 9),
				     FALSE, H5C2__NO_FLAGS_SET);
		}
	    } /* if ( do_mult_ro_protects ) */

            if ( ( pass2 ) && ( idx >= 0 ) && ( idx <= max_indices2[type] ) ) {

                if ( verbose )
                    HDfprintf(stdout, "13(p, %d, %d) ", type, idx);

                protect_entry2(cache_ptr, type, idx);
            }

            if ( ( pass2 ) && ( (idx - lag + 2) >= 0 ) &&
                 ( (idx - lag + 2) <= max_indices2[type] ) &&
                 ( ( (idx - lag + 2) % 7 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "14(u, %d, %d) ", type, (idx - lag + 2));

                unprotect_entry2(cache_ptr, type, idx-lag+2, NO_CHANGE,
                                H5C2__NO_FLAGS_SET);
            }

            if ( ( pass2 ) && ( (idx - lag + 1) >= 0 ) &&
                 ( (idx - lag + 1) <= max_indices2[type] ) &&
                 ( ( (idx - lag + 1) % 7 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "15(p, %d, %d) ", type, (idx - lag + 1));

                protect_entry2(cache_ptr, type, (idx - lag + 1));
            }


            if ( do_destroys ) {

                if ( ( pass2 ) && ( (idx - lag) >= 0 ) &&
                     ( ( idx - lag) <= max_indices2[type] ) ) {

                    switch ( (idx - lag) %4 ) {

                        case 0: /* we just did an insert */

                            if ( verbose )
                                HDfprintf(stdout, 
					 "16(u, %d, %d) ", type, (idx - lag));

                            unprotect_entry2(cache_ptr, type, idx - lag,
                                            NO_CHANGE, H5C2__NO_FLAGS_SET);
                            break;

                        case 1:
                            if ( (entries2[type])[idx-lag].is_dirty ) {

                            if ( verbose )
                                HDfprintf(stdout, 
					 "17(u, %d, %d) ", type, (idx - lag));

                                unprotect_entry2(cache_ptr, type, idx - lag,
                                                NO_CHANGE, H5C2__NO_FLAGS_SET);
                            } else {

                            if ( verbose )
                                HDfprintf(stdout, 
					 "18(u, %d, %d) ", type, (idx - lag));

                                unprotect_entry2(cache_ptr, type, idx - lag,
                                                dirty_unprotects,
                                                H5C2__NO_FLAGS_SET);
                            }
                            break;

                        case 2: /* we just did an insrt */

                            if ( verbose )
                                HDfprintf(stdout, 
					 "19(u-del, %d, %d) ", type, (idx - lag));

                            unprotect_entry2(cache_ptr, type, idx - lag,
                                            NO_CHANGE, H5C2__DELETED_FLAG);
                            break;

                        case 3:
                            if ( (entries2[type])[idx-lag].is_dirty ) {

                                if ( verbose )
                                    HDfprintf(stdout, 
					      "20(u-del, %d, %d) ", 
					      type, (idx - lag));

                                unprotect_entry2(cache_ptr, type, idx - lag,
                                                NO_CHANGE, H5C2__DELETED_FLAG);
                            } else {

                                if ( verbose )
                                    HDfprintf(stdout, 
					      "21(u-del, %d, %d) ", 
					      type, (idx - lag));

                                unprotect_entry2(cache_ptr, type, idx - lag,
                                                dirty_destroys,
                                                H5C2__DELETED_FLAG);
                            }
                            break;

                        default:
                            HDassert(0); /* this can't happen... */
                            break;
                    }
                }

            } else {

                if ( ( pass2 ) && ( (idx - lag) >= 0 ) &&
                     ( ( idx - lag) <= max_indices2[type] ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "22(u, %d, %d) ", type, (idx - lag));

                    unprotect_entry2(cache_ptr, type, idx - lag,
                                    dirty_unprotects, H5C2__NO_FLAGS_SET);
                }
            }

            if ( verbose )
                HDfprintf(stdout, "\n");

            idx++;
        }
        type++;
    }

    if ( ( pass2 ) && ( display_stats ) ) {

        H5C2_stats(cache_ptr, "test cache", display_detailed_stats);
    }

    return;

} /* row_major_scan_forward2() */


/*-------------------------------------------------------------------------
 * Function:	hl_row_major_scan_forward2()
 *
 * Purpose:	Do a high locality sequence of inserts, protects, and
 *		unprotects while scanning through the set of entries.
 *		If pass2 is false on entry, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              10/21/04
 *
 * Modifications:
 *
 *		JRM -- 1/21/05
 *		Added the max_index parameter to allow the caller to
 *		throttle the size of the inner loop, and thereby the
 *		execution time of the function.
 *
 *-------------------------------------------------------------------------
 */

void
hl_row_major_scan_forward2(H5C2_t * cache_ptr,
                           int32_t max_index,
                           hbool_t verbose,
                           hbool_t reset_stats,
                           hbool_t display_stats,
                           hbool_t display_detailed_stats,
                           hbool_t do_inserts,
                           hbool_t dirty_inserts)
{
    const char * fcn_name = "hl_row_major_scan_forward2";
    int32_t type;
    int32_t idx;
    int32_t i;
    int32_t lag = 100;
    int32_t local_max_index;

    if ( verbose )
        HDfprintf(stdout, "%s(): entering.\n", fcn_name);

    HDassert( lag > 5 );
    HDassert( max_index >= 200 );
    HDassert( max_index <= MAX_ENTRIES );

    type = 0;

    if ( ( pass2 ) && ( reset_stats ) ) {

        H5C2_stats__reset(cache_ptr);
    }

    while ( ( pass2 ) && ( type < NUMBER_OF_ENTRY_TYPES ) )
    {
        idx = -lag;

        local_max_index = MIN(max_index, max_indices2[type]);

        while ( ( pass2 ) && ( idx <= (local_max_index + lag) ) )
        {
            if ( ( pass2 ) && ( do_inserts ) && ( (idx + lag) >= 0 ) &&
                 ( (idx + lag) <= max_indices2[type] ) &&
                 ( ((idx + lag) % 2) == 0 ) &&
                 ( ! entry_in_cache2(cache_ptr, type, (idx + lag)) ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(i, %d, %d) ", type, (idx + lag));

                insert_entry2(cache_ptr, type, (idx + lag), dirty_inserts,
                              H5C2__NO_FLAGS_SET);
            }

            i = idx;

            while ( ( pass2 ) && ( i >= idx - lag ) && ( i >= 0 ) )
            {
                if ( ( pass2 ) && ( i >= 0 ) && ( i <= local_max_index ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(p, %d, %d) ", type, i);

                    protect_entry2(cache_ptr, type, i);

                    if ( verbose )
                        HDfprintf(stdout, "(u, %d, %d) ", type, i);

                    unprotect_entry2(cache_ptr, type, i, NO_CHANGE,
                                    H5C2__NO_FLAGS_SET);
                }
                i--;
            }

            if ( verbose )
                HDfprintf(stdout, "\n");

            idx++;
        }
        type++;
    }

    if ( ( pass2 ) && ( display_stats ) ) {

        H5C2_stats(cache_ptr, "test cache", display_detailed_stats);
    }

    return;

} /* hl_row_major_scan_forward2() */


/*-------------------------------------------------------------------------
 * Function:	row_major_scan_backward2()
 *
 * Purpose:	Do a sequence of inserts, protects, unprotects, renames,
 *		destroys while scanning backwards through the set of
 *		entries.  If pass2 is false on entry, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/12/04
 *
 * Modifications:
 *
 * 		JRM -- 4/4/07
 * 		Added code supporting multiple read only protects.
 * 		Note that this increased the minimum lag to 10.
 *
 *-------------------------------------------------------------------------
 */

void
row_major_scan_backward2(H5C2_t * cache_ptr,
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
                         int dirty_unprotects)
{
    const char * fcn_name = "row_major_scan_backward2";
    int32_t type;
    int32_t idx;

    if ( verbose )
        HDfprintf(stdout, "%s(): Entering.\n", fcn_name);

    HDassert( lag >= 10 );

    type = NUMBER_OF_ENTRY_TYPES - 1;

    if ( ( pass2 ) && ( reset_stats ) ) {

        H5C2_stats__reset(cache_ptr);
    }

    while ( ( pass2 ) && ( type >= 0 ) )
    {
        idx = max_indices2[type] + lag;

        while ( ( pass2 ) && ( idx >= -lag ) )
        {
            if ( ( pass2 ) && ( do_inserts ) && ( (idx - lag) >= 0 ) &&
                 ( (idx - lag) <= max_indices2[type] ) &&
                 ( ((idx - lag) % 2) == 1 ) &&
                 ( ! entry_in_cache2(cache_ptr, type, (idx - lag)) ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(i, %d, %d) ", type, (idx - lag));

                insert_entry2(cache_ptr, type, (idx - lag), dirty_inserts,
                              H5C2__NO_FLAGS_SET);
            }


            if ( ( pass2 ) && ( (idx - lag + 1) >= 0 ) &&
                 ( (idx - lag + 1) <= max_indices2[type] ) &&
                 ( ( (idx - lag + 1) % 3 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, (idx - lag + 1));

                protect_entry2(cache_ptr, type, (idx - lag + 1));
            }

            if ( ( pass2 ) && ( (idx - lag + 2) >= 0 ) &&
                 ( (idx - lag + 2) <= max_indices2[type] ) &&
                 ( ( (idx - lag + 2) % 3 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(u, %d, %d) ", type, (idx - lag + 2));

                unprotect_entry2(cache_ptr, type, idx-lag+2, NO_CHANGE,
                                H5C2__NO_FLAGS_SET);
            }


            if ( ( pass2 ) && ( do_renames ) && ( (idx - lag + 2) >= 0 ) &&
                 ( (idx - lag + 2) <= max_indices2[type] ) &&
                 ( ( (idx - lag + 2) % 3 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(r, %d, %d, %d) ", 
			      type, (idx + lag + 2), (int)rename_to_main_addr);

                rename_entry2(cache_ptr, type, (idx - lag + 2),
                             rename_to_main_addr);
            }


            if ( ( pass2 ) && ( (idx - lag + 3) >= 0 ) &&
                 ( (idx - lag + 3) <= max_indices2[type] ) &&
                 ( ( (idx - lag + 3) % 5 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, (idx - lag + 3));

                protect_entry2(cache_ptr, type, (idx - lag + 3));
            }

            if ( ( pass2 ) && ( (idx - lag + 5) >= 0 ) &&
                 ( (idx - lag + 5) <= max_indices2[type] ) &&
                 ( ( (idx - lag + 5) % 5 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(u, %d, %d) ", type, (idx - lag + 5));

                unprotect_entry2(cache_ptr, type, idx-lag+5, NO_CHANGE,
                                H5C2__NO_FLAGS_SET);
            }

	    if ( do_mult_ro_protects )
	    {
		if ( ( pass2 ) && ( (idx - lag + 5) >= 0 ) &&
		     ( (idx - lag + 5) < max_indices2[type] ) &&
		     ( (idx - lag + 5) % 9 == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(p-ro, %d, %d) ", type, 
				  (idx - lag + 5));

		    protect_entry_ro2(cache_ptr, type, (idx - lag + 5));
		}

		if ( ( pass2 ) && ( (idx - lag + 6) >= 0 ) &&
		     ( (idx - lag + 6) < max_indices2[type] ) &&
		     ( (idx - lag + 6) % 11 == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(p-ro, %d, %d) ", type, 
				  (idx - lag + 6));

		    protect_entry_ro2(cache_ptr, type, (idx - lag + 6));
		}

		if ( ( pass2 ) && ( (idx - lag + 7) >= 0 ) &&
		     ( (idx - lag + 7) < max_indices2[type] ) &&
		     ( (idx - lag + 7) % 13 == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(p-ro, %d, %d) ", type, 
				  (idx - lag + 7));

		    protect_entry_ro2(cache_ptr, type, (idx - lag + 7));
		}

		if ( ( pass2 ) && ( (idx - lag + 7) >= 0 ) &&
		     ( (idx - lag + 7) < max_indices2[type] ) &&
		     ( (idx - lag + 7) % 9 == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(u-ro, %d, %d) ", type, 
				  (idx - lag + 7));

		    unprotect_entry2(cache_ptr, type, (idx - lag + 7),
				    FALSE, H5C2__NO_FLAGS_SET);
		}

		if ( ( pass2 ) && ( (idx - lag + 8) >= 0 ) &&
		     ( (idx - lag + 8) < max_indices2[type] ) &&
		     ( (idx - lag + 8) % 11 == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(u-ro, %d, %d) ", type, 
				  (idx - lag + 8));

		    unprotect_entry2(cache_ptr, type, (idx - lag + 8),
				    FALSE, H5C2__NO_FLAGS_SET);
		}

		if ( ( pass2 ) && ( (idx - lag + 9) >= 0 ) &&
		     ( (idx - lag + 9) < max_indices2[type] ) &&
		     ( (idx - lag + 9) % 13 == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(u-ro, %d, %d) ", type, 
				  (idx - lag + 9));

		    unprotect_entry2(cache_ptr, type, (idx - lag + 9),
				    FALSE, H5C2__NO_FLAGS_SET);
		}
	    } /* if ( do_mult_ro_protects ) */

            if ( ( pass2 ) && ( idx >= 0 ) && ( idx <= max_indices2[type] ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, idx);

                protect_entry2(cache_ptr, type, idx);
            }


            if ( ( pass2 ) && ( (idx + lag - 2) >= 0 ) &&
                 ( (idx + lag - 2) <= max_indices2[type] ) &&
                 ( ( (idx + lag - 2) % 7 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(u, %d, %d) ", type, (idx + lag - 2));

                unprotect_entry2(cache_ptr, type, idx+lag-2, NO_CHANGE,
                                H5C2__NO_FLAGS_SET);
            }

            if ( ( pass2 ) && ( (idx + lag - 1) >= 0 ) &&
                 ( (idx + lag - 1) <= max_indices2[type] ) &&
                 ( ( (idx + lag - 1) % 7 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, (idx + lag - 1));

                protect_entry2(cache_ptr, type, (idx + lag - 1));
            }


            if ( do_destroys ) {

                if ( ( pass2 ) && ( (idx + lag) >= 0 ) &&
                     ( ( idx + lag) <= max_indices2[type] ) ) {

                    switch ( (idx + lag) %4 ) {

                        case 0:
                            if ( (entries2[type])[idx+lag].is_dirty ) {

                                unprotect_entry2(cache_ptr, type, idx + lag,
                                                 NO_CHANGE, H5C2__NO_FLAGS_SET);
                            } else {

                                unprotect_entry2(cache_ptr, type, idx + lag,
                                                dirty_unprotects,
                                                H5C2__NO_FLAGS_SET);
                            }
                            break;

                        case 1: /* we just did an insert */
                            unprotect_entry2(cache_ptr, type, idx + lag,
                                            NO_CHANGE, H5C2__NO_FLAGS_SET);
                            break;

                        case 2:
                            if ( (entries2[type])[idx + lag].is_dirty ) {

                                unprotect_entry2(cache_ptr, type, idx + lag,
                                                NO_CHANGE, H5C2__DELETED_FLAG);
                            } else {

                                unprotect_entry2(cache_ptr, type, idx + lag,
                                                dirty_destroys,
                                                H5C2__DELETED_FLAG);
                            }
                            break;

                        case 3: /* we just did an insrt */
                            unprotect_entry2(cache_ptr, type, idx + lag,
                                            NO_CHANGE, H5C2__DELETED_FLAG);
                            break;

                        default:
                            HDassert(0); /* this can't happen... */
                            break;
                    }
                }
            } else {

                if ( ( pass2 ) && ( (idx + lag) >= 0 ) &&
                     ( ( idx + lag) <= max_indices2[type] ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(u, %d, %d) ", type, (idx + lag));

                    unprotect_entry2(cache_ptr, type, idx + lag,
                                    dirty_unprotects, H5C2__NO_FLAGS_SET);
                }
            }

            if ( verbose )
                HDfprintf(stdout, "\n");

            idx--;
        }
        type--;
    }

    if ( ( pass2 ) && ( display_stats ) ) {

        H5C2_stats(cache_ptr, "test cache", display_detailed_stats);
    }

    return;

} /* row_major_scan_backward2() */


/*-------------------------------------------------------------------------
 * Function:	hl_row_major_scan_backward2()
 *
 * Purpose:	Do a high locality sequence of inserts, protects, and
 *		unprotects while scanning through the set of entries.
 *		If pass2 is false on entry, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              10/21/04
 *
 * Modifications:
 *
 *		JRM -- 1/21/05
 *		Added the max_index parameter to allow the caller to
 *		throttle the size of the inner loop, and thereby the
 *		execution time of the function.
 *
 *-------------------------------------------------------------------------
 */

void
hl_row_major_scan_backward2(H5C2_t * cache_ptr,
                            int32_t max_index,
                            hbool_t verbose,
                            hbool_t reset_stats,
                            hbool_t display_stats,
                            hbool_t display_detailed_stats,
                            hbool_t do_inserts,
                            hbool_t dirty_inserts)
{
    const char * fcn_name = "hl_row_major_scan_backward2";
    int32_t type;
    int32_t idx;
    int32_t i;
    int32_t lag = 100;
    int32_t local_max_index;

    if ( verbose )
        HDfprintf(stdout, "%s(): entering.\n", fcn_name);

    HDassert( lag > 5 );
    HDassert( max_index >= 200 );
    HDassert( max_index <= MAX_ENTRIES );

    type = NUMBER_OF_ENTRY_TYPES - 1;

    if ( ( pass2 ) && ( reset_stats ) ) {

        H5C2_stats__reset(cache_ptr);
    }

    while ( ( pass2 ) && ( type >= 0 ) )
    {
        idx = max_indices2[type] + lag;

        local_max_index = MIN(max_index, max_indices2[type]);

        while ( ( pass2 ) && ( idx >= -lag ) )
        {
            if ( ( pass2 ) && ( do_inserts ) && ( (idx + lag) >= 0 ) &&
                 ( (idx + lag) <= local_max_index ) &&
                 ( ((idx + lag) % 2) == 0 ) &&
                 ( ! entry_in_cache2(cache_ptr, type, (idx + lag)) ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(i, %d, %d) ", type, (idx + lag));

                insert_entry2(cache_ptr, type, (idx + lag), dirty_inserts,
                              H5C2__NO_FLAGS_SET);
            }

            i = idx;

            while ( ( pass2 ) && ( i >= idx - lag ) && ( i >= 0 ) )
            {
                if ( ( pass2 ) && ( i >= 0 ) && ( i <= local_max_index ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(p, %d, %d) ", type, i);

                    protect_entry2(cache_ptr, type, i);

                    if ( verbose )
                        HDfprintf(stdout, "(u, %d, %d) ", type, i);

                    unprotect_entry2(cache_ptr, type, i, NO_CHANGE,
                                    H5C2__NO_FLAGS_SET);
                }
                i--;
            }

            if ( verbose )
                HDfprintf(stdout, "\n");

            idx--;
        }
        type--;
    }

    if ( ( pass2 ) && ( display_stats ) ) {

        H5C2_stats(cache_ptr, "test cache", display_detailed_stats);
    }

    return;

} /* hl_row_major_scan_backward2() */


/*-------------------------------------------------------------------------
 * Function:	col_major_scan_forward2()
 *
 * Purpose:	Do a sequence of inserts, protects, and unprotects
 *		while scanning through the set of entries.  If
 *		pass2 is false on entry, do nothing.
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

void
col_major_scan_forward2(H5C2_t * cache_ptr,
                        int32_t lag,
                        hbool_t verbose,
                        hbool_t reset_stats,
                        hbool_t display_stats,
                        hbool_t display_detailed_stats,
                        hbool_t do_inserts,
                        hbool_t dirty_inserts,
                        int dirty_unprotects)
{
    const char * fcn_name = "col_major_scan_forward2()";
    int32_t type;
    int32_t idx;

    if ( verbose )
        HDfprintf(stdout, "%s: entering.\n", fcn_name);

    HDassert( lag > 5 );

    type = 0;

    if ( ( pass2 ) && ( reset_stats ) ) {

        H5C2_stats__reset(cache_ptr);
    }

    idx = -lag;

    while ( ( pass2 ) && ( (idx - lag) <= MAX_ENTRIES ) )
    {
        type = 0;

        while ( ( pass2 ) && ( type < NUMBER_OF_ENTRY_TYPES ) )
        {
            if ( ( pass2 ) && ( do_inserts ) && ( (idx + lag) >= 0 ) &&
                 ( (idx + lag) <= max_indices2[type] ) &&
                 ( ((idx + lag) % 3) == 0 ) &&
                 ( ! entry_in_cache2(cache_ptr, type, (idx + lag)) ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(i, %d, %d) ", type, (idx + lag));

                insert_entry2(cache_ptr, type, (idx + lag), dirty_inserts,
                              H5C2__NO_FLAGS_SET);
            }

            if ( ( pass2 ) && ( idx >= 0 ) && ( idx <= max_indices2[type] ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, idx);

                protect_entry2(cache_ptr, type, idx);
            }

            if ( ( pass2 ) && ( (idx - lag) >= 0 ) &&
                 ( (idx - lag) <= max_indices2[type] ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(u, %d, %d) ", type, (idx - lag));

                unprotect_entry2(cache_ptr, type, idx - lag,
                                dirty_unprotects, H5C2__NO_FLAGS_SET);
            }

            if ( verbose )
                HDfprintf(stdout, "\n");

            type++;
        }

        idx++;
    }

    if ( ( pass2 ) && ( display_stats ) ) {

        H5C2_stats(cache_ptr, "test cache", display_detailed_stats);
    }

    return;

} /* col_major_scan_forward2() */


/*-------------------------------------------------------------------------
 * Function:	hl_col_major_scan_forward2()
 *
 * Purpose:	Do a high locality sequence of inserts, protects, and
 *		unprotects while scanning through the set of entries.  If
 *		pass2 is false on entry, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              19/25/04
 *
 * Modifications:
 *
 *		JRM -- 1/21/05
 *		Added the max_index parameter to allow the caller to
 *		throttle the size of the inner loop, and thereby the
 *		execution time of the function.
 *
 *-------------------------------------------------------------------------
 */

void
hl_col_major_scan_forward2(H5C2_t * cache_ptr,
                          int32_t max_index,
                          hbool_t verbose,
                          hbool_t reset_stats,
                          hbool_t display_stats,
                          hbool_t display_detailed_stats,
                          hbool_t do_inserts,
                          hbool_t dirty_inserts,
                          int dirty_unprotects)
{
    const char * fcn_name = "hl_col_major_scan_forward2()";
    int32_t type;
    int32_t idx;
    int32_t lag = 200;
    int32_t i;
    int32_t local_max_index;

    if ( verbose )
        HDfprintf(stdout, "%s: entering.\n", fcn_name);

    HDassert( lag > 5 );
    HDassert( max_index >= 500 );
    HDassert( max_index <= MAX_ENTRIES );

    type = 0;

    if ( ( pass2 ) && ( reset_stats ) ) {

        H5C2_stats__reset(cache_ptr);
    }

    idx = 0;

    local_max_index = MIN(max_index, MAX_ENTRIES);

    while ( ( pass2 ) && ( idx <= local_max_index ) )
    {

        i = idx;

        while ( ( pass2 ) && ( i >= 0 ) && ( i >= (idx - lag) ) ) {

            type = 0;

            while ( ( pass2 ) && ( type < NUMBER_OF_ENTRY_TYPES ) )
            {
                if ( ( pass2 ) && ( do_inserts ) && ( i == idx ) &&
                     ( i <= local_max_index ) &&
                     ( (i % 3) == 0 ) &&
                     ( ! entry_in_cache2(cache_ptr, type, i) ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(i, %d, %d) ", type, i);

                    insert_entry2(cache_ptr, type, i, dirty_inserts,
                                  H5C2__NO_FLAGS_SET);
                }

                if ( ( pass2 ) && ( i >= 0 ) && ( i <= local_max_index ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(p, %d, %d) ", type, i);

                    protect_entry2(cache_ptr, type, i);
                }

                if ( ( pass2 ) && ( i >= 0 ) &&
                     ( i <= max_indices2[type] ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(u, %d, %d) ", type, i);

                    unprotect_entry2(cache_ptr, type, i,
                                    dirty_unprotects, H5C2__NO_FLAGS_SET);
                }

                if ( verbose )
                    HDfprintf(stdout, "\n");

                type++;
            }

            i--;
        }

        idx++;
    }

    if ( ( pass2 ) && ( display_stats ) ) {

        H5C2_stats(cache_ptr, "test cache", display_detailed_stats);
    }

    return;

} /* hl_col_major_scan_forward2() */


/*-------------------------------------------------------------------------
 * Function:	col_major_scan_backward2()
 *
 * Purpose:	Do a sequence of inserts, protects, and unprotects
 *		while scanning backwards through the set of
 *		entries.  If pass2 is false on entry, do nothing.
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

void
col_major_scan_backward2(H5C2_t * cache_ptr,
                         int32_t lag,
                         hbool_t verbose,
                         hbool_t reset_stats,
                         hbool_t display_stats,
                         hbool_t display_detailed_stats,
                         hbool_t do_inserts,
                         hbool_t dirty_inserts,
                         int dirty_unprotects)
{
    const char * fcn_name = "col_major_scan_backward2()";
    int mile_stone = 1;
    int32_t type;
    int32_t idx;

    if ( verbose )
        HDfprintf(stdout, "%s: entering.\n", fcn_name);

    HDassert( lag > 5 );

    if ( ( pass2 ) && ( reset_stats ) ) {

        H5C2_stats__reset(cache_ptr);
    }

    idx = MAX_ENTRIES + lag;

    if ( verbose ) /* 1 */
        HDfprintf(stdout, "%s: point %d.\n", fcn_name, mile_stone++);


    while ( ( pass2 ) && ( (idx + lag) >= 0 ) )
    {
        type = NUMBER_OF_ENTRY_TYPES - 1;

        while ( ( pass2 ) && ( type >= 0 ) )
        {
            if ( ( pass2 ) && ( do_inserts) && ( (idx - lag) >= 0 ) &&
                 ( (idx - lag) <= max_indices2[type] ) &&
                 ( ((idx - lag) % 3) == 0 ) &&
                 ( ! entry_in_cache2(cache_ptr, type, (idx - lag)) ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(i, %d, %d) ", type, (idx - lag));

                insert_entry2(cache_ptr, type, (idx - lag), dirty_inserts,
                              H5C2__NO_FLAGS_SET);
            }

            if ( ( pass2 ) && ( idx >= 0 ) && ( idx <= max_indices2[type] ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, idx);

                protect_entry2(cache_ptr, type, idx);
            }

            if ( ( pass2 ) && ( (idx + lag) >= 0 ) &&
                 ( (idx + lag) <= max_indices2[type] ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(u, %d, %d) ", type, (idx + lag));

                unprotect_entry2(cache_ptr, type, idx + lag,
                                dirty_unprotects, H5C2__NO_FLAGS_SET);
            }

            if ( verbose )
                HDfprintf(stdout, "\n");

            type--;
        }

        idx--;
    }

    if ( verbose ) /* 2 */
        HDfprintf(stdout, "%s: point %d.\n", fcn_name, mile_stone++);

    if ( ( pass2 ) && ( display_stats ) ) {

        H5C2_stats(cache_ptr, "test cache", display_detailed_stats);
    }

    if ( verbose )
        HDfprintf(stdout, "%s: exiting.\n", fcn_name);

    return;

} /* col_major_scan_backward2() */


/*-------------------------------------------------------------------------
 * Function:	hl_col_major_scan_backward2()
 *
 * Purpose:	Do a high locality sequence of inserts, protects, and
 *		unprotects while scanning backwards through the set of
 *		entries.  If pass2 is false on entry, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              10/25/04
 *
 * Modifications:
 *
 *		JRM -- 1/21/05
 *		Added the max_index parameter to allow the caller to
 *		throttle the size of the inner loop, and thereby the
 *		execution time of the function.
 *
 *-------------------------------------------------------------------------
 */

void
hl_col_major_scan_backward2(H5C2_t * cache_ptr,
                            int32_t max_index,
                            hbool_t verbose,
                            hbool_t reset_stats,
                            hbool_t display_stats,
                            hbool_t display_detailed_stats,
                            hbool_t do_inserts,
                            hbool_t dirty_inserts,
                            int dirty_unprotects)
{
    const char * fcn_name = "hl_col_major_scan_backward2()";
    int32_t type;
    int32_t idx;
    int32_t lag = 50;
    int32_t i;
    int32_t local_max_index;

    if ( verbose )
        HDfprintf(stdout, "%s: entering.\n", fcn_name);

    HDassert( lag > 5 );
    HDassert( max_index >= 500 );
    HDassert( max_index <= MAX_ENTRIES );

    type = 0;

    local_max_index = MIN(max_index, MAX_ENTRIES);

    if ( ( pass2 ) && ( reset_stats ) ) {

        H5C2_stats__reset(cache_ptr);
    }

    idx = local_max_index;

    while ( ( pass2 ) && ( idx >= 0 ) )
    {

        i = idx;

        while ( ( pass2 ) && ( i <= local_max_index ) && ( i <= (idx + lag) ) ) {

            type = 0;

            while ( ( pass2 ) && ( type < NUMBER_OF_ENTRY_TYPES ) )
            {
                if ( ( pass2 ) && ( do_inserts ) && ( i == idx ) &&
                     ( i <= local_max_index ) &&
                     ( ! entry_in_cache2(cache_ptr, type, i) ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(i, %d, %d) ", type, i);

                    insert_entry2(cache_ptr, type, i, dirty_inserts,
                                  H5C2__NO_FLAGS_SET);
                }

                if ( ( pass2 ) && ( i >= 0 ) && ( i <= local_max_index ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(p, %d, %d) ", type, i);

                    protect_entry2(cache_ptr, type, i);
                }

                if ( ( pass2 ) && ( i >= 0 ) &&
                     ( i <= local_max_index ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(u, %d, %d) ", type, i);

                    unprotect_entry2(cache_ptr, type, i,
                                    dirty_unprotects, H5C2__NO_FLAGS_SET);
                }

                if ( verbose )
                    HDfprintf(stdout, "\n");

                type++;
            }

            i++;
        }

        idx--;
    }

    if ( ( pass2 ) && ( display_stats ) ) {

        H5C2_stats(cache_ptr, "test cache", display_detailed_stats);
    }

    return;

} /* hl_col_major_scan_backward2() */
