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
 *		implemented in H5C.c
 */

#include <aio.h>

#include "H5private.h"

#include "h5test.h"
#include "H5Iprivate.h"
#include "H5ACprivate.h"
#include "H5MFprivate.h"
#include "H5MMprivate.h"
#include "cache_common.h"


/* global variable declarations: */

const char *FILENAME[] = {
    "cache_test",
    "cache_api_test",
    NULL
};

hid_t saved_fapl_id = H5P_DEFAULT; /* store the fapl id here between
				    * cache setup and takedown.  Note
				    * that if saved_fapl_id == H5P_DEFAULT,
				    * we assume that there is no fapl to
				    * close.
				    */

hid_t saved_fid = -1; /* store the file id here between cache setup
		       * and takedown.
		       */

H5C_t * saved_cache = NULL; /* store the pointer to the instance of
 			       * of H5C_t created by H5Fcreate()
			       * here between test cache setup and
			       * shutdown.
			       */

haddr_t saved_actual_base_addr = HADDR_UNDEF;   /* Store the address of the
                            space allocated for cache items in the file between
                            cache setup & takedown */

hbool_t write_permitted = TRUE;
hbool_t pass = TRUE; /* set to false on error */
hbool_t skip_long_tests = TRUE;
hbool_t run_full_test = TRUE;
hbool_t try_core_file_driver = TRUE;
hbool_t core_file_driver_failed = FALSE;
const char *failure_mssg = NULL;
int failures = 0;
int express_test = 0;

test_entry_t pico_entries[NUM_PICO_ENTRIES];
test_entry_t nano_entries[NUM_NANO_ENTRIES];
test_entry_t micro_entries[NUM_MICRO_ENTRIES];
test_entry_t tiny_entries[NUM_TINY_ENTRIES];
test_entry_t small_entries[NUM_SMALL_ENTRIES];
test_entry_t medium_entries[NUM_MEDIUM_ENTRIES];
test_entry_t large_entries[NUM_LARGE_ENTRIES];
test_entry_t huge_entries[NUM_HUGE_ENTRIES];
test_entry_t monster_entries[NUM_MONSTER_ENTRIES];
test_entry_t variable_entries[NUM_VARIABLE_ENTRIES];

static herr_t pico_get_load_size(const void * udata_ptr, size_t *image_len_ptr);
static herr_t nano_get_load_size(const void * udata_ptr, size_t *image_len_ptr);
static herr_t micro_get_load_size(const void * udata_ptr, size_t *image_len_ptr);
static herr_t tiny_get_load_size(const void * udata_ptr, size_t *image_len_ptr);
static herr_t small_get_load_size(const void * udata_ptr, size_t *image_len_ptr);
static herr_t medium_get_load_size(const void * udata_ptr, size_t *image_len_ptr);
static herr_t large_get_load_size(const void * udata_ptr, size_t *image_len_ptr);
static herr_t huge_get_load_size(const void * udata_ptr, size_t *image_len_ptr);
static herr_t monster_get_load_size(const void * udata_ptr, size_t *image_len_ptr);
static herr_t variable_get_load_size(const void * udata_ptr, size_t *image_len_ptr);

static void * pico_deserialize(const void * image_ptr, size_t len, void * udata_ptr,
    hbool_t * dirty_ptr);
static void * nano_deserialize(const void * image_ptr, size_t len, void * udata_ptr,
    hbool_t * dirty_ptr);
static void * micro_deserialize(const void * image_ptr, size_t len, void * udata_ptr,
    hbool_t * dirty_ptr);
static void * tiny_deserialize(const void * image_ptr, size_t len, void * udata_ptr,
    hbool_t * dirty_ptr);
static void * small_deserialize(const void * image_ptr, size_t len, void * udata_ptr,
    hbool_t * dirty_ptr);
static void * medium_deserialize(const void * image_ptr, size_t len, void * udata_ptr,
    hbool_t * dirty_ptr);
static void * large_deserialize(const void * image_ptr, size_t len, void * udata_ptr,
    hbool_t * dirty_ptr);
static void * huge_deserialize(const void * image_ptr, size_t len, void * udata_ptr,
    hbool_t * dirty_ptr);
static void * monster_deserialize(const void * image_ptr, size_t len, void * udata_ptr,
    hbool_t * dirty_ptr);
static void * variable_deserialize(const void * image_ptr, size_t len, void * udata_ptr,
    hbool_t * dirty_ptr);

static herr_t pico_image_len(void *thing, size_t *image_len_ptr);
static herr_t nano_image_len(void *thing, size_t *image_len_ptr);
static herr_t micro_image_len(void *thing, size_t *image_len_ptr);
static herr_t tiny_image_len(void *thing, size_t *image_len_ptr);
static herr_t small_image_len(void *thing, size_t *image_len_ptr);
static herr_t medium_image_len(void *thing, size_t *image_len_ptr);
static herr_t large_image_len(void *thing, size_t *image_len_ptr);
static herr_t huge_image_len(void *thing, size_t *image_len_ptr);
static herr_t monster_image_len(void *thing, size_t *image_len_ptr);
static herr_t variable_image_len(void *thing, size_t *image_len_ptr);

static herr_t pico_serialize(const H5F_t *f, hid_t dxpl_id, haddr_t addr,
		      size_t len, void * image_ptr,
		      void * thing, unsigned * flags_ptr,
		      haddr_t * new_addr_ptr, size_t * new_len_ptr,
		      void ** new_image_ptr_ptr);
static herr_t nano_serialize(const H5F_t *f, hid_t dxpl_id, haddr_t addr,
		      size_t len, void * image_ptr,
		      void * thing, unsigned * flags_ptr,
		      haddr_t * new_addr_ptr, size_t * new_len_ptr,
		      void ** new_image_ptr_ptr);
static herr_t micro_serialize(const H5F_t *f, hid_t dxpl_id, haddr_t addr,
		       size_t len, void * image_ptr,
		       void * thing, unsigned * flags_ptr,
		       haddr_t * new_addr_ptr, size_t * new_len_ptr,
		       void ** new_image_ptr_ptr);
static herr_t tiny_serialize(const H5F_t *f, hid_t dxpl_id, haddr_t addr,
		      size_t len, void * image_ptr,
		      void * thing, unsigned * flags_ptr,
		      haddr_t * new_addr_ptr, size_t * new_len_ptr,
		      void ** new_image_ptr_ptr);
static herr_t small_serialize(const H5F_t *f, hid_t dxpl_id, haddr_t addr,
		       size_t len, void * image_ptr,
		       void * thing, unsigned * flags_ptr,
		       haddr_t * new_addr_ptr, size_t * new_len_ptr,
		       void ** new_image_ptr_ptr);
static herr_t medium_serialize(const H5F_t *f, hid_t dxpl_id, haddr_t addr,
		        size_t len, void * image_ptr,
	  	        void * thing, unsigned * flags_ptr,
		        haddr_t * new_addr_ptr, size_t * new_len_ptr,
		        void ** new_image_ptr_ptr);
static herr_t large_serialize(const H5F_t *f, hid_t dxpl_id, haddr_t addr,
		       size_t len, void * image_ptr,
		       void * thing, unsigned * flags_ptr,
		       haddr_t * new_addr_ptr, size_t * new_len_ptr,
		       void ** new_image_ptr_ptr);
static herr_t huge_serialize(const H5F_t *f, hid_t dxpl_id, haddr_t addr,
		      size_t len, void * image_ptr,
	              void * thing, unsigned * flags_ptr,
	              haddr_t * new_addr_ptr, size_t * new_len_ptr,
	              void ** new_image_ptr_ptr);
static herr_t monster_serialize(const H5F_t *f, hid_t dxpl_id, haddr_t addr,
		         size_t len, void * image_ptr,
	                 void * thing, unsigned * flags_ptr,
	                 haddr_t * new_addr_ptr, size_t * new_len_ptr,
	                 void ** new_image_ptr_ptr);
static herr_t variable_serialize(const H5F_t *f, hid_t dxpl_id, haddr_t addr,
		          size_t len, void * image_ptr,
	                  void * thing, unsigned * flags_ptr,
	                  haddr_t * new_addr_ptr, size_t * new_len_ptr,
	                  void ** new_image_ptr_ptr);

static herr_t pico_free_icr(void * thing);
static herr_t nano_free_icr(void * thing);
static herr_t micro_free_icr(void * thing);
static herr_t tiny_free_icr(void * thing);
static herr_t small_free_icr(void * thing);
static herr_t medium_free_icr(void * thing);
static herr_t large_free_icr(void * thing);
static herr_t huge_free_icr(void * thing);
static herr_t monster_free_icr(void * thing);
static herr_t variable_free_icr(void * thing);


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
    monster_entries,
    variable_entries
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
    NUM_MONSTER_ENTRIES - 1,
    NUM_VARIABLE_ENTRIES - 1
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
    MONSTER_ENTRY_SIZE,
    VARIABLE_ENTRY_SIZE
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
    MONSTER_BASE_ADDR,
    VARIABLE_BASE_ADDR
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
    MONSTER_ALT_BASE_ADDR,
    VARIABLE_ALT_BASE_ADDR
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
    "monster entries -- 64 KB",
    "variable entries -- 1B - 10KB"
};


/* callback table declaration */

const H5C_class_t types[NUMBER_OF_ENTRY_TYPES] =
{
  {
    PICO_ENTRY_TYPE,
    "pico_entry",
    H5FD_MEM_DEFAULT,
    (H5C_get_load_size_func_t)pico_get_load_size,
    (H5C_deserialize_func_t)pico_deserialize,
    (H5C_image_len_func_t)pico_image_len,
    (H5C_serialize_func_t)pico_serialize,
    (H5C_free_icr_func_t)pico_free_icr,
  },
  {
    NANO_ENTRY_TYPE,
    "nano_entry",
    H5FD_MEM_DEFAULT,
    (H5C_get_load_size_func_t)nano_get_load_size,
    (H5C_deserialize_func_t)nano_deserialize,
    (H5C_image_len_func_t)nano_image_len,
    (H5C_serialize_func_t)nano_serialize,
    (H5C_free_icr_func_t)nano_free_icr,
  },
  {
    MICRO_ENTRY_TYPE,
    "micro_entry",
    H5FD_MEM_DEFAULT,
    (H5C_get_load_size_func_t)micro_get_load_size,
    (H5C_deserialize_func_t)micro_deserialize,
    (H5C_image_len_func_t)micro_image_len,
    (H5C_serialize_func_t)micro_serialize,
    (H5C_free_icr_func_t)micro_free_icr,
  },
  {
    TINY_ENTRY_TYPE,
    "tiny_entry",
    H5FD_MEM_DEFAULT,
    (H5C_get_load_size_func_t)tiny_get_load_size,
    (H5C_deserialize_func_t)tiny_deserialize,
    (H5C_image_len_func_t)tiny_image_len,
    (H5C_serialize_func_t)tiny_serialize,
    (H5C_free_icr_func_t)tiny_free_icr,
  },
  {
    SMALL_ENTRY_TYPE,
    "small_entry",
    H5FD_MEM_DEFAULT,
    (H5C_get_load_size_func_t)small_get_load_size,
    (H5C_deserialize_func_t)small_deserialize,
    (H5C_image_len_func_t)small_image_len,
    (H5C_serialize_func_t)small_serialize,
    (H5C_free_icr_func_t)small_free_icr,
  },
  {
    MEDIUM_ENTRY_TYPE,
    "medium_entry",
    H5FD_MEM_DEFAULT,
    (H5C_get_load_size_func_t)medium_get_load_size,
    (H5C_deserialize_func_t)medium_deserialize,
    (H5C_image_len_func_t)medium_image_len,
    (H5C_serialize_func_t)medium_serialize,
    (H5C_free_icr_func_t)medium_free_icr,
  },
  {
    LARGE_ENTRY_TYPE,
    "large_entry",
    H5FD_MEM_DEFAULT,
    (H5C_get_load_size_func_t)large_get_load_size,
    (H5C_deserialize_func_t)large_deserialize,
    (H5C_image_len_func_t)large_image_len,
    (H5C_serialize_func_t)large_serialize,
    (H5C_free_icr_func_t)large_free_icr,
  },
  {
    HUGE_ENTRY_TYPE,
    "huge_entry",
    H5FD_MEM_DEFAULT,
    (H5C_get_load_size_func_t)huge_get_load_size,
    (H5C_deserialize_func_t)huge_deserialize,
    (H5C_image_len_func_t)huge_image_len,
    (H5C_serialize_func_t)huge_serialize,
    (H5C_free_icr_func_t)huge_free_icr,
  },
  {
    MONSTER_ENTRY_TYPE,
    "monster_entry",
    H5FD_MEM_DEFAULT,
    (H5C_get_load_size_func_t)monster_get_load_size,
    (H5C_deserialize_func_t)monster_deserialize,
    (H5C_image_len_func_t)monster_image_len,
    (H5C_serialize_func_t)monster_serialize,
    (H5C_free_icr_func_t)monster_free_icr,
  },
  {
    VARIABLE_ENTRY_TYPE,
    "variable_entry",
    H5FD_MEM_DEFAULT,
    (H5C_get_load_size_func_t)variable_get_load_size,
    (H5C_deserialize_func_t)variable_deserialize,
    (H5C_image_len_func_t)variable_image_len,
    (H5C_serialize_func_t)variable_serialize,
    (H5C_free_icr_func_t)variable_free_icr,
  }
};

static herr_t get_load_size(const void * udata_ptr,
                          size_t *image_len_ptr);

static void * deserialize(const void * image_ptr,
                          size_t len,
                          void * udata_ptr,
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

static herr_t free_icr(test_entry_t *entry_ptr);

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
 *-------------------------------------------------------------------------
 */
void
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

        idx = (int32_t)((addr - alt_base_addrs[type]) / entry_sizes[type]);
        HDassert( ( idx >= 0 ) && ( idx <= max_indices[type] ) );
        HDassert( !((entries[type])[idx].at_main_addr) );
        HDassert( addr == (entries[type])[idx].alt_addr );

    } else {

        idx = (int32_t)((addr - base_addrs[type]) / entry_sizes[type]);
        HDassert( ( idx >= 0 ) && ( idx <= max_indices[type] ) );
        HDassert( (entries[type])[idx].at_main_addr );
        HDassert( addr == (entries[type])[idx].main_addr );
    }

    HDassert( addr == (entries[type])[idx].addr );

    *type_ptr = type;
    *index_ptr = idx;

    return;

} /* addr_to_type_and_index() */

/* Call back functions: */


/*-------------------------------------------------------------------------
 *
 * Function:    check_if_write_permitted
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
 *-------------------------------------------------------------------------
 */

herr_t
check_write_permitted(const H5F_t UNUSED * f,
                      hid_t UNUSED dxpl_id,
                      hbool_t * write_permitted_ptr)
{

    HDassert( write_permitted_ptr );
    *write_permitted_ptr = write_permitted;

    return(SUCCEED);

} /* check_write_permitted() */


/*-------------------------------------------------------------------------
 * Function:	get_load_size & friends
 *
 * Purpose:	Query the image size for loading an entry.  The helper
 *              functions funnel into get_load_size proper.
 *
 * Return:	SUCCEED
 *
 * Programmer:	Quincey Koziol
 *              5/18/10
 *
 *-------------------------------------------------------------------------
 */
static herr_t
get_load_size(const void * udata_ptr, size_t *image_len_ptr)
{
    haddr_t addr = *(const haddr_t *)udata_ptr;
    int32_t type;
    int32_t idx;
    test_entry_t * entry_ptr;
    test_entry_t * base_addr;

    addr_to_type_and_index(addr, &type, &idx);

    base_addr = entries[type];
    entry_ptr = &(base_addr[idx]);

    HDassert( entry_ptr->type >= 0 );
    HDassert( entry_ptr->type < NUMBER_OF_ENTRY_TYPES );
    HDassert( entry_ptr->index == idx );
    HDassert( entry_ptr->index >= 0 );
    HDassert( entry_ptr->index <= max_indices[type] );
    HDassert( entry_ptr == entry_ptr->self );
    HDassert( entry_ptr->addr == addr );

    *image_len_ptr = entry_ptr->size;

    return(SUCCEED);
} /* get_load_size() */

static herr_t
pico_get_load_size(const void *udata_ptr, size_t *image_len_ptr)
{
    return get_load_size(udata_ptr, image_len_ptr);
}

static herr_t
nano_get_load_size(const void *udata_ptr, size_t *image_len_ptr)
{
    return get_load_size(udata_ptr, image_len_ptr);
}

static herr_t
micro_get_load_size(const void *udata_ptr, size_t *image_len_ptr)
{
    return get_load_size(udata_ptr, image_len_ptr);
}

static herr_t
tiny_get_load_size(const void *udata_ptr, size_t *image_len_ptr)
{
    return get_load_size(udata_ptr, image_len_ptr);
}

static herr_t
small_get_load_size(const void *udata_ptr, size_t *image_len_ptr)
{
    return get_load_size(udata_ptr, image_len_ptr);
}

static herr_t
medium_get_load_size(const void *udata_ptr, size_t *image_len_ptr)
{
    return get_load_size(udata_ptr, image_len_ptr);
}

static herr_t
large_get_load_size(const void *udata_ptr, size_t *image_len_ptr)
{
    return get_load_size(udata_ptr, image_len_ptr);
}

static herr_t
huge_get_load_size(const void *udata_ptr, size_t *image_len_ptr)
{
    return get_load_size(udata_ptr, image_len_ptr);
}

static herr_t
monster_get_load_size(const void *udata_ptr, size_t *image_len_ptr)
{
    return get_load_size(udata_ptr, image_len_ptr);
}

static herr_t
variable_get_load_size(const void *udata_ptr, size_t *image_len_ptr)
{
    return get_load_size(udata_ptr, image_len_ptr);
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
 *-------------------------------------------------------------------------
 */
static void *
deserialize(const void * image_ptr, size_t len, void *udata, hbool_t * dirty_ptr)
{
    haddr_t addr = *(haddr_t *)udata;
    int32_t type;
    int32_t idx;
    test_entry_t * entry_ptr;
    test_entry_t * base_addr;

    addr_to_type_and_index(addr, &type, &idx);

    base_addr = entries[type];
    entry_ptr = &(base_addr[idx]);

    HDassert( entry_ptr->type >= 0 );
    HDassert( entry_ptr->type < NUMBER_OF_ENTRY_TYPES );
    HDassert( entry_ptr->index == idx );
    HDassert( entry_ptr->index >= 0 );
    HDassert( entry_ptr->index <= max_indices[type] );
    HDassert( entry_ptr == entry_ptr->self );
    HDassert( entry_ptr->addr == addr );
    HDassert( entry_ptr->size == len );
    HDassert( ( entry_ptr->type == VARIABLE_ENTRY_TYPE ) ||
	      ( entry_ptr->size == entry_sizes[type] ) );
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
                          (int)((idx & 0xFF00)>>8));
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
pico_deserialize(const void * image_ptr, size_t len, void * udata_ptr,
    hbool_t * dirty_ptr)
{
    return deserialize(image_ptr, len, udata_ptr, dirty_ptr);
}

void *
nano_deserialize(const void * image_ptr, size_t len, void * udata_ptr,
    hbool_t * dirty_ptr)
{
    return deserialize(image_ptr, len, udata_ptr, dirty_ptr);
}

void *
micro_deserialize(const void * image_ptr, size_t len, void * udata_ptr,
    hbool_t * dirty_ptr)
{
    return deserialize(image_ptr, len, udata_ptr, dirty_ptr);
}

void *
tiny_deserialize(const void * image_ptr, size_t len, void * udata_ptr,
    hbool_t * dirty_ptr)
{
    return deserialize(image_ptr, len, udata_ptr, dirty_ptr);
}

void *
small_deserialize(const void * image_ptr, size_t len, void * udata_ptr,
    hbool_t * dirty_ptr)
{
    return deserialize(image_ptr, len, udata_ptr, dirty_ptr);
}

void *
medium_deserialize(const void * image_ptr, size_t len, void * udata_ptr,
    hbool_t * dirty_ptr)
{
    return deserialize(image_ptr, len, udata_ptr, dirty_ptr);
}

void *
large_deserialize(const void * image_ptr, size_t len, void * udata_ptr,
    hbool_t * dirty_ptr)
{
    return deserialize(image_ptr, len, udata_ptr, dirty_ptr);
}

void *
huge_deserialize(const void * image_ptr, size_t len, void * udata_ptr,
    hbool_t * dirty_ptr)
{
    return deserialize(image_ptr, len, udata_ptr, dirty_ptr);
}

void *
monster_deserialize(const void * image_ptr, size_t len, void * udata_ptr,
    hbool_t * dirty_ptr)
{
    return deserialize(image_ptr, len, udata_ptr, dirty_ptr);
}

void *
variable_deserialize(const void * image_ptr, size_t len, void * udata_ptr,
    hbool_t * dirty_ptr)
{
    return deserialize(image_ptr, len, udata_ptr, dirty_ptr);
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
 *-------------------------------------------------------------------------
 */

herr_t
image_len(void *thing, size_t *image_len_ptr)
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
    HDassert( ( idx >= 0 ) && ( idx <= max_indices[type] ) );

    base_addr = entries[type];
    HDassert( entry_ptr == &(base_addr[idx]) );

    if ( type != VARIABLE_ENTRY_TYPE ) {

	HDassert( entry_ptr->size == entry_sizes[type] );

    } else {

	HDassert( entry_ptr->size <= entry_sizes[type] );
	HDassert( entry_ptr->size > 0 );
    }

    *image_len_ptr = entry_ptr->size;

    return(SUCCEED);

} /* image_len() */


herr_t
pico_image_len(void *thing, size_t *image_len_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == PICO_ENTRY_TYPE );
    return(image_len(thing, image_len_ptr));
}

herr_t
nano_image_len(void *thing, size_t *image_len_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == NANO_ENTRY_TYPE );
    return(image_len(thing, image_len_ptr));
}

herr_t
micro_image_len(void *thing, size_t *image_len_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == MICRO_ENTRY_TYPE );
    return(image_len(thing, image_len_ptr));
}

herr_t
tiny_image_len(void *thing, size_t *image_len_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == TINY_ENTRY_TYPE );
    return(image_len(thing, image_len_ptr));
}


herr_t
small_image_len(void *thing, size_t *image_len_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == SMALL_ENTRY_TYPE );
    return(image_len(thing, image_len_ptr));
}

herr_t
medium_image_len(void *thing, size_t *image_len_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == MEDIUM_ENTRY_TYPE );
    return(image_len(thing, image_len_ptr));
}

herr_t
large_image_len(void *thing, size_t *image_len_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == LARGE_ENTRY_TYPE );
    return(image_len(thing, image_len_ptr));
}

herr_t
huge_image_len(void *thing, size_t *image_len_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == HUGE_ENTRY_TYPE );
    return(image_len(thing, image_len_ptr));
}

herr_t
monster_image_len(void *thing, size_t *image_len_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == MONSTER_ENTRY_TYPE );
    return(image_len(thing, image_len_ptr));
}

herr_t
variable_image_len(void *thing, size_t *image_len_ptr)
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
 *-------------------------------------------------------------------------
 */

herr_t
serialize(haddr_t addr, size_t len, void * image_ptr, void * thing,
    unsigned * flags_ptr, haddr_t * new_addr_ptr, size_t * new_len_ptr,
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
    HDassert( ( idx >= 0 ) && ( idx <= max_indices[type] ) );

    base_addr = entries[type];

    HDassert( entry_ptr == &(base_addr[idx]) );

    HDassert( entry_ptr->num_flush_ops >= 0 );
    HDassert( entry_ptr->num_flush_ops < MAX_FLUSH_OPS );

    if ( entry_ptr->num_flush_ops > 0 ) {

        for ( i = 0; i < entry_ptr->num_flush_ops; i++ )
        {
            HDassert( entry_ptr->file_ptr );

            execute_flush_op(entry_ptr->file_ptr,
                              entry_ptr,
                              &((entry_ptr->flush_ops)[i]),
                              flags_ptr);
        }
        entry_ptr->num_flush_ops = 0;
        entry_ptr->flush_op_self_resize_in_progress = FALSE;

	/* This looks wrong, but it isn't -- *flags_ptr will be modified
	 * by execute_flush_op() only if the target is this entry --
	 * and the flags set will accumulate over the set of calls in
	 * the for loop.
	 */

	if ( ( pass ) &&
             ( ((*flags_ptr) & H5C__SERIALIZE_RESIZED_FLAG) != 0 ) ) {

	    /* re-allocate *image_ptr, and place the new pointer in
	     * *new_image_ptr_ptr.
	     */
            image_ptr = H5MM_xfree(image_ptr);

            if ( image_ptr != NULL ) {

                ret_val = FAIL;
                pass = FALSE;
                failure_mssg = "couldn't free image_ptr.";
            }

            if ( pass ) {

                HDassert( entry_ptr->type == VARIABLE_ENTRY_TYPE );
                HDassert( entry_ptr->size > 0 );
                HDassert( entry_ptr->size <= VARIABLE_ENTRY_SIZE );

                image_ptr = H5MM_malloc((size_t)(entry_ptr->size));

                if ( image_ptr == NULL ) {

                    ret_val = FAIL;
                    pass = FALSE;
                    failure_mssg = "couldn't allocate new image.";

                } else {

                    *new_image_ptr_ptr = image_ptr;
                    *new_len_ptr = entry_ptr->size;
		    len = entry_ptr->size;

                }
            }
	}

	if ( ((*flags_ptr) & H5C__SERIALIZE_MOVED_FLAG) != 0 ) {

            HDassert( ((*flags_ptr) | H5C__SERIALIZE_RESIZED_FLAG) != 0 );

	    /* place the new address in *new_addr_ptr */

            *new_addr_ptr = entry_ptr->addr;
	}
    }

    /* null out the image to avoid spurious failures */
    HDmemset(image_ptr, 0, len);

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
pico_serialize(const H5F_t UNUSED *f, hid_t UNUSED dxpl_id, haddr_t addr,
    size_t len, void * image_ptr, void * thing, unsigned * flags_ptr,
    haddr_t * new_addr_ptr, size_t * new_len_ptr, void ** new_image_ptr_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == PICO_ENTRY_TYPE );
    return(serialize(addr, len, image_ptr, thing, flags_ptr,
                     new_addr_ptr, new_len_ptr, new_image_ptr_ptr));
}

herr_t
nano_serialize(const H5F_t UNUSED *f, hid_t UNUSED dxpl_id, haddr_t addr,
    size_t len, void * image_ptr, void * thing, unsigned * flags_ptr,
    haddr_t * new_addr_ptr, size_t * new_len_ptr, void ** new_image_ptr_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == NANO_ENTRY_TYPE );
    return(serialize(addr, len, image_ptr, thing, flags_ptr,
                     new_addr_ptr, new_len_ptr, new_image_ptr_ptr));
}

herr_t
micro_serialize(const H5F_t UNUSED *f, hid_t UNUSED dxpl_id, haddr_t addr,
    size_t len, void * image_ptr, void * thing, unsigned * flags_ptr,
    haddr_t * new_addr_ptr, size_t * new_len_ptr, void ** new_image_ptr_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == MICRO_ENTRY_TYPE );
    return(serialize(addr, len, image_ptr, thing, flags_ptr,
                     new_addr_ptr, new_len_ptr, new_image_ptr_ptr));
}

herr_t
tiny_serialize(const H5F_t UNUSED *f, hid_t UNUSED dxpl_id, haddr_t addr,
    size_t len, void * image_ptr, void * thing, unsigned * flags_ptr,
    haddr_t * new_addr_ptr, size_t * new_len_ptr, void ** new_image_ptr_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == TINY_ENTRY_TYPE );
    return(serialize(addr, len, image_ptr, thing, flags_ptr,
                     new_addr_ptr, new_len_ptr, new_image_ptr_ptr));
}

herr_t
small_serialize(const H5F_t UNUSED *f, hid_t UNUSED dxpl_id, haddr_t addr,
    size_t len, void * image_ptr, void * thing, unsigned * flags_ptr,
    haddr_t * new_addr_ptr, size_t * new_len_ptr, void ** new_image_ptr_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == SMALL_ENTRY_TYPE );
    return(serialize(addr, len, image_ptr, thing, flags_ptr,
                     new_addr_ptr, new_len_ptr, new_image_ptr_ptr));
}

herr_t
medium_serialize(const H5F_t UNUSED *f, hid_t UNUSED dxpl_id, haddr_t addr,
    size_t len, void * image_ptr, void * thing, unsigned * flags_ptr,
    haddr_t * new_addr_ptr, size_t * new_len_ptr, void ** new_image_ptr_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == MEDIUM_ENTRY_TYPE );
    return(serialize(addr, len, image_ptr, thing, flags_ptr,
                     new_addr_ptr, new_len_ptr, new_image_ptr_ptr));
}

herr_t
large_serialize(const H5F_t UNUSED *f, hid_t UNUSED dxpl_id, haddr_t addr,
    size_t len, void * image_ptr, void * thing, unsigned * flags_ptr,
    haddr_t * new_addr_ptr, size_t * new_len_ptr, void ** new_image_ptr_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == LARGE_ENTRY_TYPE );
    return(serialize(addr, len, image_ptr, thing, flags_ptr,
                     new_addr_ptr, new_len_ptr, new_image_ptr_ptr));
}

herr_t
huge_serialize(const H5F_t UNUSED *f, hid_t UNUSED dxpl_id, haddr_t addr,
    size_t len, void * image_ptr, void * thing, unsigned * flags_ptr,
    haddr_t * new_addr_ptr, size_t * new_len_ptr, void ** new_image_ptr_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == HUGE_ENTRY_TYPE );
    return(serialize(addr, len, image_ptr, thing, flags_ptr,
                     new_addr_ptr, new_len_ptr, new_image_ptr_ptr));
}

herr_t
monster_serialize(const H5F_t UNUSED *f, hid_t UNUSED dxpl_id, haddr_t addr,
    size_t len, void * image_ptr, void * thing, unsigned * flags_ptr,
    haddr_t * new_addr_ptr, size_t * new_len_ptr, void ** new_image_ptr_ptr)
{
    HDassert( ((test_entry_t *)thing)->type == MONSTER_ENTRY_TYPE );
    return(serialize(addr, len, image_ptr, thing, flags_ptr,
                     new_addr_ptr, new_len_ptr, new_image_ptr_ptr));
}

herr_t
variable_serialize(const H5F_t UNUSED *f, hid_t UNUSED dxpl_id, haddr_t addr,
    size_t len, void * image_ptr, void * thing, unsigned * flags_ptr,
    haddr_t * new_addr_ptr, size_t * new_len_ptr, void ** new_image_ptr_ptr)
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
 *-------------------------------------------------------------------------
 */

herr_t
free_icr(test_entry_t *entry_ptr)
{
    test_entry_t * base_addr;

    HDassert( entry_ptr );

    base_addr = entries[entry_ptr->type];

    HDassert( entry_ptr->index >= 0 );
    HDassert( entry_ptr->index <= max_indices[entry_ptr->type] );
    HDassert( entry_ptr == &(base_addr[entry_ptr->index]) );
    HDassert( entry_ptr == entry_ptr->self );
    HDassert( entry_ptr->cache_ptr != NULL );
    HDassert( entry_ptr->cache_ptr->magic == H5C__H5C_T_MAGIC );
    HDassert( ( entry_ptr->header.destroy_in_progress ) ||
              ( entry_ptr->header.addr == entry_ptr->addr ) );
    HDassert( entry_ptr->header.size == entry_ptr->size );
    HDassert( ( entry_ptr->type == VARIABLE_ENTRY_TYPE ) ||
	      ( entry_ptr->size == entry_sizes[entry_ptr->type] ) );

    if ( entry_ptr->num_pins > 0 ) {
        int i;

	for ( i = 0; i < entry_ptr->num_pins; i++ ) {
            test_entry_t * pinned_entry_ptr;
            test_entry_t * pinned_base_addr;

	    pinned_base_addr = entries[entry_ptr->pin_type[i]];
	    pinned_entry_ptr = &(pinned_base_addr[entry_ptr->pin_idx[i]]);

	    HDassert( 0 <= pinned_entry_ptr->type );
            HDassert( pinned_entry_ptr->type < NUMBER_OF_ENTRY_TYPES );
	    HDassert( pinned_entry_ptr->type == entry_ptr->pin_type[i] );
	    HDassert( pinned_entry_ptr->index >= 0 );
	    HDassert( pinned_entry_ptr->index <=
		      max_indices[pinned_entry_ptr->type] );
	    HDassert( pinned_entry_ptr->index == entry_ptr->pin_idx[i] );
	    HDassert( pinned_entry_ptr == pinned_entry_ptr->self );
	    HDassert( pinned_entry_ptr->header.is_pinned );
	    HDassert( pinned_entry_ptr->is_pinned );
	    HDassert( pinned_entry_ptr->pinning_ref_count > 0 );

	    pinned_entry_ptr->pinning_ref_count--;

	    if ( pinned_entry_ptr->pinning_ref_count <= 0 ) {

                HDassert( pinned_entry_ptr->file_ptr );

		unpin_entry(pinned_entry_ptr->file_ptr,
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
pico_free_icr(void * thing)
{
    test_entry_t * entry_ptr = (test_entry_t *)thing;

    HDassert( entry_ptr->type == PICO_ENTRY_TYPE );

    return(free_icr(entry_ptr));
}

herr_t
nano_free_icr(void * thing)
{
    test_entry_t * entry_ptr = (test_entry_t *)thing;

    HDassert( entry_ptr->type == NANO_ENTRY_TYPE );

    return(free_icr(entry_ptr));
}

herr_t
micro_free_icr(void * thing)
{
    test_entry_t * entry_ptr = (test_entry_t *)thing;

    HDassert( entry_ptr->type == MICRO_ENTRY_TYPE );

    return(free_icr(entry_ptr));
}

herr_t
tiny_free_icr(void * thing)
{
    test_entry_t * entry_ptr = (test_entry_t *)thing;

    HDassert( entry_ptr->type == TINY_ENTRY_TYPE );

    return(free_icr(entry_ptr));
}

herr_t
small_free_icr(void * thing)
{
    test_entry_t * entry_ptr = (test_entry_t *)thing;

    HDassert( entry_ptr->type == SMALL_ENTRY_TYPE );

    return(free_icr(entry_ptr));
}

herr_t
medium_free_icr(void * thing)
{
    test_entry_t * entry_ptr = (test_entry_t *)thing;

    HDassert( entry_ptr->type == MEDIUM_ENTRY_TYPE );

    return(free_icr(entry_ptr));
}

herr_t
large_free_icr(void * thing)
{
    test_entry_t * entry_ptr = (test_entry_t *)thing;

    HDassert( entry_ptr->type == LARGE_ENTRY_TYPE );

    return(free_icr(entry_ptr));
}

herr_t
huge_free_icr(void * thing)
{
    test_entry_t * entry_ptr = (test_entry_t *)thing;

    HDassert( entry_ptr->type == HUGE_ENTRY_TYPE );

    return(free_icr(entry_ptr));
}

herr_t
monster_free_icr(void * thing)
{
    test_entry_t * entry_ptr = (test_entry_t *)thing;

    HDassert( entry_ptr->type == MONSTER_ENTRY_TYPE );

    return(free_icr(entry_ptr));
}

herr_t
variable_free_icr(void * thing)
{
    test_entry_t * entry_ptr = (test_entry_t *)thing;

    HDassert( entry_ptr->type == VARIABLE_ENTRY_TYPE );

    return(free_icr(entry_ptr));
}


/**************************************************************************/
/**************************************************************************/
/************************** test utility functions: ***********************/
/**************************************************************************/
/**************************************************************************/

/*-------------------------------------------------------------------------
 * Function:	add_flush_op
 *
 * Purpose:	Do nothing if pass is FALSE on entry.
 *
 *              Otherwise, add the specified flush operation to the
 *              target instance of test_entry_t.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              9/1/06
 *
 *-------------------------------------------------------------------------
 */

void
add_flush_op(int target_type,
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
	      ( target_idx <= max_indices[target_type] ) );
    HDassert( ( 0 <= op_code ) && ( op_code <= FLUSH_OP__MAX_OP ) );
    HDassert( ( op_code != FLUSH_OP__RESIZE ) ||
	      ( type == VARIABLE_ENTRY_TYPE ) );
    HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
    HDassert( ( 0 <= idx ) && ( idx <= max_indices[type] ) );
    HDassert( ( flag == TRUE ) || ( flag == FALSE ) );
    HDassert( new_size <= VARIABLE_ENTRY_SIZE );

    if ( pass ) {

        target_base_addr = entries[target_type];
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

} /* add_flush_op() */


/*-------------------------------------------------------------------------
 * Function:	create_pinned_entry_dependency
 *
 * Purpose:	Do nothing if pass is FALSE on entry.
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
 *-------------------------------------------------------------------------
 */

void
create_pinned_entry_dependency(H5F_t * file_ptr,
		               int pinning_type,
                               int pinning_idx,
	                       int pinned_type,
	                       int pinned_idx)
{
    test_entry_t * pinning_base_addr;
    test_entry_t * pinning_entry_ptr;
    test_entry_t * pinned_base_addr;
    test_entry_t * pinned_entry_ptr;

    if ( pass ) {

        HDassert( ( 0 <= pinning_type ) &&
 	          ( pinning_type < NUMBER_OF_ENTRY_TYPES ) );
        HDassert( ( 0 <= pinning_idx ) &&
	          ( pinning_idx <= max_indices[pinning_type] ) );
        HDassert( ( 0 <= pinned_type ) &&
	          ( pinned_type < NUMBER_OF_ENTRY_TYPES ) );
        HDassert( ( 0 <= pinned_idx ) &&
	          ( pinned_idx <= max_indices[pinned_type] ) );

        pinning_base_addr = entries[pinning_type];
        pinning_entry_ptr = &(pinning_base_addr[pinning_idx]);

        pinned_base_addr = entries[pinned_type];
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

	    protect_entry(file_ptr, pinned_type, pinned_idx);
	    unprotect_entry(file_ptr, pinned_type, pinned_idx, H5C__PIN_ENTRY_FLAG);
	}

	(pinned_entry_ptr->pinning_ref_count)++;
    }

    return;

} /* create_pinned_entry_dependency() */


/*-------------------------------------------------------------------------
 * Function:	dirty_entry
 *
 * Purpose:	Given a pointer to a cache, an entry type, and an index,
 *		dirty the target entry.
 *
 *		If the dirty_pin parameter is true, verify that the
 *		target entry is in the cache and is pinned.  If it
 *		isn't, scream and die.  If it is, use the
 *		H5C_mark_entry_dirty() call to dirty it.
 *
 *		Do nothing if pass is false on entry.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/10/04
 *
 *-------------------------------------------------------------------------
 */

void
dirty_entry(H5F_t * file_ptr,
            int32_t type,
            int32_t idx,
	    hbool_t dirty_pin)
{
    H5C_t * cache_ptr;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    if ( pass ) {

        cache_ptr = file_ptr->shared->cache;

        HDassert( cache_ptr );
        HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
        HDassert( ( 0 <= idx ) && ( idx <= max_indices[type] ) );

        if ( dirty_pin ) {

	    if ( ! entry_in_cache(cache_ptr, type, idx) ) {

		pass = FALSE;
                failure_mssg = "entry to be dirty pinned is not in cache.";

	    } else {

                base_addr = entries[type];
                entry_ptr = &(base_addr[idx]);

	        HDassert( entry_ptr->index == idx );
	        HDassert( entry_ptr->type == type );
                HDassert( entry_ptr == entry_ptr->self );

		if ( ! ( (entry_ptr->header).is_pinned ) ) {

                    pass = FALSE;
                    failure_mssg = "entry to be dirty pinned is not pinned.";

                } else {

		    mark_entry_dirty(file_ptr, type, idx);

		}
	    }
        } else {

	    protect_entry(file_ptr, type, idx);
            unprotect_entry(file_ptr, type, idx, H5C__DIRTIED_FLAG);
	}
    }

    return;

} /* dirty_entry() */


/*-------------------------------------------------------------------------
 * Function:	execute_flush_op
 *
 * Purpose:	Given a pointer to an instance of struct flush_op, execute
 * 		it.
 *
 *		Do nothing if pass is false on entry.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              9/1/06
 *
 *-------------------------------------------------------------------------
 */

void
execute_flush_op(H5F_t * file_ptr,
		 struct test_entry_t * entry_ptr,
		 struct flush_op * op_ptr,
		 unsigned * flags_ptr)
{
    /* const char * fcn_name = "execute_flush_op()"; */
    H5C_t * cache_ptr;

    if ( pass ) {

        cache_ptr = file_ptr->shared->cache;
        HDassert( cache_ptr != NULL );
        HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );
        HDassert( entry_ptr != NULL );
        HDassert( entry_ptr = entry_ptr->self );
        HDassert( entry_ptr->header.addr == entry_ptr->addr );
        HDassert( ( entry_ptr->flush_op_self_resize_in_progress ) ||
                  ( entry_ptr->header.size == entry_ptr->size ) );
        HDassert( op_ptr != NULL );
        HDassert( ( 0 <= entry_ptr->type ) &&
	          ( entry_ptr->type < NUMBER_OF_ENTRY_TYPES ) );
        HDassert( ( 0 <= entry_ptr->index ) &&
                  ( entry_ptr->index <= max_indices[entry_ptr->type] ) );
        HDassert( ( 0 <= op_ptr->type ) &&
                  ( op_ptr->type < NUMBER_OF_ENTRY_TYPES ) );
        HDassert( ( 0 <= op_ptr->idx ) &&
                  ( op_ptr->idx <= max_indices[op_ptr->type] ) );
        HDassert( ( op_ptr->flag == FALSE ) || ( op_ptr->flag == TRUE ) );
        HDassert( flags_ptr != NULL );

	switch ( op_ptr->op_code )
	{
	    case FLUSH_OP__NO_OP:
		break;

	    case FLUSH_OP__DIRTY:
		HDassert( ( entry_ptr->type != op_ptr->type ) ||
			  ( entry_ptr->index != op_ptr->idx ) );

		dirty_entry(file_ptr, op_ptr->type, op_ptr->idx,
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

		    (*flags_ptr) |= H5C__SERIALIZE_RESIZED_FLAG;

		    entry_ptr->flush_op_self_resize_in_progress = TRUE;

		} else {

		    /* change the size of some other entry */

		    resize_entry(file_ptr, op_ptr->type, op_ptr->idx,
                                 op_ptr->size, op_ptr->flag);
		}
		break;

	    case FLUSH_OP__MOVE:
		if ( ( entry_ptr->type == op_ptr->type ) &&
                     ( entry_ptr->index == op_ptr->idx ) ) {

                    /* the flush operation is acting on the entry to
		     * which it is attached.  Handle this here:
		     */

		    HDassert( ((*flags_ptr) & H5C__SERIALIZE_RESIZED_FLAG)
                              != 0 );

                    (*flags_ptr) |= H5C__SERIALIZE_MOVED_FLAG;

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

		    move_entry(cache_ptr, op_ptr->type, op_ptr->idx,
			          op_ptr->flag);
                }
		break;

	    default:
                pass = FALSE;
                failure_mssg = "Undefined flush op code.";
		break;
	}
    }

    return;

} /* execute_flush_op() */


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

    H5C_TEST__SEARCH_INDEX(cache_ptr, entry_ptr->addr, test_ptr)

    if ( test_ptr != NULL ) {

        in_cache = TRUE;
        HDassert( test_ptr == (H5C_cache_entry_t *)entry_ptr );
        HDassert( entry_ptr->addr == entry_ptr->header.addr );
    }

    return(in_cache);

} /* entry_in_cache() */


/*-------------------------------------------------------------------------
 * Function:	recommend_core_file_driver
 *
 * Purpose:	Using the core file driver for test files allows us to avoid
 * 		disk I/O, and thust offers the potential of speeding up
 * 		tests.  However, if the target system has insufficient
 * 		physical memory to accomodate the test file, using the
 * 		core file driver will actually slow the test down, as the
 * 		target system will page fault continuously.
 *
 * 		The purpose of this function is to determine whether it
 * 		makes sense to use the core file driver on this particular
 * 		system.
 *
 * 		To do this, we need to determine a number of facts about
 * 		the host system -- most particularly how much physical
 * 		RAM it has.  This information is not always available.
 * 		If it is not, we assume the worst and report that using
 * 		the core file driver would be ill advised.
 *
 * Return:	TRUE 	If use of the core file driver seems a good idea.
 *
 *              FALSE   If available data on the host system suggests that
 *              	user of the core file driver is ill advised, or
 *              	if no information is available upon which to base
 *              	a suggestion.
 *
 * Programmer:	John Mainzer
 *              1/13/09
 *
 *-------------------------------------------------------------------------
 */

hbool_t
recommend_core_file_driver(void)
{
    const char * fcn_name = "recommend_core_file_driver()";
    hbool_t core_file_driver_recomended = FALSE;
    hbool_t verbose = FALSE;
    unsigned long long free_ram = 0;
    unsigned long long free_swap = 0;
    unsigned long long physical_ram = 0;

    free_ram = H5_get_free_ram();
    free_swap = H5_get_free_swap();
    physical_ram = H5_get_physical_ram();

    if ( ( physical_ram > ((((unsigned long long)MAX_ADDR) / 5) * 9) ) ||
         ( ( physical_ram > ((((unsigned long long)MAX_ADDR) / 2) * 3) ) &&
           ( ( ((unsigned long long)MAX_ADDR) < free_ram ) ||
             ( ((unsigned long long)MAX_ADDR) < free_swap ) ) ) ) {

	core_file_driver_recomended = TRUE;
    }

    if ( verbose ) {

        HDfprintf(stdout, "%s: physical_ram = %lld\n",
                  fcn_name, (long long)physical_ram);
        HDfprintf(stdout, "%s: free_ram = %lld\n",
                  fcn_name, (long long)free_ram);
        HDfprintf(stdout, "%s: free_swap = %lld\n",
                  fcn_name, (long long)free_swap);
        HDfprintf(stdout, "%s: MAX_ADDR = %lld\n",
                  fcn_name, (long long)MAX_ADDR);
	HDfprintf(stdout, "%s: core_file_driver_recomended = %d\n",
		  fcn_name, (int)core_file_driver_recomended);
    }

    return(core_file_driver_recomended);

} /* recommend_core_file_driver() */


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
 *-------------------------------------------------------------------------
 */

void
reset_entries(void)

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
            base_addr[j].serialized = FALSE;
            base_addr[j].destroyed = FALSE;

            addr += (haddr_t)entry_size;
            alt_addr += (haddr_t)entry_size;
        }
    }

    return;

} /* reset_entries() */


/*-------------------------------------------------------------------------
 * Function:    resize_entry
 *
 * Purpose:     Given a pointer to a cache, an entry type, an index, and
 * 		a new size, set the size of the target entry to the new size.
 *
 *		Note that at present, the type of the entry must be
 * 		VARIABLE_ENTRY_TYPE.
 *
 *              Do nothing if pass is false on entry.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer
 *              1/11/08
 *
 *-------------------------------------------------------------------------
 */

void
resize_entry(H5F_t * file_ptr,
             int32_t type,
             int32_t idx,
             size_t new_size,
	     hbool_t in_cache)
{
    H5C_t * cache_ptr;
    herr_t result;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    if ( pass ) {

        cache_ptr = file_ptr->shared->cache;
        HDassert( cache_ptr );
        HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
        HDassert( ( 0 <= idx ) && ( idx <= max_indices[type] ) );
        HDassert( type == VARIABLE_ENTRY_TYPE ) ;
        HDassert( ( 0 < new_size ) && ( new_size <= entry_sizes[type] ) );

        if ( in_cache ) {

            if ( ! entry_in_cache(cache_ptr, type, idx) ) {

                pass = FALSE;
                failure_mssg = "entry not in cache.";

            } else {

                base_addr = entries[type];
                entry_ptr = &(base_addr[idx]);

                HDassert( entry_ptr->index == idx );
                HDassert( entry_ptr->type == type );
                HDassert( entry_ptr->cache_ptr == cache_ptr );
                HDassert( entry_ptr == entry_ptr->self );

                if ( ! ( (entry_ptr->header).is_pinned || (entry_ptr->header).is_protected ) ) {

                    pass = FALSE;
                    failure_mssg = "entry to be resized is not pinned or protected.";

                } else {

                    entry_ptr->size = new_size;

                    result = H5C_resize_entry((void *)entry_ptr, new_size);
                    entry_ptr->is_dirty = TRUE;

                    if ( result != SUCCEED ) {

                        pass = FALSE;
                        failure_mssg = "error(s) in H5C_resize_entry().";

                    } else {

                        HDassert( entry_ptr->size = (entry_ptr->header).size );

                    }
                }
            }
        } else {

	    protect_entry(file_ptr, type, idx);
            resize_entry(file_ptr, type, idx, new_size, TRUE);
	    unprotect_entry(file_ptr, type, idx, H5C__DIRTIED_FLAG);
	}
    }

    return;

} /* resize_entry() */


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
 *-------------------------------------------------------------------------
 */

void
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
                if ( ( base_addr[j].header.is_dirty ) ||
		     ( base_addr[j].is_dirty ) ) {

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
 * Function:	verify_entry_status
 *
 * Purpose:	Verify that a list of entries have the expected status.
 * 		If any discrepencies are found, set the failure message
 * 		and set pass to FALSE.
 *
 *		Do nothing if pass is FALSE on entry.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              10/8/04
 *
 *-------------------------------------------------------------------------
 */

void
verify_entry_status(H5C_t * cache_ptr,
		    int tag,
		    int num_entries,
		    struct expected_entry_status expected[])
{
    static char    msg[128];
    hbool_t        in_cache = FALSE; /* will set to TRUE if necessary */
    int            i;
    test_entry_t * entry_ptr;
    test_entry_t * base_addr;

    i = 0;
    while ( ( pass ) && ( i < num_entries ) )
    {
        base_addr = entries[expected[i].entry_type];
	entry_ptr = &(base_addr[expected[i].entry_index]);

	if ( ( ! expected[i].in_cache ) &&
	     ( ( expected[i].is_dirty ) ||
	       ( expected[i].is_protected ) ||
	       ( expected[i].is_pinned ) ) ) {

	    pass = FALSE;
	    sprintf(msg, "%d: Contradictory data in expected[%d].\n", tag, i);
	    failure_mssg = msg;
	}

        if ( pass ) {

	    in_cache = entry_in_cache(cache_ptr, expected[i].entry_type,
		                       expected[i].entry_index);

	    if ( in_cache != expected[i].in_cache ) {

	        pass = FALSE;
	        sprintf(msg,
		      "%d entry (%d, %d) in cache actual/expected = %d/%d.\n",
		      tag,
		      (int)expected[i].entry_type,
		      (int)expected[i].entry_index,
		      (int)in_cache,
		      (int)expected[i].in_cache);
	        failure_mssg = msg;
	    }
	}

        if ( pass ) {

	    if ( entry_ptr->size != expected[i].size ) {

	        pass = FALSE;
	        sprintf(msg,
                        "%d entry (%d, %d) size actualexpected = %ld/%ld.\n",
			tag,
	                (int)expected[i].entry_type,
		        (int)expected[i].entry_index,
		        (long)(entry_ptr->size),
		        (long)expected[i].size);
	        failure_mssg = msg;
	    }
	}

        if ( ( pass ) && ( in_cache ) ) {

	    if ( entry_ptr->header.size != expected[i].size ) {

	        pass = FALSE;
	        sprintf(msg,
                        "%d entry (%d, %d) header size actual/expected = %ld/%ld.\n",
			tag,
		        (int)expected[i].entry_type,
		        (int)expected[i].entry_index,
		        (long)(entry_ptr->header.size),
		        (long)expected[i].size);
	        failure_mssg = msg;
	    }
	}

	if ( pass ) {

	    if ( entry_ptr->at_main_addr != expected[i].at_main_addr ) {

	        pass = FALSE;
	        sprintf(msg,
                      "%d entry (%d, %d) at main addr actual/expected = %d/%d.\n",
		      tag,
		      (int)expected[i].entry_type,
		      (int)expected[i].entry_index,
		      (int)(entry_ptr->at_main_addr),
		      (int)expected[i].at_main_addr);
	        failure_mssg = msg;
	    }
	}

	if ( pass ) {

	    if ( entry_ptr->is_dirty != expected[i].is_dirty ) {

	        pass = FALSE;
	        sprintf(msg,
                      "%d entry (%d, %d) is_dirty actual/expected = %d/%d.\n",
		      tag,
		      (int)expected[i].entry_type,
		      (int)expected[i].entry_index,
		      (int)(entry_ptr->is_dirty),
		      (int)expected[i].is_dirty);
	        failure_mssg = msg;
	    }
	}

	if ( ( pass ) && ( in_cache ) ) {

	    if ( entry_ptr->header.is_dirty != expected[i].is_dirty ) {

	        pass = FALSE;
	        sprintf(msg,
                      "%d entry (%d, %d) header is_dirty actual/expected = %d/%d.\n",
		      tag,
		      (int)expected[i].entry_type,
		      (int)expected[i].entry_index,
		      (int)(entry_ptr->header.is_dirty),
		      (int)expected[i].is_dirty);
	        failure_mssg = msg;
	    }
	}

	if ( pass ) {

	    if ( entry_ptr->is_protected != expected[i].is_protected ) {

	        pass = FALSE;
	        sprintf(msg,
                      "%d entry (%d, %d) is_protected actual/expected = %d/%d.\n",
		      tag,
		      (int)expected[i].entry_type,
		      (int)expected[i].entry_index,
		      (int)(entry_ptr->is_protected),
		      (int)expected[i].is_protected);
	        failure_mssg = msg;
	    }
	}

	if ( ( pass ) && ( in_cache ) ) {

	    if ( entry_ptr->header.is_protected != expected[i].is_protected ) {

	        pass = FALSE;
	        sprintf(msg,
                      "%d entry (%d, %d) header is_protected actual/expected = %d/%d.\n",
		      tag,
		      (int)expected[i].entry_type,
		      (int)expected[i].entry_index,
		      (int)(entry_ptr->header.is_protected),
		      (int)expected[i].is_protected);
	        failure_mssg = msg;
	    }
	}

	if ( pass ) {

	    if ( entry_ptr->is_pinned != expected[i].is_pinned ) {

	        pass = FALSE;
	        sprintf(msg,
                      "%d entry (%d, %d) is_pinned actual/expected = %d/%d.\n",
		      tag,
		      (int)expected[i].entry_type,
		      (int)expected[i].entry_index,
		      (int)(entry_ptr->is_pinned),
		      (int)expected[i].is_pinned);
	        failure_mssg = msg;
	    }
	}

	if ( ( pass ) && ( in_cache ) ) {

	    if ( entry_ptr->header.is_pinned != expected[i].is_pinned ) {

	        pass = FALSE;
	        sprintf(msg,
                  "%d entry (%d, %d) header is_pinned actual/expected = %d/%d.\n",
		  tag,
		  (int)expected[i].entry_type,
		  (int)expected[i].entry_index,
		  (int)(entry_ptr->header.is_pinned),
		  (int)expected[i].is_pinned);
	        failure_mssg = msg;
	    }
	}

	if ( pass ) {

            if ( ( entry_ptr->deserialized != expected[i].deserialized ) ||
	         ( entry_ptr->serialized != expected[i].serialized ) ||
	         ( entry_ptr->destroyed != expected[i].destroyed ) ) {

	        pass = FALSE;
                sprintf(msg,
                        "%d entry (%d,%d) deserialized = %d(%d), serialized = %d(%d), dest = %d(%d)\n",
			tag,
		        (int)expected[i].entry_type,
		        (int)expected[i].entry_index,
		        (int)(entry_ptr->deserialized),
		        (int)(expected[i].deserialized),
		        (int)(entry_ptr->serialized),
		        (int)(expected[i].serialized),
		        (int)(entry_ptr->destroyed),
		        (int)(expected[i].destroyed));
                failure_mssg = msg;
            }
        }
	i++;
    } /* while */

    return;

} /* verify_entry_status() */


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
 *-------------------------------------------------------------------------
 */

void
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


/*****************************************************************************
 *
 * Function:    setup_cache()
 *
 * Purpose:     Open an HDF file.  This will allocate an instance and
 * 		initialize an associated instance of H5C_t.  However,
 * 		we want to test an instance of H5C_t, so allocate and
 * 		initialize one with the file ID returned by the call to
 * 		H5Fcreate().  Return a pointer to this instance of H5C_t.
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
 * Return:      Success:        Ptr to H5C_t
 *
 *              Failure:        NULL
 *
 * Programmer:  JRM -- 9/13/07
 *
 *****************************************************************************/

H5F_t *
setup_cache(size_t max_cache_size,
            size_t min_clean_size)
{
    const char * fcn_name = "setup_cache()";
    char filename[512];
    hbool_t show_progress = FALSE;
    hbool_t verbose = TRUE;
    int mile_stone = 1;
    hid_t fid = -1;
    H5F_t * file_ptr = NULL;
    H5C_t * cache_ptr = NULL;
    H5F_t * ret_val = NULL;
    haddr_t actual_base_addr;
    hid_t fapl_id = H5P_DEFAULT;

    if ( show_progress ) /* 1 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    saved_fid = -1;

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
    if ( pass ) {

        if ( h5_fixname(FILENAME[0], H5P_DEFAULT, filename, sizeof(filename))
            == NULL ) {

            pass = FALSE;
            failure_mssg = "h5_fixname() failed.\n";
        }
    }

    if ( show_progress ) /* 2 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    if ( ( pass ) && ( try_core_file_driver ) ) {

	if ( (fapl_id = H5Pcreate(H5P_FILE_ACCESS)) == FAIL ) {

	    pass = FALSE;
	    failure_mssg = "H5Pcreate(H5P_FILE_ACCESS) failed.\n";
        }
	else if ( H5Pset_fapl_core(fapl_id, MAX_ADDR, FALSE) < 0 ) {

	    H5Pclose(fapl_id);
	    fapl_id = H5P_DEFAULT;
	    pass = FALSE;
	    failure_mssg = "H5P_set_fapl_core() failed.\n";
        }
	else if ( (fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id))
	          < 0 ) {

	    core_file_driver_failed = TRUE;

            if ( verbose ) {
                HDfprintf(stdout, "%s: H5Fcreate() with CFD failed.\n", fcn_name);
            }

        } else {

	    saved_fapl_id = fapl_id;
	}
    }

    if ( show_progress ) /* 3 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    /* if we either aren't using the core file driver, or a create
     * with the core file driver failed, try again with a regular file.
     * If this fails, we are cooked.
     */
    if ( ( pass ) && ( fid < 0 ) ) {

        fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id);

	saved_fid = fid;

        if ( fid < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Fcreate() failed.";

            if ( verbose ) {
                HDfprintf(stdout, "%s: H5Fcreate() failed.\n", fcn_name);
            }
        }
    }

    if ( show_progress ) /* 4 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    if ( pass ) {

	HDassert( fid >= 0 );

	saved_fid = fid;

        if ( H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Fflush() failed.";

            if ( verbose ) {
                HDfprintf(stdout, "%s: H5Fflush() failed.\n", fcn_name);
            }

        } else {

            file_ptr = (H5F_t *)H5I_object_verify(fid, H5I_FILE);

	    if ( file_ptr == NULL ) {

                pass = FALSE;
                failure_mssg = "Can't get file_ptr.";

                if ( verbose ) {
                    HDfprintf(stdout, "%s: H5Fflush() failed.\n", fcn_name);
                }
	    }
        }
    }

    if ( show_progress ) /* 5 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    if ( pass ) {

        /* A bit of fancy footwork here:
	 *
	 * The call to H5Fcreate() allocates an instance of H5C_t,
	 * initializes it, and stores its address in f->shared->cache.
	 *
	 * We don't want to use this cache, as it has a bunch of extra
	 * initialization that may change over time, and in any case
	 * it will not in general be configured the way we want it.
	 *
	 * We used to deal with this problem by storing the file pointer
	 * in another instance of H5C_t, and then ignoring the original
	 * version.  However, this strategy doesn't work any more, as
	 * we can't store the file pointer in the instance of H5C_t,
	 * and we have modified many cache routines to use a file
	 * pointer to look up the target cache.
	 *
	 * Thus we now make note of the address of the instance of
	 * H5C_t created by the call to H5Fcreate(), set
	 * file_ptr->shared->cache to NULL, call H5C_create()
	 * to allocate a new instance of H5C_t for test purposes,
	 * and store than new instance's address in
	 * file_ptr->shared->cache.
	 *
	 * On shut down, we call H5C_dest on our instance of H5C_t,
	 * set file_ptr->shared->cache to point to the original
	 * instance, and then close the file normally.
	 */

        HDassert( saved_cache == NULL );

	saved_cache = file_ptr->shared->cache;

	file_ptr->shared->cache = NULL;

        cache_ptr = H5C_create(max_cache_size,
                               min_clean_size,
                               (NUMBER_OF_ENTRY_TYPES - 1),
			       (const char **)entry_type_names,
                               check_write_permitted,
                               TRUE,
                               NULL,
                               NULL);

        file_ptr->shared->cache = cache_ptr;
    }

    if ( show_progress ) /* 6 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    if ( pass ) {

	if ( cache_ptr == NULL ) {

            pass = FALSE;
            failure_mssg = "H5C_create() failed.";

            if ( verbose ) {
                 HDfprintf(stdout, "%s: H5C_create() failed.\n", fcn_name);
            }

        } else if ( cache_ptr->magic != H5C__H5C_T_MAGIC ) {

            pass = FALSE;
	    failure_mssg = "Bad cache_ptr magic.";

            if ( verbose ) {
                HDfprintf(stdout, "%s: Bad cache_ptr magic.\n", fcn_name);
            }
	}
    }

    if ( show_progress ) /* 7 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    if ( pass ) { /* allocate space for test entries */

        actual_base_addr = H5MF_alloc(file_ptr, H5FD_MEM_DEFAULT, H5P_DEFAULT,
			              (hsize_t)(ADDR_SPACE_SIZE + BASE_ADDR));

	if ( actual_base_addr == HADDR_UNDEF ) {

            pass = FALSE;
	    failure_mssg = "H5MF_alloc() failed.";

	    if ( verbose ) {
                HDfprintf(stdout, "%s: H5MF_alloc() failed.\n", fcn_name);
            }

	} else if ( actual_base_addr > BASE_ADDR ) {

	    /* If this happens, must increase BASE_ADDR so that the
	     * actual_base_addr is <= BASE_ADDR.  This should only happen
	     * if the size of the superblock is increase.
	     */
            pass = FALSE;
	    failure_mssg = "actual_base_addr > BASE_ADDR";

	    if ( verbose ) {
                HDfprintf(stdout, "%s: actual_base_addr > BASE_ADDR.\n",
			  fcn_name);
            }
        }

        saved_actual_base_addr = actual_base_addr;
    }

    if ( show_progress ) /* 8 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    if ( pass ) {

        H5C_stats__reset(cache_ptr);
        ret_val = file_ptr;
    }

    if ( show_progress ) /* 9 */
        HDfprintf(stdout, "%s() - %0d -- pass = %d\n",
                  fcn_name, mile_stone++, (int)pass);

    return(ret_val);

} /* setup_cache() */


/*-------------------------------------------------------------------------
 * Function:	takedown_cache()
 *
 * Purpose:	Flush the specified cache and destroy it.  If requested,
 *		dump stats first.  Then close and delete the associate
 *		file.
 *
 *		If pass is FALSE, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              9/14/07
 *
 *-------------------------------------------------------------------------
 */

void
takedown_cache(H5F_t * file_ptr,
               hbool_t dump_stats,
               hbool_t dump_detailed_stats)
{
    char filename[512];

    if ( file_ptr != NULL ) {
        H5C_t * cache_ptr = file_ptr->shared->cache;

        if ( dump_stats ) {

            H5C_stats(cache_ptr, "test cache", dump_detailed_stats);
        }

        flush_cache(file_ptr, TRUE, FALSE, FALSE);

        H5C_dest(file_ptr, H5P_DATASET_XFER_DEFAULT);

	if ( saved_cache != NULL ) {

	    file_ptr->shared->cache = saved_cache;
	    saved_cache = NULL;
	}

        if ( H5F_addr_defined(saved_actual_base_addr) ) {

            H5MF_xfree(file_ptr, H5FD_MEM_DEFAULT, H5P_DEFAULT, saved_actual_base_addr,
                                          (hsize_t)(ADDR_SPACE_SIZE + BASE_ADDR));
            saved_actual_base_addr = HADDR_UNDEF;
        }
    }

    if ( saved_fapl_id != H5P_DEFAULT ) {

        H5Pclose(saved_fapl_id);
	saved_fapl_id = H5P_DEFAULT;
    }

    if ( saved_fid != -1 ) {

	if ( H5Fclose(saved_fid) < 0  ) {

            pass = FALSE;
	    failure_mssg = "couldn't close test file.";

	} else {

	    saved_fid = -1;

        }

	if ( ( ! try_core_file_driver ) || ( core_file_driver_failed ) ) {

            if ( h5_fixname(FILENAME[0], H5P_DEFAULT, filename, sizeof(filename))
                 == NULL ) {

                pass = FALSE;
                failure_mssg = "h5_fixname() failed.\n";
            }

            if ( HDremove(filename) < 0 ) {

                pass = FALSE;
	        failure_mssg = "couldn't delete test file.";

	    }
	}
    }

    return;

} /* takedown_cache() */


/*-------------------------------------------------------------------------
 * Function:	expunge_entry()
 *
 * Purpose:	Expunge the entry indicated by the type and index.
 *
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              7/6/06
 *
 *-------------------------------------------------------------------------
 */

void
expunge_entry(H5F_t * file_ptr,
              int32_t type,
              int32_t idx)
{
    /* const char * fcn_name = "expunge_entry()"; */
    herr_t result;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    if ( pass ) {
#ifndef NDEBUG
        H5C_t * cache_ptr = file_ptr->shared->cache;

        HDassert( cache_ptr );
#endif /* NDEBUG */

        HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
        HDassert( ( 0 <= idx ) && ( idx <= max_indices[type] ) );

        base_addr = entries[type];
        entry_ptr = &(base_addr[idx]);

        HDassert( entry_ptr->index == idx );
        HDassert( entry_ptr->type == type );
        HDassert( entry_ptr == entry_ptr->self );
	HDassert( entry_ptr->cache_ptr == cache_ptr );
        HDassert( ! ( entry_ptr->header.is_protected ) );
        HDassert( ! ( entry_ptr->is_protected ) );
        HDassert( ! ( entry_ptr->header.is_pinned ) );
	HDassert( ! ( entry_ptr->is_pinned ) );

        result = H5C_expunge_entry(file_ptr, H5P_DATASET_XFER_DEFAULT,
			            &(types[type]),
                                    entry_ptr->addr);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "error in H5C_expunge_entry().";

        }
    }

    return;

} /* expunge_entry() */


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
 *-------------------------------------------------------------------------
 */

void
flush_cache(H5F_t * file_ptr,
            hbool_t destroy_entries,
            hbool_t dump_stats,
            hbool_t dump_detailed_stats)
{
    const char * fcn_name = "flush_cache()";
    H5C_t * cache_ptr;
    hbool_t show_progress = FALSE;
    herr_t result = 0;
    int mile_post = 0;

    if ( show_progress ) {
	HDfprintf(stdout, "%s: mile_post = %d.\n",
		  fcn_name, mile_post++); /* 0 */
    }

    verify_unprotected();

    if ( show_progress ) {
	HDfprintf(stdout, "%s: mile_post = %d - %d.\n",
		  fcn_name, mile_post++, (int)pass); /* 1 */
    }

    if ( pass ) {

        HDassert(file_ptr);

        cache_ptr = file_ptr->shared->cache;

        if ( destroy_entries ) {

            result = H5C_flush_cache(file_ptr, H5P_DATASET_XFER_DEFAULT,
                                    H5C__FLUSH_INVALIDATE_FLAG);

        } else {

            result = H5C_flush_cache(file_ptr, H5P_DATASET_XFER_DEFAULT,
                                    H5C__NO_FLAGS_SET);
        }
    }

    if ( show_progress ) {
	HDfprintf(stdout, "%s: mile_post = %d - %d.\n",
		  fcn_name, mile_post++, (int)pass); /* 2 */
    }

    if ( dump_stats ) {

        H5C_stats(cache_ptr, "test cache", dump_detailed_stats);
    }

    if ( show_progress ) {
	HDfprintf(stdout, "%s: *mile_post = %d - %d.\n",
		  fcn_name, mile_post++, (int)pass); /* 3 */
    }

    if ( result < 0 ) {

        pass = FALSE;
        failure_mssg = "error in H5C_flush_cache().";
    }

    if ( show_progress ) {
	HDfprintf(stdout, "%s: mile_post = %d - %d.\n",
		  fcn_name, mile_post++, (int)pass); /* 4 */
    }

    return;

} /* flush_cache() */


/*-------------------------------------------------------------------------
 * Function:	insert_entry()
 *
 * Purpose:	Insert the entry indicated by the type and index.
 *
 *		Do nothing if pass is false.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/16/04
 *
 *-------------------------------------------------------------------------
 */

void
insert_entry(H5F_t * file_ptr,
             int32_t type,
             int32_t idx,
             unsigned int flags)
{
    H5C_t * cache_ptr;
    herr_t result;
    hbool_t insert_pinned;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    if ( pass ) {

        cache_ptr = file_ptr->shared->cache;

        HDassert( cache_ptr );
        HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
        HDassert( ( 0 <= idx ) && ( idx <= max_indices[type] ) );

        base_addr = entries[type];
        entry_ptr = &(base_addr[idx]);

        HDassert( entry_ptr->index == idx );
        HDassert( entry_ptr->type == type );
        HDassert( entry_ptr == entry_ptr->self );
        HDassert( !(entry_ptr->is_protected) );

	insert_pinned = ((flags & H5C__PIN_ENTRY_FLAG) != 0 );

	entry_ptr->is_dirty = TRUE;

        result = H5C_insert_entry(file_ptr, H5P_DATASET_XFER_DEFAULT,
			           &(types[type]), entry_ptr->addr,
				   entry_ptr->size, (void *)entry_ptr, flags);

        if ( ( result < 0 ) ||
             ( entry_ptr->header.is_protected ) ||
             ( entry_ptr->header.type != &(types[type]) ) ||
             ( entry_ptr->size != entry_ptr->header.size ) ||
             ( entry_ptr->addr != entry_ptr->header.addr ) ) {

            pass = FALSE;
            failure_mssg = "error in H5C_insert().";

#if 0 /* This is useful debugging code.  Lets keep it around. */

            HDfprintf(stdout, "result = %d\n", (int)result);
            HDfprintf(stdout, "entry_ptr->header.is_protected = %d\n",
                      (int)(entry_ptr->header.is_protected));
            HDfprintf(stdout,
		      "entry_ptr->header.type != &(types[type]) = %d\n",
                      (int)(entry_ptr->header.type != &(types[type])));
            HDfprintf(stdout,
                      "entry_ptr->size != entry_ptr->header.size = %d\n",
                      (int)(entry_ptr->size != entry_ptr->header.size));
            HDfprintf(stdout,
                      "entry_ptr->addr != entry_ptr->header.addr = %d\n",
                       (int)(entry_ptr->addr != entry_ptr->header.addr));
#endif
        }
	HDassert( entry_ptr->cache_ptr == NULL );

        entry_ptr->file_ptr = file_ptr;
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

} /* insert_entry() */


/*-------------------------------------------------------------------------
 * Function:	mark_entry_dirty()
 *
 * Purpose:	Mark the specified entry as dirty.
 *
 *		Do nothing if pass is FALSE on entry.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              3/28/06
 *
 *-------------------------------------------------------------------------
 */

void
mark_entry_dirty(H5F_t * file_ptr,
                        int32_t type,
                        int32_t idx)
{
#ifndef NDEBUG
    H5C_t * cache_ptr;
#endif /* NDEBUG */
    herr_t result;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    if ( pass ) {

#ifndef NDEBUG
        cache_ptr = file_ptr->shared->cache;
#endif /* NDEBUG */

        HDassert( cache_ptr );
        HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
        HDassert( ( 0 <= idx ) && ( idx <= max_indices[type] ) );

        base_addr = entries[type];
        entry_ptr = &(base_addr[idx]);

        HDassert( entry_ptr->index == idx );
        HDassert( entry_ptr->type == type );
        HDassert( entry_ptr == entry_ptr->self );
	HDassert( entry_ptr->cache_ptr == cache_ptr );
        HDassert( entry_ptr->header.is_protected ||
		  entry_ptr->header.is_pinned );

	entry_ptr->is_dirty = TRUE;

        result = H5C_mark_entry_dirty((void *)entry_ptr);

        if ( ( result < 0 ) ||
             ( !entry_ptr->header.is_protected && !entry_ptr->header.is_pinned ) ||
             ( entry_ptr->header.is_protected && !entry_ptr->header.dirtied ) ||
             ( !entry_ptr->header.is_protected && !entry_ptr->header.is_dirty ) ||
             ( entry_ptr->header.type != &(types[type]) ) ||
             ( entry_ptr->size != entry_ptr->header.size ) ||
             ( entry_ptr->addr != entry_ptr->header.addr ) ) {

            pass = FALSE;
            failure_mssg = "error in H5C_mark_entry_dirty().";

        }

        HDassert( ((entry_ptr->header).type)->id == type );

    }

    return;

} /* mark_entry_dirty() */


/*-------------------------------------------------------------------------
 * Function:	move_entry()
 *
 * Purpose:	Move the entry indicated by the type and index to its
 *		main or alternate address as indicated.  If the entry is
 *		already at the desired entry, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/21/04
 *
 *-------------------------------------------------------------------------
 */

void
move_entry(H5C_t * cache_ptr,
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

    if ( pass ) {

        HDassert( cache_ptr );
        HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
        HDassert( ( 0 <= idx ) && ( idx <= max_indices[type] ) );

        base_addr = entries[type];
        entry_ptr = &(base_addr[idx]);

        HDassert( entry_ptr->index == idx );
        HDassert( entry_ptr->type == type );
        HDassert( entry_ptr == entry_ptr->self );
        HDassert( entry_ptr->cache_ptr == cache_ptr );
        HDassert( !(entry_ptr->is_protected) );
        HDassert( !(entry_ptr->header.is_protected) );


        if ( entry_ptr->at_main_addr && !main_addr ) {

            /* move to alt addr */

            HDassert( entry_ptr->addr == entry_ptr->main_addr );

            done = FALSE;
            old_addr = entry_ptr->addr;
            new_addr = entry_ptr->alt_addr;

        } else if ( !(entry_ptr->at_main_addr) && main_addr ) {

            /* move to main addr */

            HDassert( entry_ptr->addr == entry_ptr->alt_addr );

            done = FALSE;
            old_addr = entry_ptr->addr;
            new_addr = entry_ptr->main_addr;
        }

        if ( ! done ) {

            entry_ptr->is_dirty = TRUE;

            result = H5C_move_entry(cache_ptr, &(types[type]),
                                       old_addr, new_addr);
        }

        if ( ! done ) {

            if ( ( result < 0 ) ||
	         ( ( ! ( entry_ptr->header.destroy_in_progress ) ) &&
	           ( entry_ptr->header.addr != new_addr ) ) ) {

                pass = FALSE;
                failure_mssg = "error in H5C_move_entry().";

            } else {

                entry_ptr->addr = new_addr;
                entry_ptr->at_main_addr = main_addr;
            }
        }

        HDassert( ((entry_ptr->header).type)->id == type );

        HDassert( entry_ptr->header.is_dirty );
        HDassert( entry_ptr->is_dirty );
    }

    return;

} /* move_entry() */


/*-------------------------------------------------------------------------
 * Function:	pin_protected_entry()
 *
 * Purpose:	Pin the specified protected entry.
 *
 *		Do nothing if pass is FALSE on entry.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              5/17/06
 *
 *-------------------------------------------------------------------------
 */

void
pin_protected_entry(H5F_t * file_ptr,
                     int32_t type,
                     int32_t idx)
{
    /* const char * fcn_name = "pin_protected_entry()"; */
    herr_t result;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    if ( pass ) {
#ifndef NDEBUG
        H5C_t * cache_ptr = file_ptr->shared->cache;
#endif /* NDEBUG */

        HDassert( cache_ptr );
        HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
        HDassert( ( 0 <= idx ) && ( idx <= max_indices[type] ) );

        base_addr = entries[type];
        entry_ptr = &(base_addr[idx]);

        HDassert( entry_ptr->index == idx );
        HDassert( entry_ptr->type == type );
        HDassert( entry_ptr == entry_ptr->self );
	HDassert( entry_ptr->cache_ptr == cache_ptr );
        HDassert( entry_ptr->header.is_protected );
	HDassert( ! entry_ptr->header.is_pinned );

        result = H5C_pin_protected_entry((void *)entry_ptr);

        if ( ( result < 0 )
	     ||
	     ( ! (entry_ptr->header.is_protected) )
	     ||
	     ( ! (entry_ptr->header.is_pinned) )
	     ||
             ( entry_ptr->header.type != &(types[type]) )
	     ||
             ( entry_ptr->size != entry_ptr->header.size )
	     ||
             ( entry_ptr->addr != entry_ptr->header.addr ) ) {

            pass = FALSE;
            failure_mssg =
                "error in H5C_pin_protected_entry().";

        }

	entry_ptr->is_pinned = TRUE;

        HDassert( ((entry_ptr->header).type)->id == type );

    }

    return;

} /* pinned_protected_entry2() */


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
 *-------------------------------------------------------------------------
 */

void
protect_entry(H5F_t * file_ptr,
              int32_t type,
              int32_t idx)
{
    const char * fcn_name = "protect_entry()";
    H5C_t * cache_ptr;
    hbool_t verbose = FALSE;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;
    H5C_cache_entry_t * cache_entry_ptr;

    if ( verbose ) {
	HDfprintf(stdout, "\n%s: entering. type = %d, idx = %d.\n",
		  fcn_name, type, idx);
    }

    if ( pass ) {

        cache_ptr = file_ptr->shared->cache;

        HDassert( cache_ptr );
        HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
        HDassert( ( 0 <= idx ) && ( idx <= max_indices[type] ) );

        base_addr = entries[type];
        entry_ptr = &(base_addr[idx]);

        HDassert( entry_ptr->index == idx );
        HDassert( entry_ptr->type == type );
        HDassert( entry_ptr == entry_ptr->self );
        HDassert( !(entry_ptr->is_protected) );

	if ( verbose ) {
	    HDfprintf(stdout,
		      "%s: calling H5C_protect(). addr = 0x%lx, len = %ld.\n",
		      fcn_name, (long)(entry_ptr->addr),
		      (long)(entry_ptr->size));
	}

        cache_entry_ptr = (H5C_cache_entry_t *)H5C_protect(file_ptr, H5P_DATASET_XFER_DEFAULT,
			               &(types[type]), entry_ptr->addr,
				       &entry_ptr->addr, H5C__NO_FLAGS_SET);

	if ( verbose ) {
	    HDfprintf(stdout,
		      "%s: H5C_protect() returns. addr = 0x%lx, len = %ld.\n",
		      fcn_name, (long)(entry_ptr->addr),
		      (long)(entry_ptr->size));
	}

        if ( ( cache_entry_ptr != (void *)entry_ptr ) ||
             ( !(entry_ptr->header.is_protected) ) ||
             ( entry_ptr->header.type != &(types[type]) ) ||
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
                      "( entry_ptr->header.type != &(types[type]) ) = %d\n",
                      (int)( entry_ptr->header.type != &(types[type]) ));
            HDfprintf(stdout,
                      "entry_ptr->size = %d, entry_ptr->header.size = %d\n",
                      (int)(entry_ptr->size), (int)(entry_ptr->header.size));
            HDfprintf(stdout,
                      "entry_ptr->addr = %d, entry_ptr->header.addr = %d\n",
                      (int)(entry_ptr->addr), (int)(entry_ptr->header.addr));
#endif
            pass = FALSE;
            failure_mssg = "error in H5C_protect().";

        } else {

	    HDassert( ( entry_ptr->cache_ptr == NULL ) ||
		      ( entry_ptr->cache_ptr == cache_ptr ) );

	    entry_ptr->cache_ptr = cache_ptr;
	    entry_ptr->file_ptr = file_ptr;
            entry_ptr->is_protected = TRUE;

        }

        HDassert( ((entry_ptr->header).type)->id == type );
    }

    if ( verbose ) {
	HDfprintf(stdout, "%s: exiting.\n", fcn_name);
    }

    return;

} /* protect_entry() */


/*-------------------------------------------------------------------------
 * Function:	protect_entry_ro()
 *
 * Purpose:	Do a read only protect the entry indicated by the type
 * 		and index.
 *
 *		Do nothing if pass is FALSE on entry.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              4/1/07
 *
 *-------------------------------------------------------------------------
 */

void
protect_entry_ro(H5F_t * file_ptr,
                int32_t type,
                int32_t idx)
{
    H5C_t *cache_ptr;
    test_entry_t *base_addr;
    test_entry_t *entry_ptr;
    H5C_cache_entry_t * cache_entry_ptr;

    if ( pass ) {

        cache_ptr = file_ptr->shared->cache;

        HDassert( cache_ptr );
        HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
        HDassert( ( 0 <= idx ) && ( idx <= max_indices[type] ) );

        base_addr = entries[type];
        entry_ptr = &(base_addr[idx]);

        HDassert( entry_ptr->index == idx );
        HDassert( entry_ptr->type == type );
        HDassert( entry_ptr == entry_ptr->self );
        HDassert( ( ! ( entry_ptr->is_protected ) ) ||
		  ( ( entry_ptr->is_read_only ) &&
		    ( entry_ptr->ro_ref_count > 0 ) ) );

        cache_entry_ptr = (H5C_cache_entry_t *)H5C_protect(file_ptr, H5P_DATASET_XFER_DEFAULT,
			               &(types[type]), entry_ptr->addr,
				       &entry_ptr->addr, H5C__READ_ONLY_FLAG);

        if ( ( cache_entry_ptr != (void *)entry_ptr ) ||
             ( !(entry_ptr->header.is_protected) ) ||
             ( !(entry_ptr->header.is_read_only) ) ||
             ( entry_ptr->header.ro_ref_count <= 0 ) ||
             ( entry_ptr->header.type != &(types[type]) ) ||
             ( entry_ptr->size != entry_ptr->header.size ) ||
             ( entry_ptr->addr != entry_ptr->header.addr ) ) {

            pass = FALSE;
            failure_mssg = "error in read only H5C_protect().";

        } else {

	    HDassert( ( entry_ptr->cache_ptr == NULL ) ||
		      ( entry_ptr->cache_ptr == cache_ptr ) );

	    entry_ptr->cache_ptr = cache_ptr;
	    entry_ptr->file_ptr = file_ptr;
            entry_ptr->is_protected = TRUE;
	    entry_ptr->is_read_only = TRUE;
	    entry_ptr->ro_ref_count++;
        }

        HDassert( ((entry_ptr->header).type)->id == type );
    }

    return;

} /* protect_entry_ro() */


/*-------------------------------------------------------------------------
 * Function:	unpin_entry()
 *
 * Purpose:	Unpin the entry indicated by the type and index.
 *
 *		Do nothing if pass is FALSE on entry.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              3/28/06
 *
 *-------------------------------------------------------------------------
 */

void
unpin_entry(H5F_t * file_ptr,
            int32_t type,
            int32_t idx)
{
    herr_t result;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    if ( pass ) {
#ifndef NDEBUG
        H5C_t * cache_ptr;

        cache_ptr = file_ptr->shared->cache;

        HDassert( cache_ptr );
#endif /* NDEBUG */
        HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
        HDassert( ( 0 <= idx ) && ( idx <= max_indices[type] ) );

        base_addr = entries[type];
        entry_ptr = &(base_addr[idx]);

        HDassert( entry_ptr->index == idx );
        HDassert( entry_ptr->type == type );
        HDassert( entry_ptr == entry_ptr->self );
	HDassert( entry_ptr->cache_ptr == cache_ptr );
        HDassert( ! (entry_ptr->header.is_protected) );
        HDassert( entry_ptr->header.is_pinned );
	HDassert( entry_ptr->is_pinned );

        result = H5C_unpin_entry(entry_ptr);

        if ( ( result < 0 ) ||
             ( entry_ptr->header.is_pinned ) ||
             ( entry_ptr->header.type != &(types[type]) ) ||
             ( entry_ptr->size != entry_ptr->header.size ) ||
             ( entry_ptr->addr != entry_ptr->header.addr ) ) {

            pass = FALSE;
            failure_mssg = "error in H5C_unpin().";

        }

	entry_ptr->is_pinned = FALSE;

        HDassert( ((entry_ptr->header).type)->id == type );

    }

    return;

} /* unpin_entry() */


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
 *-------------------------------------------------------------------------
 */

void
unprotect_entry(H5F_t * file_ptr,
                int32_t type,
                int32_t idx,
                unsigned int flags)
{
    const char * fcn_name = "unprotect_entry()";
    herr_t result;
    hbool_t verbose = FALSE;
    hbool_t pin_flag_set;
    hbool_t unpin_flag_set;
    test_entry_t * base_addr;
    test_entry_t * entry_ptr;

    if ( verbose ) {
	HDfprintf(stdout,
            "\n%s: entering. type = %d, idx = %d, flags = %0x.\n",
	    fcn_name, type, idx, (int)flags);
    }

    if ( pass ) {
#ifndef NDEBUG
        H5C_t * cache_ptr;

        cache_ptr = file_ptr->shared->cache;

        HDassert( cache_ptr );
#endif /* NDEBUG */
        HDassert( ( 0 <= type ) && ( type < NUMBER_OF_ENTRY_TYPES ) );
        HDassert( ( 0 <= idx ) && ( idx <= max_indices[type] ) );

        base_addr = entries[type];
        entry_ptr = &(base_addr[idx]);

        HDassert( entry_ptr->index == idx );
        HDassert( entry_ptr->type == type );
        HDassert( entry_ptr == entry_ptr->self );
	HDassert( entry_ptr->cache_ptr == cache_ptr );
        HDassert( entry_ptr->header.is_protected );
        HDassert( entry_ptr->is_protected );

	pin_flag_set = ((flags & H5C__PIN_ENTRY_FLAG) != 0 );
	unpin_flag_set = ((flags & H5C__UNPIN_ENTRY_FLAG) != 0 );

	HDassert ( ! ( pin_flag_set && unpin_flag_set ) );
	HDassert ( ( ! pin_flag_set ) || ( ! (entry_ptr->is_pinned) ) );
	HDassert ( ( ! unpin_flag_set ) || ( entry_ptr->is_pinned ) );

        if ( flags & H5C__DIRTIED_FLAG) {
            entry_ptr->is_dirty = TRUE;
        }

	if ( verbose ) {
	    HDfprintf(stdout, "%s: calling H5C_unprotect(). addr = 0X%lx.\n",
	              fcn_name, (long)(entry_ptr->addr));
        }

        result = H5C_unprotect(file_ptr, H5P_DATASET_XFER_DEFAULT,
			        &(types[type]), entry_ptr->addr,
				(void *)entry_ptr, flags);

	if ( verbose ) {
	    HDfprintf(stdout, "%s: H5C_unprotect() returns. addr = 0X%lx.\n",
	              fcn_name, (long)(entry_ptr->addr));
        }


        if ( ( result < 0 ) ||
             ( ( entry_ptr->header.is_protected ) &&
	       ( ( ! ( entry_ptr->is_read_only ) ) ||
		 ( entry_ptr->ro_ref_count <= 0 ) ) ) ||
             ( entry_ptr->header.type != &(types[type]) ) ||
             ( entry_ptr->size != entry_ptr->header.size ) ||
             ( entry_ptr->addr != entry_ptr->header.addr ) ) {

            pass = FALSE;
            failure_mssg = "error in H5C_unprotect().";

	    if ( verbose ) {
                HDfprintf(stdout, "%s: error in H5C_unprotect():\n", fcn_name);

		if ( result < 0 )
		    HDfprintf(stdout, "	result = %d.\n", (int)result);

		if ( ( entry_ptr->header.is_protected ) &&
                     ( ( ! ( entry_ptr->is_read_only ) ) ||
                       ( entry_ptr->ro_ref_count <= 0 ) ) )
		    HDfprintf(stdout, "	R/O sanity check failed.\n");

		if ( entry_ptr->header.type != &(types[type]) )
		    HDfprintf(stdout, "	Type mismatch.\n");

		if ( entry_ptr->size != entry_ptr->header.size )
		    HDfprintf(stdout, "	size mismatch.\n");

		if ( entry_ptr->addr != entry_ptr->header.addr )
		    HDfprintf(stdout, "	addr mismatch.\n");
	    }
        }
        else
        {
	    if ( entry_ptr->ro_ref_count > 1 ) {

		entry_ptr->ro_ref_count--;

		if ( verbose )
                    HDfprintf(stdout, "%s: decremented ro ref count to %d.\n",
			      fcn_name, entry_ptr->ro_ref_count);

	    } else if ( entry_ptr->ro_ref_count == 1 ) {

		entry_ptr->is_protected = FALSE;
		entry_ptr->is_read_only = FALSE;
		entry_ptr->ro_ref_count = 0;

		if ( verbose )
                    HDfprintf(stdout, "%s: r/o unprotected.\n", fcn_name);

	    } else {

		entry_ptr->is_protected = FALSE;

		if ( verbose )
                    HDfprintf(stdout, "%s: r/w unprotected.\n", fcn_name);

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

        if ( ( flags & H5C__DIRTIED_FLAG ) != 0
                && ( (flags & H5C__DELETED_FLAG) == 0 ) ) {

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

} /* unprotect_entry() */


/*-------------------------------------------------------------------------
 * Function:	row_major_scan_forward()
 *
 * Purpose:	Do a sequence of inserts, protects, unprotects, moves,
 *		destroys while scanning through the set of entries.  If
 *		pass is false on entry, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/12/04
 *
 *-------------------------------------------------------------------------
 */

void
row_major_scan_forward(H5F_t * file_ptr,
                        int32_t max_index,
                       int32_t lag,
                       hbool_t verbose,
                       hbool_t reset_stats,
                       hbool_t display_stats,
                       hbool_t display_detailed_stats,
                       hbool_t do_inserts,
                       hbool_t do_moves,
                       hbool_t move_to_main_addr,
                       hbool_t do_destroys,
		       hbool_t do_mult_ro_protects,
                       int dirty_destroys,
                       int dirty_unprotects)
{
    const char * fcn_name = "row_major_scan_forward";
    H5C_t * cache_ptr;
    int32_t type = 0;
    int32_t idx;
    int32_t local_max_index;

    if ( verbose )
        HDfprintf(stdout, "%s(): entering.\n", fcn_name);

    if ( pass ) {

        cache_ptr = file_ptr->shared->cache;

        HDassert( cache_ptr != NULL );

        HDassert( lag >= 10 );

        if ( reset_stats ) {

            H5C_stats__reset(cache_ptr);
        }
    }

    while ( ( pass ) && ( type < NUMBER_OF_ENTRY_TYPES ) )
    {
        idx = -lag;

        local_max_index = MIN(max_index, max_indices[type]);

        while ( ( pass ) && ( idx <= (local_max_index + lag) ) )
        {
	    if ( verbose ) {

                HDfprintf(stdout, "%d:%d: ", type, idx);
	    }

            if ( ( pass ) && ( do_inserts ) && ( (idx + lag) >= 0 ) &&
                 ( (idx + lag) <= local_max_index ) &&
                 ( ((idx + lag) % 2) == 0 ) &&
                 ( ! entry_in_cache(cache_ptr, type, (idx + lag)) ) ) {

                if ( verbose )
                    HDfprintf(stdout, "1(i, %d, %d) ", type, (idx + lag));

                insert_entry(file_ptr, type, (idx + lag), H5C__NO_FLAGS_SET);
            }


            if ( ( pass ) && ( (idx + lag - 1) >= 0 ) &&
                 ( (idx + lag - 1) <= local_max_index ) &&
                 ( ( (idx + lag - 1) % 3 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "2(p, %d, %d) ", type, (idx + lag - 1));

                protect_entry(file_ptr, type, (idx + lag - 1));
            }

            if ( ( pass ) && ( (idx + lag - 2) >= 0 ) &&
                 ( (idx + lag - 2) <= local_max_index ) &&
                 ( ( (idx + lag - 2) % 3 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "3(u, %d, %d) ", type, (idx + lag - 2));

                unprotect_entry(file_ptr, type, idx+lag-2, H5C__NO_FLAGS_SET);
            }


            if ( ( pass ) && ( do_moves ) && ( (idx + lag - 2) >= 0 ) &&
                 ( (idx + lag - 2) <= local_max_index ) &&
                 ( ( (idx + lag - 2) % 3 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "4(r, %d, %d, %d) ",
			      type, (idx + lag - 2), (int)move_to_main_addr);

                move_entry(cache_ptr, type, (idx + lag - 2),
                             move_to_main_addr);
            }


            if ( ( pass ) && ( (idx + lag - 3) >= 0 ) &&
                 ( (idx + lag - 3) <= local_max_index ) &&
                 ( ( (idx + lag - 3) % 5 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "5(p, %d, %d) ", type, (idx + lag - 3));

                protect_entry(file_ptr, type, (idx + lag - 3));
            }

            if ( ( pass ) && ( (idx + lag - 5) >= 0 ) &&
                 ( (idx + lag - 5) <= local_max_index ) &&
                 ( ( (idx + lag - 5) % 5 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "6(u, %d, %d) ", type, (idx + lag - 5));

                unprotect_entry(file_ptr, type, idx+lag-5, H5C__NO_FLAGS_SET);
            }

	    if ( do_mult_ro_protects )
	    {
		if ( ( pass ) && ( (idx + lag - 5) >= 0 ) &&
		     ( (idx + lag - 5) < local_max_index ) &&
		     ( (idx + lag - 5) % 9 == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "7(p-ro, %d, %d) ", type,
				  (idx + lag - 5));

		    protect_entry_ro(file_ptr, type, (idx + lag - 5));
		}

		if ( ( pass ) && ( (idx + lag - 6) >= 0 ) &&
		     ( (idx + lag - 6) < local_max_index ) &&
		     ( (idx + lag - 6) % 11 == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "8(p-ro, %d, %d) ", type,
				  (idx + lag - 6));

		    protect_entry_ro(file_ptr, type, (idx + lag - 6));
		}

		if ( ( pass ) && ( (idx + lag - 7) >= 0 ) &&
		     ( (idx + lag - 7) < local_max_index ) &&
		     ( (idx + lag - 7) % 13 == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "9(p-ro, %d, %d) ", type,
				  (idx + lag - 7));

		    protect_entry_ro(file_ptr, type, (idx + lag - 7));
		}

		if ( ( pass ) && ( (idx + lag - 7) >= 0 ) &&
		     ( (idx + lag - 7) < local_max_index ) &&
		     ( (idx + lag - 7) % 9 == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "10(u-ro, %d, %d) ", type,
				  (idx + lag - 7));

		    unprotect_entry(file_ptr, type, (idx + lag - 7), H5C__NO_FLAGS_SET);
		}

		if ( ( pass ) && ( (idx + lag - 8) >= 0 ) &&
		     ( (idx + lag - 8) < local_max_index ) &&
		     ( (idx + lag - 8) % 11 == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "11(u-ro, %d, %d) ", type,
				  (idx + lag - 8));

		    unprotect_entry(file_ptr, type, (idx + lag - 8), H5C__NO_FLAGS_SET);
		}

		if ( ( pass ) && ( (idx + lag - 9) >= 0 ) &&
		     ( (idx + lag - 9) < local_max_index ) &&
		     ( (idx + lag - 9) % 13 == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "12(u-ro, %d, %d) ", type,
				  (idx + lag - 9));

		    unprotect_entry(file_ptr, type, (idx + lag - 9), H5C__NO_FLAGS_SET);
		}
	    } /* if ( do_mult_ro_protects ) */

            if ( ( pass ) && ( idx >= 0 ) && ( idx <= local_max_index ) ) {

                if ( verbose )
                    HDfprintf(stdout, "13(p, %d, %d) ", type, idx);

                protect_entry(file_ptr, type, idx);
            }

            if ( ( pass ) && ( (idx - lag + 2) >= 0 ) &&
                 ( (idx - lag + 2) <= local_max_index ) &&
                 ( ( (idx - lag + 2) % 7 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "14(u, %d, %d) ", type, (idx - lag + 2));

                unprotect_entry(file_ptr, type, idx-lag+2, H5C__NO_FLAGS_SET);
            }

            if ( ( pass ) && ( (idx - lag + 1) >= 0 ) &&
                 ( (idx - lag + 1) <= local_max_index ) &&
                 ( ( (idx - lag + 1) % 7 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "15(p, %d, %d) ", type, (idx - lag + 1));

                protect_entry(file_ptr, type, (idx - lag + 1));
            }


            if ( do_destroys ) {

                if ( ( pass ) && ( (idx - lag) >= 0 ) &&
                     ( ( idx - lag) <= local_max_index ) ) {

                    switch ( (idx - lag) %4 ) {

                        case 0: /* we just did an insert */

                            if ( verbose )
                                HDfprintf(stdout,
					 "16(u, %d, %d) ", type, (idx - lag));

                            unprotect_entry(file_ptr, type, idx - lag, H5C__NO_FLAGS_SET);
                            break;

                        case 1:
                            if ( (entries[type])[idx-lag].is_dirty ) {

                            if ( verbose )
                                HDfprintf(stdout,
					 "17(u, %d, %d) ", type, (idx - lag));

                                unprotect_entry(file_ptr, type, idx - lag, H5C__NO_FLAGS_SET);
                            } else {

                            if ( verbose )
                                HDfprintf(stdout,
					 "18(u, %d, %d) ", type, (idx - lag));

                                unprotect_entry(file_ptr, type, idx - lag,
                                        (dirty_unprotects ? H5C__DIRTIED_FLAG : H5C__NO_FLAGS_SET));
                            }
                            break;

                        case 2: /* we just did an insrt */

                            if ( verbose )
                                HDfprintf(stdout,
					 "19(u-del, %d, %d) ", type, (idx - lag));

                            unprotect_entry(file_ptr, type, idx - lag, H5C__DELETED_FLAG);
                            break;

                        case 3:
                            if ( (entries[type])[idx-lag].is_dirty ) {

                                if ( verbose )
                                    HDfprintf(stdout,
					      "20(u-del, %d, %d) ",
					      type, (idx - lag));

                                unprotect_entry(file_ptr, type, idx - lag, H5C__DELETED_FLAG);
                            } else {

                                if ( verbose )
                                    HDfprintf(stdout,
					      "21(u-del, %d, %d) ",
					      type, (idx - lag));

                                unprotect_entry(file_ptr, type, idx - lag,
                                        (dirty_destroys ? H5C__DIRTIED_FLAG : H5C__NO_FLAGS_SET)
                                        | H5C__DELETED_FLAG);
                            }
                            break;

                        default:
                            HDassert(0); /* this can't happen... */
                            break;
                    }
                }

            } else {

                if ( ( pass ) && ( (idx - lag) >= 0 ) &&
                     ( ( idx - lag) <= local_max_index ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "22(u, %d, %d) ", type, (idx - lag));

                    unprotect_entry(file_ptr, type, idx - lag,
                            (dirty_unprotects ? H5C__DIRTIED_FLAG : H5C__NO_FLAGS_SET));
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
 * Function:	hl_row_major_scan_forward()
 *
 * Purpose:	Do a high locality sequence of inserts, protects, and
 *		unprotects while scanning through the set of entries.
 *		If pass is false on entry, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              10/21/04
 *
 *-------------------------------------------------------------------------
 */

void
hl_row_major_scan_forward(H5F_t * file_ptr,
                          int32_t max_index,
                          hbool_t verbose,
                          hbool_t reset_stats,
                          hbool_t display_stats,
                          hbool_t display_detailed_stats,
                          hbool_t do_inserts)
{
    const char * fcn_name = "hl_row_major_scan_forward";
    H5C_t * cache_ptr;
    int32_t type = 0;
    int32_t idx;
    int32_t i;
    int32_t lag = 100;
    int32_t local_max_index;

    if ( verbose )
        HDfprintf(stdout, "%s(): entering.\n", fcn_name);

    if ( pass ) {

        cache_ptr = file_ptr->shared->cache;

        HDassert( cache_ptr != NULL );
        HDassert( lag > 5 );
        HDassert( max_index >= 200 );
        HDassert( max_index <= MAX_ENTRIES );

        if ( reset_stats ) {

            H5C_stats__reset(cache_ptr);
        }
    }

    while ( ( pass ) && ( type < NUMBER_OF_ENTRY_TYPES ) )
    {
        idx = -lag;

        local_max_index = MIN(max_index, max_indices[type]);

        while ( ( pass ) && ( idx <= (local_max_index + lag) ) )
        {
            if ( ( pass ) && ( do_inserts ) && ( (idx + lag) >= 0 ) &&
                 ( (idx + lag) <= max_indices[type] ) &&
                 ( ((idx + lag) % 2) == 0 ) &&
                 ( ! entry_in_cache(cache_ptr, type, (idx + lag)) ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(i, %d, %d) ", type, (idx + lag));

                insert_entry(file_ptr, type, (idx + lag), H5C__NO_FLAGS_SET);
            }

            i = idx;

            while ( ( pass ) && ( i >= idx - lag ) && ( i >= 0 ) )
            {
                if ( ( pass ) && ( i >= 0 ) && ( i <= local_max_index ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(p, %d, %d) ", type, i);

                    protect_entry(file_ptr, type, i);

                    if ( verbose )
                        HDfprintf(stdout, "(u, %d, %d) ", type, i);

                    unprotect_entry(file_ptr, type, i, H5C__NO_FLAGS_SET);
                }
                i--;
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

} /* hl_row_major_scan_forward() */


/*-------------------------------------------------------------------------
 * Function:	row_major_scan_backward()
 *
 * Purpose:	Do a sequence of inserts, protects, unprotects, moves,
 *		destroys while scanning backwards through the set of
 *		entries.  If pass is false on entry, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              6/12/04
 *
 *-------------------------------------------------------------------------
 */

void
row_major_scan_backward(H5F_t * file_ptr,
                        int32_t max_index,
                        int32_t lag,
                        hbool_t verbose,
                        hbool_t reset_stats,
                        hbool_t display_stats,
                        hbool_t display_detailed_stats,
                        hbool_t do_inserts,
                        hbool_t do_moves,
                        hbool_t move_to_main_addr,
                        hbool_t do_destroys,
			hbool_t do_mult_ro_protects,
                        int dirty_destroys,
                        int dirty_unprotects)
{
    const char * fcn_name = "row_major_scan_backward";
    H5C_t * cache_ptr;
    int32_t type = NUMBER_OF_ENTRY_TYPES - 1;
    int32_t idx;
    int32_t local_max_index;

    if ( verbose )
        HDfprintf(stdout, "%s(): Entering.\n", fcn_name);

    if ( pass ) {

        cache_ptr = file_ptr->shared->cache;

        HDassert( cache_ptr != NULL );
        HDassert( lag >= 10 );

        if ( reset_stats ) {

            H5C_stats__reset(cache_ptr);
        }
    }

    while ( ( pass ) && ( type >= 0 ) )
    {
        local_max_index = MIN(max_index, max_indices[type]);

        /*idx = max_indices[type] + lag; */
        idx = local_max_index + lag;

        while ( ( pass ) && ( idx >= -lag ) )
        {
            if ( ( pass ) && ( do_inserts ) && ( (idx - lag) >= 0 ) &&
                 /*( (idx - lag) <= max_indices[type] ) && */
                 ( (idx - lag) <= local_max_index ) &&
                 ( ((idx - lag) % 2) == 1 ) &&
                 ( ! entry_in_cache(cache_ptr, type, (idx - lag)) ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(i, %d, %d) ", type, (idx - lag));

                insert_entry(file_ptr, type, (idx - lag), H5C__NO_FLAGS_SET);
            }


            if ( ( pass ) && ( (idx - lag + 1) >= 0 ) &&
                 /*( (idx - lag + 1) <= max_indices[type] ) && */
                 ( (idx - lag + 1) <= local_max_index ) &&
                 ( ( (idx - lag + 1) % 3 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, (idx - lag + 1));

                protect_entry(file_ptr, type, (idx - lag + 1));
            }

            if ( ( pass ) && ( (idx - lag + 2) >= 0 ) &&
                 /*( (idx - lag + 2) <= max_indices[type] ) && */
                 ( (idx - lag + 2) <= local_max_index ) &&
                 ( ( (idx - lag + 2) % 3 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(u, %d, %d) ", type, (idx - lag + 2));

                unprotect_entry(file_ptr, type, idx-lag+2, H5C__NO_FLAGS_SET);
            }


            if ( ( pass ) && ( do_moves ) && ( (idx - lag + 2) >= 0 ) &&
                 /*( (idx - lag + 2) <= max_indices[type] ) && */
                 ( (idx - lag + 2) <= local_max_index ) &&
                 ( ( (idx - lag + 2) % 3 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(r, %d, %d, %d) ",
			      type, (idx + lag + 2), (int)move_to_main_addr);

                move_entry(cache_ptr, type, (idx - lag + 2),
                             move_to_main_addr);
            }


            if ( ( pass ) && ( (idx - lag + 3) >= 0 ) &&
                 /*( (idx - lag + 3) <= max_indices[type] ) && */
                 ( (idx - lag + 3) <= local_max_index ) &&
                 ( ( (idx - lag + 3) % 5 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, (idx - lag + 3));

                protect_entry(file_ptr, type, (idx - lag + 3));
            }

            if ( ( pass ) && ( (idx - lag + 5) >= 0 ) &&
                 /*( (idx - lag + 5) <= max_indices[type] ) && */
                 ( (idx - lag + 5) <= local_max_index ) &&
                 ( ( (idx - lag + 5) % 5 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(u, %d, %d) ", type, (idx - lag + 5));

                unprotect_entry(file_ptr, type, idx-lag+5, H5C__NO_FLAGS_SET);
            }

	    if ( do_mult_ro_protects )
	    {
		if ( ( pass ) && ( (idx - lag + 5) >= 0 ) &&
		     /*( (idx - lag + 5) < max_indices[type] ) && */
		     ( (idx - lag + 5) < local_max_index ) &&
		     ( (idx - lag + 5) % 9 == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(p-ro, %d, %d) ", type,
				  (idx - lag + 5));

		    protect_entry_ro(file_ptr, type, (idx - lag + 5));
		}

		if ( ( pass ) && ( (idx - lag + 6) >= 0 ) &&
		     /*( (idx - lag + 6) < max_indices[type] ) && */
		     ( (idx - lag + 6) < local_max_index ) &&
		     ( (idx - lag + 6) % 11 == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(p-ro, %d, %d) ", type,
				  (idx - lag + 6));

		    protect_entry_ro(file_ptr, type, (idx - lag + 6));
		}

		if ( ( pass ) && ( (idx - lag + 7) >= 0 ) &&
		     /* ( (idx - lag + 7) < max_indices[type] ) && */
		     ( (idx - lag + 7) < local_max_index ) &&
		     ( (idx - lag + 7) % 13 == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(p-ro, %d, %d) ", type,
				  (idx - lag + 7));

		    protect_entry_ro(file_ptr, type, (idx - lag + 7));
		}

		if ( ( pass ) && ( (idx - lag + 7) >= 0 ) &&
		     /*( (idx - lag + 7) < max_indices[type] ) && */
		     ( (idx - lag + 7) < local_max_index ) &&
		     ( (idx - lag + 7) % 9 == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(u-ro, %d, %d) ", type,
				  (idx - lag + 7));

		    unprotect_entry(file_ptr, type, (idx - lag + 7), H5C__NO_FLAGS_SET);
		}

		if ( ( pass ) && ( (idx - lag + 8) >= 0 ) &&
		     /*( (idx - lag + 8) < max_indices[type] ) && */
		     ( (idx - lag + 8) < local_max_index ) &&
		     ( (idx - lag + 8) % 11 == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(u-ro, %d, %d) ", type,
				  (idx - lag + 8));

		    unprotect_entry(file_ptr, type, (idx - lag + 8), H5C__NO_FLAGS_SET);
		}

		if ( ( pass ) && ( (idx - lag + 9) >= 0 ) &&
		     /*( (idx - lag + 9) < max_indices[type] ) && */
		     ( (idx - lag + 9) < local_max_index ) &&
		     ( (idx - lag + 9) % 13 == 0 ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(u-ro, %d, %d) ", type,
				  (idx - lag + 9));

		    unprotect_entry(file_ptr, type, (idx - lag + 9), H5C__NO_FLAGS_SET);
		}
	    } /* if ( do_mult_ro_protects ) */

            /*if ( ( pass ) && ( idx >= 0 ) && ( idx <= max_indices[type] ) ) { */
            if ( ( pass ) && ( idx >= 0 ) && ( idx <= local_max_index ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, idx);

                protect_entry(file_ptr, type, idx);
            }


            if ( ( pass ) && ( (idx + lag - 2) >= 0 ) &&
                 /*( (idx + lag - 2) <= max_indices[type] ) && */
                 ( (idx + lag - 2) <= local_max_index ) &&
                 ( ( (idx + lag - 2) % 7 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(u, %d, %d) ", type, (idx + lag - 2));

                unprotect_entry(file_ptr, type, idx+lag-2, H5C__NO_FLAGS_SET);
            }

            if ( ( pass ) && ( (idx + lag - 1) >= 0 ) &&
                 /*( (idx + lag - 1) <= max_indices[type] ) && */
                 ( (idx + lag - 1) <= local_max_index ) &&
                 ( ( (idx + lag - 1) % 7 ) == 0 ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, (idx + lag - 1));

                protect_entry(file_ptr, type, (idx + lag - 1));
            }


            if ( do_destroys ) {

                if ( ( pass ) && ( (idx + lag) >= 0 ) &&
                     /*( ( idx + lag) <= max_indices[type] ) ) { */
                     ( ( idx + lag) <= local_max_index ) ) {

                    switch ( (idx + lag) %4 ) {

                        case 0:
                            if ( (entries[type])[idx+lag].is_dirty ) {

                                unprotect_entry(file_ptr, type, idx + lag, H5C__NO_FLAGS_SET);
                            } else {

                                unprotect_entry(file_ptr, type, idx + lag,
                                        (dirty_unprotects ? H5C__DIRTIED_FLAG : H5C__NO_FLAGS_SET));
                            }
                            break;

                        case 1: /* we just did an insert */
                            unprotect_entry(file_ptr, type, idx + lag, H5C__NO_FLAGS_SET);
                            break;

                        case 2:
                            if ( (entries[type])[idx + lag].is_dirty ) {

                                unprotect_entry(file_ptr, type, idx + lag, H5C__DELETED_FLAG);
                            } else {

                                unprotect_entry(file_ptr, type, idx + lag,
                                        (dirty_destroys ? H5C__DIRTIED_FLAG : H5C__NO_FLAGS_SET)
                                        | H5C__DELETED_FLAG);
                            }
                            break;

                        case 3: /* we just did an insrt */
                            unprotect_entry(file_ptr, type, idx + lag, H5C__DELETED_FLAG);
                            break;

                        default:
                            HDassert(0); /* this can't happen... */
                            break;
                    }
                }
            } else {

                if ( ( pass ) && ( (idx + lag) >= 0 ) &&
                     /*( ( idx + lag) <= max_indices[type] ) ) { */
                     ( ( idx + lag) <= local_max_index ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(u, %d, %d) ", type, (idx + lag));

                    unprotect_entry(file_ptr, type, idx + lag,
                            (dirty_unprotects ? H5C__DIRTIED_FLAG : H5C__NO_FLAGS_SET));
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
 * Function:	hl_row_major_scan_backward()
 *
 * Purpose:	Do a high locality sequence of inserts, protects, and
 *		unprotects while scanning through the set of entries.
 *		If pass is false on entry, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              10/21/04
 *
 *-------------------------------------------------------------------------
 */

void
hl_row_major_scan_backward(H5F_t * file_ptr,
                           int32_t max_index,
                           hbool_t verbose,
                           hbool_t reset_stats,
                           hbool_t display_stats,
                           hbool_t display_detailed_stats,
                           hbool_t do_inserts)
{
    const char * fcn_name = "hl_row_major_scan_backward";
    H5C_t * cache_ptr;
    int32_t type = NUMBER_OF_ENTRY_TYPES - 1;
    int32_t idx;
    int32_t i;
    int32_t lag = 100;
    int32_t local_max_index;

    if ( verbose )
        HDfprintf(stdout, "%s(): entering.\n", fcn_name);

    if ( pass ) {

        cache_ptr = file_ptr->shared->cache;

        HDassert( cache_ptr != NULL );
        HDassert( lag > 5 );
        HDassert( max_index >= 200 );
        HDassert( max_index <= MAX_ENTRIES );

        if ( reset_stats ) {

            H5C_stats__reset(cache_ptr);
        }
    }

    while ( ( pass ) && ( type >= 0 ) )
    {
        idx = max_indices[type] + lag;

        local_max_index = MIN(max_index, max_indices[type]);

        while ( ( pass ) && ( idx >= -lag ) )
        {
            if ( ( pass ) && ( do_inserts ) && ( (idx + lag) >= 0 ) &&
                 ( (idx + lag) <= local_max_index ) &&
                 ( ((idx + lag) % 2) == 0 ) &&
                 ( ! entry_in_cache(cache_ptr, type, (idx + lag)) ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(i, %d, %d) ", type, (idx + lag));

                insert_entry(file_ptr, type, (idx + lag), H5C__NO_FLAGS_SET);
            }

            i = idx;

            while ( ( pass ) && ( i >= idx - lag ) && ( i >= 0 ) )
            {
                if ( ( pass ) && ( i >= 0 ) && ( i <= local_max_index ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(p, %d, %d) ", type, i);

                    protect_entry(file_ptr, type, i);

                    if ( verbose )
                        HDfprintf(stdout, "(u, %d, %d) ", type, i);

                    unprotect_entry(file_ptr, type, i, H5C__NO_FLAGS_SET);
                }
                i--;
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

} /* hl_row_major_scan_backward() */


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
 *-------------------------------------------------------------------------
 */

void
col_major_scan_forward(H5F_t * file_ptr,
		       int32_t max_index,
                       int32_t lag,
                       hbool_t verbose,
                       hbool_t reset_stats,
                       hbool_t display_stats,
                       hbool_t display_detailed_stats,
                       hbool_t do_inserts,
                       int dirty_unprotects)
{
    const char * fcn_name = "col_major_scan_forward()";
    H5C_t * cache_ptr;
    int i;
    int32_t type = 0;
    int32_t idx;
    int32_t local_max_index[NUMBER_OF_ENTRY_TYPES];

    if ( verbose )
        HDfprintf(stdout, "%s: entering.\n", fcn_name);

    if ( pass ) {

        cache_ptr = file_ptr->shared->cache;

        for ( i = 0; i < NUMBER_OF_ENTRY_TYPES; i++ )
        {
            local_max_index[i] = MIN(max_index, max_indices[i]);
        }

        HDassert( lag > 5 );

        if ( reset_stats ) {

            H5C_stats__reset(cache_ptr);
        }
    }

    idx = -lag;

    while ( ( pass ) && ( (idx - lag) <= MAX_ENTRIES ) )
    {
        type = 0;

        while ( ( pass ) && ( type < NUMBER_OF_ENTRY_TYPES ) )
        {
            if ( ( pass ) && ( do_inserts ) && ( (idx + lag) >= 0 ) &&
                 /*( (idx + lag) <= max_indices[type] ) && */
                 ( (idx + lag) <= local_max_index[type] ) &&
                 ( ((idx + lag) % 3) == 0 ) &&
                 ( ! entry_in_cache(cache_ptr, type, (idx + lag)) ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(i, %d, %d) ", type, (idx + lag));

                insert_entry(file_ptr, type, (idx + lag), H5C__NO_FLAGS_SET);
            }

            /*if ( ( pass ) && ( idx >= 0 ) && ( idx <= max_indices[type] ) ) { */
            if ( ( pass ) &&
                 ( idx >= 0 ) &&
                 ( idx <= local_max_index[type] ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, idx);

                protect_entry(file_ptr, type, idx);
            }

            if ( ( pass ) && ( (idx - lag) >= 0 ) &&
                 /*( (idx - lag) <= max_indices[type] ) ) { */
                 ( (idx - lag) <= local_max_index[type] ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(u, %d, %d) ", type, (idx - lag));

                unprotect_entry(file_ptr, type, idx - lag,
                        (dirty_unprotects ? H5C__DIRTIED_FLAG : H5C__NO_FLAGS_SET));
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
 * Function:	hl_col_major_scan_forward()
 *
 * Purpose:	Do a high locality sequence of inserts, protects, and
 *		unprotects while scanning through the set of entries.  If
 *		pass is false on entry, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              19/25/04
 *
 *-------------------------------------------------------------------------
 */

void
hl_col_major_scan_forward(H5F_t * file_ptr,
                          int32_t max_index,
                          hbool_t verbose,
                          hbool_t reset_stats,
                          hbool_t display_stats,
                          hbool_t display_detailed_stats,
                          hbool_t do_inserts,
                          int dirty_unprotects)
{
    const char * fcn_name = "hl_col_major_scan_forward()";
    H5C_t * cache_ptr;
    int32_t type = 0;
    int32_t idx;
    int32_t lag = 200;
    int32_t i;
    int32_t local_max_index;

    if ( verbose )
        HDfprintf(stdout, "%s: entering.\n", fcn_name);

    if ( pass ) {

        cache_ptr = file_ptr->shared->cache;

        HDassert( cache_ptr != NULL );
        HDassert( lag > 5 );
        HDassert( max_index >= 500 );
        HDassert( max_index <= MAX_ENTRIES );

        if ( reset_stats ) {

            H5C_stats__reset(cache_ptr);
        }
    }

    idx = 0;

    local_max_index = MIN(max_index, MAX_ENTRIES);

    while ( ( pass ) && ( idx <= local_max_index ) )
    {

        i = idx;

        while ( ( pass ) && ( i >= 0 ) && ( i >= (idx - lag) ) ) {

            type = 0;

            while ( ( pass ) && ( type < NUMBER_OF_ENTRY_TYPES ) )
            {
                if ( ( pass ) && ( do_inserts ) && ( i == idx ) &&
                     ( i <= local_max_index ) &&
                     ( (i % 3) == 0 ) &&
                     ( ! entry_in_cache(cache_ptr, type, i) ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(i, %d, %d) ", type, i);

                    insert_entry(file_ptr, type, i, H5C__NO_FLAGS_SET);
                }

                if ( ( pass ) && ( i >= 0 ) && ( i <= local_max_index ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(p, %d, %d) ", type, i);

                    protect_entry(file_ptr, type, i);
                }

                if ( ( pass ) && ( i >= 0 ) &&
                     /*( i <= max_indices[type] ) ) { */
                     ( i <= local_max_index ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(u, %d, %d) ", type, i);

                    unprotect_entry(file_ptr, type, i,
                            (dirty_unprotects ? H5C__DIRTIED_FLAG : H5C__NO_FLAGS_SET));
                }

                if ( verbose )
                    HDfprintf(stdout, "\n");

                type++;
            }

            i--;
        }

        idx++;
    }

    if ( ( pass ) && ( display_stats ) ) {

        H5C_stats(cache_ptr, "test cache", display_detailed_stats);
    }

    return;

} /* hl_col_major_scan_forward() */


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
 *-------------------------------------------------------------------------
 */

void
col_major_scan_backward(H5F_t * file_ptr,
		        int32_t max_index,
                        int32_t lag,
                        hbool_t verbose,
                        hbool_t reset_stats,
                        hbool_t display_stats,
                        hbool_t display_detailed_stats,
                        hbool_t do_inserts,
                        int dirty_unprotects)
{
    const char * fcn_name = "col_major_scan_backward()";
    H5C_t * cache_ptr;
    int i;
    int mile_stone = 1;
    int32_t type;
    int32_t idx;
    int32_t local_max_index[NUMBER_OF_ENTRY_TYPES];

    if ( verbose )
        HDfprintf(stdout, "%s: entering.\n", fcn_name);

    if ( pass ) {

        cache_ptr = file_ptr->shared->cache;

        HDassert( cache_ptr != NULL );

        for ( i = 0; i < NUMBER_OF_ENTRY_TYPES; i++ )
        {
            local_max_index[i] = MIN(max_index, max_indices[i]);
        }

        HDassert( lag > 5 );

        if ( reset_stats ) {

            H5C_stats__reset(cache_ptr);
        }
    }

    /* idx = MAX_ENTRIES + lag; */
    idx = local_max_index[NUMBER_OF_ENTRY_TYPES - 1] + lag;

    if ( verbose ) /* 1 */
        HDfprintf(stdout, "%s: point %d.\n", fcn_name, mile_stone++);


    while ( ( pass ) && ( (idx + lag) >= 0 ) )
    {
        type = NUMBER_OF_ENTRY_TYPES - 1;

        while ( ( pass ) && ( type >= 0 ) )
        {
            if ( ( pass ) && ( do_inserts) && ( (idx - lag) >= 0 ) &&
                 /*( (idx - lag) <= max_indices[type] ) && */
                 ( (idx - lag) <= local_max_index[type] ) &&
                 ( ((idx - lag) % 3) == 0 ) &&
                 ( ! entry_in_cache(cache_ptr, type, (idx - lag)) ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(i, %d, %d) ", type, (idx - lag));

                insert_entry(file_ptr, type, (idx - lag), H5C__NO_FLAGS_SET);
            }

            /*if ( ( pass ) && ( idx >= 0 ) && ( idx <= max_indices[type] ) ) { */
            if ( ( pass ) &&
		 ( idx >= 0 ) &&
		 ( idx <= local_max_index[type] ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(p, %d, %d) ", type, idx);

                protect_entry(file_ptr, type, idx);
            }

            if ( ( pass ) && ( (idx + lag) >= 0 ) &&
                 /*( (idx + lag) <= max_indices[type] ) ) { */
                 ( (idx + lag) <= local_max_index[type] ) ) {

                if ( verbose )
                    HDfprintf(stdout, "(u, %d, %d) ", type, (idx + lag));

                unprotect_entry(file_ptr, type, idx + lag,
                        (dirty_unprotects ? H5C__DIRTIED_FLAG : H5C__NO_FLAGS_SET));
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


/*-------------------------------------------------------------------------
 * Function:	hl_col_major_scan_backward()
 *
 * Purpose:	Do a high locality sequence of inserts, protects, and
 *		unprotects while scanning backwards through the set of
 *		entries.  If pass is false on entry, do nothing.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              10/25/04
 *
 *-------------------------------------------------------------------------
 */

void
hl_col_major_scan_backward(H5F_t * file_ptr,
                           int32_t max_index,
                           hbool_t verbose,
                           hbool_t reset_stats,
                           hbool_t display_stats,
                           hbool_t display_detailed_stats,
                           hbool_t do_inserts,
                           int dirty_unprotects)
{
    const char * fcn_name = "hl_col_major_scan_backward()";
    H5C_t * cache_ptr;
    int32_t type = 0;
    int32_t idx;
    int32_t lag = 50;
    int32_t i;
    int32_t local_max_index;

    if ( verbose )
        HDfprintf(stdout, "%s: entering.\n", fcn_name);

    if ( pass ) {

        cache_ptr = file_ptr->shared->cache;

        HDassert( cache_ptr != NULL );
        HDassert( lag > 5 );
        HDassert( max_index >= 500 );
        HDassert( max_index <= MAX_ENTRIES );

        local_max_index = MIN(max_index, MAX_ENTRIES);

        if ( reset_stats ) {

            H5C_stats__reset(cache_ptr);
        }

        idx = local_max_index;
    }

    while ( ( pass ) && ( idx >= 0 ) )
    {

        i = idx;

        while ( ( pass ) && ( i <= local_max_index ) && ( i <= (idx + lag) ) ) {

            type = 0;

            while ( ( pass ) && ( type < NUMBER_OF_ENTRY_TYPES ) )
            {
                if ( ( pass ) && ( do_inserts ) && ( i == idx ) &&
                     ( i <= local_max_index ) &&
                     ( ! entry_in_cache(cache_ptr, type, i) ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(i, %d, %d) ", type, i);

                    insert_entry(file_ptr, type, i, H5C__NO_FLAGS_SET);
                }

                if ( ( pass ) && ( i >= 0 ) && ( i <= local_max_index ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(p, %d, %d) ", type, i);

                    protect_entry(file_ptr, type, i);
                }

                if ( ( pass ) && ( i >= 0 ) &&
                     ( i <= local_max_index ) ) {

                    if ( verbose )
                        HDfprintf(stdout, "(u, %d, %d) ", type, i);

                    unprotect_entry(file_ptr, type, i,
                            (dirty_unprotects ? H5C__DIRTIED_FLAG : H5C__NO_FLAGS_SET));
                }

                if ( verbose )
                    HDfprintf(stdout, "\n");

                type++;
            }

            i++;
        }

        idx--;
    }

    if ( ( pass ) && ( display_stats ) ) {

        H5C_stats(cache_ptr, "test cache", display_detailed_stats);
    }

    return;

} /* hl_col_major_scan_backward() */


/*** H5AC level utility functions ***/

/*-------------------------------------------------------------------------
 * Function:	check_and_validate_cache_hit_rate()
 *
 * Purpose:	Use the API functions to get and reset the cache hit rate.
 *		Verify that the value returned by the API call agrees with
 *		the cache internal data structures.
 *
 *		If the number of cache accesses exceeds the value provided
 *		in the min_accesses parameter, and the hit rate is less than
 *		min_hit_rate, set pass to FALSE, and set failure_mssg to
 *		a string indicating that hit rate was unexpectedly low.
 *
 *		Return hit rate in *hit_rate_ptr, and print the data to
 *		stdout if requested.
 *
 *		If an error is detected, set pass to FALSE, and set
 *		failure_mssg to an appropriate value.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              4/18/04
 *
 *-------------------------------------------------------------------------
 */

void
check_and_validate_cache_hit_rate(hid_t file_id,
                                  double * hit_rate_ptr,
                                  hbool_t dump_data,
                                  int64_t min_accesses,
                                  double min_hit_rate)
{
    /* const char * fcn_name = "check_and_validate_cache_hit_rate()"; */
    herr_t result;
    int64_t cache_hits = 0;
    int64_t cache_accesses = 0;
    double expected_hit_rate;
    double hit_rate;
    H5F_t * file_ptr = NULL;
    H5C_t * cache_ptr = NULL;

    /* get a pointer to the files internal data structure */
    if ( pass ) {

        file_ptr = (H5F_t *)H5I_object_verify(file_id, H5I_FILE);

        if ( file_ptr == NULL ) {

            pass = FALSE;
            failure_mssg = "Can't get file_ptr.";

        } else {

            cache_ptr = file_ptr->shared->cache;
        }
    }

    /* verify that we can access the cache data structure */
    if ( pass ) {

        if ( ( cache_ptr == NULL ) ||
             ( cache_ptr->magic != H5C__H5C_T_MAGIC ) ) {

            pass = FALSE;
            failure_mssg = "Can't access cache resize_ctl.";
        }
    }

    /* compare the cache's internal configuration with the expected value */
    if ( pass ) {

        cache_hits     = cache_ptr->cache_hits;
        cache_accesses = cache_ptr->cache_accesses;

        if ( cache_accesses > 0 ) {

            expected_hit_rate = ((double)cache_hits) / ((double)cache_accesses);

        } else {

            expected_hit_rate = 0.0;
        }

        result = H5Fget_mdc_hit_rate(file_id, &hit_rate);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Fget_mdc_hit_rate() failed.";

        } else if ( ! DBL_REL_EQUAL(hit_rate, expected_hit_rate, 0.00001) ) {

            pass = FALSE;
            failure_mssg = "unexpected hit rate (1).";

        }
    }

    if ( pass ) { /* reset the hit rate */

        result = H5Freset_mdc_hit_rate_stats(file_id);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Freset_mdc_hit_rate_stats() failed.";
        }
    }

    /* set *hit_rate_ptr if appropriate */
    if ( ( pass ) && ( hit_rate_ptr != NULL ) ) {

        *hit_rate_ptr = hit_rate;
    }

    /* dump data to stdout if requested */
    if ( ( pass ) && ( dump_data ) ) {

        HDfprintf(stdout,
                  "cache_hits: %ld, cache_accesses: %ld, hit_rate: %lf\n",
                  (long)cache_hits, (long)cache_accesses, hit_rate);
    }

    if ( ( pass ) &&
         ( cache_accesses > min_accesses ) &&
         ( hit_rate < min_hit_rate ) ) {

            pass = FALSE;
            failure_mssg = "Unexpectedly low hit rate.";
    }

    return;

} /* check_and_validate_cache_hit_rate() */


/*-------------------------------------------------------------------------
 * Function:	check_and_validate_cache_size()
 *
 * Purpose:	Use the API function to get the cache size data.  Verify
 *		that the values returned by the API call agree with
 *		the cache internal data structures.
 *
 *		Return size data in the locations specified by the pointer
 *		parameters if these parameters are not NULL.  Print the
 *		data to stdout if requested.
 *
 *		If an error is detected, set pass to FALSE, and set
 *		failure_mssg to an appropriate value.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              4/18/04
 *
 *-------------------------------------------------------------------------
 */

void
check_and_validate_cache_size(hid_t file_id,
                              size_t * max_size_ptr,
                              size_t * min_clean_size_ptr,
                              size_t * cur_size_ptr,
                              int32_t * cur_num_entries_ptr,
                              hbool_t dump_data)
{
    /* const char * fcn_name = "check_and_validate_cache_size()"; */
    herr_t result;
    size_t expected_max_size;
    size_t max_size;
    size_t expected_min_clean_size;
    size_t min_clean_size;
    size_t expected_cur_size;
    size_t cur_size;
    int32_t expected_cur_num_entries;
    int cur_num_entries;
    H5F_t * file_ptr = NULL;
    H5C_t * cache_ptr = NULL;

    /* get a pointer to the files internal data structure */
    if ( pass ) {

        file_ptr = (H5F_t *)H5I_object_verify(file_id, H5I_FILE);

        if ( file_ptr == NULL ) {

            pass = FALSE;
            failure_mssg = "Can't get file_ptr.";

        } else {

            cache_ptr = file_ptr->shared->cache;
        }
    }

    /* verify that we can access the cache data structure */
    if ( pass ) {

        if ( ( cache_ptr == NULL ) ||
             ( cache_ptr->magic != H5C__H5C_T_MAGIC ) ) {

            pass = FALSE;
            failure_mssg = "Can't access cache data structure.";
        }
    }

    /* compare the cache's internal configuration with the expected value */
    if ( pass ) {

        expected_max_size        = cache_ptr->max_cache_size;
        expected_min_clean_size  = cache_ptr->min_clean_size;
        expected_cur_size        = cache_ptr->index_size;
        expected_cur_num_entries = cache_ptr->index_len;

        result = H5Fget_mdc_size(file_id,
                                 &max_size,
                                 &min_clean_size,
                                 &cur_size,
                                 &cur_num_entries);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Fget_mdc_size() failed.";

        } else if ( ( max_size != expected_max_size ) ||
                    ( min_clean_size != expected_min_clean_size ) ||
                    ( cur_size != expected_cur_size ) ||
                    ( cur_num_entries != (int)expected_cur_num_entries ) ) {

            pass = FALSE;
            failure_mssg = "H5Fget_mdc_size() returned unexpected value(s).";

        }
    }

    /* return size values if requested */
    if ( ( pass ) && ( max_size_ptr != NULL ) ) {

        *max_size_ptr = max_size;
    }

    if ( ( pass ) && ( min_clean_size_ptr != NULL ) ) {

        *min_clean_size_ptr = min_clean_size;
    }

    if ( ( pass ) && ( cur_size_ptr != NULL ) ) {

        *cur_size_ptr = cur_size;
    }

    if ( ( pass ) && ( cur_num_entries_ptr != NULL ) ) {

        *cur_num_entries_ptr = cur_num_entries;
    }


    /* dump data to stdout if requested */
    if ( ( pass ) && ( dump_data ) ) {

        HDfprintf(stdout,
                  "max_sz: %ld, min_clean_sz: %ld, cur_sz: %ld, cur_ent: %ld\n",
                  (long)max_size, (long)min_clean_size, (long)cur_size,
                  (long)cur_num_entries);
    }

    return;

} /* check_and_validate_cache_size() */


/*-------------------------------------------------------------------------
 * Function:	validate_mdc_config()
 *
 * Purpose:	Verify that the file indicated by the file_id parameter
 *		has both internal and external configuration matching
 *		*config_ptr.
 *
 *		Do nothin on success.  On failure, set pass to FALSE, and
 *		load an error message into failue_mssg.  Note that
 *		failure_msg is assumed to be at least 128 bytes in length.
 *
 * Return:	void
 *
 * Programmer:	John Mainzer
 *              4/14/04
 *
 *-------------------------------------------------------------------------
 */

void
validate_mdc_config(hid_t file_id,
                    H5AC_cache_config_t * ext_config_ptr,
                    hbool_t compare_init,
                    int test_num)
{
    /* const char * fcn_name = "validate_mdc_config()"; */
    static char msg[256];
    H5F_t * file_ptr = NULL;
    H5C_t * cache_ptr = NULL;
    H5AC_cache_config_t scratch;
    H5C_auto_size_ctl_t int_config;

    XLATE_EXT_TO_INT_MDC_CONFIG(int_config, (*ext_config_ptr))

    /* get a pointer to the files internal data structure */
    if ( pass ) {

        file_ptr = (H5F_t *)H5I_object_verify(file_id, H5I_FILE);

        if ( file_ptr == NULL ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128, "Can't get file_ptr #%d.", test_num);
            failure_mssg = msg;

        } else {

            cache_ptr = file_ptr->shared->cache;
        }
    }

    /* verify that we can access the internal version of the cache config */
    if ( pass ) {

        if ( ( cache_ptr == NULL ) ||
             ( cache_ptr->magic != H5C__H5C_T_MAGIC ) ||
             ( cache_ptr->resize_ctl.version != H5C__CURR_AUTO_SIZE_CTL_VER ) ){

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "Can't access cache resize_ctl #%d.", test_num);
            failure_mssg = msg;
        }
    }

    /* compare the cache's internal configuration with the expected value */
    if ( pass ) {

	if ( ! RESIZE_CONFIGS_ARE_EQUAL(int_config, cache_ptr->resize_ctl,
                                        compare_init) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "Unexpected internal config #%d.", test_num);
            failure_mssg = msg;
        }
    }

    /* obtain external cache config */
    if ( pass ) {

        scratch.version = H5AC__CURR_CACHE_CONFIG_VERSION;

        if ( H5Fget_mdc_config(file_id, (H5AC_cache_config_t *)&scratch) < 0 ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "H5Fget_mdc_config() failed #%d.", test_num);
            failure_mssg = msg;
        }
    }

    if ( pass ) {

        /* Recall that in any configuration supplied by the cache
         * at run time, the set_initial_size field will always
         * be FALSE, regardless of the value passed in.  Thus we
         * always presume that this field need not match that of
         * the supplied external configuration.
         *
         * The cache also sets the initial_size field to the current
         * cache max size instead of the value initialy supplied.
         * Depending on circumstances, this may or may not match
         * the original.  Hence the compare_init parameter.
         */
        if ( ! CACHE_CONFIGS_EQUAL((*ext_config_ptr), scratch, \
                                   FALSE, compare_init) ) {

            pass = FALSE;
            HDsnprintf(msg, (size_t)128,
                       "Unexpected external config #%d.", test_num);
            failure_mssg = msg;
        }
    }

    return;

} /* validate_mdc_config() */

