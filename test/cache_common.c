/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 *        This file contains common code for tests of the cache
 *        implemented in H5C.c
 */
#include "H5private.h"
#include "H5CXprivate.h" /* API Contexts                         */
#include "H5MMprivate.h"

#include "cache_common.h"

bool        pass         = true; /* set to false on error */
const char *failure_mssg = NULL;
static char tmp_msg_buf[256];

static test_entry_t *pico_entries = NULL, *orig_pico_entries = NULL;
static test_entry_t *nano_entries = NULL, *orig_nano_entries = NULL;
static test_entry_t *micro_entries = NULL, *orig_micro_entries = NULL;
static test_entry_t *tiny_entries = NULL, *orig_tiny_entries = NULL;
static test_entry_t *small_entries = NULL, *orig_small_entries = NULL;
static test_entry_t *medium_entries = NULL, *orig_medium_entries = NULL;
static test_entry_t *large_entries = NULL, *orig_large_entries = NULL;
static test_entry_t *huge_entries = NULL, *orig_huge_entries = NULL;
static test_entry_t *monster_entries = NULL, *orig_monster_entries = NULL;
static test_entry_t *variable_entries = NULL, *orig_variable_entries = NULL;
static test_entry_t *notify_entries = NULL, *orig_notify_entries = NULL;

bool orig_entry_arrays_init = false;

static herr_t pico_get_initial_load_size(void *udata_ptr, size_t *image_len_ptr);
static herr_t nano_get_initial_load_size(void *udata_ptr, size_t *image_len_ptr);
static herr_t micro_get_initial_load_size(void *udata_ptr, size_t *image_len_ptr);
static herr_t tiny_get_initial_load_size(void *udata_ptr, size_t *image_len_ptr);
static herr_t small_get_initial_load_size(void *udata_ptr, size_t *image_len_ptr);
static herr_t medium_get_initial_load_size(void *udata_ptr, size_t *image_len_ptr);
static herr_t large_get_initial_load_size(void *udata_ptr, size_t *image_len_ptr);
static herr_t huge_get_initial_load_size(void *udata_ptr, size_t *image_len_ptr);
static herr_t monster_get_initial_load_size(void *udata_ptr, size_t *image_len_ptr);
static herr_t variable_get_initial_load_size(void *udata_ptr, size_t *image_len_ptr);
static herr_t notify_get_initial_load_size(void *udata_ptr, size_t *image_len_ptr);

static herr_t variable_get_final_load_size(const void *image, size_t image_len, void *udata,
                                           size_t *actual_len);

static htri_t variable_verify_chksum(const void *image_ptr, size_t len, void *udata_ptr);

static void *pico_deserialize(const void *image_ptr, size_t len, void *udata_ptr, bool *dirty_ptr);
static void *nano_deserialize(const void *image_ptr, size_t len, void *udata_ptr, bool *dirty_ptr);
static void *micro_deserialize(const void *image_ptr, size_t len, void *udata_ptr, bool *dirty_ptr);
static void *tiny_deserialize(const void *image_ptr, size_t len, void *udata_ptr, bool *dirty_ptr);
static void *small_deserialize(const void *image_ptr, size_t len, void *udata_ptr, bool *dirty_ptr);
static void *medium_deserialize(const void *image_ptr, size_t len, void *udata_ptr, bool *dirty_ptr);
static void *large_deserialize(const void *image_ptr, size_t len, void *udata_ptr, bool *dirty_ptr);
static void *huge_deserialize(const void *image_ptr, size_t len, void *udata_ptr, bool *dirty_ptr);
static void *monster_deserialize(const void *image_ptr, size_t len, void *udata_ptr, bool *dirty_ptr);
static void *variable_deserialize(const void *image_ptr, size_t len, void *udata_ptr, bool *dirty_ptr);
static void *notify_deserialize(const void *image_ptr, size_t len, void *udata_ptr, bool *dirty_ptr);

static herr_t pico_image_len(const void *thing, size_t *image_len_ptr);
static herr_t nano_image_len(const void *thing, size_t *image_len_ptr);
static herr_t micro_image_len(const void *thing, size_t *image_len_ptr);
static herr_t tiny_image_len(const void *thing, size_t *image_len_ptr);
static herr_t small_image_len(const void *thing, size_t *image_len_ptr);
static herr_t medium_image_len(const void *thing, size_t *image_len_ptr);
static herr_t large_image_len(const void *thing, size_t *image_len_ptr);
static herr_t huge_image_len(const void *thing, size_t *image_len_ptr);
static herr_t monster_image_len(const void *thing, size_t *image_len_ptr);
static herr_t variable_image_len(const void *thing, size_t *image_len_ptr);
static herr_t notify_image_len(const void *thing, size_t *image_len_ptr);

static herr_t pico_pre_serialize(H5F_t *f, void *thing, haddr_t addr, size_t len, haddr_t *new_addr_ptr,
                                 size_t *new_len_ptr, unsigned *flags_ptr);
static herr_t nano_pre_serialize(H5F_t *f, void *thing, haddr_t addr, size_t len, haddr_t *new_addr_ptr,
                                 size_t *new_len_ptr, unsigned *flags_ptr);
static herr_t micro_pre_serialize(H5F_t *f, void *thing, haddr_t addr, size_t len, haddr_t *new_addr_ptr,
                                  size_t *new_len_ptr, unsigned *flags_ptr);
static herr_t tiny_pre_serialize(H5F_t *f, void *thing, haddr_t addr, size_t len, haddr_t *new_addr_ptr,
                                 size_t *new_len_ptr, unsigned *flags_ptr);
static herr_t small_pre_serialize(H5F_t *f, void *thing, haddr_t addr, size_t len, haddr_t *new_addr_ptr,
                                  size_t *new_len_ptr, unsigned *flags_ptr);
static herr_t medium_pre_serialize(H5F_t *f, void *thing, haddr_t addr, size_t len, haddr_t *new_addr_ptr,
                                   size_t *new_len_ptr, unsigned *flags_ptr);
static herr_t large_pre_serialize(H5F_t *f, void *thing, haddr_t addr, size_t len, haddr_t *new_addr_ptr,
                                  size_t *new_len_ptr, unsigned *flags_ptr);
static herr_t huge_pre_serialize(H5F_t *f, void *thing, haddr_t addr, size_t len, haddr_t *new_addr_ptr,
                                 size_t *new_len_ptr, unsigned *flags_ptr);
static herr_t monster_pre_serialize(H5F_t *f, void *thing, haddr_t addr, size_t len, haddr_t *new_addr_ptr,
                                    size_t *new_len_ptr, unsigned *flags_ptr);
static herr_t variable_pre_serialize(H5F_t *f, void *thing, haddr_t addr, size_t len, haddr_t *new_addr_ptr,
                                     size_t *new_len_ptr, unsigned *flags_ptr);
static herr_t notify_pre_serialize(H5F_t *f, void *thing, haddr_t addr, size_t len, haddr_t *new_addr_ptr,
                                   size_t *new_len_ptr, unsigned *flags_ptr);

static herr_t pico_serialize(const H5F_t *f, void *image_ptr, size_t len, void *thing);
static herr_t nano_serialize(const H5F_t *f, void *image_ptr, size_t len, void *thing);
static herr_t micro_serialize(const H5F_t *f, void *image_ptr, size_t len, void *thing);
static herr_t tiny_serialize(const H5F_t *f, void *image_ptr, size_t len, void *thing);
static herr_t small_serialize(const H5F_t *f, void *image_ptr, size_t len, void *thing);
static herr_t medium_serialize(const H5F_t *f, void *image_ptr, size_t len, void *thing);
static herr_t large_serialize(const H5F_t *f, void *image_ptr, size_t len, void *thing);
static herr_t huge_serialize(const H5F_t *f, void *image_ptr, size_t len, void *thing);
static herr_t monster_serialize(const H5F_t *f, void *image_ptr, size_t len, void *thing);
static herr_t variable_serialize(const H5F_t *f, void *image_ptr, size_t len, void *thing);
static herr_t notify_serialize(const H5F_t *f, void *image_ptr, size_t len, void *thing);

static herr_t pico_free_icr(void *thing);
static herr_t nano_free_icr(void *thing);
static herr_t micro_free_icr(void *thing);
static herr_t tiny_free_icr(void *thing);
static herr_t small_free_icr(void *thing);
static herr_t medium_free_icr(void *thing);
static herr_t large_free_icr(void *thing);
static herr_t huge_free_icr(void *thing);
static herr_t monster_free_icr(void *thing);
static herr_t variable_free_icr(void *thing);
static herr_t notify_free_icr(void *thing);

static herr_t notify_notify(H5C_notify_action_t action, void *thing);

static void mark_flush_dep_dirty(test_entry_t *entry_ptr);
static void mark_flush_dep_clean(test_entry_t *entry_ptr);

/* Generic callback routines */
static herr_t get_initial_load_size(void *udata_ptr, size_t *image_len_ptr, int32_t entry_type);
static herr_t get_final_load_size(const void *image, size_t image_len, void *udata, size_t *actual_len,
                                  int32_t entry_type);
static void  *deserialize(const void *image_ptr, size_t len, void *udata_ptr, bool *dirty_ptr,
                          int32_t entry_type);
static herr_t image_len(const void *thing, size_t *image_len_ptr, int32_t entry_type);
static herr_t pre_serialize(H5F_t *f, void *thing, haddr_t addr, size_t len, haddr_t *new_addr_ptr,
                            size_t *new_len_ptr, unsigned *flags_ptr);
static herr_t serialize(const H5F_t *f, void *image_ptr, size_t len, void *thing);
static herr_t notify(H5C_notify_action_t action, void *thing, int32_t entry_type);
static herr_t free_icr(test_entry_t *entry, int32_t entry_type);

/* Local routines */
static void execute_flush_op(H5F_t *file_ptr, struct test_entry_t *entry_ptr, struct flush_op *op_ptr,
                             unsigned *flags_ptr);

test_entry_t *entries[NUMBER_OF_ENTRY_TYPES];

test_entry_t *orig_entries[NUMBER_OF_ENTRY_TYPES];

const int32_t max_indices[NUMBER_OF_ENTRY_TYPES] = {
    NUM_PICO_ENTRIES - 1,    NUM_NANO_ENTRIES - 1,     NUM_MICRO_ENTRIES - 1, NUM_TINY_ENTRIES - 1,
    NUM_SMALL_ENTRIES - 1,   NUM_MEDIUM_ENTRIES - 1,   NUM_LARGE_ENTRIES - 1, NUM_HUGE_ENTRIES - 1,
    NUM_MONSTER_ENTRIES - 1, NUM_VARIABLE_ENTRIES - 1, NUM_NOTIFY_ENTRIES - 1};

const size_t entry_sizes[NUMBER_OF_ENTRY_TYPES] = {PICO_ENTRY_SIZE,     NANO_ENTRY_SIZE,  MICRO_ENTRY_SIZE,
                                                   TINY_ENTRY_SIZE,     SMALL_ENTRY_SIZE, MEDIUM_ENTRY_SIZE,
                                                   LARGE_ENTRY_SIZE,    HUGE_ENTRY_SIZE,  MONSTER_ENTRY_SIZE,
                                                   VARIABLE_ENTRY_SIZE, NOTIFY_ENTRY_SIZE};

const haddr_t base_addrs[NUMBER_OF_ENTRY_TYPES] = {
    PICO_BASE_ADDR,  NANO_BASE_ADDR, MICRO_BASE_ADDR,   TINY_BASE_ADDR,     SMALL_BASE_ADDR, MEDIUM_BASE_ADDR,
    LARGE_BASE_ADDR, HUGE_BASE_ADDR, MONSTER_BASE_ADDR, VARIABLE_BASE_ADDR, NOTIFY_BASE_ADDR};

const haddr_t alt_base_addrs[NUMBER_OF_ENTRY_TYPES] = {
    PICO_ALT_BASE_ADDR,    NANO_ALT_BASE_ADDR,     MICRO_ALT_BASE_ADDR, TINY_ALT_BASE_ADDR,
    SMALL_ALT_BASE_ADDR,   MEDIUM_ALT_BASE_ADDR,   LARGE_ALT_BASE_ADDR, HUGE_ALT_BASE_ADDR,
    MONSTER_ALT_BASE_ADDR, VARIABLE_ALT_BASE_ADDR, NOTIFY_ALT_BASE_ADDR};

/* Callback classes */
static const H5C_class_t pico_class[1] = {{
    PICO_ENTRY_TYPE,
    "pico_entry",
    H5FD_MEM_DEFAULT,
    H5C__CLASS_NO_FLAGS_SET,
    pico_get_initial_load_size,
    NULL,
    NULL,
    pico_deserialize,
    pico_image_len,
    pico_pre_serialize,
    pico_serialize,
    NULL,
    pico_free_icr,
    NULL,
}};

static const H5C_class_t nano_class[1] = {{
    NANO_ENTRY_TYPE,
    "nano_entry",
    H5FD_MEM_DEFAULT,
    H5C__CLASS_NO_FLAGS_SET,
    nano_get_initial_load_size,
    NULL,
    NULL,
    nano_deserialize,
    nano_image_len,
    nano_pre_serialize,
    nano_serialize,
    NULL,
    nano_free_icr,
    NULL,
}};

static const H5C_class_t micro_class[1] = {{
    MICRO_ENTRY_TYPE,
    "micro_entry",
    H5FD_MEM_DEFAULT,
    H5C__CLASS_NO_FLAGS_SET,
    micro_get_initial_load_size,
    NULL,
    NULL,
    micro_deserialize,
    micro_image_len,
    micro_pre_serialize,
    micro_serialize,
    NULL,
    micro_free_icr,
    NULL,
}};

static const H5C_class_t tiny_class[1] = {{
    TINY_ENTRY_TYPE,
    "tiny_entry",
    H5FD_MEM_DEFAULT,
    H5C__CLASS_NO_FLAGS_SET,
    tiny_get_initial_load_size,
    NULL,
    NULL,
    tiny_deserialize,
    tiny_image_len,
    tiny_pre_serialize,
    tiny_serialize,
    NULL,
    tiny_free_icr,
    NULL,
}};

static const H5C_class_t small_class[1] = {{
    SMALL_ENTRY_TYPE,
    "small_entry",
    H5FD_MEM_DEFAULT,
    H5C__CLASS_NO_FLAGS_SET,
    small_get_initial_load_size,
    NULL,
    NULL,
    small_deserialize,
    small_image_len,
    small_pre_serialize,
    small_serialize,
    NULL,
    small_free_icr,
    NULL,
}};

static const H5C_class_t medium_class[1] = {{
    MEDIUM_ENTRY_TYPE,
    "medium_entry",
    H5FD_MEM_DEFAULT,
    H5C__CLASS_NO_FLAGS_SET,
    medium_get_initial_load_size,
    NULL,
    NULL,
    medium_deserialize,
    medium_image_len,
    medium_pre_serialize,
    medium_serialize,
    NULL,
    medium_free_icr,
    NULL,
}};

static const H5C_class_t large_class[1] = {{
    LARGE_ENTRY_TYPE,
    "large_entry",
    H5FD_MEM_DEFAULT,
    H5C__CLASS_NO_FLAGS_SET,
    large_get_initial_load_size,
    NULL,
    NULL,
    large_deserialize,
    large_image_len,
    large_pre_serialize,
    large_serialize,
    NULL,
    large_free_icr,
    NULL,
}};

static const H5C_class_t huge_class[1] = {{
    HUGE_ENTRY_TYPE,
    "huge_entry",
    H5FD_MEM_DEFAULT,
    H5C__CLASS_NO_FLAGS_SET,
    huge_get_initial_load_size,
    NULL,
    NULL,
    huge_deserialize,
    huge_image_len,
    huge_pre_serialize,
    huge_serialize,
    NULL,
    huge_free_icr,
    NULL,
}};

static const H5C_class_t monster_class[1] = {{
    MONSTER_ENTRY_TYPE,
    "monster_entry",
    H5FD_MEM_DEFAULT,
    H5C__CLASS_NO_FLAGS_SET,
    monster_get_initial_load_size,
    NULL,
    NULL,
    monster_deserialize,
    monster_image_len,
    monster_pre_serialize,
    monster_serialize,
    NULL,
    monster_free_icr,
    NULL,
}};

static const H5C_class_t variable_class[1] = {{
    VARIABLE_ENTRY_TYPE,
    "variable_entry",
    H5FD_MEM_DEFAULT,
    H5C__CLASS_SPECULATIVE_LOAD_FLAG,
    variable_get_initial_load_size,
    variable_get_final_load_size,
    variable_verify_chksum,
    variable_deserialize,
    variable_image_len,
    variable_pre_serialize,
    variable_serialize,
    NULL,
    variable_free_icr,
    NULL,
}};

static const H5C_class_t notify_class[1] = {{
    NOTIFY_ENTRY_TYPE,
    "notify_entry",
    H5FD_MEM_DEFAULT,
    H5C__CLASS_NO_FLAGS_SET,
    notify_get_initial_load_size,
    NULL,
    NULL,
    notify_deserialize,
    notify_image_len,
    notify_pre_serialize,
    notify_serialize,
    notify_notify,
    notify_free_icr,
    NULL,
}};

/* callback table declaration */

const H5C_class_t *types[NUMBER_OF_ENTRY_TYPES] = {pico_class,    nano_class,     micro_class, tiny_class,
                                                   small_class,   medium_class,   large_class, huge_class,
                                                   monster_class, variable_class, notify_class};

/* address translation functions: */

/*-------------------------------------------------------------------------
 * Function:    addr_to_type_and_index
 *
 * Purpose:    Given an address, compute the type and index of the
 *        associated entry.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */
void
addr_to_type_and_index(haddr_t addr, int32_t *type_ptr, int32_t *index_ptr)
{
    int     i;
    int32_t type;
    int32_t idx;

    assert(type_ptr);
    assert(index_ptr);

    /* we only have a small number of entry types, so just do a
     * linear search.  If NUMBER_OF_ENTRY_TYPES grows, we may want
     * to do a binary search instead.
     */
    i = 1;
    if (addr >= PICO_ALT_BASE_ADDR) {

        while ((i < NUMBER_OF_ENTRY_TYPES) && (addr >= alt_base_addrs[i])) {
            i++;
        }
    }
    else {

        while ((i < NUMBER_OF_ENTRY_TYPES) && (addr >= base_addrs[i])) {
            i++;
        }
    }

    type = i - 1;

    assert((type >= 0) && (type < NUMBER_OF_ENTRY_TYPES));

    if (addr >= PICO_ALT_BASE_ADDR) {

        idx = (int32_t)((addr - alt_base_addrs[type]) / entry_sizes[type]);
        assert((idx >= 0) && (idx <= max_indices[type]));
        assert(!((entries[type])[idx].at_main_addr));
        assert(addr == (entries[type])[idx].alt_addr);
    }
    else {

        idx = (int32_t)((addr - base_addrs[type]) / entry_sizes[type]);
        assert((idx >= 0) && (idx <= max_indices[type]));
        assert((entries[type])[idx].at_main_addr);
        assert(addr == (entries[type])[idx].main_addr);
    }

    assert(addr == (entries[type])[idx].addr);

    *type_ptr  = type;
    *index_ptr = idx;

} /* addr_to_type_and_index() */

/* Call back functions: */

/*-------------------------------------------------------------------------
 * Function:    get_initial_load_size & friends
 *
 * Purpose:    Query the image size for loading an entry.  The helper
 *              functions funnel into get_initial_load_size proper.
 *
 * Return:    SUCCEED
 *
 *-------------------------------------------------------------------------
 */
static herr_t
get_initial_load_size(void *udata, size_t *image_length, int32_t H5_ATTR_NDEBUG_UNUSED entry_type)
{
    test_entry_t *entry;
    test_entry_t *base_addr;
    haddr_t       addr = *(const haddr_t *)udata;
    int32_t       type;
    int32_t       idx;

    addr_to_type_and_index(addr, &type, &idx);

    base_addr = entries[type];
    entry     = &(base_addr[idx]);

    assert(entry->type >= 0);
    assert(entry->type == type);
    assert(entry->type == entry_type);
    assert(entry->type < NUMBER_OF_ENTRY_TYPES);
    assert(entry->index == idx);
    assert(entry->index >= 0);
    assert(entry->index <= max_indices[type]);
    assert(entry == entry->self);
    assert(entry->addr == addr);

    *image_length = entry->size;

    return (SUCCEED);
} /* get_initial_load_size() */

static herr_t
pico_get_initial_load_size(void *udata, size_t *image_length)
{
    return get_initial_load_size(udata, image_length, PICO_ENTRY_TYPE);
}

static herr_t
nano_get_initial_load_size(void *udata, size_t *image_length)
{
    return get_initial_load_size(udata, image_length, NANO_ENTRY_TYPE);
}

static herr_t
micro_get_initial_load_size(void *udata, size_t *image_length)
{
    return get_initial_load_size(udata, image_length, MICRO_ENTRY_TYPE);
}

static herr_t
tiny_get_initial_load_size(void *udata, size_t *image_length)
{
    return get_initial_load_size(udata, image_length, TINY_ENTRY_TYPE);
}

static herr_t
small_get_initial_load_size(void *udata, size_t *image_length)
{
    return get_initial_load_size(udata, image_length, SMALL_ENTRY_TYPE);
}

static herr_t
medium_get_initial_load_size(void *udata, size_t *image_length)
{
    return get_initial_load_size(udata, image_length, MEDIUM_ENTRY_TYPE);
}

static herr_t
large_get_initial_load_size(void *udata, size_t *image_length)
{
    return get_initial_load_size(udata, image_length, LARGE_ENTRY_TYPE);
}

static herr_t
huge_get_initial_load_size(void *udata, size_t *image_length)
{
    return get_initial_load_size(udata, image_length, HUGE_ENTRY_TYPE);
}

static herr_t
monster_get_initial_load_size(void *udata, size_t *image_length)
{
    return get_initial_load_size(udata, image_length, MONSTER_ENTRY_TYPE);
}

static herr_t
variable_get_initial_load_size(void *udata, size_t *image_length)
{
    return get_initial_load_size(udata, image_length, VARIABLE_ENTRY_TYPE);
}

static herr_t
notify_get_initial_load_size(void *udata, size_t *image_length)
{
    return get_initial_load_size(udata, image_length, NOTIFY_ENTRY_TYPE);
}

/*-------------------------------------------------------------------------
 * Function:    get_final_load_size & friends
 *
 * Purpose:    Query the final image size for loading an entry.  The helper
 *              functions funnel into get_final_load_size proper.
 *
 * Return:    SUCCEED
 *
 *-------------------------------------------------------------------------
 */
static herr_t
get_final_load_size(const void H5_ATTR_UNUSED *image, size_t H5_ATTR_UNUSED image_len, void *udata,
                    size_t *actual_len, int32_t H5_ATTR_NDEBUG_UNUSED entry_type)
{
    test_entry_t *entry;
    test_entry_t *base_addr;
    haddr_t       addr = *(const haddr_t *)udata;
    int32_t       type;
    int32_t       idx;

    addr_to_type_and_index(addr, &type, &idx);

    base_addr = entries[type];
    entry     = &(base_addr[idx]);

    assert(entry->type >= 0);
    assert(entry->type == type);
    assert(entry->type == entry_type);
    assert(entry->type < NUMBER_OF_ENTRY_TYPES);
    assert(entry->index == idx);
    assert(entry->index >= 0);
    assert(entry->index <= max_indices[type]);
    assert(entry == entry->self);
    assert(entry->addr == addr);
    assert(type == VARIABLE_ENTRY_TYPE);

    /* Simulate SPECULATIVE read with a specified actual_len */
    if (entry->actual_len) {
        *actual_len = entry->actual_len;
        entry->size = entry->actual_len;
    } /* end if */
    else
        *actual_len = entry->size;

    return (SUCCEED);
} /* get_final_load_size() */

static herr_t
variable_get_final_load_size(const void *image, size_t image_len, void *udata, size_t *actual_len)
{
    return get_final_load_size(image, image_len, udata, actual_len, VARIABLE_ENTRY_TYPE);
}

/*-------------------------------------------------------------------------
 * Function:    verify_chksum & friends
 *        (only done for VARIABLE_ENTRY_TYPE which has a speculative read)
 *
 * Purpose:    Simulate checksum verification:
 *          --check is ok only after 'max_verify_ct' is reached
 *          --otherwise check is not ok
 *
 * Return:    true: checksum is ok
 *        false: checksum is not ok
 *
 *-------------------------------------------------------------------------
 */

static htri_t
verify_chksum(const void H5_ATTR_UNUSED *image, size_t H5_ATTR_UNUSED len, void *udata,
              int32_t H5_ATTR_NDEBUG_UNUSED entry_type)
{
    test_entry_t *entry;
    test_entry_t *base_addr;
    haddr_t       addr = *(const haddr_t *)udata;
    int32_t       type;
    int32_t       idx;

    addr_to_type_and_index(addr, &type, &idx);

    base_addr = entries[type];
    entry     = &(base_addr[idx]);

    assert(entry->type >= 0);
    assert(entry->type == type);
    assert(entry->type == entry_type);
    assert(entry->type < NUMBER_OF_ENTRY_TYPES);
    assert(type == VARIABLE_ENTRY_TYPE);
    assert(entry->index == idx);
    assert(entry->index >= 0);
    assert(entry->index <= max_indices[type]);
    assert(entry == entry->self);
    assert(entry->addr == addr);

    if (++entry->verify_ct >= entry->max_verify_ct)
        return (true);
    else
        return (false);

} /* verify_chksum() */

static htri_t
variable_verify_chksum(const void *image, size_t len, void *udata)
{
    return verify_chksum(image, len, udata, VARIABLE_ENTRY_TYPE);
}

/*-------------------------------------------------------------------------
 * Function:    deserialize & friends
 *
 * Purpose:    deserialize the entry.  The helper functions verify that the
 *        correct version of deserialize is being called, and then call
 *        deserialize proper.
 *
 * Return:    void * (pointer to the in core representation of the entry)
 *
 *-------------------------------------------------------------------------
 */
static void *
deserialize(const void *image, size_t H5_ATTR_NDEBUG_UNUSED len, void *udata, bool *dirty,
            int32_t H5_ATTR_NDEBUG_UNUSED entry_type)
{
    test_entry_t *entry;
    test_entry_t *base_addr;
    haddr_t       addr = *(haddr_t *)udata;
    int32_t       type;
    int32_t       idx;

    addr_to_type_and_index(addr, &type, &idx);

    base_addr = entries[type];
    entry     = &(base_addr[idx]);

    assert(entry->type >= 0);
    assert(entry->type == type);
    assert(entry->type == entry_type);
    assert(entry->type < NUMBER_OF_ENTRY_TYPES);
    assert(entry->index == idx);
    assert(entry->index >= 0);
    assert(entry->index <= max_indices[type]);
    assert(entry == entry->self);
    assert(entry->addr == addr);
    assert(entry->size == len);
    assert((entry->type == VARIABLE_ENTRY_TYPE) || (entry->size == entry_sizes[type]));
    assert(dirty != NULL);
    assert(entry->flush_dep_npar == 0);
    assert(entry->flush_dep_nchd == 0);

    /* for now *dirty will always be false */
    *dirty = false;

    /* verify that the image contains the expected data. */
    assert(image != NULL);
    if ((entry->at_main_addr && entry->written_to_main_addr) ||
        (!entry->at_main_addr && entry->written_to_alt_addr)) {
        if ((type == PICO_ENTRY_TYPE) || (type == VARIABLE_ENTRY_TYPE) || (type == NOTIFY_ENTRY_TYPE)) {
            if ((*((const char *)image)) != (char)(idx & 0xFF)) {
                fprintf(stdout, "type = %d, idx = %d, addr = 0x%lx.\n", type, idx, (long)addr);
                fprintf(stdout, "*image = 0x%x\n", (int)(*((const char *)image)));
                fprintf(stdout, "expected *image = 0x%x\n", (int)(idx & 0xFF));
            } /* end if */
            assert((*((const char *)image)) == (char)(idx & 0xFF));
        } /* end if */
        else {
            if ((*(((const char *)image) + 2)) != (char)(idx & 0xFF)) {
                fprintf(stdout, "type = %d, idx = %d, addr = 0x%lx.\n", type, idx, (long)addr);
                fprintf(stdout, "*image = 0x%" PRIx8 " 0x%" PRIx8 " 0x%" PRIx8 "\n",
                        (*((const uint8_t *)image)), (*(((const uint8_t *)image) + 1)),
                        (*(((const uint8_t *)image) + 2)));
                fprintf(stdout, "expected *image = 0x%02" PRIx32 "%02" PRIx32 "\n", (uint32_t)idx & 0xFF,
                        (((uint32_t)idx & 0xFF00) >> 8));
            } /* end if */
            assert((*((const char *)image)) == (char)(type & 0xFF));
            assert((*(((const char *)image) + 1)) == (char)((idx & 0xFF00) >> 8));
            assert((*(((const char *)image) + 2)) == (char)(idx & 0xFF));
        } /* end else */
    }     /* end if */

    entry->deserialized    = true;
    entry->header.is_dirty = false;
    entry->is_dirty        = false;
    (entry->deserializes)++;

    return ((void *)entry);
} /* deserialize() */

void *
pico_deserialize(const void *image, size_t len, void *udata, bool *dirty)
{
    return deserialize(image, len, udata, dirty, PICO_ENTRY_TYPE);
}

void *
nano_deserialize(const void *image, size_t len, void *udata, bool *dirty)
{
    return deserialize(image, len, udata, dirty, NANO_ENTRY_TYPE);
}

void *
micro_deserialize(const void *image, size_t len, void *udata, bool *dirty)
{
    return deserialize(image, len, udata, dirty, MICRO_ENTRY_TYPE);
}

void *
tiny_deserialize(const void *image, size_t len, void *udata, bool *dirty)
{
    return deserialize(image, len, udata, dirty, TINY_ENTRY_TYPE);
}

void *
small_deserialize(const void *image, size_t len, void *udata, bool *dirty)
{
    return deserialize(image, len, udata, dirty, SMALL_ENTRY_TYPE);
}

void *
medium_deserialize(const void *image, size_t len, void *udata, bool *dirty)
{
    return deserialize(image, len, udata, dirty, MEDIUM_ENTRY_TYPE);
}

void *
large_deserialize(const void *image, size_t len, void *udata, bool *dirty)
{
    return deserialize(image, len, udata, dirty, LARGE_ENTRY_TYPE);
}

void *
huge_deserialize(const void *image, size_t len, void *udata, bool *dirty)
{
    return deserialize(image, len, udata, dirty, HUGE_ENTRY_TYPE);
}

void *
monster_deserialize(const void *image, size_t len, void *udata, bool *dirty)
{
    return deserialize(image, len, udata, dirty, MONSTER_ENTRY_TYPE);
}

void *
variable_deserialize(const void *image, size_t len, void *udata, bool *dirty)
{
    return deserialize(image, len, udata, dirty, VARIABLE_ENTRY_TYPE);
}

void *
notify_deserialize(const void *image, size_t len, void *udata, bool *dirty)
{
    return deserialize(image, len, udata, dirty, NOTIFY_ENTRY_TYPE);
}

/*-------------------------------------------------------------------------
 * Function:    image_len & friends
 *
 * Purpose:    Return the real (and possibly reduced) length of the image.
 *         The helper functions verify that the correct version of
 *         deserialize is being called, and then call deserialize
 *         proper.
 *
 * Return:    SUCCEED
 *
 *-------------------------------------------------------------------------
 */
herr_t
image_len(const void *thing, size_t *image_length, int32_t H5_ATTR_NDEBUG_UNUSED entry_type)
{
    const test_entry_t *entry;
    int32_t             type;

    assert(thing);
    assert(image_length);

    entry = (const test_entry_t *)thing;

    assert(entry->self == entry);

    type = entry->type;

    assert((type >= 0) && (type < NUMBER_OF_ENTRY_TYPES));
    assert(type == entry_type);
    assert((entry->index >= 0) && (entry->index <= max_indices[type]));

    assert(entry == &(entries[type][entry->index]));

    if (type != VARIABLE_ENTRY_TYPE)
        assert(entry->size == entry_sizes[type]);
    else {
        assert(entry->size <= entry_sizes[type]);
        assert(entry->size > 0);
    } /* end else */

    *image_length = entry->size;

    return (SUCCEED);
} /* image_len() */

herr_t
pico_image_len(const void *thing, size_t *image_length)
{
    return image_len(thing, image_length, PICO_ENTRY_TYPE);
}

herr_t
nano_image_len(const void *thing, size_t *image_length)
{
    return image_len(thing, image_length, NANO_ENTRY_TYPE);
}

herr_t
micro_image_len(const void *thing, size_t *image_length)
{
    return image_len(thing, image_length, MICRO_ENTRY_TYPE);
}

herr_t
tiny_image_len(const void *thing, size_t *image_length)
{
    return image_len(thing, image_length, TINY_ENTRY_TYPE);
}

herr_t
small_image_len(const void *thing, size_t *image_length)
{
    return image_len(thing, image_length, SMALL_ENTRY_TYPE);
}

herr_t
medium_image_len(const void *thing, size_t *image_length)
{
    return image_len(thing, image_length, MEDIUM_ENTRY_TYPE);
}

herr_t
large_image_len(const void *thing, size_t *image_length)
{
    return image_len(thing, image_length, LARGE_ENTRY_TYPE);
}

herr_t
huge_image_len(const void *thing, size_t *image_length)
{
    return image_len(thing, image_length, HUGE_ENTRY_TYPE);
}

herr_t
monster_image_len(const void *thing, size_t *image_length)
{
    return image_len(thing, image_length, MONSTER_ENTRY_TYPE);
}

herr_t
variable_image_len(const void *thing, size_t *image_length)
{
    return image_len(thing, image_length, VARIABLE_ENTRY_TYPE);
}

herr_t
notify_image_len(const void *thing, size_t *image_length)
{
    return image_len(thing, image_length, NOTIFY_ENTRY_TYPE);
}

/*-------------------------------------------------------------------------
 * Function:    pre_serialize & friends
 *
 * Purpose:    Pre_serialize the supplied entry.  For now this consists of
 *         executing any flush operations and loading the appropriate
 *        values into *new_addr_ptr, *new_len_ptr, and *flags_ptr.
 *
 *         The helper functions verify that the correct version of
 *         serialize is being called, and then call serialize
 *         proper.
 *
 * Return:    SUCCEED if successful, FAIL otherwise.
 *
 *-------------------------------------------------------------------------
 */
herr_t
pre_serialize(H5F_t H5_ATTR_NDEBUG_UNUSED *f, void *thing, haddr_t H5_ATTR_NDEBUG_UNUSED addr,
              size_t H5_ATTR_NDEBUG_UNUSED len, haddr_t *new_addr_ptr, size_t *new_len_ptr,
              unsigned *flags_ptr)
{
    test_entry_t *entry;
    int32_t       i;

    assert(f);
    assert(thing);
    assert(flags_ptr);

    *flags_ptr = H5C__SERIALIZE_NO_FLAGS_SET;

    assert(new_addr_ptr);
    assert(new_len_ptr);

    entry = (test_entry_t *)thing;

    assert(entry->self == entry);
    assert(entry->addr == addr);
    assert(entry->size == len);

    /* shouldn't serialize the entry unless it is dirty */
    assert(entry->is_dirty);
    assert((entry->type >= 0) && (entry->type < NUMBER_OF_ENTRY_TYPES));
    assert((entry->index >= 0) && (entry->index <= max_indices[entry->type]));
    assert(entry == &(entries[entry->type][entry->index]));
    assert(entry->num_flush_ops >= 0);
    assert(entry->num_flush_ops < MAX_FLUSH_OPS);

    if (entry->num_flush_ops > 0) {
        for (i = 0; i < entry->num_flush_ops; i++) {
            assert(entry->file_ptr);

            execute_flush_op(entry->file_ptr, entry, &((entry->flush_ops)[i]), flags_ptr);
        } /* end for */
        entry->num_flush_ops                    = 0;
        entry->flush_op_self_resize_in_progress = false;

        /* This looks wrong, but it isn't -- *flags_ptr will be modified
         * by execute_flush_op() only if the target is this entry --
         * and the flags set will accumulate over the set of calls in
         * the for loop.
         */
        if (pass && (((*flags_ptr) & H5C__SERIALIZE_RESIZED_FLAG) != 0)) {

            /* set *new_len_ptr to the new length. */

            assert(entry->type == VARIABLE_ENTRY_TYPE);
            assert(entry->size > 0);
            assert(entry->size <= VARIABLE_ENTRY_SIZE);

            *new_len_ptr = entry->size;
        } /* end if */

        if (((*flags_ptr) & H5C__SERIALIZE_MOVED_FLAG) != 0) {

            assert(((*flags_ptr) | H5C__SERIALIZE_RESIZED_FLAG) != 0);

            /* place the new address in *new_addr */
            *new_addr_ptr = entry->addr;
        } /* end if */
    }     /* end if */

    return (SUCCEED);

} /* pre_serialize() */

herr_t
pico_pre_serialize(H5F_t *f, void *thing, haddr_t addr, size_t len, haddr_t *new_addr_ptr,
                   size_t *new_len_ptr, unsigned *flags_ptr)
{
    return pre_serialize(f, thing, addr, len, new_addr_ptr, new_len_ptr, flags_ptr);
}

herr_t
nano_pre_serialize(H5F_t *f, void *thing, haddr_t addr, size_t len, haddr_t *new_addr_ptr,
                   size_t *new_len_ptr, unsigned *flags_ptr)
{
    return pre_serialize(f, thing, addr, len, new_addr_ptr, new_len_ptr, flags_ptr);
}

herr_t
micro_pre_serialize(H5F_t *f, void *thing, haddr_t addr, size_t len, haddr_t *new_addr_ptr,
                    size_t *new_len_ptr, unsigned *flags_ptr)
{
    return pre_serialize(f, thing, addr, len, new_addr_ptr, new_len_ptr, flags_ptr);
}

herr_t
tiny_pre_serialize(H5F_t *f, void *thing, haddr_t addr, size_t len, haddr_t *new_addr_ptr,
                   size_t *new_len_ptr, unsigned *flags_ptr)
{
    return pre_serialize(f, thing, addr, len, new_addr_ptr, new_len_ptr, flags_ptr);
}

herr_t
small_pre_serialize(H5F_t *f, void *thing, haddr_t addr, size_t len, haddr_t *new_addr_ptr,
                    size_t *new_len_ptr, unsigned *flags_ptr)
{
    return pre_serialize(f, thing, addr, len, new_addr_ptr, new_len_ptr, flags_ptr);
}

herr_t
medium_pre_serialize(H5F_t *f, void *thing, haddr_t addr, size_t len, haddr_t *new_addr_ptr,
                     size_t *new_len_ptr, unsigned *flags_ptr)
{
    return pre_serialize(f, thing, addr, len, new_addr_ptr, new_len_ptr, flags_ptr);
}

herr_t
large_pre_serialize(H5F_t *f, void *thing, haddr_t addr, size_t len, haddr_t *new_addr_ptr,
                    size_t *new_len_ptr, unsigned *flags_ptr)
{
    return pre_serialize(f, thing, addr, len, new_addr_ptr, new_len_ptr, flags_ptr);
}

herr_t
huge_pre_serialize(H5F_t *f, void *thing, haddr_t addr, size_t len, haddr_t *new_addr_ptr,
                   size_t *new_len_ptr, unsigned *flags_ptr)
{
    return pre_serialize(f, thing, addr, len, new_addr_ptr, new_len_ptr, flags_ptr);
}

herr_t
monster_pre_serialize(H5F_t *f, void *thing, haddr_t addr, size_t len, haddr_t *new_addr_ptr,
                      size_t *new_len_ptr, unsigned *flags_ptr)
{
    return pre_serialize(f, thing, addr, len, new_addr_ptr, new_len_ptr, flags_ptr);
}

herr_t
variable_pre_serialize(H5F_t *f, void *thing, haddr_t addr, size_t len, haddr_t *new_addr_ptr,
                       size_t *new_len_ptr, unsigned *flags_ptr)
{
    return pre_serialize(f, thing, addr, len, new_addr_ptr, new_len_ptr, flags_ptr);
}

herr_t
notify_pre_serialize(H5F_t *f, void *thing, haddr_t addr, size_t len, haddr_t *new_addr_ptr,
                     size_t *new_len_ptr, unsigned *flags_ptr)
{
    return pre_serialize(f, thing, addr, len, new_addr_ptr, new_len_ptr, flags_ptr);
}

/*-------------------------------------------------------------------------
 * Function:    serialize & friends
 *
 * Purpose:    Serialize the supplied entry.  For now this consists of
 *         loading the type and index of the entry into the first
 *         three bytes of the image (if it is long enough -- if not
 *         just load the low order byte of the index into the first
 *         byte of the image).
 *
 *         The helper functions verify that the correct version of
 *         serialize is being called, and then call serialize
 *         proper.
 *
 * Return:    SUCCEED if successful, FAIL otherwise.
 *
 *-------------------------------------------------------------------------
 */
herr_t
serialize(const H5F_t H5_ATTR_UNUSED *f, void *image_ptr, size_t len, void *thing)
{
    test_entry_t *entry;
    int32_t       type;

    assert(image_ptr);
    assert(thing);

    entry = (test_entry_t *)thing;

    assert(entry->self == entry);
    assert(entry->size == len);

    /* shouldn't serialize the entry unless it is dirty */
    assert(entry->is_dirty);

    type = entry->type;

    assert((type >= 0) && (type < NUMBER_OF_ENTRY_TYPES));
    assert((entry->index >= 0) && (entry->index <= max_indices[type]));

    assert(entry == &(entries[type][entry->index]));
    assert(entry->num_flush_ops >= 0);
    assert(entry->num_flush_ops < MAX_FLUSH_OPS);

    /* null out the image to avoid spurious failures */
    memset(image_ptr, 0, len);

    if ((type == PICO_ENTRY_TYPE) || (type == VARIABLE_ENTRY_TYPE) || (type == NOTIFY_ENTRY_TYPE)) {
        assert(entry->size >= PICO_ENTRY_SIZE);
        *((char *)image_ptr) = (char)((entry->index) & 0xFF);
    } /* end if */
    else {
        assert(entry->size >= NANO_ENTRY_SIZE);
        *((char *)image_ptr)       = (char)((entry->type) & 0xFF);
        *(((char *)image_ptr) + 1) = (char)(((entry->index) & 0xFF00) >> 8);
        *(((char *)image_ptr) + 2) = (char)((entry->index) & 0xFF);
    } /* end else */

    /* We no longer do the actual write through an callback -- this is
     * as close to that callback as we will get.  Hence mark the entry
     * clean here.  If all goes well, it will be flushed shortly.
     */
    entry->is_dirty = false;

    if (entry->flush_dep_npar > 0) {
        assert(entry->flush_dep_ndirty_chd == 0);
        mark_flush_dep_clean(entry);
    } /* end if */

    /* since the entry is about to be written to disk, we can mark it
     * as initialized.
     */
    if (entry->at_main_addr)
        entry->written_to_main_addr = true;
    else
        entry->written_to_alt_addr = true;

    /* do book keeping */
    (entry->serializes)++;
    entry->serialized = true;

    return (SUCCEED);
} /* serialize() */

herr_t
pico_serialize(const H5F_t H5_ATTR_UNUSED *f, void *image_ptr, size_t len, void *thing)
{
    return serialize(f, image_ptr, len, thing);
}

herr_t
nano_serialize(const H5F_t H5_ATTR_UNUSED *f, void *image_ptr, size_t len, void *thing)
{
    return serialize(f, image_ptr, len, thing);
}

herr_t
micro_serialize(const H5F_t H5_ATTR_UNUSED *f, void *image_ptr, size_t len, void *thing)
{
    return serialize(f, image_ptr, len, thing);
}

herr_t
tiny_serialize(const H5F_t H5_ATTR_UNUSED *f, void *image_ptr, size_t len, void *thing)
{
    return serialize(f, image_ptr, len, thing);
}

herr_t
small_serialize(const H5F_t H5_ATTR_UNUSED *f, void *image_ptr, size_t len, void *thing)
{
    return serialize(f, image_ptr, len, thing);
}

herr_t
medium_serialize(const H5F_t H5_ATTR_UNUSED *f, void *image_ptr, size_t len, void *thing)
{
    return serialize(f, image_ptr, len, thing);
}

herr_t
large_serialize(const H5F_t H5_ATTR_UNUSED *f, void *image_ptr, size_t len, void *thing)
{
    return serialize(f, image_ptr, len, thing);
}

herr_t
huge_serialize(const H5F_t H5_ATTR_UNUSED *f, void *image_ptr, size_t len, void *thing)
{
    return serialize(f, image_ptr, len, thing);
}

herr_t
monster_serialize(const H5F_t H5_ATTR_UNUSED *f, void *image_ptr, size_t len, void *thing)
{
    return serialize(f, image_ptr, len, thing);
}

herr_t
variable_serialize(const H5F_t H5_ATTR_UNUSED *f, void *image_ptr, size_t len, void *thing)
{
    return serialize(f, image_ptr, len, thing);
}

herr_t
notify_serialize(const H5F_t H5_ATTR_UNUSED *f, void *image_ptr, size_t len, void *thing)
{
    return serialize(f, image_ptr, len, thing);
}

/*-------------------------------------------------------------------------
 * Function:    notify & friends
 *
 * Purpose:    Record notifications of cache events for the entry.
 *              The helper functions verify that the correct version of notify
 *              is being called, and then call notify proper.
 *
 * Return:    SUCCEED
 *
 *-------------------------------------------------------------------------
 */
static herr_t
notify(H5C_notify_action_t action, void *thing, int32_t H5_ATTR_NDEBUG_UNUSED entry_type)
{
    test_entry_t *entry;

    assert(thing);

    entry = (test_entry_t *)thing;

    assert(entry->index >= 0);
    assert(entry->index <= max_indices[entry->type]);
    assert((entry->type >= 0) && (entry->type < NUMBER_OF_ENTRY_TYPES));
    assert(entry->type == entry_type);
    assert(entry == &(entries[entry->type][entry->index]));
    assert(entry == entry->self);
    if (!(action == H5C_NOTIFY_ACTION_ENTRY_DIRTIED && entry->action == TEST_ENTRY_ACTION_MOVE))
        assert(entry->header.addr == entry->addr);
    assert((entry->type == VARIABLE_ENTRY_TYPE) || (entry->size == entry_sizes[entry->type]));

    /* Increment count for appropriate action */
    switch (action) {
        case H5C_NOTIFY_ACTION_AFTER_INSERT: /* Entry has been added */
        case H5C_NOTIFY_ACTION_AFTER_LOAD:   /* to the cache.        */
            entry->notify_after_insert_count++;
            break;

        case H5C_NOTIFY_ACTION_AFTER_FLUSH:
        case H5C_NOTIFY_ACTION_ENTRY_DIRTIED:
        case H5C_NOTIFY_ACTION_ENTRY_CLEANED:
        case H5C_NOTIFY_ACTION_CHILD_DIRTIED:
        case H5C_NOTIFY_ACTION_CHILD_CLEANED:
        case H5C_NOTIFY_ACTION_CHILD_UNSERIALIZED:
        case H5C_NOTIFY_ACTION_CHILD_SERIALIZED:
            /* do nothing */
            break;

        case H5C_NOTIFY_ACTION_BEFORE_EVICT: /* Entry is about to be evicted from cache */
            entry->notify_before_evict_count++;
            break;

        default:
            assert(0 && "Unknown notify action!?!");
    } /* end switch */

    return (SUCCEED);
} /* notify() */

herr_t
notify_notify(H5C_notify_action_t action, void *thing)
{
    return (notify(action, thing, NOTIFY_ENTRY_TYPE));
}

/*-------------------------------------------------------------------------
 * Function:    free_icr & friends
 *
 * Purpose:    Nominally, this callback is supposed to free the
 *         in core representation of the entry.
 *
 *         In the context of this test bed, we use it to do
 *         do all the processing we used to do on a destroy.
 *         In particular, we use it to release all the pins
 *         that this entry may have on other entries.
 *
 *         The helper functions verify that the correct version of
 *         serialize is being called, and then call free_icr
 *         proper.
 *
 * Return:    SUCCEED
 *
 *-------------------------------------------------------------------------
 */
herr_t
free_icr(test_entry_t *entry, int32_t H5_ATTR_NDEBUG_UNUSED entry_type)
{
    assert(entry);

    assert(entry->type == entry_type);
    assert(entry->index >= 0);
    assert(entry->index <= max_indices[entry->type]);
    assert(entry == &(entries[entry->type][entry->index]));
    assert(entry == entry->self);
    assert(entry->cache_ptr != NULL);
    assert((entry->header.destroy_in_progress) || (entry->header.addr == entry->addr));
    assert(entry->header.size == entry->size);
    assert((entry->type == VARIABLE_ENTRY_TYPE) || (entry->size == entry_sizes[entry->type]));
    assert(entry->header.tl_next == NULL);
    assert(entry->header.tl_prev == NULL);

    if (entry->num_pins > 0) {
        int i;

        for (i = 0; i < entry->num_pins; i++) {
            test_entry_t *pinned_entry;
            test_entry_t *pinned_base_addr;

            pinned_base_addr = entries[entry->pin_type[i]];
            pinned_entry     = &(pinned_base_addr[entry->pin_idx[i]]);

            assert(0 <= pinned_entry->type);
            assert(pinned_entry->type < NUMBER_OF_ENTRY_TYPES);
            assert(pinned_entry->type == entry->pin_type[i]);
            assert(pinned_entry->index >= 0);
            assert(pinned_entry->index <= max_indices[pinned_entry->type]);
            assert(pinned_entry->index == entry->pin_idx[i]);
            assert(pinned_entry == pinned_entry->self);
            assert(pinned_entry->header.is_pinned);
            assert(pinned_entry->is_pinned);
            assert(pinned_entry->pinning_ref_count > 0);

            pinned_entry->pinning_ref_count--;

            if (pinned_entry->pinning_ref_count <= 0) {
                assert(pinned_entry->file_ptr);

                unpin_entry(pinned_entry->type, pinned_entry->index);
            } /* end if */

            entry->pin_type[i] = -1;
            entry->pin_idx[i]  = -1;
        } /* end if */
        entry->num_pins = 0;
    } /* end if */

    entry->destroyed = true;
    entry->cache_ptr = NULL;

    return (SUCCEED);
} /* free_icr() */

herr_t
pico_free_icr(void *thing)
{
    return free_icr((test_entry_t *)thing, PICO_ENTRY_TYPE);
}

herr_t
nano_free_icr(void *thing)
{
    return free_icr((test_entry_t *)thing, NANO_ENTRY_TYPE);
}

herr_t
micro_free_icr(void *thing)
{
    return free_icr((test_entry_t *)thing, MICRO_ENTRY_TYPE);
}

herr_t
tiny_free_icr(void *thing)
{
    return free_icr((test_entry_t *)thing, TINY_ENTRY_TYPE);
}

herr_t
small_free_icr(void *thing)
{
    return free_icr((test_entry_t *)thing, SMALL_ENTRY_TYPE);
}

herr_t
medium_free_icr(void *thing)
{
    return free_icr((test_entry_t *)thing, MEDIUM_ENTRY_TYPE);
}

herr_t
large_free_icr(void *thing)
{
    return free_icr((test_entry_t *)thing, LARGE_ENTRY_TYPE);
}

herr_t
huge_free_icr(void *thing)
{
    return free_icr((test_entry_t *)thing, HUGE_ENTRY_TYPE);
}

herr_t
monster_free_icr(void *thing)
{
    return free_icr((test_entry_t *)thing, MONSTER_ENTRY_TYPE);
}

herr_t
variable_free_icr(void *thing)
{
    return free_icr((test_entry_t *)thing, VARIABLE_ENTRY_TYPE);
}

herr_t
notify_free_icr(void *thing)
{
    return free_icr((test_entry_t *)thing, NOTIFY_ENTRY_TYPE);
}

/**************************************************************************/
/**************************************************************************/
/************************** test utility functions: ***********************/
/**************************************************************************/
/**************************************************************************/

/*-------------------------------------------------------------------------
 * Function:    add_flush_op
 *
 * Purpose:    Do nothing if pass is false on entry.
 *
 *              Otherwise, add the specified flush operation to the
 *              target instance of test_entry_t.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */

void
add_flush_op(int target_type, int target_idx, int op_code, int type, int idx, bool flag, size_t new_size,
             unsigned *order_ptr)
{
    int           i;
    test_entry_t *target_base_addr;
    test_entry_t *target_entry_ptr;

    assert((0 <= target_type) && (target_type < NUMBER_OF_ENTRY_TYPES));
    assert((0 <= target_idx) && (target_idx <= max_indices[target_type]));
    assert((0 <= op_code) && (op_code <= FLUSH_OP__MAX_OP));
    assert((op_code != FLUSH_OP__RESIZE) || (type == VARIABLE_ENTRY_TYPE));
    assert((0 <= type) && (type < NUMBER_OF_ENTRY_TYPES));
    assert((0 <= idx) && (idx <= max_indices[type]));
    assert(new_size <= VARIABLE_ENTRY_SIZE);

    if (pass) {

        target_base_addr = entries[target_type];
        target_entry_ptr = &(target_base_addr[target_idx]);

        assert(target_entry_ptr->index == target_idx);
        assert(target_entry_ptr->type == target_type);
        assert(target_entry_ptr == target_entry_ptr->self);
        assert(target_entry_ptr->num_flush_ops < MAX_FLUSH_OPS);

        i                                          = (target_entry_ptr->num_flush_ops)++;
        (target_entry_ptr->flush_ops)[i].op_code   = op_code;
        (target_entry_ptr->flush_ops)[i].type      = type;
        (target_entry_ptr->flush_ops)[i].idx       = idx;
        (target_entry_ptr->flush_ops)[i].flag      = flag;
        (target_entry_ptr->flush_ops)[i].size      = new_size;
        (target_entry_ptr->flush_ops)[i].order_ptr = order_ptr;
    }

} /* add_flush_op() */

/*-------------------------------------------------------------------------
 * Function:    create_pinned_entry_dependency
 *
 * Purpose:    Do nothing if pass is false on entry.
 *
 *              Otherwise, set up a pinned entry dependency so we can
 *              test the pinned entry modifications to the flush routine.
 *
 *        Given the types and indices of the pinned and pinning
 *        entries, add the pinned entry to the list of pinned
 *        entries in the pinning entry, increment the
 *        pinning reference count of the pinned entry, and
 *        if that count was zero initially, pin the entry.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */

void
create_pinned_entry_dependency(H5F_t *file_ptr, int pinning_type, int pinning_idx, int pinned_type,
                               int pinned_idx)
{
    test_entry_t *pinning_base_addr;
    test_entry_t *pinning_entry_ptr;
    test_entry_t *pinned_base_addr;
    test_entry_t *pinned_entry_ptr;

    if (pass) {

        assert((0 <= pinning_type) && (pinning_type < NUMBER_OF_ENTRY_TYPES));
        assert((0 <= pinning_idx) && (pinning_idx <= max_indices[pinning_type]));
        assert((0 <= pinned_type) && (pinned_type < NUMBER_OF_ENTRY_TYPES));
        assert((0 <= pinned_idx) && (pinned_idx <= max_indices[pinned_type]));

        pinning_base_addr = entries[pinning_type];
        pinning_entry_ptr = &(pinning_base_addr[pinning_idx]);

        pinned_base_addr = entries[pinned_type];
        pinned_entry_ptr = &(pinned_base_addr[pinned_idx]);

        assert(pinning_entry_ptr->index == pinning_idx);
        assert(pinning_entry_ptr->type == pinning_type);
        assert(pinning_entry_ptr == pinning_entry_ptr->self);
        assert(pinning_entry_ptr->num_pins < MAX_PINS);

        assert(pinning_entry_ptr->index == pinning_idx);
        assert(pinning_entry_ptr->type == pinning_type);
        assert(pinning_entry_ptr == pinning_entry_ptr->self);
        assert(!(pinning_entry_ptr->is_protected));

        pinning_entry_ptr->pin_type[pinning_entry_ptr->num_pins] = pinned_type;
        pinning_entry_ptr->pin_idx[pinning_entry_ptr->num_pins]  = pinned_idx;
        (pinning_entry_ptr->num_pins)++;

        if (pinned_entry_ptr->pinning_ref_count == 0) {

            protect_entry(file_ptr, pinned_type, pinned_idx);
            unprotect_entry(file_ptr, pinned_type, pinned_idx, H5C__PIN_ENTRY_FLAG);
        }

        (pinned_entry_ptr->pinning_ref_count)++;
    }

} /* create_pinned_entry_dependency() */

/*-------------------------------------------------------------------------
 * Function:    dirty_entry
 *
 * Purpose:    Given a pointer to a cache, an entry type, and an index,
 *        dirty the target entry.
 *
 *        If the dirty_pin parameter is true, verify that the
 *        target entry is in the cache and is pinned.  If it
 *        isn't, scream and die.  If it is, use the
 *        H5C_mark_entry_dirty() call to dirty it.
 *
 *        Do nothing if pass is false on entry.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */

void
dirty_entry(H5F_t *file_ptr, int32_t type, int32_t idx, bool dirty_pin)
{
    test_entry_t *base_addr;
    test_entry_t *entry_ptr;

    assert(file_ptr);
    assert((0 <= type) && (type < NUMBER_OF_ENTRY_TYPES));
    assert((0 <= idx) && (idx <= max_indices[type]));

    if (pass) {

        if (dirty_pin) {
            H5C_t *cache_ptr = file_ptr->shared->cache;

            assert(cache_ptr);

            if (!entry_in_cache(cache_ptr, type, idx)) {

                pass         = false;
                failure_mssg = "entry to be dirty pinned is not in cache.";
            }
            else {

                base_addr = entries[type];
                entry_ptr = &(base_addr[idx]);

                assert(entry_ptr->index == idx);
                assert(entry_ptr->type == type);
                assert(entry_ptr == entry_ptr->self);

                if (!((entry_ptr->header).is_pinned)) {

                    pass         = false;
                    failure_mssg = "entry to be dirty pinned is not pinned.";
                }
                else {

                    mark_entry_dirty(type, idx);
                }
            }
        }
        else {

            protect_entry(file_ptr, type, idx);
            unprotect_entry(file_ptr, type, idx, H5C__DIRTIED_FLAG);
        }
    }

} /* dirty_entry() */

/*-------------------------------------------------------------------------
 * Function:    execute_flush_op
 *
 * Purpose:    Given a pointer to an instance of struct flush_op, execute
 *         it.
 *
 *        Do nothing if pass is false on entry.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */

void
execute_flush_op(H5F_t *file_ptr, struct test_entry_t *entry_ptr, struct flush_op *op_ptr,
                 unsigned *flags_ptr)
{
    H5C_t *cache_ptr;

    assert(file_ptr);
    cache_ptr = file_ptr->shared->cache;
    assert(cache_ptr != NULL);
    assert(entry_ptr != NULL);
    assert(entry_ptr == entry_ptr->self);
    assert(entry_ptr->header.addr == entry_ptr->addr);
    assert((entry_ptr->flush_op_self_resize_in_progress) || (entry_ptr->header.size == entry_ptr->size));
    assert(op_ptr != NULL);
    assert((0 <= entry_ptr->type) && (entry_ptr->type < NUMBER_OF_ENTRY_TYPES));
    assert((0 <= entry_ptr->index) && (entry_ptr->index <= max_indices[entry_ptr->type]));
    assert((0 <= op_ptr->type) && (op_ptr->type < NUMBER_OF_ENTRY_TYPES));
    assert((0 <= op_ptr->idx) && (op_ptr->idx <= max_indices[op_ptr->type]));
    assert(flags_ptr != NULL);

    if (pass) {

        switch (op_ptr->op_code) {
            case FLUSH_OP__NO_OP:
                break;

            case FLUSH_OP__DIRTY:
                assert((entry_ptr->type != op_ptr->type) || (entry_ptr->index != op_ptr->idx));

                dirty_entry(file_ptr, op_ptr->type, op_ptr->idx, op_ptr->flag);
                break;

            case FLUSH_OP__RESIZE:
                if ((entry_ptr->type == op_ptr->type) && (entry_ptr->index == op_ptr->idx)) {

                    /* the flush operation is acting on the entry to
                     * which it is attached.  Handle this here:
                     */
                    assert(entry_ptr->type == VARIABLE_ENTRY_TYPE);
                    assert(op_ptr->size > 0);
                    assert(op_ptr->size <= VARIABLE_ENTRY_SIZE);

                    entry_ptr->size = op_ptr->size;

                    (*flags_ptr) |= H5C__SERIALIZE_RESIZED_FLAG;

                    entry_ptr->flush_op_self_resize_in_progress = true;
                }
                else {

                    /* change the size of some other entry */

                    resize_entry(file_ptr, op_ptr->type, op_ptr->idx, op_ptr->size, op_ptr->flag);
                }
                break;

            case FLUSH_OP__MOVE:
                if ((entry_ptr->type == op_ptr->type) && (entry_ptr->index == op_ptr->idx)) {

                    /* the flush operation is acting on the entry to
                     * which it is attached.  Handle this here:
                     */

                    assert(((*flags_ptr) & H5C__SERIALIZE_RESIZED_FLAG) != 0);
                    (*flags_ptr) |= H5C__SERIALIZE_MOVED_FLAG;

                    if (op_ptr->flag) {
                        assert(entry_ptr->addr == entry_ptr->alt_addr);
                        entry_ptr->addr         = entry_ptr->main_addr;
                        entry_ptr->at_main_addr = true;
                    } /* end if */
                    else {
                        assert(entry_ptr->addr == entry_ptr->main_addr);
                        entry_ptr->addr         = entry_ptr->alt_addr;
                        entry_ptr->at_main_addr = false;
                    } /* end else */
                }     /* end if */
                else
                    move_entry(cache_ptr, op_ptr->type, op_ptr->idx, op_ptr->flag);
                break;

            case FLUSH_OP__ORDER:
                assert(op_ptr->order_ptr);
                entry_ptr->flush_order = *op_ptr->order_ptr;
                (*op_ptr->order_ptr)++;
                break;

            case FLUSH_OP__EXPUNGE:
                /* the expunge flush op exists to allow us to simulate the
                 * case in which an entry is removed from the cache as the
                 * the result of the flush of a second entry.  At present,
                 * this can only happen via the take ownership flag, but
                 * we will make this test feature more general to as to make
                 * tests easier to write.
                 *
                 * When this operation is executed, the target entry is
                 * removed from the cache without being flushed if dirty
                 * via the expunge_entry() test function (which calls
                 * H5C_expunge_entry()).  Note that this flush operation
                 * must always be executed on an entry other than the
                 * entry being flushed.
                 */
                assert((entry_ptr->type != op_ptr->type) || (entry_ptr->index != op_ptr->idx));
                expunge_entry(file_ptr, op_ptr->type, op_ptr->idx);
                break;

            case FLUSH_OP__DEST_FLUSH_DEP:
                assert((entry_ptr->type != op_ptr->type) || (entry_ptr->index != op_ptr->idx));
                destroy_flush_dependency(op_ptr->type, op_ptr->idx, entry_ptr->type, entry_ptr->index);
                break;

            default:
                pass         = false;
                failure_mssg = "Undefined flush op code.";
                break;
        }
    }

} /* execute_flush_op() */

/*-------------------------------------------------------------------------
 * Function:    entry_in_cache
 *
 * Purpose:    Given a pointer to a cache, an entry type, and an index,
 *        determine if the entry is currently in the cache.
 *
 * Return:    true if the entry is in the cache, and false otherwise.
 *
 *-------------------------------------------------------------------------
 */

bool
entry_in_cache(H5C_t *cache_ptr, int32_t type, int32_t idx)
{
    bool               in_cache = false; /* will set to true if necessary */
    test_entry_t      *base_addr;
    test_entry_t      *entry_ptr;
    H5C_cache_entry_t *test_ptr = NULL;

    assert(cache_ptr);
    assert((0 <= type) && (type < NUMBER_OF_ENTRY_TYPES));
    assert((0 <= idx) && (idx <= max_indices[type]));

    base_addr = entries[type];
    entry_ptr = &(base_addr[idx]);

    assert(entry_ptr->index == idx);
    assert(entry_ptr->type == type);
    assert(entry_ptr == entry_ptr->self);

    H5C_TEST__SEARCH_INDEX(cache_ptr, entry_ptr->addr, test_ptr)

    if (test_ptr != NULL) {

        in_cache = true;
        assert(test_ptr == (H5C_cache_entry_t *)entry_ptr);
        assert(entry_ptr->addr == entry_ptr->header.addr);
    }

    return (in_cache);

} /* entry_in_cache() */

/*-------------------------------------------------------------------------
 * Function:    create_entry_arrays
 *
 * Purpose:     Create the entry arrays, both regular and original.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */

herr_t
create_entry_arrays(void)

{
    /* pico entries */
    if (NULL == (pico_entries = (test_entry_t *)calloc(NUM_PICO_ENTRIES, sizeof(test_entry_t))))
        goto error;
    if (NULL == (orig_pico_entries = (test_entry_t *)calloc(NUM_PICO_ENTRIES, sizeof(test_entry_t))))
        goto error;

    /* nano entries */
    if (NULL == (nano_entries = (test_entry_t *)calloc(NUM_NANO_ENTRIES, sizeof(test_entry_t))))
        goto error;
    if (NULL == (orig_nano_entries = (test_entry_t *)calloc(NUM_NANO_ENTRIES, sizeof(test_entry_t))))
        goto error;

    /* micro entries */
    if (NULL == (micro_entries = (test_entry_t *)calloc(NUM_MICRO_ENTRIES, sizeof(test_entry_t))))
        goto error;
    if (NULL == (orig_micro_entries = (test_entry_t *)calloc(NUM_MICRO_ENTRIES, sizeof(test_entry_t))))
        goto error;

    /* tiny entries */
    if (NULL == (tiny_entries = (test_entry_t *)calloc(NUM_TINY_ENTRIES, sizeof(test_entry_t))))
        goto error;
    if (NULL == (orig_tiny_entries = (test_entry_t *)calloc(NUM_TINY_ENTRIES, sizeof(test_entry_t))))
        goto error;

    /* small entries */
    if (NULL == (small_entries = (test_entry_t *)calloc(NUM_SMALL_ENTRIES, sizeof(test_entry_t))))
        goto error;
    if (NULL == (orig_small_entries = (test_entry_t *)calloc(NUM_SMALL_ENTRIES, sizeof(test_entry_t))))
        goto error;

    /* medium entries */
    if (NULL == (medium_entries = (test_entry_t *)calloc(NUM_MEDIUM_ENTRIES, sizeof(test_entry_t))))
        goto error;
    if (NULL == (orig_medium_entries = (test_entry_t *)calloc(NUM_MEDIUM_ENTRIES, sizeof(test_entry_t))))
        goto error;

    /* large entries */
    if (NULL == (large_entries = (test_entry_t *)calloc(NUM_LARGE_ENTRIES, sizeof(test_entry_t))))
        goto error;
    if (NULL == (orig_large_entries = (test_entry_t *)calloc(NUM_LARGE_ENTRIES, sizeof(test_entry_t))))
        goto error;

    /* huge entries */
    if (NULL == (huge_entries = (test_entry_t *)calloc(NUM_HUGE_ENTRIES, sizeof(test_entry_t))))
        goto error;
    if (NULL == (orig_huge_entries = (test_entry_t *)calloc(NUM_HUGE_ENTRIES, sizeof(test_entry_t))))
        goto error;

    /* monster entries */
    if (NULL == (monster_entries = (test_entry_t *)calloc(NUM_MONSTER_ENTRIES, sizeof(test_entry_t))))
        goto error;
    if (NULL == (orig_monster_entries = (test_entry_t *)calloc(NUM_MONSTER_ENTRIES, sizeof(test_entry_t))))
        goto error;

    /* variable entries */
    if (NULL == (variable_entries = (test_entry_t *)calloc(NUM_VARIABLE_ENTRIES, sizeof(test_entry_t))))
        goto error;
    if (NULL == (orig_variable_entries = (test_entry_t *)calloc(NUM_VARIABLE_ENTRIES, sizeof(test_entry_t))))
        goto error;

    /* notify entries */
    if (NULL == (notify_entries = (test_entry_t *)calloc(NUM_NOTIFY_ENTRIES, sizeof(test_entry_t))))
        goto error;
    if (NULL == (orig_notify_entries = (test_entry_t *)calloc(NUM_NOTIFY_ENTRIES, sizeof(test_entry_t))))
        goto error;

    entries[0]  = pico_entries;
    entries[1]  = nano_entries;
    entries[2]  = micro_entries;
    entries[3]  = tiny_entries;
    entries[4]  = small_entries;
    entries[5]  = medium_entries;
    entries[6]  = large_entries;
    entries[7]  = huge_entries;
    entries[8]  = monster_entries;
    entries[9]  = variable_entries;
    entries[10] = notify_entries;

    orig_entries[0]  = orig_pico_entries;
    orig_entries[1]  = orig_nano_entries;
    orig_entries[2]  = orig_micro_entries;
    orig_entries[3]  = orig_tiny_entries;
    orig_entries[4]  = orig_small_entries;
    orig_entries[5]  = orig_medium_entries;
    orig_entries[6]  = orig_large_entries;
    orig_entries[7]  = orig_huge_entries;
    orig_entries[8]  = orig_monster_entries;
    orig_entries[9]  = orig_variable_entries;
    orig_entries[10] = orig_notify_entries;

    return SUCCEED;

error:
    free_entry_arrays();
    return FAIL;

} /* create_entry_arrays() */

/*-------------------------------------------------------------------------
 * Function:    free_entry_arrays
 *
 * Purpose:     Free the entry arrays, both regular and original.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */

void
free_entry_arrays(void)

{
    /* pico entries */
    free(pico_entries);
    free(orig_pico_entries);

    /* nano entries */
    free(nano_entries);
    free(orig_nano_entries);

    /* micro entries */
    free(micro_entries);
    free(orig_micro_entries);

    /* tiny entries */
    free(tiny_entries);
    free(orig_tiny_entries);

    /* small entries */
    free(small_entries);
    free(orig_small_entries);

    /* medium entries */
    free(medium_entries);
    free(orig_medium_entries);

    /* large entries */
    free(large_entries);
    free(orig_large_entries);

    /* huge entries */
    free(huge_entries);
    free(orig_huge_entries);

    /* monster entries */
    free(monster_entries);
    free(orig_monster_entries);

    /* variable entries */
    free(variable_entries);
    free(orig_variable_entries);

    /* notify entries */
    free(notify_entries);
    free(orig_notify_entries);

} /* free_entry_arrays() */

/*-------------------------------------------------------------------------
 * Function:    reset_entries
 *
 * Purpose:    reset the contents of the entries arrays to known values.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */

void
reset_entries(void)

{
    int           i;
    int32_t       max_index;
    test_entry_t *base_addr;
    test_entry_t *orig_base_addr;

    if (!orig_entry_arrays_init) {
        haddr_t addr     = PICO_BASE_ADDR;
        haddr_t alt_addr = PICO_ALT_BASE_ADDR;
        size_t  entry_size;

        for (i = 0; i < NUMBER_OF_ENTRY_TYPES; i++) {
            int j;

            entry_size     = entry_sizes[i];
            max_index      = max_indices[i];
            base_addr      = entries[i];
            orig_base_addr = orig_entries[i];

            assert(base_addr);
            assert(orig_base_addr);

            for (j = 0; j <= max_index; j++) {
                int k;

                /* one can argue that we should fill the header with garbage.
                 * If this is desired, we can simply comment out the header
                 * initialization - the headers will be full of garbage soon
                 * enough.
                 */

                base_addr[j].header.addr         = (haddr_t)0;
                base_addr[j].header.size         = (size_t)0;
                base_addr[j].header.type         = NULL;
                base_addr[j].header.is_dirty     = false;
                base_addr[j].header.is_protected = false;
                base_addr[j].header.is_read_only = false;
                base_addr[j].header.ro_ref_count = false;
                base_addr[j].header.next         = NULL;
                base_addr[j].header.prev         = NULL;
#if H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS
                base_addr[j].header.aux_next = NULL;
                base_addr[j].header.aux_prev = NULL;
#endif /* H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

                base_addr[j].self                 = &(base_addr[j]);
                base_addr[j].cache_ptr            = NULL;
                base_addr[j].written_to_main_addr = false;
                base_addr[j].written_to_alt_addr  = false;
                base_addr[j].addr                 = addr;
                base_addr[j].at_main_addr         = true;
                base_addr[j].main_addr            = addr;
                base_addr[j].alt_addr             = alt_addr;
                base_addr[j].size                 = entry_size;
                base_addr[j].type                 = i;
                base_addr[j].index                = j;
                base_addr[j].serializes           = 0;
                base_addr[j].deserializes         = 0;
                base_addr[j].is_dirty             = false;
                base_addr[j].is_protected         = false;
                base_addr[j].is_read_only         = false;
                base_addr[j].ro_ref_count         = false;

                base_addr[j].is_corked = false;

                base_addr[j].is_pinned         = false;
                base_addr[j].pinning_ref_count = 0;
                base_addr[j].num_pins          = 0;
                for (k = 0; k < MAX_PINS; k++) {
                    base_addr[j].pin_type[k] = -1;
                    base_addr[j].pin_idx[k]  = -1;
                }

                base_addr[j].num_flush_ops = 0;
                for (k = 0; k < MAX_FLUSH_OPS; k++) {
                    base_addr[j].flush_ops[k].op_code = FLUSH_OP__NO_OP;
                    base_addr[j].flush_ops[k].type    = -1;
                    base_addr[j].flush_ops[k].idx     = -1;
                    base_addr[j].flush_ops[k].flag    = false;
                    base_addr[j].flush_ops[k].size    = 0;
                }
                base_addr[j].flush_op_self_resize_in_progress = false;

                base_addr[j].deserialized = false;
                base_addr[j].serialized   = false;
                base_addr[j].destroyed    = false;
                base_addr[j].expunged     = false;

                base_addr[j].flush_dep_npar       = 0;
                base_addr[j].flush_dep_nchd       = 0;
                base_addr[j].flush_dep_ndirty_chd = 0;
                base_addr[j].pinned_from_client   = false;
                base_addr[j].pinned_from_cache    = false;

                base_addr[j].flush_order = 0;

                base_addr[j].notify_after_insert_count = 0;
                base_addr[j].notify_before_evict_count = 0;

                base_addr[j].actual_len    = 0;
                base_addr[j].max_verify_ct = 0;
                base_addr[j].verify_ct     = 0;

                addr += (haddr_t)entry_size;
                alt_addr += (haddr_t)entry_size;
            } /* end for */

            /* Make copy of entries in base_addr for later */
            memcpy(orig_base_addr, base_addr, (size_t)(max_index + 1) * sizeof(*base_addr));
        } /* end for */

        /* Indicate that we've made a copy for later */
        orig_entry_arrays_init = true;
    } /* end if */
    else {
        for (i = 0; i < NUMBER_OF_ENTRY_TYPES; i++) {
            max_index      = max_indices[i];
            base_addr      = entries[i];
            orig_base_addr = orig_entries[i];

            /* Make copy of entries in base_addr for later */
            memcpy(base_addr, orig_base_addr, (size_t)(max_index + 1) * sizeof(*base_addr));
        } /* end for */
    }     /* end else */

} /* reset_entries() */

/*-------------------------------------------------------------------------
 * Function:    resize_entry
 *
 * Purpose:     Given a pointer to a cache, an entry type, an index, and
 *         a new size, set the size of the target entry to the new size.
 *
 *        Note that at present, the type of the entry must be
 *         VARIABLE_ENTRY_TYPE.
 *
 *              Do nothing if pass is false on entry.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */

void
resize_entry(H5F_t *file_ptr, int32_t type, int32_t idx, size_t new_size, bool in_cache)
{
    test_entry_t *base_addr;
    test_entry_t *entry_ptr;
    herr_t        result;

    assert((0 <= type) && (type < NUMBER_OF_ENTRY_TYPES));
    assert(type == VARIABLE_ENTRY_TYPE);
    assert((0 <= idx) && (idx <= max_indices[type]));
    assert((0 < new_size) && (new_size <= entry_sizes[type]));

    if (pass) {

        if (in_cache) {
            H5C_t *cache_ptr = file_ptr->shared->cache;

            assert(cache_ptr);

            if (!entry_in_cache(cache_ptr, type, idx)) {

                pass         = false;
                failure_mssg = "entry to be resized pinned is not in cache.";
            }
            else {

                base_addr = entries[type];
                entry_ptr = &(base_addr[idx]);

                assert(entry_ptr->index == idx);
                assert(entry_ptr->type == type);
                assert(entry_ptr->cache_ptr == cache_ptr);
                assert(entry_ptr == entry_ptr->self);

                if (!(entry_ptr->header.is_pinned || entry_ptr->header.is_protected)) {

                    pass         = false;
                    failure_mssg = "entry to be resized is not pinned or protected.";
                }
                else {
                    bool was_dirty = entry_ptr->is_dirty;

                    entry_ptr->size = new_size;

                    result              = H5C_resize_entry((void *)entry_ptr, new_size);
                    entry_ptr->is_dirty = true;

                    if (entry_ptr->flush_dep_npar > 0 && !was_dirty)
                        mark_flush_dep_dirty(entry_ptr);

                    if (result != SUCCEED) {

                        pass         = false;
                        failure_mssg = "error(s) in H5C_resize_entry().";
                    }
                    else {

                        assert(entry_ptr->size == (entry_ptr->header).size);
                    }
                }
            }
        }
        else {

            protect_entry(file_ptr, type, idx);
            resize_entry(file_ptr, type, idx, new_size, true);
            unprotect_entry(file_ptr, type, idx, H5C__DIRTIED_FLAG);
        }
    }

} /* resize_entry() */

/*-------------------------------------------------------------------------
 * Function:    verify_clean
 *
 * Purpose:    Verify that all cache entries are marked as clean.  If any
 *        are not, set pass to false.
 *
 *        Do nothing if pass is false on entry.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */

void
verify_clean(void)

{
    int           i;
    int           j;
    int           dirty_count = 0;
    int32_t       max_index;
    test_entry_t *base_addr;

    if (pass) {

        for (i = 0; i < NUMBER_OF_ENTRY_TYPES; i++) {
            max_index = max_indices[i];
            base_addr = entries[i];

            assert(base_addr);

            for (j = 0; j <= max_index; j++) {
                if ((base_addr[j].header.is_dirty) || (base_addr[j].is_dirty)) {

                    dirty_count++;
                }
            }
        }

        if (dirty_count > 0) {

            pass         = false;
            failure_mssg = "verify_clean() found dirty entry(s).";
        }
    }

} /* verify_clean() */

/*-------------------------------------------------------------------------
 * Function:    verify_entry_status
 *
 * Purpose:    Verify that a list of entries have the expected status.
 *         If any discrepancies are found, set the failure message
 *         and set pass to false.
 *
 *        Do nothing if pass is false on entry.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */

void
verify_entry_status(H5C_t *cache_ptr, int tag, int num_entries, struct expected_entry_status expected[])
{
    int i;

    i = 0;
    while ((pass) && (i < num_entries)) {
        test_entry_t *base_addr = entries[expected[i].entry_type];
        test_entry_t *entry_ptr = &(base_addr[expected[i].entry_index]);
        bool          in_cache  = false; /* will set to true if necessary */
        unsigned      u;                 /* Local index variable */

        if ((!expected[i].in_cache) && ((expected[i].is_protected) || (expected[i].is_pinned))) {

            pass = false;
            snprintf(tmp_msg_buf, sizeof(tmp_msg_buf), "%d: Contradictory data in expected[%d].\n", tag, i);
            failure_mssg = tmp_msg_buf;
        }

        if ((!expected[i].in_cache) && (expected[i].is_dirty) && (!entry_ptr->expunged)) {

            pass = false;
            snprintf(tmp_msg_buf, sizeof(tmp_msg_buf),
                     "%d: expected[%d] specs non-expunged, dirty, non-resident.\n", tag, i);
            failure_mssg = tmp_msg_buf;
        }

        if (pass) {

            in_cache = entry_in_cache(cache_ptr, expected[i].entry_type, expected[i].entry_index);

            if (in_cache != expected[i].in_cache) {

                pass = false;
                snprintf(tmp_msg_buf, sizeof(tmp_msg_buf),
                         "%d entry (%d, %d) in cache actual/expected = %d/%d.\n", tag,
                         (int)expected[i].entry_type, (int)expected[i].entry_index, (int)in_cache,
                         (int)expected[i].in_cache);
                failure_mssg = tmp_msg_buf;
            }
        }

        if (pass) {

            if (entry_ptr->size != expected[i].size) {

                pass = false;
                snprintf(tmp_msg_buf, sizeof(tmp_msg_buf),
                         "%d entry (%d, %d) size actual/expected = %ld/%ld.\n", tag,
                         (int)expected[i].entry_type, (int)expected[i].entry_index, (long)(entry_ptr->size),
                         (long)expected[i].size);
                failure_mssg = tmp_msg_buf;
            }
        }

        if ((pass) && (in_cache)) {

            if (entry_ptr->header.size != expected[i].size) {

                pass = false;
                snprintf(tmp_msg_buf, sizeof(tmp_msg_buf),
                         "%d entry (%d, %d) header size actual/expected = %ld/%ld.\n", tag,
                         (int)expected[i].entry_type, (int)expected[i].entry_index,
                         (long)(entry_ptr->header.size), (long)expected[i].size);
                failure_mssg = tmp_msg_buf;
            }
        }

        if (pass) {

            if (entry_ptr->at_main_addr != expected[i].at_main_addr) {

                pass = false;
                snprintf(tmp_msg_buf, sizeof(tmp_msg_buf),
                         "%d entry (%d, %d) at main addr actual/expected = %d/%d.\n", tag,
                         (int)expected[i].entry_type, (int)expected[i].entry_index,
                         (int)(entry_ptr->at_main_addr), (int)expected[i].at_main_addr);
                failure_mssg = tmp_msg_buf;
            }
        }

        if (pass) {

            if (entry_ptr->is_dirty != expected[i].is_dirty) {

                pass = false;
                snprintf(tmp_msg_buf, sizeof(tmp_msg_buf),
                         "%d entry (%d, %d) is_dirty actual/expected = %d/%d.\n", tag,
                         (int)expected[i].entry_type, (int)expected[i].entry_index,
                         (int)(entry_ptr->is_dirty), (int)expected[i].is_dirty);
                failure_mssg = tmp_msg_buf;
            }
        }

        if ((pass) && (in_cache)) {

            if (entry_ptr->header.is_dirty != expected[i].is_dirty) {

                pass = false;
                snprintf(tmp_msg_buf, sizeof(tmp_msg_buf),
                         "%d entry (%d, %d) header is_dirty actual/expected = %d/%d.\n", tag,
                         (int)expected[i].entry_type, (int)expected[i].entry_index,
                         (int)(entry_ptr->header.is_dirty), (int)expected[i].is_dirty);
                failure_mssg = tmp_msg_buf;
            }
        }

        if (pass) {

            if (entry_ptr->is_protected != expected[i].is_protected) {

                pass = false;
                snprintf(tmp_msg_buf, sizeof(tmp_msg_buf),
                         "%d entry (%d, %d) is_protected actual/expected = %d/%d.\n", tag,
                         (int)expected[i].entry_type, (int)expected[i].entry_index,
                         (int)(entry_ptr->is_protected), (int)expected[i].is_protected);
                failure_mssg = tmp_msg_buf;
            }
        }

        if ((pass) && (in_cache)) {

            if (entry_ptr->header.is_protected != expected[i].is_protected) {

                pass = false;
                snprintf(tmp_msg_buf, sizeof(tmp_msg_buf),
                         "%d entry (%d, %d) header is_protected actual/expected = %d/%d.\n", tag,
                         (int)expected[i].entry_type, (int)expected[i].entry_index,
                         (int)(entry_ptr->header.is_protected), (int)expected[i].is_protected);
                failure_mssg = tmp_msg_buf;
            }
        }

        if (pass) {

            if (entry_ptr->is_pinned != expected[i].is_pinned) {

                pass = false;
                snprintf(tmp_msg_buf, sizeof(tmp_msg_buf),
                         "%d entry (%d, %d) is_pinned actual/expected = %d/%d.\n", tag,
                         (int)expected[i].entry_type, (int)expected[i].entry_index,
                         (int)(entry_ptr->is_pinned), (int)expected[i].is_pinned);
                failure_mssg = tmp_msg_buf;
            }
        }

        if (pass) {

            if (entry_ptr->is_corked != expected[i].is_corked) {

                pass = false;
                snprintf(tmp_msg_buf, sizeof(tmp_msg_buf),
                         "%d entry (%d, %d) is_corked actual/expected = %d/%d.\n", tag,
                         (int)expected[i].entry_type, (int)expected[i].entry_index,
                         (int)(entry_ptr->is_corked), (int)expected[i].is_corked);
                failure_mssg = tmp_msg_buf;
            }
        }

        if ((pass) && (in_cache)) {

            if (entry_ptr->header.is_pinned != expected[i].is_pinned) {

                pass = false;
                snprintf(tmp_msg_buf, sizeof(tmp_msg_buf),
                         "%d entry (%d, %d) header is_pinned actual/expected = %d/%d.\n", tag,
                         (int)expected[i].entry_type, (int)expected[i].entry_index,
                         (int)(entry_ptr->header.is_pinned), (int)expected[i].is_pinned);
                failure_mssg = tmp_msg_buf;
            }
        }

        if (pass) {

            if ((entry_ptr->deserialized != expected[i].deserialized) ||
                (entry_ptr->serialized != expected[i].serialized) ||
                (entry_ptr->destroyed != expected[i].destroyed)) {

                pass = false;
                snprintf(tmp_msg_buf, sizeof(tmp_msg_buf),
                         "%d entry (%d,%d) deserialized = %d(%d), serialized = %d(%d), dest = %d(%d)\n", tag,
                         (int)expected[i].entry_type, (int)expected[i].entry_index,
                         (int)(entry_ptr->deserialized), (int)(expected[i].deserialized),
                         (int)(entry_ptr->serialized), (int)(expected[i].serialized),
                         (int)(entry_ptr->destroyed), (int)(expected[i].destroyed));
                failure_mssg = tmp_msg_buf;
            }
        }

        /* Check flush dependency fields */

        /* # of flush dependency parents */
        if (pass) {
            if (entry_ptr->flush_dep_npar != expected[i].flush_dep_npar) {
                pass = false;
                snprintf(tmp_msg_buf, sizeof(tmp_msg_buf),
                         "%d entry (%d, %d) flush_dep_npar actual/expected = %u/%u.\n", tag,
                         expected[i].entry_type, expected[i].entry_index, entry_ptr->flush_dep_npar,
                         expected[i].flush_dep_npar);
                failure_mssg = tmp_msg_buf;
            } /* end if */
        }     /* end if */
        if ((pass) && (in_cache)) {
            if (entry_ptr->header.flush_dep_nparents != expected[i].flush_dep_npar) {
                pass = false;
                snprintf(tmp_msg_buf, sizeof(tmp_msg_buf),
                         "%d entry (%d, %d) header flush_dep_nparents actual/expected = %u/%u.\n", tag,
                         expected[i].entry_type, expected[i].entry_index,
                         entry_ptr->header.flush_dep_nparents, expected[i].flush_dep_npar);
                failure_mssg = tmp_msg_buf;
            } /* end if */
        }     /* end if */

        /* Flush dependency parent type & index.  Note this algorithm assumes
         * that the parents in both arrays are in the same order. */
        if (pass) {
            for (u = 0; u < entry_ptr->flush_dep_npar; u++) {
                if (entry_ptr->flush_dep_par_type[u] != expected[i].flush_dep_par_type[u]) {
                    pass = false;
                    snprintf(tmp_msg_buf, sizeof(tmp_msg_buf),
                             "%d entry (%d, %d) flush_dep_par_type[%u] actual/expected = %d/%d.\n", tag,
                             expected[i].entry_type, expected[i].entry_index, u,
                             entry_ptr->flush_dep_par_type[u], expected[i].flush_dep_par_type[u]);
                    failure_mssg = tmp_msg_buf;
                } /* end if */
            }     /* end for */
        }         /* end if */
        if (pass) {
            for (u = 0; u < entry_ptr->flush_dep_npar; u++) {
                if (entry_ptr->flush_dep_par_idx[u] != expected[i].flush_dep_par_idx[u]) {
                    pass = false;
                    snprintf(tmp_msg_buf, sizeof(tmp_msg_buf),
                             "%d entry (%d, %d) flush_dep_par_idx[%u] actual/expected = %d/%d.\n", tag,
                             expected[i].entry_type, expected[i].entry_index, u,
                             entry_ptr->flush_dep_par_idx[u], expected[i].flush_dep_par_idx[u]);
                    failure_mssg = tmp_msg_buf;
                } /* end if */
            }     /* end for */
        }         /* end if */

        /* # of flush dependency children and dirty children */
        if (pass) {
            if (entry_ptr->flush_dep_nchd != expected[i].flush_dep_nchd) {
                pass = false;
                snprintf(tmp_msg_buf, sizeof(tmp_msg_buf),
                         "%d entry (%d, %d) flush_dep_nchd actual/expected = %u/%u.\n", tag,
                         expected[i].entry_type, expected[i].entry_index, entry_ptr->flush_dep_nchd,
                         expected[i].flush_dep_nchd);
                failure_mssg = tmp_msg_buf;
            } /* end if */
        }     /* end if */
        if ((pass) && (in_cache)) {
            if (entry_ptr->header.flush_dep_nchildren != expected[i].flush_dep_nchd) {
                pass = false;
                snprintf(tmp_msg_buf, sizeof(tmp_msg_buf),
                         "%d entry (%d, %d) header flush_dep_nchildren actual/expected = %u/%u.\n", tag,
                         expected[i].entry_type, expected[i].entry_index,
                         entry_ptr->header.flush_dep_nchildren, expected[i].flush_dep_nchd);
                failure_mssg = tmp_msg_buf;
            } /* end if */
        }     /* end if */
        if (pass) {
            if (entry_ptr->flush_dep_ndirty_chd != expected[i].flush_dep_ndirty_chd) {
                pass = false;
                snprintf(tmp_msg_buf, sizeof(tmp_msg_buf),
                         "%d entry (%d, %d) flush_dep_ndirty_chd actual/expected = %u/%u.\n", tag,
                         expected[i].entry_type, expected[i].entry_index, entry_ptr->flush_dep_ndirty_chd,
                         expected[i].flush_dep_ndirty_chd);
                failure_mssg = tmp_msg_buf;
            } /* end if */
        }     /* end if */
        if ((pass) && (in_cache)) {
            if (entry_ptr->header.flush_dep_ndirty_children != expected[i].flush_dep_ndirty_chd) {
                pass = false;
                snprintf(tmp_msg_buf, sizeof(tmp_msg_buf),
                         "%d entry (%d, %d) header flush_dep_ndirty_children actual/expected = %u/%u.\n", tag,
                         expected[i].entry_type, expected[i].entry_index,
                         entry_ptr->header.flush_dep_ndirty_children, expected[i].flush_dep_ndirty_chd);
                failure_mssg = tmp_msg_buf;
            } /* end if */
        }     /* end if */

        /* Flush dependency flush order */
        if (pass) {
            if (expected[i].flush_order >= 0 && entry_ptr->flush_order != (unsigned)expected[i].flush_order) {
                pass = false;
                snprintf(tmp_msg_buf, sizeof(tmp_msg_buf),
                         "%d entry (%d, %d) flush_order actual/expected = %u/%d.\n", tag,
                         expected[i].entry_type, expected[i].entry_index, entry_ptr->flush_order,
                         expected[i].flush_order);
                failure_mssg = tmp_msg_buf;
            } /* end if */
        }     /* end if */

        i++;
    } /* while */

} /* verify_entry_status() */

/*-------------------------------------------------------------------------
 * Function:    verify_unprotected
 *
 * Purpose:    Verify that no cache entries are marked as protected.  If
 *        any are, set pass to false.
 *
 *        Do nothing if pass is false on entry.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */

void
verify_unprotected(void)

{
    int           i;
    int           j;
    int           protected_count = 0;
    int32_t       max_index;
    test_entry_t *base_addr;

    if (pass) {

        for (i = 0; i < NUMBER_OF_ENTRY_TYPES; i++) {
            max_index = max_indices[i];
            base_addr = entries[i];

            assert(base_addr);

            for (j = 0; j <= max_index; j++) {
                assert(base_addr[j].header.is_protected == base_addr[j].is_protected);

                if ((base_addr[j].header.is_protected) || (base_addr[j].is_protected)) {

                    protected_count++;
                }
            }
        }

        if (protected_count > 0) {

            pass         = false;
            failure_mssg = "verify_unprotected() found protected entry(s).";
        }
    }

} /* verify_unprotected() */

/*-------------------------------------------------------------------------
 * Function:    expunge_entry()
 *
 * Purpose:    Expunge the entry indicated by the type and index.
 *
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */

void
expunge_entry(H5F_t *file_ptr, int32_t type, int32_t idx)
{
    herr_t        result;
    test_entry_t *base_addr;
    test_entry_t *entry_ptr;

    if (pass) {
#ifndef NDEBUG
        H5C_t *cache_ptr = file_ptr->shared->cache;

        assert(cache_ptr);
#endif /* NDEBUG */

        assert((0 <= type) && (type < NUMBER_OF_ENTRY_TYPES));
        assert((0 <= idx) && (idx <= max_indices[type]));

        base_addr = entries[type];
        entry_ptr = &(base_addr[idx]);

        assert(entry_ptr->index == idx);
        assert(entry_ptr->type == type);
        assert(entry_ptr == entry_ptr->self);
        assert(entry_ptr->cache_ptr == cache_ptr);
        assert(!(entry_ptr->header.is_protected));
        assert(!(entry_ptr->is_protected));
        assert(!(entry_ptr->header.is_pinned));
        assert(!(entry_ptr->is_pinned));

        result = H5C_expunge_entry(file_ptr, types[type], entry_ptr->addr, H5C__NO_FLAGS_SET);

        if (result < 0) {

            pass         = false;
            failure_mssg = "error in H5C_expunge_entry().";
        }
        else {

            entry_ptr->expunged = true;
        }
    }

} /* expunge_entry() */

/*-------------------------------------------------------------------------
 * Function:    flush_cache()
 *
 * Purpose:    Flush the specified cache, destroying all entries if
 *             requested.  If requested, dump stats first.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */

void
flush_cache(H5F_t *file_ptr, bool destroy_entries, bool dump_stats, bool dump_detailed_stats)
{
    bool verbose = false;

    verify_unprotected();

    if (pass) {
        H5C_t *cache_ptr;

        assert(file_ptr);

        cache_ptr = file_ptr->shared->cache;

        if (destroy_entries) {

            H5C_FLUSH_CACHE(file_ptr, H5C__FLUSH_INVALIDATE_FLAG, "error in H5C_flush_cache().")
        }
        else {

            H5C_FLUSH_CACHE(file_ptr, H5C__NO_FLAGS_SET, "error in H5C_flush_cache().")
        }

        if (dump_stats) {

            H5C_stats(cache_ptr, "test cache", dump_detailed_stats);
        }

        if ((pass) && (destroy_entries) &&
            ((cache_ptr->index_len != 0) || (cache_ptr->index_size != 0) ||
             (cache_ptr->clean_index_size != 0) || (cache_ptr->dirty_index_size != 0))) {

            if (verbose) {

                fprintf(stdout, "%s: unexpected il/is/cis/dis = %lld/%lld/%lld/%lld.\n", __func__,
                        (long long)(cache_ptr->index_len), (long long)(cache_ptr->index_size),
                        (long long)(cache_ptr->clean_index_size), (long long)(cache_ptr->dirty_index_size));
            }
            pass         = false;
            failure_mssg = "non zero index len/sizes after H5C_flush_cache() with invalidate.";
        }
    }

} /* flush_cache() */

/*-------------------------------------------------------------------------
 * Function:    cork_entry_type()
 *
 * Purpose:    To "cork" an object:
 *        --insert the base address of an entry type into
 *          the cache's list of corked object addresses
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */
void
cork_entry_type(H5F_t *file_ptr, int32_t type)
{
    if (pass) {
        H5C_t  *cache_ptr;
        haddr_t baddrs;

        cache_ptr = file_ptr->shared->cache;

        assert(cache_ptr);
        assert((0 <= type) && (type < NUMBER_OF_ENTRY_TYPES));

        baddrs = base_addrs[type];
        if (H5C_cork(cache_ptr, baddrs, H5C__SET_CORK, NULL) < 0) {
            pass         = false;
            failure_mssg = "error in H5C_cork().";
        } /* end if */
    }     /* end if */

} /* cork_entry_type() */

/*-------------------------------------------------------------------------
 * Function:    uncork_entry_type()
 *
 * Purpose:    To "uncork" an object:
 *        --insert the base address of an entry type into
 *          the cache's list of corked object addresses
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */
void
uncork_entry_type(H5F_t *file_ptr, int32_t type)
{
    if (pass) {
        H5C_t  *cache_ptr;
        haddr_t baddrs;

        cache_ptr = file_ptr->shared->cache;

        assert(cache_ptr);
        assert((0 <= type) && (type < NUMBER_OF_ENTRY_TYPES));

        baddrs = base_addrs[type];
        if (H5C_cork(cache_ptr, baddrs, H5C__UNCORK, NULL) < 0) {
            pass         = false;
            failure_mssg = "error in H5C_cork().";
        } /* end if */
    }     /* end if */

} /* uncork_entry_type() */

/*-------------------------------------------------------------------------
 * Function:    insert_entry()
 *
 * Purpose:    Insert the entry indicated by the type and index.
 *
 *        Do nothing if pass is false.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */

void
insert_entry(H5F_t *file_ptr, int32_t type, int32_t idx, unsigned int flags)
{
    H5C_t        *cache_ptr;
    herr_t        result;
    bool          insert_pinned;
    test_entry_t *base_addr;
    test_entry_t *entry_ptr;
    haddr_t       baddrs;

    if (pass) {

        cache_ptr = file_ptr->shared->cache;

        assert(cache_ptr);
        assert((0 <= type) && (type < NUMBER_OF_ENTRY_TYPES));
        assert((0 <= idx) && (idx <= max_indices[type]));

        base_addr = entries[type];
        entry_ptr = &(base_addr[idx]);
        baddrs    = base_addrs[type];

        assert(entry_ptr->index == idx);
        assert(entry_ptr->type == type);
        assert(entry_ptr == entry_ptr->self);
        assert(!(entry_ptr->is_protected));
        assert(entry_ptr->flush_dep_npar == 0);
        assert(entry_ptr->flush_dep_nchd == 0);

        insert_pinned = (bool)((flags & H5C__PIN_ENTRY_FLAG) != 0);

        entry_ptr->is_dirty = true;

        /* Set the base address of the entry type into the property list as tag */
        /* Use to cork entries for the object */
        H5AC_tag(baddrs, NULL);

        result = H5C_insert_entry(file_ptr, types[type], entry_ptr->addr, (void *)entry_ptr, flags);

        if ((result < 0) || (entry_ptr->header.is_protected) || (entry_ptr->header.type != types[type]) ||
            (entry_ptr->size != entry_ptr->header.size) || (entry_ptr->addr != entry_ptr->header.addr)) {

            pass         = false;
            failure_mssg = "error in H5C_insert().";

#if 0 /* This is useful debugging code.  Lets keep it around. */

            fprintf(stdout, "result = %d\n", (int)result);
            fprintf(stdout, "entry_ptr->header.is_protected = %d\n",
                      (int)(entry_ptr->header.is_protected));
            fprintf(stdout,
            "entry_ptr->header.type != types[type] = %d\n",
                      (int)(entry_ptr->header.type != types[type]));
            fprintf(stdout,
                      "entry_ptr->size != entry_ptr->header.size = %d\n",
                      (int)(entry_ptr->size != entry_ptr->header.size));
            fprintf(stdout,
                      "entry_ptr->addr != entry_ptr->header.addr = %d\n",
                       (int)(entry_ptr->addr != entry_ptr->header.addr));
#endif
        } /* end if */
        assert(entry_ptr->cache_ptr == NULL);

        entry_ptr->file_ptr  = file_ptr;
        entry_ptr->cache_ptr = cache_ptr;

        if (insert_pinned)
            assert(entry_ptr->header.is_pinned);
        else
            assert(!(entry_ptr->header.is_pinned));
        entry_ptr->is_pinned          = insert_pinned;
        entry_ptr->pinned_from_client = insert_pinned;

        if (entry_ptr->header.tag_info && entry_ptr->header.tag_info->corked)
            entry_ptr->is_corked = true;

        assert(entry_ptr->header.is_dirty);
        assert(((entry_ptr->header).type)->id == type);
    } /* end if */

} /* insert_entry() */

/*-------------------------------------------------------------------------
 * Function:    mark_entry_dirty()
 *
 * Purpose:    Mark the specified entry as dirty.
 *
 *        Do nothing if pass is false on entry.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */

void
mark_entry_dirty(int32_t type, int32_t idx)
{
    herr_t        result;
    test_entry_t *base_addr;
    test_entry_t *entry_ptr;
    bool          was_dirty;

    if (pass) {

        assert((0 <= type) && (type < NUMBER_OF_ENTRY_TYPES));
        assert((0 <= idx) && (idx <= max_indices[type]));

        base_addr = entries[type];
        entry_ptr = &(base_addr[idx]);

        assert(entry_ptr->index == idx);
        assert(entry_ptr->type == type);
        assert(entry_ptr == entry_ptr->self);
        assert(entry_ptr->header.is_protected || entry_ptr->header.is_pinned);

        was_dirty           = entry_ptr->is_dirty;
        entry_ptr->is_dirty = true;

        if (entry_ptr->flush_dep_npar > 0 && !was_dirty)
            mark_flush_dep_dirty(entry_ptr);

        result = H5C_mark_entry_dirty((void *)entry_ptr);

        if ((result < 0) || (!entry_ptr->header.is_protected && !entry_ptr->header.is_pinned) ||
            (entry_ptr->header.is_protected && !entry_ptr->header.dirtied) ||
            (!entry_ptr->header.is_protected && !entry_ptr->header.is_dirty) ||
            (entry_ptr->header.type != types[type]) || (entry_ptr->size != entry_ptr->header.size) ||
            (entry_ptr->addr != entry_ptr->header.addr)) {

            pass         = false;
            failure_mssg = "error in H5C_mark_entry_dirty().";
        }

        assert(((entry_ptr->header).type)->id == type);
    }

} /* mark_entry_dirty() */

/*-------------------------------------------------------------------------
 * Function:    move_entry()
 *
 * Purpose:    Move the entry indicated by the type and index to its
 *        main or alternate address as indicated.  If the entry is
 *        already at the desired entry, do nothing.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */

void
move_entry(H5C_t *cache_ptr, int32_t type, int32_t idx, bool main_addr)
{
    herr_t        result   = 0;
    bool          done     = true; /* will set to false if we have work to do */
    haddr_t       old_addr = HADDR_UNDEF;
    haddr_t       new_addr = HADDR_UNDEF;
    test_entry_t *base_addr;
    test_entry_t *entry_ptr;

    if (pass) {

        assert(cache_ptr);
        assert((0 <= type) && (type < NUMBER_OF_ENTRY_TYPES));
        assert((0 <= idx) && (idx <= max_indices[type]));

        base_addr = entries[type];
        entry_ptr = &(base_addr[idx]);

        assert(entry_ptr->index == idx);
        assert(entry_ptr->type == type);
        assert(entry_ptr == entry_ptr->self);
        assert(entry_ptr->cache_ptr == cache_ptr);
        assert(!entry_ptr->is_read_only);
        assert(!entry_ptr->header.is_read_only);

        if (entry_ptr->at_main_addr && !main_addr) {

            /* move to alt addr */

            assert(entry_ptr->addr == entry_ptr->main_addr);

            done     = false;
            old_addr = entry_ptr->addr;
            new_addr = entry_ptr->alt_addr;
        }
        else if (!(entry_ptr->at_main_addr) && main_addr) {

            /* move to main addr */

            assert(entry_ptr->addr == entry_ptr->alt_addr);

            done     = false;
            old_addr = entry_ptr->addr;
            new_addr = entry_ptr->main_addr;
        }

        if (!done) {
            bool was_dirty = entry_ptr->is_dirty;

            entry_ptr->is_dirty = true;

            if (entry_ptr->flush_dep_npar > 0 && !was_dirty)
                mark_flush_dep_dirty(entry_ptr);

            entry_ptr->action = TEST_ENTRY_ACTION_MOVE;
            result            = H5C_move_entry(cache_ptr, types[type], old_addr, new_addr);
            entry_ptr->action = TEST_ENTRY_ACTION_NUL;
        }

        if (!done) {

            if ((result < 0) ||
                ((!(entry_ptr->header.destroy_in_progress)) && (entry_ptr->header.addr != new_addr))) {

                pass         = false;
                failure_mssg = "error in H5C_move_entry().";
            }
            else {

                entry_ptr->addr         = new_addr;
                entry_ptr->at_main_addr = main_addr;
            }
        }

        assert(((entry_ptr->header).type)->id == type);

        assert(entry_ptr->header.is_dirty);
        assert(entry_ptr->is_dirty);
    }

} /* move_entry() */

/*-------------------------------------------------------------------------
 * Function:    protect_entry()
 *
 * Purpose:    Protect the entry indicated by the type and index.
 *
 *        Do nothing if pass is false on entry.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */
void
protect_entry(H5F_t *file_ptr, int32_t type, int32_t idx)
{
    H5C_t             *cache_ptr;
    test_entry_t      *base_addr;
    test_entry_t      *entry_ptr;
    haddr_t            baddrs;
    H5C_cache_entry_t *cache_entry_ptr;

    if (pass) {
        cache_ptr = file_ptr->shared->cache;

        assert(cache_ptr);
        assert((0 <= type) && (type < NUMBER_OF_ENTRY_TYPES));
        assert((0 <= idx) && (idx <= max_indices[type]));

        base_addr = entries[type];
        entry_ptr = &(base_addr[idx]);
        baddrs    = base_addrs[type];

        assert(entry_ptr->index == idx);
        assert(entry_ptr->type == type);
        assert(entry_ptr == entry_ptr->self);
        assert(!(entry_ptr->is_protected));

        /* Set the base address of the entry type into the property list as tag */
        /* Use to cork entries for the object */
        H5AC_tag(baddrs, NULL);

        cache_entry_ptr = (H5C_cache_entry_t *)H5C_protect(file_ptr, types[type], entry_ptr->addr,
                                                           &entry_ptr->addr, H5C__NO_FLAGS_SET);

        if ((cache_entry_ptr != (void *)entry_ptr) || (!(entry_ptr->header.is_protected)) ||
            (entry_ptr->header.type != types[type]) || (entry_ptr->size != entry_ptr->header.size) ||
            (entry_ptr->addr != entry_ptr->header.addr)) {

#if 0
            /* I've written the following debugging code several times
             * now.  Lets keep it around so I don't have to write it
             * again.
             *                              - JRM
             */
            fprintf(stdout, "( cache_entry_ptr != (void *)entry_ptr ) = %d\n",
                      (int)( cache_entry_ptr != (void *)entry_ptr ));
            fprintf(stdout, "cache_entry_ptr = 0x%lx, entry_ptr = 0x%lx\n",
                      (long)cache_entry_ptr, (long)entry_ptr);
            fprintf(stdout, "entry_ptr->header.is_protected = %d\n",
                      (int)(entry_ptr->header.is_protected));
            fprintf(stdout,
                      "( entry_ptr->header.type != types[type] ) = %d\n",
                      (int)( entry_ptr->header.type != types[type] ));
            fprintf(stdout,
                      "entry_ptr->size = %d, entry_ptr->header.size = %d\n",
                      (int)(entry_ptr->size), (int)(entry_ptr->header.size));
            fprintf(stdout,
                      "entry_ptr->addr = %d, entry_ptr->header.addr = %d\n",
                      (int)(entry_ptr->addr), (int)(entry_ptr->header.addr));
            fprintf(stdout,
                    "entry_ptr->verify_ct = %d, entry_ptr->max_verify_ct = %d\n",
                    entry_ptr->verify_ct, entry_ptr->max_verify_ct);
            H5Eprint2(H5E_DEFAULT, stdout);
#endif
            pass         = false;
            failure_mssg = "error in H5C_protect().";

        } /* end if */
        else {

            assert((entry_ptr->cache_ptr == NULL) || (entry_ptr->cache_ptr == cache_ptr));

            entry_ptr->cache_ptr    = cache_ptr;
            entry_ptr->file_ptr     = file_ptr;
            entry_ptr->is_protected = true;

        } /* end else */

        if (entry_ptr->header.tag_info && entry_ptr->header.tag_info->corked)
            entry_ptr->is_corked = true;

        assert(((entry_ptr->header).type)->id == type);
    } /* end if */

} /* protect_entry() */

/*-------------------------------------------------------------------------
 * Function:    protect_entry_ro()
 *
 * Purpose:    Do a read only protect the entry indicated by the type
 *         and index.
 *
 *        Do nothing if pass is false on entry.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */

void
protect_entry_ro(H5F_t *file_ptr, int32_t type, int32_t idx)
{
    H5C_t             *cache_ptr;
    test_entry_t      *base_addr;
    test_entry_t      *entry_ptr;
    H5C_cache_entry_t *cache_entry_ptr;

    if (pass) {

        cache_ptr = file_ptr->shared->cache;

        assert(cache_ptr);
        assert((0 <= type) && (type < NUMBER_OF_ENTRY_TYPES));
        assert((0 <= idx) && (idx <= max_indices[type]));

        base_addr = entries[type];
        entry_ptr = &(base_addr[idx]);

        assert(entry_ptr->index == idx);
        assert(entry_ptr->type == type);
        assert(entry_ptr == entry_ptr->self);
        assert((!(entry_ptr->is_protected)) || ((entry_ptr->is_read_only) && (entry_ptr->ro_ref_count > 0)));

        cache_entry_ptr = (H5C_cache_entry_t *)H5C_protect(file_ptr, types[type], entry_ptr->addr,
                                                           &entry_ptr->addr, H5C__READ_ONLY_FLAG);

        if ((cache_entry_ptr != (void *)entry_ptr) || (!(entry_ptr->header.is_protected)) ||
            (!(entry_ptr->header.is_read_only)) || (entry_ptr->header.ro_ref_count <= 0) ||
            (entry_ptr->header.type != types[type]) || (entry_ptr->size != entry_ptr->header.size) ||
            (entry_ptr->addr != entry_ptr->header.addr)) {

            pass         = false;
            failure_mssg = "error in read only H5C_protect().";
        }
        else {

            assert((entry_ptr->cache_ptr == NULL) || (entry_ptr->cache_ptr == cache_ptr));

            entry_ptr->cache_ptr    = cache_ptr;
            entry_ptr->file_ptr     = file_ptr;
            entry_ptr->is_protected = true;
            entry_ptr->is_read_only = true;
            entry_ptr->ro_ref_count++;
        }

        assert(((entry_ptr->header).type)->id == type);
    }

} /* protect_entry_ro() */

/*-------------------------------------------------------------------------
 * Function:    pin_entry()
 *
 * Purpose:    Pin the entry indicated by the type and index.
 *
 *        Do nothing if pass is false on entry.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */

void
pin_entry(int32_t type, int32_t idx)
{
    assert((0 <= type) && (type < NUMBER_OF_ENTRY_TYPES));
    assert((0 <= idx) && (idx <= max_indices[type]));

    if (pass) {
        test_entry_t *base_addr;
        test_entry_t *entry_ptr;
        herr_t        result;

        base_addr = entries[type];
        entry_ptr = &(base_addr[idx]);

        assert(entry_ptr->index == idx);
        assert(entry_ptr->type == type);
        assert(entry_ptr == entry_ptr->self);
        assert(entry_ptr->is_protected);
        assert(!(entry_ptr->pinned_from_client));

        result = H5C_pin_protected_entry((void *)entry_ptr);

        if (result < 0) {

            pass         = false;
            failure_mssg = "H5C_pin_protected_entry() reports failure.";
        }
        else if (!(entry_ptr->header.is_pinned)) {

            pass         = false;
            failure_mssg = "entry not pinned when it should be.";
        }
        else {

            entry_ptr->pinned_from_client = true;
            entry_ptr->is_pinned          = true;
        }
    } /* end if */

} /* pin_entry() */

/*-------------------------------------------------------------------------
 * Function:    unpin_entry()
 *
 * Purpose:    Unpin the entry indicated by the type and index.
 *
 *        Do nothing if pass is false on entry.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */

void
unpin_entry(int32_t type, int32_t idx)
{
    herr_t        result;
    test_entry_t *base_addr;
    test_entry_t *entry_ptr;

    if (pass) {
        assert((0 <= type) && (type < NUMBER_OF_ENTRY_TYPES));
        assert((0 <= idx) && (idx <= max_indices[type]));

        base_addr = entries[type];
        entry_ptr = &(base_addr[idx]);

        assert(entry_ptr->index == idx);
        assert(entry_ptr->type == type);
        assert(entry_ptr == entry_ptr->self);
        assert(entry_ptr->header.is_pinned);
        assert(entry_ptr->header.pinned_from_client);
        assert(entry_ptr->is_pinned);
        assert(entry_ptr->pinned_from_client);

        result = H5C_unpin_entry(entry_ptr);

        if ((result < 0) || (entry_ptr->header.pinned_from_client) ||
            (entry_ptr->header.is_pinned && !entry_ptr->header.pinned_from_cache) ||
            (entry_ptr->header.type != types[type]) || (entry_ptr->size != entry_ptr->header.size) ||
            (entry_ptr->addr != entry_ptr->header.addr)) {

            pass         = false;
            failure_mssg = "error in H5C_unpin().";
        }

        entry_ptr->pinned_from_client = false;

        entry_ptr->is_pinned = entry_ptr->pinned_from_cache;

        assert(((entry_ptr->header).type)->id == type);
    }

} /* unpin_entry() */

/*-------------------------------------------------------------------------
 * Function:    unprotect_entry()
 *
 * Purpose:    Unprotect the entry indicated by the type and index.
 *
 *        Do nothing if pass is false on entry.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */

void
unprotect_entry(H5F_t *file_ptr, int32_t type, int32_t idx, unsigned int flags)
{
    herr_t        result;
    bool          pin_flag_set;
    bool          unpin_flag_set;
    test_entry_t *base_addr;
    test_entry_t *entry_ptr;

    if (pass) {
        assert((0 <= type) && (type < NUMBER_OF_ENTRY_TYPES));
        assert((0 <= idx) && (idx <= max_indices[type]));

        base_addr = entries[type];
        entry_ptr = &(base_addr[idx]);

        assert(entry_ptr->index == idx);
        assert(entry_ptr->type == type);
        assert(entry_ptr == entry_ptr->self);
        assert(entry_ptr->header.is_protected);
        assert(entry_ptr->is_protected);

        pin_flag_set   = (bool)((flags & H5C__PIN_ENTRY_FLAG) != 0);
        unpin_flag_set = (bool)((flags & H5C__UNPIN_ENTRY_FLAG) != 0);

        assert(!(pin_flag_set && unpin_flag_set));
        assert((!pin_flag_set) || (!(entry_ptr->is_pinned)));
        assert((!unpin_flag_set) || (entry_ptr->is_pinned));

        if (flags & H5C__DIRTIED_FLAG) {
            bool was_dirty = entry_ptr->is_dirty;

            entry_ptr->is_dirty = true;

            if (entry_ptr->flush_dep_npar > 0 && !was_dirty)
                mark_flush_dep_dirty(entry_ptr);
        } /* end if */

        result = H5C_unprotect(file_ptr, entry_ptr->addr, (void *)entry_ptr, flags);

        if ((result < 0) ||
            ((entry_ptr->header.is_protected) &&
             ((!(entry_ptr->is_read_only)) || (entry_ptr->ro_ref_count <= 0))) ||
            (entry_ptr->header.type != types[type]) || (entry_ptr->size != entry_ptr->header.size) ||
            (entry_ptr->addr != entry_ptr->header.addr)) {

            pass         = false;
            failure_mssg = "error in H5C_unprotect().";
        }
        else {
            if (entry_ptr->ro_ref_count > 1) {

                entry_ptr->ro_ref_count--;
            }
            else if (entry_ptr->ro_ref_count == 1) {

                entry_ptr->is_protected = false;
                entry_ptr->is_read_only = false;
                entry_ptr->ro_ref_count = 0;
            }
            else {

                entry_ptr->is_protected = false;
            }

            if (pin_flag_set) {

                assert(entry_ptr->header.is_pinned);
                entry_ptr->pinned_from_client = true;
                entry_ptr->is_pinned          = true;
            }
            else if (unpin_flag_set) {

                assert(entry_ptr->header.is_pinned == entry_ptr->header.pinned_from_cache);
                entry_ptr->pinned_from_client = false;
                entry_ptr->is_pinned          = entry_ptr->pinned_from_cache;
            }
        }

        assert(((entry_ptr->header).type)->id == type);

        if ((flags & H5C__DIRTIED_FLAG) != 0 && ((flags & H5C__DELETED_FLAG) == 0)) {

            assert(entry_ptr->header.is_dirty);
            assert(entry_ptr->is_dirty);
        }

        assert(entry_ptr->header.is_protected == entry_ptr->is_protected);
        assert(entry_ptr->header.is_read_only == entry_ptr->is_read_only);
        assert(entry_ptr->header.ro_ref_count == entry_ptr->ro_ref_count);
    }

} /* unprotect_entry() */

/*-------------------------------------------------------------------------
 * Function:    row_major_scan_forward()
 *
 * Purpose:    Do a sequence of inserts, protects, unprotects, moves,
 *             destroys while scanning through the set of entries.  If
 *             pass is false on entry, do nothing.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */
void
row_major_scan_forward(H5F_t *file_ptr, int32_t max_index, int32_t lag, bool verbose, bool reset_stats,
                       bool display_stats, bool display_detailed_stats, bool do_inserts, bool do_moves,
                       bool move_to_main_addr, bool do_destroys, bool do_mult_ro_protects, int dirty_destroys,
                       int dirty_unprotects)
{
    H5C_t  *cache_ptr = NULL;
    int32_t type      = 0;
    int32_t idx;
    int32_t local_max_index;

    if (verbose)
        fprintf(stdout, "%s(): entering.\n", __func__);

    if (pass) {
        cache_ptr = file_ptr->shared->cache;
        assert(cache_ptr != NULL);
        assert(lag >= 10);

        if (reset_stats)
            H5C_stats__reset(cache_ptr);
    } /* end if */

    while (pass && type < NUMBER_OF_ENTRY_TYPES) {
        idx = -lag;

        local_max_index = MIN(max_index, max_indices[type]);
        while (pass && idx <= (local_max_index + lag)) {
            int32_t tmp_idx;

            if (verbose)
                fprintf(stdout, "%d:%d: ", type, idx);

            tmp_idx = idx + lag;
            if (pass && do_inserts && (tmp_idx >= 0) && (tmp_idx <= local_max_index) &&
                ((tmp_idx % 2) == 0) && !entry_in_cache(cache_ptr, type, tmp_idx)) {

                if (verbose)
                    fprintf(stdout, "1(i, %d, %d) ", type, tmp_idx);

                insert_entry(file_ptr, type, tmp_idx, H5C__NO_FLAGS_SET);

                assert((!cache_ptr->slist_enabled) || (cache_ptr->slist_size == cache_ptr->dirty_index_size));
            } /* end if */

            tmp_idx--;
            if (pass && (tmp_idx >= 0) && (tmp_idx <= local_max_index) && (tmp_idx % 3) == 0) {

                if (verbose)
                    fprintf(stdout, "2(p, %d, %d) ", type, tmp_idx);

                protect_entry(file_ptr, type, tmp_idx);

                assert((!cache_ptr->slist_enabled) || (cache_ptr->slist_size == cache_ptr->dirty_index_size));
            } /* end if */

            tmp_idx--;
            if (pass && (tmp_idx >= 0) && (tmp_idx <= local_max_index) && (tmp_idx % 3) == 0) {

                if (verbose)
                    fprintf(stdout, "3(u, %d, %d) ", type, tmp_idx);

                unprotect_entry(file_ptr, type, tmp_idx, H5C__NO_FLAGS_SET);

                assert((!cache_ptr->slist_enabled) || (cache_ptr->slist_size == cache_ptr->dirty_index_size));
            } /* end if */

            /* (don't decrement tmp_idx) */
            if (pass && do_moves && (tmp_idx >= 0) && (tmp_idx <= local_max_index) && (tmp_idx % 3) == 0) {

                if (verbose)
                    fprintf(stdout, "4(r, %d, %d, %d) ", type, tmp_idx, (int)move_to_main_addr);

                move_entry(cache_ptr, type, tmp_idx, move_to_main_addr);

                assert((!cache_ptr->slist_enabled) || (cache_ptr->slist_size == cache_ptr->dirty_index_size));
            } /* end if */

            tmp_idx--;
            if (pass && (tmp_idx >= 0) && (tmp_idx <= local_max_index) && (tmp_idx % 5) == 0) {

                if (verbose)
                    fprintf(stdout, "5(p, %d, %d) ", type, tmp_idx);

                protect_entry(file_ptr, type, tmp_idx);

                assert((!cache_ptr->slist_enabled) || (cache_ptr->slist_size == cache_ptr->dirty_index_size));
            } /* end if */

            tmp_idx -= 2;
            if (pass && (tmp_idx >= 0) && (tmp_idx <= local_max_index) && (tmp_idx % 5) == 0) {

                if (verbose)
                    fprintf(stdout, "6(u, %d, %d) ", type, tmp_idx);

                unprotect_entry(file_ptr, type, tmp_idx, H5C__NO_FLAGS_SET);

                assert((!cache_ptr->slist_enabled) || (cache_ptr->slist_size == cache_ptr->dirty_index_size));
            } /* end if */

            if (do_mult_ro_protects) {
                /* (don't decrement tmp_idx) */
                if (pass && (tmp_idx >= 0) && (tmp_idx < local_max_index) && (tmp_idx % 9) == 0) {

                    if (verbose)
                        fprintf(stdout, "7(p-ro, %d, %d) ", type, tmp_idx);

                    protect_entry_ro(file_ptr, type, tmp_idx);

                    assert((!cache_ptr->slist_enabled) ||
                           (cache_ptr->slist_size == cache_ptr->dirty_index_size));
                } /* end if */

                tmp_idx--;
                if (pass && (tmp_idx >= 0) && (tmp_idx < local_max_index) && (tmp_idx % 11) == 0) {

                    if (verbose)
                        fprintf(stdout, "8(p-ro, %d, %d) ", type, tmp_idx);

                    protect_entry_ro(file_ptr, type, tmp_idx);

                    assert((!cache_ptr->slist_enabled) ||
                           (cache_ptr->slist_size == cache_ptr->dirty_index_size));
                } /* end if */

                tmp_idx--;
                if (pass && (tmp_idx >= 0) && (tmp_idx < local_max_index) && (tmp_idx % 13) == 0) {

                    if (verbose)
                        fprintf(stdout, "9(p-ro, %d, %d) ", type, tmp_idx);

                    protect_entry_ro(file_ptr, type, tmp_idx);

                    assert((!cache_ptr->slist_enabled) ||
                           (cache_ptr->slist_size == cache_ptr->dirty_index_size));
                } /* end if */

                /* (don't decrement tmp_idx) */
                if (pass && (tmp_idx >= 0) && (tmp_idx < local_max_index) && (tmp_idx % 9) == 0) {

                    if (verbose)
                        fprintf(stdout, "10(u-ro, %d, %d) ", type, tmp_idx);

                    unprotect_entry(file_ptr, type, tmp_idx, H5C__NO_FLAGS_SET);

                    assert((!cache_ptr->slist_enabled) ||
                           (cache_ptr->slist_size == cache_ptr->dirty_index_size));
                } /* end if */

                tmp_idx--;
                if (pass && (tmp_idx >= 0) && (tmp_idx < local_max_index) && (tmp_idx % 11) == 0) {

                    if (verbose)
                        fprintf(stdout, "11(u-ro, %d, %d) ", type, tmp_idx);

                    unprotect_entry(file_ptr, type, tmp_idx, H5C__NO_FLAGS_SET);

                    assert((!cache_ptr->slist_enabled) ||
                           (cache_ptr->slist_size == cache_ptr->dirty_index_size));
                } /* end if */

                tmp_idx--;
                if (pass && (tmp_idx >= 0) && (tmp_idx < local_max_index) && (tmp_idx % 13) == 0) {

                    if (verbose)
                        fprintf(stdout, "12(u-ro, %d, %d) ", type, tmp_idx);

                    unprotect_entry(file_ptr, type, tmp_idx, H5C__NO_FLAGS_SET);

                    assert((!cache_ptr->slist_enabled) ||
                           (cache_ptr->slist_size == cache_ptr->dirty_index_size));
                } /* end if */
            }     /* if ( do_mult_ro_protects ) */

            if (pass && (idx >= 0) && (idx <= local_max_index)) {
                if (verbose)
                    fprintf(stdout, "13(p, %d, %d) ", type, idx);

                protect_entry(file_ptr, type, idx);

                assert((!cache_ptr->slist_enabled) || (cache_ptr->slist_size == cache_ptr->dirty_index_size));
            } /* end if */

            tmp_idx = idx - lag + 2;
            if (pass && (tmp_idx >= 0) && (tmp_idx <= local_max_index) && (tmp_idx % 7) == 0) {

                if (verbose)
                    fprintf(stdout, "14(u, %d, %d) ", type, tmp_idx);

                unprotect_entry(file_ptr, type, tmp_idx, H5C__NO_FLAGS_SET);

                assert((!cache_ptr->slist_enabled) || (cache_ptr->slist_size == cache_ptr->dirty_index_size));
            } /* end if */

            tmp_idx--;
            if (pass && (tmp_idx >= 0) && (tmp_idx <= local_max_index) && (tmp_idx % 7) == 0) {

                if (verbose)
                    fprintf(stdout, "15(p, %d, %d) ", type, tmp_idx);

                protect_entry(file_ptr, type, tmp_idx);

                assert((!cache_ptr->slist_enabled) || (cache_ptr->slist_size == cache_ptr->dirty_index_size));
            } /* end if */

            if (do_destroys) {
                tmp_idx = idx - lag;
                if (pass && (tmp_idx >= 0) && (tmp_idx <= local_max_index)) {
                    switch (tmp_idx % 4) {
                        case 0: /* we just did an insert */
                            if (verbose)
                                fprintf(stdout, "16(u, %d, %d) ", type, tmp_idx);

                            unprotect_entry(file_ptr, type, tmp_idx, H5C__NO_FLAGS_SET);

                            assert((!cache_ptr->slist_enabled) ||
                                   (cache_ptr->slist_size == cache_ptr->dirty_index_size));
                            break;

                        case 1:
                            if ((entries[type])[tmp_idx].is_dirty) {
                                if (verbose)
                                    fprintf(stdout, "17(u, %d, %d) ", type, tmp_idx);

                                unprotect_entry(file_ptr, type, tmp_idx, H5C__NO_FLAGS_SET);

                                assert((!cache_ptr->slist_enabled) ||
                                       (cache_ptr->slist_size == cache_ptr->dirty_index_size));
                            } /* end if */
                            else {
                                if (verbose)
                                    fprintf(stdout, "18(u, %d, %d) ", type, tmp_idx);

                                unprotect_entry(file_ptr, type, tmp_idx,
                                                (dirty_unprotects ? H5C__DIRTIED_FLAG : H5C__NO_FLAGS_SET));

                                assert((!cache_ptr->slist_enabled) ||
                                       (cache_ptr->slist_size == cache_ptr->dirty_index_size));
                            } /* end else */
                            break;

                        case 2: /* we just did an insert */
                            if (verbose)
                                fprintf(stdout, "19(u-del, %d, %d) ", type, tmp_idx);

                            unprotect_entry(file_ptr, type, tmp_idx, H5C__DELETED_FLAG);

                            assert((!cache_ptr->slist_enabled) ||
                                   (cache_ptr->slist_size == cache_ptr->dirty_index_size));
                            break;

                        case 3:
                            if ((entries[type])[tmp_idx].is_dirty) {
                                if (verbose)
                                    fprintf(stdout, "20(u-del, %d, %d) ", type, tmp_idx);

                                unprotect_entry(file_ptr, type, tmp_idx, H5C__DELETED_FLAG);

                                assert((!cache_ptr->slist_enabled) ||
                                       (cache_ptr->slist_size == cache_ptr->dirty_index_size));
                            } /* end if */
                            else {
                                if (verbose)
                                    fprintf(stdout, "21(u-del, %d, %d) ", type, tmp_idx);

                                unprotect_entry(file_ptr, type, tmp_idx,
                                                (dirty_destroys ? H5C__DIRTIED_FLAG : H5C__NO_FLAGS_SET) |
                                                    H5C__DELETED_FLAG);

                                assert((!cache_ptr->slist_enabled) ||
                                       (cache_ptr->slist_size == cache_ptr->dirty_index_size));
                            } /* end else */
                            break;

                        default:
                            assert(0); /* this can't happen... */
                            break;
                    } /* end switch */
                }     /* end if */
            }         /* end if */
            else {
                tmp_idx = idx - lag;
                if (pass && (tmp_idx >= 0) && (tmp_idx <= local_max_index)) {
                    if (verbose)
                        fprintf(stdout, "22(u, %d, %d) ", type, tmp_idx);

                    unprotect_entry(file_ptr, type, tmp_idx,
                                    (dirty_unprotects ? H5C__DIRTIED_FLAG : H5C__NO_FLAGS_SET));

                    assert((!cache_ptr->slist_enabled) ||
                           (cache_ptr->slist_size == cache_ptr->dirty_index_size));
                } /* end if */
            }     /* end elseif */

            if (verbose)
                fprintf(stdout, "\n");

            idx++;
        } /* end while */

        type++;
    } /* end while */

    if (pass && display_stats)
        H5C_stats(cache_ptr, "test cache", display_detailed_stats);

} /* row_major_scan_forward() */

/*-------------------------------------------------------------------------
 * Function:    hl_row_major_scan_forward()
 *
 * Purpose:    Do a high locality sequence of inserts, protects, and
 *        unprotects while scanning through the set of entries.
 *        If pass is false on entry, do nothing.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */

void
hl_row_major_scan_forward(H5F_t *file_ptr, int32_t max_index, bool verbose, bool reset_stats,
                          bool display_stats, bool display_detailed_stats, bool do_inserts)
{
    H5C_t  *cache_ptr = NULL;
    int32_t type      = 0;
    int32_t idx;
    int32_t i;
    int32_t lag = 100;
    int32_t local_max_index;

    if (verbose)
        fprintf(stdout, "%s(): entering.\n", __func__);

    if (pass) {

        cache_ptr = file_ptr->shared->cache;

        assert(cache_ptr != NULL);
        assert(lag > 5);
        assert(max_index >= 200);
        assert(max_index <= MAX_ENTRIES);

        if (reset_stats) {

            H5C_stats__reset(cache_ptr);
        }
    }

    while ((pass) && (type < NUMBER_OF_ENTRY_TYPES)) {
        idx = -lag;

        local_max_index = MIN(max_index, max_indices[type]);

        while ((pass) && (idx <= (local_max_index + lag))) {
            if ((pass) && (do_inserts) && ((idx + lag) >= 0) && ((idx + lag) <= max_indices[type]) &&
                (((idx + lag) % 2) == 0) && (!entry_in_cache(cache_ptr, type, (idx + lag)))) {

                if (verbose)
                    fprintf(stdout, "(i, %d, %d) ", type, (idx + lag));

                insert_entry(file_ptr, type, (idx + lag), H5C__NO_FLAGS_SET);
            }

            i = idx;

            while ((pass) && (i >= idx - lag) && (i >= 0)) {
                if ((pass) && (i >= 0) && (i <= local_max_index)) {

                    if (verbose)
                        fprintf(stdout, "(p, %d, %d) ", type, i);

                    protect_entry(file_ptr, type, i);

                    if (verbose)
                        fprintf(stdout, "(u, %d, %d) ", type, i);

                    unprotect_entry(file_ptr, type, i, H5C__NO_FLAGS_SET);
                }
                i--;
            }

            if (verbose)
                fprintf(stdout, "\n");

            idx++;
        }
        type++;
    }

    if ((pass) && (display_stats)) {

        H5C_stats(cache_ptr, "test cache", display_detailed_stats);
    }

} /* hl_row_major_scan_forward() */

/*-------------------------------------------------------------------------
 * Function:    row_major_scan_backward()
 *
 * Purpose:    Do a sequence of inserts, protects, unprotects, moves,
 *        destroys while scanning backwards through the set of
 *        entries.  If pass is false on entry, do nothing.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */

void
row_major_scan_backward(H5F_t *file_ptr, int32_t max_index, int32_t lag, bool verbose, bool reset_stats,
                        bool display_stats, bool display_detailed_stats, bool do_inserts, bool do_moves,
                        bool move_to_main_addr, bool do_destroys, bool do_mult_ro_protects,
                        int dirty_destroys, int dirty_unprotects)
{
    H5C_t  *cache_ptr = NULL;
    int32_t type      = NUMBER_OF_ENTRY_TYPES - 1;
    int32_t idx;
    int32_t local_max_index;

    if (verbose)
        fprintf(stdout, "%s(): Entering.\n", __func__);

    if (pass) {

        cache_ptr = file_ptr->shared->cache;

        assert(cache_ptr != NULL);
        assert(lag >= 10);

        if (reset_stats) {

            H5C_stats__reset(cache_ptr);
        }
    }

    while ((pass) && (type >= 0)) {
        local_max_index = MIN(max_index, max_indices[type]);

        idx = local_max_index + lag;

        while ((pass) && (idx >= -lag)) {
            int32_t tmp_idx;

            tmp_idx = idx - lag;
            if ((pass) && (do_inserts) && (tmp_idx >= 0) && (tmp_idx <= local_max_index) &&
                ((tmp_idx % 2) == 1) && (!entry_in_cache(cache_ptr, type, tmp_idx))) {

                if (verbose)
                    fprintf(stdout, "(i, %d, %d) ", type, tmp_idx);

                insert_entry(file_ptr, type, tmp_idx, H5C__NO_FLAGS_SET);
            }

            tmp_idx++;
            if ((pass) && (tmp_idx >= 0) && (tmp_idx <= local_max_index) && ((tmp_idx % 3) == 0)) {

                if (verbose)
                    fprintf(stdout, "(p, %d, %d) ", type, tmp_idx);

                protect_entry(file_ptr, type, tmp_idx);
            }

            tmp_idx++;
            if ((pass) && (tmp_idx >= 0) && (tmp_idx <= local_max_index) && ((tmp_idx % 3) == 0)) {

                if (verbose)
                    fprintf(stdout, "(u, %d, %d) ", type, tmp_idx);

                unprotect_entry(file_ptr, type, tmp_idx, H5C__NO_FLAGS_SET);
            }

            /* (don't increment tmp_idx) */
            if ((pass) && (do_moves) && (tmp_idx >= 0) && (tmp_idx <= local_max_index) &&
                ((tmp_idx % 3) == 0)) {

                if (verbose)
                    fprintf(stdout, "(r, %d, %d, %d) ", type, tmp_idx, (int)move_to_main_addr);

                move_entry(cache_ptr, type, tmp_idx, move_to_main_addr);
            }

            tmp_idx++;
            if ((pass) && (tmp_idx >= 0) && (tmp_idx <= local_max_index) && ((tmp_idx % 5) == 0)) {

                if (verbose)
                    fprintf(stdout, "(p, %d, %d) ", type, tmp_idx);

                protect_entry(file_ptr, type, (idx - lag + 3));
            }

            tmp_idx += 2;
            if ((pass) && (tmp_idx >= 0) && (tmp_idx <= local_max_index) && ((tmp_idx % 5) == 0)) {

                if (verbose)
                    fprintf(stdout, "(u, %d, %d) ", type, tmp_idx);

                unprotect_entry(file_ptr, type, tmp_idx, H5C__NO_FLAGS_SET);
            }

            /* (don't increment tmp_idx) */
            if (do_mult_ro_protects) {
                if ((pass) && (tmp_idx >= 0) && (tmp_idx < local_max_index) && (tmp_idx % 9 == 0)) {

                    if (verbose)
                        fprintf(stdout, "(p-ro, %d, %d) ", type, tmp_idx);

                    protect_entry_ro(file_ptr, type, tmp_idx);
                }

                tmp_idx++;
                if ((pass) && (tmp_idx >= 0) && (tmp_idx < local_max_index) && (tmp_idx % 11 == 0)) {

                    if (verbose)
                        fprintf(stdout, "(p-ro, %d, %d) ", type, tmp_idx);

                    protect_entry_ro(file_ptr, type, tmp_idx);
                }

                tmp_idx++;
                if ((pass) && (tmp_idx >= 0) && (tmp_idx < local_max_index) && (tmp_idx % 13 == 0)) {

                    if (verbose)
                        fprintf(stdout, "(p-ro, %d, %d) ", type, tmp_idx);

                    protect_entry_ro(file_ptr, type, tmp_idx);
                }

                /* (don't increment tmp_idx) */
                if ((pass) && (tmp_idx >= 0) && (tmp_idx < local_max_index) && (tmp_idx % 9 == 0)) {

                    if (verbose)
                        fprintf(stdout, "(u-ro, %d, %d) ", type, tmp_idx);

                    unprotect_entry(file_ptr, type, tmp_idx, H5C__NO_FLAGS_SET);
                }

                tmp_idx++;
                if ((pass) && (tmp_idx >= 0) && (tmp_idx < local_max_index) && (tmp_idx % 11 == 0)) {

                    if (verbose)
                        fprintf(stdout, "(u-ro, %d, %d) ", type, tmp_idx);

                    unprotect_entry(file_ptr, type, tmp_idx, H5C__NO_FLAGS_SET);
                }

                tmp_idx++;
                if ((pass) && (tmp_idx >= 0) && (tmp_idx < local_max_index) && (tmp_idx % 13 == 0)) {

                    if (verbose)
                        fprintf(stdout, "(u-ro, %d, %d) ", type, tmp_idx);

                    unprotect_entry(file_ptr, type, tmp_idx, H5C__NO_FLAGS_SET);
                }
            } /* if ( do_mult_ro_protects ) */

            if ((pass) && (idx >= 0) && (idx <= local_max_index)) {

                if (verbose)
                    fprintf(stdout, "(p, %d, %d) ", type, idx);

                protect_entry(file_ptr, type, idx);
            }

            tmp_idx = idx + lag - 2;
            if ((pass) && (tmp_idx >= 0) && (tmp_idx <= local_max_index) && ((tmp_idx % 7) == 0)) {

                if (verbose)
                    fprintf(stdout, "(u, %d, %d) ", type, tmp_idx);

                unprotect_entry(file_ptr, type, tmp_idx, H5C__NO_FLAGS_SET);
            }

            tmp_idx++;
            if ((pass) && (tmp_idx >= 0) && (tmp_idx <= local_max_index) && ((tmp_idx % 7) == 0)) {

                if (verbose)
                    fprintf(stdout, "(p, %d, %d) ", type, tmp_idx);

                protect_entry(file_ptr, type, tmp_idx);
            }

            if (do_destroys) {

                if ((pass) && ((idx + lag) >= 0) && ((idx + lag) <= local_max_index)) {

                    switch ((idx + lag) % 4) {

                        case 0:
                            if ((entries[type])[idx + lag].is_dirty) {

                                unprotect_entry(file_ptr, type, idx + lag, H5C__NO_FLAGS_SET);
                            }
                            else {

                                unprotect_entry(file_ptr, type, idx + lag,
                                                (dirty_unprotects ? H5C__DIRTIED_FLAG : H5C__NO_FLAGS_SET));
                            }
                            break;

                        case 1: /* we just did an insert */
                            unprotect_entry(file_ptr, type, idx + lag, H5C__NO_FLAGS_SET);
                            break;

                        case 2:
                            if ((entries[type])[idx + lag].is_dirty) {

                                unprotect_entry(file_ptr, type, idx + lag, H5C__DELETED_FLAG);
                            }
                            else {

                                unprotect_entry(file_ptr, type, idx + lag,
                                                (dirty_destroys ? H5C__DIRTIED_FLAG : H5C__NO_FLAGS_SET) |
                                                    H5C__DELETED_FLAG);
                            }
                            break;

                        case 3: /* we just did an insert */
                            unprotect_entry(file_ptr, type, idx + lag, H5C__DELETED_FLAG);
                            break;

                        default:
                            assert(0); /* this can't happen... */
                            break;
                    }
                }
            }
            else {

                if ((pass) && ((idx + lag) >= 0) && ((idx + lag) <= local_max_index)) {

                    if (verbose)
                        fprintf(stdout, "(u, %d, %d) ", type, (idx + lag));

                    unprotect_entry(file_ptr, type, idx + lag,
                                    (dirty_unprotects ? H5C__DIRTIED_FLAG : H5C__NO_FLAGS_SET));
                }
            }

            if (verbose)
                fprintf(stdout, "\n");

            idx--;
        }
        type--;
    }

    if ((pass) && (display_stats)) {

        H5C_stats(cache_ptr, "test cache", display_detailed_stats);
    }

} /* row_major_scan_backward() */

/*-------------------------------------------------------------------------
 * Function:    hl_row_major_scan_backward()
 *
 * Purpose:    Do a high locality sequence of inserts, protects, and
 *        unprotects while scanning through the set of entries.
 *        If pass is false on entry, do nothing.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */

void
hl_row_major_scan_backward(H5F_t *file_ptr, int32_t max_index, bool verbose, bool reset_stats,
                           bool display_stats, bool display_detailed_stats, bool do_inserts)
{
    H5C_t  *cache_ptr = NULL;
    int32_t type      = NUMBER_OF_ENTRY_TYPES - 1;
    int32_t idx;
    int32_t i;
    int32_t lag = 100;
    int32_t local_max_index;

    if (verbose)
        fprintf(stdout, "%s(): entering.\n", __func__);

    if (pass) {

        cache_ptr = file_ptr->shared->cache;

        assert(cache_ptr != NULL);
        assert(lag > 5);
        assert(max_index >= 200);
        assert(max_index <= MAX_ENTRIES);

        if (reset_stats) {

            H5C_stats__reset(cache_ptr);
        }
    }

    while ((pass) && (type >= 0)) {
        idx = max_indices[type] + lag;

        local_max_index = MIN(max_index, max_indices[type]);

        while ((pass) && (idx >= -lag)) {
            if ((pass) && (do_inserts) && ((idx + lag) >= 0) && ((idx + lag) <= local_max_index) &&
                (((idx + lag) % 2) == 0) && (!entry_in_cache(cache_ptr, type, (idx + lag)))) {

                if (verbose)
                    fprintf(stdout, "(i, %d, %d) ", type, (idx + lag));

                insert_entry(file_ptr, type, (idx + lag), H5C__NO_FLAGS_SET);
            }

            i = idx;

            while ((pass) && (i >= idx - lag) && (i >= 0)) {
                if ((pass) && (i >= 0) && (i <= local_max_index)) {

                    if (verbose)
                        fprintf(stdout, "(p, %d, %d) ", type, i);

                    protect_entry(file_ptr, type, i);

                    if (verbose)
                        fprintf(stdout, "(u, %d, %d) ", type, i);

                    unprotect_entry(file_ptr, type, i, H5C__NO_FLAGS_SET);
                }
                i--;
            }

            if (verbose)
                fprintf(stdout, "\n");

            idx--;
        }
        type--;
    }

    if ((pass) && (display_stats)) {

        H5C_stats(cache_ptr, "test cache", display_detailed_stats);
    }

} /* hl_row_major_scan_backward() */

/*-------------------------------------------------------------------------
 * Function:    col_major_scan_forward()
 *
 * Purpose:    Do a sequence of inserts, protects, and unprotects
 *        while scanning through the set of entries.  If
 *        pass is false on entry, do nothing.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */

void
col_major_scan_forward(H5F_t *file_ptr, int32_t max_index, int32_t lag, bool verbose, bool reset_stats,
                       bool display_stats, bool display_detailed_stats, bool do_inserts, int dirty_unprotects)
{
    H5C_t  *cache_ptr = NULL;
    int32_t type      = 0;
    int32_t idx;
    int32_t local_max_index[NUMBER_OF_ENTRY_TYPES];

    if (verbose)
        fprintf(stdout, "%s: entering.\n", __func__);

    if (pass) {
        int i;

        cache_ptr = file_ptr->shared->cache;

        for (i = 0; i < NUMBER_OF_ENTRY_TYPES; i++)
            local_max_index[i] = MIN(max_index, max_indices[i]);

        assert(lag > 5);

        if (reset_stats) {

            H5C_stats__reset(cache_ptr);
        }
    }

    idx = -lag;

    while ((pass) && ((idx - lag) <= MAX_ENTRIES)) {
        type = 0;

        while ((pass) && (type < NUMBER_OF_ENTRY_TYPES)) {
            if ((pass) && (do_inserts) && ((idx + lag) >= 0) && ((idx + lag) <= local_max_index[type]) &&
                (((idx + lag) % 3) == 0) && (!entry_in_cache(cache_ptr, type, (idx + lag)))) {

                if (verbose)
                    fprintf(stdout, "(i, %d, %d) ", type, (idx + lag));

                insert_entry(file_ptr, type, (idx + lag), H5C__NO_FLAGS_SET);
            }

            if ((pass) && (idx >= 0) && (idx <= local_max_index[type])) {

                if (verbose)
                    fprintf(stdout, "(p, %d, %d) ", type, idx);

                protect_entry(file_ptr, type, idx);
            }

            if ((pass) && ((idx - lag) >= 0) && ((idx - lag) <= local_max_index[type])) {

                if (verbose)
                    fprintf(stdout, "(u, %d, %d) ", type, (idx - lag));

                unprotect_entry(file_ptr, type, idx - lag,
                                (dirty_unprotects ? H5C__DIRTIED_FLAG : H5C__NO_FLAGS_SET));
            }

            if (verbose)
                fprintf(stdout, "\n");

            type++;
        }

        idx++;
    }

    if ((pass) && (display_stats)) {

        H5C_stats(cache_ptr, "test cache", display_detailed_stats);
    }

} /* col_major_scan_forward() */

/*-------------------------------------------------------------------------
 * Function:    hl_col_major_scan_forward()
 *
 * Purpose:    Do a high locality sequence of inserts, protects, and
 *        unprotects while scanning through the set of entries.  If
 *        pass is false on entry, do nothing.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */

void
hl_col_major_scan_forward(H5F_t *file_ptr, int32_t max_index, bool verbose, bool reset_stats,
                          bool display_stats, bool display_detailed_stats, bool do_inserts,
                          int dirty_unprotects)
{
    H5C_t  *cache_ptr = NULL;
    int32_t type      = 0;
    int32_t idx;
    int32_t lag = 200;
    int32_t i;
    int32_t local_max_index;

    if (verbose)
        fprintf(stdout, "%s: entering.\n", __func__);

    if (pass) {

        cache_ptr = file_ptr->shared->cache;

        assert(cache_ptr != NULL);
        assert(lag > 5);
        assert(max_index >= 500);
        assert(max_index <= MAX_ENTRIES);

        if (reset_stats) {

            H5C_stats__reset(cache_ptr);
        }
    }

    idx = 0;

    local_max_index = MIN(max_index, MAX_ENTRIES);

    while ((pass) && (idx <= local_max_index)) {

        i = idx;

        while ((pass) && (i >= 0) && (i >= (idx - lag))) {

            type = 0;

            while ((pass) && (type < NUMBER_OF_ENTRY_TYPES)) {
                if ((pass) && (do_inserts) && (i == idx) && (i <= local_max_index) && ((i % 3) == 0) &&
                    (!entry_in_cache(cache_ptr, type, i))) {

                    if (verbose)
                        fprintf(stdout, "(i, %d, %d) ", type, i);

                    insert_entry(file_ptr, type, i, H5C__NO_FLAGS_SET);
                }

                if ((pass) && (i >= 0) && (i <= local_max_index)) {

                    if (verbose)
                        fprintf(stdout, "(p, %d, %d) ", type, i);

                    protect_entry(file_ptr, type, i);
                }

                if ((pass) && (i >= 0) && (i <= local_max_index)) {

                    if (verbose)
                        fprintf(stdout, "(u, %d, %d) ", type, i);

                    unprotect_entry(file_ptr, type, i,
                                    (dirty_unprotects ? H5C__DIRTIED_FLAG : H5C__NO_FLAGS_SET));
                }

                if (verbose)
                    fprintf(stdout, "\n");

                type++;
            }

            i--;
        }

        idx++;
    }

    if ((pass) && (display_stats)) {

        H5C_stats(cache_ptr, "test cache", display_detailed_stats);
    }

} /* hl_col_major_scan_forward() */

/*-------------------------------------------------------------------------
 * Function:    col_major_scan_backward()
 *
 * Purpose:    Do a sequence of inserts, protects, and unprotects
 *        while scanning backwards through the set of
 *        entries.  If pass is false on entry, do nothing.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */

void
col_major_scan_backward(H5F_t *file_ptr, int32_t max_index, int32_t lag, bool verbose, bool reset_stats,
                        bool display_stats, bool display_detailed_stats, bool do_inserts,
                        int dirty_unprotects)
{
    H5C_t  *cache_ptr  = NULL;
    int     mile_stone = 1;
    int32_t type;
    int32_t idx;
    int32_t local_max_index[NUMBER_OF_ENTRY_TYPES] = {0};

    if (verbose)
        fprintf(stdout, "%s: entering.\n", __func__);

    if (pass) {
        int i;

        cache_ptr = file_ptr->shared->cache;

        assert(cache_ptr != NULL);

        for (i = 0; i < NUMBER_OF_ENTRY_TYPES; i++)
            local_max_index[i] = MIN(max_index, max_indices[i]);

        assert(lag > 5);

        if (reset_stats) {

            H5C_stats__reset(cache_ptr);
        }
    }

    idx = local_max_index[NUMBER_OF_ENTRY_TYPES - 1] + lag;

    if (verbose) /* 1 */
        fprintf(stdout, "%s: point %d.\n", __func__, mile_stone++);

    while ((pass) && ((idx + lag) >= 0)) {
        type = NUMBER_OF_ENTRY_TYPES - 1;

        while ((pass) && (type >= 0)) {
            if ((pass) && (do_inserts) && ((idx - lag) >= 0) && ((idx - lag) <= local_max_index[type]) &&
                (((idx - lag) % 3) == 0) && (!entry_in_cache(cache_ptr, type, (idx - lag)))) {

                if (verbose)
                    fprintf(stdout, "(i, %d, %d) ", type, (idx - lag));

                insert_entry(file_ptr, type, (idx - lag), H5C__NO_FLAGS_SET);
            }

            if ((pass) && (idx >= 0) && (idx <= local_max_index[type])) {

                if (verbose)
                    fprintf(stdout, "(p, %d, %d) ", type, idx);

                protect_entry(file_ptr, type, idx);
            }

            if ((pass) && ((idx + lag) >= 0) && ((idx + lag) <= local_max_index[type])) {

                if (verbose)
                    fprintf(stdout, "(u, %d, %d) ", type, (idx + lag));

                unprotect_entry(file_ptr, type, idx + lag,
                                (dirty_unprotects ? H5C__DIRTIED_FLAG : H5C__NO_FLAGS_SET));
            }

            if (verbose)
                fprintf(stdout, "\n");

            type--;
        }

        idx--;
    }

    if (verbose) /* 2 */
        fprintf(stdout, "%s: point %d.\n", __func__, mile_stone++);

    if ((pass) && (display_stats)) {

        H5C_stats(cache_ptr, "test cache", display_detailed_stats);
    }

    if (verbose)
        fprintf(stdout, "%s: exiting.\n", __func__);

} /* col_major_scan_backward() */

/*-------------------------------------------------------------------------
 * Function:    hl_col_major_scan_backward()
 *
 * Purpose:    Do a high locality sequence of inserts, protects, and
 *        unprotects while scanning backwards through the set of
 *        entries.  If pass is false on entry, do nothing.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */

void
hl_col_major_scan_backward(H5F_t *file_ptr, int32_t max_index, bool verbose, bool reset_stats,
                           bool display_stats, bool display_detailed_stats, bool do_inserts,
                           int dirty_unprotects)
{
    H5C_t  *cache_ptr = NULL;
    int32_t type      = 0;
    int32_t idx       = -1;
    int32_t lag       = 50;
    int32_t i;
    int32_t local_max_index = -1;

    if (verbose)
        fprintf(stdout, "%s: entering.\n", __func__);

    if (pass) {

        cache_ptr = file_ptr->shared->cache;

        assert(cache_ptr != NULL);
        assert(lag > 5);
        assert(max_index >= 500);
        assert(max_index <= MAX_ENTRIES);

        local_max_index = MIN(max_index, MAX_ENTRIES);

        if (reset_stats) {

            H5C_stats__reset(cache_ptr);
        }

        idx = local_max_index;
    }

    while ((pass) && (idx >= 0)) {

        i = idx;

        while ((pass) && (i <= local_max_index) && (i <= (idx + lag))) {

            type = 0;

            while ((pass) && (type < NUMBER_OF_ENTRY_TYPES)) {
                if ((pass) && (do_inserts) && (i == idx) && (i <= local_max_index) &&
                    (!entry_in_cache(cache_ptr, type, i))) {

                    if (verbose)
                        fprintf(stdout, "(i, %d, %d) ", type, i);

                    insert_entry(file_ptr, type, i, H5C__NO_FLAGS_SET);
                }

                if ((pass) && (i >= 0) && (i <= local_max_index)) {

                    if (verbose)
                        fprintf(stdout, "(p, %d, %d) ", type, i);

                    protect_entry(file_ptr, type, i);
                }

                if ((pass) && (i >= 0) && (i <= local_max_index)) {

                    if (verbose)
                        fprintf(stdout, "(u, %d, %d) ", type, i);

                    unprotect_entry(file_ptr, type, i,
                                    (dirty_unprotects ? H5C__DIRTIED_FLAG : H5C__NO_FLAGS_SET));
                }

                if (verbose)
                    fprintf(stdout, "\n");

                type++;
            }

            i++;
        }

        idx--;
    }

    if ((pass) && (display_stats)) {

        H5C_stats(cache_ptr, "test cache", display_detailed_stats);
    }

} /* hl_col_major_scan_backward() */

/*-------------------------------------------------------------------------
 * Function:    create_flush_dependency()
 *
 * Purpose:    Create a 'flush dependency' between two entries.
 *
 *        Do nothing if pass is false.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */

void
create_flush_dependency(int32_t par_type, int32_t par_idx, int32_t chd_type, int32_t chd_idx)
{
    assert((0 <= par_type) && (par_type < NUMBER_OF_ENTRY_TYPES));
    assert((0 <= par_idx) && (par_idx <= max_indices[par_type]));
    assert((0 <= chd_type) && (chd_type < NUMBER_OF_ENTRY_TYPES));
    assert((0 <= chd_idx) && (chd_idx <= max_indices[chd_type]));

    if (pass) {
        test_entry_t *par_base_addr; /* Base entry of parent's entry array */
        test_entry_t *par_entry_ptr; /* Parent entry */
        test_entry_t *chd_base_addr; /* Base entry of child's entry array */
        test_entry_t *chd_entry_ptr; /* Child entry */
        bool          par_is_pinned; /* Whether parent is already pinned */
        herr_t        result;        /* API routine status */

        /* Get parent entry */
        par_base_addr = entries[par_type];
        par_entry_ptr = &(par_base_addr[par_idx]);
        par_is_pinned = par_entry_ptr->header.is_pinned;

        /* Sanity check parent entry */
        assert(par_entry_ptr->index == par_idx);
        assert(par_entry_ptr->type == par_type);
        assert(par_entry_ptr->header.is_protected);
        assert(par_entry_ptr == par_entry_ptr->self);

        /* Get parent entry */
        chd_base_addr = entries[chd_type];
        chd_entry_ptr = &(chd_base_addr[chd_idx]);

        /* Sanity check child entry */
        assert(chd_entry_ptr->index == chd_idx);
        assert(chd_entry_ptr->type == chd_type);
        assert(chd_entry_ptr == chd_entry_ptr->self);

        result = H5C_create_flush_dependency(par_entry_ptr, chd_entry_ptr);

        if ((result < 0) || (!par_entry_ptr->header.is_pinned) ||
            (!(par_entry_ptr->header.flush_dep_nchildren > 0))) {

            pass         = false;
            failure_mssg = "error in H5C_create_flush_dependency().";
        } /* end if */

        /* Update information about entries */
        assert(chd_entry_ptr->flush_dep_npar < MAX_FLUSH_DEP_PARS);
        chd_entry_ptr->flush_dep_par_type[chd_entry_ptr->flush_dep_npar] = par_type;
        chd_entry_ptr->flush_dep_par_idx[chd_entry_ptr->flush_dep_npar]  = par_idx;
        chd_entry_ptr->flush_dep_npar++;
        par_entry_ptr->flush_dep_nchd++;
        if (chd_entry_ptr->is_dirty || chd_entry_ptr->flush_dep_ndirty_chd > 0) {
            assert(par_entry_ptr->flush_dep_ndirty_chd < par_entry_ptr->flush_dep_nchd);
            par_entry_ptr->flush_dep_ndirty_chd++;
        } /* end if */
        par_entry_ptr->pinned_from_cache = true;
        if (!par_is_pinned)
            par_entry_ptr->is_pinned = true;
    } /* end if */
} /* create_flush_dependency() */

/*-------------------------------------------------------------------------
 * Function:    destroy_flush_dependency()
 *
 * Purpose:    Destroy a 'flush dependency' between two entries.
 *
 *        Do nothing if pass is false.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */

void
destroy_flush_dependency(int32_t par_type, int32_t par_idx, int32_t chd_type, int32_t chd_idx)
{
    assert((0 <= par_type) && (par_type < NUMBER_OF_ENTRY_TYPES));
    assert((0 <= par_idx) && (par_idx <= max_indices[par_type]));
    assert((0 <= chd_type) && (chd_type < NUMBER_OF_ENTRY_TYPES));
    assert((0 <= chd_idx) && (chd_idx <= max_indices[chd_type]));

    if (pass) {
        test_entry_t *par_base_addr; /* Base entry of parent's entry array */
        test_entry_t *par_entry_ptr; /* Parent entry */
        test_entry_t *chd_base_addr; /* Base entry of child's entry array */
        test_entry_t *chd_entry_ptr; /* Child entry */
        unsigned      i;             /* Local index variable */

        /* Get parent entry */
        par_base_addr = entries[par_type];
        par_entry_ptr = &(par_base_addr[par_idx]);

        /* Sanity check parent entry */
        assert(par_entry_ptr->is_pinned);
        assert(par_entry_ptr->pinned_from_cache);
        assert(par_entry_ptr->flush_dep_nchd > 0);
        assert(par_entry_ptr == par_entry_ptr->self);

        /* Get parent entry */
        chd_base_addr = entries[chd_type];
        chd_entry_ptr = &(chd_base_addr[chd_idx]);

        /* Sanity check child entry */
        assert(chd_entry_ptr->index == chd_idx);
        assert(chd_entry_ptr->type == chd_type);
        assert(chd_entry_ptr->flush_dep_npar > 0);
        assert(chd_entry_ptr == chd_entry_ptr->self);

        if (H5C_destroy_flush_dependency(par_entry_ptr, chd_entry_ptr) < 0) {
            pass         = false;
            failure_mssg = "error in H5C_destroy_flush_dependency().";
        } /* end if */

        /* Update information about entries */
        for (i = 0; i < chd_entry_ptr->flush_dep_npar; i++)
            if (chd_entry_ptr->flush_dep_par_type[i] == par_type &&
                chd_entry_ptr->flush_dep_par_idx[i] == par_idx)
                break;
        assert(i < chd_entry_ptr->flush_dep_npar);
        if (i < chd_entry_ptr->flush_dep_npar - 1)
            memmove(&chd_entry_ptr->flush_dep_par_type[i], &chd_entry_ptr->flush_dep_par_type[i + 1],
                    (chd_entry_ptr->flush_dep_npar - i - 1) * sizeof(chd_entry_ptr->flush_dep_par_type[0]));
        if (i < chd_entry_ptr->flush_dep_npar - 1)
            memmove(&chd_entry_ptr->flush_dep_par_idx[i], &chd_entry_ptr->flush_dep_par_idx[i + 1],
                    (chd_entry_ptr->flush_dep_npar - i - 1) * sizeof(chd_entry_ptr->flush_dep_par_idx[0]));
        chd_entry_ptr->flush_dep_npar--;
        par_entry_ptr->flush_dep_nchd--;
        if (par_entry_ptr->flush_dep_nchd == 0) {
            par_entry_ptr->pinned_from_cache = false;
            par_entry_ptr->is_pinned         = par_entry_ptr->pinned_from_client;
        } /* end if */
        if (chd_entry_ptr->is_dirty || chd_entry_ptr->flush_dep_ndirty_chd > 0) {
            assert(par_entry_ptr->flush_dep_ndirty_chd > 0);
            par_entry_ptr->flush_dep_ndirty_chd--;
            if (!par_entry_ptr->is_dirty && par_entry_ptr->flush_dep_ndirty_chd == 0)
                mark_flush_dep_clean(par_entry_ptr);
        } /* end if */
    }     /* end if */
} /* destroy_flush_dependency() */

/*-------------------------------------------------------------------------
 * Function:    mark_flush_dep_dirty()
 *
 * Purpose:     Recursively propagate the flush_dep_ndirty_children flag
 *              up the dependency chain in response to entry either
 *              becoming dirty or having its flush_dep_ndirty_children
 *              increased from 0.
 *
 * Return:      <none>
 *
 *-------------------------------------------------------------------------
 */
static void
mark_flush_dep_dirty(test_entry_t *entry_ptr)
{
    /* Sanity checks */
    assert(entry_ptr);

    /* Iterate over the parent entries */
    if (entry_ptr->flush_dep_npar) {
        test_entry_t *par_base_addr; /* Base entry of parent's entry array */
        test_entry_t *par_entry_ptr; /* Parent entry */
        unsigned      u;             /* Local index variable */

        for (u = 0; u < entry_ptr->flush_dep_npar; u++) {
            /* Get parent entry */
            par_base_addr = entries[entry_ptr->flush_dep_par_type[u]];
            par_entry_ptr = &(par_base_addr[entry_ptr->flush_dep_par_idx[u]]);

            /* Sanity check */
            assert(par_entry_ptr->flush_dep_ndirty_chd < par_entry_ptr->flush_dep_nchd);

            /* Adjust the parent's number of dirty children */
            par_entry_ptr->flush_dep_ndirty_chd++;
        } /* end for */
    }     /* end if */
} /* end mark_flush_dep_dirty() */

/*-------------------------------------------------------------------------
 * Function:    mark_flush_dep_clean()
 *
 * Purpose:     Recursively propagate the flush_dep_ndirty_children flag
 *              up the dependency chain in response to entry either
 *              becoming clean or having its flush_dep_ndirty_children
 *              reduced to 0.
 *
 * Return:      <none>
 *
 *-------------------------------------------------------------------------
 */
static void
mark_flush_dep_clean(test_entry_t *entry_ptr)
{
    /* Sanity checks */
    assert(entry_ptr);
    assert(!entry_ptr->is_dirty && entry_ptr->flush_dep_ndirty_chd == 0);

    /* Iterate over the parent entries */
    if (entry_ptr->flush_dep_npar) {
        test_entry_t *par_base_addr; /* Base entry of parent's entry array */
        test_entry_t *par_entry_ptr; /* Parent entry */
        unsigned      u;             /* Local index variable */

        for (u = 0; u < entry_ptr->flush_dep_npar; u++) {
            /* Get parent entry */
            par_base_addr = entries[entry_ptr->flush_dep_par_type[u]];
            par_entry_ptr = &(par_base_addr[entry_ptr->flush_dep_par_idx[u]]);

            /* Sanity check */
            assert(par_entry_ptr->flush_dep_ndirty_chd > 0);

            /* Adjust the parent's number of dirty children */
            par_entry_ptr->flush_dep_ndirty_chd--;
        } /* end for */
    }     /* end if */
} /* end mark_flush_dep_clean() */

/*** H5AC level utility functions ***/

/*-------------------------------------------------------------------------
 * Function:    check_and_validate_cache_hit_rate()
 *
 * Purpose:    Use the API functions to get and reset the cache hit rate.
 *        Verify that the value returned by the API call agrees with
 *        the cache internal data structures.
 *
 *        If the number of cache accesses exceeds the value provided
 *        in the min_accesses parameter, and the hit rate is less than
 *        min_hit_rate, set pass to false, and set failure_mssg to
 *        a string indicating that hit rate was unexpectedly low.
 *
 *        Return hit rate in *hit_rate_ptr, and print the data to
 *        stdout if requested.
 *
 *        If an error is detected, set pass to false, and set
 *        failure_mssg to an appropriate value.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */

void
check_and_validate_cache_hit_rate(hid_t file_id, double *hit_rate_ptr, bool dump_data, int64_t min_accesses,
                                  double min_hit_rate)
{
    herr_t  result;
    int64_t cache_hits     = 0;
    int64_t cache_accesses = 0;
    double  expected_hit_rate;
    double  hit_rate  = 0.0;
    H5F_t  *file_ptr  = NULL;
    H5C_t  *cache_ptr = NULL;

    /* get a pointer to the files internal data structure */
    if (pass) {

        file_ptr = (H5F_t *)H5VL_object_verify(file_id, H5I_FILE);

        if (file_ptr == NULL) {

            pass         = false;
            failure_mssg = "Can't get file_ptr.";
        }
        else {

            cache_ptr = file_ptr->shared->cache;
            if (NULL == cache_ptr) {
                pass         = false;
                failure_mssg = "NULL cache pointer";
            }
        }
    }

    /* compare the cache's internal configuration with the expected value */
    if (pass) {

        cache_hits     = cache_ptr->cache_hits;
        cache_accesses = cache_ptr->cache_accesses;

        if (cache_accesses > 0) {

            expected_hit_rate = ((double)cache_hits) / ((double)cache_accesses);
        }
        else {

            expected_hit_rate = 0.0;
        }

        result = H5Fget_mdc_hit_rate(file_id, &hit_rate);

        if (result < 0) {

            pass         = false;
            failure_mssg = "H5Fget_mdc_hit_rate() failed.";
        }
        else if (!H5_DBL_ABS_EQUAL(hit_rate, expected_hit_rate)) {

            pass         = false;
            failure_mssg = "unexpected hit rate.";
        }
    }

    if (pass) { /* reset the hit rate */

        result = H5Freset_mdc_hit_rate_stats(file_id);

        if (result < 0) {

            pass         = false;
            failure_mssg = "H5Freset_mdc_hit_rate_stats() failed.";
        }
    }

    /* set *hit_rate_ptr if appropriate */
    if ((pass) && (hit_rate_ptr != NULL)) {

        *hit_rate_ptr = hit_rate;
    }

    /* dump data to stdout if requested */
    if ((pass) && (dump_data)) {

        fprintf(stdout, "cache_hits: %ld, cache_accesses: %ld, hit_rate: %lf\n", (long)cache_hits,
                (long)cache_accesses, hit_rate);
    }

    if ((pass) && (cache_accesses > min_accesses) && (hit_rate < min_hit_rate)) {

        pass         = false;
        failure_mssg = "Unexpectedly low hit rate.";
    }

} /* check_and_validate_cache_hit_rate() */

/*-------------------------------------------------------------------------
 * Function:    check_and_validate_cache_size()
 *
 * Purpose:    Use the API function to get the cache size data.  Verify
 *        that the values returned by the API call agree with
 *        the cache internal data structures.
 *
 *        Return size data in the locations specified by the pointer
 *        parameters if these parameters are not NULL.  Print the
 *        data to stdout if requested.
 *
 *        If an error is detected, set pass to false, and set
 *        failure_mssg to an appropriate value.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */

void
check_and_validate_cache_size(hid_t file_id, size_t *max_size_ptr, size_t *min_clean_size_ptr,
                              size_t *cur_size_ptr, int32_t *cur_num_entries_ptr, bool dump_data)
{
    herr_t   result;
    size_t   expected_max_size;
    size_t   max_size = 0;
    size_t   expected_min_clean_size;
    size_t   min_clean_size = 0;
    size_t   expected_cur_size;
    size_t   cur_size = 0;
    uint32_t expected_cur_num_entries;
    int      cur_num_entries = 0;
    H5F_t   *file_ptr        = NULL;
    H5C_t   *cache_ptr       = NULL;

    /* get a pointer to the files internal data structure */
    if (pass) {

        file_ptr = (H5F_t *)H5VL_object_verify(file_id, H5I_FILE);

        if (file_ptr == NULL) {

            pass         = false;
            failure_mssg = "Can't get file_ptr.";
        }
        else {

            cache_ptr = file_ptr->shared->cache;
            if (NULL == cache_ptr) {
                pass         = false;
                failure_mssg = "NULL cache pointer";
            }
        }
    }

    /* compare the cache's internal configuration with the expected value */
    if (pass) {

        expected_max_size        = cache_ptr->max_cache_size;
        expected_min_clean_size  = cache_ptr->min_clean_size;
        expected_cur_size        = cache_ptr->index_size;
        expected_cur_num_entries = cache_ptr->index_len;

        result = H5Fget_mdc_size(file_id, &max_size, &min_clean_size, &cur_size, &cur_num_entries);

        if (result < 0) {

            pass         = false;
            failure_mssg = "H5Fget_mdc_size() failed.";
        }
        else if ((max_size != expected_max_size) || (min_clean_size != expected_min_clean_size) ||
                 (cur_size != expected_cur_size) || (cur_num_entries != (int)expected_cur_num_entries)) {

            pass         = false;
            failure_mssg = "H5Fget_mdc_size() returned unexpected value(s).";
        }
    }

    /* return size values if requested */
    if ((pass) && (max_size_ptr != NULL)) {

        *max_size_ptr = max_size;
    }

    if ((pass) && (min_clean_size_ptr != NULL)) {

        *min_clean_size_ptr = min_clean_size;
    }

    if ((pass) && (cur_size_ptr != NULL)) {

        *cur_size_ptr = cur_size;
    }

    if ((pass) && (cur_num_entries_ptr != NULL)) {

        *cur_num_entries_ptr = cur_num_entries;
    }

    /* dump data to stdout if requested */
    if ((pass) && (dump_data)) {

        fprintf(stdout, "max_sz: %ld, min_clean_sz: %ld, cur_sz: %ld, cur_ent: %ld\n", (long)max_size,
                (long)min_clean_size, (long)cur_size, (long)cur_num_entries);
    }

} /* check_and_validate_cache_size() */

H5_ATTR_PURE bool
resize_configs_are_equal(const H5C_auto_size_ctl_t *a, const H5C_auto_size_ctl_t *b, bool compare_init)
{
    if (a->version != b->version)
        return (false);
    else if (a->rpt_fcn != b->rpt_fcn)
        return (false);
    else if (compare_init && (a->set_initial_size != b->set_initial_size))
        return (false);
    else if (compare_init && (a->initial_size != b->initial_size))
        return (false);
    else if (!H5_DBL_ABS_EQUAL(a->min_clean_fraction, b->min_clean_fraction))
        return (false);
    else if (a->max_size != b->max_size)
        return (false);
    else if (a->min_size != b->min_size)
        return (false);
    else if (a->epoch_length != b->epoch_length)
        return (false);
    else if (a->incr_mode != b->incr_mode)
        return (false);
    else if (!H5_DBL_ABS_EQUAL(a->lower_hr_threshold, b->lower_hr_threshold))
        return (false);
    else if (!H5_DBL_ABS_EQUAL(a->increment, b->increment))
        return (false);
    else if (a->apply_max_increment != b->apply_max_increment)
        return (false);
    else if (a->max_increment != b->max_increment)
        return (false);
    else if (a->flash_incr_mode != b->flash_incr_mode)
        return (false);
    else if (!H5_DBL_ABS_EQUAL(a->flash_multiple, b->flash_multiple))
        return (false);
    else if (!H5_DBL_ABS_EQUAL(a->flash_threshold, b->flash_threshold))
        return (false);
    else if (a->decr_mode != b->decr_mode)
        return (false);
    else if (!H5_DBL_ABS_EQUAL(a->upper_hr_threshold, b->upper_hr_threshold))
        return (false);
    else if (!H5_DBL_ABS_EQUAL(a->decrement, b->decrement))
        return (false);
    else if (a->apply_max_decrement != b->apply_max_decrement)
        return (false);
    else if (a->max_decrement != b->max_decrement)
        return (false);
    else if (a->epochs_before_eviction != b->epochs_before_eviction)
        return (false);
    else if (a->apply_empty_reserve != b->apply_empty_reserve)
        return (false);
    else if (!H5_DBL_ABS_EQUAL(a->empty_reserve, b->empty_reserve))
        return (false);
    return (true);
}

/*-------------------------------------------------------------------------
 * Function:    validate_mdc_config()
 *
 * Purpose:    Verify that the file indicated by the file_id parameter
 *        has both internal and external configuration matching
 *        *config_ptr.
 *
 *        Do nothing on success.  On failure, set pass to false, and
 *        load an error message into failue_mssg.  Note that
 *        failure_msg is assumed to be at least 128 bytes in length.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */

void
validate_mdc_config(hid_t file_id, H5AC_cache_config_t *ext_config_ptr, bool compare_init, int test_num)
{
    H5F_t              *file_ptr  = NULL;
    H5C_t              *cache_ptr = NULL;
    H5AC_cache_config_t scratch;
    H5C_auto_size_ctl_t int_config;

    XLATE_EXT_TO_INT_MDC_CONFIG(int_config, (*ext_config_ptr))

    /* get a pointer to the files internal data structure */
    if (pass) {

        file_ptr = (H5F_t *)H5VL_object_verify(file_id, H5I_FILE);

        if (file_ptr == NULL) {

            pass = false;
            snprintf(tmp_msg_buf, sizeof(tmp_msg_buf), "Can't get file_ptr #%d.", test_num);
            failure_mssg = tmp_msg_buf;
        }
        else {

            cache_ptr = file_ptr->shared->cache;
        }
    }

    /* verify that we can access the internal version of the cache config */
    if (pass) {

        if (cache_ptr == NULL || cache_ptr->resize_ctl.version != H5C__CURR_AUTO_SIZE_CTL_VER) {

            pass = false;
            snprintf(tmp_msg_buf, sizeof(tmp_msg_buf), "Can't access cache resize_ctl #%d.", test_num);
            failure_mssg = tmp_msg_buf;
        }
    }

    /* compare the cache's internal configuration with the expected value */
    if (pass) {

        if (!resize_configs_are_equal(&int_config, &cache_ptr->resize_ctl, compare_init)) {

            pass = false;
            snprintf(tmp_msg_buf, sizeof(tmp_msg_buf), "Unexpected internal config #%d.", test_num);
            failure_mssg = tmp_msg_buf;
        }
    }

    /* obtain external cache config */
    if (pass) {

        scratch.version = H5AC__CURR_CACHE_CONFIG_VERSION;

        if (H5Fget_mdc_config(file_id, &scratch) < 0) {

            pass = false;
            snprintf(tmp_msg_buf, sizeof(tmp_msg_buf), "H5Fget_mdc_config() failed #%d.", test_num);
            failure_mssg = tmp_msg_buf;
        }
    }

    if (pass) {

        /* Recall that in any configuration supplied by the cache
         * at run time, the set_initial_size field will always
         * be false, regardless of the value passed in.  Thus we
         * always presume that this field need not match that of
         * the supplied external configuration.
         *
         * The cache also sets the initial_size field to the current
         * cache max size instead of the value initially supplied.
         * Depending on circumstances, this may or may not match
         * the original.  Hence the compare_init parameter.
         */
        if (!CACHE_CONFIGS_EQUAL((*ext_config_ptr), scratch, false, compare_init)) {

            pass = false;
            snprintf(tmp_msg_buf, sizeof(tmp_msg_buf), "Unexpected external config #%d.", test_num);
            failure_mssg = tmp_msg_buf;
        }
    }

} /* validate_mdc_config() */

#if 0 /* debugging functions -- normally commented out */
/*-------------------------------------------------------------------------
 * Function:    dump_LRU
 *
 * Purpose:     Display a summarize list of the contents of the LRU
 *              from head to tail.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
void
dump_LRU(H5F_t * file_ptr)
{
    const char * hdr_0 =
        " Entry  Entry   Entry       Entry         Entry               \n";
    const char * hdr_1 =
        " Num:   Dirty:  Size:       Addr:         Type:               \n";
    const char * hdr_2 =
        "==============================================================\n";
    int i = 0;
    H5C_cache_entry_t * entry_ptr = NULL;
    H5C_t *cache_ptr = file_ptr->shared->cache;

    assert(cache_ptr);

    entry_ptr = cache_ptr->LRU_head_ptr;

    fprintf(stdout,
              "\n\nIndex len/size/clean size/dirty size = %u/%lld/%lld/%lld\n",
              cache_ptr->index_len, (long long)(cache_ptr->index_size),
              (long long)(cache_ptr->clean_index_size),
              (long long)(cache_ptr->dirty_index_size));
    fprintf(stdout, "\nLRU len/size = %d/%lld.\n\n",
              cache_ptr->LRU_list_len, (long long)(cache_ptr->LRU_list_size));

    if ( entry_ptr != NULL )
    {
        fprintf(stdout, "%s%s%s", hdr_0, hdr_1, hdr_2);
    }

    while ( entry_ptr != NULL )
    {
        fprintf(stdout,
                  "  %3d     %d     %10lld  0x%010llx  %s(%d)\n",
                  i,
                  (int)(entry_ptr->is_dirty),
                  (long long)(entry_ptr->size),
                  (long long)(entry_ptr->addr),
                  entry_ptr->type->name,
                  entry_ptr->type->id);
        i++;
        entry_ptr = entry_ptr->next;
    }

    if ( cache_ptr->LRU_list_len > 0 )
    {
        fprintf(stdout, "%s\n", hdr_2);
    }

    return;

} /* dump_LRU() */

#endif /* debugging functions -- normally commented out */
