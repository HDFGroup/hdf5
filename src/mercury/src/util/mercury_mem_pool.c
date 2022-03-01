/*
 * Copyright (C) 2013-2020 Argonne National Laboratory, Department of Energy,
 *                    UChicago Argonne, LLC and The HDF Group.
 * All rights reserved.
 *
 * The full copyright notice, including terms governing use, modification,
 * and redistribution, is contained in the COPYING file that can be
 * found at the root of the source code distribution tree.
 */

#include "mercury_mem_pool.h"

#include "mercury_mem.h"
#include "mercury_queue.h"
#include "mercury_thread_condition.h"
#include "mercury_thread_mutex.h"
#include "mercury_thread_spin.h"
#include "mercury_util_error.h"

#include <stdlib.h>
#include <string.h>

/****************/
/* Local Macros */
/****************/

/**
 * container_of - cast a member of a structure done to the containing structure
 * \ptr:        the pointer to the member.
 * \type:       the type of the container struct this is embedded in.
 * \member:     the name of the member within the struct.
 *
 */
#if !defined(container_of)
#define container_of(ptr, type, member) ((type *)((char *)ptr - offsetof(type, member)))
#endif

/************************************/
/* Local Type and Struct Definition */
/************************************/

/**
 * Memory chunk (points to actual data).
 */
struct hg_mem_pool_chunk {
    HG_QUEUE_ENTRY(hg_mem_pool_chunk) entry; /* Entry in chunk_list */
    char *chunk;                             /* Must be last        */
};

/**
 * Memory block. Each block has a fixed chunk size, the underlying memory
 * buffer is registered.
 */
struct hg_mem_pool_block {
    HG_QUEUE_HEAD(hg_mem_pool_chunk) chunks; /* Chunk list           */
    HG_QUEUE_ENTRY(hg_mem_pool_block) entry; /* Entry in block list  */
    void *           mr_handle;              /* Pointer to MR handle */
    hg_thread_spin_t chunk_lock;             /* Chunk list lock      */
};

/**
 * Memory pool. A pool is composed of multiple blocks.
 */
struct hg_mem_pool {
    hg_thread_mutex_t extend_mutex;                /* Extend mutex    */
    hg_thread_cond_t  extend_cond;                 /* Extend cond     */
    HG_QUEUE_HEAD(hg_mem_pool_block) blocks;       /* Block list      */
    hg_mem_pool_register_func_t   register_func;   /* Register func   */
    hg_mem_pool_deregister_func_t deregister_func; /* Deregister func */
    void *                        arg;             /* Func args       */
    size_t                        chunk_size;      /* Chunk size      */
    size_t                        chunk_count;     /* Chunk count     */
    int                           extending;       /* Extending pool  */
    hg_thread_spin_t              block_lock;      /* Block list lock */
};

/********************/
/* Local Prototypes */
/********************/

/* Allocate new pool block */
static struct hg_mem_pool_block *hg_mem_pool_block_alloc(size_t chunk_size, size_t chunk_count,
                                                         hg_mem_pool_register_func_t register_func,
                                                         void *                      arg);

/* Free pool block */
static void hg_mem_pool_block_free(struct hg_mem_pool_block *    hg_mem_pool_block,
                                   hg_mem_pool_deregister_func_t deregister_func, void *arg);

/*******************/
/* Local Variables */
/*******************/

/*---------------------------------------------------------------------------*/

struct hg_mem_pool *
hg_mem_pool_create(size_t chunk_size, size_t chunk_count, size_t block_count,
                   hg_mem_pool_register_func_t register_func, hg_mem_pool_deregister_func_t deregister_func,
                   void *arg)
{
    struct hg_mem_pool *hg_mem_pool = NULL;
    size_t              i;

    hg_mem_pool = (struct hg_mem_pool *)malloc(sizeof(struct hg_mem_pool));
    HG_UTIL_CHECK_ERROR_NORET(hg_mem_pool == NULL, done, "Could not allocate memory pool");
    HG_QUEUE_INIT(&hg_mem_pool->blocks);
    hg_mem_pool->register_func   = register_func;
    hg_mem_pool->deregister_func = deregister_func;
    hg_mem_pool->arg             = arg;
    hg_mem_pool->chunk_size      = chunk_size;
    hg_mem_pool->chunk_count     = chunk_count;
    hg_thread_mutex_init(&hg_mem_pool->extend_mutex);
    hg_thread_cond_init(&hg_mem_pool->extend_cond);
    hg_thread_spin_init(&hg_mem_pool->block_lock);
    hg_mem_pool->extending = 0;

    /* Allocate single block */
    for (i = 0; i < block_count; i++) {
        struct hg_mem_pool_block *hg_mem_pool_block =
            hg_mem_pool_block_alloc(chunk_size, chunk_count, register_func, arg);
        HG_UTIL_CHECK_ERROR_NORET(hg_mem_pool_block == NULL, error, "Could not allocate block of %zu bytes",
                                  chunk_size * chunk_count);
        HG_QUEUE_PUSH_TAIL(&hg_mem_pool->blocks, hg_mem_pool_block, entry);
    }

done:
    return hg_mem_pool;

error:
    hg_mem_pool_destroy(hg_mem_pool);
    return NULL;
}

/*---------------------------------------------------------------------------*/
void
hg_mem_pool_destroy(struct hg_mem_pool *hg_mem_pool)
{
    if (!hg_mem_pool)
        return;

    while (!HG_QUEUE_IS_EMPTY(&hg_mem_pool->blocks)) {
        struct hg_mem_pool_block *hg_mem_pool_block = HG_QUEUE_FIRST(&hg_mem_pool->blocks);
        HG_QUEUE_POP_HEAD(&hg_mem_pool->blocks, entry);
        hg_mem_pool_block_free(hg_mem_pool_block, hg_mem_pool->deregister_func, hg_mem_pool->arg);
    }
    hg_thread_mutex_destroy(&hg_mem_pool->extend_mutex);
    hg_thread_cond_destroy(&hg_mem_pool->extend_cond);
    hg_thread_spin_destroy(&hg_mem_pool->block_lock);
    free(hg_mem_pool);
}

/*---------------------------------------------------------------------------*/
static struct hg_mem_pool_block *
hg_mem_pool_block_alloc(size_t chunk_size, size_t chunk_count, hg_mem_pool_register_func_t register_func,
                        void *arg)
{
    struct hg_mem_pool_block *hg_mem_pool_block = NULL;
    size_t                    page_size         = (size_t)hg_mem_get_page_size();
    void *                    mem_ptr = NULL, *mr_handle = NULL;
    size_t                    block_size, i;
    size_t                    block_header = sizeof(struct hg_mem_pool_block);
    size_t                    chunk_header = offsetof(struct hg_mem_pool_chunk, chunk);

    /* Size of block struct + number of chunks x (chunk_size + size of entry) */
    block_size = block_header + chunk_count * (chunk_header + chunk_size);

    /* Allocate backend buffer */
    mem_ptr = hg_mem_aligned_alloc(page_size, block_size);
    HG_UTIL_CHECK_ERROR_NORET(mem_ptr == NULL, done, "Could not allocate %zu bytes", block_size);
    memset(mem_ptr, 0, block_size);

    /* Register memory if registration function is provided */
    if (register_func) {
        int rc = register_func(mem_ptr, block_size, &mr_handle, arg);
        if (unlikely(rc != HG_UTIL_SUCCESS)) {
            hg_mem_aligned_free(mem_ptr);
            HG_UTIL_GOTO_ERROR(done, mem_ptr, NULL, "register_func() failed");
        }
    }

    /* Map allocated memory to block */
    hg_mem_pool_block = (struct hg_mem_pool_block *)mem_ptr;

    HG_QUEUE_INIT(&hg_mem_pool_block->chunks);
    hg_thread_spin_init(&hg_mem_pool_block->chunk_lock);
    hg_mem_pool_block->mr_handle = mr_handle;

    /* Assign chunks and insert them to free list */
    for (i = 0; i < chunk_count; i++) {
        struct hg_mem_pool_chunk *hg_mem_pool_chunk =
            (struct hg_mem_pool_chunk *)((char *)hg_mem_pool_block + block_header +
                                         i * (chunk_header + chunk_size));
        HG_QUEUE_PUSH_TAIL(&hg_mem_pool_block->chunks, hg_mem_pool_chunk, entry);
    }

done:
    return hg_mem_pool_block;
}

/*---------------------------------------------------------------------------*/
static void
hg_mem_pool_block_free(struct hg_mem_pool_block *    hg_mem_pool_block,
                       hg_mem_pool_deregister_func_t deregister_func, void *arg)
{
    if (!hg_mem_pool_block)
        return;

    /* Release MR handle is there was any */
    if (hg_mem_pool_block->mr_handle && deregister_func) {
        int rc = deregister_func(hg_mem_pool_block->mr_handle, arg);
        HG_UTIL_CHECK_ERROR_NORET(rc != HG_UTIL_SUCCESS, done, "deregister_func() failed");
    }

done:
    hg_thread_spin_destroy(&hg_mem_pool_block->chunk_lock);
    hg_mem_aligned_free((void *)hg_mem_pool_block);
    return;
}

/*---------------------------------------------------------------------------*/
void *
hg_mem_pool_alloc(struct hg_mem_pool *hg_mem_pool, size_t size, void **mr_handle)
{
    struct hg_mem_pool_block *hg_mem_pool_block;
    struct hg_mem_pool_chunk *hg_mem_pool_chunk = NULL;
    void *                    mem_ptr           = NULL;

    HG_UTIL_CHECK_ERROR(size > hg_mem_pool->chunk_size, done, mem_ptr, NULL,
                        "Chunk size is too small for requested size");
    HG_UTIL_CHECK_ERROR(!mr_handle && hg_mem_pool->register_func, done, mem_ptr, NULL, "MR handle is NULL");

    do {
        int found = 0;

        /* Check whether we can get a block from one of the pools */
        hg_thread_spin_lock(&hg_mem_pool->block_lock);
        HG_QUEUE_FOREACH(hg_mem_pool_block, &hg_mem_pool->blocks, entry)
        {
            hg_thread_spin_lock(&hg_mem_pool_block->chunk_lock);
            found = !HG_QUEUE_IS_EMPTY(&hg_mem_pool_block->chunks);
            hg_thread_spin_unlock(&hg_mem_pool_block->chunk_lock);
            if (found)
                break;
        }
        hg_thread_spin_unlock(&hg_mem_pool->block_lock);

        /* If not, allocate and register a new pool */
        if (!found) {
            /* Let other threads sleep while the pool is being extended */
            hg_thread_mutex_lock(&hg_mem_pool->extend_mutex);
            if (hg_mem_pool->extending) {
                hg_thread_cond_wait(&hg_mem_pool->extend_cond, &hg_mem_pool->extend_mutex);
                hg_thread_mutex_unlock(&hg_mem_pool->extend_mutex);
                continue;
            }
            hg_mem_pool->extending = 1;
            hg_thread_mutex_unlock(&hg_mem_pool->extend_mutex);

            hg_mem_pool_block = hg_mem_pool_block_alloc(hg_mem_pool->chunk_size, hg_mem_pool->chunk_count,
                                                        hg_mem_pool->register_func, hg_mem_pool->arg);
            HG_UTIL_CHECK_ERROR(hg_mem_pool_block == NULL, done, mem_ptr, NULL,
                                "Could not allocate block of %zu bytes",
                                hg_mem_pool->chunk_size * hg_mem_pool->chunk_count);

            hg_thread_spin_lock(&hg_mem_pool->block_lock);
            HG_QUEUE_PUSH_TAIL(&hg_mem_pool->blocks, hg_mem_pool_block, entry);
            hg_thread_spin_unlock(&hg_mem_pool->block_lock);

            hg_thread_mutex_lock(&hg_mem_pool->extend_mutex);
            hg_mem_pool->extending = 0;
            hg_thread_cond_broadcast(&hg_mem_pool->extend_cond);
            hg_thread_mutex_unlock(&hg_mem_pool->extend_mutex);
        }

        /* Try to pick a node from one of the available pools */
        hg_thread_spin_lock(&hg_mem_pool_block->chunk_lock);
        if (!HG_QUEUE_IS_EMPTY(&hg_mem_pool_block->chunks)) {
            hg_mem_pool_chunk = HG_QUEUE_FIRST(&hg_mem_pool_block->chunks);
            HG_QUEUE_POP_HEAD(&hg_mem_pool_block->chunks, entry);
        }
        hg_thread_spin_unlock(&hg_mem_pool_block->chunk_lock);
    } while (!hg_mem_pool_chunk);

    mem_ptr = &hg_mem_pool_chunk->chunk;
    if (mr_handle && hg_mem_pool_block)
        *mr_handle = hg_mem_pool_block->mr_handle;

done:
    return mem_ptr;
}

/*---------------------------------------------------------------------------*/
void
hg_mem_pool_free(struct hg_mem_pool *hg_mem_pool, void *mem_ptr, void *mr_handle)
{
    struct hg_mem_pool_block *hg_mem_pool_block;
    int                       found = 0;

    if (!mem_ptr)
        return;

    /* Put the node back to the pool */
    hg_thread_spin_lock(&hg_mem_pool->block_lock);
    HG_QUEUE_FOREACH(hg_mem_pool_block, &hg_mem_pool->blocks, entry)
    {
        /* If MR handle is NULL, it does not really matter which pool we push
         * the node back to.
         */
        if (hg_mem_pool_block->mr_handle == mr_handle) {
            struct hg_mem_pool_chunk *hg_mem_pool_chunk =
                container_of(mem_ptr, struct hg_mem_pool_chunk, chunk);
            hg_thread_spin_lock(&hg_mem_pool_block->chunk_lock);
            HG_QUEUE_PUSH_TAIL(&hg_mem_pool_block->chunks, hg_mem_pool_chunk, entry);
            hg_thread_spin_unlock(&hg_mem_pool_block->chunk_lock);
            found = 1;
            break;
        }
    }
    hg_thread_spin_unlock(&hg_mem_pool->block_lock);

    HG_UTIL_CHECK_WARNING(found != 1, "Memory block was not found");
}

/*---------------------------------------------------------------------------*/
size_t
hg_mem_pool_chunk_offset(struct hg_mem_pool *hg_mem_pool, void *mem_ptr, void *mr_handle)
{
    struct hg_mem_pool_block *hg_mem_pool_block;

    hg_thread_spin_lock(&hg_mem_pool->block_lock);
    HG_QUEUE_FOREACH(hg_mem_pool_block, &hg_mem_pool->blocks, entry)
    if (hg_mem_pool_block->mr_handle == mr_handle)
        break;
    hg_thread_spin_unlock(&hg_mem_pool->block_lock);

    return (size_t)((char *)mem_ptr - (char *)hg_mem_pool_block);
}
