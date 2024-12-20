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

/*-------------------------------------------------------------------------
 *
 * Created:     H5SCprivate.h
 *
 * Purpose:     Private header for library accessible shared chunk cache routines.
 *
 *-------------------------------------------------------------------------
 */

#ifndef H5SCprivate_H
#define H5SCprivate_H

/* Private headers needed by this file */
#include "H5private.h"  /* Generic Functions                   */
#include "H5Dprivate.h" /* Datasets                            */

/**************************/
/* Library Private Macros */
/**************************/

/****************************/
/* Library Private Typedefs */
/****************************/

/* Forward declaration for shared chunk cache struct (defined in H5SCpkg.h) */
typedef struct H5SC_t H5SC_t;

/*
 * Layout callbacks
 */
/* Looks up count chunk address and size on disk. defined_values_size is the number of bytes to read if only
 * the list of defined values is needed. size_hint is the suggested allocation size for the chunk (could be
 * larger if the chunk might expand when decoded). defined_values_size_hint is the suggested allocation size
 * if only the list of defined values is needed. If *defined_values_size is returned as 0, then all values are
 * defined for the chunk. In this case, the chunk may still be decoded without reading from disk, by
 * allocating a buffer of size defined_valued_size_hint and passing it to H5SC_chunk_decode_t with
 * *nbytes_used set to 0. *udata can be set to anything and will be passed through to H5SC_chunk_decode_t
 * and/or the selection or vector I/O routines, then freed with free() (we will create an H5SC_free_udata_t
 * callback if necessary). */
typedef herr_t (*H5SC_chunk_lookup_t)(H5D_t *dset, size_t count, hsize_t *scaled[] /*in*/,
                                      haddr_t *addr[] /*out*/, hsize_t *size[] /*out*/,
                                      hsize_t *defined_values_size[] /*out*/, size_t *size_hint[] /*out*/,
                                      size_t *defined_values_size_hint[] /*out*/, void **udata[] /*out*/);

/* Decompresses/decodes the chunk from file format to memory cache format if necessary. Reallocs chunk buffer
 * if necessary. On entry, nbytes is the number of bytes used in the chunk buffer. On exit, it shall be set to
 * the total number of bytes used (not allocated) across all buffers for this chunk. On entry, alloc_size is
 * the size of the chunk buffer. On exit, it shall be set to the total number of bytes allocated across all
 * buffers for this chunk. Optional, if not present, chunk is the same in cache as on disk. */
typedef herr_t (*H5SC_chunk_decode_t)(H5D_t *dset, size_t *nbytes /*in,out*/, size_t *alloc_size /*in,out*/,
                                      void **chunk /*in,out*/, void *udata);

/* The same as H5SC_chunk_decode_t but only decodes the defined values. Optional, if not present, the entire
 * chunk must always be decoded. */
typedef herr_t (*H5SC_chunk_decode_defined_values_t)(H5D_t *dset, size_t *nbytes /*in,out*/,
                                                     size_t *alloc_size /*in,out*/, void **chunk /*in,out*/);

/* Creates a new empty chunk. Does not insert into on disk chunk index. If fill is true, writes the fill value
 * to the chunk (unless this is a sparse chunk). The number of bytes used is returned in *nbytes and the size
 * of the chunk buffer is returned in *buf_size. */
typedef herr_t (*H5SC_new_chunk_t)(H5D_t *dset, bool fill, size_t *nbytes /*out*/, size_t *buf_size /*out*/,
                                   void **chunk /*chunk*/);

/* Reallocates buffers as necessary so the total allocated size of buffers for the chunk (alloc_size) is equal
 * to the total number of bytes used (nbytes). Optional, if not present the chunk cache will be more likely to
 * evict chunks if there is wasted space in the buffers. */
typedef herr_t (*H5SC_chunk_condense_t)(H5D_t *dset, size_t *nbytes /*in, out*/, void **chunk /*in, out*/);

/* Compresses/encodes the chunk as necessary. If chunk is the same as cache_buf, leaves *write_buf as NULL.
 * This function leaves chunk alone and allocates write_buf if necessary to hold compressed data, sets
 * *write_size to the size of the data in write_buf, and sets *write_size_alloc to the size of write_buf, if
 * it was allocated. */
typedef herr_t (*H5SC_chunk_encode_t)(H5D_t *dset, hsize_t *write_size /*out*/,
                                      hsize_t *write_buf_alloc /*out*/, const void *chunk,
                                      void **write_buf /*out*/);

/* Frees chunk and all memory referenced by it. Optional, if not present free() is simply used. */
typedef herr_t (*H5SC_chunk_evict_t)(H5D_t *dset, void *chunk);

/* The same as H5SC_chunk_encode_t but does not preserve chunk buffer, encoding is performed in-place. Must
 * free all other data used. */
typedef herr_t (*H5SC_chunk_encode_in_place_t)(H5D_t *dset, size_t *write_size /*out*/,
                                               void **chunk /*in,out*/);

/* Inserts (or reinserts) count chunks into the chunk index if necessary. Old address and size (if any) of the
 * chunks on disk are passed as addr and old_disk_size, the new size is passed in as new_disk_size. This
 * function resizes and reallocates on disk if necessary, returning the address of the chunks on disk in
 * *addr. If an element in chunk is passed as NULL then this function shall insert a chunk large enough and
 * with properties set to (initially) hold only fill values. */
typedef herr_t (*H5SC_chunk_insert_t)(H5D_t *dset, size_t count, hsize_t *scaled[] /*in*/,
                                      haddr_t *addr[] /*in,out*/, hsize_t old_disk_size[],
                                      hsize_t new_disk_size[], void *chunk[] /*in*/);

/* Called when the chunk cache wants to read data directly from the disk to the user buffer via selection I/O.
 * If not possible due to compression, etc, returns select_possible=false. Otherwise transforms the file space
 * if necessary to describe the selection in the on disk format (returns transformed space in file_space_out).
 * If no transformation is necessary, leaves *file_space_out as NULL. chunk may be passed as NULL, and may
 * also be an in-cache chunk that only contains information on selected elements. Optional, if not present,
 * chunk I/O is only performed on entire chunks or with vector I/O. The H5SC code checks for type conversion
 * before calling this. */
typedef herr_t (*H5SC_chunk_selection_read_t)(H5D_t *dset, H5S_t *file_space_in, void *chunk /*in*/,
                                              H5S_t **file_space_out /*out*/, bool *select_possible /*out*/,
                                              void *udata);

/* Called when the chunk cache wants to read data directly from the disk to the user buffer via vector I/O. If
 * not possible due to compression, etc, returns vector_possible=false. Otherwise returns the vector of
 * selected elements in offsets (within the file, not the chunk, this is why addr is passed in) and sizes,
 * with the number of vectors returned in vec_count. chunk may be passed as NULL, and may also be an in-cache
 * chunk that only contains information on selected elements. Optional, if not present, chunk I/O is only
 * performed on entire chunks or with selection I/O. The H5SC code checks for type conversion before calling
 * this. */
typedef herr_t (*H5SC_chunk_vector_read_t)(H5D_t *dset, haddr_t addr, H5S_t *file_space_in,
                                           void *chunk /*in*/, size_t *vec_count /*out*/,
                                           haddr_t **offsets /*out*/, size_t **sizes /*out*/,
                                           bool *vector_possible /*out*/, void *udata);

/* Called when the chunk cache wants to write data directly from the user buffer to the cache via selection
 * I/O. If not possible due to compression, etc, returns select_possible=false. Otherwise transforms the file
 * space if necessary to describe the selection in the on disk format (returns transformed space in
 * file_space_out). If no transformation is necessary, leaves *file_space_out as NULL. chunk may be passed as
 * NULL, and may also be an in-cache chunk that only contains information on selected elements. Optional, if
 * not present, chunk I/O is only performed on entire chunks or with vector I/O. The H5SC code checks for type
 * conversion before calling this. */
typedef herr_t (*H5SC_chunk_selection_write_t)(H5D_t *dset, H5S_t *file_space_in, void *chunk /*in*/,
                                               H5S_t *file_space_out /*out*/, bool *select_possible /*out*/,
                                               void *udata);

/* Called when the chunk cache wants to write data directly from the user buffer to the cache via vector I/O.
 * If not possible due to compression, etc, returns vector_possible=false. Otherwise returns the vector of
 * selected elements in offsets (within the file, not the chunk, this is why addr is passed in) and sizes,
 * with the number of vectors returned in vec_count. chunk may be passed as NULL, and may also be an in-cache
 * chunk that only contains information on selected elements. Optional, if not present, chunk I/O is only
 * performed on entire chunks or with selection I/O. The H5SC code checks for type conversion before calling
 * this. */
typedef herr_t (*H5SC_chunk_vector_write_t)(H5D_t *dset, haddr_t addr, H5S_t *file_space_in,
                                            void *chunk /*in*/, size_t *vec_count /*out*/,
                                            haddr_t **offsets /*out*/, size_t **sizes /*out*/,
                                            bool *vector_possible /*out*/, void *udata);

/* Scatters data from the chunk buffer into the memory buffer (in dset_info), performing type conversion if
 * necessary. file_space's extent matches the chunk dimensions and the selection is within the chunk.
 * mem_space's extent matches the entire memory buffer's and the selection within it is the selected values
 * within the chunk, offset appropriately within the full extent. Optional, if not present, chunk is the same
 * in memory as it is in cache, with the exception of type conversion (which will be handled by the H5SC
 * layer). If the layout stores variable length data within the chunk this callback must be defined. */
typedef herr_t (*H5SC_chunk_scatter_mem_t)(H5D_io_info_t *io_info, H5D_dset_io_info_t *dset_info,
                                           H5S_t *mem_space, H5S_t *file_space, const void *chunk);

/* Gathers data from the memory buffer (in dset_info) into the chunk buffer, performing type conversion if
 * necessary. file_space's extent matches the chunk dimensions and the selection is within the chunk.
 * mem_space's extent matches the entire memory buffer's and the selection within it is the selected values
 * within the chunk, offset appropriately within the full extent. Defines selected values in the chunk.
 * Optional, if not present, chunk is the same in memory as it is in cache, with the exception of type
 * conversion (which will be handled by H5SC layer). If the layout stores variable length data within the
 * chunk this callback must be defined. */
typedef herr_t (*H5SC_chunk_gather_mem_t)(H5D_io_info_t *io_info, H5D_dset_io_info_t *dset_info,
                                          H5S_t *mem_space, H5S_t *file_space, size_t *nbytes /*in,out*/,
                                          size_t *alloc_size /*in,out*/, size_t *buf_size_total /*in,out*/,
                                          void *chunk);

/* Propagates the fill value into the selected elements of the chunk buffer, performing type conversion if
 * necessary. space's extent matches the chunk dimensions and the selection is within the chunk. Optional, if
 * not present, chunk is the same in memory as it is in cache, with the exception of type conversion (which
 * will be handled by H5SC layer). If the layout stores variable length data within the chunk this callback
 * must be defined. */
typedef herr_t (*H5SC_chunk_fill_t)(H5D_io_info_t *io_info, H5D_dset_io_info_t *dset_info, H5S_t *space,
                                    size_t *nbytes /*in,out*/, size_t *alloc_size /*in,out*/,
                                    size_t *buf_size_total /*in,out*/, void *chunk);

/* Queries the defined elements in the chunk. selection may be passed as H5S_ALL. These selections are within
 * the logical chunk. Optional, if not present, all values are defined. */
typedef herr_t (*H5SC_chunk_defined_values_t)(H5D_t *dset, H5S_t *selection, void *chunk,
                                              H5S_t **defined_values /*out*/);

/* Erases the selected elements in the chunk, causing them to no longer be defined. If all values in the chunk
 * are erased and the chunk should be deleted, sets *delete_chunk to true, causing the cache to delete the
 * chunk from cache, free it in memory using H5SC_chunk_evict_t, and delete it on disk using
 * H5SC_chunk_delete_t. These selections are within the logical chunk. Optional, if not present, the fill
 * value will be written to the selection using H5SC_chunk_fill_t. */
typedef herr_t (*H5SC_chunk_erase_values_t)(H5D_t *dset, H5S_t *selection, size_t *nbytes /*in,out*/,
                                            size_t *alloc_size /*in,out*/, void *chunk,
                                            bool *delete_chunk /*out*/);

/* Frees the data values in the cached chunk and memory used by them (but does not reallocate - see
 * H5SC_chunk_condense_t), but leaves the defined values intact. Optional, if not present the entire chunk
 * will be evicted. */
typedef herr_t (*H5SC_chunk_evict_values_t)(H5D_t *dset, size_t *nbytes /*in,out*/,
                                            size_t *alloc_size /*in,out*/, void *chunk);

/* Removes the chunk from the index and deletes it on disk. Only called if a chunk goes out of scope due to
 * H5Dset_extent() or if H5SC_chunk_erase_values_t returns *delete_chunk == true. */
typedef herr_t (*H5SC_delete_chunk_t)(H5D_t *dset, hsize_t *scaled /*in*/, haddr_t addr, hsize_t disk_size);

/* Operations that are implemented by shared chunk cache clients */
typedef struct H5SC_layout_ops_t {
    H5SC_chunk_lookup_t                lookup;
    H5SC_chunk_decode_t                decode;
    H5SC_chunk_decode_defined_values_t decode_defined_values;
    H5SC_new_chunk_t                   new_chunk;
    H5SC_chunk_condense_t              condense;
    H5SC_chunk_encode_t                encode;
    H5SC_chunk_evict_t                 evict;
    H5SC_chunk_encode_in_place_t       encode_in_place;
    H5SC_chunk_insert_t                insert;
    H5SC_chunk_selection_read_t        selection_read;
    H5SC_chunk_vector_read_t           vector_read;
    H5SC_chunk_selection_write_t       selection_write;
    H5SC_chunk_vector_write_t          vector_write;
    H5SC_chunk_scatter_mem_t           scatter_mem;
    H5SC_chunk_gather_mem_t            gather_mem;
    H5SC_chunk_fill_t                  fill;
    H5SC_chunk_defined_values_t        defined_values;
    H5SC_chunk_erase_values_t          erase_values;
    H5SC_chunk_evict_values_t          evict_values;
    H5SC_delete_chunk_t                delete_chunk;
} H5SC_layout_ops_t;

/*****************************/
/* Library-private Variables */
/*****************************/

/***************************************/
/* Library-private Function Prototypes */
/***************************************/

/* Functions that operate on a shared chunk cache */
H5_DLL H5SC_t *H5SC_create(H5F_t *file, hid_t fapl_id);
H5_DLL herr_t  H5SC_destroy(H5SC_t *cache);

/* Flush functions */
H5_DLL herr_t H5SC_flush(H5SC_t *cache);
H5_DLL herr_t H5SC_flush_dset(H5SC_t *cache, H5D_t *dset, bool evict);

/* I/O functions */
H5_DLL herr_t H5SC_read(H5SC_t *cache, size_t count, H5D_dset_io_info_t *dset_info,
                        H5D_io_type_info_t *io_type_info);
H5_DLL herr_t H5SC_write(H5SC_t *cache, size_t count, H5D_dset_io_info_t *dset_info,
                         H5D_io_type_info_t *io_type_info);
H5_DLL herr_t H5SC_direct_chunk_read(H5SC_t *cache, H5D_t *dset, hsize_t *offset, void *buf);
H5_DLL herr_t H5SC_direct_chunk_write(H5SC_t *cache, H5D_t *dset, hsize_t *offset, const void *buf);

/* Other functions */
H5_DLL herr_t H5SC_set_extent_notify(H5SC_t *cache, H5D_t *dset, hsize_t *old_dims);

#endif /* H5SCprivate_H */
